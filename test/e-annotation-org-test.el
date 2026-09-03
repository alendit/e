;;; e-annotation-org-test.el --- Tests for the org-annotate backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the org-annotate annotation actions and the actionable
;; predicate.  All tests run headless against a temp Org file.  The supported
;; test environment provides org-annotate even though it is optional at runtime.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-actions)
(require 'e-annotation-org)
(require 'e-annotations)
(require 'e-backend)
(require 'e-harness)
(load (expand-file-name
       "e-test-environment-support.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defmacro e-annotation-org-test--with-file (file-var &rest body)
  "Run BODY with a temp Org FILE-VAR carrying one user-authored annotation.
The annotation is created headless with author `user' so it is actionable."
  (declare (indent 1) (debug (symbolp body)))
  `(progn
     (e-test-require-capability
      (e-annotation-org-available-p)
      "Required org-annotate test integration is unavailable or incompatible")
     (let* ((dir (make-temp-file "e-annotation-org-" t))
            (,file-var (expand-file-name "notes.org" dir)))
       (unwind-protect
           (progn
             (write-region "First body sentence about scope.\nSecond body line.\n"
                           nil ,file-var nil 'silent)
             ;; Seed one open, user-authored annotation over the first line.
             (e-annotation-org--write
              ,file-var
              (lambda ()
                (goto-char (point-min))
                (org-annotate-create (point-min) (line-end-position)
                                      "Please clarify the scope." "user")))
             ,@body)
         (delete-directory dir t)))))

;; --- Org-file guard ---------------------------------------------------------

(ert-deftest e-annotation-org-test-guards-non-org-file ()
  "Actions error loudly on a non-Org file target."
  (e-test-require-capability
   (e-annotation-org-available-p)
   "Required org-annotate test integration is unavailable or incompatible")
  (should-error (e-annotation-org-list :file "/tmp/notes.txt")
                :type 'user-error)
  (should-error (e-annotation-org-list :file nil) :type 'user-error))

;; --- list + actionable predicate --------------------------------------------

(ert-deftest e-annotation-org-test-list-returns-actionable-user-thread ()
  "A fresh user-authored open thread is actionable."
  (e-annotation-org-test--with-file file
    (let* ((listing (e-annotation-org-list :file file))
           (thread (car (plist-get listing :threads))))
      (should (= 1 (plist-get listing :count)))
      (should (equal "open" (plist-get thread :state)))
      (should (plist-get thread :actionable))
      (should (equal "Please clarify the scope."
                     (plist-get (car (plist-get thread :messages)) :body))))))

(ert-deftest e-annotation-org-test-agent-reply-clears-actionable ()
  "After an agent reply, the thread's last author is agent and it is inactive."
  (e-annotation-org-test--with-file file
    (let ((id (plist-get (car (plist-get (e-annotation-org-list :file file)
                                         :threads))
                         :id)))
      (e-annotation-org-reply :file file :id id :body "Scope is the whole doc.")
      (let ((thread (car (plist-get (e-annotation-org-list :file file) :threads))))
        (should-not (plist-get thread :actionable))
        (should (equal "agent"
                       (plist-get (car (last (plist-get thread :messages)))
                                  :author))))
      ;; actionable-only now returns nothing: the loop is idempotent.
      (should (= 0 (plist-get (e-annotation-org-list :file file
                                                     :actionable-only t)
                              :count))))))

(ert-deftest e-annotation-org-test-add-creates-thread ()
  "Adding creates a new annotation with an agent-authored comment."
  (e-annotation-org-test--with-file file
    (let* ((result (e-annotation-org-add :file file :body "Proposed rewrite."))
           (id (plist-get result :id)))
      (should (stringp id))
      (should (= 2 (plist-get (e-annotation-org-list :file file) :count))))))

(ert-deftest e-annotation-org-test-resolve-sets-state-and-runs-hook ()
  "Resolving sets state, appends an optional reply, and runs the resolve hook."
  (e-annotation-org-test--with-file file
    (let* ((id (plist-get (car (plist-get (e-annotation-org-list :file file)
                                          :threads))
                          :id))
           (seen nil)
           (e-annotation-tools-resolve-functions
            (list (lambda (event)
                    (setq seen event)
                    (list :did (plist-get event :state)))))
           (resolved (e-annotation-org-resolve
                      :file file :id id :state "resolved"
                      :reply "Addressed.")))
      (should (equal "resolved" (plist-get resolved :state)))
      (should (equal id (plist-get seen :annotation-id)))
      ;; Back-compat aliases for review-channel subscribers.
      (should (equal id (plist-get seen :thread-id)))
      (should (equal "resolved" (plist-get seen :verdict)))
      (should (equal '((:did "resolved")) (plist-get resolved :effects)))
      (let ((thread (car (plist-get (e-annotation-org-list :file file) :threads))))
        (should (equal "resolved" (plist-get thread :state)))))))

(ert-deftest e-annotation-org-test-resolve-hook-error-is-captured ()
  "A signaling resolve handler is captured as an effect, not propagated."
  (e-annotation-org-test--with-file file
    (let* ((id (plist-get (car (plist-get (e-annotation-org-list :file file)
                                          :threads))
                          :id))
           (e-annotation-tools-resolve-functions
            (list (lambda (_event) (error "boom"))))
           (resolved (e-annotation-org-resolve :file file :id id)))
      (should (equal "resolved" (plist-get resolved :state)))
      (should (string-match-p "boom"
                              (plist-get (car (plist-get resolved :effects))
                                         :error))))))

;; --- concurrency defer ------------------------------------------------------

(ert-deftest e-annotation-org-test-write-defers-modified-live-buffer ()
  "A write signals when a live buffer has unsaved edits, so a loop can defer."
  (e-annotation-org-test--with-file file
    (let ((buffer (find-file-noselect file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "unsaved edit\n"))
            (let ((id (plist-get (car (plist-get (e-annotation-org-list :file file)
                                                 :threads))
                                 :id)))
              (should-error (e-annotation-org-reply :file file :id id :body "x")
                            :type 'e-annotation-org-buffer-dirty)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

;; --- action dispatch --------------------------------------------------------

(ert-deftest e-annotation-org-test-actions-roundtrip-through-dispatch ()
  "The registered actions list, reply, and resolve through action dispatch."
  (e-test-require-capability
   (e-annotation-org-available-p)
   "Required org-annotate test integration is unavailable or incompatible")
  (e-annotation-org-test--with-file file
    (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
           (capability (e-annotations-capability-create))
           (context (list :harness harness :session-id "session-1"
                          :turn-id "turn-1")))
      (should capability)
      (e-harness-activate-capability harness capability)
      (e-harness-create-session harness :id "session-1")
      (let* ((listed (e-actions-call 'annotations :list
                                     (list :file file :actionable_only t)
                                     context))
             (id (plist-get (car (plist-get listed :threads)) :id)))
        (should (= 1 (plist-get listed :count)))
        (e-actions-call 'annotations :reply
                        (list :file file :id id :body "Answer.")
                        context)
        (should (= 0 (plist-get (e-annotation-org-list :file file
                                                       :actionable-only t)
                                :count)))))))

(ert-deftest e-annotation-org-test-layer-omits-capability-without-backend ()
  "The annotations layer carries no capability when org-annotate is absent."
  (cl-letf (((symbol-function 'e-annotation-org-available-p) (lambda () nil)))
    (let ((layer (e-annotations-layer-create)))
      (should (eq (e-layer-id layer) 'annotations))
      (should-not (e-layer-capabilities layer)))))

(provide 'e-annotation-org-test)

;;; e-annotation-org-test.el ends here
