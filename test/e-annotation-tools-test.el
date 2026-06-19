;;; e-annotation-tools-test.el --- Tests for annotation review tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the Simply Annotate review-channel tools.  All tests run
;; headless: a temp source file plus a project-local annotation database, with
;; no live `simply-annotate-mode' buffer.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-tools)
(require 'e-annotation-tools)
(require 'e-text-editing)
(require 'simply-annotate)

(defmacro e-annotation-tools-test--with-project (file-var &rest body)
  "Run BODY in a temp project with a sample Org FILE-VAR bound to its path.
The annotation database strategy is forced to `project' so annotations land in a
project-local `.simply-annotations.el' beside the file."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((dir (make-temp-file "e-annotation-" t))
          (,file-var (expand-file-name "notes.org" dir))
          (simply-annotate-database-strategy 'project)
          ;; Pin the project root to DIR so file keys are stable and local.
          (project-find-functions (list (lambda (d)
                                          (ignore d)
                                          (cons 'transient dir)))))
     (unwind-protect
         (progn
           (write-region "* Task one\nBody of task one.\n* Task two\nBody two.\n"
                         nil ,file-var nil 'silent)
           ,@body)
       (delete-directory dir t))))

(defun e-annotation-tools-test--registry ()
  "Return a tools registry with the annotation tools registered."
  (let ((registry (e-tools-registry-create)))
    (e-annotation-tools--register registry)
    registry))

(defun e-annotation-tools-test--call (registry name arguments)
  "Execute NAME with ARGUMENTS against REGISTRY and return :content."
  (plist-get
   (e-tools-execute registry (list :id "call-1" :name name :arguments arguments))
   :content))

;; --- primitive-level tests --------------------------------------------------

(ert-deftest e-annotation-tools-test-add-creates-thread-with-payload ()
  "Adding a thread persists it with its proposal text and payload."
  (e-annotation-tools-test--with-project file
    (let* ((result (e-annotation-tools-add
                    :file file :start 1 :end 11
                    :text "classify as grimoire task"
                    :payload '(:org-id "abc-123" :kind "classification"
                               :apply "set-todo TRIAGED")))
           (thread-id (plist-get result :thread-id))
           (listing (e-annotation-tools-list :file file))
           (threads (plist-get listing :threads)))
      (should (stringp thread-id))
      (should (= 1 (plist-get listing :count)))
      (let ((thread (car threads)))
        (should (equal thread-id (plist-get thread :thread-id)))
        (should (equal "classify as grimoire task"
                       (plist-get thread :proposal)))
        (should (null (plist-get thread :verdict)))
        (should (equal "abc-123"
                       (alist-get 'org-id (plist-get thread :payload))))
        (should (equal "set-todo TRIAGED"
                       (alist-get 'apply (plist-get thread :payload))))))))

(ert-deftest e-annotation-tools-test-list-filters-by-org-id ()
  "Listing can filter threads by the org-id stored in the payload."
  (e-annotation-tools-test--with-project file
    (e-annotation-tools-add :file file :start 1 :end 11
                            :text "proposal A" :payload '(:org-id "id-A"))
    (e-annotation-tools-add :file file :start 20 :end 30
                            :text "proposal B" :payload '(:org-id "id-B"))
    (let ((all (e-annotation-tools-list :file file))
          (just-b (e-annotation-tools-list :file file :org-id "id-B")))
      (should (= 2 (plist-get all :count)))
      (should (= 1 (plist-get just-b :count)))
      (should (equal "proposal B"
                     (plist-get (car (plist-get just-b :threads)) :proposal))))))

(ert-deftest e-annotation-tools-test-resolve-records-verdict-and-returns-payload ()
  "Resolving sets the verdict, appends a reply, and returns the payload."
  (e-annotation-tools-test--with-project file
    (let* ((added (e-annotation-tools-add
                   :file file :start 1 :end 11 :text "proposal"
                   :payload '(:org-id "id-1" :apply "set-todo TRIAGED")))
           (thread-id (plist-get added :thread-id))
           (resolved (e-annotation-tools-resolve
                      :file file :thread-id thread-id
                      :verdict "accepted" :comment "looks right")))
      (should (equal "accepted" (plist-get resolved :verdict)))
      (should (equal "set-todo TRIAGED"
                     (alist-get 'apply (plist-get resolved :payload))))
      ;; Verdict persists and the reply is recorded on the thread.
      (let ((thread (car (plist-get (e-annotation-tools-list :file file)
                                    :threads))))
        (should (equal "accepted" (plist-get thread :verdict)))))))

(ert-deftest e-annotation-tools-test-resolve-rejects-bad-verdict ()
  "Resolving rejects a verdict outside the allowed set."
  (e-annotation-tools-test--with-project file
    (let ((thread-id (plist-get (e-annotation-tools-add
                                 :file file :start 1 :end 11 :text "p")
                                :thread-id)))
      (should-error (e-annotation-tools-resolve
                     :file file :thread-id thread-id :verdict "maybe")
                    :type 'user-error))))

(ert-deftest e-annotation-tools-test-resolve-unknown-thread-errors ()
  "Resolving an unknown thread id signals a user error."
  (e-annotation-tools-test--with-project file
    (e-annotation-tools-add :file file :start 1 :end 11 :text "p")
    (should-error (e-annotation-tools-resolve
                   :file file :thread-id "thread-nope" :verdict "accepted")
                  :type 'user-error)))

(ert-deftest e-annotation-tools-test-add-validates-region ()
  "Adding rejects an empty or inverted region."
  (e-annotation-tools-test--with-project file
    (should-error (e-annotation-tools-add :file file :start 5 :end 5 :text "p")
                  :type 'user-error)
    (should-error (e-annotation-tools-add :file file :start 9 :end 2 :text "p")
                  :type 'user-error)))

;; --- resolve-hook tests -----------------------------------------------------

(ert-deftest e-annotation-tools-test-resolve-runs-resolve-hook ()
  "Resolving runs `e-annotation-tools-resolve-functions' with the resolution."
  (e-annotation-tools-test--with-project file
    (let* ((seen nil)
           (e-annotation-tools-resolve-functions
            (list (lambda (event)
                    (setq seen event)
                    (list :did (plist-get event :verdict)))))
           (thread-id (plist-get (e-annotation-tools-add
                                  :file file :start 1 :end 11 :text "p"
                                  :payload '(:org-id "id-h"))
                                 :thread-id))
           (resolved (e-annotation-tools-resolve
                      :file file :thread-id thread-id :verdict "accepted")))
      ;; The hook saw the full resolution event, keyed to the same thread.
      (should (equal thread-id (plist-get seen :thread-id)))
      (should (equal "accepted" (plist-get seen :verdict)))
      (should (equal "id-h" (alist-get 'org-id (plist-get seen :payload))))
      ;; Non-nil handler results are surfaced to the caller as :effects.
      (should (equal '((:did "accepted")) (plist-get resolved :effects))))))

(ert-deftest e-annotation-tools-test-resolve-hook-error-is-captured ()
  "A signaling resolve handler is captured as an effect, not propagated.
Verdict persistence has already happened, so a domain-side failure must not
abort the resolution."
  (e-annotation-tools-test--with-project file
    (let* ((e-annotation-tools-resolve-functions
            (list (lambda (_event) (error "boom"))))
           (thread-id (plist-get (e-annotation-tools-add
                                  :file file :start 1 :end 11 :text "p")
                                 :thread-id))
           (resolved (e-annotation-tools-resolve
                      :file file :thread-id thread-id :verdict "rejected")))
      (should (equal "rejected" (plist-get resolved :verdict)))
      (let ((effect (car (plist-get resolved :effects))))
        (should (string-match-p "boom" (plist-get effect :error))))
      ;; The verdict still persisted despite the handler error.
      (should (equal "rejected"
                     (plist-get (car (plist-get (e-annotation-tools-list :file file)
                                                :threads))
                                :verdict))))))

(ert-deftest e-annotation-tools-test-resolve-without-hook-omits-effects ()
  "With no resolve handlers the result carries no :effects key."
  (e-annotation-tools-test--with-project file
    (let* ((e-annotation-tools-resolve-functions nil)
           (thread-id (plist-get (e-annotation-tools-add
                                  :file file :start 1 :end 11 :text "p")
                                 :thread-id))
           (resolved (e-annotation-tools-resolve
                      :file file :thread-id thread-id :verdict "accepted")))
      (should (equal "accepted" (plist-get resolved :verdict)))
      (should-not (plist-member resolved :effects)))))

;; --- registered-tool tests --------------------------------------------------

(ert-deftest e-annotation-tools-test-tools-roundtrip-through-registry ()
  "The registered tools add, list, and resolve through the registry."
  (e-annotation-tools-test--with-project file
    (let* ((registry (e-annotation-tools-test--registry))
           (added (e-annotation-tools-test--call
                   registry "annotation_add"
                   (list :file file :start 1 :end 11 :text "proposal"
                         :payload '(:org_id "id-9" :apply "add-tag working"))))
           (thread-id (plist-get added :thread-id)))
      (should (stringp thread-id))
      (let ((listed (e-annotation-tools-test--call
                     registry "annotation_list" (list :file file))))
        (should (= 1 (plist-get listed :count))))
      (let ((resolved (e-annotation-tools-test--call
                       registry "annotation_resolve"
                       (list :file file :thread_id thread-id
                             :verdict "rejected"))))
        (should (equal "rejected" (plist-get resolved :verdict)))
        (should (equal "add-tag working"
                       (alist-get 'apply (plist-get resolved :payload))))))))

(ert-deftest e-annotation-tools-test-layer-exposes-tools ()
  "The text-editing layer exposes the annotation tools capability."
  (let* ((layer (e-text-editing-layer-create))
         (capability (cl-find 'annotation-tools (e-layer-capabilities layer)
                              :key #'e-capability-id)))
    (should capability)
    (should (e-capability-tools capability))))

(provide 'e-annotation-tools-test)

;;; e-annotation-tools-test.el ends here
