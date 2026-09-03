;;; e-annotation-answer-test.el --- Tests for the annotation answer loop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the reusable annotation answer operation and the Tier-2 sweep.
;; Optional org-annotate integration tests cover thread decisions.  Board-only
;; tests below remain runnable without that external package and prove producer
;; authority, fact publication, and restart fencing.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-annotation-org)
(require 'e-annotation-answer)
(load (expand-file-name
       "e-test-environment-support.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defmacro e-annotation-answer-test--with-file (file-var &rest body)
  "Run BODY with a temp Org FILE-VAR carrying one open user-authored thread."
  (declare (indent 1) (debug (symbolp body)))
  `(progn
     (e-test-require-capability
      (e-annotation-org-available-p)
      "Required org-annotate test integration is unavailable or incompatible")
     (let* ((dir (make-temp-file "e-annotation-answer-" t))
            (,file-var (expand-file-name "notes.org" dir)))
       (unwind-protect
           (progn
             (write-region "A sentence to question.\n" nil ,file-var nil 'silent)
             (e-annotation-org--write
              ,file-var
              (lambda ()
                (goto-char (point-min))
                (org-annotate-create (point-min) (line-end-position)
                                      "Why this?" "user")))
             ,@body)
         (delete-directory dir t)))))

(cl-defmacro e-annotation-answer-test--with-board ((board binding) &body body)
  "Run BODY with isolated producer authority BINDING on BOARD."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let ((e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
         (e-board-registry--boards (make-hash-table :test 'equal))
         (e-board-registry--id-sequence 0)
         (e-board-runtime--producer-bindings (make-hash-table :test 'equal))
         (e-board-runtime--producer-epoch 0)
         (e-board-runtime--producer-head nil)
         (e-board-runtime--producer-tail nil)
         (e-board-runtime--producer-drain-scheduled nil)
         (e-board-runtime--producer-scheduler (lambda (_callback)))
         (e-board-runtime--admission-open-p t)
         (e-board-runtime--unsettled-producer-count 0)
         (e-board-runtime--unsettled-generation 0)
         (e-annotation-answer-producer-binding nil))
     (let* ((,board (e-board-registry-create :id "annotation-board"))
            (,binding (e-board-runtime-producer-bind
                       'annotation-test ,board :tags '(documents))))
       ,@body)))

(defconst e-annotation-answer-test--thread
  '(:id "ann-1" :range-text "A sentence"
    :messages ((:author "user" :body "Why this?")))
  "One synthetic actionable annotation thread.")

;; --- prompt -----------------------------------------------------------------

(ert-deftest e-annotation-answer-test-prompt-lists-actionable-threads ()
  "The fixed prompt names the document, the skill, and each actionable thread."
  (e-annotation-answer-test--with-file file
    (let* ((threads (e-annotation-answer--actionable file))
           (prompt (e-annotation-answer--prompt file threads)))
      (should (string-match-p (regexp-quote (expand-file-name file)) prompt))
      (should (string-match-p "e://annotations/skills/org-annotate" prompt))
      (should (string-match-p "Actionable threads (1)" prompt))
      (should (string-match-p "Why this?" prompt))
      (should (string-match-p "annotations :reply" prompt)))))

;; --- Tier 0 dispatch --------------------------------------------------------

(ert-deftest e-annotation-answer-test-dispatch-publishes-when-actionable ()
  "Dispatch publishes the actionable prompt through a board producer."
  (e-annotation-answer-test--with-file file
    (e-annotation-answer-test--with-board (_board binding)
      (let ((record (e-annotation-answer-dispatch
                     :file file :producer-binding binding)))
        (should record)
        (e-board-runtime-drain-producers)
        (should (string-match-p
                 "Why this?"
                 (e-board-message-content
                  (e-board-publication-message
                   (e-board-runtime-producer-publication-publication
                    record)))))))))

(ert-deftest e-annotation-answer-test-dispatch-noop-without-actionable ()
  "Dispatch spawns nothing when no thread is actionable."
  (e-annotation-answer-test--with-file file
    ;; Answer the only thread so it is no longer actionable.
    (let ((id (plist-get (car (plist-get (e-annotation-org-list :file file)
                                         :threads))
                         :id)))
      (e-annotation-org-reply :file file :id id :body "Answered."))
    (should-not (e-annotation-answer-dispatch :file file))))

;; --- Tier 2 sweep -----------------------------------------------------------

(ert-deftest e-annotation-answer-test-sweep-publishes-actionable-files ()
  "The sweep publishes one fact per Org file with actionable threads."
  (e-annotation-answer-test--with-file file
    (e-annotation-answer-test--with-board (_board binding)
      (let ((result (e-annotation-answer-sweep
                     (list file "/tmp/not-org.txt")
                     :producer-binding binding)))
        (should (equal (list file) (plist-get result :dispatched)))
        (should (member "/tmp/not-org.txt" (plist-get result :skipped)))
        (should (= e-board-runtime--unsettled-producer-count 1))))))

(ert-deftest e-annotation-answer-test-sweep-honors-kill-switch ()
  "The sweep dispatches nothing when the kill switch is set."
  (e-annotation-answer-test--with-file file
    (let ((e-annotation-answer-sweep-inhibit t))
      (let ((result (e-annotation-answer-sweep (list file))))
        (should (plist-get result :inhibited))
        (should-not (plist-get result :dispatched))))))

(ert-deftest e-annotation-answer-test-sweep-defers-modified-buffer ()
  "The sweep defers a file whose live buffer has unsaved edits."
  (e-annotation-answer-test--with-file file
    (let ((buffer (find-file-noselect file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "dirty\n"))
            (let ((result (e-annotation-answer-sweep (list file))))
              (should (equal (list file) (plist-get result :deferred)))
              (should-not (plist-get result :dispatched))))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

;; --- board-only cutover -----------------------------------------------------

(ert-deftest e-annotation-answer-test-dispatch-requires-producer-binding ()
  "An actionable dispatch cannot escape through missing board authority."
  (let ((file (make-temp-file "e-annotation-answer-" nil ".org")))
    (unwind-protect
        (cl-letf (((symbol-function 'e-annotation-answer--actionable)
                   (lambda (_file) (list e-annotation-answer-test--thread))))
          (should-error (e-annotation-answer-dispatch :file file)
                        :type 'e-board-runtime-producer-disabled))
      (delete-file file))))

(ert-deftest e-annotation-answer-test-dispatch-publishes-board-input ()
  "An actionable dispatch publishes work and exposes zero-match routing."
  (e-annotation-answer-test--with-board (board binding)
    (let ((file (make-temp-file "e-annotation-answer-" nil ".org")))
      (unwind-protect
          (cl-letf (((symbol-function 'e-annotation-answer--actionable)
                     (lambda (_file) (list e-annotation-answer-test--thread))))
            (let ((item (e-annotation-answer-dispatch
                         :file file :producer-binding binding)))
              (should (eq (e-board-runtime-producer-publication-state item)
                          'queued))
              (e-board-runtime-drain-producers)
              (let ((message (e-board-publication-message
                              (e-board-runtime-producer-publication-publication
                               item))))
                (should (eq (e-board-message-kind message) 'input))
                (should (equal (e-board-message-tags message)
                               '(documents annotation answer)))
                (should (equal (plist-get
                                (e-board-message-attributes message)
                                :thread-count)
                               1))
                (should (string-match-p "Why this?"
                                        (e-board-message-content message)))
                (e-board-runtime--drain-input-routing
                 board
                 (lambda ()
                   (e-board-drain-input-classifications
                    (e-board-registry-board-source-board board))))
                (should (eq (e-board-message-routing-state message) 'unrouted)))
              (should (= (hash-table-count
                          (e-board-registry-board-participants board))
                         0))))
        (delete-file file)))))

(ert-deftest e-annotation-answer-test-session-context-path-is-retired ()
  "The former fork-lite path fails instead of bypassing the board."
  (should-error
   (e-annotation-answer-dispatch
    :file "/tmp/answer.org" :with-session-context t)))

(ert-deftest e-annotation-answer-test-stale-binding-fences-sweep ()
  "A retained annotation sweep binding cannot publish after restart."
  (e-annotation-answer-test--with-board (_board binding)
    (e-board-runtime-producer-disable binding)
    (let ((file (make-temp-file "e-annotation-answer-" nil ".org")))
      (unwind-protect
          (cl-letf (((symbol-function 'e-annotation-answer--actionable)
                     (lambda (_file) (list e-annotation-answer-test--thread))))
            (should-error
             (e-annotation-answer-sweep
              (list file) :producer-binding binding)
             :type 'e-board-runtime-producer-disabled))
        (delete-file file)))))

(provide 'e-annotation-answer-test)

;;; e-annotation-answer-test.el ends here
