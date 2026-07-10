;;; e-annotation-answer-test.el --- Tests for the annotation answer loop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the reusable annotation answer operation and the Tier-2 sweep.
;; The subagent spawn and task-queue enqueue are stubbed so the tests assert the
;; dispatch decisions (which files get an answerer, prompt content, idempotence,
;; kill switch, and live-modified defer) without running a real turn.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-annotation-org)
(require 'e-annotation-answer)

(defmacro e-annotation-answer-test--with-file (file-var &rest body)
  "Run BODY with a temp Org FILE-VAR carrying one open user-authored thread."
  (declare (indent 1) (debug (symbolp body)))
  `(progn
     (skip-unless (e-annotation-org-available-p))
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

(ert-deftest e-annotation-answer-test-dispatch-spawns-when-actionable ()
  "Dispatch configures the answerer type once and spawns a queued subagent."
  (e-annotation-answer-test--with-file file
    (let (configured spawned)
      (cl-letf (((symbol-function 'e-subagent-configure-type)
                 (lambda (type &rest args) (push (cons type args) configured)))
                ((symbol-function 'e-subagent-spawn)
                 (lambda (_registry _harness _session &rest args)
                   (setq spawned args)
                   (list :subagent-id "sub_1"))))
        (let ((record (e-annotation-answer-dispatch :file file)))
          (should record)
          (should configured)
          (should (equal e-annotation-answer-type (caar configured)))
          (should (equal 'queue (plist-get spawned :schedule)))
          (should (string-match-p "Why this?" (plist-get spawned :prompt))))))))

(ert-deftest e-annotation-answer-test-dispatch-noop-without-actionable ()
  "Dispatch spawns nothing when no thread is actionable."
  (e-annotation-answer-test--with-file file
    ;; Answer the only thread so it is no longer actionable.
    (let ((id (plist-get (car (plist-get (e-annotation-org-list :file file)
                                         :threads))
                         :id)))
      (e-annotation-org-reply :file file :id id :body "Answered."))
    (let (spawned)
      (cl-letf (((symbol-function 'e-subagent-configure-type) #'ignore)
                ((symbol-function 'e-subagent-spawn)
                 (lambda (&rest args) (setq spawned args) (list :subagent-id "x"))))
        (should-not (e-annotation-answer-dispatch :file file))
        (should-not spawned)))))

;; --- Tier 2 sweep -----------------------------------------------------------

(ert-deftest e-annotation-answer-test-sweep-enqueues-actionable-files ()
  "The sweep enqueues one task per Org file with actionable threads."
  (e-annotation-answer-test--with-file file
    (let (enqueued)
      (cl-letf (((symbol-function 'e-task-queue-enqueue)
                 (lambda (_queue &rest args) (push args enqueued)
                   (list :task-id "t"))))
        (let ((result (e-annotation-answer-sweep (list file "/tmp/not-org.txt")
                                                 :queue 'fake)))
          (should (equal (list file) (plist-get result :dispatched)))
          (should (member "/tmp/not-org.txt" (plist-get result :skipped)))
          (should (= 1 (length enqueued)))
          (should (string-match-p "Why this?"
                                  (plist-get (car enqueued) :prompt))))))))

(ert-deftest e-annotation-answer-test-sweep-honors-kill-switch ()
  "The sweep dispatches nothing when the kill switch is set."
  (e-annotation-answer-test--with-file file
    (let ((e-annotation-answer-sweep-inhibit t)
          enqueued)
      (cl-letf (((symbol-function 'e-task-queue-enqueue)
                 (lambda (&rest _args) (push t enqueued) (list :task-id "t"))))
        (let ((result (e-annotation-answer-sweep (list file) :queue 'fake)))
          (should (plist-get result :inhibited))
          (should-not (plist-get result :dispatched))
          (should-not enqueued))))))

(ert-deftest e-annotation-answer-test-sweep-defers-modified-buffer ()
  "The sweep defers a file whose live buffer has unsaved edits."
  (e-annotation-answer-test--with-file file
    (let ((buffer (find-file-noselect file))
          enqueued)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "dirty\n"))
            (cl-letf (((symbol-function 'e-task-queue-enqueue)
                       (lambda (&rest _args) (push t enqueued) (list :task-id "t"))))
              (let ((result (e-annotation-answer-sweep (list file) :queue 'fake)))
                (should (equal (list file) (plist-get result :deferred)))
                (should-not (plist-get result :dispatched))
                (should-not enqueued))))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(provide 'e-annotation-answer-test)

;;; e-annotation-answer-test.el ends here
