;;; e-subagents-shell-test.el --- Tests for the subagents list shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shell smoke tests for `e-subagents-shell': the list buffer renders one row
;; per child, scopes to the parent session, and refreshes on registry change.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-harness-instances)
(require 'e-subagent-registry)
(require 'e-subagent-runner)
(require 'e-subagents-shell)

(defmacro e-subagents-shell-test--with-instances (&rest body)
  "Run BODY with isolated harness and harness-instance registries."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     (e-harness-instance-register
      :id :reviewer
      :name "Reviewer"
      :kind 'reviewer
      :subagent t
      :description "Use for review."
      :factory (lambda () (e-harness-create
                           :backend (e-backend-fake-create :items nil))))
     ,@body))

(defun e-subagents-shell-test--spawn (registry parent parent-session-id label)
  "Spawn a non-settling reviewer child under PARENT with LABEL."
  (e-subagent-spawn registry parent parent-session-id
                    :source-turn-id "parent-turn"
                    :type :reviewer :prompt "go" :label label
                    :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))

(ert-deftest e-subagents-shell-test-renders-children-scoped-to-parent ()
  "The list buffer renders one row per child of the parent session."
  (e-subagents-shell-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-board-session parent :id "parent-1")
      (e-harness-test-create-board-session parent :id "parent-2")
      (e-subagents-shell-test--spawn registry parent "parent-1" "child a")
      (e-subagents-shell-test--spawn registry parent "parent-2" "child b")
      (let ((buffer (e-subagents-list-buffer
                     :registry registry :parent-session-id "parent-1")))
        (unwind-protect
            (with-current-buffer buffer
              (should (derived-mode-p 'e-subagents-shell-mode))
              (should (= 1 (length tabulated-list-entries)))
              (should (equal (aref (cadr (car tabulated-list-entries)) 0)
                             "child a")))
          (kill-buffer buffer)
          (remove-hook 'e-subagent-registry-change-functions
                       #'e-subagents-shell--refresh-buffers))))))

(ert-deftest e-subagents-shell-test-refreshes-on-registry-change ()
  "A spawn after opening the buffer is reflected by the change hook."
  (e-subagents-shell-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let ((buffer (e-subagents-list-buffer
                     :registry registry :parent-session-id "parent-1")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (should (null tabulated-list-entries)))
              (e-subagents-shell-test--spawn registry parent "parent-1" "late")
              (with-current-buffer buffer
                (should (= 1 (length tabulated-list-entries)))))
          (kill-buffer buffer)
          (remove-hook 'e-subagent-registry-change-functions
                       #'e-subagents-shell--refresh-buffers))))))

(ert-deftest e-subagents-shell-test-row-actions-are-commands ()
  "The documented row-action keys are bound to interactive commands."
  (e-subagents-shell-test--with-instances
    (let ((buffer (e-subagents-list-buffer
                   :registry (e-subagent-registry-create))))
      (unwind-protect
          (with-current-buffer buffer
            (dolist (cell '(("RET" . e-subagents-shell-open-chat)
                            ("i" . e-subagents-shell-interrupt)
                            ("k" . e-subagents-shell-shutdown)
                            ("g" . e-subagents-shell-refresh)))
              (let ((binding (keymap-lookup e-subagents-shell-mode-map (car cell))))
                (should (eq binding (cdr cell)))
                (should (commandp binding)))))
        (kill-buffer buffer)
        (remove-hook 'e-subagent-registry-change-functions
                     #'e-subagents-shell--refresh-buffers)))))

(provide 'e-subagents-shell-test)

;;; e-subagents-shell-test.el ends here

(ert-deftest e-subagents-shell-test-renders-progress-columns-and-soft-stale-warning ()
  "Rows show live progress evidence and warn without changing running state."
  (let ((buffer (e-subagents-list-buffer
                 :registry (e-subagent-registry-create))))
    (unwind-protect
        (with-current-buffer buffer
          (let ((record '(:subagent-id "sub_000001" :type :reviewer :status running
                          :started-at 0.0 :last-activity-at 0.0 :progress-sequence 3
                          :progress (:sequence 3 :summary "Finished focused ERT")
                          :outputs nil)))
            (should (equal (mapcar #'car tabulated-list-format)
                           '("Label" "Type" "Status" "Runtime" "Last activity"
                             "Progress" "Result" "Outputs")))
            (let ((first (e-subagents-shell--entry record))
                  (second (e-subagents-shell--entry record)))
              (should (equal (aref (cadr first) 5) "#3 Finished focused ERT"))
              (should (eq (get-text-property 0 'face (aref (cadr second) 2))
                          'font-lock-warning-face))
              (should (eq (plist-get record :status) 'running)))))
      (kill-buffer buffer)
      (remove-hook 'e-subagent-registry-change-functions
                   #'e-subagents-shell--refresh-buffers))))

(ert-deftest e-subagents-shell-test-supervision-keys-are-commands ()
  "The live operator controls expose steer and progress inspection."
  (let ((buffer (e-subagents-list-buffer
                 :registry (e-subagent-registry-create))))
    (unwind-protect
        (with-current-buffer buffer
          (dolist (cell '(("s" . e-subagents-shell-steer)
                          ("p" . e-subagents-shell-progress)))
            (let ((binding (keymap-lookup e-subagents-shell-mode-map (car cell))))
              (should (eq binding (cdr cell)))
              (should (commandp binding)))))
      (kill-buffer buffer)
      (remove-hook 'e-subagent-registry-change-functions
                   #'e-subagents-shell--refresh-buffers))))

(ert-deftest e-subagents-shell-test-progress-command-shows-bounded-tail ()
  "Progress inspection shows the latest snapshot and requests only ten messages."
  (let* ((registry (e-subagent-registry-create))
         (record '(:subagent-id "sub_000001" :type :reviewer :status running
                   :session-id "child" :parent-session-id "parent"
                   :progress (:sequence 4 :summary "Finished focused ERT")
                   :outputs nil))
         (buffer (e-subagents-list-buffer :registry registry)))
    (puthash "sub_000001" record (e-subagent-registry-records registry))
    (setf (e-subagent-registry-order registry) '("sub_000001"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (e-subagents-shell--refresh)
            (cl-letf (((symbol-function 'e-subagents-shell--subagent-id-at-point)
                       (lambda () "sub_000001"))
                      ((symbol-function 'e-subagent-raw-read)
                       (lambda (_registry _subagent-id limit)
                         (should (= limit 10))
                         '(:messages ((:role assistant :content "tail")))))
                      ((symbol-function 'e-workspace-pop-to-buffer)
                       (lambda (progress-buffer &rest _)
                         (should (buffer-live-p progress-buffer)))))
              (e-subagents-shell-progress)))
          (with-current-buffer e-subagents-shell-progress-buffer-name
            (should (string-match-p "Finished focused ERT" (buffer-string)))
            (should (string-match-p "Transcript tail" (buffer-string)))))
      (when-let ((progress-buffer (get-buffer e-subagents-shell-progress-buffer-name)))
        (kill-buffer progress-buffer))
      (kill-buffer buffer)
      (remove-hook 'e-subagent-registry-change-functions
                   #'e-subagents-shell--refresh-buffers))))
