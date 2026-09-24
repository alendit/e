;;; e-board-capability-test.el --- Board capability surface tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board)
(require 'e-capabilities)
(require 'e-default-harnesses)
(require 'e-default-layers)
(require 'e-subagents)

(defun e-board-capability-test--keys (capability)
  "Return CAPABILITY action keys in declaration order."
  (let (keys)
    (cl-loop for (key _action) on (e-capability-actions capability) by #'cddr
             do (push key keys))
    (nreverse keys)))

(ert-deftest e-board-capability-test-is-independent-and-described ()
  "Board owns durable observation while subagents owns live controls."
  (let* ((board (e-board-capability-create))
         (subagents (e-subagents-parent-capability-create))
         (board-keys (e-board-capability-test--keys board))
         (subagent-keys (e-board-capability-test--keys subagents)))
    (should (equal board-keys
                   '(:list :status :read :detail :list-runs :run-status)))
    (dolist (key board-keys)
      (let ((action (plist-get (e-capability-actions board) key)))
        (should (stringp (e-action-description action)))
        (should (not (string-empty-p (e-action-description action))))))
    (should-not (memq :list subagent-keys))
    (should-not (memq :status subagent-keys))
    (should-not (memq :read subagent-keys))
    (should-not (memq :list-runs subagent-keys))
    (should-not (memq :run-status subagent-keys))))

(ert-deftest e-board-capability-test-default-layer-and-restoring-context ()
  "The Board layer is a default capability and exposes restoring first."
  (let* ((spec (cl-find 'board e-default-layer-specs :key (lambda (item)
                                                           (plist-get item :id))))
         (state (e-board-orchestration-run-set-state-create :board-id "board"))
         (context (e-board-orchestration-run-set-context state))
         (status (e-board-orchestration-run-set-compact-status state)))
    (should spec)
    (should (memq 'board e-default-chat-layer-ids))
    (should-not (plist-get context :ready-p))
    (should (eq (plist-get context :status) 'restoring))
    (should (eq (plist-get status :status) 'restoring))
    (should (equal (plist-get context :projection)
                   (plist-get status :projection)))))

(provide 'e-board-capability-test)

;;; e-board-capability-test.el ends here
