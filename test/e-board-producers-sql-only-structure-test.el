;;; e-board-producers-sql-only-structure-test.el --- SQL-only Board producers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)

(defconst e-board-producers-sql-only-structure-test--files
  '("lisp/core/e-task-queue.el"
    "lisp/defaults/e-runtime-sqlite.el"
    "lisp/layers/agents/e-board-orchestration-actions.el"
    "lisp/layers/agents/e-cron-actions.el"
    "lisp/layers/agents/e-subagent-actions.el"
    "lisp/layers/agents/e-subagent-live.el"
    "lisp/layers/agents/e-subagent-runner.el"
    "lisp/layers/agents/e-subagents.el"
    "lisp/layers/annotations/e-annotation-answer.el"
    "lisp/shells/e-background-session.el"
    "lisp/shells/e-board-runs-shell.el"
    "lisp/shells/e-subagents-shell.el"
    "test/e-annotation-answer-test.el"
    "test/e-background-session-test.el"
    "test/e-board-orchestration-actions-test.el"
    "test/e-board-producer-test-support.el"
    "test/e-board-runs-shell-test.el"
    "test/e-cron-actions-test.el"
    "test/e-subagent-runner-test.el"
    "test/e-subagents-shell-test.el"
    "test/e-task-queue-test.el")
  "Production and proof files in the DP7B Board-producer boundary.")

(defconst e-board-producers-sql-only-structure-test--retired-symbols
  '(e-board-runtime
    e-board-registry
    e-board-orchestration-engine
    e-board-runtime-producer-bind
    e-board-runtime-producer-disable
    e-board-runtime-producer-publish-fact
    e-board-runtime-producer-publish-input
    e-board-runtime-producer-retry
    e-board-runtime-producer-cancel
    e-board-runtime-drain-producers
    e-board-runtime--drain-activity-mailboxes
    e-board-registry-board-source-board
    e-chat-service-binding-board
    e-board-post-output
    e-board-observed-work
    e-board-subscribe-aggregation
    e-board-orchestration-actions-report-from-context
    e-board-orchestration-actions-select-next-attempt
    e-board-orchestration-actions-dispatch-queue-task
    e-cron-actions-producer-binding
    e-cron-actions-bind-producer
    e-annotation-answer-producer-binding
    e-annotation-answer-bind-producer
    e-subagent--producer-bindings
    e-subagent--record-publication-target
    e-subagent-resume
    e-subagent-registry-parent-harness
    e-subagent-registry-shutdown-p
    e-task-queue-producer-binding
    e-background-trigger-producer-binding)
  "Aggregate lookup and producer-binding symbols retired by DP7B.")

(defun e-board-producers-sql-only-structure-test--source (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest e-board-producers-sql-only-structure-test-no-retired-ports ()
  "Every Board producer and its tests address SQLite explicitly."
  (let (hits)
    (dolist (file e-board-producers-sql-only-structure-test--files)
      (let ((source (e-board-producers-sql-only-structure-test--source file)))
        (dolist (symbol e-board-producers-sql-only-structure-test--retired-symbols)
          (when (string-match-p
                 (concat "\\_<" (regexp-quote (symbol-name symbol)) "\\_>")
                 source)
            (push (list file symbol) hits)))))
    (should-not (nreverse hits))))

(provide 'e-board-producers-sql-only-structure-test)

;;; e-board-producers-sql-only-structure-test.el ends here
