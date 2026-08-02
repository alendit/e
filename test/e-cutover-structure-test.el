;;; e-cutover-structure-test.el --- Board-only product boundary tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'subr-x)

(defun e-cutover-structure-test--source (file)
  "Return repository-relative Lisp FILE contents."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defconst e-cutover-structure-test--retired-live-symbols
  '("e-harness-create-session"
    "e-harness-prompt-async"
    "e-harness-queue-prompt"
    "e-harness-steer-active-turn"
    "e-harness-abort"
    "e-harness-subscribe"
    "e-harness-messages"
    "e-harness-session-activity-events"
    "e-harness-state"
    "e-harness-active-turns")
  "Private live-session symbols forbidden in public shells and producers.")

(defun e-cutover-structure-test--symbol-hits (files)
  "Return forbidden symbol hits as (FILE SYMBOL) pairs across FILES."
  (let (hits)
    (dolist (file files)
      (let ((source (e-cutover-structure-test--source file)))
        (dolist (symbol e-cutover-structure-test--retired-live-symbols)
          (when (string-match-p
                 (concat "\\_<" (regexp-quote symbol) "\\_>") source)
            (push (list file symbol) hits)))))
    (nreverse hits)))

(ert-deftest e-cutover-structure-test-public-shells-use-chat-service-boundary ()
  "Public shells contain no direct live harness interaction or presentation feed."
  (let ((files (directory-files-recursively "lisp/shells" "\\.el\\'")))
    (should-not (e-cutover-structure-test--symbol-hits files))))

(ert-deftest e-cutover-structure-test-bundled-producers-have-no-live-session-path ()
  "Bundled producer implementations cannot create, prompt, or subscribe directly."
  (let ((files '("lisp/core/e-task-queue.el"
                 "lisp/layers/agents/e-cron-actions.el"
                 "lisp/layers/agents/e-subagent-runner.el"
                 "lisp/layers/annotations/e-annotation-answer.el"
                 "lisp/shells/e-background-session.el")))
    (should-not (e-cutover-structure-test--symbol-hits files))))

(ert-deftest e-cutover-structure-test-chat-capability-ingress-is-board-backed ()
  "Chat semantic actions contain no direct harness prompt/queue/steer/abort call."
  (should-not
   (e-cutover-structure-test--symbol-hits
    '("lisp/layers/chat/e-chat-session.el"))))

(provide 'e-cutover-structure-test)

;;; e-cutover-structure-test.el ends here
