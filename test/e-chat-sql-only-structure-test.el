;;; e-chat-sql-only-structure-test.el --- SQL-only chat boundary -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-chat)
(require 'e-modernchat)

(defconst e-chat-sql-only-structure-test--retired-public-symbols
  '(e-chat-service-create-ephemeral-board
    e-chat-service-open-ephemeral-board
    e-chat-service-list-ephemeral-boards-page
    e-chat-service-create-ephemeral-participant
    e-chat-service-create-ephemeral-session
    e-chat-service-ensure-ephemeral-binding
    e-chat-service-drain-ephemeral-binding
    e-chat-service-reset-ephemeral-session
    e-chat-service-binding-board
    e-chat-service-session-list
    e-chat-service-root-session-list
    e-chat-service-subscribe-view
    e-chat-service-activity-events
    e-chat-service-queued-inputs
    e-chat-service-session-name
    e-chat-service-session-title
    e-chat-service-view-p
    e-chat-service-view-subscription
    e-chat-list-boards-page
    e-chat-session-candidates
    e-chat-overview-session-candidates
    e-chat-session-reset-ephemeral
    e-chat-reset)
  "Public aggregate and ephemeral chat APIs removed by DP7A.")

(defconst e-chat-sql-only-structure-test--public-files
  (append
   '("lisp/core/e-chat-service.el"
     "lisp/core/e-board-selector.el"
     "lisp/core/e-board-orchestration.el"
     "lisp/layers/chat/e-chat-session.el"
     "lisp/shells/e-canvas.el"
     "lisp/shells/e-org-canvas.el")
   (directory-files-recursively "lisp/shells/chat" "\\.el\\'")
   (directory-files-recursively "lisp/shells/modernchat" "\\.el\\'"))
  "Production files implementing public chat and Daily behavior.")

(defconst e-chat-sql-only-structure-test--forbidden-public-symbols
  (append
   e-chat-sql-only-structure-test--retired-public-symbols
   '(e-chat-service-messages
     e-chat-service-binding-board
     e-chat-service-binding-attachment
     e-runtime-store-call
     e-runtime-store-await
     accept-process-output
     e-session-get
     e-session-list
     e-session-root-sessions
     e-session-load
     e-session-messages
     e-session-local-state
     e-session-local-messages
     e-session-aggregate
     e-board-registry
     e-board-runtime
     e-board-admission
     e-board-state))
  "Aggregate symbols forbidden in the SQL-only public chat implementation.")

(defun e-chat-sql-only-structure-test--source (file)
  "Return FILE's source text."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun e-chat-sql-only-structure-test--hits (files symbols)
  "Return exact SYMBOLS found in FILES."
  (let (hits)
    (dolist (file files)
      (let ((source (e-chat-sql-only-structure-test--source file)))
        (dolist (symbol symbols)
          (when (string-match-p
                 (concat "\\_<" (regexp-quote (symbol-name symbol)) "\\_>")
                 source)
            (push (list file symbol) hits)))))
    (nreverse hits)))

(ert-deftest e-chat-sql-only-structure-test-retired-public-apis-are-absent ()
  "No retired aggregate or ephemeral chat API remains callable."
  (dolist (symbol e-chat-sql-only-structure-test--retired-public-symbols)
    (should-not (fboundp symbol)))
  (dolist (variable '(e-chat-client-id e-chat-observer-id))
    (should-not (boundp variable))))

(ert-deftest e-chat-sql-only-structure-test-public-path-has-no-aggregate-port ()
  "Public chat, Daily, Canvas, and modernchat source uses only SQL ports."
  (should-not
   (e-chat-sql-only-structure-test--hits
    e-chat-sql-only-structure-test--public-files
    e-chat-sql-only-structure-test--forbidden-public-symbols)))

(ert-deftest e-chat-sql-only-structure-test-focused-fixtures-are-sql-shaped ()
  "DP7A acceptance fixtures cannot reintroduce a retired chat API."
  (should-not
   (e-chat-sql-only-structure-test--hits
    '("test/e-board-sqlite-service-test.el"
      "test/e-chat-daily-query-test.el"
      "test/e-modernchat-test.el"
      "test/e-chat-test-support.el"
      "test/e-chat-session-test.el"
      "test/e-chat-settlement-composition-test.el"
      "test/e-canvas-test.el"
      "test/e-chat-test.el"
      "test/e-chat-surface-composition-test.el"
      "test/e-chat-overview-composition-test.el"
      "test/e-org-canvas-test.el"
      "test/e-chat-transcript-composition-test.el"
      "test/e-chat-transcript-mechanism-test.el"
      "test/e-chat-activity-composition-test.el"
      "test/e-chat-starter-test.el"
      "e2e/graphical/e-chat-behavior-test.el"
      "e2e/graphical/e-runtime-store-recovery-behavior-test.el"
      "e2e/e-live-e2e-test.el")
    e-chat-sql-only-structure-test--retired-public-symbols)))

(provide 'e-chat-sql-only-structure-test)

;;; e-chat-sql-only-structure-test.el ends here
