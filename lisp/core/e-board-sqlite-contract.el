;;; e-board-sqlite-contract.el --- Durable Board SQL value contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Stable values shared by the Board SQL application service, its worker, and
;; the explicit offline migrator.  This is not a storage abstraction: SQLite is
;; the only implementation.

;;; Code:

(require 'e-runtime-store-codec)

(define-error 'e-board-sqlite-error "Board SQLite error")
(define-error 'e-board-sqlite-conflict "Board SQLite conflict"
  'e-board-sqlite-error)

(defconst e-board-sqlite-activity-page-count-limit 256
  "Maximum participant rows returned by one Board activity page.

The count is a consumer bound, not a storage capacity.  Callers must request
a positive count no greater than this value; the worker applies the same bound
when constructing the detached page.")

(defconst e-board-sqlite-activity-page-byte-limit (* 512 1024)
  "Maximum encoded bytes retained by one detached Board activity page.")

(defconst e-board-sqlite-activity-fact-row-limit 4096
  "Maximum newest relevant fact rows inspected per activity identity.

Activity observation is intentionally bounded.  The worker applies this bound
independently to the selected session and run identity sets, so unrelated Board
history cannot displace a participant's relevant outcome.  It does not replay
an unbounded Board history after a page's participant set has been selected.")

(defconst e-board-sqlite-activity-lifecycle-statuses
  '(queued running blocked done failed cancelled)
  "Durable lifecycle statuses emitted by the current subagent contract.

The lifecycle source key contains the session identity and one of these
statuses.  Keeping the selector domain in the Board contract lets the SQL
worker select exact durable source identities without depending on the live
runner or its registry.")

(defconst e-board-sqlite-activity-session-row-limit 512
  "Maximum current session query rows reduced for one activity page.")

(defun e-board-sqlite-signature-hash (value)
  "Return VALUE's canonical SHA-256 identity for SQL idempotency checks."
  (secure-hash 'sha256 (e-runtime-store-codec-encode value)))

(provide 'e-board-sqlite-contract)

;;; e-board-sqlite-contract.el ends here
