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

(defun e-board-sqlite-signature-hash (value)
  "Return VALUE's canonical SHA-256 identity for SQL idempotency checks."
  (secure-hash 'sha256 (e-runtime-store-codec-encode value)))

(provide 'e-board-sqlite-contract)

;;; e-board-sqlite-contract.el ends here
