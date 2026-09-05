;;; e-session-storage-limits.el --- Practical session persistence bounds -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared physical limits for the session SQLite adapter and worker.  These
;; bound canonical protocol values, not aggregate object graphs.

;;; Code:

(defconst e-session-storage-record-byte-limit (* 1024 1024)
  "Maximum canonical bytes for one durable session record.")

(defconst e-session-storage-batch-record-limit 256
  "Maximum records in one atomic session append batch.")

(defconst e-session-storage-batch-byte-limit (* 8 1024 1024)
  "Maximum canonical bytes for one complete session append batch body.")

(provide 'e-session-storage-limits)

;;; e-session-storage-limits.el ends here
