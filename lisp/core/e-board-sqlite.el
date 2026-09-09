;;; e-board-sqlite.el --- SQLite-backed Board creation adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Legacy explicit Board creation adapter.  Ordinary SQLite chat uses
;; `e-board-sqlite-service' and never constructs a durable-state Board mirror.

;;; Code:

(require 'cl-lib)
(require 'e-board)
(require 'e-board-registry)
(require 'e-board-storage-sqlite)

(cl-defun e-board-sqlite-create-registry-board
    (runtime &key id author principal id-function)
  "Create a durable registry Board on shared RUNTIME."
  (e-board-registry-create
   :id id :author author :principal principal :id-function id-function
   :storage (e-board-storage-sqlite-create-async runtime)))

(provide 'e-board-sqlite)

;;; e-board-sqlite.el ends here
