;;; e-board-runtime-error.el --- Stable Board-runtime error contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This lower contract is shared by the Board-runtime admission owner and its
;; composition facade.  It deliberately contains only the base condition;
;; facade-specific conditions remain in `e-board-runtime'.

;;; Code:

(define-error 'e-board-runtime-error "e board runtime error")

(provide 'e-board-runtime-error)

;;; e-board-runtime-error.el ends here
