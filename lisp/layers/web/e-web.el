;;; e-web.el --- Basic web access layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Layer preset for web search, passive fetch, and browser-backed page control.

;;; Code:

(require 'e-layers)
(require 'e-web-capabilities)

(defun e-web-layer-create ()
  "Create the basic web access layer."
  (e-layer-create
   :id 'web
   :name "Web"
   :capabilities (list (e-web-access-capability-create))))

(provide 'e-web)

;;; e-web.el ends here
