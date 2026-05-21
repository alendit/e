;;; e-layers.el --- Harness-owned layer bundles for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Layers bundle tools, instructions, prompts, skills, and context providers.
;; The harness activates layers; presentation shells only choose how to expose
;; that active runtime state to users.

;;; Code:

(require 'cl-lib)

(cl-defstruct (e-layer (:constructor e-layer-create))
  id
  name
  capabilities
  defaults)

(provide 'e-layers)

;;; e-layers.el ends here
