;;; e-layer-selection.el --- Generic layer selection capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Layer selection actions for known layer ids.

;;; Code:

(require 'e-capabilities)
(require 'e-harness)
(require 'e-layers)

(defun e-layer-selection-list (harness)
  "Return known layer state for HARNESS."
  (mapcar (lambda (spec)
            (list :id (e-layer-spec-id spec)
                  :name (e-layer-spec-name spec)
                  :summary (e-layer-spec-summary spec)
                  :active (e-harness-layer-active-p
                           harness
                           (e-layer-spec-id spec))))
          (e-layer-list)))

(defun e-layer-selection-enable (harness layer-id)
  "Enable registered LAYER-ID in HARNESS."
  (if (e-harness-layer-active-p harness layer-id)
      (list :status 'already-enabled
            :layer-id layer-id
            :layer (e-harness-active-layer harness layer-id))
    (let ((layer (e-layer-create-registered layer-id)))
      (e-harness-activate-layer harness layer)
      (list :status 'enabled
            :layer-id layer-id
            :layer layer))))

(defun e-layer-selection-disable (harness layer-id)
  "Disable LAYER-ID in HARNESS."
  (let ((layer (e-harness-deactivate-layer harness layer-id)))
    (if layer
        (list :status 'disabled :layer-id layer-id :layer layer)
      (list :status 'already-disabled :layer-id layer-id :layer nil))))

(defun e-layer-selection-toggle (harness layer-id)
  "Toggle registered LAYER-ID in HARNESS."
  (if (e-harness-layer-active-p harness layer-id)
      (e-layer-selection-disable harness layer-id)
    (e-layer-selection-enable harness layer-id)))

(defun e-layer-selection-capability-create ()
  "Create the generic layer-selection capability."
  (e-capability-create
   :id 'layer-selection
   :name "Layer Selection"
   :actions (list :list #'e-layer-selection-list
                  :enable #'e-layer-selection-enable
                  :disable #'e-layer-selection-disable
                  :toggle #'e-layer-selection-toggle)))

(provide 'e-layer-selection)

;;; e-layer-selection.el ends here
