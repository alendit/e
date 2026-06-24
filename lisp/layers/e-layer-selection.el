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
            (let ((id (e-layer-spec-id spec)))
              (list :id id
                    :name (e-layer-spec-name spec)
                    :summary (e-layer-spec-summary spec)
                    :enabled (e-harness-layer-enabled-p harness id)
                    :active (e-harness-layer-effective-p harness id))))
          (e-layer-list)))

(defun e-layer-selection-enable (harness layer-id)
  "Enable registered LAYER-ID in HARNESS."
  (if (e-harness-layer-enabled-p harness layer-id)
      (list :status 'already-enabled
            :layer-id layer-id
            :enabled t
            :active (e-harness-layer-effective-p harness layer-id))
    (e-harness-enable-layer-id harness layer-id)))

(defun e-layer-selection-disable (harness layer-id)
  "Disable LAYER-ID in HARNESS."
  (e-harness-disable-layer-id harness layer-id))

(defun e-layer-selection-toggle (harness layer-id)
  "Toggle registered LAYER-ID in HARNESS."
  (if (e-harness-layer-enabled-p harness layer-id)
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
