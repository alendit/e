;;; e-context-reference.el --- Active canvas reference metadata -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Small provider-neutral marker and layout helpers shared by context
;; construction, lifetime framing, and native serializers.

;;; Code:

(require 'seq)

(defconst e-context-canvas-reference-metadata-key :e-canvas-reference
  "Message metadata key that marks active canvas reference data.")

(defconst e-context-canvas-reference-layout-revision
  "active-canvas-reference-v1"
  "Material layout identity for request-local active canvas reference data.")

(defun e-context-canvas-reference-message-p (message)
  "Return non-nil when MESSAGE is active canvas reference data."
  (and (eq (plist-get message :role) 'context)
       (eq (plist-get (plist-get message :metadata)
                      e-context-canvas-reference-metadata-key)
           t)))

(defun e-context-canvas-reference-layout-for-messages (messages)
  "Return the active canvas layout revision when MESSAGES contain its snapshot."
  (and (seq-some #'e-context-canvas-reference-message-p messages)
       e-context-canvas-reference-layout-revision))

(defun e-context-move-canvas-reference-to-end (messages)
  "Return MESSAGES with active canvas references after the other messages."
  (append (seq-remove #'e-context-canvas-reference-message-p messages)
          (seq-filter #'e-context-canvas-reference-message-p messages)))

(provide 'e-context-reference)

;;; e-context-reference.el ends here
