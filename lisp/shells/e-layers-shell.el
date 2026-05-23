;;; e-layers-shell.el --- Generic layer shell commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; User-facing shell commands for known layer selection.

;;; Code:

(require 'cl-lib)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-layer-selection)
(require 'e-layers)
(require 'e-shells)
(require 'e-startup)
(require 'subr-x)

(defun e-layers--target-harness ()
  "Return the current harness for layer commands."
  (or e-current-harness
      (e-harness-registry-get-or-create :chat-default)))

(defun e-layers--layer-label (spec harness)
  "Return completion label for SPEC in HARNESS."
  (format "%s  [%s]%s"
          (or (e-layer-spec-name spec)
              (symbol-name (e-layer-spec-id spec)))
          (e-layer-spec-id spec)
          (if (e-harness-layer-active-p harness (e-layer-spec-id spec))
              " active"
            "")))

(defun e-layers--spec-for-label (specs labels label)
  "Return the spec from SPECS corresponding to LABELS LABEL."
  (when-let ((index (cl-position label labels :test #'equal)))
    (nth index specs)))

(defun e-layers--read-layer-id (harness)
  "Read a known layer id for HARNESS."
  (let* ((specs (e-layer-list))
         (labels (mapcar (lambda (spec)
                           (e-layers--layer-label spec harness))
                         specs))
         (selected (completing-read "Layer: " labels nil t))
         (spec (e-layers--spec-for-label specs labels selected)))
    (unless spec
      (user-error "No layer selected"))
    (e-layer-spec-id spec)))

(defun e-layers--status-label (status)
  "Return display label for layer selection STATUS."
  (capitalize (replace-regexp-in-string "-" " " (symbol-name status))))

(defun e-layers--message-result (result layer-id)
  "Report layer command RESULT for LAYER-ID."
  (message "%s %s layer"
           (e-layers--status-label (plist-get result :status))
           layer-id))

;;;###autoload
(defun e-layers-toggle (&optional layer-id harness)
  "Toggle known LAYER-ID in HARNESS.
Interactively, choose a known layer and use the current presentation harness."
  (interactive)
  (let* ((target (or harness (e-layers--target-harness)))
         (id (or layer-id (e-layers--read-layer-id target)))
         (result (e-layer-selection-toggle target id)))
    (when (called-interactively-p 'interactive)
      (e-layers--message-result result id))
    result))

;;;###autoload
(defun e-layers-enable (&optional layer-id harness)
  "Enable known LAYER-ID in HARNESS."
  (interactive)
  (let* ((target (or harness (e-layers--target-harness)))
         (id (or layer-id (e-layers--read-layer-id target)))
         (result (e-layer-selection-enable target id)))
    (when (called-interactively-p 'interactive)
      (e-layers--message-result result id))
    result))

;;;###autoload
(defun e-layers-disable (&optional layer-id harness)
  "Disable LAYER-ID in HARNESS."
  (interactive)
  (let* ((target (or harness (e-layers--target-harness)))
         (id (or layer-id (e-layers--read-layer-id target)))
         (result (e-layer-selection-disable target id)))
    (when (called-interactively-p 'interactive)
      (e-layers--message-result result id))
    result))

;;;###autoload
(defun e-layers-shell ()
  "Return the generic layers shell manifest."
  (e-shell-create
   :id 'layers
   :name "Layers"
   :summary "Known layer selection."
   :required-capabilities '(layer-selection)
   :commands
   (list
    (e-shell-command-create
     :id 'toggle
     :summary "Toggle a known layer in the current harness."
     :interactive 'e-layers-toggle
     :function 'e-layer-selection-toggle
     :scope 'harness)
    (e-shell-command-create
     :id 'enable
     :summary "Enable a known layer in the current harness."
     :interactive 'e-layers-enable
     :function 'e-layer-selection-enable
     :scope 'harness)
    (e-shell-command-create
     :id 'disable
     :summary "Disable a layer in the current harness."
     :interactive 'e-layers-disable
     :function 'e-layer-selection-disable
     :scope 'harness))))

(defun e-layers-shell-startup ()
  "Refresh and register the generic layers shell provider."
  (e-shell-register (e-layers-shell)))

(add-hook 'e-startup-shell-hook #'e-layers-shell-startup)

(provide 'e-layers-shell)

;;; e-layers-shell.el ends here
