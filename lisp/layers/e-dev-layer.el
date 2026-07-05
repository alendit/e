;;; e-dev-layer.el --- e development layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Development layer packaging context-inspection tools.

;;; Code:

(require 'e-context-inspection)
(require 'e-capabilities)
(require 'e-dev)
(require 'e-layers)

(defun e-dev-layer--argument-string (arguments key &optional default)
  "Return string argument KEY from ARGUMENTS, or DEFAULT."
  (let ((value (plist-get arguments key)))
    (cond
     ((null value) default)
     ((stringp value) value)
     ((symbolp value) (symbol-name value))
     (t default))))

(defun e-dev-layer--argument-files (arguments)
  "Return reload file list from ARGUMENTS."
  (let ((files (plist-get arguments :files)))
    (cond
     ((vectorp files) (append files nil))
     ((listp files) files)
     ((stringp files) (list files))
     (t nil))))

(defun e-dev-layer--reload-actions ()
  "Return e-dev reload notification actions."
  (list
   :mark-reload-required
   (e-action-create
    :caller
    (lambda (_context arguments)
      (e-dev-mark-reload-required
       (e-dev-layer--argument-string arguments :reason "e source changed")
       (e-dev-layer--argument-files arguments)
       (intern (e-dev-layer--argument-string arguments :scope "full"))))
    :description
    "Mark the running Emacs as needing an explicit full reload when the user is idle."
    :parameters
    '(:type "object"
      :properties (:reason (:type "string")
                   :files (:type "array" :items (:type "string"))
                   :scope (:type "string"))
      :required []))
   :reload-required-status
   (e-action-create
    :caller (lambda (_context _arguments)
              (e-dev-reload-required-status))
    :description "Return pending explicit e reload status without reloading."
    :parameters '(:type "object" :properties nil :required []))))

(defun e-dev-layer--reload-capability-create ()
  "Create the e-dev reload notification capability."
  (e-capability-create
   :id 'e-dev
   :name "e Dev"
   :instructions
   (list
    "When editing e from inside e, do not call `e-dev-reload' during an active turn."
    "Use the `e-dev' action `mark-reload-required' when a full reload is needed, and let the user run `M-x e-dev-reload' when idle."
    "Only mark a full reload for changes that cannot be applied safely by a lightweight scoped reload path.")
   :actions (e-dev-layer--reload-actions)))

(defun e-dev-layer-create ()
  "Create the e-dev layer."
  (e-layer-create
   :id 'e-dev
   :name "e Dev"
   :capabilities (list (e-context-inspection-capability-create)
                       (e-dev-layer--reload-capability-create))))

(provide 'e-dev-layer)

;;; e-dev-layer.el ends here
