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
(require 'e-json)
(require 'e-layers)

(defun e-dev-layer--argument-string (arguments key &optional default)
  "Return string argument KEY from ARGUMENTS, or DEFAULT."
  (let ((value (plist-get arguments key)))
    (cond
     ((null value) default)
     ((stringp value) value)
     (t default))))

(defun e-dev-layer--argument-files (arguments)
  "Return reload file list from ARGUMENTS."
  (let ((files (plist-get arguments :files)))
    (if (vectorp files) (append files nil) nil)))

(defun e-dev-layer--canonical-entry (entry)
  "Project one internal reload ENTRY into canonical JSON."
  (list :id (or (plist-get entry :id) e-json-null)
        :reason (or (plist-get entry :reason) e-json-null)
        :files (vconcat (mapcar (lambda (file) (or file e-json-null))
                                (or (plist-get entry :files) nil)))
        :scope (if (plist-get entry :scope)
                   (symbol-name (plist-get entry :scope))
                 e-json-null)
        :created-at (or (plist-get entry :created-at) e-json-null)))

(defun e-dev-layer--canonical-result (value)
  "Project an e-dev action VALUE into canonical JSON."
  (list :required (if (plist-get value :required) t e-json-false)
        :count (or (plist-get value :count) 0)
        :entries (vconcat (mapcar #'e-dev-layer--canonical-entry
                                  (or (plist-get value :entries) nil)))))

(defun e-dev-layer--reload-actions ()
  "Return e-dev reload notification actions."
  (list
	   :mark-reload-required
	   (e-action-cheap-create
	    :owner 'e-dev
	    :runner
	    (lambda (arguments _context)
	      (e-dev-layer--canonical-result
	       (e-dev-mark-reload-required
	        (e-dev-layer--argument-string arguments :reason "e source changed")
	        (e-dev-layer--argument-files arguments)
	        (intern (e-dev-layer--argument-string arguments :scope "restart")))))
    :description
    "Mark the running Emacs as needing a supported extension reload or a restart."
    :parameters
    '(:type "object"
      :properties (:reason (:type "string")
                   :files (:type "array" :items (:type "string"))
                   :scope (:type "string"))
	      :required []
      :additionalProperties :json-false))
	   :reload-required-status
	   (e-action-cheap-create
	    :owner 'e-dev
	    :runner (lambda (_arguments _context)
	              (e-dev-layer--canonical-result
	               (e-dev-reload-required-status)))
	    :description "Return pending explicit e reload status without reloading."
	    :parameters nil)))

(defun e-dev-layer--reload-capability-create ()
  "Create the e-dev reload notification capability."
  (e-capability-create
   :id 'e-dev
   :name "e Dev"
   :instructions
   (concat
    "When editing e from inside e, do not call `e-dev-reload' during an active turn."
    "\n\n"
    "Use the `e-dev' action `mark-reload-required' with scope `reloadable' for layer, default, shell, or developer-module changes, and let the user run `M-x e-dev-reload' when idle."
    "\n\n"
    "Use scope `restart' for core runtime, session, harness, Work, provider adapter, or record-shape changes; those are not applied by `e-dev-reload'.")
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
