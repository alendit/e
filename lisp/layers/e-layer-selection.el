;;; e-layer-selection.el --- Generic layer selection capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Layer selection actions for known layer ids.

;;; Code:

(require 'e-capabilities)
(require 'e-harness)
(require 'e-json)
(require 'e-layers)

(defun e-layer-selection--string-or-null (value)
  "Return VALUE as a canonical string or JSON null."
  (cond
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   ((null value) e-json-null)
   (t (format "%s" value))))

(defun e-layer-selection--canonical-bool (value)
  "Return VALUE as a canonical JSON boolean."
  (if value t e-json-false))

(defun e-layer-selection--canonical-record (value)
  "Project one internal layer-selection VALUE into canonical JSON."
  (list :id (e-layer-selection--string-or-null (plist-get value :id))
        :name (e-layer-selection--string-or-null (plist-get value :name))
        :summary (e-layer-selection--string-or-null
                  (plist-get value :summary))
        :enabled (e-layer-selection--canonical-bool
                  (plist-get value :enabled))
        :active (e-layer-selection--canonical-bool
                 (plist-get value :active))))

(defun e-layer-selection--canonical-result (value)
  "Project an internal layer-selection VALUE into canonical JSON."
  (if (and (listp value)
           (or (null value) (keywordp (car value)))
           (plist-member value :status))
      (list :status (e-layer-selection--string-or-null
                     (plist-get value :status))
            :layer-id (e-layer-selection--string-or-null
                       (plist-get value :layer-id))
            :enabled (e-layer-selection--canonical-bool
                      (plist-get value :enabled))
            :active (e-layer-selection--canonical-bool
                     (plist-get value :active)))
    (vconcat (mapcar #'e-layer-selection--canonical-record value))))

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

(defun e-layer-selection--action-layer-id (arguments)
  "Return layer id from action ARGUMENTS."
  (let ((layer (plist-get arguments :layer)))
    (unless (stringp layer)
      (user-error "Layer action requires a canonical string :layer"))
    (intern layer)))

(defun e-layer-selection--action (handler caller description &optional parameters)
  "Return layer-selection cheap work action descriptor for HANDLER.
DESCRIPTION explains the action contract to callers."
  (e-action-cheap-create
   :id (format "layer_selection_%s" handler)
   :owner 'layer-selection
   :description description
   :parameters parameters
   :runner (lambda (arguments context)
             (e-layer-selection--canonical-result
              (funcall caller context arguments)))))

(defun e-layer-selection-capability-create ()
  "Create the generic layer-selection capability."
  (e-capability-create
   :id 'layer-selection
   :name "Layer Selection"
   :actions
   (list :list
         (e-layer-selection--action
          #'e-layer-selection-list
          (lambda (context _arguments)
            (e-layer-selection-list (plist-get context :harness)))
          "List globally selectable layer ids and their enabled/effective state. Project-local extension layers are activated through the project-local aggregate and are not independently selectable.")
         :enable
         (e-layer-selection--action
          #'e-layer-selection-enable
          (lambda (context arguments)
            (e-layer-selection-enable
             (plist-get context :harness)
             (e-layer-selection--action-layer-id arguments)))
          "Enable one globally registered layer. Pass an exact id returned by :list; project-local extension ids are not valid here."
          '(:type "object"
            :properties (:layer (:type "string"))
            :required ["layer"]
            :additionalProperties :json-false))
         :disable
         (e-layer-selection--action
          #'e-layer-selection-disable
          (lambda (context arguments)
            (e-layer-selection-disable
             (plist-get context :harness)
             (e-layer-selection--action-layer-id arguments)))
          "Disable one explicitly enabled globally registered layer. Pass an exact id returned by :list."
          '(:type "object"
            :properties (:layer (:type "string"))
            :required ["layer"]
            :additionalProperties :json-false))
         :toggle
         (e-layer-selection--action
          #'e-layer-selection-toggle
          (lambda (context arguments)
            (e-layer-selection-toggle
             (plist-get context :harness)
             (e-layer-selection--action-layer-id arguments)))
          "Toggle one globally registered layer. Pass an exact id returned by :list; project-local extension ids are not valid here."
          '(:type "object"
            :properties (:layer (:type "string"))
            :required ["layer"]
            :additionalProperties :json-false)))))

(provide 'e-layer-selection)

;;; e-layer-selection.el ends here
