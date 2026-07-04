;;; e-layer.el --- e self-management layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Runtime self-management layer.

;;; Code:

(require 'e-action-resources)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-context-inspection)
(require 'e-harness)
(require 'e-layer-selection)
(require 'e-layers)
(require 'e-runtime-context)
(require 'e-work)

(define-error 'e-layer-invalid-tool-context
  "e self-management tool context is invalid")

(defun e-layer--compact-session-instructions (arguments)
  "Return optional compaction instructions from ARGUMENTS."
  (let ((value (plist-get arguments :instructions)))
    (cond
     ((null value) nil)
     ((stringp value) value)
     (t (signal 'wrong-type-argument (list 'stringp :instructions))))))

(defun e-layer--compact-session-keep-recent-tokens (arguments)
  "Return optional keep-recent token budget from ARGUMENTS."
  (let ((value (plist-get arguments :keep_recent_tokens)))
    (cond
     ((null value) nil)
     ((and (integerp value) (> value 0)) value)
     (t (signal 'wrong-type-argument
                (list 'positive-integer-p :keep_recent_tokens))))))


(defun e-layer--compact-session-result-message (record)
  "Return user-facing compaction result text for RECORD."
  (format "Session compacted into %s."
          (plist-get record :id)))


(defun e-layer--compact-session-action (context arguments)
  "Compact the current action context session."
  (let ((harness (plist-get context :harness))
        (session-id (plist-get context :session-id))
        (turn-id (plist-get context :turn-id)))
    (unless (e-harness-p harness)
      (signal 'e-layer-invalid-tool-context
              (list "session compaction requires an active harness")))
    (unless (stringp session-id)
      (signal 'e-layer-invalid-tool-context
              (list "session compaction requires an active session id")))
    (let ((record (e-harness-compact-session
                   harness session-id
                   :instructions
                   (e-layer--compact-session-instructions arguments)
                   :keep-recent-tokens
                   (e-layer--compact-session-keep-recent-tokens arguments)
                   :allow-active-turn (and turn-id t)
                   :turn-id turn-id)))
      (e-layer--compact-session-action-result record))))

(defun e-layer--compact-session-action-result (record)
  "Return the action-facing result plist for compaction RECORD."
  (list :message (e-layer--compact-session-result-message record)
        :compaction-id (plist-get record :id)
        :first-kept-entry-id (plist-get record :first-kept-entry-id)
        :tokens-before (plist-get record :tokens-before)
        :tokens-kept (plist-get record :tokens-kept)))

(cl-defun e-layer--compact-session-action-request
    (context arguments &key on-done on-error &allow-other-keys)
  "Request compaction for the current action context session."
  (let ((harness (plist-get context :harness))
        (session-id (plist-get context :session-id))
        (turn-id (plist-get context :turn-id)))
    (unless (e-harness-p harness)
      (signal 'e-layer-invalid-tool-context
              (list "session compaction requires an active harness")))
    (unless (stringp session-id)
      (signal 'e-layer-invalid-tool-context
              (list "session compaction requires an active session id")))
    (e-harness-compact-session-start
     harness session-id
     :instructions
     (e-layer--compact-session-instructions arguments)
     :keep-recent-tokens
     (e-layer--compact-session-keep-recent-tokens arguments)
     :allow-active-turn (and turn-id t)
     :turn-id turn-id
     :on-done (lambda (record)
                (when on-done
                  (funcall on-done
                           (e-layer--compact-session-action-result record))))
     :on-error on-error)))

(defun e-layer--compact-session-action-work-runner (handle arguments context)
  "Start session compaction action ARGUMENTS from action CONTEXT on HANDLE."
  (let ((request
         (e-layer--compact-session-action-request
          context arguments
          :on-done (lambda (result)
                     (e-work-finish handle result))
          :on-error (lambda (err)
                      (e-work-fail handle err)))))
    (setf (e-work-handle-cancel-function handle)
          (lambda (_handle)
            (when (e-backend-request-p request)
              (e-backend-cancel-request request))
            t))
    (setf (e-work-handle-metadata handle)
          (append (e-work-handle-metadata handle)
                  (list :operation 'session-compaction
                        :request request)))
    :deferred))

(defun e-layer--compact-session-action-work ()
  "Return Work spec for the session compaction action."
  (e-work-spec-create
   :id "session_compaction"
   :description "Compact the current session context during this agent turn."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'actions
   :runner #'e-layer--compact-session-action-work-runner))

(defun e-layer--session-compaction-action ()
  "Return the session compaction action descriptor."
  (e-action-create
   :caller #'e-layer--compact-session-action
   :work (e-layer--compact-session-action-work)
   :description "Compact the current session context during this agent turn."
   :parameters '(:type "object"
                 :properties (:instructions
                              (:type "string")
                              :keep_recent_tokens
                              (:type "integer"))
                 :required [])
   :requires-session t))

(defun e-core-layer-create ()
  "Create the e self-management layer."
  (e-layer-create
   :id 'e
   :name "e"
   :capabilities (list (e-action-resources-capability-create)
                       (e-runtime-context-capability-create)
                       (e-layer-selection-capability-create)
                       (e-context-inspection-capability-create)
                       (e-capability-create
                        :id 'session-compaction
                        :name "Session Compaction"
                        :actions (list :compact
                                       (e-layer--session-compaction-action))))))

(provide 'e-layer)

;;; e-layer.el ends here
