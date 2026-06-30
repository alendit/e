;;; e-layer.el --- e self-management layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Runtime self-management layer.

;;; Code:

(require 'e-capabilities)
(require 'e-context-inspection)
(require 'e-harness)
(require 'e-layer-selection)
(require 'e-layers)
(require 'e-runtime-context)
(require 'e-tools)

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

(defun e-layer--compact-session-tool (arguments)
  "Compact the current session from inside an agent turn."
  (let* ((context (e-tools-current-context))
         (tool-call (plist-get context :tool-call))
         (harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (turn-id (plist-get context :turn-id)))
    (unless (e-harness-p harness)
      (signal 'e-layer-invalid-tool-context
              (list "compact_session requires an active harness")))
    (unless (stringp session-id)
      (signal 'e-layer-invalid-tool-context
              (list "compact_session requires an active session id")))
    (let ((record (e-harness-compact-session
                   harness session-id
                   :instructions
                   (e-layer--compact-session-instructions arguments)
                   :keep-recent-tokens
                   (e-layer--compact-session-keep-recent-tokens arguments)
                   :allow-active-turn t
                   :turn-id turn-id)))
      (e-tools-result-create
       tool-call
       'ok
       (e-layer--compact-session-result-message record)
       (e-layer--compact-session-result-metadata record)))))

(defun e-layer--compact-session-result-message (record)
  "Return user-facing compaction result text for RECORD."
  (format "Session compacted into %s."
          (plist-get record :id)))

(defun e-layer--compact-session-result-metadata (record)
  "Return tool/action result metadata for compaction RECORD."
  (list :refresh-context t
        :compaction-id (plist-get record :id)
        :first-kept-entry-id (plist-get record :first-kept-entry-id)
        :tokens-before (plist-get record :tokens-before)
        :tokens-kept (plist-get record :tokens-kept)))

(cl-defun e-layer--compact-session-tool-start
    (&key arguments on-done on-error &allow-other-keys)
  "Start compacting the current session from inside an agent turn."
  (let* ((context (e-tools-current-context))
         (tool-call (plist-get context :tool-call))
         (harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (turn-id (plist-get context :turn-id)))
    (unless (e-harness-p harness)
      (signal 'e-layer-invalid-tool-context
              (list "compact_session requires an active harness")))
    (unless (stringp session-id)
      (signal 'e-layer-invalid-tool-context
              (list "compact_session requires an active session id")))
    (e-harness-compact-session-start
     harness session-id
     :instructions (e-layer--compact-session-instructions arguments)
     :keep-recent-tokens (e-layer--compact-session-keep-recent-tokens arguments)
     :allow-active-turn t
     :turn-id turn-id
     :on-done (lambda (record)
                (when on-done
                  (funcall
                   on-done
                   (e-tools-result-create
                    tool-call
                    'ok
                    (e-layer--compact-session-result-message record)
                    (e-layer--compact-session-result-metadata record)))))
     :on-error on-error)))

(defun e-layer-register-compact-session-tool (registry)
  "Register the self-management context compaction tool in REGISTRY."
  (e-tools-register
   registry
   :name "compact_session"
   :description "Compact the current session context during this agent turn, then continue from the compacted transcript."
   :parameters '(:type "object"
                 :properties (:instructions
                              (:type "string"
                               :description "Optional focus instructions for the compaction summary.")
                              :keep_recent_tokens
                              (:type "integer"
                               :description "Optional approximate token budget for the verbatim suffix kept after compaction."))
                 :required [])
   :handler #'e-layer--compact-session-tool
   :start #'e-layer--compact-session-tool-start
   :blocking-class 'network))

(defun e-core-layer-create ()
  "Create the e self-management layer."
  (e-layer-create
   :id 'e
   :name "e"
   :capabilities (list (e-runtime-context-capability-create)
                       (e-layer-selection-capability-create)
                       (e-context-inspection-capability-create)
                       (e-capability-create
                        :id 'session-compaction
                        :name "Session Compaction"
                        :tools
                        (list #'e-layer-register-compact-session-tool)))))

(provide 'e-layer)

;;; e-layer.el ends here
