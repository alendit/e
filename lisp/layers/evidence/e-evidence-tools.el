;;; e-evidence-tools.el --- Read-only session evidence tools for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capability-backed helpers for retrieving durable transcript and activity
;; evidence from the session store.  These helpers are read-only by design.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-session)
(require 'e-tools)

(defconst e-evidence-tools--messages-tool "evidence_messages"
  "Tool name used to fetch session message evidence.")

(defconst e-evidence-tools--activity-tool "evidence_activity_events"
  "Tool name used to fetch session activity evidence.")

(defconst e-evidence-tools--tool-result-tool "evidence_tool_result"
  "Tool name used to fetch one tool result evidence item.")

(defun e-evidence-tools--natural-number (value fallback)
  "Return VALUE as a natural number, or FALLBACK when VALUE is absent."
  (cond
   ((null value) fallback)
   ((and (integerp value) (>= value 0)) value)
   ((and (numberp value) (>= value 0)) (floor value))
   (t (signal 'wrong-type-argument (list 'natnump value)))))

(defun e-evidence-tools--slice (items offset limit)
  "Return ITEMS sliced from OFFSET up to LIMIT elements."
  (let* ((start (min offset (length items)))
         (tail (nthcdr start items)))
    (if limit
        (cl-subseq tail 0 (min limit (length tail)))
      (copy-sequence tail))))

(cl-defun e-evidence-fetch-messages (store session-id &key offset limit)
  "Return a read-only message evidence slice for SESSION-ID in STORE."
  (let* ((offset (e-evidence-tools--natural-number offset 0))
         (limit (and limit (e-evidence-tools--natural-number limit 0)))
         (messages (e-session-messages store session-id)))
    (list :session-id session-id
          :offset offset
          :limit limit
          :total (length messages)
          :messages (e-evidence-tools--slice messages offset limit))))

(cl-defun e-evidence-fetch-activity-events
    (store session-id &key offset limit)
  "Return a read-only activity event evidence slice for SESSION-ID in STORE."
  (let* ((offset (e-evidence-tools--natural-number offset 0))
         (limit (and limit (e-evidence-tools--natural-number limit 0)))
         (events (e-session-activity-events store session-id)))
    (list :session-id session-id
          :offset offset
          :limit limit
          :total (length events)
          :activity-events
          (e-evidence-tools--slice events offset limit))))

(defun e-evidence-tools--tool-result-message-p
    (message turn-id tool-call-id)
  "Return non-nil when MESSAGE matches TURN-ID and TOOL-CALL-ID."
  (let ((content (plist-get message :content)))
    (and (eq (plist-get message :role) 'tool)
         (equal (plist-get message :turn-id) turn-id)
         (equal (plist-get content :tool-call-id) tool-call-id))))

(defun e-evidence-fetch-tool-result
    (store session-id turn-id tool-call-id)
  "Return one read-only tool result from STORE for SESSION-ID.
TURN-ID and TOOL-CALL-ID identify the tool result."
  (let ((message
         (cl-find-if
          (lambda (candidate)
            (e-evidence-tools--tool-result-message-p
             candidate turn-id tool-call-id))
          (e-session-messages store session-id))))
    (when message
      (list :session-id session-id
            :turn-id turn-id
            :tool-call-id tool-call-id
            :message message
            :result (plist-get message :content)))))

(defun e-evidence-tools-register (registry store session-id)
  "Register read-only evidence tools in REGISTRY for STORE and SESSION-ID."
  (e-tools-register
   registry
   :name e-evidence-tools--messages-tool
   :description "Fetch a range of durable messages from the active session."
   :parameters '(:type "object"
                 :properties (:offset (:type "integer" :minimum 0)
                              :limit (:type "integer" :minimum 0)))
   :handler
   (lambda (arguments)
     (e-evidence-fetch-messages
      store session-id
      :offset (plist-get arguments :offset)
      :limit (plist-get arguments :limit))))
  (e-tools-register
   registry
   :name e-evidence-tools--activity-tool
   :description "Fetch a range of durable activity events from the active session."
   :parameters '(:type "object"
                 :properties (:offset (:type "integer" :minimum 0)
                              :limit (:type "integer" :minimum 0)))
   :handler
   (lambda (arguments)
     (e-evidence-fetch-activity-events
      store session-id
      :offset (plist-get arguments :offset)
      :limit (plist-get arguments :limit))))
  (e-tools-register
   registry
   :name e-evidence-tools--tool-result-tool
   :description "Fetch one durable tool result by turn id and tool-call id."
   :parameters '(:type "object"
                 :required ["turn_id" "tool_call_id"]
                 :properties (:turn_id (:type "string")
                              :tool_call_id (:type "string")))
   :handler
   (lambda (arguments)
     (or (e-evidence-fetch-tool-result
          store session-id
          (plist-get arguments :turn_id)
          (plist-get arguments :tool_call_id))
         (list :session-id session-id
               :turn-id (plist-get arguments :turn_id)
               :tool-call-id (plist-get arguments :tool_call_id)
               :result nil)))))

(defun e-evidence-retrieval-capability-create (store session-id)
  "Return a read-only evidence retrieval capability for STORE and SESSION-ID."
  (e-capability-create
   :id 'evidence-retrieval
   :name "Evidence Retrieval"
   :tools (list (lambda (registry)
                  (e-evidence-tools-register registry store session-id)))))

(provide 'e-evidence-tools)

;;; e-evidence-tools.el ends here
