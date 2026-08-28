;;; e-harness-base.el --- Harness support layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Built-in harness support layer.  This layer contributes resources and
;; lifecycle hooks owned by the harness rather than by OS or editor tool sets.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'e-capabilities)
(require 'e-context)
(require 'e-layers)
(require 'e-raw-results)
(require 'e-session)
(require 'e-session-tmp-resources)
(require 'e-session-resources)
(require 'e-raw-result-cleanup)
(require 'e-tool-invocation-details)
(require 'e-tool-output-truncation)

(declare-function e-harness-sessions "e-harness")

(defcustom e-harness-base-receipt-max-entries 8
  "Initial measured maximum number of receipts in model context.

The default was selected from representative tool-heavy sessions: eight
compact receipts leave room for the active turn while keeping the changing
frontier small.  Callers may pass a narrower bound to the pure projection
helper for a particular context budget."
  :type 'integer
  :group 'e)

(defcustom e-harness-base-receipt-max-bytes 4096
  "Initial measured maximum UTF-8 bytes for the receipt context block.

The default is independent of invocation-detail resource size.  Receipt
context contains identity and liveness only, never arguments or result
content."
  :type 'integer
  :group 'e)

(defun e-harness-base--receipt-events
    (harness session-id erased-tool-call-ids)
  "Return current-path receipt events excluding erased call identities.
Filtering is performed before any ordering, bounds, or count is derived."
  (let ((erased (delete-dups
                 (cl-remove-if-not #'stringp
                                   (copy-sequence erased-tool-call-ids)))))
    (cl-loop for entry in
             (e-session-current-path
              (e-harness-sessions harness) session-id)
             for payload = (plist-get entry :payload)
             for receipt = (and (eq (plist-get entry :type) 'activity-event)
                                (eq (plist-get entry :event-type)
                                    'tool-finished)
                                (plist-get payload :receipt))
             for id = (and (listp receipt)
                           (plist-get receipt :tool-call-id))
             when (and (listp receipt)
                       (stringp id)
                       (not (member id erased)))
             collect (copy-tree receipt))))

(defun e-harness-base--receipt-view (harness session-id receipt)
  "Return RECEIPT with current detail-resource liveness for presentation."
  (let* ((uri (plist-get receipt :details-uri))
         (available (and (stringp uri)
                         (e-session-tmp-reference-available-p
                          harness session-id uri))))
    (append
     (list :tool-call-id (plist-get receipt :tool-call-id)
           :tool (plist-get receipt :tool)
           :status (plist-get receipt :status))
     (when (plist-member receipt :stated-purpose)
       (list :stated-purpose (plist-get receipt :stated-purpose)))
     (when (plist-member receipt :purpose-status)
       (list :purpose-status (plist-get receipt :purpose-status)))
     (when (stringp uri)
       (list :details-uri uri))
     (list :details (if available 'available 'unavailable)
           :details-lifetime 'session-tmp))))

(defun e-harness-base--receipt-json (view)
  "Return canonical JSON for one model-facing receipt VIEW."
  (let ((object
         (append
          (list (cons "tool_call_id"
                      (format "%s" (plist-get view :tool-call-id)))
                (cons "tool" (format "%s" (plist-get view :tool)))
                (cons "status" (format "%s" (plist-get view :status))))
          (when (plist-member view :stated-purpose)
            (list (cons "stated_purpose"
                        (plist-get view :stated-purpose))))
          (when (plist-member view :purpose-status)
            (list (cons "purpose_status"
                        (format "%s" (plist-get view :purpose-status)))))
          (when (plist-member view :details-uri)
            (list (cons "details_uri" (plist-get view :details-uri))))
          (list (cons "details" (format "%s" (plist-get view :details)))
                (cons "details_lifetime"
                      (format "%s" (plist-get view :details-lifetime)))))))
    (json-encode object)))

(defun e-harness-base--receipt-block-content (views omitted)
  "Return deterministic receipt block CONTENT for VIEWS and OMITTED count."
  (string-join
   (append (when (> omitted 0)
             (list (format "[%d earlier tool receipt%s omitted]"
                           omitted
                           (if (= omitted 1) "" "s"))))
           (mapcar (lambda (view)
                     (concat "- " (e-harness-base--receipt-json view)))
                   views))
   "\n"))

(cl-defun e-harness-base-receipt-projection
    (harness session-id &key erased-tool-call-ids max-entries max-bytes)
  "Return bounded receipt context for HARNESS SESSION-ID.

The return plist contains selected liveness-aware `:receipts', one optional
system `:messages' block, `:omitted-count', and canonical UTF-8 `:bytes'.
ERASED-TOOL-CALL-IDS is intentionally the only erasure input.  It is consumed
before current-path ordering, entry/byte bounds, and omitted-count derivation."
  (let* ((entry-limit (max 0 (or max-entries
                                 e-harness-base-receipt-max-entries)))
         (byte-limit (max 0 (or max-bytes
                                e-harness-base-receipt-max-bytes)))
         (receipts (e-harness-base--receipt-events
                    harness session-id erased-tool-call-ids))
         (total (length receipts))
         (selected (copy-sequence
                    (last receipts (min entry-limit total))))
         (omitted (- total (length selected)))
         (views (mapcar (lambda (receipt)
                          (e-harness-base--receipt-view
                           harness session-id receipt))
                        selected))
         (content (e-harness-base--receipt-block-content views omitted)))
    (while (and selected (> (string-bytes content) byte-limit))
      (setq selected (cdr selected)
            omitted (1+ omitted)
            views (mapcar (lambda (receipt)
                            (e-harness-base--receipt-view
                             harness session-id receipt))
                          selected)
            content (e-harness-base--receipt-block-content views omitted)))
    (when (> (string-bytes content) byte-limit)
      ;; Never expose a syntactically incomplete JSON receipt/header.  This can
      ;; happen when even the omitted marker cannot fit in the caller's bound.
      (setq selected nil
            views nil
            content ""))
    (list :receipts views
          :selected-count (length views)
          :total-count total
          :omitted-count omitted
          :bytes (string-bytes content)
          :messages (and (not (string-empty-p content))
                         (list (list :role 'system :content content))))))

(cl-defun e-harness-base--receipt-context-provider
    (&key harness session-id _turn-id _context-purpose)
  "Build the changing receipt block for HARNESS SESSION-ID."
  (plist-get
   (e-harness-base-receipt-projection
    harness session-id
    :erased-tool-call-ids
    (e-session-erased-tool-call-ids
     (e-harness-sessions harness) session-id))
   :messages))

(defconst e-harness-base-instructions
  "Communicate reasoning explicitly and concretely, without unnecessary detail. Surface concise reasoning when it changes what the user can understand about the turn: a distinct phase begins, new evidence narrows the work, a decision or tradeoff is made, a blocker appears, the approach changes, or a non-obvious next action is about to happen. Do not send an update for every command or tool call, repeat the same reason for similar commands, restate visible plans, or narrate obvious continuation."
  "Base model-facing instructions contributed by the harness-base layer.")

(defun e-harness-base-context-capability-create ()
  "Create the harness-base context guidance capability."
  (e-capability-create
   :id 'harness-base-context
   :name "Harness Base Context"
   :instruction-priority 240
   :instructions e-harness-base-instructions
   :context-providers
   (list (e-context-provider-create
          :name 'tool-invocation-receipts
          :priority 200
          :cache-placement 'dynamic-context
          :build #'e-harness-base--receipt-context-provider))))

(defun e-harness-base-layer-create ()
  "Create the harness-base support layer."
  (e-layer-create
   :id 'harness-base
   :name "Harness Base"
   :capabilities (list (e-harness-base-context-capability-create)
                       (e-raw-results-capability-create)
                       (e-session-tmp-capability-create)
                       (e-session-resources-capability-create)
                       (e-tool-invocation-details-capability-create)
                       (e-tool-output-truncation-capability-create))))

(provide 'e-harness-base)

;;; e-harness-base.el ends here
