;;; e-harness-turn-state.el --- Active-turn and prompt-queue owner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns active-turn identity, prompt queues, steering/inbox counts, and the
;; bounded unsettled projections used by board quiescence.  It emits semantic
;; activity events through the activity owner but does not schedule provider
;; turns or call the harness facade.

;;; Code:

(require 'cl-lib)
(require 'e-harness-state)
(require 'e-harness-activity)
(require 'e-events)
(require 'e-session)
(require 'seq)
(require 'subr-x)

(define-error 'e-harness-error "Harness runtime state error")
(define-error 'e-harness-active-turn-exists
  "Session already has an active turn")

(defun e-harness-turn-state-queue-timestamp ()
  "Return an ISO-8601 UTC timestamp for prompt queue entries."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun e-harness-queued-prompts (harness session-id)
  "Return queued prompt items for SESSION-ID in HARNESS."
  (copy-sequence (gethash session-id (e-harness-prompt-queues harness))))

(defun e-harness-turn-state-active-turn-id (entry)
  "Return active turn id from ENTRY."
  (if (listp entry)
      (plist-get entry :id)
    entry))

(defun e-harness-turn-state-active-turn-running-p (entry)
  "Return non-nil when active turn ENTRY is still running."
  (and (listp entry)
       (eq (plist-get entry :status) 'running)))

(defun e-harness-turn-state-context-frame (entry)
  "Return the context-lifetime frame retained by active turn ENTRY.
This is a read-only projection for the context owner; turn-state remains the
only owner allowed to install a replacement frame on an active entry."
  (and (listp entry)
       (plist-get entry :context-frame)))

(defun e-harness-turn-state-lifetime-generation (entry)
  "Return the lifetime generation retained by active turn ENTRY."
  (and (listp entry)
       (plist-get entry :lifetime-generation)))

(defun e-harness-turn-state-set-context-frame (entry frame)
  "Install FRAME on active turn ENTRY.
The turn-state owner owns this transition even though the context owner
produces the immutable frame value.  Return ENTRY for composition callbacks."
  (plist-put entry :context-frame frame)
  entry)

(defun e-harness-turn-state-set-lifetime-generation (entry generation)
  "Install GENERATION on active turn ENTRY and return ENTRY.
The context owner computes the generation; turn-state owns its active-entry
storage so context policy never mutates the turn record directly."
  (plist-put entry :lifetime-generation generation)
  entry)

(defun e-harness-turn-state-pending-steering (entry)
  "Return a copy of the pending steering projection from active ENTRY.
The returned list is an observation for shells and tests; callers cannot
mutate the turn owner's retained list through it."
  (copy-tree (and (listp entry)
                  (plist-get entry :pending-steering-input))))

(defun e-harness-turn-state-queue-item-metadata (item)
  "Return turn metadata for queued ITEM, including references."
  (append (copy-sequence (plist-get item :metadata))
          (when-let ((references (plist-get item :references)))
            (list :references references))))

(defun e-harness-unsettled-state (harness)
  "Return HARNESS's constant-time owner-local unsettled snapshot."
  (list :generation (e-harness-unsettled-generation harness)
        :active-turns (hash-table-count (e-harness-active-turns harness))
        :queued-inputs (e-harness-queued-input-count harness)))

(defvar e-harness-turn-state--aggregate-active-turn-count 0
  "Process-local count of active turns across all harnesses.")

(defvar e-harness-turn-state--aggregate-queued-input-count 0
  "Process-local count of queued inputs across all harnesses.")

(defvar e-harness-turn-state--aggregate-unsettled-generation 0
  "Monotonic generation of aggregate harness unsettled state.")

(defvar e-harness-aggregate-unsettled-change-hook nil
  "Hard-bounded observers of aggregate harness unsettled transitions.")

(defun e-harness-aggregate-unsettled-state ()
  "Return the constant-time process-local harness unsettled snapshot."
  (list :generation e-harness-turn-state--aggregate-unsettled-generation
        :active-turns e-harness-turn-state--aggregate-active-turn-count
        :queued-inputs e-harness-turn-state--aggregate-queued-input-count))

(defun e-harness-turn-state-reset-aggregate ()
  "Reset the process-local aggregate at an explicit runtime boundary.
This is used by isolated runtime fixtures and by a full application reset;
it changes the retained aggregate observation without fabricating a turn
transition or invoking its observers."
  (setq e-harness-turn-state--aggregate-active-turn-count 0
        e-harness-turn-state--aggregate-queued-input-count 0
        e-harness-turn-state--aggregate-unsettled-generation 0)
  (e-harness-aggregate-unsettled-state))

(defun e-harness-turn-state--adjust-aggregate-unsettled (class delta)
  "Adjust aggregate harness unsettled CLASS by DELTA."
  (let ((value
         (pcase class
           ('active-turn
            (cl-incf e-harness-turn-state--aggregate-active-turn-count delta))
           ('queued-input
            (cl-incf e-harness-turn-state--aggregate-queued-input-count delta))
           (_
            (signal 'e-harness-error
                    (list "Unknown aggregate unsettled class" class))))))
    (when (< value 0)
      (signal 'e-harness-error
              (list "Negative aggregate unsettled count" class value)))
    (cl-incf e-harness-turn-state--aggregate-unsettled-generation)
    (run-hook-with-args 'e-harness-aggregate-unsettled-change-hook
                        (e-harness-aggregate-unsettled-state))
    value))

(defun e-harness-turn-state--unsettled-changed (harness)
  "Record and publish one owner-local unsettled transition in HARNESS."
  (cl-incf (e-harness-unsettled-generation harness))
  (when-let ((function (e-harness-unsettled-change-function harness)))
    (funcall function (e-harness-unsettled-state harness))))

(defun e-harness-turn-state-adjust-queued-input-count (harness delta)
  "Adjust HARNESS's queued input count by DELTA at its owning transition."
  (let ((count (cl-incf (e-harness-queued-input-count harness) delta)))
    (when (< count 0)
      (signal 'e-harness-error (list "Negative queued input count" count)))
    (unless (= delta 0)
      (e-harness-turn-state--adjust-aggregate-unsettled 'queued-input delta)
      (e-harness-turn-state--unsettled-changed harness))
    count))

(defun e-harness-turn-state-put-active-turn (harness session-id entry)
  "Install ENTRY as SESSION-ID's active turn and publish the transition."
  (when (gethash session-id (e-harness-active-turns harness))
    (signal 'e-harness-active-turn-exists (list session-id)))
  (puthash session-id entry (e-harness-active-turns harness))
  (e-harness-turn-state--adjust-aggregate-unsettled 'active-turn 1)
  (e-harness-turn-state--unsettled-changed harness)
  entry)

(defun e-harness-turn-state-remove-active-turn (harness session-id &optional expected)
  "Remove SESSION-ID's active turn when it matches EXPECTED, if supplied."
  (let ((current (gethash session-id (e-harness-active-turns harness))))
    (when (and current (or (null expected) (eq current expected)))
      (remhash session-id (e-harness-active-turns harness))
      (e-harness-turn-state--adjust-aggregate-unsettled 'active-turn -1)
      (e-harness-turn-state--unsettled-changed harness)
      current)))

(defun e-harness-turn-state-set-queued-prompts (harness session-id items &optional delta)
  "Replace SESSION-ID queued prompt ITEMS in HARNESS by known DELTA.
Clearing a queue may omit DELTA because its owner-local count is indexed."
  (let* ((counts (e-harness-prompt-queue-counts harness))
         (old-count (or (gethash session-id counts) 0))
         (delta (or delta
                    (and (null items) (- old-count))
                    (signal 'e-harness-error
                            (list "Queued prompt delta is required"
                                  session-id))))
         (new-count (+ old-count delta)))
    (when (< new-count 0)
      (signal 'e-harness-error
              (list "Negative session prompt queue count" session-id new-count)))
    (if items
        (progn
          (puthash session-id items (e-harness-prompt-queues harness))
          (puthash session-id new-count counts))
      (remhash session-id (e-harness-prompt-queues harness))
      (remhash session-id counts))
    (e-harness-turn-state-adjust-queued-input-count harness delta))
  items)

(defun e-harness-discard-queued-board-input
    (harness session-id delivery-id endpoint-token endpoint-generation reason)
  "Discard HARNESS SESSION-ID's exact queued board head with REASON.
Only the first queued item is inspected, keeping reconciliation bounded and
preserving FIFO.  DELIVERY-ID, ENDPOINT-TOKEN, and ENDPOINT-GENERATION must all
match the immutable metadata accepted with that item.  Return the removed item,
or nil without changing the queue when the head belongs to another delivery."
  (let* ((items (e-harness-queued-prompts harness session-id))
         (item (car items))
         (metadata (and item (plist-get item :metadata))))
    (when (and (equal (plist-get metadata :board-delivery-id) delivery-id)
               (equal (plist-get metadata :board-endpoint-token) endpoint-token)
               (equal (plist-get metadata :board-endpoint-generation)
                      endpoint-generation))
      (e-harness-turn-state-set-queued-prompts harness session-id (cdr items) -1)
      (e-harness-turn-state-emit-queue-changed harness session-id)
      (e-harness-activity-emit
       harness
       (e-events-make
        :type 'input-discarded :session-id session-id :turn-id nil
        :payload
        (list :delivery-id (copy-tree delivery-id)
              :endpoint-token
              (if (vectorp endpoint-token)
                  (copy-sequence endpoint-token)
                (copy-tree endpoint-token))
              :endpoint-generation (copy-tree endpoint-generation)
              :queue-id (plist-get item :id)
              :reason reason)))
      item)))

(defun e-harness-turn-state-emit-queue-changed (harness session-id)
  "Emit a queue update event for SESSION-ID."
  (e-harness-activity-emit
   harness
   (e-events-make :type 'queue-changed
                  :session-id session-id
                  :turn-id nil
                  :payload (list :queue
                                 (e-harness-queued-prompts
                                  harness session-id)))))

(defun e-harness-turn-state-enqueue-prompt
    (harness session-id prompt references metadata &optional attached-turn-port)
  "Append PROMPT to SESSION-ID's follow-up queue in HARNESS and return its id.
Shared enqueue body with no active-turn guard for attached queue and settlement
follow-up ports."
  (let* ((queue-id (e-session-generate-ulid))
         (item (list :id queue-id
                     :prompt prompt
                     :references (copy-tree references)
                     :metadata (copy-sequence metadata)
                     :attached-turn-port attached-turn-port
                     :created-at (e-harness-turn-state-queue-timestamp)))
         (items (append (e-harness-queued-prompts harness session-id)
                        (list item))))
    (e-harness-turn-state-set-queued-prompts harness session-id items 1)
    (e-harness-turn-state-emit-queue-changed harness session-id)
    queue-id))

(provide 'e-harness-turn-state)

;;; e-harness-turn-state.el ends here
