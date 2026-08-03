;;; e-board-e2e-support.el --- Board-first E2E helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Small deterministic helpers for E2E tests that need to drive a chat turn
;; through the production board boundary before waiting on the harness.

;;; Code:

(require 'cl-lib)
(require 'e-board-registry)
(require 'e-board-runtime)
(require 'e-chat-service)
(require 'e-harness)

(defun e-board-e2e-reset-runtime ()
  "Put the batch-only board runtime in an open, catalog-ready test state."
  (setq e-board--registry (make-hash-table :test 'equal)
        e-board--id-sequence 0
        e-board-registry--boards (make-hash-table :test 'equal)
        e-board-registry--id-sequence 0
        e-board-registry--unsettled-pickup-count 0
        e-board-registry--unsettled-effect-count 0
        e-board-registry--unsettled-routing-count 0
        e-board-registry--unsettled-generation 0
        e-board-runtime--attachments (make-hash-table :test 'equal)
        e-board-runtime--session-attachments (make-hash-table :test 'equal)
        e-board-runtime--endpoint-attachments (make-hash-table :test 'equal)
        e-board-runtime--invocations (make-hash-table :test 'equal)
        e-board-runtime--producer-bindings (make-hash-table :test 'equal)
        e-board-runtime--producer-inputs (make-hash-table :test 'equal)
        e-board-runtime--producer-deliveries (make-hash-table :test 'equal)
        e-board-runtime--producer-turns (make-hash-table :test 'equal)
        e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key)
        e-chat-service--board-bindings (make-hash-table :test 'equal)
        e-chat-service--board-log-owners (make-hash-table :test 'equal)
        e-board-runtime--admission-open-p t
        e-board-runtime--quiescence-current nil
        e-board-runtime--activation-current nil
        e-board-runtime--catalog-state 'ready
        e-board-runtime--catalog-condition nil
        e-board-runtime--pending-pickup-head nil
        e-board-runtime--pending-pickup-tail nil
        e-board-runtime--pending-pickup-set (make-hash-table :test 'equal)
        e-board-runtime--pickup-drain-scheduled nil))

(cl-defun e-board-e2e-create-session (harness &key id metadata)
  "Create and bind a board-backed session in HARNESS."
  (e-board-e2e-reset-runtime)
  (plist-get (e-chat-service-create-session
              :harness harness :id id :metadata metadata)
             :id))

(defun e-board-e2e-drain-session (harness session-id)
  "Drain routing and pickup for HARNESS SESSION-ID's board."
  (let* ((binding (e-chat-service-binding harness session-id))
         (board (e-chat-service-binding-board binding)))
    (e-board-runtime--drain-input-routing
     board
     (lambda ()
       (e-board-drain-input-classifications
        (e-board-registry-board-source-board board))))
    (let ((remaining 64))
      (while (and e-board-runtime--pending-pickup-head (> remaining 0))
        (setq remaining (1- remaining))
        (e-board-runtime--drain-pickups))
      (when e-board-runtime--pending-pickup-head
        (error "E2E pickup drain exceeded its bounded page budget")))))

(defun e-board-e2e-wait-until (predicate &optional timeout)
  "Return PREDICATE's value once non-nil, or nil after TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (sit-for 0.01))
    value))

(defun e-board-e2e-prompt-async (harness session-id prompt)
  "Submit PROMPT board-first and return the resulting active turn id."
  (let ((message-id (e-chat-service-submit-session harness session-id prompt)))
    (e-board-e2e-drain-session harness session-id)
    (let ((entry (gethash session-id (e-harness-active-turns harness))))
      (when (and entry
                 (eq (plist-get entry :status) 'running)
                 (null (plist-get entry :context))
                 (null (plist-get entry :request))
                 (null (plist-get entry :timer)))
        (let* ((binding (e-chat-service-binding harness session-id))
               (source (e-board-registry-board-source-board
                        (e-chat-service-binding-board binding)))
               (message (e-board-message source message-id))
               (reasons
                (mapcar
                 (lambda (pickup-id)
                   (when-let ((attempt
                               (e-board-pickup-attempt
                                (e-board-pickup source pickup-id))))
                     (e-board-delivery-attempt-reason attempt)))
                 (e-board-message-pickup-ids message))))
          (error "Board delivery failed before provider start: %S; attempts: %S"
                 reasons
                 (mapcar
                  #'e-board-event-data
                  (cl-remove-if-not
                   (lambda (event)
                     (eq (e-board-event-type event) 'pickup-delivery-failed))
                   (e-board-events-after source 0))))))
    (unless entry
      (signal 'e-harness-no-active-turn (list session-id)))
      (plist-get entry :id))))

(defun e-board-e2e-wait-batch (harness session-id &optional timeout)
  "Run batch timers until HARNESS SESSION-ID settles, then return its entry."
  (let* ((deadline (+ (float-time) (or timeout 30.0)))
         (entry (gethash session-id (e-harness-active-turns harness))))
    (unless entry
      (signal 'e-harness-no-active-turn (list session-id)))
    ;; `accept-process-output' does not reliably dispatch zero-delay timers in
    ;; a process-free batch Emacs.  `sit-for' services both timers and process
    ;; output, which covers fake and live backends through the same helper.
    (while (and (eq (plist-get entry :status) 'running)
                (< (float-time) deadline))
      (sit-for 0.01))
    (when (eq (plist-get entry :status) 'running)
      (error "E2E turn did not settle within %.1f seconds" (or timeout 30.0)))
    ;; The queue-drain timer can remove this exact settled entry from the
    ;; active-turn table while `sit-for' dispatches timers.  Keep using the
    ;; captured plist, just like `e-harness-wait-batch', and clear the slot only
    ;; if it still points to this entry.
    (when (and (eq (gethash session-id (e-harness-active-turns harness)) entry)
               (not (e-harness--active-turn-running-p entry)))
      (e-harness--remove-active-turn harness session-id entry))
    entry))

(defun e-board-e2e-prompt-batch (harness session-id prompt &optional timeout)
  "Submit PROMPT board-first and return its settled harness result."
  (e-board-e2e-prompt-async harness session-id prompt)
  (let ((result (e-board-e2e-wait-batch harness session-id timeout)))
    (when (eq (plist-get result :status) 'error)
      (error "%s" (or (plist-get result :error) "E2E turn failed")))
    result))

(provide 'e-board-e2e-support)

;;; e-board-e2e-support.el ends here
