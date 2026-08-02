;;; e-board-runtime.el --- Harness delivery adapter for board pickups -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This composition adapter connects registry participants to existing harness
;; sessions.  Board routing remains pure: this module receives only the frozen
;; pending pickups produced by the source board and delivers them through an
;; explicit port.

;;; Code:

(require 'cl-lib)
(require 'e-board-registry)
(require 'e-harness)

(define-error 'e-board-runtime-error "e board runtime error")
(define-error 'e-board-runtime-attachment-exists
  "e board runtime participant is already attached"
  'e-board-runtime-error)
(define-error 'e-board-runtime-session-busy
  "e board runtime session is busy"
  'e-board-runtime-error)
(define-error 'e-board-runtime-session-missing
  "e board runtime session is missing"
  'e-board-runtime-error)

(defvar e-board-runtime--attachments (make-hash-table :test 'equal)
  "Live runtime attachments keyed by board and participant identity.")

(defvar e-board-runtime--session-attachments (make-hash-table :test 'equal)
  "Live attachments keyed by a concrete harness and session identity.")

(defconst e-board-runtime-deferred-hook-drain-limit 16
  "Maximum deferred carrier hooks the private runtime starts per drain.")

(defvar e-board-runtime--deferred-hooks nil
  "Generation-fenced deferred carrier hooks awaiting a runtime drain.")

(defvar e-board-runtime--deferred-hook-drain-scheduled nil
  "Non-nil while one deferred-hook runtime drain has been scheduled.")

(defvar e-board-runtime--deferred-hook-generation 0
  "Current private runtime generation for deferred carrier hook receipts.")

(defvar e-board-runtime--work-activity-mailboxes (make-hash-table :test 'equal)
  "Latest bounded activity capture for each board-enrolled work handle.")

(cl-defstruct (e-board-runtime-attachment
               (:constructor e-board-runtime-attachment--create)
               (:conc-name e-board-runtime-attachment-))
  board participant harness session-id delivery-function subscription)

(defun e-board-runtime--attachment-key (board participant)
  "Return the attachment lookup key for BOARD and PARTICIPANT."
  (list (e-board-registry-board-id board)
         (e-board-registry-participant-id participant)))

(defun e-board-runtime--session-key (harness session-id)
  "Return the runtime attachment key for HARNESS SESSION-ID."
  (list harness session-id))

(defun e-board-runtime--drain-deferred-hooks ()
  "Start one bounded page of deferred carrier hooks outside settlement.
Queue items retain the installing runtime generation, so a reload/replacement
can invalidate pending callbacks without letting an old closure advance the
new runtime.  Hook thunks are already receipt-deduplicated by `e-work'."
  (setq e-board-runtime--deferred-hook-drain-scheduled nil)
  (let ((started 0))
    (while (and e-board-runtime--deferred-hooks
                (< started e-board-runtime-deferred-hook-drain-limit))
      (pcase-let ((`(,generation ,_receipt ,thunk)
                   (pop e-board-runtime--deferred-hooks)))
        (when (= generation e-board-runtime--deferred-hook-generation)
          (cl-incf started)
          (funcall thunk))))
    (when e-board-runtime--deferred-hooks
      (setq e-board-runtime--deferred-hook-drain-scheduled t)
      (run-at-time 0 nil #'e-board-runtime--drain-deferred-hooks))))

(defun e-board-runtime--schedule-deferred-hook (_handle receipt thunk)
  "Queue deferred carrier THUNK with stable RECEIPT outside its start stack."
  (push (list e-board-runtime--deferred-hook-generation receipt thunk)
        e-board-runtime--deferred-hooks)
  (unless e-board-runtime--deferred-hook-drain-scheduled
    (setq e-board-runtime--deferred-hook-drain-scheduled t)
    (run-at-time 0 nil #'e-board-runtime--drain-deferred-hooks)))

(defun e-board-runtime--capture-work-activity (handle payload)
  "Replace HANDLE's bounded progress mailbox with PAYLOAD.
This is the sole synchronous activity observer installed by board enrollment.
It neither formats nor publishes PAYLOAD; a later runtime activity publisher
will consume the mailbox under its own bounded drain."
  (puthash (e-work-handle-id handle)
           (list :work-id (e-work-handle-id handle) :payload payload)
           e-board-runtime--work-activity-mailboxes))

(defun e-board-runtime--install-work-hooks (handle)
  "Install the private board-runtime hook classification on prepared HANDLE."
  (let ((policies '(:cancel deferred :cleanup deferred :settle deferred)))
    (dolist (key '(:on-done :on-error :on-progress :on-event))
      (when (plist-get (e-work-handle-callbacks handle) key)
        (setq policies (plist-put policies key 'deferred))))
    (when (e-work-spec-result-shaper (e-work-handle-spec handle))
      ;; Result shaping is still part of a cheap runner in this foundation.
      ;; The board runtime therefore admits it only under the narrow inline
      ;; classification; a later async shaper will use a separate work unit.
      (setq policies (plist-put policies :result-shaper 'hard-bounded)))
    (e-work-install-hook-dispatcher
     handle #'e-board-runtime--schedule-deferred-hook policies)
    (e-work-install-activity-observer
     handle #'e-board-runtime--capture-work-activity)))

(defun e-board-runtime--enroll-work (harness handle callback)
  "Enroll HANDLE for its attached HARNESS session before runner entry.
CALLBACK is the private loop result seam for executable tool work; turn work
has no callback and is observed only."
  (when-let* ((session-id (plist-get (e-work-handle-context handle) :session-id))
              (attachment (gethash (e-board-runtime--session-key harness session-id)
                                   e-board-runtime--session-attachments)))
    (let* ((board (e-board-registry-board-source-board
                   (e-board-runtime-attachment-board attachment)))
           (metadata (e-work-handle-metadata handle)))
      (e-board-runtime--install-work-hooks handle)
      (e-board-enroll-work board handle :metadata metadata)
      (when callback
        (let* ((context (e-work-handle-context handle))
               (turn-id (plist-get context :turn-id))
               (tool-call-id (plist-get (plist-get context :tool-call) :id)))
          (e-board-subscribe-invocation
           board (e-work-handle-id handle) callback
            :id (list turn-id tool-call-id)))))))

(defun e-board-runtime--subscribe-aggregation
    (harness handles mode timeout callback)
  "Subscribe attached HARNESS await work through its source board."
  (when-let* ((context (e-work-handle-context (car handles)))
              (session-id (plist-get context :session-id))
              (attachment (gethash (e-board-runtime--session-key harness session-id)
                                   e-board-runtime--session-attachments)))
    (let* ((board (e-board-registry-board-source-board
                   (e-board-runtime-attachment-board attachment)))
           (call (plist-get context :tool-call))
           (aggregation
            (e-board-subscribe-aggregation
             board (mapcar #'e-work-handle-id handles) mode callback
             :id (list (plist-get context :turn-id) (plist-get call :id))
             :timeout timeout)))
      (lambda ()
        (e-board-cancel-aggregation board (e-board-aggregation-id aggregation))))))

(defun e-board-runtime--publish-output (attachment turn-id)
  "Publish ATTACHMENT's final assistant message for TURN-ID exactly once."
  (let* ((harness (e-board-runtime-attachment-harness attachment))
         (session-id (e-board-runtime-attachment-session-id attachment))
         (message (e-harness--turn-assistant-message harness session-id turn-id)))
    (when-let ((sequence (and message
                              (plist-get message :board-output-sequence))))
      (let* ((board (e-board-registry-board-source-board
                     (e-board-runtime-attachment-board attachment)))
             (participant-id
              (e-board-registry-participant-id
               (e-board-runtime-attachment-participant attachment))))
        (e-board-post-output
         board
         :author (format "participant:%s" participant-id)
         :content (plist-get message :content)
         :source-output-key (list participant-id 1 sequence))))))

(defun e-board-runtime--handle-harness-event (attachment event)
  "Publish an attached participant's durable final output from EVENT."
  (when (eq (e-events-type event) 'turn-finished)
    (e-board-runtime--publish-output attachment (plist-get event :turn-id))))

(defun e-board-runtime--active-board (board-or-id)
  "Return active registry BOARD-OR-ID."
  (let ((board (e-board-registry-get board-or-id)))
    (unless (eq (e-board-registry-board-state board) 'active)
      (signal 'e-board-registry-closed
              (list (e-board-registry-board-id board))))
    board))

(defun e-board-runtime--require-live-session (harness session-id)
  "Return HARNESS's existing SESSION-ID, or signal a runtime-specific error."
  (condition-case nil
      (e-session-get (e-harness-sessions harness) session-id)
    (error (signal 'e-board-runtime-session-missing (list session-id)))))

(defun e-board-runtime--delivery-metadata (pickup)
  "Return harness metadata that identifies the frozen PICKUP."
  (list :input-origin 'board
        :board-delivery-id (copy-tree (e-board-pickup-delivery-id pickup))
        :board-id (e-board-pickup-board-id pickup)
        :board-participant-id (e-board-pickup-participant-id pickup)))

(defun e-board-runtime--deliver-to-harness (attachment pickup message)
  "Deliver PICKUP's MESSAGE through ATTACHMENT's idle harness session.
Queue-mode messages enter the harness follow-up queue.  Inject-mode messages
start an asynchronous prompt only while no active turn owns the session; a
busy session leaves its pickup pending for an explicit later retry."
  (let* ((harness (e-board-runtime-attachment-harness attachment))
         (session-id (e-board-runtime-attachment-session-id attachment))
         (prompt (e-board-message-content message))
         (metadata (e-board-runtime--delivery-metadata pickup)))
    (unless (and (stringp prompt) (not (string-empty-p prompt)))
      (user-error "Board input content must be a non-empty string"))
    (when (plist-get (e-harness-state harness session-id) :active-turn)
      (signal 'e-board-runtime-session-busy (list session-id)))
    (pcase (e-board-pickup-mode pickup)
      ('queue
       (e-harness-request-follow-up harness session-id prompt :metadata metadata))
      ('inject
       (e-harness-prompt-async harness session-id prompt :metadata metadata)))))

(cl-defun e-board-runtime-attach
    (board-or-id harness session-id
                 &key participant-id author principal delivery-function)
  "Attach existing live HARNESS SESSION-ID to BOARD-OR-ID as one participant.

DELIVERY-FUNCTION is called as (FUNCTION ATTACHMENT PICKUP MESSAGE) for each
frozen pending pickup addressed to the participant.  It must perform one
delivery or signal; normal return marks that pickup delivered.  When omitted,
the conservative idle-only harness delivery port is used."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (or (null delivery-function) (functionp delivery-function))
    (signal 'wrong-type-argument (list 'functionp delivery-function)))
  (e-board-runtime--require-live-session harness session-id)
  (let* ((board (e-board-runtime--active-board board-or-id))
         (participant (e-board-registry-add-participant
                       board :id participant-id :author author :principal principal))
         (key (e-board-runtime--attachment-key board participant)))
    (when (gethash key e-board-runtime--attachments)
      (signal 'e-board-runtime-attachment-exists (list key)))
    (let ((attachment
           (e-board-runtime-attachment--create
            :board board
            :participant participant
             :harness harness
             :session-id session-id
              :delivery-function (or delivery-function
                                     #'e-board-runtime--deliver-to-harness))))
       (setf (e-board-runtime-attachment-subscription attachment)
             (e-harness-subscribe
              harness
              (lambda (event)
                (e-board-runtime--handle-harness-event attachment event))
              :session-id session-id))
        (puthash key attachment e-board-runtime--attachments)
        (puthash (e-board-runtime--session-key harness session-id) attachment
                 e-board-runtime--session-attachments)
        (setf (e-board-effect-scheduler
               (e-board-registry-board-source-board board))
              (lambda (effect)
                (run-at-time
                 0 nil
                 (lambda ()
                   (funcall effect)
                   (e-board-runtime--deliver-pending-pickups board)))))
         (e-harness-set-work-enrollment-function
         harness
         (lambda (handle &optional callback)
           (e-board-runtime--enroll-work harness handle callback)))
        (e-harness-set-board-aggregation-function
         harness
         (lambda (handles mode timeout callback)
           (e-board-runtime--subscribe-aggregation
            harness handles mode timeout callback)))
        attachment)))

(defun e-board-runtime--deliver-pickups (board pickup-ids)
  "Deliver BOARD's frozen pending PICKUP-IDS through their attachments."
  (let ((source-board (e-board-registry-board-source-board board)))
    (dolist (delivery-id pickup-ids)
      (when-let* ((pickup (e-board-pickup source-board delivery-id))
                  ((eq (e-board-pickup-state pickup) 'pending))
                  (participant (e-board-registry-participant
                                board (e-board-pickup-participant-id pickup)))
                  (attachment
                   (gethash (e-board-runtime--attachment-key board participant)
                            e-board-runtime--attachments))
                  (message (e-board-message source-board
                                            (e-board-pickup-message-id pickup))))
        (funcall (e-board-runtime-attachment-delivery-function attachment)
                 attachment pickup message)
         (setf (e-board-pickup-state pickup) 'delivered)))))

(defun e-board-runtime--deliver-pending-pickups (board)
  "Attempt delivery for BOARD's already-frozen pending pickups.
This runs after a deferred effect commits its input append; the effect succeeds
at append time, independently of downstream session acceptance."
  (let (pickup-ids)
    (maphash
     (lambda (delivery-id pickup)
       (when (eq (e-board-pickup-state pickup) 'pending)
         (push delivery-id pickup-ids)))
     (e-board-pickups (e-board-registry-board-source-board board)))
    (e-board-runtime--deliver-pickups board (nreverse pickup-ids))))

(cl-defun e-board-runtime-post-input
    (board-or-id &key id author tags attributes to (mode 'inject) content reference source-input-key)
  "Post one input to BOARD-OR-ID's source board, then deliver its frozen pickups.
The returned value is the source board's `e-board-publication'.  Duplicate
publications only retry pickups that remain pending."
  (let* ((board (e-board-runtime--active-board board-or-id))
         (publication
          (e-board-post-input
           (e-board-registry-board-source-board board)
            :id id :author author :tags tags :attributes attributes :to to :mode mode :content content
           :reference reference :source-input-key source-input-key)))
    (e-board-runtime--deliver-pickups board
                                      (e-board-publication-pickup-ids publication))
    publication))

(provide 'e-board-runtime)

;;; e-board-runtime.el ends here
