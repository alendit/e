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

(defvar e-board-runtime--invocations (make-hash-table :test 'equal)
  "Exact invocation effect targets owned by their original attachment.")

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

(defconst e-board-runtime-activity-drain-limit 16
  "Maximum latest-value work activity mailboxes published per drain.")

(defvar e-board-runtime--pending-activity-head nil
  "Head cell of work ids whose latest activity mailbox needs a flush.")

(defvar e-board-runtime--pending-activity-tail nil
  "Tail cell of work ids whose latest activity mailbox needs a flush.")

(defvar e-board-runtime--pending-activity-set (make-hash-table :test 'equal)
  "Deduplication set for scheduled work activity mailbox flushes.")

(defvar e-board-runtime--activity-drain-scheduled nil
  "Non-nil while the runtime has one activity mailbox drain pending.")

(defconst e-board-runtime-pickup-drain-limit 16
  "Maximum frozen pickup attempts the private runtime starts per drain.")

(defvar e-board-runtime--pending-pickup-head nil
  "Head cell of the FIFO queue of pending board pickup identities.")

(defvar e-board-runtime--pending-pickup-tail nil
  "Tail cell of the FIFO queue of pending board pickup identities.")

(defvar e-board-runtime--pending-pickup-set (make-hash-table :test 'equal)
  "Deduplication set for runtime pickup identities awaiting an attempt.")

(defvar e-board-runtime--pickup-drain-scheduled nil
  "Non-nil while the runtime has one pickup-drain timer pending.")

(cl-defstruct (e-board-runtime-attachment
               (:constructor e-board-runtime-attachment--create)
               (:conc-name e-board-runtime-attachment-))
  board participant harness session-id delivery-function subscription activity-sequence generation)

(cl-defstruct (e-board-runtime-invocation
               (:constructor e-board-runtime-invocation--create)
               (:conc-name e-board-runtime-invocation-))
  target attachment attachment-generation callback state)

(defun e-board-runtime--attachment-key (board participant)
  "Return the attachment lookup key for BOARD and PARTICIPANT."
  (list (e-board-registry-board-id board)
         (e-board-registry-participant-id participant)))

(defun e-board-runtime--session-key (harness session-id)
  "Return the runtime attachment key for HARNESS SESSION-ID."
  (list harness session-id))

(defun e-board-runtime--invocation-target (attachment turn-id tool-call-id)
  "Return ATTACHMENT's immutable target for TURN-ID and TOOL-CALL-ID."
  (list (e-board-registry-board-id (e-board-runtime-attachment-board attachment))
        (e-board-registry-participant-id
         (e-board-runtime-attachment-participant attachment))
        turn-id tool-call-id))

(defun e-board-runtime--register-invocation
    (attachment turn-id tool-call-id callback)
  "Capture CALLBACK behind one exact invocation target before work starts.
The target retains the original live ATTACHMENT and its generation.  Later
effects may use that capture or fail visibly; they never resolve a replacement
endpoint by session identity."
  (unless (and turn-id tool-call-id (functionp callback))
    (signal 'e-board-runtime-error
            (list "Invocation requires turn id, tool call id, and callback")))
  (let ((target (e-board-runtime--invocation-target
                 attachment turn-id tool-call-id)))
    (when (gethash target e-board-runtime--invocations)
      (signal 'e-board-runtime-error (list "Invocation target already exists" target)))
    (puthash target
             (e-board-runtime-invocation--create
              :target target
              :attachment attachment
              :attachment-generation (e-board-runtime-attachment-generation attachment)
              :callback callback
              :state 'open)
             e-board-runtime--invocations)
    target))

(defun e-board-runtime--apply-invocation-effect (_board target state payload)
  "Apply TARGET exactly once through its captured runtime invocation service."
  (let ((invocation (gethash target e-board-runtime--invocations)))
    (unless invocation
      (signal 'e-board-runtime-error (list "Unknown invocation target" target)))
    (unless (eq (e-board-runtime-invocation-state invocation) 'open)
      (signal 'e-board-runtime-error (list "Invocation target is not open" target)))
    (let ((attachment (e-board-runtime-invocation-attachment invocation)))
      (unless (= (e-board-runtime-invocation-attachment-generation invocation)
                 (e-board-runtime-attachment-generation attachment))
        (setf (e-board-runtime-invocation-state invocation) 'unavailable)
        (signal 'e-board-runtime-error
                (list "Original invocation endpoint is unavailable" target)))
      (setf (e-board-runtime-invocation-state invocation) 'applying)
      (condition-case err
          (progn
            (funcall (e-board-runtime-invocation-callback invocation) state payload)
            (setf (e-board-runtime-invocation-state invocation) 'committed))
        (error
         (setf (e-board-runtime-invocation-state invocation) 'failed)
         (signal (car err) (cdr err)))))))

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

(defun e-board-runtime--enqueue-activity-flush (work-id)
  "Queue one future flush for WORK-ID without retaining each raw update."
  (unless (gethash work-id e-board-runtime--pending-activity-set)
    (puthash work-id t e-board-runtime--pending-activity-set)
    (let ((cell (list work-id)))
      (if e-board-runtime--pending-activity-tail
          (setcdr e-board-runtime--pending-activity-tail cell)
        (setq e-board-runtime--pending-activity-head cell))
      (setq e-board-runtime--pending-activity-tail cell)))
  (unless e-board-runtime--activity-drain-scheduled
    (setq e-board-runtime--activity-drain-scheduled t)
    (run-at-time 0 nil #'e-board-runtime--drain-activity-mailboxes)))

(defun e-board-runtime--activity-content (payload)
  "Return a bounded diagnostic rendering of raw activity PAYLOAD."
  (truncate-string-to-width (e-prin1-safe payload) 512 nil nil "..."))

(defun e-board-runtime--drain-activity-mailboxes ()
  "Publish one bounded page of latest work activity mailbox snapshots."
  (setq e-board-runtime--activity-drain-scheduled nil)
  (let ((published 0))
    (while (and e-board-runtime--pending-activity-head
                (< published e-board-runtime-activity-drain-limit))
      (let* ((work-id (pop e-board-runtime--pending-activity-head))
             (mailbox (gethash work-id e-board-runtime--work-activity-mailboxes)))
        (unless e-board-runtime--pending-activity-head
          (setq e-board-runtime--pending-activity-tail nil))
        (remhash work-id e-board-runtime--pending-activity-set)
        (remhash work-id e-board-runtime--work-activity-mailboxes)
        (when mailbox
          (cl-incf published)
          (let* ((attachment (plist-get mailbox :attachment))
                 (board (e-board-registry-board-source-board
                         (e-board-runtime-attachment-board attachment)))
                 (participant-id
                  (e-board-registry-participant-id
                   (e-board-runtime-attachment-participant attachment))))
            (e-board-post-activity
             board
             :author (format "participant:%s" participant-id)
             :subject-participant-id participant-id
             :source-turn-id (plist-get mailbox :turn-id)
             :activity-kind 'work-progress
             :attributes (list :work-id work-id)
             :content (e-board-runtime--activity-content
                       (plist-get mailbox :payload))
             :source-activity-key (plist-get mailbox :source-key))))))
    (when e-board-runtime--pending-activity-head
      (setq e-board-runtime--activity-drain-scheduled t)
      (run-at-time 0 nil #'e-board-runtime--drain-activity-mailboxes))))

(defun e-board-runtime--capture-work-activity (attachment handle payload)
  "Replace HANDLE's bounded progress mailbox with PAYLOAD.
This is the sole synchronous activity observer installed by board enrollment.
It neither formats nor publishes PAYLOAD; a later runtime activity publisher
will consume the mailbox under its own bounded drain."
  (let* ((work-id (e-work-handle-id handle))
         (participant-id
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant attachment)))
         (sequence (cl-incf (e-board-runtime-attachment-activity-sequence attachment))))
    (puthash work-id
             (list :attachment attachment
                   :turn-id (plist-get (e-work-handle-context handle) :turn-id)
                   :payload payload
                   :source-key (list participant-id 1 sequence))
             e-board-runtime--work-activity-mailboxes)
    (e-board-runtime--enqueue-activity-flush work-id)))

(defun e-board-runtime--install-work-hooks (attachment handle)
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
     handle (lambda (current-handle payload)
              (e-board-runtime--capture-work-activity
               attachment current-handle payload)))))

(defun e-board-runtime--pickup-queue-key (board pickup-id)
  "Return the process-local queue identity for BOARD's PICKUP-ID."
  (list (e-board-registry-board-id board) pickup-id))

(defun e-board-runtime--enqueue-pickups (board pickup-ids)
  "Enqueue BOARD PICKUP-IDS once; never deliver on an append/effect stack."
  (dolist (pickup-id pickup-ids)
    (let ((key (e-board-runtime--pickup-queue-key board pickup-id)))
      (unless (gethash key e-board-runtime--pending-pickup-set)
        (puthash key t e-board-runtime--pending-pickup-set)
        (let ((cell (list key)))
          (if e-board-runtime--pending-pickup-tail
              (setcdr e-board-runtime--pending-pickup-tail cell)
            (setq e-board-runtime--pending-pickup-head cell))
          (setq e-board-runtime--pending-pickup-tail cell)))))
  (unless e-board-runtime--pickup-drain-scheduled
    (setq e-board-runtime--pickup-drain-scheduled t)
    (run-at-time 0 nil #'e-board-runtime--drain-pickups)))

(defun e-board-runtime--drain-pickups ()
  "Attempt one bounded FIFO page of previously frozen pickup envelopes."
  (setq e-board-runtime--pickup-drain-scheduled nil)
  (let ((attempts 0))
    (while (and e-board-runtime--pending-pickup-head
                (< attempts e-board-runtime-pickup-drain-limit))
      (let ((key (pop e-board-runtime--pending-pickup-head)))
        (unless e-board-runtime--pending-pickup-head
          (setq e-board-runtime--pending-pickup-tail nil))
        (remhash key e-board-runtime--pending-pickup-set)
        (let ((board (condition-case nil
                         (e-board-registry-get (car key))
                       (e-board-registry-missing nil))))
          (when board
            (cl-incf attempts)
            (e-board-runtime--deliver-pickups board (list (cadr key)))))))
    (when e-board-runtime--pending-pickup-head
      (setq e-board-runtime--pickup-drain-scheduled t)
      (run-at-time 0 nil #'e-board-runtime--drain-pickups))))

(defun e-board-runtime--drain-input-routing (board drain)
  "Run BOARD's bounded classifier, then queue only its finalized pickups."
  (funcall drain)
  (dolist (result (e-board-drain-routed-pickups
                   (e-board-registry-board-source-board board)))
    (e-board-runtime--enqueue-pickups board (cadr result))))

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
      (e-board-runtime--install-work-hooks attachment handle)
      (if callback
          (let* ((context (e-work-handle-context handle))
                 (turn-id (plist-get context :turn-id))
                 (tool-call-id (plist-get (plist-get context :tool-call) :id))
                 (invocation-id (list turn-id tool-call-id))
                 ;; Capturing this service first means an invalid tool-call
                 ;; identity cannot leave an enrolled board operation behind.
                 (target (e-board-runtime--register-invocation
                          attachment turn-id tool-call-id callback)))
          (condition-case err
              (e-board-enroll-invocation-work
               board handle invocation-id target :metadata metadata)
            (error
             (remhash target e-board-runtime--invocations)
             (signal (car err) (cdr err)))))
        (e-board-enroll-work board handle :metadata metadata)))))

(defun e-board-runtime--subscribe-aggregation
    (harness handles mode timeout callback invocation-context)
  "Subscribe attached HARNESS await work through its source board."
  (when-let* ((session-id (plist-get invocation-context :session-id))
              (attachment (gethash (e-board-runtime--session-key harness session-id)
                                   e-board-runtime--session-attachments)))
    (let* ((board (e-board-registry-board-source-board
                   (e-board-runtime-attachment-board attachment)))
           (call (plist-get invocation-context :tool-call))
           (turn-id (plist-get invocation-context :turn-id))
           (invocation-id (list turn-id (plist-get call :id)))
           (target (e-board-runtime--register-invocation
                    attachment turn-id (plist-get call :id)
                    (lambda (_state reason) (funcall callback reason))))
           aggregation)
      (condition-case err
          (setq aggregation
                (e-board-subscribe-aggregation
                 board (mapcar #'e-work-handle-id handles) mode target
                 :id invocation-id :timeout timeout))
        (error
         (remhash target e-board-runtime--invocations)
         (signal (car err) (cdr err))))
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
         :subject-participant-id participant-id
         :source-turn-id turn-id
         :content (plist-get message :content)
         :source-output-key (list participant-id 1 sequence))))))

(defun e-board-runtime--handle-harness-event (attachment event)
  "Publish attached output and reconcile board-delivery receipts from EVENT."
  (let ((type (e-events-type event)))
    (cond
     ((eq type 'turn-finished)
      (e-board-runtime--publish-output attachment (plist-get event :turn-id)))
     ((eq type 'input-consumed)
      (let* ((payload (plist-get event :payload))
             (delivery-id (plist-get payload :delivery-id))
             (registry-board (e-board-runtime-attachment-board attachment))
             (board (e-board-registry-board-source-board registry-board))
             (pickup (e-board-pickup board delivery-id)))
        (when (and pickup (memq (e-board-pickup-state pickup) '(accepted cancelling)))
          (when-let ((next-id (e-board-pickup-complete-delivery board delivery-id)))
            (e-board-runtime--enqueue-pickups registry-board (list next-id))))))
     ((eq type 'session-reset)
      (let* ((registry-board (e-board-runtime-attachment-board attachment))
             (board (e-board-registry-board-source-board registry-board))
             (participant-id (e-board-registry-participant-id
                              (e-board-runtime-attachment-participant attachment)))
             (delivery-id (car (e-board--pickup-queue board participant-id)))
             (pickup (and delivery-id (e-board-pickup board delivery-id))))
        (when (and pickup (memq (e-board-pickup-state pickup) '(accepted cancelling)))
          (when-let ((next-id
                      (e-board-pickup-discard-delivery board delivery-id 'session-reset)))
            (e-board-runtime--enqueue-pickups registry-board (list next-id)))))))))

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
       (list :accepted
             (e-harness-request-follow-up harness session-id prompt :metadata metadata)))
      ('inject
       (e-harness-prompt-async harness session-id prompt :metadata metadata)
       :consumed))))

(cl-defun e-board-runtime-attach
    (board-or-id harness session-id
                 &key participant-id author principal delivery-function)
  "Attach existing live HARNESS SESSION-ID to BOARD-OR-ID as one participant.

DELIVERY-FUNCTION is called as (FUNCTION ATTACHMENT PICKUP MESSAGE) for each
frozen pending pickup addressed to the participant.  It must perform one
delivery or signal; normal return marks that pickup delivered.  Returning
=(:accepted RECEIPT)= waits for a later consumption receipt; returning
=(:uncertain REASON)= records an ambiguous original-endpoint attempt without
retrying it; returning =(:discarded REASON)= records a proven non-commit;
returning =(:failed REASON)= records a permanent endpoint rejection.
When omitted, the conservative idle-only harness delivery port is used."
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
            :activity-sequence 0
            :generation 1
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
                   (funcall effect)))))
        (setf (e-board-invocation-effect-dispatcher
               (e-board-registry-board-source-board board))
              #'e-board-runtime--apply-invocation-effect)
        (setf (e-board-input-classification-scheduler
               (e-board-registry-board-source-board board))
              (lambda (drain)
                (run-at-time 0 nil
                             (lambda ()
                               (e-board-runtime--drain-input-routing board drain)))))
         (e-harness-set-work-enrollment-function
         harness
         (lambda (handle &optional callback)
           (e-board-runtime--enroll-work harness handle callback)))
        (e-harness-set-board-aggregation-function
         harness
         (lambda (handles mode timeout callback invocation-context)
           (e-board-runtime--subscribe-aggregation
            harness handles mode timeout callback invocation-context)))
        attachment)))

(defun e-board-runtime--deliver-pickups (board pickup-ids)
  "Deliver BOARD's frozen ready PICKUP-IDS through their attachments."
  (let ((source-board (e-board-registry-board-source-board board)))
    (dolist (delivery-id pickup-ids)
      (when-let* ((pickup (e-board-pickup source-board delivery-id))
                  ((eq (e-board-pickup-state pickup) 'ready))
                  (participant (e-board-registry-participant
                                board (e-board-pickup-participant-id pickup)))
                  (attachment
                   (gethash (e-board-runtime--attachment-key board participant)
                            e-board-runtime--attachments))
                  (message (e-board-message source-board
                                            (e-board-pickup-message-id pickup))))
        (e-board-pickup-start-delivery source-board delivery-id)
        (condition-case err
            (let ((result (funcall (e-board-runtime-attachment-delivery-function attachment)
                                   attachment pickup message)))
              (pcase (car-safe result)
                (:accepted
                 (e-board-pickup-accept-delivery source-board delivery-id))
                (:uncertain
                 (when-let ((next-id
                             (e-board-pickup-mark-uncertain
                              source-board delivery-id (or (cadr result) 'delivery-uncertain))))
                   (e-board-runtime--enqueue-pickups board (list next-id))))
                (:discarded
                 (e-board-pickup-accept-delivery source-board delivery-id)
                 (when-let ((next-id
                             (e-board-pickup-discard-delivery
                              source-board delivery-id (or (cadr result) 'delivery-discarded))))
                   (e-board-runtime--enqueue-pickups board (list next-id))))
                (:failed
                 (when-let ((next-id
                             (e-board-fail-pickup
                              source-board delivery-id (or (cadr result) 'delivery-failed))))
                   (e-board-runtime--enqueue-pickups board (list next-id))))
                (_
                 (when-let ((next-id (e-board-pickup-complete-delivery
                                      source-board delivery-id)))
                   (e-board-runtime--enqueue-pickups board (list next-id))))))
          (error
           (e-board-pickup-return-ready source-board delivery-id err)
           (signal (car err) (cdr err))))))))

(cl-defun e-board-runtime-post-input
    (board-or-id &key id author tags attributes to (mode 'inject) content reference source-input-key)
  "Post one input to BOARD-OR-ID's source board and enqueue its frozen pickups.
The returned value is the source board's `e-board-publication'.  Duplicate
publications only retry pickups that remain pending."
  (let* ((board (e-board-runtime--active-board board-or-id))
         (publication
          (e-board-post-input
           (e-board-registry-board-source-board board)
            :id id :author author :tags tags :attributes attributes :to to :mode mode :content content
           :reference reference :source-input-key source-input-key)))
    (e-board-runtime--enqueue-pickups
     board (e-board-publication-pickup-ids publication))
    publication))

(provide 'e-board-runtime)

;;; e-board-runtime.el ends here
