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
(require 'e-harness-instances)
(require 'e-harness-registry)

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
(define-error 'e-board-runtime-instance-ineligible
  "e board runtime harness instance is not eligible for session attachment"
  'e-board-runtime-error)

(defvar e-board-runtime--attachments (make-hash-table :test 'equal)
  "Live runtime attachments keyed by board and participant identity.")

(defvar e-board-runtime--session-attachments (make-hash-table :test 'equal)
  "Live attachments keyed by stable session attachment identity.")

(defvar e-board-runtime--endpoint-attachments (make-hash-table :test 'equal)
  "Live attachments keyed by concrete harness object and session identity.")

(defvar e-board-runtime--invocations (make-hash-table :test 'equal)
  "Exact invocation effect targets owned by their original attachment.")

(defconst e-board-runtime-deferred-hook-drain-limit 16
  "Maximum deferred carrier hooks the private runtime starts per drain.")

(defvar e-board-runtime--deferred-hook-head nil
  "Head cell of the FIFO of generation-fenced deferred carrier hooks.")

(defvar e-board-runtime--deferred-hook-tail nil
  "Tail cell of the FIFO of generation-fenced deferred carrier hooks.")

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

(defconst e-board-runtime--visible-harness-activity-types
  '(turn-started provider-request-started provider-request-finished
    tool-started tool-finished action-started action-finished action-failed
    turn-steered compaction-started compaction-finished compaction-failed)
  "Harness lifecycle edges that are safe to expose as board activity.
Raw event payloads and high-frequency reasoning or progress edges stay outside
the board transcript.  Terminal events use their dedicated publisher below.")

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
  board participant harness session-id delivery-function subscription activity-sequence generation
  turn-activity instance-id instance-catalog-generation harness-id harness-object-generation
  session-store-id endpoint-token)

(cl-defstruct (e-board-runtime-endpoint-token
               (:constructor e-board-runtime-endpoint-token--create)
               (:conc-name e-board-runtime-endpoint-token-))
  harness-id harness-object-generation session-store-id session-id)

(cl-defstruct (e-board-runtime-turn-activity
               (:constructor e-board-runtime-turn-activity--create)
               (:conc-name e-board-runtime-turn-activity-))
  provider-seen provider-started-at tool-count action-count)

(cl-defstruct (e-board-runtime-invocation
               (:constructor e-board-runtime-invocation--create)
               (:conc-name e-board-runtime-invocation-))
  target attachment attachment-generation callback state)

(defun e-board-runtime--attachment-key (board participant)
  "Return the attachment lookup key for BOARD and PARTICIPANT."
  (list (e-board-registry-board-id board)
         (e-board-registry-participant-id participant)))

(defun e-board-runtime--session-key (harness session-id)
  "Return the concrete endpoint lookup key for HARNESS SESSION-ID."
  (list harness session-id))

(defun e-board-runtime--resolved-session-attachment-key
    (harness session-id session-store-id)
  "Return stable reverse key for one resolved session endpoint."
  (if session-store-id
      (list 'session-store session-store-id session-id)
    (e-board-runtime--session-key harness session-id)))

(defun e-board-runtime--attachment-session-key (attachment)
  "Return ATTACHMENT's stable reverse session identity."
  (e-board-runtime--resolved-session-attachment-key
   (e-board-runtime-attachment-harness attachment)
   (e-board-runtime-attachment-session-id attachment)
   (e-board-runtime-attachment-session-store-id attachment)))

(defun e-board-runtime--invocation-target (attachment turn-id tool-call-id)
  "Return ATTACHMENT's immutable target for TURN-ID and TOOL-CALL-ID."
  (list (e-board-registry-board-id (e-board-runtime-attachment-board attachment))
        (e-board-registry-participant-id
         (e-board-runtime-attachment-participant attachment))
        turn-id tool-call-id))

(defun e-board-runtime--current-attachment-p (attachment)
  "Return non-nil when ATTACHMENT still owns its board participant endpoint."
  (let* ((board (e-board-runtime-attachment-board attachment))
         (participant (e-board-runtime-attachment-participant attachment))
         (instance-id (e-board-runtime-attachment-instance-id attachment))
         (harness-id (e-board-runtime-attachment-harness-id attachment)))
    (and (or (null instance-id)
             (and (equal
                   (e-board-runtime-attachment-instance-catalog-generation
                    attachment)
                   (e-harness-instance-generation))
                  (e-harness-instance-get instance-id)))
         (or (null harness-id)
             (and (equal
                   (e-board-runtime-attachment-harness-object-generation
                    attachment)
                   (e-harness-registry-generation harness-id))
                  (eq (e-board-runtime-attachment-harness attachment)
                      (e-harness-registry-get harness-id))))
         (eq (gethash (e-board-runtime--attachment-key board participant)
                      e-board-runtime--attachments)
             attachment)
         (eq (gethash (e-board-runtime--attachment-session-key attachment)
                      e-board-runtime--session-attachments)
             attachment)
         (eq (gethash (e-board-runtime--session-key
                       (e-board-runtime-attachment-harness attachment)
                       (e-board-runtime-attachment-session-id attachment))
                      e-board-runtime--endpoint-attachments)
             attachment))))

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
  (let ((processed 0))
    (while (and e-board-runtime--deferred-hook-head
                (< processed e-board-runtime-deferred-hook-drain-limit))
      (pcase-let ((`(,generation ,_receipt ,thunk)
                   (pop e-board-runtime--deferred-hook-head)))
        (unless e-board-runtime--deferred-hook-head
          (setq e-board-runtime--deferred-hook-tail nil))
        (cl-incf processed)
        (when (= generation e-board-runtime--deferred-hook-generation)
          (funcall thunk))))
    (when e-board-runtime--deferred-hook-head
      (setq e-board-runtime--deferred-hook-drain-scheduled t)
      (run-at-time 0 nil #'e-board-runtime--drain-deferred-hooks))))

(defun e-board-runtime--schedule-deferred-hook (_handle receipt thunk)
  "Queue deferred carrier THUNK with stable RECEIPT outside its start stack."
  (let ((cell (list (list e-board-runtime--deferred-hook-generation receipt thunk))))
    (if e-board-runtime--deferred-hook-tail
        (setcdr e-board-runtime--deferred-hook-tail cell)
      (setq e-board-runtime--deferred-hook-head cell))
    (setq e-board-runtime--deferred-hook-tail cell))
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
  (let ((processed 0))
    (while (and e-board-runtime--pending-activity-head
                (< processed e-board-runtime-activity-drain-limit))
      (let* ((work-id (pop e-board-runtime--pending-activity-head))
             (mailbox (gethash work-id e-board-runtime--work-activity-mailboxes)))
        (unless e-board-runtime--pending-activity-head
          (setq e-board-runtime--pending-activity-tail nil))
        (remhash work-id e-board-runtime--pending-activity-set)
        (remhash work-id e-board-runtime--work-activity-mailboxes)
        (cl-incf processed)
        (when mailbox
          (let* ((attachment (plist-get mailbox :attachment))
                 (board (e-board-registry-board-source-board
                         (e-board-runtime-attachment-board attachment)))
                 (participant-id
                  (e-board-registry-participant-id
                   (e-board-runtime-attachment-participant attachment))))
            (when (e-board-runtime--current-attachment-p attachment)
              (e-board-post-activity
               board
               :author (format "participant:%s" participant-id)
               :subject-participant-id participant-id
               :source-turn-id (plist-get mailbox :turn-id)
               :activity-kind 'work-progress
               :attributes (list :work-id work-id)
               :content (e-board-runtime--activity-content
                         (plist-get mailbox :payload))
               :source-activity-key (plist-get mailbox :source-key)))))))
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
                   :source-key
                   (list participant-id
                         (e-board-runtime-attachment-generation attachment)
                         sequence))
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
  (let ((processed 0))
    (while (and e-board-runtime--pending-pickup-head
                (< processed e-board-runtime-pickup-drain-limit))
      (let ((key (pop e-board-runtime--pending-pickup-head)))
        (unless e-board-runtime--pending-pickup-head
          (setq e-board-runtime--pending-pickup-tail nil))
        (remhash key e-board-runtime--pending-pickup-set)
        (cl-incf processed)
        (let ((board (condition-case nil
                         (e-board-registry-get (car key))
                       (e-board-registry-missing nil))))
          (when board
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
                                   e-board-runtime--endpoint-attachments)))
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
                                   e-board-runtime--endpoint-attachments)))
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
        (e-board-cancel-aggregation board (e-board-aggregation-id aggregation))
        (remhash target e-board-runtime--invocations)))))

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
         :source-output-key
         (list participant-id
               (e-board-runtime-attachment-generation attachment)
               sequence))))))

(defun e-board-runtime--enqueue-ready-participant-pickup (attachment)
  "Queue ATTACHMENT's current ready FIFO head, if it has one.
This is the runtime-side wake edge for a pickup that could not enter a busy
session earlier.  It reads only the owning participant's head rather than
scanning a board-wide delivery table."
  (let* ((registry-board (e-board-runtime-attachment-board attachment))
         (board (e-board-registry-board-source-board registry-board))
         (participant-id
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant attachment)))
         (delivery-id (car (e-board--pickup-queue board participant-id)))
         (pickup (and delivery-id (e-board-pickup board delivery-id))))
    (when (and pickup (eq (e-board-pickup-state pickup) 'ready))
      (e-board-runtime--enqueue-pickups registry-board (list delivery-id)))))

(defun e-board-runtime--event-activity-source-key (attachment event publication-kind)
  "Return EVENT's stable board source key for PUBLICATION-KIND.
The durable session activity sequence is the retry identity when available.
The locally allocated fallback only serves legacy or synthetic events that have
no durable activity entry.  One source event may publish more than one derived
row, so its numeric key reserves an adjacent slot for the terminal summary."
  (let* ((participant-id
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant attachment)))
         (durable-sequence (plist-get event :board-activity-sequence))
         (sequence
          (if durable-sequence
              (+ (* 2 durable-sequence)
                 (if (eq publication-kind 'turn-summary) 1 0))
            (cl-incf (e-board-runtime-attachment-activity-sequence attachment)))))
    (list participant-id
          (e-board-runtime-attachment-generation attachment)
          sequence)))

(defun e-board-runtime--publish-terminal-activity (attachment event)
  "Publish EVENT's failure or cancellation as one terminal board activity.
Successful turns publish their final output through the separate output seam;
these terminal states have no output to close the board-owned open projection."
  (let* ((registry-board (e-board-runtime-attachment-board attachment))
         (board (e-board-registry-board-source-board registry-board))
         (participant-id (e-board-registry-participant-id
                          (e-board-runtime-attachment-participant attachment)))
         (turn-id (plist-get event :turn-id))
         (activity-kind (e-events-type event)))
    (when (and turn-id (memq activity-kind '(turn-failed turn-cancelled)))
      (e-board-post-activity
       board :author (format "participant:%s" participant-id)
       :subject-participant-id participant-id :source-turn-id turn-id
       :activity-kind activity-kind
       :attributes (when-let ((source-event-id
                               (plist-get event :activity-entry-id)))
                     (list :source-event-id source-event-id))
       :source-activity-key
       (e-board-runtime--event-activity-source-key attachment event activity-kind)))))

(defun e-board-runtime--publish-harness-activity (attachment event)
  "Publish EVENT's bounded lifecycle edge without exposing its raw payload."
  (let ((activity-kind (e-events-type event))
        (turn-id (plist-get event :turn-id)))
    (when (and turn-id
               (memq activity-kind e-board-runtime--visible-harness-activity-types))
      (let* ((registry-board (e-board-runtime-attachment-board attachment))
             (board (e-board-registry-board-source-board registry-board))
             (participant-id
              (e-board-registry-participant-id
               (e-board-runtime-attachment-participant attachment))))
        (e-board-post-activity
         board :author (format "participant:%s" participant-id)
         :subject-participant-id participant-id :source-turn-id turn-id
         :activity-kind activity-kind
         :attributes (when-let ((source-event-id
                                 (plist-get event :activity-entry-id)))
                       (list :source-event-id source-event-id))
         :source-activity-key
         (e-board-runtime--event-activity-source-key attachment event activity-kind))))))

(defun e-board-runtime--turn-activity (attachment turn-id)
  "Return ATTACHMENT's bounded activity accumulator for TURN-ID."
  (or (gethash turn-id (e-board-runtime-attachment-turn-activity attachment))
      (let ((state (e-board-runtime-turn-activity--create
                    :tool-count 0 :action-count 0)))
        (puthash turn-id state (e-board-runtime-attachment-turn-activity attachment))
        state)))

(defun e-board-runtime--observe-turn-activity (attachment event)
  "Record the narrow summary fields exposed by one attached harness EVENT."
  (when-let ((turn-id (plist-get event :turn-id)))
    (let ((state (e-board-runtime--turn-activity attachment turn-id)))
      (pcase (e-events-type event)
        ('provider-request-started
         (setf (e-board-runtime-turn-activity-provider-seen state) t)
         (unless (e-board-runtime-turn-activity-provider-started-at state)
           (setf (e-board-runtime-turn-activity-provider-started-at state)
                 (plist-get event :created-at))))
        ('tool-started
         (cl-incf (e-board-runtime-turn-activity-tool-count state)))
        ('action-started
         (cl-incf (e-board-runtime-turn-activity-action-count state)))))))

(defun e-board-runtime--publish-turn-summary (attachment event status)
  "Publish one terminal summary for provider-active TURN-ID, then release state."
  (when-let* ((turn-id (plist-get event :turn-id))
              (state (gethash turn-id (e-board-runtime-attachment-turn-activity attachment))))
    (remhash turn-id (e-board-runtime-attachment-turn-activity attachment))
    (when (e-board-runtime-turn-activity-provider-seen state)
      (let* ((registry-board (e-board-runtime-attachment-board attachment))
             (board (e-board-registry-board-source-board registry-board))
             (participant-id (e-board-registry-participant-id
                              (e-board-runtime-attachment-participant attachment))))
        (e-board-post-activity
         board :author (format "participant:%s" participant-id)
         :subject-participant-id participant-id :source-turn-id turn-id
         :activity-kind 'turn-summary
         :attributes
         (append (list :status status
                       :duration-seconds
                       (max 0 (- (plist-get event :created-at)
                                 (e-board-runtime-turn-activity-provider-started-at state)))
                       :tool-count (e-board-runtime-turn-activity-tool-count state)
                       :action-count (e-board-runtime-turn-activity-action-count state))
                 (when-let ((source-event-id
                             (plist-get event :activity-entry-id)))
                   (list :source-event-id source-event-id)))
         :source-activity-key
         (e-board-runtime--event-activity-source-key attachment event 'turn-summary))))))

(defun e-board-runtime--handle-harness-event (attachment event)
  "Publish attached output and reconcile board-delivery receipts from EVENT."
  (when (e-board-runtime--current-attachment-p attachment)
    (let ((type (e-events-type event)))
    (e-board-runtime--observe-turn-activity attachment event)
    (e-board-runtime--publish-harness-activity attachment event)
    (cond
     ((eq type 'turn-finished)
     (e-board-runtime--publish-output attachment (plist-get event :turn-id))
      (e-board-runtime--publish-turn-summary attachment event 'finished)
      (e-board-runtime--enqueue-ready-participant-pickup attachment))
     ((memq type '(turn-failed turn-cancelled))
      (e-board-runtime--publish-terminal-activity attachment event)
      (e-board-runtime--publish-turn-summary attachment event type))
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
            (e-board-runtime--enqueue-pickups registry-board (list next-id))))))))))

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
        :board-participant-id (e-board-pickup-participant-id pickup)
        :board-message-id (e-board-pickup-message-id pickup)
        :board-subscription-ids
        (copy-sequence (e-board-pickup-subscription-ids pickup))
        :board-event-seq-range
        (copy-sequence (e-board-pickup-event-seq-range pickup))
        :board-input-mode (e-board-pickup-mode pickup)
        :board-reference (copy-tree (e-board-pickup-reference pickup))
        :board-requester-actor
        (copy-tree (e-board-pickup-requester-actor pickup))
        :board-cause-metadata
        (copy-tree (e-board-pickup-cause-metadata pickup))))

(defun e-board-runtime--deliver-to-harness (attachment pickup _message)
  "Deliver PICKUP's MESSAGE through ATTACHMENT's idle harness session.
Queue-mode messages enter the harness follow-up queue.  Inject-mode messages
start an asynchronous prompt only while no active turn owns the session; a
busy session leaves its pickup pending for an explicit later retry."
  (let* ((harness (e-board-runtime-attachment-harness attachment))
         (session-id (e-board-runtime-attachment-session-id attachment))
         (prompt (e-board-pickup-content pickup))
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
                 &key participant-id author principal controller delivery-function)
  "Attach existing live HARNESS SESSION-ID to BOARD-OR-ID as one participant.

DELIVERY-FUNCTION is called as (FUNCTION ATTACHMENT PICKUP MESSAGE) for each
frozen pending pickup addressed to the participant.  It must perform one
delivery or signal; normal return marks that pickup delivered.  Returning
=(:accepted RECEIPT)= waits for a later consumption receipt; returning
=(:uncertain REASON)= records an ambiguous original-endpoint attempt without
retrying it; returning =(:discarded REASON)= records a proven non-commit;
returning =(:failed REASON)= records a permanent endpoint rejection.
When omitted, the conservative idle-only harness delivery port is used."
  (e-board-runtime--attach-resolved
   board-or-id harness session-id
   :participant-id participant-id :author author :principal principal
   :controller controller
   :delivery-function delivery-function))

(cl-defun e-board-runtime--attach-resolved
    (board-or-id harness session-id
                 &key participant-id author principal controller delivery-function
                 instance-id instance-catalog-generation harness-id
                 harness-object-generation session-store-id endpoint-token)
  "Attach one already-resolved endpoint with optional qualified metadata."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (or (null delivery-function) (functionp delivery-function))
    (signal 'wrong-type-argument (list 'functionp delivery-function)))
  (e-board-runtime--require-live-session harness session-id)
  (let ((session-key (e-board-runtime--resolved-session-attachment-key
                      harness session-id session-store-id))
        (endpoint-key (e-board-runtime--session-key harness session-id)))
    (when (or (gethash session-key e-board-runtime--session-attachments)
              (gethash endpoint-key e-board-runtime--endpoint-attachments))
      (signal 'e-board-runtime-session-busy (list session-key))))
  (let* ((board (e-board-runtime--active-board board-or-id))
         (participant (e-board-registry-add-participant
                       board :id participant-id :author author :principal principal
                       :controller controller))
         (key (e-board-runtime--attachment-key board participant)))
    (when (gethash key e-board-runtime--attachments)
      (signal 'e-board-runtime-attachment-exists (list key)))
    (e-board-runtime--activate-attachment
     (e-board-runtime--make-attachment
      board participant harness session-id delivery-function 1
      :instance-id instance-id
      :instance-catalog-generation instance-catalog-generation
      :harness-id harness-id
      :harness-object-generation harness-object-generation
      :session-store-id session-store-id
      :endpoint-token endpoint-token))))

(cl-defun e-board-runtime-attach-instance
    (board-or-id instance-id session-id
                 &key participant-id author principal controller delivery-function)
  "Attach an existing live SESSION-ID through configured INSTANCE-ID.
This operation never invokes an instance factory or loads dormant history."
  (let* ((instance-generation (e-harness-instance-generation))
         (instance (or (e-harness-instance-get instance-id)
                       (signal 'e-harness-instance-missing (list instance-id))))
         (store-id (e-harness-instance-session-store-id instance))
         (harness-id (e-harness-instance-harness-id instance))
         (harness (e-harness-registry-get harness-id))
         (harness-generation (e-harness-registry-generation harness-id)))
    (unless store-id
      (signal 'e-board-runtime-instance-ineligible (list instance-id)))
    (unless harness
      (signal 'e-harness-registry-missing (list harness-id)))
    (unless (= instance-generation (e-harness-instance-generation))
      (signal 'e-board-runtime-error (list instance-id 'stale-instance-catalog)))
    (let ((token (e-board-runtime-endpoint-token--create
                  :harness-id harness-id
                  :harness-object-generation harness-generation
                  :session-store-id store-id
                  :session-id session-id)))
      (e-board-runtime--attach-resolved
       board-or-id harness session-id
       :participant-id participant-id :author author :principal principal
       :controller controller
       :delivery-function delivery-function
       :instance-id instance-id
       :instance-catalog-generation instance-generation
       :harness-id harness-id
       :harness-object-generation harness-generation
       :session-store-id store-id
       :endpoint-token token))))

(defun e-board-runtime--make-attachment
    (board participant harness session-id delivery-function generation
           &rest metadata)
  "Construct one immutable-generation attachment without registering it."
  (e-board-runtime-attachment--create
   :board board :participant participant :harness harness :session-id session-id
   :activity-sequence 0 :generation generation
   :turn-activity (make-hash-table :test 'equal)
   :delivery-function (or delivery-function #'e-board-runtime--deliver-to-harness)
   :instance-id (plist-get metadata :instance-id)
   :instance-catalog-generation
   (plist-get metadata :instance-catalog-generation)
   :harness-id (plist-get metadata :harness-id)
   :harness-object-generation
   (plist-get metadata :harness-object-generation)
   :session-store-id (plist-get metadata :session-store-id)
   :endpoint-token (plist-get metadata :endpoint-token)))

(defun e-board-runtime--configure-attachment (attachment)
  "Install the private board/harness ports required by ATTACHMENT."
  (let* ((board (e-board-runtime-attachment-board attachment))
         (source-board (e-board-registry-board-source-board board))
         (harness (e-board-runtime-attachment-harness attachment)))
    (setf (e-board-effect-scheduler source-board)
          (lambda (effect) (run-at-time 0 nil (lambda () (funcall effect))))
          (e-board-invocation-effect-dispatcher source-board)
          #'e-board-runtime--apply-invocation-effect
          (e-board-input-classification-scheduler source-board)
          (lambda (drain)
            (run-at-time 0 nil
                         (lambda ()
                           (e-board-runtime--drain-input-routing board drain)))))
    (e-harness-set-work-enrollment-function
     harness (lambda (handle &optional callback)
               (e-board-runtime--enroll-work harness handle callback)))
    (e-harness-set-board-aggregation-function
     harness (lambda (handles mode timeout callback invocation-context)
               (e-board-runtime--subscribe-aggregation
                harness handles mode timeout callback invocation-context)))))

(defun e-board-runtime--activate-attachment (attachment)
  "Subscribe and register ATTACHMENT after its ownership checks pass."
  (let* ((board (e-board-runtime-attachment-board attachment))
         (participant (e-board-runtime-attachment-participant attachment))
         (harness (e-board-runtime-attachment-harness attachment))
         (session-id (e-board-runtime-attachment-session-id attachment))
         (key (e-board-runtime--attachment-key board participant))
         (session-key (e-board-runtime--attachment-session-key attachment))
         (endpoint-key (e-board-runtime--session-key harness session-id)))
    (when (or (gethash session-key e-board-runtime--session-attachments)
              (gethash endpoint-key e-board-runtime--endpoint-attachments))
      (signal 'e-board-runtime-session-busy (list session-key endpoint-key)))
    (setf (e-board-runtime-attachment-subscription attachment)
          (e-harness-subscribe
           harness (lambda (event) (e-board-runtime--handle-harness-event attachment event))
           :session-id session-id))
    (puthash key attachment e-board-runtime--attachments)
    (puthash session-key attachment e-board-runtime--session-attachments)
    (puthash endpoint-key attachment e-board-runtime--endpoint-attachments)
    (e-board-runtime--configure-attachment attachment)
    attachment))

(cl-defun e-board-runtime-rebind
    (board-or-id participant-or-id harness session-id &key delivery-function)
  "Move an existing participant to live HARNESS SESSION-ID.
The logical participant and its board pickups survive.  The old attachment is
generation-fenced and unsubscribed, so its late events cannot resolve against
the new endpoint."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (or (null delivery-function) (functionp delivery-function))
    (signal 'wrong-type-argument (list 'functionp delivery-function)))
  (e-board-runtime--require-live-session harness session-id)
  (let* ((board (e-board-runtime--active-board board-or-id))
         (participant (e-board-registry-participant board participant-or-id))
         (key (e-board-runtime--attachment-key board participant))
         (old (gethash key e-board-runtime--attachments)))
    (unless old
      (signal 'e-board-runtime-error (list "Participant is not attached" participant-or-id)))
    (when (or (gethash (e-board-runtime--session-key harness session-id)
                       e-board-runtime--session-attachments)
              (gethash (e-board-runtime--session-key harness session-id)
                       e-board-runtime--endpoint-attachments))
      (signal 'e-board-runtime-session-busy (list harness session-id)))
    (let* ((source-board (e-board-registry-board-source-board board))
           (delivery-id (car (e-board--pickup-queue
                              source-board
                              (e-board-registry-participant-id participant))))
           (pickup (and delivery-id (e-board-pickup source-board delivery-id))))
      ;; The old endpoint owns the only evidence for an in-flight or accepted
      ;; mutation.  Once it is fenced, that uncertainty is terminal rather
      ;; than permission to submit the same logical pickup to a new session.
      (when (and pickup
                 (memq (e-board-pickup-state pickup)
                       '(delivering accepted cancelling)))
        (e-board-pickup-mark-uncertain source-board delivery-id 'endpoint-rebound)))
    (e-harness-unsubscribe (e-board-runtime-attachment-harness old)
                           (e-board-runtime-attachment-subscription old))
    (remhash (e-board-runtime--attachment-session-key old)
             e-board-runtime--session-attachments)
    (remhash (e-board-runtime--session-key
              (e-board-runtime-attachment-harness old)
              (e-board-runtime-attachment-session-id old))
             e-board-runtime--endpoint-attachments)
    (cl-incf (e-board-runtime-attachment-generation old))
    (let ((attachment
           (e-board-runtime--make-attachment
            board participant harness session-id delivery-function
            (e-board-runtime-attachment-generation old))))
      (e-board-runtime--activate-attachment attachment)
      (e-board--append-event
       (e-board-registry-board-source-board board) 'participant-rebound
       (list :participant-id (e-board-registry-participant-id participant)
             :attachment-generation (e-board-runtime-attachment-generation attachment)))
      (e-board-runtime--enqueue-ready-participant-pickup attachment)
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
                  ((e-board-runtime--current-attachment-p attachment))
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
           (unless (eq (car err) 'e-board-runtime-session-busy)
             (signal (car err) (cdr err)))))))))

(cl-defun e-board-runtime-post-input
    (board-or-id &key id author tags attributes to requester
                 (mode 'inject) content reference source-input-key)
  "Post one input to BOARD-OR-ID's source board and enqueue its frozen pickups.
The returned value is the source board's `e-board-publication'.  Duplicate
publications only retry pickups that remain pending.  REQUESTER, when supplied,
must be an active registry client requester context; exact posts additionally
require authority for their target participant."
  (let* ((board (e-board-runtime--active-board board-or-id))
         (requester-principal
          (and requester
               (e-board-registry-resolve-requester-principal board requester)))
         (_authorization
          (and requester to
               (e-board-registry-authorize-exact-post
                board requester-principal to)))
         (publication
          (e-board-post-input
           (e-board-registry-board-source-board board)
            :id id :author author :requester-actor requester-principal
           :tags tags :attributes attributes :to to :mode mode :content content
           :reference reference :source-input-key source-input-key)))
    (e-board-runtime--enqueue-pickups
     board (e-board-publication-pickup-ids publication))
    publication))

(provide 'e-board-runtime)

;;; e-board-runtime.el ends here
