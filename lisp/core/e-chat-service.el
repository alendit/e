;;; e-chat-service.el --- Board-backed chat application service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shell-neutral board client for chat presentation shells.  The harness owns
;; the private transcript and turn endpoint; callers submit and observe only
;; through the board binding created here.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-board-registry)
(require 'e-board-runtime)
(require 'e-board-orchestration)
(require 'e-chat-output-mode)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-session)

(defvar e-chat-default-harness-id)

(defgroup e-chat-service nil
  "Shell-neutral chat service operations."
  :group 'e)

(defcustom e-chat-service-default-harness-id :chat-default
  "Harness registry id used by shell-neutral default chat commands."
  :type 'symbol
  :group 'e-chat-service)

(defconst e-chat-service-observer-page-limit 16
  "Maximum board messages translated during one chat observer drain.")

(defconst e-chat-service-subscriber-limit 8
  "Maximum presentation subscribers admitted to one chat binding.")

(defconst e-chat-service-projection-capacity 256
  "Maximum immutable board events retained per chat projection category.")

(defcustom e-chat-service-idle-close-delay 300
  "Seconds without a presentation client before an idle board closes."
  :type 'number
  :group 'e-chat-service)

(cl-defstruct (e-chat-service-binding
               (:constructor e-chat-service--binding-create))
  harness session-id board client requester attachment observer subscribers
  observer-drain-scheduled pending-input-head pending-input-tail turn-map
  input-sequence default-tags default-to idle-close-timer
  message-projection activity-projection)

(cl-defstruct (e-chat-service-projection
               (:constructor e-chat-service--projection-create))
  ring head count seen)

(cl-defstruct (e-chat-service-subscription
               (:constructor e-chat-service--subscription-create))
  binding function active-p client observer drain-scheduled state)

(cl-defstruct (e-chat-service-view
               (:constructor e-chat-service--view-create))
  cursor messages activity-events subscription)

(defvar e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key)
  "Board-backed chat bindings, first by harness identity then session id.")

(defvar e-chat-service--board-bindings (make-hash-table :test 'equal)
  "Live chat bindings sharing each registered board identity.")

(defvar e-chat-service--board-log-owners (make-hash-table :test 'equal)
  "Stable root binding that persists each live board's durable message log.")

(defvar e-chat-service--continuation-reconciling (make-hash-table :test 'equal)
  "Boards whose terminal continuation is being reconciled synchronously.")

(defun e-chat-service--publish-continuation-claim
    (board run-id publication-key status &optional error)
  "Publish RUN-ID's continuation STATUS with its stable PUBLICATION-KEY."
  (e-board-orchestration-publish-fact
   board
   (list :version e-board-orchestration-fact-version
         :type 'continuation-claim
         :idempotency-key
         (e-board-orchestration-continuation-claim-key publication-key status)
         :payload (append (list :run-id run-id :publication-key publication-key
                                :status status)
                          (when error (list :error error))))))

(defun e-chat-service--reconcile-board-continuation (binding)
  "Publish each terminal continuation on BINDING's board exactly once.
The input key lives in the durable manifest and is reused after a crash between
input publication and the acknowledgement fact."
  (let* ((board (e-board-registry-board-source-board
                 (e-chat-service-binding-board binding)))
         (board-id (e-board-id board)))
    (unless (gethash board-id e-chat-service--continuation-reconciling)
      (puthash board-id t e-chat-service--continuation-reconciling)
      (unwind-protect
          (dolist (run-id (e-board-orchestration-run-ids board))
            (condition-case err
                (let* ((projection (e-board-orchestration-run-projection board run-id))
                       (continuation (plist-get projection :continuation)))
                  (when (and (plist-get projection :terminal-status)
                             continuation
                             (not (eq (plist-get continuation :state) 'published)))
                    (let ((key (plist-get continuation :publication-key)))
                      (e-chat-service--publish-continuation-claim
                       board run-id key 'pending)
                      (e-chat-service-queue-session
                       (e-chat-service-binding-harness binding)
                       (plist-get continuation :session-id)
                       (plist-get continuation :prompt)
                       :metadata (list :board-run-id run-id
                                       :board-continuation-key key)
                       :source-input-key (list "orchestration-continuation" key 0))
                      (e-chat-service--publish-continuation-claim
                       board run-id key 'published))))
              (e-board-orchestration-invalid-fact nil)
              (error
               (when-let* ((projection (ignore-errors
                                         (e-board-orchestration-run-projection board run-id)))
                           (continuation (plist-get projection :continuation)))
                 (e-chat-service--publish-continuation-claim
                  board run-id (plist-get continuation :publication-key) 'failed err)))))
        (remhash board-id e-chat-service--continuation-reconciling)))))

(defun e-chat-service--board-envelope (message)
  "Return MESSAGE's frozen durable board envelope."
  (list :id (e-board-message-id message)
        :kind (e-board-message-kind message)
        :author (e-board-message-author message)
        :requester-actor (e-board-message-requester-actor message)
        :tags (copy-tree (e-board-message-tags message))
        :attributes (copy-tree (e-board-message-attributes message))
        :to (e-board-message-to message) :mode (e-board-message-mode message)
        :content (e-board-message-content message)
        :reference (copy-tree (e-board-message-reference message))
        :source-input-key (copy-tree (e-board-message-source-input-key message))
        :source-output-key (copy-tree (e-board-message-source-output-key message))
        :reply-to-message-ids
        (copy-tree (e-board-message-reply-to-message-ids message))
        :caused-by-delivery-ids
        (copy-tree (e-board-message-caused-by-delivery-ids message))
        :source-activity-key
        (copy-tree (e-board-message-source-activity-key message))
        :source-fact-key (copy-tree (e-board-message-source-fact-key message))
        :subject-participant-id (e-board-message-subject-participant-id message)
        :source-turn-id (e-board-message-source-turn-id message)
        :activity-kind (e-board-message-activity-kind message)
        :created-at (e-board-message-created-at message)
        :matching-participant-ids
        (copy-tree (e-board-message-matching-participant-ids message))
        :unrouted-reason (e-board-message-unrouted-reason message)
        :routing-state (e-board-message-routing-state message)))

(defun e-chat-service--persist-board-message (binding message)
  "Append MESSAGE once to BINDING's durable board log."
  (e-session-append-board-message
   (e-harness-sessions (e-chat-service-binding-harness binding))
   (e-chat-service-binding-session-id binding)
   (e-chat-service--board-envelope message)))

(defun e-chat-service--persist-board-processing-record (binding record _type)
  "Append immutable processing RECORD once to BINDING's durable board log."
  (e-session-append-board-message
   (e-harness-sessions (e-chat-service-binding-harness binding))
   (e-chat-service-binding-session-id binding)
   (e-board-processing-record-envelope record)))

(defun e-chat-service--harness-bindings (harness)
  "Return the session binding table owned by HARNESS."
  (or (gethash harness e-chat-service--bindings)
      (puthash harness (make-hash-table :test 'equal)
               e-chat-service--bindings)))

(defun e-chat-service--binding-live-p (binding)
  "Return non-nil when BINDING still names its active registered board."
  (let ((board (and (e-chat-service-binding-p binding)
                    (e-chat-service-binding-board binding))))
    (and board
         (eq (e-board-registry-board-state board) 'active)
         (condition-case nil
             (eq board
                 (e-board-registry-get
                  (e-board-registry-board-id board)))
           (e-board-registry-missing nil)))))

(defun e-chat-service--retire-binding (binding)
  "Retire BINDING's process-local presentation subscriptions."
  (dolist (subscription (e-chat-service-binding-subscribers binding))
    (setf (e-chat-service-subscription-active-p subscription) nil))
  (setf (e-chat-service-binding-subscribers binding) nil)
  (setf (e-chat-service-binding-observer-drain-scheduled binding) nil)
  (when-let ((timer (e-chat-service-binding-idle-close-timer binding)))
    (when (timerp timer) (cancel-timer timer))
    (setf (e-chat-service-binding-idle-close-timer binding) nil))
  (let* ((board (e-chat-service-binding-board binding))
         (board-id (e-board-registry-board-id board)))
    (puthash board-id
             (delq binding (gethash board-id e-chat-service--board-bindings))
             e-chat-service--board-bindings)
    (ignore-errors
      (e-board-registry-detach-client
       board
         (e-board-registry-client-id
        (e-chat-service-binding-client binding))))))

(defun e-chat-service--discard-binding (binding)
  "Discard an unpublished BINDING and all of its owned runtime state.
This is the application-service admission inverse, not ordinary participant
removal: it emits no board removal event and removes the binding from every
  process-local catalog before releasing its client and attachment."
  (when (e-chat-service-binding-p binding)
    (let* ((harness (e-chat-service-binding-harness binding))
           (session-id (e-chat-service-binding-session-id binding))
           (board (e-chat-service-binding-board binding))
           (board-id (e-board-registry-board-id board))
           (bindings (e-chat-service--harness-bindings harness))
           (attachment (e-chat-service-binding-attachment binding))
           (client (e-chat-service-binding-client binding)))
      (dolist (subscription (e-chat-service-binding-subscribers binding))
        (setf (e-chat-service-subscription-active-p subscription) nil))
      (setf (e-chat-service-binding-subscribers binding) nil)
      (when (eq (gethash session-id bindings) binding)
        (remhash session-id bindings))
      (puthash board-id
               (delq binding (gethash board-id e-chat-service--board-bindings))
               e-chat-service--board-bindings)
      (when (eq (gethash board-id e-chat-service--board-log-owners) binding)
        (remhash board-id e-chat-service--board-log-owners))
      (when attachment
        (ignore-errors (e-board-runtime-abort-new-attachment attachment)))
      (when client
        (ignore-errors
          (e-board-registry-detach-client
           board (e-board-registry-client-id client))))
      t)))

(defun e-chat-service-binding (harness session-id)
  "Return HARNESS SESSION-ID's live chat board binding, or nil."
  (when-let ((bindings (gethash harness e-chat-service--bindings)))
    (when-let ((binding (gethash session-id bindings)))
      (if (e-chat-service--binding-live-p binding)
          binding
        (e-chat-service--retire-binding binding)
        (remhash session-id bindings)
        nil))))

(defun e-chat-service-board-session-p (harness session-id)
  "Return non-nil when HARNESS SESSION-ID has durable board identity."
  (condition-case nil
      (let* ((session (e-session-get (e-harness-sessions harness) session-id))
             (state (plist-get session :board-session-state)))
        (and (stringp (plist-get state :board-id))
             (plist-get state :principal)))
    (e-session-missing nil)))

(defun e-chat-service--board-has-active-subscriber-p (board-id)
  "Return non-nil when BOARD-ID has any live presentation subscriber."
  (cl-some
   (lambda (binding)
     (cl-some #'e-chat-service-subscription-active-p
              (e-chat-service-binding-subscribers binding)))
   (gethash board-id e-chat-service--board-bindings)))

(defun e-chat-service--cancel-idle-close (binding)
  "Cancel BINDING's pending idle close, if any."
  (when-let ((timer (e-chat-service-binding-idle-close-timer binding)))
    (when (timerp timer) (cancel-timer timer))
    (setf (e-chat-service-binding-idle-close-timer binding) nil)))

(defun e-chat-service-close-board (binding)
  "Retire all process-local clients and begin async close of BINDING's board."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (let* ((board (e-chat-service-binding-board binding))
         (board-id (e-board-registry-board-id board)))
    (dolist (current (copy-sequence
                      (gethash board-id e-chat-service--board-bindings)))
      (e-chat-service--retire-binding current))
    (remhash board-id e-chat-service--board-bindings)
    (remhash board-id e-chat-service--board-log-owners)
    (e-board-registry-close board)))

(defun e-chat-service-reset-session (harness session-id)
  "Reset SESSION-ID's transcript and bounded board presentation projection."
  (let* ((store (e-harness-sessions harness))
         (binding (e-chat-service-ensure-binding harness session-id))
         (board-id (e-board-registry-board-id
                    (e-chat-service-binding-board binding))))
    (e-harness-reset harness session-id)
    (e-session-clear-board-messages store session-id)
    (dolist (current (gethash board-id e-chat-service--board-bindings))
      (dolist (projection
               (list (e-chat-service-binding-message-projection current)
                     (e-chat-service-binding-activity-projection current)))
        (fillarray (e-chat-service-projection-ring projection) nil)
        (clrhash (e-chat-service-projection-seen projection))
        (setf (e-chat-service-projection-head projection) 0
              (e-chat-service-projection-count projection) 0))
      (clrhash (e-chat-service-binding-turn-map current))
      (setf (e-chat-service-binding-pending-input-head current) nil
            (e-chat-service-binding-pending-input-tail current) nil))
    binding))

(defun e-chat-service--schedule-idle-close (binding)
  "Schedule registry-owned board cleanup after BINDING becomes idle."
  (e-chat-service--cancel-idle-close binding)
  (setf (e-chat-service-binding-idle-close-timer binding)
        (run-at-time
         (max 0 e-chat-service-idle-close-delay) nil
         (lambda ()
           (setf (e-chat-service-binding-idle-close-timer binding) nil)
           (let* ((board (e-chat-service-binding-board binding))
                  (board-id (e-board-registry-board-id board)))
             (when (and (eq (e-board-registry-board-state board) 'active)
                        (not (e-chat-service--board-has-active-subscriber-p
                              board-id)))
               (e-chat-service-close-board binding)))))))

(defun e-chat-service--enqueue-pending-input (binding message-id)
  "Append MESSAGE-ID to BINDING's uncorrelated input FIFO."
  (let ((cell (list message-id)))
    (if-let ((tail (e-chat-service-binding-pending-input-tail binding)))
        (setcdr tail cell)
      (setf (e-chat-service-binding-pending-input-head binding) cell))
    (setf (e-chat-service-binding-pending-input-tail binding) cell)))

(defun e-chat-service--turn-id (binding subject-participant-id source-turn-id)
  "Return BINDING's presentation id for one participant SOURCE-TURN-ID."
  (or (and source-turn-id
           (gethash (list subject-participant-id source-turn-id)
                    (e-chat-service-binding-turn-map binding)))
      (and source-turn-id
           (list (e-board-registry-board-id
                  (e-chat-service-binding-board binding))
                 subject-participant-id source-turn-id))))

(defun e-chat-service--causal-input-id (binding message)
  "Return the board input id that causally owns MESSAGE, if retained."
  (let ((source (e-board-registry-board-source-board
                 (e-chat-service-binding-board binding))))
    (or
     (when-let* ((delivery-id
                  (car (e-board-message-caused-by-delivery-ids message)))
                 (pickup (e-board-pickup source delivery-id)))
       (e-board-pickup-message-id pickup))
     (when-let* ((reply-id (car (e-board-message-reply-to-message-ids message)))
                 (reply (e-board-message source reply-id)))
       (and (eq (e-board-message-kind reply) 'input) reply-id)))))

(defun e-chat-service--binding-participant-id (binding)
  "Return the participant identity attached to BINDING, or nil.
The attachment is the process-local owner of a binding; board message subjects
are compared with it rather than inferred from tags, authors, or causal ids."
  (when-let* ((attachment (e-chat-service-binding-attachment binding))
              (participant (e-board-runtime-attachment-participant attachment)))
    (e-board-registry-participant-id participant)))

(defun e-chat-service--selected-participant-p (binding subject-participant-id)
  "Return non-nil when SUBJECT-PARTICIPANT-ID owns BINDING's attachment."
  (and subject-participant-id
       (equal subject-participant-id
              (e-chat-service--binding-participant-id binding))))

(defun e-chat-service--event-selected-participant-p (event)
  "Return whether EVENT is owned by the attached participant.

Synthetic events without board identity retain the historical direct-service
behavior.  A board-shaped event with a missing ownership fact is fail-closed,
so a sibling cannot settle a selected binding through a malformed projection."
  (if (plist-member event :selected-participant-p)
      (eq (plist-get event :selected-participant-p) t)
    (not (or (plist-member event :board-id)
             (plist-member event :board-seq)
             (plist-member event :message-id)
             (plist-member event :subject-participant-id)))))

(defun e-chat-service--message-event (binding message)
  "Translate one immutable board MESSAGE for BINDING's existing reducers."
  (let* ((kind (e-board-message-kind message))
         (source-turn-id (e-board-message-source-turn-id message))
         (subject-participant-id
          (e-board-message-subject-participant-id message))
         (causal-input-id (e-chat-service--causal-input-id binding message))
         (turn-id (or causal-input-id
                      (e-chat-service--turn-id
                       binding subject-participant-id source-turn-id)))
         (session-id (e-chat-service-binding-session-id binding))
         (identity
          (list :board-id (e-board-message-board-id message)
                :board-seq (e-board-message-seq message)
                :created-at (e-board-message-created-at message)
                :message-id (e-board-message-id message)
                :board-kind kind
                :tags (copy-tree (e-board-message-tags message))
                :attributes (copy-tree (e-board-message-attributes message))
                :to (e-board-message-to message)
                :mode (e-board-message-mode message)
                :reference (copy-tree (e-board-message-reference message))
                :subject-participant-id
                subject-participant-id
                :selected-participant-p
                ;; Ordinary user input has no participant subject, but it is
                ;; the selected conversation turn.  Terminal/output ownership
                ;; remains subject-based below.
                (or (eq kind 'input)
                    (e-chat-service--selected-participant-p
                     binding subject-participant-id))
                :source-turn-id source-turn-id
                :caused-by-delivery-ids
                (copy-tree (e-board-message-caused-by-delivery-ids message)))))
    (pcase kind
      ('input
       (let ((message-id (e-board-message-id message)))
         (append identity
                 (list :type 'message-added :session-id session-id
               :turn-id message-id
               :payload
               (list :message
                     (list :id message-id :role 'user
                           :content (e-board-message-content message)
                           :turn-id message-id
                           :created-at (e-board-message-created-at message)
                           :references (e-board-message-reference message)
                           :metadata
                           (copy-tree (e-board-message-attributes message))
                           :board-id (e-board-message-board-id message)
                           :board-seq (e-board-message-seq message)
                           :subject-participant-id
                           (e-board-message-subject-participant-id message)
                           :selected-participant-p
                           ;; An input has no participant subject.  It is the
                           ;; chat's own user turn for replay grouping; terminal
                           ;; ownership is still derived only from output and
                           ;; activity subjects below.
                           t))))))
      ('output
       (append identity
               (list :type 'message-added :session-id session-id
                     :turn-id turn-id
                     :payload
                   (list :message
                         (list :id (e-board-message-id message) :role 'assistant
                               :content (e-board-message-content message)
                               ;; Runtime output is published only while
                               ;; handling `turn-finished'.  Preserve that
                               ;; terminal witness across the shell boundary
                               ;; even when the separate turn-summary row
                               ;; arrives on a later board page.
                               :terminal-output t
                               :turn-id turn-id
                               :created-at (e-board-message-created-at message)
                               :metadata
                               (copy-tree (e-board-message-attributes message))
                               :board-id (e-board-message-board-id message)
                               :board-seq (e-board-message-seq message)
                               :subject-participant-id
                               (e-board-message-subject-participant-id message)
                               ;; Replay uses the nested message projection
                               ;; for observed-row identity.  Preserve the
                               ;; durable source turn there as well as on the
                               ;; outer board event so one sibling does not
                               ;; split into an output record plus activity
                               ;; records after reopen.
                               :source-turn-id
                               (e-board-message-source-turn-id message)
                               :selected-participant-p
                               (plist-get identity :selected-participant-p))))))
      ('activity
       (let ((activity-kind (e-board-message-activity-kind message)))
         (when (and source-turn-id causal-input-id)
           (puthash (list subject-participant-id source-turn-id)
                    causal-input-id
                    (e-chat-service-binding-turn-map binding)))
         (when (eq activity-kind 'turn-summary)
           (setq activity-kind
                 (pcase (plist-get (e-board-message-attributes message) :status)
                   ('finished 'turn-finished)
                   ((or 'turn-failed 'failed) 'turn-failed)
                   ((or 'turn-cancelled 'cancelled) 'turn-cancelled)
                   (_ 'turn-failed))))
         (when activity-kind
           (append identity
                   (list :type activity-kind :session-id session-id
                 :turn-id (e-chat-service--turn-id
                           binding subject-participant-id source-turn-id)
                 :payload
                 (append
                  (when-let ((content (e-board-message-content message)))
                    (list :content content))
                  (copy-tree (e-board-message-attributes message))))))))
      (_
       (append identity
               (list :type 'board-fact :session-id session-id
             :turn-id turn-id
             :payload (list :message-id (e-board-message-id message)
                            :content (e-board-message-content message))))))))

(defun e-chat-service--make-projection ()
  "Return one empty fixed-capacity presentation projection."
  (e-chat-service--projection-create
   :ring (make-vector e-chat-service-projection-capacity nil)
   :head 0 :count 0 :seen (make-hash-table :test 'equal)))

(defun e-chat-service--event-projection (binding event)
  "Return BINDING projection that owns EVENT, or nil for non-presentation facts."
  (pcase (plist-get event :type)
    ('message-added (e-chat-service-binding-message-projection binding))
    ('board-fact nil)
    (_ (e-chat-service-binding-activity-projection binding))))

(defun e-chat-service--projection-record (binding event)
  "Retain immutable board EVENT in BINDING's category-specific fixed ring."
  (when-let ((projection (and event
                              (e-chat-service--event-projection binding event))))
    (let* ((message-id (plist-get event :message-id))
           (seen (e-chat-service-projection-seen projection)))
      (unless (gethash message-id seen)
        (let* ((ring (e-chat-service-projection-ring projection))
               (head (e-chat-service-projection-head projection))
               (count (e-chat-service-projection-count projection))
               (index (mod (+ head count) e-chat-service-projection-capacity)))
          (when (= count e-chat-service-projection-capacity)
            (when-let ((evicted (aref ring head)))
              (remhash (plist-get evicted :message-id) seen))
            (setq head (mod (1+ head) e-chat-service-projection-capacity)
                  index (mod (+ head (1- count))
                             e-chat-service-projection-capacity)))
          (aset ring index (copy-tree event))
          (puthash message-id t seen)
          (setf (e-chat-service-projection-head projection) head
                (e-chat-service-projection-count projection)
                (min e-chat-service-projection-capacity (1+ count))))))))

(defun e-chat-service--projection-category-events (projection)
  "Return PROJECTION events in board sequence order."
  (let ((ring (e-chat-service-projection-ring projection))
        (head (e-chat-service-projection-head projection))
        (count (e-chat-service-projection-count projection)))
    (cl-loop for offset from 0 below count
             collect
             (copy-tree
              (aref ring (mod (+ head offset)
                              e-chat-service-projection-capacity))))))

(defun e-chat-service--projection-events (binding)
  "Return BINDING's independently bounded events in board sequence order."
  (sort
   (append
    (e-chat-service--projection-category-events
     (e-chat-service-binding-message-projection binding))
    (e-chat-service--projection-category-events
     (e-chat-service-binding-activity-projection binding)))
   (lambda (left right)
     (< (plist-get left :board-seq) (plist-get right :board-seq)))))

(defun e-chat-service--events-messages (events)
  "Return copied durable messages represented by board EVENTS."
  (let (messages)
    (dolist (event events (nreverse messages))
      (when (eq (plist-get event :type) 'message-added)
        (push (copy-tree (plist-get (plist-get event :payload) :message))
              messages)))))

(defun e-chat-service--events-activity-events (events)
  "Return copied shell activity records represented by board EVENTS."
  (let (activities)
    (dolist (event events (nreverse activities))
      (unless (memq (plist-get event :type) '(message-added board-fact))
        (push (append (list :event-type (plist-get event :type)
                            :created-at (plist-get event :created-at))
                      (copy-tree event))
              activities)))))

(defun e-chat-service--snapshot-events (binding before-seq)
  "Return independently bounded presentation events before BEFORE-SEQ."
  (let* ((board (e-chat-service-binding-board binding))
         (observer (e-chat-service-binding-observer binding))
         (source (e-board-registry-board-source-board board))
         events)
    (dolist (message
             (append
              (e-board-observer-recent-messages
               source (e-board-observer-id observer)
               :kinds '(input output)
               :limit e-chat-service-projection-capacity
               :before-seq before-seq)
              (e-board-observer-recent-messages
               source (e-board-observer-id observer)
               :kinds '(activity)
               :limit e-chat-service-projection-capacity
               :before-seq before-seq)))
      (when-let ((event (e-chat-service--message-event binding message)))
        (push event events)))
    (sort events
          (lambda (left right)
            (< (plist-get left :board-seq) (plist-get right :board-seq))))))

(defun e-chat-service--seed-binding-projection (binding)
  "Seed BINDING from independently bounded board message categories."
  (dolist (event
           (e-chat-service--snapshot-events
            binding
            (1+ (e-board-next-seq
                 (e-board-registry-board-source-board
                  (e-chat-service-binding-board binding))))))
    (e-chat-service--projection-record binding event)))

(defun e-chat-service--notify-subscribers (binding message)
  "Deliver MESSAGE from BINDING to each current shell subscriber."
  (when-let ((event (e-chat-service--message-event binding message)))
    (dolist (subscription (copy-sequence
                           (e-chat-service-binding-subscribers binding)))
      (when (e-chat-service-subscription-active-p subscription)
        (condition-case nil
            (funcall (e-chat-service-subscription-function subscription) event)
          (error nil))))))

(defun e-chat-service--schedule-observer-drain (binding)
  "Schedule one later bounded observer drain for BINDING."
  (when (and (e-chat-service--binding-live-p binding)
             (not (e-chat-service-binding-observer-drain-scheduled binding)))
    (setf (e-chat-service-binding-observer-drain-scheduled binding) t)
    (run-at-time 0 nil #'e-chat-service--drain-observer binding)))

(defun e-chat-service--schedule-subscription-drain (subscription)
  "Schedule one bounded independent observer drain for SUBSCRIPTION."
  (when (and (e-chat-service-subscription-active-p subscription)
             (not (e-chat-service-subscription-drain-scheduled subscription)))
    (setf (e-chat-service-subscription-drain-scheduled subscription) t)
    (run-at-time 0 nil #'e-chat-service--drain-subscription subscription)))

(defun e-chat-service--retire-subscription (subscription &optional state)
  "Release SUBSCRIPTION's presentation lease and record optional STATE."
  (setf (e-chat-service-subscription-active-p subscription) nil
        (e-chat-service-subscription-drain-scheduled subscription) nil)
  (when state
    (setf (e-chat-service-subscription-state subscription) state))
  (let ((binding (e-chat-service-subscription-binding subscription)))
    (setf (e-chat-service-binding-subscribers binding)
          (delq subscription
                (e-chat-service-binding-subscribers binding)))
    (when-let ((client (e-chat-service-subscription-client subscription)))
      (ignore-errors
        (e-board-registry-detach-client
         (e-chat-service-binding-board binding)
         (e-board-registry-client-id client))))
    (unless (cl-some #'e-chat-service-subscription-active-p
                     (e-chat-service-binding-subscribers binding))
      (e-chat-service--schedule-idle-close binding))))

(defun e-chat-service--drain-subscription (subscription)
  "Deliver and accept one independent observer page for SUBSCRIPTION."
  (setf (e-chat-service-subscription-drain-scheduled subscription) nil)
  (when (e-chat-service-subscription-active-p subscription)
    (condition-case err
        (let* ((binding (e-chat-service-subscription-binding subscription))
               (board (e-chat-service-binding-board binding))
               (client (e-chat-service-subscription-client subscription))
               (observer (e-chat-service-subscription-observer subscription))
               (page (e-board-registry-prepare-observer-page
                      board (e-board-registry-client-id client)
                      (e-board-observer-id observer)
                      :limit e-chat-service-observer-page-limit)))
          (condition-case callback-error
              (dolist (message (plist-get page :messages))
                (funcall (e-chat-service-subscription-function subscription)
                         (e-chat-service--message-event binding message)))
            (error
             (e-chat-service--retire-subscription
              subscription (list 'faulted callback-error))))
          (when (e-chat-service-subscription-active-p subscription)
            (when-let ((receipt (plist-get page :receipt)))
              (e-board-registry-accept-observer-page
               board (e-board-registry-client-id client)
               (e-board-observer-id observer) receipt))
            (when (< (or (plist-get page :through-index) 0)
                     (e-board-message-count
                      (e-board-registry-board-source-board board)))
              (e-chat-service--schedule-subscription-drain subscription))))
      (e-board-registry-client-missing
       (e-chat-service--retire-subscription
        subscription (list 'detached err))))))

(defun e-chat-service--drain-observer (binding)
  "Accept and translate one bounded live observer page for BINDING."
  (setf (e-chat-service-binding-observer-drain-scheduled binding) nil)
  (if (not (e-chat-service--binding-live-p binding))
      (e-chat-service--retire-binding binding)
    (let* ((board (e-chat-service-binding-board binding))
         (client (e-chat-service-binding-client binding))
         (observer (e-chat-service-binding-observer binding))
         (page (e-board-registry-prepare-observer-page
                board (e-board-registry-client-id client)
                (e-board-observer-id observer)
                :limit e-chat-service-observer-page-limit)))
    (dolist (message (plist-get page :messages))
      (e-chat-service--projection-record
       binding (e-chat-service--message-event binding message)))
    (when-let ((receipt (plist-get page :receipt)))
      (e-board-registry-accept-observer-page
       board (e-board-registry-client-id client)
       (e-board-observer-id observer) receipt)
      (when (< (or (plist-get page :through-index) 0)
               (e-board-message-count
                (e-board-registry-board-source-board board)))
        (e-chat-service--schedule-observer-drain binding))))))

(defconst e-chat-service--board-role-root "owner"
  "Durable chat board role for the user-facing owning session.")

(defconst e-chat-service--board-role-participant "participant"
  "Durable chat board role for a private execution session.")

(defun e-chat-service--routing-policy
    (participant-id pickup-selector observer-selector default-tags default-to)
  "Return one validated, detached routing policy for PARTICIPANT-ID.
`:self' is a caller-facing admission shorthand only; durable state contains the
resolved participant identity so restart never needs shell or caller policy."
  (unless (stringp participant-id)
    (signal 'e-session-error
            (list "Routing policy participant id must be a string"
                  participant-id)))
  (let* ((observer-selector
          (if (eq observer-selector :self)
              (list :subject-participant-id participant-id)
            observer-selector))
         (default-to (if (eq default-to :self) participant-id default-to))
         ;; Keep caller-owned values untouched until session's bounded
         ;; admission walk has completed.  The session copy is the one durable
         ;; detached representation used after this check.
         (policy (list :participant-id participant-id
                       :pickup-selector pickup-selector
                       :observer-selector observer-selector
                       :default-tags default-tags
                       :default-to default-to)))
    (unless (e-session-board-routing-policy-valid-p policy)
      (signal 'e-session-error (list "Invalid board routing policy" policy)))
    (e-session-aggregate-board-routing-copy-value policy)))

(defun e-chat-service--canonical-legacy-root-p (session association)
  "Return non-nil when SESSION has the established root identity defaults."
  (let ((role (plist-get association :association-role))
        (principal (plist-get association :principal))
        (session-id (plist-get session :id)))
    (or (equal role e-chat-service--board-role-root)
        (and (null role)
             (stringp session-id)
             (equal principal (format "chat:%s" session-id))))))

(defun e-chat-service--routing-overrides-present-p
    (participant-id participant-id-supplied-p
                   pickup-selector pickup-selector-supplied-p
                   observer-selector observer-selector-supplied-p
                   default-tags default-tags-supplied-p
                   default-to default-to-supplied-p)
  "Return non-nil when a caller supplied any routing override."
  (or participant-id-supplied-p pickup-selector-supplied-p
      observer-selector-supplied-p default-tags-supplied-p
      default-to-supplied-p
      ;; Callers outside CL keyword binding sometimes pass a non-nil value via
      ;; an adapter; preserve the explicit-value meaning at this boundary.
      participant-id pickup-selector observer-selector default-tags default-to))

(defun e-chat-service--complete-routing-arguments-p
    (participant-id participant-id-supplied-p
                    pickup-selector pickup-selector-supplied-p
                    observer-selector observer-selector-supplied-p
                    default-tags default-tags-supplied-p
                    default-to default-to-supplied-p)
  "Return non-nil when all five caller routing fields are explicitly present."
  (and participant-id-supplied-p pickup-selector-supplied-p
       observer-selector-supplied-p default-tags-supplied-p
       default-to-supplied-p
       ;; Route all complete-policy validation through the session-owned
       ;; bounded admission contract.  In particular, do not scan a hostile
       ;; selector/tag value here before that budget is charged.
       (condition-case nil
           (progn
             (e-chat-service--routing-policy
              participant-id pickup-selector observer-selector
              default-tags default-to)
             t)
         (e-session-error nil))))

(defun e-chat-service--routing-override-conflicts-p
    (policy participant-id participant-id-supplied-p
           pickup-selector pickup-selector-supplied-p
           observer-selector observer-selector-supplied-p
           default-tags default-tags-supplied-p default-to default-to-supplied-p)
  "Return non-nil when supplied routing values conflict with POLICY."
  (or (and participant-id-supplied-p
           (not (equal participant-id (plist-get policy :participant-id))))
      (and pickup-selector-supplied-p
           (not (equal pickup-selector (plist-get policy :pickup-selector))))
      (and observer-selector-supplied-p
           (let ((expected
                  (if (eq observer-selector :self)
                      (list :subject-participant-id
                            (plist-get policy :participant-id))
                    observer-selector)))
             (not (equal expected
                         (plist-get policy :observer-selector)))))
      (and default-tags-supplied-p
           (not (equal default-tags (plist-get policy :default-tags))))
      (and default-to-supplied-p
           (let ((expected
                  (if (eq default-to :self)
                      (plist-get policy :participant-id)
                    default-to)))
             (not (equal expected (plist-get policy :default-to)))))))

(defun e-chat-service--session-routing-policy (session &optional allow-missing)
  "Return SESSION's durable routing policy or signal for unsafe restoration."
  (let ((association (e-session-board-association session)))
    (when (e-session-board-association-invalid-p association)
      (signal 'e-session-error
              (list "Malformed board association" (plist-get session :id))))
    (cond
     ((e-session-board-association-policy-present-p association)
      (let ((policy (e-session-board-routing-policy session)))
        (unless (e-session-board-routing-policy-valid-p policy)
          (signal 'e-session-error
                  (list "Malformed board routing policy"
                        (plist-get session :id))))
        policy))
     ;; A complete caller-supplied policy may upgrade a legacy participant at
     ;; the explicit open boundary.  The ordinary restore path never gets
     ;; this exception; it must fail closed below.
     ((and allow-missing association)
      ;; The explicit open boundary may supply a complete replacement for any
      ;; legacy association whose policy is absent.  The caller still decides
      ;; below whether the supplied fields are complete; implicit restoration
      ;; never takes this branch.
      nil)
     ((e-chat-service--canonical-legacy-root-p session association)
      ;; Canonical roleless/owner records are the only legacy shapes with the
      ;; established main defaults.  Ambiguous roleless records do not get a
      ;; default participant policy by inference.
      nil)
     ((null association)
      ;; Let the caller report the established missing board-state condition.
      nil)
     (t
      (signal 'e-session-error
              (list "Legacy board participant has no complete routing policy"
                    (plist-get session :id)))))))

(cl-defun e-chat-service--install-participant-binding
    (board harness session-id &key principal participant-id
           (pickup-selector '(:tags (main)))
           (observer-selector '(:tags (main)))
           (default-tags '(main)) default-to
           defer-participant-publication)
  "Install one HARNESS SESSION-ID participant/client binding on BOARD."
  (or (e-chat-service-binding harness session-id)
      (progn
        (e-session-get (e-harness-sessions harness) session-id)
        (let (client requester attachment participant main-subscription
                     source-board snapshot-cursor observer binding)
          (condition-case error
              (progn
                (setq principal (or principal
                                    (e-board-registry-board-principal board)))
                (setq client
                      (e-board-registry-attach-client
                       board :principal principal :author "e-chat"))
                (setq requester
                      (e-board-registry-client-requester-context
                       board (e-board-registry-client-id client)))
                (setq attachment
                      (e-board-runtime-attach
                       board harness session-id :participant-id participant-id
                       :principal principal :controller principal
                       :author "e-chat"
                       :defer-participant-publication
                       defer-participant-publication))
                (setq participant
                      (e-board-runtime-attachment-participant attachment))
                (setq main-subscription
                      (e-board-registry-install-subscription
                       board participant pickup-selector))
                (setq source-board
                      (e-board-registry-board-source-board board))
                (setq snapshot-cursor (e-board-next-seq source-board))
                (setq observer
                      (e-board-registry-install-observer
                       board (e-board-registry-client-id client)
                       observer-selector :start-seq snapshot-cursor))
                (setq binding
                      (e-chat-service--binding-create
                       :harness harness :session-id session-id :board board
                       :client client :requester requester
                       :attachment attachment :observer observer
                       :subscribers nil :turn-map (make-hash-table :test 'equal)
                       :input-sequence 0 :default-tags (copy-tree default-tags)
                       :default-to default-to
                       :message-projection (e-chat-service--make-projection)
                       :activity-projection (e-chat-service--make-projection)))
                (puthash session-id binding
                         (e-chat-service--harness-bindings harness))
                (puthash (e-board-registry-board-id board)
                         (cons binding
                               (gethash (e-board-registry-board-id board)
                                        e-chat-service--board-bindings))
                         e-chat-service--board-bindings)
                (unless (gethash (e-board-registry-board-id board)
                                 e-chat-service--board-log-owners)
                  (puthash (e-board-registry-board-id board) binding
                           e-chat-service--board-log-owners))
                ;; Materialize only the recent bounded tail.  The observer's
                ;; live cursor already starts at the same high watermark, so
                ;; retained history is never rescanned to fill a fixed-capacity
                ;; projection.
                (e-chat-service--seed-binding-projection binding)
                (e-chat-service--reconcile-board-continuation binding)
                ;; These callbacks are installed only after all admission
                ;; steps above succeed, keeping attachment failure cleanup
                ;; independent of board notification publication.
                (setf (e-board-message-notification-function source-board)
                      (lambda (source message)
                        (when-let ((owner (gethash
                                           (e-board-id source)
                                           e-chat-service--board-log-owners)))
                          (e-chat-service--persist-board-message owner message))
                        (dolist (current (copy-sequence
                                          (gethash (e-board-id source)
                                                   e-chat-service--board-bindings)))
                          (dolist (subscription
                                   (copy-sequence
                                    (e-chat-service-binding-subscribers current)))
                            (e-chat-service--schedule-subscription-drain
                             subscription))
                          (e-chat-service--schedule-observer-drain current))
                        (when-let ((owner (gethash
                                           (e-board-id source)
                                           e-chat-service--board-log-owners)))
                          (e-chat-service--reconcile-board-continuation owner))))
                (setf (e-board-processing-record-notification-function source-board)
                      (lambda (source record type)
                        (when-let ((owner (gethash
                                           (e-board-id source)
                                           e-chat-service--board-log-owners)))
                          (e-chat-service--persist-board-processing-record
                           owner record type))))
                binding)
            (error
             ;; No binding is returned until all process-local maps are in a
             ;; coherent state.  If a later setup step fails, remove only the
             ;; objects allocated by this admission and leave the board event
             ;; stream untouched.
             (when binding
               (let ((bindings (e-chat-service--harness-bindings harness))
                     (board-id (e-board-registry-board-id board)))
                 (when (eq (gethash session-id bindings) binding)
                   (remhash session-id bindings))
                 (puthash board-id
                          (delq binding (gethash board-id
                                                 e-chat-service--board-bindings))
                          e-chat-service--board-bindings)
                 (when (eq (gethash board-id e-chat-service--board-log-owners)
                           binding)
                   (remhash board-id e-chat-service--board-log-owners))))
             (when main-subscription
               (setf (e-board-subscription-state main-subscription)
                     'cancelled)
               (remhash (e-board-subscription-id main-subscription)
                        (e-board-subscription-id-table source-board)))
             (when attachment
               (ignore-errors
                 (e-board-runtime-abort-new-attachment attachment)))
             (when client
               (ignore-errors
                 (e-board-registry-detach-client
                  board (e-board-registry-client-id client))))
             (signal (car error) (cdr error))))))))

(defun e-chat-service--bind-session (harness session-id)
  "Restore and bind board-native HARNESS SESSION-ID as its main member."
  (or (e-chat-service-binding harness session-id)
      (let* ((session (e-session-get (e-harness-sessions harness) session-id))
             (board-state (plist-get session :board-session-state))
             (routing-policy (e-chat-service--session-routing-policy session))
             (principal (plist-get board-state :principal))
             (board-id (plist-get board-state :board-id))
             (_ (unless (and (stringp board-id) principal)
                  (signal 'e-session-missing
                          (list session-id 'board-session-state))))
             (existing (and board-id
                            (condition-case nil
                                (e-board-registry-get board-id)
                              (e-board-registry-missing nil))))
             (board (or existing
                        (e-board-registry-create
                         :id board-id :principal principal))))
        (unless existing
          (e-board-orchestration-mark-restoring
           (e-board-registry-board-source-board board))
          (unwind-protect
              (dolist (envelope (e-session-board-messages
                                 (e-harness-sessions harness) session-id))
                (if (plist-get envelope :record-type)
                    (e-board-import-processing-record
                     (e-board-registry-board-source-board board) envelope)
                  (e-board-import-message
                   (e-board-registry-board-source-board board) envelope)))
            (e-board-orchestration-mark-restored
             (e-board-registry-board-source-board board))))
        (if routing-policy
            (e-chat-service--install-participant-binding
             board harness session-id :principal principal
             :participant-id (plist-get routing-policy :participant-id)
             :pickup-selector (plist-get routing-policy :pickup-selector)
             :observer-selector (plist-get routing-policy :observer-selector)
             :default-tags (plist-get routing-policy :default-tags)
             :default-to (plist-get routing-policy :default-to))
          (e-chat-service--install-participant-binding
           board harness session-id :principal principal)))))

(defun e-chat-service--persist-board-state
    (store session-id principal board-id role &optional routing-policy)
  "Persist board identity, chat ROLE, and ROUTING-POLICY through STORE."
  ;; Session composition owns the aggregate-to-storage transition.  Keeping
  ;; this call at the facade boundary prevents the chat service from reaching
  ;; into the storage controller's projection details.
  (e-session-declare-board-state
   store session-id principal board-id role routing-policy))

(cl-defun e-chat-service-create-board (&key harness metadata id)
  "Create a top-level board with one main participant and return its binding."
  (let* ((harness (or harness (e-chat-service-default-harness)))
         (session (e-harness-create-session harness :id id :metadata metadata))
         (session-id (plist-get session :id))
         (principal (format "chat:%s" session-id))
         (board (e-board-registry-create :principal principal))
         (store (e-harness-sessions harness))
         (participant-id (e-board-registry-allocate-participant-id board))
         (routing-policy
          (e-chat-service--routing-policy
           participant-id '(:tags (main)) '(:tags (main)) '(main) nil)))
    (e-chat-service--persist-board-state
     store session-id principal (e-board-registry-board-id board)
     e-chat-service--board-role-root routing-policy)
    (e-chat-service--install-participant-binding
     board harness session-id :principal principal
     :participant-id participant-id
     :pickup-selector (plist-get routing-policy :pickup-selector)
     :observer-selector (plist-get routing-policy :observer-selector)
     :default-tags (plist-get routing-policy :default-tags)
     :default-to (plist-get routing-policy :default-to))))

(cl-defun e-chat-service-open-board
    (board harness session-id
           &key (participant-id nil participant-id-supplied-p)
           (pickup-selector nil pickup-selector-supplied-p)
           (observer-selector nil observer-selector-supplied-p)
           (default-tags nil default-tags-supplied-p)
           (default-to nil default-to-supplied-p))
  "Open existing BOARD by attaching HARNESS SESSION-ID as one participant."
  (let* ((board (e-board-registry-get board))
         (session (e-session-get (e-harness-sessions harness) session-id))
         (state (plist-get session :board-session-state))
         (association (e-session-board-association session))
         (routing-policy
          (e-chat-service--session-routing-policy session t))
         (overrides-p
          (e-chat-service--routing-overrides-present-p
           participant-id participant-id-supplied-p
           pickup-selector pickup-selector-supplied-p
           observer-selector observer-selector-supplied-p
           default-tags default-tags-supplied-p
           default-to default-to-supplied-p))
         (complete-p
          (e-chat-service--complete-routing-arguments-p
           participant-id participant-id-supplied-p
           pickup-selector pickup-selector-supplied-p
           observer-selector observer-selector-supplied-p
           default-tags default-tags-supplied-p
           default-to default-to-supplied-p)))
    (unless (and (equal (plist-get state :board-id)
                        (e-board-registry-board-id board))
                 (equal (plist-get state :principal)
                        (e-board-registry-board-principal board)))
      (signal 'e-session-missing (list session-id 'board-session-state)))
    (cond
     (routing-policy
      (when (and overrides-p
                 (e-chat-service--routing-override-conflicts-p
                  routing-policy participant-id participant-id-supplied-p
                  pickup-selector pickup-selector-supplied-p
                  observer-selector observer-selector-supplied-p
                  default-tags default-tags-supplied-p
                  default-to default-to-supplied-p))
        (signal 'e-session-error
                (list "Routing policy override conflicts with durable state"
                      session-id))))
     (complete-p
     (setq routing-policy
            (e-chat-service--routing-policy
             participant-id pickup-selector observer-selector
             default-tags default-to))
      ;; A complete legacy upgrade is admitted against the real runtime before
      ;; changing durable association bytes.  In particular, an occupied board
      ;; participant id or already-attached session fails without wedging the
      ;; legacy record into an unusable policy.
      (e-board-runtime-admission-available-p
       board harness session-id participant-id
       :principal (plist-get state :principal))
      ;; Admission upgrades are durable before a runtime attachment can expose
      ;; the participant to board traffic.
      (e-chat-service--persist-board-state
       (e-harness-sessions harness) session-id
       (plist-get state :principal) (plist-get state :board-id)
       (plist-get state :association-role) routing-policy))
     ((e-chat-service--canonical-legacy-root-p session association)
      ;; Canonical roleless/owner legacy sessions retain the historical main
      ;; defaults.  No caller-supplied partial policy is silently borrowed.
      nil)
     (t
      (signal 'e-session-error
              (list "Legacy board participant has no complete routing policy"
                    session-id))))
    (if routing-policy
        (e-chat-service--install-participant-binding
         board harness session-id
         :participant-id (plist-get routing-policy :participant-id)
         :pickup-selector (plist-get routing-policy :pickup-selector)
         :observer-selector (plist-get routing-policy :observer-selector)
         :default-tags (plist-get routing-policy :default-tags)
         :default-to (plist-get routing-policy :default-to))
      (e-chat-service--install-participant-binding
       board harness session-id
       :participant-id participant-id
       :pickup-selector (or pickup-selector '(:tags (main)))
       :observer-selector (or observer-selector '(:tags (main)))
       :default-tags (or default-tags '(main)) :default-to default-to))))

(cl-defun e-chat-service-list-boards-page (&key after limit)
  "Return one bounded registry board page after AFTER.
LIMIT defaults to the registry's fixed page bound."
  (if limit
      (e-board-registry-list-page :after after :limit limit)
    (e-board-registry-list-page :after after)))

(cl-defun e-chat-service-create-participant
    (board harness &key metadata id participant-id pickup-selector
           observer-selector (default-tags '(main)) default-to)
  "Create and attach a private execution session as a participant on BOARD."
  ;; Resolve every caller-controlled admission input before creating the
  ;; session.  The session is not a valid root/participant until the complete
  ;; association is durable and the attachment succeeds.
  (let* ((board (e-board-registry-get board))
         (participant-id (or participant-id
                             (e-board-registry-allocate-participant-id board)))
         (principal (e-board-registry-board-principal board))
         (store (e-harness-sessions harness))
         (pickup-selector (or pickup-selector '(:tags (main))))
         (observer-selector (or observer-selector '(:tags (main))))
         (routing-policy
          (e-chat-service--routing-policy
           participant-id pickup-selector observer-selector
           default-tags default-to))
         (_ (when (gethash participant-id
                           (e-board-registry-board-participants board))
             (signal 'e-board-registry-id-conflict (list participant-id))))
         (session-id (or id (e-session-generate-id)))
         (_ (e-board-runtime-admission-available-p
             board harness session-id participant-id
             :principal principal :require-session nil))
         (_ (when id
             (condition-case nil
                 (progn (e-session-get store id)
                        (signal 'e-session-duplicate (list id)))
               (e-session-missing nil)))))
    (let ((session nil)
          (binding nil))
      (condition-case error
          (progn
            ;; Session id and runtime occupancy were preflighted above.  Keep
            ;; creation inside this owning failure boundary so a later service
            ;; error cannot strand the newly allocated root.
            (setq session
                  (e-session-create-board-admission
                   store :id session-id :metadata metadata
                   :principal principal
                   :board-id (e-board-registry-board-id board)
                   :association-role e-chat-service--board-role-participant
                   :routing-policy routing-policy))
            (setq binding
                  (e-chat-service--install-participant-binding
                   board harness session-id
                   :participant-id (plist-get routing-policy :participant-id)
                   :pickup-selector (plist-get routing-policy :pickup-selector)
                   :observer-selector (plist-get routing-policy :observer-selector)
                   :default-tags (plist-get routing-policy :default-tags)
                   :default-to (plist-get routing-policy :default-to)
                   :defer-participant-publication t))
              ;; The session owner publishes root + association only after the
              ;; registry/runtime attachment has completed successfully.
              (e-session-commit-board-admission store session-id)
              ;; The participant's source-board event is deliberately
              ;; published only after the session declaration has crossed its
              ;; durable admission boundary.  A failed commit therefore
              ;; cannot leave a replayable participant-added ghost.
              (e-board-registry-publish-participant-admission
               board
               (e-board-runtime-attachment-participant
                (e-chat-service-binding-attachment binding)))
              session)
        (error
         ;; Expected service-owned failures must not leave a false root in the
         ;; catalog.  Discard the unpublished runtime binding without emitting
         ;; a board removal event, then remove the session reservation.
         (when (e-chat-service-binding-p binding)
           (e-chat-service--discard-binding binding))
         (ignore-errors (e-session-abort-created store session-id))
         (signal (car error) (cdr error)))))))

(defun e-chat-service--harness-has-capability-p (harness capability-id)
  "Return non-nil when HARNESS has active capability CAPABILITY-ID."
  (memq capability-id
        (mapcar #'e-capability-id
                (e-harness-active-capabilities harness))))

(defun e-chat-service--harness-for-instance (instance)
  "Return the live chat harness for INSTANCE."
  (let* ((instance-id (e-harness-instance-id instance))
         (harness
          (condition-case err
              (e-harness-instance-get-or-create instance-id)
            ((e-harness-instance-missing e-harness-registry-missing)
             (user-error "No e harness registered for %S" (cadr err))))))
    (unless (e-chat-service--harness-has-capability-p harness 'chat-session)
      (user-error "Harness instance %S does not provide chat-session capability"
                  instance-id))
    harness))

(defun e-chat-service-default-harness-id ()
  "Return the effective default chat harness id."
  (if (boundp 'e-chat-default-harness-id)
      e-chat-default-harness-id
    e-chat-service-default-harness-id))

(defun e-chat-service-default-harness ()
  "Return the configured default chat harness."
  (let* ((harness-id (e-chat-service-default-harness-id))
         (instance (e-harness-instance-get harness-id))
         (harness
          (if instance
              (e-chat-service--harness-for-instance instance)
            (condition-case err
                (e-harness-registry-get-or-create harness-id)
              (e-harness-registry-missing
               (user-error "No e harness registered for %S" (cadr err)))))))
    (unless (e-chat-service--harness-has-capability-p harness 'chat-session)
      (user-error "Harness %S does not provide chat-session capability"
                  harness-id))
    harness))

(cl-defun e-chat-service-create-session (&key harness metadata id)
  "Create a new board's main participant and return its private session record."
  (let ((binding (e-chat-service-create-board
                  :harness harness :metadata metadata :id id)))
    (e-session-get
     (e-harness-sessions (e-chat-service-binding-harness binding))
     (e-chat-service-binding-session-id binding))))

(defun e-chat-service-ensure-binding (harness session-id)
  "Return HARNESS SESSION-ID's board binding, creating it when needed."
  (e-chat-service--bind-session harness session-id))

(cl-defun e-chat-service--subscribe
    (harness session-id function &key start-seq history-before-seq)
  "Create one board observer for FUNCTION at the requested cursors."
  (unless (functionp function)
    (signal 'wrong-type-argument (list 'functionp function)))
  (let* ((binding (e-chat-service--bind-session harness session-id))
         (_capacity
          (when (>= (length (e-chat-service-binding-subscribers binding))
                    e-chat-service-subscriber-limit)
            (user-error "Chat board subscriber limit reached for %s" session-id)))
         (board (e-chat-service-binding-board binding))
         (principal (e-board-registry-client-principal
                     (e-chat-service-binding-client binding)))
         (client (e-board-registry-attach-client
                  board :principal principal :author "e-chat-subscriber"))
         (observer (e-board-registry-install-observer
                    board (e-board-registry-client-id client)
                    (copy-tree
                     (e-board-observer-selector
                      (e-chat-service-binding-observer binding)))
                    :start-seq start-seq
                    :history-before-seq history-before-seq))
         (subscription (e-chat-service--subscription-create
                        :binding binding :function function :active-p t
                        :client client :observer observer :state 'active)))
    (e-chat-service--cancel-idle-close binding)
    (setf (e-chat-service-binding-subscribers binding)
          (cons subscription (e-chat-service-binding-subscribers binding)))
    (e-chat-service--schedule-subscription-drain subscription)
    subscription))

(defun e-chat-service-subscribe (harness session-id function)
  "Subscribe FUNCTION to future board events for HARNESS SESSION-ID.
Retained history is a snapshot concern and is never replayed implicitly."
  (e-chat-service--subscribe harness session-id function))

(defun e-chat-service-subscribe-view (harness session-id function)
  "Return a bounded snapshot plus live subscription for HARNESS SESSION-ID.
The snapshot and subscription share one board high-watermark cursor.  EVENTS
before that cursor appear only in the snapshot; later events appear only via
FUNCTION."
  (let* ((binding (e-chat-service--bind-session harness session-id))
         (board (e-chat-service-binding-board binding))
         (source (e-board-registry-board-source-board board))
         (cursor (e-board-next-seq source))
         (subscription
          (e-chat-service--subscribe
           harness session-id function
           :start-seq cursor))
         (events (e-chat-service--snapshot-events binding (1+ cursor))))
    (e-chat-service--view-create
     :cursor cursor
     :messages (e-chat-service--events-messages events)
     :activity-events (e-chat-service--events-activity-events events)
     :subscription subscription)))

(defun e-chat-service-unsubscribe (subscription)
  "Idempotently retire board-observer SUBSCRIPTION."
  (when (e-chat-service-subscription-p subscription)
    (e-chat-service--retire-subscription subscription))
  nil)

(cl-defun e-chat-service-replace-selector
    (subscription selector &key start-seq)
  "Replace SUBSCRIPTION's observer with SELECTOR and optional START-SEQ backfill."
  (unless (e-chat-service-subscription-p subscription)
    (signal 'wrong-type-argument
            (list 'e-chat-service-subscription-p subscription)))
  (let* ((binding (e-chat-service-subscription-binding subscription))
         (board (e-chat-service-binding-board binding))
         (client (e-chat-service-subscription-client subscription))
         (observer (e-chat-service-subscription-observer subscription))
         (replacement
          (e-board-registry-replace-observer
           board (e-board-registry-client-id client)
           (e-board-observer-id observer) selector :start-seq start-seq)))
    (setf (e-chat-service-subscription-observer subscription) replacement
          (e-chat-service-subscription-active-p subscription) t
          (e-chat-service-subscription-state subscription) 'active)
    (e-chat-service--schedule-subscription-drain subscription)
    replacement))

(cl-defun e-chat-service-post
    (binding prompt &key (mode 'inject) tags attributes to references metadata)
  "Post PROMPT through board-first BINDING with generic routing fields."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (e-chat-service--post
   (e-chat-service-binding-harness binding)
   (e-chat-service-binding-session-id binding)
   prompt mode :tags tags :attributes attributes :to to
   :references references :metadata metadata))

(cl-defun e-chat-service--post
    (harness session-id prompt mode &key references metadata tags attributes to source-input-key)
  "Post PROMPT to HARNESS SESSION-ID's board binding in MODE."
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (let* ((binding (e-chat-service--bind-session harness session-id))
         (client (e-chat-service-binding-client binding))
         (sequence (cl-incf (e-chat-service-binding-input-sequence binding)))
         (publication
          (e-board-runtime-post-input
           (e-chat-service-binding-board binding)
           :author (format "client:%s" (e-board-registry-client-id client))
           :requester (e-chat-service-binding-requester binding)
           :tags (or (copy-tree tags)
                     (copy-tree (e-chat-service-binding-default-tags binding)))
           :to (or to (e-chat-service-binding-default-to binding))
           :attributes (append (copy-tree attributes) (copy-tree metadata)
                               (and references
                                    (list :references (copy-tree references))))
           :mode mode :content prompt :reference (copy-tree references)
           :source-input-key
           (or source-input-key
               (list (e-board-registry-client-id client)
                     (e-board-registry-client-generation client)
                     sequence)))))
    (e-board-message-id (e-board-publication-message publication))))

(cl-defun e-chat-service-submit-session
    (harness session-id prompt &key references metadata)
  "Submit PROMPT through HARNESS SESSION-ID's board binding."
  (e-chat-service--post harness session-id prompt 'inject
                        :references references :metadata metadata))

(cl-defun e-chat-service-steer-session
    (harness session-id prompt &key metadata)
  "Post steering PROMPT through HARNESS SESSION-ID's board binding."
  (e-chat-service--post harness session-id prompt 'inject :metadata metadata))

(cl-defun e-chat-service-queue-session
    (harness session-id prompt &key references metadata source-input-key)
  "Queue PROMPT through HARNESS SESSION-ID's board binding.
SOURCE-INPUT-KEY lets durable callers retry one queued input exactly once."
  (e-chat-service--post harness session-id prompt 'queue
                        :references references :metadata metadata
                        :source-input-key source-input-key))

(defun e-chat-service-abort-session (harness session-id)
  "Abort the current board-bound turn for HARNESS SESSION-ID."
  (e-board-runtime-abort-attachment
   (e-chat-service-binding-attachment
    (e-chat-service--bind-session harness session-id))))

(defun e-chat-service--binding-active-turn (binding)
  "Return BINDING's running turn in the presentation id namespace.
Board activity events are correlated to their causal input message before they
reach presentation subscribers.  Apply the same translation to the private
harness turn here so state queries and events identify one turn consistently."
  (when-let* ((attachment (e-chat-service-binding-attachment binding))
              (active-turn
               (e-board-runtime-attachment-active-turn attachment))
              ((eq (plist-get active-turn :status) 'running)))
    (let* ((participant-id
            (e-board-registry-participant-id
             (e-board-runtime-attachment-participant attachment)))
           (source-turn-id (plist-get active-turn :id)))
      (plist-put active-turn :id
                 (e-chat-service--turn-id
                  binding participant-id source-turn-id))
      active-turn)))

(defun e-chat-service-active-turn (harness session-id)
  "Return SESSION-ID's running turn using presentation-facing identity.
The returned `:id' is in the same namespace as board-derived event `:turn-id'
values delivered by `e-chat-service-subscribe'."
  (e-chat-service--binding-active-turn
   (e-chat-service--bind-session harness session-id)))

(defun e-chat-service-active-turn-p (harness session-id)
  "Return non-nil when HARNESS SESSION-ID's board participant is running."
  (and (e-chat-service-active-turn harness session-id) t))

(defun e-chat-service-session (harness session-id)
  "Return SESSION-ID's private metadata through the board application seam."
  (e-session-get (e-harness-sessions harness) session-id))

(defun e-chat-service-session-options (harness session-id)
  "Return the effective option projection for SESSION-ID.
Chat presentation controls use this service operation instead of depending on
the harness context owner directly."
  (e-harness-session-options harness session-id))

(defun e-chat-service-session-store (harness)
  "Return HARNESS's private session store for controlled shell metadata work."
  (e-harness-sessions harness))

(defun e-chat-service-session-list (harness)
  "Return HARNESS's private session catalog for bounded shell navigation."
  (e-harness-session-list harness))

(defun e-chat-service--root-session-p (session)
  "Return non-nil when SESSION is a user-facing chat root.
An explicit durable chat role is authoritative.  Canonical legacy board state
without that role falls back to its historical `chat:<session-id>' owner
identity so existing indexes remain readable without mutation."
  (let* ((state (e-session-board-association session))
         (role-present (and state (plist-member state :association-role)))
         (role (and role-present (plist-get state :association-role)))
         (principal (plist-get state :principal))
         (session-id (plist-get session :id)))
    (cond
     ((null state) t)
     ((e-session-board-association-invalid-p state) nil)
     (role-present
      (equal role e-chat-service--board-role-root))
     (t
      (and (stringp session-id)
           (equal principal (format "chat:%s" session-id)))))))

(defun e-chat-service-root-session-list (harness)
  "Return HARNESS's user-facing chat roots for shell navigation."
  (cl-remove-if-not #'e-chat-service--root-session-p
                    (e-harness-root-session-list harness)))

(defun e-chat-service-messages (harness session-id)
  "Return SESSION-ID's bounded board-derived message projection."
  (e-chat-service--events-messages
   (e-chat-service--projection-events
    (e-chat-service--bind-session harness session-id))))

(defun e-chat-service-activity-events (harness session-id)
  "Return SESSION-ID's bounded board-derived activity projection."
  (e-chat-service--events-activity-events
   (e-chat-service--projection-events
    (e-chat-service--bind-session harness session-id))))

(defun e-chat-service-state (harness session-id)
  "Return SESSION-ID's bounded board-derived presentation state."
  (let* ((binding (e-chat-service--bind-session harness session-id))
         (activities (e-chat-service-activity-events harness session-id))
         (active-turn (e-chat-service--binding-active-turn binding)))
    (dolist (event activities)
      (pcase (plist-get event :event-type)
        ('turn-started
         (when (and (e-chat-service--event-selected-participant-p event)
                    (not active-turn))
           (setq active-turn (list :id (plist-get event :turn-id)
                                   :status 'running))))
        ((or 'turn-finished 'turn-failed 'turn-cancelled)
         (when (and (e-chat-service--event-selected-participant-p event)
                    (equal (plist-get active-turn :id)
                           (plist-get event :turn-id)))
           (setq active-turn nil)))))
    (list :board-id
          (e-board-registry-board-id (e-chat-service-binding-board binding))
          :message-count (length (e-chat-service-messages harness session-id))
          :active-turn active-turn)))

(defun e-chat-service-active-capabilities (harness)
  "Return HARNESS's active capabilities for shell affordance discovery."
  (e-harness-active-capabilities harness))

(defun e-chat-service-structured-blocks (harness session-id)
  "Return SESSION-ID's private structured-block registry projection."
  (e-harness-structured-blocks harness session-id))

(defun e-chat-service-message-presentation (harness session-id message)
  "Return generic display content and details for durable MESSAGE.
The service applies the active structured-block registry once, then gives the
parsed blocks to capability-owned detail providers.  Presentation shells never
need to know a block kind or capability policy."
  (let ((content (plist-get message :content)))
    (if (not (and (eq (plist-get message :role) 'assistant)
                  (stringp content)))
        (list :content content :details nil)
      (let* ((registry (e-harness-structured-blocks
                        harness session-id (plist-get message :turn-id)))
             (rendered (e-structured-blocks-render content registry)))
        (list :content (plist-get rendered :text)
              :details
              (e-harness-message-details
               harness session-id message
               :structured-blocks (plist-get rendered :blocks)))))))

(defun e-chat-service-session-name (harness session-id)
  "Return SESSION-ID's private configured name."
  (e-harness-session-name harness session-id))

(defun e-chat-service-session-title (harness session-id)
  "Return SESSION-ID's private display title."
  (e-harness-session-title harness session-id))

(defun e-chat-service-prompt-catalog (harness)
  "Return HARNESS's named prompt catalog for composer completion."
  (e-harness-prompts harness))

(defun e-chat-service-queued-inputs (harness session-id)
  "Return SESSION-ID's queued board inputs for bounded local presentation."
  (let* ((binding (e-chat-service--bind-session harness session-id))
         (source (e-board-registry-board-source-board
                  (e-chat-service-binding-board binding)))
         queued)
    (dolist (event (e-chat-service--projection-events binding)
                   (nreverse queued))
      (when (and (eq (plist-get event :board-kind) 'input)
                 (eq (plist-get event :mode) 'queue))
        (let* ((message-id (plist-get event :message-id))
               (message (e-board-message source message-id))
               (pending
                (cl-some
                 (lambda (pickup-id)
                   (memq (e-board-pickup-state (e-board-pickup source pickup-id))
                         '(pending ready delivering accepted cancelling)))
                 (and message (e-board-message-pickup-ids message)))))
          (when pending
            (push (list :prompt
                        (plist-get
                         (plist-get (plist-get event :payload) :message)
                         :content)
                        :references (plist-get event :reference)
                        :metadata (copy-tree (plist-get event :attributes)))
                  queued)))))))

(defun e-chat-service-active-turns (harness)
  "Return HARNESS's board-derived active-turn index for shell diagnostics."
  (let ((result (make-hash-table :test 'equal)))
    (when-let ((bindings (gethash harness e-chat-service--bindings)))
      (maphash
       (lambda (session-id binding)
         (when (e-chat-service--binding-live-p binding)
           (when-let ((active-turn
                       (e-chat-service--binding-active-turn binding)))
             (puthash session-id active-turn result))))
       bindings))
    result))

(defun e-chat-service-append-seed-message (harness session-id message)
  "Append explicit pre-turn seed MESSAGE to board-bound SESSION-ID."
  (e-chat-service--bind-session harness session-id)
  (e-session-append-message
   (e-harness-sessions harness) session-id (copy-sequence message)))

(defun e-chat-service-output-mode (harness session-id)
  "Return effective assistant output mode for HARNESS SESSION-ID."
  (e-chat-output-mode-resolve harness session-id))

(defun e-chat-service-set-output-mode (harness session-id mode)
  "Set assistant output MODE for HARNESS SESSION-ID."
  (e-chat-output-mode-session-set harness session-id mode))

(provide 'e-chat-service)

;;; e-chat-service.el ends here
