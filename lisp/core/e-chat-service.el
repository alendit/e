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

(cl-defstruct (e-chat-service-binding
               (:constructor e-chat-service--binding-create))
  harness session-id board client requester attachment observer subscribers
  observer-drain-scheduled pending-input-head pending-input-tail turn-map
  input-sequence default-tags default-to)

(cl-defstruct (e-chat-service-subscription
               (:constructor e-chat-service--subscription-create))
  binding function active-p client observer drain-scheduled state)

(defvar e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key)
  "Board-backed chat bindings, first by harness identity then session id.")

(defvar e-chat-service--board-bindings (make-hash-table :test 'equal)
  "Live chat bindings sharing each registered board identity.")

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

(defun e-chat-service-binding (harness session-id)
  "Return HARNESS SESSION-ID's live chat board binding, or nil."
  (when-let ((bindings (gethash harness e-chat-service--bindings)))
    (when-let ((binding (gethash session-id bindings)))
      (if (e-chat-service--binding-live-p binding)
          binding
        (e-chat-service--retire-binding binding)
        (remhash session-id bindings)
        nil))))

(defun e-chat-service--enqueue-pending-input (binding message-id)
  "Append MESSAGE-ID to BINDING's uncorrelated input FIFO."
  (let ((cell (list message-id)))
    (if-let ((tail (e-chat-service-binding-pending-input-tail binding)))
        (setcdr tail cell)
      (setf (e-chat-service-binding-pending-input-head binding) cell))
    (setf (e-chat-service-binding-pending-input-tail binding) cell)))

(defun e-chat-service--turn-id (binding source-turn-id)
  "Return BINDING's presentation id for SOURCE-TURN-ID."
  (or (and source-turn-id
           (gethash source-turn-id (e-chat-service-binding-turn-map binding)))
      source-turn-id))

(defun e-chat-service--causal-input-id (binding message)
  "Return the board input id that causally owns MESSAGE, if retained."
  (when-let* ((delivery-id (car (e-board-message-caused-by-delivery-ids message)))
              (pickup (e-board-pickup
                       (e-board-registry-board-source-board
                        (e-chat-service-binding-board binding))
                       delivery-id)))
    (e-board-pickup-message-id pickup)))

(defun e-chat-service--message-event (binding message)
  "Translate one immutable board MESSAGE for BINDING's existing reducers."
  (let* ((kind (e-board-message-kind message))
         (source-turn-id (e-board-message-source-turn-id message))
         (causal-input-id (e-chat-service--causal-input-id binding message))
         (turn-id (or causal-input-id
                      (e-chat-service--turn-id binding source-turn-id)))
         (session-id (e-chat-service-binding-session-id binding))
         (identity
          (list :board-id (e-board-message-board-id message)
                :board-seq (e-board-message-seq message)
                :message-id (e-board-message-id message)
                :subject-participant-id
                (e-board-message-subject-participant-id message)
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
                           :references (e-board-message-reference message)))))))
      ('output
       (append identity
               (list :type 'message-added :session-id session-id
             :turn-id turn-id
             :payload
             (list :message
                   (list :id (e-board-message-id message) :role 'assistant
                         :content (e-board-message-content message)
                         :turn-id turn-id)))))
      ('activity
       (let ((activity-kind (e-board-message-activity-kind message)))
         (when (and source-turn-id causal-input-id)
           (puthash source-turn-id causal-input-id
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
                 :turn-id (e-chat-service--turn-id binding source-turn-id)
                 :payload (copy-tree (e-board-message-attributes message)))))))
      (_
       (append identity
               (list :type 'board-fact :session-id session-id
             :turn-id turn-id
             :payload (list :message-id (e-board-message-id message)
                            :content (e-board-message-content message))))))))

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

(defun e-chat-service--drain-subscription (subscription)
  "Deliver and accept one independent observer page for SUBSCRIPTION."
  (setf (e-chat-service-subscription-drain-scheduled subscription) nil)
  (when (e-chat-service-subscription-active-p subscription)
    (let* ((binding (e-chat-service-subscription-binding subscription))
           (board (e-chat-service-binding-board binding))
           (client (e-chat-service-subscription-client subscription))
           (observer (e-chat-service-subscription-observer subscription))
           (page (e-board-registry-prepare-observer-page
                  board (e-board-registry-client-id client)
                  (e-board-observer-id observer)
                  :limit e-chat-service-observer-page-limit))
           (ok t))
      (condition-case err
          (dolist (message (plist-get page :messages))
            (funcall (e-chat-service-subscription-function subscription)
                     (e-chat-service--message-event binding message)))
        (error
         (setq ok nil)
         (setf (e-chat-service-subscription-active-p subscription) nil
               (e-chat-service-subscription-state subscription)
               (list 'faulted err))))
      (when ok
        (when-let ((receipt (plist-get page :receipt)))
          (e-board-registry-accept-observer-page
           board (e-board-registry-client-id client)
           (e-board-observer-id observer) receipt))
        (when (< (or (plist-get page :through-index) 0)
                 (e-board-message-count
                  (e-board-registry-board-source-board board)))
          (e-chat-service--schedule-subscription-drain subscription))))))

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
      (e-chat-service--notify-subscribers binding message))
    (when-let ((receipt (plist-get page :receipt)))
      (e-board-registry-accept-observer-page
       board (e-board-registry-client-id client)
       (e-board-observer-id observer) receipt)
      (when (< (or (plist-get page :through-index) 0)
               (e-board-message-count
                (e-board-registry-board-source-board board)))
        (e-chat-service--schedule-observer-drain binding))))))

(cl-defun e-chat-service--install-participant-binding
    (board harness session-id &key principal participant-id
           (pickup-selector '(:tags (main)))
           (observer-selector '(:tags (main)))
           (default-tags '(main)) default-to)
  "Install one HARNESS SESSION-ID participant/client binding on BOARD."
  (or (e-chat-service-binding harness session-id)
      (progn
        (e-session-get (e-harness-sessions harness) session-id)
        (let* ((principal (or principal (e-board-registry-board-principal board)))
               (client (e-board-registry-attach-client
                        board :principal principal :author "e-chat"))
               (requester (e-board-registry-client-requester-context
                           board (e-board-registry-client-id client)))
               (attachment
                (e-board-runtime-attach
                 board harness session-id :participant-id participant-id
                 :principal principal :controller principal :author "e-chat"))
               (participant (e-board-runtime-attachment-participant attachment))
               (_main-subscription
                (e-board-registry-install-subscription
                 board participant pickup-selector))
               (observer (e-board-registry-install-observer
                          board (e-board-registry-client-id client)
                          observer-selector :start-seq 0))
               (binding
                (e-chat-service--binding-create
                 :harness harness :session-id session-id :board board
                 :client client :requester requester :attachment attachment
                 :observer observer :subscribers nil
                 :turn-map (make-hash-table :test 'equal) :input-sequence 0
                 :default-tags (copy-tree default-tags) :default-to default-to)))
          (puthash session-id binding (e-chat-service--harness-bindings harness))
          (puthash (e-board-registry-board-id board)
                   (cons binding
                         (gethash (e-board-registry-board-id board)
                                  e-chat-service--board-bindings))
                   e-chat-service--board-bindings)
          (setf (e-board-message-notification-function
                 (e-board-registry-board-source-board board))
                (lambda (source _message)
                  (dolist (current (copy-sequence
                                    (gethash (e-board-id source)
                                             e-chat-service--board-bindings)))
                    (dolist (subscription
                             (copy-sequence
                              (e-chat-service-binding-subscribers current)))
                      (e-chat-service--schedule-subscription-drain
                       subscription)))))
          binding))))

(defun e-chat-service--bind-session (harness session-id)
  "Create one new top-level board and bind HARNESS SESSION-ID as its main member."
  (or (e-chat-service-binding harness session-id)
      (let* ((principal (format "chat:%s" session-id))
             (board (e-board-registry-create :principal principal)))
        (e-chat-service--install-participant-binding
         board harness session-id :principal principal))))

(cl-defun e-chat-service-create-board (&key harness metadata id)
  "Create a top-level board with one main participant and return its binding."
  (let* ((harness (or harness (e-chat-service-default-harness)))
         (session (e-harness-create-session harness :id id :metadata metadata)))
    (e-chat-service--bind-session harness (plist-get session :id))))

(cl-defun e-chat-service-open-board
    (board harness session-id &key participant-id pickup-selector
           observer-selector default-tags default-to)
  "Open existing BOARD by attaching HARNESS SESSION-ID as one participant."
  (e-chat-service--install-participant-binding
   (e-board-registry-get board) harness session-id
   :participant-id participant-id
   :pickup-selector (or pickup-selector '(:tags (main)))
   :observer-selector (or observer-selector '(:tags (main)))
   :default-tags (or default-tags '(main)) :default-to default-to))

(cl-defun e-chat-service-create-participant
    (board harness &key metadata id participant-id pickup-selector
           observer-selector default-tags default-to)
  "Create and attach a private execution session as a participant on BOARD."
  (let* ((session (e-harness-create-session harness :id id :metadata metadata))
         (session-id (plist-get session :id))
         (participant-id (or participant-id (format "participant:%s" session-id)))
         (binding
          (e-chat-service--install-participant-binding
           (e-board-registry-get board) harness session-id
           :participant-id participant-id
           :pickup-selector (or pickup-selector '(:tags (main)))
           :observer-selector
           (if (eq observer-selector :self)
               (list :subject-participant-id participant-id)
             (or observer-selector '(:tags (main))))
           :default-tags default-tags
           :default-to (if (eq default-to :self) participant-id default-to))))
    (ignore binding)
    session))

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

(defun e-chat-service-subscribe (harness session-id function)
  "Subscribe FUNCTION to board-observed events for HARNESS SESSION-ID."
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
                    :start-seq 0))
         (subscription (e-chat-service--subscription-create
                        :binding binding :function function :active-p t
                        :client client :observer observer :state 'active)))
    (setf (e-chat-service-binding-subscribers binding)
          (cons subscription (e-chat-service-binding-subscribers binding)))
    (e-chat-service--schedule-subscription-drain subscription)
    subscription))

(defun e-chat-service-unsubscribe (subscription)
  "Idempotently retire board-observer SUBSCRIPTION."
  (when (e-chat-service-subscription-p subscription)
    (setf (e-chat-service-subscription-active-p subscription) nil)
    (let ((binding (e-chat-service-subscription-binding subscription)))
      (setf (e-chat-service-binding-subscribers binding)
            (delq subscription
                  (e-chat-service-binding-subscribers binding)))
      (when-let ((client (e-chat-service-subscription-client subscription)))
        (ignore-errors
          (e-board-registry-detach-client
           (e-chat-service-binding-board binding)
           (e-board-registry-client-id client))))))
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
    (harness session-id prompt mode &key references metadata tags attributes to)
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
           (list (e-board-registry-client-id client)
                 (e-board-registry-client-generation client)
                 sequence))))
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
    (harness session-id prompt &key references metadata)
  "Queue PROMPT through HARNESS SESSION-ID's board binding."
  (e-chat-service--post harness session-id prompt 'queue
                        :references references :metadata metadata))

(defun e-chat-service-abort-session (harness session-id)
  "Abort the current board-bound turn for HARNESS SESSION-ID."
  (e-board-runtime-abort-attachment
   (e-chat-service-binding-attachment
    (e-chat-service--bind-session harness session-id))))

(defun e-chat-service-active-turn-p (harness session-id)
  "Return non-nil when HARNESS SESSION-ID's board participant is running."
  (e-board-runtime-attachment-active-turn-p
   (e-chat-service-binding-attachment
    (e-chat-service--bind-session harness session-id))))

(defun e-chat-service-session (harness session-id)
  "Return SESSION-ID's private metadata through the board application seam."
  (e-session-get (e-harness-sessions harness) session-id))

(defun e-chat-service-session-store (harness)
  "Return HARNESS's private session store for controlled shell metadata work."
  (e-harness-sessions harness))

(defun e-chat-service-session-list (harness)
  "Return HARNESS's private session catalog for bounded shell navigation."
  (e-harness-session-list harness))

(defun e-chat-service-root-session-list (harness)
  "Return HARNESS's private root-session catalog for shell navigation."
  (e-harness-root-session-list harness))

(defun e-chat-service-messages (harness session-id)
  "Return SESSION-ID's durable transcript for bounded initial presentation."
  (e-harness-messages harness session-id))

(defun e-chat-service-activity-events (harness session-id)
  "Return SESSION-ID's durable activity for bounded initial presentation."
  (e-session-activity-events (e-harness-sessions harness) session-id))

(defun e-chat-service-state (harness session-id)
  "Return SESSION-ID's current private state through the application seam."
  (e-harness-state harness session-id))

(defun e-chat-service-active-capabilities (harness)
  "Return HARNESS's active capabilities for shell affordance discovery."
  (e-harness-active-capabilities harness))

(defun e-chat-service-structured-blocks (harness session-id)
  "Return SESSION-ID's private structured-block registry projection."
  (e-harness-structured-blocks harness session-id))

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
  "Return SESSION-ID's queued inputs for bounded local presentation."
  (e-harness-queued-prompts harness session-id))

(defun e-chat-service-active-turns (harness)
  "Return HARNESS's private active-turn index for bounded shell diagnostics."
  (e-harness-active-turns harness))

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
