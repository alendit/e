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
  input-sequence)

(cl-defstruct (e-chat-service-subscription
               (:constructor e-chat-service--subscription-create))
  binding function active-p)

(defvar e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key)
  "Board-backed chat bindings, first by harness identity then session id.")

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
  (setf (e-chat-service-binding-observer-drain-scheduled binding) nil))

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

(defun e-chat-service--message-event (binding message)
  "Translate one immutable board MESSAGE for BINDING's existing reducers."
  (let* ((kind (e-board-message-kind message))
         (source-turn-id (e-board-message-source-turn-id message))
         (turn-id (e-chat-service--turn-id binding source-turn-id))
         (session-id (e-chat-service-binding-session-id binding))
         (created-at (float-time)))
    (pcase kind
      ('input
       (let ((message-id (e-board-message-id message)))
         (e-chat-service--enqueue-pending-input binding message-id)
         (list :type 'message-added :session-id session-id
               :turn-id message-id :created-at created-at
               :payload
               (list :message
                     (list :id message-id :role 'user
                           :content (e-board-message-content message)
                           :turn-id message-id
                           :references (e-board-message-reference message))))))
      ('output
       (list :type 'message-added :session-id session-id
             :turn-id turn-id :created-at created-at
             :payload
             (list :message
                   (list :id (e-board-message-id message) :role 'assistant
                         :content (e-board-message-content message)
                         :turn-id turn-id))))
      ('activity
       (let ((activity-kind (e-board-message-activity-kind message)))
         (when (eq activity-kind 'turn-started)
           (let ((input-id (pop (e-chat-service-binding-pending-input-head binding))))
             (unless (e-chat-service-binding-pending-input-head binding)
               (setf (e-chat-service-binding-pending-input-tail binding) nil))
             (when (and source-turn-id input-id)
               (puthash source-turn-id input-id
                        (e-chat-service-binding-turn-map binding))
               (setq turn-id input-id))))
         (when (eq activity-kind 'turn-summary)
           (setq activity-kind
                 (and (eq (plist-get (e-board-message-attributes message)
                                     :status)
                          'finished)
                      'turn-finished)))
         (when activity-kind
           (list :type activity-kind :session-id session-id
                 :turn-id (e-chat-service--turn-id binding source-turn-id)
                 :created-at created-at
                 :payload (copy-tree (e-board-message-attributes message))))))
      (_
       (list :type 'board-fact :session-id session-id
             :turn-id turn-id :created-at created-at
             :payload (list :message-id (e-board-message-id message)
                            :content (e-board-message-content message)))))))

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

(defun e-chat-service--bind-session (harness session-id)
  "Create and install the board-only live binding for HARNESS SESSION-ID."
  (or (e-chat-service-binding harness session-id)
      (progn
        (e-session-get (e-harness-sessions harness) session-id)
        (let* ((principal (format "chat:%s" session-id))
               (board (e-board-registry-create :principal principal))
               (client (e-board-registry-attach-client
                        board :principal principal :author "e-chat"))
               (requester (e-board-registry-client-requester-context
                           board (e-board-registry-client-id client)))
               (attachment
                (e-board-runtime-attach
                 board harness session-id :principal principal
                 :controller principal :author "e-chat"))
               (participant (e-board-runtime-attachment-participant attachment))
               (_main-subscription
                (e-board-registry-install-subscription
                 board participant '(:tags (main))))
               (observer (e-board-registry-install-observer
                          board (e-board-registry-client-id client)
                          '(:tags (main)) :start-seq 0))
               (binding
                (e-chat-service--binding-create
                 :harness harness :session-id session-id :board board
                 :client client :requester requester :attachment attachment
                 :observer observer :subscribers nil
                 :turn-map (make-hash-table :test 'equal) :input-sequence 0)))
          (setf (e-board-message-notification-function
                 (e-board-registry-board-source-board board))
                (lambda (_source _message)
                  (e-chat-service--schedule-observer-drain binding)))
          (puthash session-id binding (e-chat-service--harness-bindings harness))
          binding))))

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
  "Create a private session plus its board binding and return the session."
  (let* ((harness (or harness (e-chat-service-default-harness)))
         (session (e-harness-create-session harness :id id :metadata metadata)))
    (e-chat-service--bind-session harness (plist-get session :id))
    session))

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
         (subscription (e-chat-service--subscription-create
                        :binding binding :function function :active-p t)))
    (setf (e-chat-service-binding-subscribers binding)
          (cons subscription (e-chat-service-binding-subscribers binding)))
    subscription))

(defun e-chat-service-unsubscribe (subscription)
  "Idempotently retire board-observer SUBSCRIPTION."
  (when (e-chat-service-subscription-p subscription)
    (setf (e-chat-service-subscription-active-p subscription) nil)
    (let ((binding (e-chat-service-subscription-binding subscription)))
      (setf (e-chat-service-binding-subscribers binding)
            (delq subscription
                  (e-chat-service-binding-subscribers binding)))))
  nil)

(cl-defun e-chat-service--post
    (harness session-id prompt mode &key references metadata)
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
           :tags '(main)
           :attributes (append (copy-tree metadata)
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
