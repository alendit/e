;;; e-loop.el --- Agent turn loop for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral synchronous turn loop for core runtime tests.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-backend)
(require 'e-request)
(require 'e-session)
(require 'e-tools)
(require 'e-work)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(define-error 'e-loop-backend-error "Backend returned an error")
(define-error 'e-loop-empty-output "Backend returned no assistant output")

(defun e-loop--profile-enabled-p ()
  "Return non-nil when developer profiling is available and enabled."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-loop--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when dev profiling is enabled."
  (if (e-loop--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-loop--assistant-message (content &optional metadata)
  "Return an assistant message with CONTENT and optional METADATA."
  (list :role 'assistant
        :content content
        :metadata metadata))

(cl-defun e-loop--emit (&key on-event type payload)
  "Report internal turn descriptor TYPE and PAYLOAD through ON-EVENT."
  (funcall on-event type payload))

(defun e-loop--diagnostic-scalar-p (value)
  "Return non-nil when VALUE is safe for lifecycle diagnostics."
  (or (null value)
      (stringp value)
      (numberp value)
      (symbolp value)))

(defun e-loop--sanitize-diagnostics (diagnostics)
  "Return scalar-only DIAGNOSTICS as a plist."
  (when (listp diagnostics)
    (let ((rest diagnostics)
          sanitized)
      (while (and (consp rest) (consp (cdr rest)))
        (let ((key (car rest))
              (value (cadr rest)))
          (when (and (keywordp key)
                     (e-loop--diagnostic-scalar-p value))
            (setq sanitized (append sanitized (list key value)))))
        (setq rest (cddr rest)))
      sanitized)))

(defun e-loop--normalized-backend-item (item)
  "Return backend-neutral ITEM with legacy provider spellings normalized."
  (let ((type (plist-get item :type)))
    (cond
     ((member type '(tool_call "tool_call" "tool-call"))
      (plist-put (copy-sequence item) :type 'tool-call))
     (t item))))

(defun e-loop--continuation-candidate-p (options candidate)
  "Return non-nil when CANDIDATE may continue the request in OPTIONS."
  (and (plist-get options :provider-continuation)
       (eq (plist-get candidate :provider-id)
           (plist-get options :provider-anchor-provider-id))))

(defun e-loop--promote-continuation-candidate
    (options candidate source-message-count delta-messages)
  "Return OPTIONS advanced to CANDIDATE for an in-turn follow-up.
SOURCE-MESSAGE-COUNT covers the local transcript through the completed
provider response.  DELTA-MESSAGES are new client inputs, normally tool
results, that the stored response does not contain."
  (if (not (e-loop--continuation-candidate-p options candidate))
      options
    (let ((advanced (copy-sequence options)))
      (setq advanced
            (plist-put advanced
                       :provider-anchor
                       (list :provider-id (plist-get candidate :provider-id)
                             :metadata (copy-tree
                                        (plist-get candidate :metadata)))))
      (setq advanced
            (plist-put advanced
                       :provider-anchor-delta-messages
                       (copy-tree delta-messages)))
      (plist-put advanced
                 :provider-anchor-source-message-count
                 source-message-count))))

(defun e-loop--shape-value (value)
  "Return durable hash and byte length for model-visible VALUE."
  (let ((text (prin1-to-string value)))
    (list :sha256 (secure-hash 'sha256 text)
          :bytes (string-bytes text))))

(defun e-loop--without-marker-messages (messages)
  "Return MESSAGES with complete process_marker call/result pairs removed."
  (let* ((call-ids
          (delq nil
                (mapcar
                 (lambda (message)
                   (when (and (eq (plist-get message :role) 'tool-call)
                              (equal (plist-get
                                      (plist-get message :content) :name)
                                     "process_marker"))
                     (plist-get (plist-get message :content) :id)))
                 messages))))
    (seq-remove
     (lambda (message)
       (or (and (eq (plist-get message :role) 'tool-call)
                (member (plist-get (plist-get message :content) :id)
                        call-ids))
           (and (eq (plist-get message :role) 'tool)
                (member (plist-get (plist-get message :content)
                                   :tool-call-id)
                        call-ids))))
     messages)))

(defun e-loop--marker-guidance-segment-p (segment)
  "Return non-nil when SEGMENT is process-reporting capability guidance."
  (equal (plist-get segment :id) '(process-reporting instructions)))

(defun e-loop--remove-message-once (messages target)
  "Return MESSAGES with the first message equal to TARGET removed."
  (let (removed result)
    (dolist (message messages (nreverse result))
      (if (and (not removed) (equal message target))
          (setq removed t)
        (push message result)))))

(defun e-loop--without-marker-guidance (messages segments)
  "Return MESSAGES without guidance identified by context SEGMENTS."
  (let ((result messages))
    (dolist (segment segments result)
      (when (e-loop--marker-guidance-segment-p segment)
        (dolist (message (plist-get segment :messages))
          (setq result (e-loop--remove-message-once result message)))))))

(defun e-loop--without-marker-tool (tools)
  "Return TOOLS without the process_marker descriptor."
  (seq-remove (lambda (tool)
                (equal (plist-get tool :name) "process_marker"))
              tools))

(defun e-loop--request-snapshot (messages options)
  "Return one backend-neutral provider request value."
  (let ((copy (copy-tree options)))
    (plist-put copy :messages (copy-tree messages))
    copy))

(defun e-loop--request-shape (messages options segments)
  "Measure actual and marker-free request shapes without retaining content."
  (let* ((actual (e-loop--request-snapshot messages options))
         (no-active-messages (e-loop--without-marker-messages messages))
         (no-passive-messages (e-loop--without-marker-guidance messages segments))
         (paired-messages
          (e-loop--without-marker-guidance no-active-messages segments))
         (no-passive-options (copy-tree options))
         (paired-options (copy-tree options)))
    (plist-put no-passive-options :tools
               (e-loop--without-marker-tool
                (plist-get no-passive-options :tools)))
    (plist-put paired-options :tools
               (e-loop--without-marker-tool
                (plist-get paired-options :tools)))
    (let ((without-passive
           (e-loop--request-snapshot no-passive-messages no-passive-options))
          (without-active
           (e-loop--request-snapshot no-active-messages options))
          (paired
           (e-loop--request-snapshot paired-messages paired-options)))
      (list :revision "request-shape-v2"
            :serialization "backend-neutral-elisp-v1"
            :tokenizer-revision (plist-get options :tokenizer-revision)
            :actual-shape (e-loop--shape-value actual)
            :without-passive-shape (e-loop--shape-value without-passive)
            :without-active-shape (e-loop--shape-value without-active)
            :paired-shape (e-loop--shape-value paired)
            :model (plist-get options :model)
            :reasoning-effort (or (plist-get options :reasoning-effort)
                                  (plist-get options :effort))
            :prompt-cache-key-present
            (and (plist-get options :prompt-cache-key) t)
            :prompt-cache-key-sha256
            (when-let ((key (plist-get options :prompt-cache-key)))
              (secure-hash 'sha256 (format "%s" key)))
            :prompt-cache-retention
            (plist-get options :prompt-cache-retention)))))

(defun e-loop--request-cause-fields (causes)
  "Return stable lifecycle fields for completed tool-call CAUSES."
  (when causes
    (let* ((last-cause (car (last causes)))
           (records
            (mapcar (lambda (cause)
                      (list :id (plist-get cause :id)
                            :name (plist-get cause :name)))
                    causes)))
      (list :caused-by-tool-call-id (plist-get last-cause :id)
            :caused-by-tool-name (plist-get last-cause :name)
            :caused-by-tool-calls (vconcat records)))))

(defun e-loop--request-lifecycle-payload
    (request status request-id request-ordinal request-shape
             &optional started-at causes)
  "Return sanitized lifecycle payload for REQUEST with stable identity.
REQUEST-ID and REQUEST-ORDINAL join all events for one provider request.
REQUEST-SHAPE contains hashes and sizes of model-visible request ingredients.
STARTED-AT is the `float-time' value captured when the request was published.
CAUSES lists every completed tool call that induced a follow-up request."
  (let* ((metadata (and (e-backend-request-p request)
                        (e-backend-request-metadata request)))
         (payload (list :provider-request-id request-id
                        :provider-request-ordinal request-ordinal
                        :request-shape request-shape
                        :provider (plist-get metadata :provider)
                        :transport (plist-get metadata :transport)
                        :url-host (plist-get metadata :url-host)
                        :url-path (plist-get metadata :url-path)
                        :timeout-seconds (plist-get metadata
                                                     :timeout-seconds)
                        :deadline (plist-get metadata :deadline)
                        :status status)))
    (when causes
      (setq payload
            (append payload (e-loop--request-cause-fields causes))))
    (when-let ((diagnostics
                (e-loop--sanitize-diagnostics
                 (plist-get metadata :diagnostics))))
      (setq payload (append payload (list :diagnostics diagnostics))))
    (when started-at
      (plist-put payload
                 :elapsed-seconds
                 (/ (float (round (* 1000 (max 0.0
                                                (- (float-time)
                                                   started-at)))))
                    1000)))
    payload))

(defun e-loop--backend-work-request (handle)
  "Return an `e-backend-request' projection for backend work HANDLE."
  (let* ((metadata (and (e-work-handle-p handle)
                        (e-work-handle-metadata handle)))
         (provider-metadata
          (copy-sequence
           (or (plist-get metadata :backend-request-metadata) nil))))
    (e-backend-request-create
     :cancel (lambda ()
               (when (e-work-handle-p handle)
                 (e-work-cancel handle)))
     :metadata
     (append provider-metadata
             (list :work-id (and (e-work-handle-p handle)
                                 (e-work-handle-id handle))
                   :work-handle handle
                   :work-transport (plist-get metadata :transport)
                   :deadline (plist-get metadata :deadline)
                   :backend-request
                   (plist-get metadata :backend-request))))))

(cl-defun e-loop-start-turn
    (&key session-id turn-id messages backend tools tool-lifecycle options on-event
            append-message refresh-messages on-request-start on-done on-error
            cancelled-p drain-pending-input segments turn-work-handle
            board-enroll-work)
  "Start one async agent turn for SESSION-ID and TURN-ID.
MESSAGES, BACKEND, TOOLS, TOOL-LIFECYCLE, and OPTIONS describe the turn input.
ON-EVENT, APPEND-MESSAGE, REFRESH-MESSAGES, ON-REQUEST-START, ON-DONE,
ON-ERROR, CANCELLED-P, and DRAIN-PENDING-INPUT receive turn progress, output,
refreshed context, provider request handles, settlement, failures,
cancellation state, and same-turn pending user input.  The provider request is
started through `e-backend-start'.  Tool execution is started through
TOOL-LIFECYCLE when supplied, otherwise through `e-tools-start'.  Provider I/O,
tool I/O, and turn settlement are callback-driven."
  (let ((turn-messages (copy-sequence messages))
        (turn-options (copy-sequence options))
        (settled nil)
        (active-request nil)
        (provider-request-sequence 0)
        (next-request-causes nil))
    (cl-labels
        ((cancelled ()
           (and cancelled-p (funcall cancelled-p)))
         (fail
          (err)
          (unless settled
            (setq settled t)
            (when on-error
              (funcall on-error err))))
         (finish
          (done-reason assistant-content)
          (unless settled
            (setq settled t)
            (e-loop--emit :on-event on-event
                          :type 'turn-finished
                          :payload (list :reason done-reason))
            (when on-done
              (funcall on-done
                       (list :status 'done
                             :reason done-reason
                             :assistant-content assistant-content)))))
         (publish-request
          (request)
          (setq active-request request)
          (when on-request-start
            (funcall on-request-start request)))
         (drain-pending
          ()
          (let ((pending (and drain-pending-input
                              (funcall drain-pending-input))))
            (when pending
              (dolist (message pending)
                (setq turn-messages (append turn-messages (list message)))
                (funcall append-message message))
              t)))
         (start-request
          ()
          (unless (or settled (cancelled))
            (drain-pending)
            (let ((tool-called nil)
                  (tool-queue nil)
                  (active-tool nil)
                  (provider-done nil)
                  (followup-started nil)
                  (response-assistant-content nil)
                  (response-assistant-message nil)
                  (token-usage nil)
                  (done-reason nil)
                  (provider-anchor-candidate nil)
                  (provider-followup-messages nil)
                  (provider-request nil)
                  (provider-request-id nil)
                  (provider-request-ordinal nil)
                  (provider-request-shape nil)
                  (provider-request-started-at nil)
                  (provider-request-finished nil)
                  (provider-request-causes next-request-causes))
              (cl-labels
                  ((response-text ()
                     (or response-assistant-message
                         response-assistant-content))
                   (publish-provider-request
                    (request)
                    (setq provider-request request)
                    (setq next-request-causes nil)
                    (setq provider-request-sequence
                          (1+ provider-request-sequence))
                    (setq provider-request-id (e-session-generate-ulid))
                    (setq provider-request-ordinal provider-request-sequence)
                    (setq provider-request-shape
                          (e-loop--request-shape
                           turn-messages turn-options segments))
                    (setq provider-request-started-at (float-time))
                    (setq provider-request-finished nil)
                    (publish-request request)
                    (e-loop--emit
                     :on-event on-event
                     :type 'provider-request-started
                     :payload
                     (e-loop--request-lifecycle-payload
                      request 'started provider-request-id
                      provider-request-ordinal provider-request-shape nil
                      provider-request-causes)))
                   (finish-provider-request
                    (status)
                    (when (and provider-request
                               (not provider-request-finished))
                      (setq provider-request-finished t)
                      (e-loop--emit
                       :on-event on-event
                       :type 'provider-request-finished
                       :payload
                       (e-loop--request-lifecycle-payload
                        provider-request status provider-request-id
                        provider-request-ordinal provider-request-shape
                        provider-request-started-at
                        provider-request-causes))))
                   (promote-provider-anchor
                    ()
                    (when provider-anchor-candidate
                      (setq turn-options
                            (e-loop--promote-continuation-candidate
                             turn-options
                             provider-anchor-candidate
                             (length turn-messages)
                             provider-followup-messages))))
                   (fail-provider
                    (err)
                    (finish-provider-request 'error)
                    (fail err))
                   (maybe-start-followup
                    ()
                    (when (and provider-done
                               tool-called
                               (not active-tool)
                               (null tool-queue)
                               (not followup-started)
                               (not settled)
                               (not (cancelled)))
                      (setq followup-started t)
                      (when (not (string-empty-p
                                  (or (response-text) "")))
                        (e-loop--emit
                         :on-event on-event
                         :type 'reasoning-delta
                         :payload
                         (list :type 'reasoning-delta
                               :stream-kind 'summary
                               :content (response-text))))
                      (promote-provider-anchor)
                      (start-request)))
                   (current-tool-p
                    (token)
                    (and (listp active-tool)
                         (eq (plist-get active-tool :token) token)))
                   (publish-tool-request
                    (token request)
                    (when (and (current-tool-p token)
                               (not settled)
                               (not (cancelled)))
                      (setq active-tool
                            (list :token token :request request))
                      (publish-request request)))
                   (finish-tool
                    (token tool-call result)
                    (when (and (not settled)
                               (not (cancelled))
                               (current-tool-p token))
                      (setq active-tool nil)
                      (let ((message
                             (list :role 'tool
                                   :content result
                                   :metadata (plist-get result :metadata))))
                        (setq turn-messages
                              (append turn-messages (list message)))
                        (setq provider-followup-messages
                              (append provider-followup-messages
                                      (list message)))
                        (funcall append-message message)
                        (e-loop--emit
                         :on-event on-event
                         :type 'tool-finished
                         :payload (list :tool-call tool-call
                                        :result result)))
                      (when (and refresh-messages
                                 (plist-get (plist-get result :metadata)
                                            :refresh-context))
                        (setq turn-messages (funcall refresh-messages)))
                      (setq next-request-causes
                            (append next-request-causes (list tool-call)))
                      (start-next-tool)
                      (maybe-start-followup)))
                   (start-next-tool
                    ()
                    (when (and (not active-tool)
                               tool-queue
                               (not settled)
                               (not (cancelled)))
                      (condition-case err
                          (let* ((entry (pop tool-queue))
                                 (execution-call
                                  (if tool-lifecycle
                                      (e-tool-lifecycle-prepare-call
                                       tool-lifecycle
                                       (plist-get entry :tool-call))
                                    (plist-get entry :tool-call)))
                                 (tool-call
                                  (if (not (e-tools-registry-p tools))
                                      execution-call
                                    (condition-case nil
                                        (e-tools-prepare-call
                                         tools execution-call)
                                      (error
                                       ;; Keep provider protocol shape while
                                       ;; dropping undeclared rejected fields
                                       ;; before transcript and activity writes.
                                       (e-tools-project-call-for-rejection
                                        tools execution-call)))))
                                 (tool-token (list :tool-call tool-call))
                                 (tool-call-message
                                  (list :role 'tool-call
                                        :content tool-call
                                        :metadata nil)))
                            (setq active-tool (list :token tool-token))
                            (setq turn-messages
                                  (append turn-messages
                                          (list tool-call-message)))
                            (funcall append-message tool-call-message)
                            (e-loop--emit :on-event on-event
                                          :type 'tool-started
                                          :payload tool-call)
                            (let ((request
                                   (if tool-lifecycle
                                       (e-tool-lifecycle-start-call
                                        tool-lifecycle
                                        execution-call
                                        :on-request-start
                                        (lambda (request)
                                          (publish-tool-request
                                           tool-token request))
                                        :on-event
                                        (lambda (type payload)
                                          (e-loop--emit
                                           :on-event on-event
                                           :type type
                                           :payload payload))
                                        :on-done
                                        (lambda (result)
                                          (finish-tool
                                           tool-token tool-call result))
                                        :on-error #'fail)
                                     (e-tools-start
                                      tools
                                      execution-call
                                       :context
                                       (list :session-id session-id
                                             :turn-id turn-id
                                             :parent-work-id
                                             (and (e-work-handle-p turn-work-handle)
                                                  (e-work-handle-id turn-work-handle))
                                              :root-work-id
                                              (and (e-work-handle-p turn-work-handle)
                                                   (e-work-handle-id turn-work-handle))
                                              :board-enroll-work board-enroll-work
                                              :deadline
                                              (plist-get turn-options :deadline))
                                      :on-request-start
                                      (lambda (request)
                                        (publish-tool-request
                                         tool-token request))
                                      :on-event
                                      (lambda (type payload)
                                        (e-loop--emit
                                         :on-event on-event
                                         :type type
                                         :payload payload))
                                      :on-done
                                      (lambda (result)
                                        (finish-tool
                                         tool-token tool-call result))
                                      :on-error #'fail))))
                              (when (and request
                                         (current-tool-p tool-token)
                                         (not settled)
                                         (not (cancelled)))
                                (setq active-tool
                                      (list :token tool-token
                                            :request request)))))
                        (error
                         (fail err)))))
                  (handle-backend-item
                   (item)
                   (unless (or settled (cancelled))
                     (condition-case err
                         (progn
                           (setq item (e-loop--normalized-backend-item item))
                           (pcase (plist-get item :type)
                             ('assistant-delta
                              (setq response-assistant-content
                                    (concat response-assistant-content
                                            (plist-get item :content)))
                              (e-loop--emit :on-event on-event
                                            :type 'assistant-delta
                                            :payload item))
                             ('assistant-message
                              (setq response-assistant-message
                                    (plist-get item :content)))
                             ('reasoning-delta
                              (e-loop--emit :on-event on-event
                                            :type 'reasoning-delta
                                            :payload item))
                             ('reasoning-raw-delta
                              (e-loop--emit :on-event on-event
                                            :type 'reasoning-raw-delta
                                            :payload item))
                             ('tool-call
                              (enqueue-tool-call item))
                             ('token-usage
                              (setq token-usage
                                    (append
                                     (copy-sequence (plist-get item :usage))
                                     (list :provider-request-id
                                           provider-request-id
                                           :provider-request-ordinal
                                           provider-request-ordinal)
                                     (e-loop--request-cause-fields
                                      provider-request-causes)))
                              (e-loop--emit
                               :on-event on-event
                               :type 'token-usage
                               :payload token-usage))
                             ('provider-anchor-candidate
                              (when (e-loop--continuation-candidate-p
                                     turn-options item)
                                (setq provider-anchor-candidate item))
                              (e-loop--emit
                               :on-event on-event
                               :type 'provider-anchor-candidate
                               :payload item))
                             ('done
                              (setq done-reason
                                    (plist-get item :reason)))
                             ('backend-error
                              (fail-provider
                               (list 'e-loop-backend-error
                                     (plist-get item :content)
                                     (plist-get item :payload))))
                             (_
                              (e-loop--emit
                               :on-event on-event
                               :type 'backend-item-ignored
                               :payload item))))
                       (error
                        (fail-provider err)))))
                  (enqueue-tool-call
                   (item)
                   (setq tool-called t)
                   (setq tool-queue
                         (append tool-queue
                                 (list (list :tool-call item))))
                   (start-next-tool)))
              (setq
               active-request
               (condition-case err
                   (let ((reported-request nil))
                     (let* ((work-handle
                             (e-loop--profile-call
                              'loop.backend-start
                              (list :session-id session-id
                                    :turn-id turn-id
                                    :metadata
                                    (list :message-count (length turn-messages)
                                          :tool-count (length tools)))
                              (lambda ()
                                (e-work-start
                                 (e-work-spec-create
                                  :id "backend_turn"
                                  :description "Run one provider turn."
                                  :execution 'backend
                                  :interactive-policy 'async
                                  :owner 'loop
                                  :backend (lambda (_arguments _context)
                                             backend)
                                  :messages (lambda (_arguments _context)
                                              turn-messages)
                                  :options (lambda (_arguments _context)
                                             turn-options)
                                  :request-handler
                                  (lambda (handle _request _arguments _context)
                                    (let ((request
                                           (e-loop--backend-work-request
                                            handle)))
                                      (setq reported-request request)
                                      (publish-provider-request request)))
                                  :item-handler
                                  (lambda (_handle item _arguments _context)
                                    (handle-backend-item item)))
                                 nil
                                  :context (list :session-id session-id
                                                 :turn-id turn-id
                                                 :parent-work-id
                                                 (and (e-work-handle-p turn-work-handle)
                                                      (e-work-handle-id turn-work-handle))
                                                 :root-work-id
                                                 (and (e-work-handle-p turn-work-handle)
                                                      (e-work-handle-id turn-work-handle))
                                                 :deadline
                                                 (plist-get turn-options :deadline))
                                 :on-done
                                 (lambda (_backend-result)
                                   (unless (or settled (cancelled))
                                     (condition-case err
                                         (progn
                                           (finish-provider-request 'done)
                                           (setq provider-done t)
                                           (if tool-called
                                               (maybe-start-followup)
                                             (if (string-empty-p
                                                  (or (response-text) ""))
                                                 (progn
                                                   (e-loop--emit
                                                    :on-event on-event
                                                    :type 'backend-empty-output
                                                    :payload (list :reason
                                                                   done-reason))
                                                   (fail '(e-loop-empty-output)))
                                               (let ((message
                                                      (e-loop--assistant-message
                                                       (response-text))))
                                                 (setq turn-messages
                                                       (append turn-messages
                                                               (list message)))
                                                 (funcall append-message message)
                                                 (promote-provider-anchor)
                                                 (if (drain-pending)
                                                     (start-request)
                                                   (finish done-reason
                                                           (response-text)))))))
                                       (error
                                        (fail-provider err)))))
                                 :on-error #'fail-provider))))
                            (request
                             (or reported-request
                                 (when (and work-handle (not settled))
                                   (when (plist-get
                                          (e-work-handle-metadata work-handle)
                                          :backend-request)
                                     (e-loop--backend-work-request
                                      work-handle))))))
                       (when (and request
                                  (not settled)
                                  (not (eq request reported-request)))
                         (publish-provider-request request))
                       request))
                 (error
                  (fail-provider err)
                  nil))))))))
      (e-loop--emit :on-event on-event
                    :type 'turn-started
                    :payload nil)
      (start-request)
      active-request)))

(cl-defun e-loop-run-turn-batch
    (&key session-id turn-id messages backend tools tool-lifecycle options on-event
            append-message refresh-messages on-request-start segments turn-work-handle
            board-enroll-work)
  "Synchronously run one agent turn from batch/test code.
SESSION-ID and TURN-ID identify the turn.
MESSAGES, BACKEND, TOOLS, TOOL-LIFECYCLE, OPTIONS, ON-EVENT, APPEND-MESSAGE,
and REFRESH-MESSAGES define the turn context and output callbacks.
ON-REQUEST-START receives the backend request handle when an adapter exposes
one."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-loop-run-turn-batch))
  (let ((done nil)
        (result nil)
        (failure nil))
    (e-loop-start-turn
     :session-id session-id
     :turn-id turn-id
     :messages messages
     :backend backend
     :tools tools
     :tool-lifecycle tool-lifecycle
      :options options
      :turn-work-handle turn-work-handle
      :board-enroll-work board-enroll-work
     :segments segments
     :on-event on-event
     :append-message append-message
     :refresh-messages refresh-messages
     :on-request-start on-request-start
     :on-done (lambda (value)
                (setq result value)
                (setq done t))
     :on-error (lambda (err)
                 (setq failure err)
                 (setq done t)))
    (while (not done)
      (accept-process-output nil 0.01))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(provide 'e-loop)

;;; e-loop.el ends here
