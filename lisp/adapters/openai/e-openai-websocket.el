;;; e-openai-websocket.el --- Responses WebSocket transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns WebSocket connection/session lifecycle, cancellation, idle close, and
;; continuation retry.  Event decoding is delegated to e-openai-decoder.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'websocket)
(require 'e-backend)
(require 'e-json)
(require 'e-openai-profile)
(require 'e-openai-diagnostics)
(require 'e-openai-decoder)

(defun e-openai-websocket--plist-without (plist key)
  "Return PLIST without KEY."
  (let (result)
    (while plist
      (let ((current-key (pop plist))
            (value (pop plist)))
        (unless (eq current-key key)
          (setq result (append result (list current-key value))))))
    result))

(defun e-openai-websocket--frame-text (frame)
  "Return text payload from WebSocket FRAME."
  (cond
   ((stringp frame) frame)
   ((websocket-frame-payload frame))
   (t "")))
(cl-defstruct
    (e-openai-websocket--session
     (:constructor e-openai-websocket-session-create))
  websocket
  url
  headers
  close-function
  connection-id
  reuse-count
  latest-response-id
  latest-response-properties
  active-request
  idle-timer)

(defvar e-openai-websocket--connection-sequence 0
  "Process-local sequence for bounded WebSocket connection diagnostics.")

(define-error 'e-openai-websocket-busy
  "Responses WebSocket already has an active request")

(defun e-openai-websocket--json-value-copy (value)
  "Return a detached copy of canonical JSON VALUE.
The WebSocket continuation snapshot contains only canonical objects and
arrays, so hash tables, alists, and list arrays are invalid here."
  (e-json-assert-value value)
  (cond
   ((stringp value)
    (copy-sequence value))
   ((vectorp value)
    (apply #'vector
           (mapcar #'e-openai-websocket--json-value-copy value)))
   ((consp value)
    (let ((copy nil)
          (cursor value))
      (while cursor
        (setq copy
              (append copy
                      (list (car cursor)
                            (e-openai-websocket--json-value-copy
                             (cadr cursor)))))
        (setq cursor (cddr cursor)))
      copy))
   (t value)))

(defun e-openai-websocket--session-clear-response-state (session)
  "Clear the immediate continuation state owned by SESSION."
  (setf (e-openai-websocket--session-latest-response-id session) nil)
  (setf (e-openai-websocket--session-latest-response-properties session)
        nil)
  session)

(defun e-openai-websocket--session-record-response
    (session response-id properties)
  "Record only the latest completed RESPONSE-ID and PROPERTIES for SESSION.

The state is sufficient for the matching immediate function-call-output
continuation.  Older response identities are deliberately forgotten rather
than retained as a connection-local response graph."
  (e-openai-websocket--session-clear-response-state session)
  (when (and (stringp response-id) (not (string-empty-p response-id)))
    (setf (e-openai-websocket--session-latest-response-id session)
          response-id)
    (setf (e-openai-websocket--session-latest-response-properties session)
          (e-openai-websocket--json-value-copy properties)))
  session)

(defun e-openai-websocket--cancel-idle-close (session)
  "Cancel SESSION's pending idle close timer."
  (when-let* ((timer (e-openai-websocket--session-idle-timer session)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (e-openai-websocket--session-idle-timer session) nil)))

(defun e-openai-websocket--session-close (session)
  "Close SESSION's connection and discard its warm continuation state."
  (e-openai-websocket--cancel-idle-close session)
  (e-openai-websocket--session-clear-response-state session)
  (let ((websocket (e-openai-websocket--session-websocket session))
        (close-function
         (e-openai-websocket--session-close-function session)))
    ;; Clear identity first so the library's synchronous on-close callback is
    ;; recognized as intentional cleanup rather than a transport failure.
    (setf (e-openai-websocket--session-websocket session) nil)
    (setf (e-openai-websocket--session-url session) nil)
    (setf (e-openai-websocket--session-headers session) nil)
    (setf (e-openai-websocket--session-close-function session) nil)
    (setf (e-openai-websocket--session-reuse-count session) 0)
    (when (and websocket close-function)
      (funcall close-function websocket))))

(defun e-openai-websocket-session-close (session)
  "Close opaque Responses WebSocket SESSION and clear its active request.
This is the transport lifecycle operation used by adapter teardown and
integration fixtures; callers do not need to inspect the session record."
  (unless (e-openai-websocket--session-p session)
    (signal 'wrong-type-argument
            (list 'e-openai-websocket-session-p session)))
  (e-openai-websocket--session-close session))

(defun e-openai-websocket--active-handler (session key &rest arguments)
  "Call KEY handler for SESSION's active request with ARGUMENTS."
  (when-let* ((active
               (e-openai-websocket--session-active-request session))
              (handler (plist-get active key)))
    (apply handler arguments)))

(defun e-openai-websocket--session-open (session url headers)
  "Open SESSION for URL and HEADERS and return the connection."
  (e-openai-websocket--session-close session)
  (let* ((close-function (symbol-function 'websocket-close))
         (connection-id
          (format "e-ws-%d"
                  (cl-incf e-openai-websocket--connection-sequence)))
         websocket)
    (setq websocket
          (websocket-open
           url
           :custom-header-alist headers
           :on-message
           (lambda (candidate frame)
             (when (eq candidate
                       (e-openai-websocket--session-websocket session))
               (e-openai-websocket--active-handler
                session :on-message candidate frame)))
           :on-close
           (lambda (candidate &rest _args)
             (when (eq candidate
                       (e-openai-websocket--session-websocket session))
               (let ((active
                      (e-openai-websocket--session-active-request
                       session)))
                 (e-openai-websocket--cancel-idle-close session)
                 (setf (e-openai-websocket--session-websocket session) nil)
                 (setf (e-openai-websocket--session-url session) nil)
                 (setf (e-openai-websocket--session-headers session) nil)
                 (setf (e-openai-websocket--session-close-function
                        session)
                       nil)
                 (e-openai-websocket--session-clear-response-state
                  session)
                 (setf (e-openai-websocket--session-active-request session)
                       nil)
                 (when-let* ((handler (plist-get active :on-close)))
                   (funcall handler candidate)))))
           :on-error
           (lambda (candidate &rest args)
             (when (eq candidate
                       (e-openai-websocket--session-websocket session))
               (e-openai-websocket--active-handler
                session :on-error candidate args)))))
    (setf (e-openai-websocket--session-websocket session) websocket)
    (setf (e-openai-websocket--session-url session) url)
    (setf (e-openai-websocket--session-headers session)
          (copy-tree headers))
    (setf (e-openai-websocket--session-close-function session)
          close-function)
    (setf (e-openai-websocket--session-connection-id session)
          connection-id)
    (setf (e-openai-websocket--session-reuse-count session) 0)
    websocket))

(defun e-openai-websocket--schedule-idle-close
    (session idle-close-seconds)
  "Schedule SESSION's idle close using IDLE-CLOSE-SECONDS.
The effective policy is resolved by the request context before the WebSocket
request starts; this owner never reads the global fallback directly."
  (e-openai-websocket--cancel-idle-close session)
  (when (and (numberp idle-close-seconds)
             (>= idle-close-seconds 0)
             (e-openai-websocket--session-websocket session))
    (let ((connection-id
           (e-openai-websocket--session-connection-id session)))
      (setf
       (e-openai-websocket--session-idle-timer session)
       (run-at-time
        idle-close-seconds nil
        (lambda ()
          (setf (e-openai-websocket--session-idle-timer session) nil)
          (when (and
                 (null (e-openai-websocket--session-active-request session))
                 (equal connection-id
                        (e-openai-websocket--session-connection-id
                         session)))
            (e-openai-websocket--session-close session))))))))

(defun e-openai-websocket--request-properties (body-data)
  "Return continuation-invariant request properties from BODY-DATA.
The Responses API replaces top-level `instructions' when a request supplies
`previous_response_id', so changed instructions do not invalidate the warm
response chain."
  (e-openai-websocket--plist-without
   (e-openai-websocket--plist-without
    (e-openai-websocket--plist-without body-data :input)
    :previous_response_id)
   :instructions))

(defun e-openai-websocket--json-value-equal-p (first second)
  "Return non-nil when canonical JSON FIRST and SECOND are equivalent."
  (e-json-assert-value first)
  (e-json-assert-value second)
  (equal first second))

(defun e-openai-websocket--unresolved-response-p (event)
  "Return non-nil when EVENT rejects an unavailable previous response id."
  ;; The Responses WebSocket protocol has emitted this rejection both as a
  ;; response-scoped failure and as a top-level error event.  They have the
  ;; same causal meaning: the immediate connection-local response cannot be
  ;; continued, so the one bounded recovery is a complete canonical request.
  (when (member (plist-get event :type) '("response.failed" "error"))
    (let* ((response (plist-get event :response))
           (error (or (plist-get response :error)
                      (plist-get event :error)))
           (code (or (and (listp error) (plist-get error :code))
                     (plist-get event :code)))
           (param (or (and (listp error) (plist-get error :param))
                      (plist-get event :param)))
           (message (downcase
                     (or (and (listp error) (plist-get error :message))
                         (and (stringp error) error)
                         (plist-get event :message)
                         ""))))
      (or (equal param "previous_response_id")
          (member code '("previous_response_not_found"
                         "response_not_found"
                         "unknown_previous_response_id"))
          (and (string-match-p "previous.response" message)
               (string-match-p
                "not found\\|not cached\\|uncached\\|unknown\\|expired"
                message))))))

(defun e-openai-websocket--actual-metadata
    (metadata body-data connection-id reused reuse-count mode idle-close-seconds)
  "Return METADATA updated for the actual WebSocket BODY-DATA sent.
CONNECTION-ID identifies the socket, REUSED and REUSE-COUNT describe its
lifecycle, MODE describes the selected request shape, and IDLE-CLOSE-SECONDS
is the request-resolved local policy."
  (let* ((metadata (copy-tree metadata))
         (diagnostics (copy-sequence (plist-get metadata :diagnostics)))
         (previous-present
          (not (null (plist-member body-data :previous_response_id))))
         (continuation
          (if previous-present
              'used
            (if (eq (plist-get metadata :provider-continuation) 'disabled)
                'disabled
              'full))))
    (setq diagnostics
          (plist-put diagnostics :provider-continuation continuation))
    (setq diagnostics
          (plist-put diagnostics :previous-response-id-present
                     previous-present))
    (setq diagnostics
          (plist-put diagnostics :input-message-count
                     (length (plist-get body-data :input))))
    (setq diagnostics
          (plist-put diagnostics :websocket-connection-id connection-id))
    (setq diagnostics
          (plist-put diagnostics :websocket-reused (and reused t)))
    (setq diagnostics
          (plist-put diagnostics :websocket-reuse-count reuse-count))
    (setq diagnostics
          (plist-put diagnostics :websocket-request-mode mode))
    (setq diagnostics
          (plist-put diagnostics :websocket-idle-close-seconds
                     idle-close-seconds))
    (setq metadata (plist-put metadata :provider-continuation continuation))
    (setq metadata (plist-put metadata :diagnostics diagnostics))
    (unless previous-present
      (cl-remf metadata :provider-anchor-response-id)
      (cl-remf metadata :provider-continuation-delta-count)
      (cl-remf metadata :provider-anchor-source-message-count))
    metadata))

(cl-defun e-openai-websocket-request-start
    (&key session url headers body-data full-body-data request-metadata
          prompt-layout-revision reasoning-identity idle-close-seconds
          on-item on-complete on-error)
  "Send BODY-DATA as a Responses WebSocket request to URL with HEADERS.
SESSION owns a connection reusable by compatible sequential requests.
FULL-BODY-DATA is the safe request without provider continuation.
ON-ITEM receives backend-neutral stream items.  ON-COMPLETE receives a status
plist when a completed event arrives.  ON-ERROR receives an Emacs condition
list.  Return a cancellable `e-backend-request' handle."
  (let* ((session (or session
                      (e-openai-websocket-session-create)))
         (timeout e-openai-websocket-idle-timeout-seconds)
         (full-body-data (or full-body-data body-data))
         (properties
          ;; Snapshot all continuation properties before the first send.  A
          ;; caller may reuse and mutate its request tree while the response
          ;; is still in flight; the later latest-response admission must compare the
          ;; request that was actually started, not that mutable tree.
          (e-openai-websocket--json-value-copy
           (e-openai-websocket--request-properties full-body-data)))
         (requested-response-id (plist-get body-data :previous_response_id))
         (existing-websocket
          (e-openai-websocket--session-websocket session))
         (connection-compatible
          (and existing-websocket
               (equal url (e-openai-websocket--session-url session))
               (equal headers
                      (e-openai-websocket--session-headers session))))
         (response-known-p
          (and existing-websocket
               (stringp requested-response-id)
               (equal requested-response-id
                      (e-openai-websocket--session-latest-response-id
                       session))))
         (properties-compatible
          (and response-known-p
               (e-openai-websocket--json-value-equal-p
                properties
                (e-openai-websocket--session-latest-response-properties
                 session))))
         (incremental-p
          (and connection-compatible response-known-p properties-compatible))
         (actual-body-data (if incremental-p body-data full-body-data))
         (reused connection-compatible)
        timeout-timer
        settled
        retried-full
        completed-response-id
        request
        request-token
        assistant-message-candidate
        assistant-message-seen)
    (when (e-openai-websocket--session-active-request session)
      (signal 'e-openai-websocket-busy (list url)))
    (e-openai-websocket--cancel-idle-close session)
    (unless connection-compatible
      (e-openai-websocket--session-open session url headers)
      (setq reused nil))
    (when reused
      (cl-incf (e-openai-websocket--session-reuse-count session)))
    (setq request-token (list :websocket-request))
    (cl-labels
        ((cancel-timeout ()
           (when (timerp timeout-timer)
             (cancel-timer timeout-timer))
           (setq timeout-timer nil))
         (arm-timeout ()
           (cancel-timeout)
           (when (and timeout (not settled))
             (setq timeout-timer
                   (run-at-time
                    timeout nil
                    (lambda ()
                      (settle-error
                       (list 'e-openai-request-timeout
                             (format "OpenAI WebSocket idle timed out after %s seconds"
                                     timeout))))))))
         (active-request-p ()
           (eq request-token
               (plist-get
                (e-openai-websocket--session-active-request session)
                :token)))
         (clear-active-request ()
           (when (active-request-p)
             (setf (e-openai-websocket--session-active-request session)
                   nil)))
         (refresh-request-metadata (mode)
           (let ((actual
                  (e-openai-websocket--actual-metadata
                   request-metadata
                   actual-body-data
                   (e-openai-websocket--session-connection-id session)
                   reused
                   (e-openai-websocket--session-reuse-count session)
                   mode
                   idle-close-seconds)))
             (setf (e-backend-request-metadata request)
                   (append
                    (list :transport 'websocket
                          :url url
                          :timeout-seconds timeout
                          :cancellable t)
                    actual
                    (e-openai-diagnostics-url-metadata url)))))
         (settle-error (err)
           (unless settled
             (setq settled t)
             (cancel-timeout)
             (clear-active-request)
             (e-openai-websocket--session-close session)
             (when on-error
               (funcall on-error err))))
         (settle-complete (&optional response-id)
           (unless settled
             (setq settled t)
             (cancel-timeout)
             (clear-active-request)
             (when (stringp response-id)
               (e-openai-websocket--session-record-response
                session response-id properties))
             (e-openai-websocket--schedule-idle-close
              session idle-close-seconds)
             (when on-complete
               (funcall on-complete
                        (list :status 'done
                              :response-id response-id)))))
         (settle-backend-error (item)
           ;; `response.failed' is a terminal Responses event.  Preserve its
           ;; backend-neutral error item for the loop/harness while releasing
           ;; the transport first, so a retry cannot collide with this request.
           (unless settled
             (setq settled t)
             (cancel-timeout)
             (clear-active-request)
             (e-openai-websocket--session-close session)
             (when on-item
               (funcall on-item
                        (e-openai-diagnostics-normalize-backend-error-item item)))))
         (emit-item (item)
           (pcase (plist-get item :type)
             ('assistant-message
              (setq assistant-message-seen t)
              (when on-item
                (funcall on-item item)))
             ('assistant-message-candidate
              (unless assistant-message-candidate
                (setq assistant-message-candidate
                      (list :type 'assistant-message
                            :content (plist-get item :content)))))
             ('done
              (unless assistant-message-seen
                (when assistant-message-candidate
                  (setq assistant-message-seen t)
                  (when on-item
                    (funcall on-item assistant-message-candidate))))
              (when on-item
                (funcall on-item item)))
             ('backend-error
              (settle-backend-error item))
             (_
              (when on-item
                (funcall on-item item)))))
         (send-current-body ()
           (websocket-send-text
            (e-openai-websocket--session-websocket session)
            (e-json-serialize (append (list :type "response.create")
                                       actual-body-data))))
         (retry-full-request ()
           (setq retried-full t)
           ;; The provider rejected the only retained response identity.  Do
           ;; not preserve it while the complete canonical retry is in flight.
           (e-openai-websocket--session-clear-response-state session)
           (setq actual-body-data full-body-data)
           (setq completed-response-id nil)
           (setq assistant-message-candidate nil)
           (setq assistant-message-seen nil)
           (refresh-request-metadata 'full-retry)
           (arm-timeout)
           (condition-case err
               (send-current-body)
             (error (settle-error err))))
         (handle-message (_websocket frame)
           (unless settled
             (condition-case err
                 (let* ((text (e-openai-websocket--frame-text frame))
                        (event (e-openai-decoder-parse-json text))
                        (completed-event-p
                         (member (plist-get event :type)
                                 '("response.completed" "response.done")))
                        (incomplete-event-p
                         (equal (plist-get event :type)
                                "response.incomplete"))
                        (terminal-event-p
                         (or completed-event-p incomplete-event-p))
                        ;; WebSocket responses are valid connection-local
                        ;; anchors even with store=false.  If the connection is
                        ;; later lost, previous_response_not_found already
                        ;; triggers a complete replay.
                        (emit-anchor t))
                   (arm-timeout)
                   (if (and incremental-p
                            (not retried-full)
                            (e-openai-websocket--unresolved-response-p
                             event))
                       (retry-full-request)
                     (when completed-event-p
                       (setq completed-response-id
                             (plist-get (plist-get event :response) :id)))
                     (dolist (item (e-openai-decoder-event-items
                                    event
                                    emit-anchor
                                    prompt-layout-revision
                                    reasoning-identity))
                       (emit-item item))
                     (when terminal-event-p
                       ;; An incomplete response is a successful terminal
                       ;; lifecycle event, but its response id is not a
                       ;; confirmed connection-local continuation anchor.
                       (settle-complete
                        (unless incomplete-event-p completed-response-id)))))
               (error
                (settle-error err)))))
         (handle-close (&rest _args)
           (unless settled
             (settle-error '(error "Responses WebSocket closed before completion"))))
         (handle-error (&rest args)
           (settle-error (list 'error
                               (format "Responses WebSocket error: %s"
                                       (e-openai-diagnostics-bounded-string
                                       args))))))
      (setq request
            (e-backend-request-create
             :cancel
             (lambda ()
               (unless settled
                 (setq settled t))
               (cancel-timeout)
               (clear-active-request)
               (e-openai-websocket--session-close session)
               t)))
      (setf
       (e-openai-websocket--session-active-request session)
       (list :token request-token
             :on-message #'handle-message
             :on-close #'handle-close
             :on-error #'handle-error))
      (refresh-request-metadata (if incremental-p 'incremental 'full))
      (arm-timeout)
      (condition-case err
          (send-current-body)
        (error
         (if reused
             (condition-case retry-error
                 (progn
                   (e-openai-websocket--session-open session url headers)
                   (setq reused nil)
                   (setq actual-body-data full-body-data)
                   (refresh-request-metadata 'full-retry)
                   (send-current-body))
               (error (settle-error retry-error)))
           (settle-error err))))
      request)))



(provide 'e-openai-websocket)

;;; e-openai-websocket.el ends here
