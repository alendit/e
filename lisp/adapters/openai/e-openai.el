;;; e-openai.el --- OpenAI/Codex backend adapter for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public OpenAI facade and application-service composition root.  Provider
;; policy, protocol mapping, transport, decoding, and compaction are owned by
;; the adjacent modules; this file coordinates backend-neutral request flow.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-backend)
(require 'e-harness)
(require 'e-request)
(require 'e-tools)
(require 'e-work)
(require 'e-json)
(require 'e-openai-diagnostics)
(require 'e-openai-profile)
(require 'e-openai-responses)
(require 'e-openai-chat-completions)
(require 'e-openai-http)
(require 'e-openai-decoder)
(require 'e-openai-websocket)
(require 'e-openai-compaction)

(defun e-openai--request-metadata (wire-api options body)
  "Return sanitized request metadata for WIRE-API, OPTIONS, and BODY."
  (when (eq wire-api 'responses)
    (let* ((anchor (plist-get options :provider-anchor))
           (anchor-metadata (plist-get anchor :metadata))
           (response-id (plist-get anchor-metadata :response-id))
           (delta-messages (plist-get options :provider-anchor-delta-messages))
           (responses-transport
            (or (plist-get options :responses-transport) 'http))
           (continuation-state
            (cond
             ((not (plist-get options :provider-continuation)) 'disabled)
             ((plist-member body :previous_response_id) 'used)
             (t 'full)))
           (reasoning (plist-get body :reasoning))
           (diagnostics
            (list :model (plist-get body :model)
                  :reasoning-effort (plist-get reasoning :effort)
                  :reasoning-summary (plist-get reasoning :summary)
                  :response-store (e-openai-profile-response-store options)
                  :prompt-cache-key-present
                  (not (null (plist-member body :prompt_cache_key)))
                  :prompt-cache-retention-present
                  (not (null (plist-member body :prompt_cache_retention)))
                  :provider-continuation continuation-state
                  :previous-response-id-present
                  (not (null (plist-member body :previous_response_id)))
                  :provider-anchor-present
                  (and (plist-get options :provider-anchor) t)
                  :provider-compaction-selected
                  (plist-member options :provider-compaction-output)
                  :provider-compaction-source-entry-id
                  (plist-get options :provider-compaction-source-entry-id)
                  :input-message-count (length (plist-get body :input))
                  ;; The reserved curation carrier is an adapter wire
                  ;; detail, not a user-dispatchable tool.  Keep diagnostics
                  ;; compatible with the ordinary tool count.
                  :tool-count
                  (cl-count-if
                   (lambda (tool)
                     (not (member (plist-get tool :name)
                                  '("context-curate" context-curate))))
                   (plist-get body :tools))
                  :responses-transport responses-transport))
           (metadata (list :provider-continuation continuation-state
                           :responses-transport responses-transport
                           :reasoning-identity
                           (list :effort (plist-get reasoning :effort)
                                 :summary (plist-get reasoning :summary))
                           :diagnostics diagnostics)))
      (when (or (plist-member options :observation-delivery)
                (plist-member options :context-capabilities)
                (plist-member options :replaceable-current-state)
                (plist-member options :current-state-fingerprint))
        (setq diagnostics
              (append diagnostics
                      (list :observation-delivery
                            (or (plist-get options :observation-delivery)
                                (plist-get
                                 (plist-get options :context-capabilities)
                                 :observation-delivery))
                            :replaceable-current-state-present
                            (and (e-openai-responses-replaceable-current-state-present-p
                                  options)
                                 t)
                            :current-state-fingerprint
                            (plist-get options :current-state-fingerprint)
                            :context-rendering-strategy
                            (plist-get options :context-rendering-strategy)
                            :provider-anchor-safety
                            (plist-get options :provider-anchor-safety))))
        (setq metadata (plist-put metadata :diagnostics diagnostics)))
      (when-let ((revision (e-openai-responses-prompt-layout-revision options)))
        (setq diagnostics
              (append diagnostics
                      (list :prompt-cache-mode
                            (e-openai-responses-prompt-cache-mode-label options)
                            :prompt-layout-revision revision)))
        (setq metadata (plist-put metadata :diagnostics diagnostics))
        (setq metadata
              (append metadata
                      (list :openai-prompt-layout-revision revision))))
      (when (and (eq continuation-state 'used)
                 (stringp response-id))
        (setq metadata
              (append metadata
                      (list :provider-anchor-response-id response-id))))
      (when-let ((covered-entry-id (plist-get anchor :covered-entry-id)))
        (setq metadata
              (append metadata
                      (list :provider-anchor-covered-entry-id
                            covered-entry-id))))
      (when (listp delta-messages)
        (setq metadata
              (append metadata
                      (list :provider-continuation-delta-count
                            (length delta-messages)))))
      (when (integerp (plist-get options
                                 :provider-anchor-source-message-count))
        (setq metadata
              (append metadata
                      (list :provider-anchor-source-message-count
                            (plist-get
                             options
                             :provider-anchor-source-message-count)))))
      (when-let ((reason
                  (plist-get options :provider-anchor-invalidation-reason)))
        (setq metadata
              (append metadata
                      (list :provider-anchor-invalidation-reason reason))))
      metadata)))

(cl-defun e-openai--request-context
    (&key provider auth-file base-url model messages options)
  "Return adapter-local request context for PROVIDER request data.
AUTH-FILE, BASE-URL, MODEL, MESSAGES, and OPTIONS contribute to the encoded
OpenAI request and backend-neutral context."
  (e-openai-adapter-measure
   'openai.request-context
   (list :metadata (list :provider provider
                         :message-count (length messages)
                         :tool-count (length (plist-get options :tools))))
   (lambda ()
     (let* ((profile (e-openai-provider-profile provider))
            (wire-api (e-openai-provider-wire-api profile))
            (responses-transport (e-openai-profile-responses-transport profile))
            (websocket-idle-close-seconds
             (when (eq responses-transport 'websocket)
               (e-openai-profile-websocket-idle-close-seconds profile)))
            (effective-options (copy-sequence options))
            (_ (when (and (eq responses-transport 'websocket)
                          (not (eq wire-api 'responses)))
                 (signal 'e-openai-provider-invalid
                         '("WebSocket transport is only supported for Responses providers"))))
            (_ (unless (plist-get effective-options :model)
                 (setq effective-options
                       (plist-put effective-options
                                  :model
                                  (e-openai-provider-model profile model)))))
            (_ (when (eq wire-api 'responses)
                 (setq effective-options
                       (plist-put effective-options
                                  :responses-transport
                                  responses-transport))
                 (setq effective-options
                       (plist-put
                        effective-options
                        :prompt-cache-breakpoint-mode
                        (plist-get profile :prompt-cache-breakpoint-mode)))
                 (setq effective-options
                       (plist-put
                        effective-options
                        :responses-context-layout
                        (plist-get profile :responses-context-layout)))
                 ;; Responses always carries a summary choice.  Materialize
                 ;; the profile value here for direct adapter callers while
                 ;; preserving a caller-provided request override.
                 (unless (plist-member effective-options :reasoning-summary)
                   (setq effective-options
                         (plist-put
                          effective-options
                          :reasoning-summary
                          (e-openai-profile-reasoning-summary profile))))
                 ;; Direct adapter callers do not pass through the harness
                 ;; capability projection.  Materialize the profile's
                 ;; semantic delivery contract here so the input layout is
                 ;; stable with or without a prompt-cache key.  Preserve an
                 ;; explicit harness value when one is already present.
                 (unless (plist-member effective-options
                                        :observation-delivery)
                   (setq effective-options
                         (plist-put
                          effective-options
                          :observation-delivery
                          (e-openai-profile-observation-delivery profile))))
                 (setq effective-options
                       (plist-put
                        effective-options
                        :include-encrypted-reasoning
                        (plist-get profile :include-encrypted-reasoning)))
                 (if (plist-get effective-options :context-lifetime-enabled)
                     ;; The profile supplies the default carrier, while an
                     ;; explicit request-local nil from the loop closes a
                     ;; consumed frame opportunity without teaching core any
                     ;; provider wire syntax.
                     (unless (plist-member effective-options
                                            :reserved-effect-carrier)
                       (setq effective-options
                             (plist-put effective-options
                                        :reserved-effect-carrier
                                        'context-curate-wire)))
                   (cl-remf effective-options :reserved-effect-carrier))
                 (when (plist-member profile :response-store)
                   (setq effective-options
                         (plist-put effective-options
                                    :response-store
                                    (plist-get profile :response-store))))))
            (_ (when (and (eq wire-api 'responses)
                          (plist-member effective-options :prompt-cache-retention)
                          (not (e-openai-profile-prompt-cache-retention-supported-p
                                profile
                                (plist-get effective-options :model))))
                 (cl-remf effective-options :prompt-cache-retention)))
            (reasoning-identity
             (when (eq wire-api 'responses)
               (e-openai-responses-reasoning-identity effective-options)))
            (body-data
             (e-openai-adapter-measure
              'openai.request-body
              (list :metadata (list :provider provider
                                    :wire-api wire-api
                                    :message-count (length messages)
                                    :tool-count
                                    (length (plist-get effective-options :tools))))
              (lambda ()
                (pcase wire-api
                  ('responses
                   (e-openai-codex-request-body
                    :messages messages
                    :options effective-options
                    :tools (plist-get effective-options :tools)))
                  ('chat-completion
                   (e-openai-chat-completion-request-body
                    :messages messages
                    :options effective-options
                    :tools (plist-get effective-options :tools)))))))
            (full-body-data
             (when (and (eq wire-api 'responses)
                        (eq responses-transport 'websocket))
               (e-openai-codex-request-body
                :messages messages
                :options
                (e-openai-responses-options-without-provider-anchor effective-options)
                :tools (plist-get effective-options :tools))))
            (metadata (e-openai--request-metadata
                       wire-api effective-options body-data))
            (body
             (e-openai-adapter-measure
              'openai.request-json
              (list :metadata (list :provider provider
                                    :wire-api wire-api))
              (lambda ()
                (e-json-serialize body-data))))
            (session-id (plist-get effective-options :session-id))
            (url (pcase wire-api
                   ('responses
                    (let ((resolved-base-url
                           (or base-url
                               (e-openai-provider-base-url profile))))
                      (if (eq responses-transport 'websocket)
                          (e-openai-responses-websocket-url resolved-base-url)
                        (e-openai-responses-url resolved-base-url))))
                   ('chat-completion
                    (e-openai-chat-completion-url
                     (or base-url
                         (e-openai-provider-base-url profile))))))
            (headers
             (e-openai-adapter-measure
              'openai.request-headers
              (list :metadata (list :provider provider
                                    :wire-api wire-api
                                    :transport responses-transport))
              (lambda ()
                (e-openai-profile-headers
                 :profile profile
                 :auth-file auth-file
                 :session-id session-id))))
            (headers (if (and (eq wire-api 'responses)
                              (eq responses-transport 'websocket))
                         (e-openai-profile-responses-websocket-headers headers)
                       headers)))
       (list :provider provider
             :wire-api wire-api
             :responses-transport responses-transport
             :websocket-idle-close-seconds websocket-idle-close-seconds
             :prompt-layout-revision
             (plist-get metadata :openai-prompt-layout-revision)
             :reasoning-identity reasoning-identity
             :session-id session-id
             :url url
             :headers headers
             :metadata metadata
             :body-data body-data
             :full-body-data full-body-data
             :body body)))))

(defun e-openai--premature-stream-error-item (wire-api)
  "Return a retryable premature-stream error item for WIRE-API."
  (list :type 'backend-error
        :content
        (format "%s stream ended prematurely before a terminal event"
                (if (eq wire-api 'responses)
                    "Responses"
                  "Chat Completions"))
        :payload (list :response-kind 'sse :wire-api wire-api)))

(defun e-openai--http-response-error-details (response)
  "Return backend error details carried by structured HTTP RESPONSE."
  (when (e-openai-http-response-p response)
    (append
     (when-let ((status (e-openai-http-response-status response)))
       (list :status status))
     (when-let ((retry-after (e-openai-http-response-retry-after response)))
       (list :retry-after retry-after)))))

(defun e-openai--http-error-item (response items)
  "Return one backend error for HTTP RESPONSE, preserving parsed ITEMS."
  (let* ((body (e-openai-http-response-body-text response))
         (details (e-openai--http-response-error-details response))
         (parsed (seq-find (lambda (item)
                             (eq (plist-get item :type) 'backend-error))
                           items))
         (item
          (or (and parsed (copy-tree parsed))
              (let ((preview (e-openai-decoder-text-preview body)))
                (list
                 :type 'backend-error
                 :content
                 (if (string-empty-p preview)
                     (format "OpenAI HTTP request failed with status %s"
                             (e-openai-http-response-status response))
                   (format "OpenAI HTTP request failed with status %s: %s"
                           (e-openai-http-response-status response)
                           preview)))))))
    (plist-put item :payload
               (append details (copy-tree (plist-get item :payload))))
    item))

(defun e-openai--terminal-response-item-p (item)
  "Return non-nil when ITEM settles a complete provider response."
  (memq (plist-get item :type) '(done backend-error)))

(defun e-openai--complete-response-items (response context)
  "Parse complete HTTP RESPONSE for CONTEXT and validate stream settlement."
  (let* ((wire-api (plist-get context :wire-api))
         (body (e-openai-http-response-body-text response))
         items
         parse-error)
    (condition-case err
        (setq items
              (pcase wire-api
                ('responses
                 (e-openai-codex-parse-stream
                  body
                  (plist-get context :prompt-layout-revision)
                  (plist-get context :reasoning-identity)))
                ('chat-completion
                 (e-openai-chat-completion-parse-stream body))))
      (e-json-error (setq parse-error err)))
    (cond
     ((e-openai-http-error-p response)
      (list (e-openai--http-error-item response items)))
     ((and parse-error
           (and (eq (car parse-error) 'e-json-error)
                (string-match-p "end of file"
                                (error-message-string parse-error)))
           (e-openai-decoder-sse-response-p body))
      (list (e-openai--premature-stream-error-item wire-api)))
     (parse-error
      (signal (car parse-error) (cdr parse-error)))
     ((seq-some #'e-openai--terminal-response-item-p items)
      items)
     ((string-empty-p (string-trim (or body "")))
      (list (e-openai--premature-stream-error-item wire-api)))
     ((e-openai-decoder-sse-response-p body)
      (list (e-openai--premature-stream-error-item wire-api)))
     (t
      (list (e-openai-decoder-non-stream-error-item body wire-api))))))

(defun e-openai--emit-response-items (response context on-item)
  "Parse complete RESPONSE for CONTEXT and emit items through ON-ITEM."
  (dolist (item (e-openai--complete-response-items response context))
    (funcall on-item (e-openai-diagnostics-normalize-backend-error-item item))))

(cl-defun e-openai-backend-create
    (&key provider auth-file base-url request-function
          compaction-request-function name model)
  "Create an OpenAI-like backend named NAME.
PROVIDER selects a profile from `e-openai-model-providers'.  AUTH-FILE is used
for Codex-managed OpenAI auth profiles.  BASE-URL overrides the profile base
URL.  REQUEST-FUNCTION is injectable for tests.  MODEL is the backend-local
default when turn options do not include `:model'.  The provider profile's
`:wire-api' chooses the Responses or Chat Completions request/stream mapping."
  (let* ((provider (or provider e-openai-default-provider))
         (profile (e-openai-provider-profile provider))
         (provider-compaction-supported
          (e-openai-compaction-eligible-p
           provider profile base-url request-function
           compaction-request-function))
         (provider-compaction
          (when provider-compaction-supported
            (cl-function
             (lambda (&key messages options on-done on-error)
               (e-openai-compaction-run
                profile auth-file base-url model compaction-request-function
                :messages messages
                :options options
                :on-done on-done
                :on-error on-error)))))
         (websocket-sessions (make-hash-table :test 'equal)))
    (cl-labels
        ((request-metadata (context transport cancellable)
           (append
            (list :provider (plist-get context :provider)
                  :wire-api (plist-get context :wire-api)
                  :url (plist-get context :url)
                  :cancellable cancellable
                  :transport transport)
            (plist-get context :metadata)
            (e-openai-diagnostics-url-metadata
             (plist-get context :url))))
         (websocket-session (context)
           (let ((key (or (plist-get context :session-id)
                          :backend-default)))
             (or (gethash key websocket-sessions)
                 (puthash
                  key
                  (e-openai-websocket-session-create)
                  websocket-sessions))))
         (websocket-request-metadata (context)
           (append
            (list :provider (plist-get context :provider)
                  :wire-api (plist-get context :wire-api))
            (plist-get context :metadata))))
      (e-backend-create
       :name (or name (e-openai-provider-name provider))
       :normalize-error-details #'e-openai-diagnostics-normalize-error-details
       :context-capabilities
       (lambda (options)
         (e-openai-profile-context-capabilities
          (e-openai-provider-profile provider)
          options
          :provider provider
          :base-url base-url
          :request-function request-function
          :provider-compaction-supported
          (and provider-compaction-supported
               (functionp provider-compaction))))
       :provider-compaction provider-compaction
       :stream
       (cl-function
        (lambda (&key messages options on-item)
          (e-openai-provider-reject-sync-in-hot-path 'e-openai-backend-stream)
          (let* ((context (e-openai--request-context
                           :provider provider
                           :auth-file auth-file
                           :base-url base-url
                           :model model
                           :messages messages
                           :options options))
                 (websocket-p (and (not request-function)
                                   (eq (plist-get context :responses-transport)
                                       'websocket))))
            (if websocket-p
                (let (done failure)
                  (e-backend-note-request-started
                   (e-openai-websocket-request-start
                    :session (websocket-session context)
                    :url (plist-get context :url)
                    :headers (plist-get context :headers)
                    :body-data (plist-get context :body-data)
                    :full-body-data (plist-get context :full-body-data)
                    :request-metadata (websocket-request-metadata context)
                    :prompt-layout-revision
                    (plist-get context :prompt-layout-revision)
                    :reasoning-identity
                    (plist-get context :reasoning-identity)
                    :idle-close-seconds
                    (plist-get context :websocket-idle-close-seconds)
                    :on-item on-item
                    :on-complete (lambda (_status)
                                   (setq done t))
                    :on-error (lambda (err)
                                (setq failure err)
                                (setq done t))))
                  (while (not done)
                    (accept-process-output nil 0.01))
                  (when failure
                    (signal (car failure) (cdr failure))))
              (let* ((requester (or request-function
                                    #'e-openai-http-request))
                     (response nil))
                (e-backend-note-request-started
                 (e-backend-request-create
                  :metadata
                  (request-metadata context 'sync-wrapper nil)))
                (setq response
                      (funcall requester
                               :url (plist-get context :url)
                               :headers (plist-get context :headers)
                               :body (plist-get context :body)))
                (e-openai--emit-response-items response context on-item))))))
       :start
       (cl-function
        (lambda (&key messages options on-item on-done on-error
                      on-request-start)
          (let* ((context (e-openai--request-context
                           :provider provider
                           :auth-file auth-file
                           :base-url base-url
                           :model model
                           :messages messages
                           :options options)))
            (cond
             (request-function
              (let ((cancelled nil)
                    (timer nil)
                    request)
                (setq request
                      (e-backend-request-create
                       :cancel (lambda ()
                                 (setq cancelled t)
                                 (when (timerp timer)
                                   (cancel-timer timer))
                                 t)
                       :metadata
                       (request-metadata
                        context
                        'injected-request-function
                        'queued-only)))
                (when on-request-start
                  (funcall on-request-start request))
                (setq timer
                      (run-at-time
                       0 nil
                       (lambda ()
                         (unless cancelled
                           (condition-case err
                               (let ((response
                                      (funcall
                                       request-function
                                       :url (plist-get context :url)
                                       :headers (plist-get context :headers)
                                       :body (plist-get context :body))))
                                 (e-openai--emit-response-items
                                  response context on-item)
                                 (when on-done
                                   (funcall on-done '(:status done))))
                             (error
                              (when on-error
                                (funcall on-error err))))))))
                request))
             ((eq (plist-get context :responses-transport) 'websocket)
              (let ((request
                     (e-openai-websocket-request-start
                      :session (websocket-session context)
                      :url (plist-get context :url)
                      :headers (plist-get context :headers)
                      :body-data (plist-get context :body-data)
                      :full-body-data (plist-get context :full-body-data)
                      :request-metadata (websocket-request-metadata context)
                      :prompt-layout-revision
                      (plist-get context :prompt-layout-revision)
                      :reasoning-identity
                      (plist-get context :reasoning-identity)
                      :idle-close-seconds
                      (plist-get context :websocket-idle-close-seconds)
                      :on-item on-item
                      :on-complete on-done
                      :on-error on-error)))
                (when on-request-start
                  (funcall on-request-start request))
                request))
             (t
              (let ((request
                     (e-openai-http-request-start
                      :url (plist-get context :url)
                      :headers (plist-get context :headers)
                      :body (plist-get context :body)
                      :on-complete
                      (lambda (response)
                        (condition-case err
                            (progn
                              (e-openai--emit-response-items response context on-item)
                              (when on-done
                                (funcall on-done '(:status done))))
                          (error
                           (when on-error
                             (funcall on-error err)))))
                      :on-error on-error)))
                (setf (e-backend-request-metadata request)
                      (append
                       (list :provider (plist-get context :provider)
                             :wire-api (plist-get context :wire-api))
                       (plist-get context :metadata)
                       (e-backend-request-metadata request)))
                (when on-request-start
                  (funcall on-request-start request))
                request))))))))))

(cl-defun e-openai-create-harness
    (&key provider auth-file base-url request-function
          compaction-request-function model sessions)
  "Create a harness configured for an OpenAI-like provider.
PROVIDER selects `e-openai-default-provider' when nil.  AUTH-FILE, BASE-URL,
and REQUEST-FUNCTION configure the backend adapter.  MODEL is written into
backend-neutral turn options by the default context strategy used by harness
turn paths.  SESSIONS supplies an existing session store."
  (let* ((provider (or provider e-openai-default-provider))
         (profile (e-openai-provider-profile provider))
         (model (e-openai-provider-model profile model)))
    (e-harness-create
     :backend (e-openai-backend-create
               :provider provider
               :auth-file auth-file
               :base-url base-url
               :request-function request-function
               :compaction-request-function compaction-request-function
               :model model)
     :default-options (e-openai-profile-harness-default-options profile model)
     :sessions sessions)))

(cl-defun e-openai-codex-backend-create
    (&key auth-file base-url request-function name model)
  "Create an OpenAI/Codex backend named NAME.
AUTH-FILE points at Codex-managed auth.  BASE-URL defaults to ChatGPT's Codex
backend.  REQUEST-FUNCTION is injectable for tests."
  (e-openai-backend-create
   :provider 'codex
   :auth-file auth-file
   :base-url (when base-url (e-openai-codex-url base-url))
   :request-function request-function
   :name (or name "openai-codex")
   :model model))

(cl-defun e-openai-codex-create-harness
    (&key auth-file base-url request-function model)
  "Create a harness configured for ChatGPT-backed Codex.
AUTH-FILE, BASE-URL, and REQUEST-FUNCTION configure the backend adapter.  MODEL
is written into backend-neutral turn options by the default harness context
strategy."
  (e-openai-create-harness
   :provider 'codex
   :auth-file auth-file
   :base-url (when base-url (e-openai-codex-url base-url))
   :request-function request-function
   :model model))


(provide 'e-openai)

;;; e-openai.el ends here
