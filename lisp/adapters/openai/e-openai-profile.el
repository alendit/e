;;; e-openai-profile.el --- OpenAI profile and auth policy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns provider profile data, authentication policy, request identity
;; capabilities, and provider-specific defaults.  Diagnostic formatting lives
;; in e-openai-diagnostics; wire and transport state live below this owner.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-context-lifetime)
(require 'e-json)
(require 'e-openai-diagnostics)

(defconst e-openai-codex-default-base-url
  "https://chatgpt.com/backend-api"
  "Default base URL for ChatGPT-backed Codex Responses access.")

(defconst e-openai-api-default-base-url
  "https://api.openai.com/v1"
  "Default base URL for first-party OpenAI API access.")

(defconst e-openai-codex-account-claim
  "https://api.openai.com/auth"
  "JWT claim namespace containing the ChatGPT account id.")

(defgroup e-openai nil
  "OpenAI/Codex backend adapter for e."
  :group 'e
  :prefix "e-openai-")

(defvaralias 'e-openai-codex-default-model 'e-openai-default-model)

(defcustom e-openai-default-model "gpt-6-sol"
  "Default model for OpenAI-like Responses requests."
  :type 'string
  :group 'e-openai)

(defcustom e-openai-default-reasoning-effort "high"
  "Default reasoning effort for OpenAI-like Responses requests."
  :type '(choice (const :tag "Unset" nil)
                 (string :tag "Effort"))
  :group 'e-openai)

(defcustom e-openai-default-reasoning-summary "auto"
  "Default Responses reasoning summary mode.

Responses requests always include this field.  `auto' is the provider's
ordinary summary mode; `detailed' is an explicit diagnostic choice."
  :type '(choice (const "auto") (const "detailed"))
  :group 'e-openai)

(defun e-openai-profile--validate-reasoning-summary (value)
  "Return valid Responses reasoning summary VALUE, or signal an error."
  (unless (and (stringp value)
               (member value '("auto" "detailed")))
    (signal 'e-openai-provider-invalid
            (list (format "Invalid Responses reasoning summary %S" value))))
  value)

(defun e-openai-profile--validate-reasoning-effort (value)
  "Return non-empty reasoning effort VALUE, or signal an error."
  (unless (and (stringp value)
               (not (string-empty-p value)))
    (signal 'e-openai-provider-invalid
            (list (format "Invalid Responses reasoning effort %S" value))))
  value)

(defun e-openai-profile--keyword-plist-p (value)
  "Return non-nil when VALUE is a proper plist with keyword keys."
  (let ((tail value)
        (valid t))
    (while (and valid tail)
      (if (and (consp tail)
               (keywordp (car tail))
               (consp (cdr tail)))
          (setq tail (cddr tail))
        (setq valid nil)))
    (and valid (null tail))))

(defun e-openai-profile-effective-reasoning (options)
  "Return validated Responses reasoning for OPTIONS.
An explicit `:reasoning' plist supplies the highest-precedence fields; absent
effort and summary fields are filled from the corresponding effective options.
The returned plist is detached from OPTIONS and always contains both required
fields."
  (let* ((explicit-p (plist-member options :reasoning))
         (explicit (and explicit-p (plist-get options :reasoning)))
         (option-summary
          (if (plist-member options :reasoning-summary)
              (plist-get options :reasoning-summary)
            e-openai-default-reasoning-summary)))
    (when explicit-p
      (unless (e-openai-profile--keyword-plist-p explicit)
        (signal 'e-openai-provider-invalid
                (list (format "Invalid Responses reasoning %S" explicit))))
      (when (null explicit)
        (signal 'e-openai-provider-invalid
              '("Responses :reasoning cannot be nil"))))
    (let* ((reasoning (copy-tree (or explicit nil)))
           (effort
            (if (and explicit-p (plist-member explicit :effort))
                (plist-get explicit :effort)
              (if (plist-member options :reasoning-effort)
                  (plist-get options :reasoning-effort)
                e-openai-default-reasoning-effort)))
           (summary
            (if (and explicit-p (plist-member explicit :summary))
                (plist-get explicit :summary)
              option-summary)))
      (setq effort (e-openai-profile--validate-reasoning-effort effort)
            summary (e-openai-profile--validate-reasoning-summary summary))
      ;; Keep the required fields in a stable order while retaining any
      ;; provider-specific fields carried by an explicit reasoning plist.
      (cl-remf reasoning :effort)
      (cl-remf reasoning :summary)
      (append (list :effort effort) reasoning (list :summary summary)))))

(defcustom e-openai-default-text-verbosity "low"
  "Default GPT-5 Responses text verbosity for OpenAI-like requests."
  :type '(choice (const :tag "Unset" nil)
                 (string :tag "Verbosity"))
  :group 'e-openai)

(defcustom e-openai-default-provider 'codex
  "Default provider profile used by generic OpenAI harness helpers."
  :type 'symbol
  :group 'e-openai)

(defcustom e-openai-request-timeout-seconds nil
  "Optional idle timeout for OpenAI-like HTTP requests, in seconds.

The default is nil because an HTTP provider or gateway may buffer the response
while a long reasoning request is still healthy.  A local deadline cannot
distinguish that state from a stalled request.  Set a number only when the
selected HTTP provider guarantees response progress within that interval."
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds"))
  :group 'e-openai)

(defcustom e-openai-websocket-idle-timeout-seconds 60
  "Seconds without Responses WebSocket events before a request fails.
When nil, Responses WebSocket requests do not time out locally."
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds"))
  :group 'e-openai)

(defcustom e-openai-websocket-connection-idle-seconds 300
  "Seconds to retain an idle Responses WebSocket connection.
The connection stays open across compatible model/tool round trips so
`previous_response_id' can use the provider's connection-local cache.  Set
this to nil to retain idle connections until cancellation, failure, or backend
replacement."
  :type '(choice (const :tag "Retain until teardown" nil)
                 (number :tag "Seconds"))
  :group 'e-openai)

(defun e-openai-profile--custom-override-p (symbol)
  "Return non-nil when SYMBOL has a Custom override."
  (or (get symbol 'saved-value)
      (get symbol 'customized-value)
      (get symbol 'theme-value)))

(defun e-openai-profile--migrate-websocket-idle-timeout-default ()
  "Adopt the current WebSocket idle timeout across live reloads.
Preserve real Custom/theme overrides while migrating an old uncustomized
default."
  (when (and (memq e-openai-websocket-idle-timeout-seconds '(nil 180))
             (not (e-openai-profile--custom-override-p
                   'e-openai-websocket-idle-timeout-seconds)))
    (setq e-openai-websocket-idle-timeout-seconds 60)))

(defun e-openai-profile--migrate-http-timeout-default ()
  "Disable the old implicit HTTP timeout across live reloads.
Preserve a real Custom/theme override while migrating the old default."
  (when (and (equal e-openai-request-timeout-seconds 180)
             (not (e-openai-profile--custom-override-p
                   'e-openai-request-timeout-seconds)))
    (setq e-openai-request-timeout-seconds nil)))

(e-openai-profile--migrate-http-timeout-default)
(e-openai-profile--migrate-websocket-idle-timeout-default)

(defun e-openai-profile-builtin-codex-p (provider-id profile)
  "Return non-nil when PROFILE describes ChatGPT's built-in Codex endpoint.
This stable identity predicate is used by compatibility scenarios and profile
capability selection; it does not expose mutable profile state."
  (and (eq provider-id 'codex)
       (equal (plist-get profile :name) "ChatGPT Codex")
       (equal (plist-get profile :base-url)
              (concat e-openai-codex-default-base-url "/codex"))
       (eq (plist-get profile :wire-api) 'responses)
       (eq (plist-get profile :responses-transport) 'websocket)
       (plist-get profile :requires-openai-auth)))

(defconst e-openai-profile--request-local-observation-delivery-map
  '((:kind current-state :mode request-local-replaceable)
    (:kind dynamic-context :mode request-local-replaceable)
    (:kind tool-result :mode inherited)
    (:kind trace :mode inherited)
    (:kind retrieved-excerpt :mode inherited))
  "Kind-scoped delivery map for statically proven identity-compatible
Responses profiles.")

(defun e-openai-profile--builtin-openai-profile ()
  "Return the canonical first-party OpenAI API provider profile."
  (list :name "OpenAI API"
        :base-url e-openai-api-default-base-url
        :wire-api 'responses
        :responses-transport 'websocket
        :response-store :json-false
        :responses-context-layout 'developer-input
        :prompt-cache-breakpoint-mode 'explicit
        ;; The canonical layout keeps the changing observation frontier in
        ;; late developer-role input.  It is therefore inherited by a
        ;; continuation; only profiles that deliberately retain the complete
        ;; observation in top-level `instructions' may opt into the older
        ;; request-local replacement contract.
        :observation-delivery 'inherited
        :include-encrypted-reasoning t
        :continuation t
        :requires-openai-auth nil
        :env-key "OPENAI_API_KEY"
        :default-model "gpt-5.6"))

(defun e-openai-profile-websocket-idle-close-seconds (profile)
  "Return the effective WebSocket idle-close policy for PROFILE.
An explicitly declared profile value must be a non-negative number.  Profiles
without the adapter-private field retain the existing global fallback,
including its nil no-timer behavior."
  (if (plist-member profile :websocket-idle-close-seconds)
      (let ((value (plist-get profile :websocket-idle-close-seconds)))
        (unless (and (numberp value) (>= value 0))
          (signal 'e-openai-provider-invalid
                  (list
                   (format
                    "Invalid :websocket-idle-close-seconds %S"
                    value))))
        value)
    e-openai-websocket-connection-idle-seconds))

(defun e-openai-profile--normalize-model-providers (providers)
  "Return PROVIDERS with current built-in OpenAI requirements applied."
  (let ((normalized
         (mapcar (lambda (entry)
                   (let ((provider-id (car entry))
                         (profile (cdr entry)))
                     (if (e-openai-profile-builtin-codex-p
                          provider-id
                          profile)
                         (let ((normalized
                                (plist-put
                                 (plist-put
                                  (plist-put
                                   (plist-put (copy-sequence profile)
                                              :response-store :json-false)
                                   :prompt-cache-breakpoint-mode nil)
                                  :responses-context-layout 'developer-input)
                                 :include-encrypted-reasoning t)))
                           (unless (plist-member normalized
                                                 :observation-delivery)
                             (setq normalized
                                   (plist-put
                                    normalized
                                    :observation-delivery
                                    'inherited)))
                           (cons provider-id normalized))
                       entry)))
                 providers)))
    (if (assq 'openai normalized)
        normalized
      (append normalized
              (list (cons 'openai (e-openai-profile--builtin-openai-profile)))))))

(defcustom e-openai-model-providers
  `((codex
     :name "ChatGPT Codex"
     :base-url ,(concat e-openai-codex-default-base-url "/codex")
     :wire-api responses
     :responses-transport websocket
     :response-store :json-false
     :prompt-cache-breakpoint-mode nil
     :responses-context-layout developer-input
     :observation-delivery inherited
     :include-encrypted-reasoning t
     ;; Detached SQLite context assembly cannot yet query provider anchors.
     :continuation nil
     :requires-openai-auth t)
    (openai
     :name "OpenAI API"
     :base-url ,e-openai-api-default-base-url
     :wire-api responses
     :responses-transport websocket
     :response-store :json-false
     :responses-context-layout developer-input
     :prompt-cache-breakpoint-mode explicit
     :observation-delivery inherited
     :include-encrypted-reasoning t
     :continuation t
     :requires-openai-auth nil
     :env-key "OPENAI_API_KEY"
     :default-model "gpt-5.6"))
  "OpenAI-like model provider profiles keyed by provider symbol.

Each profile is plist data.  `:wire-api' supports `responses' and
`chat-completion' (`chat-completions' is accepted as a compatibility alias).
Profiles with `:requires-openai-auth' non-nil use Codex-managed ChatGPT auth.
Profiles with `:requires-openai-auth' nil read a bearer token from `:env-key'.
Profiles can set `:prompt-cache-retention' to nil to suppress that request
field, or non-nil to force it on.  Responses profiles can set
`:responses-transport' to `http' or `websocket'.  WebSocket Responses requests
store responses by default; set `:response-store' to explicitly override that
value.  Responses profiles can opt into provider continuation anchors with
`:continuation' non-nil.  GPT-5.6 Responses profiles set
`:prompt-cache-breakpoint-mode' to `explicit' when they accept explicit
breakpoints and `prompt_cache_options', or leave it nil when unsupported.
Profiles can independently set `:responses-context-layout' to
`developer-input' when system segments should remain distinct even without an
explicit breakpoint.  Set `:include-encrypted-reasoning' when a provider
supports returning stateless reasoning items for complete replay.  A Responses
profile may set `:reasoning-summary' to the diagnostic value \"detailed\";
when absent, the adapter sends \"auto\".  The request-level option can
override the profile, but the field cannot be disabled.  A Responses
profile may set `:observation-delivery' to `request-local-replaceable' only
when that profile and transport have proven that top-level `instructions' are
not inherited by its continuation response; otherwise the conservative
default is `inherited'.  Responses WebSocket profiles may set the adapter-
private `:websocket-idle-close-seconds' to a non-negative number; when absent,
the existing `e-openai-websocket-connection-idle-seconds' fallback applies."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'e-openai)

(setq e-openai-model-providers
      (e-openai-profile--normalize-model-providers e-openai-model-providers))

(defun e-openai-provider-profile (&optional provider)
  "Return configured profile for PROVIDER.
When PROVIDER is nil, use `e-openai-default-provider'."
  (setq e-openai-model-providers
        (e-openai-profile--normalize-model-providers e-openai-model-providers))
  (let* ((provider (or provider e-openai-default-provider))
         (entry (assq provider e-openai-model-providers)))
    (unless entry
      (signal 'e-openai-provider-invalid
              (list (format "Unknown provider profile: %S" provider))))
    (let ((profile (cdr entry)))
      (unless (and (listp profile)
                   (memq (plist-get profile :wire-api)
                         '(responses chat-completion chat-completions)))
        (signal 'e-openai-provider-invalid
                (list (format "Provider %S must use :wire-api responses or chat-completion"
                              provider))))
      profile)))

(defun e-openai-provider-name (&optional provider)
  "Return display name for PROVIDER."
  (or (plist-get (e-openai-provider-profile provider) :name)
      (symbol-name (or provider e-openai-default-provider))))

(defun e-openai-provider-base-url (profile)
  "Return PROFILE's base URL or signal a provider configuration error."
  (let ((base-url (plist-get profile :base-url)))
    (unless (and (stringp base-url) (not (string-empty-p base-url)))
      (signal 'e-openai-provider-invalid
              '("Provider profile is missing :base-url")))
    base-url))

(defun e-openai-provider-model (profile explicit-model)
  "Return model for PROFILE, preferring EXPLICIT-MODEL."
  (or explicit-model
      (plist-get profile :default-model)
      e-openai-default-model))

(defun e-openai-provider-wire-api (profile)
  "Return PROFILE's normalized wire API."
  (pcase (plist-get profile :wire-api)
    ((or 'nil 'responses) 'responses)
    ((or 'chat-completion 'chat-completions) 'chat-completion)
    (other (signal 'e-openai-provider-invalid
                   (list (format "Unsupported :wire-api %S" other))))))

(defun e-openai-profile-reasoning-summary (profile)
  "Return the validated Responses summary mode declared by PROFILE."
  (e-openai-profile--validate-reasoning-summary
   (if (plist-member profile :reasoning-summary)
       (plist-get profile :reasoning-summary)
     e-openai-default-reasoning-summary)))

(defun e-openai-profile-responses-transport (profile)
  "Return normalized Responses transport for PROFILE."
  (let ((transport (or (plist-get profile :responses-transport) 'http)))
    (unless (memq transport '(http websocket))
      (signal 'e-openai-provider-invalid
              (list (format "Unsupported :responses-transport %S"
                            transport))))
    transport))

(defun e-openai-profile-response-store (options)
  "Return Responses store value for OPTIONS."
  (cond
   ((plist-member options :response-store)
    (plist-get options :response-store))
   ((eq (plist-get options :responses-transport) 'websocket)
    t)
   ((plist-get options :provider-continuation)
    t)
   (t :json-false)))

(defun e-openai-profile-implicit-websocket-store-p (options)
  "Return non-nil when OPTIONS use WebSocket's stored-response default."
  (and (eq (plist-get options :responses-transport) 'websocket)
       (not (eq (e-openai-profile-response-store options) :json-false))))

(defun e-openai-profile-websocket-request-p (options)
  "Return non-nil when OPTIONS describe a Responses WebSocket request."
  (eq (plist-get options :responses-transport) 'websocket))

(defun e-openai-profile-model-gpt56-or-later-p (model)
  "Return non-nil when MODEL names GPT-5.6 or a later numbered GPT model."
  (and (stringp model)
       (string-match "\\`gpt-\\([0-9]+\\)\\.\\([0-9]+\\)" model)
       (let ((major (string-to-number (match-string 1 model)))
             (minor (string-to-number (match-string 2 model))))
         (or (> major 5)
             (and (= major 5) (>= minor 6))))))

(defconst e-openai-gpt56-explicit-cache-layout-revision
  "responses-explicit-cache-v1"
  "Anchor revision for the GPT-5.6 explicit prompt-cache input layout.")

(defconst e-openai-gpt56-segmented-context-layout-revision
  "responses-segmented-context-v1"
  "Anchor revision for GPT-5.6 segmented input without explicit caching.")

(defun e-openai-profile--prompt-cache-breakpoint-mode (options)
  "Return the provider-supported GPT-5.6 breakpoint mode from OPTIONS.
Direct request-renderer callers default to `explicit' for compatibility; real
provider requests always materialize this option from the provider profile."
  (if (plist-member options :prompt-cache-breakpoint-mode)
      (plist-get options :prompt-cache-breakpoint-mode)
    'explicit))

(defun e-openai-profile--stable-cache-segment-p (segment)
  "Return non-nil when SEGMENT belongs to the stable prompt prefix."
  (memq (plist-get segment :kind) '(static-prefix stable-context)))

(defun e-openai-profile--stable-system-message-count (options)
  "Return the number of stable system messages described by OPTIONS."
  (cl-loop for segment in (plist-get options :segments)
           when (e-openai-profile--stable-cache-segment-p segment)
           sum (cl-count-if
                (lambda (message)
                  (eq (plist-get message :role) 'system))
                (plist-get segment :messages))))

(defun e-openai-profile--segmented-prompt-layout-p (options)
  "Return non-nil when OPTIONS use the semantic segmented input layout.

This is a wire-layout decision, not a cache-breakpoint decision.  The
developer-input profile declaration keeps stable and changing system messages
in input even when no prompt-cache key is available when the profile declares
the inherited delivery contract.  An explicit GPT-5.6 breakpoint declaration
with a key retains the direct renderer's segmented shape for legacy/direct
callers."
  (or (and (eq (e-openai-profile-observation-delivery-for-options options)
              'inherited)
           (eq (plist-get options :responses-context-layout)
               'developer-input))
      (and (e-openai-profile-model-gpt56-or-later-p (plist-get options :model))
           (eq (e-openai-profile--prompt-cache-breakpoint-mode options)
               'explicit)
           (let ((key (plist-get options :prompt-cache-key)))
             (and (stringp key)
                  (not (string-empty-p key)))))))

(defun e-openai-profile--wire-prompt-layout-revision (options)
  "Return the provider wire-layout revision for OPTIONS, or nil.

This is deliberately separate from the material continuation identity below:
the curation revision must fence anchors even when no prompt-cache key exists,
while breakpoint emission remains key-dependent."
  (let ((key (plist-get options :prompt-cache-key))
        (mode (e-openai-profile--prompt-cache-breakpoint-mode options)))
    (when (and (e-openai-profile-model-gpt56-or-later-p
                (plist-get options :model))
               (e-openai-profile--segmented-prompt-layout-p options)
               (stringp key)
               (not (string-empty-p key))
               (> (e-openai-profile--stable-system-message-count options) 0))
      (if (eq mode 'explicit)
          e-openai-gpt56-explicit-cache-layout-revision
        e-openai-gpt56-segmented-context-layout-revision))))

(defun e-openai-profile--prompt-layout-revision (options)
  "Return the material prompt-layout identity for OPTIONS, or nil.

The reserved curation carrier adds the complete provider-neutral curation
revision identity.  Per-frame labels, estimates, values, and provenance are
not options and therefore cannot enter this identity."
  (let ((wire-revision
         (e-openai-profile--wire-prompt-layout-revision options)))
    (if (eq (plist-get options :reserved-effect-carrier)
            'context-curate-wire)
        (list :prompt-layout-revision wire-revision
              :context-curation-revision-identity
              (e-context-lifetime-curation-revision-identity))
      wire-revision)))

(defun e-openai-profile--prompt-cache-mode-label (options)
  "Return the diagnostic cache mode label for segmented OPTIONS."
  (if (eq (e-openai-profile--prompt-cache-breakpoint-mode options) 'explicit)
      "explicit"
    "implicit-segmented"))

(defun e-openai-profile-prompt-cache-breakpoint-mode (options)
  "Return the provider-supported prompt-cache breakpoint mode for OPTIONS."
  (e-openai-profile--prompt-cache-breakpoint-mode options))

(defun e-openai-profile-stable-system-message-count (options)
  "Return the number of stable system messages in OPTIONS."
  (e-openai-profile--stable-system-message-count options))

(defun e-openai-profile-segmented-prompt-layout-p (options)
  "Return non-nil when OPTIONS use the semantic segmented input layout."
  (e-openai-profile--segmented-prompt-layout-p options))

(defun e-openai-profile-wire-prompt-layout-revision (options)
  "Return the provider wire-layout revision for OPTIONS, or nil."
  (e-openai-profile--wire-prompt-layout-revision options))

(defun e-openai-profile-prompt-layout-revision (options)
  "Return the material prompt-layout identity for OPTIONS, or nil."
  (e-openai-profile--prompt-layout-revision options))

(defun e-openai-profile-reasoning-identity (options)
  "Return the effective Responses reasoning identity for OPTIONS."
  (let ((reasoning (e-openai-profile-effective-reasoning options)))
    (list :effort (plist-get reasoning :effort)
          :summary (plist-get reasoning :summary))))

(defun e-openai-profile-prompt-cache-mode-label (options)
  "Return the diagnostic cache mode label for OPTIONS."
  (e-openai-profile--prompt-cache-mode-label options))

(defun e-openai-profile-prompt-cache-retention-supported-p (profile model)
  "Return non-nil when PROFILE and MODEL accept `prompt_cache_retention'.
GPT-5.6 and later use `prompt_cache_options.ttl' instead; their only current
TTL is the default, so the legacy retention option must not reach the wire."
  (and (not (e-openai-profile-model-gpt56-or-later-p model))
       (if (plist-member profile :prompt-cache-retention)
           (plist-get profile :prompt-cache-retention)
         (not (plist-get profile :requires-openai-auth)))))

(defun e-openai-profile--continuation-supported-p (profile)
  "Return non-nil when PROFILE should use Responses continuation anchors."
  (and (eq (e-openai-provider-wire-api profile) 'responses)
       (plist-get profile :continuation)))

(defun e-openai-profile--continuation-mode (profile)
  "Return the proven continuation mode declared by PROFILE.

Boolean continuation declarations retain the historical linear mode.  A
named profile may explicitly declare `branchable' when it has independently
proved that inherited observations can branch from a clean anchor."
  (when (e-openai-profile--continuation-supported-p profile)
    (if (eq (plist-get profile :continuation) 'branchable)
        'branchable
      'linear)))

(defun e-openai-profile-observation-delivery (profile)
  "Return the validated semantic observation delivery for PROFILE.
An absent profile field is the conservative inherited default.  An explicitly
unknown value is a provider configuration error rather than an implicit
fallback, because selecting replacement changes continuation safety."
  (if (not (plist-member profile :observation-delivery))
      'inherited
    (let ((delivery (plist-get profile :observation-delivery)))
      (unless (memq delivery '(inherited request-local-replaceable))
        (signal 'e-openai-provider-invalid
                (list (format "Unsupported :observation-delivery %S"
                              delivery))))
      delivery)))

(defun e-openai-profile-observation-delivery-for-options (options)
  "Return semantic observation delivery declared by request OPTIONS."
  (or (plist-get options :observation-delivery)
      (plist-get (plist-get options :context-capabilities)
                 :observation-delivery)))

(defun e-openai-profile-context-base-url-equal-p (left right)
  "Return non-nil when provider base URLs LEFT and RIGHT identify one endpoint."
  (and (stringp left)
       (stringp right)
       (equal (string-remove-suffix "/" left)
              (string-remove-suffix "/" right))))

(cl-defun e-openai-profile-context-capabilities
    (profile options &key provider base-url request-function
             provider-compaction-supported)
  "Return provider-neutral context capabilities for PROFILE and OPTIONS.

Only explicit profile evidence can select a request-local replaceable
observation channel.  The adapter owns this profile/transport decision; the
harness sees only the normalized semantic values.  BASE-URL and
REQUEST-FUNCTION describe the effective backend identity.  A built-in
first-party claim is not carried across an endpoint or transport override;
named custom profiles may retain their own explicit proof while using an
injected requester for conformance tests."
  (let* ((wire-api (e-openai-provider-wire-api profile))
         (declared-delivery (e-openai-profile-observation-delivery profile))
         (profile-base-url (plist-get profile :base-url))
         (profile-transport
          (and (eq wire-api 'responses)
               (e-openai-profile-responses-transport profile)))
         (requested-transport
          (and (plist-member options :responses-transport)
               (plist-get options :responses-transport)))
         (endpoint-compatible
          (or (null base-url)
              (e-openai-profile-context-base-url-equal-p
               base-url profile-base-url)))
         (transport-compatible
          (or (null requested-transport)
              (eq requested-transport profile-transport)))
         (first-party-provider-p (memq provider '(codex openai)))
         ;; Built-in proof is tied to the first-party Responses endpoint and
         ;; native WebSocket requester.  A named custom profile may still use
         ;; an injected requester in its own conformance tests.
         (first-party-transport-compatible
          (or (not first-party-provider-p)
              (and (not request-function)
                   (eq profile-transport 'websocket))))
         (first-party-profile-compatible
          (pcase provider
            ('codex (e-openai-profile-builtin-codex-p provider profile))
            ('openai
             (e-openai-profile-context-base-url-equal-p
              profile-base-url e-openai-api-default-base-url))
            (_ t)))
         (identity-compatible
          (and endpoint-compatible
               transport-compatible
               first-party-transport-compatible
               first-party-profile-compatible))
         (observation-delivery
          (if (and identity-compatible
                   (eq wire-api 'responses)
                   (eq declared-delivery 'request-local-replaceable))
              (copy-tree e-openai-profile--request-local-observation-delivery-map)
            'inherited))
         (prefix-cache
          (cond
           ((and (eq wire-api 'responses)
                 (eq (plist-get profile :prompt-cache-breakpoint-mode)
                     'explicit))
            'explicit)
           ((plist-get options :prompt-cache-key) 'implicit)
           (t 'none))))
    (list :continuation
          (if identity-compatible
              (or (e-openai-profile--continuation-mode profile) 'none)
            'none)
          :observation-delivery observation-delivery
          :prefix-cache prefix-cache
          :provider-compaction
          (if provider-compaction-supported 'opaque 'none)
          :reasoning-state
          (if (plist-get profile :include-encrypted-reasoning)
              'replayable
            'none)
          :reserved-effect-carrier
          (if (eq wire-api 'responses)
              'context-curate-wire
            'none))))

(defun e-openai-profile-harness-default-options (profile model)
  "Return backend-neutral harness options for PROFILE and MODEL."
  (let ((options (list :model model
                       :reasoning-effort e-openai-default-reasoning-effort)))
    (when (eq (e-openai-provider-wire-api profile) 'responses)
      (setq options
            (append options
                    (list :reasoning-summary
                          (e-openai-profile-reasoning-summary profile)))))
    (if (e-openai-profile--continuation-supported-p profile)
        (append options
                (list :provider-continuation t
                      :provider-anchor-provider-id 'openai))
      options)))

(defun e-openai-profile--env-token (env-key)
  "Return bearer token from ENV-KEY or signal an auth error."
  (unless (and (stringp env-key) (not (string-empty-p env-key)))
    (signal 'e-openai-auth-invalid
            '("Token-auth provider is missing :env-key")))
  (let ((token (getenv env-key)))
    (unless (and (stringp token) (not (string-empty-p token)))
      (signal 'e-openai-auth-missing
              (list (format "Environment variable %s is missing" env-key))))
    token))


(defun e-openai-codex-auth-file (&optional codex-home)
  "Return the Codex auth file path for CODEX-HOME.
When CODEX-HOME is nil, use the CODEX_HOME environment variable or
`~/.codex'."
  (expand-file-name
   "auth.json"
   (or codex-home
       (getenv "CODEX_HOME")
       (expand-file-name "~/.codex"))))

(defun e-openai-codex-read-auth (&optional auth-file)
  "Read Codex auth from AUTH-FILE and return a plist."
  (let ((file (or auth-file (e-openai-codex-auth-file))))
    (unless (file-readable-p file)
      (signal 'e-openai-auth-missing (list file)))
    (e-json-parse-string (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))

(defun e-openai-codex-auth-access-token (auth)
  "Return AUTH's access token."
  (or (plist-get (plist-get auth :tokens) :access_token)
      (plist-get auth :access_token)
      (signal 'e-openai-auth-invalid '("Missing access_token"))))

(defun e-openai-profile--base64url-decode (value)
  "Decode base64url VALUE."
  (let* ((normalized (replace-regexp-in-string "-" "+" value))
         (normalized (replace-regexp-in-string "_" "/" normalized))
         (padding (mod (- 4 (mod (length normalized) 4)) 4)))
    (base64-decode-string
     (concat normalized (make-string padding ?=)))))

(defun e-openai-profile--json-key (key)
  "Return plist keyword for JSON object KEY."
  (intern (concat ":" key)))

(defun e-openai-codex-auth-account-id (auth)
  "Extract the ChatGPT account id from AUTH's access token."
  (let* ((token (e-openai-codex-auth-access-token auth))
         (parts (split-string token "\\."))
         (payload (nth 1 parts)))
    (unless (= (length parts) 3)
      (signal 'e-openai-auth-invalid '("Access token is not a JWT")))
    (let* ((claims (e-json-parse-string
                    (e-openai-profile--base64url-decode payload)))
           (account-claims (plist-get
                            claims
                            (e-openai-profile--json-key
                             e-openai-codex-account-claim)))
           (account-id (plist-get account-claims :chatgpt_account_id)))
      (or account-id
          (signal 'e-openai-auth-invalid
                  '("Missing ChatGPT account id claim"))))))


(defun e-openai-profile--codex-headers (auth &optional session-id)
  "Return Codex request headers for AUTH and SESSION-ID."
  (let* ((token (e-openai-codex-auth-access-token auth))
         (headers `(("Authorization" . ,(concat "Bearer " token))
                    ("chatgpt-account-id" . ,(e-openai-codex-auth-account-id auth))
                    ("originator" . "e")
                    ("OpenAI-Beta" . "responses=experimental")
                    ("Accept" . "text/event-stream")
                    ("Content-Type" . "application/json"))))
    (if session-id
        (append headers
                `(("session_id" . ,session-id)
                  ("x-client-request-id" . ,session-id)))
      headers)))

(defun e-openai-profile--token-headers (token)
  "Return standard Responses headers using bearer TOKEN."
  `(("Authorization" . ,(concat "Bearer " token))
    ("Accept" . "text/event-stream")
    ("Content-Type" . "application/json")))

(cl-defun e-openai-profile-headers (&key profile auth-file session-id)
  "Return request headers for PROFILE.
AUTH-FILE and SESSION-ID are used only for Codex-managed OpenAI auth
profiles."
  (if (plist-get profile :requires-openai-auth)
      (e-openai-profile--codex-headers
       (e-openai-codex-read-auth auth-file)
       session-id)
    (e-openai-profile--token-headers
     (e-openai-profile--env-token (plist-get profile :env-key)))))

(defun e-openai-profile-responses-websocket-headers (headers)
  "Return HEADERS adjusted for the Responses WebSocket handshake."
  (append (seq-remove (lambda (header)
                        (member (car header) '("Accept" "OpenAI-Beta")))
                      headers)
          '(("OpenAI-Beta" . "responses_websockets=2026-02-06"))))



(provide 'e-openai-profile)

;;; e-openai-profile.el ends here
