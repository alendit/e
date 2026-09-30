;;; e-current-config-anthropic-s7-test.el --- F97 current Doom gateway probe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Opt-in F97-S7 composition through the private current-config daemon. The
;; selector uses the registered Doom :chat-default factory and records only
;; bounded request identity, cache, status, and usage evidence.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'e-anthropic)
(require 'e-backend)
(require 'e-context)
(require 'e-context-lifetime)
(require 'e-json)
(require 'e-harness)
(require 'e-harness-activity)
(require 'e-harness-registry)
(require 'e-layers)
(require 'e-session)
(require 'e-session-async)
(require 'e-tools)

(load (expand-file-name
       "e-chat-sql-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(defvar e-current-config-anthropic-s7--tool-output nil
  "Per-run output returned by the S7 deterministic tool.")

(defun e-current-config-anthropic-s7--tool-register (registry &rest _context)
  "Register the test-only deterministic tool in REGISTRY."
  (e-tools-register
   registry
   :name "e2e_s7_deterministic"
   :description "Return one bounded value for current-config validation."
   :parameters '(:type "object" :properties nil)
   :work
   (e-tools-cheap-work
    "e2e.current-config.f97-s7"
    (lambda (_arguments)
      e-current-config-anthropic-s7--tool-output))))

(defun e-current-config-anthropic-s7--source-provider (name value-function)
  "Create a dynamic context provider named NAME using VALUE-FUNCTION."
  (e-context-provider-create
   :name name
   :cache-placement 'dynamic-context
   :build (lambda (&rest _context)
            (list (list :role 'system :content (funcall value-function))))))

(defun e-current-config-anthropic-s7--tool-layer ()
  "Create the one ordinary tool required by F97-S7."
  (e-layer-create
   :id 'f97-s7-anthropic-tool
   :name "F97 S7 Anthropic Tool"
   :capabilities
   (list
    (e-capability-create
     :id 'f97-s7-anthropic-tool
     :name "F97 S7 Anthropic Tool"
     :instructions
     "For this validation, call e2e_s7_deterministic exactly when the user asks."
     :tools (list #'e-current-config-anthropic-s7--tool-register)))))

(defun e-current-config-anthropic-s7--context-layer
    (stable-instructions providers)
  "Create a stable instruction layer with dynamic source PROVIDERS."
  (e-layer-create
   :id 'f97-s7-anthropic-context
   :name "F97 S7 Anthropic Context"
   :capabilities
   (list
    (e-capability-create
     :id 'f97-s7-anthropic-context
     :name "F97 S7 Anthropic Context"
     :instructions stable-instructions
     :context-providers providers))))

(defun e-current-config-anthropic-s7--safe-headers (headers)
  "Return HEADERS with secret header values replaced by presence markers."
  (mapcar
   (lambda (header)
     (let* ((name (car header))
            (normalized (downcase (format "%s" name)))
            (secret (member normalized
                            '("authorization" "x-api-key"
                              "proxy-authorization" "cookie" "set-cookie"))))
       (cons name (if secret "present" (cdr header)))))
   headers))

(defun e-current-config-anthropic-s7--capture-http-start
    (original capture-state &rest arguments)
  "Capture the actual outgoing Messages request and call ORIGINAL unchanged."
  (let* ((arguments (copy-sequence arguments))
         (body (e-json-parse-string (plist-get arguments :body)))
         (record (list :provider e-anthropic-default-provider
                       :url (plist-get arguments :url)
                       :transport "url-retrieve"
                       :headers
                       (e-current-config-anthropic-s7--safe-headers
                        (plist-get arguments :headers))
                       :body body
                       :status nil
                       :response-events nil
                       :response-shapes nil
                       :response-event-count nil
                       :response-bytes nil
                       :failure-class nil))
         (on-body-chunk (plist-get arguments :on-body-chunk))
         (on-complete (plist-get arguments :on-complete))
         (on-http-error (plist-get arguments :on-http-error))
         (on-error (plist-get arguments :on-error)))
    (setf (plist-get capture-state :requests)
          (cons record (plist-get capture-state :requests)))
    (setq arguments
          (plist-put
           arguments :on-body-chunk
           (lambda (chunk status)
             (setf (plist-get record :status) status)
             (when on-body-chunk
               (funcall on-body-chunk chunk status)))))
    (setq arguments
          (plist-put
           arguments :on-complete
           (lambda (response)
             (let* ((parsed-events
                     (condition-case condition
                         (e-anthropic--sse-data response)
                       (error (list (list :type (car-safe condition))))))
                    (events (mapcar (lambda (event) (plist-get event :type))
                                    parsed-events))
                    (shapes
                     (mapcar
                      (lambda (event)
                        (let* ((block (plist-get event :content_block))
                               (delta (plist-get event :delta)))
                          (list (plist-get event :type)
                                (plist-get event :index)
                                (plist-get block :type)
                                (plist-get delta :type)
                                (and (plist-member block :signature)
                                     (if (equal (plist-get block :signature) "")
                                         'empty 'present))
                                (when (stringp (plist-get delta :thinking))
                                  (length (plist-get delta :thinking)))
                                (when (stringp (plist-get delta :signature))
                                  (length (plist-get delta :signature))))))
                      parsed-events))
                    (count (length events)))
               (setf (plist-get record :response-bytes) (length response)
                     (plist-get record :response-event-count) count
                     (plist-get record :response-shapes)
                     (if (> count 40)
                         (append (seq-take shapes 30)
                                 '(("...")) (last shapes 10))
                       shapes)
                     (plist-get record :response-events)
                     (if (> count 40)
                         (append (seq-take events 30)
                                 '("...") (last events 10))
                       events)))
             (when on-complete
               (funcall on-complete response)))))
    (setq arguments
          (plist-put
           arguments :on-http-error
           (lambda (response status)
             (setf (plist-get record :status) status)
             (when on-http-error
               (funcall on-http-error response status)))))
    (setq arguments
          (plist-put
           arguments :on-error
           (lambda (condition)
             (setf (plist-get record :failure-class)
                   (car-safe condition))
             (when on-error
               (funcall on-error condition)))))
    (apply original arguments)))

(defun e-current-config-anthropic-s7--content-blocks (body)
  "Return native message content blocks in BODY in conversation order."
  (let (blocks)
    (dolist (message (append (plist-get body :messages) nil))
      (dolist (block (append (plist-get message :content) nil))
        (push block blocks)))
    (nreverse blocks)))

(defun e-current-config-anthropic-s7--find-tool-use (body name)
  "Return BODY's first native tool-use block named NAME."
  (seq-find
   (lambda (block)
     (and (equal (plist-get block :type) "tool_use")
          (equal (plist-get block :name) name)))
   (e-current-config-anthropic-s7--content-blocks body)))

(defun e-current-config-anthropic-s7--tool-results (body)
  "Return BODY's native tool-result blocks in conversation order."
  (seq-filter
   (lambda (block) (equal (plist-get block :type) "tool_result"))
   (e-current-config-anthropic-s7--content-blocks body)))

(defun e-current-config-anthropic-s7--tool-result-value (result)
  "Return RESULT's raw value from plain or source-labeled content."
  (let* ((content (plist-get result :content))
         (separator (and (stringp content)
                         (string-match "\n\n" content))))
    (if (and separator
             (string-match-p
              "\\`\\[ephemeral context source [0-9]+,"
              (substring content 0 separator)))
        (substring content (+ separator 2))
      content)))

(defun e-current-config-anthropic-s7--tool-names (body)
  "Return the offered tool names in BODY."
  (mapcar (lambda (tool) (plist-get tool :name))
          (append (plist-get body :tools) nil)))

(defun e-current-config-anthropic-s7--tool-pairs-in-body (body)
  "Return native call/result pairs in BODY, rejecting malformed pairs."
  (let ((calls (make-hash-table :test 'equal))
        (results (make-hash-table :test 'equal))
        call-entries
        result-entries)
    (cl-loop for block in (e-current-config-anthropic-s7--content-blocks body)
             for position from 0
             do (pcase (plist-get block :type)
                  ("tool_use"
                   (push (cons position block) call-entries))
                  ("tool_result"
                   (push (cons position block) result-entries))))
    (setq call-entries (nreverse call-entries)
          result-entries (nreverse result-entries))
    (dolist (entry call-entries)
      (let ((id (plist-get (cdr entry) :id)))
        (unless (and (stringp id) (not (string-empty-p id)))
          (error "F97-S7 native tool call has no valid id"))
        (when (gethash id calls)
          (error "F97-S7 request body has duplicate native tool call id"))
        (puthash id entry calls)))
    (dolist (entry result-entries)
      (let ((id (plist-get (cdr entry) :tool_use_id)))
        (unless (and (stringp id) (not (string-empty-p id)))
          (error "F97-S7 native tool result has no valid call id"))
        (when (gethash id results)
          (error "F97-S7 request body has duplicate native tool result id"))
        (unless (gethash id calls)
          (error "F97-S7 native tool result has no matching call"))
        (puthash id entry results)))
    (mapcar
     (lambda (entry)
       (let* ((call (cdr entry))
              (id (plist-get call :id))
              (result-entry (gethash id results)))
         (unless result-entry
           (error "F97-S7 native tool call has no matching result"))
         (unless (< (car entry) (car result-entry))
           (error "F97-S7 native tool result precedes its call"))
         (cons call (cdr result-entry))))
     call-entries)))

(defun e-current-config-anthropic-s7--captured-tool-pairs (bodies)
  "Return unique native tool pairs across captured request BODIES.
Repeated transcript history is accepted only when its pair is unchanged."
  (let ((pairs-by-id (make-hash-table :test 'equal))
        (ids-by-name (make-hash-table :test 'equal))
        pairs)
    (dolist (body bodies)
      (dolist (pair (e-current-config-anthropic-s7--tool-pairs-in-body body))
        (let* ((call (car pair))
               (id (plist-get call :id))
               (name (plist-get call :name))
               (previous-pair (gethash id pairs-by-id)))
          (unless (and (stringp name) (not (string-empty-p name)))
            (error "F97-S7 native tool call has no valid name"))
          (if previous-pair
              (unless (equal previous-pair pair)
                (error "F97-S7 repeated native tool pair changed"))
            (when (gethash name ids-by-name)
              (error "F97-S7 captured a duplicate call for one tool"))
            (puthash id pair pairs-by-id)
            (puthash name id ids-by-name)
            (push pair pairs)))))
    (nreverse pairs)))

(ert-deftest e-current-config-anthropic-s7-tool-pair-helper ()
  "Validate native tool pairing without credentials or an Emacs daemon."
  (let* ((make-body
          (lambda (&rest blocks)
            (list :messages
                  (vector (list :role "assistant"
                                :content (vconcat blocks))))))
         (call-a '(:type "tool_use" :id "call-a"
                   :name "e2e_s7_deterministic" :input ()))
         (result-a '(:type "tool_result" :tool_use_id "call-a"
                     :content "output-a"))
         (call-b '(:type "tool_use" :id "call-b"
                   :name "context-curate" :input ()))
         (result-b '(:type "tool_result" :tool_use_id "call-b"
                     :content "ack-b"))
         (duplicate-call
          '(:type "tool_use" :id "call-c"
            :name "e2e_s7_deterministic" :input ()))
         (duplicate-result
          '(:type "tool_result" :tool_use_id "call-c" :content "output-c")))
    (should
     (equal
      (mapcar (lambda (pair)
                (cons (plist-get (car pair) :name)
                      (plist-get (cdr pair) :tool_use_id)))
              (e-current-config-anthropic-s7--captured-tool-pairs
               (list (funcall make-body call-a result-a)
                     (funcall make-body call-a result-a call-b result-b))))
      '(("e2e_s7_deterministic" . "call-a")
        ("context-curate" . "call-b"))))
    (should
     (equal
      (e-current-config-anthropic-s7--tool-result-value
       '(:content "[ephemeral context source 3, ~4 tokens, erase-eligible]\n\noutput-a"))
      "output-a"))
    (should-error
     (e-current-config-anthropic-s7--tool-pairs-in-body
      (funcall make-body call-a call-a result-a)))
    (should-error
     (e-current-config-anthropic-s7--tool-pairs-in-body
      (funcall make-body call-a result-b)))
    (should-error
     (e-current-config-anthropic-s7--captured-tool-pairs
      (list (funcall make-body call-a result-a)
            (funcall make-body duplicate-call duplicate-result))))))

(defun e-current-config-anthropic-s7--cache-checkpoint (body)
  "Return BODY's native stable system checkpoint and its prefix/suffix."
  (let* ((system (plist-get body :system))
         (index
          (and (vectorp system)
               (not (stringp system))
               (cl-position-if
                (lambda (block) (plist-member block :cache_control)) system))))
    (when index
      (let ((checkpoint (aref system index)))
        (list :index index
              :control (plist-get checkpoint :cache_control)
              :prefix
              (list :tools (plist-get body :tools)
                    :system (cl-subseq system 0 (1+ index)))
              :suffix (cl-subseq system (1+ index)))))))

(defun e-current-config-anthropic-s7--open-cache-pair
    (bodies checkpoints old-source new-source)
  "Find open-frame BODIES sharing a prefix across OLD-SOURCE and NEW-SOURCE.
The curation acknowledgement intentionally closes the reserved carrier, so its
tool schema is a different prefix and must not be compared with open requests."
  (cl-loop for first from 0 below (length bodies)
           for first-body = (nth first bodies)
           for first-checkpoint = (nth first checkpoints)
           when (and (member "context-curate"
                             (e-current-config-anthropic-s7--tool-names first-body))
                     (string-match-p
                      (regexp-quote old-source)
                      (prin1-to-string (plist-get first-checkpoint :suffix))))
           thereis
           (cl-loop for second from (1+ first) below (length bodies)
                    for second-body = (nth second bodies)
                    for second-checkpoint = (nth second checkpoints)
                    when (and
                          (member "context-curate"
                                  (e-current-config-anthropic-s7--tool-names
                                   second-body))
                          (equal (plist-get first-checkpoint :prefix)
                                 (plist-get second-checkpoint :prefix))
                          (string-match-p
                           (regexp-quote new-source)
                           (prin1-to-string
                            (plist-get second-checkpoint :suffix))))
                    return (list :first-index first
                                 :second-index second
                                 :first-checkpoint first-checkpoint
                                 :second-checkpoint second-checkpoint))))

(ert-deftest e-current-config-anthropic-s7-open-cache-pair-helper ()
  "Compare open-frame prefixes while allowing the consumed carrier to close."
  (let* ((open-tools [(:name "e2e_s7_deterministic")
                     (:name "context-curate")])
         (closed-tools [(:name "e2e_s7_deterministic")])
         (stable '(:type "text" :text "stable"
                   :cache_control (:type "ephemeral" :ttl "1h")))
         (make-body
          (lambda (tools suffix)
            (list :tools tools
                  :system (vector stable (list :type "text" :text suffix)))))
         (bodies (list (funcall make-body open-tools "old-source")
                       (funcall make-body open-tools "old-source other turn")
                       (funcall make-body closed-tools "after-curation")
                       (funcall make-body open-tools "new-source")))
         (checkpoints
          (mapcar #'e-current-config-anthropic-s7--cache-checkpoint bodies))
         (pair (e-current-config-anthropic-s7--open-cache-pair
                bodies checkpoints "old-source" "new-source")))
    (should pair)
    (should (= (plist-get pair :first-index) 0))
    (should (= (plist-get pair :second-index) 3))
    (should (equal (plist-get (plist-get pair :first-checkpoint) :prefix)
                   (plist-get (plist-get pair :second-checkpoint) :prefix)))
    (should-not (equal (plist-get (plist-get pair :first-checkpoint) :prefix)
                       (plist-get (nth 2 checkpoints) :prefix)))))

(defun e-current-config-anthropic-s7--request-summary (record)
  "Return a content-free evidence summary for captured REQUEST RECORD."
  (let* ((body (plist-get record :body))
         (body-text (prin1-to-string body))
         (ordinary-call
          (e-current-config-anthropic-s7--find-tool-use
           body "e2e_s7_deterministic"))
         (ordinary-result
          (and ordinary-call
               (seq-find
                (lambda (result)
                  (equal (plist-get result :tool_use_id)
                         (plist-get ordinary-call :id)))
                (e-current-config-anthropic-s7--tool-results body))))
         (ordinary-content (plist-get ordinary-result :content))
         (curation-call
          (e-current-config-anthropic-s7--find-tool-use
           body "context-curate"))
         (checkpoint
          (e-current-config-anthropic-s7--cache-checkpoint body))
         (thinking (plist-get body :thinking))
         (output-config (plist-get body :output_config)))
    (list :provider (plist-get record :provider)
          :endpoint (plist-get record :url)
          :transport (plist-get record :transport)
          :headers (plist-get record :headers)
          :status (plist-get record :status)
          :response-bytes (plist-get record :response-bytes)
          :response-event-count (plist-get record :response-event-count)
          :response-events (plist-get record :response-events)
          :response-shapes (plist-get record :response-shapes)
          :failure-class (plist-get record :failure-class)
          :ordinary-result-shape
          (when ordinary-result
            (list :content-kind (cond ((vectorp ordinary-content) 'blocks)
                                      ((stringp ordinary-content) 'string)
                                      (t 'other))
                  :block-count (and (vectorp ordinary-content)
                                    (length ordinary-content))
                  :source-marker
                  (and (string-match-p
                        (regexp-quote "[ephemeral context source")
                        (prin1-to-string ordinary-content))
                       t)
                  :raw-value
                  (and (string-match-p "F97-S7-TOOL-"
                                       (prin1-to-string ordinary-content))
                       t)))
          :curation-input-shape
          (when curation-call
            (let ((input (plist-get curation-call :input)))
              (list :keep (plist-get input :keep)
                    :summary-count
                    (length (plist-get input :summaries))
                    :erase-count
                    (length (plist-get input :erase)))))
          :scenario-markers
          (list :r2-prompt (and (string-match-p "LIVE-S7-R2-READY" body-text) t)
                :curation-marker
                (and (string-match-p
                      (regexp-quote "[ephemeral context source") body-text)
                     t)
                :initial-source
                (and (string-match-p "F97-S7-CURRENT-INITIAL-" body-text) t)
                :omitted-source
                (and (string-match-p "F97-S7-OMIT-" body-text) t)
                :tool-output
                (and (string-match-p "F97-S7-TOOL-" body-text) t))
          :options
          (list :model (plist-get body :model)
                :max-tokens (plist-get body :max_tokens)
                :stream (plist-get body :stream)
                :thinking thinking
                :effort (plist-get output-config :effort))
          :cache-control (plist-get checkpoint :control)
          :checkpoint-index (plist-get checkpoint :index)
          :tool-names
          (mapcar (lambda (tool) (plist-get tool :name))
                  (append (plist-get body :tools) nil)))))

(defun e-current-config-anthropic-s7--sum-usage (events key)
  "Sum numeric token counter KEY from captured token-usage EVENTS."
  (apply #'+
         (mapcar
          (lambda (event)
            (or (plist-get (plist-get event :payload) key) 0))
          events)))

(defun e-current-config-anthropic-s7--curations (harness session-id)
  "Return durable curation promotions through a bounded SQLite read."
  (delq nil
        (mapcar
         (lambda (entry)
           (plist-get (plist-get entry :value) :promotion))
         (plist-get
          (e-chat-sql-e2e--await
           (e-session-async-record-page
            (e-harness-sessions harness) session-id
            :record-type "context-curation-package" :limit 20))
          :records))))

(defun e-current-config-anthropic-s7--utc-now ()
  "Return the current time in explicit UTC form."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun e-current-config-anthropic-s7--safe-error (value)
  "Return a bounded diagnostic for VALUE without the gateway credential."
  (let ((text (format "%s" value))
        (credential (getenv "ENG_AI_MODEL_GW_KEY")))
    (when (and credential (not (string-empty-p credential)))
      (setq text (replace-regexp-in-string
                  (regexp-quote credential) "<redacted>" text t t)))
    (substring text 0 (min 500 (length text)))))

(ert-deftest e-current-config-e2e-test-f97-s7-anthropic-default ()
  "Run the opt-in Opus 5.5 current-Doom composition through :chat-default."
  (unless (equal (getenv "E_CURRENT_CONFIG_F97_S7") "1")
    (ert-skip "Set E_CURRENT_CONFIG_F97_S7=1 to enable F97-S7."))
  (unless (equal (getenv "E_CURRENT_CONFIG_E2E_SELECTOR")
                 "e-current-config-e2e-test-f97-s7-anthropic-default")
    (ert-skip "F97-S7 requires its exact current-config selector."))
  (when (getenv "CI")
    (ert-skip "Current-Doom gateway probes are disabled in CI."))
  (should-not (getenv "E_E2E_CONFIG"))
  (should (string-match-p
           "\\`[[:xdigit:]]\\{40\\}\\'"
           (or (getenv "E_CURRENT_CONFIG_F97_REPO_HEAD") "")))
  (dolist (name '("E_CURRENT_CONFIG_F97_DOOM_ORG_SHA256"
                  "E_CURRENT_CONFIG_F97_DOOM_EL_SHA256"))
    (should (string-match-p
             "\\`[[:xdigit:]]\\{64\\}\\'"
             (or (getenv name) ""))))
  (unless (and (stringp (getenv "ENG_AI_MODEL_GW_KEY"))
               (not (string-empty-p (getenv "ENG_AI_MODEL_GW_KEY"))))
    (e-current-config-e2e-test--print
     "F97-S7 evidence: (:configuration-result unavailable :reason missing-gateway-key :first-failing-boundary configuration)\n")
    (ert-skip "ENG_AI_MODEL_GW_KEY is unavailable."))
  (let* ((harness (e-harness-registry-get-or-create :chat-default))
         (profile (e-anthropic-provider-profile e-anthropic-default-provider))
         (old-current (concat "F97-S7-CURRENT-INITIAL-" (format "%x" (random))))
         (new-current (concat "F97-S7-CURRENT-LATEST-" (format "%x" (random))))
         (omitted-source (concat "F97-S7-OMIT-" (format "%x" (random))))
         (tool-output (concat "F97-S7-TOOL-" (format "%x" (random))))
         (current-source old-current)
         (omit-source omitted-source)
         (providers
          (list
           (e-current-config-anthropic-s7--source-provider
            'f97-s7-current-source (lambda () current-source))
           (e-current-config-anthropic-s7--source-provider
            'f97-s7-omitted-source (lambda () omit-source))))
         (stable-instructions
          (concat "F97-S7 stable-prefix instructions. "
                  (mapconcat #'identity
                             (make-list 100 "stable-prefix-guidance-token")
                             " ")))
         (context-layer
          (e-current-config-anthropic-s7--context-layer
           stable-instructions providers))
         (tool-layer (e-current-config-anthropic-s7--tool-layer))
         (capture-state (list :requests nil))
         events
         subscription
         session-id
         (started-at (e-current-config-anthropic-s7--utc-now))
         (failure-stage "registered-default")
         (failure-detail nil)
         (composition-result "in-progress")
         (usage-events nil)
         (requests nil)
         (prefix-hash nil)
         (first-suffix-hash nil)
         (second-suffix-hash nil)
         (tool-schema-hash nil)
         (first-pair-index nil)
         (second-pair-index nil)
         (failure-class nil)
         (tool-option-trace nil)
         (native-tool-definitions
          (symbol-function 'e-anthropic--request-tool-definitions))
         (native-start (symbol-function 'e-anthropic--http-request-start)))
    (unwind-protect
        (condition-case condition
            (progn
              (should (e-harness-p harness))
              (should (eq e-anthropic-default-provider
                          'eng-ai-gateway-opus-5-5))
              (should (equal (plist-get profile :default-model)
                             "claude-opus-5-5"))
              (should (equal (plist-get
                              (e-harness-default-options harness)
                              :prompt-cache-ttl)
                             "1h"))
              (should (eq (plist-get (e-harness-default-options harness)
                                     :prompt-cache)
                          t))
              (e-harness-set-intrinsic-capabilities
               harness
               (append (e-harness-intrinsic-capabilities harness)
                       (e-layer-capabilities context-layer)
                       (e-layer-capabilities tool-layer)))
              (setq session-id
                    (e-chat-sql-e2e-create-session
                     harness :metadata
                     (list :project-root e-e2e-project-root)))
              (setq subscription
                    (e-harness-activity-subscribe
                     harness
                     (lambda (event)
                       (when (memq (plist-get event :type)
                                   '(token-usage context-frame-consumed))
                         (push (copy-tree event) events)))
                     :session-id session-id))
              (let ((e-current-config-anthropic-s7--tool-output tool-output))
                (cl-letf
                    (((symbol-function 'e-anthropic--request-tool-definitions)
                      (lambda (tools options)
                        (push (list :carrier-present
                                    (and (plist-member options
                                                       :reserved-effect-carrier)
                                         t)
                                    :carrier
                                    (plist-get options :reserved-effect-carrier)
                                    :capability-carrier
                                    (plist-get
                                     (plist-get options :context-capabilities)
                                     :reserved-effect-carrier)
                                    :lifetime-enabled
                                    (plist-get options :context-lifetime-enabled))
                              tool-option-trace)
                        (funcall native-tool-definitions tools options)))
                     ((symbol-function 'e-anthropic--http-request-start)
                      (lambda (&rest arguments)
                        (apply #'e-current-config-anthropic-s7--capture-http-start
                               native-start capture-state arguments))))
                  (setq failure-stage "initial-cache-write-turn")
                  (let ((result
                         (e-chat-sql-e2e-prompt-batch
                          harness session-id
                          "Reply with exactly LIVE-S7-R0-READY. Do not call tools or context-curate, and do not repeat either observation source."
                          240.0)))
                    (unless (eq (plist-get result :status) 'done)
                      (setq failure-detail
                            (e-current-config-anthropic-s7--safe-error
                             (plist-get result :error))))
                    (should (eq (plist-get result :status) 'done))
                    (should (equal (string-trim
                                    (plist-get result :assistant-content))
                                   "LIVE-S7-R0-READY")))
                  (setq failure-stage "ordinary-tool-and-context-curation")
                  (let ((result
                         (e-chat-sql-e2e-prompt-batch
                          harness session-id
                          (concat
                           "Call e2e_s7_deterministic exactly once. Its tool_result contains a marker of the form [ephemeral context source N, ...] beside the returned value. Read N from that same tool_result, then call the reserved context-curate tool exactly once with keep containing only N. Omit every other displayed source, including the source containing F97-S7-OMIT. Do not summarize or erase any source. Then reply with exactly LIVE-S7-R2-READY and no other text.")
                          300.0)))
                    (unless (eq (plist-get result :status) 'done)
                      (setq failure-detail
                            (e-current-config-anthropic-s7--safe-error
                             (plist-get result :error))))
                    (should (eq (plist-get result :status) 'done))
                    (should (equal (string-trim
                                    (plist-get result :assistant-content))
                                   "LIVE-S7-R2-READY")))
                  (setq failure-stage "fresh-turn-after-curation")
                  (setq current-source new-current
                        omit-source (concat "F97-S7-OMIT-REPLACED-"
                                            (format "%x" (random))))
                  (let ((result
                         (e-chat-sql-e2e-prompt-batch
                          harness session-id
                          "Reply with exactly the current observation value from the latest context and no extra words. Do not call tools or context-curate."
                          240.0)))
                    (unless (eq (plist-get result :status) 'done)
                      (setq failure-detail
                            (e-current-config-anthropic-s7--safe-error
                             (plist-get result :error))))
                    (should (eq (plist-get result :status) 'done))
                    (should (equal (string-trim
                                    (plist-get result :assistant-content))
                                   new-current)))))
              (setq requests (reverse (plist-get capture-state :requests))
                    usage-events
                    (seq-filter
                     (lambda (event)
                       (eq (plist-get event :type) 'token-usage))
                     (reverse events)))
              (setq failure-stage "request-shape-and-cache-evidence")
              (should requests)
              (should (seq-every-p
                       (lambda (request)
                         (= (plist-get request :status) 200))
                       requests))
              (let* ((bodies (mapcar (lambda (request)
                                       (plist-get request :body))
                                     requests))
                     (checkpoints
                      (mapcar
                       #'e-current-config-anthropic-s7--cache-checkpoint
                       bodies))
                     (final-body (car (last bodies)))
                     (cache-pair
                      (e-current-config-anthropic-s7--open-cache-pair
                       bodies checkpoints old-current new-current))
                     (first-checkpoint
                      (plist-get cache-pair :first-checkpoint))
                     (second-checkpoint
                      (plist-get cache-pair :second-checkpoint))
                     (first-suffix (plist-get first-checkpoint :suffix))
                     (second-suffix (plist-get second-checkpoint :suffix))
                     (tool-pairs
                      (e-current-config-anthropic-s7--captured-tool-pairs
                       bodies))
                     (tool-uses (mapcar #'car tool-pairs))
                     (tool-results (mapcar #'cdr tool-pairs))
                     (tool-use-names
                      (mapcar (lambda (block) (plist-get block :name))
                              tool-uses))
                     (curation-ack-bodies
                      (seq-filter
                       (lambda (body)
                         (let ((call
                                (e-current-config-anthropic-s7--find-tool-use
                                 body "context-curate")))
                           (and call
                                (seq-some
                                 (lambda (result)
                                   (equal (plist-get result :tool_use_id)
                                          (plist-get call :id)))
                                 (e-current-config-anthropic-s7--tool-results
                                  body)))))
                       bodies))
                     (tool-result-body
                      (seq-find
                       (lambda (body)
                         (and (string-match-p
                               (regexp-quote omitted-source)
                               (prin1-to-string body))
                              (seq-some
                               (lambda (result)
                                 (and (equal
                                       (e-current-config-anthropic-s7--tool-result-value
                                        result)
                                       tool-output)
                                      (let ((call
                                             (e-current-config-anthropic-s7--find-tool-use
                                              body "e2e_s7_deterministic")))
                                        (and call
                                             (equal
                                              (plist-get result :tool_use_id)
                                              (plist-get call :id))))))
                               (e-current-config-anthropic-s7--tool-results
                                body))))
                       bodies))
                     (curations
                      (e-current-config-anthropic-s7--curations
                       harness session-id))
                     (curation-record (car (last curations)))
                     (curation-items
                      (plist-get curation-record :items))
                     (consumed-events
                      (seq-filter
                       (lambda (event)
                         (and (eq (plist-get event :type)
                                  'context-frame-consumed)
                              (plist-get (plist-get event :payload)
                                         :curation)))
                       (reverse events)))
                     (curation-projection
                      (plist-get (plist-get (car consumed-events) :payload)
                                 :curation))
                     (tool-created
                      (e-current-config-anthropic-s7--sum-usage
                       usage-events :cache-creation-input-tokens))
                     (tool-read
                      (e-current-config-anthropic-s7--sum-usage
                       usage-events :cached-input-tokens)))
                (should (seq-every-p #'identity checkpoints))
                (unless cache-pair
                  (ert-fail "F97-S7 found no stable open-frame cache prefix across the changing source"))
                (should (equal (plist-get first-checkpoint :control)
                               '(:type "ephemeral" :ttl "1h")))
                (should (equal (plist-get second-checkpoint :control)
                               '(:type "ephemeral" :ttl "1h")))
                (unless (and (equal tool-use-names
                                    '("e2e_s7_deterministic" "context-curate"))
                             (= (length tool-results) 2))
                  (ert-fail "F97-S7 transcript did not contain one ordinary tool and one curation call"))
                (should curation-ack-bodies)
                (should tool-result-body)
                (when (member "context-curate"
                              (e-current-config-anthropic-s7--tool-names
                               (car curation-ack-bodies)))
                  (ert-fail "F97-S7 consumed-frame continuation still offered context-curate"))
                (unless (not (string-match-p
                              (regexp-quote omitted-source)
                              (prin1-to-string (car curation-ack-bodies))))
                  (ert-fail "F97-S7 curation acknowledgement repeated an omitted source"))
                (unless (and (string-match-p
                              (regexp-quote new-current)
                              (prin1-to-string final-body))
                             (not (string-match-p
                                   (regexp-quote old-current)
                                   (prin1-to-string final-body)))
                             (not (string-match-p
                                   (regexp-quote omitted-source)
                                   (prin1-to-string final-body))))
                  (ert-fail "F97-S7 fresh turn did not rebuild context without consumed sources"))
                (should curation-record)
                (should (= (length curation-items) 1))
                (should (eq (plist-get (car curation-items) :kind) 'exact))
                (should (equal (plist-get (car curation-items) :value)
                               tool-output))
                (should (= (plist-get curation-projection
                                      :kept-source-count)
                           1))
                (should (= (plist-get curation-projection :summary-count) 0))
                (should (> tool-created 0))
                (should (> tool-read 0))
                (setq prefix-hash
                      (secure-hash 'sha256
                                   (e-json-serialize
                                    (plist-get first-checkpoint :prefix)))
                      first-suffix-hash
                      (secure-hash 'sha256
                                   (e-json-serialize first-suffix))
                      second-suffix-hash
                      (secure-hash 'sha256
                                   (e-json-serialize second-suffix))
                      tool-schema-hash
                      (secure-hash 'sha256
                                   (e-json-serialize
                                    (plist-get
                                     (plist-get first-checkpoint :prefix)
                                     :tools)))
                      first-pair-index (plist-get cache-pair :first-index)
                      second-pair-index (plist-get cache-pair :second-index)))
              (setq composition-result "pass"
                    failure-stage "none"))
          (error
           (setq composition-result "failure"
                 failure-class (car-safe condition))
           (signal (car condition) (cdr condition))))
      (when subscription
        (ignore-errors
          (e-harness-activity-unsubscribe harness subscription)))
      (ignore-errors (e-chat-sql-e2e-reset))
      (let* ((ordered-requests (reverse (plist-get capture-state :requests)))
             (request-summaries
              (mapcar
               #'e-current-config-anthropic-s7--request-summary
               ordered-requests))
             (usage-events
              (seq-filter
               (lambda (event)
                 (eq (plist-get event :type) 'token-usage))
               (reverse events))))
        (e-current-config-e2e-test--print
         "F97-S7 evidence: %S\n"
         (list :scenario "F97-S7 current-Doom default composition"
               :utc-started-at started-at
               :utc-ended-at (e-current-config-anthropic-s7--utc-now)
               :configuration-result "available"
               :credential-source "ENG_AI_MODEL_GW_KEY"
               :configured-profile e-anthropic-default-provider
               :profile-name (plist-get profile :name)
               :endpoint (plist-get profile :base-url)
               :protocol "anthropic-messages"
               :composition-result composition-result
               :first-failing-boundary failure-stage
               :failure-class failure-class
               :failure-detail failure-detail
               :request-count (length ordered-requests)
               :requests request-summaries
               :tool-option-trace (reverse tool-option-trace)
               :repo-head (getenv "E_CURRENT_CONFIG_F97_REPO_HEAD")
               :doom-config-org-sha256
               (getenv "E_CURRENT_CONFIG_F97_DOOM_ORG_SHA256")
               :doom-config-el-sha256
               (getenv "E_CURRENT_CONFIG_F97_DOOM_EL_SHA256")
               :curation-schema-revision
               e-context-lifetime-curation-schema-revision
               :tool-schema-sha256 tool-schema-hash
               :stable-prefix-sha256 prefix-hash
               :first-cache-pair-request-index first-pair-index
               :second-cache-pair-request-index second-pair-index
               :first-cache-pair-suffix-sha256 first-suffix-hash
               :second-cache-pair-suffix-sha256 second-suffix-hash
               :cache-write-input-tokens
               (e-current-config-anthropic-s7--sum-usage
                usage-events :cache-creation-input-tokens)
               :cache-read-input-tokens
               (e-current-config-anthropic-s7--sum-usage
                usage-events :cached-input-tokens)))))))

(provide 'e-current-config-anthropic-s7-test)

;;; e-current-config-anthropic-s7-test.el ends here
