;;; e-anthropic-test.el --- Tests for e Anthropic Messages backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for Anthropic Messages auth, request mapping, and stream parsing.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-json)
(require 'e-backend)
(require 'e-harness)
(require 'e-loop)
(require 'e-session-codec)
(require 'e-session-sqlite)
(require 'e-tools)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(load (expand-file-name "e-tools-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-anthropic)

(defun e-anthropic-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun e-anthropic-test--sse-stream (events)
  "Encode canonical Messages EVENTS as an SSE response string."
  (mapconcat (lambda (event)
               (format "event: %s\ndata: %s\n\n"
                       (plist-get event :type)
                       (e-json-serialize event)))
             events
             ""))

(defconst e-anthropic-test--signed-two-tool-events
  '((:type "message_start"
     :message (:role "assistant" :content [] :usage (:input_tokens 17)))
    (:type "content_block_start" :index 0
     :content_block (:type "thinking" :thinking ""))
    (:type "content_block_delta" :index 0
     :delta (:type "thinking_delta" :thinking "Checking both paths."))
    (:type "content_block_delta" :index 0
     :delta (:type "signature_delta" :signature "sig-native-0"))
    (:type "content_block_stop" :index 0)
    (:type "content_block_start" :index 1
     :content_block (:type "text" :text ""))
    (:type "content_block_delta" :index 1
     :delta (:type "text_delta" :text "First, inspect both."))
    (:type "content_block_stop" :index 1)
    (:type "content_block_start" :index 2
     :content_block (:type "redacted_thinking" :data "redacted-native-2"))
    (:type "content_block_stop" :index 2)
    (:type "content_block_start" :index 3
     :content_block (:type "tool_use" :id "toolu-one"
                    :name "inspect-one" :input (:path "one")))
    (:type "content_block_stop" :index 3)
    (:type "content_block_start" :index 4
     :content_block (:type "tool_use" :id "toolu-two"
                    :name "inspect-two" :input (:path "two")))
    (:type "content_block_stop" :index 4)
    (:type "message_delta" :delta (:stop_reason "tool_use")
     :usage (:output_tokens 9))
    (:type "message_stop"))
  "An ordered signed/redacted response with two ordinary Messages tools.")

(defconst e-anthropic-test--signed-follow-up-tool-events
  '((:type "message_start"
     :message (:role "assistant" :content [] :usage (:input_tokens 20)))
    (:type "content_block_start" :index 0
     :content_block (:type "thinking" :thinking ""))
    (:type "content_block_delta" :index 0
     :delta (:type "thinking_delta" :thinking "Checking the third path."))
    (:type "content_block_delta" :index 0
     :delta (:type "signature_delta" :signature "sig-native-next-0"))
    (:type "content_block_stop" :index 0)
    (:type "content_block_start" :index 1
     :content_block (:type "tool_use" :id "toolu-three"
                    :name "inspect-three" :input (:path "three")))
    (:type "content_block_stop" :index 1)
    (:type "message_delta" :delta (:stop_reason "tool_use")
     :usage (:output_tokens 4))
    (:type "message_stop"))
  "A signed response with one tool for a subsequent follow-up round.")

(defun e-anthropic-test--assert-invalid-tool-response (events)
  "Assert EVENTS fail as one backend error before exposing any tool effects."
  (let ((items (e-anthropic-parse-stream
                (e-anthropic-test--sse-stream events))))
    (should (= (length items) 1))
    (should (eq (plist-get (car items) :type) 'backend-error))
    (should-not
     (seq-some (lambda (item)
                 (memq (plist-get item :type)
                       '(tool-call provider-replay-item)))
               items))))

(defun e-anthropic-test--tool-source-marker (tool-call-id text)
  "Return a typed request-local source marker for TOOL-CALL-ID and TEXT."
  (list :role 'system
        :content text
        :metadata
        (list e-context-lifetime--request-local-source-marker-key
              (list :kind
                    e-context-lifetime--request-local-tool-result-marker-kind
                    :tool-call-id tool-call-id))))

(defun e-anthropic-test--system-text (body)
  "Return the semantic system text from request BODY."
  (let ((system (plist-get body :system)))
    (cond
     ((stringp system) system)
     ((vectorp system)
      (mapconcat (lambda (block) (plist-get block :text))
                 (append system nil) "\n\n"))
     (t nil))))

(defun e-anthropic-test--stable-cache-prefix (body)
  "Return BODY's cached tool and system prefix, excluding its dynamic suffix."
  (let* ((system (append (plist-get body :system) nil))
         (breakpoint
          (cl-position-if (lambda (block)
                            (plist-member block :cache_control))
                          system)))
    (when breakpoint
      (list :tools (plist-get body :tools)
            :system (seq-take system (1+ breakpoint))))))

(defun e-anthropic-test--tool-definitions-without-cache-control (body)
  "Return BODY's tool definitions without their cache boundary metadata."
  (mapcar (lambda (tool)
            (let ((copy (copy-sequence tool)))
              (cl-remf copy :cache_control)
              copy))
          (append (plist-get body :tools) nil)))

(ert-deftest e-anthropic-test-request-body-maps-neutral-messages ()
  "Anthropic request body uses Messages turns with explicit max_tokens."
  (should
   (equal
    (e-anthropic-request-body
     :messages '((:role user :content "hello")
                 (:role assistant :content "hi"))
     :options '(:model "claude-test" :max-tokens 1024 :effort "high"))
    '(:model "claude-test"
      :max_tokens 1024
      :stream t
      :messages [(:role "user"
                  :content [(:type "text" :text "hello")])
                 (:role "assistant"
                  :content [(:type "text" :text "hi")])]
      :thinking (:type "adaptive")
      :output_config (:effort "high")))))

(ert-deftest e-anthropic-test-request-body-moves-system-messages-to-system-field ()
  "System messages and the instructions option fold into the top-level system field."
  (should
   (equal
    (e-anthropic-request-body
     :messages '((:role system :content "Layer instructions.")
                 (:role system :content "Visible buffer context.")
                 (:role user :content "hello"))
     :options '(:model "claude-test" :max-tokens 1024
                :instructions "Base instructions."))
    '(:model "claude-test"
      :max_tokens 1024
      :stream t
      :system "Base instructions.\n\nLayer instructions.\n\nVisible buffer context."
      :messages [(:role "user"
                  :content [(:type "text" :text "hello")])]
      :thinking (:type "adaptive")
      :output_config (:effort "high")))))

(ert-deftest e-anthropic-test-request-body-omits-system-when-empty ()
  "No system field is sent when there are no system messages or instructions."
  (should-not
   (plist-member
    (e-anthropic-request-body
     :messages '((:role user :content "hello"))
     :options '(:model "claude-test" :max-tokens 1024))
    :system)))

(ert-deftest e-anthropic-test-request-body-omits-thinking-when-opted-out ()
  "An explicit nil :anthropic-thinking omits the thinking and effort knobs.
Models such as Haiku reject `adaptive' thinking; a subagent harness opts out."
  (let ((body (e-anthropic-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "claude-haiku" :max-tokens 1024
                          :anthropic-thinking nil))))
    (should-not (plist-member body :thinking))
    (should-not (plist-member body :output_config))))

(ert-deftest e-anthropic-test-request-body-keeps-adaptive-thinking-by-default ()
  "Absent :anthropic-thinking, the adaptive thinking default is unchanged."
  (let ((body (e-anthropic-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "claude-test" :max-tokens 1024))))
    (should (equal (plist-get body :thinking) '(:type "adaptive")))))

(ert-deftest e-anthropic-test-request-body-maps-tool-definitions ()
  "Backend-neutral tools map to Messages tools with input_schema."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages '((:role user :content "hello"))
      :options '(:model "claude-test" :max-tokens 1024)
      :tools '((:type "function"
                :name "read"
                :description "Read a URI."
                :parameters (:type "object")
                :strict :json-false)))
     :tools)
    [(:name "read"
      :description "Read a URI."
      :input_schema (:type "object"))])))

(ert-deftest e-anthropic-test-request-body-retains-native-tool-schema ()
  "Messages carries the native operation schema without reinterpretation."
  (let* ((body
          (e-anthropic-request-body
           :messages '((:role user :content "hello"))
           :options '(:model "claude-test" :max-tokens 1024)
           :tools
           '((:type "function"
             :name "read"
             :description "Read a URI."
             :parameters (:type "object"
                           :properties (:uri (:type "string"))
             :required ["uri"]
             :additionalProperties :json-false)
             :strict :json-false))))
         (wire-tool (aref (plist-get body :tools) 0))
         (round-trip (e-json-parse-string (e-json-serialize body)))
         (round-trip-tool (aref (plist-get round-trip :tools) 0)))
    (should (equal (plist-get (plist-get wire-tool :input_schema) :required)
                   ["uri"]))
    (should (eq (plist-get (plist-get wire-tool :input_schema)
                           :additionalProperties)
                :json-false))
    (should (equal (plist-get (plist-get round-trip-tool :input_schema)
                             :required)
                   ["uri"]))
    (should (eq (plist-get (plist-get round-trip-tool :input_schema)
                           :additionalProperties)
                :json-false))))

(ert-deftest e-anthropic-test-request-body-maps-tool-call-and-result-turns ()
  "Tool-call messages become tool_use blocks; tool results become user tool_result turns."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages '((:role user :content "hello")
                  (:role tool-call
                   :content (:id "call-1"
                             :name "read"
                             :arguments (:uri "file://README.md")))
                  (:role tool
                   :content (:tool-call-id "call-1"
                             :content (:ok t))))
      :options '(:model "claude-test" :max-tokens 1024))
     :messages)
    [(:role "user"
      :content [(:type "text" :text "hello")])
     (:role "assistant"
      :content [(:type "tool_use"
                 :id "call-1"
                 :name "read"
                 :input (:uri "file://README.md"))])
     (:role "user"
     :content [(:type "tool_result"
                 :tool_use_id "call-1"
                 :content "{\"ok\":true}")])])))

(ert-deftest e-anthropic-test-request-body-pairs-late-tool-markers-in-both-projections ()
  "Full and result-only continuation projections preserve paired markers."
  (let* ((stable-one '(:role system :content "Stable instructions."))
         (stable-two '(:role system :content "Stable guidance."))
         (late-marker '(:role system
                        :content "[ephemeral context source 1, ~5 tokens]"))
         (late-source '(:role system :content "Current buffer source."))
         (marker-one
          (e-anthropic-test--tool-source-marker
           "call-one" "[ephemeral context source 2, ~8 tokens]"))
         (marker-two
          (e-anthropic-test--tool-source-marker
           "call-two" "[ephemeral context source 3, ~9 tokens]"))
         (call-one '(:role tool-call
                     :content (:id "call-one" :name "inspect-one"
                               :arguments (:path "one"))))
         (call-two '(:role tool-call
                     :content (:id "call-two" :name "inspect-two"
                               :arguments (:path "two"))))
         (result-one '(:role tool
                       :content (:tool-call-id "call-one"
                                 :content "result one")))
         (result-two
          '(:role tool
            :content (:tool-call-id "call-two" :content "result two")
            :metadata
            (:provider-replay-items
             ((:type provider-replay-item :provider-id anthropic
               :item (:type "thinking" :thinking "Inspecting both."
                      :signature "sig-markers"))
              (:type provider-replay-item :provider-id anthropic
               :item (:type "tool_use" :id "call-one"
                      :name "inspect-one" :input (:path "one")))
              (:type provider-replay-item :provider-id anthropic
               :item (:type "tool_use" :id "call-two"
                      :name "inspect-two" :input (:path "two")))))))
         (continuation-result-two
          '(:role tool
            :content (:tool-call-id "call-two" :content "result two")))
         (full-projection
          (list stable-one stable-two late-marker late-source
                '(:role user :content "Inspect both sources.")
                call-one marker-one result-one call-two marker-two result-two))
         (provider-followup-delta
          (list marker-one result-one marker-two continuation-result-two))
         (options
          '(:model "claude-test" :max-tokens 1024 :prompt-cache t
            :prompt-cache-ttl "1h"
            :segments ((:kind static-prefix :id stable-instructions
                        :fingerprint "stable-instructions-fp"
                        :messages ((:role system
                                    :content "Stable instructions.")
                                   (:role system
                                    :content "Stable guidance.")))
                       (:kind current-state :id current-buffer
                        :fingerprint "current-buffer-fp"
                        :messages ((:role system
                                    :content "[ephemeral context source 1, ~5 tokens]")
                                   (:role system
                                    :content "Current buffer source."))))))
         (tools '((:name "inspect-one" :description "Inspect one source."
                   :parameters (:type "object"))
                  (:name "inspect-two" :description "Inspect another source."
                   :parameters (:type "object"))))
         (uncached-options (copy-sequence options)))
    (cl-remf uncached-options :prompt-cache)
    (dolist (projection (list full-projection provider-followup-delta))
      (let* ((cached (e-anthropic-request-body
                      :messages projection :options options :tools tools))
             (uncached (e-anthropic-request-body
                        :messages projection :options uncached-options
                        :tools tools))
             (cached-system (plist-get cached :system))
             (cached-messages (plist-get cached :messages))
             (cached-tools (plist-get cached :tools))
             (uncached-tools (plist-get uncached :tools)))
        (should (equal (e-anthropic-test--system-text cached)
                       (e-anthropic-test--system-text uncached)))
        (should (equal (plist-get cached :messages)
                       (plist-get uncached :messages)))
        (if (eq projection full-projection)
            (let ((followup
                   (aref cached-messages (1- (length cached-messages)))))
              (should (equal cached-tools uncached-tools))
              (should
               (equal (mapcar (lambda (block) (plist-get block :text))
                              (append cached-system nil))
                      (list "Stable instructions."
                            "Stable guidance."
                            "[ephemeral context source 1, ~5 tokens]"
                            "Current buffer source.")))
              (should (equal (plist-get (aref cached-system 1) :cache_control)
                             '(:type "ephemeral" :ttl "1h")))
              (should-not (plist-member (aref cached-system 2) :cache_control))
              (should-not (plist-member (aref cached-system 3) :cache_control))
              (should
               (equal (append (plist-get followup :content) nil)
                      (list '(:type "text"
                              :text "[ephemeral context source 2, ~8 tokens]")
                            '(:type "tool_result" :tool_use_id "call-one"
                              :content "result one")
                            '(:type "text"
                              :text "[ephemeral context source 3, ~9 tokens]")
                            '(:type "tool_result" :tool_use_id "call-two"
                              :content "result two")))))
          (progn
            (should-not cached-system)
            (should
             (equal
              (e-anthropic-test--tool-definitions-without-cache-control cached)
              (e-anthropic-test--tool-definitions-without-cache-control
               uncached)))
            (should-not (plist-member (aref cached-tools 0) :cache_control))
            (should
             (equal (plist-get (aref cached-tools 1) :cache_control)
                    '(:type "ephemeral" :ttl "1h")))
            (dolist (tool (append uncached-tools nil))
              (should-not (plist-member tool :cache_control)))
            (should
             (equal
              (mapcar (lambda (message)
                        (append (plist-get message :content) nil))
                      (append cached-messages nil))
              (list (list '(:type "text"
                            :text "[ephemeral context source 2, ~8 tokens]")
                          '(:type "tool_result" :tool_use_id "call-one"
                            :content "result one"))
                    (list '(:type "text"
                            :text "[ephemeral context source 3, ~9 tokens]")
                          '(:type "tool_result" :tool_use_id "call-two"
                            :content "result two")))))))))))

(ert-deftest e-anthropic-test-request-body-rejects-detached-source-marker ()
  "A typed marker that does not immediately precede its result fails clearly."
  (should-error
   (e-anthropic-request-body
    :messages (list (e-anthropic-test--tool-source-marker
                     "call-one" "[ephemeral source]")
                    '(:role user :content "detached")
                    '(:role tool
                      :content (:tool-call-id "call-one" :content "result")))
   :options '(:model "claude-test" :max-tokens 1024))
   :type 'e-anthropic-response-invalid))

(ert-deftest e-anthropic-test-request-body-pairs-unreplayed-tool-marker-and-result ()
  "An ordinary result keeps its typed marker in the same user content array."
  (let* ((marker (e-anthropic-test--tool-source-marker
                  "call-one" "[ephemeral context source 1, ~5 tokens]"))
         (result '(:role tool
                   :content (:tool-call-id "call-one" :content "tool output")))
         (body (e-anthropic-request-body
                :messages (list marker result)
                :options '(:model "claude-test" :max-tokens 1024))))
    (should-not (plist-member body :system))
    (should
     (equal (plist-get body :messages)
            [(:role "user"
              :content [(:type "text"
                         :text "[ephemeral context source 1, ~5 tokens]")
                        (:type "tool_result" :tool_use_id "call-one"
                         :content "tool output")])]))))

(ert-deftest e-anthropic-test-request-body-ignores-call-carried-native-replay ()
  "A tool-call cannot extend provider replay beyond the settled result."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages
      '((:role tool-call
         :content (:id "call-1"
                   :name "read"
                   :arguments (:uri "file://README.md")
                   :provider-replay-items
                   ((:type provider-replay-item
                     :provider-id anthropic
                     :item (:type "thinking"
                            :thinking "Earlier thought."
                            :signature "sig-earlier"))
                    (:type provider-replay-item
                     :provider-id anthropic
                     :item (:type "tool_use" :id "call-1"
                            :name "read"
                            :input (:uri "file://README.md"))))))
        (:role tool
         :content (:tool-call-id "call-1" :content "read result")))
      :options '(:model "claude-test" :max-tokens 1024))
     :messages)
    [(:role "assistant"
      :content [(:type "tool_use" :id "call-1" :name "read"
                 :input (:uri "file://README.md"))])
     (:role "user"
      :content [(:type "tool_result" :tool_use_id "call-1"
                 :content "read result")])])))

(ert-deftest e-anthropic-test-request-body-adds-cache-control-on-system ()
  "Prompt caching attaches a cache_control breakpoint to the system block."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages '((:role system :content "Stable instructions.")
                  (:role user :content "hello"))
      :options '(:model "claude-test" :max-tokens 1024 :prompt-cache t))
     :system)
    [(:type "text"
      :text "Stable instructions."
      :cache_control (:type "ephemeral"))])))

(ert-deftest e-anthropic-test-request-body-cache-control-honors-ttl ()
  "An explicit prompt-cache-ttl is forwarded to cache_control."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages '((:role system :content "Stable instructions.")
                  (:role user :content "hello"))
      :options '(:model "claude-test" :max-tokens 1024
                 :prompt-cache t :prompt-cache-ttl "1h"))
     :system)
    [(:type "text"
      :text "Stable instructions."
      :cache_control (:type "ephemeral" :ttl "1h"))])))

(ert-deftest e-anthropic-test-request-body-cache-control-uses-segment-breakpoint ()
  "Segment-aware caching stops before current-state system context."
  (let* ((body
          (e-anthropic-request-body
           :messages '((:role system :content "Stable instructions.")
                       (:role system :content "Current buffer.")
                       (:role user :content "hello"))
           :options
           '(:model "claude-test"
             :max-tokens 1024
             :prompt-cache t
             :segments ((:kind static-prefix
                         :id stable-instructions
                         :fingerprint "stable-fp"
                         :messages ((:role system
                                     :content "Stable instructions.")))
                        (:kind current-state
                         :id current-buffer
                         :fingerprint "current-fp"
                         :messages ((:role system
                                     :content "Current buffer.")))))))
         (system (plist-get body :system))
         (messages (plist-get body :messages)))
    (should (equal system
                   [(:type "text"
                     :text "Stable instructions."
                     :cache_control (:type "ephemeral"))
                    (:type "text"
                     :text "Current buffer.")]))
    (should (equal messages
                   [(:role "user"
                     :content [(:type "text" :text "hello")])]))))

(ert-deftest e-anthropic-test-request-body-cache-prefix-excludes-dynamic-suffix ()
  "Dynamic changes leave the cached prefix stable; stable changes replace it."
  (cl-labels
      ((render (instructions suffix tool-schema)
         (let* ((stable-message '(:role system :content "Stable guidance."))
                (suffix-message (list :role 'system :content suffix))
                (body-options
                 (list :model "claude-test"
                       :max-tokens 1024
                       :instructions instructions
                       :prompt-cache t
                       :prompt-cache-ttl "5m"
                       :segments
                       (list (list :kind 'static-prefix
                                   :id 'stable-guidance
                                   :fingerprint "stable-guidance-fp"
                                   :messages (list stable-message))
                             (list :kind 'current-state
                                   :id 'dynamic-suffix
                                   :fingerprint
                                   (secure-hash 'sha256 suffix)
                                   :messages (list suffix-message))))))
           (e-anthropic-request-body
            :messages (list stable-message suffix-message
                            '(:role user :content "hello"))
            :options body-options
            :tools (list (list :name "lookup"
                               :description "Lookup."
                               :parameters tool-schema))))))
    (let* ((stable-schema
            '(:type "object" :properties (:path (:type "string"))))
           (changed-schema
            '(:type "object" :properties (:uri (:type "string"))))
           (base (render "Stable instructions." "Dynamic source A."
                         stable-schema))
           (dynamic-change (render "Stable instructions." "Dynamic source B."
                                   stable-schema))
           (instruction-change (render "Changed stable instructions."
                                       "Dynamic source A." stable-schema))
           (tool-change (render "Stable instructions." "Dynamic source A."
                                changed-schema))
           (base-system (plist-get base :system))
           (dynamic-system (plist-get dynamic-change :system)))
      (should (equal (e-anthropic-test--stable-cache-prefix base)
                     (e-anthropic-test--stable-cache-prefix dynamic-change)))
      (should (equal (aref base-system 0) (aref dynamic-system 0)))
      (should (equal (aref base-system 1) (aref dynamic-system 1)))
      (should-not (equal (aref base-system 2) (aref dynamic-system 2)))
      (should (equal (plist-get (aref base-system 1) :cache_control)
                     '(:type "ephemeral" :ttl "5m")))
      (should-not (plist-member (aref dynamic-system 2) :cache_control))
      (should-not (equal (e-anthropic-test--stable-cache-prefix base)
                         (e-anthropic-test--stable-cache-prefix
                          instruction-change)))
      (should-not (equal (e-anthropic-test--stable-cache-prefix base)
                         (e-anthropic-test--stable-cache-prefix tool-change))))))

(ert-deftest e-anthropic-test-request-body-top-level-cache-control ()
  "Top-level automatic cache mode leaves system as plain content."
  (let ((body (e-anthropic-request-body
               :messages '((:role system :content "Stable instructions.")
                           (:role user :content "hello"))
               :options '(:model "claude-test"
                          :max-tokens 1024
                          :prompt-cache t
                          :prompt-cache-mode top-level
                          :prompt-cache-ttl "1h"))))
    (should (equal (plist-get body :cache_control)
                   '(:type "ephemeral" :ttl "1h")))
    (should (equal (plist-get body :system) "Stable instructions."))))

(ert-deftest e-anthropic-test-request-body-sends-container-when-configured ()
  "Container ids are sent only when configured in turn options."
  (let ((body (e-anthropic-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "claude-test"
                          :max-tokens 1024
                          :anthropic-container-id "container-1"))))
    (should (equal (plist-get body :container) "container-1"))))

(ert-deftest e-anthropic-test-request-body-caches-tools-when-no-system ()
  "With caching enabled and no system, the breakpoint lands on the last tool."
  (let ((tools (plist-get
                (e-anthropic-request-body
                 :messages '((:role user :content "hello"))
                 :options '(:model "claude-test" :max-tokens 1024
                            :prompt-cache t)
                 :tools '((:type "function" :name "a" :description "A"
                           :parameters (:type "object"))
                          (:type "function" :name "b" :description "B"
                           :parameters (:type "object"))))
                :tools)))
    (should-not (plist-member (aref tools 0) :cache_control))
    (should (equal (plist-get (aref tools 1) :cache_control)
                   '(:type "ephemeral")))))

(ert-deftest e-anthropic-test-request-body-system-plain-without-caching ()
  "Without caching the system field stays a plain string."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages '((:role system :content "Stable instructions.")
                  (:role user :content "hello"))
      :options '(:model "claude-test" :max-tokens 1024))
     :system)
    "Stable instructions.")))

(ert-deftest e-anthropic-test-parse-text-stream ()
  "Messages SSE text events become backend-neutral stream items."
  (should
   (equal
    (e-anthropic-parse-stream
     "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"role\":\"assistant\",\"content\":[],\"usage\":{\"input_tokens\":10}}}\n\n\
event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"he\"}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"llo\"}}\n\n\
event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n")
    '((:type assistant-delta :content "he")
      (:type assistant-delta :content "llo")
      (:type assistant-message :content "hello")
      (:type token-usage
       :usage (:context-input-tokens 10
               :input-tokens 10
               :cached-input-tokens nil
               :cache-creation-input-tokens nil
               :output-tokens 5
               :reasoning-output-tokens nil
               :total-tokens 15))
      (:type done :reason stop)))))

(ert-deftest e-anthropic-test-parse-thinking-delta-as-raw-reasoning ()
  "Anthropic thinking deltas become generic raw reasoning items."
  (should
   (equal
    (e-anthropic-parse-stream
     "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"\"}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"checking\"}}\n\n\
event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n")
    '((:type reasoning-raw-delta
       :stream-kind raw
       :content "checking"
       :content-index 0)
      (:type done :reason stop)))))

(ert-deftest e-anthropic-test-parse-stream-maps-cache-tokens ()
  "Full-context input sums cache counters while preserving billing usage."
  (should
   (equal
    (seq-find (lambda (item) (eq (plist-get item :type) 'token-usage))
              (e-anthropic-parse-stream
               "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":10,\"cache_read_input_tokens\":4,\"cache_creation_input_tokens\":6}}}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"))
    '(:type token-usage
      :usage (:context-input-tokens 20
              :input-tokens 10
              :cached-input-tokens 4
              :cache-creation-input-tokens 6
              :output-tokens 5
              :reasoning-output-tokens nil
              :total-tokens 25)))))

(ert-deftest e-anthropic-test-usage-full-context-count-preserves-billing-counters ()
  "Fresh, cache-read, and cache-write input sum without changing billing fields."
  (let* ((usage (plist-get (e-anthropic--usage-item 17 5 4802 0) :usage)))
    (should (equal (plist-get usage :context-input-tokens) 4819))
    (should (equal (plist-get usage :input-tokens) 17))
    (should (equal (plist-get usage :cached-input-tokens) 4802))
    (should (equal (plist-get usage :cache-creation-input-tokens) 0))
    (should (equal (plist-get usage :output-tokens) 5))
    (should (equal (plist-get usage :total-tokens) 4824))))

(ert-deftest e-anthropic-test-emits-cache-anchor-candidate ()
  "Successful cached Anthropic responses emit a durable provider anchor candidate."
  (let (items)
    (e-anthropic--emit-response-items-with-context
     "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"
     '(:metadata (:provider anthropic
                  :model "claude-test"
                  :anthropic-cache-mode explicit
                  :anthropic-cache-breakpoint system-stable-prefix
                  :anthropic-breakpoint-segment-id "stable-instructions"
                  :anthropic-breakpoint-fingerprint "stable-fp"
                  :full-history t))
     (lambda (item) (push item items)))
    (should
     (equal
      (nreverse items)
      '((:type provider-anchor-candidate
         :provider-id anthropic
         :metadata (:provider anthropic
                    :model "claude-test"
                    :anthropic-cache-mode explicit
                    :anthropic-cache-breakpoint system-stable-prefix
                    :anthropic-breakpoint-segment-id "stable-instructions"
                    :anthropic-breakpoint-fingerprint "stable-fp"
                    :full-history t))
        (:type done :reason stop))))))

(ert-deftest e-anthropic-test-parse-non-stream-json-error ()
  "A non-stream JSON error body becomes a backend error item.
The error type is prefixed onto the content and folded into the payload so the
retry classifier sees the kind even when the message does not name it."
  (let* ((items (e-anthropic-parse-stream
                 "{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"message\":\"max_tokens is required\"}}"))
         (item (car items)))
    (should (= (length items) 1))
    (should (eq (plist-get item :type) 'backend-error))
    (should (equal (plist-get item :content)
                   "invalid_request_error: max_tokens is required"))
    (should (equal (plist-get (plist-get item :payload) :error-type)
                   "invalid_request_error"))))

(ert-deftest e-anthropic-test-parse-non-stream-overloaded-error ()
  "An `overloaded_error' leaves the adapter with normalized retry details."
  (let (items)
    (e-anthropic--emit-response-items
     "{\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\",\"message\":\"Overloaded\"}}"
     (lambda (item) (push item items)))
    (setq items (nreverse items))
    (let ((item (car items)))
      (should (eq (plist-get item :type) 'backend-error))
      (should (string-match-p "overloaded_error" (plist-get item :content)))
      (let ((details (plist-get item :payload)))
        (should (eq (plist-get details :retryable) t))
        (should (eq (plist-get details :retry-reason)
                    'provider-unavailable))))))

(ert-deftest e-anthropic-test-normalizes-provider-retry-hints ()
  "The Anthropic adapter owns transient classification and reset parsing."
  (let* ((now (float-time
               (encode-time (parse-time-string
                             "2026-07-03 08:20:00 +0000"))))
         (absolute
          (e-anthropic--retry-after-from-text
           "rate limit resets at: 2026-07-03 08:23:02 UTC"
           now)))
    (should (= absolute 182.0))
    (should (= (e-anthropic--retry-after-from-text
                "please retry after 2 minutes" now)
               120.0)))
  (dolist (case
           '(("rate_limit_error: slow down" nil rate-limit)
             ("overloaded_error: Overloaded" nil provider-unavailable)
             ("connection reset by peer" nil transport)
             ("request failed" (:status 529) provider-unavailable)))
    (pcase-let ((`(,message ,payload ,reason) case))
      (let ((details (e-anthropic--normalize-error-details
                      message payload nil)))
        (should (eq (plist-get details :retryable) t))
        (should (eq (plist-get details :retry-reason) reason)))))
  (let ((details
         (e-anthropic--normalize-error-details
          "Anthropic request timed out"
          nil
          '(e-anthropic-request-timeout "timed out"))))
    (should (eq (plist-get details :retryable) t))
    (should (eq (plist-get details :retry-reason) 'timeout)))
  (let ((details
         (e-anthropic--normalize-error-details
          "invalid request" '(:status 400) nil)))
    (should-not (eq (plist-get details :retryable) t))))

(ert-deftest e-anthropic-test-parse-non-stream-html-error ()
  "A non-stream HTML error body becomes a single backend error item."
  (let* ((items (e-anthropic-parse-stream
                 "<html><head><title>520</title></head><body><h1>Web server is returning an unknown error</h1></body></html>"))
         (item (car items)))
    (should (= (length items) 1))
    (should (eq (plist-get item :type) 'backend-error))
    (should (eq (plist-get (plist-get item :payload) :response-kind) 'html))
    (should (string-match-p "HTML" (plist-get item :content)))
    (should (string-match-p "unknown error" (plist-get item :content)))))

(ert-deftest e-anthropic-test-parse-non-stream-text-error ()
  "A non-stream, non-JSON text body becomes a single backend error item."
  (let* ((items (e-anthropic-parse-stream "upstream connect error or disconnect/reset before headers"))
         (item (car items)))
    (should (= (length items) 1))
    (should (eq (plist-get item :type) 'backend-error))
    (should (eq (plist-get (plist-get item :payload) :response-kind) 'text))
    (should (string-match-p "upstream connect error" (plist-get item :content)))))

(ert-deftest e-anthropic-test-parse-empty-body-returns-no-items ()
  "A truly empty body yields no items so the loop reports empty output."
  (should (null (e-anthropic-parse-stream "")))
  (should (null (e-anthropic-parse-stream "   \n  "))))

(ert-deftest e-anthropic-test-parse-tool-use-stream ()
  "Messages tool_use blocks accumulate input JSON into a neutral tool call."
  (should
   (equal
    (e-anthropic-parse-stream
     "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_1\",\"name\":\"inspect\",\"input\":{}}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"object\\\":{},\\\"array\\\":[],\\\"flags\\\":[false,null],\\\"items\\\":[{\\\"empty\\\":{},\\\"values\\\":[1,false]}]}\"}}\n\n\
event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n")
    '((:type tool-call
       :id "toolu_1"
       :name "inspect"
       :arguments (:object nil
                    :array []
                    :flags [:json-false :json-null]
                    :items [(:empty nil :values [1 :json-false])]))
      (:type provider-replay-item
       :provider-id anthropic
       :item (:type "tool_use"
              :id "toolu_1"
              :name "inspect"
              :input (:object nil
                      :array []
                      :flags [:json-false :json-null]
                      :items [(:empty nil :values [1 :json-false])])))
      (:type done :reason tool-use)))))

(ert-deftest e-anthropic-test-parse-joins-split-content-block-deltas-in-order ()
  "Text, thinking, and tool input survive many provider delta boundaries."
  (let* ((items
          (e-anthropic-parse-stream
           (e-anthropic-test--sse-stream
            '((:type "message_start" :message (:role "assistant" :content []))
              (:type "content_block_start" :index 0
               :content_block (:type "thinking" :thinking ""))
              (:type "content_block_delta" :index 0
               :delta (:type "thinking_delta" :thinking "Think "))
              (:type "content_block_delta" :index 0
               :delta (:type "thinking_delta" :thinking "in pieces."))
              (:type "content_block_delta" :index 0
               :delta (:type "signature_delta" :signature "split-signature"))
              (:type "content_block_stop" :index 0)
              (:type "content_block_start" :index 1
               :content_block (:type "text" :text ""))
              (:type "content_block_delta" :index 1
               :delta (:type "text_delta" :text "split "))
              (:type "content_block_delta" :index 1
               :delta (:type "text_delta" :text "text"))
              (:type "content_block_stop" :index 1)
              (:type "content_block_start" :index 2
               :content_block (:type "tool_use" :id "toolu-split"
                              :name "inspect-split" :input nil))
              (:type "content_block_delta" :index 2
               :delta (:type "input_json_delta" :partial_json "{\"path\":"))
              (:type "content_block_delta" :index 2
               :delta (:type "input_json_delta" :partial_json "\"fragmented"))
              (:type "content_block_delta" :index 2
               :delta (:type "input_json_delta" :partial_json "\"}"))
              (:type "content_block_stop" :index 2)
              (:type "message_delta" :delta (:stop_reason "tool_use"))
              (:type "message_stop")))))
         (tool-call (seq-find (lambda (item)
                                (eq (plist-get item :type) 'tool-call))
                              items))
         (replay-blocks
          (mapcar (lambda (item) (plist-get item :item))
                  (seq-filter
                   (lambda (item)
                     (eq (plist-get item :type) 'provider-replay-item))
                   items))))
    (should
     (equal (plist-get (seq-find (lambda (item)
                                   (eq (plist-get item :type)
                                       'assistant-message))
                                 items)
                       :content)
            "split text"))
    (should (equal (plist-get tool-call :arguments) '(:path "fragmented")))
    (should (equal replay-blocks
                   '((:type "thinking" :thinking "Think in pieces."
                      :signature "split-signature")
                     (:type "text" :text "split text")
                     (:type "tool_use" :id "toolu-split"
                      :name "inspect-split" :input (:path "fragmented")))))))

(ert-deftest e-anthropic-test-parse-signed-two-tool-response-retains-native-order ()
  "A complete response releases each tool call before its native replay blocks."
  (let* ((items (e-anthropic-parse-stream
                 (e-anthropic-test--sse-stream
                  e-anthropic-test--signed-two-tool-events)))
         (types (mapcar (lambda (item) (plist-get item :type)) items))
         (tool-calls (seq-filter (lambda (item)
                                   (eq (plist-get item :type) 'tool-call))
                                 items))
         (replay-items
          (seq-filter (lambda (item)
                        (eq (plist-get item :type) 'provider-replay-item))
                      items)))
    (should (equal (mapcar (lambda (item) (plist-get item :id)) tool-calls)
                   '("toolu-one" "toolu-two")))
    (should (equal (mapcar (lambda (item)
                             (plist-get (plist-get item :item) :type))
                           replay-items)
                   '("thinking" "text" "redacted_thinking"
                     "tool_use" "tool_use")))
    (should (< (cl-position 'tool-call types)
               (cl-position 'provider-replay-item types)))
    (should (equal (plist-get (car replay-items) :item)
                   '(:type "thinking"
                     :thinking "Checking both paths."
                     :signature "sig-native-0")))
    (should (eq (plist-get (car (last items)) :type) 'done))))

(ert-deftest e-anthropic-test-parse-rejects-incomplete-native-tool-responses ()
  "Missing terminal, signed material, or contiguous indices release no tools."
  (e-anthropic-test--assert-invalid-tool-response
   (butlast e-anthropic-test--signed-two-tool-events))
  (e-anthropic-test--assert-invalid-tool-response
   (seq-remove (lambda (event)
                 (equal (plist-get (plist-get event :delta) :type)
                        "signature_delta"))
               e-anthropic-test--signed-two-tool-events))
  (e-anthropic-test--assert-invalid-tool-response
   (mapcar (lambda (event)
             (if (and (equal (plist-get event :type) "content_block_start")
                      (= (or (plist-get event :index) -1) 3))
                 (plist-put (copy-sequence event) :index 5)
               event))
           e-anthropic-test--signed-two-tool-events)))

(ert-deftest e-anthropic-test-parse-rejects-signature-before-thinking-text ()
  "A signature delta cannot precede the thinking content it signs."
  (let* ((events (copy-tree e-anthropic-test--signed-two-tool-events))
         (signature (seq-find
                     (lambda (event)
                       (equal (plist-get (plist-get event :delta) :type)
                              "signature_delta"))
                     events))
         (without-signature (delq signature events))
         (thinking-delta-index
          (cl-position-if
           (lambda (event)
             (equal (plist-get (plist-get event :delta) :type)
                    "thinking_delta"))
           without-signature)))
    (e-anthropic-test--assert-invalid-tool-response
     (append (cl-subseq without-signature 0 thinking-delta-index)
             (list signature)
             (cl-subseq without-signature thinking-delta-index)))))

(ert-deftest e-anthropic-test-parse-max-tokens-stop-is-surfaced ()
  "A truncated turn surfaces a distinct max-tokens done reason."
  (should
   (equal
    (e-anthropic-parse-stream
     "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Writing now.\"}}\n\n\
event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"max_tokens\"}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n")
    '((:type assistant-delta :content "Writing now.")
      (:type assistant-message :content "Writing now.")
      (:type done :reason max-tokens)))))

(ert-deftest e-anthropic-test-messages-url-appends-messages-path ()
  "Messages providers append /messages unless the base URL already has it."
  (should (equal (e-anthropic-messages-url "https://gateway.example.test/v1")
                 "https://gateway.example.test/v1/messages"))
  (should (equal (e-anthropic-messages-url "https://gateway.example.test/v1/")
                 "https://gateway.example.test/v1/messages"))
  (should (equal (e-anthropic-messages-url
                  "https://gateway.example.test/v1/messages")
                 "https://gateway.example.test/v1/messages")))

(ert-deftest e-anthropic-test-request-context-uses-bearer-auth ()
  "Bearer providers send x-api-key and anthropic-version headers."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (context (e-anthropic--request-context
                   :provider 'eng-anthropic
                   :messages '((:role user :content "hello"))
                   :options '(:model "claude-test" :max-tokens 1024))))
    (should (equal (plist-get context :url)
                   "https://gateway.example.test/v1/messages"))
    (should (equal (cdr (assoc "x-api-key" (plist-get context :headers)))
                   "test-token"))
    (should (equal (cdr (assoc "anthropic-version" (plist-get context :headers)))
                   e-anthropic-version))
    (should (assoc "Content-Type" (plist-get context :headers)))
    (should (equal (e-json-parse-string (plist-get context :body))
                   '(:model "claude-test"
                     :max_tokens 1024
                     :stream t
                     :messages [(:role "user"
                                 :content [(:type "text" :text "hello")])]
                     :thinking (:type "adaptive")
                     :output_config (:effort "high"))))))

(ert-deftest e-anthropic-test-request-context-reports-cache-metadata ()
  "Anthropic request metadata explains cache placement without omitting history."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (context
          (e-anthropic--request-context
           :provider 'eng-anthropic
           :messages '((:role system :content "Stable instructions.")
                       (:role system :content "Current buffer.")
                       (:role user :content "hello"))
           :options
           '(:model "claude-test"
             :max-tokens 1024
             :effort "low"
             :prompt-cache t
             :prompt-cache-ttl "1h"
             :anthropic-container-id "container-1"
             :tools ((:name "lookup"
                      :description "Lookup."
                      :parameters (:type "object")))
             :segments ((:kind static-prefix
                         :id stable-instructions
                         :fingerprint "stable-fp"
                         :messages ((:role system
                                     :content "Stable instructions.")))
                        (:kind current-state
                         :id current-buffer
                         :fingerprint "current-fp"
                         :messages ((:role system
                                     :content "Current buffer.")))))))
         (metadata (plist-get context :metadata)))
    (should (equal (plist-get metadata :anthropic-cache-mode) 'explicit))
    (should (equal (plist-get metadata :anthropic-cache-breakpoint)
                   'system-stable-prefix))
    (should (equal (plist-get metadata :anthropic-breakpoint-segment-id)
                   "stable-instructions"))
    (should (equal (plist-get metadata :anthropic-breakpoint-fingerprint)
                   "stable-fp"))
    (should (equal (plist-get metadata :anthropic-cache-ttl) "1h"))
    (should (equal (plist-get metadata :anthropic-container-id)
                   "container-1"))
    (should (equal (plist-get metadata :provider) 'anthropic))
    (should (equal (plist-get metadata :model) "claude-test"))
    (should (equal (plist-get metadata :segment-fingerprints)
                   '("stable-fp" "current-fp")))
    (should (equal (plist-get metadata :anthropic-beta-headers) nil))
    (should (eq (plist-get metadata :full-history) t))
    (should (= (plist-get metadata :segment-fingerprint-count) 2))
    (should (equal (plist-get metadata :diagnostics)
                   '(:model "claude-test"
                     :effort "low"
                     :max-tokens 1024
                     :prompt-cache t
                     :anthropic-cache-mode explicit
                     :anthropic-cache-breakpoint system-stable-prefix
                     :anthropic-cache-ttl "1h"
                     :anthropic-container-id-present t
                     :input-message-count 1
                     :tool-count 1)))))

(ert-deftest e-anthropic-test-request-context-sends-gated-context-management ()
  "Raw Anthropic context_management is sent only with explicit beta headers."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY"
             :context-management (:edits [(:type "clear_tool_results")])
             :beta-headers ("context-management-test"))))
         (context
          (e-anthropic--request-context
           :provider 'eng-anthropic
           :messages '((:role user :content "hello"))
           :options '(:model "claude-test" :max-tokens 1024)))
         (body (e-json-parse-string (plist-get context :body)))
         (metadata (plist-get context :metadata)))
    (should (equal (plist-get body :context_management)
                   '(:edits [(:type "clear_tool_results")])))
    (should (equal (cdr (assoc "anthropic-beta" (plist-get context :headers)))
                   "context-management-test"))
    (should (equal (plist-get metadata :anthropic-context-management)
                   'requested))
    (should (equal (plist-get metadata :anthropic-beta-headers)
                   '("context-management-test")))))

(ert-deftest e-anthropic-test-request-context-omits-unused-beta-headers ()
  "Anthropic beta headers are sent only for active context-management requests."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY"
             :beta-headers ("context-management-test"))))
         (context
          (e-anthropic--request-context
           :provider 'eng-anthropic
           :messages '((:role user :content "hello"))
           :options '(:model "claude-test" :max-tokens 1024)))
         (body (e-json-parse-string (plist-get context :body)))
         (metadata (plist-get context :metadata)))
    (should-not (assoc "anthropic-beta" (plist-get context :headers)))
    (should-not (plist-member body :context_management))
    (should-not (plist-member metadata :anthropic-context-management))
    (should-not (plist-member metadata :anthropic-beta-headers))))

(ert-deftest e-anthropic-test-request-context-reports-top-level-cache-metadata ()
  "Top-level cache mode is visible in sanitized request metadata."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (context
          (e-anthropic--request-context
           :provider 'eng-anthropic
           :messages '((:role user :content "hello"))
           :options '(:model "claude-test"
                      :max-tokens 1024
                      :prompt-cache t
                      :prompt-cache-mode top-level)))
         (metadata (plist-get context :metadata)))
    (should (equal (plist-get metadata :anthropic-cache-mode) 'top-level))
    (should (equal (plist-get metadata :anthropic-cache-breakpoint)
                   'provider-managed))
    (should (eq (plist-get metadata :full-history) t))))

(ert-deftest e-anthropic-test-request-context-authorization-header ()
  "Bearer providers can send Authorization: Bearer instead of x-api-key."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((gw
             :name "Gateway"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :auth-header authorization
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (context (e-anthropic--request-context
                   :provider 'gw
                   :messages '((:role user :content "hi"))
                   :options '(:model "claude-test" :max-tokens 8))))
    (should (equal (cdr (assoc "Authorization" (plist-get context :headers)))
                   "Bearer test-token"))
    (should-not (assoc "x-api-key" (plist-get context :headers)))
    (should (assoc "anthropic-version" (plist-get context :headers)))))

(ert-deftest e-anthropic-test-request-context-prefixes-model ()
  "Provider model prefixes are applied to the request model id."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((bedrockish
             :name "Bedrock-ish"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY"
             :model-prefix "anthropic.")))
         (context (e-anthropic--request-context
                   :provider 'bedrockish
                   :messages '((:role user :content "hi"))
                   :options '(:model "claude-opus-4-8" :max-tokens 8))))
    (should (equal (plist-get (e-json-parse-string (plist-get context :body))
                              :model)
                   "anthropic.claude-opus-4-8"))))

(ert-deftest e-anthropic-test-sigv4-auth-is-not-yet-supported ()
  "Selecting a SigV4 provider signals a clear unsupported error."
  (let ((e-anthropic-model-providers
         '((bedrock
            :name "Amazon Bedrock"
            :base-url "https://bedrock-runtime.example.test"
            :auth sigv4))))
    (should-error
     (e-anthropic--request-context
      :provider 'bedrock
      :messages '((:role user :content "hi"))
      :options '(:model "claude-test" :max-tokens 8))
     :type 'e-anthropic-unsupported)))

(defconst e-anthropic-test--text-stream
  "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"gateway answer\"}}\n\n\
event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"
  "A minimal Messages text stream used by integration tests.")

(ert-deftest e-anthropic-test-backend-streams-through-injected-requester ()
  "The Anthropic backend streams parsed events from an injected requester."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (seen nil)
         (captured nil)
         (backend
          (e-anthropic-backend-create
           :provider 'eng-anthropic
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (setq captured (list :url url :headers headers :body body))
              e-anthropic-test--text-stream)))))
    (e-backend-stream-batch backend
                      :messages '((:role user :content "hello"))
                      :options '(:model "claude-test" :max-tokens 1024)
                      :on-item (lambda (item) (push item seen)))
    (should (equal (nreverse seen)
                   '((:type assistant-delta :content "gateway answer")
                     (:type assistant-message :content "gateway answer")
                     (:type done :reason stop))))
    (should (equal (plist-get captured :url)
                   "https://gateway.example.test/v1/messages"))
    (should (equal (cdr (assoc "x-api-key" (plist-get captured :headers)))
                   "test-token"))))

(ert-deftest e-anthropic-test-multiple-tool-rounds-group-each-native-response ()
  "Each tool round replays its native response once and stays ephemeral."
  (let* ((directory (make-temp-file "e-anthropic-replay-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "anthropic-replay")
         reopened
         (process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (request-count 0)
         (captured-bodies nil)
         (backend
          (e-anthropic-backend-create
           :provider 'eng-anthropic
           :request-function
           (cl-function
            (lambda (&key body &allow-other-keys)
              (cl-incf request-count)
              (push (e-json-parse-string body) captured-bodies)
              (pcase request-count
                (1 (e-anthropic-test--sse-stream
                    e-anthropic-test--signed-two-tool-events))
                (2 (e-anthropic-test--sse-stream
                    e-anthropic-test--signed-follow-up-tool-events))
                (_ e-anthropic-test--text-stream))))))
         (tools (e-tools-registry-create)))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (dolist (name '("inspect-one" "inspect-two" "inspect-three"))
            (e-tools-test-register
             tools
             :name name
             :description (format "Run %s." name)
             :parameters '(:type "object"
                           :properties (:path (:type "string")))
             :handler (lambda (arguments)
                        (format "result %s" (plist-get arguments :path)))))
          (e-loop-run-turn-batch
           :session-id session-id
           :turn-id "turn-native-replay"
           :messages '((:role user :content "Inspect both paths."))
           :backend backend
           :tools tools
           :options '(:model "claude-test" :max-tokens 1024)
           :on-event #'ignore
           :append-message
           (lambda (message)
             (e-session-append-message store session-id message)))
          (e-session-flush-write-queue store)
          (e-session-sqlite-store-close store)
          (setq reopened (e-session-persistent-store-create directory))
          (let* ((bodies (nreverse captured-bodies))
                 (first-follow-up-messages
                  (plist-get (cadr bodies) :messages))
                 (second-follow-up-messages
                  (plist-get (nth 2 bodies) :messages))
                 (expected-assistant
                  '(:role "assistant"
                    :content [(:type "thinking"
                               :thinking "Checking both paths."
                               :signature "sig-native-0")
                              (:type "text"
                               :text "First, inspect both.")
                              (:type "redacted_thinking"
                               :data "redacted-native-2")
                              (:type "tool_use" :id "toolu-one"
                               :name "inspect-one" :input (:path "one"))
                              (:type "tool_use" :id "toolu-two"
                               :name "inspect-two" :input (:path "two"))]))
                 (expected-results
                  '(:role "user"
                    :content [(:type "tool_result"
                               :tool_use_id "toolu-one"
                               :content "result one")
                              (:type "tool_result"
                               :tool_use_id "toolu-two"
                               :content "result two")]))
                 (expected-second-assistant
                  '(:role "assistant"
                    :content [(:type "thinking"
                               :thinking "Checking the third path."
                               :signature "sig-native-next-0")
                              (:type "tool_use" :id "toolu-three"
                               :name "inspect-three" :input (:path "three"))]))
                 (expected-second-results
                  '(:role "user"
                    :content [(:type "tool_result"
                               :tool_use_id "toolu-three"
                               :content "result three")]))
                 (reopened-records
                  (e-session-storage-read-session-records reopened session-id))
                 (reopened-messages
                  (mapcar
                   (lambda (record)
                     (plist-get (e-session-codec-decode-record record) :message))
                   (seq-filter
                    (lambda (record)
                      (equal (plist-get record :type) "message"))
                    reopened-records)))
                 (later-body
                  (e-anthropic-request-body
                   :messages
                   (append reopened-messages
                           '((:role user :content "A later user turn.")))
                   :options '(:model "claude-test" :max-tokens 1024))))
            (should (= request-count 3))
            (should (= (length bodies) 3))
            (should
             (equal (append first-follow-up-messages nil)
                    (list
                     '(:role "user"
                       :content [(:type "text"
                                  :text "Inspect both paths.")])
                     expected-assistant
                     expected-results)))
            (should
             (equal (append second-follow-up-messages nil)
                    (list
                     '(:role "user"
                       :content [(:type "text"
                                  :text "Inspect both paths.")])
                     expected-assistant
                     expected-results
                     expected-second-assistant
                     expected-second-results)))
            (should-not
             (seq-some (lambda (message)
                         (string-match-p
                          "provider-replay-items\\|sig-native-0\\|redacted-native-2\\|sig-native-next-0"
                          (format "%S" message)))
                       reopened-messages))
            (should-not
             (string-match-p
              "provider-replay-items\\|sig-native-0\\|redacted-native-2\\|sig-native-next-0"
              (format "%S" reopened-records)))
            (should-not
             (string-match-p
              "sig-native-0\\|redacted-native-2\\|sig-native-next-0"
              (e-json-serialize later-body)))))
      (ignore-errors (e-session-sqlite-store-close store))
      (when reopened
        (ignore-errors (e-session-sqlite-store-close reopened)))
      (delete-directory directory t))))

(ert-deftest e-anthropic-test-cancelled-partial-response-discards-late-tools ()
  "Cancelling an in-flight Messages response discards its native tool replay."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (e-anthropic-request-timeout-seconds nil)
         (tools (e-tools-registry-create))
         (harness
          (e-harness-create
           :backend (e-anthropic-backend-create :provider 'eng-anthropic)))
         (request-buffer nil)
         (url-callback nil)
         (tool-runs 0)
         (parse-count 0)
         (parse-function (symbol-function 'e-anthropic-parse-stream)))
    (e-tools-test-register
     tools
     :name "inspect-one"
     :description "Inspect a path."
     :parameters '(:type "object"
                   :properties (:path (:type "string")))
     :handler (lambda (_arguments)
                (cl-incf tool-runs)
                "result"))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (_url callback &rest _args)
                     (setq request-buffer
                           (generate-new-buffer " *e-anthropic-cancel*"))
                     (setq url-callback callback)
                     request-buffer))
                  ((symbol-function 'e-anthropic-parse-stream)
                   (lambda (response)
                     (cl-incf parse-count)
                     (funcall parse-function response)))
                  ((symbol-function 'e-harness-tools)
                   (lambda (_harness &optional _session-id _turn-id)
                     tools)))
          (e-harness-create-session harness :id "anthropic-cancel")
          (e-harness-test-prompt-async
           harness "anthropic-cancel" "Inspect both paths.")
          (should (e-anthropic-test--wait-until
                   (lambda () url-callback) 1.0))
          (with-current-buffer request-buffer
            (insert "HTTP/1.1 200 OK\n\n"
                    (e-anthropic-test--sse-stream
                     (butlast e-anthropic-test--signed-two-tool-events))))
          (should (e-harness-test-abort harness "anthropic-cancel"))
          (should
           (eq (plist-get (e-harness-wait-batch harness "anthropic-cancel" 0.5)
                          :status)
               'cancelled))
          ;; A transport completion already queued when cancellation runs must
          ;; not parse or release the now-complete response.
          (let ((late-buffer
                 (generate-new-buffer " *e-anthropic-cancel-late*")))
            (unwind-protect
                (with-current-buffer late-buffer
                  (insert "HTTP/1.1 200 OK\n\n"
                          (e-anthropic-test--sse-stream
                           e-anthropic-test--signed-two-tool-events))
                  (funcall url-callback nil))
              (when (buffer-live-p late-buffer)
                (kill-buffer late-buffer))))
          (should (= parse-count 0))
          (should (= tool-runs 0))
          (should-not
           (seq-some (lambda (message)
                       (eq (plist-get message :role) 'tool-call))
                     (e-harness-messages harness "anthropic-cancel"))))
      (ignore-errors (e-harness-test-abort harness "anthropic-cancel"))
      (when (buffer-live-p request-buffer)
        (kill-buffer request-buffer)))))

(ert-deftest e-anthropic-test-harness-streams-prompt-flow ()
  "The Anthropic harness helper runs prompt to persisted assistant message."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY"
             :default-model "claude-default")))
         (harness
          (e-anthropic-create-harness
           :provider 'eng-anthropic
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers body)
              e-anthropic-test--text-stream)))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant)))
    (should (equal (plist-get (cadr (e-harness-messages harness "session-1"))
                              :content)
                   "gateway answer"))))

(ert-deftest e-anthropic-test-create-harness-default-options ()
  "The Anthropic harness seeds model, max-tokens, and effort defaults."
  (let ((e-anthropic-model-providers
         '((eng-anthropic
            :name "Engineering Anthropic"
            :base-url "https://gateway.example.test/v1"
            :auth bearer
            :env-key "ANTHROPIC_GATEWAY_KEY"
            :default-model "claude-default"))))
    (should (equal (e-harness-default-options
                    (e-anthropic-create-harness
                     :provider 'eng-anthropic
                     :request-function #'ignore))
                   (list :model "claude-default"
                         :max-tokens e-anthropic-default-max-tokens
                         :effort e-anthropic-default-effort)))))

(defconst e-anthropic-test--model-catalog-json
  (concat
   "{\"data\":["
   "{\"id\":\"claude-sonnet-5\",\"object\":\"model\","
   "\"max_input_tokens\":1000000,\"max_output_tokens\":64000},"
   "{\"id\":\"claude-opus-4-8\",\"object\":\"model\","
   "\"max_input_tokens\":1000000,\"max_output_tokens\":128000},"
   "{\"id\":\"claude-haiku-4-5-20251001\",\"object\":\"model\","
   "\"max_input_tokens\":200000}"
   "]}")
  "A representative gateway `/models' payload for tests.")

(ert-deftest e-anthropic-test-models-url-is-sibling-to-messages ()
  "The model catalog is addressed below the provider's API base URL."
  (should (equal (e-anthropic--models-url "https://gateway.test/v1")
                 "https://gateway.test/v1/models"))
  (should (equal (e-anthropic--models-url "https://gateway.test/v1/")
                 "https://gateway.test/v1/models")))

(ert-deftest e-anthropic-test-model-catalog-rejects-no-model-ids ()
  "A catalog without valid model IDs is a failed lookup."
  (should-error (e-anthropic--context-window-table-from-json "{\"data\":[]}")
                :type 'e-anthropic-backend-error))

(ert-deftest e-anthropic-test-model-catalog-retains-unknown-window-models ()
  "A valid listed model remains usable when the gateway omits its limit."
  (let* ((table
          (e-anthropic--context-window-table-from-json
           "{\"data\":[{\"id\":\"claude-opus-5-5\"}]}"))
         (missing 'missing))
    (should (eq (gethash "claude-opus-5-5" table missing) nil))
    (should-not (eq (gethash "claude-opus-5-5" table missing) missing))))

(ert-deftest e-anthropic-test-refresh-context-window-cache-accepts-unknown-limit ()
  "A listed model without a limit does not fail the asynchronous refresh."
  (e-anthropic-reset-context-window-cache)
  (let (done catalog)
    (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
              ((symbol-function 'e-anthropic--http-get-start)
               (lambda (&rest args)
                 (funcall (plist-get args :on-complete)
                          "{\"data\":[{\"id\":\"claude-opus-5-5\"}]}")
                 (e-backend-request-create :metadata '(:test immediate)))))
      (should
       (e-backend-request-p
        (e-anthropic-refresh-context-window-cache
         :on-done (lambda (table)
                    (setq done t
                          catalog table)))))
      (should done)
      (should-not (e-anthropic-context-window "claude-opus-5-5"))
      (should (eq (gethash "claude-opus-5-5" catalog 'missing) nil))
      (should-not (eq (gethash "claude-opus-5-5" catalog 'missing)
                      'missing)))
    (e-anthropic-reset-context-window-cache)))

(ert-deftest e-anthropic-test-context-window-cache-only-before-refresh ()
  "Context-window lookup does not fetch the gateway catalog synchronously."
  (e-anthropic-reset-context-window-cache)
  (let ((calls 0))
    (cl-letf (((symbol-function 'e-anthropic--http-get-start)
               (lambda (&rest _)
                 (cl-incf calls)
                 (error "cache-only lookup must not start transport"))))
      (should-not (e-anthropic-context-window "claude-opus-4-8"))
      (should (= calls 0))))
  (e-anthropic-reset-context-window-cache))

(ert-deftest e-anthropic-test-sync-http-request-rejects-hot-path-before-start ()
  "The synchronous Messages HTTP wrapper fails before starting transport."
  (let (started)
    (cl-letf (((symbol-function 'e-anthropic--http-request-start)
               (lambda (&rest _args)
                 (setq started t)
                 (error "transport should not start"))))
      (let ((err (should-error
                  (e-request-with-hot-path 'anthropic-sync-http
                    (e-anthropic--http-request
                     :url "https://example.test/messages"
                     :headers nil
                     :body "{}"))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err)
                       '(e-anthropic--http-request anthropic-sync-http))))
      (should-not started))))

(ert-deftest e-anthropic-test-sync-backend-stream-rejects-hot-path-before-request ()
  "The provider sync stream wrapper fails before issuing sync requests."
  (let ((backend (e-anthropic-backend-create
                  :request-function
                  (lambda (&rest _args)
                    (error "request function should not run")))))
    (let ((err (should-error
                (e-request-with-hot-path 'anthropic-stream
                  (funcall (e-backend--stream backend)
                           :messages nil
                           :options nil
                           :on-item #'ignore))
                :type 'e-request-blocking-call-in-hot-path)))
      (should (equal (cdr err)
                     '(e-anthropic-backend-stream anthropic-stream))))))

(ert-deftest e-anthropic-test-refresh-context-window-cache-populates-catalog ()
  "Async context-window refresh reads max-input-tokens from the gateway catalog."
  (e-anthropic-reset-context-window-cache)
  (let ((calls 0)
        done)
    (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
              ((symbol-function 'e-anthropic--http-get-start)
               (lambda (&rest args)
                 (cl-incf calls)
                 (funcall (plist-get args :on-complete)
                          e-anthropic-test--model-catalog-json)
                 (e-backend-request-create :metadata '(:test immediate)))))
      (should (e-backend-request-p
               (e-anthropic-refresh-context-window-cache
                :on-done (lambda (_table) (setq done t)))))
      (should done)
      (should (equal (e-anthropic-context-window "claude-sonnet-5") 1000000))
      (should (equal (e-anthropic-context-window "claude-opus-4-8") 1000000))
      (should (equal (e-anthropic-context-window "claude-haiku-4-5-20251001")
                     200000))
      ;; Unknown model -> nil (no static fallback).
      (should-not (e-anthropic-context-window "no-such-model"))
      (should (= calls 1))))
  (e-anthropic-reset-context-window-cache))

(ert-deftest e-anthropic-test-refresh-context-window-cache-dedupes-inflight ()
  "A provider has at most one in-flight context-window refresh."
  (e-anthropic-reset-context-window-cache)
  (let (complete
        (calls 0))
    (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
              ((symbol-function 'e-anthropic--http-get-start)
               (lambda (&rest args)
                 (cl-incf calls)
                 (setq complete (plist-get args :on-complete))
                 (e-backend-request-create :metadata '(:test deferred)))))
      (let ((first (e-anthropic-refresh-context-window-cache))
            (second (e-anthropic-refresh-context-window-cache)))
        (should (eq first second))
        (should (= calls 1))
        (funcall complete e-anthropic-test--model-catalog-json)
        (should (equal (e-anthropic-context-window "claude-opus-4-8")
                       1000000)))))
  (e-anthropic-reset-context-window-cache))

(ert-deftest e-anthropic-test-refresh-context-window-cache-reports-errors ()
  "Refresh failures report errors and leave the cache empty."
  (e-anthropic-reset-context-window-cache)
  (let (error)
    (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
              ((symbol-function 'e-anthropic--http-get-start)
               (lambda (&rest args)
                 (funcall (plist-get args :on-error)
                          '(e-anthropic-backend-error "boom"))
                 (e-backend-request-create :metadata '(:test error)))))
      (e-anthropic-refresh-context-window-cache
       :on-error (lambda (err) (setq error err)))
      (should (equal error '(e-anthropic-backend-error "boom")))
      (should-not (e-anthropic-context-window "claude-opus-4-8"))))
  (e-anthropic-reset-context-window-cache))

(ert-deftest e-anthropic-test-reset-context-window-cache-cancels-refresh ()
  "Resetting the context-window cache cancels in-flight refreshes."
  (e-anthropic-reset-context-window-cache)
  (let (cancelled)
    (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
              ((symbol-function 'e-anthropic--http-get-start)
               (lambda (&rest _)
                 (e-backend-request-create
                  :cancel (lambda () (setq cancelled t))
                  :metadata '(:test cancellable)))))
      (e-anthropic-refresh-context-window-cache)
      (e-anthropic-reset-context-window-cache)
      (should cancelled)))
  (e-anthropic-reset-context-window-cache))

(ert-deftest e-anthropic-test-context-window-failure-is-negative-cached ()
  "A failed async refresh is remembered, so repeated refreshes query once."
  (e-anthropic-reset-context-window-cache)
  (let ((calls 0)
        (e-anthropic-context-window-retry-cooldown 60))
    (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
              ((symbol-function 'e-anthropic--http-get-start)
               (lambda (&rest args)
                 (cl-incf calls)
                 (funcall (plist-get args :on-error)
                          '(e-anthropic-backend-error "boom"))
                 (e-backend-request-create :metadata '(:test error)))))
      (should (e-backend-request-p
               (e-anthropic-refresh-context-window-cache)))
      (should-not (e-anthropic-refresh-context-window-cache))
      (should-not (e-anthropic-refresh-context-window-cache))
      (should (= calls 1))))
  (e-anthropic-reset-context-window-cache))

(ert-deftest e-anthropic-test-context-window-retries-after-cooldown ()
  "Once the failure cooldown elapses, the next refresh re-queries the gateway."
  (e-anthropic-reset-context-window-cache)
  (let ((calls 0)
        (e-anthropic-context-window-retry-cooldown 60))
    (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
              ((symbol-function 'e-anthropic--http-get-start)
               (lambda (&rest args)
                 (cl-incf calls)
                 (funcall (plist-get args :on-error)
                          '(e-anthropic-backend-error "boom"))
                 (e-backend-request-create :metadata '(:test error)))))
      (should (e-backend-request-p
               (e-anthropic-refresh-context-window-cache)))
      (should (= calls 1))
      ;; Backdate the recorded failure beyond the cooldown window.
      (puthash 'gateway (- (float-time) 120)
               e-anthropic--context-window-failure-cache)
      (should (e-backend-request-p
               (e-anthropic-refresh-context-window-cache)))
      (should (= calls 2))))
  (e-anthropic-reset-context-window-cache))

(ert-deftest e-anthropic-test-context-window-cooldown-zero-disables-negative-cache ()
  "A zero cooldown retries every asynchronous refresh."
  (e-anthropic-reset-context-window-cache)
  (let ((calls 0)
        (e-anthropic-context-window-retry-cooldown 0))
    (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
              ((symbol-function 'e-anthropic--http-get-start)
               (lambda (&rest args)
                 (cl-incf calls)
                 (funcall (plist-get args :on-error)
                          '(e-anthropic-backend-error "boom"))
                 (e-backend-request-create :metadata '(:test error)))))
      (should (e-backend-request-p
               (e-anthropic-refresh-context-window-cache)))
      (should (e-backend-request-p
               (e-anthropic-refresh-context-window-cache)))
      (should (= calls 2))))
  (e-anthropic-reset-context-window-cache))

(ert-deftest e-anthropic-test-http-request-idle-timeout-fires-without-data ()
  "An HTTP request that never receives data fails after the idle timeout."
  (let ((e-anthropic-request-timeout-seconds 0.05)
        (buffer nil)
        (error-count 0)
        (complete-count 0)
        error)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url _callback &rest _args)
                 (setq buffer (generate-new-buffer " *e-anthropic-test-http*"))
                 buffer)))
      (e-anthropic--http-request-start
       :url "https://example.test/v1/messages"
       :headers '(("x-api-key" . "test"))
       :body "{}"
       :on-complete (lambda (_value) (cl-incf complete-count))
       :on-error (lambda (err)
                   (cl-incf error-count)
                   (setq error err)))
      (should (e-anthropic-test--wait-until (lambda () error) 0.5))
      (should (eq (car error) 'e-anthropic-request-timeout))
      (should (= error-count 1))
      (should (= complete-count 0))
      (should-not (buffer-live-p buffer)))))

(ert-deftest e-anthropic-test-http-request-idle-timeout-rearms-on-data ()
  "Streamed data re-arms the idle timer so a long healthy stream is not killed."
  (let ((e-anthropic-request-timeout-seconds 0.1)
        (buffer nil)
        (error-count 0)
        (complete-count 0)
        error)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url _callback &rest _args)
                 (setq buffer (generate-new-buffer " *e-anthropic-test-http*"))
                 buffer)))
      (e-anthropic--http-request-start
       :url "https://example.test/v1/messages"
       :headers '(("x-api-key" . "test"))
       :body "{}"
       :on-complete (lambda (_value) (cl-incf complete-count))
       :on-error (lambda (err)
                   (cl-incf error-count)
                   (setq error err)))
      ;; Feed data across several intervals, each shorter than the idle
      ;; timeout; the whole loop outlasts a single idle window.  A total
      ;; wall-clock timeout would have fired here; the idle timer must not.
      (dotimes (_ 5)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "data chunk\n")))
        (accept-process-output nil 0.06))
      (should-not error)
      (should (= error-count 0))
      ;; Once data stops, the idle timer eventually fires.
      (should (e-anthropic-test--wait-until (lambda () error) 0.5))
      (should (eq (car error) 'e-anthropic-request-timeout)))))

(provide 'e-anthropic-test)

;;; e-anthropic-test.el ends here
