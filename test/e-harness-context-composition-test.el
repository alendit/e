;;; e-harness-context-composition-test.el --- Public harness context composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Prompt context strategy and bounded context-preview scenarios.

;;; Code:

(require 'ert)
(load (expand-file-name "e-harness-composition-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-harness-test-prompt-uses-context-strategy ()
  "Prompting delegates backend message construction to the context strategy."
  (let* ((captured-messages nil)
         (backend (e-backend-create
                   :name "capture"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore options)
                              (setq captured-messages messages)
                              (funcall on-item
                                       '(:type assistant-message :content "ok"))
                              (funcall on-item '(:type done :reason stop))))))
         (context-strategy
          (e-context-create
           :name 'test-context
           :build (cl-function
                   (lambda (&key sessions session-id options)
                     (ignore sessions session-id options)
                     '(:strategy test-context
                       :messages ((:role user :content "from context"))
                       :options (:model "context-model"))))))
         (harness (e-harness-create
                   :backend backend
                   :context-strategy context-strategy)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "raw prompt")
    (should (equal captured-messages
                   '((:role user :content "from context"))))))

(ert-deftest e-harness-test-context-builds-current-session-preview ()
  "Context preview returns the same messages and options a turn would use."
  (let* ((provider (e-context-provider-create
                    :name 'test-provider
                    :build (cl-function
                            (lambda (&key harness session-id turn-id
                                          context-purpose)
                              (ignore harness session-id turn-id
                                      context-purpose)
                              '((:role system :content "provider context"))))))
         (capability (e-capability-create
                      :id 'test-capability
                      :instructions "capability instructions"
                      :context-providers (list provider)))
         (layer (e-layer-create
                 :id 'test-layer
                 :name "Test Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :default-options '(:model "default-model")
                   :intrinsic-capabilities (e-layer-capabilities layer))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-set-session-model harness "session-1" "session-model")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "hello"))
    (let ((context (e-harness-context harness "session-1")))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
                             (plist-get context :messages))
                     '("capability instructions" "provider context" "hello")))
      (should (equal (plist-get (plist-get context :options) :model)
                     "session-model")))))

(ert-deftest e-harness-test-turn-context-uses-explicit-turn-purpose ()
  "Turn context is a distinct correctness-critical context purpose."
  (let (captured)
    (cl-letf (((symbol-function 'e-harness-context)
               (lambda (harness session-id turn-id context-purpose)
                 (setq captured
                       (list :harness harness
                             :session-id session-id
                             :turn-id turn-id
                             :context-purpose context-purpose))
                 '(:messages nil :options nil))))
      (should (equal (e-harness-turn-context 'harness "session-1" "turn-1")
                     '(:messages nil :options nil))))
    (should (equal captured
                   '(:harness harness
                     :session-id "session-1"
                     :turn-id "turn-1"
                     :context-purpose turn)))))

(ert-deftest e-harness-test-prompt-async-builds-turn-context-purpose ()
  "Prompt startup builds correctness-critical turn context explicitly."
  (let* ((backend (e-backend-create
                   :name 'turn-context
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (ignore messages options)
                      (funcall on-item
                               '(:type assistant-message :content "Answer."))
                      (funcall on-item '(:type done :reason stop))))))
         (harness (e-harness-create :backend backend))
         (original-context (symbol-function 'e-harness-context))
         (purposes nil))
    (e-harness-create-session harness :id "session-1")
    (cl-letf (((symbol-function 'e-harness-context)
               (lambda (&rest args)
                 (push (nth 3 args) purposes)
                 (apply original-context args))))
      (e-harness-test-prompt-async harness "session-1" "hello")
      (should (equal (plist-get (e-harness-wait-batch harness "session-1" 1.0)
                                :status)
                     'done)))
    (should (member 'turn purposes))
    (should-not (member nil purposes))
    (should-not (member 'status purposes))))

(ert-deftest e-harness-test-context-preview-includes-segments ()
  "Context preview exposes segment metadata without changing flat messages."
  (let* ((stable-provider (e-context-provider-create
                           :name 'stable-provider
                           :cache-placement 'stable-context
                           :build (cl-function
                                   (lambda (&key harness session-id turn-id
                                                 context-purpose)
                                     (ignore harness session-id turn-id
                                             context-purpose)
                                     '((:role system
                                        :content "stable context"))))))
         (dynamic-provider (e-context-provider-create
                            :name 'dynamic-provider
                            :cache-placement 'dynamic-context
                            :build (cl-function
                                    (lambda (&key harness session-id turn-id
                                                  context-purpose)
                                      (ignore harness session-id turn-id
                                              context-purpose)
                                      '((:role system
                                         :content "dynamic context"))))))
         (capability (e-capability-create
                      :id 'test-capability
                      :instructions "capability instructions"
                      :context-providers (list stable-provider
                                               dynamic-provider)))
         (layer (e-layer-create
                 :id 'test-layer
                 :name "Test Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (e-layer-capabilities layer))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "hello"))
    (let* ((context (e-harness-context harness "session-1"))
           (segments (plist-get context :segments)))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :content))
                             (plist-get context :messages))
                     '("capability instructions"
                       "stable context"
                       "dynamic context"
                       "hello")))
      (should (equal (mapcar (lambda (segment)
                               (plist-get segment :kind))
                             segments)
                     '(static-prefix stable-context current-state history)))
      (dolist (segment segments)
        (should (stringp (plist-get segment :fingerprint)))
        (should (plist-get segment :messages))))))

(ert-deftest e-harness-test-context-partition-markers-are-derived-only ()
  "Caller/default/session partition markers cannot forge the context boundary."
  (let* ((dynamic-provider
          (e-context-provider-create
           :name 'hostile-dynamic-provider
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    '((:role system :content "current state")))))
         (capability
          (e-capability-create
           :id 'hostile-partition-capability
           :instructions "stable instructions"
           :context-providers (list dynamic-provider)))
         (harness
          (e-harness-create
           :backend
           (e-backend-fake-create
            :context-capabilities
            '(:continuation linear
              :observation-delivery request-local-replaceable))
           :intrinsic-capabilities (list capability)
           :default-options
           '(:context-segment-message-count 1
             :replaceable-current-state-partitioned t))))
    (e-harness-create-session harness :id "session-1")
    (e-session-set-turn-options
     (e-harness-sessions harness)
     "session-1"
     '(:context-segment-message-count 999
       :replaceable-current-state-partitioned t))
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "prompt"))
    (let* ((context (e-harness-turn-context harness "session-1" "turn-1"))
           (options (plist-get context :options))
           (messages (plist-get context :messages))
           (segments (plist-get context :segments)))
      (should-not (plist-member options :replaceable-current-state-partitioned))
      (should (equal (plist-get options :context-segment-message-count)
                     (length messages)))
      (should (equal (plist-get options :context-segment-message-count)
                     (cl-loop for segment in segments
                              sum (length (plist-get segment :messages))))))))

(ert-deftest e-harness-test-context-without-capability-is-stateless ()
  "An undeclared backend keeps semantic current state but drops anchor reuse."
  (let* ((dynamic-provider
          (e-context-provider-create
           :name 'dynamic-provider
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    '((:role system :content "current state")))))
         (capability
          (e-capability-create
           :id 'anchor-capability
           :instructions "capability instructions"
           :context-providers (list dynamic-provider)))
         (layer (e-layer-create
                 :id 'anchor-layer
                 :name "Anchor Layer"
                 :capabilities (list capability)))
         (harness
          (e-harness-create
           :backend (e-backend-fake-create :items nil)
           :intrinsic-capabilities (e-layer-capabilities layer)
           :default-options '(:model "gpt-test"
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "prompt"))
    (let* ((context (e-harness-context harness "session-1" "turn-1"))
           (options (plist-get context :options)))
      (should (equal (plist-get options :context-capabilities)
                     '(:continuation none
                       :observation-delivery inherited
                       :prefix-cache none
                       :provider-compaction none
                       :reasoning-state none)))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
                             (plist-get options :replaceable-current-state))
                     nil))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
                             (e-context-current-state-messages context))
                     '("current state")))
      (should-not (plist-get options :provider-anchor))
      (should (eq (plist-get options :provider-anchor-invalidation-reason)
                  'continuation-capability-unavailable))
      (should (eq (plist-get options :context-rendering-strategy)
                  'stateless))
      (should (eq (plist-get options :provider-anchor-safety)
                  'hold-unavailable-capability)))))

(ert-deftest e-harness-test-skill-preamble-enters-context-without-full-content ()
  "Context advertises skill references through normal capability instructions."
  (let* ((captured-messages nil)
         (backend (e-backend-create
                   :name "capture"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore options)
                              (setq captured-messages messages)
                              (funcall on-item
                                       '(:type assistant-message :content "ok"))
                              (funcall on-item '(:type done :reason stop))))))
         (capability
          (e-capability-with-skills-create
           :id 'skill-capability
           :instructions "Capability instructions."
           :skills
           (list
            (e-skill-spec-create
             :name "review"
             :description "Review implementation changes."
             :content "Secret detailed review checklist."))
           :resources
           (list (lambda (store capability)
                   (e-store-register
                    store
                    (e-capability-id capability)
                    "refs/review.md"
                    :description "Review reference."
                    :content "Reference content.")))))
         (layer (e-layer-create
                 :id 'skill-layer
                 :name "Skill Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create :backend backend)))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (let ((preamble (seq-find
                     (lambda (message)
                       (string-match-p
                        "Additional guidance is available on demand"
                        (plist-get message :content)))
                     captured-messages)))
      (should preamble)
      (should (string-match-p "Capability instructions."
                              (plist-get preamble :content)))
      (should (string-match-p "review" (plist-get preamble :content)))
      (should (string-match-p "Review implementation changes"
                              (plist-get preamble :content)))
      (should (string-match-p "e://skill-capability/skills/review"
                              (plist-get preamble :content)))
      (should-not (string-match-p "Secret detailed review checklist"
                                  (plist-get preamble :content)))
      (should-not (string-match-p "review.md"
                                  (plist-get preamble :content))))))

(provide 'e-harness-context-composition-test)

;;; e-harness-context-composition-test.el ends here
