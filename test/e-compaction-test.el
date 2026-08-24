;;; e-compaction-test.el --- Tests for e context compaction -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for provider-neutral compaction preparation.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-compaction)
(require 'e-anthropic)
(require 'e-harness)
(require 'e-openai)
(require 'e-session)

(ert-deftest e-compaction-test-prepare-chooses-user-boundary ()
  "Compaction preparation keeps a suffix starting at a user message."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (e-session-append-message store "session-1" '(:role assistant :content "old answer"))
    (let ((boundary
           (e-session-append-message
            store "session-1" '(:role user :content "keep"))))
      (e-session-append-message store "session-1" '(:role tool-call :content (:name "x")))
      (e-session-append-message store "session-1" '(:role tool :content "tool output"))
      (let ((preparation (e-compaction-prepare
                          store "session-1" :keep-recent-tokens 20)))
        (should (equal (plist-get preparation :first-kept-entry-id)
                       (plist-get boundary :id)))
        (should (string-match-p "old answer"
                                (plist-get preparation :summary-input)))
        (should-not (string-match-p "tool output"
                                    (plist-get preparation :summary-input)))))))

(ert-deftest e-compaction-test-prepare-can-choose-mid-turn-tool-call-boundary ()
  "Split-turn compaction may keep a suffix from a tool call, never its result."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (e-session-append-message store "session-1" '(:role assistant :content "early work"))
    (let ((boundary
           (e-session-append-message
            store "session-1"
            '(:role tool-call :content (:name "read_file" :arguments (:path "x.el"))))))
      (e-session-append-message store "session-1"
                                '(:role tool :content "recent tool result"))
      (let* ((preparation (e-compaction-prepare
                           store "session-1"
                           :keep-recent-tokens 1
                           :allow-split-turn t))
             (metadata (plist-get preparation :metadata)))
        (should (equal (plist-get preparation :first-kept-entry-id)
                       (plist-get boundary :id)))
        (should (equal (plist-get metadata :boundary-role) 'tool-call))
        (should (eq (plist-get metadata :split-turn) t))
        (should (string-match-p "early work"
                                (plist-get preparation :summary-input)))
        (should-not (string-match-p "recent tool result"
                                    (plist-get preparation :summary-input)))))))

(ert-deftest e-compaction-test-prepare-truncates-tool-results-and-records-resources ()
  "Summarization input truncates large tool output and metadata tracks resources."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (e-session-append-message
     store "session-1"
     (list :role 'tool
           :content (make-string (+ e-compaction-tool-result-character-limit 10) ?x)
           :metadata
           '(:tool-usage
             ((:kind resource-usage
               :tool "read_file"
               :resources ((:uri "file:///tmp/a.el" :operation read)))))))
    (e-session-append-message store "session-1" '(:role user :content "keep"))
    (let* ((preparation (e-compaction-prepare
                         store "session-1" :keep-recent-tokens 1))
           (metadata (plist-get preparation :metadata))
           (resources (plist-get metadata :affected-resources)))
      (should (string-match-p "\\[truncated 10 characters\\]"
                              (plist-get preparation :summary-input)))
      (should (equal (plist-get (car resources) :uri)
                     "file:///tmp/a.el"))
      (should (equal (plist-get (car resources) :operation) 'read)))))

(ert-deftest e-compaction-test-prepare-stringifies-tool-result-alists ()
  "Compaction handles nested alists from structured tool results."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (e-session-append-message
     store "session-1"
     '(:role tool
       :content (:tool-call-id "call-1"
                 :name "web_fetch"
                 :status ok
                 :content (:capability "web.fetch"
                           :headers (("date" . "Sun, 21 Jun 2026 10:36:23 GMT")
                                     ("content-type" . "text/html"))
                           :markdown "Fetched page"))))
    (e-session-append-message store "session-1" '(:role user :content "keep"))
    (let ((preparation (e-compaction-prepare
                        store "session-1" :keep-recent-tokens 1)))
      (should (string-match-p "Fetched page"
                              (plist-get preparation :summary-input)))
      (should (string-match-p "Sun, 21 Jun 2026 10:36:23 GMT"
                              (plist-get preparation :summary-input))))))

(ert-deftest e-compaction-test-summary-messages-include-previous-summary-on-repeat ()
  "Repeated compaction prompt includes previous summary once."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (let ((boundary
           (e-session-append-message
            store "session-1" '(:role user :content "middle"))))
      (e-session-append-compaction
       store "session-1" "Previous summary."
       :first-kept-entry-id (plist-get boundary :id))
      (e-session-append-message store "session-1" '(:role assistant :content "middle answer"))
      (e-session-append-message store "session-1" '(:role user :content "latest"))
      (let* ((preparation (e-compaction-prepare
                           store "session-1" :keep-recent-tokens 1))
             (messages (e-compaction-summary-messages preparation))
             (prompt (plist-get (cadr messages) :content)))
        (should (string-match-p "Previous summary:\nPrevious summary\\." prompt))
        (should (string-match-p "middle answer" prompt))
        (should-not (string-match-p "old" (plist-get preparation :summary-input)))))))

(ert-deftest e-compaction-test-prepare-falls-back-to-split-turn-without-user-boundary ()
  "A long single agentic turn compacts at a split-turn boundary, not failing.
With one user message at the turn start and only assistant/tool entries after,
a user-only boundary search returns nil; the fallback keeps a suffix from a
later assistant/tool-call message instead of signalling no-boundary."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "start the agentic turn"))
    (e-session-append-message store "session-1" '(:role assistant :content "early step"))
    (e-session-append-message store "session-1" '(:role tool-call :content (:name "read_file")))
    (e-session-append-message store "session-1" '(:role tool :content "early result"))
    (e-session-append-message store "session-1" '(:role assistant :content "late step"))
    (e-session-append-message store "session-1" '(:role tool-call :content (:name "grep")))
    (e-session-append-message store "session-1" '(:role tool :content "late result"))
    ;; allow-split-turn nil mirrors active-turn auto-compaction; fallback applies.
    (let* ((preparation (e-compaction-prepare
                         store "session-1"
                         :keep-recent-tokens 1
                         :allow-split-turn nil))
           (metadata (plist-get preparation :metadata)))
      (should (plist-get preparation :first-kept-entry-id))
      (should (memq (plist-get metadata :boundary-role) '(assistant tool-call)))
      (should (eq (plist-get metadata :split-turn) t))
      (should (string-match-p "early step"
                              (plist-get preparation :summary-input))))))

(ert-deftest e-compaction-test-portable-input-excludes-ephemeral-and-provider-state ()
  "Portable compaction reads only the semantic durable projection."
  (let* ((store (e-session-store-create))
         (session-id "portable-input")
         (session (e-session-create store :id session-id))
         (generation
          (e-context-lifetime-generation-create
           :id "generation-portable-input"
           :checkpoint '((:role system :content "C0"))
           :covered-session-boundary (plist-get session :root-event-id))))
    (e-session-append-context-generation store session-id generation)
    (e-session-append-message store session-id
                              '(:id "intent" :role user
                                :content "durable intent"))
    (e-session-append-message store session-id
                              '(:id "call" :role tool-call
                                :content (:id "call-1" :name "inspect")
                                :metadata (:provider-replay-items
                                            ((:type "reasoning")))))
    (e-session-append-message store session-id
                              '(:id "result" :role tool
                                :content (:tool-call-id "call-1"
                                          :content "RAW-E-PORTABLE")))
    (e-session-append-message store session-id
                              '(:id "answer" :role assistant
                                :content "durable answer"))
    (let* ((frame
            (e-context-lifetime-frame-create
             :id "frame-portable-input"
             :generation-id "generation-portable-input"
             :consumer-request-id "consumer-portable-input"
             :observations
             '((:observation-id "observation-raw"
                :kind "tool-result"
                :source-entry-ref "result"
                :source-fingerprint "raw-e-fingerprint"
                :effective-delivery "inherited"
                :body (:content "RAW-E-PORTABLE")))))
           (consumed
            (e-context-lifetime-frame-complete-for-consumer
             frame "consumer-portable-input" "answer"))
           (promotion
            (e-context-lifetime-promotion-from-effect
             consumed
             '(:type context-promote :schema-version 1
               :frame-id "frame-portable-input"
               :source-observation-ids ("observation-raw")
               :facts ((:id "selected" :value "promoted fact"))))))
      (e-session-append-context-promotion store session-id promotion))
    (let* ((prepared (e-compaction-prepare
                      store session-id :keep-recent-tokens 1 :portable t))
           (input (plist-get prepared :portable-input))
           (printed (prin1-to-string input))
           (tail (plist-get input :durable-tail)))
      (should (equal (plist-get input :checkpoint)
                     '((:role system :content "C0"))))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
                             tail)
                     '("durable intent")))
      (should-not (string-match-p "RAW-E-PORTABLE" printed))
      (should-not (string-match-p "provider-replay-items" printed))
      (should-not (string-match-p "provider-anchor" printed))
      (should (= (length (plist-get input :promotions)) 1))
      (let* ((checkpoint
              (e-compaction-portable-checkpoint-from-summary
               prepared "C1"))
             (application
              (e-compaction-preflight-portable-boundary
               store session-id prepared checkpoint)))
        (e-compaction-apply-portable-boundary
         store session-id application))
      (let* ((after (e-session-context-lifetime-projection
                     store session-id))
             (promotions (plist-get after :promotions)))
        (should-not promotions)
        (should (string-match-p
                 "Promoted fact selected: promoted fact"
                 (prin1-to-string
                  (e-context-lifetime-generation-checkpoint
                   (plist-get after :generation)))))))))

(ert-deftest e-compaction-test-portable-boundary-retains-checkpoint-and-fresh-generation ()
  "A portable boundary starts a minimal generation and derives a new tail."
  (let* ((store (e-session-store-create))
         (session-id "portable-boundary")
         (session (e-session-create store :id session-id))
         (old-generation
          (e-context-lifetime-generation-create
           :id "generation-old"
           :checkpoint '((:role system :content "C0"))
           :covered-session-boundary (plist-get session :root-event-id))))
    (e-session-append-context-generation store session-id old-generation)
    (e-session-append-message store session-id
                              '(:id "old-intent" :role user
                                :content "durable intent"))
    (e-session-append-message store session-id
                              '(:id "old-answer" :role assistant
                                :content "old answer"))
    (e-session-append-message store session-id
                              '(:id "raw-tool" :role tool
                                :content "RAW-E-DROPPED"))
    (let* ((preparation
            (e-compaction-prepare store session-id
                                  :keep-recent-tokens 1 :portable t))
           (checkpoint
            (e-compaction-portable-checkpoint-from-summary
             preparation "C1 durable intent; selected fact"))
           (application
            (e-compaction-preflight-portable-boundary
             store session-id preparation checkpoint))
           (entry
            (e-compaction-apply-portable-boundary
             store session-id application))
           (record (e-session--context-record entry)))
      (should (string-prefix-p "generation:" (plist-get record :id)))
      (should (equal (plist-get record :covered-session-boundary)
                     (plist-get (e-session-entry-by-id
                                 store session-id "old-intent")
                                :id)))
      (should-not (plist-member record :durable-tail))
      (should-not (string-match-p "RAW-E-DROPPED"
                                  (prin1-to-string record)))
      (e-session-append-message store session-id
                                '(:id "new-intent" :role user
                                  :content "after boundary"))
      (let* ((projection (e-session-context-lifetime-projection
                          store session-id))
             (generation (plist-get projection :generation))
             (tail (plist-get projection :durable-tail))
             (messages (mapcar (lambda (message)
                                 (plist-get message :content))
                               tail)))
        (should (string-prefix-p
                 "generation:"
                 (e-context-lifetime-generation-id generation)))
        (should (equal (e-context-lifetime-generation-checkpoint generation)
                       '((:role system
                          :content "C1 durable intent; selected fact"))))
        (should (equal messages '("old answer" "after boundary")))
        (should-not (member "durable intent" messages))
        (should-not (member "RAW-E-DROPPED" messages))))))

(ert-deftest e-compaction-test-portable-boundary-gets-fresh-runtime-frame ()
  "The next opted-in request receives the new generation and a new frame."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (session-id "portable-frame")
         (e-context-lifetime-shadow-projection-enabled t))
    (e-harness-create-session harness :id session-id)
    (let* ((before (e-harness-turn-context harness session-id "before"))
           (old-generation
            (e-context-lifetime-generation-id
             (plist-get before :lifetime-generation)))
           (old-frame (plist-get before :lifetime-frame))
           (old-anchor-fingerprints
            (e-harness--provider-anchor-fingerprints before)))
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role user :content "durable before boundary"))
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role assistant :content "retained after boundary"))
      (let* ((preparation
              (e-compaction-prepare
               (e-harness-sessions harness) session-id
               :keep-recent-tokens 1 :portable t))
             (checkpoint
              (e-compaction-portable-checkpoint-from-summary
               preparation "portable C1: durable before boundary"))
             (application
              (e-compaction-preflight-portable-boundary
               (e-harness-sessions harness) session-id
               preparation checkpoint)))
        (e-compaction-apply-portable-boundary
         (e-harness-sessions harness) session-id application))
      (let* ((after (e-harness-turn-context harness session-id "after"))
             (new-generation
             (e-context-lifetime-generation-id
               (plist-get after :lifetime-generation)))
             (new-frame (plist-get after :lifetime-frame))
             (new-anchor-fingerprints
              (e-harness--provider-anchor-fingerprints after)))
        (should (string-prefix-p "generation:" new-generation))
        (should-not (equal old-generation new-generation))
        (should (e-context-lifetime-frame-p new-frame))
        (should-not (equal (e-context-lifetime-frame-id old-frame)
                           (e-context-lifetime-frame-id new-frame)))
        (should-not (equal
                     (plist-get old-anchor-fingerprints :lifetime-generation)
                     (plist-get new-anchor-fingerprints :lifetime-generation)))
        (should (equal
                 (mapcar (lambda (message)
                           (list (plist-get message :role)
                                 (plist-get message :content)))
                         (plist-get after :messages))
                 '((system "portable C1: durable before boundary")
                   (assistant "retained after boundary"))))))))

(ert-deftest e-compaction-test-portable-checkpoint-rejects-speculative-shape ()
  "Portable boundaries require an ordinary message sequence, not a wrapper."
  (let* ((store (e-session-store-create))
         (session-id "portable-checkpoint-shape"))
    (e-session-create store :id session-id)
    (e-session-append-message store session-id
                              '(:role user :content "old"))
    (e-session-append-message store session-id
                              '(:role assistant :content "boundary"))
    (should-error
     (e-compaction-apply-portable-boundary
      store session-id
      (e-compaction-preflight-portable-boundary
       store session-id
       (e-compaction-prepare store session-id
                             :keep-recent-tokens 1 :portable t)
       '(:summary "not a message sequence")))
     :type 'e-compaction-error)
    (should-not (e-session-context-generations store session-id))))

(ert-deftest e-compaction-test-disabled-preparation-does-not-build-portable-input ()
  "The ordinary compaction preparation remains lazy when opt-in is absent."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "lazy-preparation")
    (e-session-append-message store "lazy-preparation"
                              '(:role user :content "old"))
    (e-session-append-message store "lazy-preparation"
                              '(:role user :content "keep"))
    (let ((preparation
           (e-compaction-prepare store "lazy-preparation" :keep-recent-tokens 1)))
      (should-not (plist-member preparation :portable-input)))))

(ert-deftest e-compaction-test-portable-preflight-rejects-late-promotion-without-mutation ()
  "A promotion frontier change rejects a stale portable application cleanly."
  (let* ((store (e-session-store-create))
         (session-id "promotion-frontier")
         (session (e-session-create store :id session-id))
         (generation
          (e-context-lifetime-generation-create
           :id "generation-frontier"
           :checkpoint '((:role system :content "C0"))
           :covered-session-boundary (plist-get session :root-event-id))))
    (e-session-append-context-generation store session-id generation)
    (e-session-append-message store session-id
                              '(:role user :content "prepare"))
    (e-session-append-message store session-id
                              '(:role assistant :content "boundary"))
    (let* ((preparation (e-compaction-prepare
                         store session-id :keep-recent-tokens 1 :portable t))
           (frame
            (e-context-lifetime-frame-create
             :id "frame-frontier"
             :generation-id "generation-frontier"
             :consumer-request-id "consumer-frontier"
             :observations
             '((:observation-id "observation-frontier"
                :kind "tool-result"
                :source-entry-ref "external:frontier"
                :source-fingerprint "frontier-fingerprint"
                :effective-delivery "inherited"
                :body (:content "late promotion")))))
           (consumed
            (e-context-lifetime-frame-complete-for-consumer
             frame "consumer-frontier" "response-frontier"))
           (promotion
            (e-context-lifetime-promotion-from-effect
             consumed
             '(:type context-promote :schema-version 1
               :frame-id "frame-frontier"
               :source-observation-ids ("observation-frontier")
               :facts ((:id "frontier-fact" :value "selected later"))))))
      (e-session-append-context-promotion store session-id promotion)
      (should-error
       (e-compaction-apply-portable-boundary
        store session-id
        (e-compaction-preflight-portable-boundary
         store session-id preparation '((:role system :content "C1"))))
       :type 'e-compaction-error)
      (should (= (length (e-session-context-generations store session-id)) 1))
      (should (= (length (plist-get
                          (e-session-context-lifetime-projection store session-id)
                          :promotions))
                 1)))))

(ert-deftest e-compaction-test-portable-preflight-rejects-generation-interleave ()
  "A generation change during summary causes no second portable append."
  (let* ((store (e-session-store-create))
         (session-id "generation-interleave")
         (session (e-session-create store :id session-id))
         (first
          (e-context-lifetime-generation-create
           :id "generation-interleave-1"
           :checkpoint '((:role system :content "C0"))
           :covered-session-boundary (plist-get session :root-event-id))))
    (e-session-append-context-generation store session-id first)
    (e-session-append-message store session-id '(:role user :content "prepare"))
    (e-session-append-message store session-id
                              '(:role assistant :content "boundary"))
    (let ((preparation (e-compaction-prepare
                        store session-id :keep-recent-tokens 1 :portable t)))
      (e-session-append-context-generation
       store session-id
       (e-context-lifetime-generation-create
        :id "generation-interleave-2"
        :checkpoint '((:role system :content "C2"))
        :covered-session-boundary
        (plist-get (e-session-get store session-id) :current-head-id)))
      (should-error
       (e-compaction-apply-portable-boundary
        store session-id
        (e-compaction-preflight-portable-boundary
         store session-id preparation '((:role system :content "C3"))))
       :type 'e-compaction-error)
      (should (= (length (e-session-context-generations store session-id)) 2)))))

(ert-deftest e-compaction-test-portable-message-normalizes-real-session-metadata ()
  "Portable preparation strips realistic transcript metadata at one boundary."
  (let* ((store (e-session-store-create))
         (session-id "portable-message-normalizer")
         (session (e-session-create store :id session-id))
         (generation
          (e-context-lifetime-generation-create
           :id "generation-normalizer"
           :checkpoint '((:role system :content "C0"))
           :covered-session-boundary (plist-get session :root-event-id))))
    (e-session-append-context-generation store session-id generation)
    (e-session-append-message
     store session-id
     '(:role user :content "old question"))
    (e-session-append-message
     store session-id
     '(:role assistant :content "kept answer"
       :id "assistant-entry" :turn-id "turn-1"
       :metadata (:provider "fake" :created-by "test")))
    (e-session-append-message
     store session-id '(:role user :content "retained question"))
    (let* ((input (plist-get (e-compaction-prepare
                              store session-id
                              :keep-recent-tokens 1 :portable t)
                             :portable-input))
           (tail (plist-get input :durable-tail)))
      (should (equal tail '((:role user :content "old question")
                            (:role assistant :content "kept answer")))))))

(ert-deftest e-compaction-test-portable-role-codec-has-closed-vocabulary ()
  "Portable role strings rehydrate finitely and unknown roles are rejected."
  (should (equal (e-context-lifetime-portable-checkpoint
                  '((:role "assistant" :content "answer")))
                 '((:role assistant :content "answer"))))
  (should-error
   (e-context-lifetime-portable-checkpoint
   '((:role "provider-private-role" :content "bad")))
   :type 'e-context-lifetime-invalid-record))

(ert-deftest e-compaction-test-portable-checkpoint-renders-symbolic-roles-per-adapter ()
  "Reopened portable roles render through each adapter's established wire shape."
  (let* ((messages
          (e-context-lifetime-portable-checkpoint
           '((:role "system" :content "portable policy")
             (:role "user" :content "portable request"))))
         (openai
          (e-openai-codex-request-body
           :messages messages :options nil :tools nil))
         (anthropic
          (e-anthropic-request-body
           :messages messages :options nil :tools nil)))
    (should (string-match-p
             (regexp-quote "portable policy")
             (plist-get openai :instructions)))
    (should (equal (plist-get (aref (plist-get openai :input) 0) :role)
                   "user"))
    (should (equal (plist-get anthropic :system) "portable policy"))
    (should (equal (plist-get (aref (plist-get anthropic :messages) 0) :role)
                   "user"))))

(ert-deftest e-compaction-test-generation-change-rejects-anchor-statelessly ()
  "A portable generation change invalidates an old anchor and selects stateless input."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (harness
          (e-harness-create
           :backend
           (e-backend-fake-create
            :context-capabilities '(:continuation linear)
            :items nil)
           :default-options '(:provider-continuation t
                              :provider-anchor-provider-id fake)))
         (store (e-harness-sessions harness)))
    (e-harness-create-session harness :id "anchor-generation")
    (let* ((before (e-harness-turn-context harness "anchor-generation" "before"))
           (session (e-session-get store "anchor-generation"))
           (anchor
            (e-session-append-provider-anchor
             store "anchor-generation" 'fake
             :model nil
             :covered-entry-id (plist-get session :root-event-id)
             :fingerprints (e-harness--provider-anchor-fingerprints before))))
      (should anchor)
      (e-session-append-message store "anchor-generation"
                                '(:role user :content "before anchor"))
      (e-session-append-message store "anchor-generation"
                                '(:role assistant :content "after anchor"))
      (let* ((preparation
              (e-compaction-prepare store "anchor-generation"
                                     :keep-recent-tokens 1 :portable t))
             (checkpoint
              (e-compaction-portable-checkpoint-from-summary
               preparation "C1"))
             (application
              (e-compaction-preflight-portable-boundary
               store "anchor-generation" preparation checkpoint)))
        (e-compaction-apply-portable-boundary
         store "anchor-generation" application))
      (let* ((after (e-harness-turn-context harness "anchor-generation" "after"))
             (options (plist-get after :options)))
        (should-not (plist-get options :provider-anchor))
        (should (eq (plist-get options :provider-anchor-invalidation-reason)
                    'context-generation-changed))
        (should (eq (plist-get options :context-rendering-strategy)
                    'stateless))))))

(provide 'e-compaction-test)

;;; e-compaction-test.el ends here
