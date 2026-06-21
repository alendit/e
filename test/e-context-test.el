;;; e-context-test.el --- Tests for e context strategies -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for provider-neutral context construction.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-context)
(require 'e-session)

(ert-deftest e-context-test-transcript-stack-preserves-session-order ()
  "Transcript-stack builds backend context from ordered session messages."
  (let ((store (e-session-store-create))
        (strategy (e-context-transcript-stack-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:id "m1" :role user :content "hi"))
    (e-session-append-message store "session-1" '(:id "m2" :role assistant :content "hello"))
    (e-session-append-message store "session-1" '(:id "m3" :role tool :content (:status ok)))
    (let ((context (e-context-build strategy
                                    :sessions store
                                    :session-id "session-1"
                                    :options '(:model "fake"))))
      (should (equal (plist-get context :strategy) 'transcript-stack))
      (should (equal (plist-get context :messages)
                     '((:role user :content "hi")
                       (:role assistant :content "hello")
                       (:role tool :content (:status ok)))))
      (should (equal (plist-get context :options) '(:model "fake"))))))

(ert-deftest e-context-test-transcript-stack-includes-history-segment ()
  "Transcript-stack exposes history segment metadata beside flat messages."
  (let ((store (e-session-store-create))
        (strategy (e-context-transcript-stack-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1"
                              '(:id "m1" :role user :content "hi"))
    (let* ((context (e-context-build strategy
                                     :sessions store
                                     :session-id "session-1"
                                     :options nil))
           (segments (plist-get context :segments))
           (segment (car segments)))
      (should (= (length segments) 1))
      (should (eq (plist-get segment :kind) 'history))
      (should (equal (plist-get segment :id) 'transcript-history))
      (should (equal (plist-get segment :messages)
                     (plist-get context :messages)))
      (should (stringp (plist-get segment :fingerprint)))
      (should (equal (plist-get segment :fingerprint)
                     (plist-get
                      (car (plist-get
                            (e-context-build strategy
                                             :sessions store
                                             :session-id "session-1"
                                             :options nil)
                            :segments))
                      :fingerprint))))))

(ert-deftest e-context-test-prefix-messages-create-compatible-segment ()
  "Prefix messages keep flat order and get deterministic segment metadata."
  (let ((store (e-session-store-create))
        (strategy (e-context-transcript-stack-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1"
                              '(:id "m1" :role user :content "hi"))
    (let* ((context (e-context-build
                     strategy
                     :sessions store
                     :session-id "session-1"
                     :options nil
                     :prefix-messages '((:role system :content "prefix"))))
           (segments (plist-get context :segments)))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :content))
                             (plist-get context :messages))
                     '("prefix" "hi")))
      (should (equal (mapcar (lambda (segment)
                               (plist-get segment :kind))
                             segments)
                     '(static-prefix history)))
      (should (equal (plist-get (car segments) :messages)
                     '((:role system :content "prefix")))))))

(ert-deftest e-context-test-transcript-stack-uses-latest-compaction-suffix ()
  "Transcript-stack sends one compaction summary plus the kept suffix."
  (let ((store (e-session-store-create))
        (strategy (e-context-transcript-stack-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:id "m1" :role user :content "old"))
    (e-session-append-message store "session-1" '(:id "m2" :role assistant :content "old answer"))
    (let ((boundary
           (e-session-append-message
            store "session-1" '(:id "m3" :role user :content "keep this"))))
      (e-session-append-message store "session-1"
                                '(:id "m4" :role assistant :content "kept answer"))
      (e-session-append-compaction
       store "session-1" "Earlier work summary."
       :first-kept-entry-id (plist-get boundary :id))
      (should
       (equal (plist-get
               (e-context-build strategy
                                :sessions store
                                :session-id "session-1"
                                :options nil)
               :messages)
              '((:role system :content "Earlier work summary.")
                (:role user :content "keep this")
                (:role assistant :content "kept answer")))))))

(ert-deftest e-context-test-compacted-suffix-previews-large-tool-results ()
  "Compacted transcript keeps turn structure while previewing large tool output."
  (let ((store (e-session-store-create))
        (strategy (e-context-transcript-stack-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:id "m1" :role user :content "old"))
    (let ((boundary
           (e-session-append-message
            store "session-1" '(:id "m2" :role user :content "inspect output"))))
      (e-session-append-message
       store "session-1"
       '(:id "m3"
         :role tool-call
         :content (:id "call-1" :name "bash" :arguments (:command "large"))))
      (e-session-append-message
       store "session-1"
       (list :id "m4"
             :role 'tool
             :content (list :tool-call-id "call-1"
                            :name "bash"
                            :status 'ok
                            :content (make-string 2100 ?x)
                            :metadata
                            '(:truncated t :tmp-uri "tmp://full-output.txt"))))
      (e-session-append-message store "session-1"
                                '(:id "m5" :role assistant :content "done"))
      (let* ((messages
              (plist-get
               (e-context-build strategy
                                :sessions store
                                :session-id "session-1"
                                :options nil)
               :messages))
             (tool-result (plist-get (nth 3 messages) :content)))
        (should (= (length (plist-get tool-result :content)) 2100)))
      (e-session-append-compaction
       store "session-1" "Earlier work summary."
       :first-kept-entry-id (plist-get boundary :id))
      (let* ((messages
              (let ((e-compaction-kept-tool-result-character-limit 20))
                (plist-get
                 (e-context-build strategy
                                  :sessions store
                                  :session-id "session-1"
                                  :options nil)
                 :messages)))
             (tool-message (nth 3 messages))
             (tool-result (plist-get tool-message :content)))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :role))
                               messages)
                       '(system user tool-call tool assistant)))
        (should (equal (plist-get tool-result :tool-call-id) "call-1"))
        (should (equal (plist-get tool-result :name) "bash"))
        (should (equal (plist-get tool-result :status) 'ok))
        (should (equal (plist-get (plist-get tool-result :metadata) :tmp-uri)
                       "tmp://full-output.txt"))
        (should (< (length (plist-get tool-result :content)) 2100))
        (should (string-match-p "tmp://full-output.txt"
                                (plist-get tool-result :content)))
        (should (string-match-p "\\[Tool output preview truncated:"
                                (plist-get tool-result :content)))))))

(ert-deftest e-context-test-rejects-missing-builder ()
  "Context strategies need a build function."
  (let ((strategy (e-context-create :name 'broken :build nil)))
    (should-error
     (e-context-build strategy
                      :sessions (e-session-store-create)
                      :session-id "session-1"
                      :options nil)
     :type 'wrong-type-argument)))

(ert-deftest e-context-test-stale-provider-records-default-to-stable-placement ()
  "Provider records compiled before cache placement keep their build contract."
  (let ((stale-provider
         (record 'e-context-provider
                 'legacy-provider
                 200
                 (lambda (&rest _args) nil))))
    (should (= (e-context-provider-priority stale-provider) 200))
    (should (eq (e-context-provider-cache-placement stale-provider)
                'stable-context))
    (should-not (e-context-provider-build stale-provider))))

(ert-deftest e-context-test-rejects-malformed-provider-records ()
  "Provider records still need the legacy build slot to be callable."
  (let ((malformed-provider
         (record 'e-context-provider 'legacy-provider 200)))
    (should-error (e-context-provider-build malformed-provider))))

(ert-deftest e-context-test-provider-cache-placement-defaults-to-stable ()
  "Context providers default to stable cache placement."
  (let ((provider (e-context-provider-create
                   :name 'provider
                   :build #'ignore)))
    (should (eq (e-context-provider-cache-placement provider)
                'stable-context))))

(ert-deftest e-context-test-rejects-invalid-provider-cache-placement ()
  "Context provider cache placement is limited to known prompt regions."
  (let ((provider (e-context-provider-create
                   :name 'provider
                   :cache-placement 'elsewhere
                   :build #'ignore)))
    (should-error (e-context-provider-cache-placement provider)
                  :type 'wrong-type-argument)))

(ert-deftest e-context-test-drops-orphan-tool-call-keeps-paired ()
  "Backend messages drop tool-calls with no matching result, keep paired ones.
Regression: a turn killed between a tool-call and its result left an orphan
tool-use that providers reject (\"tool_use ids found without tool_result\")."
  (let* ((messages
          (list '(:role user :content "hi")
                '(:role tool-call :content (:id "paired" :name "noop"))
                '(:role tool :content (:tool-call-id "paired" :content "ok"))
                '(:role assistant :content "done")
                ;; Orphan: killed before its result was appended.
                '(:role tool-call :content (:id "orphan" :name "noop"))))
         (result (e-context--backend-messages messages))
         (ids (delq nil
                    (mapcar (lambda (m)
                              (and (eq (plist-get m :role) 'tool-call)
                                   (plist-get (plist-get m :content) :id)))
                            result))))
    (should (member "paired" ids))
    (should-not (member "orphan" ids))
    ;; Only the orphan tool-call is removed; everything else is kept.
    (should (= 4 (length result)))))

(provide 'e-context-test)

;;; e-context-test.el ends here
