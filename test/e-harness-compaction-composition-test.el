;;; e-harness-compaction-composition-test.el --- Public harness compaction composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Compaction and curation composition scenarios, including failure fencing.

;;; Code:

(require 'ert)
(load (expand-file-name "e-harness-composition-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-harness-test-tool-finished-activity-compacts-result-payload ()
  "Durable tool-finished activity stores one compact lifecycle receipt."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (let ((e-harness-activity-trusted-tool-details-uri
           "tmp://tool-invocations/turn-1/call-1.json"))
      (e-harness-activity-emit-turn-event
       harness
       "session-1"
       "turn-1"
       'tool-finished
       '(:tool-call (:id "call-1" :name "bash"
                    :stated-purpose "Run the bounded command"
                    :arguments (:command "raw-command-secret"))
         :result (:tool-call-id "call-1"
                  :name "bash"
                  :status ok
                  :content "raw-result-secret"
                  :metadata (:invocation-details-uri
                             "tmp://tool-invocations/turn-1/call-1.json"
                             :authorization "Bearer raw-auth")))))
    (let* ((event (car (e-harness-session-activity-events
                        harness "session-1")))
           (payload (plist-get event :payload))
           (call (plist-get payload :tool-call))
           (receipt (plist-get payload :receipt))
           (serialized (prin1-to-string payload)))
      (should (equal call '(:id "call-1" :name "bash")))
      (should
       (equal receipt
              '(:tool-call-id "call-1"
                :tool "bash"
                :status ok
                :stated-purpose "Run the bounded command"
                :details-uri
                "tmp://tool-invocations/turn-1/call-1.json"
                :details-lifetime session-tmp)))
      (should-not (plist-member payload :result))
      (should-not (plist-member call :arguments))
      (should-not (string-match-p
                   "raw-command-secret\\|raw-result-secret\\|raw-auth"
                   serialized)))))

(ert-deftest e-harness-test-compact-session-appends-summary-and-uses-context-suffix ()
  "Manual compaction writes a durable record and context uses summary plus suffix."
  (let* ((backend (e-backend-create
                   :name 'summary
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (ignore messages options)
                      (funcall on-item
                               '(:type assistant-message
                                 :content "Old exchange summary."))))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (let ((store (e-harness-sessions harness)))
      (e-session-append-message store "session-1"
                                '(:role user :content "old question"))
      (e-session-append-message store "session-1"
                                '(:role assistant :content "old answer"))
      (let ((boundary
             (e-session-append-message
              store "session-1" '(:role user :content "new question"))))
        (e-session-append-message store "session-1"
                                  '(:role assistant :content "new answer"))
        (let ((record (e-harness-compact-session-batch
                       harness "session-1" :keep-recent-tokens 1)))
          (should (equal (plist-get record :summary)
                         "Old exchange summary."))
          (should (equal (plist-get record :first-kept-entry-id)
                         (plist-get boundary :id)))
          (should (= (length (e-session-messages store "session-1")) 4))
          (should
           (equal (plist-get (e-harness-context harness "session-1")
                             :messages)
                  '((:role system :content "Old exchange summary.")
                    (:role user :content "new question")
                    (:role assistant :content "new answer")))))))))

(ert-deftest e-harness-test-compact-session-can-opt-into-active-turn ()
  "Compaction rejects active turns unless the caller opts into turn-local compaction."
  (let* ((backend (e-backend-create
                   :name 'summary
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (ignore messages options)
                      (funcall on-item
                               '(:type assistant-message
                                 :content "Old exchange summary."))))))
         (harness (e-harness-create :backend backend))
         (store (e-harness-sessions harness))
         (active-entry '(:id "turn-active" :status running)))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message store "session-1"
                              '(:role user :content "old question"))
    (e-session-append-message store "session-1"
                              '(:role assistant :content "old answer"))
    (e-session-append-message store "session-1"
                              '(:role user :content "new question"))
    (puthash "session-1" active-entry (e-harness-active-turns harness))
    (should-error
     (e-harness-compact-session-batch harness "session-1" :keep-recent-tokens 1)
     :type 'e-harness-active-turn-exists)
    (let ((record (e-harness-compact-session-batch
                   harness "session-1"
                   :keep-recent-tokens 1
                   :allow-active-turn t
                   :turn-id "turn-active")))
      (should (equal (plist-get record :summary)
                     "Old exchange summary."))
      (should (eq (gethash "session-1" (e-harness-active-turns harness))
                  active-entry))
      (should
       (cl-find-if
        (lambda (event)
          (and (equal (plist-get event :turn-id) "turn-active")
               (eq (plist-get event :event-type) 'compaction-finished)))
        (e-session-activity-events store "session-1"))))))

(ert-deftest e-harness-test-enabled-compaction-summarizes-portable-context-only ()
  "Enabled compaction sends C0/D0/curation, never a raw observation."
  (let* ((captured-messages nil)
        (backend
         (e-backend-create
          :name 'portable-summary
          :stream
          (cl-function
           (lambda (&key messages options on-item)
             (ignore options)
             (setq captured-messages (copy-tree messages))
             (funcall on-item
                      '(:type assistant-message :content "Portable C1."))))))
        (harness (e-harness-create :backend backend))
        (store nil))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (e-harness-create-session harness :id "session-1")
      (setq store (e-harness-sessions harness))
      ;; Establish the opted-in identity generation before the durable input
      ;; and selected fact are captured by the portable summary request.
      (e-harness-turn-context harness "session-1" "seed")
      (e-session-append-message store "session-1"
                                '(:role user :content "old intent"))
      (e-session-append-message store "session-1"
                                '(:role assistant :content "old answer"))
      (e-session-append-message store "session-1"
                                '(:role tool :content "RAW-E-MUST-NOT-ESCAPE"))
      (e-session-append-message store "session-1"
                                '(:role user :content "kept intent"))
      (e-session-append-message store "session-1"
                                '(:role assistant :content "kept answer"))
      (let* ((generation
              (e-session-context-lifetime-current-generation store "session-1"))
             (frame
              (e-harness-test--curation-frame
               (e-context-lifetime-generation-id generation)
               "frame-portable-summary"
               "RAW-E-MUST-NOT-ESCAPE"
               "observation-portable-summary"
               "external:portable-summary"
               "portable-summary-fingerprint"))
             (curation
              (plist-get
               (e-context-lifetime-prepare-curation-disposition
                frame
                '(:keep nil
                  :summaries ((:sources (1)
                               :text "selected durable fact")))
                "response-portable-summary"
                1.0)
               :record)))
        (e-session-append-context-curation-package
         store "session-1" (list :promotion curation :erasure nil)))
      (e-harness-compact-session-batch harness "session-1"
                                       :keep-recent-tokens 1)
      (let* ((prompt (prin1-to-string captured-messages))
             (generation (e-session-context-lifetime-current-generation
                          store "session-1")))
        (should (string-match-p "Portable checkpoint" prompt))
        (should (string-match-p "Durable tail" prompt))
        (should (string-match-p "old intent" prompt))
        (should (string-match-p "old answer" prompt))
        (should-not (string-match-p "kept answer" prompt))
        (should (string-match-p "selected durable fact" prompt))
        (should-not (string-match-p "RAW-E-MUST-NOT-ESCAPE" prompt))
        (should-not (string-match-p "provider-replay-items" prompt))
        (should-not (string-match-p "provider-anchor" prompt))
        (should generation)
        (should (string-match-p "Portable C1"
                                (prin1-to-string
                                 (e-context-lifetime-generation-checkpoint
                                  generation))))
        (should (equal
                 (mapcar (lambda (message) (plist-get message :content))
                         (plist-get
                          (e-session-context-lifetime-projection
                           store "session-1")
                          :durable-tail))
                 '("kept intent" "kept answer")))))))

(defun e-harness-test--append-compaction-curation
    (store session-id generation-id suffix)
  "Append one valid curation for GENERATION-ID to SESSION-ID.
SUFFIX makes the runtime identities and fact unique to the owning test."
  (let* ((frame-id (format "frame-compaction-%s" suffix))
         (consumer-id (format "consumer-compaction-%s" suffix))
         (response-id (format "response-compaction-%s" suffix))
         (observation-id (format "observation-compaction-%s" suffix))
         (frame
          (e-harness-test--curation-frame
           generation-id frame-id
           (format "RAW-COMPACTION-%s" suffix)
           observation-id
           (format "external:compaction-%s" suffix)
           (format "compaction-fingerprint-%s" suffix)))
         (curation
          (plist-get
           (e-context-lifetime-prepare-curation-disposition
            frame
            (list :keep nil
                  :summaries
                  (list (list :sources '(1)
                              :text (format "selected-%s" suffix))))
            response-id
            1.0)
           :record)))
    (e-session-append-context-curation-package
     store session-id (list :promotion curation :erasure nil))))

(ert-deftest e-harness-test-enabled-async-compaction-absorbs-curation-and-filters-provider-state ()
  "Async enabled compaction absorbs facts and excludes runtime/provider state."
  (let* ((store (e-session-store-create))
         (session-id "enabled-async-compaction")
         (captured-messages nil)
         (record nil)
         (failure nil)
         (backend
          (e-backend-create
           :name 'enabled-async-summary
           :start
           (cl-function
            (lambda (&key messages on-item on-done &allow-other-keys)
              (setq captured-messages (copy-tree messages))
              (run-at-time
               0.01 nil
               (lambda ()
                 (funcall on-item
                          '(:type assistant-message
                            :content "ASYNC-PORTABLE-C1"))
                 (funcall on-done '(:status done))))
              (e-backend-request-create)))))
         (harness (e-harness-create :backend backend :sessions store)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (e-harness-create-session harness :id session-id)
      (e-harness-turn-context harness session-id "seed")
      (e-session-append-message
       store session-id
       '(:role user :content "ASYNC-OLD-INTENT"))
      (e-session-append-message
       store session-id
       '(:role assistant :content "ASYNC-OLD-ANSWER"))
      (let ((tool-call
             (e-session-append-message
              store session-id
              '(:role tool-call
                :content (:id "async-call" :name "inspect"
                          :arguments (:marker "ASYNC-RAW-CALL"))
                :metadata (:provider-replay-items
                           ((:id "ASYNC-REPLAY-MARKER")))))))
        (e-session-append-message
         store session-id
         '(:role tool
           :content (:tool-call-id "async-call"
                     :content "ASYNC-RAW-RESULT")))
        (e-session-append-message
         store session-id
         '(:role user :content "ASYNC-RETAINED-SUFFIX"))
        (e-session-append-message
         store session-id
         '(:role assistant :content "ASYNC-RETAINED-ANSWER"))
        (e-session-append-provider-anchor
         store session-id 'fake
         :model "async-model"
         :covered-entry-id (plist-get tool-call :id)
         :fingerprints '(:prompt-layout async-layout)
         :metadata '(:response-id "ASYNC-ANCHOR-MARKER")))
      (let* ((generation
              (e-session-context-lifetime-current-generation store session-id))
             (generations-before
              (length (e-session-context-generations store session-id))))
        (e-harness-test--append-compaction-curation
         store session-id
         (e-context-lifetime-generation-id generation)
         "async")
        (e-harness-compact-session-start
         harness session-id
         :keep-recent-tokens 1
         :on-done (lambda (value) (setq record value))
         :on-error (lambda (err) (setq failure err)))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (not (or record failure))
                      (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (should record)
        (should-not failure)
        (should captured-messages)
        (let* ((prompt (prin1-to-string captured-messages))
               (projection
                (e-session-context-lifetime-projection store session-id))
               (checkpoint
                (e-context-lifetime-generation-checkpoint
                 (plist-get projection :generation)))
               (tail (plist-get projection :durable-tail))
               (tail-contents
                (mapcar (lambda (message) (plist-get message :content))
                        tail)))
          (should (= (length (e-session-compactions store session-id)) 1))
          (should (= (length (e-session-context-generations store session-id))
                     (1+ generations-before)))
          (should (string-match-p "ASYNC-OLD-INTENT" prompt))
          (should (string-match-p "ASYNC-OLD-ANSWER" prompt))
          (should (string-match-p "selected-async" prompt))
          (should-not (string-match-p "ASYNC-RAW-CALL" prompt))
          (should-not (string-match-p "ASYNC-RAW-RESULT" prompt))
          (should-not (string-match-p "ASYNC-REPLAY-MARKER" prompt))
          (should-not (string-match-p "ASYNC-ANCHOR-MARKER" prompt))
          (should-not (plist-get projection :promotions))
          (should (string-match-p "selected-async"
                                  (prin1-to-string checkpoint)))
          (should (= (cl-count "ASYNC-RETAINED-SUFFIX"
                               tail-contents :test #'equal)
                     1))
          (should (= (cl-count "ASYNC-RETAINED-ANSWER"
                               tail-contents :test #'equal)
                     1)))))))

(ert-deftest e-harness-test-enabled-async-compaction-rejects-stale-curation-without-mutation ()
  "A curation appended during async summary rejects without partial append."
  (let* ((store (e-session-store-create))
         (session-id "enabled-async-stale-promotion")
         (captured-messages nil)
         (overlap-appended nil)
         (record nil)
         (failure nil)
         (backend
          (e-backend-create
           :name 'enabled-async-stale-summary
           :start
           (cl-function
            (lambda (&key messages on-item on-done &allow-other-keys)
              (setq captured-messages (copy-tree messages))
              (run-at-time
               0.01 nil
               (lambda ()
                 (e-harness-test--append-compaction-curation
                  store session-id
                  (e-context-lifetime-generation-id
                   (e-session-context-lifetime-current-generation
                    store session-id))
                  "late")
                 (setq overlap-appended t)
                 (funcall on-item
                          '(:type assistant-message
                            :content "STALE-PORTABLE-C1"))
                 (funcall on-done '(:status done))))
              (e-backend-request-create)))))
         (harness (e-harness-create :backend backend :sessions store)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (e-harness-create-session harness :id session-id)
      (e-harness-turn-context harness session-id "seed")
      (e-session-append-message store session-id
                                '(:role user :content "STALE-OLD-INTENT"))
      (e-session-append-message store session-id
                                '(:role assistant :content "STALE-OLD-ANSWER"))
      (e-session-append-message store session-id
                                '(:role user :content "STALE-RETAINED"))
      (let ((generations-before
             (length (e-session-context-generations store session-id)))
            (compactions-before
             (length (e-session-compactions store session-id))))
        (e-harness-compact-session-start
         harness session-id
         :keep-recent-tokens 1
         :on-done (lambda (value) (setq record value))
         :on-error (lambda (err) (setq failure err)))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (not (or record failure))
                      (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (should overlap-appended)
        (should-not record)
        (should failure)
        (should (eq (car failure) 'e-compaction-error))
        (should (= (length (e-session-compactions store session-id))
                   compactions-before))
        (should (= (length (e-session-context-generations store session-id))
                   generations-before))
        (should (= (length (e-session-context-promotions store session-id))
                   1))
        (should captured-messages)))))

(ert-deftest e-harness-test-enabled-auto-compaction-excludes-active-prompt-and-preserves-tail ()
  "Enabled automatic compaction excludes the active prompt and retains suffix once."
  (let* ((calls nil)
         (backend
          (e-backend-create
           :name 'enabled-auto-summary
           :start
           (cl-function
            (lambda (&key messages on-item on-done &allow-other-keys)
              (let ((ordinal (1+ (length calls))))
                (push (copy-tree messages) calls)
                (run-at-time
                 0.01 nil
                 (lambda ()
                   (funcall on-item
                            (list :type 'assistant-message
                                  :content
                                  (if (= ordinal 1)
                                      "AUTO-PORTABLE-C1"
                                    "AUTO-ANSWER")))
                   (funcall on-done '(:status done)))))
              (e-backend-request-create)))))
         (harness (e-harness-create
                   :backend backend
                   :default-options '(:model "enabled-auto-model")))
         (store (e-harness-sessions harness))
         (e-context-budget-model-token-limits
          '(("enabled-auto-model" . 100)))
         (e-harness-auto-compaction-reserve-tokens 10)
         (e-compaction-keep-recent-tokens 1))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (e-harness-create-session harness :id "enabled-auto-session")
      (e-session-append-message store "enabled-auto-session"
                                '(:role user :content "AUTO-OLD-INTENT"))
      (e-session-append-message store "enabled-auto-session"
                                '(:role assistant :content "AUTO-OLD-ANSWER"))
      (e-session-append-message store "enabled-auto-session"
                                '(:role user :content "AUTO-RETAINED-SUFFIX"))
      (e-session-append-activity-event
       store "enabled-auto-session" "auto-seed" 'token-usage
       '(:input-tokens 95 :total-tokens 96))
      (e-harness-test-prompt-async
       harness "enabled-auto-session" "AUTO-ACTIVE-PROMPT")
      (should (eq (plist-get
                   (e-harness-wait-batch harness "enabled-auto-session" 1.0)
                   :status)
                  'done)))
    (let* ((ordered (reverse calls))
           (summary-prompt (prin1-to-string (car ordered)))
           (provider-messages (cadr ordered))
           (provider-prompt (prin1-to-string provider-messages))
           (provider-contents
            (mapcar (lambda (message) (plist-get message :content))
                    provider-messages)))
      (should (= (length ordered) 2))
      (should (string-match-p "AUTO-OLD-INTENT" summary-prompt))
      (should-not (string-match-p "AUTO-ACTIVE-PROMPT" summary-prompt))
      (should (string-match-p "AUTO-ACTIVE-PROMPT" provider-prompt))
      (should (= (cl-count "AUTO-RETAINED-SUFFIX"
                           provider-contents :test #'equal)
                 1))
      (let ((e-context-lifetime-shadow-projection-enabled nil))
        (let* ((legacy (e-harness-context harness "enabled-auto-session"))
               (legacy-contents
                (mapcar (lambda (message) (plist-get message :content))
                        (plist-get legacy :messages))))
          (should (= (cl-count "AUTO-RETAINED-SUFFIX"
                               legacy-contents :test #'equal)
                     1)))))))

(ert-deftest e-harness-test-repeated-compaction-summarizes-from-previous-summary ()
  "Repeated compaction summarizes previous summary plus newly compacted suffix."
  (let ((calls nil)
        (summaries '("First summary." "Second summary.")))
    (let* ((backend (e-backend-create
                     :name 'summary
                     :stream
                     (cl-function
                      (lambda (&key messages options on-item)
                        (ignore options)
                        (push messages calls)
                        (funcall on-item
                                 (list :type 'assistant-message
                                       :content (pop summaries)))))))
           (harness (e-harness-create :backend backend))
           (store (e-harness-sessions harness)))
      (e-harness-create-session harness :id "session-1")
      (e-session-append-message store "session-1" '(:role user :content "old"))
      (e-session-append-message store "session-1" '(:role assistant :content "old answer"))
      (e-session-append-message store "session-1" '(:role user :content "middle"))
      (e-session-append-message store "session-1" '(:role assistant :content "middle answer"))
      (e-harness-compact-session-batch harness "session-1" :keep-recent-tokens 1)
      (e-session-append-message store "session-1" '(:role user :content "latest"))
      (e-session-append-message store "session-1" '(:role assistant :content "latest answer"))
      (e-harness-compact-session-batch harness "session-1" :keep-recent-tokens 1)
      (let ((second-prompt (plist-get (cadr (car calls)) :content)))
        (should (string-match-p "Previous summary:\nFirst summary\\." second-prompt))
        (should (string-match-p "middle answer" second-prompt))
        (should-not (string-match-p "old answer" second-prompt)))
      (should (equal (plist-get (e-session-latest-valid-compaction
                                 store "session-1")
                                :summary)
                     "Second summary.")))))

(ert-deftest e-harness-test-auto-compaction-runs-before_prompt_turn ()
  "Above-threshold prompts auto-compact once before appending the new user turn."
  (let ((calls nil))
    (let* ((backend (e-backend-create
                     :name 'auto-summary
                     :stream
                     (cl-function
                      (lambda (&key messages options on-item)
                        (ignore options)
                        (push messages calls)
                        (funcall on-item
                                 (list :type 'assistant-message
                                       :content (if (= (length calls) 1)
                                                    "Auto summary."
                                                  "Answer.")))
                        (funcall on-item '(:type done :reason stop))))))
           (harness (e-harness-create
                     :backend backend
                     :default-options '(:model "auto-model")))
           (store (e-harness-sessions harness))
           (e-context-budget-model-token-limits '(("auto-model" . 100)))
           (e-harness-auto-compaction-reserve-tokens 10))
      (e-harness-create-session harness :id "session-1")
      (e-session-append-message store "session-1"
                                '(:role user :content "old question"))
      (e-session-append-message store "session-1"
                                '(:role assistant :content "old answer"))
      (e-session-append-message store "session-1"
                                '(:id "kept" :role user :content "new topic"))
      (e-session-append-activity-event
       store "session-1" "turn-1" 'token-usage
       '(:input-tokens 95 :total-tokens 96))
      (e-harness-test-prompt-async harness "session-1" "fresh prompt")
      (should (equal (plist-get (e-harness-wait-batch harness "session-1" 1.0)
                                :status)
                     'done))
      (let ((record (car (e-session-compactions store "session-1"))))
        (should record)
        (should (eq (plist-get (plist-get record :metadata) :reason) 'auto)))
      (should (= (length calls) 2))
      (let ((summary-prompt (mapconcat
                             (lambda (message)
                               (or (plist-get message :content) ""))
                             (car (last calls))
                             "\n")))
        (should (string-match-p "old question" summary-prompt))
        (should-not (string-match-p "fresh prompt" summary-prompt))))))

(ert-deftest e-harness-test-auto-compaction-skips_unknown_window ()
  "Unknown model windows do not trigger auto-compaction."
  (let ((calls 0))
    (let* ((backend (e-backend-create
                     :name 'unknown-window
                     :stream
                     (cl-function
                      (lambda (&key messages options on-item)
                        (ignore messages options)
                        (setq calls (1+ calls))
                        (funcall on-item
                                 '(:type assistant-message :content "Answer."))
                        (funcall on-item '(:type done :reason stop))))))
           (harness (e-harness-create
                     :backend backend
                     :default-options '(:model "unknown-model")))
           (store (e-harness-sessions harness))
           (e-context-budget-model-token-limits nil)
           (e-harness-auto-compaction-reserve-tokens 10))
      (e-harness-create-session harness :id "session-1")
      (e-session-append-message store "session-1"
                                '(:role user :content "old question"))
      (e-session-append-activity-event
       store "session-1" "turn-1" 'token-usage
       '(:input-tokens 999999 :total-tokens 1000000))
      (e-harness-test-prompt-async harness "session-1" "fresh prompt")
      (should (equal (plist-get (e-harness-wait-batch harness "session-1" 1.0)
                                :status)
                     'done))
      (should (= calls 1))
      (should-not (e-session-compactions store "session-1")))))

(ert-deftest e-harness-test-auto-compaction_skip_no_progress_boundary ()
  "Auto-compaction skips when the prior boundary cannot move meaningfully."
  (let ((calls 0))
    (let* ((backend (e-backend-create
                     :name 'no-progress
                     :stream
                     (cl-function
                      (lambda (&key messages options on-item)
                        (ignore messages options)
                        (setq calls (1+ calls))
                        (funcall on-item
                                 '(:type assistant-message :content "Answer."))
                        (funcall on-item '(:type done :reason stop))))))
           (harness (e-harness-create
                     :backend backend
                     :default-options '(:model "auto-model")))
           (store (e-harness-sessions harness))
           (e-context-budget-model-token-limits '(("auto-model" . 100)))
           (e-harness-auto-compaction-reserve-tokens 10)
           (e-compaction-keep-recent-tokens 1000)
           events)
      (e-harness-activity-subscribe harness (lambda (event) (push event events)))
      (e-harness-create-session harness :id "session-1")
      (e-session-append-message store "session-1"
                                (list :id "kept"
                                      :role 'user
                                      :content (make-string 5000 ?k)))
      (e-session-append-compaction store "session-1" "Summary"
                                   :first-kept-entry-id "kept")
      (e-session-append-activity-event
       store "session-1" "turn-1" 'token-usage
       '(:input-tokens 95 :total-tokens 96))
      (e-harness-test-prompt-async harness "session-1" "fresh prompt")
      (should (equal (plist-get (e-harness-wait-batch harness "session-1" 1.0)
                                :status)
                     'done))
      (should (= calls 1))
      (should-not (seq-find
                   (lambda (event)
                     (eq (plist-get event :type) 'compaction-failed))
                   events))
      (should (= (length (e-session-compactions store "session-1")) 1)))))

(ert-deftest e-harness-test-auto-compaction-reuses-prompt-context-check ()
  "Prompt start does not build context twice just to check auto-compaction."
  (let* ((backend (e-backend-create
                   :name 'single-context
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (ignore messages options)
                      (funcall on-item
                               '(:type assistant-message :content "Answer."))
                      (funcall on-item '(:type done :reason stop))))))
         (harness (e-harness-create
                   :backend backend
                   :default-options '(:model "auto-model")))
         (e-context-budget-model-token-limits '(("auto-model" . 1000000)))
         (context-calls 0)
         (original-context (symbol-function 'e-harness-context)))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "short"))
    (cl-letf (((symbol-function 'e-harness-context)
               (lambda (&rest args)
                 (setq context-calls (1+ context-calls))
                 (apply original-context args))))
      (e-harness-test-prompt-async harness "session-1" "fresh prompt")
      (should (equal (plist-get (e-harness-wait-batch harness "session-1" 1.0)
                                :status)
                     'done)))
    (should (= context-calls 1))))

(ert-deftest e-harness-test-auto-compaction_expected_failure_keeps_prompt ()
  "Expected auto-compaction preparation failures do not block the prompt."
  (let ((calls 0))
    (let* ((backend (e-backend-create
                     :name 'expected-failure
                     :stream
                     (cl-function
                      (lambda (&key messages options on-item)
                        (ignore messages options)
                        (setq calls (1+ calls))
                        (funcall on-item
                                 '(:type assistant-message :content "Answer."))
                        (funcall on-item '(:type done :reason stop))))))
           (harness (e-harness-create
                     :backend backend
                     :default-options '(:model "auto-model")))
           (store (e-harness-sessions harness))
           (e-context-budget-model-token-limits '(("auto-model" . 100)))
           (e-harness-auto-compaction-reserve-tokens 10))
      (e-harness-create-session harness :id "session-1")
      (e-session-append-message store "session-1"
                                '(:role user :content "only one message"))
      (e-session-append-activity-event
       store "session-1" "turn-1" 'token-usage
       '(:input-tokens 95 :total-tokens 96))
      (e-harness-test-prompt-async harness "session-1" "fresh prompt")
      (should (equal (plist-get (e-harness-wait-batch harness "session-1" 1.0)
                                :status)
                     'done))
      (should (= calls 1))
      (should (seq-find
               (lambda (message)
                 (equal (plist-get message :content) "fresh prompt"))
               (e-session-messages store "session-1")))
      (should-not (e-session-compactions store "session-1")))))

(ert-deftest e-harness-test-compact-session-failure-does-not_append-record ()
  "Backend compaction failures leave session compactions unchanged."
  (let* ((backend (e-backend-create
                   :name 'failing-summary
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (ignore messages options on-item)
                      (signal 'user-error '("backend failed"))))))
         (harness (e-harness-create :backend backend))
         (store (e-harness-sessions harness)))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (e-session-append-message store "session-1" '(:role assistant :content "old answer"))
    (e-session-append-message store "session-1" '(:role user :content "new"))
    (should-error
     (e-harness-compact-session-batch harness "session-1" :keep-recent-tokens 1)
     :type 'user-error)
    (should-not (e-session-compactions store "session-1"))))

(ert-deftest e-harness-test-compaction-strips-tools-from-summary-request ()
  "Compaction omits the tool set so the model cannot answer with a tool-call.
Regression: when tools were exposed the summary turn could come back as a
tool-call with no assistant text, surfacing as \"Compaction backend returned
an empty summary\"."
  (let* ((seen-tools 'unset)
         (capability
          (e-capability-create
           :id 'compaction-tool-capability
           :tools (list (lambda (registry &rest _)
                          (e-tools-test-register
                           registry
                           :name "noop_tool"
                           :description "A tool that should not be offered to compaction."
                           :handler (lambda (_arguments) "noop"))))))
         (backend
          (e-backend-create
           :name 'tool-aware-summary
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages)
              (setq seen-tools (plist-get options :tools))
              (if (plist-get options :tools)
                  ;; Mirror the failure: with tools present, answer with a
                  ;; tool-call and emit no assistant text.
                  (funcall on-item '(:type tool-call :id "c1" :name "noop_tool"))
                (funcall on-item
                         '(:type assistant-message
                           :content "Old exchange summary.")))))))
         (harness (e-harness-create :backend backend))
         (store (e-harness-sessions harness)))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (e-session-append-message store "session-1"
                              '(:role assistant :content "old answer"))
    (e-session-append-message store "session-1" '(:role user :content "new"))
    ;; The harness really does have a tool registered.
    (should (e-tools-definitions (e-harness-tools harness "session-1")))
    (let ((record (e-harness-compact-session-batch
                   harness "session-1" :keep-recent-tokens 1)))
      ;; Compaction succeeds because tools were stripped from the request.
      (should (null seen-tools))
      (should (equal (plist-get record :summary) "Old exchange summary.")))))

(ert-deftest e-harness-test-compact-session-empty-summary-records-diagnostics ()
  "Empty compaction summaries record bounded backend diagnostics."
  (let* ((request (e-backend-request-create
                   :metadata '(:provider fake-summary)))
         (backend (e-backend-create
                   :name 'empty-summary
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (ignore messages options)
                      (e-backend-note-request-started request)
                      (funcall on-item
                               '(:type reasoning-delta
                                 :content "thinking"))))))
         (harness (e-harness-create :backend backend))
         (store (e-harness-sessions harness)))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (e-session-append-message store "session-1"
                              '(:role assistant :content "old answer"))
    (e-session-append-message store "session-1" '(:role user :content "new"))
    (should-error
     (e-harness-compact-session-batch harness "session-1" :keep-recent-tokens 1)
     :type 'e-compaction-error)
    (let* ((events (e-session-activity-events store "session-1"))
           (failed (seq-find
                    (lambda (event)
                      (eq (plist-get event :event-type)
                          'compaction-failed))
                    events))
           (payload (plist-get failed :payload))
           (details (plist-get payload :details)))
      (should failed)
      (should (string-match-p
               "Compaction backend returned an empty summary"
               (plist-get payload :message)))
      (should (eq (plist-get details :request-started) t))
      (should (equal (plist-get details :item-types)
                     '(reasoning-delta)))
      (should (equal (plist-get details :summary-source)
                     'none)))))

(ert-deftest e-harness-test-invalid-curation-does-not-mutate-session ()
  "Invalid reserved control stops later tools and creates no curation."
  (e-harness-test--with-empty-layer-registry
    (let* ((started nil)
           (request-count 0)
           (backend
            (e-backend-create
             :name "invalid-promotion-session"
             :context-capabilities
             '(:continuation none
               :observation-delivery request-local-replaceable
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key on-item &allow-other-keys)
                (cl-incf request-count)
                (funcall on-item
                         '(:type tool-call
                           :id "call-before-invalid-session"
                           :name "before-invalid-session"
                           :arguments (:stated_purpose "Record the result.")))
                (funcall on-item
                         '(:type context-curate
                           :arguments (:keep (999)
                                       :summaries nil)))
                (funcall on-item
                         '(:type tool-call
                           :id "call-after-invalid-session"
                           :name "after-invalid-session"
                           :arguments nil))))))
           (capability
            (e-capability-create
             :id 'invalid-promotion-tools
             :tools
             (list
              (lambda (registry)
                (e-tools-test-register
                 registry
                 :name "before-invalid-session"
                 :description "Record the ordinary call before malformed control."
                 :handler
                 (lambda (_arguments)
                   (push "before-invalid-session" started)
                   "ordinary result"))
                (e-tools-test-register
                 registry
                 :name "after-invalid-session"
                 :description "Must not run after malformed control."
                 :handler
                 (lambda (_arguments)
                   (push "after-invalid-session" started)
                   "should not run"))))))
           (harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability))))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-harness-create-session harness :id "invalid-promotion-session")
        (should-error
         (e-harness-test-prompt-batch
          harness "invalid-promotion-session" "trigger malformed control")
         :type 'e-context-lifetime-invalid-record))
      (should (= request-count 1))
      (should (equal started '("before-invalid-session")))
      (should-not
       (e-session-context-curations
        (e-harness-sessions harness) "invalid-promotion-session"))
      (should-not
       (seq-find
        (lambda (message)
          (equal (plist-get (plist-get message :content) :name)
                 "after-invalid-session"))
        (e-harness-messages harness "invalid-promotion-session")))
      (should-not
       (seq-find (lambda (message)
                 (eq (plist-get message :role) 'assistant))
                 (e-harness-messages harness "invalid-promotion-session"))))))

(provide 'e-harness-compaction-composition-test)

;;; e-harness-compaction-composition-test.el ends here
