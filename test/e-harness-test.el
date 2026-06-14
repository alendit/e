;;; e-harness-test.el --- Tests for e harness service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for core harness lifecycle behavior.

;;; Code:

(require 'ert)
(require 'seq)
(require 'e)
(require 'e-backend)
(require 'e-base)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-context)
(require 'e-dev-profile)
(require 'e-harness)
(require 'e-layers)
(require 'e-operations)
(require 'e-resources)
(require 'e-skills)
(require 'e-store)

(defconst e-harness-test--capability-config-options
  (list
   (e-capability-config-option-create
    :key :value
    :default "default"
    :validator #'stringp)
   (e-capability-config-option-create
    :key :items
    :default nil
    :normalizer #'e-capability-config-string-list
    :validator #'e-capability-config-string-list-p))
  "Option specs for harness capability config tests.")

(ert-deftest e-harness-test-capability-config-is-harness-local ()
  "Runtime capability config belongs to one harness."
  (let ((first (e-harness-create :backend (e-backend-fake-create :items nil)))
        (second (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-set-capability-config first 'dummy-config '(:value "first"))
    (e-harness-set-capability-config second 'dummy-config '(:value "second"))
    (should (equal (e-harness-capability-config first 'dummy-config)
                   '(:value "first")))
    (should (equal (e-harness-capability-config second 'dummy-config)
                   '(:value "second")))
    (e-harness-set-capability-config first 'dummy-config nil)
    (should-not (e-harness-capability-config first 'dummy-config))
    (should (equal (e-harness-capability-config second 'dummy-config)
                   '(:value "second")))))

(ert-deftest e-harness-test-effective-capability-config-uses-session-root ()
  "Effective runtime config uses session project root and harness-local config."
  (let ((directory (make-temp-file "e-harness-config-" t)))
    (unwind-protect
        (progn
          (write-region
           "((nil . ((e-capability-config . ((dummy-config :value \"project\" :items \"project-item\"))))))"
           nil
           (expand-file-name ".dir-locals.el" directory)
           nil
           'silent)
          (let* ((e-capability-config
                  '((dummy-config :value "global" :items ("global-item"))))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
            (e-harness-create-session
             harness
             :id "session-1"
             :metadata (list :project-root directory))
            (e-harness-set-capability-config
             harness
             'dummy-config
             '(:value "runtime"))
            (should
             (equal
              (e-harness-effective-capability-config
               harness
               'dummy-config
               e-harness-test--capability-config-options
               :session-id "session-1")
              '(:value "runtime" :items ("project-item"))))
            (should
             (equal
              (e-harness-effective-capability-config
               harness
               'dummy-config
               e-harness-test--capability-config-options
               :session-id "session-1"
               :overrides '(:value "explicit"))
              '(:value "explicit" :items ("project-item"))))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-capability-config-describe-uses-buffer-harness ()
  "Describing config in a chat-like buffer uses the active session root."
  (let ((project (make-temp-file "e-harness-describe-project-" t))
        (other (make-temp-file "e-harness-describe-other-" t)))
    (unwind-protect
        (progn
          (write-region
           "((nil . ((e-capability-config . ((dummy-config :value \"project\"))))))"
           nil
           (expand-file-name ".dir-locals.el" project)
           nil
           'silent)
          (let* ((harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
                 (e-capability-config '((dummy-config :value "global"))))
            (e-harness-create-session
             harness
             :id "session-1"
             :metadata (list :project-root project))
            (with-temp-buffer
              (let ((default-directory other))
                (setq-local e-current-harness harness)
                (setq-local e-chat-session-id "session-1")
                (should
                 (string-match-p
                  ":value \"project\""
                  (e-capability-config-describe
                   'dummy-config
                   nil
                   e-harness-test--capability-config-options)))))))
      (delete-directory project t)
      (delete-directory other t))))

(ert-deftest e-harness-test-prompt-writes-user-and-assistant-messages ()
  "Prompting writes user and assistant messages to the session."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (let ((messages (e-harness-messages harness "session-1")))
      (should (string-match-p "\\`[0-9A-HJKMNP-TV-Z]\\{26\\}\\'"
                              (plist-get (car messages) :turn-id)))
      (should (equal (mapcar (lambda (message) (plist-get message :role)) messages)
                     '(user assistant)))
      (should (equal (plist-get (cadr messages) :content) "answer")))
    (should (member 'turn-started (mapcar (lambda (event) (plist-get event :type)) events)))))

(ert-deftest e-harness-test-subscribe-can-filter-events-by-session ()
  "Session-scoped subscribers only receive events for their session."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (first-events nil)
         (second-events nil)
         (all-events nil))
    (e-harness-subscribe harness
                         (lambda (event) (push event first-events))
                         :session-id "session-1")
    (e-harness-subscribe harness
                         (lambda (event) (push event second-events))
                         :session-id "session-2")
    (e-harness-subscribe harness
                         (lambda (event) (push event all-events)))
    (e-harness--emit-turn-event harness "session-1" "turn-1" 'turn-started nil)
    (e-harness--emit-turn-event harness "session-2" "turn-2" 'turn-started nil)
    (should (equal (mapcar (lambda (event) (plist-get event :session-id))
                           (nreverse first-events))
                   '("session-1")))
    (should (equal (mapcar (lambda (event) (plist-get event :session-id))
                           (nreverse second-events))
                   '("session-2")))
    (should (equal (mapcar (lambda (event) (plist-get event :session-id))
                           (nreverse all-events))
                   '("session-1" "session-2")))
    (dolist (event all-events)
      (should (plist-get event :id))
      (should (plist-get event :type))
      (should (plist-get event :session-id))
      (should (plist-member event :turn-id))
      (should (plist-member event :payload))
      (should (plist-get event :created-at)))))

(ert-deftest e-harness-test-unsubscribe-removes-subscription-idempotently ()
  "Unsubscribing removes a subscription record and can be repeated."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (events nil)
         (subscription (e-harness-subscribe
                        harness
                        (lambda (event) (push event events))
                        :session-id "session-1")))
    (e-harness--emit-turn-event harness "session-1" "turn-1" 'turn-started nil)
    (should (= (length events) 1))
    (e-harness-unsubscribe harness subscription)
    (e-harness--emit-turn-event harness "session-1" "turn-2" 'turn-started nil)
    (should (= (length events) 1))
    (should-not (member subscription (e-harness-subscribers harness)))
    (e-harness-unsubscribe harness subscription)
    (should (= (length events) 1))))

(ert-deftest e-harness-test-abort-idle-session-is-explicit-error ()
  "Aborting without an active turn surfaces a lifecycle error."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-harness-abort harness "session-1")
     :type 'e-harness-no-active-turn)))

(ert-deftest e-harness-test-async-prompt-wait-settles-turn ()
  "Async prompting tracks an active turn until wait settles it."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id (e-harness-prompt-async harness "session-1" "question")))
      (should (equal (plist-get (e-harness-state harness "session-1")
                                :active-turn)
                     turn-id))
      (should (equal (plist-get (e-harness-wait harness "session-1" 1.0)
                                :status)
                     'done))
      (should (equal (plist-get (e-harness-state harness "session-1")
                                :active-turn)
                     nil)))))

(ert-deftest e-harness-test-async-prompt-appends-user-message-immediately ()
  "Async prompting records the user message before the backend timer runs."
  (let* ((called nil)
         (backend (e-backend-create
                   :name "delayed"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages options on-item)
                              (setq called t)))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question" :delay 1.0)
    (should (equal called nil))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user)))
    (should (member 'message-added
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))
    (e-harness-abort harness "session-1")))

(ert-deftest e-harness-test-abort-cancels-queued-async-turn ()
  "Aborting a queued async turn settles it as cancelled."
  (let* ((called nil)
         (backend (e-backend-create
                   :name "slow"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages options on-item)
                              (setq called t)))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question" :delay 1.0)
    (e-harness-abort harness "session-1")
    (should (equal (plist-get (e-harness-wait harness "session-1" 0.1)
                              :status)
                   'cancelled))
    (should (equal called nil))))

(ert-deftest e-harness-test-abort-cancels-active-provider-request ()
  "Aborting an active async provider call cancels its request handle."
  (let* ((cancelled nil)
         (backend
          (e-backend-create
           :name "cancellable"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (e-backend-note-request-started
               (e-backend-request-create
                :cancel (lambda ()
                          (setq cancelled t)
                          t)))
              (while (not cancelled)
                (accept-process-output nil 0.01))
              (funcall on-item '(:type done :reason cancelled))))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question")
    (run-at-time 0.01 nil (lambda ()
                            (e-harness-abort harness "session-1")))
    (should (equal (plist-get (e-harness-wait harness "session-1" 1.0)
                              :status)
                   'cancelled))
    (should cancelled)
    (should (member 'turn-cancelled
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))))

(ert-deftest e-harness-test-abort-settles-when-provider-cancel-errors ()
  "Abort still settles the turn when provider cancellation raises."
  (let* ((cancel-called nil)
         (backend
          (e-backend-create
           :name "bad-cancel"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-item on-done on-error)
              (let ((request
                     (e-backend-request-create
                      :cancel (lambda ()
                                (setq cancel-called t)
                                (error "cancel failed")))))
                (funcall on-request-start request)
                request)))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question")
    (should (e-harness-abort harness "session-1"))
    (should (equal (plist-get (e-harness-wait harness "session-1" 0.1)
                              :status)
                   'cancelled))
    (should cancel-called)
    (should (member 'turn-cancelled
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))))

(ert-deftest e-harness-test-async-provider-error-is-surfaced ()
  "Async provider failures settle as errors and emit turn-failed."
  (let* ((backend (e-backend-create
                   :name "failing"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages options on-item)
                              (error "provider failed")))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait harness "session-1" 1.0)))
      (should (equal (plist-get settled :status) 'error))
      (should (string-match-p "provider failed" (plist-get settled :error))))
    (should (member 'turn-failed
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))))

(ert-deftest e-harness-test-backend-error-details-are-surfaced ()
  "Structured backend error details survive in the turn-failed payload."
  (let* ((backend (e-backend-fake-create
                   :items '((:type backend-error
                              :content "provider failed"
                              :payload (:provider-error full)))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait harness "session-1" 1.0)))
      (should (equal (plist-get settled :status) 'error))
      (should (equal (plist-get settled :error) "provider failed"))
      (should (equal (plist-get settled :error-details)
                     '(:provider-error full))))
    (let* ((failed-event
            (seq-find (lambda (event)
                        (eq (plist-get event :type) 'turn-failed))
                      events))
           (payload (plist-get failed-event :payload)))
      (should (equal (plist-get payload :error) "provider failed"))
      (should (equal (plist-get payload :details)
                     '(:provider-error full))))))

(ert-deftest e-harness-test-async-prompt-rejects-concurrent-session-turn ()
  "A session cannot start a second async turn while the first is running."
  (let* ((finish nil)
         (backend (e-backend-create
                   :name "held"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore messages options on-error on-request-start)
                      (setq finish
                            (lambda ()
                              (funcall on-item
                                       '(:type assistant-message
                                         :content "answer"))
                              (funcall on-item
                                       '(:type done :reason stop))
                              (funcall on-done '(:status done))))
                      nil))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "first")
    (should-error
     (e-harness-prompt-async harness "session-1" "second")
     :type 'e-harness-active-turn-exists)
    (funcall finish)
    (should (equal (plist-get (e-harness-wait harness "session-1" 1.0)
                              :status)
                   'done))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant)))))


(ert-deftest e-harness-test-tool-finished-activity-compacts-result-payload ()
  "Durable tool-finished activity avoids duplicating full tool result output."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (large-output (make-string 64 ?x)))
    (e-harness-create-session harness :id "session-1")
    (let ((e-harness-durable-tool-finished-result-preview-bytes 8))
      (e-harness--emit-turn-event
       harness
       "session-1"
       "turn-1"
       'tool-finished
       (list :tool-call '(:id "call-1" :name "bash")
             :result (list :tool-call-id "call-1"
                           :name "bash"
                           :status 'ok
                           :content large-output
                           :metadata '(:tmp-uri "tmp://full.txt"))))
      (let* ((event (car (e-harness-session-activity-events
                          harness
                          "session-1")))
             (payload (plist-get event :payload))
             (result (plist-get payload :result))
             (metadata (plist-get result :metadata)))
        (should (equal (plist-get result :content) "xxxxxxxx"))
        (should (plist-get metadata :activity-truncated))
        (should (equal (plist-get metadata :activity-original-bytes) 64))
        (should (equal (plist-get metadata :tmp-uri) "tmp://full.txt"))))))

(ert-deftest e-harness-test-token-usage-events-are-durable ()
  "Backend token usage events are retained in session activity."
  (let* ((backend (e-backend-fake-create
                   :items
                   '((:type assistant-message :content "answer")
                     (:type token-usage
                      :usage (:input-tokens 202598
                              :cached-input-tokens 7552
                              :output-tokens 419
                              :reasoning-output-tokens 139
                              :total-tokens 203017))
                     (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (let* ((events (e-harness-session-activity-events harness "session-1"))
           (usage-event
            (seq-find (lambda (event)
                        (eq (plist-get event :event-type) 'token-usage))
                      events)))
      (should usage-event)
      (should (equal (plist-get usage-event :payload)
                     '(:input-tokens 202598
                       :cached-input-tokens 7552
                       :output-tokens 419
                       :reasoning-output-tokens 139
                       :total-tokens 203017))))))

(ert-deftest e-harness-test-provider-request-lifecycle-events-are-durable ()
  "Provider request lifecycle events are retained in ordered session activity."
  (let* ((backend
          (e-backend-create
           :name "durable-lifecycle"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-error)
              (funcall on-request-start
                       (e-backend-request-create
                        :metadata '(:provider codex
                                    :transport url-retrieve
                                    :url-host "example.test"
                                    :url-path "/codex/responses"
                                    :timeout-seconds 180)))
              (funcall on-item '(:type assistant-message :content "answer"))
              (funcall on-item '(:type done :reason stop))
              (funcall on-done '(:status done))
              nil))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (let* ((activity (e-harness-session-activity-events harness "session-1"))
           (types (mapcar (lambda (event)
                            (plist-get event :event-type))
                          activity))
           (started (seq-find
                     (lambda (event)
                       (eq (plist-get event :event-type)
                           'provider-request-started))
                     activity))
           (finished (seq-find
                      (lambda (event)
                        (eq (plist-get event :event-type)
                            'provider-request-finished))
                      activity)))
      (should (equal types
                     '(turn-started
                       provider-request-started
                       provider-request-finished
                       turn-finished)))
      (should (equal (plist-get (plist-get started :payload) :provider)
                     'codex))
      (should (equal (plist-get (plist-get finished :payload) :status)
                     'done))
      (should (numberp (plist-get (plist-get finished :payload)
                                  :elapsed-seconds))))))

(ert-deftest e-harness-test-provider-timeout-settles-as-failed-turn ()
  "Provider timeout errors settle as turn-failed, not turn-cancelled."
  (let* ((backend
          (e-backend-create
           :name "timeout"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-item on-done)
              (funcall on-request-start
                       (e-backend-request-create
                        :metadata '(:provider codex
                                    :transport url-retrieve
                                    :url-host "example.test"
                                    :url-path "/codex/responses"
                                    :timeout-seconds 180)))
              (run-at-time 0 nil
                           (lambda ()
                             (funcall on-error
                                      '(e-openai-request-timeout
                                        "provider timed out"))))
              nil))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait harness "session-1" 1.0)))
      (should (equal (plist-get settled :status) 'error))
      (should (string-match-p "provider timed out"
                              (plist-get settled :error))))
    (let* ((activity (e-harness-session-activity-events harness "session-1"))
           (types (mapcar (lambda (event)
                            (plist-get event :event-type))
                          activity)))
      (should (equal types
                     '(turn-started
                       provider-request-started
                       provider-request-finished
                       turn-failed)))
      (should-not (member 'turn-cancelled types)))))

(ert-deftest e-harness-test-abort-ignores-stale-provider-callbacks ()
  "Provider callbacks that arrive after abort do not mutate session state."
  (let* ((callbacks nil)
         (cancelled nil)
         (backend (e-backend-create
                   :name "stale-callback"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore messages options on-error)
                      (setq callbacks (list :on-item on-item
                                            :on-done on-done))
                      (let ((request
                             (e-backend-request-create
                              :cancel (lambda ()
                                        (setq cancelled t)
                                        t))))
                        (funcall on-request-start request)
                        request)))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question")
    (e-harness-abort harness "session-1")
    (funcall (plist-get callbacks :on-item)
             '(:type assistant-message :content "late answer"))
    (funcall (plist-get callbacks :on-item)
             '(:type done :reason stop))
    (funcall (plist-get callbacks :on-done) '(:status done))
    (should (equal (plist-get (e-harness-wait harness "session-1" 0.1)
                              :status)
                   'cancelled))
    (should cancelled)
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user)))
    (should-not (member 'turn-finished
                        (mapcar (lambda (event) (plist-get event :type))
                                events)))))

(ert-deftest e-harness-test-abort-cancels-active-tool-request ()
  "Aborting during async tool execution cancels the active tool request."
  (let* ((tool-callbacks nil)
         (tool-cancelled nil)
         (backend (e-backend-create
                   :name "tool-abort"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore messages options on-error on-request-start)
                      (funcall on-item
                               '(:type tool-call
                                 :id "call-1"
                                 :name "held-tool"
                                 :arguments (:text "hi")))
                      (funcall on-item '(:type done :reason tool-use))
                      (funcall on-done '(:status done))
                      nil))))
         (tools (e-tools-registry-create))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-tools-register tools
                      :name "held-tool"
                      :description "Hold."
                      :start
                      (cl-function
                       (lambda (&key arguments on-done on-error
                                      on-request-start)
                         (ignore arguments on-error)
                         (setq tool-callbacks (list :on-done on-done))
                         (let ((request
                                (e-tools-request-create
                                 :cancel (lambda ()
                                           (setq tool-cancelled t)
                                           t))))
                           (funcall on-request-start request)
                           request))))
    (cl-letf (((symbol-function 'e-harness-tools)
               (lambda (_harness &optional _session-id _turn-id) tools)))
      (e-harness-subscribe harness (lambda (event) (push event events)))
      (e-harness-create-session harness :id "session-1")
      (e-harness-prompt-async harness "session-1" "question")
      (should tool-callbacks)
      (e-harness-abort harness "session-1")
      (funcall (plist-get tool-callbacks :on-done) "late result")
      (should (equal (plist-get (e-harness-wait harness "session-1" 0.1)
                                :status)
                     'cancelled))
      (should tool-cancelled)
      (should (member 'turn-cancelled
                      (mapcar (lambda (event) (plist-get event :type))
                              events)))
      (should (member 'tool-finished
                      (mapcar (lambda (event) (plist-get event :type))
                              events)))
      (let* ((messages (e-harness-messages harness "session-1"))
             (tool-result (plist-get (nth 2 messages) :content)))
        (should (equal (mapcar (lambda (message) (plist-get message :role))
                               messages)
                       '(user tool-call tool)))
        (should (equal (plist-get tool-result :tool-call-id) "call-1"))
        (should (eq (plist-get tool-result :status) 'error))
        (should (equal (plist-get tool-result :content) "Cancelled"))))))

(ert-deftest e-harness-test-follow-up-appends-user-message ()
  "Follow-up submits another turn against the same session."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "first")
    (e-harness-follow-up harness "session-1" "second")
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant user assistant)))))

(ert-deftest e-harness-test-reset-clears-session-messages ()
  "Reset clears transcript messages for a session."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create
                            :items '((:type assistant-message :content "answer")
                                     (:type done :reason stop))))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (e-harness-reset harness "session-1")
    (should (equal (e-harness-messages harness "session-1") nil))))

(ert-deftest e-harness-test-state-reports-session-and-active-turn ()
  "Harness state reports settled session status."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (should (equal (e-harness-state harness "session-1")
                   '(:session-id "session-1" :active-turn nil :message-count 0)))))

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
    (e-harness-prompt harness "session-1" "raw prompt")
    (should (equal captured-messages
                   '((:role user :content "from context"))))))

(ert-deftest e-harness-test-context-builds-current-session-preview ()
  "Context preview returns the same messages and options a turn would use."
  (let* ((provider (e-context-provider-create
                    :name 'test-provider
                    :build (cl-function
                            (lambda (&key harness session-id turn-id)
                              (ignore harness session-id turn-id)
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
                   :active-layers (list layer))))
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

(ert-deftest e-harness-test-profile-records-context-build ()
  "Enabled dev profiling records harness context spans."
  (let* ((profile-directory (make-temp-file "e-harness-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-dev-profile-start)
          (e-harness-context harness "session-1" "turn-1")
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "harness.context" aggregates nil nil #'equal))))
      (delete-directory profile-directory t))))

(ert-deftest e-harness-test-activate-capability-registers-tools-and-context ()
  "Direct capability activation registers capability contributions."
  (let* ((capability
          (e-capability-create
           :id 'direct-capability
           :instructions "direct instructions"
           :tools (list (lambda (registry)
                          (e-tools-register
                           registry
                           :name "direct_tool"
                           :description "Direct capability tool."
                           :handler (lambda (_arguments) "direct"))))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "hello"))
    (let ((context (e-harness-context harness "session-1")))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
                             (plist-get context :messages))
                     '("direct instructions" "hello"))))
    (should (equal (mapcar (lambda (definition)
                             (plist-get definition :name))
                           (e-tools-definitions (e-harness-tools harness)))
                   '("direct_tool")))))

(ert-deftest e-harness-test-active-capabilities-are-derived-from-layers ()
  "Active capabilities are a view over active layers, not duplicated state."
  (let* ((first-capability (e-capability-create :id 'first-capability))
         (second-capability (e-capability-create :id 'second-capability))
         (layer (e-layer-create
                 :id 'derived-layer
                 :name "Derived Layer"
                 :capabilities (list first-capability second-capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-layer harness layer)
    (should (equal (mapcar #'e-capability-id
                           (e-harness-active-capabilities harness))
                   '(first-capability second-capability)))))

(ert-deftest e-harness-test-tools-are-derived-from-active-layers ()
  "The harness tool surface is rebuilt from active layers on demand."
  (let* ((capability
          (e-capability-create
           :id 'tool-capability
           :tools (list (lambda (registry)
                          (e-tools-register
                           registry
                           :name "derived_tool"
                           :description "Derived tool."
                           :handler (lambda (_arguments) "derived"))))))
         (layer (e-layer-create
                 :id 'tool-layer
                 :name "Tool Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (stale-tools (e-harness-tools harness)))
    (e-harness-activate-layer harness layer)
    (should-not (e-tools-definitions stale-tools))
    (should (equal (mapcar (lambda (definition)
                             (plist-get definition :name))
                           (e-tools-definitions (e-harness-tools harness)))
                   '("derived_tool")))
    (should (equal (plist-get
                    (e-tools-execute
                     (e-harness-tools harness)
                     '(:id "call-1"
                       :name "derived_tool"
                       :arguments nil))
                    :content)
                   "derived"))))

(ert-deftest e-harness-test-hooks-are-derived-from-active-layers ()
  "Harness hook registries are derived from active capability layers."
  (should (require 'e-hooks nil t))
  (let* ((capability
          (e-capability-create
           :id 'hook-capability
           :hooks
           (list (e-hook-create
                  :id "50-hook"
                  :point :post-tool-call
                  :handler (lambda (value _context)
                             (concat value "-hooked"))))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers
                   (list (e-layer-create
                          :id 'hook-layer
                          :name "Hook Layer"
                          :capabilities (list capability))))))
    (should (equal (e-hooks-run-reduce
                    (e-harness-hooks harness)
                    :post-tool-call
                    "value"
                    nil)
                   "value-hooked"))))

(ert-deftest e-harness-test-tool-lifecycle-runs-pre-and-post-hooks ()
  "Harness tool lifecycle applies active pre and post tool hooks."
  (should (require 'e-hooks nil t))
  (let* ((calls 0)
         (backend
          (e-backend-create
           :name "fake-harness-hooks"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (setq calls (1+ calls))
              (if (= calls 1)
                  (progn
                    (funcall on-item
                             '(:type tool-call
                               :id "call-1"
                               :name "echo"
                               :arguments (:text "raw")))
                    (funcall on-item '(:type done :reason tool-use)))
                (funcall on-item
                         '(:type assistant-message :content "done"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools-capability
          (e-capability-create
           :id 'echo-tool
           :tools
           (list (lambda (registry)
                   (e-tools-register
                    registry
                    :name "echo"
                    :description "Echo text."
                    :handler (lambda (arguments)
                               (plist-get arguments :text)))))))
         (harness nil)
         (hooks-capability
          (e-capability-create
           :id 'tool-hooks
           :hooks
           (list
            (e-hook-create
             :id "10-prepare"
             :point :pre-tool-call
             :handler (lambda (tool-call context)
                        (should (eq (plist-get context :harness) harness))
                        (plist-put (copy-sequence tool-call)
                                   :arguments '(:text "prepared"))))
            (e-hook-create
             :id "50-shape-result"
             :point :post-tool-call
             :handler (lambda (result context)
                        (should (equal (plist-get context :session-id)
                                       "session-1"))
                        (plist-put (copy-sequence result)
                                   :content
                                   (concat (plist-get result :content)
                                           "-post"))))))))
    (setq harness
          (e-harness-create
           :backend backend
           :active-layers
           (list (e-layer-create
                  :id 'tool-layer
                  :name "Tool Layer"
                  :capabilities (list tools-capability
                                      hooks-capability)))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "use tool")
    (let* ((messages (e-harness-messages harness "session-1"))
           (tool-call (cl-find 'tool-call messages
                               :key (lambda (message)
                                      (plist-get message :role))))
           (tool-result (cl-find 'tool messages
                                 :key (lambda (message)
                                        (plist-get message :role)))))
      (should (equal (plist-get (plist-get (plist-get tool-call :content)
                                           :arguments)
                                :text)
                     "prepared"))
      (should (equal (plist-get (plist-get tool-result :content) :content)
                     "prepared-post")))))

(ert-deftest e-harness-test-profile-records-tool-start ()
  "Enabled dev profiling records harness tool start spans."
  (let* ((profile-directory (make-temp-file "e-harness-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (calls 0)
         (backend
          (e-backend-create
           :name "fake-harness-profile-tool"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (setq calls (1+ calls))
              (if (= calls 1)
                  (progn
                    (funcall on-item
                             '(:type tool-call
                               :id "call-1"
                               :name "echo"
                               :arguments (:text "raw")))
                    (funcall on-item '(:type done :reason tool-use)))
                (funcall on-item
                         '(:type assistant-message :content "done"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools-capability
          (e-capability-create
           :id 'echo-tool
           :tools
           (list (lambda (registry)
                   (e-tools-register
                    registry
                    :name "echo"
                    :description "Echo text."
                    :handler (lambda (arguments)
                               (plist-get arguments :text)))))))
         (harness
          (e-harness-create
           :backend backend
           :active-layers
           (list (e-layer-create
                  :id 'tool-layer
                  :name "Tool Layer"
                  :capabilities (list tools-capability))))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-dev-profile-start)
          (e-harness-prompt harness "session-1" "use tool")
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "harness.tool-start" aggregates nil nil #'equal))))
      (delete-directory profile-directory t))))

(ert-deftest e-harness-test-resources-are-derived-from-active-layers ()
  "The harness resource surface is rebuilt from active layers on demand."
  (let* ((capability
          (e-capability-create
           :id 'resource-capability
           :resource-methods
           (list (lambda (registry)
                   (e-resources-register
                    registry
                    (e-resource-method-create
                     :scheme "derived"
                     :operation e-operation-read
                     :description "Derived resources."
                     :handler (lambda (_uri _range) "derived")))))))
         (layer (e-layer-create
                 :id 'resource-layer
                 :name "Resource Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (stale-resources (e-harness-resources harness)))
    (e-harness-activate-layer harness layer)
    (should-error (e-resources-read stale-resources "derived://value" nil)
                  :type 'e-resources-unknown-scheme)
    (should (equal (e-resources-read
                    (e-harness-resources harness)
                    "derived://value"
                    nil)
                   "derived"))))



(ert-deftest e-harness-test-bash-tools-prefer-session-project-root ()
  "Session-scoped bash tools run in the session project root."
  (let* ((fallback-root (make-temp-file "e-harness-fallback-" t))
         (project-root (make-temp-file "e-harness-project-" t))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers (list (e-base-layer-create fallback-root)))))
    (unwind-protect
        (progn
          (e-harness-create-session
           harness
           :id "session-1"
           :metadata (list :project-root project-root))
          (let ((result (e-tools-execute
                         (e-harness-tools harness "session-1" "turn-1")
                         '(:id "call-1"
                           :name "bash"
                           :arguments (:command "pwd")))))
            (should (equal (string-trim (plist-get result :content))
                           (directory-file-name project-root)))))
      (delete-directory fallback-root t)
      (delete-directory project-root t))))

(ert-deftest e-harness-test-file-resources-prefer-session-project-root ()
  "Session-scoped file resources resolve against the session project root."
  (let* ((fallback-root (make-temp-file "e-harness-fallback-" t))
         (project-root (make-temp-file "e-harness-project-" t))
         (nested (expand-file-name "docs/feature" project-root))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers (list (e-base-layer-create fallback-root)))))
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "rooted" nil
                        (expand-file-name "README.md" project-root)
                        nil 'silent)
          (e-harness-create-session
           harness
           :id "session-1"
           :metadata (list :project-root project-root))
          (should (equal (e-resources-read
                          (e-harness-resources harness "session-1" "turn-1")
                          "file://README.md"
                          nil)
                         "rooted")))
      (delete-directory fallback-root t)
      (delete-directory project-root t))))

(ert-deftest e-harness-test-built-in-resource-tools-dispatch-through-resources ()
  "Resource operation tools dispatch through active resource methods."
  (let* ((calls nil)
         (capability
          (e-capability-create
           :id 'resource-tool-capability
           :resource-methods
           (list (lambda (registry)
                   (dolist (method
                            (list
                             (e-resource-method-create
                              :scheme "test"
                              :operation e-operation-read
                              :description "Readable test resources."
                              :uri-patterns '("test://<value>")
                              :range-modes '("line")
                              :handler (lambda (uri range)
                                         (push (list :read uri range) calls)
                                         "read-result"))
                             (e-resource-method-create
                              :scheme "test"
                              :operation e-operation-write
                              :description "Writable test resources."
                              :uri-patterns '("test://<value>")
                              :handler (lambda (uri content)
                                         (push (list :write uri content) calls)
                                         "write-result"))
                             (e-resource-method-create
                              :scheme "test"
                              :operation e-operation-edit
                              :description "Editable test resources."
                              :uri-patterns '("test://<value>")
                              :handler (lambda (uri edits)
                                         (push (list :edit uri edits) calls)
                                         "edit-result"))))
                     (e-resources-register registry method))))))
         (layer (e-layer-create
                 :id 'resource-tool-layer
                 :name "Resource Tool Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers (list layer)))
         (tools (e-harness-tools harness)))
    (should (equal (plist-get
                    (e-tools-execute
                     tools
                     '(:id "call-1"
                       :name "read"
                       :arguments (:uri "test://value"
                                   :range (:unit "line" :start 1 :end 2))))
                    :content)
                   "read-result"))
    (should (equal (plist-get
                    (e-tools-execute
                     tools
                     '(:id "call-2"
                       :name "write"
                       :arguments (:uri "test://value" :content "content")))
                    :content)
                   "write-result"))
    (should (equal (plist-get
                    (e-tools-execute
                     tools
                     '(:id "call-3"
                       :name "edit"
                       :arguments (:uri "test://value"
                                   :edits ((:oldText "a" :newText "b")))))
                    :content)
                   "edit-result"))
    (should (equal (nreverse calls)
                   '((:read (:scheme "test" :address "value" :uri "test://value")
                            (:unit "line" :start 1 :end 2))
                     (:write (:scheme "test" :address "value" :uri "test://value")
                             "content")
                     (:edit (:scheme "test" :address "value" :uri "test://value")
                            ((:oldText "a" :newText "b"))))))))

(ert-deftest e-harness-test-resource-tool-descriptions-include-active-methods ()
  "Generated operation tool descriptions include active URI scheme metadata."
  (let* ((capability
          (e-capability-create
           :id 'resource-description-capability
           :resource-methods
           (list (lambda (registry)
                   (e-resources-register
                    registry
                    (e-resource-method-create
                     :scheme "described"
                     :operation e-operation-read
                     :description "Described resources."
                     :uri-patterns '("described://<id>")
                     :range-modes '("line" "offset")
                     :handler (lambda (_uri _range) "ok")))))))
         (layer (e-layer-create
                 :id 'resource-description-layer
                 :name "Resource Description Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers (list layer)))
         (read-tool (seq-find (lambda (definition)
                                (equal (plist-get definition :name) "read"))
                              (e-tools-definitions (e-harness-tools harness))))
         (description (plist-get read-tool :description)))
    (should read-tool)
    (should (string-match-p "described://<id>" description))
    (should (string-match-p "Described resources" description))
    (should (string-match-p "line, offset" description))))

(ert-deftest e-harness-test-write-tool-description-states-create-invariant ()
  "Generated write descriptions state the shared create/overwrite contract."
  (let* ((capability
          (e-capability-create
           :id 'write-description-capability
           :resource-methods
           (list (lambda (registry)
                   (e-resources-register
                    registry
                    (e-resource-method-create
                     :scheme "writable"
                     :operation e-operation-write
                     :description "Writable resources."
                     :uri-patterns '("writable://<id>")
                     :handler (lambda (_uri _content) "ok")))))))
         (layer (e-layer-create
                 :id 'write-description-layer
                 :name "Write Description Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers (list layer)))
         (write-tool (seq-find (lambda (definition)
                                 (equal (plist-get definition :name) "write"))
                               (e-tools-definitions (e-harness-tools harness))))
         (description (plist-get write-tool :description)))
    (should write-tool)
    (should
     (string-match-p
      "write creates missing parent paths and the target resource, or overwrites existing content"
      description))
    (should (string-match-p "writable://<id>" description))))

(ert-deftest e-harness-test-direct-capability-activation-uses-layer-source ()
  "Direct capability activation wraps the capability as a layer."
  (let* ((capability (e-capability-create :id 'direct-capability))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness capability)
    (should (equal (mapcar #'e-layer-id
                           (e-harness-active-layers harness))
                   '(direct-capability)))
    (should (equal (mapcar #'e-capability-id
                           (e-harness-active-capabilities harness))
                   '(direct-capability)))))

(ert-deftest e-harness-test-store-is-derived-from-active-layers ()
  "Harness e:// stores are derived from active capability resource contributions."
  (let* ((capability
          (e-capability-with-skills-create
           :id 'skill-capability
           :skills
           (list
            (e-skill-spec-create
             :name "focused-work"
             :description "Use for focused work."
             :content "Stay focused."))))
         (layer (e-layer-create
                 :id 'skill-layer
                 :name "Skill Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-layer harness layer)
    (should (equal (mapcar #'e-store-entry-uri
                           (e-store-list (e-harness-store harness)))
                   '("e://skill-capability/skills/focused-work")))))

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
    (e-harness-activate-layer harness layer)
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
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

(ert-deftest e-harness-test-built-in-read-loads-skill-resource ()
  "The built-in read tool can load full skill instructions on demand."
  (let* ((capability
          (e-capability-with-skills-create
           :id 'skill-capability
           :skills
           (list
            (e-skill-spec-create
             :name "planner"
             :description "Plan work."
             :content "Full planning instructions."))))
         (layer (e-layer-create
                 :id 'skill-layer
                 :name "Skill Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (read-call '(:id "call-1"
                      :name "read"
                      :arguments (:uri "e://skill-capability/skills/planner"))))
    (e-harness-activate-layer harness layer)
    (let ((result (e-tools-execute (e-harness-tools harness) read-call)))
      (should (equal (plist-get result :status) 'ok))
      (should (equal (plist-get result :content)
                     "Full planning instructions.")))))

(ert-deftest e-harness-test-built-in-read-loads-reference-resource ()
  "The built-in read tool can load capability reference resources on demand."
  (let* ((capability
          (e-capability-create
           :id 'reference-capability
           :resources
           (list (lambda (store capability)
                   (e-store-register
                    store
                    (e-capability-id capability)
                    "refs/guide.md"
                    :description "Reference guide."
                    :content "Reference guide content.")))))
         (layer (e-layer-create
                 :id 'reference-layer
                 :name "Reference Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (read-call '(:id "call-1"
                      :name "read"
                      :arguments (:uri
                                  "e://reference-capability/refs/guide.md"))))
    (e-harness-activate-layer harness layer)
    (let ((result (e-tools-execute (e-harness-tools harness) read-call)))
      (should (equal (plist-get result :status) 'ok))
      (should (equal (plist-get result :content)
                     "Reference guide content.")))))

(ert-deftest e-harness-test-skill-resources-do-not-support-write-or-edit ()
  "Skill resources are read-only even when advertised through resource tools."
  (let* ((capability
          (e-capability-with-skills-create
           :id 'skill-capability
           :skills
           (list
            (e-skill-spec-create
             :name "readonly"
             :description "Read-only skill."
             :content "Read-only instructions."))))
         (layer (e-layer-create
                 :id 'skill-layer
                 :name "Skill Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-layer harness layer)
    (should-error
     (e-resources-write (e-harness-resources harness)
                        "e://skill-capability/skills/readonly"
                        "Replacement")
     :type 'e-resources-unsupported-operation)
    (should-error
     (e-resources-edit (e-harness-resources harness)
                       "e://skill-capability/skills/readonly"
                       '((:oldText "Read" :newText "Write")))
     :type 'e-resources-unsupported-operation)))

(ert-deftest e-harness-test-derived-views-do-not-keep-struct-compiler-macros ()
  "Derived harness view functions must not expand into stale struct slots."
  (should-not (get 'e-harness-active-capabilities 'compiler-macro))
  (should-not (get 'e-harness-store 'compiler-macro))
  (should-not (get 'e-harness-resources 'compiler-macro))
  (should-not (get 'e-harness-tools 'compiler-macro)))

(ert-deftest e-harness-test-prompt-passes-tool-definitions-as-options ()
  "Prompting includes registered tool definitions in backend options."
  (let* ((captured-options nil)
         (backend (e-backend-create
                   :name "capture"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages)
                              (setq captured-options options)
                              (funcall on-item
                                       '(:type assistant-message :content "ok"))
                              (funcall on-item '(:type done :reason stop))))))
         (capability
          (e-capability-create
           :id 'noop-capability
           :tools (list (lambda (registry)
                          (e-tools-register
                           registry
                           :name "noop"
                           :description "Accept no arguments."
                           :parameters '(:type "object" :properties nil)
                           :handler (lambda (_arguments) "now"))))))
         (layer (e-layer-create
                 :id 'noop-layer
                 :name "Noop Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create :backend backend)))
    (e-harness-activate-layer harness layer)
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "raw prompt")
    (let* ((tool (seq-find (lambda (definition)
                             (equal (plist-get definition :name) "noop"))
                           (plist-get captured-options :tools)))
           (parameters (plist-get tool :parameters)))
      (should tool)
      (should (equal (plist-get parameters :type) "object"))
      (should (hash-table-p (plist-get parameters :properties)))
      (should (equal tool
                     `(:type "function"
                       :name "noop"
                       :description "Accept no arguments."
                       :parameters ,parameters
                       :strict :json-false))))))

(ert-deftest e-harness-test-session-options-override-default-options ()
  "Session-specific turn options override harness defaults and keep tools."
  (let* ((captured-options nil)
         (backend (e-backend-create
                   :name "capture"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages)
                              (setq captured-options options)
                              (funcall on-item
                                       '(:type assistant-message :content "ok"))
                              (funcall on-item '(:type done :reason stop))))))
         (capability
          (e-capability-create
           :id 'noop-capability
           :tools (list (lambda (registry)
                          (e-tools-register
                           registry
                           :name "noop"
                           :description "Accept no arguments."
                           :parameters '(:type "object" :properties nil)
                           :handler (lambda (_arguments) "now"))))))
         (layer (e-layer-create
                 :id 'noop-layer
                 :name "Noop Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend backend
                   :default-options '(:model "default-model"
                                      :reasoning-effort "medium"))))
    (e-harness-activate-layer harness layer)
    (e-harness-create-session harness :id "session-1")
    (e-harness-set-session-model harness "session-1" "session-model")
    (e-harness-set-session-reasoning-effort harness "session-1" "high")
    (e-harness-prompt harness "session-1" "raw prompt")
    (should (equal (plist-get captured-options :model) "session-model"))
    (should (equal (plist-get captured-options :reasoning-effort) "high"))
    (should (plist-get captured-options :tools))))

(ert-deftest e-harness-test-session-option-changes-emit-events ()
  "Changing session options emits a core event."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-set-session-model harness "session-1" "gpt-test")
    (let ((event (car events)))
      (should (eq (plist-get event :type) 'session-options-changed))
      (should (equal (plist-get event :session-id) "session-1"))
      (should (equal (plist-get (plist-get event :payload) :turn-options)
                     '(:model "gpt-test"))))))

(ert-deftest e-harness-test-session-projection-accessors ()
  "Harness exposes public read-only projections for presentation shells."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options '(:model "default-model"))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     store "session-1" '(:id "msg-1" :role user :content "hello title"))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'tool-started '(:name "tool"))
    (e-harness-set-session-model harness "session-1" "gpt-test")
    (should (equal (e-harness-session-title harness "session-1")
                   "hello title"))
    (should (equal (mapcar (lambda (session) (plist-get session :id))
                           (e-harness-session-list harness))
                   '("session-1")))
    (should (equal (mapcar (lambda (event) (plist-get event :event-type))
                           (e-harness-session-activity-events
                            harness "session-1"))
                   '(tool-started)))
    (let ((options (e-harness-turn-options harness "session-1")))
      (should (equal (plist-get options :model) "gpt-test"))
      (should-not (plist-get options :tools)))))

(ert-deftest e-harness-test-persists-activity-events-and-tags_turn_messages ()
  "Harness turn events persist as activity, and messages keep their turn id."
  (let* ((backend (e-backend-fake-create
                   :items '((:type reasoning-delta :content "thinking")
                            (:type assistant-message :content "done")
                            (:type done :reason stop))))
         (store (e-session-store-create))
         (harness (e-harness-create :backend backend :sessions store)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "hello")
    (let ((messages (e-session-messages store "session-1"))
          (events (e-session-activity-events store "session-1")))
      (should (equal (length (delete-dups
                              (mapcar (lambda (message)
                                        (plist-get message :turn-id))
                                      messages)))
                     1))
      (should (equal (mapcar (lambda (event)
                               (plist-get event :event-type))
                             events)
                     '(turn-started
                       provider-request-started
                       reasoning-delta
                       provider-request-finished
                       turn-finished))))))

(ert-deftest e-harness-test-activity-index-write-is-coalesced ()
  "Harness activity events flush the session index once at turn settlement."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (write-count 0))
    (e-harness-create-session harness :id "session-1")
    (cl-letf (((symbol-function 'e-session--write-index)
               (lambda (_store)
                 (setq write-count (1+ write-count)))))
      (e-harness--emit-turn-event harness "session-1" "turn-1"
                                  'provider-request-started
                                  '(:status started))
      (e-harness--emit-turn-event harness "session-1" "turn-1"
                                  'reasoning-delta
                                  '(:type reasoning-delta
                                    :content "thinking"))
      (e-harness--emit-turn-event harness "session-1" "turn-1"
                                  'tool-started
                                  '(:name "read" :arguments nil))
      (e-harness--emit-turn-event harness "session-1" "turn-1"
                                  'tool-finished
                                  '(:tool-call (:name "read")
                                    :result "ok"))
      (e-harness--emit-turn-event harness "session-1" "turn-1"
                                  'turn-finished
                                  '(:reason done)))
    (should (= write-count 1))
    (should (equal '(provider-request-started
                     reasoning-delta
                     tool-started
                     tool-finished
                     turn-finished)
                   (mapcar (lambda (event)
                             (plist-get event :event-type))
                           (e-session-activity-events store "session-1"))))))

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
        (let ((record (e-harness-compact-session
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
     (e-harness-compact-session harness "session-1" :keep-recent-tokens 1)
     :type 'e-harness-active-turn-exists)
    (let ((record (e-harness-compact-session
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
      (e-harness-compact-session harness "session-1" :keep-recent-tokens 1)
      (e-session-append-message store "session-1" '(:role user :content "latest"))
      (e-session-append-message store "session-1" '(:role assistant :content "latest answer"))
      (e-harness-compact-session harness "session-1" :keep-recent-tokens 1)
      (let ((second-prompt (plist-get (cadr (car calls)) :content)))
        (should (string-match-p "Previous summary:\nFirst summary\\." second-prompt))
        (should (string-match-p "middle answer" second-prompt))
        (should-not (string-match-p "old answer" second-prompt)))
      (should (equal (plist-get (e-session-latest-valid-compaction
                                 store "session-1")
                                :summary)
                     "Second summary.")))))

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
     (e-harness-compact-session harness "session-1" :keep-recent-tokens 1)
     :type 'user-error)
    (should-not (e-session-compactions store "session-1"))))

(provide 'e-harness-test)

;;; e-harness-test.el ends here
