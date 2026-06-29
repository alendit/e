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
(require 'e-emacs-tools)
(require 'e-harness)
(require 'e-layers)
(require 'e-operations)
(require 'e-prompts)
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

(defmacro e-harness-test--with-empty-layer-registry (&rest body)
  "Run BODY with an isolated layer registry."
  (declare (indent 0) (debug t))
  `(let ((e-layer--registry (make-hash-table :test 'eq)))
     ,@body))

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

(ert-deftest e-harness-test-turn-finished-hook-receives-assistant-message ()
  "Turn-finished hooks can inspect the final assistant message in the same turn."
  (let* ((assistant-text "Summarize change\n\nUpdated the topic file.")
         (seen nil)
         (capability
          (e-capability-create
           :id 'test-turn-finished-hook
           :name "Test turn finished hook"
           :hooks
           (list
            (e-hook-create
             :id "50-capture-turn-finished"
             :point :turn-finished
             :handler
             (lambda (value context)
               (setq seen (list :value value
                                :session-id (plist-get context :session-id)
                                :turn-id (plist-get context :turn-id)
                                :assistant-message
                                (plist-get context :assistant-message)))
               value)))))
         (backend (e-backend-fake-create
                   :items `((:type assistant-message :content ,assistant-text)
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (should (equal (plist-get seen :value)
                   `(:status done :reason stop :assistant-content
                     ,assistant-text)))
    (should (equal (plist-get seen :session-id) "session-1"))
    (should (stringp (plist-get seen :turn-id)))
    (should (equal (plist-get (plist-get seen :assistant-message) :role)
                   'assistant))
    (should (equal (plist-get (plist-get seen :assistant-message) :content)
                   assistant-text))))

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

(ert-deftest e-harness-test-retryable-error-detection ()
  "Transient errors are classified as retryable; genuine faults are not."
  ;; Rate limiting.
  (should (e-harness--retryable-error-p
           "429: Rate limit exceeded for api_key: abc. Limit type: requests."
           nil))
  (should (e-harness--retryable-error-p "Rate limit exceeded" nil))
  (should (e-harness--retryable-error-p "Too Many Requests" nil))
  (should (e-harness--retryable-error-p "quota hit" '(:status 429)))
  ;; Provider overload.
  (should (e-harness--retryable-error-p "529: overloaded_error" nil))
  (should (e-harness--retryable-error-p "Overloaded" nil))
  (should (e-harness--retryable-error-p "upstream failure" '(:status 529)))
  (should (e-harness--retryable-error-p "service unavailable" '(:status 503)))
  (should (e-harness--retryable-error-p "internal" '(:status 500)))
  (should (e-harness--retryable-error-p "request timeout" '(:status 408)))
  ;; Anthropic error-type is surfaced in the content, so the kind is matched
  ;; even when the human-readable message does not name it.
  (should (e-harness--retryable-error-p "overloaded_error: Overloaded" nil))
  (should (e-harness--retryable-error-p "rate_limit_error: slow down" nil))
  ;; Transport resets before/while the stream starts.
  (should (e-harness--retryable-error-p
           (concat "Provider returned non-stream text instead of a Messages "
                   "stream: upstream connect error or disconnect/reset before "
                   "headers. reset reason: connection termination")
           '(:response-kind text)))
  (should (e-harness--retryable-error-p "connection reset by peer" nil))
  (should (e-harness--retryable-error-p "broken pipe" nil))
  ;; A provider timeout is a genuine failure (the request ran its full budget),
  ;; not a transport reset, so it is not retried here.
  (should-not (e-harness--retryable-error-p "provider timed out" nil))
  ;; Genuine faults are not retried.  A bare "500" in free text (no structured
  ;; status) is not enough to retry; only a parsed :status of 5xx is.
  (should-not (e-harness--retryable-error-p "500: internal error" nil))
  (should-not (e-harness--retryable-error-p "provider failed" nil))
  (should-not (e-harness--retryable-error-p "bad request" '(:status 400)))
  (should-not (e-harness--retryable-error-p "not found" '(:status 404)))
  ;; A bare 429 inside an unrelated number run must not false-positive.
  (should-not (e-harness--retryable-error-p "served 14290 tokens" nil)))

(ert-deftest e-harness-test-backoff-schedule-grows-and-caps ()
  "Backoff grows by the multiplier and is capped, with jitter when enabled."
  (let ((e-harness-retry-initial-backoff-seconds 2.0)
        (e-harness-retry-backoff-multiplier 2.0)
        (e-harness-retry-max-backoff-seconds 20.0)
        (e-harness-retry-jitter-fraction 0))
    ;; With jitter disabled the schedule is deterministic.
    (should (= (e-harness--retry-backoff-seconds 1) 2.0))
    (should (= (e-harness--retry-backoff-seconds 2) 4.0))
    (should (= (e-harness--retry-backoff-seconds 3) 8.0))
    (should (= (e-harness--retry-backoff-seconds 10) 20.0)))
  (let ((e-harness-retry-initial-backoff-seconds 2.0)
        (e-harness-retry-backoff-multiplier 2.0)
        (e-harness-retry-max-backoff-seconds 20.0)
        (e-harness-retry-jitter-fraction 0.25))
    ;; Jitter only ever adds delay, never reduces below the base or past
    ;; the cap plus the jitter fraction.
    (let ((d (e-harness--retry-backoff-seconds 1)))
      (should (>= d 2.0))
      (should (<= d (* 2.0 1.25))))
    (let ((d (e-harness--retry-backoff-seconds 10)))
      (should (>= d 20.0))
      (should (<= d (* 20.0 1.25))))))

(defun e-harness-test--rate-limited-backend (failures)
  "Return a backend that fails with HTTP 429 FAILURES times, then succeeds.
Counts attempts in the returned (BACKEND . COUNTER) cons's cdr."
  (let ((counter (list 0)))
    (cons
     (e-backend-create
      :name "rate-limited"
      :stream
      (cl-function
       (lambda (&key messages options on-item)
         (ignore messages options)
         (cl-incf (car counter))
         (if (<= (car counter) failures)
             (funcall on-item
                      '(:type backend-error
                        :content "429: Rate limit exceeded for api_key: abc"
                        :payload (:status 429)))
           (funcall on-item '(:type assistant-message :content "recovered"))
           (funcall on-item '(:type done :reason stop))))))
     counter)))

(ert-deftest e-harness-test-rate-limited-turn-retries-then-succeeds ()
  "A 429 backend turn retries with backoff and eventually settles done."
  (let* ((e-harness-retry-initial-backoff-seconds 0.02)
         (e-harness-retry-backoff-multiplier 1.0)
         (e-harness-retry-max-backoff-seconds 0.02)
         (e-harness-retry-max-elapsed-seconds 5.0)
         (pair (e-harness-test--rate-limited-backend 2))
         (backend (car pair))
         (counter (cdr pair))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait harness "session-1" 5.0)))
      (should (equal (plist-get settled :status) 'done)))
    ;; Two failures + one success.
    (should (= (car counter) 3))
    (let ((types (mapcar (lambda (e) (plist-get e :type)) events)))
      (should (= 2 (seq-count (lambda (ty) (eq ty 'turn-retrying)) types)))
      (should-not (memq 'turn-failed types)))))

(ert-deftest e-harness-test-rate-limited-turn-fails-after-budget ()
  "Retries stop once the elapsed budget is exhausted, settling turn-failed."
  (let* ((e-harness-retry-initial-backoff-seconds 0.02)
         (e-harness-retry-backoff-multiplier 1.0)
         (e-harness-retry-max-backoff-seconds 0.02)
         ;; Budget large enough for a couple retries, then give up.
         (e-harness-retry-max-elapsed-seconds 0.08)
         (pair (e-harness-test--rate-limited-backend 1000))
         (backend (car pair))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait harness "session-1" 5.0)))
      (should (equal (plist-get settled :status) 'error))
      (should (string-match-p "429" (plist-get settled :error))))
    (should (member 'turn-failed
                    (mapcar (lambda (e) (plist-get e :type)) events)))))

(ert-deftest e-harness-test-non-retryable-error-fails-immediately ()
  "A genuine client-fault backend error settles without any retry."
  (let* ((e-harness-retry-max-elapsed-seconds 5.0)
         (backend (e-backend-fake-create
                   :items '((:type backend-error
                              :content "400: invalid request"
                              :payload (:status 400)))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait harness "session-1" 1.0)))
      (should (equal (plist-get settled :status) 'error)))
    (let ((types (mapcar (lambda (e) (plist-get e :type)) events)))
      (should (member 'turn-failed types))
      (should-not (memq 'turn-retrying types)))))

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

(ert-deftest e-harness-test-queue-prompt-requires-active-turn ()
  "Queueing is only valid while a session has a running active turn."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-harness-queue-prompt harness "session-1" "follow up")
     :type 'e-harness-no-active-turn)))

(ert-deftest e-harness-test-queue-prompt-stores-during-active-turn ()
  "Queueing during a running turn stores prompt data without replacing it."
  (let* ((backend (e-backend-create
                   :name "held"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore messages options on-item on-done
                                     on-error on-request-start)
                             nil))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id (e-harness-prompt-async harness "session-1" "first")))
      (let ((queue-id
             (e-harness-queue-prompt
              harness "session-1" "second"
              :references '((:uri "buffer://source"))
              :metadata '(:source chat-composer))))
        (should (equal (plist-get (e-harness-state harness "session-1")
                                  :active-turn)
                       turn-id))
        (should (= (length (e-harness-queued-prompts
                            harness "session-1"))
                   1))
        (let ((item (car (e-harness-queued-prompts harness "session-1"))))
          (should (equal (plist-get item :id) queue-id))
          (should (equal (plist-get item :prompt) "second"))
          (should (equal (plist-get item :references)
                         '((:uri "buffer://source"))))
          (should (equal (plist-get item :metadata)
                         '(:source chat-composer)))
          (should (stringp (plist-get item :created-at))))
        (should (member 'queue-changed
                        (mapcar (lambda (event) (plist-get event :type))
                                events)))))))

(ert-deftest e-harness-test-queued-prompts-drain-in-order ()
  "Queued prompts start automatically in FIFO order after active turns settle."
  (let* ((finishers nil)
         (starts nil)
         (backend (e-backend-create
                   :name "held"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore options on-error on-request-start)
                             (let* ((message (car (last messages)))
                                    (prompt (plist-get message :content)))
                               (push (list :prompt prompt
                                           :metadata
                                           (plist-get message :metadata))
                                     starts)
                               (push
                                (lambda ()
                                  (funcall on-item
                                           (list :type 'assistant-message
                                                 :content
                                                 (concat "answer " prompt)))
                                  (funcall on-item
                                           '(:type done :reason stop))
                                  (funcall on-done '(:status done)))
                                finishers))
                             nil))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "first")
    (e-harness-queue-prompt
     harness "session-1" "second"
     :references '((:uri "buffer://source"))
     :metadata '(:source chat-composer))
    (e-harness-queue-prompt harness "session-1" "third")
    (should (equal (mapcar (lambda (item) (plist-get item :prompt))
                           (e-harness-queued-prompts harness "session-1"))
                   '("second" "third")))
    (funcall (pop finishers))
    (while (< (length finishers) 1)
      (accept-process-output nil 0.01))
    (should (equal (mapcar (lambda (item) (plist-get item :prompt))
                           (e-harness-queued-prompts harness "session-1"))
                   '("third")))
    (funcall (pop finishers))
    (while (< (length finishers) 1)
      (accept-process-output nil 0.01))
    (should-not (e-harness-queued-prompts harness "session-1"))
    (funcall (pop finishers))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           (e-harness-messages harness "session-1"))
                   '("first" "answer first"
                     "second" "answer second"
                     "third" "answer third")))
    (let ((second-start (cadr (nreverse starts))))
      (should (equal (plist-get second-start :prompt) "second"))
      (should (equal (plist-get (plist-get second-start :metadata)
                                :references)
                     '((:uri "buffer://source"))))
      (should (equal (plist-get (plist-get second-start :metadata)
                                :source)
                     'chat-composer)))))

(ert-deftest e-harness-test-reset-clears-queued-prompts ()
  "Reset removes queued follow-ups so settled old turns cannot drain them."
  (let* ((finishers nil)
         (events nil)
         (backend (e-backend-create
                   :name "held"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore messages options on-error on-request-start)
                             (push
                              (lambda ()
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "answer"))
                                (funcall on-item
                                         '(:type done :reason stop))
                                (funcall on-done '(:status done)))
                              finishers)
                             nil))))
         (harness (e-harness-create :backend backend)))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "first")
    (e-harness-queue-prompt harness "session-1" "second")
    (should (e-harness-queued-prompts harness "session-1"))
    (e-harness-reset harness "session-1")
    (should-not (e-harness-queued-prompts harness "session-1"))
    (should (member 'queue-changed
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))
    (funcall (pop finishers))
    (accept-process-output nil 0.05)
    (should-not finishers)
    (should-not (e-harness-queued-prompts harness "session-1"))))

(ert-deftest e-harness-test-steer-active-turn-requires-active-turn ()
  "Steering is only valid while a session has a running active turn."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-harness-steer-active-turn harness "session-1" "focus here")
     :type 'e-harness-no-active-turn)))

(ert-deftest e-harness-test-steer-active-turn-stores-pending-input ()
  "Successful steering stores pending input on the active turn."
  (let* ((backend (e-backend-create
                   :name "steerable"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore messages options on-item on-done
                                     on-error)
                             (funcall on-request-start
                                      (e-backend-request-create))
                             nil))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id (e-harness-prompt-async harness "session-1" "first")))
      (should (equal (e-harness-steer-active-turn
                      harness "session-1" "focus here"
                      :metadata '(:source chat-composer))
                     turn-id))
      (let ((entry (gethash "session-1" (e-harness-active-turns harness))))
        (should (equal (e-harness--pending-steering-items entry)
                       '((:prompt "focus here"
                          :metadata (:source chat-composer))))))
      (let ((steered (cl-find 'turn-steered events
                              :key (lambda (event)
                                     (plist-get event :type)))))
        (should steered)
        (should (equal (plist-get steered :turn-id) turn-id))
        (should (equal (plist-get (plist-get steered :payload)
                                  :prompt-preview)
                       "focus here"))
        (should (equal (plist-get (plist-get steered :payload)
                                  :metadata)
                       '(:source chat-composer)))))))

(ert-deftest e-harness-test-steer-active-turn-drains-in-same-turn ()
  "Pending steering input is sampled as a user message in the same turn."
  (let* ((calls nil)
         (finishers nil)
         (backend (e-backend-create
                   :name "same-turn-steering"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore options on-error)
                             (funcall on-request-start
                                      (e-backend-request-create))
                             (push (copy-tree messages) calls)
                             (let ((call-number (length calls)))
                               (push
                                (lambda ()
                                  (funcall on-item
                                           (list :type 'assistant-message
                                                 :content
                                                 (format "answer %d"
                                                         call-number)))
                                  (funcall on-item
                                           '(:type done :reason stop))
                                  (funcall on-done '(:status done)))
                                finishers))
                             nil))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id (e-harness-prompt-async harness "session-1" "first")))
      (e-harness-steer-active-turn
       harness "session-1" "focus here"
       :metadata '(:source chat-composer))
      (funcall (pop finishers))
      (while (< (length calls) 2)
        (accept-process-output nil 0.01))
      (let ((follow-up (car calls)))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :role))
                               follow-up)
                       '(user assistant user)))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               follow-up)
                       '("first" "answer 1" "focus here")))
        (should (equal (plist-get (car (last follow-up)) :metadata)
                       '(:source chat-composer))))
      (should (equal (plist-get (e-harness-state harness "session-1")
                                :active-turn)
                     turn-id))
      (funcall (pop finishers))
      (let ((entry (e-harness-wait harness "session-1" 0.1)))
        (should (eq (plist-get entry :status) 'done)))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :content))
                             (e-harness-messages harness "session-1"))
                     '("first" "answer 1" "focus here" "answer 2")))
      (should (equal (plist-get (nth 2 (e-harness-messages harness "session-1"))
                                :turn-id)
                     turn-id)))))


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

(ert-deftest e-harness-test-provider-anchor-candidates-are-persisted ()
  "Provider anchor candidates persist with covered entry and context metadata."
  (e-harness-test--with-empty-layer-registry
    (let* ((dynamic-provider
            (e-context-provider-create
             :name 'visible-buffer
             :build (lambda (&rest _)
                      '((:role system :content "current state")))
             :cache-placement 'dynamic-context))
           (capability
            (e-capability-create
             :id 'context-anchor-capability
             :instructions "stable instructions"
             :context-providers (list dynamic-provider)))
           (backend (e-backend-fake-create
                     :items '((:type assistant-message :content "answer")
                              (:type provider-anchor-candidate
                               :provider-id openai
                               :metadata (:response-id "resp-1"))
                              (:type done :reason stop))))
           (harness (e-harness-create
                     :backend backend
                     :enabled-layer-ids '(context-anchor-layer)
                     :default-options '(:model "gpt-test"
                                        :provider-continuation t
                                        :provider-anchor-provider-id openai))))
      (e-layer-register
       (e-layer-spec-create
        :id 'context-anchor-layer
        :name "Context Anchor Layer"
        :factory (lambda ()
                   (e-layer-create
                    :id 'context-anchor-layer
                    :name "Context Anchor Layer"
                    :capabilities (list capability)))))
      (e-harness-create-session harness :id "session-1")
      (e-harness-prompt harness "session-1" "question")
      (let* ((messages (e-harness-messages harness "session-1"))
             (assistant (cl-find 'assistant messages
                                 :key (lambda (message)
                                        (plist-get message :role))))
             (anchors (e-session-provider-anchors
                       (e-harness-sessions harness)
                       "session-1"))
             (anchor (car anchors))
             (fingerprints (plist-get anchor :fingerprints)))
        (should (= (length anchors) 1))
        (should (eq (plist-get anchor :provider-id) 'openai))
        (should (equal (plist-get anchor :model) "gpt-test"))
        (should (equal (plist-get anchor :covered-entry-id)
                       (plist-get assistant :id)))
        (should (equal (plist-get anchor :metadata)
                       '(:response-id "resp-1")))
        (should (equal (mapcar (lambda (fingerprint)
                                 (plist-get fingerprint :kind))
                               (plist-get fingerprints :segments))
                       '("static-prefix" "current-state")))
        (should (equal (plist-get fingerprints :active-layer-ids)
                       '("context-anchor-layer")))
        (should (equal (plist-get fingerprints :reasoning)
                       '(:reasoning nil :reasoning-effort nil :effort nil)))
        (dolist (fingerprint (plist-get fingerprints :segments))
          (should (stringp (plist-get fingerprint :id)))
          (should (stringp (plist-get fingerprint :fingerprint))))))))

(ert-deftest e-harness-test-provider-anchor-persistence-keeps-latest-candidate ()
  "Provider anchor persistence keeps only the final candidate for a provider."
  (e-harness-test--with-empty-layer-registry
    (let* ((backend (e-backend-fake-create
                     :items '((:type assistant-message :content "answer")
                              (:type provider-anchor-candidate
                               :provider-id openai
                               :metadata (:response-id "resp-old"))
                              (:type provider-anchor-candidate
                               :provider-id openai
                               :metadata (:response-id "resp-new"))
                              (:type done :reason stop))))
           (harness (e-harness-create
                     :backend backend
                     :default-options '(:model "gpt-test"
                                        :provider-continuation t
                                        :provider-anchor-provider-id openai))))
      (e-harness-create-session harness :id "session-1")
      (e-harness-prompt harness "session-1" "question")
      (let ((anchors (e-session-provider-anchors
                      (e-harness-sessions harness)
                      "session-1")))
        (should (= (length anchors) 1))
        (should (equal (plist-get (plist-get (car anchors) :metadata)
                                  :response-id)
                       "resp-new"))))))

(ert-deftest e-harness-test-openai-anchor-candidates-require-continuation ()
  "OpenAI response ids are not persisted when the request was not store-enabled."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type provider-anchor-candidate
                             :provider-id openai
                             :metadata (:response-id "resp-unstored"))
                            (:type done :reason stop))))
         (harness (e-harness-create
                   :backend backend
                   :default-options '(:model "gpt-test"))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (should-not
     (e-session-provider-anchors (e-harness-sessions harness)
                                 "session-1"))))

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

(ert-deftest e-harness-test-state-uses-cached-message-count ()
  "Harness state does not copy the transcript to count messages."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness)))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "one"))
    (cl-letf (((symbol-function 'e-harness-messages)
               (lambda (&rest _args)
                 (error "messages should not be copied"))))
      (should (equal (e-harness-state harness "session-1")
                     '(:session-id "session-1"
                       :active-turn nil
                       :message-count 1))))))

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

(ert-deftest e-harness-test-context-preview-includes-segments ()
  "Context preview exposes segment metadata without changing flat messages."
  (let* ((stable-provider (e-context-provider-create
                           :name 'stable-provider
                           :cache-placement 'stable-context
                           :build (cl-function
                                   (lambda (&key harness session-id turn-id)
                                     (ignore harness session-id turn-id)
                                     '((:role system
                                        :content "stable context"))))))
         (dynamic-provider (e-context-provider-create
                            :name 'dynamic-provider
                            :cache-placement 'dynamic-context
                            :build (cl-function
                                    (lambda (&key harness session-id turn-id)
                                      (ignore harness session-id turn-id)
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

(ert-deftest e-harness-test-context-attaches-compatible-provider-anchor ()
  "Context options include a compatible provider anchor and transcript delta."
  (let* ((current-state "dynamic context")
         (dynamic-provider
          (e-context-provider-create
           :name 'dynamic-provider
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
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
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness)
             "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1"))
           (fingerprints
            (e-harness--provider-anchor-fingerprints anchor-context)))
      (e-session-append-provider-anchor
       (e-harness-sessions harness)
       "session-1"
       'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints fingerprints
       :metadata '(:response-id "resp-1"))
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:role user :content "new prompt"))
      (let* ((context (e-harness-context harness "session-1" "turn-2"))
             (options (plist-get context :options))
             (anchor (plist-get options :provider-anchor))
             (delta (plist-get options :provider-anchor-delta-messages)))
        (should (equal (plist-get (plist-get anchor :metadata) :response-id)
                       "resp-1"))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               delta)
                       '("dynamic context" "new prompt")))
        (should (equal (plist-get options
                                  :provider-anchor-source-message-count)
                       (length (plist-get context :messages))))))))

(ert-deftest e-harness-test-context-uses-provider-anchor-after-dynamic-state-change ()
  "Changed dynamic-context fingerprints keep provider continuation anchors usable."
  (let* ((current-state "dynamic context")
         (dynamic-provider
          (e-context-provider-create
           :name 'dynamic-provider
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
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
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness)
             "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness)
       "session-1"
       'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setq current-state "changed dynamic context")
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should (equal (plist-get
                        (plist-get
                         (plist-get options :provider-anchor)
                         :metadata)
                        :response-id)
                       "resp-1"))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               (plist-get options
                                          :provider-anchor-delta-messages))
                       '("changed dynamic context" "new prompt")))))))

(ert-deftest e-harness-test-context-reports-provider-anchor-invalidation-reason ()
  "Context options report why an otherwise current provider anchor was skipped."
  (let* ((stable-state "stable context")
         (stable-provider
          (e-context-provider-create
           :name 'stable-provider
           :cache-placement 'stable-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content stable-state)))))
         (capability
          (e-capability-create
           :id 'anchor-capability
           :instructions "capability instructions"
           :context-providers (list stable-provider)))
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
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness)
             "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness)
       "session-1"
       'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setq stable-state "changed stable context")
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'segment-fingerprint-mismatch))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-reasoning-change ()
  "Provider continuation anchors include reasoning options in compatibility."
  (let* ((harness
          (e-harness-create
           :backend (e-backend-fake-create :items nil)
           :default-options '(:model "gpt-test"
                              :reasoning-effort "high"
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setf (e-harness-default-options harness)
            '(:model "gpt-test"
              :reasoning-effort "low"
              :provider-continuation t
              :provider-anchor-provider-id openai))
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should-not (plist-get options :provider-anchor))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'reasoning-changed))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-tool-schema-change ()
  "Provider continuation anchors include active tool schemas in compatibility."
  (let* ((tool-parameters '(:type "object"
                           :properties (:path (:type "string"))))
         (tool-provider
          (lambda (registry)
            (e-tools-register
             registry
             :name "read"
             :description "Read a file."
             :parameters tool-parameters
             :handler (lambda (&rest _) "ok"))))
         (capability
          (e-capability-create :id 'tool-capability
                               :tools (list tool-provider)))
         (layer (e-layer-create
                 :id 'tool-layer :name "Tool Layer"
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
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setq tool-parameters '(:type "object"
                              :properties (:uri (:type "string"))))
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should-not (plist-get options :provider-anchor))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'tools-changed))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-effective-layer-id-change ()
  "Provider continuation anchors include effective layer ids in compatibility."
  (e-harness-test--with-empty-layer-registry
    (let* ((harness
            (e-harness-create
             :backend (e-backend-fake-create :items nil)
             :enabled-layer-ids '(base-layer)
             :default-options '(:model "gpt-test"
                                :provider-continuation t
                                :provider-anchor-provider-id openai))))
      (dolist (id '(base-layer extra-layer))
        (e-layer-register
         (let ((layer-id id))
           (e-layer-spec-create
            :id layer-id
            :name (symbol-name layer-id)
            :factory (lambda ()
                       (e-layer-create
                        :id layer-id
                        :name (symbol-name layer-id)))))))
      (e-harness-create-session harness :id "session-1")
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "old prompt"))
      (let* ((assistant
              (e-session-append-message
               (e-harness-sessions harness) "session-1"
               '(:role assistant :content "old answer")))
             (anchor-context (e-harness-context harness "session-1" "turn-1")))
        (e-session-append-provider-anchor
         (e-harness-sessions harness) "session-1" 'openai
         :model "gpt-test"
         :covered-entry-id (plist-get assistant :id)
         :fingerprints (e-harness--provider-anchor-fingerprints anchor-context)
         :metadata '(:response-id "resp-1"))
        (e-harness-enable-layer-id harness 'extra-layer)
        (e-session-append-message
         (e-harness-sessions harness) "session-1"
         '(:role user :content "new prompt"))
        (let ((options (plist-get
                        (e-harness-context harness "session-1" "turn-2")
                        :options)))
          (should-not (plist-get options :provider-anchor))
          (should (equal (plist-get options :provider-anchor-invalidation-reason)
                         'active-layers-changed)))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-provider-option-change ()
  "Provider continuation anchors include provider request-shaping options."
  (let* ((harness
          (e-harness-create
           :backend (e-backend-fake-create :items nil)
           :default-options '(:model "claude-test"
                              :provider-continuation t
                              :provider-anchor-provider-id anthropic
                              :prompt-cache t
                              :anthropic-container-id "container-1"))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'anthropic
       :model "claude-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness--provider-anchor-fingerprints anchor-context)
       :metadata '(:provider anthropic
                   :model "claude-test"
                   :anthropic-cache-mode explicit
                   :anthropic-container-id "container-1"
                   :full-history t))
      (setf (e-harness-default-options harness)
            '(:model "claude-test"
              :provider-continuation t
              :provider-anchor-provider-id anthropic
              :prompt-cache t
              :anthropic-container-id "container-2"))
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should-not (plist-get options :provider-anchor))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'provider-options-changed))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-instructions-change ()
  "Provider continuation anchors include explicit request instructions."
  (let* ((harness
          (e-harness-create
           :backend (e-backend-fake-create :items nil)
           :default-options '(:model "gpt-test"
                              :instructions "Be terse."
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setf (e-harness-default-options harness)
            '(:model "gpt-test"
              :instructions "Be detailed."
              :provider-continuation t
              :provider-anchor-provider-id openai))
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should-not (plist-get options :provider-anchor))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'provider-options-changed))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-max-tokens-change ()
  "Provider continuation anchors include Anthropic max token request shaping."
  (let* ((harness
          (e-harness-create
           :backend (e-backend-fake-create :items nil)
           :default-options '(:model "claude-test"
                              :max-tokens 1024
                              :provider-continuation t
                              :provider-anchor-provider-id anthropic))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'anthropic
       :model "claude-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness--provider-anchor-fingerprints anchor-context)
       :metadata '(:provider anthropic
                   :model "claude-test"
                   :full-history t))
      (setf (e-harness-default-options harness)
            '(:model "claude-test"
              :max-tokens 2048
              :provider-continuation t
              :provider-anchor-provider-id anthropic))
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should-not (plist-get options :provider-anchor))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'provider-options-changed))))))

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
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (should (equal (mapcar #'e-capability-id
                           (e-harness-active-capabilities harness))
                   '(first-capability second-capability)))))

(ert-deftest e-harness-test-tools-are-derived-from-effective-capabilities ()
  "The harness tool surface is rebuilt from effective capabilities on demand."
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
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
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

(ert-deftest e-harness-test-prompts-are-derived-from-effective-capabilities ()
  "Prompts are aggregated from effective capability prompts in order."
  (let* ((first (e-prompt-spec-create
                 :name "explain"
                 :description "Explain."
                 :template "Explain this."))
         (second (e-prompt-spec-create
                  :name "review"
                  :description "Review."
                  :template "Review this."))
         (duplicate (e-prompt-spec-create
                     :name "explain"
                     :description "Explain differently."
                     :template "Explain this differently."))
         (harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-set-intrinsic-capabilities
     harness
     (list (e-capability-with-prompts-create
            :id 'prompt-one
            :name "Prompt One"
            :prompts (list first second))
           (e-capability-with-prompts-create
            :id 'prompt-two
            :name "Prompt Two"
            :prompts (list duplicate))))
    (should (equal (e-harness-prompts harness)
                   (list first second duplicate)))
    (should (eq (e-harness-prompt-by-name harness "explain") first))
    (should (equal (mapcar (lambda (collision)
                             (list (plist-get collision :name)
                                   (length (plist-get collision :prompts))))
                           (e-harness-prompt-name-collisions harness))
                   '(("explain" 2))))))

(ert-deftest e-harness-test-hooks-are-derived-from-effective-capabilities ()
  "Harness hook registries are derived from effective capabilities."
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
                   :intrinsic-capabilities (list capability))))
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
           :intrinsic-capabilities (list tools-capability hooks-capability)))
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

(ert-deftest e-harness-test-tool-lifecycle-reuses-hook-context ()
  "A tool lifecycle builds its hook context once for prepare/start/post."
  (should (require 'e-hooks nil t))
  (let* ((tools-capability
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
         (hooks-capability
          (e-capability-create
           :id 'tool-hooks
           :hooks
           (list
            (e-hook-create
             :id "10-prepare"
             :point :pre-tool-call
             :handler (lambda (tool-call _context)
                        (plist-put (copy-sequence tool-call)
                                   :arguments '(:text "prepared"))))
            (e-hook-create
             :id "50-result"
             :point :post-tool-call
             :handler (lambda (result _context)
                        (plist-put (copy-sequence result)
                                   :content
                                   (concat (plist-get result :content)
                                           "-post")))))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list tools-capability hooks-capability)))
         (context-calls 0)
         result
         failure)
    (e-harness-create-session harness :id "session-1")
    (let ((original (symbol-function 'e-harness--tool-hook-context)))
      (cl-letf (((symbol-function 'e-harness--tool-hook-context)
                 (lambda (&rest args)
                   (setq context-calls (1+ context-calls))
                   (apply original args))))
        (let* ((lifecycle
                (e-harness-tool-lifecycle harness "session-1" "turn-1"))
               (prepared
                (e-tool-lifecycle-prepare-call
                 lifecycle
                 '(:id "call-1" :name "echo" :arguments (:text "raw")))))
          (e-tool-lifecycle-start-call
           lifecycle
           prepared
           :on-done (lambda (value) (setq result value))
           :on-error (lambda (err) (setq failure err))))))
    (let ((deadline (+ (float-time) 1.0)))
      (while (and (not (or result failure))
                  (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should (or result failure))
    (when failure
      (signal (car failure) (cdr failure)))
    (should (equal (plist-get result :content) "prepared-post"))
    (should (= context-calls 1))))

(ert-deftest e-harness-test-nested-tool-calls-use-harness-lifecycle ()
  "Nested tool calls run hooks, emit activity, and do not append messages."
  (should (require 'e-hooks nil t))
  (let* ((events nil)
         (harness nil)
         (tools-capability
          (e-capability-create
           :id 'nested-tools
           :tools
           (list
            (lambda (registry)
              (e-tools-register
               registry
               :name "outer"
               :description "Call inner."
               :handler (lambda (_arguments)
                          (e-tools-call
                           "inner" '(:text "raw")
                           '(:metadata (:purpose "chain-test")))))
              (e-tools-register
               registry
               :name "inner"
               :description "Return text."
               :handler (lambda (arguments)
                          (plist-get arguments :text)))))))
         (hooks-capability
          (e-capability-create
           :id 'nested-hooks
           :hooks
           (list
            (e-hook-create
             :id "10-nested-prepare"
             :point :pre-tool-call
             :handler
             (lambda (tool-call context)
               (should (eq (plist-get context :harness) harness))
               (if (equal (plist-get tool-call :name) "inner")
                   (plist-put (copy-sequence tool-call)
                              :arguments '(:text "prepared"))
                 tool-call)))
            (e-hook-create
             :id "50-nested-result"
             :point :post-tool-call
             :handler
             (lambda (result context)
               (should (equal (plist-get context :turn-id) "turn-1"))
               (if (equal (plist-get result :name) "inner")
                   (plist-put (copy-sequence result)
                              :content
                              (concat (plist-get result :content) "-post"))
                 result))))))
         result
         failure)
    (setq harness
          (e-harness-create
           :backend (e-backend-fake-create :items nil)
           :intrinsic-capabilities (list tools-capability hooks-capability)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-tool-lifecycle-start-call
     (e-harness-tool-lifecycle harness "session-1" "turn-1")
     '(:id "outer-1" :name "outer" :arguments nil)
     :on-done (lambda (value) (setq result value))
     :on-error (lambda (err) (setq failure err)))
    (let ((deadline (+ (float-time) 1.0)))
      (while (and (not (or result failure))
                  (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should (or result failure))
    (when failure
      (signal (car failure) (cdr failure)))
    (should
     (equal result
            '(:tool-call-id "outer-1"
              :name "outer"
              :status ok
              :content (:tool-call-id "outer-1/nested-1"
                        :name "inner"
                        :status ok
                        :content "prepared-post"
                        :metadata nil)
              :metadata nil)))
    (let* ((ordered-events (nreverse events))
           (started (cl-find 'tool-started ordered-events
                             :key (lambda (event)
                                    (plist-get event :type))))
           (finished (cl-find 'tool-finished ordered-events
                              :key (lambda (event)
                                     (plist-get event :type))))
           (started-payload (plist-get started :payload))
           (finished-payload (plist-get finished :payload)))
      (should (equal (mapcar (lambda (event) (plist-get event :type))
                             ordered-events)
                     '(tool-started tool-finished)))
      (should (equal (plist-get started-payload :nested) t))
      (should (equal (plist-get started-payload :parent-tool-call-id)
                     "outer-1"))
      (should (equal (plist-get started-payload :depth) 1))
      (should (equal (plist-get (plist-get started-payload :tool-call)
                                :arguments)
                     '(:text "prepared")))
      (should (equal (plist-get started-payload :purpose)
                     "chain-test"))
      (should (equal (plist-get finished-payload :nested) t))
      (should (equal (plist-get finished-payload :parent-tool-call-id)
                     "outer-1"))
      (should (equal (plist-get (plist-get finished-payload :result)
                                :content)
                     "prepared-post"))
      (should (equal (plist-get finished-payload :purpose)
                     "chain-test")))
    (let* ((activity (e-harness-session-activity-events harness "session-1"))
           (nested-finished
            (cl-find 'tool-finished activity
                     :key (lambda (event)
                            (plist-get event :event-type)))))
      (should nested-finished)
      (should (equal (plist-get (plist-get nested-finished :payload)
                                :nested)
                     t)))
    (should (equal (e-harness-messages harness "session-1") nil))))

(ert-deftest e-harness-test-run-elisp-chains-active-tools-end-to-end ()
  "run_elisp can compose active tools without nested transcript messages."
  (let* ((code
          "(let* ((first (e-tools-call! \"tag_text\" '(:text \"alpha\")))
        (second (e-tools-call! \"tag_text\" (list :text first))))
   (list :first first :second second))")
         (calls 0)
         (second-request-messages nil)
         (events nil)
         (backend
          (e-backend-create
           :name "fake-run-elisp-chain"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore options)
              (setq calls (1+ calls))
              (if (= calls 1)
                  (progn
                    (should (equal (mapcar (lambda (message)
                                             (plist-get message :role))
                                           messages)
                                   '(user)))
                    (funcall on-item
                             (list :type 'tool-call
                                   :id "run-1"
                                   :name "run_elisp"
                                   :arguments (list :code code)))
                    (funcall on-item '(:type done :reason tool-use)))
                (setq second-request-messages messages)
                (should (equal (mapcar (lambda (message)
                                         (plist-get message :role))
                                       messages)
                               '(user tool-call tool)))
                (funcall on-item
                         '(:type assistant-message
                           :content "done"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools-capability
          (e-capability-create
           :id 'run-elisp-chain-tools
           :tools
           (list
            (lambda (registry)
              (e-emacs-tools-register-run-elisp registry)
              (e-tools-register
               registry
               :name "tag_text"
               :description "Wrap text."
               :handler (lambda (arguments)
                          (format "[%s]" (plist-get arguments :text))))))))
         (harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities (list tools-capability))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-prompt harness "session-1" "chain tools")
    (should (equal calls 2))
    (let* ((messages (e-harness-messages harness "session-1"))
           (roles (mapcar (lambda (message)
                            (plist-get message :role))
                          messages))
           (tool-message (cl-find 'tool second-request-messages
                                  :key (lambda (message)
                                         (plist-get message :role))))
           (tool-result (plist-get tool-message :content))
           (nested-events
            (seq-filter
             (lambda (event)
               (plist-get (plist-get event :payload) :nested))
             (nreverse events)))
           (nested-activity
            (seq-filter
             (lambda (event)
               (plist-get (plist-get event :payload) :nested))
             (e-harness-session-activity-events harness "session-1"))))
      (should (equal roles '(user tool-call tool assistant)))
      (should (= (cl-count 'tool-call roles) 1))
      (should (= (cl-count 'tool roles) 1))
      (should (equal (plist-get tool-result :tool-call-id) "run-1"))
      (should (equal (plist-get tool-result :name) "run_elisp"))
      (should (eq (plist-get tool-result :status) 'ok))
      (should (string-match-p "\\[\\[alpha\\]\\]"
                              (format "%S"
                                      (plist-get tool-result :content))))
      (should (= (length nested-events) 4))
      (dolist (event nested-events)
        (should (member (plist-get event :type)
                        '(tool-started tool-finished)))
        (should (equal (plist-get (plist-get event :payload)
                                  :parent-tool-call-id)
                       "run-1")))
      (should (= (length nested-activity) 4)))))

(ert-deftest e-harness-test-run-elisp-can-catch-nested-tool-errors ()
  "Evaluated Lisp can catch nested tool failures and return data."
  (let* ((code
          "(condition-case err
     (e-tools-call! \"fail_tool\" nil)
   (e-tools-nested-tool-error
    (let ((result (cadr err)))
      (list :caught t
            :tool (plist-get result :name)
            :status (plist-get result :status)
            :content (plist-get result :content)))))")
         (calls 0)
         (second-request-messages nil)
         (backend
          (e-backend-create
           :name "fake-run-elisp-caught-nested-error"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore options)
              (setq calls (1+ calls))
              (if (= calls 1)
                  (progn
                    (funcall on-item
                             (list :type 'tool-call
                                   :id "run-error"
                                   :name "run_elisp"
                                   :arguments (list :code code)))
                    (funcall on-item '(:type done :reason tool-use)))
                (setq second-request-messages messages)
                (funcall on-item
                         '(:type assistant-message
                           :content "handled"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools-capability
          (e-capability-create
           :id 'run-elisp-error-tools
           :tools
           (list
            (lambda (registry)
              (e-emacs-tools-register-run-elisp registry)
              (e-tools-register
               registry
               :name "fail_tool"
               :description "Fail."
               :handler (lambda (_arguments)
                          (error "nested boom")))))))
         (harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities (list tools-capability))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "catch nested error")
    (should (equal calls 2))
    (let* ((tool-message (cl-find 'tool second-request-messages
                                  :key (lambda (message)
                                         (plist-get message :role))))
           (content (plist-get (plist-get tool-message :content)
                               :content)))
      (should (string-match-p ":caught t" (format "%S" content)))
      (should (string-match-p "fail_tool" (format "%S" content)))
      (should (string-match-p "nested boom" (format "%S" content))))))

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
           :intrinsic-capabilities (list tools-capability))))
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

(ert-deftest e-harness-test-resources-are-derived-from-effective-capabilities ()
  "The harness resource surface is rebuilt from effective capabilities on demand."
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
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
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
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-base-layer-create fallback-root)))))
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
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-base-layer-create fallback-root)))))
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
                                         "edit-result"))
                             (e-resource-method-create
                              :scheme "test"
                              :operation e-operation-glob
                              :description "Glob test resources."
                              :uri-patterns '("test://<root>")
                              :handler (lambda (uri pattern limit case-sensitive)
                                         (push (list :glob uri pattern limit case-sensitive)
                                               calls)
                                         '(:resources [(:uri "test://value"
                                                       :name "value")]
                                           :truncated nil)))
                             (e-resource-method-create
                              :scheme "test"
                              :operation e-operation-search
                              :description "Search test resources."
                              :uri-patterns '("test://<root>")
                              :handler (lambda (uri query options)
                                         (push (list :search uri query options) calls)
                                         '(:matches [(:uri "test://value"
                                                     :line 1
                                                     :column 1
                                                     :text "needle")]
                                           :truncated nil)))))
                     (e-resources-register registry method))))))
         (layer (e-layer-create
                 :id 'resource-tool-layer
                 :name "Resource Tool Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (e-layer-capabilities layer)))
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
    (should (equal (plist-get
                    (e-tools-execute
                     tools
                     '(:id "call-4"
                       :name "glob"
                       :arguments (:uri "test://"
                                   :pattern "*.el"
                                   :limit 5)))
                    :content)
                   '(:resources [(:uri "test://value" :name "value")]
                     :truncated nil)))
    (should (equal (plist-get
                    (e-tools-execute
                     tools
                     '(:id "call-5"
                       :name "search"
                       :arguments (:uri "test://"
                                   :query "needle"
                                   :glob "*.el"
                                   :limit 6)))
                    :content)
                   '(:matches [(:uri "test://value"
                                :line 1
                                :column 1
                                :text "needle")]
                     :truncated nil)))
    (should (equal (nreverse calls)
                   '((:read (:scheme "test" :address "value" :uri "test://value")
                            (:unit "line" :start 1 :end 2))
                     (:write (:scheme "test" :address "value" :uri "test://value")
                             "content")
                     (:edit (:scheme "test" :address "value" :uri "test://value")
                            ((:oldText "a" :newText "b")))
                     (:glob (:scheme "test" :address "" :uri "test://")
                            "*.el" 5 nil)
                     (:search (:scheme "test" :address "" :uri "test://")
                              "needle"
                              (:glob "*.el" :limit 6)))))))

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
                   :intrinsic-capabilities (e-layer-capabilities layer)))
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
                   :intrinsic-capabilities (e-layer-capabilities layer)))
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
    (should-not (e-harness-effective-layer-ids harness))
    (should (equal (mapcar #'e-capability-id
                           (e-harness-active-capabilities harness))
                   '(direct-capability)))))

(ert-deftest e-harness-test-store-is-derived-from-effective-capabilities ()
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
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
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
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
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
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
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
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (let ((result (e-tools-execute (e-harness-tools harness) read-call)))
      (should (equal (plist-get result :status) 'ok))
      (should (equal (plist-get result :content)
                     "Reference guide content.")))))

(ert-deftest e-harness-test-store-resources-expose-glob-and-search ()
  "Capability e:// store resources expose generated glob and search tools."
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
                    :content "Reference guide needle")))))
         (layer (e-layer-create
                 :id 'reference-layer
                 :name "Reference Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (should
     (equal (plist-get
             (e-tools-execute
              (e-harness-tools harness)
              '(:id "call-1"
                :name "glob"
                :arguments (:uri "e://reference-capability"
                            :pattern "refs/*"
                            :limit 5)))
             :content)
            '(:resources [(:uri "e://reference-capability/refs/guide.md"
                            :name "refs/guide.md"
                            :kind resource)]
              :truncated nil)))
    (should
     (equal (plist-get
             (e-tools-execute
              (e-harness-tools harness)
              '(:id "call-2"
                :name "search"
                :arguments (:uri "e://reference-capability"
                            :query "needle"
                            :glob "refs/*"
                            :limit 5)))
             :content)
            '(:matches [(:uri "e://reference-capability/refs/guide.md"
                          :line 1
                          :column 17
                          :text "Reference guide needle")]
              :truncated nil)))))

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
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
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
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
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
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
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

(ert-deftest e-harness-test-display-options-skip-tool-definitions ()
  "Display options merge model settings without materializing tools."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil)
                  :default-options '(:model "default-model"
                                     :reasoning-effort "medium"))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-set-session-model harness "session-1" "session-model")
    (cl-letf (((symbol-function 'e-harness-tools)
               (lambda (&rest _args)
                 (error "tools should not be materialized"))))
      (let ((options (e-harness-display-options harness "session-1")))
        (should (equal (plist-get options :model) "session-model"))
        (should (equal (plist-get options :reasoning-effort) "medium"))
        (should-not (plist-get options :tools))))))

(ert-deftest e-harness-test-persists-activity-events-and-tags_turn_messages ()
  "Harness turn events persist as activity, and messages keep their turn id."
  (let* ((backend (e-backend-fake-create
                   :items '((:type reasoning-delta :content "thinking")
                            (:type reasoning-raw-delta :content "raw thinking")
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
                       reasoning-raw-delta
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
      (e-harness-prompt-async harness "session-1" "fresh prompt")
      (should (equal (plist-get (e-harness-wait harness "session-1" 1.0)
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
      (e-harness-prompt-async harness "session-1" "fresh prompt")
      (should (equal (plist-get (e-harness-wait harness "session-1" 1.0)
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
      (e-harness-subscribe harness (lambda (event) (push event events)))
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
      (e-harness-prompt-async harness "session-1" "fresh prompt")
      (should (equal (plist-get (e-harness-wait harness "session-1" 1.0)
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
      (e-harness-prompt-async harness "session-1" "fresh prompt")
      (should (equal (plist-get (e-harness-wait harness "session-1" 1.0)
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
      (e-harness-prompt-async harness "session-1" "fresh prompt")
      (should (equal (plist-get (e-harness-wait harness "session-1" 1.0)
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
     (e-harness-compact-session harness "session-1" :keep-recent-tokens 1)
     :type 'user-error)
    (should-not (e-session-compactions store "session-1"))))

(ert-deftest e-harness-test-compaction-error-message-is-not-prefixed ()
  "Compaction-failed payload carries the bare reason, not a stacked prefix.
Regression: `e-compaction-error''s `define-error' message already starts with
\"Context compaction failed\", and the chat shell prepends it again, so the
backend-error-message helper must return only the bare reason."
  (let ((err (list 'e-compaction-error
                   "No safe message boundary available for compaction")))
    (should (equal (e-harness--backend-error-message err)
                   "No safe message boundary available for compaction"))
    (should-not (string-match-p "Context compaction failed"
                                (e-harness--backend-error-message err)))))

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
                          (e-tools-register
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
    (let ((record (e-harness-compact-session
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
     (e-harness-compact-session harness "session-1" :keep-recent-tokens 1)
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

(ert-deftest e-harness-test-workspace-roots-default-to-primary-only ()
  "Without configured extras, workspace roots are just the primary root."
  (let* ((primary (file-name-as-directory (make-temp-file "e-ws-primary-" t)))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-workspace-roots-alist nil))
    (unwind-protect
        (progn
          (e-harness-create-session
           harness :id "s1" :metadata (list :project-root primary))
          (should (equal (e-harness-workspace-roots harness "s1")
                         (list primary))))
      (delete-directory primary t))))

(ert-deftest e-harness-test-workspace-roots-include-configured-extras ()
  "Configured extras for an ancestor key widen a session's workspace roots."
  (let* ((primary (file-name-as-directory (make-temp-file "e-ws-primary-" t)))
         (extra (file-name-as-directory (make-temp-file "e-ws-extra-" t)))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-workspace-roots-alist (list (cons primary (list extra)))))
    (unwind-protect
        (progn
          (e-harness-create-session
           harness :id "s1" :metadata (list :project-root primary))
          (should (equal (e-harness-workspace-roots harness "s1")
                         (list primary extra))))
      (delete-directory primary t)
      (delete-directory extra t))))

(ert-deftest e-harness-test-workspace-roots-match-descendant-primary ()
  "An alist key that is an ancestor of the primary root still contributes."
  (let* ((parent (file-name-as-directory (make-temp-file "e-ws-parent-" t)))
         (primary (file-name-as-directory
                   (expand-file-name "child/" parent)))
         (extra (file-name-as-directory (make-temp-file "e-ws-extra-" t)))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-workspace-roots-alist (list (cons parent (list extra)))))
    (unwind-protect
        (progn
          (make-directory primary t)
          (e-harness-create-session
           harness :id "s1" :metadata (list :project-root primary))
          (should (member extra
                          (e-harness-workspace-roots harness "s1"))))
      (delete-directory parent t)
      (delete-directory extra t))))

(provide 'e-harness-test)

;;; e-harness-test.el ends here
