;;; e-harness-turn-composition-test.el --- Public harness turn composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Prompt, submission, queue, retry, wait, and settlement scenarios.

;;; Code:

(require 'ert)
(load (expand-file-name "e-harness-composition-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

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
                                :model-context
                                (plist-get context :model-context)
                                :assistant-message
                                (plist-get context :assistant-message)))
               value)))))
         (backend (e-backend-fake-create
                   :items `((:type assistant-message :content ,assistant-text)
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (should (equal (plist-get seen :value)
                   `(:status done :reason stop :assistant-content
                     ,assistant-text)))
    (should (equal (plist-get seen :session-id) "session-1"))
    (should (stringp (plist-get seen :turn-id)))
    (should (equal (plist-get (plist-get seen :model-context) :strategy)
                   'transcript-stack))
    (should (equal (plist-get (plist-get seen :assistant-message) :role)
                   'assistant))
    (should (equal (plist-get (plist-get seen :assistant-message) :content)
                   assistant-text))))

(ert-deftest e-harness-test-async-turn-finished-hook-receives-detached-metadata ()
  "An async terminal hook consumes bounded turn state, never a session mirror."
  (let* ((directory (make-temp-file "e-harness-terminal-metadata-" t))
         (metadata '(:org-canvas-ref
                     (:uri "file:///tmp/daily.org" :mode org)))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (seen nil)
         (capability
          (e-capability-create
           :id 'test-terminal-metadata
           :hooks
           (list
            (e-hook-create
             :id "50-capture-terminal-metadata"
             :point :turn-finished
             :handler
             (lambda (value context)
               (setq seen (copy-tree
                           (plist-get context :session-metadata) t))
               value)))))
         (harness
          (e-harness-create
           :backend
           (e-backend-fake-create
            :items '((:type assistant-message :content "answer")
                     (:type done :reason stop)))
           :sessions store)))
    (unwind-protect
        (progn
          (e-harness-activate-capability harness capability)
          (e-harness-create-session harness :id "session-1" :metadata metadata)
          (cl-letf (((symbol-function 'e-session-local-state)
                     (lambda (&rest arguments)
                       (ert-fail
                        (format "terminal hook read session aggregate: %S"
                                arguments)))))
            (e-harness-test-prompt-batch harness "session-1" "question"))
          (should (equal seen metadata)))
      (ignore-errors (e-session-sqlite-store-close store))
      (delete-directory directory t))))

(ert-deftest e-harness-test-turn-finished-hook-error-settles-visible-failure ()
  "An unexpected terminal hook error cannot strand a completed provider turn."
  (let* ((capability
          (e-capability-create
           :id 'test-terminal-error
           :hooks
           (list
            (e-hook-create
             :id "50-fail-terminal-hook"
             :point :turn-finished
             :handler
             (lambda (_value _context)
               (error "terminal hook failed"))))))
         (harness
          (e-harness-create
           :backend
           (e-backend-fake-create
            :items '((:type assistant-message :content "answer")
                     (:type done :reason stop)))))
         events)
    (e-harness-activate-capability harness capability)
    (e-harness-activity-subscribe
     harness (lambda (event) (push (copy-tree event t) events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 1.0)))
      (should (eq (plist-get settled :status) 'error))
      (should (string-match-p "terminal hook failed"
                              (plist-get settled :error)))
      (should (equal (plist-get settled :error-details)
                     '(:stage turn-finished-hooks))))
    (should-not (plist-get (e-harness-state harness "session-1")
                           :active-turn))
    (should (= (cl-count 'turn-failed events
                         :key (lambda (event) (plist-get event :type)))
               1))
    (should-not (cl-find 'turn-finished events
                         :key (lambda (event) (plist-get event :type))))))

(ert-deftest e-harness-test-async-message-append-publishes-semantic-message ()
  "An accepted async append never exposes its work handle as a message."
  (let* ((harness (e-harness-create))
         (session-id "session-async-message")
         (turn-id "turn-async-message")
         (entry (list :id turn-id :status 'running))
         (work (e-work-prepare
                (e-work-spec-create
                 :id "test-session-append" :execution 'cooperative
                 :interactive-policy 'async :owner 'test
                 :runner (lambda (&rest _arguments) :deferred))
                nil))
         events returned)
    (unwind-protect
        (progn
          (e-harness-create-session harness :id session-id)
          (puthash session-id entry (e-harness-active-turns harness))
          (e-harness-activity-subscribe
           harness (lambda (event) (push event events)) :session-id session-id)
          (cl-letf (((symbol-function 'e-session-append-message)
                     (lambda (&rest _arguments) work)))
            (setq returned
                  (e-harness-turn--append-message
                   harness session-id turn-id
                   '(:role assistant :content "accepted before commit"))))
          (should-not (e-work-handle-p returned))
          (should (eq (plist-get returned :role) 'assistant))
          (should (equal (plist-get returned :content)
                         "accepted before commit"))
          (should
           (equal
            (plist-get
             (plist-get
              (seq-find (lambda (event)
                          (eq (plist-get event :type) 'message-added))
                        events)
              :payload)
             :message)
            returned))
          (should
           (equal (e-harness-turn--turn-assistant-message
                   harness session-id turn-id)
                  returned)))
      (unless (memq (plist-get (e-work-status work) :state)
                    '(finished failed cancelled))
        (e-work-cancel work)))))

(ert-deftest e-harness-test-sqlite-turn-waits-for-input-commit-before-context ()
  "A SQLite turn cannot query context or start its provider before input commits."
  (let* ((harness
          (e-harness-create
           :backend
           (e-backend-fake-create
            :items '((:type assistant-message :content "answer")
                     (:type done :reason stop)))))
         (session-id "session-input-commit")
         (admission
          (e-work-prepare
           (e-work-spec-create
            :id "test-input-admission" :execution 'cooperative
            :interactive-policy 'async :owner 'test
            :runner (lambda (&rest _arguments) :deferred))
           nil))
         (original-append (symbol-function 'e-session-append-message))
         (context-start-count 0))
    (e-harness-create-session harness :id session-id)
    (e-work-start-prepared admission :arguments nil)
    (cl-letf (((symbol-function 'e-session-storage-sqlite-p)
               (lambda (_store) t))
              ((symbol-function 'e-session-append-message)
               (lambda (store candidate-session-id message)
                 (if (eq (plist-get message :role) 'user)
                     admission
                   (funcall original-append
                            store candidate-session-id message))))
              ((symbol-function 'e-harness-turn-context-start)
               (lambda (candidate-harness candidate-session-id turn-id)
                 (cl-incf context-start-count)
                 (let ((work
                        (e-work-prepare
                         (e-work-spec-create
                          :id "test-context-read" :execution 'cooperative
                          :interactive-policy 'async :owner 'test
                          :runner (lambda (&rest _arguments) :deferred))
                         nil)))
                   (e-work-start-prepared work :arguments nil)
                   (e-work-finish
                    work
                    (e-harness-turn-context
                     candidate-harness candidate-session-id turn-id))
                   work))))
      (e-harness-test-prompt-async harness session-id "question")
      (should (= context-start-count 0))
      (should (eq (plist-get (e-work-status admission) :state) 'started))
      (e-work-finish admission '(:status posted :position 1))
      (should (= context-start-count 1))
      (should (eq (plist-get (e-harness-wait-batch harness session-id 1.0)
                             :status)
                  'done)))))

(ert-deftest e-harness-test-turn-finished-is-published-after-hook-audit ()
  "The public terminal event follows all turn-finished hook side effects."
  (let* ((events nil)
         (capability
          (e-capability-create
           :id 'test-terminal-order
           :hooks
           (list
            (e-hook-create
             :id "50-record-terminal-order"
             :point :turn-finished
             :handler
             (lambda (value context)
               (e-harness-record-hook-audit
                (plist-get context :harness)
                (plist-get context :session-id)
                (plist-get context :turn-id)
                :owner 'test-terminal-order
                :hook-id "50-record-terminal-order"
                :outcome 'checked
                :summary "Checked before settlement")
               value)))))
         (harness
          (e-harness-create
           :backend
           (e-backend-fake-create
            :items '((:type assistant-message :content "answer")
                     (:type done :reason stop))))))
    (e-harness-activate-capability harness capability)
    (e-harness-activity-subscribe
     harness (lambda (event) (push (plist-get event :type) events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (setq events (nreverse events))
    (should (= (cl-count 'turn-finished events) 1))
    (should (< (cl-position 'hook-audit events)
               (cl-position 'turn-finished events)))
    (should (equal
             (mapcar (lambda (event) (plist-get event :event-type))
                     (e-harness-session-activity-events harness "session-1"))
             '(turn-started provider-request-started provider-request-finished
               context-frame-consumed hook-audit turn-finished)))))

(ert-deftest e-harness-test-subscribe-can-filter-events-by-session ()
  "Session-scoped subscribers only receive events for their session."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (first-events nil)
         (second-events nil)
         (all-events nil))
    (e-harness-activity-subscribe harness
                         (lambda (event) (push event first-events))
                         :session-id "session-1")
    (e-harness-activity-subscribe harness
                         (lambda (event) (push event second-events))
                         :session-id "session-2")
    (e-harness-activity-subscribe harness
                         (lambda (event) (push event all-events)))
    (e-harness-activity-emit-turn-event harness "session-1" "turn-1" 'turn-started nil)
    (e-harness-activity-emit-turn-event harness "session-2" "turn-2" 'turn-started nil)
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
         (subscription (e-harness-activity-subscribe
                        harness
                        (lambda (event) (push event events))
                        :session-id "session-1")))
    (e-harness-activity-emit-turn-event harness "session-1" "turn-1" 'turn-started nil)
    (should (= (length events) 1))
    (e-harness-activity-unsubscribe harness subscription)
    (e-harness-activity-emit-turn-event harness "session-1" "turn-2" 'turn-started nil)
    (should (= (length events) 1))
    (should-not (member subscription (e-harness-subscribers harness)))
    (e-harness-activity-unsubscribe harness subscription)
    (should (= (length events) 1))))

(ert-deftest e-harness-test-abort-idle-session-is-explicit-error ()
  "Aborting without an active turn surfaces a lifecycle error."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-harness-test-abort harness "session-1")
     :type 'e-harness-no-active-turn)))

(ert-deftest e-harness-test-async-prompt-wait-settles-turn ()
  "Async prompting tracks an active turn until wait settles it."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id (e-harness-test-prompt-async harness "session-1" "question")))
      (should (equal (plist-get (e-harness-state harness "session-1")
                                :active-turn)
                     turn-id))
      (should (equal (plist-get (e-harness-wait-batch harness "session-1" 1.0)
                                :status)
                     'done))
      (should (equal (plist-get (e-harness-state harness "session-1")
                                :active-turn)
                     nil)))))

(ert-deftest e-harness-test-sync-prompt-rejects-hot-path-before-submit ()
  "The synchronous prompt wrapper cannot start inside a marked hot path."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (let ((err (should-error
                (e-request-with-hot-path 'chat-submit
                  (e-harness-test-prompt-batch harness "session-1" "question"))
                :type 'e-request-blocking-call-in-hot-path)))
      (should (equal (cdr err)
                     '(e-harness-attached-turn-submit-batch chat-submit))))
    (should-not (e-harness-messages harness "session-1"))
    (should-not (plist-get (e-harness-state harness "session-1")
                           :active-turn))))

(ert-deftest e-harness-test-wait-rejects-hot-path ()
  "The explicit harness wait helper cannot run inside a marked hot path."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))
                   :delay 1.0))
         (harness (e-harness-create :backend backend)))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-harness-test-prompt-async harness "session-1" "question")
          (let ((err (should-error
                      (e-request-with-hot-path 'turn-wait
                        (e-harness-wait-batch harness "session-1" 0.1))
                      :type 'e-request-blocking-call-in-hot-path)))
            (should (equal (cdr err) '(e-harness-wait-batch turn-wait))))
          (should (plist-get (e-harness-state harness "session-1")
                             :active-turn)))
      (ignore-errors
        (e-harness-test-abort harness "session-1")))))

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
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question" :delay 1.0)
    (should (equal called nil))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user)))
    (should (member 'message-added
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))
    (e-harness-test-abort harness "session-1")))

(ert-deftest e-harness-test-message-hidden-p-recognizes-both-channels ()
  "A message is hidden via top-level `:display' or `:metadata' `:display'.
The first-attempt reply is hidden with a top-level symbol; the follow-up
prompt rides the metadata channel and its value may replay as a string."
  (should-not (e-harness-message-hidden-p '(:role assistant :content "x")))
  (should (e-harness-message-hidden-p
           '(:role assistant :content "x" :display hidden)))
  (should (e-harness-message-hidden-p
           '(:role user :content "x" :metadata (:display hidden))))
  (should (e-harness-message-hidden-p
           '(:role user :content "x" :metadata (:display "hidden"))))
  (should-not (e-harness-message-hidden-p
               '(:role assistant :content "x" :display inline))))

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
    (e-harness-test-prompt-async harness "session-1" "question" :delay 1.0)
    (e-harness-test-abort harness "session-1")
    (should (equal (plist-get (e-harness-wait-batch harness "session-1" 0.1)
                              :status)
                   'cancelled))
    (should (equal called nil))))

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
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (should (e-harness-test-abort harness "session-1"))
    (should (equal (plist-get (e-harness-wait-batch harness "session-1" 0.1)
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
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 1.0)))
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
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 1.0)))
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




(defun e-harness-test--rate-limited-backend (failures)
  "Return a backend that fails with HTTP 429 FAILURES times, then succeeds.
Counts attempts in the returned (BACKEND . COUNTER) cons's cdr."
  (let ((counter (list 0)))
    (cons
     (e-backend-create
      :name "rate-limited"
      :normalize-error-details
      (lambda (_message details _condition)
        (append details '(:retryable t :retry-reason rate-limit)))
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
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 5.0)))
      (should (equal (plist-get settled :status) 'done)))
    ;; Two failures + one success.
    (should (= (car counter) 3))
    (let ((types (mapcar (lambda (e) (plist-get e :type)) events)))
      (should (= 2 (seq-count (lambda (ty) (eq ty 'turn-retrying)) types)))
      (should-not (memq 'turn-failed types)))
    (let ((retry-events
           (seq-filter
            (lambda (event)
              (eq (plist-get event :event-type) 'turn-retrying))
            (e-harness-session-activity-events harness "session-1"))))
      (should (= (length retry-events) 2))
      (dolist (event retry-events)
        (let ((payload (plist-get event :payload)))
          (should (equal (plist-get payload :error)
                         "429: Rate limit exceeded for api_key:[REDACTED]"))
          (should (equal (plist-get payload :details)
                         '(:status 429
                           :retryable t
                           :retry-reason rate-limit)))
          (should (numberp (plist-get payload :backoff-seconds))))))))

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
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 5.0)))
      (should (equal (plist-get settled :status) 'error))
      (should (string-match-p "429" (plist-get settled :error))))
    (should (member 'turn-failed
                    (mapcar (lambda (e) (plist-get e :type)) events)))))

(ert-deftest e-harness-test-retry-deadline-resets-after-successful-request ()
  "Successful provider work does not consume the transient-retry budget.
A turn that blips retryably, recovers, then spends longer than the retry
budget doing real work (a tool call) must still retry a later blip: the
budget bounds a consecutive failure burst, not the turn's total wall clock."
  (let* ((e-harness-retry-initial-backoff-seconds 0.02)
         (e-harness-retry-backoff-multiplier 1.0)
         (e-harness-retry-max-backoff-seconds 0.02)
         ;; Budget covers a short failure burst but is far shorter than the
         ;; tool's runtime, so a never-reset deadline would be long expired
         ;; by the time the follow-up request blips.
         (e-harness-retry-max-elapsed-seconds 0.2)
         (tool-delay 0.5)
         (counter (list 0))
         (backend
          (e-backend-create
           :name "flaky-then-tool"
           :normalize-error-details
           (lambda (_message details _condition)
             (append details
                     '(:retryable t
                       :retry-reason provider-unavailable)))
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-error)
              ;; Publish a request handle like the real adapters, so the loop
              ;; emits provider-request-started/finished lifecycle events.
              (when on-request-start
                (funcall on-request-start
                         (e-backend-request-create :cancel (lambda () t))))
              (run-at-time
               0 nil
               (lambda ()
                 (cl-incf (car counter))
                 (pcase (car counter)
                   ;; First request blips: plants the retry deadline.
                   (1 (funcall on-item
                               '(:type backend-error
                                 :content "529: overloaded_error"
                                 :payload (:status 529))))
                   ;; Recovers into a tool call.
                   (2 (funcall on-item '(:type tool-call
                                         :id "call-1"
                                         :name "slow-tool"
                                         :arguments (:text "hi")))
                      (funcall on-item '(:type done :reason tool-use)))
                   ;; Follow-up after the long tool blips again.
                   (3 (funcall on-item
                               '(:type backend-error
                                 :content "529: overloaded_error"
                                 :payload (:status 529))))
                   ;; Final recovery.
                   (_ (funcall on-item
                               '(:type assistant-message :content "done"))
                      (funcall on-item '(:type done :reason stop))))
                 (funcall on-done '(:status done))))
              nil))))
         (tools (e-tools-registry-create))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-tools-test-register
     tools
     :name "slow-tool"
     :description "Runs longer than the retry budget."
     :start
     (cl-function
      (lambda (&key arguments on-done on-error on-request-start)
        (ignore arguments on-error)
        (let ((request (e-tools-request-create :cancel (lambda () t))))
          (when on-request-start (funcall on-request-start request))
          (run-at-time tool-delay nil
                       (lambda () (funcall on-done "tool done")))
          request))))
    (cl-letf (((symbol-function 'e-harness-tools)
               (lambda (_harness &optional _session-id _turn-id) tools)))
      (e-harness-activity-subscribe harness (lambda (event) (push event events)))
      (e-harness-create-session harness :id "session-1")
      (e-harness-test-prompt-async harness "session-1" "question")
      (let ((settled (e-harness-wait-batch harness "session-1" 5.0)))
        (should (equal (plist-get settled :status) 'done)))
      (let ((types (mapcar (lambda (e) (plist-get e :type)) events)))
        ;; The follow-up blip is retried rather than settling the turn failed.
        (should (= 2 (seq-count (lambda (ty) (eq ty 'turn-retrying)) types)))
        (should-not (memq 'turn-failed types))))))

(ert-deftest e-harness-test-non-retryable-error-fails-immediately ()
  "A genuine client-fault backend error settles without any retry."
  (let* ((e-harness-retry-max-elapsed-seconds 5.0)
         (backend (e-backend-fake-create
                   :items '((:type backend-error
                              :content "400: invalid request"
                              :payload (:status 400)))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 1.0)))
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
    (e-harness-test-prompt-async harness "session-1" "first")
    (should-error
     (e-harness-test-prompt-async harness "session-1" "second")
     :type 'e-harness-active-turn-exists)
    (funcall finish)
    (should (equal (plist-get (e-harness-wait-batch harness "session-1" 1.0)
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
     (e-harness-test-queue-prompt harness "session-1" "follow up")
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
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id (e-harness-test-prompt-async harness "session-1" "first")))
      (let ((queue-id
             (e-harness-test-queue-prompt
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
          (should (eq (plist-get (plist-get item :metadata) :source)
                      'chat-composer))
          (should (eq (plist-get (plist-get item :metadata)
                                 :board-endpoint-token)
                      e-harness-test--attachment-token))
          (should (stringp (plist-get item :created-at))))
        (should (member 'queue-changed
                        (mapcar (lambda (event) (plist-get event :type))
                                events)))
        ;; Do not leave the held turn and its queued successor for the real
        ;; provider deadline timer to settle after this test has returned.
        (e-harness-reset harness "session-1")))))

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
    (e-harness-test-prompt-async harness "session-1" "first")
    (e-harness-test-queue-prompt
     harness "session-1" "second"
     :references '((:uri "buffer://source"))
     :metadata '(:source chat-composer))
    (e-harness-test-queue-prompt harness "session-1" "third")
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
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "first")
    (e-harness-test-queue-prompt harness "session-1" "second")
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
     (e-harness-test-steer-active-turn harness "session-1" "focus here")
     :type 'e-harness-no-active-turn)))

(ert-deftest e-harness-test-steer-active-turn-drains-in-same-turn ()
  "Pending steering input is sampled as a user message in the same turn."
  (require 'e-board-runtime)
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
         (harness (e-harness-create :backend backend))
         (token (e-harness-test--port-token harness "session-1")))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id
           (e-harness-test-prompt-async
            harness "session-1" "first"
            :metadata (list :input-origin 'board
                            :board-endpoint-token token))))
      (e-harness-test-steer-active-turn
       harness "session-1" "focus here"
       :metadata (list :source 'chat-composer
                       :board-endpoint-token token))
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
      (let ((entry (e-harness-wait-batch harness "session-1" 0.1)))
        (should (eq (plist-get entry :status) 'done)))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :content))
                             (e-harness-messages harness "session-1"))
                     '("first" "answer 1" "focus here" "answer 2")))
      (should (equal (plist-get (nth 2 (e-harness-messages harness "session-1"))
                                :turn-id)
                     turn-id)))))

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
    (e-harness-test-prompt-batch harness "session-1" "question")
    (should-not
     (e-session-local-provider-anchors (e-harness-sessions harness)
                                 "session-1"))))

(ert-deftest e-harness-test-reset-clears-session-messages ()
  "Reset clears transcript messages for a session."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create
                            :items '((:type assistant-message :content "answer")
                                     (:type done :reason stop))))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (e-harness-reset harness "session-1")
    (should (equal (e-harness-messages harness "session-1") nil))))

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
                                   :arguments (list :stated_purpose
                                                     "Run the requested code."
                                                     :code code)))
                    (funcall on-item '(:type done :reason tool-use)))
                (setq second-request-messages messages)
                (should (equal (mapcar (lambda (message)
                                         (plist-get message :role))
                                       messages)
                               '(user tool-call system tool)))
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
              (e-tools-test-register
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
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-test-prompt-batch harness "session-1" "chain tools")
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

(provide 'e-harness-turn-composition-test)

;;; e-harness-turn-composition-test.el ends here
