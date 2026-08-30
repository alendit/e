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
(require 'e-actions)
(require 'e-backend)
(require 'e-chat-session)
(require 'e-base)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-context)
(require 'e-dev-profile)
(load (expand-file-name "e-tools-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-emacs-tools)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-layers)
(require 'e-operations)
(require 'e-prompts)
(require 'e-request)
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

(defun e-harness-test--curation-frame
    (&optional generation-id frame-id value observation-id source-ref
              source-fingerprint)
  "Return one small live frame for harness commit-boundary tests."
  (e-context-lifetime-frame-create
   :id (or frame-id "frame:harness-test")
   :generation-id (or generation-id "generation:harness-test")
   :consumer-request-id "consumer:harness-test"
   :observations
   (list
    (list :observation-id (or observation-id "observation:harness-test")
          :kind "current-state"
          :source-entry-ref (or source-ref "source:harness-test")
          :source-fingerprint
          (or source-fingerprint "fingerprint:harness-test")
          :effective-delivery "inherited"
          :body (list :role 'system
                      :content (or value "HARNESS-EXACT-VALUE"))))))

(defun e-harness-test--tool-result-curation-frame
    (generation-id &optional frame-id call-id value observation-id source-ref
                  source-fingerprint)
  "Return one live tool-result source frame for erasure tests."
  (let ((call-id (or call-id "call:harness-tool-result")))
    (e-context-lifetime-frame-create
     :id (or frame-id "frame:harness-tool-result")
     :generation-id generation-id
     :consumer-request-id "consumer:harness-tool-result"
     :observations
     (list
      (list :observation-id (or observation-id "observation:harness-tool-result")
            :kind "tool-result"
            :source-entry-ref (or source-ref "source:harness-tool-result")
            :source-fingerprint
            (or source-fingerprint "fingerprint:harness-tool-result")
            :effective-delivery "inherited"
            :body (list :role 'tool
                        :content (list :tool-call-id call-id
                                        :name "inspect"
                                        :status 'ok
                                        :content
                                        (or value "HARNESS-TOOL-RESULT"))))))))

(defun e-harness-test--two-tool-result-curation-frame (generation-id)
  "Return a two-source tool-result frame for mixed curation tests."
  (e-context-lifetime-frame-create
   :id "frame:harness-mixed-tool-results"
   :generation-id generation-id
   :consumer-request-id "consumer:harness-mixed-tool-results"
   :observations
   (list
    (list :observation-id "observation:harness-mixed-1"
          :kind "tool-result"
          :source-entry-ref "source:harness-mixed-1"
          :source-fingerprint "fingerprint:harness-mixed-1"
          :effective-delivery "inherited"
          :body (list :role 'tool
                      :content (list :tool-call-id "call:harness-mixed-1"
                                      :name "inspect"
                                      :status 'ok
                                      :content "HARNESS-MIXED-ONE")))
    (list :observation-id "observation:harness-mixed-2"
          :kind "tool-result"
          :source-entry-ref "source:harness-mixed-2"
          :source-fingerprint "fingerprint:harness-mixed-2"
          :effective-delivery "inherited"
          :body (list :role 'tool
                      :content (list :tool-call-id "call:harness-mixed-2"
                                      :name "inspect"
                                      :status 'ok
                                      :content "HARNESS-MIXED-TWO"))))))

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

(ert-deftest e-harness-test-effective-capability-config-caches-resolved-values ()
  "Repeated equivalent config reads avoid resolving directory-local state."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
        (calls 0))
    (cl-letf (((symbol-function 'e-capability-config-resolve)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (list :value "resolved"))))
      (let ((first (e-harness-effective-capability-config
                    harness 'dummy-config
                    e-harness-test--capability-config-options))
            (second (e-harness-effective-capability-config
                     harness 'dummy-config
                     e-harness-test--capability-config-options)))
        (should (= calls 1))
        (should (equal first second))
        (setf (plist-get first :value) "mutated")
        (should (equal (plist-get second :value) "resolved"))))))

(ert-deftest e-harness-test-effective-capability-config-invalidates-runtime-cache ()
  "Changing runtime config discards derived effective config for the harness."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
        (calls 0))
    (cl-letf (((symbol-function 'e-capability-config-resolve)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (list :value (number-to-string calls)))))
      (e-harness-effective-capability-config
       harness 'dummy-config e-harness-test--capability-config-options)
      (e-harness-set-capability-config harness 'dummy-config '(:value "runtime"))
      (should
       (equal
        (e-harness-effective-capability-config
         harness 'dummy-config e-harness-test--capability-config-options)
        '(:value "2")))
      (should (= calls 2)))))

(ert-deftest e-harness-test-effective-capability-config-observes-global-revision ()
  "Changing global config makes a cached effective config stale."
  (let ((e-capability-config nil)
        (harness (e-harness-create :backend (e-backend-fake-create :items nil)))
        (calls 0))
    (cl-letf (((symbol-function 'e-capability-config-resolve)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (list :value (number-to-string calls)))))
      (e-harness-effective-capability-config
       harness 'dummy-config e-harness-test--capability-config-options)
      (setq e-capability-config '((dummy-config :value "global")))
      (should
       (equal
        (e-harness-effective-capability-config
         harness 'dummy-config e-harness-test--capability-config-options)
        '(:value "2")))
      (should (= calls 2)))))

(ert-deftest e-harness-test-effective-capability-config-does-not-cache-overrides ()
  "Explicit override values remain per-call and are never retained by the cache."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
        (calls 0))
    (cl-letf (((symbol-function 'e-capability-config-resolve)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (list :value (number-to-string calls)))))
      (e-harness-effective-capability-config
       harness 'dummy-config e-harness-test--capability-config-options
       :overrides '(:value "one"))
      (e-harness-effective-capability-config
       harness 'dummy-config e-harness-test--capability-config-options
       :overrides '(:value "two"))
      (should (= calls 2)))))

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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (let ((messages (e-harness-messages harness "session-1")))
      (should (string-match-p "\\`[0-9A-HJKMNP-TV-Z]\\{26\\}\\'"
                              (plist-get (car messages) :turn-id)))
      (should (equal (mapcar (lambda (message) (plist-get message :role)) messages)
                     '(user assistant)))
      (should (eq (plist-get (car messages) :origin) 'human))
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
    (e-harness--install-activity-sink
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
               hook-audit turn-finished)))))

(ert-deftest e-harness-test-subscribe-can-filter-events-by-session ()
  "Session-scoped subscribers only receive events for their session."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (first-events nil)
         (second-events nil)
         (all-events nil))
    (e-harness--install-activity-sink harness
                         (lambda (event) (push event first-events))
                         :session-id "session-1")
    (e-harness--install-activity-sink harness
                         (lambda (event) (push event second-events))
                         :session-id "session-2")
    (e-harness--install-activity-sink harness
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

(ert-deftest e-harness-test-durable-activity-provenance-reaches-subscribers ()
  "A public durable event names the activity entry written before it."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (events nil))
    (e-harness-create-session harness :id "session-1")
    (e-harness--install-activity-sink harness (lambda (event) (push event events))
                         :session-id "session-1")
    (e-harness--emit-turn-event
     harness "session-1" "turn-1" 'tool-started '(:name "read"))
    (let* ((event (car events))
           (activity (car (e-harness-session-activity-events harness "session-1"))))
      (should (equal (plist-get event :payload) '(:name "read")))
      (should (equal (plist-get event :activity-entry-id)
                     (plist-get activity :id)))
      (should (= (plist-get event :board-activity-sequence)
                 (plist-get activity :board-activity-sequence))))))

(ert-deftest e-harness-test-unsubscribe-removes-subscription-idempotently ()
  "Unsubscribing removes a subscription record and can be repeated."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (events nil)
         (subscription (e-harness--install-activity-sink
                        harness
                        (lambda (event) (push event events))
                        :session-id "session-1")))
    (e-harness--emit-turn-event harness "session-1" "turn-1" 'turn-started nil)
    (should (= (length events) 1))
    (e-harness--remove-activity-sink harness subscription)
    (e-harness--emit-turn-event harness "session-1" "turn-2" 'turn-started nil)
    (should (= (length events) 1))
    (should-not (member subscription (e-harness-subscribers harness)))
    (e-harness--remove-activity-sink harness subscription)
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
                     '(e-harness--prompt-attached-batch chat-submit))))
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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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

(ert-deftest e-harness-test-set-message-display-hides-and-emits-event ()
  "Setting a message's display flips it hidden and emits `message-updated'."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (events nil))
    (e-harness-create-session harness :id "session-1")
    (let ((message (e-harness--append-message
                    harness "session-1" "turn-1"
                    '(:role assistant :content "The fix shipped in commit 42."))))
      (e-harness--install-activity-sink harness (lambda (event) (push event events)))
      (e-harness-set-message-display
       harness "session-1" (plist-get message :id) 'hidden)
      (should (eq (plist-get (car (last (e-harness-messages harness "session-1")))
                             :display)
                  'hidden))
      (let ((updated (seq-find (lambda (event)
                                 (eq (plist-get event :type) 'message-updated))
                               events)))
        (should updated)
        (should (equal (plist-get (plist-get (plist-get updated :payload)
                                             :message)
                                  :id)
                       (plist-get message :id)))
        (should (eq (plist-get (plist-get (plist-get updated :payload)
                                          :message)
                               :display)
                    'hidden))))))

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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (run-at-time 0.01 nil (lambda ()
                            (e-harness-test-abort harness "session-1")))
    (should (equal (plist-get (e-harness-wait-batch harness "session-1" 1.0)
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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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

(ert-deftest e-harness-test-retryable-error-detection ()
  "The harness consumes only the backend-normalized retry decision."
  (should (e-harness--retryable-error-p
           '(:retryable t :retry-reason rate-limit)))
  (should-not (e-harness--retryable-error-p '(:retryable nil :status 429)))
  (should-not (e-harness--retryable-error-p '(:status 503)))
  (should-not (e-harness--retryable-error-p nil)))

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

(ert-deftest e-harness-test-retry-reset-seconds-bounds-adapter-hint ()
  "The harness bounds normalized retry delays without interpreting providers."
  (let ((e-harness-retry-reset-max-wait-seconds 900.0))
    (should (= 45 (e-harness--retry-reset-seconds
                   '(:retry-after-seconds 45))))
    (should (= 1.0 (e-harness--retry-reset-seconds
                    '(:retry-after-seconds -60))))
    (should-not (e-harness--retry-reset-seconds
                 '(:retry-after-seconds 2400)))
    (should-not (e-harness--retry-reset-seconds '(:retryable t))))
  ;; A zero cap disables reset-aware waiting entirely.
  (let ((e-harness-retry-reset-max-wait-seconds 0))
    (should-not (e-harness--retry-reset-seconds
                 '(:retry-after-seconds 5)))))

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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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
      (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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

(ert-deftest e-harness-test-unsettled-state-follows-turn-and-input-owners ()
  "Harness counts active turns and queued inputs at their exact transitions."
  (let* ((e-harness--aggregate-active-turn-count 0)
         (e-harness--aggregate-queued-input-count 0)
         (e-harness--aggregate-unsettled-generation 0)
         (backend (e-backend-create
                   :name "held"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore messages options on-item on-done on-error
                                     on-request-start)))))
         (harness (e-harness-create :backend backend))
         scheduled snapshots)
    (setf (e-harness-unsettled-change-function harness)
          (lambda (snapshot) (push snapshot snapshots)))
    (e-harness-create-session harness :id "session-1")
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (push (lambda () (apply function arguments)) scheduled))))
      (e-harness-test-prompt-async harness "session-1" "first")
      (should (equal (e-harness-unsettled-state harness)
                     '(:generation 1 :active-turns 1 :queued-inputs 0)))
      (e-harness-test-queue-prompt harness "session-1" "second")
      (e-harness-test-steer-active-turn harness "session-1" "steer")
      (should (equal (e-harness-unsettled-state harness)
                     '(:generation 3 :active-turns 1 :queued-inputs 2)))
      (should (equal (e-harness-aggregate-unsettled-state)
                     '(:generation 3 :active-turns 1 :queued-inputs 2)))
      (e-harness-test-abort harness "session-1")
      (should (equal (e-harness-unsettled-state harness)
                     '(:generation 4 :active-turns 1 :queued-inputs 1)))
      (funcall (pop scheduled))
      (should (equal (e-harness-unsettled-state harness)
                     '(:generation 7 :active-turns 1 :queued-inputs 0)))
      (e-harness-test-abort harness "session-1")
      (funcall (pop scheduled))
      (should (equal (e-harness-unsettled-state harness)
                     '(:generation 8 :active-turns 0 :queued-inputs 0)))
      (should (equal (e-harness-aggregate-unsettled-state)
                     '(:generation 8 :active-turns 0 :queued-inputs 0)))
      (should (= (length snapshots) 8)))))

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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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

(ert-deftest e-harness-test-steer-active-turn-stores-pending-input ()
  "Successful steering stores only durable input on the active turn."
  (require 'e-board-runtime)
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
         (token (e-board-runtime-endpoint-token--create
                 :harness-id :live
                 :harness-object-generation 7
                 :session-id "session-1"))
         (events nil))
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id
           (e-harness-test-prompt-async
            harness "session-1" "first"
            :metadata (list :input-origin 'board
                            :board-endpoint-token token))))
      (should (equal (e-harness-test-steer-active-turn
                      harness "session-1" "focus here"
                      :metadata (list :source 'chat-composer
                                      :board-endpoint-token token))
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
                       '(:source chat-composer))))
      (let ((durable
             (cl-find 'turn-steered
                      (e-harness-session-activity-events harness "session-1")
                      :key (lambda (event)
                             (plist-get event :event-type)))))
        (should durable)
        (should (equal (plist-get (plist-get durable :payload) :metadata)
                       '(:source chat-composer)))))))

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
         (token (e-board-runtime-endpoint-token--create
                 :harness-id :live
                 :harness-object-generation 7
                 :session-id "session-1")))
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


(ert-deftest e-harness-test-tool-finished-activity-compacts-result-payload ()
  "Durable tool-finished activity stores one compact lifecycle receipt."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (let ((e-harness--trusted-tool-details-uri
           "tmp://tool-invocations/turn-1/call-1.json"))
      (e-harness--emit-turn-event
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

(ert-deftest e-harness-test-tool-finished-activity-drops-unknown-metadata ()
  "Invalid purpose status is durable without persisting invalid text."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (let ((e-harness--trusted-tool-details-uri "tmp://safe"))
      (e-harness--emit-turn-event
       harness "session-1" "turn-1" 'tool-finished
       '(:tool-call (:id "call-1" :name "probe"
                    :stated-purpose "Bearer raw-secret"
                    :metadata (:purpose-status invalid)
                    :arguments (:query "raw-nested"))
         :result (:tool-call-id "call-1" :name "probe" :status ok
                  :content "raw-result-secret"
                  :metadata (:invocation-details-uri "tmp://safe"
                             :authorization "Bearer raw-auth")))))
    (let* ((event (car (e-harness-session-activity-events
                        harness "session-1")))
           (payload (plist-get event :payload))
           (receipt (plist-get payload :receipt))
           (serialized (prin1-to-string payload)))
      (should (eq (plist-get receipt :purpose-status) 'invalid))
      (should-not (plist-member receipt :stated-purpose))
      (should-not (plist-member payload :result))
      (should-not (string-match-p
                   "raw-secret\\|raw-nested\\|raw-result-secret\\|raw-auth"
                   serialized)))))

(ert-deftest e-harness-test-tool-finished-activity-rejects-mismatched-result ()
  "A result for another call never becomes a durable receipt."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (let ((e-harness--trusted-tool-details-uri "tmp://trusted.json"))
      (e-harness--emit-turn-event
       harness "session-1" "turn-1" 'tool-finished
       '(:tool-call (:id "call-1" :name "probe"
                    :stated-purpose "Inspect the bounded probe")
         :result (:tool-call-id "call-2" :name "other" :status ok
                  :content "raw-mismatched-result")) ))
    (let* ((payload (plist-get
                     (car (e-harness-session-activity-events harness "session-1"))
                     :payload))
           (serialized (prin1-to-string payload)))
      (should-not (plist-member payload :receipt))
      (should-not (plist-member payload :result))
      (should-not (string-match-p "raw-mismatched-result" serialized)))))

(ert-deftest e-harness-test-tool-finished-activity-rejects-untrusted-details-uri ()
  "A URI copied into result metadata cannot authorize a receipt."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-harness--emit-turn-event
     harness "session-1" "turn-1" 'tool-finished
     '(:tool-call (:id "call-1" :name "probe"
                  :stated-purpose "Inspect the bounded probe")
       :result (:tool-call-id "call-1" :name "probe" :status ok
                :content "raw-untrusted-result"
                :metadata (:invocation-details-uri
                           "tmp://forged/call-1.json"))))
    (let* ((payload (plist-get
                     (car (e-harness-session-activity-events harness "session-1"))
                     :payload))
           (serialized (prin1-to-string payload)))
      (should-not (plist-member payload :receipt))
      (should-not (plist-member payload :result))
      (should-not (string-match-p "raw-untrusted-result" serialized)))))

(ert-deftest e-harness-test-tool-lifecycle-trusts-details-stage-for-receipt ()
  "The lifecycle archival stage authorizes the finished receipt transiently."
  (let* ((capability
          (e-capability-create
           :id 'receipt-tool
           :tools
           (list (lambda (registry)
                   (e-tools-test-register
                    registry
                    :name "probe"
                    :description "Return a bounded probe."
                    :handler (lambda (_arguments) "ok"))))
           :hooks
           (list
            (e-hook-create
             :id "40-test-details"
             :point :invocation-details
             :handler
             (lambda (result context)
               (plist-put context :invocation-details-uri
                          "tmp://tool-invocations/turn-1/call-1.json")
               result)))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (list capability)))
         (call '(:id "call-1" :name "probe"
                 :stated-purpose "Inspect the bounded probe"
                 :arguments nil))
         result
         failure)
    (e-harness-create-session harness :id "session-1")
    (e-tool-lifecycle-start-call
     (e-harness-tool-lifecycle harness "session-1" "turn-1")
     call
     :on-done
     (lambda (value)
       (setq result value)
       (e-harness--emit-turn-event
        harness "session-1" "turn-1" 'tool-finished
        (list :tool-call call :result value)))
     :on-error (lambda (err) (setq failure err)))
    (should-not failure)
    (should (e-tools-result-p result))
    (let* ((event (car (e-harness-session-activity-events
                        harness "session-1")))
           (payload (plist-get event :payload))
           (receipt (plist-get payload :receipt)))
      (should (equal (plist-get receipt :details-uri)
                     "tmp://tool-invocations/turn-1/call-1.json"))
      (should-not (plist-member payload :trusted-details-uri))
      (should-not (plist-member (plist-get payload :result)
                                :trusted-details-uri)))))

(ert-deftest e-harness-test-tool-started-activity-retains-purpose-without-arguments ()
  "Durable tool-started activity retains identity and stated purpose only."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-harness--emit-turn-event
     harness "session-1" "turn-1" 'tool-started
     '(:id "call-1" :name "probe"
       :stated-purpose "Inspect the bounded probe"
       :arguments (:query "raw-query")))
    (let* ((event (car (e-harness-session-activity-events
                        harness "session-1")))
           (payload (plist-get event :payload))
           (serialized (prin1-to-string payload)))
      (should (equal payload
                     '(:id "call-1"
                       :name "probe"
                       :stated-purpose "Inspect the bounded probe")))
      (should-not (string-match-p "raw-query" serialized)))))

(ert-deftest e-harness-test-tool-receipt-survives-persistent-reopen-without-preview ()
  "Reopened durable activity keeps receipt identity without raw content."
  (let* ((directory (make-temp-file "e-harness-tool-receipt-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-harness--emit-turn-event
           harness "session-1" "turn-1" 'tool-started
           '(:id "call-1" :name "bash"
             :stated-purpose "Run the bounded command"
             :arguments (:command "raw-command-secret")))
          (let ((e-harness--trusted-tool-details-uri
                 "tmp://tool-invocations/turn-1/call-1.json"))
            (e-harness--emit-turn-event
             harness "session-1" "turn-1" 'tool-finished
             '(:tool-call (:id "call-1" :name "bash"
                          :stated-purpose "Run the bounded command"
                          :arguments (:command "raw-command-secret"))
               :result (:tool-call-id "call-1"
                        :name "bash"
                        :status ok
                        :content "raw-result-secret"
                        :metadata (:invocation-details-uri
                                   "tmp://tool-invocations/turn-1/call-1.json")))))
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (events (e-session-activity-events reopened "session-1"))
                 (started (car events))
                 (finished (cadr events))
                 (started-payload (plist-get started :payload))
                 (finished-payload (plist-get finished :payload))
                 (receipt (plist-get finished-payload :receipt))
                 (serialized (prin1-to-string events)))
            (should (equal started-payload
                           '(:id "call-1"
                             :name "bash"
                             :stated-purpose "Run the bounded command")))
            (should (equal receipt
                           '(:tool-call-id "call-1"
                             :tool "bash"
                             :status "ok"
                             :stated-purpose "Run the bounded command"
                             :details-uri
                             "tmp://tool-invocations/turn-1/call-1.json"
                             :details-lifetime "session-tmp")))
            (should-not (string-match-p
                         "raw-command-secret\\|raw-result-secret"
                         serialized))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-malformed-tool-finished-activity-never-falls-back-raw ()
  "Malformed legacy payloads retain identity, not arbitrary raw values."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-harness--emit-turn-event
     harness "session-1" "turn-1" 'tool-finished
     '(:tool-call (:id "call-1" :name "legacy")
       :result (:content "token=raw-secret")
       :authorization "Bearer raw-auth"
       :unknown (:secret "raw-nested")))
    (let* ((event (car (e-harness-session-activity-events
                        harness "session-1")))
           (payload (plist-get event :payload))
           (serialized (prin1-to-string payload)))
      (should (equal (plist-get (plist-get payload :tool-call) :id) "call-1"))
      (should-not (plist-member payload :result))
      (should-not
       (string-match-p "raw-secret\\|raw-auth\\|raw-nested" serialized)))))

(ert-deftest e-harness-test-activity-events-declare-persistence-class ()
  "Every durable activity event declares why it is persisted."
  (dolist (type e-harness--durable-activity-event-types)
    (should (memq (e-harness-activity-event-class type)
                  '(audit replay presentation-log)))))

(ert-deftest e-harness-test-action-events-are-durable ()
  "Action dispatch emits durable action activity events."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (e-actions-call
     'chat-session
     :rename
     '(:name "Action telemetry")
     (list :harness harness :session-id "session-1" :turn-id "turn-1"))
    (let ((types (mapcar (lambda (event)
                           (plist-get event :event-type))
                         (e-harness-session-activity-events
                          harness "session-1"))))
      (should (equal types '(action-started action-finished))))))

(ert-deftest e-harness-test-token-usage-events-are-durable ()
  "Backend token usage events are retained in session activity."
  (let* ((backend (e-backend-fake-create
                   :items
                   '((:type assistant-message :content "answer")
                     (:type token-usage
                      :usage (:input-tokens 202598
                              :cached-input-tokens 7552
                              :cache-creation-input-tokens 4096
                              :output-tokens 419
                              :reasoning-output-tokens 139
                              :total-tokens 203017))
                     (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (let* ((events (e-harness-session-activity-events harness "session-1"))
           (usage-event
            (seq-find (lambda (event)
                        (eq (plist-get event :event-type) 'token-usage))
                      events)))
      (should usage-event)
      (let ((payload (plist-get usage-event :payload)))
        (should (equal (plist-get payload :input-tokens) 202598))
        (should (equal (plist-get payload :cached-input-tokens) 7552))
        (should (equal (plist-get payload :cache-creation-input-tokens) 4096))
        (should (equal (plist-get payload :output-tokens) 419))
        (should (equal (plist-get payload :reasoning-output-tokens) 139))
        (should (equal (plist-get payload :total-tokens) 203017))
        (should (stringp (plist-get payload :provider-request-id)))
        (should (= (plist-get payload :provider-request-ordinal) 1))))))

(ert-deftest e-harness-test-provider-telemetry-is-narrow-on-disk-and-reload ()
  "Provider lifecycle and usage cross a narrow redacted durable boundary."
  (let* ((directory (make-temp-file "e-harness-provider-telemetry-" t))
         (secret "Bearer disk-provider-secret")
         (cache-key "cache-key-disk-secret")
         (backend
          (e-backend-create
           :name "durable-provider-projection"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-error)
              (funcall on-request-start
                       (e-backend-request-create
                        :metadata
                        (list :provider secret
                              :transport 'websocket
                              :url-host "https://user:password@example.test"
                              :url-path "/responses?token=disk-path-secret"
                              :diagnostics
                              (list :model "api_key=disk-model-secret"
                                    :reasoning-effort "high"
                                    :unknown "disk-diagnostic-secret"))))
              (funcall on-item '(:type assistant-message :content "answer"))
              (funcall on-item
                       '(:type token-usage
                         :usage (:input-tokens 12 :output-tokens 3
                                 :total-tokens 15
                                 :unknown "disk-usage-secret")))
              (funcall on-item '(:type done :reason stop))
              (funcall on-done '(:status done))
              nil))))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend backend :sessions store
                   :default-options (list :prompt-cache-key cache-key))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-harness-test-prompt-batch harness "session-1" "question")
          (e-session-flush-write-queue store)
          (let ((disk
                 (with-temp-buffer
                   (insert-file-contents
                    (e-session-storage-session-reference store "session-1"))
                   (buffer-string))))
            (should (string-match-p "REDACTED" disk))
            (should-not
             (string-match-p
              "disk-provider-secret\\|password@example\\|disk-path-secret\\|disk-model-secret\\|disk-diagnostic-secret\\|disk-usage-secret\\|cache-key-disk-secret"
              disk)))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (activity (e-session-activity-events loaded "session-1"))
                 (started (seq-find
                           (lambda (event)
                             (eq (plist-get event :event-type)
                                 'provider-request-started))
                           activity))
                 (usage (seq-find
                         (lambda (event)
                           (eq (plist-get event :event-type) 'token-usage))
                         activity))
                 (started-payload (plist-get started :payload))
                 (usage-payload (plist-get usage :payload)))
            (should (equal (plist-get started-payload :provider)
                           "Bearer [REDACTED]"))
            (should-not (plist-member started-payload :request-shape))
            (should-not (plist-member
                         (plist-get started-payload :diagnostics) :unknown))
            (should (= (plist-get usage-payload :input-tokens) 12))
            (should-not (plist-member usage-payload :unknown))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-provider-diagnostics-retain-context-semantics-only ()
  "Durable provider activity keeps context semantics without observation data."
  (let* ((directory (make-temp-file "e-harness-provider-context-diagnostics-" t))
         (fingerprint "sha256:current-state-v1")
         (raw "raw-current-state-secret")
         (diagnostics
          (list :model "gpt-test"
                :reasoning-effort "high"
                :reasoning-summary "detailed"
                :observation-delivery 'request-local-replaceable
                :replaceable-current-state-present t
                :current-state-fingerprint fingerprint
                :context-rendering-strategy 'replaceable-channel
                :provider-anchor-safety 'advance-eligible
                ;; These fields intentionally model the full frontier and
                ;; request-local value.  They must never cross the activity
                ;; projection or journal boundary.
                :observation-frontier
                (list :messages (list (list :role 'system :content raw))
                      :fingerprint fingerprint)
                :replaceable-current-state
                (list (list :role 'system :content raw))
                :current-state-messages
                (list (list :role 'system :content raw))
                :current-state-content raw
                :messages (list (list :role 'system :content raw))))
         (backend
          (e-backend-create
           :name "context-diagnostics"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-error)
              (funcall on-request-start
                       (e-backend-request-create
                        :metadata
                        (list :provider 'openai
                              :transport 'http
                              :diagnostics diagnostics)))
              (funcall on-item '(:type assistant-message :content "answer"))
              (funcall on-item '(:type done :reason stop))
              (funcall on-done '(:status done))
              nil))))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create :backend backend :sessions store)))
    (unwind-protect
        (progn
          (let ((projected
                 (e-harness--provider-diagnostics-activity-projection
                  diagnostics)))
            (dolist (key '(:observation-delivery
                           :reasoning-effort
                           :reasoning-summary
                           :replaceable-current-state-present
                           :current-state-fingerprint
                           :context-rendering-strategy
                           :provider-anchor-safety))
              (should (plist-member projected key)))
            (should (equal (plist-get projected :current-state-fingerprint)
                           fingerprint))
            (should (equal (plist-get projected :reasoning-summary)
                           "detailed"))
            (should-not (plist-member projected :observation-frontier))
            (should-not (plist-member projected :replaceable-current-state))
            (should-not (plist-member projected :current-state-messages))
            (should-not (plist-member projected :current-state-content))
            (should-not (plist-member projected :messages))
            (should-not (string-match-p (regexp-quote raw)
                                        (prin1-to-string projected))))
          (e-harness-create-session harness :id "session-1")
          (e-harness-test-prompt-batch harness "session-1" "question")
          (let* ((activity (e-harness-session-activity-events
                            harness "session-1"))
                 (started
                  (seq-find
                   (lambda (event)
                     (eq (plist-get event :event-type)
                         'provider-request-started))
                   activity))
                 (projected (plist-get (plist-get started :payload)
                                       :diagnostics)))
            (should started)
            (should (equal (plist-get projected :reasoning-summary)
                           "detailed"))
            (should (eq (plist-get projected :observation-delivery)
                        'request-local-replaceable))
            (should (eq (plist-get projected :context-rendering-strategy)
                        'replaceable-channel))
            (should (eq (plist-get projected :provider-anchor-safety)
                        'advance-eligible)))
          (e-session-flush-write-queue store)
          (let* ((loaded (e-session-persistent-store-create directory))
                 (activity (e-session-activity-events loaded "session-1"))
                 (started
                  (seq-find
                   (lambda (event)
                     (eq (plist-get event :event-type)
                         'provider-request-started))
                   activity))
                 (payload (plist-get started :payload))
                 (journal
                  (with-temp-buffer
                    (insert-file-contents
                     (e-session-storage-session-reference store "session-1"))
                    (buffer-string)))
                 (projected (plist-get payload :diagnostics)))
            (should started)
            (dolist (key '(:observation-delivery
                           :reasoning-effort
                           :reasoning-summary
                           :replaceable-current-state-present
                           :current-state-fingerprint
                           :context-rendering-strategy
                           :provider-anchor-safety))
              (should (plist-member projected key)))
            ;; JSONL keeps scalar enum values as strings on replay; the
            ;; semantic value remains present and the live activity projection
            ;; above retains the original symbols used by the E2E consumers.
            (should (member (plist-get projected :observation-delivery)
                            '(request-local-replaceable
                              "request-local-replaceable")))
            (should (equal (plist-get projected
                                      :replaceable-current-state-present)
                           t))
            (should (equal (plist-get projected :current-state-fingerprint)
                           fingerprint))
            (should (equal (plist-get projected :reasoning-summary)
                           "detailed"))
            (should (member (plist-get projected :context-rendering-strategy)
                            '(replaceable-channel "replaceable-channel")))
            (should (member (plist-get projected :provider-anchor-safety)
                            '(advance-eligible "advance-eligible")))
            (dolist (key '(:observation-frontier
                           :replaceable-current-state
                           :current-state-messages
                           :current-state-content
                           :messages))
              (should-not (plist-member (plist-get payload :diagnostics)
                                        key)))
            (should-not (string-match-p (regexp-quote raw) journal))))
      (delete-directory directory t))))

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
                     :context-capabilities
                     '(:continuation linear
                       :observation-delivery request-local-replaceable)
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
      (e-harness-test-prompt-batch harness "session-1" "question")
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
                       '("static-prefix")))
        (should-not (plist-member fingerprints :current-state-fingerprint))
        (should (equal (plist-get fingerprints :active-layer-ids)
                       '("context-anchor-layer")))
        (should (equal (plist-get fingerprints :reasoning)
                       '(:reasoning nil :reasoning-effort nil :effort nil)))
        (dolist (fingerprint (plist-get fingerprints :segments))
          (should (stringp (plist-get fingerprint :id)))
          (should (stringp (plist-get fingerprint :fingerprint))))))))

(ert-deftest e-harness-test-context-curation-revision-fences-material-anchor ()
  "Curation revision fences anchors without hashing frame-local sources."
  (let* ((options
          '(:context-lifetime-enabled t
            :reserved-effect-carrier context-curate-wire
            :observation-delivery request-local-replaceable
            :observation-delivery-map request-local-replaceable
            :context-capabilities
            (:observation-delivery request-local-replaceable
             :reserved-effect-carrier context-curate-wire)))
         (context
          (lambda (value messages)
            (list :options (copy-tree options)
                  :messages messages
                  :segments
                  (list
                   '(:kind static-prefix
                     :id static
                     :fingerprint "static-fingerprint"
                     :messages ((:role system :content "stable policy")))
                   (list :kind 'current-state
                         :id 'current
                         :fingerprint "current-fingerprint"
                         :messages
                         (list (list :role 'system :content value))))
                  :provider-anchor-active-layer-ids '("layer")
                  :provider-anchor-compaction-boundary nil)))
         (first (funcall context
                         "SOURCE-ONE"
                         '((:role system :content "stable policy")
                           (:role system :content "SOURCE-ONE"))))
         (second (funcall context
                          "SOURCE-TWO"
                          '((:role system :content "stable policy")
                            (:role system :content "SOURCE-TWO"))))
         (first-fingerprints
          (e-harness--provider-anchor-fingerprints first))
         (second-fingerprints
          (e-harness--provider-anchor-fingerprints second))
         (revision (plist-get first-fingerprints
                              :context-curation-revision-identity)))
    (should revision)
    (should (equal revision
                   (e-context-lifetime-curation-revision-identity)))
    (should (equal first-fingerprints second-fingerprints))
    (should-not (string-match-p "SOURCE-ONE\|SOURCE-TWO"
                                (prin1-to-string first-fingerprints)))
    (let ((e-context-budget-estimate-bytes-per-token 2.0))
      (should-not
       (equal revision
              (plist-get
               (e-harness--provider-anchor-fingerprints first)
               :context-curation-revision-identity))))
    (let ((e-context-lifetime-curation-presentation-revision
           "context-curation-presentation-test-v2"))
      (should-not
       (equal revision
              (plist-get
               (e-harness--provider-anchor-fingerprints first)
               :context-curation-revision-identity))))
    (let* ((store (e-session-store-create))
           (session-id "curation-revision-session"))
      (e-session-create store :id session-id)
      (let* ((message (e-session-append-message
                       store session-id
                       '(:role assistant :content "answer")))
             (anchor
              (e-session-append-provider-anchor
               store session-id 'openai
               :model "gpt-test"
               :covered-entry-id (plist-get message :id)
               :fingerprints first-fingerprints)))
        (let ((e-context-budget-estimate-bytes-per-token 2.0))
          (should
           (eq (e-session-provider-anchor-incompatibility-reason
                store session-id anchor 'openai "gpt-test"
                (e-harness--provider-anchor-fingerprints first))
               'context-curation-revision-changed)))))))

(ert-deftest e-harness-test-reasoning-summary-fences-provider-identity ()
  "Effective Responses reasoning summary is an anchor identity input."
  (let* ((base-options '(:model "gpt-test"
                         :reasoning-effort "high"
                         :reasoning-summary "auto"))
         (detailed-options (plist-put (copy-sequence base-options)
                                      :reasoning-summary
                                      "detailed"))
         (base (list :options base-options :segments nil))
         (detailed (list :options detailed-options :segments nil))
         (base-fingerprints
          (e-harness--provider-anchor-fingerprints base))
         (detailed-fingerprints
          (e-harness--provider-anchor-fingerprints detailed))
         (diagnostics (list :model "gpt-test"
                            :reasoning-effort "high"
                            :reasoning-summary "detailed"))
         (projected
          (e-harness--provider-diagnostics-activity-projection diagnostics)))
    (should (equal (plist-get (plist-get base-fingerprints :reasoning)
                              :reasoning-summary)
                   "auto"))
    (should-not (equal base-fingerprints detailed-fingerprints))
    (should (equal (plist-get (plist-get detailed-fingerprints :reasoning)
                              :reasoning-summary)
                   "detailed"))
    (should (equal (plist-get projected :reasoning-summary) "detailed"))))

(ert-deftest e-harness-test-provider-anchor-persistence-keeps-latest-candidate ()
  "Provider anchor persistence keeps only the final candidate for a provider."
  (e-harness-test--with-empty-layer-registry
    (let* ((backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
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
      (e-harness-test-prompt-batch harness "session-1" "question")
      (let ((anchors (e-session-provider-anchors
                      (e-harness-sessions harness)
                      "session-1")))
        (should (= (length anchors) 1))
        (should (equal (plist-get (plist-get (car anchors) :metadata)
                                  :response-id)
                       "resp-new"))))))

(defun e-harness-test--run-final-refresh-anchor-scenario
    (kind second-candidate-p)
  "Run a real harness refresh scenario for identity KIND.
Return request options, persisted anchors, and the final context."
  (let* ((stable-content "STABLE-A")
         (current-state "STATE-A")
         (tool-version "TOOL-A")
         (request-count 0)
         (requests nil)
         (harness nil)
         (backend
          (e-backend-create
           :name (format "anchor-refresh-%s" kind)
           :context-capabilities
           '(:continuation linear
             :observation-delivery request-local-replaceable)
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages)
              (push (copy-tree options) requests)
              (cl-incf request-count)
              (if (= request-count 1)
                  (progn
                    (funcall
                     on-item
                     '(:type tool-call
                       :id "refresh-1"
                       :name "refresh-anchor"
                       :arguments (:stated_purpose "Refresh the anchor.")))
                    (funcall
                     on-item
                     '(:type provider-anchor-candidate
                       :provider-id openai
                       :metadata (:response-id "resp-A")))
                    (funcall on-item '(:type done :reason stop)))
                (funcall on-item
                         '(:type assistant-message :content "answer-B"))
                (when second-candidate-p
                  (funcall
                   on-item
                   '(:type provider-anchor-candidate
                     :provider-id openai
                     :metadata (:response-id "resp-B"))))
                (funcall on-item '(:type done :reason stop)))))))
         (stable-provider
          (e-context-provider-create
           :name 'anchor-refresh-stable
           :cache-placement 'stable-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content stable-content)))))
         (dynamic-provider
          (e-context-provider-create
           :name 'anchor-refresh-current
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (capability
          (e-capability-create
           :id 'anchor-refresh-capability
           :instructions "stable policy"
           :context-providers (list stable-provider dynamic-provider)
           :tools
           (list
            (lambda (registry)
              (e-tools-register
               registry
               :name "refresh-anchor"
               :description (format "Refresh %s" tool-version)
               :work
               (e-tools-cheap-work
                "test.anchor-refresh"
                (lambda (_arguments)
                  (pcase kind
                    ('stable
                     (setq stable-content "STABLE-B"))
                    ('current-state
                     (setq current-state "STATE-B"))
                    ('tool-schema
                     (setq tool-version "TOOL-B"))
                    ('provider-option
                     (setf (e-harness-default-options harness)
                           (plist-put
                            (copy-sequence
                             (e-harness-default-options harness))
                            :max-tokens
                            123)))
                    ('compaction
                     (let* ((store (e-harness-sessions harness))
                            (first-entry
                             (car (e-session-current-path
                                   store "session-1"))))
                       (e-session-append-compaction
                        store "session-1" "refresh summary"
                        :first-kept-entry-id
                        (plist-get first-entry :id)))))
                  (e-tools-result-create
                   (plist-get (e-tools-current-context) :tool-call)
                   'ok
                   "refreshed"
                   '(:refresh-context t)))))))))
         )
    (setq harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities (list capability)
           :default-options
           '(:model "gpt-test"
             :provider-continuation t
             :provider-anchor-provider-id openai)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "refresh")
    (list :requests (nreverse requests)
          :anchors (e-session-provider-anchors
                    (e-harness-sessions harness) "session-1")
          :context (e-harness-turn-context
                    harness "session-1" "after-refresh"))))

(ert-deftest e-harness-test-provider-anchor-persistence-follows-final-refresh ()
  "Only the candidate owned by the final refreshed request is persisted."
  (e-harness-test--with-empty-layer-registry
    (dolist (kind '(stable tool-schema provider-option compaction))
      (let* ((result
             (e-harness-test--run-final-refresh-anchor-scenario kind t))
             (requests (plist-get result :requests))
             (anchors (plist-get result :anchors))
             (context (plist-get result :context)))
        (should (= (length requests) 2))
        (should-not (plist-get (nth 1 requests) :provider-anchor))
        (should (= (length anchors) 1))
        (should (equal (plist-get (plist-get (car anchors) :metadata)
                                  :response-id)
                       "resp-B"))
        (should (equal (plist-get (car anchors) :fingerprints)
                       (e-harness--provider-anchor-fingerprints context)))))
    (let* ((result
            (e-harness-test--run-final-refresh-anchor-scenario
             'current-state t))
           (requests (plist-get result :requests))
           (anchors (plist-get result :anchors))
           (context (plist-get result :context)))
      ;; The replaceable frontier may use resp-A for this immediate follow-up,
      ;; but the final persisted owner is still the refreshed response.
      (should (equal
               (plist-get
                (plist-get (nth 1 requests) :provider-anchor)
                :metadata)
               '(:response-id "resp-A")))
      (should (= (length anchors) 1))
      (should (equal (plist-get (plist-get (car anchors) :metadata)
                                :response-id)
                     "resp-B"))
      (should-not
       (plist-member (plist-get (car anchors) :fingerprints)
                     :current-state-fingerprint))
      (should (equal (plist-get (car anchors) :fingerprints)
                     (e-harness--provider-anchor-fingerprints context))))
    (dolist (kind '(stable current-state))
      (let* ((result
              (e-harness-test--run-final-refresh-anchor-scenario kind nil))
             (requests (plist-get result :requests))
             (anchors (plist-get result :anchors)))
        (should (= (length requests) 2))
        (should (= (length anchors) 0))))))

(ert-deftest e-harness-test-provider-anchor-reversion-selects-matching-old-fingerprint ()
  "A later reversion selects an old compatible anchor, never a newer mismatch."
  (e-harness-test--with-empty-layer-registry
    (let* ((stable-content "STABLE-A")
           (request-count 0)
           (requests nil)
           (backend
            (e-backend-create
             :name "anchor-reversion"
             :context-capabilities
             '(:continuation linear
               :observation-delivery request-local-replaceable)
             :stream
             (cl-function
              (lambda (&key messages options on-item)
                (ignore messages)
                (push (copy-tree options) requests)
                (cl-incf request-count)
                (funcall on-item
                         (list :type 'assistant-message
                               :content (format "answer-%s" request-count)))
                (funcall on-item
                         (list :type 'provider-anchor-candidate
                               :provider-id 'openai
                               :metadata
                               (list :response-id
                                     (format "resp-%s" request-count))))
                (funcall on-item '(:type done :reason stop))))))
           (stable-provider
            (e-context-provider-create
             :name 'anchor-reversion-stable
             :cache-placement 'stable-context
             :build (lambda (&rest _)
                      (list (list :role 'system :content stable-content)))))
           (capability
            (e-capability-create
             :id 'anchor-reversion-capability
             :instructions "stable policy"
             :context-providers (list stable-provider)))
           (harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability)
             :default-options
             '(:model "gpt-test"
               :provider-continuation t
               :provider-anchor-provider-id openai))))
      (e-harness-create-session harness :id "session-1")
      (e-harness-test-prompt-batch harness "session-1" "one")
      (setq stable-content "STABLE-B")
      (e-harness-test-prompt-batch harness "session-1" "two")
      (setq stable-content "STABLE-A")
      (e-harness-test-prompt-batch harness "session-1" "three")
      (let* ((ordered (nreverse requests))
             (third (nth 2 ordered))
             (anchors (e-session-provider-anchors
                       (e-harness-sessions harness) "session-1")))
        (should (= (length ordered) 3))
        (should (equal
                 (plist-get (plist-get third :provider-anchor) :metadata)
                 '(:response-id "resp-1")))
        (should (= (length anchors) 3))
        (should (equal (mapcar (lambda (anchor)
                                (plist-get (plist-get anchor :metadata)
                                           :response-id))
                              anchors)
                       '("resp-1" "resp-2" "resp-3")))))))

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
    (e-harness-test-prompt-batch harness "session-1" "question")
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

(ert-deftest e-harness-test-provider-deadline-default-settles-stalled-turn ()
  "A provider request with no callbacks fails visibly through the Work deadline."
  (let* ((e-harness-provider-request-deadline-seconds 0.02)
         (cancelled nil)
         (backend
          (e-backend-create
           :name "stalled-harness-provider"
           :start
           (cl-function
            (lambda (&key on-request-start &allow-other-keys)
              (let ((request
                     (e-backend-request-create
                      :cancel (lambda ()
                                (setq cancelled t)
                                t)
                      :metadata '(:provider fake :transport timer))))
                (funcall on-request-start request)
                request)))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 1.0)))
      (should (equal (plist-get settled :status) 'error))
      (should (string-match-p "deadline" (plist-get settled :error)))
      (should (eq (plist-get (plist-get settled :error-details) :execution)
                  'backend)))
    (should cancelled)
    (let* ((types (mapcar (lambda (event) (plist-get event :type))
                          events))
           (failed (seq-find
                    (lambda (event)
                      (eq (plist-get event :type) 'turn-failed))
                    events))
           (provider-finished
            (seq-find
             (lambda (event)
               (eq (plist-get event :type) 'provider-request-finished))
             events)))
      (should (member 'provider-request-started types))
      (should (member 'provider-request-finished types))
      (should (member 'turn-failed types))
      (should (eq (plist-get (plist-get provider-finished :payload) :status)
                  'error))
      (should (eq (plist-get (plist-get (plist-get failed :payload)
                                        :details)
                             :execution)
                  'backend)))))

(ert-deftest e-harness-test-provider-timeout-retries-then-succeeds ()
  "Provider timeout errors retry with backoff instead of failing immediately."
  (let* ((e-harness-retry-initial-backoff-seconds 0.02)
         (e-harness-retry-backoff-multiplier 1.0)
         (e-harness-retry-max-backoff-seconds 0.02)
         (attempts 0)
         (events nil)
         (backend
          (e-backend-create
           :name "timeout"
           :normalize-error-details
           (lambda (_message details condition)
             (append details
                     (when (eq (car-safe condition) 'e-openai-request-timeout)
                       '(:retryable t :retry-reason timeout))))
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options)
              (cl-incf attempts)
              (funcall on-request-start
                       (e-backend-request-create
                        :metadata '(:provider codex
                                    :transport url-retrieve
                                    :url-host "example.test"
                                    :url-path "/codex/responses"
                                    :timeout-seconds 60)))
              (run-at-time
               0 nil
               (lambda ()
                 (if (= attempts 1)
                     (funcall on-error
                              '(e-openai-request-timeout
                                "request timed out"))
                   (funcall on-item
                            '(:type assistant-message :content "recovered"))
                   (funcall on-item '(:type done :reason stop))
                   (funcall on-done '(:status done)))))
              nil))))
         (harness (e-harness-create :backend backend)))
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 1.0)))
      (should (equal (plist-get settled :status) 'done))
      (should (= attempts 2)))
    (let ((types (mapcar (lambda (event) (plist-get event :type)) events)))
      (should (member 'turn-retrying types))
      (should (member 'turn-finished types))
      (should-not (member 'turn-failed types))
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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (e-harness-test-abort harness "session-1")
    (funcall (plist-get callbacks :on-item)
             '(:type assistant-message :content "late answer"))
    (funcall (plist-get callbacks :on-item)
             '(:type done :reason stop))
    (funcall (plist-get callbacks :on-done) '(:status done))
    (should (equal (plist-get (e-harness-wait-batch harness "session-1" 0.1)
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
                                 :arguments (:stated_purpose "Hold the request."
                                             :text "hi")))
                      (funcall on-item '(:type done :reason tool-use))
                      (funcall on-done '(:status done))
                      nil))))
         (tools (e-tools-registry-create))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-tools-test-register tools
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
      (e-harness--install-activity-sink harness (lambda (event) (push event events)))
      (e-harness-create-session harness :id "session-1")
      (e-harness-test-prompt-async harness "session-1" "question")
      (should tool-callbacks)
      (e-harness-test-abort harness "session-1")
      (funcall (plist-get tool-callbacks :on-done) "late result")
      (should (equal (plist-get (e-harness-wait-batch harness "session-1" 0.1)
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
    (e-harness-test-prompt-batch harness "session-1" "first")
    (e-harness-test-prompt-batch harness "session-1" "second")
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant user assistant)))))

(ert-deftest e-harness-test-board-consumption-receipt-carries-endpoint-fence ()
  "A consumed board input reports the accepted token and generation."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (token [endpoint :live 7 "store" "session"])
         events)
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session")
    (e-harness-test-prompt-batch
     harness "session" "board input"
     :metadata
     (list :input-origin 'board
           :board-delivery-id '("board" "message" "participant")
           :board-id "board" :board-participant-id "participant"
           :board-endpoint-token token :board-endpoint-generation '(3 7)))
    (let* ((event (cl-find-if
                   (lambda (candidate)
                     (eq (e-events-type candidate) 'input-consumed))
                   events))
           (payload (plist-get event :payload)))
      (should event)
      (should (equal (plist-get payload :endpoint-token) token))
      (should-not (eq (plist-get payload :endpoint-token) token))
      (should (equal (plist-get payload :endpoint-generation) '(3 7))))))

(ert-deftest e-harness-test-board-endpoint-token-is-not-message-metadata ()
  "A live endpoint fence is excluded from durable transcript metadata."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (token [endpoint :live 7 "store" "session"]))
    (e-harness-create-session harness :id "session")
    (e-harness-test-prompt-batch
     harness "session" "board input"
     :metadata (list :input-origin 'board :board-endpoint-token token))
    (let ((metadata (plist-get (car (e-harness-messages harness "session"))
                               :metadata)))
      (should (eq (plist-get metadata :input-origin) 'board))
      (should-not (plist-member metadata :board-endpoint-token)))))

(ert-deftest e-harness-test-discards-only-fenced-queued-board-head ()
  "Queued board discard preserves FIFO and acknowledges the exact endpoint."
  (let* ((harness (e-harness-create))
         (token [endpoint :live 7 "store" "session"])
         (delivery-id '("board" "message" "participant"))
         events)
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session")
    (puthash "session"
             (list :id "settling" :status 'done
                   :endpoint-token token)
             (e-harness-active-turns harness))
    (e-harness-test-request-follow-up
     harness "session" "queued"
     :metadata
     (list :input-origin 'board :board-delivery-id delivery-id
           :board-endpoint-token token :board-endpoint-generation '(3 7)))
    (should-not
     (e-harness-discard-queued-board-input
      harness "session" delivery-id token '(3 8) 'participant-removed))
    (should (= (length (e-harness-queued-prompts harness "session")) 1))
    (should
     (e-harness-discard-queued-board-input
      harness "session" delivery-id token '(3 7) 'participant-removed))
    (should-not (e-harness-queued-prompts harness "session"))
    (let* ((event (cl-find-if
                   (lambda (candidate)
                     (eq (e-events-type candidate) 'input-discarded))
                   events))
           (payload (plist-get event :payload)))
      (should event)
      (should (equal (plist-get payload :delivery-id) delivery-id))
      (should (equal (plist-get payload :endpoint-token) token))
      (should-not (eq (plist-get payload :endpoint-token) token))
      (should (eq (plist-get payload :reason) 'participant-removed)))))

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
           :backend
           (e-backend-fake-create
            :items nil
            :context-capabilities
            '(:continuation linear
              :observation-delivery request-local-replaceable))
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
                               (plist-get options
                                          :replaceable-current-state))
                       '("dynamic context")))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               delta)
                       '("new prompt")))
        (should (equal (plist-get options
                                  :provider-anchor-source-message-count)
                       (length (plist-get context :messages))))))))

(ert-deftest e-harness-test-provider-anchor-delta-lifetime-filter-is-opt-in ()
  "Lifetime anchor deltas drop tool bundles without changing legacy deltas."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         (session-id "anchor-delta-lifetime-filter"))
    (e-harness-create-session harness :id session-id)
    (e-session-append-message store session-id
                              '(:role user :content "before-anchor"))
    (let* ((anchor-message
            (e-session-append-message store session-id
                                      '(:role assistant
                                        :content "anchor-response")))
           (tool-call
            (e-session-append-message
             store session-id
             '(:role tool-call
               :content (:id "anchor-call" :name "inspect"
                         :arguments (:target "one"))
               :metadata (:provider-replay-items
                          ((:id "anchor-replay"))))))
           (tool-result
            (e-session-append-message
             store session-id
             '(:role tool
               :content (:tool-call-id "anchor-call"
                         :content "anchor-result"))))
           (_tail
            (e-session-append-message store session-id
                                      '(:role user :content "after-anchor")))
           (anchor (list :covered-entry-id
                         (plist-get anchor-message :id)))
           (legacy
            (e-harness--provider-anchor-delta-messages
             harness session-id anchor nil))
           (lifetime
            (e-harness--provider-anchor-delta-messages
             harness session-id anchor
             '(:context-lifetime-enabled t))))
      (should (seq-some
               (lambda (message)
                 (equal (plist-get (plist-get message :content) :id)
                        (plist-get (plist-get tool-call :content) :id)))
               legacy))
      (should (seq-some
               (lambda (message)
                 (equal (plist-get (plist-get message :content)
                                   :tool-call-id)
                        (plist-get (plist-get tool-result :content)
                                   :tool-call-id)))
               legacy))
      (should-not
       (seq-some
        (lambda (message)
          (member (plist-get message :role) '(tool-call tool)))
        lifetime))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :content))
                             lifetime)
                     '("after-anchor"))))))

(ert-deftest e-harness-test-context-uses-provider-anchor-after-dynamic-state-change ()
  "Replaceable current-state changes preserve the stable provider anchor."
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
           :backend
           (e-backend-fake-create
            :items nil
            :context-capabilities
            '(:continuation linear
              :observation-delivery request-local-replaceable))
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
           (anchor-identity
            (plist-get (plist-get anchor-context :options)
                       :continuation-projection-identity)))
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
      (let* ((context (e-harness-context harness "session-1" "turn-2"))
             (options (plist-get context :options))
             (identity (plist-get options
                                 :continuation-projection-identity)))
        (should (eq (plist-get options :observation-delivery)
                    'request-local-replaceable))
        (should (equal identity anchor-identity))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               (plist-get options
                                          :replaceable-current-state))
                       '("changed dynamic context")))
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
                       '("new prompt")))))))

(ert-deftest e-harness-test-context-holds-linear-anchor-for-inherited-state ()
  "Linear continuation reconstructs statelessly around inherited current state."
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
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
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
      (setq current-state "changed dynamic context")
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let* ((options (plist-get
                       (e-harness-context harness "session-1" "turn-2")
                       :options)))
        (should-not (plist-get options :provider-anchor))
        (should (eq (plist-get options :observation-delivery) 'inherited))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'inherited-observation-requires-branchable))
        (should (eq (plist-get options :context-rendering-strategy)
                    'stateless))
        (should (eq (plist-get options :provider-anchor-safety)
                    'hold-inherited-observation))))))

(ert-deftest e-harness-test-branchable-inherited-state-uses-clean-anchor ()
  "Branchable inherited state reuses only a clean anchor and stays unadvanced."
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
           :backend (e-backend-fake-create
                     :context-capabilities
                     '(:continuation branchable)
                     :items nil)
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
           (anchor-context (e-harness-context harness "session-1" "turn-1"))
           (clean-fingerprints
            (plist-put
             (copy-sequence
              (e-harness--provider-anchor-fingerprints anchor-context))
             :current-state-fingerprint
             nil)))
      ;; This is the clean response id from before current state was observed.
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints clean-fingerprints
       :metadata '(:response-id "resp-clean"))
      ;; A contaminated anchor must not win merely because it is newer.
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-contaminated"))
      (setq current-state "changed dynamic context")
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let* ((context (e-harness-context harness "session-1" "turn-2"))
             (options (plist-get context :options))
             (anchor (plist-get options :provider-anchor))
             (delta (plist-get options :provider-anchor-delta-messages)))
        (should (equal (plist-get (plist-get anchor :metadata) :response-id)
                       "resp-clean"))
        (should (eq (plist-get options :provider-anchor-safety)
                    'branchable-clean-anchor-only))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               delta)
                       '("changed dynamic context" "new prompt")))))))

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
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
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
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
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
            (e-tools-test-register
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
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
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
             :backend (e-backend-fake-create
                       :context-capabilities '(:continuation linear)
                       :items nil)
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
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
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
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
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
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
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
                          (e-tools-test-register
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
                          (e-tools-test-register
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
                    (e-tools-execute-batch
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
                               :arguments (:stated_purpose "Echo the text."
                                           :text "raw")))
                    (funcall on-item '(:type done :reason tool-use)))
                (funcall on-item
                         '(:type assistant-message :content "done"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools-capability
          (e-capability-create
           :id 'echo-tool
           :tools
           (list (lambda (registry)
                   (e-tools-test-register
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
                        (let ((prepared (copy-sequence tool-call)))
                          (plist-put
                           prepared
                           :arguments
                           (list :stated_purpose
                                 (plist-get (plist-get tool-call :arguments)
                                            :stated_purpose)
                                 :text "prepared")))))
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
    (e-harness-test-prompt-batch harness "session-1" "use tool")
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
                   (e-tools-test-register
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
              (e-tools-test-register
               registry
               :name "outer"
               :description "Call inner."
               :handler (lambda (_arguments)
                          (e-tools-call
                           "inner" '(:text "raw")
                           '(:metadata (:purpose "chain-test")))))
              (e-tools-test-register
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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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
                                   :arguments (list :stated_purpose
                                                     "Run the requested code."
                                                     :code code)))
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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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
                                   :arguments (list :stated_purpose
                                                     "Run the requested code."
                                                     :code code)))
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
              (e-tools-test-register
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
    (e-harness-test-prompt-batch harness "session-1" "catch nested error")
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
                   (e-tools-test-register
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
          (e-harness-test-prompt-batch harness "session-1" "use tool")
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
          (let ((result (e-tools-execute-batch
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
                    (e-tools-execute-batch
                     tools
                     '(:id "call-1"
                       :name "read"
                       :arguments (:uri "test://value"
                                   :range (:unit "line" :start 1 :end 2))))
                    :content)
                   "read-result"))
    (should (equal (plist-get
                    (e-tools-execute-batch
                     tools
                     '(:id "call-2"
                       :name "write"
                       :arguments (:uri "test://value" :content "content")))
                    :content)
                   "write-result"))
    (should (equal (plist-get
                    (e-tools-execute-batch
                     tools
                     '(:id "call-3"
                       :name "edit"
                       :arguments (:uri "test://value"
                                   :edits ((:oldText "a" :newText "b")))))
                    :content)
                   "edit-result"))
    (should (equal (plist-get
                    (e-tools-execute-batch
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
                    (e-tools-execute-batch
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

(ert-deftest e-harness-test-discovery-tool-descriptions-stay-lean ()
  "Discovery tools omit active scheme details from their prompt schemas."
  (let* ((capability
          (e-capability-create
           :id 'discovery-description-capability
           :resource-methods
           (list (lambda (registry)
                   (dolist (operation (list e-operation-glob
                                            e-operation-search))
                     (e-resources-register
                      registry
                      (e-resource-method-create
                       :scheme "described"
                       :operation operation
                       :description "Verbose scheme detail."
                       :uri-patterns '("described://<id>")
                       :handler #'ignore)))))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (list capability))))
    (dolist (name '("glob" "search"))
      (let* ((tool (seq-find (lambda (definition)
                               (equal (plist-get definition :name) name))
                             (e-tools-definitions (e-harness-tools harness))))
             (description (plist-get tool :description)))
        (should tool)
        (should-not (string-match-p "described://" description))
        (should-not (string-match-p "Verbose scheme detail" description))))))

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
    (let ((result (e-tools-execute-batch (e-harness-tools harness) read-call)))
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
    (let ((result (e-tools-execute-batch (e-harness-tools harness) read-call)))
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
             (e-tools-execute-batch
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
    (let* ((content
            (plist-get
             (e-tools-execute-batch
              (e-harness-tools harness)
              '(:id "call-2"
                :name "search"
                :arguments (:uri "e://reference-capability"
                            :query "needle"
                            :glob "refs/*"
                            :limit 5)))
             :content))
           (match (aref (plist-get content :matches) 0)))
      (should-not (plist-get content :truncated))
      (should (equal (plist-get match :uri)
                     "e://reference-capability/refs/guide.md"))
      (should (= (plist-get match :line) 1))
      (should (= (plist-get match :column) 17))
      (should (equal (plist-get match :text)
                     "Reference guide needle")))))

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
                          (e-tools-test-register
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
    (e-harness-test-prompt-batch harness "session-1" "raw prompt")
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
                          (e-tools-test-register
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
    (e-harness-test-prompt-batch harness "session-1" "raw prompt")
    (should (equal (plist-get captured-options :model) "session-model"))
    (should (equal (plist-get captured-options :reasoning-effort) "high"))
    (should (plist-get captured-options :tools))))

(ert-deftest e-harness-test-session-option-changes-emit-events ()
  "Changing session options emits a core event."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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

(ert-deftest e-harness-test-derived-prompt-cache-key-fits-provider-limit ()
  "Derived prompt cache keys preserve identity within the provider limit."
  (e-harness-test--with-empty-layer-registry
    (let* ((harness
            (e-harness-create
             :backend (e-backend-fake-create :items nil)
             :default-options '(:model "gpt-5.6"
                                :prompt-cache-default t)))
           (session-id "session-1")
           (other-root-session-id "session-2"))
      (e-harness-create-session
       harness
       :id session-id
       :metadata '(:project-root "/tmp/cache-project"))
      (e-harness-create-session
       harness
       :id other-root-session-id
       :metadata '(:project-root "/tmp/other-cache-project"))
      (let* ((options (e-harness-turn-options harness session-id))
             (key (plist-get options :prompt-cache-key))
             (other-root-key
              (plist-get
               (e-harness-turn-options harness other-root-session-id)
               :prompt-cache-key))
             (other-model-key
              (e-harness--derived-prompt-cache-key
               harness session-id
               (plist-put (copy-sequence options) :model "gpt-5.7")))
             (other-tools-key
              (e-harness--derived-prompt-cache-key
               harness session-id
               (plist-put (copy-sequence options)
                          :tools
                          '((:name "different-tool"))))))
        (should (string-match-p "\\`e:pcctx[0-9]+:[[:xdigit:]]+\\'" key))
        (should (= (length key) e-harness-prompt-cache-key-max-length))
        (should (equal key
                       (e-harness--derived-prompt-cache-key
                        harness session-id options)))
        (should-not (equal key other-root-key))
        (should-not (equal key other-model-key))
        (should-not (equal key other-tools-key))))))

(ert-deftest e-harness-test-derived-prompt-cache-key-canonicalizes-tool-schemas ()
  "Equivalent nested hash schemas share a key; material changes do not."
  (e-harness-test--with-empty-layer-registry
    (let* ((properties-a (make-hash-table :test 'equal))
           (properties-b (make-hash-table :test 'equal))
           (properties-c (make-hash-table :test 'equal))
           (path-schema '(:type "string" :minLength 1))
           (other-schema '(:type "string" :maxLength 40)))
      (puthash "path" path-schema properties-a)
      (puthash "other" other-schema properties-a)
      ;; Insert the equivalent hash object in the opposite order.
      (puthash "other" (copy-tree other-schema) properties-b)
      (puthash "path" (copy-tree path-schema) properties-b)
      (puthash "path" '(:type "number") properties-c)
      (puthash "other" (copy-tree other-schema) properties-c)
      (let* ((definition-a
              (list :type "function" :name "read" :description "Read."
                    :parameters (list :type "object"
                                      :properties properties-a
                                      :required ["path"]
                                      :additionalProperties :json-false)
                    :strict :json-false))
             (definition-b
              (list :strict :json-false :parameters
                    (list :additionalProperties :json-false
                          :required ["path"] :properties properties-b
                          :type "object")
                    :description "Read." :name "read" :type "function"))
             (definition-c
              (list :type "function" :name "read" :description "Read."
                    :parameters (list :type "object"
                                      :properties properties-c
                                      :required ["path"]
                                      :additionalProperties :json-false)
                    :strict :json-false))
             (options-a (list :model "gpt-test" :tools (list definition-a)))
             (options-b (list :model "gpt-test" :tools (list definition-b)))
             (options-c (list :model "gpt-test" :tools (list definition-c)))
             (key-a (e-harness--derived-prompt-cache-key
                     (e-harness-create :backend (e-backend-fake-create :items nil))
                     "session-1" options-a))
             (key-b (e-harness--derived-prompt-cache-key
                     (e-harness-create :backend (e-backend-fake-create :items nil))
                     "session-1" options-b))
             (key-c (e-harness--derived-prompt-cache-key
                     (e-harness-create :backend (e-backend-fake-create :items nil))
                     "session-1" options-c)))
        (should (equal (e-tools-definition-fingerprint definition-a)
                       (e-tools-definition-fingerprint definition-b)))
        (should (equal key-a key-b))
        (should-not (equal key-a key-c))))))

(ert-deftest e-harness-test-provider-diagnostics-retain-websocket-lifecycle ()
  "Durable provider diagnostics retain bounded WebSocket lifecycle state."
  (let ((projected
         (e-harness--provider-diagnostics-activity-projection
          '(:provider-continuation full
            :previous-response-id-present nil
            :websocket-connection-id "e-ws-7"
            :websocket-reused t
            :websocket-reuse-count 3
            :websocket-request-mode full
            :websocket-idle-close-seconds 600))))
    (should (equal (plist-get projected :websocket-connection-id) "e-ws-7"))
    (should (eq (plist-get projected :websocket-reused) t))
    (should (= (plist-get projected :websocket-reuse-count) 3))
    (should (eq (plist-get projected :websocket-request-mode) 'full))
    (should (= (plist-get projected :websocket-idle-close-seconds)
               600))))

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
    (e-harness-test-prompt-batch harness "session-1" "hello")
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
    (cl-letf (((symbol-function 'e-session-storage-publish-projections)
               (lambda (&rest _)
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
      (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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

(ert-deftest e-harness-test-workspace-roots-default-to-primary-only ()
  "Without configured extras, workspace roots are just the primary root."
  (let* ((primary (file-name-as-directory (make-temp-file "e-ws-primary-" t)))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-default-projects nil)
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
         (e-default-projects nil)
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
         (e-default-projects nil)
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

(ert-deftest e-harness-test-default-project-roots-normalize-and-deduplicate ()
  "Default project roots preserve configured order after normalization."
  (let* ((first (file-name-as-directory (make-temp-file "e-default-first-" t)))
         (second (file-name-as-directory (make-temp-file "e-default-second-" t)))
         (e-default-projects
          (list (directory-file-name first) "" first second nil)))
    (unwind-protect
        (should (equal (e-default-project-roots) (list first second)))
      (delete-directory first t)
      (delete-directory second t))))

(ert-deftest e-harness-test-workspace-roots-include-default-projects-last ()
  "Specific extras precede global defaults without duplicating the primary."
  (let* ((primary (file-name-as-directory (make-temp-file "e-ws-primary-" t)))
         (specific (file-name-as-directory (make-temp-file "e-ws-specific-" t)))
         (default (file-name-as-directory (make-temp-file "e-ws-default-" t)))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-default-projects (list default specific primary default))
         (e-workspace-roots-alist (list (cons primary (list specific)))))
    (unwind-protect
        (progn
          (e-harness-create-session
           harness :id "s1" :metadata (list :project-root primary))
          (should (equal (e-harness-workspace-roots harness "s1")
                         (list primary specific default))))
      (delete-directory primary t)
      (delete-directory specific t)
      (delete-directory default t))))

(ert-deftest e-harness-test-context-lifetime-tool-bundle-curates-and-forgets ()
  "A normal opted-in tool turn exposes a paired bundle once and keeps its curation."
  (e-harness-test--with-empty-layer-registry
    (let* ((request-count 0)
           (requests nil)
           (curation-input nil)
           (backend
            (e-backend-create
             :name "context-lifetime-tool"
             :context-capabilities
             '(:continuation none
               :observation-delivery request-local-replaceable
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key messages options on-item)
                (push (list :messages (copy-tree messages)
                            :options (copy-tree options))
                      requests)
                (setq request-count (1+ request-count))
                (if (= request-count 1)
                    (progn
                      ;; The ordinary call remains an ordinary streamed tool
                      ;; item; the reserved effect is carried beside it.
                      (funcall
                       on-item
                       '(:type tool-call
                         :id "call-context-lifetime"
                         :name "remember-fact"
                         :arguments (:value "canvas marker")))
                      (setq curation-input
                            (list :type 'context-curate
                                  :arguments
                                  '(:keep nil
                                    :summaries
                                    ((:sources (1)
                                      :text "selected durable fact")))))
                      (funcall on-item curation-input)
                      (funcall on-item '(:type done :reason tool-use)))
                  (funcall on-item
                           '(:type assistant-message
                             :content "follow-up answer"))
                  (funcall on-item '(:type done :reason stop)))))))
           (provider
            (e-context-provider-create
             :name 'context-lifetime-canvas
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      '((:role system
                         :content "CANVAS-OBSERVATION")))))
           (capability
            (e-capability-create
             :id 'context-lifetime-tool-capability
             :context-providers (list provider)
             :tools
             (list
              (lambda (registry)
                (e-tools-test-register
                 registry
                 :name "remember-fact"
                 :description "Return the selected marker."
                 :handler (lambda (_arguments)
                            "BULKY-TOOL-RESULT"))))))
           (harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability))))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-harness-create-session harness :id "session-1")
        (let ((probe (e-harness-turn-context harness "session-1" "probe")))
          (should (e-context-lifetime-frame-p
                   (plist-get probe :lifetime-frame))))
        (e-harness-test-prompt-batch harness "session-1" "remember this"))
      (should (= request-count 2))
      (should curation-input)
      (let* ((ordered-requests (reverse requests))
             (first-request (car ordered-requests))
             (second-request (cadr ordered-requests))
             (first-messages (plist-get first-request :messages))
             (first-options (plist-get first-request :options))
             (second-messages (plist-get second-request :messages))
             (roles (mapcar (lambda (message) (plist-get message :role))
                            second-messages)))
        (should (plist-get first-options :context-lifetime-enabled))
        (should (equal (mapcar (lambda (message) (plist-get message :role))
                               first-messages)
                       '(user system system)))
        (should (equal (plist-get (nth 1 first-messages) :content)
                       "[ephemeral context source 1, ~5 tokens]"))
        (should (equal (plist-get (nth 2 first-messages) :content)
                       "CANVAS-OBSERVATION"))
        (should (= (cl-count "CANVAS-OBSERVATION"
                             (mapcar (lambda (message)
                                       (plist-get message :content))
                                     first-messages)
                             :test #'equal)
                   1))
        (should (member 'tool-call roles))
        (should (member 'tool roles))
        (should (equal
                 (plist-get
                  (plist-get
                   (seq-find
                    (lambda (message)
                      (eq (plist-get message :role) 'tool))
                    second-messages)
                   :content)
                  :tool-call-id)
                 "call-context-lifetime")))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (let* ((store (e-harness-sessions harness))
               (projection (e-session-context-lifetime-projection
                            store "session-1"))
               (curations (plist-get projection :curations))
               (context (e-harness-turn-context
                         harness "session-1" "next-consumer"))
               (messages (plist-get context :messages))
               (printed (prin1-to-string messages))
               (next-frame (plist-get context :lifetime-frame))
               (next-roles
                (mapcar (lambda (message) (plist-get message :role))
                        messages)))
          (should (= (length curations) 1))
          (let ((item (car (plist-get (car curations) :items))))
            (should (eq (plist-get item :kind) 'summary))
            (should (equal (plist-get item :text)
                           "selected durable fact")))
          (should
           (equal (plist-get projection :promotion-messages)
                  '((:role system :content "selected durable fact"))))
          (should (string-match-p "selected durable fact" printed))
          (should-not (string-match-p "BULKY-TOOL-RESULT" printed))
          (should-not (member 'tool-call next-roles))
          (should-not (member 'tool next-roles))
          (should (e-context-lifetime-frame-p next-frame))
          (should (seq-find
                   (lambda (message)
                     (equal (plist-get message :content)
                            "selected durable fact"))
                   messages))
        (should-not (string-match-p
                       "frame:consumer\|observation:"
                       printed)))))))

(ert-deftest e-harness-test-context-lifetime-presents-multiple-sources-at-late-frontier ()
  "Multiple live sources are marked once behind the stable canonical prefix."
  (e-harness-test--with-empty-layer-registry
    (let* ((requests nil)
           (source-one "FIRST-LATE-SOURCE")
           (source-two '(:kind "structured" :value 42 :items (alpha beta)))
           (backend
            (e-backend-create
             :name "context-lifetime-presentation"
             :context-capabilities
             '(:continuation none
               :observation-delivery inherited
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key messages options on-item &allow-other-keys)
                (push (list :messages (copy-tree messages)
                            :options (copy-tree options))
                      requests)
                (funcall on-item
                         '(:type assistant-message :content "answer"))
                (funcall on-item '(:type done :reason stop))))))
           (stable-provider
            (e-context-provider-create
             :name 'stable-guidance
             :cache-placement 'stable-context
             :build (lambda (&rest _)
                      '((:role system :content "STABLE-GUIDANCE")))))
           (dynamic-provider
            (e-context-provider-create
             :name 'late-sources
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      (list (list :role 'system :content source-one)
                            (list :role 'system :content source-two)))))
           (capability
            (e-capability-create
             :id 'context-lifetime-presentation-capability
             :instructions "STATIC-POLICY"
             :context-providers (list stable-provider dynamic-provider)))
           (harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability))))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-harness-create-session harness :id "session-1")
        (e-harness-test-prompt-batch harness "session-1" "durable prompt"))
      (let* ((request (car (last requests)))
             (messages (plist-get request :messages))
             (contents (mapcar (lambda (message)
                                 (plist-get message :content))
                               messages))
             (printed (prin1-to-string messages))
             (first-source-index
              (cl-position source-one contents :test #'equal))
             (second-source-index
              (cl-position source-two contents :test #'equal)))
        (should (equal contents
                       (list "STATIC-POLICY"
                             "STABLE-GUIDANCE"
                             "durable prompt"
                             "[ephemeral context source 1, ~5 tokens]"
                             source-one
                             "[ephemeral context source 2, ~14 tokens]"
                             source-two)))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :role))
                               messages)
                       '(system system user system system system system)))
        (should (< (cl-position "durable prompt" contents :test #'equal)
                   (cl-position "[ephemeral context source 1, ~5 tokens]" contents :test #'equal)))
        (should (< (cl-position "[ephemeral context source 1, ~5 tokens]" contents :test #'equal)
                   first-source-index))
        (should (< first-source-index
                   (cl-position "[ephemeral context source 2, ~14 tokens]" contents :test #'equal)))
        (should (< (cl-position "[ephemeral context source 2, ~14 tokens]" contents :test #'equal)
                   second-source-index))
        (should (= (cl-count source-one contents :test #'equal) 1))
        (should (= (cl-count source-two contents :test #'equal) 1))
        (should-not (string-match-p
                     "frame\|generation\|observation\|backing\|replay"
                     printed))))))

(ert-deftest e-harness-test-context-lifetime-preserves-observation-segment-ownership ()
  "Late markers preserve kind-scoped segment ownership and transport shape."
  (let* ((static-message '(:role system :content "STATIC-POLICY"))
         (current-message '(:role system :content "CURRENT-SOURCE"))
         (tool-message
          '(:role tool
            :content (:tool-call-id "call-1"
                      :name "lookup"
                      :status ok
                      :content "TOOL-SOURCE")))
         (dynamic-value '(:dynamic "DYNAMIC-SOURCE"))
         (dynamic-message (list :role 'system :content dynamic-value))
         (segments
          (list
           (list :kind 'static-prefix :id 'static-segment
                 :fingerprint "static-fingerprint"
                 :messages (list static-message))
           (list :kind 'current-state :id 'current-segment
                 :fingerprint "current-fingerprint"
                 :messages (list current-message))
           (list :kind 'history :id 'history-segment
                 :fingerprint "history-fingerprint"
                 :messages nil)
           (list :kind 'tool-result :id 'tool-segment
                 :fingerprint "tool-fingerprint"
                 :messages (list tool-message))
           (list :kind 'dynamic-context :id 'dynamic-segment
                 :fingerprint "dynamic-fingerprint"
                 :messages (list dynamic-message))))
         (context
          (list :messages
                (append (list static-message current-message)
                        (list tool-message dynamic-message))
                :segments segments))
         (backend
          (e-backend-create
           :name "context-lifetime-segment-ownership"
           :context-capabilities
           '(:observation-delivery request-local-replaceable)))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (let* ((capabilities '(:observation-delivery request-local-replaceable))
           (projected
            (e-harness--context-lifetime-apply-projection
             harness "session-1" "turn-1" context capabilities))
           (projected
            (e-harness--context-observation-frontier
             projected capabilities))
           (segments (plist-get projected :segments))
           (frame (plist-get projected :lifetime-frame))
           (presentation
            (e-context-lifetime-frame-curation-presentation frame))
           (markers (mapcar (lambda (source)
                              (plist-get source :marker))
                            presentation))
           (messages (plist-get projected :messages))
           (contents (mapcar (lambda (message)
                               (plist-get message :content))
                             messages))
           (options (plist-get projected :options))
           (replaceable (plist-get options :replaceable-current-state))
           (printed (prin1-to-string messages)))
      (should (equal (mapcar (lambda (segment) (plist-get segment :kind))
                             segments)
                     '(static-prefix history current-state tool-result
                       dynamic-context)))
      (dolist (spec '((static-prefix static-segment "static-fingerprint")
                      (history history-segment "history-fingerprint")
                      (current-state current-segment "current-fingerprint")
                      (tool-result tool-segment "tool-fingerprint")
                      (dynamic-context dynamic-segment "dynamic-fingerprint")))
        (let ((segment (seq-find
                        (lambda (candidate)
                          (eq (plist-get candidate :kind) (car spec)))
                        segments)))
          (should (equal (plist-get segment :id) (nth 1 spec)))
          (should (equal (plist-get segment :fingerprint)
                         (nth 2 spec)))))
      (should (equal contents
                     (list "STATIC-POLICY"
                           (nth 0 markers) "CURRENT-SOURCE"
                           (nth 1 markers) (plist-get tool-message :content)
                           (nth 2 markers) dynamic-value)))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
                             replaceable)
                     (list (nth 0 markers) "CURRENT-SOURCE"
                           (nth 2 markers) dynamic-value)))
      (should (eq (e-backend-observation-delivery-for-kind
                   capabilities 'current-state)
                  'request-local-replaceable))
      (should (eq (e-backend-observation-delivery-for-kind
                   capabilities 'tool-result)
                  'inherited))
      (should (equal (cadr
                      (plist-get
                       (seq-find
                        (lambda (segment)
                          (eq (plist-get segment :kind) 'tool-result))
                        segments)
                       :messages))
                     tool-message))
      (should (= (cl-count "CURRENT-SOURCE" contents :test #'equal) 1))
      (should (= (cl-count dynamic-value contents :test #'equal) 1))
      (should-not (string-match-p
                   "frame:|generation:|observation:|fingerprint:|backing|replay"
                   printed)))))

(ert-deftest e-harness-test-context-lifetime-tool-marker-follows-all-prior-sources ()
  "A descendant tool source follows every prior presented source."
  (let* ((prior-observations
          (list
           (list :observation-id "observation:prior-one"
                 :kind "dynamic-context"
                 :source-entry-ref "context-source:prior-one"
                 :source-fingerprint "fingerprint:prior-one"
                 :effective-delivery "inherited"
                 :body "PRIOR-SOURCE-ONE")
           (list :observation-id "observation:prior-two"
                 :kind "current-state"
                 :source-entry-ref "context-source:prior-two"
                 :source-fingerprint "fingerprint:prior-two"
                 :effective-delivery "inherited"
                 :body "PRIOR-SOURCE-TWO")))
         (previous-frame
          (e-context-lifetime-frame-create
           :id "frame:prior"
           :generation-id "generation:prior"
           :consumer-request-id "consumer:prior"
           :observations prior-observations))
         (tool-message
          '(:role tool
            :content (:tool-call-id "call-descendant"
                      :name "inspect"
                      :status ok
                      :content "DESCENDANT-TOOL-SOURCE")))
         (frame
          (e-context-lifetime-frame-create
           :id "frame:descendant"
           :generation-id "generation:prior"
           :consumer-request-id "consumer:descendant"
           :observations
           (append prior-observations
                   (list
                    (list :observation-id "observation:descendant"
                          :kind "tool-result"
                          :source-entry-ref "context-source:tool"
                          :source-fingerprint "fingerprint:tool"
                          :effective-delivery "inherited"
                          :body tool-message)))))
         (messages (list (list :role 'system :content "STABLE") tool-message))
         (projection
          (e-harness--context-lifetime-present-tool-observation
           (list :frame frame
                 :previous-frame previous-frame
                 :message tool-message
                 :turn-messages messages
                 :provider-followup-messages (copy-tree messages))))
         (turn-messages (plist-get projection :turn-messages))
         (followup-messages (plist-get projection
                                      :provider-followup-messages)))
    (should (equal
             (mapcar (lambda (source) (plist-get source :label))
                     (e-context-lifetime-frame-curation-presentation frame))
             '(1 2 3)))
    (dolist (projected (list turn-messages followup-messages))
      (let ((contents (mapcar (lambda (message)
                                (plist-get message :content))
                              projected)))
        (should (equal (car contents) "STABLE"))
        (should (string-match-p "\\[ephemeral context source 3, ~[0-9]+ tokens\\]"
                                (cadr contents)))
        (should (equal
                 (plist-get (plist-get (caddr projected) :content) :content)
                 "DESCENDANT-TOOL-SOURCE"))
        (should (= (cl-count (caddr projected) projected :test #'equal)
                   1))))))

(ert-deftest e-harness-test-context-lifetime-disabled-keeps-default-path ()
  "The opt-in boundary leaves the existing path without a runtime frame."
  "The opt-in boundary leaves the existing path without a runtime frame."
  (let* ((captured-options nil)
         (backend
          (e-backend-create
           :name "context-lifetime-disabled"
           :stream
           (cl-function
            (lambda (&key messages options on-item &allow-other-keys)
              (ignore messages)
              (setq captured-options (copy-tree options))
              (funcall on-item
                       '(:type assistant-message :content "ordinary answer"))
              (funcall on-item '(:type done :reason stop))))))
         (harness (e-harness-create :backend backend)))
    (let ((e-context-lifetime-shadow-projection-enabled nil))
      (e-harness-create-session harness :id "session-1")
      (e-harness-test-prompt-batch harness "session-1" "ordinary"))
    (should-not (plist-get captured-options :context-lifetime-enabled))
    (should-not (e-session-context-generations
                 (e-harness-sessions harness) "session-1"))
    (should-not (e-session-context-promotions
                 (e-harness-sessions harness) "session-1"))))

(ert-deftest e-harness-test-context-lifetime-tool-result-curates-on-follow-up ()
  "A tool result is observed by B, curated there, then forgotten afterward."
  (e-harness-test--with-empty-layer-registry
    (let* ((directory (make-temp-file "e-harness-reserved-curation-" t))
           (store (e-session-persistent-store-create directory))
           (request-count 0)
           (requests nil)
           (curation-input nil)
           (events nil)
           (consumed-frames nil)
           (raw-result-marker "UNIQUE-RAW-TOOL-RESULT")
           (raw-result
            `(:capability "web.fetch"
              :headers ((:name "server" :value "nginx")
                        (:name "content-type" :value "text/html"))
              :text ,raw-result-marker))
           (backend
            (e-backend-create
             :name "context-lifetime-tool-result"
             :context-capabilities
             '(:continuation none
               :observation-delivery request-local-replaceable
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key messages options on-item)
                (push (list :messages (copy-tree messages)
                            :options (copy-tree options))
                      requests)
                (setq request-count (1+ request-count))
                (pcase request-count
                  (1
                   ;; Response A only requests the ordinary tool.  The
                   ;; curation is deliberately withheld until B sees the
                   ;; paired call/result bundle.
                   (funcall
                    on-item
                    '(:type tool-call
                      :id "call-tool-result"
                      :name "inspect-result"
                      :arguments (:stated_purpose "Inspect the raw result."
                                  :target "raw")))
                   (funcall on-item '(:type done :reason tool-use)))
                  (2
                   (setq curation-input
                         (list :type 'context-curate
                               :arguments
                               '(:keep nil
                                 :summaries
                                 ((:sources (2)
                                   :text "selected from tool result")))
                               :provider-replay-items
                               '((:type provider-replay-item
                                 :provider-id openai
                                 :item (:type "function_call"
                                        :call_id "curation-call"
                                        :name "context-curate"
                                        :arguments "{}"))
                                (:type provider-replay-item
                                 :provider-id openai
                                 :item (:type "function_call_output"
                                        :call_id "curation-call"
                                        :output "")))))
                   (funcall on-item curation-input)
                   (funcall on-item '(:type done :reason stop)))
                  (3
                   (funcall on-item
                            '(:type assistant-message
                              :content "follow-up selected"))
                   (funcall on-item '(:type done :reason stop))))))))
           (provider
            (e-context-provider-create
             :name 'context-lifetime-tool-result-canvas
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      '((:role system :content "CANVAS-A")))))
           (capability
            (e-capability-create
             :id 'context-lifetime-tool-result-capability
             :context-providers (list provider)
             :tools
             (list
              (lambda (registry)
                (e-tools-test-register
                 registry
                 :name "inspect-result"
                 :description "Return a uniquely identifiable result."
                 :handler (lambda (_arguments) raw-result))))))
           (harness
            (e-harness-create
             :backend backend
             :sessions store
             :intrinsic-capabilities (list capability))))
      (unwind-protect
          (progn
            (let ((e-context-lifetime-shadow-projection-enabled t)
                  (complete-frame
                   (symbol-function
                    'e-context-lifetime-frame-complete-for-consumer)))
              (e-harness-create-session harness :id "session-1")
              (e-harness--install-activity-sink
               harness (lambda (event) (push event events))
               :session-id "session-1")
              (cl-letf (((symbol-function
                          'e-context-lifetime-frame-complete-for-consumer)
                         (lambda (&rest arguments)
                           (let ((frame (apply complete-frame arguments)))
                             (push (list :frame frame
                                         :response-id (nth 2 arguments))
                                   consumed-frames)
                             frame))))
                (e-harness-test-prompt-batch harness "session-1" "inspect")))
            (e-session-flush-write-queue store)
            (should (= request-count 3))
            (let* ((ordered (reverse requests))
                   (request-b (cadr ordered))
                   (request-c (caddr ordered))
                   (messages-b (plist-get request-b :messages))
                   (roles-b (mapcar (lambda (message) (plist-get message :role))
                                    messages-b))
                   (tool-message-c
                    (seq-find
                     (lambda (message)
                       (and (eq (plist-get message :role) 'tool)
                            (equal (plist-get (plist-get message :content)
                                              :tool-call-id)
                                   "call-tool-result")))
                     (plist-get request-c :messages)))
                   (replay-items-c
                    (plist-get (plist-get tool-message-c :metadata)
                               :provider-replay-items))
                   (curation-call-position
                    (cl-position-if
                     (lambda (item)
                       (equal (plist-get (plist-get item :item) :type)
                              "function_call"))
                     replay-items-c))
                   (curation-ack-position
                    (cl-position-if
                     (lambda (item)
                       (and (equal (plist-get (plist-get item :item) :type)
                                   "function_call_output")
                            (equal (plist-get (plist-get item :item) :output)
                                   "")))
                     replay-items-c))
                   (curation-arguments (plist-get curation-input :arguments)))
              (should (equal (mapcar (lambda (message)
                                       (plist-get message :role))
                                     messages-b)
                             '(user system system tool-call system tool)))
              (should (string-match-p raw-result-marker
                                      (prin1-to-string messages-b)))
              (let ((tool-position
                     (cl-position 'tool roles-b :from-end t)))
                (should (equal (plist-get (nth (1- tool-position) messages-b)
                                          :role)
                               'system))
                (should (string-match-p
                         "\\[ephemeral context source 2, ~[0-9]+ tokens\\]"
                         (plist-get (nth (1- tool-position) messages-b)
                                    :content))))
              (should (equal curation-arguments
                             '(:keep nil
                               :summaries
                               ((:sources (2)
                                 :text "selected from tool result")))))
              (should tool-message-c)
              (should (equal (mapcar (lambda (item)
                                       (plist-get (plist-get item :item) :type))
                                     replay-items-c)
                             '("function_call" "function_call_output")))
              (should (integerp curation-call-position))
              (should (integerp curation-ack-position))
              (should (< curation-call-position curation-ack-position)))
            (should (equal (mapcar (lambda (message) (plist-get message :role))
                                   (e-harness-messages harness "session-1"))
                           '(user tool-call tool assistant)))
            (let* ((projection (e-session-context-lifetime-projection
                                store "session-1"))
                   (curations (plist-get projection :curations))
                   (tool-call
                    (seq-find (lambda (message)
                                (eq (plist-get message :role) 'tool-call))
                              (e-harness-messages harness "session-1")))
                   (assistant
                    (car (last
                          (seq-filter
                           (lambda (message)
                             (eq (plist-get message :role) 'assistant))
                           (e-harness-messages harness "session-1")))))
                   (next-context
                    (let ((e-context-lifetime-shadow-projection-enabled t))
                      (e-harness-turn-context
                       harness "session-1" "next-consumer")))
                   (next-messages (plist-get next-context :messages))
                   (printed (prin1-to-string next-messages))
                   (control-events
                    (seq-filter
                     (lambda (event)
                       (eq (plist-get event :event-type)
                           'context-curation-response))
                     (e-session-activity-events store "session-1")))
                   (control (car control-events))
                   (control-id (and control (plist-get control :id)))
                   (consumed-event
                    (seq-find
                     (lambda (event)
                       (eq (plist-get event :type) 'context-frame-consumed))
                     events))
                   (consumed-binding
                    (seq-find
                     (lambda (binding)
                       (equal (plist-get binding :response-id) control-id))
                     consumed-frames))
                   (reopened (e-session-persistent-store-create directory)))
              (should (= (length curations) 1))
              (should (= (length control-events) 1))
              (should control)
              (should (equal control-id
                             (plist-get (car curations) :response-entry-id)))
              (should (equal control-id
                             (plist-get (plist-get control :payload)
                                        :response-entry-id)))
              (should (equal control-id
                             (plist-get (plist-get consumed-event :payload)
                                        :response-entry-id)))
              (should consumed-binding)
              (should (equal control-id
                             (e-context-lifetime-frame-consuming-response-entry-id
                              (plist-get consumed-binding :frame))))
              (should-not (equal control-id (plist-get tool-call :id)))
              (should-not (equal control-id (plist-get assistant :id)))
              (should (equal (plist-get control :event-type)
                             'context-curation-response))
              (should (equal (plist-get control :payload)
                             (list :response-entry-id control-id)))
              (should-not (string-match-p
                           "curation-call\\|function_call_output\\|frame-id\\|retry"
                           (prin1-to-string control)))
              (should-not (seq-find
                           (lambda (message)
                             (equal (plist-get message :id) control-id))
                           (e-session-messages store "session-1")))
              (should-not (string-match-p control-id printed))
              (should
               (equal (plist-get projection :promotion-messages)
                      '((:role system :content "selected from tool result"))))
              (should (string-match-p "selected from tool result" printed))
              (should-not (string-match-p raw-result-marker printed))
              (should-not (string-match-p "call-tool-result" printed))
              (should-not (member 'tool-call
                                  (mapcar (lambda (message)
                                            (plist-get message :role))
                                          next-messages)))
              (should-not (member 'tool
                                  (mapcar (lambda (message)
                                            (plist-get message :role))
                                          next-messages)))
              (let* ((reopened-record
                      (car (e-session-context-curations reopened "session-1")))
                     (reopened-control
                      (e-session-entry-by-id reopened "session-1" control-id))
                     (fork (e-session-fork reopened "session-1"))
                     (fork-id (plist-get fork :id))
                     (fork-projection
                      (e-session-context-lifetime-projection reopened fork-id))
                     (fork-generation (plist-get fork-projection :generation))
                     (fork-text
                      (prin1-to-string
                       (e-context-lifetime-generation-checkpoint
                        fork-generation))))
                (should (equal control-id
                               (plist-get reopened-record :response-entry-id)))
                (should (equal (e-session-entry-by-id reopened
                                                      "session-1" control-id)
                               reopened-control))
                (should (string-match-p "selected from tool result" fork-text))
                (should-not (string-match-p raw-result-marker fork-text))
                (should-not (string-match-p control-id fork-text))
                (should-not (e-session-entry-by-id reopened fork-id control-id)))))
        (delete-directory directory t)))))

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

(ert-deftest e-harness-test-context-lifetime-preflights-before-assistant-append ()
  "Completion-only curation failures do not append an assistant or consume its frame."
  (e-harness-test--with-empty-layer-registry
             (dolist (case '((unknown-label . (:keep (2) :summaries nil))
                    (oversized-record . (:keep (1) :summaries nil))))
      (let* ((request-count 0)
             (source-value "PREFLIGHT-SOURCE")
             (captured-frame nil)
             (prepared-bytes nil)
             (backend
              (e-backend-create
               :name "context-lifetime-completion-preflight"
               :context-capabilities
               '(:continuation none
                 :observation-delivery request-local-replaceable
                 :reserved-effect-carrier context-curate-wire)
               :stream
               (cl-function
                (lambda (&key on-item &allow-other-keys)
                  (cl-incf request-count)
                  (let ((arguments
                         (if (eq (car case) 'oversized-record)
                             (let ((text "x")
                                   (bytes 0)
                                   (sources
                                    (e-context-lifetime-frame-curation-sources
                                     captured-frame)))
                               ;; The provider request ID is a 26-character
                               ;; ULID.  Hash output has fixed width, so this
                               ;; computes the exact one-over prepared bound
                               ;; before the real completion callback runs.
                               (while (< bytes 8193)
                                 (setq text (concat text "x")
                                       bytes
                                       (e-context-lifetime--bytes
                                        (e-context-lifetime--curation-record
                                         captured-frame
                                         (list
                                          :keep nil
                                          :summaries
                                          (list (list :sources '(1)
                                                      :text text)))
                                         (make-string 26 ?r)
                                         sources))))
                               (setq prepared-bytes bytes)
                               (list :keep nil
                                     :summaries
                                     (list (list :sources '(1)
                                                 :text text))))
                           (cdr case))))
                    (funcall on-item
                             (list :type 'context-curate
                                   :arguments arguments)))
                  (funcall on-item
                           '(:type assistant-message
                             :content "MUST-NOT-PERSIST"))
                  (funcall on-item '(:type done :reason stop))))))
             (provider
              (e-context-provider-create
               :name 'completion-preflight-source
               :cache-placement 'dynamic-context
               :build (lambda (&rest _)
                        (list (list :role 'system :content source-value)))))
             (capability
              (e-capability-create
               :id 'completion-preflight-capability
               :context-providers (list provider)))
             (harness
              (e-harness-create
               :backend backend
               :intrinsic-capabilities (list capability)))
             (make-frame (symbol-function
                          'e-context-lifetime-frame-create-from-segments)))
        (cl-letf (((symbol-function
                    'e-context-lifetime-frame-create-from-segments)
                   (lambda (&rest arguments)
                     (setq captured-frame (apply make-frame arguments))
                     captured-frame)))
          (let ((e-context-lifetime-shadow-projection-enabled t))
            (e-harness-create-session harness :id "completion-preflight")
            (should-error
             (e-harness-test-prompt-batch
              harness "completion-preflight" "trigger curation")
             :type 'e-context-lifetime-invalid-record)))
        (should (= request-count 1))
        (when (eq (car case) 'oversized-record)
          (should (= prepared-bytes 8193)))
        (should (e-context-lifetime-frame-p captured-frame))
        (should-not (e-context-lifetime-frame-consumed-p captured-frame))
        (should (equal
                 (plist-get
                  (car (e-context-lifetime-frame-observations captured-frame))
                  :body)
                 (list :role 'system :content source-value)))
        (should-not (e-session-context-curations
                     (e-harness-sessions harness) "completion-preflight"))
        (should-not
         (seq-find (lambda (message)
                     (eq (plist-get message :role) 'assistant))
                   (e-harness-messages harness "completion-preflight")))))))

(ert-deftest e-harness-test-context-lifetime-assistant-curation-preserves-response-entry-id ()
  "A prepared curation and its assistant share one durable response identity."
  (e-harness-test--with-empty-layer-registry
    (let* ((directory (make-temp-file "e-harness-response-id-" t))
           (store (e-session-persistent-store-create directory))
           (captured-frame nil)
           (consumed-frame nil)
           (events nil)
           (backend
            (e-backend-create
             :name "context-lifetime-assistant-response-id"
             :context-capabilities
             '(:continuation none
               :observation-delivery request-local-replaceable
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key on-item &allow-other-keys)
                (funcall on-item
                         '(:type context-curate
                           :arguments (:keep (1) :summaries nil)))
                (funcall on-item
                         '(:type assistant-message :content "selected"))
                (funcall on-item '(:type done :reason stop))))))
           (provider
            (e-context-provider-create
             :name 'assistant-response-id-source
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      '((:role system :content "SOURCE-FOR-RESPONSE-ID")))))
           (capability
            (e-capability-create
             :id 'assistant-response-id-capability
             :context-providers (list provider)))
           (harness
            (e-harness-create
             :backend backend
             :sessions store
             :intrinsic-capabilities (list capability)))
           (make-frame
            (symbol-function 'e-context-lifetime-frame-create-from-segments))
           (complete-frame
             (symbol-function
              'e-context-lifetime-frame-complete-for-consumer)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'e-context-lifetime-frame-create-from-segments)
                       (lambda (&rest arguments)
                         (setq captured-frame (apply make-frame arguments))
                         captured-frame))
                      ((symbol-function 'e-context-lifetime-frame-complete-for-consumer)
                       (lambda (&rest arguments)
                         (setq consumed-frame (apply complete-frame arguments)))))
              (e-harness--install-activity-sink
               harness (lambda (event) (push event events))
               :session-id "assistant-response-id")
              (let ((e-context-lifetime-shadow-projection-enabled t))
                (e-harness-create-session harness :id "assistant-response-id")
                (e-harness-test-prompt-batch
                 harness "assistant-response-id" "curate this source")))
            (e-session-flush-write-queue store)
            (let* ((messages (e-harness-messages harness "assistant-response-id"))
                   (assistant (car (last (seq-filter
                                          (lambda (message)
                                            (eq (plist-get message :role) 'assistant))
                                          messages))))
                   (record (car (e-session-context-curations
                                 store "assistant-response-id")))
                   (event (seq-find
                           (lambda (entry)
                             (eq (plist-get entry :type)
                                 'context-frame-consumed))
                           events))
                   (response-id (plist-get assistant :id))
                   (reopened (e-session-persistent-store-create directory)))
              (should (stringp response-id))
              (should (equal response-id (plist-get record :response-entry-id)))
              (should (equal response-id
                             (e-context-lifetime-frame-consuming-response-entry-id
                              consumed-frame)))
              (should (equal response-id
                             (plist-get (plist-get event :payload)
                                        :response-entry-id)))
              (should captured-frame)
              (should consumed-frame)
              (should (e-context-lifetime-frame-consumed-p consumed-frame))
              (let* ((reopened-record
                      (car (e-session-context-curations
                            reopened "assistant-response-id")))
                     (reopened-assistant
                      (seq-find (lambda (message)
                                  (equal (plist-get message :id) response-id))
                                (e-session-messages reopened
                                                     "assistant-response-id")))
                     (fork (e-session-fork reopened "assistant-response-id"))
                     (fork-id (plist-get fork :id))
                     (fork-projection
                      (e-session-context-lifetime-projection reopened fork-id))
                     (fork-generation (plist-get fork-projection :generation))
                     (source-record-after-fork
                      (car (e-session-context-curations
                            reopened "assistant-response-id")))
                     (source-assistant-after-fork
                      (seq-find (lambda (message)
                                  (equal (plist-get message :id) response-id))
                                (e-session-messages reopened
                                                     "assistant-response-id"))))
                (should (equal response-id
                               (plist-get reopened-record :response-entry-id)))
                (should (equal response-id (plist-get reopened-assistant :id)))
                (should (equal response-id
                               (plist-get source-assistant-after-fork :id)))
                (should (equal response-id
                               (plist-get source-record-after-fork
                                          :response-entry-id)))
                (should (member '(:role system :content "SOURCE-FOR-RESPONSE-ID")
                                (e-context-lifetime-generation-checkpoint
                                 fork-generation)))
                (should-not
                 (string-match-p "\\[ephemeral context source 1, ~"
                                 (prin1-to-string
                                  (e-context-lifetime-generation-checkpoint
                                   fork-generation))))
                (should (stringp fork-id)))))
        (delete-directory directory t)))))

(ert-deftest e-harness-test-context-lifetime-zero-curation-consumes-frame ()
  "A response with no curation drops its live frame without a durable record."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         frame entry)
    (e-harness-create-session harness :id "session-1")
    (setq frame (e-harness-test--curation-frame))
    (setq entry (list :status 'running :context-frame frame))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (should
       (e-context-lifetime-frame-consumed-p
        (e-harness--lifetime-commit-response
         harness "session-1" "turn-1" entry
         (list :frame frame
               :provider-request-id "response-1"
               :response-entry-id "response-1"
               :curation-effects nil)))))
    (should (e-context-lifetime-frame-consumed-p
             (plist-get entry :context-frame)))
    (should (e-context-lifetime-frame-observations frame))
    (should-not (e-session-context-curations store "session-1"))))

(ert-deftest e-harness-test-context-lifetime-erase-only-curation-is-audit-and-erasure ()
  "An erase-only response records erasure and audit before consuming its frame."
  (let* ((directory (make-temp-file "e-harness-erase-only-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         frame entry
         (events nil)
         (order nil)
         (append-package
          (symbol-function 'e-session-append-context-curation-package))
         (append-control
          (symbol-function 'e-session-append-context-curation-response))
         (complete-frame
          (symbol-function 'e-context-lifetime-frame-complete-for-consumer)))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "erase-only")
          (let ((generation
                 (e-harness--context-lifetime-ensure-generation
                  harness "erase-only")))
            (setq frame
                  (e-harness-test--tool-result-curation-frame
                   (e-context-lifetime-generation-id generation)
                   "frame:erase-only" "call-erase-only"
                   "ERASE-ONLY-RAW" "observation:erase-only"
                   "source:erase-only" "fingerprint:erase-only"))
            (setq entry (list :status 'running :context-frame frame)))
          (e-harness--install-activity-sink
           harness (lambda (event) (push event events))
           :session-id "erase-only")
          (let ((e-context-lifetime-shadow-projection-enabled t))
            (cl-letf (((symbol-function
                        'e-session-append-context-curation-package)
                       (lambda (&rest arguments)
                         (setq order (append order '(package)))
                         (apply append-package arguments)))
                      ((symbol-function
                        'e-session-append-context-curation-response)
                       (lambda (&rest arguments)
                         (setq order (append order '(control)))
                         (apply append-control arguments)))
                      ((symbol-function
                        'e-context-lifetime-frame-complete-for-consumer)
                       (lambda (&rest arguments)
                         (setq order (append order '(consume)))
                         (apply complete-frame arguments))))
              (should
               (e-context-lifetime-frame-consumed-p
                (e-harness--lifetime-commit-response
                 harness "erase-only" "turn-erase" entry
                 (list :frame frame
                       :response-entry-id "response-erase-only"
                       :curation-effects
                       (list
                        (list :type 'context-curate
                              :arguments '(:keep nil :summaries nil
                                            :erase (1))))))))))
          (should (equal order '(package control consume)))
          (e-session-flush-write-queue store)
          (let* ((erasures (e-session-context-erasures store "erase-only"))
                 (controls
                  (seq-filter
                   (lambda (event)
                     (eq (plist-get event :event-type)
                         'context-curation-response))
                   (e-session-activity-events store "erase-only")))
                 (consumed-event
                 (seq-find
                   (lambda (event)
                     (eq (plist-get event :type)
                         'context-frame-consumed))
                   events))
                 (reopened (e-session-persistent-store-create directory)))
            (should (= (length erasures) 1))
            (should (equal
                     (e-session-erased-tool-call-ids store "erase-only")
                     '("call-erase-only")))
            (should (= (length controls) 1))
            (should consumed-event)
            (should (equal (plist-get (plist-get consumed-event :payload)
                                      :response-entry-id)
                           "response-erase-only"))
            (should (equal (plist-get (plist-get consumed-event :payload)
                                      :frame-id)
                           (e-context-lifetime-frame-id frame)))
            (should-not (e-session-context-curations store "erase-only"))
            (let* ((reopened-erasures
                    (e-session-context-erasures reopened "erase-only"))
                   (reopened-controls
                    (seq-filter
                     (lambda (event)
                       (eq (plist-get event :event-type)
                           'context-curation-response))
                     (e-session-activity-events reopened "erase-only")))
                   (control (car controls))
                   (reopened-control
                    (car reopened-controls)))
              (should (= (length reopened-erasures) 1))
              (should (equal
                       (e-session-erased-tool-call-ids reopened "erase-only")
                       '("call-erase-only")))
              (should (= (length reopened-controls) 1))
              (should (equal (plist-get control :id)
                             (plist-get reopened-control :id)))
              (should (equal (plist-get (plist-get reopened-control :payload)
                                        :response-entry-id)
                             "response-erase-only"))
              (should (e-session-entry-by-id
                       reopened "erase-only" (plist-get control :id)))
              (should-not
               (seq-find
                (lambda (message)
                  (equal (plist-get message :id) (plist-get control :id)))
                (e-session-messages reopened "erase-only"))))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-context-lifetime-mixed-curation-package-is-atomic ()
  "Mixed exact retention and erasure share one package and failure is inert."
  (let* ((directory (make-temp-file "e-harness-mixed-curation-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         frame entry)
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "mixed")
          (let ((generation
                 (e-harness--context-lifetime-ensure-generation
                  harness "mixed")))
            (setq frame
                  (e-harness-test--two-tool-result-curation-frame
                   (e-context-lifetime-generation-id generation)))
            (setq entry (list :status 'running :context-frame frame)))
          (let ((e-context-lifetime-shadow-projection-enabled t))
            (should
             (e-context-lifetime-frame-consumed-p
              (e-harness--lifetime-commit-response
               harness "mixed" "turn-mixed" entry
               (list :frame frame
                     :response-entry-id "response-mixed"
                     :curation-effects
                     (list
                      (list :type 'context-curate
                            :arguments
                            '(:keep (1)
                              :summaries nil
                              :erase (2)))))))))
          (let* ((package-entry
                  (seq-find
                   (lambda (item)
                     (eq (plist-get item :type)
                         'context-curation-package))
                   (e-session-current-path store "mixed")))
                 (curations (e-session-context-curations store "mixed"))
                 (erasures (e-session-context-erasures store "mixed")))
            (should package-entry)
            (should (plist-get package-entry :promotion))
            (should (plist-get package-entry :erasure))
            (should (= (length curations) 1))
            (should (= (length erasures) 1))
            (should (equal (e-session-erased-tool-call-ids store "mixed")
                           '("call:harness-mixed-2"))))
          (e-session-flush-write-queue store)
          (let ((reopened (e-session-persistent-store-create directory)))
            (should (= (length (e-session-context-curations
                                reopened "mixed"))
                       1))
            (should (= (length (e-session-context-erasures reopened "mixed"))
                       1))
            (should (equal (e-session-erased-tool-call-ids reopened "mixed")
                           '("call:harness-mixed-2"))))

          (e-harness-create-session harness :id "mixed-invalid")
          (let (bad-frame bad-entry)
            (let ((generation
                   (e-harness--context-lifetime-ensure-generation
                    harness "mixed-invalid")))
              (setq bad-frame
                    (e-harness-test--two-tool-result-curation-frame
                     (e-context-lifetime-generation-id generation)))
              (setq bad-entry
                    (list :status 'running :context-frame bad-frame)))
            (let ((e-context-lifetime-shadow-projection-enabled t))
              (should-error
               (e-harness--lifetime-commit-response
                harness "mixed-invalid" "turn-mixed-invalid" bad-entry
                (list :frame bad-frame
                      :response-entry-id "response-mixed-invalid"
                      :curation-effects
                      (list
                       (list :type 'context-curate
                             :arguments
                             '(:keep (1)
                               :summaries nil
                               :erase (1))))))
               :type 'e-context-lifetime-invalid-record))
            (should-not (e-context-lifetime-frame-consumed-p bad-frame))
            (should-not (e-session-context-curations store "mixed-invalid"))
            (should-not (e-session-context-erasures store "mixed-invalid")))

          (e-harness-create-session harness :id "mixed-failure")
          (let (failure-frame failure-entry)
            (let ((generation
                   (e-harness--context-lifetime-ensure-generation
                    harness "mixed-failure")))
              (setq failure-frame
                    (e-harness-test--two-tool-result-curation-frame
                     (e-context-lifetime-generation-id generation)))
              (setq failure-entry
                    (list :status 'running :context-frame failure-frame)))
            (cl-letf (((symbol-function
                        'e-session-append-context-curation-package)
                       (lambda (&rest _)
                         (error "synthetic curation package failure"))))
              (let ((e-context-lifetime-shadow-projection-enabled t))
                (should-error
                 (e-harness--lifetime-commit-response
                  harness "mixed-failure" "turn-mixed-failure" failure-entry
                  (list :frame failure-frame
                        :response-entry-id "response-mixed-failure"
                        :curation-effects
                        (list
                         (list :type 'context-curate
                               :arguments
                               '(:keep (1)
                                 :summaries nil
                                 :erase (2))))))
                 :type 'error)))
            (should-not (e-context-lifetime-frame-consumed-p failure-frame))
            (should-not (e-session-context-curations store "mixed-failure"))
            (should-not (e-session-context-erasures store "mixed-failure"))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-context-lifetime-curation-binds-payload-frame-over-descendant ()
  "Curation labels bind to the response frame, not a newer tool descendant."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         frame-a frame-b entry consumed)
    (e-harness-create-session harness :id "session-1")
    (let ((generation
           (e-harness--context-lifetime-ensure-generation
            harness "session-1")))
      (setq frame-a
            (e-harness-test--curation-frame
             (e-context-lifetime-generation-id generation)
             "frame:a" "PAYLOAD-FRAME-VALUE" "observation:a" "source:a"
             "fingerprint:a"))
      (setq frame-b
            (e-harness-test--curation-frame
             (e-context-lifetime-generation-id generation)
             "frame:b" "DESCENDANT-TOOL-VALUE" "observation:b" "source:b"
             "fingerprint:b"))
      (setq entry (list :status 'running :context-frame frame-b)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (setq consumed
            (e-harness--lifetime-commit-response
             harness "session-1" "turn-1" entry
             (list :frame frame-a
                   :provider-request-id "response-a"
                   :response-entry-id "response-a"
                   :curation-effects
                   (list (list :type 'context-curate
                               :arguments
                               '(:keep (1) :summaries nil)))))))
    (should (equal (e-context-lifetime-frame-id consumed) "frame:a"))
    (should (eq (plist-get entry :context-frame) frame-b))
    (should-not (e-context-lifetime-frame-consumed-p frame-b))
    (let* ((record (car (e-session-context-curations store "session-1")))
           (item (car (plist-get record :items))))
      (should (equal (plist-get item :value) "PAYLOAD-FRAME-VALUE"))
      (should (equal (plist-get item :source-observation-ids)
                     '("observation:a")))
      (should (equal (plist-get item :source-refs) '("source:a")))
      (should (equal (plist-get item :source-fingerprints)
                     '("fingerprint:a")))
      (should-not (string-match-p "DESCENDANT-TOOL-VALUE"
                                  (prin1-to-string record)))
      (should-not (string-match-p "observation:b"
                                  (prin1-to-string record))))))

(ert-deftest e-harness-test-context-lifetime-zero-curation-does-not-consume-descendant ()
  "Zero-effect completion for A leaves a newer live descendant B untouched."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         frame-a frame-b entry consumed)
    (e-harness-create-session harness :id "session-1")
    (let ((generation
           (e-harness--context-lifetime-ensure-generation
            harness "session-1")))
      (setq frame-a
            (e-harness-test--curation-frame
             (e-context-lifetime-generation-id generation)
             "frame:a-zero" "PAYLOAD-ZERO-VALUE" "observation:a-zero"
             "source:a-zero" "fingerprint:a-zero"))
      (setq frame-b
            (e-harness-test--curation-frame
             (e-context-lifetime-generation-id generation)
             "frame:b-zero" "DESCENDANT-ZERO-VALUE" "observation:b-zero"
             "source:b-zero" "fingerprint:b-zero"))
      (setq entry (list :status 'running :context-frame frame-b)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (setq consumed
            (e-harness--lifetime-commit-response
             harness "session-1" "turn-1" entry
             (list :frame frame-a
                   :provider-request-id "response-a-zero"
                   :response-entry-id "response-a-zero"
                   :curation-effects nil))))
    (should (equal (e-context-lifetime-frame-id consumed) "frame:a-zero"))
    (should (eq (plist-get entry :context-frame) frame-b))
    (should-not (e-context-lifetime-frame-consumed-p frame-b))
    (should-not (e-session-context-curations store "session-1"))))

(ert-deftest e-harness-test-context-lifetime-invalid-curation-keeps-live-frame ()
  "Preparation failure leaves both the live source body and session untouched."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         (frame (e-harness-test--curation-frame))
         (entry (list :status 'running :context-frame frame)))
    (e-harness-create-session harness :id "session-1")
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (should-error
       (e-harness--lifetime-commit-response
        harness "session-1" "turn-1" entry
        (list :frame frame
               :provider-request-id "response-1"
               :curation-effects
               (list (list :type 'context-curate
                           :arguments
                           '(:keep (2) :summaries nil)))))
       :type 'e-context-lifetime-invalid-record))
    (should (eq (plist-get entry :context-frame) frame))
    (should-not (e-context-lifetime-frame-consumed-p frame))
    (should (equal (plist-get (car (e-context-lifetime-frame-observations frame))
                              :body)
                   '(:role system :content "HARNESS-EXACT-VALUE")))
    (should-not (e-session-context-curations store "session-1"))))

(ert-deftest e-harness-test-context-lifetime-curation-appends-before-consuming ()
  "A valid curation appends its record before the frame body is consumed."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         frame entry
         (order nil)
         (append-context-curation-package
          (symbol-function 'e-session-append-context-curation-package))
         (complete-frame
          (symbol-function 'e-context-lifetime-frame-complete-for-consumer)))
    (e-harness-create-session harness :id "session-1")
    (let ((generation
           (e-harness--context-lifetime-ensure-generation
            harness "session-1")))
      (setq frame
            (e-harness-test--curation-frame
             (e-context-lifetime-generation-id generation)))
      (setq entry (list :status 'running :context-frame frame)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (cl-letf (((symbol-function 'e-session-append-context-curation-package)
                 (lambda (&rest arguments)
                   (setq order (append order '(package)))
                   (apply append-context-curation-package arguments)))
                ((symbol-function
                  'e-context-lifetime-frame-complete-for-consumer)
                 (lambda (&rest arguments)
                   (setq order (append order '(consume)))
                   (apply complete-frame arguments))))
        (e-harness--lifetime-commit-response
         harness "session-1" "turn-1" entry
         (list :frame frame
               :provider-request-id "response-1"
               :response-entry-id "response-1"
               :curation-effects
               (list (list :type 'context-curate
                           :arguments
                           '(:keep (1) :summaries nil)))))))
    (should (equal order '(package consume)))
    (should (= (length (e-session-context-curations store "session-1")) 1))
    (should (e-context-lifetime-frame-consumed-p
             (plist-get entry :context-frame)))))

(ert-deftest e-harness-test-context-lifetime-steering-rebuilds-after-follow-up ()
  "Steering after a tool follow-up uses the fresh projection exactly once."
  (e-harness-test--with-empty-layer-registry
    (let (backend provider capability harness turn-id
          (request-count 0) requests finish-b raw-result)
      (setq raw-result "RAW-STEERING-RESULT")
      (setq backend
            (e-backend-create
             :name "context-lifetime-steering-refresh"
             :context-capabilities
             '(:continuation linear
               :observation-delivery inherited
               :reserved-effect-carrier context-curate-wire)
             :start
             (cl-function
              (lambda (&key messages options on-item on-done on-request-start
                             &allow-other-keys)
                (setq request-count (1+ request-count))
                (push (list :messages (copy-tree messages)
                            :options (copy-tree options))
                      requests)
                (let ((request (e-backend-request-create))
                      (ordinal request-count))
                  (funcall on-request-start request)
                  (cond
                   ((= ordinal 1)
                    (funcall on-item
                             '(:type provider-replay-item
                               :provider-id fake
                               :item (:type "steering-replay"
                                      :id "REPLAY-STEERING")))
                    (funcall on-item
                             '(:type tool-call
                               :id "call-steering"
                               :name "inspect-steering"
                               :arguments (:stated_purpose "Inspect steering.")))
                    (funcall on-item
                             '(:type provider-anchor-candidate
                               :provider-id fake
                               :metadata (:response-id "resp-A")))
                    (funcall on-item '(:type done :reason tool-use))
                    (funcall on-done '(:status done)))
                   ((= ordinal 2)
                    ;; Leave B in flight so steering is queued while its
                    ;; consumed frame has not yet been finalized.
                    (setq finish-b
                          (lambda ()
                            (funcall
                             on-item
                             (list :type 'context-curate
                                   :arguments
                                   '(:keep nil
                                     :summaries
                                     ((:sources (2)
                                       :text "selected after B")))))
                              (funcall on-item
                                       '(:type assistant-message
                                         :content "B"))
                              (funcall on-item
                                       '(:type provider-anchor-candidate
                                         :provider-id fake
                                         :metadata (:response-id "resp-B")))
                              (funcall on-item '(:type done :reason stop))
                              (funcall on-done '(:status done)))))
                   ((= ordinal 3)
                    (funcall on-item
                             '(:type assistant-message :content "C"))
                    (funcall on-item '(:type done :reason stop))
                    (funcall on-done '(:status done)))
                   (t (error "unexpected request %S" ordinal)))
                  )))))
      (setq provider
            (e-context-provider-create
             :name 'context-lifetime-steering-canvas
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      '((:role system :content "CANVAS-STEERING")))))
      (setq capability
            (e-capability-create
             :id 'context-lifetime-steering-capability
             :context-providers (list provider)
             :tools
             (list
              (lambda (registry)
                (e-tools-test-register
                 registry
                 :name "inspect-steering"
                 :description "Produce one ephemeral steering result."
                 :handler (lambda (_arguments) raw-result))))))
      (setq harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability)
             :default-options '(:provider-continuation t
                                :provider-anchor-provider-id fake)))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-harness-create-session harness :id "session-1")
        (setq turn-id
              (e-harness-test-prompt-async harness "session-1" "inspect"))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (< request-count 2)
                      (not finish-b)
                      (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (should (= request-count 2))
        (should finish-b)
        (should (equal
                 (e-harness-test-steer-active-turn
                  harness "session-1" "steer now")
                 turn-id))
        (funcall finish-b)
        (let ((entry (e-harness-wait-batch harness "session-1" 1.0)))
          (should (eq (plist-get entry :status) 'done))))
      (let* ((ordered (nreverse requests))
             (request-b (nth 1 ordered))
             (request-c (nth 2 ordered))
             (messages-b (plist-get request-b :messages))
             (messages-c (plist-get request-c :messages))
             (printed-b (prin1-to-string messages-b))
             (printed-c (prin1-to-string messages-c)))
        (should (= request-count 3))
        (should (string-match-p raw-result printed-b))
        (should (string-match-p "call-steering" printed-b))
        (should (string-match-p "REPLAY-STEERING" printed-b))
        (should (string-match-p "selected after B" printed-c))
        (should (string-match-p "steer now" printed-c))
        (dolist (marker (list raw-result "call-steering" "REPLAY-STEERING"
                               "resp-A" "resp-B"))
          (should-not (string-match-p marker printed-c)))
        (should-not
         (e-session-provider-anchors
          (e-harness-sessions harness) "session-1"))))))

(ert-deftest e-harness-test-provider-compaction-sync-installs-runtime-candidate ()
  "A successful portable boundary may install only a runtime provider candidate."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (provider-input nil)
         (backend
          (e-backend-create
           :name 'provider-compaction-fake
           :context-capabilities
           '(:continuation none
             :provider-compaction opaque)
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (funcall on-item
                       '(:type assistant-message :content "portable C1"))))
           :provider-compaction
           (cl-function
            (lambda (&key messages options &allow-other-keys)
              (setq provider-input (list :messages (copy-tree messages)
                                         :options (copy-tree options)))
              '(:output ((:type "opaque" :marker "provider-state"))
                :usage (:total-tokens 3))))))
         (harness
          (e-harness-create
           :backend backend
           :default-options '(:model "fake-model"
                              :provider-anchor-provider-id fake
                              :context-lifetime-enabled t))))
    (e-harness-create-session harness :id "provider-session")
    ;; Establish the opt-in v2 generation before the ordinary transcript.
    (e-harness-turn-context harness "provider-session" "seed-turn")
    (e-session-append-message
     (e-harness-sessions harness) "provider-session"
     '(:role user :content "durable intent"))
    (e-session-append-message
     (e-harness-sessions harness) "provider-session"
     '(:role assistant :content "durable answer"))
    (e-session-append-message
     (e-harness-sessions harness) "provider-session"
     '(:role user :content "keep this"))
    (e-harness-compact-session-batch
     harness "provider-session" :keep-recent-tokens 1)
    (should provider-input)
    (should (string-match-p "portable C1"
                            (prin1-to-string (plist-get provider-input :messages))))
    (should-not (string-match-p "provider-state"
                                (prin1-to-string provider-input)))
    (let* ((candidate
            (gethash "provider-session"
                     (e-harness-provider-compaction-candidates harness)))
           (context (e-harness-turn-context
                     harness "provider-session" "candidate-turn"))
           (options (plist-get context :options)))
      (should candidate)
      (should-not (gethash "provider-session"
                           (e-harness-provider-compaction-candidates harness)))
      (should (equal (plist-get options :provider-compaction-output)
                     '((:type "opaque" :marker "provider-state"))))
      (should (eq (plist-get options :context-rendering-strategy)
                  'opaque-provider-compaction))
      ;; Provider state is a runtime candidate only, never a session record.
      (should-not
       (string-match-p
        "provider-state"
        (prin1-to-string
         (list (e-session-messages (e-harness-sessions harness)
                                   "provider-session")
               (e-session-activity-events
                (e-harness-sessions harness) "provider-session")
               (e-session-context-generations
               (e-harness-sessions harness) "provider-session"))))))))

(ert-deftest e-harness-test-provider-compaction-none-skips-input-sync ()
  "Synchronous portable compaction does no opaque-input work for NONE."
  (let* ((input-calls 0)
         (backend
          (e-backend-create
           :name 'provider-compaction-none-sync
           :context-capabilities
           '(:continuation none :provider-compaction none)
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (funcall on-item
                       '(:type assistant-message :content "NONE-SYNC-C1"))))))
         (harness (e-harness-create :backend backend
                                    :default-options
                                    '(:model "none-sync-model")))
         (store nil))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (e-harness-create-session harness :id "none-sync-session")
      (setq store (e-harness-sessions harness))
      (e-harness-turn-context harness "none-sync-session" "seed")
      (e-session-append-message store "none-sync-session"
                                '(:role user :content "NONE-SYNC-INTENT"))
      (e-session-append-message store "none-sync-session"
                                '(:role assistant :content "NONE-SYNC-ANSWER"))
      (cl-letf (((symbol-function 'e-harness--provider-compaction-input)
                 (lambda (&rest _args)
                   (setq input-calls (1+ input-calls))
                   (error "provider compaction input must stay lazy"))))
        (let ((record
               (e-harness-compact-session-batch
                harness "none-sync-session" :keep-recent-tokens 1)))
          (should record)))
      (should (= input-calls 0))
      (should (= (length (e-session-compactions store "none-sync-session"))
                 1))
      (should (string-match-p
               "NONE-SYNC-C1"
               (prin1-to-string
                (e-context-lifetime-generation-checkpoint
                 (e-session-context-lifetime-current-generation
                  store "none-sync-session"))))))))

(ert-deftest e-harness-test-provider-compaction-none-skips-input-async ()
  "Asynchronous portable compaction does no opaque-input work for NONE."
  (let* ((input-calls 0)
         (record nil)
         (failure nil)
         (backend
          (e-backend-create
           :name 'provider-compaction-none-async
           :context-capabilities
           '(:continuation none :provider-compaction none)
           :start
           (cl-function
            (lambda (&key on-item on-done &allow-other-keys)
              (run-at-time
               0.01 nil
               (lambda ()
                 (funcall on-item
                          '(:type assistant-message :content "NONE-ASYNC-C1"))
                 (funcall on-done '(:status done))))
              (e-backend-request-create)))))
         (harness (e-harness-create :backend backend
                                    :default-options
                                    '(:model "none-async-model")))
         (store nil))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (e-harness-create-session harness :id "none-async-session")
      (setq store (e-harness-sessions harness))
      (e-harness-turn-context harness "none-async-session" "seed")
      (e-session-append-message store "none-async-session"
                                '(:role user :content "NONE-ASYNC-INTENT"))
      (e-session-append-message store "none-async-session"
                                '(:role assistant :content "NONE-ASYNC-ANSWER"))
      (cl-letf (((symbol-function 'e-harness--provider-compaction-input)
                 (lambda (&rest _args)
                   (setq input-calls (1+ input-calls))
                   (error "provider compaction input must stay lazy"))))
        (e-harness-compact-session-start
         harness "none-async-session"
         :keep-recent-tokens 1
         :on-done (lambda (value) (setq record value))
         :on-error (lambda (err) (setq failure err)))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (not (or record failure))
                      (< (float-time) deadline))
            (accept-process-output nil 0.01))))
      (should record)
      (should-not failure)
      (should (= input-calls 0))
      (should (= (length (e-session-compactions store "none-async-session"))
                 1))
      (should (string-match-p
               "NONE-ASYNC-C1"
               (prin1-to-string
                (e-context-lifetime-generation-checkpoint
                 (e-session-context-lifetime-current-generation
                  store "none-async-session"))))))))

(ert-deftest e-harness-test-provider-compaction-candidate-fences-late-tail-and-promotion ()
  "A candidate covers its captured boundary and rejects a changed promotion frontier."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (backend
          (e-backend-fake-create
           :items nil
           :context-capabilities
           '(:continuation none :provider-compaction opaque)
           :provider-compaction
           (lambda (&rest _args)
             '(:output ((:type "opaque")) :usage nil))))
         (harness
          (e-harness-create
           :backend backend
           :default-options '(:model "candidate-model"
                              :provider-anchor-provider-id fake)))
         (session-id "candidate-fence"))
    (e-harness-create-session harness :id session-id)
    (let ((session (e-session-get (e-harness-sessions harness) session-id)))
      (e-session-append-context-generation
       (e-harness-sessions harness) session-id
       (e-context-lifetime-generation-create
        :id "generation:candidate-fence"
        :checkpoint '((:role system :content "C0"))
        :covered-session-boundary (plist-get session :root-event-id))))
    (e-session-append-message
     (e-harness-sessions harness) session-id
     '(:role user :content "before compact"))
    (let* ((before (e-harness-turn-context harness session-id "before"))
           (generation (plist-get before :lifetime-generation))
           (input (e-harness--provider-compaction-input
                   harness session-id generation))
           (source-entry-id (plist-get input :source-entry-id)))
      (should source-entry-id)
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role tool-call
         :content (:id "late-call" :name "inspect"
                   :arguments (:marker "LATE-RAW-CALL"))
         :metadata (:provider-replay-items
                    ((:id "LATE-RAW-REPLAY")))))
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role tool
         :content (:tool-call-id "late-call"
                   :content "LATE-RAW-RESULT")))
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role assistant :content "late durable tail"))
      (e-harness--provider-compaction-store-candidate
       harness session-id before generation
       '((:type "opaque")) nil
       source-entry-id
       (plist-get input :promotion-frontier)
       (plist-get input :input-fingerprint))
      (let* ((after (e-harness-turn-context harness session-id "after"))
             (options (plist-get after :options))
             (delta (plist-get options :provider-compaction-delta-messages)))
        (should (equal (plist-get options :provider-compaction-output)
                       '((:type "opaque"))))
        (should (= (cl-count "late durable tail" delta
                             :key (lambda (message)
                                    (plist-get message :content))
                             :test #'equal)
                   1))
        (dolist (marker '("LATE-RAW-CALL" "LATE-RAW-REPLAY"
                          "LATE-RAW-RESULT"))
          (should-not (string-match-p marker (prin1-to-string delta)))))
      ;; A promotion committed after the captured provider input makes the
      ;; opaque result incomplete, so no candidate is installed.
      (clrhash (e-harness-provider-compaction-candidates harness))
      (let ((promotion-generation
             (e-context-lifetime-generation-id generation)))
        (e-harness-test--append-compaction-curation
         (e-harness-sessions harness) session-id promotion-generation "late"))
      (let* ((latest-input (e-harness--provider-compaction-input
                            harness session-id generation)))
        (e-harness--provider-compaction-store-candidate
         harness session-id before generation
         '((:type "opaque")) nil
         (plist-get latest-input :source-entry-id)
         (plist-get input :promotion-frontier)
         (plist-get input :input-fingerprint)))
      (should-not (gethash session-id
                           (e-harness-provider-compaction-candidates harness))))))

(ert-deftest e-harness-test-provider-compaction-async-captures-fixed-boundary ()
  "An async result uses its start-time boundary, leaving later durable input in delta."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (done-callback nil)
         (backend
          (e-backend-fake-create
           :items nil
           :context-capabilities
           '(:continuation none :provider-compaction opaque)
           :provider-compaction
           (cl-function
            (lambda (&key on-done &allow-other-keys)
              (setq done-callback on-done)
              (e-backend-request-create :metadata '(:async t))))))
         (harness
          (e-harness-create
           :backend backend
           :default-options '(:model "async-candidate-model"
                              :provider-anchor-provider-id fake)))
         (session-id "async-candidate"))
    (e-harness-create-session harness :id session-id)
    (let* ((session (e-session-get (e-harness-sessions harness) session-id))
           (generation-entry
           (e-session-append-context-generation
             (e-harness-sessions harness) session-id
             (e-context-lifetime-generation-create
              :id "generation:async-candidate"
              :checkpoint '((:role system :content "C0"))
              :covered-session-boundary (plist-get session :root-event-id)))))
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role user :content "async before"))
      (e-harness--maybe-provider-compaction-start
       harness session-id generation-entry)
      (should done-callback)
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role assistant :content "async late"))
      (funcall done-callback '(:output ((:type "opaque")) :usage nil))
      (let* ((context (e-harness-turn-context harness session-id "async-after"))
             (options (plist-get context :options))
             (delta (plist-get options :provider-compaction-delta-messages)))
        (should (equal (plist-get options :provider-compaction-output)
                       '((:type "opaque"))))
        (should (= (cl-count "async late" delta
                             :key (lambda (message)
                                    (plist-get message :content))
                             :test #'equal)
                   1))))))

(ert-deftest e-harness-test-provider-compaction-mismatch-and-reopen-fallback ()
  "Model/layout/generation mismatches and a reopened harness use portable context."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (store (e-session-store-create))
         (backend (e-backend-fake-create
                   :items nil
                   :context-capabilities
                   '(:continuation none :provider-compaction opaque)
                   :provider-compaction
                   (lambda (&rest _args)
                     '(:output ((:type "opaque")) :usage nil))))
         (harness
          (e-harness-create
           :backend backend
           :sessions store
           :default-options '(:model "candidate-model"
                              :provider-anchor-provider-id fake)))
         (session-id "candidate-mismatch"))
    (e-harness-create-session harness :id session-id)
    (let ((session (e-session-get store session-id)))
      (e-session-append-context-generation
       store session-id
       (e-context-lifetime-generation-create
        :id "generation:candidate-mismatch"
        :checkpoint '((:role system :content "C0"))
        :covered-session-boundary (plist-get session :root-event-id))))
    (e-session-append-message store session-id
                              '(:role user :content "portable source"))
    (let* ((context (e-harness-turn-context harness session-id "original"))
           (generation (plist-get context :lifetime-generation))
           (input (e-harness--provider-compaction-input
                   harness session-id generation))
           (source (plist-get input :source-entry-id)))
      (e-harness--provider-compaction-store-candidate
       harness session-id context generation '((:type "opaque")) nil
       source (plist-get input :promotion-frontier)
       (plist-get input :input-fingerprint))
      (let ((model-context (copy-tree context)))
        (plist-put (plist-get model-context :options) :model "other-model")
        (should-not
         (e-harness--provider-compaction-candidate-compatible-p
          harness session-id model-context
          (gethash session-id
                   (e-harness-provider-compaction-candidates harness))
          (plist-get (plist-get model-context :options)
                     :context-capabilities))))
      (let ((layout-context (copy-tree context)))
        (plist-put (plist-get layout-context :options)
                   :instructions "changed layout")
        (should-not
         (e-harness--provider-compaction-candidate-compatible-p
          harness session-id layout-context
          (gethash session-id
                   (e-harness-provider-compaction-candidates harness))
          (plist-get (plist-get layout-context :options)
                     :context-capabilities))))
      (let ((generation-context (copy-tree context)))
        (plist-put generation-context :lifetime-generation
                   (e-context-lifetime-generation-create
                    :id "generation:replacement"
                    :checkpoint '((:role system :content "new"))
                    :covered-session-boundary source))
        (should-not
         (e-harness--provider-compaction-candidate-compatible-p
          harness session-id generation-context
          (gethash session-id
                   (e-harness-provider-compaction-candidates harness))
          (plist-get (plist-get generation-context :options)
                     :context-capabilities))))
      (let* ((reopened
              (e-harness-create
               :backend backend :sessions store
               :default-options '(:model "candidate-model"
                                  :provider-anchor-provider-id fake)))
             (reopened-context
              (e-harness-turn-context reopened session-id "reopened"))
             (reopened-options (plist-get reopened-context :options)))
        (should-not (plist-get reopened-options :provider-compaction-output))
        (should (string-match-p "portable source"
                                (prin1-to-string
                                 (plist-get reopened-context :messages))))))))

(ert-deftest e-harness-test-provider-compaction-candidate-is-one-shot ()
  "Preview does not consume opaque state; the actual turn consumes it once."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (backend
          (e-backend-fake-create
           :items nil
           :context-capabilities
           '(:continuation linear :provider-compaction opaque)
           :provider-compaction
           (lambda (&rest _args)
             '(:output ((:type "opaque" :marker "ONE-SHOT"))
               :usage nil))))
         (store (e-session-store-create))
         (harness
          (e-harness-create
           :backend backend
           :sessions store
           :default-options '(:model "one-shot-model"
                              :provider-anchor-provider-id fake
                              :provider-continuation t
                              :context-lifetime-enabled t)))
         (session-id "provider-one-shot"))
    (e-harness-create-session harness :id session-id)
    (e-harness-turn-context harness session-id "seed")
    (e-session-append-message store session-id
                              '(:role user :content "one-shot durable"))
    (let ((boundary (plist-get
                     (car (last (e-session-current-path store session-id)))
                     :id)))
      (e-session-append-context-generation
       store session-id
       (e-context-lifetime-generation-create
        :id "generation:one-shot"
        :checkpoint '((:role system :content "C0-ONE-SHOT"))
        :covered-session-boundary boundary)))
    (let* ((context (e-harness-turn-context harness session-id "capture"))
           (generation (plist-get context :lifetime-generation))
           (input (e-harness--provider-compaction-input
                   harness session-id generation)))
      (e-harness--provider-compaction-store-candidate
       harness session-id context generation
       '((:type "opaque" :marker "ONE-SHOT")) nil
       (plist-get input :source-entry-id)
       (plist-get input :promotion-frontier)
       (plist-get input :input-fingerprint)))
    (let ((preview (e-harness-context harness session-id nil 'preview)))
      (should-not (plist-get (plist-get preview :options)
                             :provider-compaction-output))
      (should (gethash session-id
                       (e-harness-provider-compaction-candidates harness))))
    (let* ((first (e-harness-turn-context harness session-id "first-turn"))
           (first-options (plist-get first :options))
           (head (plist-get
                  (car (last (e-session-current-path store session-id)))
                  :id)))
      (should (equal (plist-get first-options :provider-compaction-output)
                     '((:type "opaque" :marker "ONE-SHOT"))))
      (should-not (gethash session-id
                           (e-harness-provider-compaction-candidates harness)))
      ;; A normal compatible response anchor is eligible on the next turn;
      ;; the consumed opaque output is not replayed.
      (e-session-append-provider-anchor
       store session-id 'fake
       :model "one-shot-model"
       :covered-entry-id head
       :fingerprints (e-harness--provider-anchor-fingerprints first)
       :metadata '(:response-id "fresh-anchor"))
      (let* ((second (e-harness-turn-context harness session-id "second-turn"))
             (second-options (plist-get second :options)))
        (should-not (plist-get second-options :provider-compaction-output))
        (should (equal (plist-get
                        (plist-get second-options :provider-anchor)
                        :metadata)
                       '(:response-id "fresh-anchor")))))))

(ert-deftest e-harness-test-provider-compaction-async-generation-order-is-fenced ()
  "An older async compact result cannot replace a newer generation candidate."
  (let* ((callbacks nil)
         (backend
          (e-backend-fake-create
           :items nil
           :context-capabilities
           '(:continuation none :provider-compaction opaque)
           :provider-compaction
           (cl-function
            (lambda (&key on-done &allow-other-keys)
              (push on-done callbacks)
              (e-backend-request-create :metadata '(:async t))))))
         (store (e-session-store-create))
         (harness (e-harness-create :backend backend :sessions store
                                    :default-options
                                    '(:model "generation-order-model"
                                      :provider-anchor-provider-id fake)))
         (session-id "provider-generation-order"))
    (e-harness-create-session harness :id session-id)
    (let* ((session (e-session-get store session-id))
           (root (plist-get session :root-event-id))
           (generation-a
            (e-session-append-context-generation
             store session-id
             (e-context-lifetime-generation-create
              :id "generation:async-a"
              :checkpoint '((:role system :content "C-A"))
              :covered-session-boundary root))))
      (e-harness--maybe-provider-compaction-start
       harness session-id generation-a)
      (let* ((head-a (plist-get
                      (car (last (e-session-current-path store session-id)))
                      :id))
             (generation-b
              (e-session-append-context-generation
               store session-id
               (e-context-lifetime-generation-create
                :id "generation:async-b"
                :checkpoint '((:role system :content "C-B"))
                :covered-session-boundary head-a))))
        (e-harness--maybe-provider-compaction-start
         harness session-id generation-b)
        (should (= (length callbacks) 2))
        ;; PUSH stores B first.  A's callback arrives late and is fenced by
        ;; the current session generation check before any puthash.
        (funcall (car callbacks)
                 '(:output ((:type "opaque" :marker "RESULT-B"))
                   :usage nil))
        (funcall (cadr callbacks)
                 '(:output ((:type "opaque" :marker "RESULT-A"))
                   :usage nil))
        (should (equal
                 (plist-get
                  (gethash session-id
                           (e-harness-provider-compaction-candidates harness))
                  :output)
                 '((:type "opaque" :marker "RESULT-B"))))))))

(provide 'e-harness-test)

;;; e-harness-test.el ends here
