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
(require 'e-capabilities)
(require 'e-context)
(require 'e-harness)
(require 'e-layers)
(require 'e-operations)
(require 'e-resources)
(require 'e-skills)
(require 'e-store)

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
               (lambda (_harness) tools)))
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
      (should (equal (mapcar (lambda (message) (plist-get message :role))
                             (e-harness-messages harness "session-1"))
                     '(user tool-call))))))

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
          (e-capability-create
           :id 'skill-capability
           :resources
           (list (lambda (store capability)
                   (e-skills-register
                    store
                    (e-capability-id capability)
                    (e-skill-create
                     :name "focused-work"
                     :description "Use for focused work."
                     :content "Stay focused."))))))
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

(ert-deftest e-harness-test-skill-catalog-enters-context-without-full-content ()
  "Context advertises active skill metadata, not full skill instructions."
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
          (e-capability-create
           :id 'skill-capability
           :resources
           (list (lambda (store capability)
                   (e-skills-register
                    store
                    (e-capability-id capability)
                    (e-skill-create
                     :name "review"
                     :description "Review implementation changes."
                     :content "Secret detailed review checklist."))
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
    (let ((catalog (seq-find
                    (lambda (message)
                      (string-match-p "Available skills"
                                      (plist-get message :content)))
                    captured-messages)))
      (should catalog)
      (should (string-match-p "review" (plist-get catalog :content)))
      (should (string-match-p "Review implementation changes"
                              (plist-get catalog :content)))
      (should (string-match-p "e://skill-capability/skills/review"
                              (plist-get catalog :content)))
      (should-not (string-match-p "Secret detailed review checklist"
                                  (plist-get catalog :content)))
      (should-not (string-match-p "review.md"
                                  (plist-get catalog :content))))))

(ert-deftest e-harness-test-built-in-read-loads-skill-resource ()
  "The built-in read tool can load full skill instructions on demand."
  (let* ((capability
          (e-capability-create
           :id 'skill-capability
           :resources
           (list (lambda (store capability)
                   (e-skills-register
                    store
                    (e-capability-id capability)
                    (e-skill-create
                     :name "planner"
                     :description "Plan work."
                     :content "Full planning instructions."))))))
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
          (e-capability-create
           :id 'skill-capability
           :resources
           (list (lambda (store capability)
                   (e-skills-register
                    store
                    (e-capability-id capability)
                    (e-skill-create
                     :name "readonly"
                     :description "Read-only skill."
                     :content "Read-only instructions."))))))
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
                     '(turn-started reasoning-delta turn-finished))))))

(provide 'e-harness-test)

;;; e-harness-test.el ends here
