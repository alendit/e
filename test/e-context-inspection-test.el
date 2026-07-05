;;; e-context-inspection-test.el --- Tests for context export tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the e-dev context inspection capability.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-context-inspection)
(require 'e-dev-layer)
(require 'e-harness)
(require 'e-harness-base)
(require 'e-resources)
(require 'e-tools)

(ert-deftest e-context-inspection-test-capability-registers-actions ()
  "The context-inspection capability exposes context and error actions."
  (let ((capability (e-context-inspection-capability-create)))
    (should (eq (e-capability-id capability) 'context-inspection))
    (should (equal (cl-loop for (key _value) on (e-capability-actions capability)
                            by #'cddr
                            collect key)
                   '(:export-context
                     :recent-failures
                     :failure-detail
                     :raw-provider-preview)))))

(ert-deftest e-context-inspection-test-dev-layer-contains-context-inspection ()
  "The e-dev layer packages context-inspection."
  (let ((layer (e-dev-layer-create)))
    (should (eq (e-layer-id layer) 'e-dev))
    (should (equal (mapcar #'e-capability-id (e-layer-capabilities layer))
                   '(context-inspection e-dev)))))

(ert-deftest e-context-inspection-test-export-default-pre-prompt-context ()
  "export-context writes pre-prompt context to a resource and returns metadata."
  (let (seen-purpose)
    (let* ((context-provider
            (e-context-provider-create
             :name 'test-context
             :build (cl-function
                     (lambda (&key context-purpose &allow-other-keys)
                       (setq seen-purpose context-purpose)
                       '((:role system :content "provider context"))))))
           (harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :intrinsic-capabilities
                     (append
                      (list
                       (e-capability-create
                        :id 'context-capability
                        :instructions "capability instructions"
                        :context-providers (list context-provider)))
                      (e-layer-capabilities (e-harness-base-layer-create))
                      (e-layer-capabilities (e-dev-layer-create))))))
      (e-harness-create-session harness :id "session-1")
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:id "msg-1" :role user :content "existing prompt"))
      (let* ((metadata (e-actions-call
                        'context-inspection
                        :export-context
                        (list :uri "tmp://default_context.md")
                        (list :harness harness
                              :session-id "session-1"
                              :turn-id "turn-1")))
             (content (e-resources-read
                       (e-harness-resources harness "session-1" "turn-1")
                       "tmp://default_context.md")))
        (should (equal (plist-get metadata :uri) "tmp://default_context.md"))
        (should (eq (plist-get metadata :mode) 'pre-prompt))
        (should (equal (plist-get metadata :message-count) 4))
        (should (eq seen-purpose 'preview))
        (should (string-match-p "capability instructions" content))
        (should (string-match-p "mark-reload-required" content))
        (should (string-match-p "provider context" content))
        (should-not (string-match-p "existing prompt" content))))))

(ert-deftest e-context-inspection-test-export-full-context-when-requested ()
  "export-context can include transcript messages when explicitly requested."
  (let (seen-purpose)
    (let* ((context-provider
            (e-context-provider-create
             :name 'test-full-context
             :build (cl-function
                     (lambda (&key context-purpose &allow-other-keys)
                       (setq seen-purpose context-purpose)
                       nil))))
           (harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :intrinsic-capabilities
                     (append
                      (list (e-capability-create
                             :id 'instructions-capability
                             :instructions "system guidance"
                             :context-providers (list context-provider)))
                      (e-layer-capabilities (e-harness-base-layer-create))
                      (e-layer-capabilities (e-dev-layer-create))))))
      (e-harness-create-session harness :id "session-1")
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:id "msg-1" :role user :content "existing prompt"))
      (let* ((metadata (e-actions-call
                        'context-inspection
                        :export-context
                        (list :uri "tmp://context.md"
                              :include_transcript t
                              :include_metadata :json-false)
                        (list :harness harness
                              :session-id "session-1"
                              :turn-id "turn-1")))
             (content (e-resources-read
                       (e-harness-resources harness "session-1" "turn-1")
                       "tmp://context.md")))
        (should (eq (plist-get metadata :mode) 'full))
        (should (equal (plist-get metadata :message-count) 4))
        (should (eq seen-purpose 'preview))
        (should (string-match-p "system guidance" content))
        (should (string-match-p "mark-reload-required" content))
        (should (string-match-p "existing prompt" content))
        (should-not (string-match-p "Export metadata" content))))))

(ert-deftest e-context-inspection-test-recent-failures-finds-turn-failed ()
  "Recent failure inspection lists failed turns newest first."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     store "session-1"
     '(:id "msg-1" :role user :content "broken prompt" :turn-id "turn-1"))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'turn-failed
     '(:error "provider failed" :details (:status 520)))
    (let ((failures (e-context-inspection-recent-failures
                     :harness harness)))
      (should (= (length failures) 1))
      (should (equal (plist-get (car failures) :session-id) "session-1"))
      (should (equal (plist-get (car failures) :turn-id) "turn-1"))
      (should (equal (plist-get (car failures) :error) "provider failed"))
      (should (equal (plist-get (car failures) :details) '(:status 520))))))

(ert-deftest e-context-inspection-test-failure-detail-includes-turn-timeline ()
  "Failure detail includes prompt, provider lifecycle, tools, and terminal error."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (e-harness-create-session harness :id "session-1"
                              :metadata '(:project-root "/tmp/project/"))
    (e-session-append-message
     store "session-1"
     '(:id "msg-1" :role user :content "please debug" :turn-id "turn-1"))
    (e-session-append-message
     store "session-1"
     '(:id "call-msg" :role tool-call
       :content (:type "tool-call" :id "call-1" :name "read"
                 :arguments (:uri "file://broken"))
       :turn-id "turn-1"))
    (e-session-append-message
     store "session-1"
     '(:id "tool-msg" :role tool
       :content (:tool-call-id "call-1" :name "read" :status ok
                 :content "tool output")
       :turn-id "turn-1"))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'provider-request-started
     '(:provider codex :url-path "/backend-api/codex/responses"))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'tool-started
     '(:type "tool-call" :id "call-1" :name "read"))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'tool-finished
     '(:tool-call (:id "call-1" :name "read")
       :result (:status ok :content "tool output")))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'turn-failed
     '(:error "provider returned HTML"
       :details (:response-kind html :preview "520")))
    (let ((detail (e-context-inspection-failure-detail
                   :harness harness
                   :session-id "session-1"
                   :turn-id "turn-1")))
      (should (equal (plist-get (plist-get detail :session) :id)
                     "session-1"))
      (should (equal (plist-get (plist-get detail :session) :project-root)
                     "/tmp/project/"))
      (should (equal (plist-get (plist-get detail :turn) :id) "turn-1"))
      (should (equal (plist-get (plist-get detail :terminal-error) :error)
                     "provider returned HTML"))
      (should (= (length (plist-get detail :messages)) 3))
      (should (= (length (plist-get detail :tool-calls)) 1))
      (should (seq-find (lambda (event)
                          (eq (plist-get event :event-type)
                              'provider-request-started))
                        (plist-get detail :events))))))

(ert-deftest e-context-inspection-test-failure-detail-rejects-unknown-turn ()
  "Failure detail errors when the requested turn has no terminal failure."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-context-inspection-failure-detail
      :harness harness
      :session-id "session-1"
      :turn-id "missing-turn")
     :type 'e-context-inspection-invalid)))

(ert-deftest e-context-inspection-test-raw-provider-preview-unavailable ()
  "Raw provider preview returns an explicit unavailable shape by default."
  (let ((preview (e-context-inspection-raw-provider-preview)))
    (should-not (plist-get preview :available))
    (should (equal (plist-get preview :source) "unavailable"))))

(provide 'e-context-inspection-test)

;;; e-context-inspection-test.el ends here
