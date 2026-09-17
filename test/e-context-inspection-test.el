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
(require 'e-json)
(require 'e-resources)
(require 'e-tools)

(defun e-context-inspection-test--finished-work (result)
  "Return a request-scoped work handle already finished with RESULT."
  (let ((work
         (e-work-prepare
          (e-work-spec-create
           :id "context-inspection-test-result"
           :execution 'cooperative
           :interactive-policy 'async
           :owner 'context-inspection-test
           :runner (lambda (&rest _arguments) :deferred))
          nil)))
    (e-work-start-prepared work :arguments nil)
    (e-work-finish work result)
    work))

(defun e-context-inspection-test--await (work)
  "Return terminal result from WORK at the explicit test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 1.0)))

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

(ert-deftest e-context-inspection-test-recent-failures-schema-matches-boundary ()
  "The public failure-page schema advertises the enforced 1..32 interval."
  (let* ((capability (e-context-inspection-capability-create))
         (action (plist-get (e-capability-actions capability) :recent-failures))
         (limit (plist-get (plist-get (e-action-parameters action) :properties)
                           :limit)))
    (should (= (plist-get limit :minimum) 1))
    (should (= (plist-get limit :maximum) 32))))

(ert-deftest e-context-inspection-test-dev-layer-contains-context-inspection ()
  "The e-dev layer packages context-inspection."
  (let ((layer (e-dev-layer-create)))
    (should (eq (e-layer-id layer) 'e-dev))
    (should (equal (mapcar #'e-capability-id (e-layer-capabilities layer))
                   '(context-inspection e-dev)))))

(ert-deftest e-context-inspection-test-export-default-pre-prompt-context ()
  "export-context writes pre-prompt context to a resource and returns metadata."
  (let (seen-purpose)
    (let* ((store (e-session-store-create))
           (context-provider
            (e-context-provider-create
             :name 'test-context
             :build (cl-function
                     (lambda (&key context-purpose &allow-other-keys)
                       (setq seen-purpose context-purpose)
                       '((:role system :content "provider context"))))))
           (harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions store
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
        (should (equal (plist-get metadata :mode) "pre-prompt"))
        (should (equal (plist-get metadata :message-count) 4))
        (should (eq seen-purpose 'preview))
        (should (string-match-p "capability instructions" content))
        (should (string-match-p "mark-reload-required" content))
        (should (string-match-p "provider context" content))
        (should-not (string-match-p "existing prompt" content))))))

(ert-deftest e-context-inspection-test-export-full-context-when-requested ()
  "export-context can include transcript messages when explicitly requested."
  (let (seen-purpose)
    (let* ((store (e-session-store-create))
           (context-provider
            (e-context-provider-create
             :name 'test-full-context
             :build (cl-function
                     (lambda (&key context-purpose &allow-other-keys)
                       (setq seen-purpose context-purpose)
                       nil))))
           (harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions store
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
        (should (equal (plist-get metadata :mode) "full"))
        (should (equal (plist-get metadata :message-count) 4))
        (should (eq seen-purpose 'preview))
        (should (string-match-p "system guidance" content))
        (should (string-match-p "mark-reload-required" content))
        (should (string-match-p "existing prompt" content))
        (should-not (string-match-p "Export metadata" content))))))

(ert-deftest e-context-inspection-test-recent-failures-finds-turn-failed ()
  "Recent failure inspection returns detached SQLite query evidence."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (cl-letf (((symbol-function 'e-session-async-recent-failures)
               (lambda (seen-store &rest options)
                 (should (eq seen-store store))
                 (should (= (plist-get options :limit) 10))
                 (e-context-inspection-test--finished-work
                  '(:failures
                    ((:session-id "session-1" :turn-id "turn-1"
                      :error "provider failed" :details (:status 520))))))))
      (let ((failures
             (e-context-inspection-test--await
              (e-context-inspection-recent-failures-start
               :harness harness))))
        (should (= (length failures) 1))
        (should (equal (plist-get (car failures) :session-id) "session-1"))
        (should (equal (plist-get (car failures) :turn-id) "turn-1"))
        (should (equal (plist-get (car failures) :error) "provider failed"))
        (should (equal (plist-get (car failures) :details) '(:status 520)))))))

(ert-deftest e-context-inspection-test-recent-failures-enforces-both-boundaries ()
  "Failure inspection accepts 1 and 32 while rejecting adjacent values."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         seen)
    (cl-letf (((symbol-function 'e-session-async-recent-failures)
               (lambda (_store &rest options)
                 (push (plist-get options :limit) seen)
                 (e-context-inspection-test--finished-work '(:failures nil)))))
      (dolist (limit '(1 32))
        (should-not
         (e-context-inspection-test--await
          (e-context-inspection-recent-failures-start
           :harness harness :limit limit)))))
    (should (equal seen '(32 1)))
    (dolist (limit '(0 33))
      (should-error
       (e-context-inspection-recent-failures-start
        :harness harness :limit limit)
       :type 'e-context-inspection-invalid))))

(ert-deftest e-context-inspection-test-failure-action-flattens-async-query-work ()
  "The public failure action settles with the query value, not a nested Work."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-dev-layer-create))
                   :sessions store)))
    (cl-letf (((symbol-function 'e-session-async-recent-failures)
               (lambda (&rest _arguments)
                 (e-context-inspection-test--finished-work
                  '(:failures ((:session-id "session-1"
                                :turn-id "turn-1"))))))
              ((symbol-function 'e-runtime-store-call)
               (lambda (&rest _arguments)
                 (error "failure inspection reached synchronous storage"))))
      (should
       (equal
        (e-actions-call 'context-inspection :recent-failures nil
                        (list :harness harness))
        [(:session-id "session-1" :turn-id "turn-1"
          :error :json-null :details :json-null)])))))

(ert-deftest e-context-inspection-test-failure-detail-includes-turn-timeline ()
  "Failure detail maps a detached bounded SQLite turn timeline."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (let ((query
           '(:present t
             :session (:id "session-1" :name "Broken"
                       :metadata (:project-root "/tmp/project/"))
             :turn-id "turn-1"
             :messages
             ((:id "msg-1" :role user :content "please debug"
               :turn-id "turn-1")
              (:id "call-msg" :role tool-call
               :content (:type "tool-call" :id "call-1" :name "read"
                         :arguments (:uri "file://broken"))
               :turn-id "turn-1")
              (:id "tool-msg" :role tool
               :content (:tool-call-id "call-1" :name "read" :status ok
                         :content "tool output")
               :turn-id "turn-1"))
             :events
             ((:id "evt-1" :turn-id "turn-1"
               :event-type provider-request-started)
              (:id "evt-2" :turn-id "turn-1" :event-type tool-started)
              (:id "evt-3" :turn-id "turn-1" :event-type turn-failed
               :payload (:error "provider returned HTML"
                         :details (:response-kind html :preview "520")))))))
      (cl-letf (((symbol-function 'e-session-async-turn-inspection)
                 (lambda (seen-store session-id turn-id)
                   (should (eq seen-store store))
                   (should (equal session-id "session-1"))
                   (should (equal turn-id "turn-1"))
                   (e-context-inspection-test--finished-work query))))
        (let ((detail
               (e-context-inspection-test--await
                (e-context-inspection-failure-detail-start
                 :harness harness
                 :session-id "session-1"
                 :turn-id "turn-1"))))
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
                            (plist-get detail :events))))))))

(ert-deftest e-context-inspection-test-failure-detail-rejects-unknown-turn ()
  "Failure detail errors when the requested turn has no terminal failure."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (cl-letf (((symbol-function 'e-session-async-turn-inspection)
               (lambda (&rest _arguments)
                 (e-context-inspection-test--finished-work
                  '(:present t :session (:id "session-1")
                    :events nil :messages nil)))))
      (should-error
       (e-context-inspection-test--await
        (e-context-inspection-failure-detail-start
         :harness harness
         :session-id "session-1"
         :turn-id "missing-turn"))
       :type 'e-context-inspection-invalid))))

(ert-deftest e-context-inspection-test-raw-provider-preview-unavailable ()
  "Raw provider preview returns an explicit unavailable shape by default."
  (let ((preview (e-context-inspection-raw-provider-preview)))
    (should (eq (plist-get preview :available) e-json-false))
    (should (equal (plist-get preview :source) "unavailable"))))

(provide 'e-context-inspection-test)

;;; e-context-inspection-test.el ends here
