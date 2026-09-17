;;; e-actions-test.el --- Tests for e action dispatch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for context-bound capability action dispatch.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-actions)
(require 'e-backend)
(require 'e-action-resources)
(require 'e-async-control)
(require 'e-await-tool)
(require 'e-chat-session)
(require 'e-harness)
(require 'e-resources)
(require 'e-work)

(ert-deftest e-actions-test-call-chat-session-action ()
  "Action dispatch resolves active chat-session actions and injects context."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (e-actions-call
     'chat-session
     :rename
     '(:name "Renamed")
     (list :harness harness :session-id "session-1"))
    (should (equal (e-harness-session-title harness "session-1")
                   "Renamed"))))

(ert-deftest e-actions-test-work-action-preserves-immediate-result ()
  "Work-backed actions return a handle while preserving cheap result shape."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (e-work--detached-handles (make-hash-table :test 'equal))
         (capability
          (e-capability-create
           :id 'work-action
           :name "Work Action"
           :actions
           (list :run
                 (e-action-create
                  :parameters '(:type "object"
                                :properties (:value (:type "string"))
                                :required ["value"])
                  :work (e-work-spec-create
                         :id "action_work"
                         :execution 'cheap
                         :interactive-policy 'cheap
                         :runner (lambda (arguments _context)
                                   (plist-get arguments :value))))))))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (let ((dispatch (e-actions-dispatch
                     'work-action
                     :run
                     '(:value "done")
                     (list :harness harness :session-id "session-1"))))
      (should (e-work-handle-p (plist-get dispatch :request)))
      (should (equal (plist-get dispatch :result) "done"))
      (should (zerop (hash-table-count e-work--detached-handles))))))

(ert-deftest e-actions-test-rejects-raw-function-action-spec ()
  "Action dispatch no longer wraps raw function actions as compatibility."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (called nil)
         (capability
          (e-capability-create
           :id 'legacy-action
           :name "Legacy Action"
           :actions (list :run (lambda (_arguments)
                                 (setq called t))))))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-actions-dispatch
      'legacy-action
      :run
      nil
      (list :harness harness :session-id "session-1"))
     :type 'e-actions-invalid-spec)
    (should-not called)))

(ert-deftest e-actions-test-immediate-work-failure-surfaces ()
  "Immediate work-backed action failures are not returned as started work."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (capability
          (e-capability-create
           :id 'failing-action
           :name "Failing Action"
           :actions
           (list :run
                 (e-action-cheap-create
                  :runner (lambda (_arguments _context)
                            (user-error "failed immediately"))))))
         failed-events)
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-actions-call
      'failing-action
      :run
      nil
      (list :harness harness :session-id "session-1" :turn-id "turn-1"))
     :type 'user-error)
    (setq failed-events
          (cl-remove-if-not
           (lambda (event)
             (eq (plist-get event :event-type) 'action-failed))
           (e-session-local-activity-events
            (e-harness-sessions harness) "session-1")))
    (should (equal (length failed-events) 1))))

(ert-deftest e-actions-test-call-validates-required-arguments ()
  "Action dispatch reports missing descriptor-required arguments."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-actions-call
      'chat-session
      :rename
      nil
      (list :harness harness :session-id "session-1"))
     :type 'e-actions-invalid-arguments)))

(ert-deftest e-actions-test-call-rejects-noncanonical-argument-containers ()
  "Action dispatch rejects stringified objects and alternate containers."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (called nil)
         (capability
          (e-capability-create
           :id 'strict-action
           :actions
           (list :run
                 (e-action-cheap-create
                  :parameters '(:type "object"
                                :properties (:name (:type "string"))
                                :required ["name"])
                  :runner (lambda (_arguments _context)
                            (setq called t)
                            "ok"))))))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (dolist (arguments
             (list '(("name" . "string-alist"))
                   (let ((table (make-hash-table :test #'equal)))
                     (puthash "name" "hash" table)
                     table)
                   "{\"name\":\"stringified\"}"))
      (should-error
       (e-actions-call 'strict-action :run arguments
                       (list :harness harness :session-id "session-1"))
       :type 'e-actions-invalid-arguments))
    (should-not called)))

(ert-deftest e-actions-test-call-preserves-canonical-array-of-objects ()
  "Canonical vectors of objects reach the runner without reshaping."
  (let* ((seen nil)
         (harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (parameters
          '(:type "object"
            :properties
            (:sections (:type "array"
                        :items (:type "object"
                                :properties
                                (:title (:type "string")
                                 :metadata (:type "object")
                                 :enabled (:type "boolean")))))))
         (capability
          (e-capability-create
           :id 'array-action
           :actions
           (list :run
                 (e-action-cheap-create
                  :parameters parameters
                  :runner (lambda (arguments _context)
                            (setq seen arguments)
                            "ok"))))))
    (e-harness-activate-capability harness capability)
    (let ((sections
           [(:title "first"
             :metadata (:source "plist")
             :enabled :json-false)
            (:title "second"
             :metadata nil
             :enabled t)]))
      (should
       (equal
        (e-actions-call 'array-action :run (list :sections sections)
                        (list :harness harness))
        "ok")))
    (should
     (equal seen
            '(:sections [(:title "first"
                          :metadata (:source "plist")
                          :enabled :json-false)
                         (:title "second"
                          :metadata nil
                          :enabled t)])))))

(ert-deftest e-actions-test-rejects-list-of-plist-array-before-runner ()
  "The old list-of-plists action shape is rejected before execution."
  (let* ((called nil)
         (harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (parameters
          '(:type "object"
            :properties (:sections (:type "array"
                                       :items (:type "object"
                                               :properties
                                               (:title (:type "string")))))
            :required ["sections"]))
         (capability
          (e-capability-create
           :id 'array-action
           :actions
           (list :run
                 (e-action-cheap-create
                  :parameters parameters
                  :runner (lambda (_arguments _context)
                            (setq called t)
                            "ok"))))))
    (e-harness-activate-capability harness capability)
    (should-error
     (e-actions-call 'array-action :run
                     '(:sections ((:title "first") (:title "second")))
                     (list :harness harness))
     :type 'e-actions-invalid-arguments)
    (should-not called)))

(ert-deftest e-actions-test-call-preserves-vector-of-plist-array ()
  "Canonical vector arrays keep each object and sentinel unchanged."
  (let* ((seen nil)
         (harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (parameters
          '(:type "object"
            :properties
            (:sections (:type "array"
                        :items (:type "object"
                                :properties
                                (:title (:type "string")
                                 :metadata (:type "object")
                                 :enabled (:type "boolean")))))))
         (capability
          (e-capability-create
           :id 'vector-array-action
           :actions
           (list :run
                 (e-action-cheap-create
                  :parameters parameters
                  :runner (lambda (arguments _context)
                            (setq seen arguments)
                            "ok"))))))
    (e-harness-activate-capability harness capability)
    (let ((sections
           [(:title "first"
             :metadata (:source "vector-plist")
             :enabled :json-false)
            (:title "second"
             :metadata (:source "vector-plist-2")
             :enabled t)]))
      (should
       (equal
        (e-actions-call 'vector-array-action :run (list :sections sections)
                        (list :harness harness))
        "ok")))
    (should
     (equal seen
            '(:sections [(:title "first"
                          :metadata (:source "vector-plist")
                          :enabled :json-false)
                         (:title "second"
                          :metadata (:source "vector-plist-2")
                          :enabled t)])))))

(ert-deftest e-actions-test-call-preserves-nested-canonical-objects ()
  "Canonical nested objects, arrays, and sentinels reach the runner unchanged."
  (let* ((seen nil)
         (harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (parameters
          '(:type "object"
            :properties
            (:object (:type "object")
             :array (:type "array" :items (:type "object"))
             :false (:type "boolean")
             :null (:type "null"))))
         (capability
          (e-capability-create
           :id 'nested-object-action
           :actions
           (list :run
                 (e-action-cheap-create
                  :parameters parameters
                  :runner (lambda (arguments _context)
                            (setq seen arguments)
                            "ok"))))))
    (e-harness-activate-capability harness capability)
    (let ((arguments
           '(:object (:source "plist")
             :array [(:source "array")]
             :false :json-false
             :null :json-null)))
      (should (equal (e-actions-call 'nested-object-action :run arguments
                                     (list :harness harness))
                     "ok"))
      (should (equal seen arguments)))))

(ert-deftest e-actions-test-call-uses-current-tool-context ()
  "Action dispatch uses `e-tools-current-context' when options omit context."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (e-tools--current-context
          (list :harness harness :session-id "session-1")))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (e-actions-call 'chat-session :rename '(:name "Context renamed"))
    (should (equal (e-harness-session-title harness "session-1")
                   "Context renamed"))))

(ert-deftest e-actions-test-async-action-returns-awaitable-work-reference ()
  "A pending action returns a generic reference whose await yields its result."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (e-work--detached-handles (make-hash-table :test 'equal))
         (e-waitable--resolvers (make-hash-table :test 'equal))
         finish
         (capability
          (e-capability-create
           :id 'async-capability
           :actions
           (list
            :run
            (e-action-create
             :requires-session t
             :parameters '(:type "object"
                           :properties (:value (:type "string"))
                           :required ["value"])
             :work (e-work-spec-create
                    :id "async_action"
                    :execution 'cooperative
                    :interactive-policy 'async
                    :runner
                    (lambda (handle arguments _context)
                      (setq finish (lambda ()
                                     (e-work-finish
                                      handle
                                      (list :echo
                                            (plist-get arguments :value)))))
                      :deferred)))))))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (e-async-control-register-work-resolver)
    (let ((result
           (e-actions-call
            'async-capability
            :run
            '(:value "later")
            (list :harness harness :session-id "session-1" :turn-id "turn-1"))))
      (should (string-match-p "\\`work:" result))
      (should (e-work-handle-p
               (plist-get (e-waitable-resolve result) :handle)))
      (should (functionp finish))
      (should-not
       (cl-find 'action-finished
                (e-session-local-activity-events
                 (e-harness-sessions harness) "session-1")
                :key (lambda (event) (plist-get event :event-type))))
      (let ((registry (e-tools-registry-create))
            awaited)
        (e-await-tool-register registry)
        (e-tools-start
         registry
         (list :id "await-action" :name "await"
               :arguments (list :refs (vector result)))
         :on-done (lambda (value) (setq awaited value)))
        (should-not awaited)
        (funcall finish)
        (let* ((content (plist-get awaited :content))
               (entry (aref (plist-get content :results) 0)))
          (should (plist-get content :settled))
          (should (equal (plist-get entry :state) "finished"))
          (should (equal (plist-get entry :result) '(:echo "later"))))))
    (let ((finished
           (cl-find 'action-finished
                    (e-session-local-activity-events
                     (e-harness-sessions harness) "session-1")
                    :key (lambda (event) (plist-get event :event-type)))))
      (should finished)
      (should (string-match-p
               "later"
               (plist-get (plist-get (plist-get finished :payload) :result)
                          :content))))))


(ert-deftest e-actions-test-action-description-resources-read-glob-search ()
  "Action descriptions are exposed as read-only e-action:// resources."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-action-resources-capability-create))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (let ((resources (e-harness-resources harness "session-1" "turn-1")))
      (should (string-match-p
               "e-action://chat-session"
               (e-resources-read resources "e-action://active" nil)))
      (should (string-match-p
               "rename"
               (e-resources-read resources "e-action://chat-session" nil)))
      (should (string-match-p
               "(e-actions-call 'chat-session :rename ARGUMENTS)"
               (e-resources-read resources "e-action://chat-session/rename" nil)))
      (should (string-match-p
               "Execution: cheap"
               (e-resources-read resources "e-action://chat-session/rename" nil)))
      (should (string-match-p
               "Execution: asynchronous-capable"
               (e-resources-read resources "e-action://chat-session/compact" nil)))
      (should (string-match-p
               "returns a work: reference"
               (e-resources-read resources "e-action://chat-session/compact" nil)))
      (let ((listed (e-resources-glob resources "e-action://" "chat-session/ren*" nil t)))
        (should (equal (mapcar (lambda (record) (plist-get record :uri))
                               (append (plist-get listed :resources) nil))
                       '("e-action://chat-session/rename"))))
      (let ((matches (e-resources-search resources "e-action://" "rename" nil)))
        (should (< 0 (length (plist-get matches :matches))))))))

(ert-deftest e-actions-test-dynamic-action-capability-duplicate-id-signals ()
  "Dynamic action expansion rejects ambiguous capability ids."
  (let* ((action
          (e-action-cheap-create
           :runner (lambda (_arguments _context) "ok")))
         (ordinary
          (e-capability-create
           :id 'duplicate-action
           :actions (list :run action)))
         (provided
          (e-capability-create
           :id 'duplicate-action
           :actions (list :run action)))
         (host
          (e-capability-create
           :id 'provider-host
           :action-capability-providers
           (list (lambda (&rest _context) (list provided)))))
         (harness (e-harness-create :enabled-layer-ids nil)))
    (e-harness-set-intrinsic-capabilities
     harness
     (list ordinary host (e-action-resources-capability-create)))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-harness-effective-action-capabilities harness)
     :type 'e-harness-duplicate-action-capability)
    (should-error
     (e-actions-call
      'duplicate-action :run nil
      (list :harness harness :session-id "session-1"))
     :type 'e-harness-duplicate-action-capability)
    (let ((resources (e-harness-resources harness "session-1" "turn-1")))
      (dolist (uri '("e-action://active"
                     "e-action://duplicate-action"
                     "e-action://duplicate-action/run"))
        (should-error
         (e-resources-read resources uri nil)
         :type 'e-harness-duplicate-action-capability)))))

(ert-deftest e-actions-test-rejected-arguments-never-enter-activity ()
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (parameters
          '(:type "object"
            :properties (:allowed (:type "string"))
            :additionalProperties :json-false))
         (capability
          (e-capability-create
           :id 'exact-action
           :actions
           (list :run
                 (e-action-cheap-create
                  :parameters parameters
                  :runner (lambda (_arguments _context) "ok"))))))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-actions-call 'exact-action :run
                     '(:allowed "ok" :extra "must-not-retain")
                     (list :harness harness :session-id "session-1"
                           :turn-id "turn-1"))
     :type 'e-actions-invalid-arguments)
    (let ((serialized
           (prin1-to-string
            (e-session-local-activity-events
             (e-harness-sessions harness) "session-1"))))
      (should-not (string-match-p "must-not-retain\\|:extra" serialized))
      (should-not (string-match-p "allowed" serialized)))))

(ert-deftest e-actions-test-failed-activity-redacts-error-message ()
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (capability
          (e-capability-create
           :id 'failing-secret-action
           :actions
           (list :run
                 (e-action-cheap-create
                  :runner (lambda (_arguments _context)
                            (user-error "token=super-secret action failed")))))))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-actions-call 'failing-secret-action :run nil
                     (list :harness harness :session-id "session-1"
                           :turn-id "turn-1")))
    (let* ((events (e-session-local-activity-events
                    (e-harness-sessions harness) "session-1"))
           (failed (seq-find (lambda (event)
                               (eq (plist-get event :event-type) 'action-failed))
                             events))
           (serialized (prin1-to-string failed)))
      (should (string-match-p "REDACTED" serialized))
      (should-not (string-match-p "super-secret" serialized)))))

(provide 'e-actions-test)

;;; e-actions-test.el ends here
