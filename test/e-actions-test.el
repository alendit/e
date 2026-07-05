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
         (capability
          (e-capability-create
           :id 'work-action
           :name "Work Action"
           :actions
           (list :run
                 (e-action-create
                  :parameters nil
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
      (should (equal (plist-get dispatch :result) "done")))))

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
           (e-session-activity-events
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

(ert-deftest e-actions-test-call-normalizes-string-names-and-arguments ()
  "Action dispatch accepts JSON-like string names and argument keys."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (arguments (make-hash-table :test #'equal)))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (puthash "name" "String renamed" arguments)
    (e-actions-call
     "chat-session"
     ":rename"
     arguments
     (list :harness harness :session-id "session-1"))
    (should (equal (e-harness-session-title harness "session-1")
                   "String renamed"))))

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

(ert-deftest e-actions-test-async-action-starts-before-callback ()
  "Async action dispatch returns a started result before callback settlement."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         finish
         (capability
          (e-capability-create
           :id 'async-capability
           :actions
           (list
            :run
            (e-action-create
             :requires-session t
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
    (let ((result
           (e-actions-call
            'async-capability
            :run
            '(:value "later")
            (list :harness harness :session-id "session-1" :turn-id "turn-1"))))
      (should (eq (plist-get result :status) 'started))
      (should (functionp finish))
      (should-not
       (cl-find 'action-finished
                (e-session-activity-events
                 (e-harness-sessions harness) "session-1")
                :key (lambda (event) (plist-get event :event-type)))))
    (funcall finish)
    (let ((finished
           (cl-find 'action-finished
                    (e-session-activity-events
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
      (let ((listed (e-resources-glob resources "e-action://" "chat-session/ren*" nil t)))
        (should (equal (mapcar (lambda (record) (plist-get record :uri))
                               (append (plist-get listed :resources) nil))
                       '("e-action://chat-session/rename"))))
      (let ((matches (e-resources-search resources "e-action://" "rename" nil)))
        (should (< 0 (length (plist-get matches :matches))))))))

(provide 'e-actions-test)

;;; e-actions-test.el ends here
