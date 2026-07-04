;;; e-work-e2e-test.el --- Work API e2e tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic end-to-end tests for work-backed tool execution through the
;; async loop.  These tests avoid live providers and assert API shape rather
;; than elapsed-time thresholds.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-loop)
(require 'e-tools)
(require 'e-work)

(defun e-work-e2e--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(ert-deftest e-work-e2e-test-work-backed-tool-round-trip-is-async ()
  "A work-backed tool returns a handle, settles later, and feeds follow-up."
  (let* ((calls 0)
         (backend
          (e-backend-create
           :name "work-tool-round-trip"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                          on-request-start)
              (ignore options on-error on-request-start)
              (setq calls (1+ calls))
              (run-at-time
               0 nil
               (lambda ()
                 (if (= calls 1)
                     (progn
                       (should (equal (mapcar (lambda (message)
                                                (plist-get message :role))
                                              messages)
                                      '(user)))
                       (funcall on-item
                                '(:type tool-call
                                  :id "work-call-1"
                                  :name "work_echo"
                                  :arguments (:text "state" :delay 0.5)))
                       (funcall on-item '(:type done :reason tool-use)))
                   (should (equal (mapcar (lambda (message)
                                            (plist-get message :role))
                                          messages)
                                  '(user tool-call tool)))
                   (let ((tool-result (plist-get (nth 2 messages) :content)))
                     (should (equal (plist-get tool-result :tool-call-id)
                                    "work-call-1"))
                     (should (equal (plist-get tool-result :content)
                                    "fresh state/work-call-1")))
                   (funcall on-item
                            '(:type assistant-message
                              :content "final after work"))
                   (funcall on-item '(:type done :reason stop)))
                 (funcall on-done '(:status done))))
              nil))))
         (tools (e-tools-registry-create))
         (messages nil)
         (events nil)
         (settled nil)
         (error nil)
         (requests nil))
    (e-tools-register
     tools
     :name "work_echo"
     :description "Echo state through work."
     :work (e-work-spec-create
            :id "work_echo"
            :description "E2E work echo."
            :execution 'render
            :interactive-policy 'async
            :owner 'e-work-e2e
            :runner (lambda (arguments context)
                      (format "fresh %s/%s"
                              (plist-get arguments :text)
                              (plist-get
                               (plist-get context :tool-call)
                               :id)))))
    (e-loop-start-turn
     :session-id "session-work"
     :turn-id "turn-work"
     :messages '((:role user :content "inspect through work"))
     :backend backend
     :tools tools
     :options nil
     :on-request-start (lambda (request)
                         (push request requests))
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message (lambda (message)
                       (push message messages))
     :on-done (lambda (result)
                (setq settled result))
     :on-error (lambda (err)
                 (setq error err)))
    (should (null settled))
    (should (null error))
    (should (e-work-e2e--wait-until
             (lambda ()
               (cl-find-if
                (lambda (request)
                  (and (e-tools-request-p request)
                       (eq (plist-get (e-tools-request-metadata request)
                                      :transport)
                           'work)))
                requests))))
    (let* ((work-request
            (cl-find-if
             (lambda (request)
               (and (e-tools-request-p request)
                    (eq (plist-get (e-tools-request-metadata request)
                                   :transport)
                        'work)))
             requests))
           (handle (plist-get (e-tools-request-metadata work-request)
                              :work-handle)))
      (should (e-work-handle-p handle))
      (should (equal (plist-get (e-work-status handle) :state) 'started)))
    (should (null settled))
    (should (e-work-e2e--wait-until (lambda () settled) 2.0))
    (should (null error))
    (should (equal calls 2))
    (should (equal (plist-get settled :status) 'done))
    (should (equal (mapcar (lambda (message)
                             (plist-get message :role))
                           (nreverse messages))
                   '(tool-call tool assistant)))
    (should (equal (mapcar (lambda (event)
                             (plist-get event :type))
                           (nreverse events))
                   '(turn-started tool-started tool-finished turn-finished)))))

(provide 'e-work-e2e-test)

;;; e-work-e2e-test.el ends here
