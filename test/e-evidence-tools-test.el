;;; e-evidence-tools-test.el --- Tests for session evidence tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for read-only evidence retrieval over session transcripts.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-evidence-tools)
(require 'e-tools)

(defun e-evidence-tools-test--store ()
  "Return a session store with representative evidence records."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message
     store "session-1" '(:id "msg-1" :turn-id "turn-1" :role user
                         :content "hello"))
    (e-session-append-message
     store "session-1" '(:id "msg-2" :turn-id "turn-1" :role assistant
                         :content "I will inspect that."))
    (e-session-append-message
     store "session-1" '(:id "msg-3" :turn-id "turn-1" :role tool
                         :content (:tool-call-id "call-1"
                                   :name "read"
                                   :status ok
                                   :content "file body")))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'started '(:source test))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'tool-finished '(:tool-call-id "call-1"))
    (e-session-append-activity-event
     store "session-1" "turn-2" 'finished '(:status ok))
    store))

(ert-deftest e-evidence-tools-test-fetch-message-range ()
  "Message ranges return a stable slice plus total count."
  (let* ((store (e-evidence-tools-test--store))
         (result (e-evidence-fetch-messages store "session-1"
                                            :offset 1
                                            :limit 2)))
    (should (equal (plist-get result :session-id) "session-1"))
    (should (= (plist-get result :offset) 1))
    (should (= (plist-get result :limit) 2))
    (should (= (plist-get result :total) 3))
    (should (equal (mapcar (lambda (message) (plist-get message :id))
                           (plist-get result :messages))
                   '("msg-2" "msg-3")))))

(ert-deftest e-evidence-tools-test-fetch-activity-event-range ()
  "Activity event ranges return a stable slice plus total count."
  (let* ((store (e-evidence-tools-test--store))
         (result (e-evidence-fetch-activity-events store "session-1"
                                                   :offset 1
                                                   :limit 1)))
    (should (= (plist-get result :total) 3))
    (should (equal (mapcar (lambda (event) (plist-get event :event-type))
                           (plist-get result :activity-events))
                   '(tool-finished)))))

(ert-deftest e-evidence-tools-test-fetch-tool-result-by-turn-and-call-id ()
  "Tool results can be fetched by turn id and tool-call id."
  (let* ((store (e-evidence-tools-test--store))
         (result (e-evidence-fetch-tool-result store "session-1"
                                               "turn-1"
                                               "call-1"))
         (tool-result (plist-get result :result)))
    (should (equal (plist-get result :session-id) "session-1"))
    (should (equal (plist-get result :turn-id) "turn-1"))
    (should (equal (plist-get result :tool-call-id) "call-1"))
    (should (equal (plist-get tool-result :name) "read"))
    (should (eq (plist-get tool-result :status) 'ok))
    (should (equal (plist-get tool-result :content) "file body"))))

(ert-deftest e-evidence-tools-test-capability-registers-read-only-tools ()
  "The evidence retrieval capability registers its read-only tool set."
  (let* ((store (e-evidence-tools-test--store))
         (registry (e-tools-registry-create))
         (capability (e-evidence-retrieval-capability-create
                      store "session-1")))
    (e-capabilities-register-tools capability registry)
    (should (equal (mapcar (lambda (definition)
                             (plist-get definition :name))
                           (e-tools-definitions registry))
                   '("evidence_messages"
                     "evidence_activity_events"
                     "evidence_tool_result")))
    (should (equal (plist-get
                    (e-tools-execute-batch
                     registry
                     '(:id "call-fetch"
                       :name "evidence_tool_result"
                       :arguments (:turn_id "turn-1"
                                  :tool_call_id "call-1")))
                    :status)
                   'ok))))

(provide 'e-evidence-tools-test)

;;; e-evidence-tools-test.el ends here
