;;; e-events-test.el --- Tests for e events -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for core event construction.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-events)

(ert-deftest e-events-test-make-event ()
  "Core events expose stable plist fields."
  (let ((event (e-events-make
                :id "evt-1"
                :type 'turn-started
                :session-id "session-1"
                :turn-id "turn-1"
                :payload '(:prompt "hello")
                :created-at 10)))
    (should (equal (plist-get event :id) "evt-1"))
    (should (equal (plist-get event :type) 'turn-started))
    (should (equal (plist-get event :session-id) "session-1"))
    (should (equal (plist-get event :turn-id) "turn-1"))
    (should (equal (plist-get event :payload) '(:prompt "hello")))
    (should (equal (plist-get event :created-at) 10))))

(ert-deftest e-events-test-carries-durable-activity-provenance-outside-payload ()
  "Activity provenance stays distinct from the producer's event payload."
  (let ((event (e-events-make :id "evt-1" :type 'tool-started
                              :session-id "session-1" :turn-id "turn-1"
                              :payload '(:name "read")
                              :activity-entry-id "activity-1"
                              :board-activity-sequence 7)))
    (should (equal (plist-get event :payload) '(:name "read")))
    (should (equal (plist-get event :activity-entry-id) "activity-1"))
    (should (= (plist-get event :board-activity-sequence) 7))))

(ert-deftest e-events-test-rejects-missing-type ()
  "Events require a type."
  (should-error
   (e-events-make :id "evt-1" :session-id "session-1")
   :type 'wrong-type-argument))

(provide 'e-events-test)

;;; e-events-test.el ends here
