;;; e-dev-profile-test.el --- Tests for e developer profiling -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for opt-in developer profiling traces.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-dev-profile)

(defun e-dev-profile-test--json-lines (file)
  "Return JSON objects read from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          records)
      (dolist (line (split-string (buffer-string) "\n" t))
        (push (json-read-from-string line) records))
      (nreverse records))))

(defmacro e-dev-profile-test--with-temp-profile (&rest body)
  "Run BODY with isolated profiling state."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "e-dev-profile-" t))
          (e-dev-profile-directory directory)
          (e-dev-profile--enabled nil)
          (e-dev-profile--current-file nil)
          (e-dev-profile--latest-file nil)
          (e-dev-profile--file-sequence 0))
     ,@body))

(ert-deftest e-dev-profile-test-disabled-record-does-not-write ()
  "Recording while disabled is cheap and does not create trace files."
  (e-dev-profile-test--with-temp-profile
    (should-not (e-dev-profile-enabled-p))
    (should-not
     (e-dev-profile-record 'test.event :duration-ms 12.5))
    (should-not (directory-files e-dev-profile-directory nil "\\.jsonl\\'"))))

(ert-deftest e-dev-profile-test-start-and-stop-manage-state ()
  "Start creates a JSONL trace and stop disables profiling."
  (e-dev-profile-test--with-temp-profile
    (let ((file (e-dev-profile-start)))
      (should (e-dev-profile-enabled-p))
      (should (string-suffix-p ".jsonl" file))
      (should (file-exists-p e-dev-profile-directory))
      (should (equal e-dev-profile--current-file file))
      (should (equal e-dev-profile--latest-file file))
      (should (equal (e-dev-profile-stop) file))
      (should-not (e-dev-profile-enabled-p))
      (should-not e-dev-profile--current-file)
      (should (equal e-dev-profile--latest-file file))
      (should (equal (e-dev-profile-stop) file)))))

(ert-deftest e-dev-profile-test-rapid-starts-use-unique-files ()
  "Trace starts in the same second do not reuse file paths."
  (e-dev-profile-test--with-temp-profile
    (let ((first (e-dev-profile-start)))
      (e-dev-profile-stop-trace)
      (let ((second (e-dev-profile-start)))
        (should-not (equal first second))
        (should (string-match-p "-000001\\.jsonl\\'" first))
        (should (string-match-p "-000002\\.jsonl\\'" second))))))

(ert-deftest e-dev-profile-test-record-writes-compact-json ()
  "Enabled profiling writes one compact JSON object per line."
  (e-dev-profile-test--with-temp-profile
    (let ((file (e-dev-profile-start)))
      (should
       (e-dev-profile-record 'harness.context
                             :duration-ms 3.25
                             :session-id "session-1"
                             :turn-id "turn-1"
                             :buffer-name "*e-chat*"
                             :metadata '((message-count . 2))))
      (let* ((records (e-dev-profile-test--json-lines file))
             (record (car records))
             (metadata (cdr (assq 'metadata record))))
        (should (= (length records) 1))
        (should (numberp (cdr (assq 'timestamp record))))
        (should (equal (cdr (assq 'event record)) "harness.context"))
        (should (= (cdr (assq 'duration-ms record)) 3.25))
        (should (equal (cdr (assq 'session-id record)) "session-1"))
        (should (equal (cdr (assq 'turn-id record)) "turn-1"))
        (should (equal (cdr (assq 'buffer-name record)) "*e-chat*"))
        (should (equal (cdr (assq 'message-count metadata)) 2))
        (should-not (assq 'content record))
        (should-not (assq 'prompt record))
        (should-not (assq 'tool-result record))))))

(ert-deftest e-dev-profile-test-measure-preserves-result-and-records-duration ()
  "Measuring a span preserves BODY result and writes a duration record."
  (e-dev-profile-test--with-temp-profile
    (let ((file (e-dev-profile-start)))
      (should
       (equal (e-dev-profile-measure 'session.append
                (:session-id "session-1"
                 :metadata '((kind . "message")))
                (+ 20 22))
              42))
      (let* ((records (e-dev-profile-test--json-lines file))
             (record (car records)))
        (should (= (length records) 1))
        (should (equal (cdr (assq 'event record)) "session.append"))
        (should (numberp (cdr (assq 'duration-ms record))))
        (should (equal (cdr (assq 'session-id record)) "session-1"))))))

(ert-deftest e-dev-profile-test-measure-thunk-supports-runtime-call-sites ()
  "Thunk measurement supports instrumentation without macro expansion."
  (e-dev-profile-test--with-temp-profile
    (let ((file (e-dev-profile-start)))
      (should
       (equal (e-dev-profile-measure-thunk
               'harness.prompt
               '(:session-id "session-1" :turn-id "turn-1")
               (lambda () "ok"))
              "ok"))
      (let* ((records (e-dev-profile-test--json-lines file))
             (record (car records)))
        (should (= (length records) 1))
        (should (equal (cdr (assq 'event record)) "harness.prompt"))
        (should (equal (cdr (assq 'session-id record)) "session-1"))
        (should (equal (cdr (assq 'turn-id record)) "turn-1"))))))

(ert-deftest e-dev-profile-test-measure-records-on-error-and-reraises ()
  "Measured spans still emit timings when BODY raises an error."
  (e-dev-profile-test--with-temp-profile
    (let ((file (e-dev-profile-start)))
      (should-error
       (e-dev-profile-measure 'chat.status
         (:buffer-name "*e-chat*")
         (error "boom"))
       :type 'error)
      (let* ((records (e-dev-profile-test--json-lines file))
             (record (car records))
             (metadata (cdr (assq 'metadata record))))
        (should (= (length records) 1))
        (should (equal (cdr (assq 'event record)) "chat.status"))
        (should (equal (cdr (assq 'buffer-name record)) "*e-chat*"))
        (should (equal (cdr (assq 'error metadata)) "boom"))))))

(ert-deftest e-dev-profile-test-report-aggregates-events ()
  "Report data includes slowest spans and aggregate timing by event name."
  (e-dev-profile-test--with-temp-profile
    (let ((file (e-dev-profile-start)))
      (e-dev-profile-record 'chat.status :duration-ms 5.0)
      (e-dev-profile-record 'chat.status :duration-ms 7.0)
      (e-dev-profile-record 'session.append :duration-ms 2.0)
      (e-dev-profile-stop)
      (let* ((report (e-dev-profile-report-data file))
             (aggregates (plist-get report :aggregates))
             (slowest (plist-get report :slowest))
             (chat (alist-get "chat.status" aggregates nil nil #'equal)))
        (should (= (plist-get report :event-count) 3))
        (should (= (plist-get chat :count) 2))
        (should (= (plist-get chat :total-ms) 12.0))
        (should (= (plist-get chat :average-ms) 6.0))
        (should (= (plist-get chat :max-ms) 7.0))
        (should (equal (alist-get 'event (car slowest)) "chat.status"))
        (should (= (alist-get 'duration-ms (car slowest)) 7.0))
        (should (string-match-p "chat.status"
                                (e-dev-profile-format-report report)))))))

(ert-deftest e-dev-profile-test-format-report-includes-range ()
  "Human-readable reports show the wall-clock trace range."
  (let* ((report (list :file "/tmp/e-trace.jsonl"
                       :event-count 2
                       :started-at 1780000000.0
                       :finished-at 1780000010.0
                       :aggregates nil
                       :slowest nil))
         (formatted (e-dev-profile-format-report report)))
    (should (string-match-p
             (format "Range: %s -> %s"
                     (e-dev-profile--format-timestamp 1780000000.0)
                     (e-dev-profile--format-timestamp 1780000010.0))
             formatted))))

(ert-deftest e-dev-profile-test-stop-trace-does-not-open-report ()
  "Scripted trace stops do not show presentation buffers."
  (e-dev-profile-test--with-temp-profile
    (let ((file (e-dev-profile-start))
          (report-count 0))
      (cl-letf (((symbol-function 'e-dev-profile-report)
                 (lambda (&optional _file)
                   (setq report-count (1+ report-count)))))
        (should (equal (e-dev-profile-stop-trace) file))
        (should (= report-count 0))))))

(ert-deftest e-dev-profile-test-report-tolerates-empty-trace ()
  "Report data remains useful when a trace was started but wrote no events."
  (e-dev-profile-test--with-temp-profile
    (let ((file (e-dev-profile-start)))
      (e-dev-profile-stop)
      (let ((report (e-dev-profile-report-data file)))
        (should (= (plist-get report :event-count) 0))
        (should-not (plist-get report :aggregates))
        (should (string-match-p "Events: 0"
                                (e-dev-profile-format-report report)))))))

(provide 'e-dev-profile-test)

;;; e-dev-profile-test.el ends here
