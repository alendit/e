;;; e-dev-perf-test.el --- Tests for e performance regression runner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for opt-in performance regression framework behavior.  These tests
;; use fake samples and fake scenarios instead of real timing thresholds.

;;; Code:

(require 'ert)
(unless (require 'e-dev-perf nil t)
  (load (expand-file-name "lisp/dev/e-dev-perf.el" default-directory)))

(defvar e-dev-perf-test--torn-down nil
  "State captured by teardown tests.")

(defmacro e-dev-perf-test--with-isolated-registry (&rest body)
  "Run BODY with isolated perf registry and artifact directories."
  (declare (indent 0) (debug t))
  `(let ((old-scenarios e-dev-perf--scenarios)
         (old-order e-dev-perf--scenario-order)
         (e-dev-perf-run-directory (make-temp-file "e-dev-perf-runs-" t))
         (e-dev-perf-baseline-file (make-temp-file "e-dev-perf-baseline-" nil ".json")))
     (unwind-protect
         (progn
           (e-dev-perf-clear-scenarios)
           ,@body)
       (setq e-dev-perf--scenarios old-scenarios
             e-dev-perf--scenario-order old-order))))

(defun e-dev-perf-test--baseline-metric (&rest overrides)
  "Return a baseline metric plist alist with OVERRIDES."
  (let ((metric '((baseline . 100.0)
                  (direction . "lower-is-better")
                  (fail_ratio . 1.30)
                  (fail_delta . 20.0)
                  (warn_ratio . 1.10)
                  (warn_delta . 5.0)
                  (variance_ratio . 0.50)
                  (samples . 3))))
    (while overrides
      (setf (alist-get (pop overrides) metric) (pop overrides)))
    metric))

(ert-deftest e-dev-perf-test-compare-metric-verdicts ()
  "Metric comparison reports pass, warning, regression, improvement, and noisy."
  (let ((baseline (e-dev-perf-test--baseline-metric)))
    (should (eq (plist-get (e-dev-perf-compare-metric
                            baseline '(:median 103.0 :variance-ratio 0.10))
                           :verdict)
                'pass))
    (should (eq (plist-get (e-dev-perf-compare-metric
                            baseline '(:median 112.0 :variance-ratio 0.10))
                           :verdict)
                'warning))
    (should (eq (plist-get (e-dev-perf-compare-metric
                            baseline '(:median 135.0 :variance-ratio 0.10))
                           :verdict)
                'regression))
    (should (eq (plist-get (e-dev-perf-compare-metric
                            baseline '(:median 80.0 :variance-ratio 0.10))
                           :verdict)
                'improvement))
    (should (eq (plist-get (e-dev-perf-compare-metric
                            baseline '(:median 103.0 :variance-ratio 0.75))
                           :verdict)
                'noisy))))

(ert-deftest e-dev-perf-test-summarize-samples-deterministically ()
  "Sample summaries are deterministic and independent of wall-clock timing."
  (let ((summary (e-dev-perf-summarize-samples '(9 1 5 3 7))))
    (should (= (plist-get summary :count) 5))
    (should (= (plist-get summary :median) 5))
    (should (= (plist-get summary :min) 1))
    (should (= (plist-get summary :max) 9))
    (should (= (plist-get summary :p90) 9))))

(ert-deftest e-dev-perf-test-scenario-run-teardown-after-error ()
  "Scenario teardown runs even when a measured scenario fails."
  (e-dev-perf-test--with-isolated-registry
    (let ((scenario (e-dev-perf-scenario-create
                     :id "fake.error"
                     :title "Fake error"
                     :owner 'e-dev
                     :setup (lambda (_scenario) 'state)
                     :run (lambda (_state) (error "boom"))
                     :teardown (lambda (_scenario state)
                                 (setq e-dev-perf-test--torn-down state))
                     :samples 1
                     :warmups 0)))
      (setq e-dev-perf-test--torn-down nil)
      (should-error (e-dev-perf-run-scenario-samples scenario) :type 'error)
      (should (eq e-dev-perf-test--torn-down 'state)))))

(ert-deftest e-dev-perf-test-run-fake-scenario-and-write-artifacts ()
  "A fake scenario produces samples, verdicts, JSON, and Org artifacts."
  (e-dev-perf-test--with-isolated-registry
    (let* ((baseline-file e-dev-perf-baseline-file)
           (counter 0)
           (scenario (e-dev-perf-scenario-create
                      :id "fake.fast"
                      :title "Fake fast"
                      :owner 'e-dev
                      :run (lambda (_state)
                             (setq counter (1+ counter))
                             (list :elapsed-ms 10.0 :calls counter))
                      :samples 3
                      :warmups 1)))
      (with-temp-file baseline-file
        (insert "{\"format\":1,\"scenarios\":{\"fake.fast\":{\"owner\":\"e-dev\",\"metrics\":{\"elapsed-ms\":{\"baseline\":10.0,\"direction\":\"lower-is-better\",\"fail_ratio\":1.5,\"fail_delta\":10.0,\"warn_ratio\":1.2,\"warn_delta\":5.0,\"variance_ratio\":0.5,\"samples\":3}}}}}}"))
      (e-dev-perf-register-scenario scenario)
      (let* ((run (e-dev-perf-run :scenario-ids '("fake.fast")
                                  :baseline-file baseline-file
                                  :write-artifacts t))
             (scenario-run (car (plist-get run :scenarios)))
             (elapsed-verdict (cdr (assoc "elapsed-ms"
                                          (plist-get scenario-run :verdicts)))))
        (should (= counter 4))
        (should (= (length (plist-get scenario-run :samples)) 3))
        (should (eq (plist-get elapsed-verdict :verdict) 'pass))
        (should (file-exists-p (plist-get run :json-file)))
        (should (file-exists-p (plist-get run :org-file)))
        (should (string-match-p "fake.fast" (e-dev-perf-format-report run)))))))

(ert-deftest e-dev-perf-test-load-baseline-validates-format ()
  "Baseline loading rejects unsupported formats."
  (e-dev-perf-test--with-isolated-registry
    (with-temp-file e-dev-perf-baseline-file
      (insert "{\"format\":99,\"scenarios\":{}}"))
    (should-error (e-dev-perf-load-baseline e-dev-perf-baseline-file)
                  :type 'user-error)))

(ert-deftest e-dev-perf-test-profile-metrics-aggregate_selected_spans ()
  "Profile span aggregation exposes scalar metric totals and counts."
  (let ((directory (make-temp-file "e-dev-perf-profile-" t))
        (e-dev-profile--enabled nil)
        (e-dev-profile--current-file nil)
        (e-dev-profile--latest-file nil))
    (let ((e-dev-profile-directory directory))
      (let ((metrics (e-dev-perf--profile-spans
                      (lambda ()
                        (e-dev-profile-record 'span.one :duration-ms 2.5)
                        (e-dev-profile-record 'span.one :duration-ms 3.5)
                        (e-dev-profile-record 'span.two :duration-ms 7.0))
                      '(span.one))))
        (should (= (plist-get metrics :span.one.ms) 6.0))
        (should (= (plist-get metrics :span.one.count) 2))))))

(ert-deftest e-dev-perf-test-register-default-scenarios ()
  "Default scenario registration exposes the initial scenario families."
  (e-dev-perf-test--with-isolated-registry
    (e-dev-perf-register-default-scenarios)
    (let ((ids (mapcar #'e-dev-perf-scenario-id (e-dev-perf-scenarios))))
      (should (member "turn.fake-backend" ids))
      (should (member "context.fixture" ids))
      (should (member "session.append-index" ids))
      (should (member "session.replay-list" ids))
      (should (member "session.metadata-state" ids))
      (should (member "chat.activity-burst" ids))
      (should (member "chat.final-assistant-render" ids))
      (should (member "tool.lifecycle-dispatch" ids)))))

(provide 'e-dev-perf-test)

;;; e-dev-perf-test.el ends here
