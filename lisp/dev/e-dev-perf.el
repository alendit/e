;;; e-dev-perf.el --- Performance regression runner for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Opt-in performance regression support for development.  Scenarios produce
;; scalar metrics that are compared with reviewed baselines; routine ERT tests
;; exercise the comparison and runner contracts without depending on real
;; machine speed.

;;; Code:

(require 'cl-lib)
(require 'e-json)
(require 'seq)
(require 'subr-x)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-session)
(require 'e-session-async)
(require 'e-tools)
(require 'e-work)
(require 'e-ui-work)
(require 'e-dev-profile)

(declare-function e-chat-open "e-chat")
(declare-function e-chat-render-event "e-chat")
(declare-function e-chat-invalidate-mode-line-context-estimate "e-chat")
(declare-function e-chat-set-status "e-chat")
(declare-function e-chat-overview-prepare-unread-cache "e-chat-overview")
(declare-function e-chat-surface-set-redraw-visible "e-chat-surface")
(declare-function e-chat-service-binding-start "e-chat-service")
(declare-function e-chat-service-create-session-start "e-chat-service")
(declare-function e-chat-service-submit-session "e-chat-service")

(defgroup e-dev-perf nil
  "Performance regression tests for e."
  :group 'e-dev
  :prefix "e-dev-perf-")

(defcustom e-dev-perf-run-directory
  (expand-file-name ".e/perf-runs/" (e-source-directory))
  "Directory where local performance run artifacts are written."
  :type 'directory
  :group 'e-dev-perf)

(defcustom e-dev-perf-baseline-file
  (expand-file-name "test/perf/baselines/default.json" (e-source-directory))
  "Default reviewed performance baseline file."
  :type 'file
  :group 'e-dev-perf)

(defcustom e-dev-perf-default-samples 7
  "Default number of measured samples per performance scenario."
  :type 'integer
  :group 'e-dev-perf)

(defcustom e-dev-perf-default-warmups 1
  "Default number of warmup executions before samples are collected."
  :type 'integer
  :group 'e-dev-perf)

(cl-defstruct (e-dev-perf-scenario
               (:constructor e-dev-perf-scenario-create
                             (&key id title owner description setup run
                                   teardown metrics samples warmups tags
                                   profile-events)))
  id
  title
  owner
  description
  setup
  run
  teardown
  metrics
  (samples e-dev-perf-default-samples)
  (warmups e-dev-perf-default-warmups)
  tags
  profile-events)

(defvar e-dev-perf--scenarios (make-hash-table :test 'equal)
  "Registered performance scenarios keyed by id.")

(defvar e-dev-perf--scenario-order nil
  "Registered performance scenario ids in display order.")

(defvar e-dev-perf--run-sequence 0
  "Monotonic suffix used to keep performance run files unique.")

(defun e-dev-perf-clear-scenarios ()
  "Clear the performance scenario registry."
  (setq e-dev-perf--scenarios (make-hash-table :test 'equal))
  (setq e-dev-perf--scenario-order nil))

(defun e-dev-perf-register-scenario (scenario)
  "Register performance SCENARIO and return it."
  (unless (e-dev-perf-scenario-p scenario)
    (signal 'wrong-type-argument (list 'e-dev-perf-scenario-p scenario)))
  (let ((id (e-dev-perf-scenario-id scenario)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (signal 'wrong-type-argument (list 'e-dev-perf-scenario-id id)))
    (unless (gethash id e-dev-perf--scenarios)
      (setq e-dev-perf--scenario-order
            (append e-dev-perf--scenario-order (list id))))
    (puthash id scenario e-dev-perf--scenarios)
    scenario))

(defun e-dev-perf-scenarios ()
  "Return registered scenarios in deterministic order."
  (delq nil
        (mapcar (lambda (id)
                  (gethash id e-dev-perf--scenarios))
                e-dev-perf--scenario-order)))

(defun e-dev-perf-get-scenario (id)
  "Return registered scenario ID or signal a user error."
  (or (gethash id e-dev-perf--scenarios)
      (user-error "Unknown e performance scenario: %s" id)))

(defun e-dev-perf--alist-value (key alist)
  "Return KEY value from ALIST, accepting symbol or string keys."
  (or (alist-get key alist nil nil #'equal)
      (alist-get (if (symbolp key) (symbol-name key) (intern key))
                 alist nil nil #'equal)))

(defun e-dev-perf--canonical-to-domain (value)
  "Project canonical performance JSON VALUE into its alist domain.

The parser has already fixed objects as plists and arrays as vectors.  This
named adapter is the only place where performance reports regain their
historical alist/list domain representation."
  (cond
   ((vectorp value)
    (mapcar #'e-dev-perf--canonical-to-domain (append value nil)))
   ((and (consp value)
         (cl-evenp (length value))
         (cl-loop for (key _item) on value by #'cddr always (keywordp key)))
    (let (result)
      (while value
        (let* ((key (pop value))
               (item (pop value)))
          (push (cons (intern (substring (symbol-name key) 1))
                      (e-dev-perf--canonical-to-domain item)) result)))
      (nreverse result)))
   (t value)))

(defun e-dev-perf--json-read-file (file)
  "Read JSON FILE through the canonical parser and perf projection."
  (with-temp-buffer
    (insert-file-contents file)
    (e-dev-perf--canonical-to-domain
     (e-json-parse-string (buffer-string)))))

(defun e-dev-perf-load-baseline (&optional file)
  "Load and validate performance baseline FILE."
  (let* ((path (or file e-dev-perf-baseline-file))
         (baseline (e-dev-perf--json-read-file path))
         (format (e-dev-perf--alist-value 'format baseline)))
    (unless (equal format 1)
      (user-error "Unsupported e performance baseline format: %S" format))
    (unless (e-dev-perf--alist-value 'scenarios baseline)
      (user-error "Performance baseline has no scenarios: %s" path))
    baseline))

(defun e-dev-perf--metric-baseline (baseline scenario-id metric-id)
  "Return BASELINE entry for SCENARIO-ID METRIC-ID."
  (let* ((scenarios (e-dev-perf--alist-value 'scenarios baseline))
         (scenario (e-dev-perf--alist-value scenario-id scenarios))
         (metrics (and scenario (e-dev-perf--alist-value 'metrics scenario))))
    (and metrics (e-dev-perf--alist-value metric-id metrics))))

(defun e-dev-perf--metric-number (metric key &optional default)
  "Return numeric KEY from METRIC or DEFAULT."
  (let ((value (e-dev-perf--alist-value key metric)))
    (if (numberp value) value default)))

(defun e-dev-perf--metric-string (metric key &optional default)
  "Return string KEY from METRIC or DEFAULT."
  (let ((value (e-dev-perf--alist-value key metric)))
    (if (stringp value) value default)))

(defun e-dev-perf--direction (metric)
  "Return normalized direction from METRIC."
  (intern (e-dev-perf--metric-string metric 'direction "lower-is-better")))

(defun e-dev-perf--ratio (current baseline direction)
  "Return comparison ratio for CURRENT and BASELINE under DIRECTION."
  (cond
   ((not (numberp baseline)) 1.0)
   ((zerop baseline)
    (pcase direction
      ('higher-is-better (if (> current 0) 0.0 1.0))
      (_ (if (> current 0) most-positive-fixnum 1.0))))
   ((and (eq direction 'higher-is-better) (zerop current))
    most-positive-fixnum)
   ((eq direction 'higher-is-better) (/ (float baseline) current))
   (t (/ (float current) baseline))))

(defun e-dev-perf--worse-delta (current baseline direction)
  "Return positive worse delta for CURRENT against BASELINE under DIRECTION."
  (pcase direction
    ('higher-is-better (- baseline current))
    (_ (- current baseline))))

(defun e-dev-perf--better-delta (current baseline direction)
  "Return positive improvement delta for CURRENT against BASELINE under DIRECTION."
  (pcase direction
    ('higher-is-better (- current baseline))
    (_ (- baseline current))))

(defun e-dev-perf--threshold-exceeded-p (ratio delta ratio-limit delta-limit)
  "Return non-nil when RATIO and DELTA exceed configured limits."
  (and (numberp ratio)
       (numberp delta)
       (> ratio (or ratio-limit most-positive-fixnum))
       (> delta (or delta-limit 0))))

(defun e-dev-perf-compare-metric (baseline-metric summary)
  "Compare SUMMARY against BASELINE-METRIC and return a verdict plist."
  (let* ((baseline (e-dev-perf--metric-number baseline-metric 'baseline))
         (current (plist-get summary :median))
         (direction (e-dev-perf--direction baseline-metric))
         (variance (or (plist-get summary :variance-ratio) 0.0))
         (variance-limit (e-dev-perf--metric-number baseline-metric 'variance_ratio))
         (ratio (and baseline current
                     (e-dev-perf--ratio current baseline direction)))
         (worse-delta (and baseline current
                           (e-dev-perf--worse-delta current baseline direction)))
         (better-delta (and baseline current
                            (e-dev-perf--better-delta current baseline direction)))
         (fail-ratio (e-dev-perf--metric-number baseline-metric 'fail_ratio 1.35))
         (fail-delta (e-dev-perf--metric-number baseline-metric 'fail_delta 0.0))
         (warn-ratio (e-dev-perf--metric-number baseline-metric 'warn_ratio 1.20))
         (warn-delta (e-dev-perf--metric-number baseline-metric 'warn_delta 0.0))
         (verdict 'pass))
    (setq verdict
          (cond
           ((and variance-limit (> variance variance-limit)) 'noisy)
           ((and ratio worse-delta
                 (e-dev-perf--threshold-exceeded-p
                  ratio worse-delta fail-ratio fail-delta))
            'regression)
           ((and ratio worse-delta
                 (e-dev-perf--threshold-exceeded-p
                  ratio worse-delta warn-ratio warn-delta))
            'warning)
           ((and better-delta
                 (> better-delta (max warn-delta 0.0)))
            'improvement)
           (t 'pass)))
    (list :verdict verdict
          :baseline baseline
          :current current
          :delta (and baseline current (- current baseline))
          :ratio ratio
          :variance-ratio variance)))

(defun e-dev-perf--median (numbers)
  "Return median of NUMBERS."
  (let* ((sorted (sort (copy-sequence numbers) #'<))
         (count (length sorted)))
    (cond
     ((zerop count) nil)
     ((cl-oddp count) (nth (/ count 2) sorted))
     (t (/ (+ (nth (1- (/ count 2)) sorted)
             (nth (/ count 2) sorted))
          2.0)))))

(defun e-dev-perf--percentile (numbers percentile)
  "Return PERCENTILE from NUMBERS using nearest-rank selection."
  (let* ((sorted (sort (copy-sequence numbers) #'<))
         (count (length sorted)))
    (when (> count 0)
      (nth (min (1- count)
                (max 0 (ceiling (- (* (/ percentile 100.0) count) 1))))
           sorted))))

(defun e-dev-perf-summarize-samples (values)
  "Return summary statistics for numeric sample VALUES."
  (let* ((numbers (seq-filter #'numberp values))
         (count (length numbers))
         (min-value (and numbers (apply #'min numbers)))
         (max-value (and numbers (apply #'max numbers)))
         (median (e-dev-perf--median numbers))
         (p90 (e-dev-perf--percentile numbers 90)))
    (list :count count
          :median median
          :min min-value
          :max max-value
          :p90 p90
          :variance-ratio (if (and median (> median 0) min-value max-value)
                              (/ (- max-value min-value) (float median))
                            0.0)
          :values numbers)))

(defun e-dev-perf--plist-keys (plist)
  "Return keyword keys in PLIST."
  (let (keys)
    (while plist
      (push (pop plist) keys)
      (pop plist))
    (nreverse keys)))

(defun e-dev-perf--merge-metric-keys (samples)
  "Return all metric keys seen in SAMPLES."
  (let (keys)
    (dolist (sample samples)
      (dolist (key (e-dev-perf--plist-keys sample))
        (unless (memq key keys)
          (push key keys))))
    (nreverse keys)))

(defun e-dev-perf--summaries-for-samples (samples)
  "Return metric summaries for scenario SAMPLES."
  (mapcar (lambda (key)
            (cons (substring (symbol-name key) 1)
                  (e-dev-perf-summarize-samples
                   (mapcar (lambda (sample) (plist-get sample key)) samples))))
          (e-dev-perf--merge-metric-keys samples)))

(defun e-dev-perf--elapsed-ms (thunk)
  "Run THUNK and return cons of result and elapsed milliseconds."
  (let ((start (float-time))
        result)
    (setq result (funcall thunk))
    (cons result (* 1000.0 (- (float-time) start)))))

(defun e-dev-perf--keywordize-metrics (metrics)
  "Return METRICS plist with string keys converted to keywords."
  (let (result)
    (while metrics
      (let ((key (pop metrics))
            (value (pop metrics)))
        (push (cond
               ((keywordp key) key)
               ((symbolp key) (intern (concat ":" (symbol-name key))))
               ((stringp key) (intern (concat ":" key)))
               (t key))
              result)
        (push value result)))
    (nreverse result)))

(defun e-dev-perf--call-or-nil (function &rest args)
  "Call FUNCTION with ARGS when FUNCTION is non-nil."
  (when function
    (apply function args)))

(defun e-dev-perf--run-once (scenario state)
  "Run SCENARIO once with STATE and return scalar metric plist."
  (let* ((run (or (e-dev-perf-scenario-run scenario)
                  (signal 'wrong-type-argument
                          (list 'functionp nil))))
         (timed (e-dev-perf--elapsed-ms
                 (lambda ()
                   (funcall run state))))
         (metrics (e-dev-perf--keywordize-metrics (or (car timed) nil))))
    (unless (plist-member metrics :elapsed-ms)
      (setq metrics (plist-put metrics :elapsed-ms (cdr timed))))
    metrics))

(defun e-dev-perf-run-scenario-samples (scenario)
  "Run SCENARIO warmups and samples, returning a run plist."
  (let* ((setup (e-dev-perf-scenario-setup scenario))
         (teardown (e-dev-perf-scenario-teardown scenario))
         (warmups (or (e-dev-perf-scenario-warmups scenario) 0))
         (sample-count (or (e-dev-perf-scenario-samples scenario) 1))
         state samples)
    (unwind-protect
        (progn
          (setq state (e-dev-perf--call-or-nil setup scenario))
          (dotimes (_ warmups)
            (e-dev-perf--run-once scenario state))
          (dotimes (_ sample-count)
            (push (e-dev-perf--run-once scenario state) samples))
          (setq samples (nreverse samples))
          (list :id (e-dev-perf-scenario-id scenario)
                :title (e-dev-perf-scenario-title scenario)
                :owner (symbol-name (e-dev-perf-scenario-owner scenario))
                :samples samples
                :metrics (e-dev-perf--summaries-for-samples samples)))
      (when teardown
        (funcall teardown scenario state)))))

(defun e-dev-perf--scenario-selected-p (scenario ids owner tags)
  "Return non-nil when SCENARIO matches IDS, OWNER, or TAGS filters."
  (and (or (not ids)
           (member (e-dev-perf-scenario-id scenario) ids))
       (or (not owner)
           (eq (e-dev-perf-scenario-owner scenario) owner))
       (or (not tags)
           (cl-intersection tags (e-dev-perf-scenario-tags scenario)))))

(defun e-dev-perf--run-id ()
  "Return a unique run id."
  (setq e-dev-perf--run-sequence (1+ e-dev-perf--run-sequence))
  (format "%s-%06d" (format-time-string "%Y%m%d-%H%M%S") e-dev-perf--run-sequence))

(defun e-dev-perf--git-commit ()
  "Return current git commit when available."
  (let ((default-directory (e-source-directory)))
    (string-trim
     (or (ignore-errors
           (with-output-to-string
             (with-current-buffer standard-output
               (call-process "git" nil t nil "rev-parse" "--short" "HEAD"))))
         "unknown"))))

(defun e-dev-perf--environment ()
  "Return compact environment metadata for a performance run."
  (list :emacs-version emacs-version
        :system-type (symbol-name system-type)
        :native-compilation (and (fboundp 'native-comp-available-p)
                                 (native-comp-available-p))
        :commit (e-dev-perf--git-commit)))

(defun e-dev-perf--compare-scenario (baseline scenario-run)
  "Compare SCENARIO-RUN with BASELINE."
  (let ((scenario-id (plist-get scenario-run :id))
        verdicts)
    (dolist (entry (plist-get scenario-run :metrics))
      (let* ((metric-id (car entry))
             (summary (cdr entry))
             (baseline-metric
              (e-dev-perf--metric-baseline baseline scenario-id metric-id)))
        (push (cons metric-id
                    (if baseline-metric
                        (e-dev-perf-compare-metric baseline-metric summary)
                      (list :verdict 'skipped
                            :current (plist-get summary :median))))
              verdicts)))
    (plist-put scenario-run :verdicts (nreverse verdicts))))

(defun e-dev-perf-compare-run (run baseline)
  "Return RUN with verdicts computed against BASELINE."
  (plist-put
   run
   :scenarios
   (mapcar (lambda (scenario-run)
             (e-dev-perf--compare-scenario baseline scenario-run))
           (plist-get run :scenarios))))

(cl-defun e-dev-perf-run (&key scenario-ids owner tags baseline-file write-artifacts)
  "Run selected performance scenarios and return a run plist.
SCENARIO-IDS, OWNER, and TAGS filter the registry.  BASELINE-FILE defaults to
`e-dev-perf-baseline-file'.  When WRITE-ARTIFACTS is non-nil, save JSON and Org
artifacts under `e-dev-perf-run-directory'."
  (interactive)
  (e-dev-perf-register-default-scenarios)
  (let* ((baseline (e-dev-perf-load-baseline baseline-file))
         (selected (seq-filter
                    (lambda (scenario)
                      (e-dev-perf--scenario-selected-p
                       scenario scenario-ids owner tags))
                    (e-dev-perf-scenarios)))
         (run (list :format 1
                    :id (e-dev-perf--run-id)
                    :created-at (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                    :baseline-file (or baseline-file e-dev-perf-baseline-file)
                    :environment (e-dev-perf--environment)
                    :scenarios (mapcar #'e-dev-perf-run-scenario-samples
                                       selected))))
    (setq run (e-dev-perf-compare-run run baseline))
    (when write-artifacts
      (e-dev-perf-write-run-result run))
    (when (called-interactively-p 'interactive)
      (e-dev-perf-report-run run))
    run))

(defun e-dev-perf-run-scenario (id)
  "Run one performance scenario ID interactively."
  (interactive
   (list (completing-read "Scenario: "
                          (mapcar #'e-dev-perf-scenario-id
                                  (progn
                                    (e-dev-perf-register-default-scenarios)
                                    (e-dev-perf-scenarios)))
                          nil t)))
  (let ((run (e-dev-perf-run :scenario-ids (list id) :write-artifacts t)))
    (e-dev-perf-report-run run)
    run))

(defun e-dev-perf--json-keyword (key)
  "Return canonical keyword for domain KEY."
  (intern (concat ":"
                  (cond ((keywordp key) (substring (symbol-name key) 1))
                        ((symbolp key) (symbol-name key))
                        ((stringp key) key)
                        (t (format "%s" key))))))

(defun e-dev-perf--json-optional (value)
  "Return VALUE as a JSON scalar, preserving an explicit null."
  (cond
   ((null value) e-json-null)
   ((and (symbolp value)
         (not (memq value '(t :json-false :json-null))))
    (symbol-name value))
   (t value)))

(defun e-dev-perf--environment-to-json (environment)
  "Project run ENVIRONMENT plist into canonical JSON."
  (list :emacs-version (e-dev-perf--json-optional
                        (plist-get environment :emacs-version))
        :system-type (e-dev-perf--json-optional
                      (plist-get environment :system-type))
        :native-compilation (if (plist-get environment :native-compilation)
                                t e-json-false)
        :commit (e-dev-perf--json-optional (plist-get environment :commit))))

(defun e-dev-perf--sample-to-json (sample)
  "Project one metric SAMPLE plist into canonical JSON."
  (let (result)
    (while sample
      (let ((key (pop sample))
            (value (pop sample)))
        (setq result (append result
                             (list (e-dev-perf--json-keyword key)
                                   (e-dev-perf--json-optional value))))))
    result))

(defun e-dev-perf--summary-to-json (summary)
  "Project one metric SUMMARY plist into canonical JSON."
  (let (result)
    (while summary
      (let ((key (pop summary))
            (value (pop summary)))
        (setq result
              (append result
                      (list (e-dev-perf--json-keyword key)
                            (if (eq key :values)
                                (vconcat (mapcar #'e-dev-perf--json-optional value))
                              (e-dev-perf--json-optional value)))))))
    result))

(defun e-dev-perf--metrics-to-json (metrics)
  "Project metric ALIST METRICS into a canonical JSON object."
  (let (result)
    (dolist (entry metrics result)
      (setq result
            (append result
                    (list (e-dev-perf--json-keyword (car entry))
                          (e-dev-perf--summary-to-json (cdr entry))))))))

(defun e-dev-perf--verdicts-to-json (verdicts)
  "Project verdict ALIST VERDICTS into a canonical JSON object."
  (let (result)
    (dolist (entry verdicts result)
      (setq result
            (append result
                    (list (e-dev-perf--json-keyword (car entry))
                          (e-dev-perf--summary-to-json (cdr entry))))))))

(defun e-dev-perf--scenario-to-json (scenario)
  "Project one run SCENARIO plist into canonical JSON."
  (list :id (e-dev-perf--json-optional (plist-get scenario :id))
        :title (e-dev-perf--json-optional (plist-get scenario :title))
        :owner (e-dev-perf--json-optional (plist-get scenario :owner))
        :samples (vconcat (mapcar #'e-dev-perf--sample-to-json
                                  (plist-get scenario :samples)))
        :metrics (e-dev-perf--metrics-to-json
                  (plist-get scenario :metrics))
        :verdicts (e-dev-perf--verdicts-to-json
                   (plist-get scenario :verdicts))))

(defun e-dev-perf--run-to-json (run)
  "Project performance RUN plist into canonical JSON."
  (list :format (plist-get run :format)
        :id (plist-get run :id)
        :created-at (plist-get run :created-at)
        :baseline-file (e-dev-perf--json-optional
                        (plist-get run :baseline-file))
        :environment (e-dev-perf--environment-to-json
                      (plist-get run :environment))
        :scenarios (vconcat (mapcar #'e-dev-perf--scenario-to-json
                                    (plist-get run :scenarios)))))

(defun e-dev-perf-run-result-file (run &optional extension)
  "Return artifact path for RUN with EXTENSION."
  (expand-file-name (format "%s.%s" (plist-get run :id) (or extension "json"))
                    e-dev-perf-run-directory))

(defun e-dev-perf-write-run-result (run)
  "Write RUN JSON and Org report artifacts and return the JSON path."
  (make-directory e-dev-perf-run-directory t)
  (let ((json-file (e-dev-perf-run-result-file run "json"))
        (org-file (e-dev-perf-run-result-file run "org"))
        (coding-system-for-write 'utf-8))
    (with-temp-file json-file
      (insert (e-json-serialize (e-dev-perf--run-to-json run)))
      (insert "\n"))
    (with-temp-file org-file
      (insert (e-dev-perf-format-report run)))
    (plist-put run :json-file json-file)
    (plist-put run :org-file org-file)
    json-file))

(defun e-dev-perf--count-verdicts (run)
  "Return verdict count alist for RUN."
  (let (counts)
    (dolist (scenario (plist-get run :scenarios))
      (dolist (entry (plist-get scenario :verdicts))
        (let ((verdict (plist-get (cdr entry) :verdict)))
          (setf (alist-get verdict counts) (1+ (or (alist-get verdict counts) 0))))))
    counts))

(defun e-dev-perf--format-number (value)
  "Format numeric VALUE compactly."
  (if (numberp value)
      (format "%.3f" value)
    "-"))

(defun e-dev-perf-format-report (run)
  "Return an Org report for performance RUN."
  (let ((counts (e-dev-perf--count-verdicts run))
        lines)
    (push (format "#+title: e Performance Run %s" (plist-get run :id)) lines)
    (push "" lines)
    (push "* Summary" lines)
    (push (format "- Run id: =%s=" (plist-get run :id)) lines)
    (push (format "- Created at: =%s=" (plist-get run :created-at)) lines)
    (push (format "- Baseline: =%s=" (plist-get run :baseline-file)) lines)
    (push (format "- Verdicts: pass=%d warning=%d regression=%d improvement=%d noisy=%d skipped=%d"
                  (or (alist-get 'pass counts) 0)
                  (or (alist-get 'warning counts) 0)
                  (or (alist-get 'regression counts) 0)
                  (or (alist-get 'improvement counts) 0)
                  (or (alist-get 'noisy counts) 0)
                  (or (alist-get 'skipped counts) 0))
          lines)
    (push "" lines)
    (push "* Metrics" lines)
    (push "| Scenario | Owner | Metric | Baseline | Current | Delta | Ratio | Variance | Verdict |" lines)
    (push "|----------+-------+--------+----------+---------+-------+-------+----------+---------|" lines)
    (dolist (scenario (plist-get run :scenarios))
      (dolist (entry (plist-get scenario :verdicts))
        (let ((data (cdr entry)))
          (push (format "| %s | %s | %s | %s | %s | %s | %s | %s | %s |"
                        (plist-get scenario :id)
                        (plist-get scenario :owner)
                        (car entry)
                        (e-dev-perf--format-number (plist-get data :baseline))
                        (e-dev-perf--format-number (plist-get data :current))
                        (e-dev-perf--format-number (plist-get data :delta))
                        (e-dev-perf--format-number (plist-get data :ratio))
                        (e-dev-perf--format-number (plist-get data :variance-ratio))
                        (plist-get data :verdict))
                lines))))
    (push "" lines)
    (push "* Artifacts" lines)
    (when-let* ((json-file (plist-get run :json-file)))
      (push (format "- JSON: =%s=" json-file) lines))
    (when-let* ((org-file (plist-get run :org-file)))
      (push (format "- Org: =%s=" org-file) lines))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun e-dev-perf-report-run (run)
  "Display performance RUN in a report buffer."
  (with-current-buffer (get-buffer-create "*e-dev-perf*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (e-dev-perf-format-report run))
      (goto-char (point-min))
      (org-mode)
      (view-mode 1))
    (pop-to-buffer (current-buffer))))

(defun e-dev-perf-report (file)
  "Open a report for performance run JSON FILE."
  (interactive "fPerf run JSON: ")
  (let ((run (e-dev-perf--json-read-file file)))
    (e-dev-perf-report-run (e-dev-perf--alist-run-to-plist run))))

(defun e-dev-perf--alist-run-to-plist (value)
  "Return VALUE converted shallowly from JSON alists to plists for reports."
  (cond
   ((and (listp value) (cl-every #'consp value))
    (let (plist)
      (dolist (entry value)
        (setq plist
              (plist-put plist
                         (intern (concat ":" (symbol-name (car entry))))
                         (e-dev-perf--alist-run-to-plist (cdr entry)))))
      plist))
   ((listp value)
    (mapcar #'e-dev-perf--alist-run-to-plist value))
   (t value)))

(defun e-dev-perf-update-baseline (run-file baseline-file)
  "Update BASELINE-FILE from RUN-FILE explicitly."
  (interactive "fPerf run JSON: \nFBaseline file: ")
  (let* ((run (e-dev-perf--json-read-file run-file))
         (created-at (e-dev-perf--alist-value 'created_at run))
         (scenarios (e-dev-perf--alist-value 'scenarios run))
         (baseline-scenarios nil))
    (dolist (scenario scenarios)
      (let ((metric-entries nil))
        (dolist (metric (e-dev-perf--alist-value 'metrics scenario))
          (let ((summary (cdr metric)))
            (push (cons (car metric)
                        `((baseline . ,(e-dev-perf--alist-value 'median summary))
                          (direction . "lower-is-better")
                          (fail_ratio . 1.35)
                          (fail_delta . 5.0)
                          (warn_ratio . 1.20)
                          (warn_delta . 2.0)
                          (variance_ratio . 0.50)
                          (samples . ,(e-dev-perf--alist-value 'count summary))))
                  metric-entries)))
        (push (cons (e-dev-perf--alist-value 'id scenario)
                    `((owner . ,(e-dev-perf--alist-value 'owner scenario))
                      (metrics . ,(nreverse metric-entries))))
              baseline-scenarios)))
    (make-directory (file-name-directory baseline-file) t)
    (with-temp-file baseline-file
      (insert
       (e-json-serialize
        (list :format 1
              :created_at created-at
              :source "updated-from-run"
              :environment_class "default"
              :scenarios
              (let (result)
                (dolist (scenario (nreverse baseline-scenarios) result)
                  (setq result
                        (append result
                                (list (e-dev-perf--json-keyword (car scenario))
                                      (let (scenario-object)
                                        (dolist (field (cdr scenario) scenario-object)
                                          (setq scenario-object
                                                (append scenario-object
                                                        (list (e-dev-perf--json-keyword
                                                               (car field))
                                                              (if (eq (car field) 'metrics)
                                                                  (let (metric-object)
                                                                    (dolist (metric (cdr field) metric-object)
                                                                      (setq metric-object
                                                                            (append metric-object
                                                                                    (list (e-dev-perf--json-keyword (car metric))
                                                                                          (let (summary-object)
                                                                                            (dolist (summary-field (cdr metric) summary-object)
                                                                                              (setq summary-object
                                                                                                    (append summary-object
                                                                                                            (list (e-dev-perf--json-keyword
                                                                                                                   (car summary-field))
                                                                                                                  (e-dev-perf--json-optional
                                                                                                                   (cdr summary-field))))))
                                                                                            summary-object))))))
                                                                    metric-object)
                                                                (e-dev-perf--json-optional (cdr field)))))))))))))))
      (insert "\n"))
    baseline-file))

(defun e-dev-perf-list-scenarios ()
  "List registered performance scenarios."
  (interactive)
  (e-dev-perf-register-default-scenarios)
  (with-current-buffer (get-buffer-create "*e-dev-perf-scenarios*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "e performance scenarios\n\n")
      (dolist (scenario (e-dev-perf-scenarios))
        (insert (format "- %s [%s] %s\n"
                        (e-dev-perf-scenario-id scenario)
                        (e-dev-perf-scenario-owner scenario)
                        (or (e-dev-perf-scenario-title scenario) ""))))
      (goto-char (point-min))
      (view-mode 1))
    (pop-to-buffer (current-buffer))))

(defun e-dev-perf--profile-spans (thunk events)
  "Run THUNK with a temporary profile trace and return metrics for EVENTS."
  (let ((old-enabled e-dev-profile--enabled)
        (old-current e-dev-profile--current-file)
        (old-latest e-dev-profile--latest-file)
        (file nil))
    (unwind-protect
        (progn
          (setq file (e-dev-profile-start))
          (funcall thunk)
          (e-dev-profile-stop-trace)
          (e-dev-perf--profile-metrics file events))
      (setq e-dev-profile--enabled old-enabled
            e-dev-profile--current-file old-current
            e-dev-profile--latest-file old-latest))))

(defun e-dev-perf--profile-metrics (file events)
  "Return scalar metrics for profile FILE filtered to EVENTS."
  (let* ((report (and file (file-exists-p file)
                      (e-dev-profile-report-data file)))
         (aggregates (plist-get report :aggregates))
         metrics)
    (dolist (event events)
      (let* ((name (if (symbolp event) (symbol-name event) event))
             (data (alist-get name aggregates nil nil #'equal)))
        (setq metrics
              (plist-put metrics
                         (intern (concat ":" name ".ms"))
                         (or (plist-get data :total-ms) 0.0)))
        (setq metrics
              (plist-put metrics
                         (intern (concat ":" name ".count"))
                         (or (plist-get data :count) 0)))))
    metrics))

(defun e-dev-perf--with-temp-session-store (thunk)
  "Call THUNK with a temporary persistent session store."
  (let* ((directory (make-temp-file "e-dev-perf-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (funcall thunk store directory)
      (ignore-errors (e-session-storage-close store))
      (ignore-errors (delete-directory directory t)))))

(defun e-dev-perf--scenario-session-append-run (_state)
  "Run session append/index scenario."
  (e-dev-perf--with-temp-session-store
   (lambda (store _directory)
     (let ((index-writes 0)
           metrics)
       (e-session-create store :id "session-append")
       (cl-letf (((symbol-function 'e-session-storage-publish-projections)
                  (lambda (&rest _)
                    (setq index-writes (1+ index-writes)))))
         (setq metrics
               (e-dev-perf--profile-spans
                (lambda ()
                  (dotimes (index 8)
                    (e-session-append-message
                     store "session-append"
                     (list :role (if (cl-evenp index) 'user 'assistant)
                           :content (format "message %d" index)
                           :turn-id (format "turn-%d" (/ index 2))))
                    (e-session-append-activity-event
                     store "session-append" (format "turn-%d" (/ index 2))
                     'reasoning-delta
                     (list :type 'reasoning-delta :content "thinking")
                     :write-index nil)))
                '(session.append-message session.append-activity))))
       (plist-put metrics :session.index-write.count index-writes)))))

(defun e-dev-perf--scenario-session-visible-query-run (_state)
  "Run one bounded visible-message query after queued session writes."
  (e-dev-perf--with-temp-session-store
   (lambda (store _directory)
     (e-work-with-batch-await
       (e-work-await-batch
        (e-session-create store :id "session-query") :timeout 5))
     (dotimes (index 12)
       (e-session-append-message
        store "session-query"
        (list :role (if (cl-evenp index) 'user 'assistant)
              :content (format "replay message %d" index)
              :turn-id (format "turn-%d" (/ index 2)))))
     ;; The read follows the writes in the runtime FIFO, so awaiting this one
     ;; explicit dev operation proves the committed result without opening a
     ;; second store or reconstructing a session aggregate.
     (let ((page
            (e-work-with-batch-await
              (e-work-await-batch
               (e-session-async-visible-message-page
                store "session-query" 12)
               :timeout 5))))
       (list :messages-returned.count
             (length (plist-get page :messages)))))))

(defun e-dev-perf--scenario-session-metadata-state-run (_state)
  "Run typed session metadata state write scenario."
  (e-dev-perf--with-temp-session-store
   (lambda (store _directory)
     (let ((index-writes 0)
           metrics)
       (e-session-create store :id "session-state")
       (cl-letf (((symbol-function 'e-session-storage-publish-projections)
                  (lambda (&rest _)
                    (setq index-writes (1+ index-writes)))))
         (setq metrics
               (e-dev-perf--profile-spans
                (lambda ()
                  (dotimes (index 4)
                    (e-session-set-session-config
                     store "session-state"
                     (list :project-root
                           (format "/tmp/e-state-%d/" index)))
                    (e-session-set-context-references
                     store
                     "session-state"
                     'chat-session
                     (list :attachments
                           (list (list :uri
                                       (format "buffer://source-%d" index)))))
                    (e-session-set-capability-state
                     store
                     "session-state"
                     'mcp
                     (list :enabled t
                           :iteration index))))
                '(session.append-record session.write-index))))
       (plist-put metrics :session.index-write.count index-writes)))))

(defun e-dev-perf--scenario-context-run (_state)
  "Run context assembly fixture scenario."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (e-harness-create-session harness :id "context-fixture")
    (dotimes (index 10)
      (e-session-append-message
       store "context-fixture"
       (list :role (if (cl-evenp index) 'user 'assistant)
             :content (format "context fixture message %d" index)
             :turn-id (format "turn-%d" (/ index 2)))))
    (let (context
          metrics)
      (setq metrics
            (e-dev-perf--profile-spans
             (lambda ()
               (setq context
                     (e-harness-context
                      harness "context-fixture" "turn-context" 'preview)))
             '(harness.context)))
      (setq metrics (plist-put metrics :context.message-count
                               (length (plist-get context :messages))))
      (plist-put metrics :context.bytes
                 (string-bytes (prin1-to-string (plist-get context :messages)))))))

(defun e-dev-perf--scenario-turn-start-run (_state)
  "Run fake backend turn-start scenario."
  (require 'e-chat-service)
  (e-dev-perf--with-temp-session-store
   (lambda (store _directory)
     (let* ((backend (e-backend-fake-create
                      :items (list (list :type 'assistant-message
                                         :content "ok"))))
            (harness (e-harness-create :backend backend :sessions store)))
       (e-chat-service-create-session-start
        :harness harness :id "turn-start")
       (e-dev-perf--profile-spans
        (lambda ()
          (e-chat-service-submit-session harness "turn-start" "hello")
          (while (not (plist-get (e-harness-state harness "turn-start")
                                 :active-turn))
            (accept-process-output nil 0.001))
          (e-harness-wait-batch harness "turn-start"))
        '(harness.prompt harness.prompt-async harness.context
          harness.message-append loop.backend-start))))))

(defun e-dev-perf--chat-buffer-state ()
  "Return a temporary SQL-backed chat presentation scenario state."
  (require 'e-chat)
  (let* ((directory (make-temp-file "e-dev-perf-chat-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (metadata '(:name "Performance chat")))
    (condition-case error
        (progn
          (require 'e-chat-service)
          (e-chat-service-create-session-start
           :harness harness :id "chat-perf" :metadata metadata)
          (e-work-with-batch-await
            (e-work-await-batch
             (e-chat-service-binding-start harness "chat-perf" nil t)
             :timeout 5))
          (let ((buffer (e-chat-open :harness harness
                                     :session-id "chat-perf"
                                     :metadata metadata)))
            (list :harness harness :buffer buffer :session-id "chat-perf"
                  :store store :directory directory)))
      (error
       (ignore-errors (e-session-storage-close store))
       (ignore-errors (delete-directory directory t))
       (signal (car error) (cdr error))))))

(defun e-dev-perf--chat-teardown (_scenario state)
  "Clean up chat scenario STATE."
  (when-let* ((buffer (plist-get state :buffer)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (when-let* ((store (plist-get state :store)))
    (ignore-errors (e-session-storage-close store)))
  (when-let* ((directory (plist-get state :directory)))
    (ignore-errors (delete-directory directory t))))

(defun e-dev-perf--drain-ui-work (buffer &rest args)
  "Drain finite UI work in BUFFER with ARGS for performance scenarios."
  (e-ui-work-with-batch-drain
    (apply #'e-ui-work-drain-batch :buffer buffer args)))

(defun e-dev-perf--chat-pending-ui-work-count (buffer &rest args)
  "Return count of pending UI work in BUFFER narrowed by ARGS."
  (length (apply #'e-ui-work-pending buffer args)))

(defun e-dev-perf--scenario-chat-status-run (state)
  "Measure synchronous context construction during chat status updates."
  (let ((buffer (plist-get state :buffer))
        (context-builds 0)
        (original (symbol-function 'e-harness-context)))
    (with-current-buffer buffer
      (e-chat-surface-set-redraw-visible t)
      (cl-letf (((symbol-function 'e-harness-context)
                 (lambda (&rest args)
                   (setq context-builds (1+ context-builds))
                   (apply original args))))
        (dotimes (index 8)
          (e-chat-invalidate-mode-line-context-estimate)
          (e-chat-set-status (format "perf-status-%d" index) t)))
      (list :status.synchronous-context-build.count context-builds
            :status.pending-refresh.count
            (e-dev-perf--chat-pending-ui-work-count
             buffer :owner 'chat-mode-line-status)))))

(defun e-dev-perf--scenario-chat-activity-run (state)
  "Run chat activity burst scenario."
  (let ((buffer (plist-get state :buffer))
        (session-id (plist-get state :session-id))
        (turn-id "chat-turn"))
    (with-current-buffer buffer
      (e-dev-perf--profile-spans
       (lambda ()
         (e-chat-render-event
          (list :type 'turn-started :session-id session-id :turn-id turn-id
                :created-at (float-time) :payload nil))
         (dotimes (index 6)
           (e-chat-render-event
            (list :type 'reasoning-delta :session-id session-id :turn-id turn-id
                  :created-at (float-time)
                  :payload (list :type 'reasoning-delta
                                 :content (format "thought %d" index))))
           (e-chat-render-event
            (list :type 'tool-started :session-id session-id :turn-id turn-id
                  :created-at (float-time)
                  :payload (list :id (format "tool-%d" index)
                                 :name "fake"))))
         (e-dev-perf--drain-ui-work buffer))
       '(chat.render-event chat.activity-redraw chat.composer-restore
         chat.composer-insert)))))

(defun e-dev-perf--scenario-chat-ui-work-run (state)
  "Run chat UI-work lifecycle scenario."
  (let* ((buffer (plist-get state :buffer))
         (session-id (plist-get state :session-id))
         (counter (1+ (or (plist-get state :ui-work-counter) 0)))
         (turn-id (format "ui-work-turn-%d" counter))
         (markdown (string-join
                    (cl-loop for index below 40
                             collect (format "- **item %02d** [ref](https://example.test/%02d)"
                                             index
                                             index))
                    "\n")))
    (setf (plist-get state :ui-work-counter) counter)
    (with-current-buffer buffer
      (let ((e-chat-deferred-markdown-threshold-bytes 8)
            (e-chat-deferred-markdown-chunk-lines 1))
        (let ((metrics
               (e-dev-perf--profile-spans
                (lambda ()
                  (e-chat-render-event
                   (list :type 'turn-started
                         :session-id session-id
                         :turn-id turn-id
                         :created-at (float-time)
                         :payload nil))
                  (dotimes (index 12)
                    (e-chat-render-event
                     (list :type 'reasoning-delta
                           :session-id session-id
                           :turn-id turn-id
                           :created-at (float-time)
                           :payload (list :type 'reasoning-delta
                                          :content (format "thought %d" index)))))
                  (e-dev-perf--drain-ui-work buffer)
                  (e-chat-render-event
                   (list :type 'message-added
                         :session-id session-id
                         :turn-id turn-id
                         :created-at (float-time)
                         :payload (list :message
                                        (list :role 'assistant
                                              :turn-id turn-id
                                              :content markdown))))
                  (e-dev-perf--drain-ui-work buffer)
                  (e-chat-render-event
                   (list :type 'turn-finished
                         :session-id session-id
                         :turn-id turn-id
                         :created-at (float-time)
                         :payload nil))
                  (e-dev-perf--drain-ui-work buffer))
                '(chat.render-event chat.activity-redraw chat.composer-restore
                  chat.composer-insert))))
          (let ((pending (e-dev-perf--chat-pending-ui-work-count buffer)))
            (unless (zerop pending)
              (signal 'e-work-error
                      (list (format "UI work left pending after settled turn: %d"
                                    pending))))
            (append metrics
                    (list :ui-work.pending.after-settle pending
                          :ui-work.markdown.bytes (string-bytes markdown)))))))))

(defun e-dev-perf--scenario-final-render-run (state)
  "Run final assistant render scenario."
  (let ((buffer (plist-get state :buffer))
        (session-id (plist-get state :session-id))
        (turn-id "final-turn"))
    (with-current-buffer buffer
      (let ((message (list :role 'assistant
                           :turn-id turn-id
                           :content "# Heading\n\n- item one\n- item two\n\n```elisp\n(message \"hi\")\n```\n\n[link](https://example.test)")))
        (e-dev-perf--profile-spans
         (lambda ()
           (e-chat-render-event
            (list :type 'message-added :session-id session-id :turn-id turn-id
                  :created-at (float-time)
                  :payload (list :message message))))
         '(chat.render-event chat.composer-restore chat.composer-insert))))))

(defun e-dev-perf--scenario-tool-lifecycle-run (_state)
  "Run fake tool lifecycle dispatch scenario."
  (let* ((registry (e-tools-registry-create))
         (events 0)
         (call (list :id "tool-call-1" :name "fake" :arguments nil)))
    (e-tools-register
     registry
     :name "fake"
     :description "Fake performance tool."
     :work (e-tools-cheap-work
            "tool.perf-fake"
            (lambda (_arguments) "ok")))
    (e-dev-perf--profile-spans
     (lambda ()
       (e-tools-start
        registry call
        :on-event (lambda (&rest _args) (setq events (1+ events)))
        :on-done (lambda (_result) nil)
        :on-error (lambda (err) (signal (car err) (cdr err))))
       (accept-process-output nil 0.05))
     nil)
    (list :tool.lifecycle-event.count events
          :tool.dispatch.count 1)))

(defun e-dev-perf--scenario-work-cheap-run (_state)
  "Run cheap work lifecycle start/settle scenario."
  (let ((spec (e-work-spec-create
               :id "perf-cheap-work"
               :description "Cheap work performance fixture."
               :execution 'cheap
               :interactive-policy 'async
               :owner 'e-work
               :runner (lambda (arguments _context)
                         (plist-get arguments :value))))
        (handles nil)
        (finished 0))
    (e-dev-perf--profile-spans
     (lambda ()
       (dotimes (index 100)
         (push
          (e-work-start
           spec
           (list :value index)
           :context '(:turn-id "perf-turn")
           :on-done (lambda (_result)
                      (setq finished (1+ finished))))
          handles)))
     nil)
    (list :work.start.count (length handles)
          :work.handle.count (cl-count-if #'e-work-handle-p handles)
          :work.finished.count finished)))

(defun e-dev-perf--scenario-work-tool-render-run (_state)
  "Run work-backed render tool dispatch scenario."
  (let* ((registry (e-tools-registry-create))
         (done 0)
         (requests nil)
         (spec (e-work-spec-create
                :id "perf-render-tool"
                :description "Render work tool performance fixture."
                :execution 'render
                :interactive-policy 'async
                :owner 'e-work
                :runner (lambda (arguments _context)
                          (plist-get arguments :text)))))
    (e-tools-register
     registry
     :name "perf_render_tool"
     :description "Return text through render work."
     :work spec)
    (let ((metrics
           (e-dev-perf--profile-spans
            (lambda ()
              (dotimes (index 25)
                (e-tools-start
                 registry
                 (list :id (format "call-%d" index)
                       :name "perf_render_tool"
                       :arguments (list :text "ok" :delay 0))
                 :on-request-start (lambda (request)
                                     (push request requests))
                 :on-done (lambda (_result)
                            (setq done (1+ done)))
                 :on-error (lambda (err)
                             (signal (car err) (cdr err)))))
              (let ((deadline (+ (float-time) 1.0)))
                (while (and (< done 25)
                            (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (unless (= done 25)
                (signal 'e-work-error
                        (list "Timed out waiting for perf render work"))))
            '(tool.start))))
      (append
       metrics
       (list :work.tool.request.count (length requests)
             :work.tool.finished.count done)))))

(defun e-dev-perf-register-default-scenarios ()
  "Register built-in performance scenarios."
  (interactive)
  (unless (gethash "session.append-index" e-dev-perf--scenarios)
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "session.append-index"
      :title "Session append and index update"
      :owner 'e-session
      :run #'e-dev-perf--scenario-session-append-run
      :samples 5
      :warmups 1
      :tags '(session)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "session.visible-query"
      :title "Bounded visible session query"
      :owner 'e-session
      :run #'e-dev-perf--scenario-session-visible-query-run
      :samples 5
      :warmups 1
      :tags '(session)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "session.metadata-state"
      :title "Session metadata state lanes"
      :owner 'e-session
      :run #'e-dev-perf--scenario-session-metadata-state-run
      :samples 5
      :warmups 1
      :tags '(session state)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "context.fixture"
      :title "Context assembly fixture"
      :owner 'e-context
      :run #'e-dev-perf--scenario-context-run
      :samples 5
      :warmups 1
      :tags '(context)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "turn.fake-backend"
      :title "Turn startup with fake backend"
      :owner 'e-harness
      :run #'e-dev-perf--scenario-turn-start-run
      :samples 5
      :warmups 1
      :tags '(turn harness)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "chat.activity-burst"
      :title "Chat activity burst rendering"
      :owner 'e-chat
      :setup (lambda (_scenario) (e-dev-perf--chat-buffer-state))
      :run #'e-dev-perf--scenario-chat-activity-run
      :teardown #'e-dev-perf--chat-teardown
      :samples 5
      :warmups 1
      :tags '(chat)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "chat.final-assistant-render"
      :title "Final assistant render fixture"
      :owner 'e-chat
      :setup (lambda (_scenario) (e-dev-perf--chat-buffer-state))
      :run #'e-dev-perf--scenario-final-render-run
      :teardown #'e-dev-perf--chat-teardown
      :samples 5
      :warmups 1
      :tags '(chat)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "chat.ui-work-lifecycle"
      :title "Chat UI work lifecycle"
      :owner 'e-chat
      :setup (lambda (_scenario) (e-dev-perf--chat-buffer-state))
      :run #'e-dev-perf--scenario-chat-ui-work-run
      :teardown #'e-dev-perf--chat-teardown
      :samples 5
      :warmups 1
      :tags '(chat ui-work render)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "chat.status-context-cost"
      :title "Chat status context construction"
      :owner 'e-chat
      :setup (lambda (_scenario) (e-dev-perf--chat-buffer-state))
      :run #'e-dev-perf--scenario-chat-status-run
      :teardown #'e-dev-perf--chat-teardown
      :samples 5
      :warmups 1
      :tags '(chat status context)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "tool.lifecycle-dispatch"
      :title "Tool lifecycle dispatch fixture"
      :owner 'e-tools
      :run #'e-dev-perf--scenario-tool-lifecycle-run
      :samples 5
      :warmups 1
      :tags '(tools)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "work.lifecycle-cheap"
      :title "Cheap work lifecycle fixture"
      :owner 'e-work
      :run #'e-dev-perf--scenario-work-cheap-run
      :samples 5
      :warmups 1
      :tags '(work lifecycle)))
    (e-dev-perf-register-scenario
     (e-dev-perf-scenario-create
      :id "work.tool-render-dispatch"
      :title "Work-backed render tool dispatch fixture"
      :owner 'e-work
      :run #'e-dev-perf--scenario-work-tool-render-run
      :samples 5
      :warmups 1
      :tags '(work tools render)))))

(provide 'e-dev-perf)

;;; e-dev-perf.el ends here
