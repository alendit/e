;;; e-process-reporting.el --- Durable parent-side process markers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A small parent-facing observation tool backed by the active session store.
;; Marker capture keeps only agent judgment and links to redacted session
;; telemetry.  Triage and extraction accounting append separate session facts.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-actions)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-layers)
(require 'e-session)
(require 'e-telemetry)
(require 'e-tools)

(defgroup e-process-reporting nil
  "Durable process observations for e."
  :group 'e
  :prefix "e-process-reporting-")

(defconst e-process-reporting-instructions
  "Call process_marker to record a task-relative process point the instant it happens, across the full range of signals -- not just tool failures. A process point is any of: something failed (failure); something caused friction, a retry, or a workaround (friction, workaround); you accepted a correction, including one to your own output or reasoning (correction); you repeated the same step several times (repetition); you found a reusable insight or an approach that worked well (effective, success); an operation you needed did not exist (missing-operation); a step was notably slow (performance). Record one marker per distinct point; do not weigh whether it is important enough -- these signals always qualify, and a self-corrected slip or an accepted correction counts as much as a tool error. Before you finalize ANY turn, scan the WHOLE turn for unrecorded process points of every signal above and record them now; most turns have at least one, so 'none' should be rare and deliberate. Do not narrate the marker or repeat unchanged evidence."
  "Minimal parent-facing process marker guidance.")

(defconst e-process-reporting-signals
  '("failure" "friction" "correction" "workaround" "repetition"
    "success" "effective" "missing-operation" "performance")
  "Accepted task-relative process marker signals.")

(defconst e-process-reporting-outcomes
  '("runtime-defect" "missing-capability" "judgment-procedure"
    "deterministic-procedure" "unattended-work" "project-policy"
    "duplicate" "understood" "not-actionable")
  "Accepted first-slice triage outcome kinds.")

(defconst e-process-reporting-triage-statuses
  '("open" "routed" "closed" "rejected")
  "Accepted first-slice triage statuses.")

(defun e-process-reporting--timestamp ()
  "Return an ISO-8601 UTC timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun e-process-reporting--session-store (context)
  "Return the active session store from action CONTEXT."
  (let ((harness (plist-get context :harness)))
    (unless (e-harness-p harness)
      (user-error "Process reporting requires an active harness"))
    (e-harness-sessions harness)))

(defun e-process-reporting--session-id (context)
  "Return the active session id from action CONTEXT."
  (or (plist-get context :session-id)
      (user-error "Process reporting requires an active session")))

(defun e-process-reporting--reports (context)
  "Return process reports owned by CONTEXT's active session."
  (e-session-process-reports
   (e-process-reporting--session-store context)
   (e-process-reporting--session-id context)))

(defun e-process-reporting--append (context report)
  "Append process REPORT to CONTEXT's active session."
  (e-session-append-process-report
   (e-process-reporting--session-store context)
   (e-process-reporting--session-id context)
   report))

(defun e-process-reporting--records-of-type (reports report-type)
  "Return REPORTS whose report type equals REPORT-TYPE."
  (seq-filter
   (lambda (report)
     (equal (plist-get report :report-type) report-type))
   reports))

(defun e-process-reporting--public-record (record)
  "Return a stable public copy of internal session RECORD."
  (let ((copy (copy-tree record))
        (report-type (plist-get record :report-type)))
    (when report-type
      (plist-put copy :type report-type)
      (cl-remf copy :report-type))
    (when-let ((chain (plist-get copy :trigger-chain)))
      (when (vectorp chain)
        (plist-put copy :trigger-chain (append chain nil))))
    copy))

(defun e-process-reporting--marker (reports marker-id)
  "Return marker MARKER-ID from REPORTS."
  (seq-find
   (lambda (report)
     (and (equal (plist-get report :report-type) "marker")
          (equal (plist-get report :marker-id) marker-id)))
   reports))

(defun e-process-reporting--triage-records (reports marker-id)
  "Return triage records for MARKER-ID from REPORTS."
  (seq-filter
   (lambda (report)
     (and (equal (plist-get report :report-type) "triage")
          (equal (plist-get report :marker-id) marker-id)))
   reports))

(defun e-process-reporting--string-argument (arguments key &optional required)
  "Return string KEY from ARGUMENTS, enforcing REQUIRED."
  (let ((value (plist-get arguments key)))
    (cond
     ((and (stringp value) (not (string-empty-p (string-trim value))))
      (string-trim value))
     ((not required) nil)
     (t (signal 'wrong-type-argument (list 'non-empty-string-p key))))))

(defun e-process-reporting--member-argument (arguments key values)
  "Return string KEY from ARGUMENTS when it belongs to VALUES."
  (let ((value (e-process-reporting--string-argument arguments key t)))
    (unless (member value values)
      (user-error "Unsupported %s: %s" key value))
    value))

(defun e-process-reporting--note (arguments)
  "Return validated short marker note from ARGUMENTS."
  (let ((note (e-process-reporting--string-argument arguments :note t)))
    (when (or (> (length note) 280) (string-match-p "[\n\r]" note))
      (user-error "Process marker note must be one short line (280 characters maximum)"))
    note))

(defun e-process-reporting--activity-events (harness session-id)
  "Return activity events for HARNESS SESSION-ID."
  (when (and (e-harness-p harness) (stringp session-id))
    (e-session-activity-events (e-harness-sessions harness) session-id)))

(defun e-process-reporting--own-event-p (event)
  "Return non-nil when EVENT belongs to process marker capture itself."
  (let* ((type (plist-get event :event-type))
         (payload (plist-get event :payload))
         (tool-name (or (plist-get payload :name)
                        (plist-get (plist-get payload :tool-call) :name))))
    (or (and (memq type '(tool-started tool-finished))
             (equal tool-name "process_marker"))
        (and (memq type '(action-started action-finished action-failed))
             (equal (format "%s" (plist-get payload :capability-id))
                    "process-reporting")))))

(defun e-process-reporting--event-parent-tool-call-id (event)
  "Return EVENT's stable enclosing tool call id, if any."
  (plist-get (plist-get event :payload) :parent-tool-call-id))

(defun e-process-reporting--trigger-events (harness session-id turn-id)
  "Return the explicit latest completed operation chain before capture."
  (let* ((events (e-process-reporting--activity-events harness session-id))
         (candidates
          (seq-filter
           (lambda (event)
             (and (equal (plist-get event :turn-id) turn-id)
                  (memq (plist-get event :event-type)
                        '(tool-started tool-finished action-started
                          action-finished action-failed))
                  (not (e-process-reporting--own-event-p event))))
           events))
         (latest
          (seq-find
           (lambda (event)
             (memq (plist-get event :event-type)
                   '(tool-finished action-finished action-failed)))
           (reverse candidates)))
         (latest-type (plist-get latest :event-type))
         (latest-tool-p (eq latest-type 'tool-finished))
         (latest-tool-id (and latest-tool-p
                              (e-process-reporting--event-call-id latest)))
         (nested-action
          (and latest-tool-id
               (seq-find
                (lambda (event)
                  (and (memq (plist-get event :event-type)
                             '(action-finished action-failed))
                       (equal
                        (e-process-reporting--event-parent-tool-call-id event)
                        latest-tool-id)))
                (reverse candidates))))
         (parent-id
          (and (not latest-tool-p) latest
               (e-process-reporting--event-parent-tool-call-id latest)))
         (parent
          (and parent-id
               (seq-find
                (lambda (event)
                  (and (memq (plist-get event :event-type)
                             '(tool-finished tool-started))
                       (equal (e-process-reporting--event-call-id event)
                              parent-id)))
                (reverse candidates)))))
    (cond
     (nested-action (list nested-action latest))
     (parent (list latest parent))
     (latest (list latest)))))

(defun e-process-reporting--event-call-id (event)
  "Return stable tool or action call id from EVENT."
  (let* ((payload (plist-get event :payload))
         (call (plist-get payload :tool-call)))
    (or (plist-get payload :action-call-id)
        (plist-get payload :tool-call-id)
        (plist-get payload :id)
        (plist-get call :id)
        (plist-get (plist-get payload :result) :tool-call-id))))

(defun e-process-reporting--event-name (event)
  "Return capability/action or tool name from EVENT."
  (let* ((payload (plist-get event :payload))
         (capability (plist-get payload :capability-id))
         (action (plist-get payload :action)))
    (if (and capability action)
        (format "%s/%s" capability action)
      (or (plist-get payload :name)
          (plist-get (plist-get payload :tool-call) :name)
          (plist-get (plist-get payload :result) :name)))))

(defun e-process-reporting--event-preview (event events)
  "Return safe mechanical evidence projection for EVENT among EVENTS."
  (when event
    (let* ((payload (plist-get event :payload))
           (result (plist-get payload :result))
           (call-id (e-process-reporting--event-call-id event))
           (started
            (and call-id
                 (seq-find
                  (lambda (candidate)
                    (and (memq (plist-get candidate :event-type)
                               '(tool-started action-started))
                         (equal (e-process-reporting--event-call-id candidate)
                                call-id)))
                  (reverse events))))
           (started-payload (plist-get started :payload))
           (arguments (or (plist-get payload :arguments)
                          (plist-get (plist-get payload :tool-call) :arguments)
                          (plist-get started-payload :arguments)
                          (plist-get (plist-get started-payload :tool-call)
                                     :arguments))))
      (list :activity-event-id (plist-get event :id)
            :event-type (format "%s" (plist-get event :event-type))
            :call-id call-id
            :parent-tool-call-id
            (e-process-reporting--event-parent-tool-call-id event)
            :name (e-process-reporting--event-name event)
            :status (or (plist-get payload :status)
                        (plist-get result :status))
            :arguments-preview
            (and arguments
                 (if (and (listp arguments) (plist-get arguments :redaction-policy))
                     arguments
                   (e-telemetry-preview arguments)))
            :result-preview
            (and result
                 (e-telemetry-preview (plist-get result :content)))
            :error-preview
            (and (or (plist-get payload :message)
                     (plist-get payload :message-preview)
                     (eq (plist-get result :status) 'error)
                     (equal (plist-get result :status) "error"))
                 (or (plist-get payload :message-preview)
                     (e-telemetry-preview
                      (or (plist-get payload :message)
                          (plist-get result :content)))))))))

(defun e-process-reporting--trigger-chain (harness session-id turn-id)
  "Return durable previews for the relevant explicit operation chain."
  (let ((events (e-process-reporting--activity-events harness session-id)))
    (mapcar (lambda (event)
              (e-process-reporting--event-preview event events))
            (e-process-reporting--trigger-events harness session-id turn-id))))

(defun e-process-reporting--tool-call-context (context)
  "Return current process marker tool call from action CONTEXT."
  (let* ((outer (plist-get context :context))
         (tool-call (plist-get outer :tool-call)))
    (when (and (listp tool-call)
               (equal (plist-get tool-call :name) "process_marker"))
      tool-call)))

(defun e-process-reporting--checkpoint-entry-id (harness session-id tool-call-id)
  "Return transcript head immediately before TOOL-CALL-ID."
  (when tool-call-id
    (let ((message
           (seq-find
            (lambda (item)
              (and (eq (plist-get item :role) 'tool-call)
                   (equal (plist-get (plist-get item :content) :id)
                          tool-call-id)))
            (e-harness-messages harness session-id))))
      (plist-get message :parent-id))))

(defun e-process-reporting--current-request-id (harness session-id turn-id)
  "Return provider request id only while it is active in TURN-ID."
  (let ((states (make-hash-table :test 'equal))
        latest)
    (dolist (event (e-process-reporting--activity-events harness session-id))
      (when (equal (plist-get event :turn-id) turn-id)
        (let* ((type (plist-get event :event-type))
               (payload (plist-get event :payload))
               (id (plist-get payload :provider-request-id)))
          (pcase type
            ('provider-request-started
             (setq latest id)
             (puthash id t states))
            ('provider-request-finished
             (puthash id nil states))))))
    (and latest (gethash latest states) latest)))

(defun e-process-reporting--evidence-id (signal note session-id turn-id trigger)
  "Return content-keyed identity for marker evidence."
  (secure-hash
   'sha256
   (prin1-to-string
    (list :signal signal :note note :session-id session-id :turn-id turn-id
          :trigger-event-id (plist-get trigger :activity-event-id)
          :trigger-call-id (plist-get trigger :call-id)))))

(defun e-process-reporting--latest-triage (reports marker-id)
  "Return latest triage record for MARKER-ID in REPORTS."
  (car (last (e-process-reporting--triage-records reports marker-id))))

(defun e-process-reporting--terminal-evidence-marker (reports evidence-id)
  "Return marker for terminal EVIDENCE-ID, if one exists."
  (seq-find
   (lambda (marker)
     (when (equal (plist-get marker :evidence-id) evidence-id)
       (let ((triage (e-process-reporting--latest-triage
                      reports (plist-get marker :marker-id))))
         (member (plist-get triage :status) '("routed" "closed" "rejected")))))
   (e-process-reporting--records-of-type reports "marker")))

(defun e-process-reporting-mark (arguments context)
  "Append a session-owned process marker described by ARGUMENTS and CONTEXT."
  (let* ((signal (e-process-reporting--member-argument
                  arguments :signal e-process-reporting-signals))
         (note (e-telemetry-redact-string
                (e-process-reporting--note arguments)))
         (harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (turn-id (plist-get context :turn-id))
         (tool-call (e-process-reporting--tool-call-context context))
         (tool-call-id (plist-get tool-call :id))
         (trigger-chain (e-process-reporting--trigger-chain
                         harness session-id turn-id))
         (trigger (car trigger-chain))
         (evidence-id (e-process-reporting--evidence-id
                       signal note session-id turn-id trigger)))
    (let ((reports (e-process-reporting--reports context)))
      (if-let ((terminal (e-process-reporting--terminal-evidence-marker
                          reports evidence-id)))
          (list :marker-id (plist-get terminal :marker-id)
                :evidence-id evidence-id
                :suppressed t)
        (let* ((marker-id (e-session-generate-ulid))
               (project-root
                (e-harness-project-root harness session-id turn-id))
               (record
                (list :report-type "marker"
                      :id marker-id
                      :marker-id marker-id
                      :evidence-id evidence-id
                      :created-at (e-process-reporting--timestamp)
                      :signal signal
                      :note note
                      :session-id session-id
                      :turn-id turn-id
                      :project-root project-root
                      :session-uri
                      (format "session://e/sessions/%s/" session-id)
                      :messages-uri
                      (format "session://e/sessions/%s/messages" session-id)
                      :activity-uri
                      (format "session://e/sessions/%s/activity" session-id)
                      :process-reports-uri
                      (format "session://e/sessions/%s/process-reports"
                              session-id)
                      :checkpoint-entry-id
                      (e-process-reporting--checkpoint-entry-id
                       harness session-id tool-call-id)
                      :provider-request-id
                      (e-process-reporting--current-request-id
                       harness session-id turn-id)
                      :tool-call-id tool-call-id
                      :action-call-id (plist-get context :action-call-id)
                      :trigger trigger
                      :trigger-chain (vconcat trigger-chain))))
          (e-process-reporting--public-record
           (e-process-reporting--append context record)))))))

(defun e-process-reporting-list (context &optional arguments)
  "Return current-session marker summaries, newest-first."
  (let* ((reports (e-process-reporting--reports context))
         (status (plist-get arguments :status))
         (markers
          (reverse
           (e-process-reporting--records-of-type reports "marker")))
         result)
    (dolist (marker markers (nreverse result))
      (let* ((triage (e-process-reporting--latest-triage
                      reports (plist-get marker :marker-id)))
             (current-status (or (plist-get triage :status) "open")))
        (when (or (null status) (equal status current-status))
          (push (list :marker-id (plist-get marker :marker-id)
                      :evidence-id (plist-get marker :evidence-id)
                      :created-at (plist-get marker :created-at)
                      :signal (plist-get marker :signal)
                      :note (plist-get marker :note)
                      :session-id (plist-get marker :session-id)
                      :status current-status
                      :outcome (plist-get triage :outcome)
                      :target-reference (plist-get triage :target-reference))
                result))))))

(defun e-process-reporting-read (context marker-id)
  "Return current-session marker MARKER-ID and its appended records."
  (let* ((reports (e-process-reporting--reports context))
         (marker (e-process-reporting--marker reports marker-id)))
    (unless marker
      (user-error "Unknown process marker: %s" marker-id))
    (list :marker (e-process-reporting--public-record marker)
          :triage (mapcar #'e-process-reporting--public-record
                          (e-process-reporting--triage-records
                           reports marker-id))
          :extractions
          (mapcar
           #'e-process-reporting--public-record
           (seq-filter
            (lambda (record)
              (and (equal (plist-get record :report-type) "extraction")
                   (member marker-id (plist-get record :marker-ids))))
            reports)))))

(defun e-process-reporting-triage (arguments context)
  "Append a current-session triage decision from ARGUMENTS."
  (let ((marker-id (e-process-reporting--string-argument
                    arguments :marker-id t))
        (outcome (e-process-reporting--member-argument
                  arguments :outcome e-process-reporting-outcomes))
        (status (e-process-reporting--member-argument
                 arguments :status e-process-reporting-triage-statuses))
        (decision-note
         (e-telemetry-redact-string
          (e-process-reporting--string-argument
           arguments :decision-note t)))
        (target (e-process-reporting--string-argument
                 arguments :target-reference)))
    (unless (e-process-reporting--marker
             (e-process-reporting--reports context) marker-id)
      (user-error "Unknown process marker: %s" marker-id))
    (e-process-reporting--public-record
     (e-process-reporting--append
      context
      (append
       (list :report-type "triage"
             :marker-id marker-id
             :created-at (e-process-reporting--timestamp)
             :outcome outcome
             :status status
             :decision-note decision-note)
       (when target
         (list :target-reference
               (e-telemetry-redact-string target))))))))

(defun e-process-reporting--string-vector (arguments key &optional required)
  "Return KEY from ARGUMENTS as a vector of strings."
  (let* ((value (plist-get arguments key))
         (items (cond ((vectorp value) (append value nil))
                      ((listp value) value)
                      (t nil))))
    (when (and required (null items))
      (user-error "%s requires at least one value" key))
    (unless (cl-every #'stringp items)
      (user-error "%s must contain only strings" key))
    (vconcat (mapcar #'e-telemetry-redact-string items))))

(defun e-process-reporting--token-usage (value)
  "Return a narrow numeric extraction token usage schema from VALUE."
  (let (result)
    (dolist (key '(:input-tokens :cached-input-tokens :output-tokens
                   :reasoning-output-tokens :total-tokens))
      (when-let ((number (plist-get value key)))
        (unless (numberp number)
          (user-error "%s must be numeric" key))
        (setq result (append result (list key number)))))
    result))

(defun e-process-reporting-record-extraction (arguments context)
  "Append offline extraction cost attribution from ARGUMENTS and CONTEXT."
  (let* ((marker-ids
          (append (e-process-reporting--string-vector
                   arguments :marker-ids t) nil))
         (method
          (e-telemetry-redact-string
           (e-process-reporting--string-argument
            arguments :estimation-method t)))
         (session-evidence
          (e-process-reporting--string-vector arguments :session-evidence))
         (request-ids
          (e-process-reporting--string-vector
           arguments :provider-request-ids))
         (usage (e-process-reporting--token-usage
                 (plist-get arguments :token-usage))))
    (let ((reports (e-process-reporting--reports context)))
      (dolist (marker-id marker-ids)
        (unless (e-process-reporting--marker reports marker-id)
          (user-error "Unknown process marker: %s" marker-id))))
    (e-process-reporting--public-record
     (e-process-reporting--append
      context
      (list :report-type "extraction"
            :created-at (e-process-reporting--timestamp)
            :marker-ids marker-ids
            :session-evidence session-evidence
            :provider-request-ids request-ids
            :token-usage usage
            :estimation-method method
            :session-id (plist-get context :session-id)
            :turn-id (plist-get context :turn-id))))))

(defun e-process-reporting--request-usage (events request-id)
  "Return provider token usage in EVENTS joined to REQUEST-ID."
  (when-let ((event
              (seq-find
               (lambda (candidate)
                 (and (eq (plist-get candidate :event-type) 'token-usage)
                      (equal (plist-get (plist-get candidate :payload)
                                        :provider-request-id)
                             request-id)))
               events)))
    (copy-tree (plist-get event :payload))))

(defun e-process-reporting--shape-bytes (shape key)
  "Return byte count under KEY in request SHAPE."
  (or (plist-get (plist-get shape key) :bytes) 0))

(defun e-process-reporting--request-cost-entry (event events)
  "Return one honest paired request attribution entry."
  (let* ((payload (plist-get event :payload))
         (shape (plist-get payload :request-shape))
         (actual (e-process-reporting--shape-bytes shape :actual-shape))
         (paired (e-process-reporting--shape-bytes shape :paired-shape))
         (without-passive
          (e-process-reporting--shape-bytes shape :without-passive-shape))
         (without-active
          (e-process-reporting--shape-bytes shape :without-active-shape))
         (causes
          (or (plist-get payload :caused-by-tool-calls)
              (when-let ((name (plist-get payload :caused-by-tool-name)))
                (list (list :id (plist-get payload :caused-by-tool-call-id)
                            :name name)))))
         (marker-causes
          (seq-filter (lambda (cause)
                        (equal (plist-get cause :name) "process_marker"))
                      causes)))
    (list :provider-request-id (plist-get payload :provider-request-id)
          :provider-request-ordinal
          (plist-get payload :provider-request-ordinal)
          :caused-by-tool-call-id
          (plist-get payload :caused-by-tool-call-id)
          :caused-by-tool-calls (copy-tree causes)
          :marker-follow-up (not (null marker-causes))
          :marker-follow-up-call-ids
          (vconcat (mapcar (lambda (cause) (plist-get cause :id))
                           marker-causes))
          :actual-bytes actual
          :paired-bytes paired
          :direct-context-delta-bytes (- actual paired)
          :passive-surface-bytes (- actual without-passive)
          :active-marker-bytes (- actual without-active)
          :token-usage
          (e-process-reporting--request-usage
           events (plist-get payload :provider-request-id))
          :serialization (plist-get shape :serialization)
          :tokenizer-revision (plist-get shape :tokenizer-revision))))

(defun e-process-reporting-cost-report (context)
  "Return paired request-shape accounting for CONTEXT session markers."
  (let* ((harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (events (e-process-reporting--activity-events harness session-id))
         (requests
          (seq-filter
           (lambda (event)
             (eq (plist-get event :event-type) 'provider-request-started))
           events))
         (entries
          (mapcar (lambda (event)
                    (e-process-reporting--request-cost-entry event events))
                  requests))
         (markers
          (e-process-reporting--records-of-type
           (e-process-reporting--reports context) "marker"))
         (delta-bytes
          (apply #'+ (mapcar (lambda (entry)
                               (plist-get entry :direct-context-delta-bytes))
                             entries))))
    (list :scope "request-shape-counterfactual"
          :session-id session-id
          :provider-request-count (length requests)
          :marker-count (length markers)
          :marker-follow-up-request-count
          (cl-count-if (lambda (entry)
                         (plist-get entry :marker-follow-up))
                       entries)
          :requests entries
          :direct-context-delta-bytes delta-bytes
          :estimation-method "backend-neutral-serialized-utf-8-bytes"
          :provider-tokenizer-used nil
          :behavioral-estimate nil
          :note
          "Paired byte deltas preserve enough request data for post-hoc provider serialization/tokenization; they do not claim provider-token or behavioral overhead.")))

(defconst e-process-reporting--marker-parameters
  `(:type "object"
    :properties
    (:signal (:type "string" :enum ,(vconcat e-process-reporting-signals))
     :note (:type "string" :minLength 1 :maxLength 280
            :nonBlank t :singleLine t))
    :required ["signal" "note"]
    :additionalProperties :json-false)
  "Small process marker input schema.")

(defconst e-process-reporting--marker-id-parameters
  '(:type "object"
    :properties (:marker-id (:type "string"))
    :required ["marker-id"])
  "Marker lookup action schema.")

(defconst e-process-reporting--triage-parameters
  `(:type "object"
    :properties
    (:marker-id (:type "string")
     :outcome (:type "string" :enum ,(vconcat e-process-reporting-outcomes))
     :status (:type "string" :enum ,(vconcat e-process-reporting-triage-statuses))
     :decision-note (:type "string")
     :target-reference (:type "string"))
    :required ["marker-id" "outcome" "status" "decision-note"])
  "Triage action schema.")

(defun e-process-reporting--action (id parameters runner)
  "Return cheap process reporting action ID with PARAMETERS and RUNNER."
  (e-action-cheap-create
   :id id
   :owner 'process-reporting
   :parameters parameters
   :requires-session t
   :runner runner))

(defun e-process-reporting-register-tool (registry &rest _context)
  "Register the tiny process marker tool in REGISTRY."
  (e-tools-register
   registry
   :name "process_marker"
   :description "Save one task-relative tool or action observation."
   :parameters e-process-reporting--marker-parameters
   :blocking-class 'cheap
   :handler
   (lambda (arguments)
     (e-actions-call 'process-reporting :mark arguments)
     "ok")))

(defun e-process-reporting-capability-create ()
  "Create the session-owned process reporting capability."
  (e-capability-create
   :id 'process-reporting
   :name "Process Reporting"
   :instruction-priority 245
   :instructions e-process-reporting-instructions
   :tools (list #'e-process-reporting-register-tool)
   :actions
   (list
    :mark
    (e-process-reporting--action
     "process_marker" e-process-reporting--marker-parameters
     #'e-process-reporting-mark)
    :list
    (e-process-reporting--action
     "process_marker_list"
     '(:type "object"
       :properties (:status (:type "string"
                             :enum ["open" "routed" "closed" "rejected"])))
     (lambda (arguments context)
       (e-process-reporting-list context arguments)))
    :read
    (e-process-reporting--action
     "process_marker_read" e-process-reporting--marker-id-parameters
     (lambda (arguments context)
       (e-process-reporting-read
        context (e-process-reporting--string-argument
                 arguments :marker-id t))))
    :triage
    (e-process-reporting--action
     "process_marker_triage" e-process-reporting--triage-parameters
     #'e-process-reporting-triage)
    :record-extraction
    (e-process-reporting--action
     "process_marker_extraction"
     '(:type "object"
       :properties
       (:marker-ids (:type "array" :items (:type "string"))
        :session-evidence (:type "array" :items (:type "string"))
        :provider-request-ids (:type "array" :items (:type "string"))
        :token-usage (:type "object")
        :estimation-method (:type "string"))
       :required ["marker-ids" "estimation-method"])
     #'e-process-reporting-record-extraction)
    :cost-report
    (e-process-reporting--action
     "process_marker_cost_report" nil
     (lambda (_arguments context)
       (e-process-reporting-cost-report context))))))

(defun e-process-reporting-layer-create ()
  "Create the parent-side process reporting layer."
  (e-layer-create
   :id 'process-reporting
   :name "Process Reporting"
   :capabilities (list (e-process-reporting-capability-create))))

(provide 'e-process-reporting)

;;; e-process-reporting.el ends here
