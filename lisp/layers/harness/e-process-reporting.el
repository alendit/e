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
(require 'e-harness-activity)
(require 'e-json)
(require 'e-layers)
(require 'e-session)
(require 'e-session-async)
(require 'e-telemetry)
(require 'e-tools)
(require 'e-work)

(defgroup e-process-reporting nil
  "Durable process observations for e."
  :group 'e
  :prefix "e-process-reporting-")

(defconst e-process-reporting-instructions
  "Call process_marker only when a substantive multi-step or complex flow reveals a reusable process observation: a recurring failure or friction pattern, a workaround or method likely to help future tasks, a materially blocking missing capability, or a notable performance bottleneck. Do not mark routine successful steps, one-off minor failures or retries, ordinary corrections, self-corrections, or requests to change marker frequency. Do not scan every turn for a marker; most turns should have none. Record at most one marker for the same underlying observation, when its reusable significance becomes clear. Do not narrate the marker or repeat unchanged evidence."
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

(defconst e-process-reporting-record-limit 256
  "Maximum process-report records consumed by one reporting action.")
(defconst e-process-reporting-marker-association-limit 64
  "Maximum raw marker identities accepted by one extraction action.")

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

(defun e-process-reporting--terminal-work-p (work)
  "Return non-nil when WORK has reached a terminal state."
  (memq (plist-get (e-work-status work) :state)
        '(finished failed cancelled)))

(defun e-process-reporting--forward-result (parent value &optional transform)
  "Settle PARENT from VALUE, applying TRANSFORM without blocking.

VALUE and TRANSFORM's result may each be an `e-work' handle.  The currently
owned child is the only process-local coordination retained by PARENT."
  (let (child)
    (setf (e-work-handle-cancel-function parent)
          (lambda (_handle)
            (when (and (e-work-handle-p child)
                       (not (e-process-reporting--terminal-work-p child)))
              (e-work-cancel child))
            t))
    (cl-labels
        ((advance
          (current mapper)
          (unless (e-process-reporting--terminal-work-p parent)
            (if (e-work-handle-p current)
                (progn
                  (setq child current)
                  (e-work-on-settle
                   current
                   (lambda (settled)
                     (unless (e-process-reporting--terminal-work-p parent)
                       (pcase (plist-get (e-work-status settled) :state)
                         ('finished
                          (advance (plist-get (e-work-status settled) :result)
                                   mapper))
                         ('failed
                          (e-work-fail
                           parent (plist-get (e-work-status settled) :error)))
                         ('cancelled (e-work-cancel parent)))))))
              (condition-case err
                  (let ((next (if mapper (funcall mapper current) current)))
                    (if (e-work-handle-p next)
                        (advance next nil)
                      (e-work-finish parent next)))
                ((error quit) (e-work-fail parent err)))))))
      (advance value transform)))
  parent)

(defun e-process-reporting--composition-work (id context source transform)
  "Start cooperative reporting work ID from SOURCE and TRANSFORM.

SOURCE is invoked only after the work starts.  Neither SOURCE nor TRANSFORM
may synchronously join child work; returned child handles are bridged by
terminal callbacks."
  (let* ((spec
          (e-work-spec-create
           :id id :description "Run one bounded process-reporting operation."
           :execution 'cooperative :interactive-policy 'async
           :owner 'process-reporting
           :runner
           (lambda (parent _arguments _work-context)
             (condition-case err
                 (e-process-reporting--forward-result
                  parent (funcall source) transform)
               ((error quit) (e-work-fail parent err)))
             :deferred)))
         (work (e-work-prepare
                spec nil :context
                (list :domain-ref (e-process-reporting--session-id context)
                      :work-kind 'process-reporting))))
    (e-work-start-prepared work :arguments nil)
    work))

(cl-defun e-process-reporting--ephemeral-reports
    (store session-id &key (order 'newest) (limit e-process-reporting-record-limit)
           record-id record-ids parent-id)
  "Return a consumer-shaped bounded report list for explicit local STORE."
  (let* ((ids (and record-ids
                   (if (vectorp record-ids) (append record-ids nil) record-ids)))
         (reports
          (seq-filter
           (lambda (report)
             (and (or (null record-id)
                      (equal (plist-get report :id) record-id))
                  (or (null ids)
                      (member (plist-get report :id) ids))
                  (or (null parent-id)
                      (equal (plist-get report :parent-id) parent-id))))
           (copy-tree
            (e-session-aggregate-process-reports store session-id) t))))
    (seq-take (if (eq order 'newest) (reverse reports) reports) limit)))

(cl-defun e-process-reporting--with-ephemeral-reports
    (id context transform &key (order 'newest)
        (limit e-process-reporting-record-limit) record-id record-ids parent-id)
  "Run TRANSFORM on explicit ephemeral reports as work ID.

Ordinary SQLite callers use the semantic projection queries below; this
aggregate-backed helper is intentionally limited to the in-memory store."
  (when (e-session-async-enabled-p
         (e-process-reporting--session-store context))
    (error "Ephemeral process-report query reached async SQLite"))
  (e-process-reporting--composition-work
   id context
   (lambda ()
     (e-process-reporting--ephemeral-reports
      (e-process-reporting--session-store context)
      (e-process-reporting--session-id context)
      :order order :limit limit
      :record-id record-id :record-ids record-ids :parent-id parent-id))
   (lambda (reports)
     ;; Domain reducers consume chronological subsets.  The physical
     ;; newest-first page is reversed only after the query has bounded it.
     (funcall transform
              (if (eq order 'newest) (reverse reports) reports)))))

(defun e-process-reporting--with-canonical-marker
    (id context marker-id transform)
  "Run TRANSFORM on the exact canonical MARKER-ID record as work ID.

SQLite selects the semantic marker association before its exact bound."
  (let* ((store (e-process-reporting--session-store context))
         (session-id (e-process-reporting--session-id context)))
    (if (e-session-async-enabled-p store)
        (e-process-reporting--composition-work
         id context
         (lambda ()
           (e-session-async-process-report-marker
            store session-id marker-id))
         (lambda (result)
           (funcall transform (plist-get result :marker))))
      (e-process-reporting--with-ephemeral-reports
       id context
       (lambda (reports)
         (funcall transform (e-process-reporting--marker reports marker-id)))
       :order 'oldest :limit 1 :record-id marker-id))))

(defun e-process-reporting--with-latest-triage
    (id context marker-id transform)
  "Run TRANSFORM on MARKER-ID's exact newest related triage as work ID."
  (let* ((store (e-process-reporting--session-store context))
         (session-id (e-process-reporting--session-id context)))
    (if (e-session-async-enabled-p store)
        (e-process-reporting--composition-work
         id context
         (lambda ()
           (e-session-async-process-report-triage-page
            store session-id marker-id :limit 1))
         (lambda (result)
           (funcall transform (car (plist-get result :triage)))))
      (e-process-reporting--with-ephemeral-reports
       id context
       (lambda (reports)
         (funcall transform
                  (car (e-process-reporting--triage-records reports marker-id))))
       :order 'newest :limit 1 :parent-id marker-id))))

(defun e-process-reporting--verify-markers (context marker-ids continuation)
  "Verify bounded MARKER-IDS and invoke CONTINUATION cooperatively."
  (let ((ids (copy-sequence marker-ids)))
    (cl-labels
        ((step
          (remaining)
          (if (null remaining)
              (funcall continuation)
            (e-process-reporting--with-canonical-marker
             "process-reporting.verify-marker" context (car remaining)
             (lambda (marker)
               (unless marker
                 (user-error "Unknown process marker: %s" (car remaining)))
               (step (cdr remaining)))))))
      (step ids))))

(defun e-process-reporting--append (context report)
  "Append process REPORT and settle from its persistence acknowledgement."
  (e-process-reporting--composition-work
   "process-reporting.append" context
   (lambda ()
     (e-session-append-process-report
      (e-process-reporting--session-store context)
      (e-process-reporting--session-id context)
      report))
   #'e-process-reporting--public-record))

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

(defconst e-process-reporting--canonical-string-keys
  '(:type :report-type :id :marker-id :evidence-id :created-at :signal :note
    :session-id :turn-id :project-root :session-uri :messages-uri
    :activity-uri :process-reports-uri :provider-request-id :tool-call-id
    :action-call-id :outcome :status :decision-note :target-reference
    :estimation-method :scope :measurement-status :serialization
    :tokenizer-revision :model :reasoning-effort :prompt-cache-key-sha256
    :event-type :call-id :name :activity-event-id :parent-tool-call-id)
  "Known process-reporting result fields whose values are strings.
Symbols from activity and persistence internals are deliberately projected to
their textual API identifiers at the action boundary.")

(defconst e-process-reporting--canonical-number-keys
  '(:provider-request-count :measured-request-count :marker-count
    :marker-follow-up-request-count :provider-request-ordinal :actual-bytes
    :paired-bytes :direct-context-delta-bytes :passive-surface-bytes
    :active-marker-bytes :input-tokens :cached-input-tokens
    :cache-creation-input-tokens :output-tokens :reasoning-output-tokens
    :total-tokens :bytes :message-count)
  "Known process-reporting result fields whose values are numeric.")

(defconst e-process-reporting--canonical-boolean-keys
  '(:suppressed :marker-follow-up :provider-tokenizer-used
    :behavioral-estimate :prompt-cache-key-present :marker-follow-up)
  "Known process-reporting result fields whose values are booleans.")

(defconst e-process-reporting--canonical-array-keys
  '(:trigger-chain :marker-ids :session-evidence :provider-request-ids
    :marker-follow-up-call-ids :requests :triage :extractions
    :caused-by-tool-calls)
  "Known process-reporting result fields whose values are arrays.")

(defconst e-process-reporting--canonical-object-keys
  '(:trigger :marker :shape :actual-shape :without-passive-shape
    :without-active-shape :paired-shape :token-usage)
  "Known process-reporting result fields whose values are objects.")

(defconst e-process-reporting--canonical-text-keys
  '(:arguments-preview :result-preview :error-preview :content)
  "Known process-reporting result fields that carry textual diagnostics.")

(defun e-process-reporting--canonical-result-value (value)
  "Project one known process-reporting VALUE into canonical JSON.
This is an owner-specific result mapping, not a general Elisp-to-JSON
normalizer: only the named process-reporting record fields below are mapped,
and opaque diagnostic values use the explicitly textual telemetry projector."
  (cond
   ((null value) nil)
   ((or (stringp value) (numberp value) (eq value t)
        (eq value e-json-false) (eq value e-json-null)) value)
   ((symbolp value) (symbol-name value))
   ((vectorp value)
    (vconcat (mapcar #'e-process-reporting--canonical-result-value
                     (append value nil))))
   ((and (listp value) (keywordp (car value)))
    (e-process-reporting--canonical-record value))
   (t
    (plist-get (e-telemetry-preview value) :content))))

(defun e-process-reporting--canonical-record (record)
  "Project one process-reporting RECORD into canonical JSON.
The record schema is owned here, so arrays become vectors and activity symbols
are mapped to strings without changing the persisted domain record."
  (let ((copy (copy-tree record t)))
    (dolist (key e-process-reporting--canonical-string-keys)
      (when (plist-member copy key)
        (setq copy
              (plist-put copy key
                         (let ((value (plist-get copy key)))
                           (if (null value)
                               e-json-null
                             (if (symbolp value)
                                 (symbol-name value)
                               value)))))))
    (dolist (key e-process-reporting--canonical-number-keys)
      (when (plist-member copy key)
        (setq copy (plist-put copy key
                              (if (numberp (plist-get copy key))
                                  (plist-get copy key)
                                e-json-null)))))
    (dolist (key e-process-reporting--canonical-boolean-keys)
      (when (plist-member copy key)
        (setq copy (plist-put copy key
                              (if (plist-get copy key)
                                  t
                                e-json-false)))))
    (dolist (key e-process-reporting--canonical-array-keys)
      (when (plist-member copy key)
        (let ((value (plist-get copy key)))
          (setq copy
                (plist-put copy key
                           (vconcat
                            (mapcar #'e-process-reporting--canonical-result-value
                                    (if (vectorp value)
                                        (append value nil)
                                      value))))))))
    (dolist (key e-process-reporting--canonical-object-keys)
      (when (plist-member copy key)
        (setq copy
              (plist-put copy key
                         (e-process-reporting--canonical-result-value
                          (plist-get copy key))))))
    (dolist (key e-process-reporting--canonical-text-keys)
      (when (plist-member copy key)
        (setq copy
              (plist-put copy key
                         (let ((value (plist-get copy key)))
                           (cond
                            ((null value) e-json-null)
                            ((stringp value) value)
                            (t (plist-get (e-telemetry-preview value)
                                          :content))))))))
    ;; Preserve already-canonical extension fields; reject no domain record
    ;; detail by guessing its container.  Unexpected opaque fields get the
    ;; explicit bounded textual telemetry projection.
    (let ((rest copy))
      (while rest
        (let* ((key (pop rest))
               (value (pop rest)))
          (unless (or (memq key e-process-reporting--canonical-string-keys)
                      (memq key e-process-reporting--canonical-number-keys)
                      (memq key e-process-reporting--canonical-boolean-keys)
                      (memq key e-process-reporting--canonical-array-keys)
                      (memq key e-process-reporting--canonical-object-keys)
                      (memq key e-process-reporting--canonical-text-keys))
            (unless (e-json-value-p value)
              (setq copy
                    (plist-put copy key
                               (e-process-reporting--canonical-result-value
                                value)))))))
    copy)))

(defun e-process-reporting--canonical-action-result (value)
  "Project one settled action VALUE into canonical JSON."
  (cond
   ((and (listp value) (keywordp (car value)))
    (e-process-reporting--canonical-record value))
   ;; Process-reporting list/read reducers own these list-of-record values as
   ;; collections.  Convert this known result shape to a canonical array at
   ;; the action boundary; no general Lisp-list compatibility is admitted.
   ((and (consp value) (listp (car value))
         (keywordp (car (car value))))
    (vconcat (mapcar #'e-process-reporting--canonical-record value)))
   ((vectorp value)
    (vconcat (mapcar #'e-process-reporting--canonical-result-value
                     (append value nil))))
   (t (e-process-reporting--canonical-result-value value))))

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
  "Return bounded current-turn activity for HARNESS SESSION-ID."
  (when (and (e-harness-p harness) (stringp session-id))
    (when-let* ((entry (gethash session-id
                                (e-harness-active-turns harness)))
                (turn-id (plist-get entry :id)))
      (e-harness-activity-current-turn-events
       harness session-id turn-id))))

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

(defun e-process-reporting-mark (arguments context)
  "Return work appending a session-owned marker from ARGUMENTS and CONTEXT."
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
                       signal note session-id turn-id trigger))
         ;; Evidence identity is already session-scoped and content-keyed.
         ;; Reusing it as the public marker id gives SQLite's existing
         ;; `record_id' index an exact dedupe predicate independent of total
         ;; process-report history size.
         (marker-id evidence-id))
    (cl-labels
        ((append-new-marker
          ()
          (let ((project-root
                 (e-harness-project-root harness session-id turn-id)))
            (e-process-reporting--append
             context
             (list :report-type "marker"
                   :id marker-id :marker-id marker-id :evidence-id evidence-id
                   :created-at (e-process-reporting--timestamp)
                   :signal signal :note note :session-id session-id
                   :turn-id turn-id :project-root project-root
                   :session-uri (format "session://e/sessions/%s/" session-id)
                   :messages-uri
                   (format "session://e/sessions/%s/messages" session-id)
                   :activity-uri
                   (format "session://e/sessions/%s/activity" session-id)
                   :process-reports-uri
                   (format "session://e/sessions/%s/process-reports" session-id)
                   :provider-request-id
                   (e-process-reporting--current-request-id
                    harness session-id turn-id)
                   :tool-call-id tool-call-id
                   :action-call-id (plist-get context :action-call-id)
                   :trigger trigger :trigger-chain (vconcat trigger-chain))))))
      (e-process-reporting--with-canonical-marker
       "process-reporting.mark.marker" context marker-id
       (lambda (marker)
         (if (null marker)
             (append-new-marker)
           (e-process-reporting--with-latest-triage
            "process-reporting.mark.triage" context marker-id
            (lambda (triage)
              (if (member (plist-get triage :status)
                          '("routed" "closed" "rejected"))
                  (list :marker-id marker-id :evidence-id evidence-id
                        :suppressed t)
                (append-new-marker))))))))))

(defun e-process-reporting-list (context &optional arguments)
  "Return work producing current-session marker summaries, newest-first."
  (let* ((store (e-process-reporting--session-store context))
         (session-id (e-process-reporting--session-id context))
         (status (plist-get arguments :status)))
    (cl-labels
        ((summaries
          (pairs)
          (let (result)
            (dolist (pair pairs (nreverse result))
              (let* ((marker (plist-get pair :marker))
                     (triage (plist-get pair :latest-triage))
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
                              :target-reference
                              (plist-get triage :target-reference))
                        result)))))))
      (if (e-session-async-enabled-p store)
          (e-process-reporting--composition-work
           "process-reporting.list" context
           (lambda ()
             (e-session-async-process-report-marker-page
              store session-id :status status
              :limit e-process-reporting-record-limit))
           (lambda (page) (summaries (plist-get page :markers))))
        (e-process-reporting--with-ephemeral-reports
         "process-reporting.list" context
         (lambda (reports)
           (summaries
            (mapcar
             (lambda (marker)
               (list :marker marker
                     :latest-triage
                     (e-process-reporting--latest-triage
                      reports (plist-get marker :marker-id))))
             (reverse
              (e-process-reporting--records-of-type reports "marker")))))
         :limit e-process-reporting-record-limit)))))

(defun e-process-reporting--read-result (marker triage extractions)
  "Return public read result from MARKER, TRIAGE, and EXTRACTIONS."
  (list :marker (e-process-reporting--public-record marker)
        :triage (mapcar #'e-process-reporting--public-record triage)
        :extractions
        (mapcar #'e-process-reporting--public-record extractions)))

(defun e-process-reporting-read (context marker-id)
  "Return work reading marker MARKER-ID and its bounded related records."
  (let* ((store (e-process-reporting--session-store context))
         (session-id (e-process-reporting--session-id context)))
    (e-process-reporting--with-canonical-marker
     "process-reporting.read.marker" context marker-id
     (lambda (marker)
       (unless marker
         (user-error "Unknown process marker: %s" marker-id))
       (if (e-session-async-enabled-p store)
           (e-process-reporting--composition-work
            "process-reporting.read.triage" context
            (lambda ()
              (e-session-async-process-report-triage-page
               store session-id marker-id
               :limit e-process-reporting-record-limit))
            (lambda (triage-page)
              (e-process-reporting--composition-work
               "process-reporting.read.extractions" context
               (lambda ()
                 (e-session-async-process-report-extraction-page
                  store session-id marker-id
                  :limit e-process-reporting-record-limit))
               (lambda (extraction-page)
                 (e-process-reporting--read-result
                  marker
                  (reverse (plist-get triage-page :triage))
                  (reverse
                   (plist-get extraction-page :extractions)))))))
         (e-process-reporting--with-ephemeral-reports
          "process-reporting.read.related" context
          (lambda (reports)
            (e-process-reporting--read-result
             marker
             (e-process-reporting--triage-records reports marker-id)
             (seq-filter
              (lambda (record)
                (and (equal (plist-get record :report-type) "extraction")
                     (member marker-id (plist-get record :marker-ids))))
              reports)))))))))

(defun e-process-reporting-triage (arguments context)
  "Return work appending a current-session triage decision from ARGUMENTS."
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
    (e-process-reporting--with-canonical-marker
     "process-reporting.triage.marker" context marker-id
     (lambda (marker)
       (unless marker
         (user-error "Unknown process marker: %s" marker-id))
       (e-process-reporting--append
        context
        (append
         (list :report-type "triage"
               :id (e-session-generate-ulid)
               :parent-id marker-id
               :marker-id marker-id
               :created-at (e-process-reporting--timestamp)
               :outcome outcome
               :status status
               :decision-note decision-note)
         (when target
           (list :target-reference
                 (e-telemetry-redact-string target)))))))))

(defun e-process-reporting--string-vector
    (arguments key &optional required max-items)
  "Return KEY from ARGUMENTS as a vector of strings bounded by MAX-ITEMS."
  (let* ((value (plist-get arguments key))
         (items (cond ((vectorp value) (append value nil))
                      ((listp value) value)
                      (t nil))))
    (when (and required (null items))
      (user-error "%s requires at least one value" key))
    (when (and max-items (> (length items) max-items))
      (user-error "%s accepts at most %d values" key max-items))
    (unless (cl-every #'stringp items)
      (user-error "%s must contain only strings" key))
    (vconcat (mapcar #'e-telemetry-redact-string items))))

(defun e-process-reporting--token-usage (value)
  "Return a narrow numeric extraction token usage schema from VALUE."
  (let (result)
    (dolist (key '(:input-tokens :cached-input-tokens
                   :cache-creation-input-tokens :output-tokens
                   :reasoning-output-tokens :total-tokens))
      (when-let ((number (plist-get value key)))
        (unless (numberp number)
          (user-error "%s must be numeric" key))
        (setq result (append result (list key number)))))
    result))

(defun e-process-reporting-record-extraction (arguments context)
  "Return work appending extraction attribution from ARGUMENTS and CONTEXT."
  (let* ((marker-ids
          (sort
           (delete-dups
            (append (e-process-reporting--string-vector
                     arguments :marker-ids t
                     e-process-reporting-marker-association-limit)
                    nil))
           #'string<))
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
    (e-process-reporting--verify-markers
     context marker-ids
     (lambda ()
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
              :turn-id (plist-get context :turn-id)))))))

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

(defun e-process-reporting--shape-value (value)
  "Return a hash and byte length for model-visible VALUE."
  (let ((text (prin1-to-string value)))
    (list :sha256 (secure-hash 'sha256 text)
          :bytes (string-bytes text))))

(defun e-process-reporting--without-marker-messages (messages)
  "Return MESSAGES with complete process_marker call/result pairs removed."
  (let ((call-ids
         (delq nil
               (mapcar
                (lambda (message)
                  (when (and (eq (plist-get message :role) 'tool-call)
                             (equal (plist-get
                                     (plist-get message :content) :name)
                                    "process_marker"))
                    (plist-get (plist-get message :content) :id)))
                messages))))
    (seq-remove
     (lambda (message)
       (or (and (eq (plist-get message :role) 'tool-call)
                (member (plist-get (plist-get message :content) :id)
                        call-ids))
           (and (eq (plist-get message :role) 'tool)
                (member (plist-get (plist-get message :content)
                                   :tool-call-id)
                        call-ids))))
     messages)))

(defun e-process-reporting--guidance-segment-p (segment)
  "Return non-nil when SEGMENT is process-reporting guidance."
  (equal (plist-get segment :id) '(process-reporting instructions)))

(defun e-process-reporting--remove-message-once (messages target)
  "Return MESSAGES with the first message equal to TARGET removed."
  (let (removed result)
    (dolist (message messages (nreverse result))
      (if (and (not removed) (equal message target))
          (setq removed t)
        (push message result)))))

(defun e-process-reporting--without-guidance (messages segments)
  "Return MESSAGES without process-reporting guidance in SEGMENTS."
  (let ((result messages))
    (dolist (segment segments result)
      (when (e-process-reporting--guidance-segment-p segment)
        (dolist (message (plist-get segment :messages))
          (setq result
                (e-process-reporting--remove-message-once
                 result message)))))))

(defun e-process-reporting--without-marker-tool (tools)
  "Return TOOLS without the process_marker descriptor."
  (seq-remove (lambda (tool)
                (equal (plist-get tool :name) "process_marker"))
              tools))

(defun e-process-reporting--request-snapshot (messages options)
  "Return one backend-neutral provider request value."
  (let ((copy (copy-tree options)))
    (plist-put copy :messages (copy-tree messages))
    copy))

(defun e-process-reporting-measure-request-shape (messages options segments)
  "Explicitly measure marker overhead in MESSAGES, OPTIONS, and SEGMENTS.
This intentionally expensive counterfactual is owned by process reporting and
is never called by the provider loop.  Call it only for a requested diagnostic
measurement, then persist or compare the returned content-free shape record as
needed."
  (let* ((actual
          (e-process-reporting--request-snapshot messages options))
         (no-active-messages
          (e-process-reporting--without-marker-messages messages))
         (no-passive-messages
          (e-process-reporting--without-guidance messages segments))
         (paired-messages
          (e-process-reporting--without-guidance no-active-messages segments))
         (no-passive-options (copy-tree options))
         (paired-options (copy-tree options)))
    (plist-put no-passive-options :tools
               (e-process-reporting--without-marker-tool
                (plist-get no-passive-options :tools)))
    (plist-put paired-options :tools
               (e-process-reporting--without-marker-tool
                (plist-get paired-options :tools)))
    (let ((without-passive
           (e-process-reporting--request-snapshot
            no-passive-messages no-passive-options))
          (without-active
           (e-process-reporting--request-snapshot no-active-messages options))
          (paired
           (e-process-reporting--request-snapshot
            paired-messages paired-options)))
      (list :revision "request-shape-v2"
            :serialization "backend-neutral-elisp-v1"
            :tokenizer-revision (plist-get options :tokenizer-revision)
            :actual-shape (e-process-reporting--shape-value actual)
            :without-passive-shape
            (e-process-reporting--shape-value without-passive)
            :without-active-shape
            (e-process-reporting--shape-value without-active)
            :paired-shape (e-process-reporting--shape-value paired)
            :model (plist-get options :model)
            :reasoning-effort (or (plist-get options :reasoning-effort)
                                  (plist-get options :effort))
            :prompt-cache-key-present
            (and (plist-get options :prompt-cache-key) t)
            :prompt-cache-key-sha256
            (when-let ((key (plist-get options :prompt-cache-key)))
              (secure-hash 'sha256 (format "%s" key)))
            :prompt-cache-retention
            (plist-get options :prompt-cache-retention)))))

(defun e-process-reporting--shape-measure-projection (measure)
  "Return the content-free fields from request-shape MEASURE."
  (when (listp measure)
    (let ((hash (plist-get measure :sha256))
          (bytes (plist-get measure :bytes)))
      (when (and (stringp hash) (numberp bytes))
        (list :sha256 (e-telemetry-redact-string hash)
              :bytes bytes)))))

(defun e-process-reporting--shape-projection (shape)
  "Return a narrow content-free projection of request SHAPE."
  (when (listp shape)
    (let (projected)
      (dolist (key '(:revision :serialization :tokenizer-revision :model
                     :reasoning-effort :prompt-cache-key-sha256
                     :prompt-cache-retention))
        (when-let ((value (plist-get shape key)))
          (when (stringp value)
            (setq projected
                  (append projected
                          (list key (e-telemetry-redact-string value)))))))
      (when (plist-member shape :prompt-cache-key-present)
        (setq projected
              (append projected
                      (list :prompt-cache-key-present
                            (and (plist-get shape
                                            :prompt-cache-key-present)
                                 t)))))
      (dolist (key '(:actual-shape :without-passive-shape
                     :without-active-shape :paired-shape))
        (when-let ((measure
                    (e-process-reporting--shape-measure-projection
                     (plist-get shape key))))
          (setq projected (append projected (list key measure)))))
      projected)))

(defun e-process-reporting-record-request-shape
    (context request-id request-ordinal shape)
  "Return work recording request SHAPE for REQUEST-ID in action CONTEXT.
REQUEST-ORDINAL is the ordinary provider lifecycle ordinal used for the join."
  (unless (and (stringp request-id) (not (string-empty-p request-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p request-id)))
  (unless (numberp request-ordinal)
    (signal 'wrong-type-argument (list 'numberp request-ordinal)))
  (let ((projected (e-process-reporting--shape-projection shape)))
    (unless (and (plist-get projected :actual-shape)
                 (plist-get projected :without-passive-shape)
                 (plist-get projected :without-active-shape)
                 (plist-get projected :paired-shape))
      (user-error "Incomplete process-reporting request shape"))
    (e-process-reporting--append
     context
     (list :report-type "request-shape"
           :id request-id
           :created-at (e-process-reporting--timestamp)
           :provider-request-id request-id
           :provider-request-ordinal request-ordinal
           :shape projected
           :session-id (plist-get context :session-id)
           :turn-id (plist-get context :turn-id)))))

(defun e-process-reporting-measure-and-record-request-shape
    (context request-id request-ordinal messages options segments)
  "Explicitly measure and record marker overhead for one provider request."
  (e-process-reporting-record-request-shape
   context request-id request-ordinal
   (e-process-reporting-measure-request-shape messages options segments)))

(defun e-process-reporting--shape-bytes (shape key)
  "Return byte count under KEY in request SHAPE."
  (plist-get (plist-get shape key) :bytes))

(defun e-process-reporting--recorded-request-shape (payload records)
  "Return the explicit or legacy request shape for PAYLOAD from RECORDS."
  (or (plist-get payload :request-shape)
      (when-let ((record
                  (car
                   (last
                    (seq-filter
                     (lambda (candidate)
                       (equal (plist-get candidate :provider-request-id)
                              (plist-get payload :provider-request-id)))
                     records)))))
        (plist-get record :shape))))

(defun e-process-reporting--request-cost-entry (event events shape-records)
  "Return one honest paired request attribution entry."
  (let* ((payload (plist-get event :payload))
         (shape
          (e-process-reporting--recorded-request-shape payload shape-records))
         (actual (e-process-reporting--shape-bytes shape :actual-shape))
         (paired (e-process-reporting--shape-bytes shape :paired-shape))
         (without-passive
          (e-process-reporting--shape-bytes shape :without-passive-shape))
         (without-active
          (e-process-reporting--shape-bytes shape :without-active-shape))
         (measured (and actual paired without-passive without-active))
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
          :measurement-status (if measured "recorded" "not-recorded")
          :actual-bytes actual
          :paired-bytes paired
          :direct-context-delta-bytes (and measured (- actual paired))
          :passive-surface-bytes (and measured (- actual without-passive))
          :active-marker-bytes (and measured (- actual without-active))
          :token-usage
          (e-process-reporting--request-usage
           events (plist-get payload :provider-request-id))
          :serialization (plist-get shape :serialization)
          :tokenizer-revision (plist-get shape :tokenizer-revision))))

(defun e-process-reporting--cost-result
    (session-id requests events shape-records marker-count)
  "Return detached cost result for SESSION-ID from bounded query inputs."
  (let* ((entries
          (mapcar (lambda (event)
                    (e-process-reporting--request-cost-entry
                     event events shape-records))
                  requests))
         (delta-bytes
          (let ((measured
                 (delq nil
                       (mapcar
                        (lambda (entry)
                          (plist-get entry :direct-context-delta-bytes))
                        entries))))
            (and measured (apply #'+ measured))))
         (measured-count
          (cl-count-if
           (lambda (entry)
             (equal (plist-get entry :measurement-status) "recorded"))
           entries)))
    (list :scope "request-shape-counterfactual"
          :session-id session-id
          :provider-request-count (length requests)
          :measured-request-count measured-count
          :measurement-status
          (cond
           ((= measured-count 0) "not-recorded")
           ((= measured-count (length requests)) "recorded")
           (t "partial"))
          :marker-count marker-count
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
          "Counterfactual byte deltas exist only for requests measured explicitly by process reporting; ordinary provider requests do not perform or retain this expensive measurement. Values do not claim provider-token or behavioral overhead.")))

(defun e-process-reporting-cost-report (context)
  "Return work producing paired request accounting for CONTEXT markers."
  (let* ((harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (events (e-process-reporting--activity-events harness session-id))
         (requests
          (seq-filter
           (lambda (event)
             (eq (plist-get event :event-type) 'provider-request-started))
           events))
         (request-ids
          (delq nil
                (mapcar
                 (lambda (event)
                   (plist-get (plist-get event :payload) :provider-request-id))
                 requests))))
    (let ((store (e-process-reporting--session-store context)))
      (if (e-session-async-enabled-p store)
          (e-process-reporting--composition-work
           "process-reporting.cost-report.shapes" context
           (lambda ()
             (if request-ids
                 (e-session-async-process-report-request-shapes
                  store session-id request-ids)
               (list :request-shapes nil)))
           (lambda (shape-page)
             (e-process-reporting--composition-work
              "process-reporting.cost-report.marker-count" context
              (lambda ()
                (e-session-async-process-report-marker-count store session-id))
              (lambda (count)
                (e-process-reporting--cost-result
                 session-id requests events
                 (plist-get shape-page :request-shapes)
                 (plist-get count :marker-count))))))
        (e-process-reporting--with-ephemeral-reports
         "process-reporting.cost-report" context
         (lambda (reports)
           (e-process-reporting--cost-result
            session-id requests events
            (e-process-reporting--records-of-type reports "request-shape")
            (length (e-process-reporting--records-of-type reports "marker"))))
         :record-ids (and request-ids (vconcat request-ids)))))))

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
  "Return cooperative process reporting action ID with PARAMETERS and RUNNER."
  (e-action-create
   :parameters parameters
   :requires-session t
   :work
   (e-work-spec-create
    :id id :parameters parameters
    :description "Run one bounded process-reporting action."
    :execution 'cooperative :interactive-policy 'async
    :owner 'process-reporting
    :runner
    (lambda (parent arguments context)
      (condition-case err
          (e-process-reporting--forward-result
           parent (funcall runner arguments context)
           #'e-process-reporting--canonical-action-result)
        ((error quit) (e-work-fail parent err)))
      :deferred))))

(defun e-process-reporting--marker-tool-work ()
  "Return model-facing marker work bridged to the action's true settlement."
  (e-work-spec-create
   :id "tool.process-marker"
   :description "Persist one process marker and return its committed record."
   :execution 'cooperative :interactive-policy 'async
   :owner 'process-reporting
   :runner
   (lambda (parent arguments _context)
     (condition-case err
         (let ((action-work
                (plist-get
                 (e-actions-dispatch 'process-reporting :mark arguments)
                 :request)))
           ;; `e-actions-call' deliberately returns a generic shell-facing
           ;; work reference.  A model tool is itself Work, so it owns and
           ;; bridges the exact action handle instead of reporting that
           ;; reference as successful tool content.
           (e-process-reporting--forward-result parent action-work))
       ((error quit) (e-work-fail parent err)))
     :deferred)))

(defun e-process-reporting-register-tool (registry &rest _context)
  "Register the tiny process marker tool in REGISTRY."
  (e-tools-register
   registry
   :name "process_marker"
   :description "Save one coarse, reusable process observation."
   :parameters e-process-reporting--marker-parameters
   :blocking-class 'cheap
   :work (e-process-reporting--marker-tool-work)))

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
       (:marker-ids (:type "array" :maxItems 64 :items (:type "string"))
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
