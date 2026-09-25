;;; e-board-orchestration.el --- Durable board run reducer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Durable orchestration is represented only by bounded board facts.  This file
;; validates that wire contract and reduces facts into a replayable projection;
;; it does not dispatch work, schedule timers, or cancel anything.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(define-error 'e-board-orchestration-error "Board orchestration error")
(define-error 'e-board-orchestration-invalid-fact
  "Invalid board orchestration fact" 'e-board-orchestration-error)

(defconst e-board-orchestration-fact-version 1
  "Current version of the durable orchestration board fact contract.")

(defconst e-board-orchestration-wire-version 1
  "Current JSON-safe encoding for orchestration payload attributes.")

(defconst e-board-orchestration-fact-byte-limit (* 8 1024)
  "Maximum encoded width retained by one orchestration fact payload.")

(defconst e-board-orchestration-summary-limit 240
  "Maximum width of a terminal report summary.")

(defconst e-board-orchestration-error-limit 480
  "Maximum width of a terminal report error.")

(defconst e-board-orchestration-output-limit 32
  "Maximum number of declared outputs in one terminal report.")

(defconst e-board-orchestration--fact-types
  '(manifest attempt-selection task-attempt terminal-report conflict
    continuation-claim continuation-outcome)
  "Fact types understood by the orchestration reducer.")

(defun e-board-orchestration--invalid (field value)
  "Signal a contract error for FIELD with VALUE."
  (signal 'e-board-orchestration-invalid-fact (list field value)))

(defun e-board-orchestration--string (value field &optional allow-empty)
  "Return VALUE when it is a bounded string for FIELD, otherwise signal."
  (unless (and (stringp value)
               (or allow-empty (not (string-empty-p value)))
               (<= (string-bytes value) e-board-orchestration-fact-byte-limit))
    (e-board-orchestration--invalid field value))
  value)

(defun e-board-orchestration--attempt (value field)
  "Return VALUE when it is a nonnegative attempt number for FIELD."
  (unless (and (integerp value) (>= value 0))
    (e-board-orchestration--invalid field value))
  value)

(defun e-board-orchestration--list (value field)
  "Return VALUE as a list for FIELD, accepting vectors at the boundary."
  (unless (or (listp value) (vectorp value))
    (e-board-orchestration--invalid field value))
  (append value nil))

(defun e-board-orchestration--task (task)
  "Validate and normalize one manifest TASK."
  (unless (listp task) (e-board-orchestration--invalid :task task))
  (let* ((task-key (e-board-orchestration--string
                    (plist-get task :task-key) :task-key))
         (required (plist-get task :required))
         (accepted (e-board-orchestration--attempt
                    (or (plist-get task :accepted-attempt) 0)
                    :accepted-attempt)))
    (unless (memq required '(t nil))
      (e-board-orchestration--invalid :required required))
    (list :task-key task-key :required required :accepted-attempt accepted)))

(defun e-board-orchestration--deadline (deadline)
  "Validate and normalize a manifest DEADLINE policy."
  (unless (listp deadline) (e-board-orchestration--invalid :deadline deadline))
  (let ((kind (plist-get deadline :kind)))
    (pcase kind
      ('none (list :kind 'none))
      ('at (let ((at (plist-get deadline :at)))
             (unless (numberp at) (e-board-orchestration--invalid :deadline-at at))
             (list :kind 'at :at at)))
      (_ (e-board-orchestration--invalid :deadline-kind kind)))))

(defun e-board-orchestration--outputs (outputs)
  "Validate declared report OUTPUTS without interpreting their artifact schema."
  (let ((items (e-board-orchestration--list (or outputs []) :outputs)))
    (when (> (length items) e-board-orchestration-output-limit)
      (e-board-orchestration--invalid :outputs outputs))
    (dolist (output items)
      (unless (listp output) (e-board-orchestration--invalid :output output)))
    (copy-tree items)))

(defun e-board-orchestration--continuation (value run-id)
  "Validate a manifest continuation VALUE for RUN-ID, or return nil.
The publication key is persisted in the manifest so replay never derives a
new input identity for the same reconciliation request."
  (when value
    (unless (listp value) (e-board-orchestration--invalid :continuation value))
    (list :session-id (e-board-orchestration--string
                       (plist-get value :session-id) :continuation-session-id)
          :prompt (e-board-orchestration--string
                   (plist-get value :prompt) :continuation-prompt)
          :publication-key
          (e-board-orchestration--string
           (or (plist-get value :publication-key)
               (format "continuation:%s" run-id))
           :continuation-publication-key))))

(defun e-board-orchestration--descriptor (value)
  "Validate and copy an optional bounded application-owned run descriptor VALUE.
The orchestration core persists this plist for producer recovery but does not
interpret its fields."
  (when value
    (unless (and (proper-list-p value)
                 (zerop (% (length value) 2)))
      (e-board-orchestration--invalid :descriptor value))
    (let ((tail value)
          (seen (make-hash-table :test 'eq)))
      (while tail
        (let ((key (pop tail)))
          (unless (keywordp key)
            (e-board-orchestration--invalid :descriptor-key value))
          (when (gethash key seen)
            (e-board-orchestration--invalid :duplicate-descriptor-key key))
          (puthash key t seen))
        (unless tail
          (e-board-orchestration--invalid :descriptor-key value))
        (pop tail)))
    (let* ((wire-value (e-board-orchestration--wire-encode value))
           (encoded-width (string-bytes (prin1-to-string wire-value))))
      (when (> encoded-width e-board-orchestration-fact-byte-limit)
        (e-board-orchestration--invalid :descriptor value))
      (e-board-orchestration--wire-decode wire-value))))

(defun e-board-orchestration--wire-encode (value)
  "Encode orchestration VALUE without JSON list/object ambiguity."
  (cond
   ((or (null value) (eq value t) (stringp value) (numberp value)) value)
   ((symbolp value) (vector "symbol" (symbol-name value)))
   ((vectorp value)
    (vector "vector"
            (vconcat (mapcar #'e-board-orchestration--wire-encode value))))
   ((consp value)
    (unless (proper-list-p value)
      (e-board-orchestration--invalid :wire-value value))
    (vector "list"
            (vconcat (mapcar #'e-board-orchestration--wire-encode value))))
   (t (e-board-orchestration--invalid :wire-value value))))

(defun e-board-orchestration--wire-sequence (value field)
  "Return tagged wire VALUE as a two-item list, or signal for FIELD."
  (let ((items (cond ((vectorp value) (append value nil))
                     ((listp value) value)
                     (t nil))))
    (unless (= (length items) 2)
      (e-board-orchestration--invalid field value))
    items))

(defun e-board-orchestration--wire-decode (value)
  "Decode one exact orchestration wire VALUE after JSON replay."
  (if (or (null value) (eq value t) (stringp value) (numberp value))
      value
    (pcase-let* ((`(,tag ,contents)
                  (e-board-orchestration--wire-sequence value :wire-value)))
      (pcase tag
        ("symbol"
         (unless (stringp contents)
           (e-board-orchestration--invalid :wire-symbol contents))
         (intern contents))
        ((or "list" "vector")
         (unless (or (listp contents) (vectorp contents))
           (e-board-orchestration--invalid :wire-items contents))
         (let ((decoded
                (mapcar #'e-board-orchestration--wire-decode contents)))
           (if (equal tag "vector") (vconcat decoded) decoded)))
        (_ (e-board-orchestration--invalid :wire-tag tag))))))

(defun e-board-orchestration--wire-enum (value allowed field)
  "Normalize JSON wire VALUE against ALLOWED symbols for FIELD."
  (let ((normalized
         (if (stringp value)
             (cl-find value allowed :key #'symbol-name :test #'string=)
           value)))
    (unless (memq normalized allowed)
      (e-board-orchestration--invalid field value))
    normalized))

(defun e-board-orchestration-continuation-claim-key (publication-key status)
  "Return the immutable durable claim key for PUBLICATION-KEY at STATUS."
  (format "continuation-claim:%s:%s" publication-key status))

(defun e-board-orchestration-continuation-outcome-key (run-id publication-key)
  "Return the immutable execution outcome key for RUN-ID and PUBLICATION-KEY.

Unlike an admission claim, a continuation execution has one terminal fact
identity.  A repeated harness terminal callback therefore reuses this source
key and is idempotent at the Board storage boundary; a conflicting payload is
surfaced as the ordinary Board source-key conflict rather than becoming a
second outcome."
  (format "continuation-outcome:%s:%s" run-id publication-key))

(defun e-board-orchestration-validate-fact (fact)
  "Validate and normalize versioned orchestration FACT.
FACT is a plist with `:type', `:payload', and an idempotency key.  The result is
safe to store in a board envelope and contains no runtime state."
  (unless (listp fact) (e-board-orchestration--invalid :fact fact))
  (let* ((version (or (plist-get fact :version) e-board-orchestration-fact-version))
         (type (plist-get fact :type))
         (payload (plist-get fact :payload))
         (key (e-board-orchestration--string
               (plist-get fact :idempotency-key) :idempotency-key)))
    (unless (= version e-board-orchestration-fact-version)
      (e-board-orchestration--invalid :version version))
    (unless (memq type e-board-orchestration--fact-types)
      (e-board-orchestration--invalid :type type))
    (unless (listp payload) (e-board-orchestration--invalid :payload payload))
    (let ((run-id (e-board-orchestration--string (plist-get payload :run-id) :run-id)))
      (pcase type
        ('manifest
         (let ((seen (make-hash-table :test 'equal))
               (tasks (mapcar #'e-board-orchestration--task
                              (e-board-orchestration--list (plist-get payload :tasks) :tasks)))
               (descriptor (e-board-orchestration--descriptor
                            (plist-get payload :descriptor))))
           (dolist (task tasks)
             (let ((task-key (plist-get task :task-key)))
               (when (gethash task-key seen)
                 (e-board-orchestration--invalid :duplicate-task-key task-key))
               (puthash task-key t seen)))
           (list :version version :type type :idempotency-key key
                 :payload
                 (append
                  (list :run-id run-id :tasks tasks
                        :deadline (e-board-orchestration--deadline
                                   (or (plist-get payload :deadline) '(:kind none)))
                        :continuation
                        (e-board-orchestration--continuation
                         (plist-get payload :continuation) run-id))
                  (when descriptor (list :descriptor descriptor))))))
        ('continuation-claim
         (let ((publication-key
                (e-board-orchestration--string
                 (plist-get payload :publication-key) :publication-key))
               (status (plist-get payload :status)))
           (unless (memq status '(pending published failed))
             (e-board-orchestration--invalid :continuation-status status))
           (list :version version :type type :idempotency-key key
                 :payload
                 (list :run-id run-id :publication-key publication-key :status status
                       :error (when-let* ((error (plist-get payload :error)))
                                (truncate-string-to-width
                                 (format "%s" error)
                                 e-board-orchestration-error-limit nil nil "..."))))))
        ('continuation-outcome
         (let ((publication-key
                (e-board-orchestration--string
                 (plist-get payload :publication-key) :publication-key))
               (status (plist-get payload :status))
               (turn-id (plist-get payload :turn-id)))
           (unless (memq status '(done failed cancelled))
             (e-board-orchestration--invalid :continuation-outcome-status status))
           (when turn-id
             (e-board-orchestration--string turn-id :continuation-turn-id))
           (list :version version :type type :idempotency-key key
                 :payload
                 (append
                  (list :run-id run-id :publication-key publication-key
                        :status status)
                  (when turn-id (list :turn-id turn-id))
                  (when-let* ((error (plist-get payload :error)))
                    (list :error
                          (truncate-string-to-width
                           (format "%s" error)
                           e-board-orchestration-error-limit nil nil "...")))))))
        ((or 'attempt-selection 'task-attempt 'terminal-report 'conflict)
         (let* ((task-key (e-board-orchestration--string
                           (plist-get payload :task-key) :task-key))
                (attempt (e-board-orchestration--attempt
                          (plist-get payload :attempt) :attempt))
                (status (plist-get payload :status)))
           (when (and (memq type '(task-attempt terminal-report))
                      (not (memq status '(queued running done failed cancelled))))
             (e-board-orchestration--invalid :status status))
           (when (and (eq type 'terminal-report)
                      (not (memq status '(done failed cancelled))))
             (e-board-orchestration--invalid :terminal-status status))
           (let ((body (append (list :run-id run-id :task-key task-key :attempt attempt)
                               (when status (list :status status)))) )
             (when (eq type 'terminal-report)
               (setq body
                     (append body
                             (list
                              :summary
                              (truncate-string-to-width
                               (or (plist-get payload :summary) "")
                               e-board-orchestration-summary-limit nil nil "...")
                              :outputs
                              (e-board-orchestration--outputs
                               (plist-get payload :outputs))
                              :error
                              (when-let* ((error (plist-get payload :error)))
                                (truncate-string-to-width
                                 (format "%s" error)
                                 e-board-orchestration-error-limit nil nil "..."))
                              :participant-session-id
                              (when-let* ((session-id
                                           (plist-get payload
                                                      :participant-session-id)))
                                (e-board-orchestration--string
                                 session-id :participant-session-id)))
                             (when (plist-member payload :result)
                               (list :result
                                     (e-board-orchestration--descriptor
                                      (plist-get payload :result)))))))
             (when (eq type 'conflict)
               (setq body
                     (append body
                             (list :reason (truncate-string-to-width
                                            (format "%s" (or (plist-get payload :reason) "conflict"))
                                            e-board-orchestration-error-limit nil nil "...")))))
             (list :version version :type type :idempotency-key key :payload body))))))))

(cl-defun e-board-orchestration-fact-record-fields (fact &key author)
  "Return canonical Board record fields for validated orchestration FACT."
  (let* ((normalized (e-board-orchestration-validate-fact fact))
         (payload (plist-get normalized :payload))
         (source-key (list (format "orchestration:%s:%s"
                                          (plist-get payload :run-id)
                                          (plist-get normalized :type))
                                    (plist-get normalized :idempotency-key) 0)))
    (list :source-key source-key :author author :tags '(orchestration)
          :attributes
          (list :orchestration-version e-board-orchestration-fact-version
                :orchestration-wire-version e-board-orchestration-wire-version
                :orchestration-type (symbol-name (plist-get normalized :type))
                :orchestration-run-id (plist-get payload :run-id)
                :orchestration-payload
                (e-board-orchestration--wire-encode payload)
                :orchestration-idempotency-key
                (plist-get normalized :idempotency-key))
          :content (format "Orchestration %s for run %s"
                           (plist-get normalized :type)
                           (plist-get payload :run-id)))))

(defun e-board-orchestration-fact-from-record (record)
  "Return normalized orchestration fact from detached Board RECORD, or nil."
  (when (and (listp record)
             (eq (plist-get record :record-kind) 'fact)
             (memq 'orchestration (plist-get record :tags)))
    (let* ((attributes (plist-get record :attributes))
           (wire-version (plist-get attributes :orchestration-wire-version))
           (type (e-board-orchestration--wire-enum
                  (plist-get attributes :orchestration-type)
                  e-board-orchestration--fact-types :type))
           (payload
            (progn
              (unless (and (integerp wire-version)
                           (= wire-version
                              e-board-orchestration-wire-version))
                (e-board-orchestration--invalid :wire-version wire-version))
              (e-board-orchestration--wire-decode
               (plist-get attributes :orchestration-payload)))))
      (e-board-orchestration-validate-fact
       (list :version (plist-get attributes :orchestration-version)
             :type type :payload payload
             :idempotency-key
             (plist-get attributes :orchestration-idempotency-key))))))

(defun e-board-orchestration--task-projection (task attempts reports)
  "Reduce TASK with ATTEMPTS and REPORTS into one task projection."
  (let* ((task-key (plist-get task :task-key))
         (accepted-attempt (plist-get task :accepted-attempt))
         (report (cl-find-if (lambda (item)
                               (and (= (plist-get item :attempt) accepted-attempt)
                                    (equal (plist-get item :task-key) task-key)))
                             reports))
         (attempt (cl-find-if (lambda (item)
                                (and (= (plist-get item :attempt) accepted-attempt)
                                     (equal (plist-get item :task-key) task-key)))
                              attempts)))
    (append (copy-tree task)
            (list :state (or (plist-get report :status)
                             (plist-get attempt :status)
                             'pending)
                  :accepted-report (copy-tree report)))))

(defun e-board-orchestration--select-task-attempts (tasks selections)
  "Apply contiguous durable SELECTIONS to immutable manifest TASKS.
A missing successor leaves the task at its last accepted attempt, so malformed
or out-of-order facts cannot skip retry identities during replay."
  (mapcar
   (lambda (task)
     (let* ((copy (copy-tree task))
            (task-key (plist-get copy :task-key))
            (accepted (plist-get copy :accepted-attempt))
            (selected
             (mapcar (lambda (selection) (plist-get selection :attempt))
                     (cl-remove-if-not
                      (lambda (selection)
                        (equal task-key (plist-get selection :task-key)))
                      selections))))
       (while (member (1+ accepted) selected)
         (setq accepted (1+ accepted)))
       (plist-put copy :accepted-attempt accepted)
       copy))
   tasks))

(defun e-board-orchestration--continuation-outcome (outcomes)
  "Reduce matching terminal continuation OUTCOMES to one bounded value.

One ordinary terminal callback produces one status.  If a malformed or
manually assembled fact stream contains incompatible statuses, retain that
evidence and choose a non-success status so consumers cannot mistake the run
for a durably completed coordinator turn."
  (when outcomes
    (let* ((statuses (delete-dups
                      (mapcar (lambda (outcome)
                                (plist-get outcome :status))
                              outcomes)))
           (first (copy-tree (car outcomes) t))
           (status (cond
                    ((memq 'failed statuses) 'failed)
                    ((memq 'cancelled statuses) 'cancelled)
                    (t 'done))))
      (append first
              (list :status status
                    :conflict (> (length statuses) 1)
                    :statuses statuses)))))

(defun e-board-orchestration-reduce (facts &optional now)
  "Reduce valid durable FACTS into one idempotent run projection.
FACTS may be normalized facts or detached record plists.  This pure reducer
never performs a clock-driven cancellation; a passed deadline is only
evidence."
  (let ((manifests nil)
        (selections nil)
        (attempts nil)
        (reports nil)
        (conflicts nil)
        (claims nil)
        (outcomes nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (item facts)
      (let ((fact (cond
                   ((and (listp item)
                         (or (plist-get item :record-kind)
                             (plist-get item :kind)))
                    (e-board-orchestration-fact-from-record item))
                   (t (e-board-orchestration-validate-fact item)))))
        (when fact
          (let ((key (list (plist-get fact :type)
                           (plist-get fact :idempotency-key))))
            (unless (gethash key seen)
              (puthash key t seen)
              (pcase (plist-get fact :type)
                ('manifest (push fact manifests))
                ('attempt-selection
                 (push (plist-get fact :payload) selections))
                ('task-attempt
                 (push (plist-get fact :payload) attempts))
                ('terminal-report
                 (push (plist-get fact :payload) reports))
                ('conflict
                 (push (plist-get fact :payload) conflicts))
                ('continuation-claim
                 (push (plist-get fact :payload) claims))
                ('continuation-outcome
                 (push (plist-get fact :payload) outcomes))))))))
    (let* ((manifest-fact (car (nreverse manifests)))
           (manifest (and manifest-fact
                          (plist-get manifest-fact :payload)))
           (run-id (and manifest (plist-get manifest :run-id))))
      (unless manifest
        (e-board-orchestration--invalid :manifest 'missing))
      (setq attempts
            (cl-remove-if-not
             (lambda (item) (equal (plist-get item :run-id) run-id))
             attempts)
            selections
            (cl-remove-if-not
             (lambda (item) (equal (plist-get item :run-id) run-id))
             selections)
            reports
            (cl-remove-if-not
             (lambda (item) (equal (plist-get item :run-id) run-id))
             reports)
            conflicts
            (cl-remove-if-not
             (lambda (item) (equal (plist-get item :run-id) run-id))
             conflicts)
            claims
            (cl-remove-if-not
             (lambda (item) (equal (plist-get item :run-id) run-id))
             claims)
            outcomes
            (cl-remove-if-not
             (lambda (item) (equal (plist-get item :run-id) run-id))
             outcomes))
      ;; Different reports for the same accepted task attempt are a visible
      ;; conflict.
      (let ((selected-tasks
             (e-board-orchestration--select-task-attempts
              (plist-get manifest :tasks) selections)))
        (dolist (task selected-tasks)
          (let* ((key (plist-get task :task-key))
                 (attempt (plist-get task :accepted-attempt))
                 (matches
                  (cl-remove-if-not
                   (lambda (report)
                     (and (equal key (plist-get report :task-key))
                          (= attempt (plist-get report :attempt))))
                   reports)))
            (when (> (length
                      (delete-dups
                       (mapcar (lambda (report)
                                 (prin1-to-string report))
                               matches)))
                     1)
              (push (list :run-id run-id :task-key key :attempt attempt
                          :reason "conflicting terminal reports")
                    conflicts))))
        (let* ((tasks
                (mapcar (lambda (task)
                          (e-board-orchestration--task-projection
                           task attempts reports))
                        selected-tasks))
               (required
                (cl-remove-if-not
                 (lambda (task) (plist-get task :required))
                 tasks))
               (all-terminal
                (cl-every
                 (lambda (task)
                   (memq (plist-get task :state)
                         '(done failed cancelled)))
                 required))
               (successful
                (and all-terminal
                     (null conflicts)
                     (cl-every
                      (lambda (task) (eq (plist-get task :state) 'done))
                      required)))
               (deadline (plist-get manifest :deadline))
               (deadline-state
                (and (eq (plist-get deadline :kind) 'at)
                     (<= (plist-get deadline :at)
                         (or now (float-time)))))
               (terminal-status
                (when all-terminal
                  (if successful 'done 'failed)))
               (manifest-continuation
                (plist-get manifest :continuation))
               continuation)
          (when manifest-continuation
            (let* ((publication-key
                    (plist-get manifest-continuation :publication-key))
                   (matching-outcomes
                    (cl-remove-if-not
                     (lambda (outcome)
                       (equal (plist-get outcome :publication-key)
                              publication-key))
                     outcomes))
                   (execution-outcome
                    (e-board-orchestration--continuation-outcome
                     matching-outcomes))
                   ;; The claim and outcome streams intentionally remain
                   ;; separate: `published' means admission settled, not
                   ;; that the admitted coordinator turn completed.
                   (matching-claims
                    (cl-remove-if-not
                     (lambda (claim)
                       (and (equal (plist-get claim :publication-key)
                                   publication-key)
                            (memq (plist-get claim :status)
                                  '(pending published failed))))
                     claims))
                   (published
                    (cl-find 'published matching-claims
                             :key (lambda (claim)
                                    (plist-get claim :status))))
                   (pending
                    (cl-find 'pending matching-claims
                             :key (lambda (claim)
                                    (plist-get claim :status))))
                   (failed
                    (car
                     (last
                      (cl-remove-if-not
                       (lambda (claim)
                         (eq (plist-get claim :status) 'failed))
                       matching-claims)))))
              (setq continuation
                    (append
                     (copy-tree manifest-continuation)
                     (list :state
                           (cond (published 'published)
                                 (failed 'failed)
                                 (pending 'pending)
                                 (t 'waiting))
                           :claims (copy-tree matching-claims t)
                           :execution-outcome
                           (copy-tree execution-outcome t))))))
          (list :run-id run-id
                :manifest (copy-tree manifest)
                :tasks tasks
                :reports (copy-tree reports)
                :conflicts (nreverse conflicts)
                :deadline (append (copy-tree deadline)
                                  (list :expired deadline-state))
                :continuation continuation
                :continuation-outcomes (copy-tree outcomes t)
                :continuation-outcome
                (copy-tree (plist-get continuation :execution-outcome) t)
                :attempt-selections (copy-tree selections)
                :continuation-claims (copy-tree claims)
                :terminal-status terminal-status))))))

;;;; Consumer-shaped run-set projection

(defconst e-board-orchestration-run-set-default-record-limit 32
  "Maximum number of run entries retained by one Board run-set value.")

(defconst e-board-orchestration-run-set-max-record-limit 256
  "Largest record limit accepted by a Board run-set projection query.")

(defconst e-board-orchestration-run-set-default-byte-limit (* 32 1024)
  "Maximum encoded width of one detached Board run-set value.")

(defconst e-board-orchestration-run-set-max-byte-limit (* 256 1024)
  "Largest encoded width accepted by an explicitly expanded Board run-set query.")

(defconst e-board-orchestration-run-set-label-limit 160
  "Maximum width of a run label in the consumer-shaped run-set value.")

(defun e-board-orchestration--run-set-value (projection &rest keys)
  "Return the first non-nil value of KEYS in PROJECTION."
  (cl-loop for key in keys
           for value = (plist-get projection key)
           when value return value))

(defun e-board-orchestration--run-set-task-terminal-p (task)
  "Return non-nil when TASK has reached a terminal lifecycle state."
  (memq (plist-get task :state) '(done failed cancelled terminal)))

(defun e-board-orchestration--run-set-state-counts (tasks)
  "Return bounded state totals for TASKS."
  (let ((counts (list :total (length tasks)
                      :pending 0 :running 0 :done 0 :failed 0 :cancelled 0
                      :other 0)))
    (dolist (task tasks)
      (let* ((state (plist-get task :state))
             (key (and (symbolp state)
                       (intern (concat ":" (symbol-name state))))))
        (if (and key (plist-member counts key))
            (plist-put counts key (1+ (plist-get counts key)))
          (plist-put counts :other (1+ (plist-get counts :other))))))
    counts))

(defun e-board-orchestration--run-set-entry (projection board-id restore-state)
  "Map reduced durable run PROJECTION to one bounded consumer entry."
  (let* ((run-id (plist-get projection :run-id))
         (manifest (or (plist-get projection :manifest) nil))
         (descriptor (or (plist-get manifest :descriptor)
                         (plist-get projection :descriptor)))
         (tasks (or (plist-get projection :tasks)
                    (plist-get manifest :tasks) nil))
         (required (cl-remove-if-not (lambda (task) (plist-get task :required))
                                     tasks))
         (optional (cl-remove-if
                    (lambda (task) (plist-get task :required)) tasks))
         (required-counts
          (e-board-orchestration--run-set-state-counts required))
         (optional-counts
          (e-board-orchestration--run-set-state-counts optional))
         (required-terminal
          (cl-every #'e-board-orchestration--run-set-task-terminal-p required))
         (optional-active
          (cl-some (lambda (task)
                     (not (e-board-orchestration--run-set-task-terminal-p task)))
                   optional))
         (terminal-status (plist-get projection :terminal-status))
         (continuation (plist-get projection :continuation))
         (continuation-state (plist-get continuation :state))
         (continuation-outcome (plist-get projection :continuation-outcome))
         (continuation-outcome-status
          (plist-get continuation-outcome :status))
         (continuation-outcome-conflict-p
          (plist-get continuation-outcome :conflict))
         (terminal-unconsumed
          (and terminal-status continuation
               ;; Admission/publication is not execution completion.  Keep a
               ;; terminal run active until the generic harness outcome says
               ;; the coordinator turn finished successfully.  In particular,
               ;; `input-consumed' is still only an admission/start edge.
               (not (and (eq continuation-outcome-status 'done)
                         (not continuation-outcome-conflict-p)))))
         (active-p (or (not required-terminal)
                       terminal-unconsumed optional-active))
         (deadline (plist-get projection :deadline))
         (deadline-expired (plist-get deadline :expired))
         (conflicts (or (plist-get projection :conflicts) nil))
         (failure (or (and (eq terminal-status 'failed) terminal-status)
                      (plist-get projection :failure)))
         (attention-p (or conflicts deadline-expired failure
                          (eq continuation-state 'failed)
                          (memq continuation-outcome-status '(failed cancelled))
                          continuation-outcome-conflict-p))
         (lifecycle
          (cond
           ((not (eq restore-state 'ready)) 'restoring)
           (attention-p 'attention)
           (terminal-status 'finishing)
           ((cl-some (lambda (task)
                       (memq (plist-get task :state) '(pending queued)))
                     tasks)
            'dispatching)
           (t 'running)))
         (latest-at (e-board-orchestration--run-set-value
                     projection :latest-event-at :latest-event-time
                     :updated-at))
         (latest-position (or (plist-get projection :latest-event-position) 0))
         (reports (or (plist-get projection :reports)
                      (mapcar (lambda (task) (plist-get task :accepted-report))
                              tasks)))
         (participants
          (delete-dups
           (delq nil (mapcar (lambda (report)
                               (or (plist-get report :participant-session-id)
                                   (plist-get report :participant-id)))
                             reports))))
         (label-value
          (or (plist-get descriptor :label)
              (plist-get descriptor :name)
              (plist-get manifest :label)
              run-id))
         (label (if (stringp label-value)
                    (substring label-value 0
                               (min (length label-value)
                                    e-board-orchestration-run-set-label-limit))
                  (format "%s" label-value)))
         (action-rank (pcase lifecycle
                        ('attention 0)
                        ('restoring 1)
                        ('dispatching 2)
                        ('running 3)
                        ('finishing 4)
                        (_ 5))))
    (list :board-id board-id :run-id run-id :label label
          :lifecycle lifecycle :active-p active-p
          :actionable-rank action-rank
          :required-count (length required) :optional-count (length optional)
          :required-total (length required) :optional-total (length optional)
          :required-state-counts required-counts
          :optional-state-counts optional-counts
          :required-totals required-counts
          :optional-totals optional-counts
          :required-complete
          (length (cl-remove-if-not
                   #'e-board-orchestration--run-set-task-terminal-p required))
          :optional-active optional-active
          :participant-count (length participants)
          :participant-total (length participants)
          :admission-count (length participants)
          :admission-total (length participants)
          :latest-event-at latest-at :latest-event-time latest-at
          :latest-event-position latest-position
          :conflicts (copy-tree conflicts t)
          :deadline (copy-tree deadline t)
          :failure failure
          :restore-state restore-state
          :attention-p attention-p
          :completion-state (or terminal-status 'active)
          :completion-delivery-state continuation-state
          :completion-execution-state continuation-outcome-status
          :continuation-outcome (copy-tree continuation-outcome t)
          :continuation-state continuation-state)))

(defun e-board-orchestration--run-set-encoded-bytes (value)
  "Return the detached encoded width of run-set VALUE."
  (string-bytes (prin1-to-string value)))

(defun e-board-orchestration--run-set-with-byte-accounting (value)
  "Return VALUE with `:bytes' equal to its final encoded width."
  (let ((guess 0) candidate actual)
    (dotimes (_ 4)
      (setq candidate (plist-put (copy-tree value t) :bytes guess)
            actual (e-board-orchestration--run-set-encoded-bytes candidate)
            guess actual))
    candidate))

(cl-defun e-board-orchestration-run-set-projection
    (projections &key board-id more-p
                 (record-limit e-board-orchestration-run-set-default-record-limit)
                 (byte-limit e-board-orchestration-run-set-default-byte-limit)
                 (restore-state 'ready))
  "Reduce PROJECTIONS into one bounded Board-owned run-set value.

PROJECTIONS are already detached reduced run projections; this function does
not read SQLite or inspect live execution state.  The returned value contains
only the largest fitting ordered prefix and an omitted count.  MORE-P records
that the bounded query found, or may have hidden, another manifest; it does
not count active or omitted runs.  Ordering puts actionable attention first,
then restoring/dispatching/running/finishing, and uses newest event position
and run id as deterministic tie breakers."
  (unless (and (integerp record-limit) (> record-limit 0)
               (<= record-limit e-board-orchestration-run-set-max-record-limit))
    (signal 'e-board-orchestration-error
            (list "Run-set record limit is out of bounds" record-limit)))
  (unless (and (integerp byte-limit) (> byte-limit 0)
               (<= byte-limit e-board-orchestration-run-set-max-byte-limit))
    (signal 'e-board-orchestration-error
            (list "Run-set byte limit is out of bounds" byte-limit)))
  (unless (memq restore-state '(ready restoring unavailable))
    (signal 'e-board-orchestration-error
            (list "Run-set restore state is invalid" restore-state)))
  (let* ((entries
          (cl-remove-if-not
           (lambda (entry) (plist-get entry :active-p))
           (sort (mapcar (lambda (projection)
                           (e-board-orchestration--run-set-entry
                            projection board-id restore-state))
                         projections)
                (lambda (left right)
                  (let ((left-rank (plist-get left :actionable-rank))
                        (right-rank (plist-get right :actionable-rank))
                        (left-position (plist-get left :latest-event-position))
                        (right-position (plist-get right :latest-event-position)))
                    (or (< left-rank right-rank)
                        (and (= left-rank right-rank)
                             (or (> left-position right-position)
                                 (and (= left-position right-position)
                                      (string< (or (plist-get left :run-id) "")
                                               (or (plist-get right :run-id) "")))))))))))
         (active-count (length entries))
         (selected (cl-subseq entries 0 (min record-limit (length entries))))
         (omitted (max 0 (- (length entries) (length selected))))
         value)
    (setq value
          (append
           (list :board-id board-id
                 :restore-state restore-state
                 :ready-p (eq restore-state 'ready)
                 :status (if (eq restore-state 'ready)
                             (if (zerop active-count)
                                 'idle
                               (or (plist-get (car selected) :lifecycle)
                                   'running))
                           'restoring)
                 :runs selected :active-count active-count
                 :active-run-count active-count
                 :omitted-count omitted)
           (when more-p (list :more-p t))))
    ;; A byte budget applies to the final returned representation, including
    ;; its accounting fields.  Drop only from the end, preserving the ordered
    ;; actionable prefix, until the detached value fits.
    (let ((candidate (e-board-orchestration--run-set-with-byte-accounting value)))
      (while (> (e-board-orchestration--run-set-encoded-bytes candidate)
                byte-limit)
        (if (null selected)
            (signal 'e-board-orchestration-error
                    (list "Run-set metadata exceeds byte limit" board-id byte-limit))
          (setq selected (butlast selected)
                omitted (1+ omitted)
                value (plist-put value :runs selected)
                value (plist-put value :omitted-count omitted)
                candidate
                (e-board-orchestration--run-set-with-byte-accounting value))))
      candidate)))

(cl-defstruct (e-board-orchestration-run-set-state
               (:constructor e-board-orchestration-run-set-state--create))
  board-id current subscribers generation restore-state ready-p)

(defun e-board-orchestration-run-set-state-create (&key board-id)
  "Create a session-scoped detached run-set state for BOARD-ID."
  (let ((state (e-board-orchestration-run-set-state--create
                :board-id (copy-sequence board-id) :generation 0
                :restore-state 'restoring :ready-p nil :subscribers nil)))
    (setf (e-board-orchestration-run-set-state-current state)
          (e-board-orchestration-run-set-projection
           nil :board-id board-id :restore-state 'restoring))
    state))

(defun e-board-orchestration-run-set-state-update
    (state projections &rest options)
  "Replace STATE's bounded value from detached PROJECTIONS and notify clients."
  (unless (e-board-orchestration-run-set-state-p state)
    (signal 'wrong-type-argument
            (list 'e-board-orchestration-run-set-state-p state)))
  (let* ((value (apply #'e-board-orchestration-run-set-projection projections
                       :board-id (e-board-orchestration-run-set-state-board-id state)
                       options))
         (generation (1+ (e-board-orchestration-run-set-state-generation state))))
    (setf (e-board-orchestration-run-set-state-current state) (copy-tree value t)
          (e-board-orchestration-run-set-state-generation state) generation
          (e-board-orchestration-run-set-state-restore-state state)
            (plist-get value :restore-state)
          (e-board-orchestration-run-set-state-ready-p state)
            (plist-get value :ready-p))
    (dolist (subscriber (copy-sequence
                         (e-board-orchestration-run-set-state-subscribers state)))
      (funcall subscriber (copy-tree value t) generation))
    (copy-tree value t)))

(defun e-board-orchestration-run-set-state-set-value (state value)
  "Install detached bounded consumer VALUE into STATE and notify clients.
VALUE is already reduced by the Board SQL application service.  This narrow
setter avoids reducing the same durable projection a second time in the
presentation or context consumer, while retaining one immutable value for
both consumers."
  (unless (e-board-orchestration-run-set-state-p state)
    (signal 'wrong-type-argument
            (list 'e-board-orchestration-run-set-state-p state)))
  (unless (and (listp value) (plist-member value :runs)
               (plist-member value :omitted-count)
               (plist-member value :restore-state))
    (signal 'e-board-orchestration-error
            (list "Run-set value is not a bounded projection" value)))
  (let ((copy (copy-tree value t))
        (generation (1+ (e-board-orchestration-run-set-state-generation state))))
    (setf (e-board-orchestration-run-set-state-current state) copy
          (e-board-orchestration-run-set-state-generation state) generation
          (e-board-orchestration-run-set-state-restore-state state)
          (plist-get copy :restore-state)
          (e-board-orchestration-run-set-state-ready-p state)
          (plist-get copy :ready-p))
    (dolist (subscriber
             (copy-sequence
              (e-board-orchestration-run-set-state-subscribers state)))
      (funcall subscriber (copy-tree copy t) generation))
    (copy-tree copy t)))

(defun e-board-orchestration-run-set-state-value (state)
  "Return STATE's detached current bounded run-set value."
  (copy-tree (e-board-orchestration-run-set-state-current state) t))

(defun e-board-orchestration-run-set-state-subscribe (state callback)
  "Subscribe CALLBACK to STATE changes and return an unsubscribe function."
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (push callback (e-board-orchestration-run-set-state-subscribers state))
  (lambda ()
    (setf (e-board-orchestration-run-set-state-subscribers state)
          (delq callback
                (e-board-orchestration-run-set-state-subscribers state)))))

(defun e-board-orchestration-run-set-context (state)
  "Return the bounded model-context value for STATE."
  (let ((value (e-board-orchestration-run-set-state-value state)))
    (list :ready-p (plist-get value :ready-p)
          :status (plist-get value :status)
          :board-id (plist-get value :board-id)
          :active-count (plist-get value :active-count)
          :runs (copy-tree (plist-get value :runs) t)
          :omitted-count (plist-get value :omitted-count)
          :projection (copy-tree value t))))

(defun e-board-orchestration-run-set-compact-status
    (state &optional selected-run-id)
  "Return concise persistent chat status metadata for STATE."
  (let* ((value (e-board-orchestration-run-set-state-value state))
         (status (plist-get value :status))
         (runs (plist-get value :runs))
         (active-count (or (plist-get value :active-run-count) 0))
         (selected (and selected-run-id
                        (seq-find (lambda (run)
                                   (equal (plist-get run :run-id)
                                           selected-run-id))
                                  runs)))
         (summary (and selected
                       (list :run-id (plist-get selected :run-id)
                             :label (plist-get selected :label)
                             :lifecycle (plist-get selected :lifecycle)
                             :attention-p (plist-get selected :attention-p))))
         (text
          (concat
           (pcase status
             ('restoring (format "Board runs: restoring (%d active)"
                                 active-count))
             ('dispatching (format "Board runs: dispatching (%d active)"
                                   active-count))
             ('running (format "Board runs: running (%d active)"
                               active-count))
             ('finishing (format "Board runs: finishing (%d active)"
                                 active-count))
             ('attention (format "Board runs: attention (%d active)"
                                 active-count))
             (_ (format "Board runs: idle (%d active)" active-count)))
           (when summary
             (format " · %s (%s)"
                     (plist-get summary :label)
                     (plist-get summary :lifecycle))))))
    (list :text text :status status
          :active-run-count (plist-get value :active-run-count)
          :board-id (plist-get value :board-id)
          :summary summary
          :projection (copy-tree value t)
          :selected-run-id (and selected (plist-get selected :run-id))
          :activity-link (and selected
                              (list :kind 'board-activity
                                    :run-id (plist-get selected :run-id))))))

(defun e-board-orchestration-continuation-view (projection)
  "Return detached terminal evidence needed by PROJECTION's continuation.

The view deliberately excludes the manifest and continuation prompt.  It is a
bounded, request-scoped value derived from the already-queried Board facts, not
a durable Board replica or an invitation to query the run a second time."
  (unless (and (listp projection)
               (stringp (plist-get projection :run-id)))
    (signal 'e-board-orchestration-error
            (list "Continuation view requires a reduced run projection")))
  (list
   :run-id (plist-get projection :run-id)
   :terminal-status (plist-get projection :terminal-status)
   :tasks
   (mapcar
    (lambda (task)
      (let ((report (plist-get task :accepted-report)))
        (list
         :task-key (plist-get task :task-key)
         :required (plist-get task :required)
         :accepted-attempt (plist-get task :accepted-attempt)
         :state (plist-get task :state)
         :accepted-report
         (when report
           (append
            (list :task-key (plist-get report :task-key)
                  :attempt (plist-get report :attempt)
                  :status (plist-get report :status)
                  :summary (plist-get report :summary)
                  :outputs (copy-tree (plist-get report :outputs))
                  :error (copy-tree (plist-get report :error))
                  :participant-session-id
                  (plist-get report :participant-session-id))
            (when (plist-member report :result)
              (list :result (copy-tree (plist-get report :result)))))))))
    (plist-get projection :tasks))
   :conflicts (copy-tree (plist-get projection :conflicts))))

(provide 'e-board-orchestration)

;;; e-board-orchestration.el ends here
