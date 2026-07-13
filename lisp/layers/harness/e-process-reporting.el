;;; e-process-reporting.el --- Durable parent-side process markers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A small parent-facing observation tool backed by an append-only durable
;; store.  Marker capture keeps only agent judgment and links to redacted
;; session telemetry.  Triage and extraction accounting append separate facts.

;;; Code:

(require 'cl-lib)
(require 'json)
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

(defcustom e-process-reporting-lock-timeout 5.0
  "Seconds to wait for another process-reporting writer."
  :type 'number
  :group 'e-process-reporting)

(defcustom e-process-reporting-stale-lock-seconds 300
  "Age after which an unverifiable process-reporting lock is stale."
  :type 'integer
  :group 'e-process-reporting)

(defcustom e-process-reporting-directory
  (locate-user-emacs-file "e/process-reporting/")
  "Directory containing append-only process reporting records."
  :type 'directory
  :group 'e-process-reporting)

(defconst e-process-reporting-instructions
  "Call process_marker only for one high-value process observation worth retaining; do not narrate it or repeat unchanged evidence."
  "Minimal parent-facing process marker guidance.")

(defconst e-process-reporting-signals
  '("failure" "correction" "workaround" "repetition" "success"
    "missing-operation" "performance")
  "Accepted descriptive marker signals.")

(defconst e-process-reporting-outcomes
  '("runtime-defect" "missing-capability" "judgment-procedure"
    "deterministic-procedure" "unattended-work" "project-policy"
    "duplicate" "understood" "not-actionable")
  "Accepted first-slice triage outcome kinds.")

(defconst e-process-reporting-triage-statuses
  '("open" "routed" "closed" "rejected")
  "Accepted first-slice triage statuses.")

(cl-defstruct (e-process-reporting-store
               (:constructor e-process-reporting-store-create))
  directory
  (events nil)
  (markers (make-hash-table :test 'equal))
  (triage (make-hash-table :test 'equal))
  (extractions nil)
  loaded)

(defvar e-process-reporting-default-store
  (e-process-reporting-store-create :directory e-process-reporting-directory)
  "Default durable process reporting store.")

(defun e-process-reporting--file (store)
  "Return the append-only record file for STORE."
  (expand-file-name "records.jsonl"
                    (file-name-as-directory
                     (or (e-process-reporting-store-directory store)
                         e-process-reporting-directory))))

(defun e-process-reporting--timestamp ()
  "Return an ISO-8601 UTC timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun e-process-reporting--json-line (record)
  "Return RECORD as one deterministic JSON line."
  (concat (json-encode record)
          "\n"))

(defun e-process-reporting--lock-directory (store)
  "Return the inter-process lock directory for STORE."
  (concat (e-process-reporting--file store) ".lock"))

(defun e-process-reporting--lock-owner-file (store)
  "Return the lock owner file for STORE."
  (expand-file-name "owner.json" (e-process-reporting--lock-directory store)))

(defun e-process-reporting--write-lock-owner (store)
  "Write this Emacs process as lock owner for STORE."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region
     (json-encode (list :pid (emacs-pid)
                        :host (system-name)
                        :created-at (float-time)))
     nil (e-process-reporting--lock-owner-file store) nil 'silent)))

(defun e-process-reporting--read-lock-owner (store)
  "Return lock owner metadata for STORE, or nil."
  (condition-case nil
      (let ((json-object-type 'plist)
            (json-array-type 'list))
        (json-read-file (e-process-reporting--lock-owner-file store)))
    (error nil)))

(defun e-process-reporting--local-process-live-p (pid)
  "Return non-nil when local PID still exists."
  (and (integerp pid)
       (> pid 0)
       (ignore-errors (process-attributes pid))))

(defun e-process-reporting--stale-lock-p (store)
  "Return non-nil when STORE's lock can no longer have a live owner."
  (let* ((owner (e-process-reporting--read-lock-owner store))
         (pid (plist-get owner :pid))
         (host (plist-get owner :host))
         (created-at (plist-get owner :created-at)))
    (cond
     ((and (equal host (system-name)) (integerp pid))
      (not (e-process-reporting--local-process-live-p pid)))
     ((numberp created-at)
      (> (- (float-time) created-at)
         e-process-reporting-stale-lock-seconds))
     (t nil))))

(defun e-process-reporting--acquire-lock (store)
  "Acquire STORE's inter-process lock or signal a bounded error."
  (let ((lock (e-process-reporting--lock-directory store))
        (deadline (+ (float-time) e-process-reporting-lock-timeout))
        acquired)
    (make-directory (file-name-directory lock) t)
    (while (not acquired)
      (condition-case err
          (progn
            (make-directory lock)
            (setq acquired t)
            (condition-case owner-error
                (e-process-reporting--write-lock-owner store)
              (error
               (delete-directory lock t)
               (signal (car owner-error) (cdr owner-error)))))
        (file-already-exists
         (if (e-process-reporting--stale-lock-p store)
             (ignore-errors (delete-directory lock t))
           (when (>= (float-time) deadline)
             (signal 'file-error
                     (list "Timed out waiting for process-reporting store lock"
                           lock)))
           (sleep-for 0.01)))
        (error (signal (car err) (cdr err)))))
    lock))

(defun e-process-reporting--call-with-lock (store function)
  "Call FUNCTION with STORE exclusively locked."
  (let ((lock (e-process-reporting--acquire-lock store)))
    (unwind-protect
        (funcall function)
      (when (file-directory-p lock)
        (delete-directory lock t)))))

(defun e-process-reporting--append-file-unlocked (store record)
  "Append one complete RECORD to locked STORE."
  (let ((file (e-process-reporting--file store))
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region (e-process-reporting--json-line record)
                  nil file t 'silent)))

(defun e-process-reporting--index-event (store record)
  "Add append-only RECORD to STORE's in-memory indexes."
  (setf (e-process-reporting-store-events store)
        (append (e-process-reporting-store-events store) (list record)))
  (pcase (plist-get record :type)
    ("marker"
     (puthash (plist-get record :id) record
              (e-process-reporting-store-markers store)))
    ("triage"
     (let* ((marker-id (plist-get record :marker-id))
            (records (gethash marker-id
                              (e-process-reporting-store-triage store))))
       (puthash marker-id (append records (list record))
                (e-process-reporting-store-triage store))))
    ("extraction"
     (setf (e-process-reporting-store-extractions store)
           (append (e-process-reporting-store-extractions store)
                   (list record)))))
  record)

(defun e-process-reporting--reset-indexes (store)
  "Reset STORE's derived indexes."
  (setf (e-process-reporting-store-events store) nil
        (e-process-reporting-store-markers store)
        (make-hash-table :test 'equal)
        (e-process-reporting-store-triage store)
        (make-hash-table :test 'equal)
        (e-process-reporting-store-extractions store) nil))

(defun e-process-reporting--load-unlocked (store)
  "Load locked STORE and recover an incomplete final record."
  (e-process-reporting--reset-indexes store)
  (let ((file (e-process-reporting--file store))
        (coding-system-for-read 'utf-8))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let* ((end (point-max))
               (complete-end
                (if (or (= end (point-min))
                        (eq (char-before end) ?\n))
                    end
                  (save-excursion
                    (goto-char end)
                    (if (search-backward "\n" nil t)
                        (1+ (point))
                      (point-min))))))
          (when (< complete-end end)
            ;; A killed writer can leave only the final JSON line incomplete.
            ;; The exclusive lock makes truncation safe.  Complete malformed
            ;; lines still fail below instead of being silently discarded.
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) complete-end file nil 'silent))
            (delete-region complete-end end))
          (goto-char (point-min))
          (while (< (point) complete-end)
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (string-empty-p line)
                (e-process-reporting--index-event
                 store
                 (json-parse-string line
                                    :object-type 'plist
                                    :array-type 'list
                                    :null-object nil
                                    :false-object :json-false))))
            (forward-line 1)))))
    (setf (e-process-reporting-store-loaded store) t))
  store)

(defun e-process-reporting--append-unlocked (store record)
  "Append RECORD to locked STORE and its indexes."
  (e-process-reporting--append-file-unlocked store record)
  (e-process-reporting--index-event store record))

(defun e-process-reporting--with-current-store (store function)
  "Refresh STORE under lock, then call FUNCTION with it."
  (setq store (or store e-process-reporting-default-store))
  (e-process-reporting--call-with-lock
   store
   (lambda ()
     (e-process-reporting--load-unlocked store)
     (funcall function store))))

(defun e-process-reporting-load (store)
  "Load append-only STORE records and rebuild indexes safely."
  (e-process-reporting--with-current-store store #'identity))

(defun e-process-reporting-ensure-loaded (&optional store)
  "Return STORE refreshed from its durable records file."
  (e-process-reporting-load (or store e-process-reporting-default-store)))

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
  "Return the explicit operation chain relevant before marker capture."
  (let* ((events (e-process-reporting--activity-events harness session-id))
         (candidates
          (seq-filter
           (lambda (event)
             (and (equal (plist-get event :turn-id) turn-id)
                  (memq (plist-get event :event-type)
                        '(tool-finished action-finished action-failed
                          tool-started action-started))
                  (not (e-process-reporting--own-event-p event))))
           events))
         (latest (car (last candidates)))
         (latest-action
          (or (and latest
                   (memq (plist-get latest :event-type)
                         '(action-finished action-failed action-started))
                   latest)
              (seq-find
               (lambda (event)
                 (memq (plist-get event :event-type)
                       '(action-finished action-failed action-started)))
               (reverse candidates))))
         (parent-id (and latest-action
                         (e-process-reporting--event-parent-tool-call-id
                          latest-action)))
         (parent
          (and parent-id
               (seq-find
                (lambda (event)
                  (and (memq (plist-get event :event-type)
                             '(tool-finished tool-started))
                       (equal (e-process-reporting--event-call-id event)
                              parent-id)))
                (reverse candidates))))
         result)
    (dolist (event (list latest-action parent latest))
      (when (and event (not (memq event result)))
        (setq result (append result (list event)))))
    result))

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

(defun e-process-reporting--latest-triage (store marker-id)
  "Return latest triage record for MARKER-ID in STORE."
  (car (last (gethash marker-id (e-process-reporting-store-triage store)))))

(defun e-process-reporting--terminal-evidence-marker (store evidence-id)
  "Return marker for terminal EVIDENCE-ID, if one exists."
  (seq-find
   (lambda (marker)
     (when (equal (plist-get marker :evidence-id) evidence-id)
       (let ((triage (e-process-reporting--latest-triage
                      store (plist-get marker :id))))
         (member (plist-get triage :status) '("routed" "closed" "rejected")))))
   (hash-table-values (e-process-reporting-store-markers store))))

(defun e-process-reporting-mark (store arguments context)
  "Append a process marker described by ARGUMENTS using action CONTEXT."
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
    (e-process-reporting--with-current-store
     store
     (lambda (store)
       (if-let ((terminal (e-process-reporting--terminal-evidence-marker
                           store evidence-id)))
           (list :marker-id (plist-get terminal :id)
                 :evidence-id evidence-id
                 :suppressed t)
         (let* ((marker-id (e-session-generate-ulid))
                (project-root
                 (e-harness-project-root harness session-id turn-id))
                (record
                 (list :type "marker"
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
                       :checkpoint-entry-id
                       (e-process-reporting--checkpoint-entry-id
                        harness session-id tool-call-id)
                       :provider-request-id
                       (e-process-reporting--current-request-id
                        harness session-id turn-id)
                       :tool-call-id tool-call-id
                       :action-call-id (plist-get context :action-call-id)
                       :trigger trigger
                       :trigger-chain trigger-chain)))
           (e-process-reporting--append-unlocked store record)
           (copy-tree record)))))))

(defun e-process-reporting-list (store &optional arguments)
  "Return marker summaries from STORE, newest-first."
  (let* ((store (e-process-reporting-ensure-loaded store))
         (status (plist-get arguments :status))
         (markers
          (reverse
           (seq-filter
            (lambda (record) (equal (plist-get record :type) "marker"))
            (e-process-reporting-store-events store))))
         result)
    (dolist (marker markers (nreverse result))
      (let* ((triage (e-process-reporting--latest-triage
                      store (plist-get marker :id)))
             (current-status (or (plist-get triage :status) "open")))
        (when (or (null status) (equal status current-status))
          (push (list :marker-id (plist-get marker :id)
                      :evidence-id (plist-get marker :evidence-id)
                      :created-at (plist-get marker :created-at)
                      :signal (plist-get marker :signal)
                      :note (plist-get marker :note)
                      :session-id (plist-get marker :session-id)
                      :status current-status
                      :outcome (plist-get triage :outcome)
                      :target-reference (plist-get triage :target-reference))
                result))))))

(defun e-process-reporting-read (store marker-id)
  "Return immutable marker MARKER-ID and appended records from STORE."
  (let* ((store (e-process-reporting-ensure-loaded store))
         (marker (gethash marker-id
                          (e-process-reporting-store-markers store))))
    (unless marker
      (user-error "Unknown process marker: %s" marker-id))
    (list :marker (copy-tree marker)
          :triage (copy-tree
                   (gethash marker-id
                            (e-process-reporting-store-triage store)))
          :extractions
          (copy-tree
           (seq-filter
            (lambda (record)
              (member marker-id (plist-get record :marker-ids)))
            (e-process-reporting-store-extractions store))))))

(defun e-process-reporting-triage (store arguments)
  "Append a triage decision from ARGUMENTS to STORE."
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
    (e-process-reporting--with-current-store
     store
     (lambda (store)
       (unless (gethash marker-id (e-process-reporting-store-markers store))
         (user-error "Unknown process marker: %s" marker-id))
       (copy-tree
        (e-process-reporting--append-unlocked
         store
         (append
          (list :type "triage"
                :id (e-session-generate-ulid)
                :marker-id marker-id
                :created-at (e-process-reporting--timestamp)
                :outcome outcome
                :status status
                :decision-note decision-note)
          (when target
            (list :target-reference
                  (e-telemetry-redact-string target))))))))))

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

(defun e-process-reporting-record-extraction (store arguments context)
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
    (e-process-reporting--with-current-store
     store
     (lambda (store)
       (dolist (marker-id marker-ids)
         (unless (gethash marker-id (e-process-reporting-store-markers store))
           (user-error "Unknown process marker: %s" marker-id)))
       (copy-tree
        (e-process-reporting--append-unlocked
         store
         (list :type "extraction"
               :id (e-session-generate-ulid)
               :created-at (e-process-reporting--timestamp)
               :marker-ids marker-ids
               :session-evidence session-evidence
               :provider-request-ids request-ids
               :token-usage usage
               :estimation-method method
               :session-id (plist-get context :session-id)
               :turn-id (plist-get context :turn-id))))))))

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
         (cause-name (plist-get payload :caused-by-tool-name)))
    (list :provider-request-id (plist-get payload :provider-request-id)
          :provider-request-ordinal
          (plist-get payload :provider-request-ordinal)
          :caused-by-tool-call-id
          (plist-get payload :caused-by-tool-call-id)
          :marker-follow-up (equal cause-name "process_marker")
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

(defun e-process-reporting-cost-report (store context)
  "Return paired request-shape accounting for CONTEXT session markers."
  (let* ((store (e-process-reporting-ensure-loaded store))
         (harness (plist-get context :harness))
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
          (seq-filter
           (lambda (marker)
             (equal (plist-get marker :session-id) session-id))
           (hash-table-values
            (e-process-reporting-store-markers store))))
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
     :note (:type "string" :maxLength 280))
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
   :description "Save one process observation."
   :parameters e-process-reporting--marker-parameters
   :blocking-class 'cheap
   :handler
   (lambda (arguments)
     (e-actions-call 'process-reporting :mark arguments)
     "ok")))

(defun e-process-reporting-capability-create (&optional store)
  "Create process reporting capability backed by STORE."
  (let ((store (or store e-process-reporting-default-store)))
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
       (lambda (arguments context)
         (e-process-reporting-mark store arguments context)))
      :list
      (e-process-reporting--action
       "process_marker_list"
       '(:type "object"
         :properties (:status (:type "string"
                               :enum ["open" "routed" "closed" "rejected"])))
       (lambda (arguments _context)
         (e-process-reporting-list store arguments)))
      :read
      (e-process-reporting--action
       "process_marker_read" e-process-reporting--marker-id-parameters
       (lambda (arguments _context)
         (e-process-reporting-read
          store (e-process-reporting--string-argument
                 arguments :marker-id t))))
      :triage
      (e-process-reporting--action
       "process_marker_triage" e-process-reporting--triage-parameters
       (lambda (arguments _context)
         (e-process-reporting-triage store arguments)))
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
       (lambda (arguments context)
         (e-process-reporting-record-extraction store arguments context)))
      :cost-report
      (e-process-reporting--action
       "process_marker_cost_report" nil
       (lambda (_arguments context)
         (e-process-reporting-cost-report store context)))))))

(defun e-process-reporting-layer-create ()
  "Create the parent-side process reporting layer."
  (e-process-reporting-ensure-loaded)
  (e-layer-create
   :id 'process-reporting
   :name "Process Reporting"
   :capabilities (list (e-process-reporting-capability-create))))

(provide 'e-process-reporting)

;;; e-process-reporting.el ends here
