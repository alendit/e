;;; e-elisp-job.el --- Async Elisp worker jobs for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Process-backed Emacs Lisp validation jobs.  Agents reach these through
;; `run_elisp' by calling `e-actions-call' or the public Elisp functions below;
;; this is intentionally not a model-facing tool surface.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-capabilities)

(defgroup e-elisp-job nil
  "Async Emacs Lisp worker jobs for e."
  :group 'e)

(defcustom e-elisp-job-emacs-program nil
  "Emacs executable used by Elisp batch jobs.
When nil, prefer the current Emacs executable and fall back to `emacs' on
`exec-path'."
  :type '(choice (const :tag "Use current Emacs" nil)
                 file)
  :group 'e-elisp-job)

(defcustom e-elisp-job-progress-interval 0.5
  "Minimum seconds between internal Elisp job progress snapshots."
  :type 'number
  :group 'e-elisp-job)

;; Declared by `e-emacs-tools'.  A worker job is dispatched from `run_elisp'
;; through `e-actions-call', so this file's own trusted loads run inside the
;; interactive load guard; binding the bypass lets them resolve.
(defvar e-emacs-tools-bypass-run-elisp-load-guard)

(define-error 'e-elisp-job-invalid "Elisp job input is invalid")

(defvar e-elisp-job--jobs (make-hash-table :test #'equal)
  "Active and completed Elisp batch jobs by id.")

(defvar e-elisp-job--sequence 0
  "Monotonic sequence used to generate Elisp job ids.")

(defconst e-elisp-job-instructions
  (string-join
   '("Use Elisp job actions when external Elisp may do expensive work and must not run on the live UI Emacs path."
     "Start work from run_elisp with:"
     "(e-actions-call 'elisp-job :run-batch '(:code \"...\" :load_path [\"lisp\"] :timeout 10))"
     "Then poll or fetch:"
     "(e-actions-call 'elisp-job :status '(:job_id \"JOB\"))"
     "(e-actions-call 'elisp-job :result '(:job_id \"JOB\"))"
     "Cancel running work with:"
     "(e-actions-call 'elisp-job :cancel '(:job_id \"JOB\"))"
     "The worker runs a separate emacs -Q --batch process and does not activate code in the live Emacs.")
   "\n")
  "Instructions for nonblocking Elisp worker job actions.")

(defun e-elisp-job--argument-string (arguments key)
  "Return required string argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp key)))
    value))

(defun e-elisp-job--optional-positive-number (arguments key)
  "Return optional positive numeric KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (when value
      (unless (and (numberp value) (> value 0))
        (signal 'wrong-type-argument (list 'positive-number-p key)))
      value)))

(defun e-elisp-job--directory (arguments)
  "Return worker default directory from ARGUMENTS."
  (let ((directory (or (plist-get arguments :directory)
                       default-directory)))
    (unless (stringp directory)
      (signal 'wrong-type-argument (list 'stringp :directory)))
    (let ((expanded (file-name-as-directory (expand-file-name directory))))
      (unless (file-directory-p expanded)
        (signal 'e-elisp-job-invalid
                (list (format "Directory does not exist: %s" directory))))
      expanded)))

(defun e-elisp-job--load-path (arguments directory)
  "Return normalized load path entries from ARGUMENTS rooted at DIRECTORY."
  (let ((value (plist-get arguments :load_path)))
    (when (vectorp value)
      (setq value (append value nil)))
    (cond
     ((null value) nil)
     ((and (listp value) (seq-every-p #'stringp value))
      (mapcar (lambda (entry) (expand-file-name entry directory)) value))
     (t
      (signal 'e-elisp-job-invalid
              '("load_path must be an array of directory strings"))))))

(defun e-elisp-job--emacs-program ()
  "Return executable used for worker Emacs processes."
  (or e-elisp-job-emacs-program
      (and (stringp invocation-name)
           (executable-find invocation-name))
      (and (stringp invocation-directory)
           (stringp invocation-name)
           (expand-file-name invocation-name invocation-directory))
      (executable-find "emacs")
      (signal 'e-elisp-job-invalid '("Cannot find an Emacs executable"))))

(defun e-elisp-job--max-bytes ()
  "Return maximum preview bytes for job output."
  (if (boundp 'e-tool-output-truncation-max-bytes)
      e-tool-output-truncation-max-bytes
    (* 16 1024)))

(defun e-elisp-job--max-lines ()
  "Return maximum preview lines for job output."
  (if (boundp 'e-tool-output-truncation-max-lines)
      e-tool-output-truncation-max-lines
    2000))

(defun e-elisp-job--line-count (text)
  "Return the number of logical lines in TEXT."
  (if (string-empty-p text)
      0
    (let ((count 1)
          (start 0))
      (while (string-match "\n" text start)
        (setq count (1+ count))
        (setq start (match-end 0)))
      (when (string-suffix-p "\n" text)
        (setq count (1- count)))
      count)))

(defun e-elisp-job--line-prefix (text max-lines)
  "Return TEXT limited to MAX-LINES logical lines."
  (if (<= (e-elisp-job--line-count text) max-lines)
      text
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (forward-line max-lines)
      (buffer-substring-no-properties (point-min) (point)))))

(defun e-elisp-job--byte-prefix (text max-bytes)
  "Return TEXT limited to MAX-BYTES UTF-8 bytes without splitting characters."
  (let ((bytes 0)
        (index 0)
        (length (length text)))
    (while (and (< index length)
                (let ((next-bytes
                       (string-bytes (substring text index (1+ index)))))
                  (when (<= (+ bytes next-bytes) max-bytes)
                    (setq bytes (+ bytes next-bytes))
                    t)))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-elisp-job--bounded-preview (text)
  "Return bounded preview text for TEXT."
  (e-elisp-job--byte-prefix
   (e-elisp-job--line-prefix text (e-elisp-job--max-lines))
   (e-elisp-job--max-bytes)))

(defun e-elisp-job--safe-fragment (value fallback)
  "Return VALUE as a safe path fragment, or FALLBACK."
  (let* ((text (if (and (stringp value)
                        (not (string-empty-p value)))
                   value
                 fallback))
         (safe (replace-regexp-in-string "[^A-Za-z0-9._-]" "-" text)))
    (if (string-empty-p safe) fallback safe)))

(defun e-elisp-job--next-id ()
  "Return a new Elisp job id."
  (setq e-elisp-job--sequence (1+ e-elisp-job--sequence))
  (format "elisp-job-%d-%d" (truncate (float-time)) e-elisp-job--sequence))

(defun e-elisp-job--relative-output-name (action-context job-id)
  "Return session-local output name for ACTION-CONTEXT and JOB-ID."
  (format
   "elisp-jobs/%s/%s.txt"
   (e-elisp-job--safe-fragment
    (plist-get action-context :turn-id)
    "turn")
   (e-elisp-job--safe-fragment job-id "job")))

(defun e-elisp-job--output-target (action-context job-id)
  "Return output target plist for ACTION-CONTEXT and JOB-ID."
  (let* ((relative (e-elisp-job--relative-output-name action-context job-id))
         (harness (plist-get action-context :harness))
         (session-id (plist-get action-context :session-id)))
    (if (and harness
             session-id
             ;; This is a trusted runtime load, not agent-authored code.  Bind
             ;; the bypass so the interactive `run_elisp' load guard permits it
             ;; instead of rejecting the require that backs the job output file.
             (let ((e-emacs-tools-bypass-run-elisp-load-guard t))
               (require 'e-session-tmp-resources nil t))
             (fboundp 'e-session-tmp-file-path))
        (list :path (e-session-tmp-file-path harness session-id relative)
              :uri (concat "tmp://" relative))
      (list :path (make-temp-file "e-elisp-job-" nil ".log")
            :uri nil))))

(defun e-elisp-job--with-utf8-write (thunk)
  "Call THUNK with noninteractive UTF-8 file writes."
  (let ((coding-system-for-write 'utf-8-unix)
        (select-safe-coding-system-function nil))
    (funcall thunk)))

(defun e-elisp-job--collector-create (action-context job-id)
  "Return output collector for ACTION-CONTEXT and JOB-ID."
  (let ((target (e-elisp-job--output-target action-context job-id)))
    (e-elisp-job--with-utf8-write
     (lambda ()
       (write-region "" nil (plist-get target :path) nil 'silent)))
    (list :path (plist-get target :path)
          :uri (plist-get target :uri)
          :preview ""
          :bytes 0
          :line-breaks 0
          :ends-newline nil)))

(defun e-elisp-job--collector-count-chunk (collector chunk)
  "Record CHUNK size and line stats in COLLECTOR."
  (plist-put collector :bytes
             (+ (plist-get collector :bytes)
                (string-bytes chunk)))
  (let ((start 0)
        (count 0))
    (while (string-match "\n" chunk start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    (plist-put collector :line-breaks
               (+ (plist-get collector :line-breaks) count)))
  (when (> (length chunk) 0)
    (plist-put collector :ends-newline (string-suffix-p "\n" chunk))))

(defun e-elisp-job--collector-lines (collector)
  "Return logical output lines recorded in COLLECTOR."
  (cond
   ((zerop (plist-get collector :bytes)) 0)
   ((plist-get collector :ends-newline)
    (plist-get collector :line-breaks))
   (t
    (1+ (plist-get collector :line-breaks)))))

(defun e-elisp-job--collector-append (collector chunk)
  "Append CHUNK to COLLECTOR output."
  (e-elisp-job--collector-count-chunk collector chunk)
  (e-elisp-job--with-utf8-write
   (lambda ()
     (write-region chunk nil (plist-get collector :path) t 'silent)))
  (plist-put collector :preview
             (e-elisp-job--bounded-preview
              (concat (plist-get collector :preview) chunk))))

(defun e-elisp-job--collector-truncated-p (collector)
  "Return non-nil when COLLECTOR preview is truncated."
  (or (> (plist-get collector :bytes)
         (string-bytes (plist-get collector :preview)))
      (> (e-elisp-job--collector-lines collector)
         (e-elisp-job--line-count (plist-get collector :preview)))))

(defun e-elisp-job--content (collector suffix)
  "Return result content for COLLECTOR with optional SUFFIX."
  (let* ((preview (plist-get collector :preview))
         (uri (or (plist-get collector :uri)
                  (plist-get collector :path)))
         (notice (when (e-elisp-job--collector-truncated-p collector)
                   (format "[Elisp job output truncated: full output: %s]"
                           uri))))
    (or (string-join
         (delq nil
               (list (unless (string-empty-p preview) preview)
                     suffix
                     notice))
         "\n\n")
        "")))

(defun e-elisp-job--metadata (collector exit-code)
  "Return metadata for COLLECTOR and EXIT-CODE."
  (append
   (list :exit_code exit-code
         :output_file (plist-get collector :path)
         :truncated (and (e-elisp-job--collector-truncated-p collector) t)
         :original_bytes (plist-get collector :bytes)
         :original_lines (e-elisp-job--collector-lines collector)
         :shown_bytes (string-bytes (plist-get collector :preview))
         :shown_lines (e-elisp-job--line-count
                       (plist-get collector :preview)))
   (when-let ((uri (plist-get collector :uri)))
     (list :tmp_uri uri))))

(defun e-elisp-job--command (code worker-load-path)
  "Return worker command for CODE with WORKER-LOAD-PATH."
  (append
   (list (e-elisp-job--emacs-program) "-Q" "--batch")
   (cl-mapcan (lambda (path) (list "-L" path)) worker-load-path)
   (list "--eval" code)))

(defun e-elisp-job--job-put (job key value)
  "Set JOB KEY to VALUE and store the updated job."
  (setq job (plist-put job key value))
  (puthash (plist-get job :job_id) job e-elisp-job--jobs)
  job)

(defun e-elisp-job--job (job-id)
  "Return job JOB-ID or signal."
  (or (gethash job-id e-elisp-job--jobs)
      (signal 'e-elisp-job-invalid
              (list (format "Unknown Elisp job: %s" job-id)))))

(defun e-elisp-job--snapshot (job &optional include-content)
  "Return public snapshot for JOB.
When INCLUDE-CONTENT is non-nil, include the current bounded content preview."
  (let* ((collector (plist-get job :collector))
         (metadata (e-elisp-job--metadata
                    collector
                    (plist-get job :exit_code))))
    (append
     (list :job_id (plist-get job :job_id)
           :operation (plist-get job :operation)
           :status (plist-get job :status)
           :started_at (plist-get job :started_at)
           :finished_at (plist-get job :finished_at)
           :metadata metadata)
     (when include-content
       (list :content
             (e-elisp-job--content collector (plist-get job :message)))))))

(defun e-elisp-job-run-batch (arguments &optional action-context)
  "Start an async Emacs batch job from ARGUMENTS.
ARGUMENTS is a plist with :code and optional :directory, :load_path, and
:timeout.  ACTION-CONTEXT is the context passed by `e-actions-call'."
  (let* ((code (e-elisp-job--argument-string arguments :code))
         (timeout (e-elisp-job--optional-positive-number arguments :timeout))
         (directory (e-elisp-job--directory arguments))
         (worker-load-path (e-elisp-job--load-path arguments directory))
         (job-id (e-elisp-job--next-id))
         (collector (e-elisp-job--collector-create action-context job-id))
         (job (list :job_id job-id
                    :operation "run-batch"
                    :status 'running
                    :started_at (float-time)
                    :finished_at nil
                    :exit_code nil
                    :message nil
                    :collector collector))
         (default-directory directory)
         process
         timeout-timer
         progress-timer
         settled)
    (cl-labels
        ((store
          (key value)
          (setq job (e-elisp-job--job-put job key value)))
         (cleanup
          ()
          (when (timerp timeout-timer)
            (cancel-timer timeout-timer))
          (when (timerp progress-timer)
            (cancel-timer progress-timer)))
         (mark
          (status exit-code message)
          (unless settled
            (setq settled t)
            (cleanup)
            (store :status status)
            (store :exit_code exit-code)
            (store :message message)
            (store :finished_at (float-time))))
         (cancel
          ()
          (unless settled
            (setq settled t)
            (cleanup)
            (when (and process (process-live-p process))
              (kill-process process))
            (store :status 'cancelled)
            (store :message "Elisp job cancelled")
            (store :finished_at (float-time)))
          t))
      (setq process
            (make-process
             :name "e-elisp-job"
             :command (e-elisp-job--command code worker-load-path)
             :connection-type 'pipe
             :coding 'utf-8-unix
             :noquery t
             :filter
             (lambda (_proc chunk)
               (e-elisp-job--collector-append collector chunk)
               ;; Keep a lightweight timer edge so status polling can observe
               ;; recent output without forcing a tool-progress surface.
               (unless (timerp progress-timer)
                 (setq progress-timer
                       (run-at-time e-elisp-job-progress-interval nil
                                    (lambda ()
                                      (setq progress-timer nil))))))
             :sentinel
             (lambda (proc _event)
               (when (and (not settled)
                          (memq (process-status proc) '(exit signal)))
                 (let ((exit-code (process-exit-status proc)))
                   (if (zerop exit-code)
                       (mark 'ok exit-code nil)
                     (mark 'error
                           exit-code
                           (format "Emacs batch process exited with code %d"
                                   exit-code))))))))
      (set-process-query-on-exit-flag process nil)
      (setq job (plist-put job :process process))
      (setq job (plist-put job :cancel #'cancel))
      (puthash job-id job e-elisp-job--jobs)
      (when timeout
        (setq timeout-timer
              (run-at-time
               timeout nil
               (lambda ()
                 (unless settled
                   (when (process-live-p process)
                     (kill-process process))
                   (mark
                    'error
                    nil
                    (format "Emacs batch process timed out after %s seconds"
                            timeout)))))))
      (e-elisp-job--snapshot job))))

(defun e-elisp-job-status (job-id)
  "Return status for Elisp JOB-ID."
  (e-elisp-job--snapshot (e-elisp-job--job job-id) t))

(defun e-elisp-job-result (job-id)
  "Return result for Elisp JOB-ID."
  (e-elisp-job--snapshot (e-elisp-job--job job-id) t))

(defun e-elisp-job-cancel (job-id)
  "Cancel Elisp JOB-ID and return its status."
  (let* ((job (e-elisp-job--job job-id))
         (cancel (plist-get job :cancel)))
    (when (and (eq (plist-get job :status) 'running)
               (functionp cancel))
      (funcall cancel))
    (e-elisp-job--snapshot (e-elisp-job--job job-id) t)))

(defun e-elisp-job--job-id-argument (arguments)
  "Return job id from ARGUMENTS."
  (e-elisp-job--argument-string arguments :job_id))

(defun e-elisp-job--run-batch-action (action-context arguments)
  "Run Elisp batch action for ACTION-CONTEXT with ARGUMENTS."
  (e-elisp-job-run-batch arguments action-context))

(defun e-elisp-job--status-action (_action-context arguments)
  "Return status action result for ARGUMENTS."
  (e-elisp-job-status (e-elisp-job--job-id-argument arguments)))

(defun e-elisp-job--result-action (_action-context arguments)
  "Return result action result for ARGUMENTS."
  (e-elisp-job-result (e-elisp-job--job-id-argument arguments)))

(defun e-elisp-job--cancel-action (_action-context arguments)
  "Cancel action result for ARGUMENTS."
  (e-elisp-job-cancel (e-elisp-job--job-id-argument arguments)))

(defun e-elisp-job--action (caller description parameters)
  "Return action descriptor for CALLER with DESCRIPTION and PARAMETERS."
  (e-action-create
   :caller caller
   :handler (lambda (_arguments) nil)
   :description description
   :parameters parameters))

(defconst e-elisp-job--run-batch-parameters
  '(:type "object"
    :properties
    (:code
     (:type "string"
      :description "Emacs Lisp evaluated by the worker with --eval.")
     :directory
     (:type "string"
      :description "Worker default directory. Defaults to live default-directory.")
     :load_path
     (:type "array"
      :items (:type "string")
      :description "Directories passed to the worker with -L before evaluation.")
     :timeout
     (:type "number"
      :description "Hard timeout in seconds. Reaching it kills the worker."))
    :required ["code"])
  "Action parameters for `elisp-job' run-batch.")

(defconst e-elisp-job--job-id-parameters
  '(:type "object"
    :properties
    (:job_id
     (:type "string"
      :description "Elisp job id returned by :run-batch."))
    :required ["job_id"])
  "Action parameters for job lookup operations.")

(defun e-elisp-job-capability-create ()
  "Create the Elisp job action capability."
  (e-capability-create
   :id 'elisp-job
   :name "Elisp Job"
   :instruction-priority 245
   :instructions e-elisp-job-instructions
   :actions
   (list :run-batch
         (e-elisp-job--action
          #'e-elisp-job--run-batch-action
          "Start a separate emacs -Q --batch worker for expensive Elisp validation."
          e-elisp-job--run-batch-parameters)
         :status
         (e-elisp-job--action
          #'e-elisp-job--status-action
          "Return status and bounded output preview for an Elisp job."
          e-elisp-job--job-id-parameters)
         :result
         (e-elisp-job--action
          #'e-elisp-job--result-action
          "Return the current or final result for an Elisp job."
          e-elisp-job--job-id-parameters)
         :cancel
         (e-elisp-job--action
          #'e-elisp-job--cancel-action
          "Cancel a running Elisp job."
          e-elisp-job--job-id-parameters))))

(provide 'e-elisp-job)

;;; e-elisp-job.el ends here
