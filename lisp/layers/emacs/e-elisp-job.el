;;; e-elisp-job.el --- Async Elisp worker jobs for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Process-backed Emacs Lisp validation jobs.  This gives agents a non-UI path
;; for expensive byte-compilation and batch checks instead of loading external
;; Elisp into the live Emacs from run_elisp.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-tools)

(defgroup e-elisp-job nil
  "Async Emacs Lisp worker jobs for e."
  :group 'e)

(defcustom e-elisp-job-emacs-program nil
  "Emacs executable used by `elisp_job'.
When nil, prefer the current Emacs executable and fall back to `emacs' on
`exec-path'."
  :type '(choice (const :tag "Use current Emacs" nil)
                 file)
  :group 'e-elisp-job)

(defcustom e-elisp-job-progress-interval 0.5
  "Minimum seconds between streaming `elisp_job' progress events."
  :type 'number
  :group 'e-elisp-job)

(define-error 'e-elisp-job-invalid "Elisp job input is invalid")

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

(defun e-elisp-job--relative-output-name (context call)
  "Return session-local output name for CONTEXT and CALL."
  (format
   "tool-results/%s/%s-%s.txt"
   (e-elisp-job--safe-fragment (plist-get context :turn-id) "turn")
   (e-elisp-job--safe-fragment (plist-get call :name) "elisp-job")
   (e-elisp-job--safe-fragment (plist-get call :id) "call")))

(defun e-elisp-job--output-target (context call)
  "Return output target plist for CONTEXT and CALL."
  (let* ((relative (e-elisp-job--relative-output-name context call))
         (harness (plist-get context :harness))
         (session-id (plist-get context :session-id)))
    (if (and harness
             session-id
             (require 'e-session-tmp-resources nil t)
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

(defun e-elisp-job--collector-create (context call)
  "Return output collector for CONTEXT and CALL."
  (let ((target (e-elisp-job--output-target context call)))
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

(defun e-elisp-job--progress-payload (collector call)
  "Return streaming progress payload for COLLECTOR and CALL."
  (append
   (list :tool-call-id (plist-get call :id)
         :operation "run-batch"
         :bytes (plist-get collector :bytes)
         :lines (e-elisp-job--collector-lines collector)
         :preview (plist-get collector :preview)
         :output-file (plist-get collector :path))
   (when-let ((uri (plist-get collector :uri)))
     (list :output-uri uri))))

(defun e-elisp-job--finish-content (collector suffix)
  "Return final tool content for COLLECTOR with optional SUFFIX."
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

(defun e-elisp-job--finish-metadata (collector exit-code)
  "Return final metadata for COLLECTOR and EXIT-CODE."
  (append
   (list :exit-code exit-code
         :output-file (plist-get collector :path)
         :truncated (and (e-elisp-job--collector-truncated-p collector) t)
         :original-bytes (plist-get collector :bytes)
         :original-lines (e-elisp-job--collector-lines collector)
         :shown-bytes (string-bytes (plist-get collector :preview))
         :shown-lines (e-elisp-job--line-count
                       (plist-get collector :preview)))
   (when-let ((uri (plist-get collector :uri)))
     (list :tmp-uri uri))))

(defun e-elisp-job--command (code worker-load-path)
  "Return worker command for CODE with WORKER-LOAD-PATH."
  (append
   (list (e-elisp-job--emacs-program) "-Q" "--batch")
   (cl-mapcan (lambda (path) (list "-L" path)) worker-load-path)
   (list "--eval" code)))

(cl-defun e-elisp-job--run-batch-start
    (arguments &key on-done on-error on-request-start on-event)
  "Start a batch Elisp worker from ARGUMENTS."
  (let* ((operation (e-elisp-job--argument-string arguments :operation))
         (code (e-elisp-job--argument-string arguments :code))
         (timeout (e-elisp-job--optional-positive-number arguments :timeout))
         (directory (e-elisp-job--directory arguments))
         (worker-load-path (e-elisp-job--load-path arguments directory)))
    (unless (equal operation "run-batch")
      (signal 'e-elisp-job-invalid
              (list (format "Unsupported elisp_job operation: %s"
                            operation))))
    (let* ((default-directory directory)
           (context (e-tools-current-context))
           (call (plist-get context :tool-call))
           (collector (e-elisp-job--collector-create context call))
           (settled nil)
           (last-progress-time nil)
           (progress-pending nil)
           process
           progress-timer
           timeout-timer
           request)
      (cl-labels
          ((cleanup
            ()
            (when (timerp timeout-timer)
              (cancel-timer timeout-timer))
            (when (timerp progress-timer)
              (cancel-timer progress-timer)))
           (emit-progress
            ()
            (when (and on-event
                       (> (plist-get collector :bytes) 0))
              (when (timerp progress-timer)
                (cancel-timer progress-timer))
              (setq progress-timer nil)
              (setq progress-pending nil)
              (setq last-progress-time (float-time))
              (funcall on-event
                       'tool-progress
                       (e-elisp-job--progress-payload collector call))))
           (request-progress
            ()
            (when on-event
              (setq progress-pending t)
              (let* ((interval (max 0 e-elisp-job-progress-interval))
                     (now (float-time))
                     (elapsed (and last-progress-time
                                   (- now last-progress-time))))
                (cond
                 ((or (zerop interval)
                      (not last-progress-time)
                      (and elapsed (>= elapsed interval)))
                  (emit-progress))
                 ((not (timerp progress-timer))
                  (setq progress-timer
                        (run-at-time
                         (max 0 (- interval (or elapsed 0)))
                         nil
                         (lambda ()
                           (setq progress-timer nil)
                           (when (and progress-pending (not settled))
                             (emit-progress))))))))))
           (finish
            (status exit-code &optional suffix)
            (unless settled
              (when (and on-event
                         (> (plist-get collector :bytes) 0))
                (emit-progress))
              (setq settled t)
              (cleanup)
              (when on-done
                (funcall on-done
                         (e-tools-result-create
                          call
                          status
                          (e-elisp-job--finish-content collector suffix)
                          (e-elisp-job--finish-metadata
                           collector exit-code))))))
           (cancel
            ()
            (unless settled
              (setq settled t)
              (cleanup)
              (when (and process (process-live-p process))
                (kill-process process)))
            t))
        (condition-case err
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
                     (request-progress))
                   :sentinel
                   (lambda (proc _event)
                     (when (and (not settled)
                                (memq (process-status proc) '(exit signal)))
                       (let ((exit-code (process-exit-status proc)))
                         (if (zerop exit-code)
                             (finish 'ok exit-code)
                           (finish
                            'error
                            exit-code
                            (format "Emacs batch process exited with code %d"
                                    exit-code))))))))
          (error
           (cleanup)
           (if on-error
               (funcall on-error err)
             (signal (car err) (cdr err)))))
        (set-process-query-on-exit-flag process nil)
        (setq request
              (e-tools-request-create
               :cancel #'cancel
               :metadata (list :transport 'process
                               :process process
                               :operation operation
                               :output-file (plist-get collector :path)
                               :output-uri (plist-get collector :uri)
                               :cancellable t)))
        (when on-request-start
          (funcall on-request-start request))
        (when timeout
          (setq timeout-timer
                (run-at-time
                 timeout nil
                 (lambda ()
                   (unless settled
                     (when (process-live-p process)
                       (kill-process process))
                     (finish
                      'error
                      nil
                      (format "Emacs batch process timed out after %s seconds"
                              timeout)))))))
        request))))

(defun e-elisp-job-register (registry)
  "Register `elisp_job' in REGISTRY."
  (e-tools-register
   registry
   :name "elisp_job"
   :description
   (concat
    "Run expensive Emacs Lisp validation in a separate `emacs -Q --batch' "
    "worker process. Use this for byte-compilation, loading untrusted or "
    "expensive external Elisp for inspection, and repository-local validation "
    "that must not freeze the live UI Emacs. This tool does not activate code "
    "inside the live Emacs.")
   :blocking-class 'process
   :parameters '(:type "object"
                 :properties
                 (:operation
                  (:type "string"
                   :enum ["run-batch"]
                   :description "Worker operation. Currently only run-batch is supported.")
                  :code
                  (:type "string"
                   :description "Emacs Lisp evaluated by the worker with --eval.")
                  :directory
                  (:type "string"
                   :description "Worker default directory. Defaults to the live default-directory.")
                  :load_path
                  (:type "array"
                   :items (:type "string")
                   :description "Directories passed to the worker with -L before evaluation.")
                  :timeout
                  (:type "number"
                   :description "Hard timeout in seconds. When reached, e kills the worker and returns a tool error."))
                 :required ["operation" "code"])
   :start
   (cl-function
    (lambda (&key arguments on-done on-error on-request-start
                  on-event &allow-other-keys)
      (e-elisp-job--run-batch-start
       arguments
       :on-done on-done
       :on-error on-error
       :on-request-start on-request-start
       :on-event on-event)))))

(provide 'e-elisp-job)

;;; e-elisp-job.el ends here
