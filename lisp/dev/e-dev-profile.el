;;; e-dev-profile.el --- Developer profiling traces for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Opt-in development profiling support.  The recorder writes compact JSONL
;; timing records while enabled and is inert during normal runtime use.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'e)

(defgroup e-dev-profile nil
  "Developer profiling traces for e."
  :group 'e-dev
  :prefix "e-dev-profile-")

(defcustom e-dev-profile-directory
  (expand-file-name ".e/dev-profiles/" (e-source-directory))
  "Directory where developer profiling trace files are written."
  :type 'directory
  :group 'e-dev-profile)

(defvar e-dev-profile--enabled nil
  "Non-nil when developer profiling trace recording is enabled.")

(defvar e-dev-profile--current-file nil
  "Current JSONL profiling trace file, or nil when profiling is disabled.")

(defvar e-dev-profile--latest-file nil
  "Most recent JSONL profiling trace file.")

(defun e-dev-profile-enabled-p ()
  "Return non-nil when developer profiling is enabled."
  e-dev-profile--enabled)

(defun e-dev-profile--timestamp-file-name ()
  "Return a timestamped JSONL trace file name."
  (format-time-string "%Y%m%d-%H%M%S.jsonl"))

(defun e-dev-profile--metadata-alist (metadata)
  "Return METADATA as an alist suitable for JSON encoding."
  (cond
   ((null metadata) nil)
   ((keywordp (car-safe metadata))
    (let (result)
      (while metadata
        (let ((key (pop metadata))
              (value (pop metadata)))
          (push (cons (intern (substring (symbol-name key) 1)) value)
                result)))
      (nreverse result)))
   ((and (listp metadata)
         (consp (car metadata)))
    metadata)
   (t
    `((value . ,(format "%S" metadata))))))

(defun e-dev-profile--record-alist (event duration-ms session-id turn-id buffer-name metadata)
  "Build a compact profiling record alist."
  (let ((record `((timestamp . ,(float-time))
                  (event . ,(if (symbolp event) (symbol-name event) event)))))
    (when duration-ms
      (push (cons 'duration-ms duration-ms) record))
    (when session-id
      (push (cons 'session-id session-id) record))
    (when turn-id
      (push (cons 'turn-id turn-id) record))
    (when buffer-name
      (push (cons 'buffer-name buffer-name) record))
    (when metadata
      (push (cons 'metadata (e-dev-profile--metadata-alist metadata)) record))
    (nreverse record)))

(cl-defun e-dev-profile-record (event &key duration-ms session-id turn-id buffer-name metadata)
  "Record profiling EVENT with optional scalar timing fields.
Return non-nil when a record was written."
  (when (and e-dev-profile--enabled e-dev-profile--current-file)
    (make-directory (file-name-directory e-dev-profile--current-file) t)
    (let ((record (e-dev-profile--record-alist
                   event duration-ms session-id turn-id buffer-name metadata))
          (coding-system-for-write 'utf-8))
      (with-temp-buffer
        (insert (json-encode record))
        (insert "\n")
        (append-to-file (point-min) (point-max) e-dev-profile--current-file)))
    t))

(defun e-dev-profile-measure-thunk (event options thunk)
  "Measure THUNK as profiling EVENT with plist OPTIONS.
The THUNK result is preserved.  If THUNK signals an error, a timing record is
written with compact error metadata and the original error is re-signaled."
  (if (not (e-dev-profile-enabled-p))
      (funcall thunk)
    (let ((start (float-time)))
      (condition-case err
          (let ((result (funcall thunk)))
            (e-dev-profile-record
             event
             :duration-ms (* 1000.0 (- (float-time) start))
             :session-id (plist-get options :session-id)
             :turn-id (plist-get options :turn-id)
             :buffer-name (plist-get options :buffer-name)
             :metadata (plist-get options :metadata))
            result)
        (error
         (let ((metadata (append (e-dev-profile--metadata-alist
                                  (plist-get options :metadata))
                                 `((error . ,(error-message-string err))))))
           (e-dev-profile-record
            event
            :duration-ms (* 1000.0 (- (float-time) start))
            :session-id (plist-get options :session-id)
            :turn-id (plist-get options :turn-id)
            :buffer-name (plist-get options :buffer-name)
            :metadata metadata))
         (signal (car err) (cdr err)))))))

(defmacro e-dev-profile-measure (event options &rest body)
  "Measure BODY as profiling EVENT with keyword OPTIONS."
  (declare (indent 2) (debug (form sexp body)))
  `(e-dev-profile-measure-thunk ,event (list ,@options) (lambda () ,@body)))

;;;###autoload
(defun e-dev-profile-start ()
  "Start a new developer profiling trace and return its file path."
  (interactive)
  (make-directory e-dev-profile-directory t)
  (let ((file (expand-file-name (e-dev-profile--timestamp-file-name)
                                e-dev-profile-directory)))
    (setq e-dev-profile--enabled t
          e-dev-profile--current-file file
          e-dev-profile--latest-file file)
    (when (called-interactively-p 'interactive)
      (message "Started e profile trace: %s" (abbreviate-file-name file)))
    file))

;;;###autoload
(defun e-dev-profile-stop ()
  "Stop the active developer profiling trace and return the latest file path."
  (interactive)
  (let ((file (or e-dev-profile--current-file e-dev-profile--latest-file)))
    (setq e-dev-profile--enabled nil
          e-dev-profile--current-file nil
          e-dev-profile--latest-file file)
    (when (called-interactively-p 'interactive)
      (if file
          (message "Stopped e profile trace: %s" (abbreviate-file-name file))
        (message "No e profile trace was active"))
      (when file
        (e-dev-profile-report file)))
    file))

(defun e-dev-profile--read-json-lines (file)
  "Read JSONL profiling records from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          records)
      (dolist (line (split-string (buffer-string) "\n" t))
        (push (json-read-from-string line) records))
      (nreverse records))))

(defun e-dev-profile--record-duration (record)
  "Return RECORD duration in milliseconds, or 0.0."
  (or (alist-get 'duration-ms record) 0.0))

(defun e-dev-profile--aggregate-records (records)
  "Return aggregate timing data for RECORDS."
  (let (aggregates)
    (dolist (record records)
      (let* ((event (alist-get 'event record))
             (duration (e-dev-profile--record-duration record))
             (existing (alist-get event aggregates nil nil #'equal))
             (count (1+ (or (plist-get existing :count) 0)))
             (total (+ duration (or (plist-get existing :total-ms) 0.0)))
             (max-ms (max duration (or (plist-get existing :max-ms) 0.0))))
        (setf (alist-get event aggregates nil nil #'equal)
              (list :count count
                    :total-ms total
                    :average-ms (/ total count)
                    :max-ms max-ms))))
    (sort aggregates
          (lambda (left right)
            (> (plist-get (cdr left) :total-ms)
               (plist-get (cdr right) :total-ms))))))

(defun e-dev-profile-report-data (&optional file)
  "Return aggregate report data for profiling trace FILE.
When FILE is nil, use the latest trace file."
  (let* ((trace-file (or file e-dev-profile--latest-file))
         (records (if (and trace-file (file-exists-p trace-file))
                      (e-dev-profile--read-json-lines trace-file)
                    nil))
         (slowest (seq-take
                   (sort (copy-sequence records)
                         (lambda (left right)
                           (> (e-dev-profile--record-duration left)
                              (e-dev-profile--record-duration right))))
                   10)))
    (list :file trace-file
          :event-count (length records)
          :started-at (alist-get 'timestamp (car records))
          :finished-at (alist-get 'timestamp (car (last records)))
          :aggregates (e-dev-profile--aggregate-records records)
          :slowest slowest)))

(defun e-dev-profile-format-report (report)
  "Return a human-readable string for profiling REPORT."
  (let ((lines (list (format "Trace: %s" (or (plist-get report :file) "<none>"))
                     (format "Events: %d" (plist-get report :event-count))
                     ""
                     "Aggregates:")))
    (dolist (entry (plist-get report :aggregates))
      (let ((event (car entry))
            (data (cdr entry)))
        (push (format "- %s count=%d total=%.3fms avg=%.3fms max=%.3fms"
                      event
                      (plist-get data :count)
                      (plist-get data :total-ms)
                      (plist-get data :average-ms)
                      (plist-get data :max-ms))
              lines)))
    (push "" lines)
    (push "Slowest:" lines)
    (dolist (record (plist-get report :slowest))
      (push (format "- %s %.3fms session=%s turn=%s buffer=%s"
                    (alist-get 'event record)
                    (e-dev-profile--record-duration record)
                    (or (alist-get 'session-id record) "-")
                    (or (alist-get 'turn-id record) "-")
                    (or (alist-get 'buffer-name record) "-"))
            lines))
    (mapconcat #'identity (nreverse lines) "\n")))

;;;###autoload
(defun e-dev-profile-report (&optional file)
  "Open a profiling report buffer for FILE or the latest trace."
  (interactive)
  (let* ((report (e-dev-profile-report-data file))
         (content (e-dev-profile-format-report report)))
    (with-current-buffer (get-buffer-create "*e-dev-profile*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))
        (view-mode 1))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun e-dev-profile-open-latest ()
  "Open the latest developer profiling trace file."
  (interactive)
  (unless e-dev-profile--latest-file
    (user-error "No e profile trace has been recorded"))
  (find-file e-dev-profile--latest-file))

(provide 'e-dev-profile)

;;; e-dev-profile.el ends here
