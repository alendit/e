;;; e-base-tools.el --- Base filesystem and shell tools for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Concrete Pi-like base tools for workspace file and shell access.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-operations)
(require 'e-resource-coherence)
(require 'e-resource-patterns)
(require 'e-resources)
(require 'e-tools)

(defgroup e-base-tools nil
  "Base filesystem and shell tools for e."
  :group 'e)

(defcustom e-base-tools-shell-file-name nil
  "Shell executable used by the base bash tool.
When nil, `shell-file-name' is used."
  :type '(choice (const :tag "Use shell-file-name" nil)
                 file)
  :group 'e-base-tools)

(defcustom e-base-tools-shell-command-switch nil
  "Shell command switch used by the base bash tool.
When nil, `shell-command-switch' is used."
  :type '(choice (const :tag "Use shell-command-switch" nil)
                 string)
  :group 'e-base-tools)

(define-error 'e-base-tools-read-invalid "Base read tool input is invalid")
(define-error 'e-base-tools-path-outside-root
  "Base file resource path escapes the configured root")
(define-error 'e-base-tools-edit-invalid "Base edit tool input is invalid")
(define-error 'e-base-tools-coherence-conflict
  "Base file resource conflicts with a live Emacs buffer"
  'e-resource-coherence-conflict)
(define-error 'e-base-tools-bash-invalid "Base bash tool input is invalid")
(define-error 'e-base-tools-missing-command
  "Base file discovery command is not available")
(define-error 'e-base-tools-process-failed
  "Base file discovery command failed")

(defconst e-base-tools--max-lines 2000
  "Maximum text lines returned by base tools before truncation.")

(defconst e-base-tools--max-bytes (* 16 1024)
  "Maximum text bytes returned by base tools before truncation.")

(defun e-base-tools--argument-string (arguments key)
  "Return required string argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp key)))
    value))

(defun e-base-tools--optional-positive-number (arguments key)
  "Return optional positive numeric KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (when value
      (unless (and (numberp value) (> value 0))
        (signal 'wrong-type-argument (list 'positive-number-p key)))
      value)))

(defun e-base-tools--root-list (directory)
  "Return DIRECTORY as a normalized list of root directories.
DIRECTORY may be a single directory string or a list of them; the first is the
primary root used to resolve relative paths."
  (mapcar (lambda (root) (file-name-as-directory (expand-file-name root)))
          (if (listp directory) directory (list directory))))

(defun e-base-tools--resolve-path (path directory)
  "Resolve PATH against DIRECTORY.
DIRECTORY is the primary root or a list of workspace roots whose first element
is the primary root.  PATH resolves against the primary root, but is accepted
when it falls within any of the roots, so absolute paths into a secondary
workspace root are allowed."
  (let* ((roots (e-base-tools--root-list directory))
         (primary (car roots))
         (absolute-path (expand-file-name path primary)))
    (unless (cl-some (lambda (root) (file-in-directory-p absolute-path root))
                     roots)
      (signal 'e-base-tools-path-outside-root
              (list (format "Path escapes workspace root: %s" path))))
    absolute-path))

(defun e-base-tools--canonical-file-name (path)
  "Return a canonical comparison path for local PATH."
  (file-truename (expand-file-name path)))

(defun e-base-tools--buffer-visible-p (buffer)
  "Return non-nil when BUFFER is visible in a live window."
  (and (get-buffer-window buffer t) t))

(defun e-base-tools--buffer-selected-window-p (buffer)
  "Return non-nil when BUFFER is displayed in the selected window."
  (and (selected-window)
       (eq buffer (window-buffer (selected-window)))))

(defun e-base-tools--buffer-file-matches-p (buffer file)
  "Return non-nil when BUFFER visits FILE."
  (with-current-buffer buffer
    (and buffer-file-name
         (equal (e-base-tools--canonical-file-name buffer-file-name)
                (e-base-tools--canonical-file-name file)))))

(defun e-base-tools-file-live-buffers (file)
  "Return live Emacs buffers visiting local FILE."
  (let (buffers)
    (dolist (buffer (buffer-list))
      (when (e-base-tools--buffer-file-matches-p buffer file)
        (push buffer buffers)))
    (nreverse buffers)))

(defun e-base-tools--buffer-link-state (buffer)
  "Return linked-resource state for BUFFER."
  (with-current-buffer buffer
    (list :name (buffer-name buffer)
          :file buffer-file-name
          :modified (buffer-modified-p buffer)
          :visible (e-base-tools--buffer-visible-p buffer)
          :selected-window (e-base-tools--buffer-selected-window-p buffer))))

(defun e-base-tools-file-link-state (file)
  "Return linked live-buffer state for local FILE."
  (let ((group (e-base-tools-file-buffer-coherence-group file)))
    (list :canonical (plist-get group :canonical-uri)
          :file (plist-get (plist-get group :metadata) :file)
          :buffers (mapcar (lambda (view)
                             (copy-sequence (plist-get view :metadata)))
                           (e-resource-coherence-views-by-kind
                            group 'buffer)))))

(defun e-base-tools--file-uri (file)
  "Return canonical file URI for FILE."
  (concat "file://" (e-base-tools--canonical-file-name file)))

(defun e-base-tools--buffer-uri (buffer)
  "Return buffer URI for BUFFER."
  (concat "buffer://" (buffer-name buffer)))

(defun e-base-tools--buffer-from-view (view)
  "Return live buffer described by coherence VIEW, or nil."
  (when-let ((name (plist-get (plist-get view :metadata) :name)))
    (get-buffer name)))

(defun e-base-tools--file-buffer-view-status (file buffer disk-content disk-error)
  "Return coherence status for BUFFER visiting FILE."
  (cond
   ((with-current-buffer buffer (buffer-modified-p)) 'needs-save)
   (disk-error 'unknown)
   ((not (file-exists-p file)) 'missing)
   ((equal (e-base-tools--buffer-content buffer) disk-content) 'coherent)
   (t 'stale)))

(defun e-base-tools--file-disk-view (file disk-error)
  "Return a generic coherence view for disk FILE."
  (e-resource-coherence-view-create
   :uri (e-base-tools--file-uri file)
   :canonical-uri (e-base-tools--file-uri file)
   :label (e-base-tools--file-uri file)
   :kind 'file
   :role 'persisted
   :status (cond (disk-error 'unknown)
                 ((file-exists-p file) 'coherent)
                 (t 'missing))
   :modified nil
   :live nil
   :visible nil
   :selected-window nil
   :priority 0
   :metadata (list :file file :disk-error disk-error)))

(defun e-base-tools--file-buffer-view (file buffer disk-content disk-error)
  "Return a generic coherence view for BUFFER visiting FILE."
  (let ((metadata (e-base-tools--buffer-link-state buffer)))
    (e-resource-coherence-view-create
     :uri (e-base-tools--buffer-uri buffer)
     :canonical-uri (e-base-tools--file-uri file)
     :label (buffer-name buffer)
     :kind 'buffer
     :role 'live-view
     :status (e-base-tools--file-buffer-view-status
              file buffer disk-content disk-error)
     :modified (plist-get metadata :modified)
     :live t
     :visible (plist-get metadata :visible)
     :selected-window (plist-get metadata :selected-window)
     :priority 100
     :metadata metadata)))

(defun e-base-tools-file-buffer-coherence-group (file &optional subject-uri)
  "Return a generic coherence group for local FILE and its live buffer views."
  (let* ((absolute-file (e-base-tools--canonical-file-name file))
         (file-uri (e-base-tools--file-uri absolute-file))
         (buffers (e-base-tools-file-live-buffers absolute-file))
         disk-content
         disk-error)
    (condition-case err
        (when (file-exists-p absolute-file)
          (setq disk-content (e-base-tools--file-disk-text absolute-file)))
      (error
       (setq disk-error (error-message-string err))))
    (e-resource-coherence-group-with-status
     (e-resource-coherence-group-create
      :canonical-uri file-uri
      :subject-uri (or subject-uri file-uri)
      :views (cons (e-base-tools--file-disk-view absolute-file disk-error)
                   (mapcar (lambda (buffer)
                             (e-base-tools--file-buffer-view
                              absolute-file buffer disk-content disk-error))
                           buffers))
      :metadata (list :file absolute-file :disk-error disk-error)))))

(defun e-base-tools--buffer-only-coherence-group (buffer)
  "Return a generic coherence group for non-file-backed BUFFER."
  (let ((uri (e-base-tools--buffer-uri buffer)))
    (e-resource-coherence-group-with-status
     (e-resource-coherence-group-create
      :canonical-uri uri
      :subject-uri uri
      :views (list
              (e-resource-coherence-view-create
               :uri uri
               :canonical-uri uri
               :label (buffer-name buffer)
               :kind 'buffer
               :role 'live-view
               :status 'coherent
               :modified (with-current-buffer buffer (buffer-modified-p))
               :live t
               :visible (e-base-tools--buffer-visible-p buffer)
               :selected-window (e-base-tools--buffer-selected-window-p buffer)
               :priority 100
               :metadata (e-base-tools--buffer-link-state buffer)))
      :metadata (list :buffer (buffer-name buffer))))))

(defun e-base-tools-file-buffer-coherence-provider (directory)
  "Return a generic coherence provider for files rooted at DIRECTORY and buffers."
  (e-resource-coherence-provider-create
   :id 'local-file-backed-buffers
   :schemes '("file" "buffer")
   :handler
   (lambda (uri)
     (pcase (plist-get uri :scheme)
       ("file"
        (let ((file (e-base-tools--resource-path uri directory)))
          (e-base-tools-file-buffer-coherence-group
           file
           (plist-get uri :uri))))
       ("buffer"
        (when-let ((buffer (get-buffer (plist-get uri :address))))
          (with-current-buffer buffer
            (if buffer-file-name
                (e-base-tools-file-buffer-coherence-group
                 buffer-file-name
                 (plist-get uri :uri))
              (e-base-tools--buffer-only-coherence-group buffer)))))))))

(defun e-base-tools--sync-other-live-buffers (written-buffer buffers)
  "Revert unmodified BUFFERS other than WRITTEN-BUFFER after a file save."
  (let (synced)
    (dolist (buffer buffers)
      (when (and (buffer-live-p buffer)
                 (not (eq buffer written-buffer)))
        (with-current-buffer buffer
          (unless (buffer-modified-p)
            (revert-buffer :ignore-auto :noconfirm)
            (push (e-base-tools--buffer-link-state buffer) synced)))))
    (nreverse synced)))

(defun e-base-tools--save-buffer-content-to-file (buffer content)
  "Replace BUFFER contents with CONTENT and save its visited file.
Saving is non-interactive: the file is written as UTF-8 without prompting to
choose a coding system, which `save-buffer' would otherwise do when the
buffer's detected coding cannot encode CONTENT."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (require-final-newline nil)
          (mode-require-final-newline nil)
          (coding-system-for-write 'utf-8-unix)
          (select-safe-coding-system-function nil))
      (let ((source (generate-new-buffer " *e-base-tools-content*" t)))
        (unwind-protect
            (progn
              (with-current-buffer source (insert content))
              ;; Diff-based replacement preserves overlays/markers anchored to
              ;; surviving text; a plain erase+insert slides every overlay to
              ;; position 1.  Minor modes that persist overlay regions on
              ;; `before-save-hook' (e.g. Simply Annotate serializes annotation
              ;; threads from their overlays) would otherwise write collapsed
              ;; (1 . 1) regions to disk.  `replace-buffer-contents' only falls
              ;; back to delete+insert past its time budget, never worse than
              ;; the prior behavior.
              (replace-buffer-contents source))
          (kill-buffer source)))
      (save-buffer)
      (e-base-tools--buffer-link-state buffer))))

(defmacro e-base-tools--with-utf8-write (&rest body)
  "Run BODY writing files as UTF-8 without a coding-system prompt.
Bash output and tmp-resource content may contain eight-bit bytes that the
buffer's detected coding cannot encode; without these bindings `write-region'
would invoke `select-safe-coding-system' and block on the interactive
coding-system picker.  `utf-8-unix' round-trips eight-bit chars as raw bytes,
and disabling the selector keeps writes non-interactive."
  (declare (indent 0) (debug t))
  `(let ((coding-system-for-write 'utf-8-unix)
         (select-safe-coding-system-function nil))
     ,@body))

(defun e-base-tools--read-file-literally (path)
  "Return literal contents of PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun e-base-tools--decode-text (content)
  "Decode literal file CONTENT as UTF-8 text."
  (decode-coding-string content 'utf-8-unix t))

(defun e-base-tools--binary-string-p (content)
  "Return non-nil when CONTENT appears binary."
  (or (string-search "\0" content)
      (and (> (length content) 1)
           (let ((first (aref content 0))
                 (second (aref content 1)))
             (or (and (= first #x89) (= second ?P))
                 (and (= first #xff) (= second #xd8)))))))

(defun e-base-tools--lines (content &optional drop-final-empty)
  "Return CONTENT split into lines.
When DROP-FINAL-EMPTY is non-nil, ignore a final empty line produced by a
trailing newline."
  (let ((lines (split-string content "\n")))
    (if (and drop-final-empty
             (string-suffix-p "\n" content)
             lines
             (equal (car (last lines)) ""))
        (butlast lines)
      lines)))

(defun e-base-tools--join-lines-preserving-terminal-newline
    (lines original-content)
  "Join LINES and preserve ORIGINAL-CONTENT's terminal newline when possible."
  (ignore original-content)
  (mapconcat #'identity lines "\n"))

(defun e-base-tools--truncate-head-lines (content total-lines start-line)
  "Return CONTENT truncated from the head, with metadata.
TOTAL-LINES is the full file line count.  START-LINE is 1-based."
  (let* ((lines (e-base-tools--lines content))
         (selected-count (length lines))
         (line-truncated (> selected-count e-base-tools--max-lines))
         (head-lines (if line-truncated
                         (seq-take lines e-base-tools--max-lines)
                       lines))
         (head-content
          (e-base-tools--join-lines-preserving-terminal-newline
           head-lines content))
         (byte-truncated nil))
    (when (> (string-bytes head-content) e-base-tools--max-bytes)
      (setq byte-truncated t)
      (setq head-content
            (decode-coding-string
             (seq-take (encode-coding-string head-content 'utf-8)
                       e-base-tools--max-bytes)
             'utf-8 t))
      (setq head-lines (e-base-tools--lines head-content t)))
    (let* ((output-lines (max 1 (length (e-base-tools--lines head-content t))))
           (end-line (+ start-line output-lines -1)))
      (list :content head-content
            :truncated (or line-truncated byte-truncated)
            :truncated-by (cond (line-truncated 'lines)
                                (byte-truncated 'bytes))
            :output-lines output-lines
            :end-line end-line
            :next-offset (1+ end-line)
            :total-lines total-lines))))

(defun e-base-tools--truncate-tail-lines (content)
  "Return CONTENT truncated from the tail, with metadata."
  (let* ((lines (e-base-tools--lines content t))
         (total-lines (length lines))
         (line-truncated (> total-lines e-base-tools--max-lines))
         (tail-lines (if line-truncated
                         (last lines e-base-tools--max-lines)
                       lines))
         (tail-content (concat (mapconcat #'identity tail-lines "\n")
                               (if (string-suffix-p "\n" content) "\n" "")))
         (byte-truncated nil))
    (when (> (string-bytes tail-content) e-base-tools--max-bytes)
      (setq byte-truncated t)
      (setq tail-content
            (decode-coding-string
             (apply #'unibyte-string
                    (last (append (encode-coding-string tail-content 'utf-8) nil)
                          e-base-tools--max-bytes))
             'utf-8 t))
      (setq tail-lines (e-base-tools--lines tail-content t)))
    (let* ((output-lines (length tail-lines))
           (start-line (max 1 (- total-lines output-lines -1))))
      (list :content tail-content
            :truncated (or line-truncated byte-truncated)
            :truncated-by (cond (line-truncated 'lines)
                                (byte-truncated 'bytes))
            :output-lines output-lines
            :start-line start-line
            :end-line total-lines
            :total-lines total-lines))))

(defun e-base-tools--file-disk-text (path)
  "Return text contents of disk PATH or signal a clear read error."
  (unless (file-readable-p path)
    (signal 'e-base-tools-read-invalid
            (list (format "File is not readable: %s" path))))
  (unless (file-regular-p path)
    (signal 'e-base-tools-read-invalid
            (list (format "Path is not a regular file: %s" path))))
  (let ((content (e-base-tools--read-file-literally path)))
    (when (e-base-tools--binary-string-p content)
      (signal 'e-base-tools-read-invalid
              (list "The read tool is text-only in v1; binary and image files are not supported.")))
    (e-base-tools--decode-text content)))

(defun e-base-tools--preferred-buffer-for-group (group)
  "Return preferred live buffer view from generic coherence GROUP, or nil."
  (when-let ((view (e-resource-coherence-preferred-view
                   (e-resource-coherence-views-by-kind group 'buffer)
                   "live buffer view")))
    (e-base-tools--buffer-from-view view)))

(defun e-base-tools--signal-base-coherence-conflict (err)
  "Signal ERR as a base-tools coherence conflict for compatibility."
  (signal 'user-error (cdr err)))

(defun e-base-tools--stale-buffer-views (group &optional except-uri)
  "Return stale buffer views in GROUP, optionally excluding EXCEPT-URI."
  (seq-filter
   (lambda (view)
     (and (eq (plist-get view :kind) 'buffer)
          (eq (plist-get view :status) 'stale)
          (not (equal (e-resource-coherence-view-uri view)
                      except-uri))))
   (e-resource-coherence-group-views group)))

(defun e-base-tools--check-coherence-write-conflicts
    (group subject-uri action)
  "Signal if writing SUBJECT-URI with ACTION conflicts in GROUP."
  (condition-case err
      (e-resource-coherence-conflict-if-dirty group subject-uri action)
    (e-resource-coherence-conflict
     (e-base-tools--signal-base-coherence-conflict err)))
  (when-let ((stale (e-base-tools--stale-buffer-views group subject-uri)))
    (signal
     'user-error
     (list
      (format
       "Cannot %s %s directly because linked buffer(s) %s are stale. Reload or sync the stale buffer before mutating the file."
       (or action "edit")
       (or subject-uri
           (plist-get group :subject-uri)
           (plist-get group :canonical-uri)
           "resource")
       (e-resource-coherence-view-labels stale))))))

(defun e-base-tools--file-text (path)
  "Return coherent text contents of PATH.
Live resource views visiting PATH win over disk so unsaved edits are visible
through file:// reads."
  (let* ((absolute-path (e-base-tools--canonical-file-name path))
         (group (e-base-tools-file-buffer-coherence-group absolute-path)))
    (if-let ((buffer (e-base-tools--preferred-buffer-for-group group)))
        (with-current-buffer buffer
          (buffer-substring-no-properties (point-min) (point-max)))
      (e-base-tools--file-disk-text absolute-path))))

(defun e-base-tools--read-text (path offset limit)
  "Read text PATH with 1-based OFFSET and optional LIMIT."
  (let* ((content (e-base-tools--file-text path))
         (lines (e-base-tools--lines content))
         (total-lines (length lines))
         (start-line (or offset 1))
         (start-index (1- start-line)))
    (when (>= start-index total-lines)
      (signal 'e-base-tools-read-invalid
              (list (format "Offset %s is beyond end of file (%s lines total)"
                            start-line total-lines))))
    (let* ((remaining (nthcdr start-index lines))
           (selected-lines (if limit
                               (seq-take remaining limit)
                             remaining))
           (selected-content
            (e-base-tools--join-lines-preserving-terminal-newline
             selected-lines content))
           (limited-end (+ start-index (length selected-lines)))
           (more-after-limit (and limit (< limited-end total-lines))))
      (if limit
          (if more-after-limit
              (format "%s\n\n[%d more lines in file. Use offset=%d to continue.]"
                      selected-content
                      (- total-lines limited-end)
                      (1+ limited-end))
            selected-content)
        (let ((truncation
               (e-base-tools--truncate-head-lines
                selected-content total-lines start-line)))
          (if (plist-get truncation :truncated)
              (format
               "%s\n\n[Showing lines %d-%d of %d. Use offset=%d to continue.]"
               (plist-get truncation :content)
               start-line
               (plist-get truncation :end-line)
               total-lines
               (plist-get truncation :next-offset))
            (plist-get truncation :content)))))))

(defun e-base-tools--range-number (range key)
  "Return optional positive numeric KEY from RANGE."
  (let ((value (plist-get range key)))
    (when value
      (unless (and (numberp value) (> value 0))
        (signal 'wrong-type-argument (list 'positive-number-p key)))
      value)))

(defun e-base-tools--range-offset-limit (range)
  "Return line offset and limit for structured RANGE."
  (if (null range)
      (list nil nil)
    (let ((unit (plist-get range :unit))
          (start (e-base-tools--range-number range :start)))
      (unless start
        (signal 'e-base-tools-read-invalid
                '("range.start must be a positive number")))
      (pcase unit
        ("line"
         (let ((end (e-base-tools--range-number range :end)))
           (when (and end (< end start))
             (signal 'e-base-tools-read-invalid
                     '("range.end must be greater than or equal to range.start")))
           (list start (and end (1+ (- end start))))))
        ("offset"
         (list start (e-base-tools--range-number range :limit)))
        (_
         (signal 'e-base-tools-read-invalid
                 (list (format "Unsupported file range unit: %s" unit))))))))

(defun e-base-tools--resource-path (uri directory)
  "Resolve parsed file URI against DIRECTORY."
  (e-base-tools--resolve-path (plist-get uri :address) directory))

(defun e-base-tools--primary-root (directory)
  "Return primary root from DIRECTORY."
  (car (e-base-tools--root-list directory)))

(defun e-base-tools--relative-to-root (path directory)
  "Return PATH relative to DIRECTORY primary root."
  (file-relative-name path (e-base-tools--primary-root directory)))

(defun e-base-tools--file-resource-uri (path directory)
  "Return file:// URI for PATH relative to DIRECTORY primary root."
  (concat "file://" (e-base-tools--relative-to-root path directory)))

(defun e-base-tools--clean-relative-path (path)
  "Return PATH without fd's defensive leading ./ prefix."
  (if (string-prefix-p "./" path)
      (substring path 2)
    path))

(defun e-base-tools--file-discovery-limit (limit)
  "Return normalized discovery LIMIT."
  (cond
   ((null limit) 100)
   ((and (numberp limit) (> limit 0)) (truncate limit))
   (t (signal 'wrong-type-argument (list 'positive-number-p limit)))))

(defun e-base-tools--find-executable (name &optional alternates)
  "Return executable NAME or one of ALTERNATES, or signal a clear error."
  (or (executable-find name)
      (cl-some #'executable-find alternates)
      (signal 'e-base-tools-missing-command
              (list (format "Missing executable: %s" name)))))

(defun e-base-tools--process-lines (program directory args &optional ok-statuses)
  "Run PROGRAM in DIRECTORY with ARGS and return output lines.
OK-STATUSES defaults to only zero."
  (let ((default-directory directory)
        (accepted (or ok-statuses '(0))))
    (with-temp-buffer
      (let ((status (apply #'process-file program nil (list t t) nil args)))
        (unless (member status accepted)
          (signal 'e-base-tools-process-failed
                  (list (format "%s failed with exit status %s: %s"
                                program
                                status
                                (string-trim (buffer-string)))))))
      (split-string (buffer-string) "\n" t))))

(defun e-base-tools--file-scope-relative-path (uri directory)
  "Return file URI scope as a path relative to DIRECTORY primary root."
  (let ((relative (e-base-tools--relative-to-root
                   (e-base-tools--resource-path uri directory)
                   directory)))
    (if (string= relative ".") "." relative)))

(defun e-base-tools--file-result-name (path scope)
  "Return display name for PATH relative to SCOPE when possible."
  (if (and (file-directory-p scope)
           (file-in-directory-p path scope))
      (file-relative-name path scope)
    (file-name-nondirectory path)))

(defun e-base-tools--file-glob-single-result
    (scope scope-relative pattern case-sensitive)
  "Return a single file glob result for SCOPE, or nil if it does not match."
  (when (and (file-regular-p scope)
             (e-resource-pattern-glob-match-p
              pattern
              (file-name-nondirectory scope-relative)
              case-sensitive))
    (let ((attributes (file-attributes scope)))
      (list :uri (concat "file://" scope-relative)
            :name (file-name-nondirectory scope)
            :kind 'file
            :metadata (list :bytes (file-attribute-size attributes))))))

(defun e-base-tools--file-glob-resource
    (uri pattern limit case-sensitive directory)
  "List file resources under parsed URI with PATTERN and LIMIT."
  (let* ((primary (e-base-tools--primary-root directory))
         (scope (e-base-tools--resource-path uri directory))
         (scope-relative (e-base-tools--file-scope-relative-path uri directory))
         (actual-pattern (or pattern "*"))
         (fd-pattern (e-resource-pattern-glob-fd-pattern actual-pattern))
         (fd-max-depth (e-resource-pattern-glob-max-depth actual-pattern))
         (actual-limit (e-base-tools--file-discovery-limit limit))
         (actual-case-sensitive (if (null case-sensitive) t case-sensitive)))
    (e-resource-pattern-compile-glob actual-pattern)
    (if (file-regular-p scope)
        (list :resources
              (if-let ((single (e-base-tools--file-glob-single-result
                                scope
                                scope-relative
                                actual-pattern
                                actual-case-sensitive)))
                  (vector single)
                [])
              :truncated nil)
      (let* ((lines (e-base-tools--process-lines
                     (e-base-tools--find-executable "fd" '("fdfind"))
                     primary
                     (append
                      (list "--glob"
                            "--color" "never"
                            "--base-directory" primary
                            "--search-path" scope-relative
                            "--type" "file"
                            "--max-results" (number-to-string (1+ actual-limit)))
                      (when fd-max-depth
                        (list "--max-depth" (number-to-string fd-max-depth)))
                      (list (if actual-case-sensitive
                                "--case-sensitive"
                              "--ignore-case"))
                      (list fd-pattern))))
             (filtered
              (seq-filter
               (lambda (relative)
                 (let* ((relative (e-base-tools--clean-relative-path relative))
                        (absolute (expand-file-name relative primary))
                        (name (e-base-tools--file-result-name absolute scope)))
                   (e-resource-pattern-glob-match-p
                    actual-pattern
                    name
                    actual-case-sensitive)))
               lines))
             (truncated (> (length filtered) actual-limit))
             (selected (seq-take filtered actual-limit)))
        (list :resources
              (vconcat
               (mapcar
                (lambda (relative)
                  (let* ((relative (e-base-tools--clean-relative-path relative))
                         (absolute (expand-file-name relative primary))
                         (attributes (file-attributes absolute)))
                    (list :uri (concat "file://" relative)
                          :name (e-base-tools--file-result-name absolute scope)
                          :kind 'file
                          :metadata (list :bytes (file-attribute-size attributes)))))
                selected))
              :truncated truncated)))))

(defun e-base-tools--rg-json-text (object)
  "Return text value from rg JSON OBJECT."
  (or (plist-get object :text)
      (when-let ((bytes (plist-get object :bytes)))
        (base64-decode-string bytes))))

(defun e-base-tools--search-match-from-rg-json
    (line directory scope glob-pattern)
  "Return a search match plist for rg JSON LINE, or nil."
  (let* ((object (json-parse-string line :object-type 'plist :array-type 'list))
         (type (plist-get object :type)))
    (when (equal type "match")
      (let* ((data (plist-get object :data))
             (path (e-base-tools--rg-json-text (plist-get data :path)))
             (line-text (string-remove-suffix
                         "\n"
                         (or (e-base-tools--rg-json-text
                              (plist-get data :lines))
                             "")))
             (submatches (plist-get data :submatches))
             (first-match (car submatches))
             (start (or (plist-get first-match :start) 0))
             (absolute (expand-file-name path (e-base-tools--primary-root directory))))
        (when (or (null glob-pattern)
                  (e-resource-pattern-glob-match-p
                   glob-pattern
                   (e-base-tools--file-result-name absolute scope)
                   t))
          (list :uri (e-base-tools--file-resource-uri absolute directory)
                :line (plist-get data :line_number)
                :column (1+ start)
                :text line-text))))))

(defun e-base-tools--file-search-resource (uri query options directory)
  "Search file resources under parsed URI for QUERY with OPTIONS."
  (let* ((primary (e-base-tools--primary-root directory))
         (scope (e-base-tools--resource-path uri directory))
         (scope-relative (e-base-tools--file-scope-relative-path uri directory))
         (glob-pattern (plist-get options :glob))
         (query-regexp (e-resource-pattern-search-rg-regexp query options))
         (actual-limit (e-base-tools--file-discovery-limit
                        (plist-get options :limit)))
         (args (append
                (list "--json"
                      "--line-number"
                      "--column"
                      "--color" "never")
                (unless (plist-get options :case-sensitive)
                  (list "--ignore-case"))
                (when (plist-get options :multiline)
                  (list "--multiline"))
                (list "-e" query-regexp scope-relative))))
    (when glob-pattern
      (e-resource-pattern-compile-glob glob-pattern))
    (let ((lines (e-base-tools--process-lines
                  (e-base-tools--find-executable "rg")
                  primary
                  args
                  '(0 1)))
          matches)
      (dolist (line lines)
        (when-let ((match (e-base-tools--search-match-from-rg-json
                           line
                           directory
                           scope
                           glob-pattern)))
          (push match matches)))
      (setq matches (nreverse matches))
      (list :matches (vconcat (seq-take matches actual-limit))
            :truncated (> (length matches) actual-limit)))))

(defun e-base-tools--write-file-resource (uri content directory)
  "Write CONTENT to parsed file URI in DIRECTORY."
  (let* ((path (plist-get uri :address))
         (absolute-path (e-base-tools--resource-path uri directory))
         (file-uri (e-base-tools--file-uri absolute-path))
         (group (e-base-tools-file-buffer-coherence-group
                 absolute-path
                 (plist-get uri :uri))))
    (e-base-tools--check-coherence-write-conflicts group file-uri "edit")
    (make-directory (file-name-directory absolute-path) t)
    (if-let ((buffer (e-base-tools--preferred-buffer-for-group group)))
        (let* ((saved (e-base-tools--save-buffer-content-to-file buffer content))
               (synced (e-base-tools--sync-other-live-buffers
                        buffer
                        (e-base-tools-file-live-buffers absolute-path))))
          (format "Successfully wrote %d bytes to %s through live buffer %s. Saved buffer and synced %d linked buffer(s)."
                  (string-bytes content)
                  path
                  (plist-get saved :name)
                  (length synced)))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region content nil absolute-path nil 'silent))
      (format "Successfully wrote %d bytes to %s"
              (string-bytes content)
              path))))

(defun e-base-tools--edit-file-resource (uri edits directory)
  "Apply exact EDITS to parsed file URI in DIRECTORY."
  (let* ((path (plist-get uri :address))
         (absolute-path (e-base-tools--resource-path uri directory))
         (file-uri (e-base-tools--file-uri absolute-path))
         (group (e-base-tools-file-buffer-coherence-group
                 absolute-path
                 (plist-get uri :uri))))
    (e-base-tools--check-coherence-write-conflicts group file-uri "edit")
    (let* ((buffer (e-base-tools--preferred-buffer-for-group group))
           (raw-content (e-base-tools--file-text absolute-path))
           (line-ending (e-base-tools--line-ending raw-content))
           (content (e-base-tools--normalize-line-endings raw-content))
           (normalized-edits (e-base-tools--normalize-edits edits))
           (new-content (e-base-tools--apply-edits content normalized-edits path))
           (final-content
            (e-base-tools--restore-line-endings new-content line-ending))
           saved
           synced)
      (if buffer
          (progn
            (make-directory (file-name-directory absolute-path) t)
            (setq saved (e-base-tools--save-buffer-content-to-file
                         buffer final-content))
            (setq synced (e-base-tools--sync-other-live-buffers
                          buffer
                          (e-base-tools-file-live-buffers absolute-path))))
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region final-content nil absolute-path nil 'silent)))
      (list :message (if buffer
                         (format "Successfully replaced %d block(s) in %s through live buffer %s. Saved buffer and synced %d linked buffer(s)."
                                 (length normalized-edits)
                                 path
                                 (plist-get saved :name)
                                 (length synced))
                       (format "Successfully replaced %d block(s) in %s."
                               (length normalized-edits)
                               path))
            :replacements (length normalized-edits)
            :linked-buffers (when buffer
                              (list :saved saved :synced synced))
            :diff (e-base-tools--simple-diff content new-content)))))

(defun e-base-tools--buffer-content (buffer)
  "Return BUFFER contents without text properties."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun e-base-tools--sync-status-group->result (group)
  "Return model-facing sync status RESULT for generic coherence GROUP."
  (let ((group (e-resource-coherence-group-with-status group)))
    (append
     (list :uri (plist-get group :subject-uri)
           :canonical-uri (plist-get group :canonical-uri)
           :status (plist-get group :status)
           :views (e-resource-coherence-group-views group))
     (when-let ((file (plist-get (plist-get group :metadata) :file)))
       (list :file file
             :disk-exists (file-exists-p file)
             :disk-error (plist-get (plist-get group :metadata) :disk-error)
             :buffers (mapcar (lambda (view)
                                (append (copy-sequence
                                         (plist-get view :metadata))
                                        (list :status
                                              (plist-get view :status))))
                              (e-resource-coherence-views-by-kind
                               group 'buffer)))))))

(defun e-base-tools--sync-status-for-uri (uri directory)
  "Return generic linked-resource coherence status for parsed URI in DIRECTORY."
  (let ((registry (e-resource-coherence-registry-create)))
    (e-resource-coherence-register
     registry
     (e-base-tools-file-buffer-coherence-provider directory))
    (e-base-tools--sync-status-group->result
     (e-resource-coherence-group registry (plist-get uri :uri)))))

(defun e-base-tools-register-resource-sync-status (registry directory)
  "Register a linked-resource coherence status tool in REGISTRY."
  (e-tools-register
   registry
   :name "resource_sync_status"
   :description "Report file-backed Emacs buffer coherence for a file:// or buffer:// URI."
   :parameters '(:type "object"
                 :properties (:uri (:type "string"))
                 :required ["uri"])
   :handler
   (lambda (arguments)
     (let* ((uri-text (e-base-tools--argument-string arguments :uri))
            (uri (e-resources-parse-uri uri-text)))
       (e-base-tools--sync-status-for-uri uri directory)))))

(defun e-base-tools--file-read-method (directory)
  "Return a file read resource method rooted at DIRECTORY."
  (e-resource-method-create
   :scheme "file"
   :operation e-operation-read
   :description "Workspace text files."
   :uri-patterns '("file://<path>")
   :range-modes '("line" "offset")
   :handler (lambda (uri range)
              (pcase-let ((`(,offset ,limit)
                           (e-base-tools--range-offset-limit range)))
                (e-base-tools--read-text
                 (e-base-tools--resource-path uri directory)
                 offset
                 limit)))))

(defun e-base-tools--file-write-method (directory)
  "Return a file write resource method rooted at DIRECTORY."
  (e-resource-method-create
   :scheme "file"
   :operation e-operation-write
   :description "Workspace text files."
   :uri-patterns '("file://<path>")
   :handler (lambda (uri content)
              (e-base-tools--write-file-resource uri content directory))))

(defun e-base-tools--file-edit-method (directory)
  "Return a file edit resource method rooted at DIRECTORY."
  (e-resource-method-create
   :scheme "file"
   :operation e-operation-edit
   :description "Workspace text files. Preserves CRLF line endings when possible."
   :uri-patterns '("file://<path>")
   :handler (lambda (uri edits)
              (e-base-tools--edit-file-resource uri edits directory))))

(defun e-base-tools--file-glob-method (directory)
  "Return a file glob resource method rooted at DIRECTORY."
  (e-resource-method-create
   :scheme "file"
   :operation e-operation-glob
   :description "Workspace text files and directories."
   :uri-patterns '("file://<path-or-directory>")
   :handler (lambda (uri pattern limit case-sensitive)
              (e-base-tools--file-glob-resource
               uri pattern limit case-sensitive directory))))

(defun e-base-tools--file-search-method (directory)
  "Return a file search resource method rooted at DIRECTORY."
  (e-resource-method-create
   :scheme "file"
   :operation e-operation-search
   :description "Workspace text file content."
   :uri-patterns '("file://<path-or-directory>")
   :handler (lambda (uri query options)
              (e-base-tools--file-search-resource
               uri query options directory))))

(defun e-base-tools-register-file-read-resource (registry directory)
  "Register read-only file resource methods in REGISTRY rooted at DIRECTORY."
  (dolist (method (list (e-base-tools--file-read-method directory)
                        (e-base-tools--file-glob-method directory)
                        (e-base-tools--file-search-method directory)))
    (e-resources-register registry method)))

(defun e-base-tools-register-file-resource (registry directory)
  "Register file resource methods in REGISTRY rooted at DIRECTORY."
  (dolist (method (list (e-base-tools--file-read-method directory)
                        (e-base-tools--file-write-method directory)
                        (e-base-tools--file-edit-method directory)
                        (e-base-tools--file-glob-method directory)
                        (e-base-tools--file-search-method directory)))
    (e-resources-register registry method)))

(defun e-base-tools--line-ending (content)
  "Return the dominant line ending in CONTENT."
  (if (string-match-p "\r\n" content) "\r\n" "\n"))

(defun e-base-tools--normalize-line-endings (content)
  "Return CONTENT with CRLF line endings normalized to LF."
  (replace-regexp-in-string "\r\n" "\n" content t t))

(defun e-base-tools--restore-line-endings (content line-ending)
  "Restore CONTENT to LINE-ENDING."
  (if (equal line-ending "\r\n")
      (replace-regexp-in-string "\n" "\r\n" content t t)
    content))

(defun e-base-tools--count-occurrences (content text)
  "Return number of exact TEXT occurrences in CONTENT."
  (let ((count 0)
        (start 0))
    (while (and (< start (length content))
                (string-match (regexp-quote text) content start))
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun e-base-tools--edit-field (edit key)
  "Return string KEY from EDIT."
  (let ((value (plist-get edit key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp key)))
    value))

(defun e-base-tools--normalize-edits (edits)
  "Return normalized EDITS for exact replacement."
  (unless (and (listp edits) edits)
    (signal 'e-base-tools-edit-invalid
            '("edits must contain at least one replacement")))
  (cl-loop for edit in edits
           collect (let ((old-text (e-base-tools--edit-field edit :oldText))
                         (new-text (e-base-tools--edit-field edit :newText)))
                     (list :old-text (e-base-tools--normalize-line-endings old-text)
                           :new-text (e-base-tools--normalize-line-endings new-text)))))

(defun e-base-tools--apply-edits (content edits path)
  "Apply exact EDITS to normalized CONTENT for PATH."
  (let ((matches nil)
        (index 0))
    (dolist (edit edits)
      (let ((old-text (plist-get edit :old-text)))
        (when (string-empty-p old-text)
          (signal 'e-base-tools-edit-invalid
                  (list (format "edits[%d].oldText must not be empty in %s."
                                index path))))
        (let ((occurrences (e-base-tools--count-occurrences content old-text)))
          (pcase occurrences
            (0 (signal 'e-base-tools-edit-invalid
                       (list (format "Could not find missing edits[%d] in %s. The oldText must match exactly including all whitespace and newlines."
                                     index path))))
            (1 (string-match (regexp-quote old-text) content)
               (push (list :edit-index index
                           :start (match-beginning 0)
                           :end (match-end 0)
                           :new-text (plist-get edit :new-text))
                     matches))
            (_ (signal 'e-base-tools-edit-invalid
                       (list (format "Found %d occurrences of edits[%d] in %s. Each oldText must be unique. Please provide more context to make it unique."
                                     occurrences index path)))))))
      (setq index (1+ index)))
    (setq matches (sort matches
                        (lambda (left right)
                          (< (plist-get left :start)
                             (plist-get right :start)))))
    (cl-loop for previous in matches
             for current in (cdr matches)
             when (> (plist-get previous :end) (plist-get current :start))
             do (signal 'e-base-tools-edit-invalid
                        (list (format "edits[%d] and edits[%d] overlap in %s. Merge them into one edit or target disjoint regions."
                                      (plist-get previous :edit-index)
                                      (plist-get current :edit-index)
                                      path))))
    (let ((new-content content))
      (dolist (match (reverse matches))
        (setq new-content
              (concat (substring new-content 0 (plist-get match :start))
                      (plist-get match :new-text)
                      (substring new-content (plist-get match :end)))))
      (when (equal content new-content)
        (signal 'e-base-tools-edit-invalid
                (list (format "No changes made to %s. The replacement produced identical content."
                              path))))
      new-content)))

(defun e-base-tools--simple-diff (old-content new-content)
  "Return a compact line diff between OLD-CONTENT and NEW-CONTENT."
  (let ((old-lines (e-base-tools--lines old-content t))
        (new-lines (e-base-tools--lines new-content t))
        (output nil))
    (while (or old-lines new-lines)
      (let ((old-line (car old-lines))
            (new-line (car new-lines)))
        (cond
         ((equal old-line new-line)
          (push (concat " " old-line) output)
          (setq old-lines (cdr old-lines))
          (setq new-lines (cdr new-lines)))
         (t
          (when old-line
            (push (concat "-" old-line) output)
            (setq old-lines (cdr old-lines)))
          (when new-line
            (push (concat "+" new-line) output)
            (setq new-lines (cdr new-lines)))))))
    (mapconcat #'identity (nreverse output) "\n")))

(defun e-base-tools--shell-command ()
  "Return command list prefix for the base bash tool."
  (list (or e-base-tools-shell-file-name shell-file-name "/bin/sh")
        (or e-base-tools-shell-command-switch shell-command-switch "-c")))

(cl-defstruct (e-base-tools--bash-collector
               (:constructor e-base-tools--bash-collector-create))
  output-file
  output-uri
  preview
  (preview-bytes 0)
  (preview-newlines 0)
  (preview-accepting t)
  truncated
  (total-bytes 0)
  (total-newlines 0)
  last-newline)

(defun e-base-tools--bash-max-bytes ()
  "Return the active bash output byte preview limit."
  (if (boundp 'e-tool-output-truncation-max-bytes)
      e-tool-output-truncation-max-bytes
    e-base-tools--max-bytes))

(defun e-base-tools--bash-max-lines ()
  "Return the active bash output line preview limit."
  (if (boundp 'e-tool-output-truncation-max-lines)
      e-tool-output-truncation-max-lines
    e-base-tools--max-lines))

(defun e-base-tools--logical-line-count (text)
  "Return the number of logical lines in TEXT."
  (cond
   ((string-empty-p text)
    0)
   ((string-suffix-p "\n" text)
    (cl-count ?\n text))
   (t
    (1+ (cl-count ?\n text)))))

(defun e-base-tools--safe-fragment (value fallback)
  "Return VALUE as a safe path fragment, or FALLBACK."
  (let* ((text (if (and (stringp value)
                        (not (string-empty-p value)))
                   value
                 fallback))
         (safe (replace-regexp-in-string "[^A-Za-z0-9._-]" "-" text)))
    (if (string-empty-p safe) fallback safe)))

(defun e-base-tools--bash-relative-name (context)
  "Return the session tmp relative output name for CONTEXT."
  (let ((turn-id (e-base-tools--safe-fragment
                  (plist-get context :turn-id)
                  "turn"))
        (call-id (e-base-tools--safe-fragment
                  (plist-get (plist-get context :tool-call) :id)
                  "call")))
    (format "tool-results/%s/bash-%s.txt" turn-id call-id)))

(defun e-base-tools--context-capability-active-p (context capability-id)
  "Return non-nil when CONTEXT includes CAPABILITY-ID."
  (cl-some (lambda (capability)
             (eq (e-capability-id capability) capability-id))
           (plist-get context :capabilities)))

(defun e-base-tools--bash-output-target (context)
  "Return plist describing where bash output should be streamed for CONTEXT."
  (let ((relative-name (e-base-tools--bash-relative-name context)))
    (if (and (plist-get context :harness)
             (plist-get context :session-id)
             (e-base-tools--context-capability-active-p
              context
              'session-tmp-resources)
             (require 'e-session-tmp-resources nil t)
             (fboundp 'e-session-tmp-file-path))
        (let ((path (e-session-tmp-file-path
                     (plist-get context :harness)
                     (plist-get context :session-id)
                     relative-name)))
          (list :output-file path
                :output-uri (format "tmp://%s" relative-name)))
      (list :output-file (make-temp-file "e-base-bash-" nil ".log")))))

(defun e-base-tools--bash-collector-start (context)
  "Return a streaming bash output collector for CONTEXT."
  (let* ((target (e-base-tools--bash-output-target context))
         (output-file (plist-get target :output-file)))
    (e-base-tools--with-utf8-write
      (write-region "" nil output-file nil 'silent))
    (e-base-tools--bash-collector-create
     :output-file output-file
     :output-uri (plist-get target :output-uri)
     :preview "")))

(defun e-base-tools--bash-collector-count-chunk (collector chunk)
  "Update COLLECTOR total counters for CHUNK."
  (setf (e-base-tools--bash-collector-total-bytes collector)
        (+ (e-base-tools--bash-collector-total-bytes collector)
           (string-bytes chunk)))
  (setf (e-base-tools--bash-collector-total-newlines collector)
        (+ (e-base-tools--bash-collector-total-newlines collector)
           (cl-count ?\n chunk)))
  (when (> (length chunk) 0)
    (setf (e-base-tools--bash-collector-last-newline collector)
          (eq (aref chunk (1- (length chunk))) ?\n))))

(defun e-base-tools--bash-collector-add-preview (collector chunk)
  "Add as much of CHUNK as allowed to COLLECTOR preview."
  (let ((max-bytes (max 0 (e-base-tools--bash-max-bytes)))
        (max-lines (max 0 (e-base-tools--bash-max-lines)))
        (index 0)
        (length (length chunk)))
    (when (or (zerop max-bytes) (zerop max-lines))
      (when (> length 0)
        (setf (e-base-tools--bash-collector-truncated collector) t)
        (setf (e-base-tools--bash-collector-preview-accepting collector) nil))
      (setq index length))
    (while (< index length)
      (if (not (e-base-tools--bash-collector-preview-accepting collector))
          (progn
            (setf (e-base-tools--bash-collector-truncated collector) t)
            (setq index length))
        (let* ((char (substring chunk index (1+ index)))
               (char-bytes (string-bytes char)))
          (if (> (+ (e-base-tools--bash-collector-preview-bytes collector)
                    char-bytes)
                 max-bytes)
              (progn
                (setf (e-base-tools--bash-collector-truncated collector) t)
                (setf (e-base-tools--bash-collector-preview-accepting collector)
                      nil)
                (setq index length))
            (setf (e-base-tools--bash-collector-preview collector)
                  (concat (e-base-tools--bash-collector-preview collector)
                          char))
            (setf (e-base-tools--bash-collector-preview-bytes collector)
                  (+ (e-base-tools--bash-collector-preview-bytes collector)
                     char-bytes))
            (when (equal char "\n")
              (setf (e-base-tools--bash-collector-preview-newlines collector)
                    (1+ (e-base-tools--bash-collector-preview-newlines collector)))
              (when (>= (e-base-tools--bash-collector-preview-newlines collector)
                        max-lines)
                (setf (e-base-tools--bash-collector-preview-accepting collector)
                      nil)))
            (setq index (1+ index))
            (when (and (< index length)
                       (not (e-base-tools--bash-collector-preview-accepting
                             collector)))
              (setf (e-base-tools--bash-collector-truncated collector) t)
              (setq index length))))))))

(defun e-base-tools--bash-collector-append (collector chunk)
  "Append CHUNK to COLLECTOR's output file and bounded preview."
  (when (> (length chunk) 0)
    (e-base-tools--with-utf8-write
      (write-region chunk nil
                    (e-base-tools--bash-collector-output-file collector)
                    'append
                    'silent))
    (e-base-tools--bash-collector-count-chunk collector chunk)
    (e-base-tools--bash-collector-add-preview collector chunk)))

(defun e-base-tools--bash-collector-original-lines (collector)
  "Return total logical line count for COLLECTOR."
  (if (zerop (e-base-tools--bash-collector-total-bytes collector))
      0
    (+ (e-base-tools--bash-collector-total-newlines collector)
       (if (e-base-tools--bash-collector-last-newline collector) 0 1))))

(defun e-base-tools--bash-collector-location (collector)
  "Return model-facing location for COLLECTOR full output."
  (or (e-base-tools--bash-collector-output-uri collector)
      (e-base-tools--bash-collector-output-file collector)))

(defun e-base-tools--bash-truncation-notice
    (shown-bytes shown-lines original-bytes original-lines location)
  "Return a model-facing truncation notice for bash output."
  (format "[Tool output truncated: showing first %d bytes / %d lines of %d bytes / %d lines. Full output: %s]"
          shown-bytes
          shown-lines
          original-bytes
          original-lines
          location))

(defun e-base-tools--bash-collector-metadata (collector)
  "Return truncation metadata for COLLECTOR."
  (let ((metadata (list :truncated t
                        :original-bytes
                        (e-base-tools--bash-collector-total-bytes collector)
                        :original-lines
                        (e-base-tools--bash-collector-original-lines collector)
                        :shown-bytes
                        (e-base-tools--bash-collector-preview-bytes collector)
                        :shown-lines
                        (e-base-tools--logical-line-count
                         (e-base-tools--bash-collector-preview collector)))))
    (if (e-base-tools--bash-collector-output-uri collector)
        (plist-put metadata
                   :tmp-uri
                   (e-base-tools--bash-collector-output-uri collector))
      (plist-put metadata
                 :full-output-path
                 (e-base-tools--bash-collector-output-file collector)))))

(defun e-base-tools--bash-collector-content (collector &optional suffix)
  "Return bounded model-facing content from COLLECTOR with optional SUFFIX."
  (let ((preview (e-base-tools--bash-collector-preview collector)))
    (if (not (e-base-tools--bash-collector-truncated collector))
        (if suffix
            (string-trim-right (format "%s\n\n%s" preview suffix))
          preview)
      (let* ((metadata (e-base-tools--bash-collector-metadata collector))
             (notice (e-base-tools--bash-truncation-notice
                      (plist-get metadata :shown-bytes)
                      (plist-get metadata :shown-lines)
                      (plist-get metadata :original-bytes)
                      (plist-get metadata :original-lines)
                      (e-base-tools--bash-collector-location collector)))
             (content (if (string-empty-p preview)
                          notice
                        (concat preview "\n\n" notice))))
        (if suffix
            (string-trim-right (format "%s\n\n%s" content suffix))
          content)))))

(defun e-base-tools--bash-finish-value
    (collector call status &optional suffix)
  "Return final bash result value from COLLECTOR for CALL and STATUS."
  (let ((content (e-base-tools--bash-collector-content collector suffix))
        (metadata (when (e-base-tools--bash-collector-truncated collector)
                    (e-base-tools--bash-collector-metadata collector))))
    (if call
        (e-tools-result-create call status content metadata)
      content)))

(cl-defun e-base-tools--run-shell-command-start
    (command directory timeout &key on-done on-error on-request-start)
  "Start shell COMMAND in DIRECTORY with optional TIMEOUT seconds.
ON-DONE receives captured output.  ON-ERROR receives an Emacs condition list.
ON-REQUEST-START receives the cancellable process request."
  (let* ((default-directory directory)
         (command-prefix (e-base-tools--shell-command))
         (shell-command (format "{\n%s\n} 2>&1" command))
         (tool-context (e-tools-current-context))
         (call (plist-get tool-context :tool-call))
         (collector (e-base-tools--bash-collector-start tool-context))
         (settled nil)
         process
         timeout-timer
         request)
    (cl-labels
        ((cleanup
          ()
          (when (timerp timeout-timer)
            (cancel-timer timeout-timer)))
         (finish
          (status &optional suffix)
          (unless settled
            (setq settled t)
            (let ((value (e-base-tools--bash-finish-value
                          collector
                          call
                          status
                          suffix)))
              (cleanup)
              (if (or (eq status 'ok) call)
                  (when on-done
                    (funcall on-done value))
                (when on-error
                  (funcall on-error
                           (list 'e-base-tools-bash-invalid value)))))))
         (cancel
          ()
          (unless settled
            (setq settled t)
            (when (timerp timeout-timer)
              (cancel-timer timeout-timer))
            (when (and process (process-live-p process))
              (kill-process process)))
          t))
      (setq process
            (make-process
             :name "e-base-bash"
             :command (append command-prefix (list shell-command))
             :connection-type 'pipe
             :coding 'utf-8-unix
             :noquery t
             :filter
             (lambda (_proc chunk)
               (e-base-tools--bash-collector-append collector chunk))
             :sentinel
             (lambda (proc _event)
               (when (and (not settled)
                          (memq (process-status proc) '(exit signal)))
                 (let ((exit-code (process-exit-status proc)))
                   (if (zerop exit-code)
                       (finish 'ok)
                     (finish
                      'error
                      (format "Command exited with code %d"
                              exit-code))))))))
      (set-process-query-on-exit-flag process nil)
      (setq request
            (e-tools-request-create
             :cancel #'cancel
             :metadata (list :transport 'process
                             :process process
                             :output-file
                             (e-base-tools--bash-collector-output-file collector)
                             :output-uri
                             (e-base-tools--bash-collector-output-uri collector)
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
                    (format "Command timed out after %s seconds"
                            timeout)))))))
      request)))

(defun e-base-tools--run-shell-command (command directory timeout)
  "Run shell COMMAND in DIRECTORY with optional TIMEOUT seconds."
  (let ((done nil)
        (result nil)
        (failure nil))
    (e-base-tools--run-shell-command-start
     command
     directory
     timeout
     :on-done (lambda (output)
                (setq result output)
                (setq done t))
     :on-error (lambda (err)
                 (setq failure err)
                 (setq done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(defun e-base-tools--truncate-bash-output (output)
  "Return bash OUTPUT, truncating and persisting full output when needed."
  (let ((truncation (e-base-tools--truncate-tail-lines output)))
    (if (not (plist-get truncation :truncated))
        output
      (let ((full-output-path (make-temp-file "e-base-bash-" nil ".log")))
        (e-base-tools--with-utf8-write
          (write-region output nil full-output-path nil 'silent))
        (format "%s\n\n[Showing lines %d-%d of %d. Full output: %s]"
                (plist-get truncation :content)
                (plist-get truncation :start-line)
                (plist-get truncation :end-line)
                (plist-get truncation :total-lines)
                full-output-path)))))

(defun e-base-tools-register-bash (registry directory)
  "Register the base bash tool in REGISTRY rooted at DIRECTORY."
  (e-tools-register
   registry
   :name "bash"
   :description "Execute a shell command in the current working directory and return captured stdout and stderr. Never search or traverse outside the current project: no `find /`, `find ~`, `find $HOME`, `grep -r ~`, `ls -R /`, or any recursive walk rooted at `/`, `~`, or the home directory. They are slow and flood output. Scope every search to the working directory or a known subtree, e.g. `find . -name ...` or `grep -rn PATTERN lisp/`."
   :parameters '(:type "object"
                 :properties (:command (:type "string")
                              :timeout (:type "number"
                                        :description "Hard timeout in seconds. When reached, e kills the process and returns a tool error. Use modest values for routine commands; long-running commands need an explicit control pattern.")
                              :resource_usage
                              (:type "object"
                               :description "Optional high-value resource usage for future context. Use only when the command reads, writes, or edits resources that matter for future work."
                               :properties (:resources
                                            (:type "array"
                                             :items
                                             (:type "object"
                                              :properties
                                              (:uri (:type "string")
                                               :operation
                                               (:type "string"
                                                :enum ["read" "write" "edit"]))
                                              :required ["uri" "operation"]))
                                            :summary
                                            (:type "string"
                                             :description "Compact summary of why these resources matter."))))
                 :required ["command"])
   :start
   (cl-function
    (lambda (&key arguments on-done on-error on-request-start
                  &allow-other-keys)
      (let ((command (e-base-tools--argument-string arguments :command))
            (timeout (e-base-tools--optional-positive-number arguments :timeout)))
        (e-base-tools--run-shell-command-start
         command
         directory
         timeout
         :on-request-start on-request-start
         :on-done on-done
         :on-error on-error))))))

(provide 'e-base-tools)

;;; e-base-tools.el ends here
