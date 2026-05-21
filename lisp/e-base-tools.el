;;; e-base-tools.el --- Base filesystem and shell tools for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Concrete Pi-like base tools for workspace file and shell access.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
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
(define-error 'e-base-tools-edit-invalid "Base edit tool input is invalid")
(define-error 'e-base-tools-bash-invalid "Base bash tool input is invalid")

(defconst e-base-tools--max-lines 2000
  "Maximum text lines returned by base tools before truncation.")

(defconst e-base-tools--max-bytes (* 50 1024)
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

(defun e-base-tools--resolve-path (path directory)
  "Resolve PATH against DIRECTORY."
  (expand-file-name path (file-name-as-directory directory)))

(defun e-base-tools--read-file-literally (path)
  "Return literal contents of PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

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

(defun e-base-tools--file-text (path)
  "Return text contents of PATH or signal a clear read error."
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
    content))

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

(defun e-base-tools-register-read (registry directory)
  "Register the base read tool in REGISTRY rooted at DIRECTORY."
  (e-tools-register
   registry
   :name "read"
   :description "Read text file contents by path, with optional 1-based offset and line limit."
   :parameters '(:type "object"
                 :properties (:path (:type "string")
                              :offset (:type "number")
                              :limit (:type "number"))
                 :required ["path"])
   :handler
   (lambda (arguments)
     (let ((path (e-base-tools--argument-string arguments :path))
           (offset (e-base-tools--optional-positive-number arguments :offset))
           (limit (e-base-tools--optional-positive-number arguments :limit)))
       (e-base-tools--read-text
        (e-base-tools--resolve-path path directory)
        offset
        limit)))))

(defun e-base-tools-register-write (registry directory)
  "Register the base write tool in REGISTRY rooted at DIRECTORY."
  (e-tools-register
   registry
   :name "write"
   :description "Write content to a file, creating parent directories and overwriting existing content."
   :parameters '(:type "object"
                 :properties (:path (:type "string")
                              :content (:type "string"))
                 :required ["path" "content"])
   :handler
   (lambda (arguments)
     (let* ((path (e-base-tools--argument-string arguments :path))
            (content (e-base-tools--argument-string arguments :content))
            (absolute-path (e-base-tools--resolve-path path directory)))
       (make-directory (file-name-directory absolute-path) t)
       (write-region content nil absolute-path nil 'silent)
       (format "Successfully wrote %d bytes to %s"
               (string-bytes content)
               path)))))

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

(defun e-base-tools-register-edit (registry directory)
  "Register the base edit tool in REGISTRY rooted at DIRECTORY."
  (e-tools-register
   registry
   :name "edit"
   :description "Edit a single file using exact text replacements in edits[].oldText and edits[].newText."
   :parameters '(:type "object"
                 :properties (:path (:type "string")
                              :edits (:type "array"
                                      :items (:type "object"
                                              :properties (:oldText (:type "string")
                                                           :newText (:type "string"))
                                              :required ["oldText" "newText"])))
                 :required ["path" "edits"])
   :handler
   (lambda (arguments)
     (let* ((path (e-base-tools--argument-string arguments :path))
            (absolute-path (e-base-tools--resolve-path path directory))
            (raw-content (e-base-tools--file-text absolute-path))
            (line-ending (e-base-tools--line-ending raw-content))
            (content (e-base-tools--normalize-line-endings raw-content))
            (edits (e-base-tools--normalize-edits
                    (plist-get arguments :edits)))
            (new-content (e-base-tools--apply-edits content edits path))
            (final-content
             (e-base-tools--restore-line-endings new-content line-ending)))
       (write-region final-content nil absolute-path nil 'silent)
       (list :message (format "Successfully replaced %d block(s) in %s."
                              (length edits)
                              path)
             :replacements (length edits)
             :diff (e-base-tools--simple-diff content new-content))))))

(defun e-base-tools--shell-command ()
  "Return command list prefix for the base bash tool."
  (list (or e-base-tools-shell-file-name shell-file-name "/bin/sh")
        (or e-base-tools-shell-command-switch shell-command-switch "-c")))

(defun e-base-tools--run-shell-command (command directory timeout)
  "Run shell COMMAND in DIRECTORY with optional TIMEOUT seconds."
  (let* ((buffer (generate-new-buffer " *e-base-bash*"))
         (default-directory directory)
         (done nil)
         (exit-code nil)
         (timed-out nil)
         (command-prefix (e-base-tools--shell-command))
         (shell-command (format "{\n%s\n} 2>&1" command))
         (process
          (make-process
           :name "e-base-bash"
           :buffer buffer
           :command (append command-prefix (list shell-command))
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :sentinel (lambda (proc _event)
                        (when (memq (process-status proc) '(exit signal))
                          (setq exit-code (process-exit-status proc))
                         (setq done t))))))
    (unwind-protect
        (let ((deadline (and timeout (time-add (current-time) timeout))))
          (set-process-query-on-exit-flag process nil)
          (while (not done)
            (when (and deadline (not (time-less-p (current-time) deadline)))
              (setq timed-out t)
              (when (process-live-p process)
                (kill-process process)))
            (accept-process-output process 0.05))
          (let ((output (with-current-buffer buffer
                          (buffer-string))))
            (cond
             (timed-out
              (signal 'e-base-tools-bash-invalid
                      (list (string-trim-right
                             (format "%s\n\nCommand timed out after %s seconds"
                                     output timeout)))))
             ((and exit-code (not (zerop exit-code)))
              (signal 'e-base-tools-bash-invalid
                      (list (string-trim-right
                             (format "%s\n\nCommand exited with code %d"
                                     output exit-code)))))
             (t output))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun e-base-tools--truncate-bash-output (output)
  "Return bash OUTPUT, truncating and persisting full output when needed."
  (let ((truncation (e-base-tools--truncate-tail-lines output)))
    (if (not (plist-get truncation :truncated))
        output
      (let ((full-output-path (make-temp-file "e-base-bash-" nil ".log")))
        (write-region output nil full-output-path nil 'silent)
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
   :description "Execute a shell command in the current working directory and return captured stdout and stderr."
   :parameters '(:type "object"
                 :properties (:command (:type "string")
                              :timeout (:type "number"))
                 :required ["command"])
   :handler
   (lambda (arguments)
     (let ((command (e-base-tools--argument-string arguments :command))
           (timeout (e-base-tools--optional-positive-number arguments :timeout)))
       (e-base-tools--truncate-bash-output
        (e-base-tools--run-shell-command command directory timeout))))))

(defun e-base-tools-register-defaults (registry directory)
  "Register all base tools in REGISTRY rooted at DIRECTORY."
  (e-base-tools-register-read registry directory)
  (e-base-tools-register-write registry directory)
  (e-base-tools-register-edit registry directory)
  (e-base-tools-register-bash registry directory)
  registry)

(provide 'e-base-tools)

;;; e-base-tools.el ends here
