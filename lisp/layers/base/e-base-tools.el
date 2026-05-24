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
(require 'e-capabilities)
(require 'e-operations)
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
  (let* ((root (file-name-as-directory (expand-file-name directory)))
         (absolute-path (expand-file-name path root)))
    (unless (file-in-directory-p absolute-path root)
      (signal 'e-base-tools-path-outside-root
              (list (format "Path escapes workspace root: %s" path))))
    absolute-path))

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

(defun e-base-tools--write-file-resource (uri content directory)
  "Write CONTENT to parsed file URI in DIRECTORY."
  (let* ((path (plist-get uri :address))
         (absolute-path (e-base-tools--resource-path uri directory)))
    (make-directory (file-name-directory absolute-path) t)
    (write-region content nil absolute-path nil 'silent)
    (format "Successfully wrote %d bytes to %s"
            (string-bytes content)
            path)))

(defun e-base-tools--edit-file-resource (uri edits directory)
  "Apply exact EDITS to parsed file URI in DIRECTORY."
  (let* ((path (plist-get uri :address))
         (absolute-path (e-base-tools--resource-path uri directory))
         (raw-content (e-base-tools--file-text absolute-path))
         (line-ending (e-base-tools--line-ending raw-content))
         (content (e-base-tools--normalize-line-endings raw-content))
         (normalized-edits (e-base-tools--normalize-edits edits))
         (new-content (e-base-tools--apply-edits content normalized-edits path))
         (final-content
          (e-base-tools--restore-line-endings new-content line-ending)))
    (write-region final-content nil absolute-path nil 'silent)
    (list :message (format "Successfully replaced %d block(s) in %s."
                           (length normalized-edits)
                           path)
          :replacements (length normalized-edits)
          :diff (e-base-tools--simple-diff content new-content))))

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

(defun e-base-tools-register-file-read-resource (registry directory)
  "Register read-only file resource methods in REGISTRY rooted at DIRECTORY."
  (e-resources-register
   registry
   (e-base-tools--file-read-method directory)))

(defun e-base-tools-register-file-resource (registry directory)
  "Register read/write/edit file resource methods in REGISTRY rooted at DIRECTORY."
  (dolist (method (list (e-base-tools--file-read-method directory)
                        (e-base-tools--file-write-method directory)
                        (e-base-tools--file-edit-method directory)))
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
    (write-region "" nil output-file nil 'silent)
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
    (write-region chunk nil
                  (e-base-tools--bash-collector-output-file collector)
                  'append
                  'silent)
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
                              :timeout (:type "number"
                                        :description "Hard timeout in seconds. When reached, e kills the process and returns a tool error. Use modest values for routine commands; long-running commands need an explicit control pattern."))
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
