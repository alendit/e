;;; e-tool-output-truncation.el --- Tool output context guard for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A tool-result-presentation hook that bounds model-visible tool result content and stores
;; the full output in the owning session tmp resources, or in generic raw-result
;; resources when no session owns the result.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-hooks)
(require 'e-raw-results)
(require 'e-session-tmp-resources)
(require 'e-tools)

(defcustom e-tool-output-truncation-max-bytes (* 8 1024)
  "Maximum UTF-8 bytes of tool output to expose to the model."
  :type 'integer
  :group 'e)

(defcustom e-tool-output-truncation-max-lines 1000
  "Maximum tool output lines to expose to the model."
  :type 'integer
  :group 'e)

(defun e-tool-output-truncation--line-count (text)
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

(defun e-tool-output-truncation--line-prefix (text max-lines)
  "Return TEXT limited to MAX-LINES logical lines."
  (if (<= (e-tool-output-truncation--line-count text) max-lines)
      text
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (forward-line max-lines)
      (buffer-substring-no-properties (point-min) (point)))))

(defun e-tool-output-truncation--byte-prefix (text max-bytes)
  "Return TEXT limited to MAX-BYTES UTF-8 bytes without splitting characters."
  (let ((bytes 0)
        (index 0)
        (length (length text)))
    (while (and (< index length)
                (let ((next-bytes (string-bytes (substring text index (1+ index)))))
                  (when (<= (+ bytes next-bytes) max-bytes)
                    (setq bytes (+ bytes next-bytes))
                    t)))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-tool-output-truncation--preview (text)
  "Return bounded preview text for TEXT."
  (e-tool-output-truncation--byte-prefix
   (e-tool-output-truncation--line-prefix
    text
    e-tool-output-truncation-max-lines)
   e-tool-output-truncation-max-bytes))

(defun e-tool-output-truncation--safe-fragment (value fallback)
  "Return VALUE as a safe tmp path fragment, or FALLBACK."
  (let* ((text (if (and (stringp value)
                        (not (string-empty-p value)))
                   value
                 fallback))
         (safe (replace-regexp-in-string "[^A-Za-z0-9._-]" "-" text)))
    (if (string-empty-p safe) fallback safe)))

(defun e-tool-output-truncation--relative-name (result context)
  "Return session-local tmp relative name for RESULT in CONTEXT."
  (let ((turn-id (e-tool-output-truncation--safe-fragment
                  (plist-get context :turn-id)
                  "turn"))
        (tool-name (e-tool-output-truncation--safe-fragment
                    (plist-get result :name)
                    "tool"))
        (call-id (e-tool-output-truncation--safe-fragment
                  (plist-get result :tool-call-id)
                  "call")))
    (format "tool-results/%s/%s-%s.txt" turn-id tool-name call-id)))

(defun e-tool-output-truncation--raw-result-id (relative-name)
  "Return generic raw-result id for RELATIVE-NAME."
  (concat "tool-result-"
          (replace-regexp-in-string "[^A-Za-z0-9._-]" "-" relative-name)))

(defun e-tool-output-truncation--owner (result context)
  "Return the raw-result owner plist for RESULT in CONTEXT."
  (list :kind 'tool-result
        :turn-id (plist-get context :turn-id)
        :tool-call-id (plist-get result :tool-call-id)
        :tool-name (plist-get result :name)))

(defun e-tool-output-truncation--write-raw-result
    (result context content preview)
  "Persist full tool result CONTENT for RESULT in CONTEXT."
  (let ((relative-name (e-tool-output-truncation--relative-name result context))
        (owner (e-tool-output-truncation--owner result context)))
    (if (and (plist-get context :harness)
             (stringp (plist-get context :session-id))
             (not (string-empty-p (plist-get context :session-id))))
        (e-session-tmp-write-raw-result
         (plist-get context :harness)
         (plist-get context :session-id)
         relative-name
         content
         :owner owner
         :redaction-policy 'none
         :cleanup-lifetime 'session-tmp
         :preview preview
         :preview-bytes e-tool-output-truncation-max-bytes)
      (e-raw-results-write
       :id (e-tool-output-truncation--raw-result-id relative-name)
       :content content
       :owner owner
       :redaction-policy 'none
       :cleanup-lifetime 'raw-result-store
       :preview preview
       :preview-bytes e-tool-output-truncation-max-bytes))))

(defun e-tool-output-truncation--session-p (context)
  "Return non-nil when CONTEXT names an owning harness session."
  (and (plist-get context :harness)
       (stringp (plist-get context :session-id))
       (not (string-empty-p (plist-get context :session-id)))))

(defun e-tool-output-truncation--readback-uri (result context)
  "Return the tmp:// URI RESULT read, when RESULT is a `read' of one."
  (let* ((call (plist-get context :tool-call))
         (uri (plist-get (plist-get call :arguments) :uri)))
    (when (and (equal (plist-get result :name) "read")
               (equal (plist-get call :name) "read")
               (stringp uri)
               (string-prefix-p "tmp://" uri)
               (e-tool-output-truncation--session-p context))
      uri)))

(defun e-tool-output-truncation--reference
    (result context content preview)
  "Return the full-output reference for RESULT's bounded PREVIEW.
The reference always names plain text.  The invocation-details record is a
one-line JSON archive, so pointing the model at it makes every readback
oversized again."
  (if-let* ((uri (e-tool-output-truncation--readback-uri result context)))
      ;; Saving a copy of an oversized tmp:// read would only nest the same
      ;; text under a new name; send the model back to the source instead.
      (list :uri uri
            :owner (e-tool-output-truncation--owner result context)
            :storage 'session-tmp
            :original-bytes (string-bytes content)
            :preview preview
            :preview-bytes (string-bytes preview)
            :preview-truncated t
            :redaction-policy 'none
            :cleanup-lifetime 'session-tmp)
    (e-tool-output-truncation--write-raw-result
     result context content preview)))

(defun e-tool-output-truncation--authorized-details-uri (result context)
  "Return RESULT's lifecycle-authorized details URI, or nil."
  (let ((context-uri (plist-get context :invocation-details-uri))
        (result-uri
         (plist-get (plist-get result :metadata) :invocation-details-uri)))
    (when (and (stringp context-uri)
               (equal context-uri result-uri)
               (string-prefix-p "tmp://" context-uri)
               (plist-get context :harness)
               (stringp (plist-get context :session-id))
               (not (string-empty-p (plist-get context :session-id)))
               (not (plist-get context :nested)))
      context-uri)))

(defun e-tool-output-truncation--file-content-reference
    (result context content preview)
  "Return a plain-text full-output reference for file-backed CONTENT."
  (let* ((source-uri (e-tools-file-content-uri content))
         (source-path (e-tools-file-content-path content))
         (owner (e-tool-output-truncation--owner result context))
         (original-bytes (e-tools-file-content-original-bytes content))
         (session-p (e-tool-output-truncation--session-p context))
         (tmp-source-p (and (stringp source-uri)
                            (string-prefix-p "tmp://" source-uri)))
         (harness (plist-get context :harness))
         (session-id (plist-get context :session-id)))
    (cond
     ((and session-p tmp-source-p (file-regular-p source-path))
      ;; A persistent store reads tmp:// from SQLite, not from the producer's
      ;; staging file, so the text must be ingested under the same name.
      (let ((uri (if (e-session-tmp--sqlite-store harness)
                     (prog1
                         (e-session-tmp-write-generated
                          harness session-id
                          (substring source-uri (length "tmp://"))
                          (lambda (path) (copy-file source-path path t)))
                       (e-tools-file-content-dispose content))
                   source-uri)))
        (list :uri uri
              :owner owner
              :storage 'session-tmp
              :original-bytes original-bytes
              :preview preview
              :preview-bytes (string-bytes preview)
              :preview-truncated t
              :redaction-policy 'none
              :cleanup-lifetime 'session-tmp)))
     ((file-regular-p source-path)
      (prog1
          (e-raw-results-import-file
           source-path
           :id (e-tool-output-truncation--raw-result-id
                (e-tool-output-truncation--relative-name result context))
           :owner owner
           :redaction-policy 'none
           :cleanup-lifetime 'raw-result-store
           :preview preview
           :preview-bytes e-tool-output-truncation-max-bytes
           :original-bytes original-bytes)
        (e-tools-file-content-dispose content)))
     (t
      ;; The source was consumed upstream; the details archive is the only
      ;; complete copy left.
      (list :uri (or (e-tool-output-truncation--authorized-details-uri
                      result context)
                     source-uri)
            :owner owner
            :storage 'session-tmp
            :original-bytes original-bytes
            :preview preview
            :preview-bytes (string-bytes preview)
            :preview-truncated t
            :redaction-policy 'none
            :cleanup-lifetime 'session-tmp)))))

(defun e-tool-output-truncation--file-content-result (result context content)
  "Return bounded presentation RESULT for file-backed CONTENT."
  (unless (e-tools-file-content-valid-p content)
    (signal 'wrong-type-argument (list 'e-tools-file-content-valid-p content)))
  (let* ((original-bytes (e-tools-file-content-original-bytes content))
         (original-lines (e-tools-file-content-original-lines content))
         (carrier-preview-bytes
          (e-tools-file-content-preview-bytes content))
         (carrier-preview-lines
          (e-tools-file-content-preview-lines content))
         (oversized
          (or (> original-bytes e-tool-output-truncation-max-bytes)
              (> original-lines e-tool-output-truncation-max-lines)
              (< carrier-preview-bytes original-bytes)
              (< carrier-preview-lines original-lines)))
         (preview
          (e-tool-output-truncation--preview
           (e-tools-file-content-preview content)))
         (shown-bytes (string-bytes preview))
         (shown-lines (e-tool-output-truncation--line-count preview))
         (copy (copy-sequence result)))
    (if (not oversized)
        (progn
          ;; PREVIEW is complete in this branch.  Once presentation owns the
          ;; inline result, no separate source file is required.
          (e-tools-file-content-dispose content)
          (plist-put copy :content preview))
      (let* ((reference
              (e-tool-output-truncation--file-content-reference
               result context content preview))
             (notice (e-tool-output-truncation--notice
                      shown-bytes shown-lines original-bytes original-lines
                      (plist-get reference :uri))))
        (setq copy
              (plist-put copy :content
                         (if (string-empty-p preview)
                             notice
                           (concat preview "\n\n" notice))))
        (plist-put copy :metadata
                   (e-tool-output-truncation--metadata
                    (plist-get result :metadata)
                    reference original-lines shown-lines))))))

(defun e-tool-output-truncation--notice
    (shown-bytes shown-lines original-bytes original-lines uri)
  "Return truncation notice text."
  (format "[Tool output truncated: showing first %d bytes / %d lines of %d bytes / %d lines. Full output: %s (plain text; read it with a line or char range)]"
          shown-bytes
          shown-lines
          original-bytes
          original-lines
          uri))

(defun e-tool-output-truncation--metadata
    (metadata reference original-lines shown-lines)
  "Return METADATA with truncation fields added."
  (append (list :truncated t
                :tmp-uri (plist-get reference :uri)
                :raw-result-reference reference
                :original-bytes (plist-get reference :original-bytes)
                :original-lines original-lines
                :shown-bytes (plist-get reference :preview-bytes)
                :shown-lines shown-lines)
          metadata))

(defun e-tool-output-truncation-post-tool-call (result context)
  "Apply tool output truncation policy to RESULT using CONTEXT."
  (let ((metadata (plist-get result :metadata))
        (content (plist-get result :content)))
    (cond
     ((plist-get metadata :truncated)
      result)
     ((e-tools-file-content-p content)
      (e-tool-output-truncation--file-content-result result context content))
     (t
      (let* ((content-text (e-tools-result-content-text content))
             (original-bytes (string-bytes content-text))
             (original-lines (e-tool-output-truncation--line-count content-text)))
        (if (and (<= original-bytes e-tool-output-truncation-max-bytes)
                 (<= original-lines e-tool-output-truncation-max-lines))
            result
          (let* ((preview (e-tool-output-truncation--preview content-text))
                 (shown-bytes (string-bytes preview))
                 (shown-lines (e-tool-output-truncation--line-count preview))
                 (reference
                  (e-tool-output-truncation--reference
                   result
                   context
                   content-text
                   preview))
                 (uri (plist-get reference :uri))
                 (notice (e-tool-output-truncation--notice
                          shown-bytes
                          shown-lines
                          original-bytes
                          original-lines
                          uri))
                 (truncated (copy-sequence result)))
            (plist-put truncated :content
                       (if (string-empty-p preview)
                           notice
                         (concat preview "\n\n" notice)))
            (plist-put truncated :metadata
                       (e-tool-output-truncation--metadata
                        metadata
                        reference
                        original-lines
                        shown-lines))
            truncated)))))))

(defun e-tool-output-truncation-capability-create ()
  "Return the tool output truncation capability."
  (e-capability-create
   :id 'tool-output-truncation
   :name "Tool Output Truncation"
   :hooks
   (list (e-hook-create
          :id "50-tool-output-truncation"
          :point :tool-result-presentation
          :handler #'e-tool-output-truncation-post-tool-call))))

(provide 'e-tool-output-truncation)

;;; e-tool-output-truncation.el ends here
