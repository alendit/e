;;; e-tool-output-truncation.el --- Tool output context guard for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A post-tool-call hook that bounds model-visible tool result content and stores
;; the full output in session tmp resources.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-hooks)
(require 'e-session-tmp-resources)
(require 'e-tools)

(defcustom e-tool-output-truncation-max-bytes (* 50 1024)
  "Maximum UTF-8 bytes of tool output to expose to the model."
  :type 'integer
  :group 'e)

(defcustom e-tool-output-truncation-max-lines 2000
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

(defun e-tool-output-truncation--notice
    (shown-bytes shown-lines original-bytes original-lines uri)
  "Return truncation notice text."
  (format "[Tool output truncated: showing first %d bytes / %d lines of %d bytes / %d lines. Full output: %s]"
          shown-bytes
          shown-lines
          original-bytes
          original-lines
          uri))

(defun e-tool-output-truncation--metadata
    (metadata uri original-bytes original-lines shown-bytes shown-lines)
  "Return METADATA with truncation fields added."
  (append (list :truncated t
                :tmp-uri uri
                :original-bytes original-bytes
                :original-lines original-lines
                :shown-bytes shown-bytes
                :shown-lines shown-lines)
          metadata))

(defun e-tool-output-truncation-post-tool-call (result context)
  "Apply tool output truncation policy to RESULT using CONTEXT."
  (let ((metadata (plist-get result :metadata)))
    (if (plist-get metadata :truncated)
        result
      (let* ((content-text (e-tools-result-content-text
                            (plist-get result :content)))
             (original-bytes (string-bytes content-text))
             (original-lines (e-tool-output-truncation--line-count content-text)))
        (if (and (<= original-bytes e-tool-output-truncation-max-bytes)
                 (<= original-lines e-tool-output-truncation-max-lines))
            result
          (let* ((preview (e-tool-output-truncation--preview content-text))
                 (shown-bytes (string-bytes preview))
                 (shown-lines (e-tool-output-truncation--line-count preview))
                 (uri (e-session-tmp-write
                       (plist-get context :harness)
                       (plist-get context :session-id)
                       (e-tool-output-truncation--relative-name result context)
                       content-text))
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
                        uri
                        original-bytes
                        original-lines
                        shown-bytes
                        shown-lines))
            truncated))))))

(defun e-tool-output-truncation-capability-create ()
  "Return the tool output truncation capability."
  (e-capability-create
   :id 'tool-output-truncation
   :name "Tool Output Truncation"
   :hooks
   (list (e-hook-create
          :id "50-tool-output-truncation"
          :point :post-tool-call
          :handler #'e-tool-output-truncation-post-tool-call))))

(provide 'e-tool-output-truncation)

;;; e-tool-output-truncation.el ends here
