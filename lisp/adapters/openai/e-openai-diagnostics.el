;;; e-openai-diagnostics.el --- Bounded provider diagnostics -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns provider error classification, bounded diagnostic values, and the
;; optional raw-response/event observation buffer.  It does not know profiles,
;; wire formats, or transport lifecycle.

;;; Code:

(require 'cl-lib)
(require 'pp)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'e-request)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(define-error 'e-openai-auth-missing "OpenAI/Codex auth is missing")
(define-error 'e-openai-auth-invalid "OpenAI/Codex auth is invalid")
(define-error 'e-openai-provider-invalid "OpenAI provider profile is invalid")
(define-error 'e-openai-context-projection-invalid
  "OpenAI context projection is ambiguous")
(define-error 'e-openai-request-timeout "OpenAI/Codex request timed out")
(define-error 'e-openai-websocket-premature-close
  "Responses WebSocket closed before completion")

(defconst e-openai-diagnostics--retryable-error-patterns
  '("rate limit" "rate_limit_error" "too many requests"
    "overloaded" "overloaded_error" "api_error"
    "internal_server_error" "server_error" "service unavailable"
    "bad gateway" "gateway time" "request timed out" "idle timed out"
    "connection termination" "connection reset" "reset by peer"
    "connect error" "before headers" "disconnect" "broken pipe"
    "premature")
  "OpenAI and gateway error fragments that identify transient failures.")

(defun e-openai-diagnostics--retryable-status-p (status)
  "Return non-nil when OpenAI HTTP STATUS permits a retry."
  (and (numberp status)
       (or (memq status '(408 409 429)) (>= status 500))))

(defun e-openai-diagnostics--retry-after-from-text (message &optional now)
  "Return provider retry delay parsed from MESSAGE, or nil.
NOW defaults to the current time and is injectable for tests."
  (let ((text (downcase (or message "")))
        (now (or now (float-time))))
    (cond
     ((string-match
       "\\(?:try again in\\|retry after\\|retry in\\)[^0-9]*\\([0-9]+\\(?:\\.[0-9]+\\)?\\)[[:space:]]*\\(m\\|min\\|s\\|sec\\|seconds?\\|minutes?\\)?"
       text)
      (let ((number (string-to-number (match-string 1 text)))
            (unit (match-string 2 text)))
        (if (and unit (string-prefix-p "m" unit))
            (* number 60.0)
          number)))
     ((string-match
       "resets? at[:[:space:]]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[T[:space:]][0-9]\\{2\\}:[0-9]\\{2\\}\\(?::[0-9]\\{2\\}\\)?\\)"
       text)
      (let* ((stamp (replace-regexp-in-string
                     "T" " " (match-string 1 text)))
             (parsed (ignore-errors
                       (float-time
                        (encode-time
                         (parse-time-string (concat stamp " +0000")))))))
        (and parsed (- parsed now)))))))

(defun e-openai-diagnostics--error-code (details)
  "Return an OpenAI error code or type from DETAILS, when present."
  (let* ((error (and (listp details) (plist-get details :error)))
         (response (and (listp details) (plist-get details :response)))
         (response-error (and (listp response) (plist-get response :error))))
    (or (and (listp details) (plist-get details :error-type))
        (and (listp error) (or (plist-get error :code)
                               (plist-get error :type)))
        (and (listp response-error)
             (or (plist-get response-error :code)
                 (plist-get response-error :type))))))

(defun e-openai-diagnostics-normalize-error-details (message details condition)
  "Return OpenAI-owned normalized retry metadata for an error.
MESSAGE, DETAILS, and CONDITION are the backend error surfaces received by the
provider-neutral backend contract."
  (let* ((normalized (if (listp details) (copy-tree details) nil))
         (text (downcase (or message "")))
         (status (or (plist-get normalized :status)
                     (plist-get normalized :status-code)))
         (code (e-openai-diagnostics--error-code normalized))
         (code-text (downcase (format "%s" (or code ""))))
         (timeout-p (eq (car-safe condition) 'e-openai-request-timeout))
         (premature-close-p
          (eq (car-safe condition) 'e-openai-websocket-premature-close))
         (pattern (seq-find (lambda (candidate)
                              (string-match-p (regexp-quote candidate) text))
                            e-openai-diagnostics--retryable-error-patterns))
         (reason
          (cond
           ((or (equal status 429)
                (string-match-p "rate[_ -]?limit" code-text)
                (member pattern '("rate limit" "rate_limit_error"
                                  "too many requests")))
            'rate-limit)
           ((or timeout-p (equal status 408)
                (member pattern '("request timed out" "idle timed out")))
            'timeout)
           ((or premature-close-p (equal pattern "premature"))
            'premature-stream)
           ((equal status 409) 'conflict)
           ((or (e-openai-diagnostics--retryable-status-p status)
                (string-match-p
                 "\\(?:overloaded\\|api_error\\|server_error\\)" code-text)
                (member pattern '("overloaded" "overloaded_error" "api_error"
                                  "internal_server_error" "server_error"
                                  "service unavailable" "bad gateway"
                                  "gateway time")))
            'provider-unavailable)
           (pattern 'transport)))
         (retry-after
          (or (plist-get normalized :retry-after-seconds)
              (plist-get normalized :retry-after)
              (e-openai-diagnostics--retry-after-from-text message))))
    (setq normalized (plist-put normalized :retryable (and reason t)))
    (when reason
      (setq normalized (plist-put normalized :retry-reason reason)))
    (when (numberp retry-after)
      (setq normalized
            (plist-put normalized :retry-after-seconds retry-after)))
    normalized))

(defun e-openai-diagnostics-normalize-backend-error-item (item)
  "Return backend error ITEM with OpenAI-normalized payload details."
  (if (not (eq (plist-get item :type) 'backend-error))
      item
    (let ((normalized (copy-tree item)))
      (plist-put
       normalized :payload
       (e-openai-diagnostics-normalize-error-details
        (plist-get normalized :content)
        (plist-get normalized :payload)
        nil))
      normalized)))

(defun e-openai-diagnostics--profile-enabled-p ()
  "Return non-nil when developer profiling is available and enabled."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-openai-adapter-measure (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when dev profiling is enabled."
  (if (e-openai-diagnostics--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-openai-provider-reject-sync-in-hot-path (operation)
  "Reject synchronous OpenAI OPERATION from marked interactive hot paths."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error operation)))

(defcustom e-openai-codex-debug nil
  "When non-nil, retain the last raw Codex response and event summaries."
  :type 'boolean
  :group 'e-openai)

(defcustom e-openai-codex-raw-responses-max-bytes (* 256 1024)
  "Maximum raw provider diagnostic payload retained in bytes.
The bound applies to both `e-openai-diagnostics--last-diagnostics' and the
hidden raw response buffer.  Set this to zero to keep event summaries without
raw provider payloads."
  :type '(integer :tag "Bytes")
  :group 'e-openai)

(defcustom e-openai-codex-raw-responses-buffer-name
  " *e-openai-codex-raw-responses*"
  "Hidden buffer used to retain raw Codex provider responses."
  :type 'string
  :group 'e-openai)

(defvar e-openai-diagnostics--last-diagnostics nil
  "Most recent bounded OpenAI diagnostic projection.")

(defcustom e-openai-diagnostic-print-length 50
  "Maximum list/vector/hash entries printed in OpenAI diagnostic fallbacks."
  :type 'integer
  :group 'e-openai)

(defcustom e-openai-diagnostic-print-level 6
  "Maximum nested depth printed in OpenAI diagnostic fallbacks."
  :type 'integer
  :group 'e-openai)

(defcustom e-openai-diagnostic-string-max-bytes 4096
  "Maximum bytes shown for one string in OpenAI diagnostic fallbacks."
  :type 'integer
  :group 'e-openai)

(defcustom e-openai-diagnostic-result-max-bytes (* 8 1024)
  "Maximum bytes shown for a full OpenAI diagnostic fallback."
  :type 'integer
  :group 'e-openai)

(defun e-openai-diagnostics-bounded-text (text)
  "Return TEXT bounded for provider diagnostics."
  (e-openai-diagnostics--bounded-diagnostic-text text))

(defun e-openai-diagnostics-bounded-string (value)
  "Return a bounded printed representation of VALUE for diagnostics."
  (e-openai-diagnostics--bounded-diagnostic-string value))

(defun e-openai-diagnostics-url-metadata (url)
  "Return sanitized diagnostic metadata for URL."
  (let* ((parsed (url-generic-parse-url url))
         (path (or (url-filename parsed) "/")))
    (when (string-match "\\`\\([^?#]*\\)" path)
      (setq path (match-string 1 path)))
    (when (string-empty-p path)
      (setq path "/"))
    (list :url-host (url-host parsed)
          :url-path path)))

(defun e-openai-diagnostics-record-stream (stream-text event-summaries)
  "Record bounded STREAM-TEXT and EVENT-SUMMARIES when debug is enabled."
  (when e-openai-codex-debug
    (setq e-openai-diagnostics--last-diagnostics
          (list :raw-response
                (e-openai-diagnostics--raw-response-tail stream-text)
                :events (copy-tree event-summaries))))
  e-openai-diagnostics--last-diagnostics)

(defun e-openai-diagnostics--diagnostic-byte-prefix (text max-bytes)
  "Return a prefix of TEXT no longer than MAX-BYTES."
  (let ((bytes 0)
        (index 0)
        (length (length text)))
    (while (and (< index length)
                (<= (+ bytes
                       (string-bytes (substring text index (1+ index))))
                    max-bytes))
      (setq bytes (+ bytes (string-bytes (substring text index (1+ index)))))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-openai-diagnostics--bounded-diagnostic-text (text)
  "Return TEXT bounded for provider diagnostics."
  (let* ((text (or text ""))
         (max-bytes (max 0 e-openai-diagnostic-string-max-bytes))
         (original-bytes (string-bytes text)))
    (if (<= original-bytes max-bytes)
        text
      (let* ((preview (e-openai-diagnostics--diagnostic-byte-prefix text max-bytes))
             (shown-bytes (string-bytes preview)))
        (format
         "%s\n[OpenAI diagnostic string truncated: showing first %d of %d bytes]"
         preview shown-bytes original-bytes)))))

(defun e-openai-diagnostics--diagnostic-preview-value (value depth seen)
  "Return a bounded preview copy of VALUE for provider diagnostics.
DEPTH limits recursive descent.  SEEN tracks container identity."
  (cond
   ((stringp value)
    (e-openai-diagnostics--bounded-diagnostic-text value))
   ((or (not value) (symbolp value) (numberp value) (characterp value))
    value)
   ((<= depth 0)
    '...)
   ((or (consp value) (vectorp value) (hash-table-p value))
    (if (gethash value seen)
        "#<cycle>"
      (puthash value t seen)
      (cond
       ((consp value)
        (let ((tail value)
              (items nil)
              (count 0)
              (limit (max 0 e-openai-diagnostic-print-length)))
          (while (and (consp tail) (< count limit))
            (push (e-openai-diagnostics--diagnostic-preview-value
                   (car tail) (1- depth) seen)
                  items)
            (setq tail (cdr tail))
            (setq count (1+ count)))
          (cond
           ((consp tail)
            (append (nreverse items) '(...)))
           ((null tail)
            (nreverse items))
           (t
            (append (nreverse items)
                    (list :dotted-tail
                          (e-openai-diagnostics--diagnostic-preview-value
                           tail (1- depth) seen)))))))
       ((vectorp value)
        (let* ((limit (max 0 e-openai-diagnostic-print-length))
               (count (min (length value) limit))
               (items nil))
          (dotimes (index count)
            (push (e-openai-diagnostics--diagnostic-preview-value
                   (aref value index) (1- depth) seen)
                  items))
          (apply #'vector
                 (nreverse
                  (if (< count (length value))
                      (cons '... items)
                    items)))))
       ((hash-table-p value)
        (let ((pairs nil)
              (count 0)
              (limit (max 0 e-openai-diagnostic-print-length))
              (truncated nil))
          (catch 'done
            (maphash
             (lambda (key entry)
               (if (>= count limit)
                   (progn
                     (setq truncated t)
                     (throw 'done nil))
                 (push
                  (cons
                   (e-openai-diagnostics--diagnostic-preview-value key (1- depth) seen)
                   (e-openai-diagnostics--diagnostic-preview-value entry (1- depth) seen))
                  pairs)
                 (setq count (1+ count))))
             value))
          (list :hash-table-preview (nreverse pairs)
                :truncated truncated
                :test (hash-table-test value)))))))
   (t value)))

(defun e-openai-diagnostics--truncate-diagnostic-string (text)
  "Return TEXT capped to `e-openai-diagnostic-result-max-bytes'."
  (let* ((max-bytes (max 0 e-openai-diagnostic-result-max-bytes))
         (original-bytes (string-bytes text)))
    (if (<= original-bytes max-bytes)
        text
      (let* ((preview (e-openai-diagnostics--diagnostic-byte-prefix text max-bytes))
             (shown-bytes (string-bytes preview)))
        (format
         "%s\n\n[OpenAI diagnostic truncated: showing first %d of %d bytes]"
         preview shown-bytes original-bytes)))))

(defun e-openai-diagnostics--bounded-diagnostic-string (value)
  "Return a bounded printed representation of VALUE for provider diagnostics."
  (let* ((preview
          (e-openai-diagnostics--diagnostic-preview-value
           value
           (max 0 e-openai-diagnostic-print-level)
           (make-hash-table :test 'eq)))
         (print-length (max 0 e-openai-diagnostic-print-length))
         (print-level (max 0 e-openai-diagnostic-print-level)))
    (e-openai-diagnostics--truncate-diagnostic-string
     (prin1-to-string preview))))

(defun e-openai-codex-last-diagnostics ()
  "Return the last captured OpenAI/Codex diagnostics.
When called interactively, display diagnostics in a temporary buffer.
Diagnostics are captured only when `e-openai-codex-debug' is non-nil."
  (interactive)
  (if (called-interactively-p 'interactive)
      (with-current-buffer (get-buffer-create "*e-openai-codex-diagnostics*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (pp-to-string e-openai-diagnostics--last-diagnostics))
          (goto-char (point-min))
          (special-mode))
        (display-buffer (current-buffer)))
    e-openai-diagnostics--last-diagnostics))

(defun e-openai-diagnostics--raw-response-tail (stream-text)
  "Return a UTF-8-safe bounded tail of STREAM-TEXT for debug diagnostics."
  (let* ((text (or stream-text ""))
         (limit (max 0 e-openai-codex-raw-responses-max-bytes)))
    (cond
     ((zerop limit) "")
     ((<= (string-bytes text) limit) text)
     (t
      ;; Work in UTF-8 bytes so the configured budget means the same thing for
      ;; ASCII and multibyte provider text.  Move right over continuation bytes
      ;; before decoding so the retained tail starts on a character boundary.
      (let* ((encoded (encode-coding-string text 'utf-8))
             (start (- (length encoded) limit)))
        (while (and (< start (length encoded))
                    (let ((byte (aref encoded start)))
                      (and (>= byte #x80) (<= byte #xBF))))
          (setq start (1+ start)))
        (decode-coding-string (substring encoded start) 'utf-8 t))))))

(defun e-openai-diagnostics--trim-raw-response-buffer (buffer)
  "Trim BUFFER to `e-openai-codex-raw-responses-max-bytes'."
  (with-current-buffer buffer
    (let ((limit (max 0 e-openai-codex-raw-responses-max-bytes)))
      (when (> (string-bytes (buffer-string)) limit)
        (let ((tail (e-openai-diagnostics--raw-response-tail (buffer-string))))
          (erase-buffer)
          (insert tail))))))

(defun e-openai-diagnostics-append-raw-response (stream-text)
  "Append a bounded debug tail of STREAM-TEXT to the hidden response buffer."
  (when (and e-openai-codex-debug
             (not (string-empty-p (or stream-text "")))
             (> e-openai-codex-raw-responses-max-bytes 0))
    (with-current-buffer (get-buffer-create
                          e-openai-codex-raw-responses-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n"))
        (insert ";;; " (current-time-string) "\n")
        (insert (e-openai-diagnostics--raw-response-tail stream-text))
        (unless (bolp)
          (insert "\n"))
        (e-openai-diagnostics--trim-raw-response-buffer (current-buffer))))))


(provide 'e-openai-diagnostics)

;;; e-openai-diagnostics.el ends here
