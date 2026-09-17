;;; e-tool-invocation-details.el --- Temporary semantic tool details -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Harness-owned, session-scoped details for ordinary model-facing tool calls.
;; This stage runs after semantic tool post-processing and before any
;; model-facing presentation transform.  The document is deliberately a
;; provider-neutral JSON value; transport and live runtime objects never enter
;; the temporary artifact.

;;; Code:

(require 'cl-lib)
(require 'e-json)
(require 'seq)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-hooks)
(require 'e-session-tmp-resources)
(require 'e-tools)

(defconst e-tool-invocation-details-version 1
  "Version of the temporary tool invocation details document.")

(defconst e-tool-invocation-details--base64-content-encoding
  "base64-utf8-bytes"
  "Wire encoding for semantic strings containing Emacs raw-byte characters.")

(define-error 'e-tool-invocation-details-invalid
  "Tool invocation details are invalid")
(define-error 'e-tool-invocation-details-nonportable
  "Tool invocation details contain nonportable data"
  'e-tool-invocation-details-invalid)

(defconst e-tool-invocation-details--omit
  (make-symbol "e-tool-invocation-details-omit")
  "Sentinel for metadata fields that contain nonportable values.")

(defun e-tool-invocation-details--plist-p (value)
  "Return non-nil when VALUE is a keyword plist."
  (and (listp value)
       (cl-evenp (length value))
       (cl-loop for (key _item) on value by #'cddr
                always (keywordp key))))

(defun e-tool-invocation-details--object-entry (key value)
  "Return a canonical JSON object entry for KEY and VALUE."
  (unless (or (stringp key) (symbolp key) (numberp key))
    (signal 'e-tool-invocation-details-nonportable (list key)))
  (cons (intern
         (concat ":"
                 (cond ((keywordp key) (substring (symbol-name key) 1))
                       ((symbolp key) (symbol-name key))
                       ((stringp key) key)
                       (t (number-to-string key)))))
        value))

(defun e-tool-invocation-details--portable-value
    (value &optional metadata stack)
  "Return a detached canonical JSON VALUE.
When METADATA is non-nil, unsupported nested values are omitted from semantic
metadata rather than leaking live implementation objects.  STACK detects
cycles and prevents a malformed handler value from recursing indefinitely."
  (cond
   ((stringp value)
    ;; `json-serialize' only accepts multibyte text.  Tool arguments can still
    ;; contain an explicitly byte-oriented unibyte string (for example a shell
    ;; command containing octal bytes); project that string to the JSON
    ;; boundary without changing the executed call.  File-backed result bytes
    ;; use the separate base64 stream path below and remain byte-exact.
    (if (multibyte-string-p value)
        (copy-sequence value)
      (decode-coding-string value 'iso-latin-1)))
   ((or (numberp value) (eq value t)
        (eq value e-json-false) (eq value e-json-null))
    value)
   ((null value) e-json-null)
   ((memq value stack)
    (if metadata
        e-tool-invocation-details--omit
      (signal 'e-tool-invocation-details-nonportable
              (list "Cyclic invocation details value"))))
   ((keywordp value)
    (substring (symbol-name value) 1))
   ((symbolp value)
    (symbol-name value))
   ((or (bufferp value) (markerp value) (processp value) (windowp value)
        (functionp value) (subrp value) (keymapp value))
    (if metadata e-tool-invocation-details--omit
      (signal 'e-tool-invocation-details-nonportable (list value))))
   ((vectorp value)
    (let (items)
      (dotimes (index (length value))
        (let ((normalized
               (e-tool-invocation-details--portable-value
                (aref value index) metadata (cons value stack))))
          (unless (eq normalized e-tool-invocation-details--omit)
            (push normalized items))))
      (vconcat (nreverse items))))
   ((hash-table-p value)
    (let ((source-nonempty (> (hash-table-count value) 0)) entries)
      (maphash
       (lambda (key item)
         (let ((normalized
                (e-tool-invocation-details--portable-value
                 item metadata (cons value stack))))
           (unless (eq normalized e-tool-invocation-details--omit)
             (push (e-tool-invocation-details--object-entry key normalized)
                   entries))))
       value)
      (setq entries
            (sort entries (lambda (left right)
                           (string< (symbol-name (car left))
                                    (symbol-name (car right))))))
      (cond
       (entries
        (apply #'append
               (mapcar (lambda (entry) (list (car entry) (cdr entry)))
                       entries)))
       ((and metadata source-nonempty)
        e-tool-invocation-details--omit)
       (t nil))))
   ((e-tool-invocation-details--plist-p value)
    (let ((container value)
          (source-nonempty (not (null value)))
          entries)
      (while value
        (let* ((key (pop value))
               (item (pop value))
               (normalized
                (e-tool-invocation-details--portable-value
                 item metadata (cons container stack))))
          (unless (eq normalized e-tool-invocation-details--omit)
            (push (e-tool-invocation-details--object-entry key normalized)
                  entries))))
      (setq entries
            (sort entries (lambda (left right)
                           (string< (symbol-name (car left))
                                    (symbol-name (car right))))))
      (cond
       (entries
        (apply #'append
               (mapcar (lambda (entry) (list (car entry) (cdr entry)))
                       entries)))
       ((and metadata source-nonempty)
        e-tool-invocation-details--omit)
       (t nil))))
   ((and (listp value) (cl-every #'consp value))
    (let ((source-nonempty (not (null value))) entries)
      (dolist (entry value)
        (let ((normalized
               (e-tool-invocation-details--portable-value
                (cdr entry) metadata (cons value stack))))
          (unless (eq normalized e-tool-invocation-details--omit)
            (push (e-tool-invocation-details--object-entry
                   (car entry) normalized)
                  entries))))
      (setq entries
            (sort entries (lambda (left right)
                           (string< (symbol-name (car left))
                                    (symbol-name (car right))))))
      (cond
       (entries
        (apply #'append
               (mapcar (lambda (entry) (list (car entry) (cdr entry)))
                       entries)))
       ((and metadata source-nonempty)
        e-tool-invocation-details--omit)
       (t nil))))
   ((listp value)
    (let (items)
      (dolist (item value)
        (let ((normalized
               (e-tool-invocation-details--portable-value
                item metadata (cons value stack))))
          (unless (eq normalized e-tool-invocation-details--omit)
            (push normalized items))))
      (vconcat (nreverse items))))
   (t
    (if metadata e-tool-invocation-details--omit
      (signal 'e-tool-invocation-details-nonportable (list value))))))

(defun e-tool-invocation-details--empty-object-p (value)
  "Return non-nil when VALUE is an encoded empty object."
  (and (hash-table-p value) (= (hash-table-count value) 0)))

(defun e-tool-invocation-details--valid-id-p (value)
  "Return non-nil when VALUE is a nonblank stable identifier."
  (and (stringp value) (not (string-empty-p value))))

(defun e-tool-invocation-details--result-document (result)
  "Return the portable semantic RESULT document."
  (unless (and (listp result)
               (plist-member result :status)
               (plist-member result :content))
    (signal 'e-tool-invocation-details-invalid (list result)))
  (let* ((source-content (plist-get result :content))
         (declared-encoding (plist-get result :content-encoding))
         (raw-string-p
          (and (stringp source-content)
               (seq-some (lambda (character) (> character #x10ffff))
                         source-content)))
         (content
          (if declared-encoding
              source-content
            (if raw-string-p
              (base64-encode-string
               (encode-coding-string source-content 'utf-8-unix) t)
              (e-tool-invocation-details--portable-value source-content))))
         (metadata
         (e-tool-invocation-details--portable-value
          (or (plist-get result :metadata)
              (make-hash-table :test 'equal))
          t)))
    (when (and declared-encoding
               (not (equal declared-encoding
                           e-tool-invocation-details--base64-content-encoding)))
      (signal 'e-tool-invocation-details-invalid (list result)))
    (append
     (list :status
           (e-tool-invocation-details--portable-value
            (plist-get result :status))
           :content content)
     (when (or raw-string-p declared-encoding)
       (list :content_encoding
             (or declared-encoding
                 e-tool-invocation-details--base64-content-encoding)))
     (list :metadata
                 (if (eq metadata e-tool-invocation-details--omit)
                     nil
                   metadata)))))

(defun e-tool-invocation-details--document-wire (document)
  "Validate DOCUMENT and return its canonical JSON object shape."
  (let* ((version (plist-get document :version))
         (call-id (plist-get document :tool-call-id))
         (tool (plist-get document :tool))
         (arguments-p (plist-member document :arguments))
         (received-p (plist-member document :received-arguments))
         (result (plist-get document :result))
         (allowed '(:version :tool-call-id :tool :arguments
                    :received-arguments :result)))
    (unless (and (numberp version)
                 (= version e-tool-invocation-details-version)
                 (e-tool-invocation-details--valid-id-p call-id)
                 (e-tool-invocation-details--valid-id-p tool)
                 (listp result))
      (signal 'e-tool-invocation-details-invalid (list document)))
    (let ((keys document))
      (while keys
        (unless (memq (pop keys) allowed)
          (signal 'e-tool-invocation-details-invalid (list document)))
        (pop keys)))
    (unless (or (and arguments-p (not received-p))
                (and received-p (not arguments-p)))
      (signal 'e-tool-invocation-details-invalid (list document)))
    (let ((wire (list :version version
                      :tool_call_id call-id
                      :tool tool)))
      (let ((value (if arguments-p
                       (plist-get document :arguments)
                     (plist-get document :received-arguments))))
        (unless (or (null value)
                    (e-tool-invocation-details--empty-object-p value)
                    (listp value) (vectorp value) (hash-table-p value))
          (signal 'e-tool-invocation-details-nonportable (list value)))
        (setq wire
              (append wire
                      (list (if arguments-p :arguments
                              :received_arguments)
                            ;; Canonical nil is the empty JSON object.  Keep
                            ;; the field present so decode can distinguish an
                            ;; empty object from a missing field.
                            (e-json-assert-value value)))))
      (append wire (list :result result)))))

(defun e-tool-invocation-details--document-plist (parsed)
  "Return strict Lisp DOCUMENT from parsed JSON PARSED."
  (unless (e-tool-invocation-details--plist-p parsed)
    (signal 'e-tool-invocation-details-invalid (list parsed)))
  (let ((allowed '(:version :tool_call_id :tool :arguments
                   :received_arguments :result))
        (keys parsed))
    (while keys
      (unless (memq (pop keys) allowed)
        (signal 'e-tool-invocation-details-invalid (list parsed)))
      (pop keys)))
  (let* ((version (plist-get parsed :version))
         (call-id (plist-get parsed :tool_call_id))
         (tool (plist-get parsed :tool))
         (arguments-p (plist-member parsed :arguments))
         (received-p (plist-member parsed :received_arguments))
         (result (plist-get parsed :result)))
    (unless (and (numberp version)
                 (= version e-tool-invocation-details-version)
                 (e-tool-invocation-details--valid-id-p call-id)
                 (e-tool-invocation-details--valid-id-p tool)
                 (e-tool-invocation-details--plist-p result))
      (signal 'e-tool-invocation-details-invalid (list parsed)))
    (unless (or (and arguments-p (not received-p))
                (and received-p (not arguments-p)))
      (signal 'e-tool-invocation-details-invalid (list parsed)))
    (let ((result-keys result)
          (content-encoding (plist-get result :content_encoding))
          (result-value nil))
      (while result-keys
        (unless (memq (pop result-keys)
                      '(:status :content :content_encoding :metadata))
          (signal 'e-tool-invocation-details-invalid (list parsed)))
        (pop result-keys))
      (unless (and (plist-member result :status)
                   (plist-member result :content)
                   (plist-member result :metadata))
        (signal 'e-tool-invocation-details-invalid (list parsed)))
      (when (and content-encoding
                 (not (and
                       (equal content-encoding
                              e-tool-invocation-details--base64-content-encoding)
                       (stringp (plist-get result :content)))))
        (signal 'e-tool-invocation-details-invalid (list parsed)))
      (setq result-value
            (list
             :status (plist-get result :status)
             :content
             (if content-encoding
                 (condition-case nil
                     (decode-coding-string
                      (base64-decode-string (plist-get result :content))
                      'utf-8-unix t)
                   (error
                    (signal 'e-tool-invocation-details-invalid (list parsed))))
               (plist-get result :content))
             :metadata (plist-get result :metadata)))
      (append (list :version version :tool-call-id call-id :tool tool)
              (list (if arguments-p :arguments :received-arguments)
                    (if arguments-p
                        (plist-get parsed :arguments)
                      (plist-get parsed :received_arguments))
                    :result result-value)))))

(defun e-tool-invocation-details-encode (document)
  "Validate DOCUMENT and encode canonical UTF-8 JSON text."
  (e-json-serialize
   (e-tool-invocation-details--document-wire
    (let* ((copy (copy-sequence document))
           (arguments-p (plist-member copy :arguments))
           (received-p (plist-member copy :received-arguments)))
      (when arguments-p
        (let ((value (plist-get copy :arguments)))
          (setq copy
                (plist-put copy :arguments
                           (if (null value)
                               nil
                             (e-tool-invocation-details--portable-value
                              value))))))
      (when received-p
        (let ((value (plist-get copy :received-arguments)))
          (setq copy
                (plist-put copy :received-arguments
                           (if (null value)
                               nil
                             (e-tool-invocation-details--portable-value
                              value))))))
      (setq copy
            (plist-put copy :result
                       (e-tool-invocation-details--result-document
                        (plist-get copy :result))))
      copy))))

(defun e-tool-invocation-details-decode (text)
  "Strictly decode temporary invocation details JSON TEXT."
  (unless (stringp text)
    (signal 'e-tool-invocation-details-invalid (list text)))
  (condition-case err
    (e-tool-invocation-details--document-plist
       (e-json-parse-string text))
    (e-json-error
     (signal 'e-tool-invocation-details-invalid (cdr err)))))

(defun e-tool-invocation-details--safe-fragment (value fallback)
  "Return a safe, bounded, collision-resistant path fragment.
FALLBACK is used for a missing VALUE.  A normalized or truncated value gets a
stable hash suffix so distinct provider identifiers cannot alias one artifact."
  (let* ((text (if (and (stringp value) (not (string-empty-p value)))
                   value fallback))
         (normalized (replace-regexp-in-string
                      "[^A-Za-z0-9._-]" "-" text))
         ;; Use a lowercase base so the spelling is stable across filesystems,
         ;; but suffix every case change so distinct provider identifiers never
         ;; alias on a case-insensitive volume.  Dot components are reserved by
         ;; the tmp resource path contract and therefore receive the same
         ;; normalized-and-hashed treatment.
         (canonical (downcase normalized))
         (reserved (member canonical '("." "..")))
         (changed (or reserved
                      (not (equal normalized text))
                      (not (equal canonical normalized))
                      (> (length normalized) 80)))
         (base (if reserved
                   "id"
                 (substring canonical 0 (min 64 (length canonical))))))
    (if changed
        (format "%s-%s"
                (if (string-empty-p base) "id" base)
                (substring (secure-hash 'sha256 text) 0 16))
      normalized)))

(defun e-tool-invocation-details-relative-name (turn-id call-id)
  "Return the safe session-tmp name for TURN-ID and CALL-ID."
  (format "tool-invocations/%s/%s.json"
          (e-tool-invocation-details--safe-fragment turn-id "turn")
          (e-tool-invocation-details--safe-fragment call-id "call")))

(defconst e-tool-invocation-details--stream-chunk-bytes (* 64 1024)
  "Maximum source bytes held while streaming file-backed detail content.")

(defun e-tool-invocation-details--write-bytes (path bytes &optional append)
  "Write unibyte BYTES to PATH, appending when APPEND is non-nil."
  (let ((coding-system-for-write 'binary)
        (select-safe-coding-system-function nil))
    (write-region bytes nil path append 'silent)))

(defun e-tool-invocation-details--json-escape-bytes (bytes)
  "Return bounded JSON string contents for unibyte BYTES.
UTF-8 bytes above the ASCII control range pass through unchanged; JSON syntax
and control bytes are escaped without decoding across chunk boundaries."
  (let ((start 0)
        pieces)
    (dotimes (index (length bytes))
      (let ((byte (aref bytes index)))
        (when (or (< byte 32) (= byte ?\") (= byte ?\\))
          (when (< start index)
            (push (substring bytes start index) pieces))
          (push (pcase byte
                  (8 "\\b") (9 "\\t") (10 "\\n")
                  (12 "\\f") (13 "\\r")
                  (?\" "\\\"") (?\\ "\\\\")
                  (_ (format "\\u%04x" byte)))
                pieces)
          (setq start (1+ index)))))
    (when (< start (length bytes))
      (push (substring bytes start) pieces))
    (apply #'concat (nreverse pieces))))

(defun e-tool-invocation-details--utf8-prefix (bytes final-p)
  "Validate unibyte BYTES and return its incomplete UTF-8 suffix.
Return nil for invalid UTF-8.  FINAL-P makes an incomplete suffix invalid."
  (let ((index 0)
        (length (length bytes))
        valid)
    (catch 'invalid
      (while (< index length)
        (let* ((first (aref bytes index))
               (width
                (cond ((< first #x80) 1)
                      ((<= #xc2 first #xdf) 2)
                      ((<= #xe0 first #xef) 3)
                      ((<= #xf0 first #xf4) 4)
                      (t (throw 'invalid nil)))))
          (when (> (+ index width) length)
            (if final-p
                (throw 'invalid nil)
              (setq valid (substring bytes index)
                    index length)))
          (when (< index length)
            (let ((second (and (> width 1) (aref bytes (1+ index)))))
              (unless
                  (and
                   (or (= width 1) (<= #x80 second #xbf))
                   (or (/= first #xe0) (>= second #xa0))
                   (or (/= first #xed) (<= second #x9f))
                   (or (/= first #xf0) (>= second #x90))
                   (or (/= first #xf4) (<= second #x8f))
                   (cl-loop for offset from 2 below width
                            always (<= #x80 (aref bytes (+ index offset))
                                       #xbf)))
                (throw 'invalid nil))
              (setq index (+ index width))))))
      (cons t (or valid "")))))

(defun e-tool-invocation-details--source-utf8-p (source)
  "Return non-nil when SOURCE contains complete, strictly valid UTF-8."
  (let ((offset 0)
        (size (file-attribute-size (file-attributes source)))
        (carry "")
        valid)
    (catch 'invalid
      (while (< offset size)
        (let ((end (min size (+ offset
                                e-tool-invocation-details--stream-chunk-bytes))))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally source nil offset end)
            (setq valid
                  (e-tool-invocation-details--utf8-prefix
                   (concat carry (buffer-string)) (= end size))))
          (unless valid (throw 'invalid nil))
          (setq carry (cdr valid)
                offset end)))
      (and (string-empty-p carry) t))))

(defun e-tool-invocation-details--stream-json-string (source destination)
  "Append SOURCE as escaped UTF-8 JSON string content to DESTINATION."
  (unless (and (stringp source) (file-regular-p source) (file-readable-p source))
    (signal 'e-tool-invocation-details-invalid
            (list "File-backed invocation content is unavailable")))
  (let ((offset 0)
        (size (file-attribute-size (file-attributes source))))
    (while (< offset size)
      (let ((end (min size (+ offset
                              e-tool-invocation-details--stream-chunk-bytes))))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally source nil offset end)
          (e-tool-invocation-details--write-bytes
           destination
           (e-tool-invocation-details--json-escape-bytes (buffer-string))
           t))
        (setq offset end)))))

(defun e-tool-invocation-details--stream-base64 (source destination)
  "Append SOURCE as one unpadded-line base64 string to DESTINATION."
  (let ((offset 0)
        (size (file-attribute-size (file-attributes source)))
        ;; Every non-final chunk must end on a base64 quantum boundary.
        (chunk-size (* 3 (/ e-tool-invocation-details--stream-chunk-bytes 3))))
    (while (< offset size)
      (let ((end (min size (+ offset chunk-size))))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally source nil offset end)
          (e-tool-invocation-details--write-bytes
           destination (base64-encode-string (buffer-string) t) t))
        (setq offset end)))))

(defun e-tool-invocation-details--stream-document
    (destination document carrier)
  "Write DOCUMENT with file-backed CARRIER content to DESTINATION."
  (unless (e-tools-file-content-valid-p carrier)
    (signal 'e-tool-invocation-details-invalid (list carrier)))
  (let* ((sentinel
          (format "__e_file_content_%s__"
                  (secure-hash
                   'sha256
                   (format "%s:%s" (e-tools-file-content-path carrier)
                           (float-time)))))
         (copy (copy-sequence document))
         (result (copy-sequence (plist-get copy :result)))
         (utf8-p
          (e-tool-invocation-details--source-utf8-p
           (e-tools-file-content-path carrier)))
         (encoded-sentinel (e-json-serialize sentinel))
         encoded start finish)
    (setq result (plist-put result :content sentinel))
    (unless utf8-p
      (setq result
            (plist-put result :content-encoding
                       e-tool-invocation-details--base64-content-encoding)))
    (setq copy (plist-put copy :result result))
    (setq encoded (e-tool-invocation-details-encode copy))
    (setq start (string-match (regexp-quote encoded-sentinel) encoded))
    (unless (and start
                 (not (string-match (regexp-quote encoded-sentinel)
                                    encoded (+ start (length encoded-sentinel)))))
      (signal 'e-tool-invocation-details-invalid
              (list "Invocation detail stream placeholder is ambiguous")))
    (setq finish (+ start (length encoded-sentinel)))
    (e-tool-invocation-details--write-bytes
     destination (encode-coding-string (substring encoded 0 (1+ start))
                                       'utf-8-unix))
    (if utf8-p
        (e-tool-invocation-details--stream-json-string
         (e-tools-file-content-path carrier) destination)
      (e-tool-invocation-details--stream-base64
       (e-tools-file-content-path carrier) destination))
    (e-tool-invocation-details--write-bytes
     destination
     (encode-coding-string (substring encoded (1- finish)) 'utf-8-unix)
     t)))

(defun e-tool-invocation-details--document
    (call result &optional rejected-p received-arguments)
  "Build the portable invocation document for CALL and semantic RESULT."
  (let* ((rejected (and rejected-p t))
         (document (list :version e-tool-invocation-details-version
                         :tool-call-id (plist-get call :id)
                         :tool (plist-get call :name)
                         :result result)))
    (unless (and (e-tool-invocation-details--valid-id-p (plist-get call :id))
                 (e-tool-invocation-details--valid-id-p (plist-get call :name)))
      (signal 'e-tool-invocation-details-invalid (list call)))
    (setq document
          (append document
                  (list (if rejected :received-arguments :arguments)
                        (if rejected received-arguments
                          (plist-get call :arguments)))))
    document))

(defun e-tool-invocation-details-write
    (harness session-id turn-id call result &optional rejected-p received-arguments)
  "Write one invocation detail document and return its tmp URI.
CALL is the normalized provider-neutral call at the semantic boundary; RESULT
is the complete semantic result before presentation-only truncation.  When
REJECTED-P is non-nil, RECEIVED-ARGUMENTS is kept only in this detached archive
and is never attached to CALL or its transcript metadata."
  (unless (and harness (stringp session-id)
               (not (string-empty-p session-id)))
    (signal 'e-tool-invocation-details-invalid
            (list "Invocation details require a session")))
  (let* ((document (e-tool-invocation-details--document
                    call result rejected-p received-arguments))
         (relative-name (e-tool-invocation-details-relative-name
                         turn-id (plist-get call :id)))
         (content (plist-get result :content)))
    (if (e-tools-file-content-p content)
        (e-session-tmp-write-generated
         harness session-id relative-name
         (lambda (path)
           (e-tool-invocation-details--stream-document path document content)))
      (e-session-tmp-write
       harness session-id relative-name
       (e-tool-invocation-details-encode document)))))

(defun e-tool-invocation-details--post-tool-call (result context)
  "Archive semantic RESULT once at the explicit invocation-details stage."
  (let* ((call (or (plist-get context :archival-call)
                   (plist-get context :tool-call)))
         (registry (plist-get context :tools))
         (registered (and (e-tools-registry-p registry)
                          (gethash (plist-get call :name)
                                   (e-tools-registry-tools registry))))
         (archive-p (and call
                         (not (plist-get context :nested))
                         (plist-get context :harness)
                         (or (null registry) registered))))
    (if (not archive-p)
        result
      (let* ((uri (e-tool-invocation-details-write
                   (plist-get context :harness)
                   (plist-get context :session-id)
                   (plist-get context :turn-id)
                   call
                   result
                   (when (plist-member context :archival-rejected-p)
                     (plist-get context :archival-rejected-p))
                   (when (plist-member context :archival-received-arguments)
                     (plist-get context :archival-received-arguments))))
             (metadata (copy-sequence (plist-get result :metadata)))
             (copy (copy-sequence result)))
        (when (e-tools-file-content-p (plist-get result :content))
          (e-tools-file-content-dispose (plist-get result :content)))
        ;; The harness passes this fresh slot to presentation only after the
        ;; archive write succeeds.  Presentation must not trust a URI copied
        ;; into an arbitrary result metadata plist.
        (plist-put context :invocation-details-uri uri)
        ;; `plist-put' returns a new list when METADATA is nil; assign both
        ;; results so a result without prior metadata still receives the URI.
        (setq metadata (plist-put metadata :invocation-details-uri uri))
        (setq copy (plist-put copy :metadata metadata))
        copy))))

(defun e-tool-invocation-details-capability-create ()
  "Return the harness-owned invocation-details capability."
  (e-capability-create
   :id 'tool-invocation-details
   :name "Tool Invocation Details"
   :hooks
   (list (e-hook-create
          :id "40-tool-invocation-details"
          :point :invocation-details
          :handler #'e-tool-invocation-details--post-tool-call))))

(provide 'e-tool-invocation-details)

;;; e-tool-invocation-details.el ends here
