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
(require 'json)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-hooks)
(require 'e-session-tmp-resources)
(require 'e-tools)

(defconst e-tool-invocation-details-version 1
  "Version of the temporary tool invocation details document.")

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
  "Return a normalized JSON object entry for KEY and VALUE."
  (unless (or (stringp key) (symbolp key) (numberp key))
    (signal 'e-tool-invocation-details-nonportable (list key)))
  (cons (cond ((keywordp key) (substring (symbol-name key) 1))
              ((symbolp key) (symbol-name key))
              ((stringp key) key)
              (t (number-to-string key)))
        value))

(defun e-tool-invocation-details--portable-value
    (value &optional metadata stack)
  "Return a detached JSON-compatible VALUE.
When METADATA is non-nil, unsupported nested values are omitted from semantic
metadata rather than leaking live implementation objects.  STACK detects
cycles and prevents a malformed handler value from recursing indefinitely."
  (cond
   ((or (stringp value) (numberp value) (eq value t)
        (eq value :json-false) (null value))
    (if (stringp value) (copy-sequence value) value))
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
    (vconcat
     (cl-loop for item across value
              for normalized =
              (e-tool-invocation-details--portable-value
               item metadata (cons value stack))
              unless (eq normalized e-tool-invocation-details--omit)
              collect normalized)))
   ((hash-table-p value)
    (let ((source-nonempty (> (hash-table-count value) 0))
          entries)
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
                           (string< (car left) (car right)))))
      (cond
       (entries entries)
       ((and metadata source-nonempty)
        e-tool-invocation-details--omit)
       (t (make-hash-table :test 'equal)))))
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
                           (string< (car left) (car right)))))
      (cond
       (entries entries)
       ((and metadata source-nonempty)
        e-tool-invocation-details--omit)
       (t (make-hash-table :test 'equal)))))
   ((and (listp value) (cl-every #'consp value))
    (let ((source-nonempty (not (null value)))
          entries)
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
                           (string< (car left) (car right)))))
      (cond
       (entries entries)
       ((and metadata source-nonempty)
        e-tool-invocation-details--omit)
       (t (make-hash-table :test 'equal)))))
   ((listp value)
    (vconcat
     (cl-loop for item in value
              for normalized =
              (e-tool-invocation-details--portable-value
               item metadata (cons value stack))
              unless (eq normalized e-tool-invocation-details--omit)
              collect normalized)))
   (t
    (if metadata e-tool-invocation-details--omit
      (signal 'e-tool-invocation-details-nonportable (list value))))))

(defun e-tool-invocation-details--empty-object-p (value)
  "Return non-nil when VALUE is an encoded empty object."
  (and (hash-table-p value) (= (hash-table-count value) 0)))

(defun e-tool-invocation-details--valid-purpose-p (purpose)
  "Return non-nil when PURPOSE satisfies the invocation envelope contract."
  (and (stringp purpose)
       (<= (length purpose) 200)
       (not (string-match-p "[\n\r]" purpose))
       (not (string-empty-p (string-trim purpose)))))

(defun e-tool-invocation-details--valid-id-p (value)
  "Return non-nil when VALUE is a nonblank stable identifier."
  (and (stringp value) (not (string-empty-p value))))

(defun e-tool-invocation-details--result-document (result)
  "Return the portable semantic RESULT document."
  (unless (and (listp result)
               (plist-member result :status)
               (plist-member result :content))
    (signal 'e-tool-invocation-details-invalid (list result)))
  (let ((content (e-tool-invocation-details--portable-value
                  (plist-get result :content)))
        (metadata
         (e-tool-invocation-details--portable-value
          (or (plist-get result :metadata)
              (make-hash-table :test 'equal))
          t)))
    (list (cons "status"
                (e-tool-invocation-details--portable-value
                 (plist-get result :status)))
          (cons "content" content)
          (cons "metadata"
                (if (eq metadata e-tool-invocation-details--omit)
                    (make-hash-table :test 'equal)
                  metadata)))))

(defun e-tool-invocation-details--document-wire (document)
  "Validate DOCUMENT and return its canonical JSON object shape."
  (let* ((version (plist-get document :version))
         (call-id (plist-get document :tool-call-id))
         (tool (plist-get document :tool))
         (purpose-p (plist-member document :stated-purpose))
         (arguments-p (plist-member document :arguments))
         (received-p (plist-member document :received-arguments))
         (result (plist-get document :result))
         (allowed '(:version :tool-call-id :tool :stated-purpose :arguments
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
    (when (= (+ (if purpose-p 1 0) (if arguments-p 1 0)
                (if received-p 1 0))
             0)
      (signal 'e-tool-invocation-details-invalid (list document)))
    (unless (or (and purpose-p arguments-p (not received-p)
                 (e-tool-invocation-details--valid-purpose-p
                  (plist-get document :stated-purpose)))
                (and received-p (not arguments-p)
                     (or (not purpose-p)
                         (e-tool-invocation-details--valid-purpose-p
                          (plist-get document :stated-purpose)))))
      (signal 'e-tool-invocation-details-invalid (list document)))
    (let ((wire
           (list (cons "version" version)
                 (cons "tool_call_id" call-id)
                 (cons "tool" tool))))
      (when purpose-p
        (setq wire
              (append wire
                      (list (cons "stated_purpose"
                                  (plist-get document :stated-purpose))))))
      (let ((value (if arguments-p
                       (plist-get document :arguments)
                     (plist-get document :received-arguments))))
        (unless (or (null value)
                    (e-tool-invocation-details--empty-object-p value)
                    (listp value) (vectorp value) (hash-table-p value))
          (signal 'e-tool-invocation-details-nonportable (list value)))
        (setq wire
              (append wire
                      (list (cons (if arguments-p "arguments"
                                    "received_arguments")
                                  (e-tool-invocation-details--portable-value
                                   (or value (make-hash-table :test 'equal))))))))
      (append wire (list (cons "result" result))))))

(defun e-tool-invocation-details--document-plist (parsed)
  "Return strict Lisp DOCUMENT from parsed JSON PARSED."
  (unless (e-tool-invocation-details--plist-p parsed)
    (signal 'e-tool-invocation-details-invalid (list parsed)))
  (let ((allowed '(:version :tool_call_id :tool :stated_purpose :arguments
                   :received_arguments :result))
        (keys parsed))
    (while keys
      (unless (memq (pop keys) allowed)
        (signal 'e-tool-invocation-details-invalid (list parsed)))
      (pop keys)))
  (let* ((version (plist-get parsed :version))
         (call-id (plist-get parsed :tool_call_id))
         (tool (plist-get parsed :tool))
         (purpose-p (plist-member parsed :stated_purpose))
         (arguments-p (plist-member parsed :arguments))
         (received-p (plist-member parsed :received_arguments))
         (result (plist-get parsed :result)))
    (unless (and (numberp version)
                 (= version e-tool-invocation-details-version)
                 (e-tool-invocation-details--valid-id-p call-id)
                 (e-tool-invocation-details--valid-id-p tool)
                 (e-tool-invocation-details--plist-p result))
      (signal 'e-tool-invocation-details-invalid (list parsed)))
    (unless (or (and purpose-p arguments-p (not received-p))
                (and received-p (not arguments-p)))
      (signal 'e-tool-invocation-details-invalid (list parsed)))
    (when (and purpose-p
               (not (e-tool-invocation-details--valid-purpose-p
                     (plist-get parsed :stated_purpose))))
      (signal 'e-tool-invocation-details-invalid (list parsed)))
    (let ((result-keys result)
          (result-value nil))
      (while result-keys
        (unless (memq (pop result-keys) '(:status :content :metadata))
          (signal 'e-tool-invocation-details-invalid (list parsed)))
        (pop result-keys))
      (unless (and (plist-member result :status)
                   (plist-member result :content)
                   (plist-member result :metadata))
        (signal 'e-tool-invocation-details-invalid (list parsed)))
      (setq result-value
            (list :status (plist-get result :status)
                  :content (plist-get result :content)
                  :metadata (plist-get result :metadata)))
      (append (list :version version :tool-call-id call-id :tool tool)
              (when purpose-p
                (list :stated-purpose (plist-get parsed :stated_purpose)))
              (list (if arguments-p :arguments :received-arguments)
                    (if arguments-p
                        (plist-get parsed :arguments)
                      (plist-get parsed :received_arguments))
                    :result result-value)))))

(defun e-tool-invocation-details-encode (document)
  "Validate DOCUMENT and encode canonical UTF-8 JSON text."
  (json-encode
   (e-tool-invocation-details--document-wire
    (let* ((copy (copy-sequence document))
           (arguments-p (plist-member copy :arguments))
           (received-p (plist-member copy :received-arguments)))
      (when arguments-p
        (setq copy
              (plist-put copy :arguments
                         (e-tool-invocation-details--portable-value
                          (or (plist-get copy :arguments)
                              (make-hash-table :test 'equal))))))
      (when received-p
        (setq copy
              (plist-put copy :received-arguments
                         (e-tool-invocation-details--portable-value
                          (or (plist-get copy :received-arguments)
                              (make-hash-table :test 'equal))))))
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
       (json-parse-string text
                          :object-type 'plist
                          :array-type 'list
                          :null-object nil
                          :false-object :json-false))
    (json-parse-error
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

(defun e-tool-invocation-details--rejected-p (call result)
  "Return non-nil when CALL/RESULT represents a rejected invocation."
  (or (eq (plist-get (plist-get call :metadata) :purpose-status) 'invalid)
      (eq (plist-get (plist-get result :metadata) :error)
          'e-tools-invalid-stated-purpose)))

(defun e-tool-invocation-details--document
    (call result &optional rejected-p received-arguments)
  "Build the portable invocation document for CALL and semantic RESULT."
  (let* ((rejected (if (null rejected-p)
                       (e-tool-invocation-details--rejected-p call result)
                     rejected-p))
         (purpose (plist-get call :stated-purpose))
         (document (list :version e-tool-invocation-details-version
                         :tool-call-id (plist-get call :id)
                         :tool (plist-get call :name)
                         :result result)))
    (unless (and (e-tool-invocation-details--valid-id-p (plist-get call :id))
                 (e-tool-invocation-details--valid-id-p (plist-get call :name)))
      (signal 'e-tool-invocation-details-invalid (list call)))
    (if rejected
        (progn
          (when (e-tool-invocation-details--valid-purpose-p purpose)
            (setq document
                  (append document (list :stated-purpose purpose))))
          (setq document
                (append document
                        (list :received-arguments received-arguments))))
      (unless (e-tool-invocation-details--valid-purpose-p purpose)
        (signal 'e-tool-invocation-details-invalid (list call)))
      (setq document
            (append document
                    (list :stated-purpose purpose
                          :arguments (plist-get call :arguments)))))
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
         (json (e-tool-invocation-details-encode document))
         (relative-name (e-tool-invocation-details-relative-name
                         turn-id (plist-get call :id))))
    (e-session-tmp-write harness session-id relative-name json)))

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
