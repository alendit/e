;;; e-raw-results.el --- Generic raw-result resources for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Generic raw-result resources are ephemeral text artifacts addressable through
;; raw-result:// URIs when no active harness session owns the full result.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-operations)
(require 'e-resources)
(require 'e-raw-results-storage)
(require 'e-raw-results-legacy)
(require 'e-tools)

(define-error 'e-raw-results-invalid-path
  "raw-result:// resource path is invalid")

(defcustom e-raw-results-directory
  (expand-file-name "e-raw-results/" temporary-file-directory)
  "Directory used for generic raw-result resources."
  :type 'directory
  :group 'e)

(defcustom e-raw-results-preview-bytes 4096
  "Default maximum preview bytes included in raw-result references."
  :type 'integer
  :group 'e)

(defcustom e-raw-results-default-max-age-seconds (* 24 60 60)
  "Default maximum idle age before generic raw-result files are expired."
  :type 'number
  :group 'e)

(defvar e-raw-results--counter 0
  "Counter used to make generated raw-result names unique.")

(defvar e-raw-results-storage nil
  "Optional runtime-level raw-result SQLite storage port.")

(defun e-raw-results-configure-storage (storage)
  "Install raw-result STORAGE, or nil for the legacy/default path."
  (unless (or (null storage) (e-raw-results-storage-p storage))
    (signal 'wrong-type-argument (list 'e-raw-results-storage-p storage)))
  (setq e-raw-results-storage storage))

(defun e-raw-results--safe-name (name)
  "Return NAME as a safe single-file raw-result name."
  (let* ((text (format "%s" name))
         (safe (replace-regexp-in-string "[^A-Za-z0-9._-]" "-" text)))
    (unless (and (not (string-empty-p safe))
                 (not (member safe '("." "..")))
                 (not (string-match-p "/" safe)))
      (signal 'e-raw-results-invalid-path
              (list (format "Invalid raw-result name: %S" name))))
    safe))

(defun e-raw-results--generated-name ()
  "Return a generated raw-result file name."
  (e-raw-results--safe-name
   (format "raw-%s-%d.txt"
           (replace-regexp-in-string "\\." "-" (number-to-string (float-time)))
           (cl-incf e-raw-results--counter))))

(defun e-raw-results--uri (name)
  "Return the raw-result URI for NAME."
  (format "raw-result://%s" (e-raw-results--safe-name name)))

(defun e-raw-results--name-from-uri (uri)
  "Return the raw-result file name from URI."
  (unless (and (stringp uri) (string-prefix-p "raw-result://" uri))
    (signal 'e-raw-results-invalid-path
            (list (format "Invalid raw-result URI: %S" uri))))
  (e-raw-results--safe-name (substring uri (length "raw-result://"))))

(defun e-raw-results--path (name)
  "Return the absolute storage path for NAME."
  (expand-file-name (e-raw-results--safe-name name)
                    (file-name-as-directory e-raw-results-directory)))

(defun e-raw-results--reference-uri (reference)
  "Return the raw-result URI from REFERENCE."
  (cond
   ((and (stringp reference)
         (string-prefix-p "raw-result://" reference))
    reference)
   ((and (listp reference)
         (eq (plist-get reference :storage) 'raw-result-store))
    (plist-get reference :uri))
   ((listp reference) nil)
   (t nil)))

(defun e-raw-results--write-file (path content)
  "Write CONTENT to PATH without interactive coding-system prompts."
  (let ((coding-system-for-write 'utf-8-unix)
        (select-safe-coding-system-function nil))
    (write-region content nil path nil 'silent)))

(defun e-raw-results--file-age-seconds (path now)
  "Return PATH idle age in seconds at NOW, or nil when PATH is unavailable."
  (when (and (stringp path) (file-exists-p path))
    (let ((attributes (file-attributes path)))
      (when attributes
        (- now
           (float-time
            (file-attribute-modification-time attributes)))))))

(cl-defun e-raw-results-write
    (&key id content owner redaction-policy cleanup-lifetime preview
          preview-bytes metadata)
  "Persist raw result CONTENT and return a bounded reference plist.
ID, when non-nil, names the stored resource; otherwise a unique name is
generated.  OWNER identifies the caller-visible owner of the result.
REDACTION-POLICY and CLEANUP-LIFETIME are metadata for consumers deciding how to
show or clean up the reference.  PREVIEW, when non-nil, is used as the bounded
model/display preview; otherwise CONTENT is previewed with
`e-tools-result-content-preview'."
  (let* ((name (e-raw-results--safe-name (or id (e-raw-results--generated-name))))
         (content-text (format "%s" content))
         (limit (max 0 (or preview-bytes
                           e-raw-results-preview-bytes)))
         (preview-data
          (e-tools-result-content-preview
           (or preview content-text)
           limit))
         (uri (e-raw-results--uri name))
         (created-at (float-time))
         (expires-at (+ created-at e-raw-results-default-max-age-seconds))
         (reference
          (list :uri uri
                :owner owner
                :storage 'raw-result-store
                :original-bytes (string-bytes content-text)
                :preview (plist-get preview-data :text)
                :preview-bytes (plist-get preview-data :shown-bytes)
                :preview-truncated (plist-get preview-data :truncated)
                :redaction-policy (or redaction-policy 'none)
                :cleanup-lifetime (or cleanup-lifetime 'raw-result-store)
                :expires-at expires-at)))
    (if e-raw-results-storage
        (let ((result
               (e-raw-results-storage-put
                e-raw-results-storage uri content-text
                (list :owner owner :redaction-policy redaction-policy
                      :cleanup-lifetime cleanup-lifetime :metadata metadata)
                created-at expires-at)))
          ;; Preserve the originally committed expiry on same-content dedupe;
          ;; reads never slide it.
          (plist-put reference :expires-at (plist-get result :expires-at)))
      (let ((path (e-raw-results--path name)))
        (make-directory (file-name-directory path) t)
        (e-raw-results--write-file path content-text)))
    (if metadata
        (append reference (list :metadata metadata))
      reference)))

(cl-defun e-raw-results-import-file
    (source &key id owner redaction-policy cleanup-lifetime preview
            preview-bytes original-bytes metadata)
  "Copy file-backed text SOURCE into the raw-result store.
The legacy file backend copies SOURCE without materializing it.  The SQLite
path reads the bounded text once, commits it, and only then disposes SOURCE.
PREVIEW is already bounded by the producer.  ORIGINAL-BYTES may be supplied
from streaming counters; otherwise it is read from file metadata."
  (unless (and (stringp source) (file-regular-p source))
    (signal 'file-missing (list "Raw result source does not exist" source)))
  (when e-raw-results-storage
    (let* ((content
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-unix))
                (insert-file-contents source))
              (buffer-string)))
           (result
            (e-raw-results-write
             :id id :content content :owner owner
             :redaction-policy redaction-policy
             :cleanup-lifetime cleanup-lifetime :preview preview
             :preview-bytes preview-bytes :metadata metadata)))
      ;; The file is only a producer-owned staging artifact.  Disposal is
      ;; ordered after the durable commit acknowledgement above.
      (delete-file source)
      (cl-return-from e-raw-results-import-file result)))
  (let* ((name (e-raw-results--safe-name (or id (e-raw-results--generated-name))))
         (limit (max 0 (or preview-bytes e-raw-results-preview-bytes)))
         (preview-data (e-tools-result-content-preview (or preview "") limit))
         (path (e-raw-results--path name))
         (reference
          (list :uri (e-raw-results--uri name)
                :owner owner
                :storage 'raw-result-store
                :original-bytes
                (or original-bytes
                    (file-attribute-size (file-attributes source)))
                :preview (plist-get preview-data :text)
                :preview-bytes (plist-get preview-data :shown-bytes)
                :preview-truncated t
                :redaction-policy (or redaction-policy 'none)
                :cleanup-lifetime (or cleanup-lifetime 'raw-result-store))))
    (make-directory (file-name-directory path) t)
    (copy-file source path t)
    (if metadata
        (append reference (list :metadata metadata))
      reference)))

(defun e-raw-results-read (uri)
  "Read raw-result URI and return its content."
  (if e-raw-results-storage
      (let ((result
             (e-raw-results-storage-read e-raw-results-storage uri)))
        (unless result
          (signal 'file-missing (list "Raw result does not exist" uri)))
        (plist-get result :content))
    (let* ((name (e-raw-results--name-from-uri uri))
           (path (e-raw-results--path name)))
      (unless (file-exists-p path)
        (signal 'file-missing (list "Raw result does not exist" uri)))
      (e-raw-results-legacy-decode-file path))))

(defun e-raw-results-cleanup-reference (reference)
  "Delete one raw-result REFERENCE.
REFERENCE may be a raw-result reference plist or a =raw-result://= URI string.
Return the deleted file path, or nil when REFERENCE is not raw-result backed or
the referenced file is already absent."
  (when-let* ((uri (e-raw-results--reference-uri reference))
              (name (e-raw-results--name-from-uri uri)))
    (if e-raw-results-storage
        (and (plist-get
              (e-raw-results-storage-delete e-raw-results-storage uri)
              :deleted)
             uri)
      (let ((path (e-raw-results--path name)))
        (when (file-exists-p path)
          (delete-file path)
          path)))))

(defun e-raw-results-cleanup-references (references)
  "Delete raw-result REFERENCES and return the deleted file paths."
  (delq nil (mapcar #'e-raw-results-cleanup-reference references)))

(defun e-raw-results-cleanup-expired (&optional max-age-seconds now)
  "Delete generic raw-result files idle longer than MAX-AGE-SECONDS.
MAX-AGE-SECONDS defaults to `e-raw-results-default-max-age-seconds'.  NOW
defaults to the current time.  Return the list of deleted file paths."
  (let* ((max-age (or max-age-seconds e-raw-results-default-max-age-seconds))
         (now (or now (float-time)))
         deleted)
    (if e-raw-results-storage
        (progn
          (ignore max-age)
          (plist-get
           (e-raw-results-storage-expire e-raw-results-storage now 256)
           :deleted))
      (when (and (numberp max-age)
                 (>= max-age 0)
                 (file-directory-p e-raw-results-directory))
        (dolist (name (directory-files e-raw-results-directory nil
                                       directory-files-no-dot-files-regexp))
          (let ((path (e-raw-results--path name)))
            (when (and (file-regular-p path)
                       (let ((age (e-raw-results--file-age-seconds path now)))
                         (or (null age) (> age max-age))))
              (delete-file path)
              (push path deleted)))))
      (nreverse deleted))))


(defun e-raw-results--register-resource-methods (registry &rest _context)
  "Register raw-result resource methods in REGISTRY."
  (e-resources-register
   registry
   (e-resource-method-create
    :scheme "raw-result"
    :operation e-operation-read
    :description "Read generic ephemeral raw tool result resources."
    :uri-patterns '("raw-result://<name>")
    :handler (lambda (parsed-uri _range)
               (e-raw-results-read (plist-get parsed-uri :uri)))))
  nil)

(defun e-raw-results-capability-create ()
  "Return the generic raw-result resource capability."
  (e-capability-create
   :id 'raw-result-resources
   :name "Raw Result Resources"
   :resource-methods
   (list (e-capability-resource-method-provider-create
          :handler #'e-raw-results--register-resource-methods))))

(provide 'e-raw-results)

;;; e-raw-results.el ends here
