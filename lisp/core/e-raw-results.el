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

(defvar e-raw-results--counter 0
  "Counter used to make generated raw-result names unique.")

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
         (path (e-raw-results--path name))
         (reference
          (list :uri (e-raw-results--uri name)
                :owner owner
                :storage 'raw-result-store
                :original-bytes (string-bytes content-text)
                :preview (plist-get preview-data :text)
                :preview-bytes (plist-get preview-data :shown-bytes)
                :preview-truncated (plist-get preview-data :truncated)
                :redaction-policy (or redaction-policy 'none)
                :cleanup-lifetime (or cleanup-lifetime 'raw-result-store))))
    (make-directory (file-name-directory path) t)
    (e-raw-results--write-file path content-text)
    (if metadata
        (append reference (list :metadata metadata))
      reference)))

(defun e-raw-results-read (uri)
  "Read raw-result URI and return its content."
  (let* ((name (e-raw-results--name-from-uri uri))
         (path (e-raw-results--path name)))
    (unless (file-exists-p path)
      (signal 'file-missing (list "Raw result does not exist" uri)))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents path))
      (buffer-string))))

(defun e-raw-results-cleanup-reference (reference)
  "Delete one raw-result REFERENCE.
REFERENCE may be a raw-result reference plist or a =raw-result://= URI string.
Return the deleted file path, or nil when REFERENCE is not raw-result backed or
the referenced file is already absent."
  (when-let* ((uri (e-raw-results--reference-uri reference))
              (name (e-raw-results--name-from-uri uri))
              (path (e-raw-results--path name)))
    (when (file-exists-p path)
      (delete-file path)
      path)))

(defun e-raw-results-cleanup-references (references)
  "Delete raw-result REFERENCES and return the deleted file paths."
  (delq nil (mapcar #'e-raw-results-cleanup-reference references)))

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
