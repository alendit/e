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
(require 'e-tools)
(require 'e-work)

(define-error 'e-raw-results-invalid-path
  "raw-result:// resource path is invalid")
(define-error 'e-raw-results-async-required
  "Persistent raw-result reads require asynchronous resource work"
  'e-raw-results-storage-error)
(define-error 'e-raw-results-too-large
  "Raw result exceeds the practical persistence limit"
  'e-raw-results-storage-error)

(defcustom e-raw-results-preview-bytes 4096
  "Default maximum preview bytes included in raw-result references."
  :type 'integer
  :group 'e)

(defcustom e-raw-results-default-max-age-seconds (* 24 60 60)
  "Default maximum idle age before generic raw-result files are expired."
  :type 'number
  :group 'e)

(defcustom e-raw-results-max-content-bytes (* 16 1024 1024)
  "Maximum UTF-8 bytes admitted for one persistent raw result.

This application limit matches the SQLite worker's row bound and, for file
imports, is checked from metadata before reading the staging file."
  :type 'integer
  :group 'e)

(defvar e-raw-results--counter 0
  "Counter used to make generated raw-result names unique.")

(defvar e-raw-results-storage nil
  "Runtime-level raw-result SQLite storage port.")

(defun e-raw-results-configure-storage (storage)
  "Install raw-result STORAGE, or nil while no runtime is active."
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

(defun e-raw-results--storage ()
  "Return configured raw storage or signal a targeted runtime error."
  (or e-raw-results-storage
      (signal 'e-raw-results-storage-error
              (list "Raw-result SQLite runtime is not configured"))))

(cl-defun e-raw-results-write
    (&key id content owner redaction-policy cleanup-lifetime preview
          preview-bytes metadata on-settle)
  "Persist raw result CONTENT and return a bounded reference plist.
ID, when non-nil, names the stored resource; otherwise a unique name is
generated.  OWNER identifies the caller-visible owner of the result.
REDACTION-POLICY and CLEANUP-LIFETIME are metadata for consumers deciding how to
show or clean up the reference.  PREVIEW, when non-nil, is used as the bounded
model/display preview; otherwise CONTENT is previewed with
`e-tools-result-content-preview'."
  (let* ((name (e-raw-results--safe-name (or id (e-raw-results--generated-name))))
         (content-text (format "%s" content))
         (content-bytes (string-bytes content-text))
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
                :original-bytes content-bytes
                :preview (plist-get preview-data :text)
                :preview-bytes (plist-get preview-data :shown-bytes)
                :preview-truncated (plist-get preview-data :truncated)
                :redaction-policy (or redaction-policy 'none)
                :cleanup-lifetime (or cleanup-lifetime 'raw-result-store)
                :expires-at expires-at)))
    (when (> content-bytes e-raw-results-max-content-bytes)
      (signal 'e-raw-results-too-large
              (list content-bytes e-raw-results-max-content-bytes)))
    (e-raw-results-storage-submit
     (e-raw-results--storage) 'write 'put
     (list uri content-text
           (list :owner owner :redaction-policy redaction-policy
                 :cleanup-lifetime cleanup-lifetime :metadata metadata)
           created-at expires-at)
     (lambda (result error)
       (if error
           (progn
             (message "Raw-result persistence failed for %s: %s"
                      uri (error-message-string error))
             (when on-settle (funcall on-settle nil error)))
         (when on-settle (funcall on-settle result nil)))))
    (if metadata
        (append reference (list :metadata metadata))
      reference)))

(cl-defun e-raw-results-import-file
    (source &key id owner redaction-policy cleanup-lifetime preview
            preview-bytes original-bytes metadata)
  "Ingest file-backed text SOURCE into the raw-result store.
The SQLite path reads the bounded text once, commits it, and only then
disposes SOURCE.
PREVIEW is already bounded by the producer.  ORIGINAL-BYTES may be supplied
from streaming counters; otherwise it is read from file metadata."
  (unless (and (stringp source) (file-regular-p source))
    (signal 'file-missing (list "Raw result source does not exist" source)))
  (let* ((source-bytes (or original-bytes
                           (file-attribute-size (file-attributes source)))))
    (when (> source-bytes e-raw-results-max-content-bytes)
      (signal 'e-raw-results-too-large
              (list source-bytes e-raw-results-max-content-bytes))))
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
           :preview-bytes preview-bytes :metadata metadata
           :on-settle
           (lambda (_stored error)
             ;; SOURCE is producer-owned staging and is disposed only after
             ;; the durable acknowledgement.
             (when (and (not error) (file-exists-p source))
               (delete-file source))))))
    result))

(defun e-raw-results-read (uri)
  "Read raw-result URI at an explicit blocking compatibility boundary."
  (when (e-raw-results-storage--submit-operation (e-raw-results--storage))
    (signal 'e-raw-results-async-required
            (list "Use the raw-result resource Work path" uri)))
  (let ((result (e-raw-results-storage-read (e-raw-results--storage) uri)))
    (unless result
      (signal 'file-missing (list "Raw result does not exist" uri)))
    (plist-get result :content)))

(defun e-raw-results-cleanup-reference (reference)
  "Delete one raw-result REFERENCE.
REFERENCE may be a raw-result reference plist or a =raw-result://= URI string.
Return the deleted URI, or nil when REFERENCE is not raw-result backed or the
referenced row is already absent."
  (when-let* ((uri (e-raw-results--reference-uri reference))
              (name (e-raw-results--name-from-uri uri)))
    (ignore name)
    (e-raw-results-storage-submit
     (e-raw-results--storage) 'write 'delete (list uri)
     (lambda (_result error)
       (when error
         (message "Raw-result cleanup failed for %s: %s"
                  uri (error-message-string error)))))
    uri))

(defun e-raw-results-cleanup-references (references)
  "Delete raw-result REFERENCES and return the deleted URIs."
  (delq nil (mapcar #'e-raw-results-cleanup-reference references)))

(defun e-raw-results-cleanup-expired (&optional max-age-seconds now)
  "Physically delete expired raw results at NOW.
MAX-AGE-SECONDS is accepted for API compatibility; expiry is fixed at commit."
  (let* ((max-age (or max-age-seconds e-raw-results-default-max-age-seconds))
         (now (or now (float-time)))
         (storage (e-raw-results--storage)))
    (ignore max-age)
    (e-raw-results-storage-submit
     storage 'write 'expire (list now 256)
     (lambda (_result error)
       (when error
         (message "Raw-result expiry cleanup failed: %s"
                  (error-message-string error)))))))

(defun e-raw-results--read-work-spec ()
  "Return the asynchronous resource work for one raw-result read."
  (e-work-spec-create
   :id "raw-result.read" :description "Read one bounded raw result."
   :execution 'cooperative :interactive-policy 'async :owner 'raw-results
   :runner
   (lambda (handle arguments _context)
     (condition-case err
         (let* ((parsed (plist-get arguments :uri))
                (uri (plist-get parsed :uri))
                (storage (e-raw-results--storage))
                request)
           (setq request
                 (e-raw-results-storage-submit
                  storage 'read 'read (list uri (float-time))
                  (lambda (result error)
                    (cond
                     (error (e-work-fail handle error))
                     ((null result)
                      (e-work-fail
                       handle (list 'file-missing
                                    "Raw result does not exist" uri)))
                     (t (e-work-finish handle
                                       (plist-get result :content)))))))
           (setf (e-work-handle-cancel-function handle)
                 (lambda (_handle)
                   (e-runtime-store-cancel
                    (e-raw-results-storage-runtime storage) request)
                   t)))
       (error (e-work-fail handle err)))
     :deferred)))


(defun e-raw-results--register-resource-methods (registry &rest _context)
  "Register raw-result resource methods in REGISTRY."
  (e-resources-register
   registry
   (e-resource-method-create
    :scheme "raw-result"
    :operation e-operation-read
    :description "Read generic ephemeral raw tool result resources."
    :uri-patterns '("raw-result://<name>")
    :work (e-raw-results--read-work-spec)
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
