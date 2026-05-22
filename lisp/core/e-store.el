;;; e-store.el --- Capability-scoped in-memory resources for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The e store is a path-addressable, capability-scoped in-memory resource
;; store exposed through read-only e:// URIs.

;;; Code:

(require 'cl-lib)
(require 'e-operations)
(require 'e-resources)
(require 'subr-x)

(define-error 'e-store-invalid-uri "e store URI is invalid")
(define-error 'e-store-unknown-resource "e store resource is not registered")
(define-error 'e-store-duplicate-uri "e store URI is already registered")

(cl-defstruct (e-store-entry
               (:constructor e-store-entry--create
                             (&key capability path uri description
                                   content reader metadata)))
  capability
  path
  uri
  description
  content
  reader
  metadata)

(cl-defstruct (e-store (:constructor e-store-create))
  (entries (make-hash-table :test 'equal))
  (order nil))

(defun e-store--normalize-capability (capability)
  "Return URI-safe string name for CAPABILITY."
  (let ((name (cond
               ((symbolp capability) (symbol-name capability))
               ((stringp capability) capability)
               (t (signal 'wrong-type-argument
                          (list 'string-or-symbol-p capability))))))
    (when (or (string-empty-p name)
              (string-match-p "/" name))
      (signal 'e-store-invalid-uri
              (list (format "Invalid e store capability: %s" name))))
    name))

(defun e-store--normalize-path (path)
  "Return normalized resource PATH."
  (unless (stringp path)
    (signal 'wrong-type-argument (list 'stringp path)))
  (when (or (string-empty-p path)
            (string-prefix-p "/" path))
    (signal 'e-store-invalid-uri
            (list (format "Invalid e store path: %s" path))))
  path)

(defun e-store-uri (capability path)
  "Return the e:// URI for CAPABILITY and PATH."
  (format "e://%s/%s"
          (e-store--normalize-capability capability)
          (e-store--normalize-path path)))

(defun e-store-parse-uri (uri)
  "Parse URI as an e:// store URI."
  (let ((parsed (e-resources-parse-uri uri)))
    (unless (equal (plist-get parsed :scheme) "e")
      (signal 'e-store-invalid-uri
              (list (format "Not an e store URI: %s" uri))))
    (let ((address (plist-get parsed :address)))
      (unless (and (stringp address)
                   (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" address))
        (signal 'e-store-invalid-uri
                (list (format "Invalid e store URI: %s" uri))))
      (list :scheme "e"
            :capability (match-string 1 address)
            :path (match-string 2 address)
            :address address
            :uri uri))))

(cl-defun e-store-register
    (store capability path &key description content reader metadata)
  "Register resource PATH under CAPABILITY in STORE.
CONTENT may be a string, or READER may be a function accepting the store entry
and optional range."
  (unless (or (stringp content) (functionp reader))
    (signal 'wrong-type-argument (list 'string-or-function-p path)))
  (let* ((capability (e-store--normalize-capability capability))
         (path (e-store--normalize-path path))
         (uri (e-store-uri capability path)))
    (when (gethash uri (e-store-entries store))
      (signal 'e-store-duplicate-uri
              (list (format "Duplicate e store URI: %s" uri))))
    (let ((entry (e-store-entry--create
                  :capability capability
                  :path path
                  :uri uri
                  :description description
                  :content content
                  :reader reader
                  :metadata metadata)))
      (puthash uri entry (e-store-entries store))
      (setf (e-store-order store)
            (append (e-store-order store) (list uri)))
      entry)))

(defun e-store-list (store)
  "Return STORE entries in registration order."
  (let (entries)
    (dolist (uri (e-store-order store))
      (push (gethash uri (e-store-entries store)) entries))
    (nreverse entries)))

(defun e-store-read-entry (entry &optional range)
  "Return ENTRY content, passing RANGE to dynamic readers."
  (let ((content
         (if-let ((reader (e-store-entry-reader entry)))
             (funcall reader entry range)
           (e-store-entry-content entry))))
    (unless (stringp content)
      (signal 'wrong-type-argument (list 'stringp content)))
    content))

(defun e-store-read (store uri &optional range)
  "Read URI from STORE with optional RANGE."
  (let* ((parsed (e-store-parse-uri uri))
         (entry (gethash (plist-get parsed :uri) (e-store-entries store))))
    (unless entry
      (signal 'e-store-unknown-resource
              (list (format "Unknown e store URI: %s" uri))))
    (e-store-read-entry entry range)))

(defun e-store-resource-method (store)
  "Return a read-only e:// resource method backed by STORE."
  (e-resource-method-create
   :scheme "e"
   :operation e-operation-read
   :description
   (concat
    "Read capability-contributed in-memory resources. "
    "Skills conventionally live under e://<capability>/skills/<skill>; "
    "references under e://<capability>/refs/<name>.md.")
   :uri-patterns '("e://<capability>/skills/<skill>"
                   "e://<capability>/refs/<name>.md"
                   "e://<capability>/<path>")
   :handler (lambda (uri range)
              (e-store-read store (plist-get uri :uri) range))))

(provide 'e-store)

;;; e-store.el ends here
