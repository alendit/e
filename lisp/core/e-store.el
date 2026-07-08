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
(require 'e-resource-patterns)
(require 'e-resource-query)
(require 'e-resource-toc)
(require 'e-resources)
(require 'seq)
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

(defun e-store--root-address (uri)
  "Return e:// root address from parsed resource URI."
  (plist-get uri :address))

(defun e-store--entry-under-root-p (entry root-address)
  "Return non-nil when ENTRY is under ROOT-ADDRESS."
  (let ((address (concat (e-store-entry-capability entry)
                         "/"
                         (e-store-entry-path entry))))
    (or (string-empty-p root-address)
        (string= address root-address)
        (string-prefix-p (concat root-address "/") address))))

(defun e-store--entry-name (entry root-address)
  "Return ENTRY display name relative to ROOT-ADDRESS."
  (let ((address (concat (e-store-entry-capability entry)
                         "/"
                         (e-store-entry-path entry))))
    (cond
     ((string-empty-p root-address) address)
     ((string= address root-address) (e-store-entry-path entry))
     ((string-prefix-p (concat root-address "/") address)
      (substring address (1+ (length root-address))))
     (t address))))

(defun e-store--matching-entries
    (store uri &optional pattern case-sensitive)
  "Return STORE entries under parsed URI matching optional glob PATTERN."
  (let* ((root-address (e-store--root-address uri))
         (actual-pattern (or pattern "*"))
         (actual-case-sensitive (if (null case-sensitive) t case-sensitive))
         entries)
    (e-resource-pattern-compile-glob actual-pattern)
    (dolist (entry (e-store-list store) (nreverse entries))
      (let ((name (e-store--entry-name entry root-address)))
        (when (and (e-store--entry-under-root-p entry root-address)
                   (e-resource-pattern-glob-match-p
                    actual-pattern
                    name
                    actual-case-sensitive))
          (push entry entries))))))

(defun e-store--discovery-limit (limit)
  "Return normalized discovery LIMIT."
  (cond
   ((null limit) 100)
   ((and (numberp limit) (> limit 0)) (truncate limit))
   (t (signal 'wrong-type-argument (list 'positive-number-p limit)))))

(defun e-store--entry-resource (entry root-address)
  "Return resource result for ENTRY under ROOT-ADDRESS."
  (append
   (list :uri (e-store-entry-uri entry)
         :name (e-store--entry-name entry root-address)
         :kind 'resource)
   (when-let ((metadata (e-store-entry-metadata entry)))
     (list :metadata (copy-sequence metadata)))))

(defun e-store--query-field-functions ()
  "Return e:// resource query field functions."
  `(("name" . ,(lambda (resource) (plist-get resource :name)))
    ("uri" . ,(lambda (resource) (plist-get resource :uri)))))

(defun e-store--query-resources (resources &rest arguments)
  "Apply e:// query controls to RESOURCES using ARGUMENTS."
  (apply #'e-resource-query-apply
         resources
         "e"
         '("default" "name" "uri")
         nil
         :field-functions (e-store--query-field-functions)
         arguments))

(defun e-store-glob
    (store uri pattern limit case-sensitive &optional sort-by sort-order
           created-after created-before updated-after updated-before)
  "List STORE resources under parsed URI with PATTERN and LIMIT."
  (let* ((root-address (e-store--root-address uri))
         (actual-limit (e-store--discovery-limit limit))
         (resources (mapcar
                     (lambda (entry)
                       (e-store--entry-resource entry root-address))
                     (e-store--matching-entries
                      store
                      uri
                      pattern
                      case-sensitive)))
         (entries (e-store--query-resources
                   resources
                   :sort-by sort-by
                   :sort-order sort-order
                   :created-after created-after
                   :created-before created-before
                   :updated-after updated-after
                   :updated-before updated-before))
         (truncated (> (length entries) actual-limit)))
    (list :resources (vconcat (seq-take entries actual-limit))
          :truncated truncated)))

(defun e-store--current-line-text ()
  "Return current line text without properties."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun e-store--search-entry (entry query options)
  "Return ranked search matches for ENTRY and QUERY with OPTIONS."
  (e-resource-pattern-search-matches-in-text
   (e-store-entry-uri entry)
   (e-store-read-entry entry nil)
   query
   options
   (e-store-entry-path entry)))

(defun e-store-search (store uri query options)
  "Search STORE resources under parsed URI for QUERY with OPTIONS."
  (let* ((root-address (e-store--root-address uri))
         (actual-limit (e-resource-pattern-search-limit
                        (plist-get options :limit)))
         (entries (e-store--matching-entries
                   store
                   uri
                   (plist-get options :glob)
                   t))
         (entry-by-uri (make-hash-table :test 'equal))
         matches)
    (dolist (entry entries)
      (puthash (e-store-entry-uri entry) entry entry-by-uri))
    (setq entries
          (delq nil
                (mapcar (lambda (resource)
                          (gethash (plist-get resource :uri) entry-by-uri))
                        (e-resource-query-apply-search
                         (mapcar (lambda (entry)
                                   (e-store--entry-resource entry root-address))
                                 entries)
                         "e"
                         '("default" "name" "uri")
                         nil
                         options
                         (e-store--query-field-functions)))))
    (dolist (entry entries)
      (setq matches
            (append matches
                    (e-store--search-entry entry query options))))
    (let ((ranked (e-resource-pattern-rank-search-matches
                   matches (1+ actual-limit))))
      (list :matches (vconcat (seq-take ranked actual-limit))
            :truncated (> (length ranked) actual-limit)))))

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

(defun e-store-glob-resource-method (store)
  "Return a glob e:// resource method backed by STORE."
  (e-resource-method-create
   :scheme "e"
   :operation e-operation-glob
   :description "List capability-contributed in-memory resources."
   :uri-patterns '("e://<capability>"
                   "e://<capability>/<path>"
                   "e://")
   :handler (lambda (uri pattern limit case-sensitive sort-by sort-order
                         created-after created-before updated-after updated-before)
              (e-store-glob store uri pattern limit case-sensitive
                            sort-by sort-order created-after created-before
                            updated-after updated-before))))

(defun e-store-search-resource-method (store)
  "Return a search e:// resource method backed by STORE."
  (e-resource-method-create
   :scheme "e"
   :operation e-operation-search
   :description "Search capability-contributed in-memory resources."
   :uri-patterns '("e://<capability>"
                   "e://<capability>/<path>"
                   "e://")
   :handler (lambda (uri query options)
              (e-store-search store uri query options))))


(defun e-store-table-of-content-resource-method (store)
  "Return a table-of-content e:// resource method backed by STORE, if available."
  (when (e-resource-toc-available-p)
    (e-resource-method-create
     :scheme "e"
     :operation e-operation-table-of-content
     :description "Capability-contributed in-memory resources outlined by piping text to wot --stdin. Pass language when inference is ambiguous."
     :uri-patterns '("e://<capability>/skills/<skill>"
                     "e://<capability>/refs/<name>.md"
                     "e://<capability>/<path>")
     :handler (lambda (uri options)
                (e-resource-toc-run-content
                 (plist-get uri :uri)
                 (plist-get uri :address)
                 (e-store-read store (plist-get uri :uri) nil)
                 options))
     :work (e-resource-toc-content-work
            (lambda (work-arguments _context)
              (let ((uri (plist-get work-arguments :uri)))
                (list :uri (plist-get uri :uri)
                      :name (plist-get uri :address)
                      :content (e-store-read store (plist-get uri :uri) nil)
                      :options (car (plist-get work-arguments :operation-arguments)))))))))

(defun e-store-resource-methods (store)
  "Return a registration function for read-only e:// STORE methods."
  (lambda (registry)
    (dolist (method (delq nil
                           (list (e-store-resource-method store)
                                 (e-store-glob-resource-method store)
                                 (e-store-search-resource-method store)
                                 (e-store-table-of-content-resource-method store))))
      (e-resources-register registry method))))

(provide 'e-store)

;;; e-store.el ends here
