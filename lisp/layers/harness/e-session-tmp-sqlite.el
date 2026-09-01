;;; e-session-tmp-sqlite.el --- SQLite tmp resource primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the SQLite physical primitives behind tmp:// resources.  URI policy,
;; edit semantics, glob/search matching, and resource methods remain in the
;; consumer module and use these narrow list/read/write/delete operations.

;;; Code:

(require 'cl-lib)
(require 'e-runtime-store)

(declare-function e-harness-sessions "e-harness")
(declare-function e-session-storage-sqlite-p "e-session-storage")
(declare-function e-session-storage-runtime-store "e-session-storage")
(declare-function e-session-tmp--lineage-id "e-session-tmp-resources")
(declare-function e-session-tmp--safe-relative-name "e-session-tmp-resources")
(declare-function e-session-tmp--uri "e-session-tmp-resources")
(declare-function e-session-tmp--reference-uri "e-session-tmp-resources")
(declare-function e-session-tmp--relative-name-from-uri "e-session-tmp-resources")

(defvar e-session-tmp-default-max-age-seconds)

(defun e-session-tmp--sqlite-store (harness)
  "Return HARNESS's opt-in runtime store, or nil."
  (when (and harness (fboundp 'e-harness-sessions)
             (fboundp 'e-session-storage-sqlite-p)
             (fboundp 'e-session-storage-runtime-store))
    (let ((sessions (e-harness-sessions harness)))
      (when (e-session-storage-sqlite-p sessions)
        (e-session-storage-runtime-store sessions)))))

(defun e-session-tmp--sqlite-context (harness session-id)
  "Return (RUNTIME LINEAGE-ID) for an SQLite-backed session."
  (when-let* ((runtime (e-session-tmp--sqlite-store harness)))
    (list runtime (e-session-tmp--lineage-id harness session-id))))

(defun e-session-tmp--sqlite-put
    (harness session-id relative-name content &optional metadata)
  "Commit CONTENT for RELATIVE-NAME through HARNESS's runtime store."
  (pcase-let* ((`(,runtime ,lineage-id)
                 (or (e-session-tmp--sqlite-context harness session-id)
                     (signal 'e-session-tmp-resources-missing-session
                             (list "SQLite tmp store is unavailable"))))
                (path (e-session-tmp--safe-relative-name relative-name)))
    (e-runtime-store-call
     runtime 'write
     (list :op 'resource-put :lineage-id lineage-id :session-id session-id
           :path path :content (format "%s" content) :metadata metadata
           :expires-at (+ (float-time)
                          e-session-tmp-default-max-age-seconds)))
    (e-session-tmp--uri path)))

(defun e-session-tmp--sqlite-get (harness session-id relative-name)
  "Return the current SQLite resource row for RELATIVE-NAME."
  (pcase-let ((`(,runtime ,lineage-id)
               (or (e-session-tmp--sqlite-context harness session-id)
                   (signal 'e-session-tmp-resources-missing-session
                           (list "SQLite tmp store is unavailable")))))
    (e-runtime-store-call
     runtime 'read
     (list :op 'resource-get :lineage-id lineage-id
           :path (e-session-tmp--safe-relative-name relative-name)))))

(defun e-session-tmp--sqlite-read-content (harness session-id relative-name)
  "Return paged SQLite content for RELATIVE-NAME, or nil if absent."
  (pcase-let ((`(,runtime ,lineage-id)
               (or (e-session-tmp--sqlite-context harness session-id)
                   (signal 'e-session-tmp-resources-missing-session
                           (list "SQLite tmp store is unavailable")))))
    (let ((path (e-session-tmp--safe-relative-name relative-name))
          (offset 0) parts page present)
      (while
          (progn
            (setq page
                  (e-runtime-store-call
                   runtime 'read
                   (list :op 'resource-read :lineage-id lineage-id :path path
                         :offset offset :limit 4096)))
            (when page
              (setq present t)
              (push (plist-get page :content) parts)
              (setq offset (plist-get page :next)))
            offset))
      (and present (apply #'concat (nreverse parts))))))

(defun e-session-tmp--sqlite-list (harness session-id &optional limit)
  "Return bounded SQLite resource rows for HARNESS SESSION-ID."
  (pcase-let ((`(,runtime ,lineage-id)
               (or (e-session-tmp--sqlite-context harness session-id)
                   (signal 'e-session-tmp-resources-missing-session
                           (list "SQLite tmp store is unavailable")))))
    (e-runtime-store-call
     runtime 'read
     (list :op 'resource-list :lineage-id lineage-id
           :limit (or limit 1024)))))

(defun e-session-tmp-sqlite-cleanup-lineage
    (harness session-id lineage-id)
  "Delete LINEAGE-ID when SESSION-ID owns it; return a handled pair."
  (when-let* ((runtime (e-session-tmp--sqlite-store harness)))
    (when (equal session-id lineage-id)
      (e-runtime-store-call
       runtime 'write
       (list :op 'resource-delete-lineage :lineage-id lineage-id)))
    (cons t (and (equal session-id lineage-id)
                 (format "sqlite:tmp:%s" lineage-id)))))

(defun e-session-tmp-sqlite-write-generated
    (harness session-id relative-name writer)
  "Materialize WRITER only long enough to ingest one SQLite BLOB."
  (when (e-session-tmp--sqlite-store harness)
    (let ((temporary (make-temp-file "e-session-sqlite-export-")))
      (unwind-protect
          (progn
            (funcall writer temporary)
            (unless (file-regular-p temporary)
              (signal 'file-missing
                      (list "Generated tmp resource is missing" temporary)))
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-unix))
                (insert-file-contents temporary))
              (cons t (e-session-tmp--sqlite-put
                       harness session-id relative-name (buffer-string)))))
        (when (file-exists-p temporary)
          (delete-file temporary))))))

(defun e-session-tmp-sqlite-delete-reference
    (harness session-id reference)
  "Delete SQLite REFERENCE and return a handled pair."
  (when-let* ((uri (e-session-tmp--reference-uri reference))
              (context (e-session-tmp--sqlite-context harness session-id)))
    (pcase-let ((`(,runtime ,lineage-id) context))
      (let ((result
             (e-runtime-store-call
              runtime 'write
              (list :op 'resource-delete :lineage-id lineage-id
                    :path (e-session-tmp--relative-name-from-uri uri)))))
        (cons t (and (plist-get result :deleted) uri))))))

(defun e-session-tmp-sqlite-reference-available-p
    (harness session-id reference)
  "Return a handled pair containing SQLite REFERENCE availability."
  (when (e-session-tmp--sqlite-store harness)
    (cons t
          (when-let* ((uri (e-session-tmp--reference-uri reference)))
            (and (e-session-tmp--sqlite-get
                  harness session-id
                  (e-session-tmp--relative-name-from-uri uri))
                 t)))))

(provide 'e-session-tmp-sqlite)

;;; e-session-tmp-sqlite.el ends here
