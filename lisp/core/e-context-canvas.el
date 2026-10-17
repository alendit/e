;;; e-context-canvas.el --- Exact canvas capture and patch policy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral, linear operations for captured canvas revisions.  This
;; module does not read resources or retain accepted request state.

;;; Code:

(require 'e-context)
(require 'e-resources)
(require 'subr-x)

(defconst e-context-canvas-patch-limit 16
  "Maximum number of exact patches retained after one full snapshot.")

(defun e-context-canvas-utf8-bytes (text)
  "Return the UTF-8 payload byte count for TEXT."
  (string-bytes (encode-coding-string (substring-no-properties text)
                                      'utf-8 t)))

(defun e-context-canvas-revision (text)
  "Return the uppercase SHA-256 revision for complete TEXT."
  (upcase (secure-hash 'sha256 (substring-no-properties text))))

(defun e-context-canvas-revision-check (uri current-text expected &optional root)
  "Check EXPECTED against current complete text for parsed URI.
Return nil when no guard applies, (:revision REVISION) on success, or a
bounded conflict result.  ROOT is the file-method root for relative aliases."
  (let* ((context e-resource-operation-revision-context)
         (targets (plist-get context :attached-canvas-targets))
         (identities (e-context-canvas-resource-identities
                      (plist-get uri :uri) root))
         (attached-p
          (seq-some
           (lambda (target)
             (e-context-canvas-identities-intersect-p
              identities (plist-get target :target-identities)))
           targets))
         (expected-present
          (or (plist-get context :expected-revision-present)
              (stringp expected)))
         (required-p (or attached-p expected-present)))
    (when required-p
      (let ((revision (e-context-canvas-revision current-text)))
        (if (and expected-present (stringp expected)
                 (equal expected revision))
            (list :revision revision)
          (list :status "conflict"
                :currentRevision revision
                :message
                (format "Canvas revision conflict. Read the complete resource and reconsider the change; currentRevision=%s."
                        revision)))))))

(cl-defun e-context-canvas-capture-create (&key uri label id content
                                                target-identities)
  "Capture CONTENT once as a property-free canvas revision for URI.
ID and TARGET-IDENTITIES are request-time resource identity hints."
  (unless (stringp content)
    (signal 'wrong-type-argument (list 'stringp content)))
  (let* ((text (substring-no-properties content))
         (revision (upcase (secure-hash 'sha256 text)))
         (source (e-context-source-create
                  :uri uri :label label :content text
                  :content-sha256 revision
                  :source-kind 'current-state-attachment
                  :provider 'chat-session)))
    (plist-put source :canvas-text text)
    (plist-put source :canvas-revision revision)
    (plist-put source :canvas-payload-bytes
               (e-context-canvas-utf8-bytes text))
    (when id (plist-put source :canvas-id id))
    (when target-identities
      (plist-put source :canvas-target-identities
                 (copy-tree target-identities t)))
    source))

(defun e-context-canvas-escape-attribute (value)
  "Escape VALUE for a canvas context attribute."
  (let ((text (format "%s" (or value ""))))
    (setq text (replace-regexp-in-string "&" "&amp;" text t t))
    (setq text (replace-regexp-in-string "\"" "&quot;" text t t))
    (setq text (replace-regexp-in-string "<" "&lt;" text t t))
    (replace-regexp-in-string ">" "&gt;" text t t)))

(defun e-context-canvas--attributes (source extra)
  "Build ordered XML-like attributes for SOURCE and EXTRA pairs."
  (let ((values (list (cons "id" (plist-get source :canvas-id))
                      (cons "uri" (plist-get source :uri))
                      (cons "label" (plist-get source :label)))))
    (dolist (pair extra)
      (setq values (append values (list pair))))
    (setq values
          (append values
                  (list (cons "evidence" (plist-get source :handle)))))
    (mapconcat (lambda (pair)
                 (format "%s=\"%s\""
                         (car pair)
                         (e-context-canvas-escape-attribute (cdr pair))))
               values
               " ")))

(defun e-context-canvas-snapshot-payload (source)
  "Return the complete model-facing snapshot payload for SOURCE."
  (format "<canvas %s revision=\"%s\">\n%s\n</canvas>"
          (e-context-canvas--attributes source nil)
          (e-context-canvas-escape-attribute
           (plist-get source :canvas-revision))
          (plist-get source :canvas-text)))

(defun e-context-canvas-patch-payload (source base-revision patch)
  "Return the model-facing exact PATCH payload for SOURCE."
  (format "<canvas-change %s revision=\"%s\" baseRevision=\"%s\" start=\"%d\" end=\"%d\">\n%s\n</canvas-change>"
          (e-context-canvas--attributes source nil)
          (e-context-canvas-escape-attribute
           (plist-get source :canvas-revision))
          (e-context-canvas-escape-attribute base-revision)
          (plist-get patch :start)
          (plist-get patch :end)
          (plist-get patch :replacement)))

(defun e-context-canvas-exact-patch (old-text new-text)
  "Return one exact replacement span transforming OLD-TEXT into NEW-TEXT.
The result uses zero-based, half-open character positions."
  (unless (and (stringp old-text) (stringp new-text))
    (signal 'wrong-type-argument (list 'stringp old-text new-text)))
  (let* ((old (substring-no-properties old-text))
         (new (substring-no-properties new-text))
         (old-length (length old))
         (new-length (length new))
         (prefix 0)
         (suffix 0))
    (while (and (< prefix old-length)
                (< prefix new-length)
                (= (aref old prefix) (aref new prefix)))
      (setq prefix (1+ prefix)))
    (while (and (< suffix (- old-length prefix))
                (< suffix (- new-length prefix))
                (= (aref old (- old-length suffix 1))
                   (aref new (- new-length suffix 1))))
      (setq suffix (1+ suffix)))
    (let ((patch (list :start prefix
                       :end (- old-length suffix)
                       :replacement
                       (substring new prefix (- new-length suffix)))))
      (unless (equal (e-context-canvas-apply-patch old patch) new)
        (error "Canvas patch failed exact reconstruction"))
      patch)))

(defun e-context-canvas-apply-patch (base-text patch)
  "Apply PATCH to BASE-TEXT after validating its half-open character span."
  (let* ((base (substring-no-properties base-text))
         (start (plist-get patch :start))
         (end (plist-get patch :end))
         (replacement (plist-get patch :replacement)))
    (unless (and (integerp start) (integerp end)
                 (<= 0 start) (<= start end) (<= end (length base))
                 (stringp replacement))
      (error "Invalid canvas replacement span"))
    (concat (substring base 0 start)
            (substring-no-properties replacement)
            (substring base end))))

(defun e-context-canvas--ultimate-base-buffer (buffer)
  "Return BUFFER's ultimate base buffer when it is a live buffer."
  (when (buffer-live-p buffer)
    (let ((base buffer)
          next)
      (while (and (buffer-live-p base)
                  (setq next (buffer-base-buffer base))
                  (not (eq next base)))
        (setq base next))
      (and (buffer-live-p base) base))))

(defun e-context-canvas--file-identity (path)
  "Return canonical file identity for PATH, including not-yet-created paths."
  (list :kind 'file
        :path (condition-case nil
                  (file-truename path)
                (file-error (expand-file-name path)))))

(defun e-context-canvas--buffer-identities (buffer)
  "Return BUFFER and ultimate base-buffer identities, plus its file alias."
  (when-let* ((base (e-context-canvas--ultimate-base-buffer buffer)))
    (let ((names (delete-dups (list (buffer-name buffer)
                                    (buffer-name base))))
          (file (buffer-local-value 'buffer-file-name base)))
      (append (mapcar (lambda (name) (list :kind 'buffer :name name)) names)
              (when file
                (list (e-context-canvas--file-identity file)))))))

(defun e-context-canvas-resource-identities (uri &optional root)
  "Return canonical identity hints for file:// or buffer:// URI.
ROOT resolves relative file paths."
  (let* ((parsed (e-resources-parse-uri uri))
         (scheme (plist-get parsed :scheme))
         (address (plist-get parsed :address))
         (identities nil))
    (pcase scheme
      ("file"
       (let* ((path (if (file-name-absolute-p address)
                        address
                      (expand-file-name address (or root default-directory))))
              (file-identity (e-context-canvas--file-identity path))
              (buffer (find-buffer-visiting (plist-get file-identity :path))))
         (setq identities
               (cons file-identity
                     (e-context-canvas--buffer-identities buffer)))))
      ("buffer"
       (setq identities (list (list :kind 'buffer :name address)))
       (when-let* ((buffer (get-buffer address)))
         (setq identities
               (append identities
                       (e-context-canvas--buffer-identities buffer)))))
      (_
       (push (list :kind 'uri :scheme scheme :address address) identities)))
    (delete-dups identities)))

(defun e-context-canvas-identities-intersect-p (left right)
  "Return non-nil when resource identity lists LEFT and RIGHT intersect."
  (seq-some (lambda (identity) (member identity right)) left))

(defun e-context-canvas-resource-targeted-p (uri &optional root)
  "Return non-nil when current trusted request attached URI as a canvas.
ROOT resolves relative file paths for this resource method."
  (let ((targets (plist-get e-resource-operation-revision-context
                            :attached-canvas-targets))
        (identities (e-context-canvas-resource-identities
                     (plist-get uri :uri) root)))
    (seq-some
     (lambda (target)
       (e-context-canvas-identities-intersect-p
        identities (plist-get target :target-identities)))
     targets)))

(provide 'e-context-canvas)

;;; e-context-canvas.el ends here
