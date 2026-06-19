;;; e-session-tmp-resources.el --- Session-scoped tmp:// resources for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session temporary resources are ephemeral text artifacts addressable through
;; tmp:// URIs within the current harness session.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-operations)
(require 'e-resources)

(define-error 'e-session-tmp-resources-invalid-path
  "tmp:// resource path is invalid")
(define-error 'e-session-tmp-resources-missing-session
  "tmp:// resource access requires a harness session")
(define-error 'e-session-tmp-resources-edit-mismatch
  "tmp:// edit replacement does not match exactly")

(defvar e-session-tmp--roots (make-hash-table :test 'eq)
  "Session tmp root tables keyed by harness object.")

(defun e-session-tmp--require-session (harness session-id)
  "Signal unless HARNESS and SESSION-ID identify a session tmp root."
  (unless (and harness (stringp session-id) (not (string-empty-p session-id)))
    (signal 'e-session-tmp-resources-missing-session
            (list "tmp:// resources require an active harness session"))))

(defun e-session-tmp--session-table (harness)
  "Return the tmp root table for HARNESS."
  (or (gethash harness e-session-tmp--roots)
      (puthash harness (make-hash-table :test 'equal) e-session-tmp--roots)))

(defun e-session-tmp-directory (harness session-id)
  "Return the session tmp directory for HARNESS and SESSION-ID."
  (e-session-tmp--require-session harness session-id)
  (let ((table (e-session-tmp--session-table harness)))
    (or (gethash session-id table)
        (puthash session-id
                 (file-name-as-directory
                  (make-temp-file
                   (format "e-session-%s-" (secure-hash 'sha1 session-id))
                   t))
                 table))))

(defun e-session-tmp--safe-relative-name (relative-name)
  "Return safe RELATIVE-NAME or signal."
  (unless (and (stringp relative-name)
               (not (string-empty-p relative-name))
               (not (string-match-p "\0" relative-name))
               (not (file-name-absolute-p relative-name)))
    (signal 'e-session-tmp-resources-invalid-path
            (list (format "Invalid tmp path: %S" relative-name))))
  (let ((parts (split-string relative-name "/" nil)))
    (when (or (null parts)
              (cl-some (lambda (part)
                         (or (string-empty-p part)
                             (member part '("." ".."))))
                       parts))
      (signal 'e-session-tmp-resources-invalid-path
              (list (format "Invalid tmp path: %S" relative-name)))))
  relative-name)

(defun e-session-tmp--path (harness session-id relative-name)
  "Return absolute path for RELATIVE-NAME under HARNESS SESSION-ID."
  (let* ((root (e-session-tmp-directory harness session-id))
         (safe-name (e-session-tmp--safe-relative-name relative-name))
         (path (expand-file-name safe-name root)))
    (unless (file-in-directory-p path root)
      (signal 'e-session-tmp-resources-invalid-path
              (list (format "Invalid tmp path: %S" relative-name))))
    path))

(defun e-session-tmp--uri (relative-name)
  "Return tmp URI for RELATIVE-NAME."
  (format "tmp://%s" (e-session-tmp--safe-relative-name relative-name)))

(defun e-session-tmp--write-file (path content)
  "Write CONTENT to PATH as UTF-8 without a coding-system prompt.
Content may contain eight-bit bytes that the buffer's detected coding cannot
encode; without these bindings `write-region' would invoke
`select-safe-coding-system' and block on the interactive coding-system picker."
  (let ((coding-system-for-write 'utf-8-unix)
        (select-safe-coding-system-function nil))
    (write-region content nil path nil 'silent)))

(defun e-session-tmp-write (harness session-id relative-name content)
  "Write CONTENT to RELATIVE-NAME in HARNESS SESSION-ID and return tmp URI."
  (let ((path (e-session-tmp--path harness session-id relative-name)))
    (make-directory (file-name-directory path) t)
    (e-session-tmp--write-file path (format "%s" content))
    (e-session-tmp--uri relative-name)))

(defun e-session-tmp-file-path (harness session-id relative-name)
  "Return an absolute file path for RELATIVE-NAME in HARNESS SESSION-ID.
The parent directory is created.  The returned path is inside the session tmp
root and is suitable for streaming writes."
  (let ((path (e-session-tmp--path harness session-id relative-name)))
    (make-directory (file-name-directory path) t)
    path))

(defun e-session-tmp--read-file (path range)
  "Read text PATH, honoring optional line RANGE."
  (with-temp-buffer
    (insert-file-contents path)
    (if (not range)
        (buffer-string)
      (let ((unit (plist-get range :unit))
            (start (plist-get range :start))
            (end (plist-get range :end)))
        (unless (and (equal unit "line")
                     (integerp start)
                     (> start 0)
                     (or (null end)
                         (and (integerp end)
                              (>= end start))))
          (signal 'wrong-type-argument (list 'line-range-p range)))
        (goto-char (point-min))
        (forward-line (1- start))
        (let ((beg (point)))
          (if end
              (forward-line (1+ (- end start)))
            (goto-char (point-max)))
          (buffer-substring-no-properties beg (point)))))))

(defun e-session-tmp--count-occurrences (content needle)
  "Return count of NEEDLE in CONTENT."
  (let ((count 0)
        (start 0))
    (while (string-match (regexp-quote needle) content start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun e-session-tmp--edit-field (edit field)
  "Return EDIT FIELD, accepting camelCase operation arguments."
  (or (plist-get edit field)
      (plist-get edit (pcase field
                        (:old-text :oldText)
                        (:new-text :newText)
                        (_ field)))))

(defun e-session-tmp--apply-edits (content edits uri)
  "Apply exact EDITS to CONTENT for URI."
  (let ((new-content content)
        (index 0))
    (dolist (edit edits)
      (let ((old-text (e-session-tmp--edit-field edit :old-text))
            (new-text (e-session-tmp--edit-field edit :new-text)))
        (unless (and (stringp old-text)
                     (not (string-empty-p old-text))
                     (stringp new-text))
          (signal 'e-session-tmp-resources-edit-mismatch
                  (list (format "Invalid edit at index %d for %s" index uri))))
        (pcase (e-session-tmp--count-occurrences new-content old-text)
          (1
           (string-match (regexp-quote old-text) new-content)
           (setq new-content
                 (concat (substring new-content 0 (match-beginning 0))
                         new-text
                         (substring new-content (match-end 0)))))
          (count
           (signal 'e-session-tmp-resources-edit-mismatch
                   (list (format "Expected one match for edit %d in %s, found %d"
                                 index uri count))))))
      (setq index (1+ index)))
    new-content))

(cl-defun e-session-tmp--register-resource-methods
    (registry &key harness session-id &allow-other-keys)
  "Register tmp:// resource methods in REGISTRY for HARNESS SESSION-ID."
  (e-resources-register
   registry
   (e-resource-method-create
    :scheme "tmp"
    :operation e-operation-read
    :description "Ephemeral session-scoped temporary text resources."
    :uri-patterns '("tmp://<relative-path>")
    :range-modes '("line")
    :handler (lambda (uri range)
               (e-session-tmp--read-file
                (e-session-tmp--path harness session-id (plist-get uri :address))
                range))))
  (e-resources-register
   registry
   (e-resource-method-create
    :scheme "tmp"
    :operation e-operation-write
    :description "Write an ephemeral session-scoped temporary text resource."
    :uri-patterns '("tmp://<relative-path>")
    :handler (lambda (uri content)
               (e-session-tmp-write
                harness
                session-id
                (plist-get uri :address)
                content))))
  (e-resources-register
   registry
   (e-resource-method-create
    :scheme "tmp"
    :operation e-operation-edit
    :description "Edit an existing ephemeral session-scoped temporary text resource."
    :uri-patterns '("tmp://<relative-path>")
    :handler (lambda (uri edits)
               (let* ((relative-name (plist-get uri :address))
                      (path (e-session-tmp--path harness session-id relative-name))
                      (content (e-session-tmp--read-file path nil))
                      (new-content
                       (e-session-tmp--apply-edits
                        content
                        edits
                        (plist-get uri :uri))))
                 (e-session-tmp--write-file path new-content)
                 (e-session-tmp--uri relative-name)))))
  nil)

(defun e-session-tmp-capability-create ()
  "Return the session tmp resource capability."
  (e-capability-create
   :id 'session-tmp-resources
   :name "Session Tmp Resources"
   :resource-methods
   (list (e-capability-resource-method-provider-create
          :handler #'e-session-tmp--register-resource-methods))))

(provide 'e-session-tmp-resources)

;;; e-session-tmp-resources.el ends here
