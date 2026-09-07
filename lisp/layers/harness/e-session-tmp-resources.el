;;; e-session-tmp-resources.el --- Session-scoped tmp:// resources for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session temporary resources are ephemeral text artifacts addressable through
;; tmp:// URIs within the current harness session.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-hooks)
(require 'e-operations)
(require 'e-resource-patterns)
(require 'e-resource-query)
(require 'e-request)
(require 'e-resources)
(require 'e-tools)
(require 'e-work)
(require 'e-session-tmp-sqlite)

(define-error 'e-session-tmp-resources-invalid-path
  "tmp:// resource path is invalid")
(define-error 'e-session-tmp-resources-missing-session
  "tmp:// resource access requires a harness session")
(define-error 'e-session-tmp-resources-edit-mismatch
  "tmp:// edit replacement does not match exactly")
(define-error 'e-session-tmp-resources-missing-command
  "tmp:// discovery command is not available")
(define-error 'e-session-tmp-resources-process-failed
  "tmp:// discovery command failed")
(define-error 'e-session-tmp-resources-async-required
  "Persistent tmp:// resources require detached resource Work")
(define-error 'e-session-tmp-resources-page-too-large
  "Persistent tmp:// resource exceeds the bounded visible page")

(declare-function e-harness-sessions "e-harness")
(declare-function e-harness-executing-session-state "e-harness-state")
(declare-function e-session-async-enabled-p "e-session-async")
(declare-function e-session-get "e-session")

(defvar e-session-tmp--roots (make-hash-table :test 'equal)
  "Session tmp root directories keyed by lineage id.
A lineage id defaults to a session's own id, so ordinary sessions keep an
isolated root.  Subagent children inherit their parent's lineage id, so an
entire subagent lineage -- even across different harness instances -- resolves
to one shared root.")

(defvar e-session-tmp--lineage-harnesses (make-hash-table :test 'equal)
  "Harnesses referencing each lineage root, keyed by lineage id.
Used to release a shared root only once every referencing harness has torn
down, so a child harness cleanup never deletes a root the parent still uses.")

(defcustom e-session-tmp-default-max-age-seconds (* 24 60 60)
  "Default maximum idle age before session tmp roots are expired."
  :type 'number
  :group 'e)

(defcustom e-session-tmp-raw-result-preview-bytes 4096
  "Default maximum preview bytes included in raw-result references."
  :type 'integer
  :group 'e)

(defun e-session-tmp--require-session (harness session-id)
  "Signal unless HARNESS and SESSION-ID identify a session tmp root."
  (unless (and harness (stringp session-id) (not (string-empty-p session-id)))
    (signal 'e-session-tmp-resources-missing-session
            (list "tmp:// resources require an active harness session"))))

(defun e-session-tmp--lineage-id (harness session-id)
  "Return the tmp lineage id for HARNESS SESSION-ID.
The lineage id is the session's durable `:tmp-lineage-id' metadata when present,
so a subagent lineage shares one root; otherwise it is SESSION-ID, so ordinary
sessions stay isolated."
  (or (when (and harness (fboundp 'e-harness-sessions))
        (let* ((store (e-harness-sessions harness))
               (session
                (or (and (fboundp 'e-harness-executing-session-state)
                         (e-harness-executing-session-state harness session-id))
                    (unless (and (fboundp 'e-session-async-enabled-p)
                                 (e-session-async-enabled-p store))
                      (and (fboundp 'e-session-get)
                           (ignore-errors
                             (e-session-get store session-id)))))))
          (plist-get (plist-get session :metadata) :tmp-lineage-id)))
      session-id))

(defun e-session-tmp--reference-lineage (lineage-id harness)
  "Record that HARNESS references the root for LINEAGE-ID."
  (let ((harnesses (gethash lineage-id e-session-tmp--lineage-harnesses)))
    (unless (memq harness harnesses)
      (puthash lineage-id (cons harness harnesses)
               e-session-tmp--lineage-harnesses))))

(defun e-session-tmp--forget-lineage (lineage-id)
  "Forget LINEAGE-ID root and reference bookkeeping."
  (remhash lineage-id e-session-tmp--roots)
  (remhash lineage-id e-session-tmp--lineage-harnesses))

(defun e-session-tmp-directory (harness session-id)
  "Return the tmp directory for HARNESS SESSION-ID, keyed by lineage."
  (e-session-tmp--require-session harness session-id)
  (let ((lineage-id (e-session-tmp--lineage-id harness session-id)))
    (e-session-tmp--reference-lineage lineage-id harness)
    (or (gethash lineage-id e-session-tmp--roots)
        (puthash lineage-id
                 (file-name-as-directory
                  (make-temp-file
                   (format "e-session-%s-" (secure-hash 'sha1 lineage-id))
                   t))
                 e-session-tmp--roots))))

(cl-defun e-session-tmp-cleanup-session (harness session-id)
  "Delete tmp resources for HARNESS SESSION-ID's lineage root.
The shared lineage root is removed only when SESSION-ID is the lineage root
session, so cleaning up a subagent child never destroys a root its parent still
uses."
  (e-session-tmp--require-session harness session-id)
  (let* ((lineage-id (e-session-tmp--lineage-id harness session-id))
         (root (gethash lineage-id e-session-tmp--roots))
         (sqlite-result (e-session-tmp-sqlite-cleanup-lineage
                         harness session-id lineage-id)))
    (when sqlite-result
      (cl-return-from e-session-tmp-cleanup-session
        (cdr sqlite-result)))
    (when (equal session-id lineage-id)
      (e-session-tmp--forget-lineage lineage-id)
      (when (and root (file-directory-p root))
        (delete-directory root t)))
    root))

(defun e-session-tmp-cleanup-harness (harness)
  "Delete tmp resources whose lineage roots HARNESS still solely references.
A lineage root shared with another live harness is retained; HARNESS is only
dropped from its reference set."
  (let (drop)
    (maphash
     (lambda (lineage-id harnesses)
       (when (memq harness harnesses)
         (let ((remaining (delq harness (copy-sequence harnesses))))
           (if remaining
               (puthash lineage-id remaining e-session-tmp--lineage-harnesses)
             (push lineage-id drop)))))
     e-session-tmp--lineage-harnesses)
    (dolist (lineage-id drop)
      (let ((root (gethash lineage-id e-session-tmp--roots)))
        (e-session-tmp--forget-lineage lineage-id)
        (when (and root (file-directory-p root))
          (delete-directory root t)))))
  harness)

(defun e-session-tmp--root-age-seconds (root now)
  "Return ROOT idle age in seconds at NOW, or nil when ROOT is unavailable."
  (when (and (stringp root) (file-exists-p root))
    (let ((attributes (file-attributes root)))
      (when attributes
        (- now
           (float-time
            (file-attribute-modification-time attributes)))))))

(defun e-session-tmp--touch-root (root)
  "Mark ROOT as recently used."
  (when (and (stringp root) (file-directory-p root))
    (ignore-errors
      (set-file-times root))))

(defun e-session-tmp-cleanup-expired (&optional max-age-seconds now)
  "Delete lineage tmp roots idle longer than MAX-AGE-SECONDS.
MAX-AGE-SECONDS defaults to `e-session-tmp-default-max-age-seconds'.  NOW
defaults to the current time.  Return a list of deleted root directories."
  (let* ((max-age (or max-age-seconds e-session-tmp-default-max-age-seconds))
         (now (or now (float-time)))
         expired
         deleted)
    (when (and (numberp max-age) (>= max-age 0))
      (maphash
       (lambda (lineage-id root)
         (let ((age (e-session-tmp--root-age-seconds root now)))
           (when (or (null age) (> age max-age))
             (push (cons lineage-id root) expired))))
       e-session-tmp--roots)
      (dolist (entry expired)
        (let ((lineage-id (car entry))
              (root (cdr entry)))
          (e-session-tmp--forget-lineage lineage-id)
          (when (and root (file-directory-p root))
            (delete-directory root t)
            (push root deleted)))))
    (nreverse deleted)))

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
         (path (e-session-tmp--path-in-root root relative-name)))
    path))

(defun e-session-tmp--path-in-root (root relative-name)
  "Return absolute path for RELATIVE-NAME inside existing ROOT."
  (let* ((safe-name (e-session-tmp--safe-relative-name relative-name))
         (path (expand-file-name safe-name root)))
    (unless (file-in-directory-p path root)
      (signal 'e-session-tmp-resources-invalid-path
              (list (format "Invalid tmp path: %S" relative-name))))
    path))

(defun e-session-tmp--uri (relative-name)
  "Return tmp URI for RELATIVE-NAME."
  (format "tmp://%s" (e-session-tmp--safe-relative-name relative-name)))

(defun e-session-tmp--clean-relative-path (path)
  "Return PATH without fd's defensive leading ./ prefix."
  (if (string-prefix-p "./" path)
      (substring path 2)
    path))

(defun e-session-tmp--relative-name-from-uri (uri)
  "Return the relative tmp resource name from URI."
  (unless (and (stringp uri) (string-prefix-p "tmp://" uri))
    (signal 'e-session-tmp-resources-invalid-path
            (list (format "Invalid tmp URI: %S" uri))))
  (substring uri (length "tmp://")))

(defun e-session-tmp--reference-uri (reference)
  "Return the tmp URI from raw-result REFERENCE."
  (cond
   ((stringp reference) reference)
   ((and (listp reference)
         (eq (plist-get reference :storage) 'session-tmp))
    (plist-get reference :uri))
   ((listp reference) nil)
   (t nil)))

(defun e-session-tmp--delete-empty-parents (root path)
  "Delete empty parent directories from PATH up to ROOT."
  (let ((root (file-name-as-directory (expand-file-name root)))
        (directory (file-name-directory (expand-file-name path))))
    (while (and directory
                (not (equal (file-name-as-directory directory) root))
                (file-in-directory-p directory root)
                (file-directory-p directory)
                (null (directory-files directory nil
                                       directory-files-no-dot-files-regexp)))
      (delete-directory directory)
      (setq directory (file-name-directory
                       (directory-file-name directory))))))

(defun e-session-tmp--discovery-limit (limit)
  "Return normalized discovery LIMIT."
  (cond
   ((null limit) 100)
   ((and (numberp limit) (> limit 0)) (truncate limit))
   (t (signal 'wrong-type-argument (list 'positive-number-p limit)))))

(defun e-session-tmp--find-executable (name &optional alternates)
  "Return executable NAME or one of ALTERNATES, or signal a clear error."
  (or (executable-find name)
      (cl-some #'executable-find alternates)
      (signal 'e-session-tmp-resources-missing-command
              (list (format "Missing executable: %s" name)))))

(defun e-session-tmp--reject-sync-in-hot-path (operation)
  "Reject synchronous session-tmp OPERATION from marked interactive hot paths."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error operation)))

(defun e-session-tmp--process-lines (program directory args &optional ok-statuses)
  "Run PROGRAM in DIRECTORY with ARGS and return output lines.
OK-STATUSES defaults to only zero."
  (e-session-tmp--reject-sync-in-hot-path 'e-session-tmp--process-lines)
  (let ((default-directory directory)
        (accepted (or ok-statuses '(0))))
    (with-temp-buffer
      (let ((status (apply #'process-file program nil (list t t) nil args)))
        (unless (member status accepted)
          (signal 'e-session-tmp-resources-process-failed
                  (list (format "%s failed with exit status %s: %s"
                                program
                                status
                                (string-trim (buffer-string)))))))
	      (split-string (buffer-string) "\n" t))))

(defun e-session-tmp--buffer-string (buffer)
  "Return BUFFER contents, or an empty string when BUFFER is dead."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-string))
    ""))

(defun e-session-tmp--buffer-lines (buffer)
  "Return non-empty lines from BUFFER."
  (split-string (e-session-tmp--buffer-string buffer) "\n" t))

(defun e-session-tmp--scope-relative-name (uri)
  "Return discovery scope name for parsed tmp URI."
  (let ((address (plist-get uri :address)))
    (if (string-empty-p address)
        "."
      (e-session-tmp--safe-relative-name address))))

(defun e-session-tmp--scope-path (harness session-id uri)
  "Return absolute discovery scope path for parsed tmp URI."
  (let ((address (plist-get uri :address)))
    (if (string-empty-p address)
        (e-session-tmp-directory harness session-id)
      (e-session-tmp--path harness session-id address))))

(defun e-session-tmp--result-name (path scope)
  "Return display name for PATH relative to SCOPE when possible."
  (if (and (file-directory-p scope)
           (file-in-directory-p path scope))
      (file-relative-name path scope)
    (file-name-nondirectory path)))

(defun e-session-tmp--file-metadata (path &optional query-metadata)
  "Return resource metadata for tmp PATH.
When QUERY-METADATA is non-nil, include sortable timestamp metadata."
  (let ((attributes (file-attributes path)))
    (append (list :bytes (file-attribute-size attributes))
            (when query-metadata
              (list :updated-at
                    (file-attribute-modification-time attributes))))))

(defun e-session-tmp--file-result (relative absolute scope &optional query-metadata)
  "Return tmp resource result for RELATIVE ABSOLUTE under SCOPE.
When QUERY-METADATA is non-nil, include sortable timestamp metadata."
  (list :uri (concat "tmp://" relative)
        :name (e-session-tmp--result-name absolute scope)
        :kind 'file
        :metadata (e-session-tmp--file-metadata absolute query-metadata)))

(defun e-session-tmp--listed-resource-result (row scope &optional query-metadata)
  "Return one resource result for physical ROW below SCOPE."
  (let* ((path (plist-get row :path))
         (name (if (or (string-empty-p scope) (equal scope "."))
                   path
                 (string-remove-prefix (concat (string-remove-suffix "/" scope) "/")
                                       path))))
    (list :uri (concat "tmp://" path) :name name :kind 'file
          :metadata
          (append (list :bytes (plist-get row :bytes))
                  (when query-metadata
                    (list :updated-at
                          (seconds-to-time (plist-get row :updated-at))))))))

(defun e-session-tmp--glob-listed-resources
    (rows uri pattern limit case-sensitive sort-by sort-order
          created-after created-before updated-after updated-before)
  "Apply tmp glob policy to bounded physical resource ROWS."
  (let* ((scope (e-session-tmp--scope-relative-name uri))
         (scope-prefix (unless (equal scope ".")
                         (string-remove-suffix "/" scope)))
         (actual-pattern (or pattern "*"))
         (actual-limit (e-session-tmp--discovery-limit limit))
         (advanced (or sort-by sort-order created-after created-before
                       updated-after updated-before))
         resources)
    (e-resource-pattern-compile-glob actual-pattern)
    (dolist (row rows)
      (let ((path (plist-get row :path)))
        (when (or (null scope-prefix)
                  (equal path scope-prefix)
                  (string-prefix-p (concat scope-prefix "/") path))
          (let ((resource
                 (e-session-tmp--listed-resource-result row scope advanced)))
            (when (e-resource-pattern-glob-match-p
                   actual-pattern (plist-get resource :name)
                   (if (null case-sensitive) t case-sensitive))
              (push resource resources))))))
    (setq resources
          (e-session-tmp--apply-query
           (nreverse resources) sort-by sort-order created-after created-before
           updated-after updated-before))
    (list :resources (vconcat (seq-take resources actual-limit))
          :truncated (> (length resources) actual-limit))))

(defun e-session-tmp--search-listed-resources
    (rows content-reader uri query options)
  "Apply tmp search policy to ROWS using CONTENT-READER by path."
  (let* ((glob-result
          (e-session-tmp--glob-listed-resources
           rows uri (plist-get options :glob)
           (or (plist-get options :resource-limit) 4096) t
           (plist-get options :resource-sort-by)
           (plist-get options :resource-sort-order)
           (plist-get options :created-after)
           (plist-get options :created-before)
           (plist-get options :updated-after)
           (plist-get options :updated-before)))
         (by-uri (make-hash-table :test 'equal))
         (limit (e-resource-pattern-search-limit (plist-get options :limit)))
         matches)
    (dolist (row rows)
      (puthash (concat "tmp://" (plist-get row :path)) row by-uri))
    (dolist (resource (append (plist-get glob-result :resources) nil))
      (let* ((row (gethash (plist-get resource :uri) by-uri))
             (content (funcall content-reader (plist-get row :path))))
        (setq matches
              (append
               matches
               (e-resource-pattern-search-matches-in-text
                (plist-get resource :uri) content
                query options (plist-get resource :name))))))
    (setq matches (e-resource-pattern-rank-search-matches
                   matches (1+ limit)))
    (list :matches (vconcat (seq-take matches limit))
          :truncated (> (length matches) limit))))

(defun e-session-tmp--query-field-functions ()
  "Return tmp:// resource query field functions."
  `(("name" . ,(lambda (resource) (plist-get resource :name)))
    ("uri" . ,(lambda (resource) (plist-get resource :uri)))
    ("updated-at" . ,(lambda (resource)
                         (plist-get (plist-get resource :metadata) :updated-at)))))

(defun e-session-tmp--apply-query
    (resources sort-by sort-order created-after created-before
               updated-after updated-before)
  "Apply tmp:// query controls to RESOURCES."
  (e-resource-query-apply
   resources
   "tmp"
   '("default" "name" "uri" "updated-at")
   '("updated-at")
   :sort-by sort-by
   :sort-order sort-order
   :created-after created-after
   :created-before created-before
   :updated-after updated-after
   :updated-before updated-before
   :field-functions (e-session-tmp--query-field-functions)))

(defun e-session-tmp--glob-single-result
    (scope scope-relative pattern case-sensitive &optional query-metadata)
  "Return a single tmp glob result for SCOPE, or nil if it does not match."
  (when (and (file-regular-p scope)
             (e-resource-pattern-glob-match-p
              pattern
              (file-name-nondirectory scope-relative)
              case-sensitive))
    (e-session-tmp--file-result scope-relative scope scope query-metadata)))

(cl-defun e-session-tmp--glob-resource
    (harness session-id uri pattern limit case-sensitive &optional sort-by sort-order
             created-after created-before updated-after updated-before)
  "List tmp resources under parsed URI with PATTERN and LIMIT."
  (when (e-session-tmp--sqlite-store harness)
    (cl-return-from e-session-tmp--glob-resource
      (signal 'e-session-tmp-resources-async-required
              (list "Persistent tmp:// glob must run as detachable Work"
                    (plist-get uri :uri)))))
  (let* ((root (e-session-tmp-directory harness session-id))
         (scope (e-session-tmp--scope-path harness session-id uri))
         (scope-relative (e-session-tmp--scope-relative-name uri))
         (actual-pattern (or pattern "*"))
         (fd-pattern (e-resource-pattern-glob-fd-candidate-pattern actual-pattern))
         (fd-max-depth (e-resource-pattern-glob-max-depth actual-pattern))
         (actual-limit (e-session-tmp--discovery-limit limit))
         (advanced (or sort-by sort-order created-after created-before
                       updated-after updated-before))
         (actual-case-sensitive (if (null case-sensitive) t case-sensitive)))
    (e-resource-pattern-compile-glob actual-pattern)
    (if (file-regular-p scope)
        (let* ((resources (if-let ((single (e-session-tmp--glob-single-result
                                            scope
                                            scope-relative
                                            actual-pattern
                                            actual-case-sensitive
                                            advanced)))
                              (list single)
                            nil))
               (queried (e-session-tmp--apply-query
                         resources sort-by sort-order created-after created-before
                         updated-after updated-before)))
          (list :resources (vconcat queried)
                :truncated nil))
      (let* ((lines (e-session-tmp--process-lines
                     (e-session-tmp--find-executable "fd" '("fdfind"))
                     root
                     (append
                      (list "--glob"
                            "--color" "never"
                            "--base-directory" root
                            "--search-path" scope-relative
                            "--type" "file")
                      (unless advanced
                        (list "--max-results" (number-to-string (1+ actual-limit))))
                      (when fd-max-depth
                        (list "--max-depth" (number-to-string fd-max-depth)))
                      (list (if actual-case-sensitive
                                "--case-sensitive"
                              "--ignore-case"))
                      (list fd-pattern))))
             (filtered
              (seq-filter
               (lambda (relative)
                 (let* ((relative (e-session-tmp--clean-relative-path relative))
                        (absolute (expand-file-name relative root))
                        (name (e-session-tmp--result-name absolute scope)))
                   (e-resource-pattern-glob-match-p
                    actual-pattern
                    name
                    actual-case-sensitive)))
               lines))
             (resources
              (mapcar
               (lambda (relative)
                 (let* ((relative (e-session-tmp--clean-relative-path relative))
                        (absolute (expand-file-name relative root)))
                   (e-session-tmp--file-result relative absolute scope advanced)))
               filtered))
             (queried (e-session-tmp--apply-query
                       resources sort-by sort-order created-after created-before
                       updated-after updated-before))
             (truncated (> (length queried) actual-limit))
             (selected (seq-take queried actual-limit)))
        (list :resources (vconcat selected)
	      :truncated truncated)))))

(defun e-session-tmp--glob-content
    (lines root scope pattern case-sensitive limit)
  "Return tmp glob content from fd output LINES."
  (let* ((filtered
          (seq-filter
           (lambda (relative)
             (let* ((relative (e-session-tmp--clean-relative-path relative))
                    (absolute (expand-file-name relative root))
                    (name (e-session-tmp--result-name absolute scope)))
               (e-resource-pattern-glob-match-p pattern name case-sensitive)))
           lines))
         (truncated (> (length filtered) limit))
         (selected (seq-take filtered limit)))
    (list :resources
          (vconcat
           (mapcar
            (lambda (relative)
              (let* ((relative (e-session-tmp--clean-relative-path relative))
                     (absolute (expand-file-name relative root))
                     (attributes (file-attributes absolute)))
                (list :uri (concat "tmp://" relative)
                      :name (e-session-tmp--result-name absolute scope)
                      :kind 'file
                      :metadata (list :bytes (file-attribute-size attributes)))))
            selected))
          :truncated truncated)))

(defun e-session-tmp--rg-json-text (object)
  "Return text value from rg JSON OBJECT."
  (or (plist-get object :text)
      (when-let ((bytes (plist-get object :bytes)))
        (base64-decode-string bytes))))

(defun e-session-tmp--search-match-from-rg-json
    (line root scope glob-pattern query options)
  "Return a ranked search match plist for rg JSON LINE under ROOT, or nil."
  (let* ((object (json-parse-string line :object-type 'plist :array-type 'list))
         (type (plist-get object :type)))
    (when (equal type "match")
      (let* ((data (plist-get object :data))
             (path (e-session-tmp--rg-json-text (plist-get data :path)))
             (line-text (string-remove-suffix
                         "\n"
                         (or (e-session-tmp--rg-json-text
                              (plist-get data :lines))
                             "")))
             (absolute (expand-file-name path root))
             (relative (file-relative-name absolute root))
             (name (e-session-tmp--result-name absolute scope))
             (uri (concat "tmp://" relative)))
        (when (or (null glob-pattern)
                  (e-resource-pattern-glob-match-p glob-pattern name t))
          (when-let ((score (e-resource-pattern-search-score
                             line-text query options uri name)))
            (list :uri uri
                  :line (plist-get data :line_number)
                  :column (plist-get score :column)
                  :text line-text
                  :score (plist-get score :score)
                  :matched-terms (plist-get score :matched-terms))))))))

(defun e-session-tmp--search-advanced-p (options)
  "Return non-nil when OPTIONS needs tmp resource enumeration."
  (seq-some (lambda (key) (plist-member options key))
            '(:multiline :multi-term :resource-sort-by :resource-sort-order :resource-limit
              :created-after :created-before :updated-after :updated-before)))

(defun e-session-tmp--search-one-advanced (resource query options root)
  "Return ranked search matches for RESOURCE using Emacs search."
  (let* ((uri (plist-get resource :uri))
         (relative (string-remove-prefix "tmp://" uri))
         (path (expand-file-name relative root)))
    (when (file-regular-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (e-resource-pattern-search-matches-in-text
         uri
         (buffer-string)
         query
         options
         (plist-get resource :name))))))

(defun e-session-tmp--search-resource-advanced
    (harness session-id uri query options)
  "Search tmp resources with resource-level controls in OPTIONS."
  (let* ((root (e-session-tmp-directory harness session-id))
         (resource-limit (e-resource-query-resource-limit
                          (plist-get options :resource-limit)))
         (resource-result (e-session-tmp--glob-resource
                           harness
                           session-id
                           uri
                           (plist-get options :glob)
                           (or resource-limit most-positive-fixnum)
                           t
                           (plist-get options :resource-sort-by)
                           (plist-get options :resource-sort-order)
                           (plist-get options :created-after)
                           (plist-get options :created-before)
                           (plist-get options :updated-after)
                           (plist-get options :updated-before)))
         (resources (append (plist-get resource-result :resources) nil))
         (actual-limit (e-resource-pattern-search-limit
                        (plist-get options :limit)))
         matches)
    (dolist (resource resources)
      (setq matches
            (append matches
                    (e-session-tmp--search-one-advanced
                     resource query options root))))
    (let ((ranked (e-resource-pattern-rank-search-matches
                   matches (1+ actual-limit))))
      (list :matches (vconcat (seq-take ranked actual-limit))
            :truncated (> (length ranked) actual-limit)))))

(cl-defun e-session-tmp--search-resource (harness session-id uri query options)
  "Search tmp resources under parsed URI for QUERY with OPTIONS."
  (when (e-session-tmp--sqlite-store harness)
    (cl-return-from e-session-tmp--search-resource
      (signal 'e-session-tmp-resources-async-required
              (list "Persistent tmp:// search must run as detachable Work"
                    (plist-get uri :uri)))))
  (if (or (> (length (e-resource-pattern-search-terms query)) 1)
          (e-session-tmp--search-advanced-p options))
      (e-session-tmp--search-resource-advanced harness session-id uri query options)
    (let* ((root (e-session-tmp-directory harness session-id))
         (scope (e-session-tmp--scope-path harness session-id uri))
         (scope-relative (e-session-tmp--scope-relative-name uri))
         (glob-pattern (plist-get options :glob))
         (query-regexp (e-resource-pattern-search-rg-prefilter-regexp query options))
         (actual-limit (e-resource-pattern-search-limit
                        (plist-get options :limit)))
         (args (append
                (list "--json"
                      "--line-number"
                      "--column"
                      "--color" "never")
                (unless (plist-get options :case-sensitive)
                  (list "--ignore-case"))
                (when (plist-get options :multiline)
                  (list "--multiline"))
                (list "-e" query-regexp scope-relative))))
    (when glob-pattern
      (e-resource-pattern-compile-glob glob-pattern))
    (let ((lines (e-session-tmp--process-lines
                  (e-session-tmp--find-executable "rg")
                  root
                  args
                  '(0 1)))
          matches)
      (dolist (line lines)
        (when-let ((match (e-session-tmp--search-match-from-rg-json
                           line
                           root
                           scope
                           glob-pattern
                           query
                           options)))
          (push match matches)))
      (let ((ranked (e-resource-pattern-rank-search-matches
                     (nreverse matches) (1+ actual-limit))))
        (list :matches (vconcat (seq-take ranked actual-limit))
              :truncated (> (length ranked) actual-limit)))))))

(defun e-session-tmp--search-content
    (lines root scope glob-pattern actual-limit query options)
  "Return ranked tmp search content from rg JSON LINES."
  (let (matches)
    (dolist (line lines)
      (when-let ((match (e-session-tmp--search-match-from-rg-json
                         line
                         root
                         scope
                         glob-pattern
                         query
                         options)))
        (push match matches)))
    (let ((ranked (e-resource-pattern-rank-search-matches
                   (nreverse matches) (1+ actual-limit))))
      (list :matches (vconcat (seq-take ranked actual-limit))
            :truncated (> (length ranked) actual-limit)))))

(defun e-session-tmp--glob-work-command
    (harness session-id work-arguments _context)
  "Return process command for tmp glob WORK-ARGUMENTS."
  (let ((uri (plist-get work-arguments :uri))
        (arguments (plist-get work-arguments :operation-arguments)))
    (pcase-let ((`(,pattern ,limit ,case-sensitive . ,query-arguments) arguments))
      (let* ((root (e-session-tmp-directory harness session-id))
             (scope (e-session-tmp--scope-path harness session-id uri))
             (scope-relative (e-session-tmp--scope-relative-name uri))
             (actual-pattern (or pattern "*"))
             (fd-pattern (e-resource-pattern-glob-fd-candidate-pattern actual-pattern))
             (fd-max-depth (e-resource-pattern-glob-max-depth actual-pattern))
             (actual-limit (e-session-tmp--discovery-limit limit))
             (advanced (seq-some #'identity query-arguments))
             (actual-case-sensitive (if (null case-sensitive) t case-sensitive))
             (metadata (list :operation 'glob :scheme "tmp")))
        (e-resource-pattern-compile-glob actual-pattern)
        (if (file-regular-p scope)
            (list :immediate
                  (list :resources
                        (if-let ((single (e-session-tmp--glob-single-result
                                          scope
                                          scope-relative
                                          actual-pattern
                                          actual-case-sensitive
                                          advanced)))
                            (vector single)
                          [])
                        :truncated nil)
                  :metadata metadata)
          (if advanced
              (list :immediate
                    (apply #'e-session-tmp--glob-resource
                           harness session-id uri pattern limit case-sensitive
                           query-arguments)
                    :metadata metadata)
            (list :program (e-session-tmp--find-executable "fd" '("fdfind"))
                  :directory root
                  :args (append
                         (list "--glob"
                               "--color" "never"
                               "--base-directory" root
                               "--search-path" scope-relative
                               "--type" "file"
                               "--max-results" (number-to-string
                                                (1+ actual-limit)))
                       (when fd-max-depth
                         (list "--max-depth"
                               (number-to-string fd-max-depth)))
                       (list (if actual-case-sensitive
                                 "--case-sensitive"
                               "--ignore-case"))
                         (list fd-pattern))
                  :metadata metadata)))))))

(defun e-session-tmp--glob-work-result
    (harness session-id raw work-arguments _context)
  "Return tmp glob resource content from process RAW result."
  (if (plist-member raw :resources)
      raw
    (let ((uri (plist-get work-arguments :uri))
          (arguments (plist-get work-arguments :operation-arguments)))
      (pcase-let ((`(,pattern ,limit ,case-sensitive . ,query-arguments) arguments))
        (let* ((root (e-session-tmp-directory harness session-id))
               (scope (e-session-tmp--scope-path harness session-id uri))
               (actual-pattern (or pattern "*"))
               (actual-limit (e-session-tmp--discovery-limit limit))
               (actual-case-sensitive
                (if (null case-sensitive) t case-sensitive)))
          (if query-arguments
              (apply #'e-session-tmp--glob-resource
                     harness session-id uri pattern limit case-sensitive
                     query-arguments)
            (e-session-tmp--glob-content
             (plist-get raw :lines)
             root
             scope
             actual-pattern
             actual-case-sensitive
             actual-limit)))))))

(defun e-session-tmp--glob-work (harness session-id)
  "Return tmp glob work spec for HARNESS SESSION-ID."
  (e-work-spec-create
   :id "tmp_glob"
   :description "Glob session tmp resources through fd."
   :execution 'process
   :interactive-policy 'async
   :owner 'resources
   :command (lambda (work-arguments context)
              (e-session-tmp--glob-work-command
               harness session-id work-arguments context))
   :result-shaper (lambda (raw work-arguments context)
                    (e-session-tmp--glob-work-result
                     harness session-id raw work-arguments context))))

(defun e-session-tmp--search-work-command
    (harness session-id work-arguments _context)
  "Return process command for tmp search WORK-ARGUMENTS."
  (let ((uri (plist-get work-arguments :uri))
        (arguments (plist-get work-arguments :operation-arguments)))
    (pcase-let ((`(,query ,options) arguments))
      (let* ((root (e-session-tmp-directory harness session-id))
             (scope (e-session-tmp--scope-path harness session-id uri))
             (scope-relative (e-session-tmp--scope-relative-name uri))
             (glob-pattern (plist-get options :glob))
             (actual-limit (e-resource-pattern-search-limit
                            (plist-get options :limit)))
             (query-regexp (e-resource-pattern-search-rg-prefilter-regexp query options))
             (args (append
                    (list "--json"
                          "--line-number"
                          "--column"
                          "--color" "never")
                    (unless (plist-get options :case-sensitive)
                      (list "--ignore-case"))
                    (when (plist-get options :multiline)
                      (list "--multiline"))
                    (list "-e" query-regexp scope-relative))))
        (when glob-pattern
          (e-resource-pattern-compile-glob glob-pattern))
        (if (or (> (length (e-resource-pattern-search-terms query)) 1)
                (e-session-tmp--search-advanced-p options))
            (list :immediate
                  (e-session-tmp--search-resource harness session-id uri query options)
                  :metadata (list :operation 'search :scheme "tmp"))
          (let ((collector
                 (e-resource-pattern-search-collector-create
                  :limit actual-limit
                  :count 0
                  :transform
                  (lambda (line)
                    (e-session-tmp--search-match-from-rg-json
                     line root scope glob-pattern query options)))))
            (list :program (e-session-tmp--find-executable "rg")
                  :directory root
                  :args args
                  :ok-statuses '(0 1)
                  :capture-output nil
                  :state collector
                  :on-output
                  (lambda (_handle process chunk active-collector)
                    (when (e-resource-pattern-search-collector-feed
                           active-collector chunk)
                      (when (process-live-p process)
                        (kill-process process))))
                  :finish-on-nonzero t
                  :metadata (list :operation 'search :scheme "tmp"))))))))

(defun e-session-tmp--search-work-result
    (_harness _session-id raw _work-arguments _context)
  "Return tmp search resource content from process RAW result."
  (if (plist-member raw :matches)
      raw
    (let ((collector (plist-get raw :state)))
      (unless (or (eq (plist-get raw :status) 'ok)
                  (e-resource-pattern-search-collector-truncated collector))
        (signal 'e-session-tmp-resources-process-failed
                (list (or (plist-get raw :suffix)
                          (string-trim (or (plist-get raw :stderr) ""))))))
      (e-resource-pattern-search-collector-result collector))))

(defun e-session-tmp--search-work (harness session-id)
  "Return tmp search work spec for HARNESS SESSION-ID."
  (e-work-spec-create
   :id "tmp_search"
   :description "Search session tmp resources through rg."
   :execution 'process
   :interactive-policy 'async
   :owner 'resources
   :command (lambda (work-arguments context)
              (e-session-tmp--search-work-command
               harness session-id work-arguments context))
   :result-shaper (lambda (raw work-arguments context)
                    (e-session-tmp--search-work-result
                     harness session-id raw work-arguments context))))

(defun e-session-tmp--write-file (path content)
  "Write CONTENT to PATH as UTF-8 without a coding-system prompt.
Content may contain eight-bit bytes that the buffer's detected coding cannot
encode; without these bindings `write-region' would invoke
`select-safe-coding-system' and block on the interactive coding-system picker."
  (let ((coding-system-for-write 'utf-8-unix)
        (select-safe-coding-system-function nil))
    (write-region content nil path nil 'silent)))

(cl-defun e-session-tmp-write (harness session-id relative-name content)
  "Write CONTENT to RELATIVE-NAME in HARNESS SESSION-ID and return tmp URI."
  (when (e-session-tmp--sqlite-store harness)
    (cl-return-from e-session-tmp-write
      (e-session-tmp--sqlite-put
       harness session-id relative-name content)))
  (let ((path (e-session-tmp--path harness session-id relative-name))
        (root (e-session-tmp-directory harness session-id)))
    (make-directory (file-name-directory path) t)
    (e-session-tmp--write-file path (format "%s" content))
    (e-session-tmp--touch-root root)
    (e-session-tmp--uri relative-name)))

(defun e-session-tmp--read-relative (harness session-id relative-name range)
  "Read RELATIVE-NAME through the active physical content primitive."
  (if (e-session-tmp--sqlite-store harness)
      (signal 'e-session-tmp-resources-async-required
              (list "Persistent tmp:// read must run as detachable Work"
                    (e-session-tmp--uri relative-name)))
    (e-session-tmp--read-file
     (e-session-tmp--path harness session-id relative-name) range)))

(cl-defun e-session-tmp-write-generated (harness session-id relative-name writer)
  "Have WRITER generate RELATIVE-NAME and return its tmp URI.
WRITER receives the absolute destination path.  This is the bounded-memory
counterpart to `e-session-tmp-write' for producers that stream their content."
  (unless (functionp writer)
    (signal 'wrong-type-argument (list 'functionp writer)))
  (when-let* ((sqlite-result
               (e-session-tmp-sqlite-write-generated
                harness session-id relative-name writer)))
    (cl-return-from e-session-tmp-write-generated (cdr sqlite-result)))
  (let ((path (e-session-tmp--path harness session-id relative-name))
        (root (e-session-tmp-directory harness session-id)))
    (make-directory (file-name-directory path) t)
    (let ((temporary (make-temp-file (concat path ".") nil ".tmp")))
      (unwind-protect
          (progn
            (funcall writer temporary)
            (unless (file-regular-p temporary)
              (signal 'file-missing
                      (list "Generated tmp resource is missing" temporary)))
            (rename-file temporary path t)
            (setq temporary nil))
        (when (and temporary (file-exists-p temporary))
          (delete-file temporary))))
    (unless (file-regular-p path)
      (signal 'file-missing (list "Generated tmp resource is missing" path)))
    (e-session-tmp--touch-root root)
    (e-session-tmp--uri relative-name)))

(cl-defun e-session-tmp-write-raw-result
    (harness session-id relative-name content
             &key owner redaction-policy cleanup-lifetime preview
             preview-bytes metadata)
  "Persist raw result CONTENT and return a bounded reference plist.
RELATIVE-NAME is written under the session tmp root.  OWNER identifies the
caller-visible owner of the result.  REDACTION-POLICY and CLEANUP-LIFETIME are
metadata for consumers deciding how to show or clean up the reference.  PREVIEW,
when non-nil, is used as the bounded model/display preview; otherwise CONTENT is
previewed with `e-tools-result-content-preview'."
  (let* ((content-text (format "%s" content))
         (limit (max 0 (or preview-bytes
                           e-session-tmp-raw-result-preview-bytes)))
         (preview-data
          (e-tools-result-content-preview
           (or preview content-text)
           limit))
         (uri (e-session-tmp-write harness session-id relative-name content-text))
         (reference
          (list :uri uri
                :owner owner
                :storage 'session-tmp
                :original-bytes (string-bytes content-text)
                :preview (plist-get preview-data :text)
                :preview-bytes (plist-get preview-data :shown-bytes)
                :preview-truncated (plist-get preview-data :truncated)
                :redaction-policy (or redaction-policy 'none)
                :cleanup-lifetime (or cleanup-lifetime 'session-tmp))))
    (if metadata
        (append reference (list :metadata metadata))
      reference)))

(cl-defun e-session-tmp-cleanup-reference (harness session-id reference)
  "Delete one session tmp REFERENCE for HARNESS SESSION-ID.
REFERENCE may be a raw-result reference plist or a =tmp://= URI string.  Return
the deleted file path, or nil when the reference is not session-tmp backed, its
session root is already gone, or the referenced file is already absent."
  (e-session-tmp--require-session harness session-id)
  (when-let* ((sqlite-result
               (e-session-tmp-sqlite-delete-reference
                harness session-id reference)))
    (cl-return-from e-session-tmp-cleanup-reference (cdr sqlite-result)))
  (when-let* ((uri (e-session-tmp--reference-uri reference))
              (lineage-id (e-session-tmp--lineage-id harness session-id))
              (root (gethash lineage-id e-session-tmp--roots)))
    (let* ((relative-name (e-session-tmp--relative-name-from-uri uri))
           (path (e-session-tmp--path-in-root root relative-name)))
      (when (file-exists-p path)
        (delete-file path)
        (e-session-tmp--delete-empty-parents root path)
        (e-session-tmp--touch-root root)
        path))))

(defun e-session-tmp-reference-available-p (harness session-id reference)
  "Return non-nil when REFERENCE currently resolves to a regular tmp file.

This is a non-mutating liveness query.  Unlike the read and write helpers it
never creates a lineage root, registers a harness, touches root expiry, or
updates file timestamps.  Invalid/missing references simply report nil so
receipt projection can render an unavailable detail state without changing the
durable receipt."
  (when (and harness
             (stringp session-id)
             (not (string-empty-p session-id)))
    (condition-case nil
        (let ((sqlite-result
               (e-session-tmp-sqlite-reference-available-p
                harness session-id reference)))
          (if sqlite-result
              (cdr sqlite-result)
            (when-let* ((uri (e-session-tmp--reference-uri reference))
                        (lineage-id
                         (e-session-tmp--lineage-id harness session-id))
                        (root (gethash lineage-id e-session-tmp--roots)))
              (and (file-directory-p root)
                   (let* ((relative-name
                           (e-session-tmp--relative-name-from-uri uri))
                          (path (e-session-tmp--path-in-root
                                 root relative-name)))
                     (and (file-regular-p path)
                          (file-readable-p path)))))))
      (error nil))))

(defun e-session-tmp-cleanup-references (harness session-id references)
  "Delete session tmp REFERENCES for HARNESS SESSION-ID.
Return the list of deleted file paths.  This helper is intended for cache
invalidation paths that own multiple raw-result references."
  (delq nil
        (mapcar (lambda (reference)
                  (e-session-tmp-cleanup-reference
                   harness session-id reference))
                references)))

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

(defun e-session-tmp--read-content (content range)
  "Read text CONTENT with the same optional line RANGE contract as files."
  (with-temp-buffer
    (insert content)
    (if (not range)
        (buffer-string)
      (let ((unit (plist-get range :unit))
            (start (plist-get range :start))
            (end (plist-get range :end)))
        (unless (and (equal unit "line") (integerp start) (> start 0)
                     (or (null end)
                         (and (integerp end) (>= end start))))
          (signal 'wrong-type-argument (list 'line-range-p range)))
        (goto-char (point-min))
        (forward-line (1- start))
        (let ((beg (point)))
          (if end (forward-line (1+ (- end start)))
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

(defun e-session-tmp--sqlite-finish-work (handle result error transform)
  "Settle HANDLE from RESULT or ERROR after applying TRANSFORM."
  (if error
      (e-work-fail handle error)
    (condition-case err
        (e-work-finish handle (funcall transform result))
      (error (e-work-fail handle err)))))

(defun e-session-tmp--sqlite-submit-work
    (handle harness session-id kind body transform)
  "Submit one tmp request and settle HANDLE through TRANSFORM."
  (let ((request
         (e-session-tmp-sqlite-submit
          harness session-id kind body
          (lambda (result error)
            (e-session-tmp--sqlite-finish-work
             handle result error transform)))))
    (setf (e-work-handle-cancel-function handle)
          (lambda (_handle)
            (when-let ((runtime (e-session-tmp--sqlite-store harness)))
              (e-runtime-store-cancel runtime request))
            t))
    :deferred))

(defun e-session-tmp--sqlite-read-result (relative-name range page)
  "Shape bounded PAGE for RELATIVE-NAME and optional line RANGE."
  (unless page
    (signal 'file-missing
            (list "Opening tmp resource" (e-session-tmp--uri relative-name))))
  (when (plist-get page :next)
    (signal 'e-session-tmp-resources-page-too-large
            (list (format "tmp://%s exceeds the 256 KiB visible read page"
                          relative-name))))
  (e-session-tmp--read-content (plist-get page :content) range))

(defun e-session-tmp--sqlite-search-result (rows uri query options)
  "Shape a bounded tmp search over detached ROWS."
  (let ((content-by-path (make-hash-table :test 'equal)))
    (dolist (row rows)
      (puthash (plist-get row :path) (or (plist-get row :content) "")
               content-by-path))
    (let ((result
           (e-session-tmp--search-listed-resources
            rows (lambda (path) (gethash path content-by-path ""))
            uri query options)))
      (when (= (length rows) 256)
        (plist-put result :truncated t))
      result)))

(defun e-session-tmp--sqlite-edit-work
    (handle harness session-id parsed-uri edits)
  "Run one bounded read-modify-write edit and settle HANDLE."
  (let* ((relative-name
          (e-session-tmp--safe-relative-name (plist-get parsed-uri :address)))
         current-request)
    (setf (e-work-handle-cancel-function handle)
          (lambda (_handle)
            (when (and current-request
                       (e-session-tmp--sqlite-store harness))
              (e-runtime-store-cancel
               (e-session-tmp--sqlite-store harness) current-request))
            t))
    (setq
     current-request
     (e-session-tmp-sqlite-submit
      harness session-id 'read
      (list :op 'resource-read :path relative-name
            :offset 0 :limit (* 256 1024))
      (lambda (page error)
        (if error
            (e-work-fail handle error)
          (condition-case err
              (let* ((content
                      (e-session-tmp--sqlite-read-result
                       relative-name nil page))
                     (new-content
                      (e-session-tmp--apply-edits
                       content edits (plist-get parsed-uri :uri))))
                (setq
                 current-request
                 (e-session-tmp-sqlite-submit
                  harness session-id 'write
                  (list :op 'resource-put :session-id session-id
                        :path relative-name :content new-content
                        :expires-at
                        (+ (float-time)
                           e-session-tmp-default-max-age-seconds))
                  (lambda (_result put-error)
                    (if put-error
                        (e-work-fail handle put-error)
                      (e-work-finish handle
                                     (e-session-tmp--uri relative-name)))))))
            (error (e-work-fail handle err)))))))
    :deferred))

(defun e-session-tmp--sqlite-resource-work (harness session-id operation)
  "Return detached SQLite tmp Work for OPERATION."
  (e-work-spec-create
   :id (format "tmp-resource.%s" (e-operation-id-of operation))
   :description "Run one bounded SQLite-backed tmp resource operation."
   :execution 'cooperative :interactive-policy 'async
   :owner 'session-tmp-resources
   :runner
   (lambda (handle arguments _context)
     (condition-case err
         (let* ((parsed-uri (plist-get arguments :uri))
                (operation-arguments
                 (plist-get arguments :operation-arguments)))
           (pcase (e-operation-id-of operation)
             ('read
              (let ((relative-name
                     (e-session-tmp--safe-relative-name
                      (plist-get parsed-uri :address))))
                (e-session-tmp--sqlite-submit-work
                 handle harness session-id 'read
                 (list :op 'resource-read :path relative-name
                       :offset 0 :limit (* 256 1024))
                 (lambda (page)
                   (e-session-tmp--sqlite-read-result
                    relative-name (car operation-arguments) page)))))
             ('write
              (let ((relative-name
                     (e-session-tmp--safe-relative-name
                      (plist-get parsed-uri :address))))
                (e-session-tmp--sqlite-submit-work
                 handle harness session-id 'write
                 (list :op 'resource-put :session-id session-id
                       :path relative-name
                       :content (format "%s" (car operation-arguments))
                       :expires-at
                       (+ (float-time) e-session-tmp-default-max-age-seconds))
                 (lambda (_result) (e-session-tmp--uri relative-name)))))
             ('edit
              (e-session-tmp--sqlite-edit-work
               handle harness session-id parsed-uri (car operation-arguments)))
             ('glob
              (e-session-tmp--sqlite-submit-work
               handle harness session-id 'read
               (list :op 'resource-list :limit 4096)
               (lambda (rows)
                 (apply #'e-session-tmp--glob-listed-resources
                        rows parsed-uri operation-arguments))))
             ('search
              (e-session-tmp--sqlite-submit-work
               handle harness session-id 'read
               (list :op 'resource-search-source :limit 256)
               (lambda (rows)
                 (e-session-tmp--sqlite-search-result
                  rows parsed-uri (car operation-arguments)
                  (cadr operation-arguments)))))
             (_
              (signal 'e-resources-unsupported-operation
                      (list "Unsupported persistent tmp operation"
                            (e-operation-id-of operation))))))
       (error (e-work-fail handle err)))
     :deferred)))


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
               (e-session-tmp--read-relative
                harness session-id (plist-get uri :address) range))
    :work (when (e-session-tmp--sqlite-store harness)
            (e-session-tmp--sqlite-resource-work
             harness session-id e-operation-read))))
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
                content))
    :work (when (e-session-tmp--sqlite-store harness)
            (e-session-tmp--sqlite-resource-work
             harness session-id e-operation-write))))
  (e-resources-register
   registry
   (e-resource-method-create
    :scheme "tmp"
    :operation e-operation-edit
    :description "Edit an existing ephemeral session-scoped temporary text resource."
    :uri-patterns '("tmp://<relative-path>")
    :handler (lambda (uri edits)
               (let* ((relative-name (plist-get uri :address))
                      (content (e-session-tmp--read-relative
                                harness session-id relative-name nil))
                      (new-content
                       (e-session-tmp--apply-edits
                        content
                        edits
                        (plist-get uri :uri))))
                 (e-session-tmp-write
                  harness session-id relative-name new-content)))
    :work (when (e-session-tmp--sqlite-store harness)
            (e-session-tmp--sqlite-resource-work
             harness session-id e-operation-edit))))
  (e-resources-register
   registry
   (e-resource-method-create
    :scheme "tmp"
    :operation e-operation-glob
   :description "List ephemeral session-scoped temporary text resources."
   :uri-patterns '("tmp://<relative-path-or-directory>"
                   "tmp://")
   :handler (lambda (uri pattern limit case-sensitive sort-by sort-order
                     created-after created-before updated-after updated-before)
              (e-session-tmp--glob-resource
               harness
               session-id
               uri
               pattern
               limit
               case-sensitive
               sort-by
               sort-order
               created-after
               created-before
               updated-after
               updated-before))
   :work (if (e-session-tmp--sqlite-store harness)
             (e-session-tmp--sqlite-resource-work
              harness session-id e-operation-glob)
           (e-session-tmp--glob-work harness session-id))))
  (e-resources-register
   registry
   (e-resource-method-create
    :scheme "tmp"
    :operation e-operation-search
   :description "Search ephemeral session-scoped temporary text resources."
   :uri-patterns '("tmp://<relative-path-or-directory>"
                   "tmp://")
   :handler (lambda (uri query options)
              (e-session-tmp--search-resource
               harness
               session-id
               uri
               query
               options))
   :work (if (e-session-tmp--sqlite-store harness)
             (e-session-tmp--sqlite-resource-work
              harness session-id e-operation-search)
           (e-session-tmp--search-work harness session-id))))
  nil)

(defun e-session-tmp-capability-create ()
  "Return the session tmp resource capability."
  (e-capability-create
   :id 'session-tmp-resources
   :name "Session Tmp Resources"
   :resource-methods
   (list (e-capability-resource-method-provider-create
          :handler #'e-session-tmp--register-resource-methods))
   :hooks
   (list (e-hook-create
          :id "50-session-tmp-cleanup"
          :point :session-reset
          :handler (lambda (_value context)
                     (e-session-tmp-cleanup-session
                      (plist-get context :harness)
                      (plist-get context :session-id))
                     nil)))))

(provide 'e-session-tmp-resources)

;;; e-session-tmp-resources.el ends here
