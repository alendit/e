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
(require 'e-request)
(require 'e-resources)
(require 'e-tools)
(require 'e-work)

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

(defvar e-session-tmp--roots (make-hash-table :test 'eq)
  "Session tmp root tables keyed by harness object.")

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

(defun e-session-tmp-cleanup-session (harness session-id)
  "Delete tmp resources owned by HARNESS SESSION-ID."
  (e-session-tmp--require-session harness session-id)
  (let* ((table (gethash harness e-session-tmp--roots))
         (root (and table (gethash session-id table))))
    (when table
      (remhash session-id table))
    (when (and root (file-directory-p root))
      (delete-directory root t))
    root))

(defun e-session-tmp-cleanup-harness (harness)
  "Delete all tmp resources owned by HARNESS."
  (let ((table (gethash harness e-session-tmp--roots)))
    (when table
      (maphash
       (lambda (_session-id root)
         (when (and root (file-directory-p root))
           (delete-directory root t)))
       table)
      (remhash harness e-session-tmp--roots))
    harness))

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
  "Delete session tmp roots idle longer than MAX-AGE-SECONDS.
MAX-AGE-SECONDS defaults to `e-session-tmp-default-max-age-seconds'.  NOW
defaults to the current time.  Return a list of deleted root directories."
  (let* ((max-age (or max-age-seconds e-session-tmp-default-max-age-seconds))
         (now (or now (float-time)))
         expired
         deleted)
    (when (and (numberp max-age) (>= max-age 0))
      (maphash
       (lambda (harness table)
         (maphash
          (lambda (session-id root)
            (let ((age (e-session-tmp--root-age-seconds root now)))
              (when (or (null age) (> age max-age))
                (push (list harness session-id root) expired))))
          table))
       e-session-tmp--roots)
      (dolist (entry expired)
        (let* ((harness (nth 0 entry))
               (session-id (nth 1 entry))
               (root (nth 2 entry))
               (table (gethash harness e-session-tmp--roots)))
          (when table
            (remhash session-id table)
            (when (= (hash-table-count table) 0)
              (remhash harness e-session-tmp--roots)))
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

(defun e-session-tmp--glob-single-result
    (scope scope-relative pattern case-sensitive)
  "Return a single tmp glob result for SCOPE, or nil if it does not match."
  (when (and (file-regular-p scope)
             (e-resource-pattern-glob-match-p
              pattern
              (file-name-nondirectory scope-relative)
              case-sensitive))
    (let ((attributes (file-attributes scope)))
      (list :uri (concat "tmp://" scope-relative)
            :name (file-name-nondirectory scope)
            :kind 'file
            :metadata (list :bytes (file-attribute-size attributes))))))

(defun e-session-tmp--glob-resource
    (harness session-id uri pattern limit case-sensitive)
  "List tmp resources under parsed URI with PATTERN and LIMIT."
  (let* ((root (e-session-tmp-directory harness session-id))
         (scope (e-session-tmp--scope-path harness session-id uri))
         (scope-relative (e-session-tmp--scope-relative-name uri))
         (actual-pattern (or pattern "*"))
         (fd-pattern (e-resource-pattern-glob-fd-pattern actual-pattern))
         (fd-max-depth (e-resource-pattern-glob-max-depth actual-pattern))
         (actual-limit (e-session-tmp--discovery-limit limit))
         (actual-case-sensitive (if (null case-sensitive) t case-sensitive)))
    (e-resource-pattern-compile-glob actual-pattern)
    (if (file-regular-p scope)
        (list :resources
              (if-let ((single (e-session-tmp--glob-single-result
                                scope
                                scope-relative
                                actual-pattern
                                actual-case-sensitive)))
                  (vector single)
                [])
              :truncated nil)
      (let* ((lines (e-session-tmp--process-lines
                     (e-session-tmp--find-executable "fd" '("fdfind"))
                     root
                     (append
                      (list "--glob"
                            "--color" "never"
                            "--base-directory" root
                            "--search-path" scope-relative
                            "--type" "file"
                            "--max-results" (number-to-string (1+ actual-limit)))
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
             (truncated (> (length filtered) actual-limit))
             (selected (seq-take filtered actual-limit)))
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
    (line root scope glob-pattern)
  "Return a search match plist for rg JSON LINE under ROOT, or nil."
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
             (submatches (plist-get data :submatches))
             (first-match (car submatches))
             (start (or (plist-get first-match :start) 0))
             (absolute (expand-file-name path root))
             (relative (file-relative-name absolute root)))
        (when (or (null glob-pattern)
                  (e-resource-pattern-glob-match-p
                   glob-pattern
                   (e-session-tmp--result-name absolute scope)
                   t))
          (list :uri (concat "tmp://" relative)
                :line (plist-get data :line_number)
                :column (1+ start)
                :text line-text))))))

(defun e-session-tmp--search-resource (harness session-id uri query options)
  "Search tmp resources under parsed URI for QUERY with OPTIONS."
  (let* ((root (e-session-tmp-directory harness session-id))
         (scope (e-session-tmp--scope-path harness session-id uri))
         (scope-relative (e-session-tmp--scope-relative-name uri))
         (glob-pattern (plist-get options :glob))
         (query-regexp (e-resource-pattern-search-rg-regexp query options))
         (actual-limit (e-session-tmp--discovery-limit
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
                           glob-pattern)))
          (push match matches)))
      (setq matches (nreverse matches))
	      (list :matches (vconcat (seq-take matches actual-limit))
	            :truncated (> (length matches) actual-limit)))))

(defun e-session-tmp--search-content
    (lines root scope glob-pattern actual-limit)
  "Return tmp search content from rg JSON LINES."
  (let (matches)
    (dolist (line lines)
      (when-let ((match (e-session-tmp--search-match-from-rg-json
                         line
                         root
                         scope
                         glob-pattern)))
        (push match matches)))
    (setq matches (nreverse matches))
    (list :matches (vconcat (seq-take matches actual-limit))
          :truncated (> (length matches) actual-limit))))

(defun e-session-tmp--glob-work-command
    (harness session-id work-arguments _context)
  "Return process command for tmp glob WORK-ARGUMENTS."
  (let ((uri (plist-get work-arguments :uri))
        (arguments (plist-get work-arguments :operation-arguments)))
    (pcase-let ((`(,pattern ,limit ,case-sensitive) arguments))
      (let* ((root (e-session-tmp-directory harness session-id))
             (scope (e-session-tmp--scope-path harness session-id uri))
             (scope-relative (e-session-tmp--scope-relative-name uri))
             (actual-pattern (or pattern "*"))
             (fd-pattern (e-resource-pattern-glob-fd-pattern actual-pattern))
             (fd-max-depth (e-resource-pattern-glob-max-depth actual-pattern))
             (actual-limit (e-session-tmp--discovery-limit limit))
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
                                          actual-case-sensitive)))
                            (vector single)
                          [])
                        :truncated nil)
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
                :metadata metadata))))))

(defun e-session-tmp--glob-work-result
    (harness session-id raw work-arguments _context)
  "Return tmp glob resource content from process RAW result."
  (if (plist-member raw :resources)
      raw
    (let ((uri (plist-get work-arguments :uri))
          (arguments (plist-get work-arguments :operation-arguments)))
      (pcase-let ((`(,pattern ,limit ,case-sensitive) arguments))
        (let* ((root (e-session-tmp-directory harness session-id))
               (scope (e-session-tmp--scope-path harness session-id uri))
               (actual-pattern (or pattern "*"))
               (actual-limit (e-session-tmp--discovery-limit limit))
               (actual-case-sensitive
                (if (null case-sensitive) t case-sensitive)))
          (e-session-tmp--glob-content
           (plist-get raw :lines)
           root
           scope
           actual-pattern
           actual-case-sensitive
           actual-limit))))))

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
             (scope-relative (e-session-tmp--scope-relative-name uri))
             (glob-pattern (plist-get options :glob))
             (query-regexp (e-resource-pattern-search-rg-regexp query options))
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
        (list :program (e-session-tmp--find-executable "rg")
              :directory root
              :args args
              :ok-statuses '(0 1)
              :metadata (list :operation 'search :scheme "tmp"))))))

(defun e-session-tmp--search-work-result
    (harness session-id raw work-arguments _context)
  "Return tmp search resource content from process RAW result."
  (let ((uri (plist-get work-arguments :uri))
        (arguments (plist-get work-arguments :operation-arguments)))
    (pcase-let ((`(,_query ,options) arguments))
      (let* ((root (e-session-tmp-directory harness session-id))
             (scope (e-session-tmp--scope-path harness session-id uri))
             (glob-pattern (plist-get options :glob))
             (actual-limit (e-session-tmp--discovery-limit
                            (plist-get options :limit))))
        (e-session-tmp--search-content
         (plist-get raw :lines)
         root
         scope
         glob-pattern
         actual-limit)))))

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

(defun e-session-tmp-write (harness session-id relative-name content)
  "Write CONTENT to RELATIVE-NAME in HARNESS SESSION-ID and return tmp URI."
  (let ((path (e-session-tmp--path harness session-id relative-name))
        (root (e-session-tmp-directory harness session-id)))
    (make-directory (file-name-directory path) t)
    (e-session-tmp--write-file path (format "%s" content))
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

(defun e-session-tmp-cleanup-reference (harness session-id reference)
  "Delete one session tmp REFERENCE for HARNESS SESSION-ID.
REFERENCE may be a raw-result reference plist or a =tmp://= URI string.  Return
the deleted file path, or nil when the reference is not session-tmp backed, its
session root is already gone, or the referenced file is already absent."
  (e-session-tmp--require-session harness session-id)
  (when-let* ((uri (e-session-tmp--reference-uri reference))
              (table (gethash harness e-session-tmp--roots))
              (root (gethash session-id table)))
    (let* ((relative-name (e-session-tmp--relative-name-from-uri uri))
           (path (e-session-tmp--path-in-root root relative-name)))
      (when (file-exists-p path)
        (delete-file path)
        (e-session-tmp--delete-empty-parents root path)
        (e-session-tmp--touch-root root)
        path))))

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
  (e-resources-register
   registry
   (e-resource-method-create
    :scheme "tmp"
    :operation e-operation-glob
   :description "List ephemeral session-scoped temporary text resources."
   :uri-patterns '("tmp://<relative-path-or-directory>"
                   "tmp://")
   :handler (lambda (uri pattern limit case-sensitive)
              (e-session-tmp--glob-resource
               harness
               session-id
               uri
               pattern
               limit
               case-sensitive))
   :work (e-session-tmp--glob-work harness session-id)))
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
   :work (e-session-tmp--search-work harness session-id)))
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
