;;; e-goodnite-resources.el --- Read-only goodnite:// knowledge resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Daydream: the online read side of goodnite.  Goodnite's offline `dream' run
;; mines past sessions and distils durable task knowledge under GOODNITE_HOME.
;; This module exposes that knowledge to a live agent through a read-only
;; goodnite:// URI scheme, keyed by what the knowledge helps the agent DO --
;; not by goodnite's internal pipeline stage:
;;
;;   goodnite://workflows/<slug>     how to do a recurring task
;;   goodnite://pitfalls/<slug>      a failure mode to avoid
;;   goodnite://conventions/<slug>   a project/tool convention or preference
;;
;; The mapping from goodnite artifacts (candidate skill drafts, gotcha notes,
;; facts fragments) to these consumer types is a producer-side detail.  The
;; only provenance surfaced is a `confidence' hint: `established' knowledge is
;; human-reviewed, `mined (unreviewed)' is a strong lead from real prior runs
;; that has not been vetted.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-goodnite-storage)
(require 'e-operations)
(require 'e-resource-patterns)
(require 'e-resource-query)
(require 'e-resources)

(define-error 'e-goodnite-resources-invalid-uri
  "goodnite:// resource URI is invalid")
(define-error 'e-goodnite-resources-unknown-type
  "goodnite:// knowledge type is not known")
(define-error 'e-goodnite-resources-unknown-entry
  "goodnite:// entry does not exist")

(defcustom e-goodnite-home nil
  "Root directory of the goodnite knowledge base.

When nil, resolve from the GOODNITE_HOME environment variable, falling back
to ~/.goodnite.  This is the directory goodnite's offline `dream' run writes
its distilled artifacts into."
  :type '(choice (const :tag "Resolve from environment" nil) directory)
  :group 'e)

(defcustom e-goodnite-search-program "goodnite"
  "Program invoked for semantic search over the goodnite knowledge base.

The program must accept `knowledge-search QUERY --limit N' and print a JSON
object on its last stdout line, matching goodnite's `knowledge-search' command.
When the program is absent or fails, goodnite:// search falls back to lexical
matching over the same entries."
  :type 'string
  :group 'e)

(defcustom e-goodnite-search-semantic t
  "When non-nil, goodnite:// search tries semantic retrieval first.
Falls back to lexical search when the index is absent or the program fails."
  :type 'boolean
  :group 'e)

(defcustom e-goodnite-track-access t
  "When non-nil, record goodnite:// reads and search hits as a demand signal.

Each access appends one JSON line to `daydream_access.jsonl' under the
knowledge base's `state/' directory.  The offline `dream' loop reads this log
to rank mined-but-unreviewed knowledge an agent keeps consulting ahead of
knowledge nothing has pulled up -- demand, not just recurrence."
  :type 'boolean
  :group 'e)

(defvar e-goodnite-resources-storage nil
  "Optional Goodnite demand-event SQLite storage port.")

(defvar e-goodnite-resources--event-sequence 0
  "Process-local uniqueness sequence for demand observations.")

(defun e-goodnite-resources-configure-storage (storage)
  "Install Goodnite demand STORAGE, or nil for the legacy/default path."
  (unless (or (null storage) (e-goodnite-storage-p storage))
    (signal 'wrong-type-argument (list 'e-goodnite-storage-p storage)))
  (setq e-goodnite-resources-storage storage))

(defconst e-goodnite-resources--type-order '("workflows" "pitfalls" "conventions")
  "Consumer knowledge types in display order.")

(defconst e-goodnite-resources--type-map
  '(("workflows"
     :subdir "candidates"
     :layout dir
     :summary "How to do a recurring task, mined from past sessions.")
    ("pitfalls"
     :subdir "gotchas"
     :layout file
     :summary "A failure mode to avoid, distilled from failed runs.")
    ("conventions"
     :subdir "facts"
     :layout file
     :summary "A project or tool convention accumulated across sessions."))
  "Map consumer type name to its goodnite source layout.
`:layout' is `dir' when each entry is <subdir>/<slug>/SKILL.md and `file'
when each entry is <subdir>/<slug>.md.")

(defun e-goodnite-resources--home ()
  "Return the resolved goodnite knowledge base directory."
  (expand-file-name
   (or e-goodnite-home
       (getenv "GOODNITE_HOME")
       "~/.goodnite")))

(defun e-goodnite-resources--type-plist (type)
  "Return the source plist for consumer TYPE, or nil."
  (cdr (assoc type e-goodnite-resources--type-map)))

(defun e-goodnite-resources--segments (uri)
  "Return non-empty path segments for parsed URI."
  (seq-remove #'string-empty-p
              (split-string (plist-get uri :address) "/")))

;;; Frontmatter parsing

(defun e-goodnite-resources--strip-quotes (value)
  "Return VALUE without matching surrounding quotes."
  (if (and (>= (length value) 2)
           (or (and (string-prefix-p "\"" value) (string-suffix-p "\"" value))
               (and (string-prefix-p "'" value) (string-suffix-p "'" value))))
      (substring value 1 -1)
    value))

(defun e-goodnite-resources--frontmatter-continuation-p (line)
  "Return non-nil when LINE continues the previous scalar.
A continuation is an indented line that is not itself a `key:' or a list item."
  (and (string-match-p "\\`[ \t]+" line)
       (let ((trimmed (string-trim line)))
         (and (not (string-prefix-p "- " trimmed))
              (not (string-match-p "\\`[[:alnum:]_-]+:\\([ \t]\\|\\'\\)" trimmed))))))

(defun e-goodnite-resources--split-frontmatter (content)
  "Split CONTENT into a (FRONTMATTER-ALIST . BODY) cons.
FRONTMATTER-ALIST maps top-level scalar keys to trimmed string values.  YAML
folded/continued scalars (indented continuation lines) are joined onto their
key so a multi-line `description' survives whole; nested mappings and list
values are ignored since only scalars are surfaced.  When CONTENT has no
leading `---' fence, FRONTMATTER-ALIST is nil and BODY is CONTENT."
  (let ((lines (split-string content "\n")))
    (if (not (string= (car lines) "---"))
        (cons nil content)
      (let ((rest (cdr lines))
            (front nil)
            (current-key nil)
            (body-start nil)
            (index 1))
        (catch 'done
          (dolist (line rest)
            (setq index (1+ index))
            (cond
             ((string= line "---")
              (setq body-start index) (throw 'done nil))
             ((and current-key
                   (e-goodnite-resources--frontmatter-continuation-p line))
              (setf (cdr (assoc current-key front))
                    (string-trim
                     (concat (cdr (assoc current-key front)) " "
                             (string-trim line)))))
             ((string-match "\\`\\([[:alnum:]_-]+\\):[ \t]*\\(.*\\)\\'" line)
              (let ((key (match-string 1 line))
                    (value (string-trim (match-string 2 line))))
                (setq current-key nil)
                (unless (or (string-empty-p value)
                            (string-prefix-p ">" value)
                            (string-prefix-p "|" value))
                  (setq current-key key)
                  (push (cons key (e-goodnite-resources--strip-quotes value))
                        front))))
             (t (setq current-key nil)))))
        (cons (nreverse front)
              (if body-start
                  (string-join (nthcdr body-start lines) "\n")
                content))))))

;;; Entry model

(cl-defstruct (e-goodnite-entry (:constructor e-goodnite-entry-create))
  type slug file title when-to-use scope confidence)

(defun e-goodnite-resources--first-body-line (body)
  "Return the first non-empty, non-heading line of BODY, trimmed."
  (catch 'line
    (dolist (raw (split-string body "\n"))
      (let ((line (string-trim raw)))
        (unless (or (string-empty-p line)
                    (string-prefix-p "#" line)
                    (string-prefix-p "---" line))
          (throw 'line line))))
    ""))

(defun e-goodnite-resources--confidence (front)
  "Return a consumer confidence string from FRONTMATTER alist FRONT.
Everything under GOODNITE_HOME is a pre-review proposal, so it reads as mined
unless the frontmatter explicitly marks it reviewed/promoted/published."
  (let ((status (downcase (or (cdr (assoc "status" front)) "candidate"))))
    (if (member status '("promoted" "published" "established" "reviewed"))
        "established"
      "mined (unreviewed)")))

(defun e-goodnite-resources--entry-from-content (type slug file content)
  "Build an entry of TYPE with SLUG from FILE and its CONTENT."
  (pcase-let* ((`(,front . ,body) (e-goodnite-resources--split-frontmatter content))
               (confidence (e-goodnite-resources--confidence front)))
    (pcase type
      ("workflows"
       (e-goodnite-entry-create
        :type type :slug slug :file file
        :title (or (cdr (assoc "name" front)) slug)
        :when-to-use (or (cdr (assoc "description" front))
                         (e-goodnite-resources--first-body-line body))
        :scope nil
        :confidence confidence))
      ("pitfalls"
       (e-goodnite-entry-create
        :type type :slug slug :file file
        :title slug
        :when-to-use (e-goodnite-resources--first-body-line body)
        :scope (let ((near (cdr (assoc "nearest_skill" front))))
                 (and near (not (string-empty-p near)) (format "near %s" near)))
        :confidence confidence))
      ("conventions"
       (let ((root (cdr (assoc "project_root" front))))
         (e-goodnite-entry-create
          :type type :slug slug :file file
          :title slug
          :when-to-use (if (and root (not (string-empty-p root)))
                           (format "Conventions and preferences for %s" root)
                         (format "Cross-project conventions (%s)" slug))
          :scope (and root (not (string-empty-p root)) root)
          :confidence confidence))))))

(defun e-goodnite-resources--read-file (file)
  "Return the text contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun e-goodnite-resources--type-entries (type)
  "Return the list of entries for consumer TYPE."
  (let* ((plist (e-goodnite-resources--type-plist type))
         (subdir (expand-file-name (plist-get plist :subdir)
                                   (e-goodnite-resources--home)))
         (layout (plist-get plist :layout)))
    (when (file-directory-p subdir)
      (pcase layout
        ('dir
         (delq nil
               (mapcar
                (lambda (name)
                  (let* ((dir (expand-file-name name subdir))
                         (file (expand-file-name "SKILL.md" dir)))
                    (when (and (file-directory-p dir) (file-readable-p file))
                      (e-goodnite-resources--entry-from-content
                       type name file (e-goodnite-resources--read-file file)))))
                (directory-files subdir nil "\\`[^.]"))))
        ('file
         (delq nil
               (mapcar
                (lambda (name)
                  (let ((file (expand-file-name name subdir))
                        (slug (file-name-sans-extension name)))
                    (when (file-readable-p file)
                      (e-goodnite-resources--entry-from-content
                       type slug file (e-goodnite-resources--read-file file)))))
                (directory-files subdir nil "\\.md\\'"))))))))

(defun e-goodnite-resources--find-entry (type slug)
  "Return the entry for TYPE and SLUG, or signal if absent."
  (or (seq-find (lambda (entry) (string= (e-goodnite-entry-slug entry) slug))
                (e-goodnite-resources--type-entries type))
      (signal 'e-goodnite-resources-unknown-entry
              (list (format "goodnite://%s/%s" type slug)))))

;;; Rendering

(defun e-goodnite-resources--render-entry (entry)
  "Return the consumer-facing text for ENTRY.
The goodnite-internal frontmatter is dropped; a compact consumer header
precedes the distilled body."
  (pcase-let* ((content (e-goodnite-resources--read-file (e-goodnite-entry-file entry)))
               (`(,_ . ,body) (e-goodnite-resources--split-frontmatter content))
               (type (e-goodnite-entry-type entry)))
    (concat
     (format "# %s\n" (e-goodnite-entry-title entry))
     (format "type: %s\n" (substring type 0 (1- (length type))))
     (format "confidence: %s\n" (e-goodnite-entry-confidence entry))
     (when-let ((scope (e-goodnite-entry-scope entry)))
       (format "scope: %s\n" scope))
     (when-let ((when-to-use (e-goodnite-entry-when-to-use entry)))
       (format "when-to-use: %s\n" when-to-use))
     "\n"
     (string-trim body)
     "\n")))

;;; Glob

(defun e-goodnite-resources--type-entry (type)
  "Return a glob entry describing consumer TYPE."
  (let ((plist (e-goodnite-resources--type-plist type)))
    (list :uri (format "goodnite://%s/" type)
          :name type
          :kind 'directory
          :metadata (list :type type :summary (plist-get plist :summary)))))

(defun e-goodnite-resources--knowledge-entry (entry)
  "Return a glob entry (plist) for knowledge ENTRY."
  (list :uri (format "goodnite://%s/%s"
                     (e-goodnite-entry-type entry)
                     (e-goodnite-entry-slug entry))
        :name (e-goodnite-entry-slug entry)
        :kind 'file
        :metadata (list :type (e-goodnite-entry-type entry)
                        :title (e-goodnite-entry-title entry)
                        :when-to-use (e-goodnite-entry-when-to-use entry)
                        :scope (e-goodnite-entry-scope entry)
                        :confidence (e-goodnite-entry-confidence entry))))

(defun e-goodnite-resources--limit (limit)
  "Return normalized glob LIMIT."
  (cond
   ((null limit) 200)
   ((and (numberp limit) (> limit 0)) (truncate limit))
   (t (signal 'wrong-type-argument (list 'positive-number-p limit)))))

(defun e-goodnite-resources--glob-match-p (pattern case-sensitive name &rest extra)
  "Return non-nil when NAME or any EXTRA text matches glob PATTERN."
  (or (null pattern)
      (cl-some (lambda (text)
                 (and text
                      (e-resource-pattern-glob-match-p pattern text case-sensitive)))
               (cons name extra))))

(defun e-goodnite-resources--apply-query
    (entries sort-by sort-order created-after created-before
             updated-after updated-before)
  "Apply shared resource query controls to glob ENTRIES."
  (e-resource-query-apply
   entries "goodnite"
   '("default" "name" "uri")
   '()
   :sort-by sort-by :sort-order sort-order
   :created-after created-after :created-before created-before
   :updated-after updated-after :updated-before updated-before
   :field-functions
   (list (cons "name" (lambda (entry) (plist-get entry :name)))
         (cons "uri" (lambda (entry) (plist-get entry :uri))))))

(defun e-goodnite-resources--glob
    (uri pattern limit case-sensitive sort-by sort-order
         created-after created-before updated-after updated-before)
  "Glob parsed goodnite URI."
  (let ((segments (e-goodnite-resources--segments uri))
        (actual-limit (e-goodnite-resources--limit limit))
        (actual-case (if (null case-sensitive) t case-sensitive)))
    (pcase segments
      ('()
       (let ((entries (seq-filter
                       (lambda (entry)
                         (e-goodnite-resources--glob-match-p
                          pattern actual-case (plist-get entry :name)))
                       (mapcar #'e-goodnite-resources--type-entry
                               e-goodnite-resources--type-order))))
         (list :resources (vconcat (seq-take entries actual-limit))
               :truncated nil)))
      (`(,type)
       (unless (e-goodnite-resources--type-plist type)
         (signal 'e-goodnite-resources-unknown-type (list type)))
       (let* ((entries
               (seq-filter
                (lambda (entry)
                  (let ((meta (plist-get entry :metadata)))
                    (e-goodnite-resources--glob-match-p
                     pattern actual-case (plist-get entry :name)
                     (plist-get meta :title) (plist-get meta :when-to-use)
                     (plist-get meta :scope))))
                (mapcar #'e-goodnite-resources--knowledge-entry
                        (e-goodnite-resources--type-entries type))))
              (queried (e-goodnite-resources--apply-query
                        entries sort-by sort-order
                        created-after created-before
                        updated-after updated-before))
              (selected (seq-take queried actual-limit)))
         (list :resources (vconcat selected)
               :truncated (> (length queried) actual-limit))))
      (_
       (signal 'e-goodnite-resources-invalid-uri
               (list (format "Invalid goodnite glob root: %s"
                             (plist-get uri :uri))))))))

;;; Read

(defun e-goodnite-resources--apply-line-range (content range)
  "Return CONTENT narrowed by optional line RANGE."
  (if (not range)
      content
    (let ((unit (plist-get range :unit))
          (start (plist-get range :start))
          (end (plist-get range :end)))
      (unless (and (equal unit "line")
                   (integerp start) (> start 0)
                   (or (null end) (and (integerp end) (>= end start))))
        (signal 'wrong-type-argument (list 'line-range-p range)))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (forward-line (1- start))
        (let ((beg (point)))
          (if end
              (forward-line (1+ (- end start)))
            (goto-char (point-max)))
          (buffer-substring-no-properties beg (point)))))))

(defun e-goodnite-resources--read (uri range)
  "Read parsed goodnite URI with optional line RANGE."
  (pcase (e-goodnite-resources--segments uri)
    (`(,type ,slug)
     (unless (e-goodnite-resources--type-plist type)
       (signal 'e-goodnite-resources-unknown-type (list type)))
     (e-goodnite-resources--apply-line-range
      (e-goodnite-resources--render-entry
       (e-goodnite-resources--find-entry type slug))
      range))
    (_
     (signal 'e-goodnite-resources-invalid-uri
             (list (format "goodnite:// read only supports leaf entry URIs: %s"
                           (plist-get uri :uri)))))))

;;; Search

(defun e-goodnite-resources--search-entries (uri)
  "Return the entries in scope for a search over parsed URI."
  (pcase (e-goodnite-resources--segments uri)
    ('()
     (apply #'append
            (mapcar #'e-goodnite-resources--type-entries
                    e-goodnite-resources--type-order)))
    (`(,type)
     (unless (e-goodnite-resources--type-plist type)
       (signal 'e-goodnite-resources-unknown-type (list type)))
     (e-goodnite-resources--type-entries type))
    (`(,type ,slug)
     (list (e-goodnite-resources--find-entry type slug)))
    (_
     (signal 'e-goodnite-resources-invalid-uri
             (list (format "Invalid goodnite search root: %s"
                           (plist-get uri :uri)))))))

(defun e-goodnite-resources--search-lexical (uri query options)
  "Lexical search parsed goodnite URI for QUERY with OPTIONS."
  (let* ((actual-limit (e-resource-pattern-search-limit (plist-get options :limit)))
         (glob-pattern (plist-get options :glob))
         (case-sensitive (plist-get options :case-sensitive))
         (matches nil))
    (dolist (entry (e-goodnite-resources--search-entries uri))
      (when (or (null glob-pattern)
                (e-goodnite-resources--glob-match-p
                 glob-pattern case-sensitive (e-goodnite-entry-slug entry)))
        (let ((entry-uri (format "goodnite://%s/%s"
                                 (e-goodnite-entry-type entry)
                                 (e-goodnite-entry-slug entry))))
          (setq matches
                (append matches
                        (e-resource-pattern-search-matches-in-text
                         entry-uri
                         (e-goodnite-resources--render-entry entry)
                         query options
                         (e-goodnite-entry-slug entry)))))))
    (let ((ranked (e-resource-pattern-rank-search-matches matches (1+ actual-limit))))
      (list :matches (vconcat (seq-take ranked actual-limit))
            :truncated (> (length ranked) actual-limit)))))

(defun e-goodnite-resources--search-scope (uri)
  "Return the type to scope semantic results to, or nil for all knowledge."
  (pcase (e-goodnite-resources--segments uri)
    (`(,type) type)
    (_ nil)))

(defun e-goodnite-resources--run-semantic (query limit)
  "Run the goodnite semantic search program for QUERY.
Return the parsed JSON plist on success, or nil when the program is absent,
fails, or reports no index (so the caller falls back to lexical search)."
  (let ((program (executable-find e-goodnite-search-program)))
    (when program
      (condition-case nil
          (with-temp-buffer
            (let* ((default-directory (e-goodnite-resources--home))
                   (status (process-file
                            program nil (list t nil) nil
                            "knowledge-search" query
                            "--limit" (number-to-string limit))))
              (when (eq status 0)
                (goto-char (point-max))
                (forward-line -1)
                (let ((line (string-trim
                             (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))))
                  (unless (string-empty-p line)
                    (let ((parsed (json-parse-string
                                   line :object-type 'plist :array-type 'list
                                   :false-object nil :null-object nil)))
                      (and (plist-get parsed :indexed) parsed)))))))
        (error nil)))))

(defun e-goodnite-resources--semantic-matches (result scope limit)
  "Return RESULT's matches as e match plists, filtered to SCOPE, capped at LIMIT."
  (let ((matches nil)
        (rank 0))
    (dolist (m (plist-get result :matches))
      (when (or (null scope) (equal (plist-get m :type) scope))
        (setq rank (1+ rank))
        (push (list :uri (plist-get m :uri)
                    :line (or (plist-get m :line) 1)
                    :column (or (plist-get m :column) 1)
                    :text (or (plist-get m :text) (plist-get m :title) "")
                    :score (or (plist-get m :score) 0)
                    :rank rank)
              matches)))
    (vconcat (seq-take (nreverse matches) limit))))

(defun e-goodnite-resources--search (uri query options)
  "Search parsed goodnite URI for QUERY with OPTIONS.
Try semantic retrieval first (goodnite `knowledge-search'); fall back to
lexical matching when the index or program is unavailable.  A leaf-slug search
is always lexical, since it targets one known entry."
  (let ((leaf-p (= (length (e-goodnite-resources--segments uri)) 2)))
    (or (and e-goodnite-search-semantic
             (not leaf-p)
             (let* ((limit (e-resource-pattern-search-limit
                            (plist-get options :limit)))
                    (result (e-goodnite-resources--run-semantic query limit)))
               (when result
                 (list :matches (e-goodnite-resources--semantic-matches
                                 result (e-goodnite-resources--search-scope uri)
                                 limit)
                       :truncated (and (plist-get result :truncated) t)))))
        (e-goodnite-resources--search-lexical uri query options))))

(defun e-goodnite-resources--access-log-path ()
  "Return the path of the daydream access log under the knowledge base."
  (expand-file-name "state/daydream_access.jsonl"
                    (e-goodnite-resources--home)))

(defun e-goodnite-resources--scrub (text)
  "Return TEXT reduced to a single line with control characters removed.
The query is logged as a demand signal, not stored verbatim; strip C0 control
characters so a stray NUL or newline never corrupts the JSONL line."
  (when (stringp text)
    (replace-regexp-in-string "[\000-\037]+" " " (string-trim text))))

(defun e-goodnite-resources--new-event-id ()
  "Return a unique identity for one observed Goodnite access."
  (secure-hash
   'sha256
   (format "%S\0%d\0%d\0%d"
           (current-time) (emacs-pid) (random most-positive-fixnum)
           (cl-incf e-goodnite-resources--event-sequence))))

(defun e-goodnite-resources--record-access (kind entry-uri query context)
  "Append one access record for a KIND consultation of ENTRY-URI.
KIND is `read' or `search'.  QUERY is the search text (nil for a read).
CONTEXT carries the registration `:session-id' and `:turn-id'.  The legacy log
is best-effort; a configured SQLite demand write is authoritative and errors
surface to the consultation caller."
  (when e-goodnite-track-access
    (let* ((record
            (list :kind (symbol-name kind)
                  :entry_uri entry-uri
                  :query (e-goodnite-resources--scrub query)
                  :session_id (plist-get context :session-id)
                  :turn_id (plist-get context :turn-id)
                  :engine "e"
                  :ts (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
           (event-id
            ;; Each consultation is a distinct demand observation.  Generate
            ;; once before submission; runtime command reconciliation retains
            ;; this exact body if the worker response is lost.
            (e-goodnite-resources--new-event-id)))
      (if e-goodnite-resources-storage
          (e-goodnite-storage-append
           e-goodnite-resources-storage event-id record)
        (condition-case nil
            (let ((path (e-goodnite-resources--access-log-path)))
              (make-directory (file-name-directory path) t)
              (let ((line
                     (json-serialize record :null-object nil
                                     :false-object nil)))
                (write-region (concat line "\n") nil path 'append 'silent)))
          (error nil))))))

(defun e-goodnite-resources-demand-page (&optional after limit)
  "Return one bounded durable Goodnite demand page."
  (unless e-goodnite-resources-storage
    (signal 'e-goodnite-storage-error
            (list "Goodnite demand storage is not configured")))
  (e-goodnite-storage-page e-goodnite-resources-storage after limit
                           e-goodnite-demand-consumer))

(defun e-goodnite-resources-ack-demand (position)
  "Acknowledge durable Goodnite demand through POSITION."
  (unless e-goodnite-resources-storage
    (signal 'e-goodnite-storage-error
            (list "Goodnite demand storage is not configured")))
  (e-goodnite-storage-ack e-goodnite-resources-storage position
                          e-goodnite-demand-consumer))

(defun e-goodnite-resources-cleanup-demand (&optional limit)
  "Delete one bounded acknowledged Goodnite demand prefix."
  (unless e-goodnite-resources-storage
    (signal 'e-goodnite-storage-error
            (list "Goodnite demand storage is not configured")))
  (e-goodnite-storage-cleanup e-goodnite-resources-storage limit
                              e-goodnite-demand-consumer))

;;; Registration

(cl-defun e-goodnite-resources-register-resource-methods
    (registry &rest context)
  "Register goodnite:// resource methods in REGISTRY."
  (dolist (method
           (list
            (e-resource-method-create
             :scheme "goodnite"
             :operation e-operation-read
             :description
             (concat
              "Read one piece of distilled task knowledge mined from your own "
              "past sessions. Glob or search first, then read a leaf URI: "
              "goodnite://workflows/<slug> (how to do a recurring task), "
              "goodnite://pitfalls/<slug> (a failure mode to avoid), or "
              "goodnite://conventions/<slug> (a project/tool convention). "
              "Read-only. Weight a hit by its confidence: `established' is "
              "human-reviewed, `mined (unreviewed)' is a strong lead to "
              "sanity-check.")
             :uri-patterns '("goodnite://workflows/<slug>"
                             "goodnite://pitfalls/<slug>"
                             "goodnite://conventions/<slug>")
             :range-modes '("line")
             :handler (lambda (uri range)
                        (prog1 (e-goodnite-resources--read uri range)
                          (e-goodnite-resources--record-access
                           'read (plist-get uri :uri) nil context))))
            (e-resource-method-create
             :scheme "goodnite"
             :operation e-operation-glob
             :description
             (concat
              "Discover distilled task knowledge. Workflow: glob goodnite:// "
              "to list the three knowledge types (workflows, pitfalls, "
              "conventions); glob goodnite://<type>/ to list entries with a "
              "one-line when-to-use, scope, and confidence.")
             :uri-patterns '("goodnite://" "goodnite://<type>/")
             :handler (lambda (uri pattern limit case-sensitive sort-by sort-order
                                   created-after created-before
                                   updated-after updated-before)
                        (e-goodnite-resources--glob
                         uri pattern limit case-sensitive sort-by sort-order
                         created-after created-before
                         updated-after updated-before)))
            (e-resource-method-create
             :scheme "goodnite"
             :operation e-operation-search
             :description
             (concat
              "Search distilled task knowledge by the task in front of you. "
              "Search goodnite:// across all knowledge, or goodnite://<type>/ "
              "to scope to workflows, pitfalls, or conventions.")
             :uri-patterns '("goodnite://" "goodnite://<type>/")
             :handler (lambda (uri query options)
                        (let ((result (e-goodnite-resources--search
                                       uri query options)))
                          (dolist (m (append (plist-get result :matches) nil))
                            (e-goodnite-resources--record-access
                             'search (plist-get m :uri) query context))
                          result)))))
    (e-resources-register registry method)))

(provide 'e-goodnite-resources)

;;; e-goodnite-resources.el ends here
