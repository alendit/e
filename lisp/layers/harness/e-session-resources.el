;;; e-session-resources.el --- Read-only session:// resources for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Read-only resources that expose persisted agent sessions through a semantic
;; session:// URI scheme.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-operations)
(require 'e-resource-patterns)
(require 'e-resource-query)
(require 'e-resources)
(require 'e-session)

(define-error 'e-session-resources-invalid-uri
  "session:// resource URI is invalid")
(define-error 'e-session-resources-missing-harness
  "session:// resource access requires a harness")
(define-error 'e-session-resources-unknown-engine
  "session:// engine is not registered")
(define-error 'e-session-resources-unknown-session
  "session:// session does not exist")
(define-error 'e-session-resources-unsupported-projection
  "session:// session projection is not supported")

(cl-defstruct (e-session-resource-engine
               (:constructor e-session-resource-engine-create))
  id
  display-name
  adapter
  metadata)

(defcustom e-session-resource-engines nil
  "Additional opt-in session engines exposed through session://.

Each entry is either an `e-session-resource-engine' object or a plist with
`:id', `:display-name', `:adapter', and optional `:metadata'.  The built-in
engine id `e' is always registered from the active harness session store.

An adapter is a plist that provides function-valued operations named
`:list-sessions', `:render-summary', `:render-messages', and optional
`:render-activity', `:render-events', `:render-compactions',
`:render-provider-anchors', and `:projections'.  Adapter functions receive the
engine object as their first argument.  Session-specific renderers receive the
session id as their second argument."
  :type 'sexp
  :group 'e)

(defcustom e-session-resources-default-limit 100
  "Default maximum session:// glob or search results."
  :type 'integer
  :group 'e)

(defconst e-session-resources--projection-order
  '("summary" "messages" "activity" "events" "compactions" "provider-anchors")
  "Projection order for session:// session roots.")

(defun e-session-resources--require-harness (harness)
  "Signal unless HARNESS is an e harness."
  (unless (e-harness-p harness)
    (signal 'e-session-resources-missing-harness
            (list "session:// resources require an active harness"))))

(defun e-session-resources--engine-id-string (id)
  "Return stable string form for engine ID."
  (cond
   ((symbolp id) (symbol-name id))
   ((stringp id) id)
   (t (signal 'wrong-type-argument (list 'string-or-symbol-p id)))))

(defun e-session-resources--configured-engine (entry)
  "Return configured engine object for ENTRY."
  (cond
   ((e-session-resource-engine-p entry) entry)
   ((and (listp entry) (plist-get entry :id))
    (e-session-resource-engine-create
     :id (e-session-resources--engine-id-string (plist-get entry :id))
     :display-name (or (plist-get entry :display-name)
                       (plist-get entry :name)
                       (e-session-resources--engine-id-string
                        (plist-get entry :id)))
     :adapter (plist-get entry :adapter)
     :metadata (plist-get entry :metadata)))
   (t
    (signal 'wrong-type-argument
            (list 'e-session-resource-engine-p entry)))))

(defun e-session-resources--builtin-engine (harness)
  "Return the built-in e engine for HARNESS."
  (e-session-resource-engine-create
   :id "e"
   :display-name "e"
   :adapter (list :kind 'e :harness harness)
   :metadata (list :built-in t)))

(defun e-session-resources--engines (harness)
  "Return enabled session resource engines for HARNESS."
  (cons (e-session-resources--builtin-engine harness)
        (mapcar #'e-session-resources--configured-engine
                e-session-resource-engines)))

(defun e-session-resources--find-engine (harness engine-id)
  "Return enabled ENGINE-ID for HARNESS, or signal."
  (or (seq-find (lambda (engine)
                  (string= (e-session-resource-engine-id engine) engine-id))
                (e-session-resources--engines harness))
      (signal 'e-session-resources-unknown-engine
              (list (format "Unknown session engine: %s" engine-id)))))

(defun e-session-resources--adapter-get (adapter key)
  "Return ADAPTER operation KEY."
  (cond
   ((and (listp adapter) (plist-member adapter key))
    (plist-get adapter key))
   (t nil)))

(defun e-session-resources--adapter-call (engine key &rest args)
  "Call ENGINE adapter operation KEY with ARGS."
  (let* ((adapter (e-session-resource-engine-adapter engine))
         (function (e-session-resources--adapter-get adapter key)))
    (unless (functionp function)
      (signal 'e-session-resources-unsupported-projection
              (list (format "Session engine %s does not support %s"
                            (e-session-resource-engine-id engine)
                            key))))
    (apply function engine args)))

(defun e-session-resources--e-store (engine)
  "Return the e session store for built-in ENGINE."
  (let ((harness (plist-get (e-session-resource-engine-adapter engine)
                            :harness)))
    (e-session-resources--require-harness harness)
    (e-harness-sessions harness)))

(defun e-session-resources--e-session (engine session-id)
  "Return built-in e SESSION-ID for ENGINE."
  (condition-case nil
      (e-session-get (e-session-resources--e-store engine) session-id)
    (e-session-missing
     (signal 'e-session-resources-unknown-session
             (list (format "Unknown session id: %s" session-id))))))

(defun e-session-resources--e-session-list (engine)
  "List built-in e sessions for ENGINE."
  (e-session-list (e-session-resources--e-store engine)))

(defun e-session-resources--engine-list-sessions (engine)
  "Return public session index entries for ENGINE."
  (if (eq (plist-get (e-session-resource-engine-adapter engine) :kind) 'e)
      (e-session-resources--e-session-list engine)
    (e-session-resources--adapter-call engine :list-sessions)))

(defun e-session-resources--session-title (session)
  "Return display title for SESSION index entry."
  (or (plist-get session :title)
      (plist-get session :name)
      (plist-get session :id)
      "untitled"))

(defun e-session-resources--session-id (session)
  "Return id for SESSION index entry."
  (or (plist-get session :id)
      (plist-get session :session-id)))

(defun e-session-resources--metadata (session)
  "Return stable metadata for SESSION index entry."
  (let ((metadata (copy-sequence (or (plist-get session :metadata) nil))))
    (dolist (key '(:project-root :harness-instance-id))
      (when-let ((value (plist-get metadata key)))
        (setq metadata (plist-put metadata key value))))
    (dolist (key '(:created-at :updated-at :last-message-at))
      (when-let ((value (plist-get session key)))
        (setq metadata (plist-put metadata key value))))
    metadata))

(defun e-session-resources--engine-entry (engine)
  "Return glob entry for ENGINE."
  (let ((id (e-session-resource-engine-id engine)))
    (list :uri (format "session://%s/sessions/" id)
          :name id
          :kind 'engine
          :metadata (append
                     (list :engine-id id
                           :display-name
                           (e-session-resource-engine-display-name engine))
                     (e-session-resource-engine-metadata engine)))))

(defun e-session-resources--session-entry (engine session)
  "Return glob entry for SESSION in ENGINE."
  (let ((engine-id (e-session-resource-engine-id engine))
        (session-id (e-session-resources--session-id session)))
    (list :uri (format "session://%s/sessions/%s/" engine-id session-id)
          :name (e-session-resources--session-title session)
          :kind 'session
          :metadata (append
                     (list :engine-id engine-id
                           :id session-id
                           :title (e-session-resources--session-title session))
                     (e-session-resources--metadata session)))))

(defun e-session-resources--projection-uri (engine-id session-id projection)
  "Return URI for ENGINE-ID SESSION-ID PROJECTION."
  (format "session://%s/sessions/%s/%s" engine-id session-id projection))

(defun e-session-resources--projection-entry (engine session-id projection)
  "Return glob entry for one session PROJECTION."
  (let ((engine-id (e-session-resource-engine-id engine)))
    (list :uri (e-session-resources--projection-uri
                engine-id session-id projection)
          :name projection
          :kind 'file
          :metadata (list :engine-id engine-id
                          :id session-id
                          :projection projection))))

(defun e-session-resources--e-projection-supported-p (_engine _session-id projection)
  "Return non-nil when built-in e PROJECTION exists for SESSION-ID."
  (member projection e-session-resources--projection-order))

(defun e-session-resources--engine-projections (engine session-id)
  "Return supported projection names for ENGINE SESSION-ID."
  (if (eq (plist-get (e-session-resource-engine-adapter engine) :kind) 'e)
      (progn
        (e-session-resources--e-session engine session-id)
        (seq-filter (lambda (projection)
                      (e-session-resources--e-projection-supported-p
                       engine session-id projection))
                    e-session-resources--projection-order))
    (let ((projections
           (if (functionp (e-session-resources--adapter-get
                           (e-session-resource-engine-adapter engine)
                           :projections))
               (e-session-resources--adapter-call engine :projections session-id)
             '("summary" "messages"))))
      (mapcar (lambda (projection)
                (if (symbolp projection) (symbol-name projection) projection))
              projections))))

(defun e-session-resources--format-value (value)
  "Return stable text for VALUE."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (prin1-to-string value))))

(defun e-session-resources--insert-field (label value)
  "Insert LABEL and VALUE when VALUE is non-nil."
  (when value
    (insert (format "- %s: %s\n" label
                    (e-session-resources--format-value value)))))

(defun e-session-resources--message-content (message)
  "Return readable content for MESSAGE."
  (or (plist-get message :content)
      (plist-get message :text)
      ""))

(defun e-session-resources--render-entry-list (title entries)
  "Render TITLE and plist ENTRIES as text."
  (with-temp-buffer
    (insert (format "# %s\n\n" title))
    (if entries
        (let ((index 1))
          (dolist (entry entries)
            (insert (format "* Entry %d\n" index))
            (e-session-resources--insert-field "id" (plist-get entry :id))
            (e-session-resources--insert-field "parent-id" (plist-get entry :parent-id))
            (e-session-resources--insert-field "created-at" (plist-get entry :created-at))
            (e-session-resources--insert-field "turn-id" (plist-get entry :turn-id))
            (e-session-resources--insert-field "event-type" (plist-get entry :event-type))
            (e-session-resources--insert-field "provider-id" (plist-get entry :provider-id))
            (e-session-resources--insert-field "model" (plist-get entry :model))
            (e-session-resources--insert-field "covered-entry-id" (plist-get entry :covered-entry-id))
            (e-session-resources--insert-field "summary" (plist-get entry :summary))
            (e-session-resources--insert-field "range" (plist-get entry :range))
            (e-session-resources--insert-field "first-kept-entry-id" (plist-get entry :first-kept-entry-id))
            (e-session-resources--insert-field "tokens-before" (plist-get entry :tokens-before))
            (e-session-resources--insert-field "tokens-kept" (plist-get entry :tokens-kept))
            (e-session-resources--insert-field "payload" (plist-get entry :payload))
            (e-session-resources--insert-field "metadata" (plist-get entry :metadata))
            (e-session-resources--insert-field "fingerprints" (plist-get entry :fingerprints))
            (insert "\n")
            (setq index (1+ index))))
      (insert "No records.\n"))
    (buffer-string)))

(defun e-session-resources--render-e-summary (engine session-id)
  "Render built-in e summary for SESSION-ID."
  (let* ((store (e-session-resources--e-store engine))
         (_session (e-session-resources--e-session engine session-id))
         (index-entry (or (seq-find
                           (lambda (entry)
                             (equal (plist-get entry :id) session-id))
                           (e-session-list store))
                          (list :id session-id)))
         (engine-id (e-session-resource-engine-id engine))
         (projections (e-session-resources--engine-projections engine session-id)))
    (with-temp-buffer
      (insert (format "# Session %s\n\n" session-id))
      (insert "Metadata:\n")
      (e-session-resources--insert-field "engine-id" engine-id)
      (e-session-resources--insert-field "title" (e-session-resources--session-title index-entry))
      (e-session-resources--insert-field "name" (plist-get index-entry :name))
      (e-session-resources--insert-field "created-at" (plist-get index-entry :created-at))
      (e-session-resources--insert-field "updated-at" (plist-get index-entry :updated-at))
      (e-session-resources--insert-field "last-message-at" (plist-get index-entry :last-message-at))
      (e-session-resources--insert-field "message-count" (plist-get index-entry :message-count))
      (e-session-resources--insert-field "project-root" (plist-get (plist-get index-entry :metadata) :project-root))
      (e-session-resources--insert-field "harness-instance-id" (plist-get (plist-get index-entry :metadata) :harness-instance-id))
      (insert "\nTranscript overview:\n")
      (insert (format "- messages: %s\n" (length (e-session-messages store session-id))))
      (insert (format "- activity events: %s\n" (length (e-session-activity-events store session-id))))
      (insert (format "- session events: %s\n" (length (e-session-session-events store session-id))))
      (insert (format "- compactions: %s\n" (length (e-session-compactions store session-id))))
      (insert (format "- provider anchors: %s\n" (length (e-session-provider-anchors store session-id))))
      (insert "\nReadable subresources:\n")
      (dolist (projection projections)
        (insert (format "- %s\n"
                        (e-session-resources--projection-uri
                         engine-id session-id projection))))
      (buffer-string))))

(defun e-session-resources--render-e-messages (engine session-id)
  "Render built-in e messages for SESSION-ID."
  (let ((messages (e-session-messages (e-session-resources--e-store engine)
                                      session-id))
        (index 1))
    (with-temp-buffer
      (insert (format "# Session %s messages\n\n" session-id))
      (if messages
          (dolist (message messages)
            (insert (format "* Message %d\n" index))
            (e-session-resources--insert-field "id" (plist-get message :id))
            (e-session-resources--insert-field "parent-id" (plist-get message :parent-id))
            (e-session-resources--insert-field "turn-id" (plist-get message :turn-id))
            (e-session-resources--insert-field "role" (plist-get message :role))
            (e-session-resources--insert-field "created-at" (plist-get message :created-at))
            (insert "\n")
            (insert (e-session-resources--message-content message))
            (insert "\n\n")
            (setq index (1+ index)))
        (insert "No messages.\n"))
      (buffer-string))))

(defun e-session-resources--render-e-projection (engine session-id projection)
  "Render built-in e SESSION-ID PROJECTION."
  (pcase projection
    ("summary" (e-session-resources--render-e-summary engine session-id))
    ("messages" (e-session-resources--render-e-messages engine session-id))
    ("activity" (e-session-resources--render-entry-list
                  (format "Session %s activity" session-id)
                  (e-session-activity-events
                   (e-session-resources--e-store engine) session-id)))
    ("events" (e-session-resources--render-entry-list
                (format "Session %s events" session-id)
                (e-session-session-events
                 (e-session-resources--e-store engine) session-id)))
    ("compactions" (e-session-resources--render-entry-list
                    (format "Session %s compactions" session-id)
                    (e-session-compactions
                     (e-session-resources--e-store engine) session-id)))
    ("provider-anchors" (e-session-resources--render-entry-list
                         (format "Session %s provider anchors" session-id)
                         (e-session-provider-anchors
                          (e-session-resources--e-store engine) session-id)))
    (_
     (signal 'e-session-resources-unsupported-projection
             (list (format "Unsupported session projection: %s"
                           projection))))))

(defun e-session-resources--adapter-render-key (projection)
  "Return adapter renderer key for PROJECTION."
  (pcase projection
    ("summary" :render-summary)
    ("messages" :render-messages)
    ("activity" :render-activity)
    ("events" :render-events)
    ("compactions" :render-compactions)
    ("provider-anchors" :render-provider-anchors)
    (_ (intern (format ":render-%s" projection)))))

(defun e-session-resources--render-projection (engine session-id projection)
  "Render ENGINE SESSION-ID PROJECTION."
  (unless (member projection (e-session-resources--engine-projections
                              engine session-id))
    (signal 'e-session-resources-unsupported-projection
            (list (format "Unsupported session projection: %s"
                          projection))))
  (if (eq (plist-get (e-session-resource-engine-adapter engine) :kind) 'e)
      (e-session-resources--render-e-projection engine session-id projection)
    (e-session-resources--adapter-call
     engine
     (e-session-resources--adapter-render-key projection)
     session-id)))

(defun e-session-resources--apply-line-range (content range)
  "Return CONTENT narrowed by optional line RANGE."
  (if (not range)
      content
    (let ((unit (plist-get range :unit))
          (start (plist-get range :start))
          (end (plist-get range :end)))
      (unless (and (equal unit "line")
                   (integerp start)
                   (> start 0)
                   (or (null end)
                       (and (integerp end) (>= end start))))
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

(defun e-session-resources--segments (uri)
  "Return path segments for parsed session URI."
  (split-string (plist-get uri :address) "/" t))

(defun e-session-resources--read (harness uri range)
  "Read parsed session URI for HARNESS with RANGE."
  (e-session-resources--require-harness harness)
  (pcase (e-session-resources--segments uri)
    (`(,engine-id "sessions" ,session-id ,projection)
     (let ((engine (e-session-resources--find-engine harness engine-id)))
       (e-session-resources--apply-line-range
        (e-session-resources--render-projection engine session-id projection)
        range)))
    (_
     (signal 'e-session-resources-invalid-uri
             (list (format "session:// read only supports leaf projection URIs: %s"
                           (plist-get uri :uri)))))))

(defun e-session-resources--limit (limit)
  "Return normalized LIMIT."
  (cond
   ((null limit) e-session-resources-default-limit)
   ((and (integerp limit) (> limit 0)) limit)
   ((and (numberp limit) (> limit 0)) (truncate limit))
   (t (signal 'wrong-type-argument (list 'positive-number-p limit)))))

(defun e-session-resources--glob-match-p (pattern case-sensitive &rest values)
  "Return non-nil when PATTERN matches any string in VALUES."
  (let ((pattern (or pattern "*")))
    (seq-some (lambda (value)
                (and (stringp value)
                     (e-resource-pattern-glob-match-p
                      pattern value case-sensitive)))
              values)))

(defun e-session-resources--entry-updated-at (entry)
  "Return session ENTRY updated timestamp using documented fallbacks."
  (let ((metadata (plist-get entry :metadata)))
    (or (plist-get metadata :last-message-at)
        (plist-get metadata :updated-at)
        (plist-get metadata :created-at))))

(defun e-session-resources--query-field-functions ()
  "Return session:// resource query field functions."
  `(("name" . ,(lambda (entry) (plist-get entry :name)))
    ("uri" . ,(lambda (entry) (plist-get entry :uri)))
    ("title" . ,(lambda (entry) (plist-get (plist-get entry :metadata) :title)))
    ("id" . ,(lambda (entry) (plist-get (plist-get entry :metadata) :id)))
    ("created-at" . ,(lambda (entry)
                         (plist-get (plist-get entry :metadata) :created-at)))
    ("updated-at" . ,#'e-session-resources--entry-updated-at)
    ("last-message-at" . ,(lambda (entry)
                              (plist-get (plist-get entry :metadata)
                                         :last-message-at)))))

(defun e-session-resources--apply-session-query
    (entries sort-by sort-order created-after created-before
             updated-after updated-before)
  "Apply session query controls to session ENTRIES."
  (e-resource-query-apply
   entries
   "session"
   '("default" "last-message-at" "updated-at" "created-at" "title" "id" "name" "uri")
   '("created-at" "updated-at")
   :sort-by sort-by
   :sort-order sort-order
   :created-after created-after
   :created-before created-before
   :updated-after updated-after
   :updated-before updated-before
   :field-functions (e-session-resources--query-field-functions)))

(defun e-session-resources--apply-simple-query
    (entries sort-by sort-order created-after created-before
             updated-after updated-before)
  "Apply simple session:// query controls to non-session ENTRIES."
  (e-resource-query-apply
   entries
   "session"
   '("default" "name" "uri")
   nil
   :sort-by sort-by
   :sort-order sort-order
   :created-after created-after
   :created-before created-before
   :updated-after updated-after
   :updated-before updated-before
   :field-functions (e-session-resources--query-field-functions)))

(defun e-session-resources--glob-engines
    (harness pattern limit case-sensitive &optional sort-by sort-order
             created-after created-before updated-after updated-before)
  "Glob enabled engines for HARNESS."
  (let* ((actual-limit (e-session-resources--limit limit))
         (actual-case-sensitive (if (null case-sensitive) t case-sensitive))
         (entries (seq-filter
                   (lambda (entry)
                     (e-session-resources--glob-match-p
                      pattern actual-case-sensitive
                      (plist-get entry :name)
                      (plist-get (plist-get entry :metadata) :display-name)))
                   (mapcar #'e-session-resources--engine-entry
                           (e-session-resources--engines harness))))
         (queried (e-session-resources--apply-simple-query
                   entries sort-by sort-order created-after created-before
                   updated-after updated-before))
         (selected (seq-take queried actual-limit)))
    (list :resources (vconcat selected)
          :truncated (> (length queried) actual-limit))))

(defun e-session-resources--glob-sessions
    (engine pattern limit case-sensitive &optional sort-by sort-order
            created-after created-before updated-after updated-before)
  "Glob sessions for ENGINE."
  (let* ((actual-limit (e-session-resources--limit limit))
         (actual-case-sensitive (if (null case-sensitive) t case-sensitive))
         (entries (seq-filter
                   (lambda (entry)
                     (e-session-resources--glob-match-p
                      pattern actual-case-sensitive
                      (plist-get (plist-get entry :metadata) :id)
                      (plist-get entry :name)
                      (plist-get (plist-get entry :metadata) :title)))
                   (mapcar (lambda (session)
                             (e-session-resources--session-entry
                              engine session))
                           (e-session-resources--engine-list-sessions
                            engine))))
         (queried (e-session-resources--apply-session-query
                   entries sort-by sort-order created-after created-before
                   updated-after updated-before))
         (selected (seq-take queried actual-limit)))
    (list :resources (vconcat selected)
          :truncated (> (length queried) actual-limit))))

(defun e-session-resources--glob-projections
    (engine session-id pattern limit case-sensitive &optional sort-by sort-order
            created-after created-before updated-after updated-before)
  "Glob projections for ENGINE SESSION-ID."
  (let* ((actual-limit (e-session-resources--limit limit))
         (actual-case-sensitive (if (null case-sensitive) t case-sensitive))
         (entries (seq-filter
                   (lambda (entry)
                     (e-session-resources--glob-match-p
                      pattern actual-case-sensitive
                      (plist-get entry :name)))
                   (mapcar (lambda (projection)
                             (e-session-resources--projection-entry
                              engine session-id projection))
                           (e-session-resources--engine-projections
                            engine session-id))))
         (queried (e-session-resources--apply-simple-query
                   entries sort-by sort-order created-after created-before
                   updated-after updated-before))
         (selected (seq-take queried actual-limit)))
    (list :resources (vconcat selected)
          :truncated (> (length queried) actual-limit))))

(defun e-session-resources--glob
    (harness uri pattern limit case-sensitive &optional sort-by sort-order
             created-after created-before updated-after updated-before)
  "Glob parsed session URI for HARNESS."
  (e-session-resources--require-harness harness)
  (pcase (e-session-resources--segments uri)
    ('()
     (e-session-resources--glob-engines
      harness pattern limit case-sensitive sort-by sort-order
      created-after created-before updated-after updated-before))
    (`(,engine-id "sessions")
     (e-session-resources--glob-sessions
      (e-session-resources--find-engine harness engine-id)
      pattern limit case-sensitive sort-by sort-order
      created-after created-before updated-after updated-before))
    (`(,engine-id "sessions" ,session-id)
     (e-session-resources--glob-projections
      (e-session-resources--find-engine harness engine-id)
      session-id pattern limit case-sensitive sort-by sort-order
      created-after created-before updated-after updated-before))
    (_
     (signal 'e-session-resources-invalid-uri
             (list (format "Invalid session glob root: %s"
                           (plist-get uri :uri)))))))

(defun e-session-resources--search-record (uri content query options limit)
  "Return matches in CONTENT for URI, capped to LIMIT."
  (let* ((case-fold-search (not (plist-get options :case-sensitive)))
         (regexp (e-resource-pattern-search-emacs-regexp query options))
         (lines (split-string content "\n"))
         (line-number 1)
         matches)
    (dolist (line lines)
      (when (and (< (length matches) limit)
                 (string-match regexp line))
        (push (list :uri uri
                    :line line-number
                    :column (1+ (match-beginning 0))
                    :text line)
              matches))
      (setq line-number (1+ line-number)))
    (nreverse matches)))

(defun e-session-resources--search-glob-match-p
    (glob-pattern engine-id session-id title projection)
  "Return non-nil when GLOB-PATTERN permits a search target.
Session-id and title matches select the default messages projection only.
Optional projections must be named by projection or by full path."
  (or (null glob-pattern)
      (let ((case-sensitive t)
            (path (format "%s/sessions/%s/%s"
                          engine-id session-id projection)))
        (or (e-resource-pattern-glob-match-p glob-pattern path case-sensitive)
            (e-resource-pattern-glob-match-p
             glob-pattern projection case-sensitive)
            (and (string= projection "messages")
                 (or (e-resource-pattern-glob-match-p
                      glob-pattern session-id case-sensitive)
                     (e-resource-pattern-glob-match-p
                      glob-pattern title case-sensitive)))))))

(defun e-session-resources--search-targets-for-engine
    (engine session-id projection all-projections)
  "Return search targets for ENGINE narrowed by SESSION-ID and PROJECTION.
When ALL-PROJECTIONS is non-nil, include every supported projection for matching
sessions.  Otherwise session roots search messages by default."
  (let (targets)
    (dolist (session (e-session-resources--engine-list-sessions engine))
      (let ((id (e-session-resources--session-id session)))
        (when (or (null session-id) (string= id session-id))
          ;; A session listed in the index may have no readable transcript,
          ;; e.g. a dangling index entry whose JSONL file was deleted. Such a
          ;; session yields no searchable content, so skip it instead of
          ;; aborting the whole search.
          (let ((available
                 (condition-case nil
                     (e-session-resources--engine-projections engine id)
                   (e-session-resources-unknown-session nil))))
            (when available
              (let ((projections
                     (cond
                      (projection (list projection))
                      (all-projections available)
                      (t '("messages")))))
                (dolist (candidate projections)
                  (when (member candidate available)
                    (push (list :engine engine
                                :session session
                                :projection candidate)
                          targets)))))))))
    (nreverse targets)))

(defun e-session-resources--search-targets (harness uri all-projections)
  "Return search targets for parsed URI in HARNESS.
ALL-PROJECTIONS means a glob narrowed the search enough to include optional
projections."
  (pcase (e-session-resources--segments uri)
    ('()
     (apply #'append
            (mapcar (lambda (engine)
                      (e-session-resources--search-targets-for-engine
                       engine nil nil all-projections))
                    (e-session-resources--engines harness))))
    (`(,engine-id "sessions")
     (e-session-resources--search-targets-for-engine
      (e-session-resources--find-engine harness engine-id)
      nil nil all-projections))
    (`(,engine-id "sessions" ,session-id)
     (e-session-resources--search-targets-for-engine
      (e-session-resources--find-engine harness engine-id)
      session-id nil all-projections))
    (`(,engine-id "sessions" ,session-id ,projection)
     (e-session-resources--search-targets-for-engine
      (e-session-resources--find-engine harness engine-id)
      session-id projection all-projections))
    (_
     (signal 'e-session-resources-invalid-uri
             (list (format "Invalid session search root: %s"
                           (plist-get uri :uri)))))))

(defun e-session-resources--search-target-resource (target index)
  "Return resource candidate for session search TARGET at INDEX."
  (let* ((engine (plist-get target :engine))
         (session (plist-get target :session))
         (session-id (e-session-resources--session-id session))
         (projection (plist-get target :projection))
         (engine-id (e-session-resource-engine-id engine))
         (entry (e-session-resources--session-entry engine session)))
    (append (copy-sequence entry)
            (list :uri (e-session-resources--projection-uri
                        engine-id session-id projection)
                  :projection projection
                  :index index))))

(defun e-session-resources--query-search-targets (targets options)
  "Apply resource candidate controls in OPTIONS to session search TARGETS."
  (let ((target-by-index (make-hash-table :test 'eql))
        resources
        (index 0))
    (dolist (target targets)
      (puthash index target target-by-index)
      (push (e-session-resources--search-target-resource target index) resources)
      (setq index (1+ index)))
    (setq resources (nreverse resources))
    (delq nil
          (mapcar (lambda (resource)
                    (gethash (plist-get resource :index) target-by-index))
                  (e-resource-query-apply-search
                   resources
                   "session"
                   '("default" "last-message-at" "updated-at" "created-at"
                     "title" "id" "name" "uri")
                   '("created-at" "updated-at")
                   options
                   (e-session-resources--query-field-functions))))))

(defun e-session-resources--search (harness uri query options)
  "Search parsed session URI for HARNESS."
  (e-session-resources--require-harness harness)
  (let* ((actual-limit (e-session-resources--limit (plist-get options :limit)))
         (collection-limit (1+ actual-limit))
         (glob-pattern (plist-get options :glob))
         (targets (e-session-resources--query-search-targets
                   (e-session-resources--search-targets
                    harness uri (and glob-pattern t))
                   options))
         matches)
    (dolist (target targets)
      (let* ((engine (plist-get target :engine))
             (session (plist-get target :session))
             (session-id (e-session-resources--session-id session))
             (title (e-session-resources--session-title session))
             (projection (plist-get target :projection))
             (engine-id (e-session-resource-engine-id engine))
             (resource-uri (e-session-resources--projection-uri
                            engine-id session-id projection)))
        (when (and (< (length matches) collection-limit)
                   (e-session-resources--search-glob-match-p
                    glob-pattern engine-id session-id title projection))
          (setq matches
                (append matches
                        (e-session-resources--search-record
                         resource-uri
                         (e-session-resources--render-projection
                          engine session-id projection)
                         query
                         options
                         (- collection-limit (length matches))))))))
    (list :matches (vconcat (seq-take matches actual-limit))
          :truncated (> (length matches) actual-limit))))

(cl-defun e-session-resources-register-resource-methods
    (registry &key harness &allow-other-keys)
  "Register session:// resource methods in REGISTRY."
  (dolist (method
           (list
            (e-resource-method-create
             :scheme "session"
             :operation e-operation-read
             :description (concat
                           "Read persisted agent session projections. "
                           "Use glob discovery first: glob session://, then "
                           "session://<engine-id>/sessions/, then one "
                           "session root, then read a returned leaf URI. "
                           "Directory roots are glob-only and this scheme is read-only.")
             :uri-patterns '("session://<engine-id>/sessions/<session-id>/<projection>")
             :range-modes '("line")
             :handler (lambda (uri range)
                        (e-session-resources--read harness uri range)))
            (e-resource-method-create
             :scheme "session"
             :operation e-operation-glob
             :description (concat
                           "Discover persisted agent sessions. Workflow: "
                           "glob session:// to discover engines; glob "
                           "session://<engine-id>/sessions/ to discover "
                           "sessions; glob session://<engine-id>/sessions/"
                           "<session-id>/ to discover readable projections.")
             :uri-patterns '("session://"
                             "session://<engine-id>/sessions/"
                             "session://<engine-id>/sessions/<session-id>/")
             :handler (lambda (uri pattern limit case-sensitive sort-by sort-order
                               created-after created-before updated-after updated-before)
                        (e-session-resources--glob
                         harness uri pattern limit case-sensitive sort-by sort-order
                         created-after created-before updated-after updated-before)))
            (e-resource-method-create
             :scheme "session"
             :operation e-operation-search
             :description (concat
                           "Search persisted session text across enabled "
                           "session engines. Searching a session root uses "
                           "the messages projection by default; narrow the "
                           "URI or glob to search activity, events, "
                           "compactions, or provider anchors explicitly.")
             :uri-patterns '("session://"
                             "session://<engine-id>/sessions/"
                             "session://<engine-id>/sessions/<session-id>/"
                             "session://<engine-id>/sessions/<session-id>/<projection>")
             :handler (lambda (uri query options)
                        (e-session-resources--search
                         harness uri query options)))))
    (e-resources-register registry method)))

(defun e-session-resources-capability-create ()
  "Create the session:// resource capability."
  (e-capability-create
   :id 'session-resources
   :name "Session Resources"
   :resource-methods
   (list (e-capability-resource-method-provider-create
          :handler #'e-session-resources-register-resource-methods))))

(provide 'e-session-resources)

;;; e-session-resources.el ends here
