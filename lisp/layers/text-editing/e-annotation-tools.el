;;; e-annotation-tools.el --- Simply Annotate review-channel tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Generic annotation tools over the Simply Annotate package.  These expose the
;; review-channel primitives an agent needs to post non-destructive proposals as
;; inline annotation threads and to resolve them:
;;
;;   - annotation_add     create a thread on a file region with a proposal
;;                        payload, returning a thread id.
;;   - annotation_list    enumerate threads for a file (optionally filtered by an
;;                        opaque correlation key such as an org-id) with their
;;                        payload and verdict.
;;   - annotation_resolve set a thread's verdict (accepted | rejected), optionally
;;                        appending a reply, and return its payload so the caller
;;                        can execute any domain-specific acceptance mutation.
;;
;; The capability is deliberately generic.  Simply Annotate anchors a thread to a
;; buffer region (start/end + text-hash/context); it has no concept of an org-id
;; or any external key.  Callers that need to correlate a thread with a domain
;; entity pass that key inside the proposal :payload (for example an org-id);
;; e stores it verbatim on the thread alist, where it round-trips through the
;; package's prin1/read persistence untouched.  Resolving a thread records the
;; verdict and returns the payload; it does NOT mutate domain state.  Executing
;; an accepted proposal is the caller's job (e.g. grimoire's agenda_apply), which
;; keeps domain-specific write logic out of this generic layer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-tools)
(require 'simply-annotate)

(defcustom e-annotation-tools-author "Agent"
  "Default author name recorded on annotation threads created by e."
  :type 'string
  :group 'e)

(defconst e-annotation-tools-verdicts '("accepted" "rejected")
  "Verdict values accepted by `annotation_resolve'.")

(defvar e-annotation-tools-resolve-functions nil
  "Abnormal hook run after an annotation thread verdict is persisted.
Each function is called with one plist argument describing the resolution:

  (:file FILE :thread-id ID :verdict VERDICT :payload PAYLOAD)

This is the extension point that lets a domain layer execute an accepted
proposal automatically -- for example grimoire subscribes here to route the
payload through its `agenda_apply' write primitive when the payload carries an
org-id.  The annotation layer performs no domain mutation itself; verdict
persistence has already happened before these functions run, so a handler must
be idempotent on the domain side (a re-resolve will call it again).  Each
handler's non-nil return value is collected and returned to the tool caller
under :effects; a handler that signals is captured as an effect entry rather
than aborting the already-persisted resolution.")

;; --- payload normalization --------------------------------------------------

(defun e-annotation-tools--key-symbol (key)
  "Return the bare symbol for plist/alist KEY (keyword or symbol)."
  (cond
   ((keywordp key) (intern (substring (symbol-name key) 1)))
   ((symbolp key) key)
   ((stringp key) (intern key))
   (t (intern (format "%s" key)))))

(defun e-annotation-tools--to-alist (value)
  "Return VALUE as an alist with bare symbol keys.
VALUE may be a keyword plist (as decoded from tool JSON) or an alist.
Returns nil for nil or a non-list VALUE."
  (cond
   ((null value) nil)
   ((and (listp value) (cl-evenp (length value))
         (cl-loop for (k _v) on value by #'cddr always (keywordp k)))
    (cl-loop for (k v) on value by #'cddr
             collect (cons (e-annotation-tools--key-symbol k) v)))
   ((and (listp value) (cl-every #'consp value))
    (mapcar (lambda (entry)
              (cons (e-annotation-tools--key-symbol (car entry)) (cdr entry)))
            value))
   (t nil)))

(defun e-annotation-tools--payload-value (payload key)
  "Return KEY from PAYLOAD alist, tolerating dash/underscore spelling."
  (or (alist-get key payload)
      (alist-get (intern (string-replace "-" "_" (symbol-name key))) payload)))

;; --- file-key and database access (headless) --------------------------------

(defun e-annotation-tools--with-file-context (file body)
  "Call BODY in a buffer context bound to FILE for Simply Annotate db access.
FILE's directory becomes `default-directory' so project-relative file keys and
the database path resolve exactly as they would for a live buffer.  When FILE
exists its contents are loaded so region text, hashes, and context can be
computed.  BODY receives no arguments and runs with point at `point-min'."
  (let ((path (expand-file-name file)))
    (with-temp-buffer
      (setq buffer-file-name path)
      (setq default-directory (file-name-directory path))
      (when (file-exists-p path)
        (insert-file-contents path))
      (goto-char (point-min))
      (unwind-protect
          (funcall body)
        ;; Avoid leaving a phantom visited-file association on the temp buffer.
        (setq buffer-file-name nil)))))

(defun e-annotation-tools--file-key (file)
  "Return the Simply Annotate database key for FILE."
  (e-annotation-tools--with-file-context
   file (lambda () (simply-annotate--file-key))))

(defun e-annotation-tools--entries (file-key)
  "Return the serialized annotation entries stored under FILE-KEY."
  (alist-get file-key (simply-annotate--load-database) nil nil #'string=))

;; --- thread helpers ---------------------------------------------------------

(defun e-annotation-tools--thread-p (text)
  "Return non-nil when annotation TEXT is a thread alist."
  (and (listp text)
       (assq 'id text)
       (assq 'comments text)))

(defun e-annotation-tools--root-text (thread)
  "Return the root comment text of THREAD, or nil."
  (when-let ((comments (alist-get 'comments thread)))
    (alist-get 'text (car comments))))

(defun e-annotation-tools--thread-summary (entry)
  "Return a result plist describing annotation ENTRY, or nil for bare notes."
  (let ((text (alist-get 'text entry)))
    (when (e-annotation-tools--thread-p text)
      (list :thread-id (alist-get 'id text)
            :start (alist-get 'start entry)
            :end (alist-get 'end entry)
            :status (alist-get 'status text)
            :verdict (alist-get 'verdict text)
            :proposal (e-annotation-tools--root-text text)
            :payload (alist-get 'payload text)))))

;; --- primitives -------------------------------------------------------------

(cl-defun e-annotation-tools-add (&key file start end text payload author)
  "Add an annotation thread to FILE over the region START..END.
TEXT is the human-readable proposal recorded as the thread's root comment.
PAYLOAD is an optional plist/alist of machine fields (e.g. org-id, kind, apply)
stored verbatim on the thread for later correlation and execution.  AUTHOR
defaults to `e-annotation-tools-author'.  Returns a result plist with the new
thread id, file key, and region."
  (unless (and file (stringp file)) (user-error "Provide :file"))
  (unless (file-exists-p (expand-file-name file))
    (user-error "Cannot annotate missing file: %s" file))
  (unless (and (integerp start) (integerp end) (< start end))
    (user-error "Provide integer :start < :end"))
  (unless (and text (stringp text) (not (string-empty-p text)))
    (user-error "Provide non-empty :text"))
  (e-annotation-tools--with-file-context
   file
   (lambda ()
     (let* ((max (point-max))
            (region-start (max (point-min) (min start max)))
            (region-end (max region-start (min end max)))
            (region-text (buffer-substring-no-properties region-start region-end))
            (hash (sxhash-equal region-text))
            (context (simply-annotate--make-context region-start region-end))
            (thread (simply-annotate--create-thread
                     text (or author e-annotation-tools-author)))
            (payload-alist (e-annotation-tools--to-alist payload))
            (file-key (simply-annotate--file-key)))
       (when payload-alist
         (setf (alist-get 'payload thread) payload-alist))
       (setf (alist-get 'verdict thread) nil)
       (let* ((entry `((start . ,region-start)
                       (end . ,region-end)
                       (text . ,thread)
                       (text-hash . ,hash)
                       ,@(when (and context (not (string-empty-p context)))
                           `((text-context . ,context)))))
              (entries (append (e-annotation-tools--entries file-key)
                               (list entry))))
         (simply-annotate--update-database file-key entries)
         (list :thread-id (alist-get 'id thread)
               :file-key file-key
               :start region-start
               :end region-end))))))

(cl-defun e-annotation-tools-list (&key file org-id)
  "Return annotation threads on FILE as result plists.
When ORG-ID is non-nil, return only threads whose payload carries that org-id."
  (unless (and file (stringp file)) (user-error "Provide :file"))
  (e-annotation-tools--with-file-context
   file
   (lambda ()
     (let* ((file-key (simply-annotate--file-key))
            (summaries (delq nil (mapcar #'e-annotation-tools--thread-summary
                                         (e-annotation-tools--entries file-key)))))
       (when org-id
         (setq summaries
               (cl-remove-if-not
                (lambda (summary)
                  (equal org-id (e-annotation-tools--payload-value
                                 (plist-get summary :payload) 'org-id)))
                summaries)))
       (list :file-key file-key
             :count (length summaries)
             :threads summaries)))))

(cl-defun e-annotation-tools-resolve (&key file thread-id verdict comment author)
  "Set VERDICT on THREAD-ID in FILE and return its payload.
VERDICT must be one of `e-annotation-tools-verdicts'.  COMMENT, when supplied,
is appended as a reply by AUTHOR (default `e-annotation-tools-author').  The
verdict and reply are persisted; this does NOT execute any domain mutation --
the returned :payload (e.g. an :apply description) lets the caller do that."
  (unless (and file (stringp file)) (user-error "Provide :file"))
  (unless (and thread-id (stringp thread-id)) (user-error "Provide :thread-id"))
  (unless (member verdict e-annotation-tools-verdicts)
    (user-error "Verdict must be one of %s" e-annotation-tools-verdicts))
  (e-annotation-tools--with-file-context
   file
   (lambda ()
     (let* ((file-key (simply-annotate--file-key))
            (entries (e-annotation-tools--entries file-key))
            (found nil)
            (payload nil))
       (unless entries
         (user-error "No annotations for file: %s" file))
       (dolist (entry entries)
         (let ((text (alist-get 'text entry)))
           (when (and (e-annotation-tools--thread-p text)
                      (equal (alist-get 'id text) thread-id))
             (setf (alist-get 'verdict text) verdict)
             (when (and comment (stringp comment) (not (string-empty-p comment)))
               (simply-annotate--add-reply
                text comment (or author e-annotation-tools-author)))
             (setf (alist-get 'text entry) text)
             (setq payload (alist-get 'payload text))
             (setq found t))))
       (unless found
         (user-error "No annotation thread with id: %s" thread-id))
       (simply-annotate--update-database file-key entries)
       (let ((result (list :thread-id thread-id
                           :file-key file-key
                           :verdict verdict
                           :payload payload))
             (effects (e-annotation-tools--run-resolve-hook
                       file thread-id verdict payload)))
         (if effects (append result (list :effects effects)) result))))))

(defun e-annotation-tools--run-resolve-hook (file thread-id verdict payload)
  "Run `e-annotation-tools-resolve-functions' for a persisted resolution.
Returns the list of non-nil handler results (domain effects).  A handler that
signals is captured as an (:error MESSAGE) effect rather than propagating, so a
domain-side failure never rolls back the already-persisted verdict."
  (let ((event (list :file file :thread-id thread-id
                     :verdict verdict :payload payload))
        (effects nil))
    (dolist (fn e-annotation-tools-resolve-functions)
      (condition-case err
          (let ((effect (funcall fn event)))
            (when effect (push effect effects)))
        (error (push (list :error (error-message-string err)) effects))))
    (nreverse effects)))

;; --- tool registration ------------------------------------------------------

(defun e-annotation-tools--register (registry &rest _context)
  "Register the annotation review-channel tools into REGISTRY."
  (e-tools-register
   registry
   :name "annotation_list"
   :description "List Simply Annotate threads on a file. Optionally filter by an org-id stored in a thread's payload. Returns each thread's id, region, status, verdict, root proposal text, and payload."
   :parameters '(:type "object"
                 :properties (:file (:type "string"
                                     :description "Path to the annotated file.")
                              :org_id (:type "string"
                                       :description "Optional payload org-id to filter threads by."))
                 :required ["file"])
   :handler (lambda (arguments)
              (e-annotation-tools-list
               :file (plist-get arguments :file)
               :org-id (or (plist-get arguments :org_id)
                           (plist-get arguments :org-id)))))
  (e-tools-register
   registry
   :name "annotation_add"
   :description "Post a non-destructive proposal as a Simply Annotate thread anchored to a file region (start/end character positions). The proposal text is the human-readable note; payload carries machine fields (e.g. org-id, kind, apply) stored verbatim for later correlation and execution. Returns the new thread id."
   :parameters '(:type "object"
                 :properties (:file (:type "string"
                                     :description "Path to the file to annotate.")
                              :start (:type "integer"
                                      :description "Region start (1-based character position).")
                              :end (:type "integer"
                                    :description "Region end (character position, exclusive).")
                              :text (:type "string"
                                     :description "Human-readable proposal recorded as the thread's root comment.")
                              :author (:type "string"
                                       :description "Optional author name; defaults to the configured agent name.")
                              :payload (:type "object"
                                        :description "Optional machine fields stored on the thread (e.g. org-id, kind, apply)."
                                        :properties (:org_id (:type "string")
                                                     :kind (:type "string")
                                                     :apply (:type "string"))))
                 :required ["file" "start" "end" "text"])
   :handler (lambda (arguments)
              (e-annotation-tools-add
               :file (plist-get arguments :file)
               :start (plist-get arguments :start)
               :end (plist-get arguments :end)
               :text (plist-get arguments :text)
               :author (plist-get arguments :author)
               :payload (or (plist-get arguments :payload)
                            (plist-get arguments :metadata)))))
  (e-tools-register
   registry
   :name "annotation_resolve"
   :description "Set a verdict (accepted | rejected) on a Simply Annotate thread by its id, optionally appending a reply comment. Persists the verdict and returns the thread's payload (e.g. an apply description) so the caller can execute any domain-specific acceptance mutation. Does not itself mutate domain state."
   :parameters '(:type "object"
                 :properties (:file (:type "string"
                                     :description "Path to the annotated file.")
                              :thread_id (:type "string"
                                          :description "Thread id to resolve.")
                              :verdict (:type "string"
                                        :description "accepted or rejected."
                                        :enum ["accepted" "rejected"])
                              :comment (:type "string"
                                        :description "Optional reply recorded on the thread.")
                              :author (:type "string"
                                       :description "Optional reply author; defaults to the configured agent name."))
                 :required ["file" "thread_id" "verdict"])
   :handler (lambda (arguments)
              (e-annotation-tools-resolve
               :file (plist-get arguments :file)
               :thread-id (or (plist-get arguments :thread_id)
                              (plist-get arguments :thread-id))
               :verdict (plist-get arguments :verdict)
               :comment (plist-get arguments :comment)
               :author (plist-get arguments :author)))))

(defun e-annotation-tools-capability-create ()
  "Create the annotation review-channel tool capability."
  (e-capability-create
   :id 'annotation-tools
   :name "Annotation Tools"
   :instruction-priority 230
   :instructions "Use annotation_add to post a non-destructive review proposal as an inline Simply Annotate thread anchored to a file region; carry any domain key (such as an org-id) and a machine-applicable description in the payload. Use annotation_list to read open proposals and their verdicts. Use annotation_resolve to record an accepted or rejected verdict; it returns the payload so a domain-specific tool can execute an accepted change. Resolving never mutates domain state itself."
   :tools (list #'e-annotation-tools--register)))

(provide 'e-annotation-tools)

;;; e-annotation-tools.el ends here
