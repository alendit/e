;;; e-annotation-org.el --- org-annotate backend for annotation actions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Annotation actions over the org-annotate package.  org-annotate keeps
;; annotation anchors (<<oa:ID>>) and threaded replies directly in the Org file
;; being annotated; the Org file is the source of truth and there is no sidecar
;; database.  These actions are thin wrappers over the org-annotate public API
;; run headless in a file-visiting buffer, plus the one predicate the answer
;; loop needs.
;;
;; The backend is deliberately not backend-neutral: there is one backend
;; (org-annotate) and every action is guarded to Org files.  A non-Org target
;; is a loud error, never a silent no-op.
;;
;; The `actionable' predicate -- open state AND a non-agent last author -- is
;; the loop keystone: once the agent has replied, the thread's last author is
;; the agent and it drops out of the worklist, so a sweep may run indefinitely
;; without ever double-answering.  The loop's own output removes work from its
;; input.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function org-annotate-entries "org-annotate")
(declare-function org-annotate-get "org-annotate")
(declare-function org-annotate-reply "org-annotate")
(declare-function org-annotate-create "org-annotate")
(declare-function org-annotate-set-state "org-annotate")
(declare-function org-annotate-list-ids "org-annotate")
(declare-function org-annotate-refresh "org-annotate")
(defvar org-annotate-agent-author)
(defvar org-annotate-mode)

(defconst e-annotation-org--backend-functions
  '(org-annotate-entries
    org-annotate-get
    org-annotate-reply
    org-annotate-create
    org-annotate-set-state)
  "org-annotate functions required by the annotation actions.")

(defvar e-annotation-tools-resolve-functions nil
  "Abnormal hook run after an annotation state change is persisted.
Each function is called with one plist argument describing the resolution:

  (:file FILE :annotation-id ID :state STATE :payload PAYLOAD)

This is the extension point that lets a domain layer react to a resolved
annotation -- for example grimoire subscribes here to route an accepted
proposal through its `agenda-apply' write primitive.  The annotation layer
performs no domain mutation itself; state persistence has already happened
before these functions run, so a handler must be idempotent on the domain side.
Each handler's non-nil return value is collected and returned to the caller
under :effects; a handler that signals is captured as an effect entry rather
than aborting the already-persisted state change.

The event carries `:verdict' and `:thread-id' as aliases of `:state' and
`:annotation-id' so subscribers written against the previous review-channel
event shape keep working.")

;; --- availability -----------------------------------------------------------

(defun e-annotation-org--missing-backend-functions ()
  "Return org-annotate backend functions that are not currently defined."
  (cl-remove-if #'fboundp e-annotation-org--backend-functions))

(defun e-annotation-org-available-p ()
  "Return non-nil when a compatible org-annotate backend is available."
  (and (condition-case nil
           (require 'org-annotate nil t)
         (error nil))
       (null (e-annotation-org--missing-backend-functions))))

(defun e-annotation-org--require-backend ()
  "Require an org-annotate backend or signal a user error."
  (unless (require 'org-annotate nil t)
    (user-error "Install org-annotate to use annotation actions"))
  (when-let ((missing (e-annotation-org--missing-backend-functions)))
    (user-error "Update org-annotate to use annotation actions: missing %S"
                missing)))

;; --- Org-file guard and headless access -------------------------------------

(defun e-annotation-org--org-file-p (file)
  "Return non-nil when FILE names an Org file by extension."
  (and (stringp file)
       (string-match-p "\\.org\\(\\.gpg\\)?\\'" file)))

(defun e-annotation-org--require-org-file (file)
  "Signal unless FILE is a non-empty Org file path."
  (unless (and file (stringp file))
    (user-error "Provide :file"))
  (unless (e-annotation-org--org-file-p file)
    (user-error "Annotation actions require an Org file, got: %s" file)))

(defun e-annotation-org--live-buffer (path)
  "Return the live buffer visiting PATH, or nil."
  (find-buffer-visiting path))

(defun e-annotation-org--read (file body)
  "Call BODY with FILE readable in an Org buffer and return its value.
Read-only: BODY must not mutate.  When a live buffer visits FILE it is used
directly so BODY observes unsaved edits; otherwise FILE is loaded into a temp
Org buffer.  BODY receives no arguments and runs with point at `point-min'."
  (e-annotation-org--require-org-file file)
  (e-annotation-org--require-backend)
  (let* ((path (expand-file-name file))
         (live (e-annotation-org--live-buffer path)))
    (if live
        (with-current-buffer live
          (save-excursion (goto-char (point-min)) (funcall body)))
      (with-temp-buffer
        (setq buffer-file-name path)
        (setq default-directory (file-name-directory path))
        (when (file-exists-p path)
          (insert-file-contents path))
        (delay-mode-hooks (org-mode))
        (goto-char (point-min))
        (unwind-protect
            (funcall body)
          (setq buffer-file-name nil))))))

(define-error 'e-annotation-org-buffer-dirty
  "Cannot write annotation: the file has unsaved changes in a live buffer")

(defun e-annotation-org--write (file body)
  "Call BODY to mutate FILE's annotations and persist the change.
When a live buffer visits FILE and is unmodified, BODY runs in it and the
buffer is saved, so overlays and visited-modtime stay coherent.  When that
buffer is modified, signal `e-annotation-org-buffer-dirty' so a caller can
defer -- a background writer never clobbers unsaved edits.  With no live
buffer, FILE is loaded into a temp Org buffer, BODY runs, and the result is
written back.  BODY receives no arguments and returns the action result."
  (e-annotation-org--require-org-file file)
  (e-annotation-org--require-backend)
  (unless (file-exists-p (expand-file-name file))
    (user-error "Cannot annotate missing file: %s" file))
  (let* ((path (expand-file-name file))
         (live (e-annotation-org--live-buffer path)))
    (cond
     ((and live (buffer-modified-p live))
      (signal 'e-annotation-org-buffer-dirty (list path)))
     (live
      (with-current-buffer live
        (prog1 (save-excursion (goto-char (point-min)) (funcall body))
          (save-buffer)
          (when (and (boundp 'org-annotate-mode) org-annotate-mode)
            (ignore-errors (org-annotate-refresh))))))
     (t
      (with-temp-buffer
        (setq buffer-file-name path)
        (setq default-directory (file-name-directory path))
        (insert-file-contents path)
        (delay-mode-hooks (org-mode))
        (goto-char (point-min))
        (unwind-protect
            (prog1 (funcall body)
              (write-region (point-min) (point-max) path nil 'silent))
          (setq buffer-file-name nil)))))))

;; --- normalization ----------------------------------------------------------

(defun e-annotation-org--agent-author ()
  "Return the org-annotate agent author string."
  (or (and (boundp 'org-annotate-agent-author) org-annotate-agent-author)
      "agent"))

(defun e-annotation-org--last-author (entry)
  "Return the author of ENTRY's last thread message, or nil."
  (let ((messages (plist-get entry :messages)))
    (plist-get (car (last messages)) :author)))

(defun e-annotation-org--actionable-p (entry)
  "Return non-nil when annotation ENTRY awaits an agent answer.
An entry is actionable when its state is \"open\" and its last message was not
authored by the agent.  This makes every answer tier idempotent: an
agent-last-authored thread is not actionable, so a repeated sweep never
double-answers."
  (and (equal (plist-get entry :state) "open")
       (not (equal (e-annotation-org--last-author entry)
                   (e-annotation-org--agent-author)))))

(defun e-annotation-org--summary (entry)
  "Return a result plist describing annotation ENTRY."
  (list :id (plist-get entry :id)
        :state (plist-get entry :state)
        :author (plist-get entry :author)
        :created (plist-get entry :created)
        :range-text (plist-get entry :range-text)
        :actionable (e-annotation-org--actionable-p entry)
        :messages (mapcar (lambda (message)
                            (list :author (plist-get message :author)
                                  :created (plist-get message :created)
                                  :body (plist-get message :body)))
                          (plist-get entry :messages))))

;; --- primitives -------------------------------------------------------------

(cl-defun e-annotation-org-list (&key file actionable-only)
  "Return annotation threads on FILE as result plists.
When ACTIONABLE-ONLY is non-nil, return only threads matching
`e-annotation-org--actionable-p'."
  (e-annotation-org--read
   file
   (lambda ()
     (let ((summaries (mapcar #'e-annotation-org--summary (org-annotate-entries))))
       (when actionable-only
         (setq summaries (cl-remove-if-not
                          (lambda (s) (plist-get s :actionable))
                          summaries)))
       (list :file file
             :count (length summaries)
             :threads summaries)))))

(cl-defun e-annotation-org-reply (&key file id body author)
  "Append a reply BODY to annotation ID in FILE and return a result plist.
AUTHOR defaults to the org-annotate agent author.  Append-only."
  (unless (and id (stringp id)) (user-error "Provide :id"))
  (unless (and body (stringp body) (not (string-empty-p body)))
    (user-error "Provide non-empty :body"))
  (e-annotation-org--write
   file
   (lambda ()
     (org-annotate-reply id body (or author (e-annotation-org--agent-author)))
     (list :file file :id id :author (or author (e-annotation-org--agent-author))))))

(cl-defun e-annotation-org-add (&key file start end body author)
  "Create an annotation thread in FILE over the region START..END.
BODY is the thread's first message.  AUTHOR defaults to the org-annotate agent
author.  START and END are optional; when both are nil the annotation anchors at
`point-min' with no covered range.  Returns a result plist with the new id."
  (unless (and body (stringp body) (not (string-empty-p body)))
    (user-error "Provide non-empty :body"))
  (when (or start end)
    (unless (and (integerp start) (integerp end) (< start end))
      (user-error "Provide integer :start < :end")))
  (e-annotation-org--write
   file
   (lambda ()
     (let* ((max (point-max))
            (beg (and start (max (point-min) (min start max))))
            (fin (and end (max (or beg (point-min)) (min end max))))
            (id (org-annotate-create beg fin body
                                     (or author (e-annotation-org--agent-author)))))
       (list :file file :id id :start beg :end fin)))))

(cl-defun e-annotation-org-resolve (&key file id state reply author)
  "Set STATE on annotation ID in FILE and run the resolve hook.
STATE defaults to \"resolved\".  REPLY, when supplied, is appended by AUTHOR
before the state change.  Persisting the state does not itself mutate any domain
state; the resolve hook is the extension point for that."
  (unless (and id (stringp id)) (user-error "Provide :id"))
  (let ((state (or state "resolved")))
    (e-annotation-org--write
     file
     (lambda ()
       (when (and reply (stringp reply) (not (string-empty-p reply)))
         (org-annotate-reply id reply (or author (e-annotation-org--agent-author))))
       (org-annotate-set-state id state)
       (let ((result (list :file file :id id :state state))
             (effects (e-annotation-org--run-resolve-hook file id state)))
         (if effects (append result (list :effects effects)) result))))))

(defun e-annotation-org--run-resolve-hook (file id state)
  "Run `e-annotation-tools-resolve-functions' for a persisted resolution.
Returns the list of non-nil handler results.  A handler that signals is
captured as an (:error MESSAGE) effect rather than propagating, so a domain-side
failure never rolls back the already-persisted state change."
  (let ((event (list :file file :annotation-id id :state state
                     ;; Aliases for subscribers written against the previous
                     ;; review-channel event shape.
                     :thread-id id :verdict state :payload nil))
        (effects nil))
    (dolist (fn e-annotation-tools-resolve-functions)
      (condition-case err
          (let ((effect (funcall fn event)))
            (when effect (push effect effects)))
        (error (push (list :error (error-message-string err)) effects))))
    (nreverse effects)))

(provide 'e-annotation-org)

;;; e-annotation-org.el ends here
