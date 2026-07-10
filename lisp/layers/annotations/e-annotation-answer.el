;;; e-annotation-answer.el --- Reusable annotation answer operation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; "Answer the open annotation threads on a document" as a first-class,
;; repeatable operation over the org-annotate in-file model.  One core builds a
;; fixed answer prompt from a file's actionable threads and dispatches a
;; background answerer; three tiers reuse it:
;;
;; - Tier 0: `e-annotations-answer', an interactive command that dispatches a
;;   background subagent for the current Org buffer's file with no content
;;   prompt.
;; - Tier 1: the same command with `:with-session-context', which seeds the
;;   subagent with the current session's exported context ("fork-lite").
;; - Tier 2: `e-annotation-answer-sweep', an unattended sweep over a supplied
;;   set of Org files that enqueues one answer task per file with actionable
;;   threads, so a scheduler (e.g. grimoire cron) keeps documents answered.
;;
;; The answerer replies (append-only).  It never rewrites prose under a live
;; buffer; a needed correction is emitted as a proposal a human accepts.  The
;; actionable predicate lives in `e-annotation-org' and makes every tier
;; idempotent, so a repeated dispatch or sweep never double-answers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-annotation-org)

(declare-function e-subagent-spawn "e-subagent-runner")
(declare-function e-subagent-configure-type "e-subagent-runner")
(declare-function e-context-inspection-export-context "e-context-inspection")
(declare-function e-harness-resources "e-harness")
(declare-function e-resources-read "e-resources")
(declare-function e-task-queue-enqueue "e-task-queue")
(defvar e-subagent-actions-default-registry)
(defvar e-task-queue-actions-default-queue)

(defcustom e-annotation-answer-type "tool-user"
  "Spawnable subagent type id that answers annotation threads.
The type is configured once per dispatch to enable
`e-annotation-answer-layers' so the child can list, reply, and propose over
org-annotate."
  :type 'string
  :group 'e)

(defcustom e-annotation-answer-layers '(annotations web)
  "Layer ids enabled on the answerer type before it is spawned.
`annotations' gives the child the org-annotate actions and skill; `web' lets it
research against the document's linked sources.  Enabling an already-enabled
layer is idempotent."
  :type '(repeat symbol)
  :group 'e)

(defcustom e-annotation-answer-sweep-inhibit nil
  "When non-nil, `e-annotation-answer-sweep' dispatches nothing.
This is the sweep kill switch: set it to pause the unattended loop without
unregistering the schedule that drives it."
  :type 'boolean
  :group 'e)

(defcustom e-annotation-answer-sources-subdir "sources"
  "Subdirectory beside a document where its linked sources live.
Included in the answer prompt so the answerer knows where to research."
  :type 'string
  :group 'e)

;; --- prompt construction ----------------------------------------------------

(defun e-annotation-answer--sources-dir (file)
  "Return FILE's sibling sources directory when it exists, else nil."
  (let ((dir (expand-file-name e-annotation-answer-sources-subdir
                               (file-name-directory (expand-file-name file)))))
    (and (file-directory-p dir) dir)))

(defun e-annotation-answer--thread-line (thread)
  "Return one prompt line describing actionable THREAD."
  (let ((messages (plist-get thread :messages)))
    (format "- annotation %s [%s]: %s"
            (plist-get thread :id)
            (or (plist-get thread :range-text) "no range")
            (or (plist-get (car messages) :body) "(no comment)"))))

(defun e-annotation-answer--prompt (file threads)
  "Return the fixed answer prompt for actionable THREADS on FILE.
This is not a user question: it is a fixed instruction telling a cold answerer
what to do with each thread."
  (let ((sources (e-annotation-answer--sources-dir file)))
    (string-join
     (delq
      nil
      (list
       "Answer the open annotation threads on an Org document."
       (format "Document: %s" (expand-file-name file))
       (when sources
         (format "Linked sources directory: %s" sources))
       ""
       "Read the org-annotate skill first: e://annotations/skills/org-annotate."
       (format "Actionable threads (%d):" (length threads))
       (mapconcat #'e-annotation-answer--thread-line threads "\n")
       ""
       "For each actionable thread:"
       (concat
        "- Research the question against the document and its linked sources"
        " (use web only when a source is external and needed).")
       (concat
        "- Post your answer as an append-only reply with"
        " (e-actions-call 'annotations :reply '(:file \"...\" :id \"...\" :body \"...\")).")
       (concat
        "- If the thread implies a prose correction, do NOT edit the document."
        " Emit it as a proposal with (e-actions-call 'annotations :add ...) keyed"
        " to the same region so a human can accept it.")
       (concat
        "- Only set a thread's state with :resolve when its request is genuinely"
        " handled; otherwise leave it open with your reply.")
       (concat
        "Re-list with (e-actions-call 'annotations :list '(:file \"...\""
        " :actionable-only t)) to confirm before finishing."))
      )
     "\n")))

;; --- thread collection ------------------------------------------------------

(defun e-annotation-answer--actionable (file)
  "Return actionable annotation threads on FILE."
  (plist-get (e-annotation-org-list :file file :actionable-only t) :threads))

;; --- Tier 1 seed ------------------------------------------------------------

(defun e-annotation-answer--session-seed (harness session-id turn-id)
  "Return a seed-message list carrying HARNESS SESSION-ID exported context.
Returns nil when no session context is available, so the seedless path is the
default.  The exported context is written to the session's own resources and
read back as one system message."
  (when (and harness session-id
             (fboundp 'e-context-inspection-export-context))
    (ignore-errors
      (let* ((uri "tmp://annotation-answer-seed.md")
             (export (e-context-inspection-export-context
                      :harness harness :session-id session-id :turn-id turn-id
                      :uri uri))
             (content (e-resources-read
                       (e-harness-resources harness session-id turn-id)
                       (plist-get export :uri))))
        (when (and (stringp content) (not (string-empty-p content)))
          (list (list :role 'system
                      :content (concat "Originating session context:\n\n"
                                       content))))))))

;; --- Tier 0 dispatch --------------------------------------------------------

(cl-defun e-annotation-answer-dispatch
    (&key file harness session-id turn-id with-session-context registry)
  "Dispatch a background answerer for actionable threads on FILE.
Collect actionable threads; when none exist return nil without spawning.  Enable
`e-annotation-answer-layers' on `e-annotation-answer-type' once, then spawn a
queued, non-blocking subagent seeded with the fixed answer prompt.  With
WITH-SESSION-CONTEXT, also seed HARNESS SESSION-ID's exported context.  REGISTRY
defaults to the process-wide subagent registry.  Returns the subagent record, or
nil when nothing was actionable."
  (e-annotation-org--require-org-file file)
  (require 'e-subagent-runner)
  (require 'e-subagent-actions)
  (let ((threads (e-annotation-answer--actionable file)))
    (when threads
      (e-subagent-configure-type e-annotation-answer-type
                                 :enable-layers e-annotation-answer-layers)
      (e-subagent-spawn
       (or registry e-subagent-actions-default-registry)
       harness session-id
       :type e-annotation-answer-type
       :prompt (e-annotation-answer--prompt file threads)
       :seed-messages (when with-session-context
                        (e-annotation-answer--session-seed
                         harness session-id turn-id))
       :label (format "answer %s" (file-name-nondirectory file))
       :schedule 'queue))))

;;;###autoload
(defun e-annotations-answer (&optional with-session-context)
  "Answer the open annotation threads on the current Org buffer's file.
Dispatch a background subagent that replies to each actionable thread and
proposes (never applies) any prose correction, with no content prompt.  With a
prefix argument, seed the answerer with this session's exported context.  The
buffer must visit a saved Org file: annotations are keyed to a file."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "e-annotations-answer requires an Org buffer"))
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Save the buffer to a file before answering threads"))
    (let* ((harness (and (boundp 'e-chat-harness) e-chat-harness))
           (session-id (and (boundp 'e-chat-session-id) e-chat-session-id))
           (record (e-annotation-answer-dispatch
                    :file file
                    :harness harness
                    :session-id session-id
                    :with-session-context with-session-context)))
      (if record
          (message "Dispatched annotation answerer for %s"
                   (file-name-nondirectory file))
        (message "No actionable annotation threads on %s"
                 (file-name-nondirectory file)))
      record)))

;; --- Tier 2 sweep -----------------------------------------------------------

(cl-defun e-annotation-answer-sweep (files &key queue harness-instance-id)
  "Enqueue an answer task for each Org file in FILES with actionable threads.
Generic mechanism: the caller supplies which files to sweep (grimoire policy
decides that).  Honors the `e-annotation-answer-sweep-inhibit' kill switch and
defers a file whose live buffer has unsaved edits, so a background write never
clobbers in-progress work; the loop is idempotent and catches it next pass.
Enqueues onto QUEUE (defaults to the task-queue capability's default queue),
which enforces its own concurrency cap.  HARNESS-INSTANCE-ID selects the
answerer harness instance.  Returns a plist summarizing the sweep."
  (require 'e-task-queue)
  (require 'e-task-queue-actions)
  (let ((queue (or queue e-task-queue-actions-default-queue))
        (dispatched nil)
        (deferred nil)
        (skipped nil))
    (unless e-annotation-answer-sweep-inhibit
      (dolist (file files)
        (cond
         ((not (e-annotation-org--org-file-p file))
          (push file skipped))
         ((let ((live (e-annotation-org--live-buffer (expand-file-name file))))
            (and live (buffer-modified-p live)))
          (push file deferred))
         (t
          (let ((threads (ignore-errors (e-annotation-answer--actionable file))))
            (if (null threads)
                (push file skipped)
              (e-task-queue-enqueue
               queue
               :prompt (e-annotation-answer--prompt file threads)
               :summary (format "Answer %d thread(s) in %s"
                                 (length threads)
                                 (file-name-nondirectory file))
               :metadata (list :annotation-answer t :file (expand-file-name file))
               :harness-instance-id harness-instance-id)
              (push file dispatched)))))))
    (list :inhibited e-annotation-answer-sweep-inhibit
          :dispatched (nreverse dispatched)
          :deferred (nreverse deferred)
          :skipped (nreverse skipped))))

(provide 'e-annotation-answer)

;;; e-annotation-answer.el ends here
