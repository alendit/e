;;; e-annotation-answer.el --- Reusable annotation answer operation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; "Answer the open annotation threads on a document" as a first-class,
;; repeatable operation over the org-annotate in-file model.  One core builds a
;; fixed answer prompt from a file's actionable threads and publishes a board
;; fact; two entry points reuse it:
;;
;; - `e-annotations-answer' publishes the current Org buffer's actionable
;;   threads through an explicitly installed SQLite publication target.
;; - `e-annotation-answer-sweep' publishes one fact per actionable file in an
;;   unattended set supplied by policy outside this module.
;;
;; The answerer replies (append-only).  It never rewrites prose under a live
;; buffer; a needed correction is emitted as a proposal a human accepts.  The
;; actionable predicate lives in `e-annotation-org' and makes every tier
;; idempotent, so a repeated dispatch or sweep never double-answers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-annotation-org)
(require 'e-board-sqlite-service)

(defvar e-annotation-answer-publication-target nil
  "Explicit SQLite Board target for annotation facts.")

(defun e-annotation-answer-configure-publication-target (target)
  "Install explicit SQLite publication TARGET for annotation dispatch."
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (setq e-annotation-answer-publication-target target))

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

(defun e-annotation-answer--source-key (file prompt)
  "Return the stable publication key for FILE and fixed PROMPT."
  (list 'annotation-answer (expand-file-name file)
        (secure-hash 'sha256 prompt)))

;; --- Tier 0 dispatch --------------------------------------------------------

(cl-defun e-annotation-answer-dispatch
    (&key file publication-target)
  "Publish actionable annotation threads from FILE as one board work input.
PUBLICATION-TARGET, or the explicitly configured default, names the durable
SQLite Board receiving the work."
  (e-annotation-org--require-org-file file)
  (let ((threads (e-annotation-answer--actionable file)))
    (when threads
      (let ((prompt (e-annotation-answer--prompt file threads)))
        (e-board-sqlite-publication-target-append-route-start
         (or publication-target e-annotation-answer-publication-target)
         prompt (e-annotation-answer--source-key file prompt)
         :tags '(annotation answer)
         :attributes (list :file (expand-file-name file)
                           :thread-count (length threads))
         :reference (expand-file-name file))))))

;;;###autoload
(defun e-annotations-answer ()
  "Answer the open annotation threads on the current Org buffer's file.
Publish the actionable-thread request as board work for an authorized board
participant to handle.  The buffer must visit a saved Org file."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "e-annotations-answer requires an Org buffer"))
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Save the buffer to a file before answering threads"))
    (let ((record (e-annotation-answer-dispatch :file file)))
      (if record
          (message "Dispatched annotation answerer for %s"
                   (file-name-nondirectory file))
        (message "No actionable annotation threads on %s"
                 (file-name-nondirectory file)))
      record)))

;; --- Tier 2 sweep -----------------------------------------------------------

(cl-defun e-annotation-answer-sweep
    (files &key publication-target)
  "Publish board work for each Org file in FILES with actionable threads.
Generic mechanism: the caller supplies which files to sweep (grimoire policy
decides that).  Honors the `e-annotation-answer-sweep-inhibit' kill switch and
defers a file whose live buffer has unsaved edits, so a background write never
clobbers in-progress work; the loop is idempotent and catches it next pass.
Return a plist summarizing the sweep."
  (let ((target (or publication-target e-annotation-answer-publication-target))
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
              (let ((prompt (e-annotation-answer--prompt file threads)))
                (e-board-sqlite-publication-target-append-route-start
                 target prompt (e-annotation-answer--source-key file prompt)
                 :tags '(annotation answer sweep)
                 :attributes (list :file (expand-file-name file)
                                   :thread-count (length threads))
                 :reference (expand-file-name file)))
              (push file dispatched)))))))
    (list :inhibited e-annotation-answer-sweep-inhibit
          :dispatched (nreverse dispatched)
          :deferred (nreverse deferred)
          :skipped (nreverse skipped))))

(provide 'e-annotation-answer)

;;; e-annotation-answer.el ends here
