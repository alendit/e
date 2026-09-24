;;; e-background-session.el --- File/schedule-triggered background sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A background trigger publishes a fixed work request to a durable SQLite
;; Board after a filesystem change or light periodic schedule.
;;
;; The trigger only submits a prompt.  It performs no domain mutation and never
;; accepts anything on the user's behalf; whatever the prompt drives (for
;; example grimoire's idempotent `triage_promote', which only emits proposals)
;; owns that policy.  Two guards keep it well-behaved:
;;
;;   - Debounce: rapid filesystem events coalesce into a single fire after a
;;     quiet window, so a burst of edits does not spawn a burst of turns.
;;   - Busy-skip: a fire is dropped when the session already has a running turn,
;;     so the background loop never queues turns on top of itself.
;;
;; The module depends on no UI shell and can run headless.  Callers supply one
;; detached publication target containing only a SQLite service and Board id.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'filenotify)
(require 'e-board-sqlite-service)
(require 'e-session-identity)

(defgroup e-background-session nil
  "File/schedule-triggered background agent sessions."
  :group 'e
  :prefix "e-background-session-")

(defcustom e-background-session-default-debounce-seconds 2.0
  "Default quiet window, in seconds, before a coalesced fire runs.
Filesystem events arriving within this window of each other collapse into a
single fire.  A non-positive value fires on the next event loop tick."
  :type 'number
  :group 'e-background-session)

(defvar e-background-session-fire-functions nil
  "Abnormal hook run after a background session fire submits a turn.
Each function is called with the trigger that fired.  Intended for observation
(logging, tests); handlers must not assume the turn has finished.")

(cl-defstruct (e-background-trigger (:constructor e-background-trigger--create))
  "A bound background-session trigger.
Configuration fields are set at creation; runtime fields track live watches and
timers and are managed by `e-background-session-start' / `-stop'."
  id
  publication-target
  prompt
  paths
  schedule-seconds
  debounce-seconds
  metadata
  ;; runtime
  watches
  schedule-timer
  debounce-timer
  enabled)

(defvar e-background-session--triggers (make-hash-table :test 'equal)
  "Registered background triggers keyed by id.")

;; --- registry ---------------------------------------------------------------

(defun e-background-session-get (id)
  "Return the registered background trigger with ID, or nil."
  (gethash id e-background-session--triggers))

(defun e-background-session-list ()
  "Return all registered background triggers."
  (hash-table-values e-background-session--triggers))

(cl-defun e-background-session-register
    (&key id publication-target prompt paths schedule-seconds
          debounce-seconds metadata)
  "Create and register a background trigger, returning it.

ID is a unique key (an existing trigger with the same ID is stopped and
replaced).  PUBLICATION-TARGET explicitly names the SQLite Board receiving
each fire.  PROMPT is the fixed fact text published on each fire.  PATHS is a
list of files and/or
directories to watch for changes.  SCHEDULE-SECONDS, when non-nil, also fires on
that periodic interval.  DEBOUNCE-SECONDS overrides
`e-background-session-default-debounce-seconds'.  METADATA is published as
bounded descriptive fact attributes."
  (unless id (user-error "Background trigger requires :id"))
  (unless (e-board-sqlite-publication-target-valid-p publication-target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p publication-target)))
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Background trigger requires a non-empty :prompt string"))
  (when-let* ((existing (e-background-session-get id)))
    (e-background-session-stop existing))
  (let ((trigger (e-background-trigger--create
                  :id id
                  :publication-target publication-target
                  :prompt prompt
                  :paths paths
                  :schedule-seconds schedule-seconds
                  :debounce-seconds (or debounce-seconds
                                        e-background-session-default-debounce-seconds)
                  :metadata metadata
                  :enabled nil)))
    (puthash id trigger e-background-session--triggers)
    trigger))

(defun e-background-session-remove (id)
  "Stop and unregister the background trigger with ID."
  (when-let* ((trigger (e-background-session-get id)))
    (e-background-session-stop trigger)
    (remhash id e-background-session--triggers)))

;; --- lifecycle --------------------------------------------------------------

(defun e-background-session--arm-path (trigger path)
  "Add a change watch on PATH for TRIGGER, returning the watch or nil."
  (when (file-exists-p path)
    (file-notify-add-watch
     path '(change)
     (lambda (event)
       (e-background-session--on-fs-event trigger path event)))))

(defun e-background-session-start (trigger)
  "Begin watching paths and scheduling for TRIGGER, then return it."
  (unless (e-board-sqlite-publication-target-valid-p
           (e-background-trigger-publication-target trigger))
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p
                  (e-background-trigger-publication-target trigger))))
  (e-background-session-stop trigger)
  ;; Enable before arming so a watch callback that fires during arming sees a
  ;; live trigger rather than being mistaken for our own teardown.
  (setf (e-background-trigger-enabled trigger) t)
  (setf (e-background-trigger-watches trigger)
        (delq nil
              (mapcar (lambda (path)
                        (e-background-session--arm-path trigger path))
                      (e-background-trigger-paths trigger))))
  (when-let* ((interval (e-background-trigger-schedule-seconds trigger)))
    (setf (e-background-trigger-schedule-timer trigger)
          (run-at-time interval interval
                       (lambda ()
                         (e-background-session--request-fire trigger)))))
  trigger)

(defun e-background-session-stop (trigger)
  "Cancel TRIGGER's watches and timers, returning it."
  ;; Clear enabled first: rm-watch delivers `stopped' events asynchronously, and
  ;; the handler must distinguish our own teardown (ignore) from an OS-driven
  ;; watch invalidation on a live trigger (re-arm + fire).
  (setf (e-background-trigger-enabled trigger) nil)
  (dolist (watch (e-background-trigger-watches trigger))
    (ignore-errors (file-notify-rm-watch watch)))
  (setf (e-background-trigger-watches trigger) nil)
  (when-let* ((timer (e-background-trigger-schedule-timer trigger)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (e-background-trigger-schedule-timer trigger) nil)
  (when-let* ((timer (e-background-trigger-debounce-timer trigger)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (e-background-trigger-debounce-timer trigger) nil)
  trigger)

;; --- firing -----------------------------------------------------------------

(defun e-background-session--on-fs-event (trigger path event)
  "Request a debounced fire of TRIGGER for filesystem EVENT on PATH.
A `stopped' event needs care: editors (and Emacs `save-buffer') commit files by
atomic rename, which invalidates a per-file watch on backends like kqueue.  When
that happens on a still-enabled trigger the content really did change, so re-arm
the path and fire; a `stopped' arriving after we have disabled the trigger is
our own teardown and is ignored."
  (let ((action (and (listp event) (nth 1 event))))
    (cond
     ((eq action 'stopped)
      (when (e-background-trigger-enabled trigger)
        (setf (e-background-trigger-watches trigger)
              (cons (e-background-session--arm-path trigger path)
                    (delq nil (e-background-trigger-watches trigger))))
        (e-background-session--request-fire trigger)))
     (t
      (e-background-session--request-fire trigger)))))

(defun e-background-session--request-fire (trigger)
  "Schedule a coalesced fire of TRIGGER after its debounce window.
Restarting the debounce timer on each request is what coalesces a burst of
events into one fire."
  (when-let* ((timer (e-background-trigger-debounce-timer trigger)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (e-background-trigger-debounce-timer trigger)
        (run-at-time (max 0 (e-background-trigger-debounce-seconds trigger)) nil
                     (lambda ()
                       (setf (e-background-trigger-debounce-timer trigger) nil)
                       (e-background-session-fire trigger)))))

(defun e-background-session--resolve-prompt (trigger)
  "Return TRIGGER's fixed work request."
  (e-background-trigger-prompt trigger))

(defun e-background-session-fire (trigger)
  "Publish TRIGGER's Board work and return its asynchronous work handle."
  (let ((item
         (e-board-sqlite-publication-target-append-route-start
          (e-background-trigger-publication-target trigger)
          (e-background-session--resolve-prompt trigger)
          (list 'background-trigger (e-background-trigger-id trigger)
                (e-session-identity-generate-ulid))
          :tags (list 'background 'trigger (e-background-trigger-id trigger))
          :attributes
          (append (list :trigger-id (e-background-trigger-id trigger)
                        :paths (copy-tree (e-background-trigger-paths trigger)))
                  (copy-tree (e-background-trigger-metadata trigger))))))
    (run-hook-with-args 'e-background-session-fire-functions trigger)
    item))

(provide 'e-background-session)

;;; e-background-session.el ends here
