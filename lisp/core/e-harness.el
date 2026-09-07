;;; e-harness.el --- Core harness service for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public application-service facade and lifecycle/composition root.  Runtime
;; owners live in the directional e-harness-* modules below this boundary.

;;; Code:

(require 'cl-lib)
(require 'e-harness-state)
(require 'e-harness-capabilities)
(require 'e-harness-activity)
(require 'e-harness-turn-state)
(require 'e-harness-context-runtime)
(require 'e-harness-turn)
(require 'e-backend)
(require 'e-context)
(require 'e-events)
(require 'e-hooks)
(require 'e-session)
(require 'e-telemetry)
(require 'e-work)
(require 'seq)
(require 'subr-x)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(defgroup e-harness nil
  "Core harness service for e."
  :group 'e)

(defvar e-harness--pending-runtime-refreshes
  (make-hash-table :test 'eq :weakness 'key)
  "Newest deferred runtime refresh request for each busy harness.")

(defvar-local e-current-harness nil
  "Harness currently owned by the active presentation buffer, when any.")

(defun e-harness--clear-derived-accessor-metadata ()
  "Clear stale struct accessor metadata for derived harness views."
  (dolist (symbol '(e-harness-active-capabilities
                    e-harness-store
                    e-harness-resources
                    e-harness-tools))
    (put symbol 'compiler-macro nil)
    (put symbol 'side-effect-free nil)
    (put symbol 'gv-expander nil)))

(e-harness--clear-derived-accessor-metadata)

(defun e-harness-set-work-enrollment-function (harness function)
  "Set HARNESS's prepared-work enrollment FUNCTION.
FUNCTION receives a prepared work handle and, for tool work, an optional exact
invocation callback.  Board-specific routing remains in the adapter."
  (unless (or (null function) (functionp function))
    (signal 'wrong-type-argument (list 'functionp function)))
  (setf (e-harness-work-enrollment-function harness) function)
  harness)

(defun e-harness-set-board-aggregation-function (harness function)
  "Set HARNESS's board aggregation subscription FUNCTION."
  (unless (or (null function) (functionp function))
    (signal 'wrong-type-argument (list 'functionp function)))
  (setf (e-harness-board-aggregation-function harness) function)
  harness)

(defun e-harness-refresh-default-context-strategy (harness)
  "Refresh HARNESS default context strategy, preserving custom strategies."
  (when (e-context-transcript-stack-p (e-harness-context-strategy harness))
    (setf (e-harness-context-strategy harness)
          (e-context-transcript-stack-create)))
  harness)

(defun e-harness-refresh-runtime-from (harness fresh)
  "Refresh replaceable runtime configuration on HARNESS from FRESH.
The retained object keeps its sessions and runtime substates; only replaceable
backend/configuration fields are copied from FRESH."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (e-harness-p fresh)
    (signal 'wrong-type-argument (list 'e-harness-p fresh)))
  (setf (e-harness-backend harness) (e-harness-backend fresh))
  (setf (e-harness-default-options harness)
        (copy-sequence (e-harness-default-options fresh)))
  (setf (e-harness-default-project-root harness)
        (e-harness-default-project-root fresh))
  (setf (e-harness-runtime-capability-config harness)
        (copy-tree (e-harness-runtime-capability-config fresh)))
  (e-harness-clear-effective-capability-config-cache harness)
  (when (e-context-transcript-stack-p (e-harness-context-strategy harness))
    (setf (e-harness-context-strategy harness)
          (e-harness-context-strategy fresh)))
  harness)

(defun e-harness--normalize-session-metadata (metadata)
  "Return session METADATA with normalized harness-owned fields."
  (let ((metadata (copy-sequence metadata)))
    (when (plist-member metadata :project-root)
      (if-let ((root (e-harness-normalize-project-root
                      (plist-get metadata :project-root))))
          (setq metadata (plist-put metadata :project-root root))
        (cl-remf metadata :project-root)))
    metadata))

(cl-defun e-harness-create
    (&key backend context-strategy default-options capability-config
          sessions enabled-layer-ids intrinsic-capabilities
          project-root layer-change-function)
  "Create a core harness.
BACKEND, CONTEXT-STRATEGY, DEFAULT-OPTIONS, CAPABILITY-CONFIG, SESSIONS,
ENABLED-LAYER-IDS, INTRINSIC-CAPABILITIES, PROJECT-ROOT, and
LAYER-CHANGE-FUNCTION configure the provider-neutral runtime.

When LAYER-CHANGE-FUNCTION is non-nil, it is called with HARNESS after public
layer selection APIs change the enabled layer set."
  (let ((harness
         (e-harness-state-create :backend backend
                          :context-strategy (or context-strategy
                                                (e-context-transcript-stack-create))
                          :default-options default-options
                          :default-project-root
                          (e-harness-normalize-project-root project-root)
                          :runtime-capability-config
                          (copy-tree capability-config)
                          :sessions (or sessions (e-session-store-create))
                          :enabled-layer-ids (copy-sequence enabled-layer-ids)
                          :intrinsic-capabilities
                          (copy-sequence intrinsic-capabilities)
                          :active-turns (make-hash-table :test 'equal)
                          :prompt-queues (make-hash-table :test 'equal)
                          :prompt-queue-counts (make-hash-table :test 'equal)
                          :provider-compaction-candidates
                          (make-hash-table :test 'equal))))
    (when layer-change-function
      (e-harness-set-layer-change-function harness layer-change-function))
    harness))
(cl-defun e-harness-create-session (harness &key id metadata)
  "Create session ID with METADATA in HARNESS."
  (e-session-create (e-harness-sessions harness)
                    :id id
                    :metadata (e-harness--normalize-session-metadata
                               metadata)))

(cl-defun e-harness-fork-session (harness session-id &key at metadata name)
  "Fork SESSION-ID in HARNESS into a new independent session and return it.
Seeds a fresh session with a snapshot of the source's messages up to AT (a head
entry id; defaults to the current head), copying context metadata and turn
options so the fork resumes with the same working context.  The source session
is left untouched.  See `e-session-fork'."
  (e-session-fork (e-harness-sessions harness)
                  session-id
                  :at at
                  :metadata (e-harness--normalize-session-metadata metadata)
                  :name name))

(defun e-harness--running-turns-p (harness)
  "Return non-nil when HARNESS owns any running turn."
  (let (running)
    (maphash
     (lambda (_session-id entry)
       (when (e-harness-turn-state-active-turn-running-p entry)
         (setq running t)))
     (e-harness-active-turns harness))
    running))

(defun e-harness--apply-pending-runtime-refresh (harness)
  "Apply HARNESS's pending runtime refresh once no turn is running."
  (when-let ((pending (gethash harness e-harness--pending-runtime-refreshes)))
    (plist-put pending :timer nil)
    (unless (e-harness--running-turns-p harness)
      (e-harness-activity-unsubscribe harness (plist-get pending :subscription))
      (remhash harness e-harness--pending-runtime-refreshes)
      (e-harness-refresh-runtime-from harness (plist-get pending :fresh)))))

(defun e-harness--schedule-pending-runtime-refresh (harness)
  "Schedule one post-terminal pending runtime refresh check for HARNESS."
  (when-let ((pending (gethash harness e-harness--pending-runtime-refreshes)))
    (unless (timerp (plist-get pending :timer))
      (plist-put pending :timer
                 (run-at-time 0 nil #'e-harness--apply-pending-runtime-refresh
                              harness)))))

(defun e-harness-request-runtime-refresh (harness fresh)
  "Refresh HARNESS from FRESH now, or at its next idle turn boundary."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (e-harness-p fresh)
    (signal 'wrong-type-argument (list 'e-harness-p fresh)))
  (if (not (e-harness--running-turns-p harness))
      (e-harness-refresh-runtime-from harness fresh)
    (if-let ((pending (gethash harness e-harness--pending-runtime-refreshes)))
        (plist-put pending :fresh fresh)
      (let (subscription)
        (setq subscription
              (e-harness-activity-subscribe
               harness
               (lambda (event)
                 (when (memq (plist-get event :type)
                             '(turn-finished turn-failed turn-cancelled))
                   (e-harness--schedule-pending-runtime-refresh harness)))))
        (puthash harness
                 (list :fresh fresh :subscription subscription :timer nil)
                 e-harness--pending-runtime-refreshes)))
    harness))

(defun e-harness-messages (harness session-id)
  "Return the bounded messages available to SESSION-ID's live consumer.

For an asynchronous SQLite store this is the executing turn's detached query
result, including its bounded optimistic overlay.  It is never a durable
session replica and is nil when no live consumer owns such a result."
  (let ((store (e-harness-sessions harness)))
    (if (e-session-async-enabled-p store)
        (copy-tree
         (plist-get (e-harness-executing-session-state harness session-id)
                    :messages)
         t)
      (e-session-messages store session-id))))

(defun e-harness-message-hidden-p (message)
  "Return non-nil when MESSAGE should be hidden from display.
A message is hidden when its display disposition is `hidden', set either as a
top-level `:display' (used to supersede a stored reply after the fact) or in
its `:metadata' `:display' (used when a message is queued hidden from the
start).  The value may be the symbol `hidden' or the string \"hidden\" after a
JSON replay, so both are recognized."
  (let* ((display (or (plist-get message :display)
                      (plist-get (plist-get message :metadata) :display))))
    (or (eq display 'hidden)
        (equal display "hidden"))))

(defun e-harness-session-title (harness session-id)
  "Return display title for SESSION-ID in HARNESS."
  (let ((store (e-harness-sessions harness)))
    (unless (and (fboundp 'e-session-async-enabled-p)
                 (e-session-async-enabled-p store))
      (e-session-display-title store session-id))))

(defun e-harness-session-name (harness session-id)
  "Return explicit name for SESSION-ID in HARNESS, or nil."
  (let ((store (e-harness-sessions harness)))
    (unless (and (fboundp 'e-session-async-enabled-p)
                 (e-session-async-enabled-p store))
      (plist-get (e-session-get store session-id) :name))))

(defun e-harness-session-list (harness)
  "Return display metadata for sessions owned by HARNESS."
  (e-session-list (e-harness-sessions harness)))

(defun e-harness-root-session-list (harness)
  "Return user-facing root sessions owned by HARNESS."
  (e-session-list-roots (e-harness-sessions harness)))

(defun e-harness-session-activity-events (harness session-id)
  "Return activity events for SESSION-ID in HARNESS."
  (e-session-activity-events (e-harness-sessions harness) session-id))

(cl-defun e-harness-record-hook-audit
    (harness session-id turn-id
             &key owner hook-id outcome details summary pending-summary)
  "Persist one capability hook audit outcome for a settled turn.

OWNER names the capability, HOOK-ID identifies its hook contract, OUTCOME is a
machine-readable result owned by that capability, DETAILS is an opaque plist or
alist owned by the capability, SUMMARY is optional generic presentation text,
and PENDING-SUMMARY is optional activity text to retain while a capability's
queued follow-up replaces the reply.  Core owns only the durable event envelope;
it must not interpret a capability's policy or mistake an audit outcome for a
truth judgment.  Returns the emitted event.

Callers should record an outcome only when their hook actually checked a turn.
This keeps audit volume proportional to conditional enforcement rather than to
all ordinary replies."
  (unless (and (symbolp owner) (not (keywordp owner)))
    (signal 'wrong-type-argument (list 'symbolp owner)))
  (unless (and (stringp hook-id) (not (string-empty-p hook-id)))
    (signal 'wrong-type-argument (list 'stringp hook-id)))
  (unless (symbolp outcome)
    (signal 'wrong-type-argument (list 'symbolp outcome)))
  (let ((payload (list :owner owner
                       :hook-id hook-id
                       :outcome outcome
                       :truth-status 'not-evaluated
                       :summary summary
                       :pending-summary pending-summary
                       :details details)))
    (e-harness-activity-emit-turn-event harness session-id turn-id 'hook-audit payload)
    (car (last (e-harness-session-activity-events harness session-id)))))

(defun e-harness-turn-hook-audits (harness session-id turn-id &optional owner)
  "Return durable hook-audit records for TURN-ID, optionally filtered by OWNER."
  (seq-filter
   (lambda (event)
     (and (eq (plist-get event :event-type) 'hook-audit)
          (equal (plist-get event :turn-id) turn-id)
          (or (null owner)
              (eq (plist-get (plist-get event :payload) :owner) owner))))
   (e-harness-session-activity-events harness session-id)))


(defun e-harness-state (harness session-id)
  "Return settled state for SESSION-ID in HARNESS."
  (let* ((entry (gethash session-id (e-harness-active-turns harness)))
         (session (ignore-errors
                    (e-session-get (e-harness-sessions harness) session-id))))
    (list :session-id session-id
          :active-turn (when (e-harness-turn-state-active-turn-running-p entry)
                         (e-harness-turn-state-active-turn-id entry))
          :message-count (or (plist-get session :message-count) 0))))

(provide 'e-harness)

;;; e-harness.el ends here
