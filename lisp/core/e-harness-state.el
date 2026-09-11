;;; e-harness-state.el --- Explicit runtime state owned by the harness -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module owns the process-local harness aggregate and its explicit
;; substates.  The substates are deliberately named by semantic owner: they
;; are not a miscellaneous property list and no owner is allowed to mutate a
;; sibling's representation.  Application-service and policy owners consume
;; the narrow projections below while `e-harness' remains the public identity
;; of a running runtime.

;;; Code:

(require 'cl-lib)
(require 'e-context)
(require 'e-session)
(require 'seq)
(require 'subr-x)

(cl-defstruct (e-harness-capability-state
               (:constructor e-harness-state--create-capability-state))
  "State owned by effective capability/layer derivation."
  runtime-config enabled-layer-ids intrinsic-capabilities effective-cache)

(cl-defstruct (e-harness-activity-state
               (:constructor e-harness-state--create-activity-state))
  "State owned by durable activity projection and event subscribers."
  subscribers reasoning-streams current-turn-events)

(cl-defstruct (e-harness-turn-state
               (:constructor e-harness-state--create-turn-state))
  "State owned by active turns and prompt queues."
  active-turns prompt-queues prompt-queue-counts
  (queued-input-count 0) (unsettled-generation 0)
  unsettled-change-function)

(cl-defstruct (e-harness-context-state
               (:constructor e-harness-state--create-context-state))
  "State owned by context/compaction/provider-anchor policy."
  provider-compaction-candidates)

(cl-defstruct (e-harness (:constructor e-harness-state--create))
  "A live harness identity with explicitly owned runtime substates.

The scalar configuration and session store are stable harness identity.  Each
high-churn runtime concern lives in one named substate so extraction does not
leave a set of owners sharing an unstructured central bag."
  backend context-strategy default-options default-project-root
  sessions capability-state activity-state turn-state context-state
  work-enrollment-function board-aggregation-function identity-token)

(defun e-harness-state--keyword (plist key)
  "Return KEY's value from PLIST, distinguishing an absent key."
  (when (plist-member plist key)
    (list :present (plist-get plist key))))

(defun e-harness-state-create (&rest args)
  "Construct a harness from keyword ARGS.

The constructor accepts the historical field keywords while creating the
explicit owner substates.  This is a construction operation, not a
compatibility alias for a removed private behavior; it keeps test fixtures and
the retained public aggregate identity restart-safe during the core shape
change."
  (let* ((backend (plist-get args :backend))
         (context-strategy (or (plist-get args :context-strategy)
                               (e-context-transcript-stack-create)))
         (default-options (plist-get args :default-options))
         (default-project-root (plist-get args :default-project-root))
         ;; Endpoint registries must not use the mutable aggregate itself as
         ;; an `equal' hash key.  This token is immutable for the lifetime of
         ;; one live harness and therefore remains stable while owner state
         ;; changes underneath it.
         (identity-token (or (plist-get args :identity-token)
                             (make-symbol "e-harness-identity-")))
         (runtime-config (plist-get args :runtime-capability-config))
         (sessions (or (plist-get args :sessions) (e-session-store-create)))
         (enabled-layer-ids (copy-sequence
                             (plist-get args :enabled-layer-ids)))
         (intrinsic-capabilities (copy-sequence
                                  (plist-get args :intrinsic-capabilities)))
         (active-turns (or (plist-get args :active-turns)
                           (make-hash-table :test 'equal)))
         (prompt-queues (or (plist-get args :prompt-queues)
                            (make-hash-table :test 'equal)))
         (prompt-queue-counts (or (plist-get args :prompt-queue-counts)
                                  (make-hash-table :test 'equal)))
         (provider-candidates
          (or (plist-get args :provider-compaction-candidates)
              (make-hash-table :test 'equal)))
         (capability-state
          (e-harness-state--create-capability-state
           :runtime-config (copy-tree runtime-config)
           :enabled-layer-ids enabled-layer-ids
           :intrinsic-capabilities intrinsic-capabilities))
         (activity-state
          (e-harness-state--create-activity-state
           :reasoning-streams (make-hash-table :test 'equal)
           :current-turn-events (make-hash-table :test 'equal)))
         (turn-state
          (e-harness-state--create-turn-state
           :active-turns active-turns
           :prompt-queues prompt-queues
           :prompt-queue-counts prompt-queue-counts
           :queued-input-count (or (plist-get args :queued-input-count) 0)
           :unsettled-generation
           (or (plist-get args :unsettled-generation) 0)
           :unsettled-change-function
           (plist-get args :unsettled-change-function)))
         (context-state
          (e-harness-state--create-context-state
           :provider-compaction-candidates provider-candidates)))
    (e-harness-state--create
     :backend backend
     :context-strategy context-strategy
     :default-options default-options
     :default-project-root default-project-root
     :sessions sessions
     :capability-state capability-state
     :activity-state activity-state
     :turn-state turn-state
     :context-state context-state
     :work-enrollment-function (plist-get args :work-enrollment-function)
     :board-aggregation-function (plist-get args :board-aggregation-function)
     :identity-token identity-token)))

;; State projections intentionally retain the established public accessor
;; names.  They are implemented here, so owner modules do not call back into
;; the facade for mutable state.  The setters preserve `setf' use in external
;; configuration code while routing mutation to the semantic owner.
(defun e-harness-runtime-capability-config (harness)
  (e-harness-capability-state-runtime-config
   (e-harness-capability-state harness)))
(defun e-harness-state--set-runtime-capability-config (harness value)
  (setf (e-harness-capability-state-runtime-config
         (e-harness-capability-state harness)) value))
(gv-define-simple-setter e-harness-runtime-capability-config
                         e-harness-state--set-runtime-capability-config)

(defun e-harness-enabled-layer-ids (harness)
  (e-harness-capability-state-enabled-layer-ids
   (e-harness-capability-state harness)))
(defun e-harness-state--set-enabled-layer-ids (harness value)
  (setf (e-harness-capability-state-enabled-layer-ids
         (e-harness-capability-state harness)) value))
(gv-define-simple-setter e-harness-enabled-layer-ids
                         e-harness-state--set-enabled-layer-ids)

(defun e-harness-intrinsic-capabilities (harness)
  (e-harness-capability-state-intrinsic-capabilities
   (e-harness-capability-state harness)))
(defun e-harness-state--set-intrinsic-capabilities (harness value)
  (setf (e-harness-capability-state-intrinsic-capabilities
         (e-harness-capability-state harness)) value))
(gv-define-simple-setter e-harness-intrinsic-capabilities
                         e-harness-state--set-intrinsic-capabilities)

(defun e-harness-subscribers (harness)
  (e-harness-activity-state-subscribers
   (e-harness-activity-state harness)))
(defun e-harness-state--set-subscribers (harness value)
  (setf (e-harness-activity-state-subscribers
         (e-harness-activity-state harness)) value))
(gv-define-simple-setter e-harness-subscribers
                         e-harness-state--set-subscribers)

(defun e-harness-active-turns (harness)
  (e-harness-turn-state-active-turns (e-harness-turn-state harness)))
(defun e-harness-state--set-active-turns (harness value)
  (setf (e-harness-turn-state-active-turns
         (e-harness-turn-state harness)) value))
(gv-define-simple-setter e-harness-active-turns
                         e-harness-state--set-active-turns)

(defun e-harness-prompt-queues (harness)
  (e-harness-turn-state-prompt-queues (e-harness-turn-state harness)))
(defun e-harness-state--set-prompt-queues (harness value)
  (setf (e-harness-turn-state-prompt-queues
         (e-harness-turn-state harness)) value))
(gv-define-simple-setter e-harness-prompt-queues
                         e-harness-state--set-prompt-queues)

(defun e-harness-prompt-queue-counts (harness)
  (e-harness-turn-state-prompt-queue-counts (e-harness-turn-state harness)))
(defun e-harness-state--set-prompt-queue-counts (harness value)
  (setf (e-harness-turn-state-prompt-queue-counts
         (e-harness-turn-state harness)) value))
(gv-define-simple-setter e-harness-prompt-queue-counts
                         e-harness-state--set-prompt-queue-counts)

(defun e-harness-queued-input-count (harness)
  (e-harness-turn-state-queued-input-count (e-harness-turn-state harness)))
(defun e-harness-state--set-queued-input-count (harness value)
  (setf (e-harness-turn-state-queued-input-count
         (e-harness-turn-state harness)) value))
(gv-define-simple-setter e-harness-queued-input-count
                         e-harness-state--set-queued-input-count)

(defun e-harness-unsettled-generation (harness)
  (e-harness-turn-state-unsettled-generation (e-harness-turn-state harness)))
(defun e-harness-state--set-unsettled-generation (harness value)
  (setf (e-harness-turn-state-unsettled-generation
         (e-harness-turn-state harness)) value))
(gv-define-simple-setter e-harness-unsettled-generation
                         e-harness-state--set-unsettled-generation)

(defun e-harness-unsettled-change-function (harness)
  (e-harness-turn-state-unsettled-change-function
   (e-harness-turn-state harness)))
(defun e-harness-state--set-unsettled-change-function (harness value)
  (setf (e-harness-turn-state-unsettled-change-function
         (e-harness-turn-state harness)) value))
(gv-define-simple-setter e-harness-unsettled-change-function
                         e-harness-state--set-unsettled-change-function)

(defun e-harness-provider-compaction-candidates (harness)
  (e-harness-context-state-provider-compaction-candidates
   (e-harness-context-state harness)))
(defun e-harness-state--set-provider-compaction-candidates (harness value)
  (setf (e-harness-context-state-provider-compaction-candidates
         (e-harness-context-state harness)) value))
(gv-define-simple-setter e-harness-provider-compaction-candidates
                         e-harness-state--set-provider-compaction-candidates)

(defun e-harness-executing-session-state (harness session-id)
  "Return detached state retained only by SESSION-ID's executing turn.

This value is request-scoped coordination, not a durable session mirror."
  (when-let* ((entry (gethash session-id (e-harness-active-turns harness))))
    (plist-get entry :session-query-state)))

(provide 'e-harness-state)

;;; e-harness-state.el ends here
