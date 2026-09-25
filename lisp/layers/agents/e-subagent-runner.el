;;; e-subagent-runner.el --- Subagent spawn and run coordination for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Spawn coordination for subagents.  The coordinator resolves a spawnable type
;; to its harness instance, creates a fresh child session carrying durable
;; lineage metadata, seeds the child's own store (default prompt-only, optional
;; explicit messages), admits it to the Board, and drives it through a
;; pluggable runner seam.  The default runner starts one non-blocking child turn
;; on the child harness and settles the private live capability record from turn
;; events.  Durable lifecycle/report facts are published to the Board; no
;; terminal result is retained in the live owner.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-chat-service)
(require 'e-session)
(require 'e-runtime-store-codec)
(require 'e-subagent-live)
(require 'e-work)
(require 'e-board-orchestration-actions)

(define-error 'e-subagent-error "e subagent error")
(define-error 'e-subagent-unknown-type
  "No spawnable subagent type is registered for id" 'e-subagent-error)
(define-error 'e-subagent-persistence-suspect
  "Subagent terminal publication did not become durable"
  'e-subagent-error)

(defvar e-subagent-runner--live-owner (e-subagent-live-create)
  "Private live execution owner shared by runner consumers and actions.

The value is deliberately not an inventory API.  It is the one process-local
owner through which the runner and the child-side report action rendezvous;
durable participant, assignment, and terminal facts remain on the Board.")

(defvar e-subagent-runner--dispatch-claims (make-hash-table :test 'equal)
  "Private in-process claims for exact Board assignment dispatches.

The claim only coalesces concurrent dispatch requests while their first
request is in flight.  SQLite remains authoritative; this table prevents two
same-process callers from each admitting a child during the queued-publication
window and is released when the shared dispatch work settles.")

(defun e-subagent-runner-live-owner ()
  "Return the runner-owned private live execution owner.

Consumers receive operations over this owner rather than the owner itself;
this accessor exists only so the capability adapter can share the same
process-local execution boundary."
  e-subagent-runner--live-owner)

(defun e-subagent--lineage-id (live board-id parent-session-id)
  "Return the live tmp-lineage root for PARENT-SESSION-ID in LIVE.

Lineage is execution coordination, not a reason to synchronously reconstruct
the parent's durable session.  Follow only the bounded live execution owner;
an ordinary root session seeds its own lineage id."
  (let ((current parent-session-id)
        (remaining 64)
        parent)
    (while (and (> remaining 0)
                (setq parent
                      (e-subagent-live-find-by-session live board-id current)))
      (setq current
            (when-let* ((callbacks (plist-get parent :callbacks))
                        (getter (plist-get callbacks :record))
                        ((functionp getter))
                        (record (funcall getter)))
              (plist-get record :parent-session-id))
            remaining (1- remaining)))
    current))

(defun e-subagent--type-instance (type)
  "Return the spawnable harness instance for TYPE, or signal."
  (let ((instance (e-harness-instance-get type)))
    (unless (and instance (e-harness-instance-subagent-p instance))
      (signal 'e-subagent-unknown-type (list type)))
    instance))

(defvar e-subagent--configured-harnesses (make-hash-table :test 'eq :weakness 'key)
  "Harnesses whose declaring instance's minimal layers/config were applied.
Keyed weakly by harness so a torn-down harness is re-configured if recreated.")

(defun e-subagent--live-chat-binding (harness session-id)
  "Return SESSION-ID's process-local chat binding or signal clearly."
  (or (e-chat-service-binding harness session-id)
      (signal 'e-subagent-error
              (list "Parent Board binding is not ready" session-id))))

(defconst e-subagent-max-intervention-reason-length 240
  "Maximum width of the audit reason retained for one intervention.")

(defconst e-subagent--inherited-prompt-cache-options
  '(:prompt-cache-default :prompt-cache-retention)
  "Prompt-cache policy options inherited by child sessions when unspecified.")

(defun e-subagent-publication-target (parent-harness parent-session-id)
  "Return the explicit SQL target for PARENT-HARNESS's parent session."
  (let ((binding
         (e-subagent--live-chat-binding parent-harness parent-session-id)))
    (e-board-sqlite-publication-target-create
     (e-chat-service-binding-sqlite-service binding)
     (e-chat-service-binding-board-id binding)
     :author (format "producer:subagent:%s:%s"
                     (e-chat-service-binding-board-id binding)
                     parent-session-id)
     :tags '(subagent))))

(cl-defun e-subagent--publish-board-fact
    (target &key tags attributes content source-fact-key)
  "Publish one bounded fact through explicit SQL TARGET."
  (e-board-sqlite-publication-target-fact-start
   target content source-fact-key
   :tags (copy-tree tags t) :attributes (copy-tree attributes t)))

(defun e-subagent--publish-lifecycle (target record &optional include-result)
  "Publish RECORD's lifecycle state through execution-owned TARGET.
RECORD is a detached publication value, not the live owner record.  The live
owner never stores this durable status or history."
  (let ((attributes (list :participant-id (plist-get record :participant-id)
                          :status (plist-get record :status)
                          :type (plist-get record :type)
                          :parent-session-id (plist-get record :parent-session-id)
                          :session-id (plist-get record :session-id))))
    (when include-result
      (dolist (key '(:result-summary :result :outputs :error))
        (when (plist-member record key)
          (setq attributes
                (plist-put attributes key (copy-tree (plist-get record key)))))))
    (e-subagent--publish-board-fact
     target
     :tags (list 'change (plist-get record :status))
     :attributes attributes
     :content (format "Subagent %s is %s"
                      (plist-get record :participant-id)
                      (plist-get record :status))
     :source-fact-key
     ;; Durable idempotency follows the admitted child session/participant.
     (list 'subagent-lifecycle (plist-get record :participant-id)
           (plist-get record :status)))))

(defcustom e-subagent-child-layer-ids '(subagents-child)
  "Layer ids always added to a spawned child harness.
These are appended to a type's declared `:layers' so a child can always report
its result, the same way base layers ride along on every harness.  A type may
still exclude one explicitly by redefining its `:layers'; that is a supported
choice, not an error."
  :type '(repeat symbol)
  :group 'e)

(defun e-subagent--child-harness (instance)
  "Return INSTANCE's live child harness, applying its declared setup once.
The instance's `:layers' (plus `e-subagent-child-layer-ids') become the
harness's enabled layer set and its `:layer-config' entries seed runtime
capability config, but only on the first creation, so a later `configure-type'
the parent applies is never clobbered."
  (let ((harness (e-harness-instance-get-or-create
                  (e-harness-instance-id instance))))
    (unless (gethash harness e-subagent--configured-harnesses)
      (let ((layers (e-harness-instance-layers instance)))
        (when (or layers e-subagent-child-layer-ids)
          (e-harness-set-enabled-layer-ids
           harness
           (append layers
                   (seq-remove (lambda (id) (memq id layers))
                               e-subagent-child-layer-ids)))))
      (dolist (entry (append (e-harness-instance-layer-config instance) nil))
        (e-harness-set-capability-config
         harness (car entry) (copy-sequence (cdr entry))))
      (puthash harness t e-subagent--configured-harnesses))
    harness))

(defun e-subagent--child-metadata
    (instance parent-harness parent-session-id lineage-id label
              &optional assignment project-root)
  "Return durable child metadata for INSTANCE under a parent lineage.
Inherit the parent's project root so repository AGENTS.md files and
=.agents/skills= are available to the child from its first turn."
  (let ((role (e-harness-instance-kind instance))
        (project-root
         (or project-root
             (and parent-session-id
                  (e-harness-project-root parent-harness parent-session-id)))))
    (append
     (list :tmp-lineage-id lineage-id)
     (when parent-session-id (list :parent-session-id parent-session-id))
     (when project-root (list :project-root project-root))
     (when role (list :subagent-role (symbol-name role)))
     (when (and (stringp label) (not (string-empty-p label)))
       (list :subagent-label label))
     (when assignment
       (list :board-run-id (plist-get assignment :run-id)
             :board-task-key (plist-get assignment :task-key)
             :board-attempt (plist-get assignment :attempt))))))

(defun e-subagent--assignment-input-metadata (record)
  "Return RECORD's bounded durable assignment metadata for its first input."
  (let ((run-id (plist-get record :run-id))
        (task-key (plist-get record :task-key))
        (attempt (plist-get record :attempt))
        (label (plist-get record :label)))
    (when (and (stringp run-id) (stringp task-key) (integerp attempt))
      (append
       (list :board-run-id run-id
             :board-task-key task-key
             :board-attempt attempt)
       (when (and (stringp label) (not (string-empty-p label)))
         (list :subagent-label
               (substring label 0
                          (min (length label)
                               e-board-orchestration-run-set-label-limit))))))))

(defun e-subagent--seed-child (child-harness child-session-id seed-messages)
  "Append SEED-MESSAGES to CHILD-SESSION-ID's own store in CHILD-HARNESS.
Each seed is a backend-neutral message plist; the parent chooses exactly what to
hand over, so nothing leaks that it did not name."
  (dolist (message (append seed-messages nil))
    (e-chat-service-append-seed-message
     child-harness child-session-id message)))

(defun e-subagent--inherit-prompt-cache-policy
    (parent-harness parent-session-id child-harness child-session-id)
  "Give a child session its parent's prompt-cache policy when unspecified.
The child derives its own cache key from its model, root, layers, and tools;
an explicit child harness or session policy always wins."
  (let ((parent-options
         (e-harness-display-options parent-harness parent-session-id))
        (child-defaults (e-harness-default-options child-harness))
        (child-options
         (copy-sequence
          (e-harness-session-options child-harness child-session-id)))
        changed)
    (dolist (key e-subagent--inherited-prompt-cache-options)
      (when (and (plist-member parent-options key)
                 (not (plist-member child-defaults key))
                 (not (plist-member child-options key)))
        (setq child-options
              (plist-put child-options key (plist-get parent-options key)))
        (setq changed t)))
    (when changed
      (e-harness-set-session-options
       child-harness child-session-id child-options))))

(defconst e-subagent--progress-diagnostic-byte-limit 512
  "Maximum UTF-8 bytes retained for one progress projection diagnostic.")

(defconst e-subagent--publication-identity-byte-limit 128
  "Maximum UTF-8 bytes retained for one publication identity scalar.")

(defconst e-subagent--persistence-suspect-byte-limit (* 8 1024)
  "Maximum canonical bytes retained by one persistence-suspect composite.")

(defconst e-subagent--persistence-suspect-terminal-args-byte-limit (* 4 1024)
  "Maximum UTF-8/canonical bytes retained for terminal proposal arguments.
This is the existing 4 KiB terminal payload allowance.")

(defconst e-subagent--persistence-suspect-publication-error-byte-limit 1024
  "Maximum UTF-8 bytes retained for one publication error projection.")

(defconst e-subagent--persistence-suspect-cause-byte-limit 1024
  "Maximum UTF-8 bytes retained for a fallback terminal cause text.")

(defconst e-subagent--persistence-suspect-fallback-field-byte-limit 512
  "Maximum canonical bytes attempted for one fallback result field.")

(defconst e-subagent--persistence-suspect-static-byte-budget 512
  "Reserved bytes for persistence-suspect labels and plist structure.")

(defconst e-subagent--persistence-suspect-identity-byte-budget
  (* 7 e-subagent--publication-identity-byte-limit)
  "Seven bounded identity scalars, each using the 128-byte identity budget.
These cover participant/status/proposal status, publication error identity, and
the three publication-state identities.")

(defconst e-subagent--persistence-suspect-projection-byte-budget
  (+ e-subagent--persistence-suspect-static-byte-budget
     e-subagent--persistence-suspect-terminal-args-byte-limit
     e-subagent--persistence-suspect-publication-error-byte-limit
     e-subagent--persistence-suspect-identity-byte-budget)
  "Explicit worst-case projection budget: 6.5 KiB before codec overhead.
This is 512 bytes of fixed structure, 4 KiB of terminal args, 1 KiB of
publication-error text, and seven 128-byte identity scalars; it remains below
the final 8 KiB persistence-suspect ceiling.")

(defun e-subagent--utf8-byte-prefix (value byte-limit)
  "Return property-free UTF-8 text VALUE bounded by BYTE-LIMIT bytes.
The prefix ends only at character boundaries, so it is always valid text even
when VALUE contains multibyte characters or zero-width combining marks."
  (let* ((text (if (stringp value) value (format "%s" value)))
         (limit (max 0 (or byte-limit 0)))
         (position 0)
         (bytes 0)
         (length (length text))
         (end length))
    (while (< position length)
      (let ((character-bytes
             (string-bytes (substring text position (1+ position)))))
        (if (> (+ bytes character-bytes) limit)
            (setq end position
                  position length)
          (setq bytes (+ bytes character-bytes)
                position (1+ position)))))
    (substring-no-properties text 0 end)))

(defun e-subagent--bounded-scalar (value byte-limit)
  "Return VALUE as a closed scalar bounded by BYTE-LIMIT bytes."
  (cond
   ((symbolp value)
    (let ((name (symbol-name value)))
      (if (<= (string-bytes name) byte-limit)
          value
        (e-subagent--utf8-byte-prefix name byte-limit))))
   ((stringp value)
    (e-subagent--utf8-byte-prefix value byte-limit))
   ((numberp value)
    (let ((text (format "%s" value)))
      (if (<= (string-bytes text) byte-limit)
          value
        (e-subagent--utf8-byte-prefix text byte-limit))))
   (t (type-of value))))

(defun e-subagent--progress-event-descriptor (event)
  "Return a closed descriptor for progress EVENT.
Never retain or print an arbitrary event payload.  A structured event exposes
only its bounded `:type' scalar; all other opaque values collapse to a type
category."
  (cond
   ((symbolp event) event)
   ((stringp event)
    (e-subagent--utf8-byte-prefix
     event e-subagent--progress-diagnostic-byte-limit))
   ((and (listp event) (plist-member event :type))
    (e-subagent--bounded-scalar
     (plist-get event :type) e-subagent--progress-diagnostic-byte-limit))
   (t (type-of event))))

(defun e-subagent--progress-error-text (error)
  "Return bounded text for progress diagnostic ERROR."
  (e-subagent--utf8-byte-prefix
   (e-work-error-message error)
   e-subagent--progress-diagnostic-byte-limit))

(defun e-subagent--progress-diagnostic (event error)
  "Return one bounded diagnostic for a failed progress projection.
The diagnostic is process-local metadata, not a durable lifecycle result."
  (list :event (e-subagent--progress-event-descriptor event)
        :error
        (e-subagent--progress-error-text error)))

(defun e-subagent--display-progress-diagnostic (event error &optional prefix)
  "Display bounded progress ERROR without entering child lifecycle code.
PREFIX describes a diagnostic sink failure when supplied.  The final `message'
fallback keeps an owner-visible breadcrumb even when warning display itself is
temporarily unavailable."
  (let ((message-text
         (e-subagent--utf8-byte-prefix
          (format "%s for %s: %s"
                  (or prefix "Progress projection failed")
                  (e-subagent--progress-event-descriptor event)
                  (e-subagent--progress-error-text error))
          (* 2 e-subagent--progress-diagnostic-byte-limit))))
    (condition-case display-error
        (display-warning 'e-subagent message-text :warning)
      (error
       (message "%s"
                (e-subagent--utf8-byte-prefix
                 (format "e-subagent: %s (warning display failed: %s)"
                         message-text
                         (e-subagent--progress-error-text display-error))
                 (* 3 e-subagent--progress-diagnostic-byte-limit)))))))

(defun e-subagent--remember-progress-error (work-handle event error)
  "Record one bounded progress projection ERROR on WORK-HANDLE metadata.
This is deliberately best effort and cannot alter Work's terminal lifecycle."
  (when (e-work-handle-p work-handle)
    (let ((metadata (e-work-handle-metadata work-handle))
          (diagnostic (e-subagent--progress-diagnostic event error))
          filtered)
      ;; Keep this diagnostic latest-visible and constant-size.  Metadata is a
      ;; plist, so replacing the first key while dropping any duplicate stale
      ;; keys is both cheaper and less ambiguous than appending another pair.
      (while metadata
        (let ((key (pop metadata))
              (value (pop metadata)))
          (unless (eq key :progress-error)
            (setq filtered (append filtered (list key value))))))
      (setf (e-work-handle-metadata work-handle)
            (append filtered (list :progress-error diagnostic))))))

(defun e-subagent--safe-progress (on-progress event &optional on-error)
  "Deliver child EVENT to ON-PROGRESS without affecting terminal settlement.
Progress is observational only: a presentation callback that signals must not
prevent the provider's terminal callback from reaching the Work gate.  ON-ERROR
receives EVENT and the original condition for a bounded owner diagnostic."
  (when on-progress
    (condition-case error
        (funcall on-progress event)
      (error
       (if on-error
           (condition-case diagnostic-error
               (funcall on-error event error)
             (error
              ;; A diagnostic sink is itself observational.  If it fails,
              ;; retain an owner-visible bounded breadcrumb rather than
              ;; silently discarding both projection errors.
              (e-subagent--display-progress-diagnostic
               event diagnostic-error
               "Progress diagnostic sink failed")))
         ;; Direct-runner callers which do not own a Work handle still get a
         ;; bounded diagnostic; the projection error cannot fault the
         ;; subscription callback or alter terminal settlement.
         (e-subagent--display-progress-diagnostic event error))))))

(defun e-subagent-direct-runner (child-harness child-session-id prompt
                                               seed-messages on-settle
                                               &optional on-progress input-metadata)
  "Prepare capabilities, seed, and start one non-blocking child turn.
Capability readiness is awaited through callbacks before the first provider
request, so a fresh child sees any eagerly prepared resources on that turn.
Settlement is reported through ON-SETTLE.
Returns a handle plist carrying a `:cancel' function that aborts the child's
active turn.  ON-SETTLE is called as (STATUS &key summary outputs error)."
  (e-subagent--seed-child child-harness child-session-id seed-messages)
  (let ((settled nil)
        (last-assistant nil)
        (turn-started nil)
        readiness-works
        cancel-readiness
        subscription)
    (cl-labels
        ((finish
          (status &rest args)
          (unless settled
            (setq settled t)
            (when subscription
              (e-chat-service-unsubscribe subscription))
            (apply on-settle status args)
            (e-subagent--safe-progress
             on-progress
             (pcase status
               ('done 'turn-finished)
               ('failed 'turn-failed)
               ('cancelled 'turn-cancelled)))))
         (submit
          ()
          (unless settled
            (setq turn-started t)
            (condition-case err
                (let ((admission
                       (if input-metadata
                           (e-chat-service-submit-session
                            child-harness child-session-id prompt
                            :metadata input-metadata)
                         (e-chat-service-submit-session
                          child-harness child-session-id prompt))))
                  (e-work-on-settle
                   admission
                   (lambda (settled-admission)
                     (let* ((status (e-work-status settled-admission))
                            (state (plist-get status :state)))
                       (pcase state
                         ('failed
                          (finish 'failed
                                  :error (e-work-error-message
                                          (plist-get status :error))))
                         ('cancelled (finish 'cancelled)))))))
              (error
               (finish 'failed :error (e-work-error-message err))))))
         (readiness-settled
          (settled-set)
          (setq cancel-readiness nil)
          (unless settled
            (let ((failed
                   (seq-find
                    (lambda (work)
                      (memq (plist-get (e-work-status work) :state)
                            '(failed cancelled)))
                    (plist-get settled-set :done))))
              (pcase (and failed
                          (plist-get (e-work-status failed) :state))
                ('failed
                 (finish 'failed
                         :error (e-work-error-message
                                 (plist-get (e-work-status failed) :error))))
                ('cancelled (finish 'cancelled))
                (_ (submit)))))))
      (setq subscription
            (e-chat-service-subscribe
             child-harness child-session-id
             (lambda (event)
               (when-let* ((message (plist-get (plist-get event :payload) :message))
                           ((eq (plist-get message :role) 'assistant))
                           (content (plist-get message :content)))
                 (setq last-assistant content))
               (pcase (plist-get event :type)
                 ((or 'provider-request-started 'provider-request-finished
                      'tool-started 'tool-finished 'action-started
                      'action-finished 'action-failed 'turn-steered)
                 (e-subagent--safe-progress
                   on-progress (plist-get event :type)))
                 ('turn-finished
                  (finish 'done
                          :summary last-assistant))
                 ('turn-failed
                  (finish 'failed
                          :error (or (plist-get (plist-get event :payload)
                                                :error)
                                     "Subagent turn failed")))
                 ('turn-cancelled
                  (finish 'cancelled))))
             ))
      (condition-case err
          (setq readiness-works
                (and child-harness
                     (e-harness-capability-readiness-start
                      child-harness child-session-id)))
        (error
         (finish 'failed :error (e-work-error-message err))))
      (cond
       (settled nil)
       ((null readiness-works) (submit))
       (t
        (setq cancel-readiness
              (e-work-await-set
               readiness-works :mode 'all :on-settle #'readiness-settled))))
      (list :cancel
            (lambda ()
              (when cancel-readiness
                (funcall cancel-readiness)
                (setq cancel-readiness nil))
              (dolist (work readiness-works)
                (unless (memq (plist-get (e-work-status work) :state)
                              '(finished failed cancelled))
                  (e-work-cancel work)))
              (if turn-started
                  (ignore-errors
                    (e-chat-service-abort-session
                     child-harness child-session-id))
                (finish 'cancelled)))))))

(defun e-subagent--progress-summary (event)
  "Return a bounded human-readable progress summary for child EVENT."
  (pcase event
    ('turn-started "Started child turn")
    ('provider-request-started "Started provider request")
    ('provider-request-finished "Finished provider request")
    ('tool-started "Started tool")
    ('tool-finished "Finished tool")
    ('action-started "Started action")
    ((or 'action-finished 'action-failed) "Finished action")
    ('turn-steered "Steered child turn")
    ('turn-finished "Finished child turn")
    ('turn-failed "Child turn failed")
    ('turn-cancelled "Child turn cancelled")
    (_ (e-subagent--utf8-byte-prefix
        (format "%s" (e-subagent--progress-event-descriptor event))
        e-subagent--progress-diagnostic-byte-limit))))

(defun e-subagent--record-progress (live board-id participant-id work-handle event)
  "Publish bounded child EVENT progress through WORK-HANDLE and LIVE."
  (let ((snapshot
         (e-subagent-live-record-progress
          live board-id participant-id
          (list :participant-id participant-id
                :event event
                :summary (e-subagent--progress-summary event)
                :at (float-time)))))
    (e-work-progress work-handle snapshot)
    snapshot))

(defun e-subagent--work-spec ()
  "Return the cooperative work spec that wraps a spawned child turn.
The child turn is driven by the harness, not this runner, so the spec's runner
defers; `e-subagent-spawn' finishes/fails the handle from the settle callback.
The handle exists so a subagent is awaitable as an `e-work' handle."
  (e-work-spec-create
   :id "subagent"
   :description "Track a spawned subagent child turn."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'subagents
   :runner (lambda (_handle _arguments _context) :deferred)))

(defconst e-subagent-runner--dispatch-work-spec
  (e-work-spec-create
   :id "subagent-dispatch" :execution 'cooperative
   :interactive-policy 'async :owner 'subagents
   :runner (lambda (_handle _arguments _context) :deferred))
  "Work contract for one Board-addressed initial dispatch disposition.")

(defun e-subagent-runner--assignment (run-id task-key attempt)
  "Validate and return one detached RUN-ID/TASK-KEY/ATTEMPT assignment."
  (unless (and (stringp run-id) (not (string-empty-p run-id))
               (stringp task-key) (not (string-empty-p task-key))
               (integerp attempt) (>= attempt 0))
    (signal 'e-subagent-error
            (list "Invalid durable subagent assignment"
                  run-id task-key attempt)))
  (list :run-id (copy-sequence run-id)
        :task-key (copy-sequence task-key)
        :attempt attempt))

(defun e-subagent--validate-deadline (deadline)
  "Validate optional positive absolute DEADLINE and return it.
The dispatch contract deliberately accepts an absolute timestamp rather than
owning a duration or default policy.  A past positive timestamp is valid and
settles the child as soon as its Work carrier starts."
  (when (and deadline
             (or (not (numberp deadline))
                 (not (> deadline 0))))
    (signal 'e-subagent-error
            (list "Subagent dispatch deadline must be a positive absolute timestamp"
                  deadline)))
  deadline)

(defun e-subagent-runner--attempt-key (assignment status)
  "Return the stable idempotency key for ASSIGNMENT STATUS."
  (format "dispatch:%s:%s:%d:%s"
          (plist-get assignment :run-id)
          (plist-get assignment :task-key)
          (plist-get assignment :attempt)
          status))

(defun e-subagent-runner--dispatch-claim-key (board-id assignment)
  "Return the exact private claim key for BOARD-ID ASSIGNMENT."
  (list board-id
        (plist-get assignment :run-id)
        (plist-get assignment :task-key)
        (plist-get assignment :attempt)))

(defun e-subagent-runner--dispatch-work-active-p (work)
  "Return non-nil when WORK is a still-pending dispatch outcome."
  (and (e-work-handle-p work)
       (not (memq (plist-get (e-work-status work) :state)
                  '(finished failed cancelled)))))

(defun e-subagent-runner--release-dispatch-claim (claim-key work)
  "Release CLAIM-KEY when it still names WORK."
  (when (eq (gethash claim-key e-subagent-runner--dispatch-claims) work)
    (remhash claim-key e-subagent-runner--dispatch-claims)))

(defun e-subagent-runner--publish-attempt (target assignment status)
  "Publish one bounded initial-dispatch STATUS for ASSIGNMENT."
  (e-board-sqlite-publication-target-orchestration-fact-start
   target
   (list :version e-board-orchestration-fact-version
         :type 'task-attempt
         :idempotency-key (e-subagent-runner--attempt-key assignment status)
         :payload (append (copy-tree assignment)
                          (list :status status)))))

(defun e-subagent--pending-result (board-id participant-id work-handle)
  "Return the bounded pending result for PARTICIPANT-ID admission."
  (list :participant-id participant-id
        :await-ref (e-subagent-live-reference board-id participant-id)
        :status 'pending
        :session-id participant-id
        :work-id (e-work-handle-id work-handle)))

(defun e-subagent--settle-work-handle (handle status args)
  "Settle work HANDLE from a subagent STATUS and settle ARGS.
The handle mirrors the record's terminal state so `await' can observe it; its
finished result carries the compact summary and outputs."
  (when (e-work-handle-p handle)
    (pcase status
      ('done (e-work-finish handle
                            (list :summary (plist-get args :summary)
                                  :result (copy-tree (plist-get args :result))
                                  :outputs (plist-get args :outputs))))
      ('failed (e-work-fail handle
                            (list 'e-subagent-error
                                  (or (plist-get args :error)
                                      "Subagent turn failed"))))
      ('cancelled (e-work-cancel handle)))))

(defconst e-subagent--missing-admitted-report-error
  "Subagent completed without an accepted report"
  "Bounded terminal error for a gated child that reports only final prose.")

(defconst e-subagent--terminal-payload-byte-limit 4096
  "Maximum canonical bytes retained or published for one terminal payload.")

(defun e-subagent--terminal-error-text (error)
  "Return ERROR as bounded public terminal text.
Internal Work failures remain typed conditions for `await' and diagnostics;
detached runner records, Board reports, and dispatch results expose text so a
consumer never has to print a condition list as if it were a string."
  (cond
   ((null error) nil)
   ((stringp error) error)
   ((and (consp error) (symbolp (car error)))
    ;; Keep the condition identity in the bounded public string.  Durable
    ;; Board consumers need to distinguish deadline, provider, and persistence
    ;; failures even though the public result cannot carry a live condition.
    (format "%s: %s" (car error) (e-work-error-message error)))
   (t (condition-case nil
          (e-work-error-message error)
        (error (e-prin1-safe error))))))

(defun e-subagent--public-terminal-args (args)
  "Return ARGS with its optional error field projected to public text."
  (if (plist-member args :error)
      (plist-put (copy-tree args)
                 :error (e-subagent--terminal-error-text
                         (plist-get args :error)))
    (copy-tree args)))

(defun e-subagent--terminal-record (record report-state status args)
  "Return detached terminal RECORD for STATUS and bounded ARGS.
No Board or live-owner side effect occurs here.  The caller decides whether
the record is published, retained for a gate, or immediately retired."
  (pcase-let* ((`(,status . ,args)
                (e-subagent--effective-settlement
                 report-state status args)))
    (setq args (e-subagent--bounded-terminal-args args))
    (let* ((finished-at (float-time))
           (terminal (copy-tree record)))
      (setq terminal (plist-put terminal :status status))
      (setq terminal (plist-put terminal :finished-at finished-at))
      (setq terminal (plist-put terminal :last-turn-at finished-at))
      (when (plist-member args :summary)
        (setq terminal (plist-put terminal :result-summary
                                  (plist-get args :summary))))
      (when (plist-member args :outputs)
        (setq terminal (plist-put terminal :outputs
                                  (copy-tree (plist-get args :outputs)))))
      (when (plist-member args :result)
        (setq terminal (plist-put terminal :result
                                  (copy-tree (plist-get args :result)))))
      (when (plist-member args :error)
        (setq terminal
              (plist-put terminal :error
                         (e-subagent--terminal-error-text
                          (plist-get args :error)))))
      terminal)))

(defun e-subagent--terminal-publication-works (target terminal status)
  "Start durable terminal publications for TERMINAL and return their Works.
Run-bound children publish both the lifecycle fact and orchestration report;
ad-hoc children publish one bounded lifecycle fact carrying the terminal
result.  The returned list is intentionally the only value the gate awaits."
  (let ((assignment (e-subagent--durable-assignment terminal)))
    (if assignment
        (list (e-subagent--publish-lifecycle target terminal nil)
              (e-subagent--publish-terminal-report target terminal status))
      (list (e-subagent--publish-lifecycle target terminal t)))))

(defun e-subagent--bounded-publication-error (error &optional byte-limit)
  "Return a bounded detached representation of publication ERROR.
Keep a condition's symbol so the composite remains classifiable, but never
retain its potentially arbitrary condition data in process-local state."
  (when error
    (let ((text-limit
           (or byte-limit
               e-subagent--persistence-suspect-publication-error-byte-limit)))
      (if (and (consp error) (symbolp (car error)))
          (list (e-subagent--bounded-scalar
                 (car error)
                 e-subagent--publication-identity-byte-limit)
                (e-subagent--utf8-byte-prefix
                 (format "%s: %s" (car error)
                         (e-work-error-message error))
                 text-limit))
        (e-subagent--utf8-byte-prefix
         (e-work-error-message error) text-limit)))))

(defun e-subagent--bounded-publication-scalar
    (value &optional byte-limit)
  "Return one closed, bounded scalar from publication STATE VALUE."
  (let ((bounded (e-subagent--bounded-scalar
                  value
                  (or byte-limit e-subagent--publication-identity-byte-limit))))
    (if (memq bounded '(cons vector hash-table buffer marker))
        nil
      bounded)))

(defun e-subagent--bounded-publication-state (state)
  "Return the closed detached projection of failed publication STATE.
Never retain result, metadata, progress, or arbitrary objects from a Work
status in a persistence-suspect composite."
  (list :state (e-subagent--bounded-publication-scalar
                (plist-get state :state))
        :id (e-subagent--bounded-publication-scalar
             (plist-get state :id))
        :spec-id (e-subagent--bounded-publication-scalar
                  (plist-get state :spec-id))))

(defun e-subagent--persistence-suspect-fallback-text (value)
  "Return bounded property-free text for fallback SUMMARY VALUE."
  (when value
    (condition-case nil
        (e-subagent--utf8-byte-prefix
         (if (stringp value)
             value
           (e-format-safe "%s" value))
         e-subagent--persistence-suspect-cause-byte-limit)
      (error "Unprintable terminal summary"))))

(defun e-subagent--persistence-suspect-fallback-descriptor (value)
  "Return a closed descriptor for an unrepresentable fallback VALUE."
  (list :type
        (e-subagent--bounded-publication-scalar
         (type-of value)
         e-subagent--publication-identity-byte-limit)))

(defun e-subagent--persistence-suspect-fallback-value (value)
  "Return a detached bounded VALUE, or a closed type descriptor."
  (condition-case nil
      (e-runtime-store-codec-decode
       (e-runtime-store-codec-encode-bounded
        value e-subagent--persistence-suspect-fallback-field-byte-limit))
    (error (e-subagent--persistence-suspect-fallback-descriptor value))))

(defun e-subagent--persistence-suspect-fallback-arg (key value)
  "Return the bounded fallback projection for allowed terminal ARGS KEY."
  (pcase key
    (:summary (e-subagent--persistence-suspect-fallback-text value))
    (:error
     (e-subagent--bounded-publication-error
      value e-subagent--persistence-suspect-cause-byte-limit))
    ((or :result :outputs)
     (e-subagent--persistence-suspect-fallback-value value))))

(defun e-subagent--persistence-suspect-fallback-args (terminal-args)
  "Return status-neutral bounded fallback ARGS preserving present keys.
Only the existing terminal keys are projected.  In particular, a finished or
cancelled proposal with no error never receives a fabricated `:error'."
  (when terminal-args
    (let (fallback)
      (dolist (key '(:summary :result :outputs :error))
        (when (condition-case nil
                  (and (listp terminal-args)
                       (plist-member terminal-args key))
                (error nil))
          (setq fallback
                (plist-put
                 fallback key
                 (e-subagent--persistence-suspect-fallback-arg
                  key (plist-get terminal-args key))))))
      fallback)))

(defun e-subagent--persistence-suspect-fallback
    (participant-id terminal-status terminal-args publication-error
                    publication-state)
  "Return the scalar-only fallback for a failed composite conversion.
This conversion is deliberately defensive only at the persistence-suspect
boundary: even a malformed Work status or codec failure must not strand the
already-proposed lifecycle."
  (let ((participant
         (condition-case nil
             (e-subagent--bounded-publication-scalar participant-id)
           (error nil)))
        (status
         (condition-case nil
             (e-subagent--bounded-publication-scalar terminal-status)
           (error nil)))
        (args (e-subagent--persistence-suspect-fallback-args terminal-args))
        (publication
         (condition-case nil
             (or (e-subagent--bounded-publication-error
                  publication-error
                  e-subagent--persistence-suspect-publication-error-byte-limit)
                 "Publication error unavailable")
           (error "Publication error unavailable")))
        (state
         (condition-case nil
             (or (e-subagent--bounded-publication-scalar
                  (plist-get publication-state :state))
                 'unknown)
           (error 'unknown)))
        (id
         (condition-case nil
             (e-subagent--bounded-publication-scalar
              (plist-get publication-state :id))
           (error nil)))
        (spec-id
         (condition-case nil
             (e-subagent--bounded-publication-scalar
              (plist-get publication-state :spec-id))
           (error nil))))
    (list 'e-subagent-persistence-suspect
          "Subagent terminal publication failed"
          :participant-id participant
          :terminal-status status
          :terminal-proposal
          (list :status status :args args)
          :publication-error publication
          :publication-state (list :state state :id id :spec-id spec-id))))

(defun e-subagent--persistence-suspect
    (participant-id terminal-status terminal-args publication-error
                    publication-state)
  "Return one bounded composite for a failed terminal publication.
TERMINAL-ARGS is the detached original provider/deadline/cancellation cause;
PUBLICATION-ERROR and PUBLICATION-STATE describe the failed durable write."
  ;; The projected field budgets total
  ;; `e-subagent--persistence-suspect-projection-byte-budget' (6.5 KiB):
  ;; 4 KiB terminal args, 1 KiB publication-error text, seven 128-byte
  ;; identity scalars, and fixed plist overhead.  That is safely below the
  ;; final 8 KiB ceiling.
  (condition-case _error
      (let ((composite
             (list 'e-subagent-persistence-suspect
                   "Subagent terminal publication failed"
                   :participant-id (e-subagent--bounded-publication-scalar
                                    participant-id)
                   :terminal-status (e-subagent--bounded-publication-scalar
                                     terminal-status)
                   :terminal-proposal
                   (list :status (e-subagent--bounded-publication-scalar
                                  terminal-status)
                         :args
                         (e-subagent--bounded-terminal-args
                          terminal-args
                          e-subagent--persistence-suspect-terminal-args-byte-limit))
                   :publication-error (e-subagent--bounded-publication-error
                                       publication-error
                                       e-subagent--persistence-suspect-publication-error-byte-limit)
                   :publication-state (e-subagent--bounded-publication-state
                                       publication-state))))
        ;; Decoding the bounded canonical value detaches strings/lists from
        ;; failed Work state before the composite crosses the lifecycle gate.
        (e-runtime-store-codec-decode
         (e-runtime-store-codec-encode-bounded
          composite e-subagent--persistence-suspect-byte-limit)))
    (error
     (e-subagent--persistence-suspect-fallback
      participant-id terminal-status terminal-args publication-error
      publication-state))))

(defun e-subagent--terminal-gate
    (live board-id participant-id record target report-state authorize-callback)
  "Return a pre-start Work terminal gate for one spawned child.
The first Work terminal proposal starts all required durable terminal
publications.  Work settlement and private live retirement happen only after
every publication has settled successfully.  A publication failure is
converted into one typed composite failure so callers cannot mistake a
provider result for a durable Board result."
  (lambda (_handle state payload authorize)
    (let* ((stored-status (plist-get report-state :terminal-status))
           (stored-args (plist-get report-state :terminal-args))
           (terminal-status (or stored-status state))
           (terminal-args
            (e-subagent--bounded-terminal-args
             (or stored-args
                 (pcase state
                   ('finished (list :result payload))
                   ('failed (list :error payload))
                   (_ nil)))))
           (live-entry (e-subagent-live-get live board-id participant-id))
           (assignment (e-subagent--durable-assignment record))
           (target-valid (and target
                              (e-board-sqlite-publication-target-valid-p
                               target))))
      (if (and (null assignment)
               (or (null live-entry) (not target-valid)))
          ;; An ad-hoc admission can settle before a live child exists.  It
          ;; has no durable assignment.  If its optional audit target has also
          ;; disappeared, local cancellation remains correct and must not be
          ;; replaced by a publication-shape error.  In either case the Work
          ;; gate remains the sole local owner.
          (progn
            (e-subagent-live-remove live board-id participant-id)
            (funcall authorize)
            (when authorize-callback
              (funcall authorize-callback terminal-status
                       (e-subagent--public-terminal-args terminal-args)
                       nil)))
        (condition-case error
            (let* ((terminal
                    (e-subagent--terminal-record
                     record report-state terminal-status terminal-args))
                   (publications
                    (e-subagent--terminal-publication-works
                     target terminal terminal-status)))
              (e-work-await-set
               publications
               :mode 'all
               :on-settle
               (lambda (settled)
                 (let ((failed
                        (seq-find
                         (lambda (work)
                           (memq (plist-get (e-work-status work) :state)
                                 '(failed cancelled)))
                         (plist-get settled :done))))
                   ;; Publication acknowledgement is the boundary after which
                   ;; live retirement and Work settlement are both safe.
                   (e-subagent-live-remove live board-id participant-id)
                   (if failed
                       (let* ((publication-state (e-work-status failed))
                              (publication-error
                               (plist-get publication-state :error))
                              (composite
                               (e-subagent--persistence-suspect
                                participant-id terminal-status terminal-args
                                publication-error publication-state)))
                         (funcall authorize 'failed composite)
                         (when authorize-callback
                           (funcall authorize-callback
                                    'failed
                                    (list :error
                                          (e-subagent--terminal-error-text
                                           composite))
                                    composite)))
                     (funcall authorize)
                     (when authorize-callback
                       (funcall authorize-callback
                                terminal-status
                                (e-subagent--public-terminal-args
                                 terminal-args)
                                nil)))))))
          (error
           (e-subagent-live-remove live board-id participant-id)
           (let ((composite
                  (e-subagent--persistence-suspect
                   participant-id terminal-status terminal-args error
                   (list :state 'start-failed
                         :error (e-subagent--bounded-publication-error
                                 error)))))
             (funcall authorize 'failed composite)
             (when authorize-callback
               (funcall authorize-callback
                        'failed (list :error composite) composite)))))))))

(defun e-subagent--bounded-terminal-args (args &optional byte-limit)
  "Return a bounded detached copy of terminal ARGS.

This bound applies to both ad-hoc lifecycle payloads and run-bound reports;
the live owner never becomes a bypass around the Board's bounded observation
contract."
  (let ((fields nil))
    (dolist (key '(:summary :result :outputs :error))
      (when (plist-member args key)
        (setq fields (plist-put fields key (plist-get args key)))))
    (if fields
        (e-runtime-store-codec-decode
         (e-runtime-store-codec-encode-bounded
          fields (or byte-limit e-subagent--terminal-payload-byte-limit)))
      nil)))

(defun e-subagent--report-assignment (record)
  "Return RECORD's detached child assignment for report admission."
  (list :run-id (plist-get record :run-id)
        :task-key (plist-get record :task-key)
        :attempt (plist-get record :attempt)
        :participant-id (plist-get record :participant-id)
        :session-id (plist-get record :session-id)
        :parent-session-id (plist-get record :parent-session-id)))

(defun e-subagent--effective-settlement (report-state status args)
  "Return effective (STATUS . ARGS) for one runner settlement.
A gated child cannot translate final prose into success.  An accepted report
also supplies the exact result observed by its work handle, independently of
later final prose.  REPORT-STATE is runner-owned closure state, never a
durable or public live-table projection."
  (let ((report (and report-state (plist-get report-state :report))))
    (cond
     ((and report-state
           (plist-get report-state :report-admission)
           (eq status 'done)
           (null report))
      (cons 'failed
            (list :error e-subagent--missing-admitted-report-error)))
     ((and report (eq status 'done))
      (cons status
            (list :summary (plist-get report :summary)
                  :result (copy-tree (plist-get report :result))
                  :outputs (copy-tree (plist-get report :outputs)))))
     (t (cons status args)))))

(defun e-subagent--settle-runner
    (record report-state work-handle status args)
  "Settle runner STATUS and ARGS through the report-admission boundary."
  (pcase-let* ((`(,effective-status . ,effective-args)
                (e-subagent--effective-settlement
                 report-state status args)))
    ;; The first runner callback owns the terminal proposal.  Keep its
    ;; bounded arguments in the gate's closure so late provider callbacks
    ;; cannot replace the payload while Board publication is in flight.  A
    ;; late callback returns nil just as the pre-gate settlement path did.
    (if (plist-get report-state :terminal-status)
        nil
      (setf (plist-get report-state :terminal-status) effective-status)
      (setf (plist-get report-state :terminal-args)
            (e-subagent--bounded-terminal-args effective-args))
      (e-subagent--settle-work-handle
       work-handle effective-status
       (e-subagent--bounded-terminal-args effective-args))
      ;; Preserve the runner callback's detached terminal record shape for
      ;; existing consumers.  This is an observation value only; the Work gate
      ;; still owns publication acknowledgement and private live retirement.
      (e-subagent--terminal-record
       record report-state effective-status effective-args))))

(defun e-subagent--durable-assignment (record)
  "Return RECORD's persisted orchestration assignment, or nil."
  (when-let* ((run-id (plist-get record :run-id)))
    (list :run-id run-id
          :task-key (plist-get record :task-key)
          :attempt (plist-get record :attempt))))

(defun e-subagent--publish-terminal-report (target record status)
  "Publish RECORD's terminal orchestration fact through TARGET.
SQLite source keys provide idempotency; no terminal publication state is
retained in the private live owner."
  (when-let* ((assignment (e-subagent--durable-assignment record)))
    (e-board-orchestration-actions-publish-terminal
     target assignment status
     :summary (or (plist-get record :result-summary) "")
     :result (copy-tree (plist-get record :result))
     :outputs (or (plist-get record :outputs) [])
     :error (plist-get record :error)
     :author (list :session-id (plist-get record :session-id)))))

(defun e-subagent--drive-turn
    (live board-id publication-target participant-id record
          parent-harness parent-session-id source-turn-id
          child-harness session-id prompt seed-messages runner report-state
          &optional work-handle on-running on-runner-started
          on-terminal)
  "Start one child turn and wire its settle + work handle.
PUBLICATION-TARGET is the execution-owned durable Board destination.  RECORD
is detached runner context; LIVE retains only the capabilities needed by this
turn.  Reuse WORK-HANDLE when admission prepared it, otherwise mint a fresh
cooperative handle.  ON-RUNNING runs immediately before invoking RUNNER."
  (let* ((runner (or runner #'e-subagent-direct-runner))
         ;; Prepare before enrollment: board ownership must be established
         ;; before runner entry, just as it is for model-facing tool work.
         (work-handle
          (or work-handle
              (e-work-prepare
               (e-subagent--work-spec) nil
               :context (list :session-id parent-session-id
                              :turn-id source-turn-id
                              :work-kind 'subagent
                              :domain-ref (e-subagent-live-reference
                                           board-id participant-id)))))
         (record-progress
          (lambda (event)
            (condition-case error
                (e-subagent--record-progress
                 live board-id participant-id work-handle event)
              (error
               (e-subagent--remember-progress-error
                work-handle event error))))))
    (when-let* ((enroll (e-harness-work-enrollment-function parent-harness)))
      (funcall enroll work-handle nil))
    (e-work-start-prepared work-handle)
    (e-subagent-live-update live board-id participant-id
                            :work-handle work-handle)
    ;; Invoking the runner is the first point at which a provider turn may
    ;; start.  Admission has already committed.  Generic spawn retains its
    ;; historical pre-run lifecycle publication; dispatch callers defer the
    ;; durable running fact until the provider invocation returns successfully.
    (unless on-runner-started
      (let ((running (plist-put (copy-tree record) :status 'running)))
        (e-subagent--publish-lifecycle publication-target running)))
    (when on-running
      (funcall on-running (copy-tree record)))
    (funcall record-progress 'turn-started)
    (condition-case error
        (let ((handle
               (if (eq runner #'e-subagent-direct-runner)
                   (funcall runner
                            child-harness session-id prompt seed-messages
                            (lambda (status &rest args)
                              (e-subagent--settle-runner
                               record report-state work-handle status args))
                            record-progress
                            (e-subagent--assignment-input-metadata record))
                 (funcall runner
                          child-harness session-id prompt seed-messages
                          (lambda (status &rest args)
                            (when (e-subagent-live-get live board-id participant-id)
                              (funcall
                               record-progress
                               (pcase status
                                 ('done 'turn-finished)
                                 ('failed 'turn-failed)
                                 ('cancelled 'turn-cancelled))))
                            (e-subagent--settle-runner
                             record report-state work-handle status args))))))
          (when (and (listp handle) (functionp (plist-get handle :cancel)))
            (let ((cancel (plist-get handle :cancel)))
              (e-subagent-live-update live board-id participant-id
                                      :cancel cancel)
              ;; Work owns provider cancellation for both deadline and
              ;; explicit interrupt/shutdown paths.  The gate may have
              ;; latched cancellation before the provider returned its handle;
              ;; honor that request exactly once at installation time.
              (setf (e-work-handle-cancel-function work-handle)
                    (lambda (_handle) (funcall cancel)))
              (when (e-work-handle-cancel-requested-p work-handle)
                (ignore-errors (funcall cancel)))))
          ;; A dispatch consumer settles only after this provider invocation
          ;; returned successfully.  Publish its lifecycle running fact at
          ;; the same boundary; generic spawn callers retain their historical
          ;; pre-run lifecycle and `on-admitted' callbacks separately.
          (when on-runner-started
            (e-subagent--publish-lifecycle
             publication-target
             (plist-put (copy-tree record) :status 'running))
            (funcall on-runner-started (copy-tree record)))
          handle)
      (error
       (e-subagent--settle-runner
        record report-state work-handle 'failed
        (list :error (e-work-error-message error)))
       ;; `on-terminal' is invoked by the Work gate after any required Board
       ;; terminal publications have acknowledged; it must not become a second
       ;; settlement owner here.
       (ignore on-terminal)
       (list :runner-failure error)))))

(cl-defun e-subagent-spawn
    (live parent-harness parent-session-id
          &key source-turn-id type prompt seed-messages label schedule runner
          run-id task-key attempt deadline project-root report-admission
          on-admitted on-running on-failure on-runner-started
          on-terminal)
  "Spawn a subagent and return its bounded admission result.
LIVE owns only private process-local execution capabilities.  The child
participant identity is its admitted session id, and is the only identity
returned to callers.  Before durable admission settles, the result contains
the admission work reference and the durable participant/session identity.
DEADLINE, when non-nil, is one positive absolute timestamp for the child Work;
the dispatch caller owns the policy that produced it."
  (unless (stringp source-turn-id)
    (signal 'wrong-type-argument (list 'stringp :source-turn-id)))
  (unless (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
    (signal 'wrong-type-argument (list 'stringp :prompt)))
  (let* ((type (e-subagent--normalize-type type))
         (deadline (e-subagent--validate-deadline deadline))
         (instance (e-subagent--type-instance type))
         (child-harness (e-subagent--child-harness instance))
         (parent-binding
          (e-subagent--live-chat-binding parent-harness parent-session-id))
         (board-id (e-chat-service-binding-board-id parent-binding))
         (lineage-id (e-subagent--lineage-id live board-id parent-session-id))
         (assignment (and run-id
                          (list :run-id run-id :task-key task-key :attempt attempt)))
         (_ (when (or run-id task-key attempt)
              (unless (and (stringp run-id) (stringp task-key)
                           (integerp attempt) (>= attempt 0))
                (signal 'wrong-type-argument
                        (list 'e-board-orchestration-assignment assignment)))))
         (metadata (e-subagent--child-metadata
                    instance parent-harness parent-session-id lineage-id label
                    assignment project-root))
         (schedule (or schedule 'direct))
         (producer-target
          (e-subagent-publication-target parent-harness parent-session-id))
         (admission-target parent-binding)
         (child-session-id (e-session-generate-id))
         (participant-id child-session-id)
         (work-handle
          (e-work-prepare
           (e-subagent--work-spec) nil
           :context (append
                     (list :session-id parent-session-id
                           :turn-id source-turn-id
                           :work-kind 'subagent
                           :domain-ref (e-subagent-live-reference
                                        board-id participant-id))
                     (when deadline (list :deadline deadline)))))
         (record (list :board-id board-id
                       :participant-id participant-id
                       :type type
                       :role (e-harness-instance-kind instance)
                       :session-id participant-id
                       :parent-session-id parent-session-id
                       :label label
                       :schedule schedule
                       :child-harness child-harness
                       :run-id run-id
                       :task-key task-key
                       :attempt attempt
                       :report-admission report-admission
                       :status 'queued))
         ;; Keep terminal slots present so `setf' can mutate the shared plist
         ;; cell captured by the gate and report callbacks.  A missing plist
         ;; key cannot be updated through `setf (plist-get ...)', while nil is
         ;; the explicit pre-terminal sentinel.
         (report-state (list :report-admission report-admission
                             :report nil
                             :terminal-status nil
                             :terminal-args nil))
         (callbacks
          (list :record
                (lambda () (copy-tree record))
                :report
                (lambda (accepted)
                  (setq report-state
                        (plist-put report-state :report (copy-tree accepted))))
                :reported
                (lambda () (plist-get report-state :report))))
         (pending
          (e-subagent--pending-result board-id participant-id work-handle))
         admission-work admitted-result)
    (e-work-install-terminal-gate
     work-handle
     (e-subagent--terminal-gate
      live board-id participant-id record producer-target report-state
      on-terminal))
    (e-subagent-live-reserve-admission
     live board-id participant-id
     :work-handle work-handle :assignment assignment :callbacks callbacks
     :report-admission report-admission
     :parent-session-id parent-session-id :lineage-id lineage-id)
    (condition-case error
        (setq admission-work
              (e-chat-service-create-participant-start
               admission-target child-harness
               :id participant-id :participant-id participant-id
               :metadata metadata
               :pickup-selector '(:tags (subagent))
               :observer-selector :self :default-tags '(subagent)
               :default-to :self))
      (error
       (e-subagent-live-forget-admission live board-id participant-id)
       (e-work-fail work-handle error)
       (when on-failure
         (funcall on-failure error pending))
       (setq pending (plist-put pending :status 'failed))
       (setq pending (plist-put pending :error error))))
    (when admission-work
      (e-work-on-settle
       admission-work
       (lambda (settled-admission)
         ;; Admission is a fenced transaction.  Once cancellation or another
         ;; terminal path removes the reservation, every later SQL outcome is
         ;; inert: it must not touch the child Work, on-failure, publication,
         ;; or dispatch Work.  Keep this guard outside the state pcase so the
         ;; failed and cancelled races are covered just like a late success.
         (when (e-subagent-live-pending-admission
                live board-id participant-id)
           (let ((status (e-work-status settled-admission)))
             (pcase (plist-get status :state)
               ('finished
                (condition-case error
                    (progn
                      (e-subagent-live-install
                       live board-id participant-id
                       :harness child-harness :work-handle work-handle
                       :callbacks callbacks :report-admission report-admission
                       :assignment assignment)
                      (e-subagent--publish-lifecycle producer-target record)
                      (e-subagent--inherit-prompt-cache-policy
                       parent-harness parent-session-id
                       child-harness participant-id)
                      (when on-admitted
                        (funcall on-admitted (copy-tree record)))
                      (let ((drive-result
                             (e-subagent--drive-turn
                              live board-id producer-target participant-id record
                              parent-harness parent-session-id source-turn-id
                              child-harness participant-id prompt seed-messages
                              runner report-state work-handle on-running
                              on-runner-started)))
                        ;; A synchronous provider-start error already settles
                        ;; the child locally and durably.  Do not return an
                        ;; admitted result for that path; the dispatch caller's
                        ;; runner-failure callback owns its outer disposition.
                        (unless (plist-member drive-result :runner-failure)
                          (setq admitted-result (copy-tree record)))))
                  (error
                   (if (e-subagent-live-get live board-id participant-id)
                       (progn
                         (e-subagent--settle-runner
                          record report-state work-handle 'failed
                          (list :error (e-work-error-message error))))
                     (e-subagent-live-forget-admission
                      live board-id participant-id)
                     (e-work-fail work-handle error)
                     (when on-failure
                       (funcall on-failure error pending))))))
               ('failed
                (e-subagent-live-forget-admission live board-id participant-id)
                (let ((admission-error (plist-get status :error)))
                  (e-work-fail work-handle admission-error)
                  (when on-failure
                    (funcall on-failure admission-error pending))))
               ('cancelled
                ;; Fence the pending reservation before settling Work.  Any
                ;; late successful acknowledgement observes no reservation and
                ;; therefore cannot install a child or invoke a dispatch path.
                (e-subagent-live-forget-admission live board-id participant-id)
                (e-work-cancel work-handle))))))))
    (or admitted-result
        (let ((work-status (e-work-status work-handle)))
          (if (eq (plist-get work-status :state) 'failed)
              (append pending
                      (list :status 'failed
                            :error (plist-get work-status :error)))
            pending)))))

(defun e-subagent--normalize-type (value)
  "Return VALUE as a spawnable type keyword.
Actions arrive as JSON, so a type id reaches here as a string; the harness
catalog keys instances by keyword."
  (cond
   ((keywordp value) value)
   ((and (symbolp value) value) (intern (concat ":" (symbol-name value))))
   ((stringp value) (intern (concat ":" (string-remove-prefix ":" value))))
   (t (signal 'wrong-type-argument (list 'keywordp :type)))))

(defun e-subagent--layer-symbol (value)
  "Return VALUE as a layer id symbol.
Layer ids arrive from the action surface as strings."
  (cond
   ((and (symbolp value) value) value)
   ((stringp value) (intern value))
   (t (signal 'wrong-type-argument (list 'symbolp value)))))

(defun e-subagent--capability-symbol (value)
  "Return VALUE as a capability id symbol."
  (cond
   ((and (symbolp value) value) value)
   ((stringp value) (intern value))
   (t (signal 'wrong-type-argument (list 'symbolp value)))))

(defun e-subagent--capability-config-plist (value)
  "Return VALUE normalized to a capability config plist.
Action arguments arrive with string keys; intern them to keywords."
  (let (plist)
    (while value
      (let ((key (pop value))
            (val (and value (pop value))))
        (setq plist
              (plist-put plist
                         (cond
                          ((keywordp key) key)
                          ((symbolp key)
                           (intern (concat ":" (symbol-name key))))
                          ((stringp key)
                           (intern (concat ":" (string-remove-prefix ":" key))))
                          (t (signal 'wrong-type-argument (list key))))
                         val))))
    plist))

(defun e-subagent-configure-type
    (type &rest args)
  "Configure the shared harness for spawnable TYPE and return its state.
ARGS is a plist of `:enable-layers', `:disable-layers', and `:layer-config'.
ENABLE-LAYERS and DISABLE-LAYERS toggle layers on the type's shared harness, so
the parent turns individual capabilities on or off for every child of that type.
LAYER-CONFIG is an alist mapping a capability id to an option plist, applied as
that capability's runtime config; this is the generic way to pass or overwrite
layer configuration (e.g. `agents-std-context' `:skills-include').  Because
children of a type share one harness, this configures the type, not a single
child."
  (let* ((type (e-subagent--normalize-type type))
         (instance (e-subagent--type-instance type))
         (harness (e-subagent--child-harness instance))
         (enable-layers (plist-get args :enable-layers))
         (disable-layers (plist-get args :disable-layers))
         (layer-config (plist-get args :layer-config)))
    (dolist (layer (append disable-layers nil))
      (e-harness-disable-layer-id harness (e-subagent--layer-symbol layer)))
    (dolist (layer (append enable-layers nil))
      (e-harness-enable-layer-id harness (e-subagent--layer-symbol layer)))
    (dolist (entry (append layer-config nil))
      (e-harness-set-capability-config
       harness
       (e-subagent--capability-symbol (car entry))
       (e-subagent--capability-config-plist (cdr entry))))
    (list :type type
          :enabled-layers (e-harness-enabled-layer-ids harness)
          :capability-config
          (mapcar (lambda (entry)
                    (let ((id (e-subagent--capability-symbol (car entry))))
                      (cons id (e-harness-capability-config harness id))))
                  (append layer-config nil)))))

(defun e-subagent-report (live board-id session-id outputs summary &optional result)
  "Record a child-reported structured result for SESSION-ID in LIVE.
OUTPUTS is a structured artifact list; SUMMARY is a short result string, and
RESULT is optional bounded application-owned structured data.  The
report is accepted only while the live execution exists.  Durable terminal
publication happens exactly once during settlement; this function retains only
the runner-owned closure callback and never writes a terminal result inventory.
Return a detached acknowledgement, or nil when SESSION-ID is not live."
  (when-let* ((entry (e-subagent-live-find-by-session live board-id session-id)))
    (let* ((proposed (append (list :summary summary :outputs outputs)
                             (when result (list :result result))))
           (callbacks (plist-get entry :callbacks))
           (reported (plist-get callbacks :reported))
           (setter (plist-get callbacks :report))
           (record-getter (plist-get callbacks :record))
           (record (and (functionp record-getter) (funcall record-getter)))
           (admission (e-subagent-live-report-admission
                       live board-id session-id))
           (assignment (e-subagent--durable-assignment record)))
      (if (and reported (funcall reported))
          (list :participant-id session-id :session-id session-id :reported t)
        (let ((accepted
               (if (and admission (functionp admission))
                   (funcall admission
                            (append (copy-tree assignment)
                                    (list :participant-id session-id
                                          :session-id session-id
                                          :parent-session-id
                                          (plist-get record :parent-session-id)))
                            proposed)
                 proposed)))
          (unless (and (listp accepted)
                       (plist-member accepted :summary)
                       (plist-member accepted :outputs))
            (signal 'e-subagent-live-error
                    (list "Report admission returned an invalid report")))
          (unless (functionp setter)
            (signal 'e-subagent-live-error
                    (list "Live report callback is unavailable")))
          (funcall setter accepted)
          (list :participant-id session-id :session-id session-id :reported t))))))

(defun e-subagent-runner-assignment-state
    (board-id run-id task-key attempt)
  "Return detached live state for one exact Board assignment.

The result is nil when no pending or executing child in this process matches
the complete BOARD-ID/RUN-ID/TASK-KEY/ATTEMPT identity.  Otherwise it contains
only the durable coordinates, participant/session identity, and `:state' of
`pending' or `live'.  No table, callback, harness, work handle, or terminal
result crosses this consumer boundary."
  (let* ((assignment (e-subagent-runner--assignment run-id task-key attempt))
         (owner (e-subagent-runner-live-owner))
         found)
    (dolist (table (list (e-subagent-live-pending-admissions owner)
                         (e-subagent-live-entries owner)))
      (maphash
       (lambda (_key entry)
         (when (and (null found)
                    (equal (plist-get entry :board-id) board-id)
                    (equal (plist-get entry :assignment) assignment))
           (setq found
                 (list :board-id board-id
                       :participant-id (plist-get entry :participant-id)
                       :session-id (plist-get entry :participant-id)
                       :run-id run-id :task-key task-key :attempt attempt
                       :state (if (eq table
                                     (e-subagent-live-pending-admissions owner))
                                  'pending
                                'live)))))
       table))
    (copy-tree found t)))

(defun e-subagent-runner--dispatch-admitted-result
    (outer board-id run-id task-key attempt record)
  "Finish OUTER with detached admission RESULT for RECORD."
  (e-work-finish
   outer
   (list :status 'admitted
         :board-id board-id
         :participant-id (plist-get record :participant-id)
         :session-id (plist-get record :session-id)
         :run-id run-id :task-key task-key :attempt attempt)))

(defun e-subagent-runner--dispatch-publish-running
    (outer target board-id assignment run-id task-key attempt record)
  "Publish running ASSIGNMENT and finish OUTER when it is acknowledged."
  (condition-case error
      (let ((running
             (e-subagent-runner--publish-attempt target assignment 'running)))
        (e-work-on-settle
         running
         (lambda (settled)
           (let ((status (e-work-status settled)))
             (if (eq (plist-get status :state) 'finished)
                 (e-subagent-runner--dispatch-admitted-result
                  outer board-id run-id task-key attempt record)
               (e-work-fail outer (plist-get status :error)))))))
    (error (e-work-fail outer error))))

(defun e-subagent-runner--dispatch-publish-failure
    (outer target board-id assignment run-id task-key attempt failure
           &optional terminal-record)
  "Publish bounded FAILED ASSIGNMENT and settle OUTER."
  (condition-case error
      (let ((terminal
             (e-board-orchestration-actions-publish-terminal
              target assignment 'failed
              :summary (if terminal-record "" "Subagent dispatch failed")
              :outputs []
              :error (e-work-error-message failure)
              :author (and terminal-record
                           (list :session-id
                                 (plist-get terminal-record :session-id))))))
        (e-work-on-settle
         terminal
         (lambda (settled)
           (let ((status (e-work-status settled)))
             (if (eq (plist-get status :state) 'finished)
                 (e-work-finish
                  outer
                  (list :status 'failed
                        :board-id board-id
                        :run-id run-id :task-key task-key
                        :attempt attempt
                        :error (e-work-error-message failure)))
               (e-work-fail outer (plist-get status :error)))))))
    (error (e-work-fail outer error))))

(cl-defun e-subagent-runner-dispatch-start
    (target parent-harness parent-session-id
            &key source-turn-id type prompt seed-messages label schedule
            project-root run-id task-key attempt report-admission deadline)
  "Start one Board-addressed child dispatch and return cooperative WORK.

TARGET is the explicit durable Board destination.  The operation publishes an
idempotent queued disposition, admits the child through the runner's private
live owner, and publishes running only after the provider runner returns
successfully.  WORK settles
with detached Board/participant/assignment coordinates after the first
disposition is acknowledged.  Grimoire callers never receive the private live
owner, registry, callbacks, or nested work value.  DEADLINE, when non-nil, is
passed as one positive absolute timestamp to the admitted child Work."
  (let ((outer (e-work-start e-subagent-runner--dispatch-work-spec nil)))
    (condition-case error
        (let* ((deadline (e-subagent--validate-deadline deadline))
               (assignment
                (e-subagent-runner--assignment run-id task-key attempt))
               (binding (e-chat-service-binding
                         parent-harness parent-session-id))
               (board-id (and binding
                              (e-chat-service-binding-board-id binding)))
               (target-board
                (and (e-board-sqlite-publication-target-valid-p target)
                     (e-board-sqlite-publication-target-board-id target)))
               (live (e-subagent-runner-live-owner)))
          (unless (and binding board-id (equal board-id target-board))
            (signal 'e-subagent-error
                    (list "Dispatch Board target does not match owner binding"
                          target-board board-id)))
          (let* ((claim-key
                  (e-subagent-runner--dispatch-claim-key board-id assignment))
                 (existing
                  (gethash claim-key e-subagent-runner--dispatch-claims)))
            (if (e-subagent-runner--dispatch-work-active-p existing)
                (progn
                  ;; The request work was started before the exact claim was
                  ;; inspected so validation failures still settle locally.
                  ;; Retire that unused handle before returning the shared
                  ;; in-flight outcome.
                  (e-work-cancel outer)
                  (setq outer existing))
              (when existing
                (remhash claim-key e-subagent-runner--dispatch-claims))
              (puthash claim-key outer e-subagent-runner--dispatch-claims)
              (e-work-on-settle
               outer
               (lambda (settled)
                 (e-subagent-runner--release-dispatch-claim
                  claim-key settled)))
              (let ((queued (e-subagent-runner--publish-attempt
                             target assignment 'queued)))
                (e-work-on-settle
                 queued
                 (lambda (settled)
                   (let ((status (e-work-status settled)))
                     (if (not (eq (plist-get status :state) 'finished))
                         (e-work-fail outer (plist-get status :error))
                       (condition-case spawn-error
                           (e-subagent-spawn
                            live parent-harness parent-session-id
                            :source-turn-id source-turn-id :type type
                            :prompt prompt :seed-messages seed-messages
                            :label label :schedule schedule :runner nil
                            :project-root project-root :run-id run-id
                            :task-key task-key :attempt attempt
                            :deadline deadline
                            :report-admission report-admission
                            :on-admitted nil
                            :on-runner-started
                            (lambda (record)
                              (e-subagent-runner--dispatch-publish-running
                               outer target board-id assignment run-id task-key
                               attempt record))
                            :on-terminal
                            (lambda (status args persistence-error)
                              (when (e-subagent-runner--dispatch-work-active-p
                                     outer)
                                (pcase status
                                  ;; Cancellation is a durable domain outcome,
                                  ;; not cancellation of the dispatch Work
                                  ;; itself.  Keep the outer Work FINISHED so
                                  ;; callers can aggregate a bounded
                                  ;; :status cancelled result with the other
                                  ;; terminal reports.
                                  ('cancelled
                                   (e-work-finish
                                    outer
                                    (list :status 'cancelled
                                          :board-id board-id
                                          :run-id run-id
                                          :task-key task-key
                                          :attempt attempt)))
                                  ('failed
                                   (if persistence-error
                                       (e-work-fail outer persistence-error)
                                     (let ((failure (plist-get args :error)))
                                       (e-work-finish
                                        outer
                                        (list :status 'failed
                                              :board-id board-id
                                              :run-id run-id
                                              :task-key task-key
                                              :attempt attempt
                                              :error
                                              (cond
                                               ((stringp failure) failure)
                                               (failure
                                                (e-work-error-message failure))
                                               (t "Subagent dispatch failed")))))))
                                  (_ (e-work-finish outer
                                                    (list :status status
                                                          :board-id board-id
                                                          :run-id run-id
                                                          :task-key task-key
                                                          :attempt attempt)))))))
                         (error
                          (e-subagent-runner--dispatch-publish-failure
                           outer target board-id assignment run-id task-key
                           attempt spawn-error)))))))))))
      (error (e-work-fail outer error)))
    outer))

(defun e-subagent--live-record (live board-id participant-id)
  "Return detached runner context from a private record callback."
  (when-let* ((entry (or (e-subagent-live-get live board-id participant-id)
                         (e-subagent-live-pending-admission
                          live board-id participant-id)))
              (callbacks (plist-get entry :callbacks))
              (getter (plist-get callbacks :record)))
    (and (functionp getter) (funcall getter))))

(defun e-subagent--record-intervention
    (publication-target participant-id record action reason)
  "Publish one ACTION intervention through PUBLICATION-TARGET."
  (when (and reason (not (stringp reason)))
    (signal 'wrong-type-argument (list 'stringp reason)))
  (let* ((bounded-reason
          (and reason (truncate-string-to-width
                       reason e-subagent-max-intervention-reason-length nil nil "...")))
         (at (float-time)))
    (e-subagent--publish-board-fact
     publication-target
     :tags (list 'intervention action)
     :attributes (list :participant-id participant-id
                       :action action
                       :reason bounded-reason
                       :parent-session-id (plist-get record :parent-session-id)
                       :session-id participant-id)
     :content (format "Subagent %s %s%s"
                      participant-id action
                      (if bounded-reason (format ": %s" bounded-reason) ""))
     :source-fact-key
     (list 'subagent-intervention participant-id action at))
    record))

(defun e-subagent--cancel-and-retire
    (live board-id publication-target participant-id action reason)
  "Cancel PARTICIPANT-ID and retire all live coordination.
PUBLICATION-TARGET belongs to the caller's current action or presentation
context.  Cancellation and local retirement run even when audit publication
fails."
  (let* ((entry (e-subagent-live-get live board-id participant-id))
         (pending (and (null entry)
                       (e-subagent-live-pending-admission
                        live board-id participant-id)))
         (record (e-subagent--live-record live board-id participant-id))
         (work-handle (and (or entry pending)
                           (plist-get (or entry pending) :work-handle)))
         snapshot)
    (unless record
      (user-error "Subagent %s is unavailable in this process" participant-id))
    (unwind-protect
        (setq snapshot
              (e-subagent--record-intervention
               publication-target participant-id record action reason))
      ;; Work is the only settlement owner.  Fence pending admission before
      ;; cancellation, then let its terminal gate publish/retire the live
      ;; child after Board acknowledgement.
      (when pending
        (e-subagent-live-forget-admission live board-id participant-id))
      (when work-handle
        (e-work-cancel work-handle)
        (setq snapshot (plist-put (copy-tree record) :status 'cancelled))))
    snapshot))

(defun e-subagent-interrupt
    (live board-id publication-target participant-id &optional reason)
  "Abort PARTICIPANT-ID's active child turn and retire live coordination."
  (e-subagent--cancel-and-retire
   live board-id publication-target participant-id 'interrupt reason))

(defun e-subagent-shutdown
    (live board-id publication-target participant-id &optional reason)
  "Interrupt PARTICIPANT-ID deliberately and retire live coordination."
  (e-subagent--cancel-and-retire
   live board-id publication-target participant-id 'shutdown reason))

(defun e-subagent-steer
    (live board-id publication-target participant-id prompt &optional reason)
  "Steer PARTICIPANT-ID's running child turn with PROMPT."
  (let* ((entry (e-subagent-live-get live board-id participant-id))
         (record (e-subagent--live-record live board-id participant-id))
         (harness (and entry (plist-get entry :harness))))
    (unless (and record harness)
      (user-error "Subagent %s has no live child harness" participant-id))
    (e-chat-service-steer-session harness participant-id prompt)
    (e-subagent--record-intervention
     publication-target participant-id record 'steer reason)))

(defun e-subagent-send (live board-id participant-id prompt)
  "Queue a follow-up PROMPT to PARTICIPANT-ID's child session."
  (let* ((entry (e-subagent-live-get live board-id participant-id))
         (harness (and entry (plist-get entry :harness))))
    (unless harness
      (user-error "Subagent %s is not executing" participant-id))
    (e-chat-service-queue-session harness participant-id prompt)
    (e-subagent--live-record live board-id participant-id)))

(provide 'e-subagent-runner)

;;; e-subagent-runner.el ends here
