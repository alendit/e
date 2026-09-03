;;; e-chat-service.el --- Board-backed chat application service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shell-neutral board client for chat presentation shells.  The harness owns
;; the private transcript and turn endpoint; callers submit and observe only
;; through the board binding created here.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-board-registry)
(require 'e-board-runtime)
(require 'e-board-session-association)
(require 'e-board-orchestration)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-session)
(require 'e-session-board-policy)
(require 'subr-x)

(defvar e-chat-default-harness-id)

(defgroup e-chat-service nil
  "Shell-neutral chat service operations."
  :group 'e)

(define-error 'e-chat-service-invalid-activity
  "e chat service activity has invalid public attributes")

(defcustom e-chat-service-default-harness-id :chat-default
  "Harness registry id used by shell-neutral default chat commands."
  :type 'symbol
  :group 'e-chat-service)

(defconst e-chat-service-observer-page-limit 16
  "Maximum board messages translated during one chat observer drain.")

(defconst e-chat-service-subscriber-limit 8
  "Maximum presentation subscribers admitted to one chat binding.")

(defconst e-chat-service-projection-capacity 256
  "Maximum immutable board events retained per chat projection category.")

(defcustom e-chat-service-idle-close-delay 300
  "Seconds without a presentation client before an idle board closes."
  :type 'number
  :group 'e-chat-service)

(defconst e-chat-service--curation-activity-keys
  '(:kept-source-count :summary-count :summarized-source-count
    :erased-source-count)
  "Exact count fields accepted by the shell-family curation formatter.")

(defun e-chat-service--curation-counts (projection)
  "Return validated curation counts from public PROJECTION."
  (unless (and (proper-list-p projection) (= (length projection) 8))
    (signal 'e-chat-service-invalid-activity
            (list 'context-curated :shape projection)))
  (let ((tail projection)
        keys)
    (while tail
      (let ((key (pop tail)))
        (unless (and (keywordp key) tail)
          (signal 'e-chat-service-invalid-activity
                  (list 'context-curated :shape projection)))
        (push key keys)
        (pop tail)))
    (unless (and (= (length keys) (length (delete-dups (copy-sequence keys))))
                 (cl-every (lambda (key)
                             (memq key e-chat-service--curation-activity-keys))
                           keys)
                 (cl-every (lambda (key) (plist-member projection key))
                           e-chat-service--curation-activity-keys))
      (signal 'e-chat-service-invalid-activity
              (list 'context-curated :keys (nreverse keys))))
    (let ((kept (plist-get projection :kept-source-count))
          (summaries (plist-get projection :summary-count))
          (summarized (plist-get projection :summarized-source-count))
          (erased (plist-get projection :erased-source-count)))
      (unless (and (cl-every (lambda (value)
                               (and (integerp value) (>= value 0)))
                             (list kept summaries summarized erased))
                   (or (> kept 0) (> summarized 0) (> erased 0))
                   (eq (= summaries 0) (= summarized 0))
                   (<= summaries summarized))
        (signal 'e-chat-service-invalid-activity
                (list 'context-curated :counts projection)))
      (list kept summaries summarized erased))))

(defun e-chat-service-format-context-curation (projection)
  "Format count-only public context-curation PROJECTION for chat shells."
  (pcase-let ((`(,kept ,summaries ,summarized ,erased)
               (e-chat-service--curation-counts projection)))
    (string-join
     (delq nil
           (list
            (and (> kept 0) (format "kept %d" kept))
            (and (> summarized 0)
                 (format "summarized %d source%s into %d summar%s"
                         summarized (if (= summarized 1) "" "s")
                         summaries (if (= summaries 1) "y" "ies")))
            (and (> erased 0) (format "erased %d" erased))))
     " · ")))

(cl-defstruct (e-chat-service-binding
               (:constructor e-chat-service--binding-create))
  harness session-id board client requester attachment observer subscribers
  observer-drain-scheduled pending-input-head pending-input-tail turn-map
  input-sequence default-tags default-to idle-close-timer
  message-projection activity-projection lifecycle-generation
  observer-drain-timer lifecycle-state)

(cl-defstruct (e-chat-service-projection
               (:constructor e-chat-service--projection-create))
  ring head count seen)

(cl-defstruct (e-chat-service-subscription
               (:constructor e-chat-service--subscription-create))
  binding function active-p client observer drain-scheduled state drain-timer
  lifecycle-generation)

(cl-defstruct (e-chat-service-view
               (:constructor e-chat-service--view-create))
  cursor messages activity-events subscription)

(defvar e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key)
  "Board-backed chat bindings, first by harness identity then session id.")

(defvar e-chat-service--board-bindings (make-hash-table :test 'equal)
  "Live chat bindings sharing each registered board identity.")

(defvar e-chat-service--continuation-reconciling (make-hash-table :test 'equal)
  "Boards whose terminal continuation is being reconciled synchronously.")

(defun e-chat-service--publish-continuation-claim
    (board run-id publication-key status &optional error)
  "Publish RUN-ID's continuation STATUS with its stable PUBLICATION-KEY."
  (e-board-orchestration-publish-fact
   board
   (list :version e-board-orchestration-fact-version
         :type 'continuation-claim
         :idempotency-key
         (e-board-orchestration-continuation-claim-key publication-key status)
         :payload (append (list :run-id run-id :publication-key publication-key
                                :status status)
                          (when error (list :error error))))))

(defun e-chat-service-reconcile-board-continuation (board harness)
  "Publish each terminal continuation on BOARD exactly once.
The input key lives in the durable manifest and is reused after a crash between
input publication and the acknowledgement fact.  HARNESS is the application
service used for the queued continuation; it may be nil for a caller that
only observes durable claim decisions."
  (let* ((board (e-board-registry-board-source-board board))
         (board-id (e-board-id board)))
    (unless (gethash board-id e-chat-service--continuation-reconciling)
      (puthash board-id t e-chat-service--continuation-reconciling)
      (unwind-protect
          (dolist (run-id (e-board-orchestration-run-ids board))
            (condition-case err
                (let* ((projection (e-board-orchestration-run-projection board run-id))
                       (continuation (plist-get projection :continuation)))
                  (when (and (plist-get projection :terminal-status)
                             continuation
                             (not (eq (plist-get continuation :state) 'published)))
                    (let ((key (plist-get continuation :publication-key)))
                      (e-chat-service--publish-continuation-claim
                       board run-id key 'pending)
                      (e-chat-service-queue-session
                       harness
                       (plist-get continuation :session-id)
                       (plist-get continuation :prompt)
                       :metadata (list :board-run-id run-id
                                       :board-continuation-key key)
                       :source-input-key (list "orchestration-continuation" key 0))
                      (e-chat-service--publish-continuation-claim
                       board run-id key 'published))))
              (e-board-orchestration-invalid-fact nil)
              (error
               (when-let* ((projection (ignore-errors
                                         (e-board-orchestration-run-projection board run-id)))
                           (continuation (plist-get projection :continuation)))
                 (e-chat-service--publish-continuation-claim
                  board run-id (plist-get continuation :publication-key) 'failed err)))))
        (remhash board-id e-chat-service--continuation-reconciling)))))

(defun e-chat-service--harness-bindings (harness)
  "Return the session binding table owned by HARNESS."
  (or (gethash harness e-chat-service--bindings)
      (puthash harness (make-hash-table :test 'equal)
               e-chat-service--bindings)))

(defun e-chat-service--binding-live-p (binding)
  "Return non-nil when BINDING still names its active registered board."
  (let ((board (and (e-chat-service-binding-p binding)
                    (e-chat-service-binding-board binding))))
    (and board
         (not (memq (e-chat-service-binding-lifecycle-state binding)
                    '(retiring retired)))
         (eq (e-board-registry-board-state board) 'active)
         (condition-case nil
             (eq board
                 (e-board-registry-get
                  (e-board-registry-board-id board)))
           (e-board-registry-missing nil)))))

(defun e-chat-service--observer-drain-live-p (binding client observer)
  "Return non-nil while CLIENT and OBSERVER can receive a page.
This is the service's bounded receiver lease check.  It keeps a stale client,
cancelled observer, revoked principal, closing board, or replaced observer
from being treated as a live page pump merely because its old cursor is behind
the board tail."
  (let* ((board (and (e-chat-service-binding-p binding)
                     (e-chat-service-binding-board binding)))
         (source (and board (e-board-registry-board-source-board board)))
         (client-id (and (e-board-registry-client-p client)
                         (e-board-registry-client-id client)))
         (observer-id (and (e-board-observer-p observer)
                           (e-board-observer-id observer)))
         (current-client (and board client-id
                              (gethash client-id
                                       (e-board-registry-board-clients board))))
         (current-observer (and source observer-id
                                (e-board-observer source observer-id))))
    (and (e-chat-service--binding-live-p binding)
         (eq current-client client)
         (eq (e-board-registry-client-state client) 'active)
         (or (null (e-board-registry-client-principal client))
             (eq (e-board-registry-principal-role
                  board (e-board-registry-client-principal client))
                 (e-board-registry-client-role client)))
         (eq current-observer observer)
         (eq (e-board-observer-state observer) 'active)
         (equal (e-board-observer-client-id observer) client-id)
         (= (or (e-board-observer-client-generation observer) 0)
            (or (e-board-registry-client-generation client) 0))
         (memq observer-id (e-board-registry-client-observer-ids client)))))

(defun e-chat-service--observer-drain-pending-p (binding observer)
  "Return non-nil when OBSERVER's current board has an unaccepted page."
  (let ((board (e-chat-service-binding-board binding)))
    (< (e-board-observer-next-index observer)
       (e-board-message-count
        (e-board-registry-board-source-board board)))))

(defun e-chat-service--observer-drain-retirement-state
    (binding client observer)
  "Return the terminal state for an ineligible observer receiver."
  (let* ((board (and (e-chat-service-binding-p binding)
                     (e-chat-service-binding-board binding)))
         (source (and board (e-board-registry-board-source-board board)))
         (client-id (and (e-board-registry-client-p client)
                         (e-board-registry-client-id client)))
         (observer-id (and (e-board-observer-p observer)
                           (e-board-observer-id observer)))
         (current-client (and board client-id
                              (gethash client-id
                                       (e-board-registry-board-clients board))))
         (current-observer (and source observer-id
                                (e-board-observer source observer-id))))
    (cond
     ((not (e-chat-service--binding-live-p binding))
      (list 'detached nil))
     ((not (eq current-client client))
      (list 'detached nil))
     ((not (eq current-observer observer))
      (list 'stale nil))
     ((not (eq (e-board-observer-state observer) 'active))
      (list (e-board-observer-state observer) nil))
     (t
      (list 'detached nil)))))

(defun e-chat-service--main-observer-repairable-p
    (binding client observer)
  "Return non-nil when BINDING's stale main receiver can be rehabilitated.
An active current client needs only a replacement observer.  A detached client
may be replaced together with its observer while the old principal remains
authorized.  An expired cursor is replaced at the retained board boundary;
revoked or externally replaced leases are terminal instead of being silently
reattached through a different authorization context."
  (let* ((board (and (e-chat-service-binding-p binding)
                     (e-chat-service-binding-board binding)))
         (source (and board (e-board-registry-board-source-board board)))
         (client-id (and (e-board-registry-client-p client)
                         (e-board-registry-client-id client)))
         (observer-id (and (e-board-observer-p observer)
                           (e-board-observer-id observer)))
         (current-client (and board client-id
                              (gethash client-id
                                       (e-board-registry-board-clients board))))
         (current-observer (and source observer-id
                                (e-board-observer source observer-id))))
    (and (e-chat-service--binding-live-p binding)
         (or (null current-client) (eq current-client client))
         (or (null (e-board-registry-client-principal client))
             (eq (e-board-registry-principal-role
                  board (e-board-registry-client-principal client))
                 (e-board-registry-client-role client)))
         (eq current-observer observer)
         (equal (e-board-observer-client-id observer) client-id)
         (= (or (e-board-observer-client-generation observer) 0)
            (or (e-board-registry-client-generation client) 0))
         (memq (e-board-observer-state observer)
               '(cancelled faulted expired)))))

(defun e-chat-service--repair-main-observer (binding client observer)
  "Rehabilitate BINDING's stale main receiver while retaining its board lease.
The old cursor's next sequence is retained so a cancelled receiver does not
skip messages that were pending before rehabilitation.  An expired cursor is
clamped to the board's retained boundary.  When the old client is still
current, only its observer is replaced; when it was detached, the service
installs a fresh authorized client and observer before releasing the old
client.  Participant attachment and durable board association remain
unchanged."
  (let* ((board (e-chat-service-binding-board binding))
         (source (e-board-registry-board-source-board board))
         (client-id (e-board-registry-client-id client))
         (observer-id (e-board-observer-id observer))
         (start-seq
          (max (e-board-observer-next-seq observer)
               (1- (e-board-retention-floor source))))
         (current-client (gethash client-id
                                  (e-board-registry-board-clients board)))
         (replacement-client nil)
         (replacement-requester nil)
         replacement)
    (if (and (eq current-client client)
             (eq (e-board-registry-client-state client) 'active))
        (setq replacement
              (e-board-registry-replace-observer
               board client-id observer-id
               (copy-tree (e-board-observer-selector observer))
               :start-seq start-seq))
      (unwind-protect
          (progn
            (setq replacement-client
                  (e-board-registry-attach-client
                   board
                   :author (e-board-registry-client-author client)
                   :principal (e-board-registry-client-principal client)))
            (setq replacement-requester
                  (e-board-registry-client-requester-context
                   board
                   (e-board-registry-client-id replacement-client)))
            (setq replacement
                  (e-board-registry-install-observer
                   board
                   (e-board-registry-client-id replacement-client)
                   (copy-tree (e-board-observer-selector observer))
                   :start-seq start-seq))
            (setf (e-chat-service-binding-client binding) replacement-client
                  (e-chat-service-binding-requester binding)
                  replacement-requester)
            (setq replacement-client nil)
            (e-board-registry-detach-client-exact board client))
        (when replacement-client
          (e-board-registry-detach-client-exact board replacement-client))))
    (setf (e-chat-service-binding-observer binding) replacement
          (e-chat-service-binding-observer-drain-scheduled binding) nil)
    ;; A cancelled observer may have been behind a board tail when its timer
    ;; fired.  Keep eventual delivery alive after the lease replacement; the
    ;; public bounded drain still returns nil for the repair step itself.
    (when (e-chat-service--observer-drain-pending-p binding replacement)
      (e-chat-service--schedule-observer-drain binding))
    replacement))

(defun e-chat-service--retire-binding (binding)
  "Retire BINDING from every process-local catalog and release its leases.
Durable session and board association remain intact; a later explicit ensure
may restore them.  Binding retirement is terminal for every presentation
client, so independent subscribers are released here instead of being left as
orphaned board-registry clients."
  (when (e-chat-service-binding-p binding)
    (let* ((harness (e-chat-service-binding-harness binding))
           (session-id (e-chat-service-binding-session-id binding))
           (bindings (and harness (gethash harness e-chat-service--bindings)))
           (board (e-chat-service-binding-board binding))
           (board-id (and board (e-board-registry-board-id board)))
           (already-retired
            (eq (e-chat-service-binding-lifecycle-state binding) 'retired)))
      ;; Invalidate every queued service callback before releasing any owned
      ;; lease.  The generation remains on the object so callbacks retained by
      ;; an embedding shell can prove they are stale without consulting a
      ;; process-global registry.
      (unless already-retired
        ;; Keep the binding discoverable as a non-live retry record until the
        ;; lower runtime owner has completed its exact attachment teardown.
        ;; This prevents a failed first attempt from letting ensure create a
        ;; replacement while the old participant/maps still hold authority.
        (setf (e-chat-service-binding-lifecycle-state binding) 'retiring)
        (cl-incf (e-chat-service-binding-lifecycle-generation binding)))
      (when-let ((timer (e-chat-service-binding-observer-drain-timer binding)))
        (when (timerp timer) (cancel-timer timer))
        (setf (e-chat-service-binding-observer-drain-timer binding) nil))
      (setf (e-chat-service-binding-observer-drain-scheduled binding) nil)
      (dolist (subscription (copy-sequence
                             (e-chat-service-binding-subscribers binding)))
        ;; Do not call `e-chat-service--retire-subscription' here: its normal
        ;; last-subscriber path schedules an idle close, while this binding is
        ;; already undergoing terminal teardown.
        (setf (e-chat-service-subscription-active-p subscription) nil
              (e-chat-service-subscription-drain-scheduled subscription) nil
              (e-chat-service-subscription-state subscription)
              (list 'detached nil))
        (cl-incf (e-chat-service-subscription-lifecycle-generation
                  subscription))
        (when-let ((timer (e-chat-service-subscription-drain-timer
                           subscription)))
          (when (timerp timer) (cancel-timer timer))
          (setf (e-chat-service-subscription-drain-timer subscription) nil))
        (when-let ((client (e-chat-service-subscription-client subscription)))
          (e-board-registry-detach-client-exact board client)))
      (setf (e-chat-service-binding-subscribers binding) nil)
      (when-let ((timer (e-chat-service-binding-idle-close-timer binding)))
        (when (timerp timer) (cancel-timer timer))
        (setf (e-chat-service-binding-idle-close-timer binding) nil))
      ;; The runtime owner, not this service, owns attachment/session/endpoint
      ;; catalogs, participant routes, and ordinary board subscriptions.
      ;; Retain the attachment object so a repeated call is idempotent.
      (when-let ((attachment (e-chat-service-binding-attachment binding)))
        (e-board-runtime-retire-attachment attachment))
      ;; The lower owner succeeded; only now make terminal service state
      ;; visible and remove this binding from lookup catalogs.
      (setf (e-chat-service-binding-lifecycle-state binding) 'retired)
      (when (and bindings (eq (gethash session-id bindings) binding))
        (remhash session-id bindings)
        (when (= (hash-table-count bindings) 0)
          (remhash harness e-chat-service--bindings)))
      (when board-id
        (let ((remaining (delq binding
                              (gethash board-id
                                       e-chat-service--board-bindings))))
          (if remaining
              (puthash board-id remaining e-chat-service--board-bindings)
            (remhash board-id e-chat-service--board-bindings)
            (e-board-session-association-release
             (e-board-registry-board-source-board board))))
        (when-let ((client (e-chat-service-binding-client binding)))
          (e-board-registry-detach-client-exact board client)))
      binding)))

(defun e-chat-service--discard-binding (binding)
  "Discard an unpublished BINDING and all of its owned runtime state.
This is the application-service admission inverse, not ordinary participant
removal: it emits no board removal event and removes the binding from every
  process-local catalog before releasing its client and attachment."
  (when (e-chat-service-binding-p binding)
    (let* ((harness (e-chat-service-binding-harness binding))
           (session-id (e-chat-service-binding-session-id binding))
           (board (e-chat-service-binding-board binding))
           (board-id (e-board-registry-board-id board))
           (bindings (e-chat-service--harness-bindings harness))
           (attachment (e-chat-service-binding-attachment binding))
           (client (e-chat-service-binding-client binding)))
      ;; Admission callbacks may already be queued even though this binding
      ;; was never published.  Retire the generation before releasing any
      ;; partially admitted lease so those callbacks cannot run the normal
      ;; repair path against an unpublished object.
      (unless (eq (e-chat-service-binding-lifecycle-state binding) 'retired)
        (setf (e-chat-service-binding-lifecycle-state binding) 'retired)
        (cl-incf (e-chat-service-binding-lifecycle-generation binding)))
      (dolist (subscription (e-chat-service-binding-subscribers binding))
        (setf (e-chat-service-subscription-active-p subscription) nil)
        (cl-incf (e-chat-service-subscription-lifecycle-generation
                  subscription))
        (when-let ((timer (e-chat-service-subscription-drain-timer
                           subscription)))
          (when (timerp timer) (cancel-timer timer))
          (setf (e-chat-service-subscription-drain-timer subscription) nil))
        (when-let ((subscription-client
                    (e-chat-service-subscription-client subscription)))
          (e-board-registry-detach-client-exact board subscription-client)))
      (setf (e-chat-service-binding-subscribers binding) nil)
      (when-let ((timer (e-chat-service-binding-observer-drain-timer binding)))
        (when (timerp timer) (cancel-timer timer))
        (setf (e-chat-service-binding-observer-drain-timer binding) nil))
      (when-let ((timer (e-chat-service-binding-idle-close-timer binding)))
        (when (timerp timer) (cancel-timer timer))
        (setf (e-chat-service-binding-idle-close-timer binding) nil))
      (setf (e-chat-service-binding-observer-drain-scheduled binding) nil)
      (when (eq (gethash session-id bindings) binding)
        (remhash session-id bindings)
        (when (= (hash-table-count bindings) 0)
          (remhash harness e-chat-service--bindings)))
      (let ((remaining (delq binding
                            (gethash board-id e-chat-service--board-bindings))))
        (if remaining
            (puthash board-id remaining e-chat-service--board-bindings)
          (remhash board-id e-chat-service--board-bindings)
          (e-board-session-association-release
           (e-board-registry-board-source-board board))))
      (when attachment
        (e-board-runtime-abort-new-attachment attachment))
      (when client
        (e-board-registry-detach-client-exact board client))
      t)))

(defun e-chat-service-binding (harness session-id)
  "Return HARNESS SESSION-ID's live chat board binding, or nil."
  (when-let ((bindings (gethash harness e-chat-service--bindings)))
    (when-let ((binding (gethash session-id bindings)))
      (if (e-chat-service--binding-live-p binding)
          binding
        (e-chat-service--retire-binding binding)
        (remhash session-id bindings)
        nil))))

(defun e-chat-service-board-session-p (harness session-id)
  "Return non-nil when HARNESS SESSION-ID has durable board identity."
  (condition-case nil
      (let* ((session (e-session-get (e-harness-sessions harness) session-id))
             (state (plist-get session :board-session-state)))
        (and (stringp (plist-get state :board-id))
             (plist-get state :principal)))
    (e-session-missing nil)))

(defun e-chat-service--board-has-active-subscriber-p (board-id)
  "Return non-nil when BOARD-ID has any live presentation subscriber."
  (cl-some
   (lambda (binding)
     (cl-some #'e-chat-service-subscription-active-p
              (e-chat-service-binding-subscribers binding)))
   (gethash board-id e-chat-service--board-bindings)))

(defun e-chat-service--cancel-idle-close (binding)
  "Cancel BINDING's pending idle close, if any."
  (when-let ((timer (e-chat-service-binding-idle-close-timer binding)))
    (when (timerp timer) (cancel-timer timer))
    (setf (e-chat-service-binding-idle-close-timer binding) nil)))

(defun e-chat-service-close-board (binding)
  "Retire all process-local clients and begin or reuse close of BINDING's board.
The service owns presentation teardown; the registry owns the asynchronous
board close.  Repeated calls while closing return the existing lifecycle
request, while a completed close is already terminal and returns nil."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (let* ((board (e-chat-service-binding-board binding))
         (board-id (e-board-registry-board-id board)))
    (dolist (current (copy-sequence
                      (gethash board-id e-chat-service--board-bindings)))
      (e-chat-service--retire-binding current))
    (remhash board-id e-chat-service--board-bindings)
    (pcase (e-board-registry-board-state board)
      ('active (e-board-registry-close board))
      ('closing
       (when-let ((operation
                   (e-board-registry-board-close-operation board)))
         (e-board-registry-close-operation-request operation)))
      ('closed nil))))

(defun e-chat-service-reset-session (harness session-id)
  "Reset SESSION-ID's transcript and bounded board presentation projection."
  (let* ((store (e-harness-sessions harness))
         (binding (e-chat-service-ensure-binding harness session-id))
         (board-id (e-board-registry-board-id
                    (e-chat-service-binding-board binding))))
    (e-harness-reset harness session-id)
    (e-board-session-association-reset-projection store session-id)
    (dolist (current (gethash board-id e-chat-service--board-bindings))
      (dolist (projection
               (list (e-chat-service-binding-message-projection current)
                     (e-chat-service-binding-activity-projection current)))
        (fillarray (e-chat-service-projection-ring projection) nil)
        (clrhash (e-chat-service-projection-seen projection))
        (setf (e-chat-service-projection-head projection) 0
              (e-chat-service-projection-count projection) 0))
      (clrhash (e-chat-service-binding-turn-map current))
      (setf (e-chat-service-binding-pending-input-head current) nil
            (e-chat-service-binding-pending-input-tail current) nil))
    binding))

(defun e-chat-service--schedule-idle-close (binding)
  "Schedule registry-owned board cleanup after BINDING becomes idle."
  (e-chat-service--cancel-idle-close binding)
  (let ((generation
         (or (e-chat-service-binding-lifecycle-generation binding) 0)))
    (setf (e-chat-service-binding-idle-close-timer binding)
          (run-at-time
           (max 0 e-chat-service-idle-close-delay) nil
           (lambda ()
             (when (= generation
                      (or (e-chat-service-binding-lifecycle-generation binding)
                          0))
               (setf (e-chat-service-binding-idle-close-timer binding) nil)
               (let* ((board (e-chat-service-binding-board binding))
                      (board-id (e-board-registry-board-id board)))
                 (if (not (e-chat-service--binding-live-p binding))
                     ;; An external close can win the race with this timer;
                     ;; run the same exact terminal cleanup instead of
                     ;; leaving a stale service catalog behind.
                     (e-chat-service--retire-binding binding)
                   (when (not (e-chat-service--board-has-active-subscriber-p
                               board-id))
                     (e-chat-service-close-board binding))))))))))

(defun e-chat-service--enqueue-pending-input (binding message-id)
  "Append MESSAGE-ID to BINDING's uncorrelated input FIFO."
  (let ((cell (list message-id)))
    (if-let ((tail (e-chat-service-binding-pending-input-tail binding)))
        (setcdr tail cell)
      (setf (e-chat-service-binding-pending-input-head binding) cell))
    (setf (e-chat-service-binding-pending-input-tail binding) cell)))

(defun e-chat-service--turn-id (binding subject-participant-id source-turn-id)
  "Return BINDING's presentation id for one participant SOURCE-TURN-ID."
  (or (and source-turn-id
           (gethash (list subject-participant-id source-turn-id)
                    (e-chat-service-binding-turn-map binding)))
      (and source-turn-id
           (list (e-board-registry-board-id
                  (e-chat-service-binding-board binding))
                 subject-participant-id source-turn-id))))

(defun e-chat-service--causal-input-id (binding message)
  "Return the board input id that causally owns MESSAGE, if retained."
  (let ((source (e-board-registry-board-source-board
                 (e-chat-service-binding-board binding))))
    (or
     (when-let* ((delivery-id
                  (car (e-board-message-caused-by-delivery-ids message)))
                 (pickup (e-board-pickup source delivery-id)))
       (e-board-pickup-message-id pickup))
     (when-let* ((reply-id (car (e-board-message-reply-to-message-ids message)))
                 (reply (e-board-message source reply-id)))
       (and (eq (e-board-message-kind reply) 'input) reply-id)))))

(defun e-chat-service--binding-participant-id (binding)
  "Return the participant identity attached to BINDING, or nil.
The attachment is the process-local owner of a binding; board message subjects
are compared with it rather than inferred from tags, authors, or causal ids."
  (when-let* ((attachment (e-chat-service-binding-attachment binding))
              (participant (e-board-runtime-attachment-participant attachment)))
    (e-board-registry-participant-id participant)))

(defun e-chat-service--selected-participant-p (binding subject-participant-id)
  "Return non-nil when SUBJECT-PARTICIPANT-ID owns BINDING's attachment."
  (and subject-participant-id
       (equal subject-participant-id
              (e-chat-service--binding-participant-id binding))))

(defun e-chat-service--event-selected-participant-p (event)
  "Return whether EVENT is owned by the attached participant.

Synthetic events without board identity retain the historical direct-service
behavior.  A board-shaped event with a missing ownership fact is fail-closed,
so a sibling cannot settle a selected binding through a malformed projection."
  (if (plist-member event :selected-participant-p)
      (eq (plist-get event :selected-participant-p) t)
    (not (or (plist-member event :board-id)
             (plist-member event :board-seq)
             (plist-member event :message-id)
             (plist-member event :subject-participant-id)))))

(defun e-chat-service--message-event (binding message)
  "Translate one immutable board MESSAGE for BINDING's existing reducers."
  (let* ((kind (e-board-message-kind message))
         (source-turn-id (e-board-message-source-turn-id message))
         (subject-participant-id
          (e-board-message-subject-participant-id message))
         (causal-input-id (e-chat-service--causal-input-id binding message))
         (turn-id (or causal-input-id
                      (e-chat-service--turn-id
                       binding subject-participant-id source-turn-id)))
         (session-id (e-chat-service-binding-session-id binding))
         (identity
          (list :board-id (e-board-message-board-id message)
                :board-seq (e-board-message-seq message)
                :created-at (e-board-message-created-at message)
                :message-id (e-board-message-id message)
                :board-kind kind
                :tags (copy-tree (e-board-message-tags message))
                :attributes (copy-tree (e-board-message-attributes message))
                :to (e-board-message-to message)
                :mode (e-board-message-mode message)
                :reference (copy-tree (e-board-message-reference message))
                :subject-participant-id
                subject-participant-id
                :selected-participant-p
                ;; Ordinary user input has no participant subject, but it is
                ;; the selected conversation turn.  Terminal/output ownership
                ;; remains subject-based below.
                (or (eq kind 'input)
                    (e-chat-service--selected-participant-p
                     binding subject-participant-id))
                :source-turn-id source-turn-id
                :caused-by-delivery-ids
                (copy-tree (e-board-message-caused-by-delivery-ids message)))))
    (pcase kind
      ('input
       (let ((message-id (e-board-message-id message)))
         (append identity
                 (list :type 'message-added :session-id session-id
               :turn-id message-id
               :payload
               (list :message
                     (list :id message-id :role 'user
                           :content (e-board-message-content message)
                           :turn-id message-id
                           :created-at (e-board-message-created-at message)
                           :references (e-board-message-reference message)
                           :metadata
                           (copy-tree (e-board-message-attributes message))
                           :board-id (e-board-message-board-id message)
                           :board-seq (e-board-message-seq message)
                           :subject-participant-id
                           (e-board-message-subject-participant-id message)
                           :selected-participant-p
                           ;; An input has no participant subject.  It is the
                           ;; chat's own user turn for replay grouping; terminal
                           ;; ownership is still derived only from output and
                           ;; activity subjects below.
                           t))))))
      ('output
       (append identity
               (list :type 'message-added :session-id session-id
                     :turn-id turn-id
                     :payload
                   (list :message
                         (list :id (e-board-message-id message) :role 'assistant
                               :content (e-board-message-content message)
                               ;; Runtime output is published only while
                               ;; handling `turn-finished'.  Preserve that
                               ;; terminal witness across the shell boundary
                               ;; even when the separate turn-summary row
                               ;; arrives on a later board page.
                               :terminal-output t
                               :turn-id turn-id
                               :created-at (e-board-message-created-at message)
                               :metadata
                               (copy-tree (e-board-message-attributes message))
                               :board-id (e-board-message-board-id message)
                               :board-seq (e-board-message-seq message)
                               :subject-participant-id
                               (e-board-message-subject-participant-id message)
                               ;; Replay uses the nested message projection
                               ;; for observed-row identity.  Preserve the
                               ;; durable source turn there as well as on the
                               ;; outer board event so one sibling does not
                               ;; split into an output record plus activity
                               ;; records after reopen.
                               :source-turn-id
                               (e-board-message-source-turn-id message)
                               :selected-participant-p
                               (plist-get identity :selected-participant-p))))))
      ('activity
       (let ((activity-kind (e-board-message-activity-kind message)))
         (when (and source-turn-id causal-input-id)
           (puthash (list subject-participant-id source-turn-id)
                    causal-input-id
                    (e-chat-service-binding-turn-map binding)))
         ;; Board publishes a detailed terminal activity and a separate
         ;; aggregate turn-summary row.  Preserve those distinct meanings:
         ;; translating the summary into another terminal event renders a
         ;; duplicate failure/cancellation notice in presentation shells.
         (when activity-kind
           (append identity
                   (list :type activity-kind :session-id session-id
                 :turn-id (e-chat-service--turn-id
                           binding subject-participant-id source-turn-id)
                 :payload
                 (append
                  (when-let ((content (e-board-message-content message)))
                    (list :content content))
                  ;; Reasoning reaches the board through a bounded
                  ;; latest-value mailbox.  Its content is therefore a
                  ;; presentation snapshot, not one raw provider fragment to
                  ;; concatenate with the preceding board publication.
                  (when (memq activity-kind
                              '(reasoning-delta reasoning-raw-delta))
                    (list :content-mode 'snapshot))
                  (copy-tree (e-board-message-attributes message))))))))
      (_
       (append identity
               (list :type 'board-fact :session-id session-id
             :turn-id turn-id
             :payload (list :message-id (e-board-message-id message)
                            :content (e-board-message-content message))))))))

(defun e-chat-service--make-projection ()
  "Return one empty fixed-capacity presentation projection."
  (e-chat-service--projection-create
   :ring (make-vector e-chat-service-projection-capacity nil)
   :head 0 :count 0 :seen (make-hash-table :test 'equal)))

(defun e-chat-service--event-projection (binding event)
  "Return BINDING projection that owns EVENT, or nil for non-presentation facts."
  (pcase (plist-get event :type)
    ('message-added (e-chat-service-binding-message-projection binding))
    ('board-fact nil)
    (_ (e-chat-service-binding-activity-projection binding))))

(defun e-chat-service--projection-record (binding event)
  "Retain immutable board EVENT in BINDING's category-specific fixed ring."
  (when-let ((projection (and event
                              (e-chat-service--event-projection binding event))))
    (let* ((message-id (plist-get event :message-id))
           (seen (e-chat-service-projection-seen projection)))
      (unless (gethash message-id seen)
        (let* ((ring (e-chat-service-projection-ring projection))
               (head (e-chat-service-projection-head projection))
               (count (e-chat-service-projection-count projection))
               (index (mod (+ head count) e-chat-service-projection-capacity)))
          (when (= count e-chat-service-projection-capacity)
            (when-let ((evicted (aref ring head)))
              (remhash (plist-get evicted :message-id) seen))
            (setq head (mod (1+ head) e-chat-service-projection-capacity)
                  index (mod (+ head (1- count))
                             e-chat-service-projection-capacity)))
          (aset ring index (copy-tree event))
          (puthash message-id t seen)
          (setf (e-chat-service-projection-head projection) head
                (e-chat-service-projection-count projection)
                (min e-chat-service-projection-capacity (1+ count))))))))

(defun e-chat-service--projection-category-events (projection)
  "Return PROJECTION events in board sequence order."
  (let ((ring (e-chat-service-projection-ring projection))
        (head (e-chat-service-projection-head projection))
        (count (e-chat-service-projection-count projection)))
    (cl-loop for offset from 0 below count
             collect
             (copy-tree
              (aref ring (mod (+ head offset)
                              e-chat-service-projection-capacity))))))

(defun e-chat-service--projection-events (binding)
  "Return BINDING's independently bounded events in board sequence order."
  (sort
   (append
    (e-chat-service--projection-category-events
     (e-chat-service-binding-message-projection binding))
    (e-chat-service--projection-category-events
     (e-chat-service-binding-activity-projection binding)))
   (lambda (left right)
     (< (plist-get left :board-seq) (plist-get right :board-seq)))))

(defun e-chat-service--events-messages (events)
  "Return copied durable messages represented by board EVENTS."
  (let (messages)
    (dolist (event events (nreverse messages))
      (when (eq (plist-get event :type) 'message-added)
        (push (copy-tree (plist-get (plist-get event :payload) :message))
              messages)))))

(defun e-chat-service--events-activity-events (events)
  "Return copied shell activity records represented by board EVENTS."
  (let (activities)
    (dolist (event events (nreverse activities))
      (unless (memq (plist-get event :type) '(message-added board-fact))
        (push (append (list :event-type (plist-get event :type)
                            :created-at (plist-get event :created-at))
                      (copy-tree event))
              activities)))))

(defun e-chat-service--snapshot-events (binding before-seq)
  "Return independently bounded presentation events before BEFORE-SEQ."
  (let* ((board (e-chat-service-binding-board binding))
         (observer (e-chat-service-binding-observer binding))
         (source (e-board-registry-board-source-board board))
         events)
    (dolist (message
             (append
              (e-board-observer-recent-messages
               source (e-board-observer-id observer)
               :kinds '(input output)
               :limit e-chat-service-projection-capacity
               :before-seq before-seq)
              (e-board-observer-recent-messages
               source (e-board-observer-id observer)
               :kinds '(activity)
               :limit e-chat-service-projection-capacity
               :before-seq before-seq)))
      (when-let ((event (e-chat-service--message-event binding message)))
        (push event events)))
    (sort events
          (lambda (left right)
            (< (plist-get left :board-seq) (plist-get right :board-seq))))))

(defun e-chat-service--seed-binding-projection (binding)
  "Seed BINDING from independently bounded board message categories."
  (dolist (event
           (e-chat-service--snapshot-events
            binding
            (1+ (e-board-next-seq
                 (e-board-registry-board-source-board
                  (e-chat-service-binding-board binding))))))
    (e-chat-service--projection-record binding event)))

(defun e-chat-service--notify-subscribers (binding message)
  "Deliver MESSAGE from BINDING to each current shell subscriber."
  (when-let ((event (e-chat-service--message-event binding message)))
    (dolist (subscription (copy-sequence
                           (e-chat-service-binding-subscribers binding)))
      (when (e-chat-service-subscription-active-p subscription)
        (condition-case nil
            (funcall (e-chat-service-subscription-function subscription) event)
          (error nil))))))

(defun e-chat-service--binding-mutation-frozen-p (binding)
  "Return non-nil while BINDING's durable Board is committing."
  (e-board-mutation-frozen-p
   (e-board-registry-board-source-board
    (e-chat-service-binding-board binding))))

(defun e-chat-service--observer-drain-callback (binding generation)
  "Run BINDING's deferred drain only for its captured GENERATION."
  (when (= generation
           (or (e-chat-service-binding-lifecycle-generation binding) 0))
    (if (e-chat-service--binding-mutation-frozen-p binding)
        ;; The cooperative worker wait may run this owner callback while an
        ;; unrelated Board command is waiting for its durable ACK.  Preserve
        ;; the drain as pending and retry after that commit instead of turning
        ;; ordinary scheduling into a rejected dependent mutation.
        (progn
          (setf (e-chat-service-binding-observer-drain-timer binding) nil)
          (e-board--defer-after-storage-barrier
           (e-board-registry-board-source-board
            (e-chat-service-binding-board binding))
           (lambda ()
             (e-chat-service--observer-drain-callback binding generation))))
      (setf (e-chat-service-binding-observer-drain-scheduled binding) nil
            (e-chat-service-binding-observer-drain-timer binding) nil)
      (if (and (e-chat-service--binding-live-p binding)
               (let ((bindings
                      (gethash (e-chat-service-binding-harness binding)
                               e-chat-service--bindings)))
                 (and bindings
                      (eq (gethash (e-chat-service-binding-session-id binding)
                                   bindings)
                          binding))))
          (e-chat-service--drain-observer binding)
        ;; A board can enter closing/closed independently of the service.  The
        ;; queued observer callback is still the service's owner-local chance
        ;; to release all exact leases and runtime attachment state.
        (e-chat-service--retire-binding binding)))))

(defun e-chat-service--subscription-drain-callback
    (subscription binding-generation subscription-generation)
  "Run SUBSCRIPTION's deferred drain for its captured generations."
  (let ((binding (e-chat-service-subscription-binding subscription)))
    (when (and (= binding-generation
                  (or (e-chat-service-binding-lifecycle-generation binding) 0))
               (= subscription-generation
                  (or (e-chat-service-subscription-lifecycle-generation
                       subscription)
                      0)))
      (setf (e-chat-service-subscription-drain-scheduled subscription) nil
            (e-chat-service-subscription-drain-timer subscription) nil)
      (if (e-chat-service--binding-mutation-frozen-p binding)
          (progn
            (setf (e-chat-service-subscription-drain-scheduled subscription) t)
            (e-board--defer-after-storage-barrier
             (e-board-registry-board-source-board
              (e-chat-service-binding-board binding))
             (lambda ()
               (e-chat-service--subscription-drain-callback
                subscription binding-generation subscription-generation))))
        (if (and (e-chat-service-subscription-active-p subscription)
                 (not (eq (e-chat-service-binding-lifecycle-state binding)
                           'retired))
                 (memq subscription
                       (e-chat-service-binding-subscribers binding)))
            (if (e-chat-service--binding-live-p binding)
                (e-chat-service--drain-subscription subscription)
              ;; An external board close is a terminal binding event even when
              ;; no main observer callback happened to run first.
              (e-chat-service--retire-binding binding)))))))

(defun e-chat-service--schedule-observer-drain (binding)
  "Schedule one later bounded observer drain for BINDING."
  (when (and (e-chat-service--binding-live-p binding)
             (not (e-chat-service-binding-observer-drain-scheduled binding)))
    (let ((generation
           (or (e-chat-service-binding-lifecycle-generation binding) 0)))
      (setf (e-chat-service-binding-observer-drain-scheduled binding) t
            (e-chat-service-binding-observer-drain-timer binding)
            (run-at-time 0 nil #'e-chat-service--observer-drain-callback
                         binding generation)))))

(defun e-chat-service--schedule-subscription-drain (subscription)
  "Schedule one later bounded independent observer drain for SUBSCRIPTION."
  (let* ((binding (e-chat-service-subscription-binding subscription))
         (binding-generation
          (or (e-chat-service-binding-lifecycle-generation binding) 0))
         (subscription-generation
          (or (e-chat-service-subscription-lifecycle-generation subscription)
              0)))
    (when (and (e-chat-service-subscription-active-p subscription)
               (not (e-chat-service-subscription-drain-scheduled subscription)))
      (setf (e-chat-service-subscription-drain-scheduled subscription) t
            (e-chat-service-subscription-drain-timer subscription)
            (run-at-time 0 nil #'e-chat-service--subscription-drain-callback
                         subscription binding-generation
                         subscription-generation)))))

(defun e-chat-service--retire-subscription (subscription &optional state)
  "Release SUBSCRIPTION's presentation lease and record optional STATE."
  (let ((binding (e-chat-service-subscription-binding subscription)))
    (setf (e-chat-service-subscription-active-p subscription) nil
          (e-chat-service-subscription-drain-scheduled subscription) nil)
    (cl-incf (e-chat-service-subscription-lifecycle-generation subscription))
    (when-let ((timer (e-chat-service-subscription-drain-timer subscription)))
      (when (timerp timer) (cancel-timer timer))
      (setf (e-chat-service-subscription-drain-timer subscription) nil))
    (when state
      (setf (e-chat-service-subscription-state subscription) state))
    (setf (e-chat-service-binding-subscribers binding)
          (delq subscription
                (e-chat-service-binding-subscribers binding)))
    (when-let ((client (e-chat-service-subscription-client subscription)))
      (e-board-registry-detach-client-exact
       (e-chat-service-binding-board binding) client))
    (unless (eq (e-chat-service-binding-lifecycle-state binding) 'retired)
      (unless (cl-some #'e-chat-service-subscription-active-p
                       (e-chat-service-binding-subscribers binding))
        (e-chat-service--schedule-idle-close binding)))))

(defun e-chat-service--drain-subscription (subscription)
  "Deliver and accept one independent observer page for SUBSCRIPTION."
  (setf (e-chat-service-subscription-drain-scheduled subscription) nil
        (e-chat-service-subscription-drain-timer subscription) nil)
  (when (e-chat-service-subscription-active-p subscription)
    (condition-case err
        (let* ((binding (e-chat-service-subscription-binding subscription))
               (board (e-chat-service-binding-board binding))
               (client (e-chat-service-subscription-client subscription))
               (observer (e-chat-service-subscription-observer subscription))
               (page (e-board-registry-prepare-observer-page
                      board (e-board-registry-client-id client)
                      (e-board-observer-id observer)
                      :limit e-chat-service-observer-page-limit)))
          (condition-case callback-error
              (dolist (message (plist-get page :messages))
                (funcall (e-chat-service-subscription-function subscription)
                         (e-chat-service--message-event binding message)))
            (error
             (e-chat-service--retire-subscription
              subscription (list 'faulted callback-error))))
          (when (e-chat-service-subscription-active-p subscription)
            (when-let ((receipt (plist-get page :receipt)))
              (e-board-registry-accept-observer-page
               board (e-board-registry-client-id client)
               (e-board-observer-id observer) receipt))
            (when (< (or (plist-get page :through-index) 0)
                     (e-board-message-count
                      (e-board-registry-board-source-board board)))
              (e-chat-service--schedule-subscription-drain subscription))))
      (e-board-registry-client-missing
       (e-chat-service--retire-subscription
        subscription (list 'detached err))))))

(defun e-chat-service--drain-observer (binding)
  "Accept and translate one bounded live observer page for BINDING."
  (setf (e-chat-service-binding-observer-drain-scheduled binding) nil
        (e-chat-service-binding-observer-drain-timer binding) nil)
  (if (not (e-chat-service--binding-live-p binding))
      (e-chat-service--retire-binding binding)
    (let* ((board (e-chat-service-binding-board binding))
           (client (e-chat-service-binding-client binding))
           (observer (e-chat-service-binding-observer binding)))
      (if (not (e-chat-service--observer-drain-live-p
                binding client observer))
          (if (e-chat-service--main-observer-repairable-p
               binding client observer)
              (e-chat-service--repair-main-observer binding client observer)
            (e-chat-service--retire-binding binding))
        (let ((page (e-board-registry-prepare-observer-page
                     board (e-board-registry-client-id client)
                     (e-board-observer-id observer)
                     :limit e-chat-service-observer-page-limit)))
          (dolist (message (plist-get page :messages))
            (e-chat-service--projection-record
             binding (e-chat-service--message-event binding message)))
          (when-let ((receipt (plist-get page :receipt)))
            (e-board-registry-accept-observer-page
             board (e-board-registry-client-id client)
             (e-board-observer-id observer) receipt)
            (when (< (or (plist-get page :through-index) 0)
                     (e-board-message-count
                      (e-board-registry-board-source-board board)))
              (e-chat-service--schedule-observer-drain binding))))))))

(defconst e-chat-service--board-role-root "owner"
  "Durable chat board role for the user-facing owning session.")

(defconst e-chat-service--board-role-participant "participant"
  "Durable chat board role for a private execution session.")

(defun e-chat-service--routing-policy
    (participant-id pickup-selector observer-selector default-tags default-to)
  "Return one validated, detached routing policy for PARTICIPANT-ID.
`:self' is a caller-facing admission shorthand only; durable state contains the
resolved participant identity so restart never needs shell or caller policy."
  (unless (stringp participant-id)
    (signal 'e-session-error
            (list "Routing policy participant id must be a string"
                  participant-id)))
  (let* ((observer-selector
          (if (eq observer-selector :self)
              (list :subject-participant-id participant-id)
            observer-selector))
         (default-to (if (eq default-to :self) participant-id default-to))
         ;; Keep caller-owned values untouched until session's bounded
         ;; admission walk has completed.  The session copy is the one durable
         ;; detached representation used after this check.
         (policy (list :participant-id participant-id
                       :pickup-selector pickup-selector
                       :observer-selector observer-selector
                       :default-tags default-tags
                       :default-to default-to)))
    (unless (e-session-board-routing-policy-valid-p policy)
      (signal 'e-session-error (list "Invalid board routing policy" policy)))
    (e-session-board-routing-policy-copy-value policy)))

(defun e-chat-service--canonical-legacy-root-p (session association)
  "Return non-nil when SESSION has the established root identity defaults."
  (let ((role (plist-get association :association-role))
        (principal (plist-get association :principal))
        (session-id (plist-get session :id)))
    (or (equal role e-chat-service--board-role-root)
        (and (null role)
             (stringp session-id)
             (equal principal (format "chat:%s" session-id))))))

(defun e-chat-service--routing-overrides-present-p
    (participant-id participant-id-supplied-p
                   pickup-selector pickup-selector-supplied-p
                   observer-selector observer-selector-supplied-p
                   default-tags default-tags-supplied-p
                   default-to default-to-supplied-p)
  "Return non-nil when a caller supplied any routing override."
  (or participant-id-supplied-p pickup-selector-supplied-p
      observer-selector-supplied-p default-tags-supplied-p
      default-to-supplied-p
      ;; Callers outside CL keyword binding sometimes pass a non-nil value via
      ;; an adapter; preserve the explicit-value meaning at this boundary.
      participant-id pickup-selector observer-selector default-tags default-to))

(defun e-chat-service--complete-routing-arguments-p
    (participant-id participant-id-supplied-p
                    pickup-selector pickup-selector-supplied-p
                    observer-selector observer-selector-supplied-p
                    default-tags default-tags-supplied-p
                    default-to default-to-supplied-p)
  "Return non-nil when all five caller routing fields are explicitly present."
  (and participant-id-supplied-p pickup-selector-supplied-p
       observer-selector-supplied-p default-tags-supplied-p
       default-to-supplied-p
       ;; Route all complete-policy validation through the session-owned
       ;; bounded admission contract.  In particular, do not scan a hostile
       ;; selector/tag value here before that budget is charged.
       (condition-case nil
           (progn
             (e-chat-service--routing-policy
              participant-id pickup-selector observer-selector
              default-tags default-to)
             t)
         (e-session-error nil))))

(defun e-chat-service--routing-override-conflicts-p
    (policy participant-id participant-id-supplied-p
           pickup-selector pickup-selector-supplied-p
           observer-selector observer-selector-supplied-p
           default-tags default-tags-supplied-p default-to default-to-supplied-p)
  "Return non-nil when supplied routing values conflict with POLICY."
  (or (and participant-id-supplied-p
           (not (equal participant-id (plist-get policy :participant-id))))
      (and pickup-selector-supplied-p
           (not (equal pickup-selector (plist-get policy :pickup-selector))))
      (and observer-selector-supplied-p
           (let ((expected
                  (if (eq observer-selector :self)
                      (list :subject-participant-id
                            (plist-get policy :participant-id))
                    observer-selector)))
             (not (equal expected
                         (plist-get policy :observer-selector)))))
      (and default-tags-supplied-p
           (not (equal default-tags (plist-get policy :default-tags))))
      (and default-to-supplied-p
           (let ((expected
                  (if (eq default-to :self)
                      (plist-get policy :participant-id)
                    default-to)))
             (not (equal expected (plist-get policy :default-to)))))))

(defun e-chat-service--session-routing-policy (session &optional allow-missing)
  "Return SESSION's durable routing policy or signal for unsafe restoration."
  (let ((association (e-session-board-association session)))
    (when (e-session-board-association-invalid-p association)
      (signal 'e-session-error
              (list "Malformed board association" (plist-get session :id))))
    (cond
     ((e-session-board-association-policy-present-p association)
      (let ((policy (e-session-board-routing-policy session)))
        (unless (e-session-board-routing-policy-valid-p policy)
          (signal 'e-session-error
                  (list "Malformed board routing policy"
                        (plist-get session :id))))
        policy))
     ;; A complete caller-supplied policy may upgrade a legacy participant at
     ;; the explicit open boundary.  The ordinary restore path never gets
     ;; this exception; it must fail closed below.
     ((and allow-missing association)
      ;; The explicit open boundary may supply a complete replacement for any
      ;; legacy association whose policy is absent.  The caller still decides
      ;; below whether the supplied fields are complete; implicit restoration
      ;; never takes this branch.
      nil)
     ((e-chat-service--canonical-legacy-root-p session association)
      ;; Canonical roleless/owner records are the only legacy shapes with the
      ;; established main defaults.  Ambiguous roleless records do not get a
      ;; default participant policy by inference.
      nil)
     ((null association)
      ;; Let the caller report the established missing board-state condition.
      nil)
     (t
      (signal 'e-session-error
              (list "Legacy board participant has no complete routing policy"
                    (plist-get session :id)))))))

(cl-defun e-chat-service--install-participant-binding
    (board harness session-id &key principal participant-id
           (pickup-selector '(:tags (main)))
           (observer-selector '(:tags (main)))
           (default-tags '(main)) default-to
           defer-participant-publication restore-existing-participant)
  "Install one HARNESS SESSION-ID participant/client binding on BOARD."
  (or (e-chat-service-binding harness session-id)
      (progn
        (e-session-get (e-harness-sessions harness) session-id)
        (let (client requester attachment participant main-subscription
                     source-board snapshot-cursor observer binding)
          (condition-case error
              (progn
                (setq principal (or principal
                                    (e-board-registry-board-principal board)))
                (setq client
                      (e-board-registry-attach-client
                       board :principal principal :author "e-chat"))
                (setq requester
                      (e-board-registry-client-requester-context
                       board (e-board-registry-client-id client)))
                (setq attachment
                      (if restore-existing-participant
                          (e-board-runtime-reattach
                           board harness session-id participant-id
                           :principal principal :controller principal
                           :author "e-chat")
                        (e-board-runtime-attach
                         board harness session-id :participant-id participant-id
                         :principal principal :controller principal
                         :author "e-chat"
                         :defer-participant-publication
                         defer-participant-publication)))
                (setq participant
                      (e-board-runtime-attachment-participant attachment))
                (setq main-subscription
                      (e-board-registry-install-subscription
                       board participant pickup-selector))
                (setq source-board
                      (e-board-registry-board-source-board board))
                (setq snapshot-cursor (e-board-next-seq source-board))
                (setq observer
                      (e-board-registry-install-observer
                       board (e-board-registry-client-id client)
                       observer-selector :start-seq snapshot-cursor))
                (setq binding
                      (e-chat-service--binding-create
                       :harness harness :session-id session-id :board board
                       :client client :requester requester
                       :attachment attachment :observer observer
                       :subscribers nil :turn-map (make-hash-table :test 'equal)
                       :input-sequence 0 :default-tags (copy-tree default-tags)
                       :default-to default-to
                       :message-projection (e-chat-service--make-projection)
                       :activity-projection (e-chat-service--make-projection)
                       :lifecycle-generation 0
                       :observer-drain-timer nil
                       :lifecycle-state 'active))
                (puthash session-id binding
                         (e-chat-service--harness-bindings harness))
                (puthash (e-board-registry-board-id board)
                         (cons binding
                               (gethash (e-board-registry-board-id board)
                                        e-chat-service--board-bindings))
                         e-chat-service--board-bindings)
                ;; Materialize only the recent bounded tail.  The observer's
                ;; live cursor already starts at the same high watermark, so
                ;; retained history is never rescanned to fill a fixed-capacity
                ;; projection.
                (e-chat-service--seed-binding-projection binding)
                (e-chat-service-reconcile-board-continuation board harness)
                ;; These callbacks are installed only after all admission
                ;; steps above succeed, keeping attachment failure cleanup
                ;; independent of board notification publication.
                (e-board-session-association-configure-notifications
                 (e-harness-sessions harness) session-id source-board
                 (lambda (source _message)
                        (dolist (current (copy-sequence
                                          (gethash (e-board-id source)
                                                   e-chat-service--board-bindings)))
                          (dolist (subscription
                                   (copy-sequence
                                    (e-chat-service-binding-subscribers current)))
                            (e-chat-service--schedule-subscription-drain
                             subscription))
                          (e-chat-service--schedule-observer-drain current))
                        (when-let* ((owner
                                    (car (gethash
                                          (e-board-id source)
                                          e-chat-service--board-bindings))))
                          (e-chat-service-reconcile-board-continuation
                           (e-chat-service-binding-board owner)
                           (e-chat-service-binding-harness owner)))))
                binding)
            (error
             ;; No binding is returned until all process-local maps are in a
             ;; coherent state.  If a later setup step fails, remove only the
             ;; objects allocated by this admission and leave the board event
             ;; stream untouched.
             (when binding
               (let ((bindings (e-chat-service--harness-bindings harness))
                     (board-id (e-board-registry-board-id board)))
                 (when (eq (gethash session-id bindings) binding)
                   (remhash session-id bindings))
                 (let ((remaining
                        (delq binding
                              (gethash board-id
                                       e-chat-service--board-bindings))))
                   (if remaining
                       (puthash board-id remaining e-chat-service--board-bindings)
                     (remhash board-id e-chat-service--board-bindings)))))
             (when main-subscription
               (e-board-retire-subscription-exact source-board main-subscription))
             (when attachment
               (e-board-runtime-abort-new-attachment attachment))
             (when client
               (e-board-registry-detach-client-exact board client))
             (signal (car error) (cdr error))))))))

(defun e-chat-service--bind-session (harness session-id)
  "Restore and bind board-native HARNESS SESSION-ID as its main member."
  (or (e-chat-service-binding harness session-id)
      (let* ((session (e-session-get (e-harness-sessions harness) session-id))
             (board-state (plist-get session :board-session-state))
             (routing-policy (e-chat-service--session-routing-policy session))
             (principal (plist-get board-state :principal))
             (board-id (plist-get board-state :board-id))
             (_ (unless (and (stringp board-id) principal)
                  (signal 'e-session-missing
                          (list session-id 'board-session-state))))
             (board
              (e-board-session-association-restore
               (e-harness-sessions harness) session)))
        (if routing-policy
            (e-chat-service--install-participant-binding
             board harness session-id :principal principal
             :participant-id (plist-get routing-policy :participant-id)
             :pickup-selector (plist-get routing-policy :pickup-selector)
             :observer-selector (plist-get routing-policy :observer-selector)
             :default-tags (plist-get routing-policy :default-tags)
             :default-to (plist-get routing-policy :default-to)
             :restore-existing-participant
             (condition-case nil
                 (progn
                   (e-board-registry-participant
                    board (plist-get routing-policy :participant-id))
                   t)
               (e-board-registry-participant-missing nil)))
          (e-chat-service--install-participant-binding
           board harness session-id :principal principal)))))

(cl-defun e-chat-service-create-board (&key harness metadata id)
  "Create a top-level board with one main participant and return its binding."
  (let* ((harness (or harness (e-chat-service-default-harness)))
         (session (e-harness-create-session harness :id id :metadata metadata))
         (session-id (plist-get session :id))
         (principal (format "chat:%s" session-id))
         (store (e-harness-sessions harness))
         (board (e-board-session-association-create-board store principal))
         (participant-id (e-board-registry-allocate-participant-id board))
         (routing-policy
          (e-chat-service--routing-policy
           participant-id '(:tags (main)) '(:tags (main)) '(main) nil)))
    (e-board-session-association-persist
     store session-id principal (e-board-registry-board-id board)
     e-chat-service--board-role-root routing-policy)
    (e-chat-service--install-participant-binding
     board harness session-id :principal principal
     :participant-id participant-id
     :pickup-selector (plist-get routing-policy :pickup-selector)
     :observer-selector (plist-get routing-policy :observer-selector)
     :default-tags (plist-get routing-policy :default-tags)
     :default-to (plist-get routing-policy :default-to))))

(cl-defun e-chat-service-open-board
    (board harness session-id
           &key (participant-id nil participant-id-supplied-p)
           (pickup-selector nil pickup-selector-supplied-p)
           (observer-selector nil observer-selector-supplied-p)
           (default-tags nil default-tags-supplied-p)
           (default-to nil default-to-supplied-p))
  "Open existing BOARD by attaching HARNESS SESSION-ID as one participant."
  (let* ((board (e-board-registry-get board))
         (session (e-session-get (e-harness-sessions harness) session-id))
         (state (plist-get session :board-session-state))
         (association (e-session-board-association session))
         (routing-policy
          (e-chat-service--session-routing-policy session t))
         (overrides-p
          (e-chat-service--routing-overrides-present-p
           participant-id participant-id-supplied-p
           pickup-selector pickup-selector-supplied-p
           observer-selector observer-selector-supplied-p
           default-tags default-tags-supplied-p
           default-to default-to-supplied-p))
         (complete-p
          (e-chat-service--complete-routing-arguments-p
           participant-id participant-id-supplied-p
           pickup-selector pickup-selector-supplied-p
           observer-selector observer-selector-supplied-p
           default-tags default-tags-supplied-p
           default-to default-to-supplied-p)))
    (unless (and (equal (plist-get state :board-id)
                        (e-board-registry-board-id board))
                 (equal (plist-get state :principal)
                        (e-board-registry-board-principal board)))
      (signal 'e-session-missing (list session-id 'board-session-state)))
    (cond
     (routing-policy
      (when (and overrides-p
                 (e-chat-service--routing-override-conflicts-p
                  routing-policy participant-id participant-id-supplied-p
                  pickup-selector pickup-selector-supplied-p
                  observer-selector observer-selector-supplied-p
                  default-tags default-tags-supplied-p
                  default-to default-to-supplied-p))
        (signal 'e-session-error
                (list "Routing policy override conflicts with durable state"
                      session-id))))
     (complete-p
     (setq routing-policy
            (e-chat-service--routing-policy
             participant-id pickup-selector observer-selector
             default-tags default-to))
      ;; A complete legacy upgrade is admitted against the real runtime before
      ;; changing durable association bytes.  In particular, an occupied board
      ;; participant id or already-attached session fails without wedging the
      ;; legacy record into an unusable policy.
      (e-board-runtime-admission-available-p
       board harness session-id participant-id
       :principal (plist-get state :principal))
      ;; Admission upgrades are durable before a runtime attachment can expose
      ;; the participant to board traffic.
      (e-board-session-association-persist
       (e-harness-sessions harness) session-id
       (plist-get state :principal) (plist-get state :board-id)
       (plist-get state :association-role) routing-policy))
     ((e-chat-service--canonical-legacy-root-p session association)
      ;; Canonical roleless/owner legacy sessions retain the historical main
      ;; defaults.  No caller-supplied partial policy is silently borrowed.
      nil)
     (t
      (signal 'e-session-error
              (list "Legacy board participant has no complete routing policy"
                    session-id))))
    (if routing-policy
        (e-chat-service--install-participant-binding
         board harness session-id
         :participant-id (plist-get routing-policy :participant-id)
         :pickup-selector (plist-get routing-policy :pickup-selector)
         :observer-selector (plist-get routing-policy :observer-selector)
         :default-tags (plist-get routing-policy :default-tags)
         :default-to (plist-get routing-policy :default-to))
      (e-chat-service--install-participant-binding
       board harness session-id
       :participant-id participant-id
       :pickup-selector (or pickup-selector '(:tags (main)))
       :observer-selector (or observer-selector '(:tags (main)))
       :default-tags (or default-tags '(main)) :default-to default-to))))

(cl-defun e-chat-service-list-boards-page (&key after limit)
  "Return one bounded registry board page after AFTER.
LIMIT defaults to the registry's fixed page bound."
  (if limit
      (e-board-registry-list-page :after after :limit limit)
    (e-board-registry-list-page :after after)))

(cl-defun e-chat-service-create-participant
    (board harness &key metadata id participant-id pickup-selector
           observer-selector (default-tags '(main)) default-to)
  "Create and attach a private execution session as a participant on BOARD."
  ;; Resolve every caller-controlled admission input before creating the
  ;; session.  The session is not a valid root/participant until the complete
  ;; association is durable and the attachment succeeds.
  (let* ((board (e-board-registry-get board))
         (participant-id (or participant-id
                             (e-board-registry-allocate-participant-id board)))
         (principal (e-board-registry-board-principal board))
         (store (e-harness-sessions harness))
         (pickup-selector (or pickup-selector '(:tags (main))))
         (observer-selector (or observer-selector '(:tags (main))))
         (routing-policy
          (e-chat-service--routing-policy
           participant-id pickup-selector observer-selector
           default-tags default-to))
         (_ (when (gethash participant-id
                           (e-board-registry-board-participants board))
             (signal 'e-board-registry-id-conflict (list participant-id))))
         (session-id (or id (e-session-generate-id)))
         (_ (e-board-runtime-admission-available-p
             board harness session-id participant-id
             :principal principal :require-session nil))
         (_ (when id
             (condition-case nil
                 (progn (e-session-get store id)
                        (signal 'e-session-duplicate (list id)))
               (e-session-missing nil)))))
    (let ((session nil)
          (binding nil))
      (condition-case error
          (progn
            ;; Session id and runtime occupancy were preflighted above.  Keep
            ;; creation inside this owning failure boundary so a later service
            ;; error cannot strand the newly allocated root.
            (setq session
                  (e-session-create-board-admission
                   store :id session-id :metadata metadata
                   :principal principal
                   :board-id (e-board-registry-board-id board)
                   :association-role e-chat-service--board-role-participant
                   :routing-policy routing-policy))
            (setq binding
                  (e-chat-service--install-participant-binding
                   board harness session-id
                   :participant-id (plist-get routing-policy :participant-id)
                   :pickup-selector (plist-get routing-policy :pickup-selector)
                   :observer-selector (plist-get routing-policy :observer-selector)
                   :default-tags (plist-get routing-policy :default-tags)
                   :default-to (plist-get routing-policy :default-to)
                   :defer-participant-publication t))
              ;; The session owner publishes root + association only after the
              ;; registry/runtime attachment has completed successfully.
              (e-session-commit-board-admission store session-id)
              ;; The participant's source-board event is deliberately
              ;; published only after the session declaration has crossed its
              ;; durable admission boundary.  A failed commit therefore
              ;; cannot leave a replayable participant-added ghost.
              (e-board-registry-publish-participant-admission
               board
               (e-board-runtime-attachment-participant
                (e-chat-service-binding-attachment binding)))
              session)
        (error
         ;; Expected service-owned failures must not leave a false root in the
         ;; catalog.  Discard the unpublished runtime binding without emitting
         ;; a board removal event, then remove the session reservation.
         (when (e-chat-service-binding-p binding)
           (e-chat-service--discard-binding binding))
         (ignore-errors (e-session-abort-created store session-id))
         (signal (car error) (cdr error)))))))

(defun e-chat-service--harness-has-capability-p (harness capability-id)
  "Return non-nil when HARNESS has active capability CAPABILITY-ID."
  (memq capability-id
        (mapcar #'e-capability-id
                (e-harness-active-capabilities harness))))

(defun e-chat-service--harness-for-instance (instance)
  "Return the live chat harness for INSTANCE."
  (let* ((instance-id (e-harness-instance-id instance))
         (harness
          (condition-case err
              (e-harness-instance-get-or-create instance-id)
            ((e-harness-instance-missing e-harness-registry-missing)
             (user-error "No e harness registered for %S" (cadr err))))))
    (unless (e-chat-service--harness-has-capability-p harness 'chat-session)
      (user-error "Harness instance %S does not provide chat-session capability"
                  instance-id))
    harness))

(defun e-chat-service-default-harness-id ()
  "Return the effective default chat harness id."
  (if (boundp 'e-chat-default-harness-id)
      e-chat-default-harness-id
    e-chat-service-default-harness-id))

(defun e-chat-service-default-harness ()
  "Return the configured default chat harness."
  (let* ((harness-id (e-chat-service-default-harness-id))
         (instance (e-harness-instance-get harness-id))
         (harness
          (if instance
              (e-chat-service--harness-for-instance instance)
            (condition-case err
                (e-harness-registry-get-or-create harness-id)
              (e-harness-registry-missing
               (user-error "No e harness registered for %S" (cadr err)))))))
    (unless (e-chat-service--harness-has-capability-p harness 'chat-session)
      (user-error "Harness %S does not provide chat-session capability"
                  harness-id))
    harness))

(cl-defun e-chat-service-create-session (&key harness metadata id)
  "Create a new board's main participant and return its private session record."
  (let ((binding (e-chat-service-create-board
                  :harness harness :metadata metadata :id id)))
    (e-session-get
     (e-harness-sessions (e-chat-service-binding-harness binding))
     (e-chat-service-binding-session-id binding))))

(defun e-chat-service-ensure-binding (harness session-id)
  "Return HARNESS SESSION-ID's board binding, creating it when needed."
  (e-chat-service--bind-session harness session-id))

(cl-defun e-chat-service--subscribe
    (harness session-id function &key start-seq history-before-seq)
  "Create one board observer for FUNCTION at the requested cursors."
  (unless (functionp function)
    (signal 'wrong-type-argument (list 'functionp function)))
  (let* ((binding (e-chat-service--bind-session harness session-id))
         (_capacity
          (when (>= (length (e-chat-service-binding-subscribers binding))
                    e-chat-service-subscriber-limit)
            (user-error "Chat board subscriber limit reached for %s" session-id)))
         (board (e-chat-service-binding-board binding))
         (principal (e-board-registry-client-principal
                     (e-chat-service-binding-client binding)))
         (client (e-board-registry-attach-client
                  board :principal principal :author "e-chat-subscriber"))
         (observer (e-board-registry-install-observer
                    board (e-board-registry-client-id client)
                    (copy-tree
                     (e-board-observer-selector
                      (e-chat-service-binding-observer binding)))
                    :start-seq start-seq
                    :history-before-seq history-before-seq))
         (subscription (e-chat-service--subscription-create
                        :binding binding :function function :active-p t
                        :client client :observer observer :state 'active
                        :drain-timer nil :lifecycle-generation 0)))
    (e-chat-service--cancel-idle-close binding)
    (setf (e-chat-service-binding-subscribers binding)
          (cons subscription (e-chat-service-binding-subscribers binding)))
    (e-chat-service--schedule-subscription-drain subscription)
    subscription))

(defun e-chat-service-subscribe (harness session-id function)
  "Subscribe FUNCTION to future board events for HARNESS SESSION-ID.
Retained history is a snapshot concern and is never replayed implicitly."
  (e-chat-service--subscribe harness session-id function))

(defun e-chat-service-subscribe-view (harness session-id function)
  "Return a bounded snapshot plus live subscription for HARNESS SESSION-ID.
The snapshot and subscription share one board high-watermark cursor.  EVENTS
before that cursor appear only in the snapshot; later events appear only via
FUNCTION."
  (let* ((binding (e-chat-service--bind-session harness session-id))
         (board (e-chat-service-binding-board binding))
         (source (e-board-registry-board-source-board board))
         (cursor (e-board-next-seq source))
         (subscription
          (e-chat-service--subscribe
           harness session-id function
           :start-seq cursor))
         (events (e-chat-service--snapshot-events binding (1+ cursor))))
    (e-chat-service--view-create
     :cursor cursor
     :messages (e-chat-service--events-messages events)
     :activity-events (e-chat-service--events-activity-events events)
     :subscription subscription)))

(defun e-chat-service-unsubscribe (subscription)
  "Idempotently retire board-observer SUBSCRIPTION."
  (when (e-chat-service-subscription-p subscription)
    (e-chat-service--retire-subscription subscription))
  nil)

(defun e-chat-service-drain-binding (binding)
  "Deliver one bounded pending page for BINDING.

Return non-nil when another page remains.  This deterministic pump is useful
to synchronous embedding shells and performance fixtures; normal clients
leave page scheduling to the service.  The board observer and its cursor stay
private to the service."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (let ((client (e-chat-service-binding-client binding))
        (observer (e-chat-service-binding-observer binding)))
    (if (not (e-chat-service--observer-drain-live-p
              binding client observer))
        (progn
          (if (e-chat-service--main-observer-repairable-p
               binding client observer)
              (e-chat-service--repair-main-observer binding client observer)
            (e-chat-service--retire-binding binding))
          nil)
      (e-chat-service--drain-observer binding)
      (and (e-chat-service--observer-drain-live-p
            binding client observer)
           (e-chat-service--observer-drain-pending-p binding observer)))))

(defun e-chat-service-drain-subscription (subscription)
  "Deliver one bounded pending page for SUBSCRIPTION.

Return non-nil when another page remains.  Observer cursors remain an
implementation detail of the chat service."
  (unless (e-chat-service-subscription-p subscription)
    (signal 'wrong-type-argument
            (list 'e-chat-service-subscription-p subscription)))
  (let* ((binding (e-chat-service-subscription-binding subscription))
         (client (e-chat-service-subscription-client subscription))
         (observer (e-chat-service-subscription-observer subscription)))
    (if (not (and (e-chat-service-subscription-active-p subscription)
                  (e-chat-service--observer-drain-live-p
                   binding client observer)))
        (progn
          (when (e-chat-service-subscription-active-p subscription)
            (if (not (e-chat-service--binding-live-p binding))
                (e-chat-service--retire-binding binding)
              (e-chat-service--retire-subscription
               subscription
               (e-chat-service--observer-drain-retirement-state
                binding client observer))))
          nil)
      (e-chat-service--drain-subscription subscription)
      (and (e-chat-service-subscription-active-p subscription)
           (e-chat-service--observer-drain-live-p
            binding client observer)
           (e-chat-service--observer-drain-pending-p binding observer)))))

(cl-defun e-chat-service-replace-selector
    (subscription selector &key start-seq)
  "Replace SUBSCRIPTION's observer with SELECTOR and optional START-SEQ backfill."
  (unless (e-chat-service-subscription-p subscription)
    (signal 'wrong-type-argument
            (list 'e-chat-service-subscription-p subscription)))
  (let* ((binding (e-chat-service-subscription-binding subscription))
         (board (e-chat-service-binding-board binding))
         (client (e-chat-service-subscription-client subscription))
         (observer (e-chat-service-subscription-observer subscription))
         (replacement
          (e-board-registry-replace-observer
           board (e-board-registry-client-id client)
           (e-board-observer-id observer) selector :start-seq start-seq)))
    (setf (e-chat-service-subscription-observer subscription) replacement
          (e-chat-service-subscription-active-p subscription) t
          (e-chat-service-subscription-state subscription) 'active)
    (e-chat-service--schedule-subscription-drain subscription)
    replacement))

(cl-defun e-chat-service-post
    (binding prompt &key (mode 'inject) tags attributes to references metadata)
  "Post PROMPT through board-first BINDING with generic routing fields."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (e-chat-service--post
   (e-chat-service-binding-harness binding)
   (e-chat-service-binding-session-id binding)
   prompt mode :tags tags :attributes attributes :to to
   :references references :metadata metadata))

(cl-defun e-chat-service--post
    (harness session-id prompt mode &key references metadata tags attributes to source-input-key)
  "Post PROMPT to HARNESS SESSION-ID's board binding in MODE."
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (let* ((binding (e-chat-service--bind-session harness session-id))
         (client (e-chat-service-binding-client binding))
         (sequence (cl-incf (e-chat-service-binding-input-sequence binding)))
         (publication
          (e-board-runtime-post-input
           (e-chat-service-binding-board binding)
           :author (format "client:%s" (e-board-registry-client-id client))
           :requester (e-chat-service-binding-requester binding)
           :tags (or (copy-tree tags)
                     (copy-tree (e-chat-service-binding-default-tags binding)))
           :to (or to (e-chat-service-binding-default-to binding))
           :attributes (append (copy-tree attributes) (copy-tree metadata)
                               (and references
                                    (list :references (copy-tree references))))
           :mode mode :content prompt :reference (copy-tree references)
           :source-input-key
           (or source-input-key
               (list (e-board-registry-client-id client)
                     (e-board-registry-client-generation client)
                     sequence)))))
    (e-board-message-id (e-board-publication-message publication))))

(cl-defun e-chat-service-submit-session
    (harness session-id prompt &key references metadata)
  "Submit PROMPT through HARNESS SESSION-ID's board binding."
  (e-chat-service--post harness session-id prompt 'inject
                        :references references :metadata metadata))

(cl-defun e-chat-service-steer-session
    (harness session-id prompt &key metadata)
  "Post steering PROMPT through HARNESS SESSION-ID's board binding."
  (e-chat-service--post harness session-id prompt 'inject :metadata metadata))

(cl-defun e-chat-service-queue-session
    (harness session-id prompt &key references metadata source-input-key)
  "Queue PROMPT through HARNESS SESSION-ID's board binding.
SOURCE-INPUT-KEY lets durable callers retry one queued input exactly once."
  (e-chat-service--post harness session-id prompt 'queue
                        :references references :metadata metadata
                        :source-input-key source-input-key))

(defun e-chat-service-abort-session (harness session-id)
  "Abort the current board-bound turn for HARNESS SESSION-ID."
  (e-board-runtime-abort-attachment
   (e-chat-service-binding-attachment
    (e-chat-service--bind-session harness session-id))))

(defun e-chat-service--binding-active-turn (binding)
  "Return BINDING's running turn in the presentation id namespace.
Board activity events are correlated to their causal input message before they
reach presentation subscribers.  Apply the same translation to the private
harness turn here so state queries and events identify one turn consistently."
  (when-let* ((attachment (e-chat-service-binding-attachment binding))
              (active-turn
               (e-board-runtime-attachment-active-turn attachment))
              ((eq (plist-get active-turn :status) 'running)))
    (let* ((participant-id
            (e-board-registry-participant-id
             (e-board-runtime-attachment-participant attachment)))
           (source-turn-id (plist-get active-turn :id)))
      (plist-put active-turn :id
                 (e-chat-service--turn-id
                  binding participant-id source-turn-id))
      active-turn)))

(defun e-chat-service-active-turn (harness session-id)
  "Return SESSION-ID's running turn using presentation-facing identity.
The returned `:id' is in the same namespace as board-derived event `:turn-id'
values delivered by `e-chat-service-subscribe'."
  (e-chat-service--binding-active-turn
   (e-chat-service--bind-session harness session-id)))

(defun e-chat-service-active-turn-p (harness session-id)
  "Return non-nil when HARNESS SESSION-ID's board participant is running."
  (and (e-chat-service-active-turn harness session-id) t))

(defun e-chat-service-session (harness session-id)
  "Return SESSION-ID's private metadata through the board application seam."
  (e-session-get (e-harness-sessions harness) session-id))

(defun e-chat-service-session-options (harness session-id)
  "Return the effective option projection for SESSION-ID.
Chat presentation controls use this service operation instead of depending on
the harness context owner directly."
  (e-harness-session-options harness session-id))

(defun e-chat-service-session-store (harness)
  "Return HARNESS's private session store for controlled shell metadata work."
  (e-harness-sessions harness))

(defun e-chat-service-session-list (harness)
  "Return HARNESS's private session catalog for bounded shell navigation."
  (e-harness-session-list harness))

(defun e-chat-service--root-session-p (session)
  "Return non-nil when SESSION is a user-facing chat root.
An explicit durable chat role is authoritative.  Canonical legacy board state
without that role falls back to its historical `chat:<session-id>' owner
identity so existing indexes remain readable without mutation."
  (let* ((state (e-session-board-association session))
         (role-present (and state (plist-member state :association-role)))
         (role (and role-present (plist-get state :association-role)))
         (principal (plist-get state :principal))
         (session-id (plist-get session :id)))
    (cond
     ((null state) t)
     ((e-session-board-association-invalid-p state) nil)
     (role-present
      (equal role e-chat-service--board-role-root))
     (t
      (and (stringp session-id)
           (equal principal (format "chat:%s" session-id)))))))

(defun e-chat-service-root-session-list (harness)
  "Return HARNESS's user-facing chat roots for shell navigation."
  (cl-remove-if-not #'e-chat-service--root-session-p
                    (e-harness-root-session-list harness)))

(defun e-chat-service-messages (harness session-id)
  "Return SESSION-ID's bounded board-derived message projection."
  (e-chat-service--events-messages
   (e-chat-service--projection-events
    (e-chat-service--bind-session harness session-id))))

(defun e-chat-service-activity-events (harness session-id)
  "Return SESSION-ID's bounded board-derived activity projection."
  (e-chat-service--events-activity-events
   (e-chat-service--projection-events
    (e-chat-service--bind-session harness session-id))))

(defun e-chat-service-state (harness session-id)
  "Return SESSION-ID's bounded board-derived presentation state."
  (let* ((binding (e-chat-service--bind-session harness session-id))
         (activities (e-chat-service-activity-events harness session-id))
         ;; Seed the public snapshot from the live attached-turn projection.
         ;; Retained board activity below reconciles terminal state and covers
         ;; restart/replay when no live attachment remains.
         (active-turn (e-chat-service--binding-active-turn binding)))
    (dolist (event activities)
      (pcase (plist-get event :event-type)
        ('turn-started
         (when (and (e-chat-service--event-selected-participant-p event)
                    (not active-turn))
           (setq active-turn (list :id (plist-get event :turn-id)
                                   :status 'running))))
        ((or 'turn-finished 'turn-failed 'turn-cancelled)
         (when (and (e-chat-service--event-selected-participant-p event)
                    (equal (plist-get active-turn :id)
                           (plist-get event :turn-id)))
           (setq active-turn nil)))
        ('turn-summary
         ;; A summary is not a second shell-facing terminal event, but its
         ;; status is the durable terminal witness used to reconcile bounded
         ;; service state after the detailed row or successful output falls
         ;; outside the retained activity page.
         (when (and (memq (plist-get (plist-get event :payload) :status)
                          '(finished failed turn-failed
                            cancelled turn-cancelled))
                    (e-chat-service--event-selected-participant-p event)
                    (equal (plist-get active-turn :id)
                           (plist-get event :turn-id)))
           (setq active-turn nil)))))
    (list :board-id
          (e-board-registry-board-id (e-chat-service-binding-board binding))
          :message-count (length (e-chat-service-messages harness session-id))
          :active-turn active-turn)))

(defun e-chat-service-active-capabilities (harness)
  "Return HARNESS's active capabilities for shell affordance discovery."
  (e-harness-active-capabilities harness))

(defun e-chat-service-structured-blocks (harness session-id)
  "Return SESSION-ID's private structured-block registry projection."
  (e-harness-structured-blocks harness session-id))

(defun e-chat-service-message-presentation (harness session-id message)
  "Return generic display content and details for durable MESSAGE.
The service applies the active structured-block registry once, then gives the
parsed blocks to capability-owned detail providers.  Presentation shells never
need to know a block kind or capability policy."
  (let ((content (plist-get message :content)))
    (if (not (and (eq (plist-get message :role) 'assistant)
                  (stringp content)))
        (list :content content :details nil)
      (let* ((registry (e-harness-structured-blocks
                        harness session-id (plist-get message :turn-id)))
             (rendered (e-structured-blocks-render content registry)))
        (list :content (plist-get rendered :text)
              :details
              (e-harness-message-details
               harness session-id message
               :structured-blocks (plist-get rendered :blocks)))))))

(defun e-chat-service-session-name (harness session-id)
  "Return SESSION-ID's private configured name."
  (e-harness-session-name harness session-id))

(defun e-chat-service-session-title (harness session-id)
  "Return SESSION-ID's private display title."
  (e-harness-session-title harness session-id))

(defun e-chat-service-prompt-catalog (harness)
  "Return HARNESS's named prompt catalog for composer completion."
  (e-harness-prompts harness))

(defun e-chat-service-queued-inputs (harness session-id)
  "Return SESSION-ID's queued board inputs for bounded local presentation."
  (let* ((binding (e-chat-service--bind-session harness session-id))
         (source (e-board-registry-board-source-board
                  (e-chat-service-binding-board binding)))
         queued)
    (dolist (event (e-chat-service--projection-events binding)
                   (nreverse queued))
      (when (and (eq (plist-get event :board-kind) 'input)
                 (eq (plist-get event :mode) 'queue))
        (let* ((message-id (plist-get event :message-id))
               (message (e-board-message source message-id))
               (pending
                (cl-some
                 (lambda (pickup-id)
                   (memq (e-board-pickup-state (e-board-pickup source pickup-id))
                         '(pending ready delivering accepted cancelling)))
                 (and message (e-board-message-pickup-ids message)))))
          (when pending
            (push (list :prompt
                        (plist-get
                         (plist-get (plist-get event :payload) :message)
                         :content)
                        :references (plist-get event :reference)
                        :metadata (copy-tree (plist-get event :attributes)))
                  queued)))))))

(defun e-chat-service-active-turns (harness)
  "Return HARNESS's board-derived active-turn index for shell diagnostics."
  (let ((result (make-hash-table :test 'equal)))
    (when-let ((bindings (gethash harness e-chat-service--bindings)))
      (maphash
       (lambda (session-id binding)
         (when (e-chat-service--binding-live-p binding)
           (when-let ((active-turn
                       (e-chat-service--binding-active-turn binding)))
             (puthash session-id active-turn result))))
       bindings))
    result))

(defun e-chat-service-append-seed-message (harness session-id message)
  "Append explicit pre-turn seed MESSAGE to board-bound SESSION-ID."
  (e-chat-service--bind-session harness session-id)
  (e-session-append-message
   (e-harness-sessions harness) session-id (copy-sequence message)))

(provide 'e-chat-service)

;;; e-chat-service.el ends here
