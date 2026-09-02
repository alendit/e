;;; e-board-registry.el --- Process-local board lifecycle registry for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module owns the process-local board lifecycle and its local identities.
;; `e-board' remains the source-board model for routing and subscriptions.

;;; Code:

(require 'cl-lib)
(require 'avl-tree)
(require 'e-board)
(require 'e-request)
(require 'e-session)

(define-error 'e-board-registry-error "e board registry error")
(define-error 'e-board-registry-id-conflict "e board registry id conflict"
  'e-board-registry-error)
(define-error 'e-board-registry-missing "e board registry board is missing"
  'e-board-registry-error)
(define-error 'e-board-registry-closed "e board registry board is closed"
  'e-board-registry-error)
(define-error 'e-board-registry-participant-board-local
  "e board registry participant belongs to another board"
  'e-board-registry-error)
(define-error 'e-board-registry-participant-missing
  "e board registry participant is missing"
  'e-board-registry-error)
(define-error 'e-board-registry-client-missing
  "e board registry client is missing"
  'e-board-registry-error)
(define-error 'e-board-registry-authorization-denied
  "e board registry authorization denied"
  'e-board-registry-error)

(defvar e-board-registry--id-sequence 0
  "Deterministic sequence retained only for explicitly injected test ids.")

(defvar e-board-registry--boards (make-hash-table :test 'equal)
  "Live and closed process-local boards keyed by board id.")

(defvar e-board-registry--board-index
  (avl-tree-create
   (lambda (left right) (string< (car left) (car right))))
  "Ordered board index of printable identity keys to registry boards.")

(defconst e-board-registry-list-page-limit 32
  "Default maximum number of board records returned by one registry page.")

(defconst e-board-registry-client-revocation-drain-limit 16
  "Maximum client or observer revocation units committed per drain.")

(defvar e-board-registry--unsettled-pickup-count 0)
(defvar e-board-registry--unsettled-effect-count 0)
(defvar e-board-registry--unsettled-routing-count 0)
(defvar e-board-registry--unsettled-generation 0)
(defvar e-board-registry--unsettled-change-function nil)
(defvar e-board-registry--unsettled-change-functions nil)

(defun e-board-registry-unsettled-state ()
  "Return the constant-time aggregate board-core owner projection."
  (list :generation e-board-registry--unsettled-generation
        :pickups e-board-registry--unsettled-pickup-count
        :effects e-board-registry--unsettled-effect-count
        :routing e-board-registry--unsettled-routing-count))

(defun e-board-registry-add-unsettled-listener (function)
  "Subscribe FUNCTION to board unsettled-state transitions.

The registry retains its hook variable privately; consumers receive an
owner-shaped operation rather than importing that implementation detail."
  (unless (functionp function)
    (signal 'wrong-type-argument (list 'functionp function)))
  (add-hook 'e-board-registry--unsettled-change-functions function)
  function)

(defun e-board-registry-remove-unsettled-listener (function)
  "Remove FUNCTION from board unsettled-state transition listeners."
  (remove-hook 'e-board-registry--unsettled-change-functions function)
  function)

(defun e-board-registry--board-unsettled-changed (source class delta _state)
  "Aggregate one SOURCE board CLASS change by DELTA without scanning boards."
  (let ((registered (gethash (e-board-id source) e-board-registry--boards)))
    ;; A callback captured by an obsolete test/process registry generation no
    ;; longer owns aggregate state and must not decrement the current registry.
    (when (and registered
               (eq source (e-board-registry-board-source-board registered)))
      (pcase class
        ('pickups (cl-incf e-board-registry--unsettled-pickup-count delta))
        ('effects (cl-incf e-board-registry--unsettled-effect-count delta))
        ('routing (cl-incf e-board-registry--unsettled-routing-count delta)))
      (when (or (< e-board-registry--unsettled-pickup-count 0)
                (< e-board-registry--unsettled-effect-count 0)
                (< e-board-registry--unsettled-routing-count 0))
        (signal 'e-board-registry-error
                (list "Negative board aggregate" class delta)))
      (cl-incf e-board-registry--unsettled-generation)
      (when e-board-registry--unsettled-change-function
        (funcall e-board-registry--unsettled-change-function
                 (e-board-registry-unsettled-state)))
      (run-hook-with-args 'e-board-registry--unsettled-change-functions
                          (e-board-registry-unsettled-state)))))

(cl-defstruct (e-board-registry-board
                (:constructor e-board-registry-board--create)
                (:conc-name e-board-registry-board-))
  id source-board state author principal principal-grants id-function clients
  client-generations principal-clients client-revocation-head
  client-revocation-tail client-revocation-scheduled client-revocation-scheduler
  participants client-ids client-ids-tail participant-ids participant-ids-tail
  close-operation)

(cl-defstruct (e-board-registry-close-operation
               (:constructor e-board-registry-close-operation--create)
               (:conc-name e-board-registry-close-operation-))
  board request phase participants subscriptions client-ids current-client
  observer-ids scheduled)

(defconst e-board-registry-close-drain-limit 32
  "Maximum lifecycle records reconciled by one board-close callback.")

(defvar e-board-registry-close-scheduler
  (lambda (function) (run-at-time 0 nil function))
  "Function scheduling one later bounded board-close callback.")

(cl-defstruct (e-board-registry-client
                (:constructor e-board-registry-client--create)
                (:conc-name e-board-registry-client-))
  id board-id author principal role generation state observer-ids)

(cl-defstruct (e-board-registry-requester-context
               (:constructor e-board-registry-requester-context--create)
               (:conc-name e-board-registry-requester-context-))
  board-id client-id client-generation principal role)

(cl-defstruct (e-board-registry-client-revocation
               (:constructor e-board-registry-client-revocation--create)
               (:conc-name e-board-registry-client-revocation-))
  principal client-ids current-client current-observer-ids)

(cl-defstruct (e-board-registry-participant
                (:constructor e-board-registry-participant--create)
                (:conc-name e-board-registry-participant-))
  id board-id author principal controller role access-grants private-grants
  source-participant publication-pending)

(defconst e-board-registry-participant-private-rights
  '(inspect-transcript control-session)
  "Closed set of session-private participant access rights.")

(defun e-board-registry--next-id (id-function kind)
  "Return an opaque identity for KIND, using ID-FUNCTION when injected."
  (let ((id (if id-function
                 (funcall id-function kind)
              (format "%s%s"
                      (pcase kind
                        ('board "brd_")
                        ('client "cli_")
                        ('participant "ptc_")
                        ('subscription "sub_")
                        (_ (format "%s_" kind)))
                      (e-session-generate-ulid)))))
    (unless id
      (signal 'e-board-registry-error
              (list "Id generator returned nil" kind)))
    id))

(defun e-board-registry--resolve (board-or-id)
  "Return registered BOARD-OR-ID, or signal `e-board-registry-missing'."
  (let* ((id (if (e-board-registry-board-p board-or-id)
                 (e-board-registry-board-id board-or-id)
               board-or-id))
         (board (gethash id e-board-registry--boards)))
    (unless (and board
                 (or (not (e-board-registry-board-p board-or-id))
                     (eq board board-or-id)))
      (signal 'e-board-registry-missing (list id)))
    board))

(defun e-board-registry--require-active (board-or-id)
  "Return active BOARD-OR-ID, or signal `e-board-registry-closed'."
  (let ((board (e-board-registry--resolve board-or-id)))
    (unless (eq (e-board-registry-board-state board) 'active)
      (signal 'e-board-registry-closed
              (list (e-board-registry-board-id board))))
    board))

(defun e-board-registry--participant (board participant-or-id)
  "Return BOARD's canonical PARTICIPANT-OR-ID record.
Participant records must be the record created for BOARD; this prevents one
board's participant identity from being used to mutate another board."
  (let* ((id (if (e-board-registry-participant-p participant-or-id)
                 (e-board-registry-participant-id participant-or-id)
               participant-or-id))
         (participant (gethash id (e-board-registry-board-participants board))))
    (when (and (e-board-registry-participant-p participant-or-id)
               (not (equal (e-board-registry-participant-board-id participant-or-id)
                           (e-board-registry-board-id board))))
      (signal 'e-board-registry-participant-board-local (list id)))
    (unless participant
      (signal 'e-board-registry-participant-missing (list id)))
    (when (and (e-board-registry-participant-p participant-or-id)
               (not (eq participant participant-or-id)))
      (signal 'e-board-registry-participant-board-local (list id)))
    participant))

(defun e-board-registry--classification-authorized-p
    (board subscription message _phase)
  "Return non-nil while SUBSCRIPTION and MESSAGE actors retain board access."
  (let* ((target (gethash (e-board-subscription-participant-id subscription)
                          (e-board-registry-board-participants board)))
         (target-principal
          (and target (e-board-registry-participant-principal target)))
         (actor (e-board-message-requester-actor message))
         (requester-principal (e-board-registry--actor-principal board actor)))
    (when (and target
               (memq (e-board-participant-state
                      (e-board-registry-participant-source-participant target))
                     '(active dormant stale))
               (or (null target-principal)
                   (e-board-registry-principal-role board target-principal)))
      (and requester-principal
           (or (null (e-board-message-to message))
               (condition-case nil
                   (e-board-registry-authorize-actor-exact-post
                    board actor target)
                 (e-board-registry-authorization-denied nil)))))))

(defun e-board-registry--actor-principal (board actor)
  "Resolve current generation-fenced ACTOR on BOARD.
Return its board principal, or a non-string member marker for an authenticated
actor without a principal.  Return nil for missing, stale, revoked, or nil
actors.  Member markers authorize tagged broadcast only, never exact posts."
  (cond
   ((stringp actor)
    (and (e-board-registry-principal-role board actor) actor))
   ((and (listp actor) (eq (car actor) 'client))
    (let ((client (gethash (cadr actor) (e-board-registry-board-clients board))))
      (and (e-board-registry--client-authorized-p board client)
           (= (or (caddr actor) -1)
              (e-board-registry-client-generation client))
           (or (e-board-registry-client-principal client) :client-member))))
   ((and (listp actor) (eq (car actor) 'participant))
    (when-let ((source
                (gethash (cadr actor)
                         (e-board-registry-board-participants board))))
      (let ((principal (e-board-registry-participant-principal source)))
        (and (memq (e-board-participant-state
                    (e-board-registry-participant-source-participant source))
                   '(active dormant stale))
             (or (null principal)
                 (e-board-registry-principal-role board principal))
             (or principal :participant-member)))))))

(defun e-board-registry--actor-has-exact-right-p (board actor target)
  "Return non-nil when current ACTOR holds the exact right for TARGET."
  (let ((principal (e-board-registry--actor-principal board actor)))
    (cond
     ((stringp principal)
      (or (eq (e-board-registry-principal-role board principal) 'owner)
          (equal principal (e-board-registry-participant-controller target))
          (memq 'post
                (gethash principal
                         (e-board-registry-participant-access-grants target)))))
     ((eq principal :client-member)
      (and (null (e-board-registry-board-principal board))
           (null (e-board-registry-participant-controller target))))
     ((eq principal :participant-member)
      (equal (cadr actor) (e-board-registry-participant-id target))))))

(defun e-board-registry-authorize-actor-exact-post (board-or-id actor target)
  "Authorize generation-fenced ACTOR to start an exact post to TARGET.
Principals use the ordinary target-grant policy.  A principal-free client may
address a principal-free participant only on a principal-free board, and a
participant actor may address itself.  These narrow cases retain authenticated
private-board operation without treating nil as authority."
  (let* ((board (e-board-registry--require-active board-or-id))
         (target (e-board-registry--participant board target))
         (state
          (e-board-participant-state
           (e-board-registry-participant-source-participant target))))
    (unless (and (memq state '(active dormant stale))
                 (e-board-registry--actor-has-exact-right-p board actor target))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) actor
                    (e-board-registry-participant-id target)
                    (if (eq state 'detaching) 'target-unavailable 'post))))
    t))

(cl-defun e-board-registry-create
    (&key id id-function author principal client-revocation-scheduler
          storage restoring generation revision)
  "Create and register an active board with stored AUTHOR and PRINCIPAL.
ID-FUNCTION receives an identity kind and supplies all registry-owned ids.
The source board is registered with `e-board' under the same board identity."
  (let* ((id (or id (e-board-registry--next-id id-function 'board))))
    (when (= (hash-table-count e-board-registry--boards) 0)
      (setq e-board-registry--board-index
            (avl-tree-create
             (lambda (left right) (string< (car left) (car right))))
            e-board-registry--unsettled-pickup-count 0
            e-board-registry--unsettled-effect-count 0
            e-board-registry--unsettled-routing-count 0
            e-board-registry--unsettled-generation 0))
    (when (gethash id e-board-registry--boards)
      (signal 'e-board-registry-id-conflict (list id)))
    (let* ((source-board
            (e-board-create :id id :id-function id-function
                            :storage storage :trusted-principal principal
                            :restoring restoring :generation generation
                            :revision revision))
           (board (e-board-registry-board--create
                  :id id
                  :source-board source-board
                  :state 'active
                  :author author
                  :principal principal
                  :principal-grants (let ((grants (make-hash-table :test 'equal)))
                                      (when principal (puthash principal 'owner grants))
                                      grants)
                  :id-function id-function
                  :clients (make-hash-table :test 'equal)
                  :client-generations (make-hash-table :test 'equal)
                  :principal-clients (make-hash-table :test 'equal)
                  :client-revocation-scheduler client-revocation-scheduler
                  :participants (make-hash-table :test 'equal))))
      (setf (e-board-classification-authorizer source-board)
            (lambda (subscription message phase)
              (e-board-registry--classification-authorized-p
               board subscription message phase)))
      (setf (e-board-unsettled-change-function source-board)
            #'e-board-registry--board-unsettled-changed)
      (puthash id board e-board-registry--boards)
      (avl-tree-enter e-board-registry--board-index
                      (cons (format "%s" id) board))
      board)))

(defun e-board-registry-get (id)
  "Return process-local board ID, or signal `e-board-registry-missing'."
  (e-board-registry--resolve id))

(defun e-board-registry-allocate-participant-id (board-or-id)
  "Reserve one board-local participant identity for BOARD-OR-ID.

The returned opaque id is intentionally not attached yet.  Application services
may persist a complete participant policy with it before attaching the runtime
endpoint; a collision is rejected rather than silently reusing an identity."
  (let* ((board (e-board-registry--require-active board-or-id))
         (id (e-board-registry--next-id
              (e-board-registry-board-id-function board) 'participant)))
    (when (gethash id (e-board-registry-board-participants board))
      (signal 'e-board-registry-id-conflict (list id)))
    id))

(defun e-board-registry-principal-role (board-or-id principal)
  "Return PRINCIPAL's board role, or nil when it has no board grant."
  (and principal
       (gethash principal
                (e-board-registry-board-principal-grants
                 (e-board-registry--resolve board-or-id)))))

(defun e-board-registry--require-owner (board requester)
  "Signal unless REQUESTER currently owns BOARD."
  (unless (eq (e-board-registry-principal-role board requester) 'owner)
    (signal 'e-board-registry-authorization-denied
            (list (e-board-registry-board-id board) requester 'owner)))
  board)

(defun e-board-registry-authorize-participant-removal
    (board-or-id requester participant-or-id)
  "Authorize REQUESTER to remove PARTICIPANT-OR-ID from active BOARD-OR-ID.
Participant removal is board administration.  Session-private transcript and
control grants are deliberately not consulted by this operation."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--require-owner board requester)
    (e-board-registry--participant board participant-or-id)
    t))

(defun e-board-registry-authorize-participant-rebind
    (board-or-id requester participant-or-id)
  "Authorize REQUESTER to rebind PARTICIPANT-OR-ID on active BOARD-OR-ID."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--require-owner board requester)
    (e-board-registry--participant board participant-or-id)
    t))

(defun e-board-registry-authorize-participant-move
    (source-board-or-id destination-board-or-id requester participant-or-id
                        destination-participant-id)
  "Authorize REQUESTER's cross-board move of PARTICIPANT-OR-ID.
SOURCE-BOARD-OR-ID and DESTINATION-BOARD-OR-ID must name distinct active boards
currently owned by REQUESTER.  The participant's principal, when present, must
already have a destination grant, and DESTINATION-PARTICIPANT-ID must remain
unclaimed there."
  (let* ((source (e-board-registry--require-active source-board-or-id))
         (destination
          (e-board-registry--require-active destination-board-or-id))
         (participant
          (e-board-registry--participant source participant-or-id))
         (destination-id
          (or destination-participant-id
              (e-board-registry-participant-id participant)))
         (principal (e-board-registry-participant-principal participant)))
    (when (eq source destination)
      (signal 'e-board-registry-error
              (list "Participant move requires distinct boards"
                    (e-board-registry-board-id source))))
    (e-board-registry--require-owner source requester)
    (e-board-registry--require-owner destination requester)
    (when (and principal
               (null (e-board-registry-principal-role destination principal)))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id destination) principal
                    'participant)))
    (when (gethash destination-id
                   (e-board-registry-board-participants destination))
      (signal 'e-board-registry-id-conflict (list destination-id)))
    destination-id))

(defun e-board-registry--owner-count (board)
  "Return the number of current owner grants on BOARD.
Administrative grant changes may scan this small registry-owned table; hot
message routing never consults it."
  (let ((count 0))
    (maphash (lambda (_principal role)
               (when (eq role 'owner) (cl-incf count)))
             (e-board-registry-board-principal-grants board))
    count))

(defun e-board-registry--schedule-client-revocation-drain (board)
  "Schedule one bounded revoked-client cleanup drain for BOARD."
  (unless (e-board-registry-board-client-revocation-scheduled board)
    (setf (e-board-registry-board-client-revocation-scheduled board) t)
    (if-let ((scheduler
              (e-board-registry-board-client-revocation-scheduler board)))
        (funcall scheduler
                 (lambda ()
                   (e-board-registry--drain-client-revocations board)))
      (run-at-time
       0 nil (lambda () (e-board-registry--drain-client-revocations board))))))

(defun e-board-registry--queue-client-revocation (board principal client-ids)
  "Queue PRINCIPAL's CLIENT-IDS for bounded generation fencing and closure."
  (when client-ids
    (let* ((job
            (e-board-registry-client-revocation--create
             :principal principal :client-ids client-ids))
           (cell (list job)))
      (if (e-board-registry-board-client-revocation-tail board)
          (setcdr (e-board-registry-board-client-revocation-tail board) cell)
        (setf (e-board-registry-board-client-revocation-head board) cell))
      (setf (e-board-registry-board-client-revocation-tail board) cell)
      (e-board-registry--schedule-client-revocation-drain board))))

(defun e-board-registry--drain-client-revocations (board)
  "Commit one bounded page of revoked client and observer cleanup for BOARD."
  (setf (e-board-registry-board-client-revocation-scheduled board) nil)
  (let ((remaining e-board-registry-client-revocation-drain-limit))
    (while (and (> remaining 0)
                (e-board-registry-board-client-revocation-head board))
      (let* ((job (car (e-board-registry-board-client-revocation-head board)))
             (client (e-board-registry-client-revocation-current-client job)))
        (cond
         ((e-board-registry-client-revocation-current-observer-ids job)
          (let ((observer-id
                 (pop (e-board-registry-client-revocation-current-observer-ids job))))
            (when-let ((observer
                        (e-board-observer
                         (e-board-registry-board-source-board board) observer-id)))
              (when (memq (e-board-observer-state observer) '(active muted))
                (e-board-set-observer-state
                 (e-board-registry-board-source-board board)
                 observer-id 'cancelled)))))
         (client
          (let* ((client-id (e-board-registry-client-id client))
                 (generation
                  (1+ (gethash client-id
                               (e-board-registry-board-client-generations board)
                               0))))
            (setf (e-board-registry-client-state client) 'detached
                  (e-board-registry-client-observer-ids client) nil
                  (e-board-registry-client-generation client) generation
                  (e-board-registry-client-revocation-current-client job) nil)
            (puthash client-id generation
                     (e-board-registry-board-client-generations board))
            (remhash client-id (e-board-registry-board-clients board))))
         ((e-board-registry-client-revocation-client-ids job)
          (let* ((client-id
                  (pop (e-board-registry-client-revocation-client-ids job)))
                 (next
                  (gethash client-id (e-board-registry-board-clients board))))
            (when (and next
                       (equal (e-board-registry-client-principal next)
                              (e-board-registry-client-revocation-principal job)))
              (setf (e-board-registry-client-revocation-current-client job) next
                    (e-board-registry-client-revocation-current-observer-ids job)
                    (e-board-registry-client-observer-ids next)
                    (e-board-registry-client-observer-ids next) nil))))
         (t
          (setf (e-board-registry-board-client-revocation-head board)
                (cdr (e-board-registry-board-client-revocation-head board)))
          (unless (e-board-registry-board-client-revocation-head board)
            (setf (e-board-registry-board-client-revocation-tail board) nil))))
        (cl-decf remaining)))
    (when (e-board-registry-board-client-revocation-head board)
      (e-board-registry--schedule-client-revocation-drain board))))

(defun e-board-registry-authorize-principal (board-or-id requester principal role)
  "Grant PRINCIPAL the board ROLE when REQUESTER is a current owner.
ROLE is either `owner' or `member'.  This application operation owns board
grant mutation; message routing remains independent of its grant table."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--require-owner board requester)
    (unless (and principal (memq role '(owner member)))
      (signal 'wrong-type-argument (list '(member owner member) role)))
    (puthash principal role (e-board-registry-board-principal-grants board))
    role))

(defun e-board-registry-revoke-principal (board-or-id requester principal)
  "Revoke PRINCIPAL's grant when REQUESTER is an owner.
The registry refuses to remove its last owner, preserving an authenticated
controller for future board lifecycle operations."
  (let* ((board (e-board-registry--require-active board-or-id))
         (grants (e-board-registry-board-principal-grants board))
         (role (gethash principal grants)))
    (e-board-registry--require-owner board requester)
    (when (and (eq role 'owner) (= (e-board-registry--owner-count board) 1))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) principal 'last-owner)))
    (remhash principal grants)
    (let ((client-ids
           (gethash principal
                    (e-board-registry-board-principal-clients board))))
      (remhash principal (e-board-registry-board-principal-clients board))
      (e-board-registry--queue-client-revocation board principal client-ids))
    role))

(defun e-board-registry-list ()
  "Return all process-local boards sorted by printable identity."
  (let (boards)
    (maphash (lambda (_id board) (push board boards)) e-board-registry--boards)
    (sort boards (lambda (left right)
                   (string< (format "%s" (e-board-registry-board-id left))
                            (format "%s" (e-board-registry-board-id right)))))))

(cl-defun e-board-registry-list-page
    (&key after (limit e-board-registry-list-page-limit))
  "Return one bounded board-registry page after opaque board identity AFTER.
The page is ordered by the registry's stable printable board identity and
returns =:next-after= only when another page exists.  The scan retains at most
LIMIT plus one candidates, rather than materializing the full registry list."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'plusp limit)))
  (let ((after-key (and after (format "%s" after)))
        cursor entries exhausted)
    (dotimes (_index (1+ limit))
      (unless exhausted
        (let ((node (avl-tree--root e-board-registry--board-index))
              candidate
              (seek (or cursor after-key)))
          (while node
            (let* ((entry (avl-tree--node-data node))
                   (key (car entry)))
              (if (or (null seek) (string< seek key))
                  (setq candidate entry
                        node (avl-tree--node-left node))
                (setq node (avl-tree--node-right node)))))
          (if candidate
              (progn
                (push candidate entries)
                (setq cursor (car candidate)))
            (setq exhausted t)))))
    (setq entries (nreverse entries))
    (let* ((more (> (length entries) limit))
           (page-entries (if more (butlast entries) entries))
           (boards (mapcar #'cdr page-entries)))
      (list :boards boards
            :next-after
            (and more (e-board-registry-board-id (car (last boards))))))))

(defun e-board-registry-participant (board-or-id participant-or-id)
  "Return BOARD-OR-ID's canonical board-local participant record."
  (e-board-registry--participant (e-board-registry--resolve board-or-id)
                                 participant-or-id))

(cl-defun e-board-registry-attach-client
    (board-or-id &key id author principal)
  "Attach a client to active BOARD-OR-ID and return its local record."
  (let* ((board (e-board-registry--require-active board-or-id))
         (id (or id (e-board-registry--next-id
                     (e-board-registry-board-id-function board) 'client)))
         (role (and principal (e-board-registry-principal-role board principal))))
    (when (and principal (not role))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) principal 'board-read)))
    (when (gethash id (e-board-registry-board-clients board))
      (signal 'e-board-registry-id-conflict (list id)))
    (let* ((generations (e-board-registry-board-client-generations board))
           (generation (1+ (gethash id generations 0)))
           (client (e-board-registry-client--create
                    :id id :board-id (e-board-registry-board-id board)
                    :author author :principal principal :role role
                    :generation generation :state 'active :observer-ids nil)))
      (puthash id generation generations)
      (puthash id client (e-board-registry-board-clients board))
      (let ((cell (list id)))
        (if-let ((tail (e-board-registry-board-client-ids-tail board)))
            (setcdr tail cell)
          (setf (e-board-registry-board-client-ids board) cell))
        (setf (e-board-registry-board-client-ids-tail board) cell))
      (when principal
        (puthash principal
                 (cons id
                       (gethash principal
                                (e-board-registry-board-principal-clients board)))
                 (e-board-registry-board-principal-clients board)))
      client)))

(defun e-board-registry--client-authorized-p (board client)
  "Return non-nil while CLIENT is active under its current board grant."
  (and client
       (eq (e-board-registry-client-state client) 'active)
       (or (null (e-board-registry-client-principal client))
           (eq (e-board-registry-principal-role
                board (e-board-registry-client-principal client))
               (e-board-registry-client-role client)))))

(defun e-board-registry--detach-client-object (board client)
  "Detach the exact current CLIENT object from BOARD.
The caller has already established object and generation ownership.  This
helper also removes empty principal catalog entries so a terminal lease does
not leave a nil-valued registry key behind."
  (dolist (observer-id (e-board-registry-client-observer-ids client))
    (when-let ((observer (e-board-observer
                          (e-board-registry-board-source-board board)
                          observer-id)))
      (when (memq (e-board-observer-state observer) '(active muted))
        (e-board-set-observer-state
         (e-board-registry-board-source-board board) observer-id 'cancelled))))
  (let ((client-id (e-board-registry-client-id client)))
    (setf (e-board-registry-client-observer-ids client) nil
          (e-board-registry-client-state client) 'detached)
    (when-let ((principal (e-board-registry-client-principal client)))
      (let ((remaining
             (delete client-id
                     (gethash principal
                              (e-board-registry-board-principal-clients board)))))
        (if remaining
            (puthash principal remaining
                     (e-board-registry-board-principal-clients board))
          (remhash principal
                   (e-board-registry-board-principal-clients board)))))
    (remhash client-id (e-board-registry-board-clients board)))
  client)

(defun e-board-registry-detach-client (board-or-id client-id)
  "Detach CLIENT-ID from active BOARD-OR-ID and release its observers.
This id-based compatibility operation resolves the current client first;
owner teardown that retained a client object must use
`e-board-registry-detach-client-exact' so a replacement generation is never
detached accidentally."
  (let* ((board (e-board-registry--require-active board-or-id))
         (client (gethash client-id
                          (e-board-registry-board-clients board))))
    (when client
      (e-board-registry--detach-client-object board client))))

(defun e-board-registry-detach-client-exact (board-or-id client)
  "Detach exactly CLIENT from BOARD-OR-ID, or do nothing if it was replaced.
CLIENT object identity and its current generation fence the operation.  A
board object may be supplied after it entered `closing' or `closed'; this is a
terminal owner operation and therefore does not reopen board admission."
  (unless (e-board-registry-client-p client)
    (signal 'wrong-type-argument
            (list 'e-board-registry-client-p client)))
  (let* ((board (if (e-board-registry-board-p board-or-id)
                    board-or-id
                  (e-board-registry--resolve board-or-id)))
         (client-id (e-board-registry-client-id client))
         (current (gethash client-id
                           (e-board-registry-board-clients board)))
         (generation (gethash client-id
                              (e-board-registry-board-client-generations board))))
    (when (and (eq current client)
               (= (e-board-registry-client-generation client)
                  (or generation 0)))
      (e-board-registry--detach-client-object board client))))

(defun e-board-registry-client-requester-context (board-or-id client-id)
  "Capture active CLIENT-ID as a generation-fenced requester context."
  (let* ((board (e-board-registry--require-active board-or-id))
         (client (gethash client-id (e-board-registry-board-clients board))))
    (unless (e-board-registry--client-authorized-p board client)
      (signal 'e-board-registry-client-missing (list client-id)))
    (e-board-registry-requester-context--create
     :board-id (e-board-registry-board-id board)
     :client-id client-id
     :client-generation (e-board-registry-client-generation client)
     :principal (e-board-registry-client-principal client)
     :role (e-board-registry-client-role client))))

(defun e-board-registry-resolve-requester-actor (board-or-id context)
  "Return CONTEXT's generation-fenced actor while its client is active."
  (let* ((board (e-board-registry--require-active board-or-id))
         (client-id (and (e-board-registry-requester-context-p context)
                         (e-board-registry-requester-context-client-id context)))
         (client (and client-id
                      (gethash client-id (e-board-registry-board-clients board)))))
    (unless (and (e-board-registry--client-authorized-p board client)
                 (equal (e-board-registry-requester-context-board-id context)
                        (e-board-registry-board-id board))
                 (= (e-board-registry-requester-context-client-generation context)
                    (e-board-registry-client-generation client))
                 (equal (e-board-registry-requester-context-principal context)
                        (e-board-registry-client-principal client))
                 (eq (e-board-registry-requester-context-role context)
                     (e-board-registry-client-role client)))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) client-id 'stale-requester)))
    (list 'client client-id (e-board-registry-client-generation client))))

(cl-defun e-board-registry-install-observer
    (board-or-id client-id selector &key id start-seq
                 history-before-seq (history-floor 0))
  "Install an effect-free board observer owned by attached CLIENT-ID.
The registry validates board-local client ownership; the source board retains
the cursor and selector because it owns ordered message observation."
  (let* ((board (e-board-registry--require-active board-or-id))
         (client (gethash client-id (e-board-registry-board-clients board))))
    (unless client
      (signal 'e-board-registry-client-missing (list client-id)))
    (unless (e-board-registry--client-authorized-p board client)
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) client-id 'board-read)))
    (let ((observer
            (e-board-observer-subscribe
            (e-board-registry-board-source-board board) client-id selector
            :id (or id (e-board-registry--next-id
                        (e-board-registry-board-id-function board) 'observer))
            :client-generation (e-board-registry-client-generation client)
            :start-seq start-seq
            :history-before-seq history-before-seq
            :history-floor history-floor)))
      (setf (e-board-registry-client-observer-ids client)
            (append (e-board-registry-client-observer-ids client)
                    (list (e-board-observer-id observer))))
      observer)))

(defun e-board-registry--observer-for-client (board client-id observer-id)
  "Return BOARD OBSERVER-ID after validating its attached owning CLIENT-ID."
  (let ((client (gethash client-id (e-board-registry-board-clients board))))
    (unless client
      (signal 'e-board-registry-client-missing (list client-id)))
    (unless (e-board-registry--client-authorized-p board client)
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) client-id 'board-read)))
    (let ((observer (or (e-board-observer
                         (e-board-registry-board-source-board board) observer-id)
                        (signal 'e-board-observer-missing (list observer-id)))))
      (unless (and (equal (e-board-observer-client-id observer) client-id)
                   (= (or (e-board-observer-client-generation observer) 0)
                      (e-board-registry-client-generation client)))
        (signal 'e-board-registry-error
                (list "Observer belongs to another client generation"
                      observer-id client-id)))
      observer)))

(cl-defun e-board-registry-replace-observer
    (board-or-id client-id observer-id selector &key id (state 'active) start-seq)
  "Replace attached CLIENT-ID's OBSERVER-ID with a fresh authorized cursor.
The source board owns the old cursor cancellation and the new cursor's
explicit START-SEQ backfill semantics; this registry only validates client
ownership before delegating that ordered transition."
  (let ((board (e-board-registry--require-active board-or-id)))
    (let* ((observer (e-board-registry--observer-for-client
                      board client-id observer-id))
           (client (gethash client-id (e-board-registry-board-clients board)))
           (replacement
            (e-board-replace-observer
             (e-board-registry-board-source-board board) observer-id selector
             :id (or id (e-board-registry--next-id
                         (e-board-registry-board-id-function board) 'observer))
             :state state :start-seq start-seq)))
      (setf (e-board-registry-client-observer-ids client)
            (append (delete (e-board-observer-id observer)
                            (e-board-registry-client-observer-ids client))
                    (list (e-board-observer-id replacement))))
      replacement)))

(defun e-board-registry-set-observer-state
    (board-or-id client-id observer-id state)
  "Transition attached CLIENT-ID's OBSERVER-ID to STATE.
The board core validates the observer lifecycle; the registry prevents one
attached client from muting, resuming, or closing another client's cursor."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--observer-for-client board client-id observer-id)
    (e-board-set-observer-state
     (e-board-registry-board-source-board board) observer-id state)))

(cl-defun e-board-registry-prepare-observer-page
    (board-or-id client-id observer-id &key (limit 32))
  "Prepare OBSERVER-ID's page for attached CLIENT-ID without cursor advance."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--observer-for-client board client-id observer-id)
    (e-board-observer-prepare-page
     (e-board-registry-board-source-board board) observer-id :limit limit)))

(defun e-board-registry-accept-observer-page
    (board-or-id client-id observer-id receipt)
  "Record attached CLIENT-ID's exact accepted observer page RECEIPT."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--observer-for-client board client-id observer-id)
    (e-board-observer-accept-page
     (e-board-registry-board-source-board board) observer-id receipt)))

(cl-defun e-board-registry-prepare-observer-history-page
    (board-or-id client-id observer-id &key (limit 32))
  "Prepare an attached CLIENT-ID's reverse observer page without advance."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--observer-for-client board client-id observer-id)
    (e-board-observer-prepare-history-page
     (e-board-registry-board-source-board board) observer-id :limit limit)))

(defun e-board-registry-accept-observer-history-page
    (board-or-id client-id observer-id receipt)
  "Record attached CLIENT-ID's exact accepted history page RECEIPT."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--observer-for-client board client-id observer-id)
    (e-board-observer-accept-history-page
     (e-board-registry-board-source-board board) observer-id receipt)))

(cl-defun e-board-registry-add-participant
    (board-or-id &key id author principal controller (state 'active)
                 subscription-id (publish-event t))
  "Add a board-local participant to active BOARD-OR-ID.
The participant's built-in exact address subscription is created by the source
board, with its identity supplied by this registry's id generator."
  (let* ((board (e-board-registry--require-active board-or-id))
         (id-function (e-board-registry-board-id-function board))
         (id (or id (e-board-registry--next-id id-function 'participant)))
         (subscription-id
          (or subscription-id
              (e-board-registry--next-id id-function 'subscription)))
         (participants (e-board-registry-board-participants board))
         (role (and principal (e-board-registry-principal-role board principal))))
    (when (and principal (not role))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) principal 'participant)))
    (when (gethash id participants)
      (signal 'e-board-registry-id-conflict (list id)))
    (e-board-persist-participant
     (e-board-registry-board-source-board board)
     (list :id id :author author :principal principal
           :controller (or controller principal) :role role :state state
           :subscription-id subscription-id
           :publication-pending (not publish-event)))
    (let* ((source-participant
            (e-board-add-participant
             (e-board-registry-board-source-board board)
             :id id
             :state state
             :create-pickup-subscription-id subscription-id
             :publish-event publish-event))
           (participant
            (e-board-registry-participant--create
             :id id :board-id (e-board-registry-board-id board)
             :author author :principal principal
             :controller (or controller principal) :role role
             :access-grants (make-hash-table :test 'equal)
            :private-grants
             (let ((grants (make-hash-table :test 'equal)))
               (when-let* ((controller (or controller principal)))
                 (puthash controller
                          (copy-sequence
                           e-board-registry-participant-private-rights)
                          grants))
               grants)
             :source-participant source-participant
             :publication-pending (not publish-event))))
      (puthash id participant participants)
      (let ((cell (list id)))
        (if-let* ((tail (e-board-registry-board-participant-ids-tail board)))
            (setcdr tail cell)
          (setf (e-board-registry-board-participant-ids board) cell))
        (setf (e-board-registry-board-participant-ids-tail board) cell))
      participant)))

(defun e-board-registry-publish-participant-admission
    (board-or-id participant)
  "Publish the deferred participant-added event for PARTICIPANT.
The registry participant and its source-board pickup route already exist, but
their durable board event remains unpublished until the owning admission
transaction has committed its session declaration.  This operation is
idempotent for an already-published participant and rejects foreign records."
  (let* ((board (e-board-registry--require-active board-or-id))
         (current (e-board-registry--participant board participant))
         (source-board (e-board-registry-board-source-board board))
         (source-participant
          (e-board-registry-participant-source-participant current)))
    (when (e-board-registry-participant-publication-pending current)
      (unless (e-board-participant source-board
                                   (e-board-participant-id source-participant))
        (signal 'e-board-registry-participant-missing
                (list (e-board-registry-participant-id current))))
      (e-board-publish-persisted-participant
       source-board (e-board-participant-id source-participant))
      (e-board-admission-append-event
       source-board 'participant-added
       (list :participant-id
             (e-board-participant-id source-participant)
             :subscription-id
             (e-board-participant-create-pickup-subscription-id
              source-participant)))
      (setf (e-board-registry-participant-publication-pending current) nil))
    current))

(defun e-board-registry-abort-participant-admission (board-or-id participant)
  "Rollback unpublished PARTICIPANT admission without a board event.
This is the registry/runtime transaction cleanup path and must only be used
before the participant has been exposed to board traffic."
  (let* ((board (e-board-registry--resolve board-or-id))
         (participant-id (if (e-board-registry-participant-p participant)
                            (e-board-registry-participant-id participant)
                          participant))
         (current (and participant-id
                       (gethash participant-id
                                (e-board-registry-board-participants board))))
         (source-board (e-board-registry-board-source-board board)))
    (when (and current (or (eq current participant)
                           (not (e-board-registry-participant-p participant))))
      ;; The participant identity committed before any later session/runtime
      ;; admission work.  Remove that unpublished durable projection first so
      ;; an ordinary failed admission cannot wedge exact retry or reappear.
      (e-board-abort-persisted-participant source-board participant-id)
      (remhash participant-id (e-board-registry-board-participants board))
      (setf (e-board-registry-board-participant-ids board)
            (delete participant-id
                    (e-board-registry-board-participant-ids board))
            (e-board-registry-board-participant-ids-tail board)
            (last (e-board-registry-board-participant-ids board)))
      (e-board-abort-participant-admission source-board participant-id))
    current))

(defun e-board-registry-grant-participant-access
    (board-or-id requester participant-or-id principal rights)
  "Grant PRINCIPAL target PARTICIPANT-OR-ID RIGHTS as a board owner."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id)))
    (e-board-registry--require-owner board requester)
    (unless (and principal (listp rights) rights)
      (signal 'wrong-type-argument (list 'listp rights)))
    (puthash principal (copy-sequence rights)
             (e-board-registry-participant-access-grants participant))
    rights))

(defun e-board-registry-revoke-participant-access
    (board-or-id requester participant-or-id principal)
  "Revoke PRINCIPAL's explicit target-participant access as a board owner."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (grants (e-board-registry-participant-access-grants participant))
         (rights (gethash principal grants)))
    (e-board-registry--require-owner board requester)
    (remhash principal grants)
    rights))

(defun e-board-registry-participant-access-rights
    (board-or-id participant-or-id principal)
  "Return PRINCIPAL's explicit immutable access rights for PARTICIPANT-OR-ID."
  (let* ((board (e-board-registry--resolve board-or-id))
         (participant (e-board-registry--participant board participant-or-id)))
    (copy-sequence
     (gethash principal (e-board-registry-participant-access-grants participant)))))

(defun e-board-registry--require-participant-controller
    (participant requester)
  "Signal unless REQUESTER is PARTICIPANT's durable controlling principal."
  (unless (and requester
               (equal requester
                      (e-board-registry-participant-controller participant)))
    (signal 'e-board-registry-authorization-denied
            (list (e-board-registry-participant-board-id participant)
                  requester (e-board-registry-participant-id participant)
                  'manage-private-access))))

(defun e-board-registry-grant-participant-private-access
    (board-or-id requester participant-or-id principal rights)
  "Grant PRINCIPAL private session RIGHTS as PARTICIPANT's controller."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id)))
    (e-board-registry--require-participant-controller participant requester)
    (unless (and principal (listp rights) rights
                 (cl-every (lambda (right)
                             (memq right
                                   e-board-registry-participant-private-rights))
                           rights))
      (signal 'wrong-type-argument
              (list 'participant-private-rights rights)))
    (puthash principal (copy-sequence rights)
             (e-board-registry-participant-private-grants participant))
    rights))

(defun e-board-registry-revoke-participant-private-access
    (board-or-id requester participant-or-id principal)
  "Revoke PRINCIPAL's private session rights as PARTICIPANT's controller."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (grants (e-board-registry-participant-private-grants participant))
         (rights (gethash principal grants)))
    (e-board-registry--require-participant-controller participant requester)
    (when (equal principal (e-board-registry-participant-controller participant))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) principal
                    (e-board-registry-participant-id participant)
                    'controller-private-access)))
    (remhash principal grants)
    rights))

(defun e-board-registry-participant-private-access-rights
    (board-or-id participant-or-id principal)
  "Return immutable private session rights for PRINCIPAL on PARTICIPANT-OR-ID."
  (let* ((board (e-board-registry--resolve board-or-id))
         (participant (e-board-registry--participant board participant-or-id)))
    (copy-sequence
     (gethash principal
              (e-board-registry-participant-private-grants participant)))))

(defun e-board-registry-authorize-participant-private-access
    (board-or-id requester participant-or-id right)
  "Authorize REQUESTER's private RIGHT on PARTICIPANT-OR-ID."
  (unless (memq right e-board-registry-participant-private-rights)
    (signal 'wrong-type-argument
            (list 'participant-private-right right)))
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (rights (gethash requester
                          (e-board-registry-participant-private-grants
                           participant))))
    (unless (memq right rights)
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) requester
                    (e-board-registry-participant-id participant) right)))
    t))

(defun e-board-registry-authorize-exact-post
    (board-or-id requester participant-or-id)
  "Authorize REQUESTER to post exactly to PARTICIPANT-OR-ID.
Owners may address every participant.  A participant's own controlling
principal may address itself; all other exact posts require the target's
explicit `post' grant."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (state
          (e-board-participant-state
           (e-board-registry-participant-source-participant participant)))
         (rights (gethash requester
                          (e-board-registry-participant-access-grants participant))))
    (unless (and (memq state '(active dormant stale))
                 (or (eq (e-board-registry-principal-role board requester) 'owner)
                     (equal requester
                            (e-board-registry-participant-controller participant))
                     (memq 'post rights)))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) requester
                    (e-board-registry-participant-id participant)
                    (if (eq state 'detaching) 'target-unavailable 'post))))
    t))

(defun e-board-registry--delivery-requester-authorized-p
    (board actor target addressed-p)
  "Return non-nil while frozen ACTOR may still deliver to TARGET.
ADDRESSED-P requires the target-owned exact-post grant in addition to current
board membership.  Nil and stale actors are always revoked."
  (let ((principal (e-board-registry--actor-principal board actor)))
    (and principal
         (or (not addressed-p)
             (e-board-registry--actor-has-exact-right-p
              board actor target)))))

(defun e-board-registry-participant-delivery-authorization
    (board-or-id participant-or-id &optional requester-actor addressed-p)
  "Return PARTICIPANT-OR-ID's current physical-delivery authorization state.
`authorized' permits a new endpoint attempt.  `waiting' preserves a dormant,
stale, or detaching member's bounded FIFO without calling a harness.  `revoked'
requires an explicit pickup tombstone and includes removed membership or a
missing current principal grant."
  (let* ((board (e-board-registry--resolve board-or-id))
         (participant
          (if (e-board-registry-participant-p participant-or-id)
              participant-or-id
            (gethash participant-or-id
                     (e-board-registry-board-participants board))))
         (current
          (and participant
               (gethash (e-board-registry-participant-id participant)
                        (e-board-registry-board-participants board))))
         (principal
          (and participant (e-board-registry-participant-principal participant)))
         (state
          (and participant
               (e-board-participant-state
                (e-board-registry-participant-source-participant participant)))))
    (cond
     ((or (null participant) (not (eq current participant))) 'revoked)
     ((and principal (null (e-board-registry-principal-role board principal)))
      'revoked)
     ((and requester-actor
           (not (e-board-registry--delivery-requester-authorized-p
                 board requester-actor participant addressed-p)))
      'revoked)
     ((eq state 'active) 'authorized)
     ((memq state '(detaching dormant stale)) 'waiting)
     (t 'revoked))))

(defun e-board-registry-remove-participant (board-or-id participant-or-id)
  "Remove PARTICIPANT-OR-ID from active BOARD-OR-ID and disable its routes."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (participant-id (e-board-registry-participant-id participant))
         (source-board (e-board-registry-board-source-board board)))
    (setf (e-board-participant-state
           (e-board-registry-participant-source-participant participant))
          'removed)
    (dolist (subscription (e-board-subscriptions source-board))
      (when (equal (e-board-subscription-participant-id subscription) participant-id)
        (setf (e-board-subscription-state subscription) 'inactive)))
    (remhash participant-id (e-board-registry-board-participants board))
    (e-board-admission-append-event
     source-board 'participant-removed (list :participant-id participant-id))
    participant))

(defun e-board-registry-retire-participant-exact (board-or-id participant)
  "Retire exactly PARTICIPANT and all of its source-board routes.
This terminal process-local operation accepts an already-held BOARD object
while it is active, closing, or closed.  It removes only the exact registry
participant and source participant objects.  Ordinary-route transition,
timer, classifier, replay, activation, and effect fencing belongs to the
source board's `e-board-retire-subscription-exact' operation; this registry
owner only coordinates exact participant membership after every route has
retired.  It emits no participant-removal event; callers that need the
durable participant lifecycle must use `e-board-registry-remove-participant'.
Returning nil means the participant was already replaced or absent."
  (unless (e-board-registry-participant-p participant)
    (signal 'wrong-type-argument
            (list 'e-board-registry-participant-p participant)))
  (let* ((board (if (e-board-registry-board-p board-or-id)
                    board-or-id
                  (e-board-registry--resolve board-or-id)))
         (participant-id (e-board-registry-participant-id participant))
         (participants (e-board-registry-board-participants board))
         (current (gethash participant-id participants))
         (source-board (e-board-registry-board-source-board board))
         (source-participant
          (e-board-registry-participant-source-participant participant))
         (source-current
          (and source-participant
               (e-board-participant source-board
                                    (e-board-participant-id
                                     source-participant))))
         (owned-p
          (and (or (eq current participant)
                   (eq source-current source-participant))
               (or (null current) (eq current participant))
               (or (null source-current)
                   (eq source-current source-participant)))))
    (when owned-p
      ;; Keep both membership maps authoritative until the board owner has
      ;; completed every route.  If a lower owner signals halfway through,
      ;; the exact participant and all remaining subscription objects remain
      ;; discoverable and a later retry can continue without an id fallback.
      (when (or source-current (eq current participant))
        (dolist (subscription
                 (copy-sequence (e-board-subscriptions source-board)))
          (when (equal (e-board-subscription-participant-id subscription)
                       participant-id)
            ;; A durable subscription id may have been replaced while the
            ;; participant object stayed current.  Retire the current route
            ;; object, not only the historical object in this ordered list;
            ;; otherwise participant teardown would leave that replacement
            ;; route indexed and live after membership removal.
            (let ((current-subscription
                   (e-board-find-subscription
                    source-board (e-board-subscription-id subscription))))
              (when (and current-subscription
                         (equal
                          (e-board-subscription-participant-id
                           current-subscription)
                          participant-id))
                (e-board-retire-subscription-exact
                 source-board current-subscription))))))
      ;; Membership is removed only after all source routes have reached their
      ;; terminal owner state.  The operation is therefore retryable even if a
      ;; route-level retirement was interrupted by an injected failure.
      (when (eq source-current source-participant)
        (remhash (e-board-participant-id source-participant)
                 (e-board-participants source-board))
        (setf (e-board-participant-state source-participant) 'removed))
      (when (eq current participant)
        (remhash participant-id participants))
      (setf (e-board-registry-participant-publication-pending participant)
            nil)
      participant)))

(defun e-board-registry-move-participant
    (source-board-or-id destination-board-or-id requester participant-or-id
                        destination-participant-id)
  "Move PARTICIPANT-OR-ID from SOURCE-BOARD-OR-ID to DESTINATION-BOARD-OR-ID.
Authorize REQUESTER and require DESTINATION-PARTICIPANT-ID to be free.  The
destination receives a fresh board-local participant record with the source
author, principal, and controller.  Cross-target and third-party private grants
remain source-board facts and are deliberately not copied."
  (let* ((source (e-board-registry--require-active source-board-or-id))
         (destination
          (e-board-registry--require-active destination-board-or-id))
         (participant
          (e-board-registry--participant source participant-or-id))
         (destination-id
          (e-board-registry-authorize-participant-move
           source destination requester participant destination-participant-id))
         (moved
          (e-board-registry-add-participant
           destination :id destination-id
           :author (e-board-registry-participant-author participant)
           :principal (e-board-registry-participant-principal participant)
           :controller (e-board-registry-participant-controller participant))))
    (e-board-registry-remove-participant source participant)
    moved))

(defun e-board-registry-set-participant-state
    (board-or-id participant-or-id state)
  "Transition a local participant to active, detaching, dormant, or stale STATE.
These nonterminal membership states retain the participant's exact address
subscription so queued/recoverable delivery can be reconciled by the runtime.
Removal remains the terminal operation in `e-board-registry-remove-participant'."
  (unless (memq state '(active detaching dormant stale))
    (signal 'wrong-type-argument
            (list '(member active detaching dormant stale) state)))
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (source-board (e-board-registry-board-source-board board))
         (source-participant
          (e-board-registry-participant-source-participant participant))
         (from (e-board-participant-state source-participant)))
    (when (eq from state)
      (signal 'e-board-registry-error
              (list "Participant already in requested state" state)))
    (setf (e-board-participant-state source-participant) state)
    (e-board-admission-append-event
     source-board
     (pcase state
       ('active 'participant-rebound)
       ('detaching 'participant-detaching)
       ('dormant 'participant-dormant)
       ('stale 'participant-stale))
     (list :participant-id (e-board-registry-participant-id participant)
           :from from :state state))
    participant))

(defun e-board-registry-activate-restored-participant
    (board-or-id participant-or-id)
  "Make a canonical dormant PARTICIPANT-OR-ID locally deliverable.
SQLite restoration deliberately hydrates durable participant identity as
`dormant' until its process-local endpoint has been reattached.  This narrow
restart operation changes only that local availability projection; it emits no
new Board fact and does not republish the already committed identity."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (source-participant
          (e-board-registry-participant-source-participant participant)))
    (unless (eq (e-board-participant-state source-participant) 'dormant)
      (signal 'e-board-registry-error
              (list "Restored participant is not dormant"
                    (e-board-registry-participant-id participant))))
    (setf (e-board-participant-state source-participant) 'active)
    participant))

(cl-defun e-board-registry-install-subscription
    (board-or-id participant-or-id selector &key id (state 'active)
                (effect 'create-pickup) (delivery 'normal) priority
                (self-delivery nil) failure-policy)
  "Install an ordinary source-board subscription for a local participant."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (id (or id (e-board-registry--next-id
                     (e-board-registry-board-id-function board) 'subscription))))
    (e-board-subscribe (e-board-registry-board-source-board board)
                       (e-board-registry-participant-id participant)
                       selector :id id :state state :effect effect
                       :delivery delivery :priority priority
                       :self-delivery self-delivery
                       :failure-policy failure-policy)))

(defun e-board-registry-mute-subscription (board-or-id subscription-id)
  "Mute ordinary SUBSCRIPTION-ID on active BOARD-OR-ID."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-set-subscription-state
     (e-board-registry-board-source-board board) subscription-id 'muted)))

(defun e-board-registry-resume-subscription (board-or-id subscription-id)
  "Resume ordinary SUBSCRIPTION-ID on active BOARD-OR-ID for future inputs."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-set-subscription-state
     (e-board-registry-board-source-board board) subscription-id 'active)))

(defun e-board-registry-cancel-subscription (board-or-id subscription-id)
  "Cancel ordinary SUBSCRIPTION-ID on active BOARD-OR-ID permanently."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-set-subscription-state
     (e-board-registry-board-source-board board) subscription-id 'cancelled)))

(defun e-board-registry-expire-subscription (board-or-id subscription-id)
  "Expire ordinary SUBSCRIPTION-ID on active BOARD-OR-ID permanently."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-set-subscription-state
     (e-board-registry-board-source-board board) subscription-id 'expired)))

(cl-defun e-board-registry-replace-subscription
    (board-or-id subscription-id selector &key id (effect nil effect-supplied-p)
                 (delivery nil delivery-supplied-p)
                 (priority nil priority-supplied-p)
                 (self-delivery nil self-delivery-supplied-p)
                 (failure-policy nil failure-policy-supplied-p)
                 (state 'active))
  "Replace SUBSCRIPTION-ID with SELECTOR on active BOARD-OR-ID.
The core retains omitted subscription policy fields; this registry supplies
only the board-local replacement identity."
  (let* ((board (e-board-registry--require-active board-or-id))
         (arguments
          (list :id (or id (e-board-registry--next-id
                            (e-board-registry-board-id-function board)
                            'subscription))
                :state state)))
    (dolist (field `((,effect-supplied-p :effect ,effect)
                     (,delivery-supplied-p :delivery ,delivery)
                     (,priority-supplied-p :priority ,priority)
                     (,self-delivery-supplied-p :self-delivery ,self-delivery)
                     (,failure-policy-supplied-p :failure-policy ,failure-policy)))
      (when (car field)
        (setq arguments (append arguments (cdr field)))))
    (apply #'e-board-replace-subscription
           (e-board-registry-board-source-board board)
           subscription-id selector arguments)))

(defun e-board-registry--close-unsettled-p (source)
  "Return non-nil when SOURCE still owns accepted nonterminal work."
  (let ((state (e-board-unsettled-state source)))
    (> (+ (plist-get state :pickups)
          (plist-get state :effects)
          (plist-get state :routing))
       0)))

(defun e-board-registry--schedule-close (operation)
  "Schedule OPERATION once for a later bounded drain."
  (unless (e-board-registry-close-operation-scheduled operation)
    (setf (e-board-registry-close-operation-scheduled operation) t)
    (funcall e-board-registry-close-scheduler
             (lambda () (e-board-registry--drain-close operation)))))

(defun e-board-registry--close-one-client (operation)
  "Reconcile at most one client or observer from OPERATION."
  (let* ((board (e-board-registry-close-operation-board operation))
         (source (e-board-registry-board-source-board board))
         (client (e-board-registry-close-operation-current-client operation)))
    (unless client
      (when-let ((id (pop (e-board-registry-close-operation-client-ids operation))))
        (setq client (gethash id (e-board-registry-board-clients board)))
        (setf (e-board-registry-close-operation-current-client operation) client
              (e-board-registry-close-operation-observer-ids operation)
              (and client (e-board-registry-client-observer-ids client)))))
    (cond
     ((null client) nil)
     ((e-board-registry-close-operation-observer-ids operation)
      (let ((observer-id
             (pop (e-board-registry-close-operation-observer-ids operation))))
        (when-let ((observer (e-board-observer source observer-id)))
          (when (memq (e-board-observer-state observer) '(active muted))
            (e-board-set-observer-state source observer-id 'cancelled))))
      t)
     (t
      (let ((client-id (e-board-registry-client-id client))
            (principal (e-board-registry-client-principal client)))
        (setf (e-board-registry-client-observer-ids client) nil
              (e-board-registry-client-state client) 'detached)
        (when principal
          (let ((remaining
                 (delete client-id
                         (gethash principal
                                  (e-board-registry-board-principal-clients board)))))
            (if remaining
                (puthash principal remaining
                         (e-board-registry-board-principal-clients board))
              (remhash principal
                       (e-board-registry-board-principal-clients board)))))
        (remhash client-id (e-board-registry-board-clients board))
        (setf (e-board-registry-close-operation-current-client operation) nil))
      t))))

(defun e-board-registry--finish-close (operation)
  "Commit OPERATION's final closed state and settle its request."
  (let* ((board (e-board-registry-close-operation-board operation))
         (id (e-board-registry-board-id board)))
    (setf (e-board-registry-board-state board) 'closed
          (e-board-registry-board-client-ids board) nil
          (e-board-registry-board-client-ids-tail board) nil
          (e-board-registry-board-participant-ids board) nil
          (e-board-registry-board-participant-ids-tail board) nil
          (e-board-registry-board-close-operation board) nil)
    (clrhash (e-board-registry-board-participants board))
    (e-board-unregister (e-board-registry-board-source-board board))
    (remhash id e-board-registry--boards)
    (avl-tree-delete e-board-registry--board-index (cons (format "%s" id) board))
    (e-request-finish (e-board-registry-close-operation-request operation) board)))

(defun e-board-registry--drain-close (operation)
  "Advance OPERATION through at most one fixed lifecycle page."
  (setf (e-board-registry-close-operation-scheduled operation) nil)
  (let* ((board (e-board-registry-close-operation-board operation))
         (source (e-board-registry-board-source-board board))
         (remaining e-board-registry-close-drain-limit))
    (unless (e-request-terminal-p
             (e-board-registry-close-operation-request operation))
      (if (e-board-registry--close-unsettled-p source)
          (e-board-registry--schedule-close operation)
        (while (> remaining 0)
          (pcase (e-board-registry-close-operation-phase operation)
            ('subscriptions
             (if-let ((subscription
                       (pop (e-board-registry-close-operation-subscriptions operation))))
                 (progn
                   ;; Route teardown belongs to the source board.  The
                   ;; explicit inactive projection preserves the historical
                   ;; closed-board state while the owner operation also
                   ;; fences prepared activations, queued classifiers/replays,
                   ;; and retained lifecycle callbacks.
                   (e-board-retire-subscription-exact
                    source subscription 'inactive)
                   (setq remaining (1- remaining)))
               (setf (e-board-registry-close-operation-phase operation)
                     'participants)))
            ('participants
             (if-let* ((participant-id
                        (pop (e-board-registry-close-operation-participants operation)))
                       (participant
                        (gethash participant-id
                                 (e-board-registry-board-participants board))))
                 (progn
                   (setf (e-board-participant-state
                          (e-board-registry-participant-source-participant
                           participant))
                         'closed)
                   (setq remaining (1- remaining)))
               (setf (e-board-registry-close-operation-phase operation) 'clients)))
            ('clients
             (if (e-board-registry--close-one-client operation)
                 (setq remaining (1- remaining))
               (setf (e-board-registry-close-operation-phase operation) 'finish)))
            ('finish
             (e-board-registry--finish-close operation)
             (setq remaining 0))))
        (unless (e-request-terminal-p
                 (e-board-registry-close-operation-request operation))
          (e-board-registry--schedule-close operation))))))

(defun e-board-registry-close (board-or-id)
  "Begin bounded close of BOARD-OR-ID and return its async request."
  (let* ((board (e-board-registry--require-active board-or-id))
         (source (e-board-registry-board-source-board board))
         (request (e-request-lifecycle-create
                   :id (e-board-registry--next-id
                        (e-board-registry-board-id-function board) 'close)
                   :owner 'e-board-registry-close))
         (operation
          (e-board-registry-close-operation--create
           :board board :request request :phase 'subscriptions
           :subscriptions (e-board-subscriptions source)
           :participants (e-board-registry-board-participant-ids board)
           :client-ids (e-board-registry-board-client-ids board))))
    (setf (e-board-registry-board-state board) 'closing
          (e-board-registry-board-close-operation board) operation)
    (e-request-start request (list :board-id (e-board-registry-board-id board)))
    (e-board-registry--schedule-close operation)
    request))

(provide 'e-board-registry)

;;; e-board-registry.el ends here
