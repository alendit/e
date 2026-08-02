;;; e-board-registry.el --- Process-local board lifecycle registry for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module owns the process-local board lifecycle and its local identities.
;; `e-board' remains the source-board model for routing and subscriptions.

;;; Code:

(require 'cl-lib)
(require 'e-board)

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
  "Process-local fallback sequence for registry-owned identities.")

(defvar e-board-registry--boards (make-hash-table :test 'equal)
  "Live and closed process-local boards keyed by board id.")

(defconst e-board-registry-list-page-limit 32
  "Default maximum number of board records returned by one registry page.")

(cl-defstruct (e-board-registry-board
                (:constructor e-board-registry-board--create)
                (:conc-name e-board-registry-board-))
  id source-board state author principal principal-grants id-function clients client-generations participants)

(cl-defstruct (e-board-registry-client
                (:constructor e-board-registry-client--create)
                (:conc-name e-board-registry-client-))
  id board-id author principal role generation state observer-ids)

(cl-defstruct (e-board-registry-requester-context
               (:constructor e-board-registry-requester-context--create)
               (:conc-name e-board-registry-requester-context-))
  board-id client-id client-generation principal role)

(cl-defstruct (e-board-registry-participant
                (:constructor e-board-registry-participant--create)
                (:conc-name e-board-registry-participant-))
  id board-id author principal controller role access-grants private-grants source-participant)

(defconst e-board-registry-participant-private-rights
  '(inspect-transcript control-session)
  "Closed set of session-private participant access rights.")

(defun e-board-registry--next-id (id-function kind)
  "Return an identity for KIND from ID-FUNCTION or the local fallback."
  (let ((id (if id-function
                 (funcall id-function kind)
              (format "%s%d"
                      (pcase kind
                        ('board "brd_")
                        ('client "cli_")
                        ('participant "ptc_")
                        ('subscription "sub_")
                        (_ (format "%s_" kind)))
                      (cl-incf e-board-registry--id-sequence)))))
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
         requester-principal)
    (when (and target
               (memq (e-board-participant-state
                      (e-board-registry-participant-source-participant target))
                     '(active dormant stale))
               (or (null target-principal)
                   (e-board-registry-principal-role board target-principal)))
      (setq requester-principal
            (cond
             ((null actor) :private-pre-cutover)
             ((stringp actor)
              (and (e-board-registry-principal-role board actor) actor))
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
                       (or principal :private-pre-cutover)))))))
      (and requester-principal
           (or (null (e-board-message-to message))
               (eq requester-principal :private-pre-cutover)
               (condition-case nil
                   (e-board-registry-authorize-exact-post
                    board requester-principal target)
                 (e-board-registry-authorization-denied nil)))))))

(cl-defun e-board-registry-create (&key id id-function author principal)
  "Create and register an active board with stored AUTHOR and PRINCIPAL.
ID-FUNCTION receives an identity kind and supplies all registry-owned ids.
The source board is registered with `e-board' under the same board identity."
  (let* ((id (or id (e-board-registry--next-id id-function 'board))))
    (when (gethash id e-board-registry--boards)
      (signal 'e-board-registry-id-conflict (list id)))
    (let* ((source-board (e-board-create :id id))
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
                  :participants (make-hash-table :test 'equal))))
      (setf (e-board-classification-authorizer source-board)
            (lambda (subscription message phase)
              (e-board-registry--classification-authorized-p
               board subscription message phase)))
      (puthash id board e-board-registry--boards)
      board)))

(defun e-board-registry-get (id)
  "Return process-local board ID, or signal `e-board-registry-missing'."
  (e-board-registry--resolve id))

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
        page more)
    (maphash
     (lambda (_id board)
       (let ((key (format "%s" (e-board-registry-board-id board))))
         (when (or (null after-key) (string< after-key key))
           (setq page
                 (sort (cons board page)
                       (lambda (left right)
                         (string< (format "%s" (e-board-registry-board-id left))
                                  (format "%s" (e-board-registry-board-id right))))))
           (when (> (length page) limit)
             (setq more t)
             (setcdr (nthcdr (1- limit) page) nil)))))
     e-board-registry--boards)
    (list :boards page
          :next-after (and more (e-board-registry-board-id (car (last page)))))))

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
      client)))

(defun e-board-registry-detach-client (board-or-id client-id)
  "Detach CLIENT-ID from active BOARD-OR-ID and release its observers."
  (let* ((board (e-board-registry--require-active board-or-id))
         (clients (e-board-registry-board-clients board))
         (client (gethash client-id clients)))
    (when client
      (dolist (observer-id (e-board-registry-client-observer-ids client))
        (when-let ((observer (e-board-observer
                              (e-board-registry-board-source-board board)
                              observer-id)))
          (when (memq (e-board-observer-state observer) '(active muted))
            (e-board-set-observer-state
             (e-board-registry-board-source-board board) observer-id 'cancelled))))
      (setf (e-board-registry-client-observer-ids client) nil)
      (setf (e-board-registry-client-state client) 'detached)
      (remhash client-id clients))
    client))

(defun e-board-registry-client-requester-context (board-or-id client-id)
  "Capture active CLIENT-ID as a generation-fenced requester context."
  (let* ((board (e-board-registry--require-active board-or-id))
         (client (gethash client-id (e-board-registry-board-clients board))))
    (unless (and client (eq (e-board-registry-client-state client) 'active))
      (signal 'e-board-registry-client-missing (list client-id)))
    (e-board-registry-requester-context--create
     :board-id (e-board-registry-board-id board)
     :client-id client-id
     :client-generation (e-board-registry-client-generation client)
     :principal (e-board-registry-client-principal client)
     :role (e-board-registry-client-role client))))

(defun e-board-registry-resolve-requester-principal (board-or-id context)
  "Return CONTEXT's principal only while its exact client generation is active."
  (let* ((board (e-board-registry--require-active board-or-id))
         (client-id (and (e-board-registry-requester-context-p context)
                         (e-board-registry-requester-context-client-id context)))
         (client (and client-id
                      (gethash client-id (e-board-registry-board-clients board)))))
    (unless (and client
                 (equal (e-board-registry-requester-context-board-id context)
                        (e-board-registry-board-id board))
                 (eq (e-board-registry-client-state client) 'active)
                 (= (e-board-registry-requester-context-client-generation context)
                    (e-board-registry-client-generation client))
                 (equal (e-board-registry-requester-context-principal context)
                        (e-board-registry-client-principal client))
                 (eq (e-board-registry-requester-context-role context)
                     (e-board-registry-client-role client)))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) client-id 'stale-requester)))
    (e-board-registry-client-principal client)))

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
    (board-or-id &key id author principal controller (state 'active))
  "Add a board-local participant to active BOARD-OR-ID.
The participant's built-in exact address subscription is created by the source
board, with its identity supplied by this registry's id generator."
  (let* ((board (e-board-registry--require-active board-or-id))
         (id-function (e-board-registry-board-id-function board))
         (id (or id (e-board-registry--next-id id-function 'participant)))
         (participants (e-board-registry-board-participants board))
         (role (and principal (e-board-registry-principal-role board principal))))
    (when (and principal (not role))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) principal 'participant)))
    (when (gethash id participants)
      (signal 'e-board-registry-id-conflict (list id)))
    (let* ((source-participant
            (e-board-add-participant
             (e-board-registry-board-source-board board)
             :id id
             :state state
             :create-pickup-subscription-id
             (e-board-registry--next-id id-function 'subscription)))
           (participant
            (e-board-registry-participant--create
             :id id :board-id (e-board-registry-board-id board)
             :author author :principal principal
             :controller (or controller principal) :role role
             :access-grants (make-hash-table :test 'equal)
             :private-grants
             (let ((grants (make-hash-table :test 'equal)))
               (when-let ((controller (or controller principal)))
                 (puthash controller
                          (copy-sequence
                           e-board-registry-participant-private-rights)
                          grants))
               grants)
             :source-participant source-participant)))
      (puthash id participant participants)
      participant)))

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

(defun e-board-registry-participant-delivery-authorization
    (board-or-id participant-or-id)
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
    (e-board--append-event
     source-board 'participant-removed (list :participant-id participant-id))
    participant))

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
    (e-board--append-event
     source-board
     (pcase state
       ('active 'participant-rebound)
       ('detaching 'participant-detaching)
       ('dormant 'participant-dormant)
       ('stale 'participant-stale))
     (list :participant-id (e-board-registry-participant-id participant)
           :from from :state state))
    participant))

(cl-defun e-board-registry-install-subscription
    (board-or-id participant-or-id selector &key id (state 'active)
                (effect 'create-pickup))
  "Install an ordinary source-board subscription for a local participant."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (id (or id (e-board-registry--next-id
                     (e-board-registry-board-id-function board) 'subscription))))
    (e-board-subscribe (e-board-registry-board-source-board board)
                      (e-board-registry-participant-id participant)
                       selector :id id :state state :effect effect)))

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
                 (state 'active))
  "Replace ordinary SUBSCRIPTION-ID with SELECTOR on active BOARD-OR-ID.
The core retains the existing participant and default effect when EFFECT is
omitted; this registry supplies only the board-local replacement identity."
  (let* ((board (e-board-registry--require-active board-or-id))
         (arguments
          (list :id (or id (e-board-registry--next-id
                            (e-board-registry-board-id-function board)
                            'subscription))
                :state state)))
    (when effect-supplied-p
      (setq arguments (append arguments (list :effect effect))))
    (apply #'e-board-replace-subscription
           (e-board-registry-board-source-board board)
           subscription-id selector arguments)))

(defun e-board-registry-close (board-or-id)
  "Close BOARD-OR-ID, disable its routes, and unregister its source board."
  (let ((board (e-board-registry--require-active board-or-id)))
    (maphash
     (lambda (_id participant)
       (setf (e-board-participant-state
              (e-board-registry-participant-source-participant participant))
             'closed))
     (e-board-registry-board-participants board))
    (dolist (subscription
             (e-board-subscriptions (e-board-registry-board-source-board board)))
      (setf (e-board-subscription-state subscription) 'inactive))
    (setf (e-board-registry-board-state board) 'closed)
    (e-board-unregister (e-board-registry-board-source-board board))
    board))

(provide 'e-board-registry)

;;; e-board-registry.el ends here
