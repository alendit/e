;;; e-board-durability.el --- Board durable transition boundary -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module joins detached Board-owned transition values to the narrow
;; storage port.  It owns commit barriers and durable revision publication,
;; but no routing choice, endpoint wake, subscription policy, or SQL.

;;; Code:

(require 'cl-lib)
(require 'e-board-state)
(require 'e-board-admission)
(require 'e-board-storage)

(declare-function e-board-message "e-board" (board id))
(declare-function e-board-participant "e-board" (board participant-id))
(declare-function e-board--source-key-parts "e-board" (source-key))
(declare-function e-board--pickup-queue "e-board" (board participant-id))
(declare-function e-board--selector-attribute-clauses "e-board" (attributes))

(defvar e-board--storage-replay-p nil
  "Non-nil while publishing an already committed durable Board transition.")

(defvar e-board--post-storage-barrier-callbacks
  (make-hash-table :test 'eq :weakness 'key)
  "Process-local owner callbacks waiting for a Board commit barrier.")

(defun e-board--defer-after-storage-barrier (board callback)
  "Run CALLBACK later, after BOARD's current durable barrier is released.
This is the narrow owner-scheduling seam for callbacks that were already
admitted before a worker wait.  It stores no durable executable state and does
not allow a caller mutation to bypass the barrier."
  (if (e-board-mutation-frozen-p board)
      (puthash board
               (nconc (gethash board e-board--post-storage-barrier-callbacks)
                      (list callback))
               e-board--post-storage-barrier-callbacks)
    (run-at-time 0 nil callback)))

(defun e-board--release-storage-barrier (board)
  "Release BOARD's barrier and schedule its admitted owner callbacks."
  (setf (e-board-mutation-frozen-p board) nil)
  (let ((callbacks (gethash board e-board--post-storage-barrier-callbacks)))
    (remhash board e-board--post-storage-barrier-callbacks)
    (dolist (callback callbacks)
      (run-at-time 0 nil callback))))

(defun e-board-storage-backed-p (board)
  "Return non-nil when BOARD has a durable storage port."
  (and (e-board-p board) (e-board-storage-p (e-board-storage board))))

(defun e-board--call-with-storage-barrier (board operation)
  "Call OPERATION while BOARD rejects reentrant semantic mutation."
  (if (not (e-board-storage-backed-p board))
      (funcall operation)
    (when (e-board-mutation-frozen-p board)
      (signal 'e-board-mutation-frozen (list (e-board-id board))))
    (setf (e-board-mutation-frozen-p board) t)
    (unwind-protect (funcall operation)
      (e-board--release-storage-barrier board))))

(defun e-board--storage-publish-record (board record &optional source)
  "Commit detached RECORD and optional SOURCE before BOARD publication."
  (when (and (e-board-storage-backed-p board)
             (not e-board--storage-replay-p))
    (let ((result
           (e-board--call-with-storage-barrier
            board
            (lambda ()
              (e-board-storage-publish-record
               (e-board-storage board) (e-board-id board)
               (e-board-generation board) record source)))))
      (setf (e-board-revision board) (plist-get result :revision))
      result)))

(defun e-board--storage-transition-pickup (board pickup transition &optional data)
  "Commit PICKUP TRANSITION before changing its live Board projection."
  (when (and (e-board-storage-backed-p board)
             (not e-board--storage-replay-p))
    (let ((result
           (e-board--call-with-storage-barrier
            board
            (lambda ()
              (e-board-storage-transition-pickup
               (e-board-storage board) (e-board-id board)
               (e-board-generation board) (e-board-pickup-delivery-id pickup)
               transition data)))))
      (setf (e-board-revision board)
            (or (plist-get result :board-revision)
                (plist-get result :revision))
            (e-board-pickup-revision pickup)
            (or (plist-get result :pickup-revision)
                (plist-get (plist-get result :pickup) :revision)))
      (when-let* ((next-envelope (plist-get result :next))
                  (next (gethash (plist-get next-envelope :delivery-id)
                                 (e-board-pickups board))))
        (setf (e-board-pickup-revision next)
              (plist-get next-envelope :revision)
              (e-board-pickup-state next) 'ready))
      result)))

(defun e-board-persist-participant (board participant)
  "Commit detached logical PARTICIPANT before registry publication."
  (when (and (e-board-storage-backed-p board)
             (not e-board--storage-replay-p))
    (let ((result
           (e-board--call-with-storage-barrier
            board
            (lambda ()
              (e-board-storage-put-participant
               (e-board-storage board) (e-board-id board)
               (e-board-generation board) participant)))))
      (setf (e-board-revision board) (plist-get result :revision))
      result)))

(defun e-board-abort-persisted-participant (board participant-id)
  "Delete unpublished PARTICIPANT-ID before removing its live projection."
  (when (and (e-board-storage-backed-p board)
             (not e-board--storage-replay-p))
    (let ((result
           (e-board--call-with-storage-barrier
            board
            (lambda ()
              (e-board-storage-delete-participant
               (e-board-storage board) (e-board-id board)
               (e-board-generation board) participant-id)))))
      (setf (e-board-revision board) (plist-get result :revision))
      result)))

(defun e-board-publish-persisted-participant (board participant-id)
  "Publish provisional PARTICIPANT-ID after its enclosing admission commits."
  (when (and (e-board-storage-backed-p board)
             (not e-board--storage-replay-p))
    (let ((result
           (e-board--call-with-storage-barrier
            board
            (lambda ()
              (e-board-storage-publish-participant
               (e-board-storage board) (e-board-id board)
               (e-board-generation board) participant-id)))))
      (setf (e-board-revision board) (plist-get result :revision))
      result)))

(defun e-board-pickup-envelope (pickup)
  "Return PICKUP's detached durable semantic projection.
Process-local participant lifetimes, endpoint tokens, receipts, and attachment
generations are deliberately absent."
  (unless (e-board-pickup-p pickup)
    (signal 'wrong-type-argument (list 'e-board-pickup-p pickup)))
  (list :delivery-id (copy-tree (e-board-pickup-delivery-id pickup))
        :participant-id (e-board-pickup-participant-id pickup)
        :message-id (e-board-pickup-message-id pickup)
        :subscription-ids (copy-tree (e-board-pickup-subscription-ids pickup))
        :event-seq-range (copy-tree (e-board-pickup-event-seq-range pickup))
        :mode (e-board-pickup-mode pickup)
        :requester-actor (copy-tree (e-board-pickup-requester-actor pickup))
        :addressed-p (e-board-pickup-addressed-p pickup)
        :cause-metadata (copy-tree (e-board-pickup-cause-metadata pickup))
        :content (copy-tree (e-board-pickup-content pickup))
        :reference (copy-tree (e-board-pickup-reference pickup))))

(defun e-board-durability-commit-routing
    (board message prepared-pickups participant-ids overflow)
  "Commit MESSAGE's final routing and PREPARED-PICKUPS before publication.
PARTICIPANT-IDS and OVERFLOW are already-decided Board policy values."
  (when (e-board-storage-backed-p board)
    (let* ((state (cond (overflow 'routing-failed)
                        ((null participant-ids) 'unrouted)
                        (t 'routed)))
           (reason (cond (overflow overflow)
                         ((null participant-ids)
                          (if (e-board-message-to message)
                              'target-unavailable
                            'no-matching-subscription))))
           (result
            (e-board--call-with-storage-barrier
             board
             (lambda ()
               (e-board-storage-commit-routing
                (e-board-storage board) (e-board-id board)
                (e-board-generation board) (e-board-message-id message)
                (list :state state :reason reason
                      :participant-ids (copy-tree participant-ids)
                      :pickup-ids
                      (mapcar (lambda (pickup)
                                (copy-tree
                                 (e-board-pickup-delivery-id pickup)))
                              prepared-pickups))
                (mapcar #'e-board-pickup-envelope prepared-pickups))))))
      (setf (e-board-revision board) (plist-get result :revision))
      (cl-mapc
       (lambda (pickup committed)
         (setf (e-board-pickup-fifo-position pickup)
               (plist-get committed :fifo-position)
               (e-board-pickup-revision pickup)
               (plist-get committed :revision)
               (e-board-pickup-state pickup)
               (plist-get committed :state)))
       prepared-pickups (append (plist-get result :pickups) nil))
      result)))

(defun e-board-restore-source (board message source)
  "Restore MESSAGE's canonical durable SOURCE projection into BOARD caches."
  (when source
    (pcase-let* ((kind (plist-get source :kind))
                 (source-key (plist-get source :key))
                 (`(,producer ,generation ,sequence)
                  (e-board--source-key-parts source-key))
                 (recent-key (list kind producer generation sequence)))
      (puthash recent-key message (e-board-source-recent board))
      (puthash recent-key (plist-get source :hash)
               (e-board-source-signatures board))
      (puthash (list kind producer generation) sequence
               (e-board-source-high-watermarks board))))
  message)

(defun e-board-restore-routing (board message-id outcome pickups)
  "Restore committed OUTCOME and unresolved PICKUPS for MESSAGE-ID."
  (let ((message (or (e-board-message board message-id)
                     (signal 'e-board-error
                             (list "Unknown restored message" message-id)))))
    (setf (e-board-message-routing-state message) (plist-get outcome :state)
          (e-board-message-unrouted-reason message) (plist-get outcome :reason)
          (e-board-message-matching-participant-ids message)
          (copy-tree (plist-get outcome :participant-ids))
          (e-board-message-pickup-ids message)
          (or (copy-tree (plist-get outcome :pickup-ids))
              (mapcar (lambda (pickup) (plist-get pickup :delivery-id))
                      pickups)))
    (dolist (envelope pickups)
      (let* ((participant-id (plist-get envelope :participant-id))
             (participant (e-board-participant board participant-id))
             (pickup
              (e-board-pickup--create
               :delivery-id (copy-tree (plist-get envelope :delivery-id))
               :board-id (e-board-id board) :participant-id participant-id
               :message-id message-id
               :subscription-ids (copy-tree (plist-get envelope :subscription-ids))
               :participant-lifetime participant
               :event-seq-range (copy-tree (plist-get envelope :event-seq-range))
               :mode (plist-get envelope :mode)
               :requester-actor (copy-tree (plist-get envelope :requester-actor))
               :addressed-p (plist-get envelope :addressed-p)
               :cause-metadata (copy-tree (plist-get envelope :cause-metadata))
               :content (copy-tree (plist-get envelope :content))
               :reference (copy-tree (plist-get envelope :reference))
               :fifo-position (plist-get envelope :fifo-position)
               :revision (plist-get envelope :revision)
               :state (plist-get envelope :state))))
        (puthash (e-board-pickup-delivery-id pickup) pickup
                 (e-board-pickups board))
        (when (memq (e-board-pickup-state pickup)
                    '(pending ready claimed accepted cancelling))
          (puthash participant-id
                   (append (e-board--pickup-queue board participant-id)
                           (list (e-board-pickup-delivery-id pickup)))
                   (e-board-pickup-queues board))
          (e-board-state-adjust-unsettled board 'pickups 1))))
    message))

(defun e-board-durability--normalize-record-selector (selector)
  "Return SELECTOR with its Board attribute grammar canonicalized."
  (unless (or (null selector) (listp selector))
    (signal 'wrong-type-argument (list 'listp selector)))
  (let ((normalized (copy-tree selector)))
    (when (plist-member normalized :attributes)
      (setq normalized
            (plist-put normalized :attributes
                       (e-board--selector-attribute-clauses
                        (plist-get normalized :attributes)))))
    normalized))

(defun e-board-durable-record-page (board &optional after limit selector generation)
  "Return one bounded durable record page for BOARD.
GENERATION defaults to the current generation; older generations remain
available as immutable audit after `e-board-clear'."
  (unless (e-board-storage-backed-p board)
    (signal 'e-board-storage-unavailable (list (e-board-id board))))
  (e-board-storage-record-page
   (e-board-storage board) (e-board-id board)
   (or generation (e-board-generation board)) after limit
   (e-board-durability--normalize-record-selector selector)))

(defun e-board-durability-status (board)
  "Return BOARD's bounded local durability and runtime status."
  (unless (e-board-storage-backed-p board)
    (signal 'e-board-storage-unavailable (list (e-board-id board))))
  (append (list :board-id (e-board-id board)
                :generation (e-board-generation board)
                :revision (e-board-revision board))
          (e-board-storage-status (e-board-storage board))))

(defun e-board-clear (board)
  "Advance durable BOARD generation and clear current live publications."
  (unless (e-board-storage-backed-p board)
    (signal 'e-board-storage-unavailable (list (e-board-id board))))
  (let ((result
         (e-board--call-with-storage-barrier
          board
          (lambda ()
            (e-board-storage-clear-board
             (e-board-storage board) (e-board-id board))))))
    (setf (e-board-generation board) (plist-get result :generation)
          (e-board-revision board) (plist-get result :revision)
          (e-board-messages board) nil
          (e-board-messages-tail board) nil
          (e-board-message-count board) 0
          (e-board-routed-pickup-results board) nil
          (e-board-routed-pickup-results-tail board) nil
          (e-board-unsettled-pickup-count board) 0)
    (dolist (table (list (e-board-message-table board)
                         (e-board-message-seq-table board)
                         (e-board-message-index-table board)
                         (e-board-message-kind-newest-table board)
                         (e-board-message-kind-tag-newest-table board)
                         (e-board-event-message-count board)
                         (e-board-pickups board)
                         (e-board-pickup-queues board)
                         (e-board-source-recent board)
                         (e-board-source-signatures board)
                         (e-board-source-high-watermarks board)))
      (clrhash table))
    (puthash (e-board-next-seq board) 0 (e-board-event-message-count board))
    (setf (e-board-event-message-prefix-high-watermark board)
          (e-board-next-seq board))
    (e-board-admission-append-event
     board 'board-cleared (list :generation (e-board-generation board)))
    result))

(provide 'e-board-durability)

;;; e-board-durability.el ends here
