;;; repair-claimed-head.el --- one-off offline Board pickup repair -*- lexical-binding: t; -*-

;; Run with: E_BOARD_REPAIR_STORE_DIR=/path/to/offline-copy eldev exec -R -f docs/bugs/board-steered-input-strands-fifo/repair-claimed-head.el
;; Set E_BOARD_REPAIR_APPLY=1 to change that copy after reviewing the dry run.
;; The exact live directory additionally requires E_BOARD_REPAIR_ALLOW_LIVE=1.

(require 'cl-lib)
(require 'sqlite)
(require 'e-board-sqlite-worker)
(require 'e-runtime-store-codec)

(defconst e-board-repair--board-id "brd_fdc2c88f13ad9d1464990437032ad3d3")
(defconst e-board-repair--session-id "20261001T124240-b830b9a2a666")
(defconst e-board-repair--generation 1)

(defun e-board-repair--value (encoded)
  (e-runtime-store-codec-decode (base64-decode-string encoded)))

(defun e-board-repair--inspect (database)
  "Verify the exact stranded head and return its proven consumption facts."
  (let* ((association
          (car (sqlite-select
                database
                "SELECT board_id,generation,participant_id FROM board_session_associations WHERE session_id=?"
                (vector e-board-repair--session-id))))
         (participant-id
          (and association (e-board-sqlite-worker--column association 2))))
    (unless (and association
                 (equal (e-board-sqlite-worker--column association 0)
                        e-board-repair--board-id)
                 (= (e-board-sqlite-worker--column association 1)
                    e-board-repair--generation)
                 (stringp participant-id))
      (error "Session Board association no longer matches this repair"))
    (let* ((rows
            (sqlite-select
             database
             "SELECT delivery_key,participant_id,fifo_position,message_id,state,revision,attempt,payload FROM board_pickups WHERE board_id=? AND generation=? AND participant_id=? AND state IN ('pending','ready','claimed','accepted','cancelling') ORDER BY fifo_position LIMIT 17"
             (vector e-board-repair--board-id e-board-repair--generation
                     participant-id)))
           (pickups
            (mapcar (lambda (row)
                      (e-board-sqlite-worker--pickup-dto
                       row e-board-repair--board-id e-board-repair--generation))
                    rows))
           (head (car pickups)))
      (unless (and (<= 4 (length pickups) 16)
                   (equal (mapcar (lambda (pickup)
                                    (plist-get pickup :fifo-position))
                                  pickups)
                          (number-sequence 2 (1+ (length pickups))))
                   (eq (plist-get head :state) 'claimed)
                   (cl-every (lambda (pickup)
                               (eq (plist-get pickup :state) 'pending))
                             (cdr pickups)))
        (error "Participant FIFO changed; inspect it before repair"))
      (let* ((message-rows
              (sqlite-select
               database
               "SELECT position,payload FROM session_records WHERE session_id=? AND record_type='message' ORDER BY position DESC LIMIT 513"
               (vector e-board-repair--session-id)))
             (matches nil))
        (when (> (length message-rows) 512)
          (error "Session message proof exceeds the offline bound"))
        (dolist (row message-rows)
          (let* ((record
                  (e-board-repair--value
                   (e-board-sqlite-worker--column row 1)))
                 (message (plist-get record :message))
                 (metadata (and (listp message)
                                (plist-get message :metadata))))
            (when (and (eq (plist-get message :role) 'user)
                       (equal (plist-get metadata :board-delivery-id)
                              (plist-get head :delivery-id)))
              (push (list :position (e-board-sqlite-worker--column row 0)
                          :message-id (plist-get message :id)
                          :turn-id (plist-get message :turn-id)
                          :content (plist-get message :content))
                    matches))))
        (unless (and (= (length matches) 1)
                     (stringp (plist-get (car matches) :message-id))
                     (stringp (plist-get (car matches) :turn-id))
                     (equal (plist-get (car matches) :content)
                            (plist-get head :content)))
          (error "No unique committed session input matches the claimed delivery"))
        (let* ((message (car matches))
               (events
                (sqlite-select
                 database
                 "SELECT payload FROM session_records WHERE session_id=? AND record_type='activity-event' AND position>? ORDER BY position LIMIT 513"
                 (vector e-board-repair--session-id
                         (plist-get message :position)))))
          (when (> (length events) 512)
            (error "Terminal turn proof exceeds the offline bound"))
          (unless (cl-some
                   (lambda (row)
                     (let ((event
                            (e-board-repair--value
                             (e-board-sqlite-worker--column row 0))))
                       (and (equal (plist-get event :turn-id)
                                   (plist-get message :turn-id))
                            (eq (plist-get event :event-type)
                                'turn-finished))))
                   events)
            (error "The committed input has no finished-turn proof"))
          (list :participant-id participant-id
                :delivery-id (plist-get head :delivery-id)
                :message-id (plist-get message :message-id)
                :turn-id (plist-get message :turn-id)
                :next-delivery-id (plist-get (cadr pickups) :delivery-id)))))))

(let* ((directory (getenv "E_BOARD_REPAIR_STORE_DIR"))
       (live-directory
        (expand-file-name "~/.config/emacs/.local/cache/e")))
  (unless (and directory (file-directory-p directory)
               (file-exists-p (expand-file-name "store.sqlite3" directory)))
    (error "Set E_BOARD_REPAIR_STORE_DIR to a store directory"))
  (when (and (equal (file-truename directory)
                    (file-truename live-directory))
             (not (equal (getenv "E_BOARD_REPAIR_ALLOW_LIVE") "1")))
    (error "Set E_BOARD_REPAIR_ALLOW_LIVE=1 for the exact live store"))
  (let ((database (sqlite-open (expand-file-name "store.sqlite3" directory))))
    (unwind-protect
        (if (equal (getenv "E_BOARD_REPAIR_APPLY") "1")
            (progn
              (sqlite-execute database "BEGIN IMMEDIATE")
              (condition-case err
                  (let* ((proof (e-board-repair--inspect database))
                         (e-board-sqlite-worker--database database)
                         (result
                          (e-board-sqlite-worker--board-pickup-transition
                           (list :board-id e-board-repair--board-id
                                 :generation e-board-repair--generation
                                 :delivery-id (plist-get proof :delivery-id)
                                 :transition 'consume
                                 :data
                                 (list :session-id e-board-repair--session-id
                                       :message-id
                                       (plist-get proof :message-id))))))
                    (unless (and (eq (plist-get (plist-get result :pickup)
                                               :state)
                                     'consumed)
                                 (equal (plist-get (plist-get result :next)
                                                   :delivery-id)
                                        (plist-get proof :next-delivery-id)))
                      (error "Transition did not consume the head and promote its successor"))
                    (sqlite-execute database "COMMIT")
                    (princ (format "Repaired store: %S\n" proof)))
                (error
                 (ignore-errors (sqlite-execute database "ROLLBACK"))
                 (signal (car err) (cdr err)))))
          (princ (format "Dry run only: %S\n" (e-board-repair--inspect database))))
      (sqlite-close database))))

;;; repair-claimed-head.el ends here
