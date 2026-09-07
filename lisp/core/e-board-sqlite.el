;;; e-board-sqlite.el --- SQLite-backed live Board controllers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Composes bounded process-local Board coordination from detached SQLite
;; current-state queries.  SQLite remains authoritative for durable history;
;; a controller retains only current participants, unresolved work, the active
;; routing rows needed by that work, and one small recent presentation window.

;;; Code:

(require 'cl-lib)
(require 'e-board)
(require 'e-board-registry)
(require 'e-board-storage-sqlite)

(defconst e-board-sqlite-default-controller-record-limit 64
  "Maximum recent Board records retained by an ordinary live controller.")

(cl-defun e-board-sqlite-create-registry-board
    (runtime &key id author principal id-function)
  "Create a durable registry Board on shared RUNTIME."
  (e-board-registry-create
   :id id :author author :principal principal :id-function id-function
   :storage (e-board-storage-sqlite-create-async runtime)))

(defun e-board-sqlite--controller-records (state)
  "Return STATE's deduplicated bounded records in durable position order."
  (let ((by-position (make-hash-table :test 'eql)) records)
    (dolist (item (append (plist-get state :records)
                          (plist-get state :working-records)))
      (puthash (plist-get item :position) item by-position))
    (maphash (lambda (_position item) (push item records)) by-position)
    (sort records (lambda (left right)
                    (< (plist-get left :position)
                       (plist-get right :position))))))

(cl-defun e-board-sqlite-controller-from-state
    (runtime state &key author id-function)
  "Build one bounded live Board controller from detached query STATE."
  (let* ((root (plist-get state :board))
         (board-id (plist-get root :board-id))
         (storage (e-board-storage-sqlite-create-async runtime))
         (generation (plist-get root :generation))
         (registry-board
          (e-board-registry-create
           :id board-id :author author
           :principal (plist-get root :trusted-principal)
           :id-function id-function :storage storage :restoring t
           :generation generation :revision (plist-get root :revision)))
         (board (e-board-registry-board-source-board registry-board)))
    (setf (e-board-storage--next-revision storage) (plist-get root :revision)
          (e-board-storage--next-position storage) (plist-get root :next-position)
          (e-board-storage--next-generation storage) generation)
    (let ((e-board--storage-replay-p t))
      (dolist (participant (plist-get state :participants))
        (unless (plist-get participant :publication-pending)
          (let ((current
                 (e-board-registry-add-participant
                  registry-board :id (plist-get participant :id)
                  :author (plist-get participant :author)
                  :principal (plist-get participant :principal)
                  :controller (plist-get participant :controller)
                  :subscription-id (plist-get participant :subscription-id)
                  :state 'dormant :publish-event nil)))
            (setf (e-board-registry-participant-publication-pending current)
                  nil))))
      (dolist (item (e-board-sqlite--controller-records state))
        (let ((record (plist-get item :record)))
          (if (plist-get record :record-type)
              (e-board-import-processing-record board record)
            (let ((message
                   (e-board-import-message
                    board record (plist-get item :position))))
              (e-board-restore-source board message (plist-get item :source)))))))
    (dolist (routing (plist-get state :routing))
      (e-board-restore-routing
       board (plist-get routing :message-id) (plist-get routing :outcome)
       (cl-remove-if-not
        (lambda (pickup)
          (equal (plist-get pickup :message-id)
                 (plist-get routing :message-id)))
        (plist-get state :pickups))))
    registry-board))

(cl-defun e-board-sqlite-open-controller-start
    (runtime board-id &key author id-function record-limit)
  "Return work that queries and builds BOARD-ID's bounded live controller."
  (let ((query
         (e-board-storage-sqlite-controller-state-start
          runtime board-id
          (or record-limit e-board-sqlite-default-controller-record-limit)))
        (result
         (e-work-prepare
          (e-work-spec-create
           :id "board-controller-open" :execution 'cooperative
           :interactive-policy 'async :owner 'e-board-sqlite
           :runner (lambda (_handle _arguments _context) :deferred))
          nil :context (list :domain-ref board-id
                             :work-kind 'board-controller-open))))
    (e-work-on-settle
     query
     (lambda (settled)
       (let ((status (e-work-status settled)))
         (if (eq (plist-get status :state) 'finished)
             (condition-case error
                 (e-work-finish
                  result
                  (e-board-sqlite-controller-from-state
                   runtime (plist-get status :result)
                   :author author :id-function id-function))
               (error (e-work-fail result error)))
           (e-work-fail result (plist-get status :error))))))
    result))

(provide 'e-board-sqlite)

;;; e-board-sqlite.el ends here
