;;; e-session-board-input-admission.el --- Staged Board input admission -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the session side of the one Board-pickup/session admission.  It stages
;; one ordinary session activity entry without publishing it.  The Board
;; application service commits that record with the pickup acceptance, then
;; invokes the narrow publish operation below.

;;; Code:

(require 'cl-lib)
(require 'e-session)
(require 'e-session-aggregate)
(require 'e-session-codec)
(require 'e-session-storage)

(cl-defstruct (e-session-board-input-admission
               (:constructor e-session-board-input-admission--create)
               (:conc-name e-session-board-input-admission--))
  store session-id delivery-id lane stage entry record)

(defvar e-session-board-input-admission--owner-barrier-held-p nil
  "Non-nil while the pickup application service owns the target session.")

(defun e-session-board-input-admission-ensure-ready (store session-id)
  "Load SESSION-ID before the pickup application acquires both owner barriers."
  (e-session-get store session-id))

(defun e-session-board-input-admission-call-with-owner-barrier
    (store session-id operation)
  "Call OPERATION while SESSION-ID rejects reentrant semantic mutation."
  (e-session--call-with-commit-barrier
   store session-id
   (lambda ()
     (let ((e-session-board-input-admission--owner-barrier-held-p t))
       (funcall operation)))))

(cl-defun e-session-board-input-admission-prepare
    (store session-id delivery-id lane content &key metadata)
  "Prepare one detached session admission for DELIVERY-ID on LANE."
  (unless (e-session-storage-sqlite-p store)
    (signal 'e-session-storage-error
            (list "Board pickup admission requires SQLite" session-id)))
  (unless e-session-board-input-admission--owner-barrier-held-p
    (e-session-board-input-admission-ensure-ready store session-id))
  (let* ((stage (e-session-aggregate-stage-session-mutation store session-id))
         (turn-id (format "board-pickup:%s"
                          (secure-hash 'sha256 (prin1-to-string delivery-id))))
         (entry
          (e-session-aggregate-append-activity-event
           stage session-id turn-id 'board-input-admitted
           (list :delivery-id (copy-tree delivery-id) :lane lane
                 :content (copy-tree content) :metadata (copy-tree metadata))
           :write-index nil))
         (record (e-session-codec-record-for-entry session-id entry)))
    (e-session-storage-prepare-mutation store session-id record)
    (e-session-board-input-admission--create
     :store store :session-id session-id :delivery-id (copy-tree delivery-id)
     :lane lane :stage stage :entry entry :record record)))

(defun e-session-board-input-admission-record (admission)
  "Return ADMISSION's detached preflighted record."
  (copy-tree (e-session-board-input-admission--record admission)))

(defun e-session-board-input-admission-session-id (admission)
  "Return ADMISSION's session identity."
  (e-session-board-input-admission--session-id admission))

(defun e-session-board-input-admission-lane (admission)
  "Return ADMISSION's durable delivery lane projection."
  (e-session-board-input-admission--lane admission))

(defun e-session-board-input-admission-entry (admission)
  "Return ADMISSION's detached semantic entry."
  (copy-tree (e-session-board-input-admission--entry admission)))

(defun e-session-board-input-admission-publish (admission)
  "Publish already committed ADMISSION into the live session aggregate."
  (e-session-aggregate-publish-staged-session
   (e-session-board-input-admission--store admission)
   (e-session-board-input-admission--stage admission)
   (e-session-board-input-admission--session-id admission))
  (copy-tree (e-session-board-input-admission--entry admission)))

(provide 'e-session-board-input-admission)

;;; e-session-board-input-admission.el ends here
