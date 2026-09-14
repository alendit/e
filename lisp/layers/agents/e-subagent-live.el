;;; e-subagent-live.el --- private live child execution state -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;;
;; This file is part of e.

;;; Commentary:
;;
;; The Board database is the authority for participant admission and
;; lifecycle/report facts.  This module owns only process-local execution
;; capabilities while a child is running: its harness, work handle,
;; cancellation operation, callbacks, and the latest bounded progress
;; snapshot.  It deliberately has no public inventory, status/result/output
;; projection, terminal history, or generated child identifier.
;;
;; The key is the durable Board/participant pair.  Keeping the Board in the
;; key is important: two Boards can legitimately contain children with the
;; same task coordinates and session-shaped values.

;;; Code:

(require 'cl-lib)
(require 'e-runtime-store-codec)

(define-error 'e-subagent-live-error
  "Invalid or unavailable live subagent execution state")

(defconst e-subagent-live--max-progress-bytes 4096
  "Maximum encoded size of a live progress snapshot.")

(cl-defstruct (e-subagent-live
               (:constructor e-subagent-live--create))
  "Private process-local live execution owner.

Only transient capability records live here.  Durable admission and terminal
facts belong to the Board SQL service and are never copied into this owner."
  (entries (make-hash-table :test #'equal))
  (pending-admissions (make-hash-table :test #'equal)))

(defun e-subagent-live-create ()
  "Create one private process-local live execution owner.

This constructor is intentionally the only owner-level entry point exposed by
the module; there is no public inventory or record projection."
  (e-subagent-live--create))

(defun e-subagent-live--string (value name)
  (unless (and (stringp value) (> (length value) 0))
    (signal 'e-subagent-live-error
            (list (format "%s must be a non-empty string" name))))
  value)

(defun e-subagent-live--key (board-id participant-id)
  (cons (e-subagent-live--string board-id "board-id")
        (e-subagent-live--string participant-id "participant-id")))

(defun e-subagent-live--entry (owner board-id participant-id)
  (gethash (e-subagent-live--key board-id participant-id)
           (e-subagent-live-entries owner)))

(defun e-subagent-live--pending (owner board-id participant-id)
  (gethash (e-subagent-live--key board-id participant-id)
           (e-subagent-live-pending-admissions owner)))

(defun e-subagent-live--copy-progress (progress)
  "Return a bounded copy of PROGRESS or nil.

Progress is intentionally treated as an opaque, latest-only value.  A
bounded encoded copy prevents a provider callback from retaining an
unbounded transcript or arbitrary mutable object in the live owner."
  (when progress
    (let ((encoded (e-runtime-store-codec-encode-bounded
                    progress e-subagent-live--max-progress-bytes)))
      (e-runtime-store-codec-decode encoded))))

(defun e-subagent-live-reserve-admission
    (owner board-id participant-id &rest properties)
  "Reserve an admission keyed by BOARD-ID and PARTICIPANT-ID.

The reservation is temporary and exists only while the asynchronous Board
admission operation settles.  PROPERTIES are execution callbacks/handles and
bounded assignment coordinates needed to start the child; callers must not
store durable status, result, output, or terminal history here.  Signal when
the same durable identity is already reserved or live."
  (let ((key (e-subagent-live--key board-id participant-id)))
    (when (or (gethash key (e-subagent-live-entries owner))
              (gethash key (e-subagent-live-pending-admissions owner)))
      (signal 'e-subagent-live-error
              (list "participant already has live execution state")))
    (let ((record (append (list :board-id board-id
                                :participant-id participant-id
                                :created-at (float-time))
                          properties)))
      (puthash key record (e-subagent-live-pending-admissions owner))
      record)))

(defun e-subagent-live-forget-admission (owner board-id participant-id)
  "Remove a pending admission without retaining terminal state."
  (remhash (e-subagent-live--key board-id participant-id)
           (e-subagent-live-pending-admissions owner)))

(defun e-subagent-live-pending-admission (owner board-id participant-id)
  "Return the private pending admission for the durable identity."
  (e-subagent-live--pending owner board-id participant-id))

(defun e-subagent-live-find-pending-assignment (owner assignment)
  "Return the pending admission whose bounded ASSIGNMENT matches."
  (let ((found nil))
    (maphash (lambda (_key record)
              (when (and (null found)
                         (equal assignment (plist-get record :assignment)))
                (setq found record)))
            (e-subagent-live-pending-admissions owner))
    found))

(cl-defun e-subagent-live-install
    (owner board-id participant-id &key harness work-handle cancel callbacks
          progress report-admission)
  "Install live capabilities after durable admission settles.

Only transient execution capabilities and the latest bounded progress are
retained.  REPORT-ADMISSION and CALLBACKS are functions owned by the runner;
they do not turn this table into a durable result or status inventory."
  (let* ((key (e-subagent-live--key board-id participant-id))
         (record (list :board-id board-id
                       :participant-id participant-id
                       :harness harness
                       :work-handle work-handle
                       :cancel cancel
                       :callbacks callbacks
                       :progress (e-subagent-live--copy-progress progress)
                       :report-admission report-admission)))
    (when (gethash key (e-subagent-live-entries owner))
      (signal 'e-subagent-live-error
              (list "participant already has a live execution")))
    (unless (gethash key (e-subagent-live-pending-admissions owner))
      (signal 'e-subagent-live-error
              (list "durable admission is required before live installation")))
    (remhash key (e-subagent-live-pending-admissions owner))
    (puthash key record (e-subagent-live-entries owner))
    record))

(defun e-subagent-live-get (owner board-id participant-id)
  "Return a private live capability record, or nil when unavailable."
  (e-subagent-live--entry owner board-id participant-id))

(defun e-subagent-live-remove (owner board-id participant-id)
  "Drop all process-local state for BOARD-ID/PARTICIPANT-ID."
  (let ((key (e-subagent-live--key board-id participant-id)))
    (remhash key (e-subagent-live-pending-admissions owner))
    (remhash key (e-subagent-live-entries owner))))

(defun e-subagent-live-update (owner board-id participant-id property value)
  "Update one transient capability PROPERTY for a live child.

This is intentionally a narrow internal operation.  It cannot add durable
status, result, output, or terminal-history fields to the owner."
  (let ((entry (e-subagent-live--entry owner board-id participant-id)))
    (unless entry
      (signal 'e-subagent-live-error (list "live execution is unavailable")))
    (unless (memq property '(:harness :work-handle :cancel :callbacks
                             :report-admission))
      (signal 'e-subagent-live-error
              (list (format "unsupported live capability property: %S"
                            property))))
    (setf (plist-get entry property) value)
    entry))

(defun e-subagent-live-record-progress
    (owner board-id participant-id progress)
  "Replace the latest bounded in-flight PROGRESS snapshot."
  (let ((entry (e-subagent-live--entry owner board-id participant-id)))
    (when entry
      (setf (plist-get entry :progress)
            (e-subagent-live--copy-progress progress))
      (plist-get entry :progress))))

(defun e-subagent-live-progress (owner board-id participant-id)
  "Return the latest bounded progress snapshot, when live state exists."
  (let ((entry (e-subagent-live--entry owner board-id participant-id)))
    (and entry (plist-get entry :progress))))

(defun e-subagent-live-harness (owner board-id participant-id)
  "Return the live harness capability, or nil when unavailable."
  (let ((entry (e-subagent-live--entry owner board-id participant-id)))
    (and entry (plist-get entry :harness))))

(defun e-subagent-live-work-handle (owner board-id participant-id)
  "Return the live work handle, or nil when unavailable."
  (let ((entry (or (e-subagent-live--entry owner board-id participant-id)
                   (e-subagent-live--pending owner board-id participant-id))))
    (and entry (plist-get entry :work-handle))))

(defun e-subagent-live-cancel-function (owner board-id participant-id)
  "Return the live cancellation capability, or nil when unavailable."
  (let ((entry (e-subagent-live--entry owner board-id participant-id)))
    (and entry (plist-get entry :cancel))))

(defun e-subagent-live-report-admission (owner board-id participant-id)
  "Return the runner-owned report admission callback, if present."
  (let ((entry (or (e-subagent-live--entry owner board-id participant-id)
                   (e-subagent-live--pending owner board-id participant-id))))
    (and entry (plist-get entry :report-admission))))

(defun e-subagent-live-callbacks (owner board-id participant-id)
  "Return the runner-owned callback set, or nil when unavailable."
  (let ((entry (e-subagent-live--entry owner board-id participant-id)))
    (and entry (plist-get entry :callbacks))))

(defun e-subagent-live-find-by-session (owner board-id session-id)
  "Find an internal live record whose participant identity is SESSION-ID.

The function is intentionally private to runner coordination; it does not
provide a public list or status projection."
  (let ((key (e-subagent-live--key board-id session-id)))
    (gethash key (e-subagent-live-entries owner))))

(defun e-subagent-live-find-identity (owner participant-id)
  "Return the singleton live BOARD/PARTICIPANT identity for PARTICIPANT-ID.

This is a private failure-safety lookup for capability actions whose parent
Board binding has already closed.  It does not expose an inventory: callers
must already possess the participant identity.  Return nil when no live or
pending entry matches, and signal when more than one Board matches rather
than selecting an arbitrary cancellation target."
  (let ((matches 0)
        identity)
    (dolist (table (list (e-subagent-live-entries owner)
                         (e-subagent-live-pending-admissions owner)))
      (maphash
       (lambda (key _entry)
         (when (equal (cdr key) participant-id)
           (setq matches (1+ matches))
           (when (= matches 1)
             (setq identity (list (car key) participant-id)))))
       table))
    (cond
     ((zerop matches) nil)
     ((= matches 1) identity)
     (t
      (signal 'e-subagent-live-error
              (list "participant identity is ambiguous across Boards"
                    participant-id))))))

(defun e-subagent-live-reference (board-id participant-id)
  "Return an opaque waitable reference for the durable identity."
  (format "subagent:%s"
          (base64-encode-string
           (e-runtime-store-codec-encode-bounded
            (list (e-subagent-live--string board-id "board-id")
                  (e-subagent-live--string participant-id "participant-id"))
            512)
           t)))

(defun e-subagent-live-reference-identity (reference)
  "Decode an opaque live waitable REFERENCE into BOARD/PARTICIPANT IDs."
  (let* ((prefix "subagent:")
         (payload (and (stringp reference)
                       (string-prefix-p prefix reference)
                       (substring reference (length prefix))))
         (decoded (and payload
                       (condition-case nil
                           (e-runtime-store-codec-decode
                            (base64-decode-string payload t))
                         (error nil)))))
    (when (and (listp decoded)
               (= (length decoded) 2)
               (stringp (nth 0 decoded))
               (stringp (nth 1 decoded)))
      decoded)))

(provide 'e-subagent-live)

;;; e-subagent-live.el ends here
