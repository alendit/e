;;; e-subagent-registry.el --- Subagent work registry for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; In-memory normalized registry for spawned subagents.  A record ties a child
;; session to its parent through lineage fields, tracks a compact result, and
;; carries an opaque cancel function so the parent can interrupt a running
;; child.  The registry is the single source of truth for subagent status; both
;; the direct-turn and queue schedules funnel their settle callbacks here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar e-subagent-registry-change-functions nil
  "Functions run with a registry after any subagent record changes.
List buffers hook onto this to track live subagent status.")

(cl-defstruct (e-subagent-registry
               (:constructor e-subagent-registry-create))
  (records (make-hash-table :test 'equal))
  (order nil)
  (sequence 0))

(defun e-subagent-registry--notify (registry)
  "Run change hooks for REGISTRY."
  (run-hook-with-args 'e-subagent-registry-change-functions registry))

(defun e-subagent-registry--next-id (registry)
  "Return the next stable subagent id from REGISTRY."
  (setf (e-subagent-registry-sequence registry)
        (1+ (e-subagent-registry-sequence registry)))
  (format "sub_%06d" (e-subagent-registry-sequence registry)))

(defun e-subagent-registry--record (registry subagent-id)
  "Return the mutable internal record SUBAGENT-ID from REGISTRY, or signal."
  (or (gethash subagent-id (e-subagent-registry-records registry))
      (user-error "Unknown subagent id: %s" subagent-id)))

(defun e-subagent-registry-normalize (record)
  "Return the model-facing normalized copy of internal RECORD."
  (list :subagent-id (plist-get record :subagent-id)
        :type (plist-get record :type)
        :role (plist-get record :role)
        :status (plist-get record :status)
        :session-id (plist-get record :session-id)
        :parent-session-id (plist-get record :parent-session-id)
        :label (plist-get record :label)
        :result-summary (plist-get record :result-summary)
        :outputs (plist-get record :outputs)
        :error (plist-get record :error)))

(cl-defun e-subagent-registry-register
    (registry &key type role session-id parent-session-id label schedule
              child-harness)
  "Register a new subagent record in REGISTRY and return its normalized form.
The record starts `queued'; the runner transitions it as the child turn
progresses.  CHILD-HARNESS is the live harness running the child, stored
internally so steer/read reach the child session on its own harness."
  (let* ((subagent-id (e-subagent-registry--next-id registry))
         (record (list :subagent-id subagent-id
                       :type type
                       :role role
                       :status 'queued
                       :session-id session-id
                       :parent-session-id parent-session-id
                       :label label
                       :schedule schedule
                       :child-harness child-harness
                       :result-summary nil
                       :outputs nil
                       :reported nil
                       :error nil
                       :cancel nil
                       :created-at (float-time)
                       :finished-at nil)))
    (puthash subagent-id record (e-subagent-registry-records registry))
    (setf (e-subagent-registry-order registry)
          (append (e-subagent-registry-order registry) (list subagent-id)))
    (e-subagent-registry--notify registry)
    (e-subagent-registry-normalize record)))

(defun e-subagent-registry-update (registry subagent-id &rest fields)
  "Apply FIELDS to SUBAGENT-ID's record in REGISTRY and return normalized form."
  (let ((record (e-subagent-registry--record registry subagent-id)))
    (while fields
      (let ((key (pop fields)))
        (when fields
          (plist-put record key (pop fields)))))
    (e-subagent-registry--notify registry)
    (e-subagent-registry-normalize record)))

(defun e-subagent-registry-get (registry subagent-id)
  "Return the normalized record for SUBAGENT-ID in REGISTRY."
  (e-subagent-registry-normalize
   (e-subagent-registry--record registry subagent-id)))

(defun e-subagent-registry-cancel-function (registry subagent-id)
  "Return the cancel function stored for SUBAGENT-ID, or nil."
  (plist-get (e-subagent-registry--record registry subagent-id) :cancel))

(defun e-subagent-registry-child-harness (registry subagent-id)
  "Return the live child harness stored for SUBAGENT-ID, or nil."
  (plist-get (e-subagent-registry--record registry subagent-id) :child-harness))

(defun e-subagent-registry-reported-p (registry subagent-id)
  "Return non-nil when SUBAGENT-ID has a child-reported structured result."
  (plist-get (e-subagent-registry--record registry subagent-id) :reported))

(defun e-subagent-registry-status (registry subagent-id)
  "Return the status symbol for SUBAGENT-ID in REGISTRY."
  (plist-get (e-subagent-registry--record registry subagent-id) :status))

(defun e-subagent-registry-list (registry &optional parent-session-id)
  "Return normalized subagent records in REGISTRY, newest-first.
When PARENT-SESSION-ID is non-nil, return only that parent's direct children."
  (let (records)
    (dolist (subagent-id (e-subagent-registry-order registry))
      (let ((record (gethash subagent-id (e-subagent-registry-records registry))))
        (when (or (null parent-session-id)
                  (equal (plist-get record :parent-session-id)
                         parent-session-id))
          (push (e-subagent-registry-normalize record) records))))
    records))

(defun e-subagent-registry-find-by-session (registry session-id)
  "Return the normalized record whose child SESSION-ID matches, or nil."
  (catch 'found
    (dolist (subagent-id (e-subagent-registry-order registry))
      (let ((record (gethash subagent-id
                             (e-subagent-registry-records registry))))
        (when (equal (plist-get record :session-id) session-id)
          (throw 'found (e-subagent-registry-normalize record)))))
    nil))

(provide 'e-subagent-registry)

;;; e-subagent-registry.el ends here
