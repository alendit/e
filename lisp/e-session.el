;;; e-session.el --- Session store for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; In-memory session storage for the pure core runtime.

;;; Code:

(require 'cl-lib)

(define-error 'e-session-missing "Session does not exist")
(define-error 'e-session-duplicate "Session already exists")

(cl-defstruct (e-session-store (:constructor e-session-store-create))
  (sessions (make-hash-table :test 'equal)))

(cl-defun e-session-create (store &key id metadata)
  "Create a session in STORE with ID and METADATA."
  (when (gethash id (e-session-store-sessions store))
    (signal 'e-session-duplicate (list id)))
  (let ((session (list :id id
                       :metadata metadata
                       :messages nil
                       :current-branch nil
                       :compactions nil)))
    (puthash id session (e-session-store-sessions store))
    session))

(defun e-session-get (store session-id)
  "Return SESSION-ID from STORE."
  (or (gethash session-id (e-session-store-sessions store))
      (signal 'e-session-missing (list session-id))))

(defun e-session-messages (store session-id)
  "Return messages for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-get store session-id) :messages)))

(defun e-session-append-message (store session-id message)
  "Append MESSAGE to SESSION-ID in STORE."
  (let* ((session (e-session-get store session-id))
         (messages (plist-get session :messages)))
    (plist-put session :messages (append messages (list message)))
    message))

(defun e-session-clear-messages (store session-id)
  "Clear all messages for SESSION-ID in STORE."
  (let ((session (e-session-get store session-id)))
    (plist-put session :messages nil)
    session))

(provide 'e-session)

;;; e-session.el ends here
