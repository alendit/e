;;; e-backend.el --- Backend contract for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral backend adapter contract for the core runtime.

;;; Code:

(require 'cl-lib)

(cl-defstruct (e-backend-request (:constructor e-backend-request-create))
  cancel
  metadata)

(cl-defstruct (e-backend
               (:constructor e-backend-create)
               (:conc-name e-backend--))
  name
  stream)

(defvar e-backend--request-start-callback nil
  "Dynamically scoped callback for backend request handles.")

(defun e-backend-note-request-started (request)
  "Publish REQUEST as the active provider request for the current stream."
  (when e-backend--request-start-callback
    (funcall e-backend--request-start-callback request))
  request)

(defun e-backend-cancel-request (request)
  "Cancel REQUEST when it has a provider cancellation function."
  (when-let ((cancel (and (e-backend-request-p request)
                          (e-backend-request-cancel request))))
    (funcall cancel)))

(cl-defun e-backend-stream
    (backend &key messages options on-item on-request-start)
  "Stream a backend turn through BACKEND.
MESSAGES and OPTIONS are backend-neutral plists/lists.  ON-ITEM receives
backend-neutral stream items.  ON-REQUEST-START receives an optional
`e-backend-request' handle when the adapter can expose request state."
  (unless (functionp (e-backend--stream backend))
    (signal 'wrong-type-argument (list 'functionp (e-backend--stream backend))))
  (let ((e-backend--request-start-callback on-request-start))
    (funcall (e-backend--stream backend)
             :messages messages
             :options options
             :on-item on-item)))

(cl-defun e-backend-fake-create (&key name items cancel-function)
  "Create fake backend NAME that streams ITEMS synchronously."
  (e-backend-create
   :name (or name "fake")
   :stream (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (when cancel-function
                (e-backend-note-request-started
                 (e-backend-request-create :cancel cancel-function)))
              (dolist (item items)
                (funcall on-item item))))))

(provide 'e-backend)

;;; e-backend.el ends here
