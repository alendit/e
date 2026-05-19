;;; e-backend.el --- Backend contract for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral backend adapter contract for the core runtime.

;;; Code:

(require 'cl-lib)

(cl-defstruct (e-backend
               (:constructor e-backend-create)
               (:conc-name e-backend--))
  name
  stream)

(cl-defun e-backend-stream (backend &key messages options on-item)
  "Stream a backend turn through BACKEND.
MESSAGES and OPTIONS are backend-neutral plists/lists.  ON-ITEM receives
backend-neutral stream items."
  (unless (functionp (e-backend--stream backend))
    (signal 'wrong-type-argument (list 'functionp (e-backend--stream backend))))
  (funcall (e-backend--stream backend)
           :messages messages
           :options options
           :on-item on-item))

(cl-defun e-backend-fake-create (&key name items)
  "Create a fake backend that streams ITEMS synchronously."
  (e-backend-create
   :name (or name "fake")
   :stream (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (dolist (item items)
                (funcall on-item item))))))

(provide 'e-backend)

;;; e-backend.el ends here
