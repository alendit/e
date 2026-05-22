;;; e-events.el --- Core event helpers for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Event constructors for the pure core runtime.

;;; Code:

(require 'cl-lib)

(defvar e-events--counter 0
  "Monotonic event id counter for in-process events.")

(defun e-events-next-id ()
  "Return a new in-process event id."
  (setq e-events--counter (1+ e-events--counter))
  (format "evt-%d" e-events--counter))

(cl-defun e-events-make (&key id type session-id turn-id payload created-at)
  "Create a core event plist.
TYPE and SESSION-ID are required.  ID and CREATED-AT default to in-process
values so tests can inject deterministic values without changing the public
shape."
  (unless type
    (signal 'wrong-type-argument '(e-event-type nil)))
  (unless session-id
    (signal 'wrong-type-argument '(e-event-session-id nil)))
  (list :id (or id (e-events-next-id))
        :type type
        :session-id session-id
        :turn-id turn-id
        :payload payload
        :created-at (or created-at (float-time))))

(defun e-events-type (event)
  "Return EVENT's type."
  (plist-get event :type))

(provide 'e-events)

;;; e-events.el ends here
