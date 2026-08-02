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

(cl-defun e-events-make
    (&key id type session-id turn-id payload created-at
          activity-entry-id board-activity-sequence)
  "Create a core event plist.
TYPE and SESSION-ID are required.  ID and CREATED-AT default to in-process
values so tests can inject deterministic values.  Durable activity producers
may additionally carry their immutable entry identity and board publication
sequence without changing PAYLOAD."
  (unless type
    (signal 'wrong-type-argument '(e-event-type nil)))
  (unless session-id
    (signal 'wrong-type-argument '(e-event-session-id nil)))
  (let ((event (list :id (or id (e-events-next-id))
                     :type type
                     :session-id session-id
                     :turn-id turn-id
                     :payload payload
                     :created-at (or created-at (float-time)))))
    (when activity-entry-id
      (plist-put event :activity-entry-id activity-entry-id))
    (when board-activity-sequence
      (plist-put event :board-activity-sequence board-activity-sequence))
    event))

(defun e-events-type (event)
  "Return EVENT's type."
  (plist-get event :type))

(provide 'e-events)

;;; e-events.el ends here
