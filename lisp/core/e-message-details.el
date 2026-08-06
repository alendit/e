;;; e-message-details.el --- Capability-owned message presentation details -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capabilities may recognize semantic detail in an assistant message that a
;; presentation shell should expose without learning that capability's policy.
;; A provider returns small `e-message-detail' records: a compact summary for a
;; turn footer and a body that a shell may expand on demand.  Core validates and
;; aggregates these records; capabilities own their meaning and wording.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (e-message-detail
               (:constructor e-message-detail--create
                             (&key id summary body)))
  "One capability-owned presentation detail for a durable message.
ID is stable within the contributing capability.  SUMMARY is optional compact
text suitable for a turn footer.  BODY is the complete on-demand detail text."
  id
  summary
  body)

(cl-defun e-message-detail-create (&key id summary body)
  "Create and validate a message detail from ID, SUMMARY, and BODY."
  (unless (or (symbolp id)
              (and (stringp id) (not (string-empty-p id))))
    (signal 'wrong-type-argument (list 'e-message-detail-id id)))
  (when (and summary
             (not (and (stringp summary)
                       (not (string-empty-p (string-trim summary))))))
    (signal 'wrong-type-argument (list 'stringp summary)))
  (unless (and (stringp body) (not (string-empty-p (string-trim body))))
    (signal 'wrong-type-argument (list 'stringp body)))
  (e-message-detail--create :id id :summary summary :body body))

(defun e-message-details-collect (providers message context)
  "Collect message details from PROVIDERS for MESSAGE and CONTEXT.
Each provider is called as (PROVIDER MESSAGE CONTEXT) and returns nil, one
`e-message-detail', or a list of them.  Provider order and contribution order
are preserved."
  (let (result)
    (dolist (provider providers)
      (unless (functionp provider)
        (signal 'wrong-type-argument (list 'functionp provider)))
      (let ((provided (funcall provider message context)))
        (dolist (detail (cond
                         ((null provided) nil)
                         ((e-message-detail-p provided) (list provided))
                         ((listp provided) provided)
                         (t (signal 'wrong-type-argument
                                    (list 'e-message-detail-p provided)))))
          (unless (e-message-detail-p detail)
            (signal 'wrong-type-argument (list 'e-message-detail-p detail)))
          (push detail result))))
    (nreverse result)))

(provide 'e-message-details)

;;; e-message-details.el ends here
