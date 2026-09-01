;;; e-voice-storage.el --- Voice tell durable storage port -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)

(define-error 'e-voice-storage-error "Voice storage error")

(cl-defstruct (e-voice-storage
               (:constructor e-voice-storage--create)
               (:conc-name e-voice-storage--))
  runtime call-operation)

(defun e-voice-storage-runtime (storage)
  "Return STORAGE's shared runtime-store identity."
  (e-voice-storage--runtime storage))

(defun e-voice-storage--call (storage operation &rest arguments)
  "Invoke STORAGE OPERATION with ARGUMENTS."
  (unless (e-voice-storage-p storage)
    (signal 'wrong-type-argument (list 'e-voice-storage-p storage)))
  (apply (e-voice-storage--call-operation storage) operation arguments))

(defun e-voice-storage-record
    (storage key label description last cap)
  "Atomically update KEY and enforce LRU CAP."
  (e-voice-storage--call storage 'record key label description last cap))

(defun e-voice-storage-list (storage &optional limit)
  "Return up to LIMIT tells in most-recently-used order."
  (e-voice-storage--call storage 'list (or limit 1024)))

(defun e-voice-storage-clear (storage)
  "Delete every durable voice tell."
  (e-voice-storage--call storage 'clear))

(provide 'e-voice-storage)

;;; e-voice-storage.el ends here
