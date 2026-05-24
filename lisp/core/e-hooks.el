;;; e-hooks.el --- Capability hook registries for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Hooks are deterministic lifecycle extensions contributed by capabilities and
;; run by harness-owned lifecycle services.

;;; Code:

(require 'cl-lib)

(define-error 'e-hooks-duplicate-id
  "Duplicate hook id for hook point")

(cl-defstruct (e-hook
               (:constructor e-hook--create
                             (&key id point handler description metadata)))
  id
  point
  handler
  description
  metadata)

(cl-defstruct (e-hooks-registry (:constructor e-hooks-registry-create))
  (hooks (make-hash-table :test 'eq)))

(defun e-hooks--validate-id (id)
  "Signal when ID is not a valid hook id."
  (unless (stringp id)
    (signal 'wrong-type-argument (list 'stringp id))))

(defun e-hooks--validate-point (point)
  "Signal when POINT is not a valid hook point."
  (unless (keywordp point)
    (signal 'wrong-type-argument (list 'keywordp point))))

(defun e-hook-create (&rest args)
  "Create an `e-hook' from keyword ARGS."
  (let ((hook (apply #'e-hook--create args)))
    (e-hooks--validate-id (e-hook-id hook))
    (e-hooks--validate-point (e-hook-point hook))
    (unless (functionp (e-hook-handler hook))
      (signal 'wrong-type-argument (list 'functionp (e-hook-handler hook))))
    hook))

(defun e-hooks-for-point (registry point)
  "Return hooks registered in REGISTRY for POINT."
  (e-hooks--validate-point point)
  (copy-sequence (gethash point (e-hooks-registry-hooks registry))))

(defun e-hooks-register (registry hook)
  "Register HOOK in REGISTRY and return HOOK."
  (unless (e-hook-p hook)
    (signal 'wrong-type-argument (list 'e-hook-p hook)))
  (let* ((point (e-hook-point hook))
         (id (e-hook-id hook))
         (hooks (e-hooks-for-point registry point)))
    (when (cl-find id hooks :key #'e-hook-id :test #'equal)
      (signal 'e-hooks-duplicate-id
              (list (format "Duplicate hook id %S for %S" id point))))
    (puthash point
             (sort (append hooks (list hook))
                   (lambda (left right)
                     (string< (e-hook-id left) (e-hook-id right))))
             (e-hooks-registry-hooks registry))
    hook))

(defun e-hooks-register-list (registry hooks)
  "Register HOOKS in REGISTRY and return REGISTRY."
  (dolist (hook hooks)
    (e-hooks-register registry hook))
  registry)

(defun e-hooks-run-reduce (registry point value context)
  "Run REGISTRY hooks for POINT as a reduce over VALUE.
Each hook handler receives the current value and CONTEXT, and returns the next
value."
  (dolist (hook (e-hooks-for-point registry point) value)
    (setq value
          (funcall (e-hook-handler hook) value context))))

(provide 'e-hooks)

;;; e-hooks.el ends here
