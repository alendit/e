;;; e-layers.el --- Harness-owned layer bundles for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Layers are presets over capability objects and defaults.  The harness
;; activates layers; presentation shells only choose how to expose that active
;; runtime state to users.

;;; Code:

(require 'cl-lib)

(define-error 'e-layer-registry-missing
  "No known layer is registered for id")

(cl-defstruct (e-layer (:constructor e-layer-create))
  id
  name
  capabilities
  defaults)

(cl-defstruct (e-layer-spec (:constructor e-layer-spec-create))
  id
  name
  summary
  feature
  factory
  metadata)

(defvar e-layer--registry (make-hash-table :test 'eq)
  "Known layer specs keyed by layer id.")

(defun e-layer--validate-id (id)
  "Signal when ID is not a valid layer id."
  (unless (symbolp id)
    (signal 'wrong-type-argument (list 'symbolp id))))

(defun e-layer--valid-factory-p (factory)
  "Return non-nil when FACTORY can identify a layer factory."
  (or (functionp factory) (symbolp factory)))

(defun e-layer-register (spec)
  "Register known layer SPEC and return it.
If another spec with the same id already exists, replace it."
  (unless (e-layer-spec-p spec)
    (signal 'wrong-type-argument (list 'e-layer-spec-p spec)))
  (e-layer--validate-id (e-layer-spec-id spec))
  (unless (e-layer--valid-factory-p (e-layer-spec-factory spec))
    (signal 'wrong-type-argument
            (list 'functionp-or-symbolp (e-layer-spec-factory spec))))
  (puthash (e-layer-spec-id spec) spec e-layer--registry)
  spec)

(defun e-layer-get (id)
  "Return the known layer spec for ID, or nil."
  (e-layer--validate-id id)
  (gethash id e-layer--registry))

(defun e-layer-list ()
  "Return known layer specs sorted by layer id."
  (let (specs)
    (maphash (lambda (_id spec) (push spec specs)) e-layer--registry)
    (sort specs (lambda (left right)
                  (string< (symbol-name (e-layer-spec-id left))
                           (symbol-name (e-layer-spec-id right)))))))

(defun e-layer--factory-accepts-directory-p (factory)
  "Return non-nil when FACTORY accepts a directory argument."
  (condition-case nil
      (let ((max-arity (cdr (func-arity factory))))
        (or (eq max-arity 'many)
            (> max-arity 0)))
    (error nil)))

(defun e-layer-create-registered (id &optional directory)
  "Create the known layer registered for ID.
Pass DIRECTORY to factories that accept a root argument."
  (let ((spec (e-layer-get id)))
    (unless spec
      (signal 'e-layer-registry-missing (list id)))
    (when (e-layer-spec-feature spec)
      (require (e-layer-spec-feature spec)))
    (unless (functionp (e-layer-spec-factory spec))
      (signal 'wrong-type-argument
              (list 'functionp (e-layer-spec-factory spec))))
    (let* ((factory (e-layer-spec-factory spec))
           (layer (if (e-layer--factory-accepts-directory-p factory)
                      (funcall factory directory)
                    (funcall factory))))
      (unless (e-layer-p layer)
        (signal 'wrong-type-argument (list 'e-layer-p layer)))
      (unless (eq (e-layer-id layer) id)
        (signal 'wrong-type-argument
                (list 'e-layer-id (e-layer-id layer))))
      layer)))

(provide 'e-layers)

;;; e-layers.el ends here
