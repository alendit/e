;;; e-harness-registry.el --- Harness registry service for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Registry for named live harness instances and lazy harness factories.
;; The registry is intentionally narrow: it does not know capability semantics,
;; default selection policy, or provider details.

;;; Code:

(require 'cl-lib)
(require 'e-harness)

(define-error 'e-harness-registry-missing
  "No harness instance or factory is registered for id")
(define-error 'e-harness-registry-stale-generation
  "Harness registry object generation is no longer current")

(defvar e-harness-registry--instances (make-hash-table :test 'equal)
  "Harness instances keyed by registry id.")

(defvar e-harness-registry--factories (make-hash-table :test 'equal)
  "Harness factories keyed by registry id.")

(defvar e-harness-registry--generations (make-hash-table :test 'equal)
  "Monotonic concrete-object generations keyed by registry id.")

(defvar e-harness-registry--invalidation-events (make-hash-table :test 'equal)
  "Latest bounded replacement or clear event keyed by registry id.")

(defun e-harness-registry--validate-id (id)
  "Signal an error when ID is not a valid registry key."
  (unless (keywordp id)
    (signal 'wrong-type-argument (list 'keywordp id))))

(defun e-harness-registry--validate-factory (factory)
  "Signal an error when FACTORY cannot be called."
  (unless (functionp factory)
    (signal 'wrong-type-argument (list 'functionp factory))))

(defun e-harness-registry-register-factory (id factory)
  "Register FACTORY as the lazy harness factory for ID.
Replacing the factory does not replace any already cached harness instance."
  (e-harness-registry--validate-id id)
  (e-harness-registry--validate-factory factory)
  (puthash id factory e-harness-registry--factories)
  factory)

(defun e-harness-registry-register (id harness)
  "Register HARNESS as the current harness instance for ID."
  (e-harness-registry--validate-id id)
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (let ((previous (gethash id e-harness-registry--instances)))
    (unless (eq previous harness)
      (let* ((previous-generation
              (gethash id e-harness-registry--generations 0))
             (generation (1+ previous-generation)))
        (puthash id generation e-harness-registry--generations)
        (when previous
          (puthash id
                   (list :kind 'replaced :harness-id id
                         :previous-generation previous-generation
                         :generation generation)
                   e-harness-registry--invalidation-events))))
    (puthash id harness e-harness-registry--instances))
  harness)

(defun e-harness-registry-get (id)
  "Return the registered harness instance for ID, or nil."
  (e-harness-registry--validate-id id)
  (gethash id e-harness-registry--instances))

(defun e-harness-registry-generation (id)
  "Return ID's current concrete-object generation, or zero if never created."
  (e-harness-registry--validate-id id)
  (gethash id e-harness-registry--generations 0))

(defun e-harness-registry-resolve (id generation)
  "Return ID's live harness only when GENERATION is still current."
  (e-harness-registry--validate-id id)
  (let ((harness (gethash id e-harness-registry--instances))
        (current (e-harness-registry-generation id)))
    (unless (and harness (equal generation current))
      (signal 'e-harness-registry-stale-generation
              (list id generation current)))
    harness))

(defun e-harness-registry-invalidation-event (id)
  "Return ID's latest bounded replacement/clear invalidation event, if any."
  (e-harness-registry--validate-id id)
  (when-let* ((event (gethash id e-harness-registry--invalidation-events)))
    (copy-sequence event)))

(defun e-harness-registry-get-or-create (id)
  "Return the harness instance for ID, lazily creating it when needed.
Signal `e-harness-registry-missing' when ID has neither an instance nor a
factory."
  (e-harness-registry--validate-id id)
  (or (gethash id e-harness-registry--instances)
      (let ((factory (gethash id e-harness-registry--factories)))
        (unless factory
          (signal 'e-harness-registry-missing (list id)))
        (e-harness-registry-register id (funcall factory)))))

(defun e-harness-registry-list ()
  "Return sorted registry ids with either registered instances or factories."
  (let (ids)
    (maphash (lambda (id _harness)
               (push id ids))
             e-harness-registry--instances)
    (maphash (lambda (id _factory)
               (cl-pushnew id ids :test #'equal))
             e-harness-registry--factories)
    (sort ids (lambda (left right)
                (string< (symbol-name left)
                         (symbol-name right))))))

(defun e-harness-registry-clear-instance (id)
  "Clear the cached harness instance for ID.
The registered factory, if any, is preserved."
  (e-harness-registry--validate-id id)
  (when (gethash id e-harness-registry--instances)
    (let* ((previous-generation
            (gethash id e-harness-registry--generations 0))
           (generation (1+ previous-generation)))
      (remhash id e-harness-registry--instances)
      (puthash id generation e-harness-registry--generations)
      (puthash id
               (list :kind 'cleared :harness-id id
                     :previous-generation previous-generation
                     :generation generation)
               e-harness-registry--invalidation-events)))
  nil)

(provide 'e-harness-registry)

;;; e-harness-registry.el ends here
