;;; e-search-providers.el --- Pluggable search providers for the search operation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The `search' operation is dispatched by URI scheme, and the registry that
;; maps (operation . scheme) to a method allows only one method per pair.  That
;; makes "a second capability also handles file:// search for a subtree"
;; inexpressible through method registration alone: the later registration
;; silently wins.
;;
;; This module adds an orthogonal seam.  A scheme's search method can consult a
;; provider registry before running its default backend.  A provider claims a
;; scope with a predicate over the resolved request; when it claims a request it
;; supplies the ranked result in the scheme's normal result shape.  Providers
;; are ordered by descending priority, so a narrowly scoped provider (e.g. one
;; repository root) can override a broad default without replacing it.
;;
;; The registry holds no filesystem or backend knowledge.  Side effects live in
;; the providers a capability registers.

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct (e-search-provider (:constructor e-search-provider-create))
  "A search backend that claims a scope of the search operation.
ID is a symbol identifying the provider.  PREDICATE is a function of one
request plist returning non-nil when the provider claims the request.  SEARCH is
a function of the same request plist returning a result plist with `:matches'
and `:truncated', matching the claimed scheme's normal result shape.  PRIORITY
orders providers; higher wins.  DESCRIPTION is human-facing."
  id
  predicate
  search
  (priority 0)
  description)

(defvar e-search-providers--registry nil
  "List of registered `e-search-provider' structs, newest first.")

(defun e-search-providers-reset ()
  "Remove every registered search provider."
  (setq e-search-providers--registry nil))

(defun e-search-providers-register (provider)
  "Register PROVIDER, replacing any existing provider with the same id."
  (unless (e-search-provider-p provider)
    (signal 'wrong-type-argument (list 'e-search-provider-p provider)))
  (e-search-providers-unregister (e-search-provider-id provider))
  (push provider e-search-providers--registry)
  provider)

(defun e-search-providers-unregister (id)
  "Remove the provider identified by ID, if present."
  (setq e-search-providers--registry
        (seq-remove (lambda (provider)
                      (eq (e-search-provider-id provider) id))
                    e-search-providers--registry))
  id)

(defun e-search-providers-list ()
  "Return registered providers ordered by descending priority."
  (seq-sort-by #'e-search-provider-priority #'>
               (copy-sequence e-search-providers--registry)))

(defun e-search-providers-provider-for (request)
  "Return the highest-priority provider that claims REQUEST, or nil."
  (seq-find (lambda (provider)
              (condition-case nil
                  (funcall (e-search-provider-predicate provider) request)
                (error nil)))
            (e-search-providers-list)))

(defun e-search-providers-run (provider request)
  "Run PROVIDER against REQUEST and return its result plist."
  (funcall (e-search-provider-search provider) request))

(defun e-search-providers-under-root-predicate (root)
  "Return a predicate claiming requests whose absolute path is under ROOT.
ROOT is compared against the request's `:absolute-path'; a request at ROOT
itself is claimed."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (lambda (request)
      (when-let ((path (plist-get request :absolute-path)))
        (let ((path (expand-file-name path)))
          (or (string-prefix-p root (file-name-as-directory path))
              (string-prefix-p root path)))))))

(provide 'e-search-providers)

;;; e-search-providers.el ends here
