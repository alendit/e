;;; e-shells.el --- Presentation shell manifests for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shell manifests describe presentation shells for discovery and integration.
;; They do not define shell lifecycle, instances, or dependency resolution.

;;; Code:

(require 'cl-lib)

(cl-defstruct (e-shell-command (:constructor e-shell-command-create))
  id
  summary
  interactive
  function
  scope
  requires
  metadata)

(cl-defstruct (e-shell (:constructor e-shell-create))
  id
  name
  summary
  required-capabilities
  optional-capabilities
  commands
  keymaps
  metadata)

(defvar e-shell--registry (make-hash-table :test 'eq)
  "Registered presentation shell manifests keyed by shell id.")

(defun e-shell-register (shell)
  "Register SHELL for discovery and return it.
If another shell with the same id already exists, replace it."
  (unless (e-shell-p shell)
    (signal 'wrong-type-argument (list 'e-shell-p shell)))
  (unless (e-shell-id shell)
    (signal 'wrong-type-argument (list 'e-shell-id nil)))
  (puthash (e-shell-id shell) shell e-shell--registry)
  shell)

(defun e-shell-get (id)
  "Return the registered shell manifest for ID, or nil."
  (gethash id e-shell--registry))

(defun e-shell-list ()
  "Return registered shell manifests."
  (let (shells)
    (maphash (lambda (_id shell) (push shell shells)) e-shell--registry)
    (nreverse shells)))

(defun e-shell-command-by-id (shell command-id)
  "Return SHELL command descriptor matching COMMAND-ID, or nil."
  (cl-find command-id
           (e-shell-commands shell)
           :key #'e-shell-command-id
           :test #'eq))

(provide 'e-shells)

;;; e-shells.el ends here
