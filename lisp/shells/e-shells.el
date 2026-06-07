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

(cl-defstruct (e-shell-registration
               (:constructor e-shell-registration-create))
  id
  shell
  owner-kind
  owner-id
  project-root
  metadata)

(defvar e-shell--registry (make-hash-table :test 'eq)
  "Registered presentation shell manifests keyed by shell id.")

(defvar e-shell--scoped-registry (make-hash-table :test 'eq)
  "Layer-owned shell registrations keyed by owning harness.")

(defun e-shell--validate-id (id)
  "Signal unless ID is a valid shell id."
  (unless (symbolp id)
    (signal 'wrong-type-argument (list 'symbolp id))))

(defun e-shell--validate-command (command)
  "Signal unless COMMAND is a valid shell command descriptor."
  (unless (e-shell-command-p command)
    (signal 'wrong-type-argument (list 'e-shell-command-p command)))
  (e-shell--validate-id (e-shell-command-id command))
  command)

(defun e-shell-validate (shell)
  "Signal unless SHELL is a valid shell manifest."
  (unless (e-shell-p shell)
    (signal 'wrong-type-argument (list 'e-shell-p shell)))
  (e-shell--validate-id (e-shell-id shell))
  (mapc #'e-shell--validate-command (e-shell-commands shell))
  shell)

(defun e-shell-register (shell)
  "Register SHELL for discovery and return it.
If another shell with the same id already exists, replace it."
  (e-shell-validate shell)
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

(defun e-shell--harness-registrations (harness)
  "Return scoped shell registrations for HARNESS."
  (gethash harness e-shell--scoped-registry))

(defun e-shell--set-harness-registrations (harness registrations)
  "Set HARNESS scoped shell REGISTRATIONS."
  (if registrations
      (puthash harness registrations e-shell--scoped-registry)
    (remhash harness e-shell--scoped-registry)))

(cl-defun e-shell-register-layer-shells
    (harness owner-id shells &key project-root metadata)
  "Register layer-owned SHELLS for HARNESS under OWNER-ID.
Global shells keep precedence: a layer shell whose id collides with a global
shell is reported and skipped instead of replacing the built-in manifest."
  (e-shell-unregister-layer-shells harness owner-id)
  (let ((registrations (e-shell--harness-registrations harness))
        (registered nil)
        (seen nil))
    (dolist (shell shells)
      (e-shell-validate shell)
      (let ((id (e-shell-id shell)))
        (cond
         ((gethash id e-shell--registry)
          (message "e-shells: ignoring layer shell id %s from %s because a global shell already exists"
                   id owner-id))
         ((or (memq id seen)
              (cl-find id registrations
                       :key #'e-shell-registration-id
                       :test #'eq))
          (message "e-shells: ignoring duplicate layer shell id %s from %s"
                   id owner-id))
         (t
          (push id seen)
          (push (e-shell-registration-create
                 :id id
                 :shell shell
                 :owner-kind 'layer
                 :owner-id owner-id
                 :project-root project-root
                 :metadata metadata)
                registered)))))
    (let ((ordered (nreverse registered)))
      (e-shell--set-harness-registrations
       harness (append ordered registrations))
      (mapcar #'e-shell-registration-shell ordered))))

(defun e-shell-unregister-layer-shells (harness owner-id)
  "Unregister layer-owned shells for HARNESS and OWNER-ID."
  (let ((remaining nil)
        (removed nil))
    (dolist (registration (e-shell--harness-registrations harness))
      (if (and (eq (e-shell-registration-owner-kind registration) 'layer)
               (eq (e-shell-registration-owner-id registration) owner-id))
          (push registration removed)
        (push registration remaining)))
    (e-shell--set-harness-registrations harness (nreverse remaining))
    (mapcar #'e-shell-registration-shell (nreverse removed))))

(defun e-shell-clear-harness-shells (harness)
  "Clear all scoped shell registrations for HARNESS."
  (let ((registrations (e-shell--harness-registrations harness)))
    (remhash harness e-shell--scoped-registry)
    (mapcar #'e-shell-registration-shell registrations)))

(defun e-shell-list-active (&optional harness)
  "Return shell manifests active for HARNESS.
Global shells are always included.  When HARNESS is non-nil, include scoped
layer-owned shells for that harness."
  (append (e-shell-list)
          (mapcar #'e-shell-registration-shell
                  (e-shell--harness-registrations harness))))

(defun e-shell-get-active (id &optional harness)
  "Return active shell manifest for ID in HARNESS, or nil.
Global shell ids take precedence over layer-owned shell ids."
  (or (e-shell-get id)
      (when harness
        (when-let ((registration
                    (cl-find id (e-shell--harness-registrations harness)
                             :key #'e-shell-registration-id
                             :test #'eq)))
          (e-shell-registration-shell registration)))))

(defun e-shell-command-by-id (shell command-id)
  "Return SHELL command descriptor matching COMMAND-ID, or nil."
  (cl-find command-id
           (e-shell-commands shell)
           :key #'e-shell-command-id
           :test #'eq))

(provide 'e-shells)

;;; e-shells.el ends here
