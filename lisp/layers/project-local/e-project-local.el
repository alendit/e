;;; e-project-local.el --- Project-local capability discovery layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Discovers extension packages a repository ships under `.e/layers/' and
;; compatibility capabilities under `.e/capabilities/'.  Loading repo elisp is a
;; trust boundary, so project loaders are only loaded for project roots the user
;; has allowlisted via `e-project-local-allowed-roots'.  Discovered
;; capabilities and shell manifests are bundled into a single `project-local'
;; layer that participates in normal turn context, tool dispatch, resource
;; registration, and shell discovery.
;;
;; The discovery walk and capability/layer wrapping live here in the layer
;; (a shell-side concern with real side effects); the harness still owns
;; lifecycle and dispatch.  Unknown roots are reported, never executed, and id
;; collisions with built-in or other project capabilities are reported rather
;; than silently overriding.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-layers)
(require 'e-skills)
(require 'e-shells)
(require 'subr-x)

(defgroup e-project-local nil
  "Project-local capability discovery for e."
  :group 'e
  :prefix "e-project-local-")

(defcustom e-project-local-allowed-roots nil
  "Project roots allowed to load project-local `.e/' extension elisp.

Each entry is a directory.  A discovered capability or layer is loaded only when
its project root is at or below an allowed root.  Loading repo elisp runs with
full Emacs privileges, so only add roots you trust."
  :type '(repeat directory)
  :group 'e-project-local)

(defconst e-project-local-instructions
  "Project-local capabilities discovered from this repository are active. Their tools, resources, and skills behave like built-in capabilities for this turn."
  "Instructions contributed when project-local capabilities are active.")

(defconst e-project-local-capabilities-subdir ".e/capabilities"
  "Directory, relative to a project ancestor, holding project capabilities.")

(defconst e-project-local-layers-subdir ".e/layers"
  "Directory, relative to a project ancestor, holding project layers.")

;;;; Registration contract used by capability.el loaders

(defvar e-project-local--registrations nil
  "Capability factories registered by the currently loading `capability.el'.
Bound dynamically around each load; a registration outside a load is an error.")

(defvar e-project-local--loading nil
  "Path of the `capability.el' currently being loaded, or nil.
Dynamically bound by `e-project-local--load-capability-file' so registration
calls can detect they are running inside a sanctioned load.")

(defvar e-project-local--layer-registrations nil
  "Layer factories registered by the currently loading `layer.el'.
Bound dynamically around each load; a registration outside a load is an error.")

(defvar e-project-local--layer-loading nil
  "Path of the `layer.el' currently being loaded, or nil.
Dynamically bound by `e-project-local--load-layer-file' so registration calls
can detect they are running inside a sanctioned project layer load.")

(cl-defstruct (e-project-local-registration
               (:constructor e-project-local-registration-create))
  id
  factory)

(cl-defstruct (e-project-local-layer-registration
               (:constructor e-project-local-layer-registration-create))
  id
  factory)

(cl-defun e-project-capability-register (&key id factory)
  "Register a project capability FACTORY under ID from a `capability.el' loader.

ID is a symbol naming the capability.  FACTORY is called with the discovered
capability directory and must return an `e-capability'.  This is only valid
while a `capability.el' is being loaded by `e-project-local'."
  (unless e-project-local--loading
    (error "`e-project-capability-register' called outside capability load"))
  (unless (symbolp id)
    (signal 'wrong-type-argument (list 'symbolp id)))
  (unless (functionp factory)
    (signal 'wrong-type-argument (list 'functionp factory)))
  (push (e-project-local-registration-create :id id :factory factory)
        e-project-local--registrations)
  id)

(cl-defun e-project-layer-register (&key id factory)
  "Register a project layer FACTORY under ID from a `layer.el' loader.

ID is a symbol naming the project layer.  FACTORY is called with the discovered
layer directory and must return an `e-layer' with the same id.  This is only
valid while a `layer.el' is being loaded by `e-project-local'."
  (unless e-project-local--layer-loading
    (error "`e-project-layer-register' called outside project layer load"))
  (unless (symbolp id)
    (signal 'wrong-type-argument (list 'symbolp id)))
  (unless (functionp factory)
    (signal 'wrong-type-argument (list 'functionp factory)))
  (push (e-project-local-layer-registration-create :id id :factory factory)
        e-project-local--layer-registrations)
  id)

;;;; Discovery

(defun e-project-local--extension-directories (directory subdir)
  "Return project extension directories named SUBDIR discovered from DIRECTORY."
  (let (directories)
    (dolist (dir (e-skills-ancestor-directories directory))
      (let ((extensions (expand-file-name subdir dir)))
        (when (file-directory-p extensions)
          (dolist (entry (e-skills-skill-directories extensions))
            (push entry directories)))))
    directories))

(defun e-project-local--capability-directories (directory)
  "Return project capability directories discovered from DIRECTORY upward.
Outer ancestors come first so nearer directories win on id collision."
  (e-project-local--extension-directories
   directory e-project-local-capabilities-subdir))

(defun e-project-local--layer-directories (directory)
  "Return project layer directories discovered from DIRECTORY upward.
Outer ancestors come first so nearer directories win on id collision."
  (e-project-local--extension-directories
   directory e-project-local-layers-subdir))

(defun e-project-local--root-allowed-p (directory)
  "Return non-nil when DIRECTORY is at or below an allowed root."
  (let ((target (file-truename (e-skills-normalize-directory directory))))
    (cl-some
     (lambda (root)
       (let ((allowed (file-truename (e-skills-normalize-directory root))))
         (string-prefix-p allowed target)))
     e-project-local-allowed-roots)))

;;;; Loading

(defun e-project-local--load-capability-file (capability-directory)
  "Load `capability.el' in CAPABILITY-DIRECTORY and return its registrations.
Returns a list of `e-project-local-registration' values in registration order.
Returns nil when no `capability.el' exists."
  (let ((file (expand-file-name "capability.el" capability-directory)))
    (when (e-skills-readable-file-p file)
      (let ((e-project-local--loading file)
            (e-project-local--registrations nil))
        (load file nil 'nomessage)
        (nreverse e-project-local--registrations)))))

(defun e-project-local--load-layer-file (layer-directory)
  "Load `layer.el' in LAYER-DIRECTORY and return its registrations.
Returns a list of `e-project-local-layer-registration' values in registration
order.  Returns nil when no `layer.el' exists."
  (let ((file (expand-file-name "layer.el" layer-directory)))
    (when (e-skills-readable-file-p file)
      (let ((e-project-local--layer-loading file)
            (e-project-local--layer-registrations nil))
        (load file nil 'nomessage)
        (nreverse e-project-local--layer-registrations)))))

(defun e-project-local--instantiate-registration (registration directory)
  "Return the capability built by REGISTRATION for DIRECTORY.
Signals when the factory does not return an `e-capability' with matching id."
  (let ((capability (funcall (e-project-local-registration-factory registration)
                             directory)))
    (unless (e-capability-p capability)
      (signal 'wrong-type-argument (list 'e-capability-p capability)))
    (unless (eq (e-capability-id capability)
                (e-project-local-registration-id registration))
      (signal 'wrong-type-argument
              (list 'e-project-local-registration-id
                    (e-capability-id capability))))
    capability))

(defun e-project-local--validate-layer-shells (layer)
  "Validate shell manifests contributed by LAYER."
  (mapc #'e-shell-validate (e-layer-shells layer))
  layer)

(defun e-project-local--instantiate-layer-registration (registration directory)
  "Return the layer built by REGISTRATION for DIRECTORY.
Signals when the factory does not return an `e-layer' with matching id."
  (let ((layer (funcall (e-project-local-layer-registration-factory
                        registration)
                        directory)))
    (unless (e-layer-p layer)
      (signal 'wrong-type-argument (list 'e-layer-p layer)))
    (unless (eq (e-layer-id layer)
                (e-project-local-layer-registration-id registration))
      (signal 'wrong-type-argument
              (list 'e-project-local-layer-registration-id
                    (e-layer-id layer))))
    (dolist (capability (e-layer-capabilities layer))
      (unless (e-capability-p capability)
        (signal 'wrong-type-argument (list 'e-capability-p capability))))
    (e-project-local--validate-layer-shells layer)))

(defun e-project-local--capability-skill-resources (directory)
  "Return a resource provider for capability-scoped skills under DIRECTORY.
Skills live in `<DIRECTORY>/skills/<slug>/SKILL.md' and register as read-only
e://<capability>/skills/project/<slug> resources.  Returns nil when none."
  (let ((skills (e-skills-specs-from-directory
                 "project" (expand-file-name "skills" directory))))
    (when skills
      (lambda (store capability)
        (dolist (skill skills)
          (e-store-register
           store
           (e-capability-id capability)
           (e-skill-spec-path skill)
           :description (e-skill-spec-description skill)
           :reader (lambda (_entry range)
                     (funcall (e-skill-spec-reader skill) skill range))
           :metadata (e-skill-spec-metadata skill)))))))

(defun e-project-local--with-capability-skills (capability directory)
  "Return CAPABILITY extended with capability-scoped skill resources in DIRECTORY."
  (if-let ((provider (e-project-local--capability-skill-resources directory)))
      (e-capability-create
       :id (e-capability-id capability)
       :name (e-capability-name capability)
       :instructions (e-capability-instructions capability)
       :tools (e-capability-tools capability)
       :resource-methods (e-capability-resource-methods capability)
       :resources (append (e-capability-resources capability)
                          (list provider))
       :context-providers (e-capability-context-providers capability)
       :actions (e-capability-actions capability)
       :hooks (e-capability-hooks capability)
       :instruction-priority (e-capability-instruction-priority capability)
       :config-options (e-capability-config-options capability)
       :config (e-capability-config capability))
    capability))

(defun e-project-local--discover-capabilities (directory)
  "Return capabilities discovered for project root DIRECTORY.
Walks `.e/capabilities/' ancestors, skips capability.el under roots that are
not allowlisted (reporting them), instantiates registered factories, folds in
capability-scoped skills, and drops duplicate ids (nearer directory wins)."
  (let (capabilities seen)
    (dolist (capability-directory
             (e-project-local--capability-directories directory))
      (cond
       ((not (e-project-local--root-allowed-p capability-directory))
        (message "e-project-local: skipping unallowed capability %s (add its root to `e-project-local-allowed-roots')"
                 (abbreviate-file-name capability-directory)))
       (t
        (dolist (registration
                 (e-project-local--load-capability-file capability-directory))
          (let ((id (e-project-local-registration-id registration)))
            (if (memq id seen)
                (message "e-project-local: ignoring duplicate capability id %s from %s"
                         id (abbreviate-file-name capability-directory))
              (push id seen)
              (push (e-project-local--with-capability-skills
                     (e-project-local--instantiate-registration
                      registration capability-directory)
                     capability-directory)
                    capabilities)))))))
    (nreverse capabilities)))

(defun e-project-local--discover-layers (directory)
  "Return project layers discovered for project root DIRECTORY.
Walks `.e/layers/' ancestors, skips layer.el under roots that are not
allowlisted (reporting them), instantiates registered factories, and drops
duplicate ids (nearer directory wins)."
  (let (layers seen)
    (dolist (layer-directory
             (e-project-local--layer-directories directory))
      (cond
       ((not (e-project-local--root-allowed-p layer-directory))
        (message "e-project-local: skipping unallowed project layer %s (add its root to `e-project-local-allowed-roots')"
                 (abbreviate-file-name layer-directory)))
       (t
        (dolist (registration
                 (e-project-local--load-layer-file layer-directory))
          (let ((id (e-project-local-layer-registration-id registration)))
            (if (memq id seen)
                (message "e-project-local: ignoring duplicate project layer id %s from %s"
                         id (abbreviate-file-name layer-directory))
              (push id seen)
              (push (e-project-local--instantiate-layer-registration
                     registration layer-directory)
                    layers)))))))
    (nreverse layers)))

(defun e-project-local--aggregate-capabilities (layers legacy-capabilities)
  "Return aggregate project-local capabilities from LAYERS and LEGACY-CAPABILITIES."
  (append
   (list (e-project-local--guidance-capability))
   (apply #'append (mapcar #'e-layer-capabilities layers))
   legacy-capabilities))

(defun e-project-local--aggregate-shells (layers)
  "Return aggregate project-local shell manifests from LAYERS."
  (let (shells seen)
    (dolist (layer layers)
      (dolist (shell (e-layer-shells layer))
        (let ((id (e-shell-id shell)))
          (if (memq id seen)
              (message "e-project-local: ignoring duplicate project shell id %s from layer %s"
                       id (e-layer-id layer))
            (push id seen)
            (push shell shells)))))
    (nreverse shells)))

;;;; Layer factory

(defun e-project-local--guidance-capability ()
  "Return a small capability advertising that project-local capabilities exist."
  (e-capability-create
   :id 'project-local
   :name "Project Local"
   :instruction-priority 215
   :instructions e-project-local-instructions))

(defun e-project-local-capabilities (&optional directory)
  "Return capabilities discovered for DIRECTORY or `default-directory'."
  (e-project-local--discover-capabilities
   (e-skills-normalize-directory directory)))

(defun e-project-local-layer-create (&optional directory)
  "Create the project-local aggregate layer rooted at DIRECTORY.

Discovers `.e/layers/' and compatibility `.e/capabilities/' from the project
root, loads allowlisted project loaders, and bundles the resulting
capabilities and shells into a single `project-local' layer.  Returns a layer
with only the guidance capability when no project extensions are discovered."
  (let* ((root (e-skills-normalize-directory directory))
         (layers (e-project-local--discover-layers root))
         (capabilities (e-project-local--discover-capabilities root)))
    (e-layer-create
     :id 'project-local
     :name "Project Local"
     :capabilities (e-project-local--aggregate-capabilities
                    layers capabilities)
     :shells (e-project-local--aggregate-shells layers))))

(provide 'e-project-local)

;;; e-project-local.el ends here
