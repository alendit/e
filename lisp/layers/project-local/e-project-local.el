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
(require 'e-startup)
(require 'e-skills)
(require 'e-shells)
(require 'subr-x)

(declare-function projectile-project-root "ext:projectile")

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

(defcustom e-project-local-projectile-prime-on-project-entry t
  "Whether Projectile project entry should prime project-local extensions.

When non-nil and Projectile is loaded, `e' installs hooks that eagerly discover
allowlisted project-local `.e/layers/' and `.e/capabilities/' packages for the
current Projectile project.  This loads trusted presentation shell files early,
so commands they define are available before an `e' harness is created.  Harness
activation still owns model-facing tool/resource/shell registration."
  :type 'boolean
  :group 'e-project-local)

(defcustom e-project-local-auto-byte-recompile nil
  "Whether project entry should byte-recompile stale project-local Elisp.

When non-nil, `e-project-local-prime-project' byte-compiles allowlisted
project-local `.e/' extension files whose `.elc' files are missing or older
than their `.el' sources before loading the project-local layer.  This is off
by default because byte compilation writes `.elc' files into the project."
  :type 'boolean
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

(defvar e-project-local--extensionless-load-roots nil
  "Trusted project-local roots whose explicit `.el' loads should be extensionless.")

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

;;;; Eager project priming

(defun e-project-local--project-has-extensions-p (directory)
  "Return non-nil when DIRECTORY contains project-local extension roots."
  (let ((root (e-skills-normalize-directory directory)))
    (or (file-directory-p (expand-file-name e-project-local-layers-subdir root))
        (file-directory-p (expand-file-name e-project-local-capabilities-subdir
                                            root)))))

(defun e-project-local-prime-project (&optional directory)
  "Eagerly load allowlisted project-local extensions for DIRECTORY.

This is a presentation convenience for project entry hooks: it runs normal
project-local discovery early so trusted layer shell files can define their
interactive commands before an `e' harness is created.  It returns the aggregate
`project-local' layer when discovery is attempted, or nil when DIRECTORY has no
project-local extension roots or is not allowlisted.

The normal trust gate still applies: repository elisp is only loaded when
DIRECTORY is at or below `e-project-local-allowed-roots'.  Harness activation
continues to own model-facing tool, resource, and scoped shell registration."
  (let ((root (e-skills-normalize-directory (or directory default-directory))))
    (when (and (e-project-local--project-has-extensions-p root)
               (e-project-local--root-allowed-p root))
      (let ((default-directory root))
        (when e-project-local-auto-byte-recompile
          (e-project-local-byte-compile-project root))
        (e-project-local-layer-create root)))))

(defun e-project-local--projectile-project-root ()
  "Return the current Projectile project root, or nil."
  (when (fboundp 'projectile-project-root)
    (when-let ((root (ignore-errors (projectile-project-root))))
      (e-skills-normalize-directory root))))

(defun e-project-local-prime-projectile-project ()
  "Prime project-local extensions for the current Projectile project.

Intended for `projectile-after-switch-project-hook',
`projectile-find-file-hook', and `projectile-find-dir-hook'."
  (when e-project-local-projectile-prime-on-project-entry
    (when-let ((root (e-project-local--projectile-project-root)))
      (e-project-local-prime-project root))))

(defun e-project-local-projectile-hooks-install ()
  "Install Projectile hooks that prime project-local extensions on entry."
  (add-hook 'projectile-after-switch-project-hook
            #'e-project-local-prime-projectile-project)
  (add-hook 'projectile-find-file-hook
            #'e-project-local-prime-projectile-project)
  (add-hook 'projectile-find-dir-hook
            #'e-project-local-prime-projectile-project))

(defun e-project-local-projectile-hooks-uninstall ()
  "Remove Projectile hooks installed by `e-project-local'."
  (remove-hook 'projectile-after-switch-project-hook
               #'e-project-local-prime-projectile-project)
  (remove-hook 'projectile-find-file-hook
               #'e-project-local-prime-projectile-project)
  (remove-hook 'projectile-find-dir-hook
               #'e-project-local-prime-projectile-project))

(defun e-project-local-startup ()
  "Install optional integrations for project-local extension discovery."
  (with-eval-after-load 'projectile
    (e-project-local-projectile-hooks-install)))

(add-hook 'e-startup-layer-hook #'e-project-local-startup)

;;;; Loading

(defun e-project-local--under-extensionless-load-root-p (file)
  "Return non-nil when FILE is below a trusted extensionless-load root."
  (let* ((expanded (expand-file-name file))
         (target (concat (file-truename (file-name-directory expanded))
                         (file-name-nondirectory expanded))))
    (cl-some
     (lambda (root)
       (string-prefix-p
        (file-truename (e-skills-normalize-directory root))
        target))
     e-project-local--extensionless-load-roots)))

(defun e-project-local--extensionless-load-advice (original file &rest args)
  "Call ORIGINAL `load', stripping `.el' from trusted project-local FILE loads."
  (let ((load-file
         (if (and (stringp file)
                  (string-suffix-p ".el" file)
                  (e-project-local--under-extensionless-load-root-p file))
             (let ((elc-file (byte-compile-dest-file file)))
               (if (and (not (file-exists-p file))
                        (file-exists-p elc-file))
                   elc-file
                 (file-name-sans-extension file)))
           file)))
    (let ((load-prefer-newer t))
      (apply original load-file args))))

(defmacro e-project-local--with-extensionless-loads (roots &rest body)
  "Run BODY with explicit `.el' loads under ROOTS converted to base names."
  (declare (indent 1))
  `(let* ((e-project-local--extensionless-load-roots
           (append ,roots e-project-local--extensionless-load-roots))
          (install-advice
           (not (advice-member-p #'e-project-local--extensionless-load-advice
                                 'load))))
     (when install-advice
       (advice-add 'load :around #'e-project-local--extensionless-load-advice))
     (unwind-protect
         (progn ,@body)
       (when install-advice
         (advice-remove 'load
                        #'e-project-local--extensionless-load-advice)))))

(defun e-project-local--load-project-file (file)
  "Load project-local Elisp FILE while letting Emacs prefer `.elc' when fresh."
  (let ((load-prefer-newer t))
    (load (file-name-sans-extension file) nil 'nomessage)))

(defun e-project-local--load-capability-file (capability-directory)
  "Load `capability.el' in CAPABILITY-DIRECTORY and return its registrations.
Returns a list of `e-project-local-registration' values in registration order.
Returns nil when no `capability.el' exists."
  (let ((file (expand-file-name "capability.el" capability-directory)))
    (when (e-skills-readable-file-p file)
      (let ((e-project-local--loading file)
            (e-project-local--registrations nil))
        (e-project-local--with-extensionless-loads (list capability-directory)
          (e-project-local--load-project-file file))
        (nreverse e-project-local--registrations)))))

(defun e-project-local--load-layer-file (layer-directory)
  "Load `layer.el' in LAYER-DIRECTORY and return its registrations.
Returns a list of `e-project-local-layer-registration' values in registration
order.  Returns nil when no `layer.el' exists."
  (let ((file (expand-file-name "layer.el" layer-directory)))
    (when (e-skills-readable-file-p file)
      (let ((e-project-local--layer-loading file)
            (e-project-local--layer-registrations nil))
        (e-project-local--with-extensionless-loads (list layer-directory)
          (e-project-local--load-project-file file))
        (nreverse e-project-local--layer-registrations)))))

(defun e-project-local--trusted-extension-directories (directory)
  "Return allowlisted project-local extension directories for DIRECTORY."
  (cl-remove-if-not
   #'e-project-local--root-allowed-p
   (append (e-project-local--layer-directories directory)
           (e-project-local--capability-directories directory))))

(defun e-project-local--elisp-files (directory)
  "Return project-local Elisp files under allowlisted extensions for DIRECTORY."
  (let (files)
    (dolist (extension-directory
             (e-project-local--trusted-extension-directories directory))
      (dolist (file (directory-files-recursively extension-directory "\\.el\\'"))
        (push file files)))
    (sort (delete-dups files) #'string<)))

(defun e-project-local--byte-compile-needed-p (file)
  "Return non-nil when FILE's `.elc' is missing or older than FILE."
  (let ((elc-file (byte-compile-dest-file file)))
    (or (not (file-exists-p elc-file))
        (file-newer-than-file-p file elc-file))))

;;;###autoload
(defun e-project-local-byte-compile-project (&optional directory force)
  "Byte-compile allowlisted project-local Elisp under DIRECTORY.

Only files under trusted `.e/layers/' and `.e/capabilities/' extension roots
are compiled.  By default, compile files whose `.elc' outputs are missing or
older than the source.  With interactive prefix argument FORCE, compile every
project-local `.el' file."
  (interactive (list default-directory current-prefix-arg))
  (let ((compiled-files nil))
    (dolist (file (e-project-local--elisp-files
                   (e-skills-normalize-directory directory)))
      (when (or force (e-project-local--byte-compile-needed-p file))
        (pcase (byte-compile-file file)
          ('no-byte-compile nil)
          ('nil (error "Failed to byte-compile `%s'" file))
          (_ (push (byte-compile-dest-file file) compiled-files)))))
    (setq compiled-files (nreverse compiled-files))
    (when (called-interactively-p 'interactive)
      (message "Byte-compiled %d project-local file%s"
               (length compiled-files)
               (if (= (length compiled-files) 1) "" "s")))
    compiled-files))

(defun e-project-local--instantiate-registration (registration directory)
  "Return the capability built by REGISTRATION for DIRECTORY.
Signals when the factory does not return an `e-capability' with matching id."
  (let ((capability
         (e-project-local--with-extensionless-loads (list directory)
           (funcall (e-project-local-registration-factory registration)
                    directory))))
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
  (let ((layer
         (e-project-local--with-extensionless-loads (list directory)
           (funcall (e-project-local-layer-registration-factory
                     registration)
                    directory))))
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

(defun e-project-local--layer-skill-specs (layer-id directory)
  "Return layer-scoped skill specs for LAYER-ID under DIRECTORY."
  (mapcar
   (lambda (skill)
     (e-skill-spec-create
      :name (e-skill-spec-name skill)
      :description (e-skill-spec-description skill)
      :path (format "layers/%s/%s" layer-id (e-skill-spec-path skill))
      :reader (lambda (_skill range)
                (funcall (e-skill-spec-reader skill) skill range))
      :metadata (append (e-skill-spec-metadata skill)
                        (list :layer-id layer-id))))
   (e-skills-specs-from-directory
    "project"
    (expand-file-name "skills" directory))))

(defun e-project-local--layer-skill-capability (layer-id directory)
  "Return a resource-only capability for LAYER-ID skills under DIRECTORY."
  (let ((skills (e-project-local--layer-skill-specs layer-id directory)))
    (when skills
      (e-capability-with-skills-create
       :id 'project-local
       :name "Project Local Layer Skills"
       :skills skills))))

(defun e-project-local--with-layer-skills (layer directory)
  "Return LAYER extended with layer-scoped skill resources from DIRECTORY."
  (if-let ((capability (e-project-local--layer-skill-capability
                        (e-layer-id layer)
                        directory)))
      (progn
        (setf (e-layer-capabilities layer)
              (append (e-layer-capabilities layer) (list capability)))
        layer)
    layer))

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
              (push (e-project-local--with-layer-skills
                     (e-project-local--instantiate-layer-registration
                      registration layer-directory)
                     layer-directory)
                    layers)))))))
    (nreverse layers)))

(defun e-project-local--aggregate-requires (layers)
  "Return de-duplicated layer ids required by discovered project LAYERS.
A project layer declares dependencies via its `requires' slot; the aggregate
`project-local' layer carries their union so the harness activates each
required built-in layer (loading its feature) before project capabilities and
shells are used."
  (let (requires seen)
    (dolist (layer layers)
      (dolist (id (e-layer-requires layer))
        (unless (memq id seen)
          (push id seen)
          (push id requires))))
    (nreverse requires)))

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

(defun e-project-local--discovered-capabilities (directory)
  "Return project-local capabilities discovered for DIRECTORY without guidance."
  (let* ((root (e-skills-normalize-directory directory))
         (layers (e-project-local--discover-layers root))
         (capabilities (e-project-local--discover-capabilities root)))
    (append
     (apply #'append (mapcar #'e-layer-capabilities layers))
     capabilities)))

(defun e-project-local--context-directory (fallback context)
  "Return session project root from CONTEXT, falling back to FALLBACK."
  (let* ((harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (turn-id (plist-get context :turn-id)))
    (or (and (fboundp 'e-harness-project-root)
             (ignore-errors
               (e-harness-project-root harness session-id turn-id)))
        fallback
        default-directory)))

(defun e-project-local--context-capabilities (fallback context)
  "Return project-local capabilities for CONTEXT or FALLBACK."
  (e-project-local--discovered-capabilities
   (e-project-local--context-directory fallback context)))

(defun e-project-local--register-context-tools
    (fallback registry &rest context)
  "Register project-local tools for CONTEXT, using FALLBACK when needed."
  (dolist (capability (e-project-local--context-capabilities fallback context))
    (apply #'e-capabilities-register-tools capability registry context)))

(defun e-project-local--register-context-resources
    (fallback store _capability &rest context)
  "Register project-local resources for CONTEXT, using FALLBACK when needed."
  (dolist (capability (e-project-local--context-capabilities fallback context))
    (apply #'e-capabilities-register-resources capability store context)))

(defun e-project-local--register-context-resource-methods
    (fallback registry &rest context)
  "Register project-local resource methods for CONTEXT, using FALLBACK when needed."
  (dolist (capability (e-project-local--context-capabilities fallback context))
    (apply #'e-capabilities-register-resource-methods
           capability
           registry
           context)))

(defun e-project-local--context-messages (fallback &rest context)
  "Return project-local context messages for CONTEXT, using FALLBACK when needed."
  (plist-get
   (apply #'e-capabilities-context
          (e-project-local--context-capabilities fallback context)
          context)
   :messages))

(defun e-project-local--run-context-hooks
    (fallback point value context)
  "Run project-local hooks for POINT in CONTEXT, using FALLBACK when needed."
  (let ((registry (e-hooks-registry-create)))
    (dolist (capability (e-project-local--context-capabilities fallback context))
      (e-capabilities-register-hooks capability registry))
    (e-hooks-run-reduce registry point value context)))

(defun e-project-local--dynamic-capability (directory)
  "Return session-root-aware project-local capability with DIRECTORY fallback."
  (let ((fallback (and directory (e-skills-normalize-directory directory))))
    (e-capability-create
     :id 'project-local
     :name "Project Local"
     :instruction-priority 215
     :instructions e-project-local-instructions
     :tools
     (list (lambda (registry &rest context)
             (apply #'e-project-local--register-context-tools
                    fallback
                    registry
                    context)))
     :resource-methods
     (list
      (e-capability-resource-method-provider-create
       :handler
       (lambda (registry &rest context)
         (apply #'e-project-local--register-context-resource-methods
                fallback
                registry
                context))))
     :resources
     (list (lambda (store capability &rest context)
             (apply #'e-project-local--register-context-resources
                    fallback
                    store
                    capability
                    context)))
     :context-providers
     (list
      (e-context-provider-create
       :name 'project-local
       :priority 215
       :cache-placement 'stable-context
       :build (lambda (&rest context)
                (apply #'e-project-local--context-messages
                       fallback
                       context))))
     :hooks
     (list
      (e-hook-create
       :id "70-project-local-turn-finished"
       :point :turn-finished
       :description "Run session-root project-local turn-finished hooks."
       :handler (lambda (value context)
                  (e-project-local--run-context-hooks
                   fallback :turn-finished value context)))
      (e-hook-create
       :id "70-project-local-pre-tool-call"
       :point :pre-tool-call
       :description "Run session-root project-local pre-tool-call hooks."
       :handler (lambda (value context)
                  (e-project-local--run-context-hooks
                   fallback :pre-tool-call value context)))
      (e-hook-create
       :id "70-project-local-post-tool-call"
       :point :post-tool-call
       :description "Run session-root project-local post-tool-call hooks."
       :handler (lambda (value context)
                  (e-project-local--run-context-hooks
                   fallback :post-tool-call value context)))))))

;;;; Layer factory

(defun e-project-local-capabilities (&optional directory)
  "Return capabilities discovered for DIRECTORY or `default-directory'."
  (e-project-local--discover-capabilities
   (e-skills-normalize-directory directory)))

(defun e-project-local-layer-create (&optional directory)
  "Create the project-local aggregate layer rooted at DIRECTORY.

Discovers `.e/layers/' from the project root for shell manifests and
dependencies.  Model-facing project-local tools, resources, context providers,
and hooks are resolved dynamically from the active session project root, with
DIRECTORY used only as the fallback root outside a session."
  (let* ((root (e-skills-normalize-directory directory))
         (layers (e-project-local--discover-layers root)))
    (e-layer-create
     :id 'project-local
     :name "Project Local"
     :capabilities (list (e-project-local--dynamic-capability root))
     :shells (e-project-local--aggregate-shells layers)
     :requires (e-project-local--aggregate-requires layers))))

(provide 'e-project-local)

;;; e-project-local.el ends here
