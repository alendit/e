;;; e-e2e-bootstrap.el --- Shared E2E process bootstrap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Prepare private E2E Emacs processes in one of two modes.  The default
;; `isolated' mode loads e from checkout source with Eldev dependencies.  The
;; opt-in `current' mode first verifies that normal user startup already loaded
;; e from this checkout, then adds only test paths and dependencies.

;;; Code:

(require 'subr-x)

(defconst e-e2e-project-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the e checkout containing this bootstrap file.")

(defconst e-e2e-emacs-config-mode
  (let ((mode (or (getenv "E_E2E_EMACS_CONFIG") "isolated")))
    (unless (member mode '("isolated" "current"))
      (error "Unknown E_E2E_EMACS_CONFIG mode: %s" mode))
    mode)
  "Configuration startup policy for the current E2E process.")

(defconst e-e2e-current-config-user-init-file user-init-file
  "User init file observed before E2E test paths are added.")

(defconst e-e2e-current-config-user-emacs-directory user-emacs-directory
  "User Emacs directory observed before E2E test paths are added.")

(defconst e-e2e-current-config-loaded-e-p (featurep 'e)
  "Whether normal user startup loaded e before this bootstrap ran.")

(defun e-e2e-current-config-p ()
  "Return non-nil when this process tests the current Emacs configuration."
  (equal e-e2e-emacs-config-mode "current"))

(defun e-e2e--command-registration-state (command)
  "Return bounded startup registration state for COMMAND."
  (let* ((definition (and (fboundp command) (symbol-function command)))
         (autoloadp (autoloadp definition)))
    (list :command command
          :commandp (commandp command)
          :autoloadp autoloadp
          :autoload-file (and autoloadp (nth 1 definition))
          :function-file (symbol-file command 'defun))))

(defconst e-e2e-current-config-package-command-state
  (when (e-e2e-current-config-p)
    (list
     :board-activity
     (e-e2e--command-registration-state 'e-board-activity-list-buffer)
     :retired-subagent-list
     (e-e2e--command-registration-state
      (intern (concat "e-" "subagents-list-buffer")))
     :retired-board-runs-list
     (e-e2e--command-registration-state
      (intern (concat "e-board-" "runs-list-buffer")))))
  "Package command state captured before E2E checkout paths are added.")

(defun e-e2e--add-project-load-paths ()
  "Add checkout runtime and test dependency directories to `load-path'."
  (add-to-list 'load-path e-e2e-project-root)
  (let ((default-directory (expand-file-name "lisp" e-e2e-project-root)))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path))
  (let ((egui (expand-file-name "emacs-egui/lisp" e-e2e-project-root)))
    (when (file-directory-p egui)
      (add-to-list 'load-path egui))))

(unless (e-e2e-current-config-p)
  (when-let* ((directory (getenv "E_GRAPHICAL_E2E_EMACS_DIR")))
    (setq user-emacs-directory (file-name-as-directory directory)))
  (load (expand-file-name
         "graphical/e-graphical-source-bootstrap.el"
         (file-name-directory (or load-file-name buffer-file-name)))
        nil nil t))

;; Eldev owns the graphical test-only evil and persp-mode dependencies.  Add
;; them after current user startup so they cannot influence whether or how the
;; configuration loads e.
(require 'package)
(let ((package-user-dir
       (expand-file-name
        (format ".eldev/%s.%s/packages" emacs-major-version emacs-minor-version)
        e-e2e-project-root)))
  (package-initialize))

(when (e-e2e-current-config-p)
  (unless e-e2e-current-config-loaded-e-p
    (error "Current Emacs configuration did not load e"))
  (unless (and (fboundp 'e-source-directory)
               (file-equal-p (e-source-directory) e-e2e-project-root))
    (error "Current Emacs configuration loaded e from %S, expected %s"
           (and (fboundp 'e-source-directory) (e-source-directory))
           e-e2e-project-root)))

(e-e2e--add-project-load-paths)

(unless (e-e2e-current-config-p)
  (require 'e))

(provide 'e-e2e-bootstrap)

;;; e-e2e-bootstrap.el ends here
