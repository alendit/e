;;; e-graphical-daemon-bootstrap.el --- Prepare isolated graphical E2E daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; macOS uses a private Emacs daemon so graphical tests can render on a
;; transparent off-screen NS frame without showing or focusing a desktop
;; window.  Eldev prepares dependencies before this file is loaded; this file
;; gives the daemon the corresponding package and project source paths.

;;; Code:

(when-let ((directory (getenv "E_GRAPHICAL_E2E_EMACS_DIR")))
  (setq user-emacs-directory (file-name-as-directory directory)))
(setq load-prefer-newer t)

(require 'package)

(defconst e-graphical-daemon-project-root
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (directory-file-name
      (file-name-directory (or load-file-name buffer-file-name))))))
  "Root of the e checkout containing this bootstrap file.")

(setq package-user-dir
      (expand-file-name
       (format ".eldev/%s.%s/packages" emacs-major-version emacs-minor-version)
       e-graphical-daemon-project-root))
(package-initialize)

(add-to-list 'load-path e-graphical-daemon-project-root)
(let ((default-directory
       (expand-file-name "lisp" e-graphical-daemon-project-root)))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))
(let ((egui (expand-file-name
             "emacs-egui/lisp" e-graphical-daemon-project-root)))
  (when (file-directory-p egui)
    (add-to-list 'load-path egui)))

(require 'e)

(provide 'e-graphical-daemon-bootstrap)

;;; e-graphical-daemon-bootstrap.el ends here
