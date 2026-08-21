;;; e-graphical-daemon-bootstrap.el --- Prepare isolated graphical E2E daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; macOS uses a private Emacs daemon so graphical tests can render on a
;; transparent off-screen NS frame without showing or focusing a desktop
;; window.  Delegate configuration provenance and test dependency preparation
;; to the shared E2E bootstrap.

;;; Code:

(load (expand-file-name
       "../e-e2e-bootstrap.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(provide 'e-graphical-daemon-bootstrap)

;;; e-graphical-daemon-bootstrap.el ends here
