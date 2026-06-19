;;; e-base.el --- OS base layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; OS base layer for workspace file and shell tools.

;;; Code:

(require 'e-file-capabilities)
(require 'e-layers)

(defconst e-base-instructions
  "Use OS base file and shell tools for workspace files and shell commands.

Never run shell commands that search or traverse outside the current project. \
This includes `find /`, `find ~`, `find $HOME`, `grep -r ~`, `ls -R /`, and any \
recursive walk rooted at `/`, `~`, the home directory, or another broad ancestor. \
They are slow, flood output, and almost never answer the question. Always scope a \
search to the working directory or a known subdirectory, e.g. `find . -name ...` \
or `grep -rn PATTERN lisp/`. When you do not know where something lives, search \
the project root (`.`), not the filesystem or home directory."
  "Default instructions contributed by the OS base guidance capability.")

(defun e-base-layer-create (&optional directory)
  "Create the OS base layer rooted at DIRECTORY or `default-directory'."
  (let ((root (file-name-as-directory
               (expand-file-name (or directory default-directory)))))
    (e-layer-create
     :id 'os-base
     :name "OS Base"
     :capabilities (list (e-base-guidance-capability-create
                          e-base-instructions
                          :instruction-priority 230)
                         (e-file-handling-capability-create root)
                         (e-shell-process-capability-create root)))))

(provide 'e-base)

;;; e-base.el ends here
