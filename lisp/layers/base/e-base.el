;;; e-base.el --- OS base layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; OS base layer for workspace file and shell tools.

;;; Code:

(require 'e-chat-output-mode)
(require 'e-file-capabilities)
(require 'e-layers)
(require 'e-output-style)

(defconst e-base-instructions
  "Use OS base file and shell tools for workspace files and shell commands.

Never run a recursive search or traversal whose effective root is `/`, `~`, \
`$HOME`, the home directory, or another broad ancestor. What matters is where \
the walk actually reaches, not the literal argument. `find /`, `find ~`, \
`find $HOME`, `grep -r ~`, and `ls -R /` are banned -- and so is a bare `find .` \
or `grep -rn PATTERN .` when the working directory itself is the home directory \
or another huge tree, because `.` then expands to exactly that broad walk. \
They are slow, flood output, and almost never answer the question. Before any \
recursive search, consider the working directory: a bare `.` is safe only inside \
a bounded project directory. When the cwd is large or you do not know where \
something lives, scope to a specific known subtree, e.g. `grep -rn PATTERN lisp/` \
or `find ~/.config/doom -name ...`, rather than `.`, `~`, or the filesystem root."
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
                         (e-shell-process-capability-create root)
                         (e-output-style-capability-create root)
                         (e-chat-output-mode-capability-create root)))))

(provide 'e-base)

;;; e-base.el ends here
