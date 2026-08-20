;;; e-dev.el --- Interactive development helpers for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Development-only helpers for interactive package work inside Emacs.

;;; Code:

(require 'e)
(require 'seq)

(defgroup e-dev nil
  "Interactive development helpers for e."
  :group 'e
  :prefix "e-dev-")

(defconst e-dev--directory
  (file-name-directory
   (file-truename (or load-file-name buffer-file-name default-directory)))
  "Directory containing this development helper file.")

(defcustom e-dev-source-directory
  (expand-file-name "../.." e-dev--directory)
  "Root directory of the local e checkout used for live reloading."
  :type 'directory
  :group 'e-dev)

(defconst e-dev--reevaluated-defaults
  '(e-default-layer-specs
    e-debug-display-strategy
    e-default-chat-layer-ids
    e-output-style-registry)
  "Uncustomized options whose changed defaults should apply after reload.")

(defconst e-dev--reloadable-source-directories
  '("lisp/layers" "lisp/defaults" "lisp/shells" "lisp/dev")
  "Source directories whose loaded modules support in-process reload.
Core runtime, session, harness, Work, and provider adapter changes require an
Emacs restart.  Reloading only these extension and presentation seams avoids
mixing incompatible record layouts and dependency generations.")

(defvar e-dev--reload-required-entries nil
  "Pending explicit reload requests for the running Emacs.")

(defconst e-dev--bytecode-scan-directories
  '("lisp" "test" "e2e")
  "Checkout-local directories scanned for stale e bytecode.")

(defun e-dev--bytecode-source-file (bytecode-file)
  "Return the source file corresponding to BYTECODE-FILE."
  (concat (file-name-sans-extension bytecode-file) ".el"))

(defun e-dev--stale-bytecode-file-p (bytecode-file)
  "Return non-nil when BYTECODE-FILE is orphaned or older than its source."
  (let ((source (e-dev--bytecode-source-file bytecode-file)))
    (or (not (file-exists-p source))
        (file-newer-than-file-p source bytecode-file))))

(defun e-dev--bytecode-candidates (root)
  "Return checkout-local bytecode candidates under ROOT."
  (let ((candidates nil)
        (entrypoint (expand-file-name "e.elc" root)))
    (when (file-exists-p entrypoint)
      (push entrypoint candidates))
    (dolist (directory e-dev--bytecode-scan-directories)
      (let ((path (expand-file-name directory root)))
        (when (file-directory-p path)
          (setq candidates
                (append (directory-files-recursively path "\\.elc\\'")
                        candidates)))))
    (sort candidates #'string<)))

;;;###autoload
(defun e-dev-stale-bytecode-files (&optional directory)
  "Return stale byte-compiled e files under DIRECTORY.
DIRECTORY defaults to `e-dev-source-directory'."
  (let ((root (file-name-as-directory
               (expand-file-name (or directory e-dev-source-directory)))))
    (seq-filter #'e-dev--stale-bytecode-file-p
                (e-dev--bytecode-candidates root))))

;;;###autoload
(defun e-dev-clean-stale-bytecode (&optional directory)
  "Delete stale byte-compiled e files under DIRECTORY.
DIRECTORY defaults to `e-dev-source-directory'.  `.elc' files whose source
is missing or newer are removed."
  (interactive)
  (let ((files (e-dev-stale-bytecode-files directory)))
    (dolist (file files)
      (delete-file file))
    (when (called-interactively-p 'interactive)
      (message "Deleted %d stale e bytecode file%s"
               (length files)
               (if (= (length files) 1) "" "s")))
    files))

(defun e-dev--reevaluate-uncustomized-defaults ()
  "Reapply changed defcustom defaults unless the user customized them."
  (dolist (symbol e-dev--reevaluated-defaults)
    (when (and (boundp symbol)
               (not (get symbol 'customized-value)))
      (custom-reevaluate-setting symbol))))

(defun e-dev--source-feature (file)
  "Return the conventional feature symbol provided by FILE."
  (intern (file-name-base file)))

(defun e-dev--reloadable-source-files (root)
  "Return loaded reloadable source files below ROOT in deterministic order."
  (let (files)
    (dolist (directory e-dev--reloadable-source-directories)
      (let ((absolute (expand-file-name directory root)))
        (when (file-directory-p absolute)
          (dolist (file (directory-files-recursively absolute "\\.el\\'"))
            (when (featurep (e-dev--source-feature file))
              (push file files))))))
    (let* ((dev-file (expand-file-name "lisp/dev/e-dev.el" root))
           (ordered (sort (delete dev-file files) #'string<)))
      (if (featurep 'e-dev)
          (append ordered (list dev-file))
        ordered))))

(defun e-dev--clear-reloadable-required-entries ()
  "Clear pending entries satisfied by a supported in-process reload."
  (setq e-dev--reload-required-entries
        (seq-remove
         (lambda (entry)
           (memq (plist-get entry :scope) '(reloadable live)))
         e-dev--reload-required-entries)))

(defun e-dev--normalize-file-list (files)
  "Return FILES as a list of strings."
  (cond
   ((null files) nil)
   ((stringp files) (list files))
   ((vectorp files) (e-dev--normalize-file-list (append files nil)))
   ((listp files)
    (delq nil
          (mapcar (lambda (file)
                    (cond
                     ((stringp file) file)
                     ((symbolp file) (symbol-name file))
                     (t nil)))
                  files)))
   (t nil)))

(defun e-dev--reload-required-entry (reason files scope)
  "Create a pending reload entry from REASON FILES and SCOPE."
  (list :id (format "reload-required-%d" (round (* (float-time) 1000)))
        :reason (or reason "e source changed")
        :files (e-dev--normalize-file-list files)
        :scope (or scope 'restart)
        :created-at (float-time)))

;;;###autoload
(defun e-dev-mark-reload-required (&optional reason files scope)
  "Record that the running Emacs needs reload or restart when idle.
SCOPE is `reloadable' for supported extension seams and `restart' for core,
record-shape, session, harness, Work, or provider changes.  The historical
`full' scope is treated as restart-required.  This notification path does not
load source, compile files, run startup hooks, or interrupt active work."
  (interactive
   (list (read-string "Reload reason: " nil nil "e source changed")
         nil
         'restart))
  (let ((entry (e-dev--reload-required-entry reason files scope)))
    (push entry e-dev--reload-required-entries)
    (message (if (memq (plist-get entry :scope) '(full restart))
                 "e restart required: %s"
               "e reload required: %s; run M-x e-dev-reload when idle")
             (plist-get entry :reason))
    (e-dev-reload-required-status)))

;;;###autoload
(defun e-dev-reload-required-status ()
  "Return pending explicit reload status for the running Emacs."
  (interactive)
  (let ((status (list :required (not (null e-dev--reload-required-entries))
                      :count (length e-dev--reload-required-entries)
                      :entries (nreverse
                                (copy-sequence
                                 e-dev--reload-required-entries)))))
    (when (called-interactively-p 'interactive)
      (if (plist-get status :required)
          (let ((restart-count
                 (seq-count
                  (lambda (entry)
                    (memq (plist-get entry :scope) '(full restart)))
                  (plist-get status :entries))))
            (message "e change pending (%d total%s)"
                     (plist-get status :count)
                     (if (> restart-count 0)
                         (format ", %d require%s restart"
                                 restart-count
                                 (if (= restart-count 1) "s" ""))
                       "")))
        (message "No e reload is pending")))
    status))

(defun e-dev-clear-reload-required ()
  "Clear pending explicit reload requests."
  (interactive)
  (setq e-dev--reload-required-entries nil)
  (when (called-interactively-p 'interactive)
    (message "Cleared pending e reload requests"))
  (e-dev-reload-required-status))

;;;###autoload
(defun e-dev-reload (&optional directory)
  "Reload supported e extension seams from DIRECTORY.
Only already-loaded layer, default, shell, and developer modules are reloaded.
Core runtime, session, harness, Work, and provider adapter changes require an
Emacs restart; pending `restart' or historical `full' entries remain visible
after this command.  DIRECTORY defaults to `e-dev-source-directory'."
  (interactive)
  (let* ((root (file-name-as-directory
                (expand-file-name (or directory e-dev-source-directory))))
         (files (e-dev--reloadable-source-files root)))
    (e-dev-clean-stale-bytecode root)
    (let ((load-prefer-newer t))
      (dolist (file files)
        (load file nil 'nomessage))
      (e-dev--reevaluate-uncustomized-defaults)
      (when (fboundp 'e-project-local-reset-loaded-files)
        ;; Project-local factory files are loaded once per session and skipped
        ;; on later opens; forget them so this reload picks up edits on next open.
        (e-project-local-reset-loaded-files))
      (when (fboundp 'e-startup-run)
        (e-startup-run)))
    (e-dev--clear-reloadable-required-entries)
    (let ((restart-count
           (seq-count
            (lambda (entry)
              (memq (plist-get entry :scope) '(full restart)))
            e-dev--reload-required-entries)))
      (message "Reloaded %d e extension module%s from %s%s"
               (length files) (if (= (length files) 1) "" "s")
               (abbreviate-file-name root)
               (if (> restart-count 0)
                   (format "; %d change%s still require%s restart"
                           restart-count
                           (if (= restart-count 1) "" "s")
                           (if (= restart-count 1) "s" ""))
                 "")))
    root))

(provide 'e-dev)

;;; e-dev.el ends here
