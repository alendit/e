;;; e-test-environment-support.el --- Required test environment helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared assertions for repository tests whose contracts depend on an
;; executable, a test-only package, or a genuinely fresh Emacs process.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defun e-test-require-executable (program)
  "Return PROGRAM's path or fail actionably when it is unavailable."
  (or (executable-find program)
      (ert-fail
       (format "Required test executable `%s' is unavailable on PATH" program))))

(defun e-test-require-feature (feature &optional package)
  "Require FEATURE or fail with its test-only PACKAGE requirement."
  (or (require feature nil t)
      (ert-fail
       (format "Required test package `%s' does not provide `%s'"
               (or package feature) feature))))

(defun e-test-require-capability (available message)
  "Return AVAILABLE or fail with actionable MESSAGE."
  (or available (ert-fail message)))

(defun e-test-run-fresh-owner-isolation
    (label required-features forbidden-features assertions)
  "Check LABEL in a fresh batch Emacs process.
Require REQUIRED-FEATURES, reject FORBIDDEN-FEATURES, and require every form
in ASSERTIONS to return non-nil.  The child inherits only explicit load paths;
its feature state can therefore never depend on aggregate test load order."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (paths (delete-dups
                 (cl-remove-if-not #'file-directory-p
                                   (copy-sequence load-path))))
         (form
          `(condition-case err
               (progn
                 (dolist (feature ',required-features)
                   (require feature))
                 (dolist (feature ',forbidden-features)
                   (when (featurep feature)
                     (error "Forbidden facade feature loaded: %S" feature)))
                 (dolist (assertion ',assertions)
                   (unless (eval assertion t)
                     (error "Owner-isolation assertion failed: %S" assertion)))
                 (kill-emacs 0))
             (error
              (princ (format "%S\n" err) external-debugging-output)
              (kill-emacs 1))))
         (args
          (append '("-Q" "--batch")
                  (cl-mapcan (lambda (path) (list "-L" path)) paths)
                  (list "--eval" (prin1-to-string form))))
         (output (generate-new-buffer " *e owner isolation*"))
         status)
    (unwind-protect
        (progn
          (setq status (apply #'call-process emacs nil output nil args))
          (unless (eq status 0)
            (ert-fail
             (format "%s failed in fresh `%s -Q --batch' (exit %S):\n%s"
                     label emacs status
                     (with-current-buffer output (buffer-string))))))
      (kill-buffer output))))

(provide 'e-test-environment-support)

;;; e-test-environment-support.el ends here
