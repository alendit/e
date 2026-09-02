;;; e-runtime-store-offline.el --- Operator-facing offline store operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Launches one private batch Emacs for explicit offline operations.  The
;; calling interactive Emacs never opens SQLite and ordinary startup never
;; upgrades an existing schema.

;;; Code:

(require 'e-runtime-store-codec)

(define-error 'e-runtime-store-offline-error "Offline runtime-store operation failed")

(defconst e-runtime-store-offline-cli-error-max-chars 2048)

(defun e-runtime-store-offline--worker-file ()
  "Return the installed offline worker path."
  (or (locate-library "e-runtime-store-offline-worker")
      (signal 'e-runtime-store-offline-error
              (list "Offline runtime-store worker is missing"))))

(defun e-runtime-store-offline--call (operation database argument)
  "Run offline OPERATION for DATABASE with ARGUMENT in batch Emacs."
  (let* ((worker (e-runtime-store-offline--worker-file))
         (output (make-temp-file "e-runtime-store-offline-result-"))
         (stderr (make-temp-file "e-runtime-store-offline-stderr-"))
         (program (expand-file-name invocation-name invocation-directory))
         result)
    (unwind-protect
        (let ((exit
               (process-file
                program nil (list nil stderr) nil "--batch" "-Q"
                "-L" (file-name-directory worker)
                "--eval" "(setq load-prefer-newer t)" "-l" worker
                "--funcall" "e-runtime-store-offline-worker-main"
                operation (expand-file-name database) output
                (expand-file-name argument))))
          (unless (zerop exit)
            (signal 'e-runtime-store-offline-error
                    (list (with-temp-buffer
                            (insert-file-contents stderr)
                            (buffer-string)))))
          (with-temp-buffer
            (insert-file-contents-literally output)
            (setq result
                  (e-runtime-store-codec-decode (buffer-string)))))
      (when (file-exists-p stderr) (delete-file stderr))
      (when (file-exists-p output) (delete-file output)))
    result))

(defun e-runtime-store-offline-upgrade (directory &optional backup-file)
  "Explicitly upgrade the closed SQLite store in DIRECTORY.

Create and verify restrictive BACKUP-FILE before the transactional upgrade.
When omitted, place the backup below DIRECTORY's private `backups' directory."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (database (expand-file-name "store.sqlite3" directory))
         (backup
          (or backup-file
              (expand-file-name
               (format "backups/store-pre-upgrade-%s.sqlite3"
                       (format-time-string "%Y%m%dT%H%M%S"))
               directory))))
    (e-runtime-store-offline--call "upgrade" database backup)))

(defun e-runtime-store-offline--bounded-error-message (error-data)
  "Return a one-line bounded operator message for ERROR-DATA."
  (let ((message
         (replace-regexp-in-string
          "[\n\r\t ]+" " " (error-message-string error-data))))
    (if (> (length message) e-runtime-store-offline-cli-error-max-chars)
        (concat (substring message 0 e-runtime-store-offline-cli-error-max-chars)
                "...")
      message)))

(defun e-runtime-store-offline-cli-main ()
  "Run the environment-configured upgrade with bounded operator errors."
  (condition-case err
      (progn
        (prin1
         (e-runtime-store-offline-upgrade
          (getenv "E_RUNTIME_UPGRADE_DIRECTORY")
          (let ((value (getenv "E_RUNTIME_UPGRADE_BACKUP")))
            (unless (equal value "") value))))
        (terpri))
    (error
     (princ
      (format "e-runtime-upgrade: %s\n"
              (e-runtime-store-offline--bounded-error-message err))
      #'external-debugging-output)
     (kill-emacs 1))))

(provide 'e-runtime-store-offline)

;;; e-runtime-store-offline.el ends here
