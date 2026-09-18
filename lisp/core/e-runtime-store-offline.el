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
(defconst e-runtime-store-offline-startup-diagnostic-byte-limit 4096
  "Maximum UTF-8 bytes retained from one offline worker failure.")

(defun e-runtime-store-offline--utf8-prefix (string limit)
  "Return the longest prefix of STRING occupying at most LIMIT UTF-8 bytes."
  (if (<= (string-bytes string) limit)
      string
    (let ((low 0)
          (high (length string)))
      (while (< low high)
        (let ((mid (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring string 0 mid)) limit)
              (setq low mid)
            (setq high (1- mid)))))
      (substring string 0 low))))

(defun e-runtime-store-offline--worker-file ()
  "Return the installed offline worker path."
  (let ((worker (locate-library "e-runtime-store-offline-worker")))
    ;; `locate-library' commonly returns bytecode, but this operator process
    ;; deliberately passes an explicit file path.  Prefer its source sibling
    ;; so `load-prefer-newer' remains effective after an in-place upgrade and
    ;; a stale ignored .elc cannot run a different migration implementation.
    (when (and worker (string-suffix-p ".elc" worker))
      (let ((source (substring worker 0 -1)))
        (when (file-readable-p source)
          (setq worker source))))
    (or worker
        (signal 'e-runtime-store-offline-error
                (list "Offline runtime-store worker is missing")))))

(defun e-runtime-store-offline--call (operation database argument)
  "Run offline OPERATION for DATABASE with ARGUMENT in batch Emacs."
  (let* ((worker (e-runtime-store-offline--worker-file))
         (output (make-temp-file "e-runtime-store-offline-result-"))
         (stderr (make-temp-file "e-runtime-store-offline-stderr-"))
         (program (expand-file-name invocation-name invocation-directory))
         result
         (core-directory
          (file-name-directory
           (file-truename (expand-file-name worker)))))
    (unwind-protect
        (let ((exit
               (process-file
                program nil (list nil stderr) nil "--batch" "-Q"
                "-L" (file-name-directory worker)
                "-L" core-directory
                "--eval" "(setq load-prefer-newer t)"
                "--eval"
                (format
                 "(condition-case err (progn (load %S) (unless (fboundp 'e-runtime-store-offline-worker-main) (error \"Offline runtime-store worker main is unavailable\"))) (error (princ (format \"Runtime-store offline worker startup failed: %%s\\n\" (error-message-string err)) #'external-debugging-output) (kill-emacs 1)))"
                 worker)
                "--funcall" "e-runtime-store-offline-worker-main"
                operation (expand-file-name database) output
                (expand-file-name argument))))
          (unless (zerop exit)
            (let ((diagnostic
                   (with-temp-buffer
                     (let ((size (file-attribute-size
                                  (file-attributes stderr))))
                       (insert-file-contents-literally
                        stderr nil 0
                        (min size e-runtime-store-offline-startup-diagnostic-byte-limit)))
                     (e-runtime-store-offline--utf8-prefix
                      (buffer-string)
                      e-runtime-store-offline-startup-diagnostic-byte-limit))))
              (signal 'e-runtime-store-offline-error
                      (list (format "Offline worker exited with status %s%s"
                                    exit
                                    (if (string-empty-p diagnostic)
                                        ""
                                      (format ": %s" (string-trim diagnostic))))
                            :exit-status exit
                            :stderr diagnostic
                            :stderr-truncated
                            (> (file-attribute-size (file-attributes stderr))
                               e-runtime-store-offline-startup-diagnostic-byte-limit)))))
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
