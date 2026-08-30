;;; migrate-session-checkpoints.el --- Offline session checkpoint migration -*- lexical-binding: t; -*-

;; Usage:
;;   emacs -Q --batch -L lisp/core -l tools/migrate-session-checkpoints.el -- DIRECTORY [SESSION-ID...]

(setq load-prefer-newer t)
(require 'e-session)

(when (equal (car command-line-args-left) "--")
  (pop command-line-args-left))

(let* ((directory-argument (pop command-line-args-left))
       (directory
        (and directory-argument
             (file-name-as-directory (expand-file-name directory-argument)))))
  (unless directory
    (error "Usage: ... -- DIRECTORY [SESSION-ID...]"))
  (let* ((store (e-session-persistent-index-store-create directory))
         (session-ids
          (or command-line-args-left
              (e-session-storage-session-ids store))))
    (let ((migrated 0)
          (skipped 0)
          failures)
      (dolist (session-id session-ids)
        (if (e-session-storage-resume-checkpoint-present-p store session-id)
            (cl-incf skipped)
          (let ((original-entry (e-session-index-entry store session-id)))
            (message "Migrating session checkpoint %s" session-id)
            (condition-case err
                (progn
                  (e-session-migrate-session-checkpoint store session-id)
                  ;; Keep migration bounded across a large session store.  The
                  ;; detached catalog projection is sufficient after the
                  ;; checkpoint and does not retain the full replayed journal.
                  (e-session-unload-session store session-id original-entry)
                  (cl-incf migrated))
              (e-session-missing
               (e-session-unload-session store session-id original-entry)
               (push (cons session-id err) failures)
               )))))
      (e-session-refresh-index store)
      (message "Migrated %d, skipped %d, and failed %d session checkpoint(s) in %s"
               migrated skipped (length failures) directory)
      (when failures
        (error "Checkpoint migration failed: %S" (nreverse failures))))))

;;; migrate-session-checkpoints.el ends here
