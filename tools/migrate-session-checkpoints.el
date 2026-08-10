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
         (sessions-directory (e-session-store-sessions-directory store))
         (session-ids
          (or command-line-args-left
              (mapcar #'file-name-base
                      (directory-files sessions-directory t "\\.jsonl\\'")))))
    (let ((migrated 0)
          (skipped 0)
          failures)
      (dolist (session-id session-ids)
        (if (file-exists-p (e-session--checkpoint-file store session-id))
            (cl-incf skipped)
          (let ((original-entry
                 (e-session--session-index-entry
                  store (e-session--peek-session store session-id))))
            (message "Migrating session checkpoint %s" session-id)
            (condition-case err
                (progn
                  (e-session-migrate-session-checkpoint store session-id)
                  ;; Keep migration bounded across a large session store.  The
                  ;; catalog projection is sufficient after the checkpoint.
                  (let* ((session (e-session-get store session-id))
                         (entry (e-session--session-index-entry store session)))
                    (e-session--clear-entry-index store session-id)
                    (puthash session-id
                             (e-session--index-entry-session store entry)
                             (e-session-store-sessions store)))
                  (cl-incf migrated))
              (e-session-missing
               (push (cons session-id err) failures)
               (puthash session-id
                        (e-session--index-entry-session store original-entry)
                        (e-session-store-sessions store)))))))
      (e-session--write-index-now store)
      (message "Migrated %d, skipped %d, and failed %d session checkpoint(s) in %s"
               migrated skipped (length failures) directory)
      (when failures
        (error "Checkpoint migration failed: %S" (nreverse failures))))))

;;; migrate-session-checkpoints.el ends here
