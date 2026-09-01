;;; e-session-sqlite.el --- Opt-in SQLite session composition -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the opt-in SQLite store composition and cooperative indexed replay.
;; Session semantic mutation remains in the facade/aggregate; physical record
;; operations remain in the session-storage SQLite adapter.

;;; Code:

(require 'cl-lib)
(require 'e-request)
(require 'e-runtime-store)
(require 'e-session-aggregate)
(require 'e-session-storage)

(declare-function e-session--apply-physical-record "e-session")
(declare-function e-session--begin-checkpoint-replay "e-session")
(declare-function e-session--finish-replay "e-session")
(declare-function e-session--load-index "e-session")
(declare-function e-session--read-checkpoint "e-session")
(declare-function e-session--reconcile-journal-roots "e-session")
(declare-function e-session-generate-ulid "e-session-identity")
(declare-function e-session-load "e-session")

(defvar e-session--load-in-progress)

(cl-defun e-session-sqlite-load-session-start
    (store session-id &key on-done on-error on-progress (page-size 256))
  "Start indexed cooperative SQLite replay for SESSION-ID."
  (let* ((header (e-session-storage-session-header store session-id))
         (checkpoint (condition-case nil
                         (e-session--read-checkpoint store session-id)
                       (e-session-checkpoint-missing nil)))
         (position (or (and checkpoint
                            (plist-get checkpoint :journal-byte-offset))
                       0))
         (total (plist-get header :record-count))
         timer request)
    (unless (plist-get header :present)
      (signal 'e-session-missing (list session-id)))
    (if checkpoint
        (e-session--begin-checkpoint-replay store session-id checkpoint)
      (e-session-aggregate-reset-session store session-id))
    (cl-labels
        ((clear-timer ()
           (when (timerp timer) (cancel-timer timer))
           (setq timer nil))
         (fail (err)
           (unless (e-request-terminal-p request)
             (clear-timer)
             (e-request-fail request err)
             (when on-error (funcall on-error err))))
         (finish ()
           (unless (e-request-terminal-p request)
             (clear-timer)
             (condition-case err
                 (let ((session (e-session--finish-replay store session-id)))
                   (e-request-finish request session)
                   (when on-done (funcall on-done session)))
               (error (fail err)))))
         (schedule () (setq timer (run-at-time 0 nil #'step)))
         (step ()
           (unless (e-request-terminal-p request)
             (condition-case err
                 (let* ((page (e-session-storage-read-session-page
                               store session-id position page-size))
                        (entries (plist-get page :records)))
                   (let ((e-session--load-in-progress t))
                     (dolist (entry entries)
                       (e-session--apply-physical-record
                        store (plist-get entry :value))))
                   (when entries
                     (setq position (plist-get (car (last entries)) :position)))
                   (let ((payload (list :session-id session-id
                                        :records-read position
                                        :records-total total)))
                     (e-request-progress request payload)
                     (when on-progress (funcall on-progress payload)))
                   (if (plist-get page :next) (schedule) (finish)))
               (error (fail err))))))
      (setq request
            (e-request-lifecycle-create
             :id (e-session-generate-ulid) :owner 'e-session-load
             :session-id session-id :state 'created
             :cancel-function (lambda (_request) (clear-timer))))
      (e-request-start request (list :session-id session-id
                                     :records-total total))
      (schedule)
      request)))

(cl-defun e-session-sqlite-store-create (&optional directory &key load-all)
  "Create an opt-in SQLite-backed session store in DIRECTORY."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or directory e-session-directory))))
         (runtime-store (e-runtime-store-open directory))
         (store (e-session-store-create
                 :directory directory :sessions-directory nil :index-file nil
                 :persistent t :write-mode 'sqlite)))
    (condition-case err
        (progn
          (e-session-storage-register
           store :directory directory :persistent t :write-mode 'sqlite
           :backend 'sqlite :runtime-store runtime-store)
          (if load-all
              (e-session-load store)
            (if (e-session--load-index store)
                (e-session--reconcile-journal-roots store t)
              (e-session--reconcile-journal-roots store)))
          store)
      (error
       (e-runtime-store-close runtime-store)
       (signal (car err) (cdr err))))))

(defun e-session-sqlite-store-close (store)
  "Close STORE's subordinate SQLite worker."
  (e-session-storage-close store))

(provide 'e-session-sqlite)

;;; e-session-sqlite.el ends here
