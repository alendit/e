;;; e-runtime-sqlite.el --- One-store runtime composition -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This is the sole ordinary composition root for Feature 87 after P4 cutover.
;; It opens one runtime-store worker, lends that physical runtime to every
;; owner-shaped adapter, and owns the only close.  It contains no domain policy.

;;; Code:

(require 'cl-lib)
(require 'e-board-storage-sqlite)
(require 'e-cron)
(require 'e-cron-storage-sqlite)
(require 'e-goodnite-resources)
(require 'e-goodnite-storage-sqlite)
(require 'e-raw-results)
(require 'e-raw-results-storage-sqlite)
(require 'e-runtime-store)
(require 'e-session-sqlite)
(require 'e-task-queue)
(require 'e-task-storage-sqlite)
(require 'e-voice-adjustment)
(require 'e-voice-storage-sqlite)

(define-error 'e-runtime-sqlite-live-composition
  "A SQLite runtime composition is already live")

(defvar e-runtime-sqlite--live-composition nil
  "Live composition or in-progress reservation for this Emacs process.")

(cl-defstruct (e-runtime-sqlite
               (:constructor e-runtime-sqlite--create)
               (:conc-name e-runtime-sqlite--))
  directory runtime-store session-store board-storage task-storage task-queue
  cron-storage voice-storage goodnite-storage raw-results-storage closed)

(defun e-runtime-sqlite-runtime-store (runtime)
  "Return RUNTIME's one physical runtime-store."
  (e-runtime-sqlite--runtime-store runtime))

(defun e-runtime-sqlite-session-store (runtime)
  "Return RUNTIME's session adapter."
  (e-runtime-sqlite--session-store runtime))

(defun e-runtime-sqlite-board-storage (runtime)
  "Return RUNTIME's Board storage port."
  (e-runtime-sqlite--board-storage runtime))

(defun e-runtime-sqlite-task-queue (runtime)
  "Return RUNTIME's task queue."
  (e-runtime-sqlite--task-queue runtime))

(defun e-runtime-sqlite-task-storage (runtime)
  "Return RUNTIME's task storage port."
  (e-runtime-sqlite--task-storage runtime))

(defun e-runtime-sqlite-cron-storage (runtime)
  "Return RUNTIME's cron storage port."
  (e-runtime-sqlite--cron-storage runtime))

(defun e-runtime-sqlite-voice-storage (runtime)
  "Return RUNTIME's voice storage port."
  (e-runtime-sqlite--voice-storage runtime))

(defun e-runtime-sqlite-goodnite-storage (runtime)
  "Return RUNTIME's Goodnite demand storage port."
  (e-runtime-sqlite--goodnite-storage runtime))

(defun e-runtime-sqlite-raw-results-storage (runtime)
  "Return RUNTIME's raw-result storage port."
  (e-runtime-sqlite--raw-results-storage runtime))

(cl-defun e-runtime-sqlite-open
    (directory &key load-sessions load-task-queue task-runner
               task-producer-binding (task-queue-id "default"))
  "Open one SQLite runtime rooted at DIRECTORY.

The returned composition injects one shared physical runtime through separate
session, Board, task, cron, voice, Goodnite, and raw-result owner ports.
LOAD-TASK-QUEUE should be used only after TASK-RUNNER or TASK-PRODUCER-BINDING
provides process-local execution authority."
  (when e-runtime-sqlite--live-composition
    (signal 'e-runtime-sqlite-live-composition
            (list "Close the active SQLite runtime before opening another")))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (reservation (list 'opening directory))
         runtime-store session-store board-storage task-storage task-queue
         cron-storage voice-storage goodnite-storage raw-storage composition)
    ;; Reserve ownership before opening or mutating any store.  Reentrant timer
    ;; callbacks and separate-directory opens therefore cannot replace globals.
    (setq e-runtime-sqlite--live-composition reservation)
    (condition-case err
        (progn
          (setq runtime-store (e-runtime-store-open directory))
          (setq session-store
                (e-session-sqlite-store-create
                 directory :load-all load-sessions
                 :runtime-store runtime-store)
                board-storage
                (e-board-storage-sqlite-create runtime-store)
                task-storage
                (e-task-storage-sqlite-create runtime-store)
                task-queue
                (e-task-queue-create
                 :id task-queue-id :storage task-storage
                 :runner task-runner :producer-binding task-producer-binding
                 ;; A live composition is process-global, so its task queue is
                 ;; the one public queue eligible for =task:= waitable links.
                 :expose-await-references-p t)
                cron-storage
                (e-cron-storage-sqlite-create runtime-store)
                voice-storage
                (e-voice-storage-sqlite-create runtime-store)
                goodnite-storage
                (e-goodnite-storage-sqlite-create runtime-store)
                raw-storage
                (e-raw-results-storage-sqlite-create runtime-store))
          (e-cron-configure-storage cron-storage)
          (e-voice-adjustment-configure-storage voice-storage)
          (e-goodnite-resources-configure-storage goodnite-storage)
          (e-raw-results-configure-storage raw-storage)
          (when load-task-queue
            (e-task-queue-load task-queue))
          (setq composition
                (e-runtime-sqlite--create
                 :directory directory :runtime-store runtime-store
                 :session-store session-store :board-storage board-storage
                 :task-storage task-storage :task-queue task-queue
                 :cron-storage cron-storage :voice-storage voice-storage
                 :goodnite-storage goodnite-storage
                 :raw-results-storage raw-storage))
          (setq e-runtime-sqlite--live-composition composition)
          composition)
      (error
       (when session-store
         (ignore-errors (e-session-sqlite-store-close session-store)))
       (e-cron-configure-storage nil)
       (e-voice-adjustment-configure-storage nil)
       (e-goodnite-resources-configure-storage nil)
       (e-raw-results-configure-storage nil)
       (when runtime-store
         (ignore-errors (e-runtime-store-close runtime-store)))
       (when (eq e-runtime-sqlite--live-composition reservation)
         (setq e-runtime-sqlite--live-composition nil))
       (signal (car err) (cdr err))))))

(defun e-runtime-sqlite-close (runtime)
  "Close RUNTIME exactly once and detach its borrowed owner adapters."
  (unless (e-runtime-sqlite--closed runtime)
    (when (eq e-cron-storage (e-runtime-sqlite--cron-storage runtime))
      (e-cron-configure-storage nil))
    (when (eq e-voice-adjustment-storage
              (e-runtime-sqlite--voice-storage runtime))
      (e-voice-adjustment-configure-storage nil))
    (when (eq e-goodnite-resources-storage
              (e-runtime-sqlite--goodnite-storage runtime))
      (e-goodnite-resources-configure-storage nil))
    (when (eq e-raw-results-storage
              (e-runtime-sqlite--raw-results-storage runtime))
      (e-raw-results-configure-storage nil))
    (e-session-sqlite-store-close
     (e-runtime-sqlite--session-store runtime))
    (e-runtime-store-close (e-runtime-sqlite--runtime-store runtime))
    (setf (e-runtime-sqlite--closed runtime) t)
    (when (eq e-runtime-sqlite--live-composition runtime)
      (setq e-runtime-sqlite--live-composition nil)))
  t)

(defun e-runtime-sqlite-status (runtime)
  "Return bounded shared-runtime and owner-injection status."
  (append
   (list :directory (e-runtime-sqlite--directory runtime)
         :closed (and (e-runtime-sqlite--closed runtime) t)
         :owners '(session board task cron voice goodnite raw-results))
   (e-runtime-store-status (e-runtime-sqlite--runtime-store runtime))))

(provide 'e-runtime-sqlite)

;;; e-runtime-sqlite.el ends here
