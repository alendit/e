;;; e-chat-sql-e2e-support.el --- SQL chat E2E helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic helpers for E2E tests that drive the public asynchronous chat
;; service over disposable SQLite.  They retain only live subscriptions and
;; terminal observations owned by the currently running test.

;;; Code:

(require 'cl-lib)
(require 'e-chat-service)
(require 'e-harness)
(require 'e-session-async)
(require 'e-session-sqlite)
(require 'e-work)

(defvar e-chat-sql-e2e--terminal-events (make-hash-table :test 'eq)
  "Per-harness terminal events captured by the active E2E test.")

(defvar e-chat-sql-e2e--activity-subscriptions (make-hash-table :test 'eq)
  "Per-harness activity subscriptions owned by the active E2E test.")

(defvar e-chat-sql-e2e--fixtures nil
  "Disposable STORE . DIRECTORY pairs owned by isolated SQL chat tests.")

(defun e-chat-sql-e2e-make-harness (&rest arguments)
  "Return a HARNESS from ARGUMENTS backed by disposable asynchronous SQLite."
  (let* ((directory (make-temp-file "e-chat-sql-e2e-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (harness
          (apply #'e-harness-create
                 (append arguments (list :sessions store)))))
    (push (cons store directory) e-chat-sql-e2e--fixtures)
    harness))

(defun e-chat-sql-e2e--event-table (harness)
  "Return the terminal event table for HARNESS."
  (or (gethash harness e-chat-sql-e2e--terminal-events)
      (let ((table (make-hash-table :test 'equal)))
        (puthash harness table e-chat-sql-e2e--terminal-events)
        table)))

(defun e-chat-sql-e2e--ensure-observer (harness)
  "Install the one terminal activity observer for HARNESS."
  (unless (gethash harness e-chat-sql-e2e--activity-subscriptions)
    (let ((table (e-chat-sql-e2e--event-table harness)))
      (puthash
       harness
       (e-harness-activity-subscribe
        harness
        (lambda (event)
          (when (memq (plist-get event :type)
                      '(turn-finished turn-failed turn-cancelled))
            (puthash (plist-get event :session-id) (copy-tree event t)
                     table))))
       e-chat-sql-e2e--activity-subscriptions))))

(defun e-chat-sql-e2e--await (work &optional timeout)
  "Observe request-scoped WORK at this explicit E2E boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout (or timeout 30.0))))

(defun e-chat-sql-e2e-wait-until (predicate &optional timeout)
  "Return PREDICATE's value once non-nil, or nil after TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 1.0))) value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (sit-for 0.01))
    value))

(defun e-chat-sql-e2e-reset ()
  "Release all live coordination retained by the active E2E test."
  (maphash
   (lambda (_harness bindings)
     (maphash
      (lambda (_session-id binding)
        (when (e-chat-service-binding-p binding)
          (e-chat-service--retire-binding binding)))
      bindings))
   e-chat-service--bindings)
  (maphash
   (lambda (harness subscription)
     (ignore-errors (e-harness-activity-unsubscribe harness subscription)))
   e-chat-sql-e2e--activity-subscriptions)
  (clrhash e-chat-sql-e2e--activity-subscriptions)
  (clrhash e-chat-sql-e2e--terminal-events)
  (dolist (fixture e-chat-sql-e2e--fixtures)
    (ignore-errors (e-session-sqlite-store-close (car fixture)))
    (when (file-directory-p (cdr fixture))
      (delete-directory (cdr fixture) t)))
  (setq e-chat-sql-e2e--fixtures nil)
  (dolist (function '(e-chat-service--observer-drain-callback
                      e-chat-service--subscription-drain-callback))
    (cancel-function-timers function)))

(cl-defun e-chat-sql-e2e-create-session (harness &key id metadata)
  "Create and bind one SQL chat session in HARNESS."
  (let* ((session-id (or id (e-session-generate-id)))
         (creation
          (e-chat-service-create-session-start
           :harness harness :id session-id :metadata metadata))
         (binding (e-chat-service-binding-start harness session-id nil t)))
    (e-chat-sql-e2e--await binding)
    (e-chat-sql-e2e--await creation)
    (e-chat-sql-e2e--ensure-observer harness)
    session-id))

(defun e-chat-sql-e2e-prompt-async (harness session-id prompt)
  "Submit PROMPT through the public SQL chat service and return its source id."
  (e-chat-sql-e2e--ensure-observer harness)
  (remhash session-id (e-chat-sql-e2e--event-table harness))
  (e-chat-service-submit-session harness session-id prompt))

(defun e-chat-sql-e2e--assistant-content (harness session-id timeout)
  "Return the newest durable assistant content for SESSION-ID.
The terminal activity event can precede the detached SQLite append, so wait
for the bounded chat view to include the assistant message."
  (e-chat-sql-e2e-wait-until
   (lambda ()
     (let* ((view
             (e-chat-sql-e2e--await
              (e-session-async-chat-view
               (e-harness-sessions harness) session-id :limit 64)
              timeout))
            (messages (plist-get view :messages)))
       (plist-get
        (car (last (cl-remove-if-not
                    (lambda (message)
                      (eq (plist-get message :role) 'assistant))
                    messages)))
        :content)))
   timeout))

(defun e-chat-sql-e2e-wait-batch (harness session-id &optional timeout)
  "Wait for HARNESS SESSION-ID's observed terminal event and return a result."
  (let* ((timeout (or timeout 30.0))
         (event
          (e-chat-sql-e2e-wait-until
           (lambda ()
             (gethash session-id (e-chat-sql-e2e--event-table harness)))
           timeout)))
    (unless event
      (error "E2E turn did not settle within %.1f seconds" timeout))
    (pcase (plist-get event :type)
      ('turn-finished
       (let ((content
              (e-chat-sql-e2e--assistant-content
               harness session-id timeout)))
         (list :status 'done :assistant-content content
               :result (list :assistant-content content
                             :reason (plist-get (plist-get event :payload)
                                                :reason)))))
      (_
       (list :status 'error
             :error (or (plist-get (plist-get event :payload) :error)
                        (format "%s" (plist-get event :type))))))))

(defun e-chat-sql-e2e-prompt-batch
    (harness session-id prompt &optional timeout)
  "Submit PROMPT and return its terminal result at this E2E boundary."
  (e-chat-sql-e2e-prompt-async harness session-id prompt)
  (e-chat-sql-e2e-wait-batch harness session-id timeout))

(provide 'e-chat-sql-e2e-support)

;;; e-chat-sql-e2e-support.el ends here
