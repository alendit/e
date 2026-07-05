;;; e-backend.el --- Backend contract for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral backend adapter contract for the core runtime.

;;; Code:

(require 'cl-lib)
(require 'e-request)

(cl-defstruct (e-backend-request (:constructor e-backend-request-create))
  cancel
  metadata)

(cl-defstruct (e-backend
               (:constructor e-backend-create)
               (:conc-name e-backend--))
  name
  stream
  start)

(defvar e-backend--request-start-callback nil
  "Dynamically scoped callback for backend request handles.")

(defun e-backend-note-request-started (request)
  "Publish REQUEST as the active provider request for the current stream."
  (when e-backend--request-start-callback
    (funcall e-backend--request-start-callback request))
  request)

(defun e-backend-cancel-request (request)
  "Cancel REQUEST when it has a provider cancellation function."
  (when-let ((cancel (and (e-backend-request-p request)
                          (e-backend-request-cancel request))))
    (funcall cancel)))

(cl-defun e-backend-stream-batch
    (backend &key messages options on-item on-request-start)
  "Synchronously stream a backend turn through BACKEND from batch/test code.
MESSAGES and OPTIONS are backend-neutral plists/lists.  ON-ITEM receives
backend-neutral stream items.  ON-REQUEST-START receives an optional
`e-backend-request' handle when the adapter can expose request state."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-backend-stream-batch))
  (cond
   ((functionp (e-backend--stream backend))
    (let ((e-backend--request-start-callback on-request-start))
      (funcall (e-backend--stream backend)
               :messages messages
               :options options
               :on-item on-item)))
   ((functionp (e-backend--start backend))
    (let ((done nil)
          (result nil)
          (failure nil))
      (e-backend-start
       backend
       :messages messages
       :options options
       :on-item on-item
       :on-done (lambda (value)
                  (setq result value)
                  (setq done t))
       :on-error (lambda (err)
                   (setq failure err)
                   (setq done t))
       :on-request-start on-request-start)
      (while (not done)
        (accept-process-output nil 0.01))
      (when failure
        (signal (car failure) (cdr failure)))
      result))
   (t
    (signal 'wrong-type-argument
            (list 'functionp (e-backend--stream backend))))))

(cl-defun e-backend-start
    (backend &key messages options on-item on-done on-error on-request-start)
  "Start a backend turn through BACKEND without blocking.
MESSAGES and OPTIONS are backend-neutral inputs.  ON-ITEM receives stream
items.  ON-DONE receives a result plist after the request completes.  ON-ERROR
receives an Emacs condition list.  ON-REQUEST-START receives an optional
`e-backend-request' handle.  Return the request handle when available."
  (cond
   ((functionp (e-backend--start backend))
    (funcall (e-backend--start backend)
             :messages messages
             :options options
             :on-item on-item
             :on-done on-done
             :on-error on-error
             :on-request-start on-request-start))
   ((functionp (e-backend--stream backend))
    (let ((cancelled nil)
          (timer nil)
          request)
      (setq request
            (e-backend-request-create
             :cancel (lambda ()
                       (setq cancelled t)
                       (when (timerp timer)
                         (cancel-timer timer))
                       t)
             :metadata '(:transport timer :cancellable queued-only)))
      (when on-request-start
        (funcall on-request-start request))
      (setq timer
            (run-at-time
             0 nil
             (lambda ()
               (unless cancelled
                 (condition-case err
                     (progn
                       (e-backend-stream-batch
                        backend
                        :messages messages
                        :options options
                        :on-item on-item
                        :on-request-start on-request-start)
                       (when on-done
                         (funcall on-done '(:status done))))
                   (error
                    (when on-error
                      (funcall on-error err))))))))
      request))
   (t
    (signal 'wrong-type-argument
            (list 'functionp (e-backend--start backend))))))

(cl-defun e-backend-fake-create (&key name items cancel-function delay)
  "Create fake backend NAME that streams ITEMS synchronously.
CANCEL-FUNCTION is attached to the fake request handle when non-nil.
DELAY controls async fake delivery in seconds."
  (e-backend-create
   :name (or name "fake")
   :stream (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (when cancel-function
                (e-backend-note-request-started
                 (e-backend-request-create :cancel cancel-function)))
              (dolist (item items)
                (funcall on-item item))))
   :start (cl-function
           (lambda (&key messages options on-item on-done on-error
                         on-request-start)
             (ignore messages options)
             (let ((cancelled nil)
                   (timer nil)
                   request)
               (setq request
                     (e-backend-request-create
                      :cancel (lambda ()
                                (setq cancelled t)
                                (when (timerp timer)
                                  (cancel-timer timer))
                                (when cancel-function
                                  (funcall cancel-function))
                                t)
                      :metadata '(:transport timer :cancellable t)))
               (when on-request-start
                 (funcall on-request-start request))
               (setq timer
                     (run-at-time
                      (or delay 0)
                      nil
                      (lambda ()
                        (unless cancelled
                          (condition-case err
                              (progn
                                (dolist (item items)
                                  (funcall on-item item))
                                (when on-done
                                  (funcall on-done '(:status done))))
                            (error
                             (when on-error
                               (funcall on-error err))))))))
               request)))))

(provide 'e-backend)

;;; e-backend.el ends here
