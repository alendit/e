;;; e-graphical-test-support.el --- Graphical E2E helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Small helpers for deterministic tests running in a real graphical Emacs.
;; The controllable backend crosses the production asynchronous backend
;; contract, while test code decides when each provider item becomes visible.

;;; Code:

(require 'cl-lib)
(require 'e-backend)
(require 'ert)
(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory
                (or load-file-name
                    (and (boundp 'byte-compile-current-file)
                         byte-compile-current-file)
                    buffer-file-name))))
(require 'e-graphical-test-screenshot)

(cl-defstruct (e-graphical-test-stream
               (:constructor e-graphical-test-stream-create))
  on-item
  on-done
  on-error
  request
  timers
  failure
  requests)

(defun e-graphical-test-stream-cancel (stream)
  "Cancel pending timers and clear callbacks owned by STREAM."
  (dolist (timer (e-graphical-test-stream-timers stream))
    (when (timerp timer)
      (cancel-timer timer)))
  (setf (e-graphical-test-stream-timers stream) nil
        (e-graphical-test-stream-on-item stream) nil
        (e-graphical-test-stream-on-done stream) nil
        (e-graphical-test-stream-on-error stream) nil
        (e-graphical-test-stream-request stream) nil))

(defun e-graphical-test-stream-backend (stream)
  "Return an asynchronous backend controlled by STREAM."
  (e-backend-create
   :name "graphical-test-stream"
   :start
   (cl-function
    (lambda (&key messages options on-item on-done on-error on-request-start)
      (when (e-graphical-test-stream-request stream)
        (error "Graphical test backend already has an active request"))
      (setf (e-graphical-test-stream-requests stream)
            (append (e-graphical-test-stream-requests stream)
                    (list (list :messages (copy-tree messages)
                                :options (copy-tree options)))))
      (let ((request
             (e-backend-request-create
              :cancel (lambda ()
                        (e-graphical-test-stream-cancel stream)
                        t)
              :metadata '(:transport graphical-test :cancellable t))))
        (setf (e-graphical-test-stream-on-item stream) on-item
              (e-graphical-test-stream-on-done stream) on-done
              (e-graphical-test-stream-on-error stream) on-error
              (e-graphical-test-stream-request stream) request
              (e-graphical-test-stream-failure stream) nil)
        (when on-request-start
          (funcall on-request-start request))
        request)))))

(defun e-graphical-test-stream-active-p (stream)
  "Return non-nil when STREAM owns a live backend request."
  (and (e-graphical-test-stream-request stream)
       (e-graphical-test-stream-on-item stream)))

(defun e-graphical-test-stream--schedule (stream function &optional delay)
  "Run FUNCTION from STREAM after DELAY seconds through the real timer loop."
  (let (timer)
    (setq timer
          (run-at-time
           (or delay 0.01) nil
           (lambda ()
             (setf (e-graphical-test-stream-timers stream)
                   (delq timer (e-graphical-test-stream-timers stream)))
             (condition-case err
                 (funcall function)
               (error
                (setf (e-graphical-test-stream-failure stream) err)
                (when-let ((on-error
                            (e-graphical-test-stream-on-error stream)))
                  (funcall on-error err)))))))
    (push timer (e-graphical-test-stream-timers stream))
    timer))

(defun e-graphical-test-stream-emit (stream item &optional delay)
  "Deliver backend ITEM through STREAM after DELAY seconds."
  (unless (e-graphical-test-stream-active-p stream)
    (error "Graphical test backend has no active request"))
  (when (e-graphical-test-screenshot-enabled-p)
    (e-graphical-test-capture-state
     (format "provider-%s-before" (or (plist-get item :type) "item"))))
  (e-graphical-test-stream--schedule
   stream
   (lambda ()
     (funcall (e-graphical-test-stream-on-item stream) item)
     (when (e-graphical-test-screenshot-enabled-p)
       (e-graphical-test-capture-state
        (format "provider-%s-after" (or (plist-get item :type) "item")))))
   delay))

(defun e-graphical-test-stream-finish (stream &optional delay reason)
  "Finish STREAM successfully after DELAY seconds with terminal REASON."
  (unless (e-graphical-test-stream-active-p stream)
    (error "Graphical test backend has no active request"))
  (when (e-graphical-test-screenshot-enabled-p)
    (e-graphical-test-capture-state "provider-finish-before"))
  (e-graphical-test-stream--schedule
   stream
   (lambda ()
     (let ((on-item (e-graphical-test-stream-on-item stream))
           (on-done (e-graphical-test-stream-on-done stream)))
       ;; Terminal handling may synchronously start a follow-up provider
       ;; request (notably after tool use).  Retire this request first so the
       ;; follow-up can install callbacks without looking concurrent, and do
       ;; not clear those new callbacks after the old request finishes.
       (setf (e-graphical-test-stream-on-item stream) nil
             (e-graphical-test-stream-on-done stream) nil
             (e-graphical-test-stream-on-error stream) nil
             (e-graphical-test-stream-request stream) nil)
       (funcall on-item (list :type 'done :reason (or reason 'stop)))
       (when on-done
         (funcall on-done '(:status done)))
       (when (e-graphical-test-screenshot-enabled-p)
         (e-graphical-test-capture-state "provider-finish-after"))))
   delay))

(defun e-graphical-test-wait-until (predicate &optional timeout description)
  "Wait for PREDICATE through graphical redisplay or fail after TIMEOUT.
DESCRIPTION names the expected state in failure output.  A state is settled
only after it remains true across an event-loop and forced-redisplay cycle;
native window restoration can otherwise invalidate a just-observed window tree
on the first redisplay after this helper returns."
  (let ((deadline (+ (float-time) (or timeout 2.0)))
        (stable-observations 0)
        value)
    (while (and (< stable-observations 2)
                (< (float-time) deadline))
      (setq value (funcall predicate))
      (setq stable-observations
            (if value (1+ stable-observations) 0))
      (unless (= stable-observations 2)
        ;; Exercise the same event-loop and redisplay boundary that the next
        ;; user command will cross before declaring graphical state settled.
        (sit-for 0.01)
        (redisplay t)))
    (unless (= stable-observations 2)
      (when (e-graphical-test-screenshot-enabled-p)
        (e-graphical-test-capture-state
         (format "timeout-%s" (or description "graphical-ui-state"))))
      (ert-fail (format "Timed out waiting for %s"
                        (or description "graphical UI state"))))
    (when (e-graphical-test-screenshot-enabled-p)
      (e-graphical-test-capture-state
       (format "settled-%s" (or description "graphical-ui-state"))))
    value))

(defun e-graphical-test-send-keys (keys)
  "Execute KEYS as one user keyboard macro and complete redisplay."
  (e-graphical-test-capture-automatic-transition
   (format "keys-%s" (if (stringp keys) keys "macro"))
   (lambda ()
     (execute-kbd-macro (if (stringp keys) (kbd keys) keys))
     (sit-for 0.01)
     (redisplay t))))

(defun e-graphical-test-type-text (text)
  "Type TEXT through the selected window's command loop."
  (e-graphical-test-capture-automatic-transition
   "type-text"
   (lambda ()
     (execute-kbd-macro text)
     (sit-for 0.01)
     (redisplay t))))

(defun e-graphical-test-tail-y (window position)
  "Return POSITION's graphical Y coordinate in WINDOW, or nil."
  (when-let ((posn (posn-at-point (max (point-min) (1- position)) window)))
    (cdr (posn-x-y posn))))

(provide 'e-graphical-test-support)

;;; e-graphical-test-support.el ends here
