;;; e-request.el --- Async request lifecycle contracts for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Small core contract for long request and cooperative job lifecycles.
;; The record is side-effect light.  Owners decide what events to display,
;; persist, or expose to adapters.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(cl-defstruct (e-request-lifecycle
               (:constructor e-request-lifecycle-create
                             (&key id owner session-id turn-id parent-id
                                   generation created-at state cancel-function
                                   progress terminal-payload cleanup-trigger
                                   diagnostics events)))
  id
  owner
  session-id
  turn-id
  parent-id
  generation
  (created-at (float-time))
  (state 'created)
  cancel-function
  progress
  terminal-payload
  cleanup-trigger
  diagnostics
  events)

(defconst e-request-terminal-states '(finished failed cancelled)
  "Terminal request lifecycle states.")

(defconst e-request-nonterminal-states '(created started progress)
  "Non-terminal request lifecycle states.")

(defvar e-request--hot-path-stack nil
  "Stack of marked interactive hot paths currently executing.")

(define-error 'e-request-invalid-transition
  "Invalid e request lifecycle transition")
(define-error 'e-request-blocking-call-in-hot-path
  "Blocking primitive called in an e interactive hot path")

(defun e-request-terminal-p (request)
  "Return non-nil when REQUEST is settled."
  (and (e-request-lifecycle-p request)
       (memq (e-request-lifecycle-state request) e-request-terminal-states)))

(defun e-request--record-event (request state payload)
  "Record STATE and PAYLOAD on REQUEST."
  (setf (e-request-lifecycle-events request)
        (append (e-request-lifecycle-events request)
                (list (list :state state
                            :at (float-time)
                            :payload payload))))
  request)

(defun e-request--record-diagnostic (request kind payload)
  "Record diagnostic KIND and PAYLOAD on REQUEST."
  (setf (e-request-lifecycle-diagnostics request)
        (append (e-request-lifecycle-diagnostics request)
                (list (list :kind kind
                            :at (float-time)
                            :payload payload))))
  request)

(defun e-request--settle (request state payload)
  "Settle REQUEST as STATE with PAYLOAD.
Return REQUEST when this call performs settlement.
Return nil when REQUEST was already terminal."
  (unless (memq state e-request-terminal-states)
    (signal 'e-request-invalid-transition
            (list "Not a terminal request state" state)))
  (cond
   ((not (e-request-lifecycle-p request))
    (signal 'wrong-type-argument (list 'e-request-lifecycle-p request)))
   ((e-request-terminal-p request)
    (e-request--record-diagnostic
     request 'late-terminal
     (list :state state :payload payload))
    nil)
   (t
    (setf (e-request-lifecycle-state request) state)
    (setf (e-request-lifecycle-terminal-payload request) payload)
    (e-request--record-event request state payload)
    (when-let ((cleanup (e-request-lifecycle-cleanup-trigger request)))
      (funcall cleanup request))
    request)))

(defun e-request-start (request &optional payload)
  "Mark REQUEST as started with optional PAYLOAD.
Return REQUEST when mutation occurred.
Return nil for stale calls after terminal settlement."
  (cond
   ((not (e-request-lifecycle-p request))
    (signal 'wrong-type-argument (list 'e-request-lifecycle-p request)))
   ((e-request-terminal-p request)
    (e-request--record-diagnostic request 'late-start payload)
    nil)
   ((memq (e-request-lifecycle-state request) '(created started progress))
    (setf (e-request-lifecycle-state request) 'started)
    (e-request--record-event request 'started payload))))

(defun e-request-progress (request payload)
  "Record non-terminal progress PAYLOAD on REQUEST.
Return REQUEST when mutation occurred.
Return nil for stale calls after terminal settlement."
  (cond
   ((not (e-request-lifecycle-p request))
    (signal 'wrong-type-argument (list 'e-request-lifecycle-p request)))
   ((e-request-terminal-p request)
    (e-request--record-diagnostic request 'late-progress payload)
    nil)
   (t
    (setf (e-request-lifecycle-state request) 'progress)
    (setf (e-request-lifecycle-progress request) payload)
    (e-request--record-event request 'progress payload))))

(defun e-request-finish (request payload)
  "Settle REQUEST as finished with PAYLOAD."
  (e-request--settle request 'finished payload))

(defun e-request-fail (request payload)
  "Settle REQUEST as failed with PAYLOAD."
  (e-request--settle request 'failed payload))

(defun e-request-cancel (request &optional payload)
  "Cancel REQUEST visibly and call its underlying cancellation function.
Cancellation is best effort at the underlying handle level, but settlement is
recorded exactly once at the lifecycle level."
  (when (and (e-request-lifecycle-p request)
             (not (e-request-terminal-p request)))
    (when-let ((cancel (e-request-lifecycle-cancel-function request)))
      (funcall cancel request)))
  (e-request--settle request 'cancelled payload))

(defun e-request-stale-generation-p (request generation)
  "Return non-nil when REQUEST should ignore callbacks for GENERATION."
  (let ((request-generation (and (e-request-lifecycle-p request)
                                 (e-request-lifecycle-generation request))))
    (and request-generation generation
         (not (equal request-generation generation)))))

(defmacro e-request-with-hot-path (name &rest body)
  "Run BODY while marking NAME as an interactive hot path."
  (declare (indent 1) (debug t))
  `(let ((e-request--hot-path-stack
          (cons ,name e-request--hot-path-stack)))
     ,@body))

(defun e-request-hot-path-active-p ()
  "Return non-nil when execution is inside a marked hot path."
  e-request--hot-path-stack)

(defun e-request-hot-path-blocking-error (primitive)
  "Signal that blocking PRIMITIVE ran in a marked hot path."
  (signal 'e-request-blocking-call-in-hot-path
          (list primitive (car e-request--hot-path-stack))))

(defmacro e-request-with-blocking-primitive-guard (&rest body)
  "Run BODY while rejecting known blocking primitives in marked hot paths.
This helper is intended for deterministic tests and focused probes."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'url-retrieve-synchronously)
              (lambda (&rest _args)
                (if (e-request-hot-path-active-p)
                    (e-request-hot-path-blocking-error
                     'url-retrieve-synchronously)
                  nil)))
             ((symbol-function 'process-file)
              (lambda (&rest _args)
                (if (e-request-hot-path-active-p)
                    (e-request-hot-path-blocking-error 'process-file)
                  0)))
             ((symbol-function 'accept-process-output)
              (lambda (&rest _args)
                (if (e-request-hot-path-active-p)
                    (e-request-hot-path-blocking-error
                     'accept-process-output)
                  nil))))
     ,@body))

(defun e-request-profile-span (event options thunk)
  "Run THUNK under profile EVENT when the dev profile framework is active.
OPTIONS must stay scalar and must not include prompt, response, context, or tool
body text."
  (if (and (fboundp 'e-dev-profile-enabled-p)
           (fboundp 'e-dev-profile-measure-thunk)
           (e-dev-profile-enabled-p))
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(provide 'e-request)

;;; e-request.el ends here
