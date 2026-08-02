;;; e-await-tool.el --- Model-facing event-driven await tool for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; One model-facing async tool that waits on a set of work references until they
;; settle or a generous timeout expires, then returns a compact per-reference
;; report.  It is the interactive, non-blocking alternative to polling status
;; across turns: the harness holds the model's turn open (async interactive
;; policy) while `e-work-await-set' waits event-driven, without freezing Emacs.
;;
;; The tool is generic over `e-work' handles: it resolves each reference through
;; the `e-waitable' registry and never knows about any specific subsystem.
;; Subagents, task-queue tasks, and elisp-jobs become awaitable by registering
;; their scheme, not by changes here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-tools)
(require 'e-work)
(require 'e-waitable)

(defcustom e-await-tool-default-timeout 120
  "Default seconds the await tool waits before returning a timed-out report."
  :type 'number
  :group 'e)

(defcustom e-await-tool-max-timeout 900
  "Ceiling in seconds for the await tool timeout.
Explicit requests above this ceiling are rejected."
  :type 'number
  :group 'e)

(define-error 'e-await-tool-invalid-request "Invalid await request")

(defun e-await-tool--effective-timeout (arguments)
  "Return ARGUMENTS' declared timeout or reject an invalid explicit value."
  (if (not (plist-member arguments :timeout))
      e-await-tool-default-timeout
    (let ((requested (plist-get arguments :timeout)))
      (unless (and (numberp requested) (> requested 0)
                   (<= requested e-await-tool-max-timeout))
        (signal 'e-await-tool-invalid-request
                (list "Timeout must be finite, positive, and within the hard maximum")))
      requested)))

(defun e-await-tool--normalize-mode (arguments)
  "Return the await mode symbol (`all' or `any') from ARGUMENTS."
  (pcase (plist-get arguments :mode)
    ((or 'nil "all" 'all) 'all)
    ((or "any" 'any) 'any)
    (other (signal 'wrong-type-argument (list '(member "all" "any") other)))))

(defun e-await-tool--handle-status (handle)
  "Return a compact terminal snapshot plist for HANDLE.
The snapshot exposes the work state plus the settled result or error, and never
inlines a transcript; detail stays behind the target subsystem's own reads."
  (let* ((status (e-work-status handle))
         (state (plist-get status :state)))
    (list :state state
          :result (plist-get status :result)
          :error (plist-get status :error))))

(defun e-await-tool--resolve-references (refs)
  "Resolve every REF in REFS, rejecting the complete request on any failure."
  (let (pairs errors)
    (dolist (ref (append refs nil))
      (let ((resolved (e-waitable-resolve ref)))
        (if-let ((handle (plist-get resolved :handle)))
            (push (cons ref handle) pairs)
          (push (list :ref ref :status 'error
                      :error (plist-get resolved :error))
                errors))))
    (when errors
      (signal 'e-await-tool-invalid-request
              (list "Every await reference must resolve" (nreverse errors))))
    (nreverse pairs)))

(defun e-await-tool--result-entry (ref handle)
  "Return the report entry for REF backed by HANDLE."
  (let* ((snapshot (e-await-tool--handle-status handle))
         (result (plist-get snapshot :result)))
    (list :ref ref
          :state (plist-get snapshot :state)
          ;; Surface a subsystem-normalized summary/outputs when the work result
          ;; carries them; otherwise expose the raw result under :result.
          :summary (and (listp result) (plist-get result :summary))
          :outputs (and (listp result) (plist-get result :outputs))
          :result result
          :error (plist-get snapshot :error))))

(defun e-await-tool--report (mode reason pairs)
  "Return the compact await report for MODE, REASON, and frozen PAIRS."
  (list :settled (eq reason 'complete)
        :reason reason
        :mode mode
        :results
        (mapcar (lambda (pair)
                  (e-await-tool--result-entry (car pair) (cdr pair)))
                pairs)))

(cl-defun e-await-tool--start
    (&key arguments context on-done on-error &allow-other-keys)
  "Start invocation-only await aggregation without creating executable work."
  (condition-case err
      (let* ((refs (plist-get arguments :refs))
             (mode (e-await-tool--normalize-mode arguments))
             (timeout (e-await-tool--effective-timeout arguments))
             (pairs (progn
                      (unless (and (or (listp refs) (vectorp refs))
                                   (> (length refs) 0))
                        (signal 'e-await-tool-invalid-request
                                (list "Await requires a non-empty reference list")))
                      (e-await-tool--resolve-references refs)))
             (handles (mapcar #'cdr pairs)))
        (cond
         ((null handles)
          (signal 'e-await-tool-invalid-request
                  (list "Await requires at least one resolved work handle")))
         (t
          (let ((cancel
                 (if-let ((subscribe (plist-get context :board-subscribe-aggregation)))
                     (funcall subscribe
                              handles mode timeout
                              (lambda (reason)
                                (funcall on-done
                                         (e-await-tool--report mode reason pairs))))
                   (e-work-await-set
                    handles :mode mode :timeout timeout
                    :on-settle
                    (lambda (set-report)
                      (funcall on-done
                               (e-await-tool--report
                                mode (plist-get set-report :reason) pairs)))))))
            (e-tools-request-create :cancel (lambda () (funcall cancel) t))))))
    (error (when on-error (funcall on-error err)) nil)))

(defun e-await-tool-register (registry)
  "Register the model-facing await tool in REGISTRY."
  (e-tools-register
   registry
   :name "await"
   :description "Wait for async work references to settle without blocking Emacs."
   :parameters '(:type "object"
                 :properties
                 (:refs (:type "array"
                         :items (:type "string"))
                  :mode (:type "string"
                         :enum ["all" "any"])
                  :timeout (:type "number"))
                 :required ["refs"])
    :start #'e-await-tool--start
    :invocation-only t
   :blocking-class 'unknown))

(provide 'e-await-tool)

;;; e-await-tool.el ends here
