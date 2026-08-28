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

(defconst e-await-tool-max-references 32
  "Maximum number of references accepted by one await invocation.")

(defconst e-await-tool-max-inline-result-bytes 4096
  "Maximum aggregate string bytes retained in one inline await result.")

(defconst e-await-tool-max-inline-result-nodes 128
  "Maximum aggregate Lisp nodes inspected for one inline await result.")

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
          :progress (plist-get status :progress)
          :result (plist-get status :result)
          :error (plist-get status :error))))

(defun e-await-tool--inline-value-p (value)
  "Return non-nil when VALUE fits the fixed inline report budget.
The traversal stops at the first byte or node overflow, so an await callback
never serializes or walks an unbounded result on the Emacs main thread."
  (let ((pending (list value))
        (nodes 0)
        (bytes 0)
        overflow)
    (while (and pending (not overflow))
      (let ((item (pop pending)))
        (setq nodes (1+ nodes))
        (cond
         ;; A file-backed value is deliberately opaque outside the lifecycle
         ;; stages that archive or present it.  Never expose its host path in
         ;; an await report, even when the carrier itself is structurally small.
         ((e-tools-file-content-p item)
          (setq overflow t))
         ((> nodes e-await-tool-max-inline-result-nodes)
          (setq overflow t))
         ((stringp item)
          (setq bytes (+ bytes (string-bytes item)))
          (when (> bytes e-await-tool-max-inline-result-bytes)
            (setq overflow t)))
         ((consp item)
          (push (car item) pending)
          (push (cdr item) pending))
         ((vectorp item)
          (let ((index 0)
                (length (length item)))
            (if (> (+ nodes length) e-await-tool-max-inline-result-nodes)
                (setq overflow t)
              (while (< index length)
                (push (aref item index) pending)
                (setq index (1+ index)))))))))
    (not overflow)))

(defun e-await-tool--inline-or-reference (value ref)
  "Return VALUE when bounded, otherwise one tombstone referring to REF."
  (if (e-await-tool--inline-value-p value)
      value
    (list :omitted t :result-ref ref :reason 'inline-budget-exceeded)))

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
         (state (plist-get snapshot :state))
         (pending (memq state '(started progress)))
         (progress (plist-get snapshot :progress))
         (result (plist-get snapshot :result))
         (summary (and (listp result) (plist-get result :summary)))
         (outputs (and (listp result) (plist-get result :outputs))))
    (append
     (list :ref ref
           :state state
           ;; Surface a subsystem-normalized summary/outputs when the work result
           ;; carries them; otherwise expose the raw result under :result.
           :summary (e-await-tool--inline-or-reference summary ref)
           :outputs (e-await-tool--inline-or-reference outputs ref)
           :result (e-await-tool--inline-or-reference result ref)
           :error (e-await-tool--inline-or-reference
                   (plist-get snapshot :error) ref))
     (when pending
       (list :progress (e-await-tool--inline-or-reference progress ref)
             :progress-sequence (and (listp progress)
                                     (plist-get progress :sequence))
             :progress-age-seconds
             (let ((at (and (listp progress) (plist-get progress :at))))
               (and (numberp at) (max 0.0 (- (float-time) at)))))))))

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
                                   (> (length refs) 0)
                                   (<= (length refs)
                                       e-await-tool-max-references))
                        (signal 'e-await-tool-invalid-request
                                (list "Await requires a non-empty bounded reference list")))
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
                                         (e-await-tool--report mode reason pairs)))
                              context)
                   (e-work-await-set
                    handles :mode mode :timeout timeout
                    :on-settle
                    (lambda (set-report)
                      (funcall on-done
                               (e-await-tool--report
                                mode (plist-get set-report :reason) pairs)))))))
            (e-tools-request-create :cancel (lambda () (funcall cancel) t))))))
    (error (when on-error (funcall on-error err)) nil)))

(defun e-await-tool--work ()
  "Return the canonical invocation-subscription work for `await'."
  (e-work-spec-create
   :id "tool.await.invocation-subscription"
   :description "Subscribe one board invocation to a bounded set of work."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'await-tool
   :runner
   (lambda (handle arguments context)
     (let ((request
            (e-await-tool--start
             :arguments arguments
             :context context
             :on-done (lambda (value) (e-work-finish handle value))
             :on-error (lambda (err) (e-work-fail handle err)))))
       (when request
         (setf (e-work-handle-cancel-function handle)
               (lambda (_handle)
                 (e-tools-cancel-request request)
                 t))
         (setf (e-work-handle-metadata handle)
               (append (e-work-handle-metadata handle)
                       (list :request request))))
       :deferred))))

(defun e-await-tool-register (registry)
  "Register the model-facing await tool in REGISTRY."
  (e-tools-register
   registry
   :name "await"
   :description "Wait for async work references to settle without blocking Emacs."
   :parameters '(:type "object"
                 :properties
                 (:refs (:type "array"
                         :maxItems 32
                         :items (:type "string"))
                  :mode (:type "string"
                         :enum ["all" "any"])
                  :timeout (:type "number"))
                 :required ["refs"])
   :work (e-await-tool--work)
   :blocking-class 'unknown))

(provide 'e-await-tool)

;;; e-await-tool.el ends here
