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
A requested timeout above this is clamped so a runaway wait still ends."
  :type 'number
  :group 'e)

(defun e-await-tool--effective-timeout (arguments)
  "Return the clamped await timeout in seconds from ARGUMENTS."
  (let ((requested (plist-get arguments :timeout)))
    (min e-await-tool-max-timeout
         (if (and (numberp requested) (> requested 0))
             requested
           e-await-tool-default-timeout))))

(defun e-await-tool--normalize-mode (arguments)
  "Return the await mode symbol (`all' or `any') from ARGUMENTS."
  (pcase (plist-get arguments :mode)
    ((or 'nil "all" "" 'all) 'all)
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
  "Resolve REFS into resolved handles and per-reference errors.
Return a plist (:pairs PAIRS :errors ERRORS) where PAIRS is a list of
\(REFERENCE . HANDLE) for resolvable references and ERRORS is a list of
\(:ref REFERENCE :status error :error MESSAGE) for the rest.  An unresolvable
reference is a per-reference error, never a whole-call failure, so an await over
a mix still waits on the resolvable references."
  (let (pairs errors)
    (dolist (ref (append refs nil))
      (let ((resolved (e-waitable-resolve ref)))
        (if-let ((handle (plist-get resolved :handle)))
            (push (cons ref handle) pairs)
          (push (list :ref ref :status 'error
                      :error (plist-get resolved :error))
                errors))))
    (list :pairs (nreverse pairs) :errors (nreverse errors))))

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

(defun e-await-tool--report (mode reason pairs errors)
  "Return the compact await report for MODE, REASON, PAIRS, and ERRORS."
  (list :settled (eq reason 'complete)
        :reason reason
        :mode mode
        :results
        (append
         (mapcar (lambda (pair)
                   (e-await-tool--result-entry (car pair) (cdr pair)))
                 pairs)
         errors)))

(defun e-await-tool--work ()
  "Return the cooperative work spec for the await tool."
  (e-work-spec-create
   :id "await"
   :description "Wait for referenced work to settle or time out."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'await
   :runner
   (lambda (handle arguments _context)
     (let* ((refs (plist-get arguments :refs))
            (mode (e-await-tool--normalize-mode arguments))
            (timeout (e-await-tool--effective-timeout arguments))
            (resolved (e-await-tool--resolve-references refs))
            (pairs (plist-get resolved :pairs))
            (errors (plist-get resolved :errors))
            (handles (mapcar #'cdr pairs)))
       (cond
        ((null refs)
         (e-work-finish handle (e-await-tool--report mode 'complete nil errors)))
        ;; Nothing resolvable: settle now with the per-reference errors rather
        ;; than waiting on an empty set.
        ((null handles)
         (e-work-finish handle (e-await-tool--report mode 'complete nil errors)))
        (t
         (let ((cancel
                (e-work-await-set
                 handles
                 :mode mode
                 :timeout timeout
                 :on-settle
                 (lambda (set-report)
                   (e-work-finish
                    handle
                    (e-await-tool--report
                     mode (plist-get set-report :reason) pairs errors))))))
           ;; Detach the set wait if the tool call itself is cancelled.
           (setf (e-work-handle-cancel-function handle)
                 (lambda (_handle) (funcall cancel) t)))))
       :deferred))))

(defun e-await-tool-register (registry)
  "Register the model-facing await tool in REGISTRY."
  (e-tools-register
   registry
   :name "await"
   :description
   (concat
    "Wait until referenced async work settles, or a generous timeout expires, "
    "then return a compact per-reference report. Use this instead of polling "
    "status across turns: the turn holds open while the wait runs event-driven, "
    "without freezing Emacs. Reference work by \"SCHEME:LOCAL-ID\", e.g. "
    "\"subagent:sub_000003\". mode \"all\" (default) settles when every "
    "resolvable reference is terminal; mode \"any\" settles at the first. On "
    "timeout the report has :settled nil, :reason timed-out, and lists pending "
    "references so you can await again or move on. An unknown reference is a "
    "per-reference error in the report, not a whole-call failure. The report "
    "gives each reference's terminal status, summary, and outputs; pull deeper "
    "detail from the target subsystem (e.g. a subagent's session:// transcript) "
    "on demand.")
   :parameters '(:type "object"
                 :properties
                 (:refs (:type "array"
                         :items (:type "string")
                         :description "Work references as SCHEME:LOCAL-ID.")
                  :mode (:type "string"
                         :enum ["all" "any"]
                         :description "Settle when all (default) or any reference is terminal.")
                  :timeout (:type "number"
                            :description "Seconds to wait before a timed-out report; clamped to a ceiling."))
                 :required ["refs"])
   :work (e-await-tool--work)
   :blocking-class 'unknown))

(provide 'e-await-tool)

;;; e-await-tool.el ends here
