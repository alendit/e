;;; e-waitable.el --- Reference-to-work-handle resolver registry for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A small runtime registry mapping a reference scheme to a resolver that
;; returns a live `e-work' handle.  A reference is "SCHEME:LOCAL-ID", e.g.
;; "subagent:sub_000003".  This is the seam that lets a generic await tool wait
;; on any subsystem's work by the ids the model already holds, without the tool
;; knowing about subagents, task-queue tasks, or elisp-jobs.  Each subsystem
;; registers its scheme; the await tool and the coordinator stay
;; scheme-agnostic.
;;
;; The registry is process-global runtime state, like the tool registry: it
;; holds no durable facts and is rebuilt by whichever layers are active.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar e-waitable--resolvers (make-hash-table :test 'equal)
  "Map of reference scheme string to a resolver function.
A resolver is called with a local id string and returns a live `e-work'
handle, or nil when the id is unknown.")

(defun e-waitable-register-resolver (scheme resolver)
  "Register RESOLVER for reference SCHEME.
SCHEME is the string before the colon in a \"SCHEME:LOCAL-ID\" reference.
RESOLVER is called with the local id and returns a live `e-work' handle or nil.
Re-registering a scheme replaces its resolver."
  (unless (stringp scheme)
    (signal 'wrong-type-argument (list 'stringp scheme)))
  (unless (functionp resolver)
    (signal 'wrong-type-argument (list 'functionp resolver)))
  (puthash scheme resolver e-waitable--resolvers)
  scheme)

(defun e-waitable-unregister-resolver (scheme)
  "Remove the resolver registered for SCHEME."
  (remhash scheme e-waitable--resolvers))

(defun e-waitable-schemes ()
  "Return the registered reference schemes, sorted."
  (sort (hash-table-keys e-waitable--resolvers) #'string<))

(defun e-waitable-parse-reference (reference)
  "Split REFERENCE into a (SCHEME . LOCAL-ID) cons, or nil when malformed.
A valid reference is \"SCHEME:LOCAL-ID\" with a non-empty scheme and id."
  (when (stringp reference)
    (when-let ((colon (string-search ":" reference)))
      (let ((scheme (substring reference 0 colon))
            (local-id (substring reference (1+ colon))))
        (unless (or (string-empty-p scheme) (string-empty-p local-id))
          (cons scheme local-id))))))

(defun e-waitable-resolve (reference)
  "Resolve REFERENCE to a live `e-work' handle.
Return a plist (:handle HANDLE) on success, or (:error MESSAGE) describing why
the reference could not be resolved: a malformed reference, an unknown scheme,
or a scheme whose resolver returned nil for the local id.  Resolution never
signals; a bad reference is data the caller reports per-reference."
  (let ((parsed (e-waitable-parse-reference reference)))
    (cond
     ((null parsed)
      (list :error
            (format "malformed reference %S; use SCHEME:LOCAL-ID with a known scheme (%s)"
                    reference (string-join (e-waitable-schemes) ", "))))
     (t
      (let* ((scheme (car parsed))
             (local-id (cdr parsed))
             (resolver (gethash scheme e-waitable--resolvers)))
        (cond
         ((null resolver)
          (list :error
                (format "unknown reference scheme %S; known schemes: %s"
                        scheme (string-join (e-waitable-schemes) ", "))))
         (t
          (let ((handle (funcall resolver local-id)))
            (if handle
                (list :handle handle)
              (list :error
                    (format "unknown %s id %S" scheme local-id)))))))))))

(provide 'e-waitable)

;;; e-waitable.el ends here
