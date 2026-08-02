;;; e-harness-instances.el --- User-facing harness instance catalog -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; User-facing catalog of configured runtime targets.  Live harness objects and
;; lazy factories remain owned by `e-harness-registry'; this module adds the
;; selection metadata shells need to present those targets uniformly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-harness-registry)

(define-error 'e-harness-instance-missing
  "No harness instance is registered for id")
(define-error 'e-harness-instance-store-conflict
  "Harness instances sharing a session store disagree on its ports")

(cl-defstruct e-harness-instance
  id
  name
  kind
  factory
  harness-id
  metadata
  default-p
  description
  (context-visibility 'always)
  subagent-p
  layers
  layer-config
  session-store-id
  session-catalog
  session-access-store)

(defvar e-harness-instance--instances (make-hash-table :test 'equal)
  "Harness instance records keyed by instance id.")

(defvar e-harness-instance--defaults (make-hash-table :test 'equal)
  "Default harness instance ids keyed by kind.")

(defvar e-harness-instance--session-stores (make-hash-table :test 'equal)
  "Configured session-store port records keyed by stable store id.")

(defvar e-harness-instance--generation 0
  "Monotonic generation of the configured harness-instance catalog.")

(defun e-harness-instance--validate-id (id)
  "Signal when ID is not a valid harness instance id."
  (unless (keywordp id)
    (signal 'wrong-type-argument (list 'keywordp id))))

(defun e-harness-instance--validate-kind (kind)
  "Signal when KIND is not a valid harness instance kind."
  (unless (symbolp kind)
    (signal 'wrong-type-argument (list 'symbolp kind))))

(defun e-harness-instance--validate-context-visibility (visibility)
  "Signal when VISIBILITY is not a valid context-visibility value."
  (unless (memq visibility '(always hidden))
    (signal 'wrong-type-argument
            (list '(member always hidden) visibility))))

(defun e-harness-instance--validate-session-port (name port)
  "Signal unless optional session NAME PORT is a callable application port."
  (when (and port (not (functionp port)))
    (signal 'wrong-type-argument (list 'functionp port))))

(defun e-harness-instance--validate-shared-store-ports
    (id session-store-id session-catalog session-access-store)
  "Require one stable pair of ports for every SESSION-STORE-ID.
ID's replacement registration is excluded so an instance can update its own
metadata without comparing against its retired record."
  (when-let ((entry (and session-store-id
                         (gethash session-store-id
                                  e-harness-instance--session-stores))))
    (when (and (seq-some (lambda (other-id) (not (eq other-id id)))
                         (plist-get entry :eligible-instance-ids))
               (not (and (eq (plist-get entry :session-catalog)
                             session-catalog)
                         (eq (plist-get entry :session-access-store)
                             session-access-store))))
      (signal 'e-harness-instance-store-conflict
              (list session-store-id id 'shared-store)))))

(defun e-harness-instance--unindex-session-store (instance)
  "Remove INSTANCE from its configured session-store eligibility index."
  (when-let* ((store-id (e-harness-instance-session-store-id instance))
              (entry (gethash store-id e-harness-instance--session-stores)))
    (let ((eligible (delq (e-harness-instance-id instance)
                          (copy-sequence
                           (plist-get entry :eligible-instance-ids)))))
      (if eligible
          (plist-put entry :eligible-instance-ids eligible)
        (remhash store-id e-harness-instance--session-stores)))))

(defun e-harness-instance--index-session-store (instance)
  "Add INSTANCE to its configured session-store eligibility index."
  (when-let ((store-id (e-harness-instance-session-store-id instance)))
    (let ((entry (gethash store-id e-harness-instance--session-stores)))
      (if entry
          (cl-pushnew (e-harness-instance-id instance)
                      (plist-get entry :eligible-instance-ids)
                      :test #'eq)
        (puthash store-id
                 (list :session-store-id store-id
                       :session-catalog
                       (e-harness-instance-session-catalog instance)
                       :session-access-store
                       (e-harness-instance-session-access-store instance)
                       :eligible-instance-ids
                       (list (e-harness-instance-id instance)))
                 e-harness-instance--session-stores)))))

(defun e-harness-instance--display-name (id name)
  "Return normalized display NAME for ID."
  (cond
   ((and (stringp name) (not (string-empty-p name))) name)
   ((keywordp id) (string-remove-prefix ":" (symbol-name id)))
   (t (format "%s" id))))

;;;###autoload
(cl-defun e-harness-instance-register
    (&key id name kind factory harness-id metadata default
          description (context-visibility 'always) subagent
          layers layer-config session-store-id session-catalog session-access-store)
  "Register a configured harness instance.
ID is the stable user-facing target id.  KIND identifies the role the
instance plays, such as `chat' or `reviewer'.  FACTORY, when non-nil, is
registered with `e-harness-registry' under HARNESS-ID or ID.  METADATA is
presentation data.  When DEFAULT is non-nil, make this instance the default
for KIND.

DESCRIPTION is free-text \"when to use this agent\" routing guidance.
CONTEXT-VISIBILITY is `always' or `hidden' and controls whether the instance
appears in the subagents context block.  When SUBAGENT is non-nil, the
instance is spawnable as a subagent type; this eligibility flag is kept
separate from KIND so role instances stay reusable across chat and subagent
use.

LAYERS, when non-nil, is the instance's declared enabled layer id list; the
subagent runner applies it as the child harness's minimal layer set.
LAYER-CONFIG, when non-nil, is an alist mapping a capability id to its option
plist, applied as that instance's initial runtime capability config.  Both are
declarative selection metadata; the factory still builds the live harness."
  (e-harness-instance--validate-id id)
  (e-harness-instance--validate-kind kind)
  (e-harness-instance--validate-context-visibility context-visibility)
  (when (and session-store-id (not (stringp session-store-id)))
    (signal 'wrong-type-argument (list 'stringp session-store-id)))
  (e-harness-instance--validate-session-port 'session-catalog session-catalog)
  (e-harness-instance--validate-session-port 'session-access-store session-access-store)
  (when (and session-store-id (not (and session-catalog session-access-store)))
    (signal 'e-harness-instance-store-conflict
            (list session-store-id 'missing-required-port)))
  (e-harness-instance--validate-shared-store-ports
   id session-store-id session-catalog session-access-store)
  (let ((harness-id (or harness-id id))
        (previous (gethash id e-harness-instance--instances)))
    (e-harness-instance--validate-id harness-id)
    (when factory
      (e-harness-registry-register-factory harness-id factory))
    (let ((instance (make-e-harness-instance
                     :id id
                     :name (e-harness-instance--display-name id name)
                     :kind kind
                     :factory factory
                     :harness-id harness-id
                     :metadata metadata
                     :default-p default
                     :description description
                     :context-visibility context-visibility
                     :subagent-p subagent
                     :layers layers
                     :layer-config layer-config
                     :session-store-id session-store-id
                     :session-catalog session-catalog
                     :session-access-store session-access-store)))
      (when previous
        (e-harness-instance--unindex-session-store previous))
      (puthash id instance e-harness-instance--instances)
      (e-harness-instance--index-session-store instance)
      (cl-incf e-harness-instance--generation)
      (when (or default
                (not (gethash kind e-harness-instance--defaults)))
        (puthash kind id e-harness-instance--defaults))
      instance)))

(defun e-harness-instance-get (id)
  "Return registered harness instance ID, or nil."
  (e-harness-instance--validate-id id)
  (gethash id e-harness-instance--instances))

(cl-defun e-harness-instance-list (&key kind)
  "Return registered harness instances, optionally filtered by KIND."
  (when kind
    (e-harness-instance--validate-kind kind))
  (let (instances)
    (maphash
     (lambda (_id instance)
       (when (or (not kind)
                 (eq (e-harness-instance-kind instance) kind))
         (push instance instances)))
     e-harness-instance--instances)
    (sort instances
          (lambda (left right)
            (string< (symbol-name (e-harness-instance-id left))
                     (symbol-name (e-harness-instance-id right)))))))

(defun e-harness-instance-session-stores ()
  "Return deduplicated configured store metadata without activating harnesses."
  (let (result)
    (maphash
     (lambda (_store-id indexed-entry)
       (let ((entry (copy-tree indexed-entry)))
         (plist-put entry :eligible-instance-ids
                    (sort (plist-get entry :eligible-instance-ids)
                          (lambda (left right)
                            (string< (symbol-name left) (symbol-name right)))))
         (push entry result)))
     e-harness-instance--session-stores)
    (sort result (lambda (left right)
                   (string< (plist-get left :session-store-id)
                            (plist-get right :session-store-id))))))

(cl-defun e-harness-instance-list-subagents (&key visibility)
  "Return spawnable subagent instances, optionally filtered by VISIBILITY.
VISIBILITY, when non-nil, is `always' or `hidden'."
  (when visibility
    (e-harness-instance--validate-context-visibility visibility))
  (seq-filter
   (lambda (instance)
     (and (e-harness-instance-subagent-p instance)
          (or (not visibility)
              (eq (e-harness-instance-context-visibility instance)
                  visibility))))
   (e-harness-instance-list)))

(cl-defun e-harness-instance-default (&key kind)
  "Return the default harness instance for KIND, or nil."
  (when kind
    (e-harness-instance--validate-kind kind))
  (or (when kind
        (when-let ((id (gethash kind e-harness-instance--defaults)))
          (let ((instance (e-harness-instance-get id)))
            (and instance
                 (eq (e-harness-instance-kind instance) kind)
                 instance))))
      (car (e-harness-instance-list :kind kind))))

(defun e-harness-instance-get-or-create (id)
  "Return the live harness for harness instance ID, creating it lazily."
  (e-harness-instance--validate-id id)
  (let ((instance (or (e-harness-instance-get id)
                      (signal 'e-harness-instance-missing (list id)))))
    (e-harness-registry-get-or-create
     (e-harness-instance-harness-id instance))))

(provide 'e-harness-instances)

;;; e-harness-instances.el ends here
