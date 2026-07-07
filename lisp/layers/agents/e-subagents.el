;;; e-subagents.el --- Subagent types context and catalog for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Subagents are a capability composed over existing primitives: harness
;; instances, the task queue, `e-work', and the read-only session:// scheme.
;; This module owns the discovery surface: a context provider that lists
;; spawnable subagent types by visibility and a read-only e:// catalog of the
;; full type list.  Spawning, lineage, and lifecycle live in later modules.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-context)
(require 'e-harness-instances)
(require 'e-layers)
(require 'e-store)

(defconst e-subagents-instructions
  "Subagents let this session delegate work to child sessions on purpose-built harness types, keeping the child's whole transcript out of this context. Read e://subagents/refs/types.md for the full catalog of spawnable types."
  "Compact model-facing instructions for the subagents capability.")

(defun e-subagents--type-line (instance)
  "Return the one-line context entry for subagent INSTANCE."
  (let ((id (e-harness-instance-id instance))
        (name (e-harness-instance-name instance))
        (description (e-harness-instance-description instance)))
    (if (and (stringp description) (not (string-empty-p description)))
        (format "- %s -- %s -- %s" id name description)
      (format "- %s -- %s" id name))))

(defun e-subagents--context-block ()
  "Return the subagents context block, or nil when no types are visible.
Only spawnable instances with `always' context-visibility appear; `hidden'
types stay out of the default context and are read on demand from
e://subagents/refs/types.md."
  (when-let ((instances (e-harness-instance-list-subagents :visibility 'always)))
    (string-join
     (cons
      "Subagent types available to spawn (id -- name -- when to use). Read e://subagents/refs/types.md for the full catalog."
      (mapcar #'e-subagents--type-line instances))
     "\n")))

(defun e-subagents--context-messages ()
  "Return context messages describing spawnable subagent types."
  (when-let ((block (e-subagents--context-block)))
    (list (list :role 'system :content block))))

(defun e-subagents-types-provider ()
  "Return a context provider listing spawnable subagent types by visibility."
  (e-context-provider-create
   :name 'subagents-types
   :priority 210
   :build (cl-function
           (lambda (&key harness session-id turn-id context-purpose)
             (ignore harness session-id turn-id context-purpose)
             (e-subagents--context-messages)))))

(defun e-subagents--catalog-entry (instance)
  "Return the full-catalog markdown entry for subagent INSTANCE."
  (let ((id (e-harness-instance-id instance))
        (name (e-harness-instance-name instance))
        (kind (e-harness-instance-kind instance))
        (visibility (e-harness-instance-context-visibility instance))
        (description (e-harness-instance-description instance)))
    (string-join
     (list (format "## %s" id)
           (format "- name: %s" name)
           (format "- kind: %s" kind)
           (format "- context-visibility: %s" visibility)
           (format "- when to use: %s"
                   (if (and (stringp description)
                            (not (string-empty-p description)))
                       description
                     "(no description)")))
     "\n")))

(defun e-subagents--catalog-markdown ()
  "Return the full markdown catalog of spawnable subagent types."
  (let ((instances (e-harness-instance-list-subagents)))
    (if instances
        (string-join
         (cons "# Subagent types"
               (mapcar #'e-subagents--catalog-entry instances))
         "\n\n")
      (string-join
       '("# Subagent types"
         "No spawnable subagent types are registered.")
       "\n\n"))))

(defun e-subagents--register-reference-resources (store capability)
  "Register subagents reference resources for CAPABILITY in STORE."
  (e-store-register
   store
   (e-capability-id capability)
   "refs/types.md"
   :description "Full catalog of spawnable subagent types."
   :reader (lambda (_entry _range) (e-subagents--catalog-markdown))))

(defun e-subagents-capability-create ()
  "Create the subagents capability.
Slice 1 contributes only the discovery surface: the types context provider and
the read-only type catalog.  Spawn/observe/steer actions arrive in later
slices."
  (e-capability-create
   :id 'subagents
   :name "Subagents"
   :instruction-priority 235
   :instructions e-subagents-instructions
   :context-providers (list (e-subagents-types-provider))
   :resources (list #'e-subagents--register-reference-resources)))

(defun e-subagents-layer-create ()
  "Create the subagents layer."
  (e-layer-create
   :id 'subagents
   :name "Subagents"
   :capabilities (list (e-subagents-capability-create))))

(provide 'e-subagents)

;;; e-subagents.el ends here
