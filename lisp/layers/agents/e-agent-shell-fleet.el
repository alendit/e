;;; e-agent-shell-fleet.el --- Agent Shell Fleet capability for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capability-owned Agent Shell worker coordination.  The harness remains
;; unaware of Agent Shell buffers, events, and lifecycle details.

;;; Code:

(require 'cl-lib)
(require 'e-agent-shell)
(require 'e-agent-shell-work)
(require 'e-capabilities)
(require 'e-layers)
(require 'e-skills)

(defconst e-agent-shell-fleet-instructions
  "Use Agent Shell Fleet actions to hand off work to real Agent Shell worker buffers, list tracked work, inspect results, send follow-up prompts, adopt manual shells, and interrupt active work. Read e://agent-shell-fleet/skills/agent-shell-work for the action contract."
  "Compact Agent Shell Fleet coordinator guidance.")

(defconst e-agent-shell-fleet-skill
  (string-join
   '("# Agent Shell Fleet work actions"
     ""
     "Agent Shell Fleet coordinates real Agent Shell buffers through normalized work records. The e harness does not know about Agent Shell."
     ""
     "## Actions"
     ""
     "- `handoff-work`: input `(:prompt STRING :project-root STRING :agent-id STRING)`. Starts a real Agent Shell buffer, subscribes to events, submits the prompt, and returns a normalized work record."
     "- `adopt-work`: input `(:buffer STRING)`. Adopts an existing manual Agent Shell buffer when project, config, and session metadata are available."
     "- `list-work`: returns compact normalized records for tracked work."
     "- `work-status`: input `(:work-id STRING)`. Returns one normalized work record."
     "- `read-work`: input `(:work-id STRING :limit INTEGER)`. Returns a bounded transcript excerpt."
     "- `send-followup`: input `(:work-id STRING :prompt STRING)`. Sends an additional prompt to the real worker buffer."
     "- `interrupt-work`: input `(:work-id STRING :force BOOLEAN)`. Interrupts the tracked worker and marks the work interrupted."
     ""
     "## Agent Shell source seams"
     ""
     "The adapter isolates `agent-shell--start`, `agent-shell-status`, `agent-shell-insert`, `agent-shell-subscribe-to`, `agent-shell-interrupt`, and `agent-shell--transcript-file`. Audit `lisp/core/e-agent-shell.el` when Agent Shell changes.")
   "\n")
  "Detailed Agent Shell Fleet action reference.")

(defvar e-agent-shell-fleet-default-registry
  (e-agent-shell-work-registry-create)
  "Default in-memory Agent Shell work registry.")

(defun e-agent-shell-fleet--argument-string (arguments key)
  "Return required string argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp key)))
    value))

(defun e-agent-shell-fleet--work-id (arguments)
  "Return required work id from ARGUMENTS."
  (e-agent-shell-fleet--argument-string arguments :work-id))

(defun e-agent-shell-fleet--subscribe (registry work-id buffer)
  "Subscribe REGISTRY WORK-ID to events from BUFFER."
  (let ((subscription
         (e-agent-shell-subscribe
          buffer
          (lambda (event)
            (e-agent-shell-work-update-from-event registry work-id event)))))
    (e-agent-shell-work-set-subscription registry work-id subscription)
    subscription))

(defun e-agent-shell-fleet--refresh-work (registry work-id)
  "Refresh WORK-ID from the live Agent Shell buffer when possible."
  (let ((record (e-agent-shell-work-get registry work-id)))
    (if (memq (plist-get record :status) '(finished failed interrupted))
        record
      (let ((buffer (ignore-errors
                      (e-agent-shell-work-buffer registry work-id))))
        (if buffer
            (e-agent-shell-work-set-status
             registry work-id (e-agent-shell-status buffer))
          record)))))

(defun e-agent-shell-fleet--handoff-work (registry arguments)
  "Start and submit Agent Shell work described by ARGUMENTS."
  (let* ((prompt (e-agent-shell-fleet--argument-string arguments :prompt))
         (project-root (plist-get arguments :project-root))
         (agent-id (plist-get arguments :agent-id))
         (buffer (e-agent-shell-start-worker
                  :project-root project-root
                  :agent-id agent-id
                  :background (plist-get arguments :background)
                  :no-focus (plist-get arguments :no-focus)
                  :session-strategy (plist-get arguments :session-strategy)))
         (record (e-agent-shell-work-create
                  registry
                  :shell-buffer buffer
                  :agent-id agent-id
                  :project-root project-root
                  :agent-session-id (plist-get arguments :agent-session-id)
                  :transcript-file (e-agent-shell-transcript-file buffer)
                  :origin 'e-created
                  :prompt prompt))
         (work-id (plist-get record :work-id)))
    (e-agent-shell-fleet--subscribe registry work-id buffer)
    (e-agent-shell-send-prompt buffer prompt)
    (e-agent-shell-work-update-from-event
     registry work-id '(:type input-submitted))))

(defun e-agent-shell-fleet--adopt-work (registry arguments)
  "Adopt an existing Agent Shell buffer from ARGUMENTS."
  (let* ((buffer-name (e-agent-shell-fleet--argument-string arguments :buffer))
         (buffer (or (get-buffer buffer-name)
                     (user-error "No buffer named %s" buffer-name)))
         (metadata (e-agent-shell-adopt-buffer buffer))
         (record (e-agent-shell-work-create
                  registry
                  :shell-buffer (plist-get metadata :shell-buffer)
                  :agent-id (plist-get metadata :agent-id)
                  :project-root (plist-get metadata :project-root)
                  :agent-session-id (plist-get metadata :agent-session-id)
                  :transcript-file (plist-get metadata :transcript-file)
                  :origin 'adopted
                  :status (e-agent-shell-status buffer)))
         (work-id (plist-get record :work-id)))
    (e-agent-shell-fleet--subscribe registry work-id buffer)
    (e-agent-shell-work-get registry work-id)))

(defun e-agent-shell-fleet--list-work (registry _arguments)
  "Return tracked work records from REGISTRY."
  (mapcar (lambda (work-id)
            (e-agent-shell-fleet--refresh-work registry work-id))
          (e-agent-shell-work-ids registry)))

(defun e-agent-shell-fleet--work-status (registry arguments)
  "Return one work record from REGISTRY."
  (e-agent-shell-fleet--refresh-work
   registry (e-agent-shell-fleet--work-id arguments)))

(defun e-agent-shell-fleet--read-work (registry arguments)
  "Return a bounded transcript excerpt for work in REGISTRY."
  (let* ((work-id (e-agent-shell-fleet--work-id arguments))
         (record (e-agent-shell-work-get registry work-id))
         (file (plist-get record :transcript-file))
         (limit (or (plist-get arguments :limit) 8000))
         (excerpt (e-agent-shell-read-transcript-excerpt file limit)))
    (append (list :work-id work-id) excerpt)))

(defun e-agent-shell-fleet--send-followup (registry arguments)
  "Send follow-up prompt to work in REGISTRY."
  (let* ((work-id (e-agent-shell-fleet--work-id arguments))
         (prompt (e-agent-shell-fleet--argument-string arguments :prompt))
         (buffer (e-agent-shell-work-buffer registry work-id)))
    (e-agent-shell-send-prompt buffer prompt)
    (e-agent-shell-work-update-from-event
     registry work-id '(:type input-submitted))))

(defun e-agent-shell-fleet--interrupt-work (registry arguments)
  "Interrupt work in REGISTRY."
  (let* ((work-id (e-agent-shell-fleet--work-id arguments))
         (buffer (e-agent-shell-work-buffer registry work-id)))
    (e-agent-shell-interrupt buffer :force (plist-get arguments :force))
    (e-agent-shell-work-mark-interrupted registry work-id)))

(cl-defun e-capability-with-agent-shell-create
    (&key (id 'agent-shell-fleet) (name "Agent Shell Fleet") registry)
  "Create the Agent Shell Fleet capability.
REGISTRY defaults to `e-agent-shell-fleet-default-registry'."
  (let ((registry (or registry e-agent-shell-fleet-default-registry)))
    (e-capability-with-skills-create
     :id id
     :name name
     :instruction-priority 260
     :instructions e-agent-shell-fleet-instructions
     :actions
     (list :handoff-work
           (lambda (arguments)
             (e-agent-shell-fleet--handoff-work registry arguments))
           :adopt-work
           (lambda (arguments)
             (e-agent-shell-fleet--adopt-work registry arguments))
           :list-work
           (lambda (arguments)
             (e-agent-shell-fleet--list-work registry arguments))
           :work-status
           (lambda (arguments)
             (e-agent-shell-fleet--work-status registry arguments))
           :read-work
           (lambda (arguments)
             (e-agent-shell-fleet--read-work registry arguments))
           :send-followup
           (lambda (arguments)
             (e-agent-shell-fleet--send-followup registry arguments))
           :interrupt-work
           (lambda (arguments)
             (e-agent-shell-fleet--interrupt-work registry arguments)))
     :skills
     (list
      (e-skill-spec-create
       :name "agent-shell-work"
       :description "Coordinate Agent Shell work through normalized records."
       :content e-agent-shell-fleet-skill)))))

(defun e-agent-shell-fleet-layer-create ()
  "Create the Agent Shell Fleet layer."
  (e-layer-create
   :id 'agent-shell-fleet
   :name "Agent Shell Fleet"
   :capabilities (list (e-capability-with-agent-shell-create))))

(provide 'e-agent-shell-fleet)

;;; e-agent-shell-fleet.el ends here
