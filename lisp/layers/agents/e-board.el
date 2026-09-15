;;; e-board.el --- Independent Board observation capability for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The Board capability is the consumer-facing surface for durable participant
;; and run observation.  It is intentionally separate from `subagents': child
;; spawning and live controls may be absent while committed Board facts remain
;; queryable.  The small run-set registry below retains only the current,
;; bounded detached value for one chat session; SQLite remains authoritative.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-board-observation)
(require 'e-board-orchestration)
(require 'e-board-orchestration-actions)
(require 'e-context)
(require 'e-layers)
(require 'e-skills)

(defconst e-board-instructions
  "Board is the durable observation surface for the current chat's Board. Use `list', `status', and `read' for participant activity and bounded durable transcripts; use `list-runs' and `run-status' for orchestration. These actions read committed SQLite facts and never classify a row from private live execution state. Read e://board/skills/board for the action contract."
  "Compact model-facing guidance for the Board capability.")

(defconst e-board-skill
  (string-join
   '("# Board observation actions"
     ""
     "Board actions read one bounded detached SQL result at a time. They remain available to ordinary owner chats even when child supervision is disabled."
     ""
     "## Participant activity"
     ""
     "- `list`: returns one ordered mixed participant/activity page. Use its opaque cursor for the next page."
     "- `status`: input `(:participant-id STRING)`, returning the exact durable participant outcome."
     "- `read`: input `(:participant-id STRING :raw BOOLEAN :limit INTEGER)`. Ordinary reads use the durable projection; `:raw` requests a bounded durable session transcript page."
     ""
     "## Runs"
     ""
     "- `list-runs`: returns the bounded active run-set for this Board. Entries are ordered by attention/actionability and latest durable event."
     "- `run-status`: input `(:run-id STRING)`, returning one bounded durable run projection."
     ""
     "Committed Board facts are authoritative. Missing or stale process-local handles only make live controls unavailable; they never hide, reorder, reject, or mutate a durable row."
     "")
   "\n")
  "Detailed Board capability action reference.")

(defvar e-board--run-set-states (make-hash-table :test 'equal)
  "Current bounded run-set states keyed by harness and session id.")

(defun e-board--run-set-key (harness session-id)
  "Return the process-local key for HARNESS and SESSION-ID."
  (list harness (copy-sequence session-id)))

(defun e-board-run-set-state-for (harness session-id &optional board-id)
  "Return the bounded run-set state for HARNESS and SESSION-ID.
When no state exists, create a restoring state.  BOARD-ID is retained only in
the detached value and may be supplied by the caller that knows the binding."
  (let* ((key (e-board--run-set-key harness session-id))
         (state (gethash key e-board--run-set-states)))
    (or state
        (setq state
              (puthash key
                       (e-board-orchestration-run-set-state-create
                        :board-id board-id)
                       e-board--run-set-states)))
    (when (and board-id
               (null (e-board-orchestration-run-set-state-board-id state)))
      (setf (e-board-orchestration-run-set-state-board-id state)
            (copy-sequence board-id)))
    state))

(defun e-board-run-set-update (harness session-id projections &rest options)
  "Update one session-scoped Board run-set value from PROJECTIONS.
The projections are detached values supplied by the Board SQL application
service; this function does not consult private live state."
  (apply #'e-board-orchestration-run-set-state-update
         (e-board-run-set-state-for
          harness session-id
          (and (e-chat-service-binding harness session-id)
               (e-chat-service-binding-board-id
                (e-chat-service-binding harness session-id))))
         projections options))

(defun e-board-run-set-context-provider (&optional states)
  "Return a context provider reading bounded Board run-set STATES.
STATES may be a function of HARNESS and SESSION-ID; by default the provider
uses `e-board-run-set-state-for' and emits restoring until an application
query installs the first durable value."
  (e-context-provider-create
   :name 'board-run-set
   :priority 215
   :build
   (cl-function
    (lambda (&key harness session-id turn-id context-purpose)
      (ignore turn-id context-purpose)
      (when (and harness session-id)
        (let* ((binding (e-chat-service-binding harness session-id))
               (board-id (and binding
                              (e-chat-service-binding-board-id binding)))
               (state (if (functionp states)
                          (funcall states harness session-id)
                        (e-board-run-set-state-for harness session-id board-id)))
               (context (e-board-orchestration-run-set-context state)))
          (list
           (list :role 'system
                 :content
                 (if (plist-get context :ready-p)
                     (format "Board run-set ready: %d active run(s), %d omitted."
                             (plist-get context :active-count)
                             (plist-get context :omitted-count))
                   "Board run-set restoring; wait for its bounded durable projection before the first model turn.")))))))))

(defun e-board-capability-create (&optional states)
  "Create the independent Board observation capability."
  (e-capability-with-skills-create
   :id 'board
   :name "Board"
   :instruction-priority 225
   :instructions e-board-instructions
   :context-providers (list (e-board-run-set-context-provider states))
   :actions (append (e-board-observation-parent-alist)
                    (e-board-orchestration-actions-parent-alist t))
   :skills (list
            (e-skill-spec-create
             :name "board"
             :description "Read durable Board participant activity and run projections."
             :content e-board-skill))))

(defun e-board-layer-create ()
  "Create the independent Board observation layer."
  (e-layer-create
   :id 'board
   :name "Board"
   :requires '(async-control)
   :capabilities (list (e-board-capability-create))))

(provide 'e-board)

;;; e-board.el ends here
