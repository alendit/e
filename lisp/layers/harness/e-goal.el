;;; e-goal.el --- Goal process capability for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Harness-owned goal orchestration guidance and deterministic actions.  The
;; capability models a controller loop that can run in the main agent or hand a
;; step to a separate executor supplied by another capability.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-store)
(require 'subr-x)

(defconst e-goal-instructions
  "Use the Goal capability when work should proceed through a durable controller loop: define the goal, ask for the next action, execute or delegate that action, review the result, record evidence, and stop only when the goal's achievement predicate is true. Read e://goal/refs/process.md before applying the process."
  "Compact model-facing guidance for goal-driven work.")

(defconst e-goal-process-reference
  (string-join
   '("# Goal process"
     ""
     "The Goal capability manages context and complexity for multi-step work."
     "It does not require subagents."
     "A main agent can run each step itself, then ask for the next action."
     "If a separate subagent capability is available and the user asks to use it, the next action prompt can be handed to that subagent instead."
     ""
     "## Controller loop"
     ""
     "1. Define the goal with an objective, optional reference URI, ordered steps, and success criteria."
     "2. Ask for the next action."
     "3. Execute that action in the main agent, or hand its prompt to a separate executor."
     "4. Review the result before marking the step done."
     "5. Record the step result, evidence, and any blockers."
     "6. Assess whether the goal is achieved."
     "7. Continue with the next action while the goal is active."
     "8. Stop when the assessment reports achieved, blocked, or stopped."
     ""
     "## Achievement predicate"
     ""
     "A goal is achieved when all required steps are done or explicitly skipped, all success criteria are verified, and no unresolved blockers remain."
     "The controller may also mark a goal stopped or blocked, but that is not achievement."
     "The deterministic action `assess-goal` returns this state."
     ""
     "## Step status"
     ""
     "Step statuses are `pending`, `in-progress`, `done`, `skipped`, and `blocked`."
     "Only `done` and explicit `skipped` satisfy a required step."
     "A blocked step prevents achievement until the blocker is resolved and the step is completed or skipped."
     ""
     "## Success criteria"
     ""
     "Criteria are verified independently from step completion."
     "This keeps the controller from confusing activity with success."
     "Verification evidence should name tests, reviews, live probes, commits, or user decisions."
     ""
     "## Prompts"
     ""
     "The `next-action` action returns a narrow prompt for the next step."
     "The prompt is executor-neutral."
     "It can be run by the main agent or sent to a subagent by a separate capability."
     "The prompt tells the executor to stop after the step and report evidence for controller review."
     ""
     "## Subagents"
     ""
     "Subagent spawning is not part of this capability."
     "A future or separate Subagents capability should own worker creation, split display, callbacks, and transcript reading."
     "Goal state only records the planned action, result, review, and achievement state.")
   "\n")
  "Detailed process reference exposed as e://goal/refs/process.md.")

(cl-defstruct (e-goal-registry
               (:constructor e-goal-registry-create))
  (goals (make-hash-table :test 'equal))
  (sequence 0))

(defvar e-goal-default-registry (e-goal-registry-create)
  "Default in-memory goal registry.")

(defun e-goal--copy (value)
  "Return a mutation-safe copy of VALUE."
  (copy-tree value))

(defun e-goal--argument-string (arguments key &optional default)
  "Return string argument KEY from ARGUMENTS, or DEFAULT."
  (let ((value (plist-get arguments key)))
    (cond
     ((stringp value) value)
     ((and (null value) default) default)
     ((null value) nil)
     (t (signal 'wrong-type-argument (list 'stringp key))))))

(defun e-goal--argument-list (arguments key)
  "Return list argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (cond
     ((null value) nil)
     ((listp value) value)
     (t (signal 'wrong-type-argument (list 'listp key))))))

(defun e-goal--allowed-status-p (status allowed)
  "Return non-nil when STATUS is in ALLOWED."
  (memq status allowed))

(defun e-goal--normalize-status (status allowed default)
  "Return normalized STATUS from ALLOWED, falling back to DEFAULT."
  (let ((status (or status default)))
    (unless (e-goal--allowed-status-p status allowed)
      (signal 'wrong-type-argument (list 'goal-status status)))
    status))

(defun e-goal--next-id (registry prefix)
  "Return the next id in REGISTRY with PREFIX."
  (setf (e-goal-registry-sequence registry)
        (1+ (e-goal-registry-sequence registry)))
  (format "%s-%d" prefix (e-goal-registry-sequence registry)))

(defun e-goal--normalize-step (step index)
  "Return normalized STEP at one-based INDEX."
  (let* ((plist (if (stringp step)
                    (list :title step)
                  step)))
    (unless (and (listp plist) (or (null plist) (keywordp (car plist))))
      (signal 'wrong-type-argument (list 'goal-step step)))
    (let ((id (or (plist-get plist :id)
                  (format "step-%d" index)))
          (title (or (plist-get plist :title)
                     (plist-get plist :description))))
      (unless (stringp id)
        (setq id (format "%s" id)))
      (unless (stringp title)
        (signal 'wrong-type-argument (list 'goal-step-title step)))
      (append (list :id id
                    :title title
                    :status (e-goal--normalize-status
                             (plist-get plist :status)
                             '(pending in-progress done skipped blocked)
                             'pending)
                    :required (if (plist-member plist :required)
                                  (plist-get plist :required)
                                t))
              (cl-loop for (key value) on plist by #'cddr
                       unless (memq key '(:id :title :description :status
                                               :required))
                       append (list key value))))))

(defun e-goal--normalize-criterion (criterion index)
  "Return normalized success CRITERION at one-based INDEX."
  (let* ((plist (if (stringp criterion)
                    (list :title criterion)
                  criterion)))
    (unless (and (listp plist) (or (null plist) (keywordp (car plist))))
      (signal 'wrong-type-argument (list 'goal-criterion criterion)))
    (let ((id (or (plist-get plist :id)
                  (format "criterion-%d" index)))
          (title (or (plist-get plist :title)
                     (plist-get plist :description))))
      (unless (stringp id)
        (setq id (format "%s" id)))
      (unless (stringp title)
        (signal 'wrong-type-argument (list 'goal-criterion-title criterion)))
      (append (list :id id
                    :title title
                    :verified (and (plist-get plist :verified) t)
                    :evidence (plist-get plist :evidence))
              (cl-loop for (key value) on plist by #'cddr
                       unless (memq key '(:id :title :description :verified
                                               :evidence))
                       append (list key value))))))

(defun e-goal--normalize-steps (steps)
  "Return normalized STEPS."
  (cl-loop for step in steps
           for index from 1
           collect (e-goal--normalize-step step index)))

(defun e-goal--normalize-criteria (criteria)
  "Return normalized success CRITERIA."
  (cl-loop for criterion in criteria
           for index from 1
           collect (e-goal--normalize-criterion criterion index)))

(defun e-goal--get (registry goal-id)
  "Return goal GOAL-ID from REGISTRY or signal."
  (let ((goal (gethash goal-id (e-goal-registry-goals registry))))
    (unless goal
      (user-error "No goal named %s" goal-id))
    goal))

(defun e-goal--put (registry goal)
  "Store GOAL in REGISTRY and return GOAL."
  (puthash (plist-get goal :goal-id) goal (e-goal-registry-goals registry))
  goal)

(defun e-goal--find-by-id (items item-id)
  "Return item with ITEM-ID from ITEMS."
  (cl-find item-id items
           :key (lambda (item) (plist-get item :id))
           :test #'equal))

(defun e-goal--replace-by-id (items replacement)
  "Return ITEMS with item matching REPLACEMENT id replaced."
  (let ((id (plist-get replacement :id)))
    (mapcar (lambda (item)
              (if (equal (plist-get item :id) id)
                  replacement
                item))
            items)))

(defun e-goal--required-step-satisfied-p (step)
  "Return non-nil when STEP satisfies required-work completion."
  (or (not (plist-get step :required))
      (memq (plist-get step :status) '(done skipped))))

(defun e-goal--criterion-verified-p (criterion)
  "Return non-nil when CRITERION is verified."
  (and (plist-get criterion :verified) t))

(defun e-goal--blockers (goal)
  "Return unresolved blockers for GOAL."
  (cl-remove-if (lambda (blocker)
                  (plist-get blocker :resolved))
                (plist-get goal :blockers)))

(defun e-goal-achieved-p (goal)
  "Return non-nil when GOAL's deterministic achievement predicate is true."
  (and (cl-every #'e-goal--required-step-satisfied-p
                 (plist-get goal :steps))
       (cl-every #'e-goal--criterion-verified-p
                 (plist-get goal :success-criteria))
       (null (e-goal--blockers goal))))

(defun e-goal--first-actionable-step (goal)
  "Return first pending or in-progress step for GOAL."
  (cl-find-if (lambda (step)
                (memq (plist-get step :status) '(pending in-progress)))
              (plist-get goal :steps)))

(defun e-goal--next-pending-criterion (goal)
  "Return first unverified criterion for GOAL."
  (cl-find-if-not #'e-goal--criterion-verified-p
                  (plist-get goal :success-criteria)))

(defun e-goal--step-prompt (goal step)
  "Return executor-neutral prompt for GOAL STEP."
  (string-join
   (delq nil
         (list
          (format "Continue goal %s: %s."
                  (plist-get goal :goal-id)
                  (plist-get goal :title))
          (when-let ((objective (plist-get goal :objective)))
            (format "Objective: %s" objective))
          (when-let ((reference (plist-get goal :reference-uri)))
            (format "Read the process/reference material as needed: %s" reference))
          (format "Do only this step: %s -- %s."
                  (plist-get step :id)
                  (plist-get step :title))
          "Preserve unrelated local work."
          "Run the checks that prove this step."
          "Update the relevant review or handoff notes."
          "Commit only the coherent slice if code or durable docs changed."
          "Stop after this step and report evidence, changed files, tests, reload status, commit id if any, and blockers."
          "Do not continue to the next step until the controller reviews the result."))
   "\n"))

(defun e-goal--verification-prompt (goal criterion)
  "Return prompt to verify GOAL CRITERION."
  (string-join
   (delq nil
         (list
          (format "Review goal %s: %s."
                  (plist-get goal :goal-id)
                  (plist-get goal :title))
          (when-let ((reference (plist-get goal :reference-uri)))
            (format "Use reference: %s" reference))
          (format "Verify success criterion %s -- %s."
                  (plist-get criterion :id)
                  (plist-get criterion :title))
          "Use concrete evidence such as tests, review findings, live probes, commits, or user acceptance."
          "Record the criterion as verified only if the evidence proves it."
          "If evidence is missing, report the missing work and stop."))
   "\n"))

(defun e-goal--status (goal)
  "Return normalized status plist for GOAL."
  (let ((achieved (e-goal-achieved-p goal))
        (blockers (e-goal--blockers goal)))
    (append
     (e-goal--copy goal)
     (list :achieved achieved
           :unresolved-blockers blockers
           :computed-status (cond
                             (achieved 'achieved)
                             (blockers 'blocked)
                             (t (plist-get goal :status)))))))

(defun e-goal-define (registry arguments)
  "Define a goal in REGISTRY from ARGUMENTS."
  (let* ((goal-id (or (e-goal--argument-string arguments :goal-id)
                      (e-goal--next-id registry "goal")))
         (title (e-goal--argument-string arguments :title "Untitled goal"))
         (objective (e-goal--argument-string arguments :objective))
         (reference-uri (e-goal--argument-string arguments :reference-uri))
         (steps (e-goal--normalize-steps
                 (e-goal--argument-list arguments :steps)))
         (criteria (e-goal--normalize-criteria
                    (e-goal--argument-list arguments :success-criteria)))
         (goal (list :goal-id goal-id
                     :title title
                     :objective objective
                     :reference-uri reference-uri
                     :status 'active
                     :steps steps
                     :success-criteria criteria
                     :blockers nil
                     :history nil)))
    (when (gethash goal-id (e-goal-registry-goals registry))
      (user-error "Goal already exists: %s" goal-id))
    (e-goal--status (e-goal--put registry goal))))

(defun e-goal-list (registry _arguments)
  "Return compact status for every goal in REGISTRY."
  (let (goals)
    (maphash (lambda (_id goal)
               (push (list :goal-id (plist-get goal :goal-id)
                           :title (plist-get goal :title)
                           :status (plist-get (e-goal--status goal)
                                              :computed-status)
                           :achieved (e-goal-achieved-p goal))
                     goals))
             (e-goal-registry-goals registry))
    (sort goals (lambda (left right)
                  (string< (plist-get left :goal-id)
                           (plist-get right :goal-id))))))

(defun e-goal-status (registry arguments)
  "Return goal status for ARGUMENTS in REGISTRY."
  (e-goal--status
   (e-goal--get registry (e-goal--argument-string arguments :goal-id))))

(defun e-goal-next-action (registry arguments)
  "Return next deterministic action prompt for a goal in REGISTRY."
  (let* ((goal (e-goal--get registry
                            (e-goal--argument-string arguments :goal-id)))
         (status (e-goal--status goal))
         (achieved (plist-get status :achieved))
         (blockers (plist-get status :unresolved-blockers))
         (step (e-goal--first-actionable-step goal))
         (criterion (e-goal--next-pending-criterion goal)))
    (cond
     (achieved
      (list :goal-id (plist-get goal :goal-id)
            :status 'achieved
            :achieved t
            :prompt nil
            :message "Goal is achieved; stop."))
     (blockers
      (list :goal-id (plist-get goal :goal-id)
            :status 'blocked
            :achieved nil
            :blockers blockers
            :prompt "Resolve or clear the unresolved blocker before continuing."))
     (step
      (list :goal-id (plist-get goal :goal-id)
            :status 'active
            :achieved nil
            :kind 'step
            :step step
            :prompt (e-goal--step-prompt goal step)))
     (criterion
      (list :goal-id (plist-get goal :goal-id)
            :status 'active
            :achieved nil
            :kind 'criterion
            :criterion criterion
            :prompt (e-goal--verification-prompt goal criterion)))
     (t
      (list :goal-id (plist-get goal :goal-id)
            :status 'stalled
            :achieved nil
            :prompt "No actionable step remains, but the goal is not achieved. Add steps, verify criteria, or stop the goal.")))))

(defun e-goal-record-step (registry arguments)
  "Record a step update in REGISTRY from ARGUMENTS."
  (let* ((goal-id (e-goal--argument-string arguments :goal-id))
         (step-id (e-goal--argument-string arguments :step-id))
         (goal (e-goal--get registry goal-id))
         (step (or (e-goal--find-by-id (plist-get goal :steps) step-id)
                   (user-error "No step %s in goal %s" step-id goal-id)))
         (status (e-goal--normalize-status
                  (plist-get arguments :status)
                  '(pending in-progress done skipped blocked)
                  (plist-get step :status)))
         (updated (copy-sequence step)))
    (plist-put updated :status status)
    (when (plist-member arguments :evidence)
      (plist-put updated :evidence (plist-get arguments :evidence)))
    (when (plist-member arguments :notes)
      (plist-put updated :notes (plist-get arguments :notes)))
    (plist-put goal :steps
               (e-goal--replace-by-id (plist-get goal :steps) updated))
    (push (list :event 'step-recorded
                :step-id step-id
                :status status
                :evidence (plist-get arguments :evidence)
                :notes (plist-get arguments :notes))
          (plist-get goal :history))
    (when (e-goal-achieved-p goal)
      (plist-put goal :status 'achieved))
    (e-goal--status goal)))

(defun e-goal-record-criterion (registry arguments)
  "Record success criterion verification in REGISTRY from ARGUMENTS."
  (let* ((goal-id (e-goal--argument-string arguments :goal-id))
         (criterion-id (e-goal--argument-string arguments :criterion-id))
         (goal (e-goal--get registry goal-id))
         (criterion (or (e-goal--find-by-id
                         (plist-get goal :success-criteria)
                         criterion-id)
                        (user-error "No criterion %s in goal %s"
                                    criterion-id goal-id)))
         (updated (copy-sequence criterion)))
    (plist-put updated :verified (and (plist-get arguments :verified) t))
    (when (plist-member arguments :evidence)
      (plist-put updated :evidence (plist-get arguments :evidence)))
    (when (plist-member arguments :notes)
      (plist-put updated :notes (plist-get arguments :notes)))
    (plist-put goal :success-criteria
               (e-goal--replace-by-id (plist-get goal :success-criteria)
                                      updated))
    (push (list :event 'criterion-recorded
                :criterion-id criterion-id
                :verified (plist-get updated :verified)
                :evidence (plist-get arguments :evidence)
                :notes (plist-get arguments :notes))
          (plist-get goal :history))
    (when (e-goal-achieved-p goal)
      (plist-put goal :status 'achieved))
    (e-goal--status goal)))

(defun e-goal-record-blocker (registry arguments)
  "Record an unresolved blocker in REGISTRY from ARGUMENTS."
  (let* ((goal-id (e-goal--argument-string arguments :goal-id))
         (goal (e-goal--get registry goal-id))
         (blocker-id (or (e-goal--argument-string arguments :blocker-id)
                         (format "blocker-%d"
                                 (1+ (length (plist-get goal :blockers))))))
         (description (e-goal--argument-string arguments :description)))
    (unless description
      (signal 'wrong-type-argument (list 'stringp :description)))
    (push (list :id blocker-id
                :description description
                :resolved nil
                :evidence (plist-get arguments :evidence))
          (plist-get goal :blockers))
    (plist-put goal :status 'blocked)
    (e-goal--status goal)))

(defun e-goal-clear-blocker (registry arguments)
  "Clear a blocker in REGISTRY from ARGUMENTS."
  (let* ((goal-id (e-goal--argument-string arguments :goal-id))
         (blocker-id (e-goal--argument-string arguments :blocker-id))
         (goal (e-goal--get registry goal-id))
         (blocker (or (e-goal--find-by-id (plist-get goal :blockers) blocker-id)
                      (user-error "No blocker %s in goal %s"
                                  blocker-id goal-id)))
         (updated (copy-sequence blocker)))
    (plist-put updated :resolved t)
    (when (plist-member arguments :evidence)
      (plist-put updated :evidence (plist-get arguments :evidence)))
    (plist-put goal :blockers
               (e-goal--replace-by-id (plist-get goal :blockers) updated))
    (unless (e-goal--blockers goal)
      (plist-put goal :status 'active))
    (when (e-goal-achieved-p goal)
      (plist-put goal :status 'achieved))
    (e-goal--status goal)))

(defun e-goal-assess (registry arguments)
  "Assess whether a goal in REGISTRY is achieved."
  (let* ((goal (e-goal--get registry
                            (e-goal--argument-string arguments :goal-id)))
         (status (e-goal--status goal)))
    (when (plist-get status :achieved)
      (plist-put goal :status 'achieved)
      (setq status (e-goal--status goal)))
    status))

(defun e-goal-stop (registry arguments)
  "Stop a goal in REGISTRY without marking it achieved."
  (let ((goal (e-goal--get registry
                           (e-goal--argument-string arguments :goal-id))))
    (plist-put goal :status 'stopped)
    (when (plist-member arguments :reason)
      (plist-put goal :stop-reason (plist-get arguments :reason)))
    (e-goal--status goal)))

(defun e-goal--resource-provider ()
  "Return resource provider for the goal process reference."
  (lambda (store capability)
    (e-store-register
     store
     (e-capability-id capability)
     "refs/process.md"
     :description "Goal controller loop and achievement predicate."
     :content e-goal-process-reference)))

(defun e-goal--action (handler)
  "Return goal action descriptor for HANDLER."
  (e-action-create :handler handler))

(cl-defun e-goal-capability-create
    (&key (id 'goal) (name "Goal") registry)
  "Create the Goal capability.
REGISTRY defaults to `e-goal-default-registry'."
  (let ((registry (or registry e-goal-default-registry)))
    (e-capability-create
     :id id
     :name name
     :instruction-priority 255
     :instructions e-goal-instructions
     :resources (list (e-goal--resource-provider))
     :actions
     (list :define-goal
           (e-goal--action
            (lambda (arguments)
              (e-goal-define registry arguments)))
           :list-goals
           (e-goal--action
            (lambda (arguments)
              (e-goal-list registry arguments)))
           :goal-status
           (e-goal--action
            (lambda (arguments)
              (e-goal-status registry arguments)))
           :next-action
           (e-goal--action
            (lambda (arguments)
              (e-goal-next-action registry arguments)))
           :record-step
           (e-goal--action
            (lambda (arguments)
              (e-goal-record-step registry arguments)))
           :record-criterion
           (e-goal--action
            (lambda (arguments)
              (e-goal-record-criterion registry arguments)))
           :record-blocker
           (e-goal--action
            (lambda (arguments)
              (e-goal-record-blocker registry arguments)))
           :clear-blocker
           (e-goal--action
            (lambda (arguments)
              (e-goal-clear-blocker registry arguments)))
           :assess-goal
           (e-goal--action
            (lambda (arguments)
              (e-goal-assess registry arguments)))
           :stop-goal
           (e-goal--action
            (lambda (arguments)
              (e-goal-stop registry arguments)))))))

(provide 'e-goal)

;;; e-goal.el ends here
