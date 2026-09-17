;;; e-annotations.el --- Annotation answer-loop layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The annotations layer over the org-annotate in-file model.  It contributes:
;;
;; - stable annotation actions (`:list', `:reply', `:add', `:resolve') that
;;   agents call from `run_elisp' with (e-actions-call 'annotations ...);
;; - the `org-annotate' skill guiding their use;
;; - the interactive `e-annotations-answer' command (Tier 0/1);
;; - the `e-annotation-answer-sweep' generic loop primitive (Tier 2).
;;
;; The capability is only active when org-annotate is installed: the layer's
;; capability list is empty otherwise, so a session without the dependency
;; simply does not carry the annotations surface.  There is one backend
;; (org-annotate) and every action is guarded to Org files.

;;; Code:

(require 'e-annotation-org)
(require 'e-capabilities)
(require 'e-json)
(require 'e-layers)
(require 'e-skills)
(require 'e-telemetry)

(autoload 'e-annotation-answer-dispatch "e-annotation-answer")
(autoload 'e-annotation-answer-sweep "e-annotation-answer")
(autoload 'e-annotations-answer "e-annotation-answer" nil t)

(defun e-annotations--string-or-null (value)
  "Return VALUE when it is a string, otherwise canonical JSON null."
  (if (stringp value) value e-json-null))

(defun e-annotations--message-result (message)
  "Project backend MESSAGE into one canonical action result object."
  (list :author (e-annotations--string-or-null (plist-get message :author))
        :created (e-annotations--string-or-null (plist-get message :created))
        :body (e-annotations--string-or-null (plist-get message :body))))

(defun e-annotations--thread-result (thread)
  "Project backend THREAD into one canonical action result object."
  (list :id (e-annotations--string-or-null (plist-get thread :id))
        :state (e-annotations--string-or-null (plist-get thread :state))
        :author (e-annotations--string-or-null (plist-get thread :author))
        :created (e-annotations--string-or-null (plist-get thread :created))
        :range-text (e-annotations--string-or-null
                     (plist-get thread :range-text))
        :actionable (if (plist-get thread :actionable)
                        t
                      e-json-false)
        :messages (vconcat
                   (mapcar #'e-annotations--message-result
                           (plist-get thread :messages)))))

(defun e-annotations--list-result (result)
  "Project backend listing RESULT into canonical action JSON."
  (list :file (e-annotations--string-or-null (plist-get result :file))
        :count (or (plist-get result :count) 0)
        :threads (vconcat
                  (mapcar #'e-annotations--thread-result
                          (plist-get result :threads)))))

(defun e-annotations--mutation-result (result &optional include-effects)
  "Project annotation mutation RESULT into canonical action JSON.
When INCLUDE-EFFECTS is non-nil, hook effects are deliberately represented as
bounded textual diagnostics because extension hooks are arbitrary Elisp and do
not have a shared structured contract."
  (let ((projected
         (list :file (e-annotations--string-or-null (plist-get result :file))
               :id (e-annotations--string-or-null (plist-get result :id)))))
    (dolist (key '(:author :state :start :end))
      (when (plist-member result key)
        (setq projected
              (plist-put projected key
                         (let ((value (plist-get result key)))
                           (cond
                            ((memq key '(:author :state))
                             (e-annotations--string-or-null value))
                            ((integerp value) value)
                            (t e-json-null)))))))
    (when include-effects
      (setq projected
            (plist-put
             projected :effects
             (vconcat
              (mapcar
               (lambda (effect)
                 (list :text (e-telemetry-preview effect)))
               (plist-get result :effects))))))
    projected))

(defconst e-annotations-org-annotate-skill
  (string-join
   '("# Working with org-annotate annotation threads"
     ""
     "org-annotate stores annotation anchors and threaded replies directly in the Org file being annotated. The Org file is the source of truth. There is no sidecar database."
     ""
     "## When to use this guidance"
     ""
     "Use this guidance when the user is working in an Org buffer that contains `<<oa:...>>` anchors or an `* Annotations :noexport:` section, or when they ask you to answer, inspect, resolve, reopen, create, or render inline org-annotate threads."
     ""
     "## Data model"
     ""
     "- Inline anchors are Org targets in normal body text: `<<oa:ID>>`."
     "- Annotation records live under a file-local `* Annotations :noexport:` heading."
     "- Each annotation is a child heading whose title is the annotation ID."
     "- Metadata lives in Org properties such as `OA_ID`, `OA_ANCHOR`, `OA_STATE`, `OA_CREATED`, `OA_AUTHOR`, `OA_RANGE_TEXT`, and `OA_RANGE_LENGTH`."
     "- Thread messages are child headings below the annotation entry. The first message is normally `Comment`; later messages are normally `Reply`."
     "- Resolved annotations can be hidden from rendered anchors and inline previews, but their records remain in the Org file."
     ""
     "## Preferred access: the annotations actions"
     ""
     "Prefer the capability actions over direct text edits or raw org-annotate calls. They run headless and are guarded to Org files:"
     ""
     "- `(e-actions-call 'annotations :list '(:file FILE :actionable-only BOOL))` returns thread result plists (`:id :state :range-text :messages` with per-message `:author`, plus `:actionable`). With `:actionable-only t` it returns only threads still awaiting an agent answer."
     "- `(e-actions-call 'annotations :reply '(:file FILE :id ID :body TEXT))` appends an agent reply. Append-only."
     "- `(e-actions-call 'annotations :add '(:file FILE :start BEG :end END :body TEXT))` creates a new annotation thread (a proposal). Omit start/end to anchor without a covered range."
     "- `(e-actions-call 'annotations :resolve '(:file FILE :id ID :state STATE :reply TEXT))` sets a thread's state (default `resolved`) and optionally appends a reply first. Resolving records the state and fires the resolve hook; it does not itself rewrite document prose."
     ""
     "## The actionable predicate"
     ""
     "A thread is actionable when its state is `open` and its last message was not authored by `agent`. Once you reply, the thread's last author becomes `agent` and it is no longer actionable, so a repeated answer pass never double-answers."
     ""
     "## Answering safely"
     ""
     "- Reply append-only; do not rewrite the user's prose in the background."
     "- If a thread implies a prose correction, emit it as a proposal with `:add` keyed to the region, and let a human accept it."
     "- Only `:resolve` a thread when its request is genuinely handled."
     ""
     "## Direct file edits and rendering"
     ""
     "Only edit the Org structure directly when the actions are unavailable. Preserve the existing anchor, annotation heading, property drawer, and message hierarchy. Use author `agent` for agent replies. In a live buffer with `org-annotate-mode` active, refresh stale inline previews with `(org-annotate-refresh)`.")
   "\n")
  "Guidance for working with org-annotate annotation threads.")

(defun e-annotations--actions ()
  "Return the annotation action plist over the org-annotate backend."
  (list
   :list
   (e-action-cheap-create
    :owner 'annotations
    :runner (lambda (arguments _context)
              (e-annotations--list-result
               (e-annotation-org-list
                :file (plist-get arguments :file)
                :actionable-only (eq (plist-get arguments :actionable_only) t))))
    :description "List org-annotate threads on an Org file. With actionable_only, return only threads still awaiting an agent answer (open state, non-agent last author)."
    :parameters '(:type "object"
                  :properties (:file (:type "string")
                               :actionable_only (:type "boolean"))
                  :required ["file"]))
   :reply
   (e-action-cheap-create
    :owner 'annotations
    :runner (lambda (arguments _context)
              (e-annotations--mutation-result
               (e-annotation-org-reply
                :file (plist-get arguments :file)
                :id (or (plist-get arguments :id)
                        (plist-get arguments :annotation_id))
                :body (plist-get arguments :body)
                :author (plist-get arguments :author))))
    :description "Append an agent reply to an org-annotate thread. Append-only; does not change state."
    :parameters '(:type "object"
                  :properties (:file (:type "string")
                               :id (:type "string")
                               :body (:type "string")
                               :author (:type "string"))
                  :required ["file" "id" "body"]))
   :add
   (e-action-cheap-create
    :owner 'annotations
    :runner (lambda (arguments _context)
              (e-annotations--mutation-result
               (e-annotation-org-add
                :file (plist-get arguments :file)
                :start (plist-get arguments :start)
                :end (plist-get arguments :end)
                :body (plist-get arguments :body)
                :author (plist-get arguments :author))))
    :description "Create a new org-annotate thread (a proposal) anchored to an Org file region. Omit start/end to anchor without a covered range."
    :parameters '(:type "object"
                  :properties (:file (:type "string")
                               :start (:type "integer")
                               :end (:type "integer")
                               :body (:type "string")
                               :author (:type "string"))
                  :required ["file" "body"]))
   :resolve
   (e-action-cheap-create
    :owner 'annotations
    :runner (lambda (arguments _context)
              (e-annotations--mutation-result
               (e-annotation-org-resolve
                :file (plist-get arguments :file)
                :id (or (plist-get arguments :id)
                        (plist-get arguments :annotation_id))
                :state (plist-get arguments :state)
                :reply (plist-get arguments :reply)
                :author (plist-get arguments :author))
               t))
    :description "Set an org-annotate thread's state (default resolved), optionally appending a reply first, and fire the resolve hook. Resolving records state but does not apply domain mutations itself."
    :parameters '(:type "object"
                  :properties (:file (:type "string")
                               :id (:type "string")
                               :state (:type "string")
                               :reply (:type "string")
                               :author (:type "string"))
                  :required ["file" "id"]))))

(defun e-annotations-capability-create ()
  "Create the annotations capability over the org-annotate backend.
Returns nil when org-annotate is not installed, so the layer omits the
capability rather than exposing actions with no backend."
  (when (e-annotation-org-available-p)
    (e-capability-with-skills-create
     :id 'annotations
     :name "Annotations"
     :instruction-priority 230
     :instructions "Use annotation actions over the org-annotate in-file model to list, answer, propose, and resolve annotation threads in Org files. Call them from run_elisp with (e-actions-call 'annotations :list/:reply/:add/:resolve ...). Read e://annotations/skills/org-annotate for the process; read e-action://annotations for the action contracts. Reply append-only; emit prose corrections as :add proposals a human accepts; only :resolve a thread whose request is genuinely handled. The interactive command e-annotations-answer dispatches a background answerer for the current document."
     :actions (e-annotations--actions)
     :skills
     (list
      (e-skill-spec-create
       :name "org-annotate"
       :description "Work with org-annotate in-file Org annotation threads."
       :content e-annotations-org-annotate-skill)))))

(defun e-annotations-layer-create ()
  "Create the annotations layer.
The capability is present only when org-annotate is installed; without it the
layer carries no capabilities."
  (e-layer-create
   :id 'annotations
   :name "Annotations"
   :capabilities (delq nil (list (e-annotations-capability-create)))))

(provide 'e-annotations)

;;; e-annotations.el ends here
