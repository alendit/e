;;; e-text-editing.el --- Text editing guidance layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Text-editing layer.  This layer contributes no tools; it packages
;; progressive, skill-like guidance for editing workflows that agents can load
;; on demand.

;;; Code:

(require 'e-annotation-tools)
(require 'e-capabilities)
(require 'e-layers)
(require 'e-skills)

(defconst e-text-editing-org-annotate-skill
  (string-join
   '("# Working with org-annotate annotation threads"
     ""
     "org-annotate stores annotation anchors and threaded replies directly in the Org file being annotated. The Org file is the source of truth. There is no sidecar database."
     ""
     "## When to use this guidance"
     ""
     "Use this guidance when the user is working in an Org buffer that contains `<<oa:...>>` anchors or an `* Annotations :noexport:` section, or when they ask you to answer, inspect, resolve, reopen, create, or render inline org-annotate threads."
     ""
     "Use the Simply Annotate skill instead for `.simply-annotations.el` databases or buffers using the `simply-annotate` package."
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
     "## Preferred API"
     ""
     "Prefer the package API over direct text edits when Emacs has the file open:"
     ""
     "- `(org-annotate-entries)` returns all annotation records in the current Org buffer."
     "- `(org-annotate-get ID)` returns one annotation record with metadata and messages."
     "- `(org-annotate-current-id)` returns the annotation ID at point or in the current annotation subtree."
     "- `(org-annotate-reply ID BODY \"agent\")` appends an agent reply."
     "- `(org-annotate-set-state ID STATE)`, `(org-annotate-resolve ID)`, `(org-annotate-reopen ID)`, and `(org-annotate-close ID)` update state."
     "- `(org-annotate-create BEG END BODY \"agent\")` creates an agent-authored annotation. Interactive user-authored commands use author `user` and do not ask for an author."
     "- `(org-annotate-refresh)` refreshes rendered anchors and inline previews when `org-annotate-mode` is active."
     ""
     "## Reading and answering threads"
     ""
     "- In a live Org buffer, use `org-annotate-current-id` when the user says the cursor points at the annotation."
     "- If the user says to answer the newest thread, pick the annotation entry with the newest `OA_CREATED` timestamp, or the newest open entry when that context matters."
     "- Inspect `:messages` from `org-annotate-get`; the first message is the user's original comment and later messages are replies."
     "- Reply with `(org-annotate-reply ID BODY \"agent\")`, then save the buffer when the user expects persistence."
     "- After replying in a visible buffer, call `org-annotate-refresh` if `org-annotate-mode` is active so inline previews do not go stale."
     ""
     "Example live reply:"
     ""
     "```elisp"
     "(with-current-buffer \"file.org\""
     "  (let ((id (or (org-annotate-current-id)"
     "                (car (last (org-annotate-list-ids))))))"
     "    (org-annotate-reply id \"Answer text.\" \"agent\")"
     "    (save-buffer)"
     "    (when org-annotate-mode"
     "      (org-annotate-refresh))))"
     "```"
     ""
     "## Direct file edits"
     ""
     "Only edit the Org structure directly when the package API is unavailable. Preserve the existing anchor, annotation heading, property drawer, and message hierarchy. Use author `agent` for agent replies and `user` only for user-authored messages. Do not move or delete unrelated annotations."
     ""
     "## Rendering behavior"
     ""
     "- `org-annotate-mode` renders anchors as compact `[ann]` markers."
     "- `org-annotate-toggle-inline` toggles a small inline paragraph preview for one annotation."
     "- Resolved annotations are hidden from rendered anchors and inline previews by default."
     "- If display looks stale after a state change or reply, refresh with `org-annotate-refresh` in the live buffer.")
   "\n")
  "Detailed guidance for working with org-annotate annotation threads.")

(defconst e-text-editing-annotations-skill
  (string-join
   '("# Working with Simply Annotate annotations"
     ""
     "Simply Annotate stores project review threads as Emacs Lisp data in a project-local `.simply-annotations.el` file when `simply-annotate-database-strategy` is `project` or `both`. The global fallback is `simply-annotations.el` under `user-emacs-directory` or Doom's local cache, depending on the user's Emacs setup."
     ""
     "## When to use this guidance"
     ""
     "Use this guidance when the user asks you to inspect, answer, reconcile, or otherwise work with Simply Annotate comments or annotation threads. Do not load it for ordinary text editing unless annotations are relevant."
     ""
     "## Discovery"
     ""
     "- Prefer the live Emacs state when available: inspect `simply-annotate-file`, `simply-annotate-project-file`, `simply-annotate-database-strategy`, and `(simply-annotate--database-path)` in the relevant buffer."
     "- In a repository, the usual project-local file is `.simply-annotations.el` at the project root."
     "- The database is an alist keyed by file key, commonly a project-relative file path. Each value is a list of serialized annotations for that file."
     "- Each annotation normally has `start`, `end`, `text`, `text-hash`, and `text-context` fields. For threaded annotations, `text` is an alist with `id`, `created`, `status`, `priority`, `tags`, and `comments`."
     ""
     "## Reading threads"
     ""
     "- Read the annotation database as data, not as prose. It is generated Lisp; preserve its shape."
     "- For each thread, inspect the first comment for the user's original note and subsequent comments for replies."
     "- Use `start`, `end`, and `text-context` to understand what source text the annotation refers to. If the buffer is live, also inspect the corresponding source range."
     "- If overlays are missing after database edits, reload annotations in the live buffer with `simply-annotate--clear-all-overlays`, `simply-annotate--load-annotations`, and `simply-annotate--update-header` when those internals are available."
     ""
     "## Replying to threads"
     ""
     "- Prefer Simply Annotate's interactive commands for user-driven editing. If directly editing the database, keep the exact data model intact."
     "- Add replies as additional comment alists inside the existing thread's `comments` list. A reply should include `id`, `parent-id`, `author`, `timestamp`, `text`, and `(type . \"reply\")`."
     "- Set `author` to the user-requested name exactly, for example `Agent`, when asked."
     "- Set `parent-id` to the comment being answered, usually the root comment id."
     "- Preserve existing thread ids, source positions, hashes, contexts, status, priority, and tags unless the user explicitly asks to change them."
     "- After editing the database, validate that Emacs can read it as Lisp data before claiming success."
     ""
     "## Safety"
     ""
     "- Do not evaluate annotation database content as code. Read it as data."
     "- Avoid rewriting the whole database unless necessary. If you must rewrite it, preserve all existing annotations and file keys."
     "- Do not drop text properties intentionally needed by the package unless simplifying corrupted data is necessary to restore readability. Plain strings are acceptable annotation text values."
     "- If an edit causes overlays to disappear or deserialization errors, stop and repair the data shape rather than asking the user to work around it.")
   "\n")
  "Detailed guidance for working with Simply Annotate annotation databases.")

(defun e-text-editing-annotations-capability-create ()
  "Create the annotations guidance and action capability."
  (e-capability-with-skills-create
   :id 'annotations
   :name "Annotations"
   :instruction-priority 230
   :instructions "Use annotation guidance when the user asks to inspect or respond to text annotation threads. For Org files with in-file `<<oa:...>>` anchors or an `* Annotations :noexport:` section, read e://annotations/skills/org-annotate. For Simply Annotate databases, read e://annotations/skills/simply-annotate. Use actions through e-actions-call only for Simply Annotate workflows; read e-action://annotations when active action contracts are needed."
   :actions (when (e-annotation-tools-available-p)
              (e-annotation-tools--actions))
   :skills
   (list
    (e-skill-spec-create
     :name "org-annotate"
     :description "Work with org-annotate in-file Org annotation threads."
     :content e-text-editing-org-annotate-skill)
    (e-skill-spec-create
     :name "simply-annotate"
     :description "Work with Simply Annotate annotation databases and threaded replies."
     :content e-text-editing-annotations-skill))))

(defun e-text-editing-layer-create ()
  "Create the text-editing layer."
  (e-layer-create
   :id 'text-editing
   :name "Text Editing"
   :capabilities
   (list (e-text-editing-annotations-capability-create))))

(provide 'e-text-editing)

;;; e-text-editing.el ends here
