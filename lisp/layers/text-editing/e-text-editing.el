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
   :instructions "Use annotation guidance when the user asks to inspect or respond to text annotation threads. Use actions through e-actions-call for annotation workflows; read e-action://annotations when active action contracts are needed."
   :actions (when (e-annotation-tools-available-p)
              (e-annotation-tools--actions))
   :skills
   (list
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
