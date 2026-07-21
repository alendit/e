;;; e-goodnite.el --- goodnite daydream layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The goodnite daydream layer.  It exposes the read-only goodnite:// knowledge
;; scheme and a `goodnite' capability that tells a live agent when and how to
;; consult the task knowledge goodnite mined from past sessions.  Reads go
;; through the ordinary read/glob/search tools over goodnite:// URIs; no new
;; tool surface is added.

;;; Code:

(require 'e-capabilities)
(require 'e-layer)
(require 'e-skills)
(require 'e-goodnite-resources)

(defconst e-goodnite-instructions
  "A local base of task knowledge mined from your own past sessions is searchable at goodnite://: workflows (how to do a recurring task), pitfalls (failure modes to avoid), and conventions (project or tool preferences). When a task looks like something that has been done before -- a multi-step operation, work with a known domain gotcha, or work in a project with accumulated conventions -- search goodnite:// for it first, before reinventing an approach. Search by the task in front of you, not by tool name; scope to goodnite://workflows/, goodnite://pitfalls/, or goodnite://conventions/ when you know which you want. Glob and search are cheap (title, when-to-use, scope, confidence); read a leaf goodnite://<type>/<slug> URI only once a hit looks worth committing to. Weight every hit by its confidence: `established' knowledge is human-reviewed; `mined (unreviewed)' is a strong lead from real prior runs but not vetted, so sanity-check it before relying on it. Do not consult goodnite:// for trivial one-off asks. Cite the goodnite:// URI when a hit shapes your approach. After a hit materially helped, record a `process_marker' with signal `effective' citing the goodnite:// URI; when a hit was stale or misleading, record `friction' or `correction' with the URI. That feedback is how the base earns promotion and how its guidance is tuned -- it is a real part of the task, not overhead."
  "Model-facing instructions for the goodnite capability.")

(defconst e-goodnite-using-skill-body
  "# Using goodnite

goodnite:// is a read-only base of task knowledge distilled from your own past
sessions. Consult it the way you would ask a colleague who has done this before.

## The three knowledge types

- **workflows** -- how to do a recurring, multi-step task. The entry body is a
  step-by-step method with a when-to-use line.
- **pitfalls** -- a known failure mode and its fix. Consult when a task touches
  a domain or tool that has bitten past runs.
- **conventions** -- accumulated project or tool preferences, scoped to a
  project root. Consult when starting work in a project you have notes on.

## How to consult

1. Search by the task, not the tool: `search goodnite:// \"resolve rebase
   conflicts\"`, not `search goodnite:// \"git\"`. Scope to one type when you
   know which fits (`search goodnite://pitfalls/ ...`). Search is semantic
   when the knowledge index is built, so plain-language task descriptions
   find the right entry even without a keyword hit; it falls back to lexical
   matching otherwise.
2. Glob to browse a type: `glob goodnite://workflows/` lists entries with a
   one-line when-to-use, scope, and confidence -- cheap, no bodies.
3. Read the leaf URI only when a stub looks worth committing to. `read
   goodnite://workflows/<slug>` returns the full method.

## Trusting a hit

Every entry carries a `confidence`:

- `established` -- human-reviewed. Trust it like a published skill.
- `mined (unreviewed)` -- a strong lead mined from real prior runs, but not yet
  vetted. Use it to orient, then sanity-check the specifics against ground
  truth before you rely on it. When two hits conflict, prefer the reviewed one.

## When not to consult

Skip goodnite:// for trivial one-off asks, for tasks with no recurring shape,
and when you already hold a better, current answer. It is a memory of process,
not a substitute for reading the actual code, tests, or docs in front of you.

## Citing and feedback

When a goodnite:// hit shapes your approach, cite the entry URI so the choice
is traceable and so the consultation registers as evidence the knowledge earns
its keep.

Close the loop with a process marker that names the entry URI:

- A hit that materially helped -> `process_marker' signal `effective', note
  citing the `goodnite://<type>/<slug>' URI.
- A hit that was stale, wrong, or sent you down a wrong path -> `friction' or
  `correction', again citing the URI.

These markers are the quality signal the offline loop reads: consulted-and-
worked reinforces promotion, consulted-and-hurt is a demote/retire signal, and
a pattern of the guidance steering you wrong becomes a proposal to adjust this
capability's own instructions. Marking is part of doing the task well, not
extra work."
  "Body of the using-goodnite skill.")

(defun e-goodnite-capability-create ()
  "Create the goodnite daydream capability."
  (e-capability-with-skills-create
   :id 'goodnite
   :name "Goodnite"
   :instruction-priority 120
   :instructions e-goodnite-instructions
   :skills
   (list (e-skill-spec-create
          :name "using-goodnite"
          :description
          (concat "How to consult the goodnite:// task-knowledge base: the "
                  "three knowledge types, how to search/glob/read, how "
                  "confidence maps to trust, and when not to consult.")
          :content e-goodnite-using-skill-body))
   :resource-methods
   (list (e-capability-resource-method-provider-create
          :handler #'e-goodnite-resources-register-resource-methods))))

(defun e-goodnite-layer-create ()
  "Create the goodnite daydream layer."
  (e-layer-create
   :id 'goodnite
   :name "Goodnite"
   :capabilities (list (e-goodnite-capability-create))))

(provide 'e-goodnite)

;;; e-goodnite.el ends here
