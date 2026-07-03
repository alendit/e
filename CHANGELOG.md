# Changelog

## Unreleased

- The cron engine (`e-cron`) now separates schedule *definitions* from runtime
  *state*.  A `last-fire` time is persisted per schedule to `e-cron-state-file`
  (default under the user emacs directory), so a schedule's cadence survives
  Emacs restarts and layer reloads.  The due time is derived arithmetically as
  `last-fire + interval` (the interval is derived once from the recurrence and
  cached on the schedule), and a never-fired schedule anchors its first fire to
  a persisted first-registration time.  Re-registering an unchanged definition
  no longer pushes an interval schedule's next fire forward, and a fire that
  came due while Emacs was down runs once on the next start under the existing
  `run` catch-up policy.

- Org Canvas gains a sentence-per-line reflow remediation: the
  `e-org-canvas-reflow-sentences` command (`C-c C-q`), a `reflow-sentences`
  shell command, and an `org-canvas` `:reflow-sentences` action rewrite prose
  paragraphs so each sentence sits on its own physical line, indenting
  list-item continuations and leaving headings, code/verbatim blocks, tables,
  drawers, and inline links/`=verbatim=` spans untouched.  Abbreviations,
  initials, and decimals do not trigger spurious breaks.

- Org Canvas / chat-session canvas guidance now directs durable writes to the
  canvas attachment uri (document-uri) and explicitly warns against writing to
  the look-alike *e-org-canvas:...* / *e-org-canvas-input:...* helper buffers,
  preventing edits that silently miss the document.
- Org Canvas buffers now show the same context-state indicator as chat (model,
  reasoning effort, and context-token usage/estimate against the model limit)
  in the major-mode slot, and offer `e-org-canvas-compact` (`C-c C-m`) to
  compact the backing session.  The background computation is shared through a
  new `e-context-status` module reused by both shells.

## 0.1.0 - 2026-05-19

- Add minimal package scaffold with `e-status` and `e-dev-reload`.
- Add Eldev-based tests, lint, compile, and package build support.
- Add local Doom Emacs installation instructions.
