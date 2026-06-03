# Changelog

## Unreleased

- Org Canvas buffers now show the same context-state indicator as chat (model,
  reasoning effort, and context-token usage/estimate against the model limit)
  in the major-mode slot, and offer `e-org-canvas-compact` (`C-c C-m`) to
  compact the backing session.  The background computation is shared through a
  new `e-context-status` module reused by both shells.

## 0.1.0 - 2026-05-19

- Add minimal package scaffold with `e-status` and `e-dev-reload`.
- Add Eldev-based tests, lint, compile, and package build support.
- Add local Doom Emacs installation instructions.
