;;; e-chat.el --- Basic chat presentation for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Minimal Emacs chat buffer for the e harness.  This module owns presentation
;; only: buffer setup, commands, keymaps, and event rendering.

;;; Code:

(require 'cl-lib)
(require 'pp)
(require 'project)
(require 'subr-x)
(require 'e-chat-session)
(require 'e-chat-output-mode)
(require 'e-chat-service)
(require 'e-board)
(require 'e-context-inspection)
(require 'e-context-status)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-message-details)
(require 'e-prompts)
(require 'e-request)
(require 'e-store)
(require 'e-structured-blocks)
(require 'e-tools)
(require 'e-picker)
(require 'e-session)
(require 'e-shells)
(require 'e-startup)
(require 'e-work)
(require 'e-ui-work)
(require 'e-workspaces)

;; Load presentation owners in dependency order.  Keeping this at the import
;; boundary makes the composition root's collaborators real at evaluation
;; time, rather than relying on declarations until the end of this file:
;; surface -> transcript -> composer -> activity -> overview.
(require 'e-chat-surface)
(require 'e-chat-transcript)
(require 'e-chat-composer)
(require 'e-chat-activity)
(require 'e-chat-overview)

;; Writable workspace snapshots must retain Emacs's native surface structure.
(add-to-list 'window-persistent-parameters '(window-atom . writable))

(declare-function markdown-mode "markdown-mode")
(declare-function org-mode "org")
(declare-function +workspace/display "ext:doom-workspaces")
(defvar org-inhibit-startup)
(defvar org-mode-hook)
(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")
(declare-function visual-fill-column-mode "ext:visual-fill-column")

;; Public presentation-owner ports used by this composition root.  Keeping the
;; declarations here documents the dependency direction without importing any
;; owner-private state into the facade.
(declare-function e-chat-surface-transcript-p "e-chat-surface")
(declare-function e-chat-surface-composer-p "e-chat-surface")
(declare-function e-chat-surface-transcript-buffer "e-chat-surface")
(declare-function e-chat-surface-composer-buffer "e-chat-surface")
(declare-function e-chat-surface-mark-transcript "e-chat-surface")
(declare-function e-chat-surface-bind-composer "e-chat-surface")
(declare-function e-chat-surface-kill-composer "e-chat-surface")
(declare-function e-chat-surface-refresh-composer-position "e-chat-surface")
(declare-function e-chat-surface-refresh-visible-windows "e-chat-surface")
(declare-function e-chat-surface-capture-live-output-follow-windows "e-chat-surface")
(declare-function e-chat-surface-restore-output-tail-windows "e-chat-surface")
(declare-function e-chat-surface-after-display-buffer "e-chat-surface")
(declare-function e-chat-surface-display-from-side-window "e-chat-surface")
(declare-function e-chat-surface-pop-to-buffer "e-chat-surface")
(declare-function e-chat-surface-initialize "e-chat-surface")
(declare-function e-chat-surface-setup-line-wrapping "e-chat-surface")
(declare-function e-chat-surface-mark-composer-layout-dirty "e-chat-surface")
(declare-function e-chat-surface-capture-selected-output-follow-command "e-chat-surface")
(declare-function e-chat-surface-pre-command "e-chat-surface")
(declare-function e-chat-surface-post-command "e-chat-surface")
(declare-function e-chat-surface-refresh-command-map "e-chat-surface")
(declare-function e-chat-overview-mark-selected-session-read "e-chat-overview")
(declare-function e-chat-overview-session-selection "e-chat-overview")
(declare-function e-chat-overview-active-session-candidates "e-chat-overview")
(declare-function e-chat-overview-active-session-candidate-key "e-chat-overview")
(declare-function e-chat-overview-active-session-line "e-chat-overview")
(declare-function e-chat-overview-active-session-preview "e-chat-overview")
(declare-function e-chat-overview-refresh-keymap "e-chat-overview")
(declare-function e-chat-surface-refresh-mode-line-status "e-chat-surface")
(declare-function e-chat-surface-request-mode-line-status-refresh "e-chat-surface")
(declare-function e-chat-surface-selected-chat-surface "e-chat-surface")
(declare-function e-chat-surface-set-window-output-follow "e-chat-surface")
(declare-function e-chat-surface-set-board-status "e-chat-surface")
(declare-function e-chat-surface-clear-board-status "e-chat-surface")
(declare-function e-chat-surface-board-status "e-chat-surface" (&optional buffer))
(declare-function e-board-activity-visual-open-or-text
                  "e-board-activity-visual-shell"
                  (target binding run-id &optional live owner-chat on-dismiss))
(declare-function e-board-activity-visual-open-buffer
                  "e-board-activity-visual-shell" (&rest args))
(declare-function e-board-activity-visual-unavailable-reason
                  "e-board-activity-visual-shell")
(declare-function e-board-activity-visual-visible-for-owner-p
                  "e-board-activity-visual-shell" (owner-chat))
(declare-function e-board-activity-visual-close-for-owner
                  "e-board-activity-visual-shell" (owner-chat))
(declare-function e-board-activity-shell--popup-available-p
                  "e-board-activity-shell")
(declare-function e-board-activity-shell-open-buffer "e-board-activity-shell"
                  (&rest args))
(declare-function e-chat-surface-window-reaches-output-p "e-chat-surface")
(declare-function e-chat-surface-without-recenter "e-chat-surface")
(declare-function e-chat-composer-active-p "e-chat-composer")
(declare-function e-chat-composer-text "e-chat-composer")
(declare-function e-chat-composer-submission "e-chat-composer")
(declare-function e-chat-composer-ensure "e-chat-composer")
(declare-function e-chat-composer-enter-input-state "e-chat-composer")
(declare-function e-chat-composer-insert "e-chat-composer")
(declare-function e-chat-composer-delete "e-chat-composer")
(declare-function e-chat-composer-insert-context-reference "e-chat-composer")
(declare-function e-chat-composer-capture-context-reference-for-command "e-chat-composer")
(declare-function e-chat-composer-cancel-pending-references "e-chat-composer")
(declare-function e-chat-composer-cancel-file-candidate-refresh "e-chat-composer")
(declare-function e-chat-composer-mark-scroll-needed "e-chat-composer")
(declare-function e-chat-composer-pre-command "e-chat-composer")
(declare-function e-chat-composer-disable-modal-editing "e-chat-composer")
(declare-function e-chat-composer-disable-completion "e-chat-composer")
(declare-function e-chat-composer-start-position "e-chat-composer")
(declare-function e-chat-composer-initialize "e-chat-composer")
(declare-function e-chat-composer-project-root "e-chat-composer")
(declare-function e-chat-transcript-render-session "e-chat-transcript")
(declare-function e-chat-transcript-render-replay "e-chat-transcript")
(declare-function e-chat-transcript-render-visible-message-window
                  "e-chat-transcript")
(declare-function e-chat-transcript-rerender "e-chat-transcript")
(declare-function e-chat-transcript-event-selected-participant-p "e-chat-transcript")
(declare-function e-chat-transcript-message-selected-participant-p "e-chat-transcript")
(declare-function e-chat-transcript-observed-turn-id "e-chat-transcript")
(declare-function e-chat-transcript-presentation-turn-id "e-chat-transcript")
(declare-function e-chat-transcript-rerender-assistant-blocks "e-chat-transcript")
(declare-function e-chat-transcript-system-glyph "e-chat-transcript")
(declare-function e-chat-transcript-insert-entry "e-chat-transcript")
(declare-function e-chat-transcript-insert-protected "e-chat-transcript")
(declare-function e-chat-transcript-render-durable-message "e-chat-transcript")
(declare-function e-chat-transcript-reconcile-message-display "e-chat-transcript")
(declare-function e-chat-transcript-block-at-point "e-chat-transcript")
(declare-function e-chat-transcript-turn-id-at-point "e-chat-transcript")
(declare-function e-chat-transcript-cancel-markdown-presentation "e-chat-transcript")
(declare-function e-chat-transcript-set-preview-p "e-chat-transcript")
(declare-function e-chat-transcript-preview-p "e-chat-transcript")
(declare-function e-chat-transcript-has-navigable-blocks-p "e-chat-transcript")
(declare-function e-chat-transcript-leave-navigation "e-chat-transcript")
(declare-function e-chat-transcript-refresh-keymaps "e-chat-transcript")
(declare-function e-chat-activity-assistant-streaming-p "e-chat-activity")
(declare-function e-chat-activity-set-assistant-streaming "e-chat-activity")
(declare-function e-chat-activity-cancel-pending-redraw "e-chat-activity")
(declare-function e-chat-activity-start-progress "e-chat-activity")
(declare-function e-chat-activity-stop-progress "e-chat-activity")
(declare-function e-chat-activity-delete-turn-transient "e-chat-activity")
(declare-function e-chat-activity-render-turn-transient "e-chat-activity")
(declare-function e-chat-activity-run-pending-redraw "e-chat-activity")
(declare-function e-chat-activity-running-status-turn-id "e-chat-activity")
(declare-function e-chat-activity-progress-turn-id "e-chat-activity")
(declare-function e-chat-activity-render-replay "e-chat-activity")
(declare-function e-chat-activity-handle-event "e-chat-activity")
(declare-function e-chat-activity-message-rendered "e-chat-activity")
(declare-function e-chat-activity-failed-turn-p "e-chat-activity")
(declare-function e-chat-activity-flush-deferred-redraws "e-chat-activity")
(declare-function e-chat-activity-flush-deferred-redraws-after-minibuffer "e-chat-activity")
(declare-function e-chat-overview-update-unread-cache "e-chat-overview")
(declare-function e-chat-overview-remove-unread-buffer "e-chat-overview")
(declare-function e-chat-overview-rebuild-unread-cache "e-chat-overview")
(declare-function e-chat-overview-mark-session-read "e-chat-overview")
(declare-function e-session-async-chat-view "e-session-async")
(defvar e-chat-response-navigation-mode-map)
(defvar e-chat-block-view-mode-map)
(defvar e-chat-tool-list-mode-map)
(defvar e-chat-tool-output-mode-map)


;; Presentation owners initialize their own buffer-local state.  The facade
;; composes their public ports below and deliberately keeps no mirror of those
;; records here.

(defgroup e-chat nil
  "Chat presentation for e."
  :group 'e
  :prefix "e-chat-")

(defcustom e-chat-context-buffer-name "*e-chat-context*"
  "Buffer name for read-only context previews."
  :type 'string
  :group 'e-chat)

(defun e-chat--profile-enabled-p ()
  "Return non-nil when developer profiling is currently available."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-chat--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when developer profiling is enabled."
  (if (e-chat--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-chat-profile-call (event options thunk)
  "Measure presentation THUNK as EVENT with OPTIONS when profiling is enabled."
  (e-chat--profile-call event options thunk))

;; Stable facade ports for callers that historically consumed presentation
;; status directly from `e-chat'.  The semantic cells now live in the surface
;; owner; these functions only compose that owner and intentionally do not
;; mirror its state.
(defun e-chat-status (&optional buffer)
  "Return the presentation status for BUFFER's chat surface."
  (e-chat-surface-status buffer))

(defun e-chat-set-status (status &optional refresh-mode-line)
  "Set the current chat surface STATUS."
  (e-chat-surface-set-status status refresh-mode-line))

(defun e-chat-mode-line-status (&optional buffer)
  "Return semantic mode-line status for BUFFER's chat surface."
  (e-chat-surface-mode-line-status buffer))

(defun e-chat-mode-line-display-text (status)
  "Return host-neutral display text for semantic STATUS."
  (e-chat-surface-mode-line-display-text status))

(defun e-chat-flush-deferred-mode-line-statuses (&rest args)
  "Flush deferred mode-line status work for visible chat buffers."
  (apply #'e-chat-surface-flush-deferred-mode-line-statuses args))

(defun e-chat-invalidate-mode-line-context-estimate (&optional buffer)
  "Invalidate mode-line context caches for BUFFER."
  (e-chat-surface-invalidate-mode-line-context-estimate buffer))

(defun e-chat-refresh-ui-work-diagnostics ()
  "Refresh pending UI-work diagnostics for the current chat surface."
  (e-chat-surface-refresh-ui-work-diagnostics))

(defun e-chat-after-display-buffer (buffer)
  "Restore the composed chat input state after displaying BUFFER.
The surface owner restores window membership and viewport state; this facade
then composes the composer owner for modal/completion cleanup and input focus."
  (e-chat-surface-after-display-buffer buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (e-chat-composer-disable-modal-editing)
      (e-chat-composer-disable-completion)
      (e-chat-composer-enter-input-state)))
  buffer)

(defun e-chat--active-transcript-turn-state ()
  "Return the currently projected active turn id, or nil.
The facade owns this small transition lookup because inserting any durable
presentation row must temporarily remove the activity transient and restore it
after the transcript owner has appended the row.  The activity and transcript
owners retain their respective state; this is only composition glue."
  (or (e-chat-activity-progress-turn-id)
      (e-chat-activity-running-status-turn-id)))

(defun e-chat--insert-transcript-entry
    (title content &optional ensure-composer turn-id details-text)
  "Insert a composed transcript entry and preserve active activity ordering.
The activity transient is removed before the durable row and restored after it,
matching the historical facade behavior while keeping both mechanisms in
their owners."
  (let ((active (e-chat--active-transcript-turn-state)))
    (when active
      (e-chat-activity-delete-turn-transient active))
    (prog1
        (e-chat-transcript-insert-entry
         title content ensure-composer turn-id details-text)
      (when active
        (e-chat-activity-render-turn-transient active)))))

(defun e-chat--render-transcript-message
    (message turn-id &optional ensure-composer details-text)
  "Render durable MESSAGE while preserving the active activity transient."
  (let ((active (e-chat--active-transcript-turn-state)))
    (when active
      (e-chat-activity-delete-turn-transient active))
    (prog1
        (e-chat-transcript-render-durable-message
         message turn-id ensure-composer details-text)
      (when active
        (e-chat-activity-render-turn-transient active)))))

(defun e-chat--ensure-presentation-hooks ()
  "Install composed chat presentation hooks and remove retired bridges.
Surface owns activation and window-refresh callbacks; overview owns read
markers; activity owns deferred redraw flushes.  The facade only composes
those owner ports into the host hook lists."
  ;; These callbacks belonged to the retired active-turn presentation path.
  ;; Remove them even when a development reload leaves them in dynamically
  ;; scoped hook variables.
  (dolist (hook '(window-selection-change-functions
                  window-configuration-change-hook
                  buffer-list-update-hook
                  persp-activated-functions))
    (when (boundp hook)
      (dolist (function '(e-chat--tail-selected-active-turn
                          e-chat--activate-selected-surface-after-buffer-switch
                          e-chat--activate-selected-surface-after-workspace-switch))
        (remove-hook hook function))))
  (e-chat-surface-initialize)
  (add-hook 'window-selection-change-functions
            #'e-chat-overview-mark-selected-session-read)
  (add-hook 'window-configuration-change-hook
            #'e-chat-activity-flush-deferred-redraws)
  (add-hook 'window-configuration-change-hook
            #'e-chat-surface-flush-deferred-mode-line-statuses)
  (add-hook 'window-buffer-change-functions
            #'e-chat-activity-flush-deferred-redraws)
  (add-hook 'window-buffer-change-functions
            #'e-chat-surface-flush-deferred-mode-line-statuses)
  (add-hook 'minibuffer-exit-hook
            #'e-chat-activity-flush-deferred-redraws-after-minibuffer)
  (when (boundp 'persp-activated-functions)
    (add-hook 'persp-activated-functions
              #'e-chat-overview-mark-selected-session-read)))

(defun e-chat--reject-sync-in-hot-path (operation)
  "Reject synchronous chat OPERATION from marked interactive hot paths."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error operation)))

(defun e-chat-reject-sync-in-hot-path (operation)
  "Reject synchronous OPERATION from a marked chat hot path."
  (e-chat--reject-sync-in-hot-path operation))

(defcustom e-chat-default-harness-id :chat-default
  "Harness registry id used by default chat commands."
  :type 'symbol
  :group 'e-chat)

;; Chat block faces inherit neutral theme faces rather than hardcoding a
;; palette, so the chat buffer follows whatever theme (light or dark) the user
;; has active.  `:extend t' lets a block colour fill to the window edge when the
;; inherited face carries a background; with a background-less theme face it is
;; harmless.

(defface e-chat-user-face
  '((t :inherit highlight :extend t))
  "Face used for user-authored chat blocks."
  :group 'e-chat)

(defface e-chat-assistant-face
  '((t :inherit default :extend t))
  "Face used for assistant response chat blocks."
  :group 'e-chat)

(defface e-chat-final-assistant-face
  '((t :inherit e-chat-assistant-face :box nil :extend t))
  "Face used for settled assistant response chat blocks."
  :group 'e-chat)

(defface e-chat-system-face
  '((t :inherit shadow :extend t))
  "Face used for compact system chat blocks."
  :group 'e-chat)

(defface e-chat-hidden-face
  '((t :inherit shadow :slant italic :extend t))
  "Face used for revealed hidden audit chat blocks."
  :group 'e-chat)

(defface e-chat-composer-face
  '((t :inherit minibuffer-prompt :extend t))
  "Face used for the composer prompt chrome."
  :group 'e-chat)

(defface e-chat-separator-face
  '((t :inherit shadow :extend t))
  "Face used for the composer separator."
  :group 'e-chat)

(defface e-chat-turn-separator-face
  '((t :inherit shadow :box nil :extend t))
  "Face used for separators between chat turns."
  :group 'e-chat)

(defface e-chat-response-separator-face
  '((t :inherit shadow :box nil :extend t))
  "Face used for separators between user prompt and agent-side blocks."
  :group 'e-chat)

(defface e-chat-activity-separator-face
  '((t :inherit shadow :box nil :extend t))
  "Face used for separators between intermittent activity rounds."
  :group 'e-chat)

(defun e-chat--apply-owned-face-defaults ()
  "Apply face defaults that should update during live reload."
  (set-face-attribute 'e-chat-separator-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'shadow
                      :extend t)
  (set-face-attribute 'e-chat-turn-separator-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'shadow
                      :box nil
                      :extend t)
  (set-face-attribute 'e-chat-response-separator-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'shadow
                      :box nil
                      :extend t)
  (set-face-attribute 'e-chat-activity-separator-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'shadow
                      :box nil
                      :extend t))

(e-chat--apply-owned-face-defaults)

(defface e-chat-title-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1 :extend t))
  "Face used for the chat buffer title."
  :group 'e-chat)

(defface e-chat-focused-turn-face
  '((t :inherit region :box nil :extend t))
  "Face used for the focused turn in response navigation mode."
  :group 'e-chat)

(defface e-chat-markdown-strong-face
  '((t :inherit e-chat-assistant-face :weight bold))
  "Face used for strong Markdown spans in assistant messages."
  :group 'e-chat)

(defface e-chat-markdown-emphasis-face
  '((t :inherit e-chat-assistant-face :slant italic))
  "Face used for emphasized Markdown spans in assistant messages."
  :group 'e-chat)

(defface e-chat-markdown-code-face
  '((t :inherit (font-lock-constant-face fixed-pitch)))
  "Face used for inline Markdown code in assistant messages."
  :group 'e-chat)

(defface e-chat-markdown-code-block-face
  '((t :inherit fixed-pitch :extend t))
  "Face used for fenced Markdown code blocks in assistant messages."
  :group 'e-chat)

(defface e-chat-markdown-heading-face
  '((t :inherit e-chat-assistant-face :weight bold :height 1.08))
  "Face used for Markdown headings in assistant messages."
  :group 'e-chat)

(defface e-chat-markdown-list-face
  '((t :inherit e-chat-assistant-face :weight bold))
  "Face used for Markdown list items in assistant messages."
  :group 'e-chat)

(defface e-chat-markdown-link-face
  '((t :inherit link))
  "Face used for Markdown link labels in assistant messages."
  :group 'e-chat)

(defface e-chat-context-reference-face
  '((t :inherit (font-lock-constant-face highlight)))
  "Face used for inline context references in the composer."
  :group 'e-chat)

(defface e-chat-overview-unread-face
  '((t :inherit warning :weight bold))
  "Face used for unread markers in the chat session overview."
  :group 'e-chat)

(defface e-chat-workspace-unread-face
  '((t :inherit secondary-selection :weight bold))
  "Face used for unread chat markers in workspace displays."
  :group 'e-chat)

(defface e-chat-overview-title-face
  '((t :inherit default :weight bold))
  "Face used for session titles in the chat session overview."
  :group 'e-chat)

(defface e-chat-overview-meta-face
  '((t :inherit shadow))
  "Face used for compact session metadata in the chat session overview."
  :group 'e-chat)

(defface e-chat-overview-summary-face
  '((t :inherit shadow :slant italic))
  "Face used for session summaries in the chat session overview."
  :group 'e-chat)

(defconst e-chat--user-face-spec
  '((t :inherit highlight :extend t))
  "Default face spec for user-authored chat blocks.")

(defconst e-chat--assistant-face-spec
  '((t :inherit default :extend t))
  "Default face spec for assistant response chat blocks.")

(defconst e-chat--final-assistant-face-spec
  '((t :inherit e-chat-assistant-face :box nil :extend t))
  "Default face spec for settled assistant response chat blocks.")

(defconst e-chat--system-face-spec
  '((t :inherit shadow :extend t))
  "Default face spec for compact system chat blocks.")

(defconst e-chat--focused-turn-face-spec
  '((t :inherit region :box nil :extend t))
  "Default face spec for focused response-navigation blocks.")

(defconst e-chat--turn-separator-face-spec
  '((t :inherit shadow :box nil :extend t))
  "Default face spec for separators between chat turns.")

(defconst e-chat--response-separator-face-spec
  '((t :inherit shadow :box nil :extend t))
  "Default face spec for separators between prompt and response blocks.")

(defconst e-chat--activity-separator-face-spec
  '((t :inherit shadow :box nil :extend t))
  "Default face spec for separators between intermittent activity rounds.")

(defconst e-chat--overview-unread-face-spec
  '((t :inherit warning :weight bold))
  "Default face spec for unread overview markers.")

(defconst e-chat--workspace-unread-face-spec
  '((t :inherit secondary-selection :weight bold))
  "Default face spec for unread workspace markers.")

(defun e-chat--face-color (face attribute)
  "Return usable FACE ATTRIBUTE color, or nil."
  (let ((color (face-attribute face attribute nil t)))
    (when (and (stringp color)
               (not (string-empty-p color))
               (not (equal color "unspecified")))
      color)))

(defun e-chat--workspace-unread-color ()
  "Return the theme secondary color for workspace unread markers."
  (or (e-chat--face-color 'secondary-selection :background)
      (e-chat--face-color 'secondary-selection :foreground)
      (e-chat--face-color 'font-lock-keyword-face :foreground)
      (e-chat--face-color 'default :foreground)))

(defconst e-chat--overview-title-face-spec
  '((t :inherit default :weight bold))
  "Default face spec for overview session titles.")

(defconst e-chat--overview-meta-face-spec
  '((t :inherit shadow))
  "Default face spec for overview session metadata.")

(defconst e-chat--overview-summary-face-spec
  '((t :inherit shadow :slant italic))
  "Default face spec for overview session summaries.")

(defun e-chat--refresh-face-specs ()
  "Refresh chat face defaults after live reload."
  (face-spec-set 'e-chat-user-face e-chat--user-face-spec)
  (face-spec-set 'e-chat-assistant-face e-chat--assistant-face-spec)
  (face-spec-set 'e-chat-final-assistant-face
                 e-chat--final-assistant-face-spec)
  (face-spec-set 'e-chat-system-face e-chat--system-face-spec)
  (face-spec-set 'e-chat-focused-turn-face
                 e-chat--focused-turn-face-spec)
  (face-spec-set 'e-chat-turn-separator-face
                 e-chat--turn-separator-face-spec)
  (face-spec-set 'e-chat-response-separator-face
                 e-chat--response-separator-face-spec)
  (face-spec-set 'e-chat-activity-separator-face
                 e-chat--activity-separator-face-spec)
  (face-spec-set 'e-chat-overview-unread-face
                 e-chat--overview-unread-face-spec)
  (face-spec-set 'e-chat-workspace-unread-face
                 e-chat--workspace-unread-face-spec)
  (set-face-attribute 'e-chat-workspace-unread-face nil
                      :foreground (or (e-chat--workspace-unread-color)
                                      'unspecified)
                      :background 'unspecified
                      :inherit 'default
                      :weight 'bold)
  (face-spec-set 'e-chat-overview-title-face
                 e-chat--overview-title-face-spec)
  (face-spec-set 'e-chat-overview-meta-face
                 e-chat--overview-meta-face-spec)
  (face-spec-set 'e-chat-overview-summary-face
                 e-chat--overview-summary-face-spec))

(e-chat--refresh-face-specs)

(defvar-local e-chat-harness nil
  "Harness used by the current chat buffer.")

(defvar-local e-chat-harness-instance-id nil
  "Configured harness instance id used by the current chat buffer.")

(defvar-local e-chat-session-id nil
  "Session id used by the current chat buffer.")

(defvar-local e-chat-session-metadata nil
  "Detached bounded metadata used by this chat presentation surface.")

(defvar-local e-chat--session-readiness-work nil
  "Private session creation work retained by the current chat surface.")

(defconst e-chat--surface-readiness-spec
  (e-work-spec-create
   :id "chat-surface-readiness" :execution 'cooperative
   :interactive-policy 'async :owner 'e-chat
   :runner
   (lambda (parent arguments _context)
     (let ((children (plist-get arguments :children))
           cancel-wait)
       (setq cancel-wait
             (e-work-await-set
              children :mode 'all
              :on-settle
              (lambda (settled)
                (let ((failed
                       (seq-find
                        (lambda (child)
                          (memq (plist-get (e-work-status child) :state)
                                '(failed cancelled)))
                        (plist-get settled :done))))
                  (cond
                   ((null failed) (e-work-finish parent t))
                   ((eq (plist-get (e-work-status failed) :state) 'cancelled)
                    (e-work-cancel parent))
                   (t (e-work-fail parent
                                   (or (e-work-handle-error failed)
                                       '(e-work-error
                                         "chat readiness failed")))))))))
       ;; Cancelling a presentation wait detaches it.  Application work such
       ;; as a durable controller remains owned by its application service.
       (setf (e-work-handle-cancel-function parent)
             (lambda (_handle)
               (when cancel-wait (funcall cancel-wait))))
       :deferred)))
  "Join binding and application readiness for one chat surface.")

(defun e-chat--surface-readiness-start (binding-work application-work)
  "Return one readiness work joining BINDING-WORK and APPLICATION-WORK."
  (unless (e-work-handle-p binding-work)
    (signal 'wrong-type-argument (list 'e-work-handle-p binding-work)))
  (cond
   ((null application-work) binding-work)
   ((not (e-work-handle-p application-work))
    (signal 'wrong-type-argument (list 'e-work-handle-p application-work)))
   ((eq binding-work application-work) binding-work)
   (t
    (e-work-start e-chat--surface-readiness-spec
                  (list :children (list binding-work application-work))))))

(defvar-local e-chat-board-id nil
  "Board id owned by the current chat buffer's public interaction context.")

(defvar-local e-chat--event-subscription nil
  "Harness event subscription owned by this chat buffer.")

(defvar-local e-chat--board-run-set-unsubscribe nil
  "Unsubscribe function for this chat surface's Board run-set status.")

(defvar-local e-chat--board-hud-dismissed nil
  "Non-nil after this chat's automatically opened Board HUD is dismissed.")

(defvar-local e-chat--session-query-work nil
  "Request-scoped persistent SQLite chat-view work for this buffer.")

(defvar-local e-chat--context-query-work nil
  "Request-scoped SQLite context-preview work for this buffer.")

(defvar-local e-chat--session-query-generation 0
  "Presentation generation fencing persistent SQLite chat-view callbacks.")

(defvar-local e-chat--rendered-session-title nil
  "Session title currently rendered in the chat title block.")

(defconst e-chat--title "E Agent Session"
  "Title shown at the top of e chat buffers.")

(defconst e-chat--reasoning-effort-values
  '("" "minimal" "low" "medium" "high" "xhigh")
  "Reasoning effort values offered by the chat presentation.")

(defconst e-chat--new-context-session-label "+ New e chat session"
  "Picker label for creating a new chat session for context insertion.")

(defun e-chat--host-alt-leader-binding ()
  "Return a host-provided alternate leader key and map, when available."
  (let ((key (and (boundp 'doom-leader-alt-key)
                  (symbol-value 'doom-leader-alt-key)))
        (map (and (boundp 'doom-leader-map)
                  (symbol-value 'doom-leader-map))))
    (when (and (stringp key)
               (keymapp map))
      (cons key map))))

(defun e-chat--preserve-host-alt-leader (map)
  "Preserve a host-provided alternate leader prefix in MAP."
  (when-let* ((binding (e-chat--host-alt-leader-binding)))
    (define-key map (kbd (car binding)) (cdr binding))))

(defun e-chat--make-mode-map (&optional map)
  "Return MAP configured as the local keymap for `e-chat-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (set-keymap-parent map text-mode-map)
    (e-chat--preserve-host-alt-leader map)
    (define-key map (kbd "<escape>") #'e-chat-enter-response-navigation)
    (define-key map (kbd "C-p") #'e-chat-previous-line)
    (define-key map (kbd "<up>") #'e-chat-previous-line)
    (define-key map (kbd "M-o") #'e-chat-open-latest-response)
    (define-key map (kbd "M-y") #'e-chat-copy-latest-response)
    (define-key map (kbd "C-c C-c") #'e-chat-submit)
    (define-key map [mouse-2] #'e-chat-open-link)
    (define-key map (kbd "C-w") #'e-chat-kill-region-or-backward-word)
    (define-key map (kbd "RET") #'newline)
    (define-key map (kbd "!") #'e-chat-composer-bang)
    (define-key map (kbd "@") #'e-chat-composer-at)
    (define-key map (kbd "/") #'e-chat-composer-slash)
    (define-key map [remap delete-backward-char]
                #'e-chat-delete-backward-char)
    (define-key map [remap backward-delete-char-untabify]
                #'e-chat-delete-backward-char)
    (define-key map [remap delete-forward-char]
                #'e-chat-delete-forward-char)
    (define-key map [remap delete-char]
                #'e-chat-delete-forward-char)
    (define-key map (kbd "C-c C-k") #'e-chat-abort)
    (define-key map (kbd "C-c C-x") #'e-chat-show-context)
    (define-key map (kbd "C-c C-m") #'e-chat-compact-session)
    (define-key map (kbd "C-c C-y") #'e-chat-retry-session-view)
    map))

(defvar e-chat-mode-map (e-chat--make-mode-map)
  "Keymap for `e-chat-mode'.")

(defvar e-chat-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-i") #'e-chat-add-context-to-latest)
    (define-key map (kbd "s-I") #'e-chat-add-context-to-session)
    map)
  "Global keymap for adding Emacs buffer context to e chat composers.")

(defvar-local e-chat-context-mode-suppressed nil
  "Non-nil when `e-chat-context-mode' bindings are suppressed locally.")

(defun e-chat-context-mode-suppress-in-current-buffer (suppress)
  "Suppress `e-chat-context-mode' bindings in the current buffer when SUPPRESS.
This leaves the global minor mode enabled for every other buffer."
  (setq-local e-chat-context-mode-suppressed (and suppress t))
  (setq-local minor-mode-overriding-map-alist
              (assq-delete-all 'e-chat-context-mode
                               (copy-sequence
                                minor-mode-overriding-map-alist)))
  (when suppress
    (push (cons 'e-chat-context-mode nil)
          minor-mode-overriding-map-alist))
  e-chat-context-mode-suppressed)

(defun e-chat--configure-evil-context-bindings ()
  "Configure Evil normal-state bindings for `e-chat-context-mode-map'."
  (cond
   ((fboundp 'evil-define-key*)
    (funcall #'evil-define-key*
             'normal
             e-chat-context-mode-map
             (kbd "s-i")
             #'e-chat-add-context-to-latest)
    (funcall #'evil-define-key*
             'normal
             e-chat-context-mode-map
             (kbd "s-I")
             #'e-chat-add-context-to-session))
   ((fboundp 'evil-define-key)
    (eval
     '(progn
        (evil-define-key 'normal
          e-chat-context-mode-map
          (kbd "s-i")
          #'e-chat-add-context-to-latest)
        (evil-define-key 'normal
          e-chat-context-mode-map
          (kbd "s-I")
          #'e-chat-add-context-to-session))))))

(defun e-chat--make-composer-mode-map (&optional map)
  "Return MAP configured as the local keymap for composed chat input."
  (let ((map (or map (make-sparse-keymap))))
    (set-keymap-parent map e-chat-mode-map)
    (define-key map (kbd "<escape>") #'e-chat-composer-enter-navigation)
    map))

(defvar e-chat-composer-mode-map
  (e-chat--make-composer-mode-map)
  "Keymap for the editable pane of a composed e chat surface.")

(defun e-chat--configure-evil-composer-bindings ()
  "Configure the composer bindings that Evil would otherwise shadow.
Evil binds `C-w' as the window-command prefix in its insert and emacs
states, shadowing the composer's own binding.  Evil also consumes the first
Escape in insert state merely to enter normal state.  Define both commands on
the dedicated composer map in every relevant Evil state so one Escape crosses
the pane boundary and subsequent keys come from transcript navigation.
A no-op when Evil is absent."
  (when (fboundp 'evil-define-key*)
    (dolist (state '(insert emacs))
      (funcall #'evil-define-key*
               state
               e-chat-composer-mode-map
               (kbd "C-w")
               #'e-chat-kill-region-or-backward-word))
    (dolist (state '(insert normal emacs))
      (funcall #'evil-define-key*
               state
               e-chat-composer-mode-map
               (kbd "<escape>")
               #'e-chat-composer-enter-navigation))))

(defun e-chat--refresh-keymaps ()
  "Refresh chat keymaps after live reload."
  (e-chat-transcript-refresh-keymaps)
  (e-chat-overview-refresh-keymap #'e-chat-overview-open-session)
  (setq e-chat-mode-map (e-chat--make-mode-map e-chat-mode-map))
  (setq e-chat-composer-mode-map
        (e-chat--make-composer-mode-map e-chat-composer-mode-map))
  (e-chat-surface-refresh-command-map)
  (e-chat--configure-evil-composer-bindings))

(define-derived-mode e-chat-mode text-mode "e-chat"
  "Major mode for e chat buffers.
In the composer, leading ! captures command output, @ inserts file context,
and / expands available prompts."
  (e-chat-surface-setup-line-wrapping)
  (e-chat-surface-mark-transcript)
  (add-hook 'kill-buffer-hook #'e-chat--unsubscribe nil t)
  (add-hook 'kill-buffer-hook #'e-chat--unsubscribe-board-run-set nil t)
  (add-hook 'kill-buffer-hook #'e-chat--close-board-hud nil t)
  (add-hook 'e-chat-surface-after-display-hook
            #'e-chat--maybe-open-board-hud nil t)
  (add-hook 'kill-buffer-hook #'e-chat-surface-kill-composer nil t)
  (add-hook 'kill-buffer-hook #'e-chat-activity-stop-progress nil t)
  (add-hook 'kill-buffer-hook #'e-chat-composer-cancel-pending-references nil t)
  (add-hook 'kill-buffer-hook
            #'e-chat-composer-cancel-file-candidate-refresh nil t)
  (add-hook 'kill-buffer-hook #'e-chat--cancel-session-query-work nil t)
  (add-hook 'kill-buffer-hook
            #'e-chat-transcript-cancel-markdown-presentation nil t)
  (add-hook 'kill-buffer-hook
            #'e-chat-overview-remove-unread-buffer nil t)
  (add-hook 'evil-local-mode-hook #'e-chat-enforce-modal-editing-policy nil t)
  (add-hook 'after-change-functions
            #'e-chat-composer-mark-scroll-needed nil t)
  (add-hook 'after-change-functions
            #'e-chat-surface-mark-composer-layout-dirty nil t)
  (add-hook 'pre-command-hook #'e-chat-surface-pre-command nil t)
  (add-hook 'post-command-hook #'e-chat-surface-post-command nil t)
  (e-chat-surface-initialize))

;;;###autoload
(define-minor-mode e-chat-context-mode
  "Globally bind commands that add current buffer context to e chat."
  :global t
  :lighter " eCtx"
  :keymap e-chat-context-mode-map
  (e-chat--configure-evil-context-bindings))

(defun e-chat-enforce-modal-editing-policy ()
  "Disable modal editing when it is reactivated in chat buffers."
  (when (and (or (derived-mode-p 'e-chat-mode)
                 (derived-mode-p 'e-chat-overview-mode))
             (boundp 'evil-local-mode)
             evil-local-mode)
    (e-chat-composer-disable-modal-editing)))

(defun e-chat--configure-modal-editing-policy ()
  "Configure modal editors to keep `e-chat-mode' non-normal."
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'e-chat-mode 'emacs)
    (evil-set-initial-state 'e-chat-composer-mode 'insert)
    (evil-set-initial-state 'e-chat-overview-mode 'emacs)))

(e-chat--configure-modal-editing-policy)
(e-chat--configure-evil-composer-bindings)
(with-eval-after-load 'evil
  (e-chat--configure-modal-editing-policy)
  (e-chat--configure-evil-composer-bindings))

(defun e-chat--harness-has-capability-p (harness capability-id)
  "Return non-nil when HARNESS has active capability CAPABILITY-ID."
  (memq capability-id
        (mapcar #'e-capability-id
                (e-chat-service-active-capabilities harness))))

(defun e-chat--chat-instances ()
  "Return configured chat harness instances."
  (e-harness-instance-list :kind 'chat))

(defun e-chat--default-chat-instance ()
  "Return the configured default chat instance, or nil."
  (e-harness-instance-get e-chat-default-harness-id))

(defun e-chat--harness-for-instance (instance)
  "Return the live chat harness for INSTANCE."
  (let* ((instance-id (e-harness-instance-id instance))
         (harness
          (condition-case err
            (e-harness-instance-get-or-create instance-id)
          ((e-harness-instance-missing e-harness-registry-missing)
           (user-error "No e harness registered for %S"
                       (cadr err))))))
    (unless (e-chat--harness-has-capability-p harness 'chat-session)
      (user-error "Harness instance %S does not provide chat-session capability"
                  instance-id))
    harness))

(defun e-chat--default-harness ()
  "Return the configured default chat harness from the harness registry."
  (let* ((instance (e-chat--default-chat-instance))
         (harness
          (if instance
              (e-chat--harness-for-instance instance)
            (condition-case err
                (e-harness-registry-get-or-create e-chat-default-harness-id)
              (e-harness-registry-missing
               (user-error "No e harness registered for %S"
                           (cadr err)))))))
    (unless (e-chat--harness-has-capability-p harness 'chat-session)
      (user-error "Harness %S does not provide chat-session capability"
                  e-chat-default-harness-id))
    harness))

;; Public facade contracts used by embedding shells.  The implementation
;; helpers stay private to this composition root; presentation consumers must
;; not reach through the facade into those helpers.
(defun e-chat-default-harness ()
  "Return the configured default chat harness."
  (e-chat--default-harness))

(defun e-chat-chat-instances ()
  "Return configured chat harness instances."
  (e-chat--chat-instances))

(defun e-chat-harness-for-instance (instance)
  "Return the live chat harness represented by INSTANCE."
  (e-chat--harness-for-instance instance))

(defun e-chat-session-candidates-start (&optional harness)
  "Return immediately with work reading a bounded picker candidate page."
  (e-chat-overview-session-candidates-start harness))

(defun e-chat-session-buffer-for-context
    (harness session-id &optional instance-id)
  "Return the existing or newly named context BUFFER for SESSION-ID."
  (e-chat--session-buffer-for-context harness session-id instance-id))

(defun e-chat-ordered-completion-table (labels &optional category)
  "Return an order-preserving completion table for LABELS."
  (e-chat--ordered-completion-table labels category))

(defun e-chat-render-resume-preview (harness session)
  "Render the bounded resume preview for SESSION from HARNESS."
  (e-chat-overview-render-resume-preview harness session))

(defun e-chat-board-session-p (session)
  "Return non-nil when SESSION has board-native persistent identity."
  (e-chat-overview-board-session-p session))

(defun e-chat-short-session-id (session-id)
  "Return the compact display id for SESSION-ID."
  (e-chat-overview-short-session-id session-id))

(defun e-chat--instance-label (instance)
  "Return a completion label for chat harness INSTANCE."
  (format "%s  [%s]"
          (e-harness-instance-name instance)
          (e-harness-instance-id instance)))

(defun e-chat--read-chat-instance (instances)
  "Read and return one chat instance from INSTANCES."
  (let* ((labels (mapcar #'e-chat--instance-label instances))
         (selected (completing-read
                    "New e chat target: "
                    (e-chat--ordered-completion-table labels 'e-chat-instance)
                    nil
                    t))
         (index (cl-position selected labels :test #'equal)))
    (or (nth index instances)
        (user-error "No e chat target selected"))))

(defun e-chat--select-chat-instance (&optional prompt)
  "Return the chat instance for a new command, prompting when needed.
PROMPT forces completion even when only one/default instance exists."
  (let* ((instances (e-chat--chat-instances))
         (default (or (e-chat--default-chat-instance)
                      (e-harness-instance-default :kind 'chat))))
    (cond
     ((null instances) nil)
     ((or prompt (> (length instances) 1))
      (e-chat--read-chat-instance instances))
     (default)
     (t (car instances)))))

(defun e-chat--cancel-session-query-work ()
  "Cancel the request-scoped persistent SQLite view for this chat buffer."
  (when (and (e-work-handle-p e-chat--session-query-work)
             (not (e-request-terminal-p
                   (e-work-handle-lifecycle e-chat--session-query-work))))
    (e-work-cancel e-chat--session-query-work))
  (setq e-chat--session-query-work nil))

(defun e-chat--session-query-current-p (work generation harness session-id)
  "Return non-nil when WORK still owns this persistent view presentation."
  (and (eq work e-chat--session-query-work)
       (= generation e-chat--session-query-generation)
       (eq harness e-chat-harness)
       (equal session-id e-chat-session-id)))

(defun e-chat-session-summary-preview (session)
  "Return bounded summary text for SESSION metadata."
  (e-chat-transcript-session-summary-preview session))

(defun e-chat-session-replay-message-count (messages)
  "Return the bounded replay count for MESSAGES."
  (e-chat-transcript-session-replay-message-count messages))

(defun e-chat-validated-replay-limit (value option)
  "Validate positive replay LIMIT VALUE for OPTION."
  (e-chat-transcript-validated-replay-limit value option))

(defun e-chat--project-root (&optional directory)
  (e-chat-composer-project-root directory))

(defun e-chat-project-root (&optional directory)
  "Return the chat-compatible project root for DIRECTORY."
  (e-chat-composer-project-root directory))

(defun e-chat--session-metadata (&optional instance-id)
  "Return metadata for a chat session created from the current buffer."
  (let ((metadata (list :project-root (e-chat--project-root default-directory))))
    (when instance-id
      (setq metadata (plist-put metadata :harness-instance-id instance-id)))
    metadata))

(defun e-chat--start-session (harness &optional session-id instance-id metadata)
  "Preallocate and asynchronously start one HARNESS chat session.

Return detached identity plus the creation work.  Public attachment starts the
SQL binding immediately; the work settles after SQLite atomically commits the
session root, Board association, and owner participant."
  (let* ((session-id (or session-id (e-session-generate-id)))
         (metadata (or metadata (e-chat--session-metadata instance-id)))
         (work (e-chat-service-create-session-start
                :harness harness :id session-id :metadata metadata)))
    (list :id session-id :work work :metadata (copy-tree metadata t))))

(defun e-chat--short-session-id (session-id)
  "Return a compact SESSION-ID for display."
  (if (> (length session-id) 12)
      (substring session-id 0 12)
    session-id))

(defun e-chat--session-buffer-name (session-id &optional metadata)
  "Return the buffer name for SESSION-ID using detached METADATA."
  (format "*e-chat:%s*"
          (or (plist-get metadata :name)
              (plist-get metadata :title)
              (when-let* ((summary (plist-get metadata :summary)))
                (if (> (length summary) 25)
                    (concat (substring summary 0 25) "...")
                  summary))
              (e-chat--short-session-id session-id))))

(defun e-chat--session-buffer-name-fast (session-id)
  "Return a metadata-free buffer name for persistent SESSION-ID."
  (format "*e-chat:%s*" (e-chat--short-session-id session-id)))

(defun e-chat--matching-session-buffer-p
    (buffer session-id &optional harness instance-id)
  "Return non-nil when BUFFER is an e chat buffer for SESSION-ID.
When HARNESS or INSTANCE-ID is non-nil, require the buffer to match it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (eq major-mode 'e-chat-mode)
           (equal e-chat-session-id session-id)
           (not (e-chat-transcript-preview-p))
           (or (not harness)
               (eq e-chat-harness harness))
           (or (not instance-id)
               (eq e-chat-harness-instance-id instance-id))))))

(defun e-chat--find-session-buffer (session-id &optional harness instance-id)
  "Return an existing chat buffer for SESSION-ID.
When HARNESS or INSTANCE-ID is non-nil, require the buffer to match it.
Prefer visible matching buffers so duplicate hidden composers cannot steal
context insertions from the chat buffer the user is looking at."
  (e-workspace-find-buffer
   (lambda (buffer)
     (e-chat--matching-session-buffer-p
      buffer session-id harness instance-id))
   :prefer-visible t))

(defun e-chat--empty-composer-p (buffer)
  "Return non-nil when BUFFER has no composer draft text."
  (with-current-buffer buffer
    (or (not (e-chat-composer-active-p))
        (string-empty-p (e-chat-composer-text)))))

(defun e-chat--prune-duplicate-session-buffers
    (keeper session-id &optional harness instance-id)
  "Kill hidden empty duplicate chat buffers for SESSION-ID except KEEPER."
  (dolist (buffer (buffer-list))
    (when (and (not (eq buffer keeper))
               (e-chat--matching-session-buffer-p
                buffer session-id harness instance-id)
               (not (get-buffer-window buffer t))
               (e-chat--empty-composer-p buffer))
      (kill-buffer buffer))))

(defun e-chat--rename-buffer-for-session ()
  "Rename the current buffer from its attached session metadata."
  (when (and e-chat-harness e-chat-session-id)
    (rename-buffer
     (e-chat--session-buffer-name
      e-chat-session-id
      e-chat-session-metadata)
     t)
    (when-let* ((composer (e-chat-surface-composer-buffer)))
      (with-current-buffer composer
        (rename-buffer
         (format " *e-chat input:%s*"
                 (buffer-name (e-chat-surface-transcript-buffer)))
         t)))))

(defun e-chat--rename-buffer-for-query-state (state)
  "Rename the current persistent chat surface from detached query STATE."
  (when (and e-chat-session-id (listp state))
    (rename-buffer
     (format "*e-chat:%s*"
             (or (plist-get state :name)
                 (when-let* ((summary (plist-get state :summary)))
                   (if (> (length summary) 25)
                       (concat (substring summary 0 25) "...")
                     summary))
                 (e-chat--short-session-id e-chat-session-id)))
     t)
    (when-let* ((composer (e-chat-surface-composer-buffer)))
      (with-current-buffer composer
        (rename-buffer
         (format " *e-chat input:%s*"
                 (buffer-name (e-chat-surface-transcript-buffer)))
         t)))))

(defun e-chat--event-consumer (harness buffer)
  "Return the live board event consumer for HARNESS chat BUFFER."
  (lambda (event)
    (when (buffer-live-p buffer)
      (e-ui-work-schedule
       (e-ui-work-spec-create
        :id (format "chat_board_event_%s"
                    (or (plist-get event :turn-id) "board"))
        :description "Render one board-observed chat event."
        :owner 'board-observer
        :target-buffer buffer
        :key (list (plist-get event :turn-id)
                   (plist-get event :type)
                   (plist-get event :created-at))
        :generation 0
        :delay 0
        :coalesce nil
        ;; Chat rendering owns the composed transcript/composer viewport as a
        ;; unit.  Its render path preserves scrollback or follows output from
        ;; the window-local follow state; a generic one-buffer snapshot would
        ;; restore the old transcript point afterward and undo that decision.
        :focus-policy 'explicit
        :reentrancy-policy 'defer
        :apply
        (lambda (_job _handle)
          (when (and (buffer-live-p buffer)
                     (eq e-chat-harness harness))
            (with-current-buffer buffer
              (unless (and
                       (eq (plist-get event :type) 'assistant-delta)
                       (e-chat-activity-assistant-streaming-p)
                       (equal (e-chat-status) "streaming"))
                (e-chat--render-event event))))))))))

(defun e-chat--subscribe (harness buffer session-id)
  "Subscribe BUFFER to future board-observed events for SESSION-ID."
  (setq e-chat--event-subscription
        (e-chat-service-subscribe
         harness session-id (e-chat--event-consumer harness buffer))))

(defun e-chat--subscribe-from-cursor (harness buffer session-id cursor)
  "Subscribe BUFFER to canonical Board rows strictly after CURSOR."
  (setq e-chat--event-subscription
        (e-chat-service-subscribe-from-cursor
         harness session-id cursor (e-chat--event-consumer harness buffer))))

(defun e-chat--unsubscribe ()
  "Remove this buffer's board observer subscription."
  (when e-chat--event-subscription
    (e-chat-service-unsubscribe e-chat--event-subscription))
  (setq e-chat--event-subscription nil))

(defun e-chat--unsubscribe-board-run-set ()
  "Remove this buffer's Board run-set status subscription."
  (when (functionp e-chat--board-run-set-unsubscribe)
    (funcall e-chat--board-run-set-unsubscribe))
  (setq e-chat--board-run-set-unsubscribe nil)
  (e-chat-surface-clear-board-status))

(defun e-chat--close-board-hud ()
  "Close the visual Board owned by the chat buffer being killed."
  (when (fboundp 'e-board-activity-visual-close-for-owner)
    (e-board-activity-visual-close-for-owner (current-buffer))))

(defun e-chat--board-hud-on-dismiss (chat)
  "Return a callback that records CHAT's explicit HUD dismissal."
  (lambda ()
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (setq-local e-chat--board-hud-dismissed t)))))

(defun e-chat--board-status-action (chat target binding run-id)
  "Return CHAT's explicit activity action for TARGET, BINDING, and RUN-ID."
  (lambda ()
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (setq-local e-chat--board-hud-dismissed nil)))
    (require 'e-board-activity-visual-shell)
    (e-board-activity-visual-open-or-text
     target binding run-id nil chat (e-chat--board-hud-on-dismiss chat))))

(defun e-chat--maybe-open-board-hud ()
  "Open this displayed chat's visual Board HUD when its binding is ready."
  (when (and (not e-chat--board-hud-dismissed)
             (functionp e-chat--board-run-set-unsubscribe)
             (featurep 'xwidget-internal))
    (let* ((chat (current-buffer))
           (binding (e-chat-service-binding e-chat-harness e-chat-session-id))
           (window (get-buffer-window chat t)))
      (when (and (e-chat-service-binding-p binding)
                 (window-live-p window)
                 (eq (window-frame window) (selected-frame)))
        (require 'e-board-activity-visual-shell)
        (when (and (not (e-board-activity-visual-unavailable-reason))
                   (e-board-activity-shell--popup-available-p)
                   (not (e-board-activity-visual-visible-for-owner-p chat)))
          (condition-case error
              (with-selected-window window
                (e-board-activity-visual-open-buffer
                 :target (e-chat-service-publication-target binding)
                 :binding binding
                 :run-id (plist-get (e-chat-surface-board-status)
                                    :selected-run-id)
                 :owner-chat chat
                 :on-dismiss (e-chat--board-hud-on-dismiss chat)))
            (error
             (message "Board HUD could not open: %s"
                      (error-message-string error)))))))))

(defun e-chat--attach-board-run-set (binding)
  "Attach BINDING's compact Board run status to the current chat surface."
  (e-chat--unsubscribe-board-run-set)
  (when (e-chat-service-binding-p binding)
    (let ((target (e-chat-service-publication-target binding))
          (buffer (current-buffer)))
      (setq-local
       e-chat--board-run-set-unsubscribe
       (e-board-run-set-subscribe
        binding
        (lambda (status)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (e-chat-surface-set-board-status
               status
               (e-chat--board-status-action
                buffer target binding
                (plist-get status :selected-run-id))))))))
      (e-chat--maybe-open-board-hud))))

;;;###autoload
(defun e-chat-open-board-activity-text ()
  "Open the current session's Board activity in the native text renderer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (let* ((binding (e-chat-service-binding e-chat-harness e-chat-session-id))
         (status (e-chat-surface-board-status))
         (run-id (plist-get status :selected-run-id)))
    (unless (e-chat-service-binding-p binding)
      (user-error "The current chat has no ready Board activity binding"))
    (setq-local e-chat--board-hud-dismissed t)
    (when (fboundp 'e-board-activity-visual-close-for-owner)
      (e-board-activity-visual-close-for-owner (current-buffer)))
    (require 'e-board-activity-shell)
    (e-board-activity-shell-open-buffer
     :target (e-chat-service-publication-target binding)
     :run-id run-id)))

(defun e-chat--session-title ()
  "Return the current attached session title, or nil."
  (and e-chat-session-id
       (or (plist-get e-chat-session-metadata :name)
           (when-let* ((summary (plist-get e-chat-session-metadata :summary)))
             (if (> (length summary) 25)
                 (concat (substring summary 0 25) "...")
               summary))
           (plist-get e-chat-session-metadata :title))))

(defun e-chat--title-block-text ()
  "Return the current chat title block text."
  (let ((title (e-chat--session-title)))
    (if title
        (format "%s\n%s\n\n" e-chat--title title)
      (concat e-chat--title "\n\n"))))

(defun e-chat--clear (&optional _omit-composer)
  "Clear and initialize the current transcript buffer."
  (e-chat-composer-cancel-pending-references)
  (let ((inhibit-read-only t))
    (e-chat-transcript-cancel-markdown-presentation)
    (erase-buffer)
    ;; Activity reset runs before transcript reset because a live activity
    ;; record may still own running-status markers in the transcript.
    (e-chat-activity-reset)
    (e-chat-transcript-reset)
    (e-chat-activity-set-assistant-streaming nil)
    (setq e-chat--rendered-session-title (e-chat--session-title))
    (e-chat-transcript-insert-protected
     (e-chat--title-block-text)
     'e-chat-title-face)))

(defun e-chat-clear (&optional omit-composer)
  "Clear the current composed chat transcript."
  (e-chat--clear omit-composer))

(defun e-chat--clear-query-view ()
  "Initialize a persistent SQLite chat surface without durable reads."
  (e-chat-composer-cancel-pending-references)
  (let ((inhibit-read-only t))
    (e-chat-transcript-cancel-markdown-presentation)
    (erase-buffer)
    (e-chat-activity-reset)
    (e-chat-transcript-reset)
    (e-chat-activity-set-assistant-streaming nil)
    (setq e-chat--rendered-session-title nil)
    (e-chat-transcript-insert-protected
     (concat e-chat--title "\n\n")
     'e-chat-title-face)
    (e-chat-transcript-insert-protected
     (format "%s Loading recent messages...\n\n"
             (e-chat-transcript-system-glyph))
     'e-chat-activity-face)))

(defun e-chat--render-detached-query-window (metadata messages)
  "Render detached METADATA and bounded MESSAGES in the current chat.
METADATA and MESSAGES are either one settled query result or the bounded
optimistic values of a newly admitted session.  Neither is retained as a
durable session mirror."
  (setq-local e-chat-session-metadata (copy-tree metadata t))
  (e-chat--rename-buffer-for-query-state metadata)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (e-chat-transcript-insert-protected
     (concat e-chat--title "\n"
             (or (plist-get metadata :name)
                 (e-chat--short-session-id e-chat-session-id))
             "\n\n")
     'e-chat-title-face)
    (e-chat-transcript-render-visible-message-window messages))
  ;; Model and effort are part of this detached presentation projection.  They
  ;; do not depend on the later live Board-subscription handshake, and reading
  ;; them must not schedule a synchronous aggregate context reconstruction.
  (e-chat-surface-refresh-mode-line-status t))

(defun e-chat-prepare-transient-surface ()
  "Prepare a standalone composed surface for an embedding shell.
Reset owner projections without touching durable session state.  Embedding
shells use this facade operation instead of initializing chat owner registries
or transient state individually."
  (e-chat-composer-reset)
  (e-chat-transcript-reset)
  (e-chat-activity-reset)
  (e-chat-surface-set-status nil)
  (e-chat-transcript-set-preview-p nil)
  t)

(defun e-chat--rerender-transcript ()
  "Refresh the visible transcript from one detached bounded SQL query."
  (e-chat--cancel-session-query-work)
  (setq e-chat--session-query-generation
        (1+ e-chat--session-query-generation))
  (e-chat--start-session-query-view
   (current-buffer) e-chat-harness e-chat-session-id
   e-chat--session-query-generation))

(defun e-chat--title-block-end ()
  "Return the end position of the current title block."
  (save-excursion
    (goto-char (point-min))
    (or (and (search-forward "\n\n" nil t)
             (point))
        (point-min))))

(defun e-chat--refresh-title-block ()
  "Refresh the top title block from attached session metadata."
  (let ((inhibit-read-only t))
    (save-excursion
      (delete-region (point-min) (e-chat--title-block-end))
      (goto-char (point-min))
      (e-chat-transcript-insert-protected (e-chat--title-block-text)
                                          'e-chat-title-face))))

(defun e-chat--refresh-session-display ()
  "Refresh presentation surfaces derived from attached session metadata."
  (e-chat-profile-call
   'chat.refresh-session-display
   (list :session-id e-chat-session-id
         :buffer-name (buffer-name))
   (lambda ()
     (when (and e-chat-harness e-chat-session-id)
       (let ((title (e-chat--session-title)))
         (unless (equal title e-chat--rendered-session-title)
           (setq e-chat--rendered-session-title title)
           (e-chat--rename-buffer-for-session)
           (e-chat--refresh-title-block)
           (when (e-chat-surface-status)
             (e-chat-surface-set-status (e-chat-surface-status)))))))))

(defun e-chat--context-buffer-text (context session-id)
  "Return display text for CONTEXT belonging to SESSION-ID."
  (with-temp-buffer
    (insert (format "Session: %s\n\n" session-id))
    (insert "Options:\n")
    (pp (plist-get context :options) (current-buffer))
    (insert "\nMessages:\n")
    (pp (plist-get context :messages) (current-buffer))
    (buffer-string)))

(defun e-chat--display-context-buffer (context session-id)
  "Display CONTEXT for SESSION-ID in a read-only temp buffer."
  (let ((buffer (get-buffer-create e-chat-context-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (e-chat--context-buffer-text context session-id)))
      (special-mode)
      (goto-char (point-min)))
    (display-buffer buffer)
    buffer))

(defun e-chat--display-context-loading-buffer (session-id)
  "Display an empty read-only context preview for SESSION-ID immediately."
  (e-chat--display-context-buffer '(:options nil :messages nil) session-id))

(defun e-chat--optimistic-turn-options (field value)
  "Set FIELD to VALUE in this surface's bounded optimistic turn options."
  (let ((options
         (copy-tree (plist-get e-chat-session-metadata :turn-options) t)))
    (if (and (stringp value) (not (string-empty-p (string-trim value))))
        (setq options (plist-put options field (string-trim value)))
      (cl-remf options field))
    (setq-local e-chat-session-metadata
                (plist-put (copy-tree e-chat-session-metadata t)
                           :turn-options options))
    options))

(defun e-chat--optimistic-output-mode (mode)
  "Record MODE in this surface's bounded detached presentation metadata."
  (let* ((metadata (copy-tree e-chat-session-metadata t))
         (all (copy-tree (plist-get metadata :capability-state) t))
         (owner-key (e-session-metadata-owner-key 'chat-output-mode)))
    (setq all (plist-put all owner-key (and mode (list :mode mode))))
    (setq-local e-chat-session-metadata
                (plist-put metadata :capability-state all))))

(defun e-chat--merge-options (base overrides)
  "Return presentation option plist BASE with detached OVERRIDES applied."
  (let ((options (copy-tree base t))
        (remaining overrides))
    (while remaining
      (setq options (plist-put options (pop remaining) (pop remaining))))
    options))

(defun e-chat--event-may-change-unread-p (event)
  "Return non-nil when board EVENT can change this buffer's unread state."
  (pcase (plist-get event :type)
    ((or 'message-added 'message-updated)
     (eq (plist-get (plist-get (plist-get event :payload) :message) :role)
         'assistant))
    ('session-reset t)
    (_ nil)))

(defun e-chat-event-may-change-unread-p (event)
  "Return non-nil when EVENT may change a session's unread projection."
  (e-chat--event-may-change-unread-p event))

(defun e-chat-event-selected-participant-p (event)
  "Return non-nil when EVENT belongs to the selected chat participant."
  (e-chat-transcript-event-selected-participant-p event))

(defun e-chat-message-selected-participant-p (message)
  "Return non-nil when durable MESSAGE belongs to the selected participant."
  (e-chat-transcript-message-selected-participant-p message))

(defun e-chat-observed-turn-id (turn-id event)
  "Return the stable observed presentation id for TURN-ID and EVENT."
  (e-chat-transcript-observed-turn-id turn-id event))

(defun e-chat-presentation-turn-id (turn-id event)
  "Return the selected or isolated presentation id for TURN-ID and EVENT."
  (e-chat-transcript-presentation-turn-id turn-id event))

(defun e-chat--settle-successful-turn-presentation (_turn-id _ended-at)
  "Settle successful turn presentation.
Board-final output carries the successful terminal fact.  The later Board
turn-summary is detached summary data and does not repeat this transition."
  (e-chat-surface-refresh-mode-line-status t)
  (e-chat-overview-mark-selected-session-read)
  (e-chat-surface-set-status "done")
  (e-chat-composer-ensure)
  (e-chat-surface-refresh-composer-position))

(defun e-chat--render-event (event)
  "Render harness EVENT into the current chat buffer.
Activity transitions are delegated to the activity owner as one semantic
operation.  The facade retains only durable transcript and shell composition."
  (e-chat--profile-call
   'chat.render-event
   (list :session-id e-chat-session-id
         :turn-id (plist-get event :turn-id)
         :buffer-name (buffer-name)
     :metadata (list :event-type
                         (symbol-name (plist-get event :type))))
   (lambda ()
     (let ((activity-result (e-chat-activity-handle-event event)))
       (cond
        ((and (listp activity-result)
              (eq (plist-get activity-result :operation) 'message-added))
         ;; Activity owns message classification and detail preparation.  The
         ;; facade only composes the returned semantic result with the durable
         ;; transcript projection, then tells activity that the row exists.
         (when (plist-get activity-result :render-p)
           (let* ((message (plist-get (plist-get event :payload) :message))
                  (render-turn-id (plist-get activity-result :turn-id))
                  (assistant-p (plist-get activity-result :assistant-p))
                  (terminal-output-p
                   (plist-get activity-result :terminal-output-p))
                  (details-text (plist-get activity-result :details-text))
                  (output-tail-windows
                   (and assistant-p
                        (e-chat-surface-capture-live-output-follow-windows))))
             (e-chat--render-transcript-message
              message render-turn-id nil details-text)
             (e-chat-activity-message-rendered
              render-turn-id message terminal-output-p
              (plist-get event :created-at))
             (when terminal-output-p
               (e-chat--settle-successful-turn-presentation
                render-turn-id (plist-get event :created-at)))
             (when output-tail-windows
               (e-chat-surface-restore-output-tail-windows
                output-tail-windows))
             (when (and assistant-p
                        (stringp (plist-get message :id)))
               ;; The unread indicator needs only the newest visible response
               ;; identity.  Retain that bounded presentation scalar; SQLite
               ;; remains authoritative and the next chat-view query replaces
               ;; it.
               (setq-local
                e-chat-session-metadata
                (plist-put (copy-tree e-chat-session-metadata t)
                           :latest-assistant-marker
                           (plist-get message :id))))
             (when (and assistant-p
                        (e-chat-event-selected-participant-p event))
               (e-chat-overview-mark-selected-session-read))
             (when (member (plist-get message :role) '(user "user"))
               ;; Keep only the visible scalar needed by this surface.  The
               ;; canonical summary remains SQLite-owned; this optimistic
               ;; copy is replaced by the next bounded session query.
               (unless (or (plist-get e-chat-session-metadata :summary)
                           (not (stringp (plist-get message :content))))
                 (setq-local
                  e-chat-session-metadata
                  (plist-put (copy-tree e-chat-session-metadata t)
                             :summary (plist-get message :content))))
               (e-chat--refresh-session-display)))))
        ((eq activity-result :message-updated)
         ;; Activity has already refreshed its semantic details.  Transcript
         ;; owns only the durable visibility projection and decides whether a
         ;; local reconcile is sufficient.
         (let ((message (plist-get (plist-get event :payload) :message)))
           (unless (e-chat-transcript-reconcile-message-display message)
             (e-chat--rerender-transcript))))
        ((eq activity-result :settled)
         (e-chat--settle-successful-turn-presentation
          (plist-get event :turn-id)
          (plist-get event :created-at)))
        ((eq activity-result :session-reset)
         (e-chat--insert-transcript-entry "System" "Session reset" t))
        ((eq activity-result t)
         ;; Session-owned metadata such as a derived title may commit after
         ;; the Board user-message observer has rendered its row.  The later
         ;; selected turn boundary is the first presentation event that can
         ;; reliably refresh those committed projections without depending on
         ;; incidental timer ordering.
         (when (and (eq (plist-get event :type) 'turn-started)
                    (e-chat-event-selected-participant-p event))
           (e-chat--refresh-session-display)))
        (t
         (pcase (plist-get event :type)
           ('compaction-started
            (let ((payload (plist-get event :payload)))
              (e-chat--insert-transcript-entry
               "System"
               (cond
                ((eq (plist-get payload :reason) 'auto)
                 "Auto-compaction started")
                ((plist-get payload :active-turn)
                 "Agent compacting context mid-turn")
                (t
                 "Context compaction started"))
               t
               (plist-get event :turn-id))))
           ('compaction-prepared
            (let ((payload (plist-get event :payload)))
              (e-chat--insert-transcript-entry
               "System"
               (format "Compaction prepared; keeping from %s"
                       (or (plist-get payload :first-kept-entry-id) "boundary"))
               t
               (plist-get event :turn-id))))
           ('compaction-summary-started
            (when (e-chat-event-selected-participant-p event)
              (e-chat-surface-set-status "summarizing context")))
           ('compaction-finished
            (let ((payload (plist-get event :payload)))
              (when (e-chat-event-selected-participant-p event)
                (e-chat-surface-invalidate-mode-line-context-estimate)
                (e-chat-surface-set-status "compacted" t))
              (e-chat--insert-transcript-entry
               "System"
               (format "%s %s"
                       (if (eq (plist-get payload :reason) 'auto)
                           "Auto-compacted context into"
                         "Context compacted into")
                       (or (plist-get payload :compaction-id) "summary"))
               t
               (plist-get event :turn-id))
              (when (e-chat-event-selected-participant-p event)
                (e-chat-composer-ensure)
                (e-chat-surface-refresh-composer-position))))
           ('compaction-failed
            (when (e-chat-event-selected-participant-p event)
              (e-chat-surface-set-status "compaction failed"))
            (e-chat--insert-transcript-entry
             "System"
             (let ((payload (plist-get event :payload)))
               (format "%s: %s"
                       (if (eq (plist-get payload :reason) 'auto)
                           "Auto-compaction failed"
                         "Context compaction failed")
                       (or (plist-get payload :message)
                           "unknown error")))
             t
             (plist-get event :turn-id)))
           ('queue-changed
            (if (e-chat-surface-transcript-p)
                (e-chat-composer-insert-queued-prompts)
              (e-chat-composer-ensure)
              (e-chat-surface-refresh-composer-position)))
           ('session-reset
            ;; Activity normally claims this event and returns a session-reset
            ;; result.  Keep this branch for a future owner that elects to
            ;; leave durable reset composition to the facade.
            (e-chat--insert-transcript-entry "System" "Session reset" t))
           (_
            (e-chat--insert-transcript-entry
             "System" (format "Event: %S" event) t)))))
       (when (e-chat-event-may-change-unread-p event)
         (e-chat-overview-update-unread-cache))))))

(defun e-chat--live-session-buffer-p
    (buffer harness session-id instance-id)
  "Return non-nil when BUFFER is already live for the requested session."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'e-chat-mode)
              (eq e-chat-harness harness)
              (equal e-chat-session-id session-id)
              (eq e-chat-harness-instance-id instance-id)
              (e-chat-service-subscription-p e-chat--event-subscription)
              (e-chat-service-subscription-active-p
               e-chat--event-subscription)))))

(defun e-chat-render-event (event)
  "Render harness EVENT through the composed chat presentation facade."
  (e-chat--render-event event))

(cl-defun e-chat-open
    (&key harness session-id new-session instance-id on-session-read-error
          readiness-work application-readiness-work metadata)
  "Attach and return an e chat buffer.
HARNESS, SESSION-ID, and NEW-SESSION are injectable for presentation tests and
reload.  INSTANCE-ID identifies a configured harness instance.
ON-SESSION-READ-ERROR, when non-nil, receives an asynchronous bounded-read
condition after the chat buffer renders it.  READINESS-WORK is an already
started participant admission supplied by a public Board-open operation.
APPLICATION-READINESS-WORK optionally gates user submission on additional
application initialization without transferring ownership of that work to the
presentation.  METADATA is bounded optimistic header state.  User-facing
commands should call `e-chat-new' or `e-chat-resume'."
  (let* ((instance (and (not harness)
                        instance-id
                        (e-harness-instance-get instance-id)))
         (chat-harness (or harness
                           (and instance
                                (e-chat--harness-for-instance instance))
                           (e-chat--default-harness)))
         (chat-instance-id (or instance-id
                               (and instance
                                    (e-harness-instance-id instance))))
         (creating-p (or new-session (not session-id)))
         (chat-session-id (or session-id
                              (and creating-p (e-session-generate-id))))
         (session-metadata
          (or (and metadata (copy-tree metadata t))
              (and creating-p (e-chat--session-metadata chat-instance-id))))
         (creation-work
          (or readiness-work
              (when creating-p
                (e-chat-service-create-session-start
                 :harness chat-harness :id chat-session-id
                 :metadata session-metadata))))
         (buffer (or (e-chat--find-session-buffer
                      chat-session-id chat-harness chat-instance-id)
                     (get-buffer-create
                      (e-chat--session-buffer-name-fast chat-session-id)))))
    (unless (e-session-storage-sqlite-p
             (e-chat-service-session-store chat-harness))
      (signal 'e-session-storage-error
              (list "Public chat requires SQLite" chat-session-id)))
    (unless (e-chat--live-session-buffer-p
             buffer chat-harness chat-session-id chat-instance-id)
      (e-chat-attach-buffer
       buffer chat-harness chat-session-id chat-instance-id
       on-session-read-error session-metadata))
    (e-chat--prune-duplicate-session-buffers
     buffer chat-session-id chat-harness chat-instance-id)
    (when (or creation-work application-readiness-work)
      ;; Binding is the application-level readiness boundary.  Starting it
      ;; also starts the pending owner's atomic SQLite admission; merely
      ;; retaining the create handle leaves the durable operation unsubmitted.
      (let* ((binding-work
              (e-chat-service-binding-start
               chat-harness chat-session-id nil t))
             (surface-readiness
              (e-chat--surface-readiness-start
               binding-work application-readiness-work)))
        (with-current-buffer buffer
          (setq-local e-chat--session-readiness-work surface-readiness)
          (e-chat-surface-set-status "persistence pending" t))
        (e-work-on-settle
         binding-work
         (lambda (work)
           (let* ((status (e-work-status work))
                  (finished-p (eq (plist-get status :state) 'finished))
                  (binding (and finished-p (plist-get status :result)))
                  (current-p
                   (and (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (and (eq e-chat-harness chat-harness)
                               (equal e-chat-session-id chat-session-id))))))
             (if (not current-p)
                 (when binding
                   (e-chat-service-release-unobserved-binding binding))
               (with-current-buffer buffer
                 (if finished-p
                     (progn
                       (setq e-chat-board-id
                             (e-chat-service-binding-board-id binding))
                       ;; The binding commit establishes an exact empty window.
                       ;; Subscribe directly to SQLite changes; a new session
                       ;; has no historical page to query or reconstruct.
                       (unless e-chat--event-subscription
                         (e-chat--subscribe
                          chat-harness buffer chat-session-id))
                       (e-chat--attach-board-run-set binding)
                       (e-chat-surface-set-status "idle" t))
                   (let* ((error (plist-get status :error))
                          (upgrade
                           (e-chat--runtime-store-upgrade-required-message
                            error)))
                     (e-chat-surface-set-status
                      (or upgrade
                          (format "persistence suspect %s: %s"
                                  chat-session-id
                                  (e-work-error-message error)))
                      t))))))))))
    buffer))

(defun e-chat--runtime-store-condition (error symbol &optional depth)
  "Return nested ERROR condition SYMBOL within a bounded cause chain."
  (let ((depth (or depth 0)))
    (when (and (< depth 8) (consp error))
      (if (eq (car error) symbol)
          error
        (let* ((data (cdr error))
               (properties (if (stringp (car data)) (cdr data) data))
               (cause (and (listp properties)
                           (plist-get properties :cause))))
          (e-chat--runtime-store-condition cause symbol (1+ depth)))))))

(defun e-chat--runtime-store-upgrade-required-message (error)
  "Return actionable schema-upgrade text for ERROR, or nil."
  (when-let* ((condition
               (e-chat--runtime-store-condition
                error 'e-runtime-store-schema-too-old))
              (data (cdr condition)))
    (when (stringp (car data)) (setq data (cdr data)))
    (format "runtime store upgrade required (schema %s -> %s); quit Emacs, run scripts/e-runtime-upgrade, then restart"
            (or (plist-get data :actual) "old")
            (or (plist-get data :required) "current"))))

(cl-defun e-chat-create-session-start (&key harness metadata id)
  "Preallocate a chat session and return its detached id plus creation work."
  (e-chat--start-session
   (or harness (e-chat--default-harness)) id nil metadata))

(cl-defun e-chat-submit-session
    (harness session-id prompt &key references metadata)
  "Submit PROMPT with REFERENCES and METADATA to HARNESS SESSION-ID."
  (e-chat-service-submit-session
   harness session-id prompt
   :references references
   :metadata metadata))

(defun e-chat--active-turn-running-p ()
  "Return non-nil when the attached session has a running active turn."
  (and e-chat-harness
       e-chat-session-id
       (e-chat-service-active-turn-p e-chat-harness e-chat-session-id)))

(defun e-chat--harness-session-active-turn-p (harness session-id)
  "Return non-nil when HARNESS has a running active turn for SESSION-ID."
  (and (e-harness-p harness)
       session-id
       (e-chat-service-active-turn-p harness session-id)))

(defun e-chat--submit-intent (prefix)
  "Return submit intent for PREFIX in the current chat state."
  (if (not (e-chat--active-turn-running-p))
      'submit
    (if prefix 'queue 'steer)))

(defun e-chat--submit-metadata (mode references)
  "Return metadata for composer submission MODE and REFERENCES."
  (append (list :source 'chat-composer
                :submit-mode mode)
          (and references (list :references references))))

(defun e-chat--watch-admission (buffer work intent)
  "Surface failed or cancelled admission WORK for BUFFER and INTENT."
  (unless (e-work-handle-p work)
    (signal 'wrong-type-argument (list 'e-work-handle-p work)))
  (e-work-on-settle
   work
   (lambda (settled)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (let* ((status (e-work-status settled))
                (state (plist-get status :state)))
           (when (memq state '(failed cancelled))
             (e-chat-surface-set-status
              (format "%s admission %s"
                      intent (if (eq state 'cancelled) "cancelled" "failed"))
              t)
             (let ((message
                    (if (eq state 'cancelled)
                        "SQLite admission cancelled"
                      (e-work-error-message (plist-get status :error)))))
               (e-chat--render-event
                (list :type (if (eq state 'cancelled)
                                'turn-cancelled
                              'turn-failed)
                      :turn-id nil
                      :payload (list :admission t :error message)))))))))))

(defun e-chat--failed-turn-target-at-point ()
  "Return failed turn target at point in a chat buffer, or nil."
  (when (and (derived-mode-p 'e-chat-mode)
             e-chat-session-id)
    (let ((turn-id (e-chat-transcript-turn-id-at-point)))
      (when (and turn-id
                 (e-chat-activity-failed-turn-p turn-id))
        (list :session-id e-chat-session-id
              :turn-id turn-id
              :harness e-chat-harness
              :source 'point)))))

(defun e-chat--inspect-error-target (session-id turn-id)
  "Return an immediate inspect-error target for SESSION-ID TURN-ID.
Nil means the caller must query SQLite for the newest failure."
  (cond
   ((and session-id turn-id)
    (list :session-id session-id :turn-id turn-id :source 'explicit))
   ((and session-id (not turn-id))
    (signal 'e-context-inspection-invalid
            (list "e-inspect-error requires turn-id with session-id")))
   ((and turn-id (not session-id))
    (signal 'e-context-inspection-invalid
            (list "e-inspect-error requires session-id with turn-id")))
   (t
    (e-chat--failed-turn-target-at-point))))

(defun e-chat--inspect-error-title (detail)
  "Return an investigation session title for failure DETAIL."
  (let* ((session (plist-get detail :session))
         (turn (plist-get detail :turn))
         (terminal-error (plist-get detail :terminal-error))
         (error-text (or (plist-get terminal-error :error)
                         "failed turn")))
    (format "Inspect error: %s %s"
            (plist-get session :id)
            (or (plist-get turn :id)
                error-text))))

(defun e-chat--inspect-error-prompt (detail)
  "Return the seeded investigation prompt for failure DETAIL."
  (let* ((session (plist-get detail :session))
         (turn (plist-get detail :turn))
         (terminal-error (plist-get detail :terminal-error))
         (session-id (plist-get session :id))
         (turn-id (plist-get turn :id)))
    (with-temp-buffer
      (insert "Investigate this recent failed e turn.\n\n")
      (insert "Goals:\n")
      (insert "- Explain the likely root cause.\n")
      (insert "- Identify the strongest evidence in the attached timeline.\n")
      (insert "- Propose remediation and hardening steps.\n")
      (insert "- Do not retry, mutate, or edit the original failed session unless explicitly asked.\n\n")
      (insert (format "Failure target: session `%s`, turn `%s`.\n"
                      session-id turn-id))
      (when-let* ((project-root (plist-get session :project-root)))
        (insert (format "Project root: `%s`.\n" project-root)))
      (insert "\nTerminal error:\n")
      (insert "```elisp\n")
      (insert (pp-to-string terminal-error))
      (insert "```\n\n")
      (insert "Structured diagnostic context:\n")
      (insert "```elisp\n")
      (insert (pp-to-string detail))
      (insert "```\n")
      (buffer-string))))

(defun e-chat--inspect-error-reference (detail prompt)
  "Return a diagnostic reference for failure DETAIL and PROMPT body."
  (let* ((session (plist-get detail :session))
         (turn (plist-get detail :turn))
         (session-id (plist-get session :id))
         (turn-id (plist-get turn :id)))
    (list :uri (format "e://session/%s/turn/%s/failure"
                       session-id turn-id)
          :label (format "Failed turn %s/%s" session-id turn-id)
          :body prompt)))

(defun e-chat--inspect-error-open-from-detail (harness detail)
  "Open a new investigation chat in HARNESS from detached DETAIL."
  (let* ((source-session-id
          (plist-get (plist-get detail :session) :id))
         (source-turn-id (plist-get (plist-get detail :turn) :id))
         (title (e-chat--inspect-error-title detail))
         (metadata (list :name title
                         :source 'e-inspect-error
                         :source-session-id source-session-id
                         :source-turn-id source-turn-id))
         (new-session-id (e-session-generate-id))
         (prompt (e-chat--inspect-error-prompt detail))
         (reference (e-chat--inspect-error-reference detail prompt)))
    (e-chat-surface-pop-to-buffer
     (e-chat-open :harness harness :session-id new-session-id
                  :new-session t))
    (e-chat-submit-session
     harness new-session-id prompt
     :references (list reference)
     :metadata metadata)
    new-session-id))

(defun e-chat--inspect-error-run (parent arguments _context)
  "Run asynchronous error inspection for PARENT from ARGUMENTS."
  (let ((harness (plist-get arguments :harness))
        (target (plist-get arguments :target))
        current-child)
    (setf (e-work-handle-cancel-function parent)
          (lambda (_handle)
            (when (e-work-handle-p current-child)
              (e-work-cancel current-child))))
    (cl-labels
        ((parent-live-p ()
           (not (e-request-terminal-p (e-work-handle-lifecycle parent))))
         (settle-failure (settled)
           (when (parent-live-p)
             (pcase (plist-get (e-work-status settled) :state)
               ('failed (e-work-fail parent (e-work-handle-error settled)))
               ('cancelled (e-work-cancel parent)))))
         (inspect-target (resolved)
           (when (parent-live-p)
             (if (not resolved)
                 (e-work-fail
                  parent
                  (list 'e-context-inspection-invalid
                        "No failed e turns found"))
               (let ((source-harness (or (plist-get resolved :harness)
                                         harness)))
                 (setq current-child
                       (e-context-inspection-failure-detail-start
                        :harness source-harness
                        :session-id (plist-get resolved :session-id)
                        :turn-id (plist-get resolved :turn-id)))
                 (e-work-on-settle
                  current-child
                  (lambda (settled)
                    (if (eq (plist-get (e-work-status settled) :state)
                            'finished)
                        (when (parent-live-p)
                          (condition-case err
                              (e-work-finish
                               parent
                               (e-chat--inspect-error-open-from-detail
                                source-harness
                                (e-work-handle-result settled)))
                            ((error quit) (e-work-fail parent err))))
                      (settle-failure settled))))))))
         (settle-target-query (settled)
           (if (eq (plist-get (e-work-status settled) :state) 'finished)
               (inspect-target (car (e-work-handle-result settled)))
             (settle-failure settled))))
      (if target
          (inspect-target target)
        (setq current-child
              (e-context-inspection-recent-failures-start
               :harness harness :limit 1))
        (e-work-on-settle current-child #'settle-target-query)))
    :deferred))

(defconst e-chat--inspect-error-work-spec
  (e-work-spec-create
   :id "chat-inspect-error"
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'e-chat
   :runner #'e-chat--inspect-error-run)
  "Async public failed-turn inspection operation.")

;;;###autoload
(cl-defun e-inspect-error (&key session-id turn-id harness)
  "Start asynchronous work opening a chat to inspect a failed e turn.
Interactive use prefers a failed turn at point in chat buffers, otherwise the
newest failed turn from the default chat harness.  SESSION-ID, TURN-ID, and
HARNESS are internal test seams."
  (interactive)
  (let* ((harness (or harness (e-chat--default-harness)))
         (target (e-chat--inspect-error-target session-id turn-id)))
    (e-work-start e-chat--inspect-error-work-spec
                  (list :harness harness :target target))))

(defun e-chat-open-session (harness session-id &optional display instance-id)
  "Open HARNESS SESSION-ID and display it when DISPLAY is non-nil."
  (let ((buffer (e-chat-open :harness harness
                             :session-id session-id
                             :instance-id instance-id)))
    (when display
      (e-chat-surface-pop-to-buffer buffer))
    buffer))

(defun e-chat-overview-open-session ()
  "Open the overview selection and compose its read-marker update.
Overview owns only row selection; this facade command owns chat construction,
attachment, and the post-open overview refresh."
  (interactive)
  (let* ((target (e-chat-overview-session-selection))
         (harness (plist-get target :harness))
         (session-id (plist-get target :session-id))
         (instance-id (plist-get target :instance-id))
         (buffer (e-chat-open-session
                  harness session-id
                  (called-interactively-p 'interactive)
                  instance-id)))
    (e-chat-overview-mark-session-read
     harness
     (plist-get target :session)
     instance-id)
    (when (derived-mode-p 'e-chat-overview-mode)
      (e-chat-overview-refresh))
    buffer))

(defun e-chat--open-active-session-candidate (candidate)
  "Open picker CANDIDATE through the facade's session constructor."
  (e-chat-open-session
   (plist-get candidate :harness)
   (plist-get candidate :session-id)
   t
   (plist-get candidate :instance-id)))

(defun e-chat--show-active-sessions (candidates)
  "Open the active-session picker for detached bounded CANDIDATES."
  (let ((status-cache (make-hash-table :test #'equal)))
    (e-picker-open
     :name 'active-sessions
     :title "Active sessions"
     :candidates (lambda () candidates)
     :candidate-key #'e-chat-overview-active-session-candidate-key
     :candidate-line
     (lambda (candidate)
       (e-chat-overview-active-session-line candidate status-cache))
     :preview #'e-chat-overview-active-session-preview
     :refresh-candidate-after-preview t
     :initial-candidate-limit 15
     :candidate-limit-step 15
     :on-select #'e-chat--open-active-session-candidate
     :footer "RET open  C-g cancel"
     :width 0.72
     :height 0.68)))

(defun e-chat-active-sessions ()
  "Query and open a picker of active or recent chat sessions."
  (interactive)
  (let ((work (e-chat-session-candidates-start)))
    (e-work-on-settle
     work
     (lambda (settled)
       (let ((status (e-work-status settled)))
         (if (not (eq (plist-get status :state) 'finished))
             (message "Unable to query active e chat sessions: %s"
                      (e-work-error-message
                       (or (plist-get status :error)
                           '(e-work-cancelled "cancelled"))))
           (let ((candidates
                  (e-chat-overview-active-session-candidates
                   (plist-get status :result))))
             (if candidates
                 (e-chat--show-active-sessions candidates)
               (message "No e chat sessions to show")))))))
    work))

(cl-defun e-chat-open-board
    (board &key harness session-id metadata
           (participant-id nil participant-id-supplied-p)
           (pickup-selector nil pickup-selector-supplied-p)
           (observer-selector nil observer-selector-supplied-p)
           (default-tags nil default-tags-supplied-p)
           (default-to nil default-to-supplied-p)
           display instance-id)
  "Open BOARD as the public interaction context for one chat participant.
When SESSION-ID is nil, create a private execution session for the participant."
  (let* ((harness (or harness (e-chat--default-harness)))
         routing-arguments)
    (unless (e-session-storage-sqlite-p
             (e-chat-service-session-store harness))
      (signal 'e-session-storage-error
              (list "Public Board chat requires SQLite" session-id)))
    (when participant-id-supplied-p
      (setq routing-arguments
            (append routing-arguments (list :participant-id participant-id))))
    (when pickup-selector-supplied-p
      (setq routing-arguments
            (append routing-arguments (list :pickup-selector pickup-selector))))
    (when observer-selector-supplied-p
      (setq routing-arguments
            (append routing-arguments
                    (list :observer-selector observer-selector))))
    (when default-tags-supplied-p
      (setq routing-arguments
            (append routing-arguments (list :default-tags default-tags))))
    (when default-to-supplied-p
      (setq routing-arguments
            (append routing-arguments (list :default-to default-to))))
    (let* ((new-p (null session-id))
           (session-id (or session-id (e-session-generate-id)))
           (work
            (if new-p
                (progn
                  (unless (and (e-chat-service-binding-p board)
                               (e-chat-service-binding-sqlite-service board))
                    (signal 'wrong-type-argument
                            (list 'sqlite-chat-binding-p board)))
                  (apply #'e-chat-service-create-participant-start
                         board harness :id session-id :metadata metadata
                         routing-arguments))
              (apply #'e-chat-service-open-board-start
                     board harness session-id routing-arguments)))
           (buffer
            (e-chat-open
             :harness harness :session-id session-id
             :instance-id instance-id :readiness-work work
             :metadata (and new-p
                            (or metadata (list :id session-id))))))
      (when display
        (e-chat-surface-pop-to-buffer buffer))
      buffer)))

(defun e-chat--start-session-query-view
    (buffer harness session-id generation &optional on-session-read-error)
  "Start the detached persistent SQLite view for BUFFER.

The application operation owns three bounded reads.  This presentation
callback only checks its request generation, validates the already-composed
identity, and renders the detached visible message window once."
  (let ((work nil))
    (condition-case err
        (progn
          (setq work
                (e-session-async-chat-view
                 (e-chat-service-session-store harness)
                 session-id
                 :limit e-chat-session-replay-message-limit))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (setq e-chat--session-query-work work)))
          (e-work-on-settle
           work
           (lambda (settled)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (e-chat--session-query-current-p
                        work generation harness session-id)
                   (setq e-chat--session-query-work nil)
                   (pcase (plist-get (e-work-status settled) :state)
                     ('finished
                      (let* ((result (plist-get (e-work-status settled) :result))
                             (metadata (plist-get result :metadata))
                             (association (plist-get result :association)))
                        ;; The application operation has already checked all
                        ;; child shapes and matching identities.  Retain only
                        ;; presentation identity, never a durable-state mirror.
                        (setq e-chat-board-id
                              (plist-get association :board-id))
                        (e-chat--render-detached-query-window
                         metadata (plist-get result :messages))
                        (let ((binding-work
                               (e-chat-service-binding-start
                                harness session-id association)))
                          (setq e-chat--session-readiness-work binding-work)
                          (e-chat-surface-set-status "connecting board" nil)
                          (e-work-on-settle
                           binding-work
                           (lambda (binding-settled)
                             (let* ((binding-status
                                     (e-work-status binding-settled))
                                    (finished-p
                                     (eq (plist-get binding-status :state)
                                         'finished))
                                    (binding
                                     (and finished-p
                                          (plist-get binding-status :result)))
                                    (current-p
                                     (and
                                      (buffer-live-p buffer)
                                      (with-current-buffer buffer
                                        (and
                                         (= generation
                                            e-chat--session-query-generation)
                                         (eq harness e-chat-harness)
                                         (equal session-id
                                                e-chat-session-id))))))
                               (if (not current-p)
                                   (when binding
                                     (e-chat-service-release-unobserved-binding
                                      binding))
                                 (with-current-buffer buffer
                                   (pcase (plist-get binding-status :state)
                                     ('finished
                                      (unless e-chat--event-subscription
                                        (e-chat--subscribe-from-cursor
                                         harness buffer session-id
                                         (plist-get result :cursor)))
                                      (e-chat--attach-board-run-set binding)
                                      (e-chat-surface-set-status "idle" t))
                                     ((or 'failed 'cancelled)
                                      (e-chat-surface-set-status
                                       "board read failed (retry available)"
                                       nil)))))))))))
                     ((or 'failed 'cancelled)
                      (let* ((error (plist-get (e-work-status settled) :error))
                             (upgrade
                              (e-chat--runtime-store-upgrade-required-message
                               error)))
                        (e-chat-surface-set-status
                         (or upgrade
                             "session read failed (retry available)") nil)
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (e-chat-transcript-insert-protected
                         (format "%s %s\n\n"
                                 (e-chat-transcript-system-glyph)
                                 (or upgrade
                                     "Unable to load recent messages; retry the session view."))
                         'e-chat-error-face))
                      (when on-session-read-error
                        (funcall on-session-read-error error)))))))))))
      (error
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (= generation e-chat--session-query-generation)
             (setq e-chat--session-query-work nil)
             (e-chat-surface-set-status
              (or (e-chat--runtime-store-upgrade-required-message err)
                  "session read failed (retry available)") nil)
             (when on-session-read-error
               (funcall on-session-read-error err)))))))
    work))

(defun e-chat-retry-session-view ()
  "Retry the request-scoped persistent SQLite view in the current chat."
  (interactive)
  (unless (and (derived-mode-p 'e-chat-mode)
               e-chat-harness e-chat-session-id
               (e-session-storage-sqlite-p
                (e-chat-service-session-store e-chat-harness)))
    (user-error "Current chat does not use a persistent SQLite session view"))
  (e-chat--cancel-session-query-work)
  (setq e-chat--session-query-generation
        (1+ e-chat--session-query-generation))
  (e-chat--clear-query-view)
  (e-chat-surface-set-status "loading session" nil)
  (e-chat--start-session-query-view
   (current-buffer) e-chat-harness e-chat-session-id
   e-chat--session-query-generation))

(defun e-chat-attach-buffer
    (buffer harness session-id &optional instance-id on-session-read-error
            new-session-metadata)
  "Attach BUFFER to HARNESS and SESSION-ID.
INSTANCE-ID identifies the configured harness instance.
ON-SESSION-READ-ERROR receives any asynchronous bounded-read failure.
NEW-SESSION-METADATA, when non-nil, is the bounded optimistic presentation
state for a newly admitted persistent session whose exact message set is
empty."
  (unless (e-session-storage-sqlite-p
           (e-chat-service-session-store harness))
    (signal 'e-session-storage-error
            (list "Public chat attachment requires SQLite" session-id)))
  (let ((binding (e-chat-service-binding harness session-id)))
  (with-current-buffer buffer
    (let* ((output-tail-windows
            (e-chat-surface-capture-output-tail-windows))
           (same-session
            (and (eq e-chat-harness harness)
                 (equal e-chat-session-id session-id)
                 (eq e-chat-harness-instance-id instance-id)))
           (previous-surface-composer (e-chat-surface-composer-buffer))
           (surface-composer
            (and same-session previous-surface-composer))
          (existing-workspace (e-buffer-workspace buffer)))
      (e-chat--cancel-session-query-work)
      (e-chat--unsubscribe)
      ;; Reattachment replaces the transcript from the bounded service view
      ;; below.  Drop the stale presentation before changing major mode: a
      ;; globalized minor mode may otherwise tear itself down by scanning the
      ;; entire old transcript during `kill-all-local-variables'.  In
      ;; particular, emojify removes its text properties with a whole-buffer
      ;; walk, making a resumed large Daily appear hung before replay starts.
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)
            (buffer-undo-list t))
        (erase-buffer))
      (e-chat-mode)
      (e-chat-transcript-set-rerender-function #'e-chat--rerender-transcript)
      (e-chat-composer-disable-modal-editing)
      (e-chat-composer-disable-completion)
      (e-chat-surface-initialize)
      ;; Publish the unloaded state before installing the session identity.
      ;; The loading header must not derive session-owned mode-line data and
      ;; start a competing synchronous lazy load while cooperative replay is
      ;; active.
      (e-chat-surface-set-status "loading session" nil)
      (setq-local e-current-harness harness)
      (setq-local e-chat-harness harness)
      (setq-local e-chat-harness-instance-id instance-id)
      (setq-local e-chat-session-id session-id)
      (setq-local e-chat-board-id
                  (and binding (e-chat-service-binding-board-id binding)))
      (e-chat-transcript-set-preview-p nil)
      (e-buffer-set-workspace
       buffer
       (if e-workspace-rebind-shell-on-open
           (e-workspace-current)
         (or existing-workspace
             (e-workspace-current))))
      (when (and (buffer-live-p previous-surface-composer)
                 (not same-session))
        (e-chat-surface-kill-composer))
      (when (buffer-live-p surface-composer)
        (e-chat-surface-bind-composer surface-composer buffer))
      (e-chat-composer-ensure)
      (e-chat--rename-buffer-for-query-state nil)
      (cond
       (new-session-metadata
        ;; Creation owns an exact empty message set until its first mutation.
        ;; Render that bounded optimistic state directly: querying the
        ;; independent read connection here can legitimately race before the
        ;; enqueued session-create transaction commits.
        (e-chat--clear-query-view)
        (e-chat--render-detached-query-window new-session-metadata nil))
       (t
        (let ((inhibit-read-only t))
          (e-chat--clear-query-view))
        (setq e-chat--session-query-generation
              (1+ e-chat--session-query-generation))
        (e-chat--start-session-query-view
         buffer harness session-id e-chat--session-query-generation
         on-session-read-error)))
      ;; The transcript no longer has an editable composer tail.  Protect it
      ;; as a whole so an early Escape or any unbound editing key cannot make
      ;; arbitrary text part of the rendered conversation.
      (setq-local buffer-read-only t)
      (e-chat-surface-restore-output-tail-windows
       output-tail-windows)
      (e-chat-surface-refresh-visible-windows)))
    buffer))

(defun e-chat-reload-buffers ()
  "Refresh live e chat buffers after development reload."
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (derived-mode-p 'e-chat-mode)
            (let ((session-id e-chat-session-id))
              (when (and session-id
                         (e-harness-p e-chat-harness)
                         (e-chat-service-board-session-p
                          e-chat-harness session-id))
                (let* ((retained-endpoint-p
                        (or (e-chat-service-binding
                             e-chat-harness session-id)
                            (e-chat--harness-session-active-turn-p
                             e-chat-harness session-id)))
                       (candidate
                        (condition-case err
                            (if e-chat-harness-instance-id
                                (e-chat--harness-for-instance
                                 (e-harness-instance-get
                                  e-chat-harness-instance-id))
                              (e-chat--default-harness))
                          (user-error
                           (if (e-harness-p e-chat-harness)
                               e-chat-harness
                             (signal (car err) (cdr err))))))
                       (harness
                        (cond
                         (retained-endpoint-p
                         (unless (eq candidate e-chat-harness)
                            (e-harness-request-runtime-refresh
                             e-chat-harness candidate))
                          e-chat-harness)
                         ((e-chat-service-board-session-p candidate session-id)
                          candidate)
                         (t e-chat-harness))))
                  (setq count (1+ count))
                  (e-chat-attach-buffer
                   buffer harness session-id
                   e-chat-harness-instance-id))))))))
    (when (called-interactively-p 'interactive)
      (message "Refreshed %d e chat buffer%s"
               count
               (if (= count 1) "" "s")))
    count))

;;;###autoload
(defun e-chat ()
  "Open a new persisted e chat session."
  (interactive)
  (e-chat-new))

;;;###autoload
(defun e-chat-new (&optional pop-to-side)
  "Create and open a new persisted e chat session.
With prefix argument POP-TO-SIDE, prompt for the chat target and use the pop
display path."
  (interactive "P")
  (let* ((instance (e-chat--select-chat-instance pop-to-side))
         (buffer (e-chat-open
                  :new-session t
                  :instance-id (and instance
                                    (e-harness-instance-id instance)))))
    (when (called-interactively-p 'interactive)
      (if pop-to-side
          (e-chat-surface-pop-to-buffer buffer)
        (e-chat-surface-switch-to-buffer buffer)))
    buffer))

(defun e-chat-session-choice-label (session)
  "Return the public completion label for SESSION metadata."
  (e-chat-overview-session-choice-label session))

(defun e-chat--ordered-completion-table (labels &optional category)
  "Return a completion table for LABELS that preserves caller order.
CATEGORY is exposed through completion metadata when non-nil."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata
          ,@(when category `((category . ,category)))
          (display-sort-function . identity)
          (cycle-sort-function . identity))
      (complete-with-action action labels string predicate))))

(defun e-chat--context-session-target (candidates)
  "Return a selected context insertion target from detached CANDIDATES."
  (let* ((default-instance (or (e-chat--default-chat-instance)
                               (e-harness-instance-default :kind 'chat)))
         (default-harness (if default-instance
                              (e-chat--harness-for-instance default-instance)
                            (e-chat--default-harness)))
         (new-target (list :harness default-harness
                           :instance-id (and default-instance
                                             (e-harness-instance-id
                                              default-instance))))
         (labels (cons e-chat--new-context-session-label
                       (mapcar (lambda (candidate)
                                 (e-chat-overview-session-candidate-label
                                  candidate
                                  (> (length (e-chat--chat-instances)) 1)))
                               candidates)))
         (selected (completing-read "Add context to e session: "
                                    (e-chat--ordered-completion-table
                                     labels
                                     'e-chat-session)
                                    nil
                                    t)))
    (if (equal selected e-chat--new-context-session-label)
        (let ((session-id (e-session-generate-id)))
          (e-chat-open :harness default-harness :session-id session-id
                       :instance-id (plist-get new-target :instance-id)
                       :new-session t)
          (append new-target (list :session-id session-id :new-session t)))
      (let ((candidate
             (when-let* ((index (cl-position selected (cdr labels)
                                      :test #'equal)))
               (nth index candidates))))
        (unless candidate
          (user-error "No e chat session selected"))
        candidate))))

(defun e-chat--session-buffer-for-context
    (harness session-id &optional instance-id)
  "Return the chat buffer for HARNESS SESSION-ID, preserving drafts when live."
  (or (e-chat--find-session-buffer session-id harness instance-id)
      (e-chat-open :harness harness
                   :session-id session-id
                   :instance-id instance-id)))

(defun e-chat--visible-session-buffer (harness)
  "Return a visible chat buffer for HARNESS, preferring selected window order."
  (let* ((selected (selected-window))
         (windows (window-list nil 'no-minibuf))
         (windows (if (memq selected windows)
                      (cons selected (delq selected windows))
                    windows)))
    (catch 'buffer
      (dolist (window windows)
        (let ((buffer (window-buffer window)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (and (derived-mode-p 'e-chat-mode)
                         (eq e-chat-harness harness)
                         e-chat-session-id)
                (throw 'buffer buffer)))))))))

(defun e-chat--visible-chat-buffer ()
  "Return a visible chat buffer, preferring selected window order."
  (let* ((selected (selected-window))
         (windows (window-list nil 'no-minibuf))
         (windows (if (memq selected windows)
                      (cons selected (delq selected windows))
                    windows)))
    (catch 'buffer
      (dolist (window windows)
        (let ((buffer (window-buffer window)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (and (derived-mode-p 'e-chat-mode)
                         e-chat-harness
                         e-chat-session-id)
                (throw 'buffer buffer)))))))))

(defun e-chat--default-context-target (candidates)
  "Return visible or newest detached context target from CANDIDATES."
  (if-let* ((buffer (e-chat--visible-chat-buffer)))
      (with-current-buffer buffer
        (list :harness e-chat-harness
              :instance-id e-chat-harness-instance-id
              :session-id e-chat-session-id))
    (let ((candidate (car candidates)))
      (if candidate
          candidate
        (let* ((instance (or (e-chat--default-chat-instance)
                             (e-harness-instance-default :kind 'chat)))
               (harness (if instance
                            (e-chat--harness-for-instance instance)
                          (e-chat--default-harness)))
               (session-id (e-session-generate-id)))
          (e-chat-open :harness harness :session-id session-id
                       :instance-id (and instance
                                         (e-harness-instance-id instance))
                       :new-session t)
          (list :harness harness
                :instance-id (and instance
                                  (e-harness-instance-id instance))
                :session-id session-id :new-session t))))))

(defconst e-chat--context-display-action
  '(display-buffer-reuse-window
    display-buffer-below-selected
    display-buffer-pop-up-window)
  "Display action used when source-buffer context insertion reveals a chat.")

(defun e-chat-add-context-reference-to-session
    (reference harness session-id &optional display instance-id source-workspace)
  "Insert REFERENCE into HARNESS SESSION-ID composer.
When DISPLAY is non-nil, show the target chat buffer.  SOURCE-WORKSPACE, when
non-nil, is the workspace that should receive the display for this context
operation."
  (let ((buffer (e-chat--session-buffer-for-context
                 harness session-id instance-id)))
    (with-current-buffer buffer
      (e-chat-composer-enter-input-state)
      (e-chat-composer-insert-context-reference reference)
      (e-chat-surface-show-composer))
    (when display
      (e-chat-surface-pop-to-buffer
       buffer
       (or source-workspace (e-workspace-current))
       e-chat--context-display-action))
    buffer))

;;;###autoload
(defun e-chat-resume ()
  "Resume a recent persisted e chat session."
  (interactive)
  (let* ((display (called-interactively-p 'interactive))
         (page-work (e-chat-session-candidates-start))
         result)
    (e-work-on-settle
     page-work
     (lambda (settled)
       (let ((status (e-work-status settled)))
         (if (not (eq (plist-get status :state) 'finished))
             (message "Unable to query e chat sessions: %s"
                      (e-work-error-message
                       (or (plist-get status :error)
                           '(e-work-cancelled "cancelled"))))
           (let ((candidates (plist-get status :result)))
             (if (null candidates)
                 (message "No e chat sessions to resume")
               (let* ((candidate
                       (e-chat-overview-read-session-candidate candidates))
                      (buffer
                       (e-chat-open
                        :harness (plist-get candidate :harness)
                        :session-id (plist-get candidate :session-id)
                        :instance-id (plist-get candidate :instance-id))))
                 (setq result buffer)
                 (when display
                   (e-chat-surface-pop-to-buffer buffer)))))))))
    (or result page-work)))

;;;###autoload
(defun e-chat-switch-session ()
  "Switch to a recent persisted e chat session."
  (interactive)
  (e-chat-resume))

(defun e-chat-add-context-to-latest ()
  "Add current point or region to a visible, or latest, e chat session."
  (interactive)
  (let* ((source-workspace (e-workspace-current))
         (reference (e-chat-composer-capture-context-reference-for-command))
         (display (called-interactively-p 'interactive)))
    (if-let* ((buffer (e-chat--visible-chat-buffer)))
        (with-current-buffer buffer
          (e-chat-add-context-reference-to-session
           reference e-chat-harness e-chat-session-id display
           e-chat-harness-instance-id source-workspace))
      (let ((page-work (e-chat-session-candidates-start)))
        (e-work-on-settle
         page-work
         (lambda (settled)
           (let ((status (e-work-status settled)))
             (if (not (eq (plist-get status :state) 'finished))
                 (message "Unable to query e chat sessions: %s"
                          (e-work-error-message
                           (or (plist-get status :error)
                               '(e-work-cancelled "cancelled"))))
               (let ((target
                      (e-chat--default-context-target
                       (plist-get status :result))))
                 (e-chat-add-context-reference-to-session
                  reference
                  (plist-get target :harness)
                  (plist-get target :session-id)
                  display
                  (plist-get target :instance-id)
                  source-workspace))))))
        page-work))))

;;;###autoload
(defun e-chat-add-context-to-session ()
  "Add the current point or active region to a selected e chat session."
  (interactive)
  (let* ((source-workspace (e-workspace-current))
         (reference (e-chat-composer-capture-context-reference-for-command))
         (display (called-interactively-p 'interactive))
         (page-work (e-chat-session-candidates-start)))
    (e-work-on-settle
     page-work
     (lambda (settled)
       (let ((status (e-work-status settled)))
         (if (not (eq (plist-get status :state) 'finished))
             (message "Unable to query e chat sessions: %s"
                      (e-work-error-message
                       (or (plist-get status :error)
                           '(e-work-cancelled "cancelled"))))
           (let ((target
                  (e-chat--context-session-target
                   (plist-get status :result))))
             (e-chat-add-context-reference-to-session
              reference
              (plist-get target :harness)
              (plist-get target :session-id)
              display
              (plist-get target :instance-id)
              source-workspace))))))
    page-work))

;;;###autoload
(defun e-chat-rename (name)
  "Rename the current e chat session to NAME."
  (interactive
   (let ((current (and e-chat-session-id
                       (plist-get e-chat-session-metadata :name))))
     (list (read-string "Session name: " current))))
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat-session-rename e-chat-harness e-chat-session-id name)
  (setq-local e-chat-session-metadata
              (plist-put (copy-tree e-chat-session-metadata t) :name name))
  (e-chat--rename-buffer-for-session)
  (e-chat--refresh-title-block)
  (e-chat-surface-set-status "idle" t)
  (current-buffer))

;;;###autoload
(defun e-chat-set-model (model)
  "Set MODEL for the current chat session."
  (interactive
   (let* ((options (and e-chat-harness
                       e-chat-session-id
                       (ignore-errors
                         (e-chat-service-session-options
                          e-chat-harness
                          e-chat-session-id))))
          (current (plist-get options :model)))
     (list (read-string "Model: " current nil current))))
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (let ((work
         (e-chat-session-set-options
          e-chat-harness e-chat-session-id
          (e-chat--optimistic-turn-options :model model))))
  (e-chat-surface-set-status "idle" t)
    (message "Set e chat model to %s"
             (if (string-empty-p model) "default" model))
    work))

;;;###autoload
(defun e-chat-set-effort (effort)
  "Set reasoning EFFORT for the current chat session."
  (interactive
   (let* ((options (and e-chat-harness
                       e-chat-session-id
                       (ignore-errors
                         (e-chat-service-session-options
                          e-chat-harness
                          e-chat-session-id))))
          (current (or (plist-get options :reasoning-effort) "")))
     (list (completing-read "Reasoning effort: "
                            e-chat--reasoning-effort-values
                            nil
                            t
                            nil
                            nil
                            current))))
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (let ((work
         (e-chat-session-set-options
          e-chat-harness e-chat-session-id
          (e-chat--optimistic-turn-options :reasoning-effort effort))))
  (e-chat-surface-set-status "idle" t)
    (message "Set e chat effort to %s"
             (if (string-empty-p effort) "default" effort))
    work))

;;;###autoload
(defun e-chat-set-output-mode (mode)
  "Set the assistant output markup MODE for the current chat session.
Choosing the empty selection clears the per-session override, falling back to
the configured global/project default.  Re-renders already-visible responses so
the transcript matches the new mode immediately."
  (interactive
   (let* ((current (and e-chat-harness e-chat-session-id
                        (e-chat-output-mode-resolve
                         e-chat-harness e-chat-session-id)))
          (choices (mapcar #'symbol-name e-chat-output-mode-values))
          (choice (completing-read
                   (format "Output mode (current %s, empty to clear override): "
                           (or current 'markdown))
                   choices nil t)))
     (list (and (not (string-empty-p choice)) (intern choice)))))
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (let ((work (e-chat-output-mode-session-set
               e-chat-harness e-chat-session-id mode)))
    ;; Rendering needs the user's chosen mode immediately, while SQLite owns
    ;; the durable value and settles independently.
    (e-chat--optimistic-output-mode mode)
    (e-chat-transcript-rerender-assistant-blocks)
    (e-chat-surface-set-status "idle" t)
    (message "Set e chat output mode to %s"
             (or mode
                 (format "default (%s)"
                         (e-chat-output-mode-resolve
                          e-chat-harness e-chat-session-id nil
                          e-chat-session-metadata))))
    work))

;;;###autoload
(defun e-chat-show-context ()
  "Show the current chat session context in a read-only temp buffer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (let* ((harness e-chat-harness)
         (session-id e-chat-session-id)
         (buffer (e-chat--display-context-loading-buffer session-id))
         (work
          (e-session-async-context-path
           (e-chat-service-session-store harness)
           session-id)))
    (with-current-buffer buffer
      (when (and (e-work-handle-p e-chat--context-query-work)
                 (not (e-request-terminal-p
                       (e-work-handle-lifecycle e-chat--context-query-work))))
        (e-work-cancel e-chat--context-query-work))
      (setq-local e-chat--context-query-work work))
    (e-work-on-settle
     work
     (lambda (settled)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (eq work e-chat--context-query-work)
             (setq e-chat--context-query-work nil)
             (pcase (plist-get (e-work-status settled) :state)
               ('finished
                (let ((result (plist-get (e-work-status settled) :result)))
                  (e-chat--display-context-buffer
                   (list :options
                         (e-chat--merge-options
                          (e-harness-default-options harness)
                          (plist-get result :turn-options))
                         :messages (plist-get result :messages))
                   session-id)))
               ((or 'failed 'cancelled)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert (format "Session: %s\n\nUnable to load context: %s\n"
                                  session-id
                                  (e-work-error-message
                                   (or (plist-get (e-work-status settled) :error)
                                       '(e-work-cancelled "cancelled")))))
                  (special-mode)))))))))
    buffer))

;;;###autoload
(defun e-chat-compact-session (&optional instructions)
  "Compact the current chat session transcript."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat-session-compact-start
   e-chat-harness
   e-chat-session-id
   :instructions instructions))

;;;###autoload
(defun e-chat--require-board-readiness ()
  "Reject a turn until the exact Board binding is durably ready.
The composer remains untouched, so the user can retry after the detached
initial run-set projection settles."
  (when-let* ((work e-chat--session-readiness-work))
    (let ((state (plist-get (e-work-status work) :state)))
      (pcase state
        ('finished nil)
        ((or 'failed 'cancelled)
         (signal (or (car (e-work-handle-error work))
                     'e-chat-service-error)
                 (or (cdr (e-work-handle-error work))
                     (list "Board readiness failed"))))
        (_
         (user-error "Board run-set is still restoring; try again when it is ready"))))))

;;;###autoload
(defun e-chat-submit (&optional arg)
  "Submit composer text.
When ARG is a string, submit it as a noninteractive prompt.  Interactively,
plain submit steers an active turn and prefix submit queues a follow-up."
  (interactive "P")
  (if (e-chat-surface-transcript-p)
      (with-current-buffer (e-chat-composer-ensure)
        (e-chat-submit arg))
    (unless (and e-chat-harness e-chat-session-id)
      (user-error "This buffer is not attached to an e chat session"))
    (e-chat--require-board-readiness)
    (let* ((explicit-prompt (and (stringp arg) arg))
         (prefix (and (not explicit-prompt) arg))
         (submission (unless explicit-prompt (e-chat-composer-submission)))
         (references (plist-get submission :references))
         (prompt (or explicit-prompt (plist-get submission :prompt)))
         (intent (if explicit-prompt
                     'submit
                   (e-chat--submit-intent prefix))))
    (e-chat--profile-call
     'chat.submit
     (list :session-id e-chat-session-id
           :buffer-name (buffer-name)
           :metadata (list :intent (symbol-name intent)
                           :prompt-chars (length prompt)
                           :reference-count (length references)
                           :explicit-prompt explicit-prompt
                           :prefix-submit (and prefix t)))
     (lambda ()
       (condition-case err
           (let ((admission-work
                  (pcase intent
               ('submit
                (e-chat-service-submit-session
                 e-chat-harness e-chat-session-id prompt
                 :references references))
               ('steer
                (e-chat-service-steer-session
                 e-chat-harness e-chat-session-id prompt
                 :metadata (e-chat--submit-metadata 'steering references)))
               ('queue
                (e-chat-service-queue-session
                 e-chat-harness e-chat-session-id prompt
                 :references references
                 :metadata (e-chat--submit-metadata 'queued references))))))
             (e-chat--watch-admission
              (current-buffer) admission-work intent)
             (e-chat-composer-delete)
             (e-chat-surface-set-status
              (pcase intent
                ('steer "steered")
                ('queue "queued")
                (_ "queued")))
             (e-chat-composer-insert))
         (user-error
          (if (memq intent '(steer queue))
              (progn
                (e-chat-surface-set-status "input rejected")
                (message "%s" (error-message-string err)))
            (signal (car err) (cdr err))))
         (error
          (if (memq intent '(steer queue))
              (progn
                (e-chat-surface-set-status "input failed")
                (message "%s" (error-message-string err)))
            (signal (car err) (cdr err))))))))))

;;;###autoload
(defun e-chat-abort ()
  "Abort the active turn for the current chat buffer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat-service-abort-session e-chat-harness e-chat-session-id))

;;;###autoload
(defun e-chat-shell ()
  "Return the chat presentation shell manifest."
  (e-shell-create
   :id 'chat
   :name "Chat"
   :summary "Session chat buffer."
   :required-capabilities '(chat-session)
   :commands
   (list
    (e-shell-command-create
     :id 'new
     :summary "Create and open a new persisted chat session."
     :interactive 'e-chat-new
     :function 'e-chat-new
     :scope 'global)
    (e-shell-command-create
     :id 'resume
     :summary "Resume a recent persisted chat session."
     :interactive 'e-chat-resume
     :function 'e-chat-resume
     :scope 'global)
    (e-shell-command-create
     :id 'switch-session
     :summary "Switch to a recent persisted chat session."
     :interactive 'e-chat-switch-session
     :function 'e-chat-switch-session
     :scope 'global)
    (e-shell-command-create
     :id 'active-sessions
     :summary "Open a floating active chat sessions picker."
     :interactive 'e-chat-active-sessions
     :function 'e-chat-active-sessions
     :scope 'global)
    (e-shell-command-create
     :id 'overview
     :summary "Open the chat session overview sidebar."
     :interactive 'e-chat-overview
     :function 'e-chat-overview
     :scope 'global)
    (e-shell-command-create
     :id 'sidebar-toggle
     :summary "Toggle the chat session overview sidebar."
     :interactive 'e-chat-sidebar-toggle
     :function 'e-chat-sidebar-toggle
     :scope 'global)
    (e-shell-command-create
     :id 'overview-close
     :summary "Close the chat session overview sidebar."
     :interactive 'e-chat-overview-close
     :function 'e-chat-overview-close
     :scope 'global)
    (e-shell-command-create
     :id 'add-context-to-latest
     :summary "Add current buffer context to a visible or latest chat session."
     :interactive 'e-chat-add-context-to-latest
     :function 'e-chat-add-context-to-latest
     :scope 'global)
    (e-shell-command-create
     :id 'add-context-to-session
     :summary "Add current buffer context to a selected chat session."
     :interactive 'e-chat-add-context-to-session
     :function 'e-chat-add-context-to-session
     :scope 'global)
    (e-shell-command-create
     :id 'rename
     :summary "Rename the current chat session."
     :interactive 'e-chat-rename
     :function 'e-chat-rename
     :scope 'session)
    (e-shell-command-create
     :id 'set-model
     :summary "Set the current chat session model."
     :interactive 'e-chat-set-model
     :function 'e-chat-set-model
     :scope 'session)
    (e-shell-command-create
     :id 'set-effort
     :summary "Set the current chat session reasoning effort."
     :interactive 'e-chat-set-effort
     :function 'e-chat-set-effort
     :scope 'session)
    (e-shell-command-create
     :id 'set-output-mode
     :summary "Set the current chat session output markup mode."
     :interactive 'e-chat-set-output-mode
     :function 'e-chat-set-output-mode
     :scope 'session)
    (e-shell-command-create
     :id 'show-context
     :summary "Show the current chat session context."
     :interactive 'e-chat-show-context
     :function 'e-chat-show-context
     :scope 'session)
    (e-shell-command-create
     :id 'board-activity-text
     :summary "Open Board activity in the native text renderer."
     :interactive 'e-chat-open-board-activity-text
     :function 'e-chat-open-board-activity-text
     :scope 'session)
    (e-shell-command-create
     :id 'compact-session
     :summary "Compact the current chat session context."
     :interactive 'e-chat-compact-session
     :function 'e-chat-compact-session
     :scope 'session)
    (e-shell-command-create
     :id 'inspect-error
     :summary "Start a new chat session to inspect a recent failed turn."
     :interactive 'e-inspect-error
     :function 'e-inspect-error
     :scope 'global)
    (e-shell-command-create
     :id 'submit
     :summary "Submit the current chat prompt."
     :interactive 'e-chat-submit
     :function 'e-chat-submit
     :scope 'session)
    (e-shell-command-create
     :id 'abort
     :summary "Abort the active chat turn."
     :interactive 'e-chat-abort
     :function 'e-chat-abort
     :scope 'session)
    (e-shell-command-create
     :id 'enter-response-navigation
     :summary "Enter response navigation mode."
     :interactive 'e-chat-enter-response-navigation
     :function 'e-chat-enter-response-navigation
     :scope 'session)
    (e-shell-command-create
     :id 'response-navigation-next
     :summary "Focus the next rendered response block."
     :interactive 'e-chat-response-navigation-next
     :function 'e-chat-response-navigation-next
     :scope 'session)
    (e-shell-command-create
     :id 'response-navigation-previous
     :summary "Focus the previous rendered response block."
     :interactive 'e-chat-response-navigation-previous
     :function 'e-chat-response-navigation-previous
     :scope 'session)
    (e-shell-command-create
     :id 'response-navigation-activate
     :summary "Activate the focused response block."
     :interactive 'e-chat-response-navigation-activate
     :function 'e-chat-response-navigation-activate
     :scope 'session)
    (e-shell-command-create
     :id 'response-navigation-copy
     :summary "Copy the focused response block."
     :interactive 'e-chat-response-navigation-copy
     :function 'e-chat-response-navigation-copy
     :scope 'session)
    (e-shell-command-create
     :id 'response-navigation-open
     :summary "Open the focused response block in an editable buffer."
     :interactive 'e-chat-response-navigation-open
     :function 'e-chat-response-navigation-open
     :scope 'session)
    (e-shell-command-create
     :id 'response-navigation-details
     :summary "Open focused response block details."
     :interactive 'e-chat-response-navigation-details
     :function 'e-chat-response-navigation-details
     :scope 'session)
    (e-shell-command-create
     :id 'response-navigation-insert
     :summary "Leave response navigation and focus the composer."
     :interactive 'e-chat-response-navigation-insert
     :function 'e-chat-response-navigation-insert
     :scope 'session)
    (e-shell-command-create
     :id 'open-latest-response
     :summary "Open the latest final assistant response."
     :interactive 'e-chat-open-latest-response
     :function 'e-chat-open-latest-response
     :scope 'session)
    (e-shell-command-create
     :id 'copy-latest-response
     :summary "Copy the latest final assistant response."
     :interactive 'e-chat-copy-latest-response
     :function 'e-chat-copy-latest-response
     :scope 'session))
   :keymaps
   (list (list :id 'chat-mode
               :keymap e-chat-mode-map
               :scope 'mode)
         (list :id 'context
               :keymap e-chat-context-mode-map
               :scope 'global
               :mode 'e-chat-context-mode)
         (list :id 'response-navigation
               :keymap e-chat-response-navigation-mode-map
               :scope 'mode)
         (list :id 'block-view
               :keymap e-chat-block-view-mode-map
               :scope 'mode)
         (list :id 'tool-list
               :keymap e-chat-tool-list-mode-map
               :scope 'mode))))

(defun e-chat-startup ()
  "Refresh and register the chat shell provider for package startup."
  (e-chat--ensure-presentation-hooks)
  (e-chat--configure-modal-editing-policy)
  (e-chat--refresh-keymaps)
  (e-chat-context-mode 1)
  (e-shell-register (e-chat-shell))
  (e-chat-reload-buffers))

(add-hook 'e-startup-shell-hook #'e-chat-startup)

(e-chat--ensure-presentation-hooks)

(provide 'e-chat)

;;; e-chat.el ends here
