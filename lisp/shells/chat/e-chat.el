;;; e-chat.el --- Basic chat presentation for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Minimal Emacs chat buffer for the e harness.  This module owns presentation
;; only: buffer setup, commands, keymaps, and event rendering.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pp)
(require 'project)
(require 'subr-x)
(require 'e-chat-session)
(require 'e-context-status)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-tools)
(require 'e-session)
(require 'e-shells)
(require 'e-startup)

(declare-function markdown-mode "markdown-mode")
(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(defgroup e-chat nil
  "Chat presentation for e."
  :group 'e
  :prefix "e-chat-")

(defcustom e-chat-buffer-name "*e-chat*"
  "Legacy fallback e chat buffer name."
  :type 'string
  :group 'e-chat)

(defcustom e-chat-default-session-id "default"
  "Legacy fallback chat session id for internal callers."
  :type 'string
  :group 'e-chat)

(defcustom e-chat-context-buffer-name "*e-chat-context*"
  "Buffer name for read-only context previews."
  :type 'string
  :group 'e-chat)

(defconst e-chat--resume-preview-buffer-name "*e-chat-resume-preview*"
  "Buffer name for temporary resume candidate previews.")

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

(defcustom e-chat-overview-buffer-name "*e-chat-overview*"
  "Buffer name for the chat session overview."
  :type 'string
  :group 'e-chat)

(defcustom e-chat-overview-state-file nil
  "Optional JSON file used for chat overview read markers.
When nil, read markers are stored in the active session store directory."
  :type '(choice (const :tag "Use session store directory" nil)
                 file)
  :group 'e-chat)

(defcustom e-chat-resume-preview-message-limit 2
  "Maximum number of transcript messages rendered in resume previews."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-details-buffer-name "*e-chat-details*"
  "Buffer name for read-only focused block details."
  :type 'string
  :group 'e-chat)

(defcustom e-chat-tool-output-buffer-name "*e-chat-tool-output*"
  "Buffer name for read-only focused tool output."
  :type 'string
  :group 'e-chat)

(defcustom e-chat-tool-activity-preview-bytes 4096
  "Maximum UTF-8 bytes of a tool result retained in chat activity UI."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-mode-line-context-estimate-cache-seconds 2.0
  "Seconds to reuse approximate context-token estimates for mode-line refreshes."
  :type 'number
  :group 'e-chat)

(defcustom e-chat-submit-backend-delay 0.05
  "Seconds to delay backend work after rendering a submitted human turn."
  :type 'number
  :group 'e-chat)

(defcustom e-chat-default-harness-id :chat-default
  "Harness registry id used by default chat commands."
  :type 'symbol
  :group 'e-chat)

(defcustom e-chat-progress-interval 0.6
  "Seconds between active assistant progress indicator frames."
  :type 'number
  :group 'e-chat)

(defcustom e-chat-activity-redraw-delay 0.05
  "Seconds to coalesce running activity redraws."
  :type 'number
  :group 'e-chat)

(defcustom e-chat-model-context-token-limits
  '(("gpt-5.5" . 258400)
    ("gpt-5.4" . 1050000)
    ("gpt-5.4-pro" . 1050000)
    ("gpt-5.3-codex" . 400000)
    ("gpt-5.3-codex-spark" . 400000)
    ("gpt-5.2" . 400000)
    ("gpt-5.1" . 400000)
    ("gpt-5.1-codex" . 400000)
    ("gpt-5-codex" . 400000)
    ("gpt-5-mini" . 400000)
    ("gpt-5-nano" . 400000)
    ("gpt-5" . 400000)
    ("gpt-5-chat-latest" . 128000))
  "Alist mapping model names to maximum context tokens.
The mode line uses this presentation-owned table for context usage display."
  :type '(alist :key-type string :value-type integer)
  :group 'e-chat)

(defcustom e-chat-context-token-estimate-bytes-per-token 4.0
  "Approximate UTF-8 bytes per token for mode-line context estimates."
  :type 'number
  :group 'e-chat)

(when (equal e-chat-progress-interval 0.35)
  (setq e-chat-progress-interval 0.6))

(defface e-chat-user-face
  '((t :inherit default
       :foreground "#d7ecff"
       :background "#243347"
       :extend t))
  "Face used for user-authored chat blocks."
  :group 'e-chat)

(defface e-chat-assistant-face
  '((t :inherit default
       :foreground "#e6f3d4"
       :background "#2b3526"
       :extend t))
  "Face used for assistant response chat blocks."
  :group 'e-chat)

(defface e-chat-final-assistant-face
  '((t :inherit e-chat-assistant-face
       :foreground "#edf7df"
       :background "#24301f"
       :box nil
       :extend t))
  "Face used for settled assistant response chat blocks."
  :group 'e-chat)

(defface e-chat-system-face
  '((t :inherit default
       :foreground "#ded2ec"
       :background "#312b3c"
       :extend t))
  "Face used for compact system chat blocks."
  :group 'e-chat)

(defface e-chat-composer-face
  '((t :inherit minibuffer-prompt :extend t))
  "Face used for the composer prompt chrome."
  :group 'e-chat)

(defface e-chat-separator-face
  '((t :inherit shadow
       :foreground "#7f8a99"
       :background "#202833"
       :extend t))
  "Face used for the composer separator."
  :group 'e-chat)

(defface e-chat-turn-separator-face
  '((t :inherit shadow
       :foreground "#7f8a99"
       :background "#202833"
       :box nil
       :extend t))
  "Face used for separators between chat turns."
  :group 'e-chat)

(defface e-chat-response-separator-face
  '((t :inherit shadow
       :foreground "#7f8a99"
       :background "#202833"
       :box nil
       :extend t))
  "Face used for separators between user prompt and agent-side blocks."
  :group 'e-chat)

(defface e-chat-activity-separator-face
  '((t :inherit shadow
       :foreground "#46505c"
       :background "#312b3c"
       :box nil
       :extend t))
  "Face used for separators between intermittent activity rounds."
  :group 'e-chat)

(defun e-chat--apply-owned-face-defaults ()
  "Apply face defaults that should update during live reload."
  (set-face-attribute 'e-chat-separator-face nil
                      :foreground "#7f8a99"
                      :background "#202833"
                      :extend t)
  (set-face-attribute 'e-chat-turn-separator-face nil
                      :foreground "#7f8a99"
                      :background "#202833"
                      :box nil
                      :extend t)
  (set-face-attribute 'e-chat-response-separator-face nil
                      :foreground "#7f8a99"
                      :background "#202833"
                      :box nil
                      :extend t)
  (set-face-attribute 'e-chat-activity-separator-face nil
                      :foreground "#46505c"
                      :background "#312b3c"
                      :box nil
                      :extend t))

(e-chat--apply-owned-face-defaults)

(defface e-chat-title-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1 :extend t))
  "Face used for the chat buffer title."
  :group 'e-chat)

(defface e-chat-focused-turn-face
  '((t :inherit nil
       :background "#27313d"
       :box nil
       :extend t))
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
  '((t :inherit fixed-pitch
       :foreground "#f6d48f"
       :background "#202833"))
  "Face used for inline Markdown code in assistant messages."
  :group 'e-chat)

(defface e-chat-markdown-code-block-face
  '((t :inherit fixed-pitch
       :foreground "#dbe7ef"
       :background "#1c252f"
       :extend t))
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
  '((t :inherit font-lock-constant-face
       :box (:line-width 1 :color "#5f8cc8")))
  "Face used for inline context references in the composer."
  :group 'e-chat)

(defface e-chat-overview-unread-face
  '((t :inherit warning :weight bold))
  "Face used for unread markers in the chat session overview."
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
  '((t :inherit default
       :foreground "#d7ecff"
       :background "#243347"
       :extend t))
  "Default face spec for user-authored chat blocks.")

(defconst e-chat--assistant-face-spec
  '((t :inherit default
       :foreground "#e6f3d4"
       :background "#2b3526"
       :extend t))
  "Default face spec for assistant response chat blocks.")

(defconst e-chat--final-assistant-face-spec
  '((t :inherit e-chat-assistant-face
       :foreground "#edf7df"
       :background "#24301f"
       :box nil
       :extend t))
  "Default face spec for settled assistant response chat blocks.")

(defconst e-chat--system-face-spec
  '((t :inherit default
       :foreground "#ded2ec"
       :background "#312b3c"
       :extend t))
  "Default face spec for compact system chat blocks.")

(defconst e-chat--focused-turn-face-spec
  '((t :inherit nil
       :background "#27313d"
       :box nil
       :extend t))
  "Default face spec for focused response-navigation blocks.")

(defconst e-chat--turn-separator-face-spec
  '((t :inherit shadow
       :foreground "#7f8a99"
       :background "#202833"
       :box nil
       :extend t))
  "Default face spec for separators between chat turns.")

(defconst e-chat--response-separator-face-spec
  '((t :inherit shadow
       :foreground "#7f8a99"
       :background "#202833"
       :box nil
       :extend t))
  "Default face spec for separators between prompt and response blocks.")

(defconst e-chat--activity-separator-face-spec
  '((t :inherit shadow
       :foreground "#46505c"
       :background "#312b3c"
       :box nil
       :extend t))
  "Default face spec for separators between intermittent activity rounds.")

(defconst e-chat--overview-unread-face-spec
  '((t :inherit warning :weight bold))
  "Default face spec for unread overview markers.")

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
  (face-spec-set 'e-chat-overview-title-face
                 e-chat--overview-title-face-spec)
  (face-spec-set 'e-chat-overview-meta-face
                 e-chat--overview-meta-face-spec)
  (face-spec-set 'e-chat-overview-summary-face
                 e-chat--overview-summary-face-spec))

(e-chat--refresh-face-specs)

(defvar-local e-chat-harness nil
  "Harness used by the current chat buffer.")

(defvar-local e-chat-session-id nil
  "Session id used by the current chat buffer.")

(defvar-local e-chat--transcript-end-marker nil
  "Marker at the end of the protected transcript region.")

(defvar-local e-chat--composer-start-marker nil
  "Marker at the beginning of editable composer text.")

(defvar-local e-chat--composer-spacer-marker nil
  "Marker at the beginning of the visual spacer above the composer.")

(defvar-local e-chat--composer-scroll-needed nil
  "Non-nil when a composer edit should scroll input fully into view.")

(defvar-local e-chat--composer-scroll-suppressed nil
  "Non-nil while internal composer rewrites should not request scrolling.")

(defvar-local e-chat--turn-registry nil
  "Hash table of rendered turn metadata keyed by turn id.")

(defvar-local e-chat--block-registry nil
  "Hash table of rendered block metadata keyed by block id.")

(defvar-local e-chat--block-order nil
  "Rendered block ids in transcript order.")

(defvar-local e-chat--focused-turn-id nil
  "Turn id belonging to the currently focused response-navigation block.")

(defvar-local e-chat--focused-block-id nil
  "Block id currently focused by response navigation.")

(defvar-local e-chat--block-counter 0
  "Counter used to assign display-local block ids.")

(defvar-local e-chat--focused-turn-overlay nil
  "Overlay highlighting the focused response-navigation turn.")

(defvar-local e-chat--latest-final-block-id nil
  "Most recent final assistant block id in this chat buffer.")

(defvar-local e-chat--last-rendered-turn-id nil
  "Most recent turn id that rendered a durable transcript block.")

(defvar-local e-chat--last-rendered-side nil
  "Side of the most recent durable transcript block.")

(defvar-local e-chat--block-view-block-id nil
  "Block id currently active in block view mode.")

(defvar-local e-chat--tool-list-block-id nil
  "Activity block id currently showing a tool list.")

(defvar-local e-chat--tool-list-index 0
  "Selected tool item index in the focused activity tool list.")

(defvar-local e-chat--event-subscription nil
  "Harness event subscription owned by this chat buffer.")

(defvar-local e-chat--tool-list-overlay nil
  "Overlay highlighting the selected activity tool list item.")

(defvar-local e-chat--tool-output-origin-buffer nil
  "Chat buffer that opened the current tool output buffer.")

(defvar-local e-chat--context-reference-counter 0
  "Counter used to assign inline composer reference ids.")

(defvar-local e-chat--composer-restore-inhibited nil
  "Non-nil when transient chat rendering should not recreate a composer.")

(defvar-local e-chat--progress-turn-id nil
  "Turn id currently represented by the assistant progress indicator.")

(defvar-local e-chat--progress-frame 0
  "Current active assistant progress indicator frame.")

(defvar-local e-chat--progress-start-marker nil
  "Marker at the start of the active assistant progress indicator.")

(defvar-local e-chat--progress-end-marker nil
  "Marker at the end of the active assistant progress indicator.")

(defvar-local e-chat--running-status-start-marker nil
  "Marker at the start of the active running-turn status region.")

(defvar-local e-chat--running-status-end-marker nil
  "Marker at the end of the active running-turn status region.")

(defvar-local e-chat--pending-activity-redraw-turn-id nil
  "Turn id with a scheduled running activity redraw.")

(defvar-local e-chat--pending-activity-redraw-timer nil
  "Timer scheduled to redraw running activity.")

(defvar-local e-chat--pending-activity-redraw-kind nil
  "Kind of pending activity redraw, either `activity' or `progress'.")

(defvar-local e-chat--mode-line-status nil
  "Current compact e chat status text shown in the mode line.")

(defvar-local e-chat--mode-line-context-estimate-cache nil
  "Caller-owned (TOKENS . TIME) cache cell for context-token estimates.
Passed to `e-context-status-text' to reuse approximate estimates between
mode-line refreshes for the current chat buffer.")

(defvar-local e-chat--status nil
  "Current chat status text shown in the header line.")

(defvar-local e-chat--rendered-session-title nil
  "Session title currently rendered in the chat title block.")

(defvar-local e-chat--progress-timer nil
  "Timer advancing the active assistant progress indicator.")

(defvar-local e-chat--progress-next-tick-time nil
  "Expected `float-time' of the next assistant progress timer tick.")

(defconst e-chat--user-glyph ">"
  "Glyph shown before user-authored chat blocks.")

(defconst e-chat--assistant-glyph "●"
  "Glyph shown before assistant chat blocks.")

(defconst e-chat--system-glyph "·"
  "Glyph shown before compact system chat blocks.")

(defconst e-chat--progress-glyphs ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Glyphs used for the active assistant progress indicator.")

(defconst e-chat--composer-glyph "❯ "
  "Glyph shown before editable e chat composer text.")

(defconst e-chat--composer-separator
  "────────────────────────────────────────────────────────────────"
  "Separator shown above the e chat composer.")

(defconst e-chat--turn-separator
  "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  "Separator shown between rendered e chat turns.")

(defconst e-chat--response-separator
  e-chat--composer-separator
  "Separator shown between prompt and agent-side blocks in a turn.")

(defconst e-chat--activity-separator
  "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈"
  "Subtle separator shown between intermittent activity rounds.")

(defconst e-chat--title "E Agent Session"
  "Title shown at the top of e chat buffers.")

(defconst e-chat--reasoning-effort-values
  '("" "minimal" "low" "medium" "high" "xhigh")
  "Reasoning effort values offered by the chat presentation.")

(defconst e-chat--new-context-session-label "+ New e chat session"
  "Picker label for creating a new chat session for context insertion.")

(defvar e-chat--test-window-body-height nil
  "Test override for the visible chat window height.")

(defvar e-chat--test-transcript-screen-lines nil
  "Test override for transcript screen-line height.")

(defvar e-chat--refresh-visible-composers-in-progress nil
  "Non-nil while visible e chat composers are being refreshed.")

(defconst e-chat--protected-properties
  '(read-only t
    e-chat-protected t
    front-sticky (read-only e-chat-protected field)
    rear-nonsticky (read-only e-chat-protected field
                    face font-lock-face invisible display
                    e-chat-block-id e-chat-turn-id e-chat-separator
                    e-chat-composer e-chat-context-reference
                    e-chat-transient-turn-id e-chat-progress-turn-id
                    e-chat-markdown-syntax mouse-face help-echo)
    field e-chat-transcript)
  "Text properties applied to protected e chat presentation text.")

(defconst e-chat--composer-stripped-properties
  '(read-only e-chat-protected field
    face font-lock-face invisible display
    e-chat-block-id e-chat-turn-id e-chat-separator e-chat-composer
    e-chat-transient-turn-id e-chat-progress-turn-id
    e-chat-markdown-syntax mouse-face help-echo)
  "Presentation properties stripped from ordinary composer text.")

(defconst e-chat--composer-reference-stripped-properties
  '(e-chat-protected field
    face invisible e-chat-block-id e-chat-turn-id e-chat-separator
    e-chat-composer e-chat-transient-turn-id e-chat-progress-turn-id
    e-chat-markdown-syntax)
  "Presentation properties stripped from inline composer references.")

(defconst e-chat--composer-edit-commands
  '(self-insert-command
    newline
    yank
    yank-pop
    clipboard-yank
    quoted-insert
    e-chat-delete-backward-char
    e-chat-delete-forward-char
    delete-backward-char
    backward-delete-char-untabify
    delete-forward-char
    delete-char)
  "Commands that should resume composer input from readback position.")

(defun e-chat--make-response-navigation-mode-map (&optional map)
  "Return MAP configured for `e-chat-response-navigation-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (define-key map (kbd "j") #'e-chat-response-navigation-next)
    (define-key map (kbd "k") #'e-chat-response-navigation-previous)
    (define-key map (kbd "RET") #'e-chat-response-navigation-activate)
    (define-key map (kbd "i") #'e-chat-response-navigation-insert)
    (define-key map (kbd "<escape>") #'e-chat-response-navigation-insert)
    (define-key map (kbd "y") #'e-chat-response-navigation-copy)
    (define-key map (kbd "o") #'e-chat-response-navigation-open)
    (define-key map (kbd "d") #'e-chat-response-navigation-details)
    map))

(defvar e-chat-response-navigation-mode-map
  (e-chat--make-response-navigation-mode-map)
  "Keymap for response navigation inside `e-chat-mode'.")

(defun e-chat--make-block-view-mode-map (&optional map)
  "Return MAP configured for `e-chat-block-view-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (define-key map (kbd "h") #'e-chat-block-view-left)
    (define-key map (kbd "j") #'e-chat-block-view-down)
    (define-key map (kbd "k") #'e-chat-block-view-up)
    (define-key map (kbd "l") #'e-chat-block-view-right)
    (define-key map (kbd "G") #'e-chat-block-view-end)
    (define-key map (kbd "g g") #'e-chat-block-view-beginning)
    (define-key map (kbd "v") #'e-chat-block-view-select)
    (define-key map (kbd "y") #'e-chat-block-view-copy)
    (define-key map (kbd "i") #'e-chat-block-view-insert)
    (define-key map (kbd "<escape>") #'e-chat-block-view-back)
    map))

(defvar e-chat-block-view-mode-map
  (e-chat--make-block-view-mode-map)
  "Keymap for block-local view mode inside `e-chat-mode'.")

(defun e-chat--make-tool-list-mode-map (&optional map)
  "Return MAP configured for `e-chat-tool-list-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (define-key map (kbd "j") #'e-chat-tool-list-next)
    (define-key map (kbd "k") #'e-chat-tool-list-previous)
    (define-key map (kbd "RET") #'e-chat-tool-list-open-output)
    (define-key map (kbd "<escape>") #'e-chat-tool-list-back)
    map))

(defvar e-chat-tool-list-mode-map
  (e-chat--make-tool-list-mode-map)
  "Keymap for activity tool-list mode inside `e-chat-mode'.")

(defun e-chat--make-tool-output-mode-map (&optional map)
  "Return MAP configured for `e-chat-tool-output-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "<escape>") #'e-chat-tool-output-back)
    map))

(defvar e-chat-tool-output-mode-map
  (e-chat--make-tool-output-mode-map)
  "Keymap for read-only tool output buffers.")

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
  (when-let ((binding (e-chat--host-alt-leader-binding)))
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
    (define-key map (kbd "RET") #'newline)
    (define-key map [remap delete-backward-char]
                #'e-chat-delete-backward-char)
    (define-key map [remap backward-delete-char-untabify]
                #'e-chat-delete-backward-char)
    (define-key map [remap delete-forward-char]
                #'e-chat-delete-forward-char)
    (define-key map [remap delete-char]
                #'e-chat-delete-forward-char)
    (define-key map (kbd "C-c C-k") #'e-chat-abort)
    (define-key map (kbd "C-c C-r") #'e-chat-reset)
    (define-key map (kbd "C-c C-x") #'e-chat-show-context)
    (define-key map (kbd "C-c C-m") #'e-chat-compact-session)
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

(defun e-chat--make-overview-mode-map (&optional map)
  "Return MAP configured as the keymap for `e-chat-overview-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'e-chat-overview-open-session)
    (define-key map (kbd "o") #'e-chat-overview-open-session)
    (define-key map (kbd "v") #'e-chat-overview-preview-session)
    (define-key map (kbd "j") #'e-chat-overview-next-session)
    (define-key map (kbd "k") #'e-chat-overview-previous-session)
    (define-key map (kbd "g") #'e-chat-overview-refresh)
    (define-key map (kbd "q") #'e-chat-overview-close)
    map))

(defvar e-chat-overview-mode-map (e-chat--make-overview-mode-map)
  "Keymap for `e-chat-overview-mode'.")

(defvar-local e-chat-overview--harness nil
  "Harness whose sessions are rendered in this overview buffer.")

(defvar-local e-chat-overview--subscription nil
  "Harness event subscription for this overview buffer.")

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

(defun e-chat--refresh-keymaps ()
  "Refresh chat keymaps after live reload."
  (setq e-chat-response-navigation-mode-map
        (e-chat--make-response-navigation-mode-map
         e-chat-response-navigation-mode-map))
  (setq e-chat-block-view-mode-map
        (e-chat--make-block-view-mode-map e-chat-block-view-mode-map))
  (setq e-chat-tool-list-mode-map
        (e-chat--make-tool-list-mode-map e-chat-tool-list-mode-map))
  (setq e-chat-tool-output-mode-map
        (e-chat--make-tool-output-mode-map e-chat-tool-output-mode-map))
  (setq e-chat-overview-mode-map
        (e-chat--make-overview-mode-map e-chat-overview-mode-map))
  (setq e-chat-mode-map (e-chat--make-mode-map e-chat-mode-map)))

(define-derived-mode e-chat-mode text-mode "e-chat"
  "Major mode for e chat buffers."
  (add-hook 'kill-buffer-hook #'e-chat--unsubscribe nil t)
  (add-hook 'kill-buffer-hook #'e-chat--stop-progress-indicator nil t)
  (add-hook 'evil-local-mode-hook #'e-chat--enforce-modal-editing-policy nil t)
  (add-hook 'after-change-functions
            #'e-chat--mark-composer-scroll-needed nil t)
  (add-hook 'pre-command-hook #'e-chat--pre-command nil t)
  (add-hook 'post-command-hook #'e-chat--post-command nil t))

(define-minor-mode e-chat-response-navigation-mode
  "Navigate rendered turn blocks in an e chat buffer."
  :lighter " Nav"
  :keymap e-chat-response-navigation-mode-map
  (unless e-chat-response-navigation-mode
    (setq e-chat--focused-turn-id nil)
    (setq e-chat--focused-block-id nil)
    (when (overlayp e-chat--focused-turn-overlay)
      (delete-overlay e-chat--focused-turn-overlay))))

(define-minor-mode e-chat-block-view-mode
  "Move within the focused e chat block."
  :lighter " View"
  :keymap e-chat-block-view-mode-map
  (unless e-chat-block-view-mode
    (setq e-chat--block-view-block-id nil)))

(define-minor-mode e-chat-tool-list-mode
  "Navigate tool calls for a focused e chat activity block."
  :lighter " Tools"
  :keymap e-chat-tool-list-mode-map
  (unless e-chat-tool-list-mode
    (setq e-chat--tool-list-block-id nil)
    (setq e-chat--tool-list-index 0)
    (when (overlayp e-chat--tool-list-overlay)
      (delete-overlay e-chat--tool-list-overlay))))

(define-derived-mode e-chat-tool-output-mode special-mode "e-chat-tool-output"
  "Major mode for read-only e chat tool output buffers.")

(define-derived-mode e-chat-overview-mode special-mode "e-chat-overview"
  "Major mode for the e chat session overview."
  (add-hook 'kill-buffer-hook #'e-chat-overview--unsubscribe nil t)
  (add-hook 'evil-local-mode-hook #'e-chat--enforce-modal-editing-policy nil t)
  (e-chat--disable-modal-editing)
  (setq-local truncate-lines t))

;;;###autoload
(define-minor-mode e-chat-context-mode
  "Globally bind commands that add current buffer context to e chat."
  :global t
  :lighter " eCtx"
  :keymap e-chat-context-mode-map
  (e-chat--configure-evil-context-bindings))

(defun e-chat--disable-modal-editing ()
  "Disable local modal editing state for the chat buffer when available."
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1))
  (when (boundp 'evil-local-mode)
    (setq-local evil-local-mode nil))
  (when (boundp 'evil-state)
    (setq-local evil-state nil)))

(defun e-chat--enforce-modal-editing-policy ()
  "Disable modal editing when it is reactivated in chat buffers."
  (when (and (or (derived-mode-p 'e-chat-mode)
                 (derived-mode-p 'e-chat-overview-mode))
             (boundp 'evil-local-mode)
             evil-local-mode)
    (e-chat--disable-modal-editing)))

(defun e-chat--configure-modal-editing-policy ()
  "Configure modal editors to keep `e-chat-mode' non-normal."
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'e-chat-mode 'emacs)
    (evil-set-initial-state 'e-chat-overview-mode 'emacs)))

(e-chat--configure-modal-editing-policy)
(with-eval-after-load 'evil
  (e-chat--configure-modal-editing-policy))

(defun e-chat--disable-completion ()
  "Disable completion sources and completion UI in the chat composer."
  (when (fboundp 'company-mode)
    (company-mode -1))
  (when (fboundp 'corfu-mode)
    (corfu-mode -1))
  (when (fboundp 'auto-complete-mode)
    (auto-complete-mode -1))
  (setq-local completion-at-point-functions nil)
  (setq-local completion-in-region-function #'ignore)
  (setq-local company-backends nil)
  (setq-local company-idle-delay nil))

(defun e-chat--harness-has-capability-p (harness capability-id)
  "Return non-nil when HARNESS has active capability CAPABILITY-ID."
  (memq capability-id
        (mapcar #'e-capability-id
                (e-harness-active-capabilities harness))))

(defun e-chat--default-harness ()
  "Return the configured default chat harness from the harness registry."
  (let ((harness
         (condition-case err
             (e-harness-registry-get-or-create e-chat-default-harness-id)
           (e-harness-registry-missing
            (user-error "No e harness registered for %S"
                        (cadr err))))))
    (unless (e-chat--harness-has-capability-p harness 'chat-session)
      (user-error "Harness %S does not provide chat-session capability"
                  e-chat-default-harness-id))
    harness))

(defun e-chat--ensure-session (harness session-id)
  "Ensure SESSION-ID exists in HARNESS."
  (condition-case nil
      (e-chat-session-context harness session-id)
    (e-session-missing
     (e-chat--create-session harness session-id))))

(defun e-chat--git-root (directory)
  "Return Git worktree root containing DIRECTORY, or nil."
  (when-let ((root (locate-dominating-file directory ".git")))
    (file-name-as-directory (expand-file-name root))))

(defun e-chat--project-root (&optional directory)
  "Return the project root for DIRECTORY, falling back to DIRECTORY.
Projectile is preferred when available, followed by `project-current', then a
plain Git ancestor check.  The return value is always a normalized directory
name."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or directory default-directory))))
         (projectile-root
          (when (fboundp 'projectile-project-root)
            (let ((default-directory directory))
              (ignore-errors (projectile-project-root)))))
         (project-root
          (let ((default-directory directory))
            (ignore-errors
              (when-let ((project (project-current nil)))
                (project-root project)))))
         (root (or projectile-root
                   project-root
                   (e-chat--git-root directory)
                   directory)))
    (file-name-as-directory (expand-file-name root))))

(defun e-chat--session-metadata ()
  "Return metadata for a chat session created from the current buffer."
  (list :project-root (e-chat--project-root default-directory)))

(defun e-chat--create-session (harness &optional session-id)
  "Create a chat session in HARNESS with SESSION-ID when non-nil."
  (e-harness-create-session
   harness
   :id session-id
   :metadata (e-chat--session-metadata)))

(defun e-chat--short-session-id (session-id)
  "Return a compact SESSION-ID for display."
  (if (> (length session-id) 12)
      (substring session-id 0 12)
    session-id))

(defun e-chat--session-buffer-name (harness session-id)
  "Return the buffer name for SESSION-ID in HARNESS."
  (format "*e-chat:%s*"
          (or (ignore-errors (e-harness-session-title harness session-id))
              (e-chat--short-session-id session-id))))

(defun e-chat--find-session-buffer (session-id)
  "Return an existing chat buffer for SESSION-ID."
  (catch 'buffer
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'e-chat-mode)
                     (equal e-chat-session-id session-id))
            (throw 'buffer buffer)))))))

(defun e-chat--rename-buffer-for-session ()
  "Rename the current buffer from its attached session metadata."
  (when (and e-chat-harness e-chat-session-id)
    (rename-buffer
     (e-chat--session-buffer-name
      e-chat-harness
      e-chat-session-id)
     t)))

(defun e-chat--subscribe (harness buffer session-id)
  "Subscribe BUFFER to HARNESS chat events for SESSION-ID."
  (setq e-chat--event-subscription
        (e-harness-subscribe
         harness
         (lambda (event)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (eq e-chat-harness harness)
                 (e-chat--render-event event)))))
         :session-id session-id)))

(defun e-chat--unsubscribe ()
  "Remove this buffer's harness event subscription."
  (when (and e-chat-harness
             e-chat--event-subscription)
    (e-harness-unsubscribe e-chat-harness e-chat--event-subscription))
  (setq e-chat--event-subscription nil))

(defun e-chat--mark-protected (start end)
  "Mark text between START and END as protected presentation text."
  (when (< start end)
    (add-text-properties start end e-chat--protected-properties)))

(defun e-chat--insert-protected (text &optional face properties)
  "Insert TEXT as protected presentation text at point.
FACE is applied when non-nil.  PROPERTIES are added with text properties."
  (let ((start (point)))
    (insert text)
    (e-chat--mark-protected start (point))
    (when face
      (add-text-properties start (point) `(font-lock-face ,face)))
    (when properties
      (add-text-properties start (point) properties))))

(defun e-chat--apply-activity-separator-face (start end)
  "Apply the quiet activity separator face between START and END."
  (when (< start end)
    (save-excursion
      (goto-char start)
      (while (search-forward e-chat--activity-separator end t)
        (add-text-properties
         (match-beginning 0)
         (match-end 0)
         '(font-lock-face e-chat-activity-separator-face))))))

(defun e-chat--entry-side (title)
  "Return the prompt/agent side represented by entry TITLE."
  (if (equal title "You") 'user 'agent))

(defun e-chat--insert-horizontal-separator (text face)
  "Insert protected separator TEXT with FACE at point."
  (e-chat--insert-protected
   (concat text "\n")
   face
   '(e-chat-separator t)))

(defun e-chat--maybe-insert-turn-separator (turn-id)
  "Insert a stable separator before TURN-ID when crossing turns."
  (when (and turn-id
             e-chat--last-rendered-turn-id
             (not (equal turn-id e-chat--last-rendered-turn-id)))
    (e-chat--insert-horizontal-separator
     e-chat--turn-separator
     'e-chat-turn-separator-face)))

(defun e-chat--maybe-insert-response-separator (turn-id side)
  "Insert a stable separator before TURN-ID's first agent SIDE block."
  (when (and turn-id
             (eq side 'agent)
             (equal e-chat--last-rendered-turn-id turn-id)
             (eq e-chat--last-rendered-side 'user))
    (let ((record (e-chat--turn-record turn-id)))
      (unless (plist-get record :response-separator-rendered)
        (e-chat--insert-horizontal-separator
         e-chat--response-separator
         'e-chat-separator-face)
        (plist-put record :response-separator-rendered t)))))

(defun e-chat--insert-durable-entry-separators (turn-id side)
  "Insert separators needed before a durable TURN-ID block on SIDE."
  (e-chat--maybe-insert-turn-separator turn-id)
  (e-chat--maybe-insert-response-separator turn-id side))

(defun e-chat--record-durable-entry-rendered (turn-id side)
  "Record that a durable TURN-ID block on SIDE was rendered."
  (when (and turn-id side)
    (setq e-chat--last-rendered-turn-id turn-id)
    (setq e-chat--last-rendered-side side)))

(defun e-chat--composer-active-p ()
  "Return non-nil when the current buffer has an active composer."
  (and (markerp e-chat--transcript-end-marker)
       (marker-position e-chat--transcript-end-marker)
       (markerp e-chat--composer-start-marker)
       (marker-position e-chat--composer-start-marker)))

(defun e-chat--capture-composer-state ()
  "Return the active composer content and point position."
  (when (e-chat--composer-active-p)
    (let ((start (marker-position e-chat--composer-start-marker))
          (end (point-max))
          (window (e-chat--visible-window)))
      (list :text (e-chat--sanitize-composer-text
                   (buffer-substring e-chat--composer-start-marker end))
            :point (point)
            :window window
            :window-point (and window (window-point window))
            :window-start (and window (window-start window))
            :point-offset
            (when (and (>= (point) start)
                       (<= (point) end))
              (- (point) start))))))

(defun e-chat--restore-composer-state (state)
  "Restore an editable composer from STATE.
When STATE is nil, insert an empty composer."
  (unless e-chat--composer-restore-inhibited
    (if (not state)
        (e-chat--insert-composer)
      (let ((offset (plist-get state :point-offset)))
        (e-chat--insert-composer (plist-get state :text) (not offset))
        (if offset
            (goto-char (min (point-max)
                            (+ (marker-position e-chat--composer-start-marker)
                               offset)))
          (let ((point (plist-get state :point))
                (window (plist-get state :window))
                (window-point (plist-get state :window-point))
                (window-start (plist-get state :window-start)))
            (when point
              (goto-char (min point (point-max))))
            (when (window-live-p window)
              (when window-start
                (set-window-start window
                                  (min window-start (point-max))
                                  t))
              (when window-point
                (set-window-point window
                                  (min window-point (point-max)))))))))))

(defun e-chat--delete-composer ()
  "Delete the active composer from the current chat buffer.
Return non-nil when a composer was removed."
  (let ((start (or (and (markerp e-chat--composer-spacer-marker)
                        (marker-position e-chat--composer-spacer-marker))
                   (and (markerp e-chat--transcript-end-marker)
                        (marker-position e-chat--transcript-end-marker))
                   (and (markerp e-chat--composer-start-marker)
                        (marker-position e-chat--composer-start-marker)))))
    (when start
      (let ((inhibit-read-only t))
        (let ((e-chat--composer-scroll-suppressed t))
          (delete-region start (point-max)))
        (set-marker e-chat--transcript-end-marker nil)
        (set-marker e-chat--composer-start-marker nil)
        (when (markerp e-chat--composer-spacer-marker)
          (set-marker e-chat--composer-spacer-marker nil))
        (setq e-chat--composer-scroll-needed nil)
        t))))

(defun e-chat--sanitize-composer-text (text)
  "Return TEXT without leaked transcript presentation properties."
  (let ((copy (copy-sequence text))
        (position 0)
        next)
    (while (< position (length copy))
      (setq next (or (next-single-property-change
                      position 'e-chat-context-reference copy)
                     (length copy)))
      (remove-list-of-text-properties
       position
       next
       (if (get-text-property position 'e-chat-context-reference copy)
           e-chat--composer-reference-stripped-properties
         e-chat--composer-stripped-properties)
       copy)
      (setq position next))
    copy))

(defun e-chat--visible-height ()
  "Return the visible body height for the current chat buffer."
  (or e-chat--test-window-body-height
      (when-let ((window (get-buffer-window (current-buffer) t)))
        (window-body-height window))))

(defun e-chat--visible-window ()
  "Return a visible window for the current chat buffer."
  (get-buffer-window (current-buffer) t))

(defun e-chat--transcript-screen-lines ()
  "Return screen lines used by transcript content before point."
  (or e-chat--test-transcript-screen-lines
      (when-let ((window (e-chat--visible-window)))
        (count-screen-lines (point-min) (point) nil window))
      (count-lines (point-min) (point))))

(defun e-chat--screen-lines (start end)
  "Return screen lines between START and END in the visible chat window."
  (if (>= start end)
      0
    (or (when-let ((window (e-chat--visible-window)))
          (count-screen-lines start end nil window))
        (count-lines start end))))

(defun e-chat--composer-spacer-lines (&optional transcript-lines composer-lines)
  "Return how many protected blank lines should precede the composer."
  (let ((height (e-chat--visible-height)))
    (if (not height)
        0
      (max 0 (- height
                (or transcript-lines (e-chat--transcript-screen-lines))
                (or composer-lines 2)
                2)))))

(defun e-chat--insert-composer (&optional text preserve-focus)
  "Insert an editable composer at the end of the current chat buffer.
When PRESERVE-FOCUS is non-nil, do not move point or window focus to it."
  (e-chat--disable-completion)
  (let ((saved-point (point))
        (saved-window (e-chat--visible-window)))
    (e-chat--delete-composer)
    (let ((inhibit-read-only t)
          (e-chat--composer-scroll-suppressed t))
      (goto-char (point-max))
      (unless (or (bobp) (bolp))
        (insert "\n"))
      (setq e-chat--composer-spacer-marker (point-marker))
      (set-marker-insertion-type e-chat--composer-spacer-marker nil)
      (e-chat--insert-protected
       (concat e-chat--composer-separator "\n")
       'e-chat-separator-face)
      (e-chat--insert-protected
       e-chat--composer-glyph
       'e-chat-composer-face
       '(e-chat-composer t))
      (setq e-chat--composer-start-marker (point-marker))
      (set-marker-insertion-type e-chat--composer-start-marker nil)
      (when text
        (insert (e-chat--sanitize-composer-text text)))
      (let* ((transcript-lines
              (save-excursion
                (goto-char e-chat--composer-spacer-marker)
                (e-chat--transcript-screen-lines)))
             (composer-lines
              (e-chat--screen-lines e-chat--composer-spacer-marker (point-max)))
             (spacer-lines
              (e-chat--composer-spacer-lines transcript-lines composer-lines)))
        (goto-char e-chat--composer-spacer-marker)
        (when (> spacer-lines 0)
          (e-chat--insert-protected
           (make-string spacer-lines ?\n)
           'e-chat-separator-face)))
      (setq e-chat--transcript-end-marker (point-marker))
      (set-marker-insertion-type e-chat--transcript-end-marker nil)
      (goto-char (point-max))
      (setq e-chat--composer-scroll-needed nil)
      (if preserve-focus
          (progn
            (goto-char (min saved-point (point-max)))
            (when (window-live-p saved-window)
              (set-window-point saved-window
                                (min saved-point (point-max)))))
        (e-chat--show-composer)))))

(defun e-chat--ensure-composer ()
  "Ensure the current chat buffer has an active composer."
  (unless (e-chat--composer-active-p)
    (e-chat--insert-composer)))

(defun e-chat--point-in-composer-p (&optional position)
  "Return non-nil when POSITION, or point, is in editable composer text."
  (and (e-chat--composer-active-p)
       (>= (or position (point))
           (marker-position e-chat--composer-start-marker))))

(defun e-chat--clamp-to-composer ()
  "Move point back to the editable composer boundary when it escaped upward."
  (when (and (e-chat--composer-active-p)
             (not e-chat-response-navigation-mode)
             (not e-chat-block-view-mode)
             (not e-chat-tool-list-mode)
             (< (point) (marker-position e-chat--composer-start-marker)))
    (goto-char e-chat--composer-start-marker)))

(defun e-chat--mark-composer-scroll-needed (_begin end _length)
  "Record that a composer edit ending at END needs bottom visibility."
  (when (and (not e-chat--composer-scroll-suppressed)
             (e-chat--composer-active-p)
             (> end (marker-position e-chat--composer-start-marker)))
    (setq e-chat--composer-scroll-needed t)))

(defun e-chat--scroll-composer-edit-into-view ()
  "Scroll the current composer edit down without changing user scroll policy."
  (when-let ((window (e-chat--visible-window)))
    (set-window-point window (point))
    (with-selected-window window
      (ignore-errors
        (recenter -2)))))

(defun e-chat--composer-edit-command-p (command)
  "Return non-nil when COMMAND should target composer input."
  (memq command e-chat--composer-edit-commands))

(defun e-chat--pre-command ()
  "Redirect edit commands from readback into the composer."
  (cond
   ((and (e-chat--composer-active-p)
         (not e-chat-response-navigation-mode)
         (not e-chat-block-view-mode)
         (not e-chat-tool-list-mode)
         (not (e-chat--point-in-composer-p))
         (e-chat--composer-edit-command-p this-command))
    (e-chat--show-composer))))

(defun e-chat--post-command ()
  "Maintain composer editing invariants after interactive commands."
  (when e-chat--composer-scroll-needed
    (setq e-chat--composer-scroll-needed nil)
    (when (e-chat--point-in-composer-p)
      (e-chat--scroll-composer-edit-into-view))))

(defun e-chat-previous-line (&optional arg try-vscroll)
  "Move up ARG lines like `previous-line', honoring TRY-VSCROLL.
Keep point inside the composer when movement starts there."
  (interactive "^p\np")
  (let ((started-in-composer (e-chat--point-in-composer-p)))
    (previous-line arg try-vscroll)
    (when started-in-composer
      (e-chat--clamp-to-composer))))

(defun e-chat--composer-text ()
  "Return the current editable composer text."
  (unless (e-chat--composer-active-p)
    (user-error "No active e chat composer"))
  (string-trim
   (buffer-substring-no-properties e-chat--composer-start-marker
                                   (point-max))))

(defun e-chat--active-region-p ()
  "Return non-nil when the current buffer has a meaningful active region."
  (and mark-active
       (mark t)
       (/= (region-beginning) (region-end))))

(defun e-chat--last-content-line-number ()
  "Return the last content line number in the current buffer."
  (save-excursion
    (goto-char (point-max))
    (if (and (bolp) (not (bobp)))
        (line-number-at-pos (1- (point)))
      (line-number-at-pos))))

(defun e-chat--line-range-text (start-line end-line)
  "Return text from START-LINE through END-LINE, preserving final newlines."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- start-line))
    (let ((start (point)))
      (forward-line (1+ (- end-line start-line)))
      (buffer-substring-no-properties start (point)))))

(defun e-chat--source-uri ()
  "Return a resource URI for the current source buffer."
  (if buffer-file-name
      (concat "file://" (expand-file-name buffer-file-name))
    (concat "buffer://" (buffer-name))))

(defun e-chat--source-label (start-line end-line &optional focus-line)
  "Return a compact source label for START-LINE through END-LINE.
When FOCUS-LINE is non-nil, make that point line the primary label and keep
the surrounding line range as context."
  (format "%s:%s"
          (if buffer-file-name
              (file-name-nondirectory buffer-file-name)
            (buffer-name))
          (cond
           ((and focus-line (= start-line end-line))
            (number-to-string focus-line))
           (focus-line
            (format "%d (context %d-%d)"
                    focus-line
                    start-line
                    end-line))
           ((= start-line end-line)
            (number-to-string start-line))
           (t
            (format "%d-%d" start-line end-line)))))

(defun e-chat--capture-context-reference ()
  "Capture the current point or active region as a chat context reference."
  (e-chat-capture-source-reference))

;;;###autoload
(defun e-chat-capture-source-reference (&optional line-radius)
  "Capture the current point or active region as a chat source reference.
LINE-RADIUS controls the number of lines around point used when no region is
active.  It defaults to the historical chat context radius of two lines."
  (let* ((point-line (line-number-at-pos))
         (has-region (e-chat--active-region-p))
         (line-radius (or line-radius 2))
         (start-line (if has-region
                         (line-number-at-pos (region-beginning))
                       (max 1 (- point-line line-radius))))
         (end-line (if has-region
                       (line-number-at-pos
                        (max (region-beginning) (1- (region-end))))
                     (min (e-chat--last-content-line-number)
                          (+ point-line line-radius))))
         (text (if has-region
                   (buffer-substring-no-properties
                    (region-beginning)
                    (region-end))
                 (e-chat--line-range-text start-line end-line))))
    (let ((reference
           (list :uri (e-chat--source-uri)
                 :label (e-chat--source-label
                         start-line
                         end-line
                         (and (not has-region) point-line))
                 :text text
                 :start-line start-line
                 :end-line end-line
                 :point-line point-line)))
      (unless has-region
        (setq reference (plist-put reference :point-context t)))
      reference)))

(defun e-chat--capture-context-reference-for-command ()
  "Capture a chat context reference and clear source buffer selection."
  (prog1 (e-chat--capture-context-reference)
    (when (e-chat--active-region-p)
      (deactivate-mark t))))

(defun e-chat--next-context-reference-id ()
  "Return the next display-local context reference id."
  (setq e-chat--context-reference-counter
        (1+ e-chat--context-reference-counter))
  (format "ref-%d" e-chat--context-reference-counter))

(defun e-chat--context-reference-with-id (reference)
  "Return REFERENCE with a stable id."
  (let ((reference (copy-sequence reference)))
    (unless (plist-get reference :id)
      (setq reference
            (plist-put reference
                       :id
                       (e-chat--next-context-reference-id))))
    reference))

(defun e-chat--insert-context-reference (reference)
  "Insert REFERENCE as a protected inline atom in the composer."
  (unless (e-chat--composer-active-p)
    (e-chat--insert-composer))
  (unless (e-chat--point-in-composer-p)
    (goto-char (point-max)))
  (let* ((reference (e-chat--context-reference-with-id reference))
         (display (format "@[%s]" (plist-get reference :label)))
         (start (point))
         (inhibit-read-only t))
    (insert display)
    (add-text-properties
     start
     (point)
     `(read-only t
       e-chat-context-reference ,reference
       font-lock-face e-chat-context-reference-face
       help-echo ,(plist-get reference :uri)
       front-sticky nil
       rear-nonsticky t))
    reference))

(defun e-chat--context-reference-bounds-at (position)
  "Return bounds of the inline context reference adjacent to POSITION."
  (when (e-chat--composer-active-p)
    (let* ((start-limit (marker-position e-chat--composer-start-marker))
           (end-limit (point-max))
           (probe (cond
                   ((and (< position end-limit)
                         (get-text-property
                          position
                          'e-chat-context-reference))
                    position)
                   ((and (> position start-limit)
                         (get-text-property
                          (1- position)
                          'e-chat-context-reference))
                    (1- position)))))
      (when probe
        (let ((reference (get-text-property probe 'e-chat-context-reference))
              (start probe)
              (end (1+ probe)))
          (while (and (> start start-limit)
                      (equal (get-text-property
                              (1- start)
                              'e-chat-context-reference)
                             reference))
            (setq start (1- start)))
          (while (and (< end end-limit)
                      (equal (get-text-property
                              end
                              'e-chat-context-reference)
                             reference))
            (setq end (1+ end)))
          (cons start end))))))

(defun e-chat--delete-context-reference-at (position)
  "Delete the inline context reference adjacent to POSITION, when present."
  (when-let ((bounds (e-chat--context-reference-bounds-at position)))
    (let ((inhibit-read-only t))
      (delete-region (car bounds) (cdr bounds)))
    t))

(defun e-chat-delete-backward-char (arg &optional killp)
  "Delete backward ARG chars, removing adjacent context atoms as units.
KILLP is passed through to `delete-char' for normal text."
  (interactive "p\nP")
  (unless (and (= arg 1)
               (e-chat--delete-context-reference-at (point)))
    (delete-char (- arg) killp)))

(defun e-chat-delete-forward-char (arg &optional killp)
  "Delete forward ARG chars, removing adjacent context atoms as units.
KILLP is passed through to `delete-char' for normal text."
  (interactive "p\nP")
  (unless (and (= arg 1)
               (e-chat--delete-context-reference-at (point)))
    (delete-char arg killp)))

(defun e-chat--xml-attribute-escape (value)
  "Return VALUE escaped for a compact XML-like attribute."
  (let ((text (format "%s" (or value ""))))
    (setq text (replace-regexp-in-string "&" "&amp;" text t t))
    (setq text (replace-regexp-in-string "\"" "&quot;" text t t))
    (setq text (replace-regexp-in-string "<" "&lt;" text t t))
    (replace-regexp-in-string ">" "&gt;" text t t)))

(defun e-chat--reference-placeholder (reference)
  "Return inline model-facing placeholder for REFERENCE."
  (format "<reference id=\"%s\" label=\"%s\">"
          (e-chat--xml-attribute-escape (plist-get reference :id))
          (e-chat--xml-attribute-escape (plist-get reference :label))))

(defun e-chat-reference-placeholder (reference)
  "Return the model-facing inline placeholder for REFERENCE."
  (e-chat--reference-placeholder reference))

(defun e-chat--reference-text-lines (text)
  "Return TEXT split into content lines, ignoring one trailing newline."
  (let ((lines (split-string (replace-regexp-in-string
                              "\r" "" (or text "") t t)
                             "\n")))
    (if (and (cdr lines)
             (string-empty-p (car (last lines))))
        (butlast lines)
      lines)))

(defun e-chat--point-context-reference-body (reference)
  "Return a line-numbered body for point-context REFERENCE."
  (let* ((start-line (plist-get reference :start-line))
         (end-line (plist-get reference :end-line))
         (point-line (plist-get reference :point-line))
         (width (length (number-to-string (or end-line start-line 0))))
         (line-number start-line)
         lines)
    (dolist (line (e-chat--reference-text-lines (plist-get reference :text)))
      (push (format "%s %s | %s"
                    (if (= line-number point-line) ">" " ")
                    (format (format "%%%dd" width) line-number)
                    line)
            lines)
      (setq line-number (1+ line-number)))
    (format "Context lines %d-%d; focused line %d:\n%s"
            start-line
            end-line
            point-line
            (string-join (nreverse lines) "\n"))))

(defun e-chat--reference-body (reference)
  "Return model-facing body text for REFERENCE."
  (if (plist-get reference :point-context)
      (e-chat--point-context-reference-body reference)
    (plist-get reference :text)))

(defun e-chat--reference-section-entry (reference)
  "Return model-facing reference body for REFERENCE."
  (format "[%s] %s (%s)\n%s"
          (plist-get reference :id)
          (plist-get reference :label)
          (plist-get reference :uri)
          (e-chat--reference-body reference)))

(defun e-chat-format-reference-prompt (text references)
  "Return TEXT with model-facing REFERENCES appended."
  (let ((references (delq nil references)))
    (if references
        (string-trim
         (concat
          text
          "\n\nReferences:\n"
          (mapconcat #'e-chat--reference-section-entry
                     references
                     "\n\n")))
      (string-trim text))))

(defun e-chat--composer-document ()
  "Return composer prompt text and ordered inline reference records."
  (unless (e-chat--composer-active-p)
    (user-error "No active e chat composer"))
  (let ((position (marker-position e-chat--composer-start-marker))
        (end (point-max))
        segments
        references)
    (while (< position end)
      (let ((reference (get-text-property position 'e-chat-context-reference)))
        (if reference
            (let ((next (or (next-single-property-change
                             position 'e-chat-context-reference nil end)
                            end)))
              (push (e-chat--reference-placeholder reference) segments)
              (push (copy-tree reference) references)
              (setq position next))
          (let ((next (or (next-single-property-change
                           position 'e-chat-context-reference nil end)
                          end)))
            (push (buffer-substring-no-properties position next) segments)
            (setq position next)))))
    (list :text (string-trim (apply #'concat (nreverse segments)))
          :references (nreverse references))))

(defun e-chat--composer-submission ()
  "Return submit-ready prompt and ordered reference metadata."
  (let* ((document (e-chat--composer-document))
         (text (plist-get document :text))
         (references (plist-get document :references)))
    (when references
      (setq text (e-chat-format-reference-prompt text references)))
    (list :prompt text :references references)))

(defun e-chat--ensure-turn-registry ()
  "Ensure turn navigation state exists for the current chat buffer."
  (unless (hash-table-p e-chat--turn-registry)
    (setq e-chat--turn-registry (make-hash-table :test 'equal)))
  e-chat--turn-registry)

(defun e-chat--ensure-block-registry ()
  "Ensure block navigation state exists for the current chat buffer."
  (unless (hash-table-p e-chat--block-registry)
    (setq e-chat--block-registry (make-hash-table :test 'equal)))
  e-chat--block-registry)

(defun e-chat--turn-record (turn-id)
  "Return mutable metadata for TURN-ID, creating it when needed."
  (when turn-id
    (let ((registry (e-chat--ensure-turn-registry)))
      (or (gethash turn-id registry)
          (let ((record (list :id turn-id
                              :started-at nil
                              :ended-at nil
                              :activity-records nil
                              :intermittent-entries nil
                              :failure-error nil
                              :failure-details nil
                              :failure-rendered nil
                              :transient-start-marker nil
                              :transient-end-marker nil
                              :activity-block-id nil
                              :response-separator-rendered nil
                              :final-rendered nil)))
            (puthash turn-id record registry)
            record)))))

(defun e-chat--existing-turn-record (turn-id)
  "Return existing metadata for TURN-ID, or nil."
  (when (and turn-id (hash-table-p e-chat--turn-registry))
    (gethash turn-id e-chat--turn-registry)))

(defun e-chat--next-block-id ()
  "Return a new display-local block id."
  (setq e-chat--block-counter (1+ e-chat--block-counter))
  (format "block-%d" e-chat--block-counter))

(defun e-chat--block-record (block-id turn-id)
  "Return mutable metadata for BLOCK-ID belonging to TURN-ID."
  (let ((registry (e-chat--ensure-block-registry)))
    (or (gethash block-id registry)
        (let ((record (list :id block-id
                            :turn-id turn-id
                            :kind nil
                            :action-text nil
                            :details-text nil
                            :content-start-marker nil
                            :content-end-marker nil
                            :tool-items nil
                            :tool-list-start-marker nil
                            :tool-list-end-marker nil
                            :start-marker nil
                            :end-marker nil)))
          (puthash block-id record registry)
          (setq e-chat--block-order (append e-chat--block-order (list block-id)))
          record))))

(defun e-chat--remove-block-record (block-id)
  "Remove BLOCK-ID from rendered block metadata."
  (when block-id
    (when (hash-table-p e-chat--block-registry)
      (remhash block-id e-chat--block-registry))
    (setq e-chat--block-order (delete block-id e-chat--block-order))
    (when (equal e-chat--focused-block-id block-id)
      (setq e-chat--focused-block-id nil)
      (setq e-chat--focused-turn-id nil)
      (when (overlayp e-chat--focused-turn-overlay)
        (delete-overlay e-chat--focused-turn-overlay)))
    (when (equal e-chat--block-view-block-id block-id)
      (setq e-chat--block-view-block-id nil))
    (when (equal e-chat--tool-list-block-id block-id)
      (setq e-chat--tool-list-block-id nil)
      (setq e-chat--tool-list-index 0)
      (when (overlayp e-chat--tool-list-overlay)
        (delete-overlay e-chat--tool-list-overlay)))))

(defun e-chat--set-turn-time (turn-id field value)
  "Set TURN-ID timing FIELD to VALUE when both are available."
  (when (and turn-id value)
    (plist-put (e-chat--turn-record turn-id) field value)))

(defun e-chat--update-block-bounds
    (block-id turn-id start end
              &optional kind action-text content-start content-end tool-items
              details-text)
  "Set BLOCK-ID bounds START through END and action metadata for TURN-ID.
Optional KIND, ACTION-TEXT, CONTENT-START, CONTENT-END, TOOL-ITEMS, and
DETAILS-TEXT describe block actions."
  (when block-id
    (e-chat--turn-record turn-id)
    (let ((record (e-chat--block-record block-id turn-id)))
      (plist-put record :start-marker (copy-marker start nil))
      (plist-put record :end-marker (copy-marker end nil))
      (when kind
        (plist-put record :kind kind))
      (when action-text
        (plist-put record :action-text action-text))
      (when content-start
        (plist-put record :content-start-marker (copy-marker content-start nil)))
      (when content-end
        (plist-put record :content-end-marker (copy-marker content-end nil)))
      (when tool-items
        (plist-put record :tool-items tool-items))
      (when details-text
        (plist-put record :details-text details-text))
      (when (eq kind 'final)
        (setq e-chat--latest-final-block-id block-id)))))

(defun e-chat--block-at-point ()
  "Return the rendered block id at point, or nil."
  (or (get-text-property (point) 'e-chat-block-id)
      (get-text-property (max (point-min) (1- (point))) 'e-chat-block-id)))

(defun e-chat--last-rendered-block-id ()
  "Return the most recent rendered block id before the composer."
  (cl-find-if #'e-chat--live-block-record (reverse e-chat--block-order)))

(defun e-chat--focus-block (block-id)
  "Focus BLOCK-ID in response navigation mode."
  (let* ((record (and block-id
                      (hash-table-p e-chat--block-registry)
                      (gethash block-id e-chat--block-registry)))
         (start-marker (plist-get record :start-marker))
         (end-marker (plist-get record :end-marker))
         (start (and (markerp start-marker) (marker-position start-marker)))
         (end (and (markerp end-marker) (marker-position end-marker))))
    (unless (and record start end (< start end))
      (user-error "No rendered e chat block to focus"))
    (setq e-chat--focused-block-id block-id)
    (setq e-chat--focused-turn-id (plist-get record :turn-id))
    (unless (overlayp e-chat--focused-turn-overlay)
      (setq e-chat--focused-turn-overlay (make-overlay start end nil t nil)))
    (move-overlay e-chat--focused-turn-overlay start end)
    (overlay-put e-chat--focused-turn-overlay 'face 'e-chat-focused-turn-face)
    (goto-char start)
    (when-let ((window (e-chat--visible-window)))
      (set-window-point window start))
    block-id))

(defun e-chat--move-focused-block (step)
  "Move focused block by STEP in rendered block order."
  (unless e-chat--focused-block-id
    (user-error "No focused e chat block"))
  (let ((live-block-order (cl-remove-if-not #'e-chat--live-block-record
                                            e-chat--block-order))
        remaining
        (index 0)
        found)
    (setq remaining live-block-order)
    (while (and remaining (not found))
      (if (equal (car remaining) e-chat--focused-block-id)
          (setq found index)
        (setq index (1+ index)
              remaining (cdr remaining))))
    (unless found
      (user-error "Focused e chat block is no longer rendered"))
    (let ((next-index (max 0 (min (1- (length live-block-order))
                                  (+ found step)))))
      (e-chat--focus-block (nth next-index live-block-order)))))

(defun e-chat--focused-block ()
  "Return the focused block record."
  (unless e-chat--focused-block-id
    (user-error "No focused e chat block"))
  (or (and (hash-table-p e-chat--block-registry)
           (gethash e-chat--focused-block-id e-chat--block-registry))
      (user-error "Focused e chat block is no longer rendered")))

(defun e-chat--block-kind-for-title (title)
  "Return block kind for rendered entry TITLE."
  (cond
   ((equal title "You") 'user)
   ((equal title "Assistant") 'final)
   ((equal title "System") 'system)
   (t 'system)))

(defun e-chat--block-content-bounds (block)
  "Return content bounds for BLOCK."
  (let* ((start-marker (or (plist-get block :content-start-marker)
                           (plist-get block :start-marker)))
         (end-marker (or (plist-get block :content-end-marker)
                         (plist-get block :end-marker)))
         (start (and (markerp start-marker) (marker-position start-marker)))
         (end (and (markerp end-marker) (marker-position end-marker))))
    (unless (and start end (<= start end))
      (user-error "Focused e chat block has no content bounds"))
    (cons start end)))

(defun e-chat--block-details-bounds (block)
  "Return visible expanded detail bounds for BLOCK, or nil."
  (let* ((start-marker (plist-get block :details-start-marker))
         (end-marker (plist-get block :details-end-marker))
         (start (and (markerp start-marker) (marker-position start-marker)))
         (end (and (markerp end-marker) (marker-position end-marker))))
    (when (and start end (< start end))
      (cons start end))))

(defun e-chat--block-view-bounds (block)
  "Return bounds used by block-local view mode for BLOCK."
  (or (e-chat--block-details-bounds block)
      (e-chat--block-content-bounds block)))

(defun e-chat--block-action-text (block)
  "Return action text for BLOCK."
  (or (plist-get block :action-text)
      (let ((bounds (e-chat--block-content-bounds block)))
        (string-trim-right
         (buffer-substring-no-properties (car bounds) (cdr bounds))))))

(defun e-chat--latest-final-block ()
  "Return the latest final assistant block record."
  (let ((block-id e-chat--latest-final-block-id))
    (unless (and block-id
                 (hash-table-p e-chat--block-registry)
                 (gethash block-id e-chat--block-registry))
      (setq block-id
            (and (hash-table-p e-chat--block-registry)
                 (cl-find-if
                  (lambda (candidate)
                    (eq (plist-get (gethash candidate e-chat--block-registry)
                                   :kind)
                        'final))
                  (reverse e-chat--block-order)))))
    (or (and block-id (gethash block-id e-chat--block-registry))
        (user-error "No final e chat response"))))

(defun e-chat--editable-buffer-mode ()
  "Enable the preferred major mode for editable chat text buffers."
  (if (or (fboundp 'markdown-mode)
          (require 'markdown-mode nil t))
      (markdown-mode)
    (text-mode)))

(defun e-chat--buffer-with-text (name text &optional read-only)
  "Display NAME containing TEXT, optionally READ-ONLY."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (if read-only
          (special-mode)
        (e-chat--editable-buffer-mode))
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

(defun e-chat--format-time-value (value)
  "Return VALUE as a compact display string."
  (cond
   ((numberp value)
    (format-time-string "%Y-%m-%d %H:%M:%S UTC"
                        (seconds-to-time value)
                        t))
   (value (format "%s" value))
   (t "unknown")))

(defun e-chat--time-seconds (value)
  "Return VALUE as seconds when it can be parsed as a time."
  (cond
   ((numberp value) value)
   ((stringp value)
    (condition-case nil
        (float-time (date-to-time value))
      (error nil)))
   (t nil)))

(defun e-chat--format-duration (started-at ended-at)
  "Return duration between STARTED-AT and ENDED-AT."
  (let ((started-seconds (e-chat--time-seconds started-at))
        (ended-seconds (e-chat--time-seconds ended-at)))
    (if (and started-seconds ended-seconds)
        (let* ((seconds (max 0 (truncate (- ended-seconds started-seconds))))
               (minutes (/ seconds 60))
               (remaining (% seconds 60)))
          (format "%dmin %dsec" minutes remaining))
      "unknown")))

(defun e-chat--current-time-seconds ()
  "Return the current time as float seconds."
  (float-time))

(defun e-chat--indent-detail-text (text)
  "Return TEXT with each line indented for expanded turn details."
  (concat "  " (replace-regexp-in-string "\n" "\n  " (string-trim-right text))))

(defun e-chat--intermittent-entry-text (entry)
  "Return display text for intermittent turn ENTRY."
  (format "%s\n%s"
          (plist-get entry :title)
          (plist-get entry :content)))

(defun e-chat--activity-tool-count-text (count)
  "Return collapsed display text for COUNT tool invocations."
  (format "%d tool call%s" count (if (= count 1) "" "s")))

(defun e-chat--activity-records (record)
  "Return semantic activity records for RECORD."
  (plist-get record :activity-records))

(defun e-chat--append-activity-record (record activity-record)
  "Append semantic ACTIVITY-RECORD to RECORD."
  (plist-put record
             :activity-records
             (append (e-chat--activity-records record)
                     (list activity-record)))
  activity-record)

(defun e-chat--last-round-record (record)
  "Return RECORD's latest provider round record."
  (car (last (e-chat--activity-records record))))

(defun e-chat--active-round-record (record)
  "Return RECORD's active provider round record."
  (cl-find-if
   (lambda (activity-record)
     (and (eq (plist-get activity-record :kind) 'round)
          (eq (plist-get activity-record :status) 'active)))
   (reverse (e-chat--activity-records record))))

(defun e-chat--round-record-for-child (record)
  "Return semantic round record that should own the next child event."
  (or (e-chat--active-round-record record)
      (e-chat--last-round-record record)))

(defun e-chat--append-round-reasoning (record content &optional append)
  "Append reasoning CONTENT to RECORD's current round.
When APPEND is non-nil, merge CONTENT into the previous reasoning child."
  (when-let ((round (and content
                         (not (string-empty-p content))
                         (e-chat--round-record-for-child record))))
    (let* ((reasoning (plist-get round :reasoning))
           (last-reasoning (car (last reasoning))))
      (if (and append last-reasoning)
          (plist-put last-reasoning
                     :content
                     (concat (plist-get last-reasoning :content) content))
        (plist-put round
                   :reasoning
                   (append reasoning
                           (list (list :kind 'reasoning
                                       :round (plist-get round :round)
                                       :content content))))))))

(defun e-chat--current-round-tool-batch (round)
  "Return ROUND's current tool batch, creating it when needed."
  (or (car (last (plist-get round :tool-batches)))
      (let ((batch (list :kind 'tool-batch
                         :round (plist-get round :round)
                         :items nil)))
        (plist-put round
                   :tool-batches
                   (append (plist-get round :tool-batches)
                           (list batch)))
        batch)))

(defun e-chat--append-round-tool-call (record payload)
  "Append tool call PAYLOAD to RECORD's current round."
  (when-let ((round (e-chat--round-record-for-child record)))
    (let* ((batch (e-chat--current-round-tool-batch round))
           (items (plist-get batch :items))
           (tool-id (or (plist-get payload :id)
                        (plist-get payload :call-id))))
      (plist-put batch
                 :items
                 (append items
                         (list (list :kind 'tool
                                     :round (plist-get round :round)
                                     :id tool-id
                                     :call (e-chat--format-tool-call payload)
                                     :call-payload payload
                                     :output nil)))))))

(defun e-chat--tool-finished-id (payload)
  "Return the tool id associated with tool-finished PAYLOAD."
  (let ((tool-call (plist-get payload :tool-call)))
    (or (plist-get payload :id)
        (plist-get payload :call-id)
        (plist-get tool-call :id)
        (plist-get tool-call :call-id))))

(defun e-chat--round-tool-items (round)
  "Return all tool items recorded for ROUND."
  (apply #'append
         (mapcar (lambda (batch)
                   (plist-get batch :items))
                 (plist-get round :tool-batches))))

(defun e-chat--find-round-tool-item (record tool-id)
  "Return semantic tool item matching TOOL-ID in RECORD."
  (when tool-id
    (cl-loop for round in (reverse (e-chat--activity-records record))
             thereis
             (cl-find-if
              (lambda (item)
                (equal (plist-get item :id) tool-id))
              (e-chat--round-tool-items round)))))

(defun e-chat--latest-incomplete-tool-item (record)
  "Return RECORD's latest tool item without output."
  (cl-loop for round in (reverse (e-chat--activity-records record))
           thereis
           (cl-find-if
            (lambda (item)
              (not (plist-get item :output)))
            (reverse (e-chat--round-tool-items round)))))

(defun e-chat--string-byte-prefix (text max-bytes)
  "Return TEXT prefix limited to MAX-BYTES UTF-8 bytes."
  (let ((bytes 0)
        (index 0)
        (length (length text)))
    (while (and (< index length)
                (let ((next-bytes
                       (string-bytes (substring text index (1+ index)))))
                  (when (<= (+ bytes next-bytes) max-bytes)
                    (setq bytes (+ bytes next-bytes))
                    t)))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-chat--tool-result-display-text (result)
  "Return compact chat activity display text for tool RESULT."
  (let* ((content (if (e-tools-result-p result)
                      (plist-get result :content)
                    result))
         (text (e-tools-result-content-text content))
         (original-bytes (string-bytes text))
         (max-bytes (max 0 e-chat-tool-activity-preview-bytes))
         (truncated (> original-bytes max-bytes))
         (preview (if truncated
                      (e-chat--string-byte-prefix text max-bytes)
                    text))
         (metadata (and (e-tools-result-p result)
                        (plist-get result :metadata)))
         (uri (or (plist-get metadata :tmp-uri)
                  (plist-get metadata :full-output-path))))
    (if truncated
        (string-trim-right
         (format "%s

[Tool result preview truncated: showing %d of %d bytes%s]"
                 preview
                 (string-bytes preview)
                 original-bytes
                 (if uri (format ". Full output: %s" uri) "")))
      text)))

(defun e-chat--complete-round-tool-result (record payload)
  "Attach tool result PAYLOAD to the matching semantic tool item in RECORD."
  (let* ((tool-id (e-chat--tool-finished-id payload))
         (item (or (e-chat--find-round-tool-item record tool-id)
                   (e-chat--latest-incomplete-tool-item record))))
    (when item
      (plist-put item
                 :output
                 (e-chat--tool-result-display-text
                  (plist-get payload :result))))))

(defun e-chat--round-tool-count (round)
  "Return number of tool calls recorded for ROUND."
  (length (e-chat--round-tool-items round)))

(defun e-chat--activity-record-tool-count (record)
  "Return number of tool calls recorded in semantic RECORD activity."
  (apply #'+
         (mapcar #'e-chat--round-tool-count
                 (e-chat--activity-records record))))

(defun e-chat--normalize-round-status (status)
  "Return presentation round status for provider STATUS."
  (pcase status
    ((or 'error "error" 'failed "failed") 'failed)
    ((or 'cancelled "cancelled") 'cancelled)
    ((or 'active "active" 'started "started") 'active)
    (_ 'done)))

(defun e-chat--thought-content (status started-at ended-at &optional active-at)
  "Return thought line text for STATUS from STARTED-AT to ENDED-AT.
ACTIVE-AT is used for active thinking duration."
  (pcase (e-chat--normalize-round-status status)
    ('active
     (format "%s Thinking for %s"
             (e-chat--progress-dots)
             (e-chat--format-duration
              started-at
              (or active-at (e-chat--current-time-seconds)))))
    ('failed
     (format "Thought failed after %s"
             (e-chat--format-duration started-at ended-at)))
    ('cancelled
     (format "Thought cancelled after %s"
             (e-chat--format-duration started-at ended-at)))
    (_
     (format "Thought for %s"
             (e-chat--format-duration started-at ended-at)))))

(defun e-chat--semantic-tool-items (items)
  "Return display tool-list items for semantic tool ITEMS."
  (mapcar
   (lambda (item)
     (list :call (plist-get item :call)
           :output (plist-get item :output)))
   items))

(defun e-chat--round-thought-text (round)
  "Return visible thought text for semantic ROUND."
  (e-chat--thought-content
   (plist-get round :status)
   (plist-get round :started-at)
   (plist-get round :ended-at)
   (when (eq (e-chat--normalize-round-status
              (plist-get round :status))
             'active)
     (e-chat--current-time-seconds))))

(defun e-chat--activity-round-row-text (left &optional right)
  "Return activity round row with LEFT and optional right-side RIGHT text."
  (if (and right (not (string-empty-p right)))
      (concat left
              (propertize
               " "
               'display
               `(space :align-to (- right ,(string-width right))))
              right)
    left))

(defun e-chat--activity-round-visible-text (round)
  "Return visible text for semantic activity ROUND."
  (let* ((thought (e-chat--round-thought-text round))
         (tool-count (e-chat--round-tool-count round))
         (tool-text (and (> tool-count 0)
                         (e-chat--activity-tool-count-text tool-count)))
         (lines (and thought
                     (list (e-chat--activity-round-row-text
                            thought tool-text))))
         (reasoning-lines nil))
    (dolist (reasoning (plist-get round :reasoning))
      (when-let ((content (plist-get reasoning :content)))
        (unless (string-empty-p content)
          (setq reasoning-lines (append reasoning-lines (list content))))))
    (when reasoning-lines
      (setq lines (append lines (list "") reasoning-lines)))
    (when lines
      (string-join lines "\n"))))

(defun e-chat--activity-record-visible-chunks (record)
  "Return visible activity chunks for semantic activity RECORD."
  (delq nil
        (mapcar #'e-chat--activity-round-visible-text
                (e-chat--activity-records record))))

(defun e-chat--activity-visible-chunks (entries)
  "Return visible collapsed activity chunks for intermittent ENTRIES.
Count tool invocations after the reasoning chunk they followed."
  (let ((chunks nil)
        (current nil)
        (tool-count 0))
    (cl-labels
        ((finish-current
          ()
          (when (> tool-count 0)
            (setq current
                  (append current
                          (list (e-chat--activity-tool-count-text
                                 tool-count))))
            (setq tool-count 0))
          (when current
            (push (string-join current "\n") chunks)
            (setq current nil))))
      (dolist (entry entries)
        (let ((title (plist-get entry :title))
              (content (plist-get entry :content)))
          (pcase title
            ((or "Thinking" "Thought")
             (finish-current)
             (when (and content (not (string-empty-p content)))
               (push content chunks)))
            ("Tool call"
             (setq tool-count (1+ tool-count)))
            ("Tool")
            (_
             (finish-current)
             (when (and content (not (string-empty-p content)))
               (setq current (list content)))))))
      (finish-current)
      (nreverse chunks))))

(defun e-chat--activity-tool-count (record)
  "Return number of tool calls recorded for RECORD."
  (if (e-chat--activity-records record)
      (e-chat--activity-record-tool-count record)
    (cl-count-if
     (lambda (entry)
       (equal (plist-get entry :title) "Tool call"))
     (plist-get record :intermittent-entries))))

(defun e-chat--activity-summary-text (record)
  "Return settled turn summary text for RECORD."
  (when (and (plist-get record :started-at)
             (plist-get record :ended-at)
             (plist-get record :has-provider-activity))
    (let* ((duration (e-chat--format-duration
                      (plist-get record :started-at)
                      (plist-get record :ended-at)))
           (tool-count (e-chat--activity-tool-count record))
           (tool-text (cond
                       ((= tool-count 0) "")
                       ((= tool-count 1) ", 1 tool call")
                       (t (format ", %d tool calls" tool-count)))))
      (format "Turn took %s%s." duration tool-text))))

(defun e-chat--activity-expanded-text (record)
  "Return expanded per-line activity history for RECORD."
  (if (e-chat--activity-records record)
      (when-let ((chunks (e-chat--activity-record-visible-chunks record)))
        (when chunks
          (concat (string-join
                   chunks
                   (concat "\n" e-chat--activity-separator "\n"))
                  "\n\n")))
    (when-let ((chunks (e-chat--activity-visible-chunks
                        (plist-get record :intermittent-entries))))
      (when chunks
        (concat (mapconcat #'identity chunks "\n") "\n\n")))))

(defun e-chat--intermittent-details-text (record)
  "Return expanded intermittent details text for RECORD."
  (when-let ((entries (plist-get record :intermittent-entries)))
    (concat
     (mapconcat
      (lambda (entry)
        (e-chat--indent-detail-text (e-chat--intermittent-entry-text entry)))
      entries
      "\n\n")
     "\n\n")))

(defun e-chat--activity-summary-details-text (turn-id record)
  "Return inline details text for TURN-ID's settled activity summary."
  (concat
   (or (e-chat--activity-expanded-text record) "")
   (format "Turn: %s\nStarted: %s\nEnded: %s\nDuration: %s\n"
           turn-id
           (e-chat--format-time-value (plist-get record :started-at))
           (e-chat--format-time-value (plist-get record :ended-at))
           (e-chat--format-duration (plist-get record :started-at)
                                    (plist-get record :ended-at)))))

(defun e-chat--activity-summary-child-records (record)
  "Return navigable child block descriptors for RECORD's activity summary."
  (let (children)
    (dolist (round (e-chat--activity-records record))
      (when-let ((thought (e-chat--round-thought-text round)))
        (unless (equal thought "Thinking...")
          (push (list :kind 'activity-thought
                      :text thought
                      :action-text thought)
                children)))
      (dolist (reasoning (plist-get round :reasoning))
        (when-let ((content (plist-get reasoning :content)))
          (unless (string-empty-p content)
            (push (list :kind 'activity-reasoning
                        :text content
                        :action-text content)
                  children))))
      (dolist (batch (plist-get round :tool-batches))
        (let* ((items (plist-get batch :items))
               (count (length items)))
          (when (> count 0)
            (let ((text (e-chat--activity-tool-count-text count)))
              (push (list :kind 'activity-tool-batch
                          :text text
                          :action-text text
                          :tool-items (e-chat--semantic-tool-items items))
                    children))))))
    (nreverse children)))

(defun e-chat--failure-details-text (record)
  "Return expanded failure details text for RECORD."
  (when-let ((error-message (plist-get record :failure-error)))
    (concat
     (e-chat--indent-detail-text
      (format "Failure\n%s" error-message))
     "\n\n"
     (when-let ((details (plist-get record :failure-details)))
       (concat
        (e-chat--indent-detail-text
         (format "Provider details\n%s" (pp-to-string details)))
        "\n\n")))))

(defun e-chat--activity-tool-items (record)
  "Return tool call/output items derived from RECORD."
  (if (e-chat--activity-records record)
      (mapcar
       (lambda (item)
         (list :call (plist-get item :call)
               :output (plist-get item :output)))
       (apply #'append
              (mapcar #'e-chat--round-tool-items
                      (e-chat--activity-records record))))
    (let ((items nil)
          current)
      (dolist (entry (plist-get record :intermittent-entries))
        (pcase (plist-get entry :title)
          ("Tool call"
           (when current
             (push current items))
           (setq current (list :call (plist-get entry :content)
                               :output nil)))
          ("Tool"
           (if current
               (progn
                 (plist-put current :output (plist-get entry :content))
                 (push current items)
                 (setq current nil))
             (push (list :call "Tool" :output (plist-get entry :content))
                   items)))))
      (when current
        (push current items))
      (nreverse items))))

(defun e-chat--settled-activity-p (record)
  "Return non-nil when RECORD has current activity to keep after final output."
  (e-chat--activity-summary-text record))

(defun e-chat--transient-text (record)
  "Return visible transient text for RECORD."
  (if (e-chat--activity-records record)
      (when-let ((chunks (e-chat--activity-record-visible-chunks record)))
        (when chunks
          (concat (string-join
                   chunks
                   (concat "\n" e-chat--activity-separator "\n"))
                  "\n\n")))
    (when-let ((entries (plist-get record :intermittent-entries)))
      (let ((chunks (e-chat--activity-visible-chunks entries)))
        (when chunks
          (concat (mapconcat #'identity chunks "\n\n") "\n\n"))))))

(defun e-chat--append-activity-entry (record entry)
  "Append structured activity ENTRY to RECORD."
  (plist-put record
             :intermittent-entries
             (append (plist-get record :intermittent-entries)
                     (list entry))))

(defun e-chat--current-activity-round (record)
  "Return RECORD's current LLM round number."
  (or (plist-get record :activity-round) 0))

(defun e-chat--record-provider-started (turn-id created-at)
  "Record provider request start for TURN-ID at CREATED-AT."
  (let* ((record (e-chat--turn-record turn-id))
         (round (1+ (e-chat--current-activity-round record))))
    (plist-put record :has-provider-activity t)
    (plist-put record :activity-round round)
    (e-chat--append-activity-record
     record
     (list :kind 'round
           :round round
           :started-at created-at
           :ended-at nil
           :status 'active
           :reasoning nil
           :tool-batches nil))
    (e-chat--append-activity-entry
     record
     (list :title "Thinking"
           :kind 'thinking
           :round round
           :status 'active
           :started-at created-at
           :content "Thinking..."
           :source 'activity))
    record))

(defun e-chat--record-provider-finished (turn-id created-at &optional status)
  "Record provider request finish for TURN-ID at CREATED-AT.
STATUS defaults to `done'."
  (when-let ((record (e-chat--existing-turn-record turn-id)))
    (let ((round (e-chat--active-round-record record))
          (status (e-chat--normalize-round-status (or status 'done))))
      (when round
        (plist-put round :status status)
        (plist-put round :ended-at created-at)))
    (let ((entry
           (cl-find-if
            (lambda (candidate)
              (and (eq (plist-get candidate :kind) 'thinking)
                   (eq (plist-get candidate :status) 'active)))
            (reverse (plist-get record :intermittent-entries)))))
      (when entry
        (plist-put entry :title "Thought")
        (plist-put entry :status
                   (e-chat--normalize-round-status (or status 'done)))
        (plist-put entry :ended-at created-at)
        (plist-put entry :content
                   (e-chat--thought-content
                    (plist-get entry :status)
                    (plist-get entry :started-at)
                    created-at))))
    record))

(defun e-chat--latest-open-round-record (record)
  "Return RECORD's latest non-terminal provider round."
  (cl-find-if
   (lambda (round)
     (memq (plist-get round :status) '(active error)))
   (reverse (e-chat--activity-records record))))

(defun e-chat--settle-open-thinking (turn-id ended-at status)
  "Settle TURN-ID's open thinking round at ENDED-AT with STATUS."
  (when-let ((record (e-chat--existing-turn-record turn-id)))
    (let ((status (e-chat--normalize-round-status status)))
      (when-let ((round (e-chat--latest-open-round-record record)))
        (plist-put round :status status)
        (plist-put round :ended-at ended-at))
      (when-let ((entry
                  (cl-find-if
                   (lambda (candidate)
                     (and (eq (plist-get candidate :kind) 'thinking)
                          (memq (plist-get candidate :status)
                                '(active error))))
                   (reverse (plist-get record :intermittent-entries)))))
        (plist-put entry :title "Thought")
        (plist-put entry :status status)
        (plist-put entry :ended-at ended-at)
        (plist-put entry :content
                   (e-chat--thought-content
                    status
                    (plist-get entry :started-at)
                    ended-at))))
    record))

(defun e-chat--record-reasoning-delta (record content &optional append source)
  "Record reasoning CONTENT in RECORD and its semantic activity records."
  (e-chat--append-round-reasoning record content append)
  (e-chat--add-intermittent-entry record "Reasoning" content append source))

(defun e-chat--record-tool-started (record payload &optional source)
  "Record tool-started PAYLOAD in RECORD."
  (e-chat--append-round-tool-call record payload)
  (e-chat--add-intermittent-entry
   record
   "Tool call"
   (e-chat--format-tool-call payload)
   nil
   source))

(defun e-chat--record-tool-finished (record payload &optional source)
  "Record tool-finished PAYLOAD in RECORD."
  (e-chat--complete-round-tool-result record payload)
  (e-chat--add-intermittent-entry
   record
   "Tool"
   (e-chat--tool-result-display-text (plist-get payload :result))
   nil
   source))

(defun e-chat--intermittent-entry-exists-p (record title content &optional source)
  "Return non-nil when RECORD already has TITLE and CONTENT.
When SOURCE is non-nil, only match entries from that source."
  (cl-some
   (lambda (entry)
     (and (equal (plist-get entry :title) title)
          (equal (plist-get entry :content) content)
          (or (not source)
              (eq (plist-get entry :source) source))))
   (plist-get record :intermittent-entries)))

(defun e-chat--remove-intermittent-entry (record title content source)
  "Remove intermittent RECORD entries matching TITLE, CONTENT, and SOURCE."
  (plist-put
   record
   :intermittent-entries
   (cl-remove-if
    (lambda (entry)
      (and (equal (plist-get entry :title) title)
           (equal (plist-get entry :content) content)
           (eq (plist-get entry :source) source)))
    (plist-get record :intermittent-entries))))

(defun e-chat--add-intermittent-entry (record title content &optional append source)
  "Add intermittent TITLE and CONTENT to RECORD.
When APPEND is non-nil, merge CONTENT into the previous entry with TITLE.
SOURCE identifies where the entry came from for duplicate suppression."
  (when (and record content (not (string-empty-p content)))
    (when (eq source 'activity)
      (e-chat--remove-intermittent-entry record title content 'transcript))
    (let* ((entries (plist-get record :intermittent-entries))
           (last-entry (car (last entries))))
      (if (and append
               last-entry
               (equal (plist-get last-entry :title) title))
          (plist-put last-entry
                     :content
                     (concat (plist-get last-entry :content) content))
        (plist-put record
                   :intermittent-entries
                   (append entries
                           (list (list :title title
                                       :content content
                                       :source source))))))))

(defun e-chat--delete-running-status (record)
  "Delete the currently visible running-turn status region for RECORD.
When RECORD is nil, clear only buffer-local status markers."
  (let* ((running-start (and (markerp e-chat--running-status-start-marker)
                             (marker-position
                              e-chat--running-status-start-marker)))
         (start (or running-start
                    (and (markerp e-chat--progress-start-marker)
                         (marker-position e-chat--progress-start-marker))
                    (and record
                         (markerp (plist-get record :transient-start-marker))
                         (marker-position
                          (plist-get record :transient-start-marker)))))
         (end (or (and running-start
                       (markerp e-chat--composer-spacer-marker)
                       (marker-position e-chat--composer-spacer-marker))
                  (and (markerp e-chat--running-status-end-marker)
                       (marker-position e-chat--running-status-end-marker))
                  (and record
                       (markerp (plist-get record :transient-end-marker))
                       (marker-position
                        (plist-get record :transient-end-marker)))
                  (and (markerp e-chat--progress-end-marker)
                       (marker-position e-chat--progress-end-marker)))))
    (when (and start end (< start end))
      (let ((inhibit-read-only t))
        (delete-region start end))))
  (setq e-chat--running-status-start-marker nil)
  (setq e-chat--running-status-end-marker nil)
  (setq e-chat--progress-start-marker nil)
  (setq e-chat--progress-end-marker nil)
  (when record
    (plist-put record :transient-start-marker nil)
    (plist-put record :transient-end-marker nil)))

(defun e-chat--clear-running-status-markers ()
  "Clear buffer-local running status markers without deleting text."
  (setq e-chat--running-status-start-marker nil)
  (setq e-chat--running-status-end-marker nil)
  (setq e-chat--progress-start-marker nil)
  (setq e-chat--progress-end-marker nil))

(defun e-chat--delete-turn-transient (record)
  "Delete the currently visible transient block for RECORD."
  (e-chat--delete-running-status record)
  (when-let ((activity-block-id (and record
                                     (plist-get record :activity-block-id))))
    (e-chat--remove-block-record activity-block-id)
    (plist-put record :activity-block-id nil)))

(defun e-chat--live-block-record (block-id)
  "Return live block record for BLOCK-ID, or nil."
  (when-let ((record (and block-id
                          (hash-table-p e-chat--block-registry)
                          (gethash block-id e-chat--block-registry))))
    (let* ((start-marker (plist-get record :start-marker))
           (end-marker (plist-get record :end-marker))
           (start (and (markerp start-marker)
                       (marker-position start-marker)))
           (end (and (markerp end-marker)
                     (marker-position end-marker))))
      (when (and start end (< start end))
        record))))

(defun e-chat--capture-running-status-navigation-state ()
  "Capture chat-local navigation state before running status redraw."
  (cond
   (e-chat-tool-list-mode
    (list :mode 'tool-list
          :block-id e-chat--tool-list-block-id
          :index e-chat--tool-list-index))
   (e-chat-block-view-mode
    (let* ((block-id e-chat--block-view-block-id)
           (block (e-chat--live-block-record block-id))
           (bounds (and block (e-chat--block-view-bounds block))))
      (list :mode 'block-view
            :block-id block-id
            :offset (and bounds (max 0 (- (point) (car bounds)))))))
   (e-chat-response-navigation-mode
    (list :mode 'response-navigation
          :block-id e-chat--focused-block-id))))

(defun e-chat--restore-running-status-navigation-state (state)
  "Restore chat-local navigation STATE after running status redraw."
  (pcase (plist-get state :mode)
    ('response-navigation
     (when (e-chat--live-block-record (plist-get state :block-id))
       (e-chat-response-navigation-mode 1)
       (e-chat--focus-block (plist-get state :block-id))))
    ('block-view
     (let* ((block-id (plist-get state :block-id))
            (block (e-chat--live-block-record block-id))
            (bounds (and block (e-chat--block-view-bounds block))))
       (when bounds
         (e-chat-response-navigation-mode -1)
         (setq e-chat--focused-block-id block-id)
         (setq e-chat--focused-turn-id (plist-get block :turn-id))
         (setq e-chat--block-view-block-id block-id)
         (e-chat-block-view-mode 1)
         (goto-char (min (cdr bounds)
                         (+ (car bounds)
                            (or (plist-get state :offset) 0)))))))
    ('tool-list
     (let* ((block-id (plist-get state :block-id))
            (block (e-chat--live-block-record block-id))
            (items (and block (plist-get block :tool-items))))
       (cond
        (items
         (let ((index (min (max 0 (or (plist-get state :index) 0))
                           (1- (length items)))))
           (e-chat--open-tool-list block)
           (setq e-chat--tool-list-index index)
           (e-chat--focus-tool-list-item)))
        (block
         (e-chat-response-navigation-mode 1)
         (e-chat--focus-block block-id)))))))

(defun e-chat--render-running-status (turn-id record)
  "Render TURN-ID's active progress and RECORD transient summary together."
  (let* ((has-progress (and e-chat--progress-turn-id
                            (equal turn-id e-chat--progress-turn-id)))
         (final-rendered (and record (plist-get record :final-rendered)))
         (summary-text (and final-rendered
                            record
                            (e-chat--activity-summary-text record)))
         (transient-text (and record (e-chat--transient-text record)))
         (text (if final-rendered
                   (or (and summary-text
                            (concat summary-text "\n\n"))
                       transient-text)
                 transient-text))
         (block-kind (if summary-text 'activity-summary 'activity))
         (details-text (and summary-text
                            (e-chat--activity-summary-details-text
                             turn-id record)))
         (navigation-state
          (e-chat--capture-running-status-navigation-state))
         (composer-state (e-chat--capture-composer-state)))
    (when (and turn-id
               (not final-rendered)
               (e-chat--active-activity-p record))
      (e-chat--ensure-progress-timer turn-id))
    (e-chat--delete-running-status record)
    (e-chat--delete-composer)
    (if (or has-progress text)
        (progn
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (or (bobp) (bolp))
              (insert "\n"))
            (e-chat--maybe-insert-response-separator turn-id 'agent)
            (let ((status-start (point)))
              (when text
                (let* ((transient-start (point))
                       (activity-block-id
                        (or (plist-get record :activity-block-id)
                            (let ((id (e-chat--next-block-id)))
                              (plist-put record :activity-block-id id)
                              id))))
                  (e-chat--insert-protected
                   text
                   'e-chat-system-face
                   `(e-chat-transient-turn-id ,turn-id
                     e-chat-turn-id ,turn-id
                     e-chat-block-id ,activity-block-id))
                  (e-chat--apply-activity-separator-face
                   transient-start
                   (point))
                  (plist-put record
                             :transient-start-marker
                             (copy-marker transient-start nil))
                  (plist-put record
                             :transient-end-marker
                             (copy-marker (point) nil))
                  (when activity-block-id
                    (e-chat--update-block-bounds
                     activity-block-id
                     turn-id
                     transient-start
                     (point)
                     block-kind
                     (string-trim-right text)
                     transient-start
                     (point)
                     (e-chat--activity-tool-items record)
                     details-text))))
              (when (and has-progress (not text))
                (let ((progress-start (point)))
                  (e-chat--insert-protected
                   (e-chat--entry-text "Assistant" (e-chat--progress-dots))
                   'e-chat-assistant-face
                   `(e-chat-progress-turn-id ,turn-id))
                  (setq e-chat--progress-start-marker
                        (copy-marker progress-start nil))
                  (setq e-chat--progress-end-marker
                        (copy-marker (point) nil))))
              (setq e-chat--running-status-start-marker
                    (copy-marker status-start nil))
              (setq e-chat--running-status-end-marker
                    (copy-marker (point) nil))))
          (e-chat--restore-composer-state composer-state))
      (e-chat--restore-composer-state composer-state))
    (e-chat--restore-running-status-navigation-state navigation-state)))

(defun e-chat--render-turn-transient (turn-id record)
  "Render RECORD's intermittent entries as a temporary block for TURN-ID."
  (e-chat--profile-call
   'chat.render-turn-transient
   (list :session-id e-chat-session-id
         :turn-id turn-id
         :buffer-name (buffer-name))
   (lambda ()
     (e-chat--render-running-status turn-id record))))

(defun e-chat--cancel-pending-activity-redraw (&optional turn-id)
  "Cancel the pending activity redraw.
When TURN-ID is non-nil, cancel only a redraw for that turn."
  (when (and e-chat--pending-activity-redraw-turn-id
             (or (not turn-id)
                 (equal turn-id e-chat--pending-activity-redraw-turn-id)))
    (when (timerp e-chat--pending-activity-redraw-timer)
      (cancel-timer e-chat--pending-activity-redraw-timer))
    (setq e-chat--pending-activity-redraw-turn-id nil)
    (setq e-chat--pending-activity-redraw-timer nil)
    (setq e-chat--pending-activity-redraw-kind nil)))

(defun e-chat--run-pending-activity-redraw ()
  "Run and clear the pending activity redraw for this chat buffer."
  (let ((turn-id e-chat--pending-activity-redraw-turn-id)
        (timer e-chat--pending-activity-redraw-timer)
        (kind e-chat--pending-activity-redraw-kind))
    (e-chat--profile-call
     'chat.activity-redraw
     (list :session-id e-chat-session-id
           :turn-id turn-id
           :buffer-name (buffer-name)
           :metadata (list :kind (and kind (symbol-name kind))))
     (lambda ()
       (when (timerp timer)
         (cancel-timer timer))
       (setq e-chat--pending-activity-redraw-turn-id nil)
       (setq e-chat--pending-activity-redraw-timer nil)
       (setq e-chat--pending-activity-redraw-kind nil)
       (when turn-id
         (pcase kind
           ('progress
            (e-chat--render-progress-indicator turn-id))
           (_
            (when-let ((record (e-chat--existing-turn-record turn-id)))
              (e-chat--render-turn-transient turn-id record)))))))))

(defun e-chat--activity-redraw-kind (existing requested)
  "Return coalesced redraw kind from EXISTING and REQUESTED kinds."
  (cond
   ((eq existing 'activity) 'activity)
   ((eq requested 'activity) 'activity)
   (requested)
   (existing)
   (t 'activity)))

(defun e-chat--request-activity-redraw (turn-id &optional kind)
  "Schedule one near-future activity redraw for TURN-ID."
  (when turn-id
    (setq e-chat--pending-activity-redraw-turn-id turn-id)
    (setq e-chat--pending-activity-redraw-kind
          (e-chat--activity-redraw-kind
           e-chat--pending-activity-redraw-kind
           (or kind 'activity)))
    (unless (timerp e-chat--pending-activity-redraw-timer)
      (let ((buffer (current-buffer)))
        (setq e-chat--pending-activity-redraw-timer
              (run-at-time
               e-chat-activity-redraw-delay
               nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (e-chat--run-pending-activity-redraw))))))))))

(defun e-chat--progress-dots ()
  "Return the current active assistant progress glyph string."
  (aref e-chat--progress-glyphs
        (mod e-chat--progress-frame
             (length e-chat--progress-glyphs))))

(defun e-chat--active-activity-p (record)
  "Return non-nil when RECORD has an active provider activity round."
  (and record (e-chat--active-round-record record)))

(defun e-chat--cancel-progress-timer ()
  "Cancel the active assistant progress timer."
  (when (timerp e-chat--progress-timer)
    (cancel-timer e-chat--progress-timer))
  (setq e-chat--progress-timer nil))

(defun e-chat--progress-timer-active-p (turn-id)
  "Return non-nil when TURN-ID has a live progress timer."
  (and (equal e-chat--progress-turn-id turn-id)
       (timerp e-chat--progress-timer)))

(defun e-chat--ensure-progress-timer (turn-id)
  "Ensure TURN-ID has a live progress timer without rendering immediately."
  (unless (e-chat--progress-timer-active-p turn-id)
    (let ((same-turn (equal e-chat--progress-turn-id turn-id)))
      (e-chat--cancel-progress-timer)
      (setq e-chat--progress-turn-id turn-id)
      (unless same-turn
        (setq e-chat--progress-frame 0))
      (setq e-chat--progress-next-tick-time
            (+ (float-time) e-chat-progress-interval))
      (let ((buffer (current-buffer)))
        (setq e-chat--progress-timer
              (run-at-time e-chat-progress-interval
                           e-chat-progress-interval
                           (lambda ()
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (e-chat--advance-progress-indicator))))))))))

(defun e-chat--delete-progress-indicator ()
  "Delete the active assistant progress indicator."
  (let ((record (and e-chat--progress-turn-id
                     (e-chat--existing-turn-record
                      e-chat--progress-turn-id))))
    (e-chat--delete-running-status record)))

(defun e-chat--render-progress-indicator (turn-id)
  "Render active assistant progress indicator for TURN-ID."
  (e-chat--render-running-status
   turn-id
   (e-chat--existing-turn-record turn-id)))

(defun e-chat--advance-progress-indicator ()
  "Advance and rerender the active assistant progress indicator."
  (when e-chat--progress-turn-id
    (let* ((now (float-time))
           (late-by (and e-chat--progress-next-tick-time
                         (- now e-chat--progress-next-tick-time)))
           (threshold (max 5.0 (* 3 e-chat-progress-interval))))
      (when (and late-by (> late-by threshold))
        (e-chat--set-status
         (format "Emacs was blocked for %.0fs; checking turn state"
                 late-by)))
      (setq e-chat--progress-next-tick-time
            (+ now e-chat-progress-interval)))
    (setq e-chat--progress-frame (1+ e-chat--progress-frame))
    (e-chat--request-activity-redraw e-chat--progress-turn-id 'progress)))

(defun e-chat--start-progress-indicator (turn-id)
  "Start the active assistant progress indicator for TURN-ID."
  (setq e-chat--progress-frame 0)
  (e-chat--ensure-progress-timer turn-id)
  (e-chat--render-progress-indicator turn-id))

(defun e-chat--stop-progress-indicator (&optional turn-id)
  "Stop and delete the active assistant progress indicator.
When TURN-ID is non-nil, only stop a matching active indicator."
  (when (and e-chat--progress-turn-id
             (or (not turn-id)
                 (equal turn-id e-chat--progress-turn-id)))
    (e-chat--cancel-progress-timer)
    (let ((old-turn-id e-chat--progress-turn-id))
      (setq e-chat--progress-turn-id nil)
      (setq e-chat--progress-frame 0)
      (setq e-chat--progress-next-tick-time nil)
      (if-let ((record (and old-turn-id
                            (e-chat--existing-turn-record old-turn-id))))
          (e-chat--render-running-status old-turn-id record)
        (progn
          (e-chat--delete-composer)
          (e-chat--delete-running-status nil)
          (unless (derived-mode-p 'special-mode)
            (e-chat--insert-composer)))))))

(defun e-chat--append-intermittent-entry (turn-id title content &optional append source)
  "Append intermittent TITLE and CONTENT to TURN-ID.
When APPEND is non-nil, merge CONTENT into the previous entry with TITLE.
SOURCE identifies where the entry came from for duplicate suppression."
  (when (and turn-id content (not (string-empty-p content)))
    (let ((record (e-chat--turn-record turn-id)))
      (e-chat--add-intermittent-entry record title content append source)
      (e-chat--render-turn-transient turn-id record))))

(defun e-chat--format-tool-call (payload)
  "Return a compact display string for tool-call PAYLOAD."
  (let ((name (plist-get payload :name))
        (arguments (plist-get payload :arguments)))
    (string-join
     (delq nil
           (list (and name (format "%s" name))
                 (and arguments
                      (format "%S" arguments))))
     "\n")))

(defun e-chat--tool-message-p (message)
  "Return non-nil when MESSAGE is a tool transcript message."
  (memq (plist-get message :role) '(tool-call tool)))

(defun e-chat--record-replayed-message-time (record message)
  "Record MESSAGE's replay timestamp into RECORD."
  (when-let ((created-at (plist-get message :created-at)))
    (unless (plist-get record :started-at)
      (plist-put record :started-at created-at))
    (plist-put record :ended-at created-at)))

(defun e-chat--record-activity-event (turn-id activity-event)
  "Record durable ACTIVITY-EVENT for TURN-ID without re-emitting it."
  (let ((record (e-chat--turn-record turn-id)))
    (pcase (plist-get activity-event :event-type)
      ('turn-started
       (e-chat--set-turn-time turn-id
                              :started-at
                              (plist-get activity-event :created-at)))
      ('provider-request-started
       (e-chat--record-provider-started
        turn-id
        (plist-get activity-event :created-at)))
      ('provider-request-finished
       (e-chat--record-provider-finished
        turn-id
        (plist-get activity-event :created-at)
        (plist-get (plist-get activity-event :payload) :status)))
      ('turn-finished
       (e-chat--set-turn-time turn-id
                              :ended-at
                              (plist-get activity-event :created-at)))
      ('reasoning-delta
       (e-chat--record-reasoning-delta
        record
        (plist-get (plist-get activity-event :payload) :content)
        t
        'activity))
      ('tool-started
       (e-chat--record-tool-started
        record
        (plist-get activity-event :payload)
        'activity))
      ('tool-finished
       (e-chat--record-tool-finished
        record
        (plist-get activity-event :payload)
        'activity))
      ('turn-failed
       (e-chat--settle-open-thinking
        turn-id
        (plist-get activity-event :created-at)
        'failed)
       (e-chat--record-turn-failure
        turn-id
        (plist-get activity-event :payload)))
      ('turn-cancelled
       (e-chat--settle-open-thinking
        turn-id
        (plist-get activity-event :created-at)
        'cancelled)))))

(defun e-chat--render-turn-activity-events (turn-id activity-events)
  "Render durable ACTIVITY-EVENTS for TURN-ID once."
  (when-let ((record (e-chat--turn-record turn-id)))
    (unless (plist-get record :activity-rendered)
      (dolist (event activity-events)
        (when (equal (plist-get event :turn-id) turn-id)
          (e-chat--record-activity-event turn-id event)))
      (plist-put record :activity-rendered t)
      (e-chat--render-turn-transient turn-id record))))

(defun e-chat--record-turn-failure (turn-id payload)
  "Record failed-turn PAYLOAD for TURN-ID."
  (when-let ((record (e-chat--turn-record turn-id)))
    (plist-put record :failure-error
               (or (plist-get payload :error) "Turn failed"))
    (plist-put record :failure-details (plist-get payload :details))
    record))

(defun e-chat--render-turn-failure
    (turn-id created-at payload &optional ensure-composer)
  "Render failed TURN-ID with CREATED-AT and failure PAYLOAD."
  (e-chat--set-turn-time turn-id :ended-at created-at)
  (e-chat--settle-open-thinking turn-id created-at 'failed)
  (e-chat--stop-progress-indicator turn-id)
  (let* ((record (e-chat--record-turn-failure turn-id payload))
         (error-message (or (plist-get payload :error) "Turn failed")))
    (e-chat--insert-entry
     "System"
     (format "Turn failed: %s" error-message)
     ensure-composer
     turn-id
     (and record (e-chat--turn-details-text turn-id record)))
    (when record
      (plist-put record :failure-rendered t))))

(defun e-chat--finalize-turn-display (turn-id)
  "Mark TURN-ID as having rendered its final response."
  (when-let ((record (e-chat--turn-record turn-id)))
    (plist-put record :final-rendered t)
    (if (e-chat--settled-activity-p record)
        (e-chat--render-turn-transient turn-id record)
      (e-chat--delete-turn-transient record))
    (e-chat--clear-running-status-markers)))

(defun e-chat--delete-block-details (block)
  "Delete expanded detail text for BLOCK."
  (let ((start (plist-get block :details-start-marker))
        (end (plist-get block :details-end-marker)))
    (when (and (markerp start)
               (markerp end)
               (marker-position start)
               (marker-position end))
      (let ((inhibit-read-only t))
        (delete-region start end)))
    (plist-put block :details-start-marker nil)
    (plist-put block :details-end-marker nil)))

(defun e-chat--block-order-insert-after (parent-id child-ids)
  "Place CHILD-IDS immediately after PARENT-ID in `e-chat--block-order'."
  (let ((remaining e-chat--block-order)
        before
        after
        found)
    (dolist (block-id remaining)
      (unless (member block-id child-ids)
        (if found
            (push block-id after)
          (push block-id before))
        (when (equal block-id parent-id)
          (setq found t))))
    (setq e-chat--block-order
          (append (nreverse before) child-ids (nreverse after)))))

(defun e-chat--activity-summary-expanded-p (block)
  "Return non-nil when activity summary BLOCK has rendered children."
  (not (null (plist-get block :children))))

(defun e-chat--delete-activity-summary-children (block)
  "Delete child blocks rendered for activity summary BLOCK."
  (let ((children (plist-get block :children)))
    (when children
      (let* ((first-block (gethash (car children) e-chat--block-registry))
             (last-block (gethash (car (last children)) e-chat--block-registry))
             (start-marker (and first-block
                                (plist-get first-block :start-marker)))
             (end-marker (and last-block
                              (plist-get last-block :end-marker)))
             (start (and (markerp start-marker)
                         (marker-position start-marker)))
             (end (and (markerp end-marker)
                       (marker-position end-marker))))
        (when (and start end (< start end))
          (let ((inhibit-read-only t))
            (delete-region start end))))
      (dolist (child-id children)
        (e-chat--remove-block-record child-id))
      (plist-put block :children nil)
      (plist-put block :expanded nil))))

(defun e-chat--insert-activity-summary-child (parent turn-id child)
  "Insert CHILD for activity summary PARENT and return its block id."
  (let* ((block-id (e-chat--next-block-id))
         (text (plist-get child :text))
         (line (format "  %s\n" text))
         (start (point))
         (content-start (+ start 2))
         (content-end (+ content-start (length text))))
    (e-chat--insert-protected
     line
     'e-chat-system-face
     `(e-chat-turn-id ,turn-id
       e-chat-block-id ,block-id
       e-chat-parent-block-id ,(plist-get parent :id)))
    (e-chat--update-block-bounds
     block-id
     turn-id
     start
     (point)
     (plist-get child :kind)
     (plist-get child :action-text)
     content-start
     content-end
     (plist-get child :tool-items)
     nil)
    (let ((block (e-chat--block-record block-id turn-id)))
      (plist-put block :parent-block-id (plist-get parent :id)))
    block-id))

(defun e-chat--insert-activity-summary-children (block)
  "Insert navigable child blocks for activity summary BLOCK."
  (e-chat--delete-activity-summary-children block)
  (let* ((turn-id (plist-get block :turn-id))
         (turn-record (and turn-id
                           (gethash turn-id e-chat--turn-registry)))
         (children (and turn-record
                        (e-chat--activity-summary-child-records turn-record)))
         (end-marker (plist-get block :end-marker))
         (end (and (markerp end-marker)
                   (marker-position end-marker)))
         (composer-state (e-chat--capture-composer-state))
         (had-composer (e-chat--delete-composer))
         child-ids)
    (unless end
      (user-error "Focused activity summary has no insertion point"))
    (when children
      (let ((inhibit-read-only t))
        (goto-char end)
        (unless (bolp)
          (insert "\n"))
        (dolist (child children)
          (push (e-chat--insert-activity-summary-child block turn-id child)
                child-ids)))
      (setq child-ids (nreverse child-ids))
      (plist-put block :children child-ids)
      (plist-put block :expanded t)
      (e-chat--block-order-insert-after (plist-get block :id) child-ids))
    (when had-composer
      (goto-char (point-max))
      (e-chat--restore-composer-state composer-state))))

(defun e-chat--toggle-activity-summary-children (block)
  "Toggle navigable activity summary children for BLOCK."
  (if (e-chat--activity-summary-expanded-p block)
      (e-chat--delete-activity-summary-children block)
    (e-chat--insert-activity-summary-children block)))

(defun e-chat--block-details-visible-p (block)
  "Return non-nil when BLOCK has visible expanded detail text."
  (not (null (e-chat--block-details-bounds block))))

(defun e-chat--turn-details-text (turn-id record)
  "Return expanded details text for TURN-ID using RECORD."
  (concat
   (or (e-chat--intermittent-details-text record) "")
   (or (e-chat--failure-details-text record) "")
   (format "  Turn: %s\n  Started: %s\n  Ended: %s\n  Duration: %s\n\n"
           turn-id
           (e-chat--format-time-value (plist-get record :started-at))
           (e-chat--format-time-value (plist-get record :ended-at))
           (e-chat--format-duration (plist-get record :started-at)
                                    (plist-get record :ended-at)))))

(defun e-chat--insert-block-details (block turn-id record)
  "Insert expanded details for BLOCK and TURN-ID using RECORD."
  (e-chat--insert-block-details-text
   block
   (e-chat--turn-details-text turn-id record)))

(defun e-chat--insert-block-details-text (block text)
  "Insert expanded detail TEXT for BLOCK."
  (e-chat--delete-block-details block)
  (let* ((end-marker (plist-get block :end-marker))
         (end (and (markerp end-marker) (marker-position end-marker)))
         (had-composer (e-chat--delete-composer)))
    (unless end
      (user-error "Focused e chat block has no insertion point"))
    (let ((inhibit-read-only t))
      (goto-char end)
      (let ((start (point)))
        (e-chat--insert-protected
         text
         'e-chat-system-face
         '(e-chat-turn-details t))
        (plist-put block :details-start-marker (copy-marker start nil))
        (plist-put block :details-end-marker (copy-marker (point) nil))))
    (when had-composer
      (goto-char (point-max))
      (e-chat--insert-composer))))

(defun e-chat--toggle-block-details-text (block text)
  "Toggle inline detail TEXT for BLOCK."
  (if (e-chat--block-details-visible-p block)
      (e-chat--delete-block-details block)
    (e-chat--insert-block-details-text block text)
    (e-chat--enter-block-view block)))

(defun e-chat--entry-face (title)
  "Return face for chat entry TITLE."
  (pcase title
    ("You" 'e-chat-user-face)
    ("Assistant" 'e-chat-final-assistant-face)
    (_ 'e-chat-system-face)))

(defun e-chat--entry-glyph (title)
  "Return glyph for chat entry TITLE."
  (pcase title
    ("You" e-chat--user-glyph)
    ("Assistant" e-chat--assistant-glyph)
    (_ e-chat--system-glyph)))

(defun e-chat--entry-heading (title)
  "Return compact heading text for chat entry TITLE."
  (pcase title
    ((or "You" "Assistant") (e-chat--entry-glyph title))
    (_ (format "%s %s" (e-chat--entry-glyph title) title))))

(defun e-chat--entry-text (title content)
  "Return display text for chat entry TITLE and CONTENT."
  (if (member title '("You" "Assistant"))
      (format "%s %s\n\n" (e-chat--entry-heading title) content)
    (format "%s\n%s\n\n" (e-chat--entry-heading title) content)))

(defun e-chat--entry-content-offset (title)
  "Return the character offset of TITLE entry content start."
  (if (member title '("You" "Assistant"))
      (1+ (length (e-chat--entry-heading title)))
    (1+ (length (e-chat--entry-heading title)))))

(defun e-chat--add-markdown-face (start end face)
  "Add Markdown FACE between START and END."
  (when (< start end)
    (add-face-text-property start end face t)))

(defconst e-chat--markdown-mode-copied-properties
  '(face font-lock-face font-lock-multiline keymap mouse-face help-echo)
  "Text properties copied from `markdown-mode' fontification.")

(defun e-chat--clear-markdown-presentation (start end)
  "Clear Markdown presentation properties between START and END."
  (when (< start end)
    (remove-list-of-text-properties
     start end
     '(face font-lock-face font-lock-multiline keymap mouse-face help-echo
       invisible display e-chat-markdown-syntax))))

(defun e-chat--apply-markdown-mode-properties (content-start content-end)
  "Apply `markdown-mode' fontification between CONTENT-START and CONTENT-END.
Return non-nil when `markdown-mode' was available and used."
  (when (and (< content-start content-end)
             (require 'markdown-mode nil t))
    (let ((content (buffer-substring-no-properties content-start content-end))
          (target-buffer (current-buffer)))
      (e-chat--clear-markdown-presentation content-start content-end)
      (with-temp-buffer
        (insert content)
        (markdown-mode)
        (font-lock-ensure (point-min) (point-max))
        (let ((source-end (point-max))
              (source-pos (point-min)))
          (while (< source-pos source-end)
            (let ((next-pos (or (next-property-change source-pos nil source-end)
                                source-end)))
              (dolist (property e-chat--markdown-mode-copied-properties)
                (let ((value (get-text-property source-pos property)))
                  (when value
                    (with-current-buffer target-buffer
                      (add-text-properties
                       (+ content-start (1- source-pos))
                       (+ content-start (1- next-pos))
                       (if (eq property 'face)
                           (list 'face value 'font-lock-face value)
                         (list property value)))))))
              (setq source-pos next-pos)))))
      t)))

(defun e-chat--conceal-markdown-syntax (start end)
  "Hide Markdown syntax between START and END."
  (when (< start end)
    (add-text-properties
     start end
     '(invisible e-chat-markdown-syntax
       e-chat-markdown-syntax t))))

(defun e-chat--display-markdown-syntax (start end display)
  "Display Markdown syntax between START and END as DISPLAY."
  (when (< start end)
    (add-text-properties start end `(display ,display e-chat-markdown-syntax t))))

(defun e-chat--line-content-start (line-start content-start)
  "Return CONTENT-START or LINE-START, whichever is later."
  (max line-start content-start))

(defun e-chat--apply-markdown-line-faces (content-start content-end)
  "Apply block-level Markdown faces between CONTENT-START and CONTENT-END."
  (save-excursion
    (goto-char content-start)
    (let ((in-code-block nil))
      (while (< (point) content-end)
        (let* ((line-start (line-beginning-position))
               (line-end (min (line-end-position) content-end))
               (line-content-start (e-chat--line-content-start
                                    line-start content-start))
               (line-text (buffer-substring-no-properties
                           line-content-start line-end)))
          (cond
           ((string-match-p "\\`[ \t]*```" line-text)
            (e-chat--conceal-markdown-syntax
             line-content-start
             (min (1+ line-end) content-end))
            (setq in-code-block (not in-code-block)))
           (in-code-block
            (e-chat--add-markdown-face line-content-start line-end
                                       'e-chat-markdown-code-block-face))
           ((string-match "\\`[ \t]*\\(#[#]*[ \t]+\\)" line-text)
            (let ((heading-start (+ line-content-start (match-beginning 1)))
                  (heading-text-start (+ line-content-start (match-end 1))))
              (e-chat--conceal-markdown-syntax
               heading-start heading-text-start)
              (e-chat--add-markdown-face heading-text-start line-end
                                         'e-chat-markdown-heading-face)))
           ((string-match
             "\\`[ \t]*\\([-+*]\\|[0-9]+\\.\\)\\([ \t]+\\)"
             line-text)
            (let ((marker-start (+ line-content-start (match-beginning 1)))
                  (marker-end (+ line-content-start (match-end 1)))
                  (content-start (+ line-content-start (match-end 0)))
                  (marker (match-string 1 line-text)))
              (if (string-match-p "\\`[-+*]\\'" marker)
                  (e-chat--display-markdown-syntax marker-start marker-end "•")
                (e-chat--add-markdown-face marker-start marker-end
                                           'e-chat-markdown-list-face))
              (e-chat--add-markdown-face content-start line-end
                                         'e-chat-markdown-list-face))))
          (forward-line 1))))))

(defun e-chat--apply-markdown-inline-face
    (regexp content-start content-end face &optional group)
  "Apply FACE to REGEXP GROUP between CONTENT-START and CONTENT-END."
  (save-excursion
    (goto-char content-start)
    (while (re-search-forward regexp content-end t)
      (let ((group (or group 1)))
        (e-chat--add-markdown-face
         (match-beginning group) (match-end group) face)))))

(defun e-chat--apply-markdown-delimited-face
    (regexp content-start content-end face)
  "Apply FACE to REGEXP group 2 between CONTENT-START and CONTENT-END.
Hide REGEXP groups 1 and 3 as Markdown syntax."
  (save-excursion
    (goto-char content-start)
    (while (re-search-forward regexp content-end t)
      (e-chat--conceal-markdown-syntax (match-beginning 1) (match-end 1))
      (e-chat--add-markdown-face (match-beginning 2) (match-end 2) face)
      (e-chat--conceal-markdown-syntax (match-beginning 3) (match-end 3)))))

(defun e-chat--apply-markdown-emphasis-face (content-start content-end)
  "Apply emphasis presentation between CONTENT-START and CONTENT-END."
  (save-excursion
    (goto-char content-start)
    (while (re-search-forward
            "\\(^\\|[[:space:]]\\)\\(\\*\\)\\([^*\n]+\\)\\(\\*\\)"
            content-end t)
      (e-chat--conceal-markdown-syntax (match-beginning 2) (match-end 2))
      (e-chat--add-markdown-face
       (match-beginning 3) (match-end 3) 'e-chat-markdown-emphasis-face)
      (e-chat--conceal-markdown-syntax (match-beginning 4) (match-end 4)))))

(defun e-chat--apply-markdown-link-faces (content-start content-end)
  "Apply Markdown link faces and metadata between CONTENT-START and CONTENT-END."
  (save-excursion
    (goto-char content-start)
    (while (re-search-forward "\\[\\([^]\n]+\\)\\](\\([^) \n]+\\))"
                              content-end t)
      (let ((label-start (match-beginning 1))
            (label-end (match-end 1))
            (url (match-string-no-properties 2)))
        (e-chat--add-markdown-face label-start label-end
                                   'e-chat-markdown-link-face)
        (add-text-properties label-start label-end
                             `(help-echo ,url e-chat-link-url ,url))
        (e-chat--conceal-markdown-syntax (match-beginning 0) label-start)
        (e-chat--conceal-markdown-syntax label-end (match-end 0))))))

(defun e-chat--apply-assistant-markdown (content-start content-end)
  "Apply Markdown presentation between CONTENT-START and CONTENT-END."
  (when (< content-start content-end)
    (unless (e-chat--apply-markdown-mode-properties content-start content-end)
      (e-chat--clear-markdown-presentation content-start content-end)
      (e-chat--apply-markdown-line-faces content-start content-end)
      (e-chat--apply-markdown-delimited-face
       "\\(`\\)\\([^`\n]+\\)\\(`\\)" content-start content-end
       'e-chat-markdown-code-face)
      (e-chat--apply-markdown-delimited-face
       "\\(\\*\\*\\)\\([^*\n]+\\)\\(\\*\\*\\)" content-start content-end
       'e-chat-markdown-strong-face)
      (e-chat--apply-markdown-emphasis-face content-start content-end)
      (e-chat--apply-markdown-link-faces content-start content-end))))

(defun e-chat--apply-final-assistant-face (content-start content-end)
  "Apply settled assistant styling from CONTENT-START to CONTENT-END.
Preserve Markdown faces already present in the range."
  (when (< content-start content-end)
    (add-face-text-property content-start
                            content-end
                            'e-chat-final-assistant-face
                            t)))

(defun e-chat--insert-entry
    (title content &optional ensure-composer turn-id details-text)
  "Insert a protected chat entry with TITLE and CONTENT.
When ENSURE-COMPOSER is non-nil, recreate the composer after inserting.
TURN-ID tags the rendered entry for response navigation.  DETAILS-TEXT, when
non-nil, is used by focused block activation."
  (e-chat--profile-call
   'chat.insert-entry
   (list :session-id e-chat-session-id
         :turn-id turn-id
         :buffer-name (buffer-name)
         :metadata (list :title title
                         :ensure-composer (and ensure-composer t)))
   (lambda ()
     (let* ((active-turn-id e-chat--progress-turn-id)
            (active-record (and active-turn-id
                                (e-chat--existing-turn-record active-turn-id)))
            (had-composer nil)
            (side (e-chat--entry-side title))
            (block-id (and turn-id (e-chat--next-block-id))))
       (when active-turn-id
         (e-chat--delete-running-status active-record))
       (setq had-composer (e-chat--delete-composer))
       (let ((inhibit-read-only t))
         (goto-char (point-max))
         (unless (or (bobp) (bolp))
           (insert "\n"))
         (e-chat--insert-durable-entry-separators turn-id side)
         (let ((start (point)))
           (let ((content-start (+ start (e-chat--entry-content-offset title))))
             (e-chat--insert-protected
              (e-chat--entry-text title content)
              (e-chat--entry-face title)
              (when block-id
                `(e-chat-turn-id ,turn-id
                  e-chat-block-id ,block-id)))
             (when (equal title "Assistant")
               (e-chat--apply-assistant-markdown
                content-start
                (point))
               (e-chat--apply-final-assistant-face
                content-start
                (point)))
             (e-chat--update-block-bounds
              block-id
              turn-id
              start
              (point)
              (e-chat--block-kind-for-title title)
              content
              content-start
              (+ content-start (length content))
              nil
              details-text)
             (e-chat--record-durable-entry-rendered turn-id side))))
       (if active-turn-id
           (e-chat--render-running-status active-turn-id active-record)
         (when (or ensure-composer had-composer)
           (e-chat--insert-composer)))))))

(defun e-chat-enter-response-navigation ()
  "Enter response navigation mode and focus the nearest rendered turn."
  (interactive)
  (unless (derived-mode-p 'e-chat-mode)
    (user-error "Response navigation is only available in e chat buffers"))
  (let ((block-id (or (e-chat--block-at-point)
                      (e-chat--last-rendered-block-id))))
    (unless block-id
      (user-error "No rendered e chat blocks"))
    (e-chat-response-navigation-mode 1)
    (e-chat--focus-block block-id)))

(defun e-chat-response-navigation-next ()
  "Focus the next rendered turn block."
  (interactive)
  (e-chat--move-focused-block 1))

(defun e-chat-response-navigation-previous ()
  "Focus the previous rendered turn block."
  (interactive)
  (e-chat--move-focused-block -1))

(defun e-chat-response-navigation-activate ()
  "Activate the focused block according to its kind."
  (interactive)
  (let ((block (e-chat--focused-block)))
    (pcase (plist-get block :kind)
      ('activity
       (e-chat--open-tool-list block))
      ('activity-summary
       (e-chat--toggle-activity-summary-children block))
      ('activity-tool-batch
       (e-chat--open-tool-list block))
      ('system
       (if-let ((details-text (plist-get block :details-text)))
           (e-chat--toggle-block-details-text block details-text)
         (e-chat--enter-block-view block)))
      (_
       (e-chat--enter-block-view block)))))

(defun e-chat-response-navigation-insert ()
  "Leave response navigation and focus the composer."
  (interactive)
  (e-chat--enter-composer-input-state))

(defun e-chat-response-navigation-copy ()
  "Copy the focused block's action text."
  (interactive)
  (let ((text (e-chat--block-action-text (e-chat--focused-block))))
    (kill-new text)
    (message "Copied e chat block")
    text))

(defun e-chat--open-block-text (block)
  "Open BLOCK action text in a new editable buffer."
  (e-chat--buffer-with-text "*e-chat-block*" (e-chat--block-action-text block)))

(defun e-chat-response-navigation-open ()
  "Open the focused block in a new editable buffer."
  (interactive)
  (e-chat--open-block-text (e-chat--focused-block)))

(defun e-chat--display-details-buffer (text)
  "Display read-only details TEXT."
  (let ((buffer (get-buffer-create e-chat-details-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (special-mode)
      (goto-char (point-min)))
    (display-buffer buffer)
    buffer))

(defun e-chat-response-navigation-details ()
  "Open details for the focused block's turn."
  (interactive)
  (let* ((block (e-chat--focused-block))
         (turn-id (plist-get block :turn-id))
         (record (and turn-id (gethash turn-id e-chat--turn-registry))))
    (unless record
      (user-error "Focused e chat block has no turn details"))
    (e-chat--display-details-buffer
     (e-chat--turn-details-text turn-id record))))

(defun e-chat-copy-latest-response ()
  "Copy the latest final assistant response."
  (interactive)
  (let ((text (e-chat--block-action-text (e-chat--latest-final-block))))
    (kill-new text)
    (message "Copied latest e chat response")
    text))

(defun e-chat-open-latest-response ()
  "Open the latest final assistant response in an editable buffer."
  (interactive)
  (e-chat--open-block-text (e-chat--latest-final-block)))

(defun e-chat--enter-block-view (block)
  "Enter block-local view mode for BLOCK."
  (let* ((block-id (plist-get block :id))
         (bounds (e-chat--block-view-bounds block)))
    (e-chat-response-navigation-mode -1)
    (setq e-chat--focused-block-id block-id)
    (setq e-chat--focused-turn-id (plist-get block :turn-id))
    (setq e-chat--block-view-block-id block-id)
    (e-chat-block-view-mode 1)
    (goto-char (car bounds))))

(defun e-chat--block-view-block ()
  "Return block active in block view mode."
  (or (and e-chat--block-view-block-id
           (hash-table-p e-chat--block-registry)
           (gethash e-chat--block-view-block-id e-chat--block-registry))
      (user-error "No e chat block view is active")))

(defun e-chat--block-view-clamp-point ()
  "Keep point inside the active block content bounds."
  (let ((bounds (e-chat--block-view-bounds (e-chat--block-view-block))))
    (when (< (point) (car bounds))
      (goto-char (car bounds)))
    (when (> (point) (cdr bounds))
      (goto-char (cdr bounds)))))

(defun e-chat--block-view-keep-region-active ()
  "Keep an active block-view region active after modal motion."
  (when (region-active-p)
    (setq deactivate-mark nil)))

(defun e-chat-block-view-left ()
  "Move left inside the focused block."
  (interactive)
  (let ((bounds (e-chat--block-view-bounds (e-chat--block-view-block))))
    (when (> (point) (car bounds))
      (backward-char 1)))
  (e-chat--block-view-keep-region-active))

(defun e-chat-block-view-right ()
  "Move right inside the focused block."
  (interactive)
  (let ((bounds (e-chat--block-view-bounds (e-chat--block-view-block))))
    (when (< (point) (cdr bounds))
      (forward-char 1)))
  (e-chat--block-view-keep-region-active))

(defun e-chat-block-view-down ()
  "Move down inside the focused block."
  (interactive)
  (forward-line 1)
  (e-chat--block-view-clamp-point)
  (e-chat--block-view-keep-region-active))

(defun e-chat-block-view-up ()
  "Move up inside the focused block."
  (interactive)
  (forward-line -1)
  (e-chat--block-view-clamp-point)
  (e-chat--block-view-keep-region-active))

(defun e-chat-block-view-beginning ()
  "Move to the beginning of the focused block content."
  (interactive)
  (goto-char (car (e-chat--block-view-bounds (e-chat--block-view-block))))
  (e-chat--block-view-keep-region-active))

(defun e-chat-block-view-end ()
  "Move to the end of the focused block content."
  (interactive)
  (goto-char (cdr (e-chat--block-view-bounds (e-chat--block-view-block))))
  (e-chat--block-view-keep-region-active))

(defun e-chat-block-view-select ()
  "Start or cancel a block-view text selection at point."
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (set-mark (point))
    (activate-mark)))

(defun e-chat-block-view-copy ()
  "Copy the active block-view selection or the whole focused block."
  (interactive)
  (let ((text (if (region-active-p)
                  (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))
                (e-chat--block-action-text (e-chat--block-view-block)))))
    (kill-new text)
    (when (region-active-p)
      (deactivate-mark))
    (message "Copied e chat block view text")
    text))

(defun e-chat-block-view-back ()
  "Return from block view to block navigation."
  (interactive)
  (if (region-active-p)
      (deactivate-mark t)
    (let ((block-id e-chat--block-view-block-id))
      (e-chat-block-view-mode -1)
      (e-chat-response-navigation-mode 1)
      (e-chat--focus-block block-id))))

(defun e-chat-block-view-insert ()
  "Leave block view and focus the composer."
  (interactive)
  (e-chat--enter-composer-input-state))

(defun e-chat--delete-tool-list (block)
  "Delete the visible tool list for BLOCK."
  (let ((start (plist-get block :tool-list-start-marker))
        (end (plist-get block :tool-list-end-marker)))
    (when (and (markerp start)
               (markerp end)
               (marker-position start)
               (marker-position end))
      (let ((inhibit-read-only t))
        (delete-region start end)))
    (plist-put block :tool-list-start-marker nil)
    (plist-put block :tool-list-end-marker nil)))

(defun e-chat--open-tool-list (block)
  "Open a collapsed tool-call list for activity BLOCK."
  (let ((items (plist-get block :tool-items)))
    (unless items
      (user-error "Focused activity block has no tool calls"))
    (e-chat--delete-tool-list block)
    (let* ((end-marker (plist-get block :end-marker))
           (end (and (markerp end-marker) (marker-position end-marker)))
           (composer-state (e-chat--capture-composer-state))
           (had-bottom-chrome (e-chat--delete-composer)))
      (unless end
        (user-error "Focused activity block has no insertion point"))
      (let ((inhibit-read-only t))
        (goto-char end)
        (let ((start (point)))
          (e-chat--insert-protected "\n" 'e-chat-system-face
                                    '(e-chat-tool-list t))
          (cl-loop for item in items
                   for index from 0
                   do
                   (let ((item-start (point)))
                     (e-chat--insert-protected
                      (format "  %d. %s\n" (1+ index)
                              (plist-get item :call))
                      'e-chat-system-face
                      `(e-chat-tool-list t e-chat-tool-index ,index))
                     (plist-put item :start-marker (copy-marker item-start nil))
                     (plist-put item :end-marker (copy-marker (point) nil))))
          (plist-put block :tool-list-start-marker (copy-marker start nil))
          (plist-put block :tool-list-end-marker (copy-marker (point) nil))))
      (when had-bottom-chrome
        (goto-char (point-max))
        (e-chat--restore-composer-state composer-state)))
    (let ((block-id (plist-get block :id)))
      (e-chat-response-navigation-mode -1)
      (setq e-chat--focused-block-id block-id)
      (setq e-chat--focused-turn-id (plist-get block :turn-id))
      (setq e-chat--tool-list-block-id block-id)
      (setq e-chat--tool-list-index 0)
      (e-chat-tool-list-mode 1)
      (e-chat--focus-tool-list-item))))

(defun e-chat--tool-list-block ()
  "Return active tool-list block."
  (or (and e-chat--tool-list-block-id
           (hash-table-p e-chat--block-registry)
           (gethash e-chat--tool-list-block-id e-chat--block-registry))
      (user-error "No e chat tool list is active")))

(defun e-chat--focus-tool-list-item ()
  "Highlight the selected tool-list item."
  (let* ((block (e-chat--tool-list-block))
         (items (plist-get block :tool-items))
         (item (nth e-chat--tool-list-index items))
         (start (and item
                     (markerp (plist-get item :start-marker))
                     (marker-position (plist-get item :start-marker))))
         (end (and item
                   (markerp (plist-get item :end-marker))
                   (marker-position (plist-get item :end-marker)))))
    (unless (and start end)
      (user-error "No e chat tool item to focus"))
    (unless (overlayp e-chat--tool-list-overlay)
      (setq e-chat--tool-list-overlay (make-overlay start end nil t nil)))
    (move-overlay e-chat--tool-list-overlay start end)
    (overlay-put e-chat--tool-list-overlay 'face 'e-chat-focused-turn-face)
    (goto-char start)))

(defun e-chat-tool-list-next ()
  "Focus the next tool call in the active tool list."
  (interactive)
  (let* ((items (plist-get (e-chat--tool-list-block) :tool-items))
         (max-index (1- (length items))))
    (setq e-chat--tool-list-index (min max-index
                                       (1+ e-chat--tool-list-index)))
    (e-chat--focus-tool-list-item)))

(defun e-chat-tool-list-previous ()
  "Focus the previous tool call in the active tool list."
  (interactive)
  (setq e-chat--tool-list-index (max 0 (1- e-chat--tool-list-index)))
  (e-chat--focus-tool-list-item))

(defun e-chat-tool-list-open-output ()
  "Open the selected tool output in a read-only buffer."
  (interactive)
  (let* ((block (e-chat--tool-list-block))
         (item (nth e-chat--tool-list-index (plist-get block :tool-items)))
         (output (or (plist-get item :output) ""))
         (origin (current-buffer)))
    (let ((buffer (get-buffer-create e-chat-tool-output-buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert output))
        (e-chat-tool-output-mode)
        (setq e-chat--tool-output-origin-buffer origin)
        (goto-char (point-min)))
      (display-buffer buffer)
      buffer)))

(defun e-chat-tool-list-back ()
  "Collapse the tool list and return to block navigation."
  (interactive)
  (let* ((block (e-chat--tool-list-block))
         (block-id (plist-get block :id)))
    (e-chat--delete-tool-list block)
    (e-chat-tool-list-mode -1)
    (e-chat-response-navigation-mode 1)
    (e-chat--focus-block block-id)))

(defun e-chat-tool-output-back ()
  "Close tool output and return to its originating tool list."
  (interactive)
  (let ((origin e-chat--tool-output-origin-buffer)
        (buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (when (buffer-live-p origin)
      (pop-to-buffer origin))))

(defun e-chat--refresh-composer-position ()
  "Refresh composer spacer for the current visible window."
  (when (e-chat--composer-active-p)
    (let ((text (buffer-substring e-chat--composer-start-marker
                                  (point-max))))
      (e-chat--delete-composer)
      (e-chat--insert-composer text))))

(defun e-chat--refresh-visible-composers ()
  "Refresh composer spacers for visible e chat buffers."
  (unless e-chat--refresh-visible-composers-in-progress
    (let ((e-chat--refresh-visible-composers-in-progress t)
          (seen nil))
      (dolist (window (window-list nil 'no-minibuf))
        (let ((buffer (window-buffer window)))
          (when (and (buffer-live-p buffer)
                     (not (memq buffer seen)))
            (push buffer seen)
            (with-current-buffer buffer
              (when (derived-mode-p 'e-chat-mode)
                (e-chat--refresh-composer-position)))))))))

(defun e-chat--ensure-window-refresh-hook ()
  "Ensure visible chat composers refresh when frame windows change."
  (add-hook 'window-configuration-change-hook
            #'e-chat--refresh-visible-composers))

(defun e-chat--show-composer ()
  "Move point and visible window focus to the composer."
  (goto-char (point-max))
  (when-let ((window (e-chat--visible-window)))
    (set-window-point window (point))
    (with-selected-window window
      (ignore-errors
        (recenter -2)))))

(defun e-chat--enter-composer-input-state ()
  "Leave chat-local navigation states and focus editable composer input."
  (when (region-active-p)
    (deactivate-mark t))
  (when e-chat-tool-list-mode
    (e-chat-tool-list-mode -1))
  (when e-chat-block-view-mode
    (e-chat-block-view-mode -1))
  (when e-chat-response-navigation-mode
    (e-chat-response-navigation-mode -1))
  (e-chat--ensure-composer)
  (e-chat--show-composer))

(defun e-chat--after-display-buffer (buffer)
  "Restore chat-local editing invariants after displaying BUFFER."
  (with-current-buffer buffer
    (e-chat--disable-modal-editing)
    (e-chat--disable-completion)
    (e-chat--enter-composer-input-state))
  buffer)

(defun e-chat--switch-to-buffer (buffer)
  "Display BUFFER in the selected window and restore chat-local editing invariants."
  (switch-to-buffer buffer)
  (e-chat--after-display-buffer buffer))

(defun e-chat--pop-to-buffer (buffer)
  "Pop to BUFFER and restore chat-local editing invariants."
  (pop-to-buffer buffer)
  (e-chat--after-display-buffer buffer))

(defun e-chat--session-title ()
  "Return the current attached session title, or nil."
  (and e-chat-harness
       e-chat-session-id
       (ignore-errors
         (e-harness-session-title
          e-chat-harness
          e-chat-session-id))))

(defun e-chat--title-block-text ()
  "Return the current chat title block text."
  (let ((title (e-chat--session-title)))
    (if title
        (format "%s\n%s\n\n" e-chat--title title)
      (concat e-chat--title "\n\n"))))

(defun e-chat--clear (&optional omit-composer)
  "Clear and initialize the current chat buffer.
When OMIT-COMPOSER is non-nil, leave the buffer as transcript-only."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq e-chat--transcript-end-marker nil)
    (setq e-chat--composer-start-marker nil)
    (setq e-chat--composer-spacer-marker nil)
    (setq e-chat--turn-registry (make-hash-table :test 'equal))
    (setq e-chat--block-registry (make-hash-table :test 'equal))
    (setq e-chat--block-order nil)
    (setq e-chat--block-counter 0)
    (setq e-chat--context-reference-counter 0)
    (setq e-chat--focused-turn-id nil)
    (setq e-chat--focused-block-id nil)
    (setq e-chat--latest-final-block-id nil)
    (setq e-chat--last-rendered-turn-id nil)
    (setq e-chat--last-rendered-side nil)
    (setq e-chat--block-view-block-id nil)
    (setq e-chat--tool-list-block-id nil)
    (setq e-chat--tool-list-index 0)
    (when (overlayp e-chat--focused-turn-overlay)
      (delete-overlay e-chat--focused-turn-overlay))
    (setq e-chat--focused-turn-overlay nil)
    (when (overlayp e-chat--tool-list-overlay)
      (delete-overlay e-chat--tool-list-overlay))
    (setq e-chat--tool-list-overlay nil)
    (e-chat--cancel-progress-timer)
    (e-chat--cancel-pending-activity-redraw)
    (setq e-chat--progress-turn-id nil)
    (setq e-chat--progress-frame 0)
    (setq e-chat--progress-start-marker nil)
    (setq e-chat--progress-end-marker nil)
    (setq e-chat--running-status-start-marker nil)
    (setq e-chat--running-status-end-marker nil)
    (setq e-chat--rendered-session-title (e-chat--session-title))
    (e-chat--insert-protected (e-chat--title-block-text)
                              'e-chat-title-face)
    (unless omit-composer
      (e-chat--insert-composer))))

(defun e-chat--format-token-count (tokens)
  "Return compact display text for TOKENS."
  (e-context-status-format-token-count tokens))

(defun e-chat--format-mode-line-status
    (model effort used-tokens max-tokens &optional approximate)
  "Return compact mode-line text for MODEL, EFFORT, and context token usage."
  (e-context-status-format
   "e-chat" model effort used-tokens max-tokens approximate))

(defun e-chat--model-context-token-limit (model)
  "Return configured max context tokens for MODEL, or nil."
  (e-context-status-model-token-limit
   model e-chat-model-context-token-limits))

(defun e-chat--context-token-estimate (context)
  "Return approximate token count for model-facing CONTEXT."
  (e-context-status-context-token-estimate
   context e-chat-context-token-estimate-bytes-per-token))

(defun e-chat--mode-line-status-text (&optional prefer-token-usage)
  "Return the current mode-line text for this e chat buffer.
When PREFER-TOKEN-USAGE is non-nil and fresh provider usage exists, skip the
expensive context-token estimate path."
  (unless (consp e-chat--mode-line-context-estimate-cache)
    (setq-local e-chat--mode-line-context-estimate-cache (cons nil nil)))
  (let ((e-context-status-estimate-cache-seconds
         e-chat-mode-line-context-estimate-cache-seconds))
    (e-context-status-text
     e-chat-harness e-chat-session-id
     :prefix "e-chat"
     :prefer-token-usage prefer-token-usage
     :estimate-context (not prefer-token-usage)
     :estimate-cache e-chat--mode-line-context-estimate-cache
     :token-limits e-chat-model-context-token-limits
     :bytes-per-token e-chat-context-token-estimate-bytes-per-token)))

(defun e-chat--refresh-mode-line-status (&optional prefer-token-usage)
  "Refresh this buffer's e chat mode-line text.
When PREFER-TOKEN-USAGE is non-nil, prefer fresh provider usage over recomputing
an approximate full-context estimate."
  (setq-local e-chat--mode-line-status
              (e-chat--mode-line-status-text prefer-token-usage))
  (setq-local mode-name e-chat--mode-line-status)
  (force-mode-line-update))

(defun e-chat--invalidate-mode-line-context-estimate ()
  "Clear the buffer-local context estimate used by the mode-line status."
  (setq-local e-chat--mode-line-context-estimate-cache (cons nil nil)))

(defun e-chat--set-status (status &optional refresh-mode-line)
  "Set chat buffer STATUS.
When REFRESH-MODE-LINE is non-nil, also refresh context-aware mode-line text."
  (unless (and (not refresh-mode-line)
               (equal status e-chat--status)
               header-line-format)
    (e-chat--profile-call
     'chat.status
     (list :session-id e-chat-session-id
           :buffer-name (buffer-name)
           :metadata (list :status status
                           :refresh-mode-line (and refresh-mode-line t)))
     (lambda ()
       (setq e-chat--status status)
       (setq header-line-format
             (if (and e-chat-harness e-chat-session-id)
                 (let* ((title (ignore-errors
                                 (e-harness-session-title
                                  e-chat-harness
                                  e-chat-session-id)))
                        (options (ignore-errors
                                   (e-harness-turn-options
                                    e-chat-harness
                                    e-chat-session-id)))
                        (model (plist-get options :model))
                        (effort (plist-get options :reasoning-effort)))
                   (format "E Chat: %s - %s - %s/%s"
                           status
                           title
                           (or model "model unset")
                           (or effort "effort unset")))
               (format "E Chat: %s" status)))
       (when refresh-mode-line
         (e-chat--refresh-mode-line-status))))))

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
      (e-chat--insert-protected (e-chat--title-block-text)
                                'e-chat-title-face))))

(defun e-chat--refresh-session-display ()
  "Refresh presentation surfaces derived from attached session metadata."
  (e-chat--profile-call
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
           (when e-chat--status
             (e-chat--set-status e-chat--status))))))))

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

(defun e-chat--message-entry (message)
  "Return a rendered entry for MESSAGE."
  (let ((role (plist-get message :role))
        (content (plist-get message :content)))
    (pcase role
      ('user (cons "You" content))
      ('assistant (cons "Assistant" content))
      ('tool (cons "Tool" (format "%S" content)))
      (_ (cons (format "%s" role) (format "%S" content))))))

(defun e-chat--render-event (event)
  "Render harness EVENT into the current chat buffer."
  (e-chat--profile-call
   'chat.render-event
   (list :session-id e-chat-session-id
         :turn-id (plist-get event :turn-id)
         :buffer-name (buffer-name)
         :metadata (list :event-type
                         (symbol-name (plist-get event :type))))
   (lambda ()
     (pcase (plist-get event :type)
    ('turn-started
     (e-chat--set-turn-time (plist-get event :turn-id)
                             :started-at
                             (plist-get event :created-at))
     (e-chat--start-progress-indicator (plist-get event :turn-id))
     (e-chat--set-status (format "running %s" (plist-get event :turn-id))))
    ('turn-finished
     (e-chat--set-turn-time (plist-get event :turn-id)
                             :ended-at
                             (plist-get event :created-at))
     (e-chat--run-pending-activity-redraw)
     (e-chat--set-status "done")
     (e-chat--ensure-composer)
     (e-chat--refresh-composer-position))
    ('turn-failed
     (e-chat--cancel-pending-activity-redraw (plist-get event :turn-id))
     (e-chat--set-status "error")
     (e-chat--render-turn-failure
      (plist-get event :turn-id)
      (plist-get event :created-at)
      (plist-get event :payload)
      t))
    ('turn-cancelled
     (e-chat--set-turn-time (plist-get event :turn-id)
                             :ended-at
                             (plist-get event :created-at))
     (e-chat--settle-open-thinking
      (plist-get event :turn-id)
      (plist-get event :created-at)
      'cancelled)
     (e-chat--cancel-pending-activity-redraw (plist-get event :turn-id))
     (e-chat--stop-progress-indicator (plist-get event :turn-id))
     (e-chat--set-status "cancelled")
     (e-chat--insert-entry "System" "Turn cancelled" t
                           (plist-get event :turn-id)))
    ('compaction-started
     (let ((payload (plist-get event :payload)))
       (e-chat--set-status "compacting")
       (e-chat--insert-entry
        "System"
        (if (plist-get payload :active-turn)
            "Agent compacting context mid-turn"
          "Context compaction started")
        t
        (plist-get event :turn-id))))
    ('compaction-prepared
     (let ((payload (plist-get event :payload)))
       (e-chat--set-status "compaction prepared")
       (e-chat--insert-entry
        "System"
        (format "Compaction prepared; keeping from %s"
                (or (plist-get payload :first-kept-entry-id) "boundary"))
        t
        (plist-get event :turn-id))))
    ('compaction-summary-started
     (e-chat--set-status "summarizing context"))
    ('compaction-finished
     (let ((payload (plist-get event :payload)))
       (e-chat--invalidate-mode-line-context-estimate)
       (e-chat--set-status "compacted" t)
       (e-chat--insert-entry
        "System"
        (format "Context compacted into %s"
                (or (plist-get payload :compaction-id) "summary"))
        t
        (plist-get event :turn-id))
       (e-chat--ensure-composer)
       (e-chat--refresh-composer-position)))
    ('compaction-failed
     (e-chat--set-status "compaction failed")
     (e-chat--insert-entry
      "System"
      (format "Context compaction failed: %s"
              (or (plist-get (plist-get event :payload) :message)
                  "unknown error"))
      t
      (plist-get event :turn-id)))
    ('message-added
     (let* ((message (plist-get (plist-get event :payload) :message))
            (entry (e-chat--message-entry message)))
       (cond
        ((e-chat--tool-message-p message)
         ;; Tool transcript messages are internal model context.  Presentation
         ;; activity is driven only by current durable activity events.
         nil)
        (t
         (when (eq (plist-get message :role) 'assistant)
           (e-chat--set-turn-time (plist-get event :turn-id)
                                  :ended-at
                                  (plist-get event :created-at))
           (e-chat--cancel-pending-activity-redraw (plist-get event :turn-id))
           (e-chat--stop-progress-indicator (plist-get event :turn-id))
           (when-let ((record (e-chat--existing-turn-record
                               (plist-get event :turn-id))))
             (e-chat--render-turn-transient (plist-get event :turn-id) record))
           (e-chat--finalize-turn-display (plist-get event :turn-id)))
         (e-chat--insert-entry (car entry) (cdr entry) nil
                               (plist-get event :turn-id))
         (when (eq (plist-get message :role) 'user)
           (e-chat--refresh-session-display))))))
    ('provider-request-started
     (e-chat--set-status "waiting for provider")
     (e-chat--record-provider-started
      (plist-get event :turn-id)
      (plist-get event :created-at))
     (e-chat--request-activity-redraw (plist-get event :turn-id)))
    ('provider-request-finished
     (e-chat--record-provider-finished
      (plist-get event :turn-id)
      (plist-get event :created-at)
      (plist-get (plist-get event :payload) :status))
     (e-chat--request-activity-redraw (plist-get event :turn-id)))
    ('assistant-delta
     (e-chat--set-status "streaming"))
    ('reasoning-delta
     (e-chat--set-status "reasoning")
     (when-let ((record (e-chat--existing-turn-record
                         (plist-get event :turn-id))))
       (e-chat--record-reasoning-delta
        record
        (plist-get (plist-get event :payload) :content)
        t
        'activity)
       (e-chat--request-activity-redraw (plist-get event :turn-id))))
    ('tool-started
     (e-chat--set-status "tool")
     (when-let ((record (e-chat--existing-turn-record
                         (plist-get event :turn-id))))
       (e-chat--record-tool-started
        record
        (plist-get event :payload)
        'activity)
       (e-chat--request-activity-redraw (plist-get event :turn-id))))
    ('tool-finished
     (e-chat--set-status "tool done")
     (when-let ((record (e-chat--existing-turn-record
                         (plist-get event :turn-id))))
       (e-chat--record-tool-finished
        record
        (plist-get event :payload)
        'activity)
       (e-chat--request-activity-redraw (plist-get event :turn-id))))
    ('backend-empty-output
     (e-chat--cancel-pending-activity-redraw (plist-get event :turn-id))
     (e-chat--stop-progress-indicator (plist-get event :turn-id))
     (e-chat--set-status "done"))
    ('token-usage
     (e-chat--refresh-mode-line-status t))
    ('session-reset
     (e-chat--cancel-pending-activity-redraw)
     (e-chat--stop-progress-indicator)
     (e-chat--set-status "idle")
     (e-chat--insert-entry "System" "Session reset" t))
    (_
     (e-chat--insert-entry "System" (format "Event: %S" event) t))))))

(defun e-chat--tail-messages (messages limit)
  "Return at most LIMIT trailing MESSAGES."
  (if (and (integerp limit)
           (> limit 0)
           (> (length messages) limit))
      (nthcdr (- (length messages) limit) messages)
    messages))

(defun e-chat--terminal-activity-event (turn-id activity-events)
  "Return TURN-ID's terminal failure activity event, or nil."
  (cl-find-if
   (lambda (event)
     (and (equal (plist-get event :turn-id) turn-id)
          (eq (plist-get event :event-type) 'turn-failed)))
   activity-events))

(defun e-chat--render-replayed-terminal-event (turn-id activity-events)
  "Render replayed terminal activity for TURN-ID when no final block exists."
  (when-let ((activity-event
              (e-chat--terminal-activity-event turn-id activity-events)))
    (let ((record (e-chat--turn-record turn-id)))
      (unless (or (plist-get record :final-rendered)
                  (plist-get record :failure-rendered))
        (e-chat--render-turn-activity-events turn-id activity-events)
        (e-chat--render-turn-failure
         turn-id
         (plist-get activity-event :created-at)
         (plist-get activity-event :payload)
         t)))))

(defun e-chat--render-session (&optional messages)
  "Render the attached session transcript in the current buffer.
When MESSAGES is non-nil, render that message list instead of the
attached session's full transcript."
  (let ((messages (or messages
                      (e-harness-messages e-chat-harness e-chat-session-id)))
        (turn-index 0)
        (activity-events (ignore-errors
                           (e-harness-session-activity-events
                            e-chat-harness
                            e-chat-session-id)))
        turn-id
        record)
    (dolist (message messages)
      (when (or (plist-get message :turn-id)
                (not turn-id)
                (eq (plist-get message :role) 'user))
        (let ((next-turn-id
               (or (plist-get message :turn-id)
                   (format "replayed-turn-%d" (1+ turn-index)))))
          (when (and turn-id (not (equal turn-id next-turn-id)))
            (e-chat--render-replayed-terminal-event turn-id activity-events))
          (setq turn-index (1+ turn-index))
          (setq turn-id next-turn-id))
        (setq record (e-chat--turn-record turn-id)))
      (e-chat--record-replayed-message-time record message)
      (unless (e-chat--tool-message-p message)
        (let ((entry (e-chat--message-entry message)))
          (when (eq (plist-get message :role) 'assistant)
            (e-chat--render-turn-activity-events turn-id activity-events)
            (e-chat--finalize-turn-display turn-id))
          (e-chat--insert-entry (car entry) (cdr entry) t turn-id))))
    (when turn-id
      (e-chat--render-replayed-terminal-event turn-id activity-events))))

(cl-defun e-chat-open (&key harness session-id new-session)
  "Attach and return an e chat buffer.
HARNESS, SESSION-ID, and NEW-SESSION are injectable for presentation tests and
reload.  User-facing commands should call `e-chat-new' or `e-chat-resume'."
  (let* ((chat-harness (or harness (e-chat--default-harness)))
         (session (when (or new-session (not session-id))
                    (e-chat--create-session chat-harness)))
         (chat-session-id (or session-id
                              (plist-get session :id)
                              e-chat-default-session-id))
         (buffer (or (e-chat--find-session-buffer chat-session-id)
                     (get-buffer-create
                      (e-chat--session-buffer-name
                       chat-harness
                       chat-session-id)))))
    (e-chat--attach-buffer buffer chat-harness chat-session-id)
    buffer))

(cl-defun e-chat-create-session (&key harness metadata id)
  "Create and return a chat session in HARNESS with METADATA and optional ID."
  (e-harness-create-session
   (or harness (e-chat--default-harness))
   :id id
   :metadata metadata))

(cl-defun e-chat-submit-session
    (harness session-id prompt &key references delay metadata)
  "Submit PROMPT with REFERENCES and METADATA to HARNESS SESSION-ID."
  (e-chat-session-submit
   harness
   session-id
   prompt
   :references references
   :delay delay
   :metadata metadata))

(defun e-chat-open-session (harness session-id &optional display)
  "Open HARNESS SESSION-ID and display it when DISPLAY is non-nil."
  (let ((buffer (e-chat-open :harness harness :session-id session-id)))
    (when display
      (e-chat--pop-to-buffer buffer))
    buffer))

(defun e-chat--attach-buffer (buffer harness session-id)
  "Attach BUFFER to HARNESS and SESSION-ID."
  (e-chat--ensure-session harness session-id)
  (with-current-buffer buffer
    (e-chat-session-ensure-project-root
     harness session-id (e-chat--project-root default-directory))
    (e-chat--unsubscribe)
    (e-chat-mode)
    (e-chat--disable-modal-editing)
    (e-chat--disable-completion)
    (e-chat--ensure-window-refresh-hook)
    (setq-local e-current-harness harness)
    (setq-local e-chat-harness harness)
    (setq-local e-chat-session-id session-id)
    (e-chat--rename-buffer-for-session)
    (e-chat--clear)
    (e-chat--render-session)
    (e-chat--set-status "idle" t)
    (e-chat--subscribe harness buffer session-id))
  buffer)

(defun e-chat-reload-buffers ()
  "Refresh live e chat buffers after development reload."
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (derived-mode-p 'e-chat-mode)
            (let ((session-id (or e-chat-session-id
                                  e-chat-default-session-id)))
              (setq count (1+ count))
              (e-chat--attach-buffer
               buffer
               (e-chat--default-harness)
               session-id))))))
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
With prefix argument POP-TO-SIDE, use the pop display path."
  (interactive "P")
  (let ((buffer (e-chat-open :new-session t)))
    (when (called-interactively-p 'interactive)
      (if pop-to-side
          (e-chat--pop-to-buffer buffer)
        (e-chat--switch-to-buffer buffer)))
    buffer))

(defun e-chat--session-choice-label (session)
  "Return completion label for SESSION metadata."
  (format "%s  [%s]"
          (plist-get session :title)
          (plist-get session :id)))

(defun e-chat--session-for-label (sessions labels label)
  "Return the session from SESSIONS corresponding to LABELS LABEL."
  (when-let ((index (cl-position label labels :test #'equal)))
    (nth index sessions)))

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

(defun e-chat--resume-preview-origin-window ()
  "Return the window that should display resume previews."
  (or (and (minibufferp)
           (window-live-p (minibuffer-selected-window))
           (minibuffer-selected-window))
      (selected-window)))

(defun e-chat--render-resume-preview (harness session)
  "Render SESSION from HARNESS into the reusable resume preview buffer."
  (let* ((session-id (plist-get session :id))
         (buffer (get-buffer-create e-chat--resume-preview-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (e-chat-mode)
        (e-chat--disable-modal-editing)
        (e-chat--disable-completion)
        (setq-local e-chat-harness harness)
        (setq-local e-chat-session-id session-id)
        (setq-local cursor-type nil)
        (e-chat--clear t)
        (if (plist-get session :loaded)
            (let ((messages (e-chat--tail-messages
                             (e-harness-messages harness session-id)
                             e-chat-resume-preview-message-limit)))
              (e-chat--render-session messages))
          (e-chat--insert-protected
           (string-join
            (delq nil
                  (list (plist-get session :title)
                        (plist-get session :summary)
                        (when-let ((message-count
                                    (plist-get session :message-count)))
                          (format "%d messages" message-count))
                        (plist-get session :last-message-at)))
            "\n\n")))
        (setq buffer-read-only t)
        (goto-char (point-min))))
    buffer))

(defun e-chat--resume-preview-state (harness sessions labels)
  "Return Consult preview state for HARNESS resume SESSIONS and LABELS."
  (let (origin-window origin-buffer preview-buffer)
    (cl-labels
        ((ensure-origin ()
           (unless (window-live-p origin-window)
             (setq origin-window (e-chat--resume-preview-origin-window))
             (setq origin-buffer (window-buffer origin-window))))
         (restore-origin (&optional kill-preview)
           (when (and (window-live-p origin-window)
                      (buffer-live-p origin-buffer))
             (with-selected-window origin-window
               (switch-to-buffer origin-buffer 'norecord)))
           (when (and kill-preview
                      (buffer-live-p preview-buffer))
             (kill-buffer preview-buffer)
             (setq preview-buffer nil))))
      (lambda (action candidate)
        (pcase action
          ('setup
           (ensure-origin))
          ('preview
           (ensure-origin)
           (if-let ((session (e-chat--session-for-label
                              sessions labels candidate)))
               (when (window-live-p origin-window)
                 (setq preview-buffer
                       (e-chat--render-resume-preview harness session))
                 (with-selected-window origin-window
                   (switch-to-buffer preview-buffer 'norecord)))
             (restore-origin)))
          ((or 'exit 'return)
           (restore-origin t)))))))

(defun e-chat--consult-read-available-p ()
  "Return non-nil when Consult's previewing reader is available."
  (and (require 'consult nil t)
       (fboundp 'consult--read)))

(defun e-chat--read-session-choice (harness sessions &optional labels)
  "Read and return a resume choice for SESSIONS in HARNESS."
  (let ((labels (or labels
                    (mapcar #'e-chat--session-choice-label sessions))))
    (if (e-chat--consult-read-available-p)
        (funcall (symbol-function 'consult--read)
                 labels
                 :prompt "Resume e session: "
                 :require-match t
                 :sort nil
                 :category 'e-chat-session
                 :state (e-chat--resume-preview-state
                         harness sessions labels))
      (completing-read "Resume e session: " labels nil t))))

(defun e-chat--latest-session-id (harness)
  "Return the latest session id in HARNESS, creating one when none exists."
  (or (plist-get (car (e-harness-session-list harness)) :id)
      (plist-get (e-chat--create-session harness) :id)))

(defun e-chat--context-session-picker (harness)
  "Return a session id selected from HARNESS, or create a new session."
  (let* ((sessions (e-harness-session-list harness))
         (labels (cons e-chat--new-context-session-label
                       (mapcar #'e-chat--session-choice-label sessions)))
         (selected (completing-read "Add context to e session: "
                                    (e-chat--ordered-completion-table
                                     labels
                                     'e-chat-session)
                                    nil
                                    t)))
    (if (equal selected e-chat--new-context-session-label)
        (plist-get (e-chat--create-session harness) :id)
      (let ((index (cl-position selected (cdr labels) :test #'equal)))
        (unless index
          (user-error "No e chat session selected"))
        (plist-get (nth index sessions) :id)))))

(defun e-chat--session-buffer-for-context (harness session-id)
  "Return the chat buffer for HARNESS SESSION-ID, preserving drafts when live."
  (or (e-chat--find-session-buffer session-id)
      (e-chat-open :harness harness :session-id session-id)))

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

(defun e-chat--default-context-session-id (harness)
  "Return visible chat session id in HARNESS, falling back to latest."
  (or (when-let ((buffer (e-chat--visible-session-buffer harness)))
        (with-current-buffer buffer
          e-chat-session-id))
      (e-chat--latest-session-id harness)))

(defun e-chat--add-context-reference-to-session
    (reference harness session-id &optional display)
  "Insert REFERENCE into HARNESS SESSION-ID composer.
When DISPLAY is non-nil, show the target chat buffer."
  (let ((buffer (e-chat--session-buffer-for-context harness session-id)))
    (with-current-buffer buffer
      (e-chat--enter-composer-input-state)
      (e-chat--insert-context-reference reference)
      (e-chat--show-composer))
    (when display
      (e-chat--pop-to-buffer buffer))
    buffer))

;;;###autoload
(defun e-chat-resume ()
  "Resume a recent persisted e chat session."
  (interactive)
  (let* ((harness (e-chat--default-harness))
         (sessions (e-harness-session-list harness)))
    (unless sessions
      (user-error "No e chat sessions to resume"))
    (let* ((labels (mapcar #'e-chat--session-choice-label sessions))
           (selected (e-chat--read-session-choice harness sessions labels))
           (index (cl-position selected labels :test #'equal))
           (session (nth index sessions))
           (buffer (e-chat-open :harness harness
                                :session-id (plist-get session :id))))
      (when (called-interactively-p 'interactive)
        (e-chat--pop-to-buffer buffer))
      buffer)))

;;;###autoload
(defun e-chat-switch-session ()
  "Switch to a recent persisted e chat session."
  (interactive)
  (e-chat-resume))

(defun e-chat-overview--state-path (&optional harness)
  "Return the JSON read-marker file for HARNESS."
  (or e-chat-overview-state-file
      (let* ((store (and harness (e-harness-sessions harness)))
             (directory (or (and store (e-session-store-directory store))
                            e-session-directory)))
        (expand-file-name "chat-overview-state.json"
                          (file-name-as-directory directory)))))

(defun e-chat-overview--read-state (&optional harness)
  "Return persisted overview read state for HARNESS."
  (let ((file (e-chat-overview--state-path harness)))
    (if (not (file-readable-p file))
        nil
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (condition-case nil
            (mapcar
             (lambda (entry)
               (cons (if (symbolp (car entry))
                         (symbol-name (car entry))
                       (car entry))
                     (cdr entry)))
             (json-parse-buffer :object-type 'alist
                                :array-type 'list
                                :null-object nil
                                :false-object nil))
          (json-parse-error nil))))))

(defun e-chat-overview--write-state (state &optional harness)
  "Persist overview read STATE for HARNESS."
  (let ((file (e-chat-overview--state-path harness)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (let ((coding-system-for-write 'utf-8))
        (insert (json-encode state))))))

(defun e-chat-overview--read-marker (session-id &optional harness)
  "Return the stored read marker for SESSION-ID in HARNESS."
  (alist-get session-id (e-chat-overview--read-state harness)
             nil nil #'equal))

(defun e-chat-overview--set-read-marker (session-id marker &optional harness)
  "Set SESSION-ID read marker to MARKER in HARNESS."
  (let ((state (e-chat-overview--read-state harness)))
    (setf (alist-get session-id state nil nil #'equal) marker)
    (e-chat-overview--write-state state harness)
    marker))

(defun e-chat-overview--latest-assistant-marker (harness session)
  "Return SESSION's latest assistant message marker from HARNESS, if loaded."
  (when (plist-get session :loaded)
    (let ((session-id (plist-get session :id))
          marker)
      (dolist (message (reverse (e-harness-messages harness session-id)))
        (when (and (not marker)
                   (eq (plist-get message :role) 'assistant))
          (setq marker (or (plist-get message :id)
                           (plist-get message :created-at)))))
      marker)))

(defun e-chat-overview--session-unread-p (harness session)
  "Return non-nil when SESSION has unread assistant output in HARNESS."
  (when-let ((marker (e-chat-overview--latest-assistant-marker harness session)))
    (not (equal marker
                (e-chat-overview--read-marker
                 (plist-get session :id)
                 harness)))))

(defun e-chat-overview--session-id-at-point ()
  "Return overview session id at point, or nil."
  (or (get-text-property (point) 'e-chat-session-id)
      (get-text-property (line-beginning-position) 'e-chat-session-id)))

(defun e-chat-overview--compact-row-text (text max-chars)
  "Return TEXT as a single overview row fragment capped at MAX-CHARS."
  (when (stringp text)
    (let ((lines (split-string (replace-regexp-in-string "\r" "" text) "\n"))
          compact)
      (while (and lines (not compact))
        (let* ((line (pop lines))
               (line (replace-regexp-in-string
                      "</?reference\\b[^>]*>" "" line))
               (line (string-trim line)))
          (unless (or (string-empty-p line)
                      (string-match-p "</?reference\\b" line)
                      (string= line "References:")
                      (string-match-p "\\`\\[[^]]+\\]" line))
            (setq compact (replace-regexp-in-string "[ \t]+" " " line)))))
      (when compact
        (if (> (length compact) max-chars)
            (concat (substring compact 0 max-chars) "...")
          compact)))))

(defun e-chat-overview--compact-timestamp (timestamp)
  "Return TIMESTAMP in compact sidebar form."
  (if (and (stringp timestamp)
           (string-match
            "\\`[0-9]\\{4\\}-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T\\([0-9][0-9]\\):\\([0-9][0-9]\\)"
            timestamp))
      (format "%s-%s %s:%s"
              (match-string 1 timestamp)
              (match-string 2 timestamp)
              (match-string 3 timestamp)
              (match-string 4 timestamp))
    timestamp))

(defun e-chat-overview--insert-faced (text face)
  "Insert TEXT with FONT-LOCK FACE."
  (let ((start (point)))
    (insert text)
    (add-text-properties start (point) `(font-lock-face ,face))))

(defun e-chat-overview--summary-duplicates-title-p (summary title)
  "Return non-nil when SUMMARY is already represented by TITLE."
  (and (stringp summary)
       (stringp title)
       (let ((prefix (if (string-suffix-p "..." title)
                         (string-remove-suffix "..." title)
                       title)))
         (or (string= summary title)
             (and (not (string-empty-p prefix))
                  (string-prefix-p prefix summary))))))

(defun e-chat-overview--insert-session-row (harness session)
  "Insert one overview row for SESSION from HARNESS."
  (let* ((session-id (plist-get session :id))
         (summary (e-chat-overview--compact-row-text
                   (plist-get session :summary)
                   72))
         (title (or (e-chat-overview--compact-row-text
                     (plist-get session :title)
                     48)
                    summary
                    session-id))
         (message-count (or (plist-get session :message-count) 0))
         (last-message-at (or (plist-get session :last-message-at)
                              (plist-get session :created-at)))
         (metadata (string-join
                    (delq nil
                          (list (format "[%s]"
                                        (e-chat--short-session-id session-id))
                                (when (> message-count 0)
                                  (format "%d %s"
                                          message-count
                                          (if (= message-count 1)
                                              "msg"
                                            "msgs")))
                                (e-chat-overview--compact-timestamp
                                 last-message-at)))
                    "  "))
         (unread (e-chat-overview--session-unread-p harness session))
         (start (point)))
    (e-chat-overview--insert-faced (if unread "! " "  ")
                                   (if unread
                                       'e-chat-overview-unread-face
                                     'e-chat-overview-meta-face))
    (e-chat-overview--insert-faced title 'e-chat-overview-title-face)
    (insert "\n  ")
    (e-chat-overview--insert-faced metadata 'e-chat-overview-meta-face)
    (when (and summary
               (not (e-chat-overview--summary-duplicates-title-p
                     summary title)))
      (insert "\n  ")
      (e-chat-overview--insert-faced summary 'e-chat-overview-summary-face))
    (insert "\n\n")
    (add-text-properties start (point)
                         `(e-chat-session-id ,session-id
                           help-echo "RET opens this e chat session"))))

(defun e-chat-overview--render (&optional harness)
  "Render HARNESS sessions into the current overview buffer."
  (let* ((harness (or harness
                      e-chat-overview--harness
                      (e-chat--default-harness)))
         (sessions (e-harness-session-list harness))
         (inhibit-read-only t))
    (setq-local e-chat-overview--harness harness)
    (erase-buffer)
    (if sessions
        (dolist (session sessions)
          (e-chat-overview--insert-session-row harness session))
      (insert "No e chat sessions\n"))
    (goto-char (point-min))))

(defun e-chat-overview--mark-session-read (harness session)
  "Record SESSION's latest assistant message as read in HARNESS."
  (when-let ((marker (e-chat-overview--latest-assistant-marker harness session)))
    (e-chat-overview--set-read-marker
     (plist-get session :id)
     marker
     harness)))

(defun e-chat-overview--session-for-id (harness session-id)
  "Return HARNESS session metadata for SESSION-ID."
  (cl-find session-id
           (e-harness-session-list harness)
           :key (lambda (session) (plist-get session :id))
           :test #'equal))

(defun e-chat-overview--session-row-starts ()
  "Return overview session row starts as (POSITION . SESSION-ID) pairs."
  (let ((pos (point-min))
        (limit (point-max))
        last-id
        rows)
    (while (< pos limit)
      (let ((session-id (get-text-property pos 'e-chat-session-id)))
        (when (and session-id (not (equal session-id last-id)))
          (push (cons pos session-id) rows))
        (setq last-id session-id)
        (setq pos (or (next-single-property-change
                       pos 'e-chat-session-id nil limit)
                      limit))))
    (nreverse rows)))

(defun e-chat-overview--current-session-row-index (rows)
  "Return current session row index in ROWS."
  (let ((session-id (e-chat-overview--session-id-at-point)))
    (or (cl-position session-id rows
                     :key #'cdr
                     :test #'equal)
        (user-error "No e chat session at point"))))

(defun e-chat-overview--preview-session-at-point (&optional display)
  "Preview the overview session at point.
When DISPLAY is non-nil, display the preview buffer."
  (let* ((harness (or e-chat-overview--harness
                      (e-chat--default-harness)))
         (session-id (or (e-chat-overview--session-id-at-point)
                         (user-error "No e chat session at point")))
         (session (or (e-chat-overview--session-for-id harness session-id)
                      (user-error "No e chat session at point")))
         (buffer (e-chat--render-resume-preview harness session)))
    (when display
      (display-buffer buffer))
    buffer))

(defun e-chat-overview--goto-session-row (index)
  "Move point to overview session row INDEX and preview it."
  (let* ((rows (e-chat-overview--session-row-starts))
         (row (nth index rows)))
    (unless row
      (user-error "No e chat session at target"))
    (goto-char (car row))
    (e-chat-overview--preview-session-at-point t)))

(defun e-chat-overview-next-session ()
  "Move to the next overview session row and preview it."
  (interactive)
  (let* ((rows (e-chat-overview--session-row-starts))
         (index (e-chat-overview--current-session-row-index rows)))
    (when (>= (1+ index) (length rows))
      (user-error "No next e chat session"))
    (e-chat-overview--goto-session-row (1+ index))))

(defun e-chat-overview-previous-session ()
  "Move to the previous overview session row and preview it."
  (interactive)
  (let* ((rows (e-chat-overview--session-row-starts))
         (index (e-chat-overview--current-session-row-index rows)))
    (when (<= index 0)
      (user-error "No previous e chat session"))
    (e-chat-overview--goto-session-row (1- index))))

(defun e-chat-overview-open-session ()
  "Open the overview session at point and mark assistant output read."
  (interactive)
  (let* ((harness (or e-chat-overview--harness
                      (e-chat--default-harness)))
         (session-id (or (e-chat-overview--session-id-at-point)
                         (user-error "No e chat session at point")))
         (buffer (e-chat-open :harness harness :session-id session-id)))
    (e-chat-overview--mark-session-read
     harness
     (or (e-chat-overview--session-for-id harness session-id)
         (user-error "No e chat session at point")))
    (when (derived-mode-p 'e-chat-overview-mode)
      (e-chat-overview--render harness))
    (when (called-interactively-p 'interactive)
      (e-chat--pop-to-buffer buffer))
    buffer))

(defun e-chat-overview-preview-session ()
  "Preview the overview session at point."
  (interactive)
  (e-chat-overview--preview-session-at-point
   (called-interactively-p 'interactive)))

(defun e-chat-overview-refresh ()
  "Refresh the current overview buffer."
  (interactive)
  (e-chat-overview--render))

(defun e-chat-overview--unsubscribe ()
  "Unsubscribe the current overview buffer from harness events."
  (when (and e-chat-overview--harness
             e-chat-overview--subscription)
    (e-harness-unsubscribe e-chat-overview--harness
                           e-chat-overview--subscription)
    (setq e-chat-overview--subscription nil)))

(defun e-chat-overview--subscribe (buffer harness)
  "Subscribe BUFFER to HARNESS events for overview refreshes."
  (with-current-buffer buffer
    (e-chat-overview--unsubscribe)
    (let ((target buffer))
      (setq-local
       e-chat-overview--subscription
       (e-harness-subscribe
        harness
        (lambda (_event)
          (when (buffer-live-p target)
            (with-current-buffer target
              (when (derived-mode-p 'e-chat-overview-mode)
                (e-chat-overview--render harness))))))))))

(defun e-chat-overview--display (buffer)
  "Display overview BUFFER as the chat session sidebar."
  (display-buffer-in-side-window
   buffer
   '((side . left)
     (slot . -1)
     (window-width . 36))))

(defun e-chat-overview--visible-window ()
  "Return the visible overview sidebar window, or nil."
  (when-let ((buffer (get-buffer e-chat-overview-buffer-name))
             (window (get-buffer-window buffer t)))
    (and (window-live-p window) window)))

;;;###autoload
(defun e-chat-overview ()
  "Open the e chat session overview sidebar."
  (interactive)
  (let* ((harness (e-chat--default-harness))
         (buffer (get-buffer-create e-chat-overview-buffer-name)))
    (with-current-buffer buffer
      (e-chat-overview-mode)
      (setq-local e-chat-overview--harness harness)
      (e-chat-overview--render harness)
      (e-chat-overview--subscribe buffer harness))
    (when (called-interactively-p 'interactive)
      (e-chat-overview--display buffer))
    buffer))

;;;###autoload
(defun e-chat-overview-close ()
  "Close the e chat session overview sidebar."
  (interactive)
  (let ((buffer (get-buffer e-chat-overview-buffer-name)))
    (when (buffer-live-p buffer)
      (when-let ((window (get-buffer-window buffer t)))
        (delete-window window))
      (kill-buffer buffer))))

;;;###autoload
(defun e-chat-sidebar-toggle ()
  "Open or close the e chat session overview sidebar."
  (interactive)
  (if (e-chat-overview--visible-window)
      (e-chat-overview-close)
    (let ((window (e-chat-overview--display (e-chat-overview))))
      (when (window-live-p window)
        (select-window window)))))

;;;###autoload
(defun e-chat-add-context-to-latest ()
  "Add current point or region to a visible, or latest, e chat session."
  (interactive)
  (let* ((reference (e-chat--capture-context-reference-for-command))
         (harness (e-chat--default-harness))
         (session-id (e-chat--default-context-session-id harness)))
    (e-chat--add-context-reference-to-session
     reference
     harness
     session-id
     (called-interactively-p 'interactive))))

;;;###autoload
(defun e-chat-add-context-to-session ()
  "Add the current point or active region to a selected e chat session."
  (interactive)
  (let* ((reference (e-chat--capture-context-reference-for-command))
         (harness (e-chat--default-harness))
         (session-id (e-chat--context-session-picker harness)))
    (e-chat--add-context-reference-to-session
     reference
     harness
     session-id
     (called-interactively-p 'interactive))))

;;;###autoload
(defun e-chat-rename (name)
  "Rename the current e chat session to NAME."
  (interactive
   (let ((current (and e-chat-harness
                       e-chat-session-id
                       (ignore-errors
                         (e-harness-session-name
                          e-chat-harness
                          e-chat-session-id)))))
     (list (read-string "Session name: " current))))
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat-session-rename e-chat-harness e-chat-session-id name)
  (e-chat--rename-buffer-for-session)
  (e-chat--clear)
  (e-chat--render-session)
  (e-chat--set-status "idle" t)
  (current-buffer))

;;;###autoload
(defun e-chat-set-model (model)
  "Set MODEL for the current chat session."
  (interactive
   (let* ((options (and e-chat-harness
                       e-chat-session-id
                       (ignore-errors
                         (e-harness-turn-options
                          e-chat-harness
                          e-chat-session-id))))
          (current (plist-get options :model)))
     (list (read-string "Model: " current nil current))))
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat-session-set-model e-chat-harness e-chat-session-id model)
  (e-chat--set-status "idle" t)
  (message "Set e chat model to %s" (if (string-empty-p model) "default" model)))

;;;###autoload
(defun e-chat-set-effort (effort)
  "Set reasoning EFFORT for the current chat session."
  (interactive
   (let* ((options (and e-chat-harness
                       e-chat-session-id
                       (ignore-errors
                         (e-harness-turn-options
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
  (e-chat-session-set-effort e-chat-harness e-chat-session-id effort)
  (e-chat--set-status "idle" t)
  (message "Set e chat effort to %s"
           (if (string-empty-p effort) "default" effort)))

;;;###autoload
(defun e-chat-show-context ()
  "Show the current chat session context in a read-only temp buffer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat--display-context-buffer
   (e-chat-session-context e-chat-harness e-chat-session-id)
   e-chat-session-id))

;;;###autoload
(defun e-chat-compact-session (&optional instructions)
  "Compact the current chat session transcript."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat-session-compact
   e-chat-harness
   e-chat-session-id
   :instructions instructions))

;;;###autoload
(defun e-chat-submit (&optional prompt)
  "Submit PROMPT or the current editable prompt text."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (let* ((submission (unless prompt (e-chat--composer-submission)))
         (references (plist-get submission :references)))
    (setq prompt (or prompt (plist-get submission :prompt)))
    (e-chat-session-submit e-chat-harness e-chat-session-id prompt
                           :delay e-chat-submit-backend-delay
                           :references references))
  (e-chat--delete-composer)
  (e-chat--set-status "queued")
  (e-chat--insert-composer)
  (redisplay t))

;;;###autoload
(defun e-chat-abort ()
  "Abort the active turn for the current chat buffer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat-session-abort e-chat-harness e-chat-session-id))

;;;###autoload
(defun e-chat-reset ()
  "Reset the current chat session and rendered buffer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat--clear)
  (e-chat-session-reset e-chat-harness e-chat-session-id))

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
     :id 'show-context
     :summary "Show the current chat session context."
     :interactive 'e-chat-show-context
     :function 'e-chat-show-context
     :scope 'session)
    (e-shell-command-create
     :id 'compact-session
     :summary "Compact the current chat session context."
     :interactive 'e-chat-compact-session
     :function 'e-chat-compact-session
     :scope 'session)
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
     :id 'reset
     :summary "Reset the current chat session."
     :interactive 'e-chat-reset
     :function 'e-chat-reset
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
  (e-chat--configure-modal-editing-policy)
  (e-chat--refresh-keymaps)
  (e-chat-context-mode 1)
  (e-shell-register (e-chat-shell))
  (e-chat-reload-buffers))

(add-hook 'e-startup-shell-hook #'e-chat-startup)

(provide 'e-chat)

;;; e-chat.el ends here
