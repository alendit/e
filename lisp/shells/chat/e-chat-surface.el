;;; e-chat-surface.el --- Chat window surface owner -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns chat surface membership, activation, paired composer windows, fitting,
;; splits, output following, and display routing.  The e-chat facade composes
;; this owner with transcript, composer, activity, and overview owners.

;;; Code:

(require 'cl-lib)
(require 'e-context-status)
(require 'e-harness)
(require 'e-session)
(require 'e-ui-work)
(require 'e-work)
(require 'e-workspaces)

(defcustom e-chat-mode-line-status-delay 0.15
  "Seconds to coalesce non-terminal chat mode-line status updates."
  :type 'number
  :group 'e-chat)

(defcustom e-chat-ui-work-diagnostics nil
  "When non-nil, show pending UI work counts in the chat header line."
  :type 'boolean
  :group 'e-chat)

(defcustom e-chat-mode-line-context-estimate-cache-seconds 2.0
  "Seconds to reuse approximate context-token estimates for mode-line refreshes."
  :type 'number
  :group 'e-chat)

(define-obsolete-variable-alias
  'e-chat-model-context-token-limits
  'e-context-budget-model-token-limits
  "0.1.0")

(defcustom e-chat-context-token-estimate-bytes-per-token 4.0
  "Approximate UTF-8 bytes per token for mode-line context estimates."
  :type 'number
  :group 'e-chat)

(defcustom e-chat-visual-fill-column 80
  "Column at which chat buffer text wraps, or nil to wrap at the window edge.
When set to an integer, chat buffers enable `visual-line-mode' and
`visual-fill-column-mode' so long lines wrap at that column instead of
running the full width of a wide frame.  When nil, no wrapping column is
imposed and text follows the default window-edge behaviour.

The wrapping is visual only: it never inserts hard line breaks into the
transcript or the composed prompt.  `visual-fill-column-mode' provides the
measured wrap; when that package is unavailable, `visual-line-mode'
is enabled as a word-wrap fallback."
  :type '(choice (const :tag "Wrap at window edge" nil)
                 (integer :tag "Wrap column"))
  :group 'e-chat)

(defcustom e-chat-composer-window-min-height 5
  "Minimum height of the paired chat composer window."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-composer-window-max-height 10
  "Maximum height of the paired chat composer window."
  :type 'integer
  :group 'e-chat)

(declare-function visual-fill-column-mode "ext:visual-fill-column")
(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")
(declare-function e-chat-service-session-store "e-chat-service")
(declare-function e-chat-service-state "e-chat-service")

(defvar e-chat-session-metadata nil
  "Detached metadata for the chat presentation's current session.")

(defvar-local e-chat-surface--output-follow-command-state nil
  "Paired transcript viewport captured before the current user command.
This transient command state belongs to the surface because output-follow
intent is a window-membership concern shared by transcript and composer
constituents.")

(defvar-local e-chat-surface--surface-composer-buffer nil
  "Composer buffer paired with this transcript buffer, if any.")

(defvar-local e-chat-surface--surface-transcript-buffer nil
  "Transcript buffer owned by this composer buffer, if any.")

(defvar-local e-chat-surface--surface-role nil
  "Semantic role of this chat surface constituent.

The facade sets this to `transcript' for the main chat buffer and the composer
owner sets it to `composer' for an input pane.  Keeping the role here lets
overview/activity consumers identify a standalone transcript before a paired
composer exists without depending on the facade's major-mode symbol.")

(defvar-local e-chat-surface--surface-composer-layout-dirty nil
  "Whether a composer edit may require its paired window to be refitted.")

(defvar-local e-chat-surface--assume-redraw-visible nil
  "Test/development override for the shared chat redraw visibility gate.

The composer and activity owners both need the same answer before doing
expensive presentation work.  Keep this buffer-local override with the
surface, which owns whether a chat is currently displayable, and expose it
through the narrow surface ports below rather than making either owner reach
into the other's state.")

(defvar-local e-chat-surface--surface-command-map-active nil
  "Non-nil when the current buffer owns chat-surface window commands.")

(defvar e-chat-surface--recenter-inhibited nil
  "Non-nil when chat display restoration must not call `recenter'.")

(defvar-local e-chat-surface--surface-activation-handle nil
  "Deferred UI work completing explicit activation of this chat surface.")

(defvar-local e-chat-surface--surface-activation-generation 0
  "Generation token for stale chat-surface activation callbacks.")

(defvar e-chat-harness nil)
(defvar e-chat-harness-instance-id nil)
(defvar e-chat-session-id nil)
(defvar e-chat-composer-window-min-height)
(defvar e-chat-composer-window-max-height)
(defvar e-chat-visual-fill-column)
(defvar e-chat-surface--refresh-visible-composers-in-progress nil
  "Non-nil while visible e chat composers are being refreshed.")
;; Status and mode-line projection are surface state.  Keeping these cells
;; here makes the composer constituent a consumer of the surface contract,
;; rather than a second owner of the transcript's display state.
(defvar-local e-chat-surface--mode-line-status nil
  "Current compact e chat status text shown in the mode line.")
(defvar-local e-chat-surface--mode-line-status-dirty nil
  "Non-nil when a hidden chat buffer needs a scheduled status refresh.")
(defvar-local e-chat-surface--mode-line-status-generation 0
  "Generation used to discard stale scheduled mode-line refreshes.")
(defvar-local e-chat-surface--mode-line-status-prefer-token-usage nil
  "Whether a pending mode-line refresh should prefer provider token usage.")
(defvar-local e-chat-surface--mode-line-context-estimate-cache nil
  "Caller-owned context-token estimate cache for this surface.")
(defvar-local e-chat-surface--mode-line-context-status-cache nil
  "Caller-owned semantic mode-line status cache for this surface.")
(defvar-local e-chat-surface--status nil
  "Current chat status text shown in the header line.")
(defvar-local e-chat-surface--board-status nil
  "Current detached Board run-set compact status for this chat surface.")
(defvar-local e-chat-surface--board-status-action nil
  "Function invoked when the compact Board status link is activated.")
(defvar-local e-chat-surface--running-status-bounds nil
  "Current transient activity bounds published by the activity owner.")

(defun e-chat-surface--chat-buffer-p ()
  "Return non-nil when the current buffer carries chat surface identity.
The surface owner uses the buffer-local harness/session binding instead of
depending on a facade-defined major-mode symbol.  This keeps redisplay and
window callbacks loadable before the composition root is evaluated."
  (or (e-chat-surface--surface-transcript-p)
      (e-chat-surface--surface-composer-p)
      (and (boundp 'e-chat-harness)
           (boundp 'e-chat-session-id)
           e-chat-harness
           e-chat-session-id)))

(defun e-chat-surface--profile-enabled-p ()
  "Return non-nil when developer profiling is currently available."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-chat-surface--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when profiling is enabled."
  (if (e-chat-surface--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-chat-surface--format-token-count (tokens)
  "Return compact display text for TOKENS."
  (e-context-status-format-token-count tokens))

(defun e-chat-surface--format-mode-line-status
    (model effort used-tokens max-tokens &optional approximate)
  "Return compact mode-line text for MODEL, EFFORT, and context usage."
  (e-context-status-format
   "e-chat" model effort used-tokens max-tokens approximate))

(defun e-chat-surface--model-context-token-limit (model)
  "Return the configured max context tokens for MODEL, or nil."
  (e-context-status-model-token-limit model))

(defun e-chat-surface--model-context-window (model)
  "Return MODEL's configured context window in tokens, or nil."
  (e-chat-surface--model-context-token-limit model))

(defun e-chat-surface--display-options ()
  "Return options from defaults plus this surface's detached metadata."
  (let ((options (copy-tree (e-harness-default-options e-chat-harness) t))
        (overrides
         (copy-tree (plist-get e-chat-session-metadata :turn-options) t)))
    (while overrides
      (setq options (plist-put options (pop overrides) (pop overrides))))
    options))

(defun e-chat-surface--context-token-estimate (context)
  "Return approximate token count for model-facing CONTEXT."
  (e-context-status-context-token-estimate
   context e-chat-context-token-estimate-bytes-per-token))

(defun e-chat-surface--mode-line-context-estimate-key ()
  "Return semantic cache key for this surface's context estimate."
  (when (and e-chat-harness e-chat-session-id)
    (ignore-errors
      (let* ((state (e-chat-service-state e-chat-harness e-chat-session-id))
             (options (e-chat-surface--display-options)))
        (list :message-count (plist-get state :message-count)
              :active-turn (plist-get state :active-turn)
              :latest-token-usage-id nil
              :model (plist-get options :model)
              :reasoning-effort (plist-get options :reasoning-effort)
              :layers (e-harness-effective-layer-ids
                       e-chat-harness e-chat-session-id))))))

(defun e-chat-surface--mode-line-status-text (&optional prefer-token-usage)
  "Return semantic mode-line text for the current surface."
  (unless (consp e-chat-surface--mode-line-context-estimate-cache)
    (setq-local e-chat-surface--mode-line-context-estimate-cache (cons nil nil)))
  (unless (consp e-chat-surface--mode-line-context-status-cache)
    (setq-local e-chat-surface--mode-line-context-status-cache (cons nil nil)))
  (let ((e-context-status-estimate-cache-seconds
         e-chat-mode-line-context-estimate-cache-seconds)
        (cache-key (e-chat-surface--mode-line-context-estimate-key))
        (asynchronous-p
         (e-session-async-enabled-p
          (e-chat-service-session-store e-chat-harness))))
    (e-context-status-text
     e-chat-harness e-chat-session-id
     :prefix "e-chat"
     :prefer-token-usage prefer-token-usage
     :estimate-context (and (not asynchronous-p)
                            (not prefer-token-usage))
     :estimate-cache e-chat-surface--mode-line-context-estimate-cache
     :estimate-cache-key cache-key
     :snapshot-cache e-chat-surface--mode-line-context-status-cache
     :snapshot-cache-key
     (list :status-key cache-key
           :prefer-token-usage (and prefer-token-usage t)
           :estimate-context (and (not asynchronous-p)
                                  (not prefer-token-usage)))
     :options (e-chat-surface--display-options)
     :token-limit-function #'e-chat-surface--model-context-window
     :bytes-per-token e-chat-context-token-estimate-bytes-per-token)))

(defun e-chat-surface--mode-line-display-text (status)
  "Return a host-neutral display form of semantic STATUS."
  (replace-regexp-in-string "%" " pct" status t t))

(defun e-chat-surface--refresh-mode-line-status (&optional prefer-token-usage)
  "Refresh this surface's mode-line projection."
  (let ((status (e-chat-surface--mode-line-status-text prefer-token-usage)))
    (unless (equal status e-chat-surface--mode-line-status)
      (setq-local e-chat-surface--mode-line-status status)
      (setq-local mode-name (e-chat-surface--mode-line-display-text status))
      (force-mode-line-update)
      (when-let ((composer (e-chat-surface-composer-buffer)))
        (force-window-update composer)))))

(defun e-chat-surface--request-mode-line-status-refresh
    (&optional prefer-token-usage immediate)
  "Schedule a coalesced mode-line refresh for the current surface."
  (setq-local e-chat-surface--mode-line-status-dirty t)
  (setq-local e-chat-surface--mode-line-status-prefer-token-usage
              (or prefer-token-usage
                  e-chat-surface--mode-line-status-prefer-token-usage))
  (when (e-chat-surface-redraw-visible-p)
    (let ((generation (cl-incf e-chat-surface--mode-line-status-generation))
          (buffer (current-buffer)))
      (e-ui-work-schedule
       (e-ui-work-spec-create
        :id "chat_mode_line_status"
        :description "Refresh coalesced chat mode-line status."
        :owner 'chat-mode-line-status
        :target-buffer buffer
        :key 'status
        :generation generation
        :delay (if immediate 0 e-chat-mode-line-status-delay)
        :coalesce t
        :focus-policy 'preserve
        :reentrancy-policy 'defer
        :stale-p (lambda (_job)
                   (/= generation e-chat-surface--mode-line-status-generation))
        :apply (lambda (_job _handle)
                 (when (= generation e-chat-surface--mode-line-status-generation)
                   (let ((prefer
                          e-chat-surface--mode-line-status-prefer-token-usage))
                     (setq-local e-chat-surface--mode-line-status-dirty nil)
                     (setq-local
                      e-chat-surface--mode-line-status-prefer-token-usage nil)
                     (e-chat-surface--refresh-mode-line-status prefer)))))))))

(defun e-chat-surface--flush-deferred-hidden-mode-line-statuses (&rest _)
  "Schedule deferred mode-line refreshes for visible chat buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (e-chat-surface--chat-buffer-p)
                   e-chat-surface--mode-line-status-dirty
                   (e-chat-surface-redraw-visible-p))
          (e-chat-surface--request-mode-line-status-refresh
           e-chat-surface--mode-line-status-prefer-token-usage t))))))

(defun e-chat-surface--invalidate-mode-line-context-estimate ()
  "Clear this surface's context-token and semantic status caches."
  (setq-local e-chat-surface--mode-line-context-estimate-cache (cons nil nil))
  (setq-local e-chat-surface--mode-line-context-status-cache (cons nil nil)))

(defun e-chat-surface--ui-work-owner-counts ()
  "Return pending UI work counts grouped by owner for this surface."
  (let ((counts (make-hash-table :test 'eq)))
    (dolist (job (e-ui-work-pending (current-buffer)))
      (let ((owner (or (plist-get job :owner) 'unknown)))
        (puthash owner (1+ (gethash owner counts 0)) counts)))
    counts))

(defun e-chat-surface--ui-work-diagnostics-text ()
  "Return compact pending UI work diagnostics for the header line."
  (when-let ((pending (and e-chat-ui-work-diagnostics
                           (e-ui-work-pending (current-buffer)))))
    (let* ((counts (e-chat-surface--ui-work-owner-counts))
           (owners nil))
      (maphash (lambda (owner count)
                 (push (format "%s:%d" owner count) owners))
               counts)
      (format " - ui %d [%s]"
              (length pending)
              (string-join (sort owners #'string<) ", ")))))

(defun e-chat-surface--header-line-text (status)
  "Return header-line text for STATUS and pending-work diagnostics."
  (let* ((diagnostics (or (e-chat-surface--ui-work-diagnostics-text) ""))
         (board-status
          (and e-chat-surface--board-status
               (plist-get e-chat-surface--board-status :text)))
         (board-status-display
          (and board-status
               (if (functionp e-chat-surface--board-status-action)
                   (propertize board-status
                               'mouse-face 'highlight
                               'help-echo "Open Board activity"
                               'keymap
                               (and (boundp 'e-chat-surface-board-status-map)
                                    e-chat-surface-board-status-map))
                 board-status))))
    (if (and e-chat-harness e-chat-session-id)
        (let* ((title (or (plist-get e-chat-session-metadata :name)
                          (plist-get e-chat-session-metadata :title)
                          (when-let* ((summary
                                       (plist-get e-chat-session-metadata
                                                  :summary)))
                            (if (> (length summary) 25)
                                (concat (substring summary 0 25) "...")
                              summary))
                          e-chat-session-id))
               (options
                (append
                 (copy-tree
                  (plist-get e-chat-session-metadata :turn-options) t)
                 (ignore-errors
                   (copy-tree
                    (e-harness-display-options
                     e-chat-harness e-chat-session-id)
                    t))))
               (model (plist-get options :model))
               (effort (e-context-budget-options-effort options)))
          (format "E Chat: %s - %s - %s/%s%s%s"
                  status title (or model "model unset")
                  (or effort "effort unset")
                  (if board-status-display
                      (format " - %s" board-status-display)
                    "")
                  diagnostics))
      (format "E Chat: %s%s%s" status
              (if board-status-display
                  (format " - %s" board-status-display)
                "")
              diagnostics))))

(defun e-chat-surface--refresh-ui-work-diagnostics ()
  "Refresh foreground UI-work diagnostics for this surface."
  (when (and e-chat-ui-work-diagnostics header-line-format)
    (setq header-line-format
          (e-chat-surface--header-line-text e-chat-surface--status))
    (force-mode-line-update t)))

(defun e-chat-surface--set-status (status &optional refresh-mode-line)
  "Set surface STATUS and optionally request a mode-line refresh."
  (if (e-chat-surface--surface-composer-p)
      (with-current-buffer (e-chat-surface--surface-transcript-buffer)
        (e-chat-surface--set-status status refresh-mode-line))
    (unless (and (not refresh-mode-line)
                 (equal status e-chat-surface--status)
                 header-line-format)
      (e-chat-surface--profile-call
       'chat.status
       (list :session-id e-chat-session-id
             :buffer-name (buffer-name)
             :metadata (list :status status
                             :refresh-mode-line (and refresh-mode-line t)))
       (lambda ()
         (setq e-chat-surface--status status)
         (setq header-line-format
               (e-chat-surface--header-line-text status))
         (when refresh-mode-line
           (e-chat-surface--request-mode-line-status-refresh nil t)))))))

(defun e-chat-surface--make-command-map (&optional map)
  "Return high-priority MAP for chat-specific surface commands.
Native atomic windows own structural delete semantics.  This map only keeps
chat focus and external-split policy above host minor-mode remappings."
  (let ((map (or map (make-sparse-keymap))))
    ;; Clear ordinary-pair bindings when refreshing a map created by an older
    ;; loaded e-chat version.
    (define-key map (kbd "C-x 0") nil)
    (define-key map (kbd "C-x 1") nil)
    (define-key map [remap delete-window] nil)
    (define-key map [remap delete-other-windows] nil)
    (define-key map (kbd "C-x o") #'e-chat-surface--other-window)
    (define-key map (kbd "C-x 2") #'e-chat-surface--split-window-below)
    (define-key map (kbd "C-x 3") #'e-chat-surface-split-window-right)
    (define-key map [remap split-window-below]
                #'e-chat-surface--split-window-below)
    (define-key map [remap split-window-right]
                #'e-chat-surface-split-window-right)
    map))

(defvar e-chat-surface-command-map
  (e-chat-surface--make-command-map)
  "High-priority structural command map for a composed chat surface.")

(defvar e-chat-surface--emulation-mode-map-alist
  `((e-chat-surface--surface-command-map-active . ,e-chat-surface-command-map))
  "Emulation map entry keeping surface commands above host minor modes.")

(add-to-list 'emulation-mode-map-alists
             'e-chat-surface--emulation-mode-map-alist)

(defun e-chat-surface-refresh-command-map ()
  "Refresh surface command bindings after a live reload."
  (setq e-chat-surface-command-map
        (e-chat-surface--make-command-map e-chat-surface-command-map)))

(defun e-chat-surface-set-command-map-active (active &optional buffer)
  "Set whether ACTIVE chat surface commands apply in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local e-chat-surface--surface-command-map-active (and active t))))

(defun e-chat-surface-redraw-visible-p (&optional buffer)
  "Return whether BUFFER's chat presentation may redraw immediately.

The explicit override is used by batch tests and development probes for
undisplayed buffers.  In normal use, a live window is the source of truth.
Both composer and activity redraw scheduling consume this surface-owned
answer."
  (with-current-buffer (or buffer (current-buffer))
    (or e-chat-surface--assume-redraw-visible
        (and (get-buffer-window (current-buffer) t) t))))

(defun e-chat-surface-set-redraw-visible (visible &optional buffer)
  "Set BUFFER's test/development redraw visibility override to VISIBLE.

This is intentionally a surface port: the composer and activity owners share
the buffer-local redraw gate but do not own one another's private state."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local e-chat-surface--assume-redraw-visible (and visible t))))

(defun e-chat-surface-set-running-status-bounds (bounds &optional buffer)
  "Publish transient activity BOUNDS for BUFFER's viewport primitives."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local e-chat-surface--running-status-bounds bounds)))

(defun e-chat-surface--running-status-bounds (&optional buffer)
  "Return transient activity bounds published for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    e-chat-surface--running-status-bounds))

(defun e-chat-surface-status (&optional buffer)
  "Return the presentation status for BUFFER's transcript surface."
  (with-current-buffer (or buffer (current-buffer))
    (and (boundp 'e-chat-surface--status)
         e-chat-surface--status)))

(defun e-chat-surface-mode-line-status (&optional buffer)
  "Return the semantic mode-line status for BUFFER's transcript surface."
  (with-current-buffer (or buffer (current-buffer))
    (and (boundp 'e-chat-surface--mode-line-status)
         e-chat-surface--mode-line-status)))

(defun e-chat-surface-mode-line-display-text (status)
  "Return a host-neutral display form of semantic STATUS."
  (e-chat-surface--mode-line-display-text status))

(defun e-chat-surface-mode-line-status-text (&optional prefer-token-usage buffer)
  "Return the semantic mode-line status text for BUFFER.
This is the surface's consumer-shaped status projection; its caches and model
lookup remain private to the surface owner."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--mode-line-status-text prefer-token-usage)))

(defun e-chat-surface-refresh-mode-line-status
    (&optional prefer-token-usage buffer)
  "Refresh BUFFER's mode-line status projection.
PREFER-TOKEN-USAGE requests provider-reported usage when available."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--refresh-mode-line-status prefer-token-usage)))

(defun e-chat-surface-request-mode-line-status-refresh
    (&optional prefer-token-usage immediate buffer)
  "Schedule a coalesced mode-line refresh for BUFFER.
PREFER-TOKEN-USAGE requests provider-reported usage when available.
IMMEDIATE keeps the scheduler's latest-value coalescing while using no delay."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--request-mode-line-status-refresh
     prefer-token-usage immediate)))

(defun e-chat-surface-set-status (status &optional refresh-mode-line)
  "Set the current surface STATUS through its owner boundary."
  (e-chat-surface--set-status status refresh-mode-line))

(defvar e-chat-surface-board-status-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'e-chat-surface-activate-board-status)
    (define-key map [mouse-1] #'e-chat-surface-activate-board-status)
    map)
  "Keymap for the compact Board run-set header link.")

(defun e-chat-surface-activate-board-status (&optional _event)
  "Open the selected run's Board activity link, when present."
  (interactive)
  (if (functionp e-chat-surface--board-status-action)
      (funcall e-chat-surface--board-status-action)
    (user-error "No selected Board run activity link is available")))

(defun e-chat-surface-set-board-status (status &optional action)
  "Install detached compact Board STATUS and optional activity ACTION.
The value is presentation metadata only; the Board layer remains the owner of
the durable run-set and consumers read one detached snapshot."
  (setq-local e-chat-surface--board-status (copy-tree status t)
              e-chat-surface--board-status-action action)
  (when header-line-format
    (setq header-line-format
          (e-chat-surface--header-line-text e-chat-surface--status))
    (force-mode-line-update t))
  status)

(defun e-chat-surface-clear-board-status ()
  "Remove the detached Board status from the current chat surface."
  (e-chat-surface-set-board-status nil nil))

(defun e-chat-surface-refresh-ui-work-diagnostics ()
  "Refresh current chat header diagnostics from pending UI work."
  (e-chat-surface--refresh-ui-work-diagnostics))

(defun e-chat-surface-flush-deferred-mode-line-statuses (&rest args)
  "Flush deferred mode-line status work for visible chat buffers."
  (apply #'e-chat-surface--flush-deferred-hidden-mode-line-statuses args))

(defun e-chat-surface-invalidate-mode-line-context-estimate (&optional buffer)
  "Clear mode-line context caches for BUFFER or the current surface."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--invalidate-mode-line-context-estimate)))

(defun e-chat-surface-running-status-bounds (&optional buffer)
  "Return the transient activity bounds published for BUFFER."
  (e-chat-surface--running-status-bounds buffer))

(defconst e-chat-surface--selected-surface-frame-parameter
  'e-chat-selected-surface
  "Frame parameter holding the last selected chat surface.")

(defun e-chat-surface--transcript-buffer-for (buffer)
  "Return BUFFER's owning chat transcript, or nil.
BUFFER may itself be a transcript or its dedicated composer."
  (when (buffer-live-p buffer)
    (let ((transcript
           (or (buffer-local-value 'e-chat-surface--surface-transcript-buffer buffer)
               buffer)))
      (and (buffer-live-p transcript)
           (buffer-local-value 'e-chat-harness transcript)
           (buffer-local-value 'e-chat-session-id transcript)
           transcript))))

(defun e-chat-surface--selected-chat-surface (&optional selected)
  "Return SELECTED window's chat surface as (TRANSCRIPT . TRANSCRIPT-WINDOW).
SELECTED defaults to the selected window.
The selected window may contain a surface's transcript or dedicated input
pane.  TRANSCRIPT owns output state and
TRANSCRIPT-WINDOW owns the viewport that activation is allowed to move."
  (let* ((selected (or selected (selected-window)))
         (selected-buffer (window-buffer selected))
         (transcript (e-chat-surface--transcript-buffer-for selected-buffer)))
    (when transcript
      (let ((transcript-window
             (if (eq selected-buffer transcript)
                 selected
               (with-current-buffer transcript
                 (cl-find-if
                  (lambda (window)
                    (and (eq (window-frame window) (window-frame selected))
                         (e-chat-surface--surface-window-directly-below-p
                          window selected)))
                  (get-buffer-window-list transcript nil t))))))
        (when (and (window-live-p transcript-window)
                   (eq (window-buffer transcript-window) transcript))
          (cons transcript transcript-window))))))

(defun e-chat-surface--selected-chat-buffer ()
  "Return the transcript owning the selected e-chat surface, or nil."
  (car-safe (e-chat-surface--selected-chat-surface)))

(defun e-chat-surface--show-surface-latest-output (surface)
  "Show latest output in SURFACE without changing its selected input pane."
  (let ((buffer (car-safe surface))
        (window (cdr-safe surface)))
    (when (and (buffer-live-p buffer)
               (window-live-p window)
               (eq (window-buffer window) buffer))
      (with-current-buffer buffer
        (e-chat-surface--show-latest-output window)))))

(defun e-chat-surface--complete-surface-activation-if-selected (surface)
  "Complete SURFACE activation when it remains selected after redisplay."
  (let* ((window (cdr-safe surface))
         (frame (and (window-live-p window) (window-frame window))))
    (when (and (frame-live-p frame)
               (equal surface
                      (e-chat-surface--selected-chat-surface
                       (frame-selected-window frame))))
      (with-current-buffer (car surface)
        (when-let ((composer e-chat-surface--surface-composer-buffer))
          ;; The composer owner creates and binds the constituent.  Surface
          ;; activation only restores an already-established pair.
          (e-chat-surface--surface-display-composer window t composer)))
      (e-chat-surface--show-surface-latest-output surface))))

(defun e-chat-surface--schedule-surface-activation (surface)
  "Schedule a post-focus latest-output restore for selected SURFACE."
  (let ((buffer (car-safe surface)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (e-work-handle-p e-chat-surface--surface-activation-handle)
          (e-ui-work-cancel e-chat-surface--surface-activation-handle)
          (setq e-chat-surface--surface-activation-handle nil))
        (setq e-chat-surface--surface-activation-generation
              (1+ e-chat-surface--surface-activation-generation))
        (let ((generation e-chat-surface--surface-activation-generation))
          (setq e-chat-surface--surface-activation-handle
                (e-ui-work-schedule
                 (e-ui-work-spec-create
                  :id "chat_surface_activation"
                  :description "Finish chat surface activation after focus settles."
                  :owner 'surface-activation
                  :target-buffer buffer
                  :key (cdr surface)
                  :generation generation
                  :delay 0
                  :coalesce t
                  :focus-policy 'explicit
                  :reentrancy-policy 'defer
                  :apply
                  (lambda (_job _handle)
                    (setq e-chat-surface--surface-activation-handle nil)
                    (when (= generation e-chat-surface--surface-activation-generation)
                      (e-chat-surface--complete-surface-activation-if-selected
                       surface))))
                 :on-event (lambda (&rest _)
                             (e-chat-surface--refresh-ui-work-diagnostics)))))))))

(defun e-chat-surface--activate-surface (surface)
  "Explicitly activate SURFACE at its latest output and input pane.
The immediate update is the presentation contract.  A single coalesced retry
wins host window-restoration races without observing every unrelated window
configuration change."
  (when surface
    (let* ((frame (window-frame (cdr surface)))
           (selected (frame-selected-window frame)))
      (when (equal surface
                   (e-chat-surface--selected-chat-surface
                    selected))
        (set-frame-parameter frame
                             e-chat-surface--selected-surface-frame-parameter
                             surface)
        ;; Entering a composed surface through its transcript routes input to
        ;; the composer.  Internal composer-to-transcript navigation does not
        ;; activate the already-selected surface again and remains focused.
        (when (eq (window-buffer selected) (car surface))
          ;; Input focus is a surface-membership operation once the composer
          ;; has been established by its owner.  Do not create state here, but
          ;; preserve the established pair's activation behavior.
          (when-let ((composer e-chat-surface--surface-composer-buffer))
            (e-chat-surface--surface-display-composer selected t composer)))))
    (e-chat-surface--show-surface-latest-output surface)
    (e-chat-surface--schedule-surface-activation surface)))

(defun e-chat-surface--activate-selected-surface-on-selection (&optional changed-frame)
  "Activate latest output when selection enters a different chat surface."
  (let* ((frame (if (frame-live-p changed-frame)
                    changed-frame
                  (selected-frame)))
         (surface (e-chat-surface--selected-chat-surface
                   (frame-selected-window frame)))
         (previous (frame-parameter
                    frame e-chat-surface--selected-surface-frame-parameter)))
    (set-frame-parameter frame e-chat-surface--selected-surface-frame-parameter surface)
    (when (and surface (not (equal surface previous)))
      (e-chat-surface--activate-surface surface))))

(defun e-chat-surface--activate-selected-surface-after-window-buffer-change (frame)
  "Activate FRAME's selected chat surface after redisplay settles.
`window-buffer-change-functions' runs from redisplay after a generic buffer or
workspace transition has finished changing the window tree."
  (when (frame-live-p frame)
    (let ((surface (e-chat-surface--selected-chat-surface
                    (frame-selected-window frame))))
      (when surface
        ;; Redisplay callbacks run in the middle of the host's window update.
        ;; Defer composer display until the coalesced UI-work job, while the
        ;; selection hook remains immediate for ordinary user transitions.
        (e-chat-surface--schedule-surface-activation surface)))))

(defun e-chat-surface--ensure-window-selection-hook ()
  "Install chat focus hooks for window and workspace changes."
  (unless (memq #'e-chat-surface-activate-selected
                window-selection-change-functions)
    (add-hook 'window-selection-change-functions
              #'e-chat-surface-activate-selected))
  (when (boundp 'window-buffer-change-functions)
    (unless (memq
             #'e-chat-surface-activate-after-window-change
             window-buffer-change-functions)
      (add-hook
       'window-buffer-change-functions
       #'e-chat-surface-activate-after-window-change))))

(defun e-chat-surface--surface-bind-composer (composer transcript)
  "Bind COMPOSER to its distinct owning TRANSCRIPT and chat context."
  (unless (and (buffer-live-p composer)
               (buffer-live-p transcript)
               (not (eq composer transcript)))
    (signal 'wrong-type-argument
            (list 'distinct-live-chat-surface-buffers composer transcript)))
  (with-current-buffer composer
    (setq-local e-chat-surface--surface-transcript-buffer transcript)
    (setq-local e-current-harness
                (buffer-local-value 'e-current-harness transcript))
    (setq-local e-chat-harness
                (buffer-local-value 'e-chat-harness transcript))
    (setq-local e-chat-harness-instance-id
                (buffer-local-value 'e-chat-harness-instance-id transcript))
    (setq-local e-chat-session-id
                (buffer-local-value 'e-chat-session-id transcript))
    (setq-local default-directory
                (buffer-local-value 'default-directory transcript))
    (when-let ((workspace (e-buffer-workspace transcript)))
      (e-buffer-set-workspace composer workspace)
      (e-workspace-add-buffer composer workspace)))
  composer)

(defun e-chat-surface--surface-ensure-composer ()
  "Return the already-bound composer for the current surface.
Creation and editable-buffer initialization belong to the composer owner;
this primitive only reports the pairing maintained by the surface."
  (if (e-chat-surface--surface-composer-p)
      (current-buffer)
    (and (buffer-live-p e-chat-surface--surface-composer-buffer)
         e-chat-surface--surface-composer-buffer)))

(defun e-chat-surface--surface-window-directly-below-p
    (transcript-window composer-window)
  "Return non-nil when COMPOSER-WINDOW is directly below TRANSCRIPT-WINDOW."
  (let ((transcript-edges (window-edges transcript-window))
        (composer-edges (window-edges composer-window)))
    (and (= (nth 0 transcript-edges) (nth 0 composer-edges))
         (= (nth 2 transcript-edges) (nth 2 composer-edges))
         (= (nth 3 transcript-edges) (nth 1 composer-edges)))))

(defun e-chat-surface--surface-composer-window (&optional transcript-window)
  "Return TRANSCRIPT-WINDOW's composer constituent, if any.
The composer buffer owns transcript identity.  Native atomic structure and
vertical adjacency identify its visible constituent without changing the host
window tree."
  (setq transcript-window (or transcript-window (selected-window)))
  (when (and (window-live-p transcript-window)
             (buffer-live-p e-chat-surface--surface-composer-buffer))
    (when-let ((atom-root (window-atom-root transcript-window)))
      (cl-find-if
       (lambda (candidate)
         (and (eq atom-root (window-atom-root candidate))
              (e-chat-surface--surface-window-directly-below-p
               transcript-window candidate)))
       (get-buffer-window-list e-chat-surface--surface-composer-buffer nil t)))))

(defun e-chat-surface--surface-member-window-p (window transcript composer)
  "Return non-nil when WINDOW belongs to TRANSCRIPT and COMPOSER's surface."
  (memq (window-buffer window) (list transcript composer)))

(defun e-chat-surface--other-window (&optional arg all-frames)
  "Select the next window outside the current composed chat surface.
The transcript and its composer are one interaction surface: transcript
navigation is explicit, so ordinary window cycling must not land in the
read-only transcript."
  (interactive "^p")
  (let* ((transcript (e-chat-surface--surface-transcript-buffer))
         (composer (and (buffer-live-p transcript)
                        (buffer-local-value 'e-chat-surface--surface-composer-buffer
                                            transcript)))
         (steps (abs (or arg 1)))
         (direction (if (< (or arg 1) 0) -1 1)))
    (if (not (buffer-live-p composer))
        (other-window (or arg 1) all-frames)
      (dotimes (_ steps)
        ;; `other-window' owns the host's frame/minibuffer policy.  We only
        ;; skip the windows that make up this one chat surface.
        (let ((remaining (max 1 (length (window-list nil 'nomini)))))
          (while (and (> remaining 0)
                      (progn
                        (other-window direction all-frames)
                        (e-chat-surface--surface-member-window-p
                         (selected-window) transcript composer)))
            (setq remaining (1- remaining))))))))

(defun e-chat-surface--surface-split-replacement-buffer (transcript composer)
  "Return a current-workspace buffer outside TRANSCRIPT and COMPOSER.
A root split initially duplicates the selected surface constituent.  Replace
that transient internal view before the command returns."
  (let* ((workspace (e-workspace-current))
         (frame (selected-frame))
         (buffer
          (cl-find-if
           (lambda (candidate)
             (let ((name (buffer-name candidate)))
               (and name
                    (not (memq candidate (list transcript composer)))
                    (not (string-prefix-p " " name))
                    (e-workspace-buffer-member-p candidate workspace))))
           (buffer-list frame))))
    (or buffer
        (let ((scratch (get-buffer-create "*scratch*")))
          (unless (e-workspace-buffer-member-p scratch workspace)
            (e-workspace-add-buffer scratch workspace))
          scratch))))

(defun e-chat-surface--surface-split-window (split-function)
  "Use SPLIT-FUNCTION without leaving an independent composer view."
  (let* ((transcript (e-chat-surface--surface-transcript-buffer))
         (composer (and (buffer-live-p transcript)
                        (buffer-local-value 'e-chat-surface--surface-composer-buffer
                                            transcript)))
         (window (funcall split-function)))
    (when (and (window-live-p window)
               (buffer-live-p composer)
               (memq (window-buffer window) (list transcript composer)))
      (set-window-buffer
       window
       (e-chat-surface--surface-split-replacement-buffer transcript composer)))
    window))

(defun e-chat-surface--split-window-below ()
  "Split below the complete composed surface."
  (interactive)
  (e-chat-surface--surface-split-window #'split-window-below))

(defun e-chat-surface-split-window-right ()
  "Split right of the complete composed surface."
  (interactive)
  (e-chat-surface--surface-split-window #'split-window-right))

(defun e-chat-surface--surface-fit-composer-window (&optional composer-window)
  "Fit COMPOSER-WINDOW to its input buffer within configured bounds."
  (when (window-live-p composer-window)
    (fit-window-to-buffer composer-window
                          e-chat-composer-window-max-height
                          e-chat-composer-window-min-height
                          nil nil t)))

(defun e-chat-surface--surface-dedicate-windows (transcript-window composer-window)
  "Reserve TRANSCRIPT-WINDOW and COMPOSER-WINDOW for their chat buffers.
The native atom owns structural split and deletion semantics.  Window
dedication separately prevents generic display commands from replacing either
constituent; windows split outside the atom remain ordinary host windows.
Soft dedication still permits an explicit host operation such as workspace
teardown or state restoration to replace the buffer without chat knowledge."
  (set-window-dedicated-p transcript-window 'soft)
  (set-window-dedicated-p composer-window 'soft))

(defun e-chat-surface--surface-refresh-visible-windows ()
  "Refresh every visible instance of the current transcript surface.
One transcript buffer may be shown in multiple windows.  Refresh each instance
locally so reload reapplies current composer layout and ownership without any
workspace or generic display component knowing about chat composition."
  (dolist (transcript-window
           (get-buffer-window-list (current-buffer) nil t))
    (e-chat-surface--surface-display-composer transcript-window)))

(defun e-chat-surface--surface-display-composer
    (&optional transcript-window select composer)
  "Display COMPOSER below TRANSCRIPT-WINDOW.
COMPOSER defaults to the already-bound surface composer.  Creation and
initialization are deliberately left to `e-chat-composer'.  When SELECT is
non-nil, select the composer window."
  (let* ((transcript (current-buffer))
         (composer (or composer e-chat-surface--surface-composer-buffer))
         (transcript-window (or transcript-window
                                (get-buffer-window transcript t)))
         composer-window)
    (when (and (buffer-live-p composer)
               (window-live-p transcript-window))
      (setq composer-window
            (or (e-chat-surface--surface-composer-window transcript-window)
                (let ((window
                       (display-buffer
                        composer
                        `((display-buffer-in-atom-window)
                          (window . ,transcript-window)
                          (side . below)
                          (window-height
                           . ,e-chat-composer-window-min-height)))))
                  (unless (and (window-live-p window)
                               (eq (window-buffer window) composer)
                               (window-atom-root transcript-window)
                               (eq (window-atom-root transcript-window)
                                   (window-atom-root window)))
                    (error "Could not create atomic e-chat composer window"))
                  window)))
      (e-chat-surface--surface-dedicate-windows transcript-window composer-window)
      (e-chat-surface--surface-fit-composer-window composer-window)
      (when select
        (select-window composer-window)))
    composer-window))

(defun e-chat-surface--surface-mark-composer-layout-dirty (_begin _end _length)
  "Record that this composer changed and may need window sizing work."
  (when (e-chat-surface--surface-composer-p)
    (setq e-chat-surface--surface-composer-layout-dirty t)))

(defun e-chat-surface--surface-composer-post-command ()
  "Keep a visible composer pane fitted after an input command."
  (when (and (e-chat-surface--surface-composer-p)
             e-chat-surface--surface-composer-layout-dirty)
    (setq e-chat-surface--surface-composer-layout-dirty nil)
    (when-let ((transcript e-chat-surface--surface-transcript-buffer))
      (when (buffer-live-p transcript)
        (with-current-buffer transcript
          (dolist (transcript-window
                   (get-buffer-window-list transcript nil t))
            (e-chat-surface--surface-fit-composer-window
             (e-chat-surface--surface-composer-window transcript-window))))))))

(defun e-chat-surface--transcript-windows ()
  "Return every live window currently displaying this transcript buffer."
  (get-buffer-window-list (current-buffer) nil t))

(defconst e-chat-surface--output-follow-window-parameter 'e-chat-output-follow-state
  "Window parameter holding transient follow state for an e chat transcript.")

(defconst e-chat-surface--output-bottom-spacer-property
  'e-chat-output-bottom-spacer-window
  "Overlay property identifying a transcript's window-scoped top spacer.")

(defun e-chat-surface--output-bottom-spacer-overlays ()
  "Return output-bottom spacers anchored at the current buffer's beginning."
  (let* ((start (point-min))
         (candidates
          (append (overlays-at start)
                  (when (< start (point-max))
                    (overlays-in start (1+ start))))))
    (cl-remove-if-not
     (lambda (overlay)
       (overlay-get overlay e-chat-surface--output-bottom-spacer-property))
     (delete-dups candidates))))

(defun e-chat-surface--prune-output-bottom-spacers ()
  "Delete output-bottom spacers whose owning window is no longer usable."
  (dolist (overlay (e-chat-surface--output-bottom-spacer-overlays))
    (let ((window
           (overlay-get overlay e-chat-surface--output-bottom-spacer-property)))
      (unless (and (window-live-p window)
                   (eq (window-buffer window) (current-buffer)))
        (delete-overlay overlay)))))

(defun e-chat-surface--clear-output-bottom-spacer (window)
  "Remove WINDOW's output-bottom alignment spacer from this transcript."
  (dolist (overlay (e-chat-surface--output-bottom-spacer-overlays))
    (when (eq (overlay-get overlay e-chat-surface--output-bottom-spacer-property)
              window)
      (delete-overlay overlay))))

(defun e-chat-surface--set-output-bottom-spacer (window lines)
  "Give pinned transcript WINDOW a top spacer of LINES display rows."
  (e-chat-surface--prune-output-bottom-spacers)
  (e-chat-surface--clear-output-bottom-spacer window)
  (when (and (e-chat-surface--surface-transcript-p) (> lines 0))
    (let ((overlay
           (make-overlay (point-min)
                         (min (point-max) (1+ (point-min)))
                         (current-buffer) nil t)))
      (overlay-put overlay 'window window)
      (overlay-put overlay e-chat-surface--output-bottom-spacer-property window)
      (overlay-put overlay 'before-string (make-string lines ?\n)))))

(defun e-chat-surface--set-window-output-follow (window follow)
  "Record whether WINDOW should FOLLOW the current transcript's live output."
  (unless follow
    (e-chat-surface--clear-output-bottom-spacer window))
  (set-window-parameter
   window e-chat-surface--output-follow-window-parameter
   (cons (current-buffer) follow)))

(defun e-chat-surface--window-reaches-output-p (window tail)
  "Return non-nil when WINDOW's current viewport visibly reaches TAIL."
  (and (window-live-p window)
       (eq (window-buffer window) (current-buffer))
       (integer-or-marker-p tail)
       (>= (window-end window t) tail)))

(defun e-chat-surface--output-follow-position ()
  "Return the position that represents the visible transcript tail."
  (or (cdr (e-chat-surface--running-status-bounds))
      (point-max)))

(defun e-chat-surface--window-follows-output-p (window)
  "Return whether WINDOW follows the current transcript's live output.
Follow intent is explicit after the user moves a viewport.  A new or reused
window derives its initial intent from whether it currently reaches the tail."
  (and (window-live-p window)
       (eq (window-buffer window) (current-buffer))
       (let ((state
              (window-parameter window
                                e-chat-surface--output-follow-window-parameter)))
         (if (and (consp state) (eq (car state) (current-buffer)))
             (cdr state)
           (e-chat-surface--window-reaches-output-p
            window (e-chat-surface--output-follow-position))))))

(defun e-chat-surface--capture-selected-output-follow-command ()
  "Capture the selected transcript viewport before a user command."
  (setq e-chat-surface--output-follow-command-state nil)
  (when-let* ((surface (e-chat-surface--selected-chat-surface))
              (transcript (car surface))
              (window (cdr surface)))
    (setq e-chat-surface--output-follow-command-state
          (list :buffer transcript
                :window window
                :window-start (window-start window)
                :window-point (window-point window)))))

(defun e-chat-surface--update-output-follow-after-command ()
  "Update paired transcript follow intent after a viewport-moving command."
  (let ((state e-chat-surface--output-follow-command-state))
    (setq e-chat-surface--output-follow-command-state nil)
    (when-let* ((state state)
              (transcript (plist-get state :buffer))
              ((buffer-live-p transcript))
              (window (plist-get state :window))
              ((window-live-p window))
              ((eq (window-buffer window) transcript)))
      (with-current-buffer transcript
        (let ((old-start (plist-get state :window-start))
              (old-point (plist-get state :window-point))
              (start (window-start window))
              (point (window-point window)))
          (cond
           ;; Any movement toward older output is deliberate scrollback, even
           ;; when a tall viewport still happens to contain the live tail.
           ((or (< start old-start)
                (and (= start old-start) (< point old-point)))
            (e-chat-surface--set-window-output-follow window nil))
           ;; Movement toward newer output repins only once the viewport reaches
           ;; the tail.  Commands which do not move the viewport preserve intent.
           ((or (> start old-start) (> point old-point))
            (e-chat-surface--set-window-output-follow
             window
             (e-chat-surface--window-reaches-output-p
              window (e-chat-surface--output-follow-position))))))))))

(defun e-chat-surface--follow-output-window (window position)
  "Place POSITION near the bottom of transcript WINDOW without selecting it."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (e-chat-surface--clear-output-bottom-spacer window)
    ;; Give `vertical-motion' a fresh origin when POSITION lies beyond the old
    ;; viewport.  Without this provisional start it can reuse the stale display
    ;; matrix and incorrectly report that a long transcript begins at point-min.
    (set-window-start window position t)
    (let* ((target-motion (- 2 (window-body-height window)))
           (motion-and-start
            (save-excursion
              (goto-char position)
              (let ((motion (vertical-motion target-motion window)))
                (cons motion (point)))))
           (motion (car motion-and-start))
           (start (cdr motion-and-start))
           (short-p (> motion target-motion))
           (spacer-lines
            (if short-p
                (max 0
                     (- (abs (- 4 (window-body-height window)))
                        (abs motion)))
              0)))
      (e-chat-surface--set-output-bottom-spacer window spacer-lines)
      (if short-p
          (progn
            (set-window-point window position)
            (set-window-start window (point-min)))
        (progn
          (set-window-point window position)
          (set-window-start window start t)))
      (e-chat-surface--set-window-output-follow window t))))

(defun e-chat-surface--capture-output-tail-windows ()
  "Return visible transcript windows physically positioned at the output tail."
  (let ((tail (e-chat-surface--output-follow-position)))
    (cl-remove-if-not
     (lambda (window)
       (and (e-chat-surface--window-reaches-output-p window tail)
            (>= (window-point window) tail)))
     (e-chat-surface--transcript-windows))))

(defun e-chat-surface--capture-live-output-follow-windows ()
  "Return transcript windows following the current live output boundary.
Unlike full projection replacement, an incremental terminal event must retain
the explicit window-local follow decision across transient status removal."
  (cl-remove-if-not #'e-chat-surface--window-follows-output-p
                    (e-chat-surface--transcript-windows)))

(defun e-chat-surface--restore-output-tail-windows (windows)
  "Move still-live transcript WINDOWS to the current output tail."
  (let ((tail (e-chat-surface--output-follow-position)))
    (dolist (window windows)
      (e-chat-surface--follow-output-window window tail))))

(defun e-chat-surface--position-running-offset (position bounds)
  "Return POSITION's offset inside BOUNDS, or nil."
  (when (and position
             bounds
             (<= (car bounds) position)
             (<= position (cdr bounds)))
    (- position (car bounds))))

(defun e-chat-surface--capture-running-status-display-state ()
  "Capture each transcript viewport before an active-status redraw.
Windows already showing the old output tail follow the new tail.  Every other
window retains its scroll position, including when the composer is focused."
  (when-let ((bounds (e-chat-surface--running-status-bounds)))
    (let ((point-offset (e-chat-surface--position-running-offset (point) bounds)))
      (list
       :point-offset point-offset
       :windows
       (mapcar
        (lambda (window)
          (let ((follow-output (e-chat-surface--window-follows-output-p window)))
            (list :window window
                  :follow-output follow-output
                  :window-point-offset
                  (e-chat-surface--position-running-offset (window-point window) bounds)
                  :window-start-offset
                  (e-chat-surface--position-running-offset (window-start window) bounds))))
        (e-chat-surface--transcript-windows))))))

(defun e-chat-surface--running-status-position-from-offset (offset bounds)
  "Return a position inside BOUNDS for OFFSET."
  (when (and offset bounds)
    (+ (car bounds)
       (min offset
            (max 0 (- (cdr bounds) (car bounds)))))))

(defun e-chat-surface--restore-running-status-display-state (state)
  "Restore transcript viewports captured by STATE after an active-status redraw."
  (when state
    (when-let ((bounds (e-chat-surface--running-status-bounds)))
      (let ((tail (e-chat-surface--output-follow-position))
            (point-position
             (e-chat-surface--running-status-position-from-offset
              (plist-get state :point-offset)
              bounds)))
        (when point-position
          (goto-char point-position))
        (dolist (entry (plist-get state :windows))
          (let ((window (plist-get entry :window)))
            (when (window-live-p window)
              (if (plist-get entry :follow-output)
                  (e-chat-surface--follow-output-window window tail)
                (let ((window-point-position
                       (e-chat-surface--running-status-position-from-offset
                        (plist-get entry :window-point-offset)
                        bounds))
                      (window-start-position
                       (e-chat-surface--running-status-position-from-offset
                        (plist-get entry :window-start-offset)
                        bounds)))
                  (when window-start-position
                    (set-window-start window window-start-position t))
                  (when window-point-position
                    (set-window-point window window-point-position)))))))))))
(defun e-chat-surface--refresh-composer-position ()
  "Fit the current visible chat surface's composer window."
  (let ((transcript (e-chat-surface--surface-transcript-buffer)))
    (when (buffer-live-p transcript)
      (with-current-buffer transcript
        (when-let ((transcript-window (get-buffer-window transcript t)))
          (e-chat-surface--surface-fit-composer-window
           (e-chat-surface--surface-composer-window transcript-window)))))))

(defun e-chat-surface--refresh-visible-composers ()
  "Refit composer windows for visible e chat buffers."
  (unless e-chat-surface--refresh-visible-composers-in-progress
    (let ((e-chat-surface--refresh-visible-composers-in-progress t)
          (seen nil))
      (dolist (window (window-list nil 'no-minibuf))
        (let ((buffer (window-buffer window)))
          (when (and (buffer-live-p buffer)
                     (not (memq buffer seen)))
            (push buffer seen)
            (with-current-buffer buffer
              (when (e-chat-surface--chat-buffer-p)
                (dolist (transcript-window
                         (get-buffer-window-list buffer nil t))
                  (e-chat-surface--surface-fit-composer-window
                   (e-chat-surface--surface-composer-window transcript-window)))))))))))

(defun e-chat-surface--ensure-window-refresh-hook ()
  "Ensure visible chat composers refresh when frame windows change."
  (add-hook 'window-configuration-change-hook
            #'e-chat-surface--refresh-visible-composers))

(defun e-chat-surface--show-composer ()
  "Move point and visible window focus to the composer."
  (if (e-chat-surface--surface-transcript-p)
      (when-let* ((composer e-chat-surface--surface-composer-buffer)
                  (window (e-chat-surface--surface-display-composer
                           nil t composer)))
        (with-current-buffer composer
          (goto-char (point-max)))
        (set-window-point window (with-current-buffer composer (point))))
    (goto-char (point-max))
    (when-let ((window (get-buffer-window (current-buffer) t)))
      (set-window-point window (point))
      (unless e-chat-surface--recenter-inhibited
        (with-selected-window window
          (ignore-errors
            (recenter -2)))))))

(defun e-chat-surface--show-latest-output (&optional window)
  "Show the latest chat output in WINDOW while preserving composer focus.
WINDOW defaults to an arbitrary visible window for the current transcript."
  (let* ((transcript (e-chat-surface--surface-transcript-buffer))
         (position (with-current-buffer transcript
                     (e-chat-surface--output-follow-position)))
         (window (or window (get-buffer-window transcript t))))
    (when (and (buffer-live-p transcript) (window-live-p window))
      (with-current-buffer transcript
        (goto-char position)
        (e-chat-surface--follow-output-window window position)))))

(defun e-chat-surface--enter-composer-input-state ()
  "Focus the already-bound editable composer input."
  (e-chat-surface--show-composer))

(defun e-chat-surface--after-display-buffer (buffer)
  "Restore chat-local editing invariants after displaying BUFFER."
  (let ((transcript-window (get-buffer-window buffer t)))
    (with-current-buffer buffer
      (when (e-chat-surface--surface-transcript-p)
        (e-chat-surface--surface-display-composer))
      (e-chat-surface--enter-composer-input-state))
    (when (window-live-p transcript-window)
      (e-chat-surface--activate-surface (cons buffer transcript-window))))
  buffer)

(defun e-chat-surface--side-window-p (&optional window)
  "Return non-nil when WINDOW (or the selected window) is a side window."
  (window-parameter (or window (selected-window)) 'window-side))

(defun e-chat-surface--non-side-window (&optional frame)
  "Return a live non-side window on FRAME, or nil when every window is a side."
  (cl-find-if-not (lambda (window) (window-parameter window 'window-side))
                  (window-list frame 'no-minibuf)))

(defun e-chat-surface--display-in-new-root-window (buffer)
  "Show BUFFER in a fresh normal window split from the frame root, and return it.
Used when every window on the frame is a side window: side windows (and their
parents) cannot be split, but the frame root can, which yields an ordinary
non-side window able to host BUFFER."
  (let ((window (split-window (frame-root-window) nil 'below)))
    (set-window-buffer window buffer)
    window))

(defun e-chat-surface--display-from-side-window (buffer)
  "Display BUFFER when the selected window is a side window, and return it.
A side window cannot host an ordinary buffer and cannot be split (nor can its
parent), so splitting-based display actions signal \"Cannot split side window
or parent of side window\".  Prefer an existing non-side window: reuse one that
already shows BUFFER, else reuse/split a normal window.  When the frame has no
normal window at all (every window is a managed side popup, e.g. an aggressive
display-buffer-alist or popup manager), split the frame root to create one
rather than commandeering a side window -- the latter would leave the frame
with no main window and break ordinary commands like \\[split-window-right]."
  (if (e-chat-surface--non-side-window)
      (display-buffer
       buffer
       '((display-buffer-reuse-window
          display-buffer-use-some-window
          display-buffer-pop-up-window)
         (inhibit-same-window . t)
         (some-window . mru)))
    (e-chat-surface--display-in-new-root-window buffer)))

(defun e-chat-surface--switch-to-buffer (buffer)
  "Display BUFFER, restoring chat-local editing invariants.
When the selected window is a side window it cannot show BUFFER, so route the
display to a normal window and select it instead of erroring."
  (if (e-chat-surface--side-window-p)
      (when-let ((window (e-chat-surface--display-from-side-window buffer)))
        (select-window window))
    (e-workspace-switch-to-buffer
     buffer
     :workspace (or (e-buffer-workspace buffer)
                    (e-workspace-current))))
  (e-chat-surface--after-display-buffer buffer))

(defun e-chat-surface--pop-to-buffer (buffer &optional workspace action)
  "Pop to BUFFER and restore chat-local editing invariants.
WORKSPACE, when non-nil, overrides BUFFER's workspace affinity for this display.
ACTION, when non-nil, is passed to `e-workspace-display-buffer'.
From a side window, `pop-to-buffer' would try to split the side window and
signal; route the display to a normal window in that case."
  (cond
   ((and (e-chat-surface--side-window-p)
         (not workspace)
         (not action))
    (when-let ((window (e-chat-surface--display-from-side-window buffer)))
      (select-window window)))
   ((or workspace action)
    (when-let ((window (e-workspace-display-buffer
                       buffer
                       :workspace (or workspace
                                      (e-buffer-workspace buffer)
                                      (e-workspace-current))
                       :action action
                       :select t)))
      (select-window window)))
   (t
    (e-workspace-pop-to-buffer
     buffer
     :workspace (or (e-buffer-workspace buffer)
                    (e-workspace-current)))))
  (e-chat-surface--after-display-buffer buffer))



(defun e-chat-surface--surface-transcript-p ()
  "Return non-nil when the current buffer is a transcript surface."
  (or (eq e-chat-surface--surface-role 'transcript)
      (buffer-live-p e-chat-surface--surface-composer-buffer)))

(defun e-chat-surface--surface-composer-p ()
  "Return non-nil when the current buffer is a composed chat input pane."
  ;; A standalone input (for example an Org Canvas prompt) uses the composer
  ;; major mode but is not a paired chat surface.  Only the explicit pairing
  ;; makes it a surface composer; its role marker alone must not redirect
  ;; status mutation to a missing transcript.
  (buffer-live-p e-chat-surface--surface-transcript-buffer))

(defun e-chat-surface--surface-transcript-buffer ()
  "Return the transcript buffer for the current chat surface."
  (if (e-chat-surface--surface-composer-p)
      e-chat-surface--surface-transcript-buffer
    (current-buffer)))

(defun e-chat-surface--surface-composer-killed ()
  "Clear this input pane from its transcript surface when it is killed."
  (let ((composer (current-buffer)))
    (when-let ((transcript e-chat-surface--surface-transcript-buffer))
      (when (buffer-live-p transcript)
        (with-current-buffer transcript
          (when (eq e-chat-surface--surface-composer-buffer composer)
            (setq e-chat-surface--surface-composer-buffer nil)))))))

(defun e-chat-surface--surface-kill-composer ()
  "Kill the composer buffer paired with the current transcript buffer."
  ;; Release both constituents before either buffer dies.  A soft-dedicated
  ;; atomic constituent is deleted when its buffer dies; deleting one also
  ;; deletes its sibling while `kill-buffer' may still be traversing another
  ;; visible transcript view.
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (set-window-dedicated-p window nil)
    (set-window-parameter window 'window-atom nil))
  (when-let ((composer e-chat-surface--surface-composer-buffer))
    (when (buffer-live-p composer)
      ;; The surface is already being torn down.  Releasing its temporary
      ;; dedication lets Emacs replace both buffers without mutating the window
      ;; tree out from under that traversal.
      (dolist (window (get-buffer-window-list composer nil t))
        (set-window-dedicated-p window nil)
        (set-window-parameter window 'window-atom nil))
      (kill-buffer composer)))
  (setq e-chat-surface--surface-composer-buffer nil))

;;; Public surface contract

(defun e-chat-surface-transcript-buffer (&optional buffer)
  "Return the transcript buffer owning BUFFER's chat surface."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (or (e-chat-surface--surface-transcript-p)
                  (e-chat-surface--surface-composer-p)
                  (and (boundp 'e-chat-harness)
                       (boundp 'e-chat-session-id)
                       e-chat-harness
                       e-chat-session-id))
          (e-chat-surface--surface-transcript-buffer))))))

(defun e-chat-surface-composer-buffer (&optional buffer)
  "Return the composer buffer paired with BUFFER, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (and (boundp 'e-chat-surface--surface-composer-buffer)
         (buffer-live-p e-chat-surface--surface-composer-buffer)
         e-chat-surface--surface-composer-buffer)))

(defun e-chat-surface-after-display-buffer (buffer)
  "Restore chat presentation invariants after displaying BUFFER."
  (e-chat-surface--after-display-buffer buffer))

(defun e-chat-surface-side-window-p (&optional window)
  "Return non-nil when WINDOW is a managed side window."
  (e-chat-surface--side-window-p window))

(defun e-chat-surface-display-from-side-window (buffer)
  "Display BUFFER from a selected side window in a normal window."
  (e-chat-surface--display-from-side-window buffer))

(defun e-chat-surface-switch-to-buffer (buffer)
  "Display and select BUFFER while preserving chat surface invariants."
  (e-chat-surface--switch-to-buffer buffer))

(defun e-chat-surface-pop-to-buffer (buffer &optional workspace action)
  "Pop to BUFFER while preserving chat surface invariants."
  (e-chat-surface--pop-to-buffer buffer workspace action))

(defun e-chat-surface-enter-composer-input-state ()
  "Leave navigation states and focus the editable composer in this surface."
  (e-chat-surface--enter-composer-input-state))

(defun e-chat-surface-initialize ()
  "Install the chat surface focus and window hooks."
  (setq-local e-chat-surface--surface-command-map-active t)
  (e-chat-surface--ensure-window-selection-hook)
  (e-chat-surface--ensure-window-refresh-hook))

;; The facade and embedding shells use these owner-shaped operations instead of
;; reaching through the surface's buffer-local state.  Keep the wrappers
;; grouped here so the surface remains the only owner of window membership,
;; viewport restoration, and composer pairing.

(defun e-chat-surface-transcript-p (&optional buffer)
  "Return non-nil when BUFFER is a chat transcript surface."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--surface-transcript-p)))

(defun e-chat-surface-composer-p (&optional buffer)
  "Return non-nil when BUFFER is a paired chat composer surface."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--surface-composer-p)))

(defun e-chat-surface-selected-chat-surface (&optional window)
  "Return the selected chat surface for WINDOW as (TRANSCRIPT . WINDOW)."
  (e-chat-surface--selected-chat-surface window))

(defun e-chat-surface-kill-composer (&optional buffer)
  "Kill the composer paired with BUFFER's transcript."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--surface-kill-composer)))

(defun e-chat-surface-refresh-composer-position (&optional buffer)
  "Refit and position BUFFER's paired composer."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--refresh-composer-position)))

(defun e-chat-surface-refresh-visible-windows (&optional buffer)
  "Refresh visible windows belonging to BUFFER's chat surface."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--surface-refresh-visible-windows)))

(defun e-chat-surface-composer-window (&optional transcript-window buffer)
  "Return the composer window paired with BUFFER's transcript window."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (e-chat-surface--surface-composer-window transcript-window))))

(defun e-chat-surface-display-composer
    (&optional transcript-window select composer buffer)
  "Display COMPOSER below BUFFER's TRANSCRIPT-WINDOW.
When SELECT is non-nil, select the paired composer window.  This is the
surface-level composition operation used by embedding shells; composer
creation itself remains owned by `e-chat-composer'."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--surface-display-composer
     transcript-window select composer)))

(defun e-chat-surface-show-latest-output (&optional window buffer)
  "Show the latest transcript output in WINDOW for BUFFER's surface.
This is the narrow viewport operation used by embedding shells and tests; the
surface owner retains the output-follow markers and window policy."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--show-latest-output window)))

(defun e-chat-surface-capture-output-tail-windows (&optional buffer)
  "Capture output-follow state for BUFFER's visible transcript windows."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--capture-output-tail-windows)))

(defun e-chat-surface-capture-live-output-follow-windows (&optional buffer)
  "Capture live output-follow state for BUFFER's chat surface."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--capture-live-output-follow-windows)))

(defun e-chat-surface-restore-output-tail-windows (windows &optional buffer)
  "Restore captured output-follow WINDOWS for BUFFER's chat surface."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--restore-output-tail-windows windows)))

(defun e-chat-surface-capture-running-status-display-state (&optional buffer)
  "Capture running-status viewport state for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--capture-running-status-display-state)))

(defun e-chat-surface-restore-running-status-display-state
    (state &optional buffer)
  "Restore running-status viewport STATE for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--restore-running-status-display-state state)))

(defun e-chat-surface-output-follow-position (&optional buffer)
  "Return the output-follow position for BUFFER's current transcript."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--output-follow-position)))

(defun e-chat-surface-set-window-output-follow (window follow)
  "Set WINDOW's output-follow intent to FOLLOW."
  (with-current-buffer (window-buffer window)
    (e-chat-surface--set-window-output-follow window follow)))

(defun e-chat-surface-window-reaches-output-p (window tail)
  "Return non-nil when WINDOW reaches transcript TAIL."
  (with-current-buffer (window-buffer window)
    (e-chat-surface--window-reaches-output-p window tail)))

(defun e-chat-surface-window-follows-output-p (window)
  "Return non-nil when WINDOW follows its transcript output tail."
  (with-current-buffer (window-buffer window)
    (e-chat-surface--window-follows-output-p window)))

(defun e-chat-surface-activate (surface)
  "Activate semantic chat SURFACE after its window selection settles."
  (e-chat-surface--activate-surface surface))

(defun e-chat-surface-activate-selected (&optional frame)
  "Activate the selected chat surface on FRAME, if one is selected."
  (e-chat-surface--activate-selected-surface-on-selection frame))

(defun e-chat-surface-activate-after-window-change (frame)
  "Restore the selected chat surface after FRAME's window buffers change."
  (e-chat-surface--activate-selected-surface-after-window-buffer-change frame))

(defun e-chat-surface-non-side-window (&optional frame)
  "Return a normal window on FRAME, or nil when none is available."
  (e-chat-surface--non-side-window frame))

(defun e-chat-surface-without-recenter (thunk &optional buffer)
  "Call THUNK with automatic chat recentering disabled for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((e-chat-surface--recenter-inhibited t))
      (funcall thunk))))

(defun e-chat-surface--setup-line-wrapping ()
  "Apply configured chat wrapping to the current surface buffer."
  (when e-chat-visual-fill-column
    (visual-line-mode 1)
    (if (require 'visual-fill-column nil t)
        (progn
          (setq-local visual-fill-column-width e-chat-visual-fill-column)
          (setq-local visual-fill-column-center-text nil)
          (visual-fill-column-mode 1))
      (message "e-chat: visual-fill-column unavailable; wrapping at window edge"))))

(defun e-chat-surface-show-composer (&optional buffer)
  "Select BUFFER's paired composer when its surface is visible."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--show-composer)))

(defun e-chat-surface-setup-line-wrapping (&optional buffer)
  "Apply configured chat wrapping to BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--setup-line-wrapping)))

(defun e-chat-surface-pre-command (&optional buffer)
  "Capture surface output-follow state before a command in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--capture-selected-output-follow-command)))

(defun e-chat-surface-post-command (&optional buffer)
  "Restore surface output-follow state after a command in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--update-output-follow-after-command)))

(defun e-chat-surface-capture-selected-output-follow-command (&optional buffer)
  "Capture output-follow state before a command in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--capture-selected-output-follow-command)))

(defun e-chat-surface-mark-composer-layout-dirty (begin end length &optional buffer)
  "Mark BUFFER's paired layout dirty after a composer change."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--surface-mark-composer-layout-dirty begin end length)))

(defun e-chat-surface-refresh-composer-layout (&optional buffer)
  "Refit a paired composer after a command in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-surface--surface-composer-post-command)))

(defun e-chat-surface-unbind-composer (composer &optional transcript)
  "Forget COMPOSER's pairing with TRANSCRIPT without killing either buffer."
  (let ((transcript (or transcript
                       (and (buffer-live-p composer)
                            (buffer-local-value
                             'e-chat-surface--surface-transcript-buffer
                             composer)))))
    (when (buffer-live-p transcript)
      (with-current-buffer transcript
        (when (eq e-chat-surface--surface-composer-buffer composer)
          (setq e-chat-surface--surface-composer-buffer nil))))
    (when (buffer-live-p composer)
      (with-current-buffer composer
        (setq e-chat-surface--surface-transcript-buffer nil
              e-chat-surface--surface-role nil)))
    nil))

(defun e-chat-surface-bind-composer (composer transcript)
  "Pair COMPOSER with TRANSCRIPT as one chat surface."
  (with-current-buffer transcript
    (e-chat-surface--surface-bind-composer composer transcript)
    (setq-local e-chat-surface--surface-composer-buffer composer)
    (with-current-buffer composer
      (setq-local e-chat-surface--surface-role 'composer))
    composer))

(defun e-chat-surface-mark-transcript (&optional buffer)
  "Mark BUFFER as a transcript constituent of a chat surface."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local e-chat-surface--surface-role 'transcript)
    (current-buffer)))

(defun e-chat-surface-mark-composer (&optional buffer)
  "Mark BUFFER as a composer constituent of a chat surface."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local e-chat-surface--surface-role 'composer)
    (current-buffer)))

(provide 'e-chat-surface)

;;; e-chat-surface.el ends here
