;;; e-chat-transcript.el --- Chat transcript owner -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns durable entry projection, structured block metadata, Markdown/Org
;; presentation, transcript replay, response/block navigation, and tool detail
;; views.  It is consumed by the e-chat facade and the activity owner.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pp)
(require 'subr-x)
(require 'e-chat-service)
(require 'e-chat-output-mode)
(require 'e-chat-surface)
(require 'e-message-details)
(require 'e-structured-blocks)
(require 'e-tools)
(require 'e-work)
(require 'e-workspaces)

(defcustom e-chat-session-summary-preview-max-chars 512
  "Maximum session-summary characters rendered before transcript replay.
Persistent indexes retain the complete first user message as their summary.
That message can be a very large generated bootstrap prompt, so metadata-only
loading and picker previews must never project it without a display bound."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-session-replay-message-limit 40
  "Maximum recent transcript messages reconstructed in a chat buffer.
Older durable messages remain available to the harness but are omitted from
the presentation replay.  Unloaded indexed sessions still use the asynchronous
session loader before this bounded view is rendered."
  :type '(integer 1)
  :group 'e-chat)

(defcustom e-chat-details-buffer-name "*e-chat-details*"
  "Buffer name for read-only focused block details."
  :type 'string
  :group 'e-chat)

(defcustom e-chat-tool-output-buffer-name "*e-chat-tool-output*"
  "Buffer name for read-only focused tool output."
  :type 'string
  :group 'e-chat)

(defcustom e-chat-deferred-markdown-threshold-bytes 8192
  "Assistant message size above which Markdown presentation is deferred.
The raw assistant text is inserted immediately; only Markdown faces and syntax
concealment are scheduled for a later timer tick."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-deferred-markdown-chunk-lines 80
  "Maximum number of lines processed by one deferred Markdown render job."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-running-status-diff-max-chars 20000
  "Region size above which running-activity updates use a bounded diff."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-running-status-diff-max-seconds 0.05
  "Time cap for a large running-activity region replacement."
  :type 'number
  :group 'e-chat)

(declare-function markdown-mode "markdown-mode")
(declare-function e-ui-work-cancel-matching "e-ui-work")
(declare-function e-ui-work-schedule "e-ui-work")
(declare-function e-ui-work-spec-create "e-ui-work")
(declare-function e-chat-surface-transcript-buffer "e-chat-surface")
(declare-function e-chat-surface-composer-buffer "e-chat-surface")
(declare-function e-chat-surface-transcript-p "e-chat-surface")
(declare-function e-chat-surface-enter-composer-input-state "e-chat-surface")
(declare-function e-chat-surface-refresh-ui-work-diagnostics "e-chat-surface")

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")
;; Public chat-buffer fields and presentation policy declared by the facade.
;; Owner-specific transcript presentation constants and registries are
;; initialized here; the facade composes their public ports.
(defvar e-chat-harness nil)
(defvar e-chat-session-id nil)
(defconst e-chat-transcript--system-glyph "·"
  "Glyph shown before compact system chat blocks.")
(defconst e-chat-transcript--user-glyph ">"
  "Glyph shown before user-authored chat blocks.")
(defconst e-chat-transcript--assistant-glyph "●"
  "Glyph shown before assistant chat blocks.")
(defconst e-chat-transcript--hidden-glyph "⋯"
  "Glyph shown before a revealed hidden audit chat block.")
(defconst e-chat-transcript--hidden-entry-title-prefix "Hidden"
  "Title prefix marking a revealed hidden message's rendered block.
Any entry title with this prefix renders as a dimmed audit block and maps to
the `hidden' block kind.")
(defconst e-chat-transcript--protected-properties
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
(defconst e-chat-activity-separator
  (make-string 64 ?┈)
  "Subtle separator shown between intermittent activity rounds.")
(defconst e-chat-transcript--turn-separator
  "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  "Separator shown between rendered e chat turns.")
(defconst e-chat-transcript--response-separator
  "────────────────────────────────────────────────────────────────"
  "Separator shown between prompt and agent-side blocks in a turn.")
(defvar e-chat-session-replay-activity-event-limit)

(defun e-chat-transcript--profile-enabled-p ()
  "Return non-nil when developer profiling is currently available."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-chat-transcript--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when profiling is enabled."
  (if (e-chat-transcript--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-chat-transcript--board-shaped-p (value)
  "Return non-nil when VALUE carries a board projection identity."
  (or (plist-member value :board-id)
      (plist-member value :board-seq)
      (plist-member value :message-id)
      (plist-member value :subject-participant-id)))

(defun e-chat-transcript--event-selected-participant-p (event)
  "Return whether projected EVENT belongs to this chat's participant."
  (if (plist-member event :selected-participant-p)
      (eq (plist-get event :selected-participant-p) t)
    (not (e-chat-transcript--board-shaped-p event))))

(defun e-chat-transcript--message-selected-participant-p (message)
  "Return whether projected MESSAGE belongs to this chat's participant."
  (if (plist-member message :selected-participant-p)
      (eq (plist-get message :selected-participant-p) t)
    (not (e-chat-transcript--board-shaped-p message))))

(defun e-chat-transcript--observed-turn-id (turn-id event)
  "Return an isolated presentation id for unselected EVENT."
  (let ((board-id (plist-get event :board-id))
        (subject-participant-id (plist-get event :subject-participant-id))
        (source-turn-id (plist-get event :source-turn-id)))
    (if (and board-id subject-participant-id)
        (list :observed-board-turn
              :board-id (copy-tree board-id)
              :subject-participant-id (copy-tree subject-participant-id)
              :source-turn-id (copy-tree source-turn-id)
              :causal-turn-id (copy-tree turn-id))
      (format "%s:observed:%s"
              turn-id
              (or (plist-get event :message-id)
                  (plist-get event :board-seq)
                  (plist-get event :id)
                  (plist-get event :event-type)
                  (sxhash-equal event))))))

(defun e-chat-transcript--presentation-turn-id (turn-id event)
  "Return EVENT's selected or isolated presentation turn id."
  (if (e-chat-transcript--event-selected-participant-p event)
      turn-id
    (e-chat-transcript--observed-turn-id turn-id event)))

;; Navigation and tool views are transcript-owned modes.  Their keymaps live
;; here too; the facade only asks this owner to refresh them during startup.
(defun e-chat-transcript--make-response-navigation-mode-map (&optional map)
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
    (define-key map (kbd "h") #'e-chat-response-navigation-toggle-hidden)
    map))

(defun e-chat-transcript--make-block-view-mode-map (&optional map)
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

(defun e-chat-transcript--make-tool-list-mode-map (&optional map)
  "Return MAP configured for `e-chat-tool-list-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (define-key map (kbd "j") #'e-chat-tool-list-next)
    (define-key map (kbd "k") #'e-chat-tool-list-previous)
    (define-key map (kbd "RET") #'e-chat-tool-list-open-output)
    (define-key map (kbd "<escape>") #'e-chat-tool-list-back)
    map))

(defun e-chat-transcript--make-tool-output-mode-map (&optional map)
  "Return MAP configured for `e-chat-tool-output-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "<escape>") #'e-chat-tool-output-back)
    map))

(defvar e-chat-response-navigation-mode-map
  (e-chat-transcript--make-response-navigation-mode-map)
  "Keymap for response navigation inside `e-chat-mode'.")
(defvar e-chat-block-view-mode-map
  (e-chat-transcript--make-block-view-mode-map)
  "Keymap for block-local view mode inside `e-chat-mode'.")
(defvar e-chat-tool-list-mode-map
  (e-chat-transcript--make-tool-list-mode-map)
  "Keymap for activity tool-list mode inside `e-chat-mode'.")
(defvar e-chat-tool-output-mode-map
  (e-chat-transcript--make-tool-output-mode-map)
  "Keymap for read-only tool output buffers.")

(declare-function e-chat-surface-capture-output-tail-windows "e-chat-surface")
(declare-function e-chat-surface-restore-output-tail-windows "e-chat-surface")
(declare-function e-chat-surface-capture-running-status-display-state "e-chat-surface")
(declare-function e-chat-surface-restore-running-status-display-state "e-chat-surface")
(declare-function e-chat-surface-set-running-status-bounds "e-chat-surface")

(defvar-local e-chat-transcript--preview-buffer nil
  "Non-nil when this buffer is a transient chat preview, not a session shell.")

(defvar-local e-chat-transcript--turn-registry nil
  "Hash table of rendered turn metadata keyed by turn id.")

(defvar-local e-chat-transcript--block-registry nil
  "Hash table of rendered block metadata keyed by block id.")

(defvar-local e-chat-transcript--message-block-index nil
  "Hash table mapping durable message ids to rendered block ids.")

(defvar-local e-chat-transcript--block-order nil
  "Rendered block ids in transcript order.")

(defvar-local e-chat-transcript--focused-turn-id nil
  "Turn id belonging to the currently focused response-navigation block.")

(defvar-local e-chat-transcript--focused-block-id nil
  "Block id currently focused by response navigation.")

(defvar-local e-chat-transcript--block-counter 0
  "Counter used to assign display-local block ids.")

(defvar-local e-chat-transcript--focused-turn-overlay nil
  "Overlay highlighting the focused response-navigation turn.")

(defvar-local e-chat-transcript--latest-final-block-id nil
  "Most recent final assistant block id in this chat buffer.")

(defvar-local e-chat-transcript--last-rendered-turn-id nil
  "Most recent turn id that rendered a durable transcript block.")

(defvar-local e-chat-transcript--last-rendered-side nil
  "Side of the most recent durable transcript block.")

(defvar-local e-chat-transcript--block-view-block-id nil
  "Block id currently active in block view mode.")

(defvar-local e-chat-transcript--tool-list-block-id nil
  "Activity block id currently showing a tool list.")

(defvar-local e-chat-transcript--tool-list-index 0
  "Selected tool item index in the focused activity tool list.")

(defvar-local e-chat-transcript--tool-list-overlay nil
  "Overlay highlighting the selected activity tool list item.")

(defvar-local e-chat-transcript--tool-output-origin-buffer nil
  "Chat buffer that opened the current tool output buffer.")

(defvar-local e-chat-transcript--markdown-presentation-generation 0
  "Generation token for deferred assistant Markdown presentation callbacks.")

(defvar-local e-chat-transcript--reveal-hidden nil
  "When non-nil, render messages hidden from the clean transcript.")

(defvar-local e-chat-transcript--activity-block-id nil
  "Display-local block id for the current transient activity projection.")

(defvar-local e-chat-transcript--activity-turn-id nil
  "Turn id represented by the current transient activity projection.")

(defvar-local e-chat-transcript--activity-start-marker nil
  "Marker at the start of the current transient activity projection.")

(defvar-local e-chat-transcript--activity-end-marker nil
  "Marker at the end of the current transient activity projection.")

(defvar-local e-chat-transcript--activity-progress-start-marker nil
  "Marker at the start of the mutable progress tail, if present.")

(defvar-local e-chat-transcript--activity-progress-end-marker nil
  "Marker at the end of the mutable progress tail, if present.")

(define-minor-mode e-chat-response-navigation-mode
  "Navigate rendered turn blocks in an e chat buffer."
  :lighter " Nav"
  :keymap e-chat-response-navigation-mode-map
  (unless e-chat-response-navigation-mode
    (setq e-chat-transcript--focused-turn-id nil)
    (setq e-chat-transcript--focused-block-id nil)
    (when (overlayp e-chat-transcript--focused-turn-overlay)
      (delete-overlay e-chat-transcript--focused-turn-overlay))))

(define-minor-mode e-chat-block-view-mode
  "Move within the focused e chat block."
  :lighter " View"
  :keymap e-chat-block-view-mode-map
  (unless e-chat-block-view-mode
    (setq e-chat-transcript--block-view-block-id nil)))

(define-minor-mode e-chat-tool-list-mode
  "Navigate tool calls for a focused e chat activity block."
  :lighter " Tools"
  :keymap e-chat-tool-list-mode-map
  (unless e-chat-tool-list-mode
    (setq e-chat-transcript--tool-list-block-id nil)
    (setq e-chat-transcript--tool-list-index 0)
    (when (overlayp e-chat-transcript--tool-list-overlay)
      (delete-overlay e-chat-transcript--tool-list-overlay))))

(define-derived-mode e-chat-tool-output-mode special-mode "e-chat-tool-output"
  "Major mode for read-only e chat tool output buffers.")

(defun e-chat-transcript--ensure-turn-registry ()
  "Ensure turn navigation state exists for the current chat buffer."
  (unless (hash-table-p e-chat-transcript--turn-registry)
    (setq e-chat-transcript--turn-registry (make-hash-table :test 'equal)))
  e-chat-transcript--turn-registry)

(defun e-chat-transcript--ensure-block-registry ()
  "Ensure block navigation state exists for the current chat buffer."
  (unless (hash-table-p e-chat-transcript--block-registry)
    (setq e-chat-transcript--block-registry (make-hash-table :test 'equal)))
  e-chat-transcript--block-registry)

(defun e-chat-transcript--ensure-message-block-index ()
  "Ensure durable-message projection state exists for the current chat buffer."
  (unless (hash-table-p e-chat-transcript--message-block-index)
    (setq e-chat-transcript--message-block-index (make-hash-table :test 'equal)))
  e-chat-transcript--message-block-index)

(defun e-chat-transcript--message-block-id (message-id)
  "Return the rendered block id projected for durable MESSAGE-ID, if any."
  (and message-id
       (hash-table-p e-chat-transcript--message-block-index)
       (gethash message-id e-chat-transcript--message-block-index)))

(defun e-chat-transcript--block-display-hidden-p (record)
  "Return non-nil when rendered block RECORD is hidden by message disposition."
  (plist-get record :display-hidden))

(defun e-chat-transcript--live-block-record (block-id)
  "Return live block metadata for BLOCK-ID, or nil.
Blocks whose display was hidden or whose markers no longer delimit text are
not navigable and are therefore omitted from the live projection."
  (when-let ((record (and block-id
                          (hash-table-p e-chat-transcript--block-registry)
                          (gethash block-id e-chat-transcript--block-registry))))
    (let* ((start-marker (plist-get record :start-marker))
           (end-marker (plist-get record :end-marker))
           (start (and (markerp start-marker)
                       (marker-position start-marker)))
           (end (and (markerp end-marker)
                     (marker-position end-marker))))
      (when (and start end (< start end)
                 (not (e-chat-transcript--block-display-hidden-p record)))
        record))))

(defun e-chat-transcript--associate-message-block (message-id block-id)
  "Associate durable MESSAGE-ID with presentation BLOCK-ID.
The relation is one-to-one inside a chat buffer."
  (when (and message-id block-id)
    (puthash message-id block-id (e-chat-transcript--ensure-message-block-index))
    (when-let ((record (gethash block-id (e-chat-transcript--ensure-block-registry))))
      (plist-put record :message-id message-id))))

(defun e-chat-transcript--session-summary-preview (session)
  "Return SESSION summary bounded for metadata-only presentation."
  (when-let ((summary (plist-get session :summary)))
    (let ((limit e-chat-session-summary-preview-max-chars))
      (unless (and (integerp limit) (> limit 0))
        (user-error
         "e-chat-session-summary-preview-max-chars must be a positive integer"))
      (if (> (length summary) limit)
          (concat (substring summary 0 limit) "…")
        summary))))

(defun e-chat-transcript--validated-replay-limit (value option)
  "Return positive integer VALUE or report invalid replay OPTION."
  (unless (and (integerp value) (> value 0))
    (user-error "%s must be a positive integer" option))
  value)

(defun e-chat-transcript--session-replay-message-count (messages)
  "Return the bounded number of recent MESSAGES to reconstruct."
  (min (length messages)
       (e-chat-transcript--validated-replay-limit
        e-chat-session-replay-message-limit
        'e-chat-session-replay-message-limit)))

(cl-defun e-chat-transcript--render-session-replay
    (messages &optional (activity-events nil activity-events-supplied-p))
  "Render the bounded recent replay of loaded transcript MESSAGES.
The caller owns composer removal and restoration.  When ACTIVITY-EVENTS is
supplied, use the same service snapshot as MESSAGES."
  (let* ((total-count (length messages))
         (rendered-count
          (e-chat-transcript--session-replay-message-count messages))
         (omitted (max 0 (- total-count rendered-count)))
         (tail (if (> rendered-count 0)
                   (e-chat-transcript--tail-messages messages rendered-count)
                 nil)))
    (when (> omitted 0)
      (e-chat-transcript--insert-protected
       (format "%s %d earlier transcript message%s omitted from this view.\n\n"
               e-chat-transcript--system-glyph
               omitted
               (if (= omitted 1) "" "s"))
       'e-chat-activity-face))
    (when tail
      (if activity-events-supplied-p
          (e-chat-transcript--render-session tail activity-events)
        (e-chat-transcript--render-session tail)))))

(defun e-chat-transcript--rerender-transcript ()
  "Rebuild this transcript's durable projection in place.
The composition root owns activity replay and calls this owner for durable
projection changes; this local path is also used by transcript-only audit
reveal, so it deliberately knows nothing about activity or facade state."
  (when (and e-chat-harness e-chat-session-id
             (e-chat-surface-transcript-p)
             (not e-chat-transcript--preview-buffer))
    (let* ((output-tail-windows
            (e-chat-surface-capture-output-tail-windows))
           (messages
            (e-chat-service-messages e-chat-harness e-chat-session-id))
           (title nil))
      (setq title
            (save-excursion
              (goto-char (point-min))
              (buffer-substring-no-properties
               (point-min)
               (or (and (search-forward "\n\n" nil t) (point))
                   (point-min)))))
      (let ((inhibit-read-only t))
        (e-chat-transcript--cancel-pending-markdown-presentation)
        (erase-buffer)
        (e-chat-transcript-reset)
        (when (not (string-empty-p title))
          (e-chat-transcript--insert-protected title 'e-chat-title-face))
        (e-chat-transcript--render-session-replay messages))
      (e-chat-surface-restore-output-tail-windows
       output-tail-windows))))


(defun e-chat-transcript--mark-protected (start end)
  "Mark text between START and END as protected presentation text."
  (when (< start end)
    (add-text-properties start end e-chat-transcript--protected-properties)))

(defun e-chat-transcript--insert-protected (text &optional face properties)
  "Insert TEXT as protected presentation text at point.
FACE is applied when non-nil.  PROPERTIES are added with text properties."
  (let ((start (point)))
    (insert text)
    (e-chat-transcript--mark-protected start (point))
    (when face
      (add-text-properties start (point) `(font-lock-face ,face)))
    (when properties
      (add-text-properties start (point) properties))))

(defun e-chat-transcript--apply-activity-separator-face (start end)
  "Apply the quiet activity separator face between START and END."
  (when (< start end)
    (save-excursion
      (goto-char start)
      (while (search-forward e-chat-activity-separator end t)
        (add-text-properties
         (match-beginning 0)
         (match-end 0)
         '(font-lock-face e-chat-activity-separator-face))))))

(defun e-chat-transcript--entry-side (title)
  "Return the prompt/agent side represented by entry TITLE."
  (if (equal title "You") 'user 'agent))

(defun e-chat-transcript--insert-horizontal-separator (text face)
  "Insert protected separator TEXT with FACE at point."
  (e-chat-transcript--insert-protected
   (concat text "\n")
   face
   '(e-chat-separator t)))

(defun e-chat-transcript--maybe-insert-turn-separator (turn-id)
  "Insert a stable separator before TURN-ID when crossing turns."
  (when (and turn-id
             e-chat-transcript--last-rendered-turn-id
             (not (equal turn-id e-chat-transcript--last-rendered-turn-id)))
    (e-chat-transcript--insert-horizontal-separator
     e-chat-transcript--turn-separator
     'e-chat-turn-separator-face)))

(defun e-chat-transcript--maybe-insert-response-separator (turn-id side)
  "Insert a stable separator before TURN-ID's first agent SIDE block."
  (when (and turn-id
             (eq side 'agent)
             (equal e-chat-transcript--last-rendered-turn-id turn-id)
             (eq e-chat-transcript--last-rendered-side 'user))
    (let ((record (e-chat-transcript--turn-record turn-id)))
      (unless (plist-get record :response-separator-rendered)
        (e-chat-transcript--insert-horizontal-separator
         e-chat-transcript--response-separator
         'e-chat-separator-face)
        (plist-put record :response-separator-rendered t)))))

(defun e-chat-transcript--insert-durable-entry-separators (turn-id side)
  "Insert separators needed before a durable TURN-ID block on SIDE."
  (e-chat-transcript--maybe-insert-turn-separator turn-id)
  (e-chat-transcript--maybe-insert-response-separator turn-id side))

(defun e-chat-transcript--record-durable-entry-rendered (turn-id side)
  "Record that a durable TURN-ID block on SIDE was rendered."
  (when (and turn-id side)
    (setq e-chat-transcript--last-rendered-turn-id turn-id)
    (setq e-chat-transcript--last-rendered-side side)))

(defun e-chat-transcript--turn-record (turn-id)
  "Return mutable metadata for TURN-ID, creating it when needed."
  (when turn-id
    (let ((registry (e-chat-transcript--ensure-turn-registry)))
      (or (gethash turn-id registry)
          (let ((record (list :id turn-id
                              :response-separator-rendered nil)))
            (puthash turn-id record registry)
            record)))))

(defun e-chat-transcript--existing-turn-record (turn-id)
  "Return existing metadata for TURN-ID, or nil."
  (when (and turn-id (hash-table-p e-chat-transcript--turn-registry))
    (gethash turn-id e-chat-transcript--turn-registry)))

(defun e-chat-transcript--next-block-id ()
  "Return a new display-local block id."
  (setq e-chat-transcript--block-counter (1+ e-chat-transcript--block-counter))
  (format "block-%d" e-chat-transcript--block-counter))

(defun e-chat-transcript--block-record (block-id turn-id)
  "Return mutable metadata for BLOCK-ID belonging to TURN-ID."
  (let ((registry (e-chat-transcript--ensure-block-registry)))
    (or (gethash block-id registry)
        (let ((record (list :id block-id
                            :turn-id turn-id
                            :kind nil
                            :action-text nil
                            :details-text nil
                            :message-id nil
                            :display-hidden nil
                            :display-overlay nil
                            :side nil
                            :layout-start-marker nil
                            :content-start-marker nil
                            :content-end-marker nil
                            :tool-items nil
                            :child-records nil
                            :tool-list-start-marker nil
                            :tool-list-end-marker nil
                            :start-marker nil
                            :end-marker nil)))
          (puthash block-id record registry)
          (setq e-chat-transcript--block-order (append e-chat-transcript--block-order (list block-id)))
          record))))

(defun e-chat-transcript--remove-block-record (block-id)
  "Remove BLOCK-ID from rendered block metadata."
  (when block-id
    (let ((record (and (hash-table-p e-chat-transcript--block-registry)
                       (gethash block-id e-chat-transcript--block-registry))))
      (when-let ((overlay (plist-get record :display-overlay)))
        (when (overlayp overlay)
          (delete-overlay overlay)))
      (when-let ((message-id (plist-get record :message-id)))
        (when (and (hash-table-p e-chat-transcript--message-block-index)
                   (equal (gethash message-id e-chat-transcript--message-block-index)
                          block-id))
          (remhash message-id e-chat-transcript--message-block-index)))
      (when (hash-table-p e-chat-transcript--block-registry)
        (remhash block-id e-chat-transcript--block-registry)))
    (setq e-chat-transcript--block-order (delete block-id e-chat-transcript--block-order))
    (when (equal e-chat-transcript--focused-block-id block-id)
      (setq e-chat-transcript--focused-block-id nil)
      (setq e-chat-transcript--focused-turn-id nil)
      (when (overlayp e-chat-transcript--focused-turn-overlay)
        (delete-overlay e-chat-transcript--focused-turn-overlay)))
    (when (equal e-chat-transcript--block-view-block-id block-id)
      (setq e-chat-transcript--block-view-block-id nil))
    (when (equal e-chat-transcript--tool-list-block-id block-id)
      (setq e-chat-transcript--tool-list-block-id nil)
      (setq e-chat-transcript--tool-list-index 0)
      (when (overlayp e-chat-transcript--tool-list-overlay)
        (delete-overlay e-chat-transcript--tool-list-overlay)))))

(defun e-chat-transcript--block-layout-bounds (record)
  "Return presentation bounds owned by rendered block RECORD, or nil.
The layout range includes separators immediately preceding the entry, so
hiding a message cannot leave an orphaned response or turn separator."
  (let* ((start-marker (or (plist-get record :layout-start-marker)
                           (plist-get record :start-marker)))
         (end-marker (plist-get record :end-marker))
         (start (and (markerp start-marker) (marker-position start-marker)))
         (end (and (markerp end-marker) (marker-position end-marker))))
    (and start end (< start end) (cons start end))))

(defun e-chat-transcript--block-entry-bounds (record)
  "Return the rendered entry bounds for RECORD, excluding preceding separators."
  (let* ((start-marker (plist-get record :start-marker))
         (end-marker (plist-get record :end-marker))
         (start (and (markerp start-marker) (marker-position start-marker)))
         (end (and (markerp end-marker) (marker-position end-marker))))
    (and start end (< start end) (cons start end))))

(defun e-chat-transcript--set-block-layout-hidden (record hidden)
  "Set rendered block RECORD's layout visibility to HIDDEN.
Only visibility owned by the durable-message projection is changed; unrelated
text invisibility in the chat buffer remains intact."
  (when-let ((bounds (e-chat-transcript--block-layout-bounds record)))
    (let ((start (car bounds))
          (end (cdr bounds))
          (overlay (plist-get record :display-overlay)))
      (when (overlayp overlay)
        (delete-overlay overlay))
      (if hidden
          (let ((overlay (make-overlay start end nil nil nil)))
            (overlay-put overlay 'invisible 'e-chat-message-hidden)
            (overlay-put overlay 'evaporate t)
            (plist-put record :display-overlay overlay))
        (plist-put record :display-overlay nil))
      (plist-put record :display-hidden hidden)
      t)))

(defun e-chat-transcript--refresh-last-rendered-entry ()
  "Recompute separator state from the final visible durable entry."
  (let (record)
    (dolist (block-id (reverse e-chat-transcript--block-order))
      (when (and (not record)
                 (hash-table-p e-chat-transcript--block-registry))
        (let ((candidate (gethash block-id e-chat-transcript--block-registry)))
          (when (and candidate
                     (plist-get candidate :side)
                     (not (e-chat-transcript--block-display-hidden-p candidate))
                     (e-chat-transcript--block-layout-bounds candidate))
            (setq record candidate)))))
    (setq e-chat-transcript--last-rendered-turn-id (plist-get record :turn-id)
          e-chat-transcript--last-rendered-side (plist-get record :side))))

(defun e-chat-transcript--refresh-latest-final-block ()
  "Refresh the latest visible final assistant block cache."
  (setq e-chat-transcript--latest-final-block-id
        (and (hash-table-p e-chat-transcript--block-registry)
             (cl-find-if
              (lambda (block-id)
                (let ((record (gethash block-id e-chat-transcript--block-registry)))
                  (and (eq (plist-get record :kind) 'final)
                       (not (e-chat-transcript--block-display-hidden-p record))
                       (e-chat-transcript--block-layout-bounds record))))
              (reverse e-chat-transcript--block-order)))))

(defun e-chat-transcript--reconcile-message-display (message)
  "Apply MESSAGE's display disposition to its rendered projection.
Return non-nil when the projection was updated locally.  Audit reveal mode
uses a distinct hidden-message presentation, so its rare updates deliberately
fall back to its existing full projection path."
  (let* ((message-id (plist-get message :id))
         (block-id (e-chat-transcript--message-block-id message-id))
         (record (and block-id (hash-table-p e-chat-transcript--block-registry)
                      (gethash block-id e-chat-transcript--block-registry))))
    (when (and record (not e-chat-transcript--reveal-hidden))
      (let ((hidden (e-harness-message-hidden-p message)))
        (e-chat-transcript--set-block-layout-hidden record hidden)
        ;; Activity details and transient redraws are owned by the activity
        ;; component.  The transcript only updates its durable visibility
        ;; projection; the facade/activity dispatcher may follow with a
        ;; detail refresh when the stored message changed.
        )
      (e-chat-transcript--refresh-last-rendered-entry)
      (e-chat-transcript--refresh-latest-final-block)
      t)))

(defun e-chat-transcript--set-turn-time (turn-id field value)
  "Set TURN-ID timing FIELD to VALUE when both are available."
  (when (and turn-id value)
    (plist-put (e-chat-transcript--turn-record turn-id) field value)))

(defun e-chat-transcript--update-block-bounds
    (block-id turn-id start end
              &optional kind action-text content-start content-end tool-items
              details-text)
  "Set BLOCK-ID bounds START through END and action metadata for TURN-ID.
Optional KIND, ACTION-TEXT, CONTENT-START, CONTENT-END, TOOL-ITEMS, and
DETAILS-TEXT describe block actions."
  (when block-id
    (e-chat-transcript--turn-record turn-id)
    (let ((record (e-chat-transcript--block-record block-id turn-id)))
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
        (setq e-chat-transcript--latest-final-block-id block-id)))))

(defun e-chat-transcript--block-at-point ()
  "Return the rendered block id at point, or nil."
  (or (get-text-property (point) 'e-chat-block-id)
      (get-text-property (max (point-min) (1- (point))) 'e-chat-block-id)))

(defun e-chat-transcript--last-rendered-block-id ()
  "Return the most recent rendered block id before the composer."
  (cl-find-if #'e-chat-transcript--live-block-record (reverse e-chat-transcript--block-order)))

(defun e-chat-transcript--focus-block (block-id)
  "Focus BLOCK-ID in response navigation mode."
  (let* ((record (and block-id
                      (hash-table-p e-chat-transcript--block-registry)
                      (gethash block-id e-chat-transcript--block-registry)))
         (start-marker (plist-get record :start-marker))
         (end-marker (plist-get record :end-marker))
         (start (and (markerp start-marker) (marker-position start-marker)))
         (end (and (markerp end-marker) (marker-position end-marker))))
    (unless (and record start end (< start end))
      (user-error "No rendered e chat block to focus"))
    (setq e-chat-transcript--focused-block-id block-id)
    (setq e-chat-transcript--focused-turn-id (plist-get record :turn-id))
    (unless (overlayp e-chat-transcript--focused-turn-overlay)
      (setq e-chat-transcript--focused-turn-overlay (make-overlay start end nil t nil)))
    (move-overlay e-chat-transcript--focused-turn-overlay start end)
    (overlay-put e-chat-transcript--focused-turn-overlay 'face 'e-chat-focused-turn-face)
    (goto-char start)
    (when-let ((window (get-buffer-window (current-buffer) t)))
      (set-window-point window start))
    block-id))

(defun e-chat-transcript--move-focused-block (step)
  "Move focused block by STEP in rendered block order."
  (unless e-chat-transcript--focused-block-id
    (user-error "No focused e chat block"))
  (let ((live-block-order (cl-remove-if-not #'e-chat-transcript--live-block-record
                                            e-chat-transcript--block-order))
        remaining
        (index 0)
        found)
    (setq remaining live-block-order)
    (while (and remaining (not found))
      (if (equal (car remaining) e-chat-transcript--focused-block-id)
          (setq found index)
        (setq index (1+ index)
              remaining (cdr remaining))))
    (unless found
      (user-error "Focused e chat block is no longer rendered"))
    (let ((next-index (max 0 (min (1- (length live-block-order))
                                  (+ found step)))))
      (e-chat-transcript--focus-block (nth next-index live-block-order)))))

(defun e-chat-transcript--focused-block ()
  "Return the focused block record."
  (unless e-chat-transcript--focused-block-id
    (user-error "No focused e chat block"))
  (or (and (hash-table-p e-chat-transcript--block-registry)
           (gethash e-chat-transcript--focused-block-id e-chat-transcript--block-registry))
      (user-error "Focused e chat block is no longer rendered")))

(defun e-chat-transcript--hidden-entry-title-p (title)
  "Return non-nil when TITLE marks a revealed hidden audit block."
  (and (stringp title)
       (string-prefix-p e-chat-transcript--hidden-entry-title-prefix title)))

(defun e-chat-transcript--block-kind-for-title (title)
  "Return block kind for rendered entry TITLE."
  (cond
   ((e-chat-transcript--hidden-entry-title-p title) 'hidden)
   ((equal title "You") 'user)
   ((equal title "Assistant") 'final)
   ((equal title "System") 'system)
   (t 'system)))

(defun e-chat-transcript--block-content-bounds (block)
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

(defun e-chat-transcript--block-details-bounds (block)
  "Return visible expanded detail bounds for BLOCK, or nil."
  (let* ((start-marker (plist-get block :details-start-marker))
         (end-marker (plist-get block :details-end-marker))
         (start (and (markerp start-marker) (marker-position start-marker)))
         (end (and (markerp end-marker) (marker-position end-marker))))
    (when (and start end (< start end))
      (cons start end))))

(defun e-chat-transcript--block-view-bounds (block)
  "Return bounds used by block-local view mode for BLOCK."
  (or (e-chat-transcript--block-details-bounds block)
      (e-chat-transcript--block-content-bounds block)))

(defun e-chat-transcript--block-action-text (block)
  "Return action text for BLOCK."
  (or (plist-get block :action-text)
      (let ((bounds (e-chat-transcript--block-content-bounds block)))
        (string-trim-right
         (buffer-substring-no-properties (car bounds) (cdr bounds))))))

(defun e-chat-transcript--latest-final-block ()
  "Return the latest final assistant block record."
  (let ((block-id e-chat-transcript--latest-final-block-id))
    (unless (and block-id
                 (hash-table-p e-chat-transcript--block-registry)
                 (let ((record (gethash block-id e-chat-transcript--block-registry)))
                   (and record
                        (not (e-chat-transcript--block-display-hidden-p record)))))
      (setq block-id
            (and (hash-table-p e-chat-transcript--block-registry)
                 (cl-find-if
                  (lambda (candidate)
                    (let ((record (gethash candidate e-chat-transcript--block-registry)))
                      (and (eq (plist-get record :kind) 'final)
                           (not (e-chat-transcript--block-display-hidden-p record)))))
                  (reverse e-chat-transcript--block-order)))))
    (or (and block-id (gethash block-id e-chat-transcript--block-registry))
        (user-error "No final e chat response"))))

(defun e-chat-transcript--editable-buffer-mode ()
  "Enable the preferred major mode for editable chat text buffers."
  (if (or (fboundp 'markdown-mode)
          (require 'markdown-mode nil t))
      (markdown-mode)
    (text-mode)))

(defun e-chat-transcript--buffer-with-text (name text &optional read-only)
  "Display NAME containing TEXT, optionally READ-ONLY."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (if read-only
          (special-mode)
        (e-chat-transcript--editable-buffer-mode))
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

(defun e-chat-transcript--resource-link-p (uri)
  "Return non-nil when URI is an e resource link handled by chat."
  (and (stringp uri)
       (or (string-prefix-p "session://" uri)
           (string-prefix-p "e://" uri))))

;;;###autoload
(defun e-chat-open-link (&optional event)
  "Open the resource link at point or mouse EVENT in a read-only buffer.
Only resources exposed by the current chat harness are opened here."
  (interactive "e")
  (when event
    (mouse-set-point event))
  (let ((uri (or (get-text-property (point) 'e-chat-link-url)
                 (and (> (point) (point-min))
                      (get-text-property (1- (point)) 'e-chat-link-url)))))
    (unless (e-chat-transcript--resource-link-p uri)
      (user-error "No supported e-chat resource link at point"))
    (unless (and e-chat-harness e-chat-session-id)
      (user-error "Resource link needs an attached e-chat session"))
    (let ((content
           (e-resources-read
            (e-harness-resources e-chat-harness e-chat-session-id)
            uri)))
      (e-chat-transcript--buffer-with-text
       (format "*e-chat-resource: %s*" uri)
       (if (stringp content) content (format "%s" content))
       t))))


(defun e-chat-transcript--delete-block-details (block)
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

(defun e-chat-transcript--block-order-insert-after (parent-id child-ids)
  "Place CHILD-IDS immediately after PARENT-ID in `e-chat-transcript--block-order'."
  (let ((remaining e-chat-transcript--block-order)
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
    (setq e-chat-transcript--block-order
          (append (nreverse before) child-ids (nreverse after)))))

(defun e-chat-transcript--activity-summary-expanded-p (block)
  "Return non-nil when activity summary BLOCK has rendered children."
  (not (null (plist-get block :children))))

(defun e-chat-transcript--delete-activity-summary-children (block)
  "Delete child blocks rendered for activity summary BLOCK."
  (let ((children (plist-get block :children)))
    (when children
      (let* ((first-block (gethash (car children) e-chat-transcript--block-registry))
             (last-block (gethash (car (last children)) e-chat-transcript--block-registry))
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
        (e-chat-transcript--remove-block-record child-id))
      (plist-put block :children nil)
      (plist-put block :expanded nil))))

(defun e-chat-transcript--insert-activity-summary-child (parent turn-id child)
  "Insert CHILD for activity summary PARENT and return its block id."
  (let* ((block-id (e-chat-transcript--next-block-id))
         (text (plist-get child :text))
         (line (format "  %s\n" text))
         (start (point))
         (content-start (+ start 2))
         (content-end (+ content-start (length text))))
    (e-chat-transcript--insert-protected
     line
     'e-chat-system-face
     `(e-chat-turn-id ,turn-id
       e-chat-block-id ,block-id
       e-chat-parent-block-id ,(plist-get parent :id)))
    (e-chat-transcript--update-block-bounds
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
    (let ((block (e-chat-transcript--block-record block-id turn-id)))
      (plist-put block :parent-block-id (plist-get parent :id)))
    block-id))

(defun e-chat-transcript--insert-activity-summary-children (block)
  "Insert navigable child blocks for activity summary BLOCK."
  (e-chat-transcript--delete-activity-summary-children block)
  (let* ((turn-id (plist-get block :turn-id))
         (children (plist-get block :child-records))
         (end-marker (plist-get block :end-marker))
         (end (and (markerp end-marker)
                   (marker-position end-marker)))
         child-ids)
    (unless end
      (user-error "Focused activity summary has no insertion point"))
    (when children
      (let ((inhibit-read-only t))
        (goto-char end)
        (unless (bolp)
          (insert "\n"))
        (dolist (child children)
          (push (e-chat-transcript--insert-activity-summary-child block turn-id child)
                child-ids)))
      (setq child-ids (nreverse child-ids))
      (plist-put block :children child-ids)
      (plist-put block :expanded t)
      (e-chat-transcript--block-order-insert-after (plist-get block :id) child-ids))))

(defun e-chat-transcript--toggle-activity-summary-children (block)
  "Toggle navigable activity summary children for BLOCK."
  (if (e-chat-transcript--activity-summary-expanded-p block)
      (e-chat-transcript--delete-activity-summary-children block)
    (e-chat-transcript--insert-activity-summary-children block)))

(defun e-chat-transcript--block-details-visible-p (block)
  "Return non-nil when BLOCK has visible expanded detail text."
  (not (null (e-chat-transcript--block-details-bounds block))))

(defun e-chat-transcript--turn-details-text (turn-id &optional details-text)
  "Return expanded details text for TURN-ID and DETAILS-TEXT.
DETAILS-TEXT is supplied by the activity owner as a semantic projection.  A
transcript-only block still has a useful stable fallback without reaching into
activity state."
  (or details-text
      (format "  Turn: %s\n\n" turn-id)))

(defun e-chat-transcript--insert-block-details (block turn-id)
  "Insert expanded details for BLOCK and TURN-ID."
  (e-chat-transcript--insert-block-details-text
   block
   (e-chat-transcript--turn-details-text
    turn-id
    (plist-get block :details-text))))

(defun e-chat-transcript--insert-block-details-text (block text)
  "Insert expanded detail TEXT for BLOCK."
  (e-chat-transcript--delete-block-details block)
  (let* ((end-marker (plist-get block :end-marker))
         (end (and (markerp end-marker) (marker-position end-marker))))
    (unless end
      (user-error "Focused e chat block has no insertion point"))
    (let ((inhibit-read-only t))
      (goto-char end)
      (let ((start (point)))
        (e-chat-transcript--insert-protected
         text
         'e-chat-system-face
         '(e-chat-turn-details t))
        (plist-put block :details-start-marker (copy-marker start nil))
        (plist-put block :details-end-marker (copy-marker (point) nil))))))

(defun e-chat-transcript--toggle-block-details-text (block text)
  "Toggle inline detail TEXT for BLOCK."
  (if (e-chat-transcript--block-details-visible-p block)
      (e-chat-transcript--delete-block-details block)
    (e-chat-transcript--insert-block-details-text block text)
    (e-chat-transcript--enter-block-view block)))

(defun e-chat-transcript--entry-face (title)
  "Return face for chat entry TITLE."
  (cond
   ((e-chat-transcript--hidden-entry-title-p title) 'e-chat-hidden-face)
   ((equal title "You") 'e-chat-user-face)
   ((equal title "Assistant") 'e-chat-final-assistant-face)
   (t 'e-chat-system-face)))

(defun e-chat-transcript--entry-glyph (title)
  "Return glyph for chat entry TITLE."
  (cond
   ((e-chat-transcript--hidden-entry-title-p title) e-chat-transcript--hidden-glyph)
   ((equal title "You") e-chat-transcript--user-glyph)
   ((equal title "Assistant") e-chat-transcript--assistant-glyph)
   (t e-chat-transcript--system-glyph)))

(defun e-chat-transcript--entry-heading (title)
  "Return compact heading text for chat entry TITLE."
  (pcase title
    ((or "You" "Assistant") (e-chat-transcript--entry-glyph title))
    (_ (format "%s %s" (e-chat-transcript--entry-glyph title) title))))

(defun e-chat-transcript--entry-text (title content)
  "Return display text for chat entry TITLE and CONTENT."
  (if (member title '("You" "Assistant"))
      (format "%s %s\n\n" (e-chat-transcript--entry-heading title) content)
    (format "%s\n%s\n\n" (e-chat-transcript--entry-heading title) content)))

(defun e-chat-transcript--entry-content-offset (title)
  "Return the character offset of TITLE entry content start."
  (if (member title '("You" "Assistant"))
      (1+ (length (e-chat-transcript--entry-heading title)))
    (1+ (length (e-chat-transcript--entry-heading title)))))

(defun e-chat-transcript--add-markdown-face (start end face)
  "Add Markdown FACE between START and END."
  (when (< start end)
    (add-face-text-property start end face t)))

(defconst e-chat-transcript--markdown-mode-copied-properties
  '(face font-lock-face font-lock-multiline keymap mouse-face help-echo)
  "Text properties copied from `markdown-mode' fontification.")

(defun e-chat-clear-markdown-presentation (start end)
  "Clear Markdown presentation properties between START and END."
  (when (< start end)
    (remove-list-of-text-properties
     start end
     '(face font-lock-face font-lock-multiline keymap mouse-face help-echo
       invisible display e-chat-markdown-syntax))))

(defun e-chat-transcript--apply-markdown-mode-properties (content-start content-end)
  "Apply `markdown-mode' fontification between CONTENT-START and CONTENT-END.
Return non-nil when `markdown-mode' was available and used."
  (when (and (< content-start content-end)
             (require 'markdown-mode nil t))
    (let ((content (buffer-substring-no-properties content-start content-end))
          (target-buffer (current-buffer)))
      (e-chat-clear-markdown-presentation content-start content-end)
      (with-temp-buffer
        (insert content)
        (let ((markdown-mode-hook nil))
          ;; Assistant presentation borrows `markdown-mode' fontification; it
          ;; must not run user buffer-setup hooks in this disposable buffer.
          (ignore markdown-mode-hook)
          (delay-mode-hooks (markdown-mode)))
        (font-lock-ensure (point-min) (point-max))
        (let ((source-end (point-max))
              (source-pos (point-min)))
          (while (< source-pos source-end)
            (let ((next-pos (or (next-property-change source-pos nil source-end)
                                source-end)))
              (dolist (property e-chat-transcript--markdown-mode-copied-properties)
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

(defun e-chat-transcript--conceal-markdown-syntax (start end)
  "Hide Markdown syntax between START and END."
  (when (< start end)
    (add-text-properties
     start end
     '(invisible e-chat-markdown-syntax
       e-chat-markdown-syntax t))))

(defun e-chat-transcript--display-markdown-syntax (start end display)
  "Display Markdown syntax between START and END as DISPLAY."
  (when (< start end)
    (add-text-properties start end `(display ,display e-chat-markdown-syntax t))))

(defun e-chat-transcript--line-content-start (line-start content-start)
  "Return CONTENT-START or LINE-START, whichever is later."
  (max line-start content-start))

(defun e-chat-transcript--apply-markdown-line-faces (content-start content-end)
  "Apply block-level Markdown faces between CONTENT-START and CONTENT-END."
  (save-excursion
    (goto-char content-start)
    (let ((in-code-block nil))
      (while (< (point) content-end)
        (let* ((line-start (line-beginning-position))
               (line-end (min (line-end-position) content-end))
               (line-content-start (e-chat-transcript--line-content-start
                                    line-start content-start))
               (line-text (buffer-substring-no-properties
                           line-content-start line-end)))
          (cond
           ((string-match-p "\\`[ \t]*```" line-text)
            (e-chat-transcript--conceal-markdown-syntax
             line-content-start
             (min (1+ line-end) content-end))
            (setq in-code-block (not in-code-block)))
           (in-code-block
            (e-chat-transcript--add-markdown-face line-content-start line-end
                                       'e-chat-markdown-code-block-face))
           ((string-match "\\`[ \t]*\\(#[#]*[ \t]+\\)" line-text)
            (let ((heading-start (+ line-content-start (match-beginning 1)))
                  (heading-text-start (+ line-content-start (match-end 1))))
              (e-chat-transcript--conceal-markdown-syntax
               heading-start heading-text-start)
              (e-chat-transcript--add-markdown-face heading-text-start line-end
                                         'e-chat-markdown-heading-face)))
           ((string-match
             "\\`[ \t]*\\([-+*]\\|[0-9]+\\.\\)\\([ \t]+\\)"
             line-text)
            (let ((marker-start (+ line-content-start (match-beginning 1)))
                  (marker-end (+ line-content-start (match-end 1)))
                  (content-start (+ line-content-start (match-end 0)))
                  (marker (match-string 1 line-text)))
              (if (string-match-p "\\`[-+*]\\'" marker)
                  (e-chat-transcript--display-markdown-syntax marker-start marker-end "•")
                (e-chat-transcript--add-markdown-face marker-start marker-end
                                           'e-chat-markdown-list-face))
              (e-chat-transcript--add-markdown-face content-start line-end
                                         'e-chat-markdown-list-face))))
          (forward-line 1))))))

(defun e-chat-transcript--apply-markdown-inline-face
    (regexp content-start content-end face &optional group)
  "Apply FACE to REGEXP GROUP between CONTENT-START and CONTENT-END."
  (save-excursion
    (goto-char content-start)
    (while (re-search-forward regexp content-end t)
      (let ((group (or group 1)))
        (e-chat-transcript--add-markdown-face
         (match-beginning group) (match-end group) face)))))

(defun e-chat-transcript--apply-markdown-delimited-face
    (regexp content-start content-end face)
  "Apply FACE to REGEXP group 2 between CONTENT-START and CONTENT-END.
Hide REGEXP groups 1 and 3 as Markdown syntax."
  (save-excursion
    (goto-char content-start)
    (while (re-search-forward regexp content-end t)
      (e-chat-transcript--conceal-markdown-syntax (match-beginning 1) (match-end 1))
      (e-chat-transcript--add-markdown-face (match-beginning 2) (match-end 2) face)
      (e-chat-transcript--conceal-markdown-syntax (match-beginning 3) (match-end 3)))))

(defun e-chat-transcript--apply-markdown-emphasis-face (content-start content-end)
  "Apply emphasis presentation between CONTENT-START and CONTENT-END."
  (save-excursion
    (goto-char content-start)
    (while (re-search-forward
            "\\(^\\|[[:space:]]\\)\\(\\*\\)\\([^*\n]+\\)\\(\\*\\)"
            content-end t)
      (e-chat-transcript--conceal-markdown-syntax (match-beginning 2) (match-end 2))
      (e-chat-transcript--add-markdown-face
       (match-beginning 3) (match-end 3) 'e-chat-markdown-emphasis-face)
      (e-chat-transcript--conceal-markdown-syntax (match-beginning 4) (match-end 4)))))

(defun e-chat-transcript--apply-markdown-link-faces (content-start content-end)
  "Apply Markdown link faces and metadata between CONTENT-START and CONTENT-END."
  (save-excursion
    (goto-char content-start)
    (while (re-search-forward "\\[\\([^]\n]+\\)\\](\\([^) \n]+\\))"
                              content-end t)
      (let ((label-start (match-beginning 1))
            (label-end (match-end 1))
            (url (match-string-no-properties 2)))
        (e-chat-transcript--add-markdown-face label-start label-end
                                   'e-chat-markdown-link-face)
        (add-text-properties label-start label-end
                             `(help-echo ,url e-chat-link-url ,url))
        (e-chat-transcript--conceal-markdown-syntax (match-beginning 0) label-start)
        (e-chat-transcript--conceal-markdown-syntax label-end (match-end 0))))))

(defun e-chat-transcript--apply-markdown-link-targets (content-start content-end)
  "Attach E's exact link targets between CONTENT-START and CONTENT-END.
`markdown-mode' owns presentation faces and syntax visibility on this path;
the chat transcript still owns the URL consumed by its link commands."
  (save-excursion
    (goto-char content-start)
    (while (re-search-forward "\\[\\([^]\n]+\\)\\](\\([^) \n]+\\))"
                              content-end t)
      (let ((label-start (match-beginning 1))
            (label-end (match-end 1))
            (url (match-string-no-properties 2)))
        (add-text-properties label-start label-end
                             `(help-echo ,url e-chat-link-url ,url))))))

(defun e-chat-transcript--apply-deterministic-markdown (content-start content-end)
  "Apply E's deterministic Markdown presentation from CONTENT-START to CONTENT-END."
  (when (< content-start content-end)
    (e-chat-transcript--apply-markdown-line-faces content-start content-end)
    (e-chat-transcript--apply-markdown-delimited-face
     "\\(`\\)\\([^`\n]+\\)\\(`\\)" content-start content-end
     'e-chat-markdown-code-face)
    (e-chat-transcript--apply-markdown-delimited-face
     "\\(\\*\\*\\)\\([^*\n]+\\)\\(\\*\\*\\)" content-start content-end
     'e-chat-markdown-strong-face)
    (e-chat-transcript--apply-markdown-emphasis-face content-start content-end)
    (e-chat-transcript--apply-markdown-link-faces content-start content-end)))

(defun e-chat-transcript--apply-assistant-markdown (content-start content-end)
  "Apply Markdown presentation between CONTENT-START and CONTENT-END."
  (when (< content-start content-end)
    (if (e-chat-transcript--apply-markdown-mode-properties content-start content-end)
        (e-chat-transcript--apply-markdown-link-targets content-start content-end)
      (e-chat-clear-markdown-presentation content-start content-end)
      (e-chat-transcript--apply-deterministic-markdown
       content-start content-end))))

(defun e-chat-transcript--output-mode ()
  "Return the effective assistant output markup mode for this chat buffer.
Defaults to `markdown' outside an attached session."
  (if (and e-chat-harness e-chat-session-id)
      (ignore-errors
        (e-chat-output-mode-resolve e-chat-harness e-chat-session-id))
    'markdown))

(defun e-chat-transcript--structured-blocks-registry ()
  "Return a fresh structured-block registry for the attached session.
Returns an empty registry outside an attached session, so unregistered
content still passes through `e-structured-blocks-render' unchanged."
  (if (and e-chat-harness e-chat-session-id)
      (e-harness-structured-blocks e-chat-harness e-chat-session-id)
    (e-structured-blocks-registry-create)))

(defun e-chat-transcript--assistant-display-text (content)
  "Return CONTENT with any registered structured blocks applied for display.
This is the shell's only knowledge of structured blocks: it asks the core
registry generically and never inspects a specific capability's block
syntax.  With no registered kinds, CONTENT is returned byte-for-byte."
  (plist-get (e-structured-blocks-render content (e-chat-transcript--structured-blocks-registry))
             :text))

(defun e-chat-transcript--apply-org-mode-properties (content-start content-end)
  "Fontify assistant Org markup between CONTENT-START and CONTENT-END.
Return non-nil when Org fontification ran."
  (when (< content-start content-end)
    (let ((content (buffer-substring-no-properties content-start content-end))
          (target-buffer (current-buffer)))
      (e-chat-clear-markdown-presentation content-start content-end)
      (with-temp-buffer
        (let ((org-mode-hook nil)
              (org-inhibit-startup t))
          ;; These bindings are dynamic inputs to `org-mode'; count them as
          ;; used explicitly so warnings-as-errors does not mistake the
          ;; intentionally side-effect-only setup for dead locals.
          (ignore org-mode-hook org-inhibit-startup)
          (delay-mode-hooks (org-mode)))
        (insert content)
        (font-lock-ensure (point-min) (point-max))
        (let ((source-end (point-max))
              (source-pos (point-min)))
          (while (< source-pos source-end)
            (let ((next-pos (or (next-property-change source-pos nil source-end)
                                source-end)))
              (dolist (property e-chat-transcript--markdown-mode-copied-properties)
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

(defun e-chat-transcript--apply-org-link-metadata (content-start content-end)
  "Attach clickable link metadata to Org links between CONTENT-START/END.
Org links `[[target][description]]' and `[[target]]' get a `help-echo' and
`e-chat-link-url' target; the surrounding bracket syntax is concealed so only
the description (or the bare target) remains visible."
  (save-excursion
    (goto-char content-start)
    (while (re-search-forward
            "\\[\\[\\([^]
]+?\\)\\(?:\\]\\[\\([^]
]+?\\)\\)?\\]\\]"
            content-end t)
      (let* ((target (match-string-no-properties 1))
             (has-description (match-beginning 2))
             (visible-start (or has-description (match-beginning 1)))
             (visible-end (or (match-end 2) (match-end 1))))
        (e-chat-transcript--add-markdown-face visible-start visible-end
                                   'e-chat-markdown-link-face)
        (add-text-properties visible-start visible-end
                             `(help-echo ,target e-chat-link-url ,target))
        (e-chat-transcript--conceal-markdown-syntax (match-beginning 0) visible-start)
        (e-chat-transcript--conceal-markdown-syntax visible-end (match-end 0))))))

(defun e-chat-transcript--apply-assistant-org (content-start content-end)
  "Apply Org presentation between CONTENT-START and CONTENT-END."
  (when (< content-start content-end)
    (e-chat-transcript--apply-org-mode-properties content-start content-end)
    (e-chat-transcript--apply-org-link-metadata content-start content-end)))

(defun e-chat-transcript--apply-assistant-presentation (content-start content-end)
  "Apply the buffer's output-mode presentation between CONTENT-START/END."
  (if (eq (e-chat-transcript--output-mode) 'org)
      (e-chat-transcript--apply-assistant-org content-start content-end)
    (e-chat-transcript--apply-assistant-markdown content-start content-end))
  (e-chat-transcript--apply-final-assistant-face content-start content-end))

(defun e-chat-transcript--rerender-assistant-blocks ()
  "Re-render already-visible final assistant blocks for the current output mode.
Toggling output mode applies only to new turns' markup, but the visible
transcript should still match the new rendering so the toggle is not confusing."
  (when (hash-table-p e-chat-transcript--block-registry)
    (e-chat-transcript--cancel-pending-markdown-presentation)
    (let ((inhibit-read-only t))
      (save-excursion
        (dolist (block-id e-chat-transcript--block-order)
          (let ((block (gethash block-id e-chat-transcript--block-registry)))
            (when (eq (plist-get block :kind) 'final)
              (let* ((bounds (ignore-errors
                               (e-chat-transcript--block-content-bounds block)))
                     (start (car-safe bounds))
                     (end (cdr-safe bounds)))
                (when (and start end (< start end))
                  (e-chat-clear-markdown-presentation start end)
                  (e-chat-transcript--apply-assistant-presentation start end))))))))))

(defun e-chat-transcript--deferred-markdown-chunk-end (chunk-start content-end)
  "Return the end of the deferred Markdown chunk after CHUNK-START."
  (save-excursion
    (goto-char chunk-start)
    (let ((lines (if (and (integerp e-chat-deferred-markdown-chunk-lines)
                          (> e-chat-deferred-markdown-chunk-lines 0))
                     e-chat-deferred-markdown-chunk-lines
                   1)))
      (forward-line lines)
      (min (point) content-end))))

(defun e-chat-transcript--apply-assistant-markdown-chunk
    (chunk-start chunk-end content-start content-end)
  "Apply fallback Markdown presentation to one deferred chunk.
CHUNK-START and CHUNK-END bound the work.  CONTENT-START and CONTENT-END
bound the original assistant content and are used for first-chunk clearing."
  (when (= chunk-start content-start)
    (e-chat-clear-markdown-presentation content-start content-end)
    (e-chat-transcript--apply-final-assistant-face content-start content-end))
  (e-chat-transcript--apply-markdown-line-faces chunk-start chunk-end)
  (e-chat-transcript--apply-markdown-delimited-face
   "\\(`\\)\\([^`\n]+\\)\\(`\\)" chunk-start chunk-end
   'e-chat-markdown-code-face)
  (e-chat-transcript--apply-markdown-delimited-face
   "\\(\\*\\*\\)\\([^*\n]+\\)\\(\\*\\*\\)" chunk-start chunk-end
   'e-chat-markdown-strong-face)
  (e-chat-transcript--apply-markdown-emphasis-face chunk-start chunk-end)
  (e-chat-transcript--apply-markdown-link-faces chunk-start chunk-end)
  (e-chat-transcript--apply-final-assistant-face chunk-start chunk-end))

(defun e-chat-transcript--cancel-pending-markdown-presentation ()
  "Cancel all pending deferred assistant Markdown presentation jobs."
  (e-ui-work-cancel-matching (current-buffer) 'markdown-presentation)
  (cl-incf e-chat-transcript--markdown-presentation-generation))

(defun e-chat-transcript--defer-assistant-markdown-p (content)
  "Return non-nil when CONTENT should defer Markdown presentation."
  (and (stringp content)
       (integerp e-chat-deferred-markdown-threshold-bytes)
       (> e-chat-deferred-markdown-threshold-bytes 0)
       (> (string-bytes content)
          e-chat-deferred-markdown-threshold-bytes)))

(defun e-chat-transcript--finish-deferred-assistant-markdown
    (start-marker end-marker position-marker)
  "Release deferred Markdown START-MARKER, END-MARKER, and POSITION-MARKER."
  (set-marker start-marker nil)
  (set-marker end-marker nil)
  (set-marker position-marker nil))

(defun e-chat-transcript--run-deferred-assistant-markdown-chunk
    (start-marker end-marker position-marker generation)
  "Apply one deferred Markdown chunk.
GENERATION must match the current buffer-local Markdown presentation
generation.  Return non-nil when another chunk remains."
  (if (not (equal generation e-chat-transcript--markdown-presentation-generation))
      (progn
        (e-chat-transcript--finish-deferred-assistant-markdown
         start-marker end-marker position-marker)
        nil)
    (let ((content-start (marker-position start-marker))
          (content-end (marker-position end-marker))
          (chunk-start (marker-position position-marker)))
      (if (not (and content-start
                    content-end
                    chunk-start
                    (< chunk-start content-end)))
          (progn
            (e-chat-transcript--finish-deferred-assistant-markdown
             start-marker end-marker position-marker)
            nil)
        (let* ((chunk-end
                (e-chat-transcript--deferred-markdown-chunk-end chunk-start content-end))
               (chunk-end (if (> chunk-end chunk-start)
                              chunk-end
                            content-end))
               (has-more (< chunk-end content-end))
               (inhibit-read-only t))
          (save-excursion
            (e-chat-transcript--apply-assistant-markdown-chunk
             chunk-start chunk-end content-start content-end))
          (set-marker position-marker chunk-end)
          (unless has-more
            (e-chat-transcript--finish-deferred-assistant-markdown
             start-marker end-marker position-marker))
          has-more)))))

(defun e-chat-transcript--schedule-deferred-assistant-markdown-chunk
    (start-marker end-marker position-marker generation block-id)
  "Schedule one deferred Markdown chunk for assistant CONTENT markers."
  (e-ui-work-schedule
   (e-ui-work-spec-create
    :id "chat_markdown_presentation"
    :description "Apply deferred assistant Markdown presentation."
    :owner 'markdown-presentation
    :target-buffer (current-buffer)
    :key generation
    :generation generation
    :delay 0
    :focus-policy 'preserve
    :reentrancy-policy 'defer
    :stale-p (lambda (_job)
               (not (equal generation
                           e-chat-transcript--markdown-presentation-generation)))
    :apply
    (lambda (_job _handle)
      (when (e-chat-transcript--run-deferred-assistant-markdown-chunk
             start-marker
             end-marker
             position-marker
             generation)
        (e-chat-transcript--schedule-deferred-assistant-markdown-chunk
         start-marker
         end-marker
         position-marker
         generation
         block-id))))
   :on-event (lambda (&rest _)
               (e-chat-surface-refresh-ui-work-diagnostics))))

(defun e-chat-transcript--schedule-assistant-markdown
    (content-start content-end &optional block-id)
  "Schedule deferred Markdown presentation for assistant CONTENT bounds."
  (let* ((start-marker (copy-marker content-start nil))
         (end-marker (copy-marker content-end t))
         (position-marker (copy-marker content-start nil))
         (generation e-chat-transcript--markdown-presentation-generation))
    (e-chat-transcript--schedule-deferred-assistant-markdown-chunk
     start-marker
     end-marker
     position-marker
     generation
     block-id)))

(defun e-chat-transcript--apply-final-assistant-face (content-start content-end)
  "Apply settled assistant styling from CONTENT-START to CONTENT-END.
Preserve Markdown faces already present in the range."
  (when (< content-start content-end)
    (add-face-text-property content-start
                            content-end
                            'e-chat-final-assistant-face
                            t)))

(defun e-chat-transcript--insert-entry
    (title content &optional ensure-composer turn-id details-text message-id hidden
           assistant-presented)
  "Insert a protected chat entry with TITLE and CONTENT.
When ENSURE-COMPOSER is non-nil, recreate the composer after inserting.
TURN-ID tags the rendered entry for response navigation.  DETAILS-TEXT, when
non-nil, is used by focused block activation.  MESSAGE-ID associates a durable
session message with its rendered block.  When HIDDEN is non-nil, the entry is
kept in the projection but invisible until its display disposition changes.
Assistant CONTENT is passed through the structured-block registry before
display unless ASSISTANT-PRESENTED is non-nil, meaning the chat application
service already applied that transform.  A shell with no registered kinds
shows CONTENT unchanged."
  (e-chat-transcript--profile-call
   'chat.insert-entry
   (list :session-id e-chat-session-id
         :turn-id turn-id
         :buffer-name (buffer-name)
         :metadata (list :title title
                         :ensure-composer (and ensure-composer t)
                         :durable-message (and message-id t)
                         :hidden (and hidden t)))
   (lambda ()
     (let* ((side (e-chat-transcript--entry-side title))
            (block-id (and turn-id (e-chat-transcript--next-block-id)))
            (content (if (and (equal title "Assistant")
                              (not assistant-presented))
                        (e-chat-transcript--assistant-display-text content)
                      content)))
       (let ((inhibit-read-only t))
         (goto-char (point-max))
         (unless (or (bobp) (bolp))
           (insert "\n"))
         (let ((layout-start (point)))
           (e-chat-transcript--insert-durable-entry-separators turn-id side)
           (let* ((start (point))
                  (content-start (+ start (e-chat-transcript--entry-content-offset title))))
             (e-chat-transcript--insert-protected
              (e-chat-transcript--entry-text title content)
              (e-chat-transcript--entry-face title)
              (when block-id
                `(e-chat-turn-id ,turn-id
                  e-chat-block-id ,block-id)))
             (when (equal title "Assistant")
               (if (eq (e-chat-transcript--output-mode) 'org)
                   (e-chat-transcript--apply-assistant-org content-start (point))
                 (if (e-chat-transcript--defer-assistant-markdown-p content)
                     (e-chat-transcript--schedule-assistant-markdown
                      content-start (point) block-id)
                   (e-chat-transcript--apply-assistant-markdown content-start (point))))
               (e-chat-transcript--apply-final-assistant-face content-start (point)))
             (e-chat-transcript--update-block-bounds
              block-id turn-id start (point)
              (e-chat-transcript--block-kind-for-title title)
              content content-start (+ content-start (length content)) nil details-text)
             (when block-id
               (let ((record (e-chat-transcript--block-record block-id turn-id)))
                 (plist-put record :layout-start-marker
                            (copy-marker layout-start nil))
                 (plist-put record :side side)
                 (e-chat-transcript--associate-message-block message-id block-id)
                 (when hidden
                   (e-chat-transcript--set-block-layout-hidden record t))))
             (unless hidden
               (e-chat-transcript--record-durable-entry-rendered turn-id side))))
       (when hidden
         (e-chat-transcript--refresh-last-rendered-entry)
         (e-chat-transcript--refresh-latest-final-block)))))))

(defun e-chat-transcript-enter-response-navigation ()
  "Enter response navigation mode and focus the nearest rendered turn.
This owner-namespaced port is used by the composer when Escape crosses from
the input pane to its transcript."
  (interactive)
  (unless (e-chat-surface-transcript-p)
    (user-error "Response navigation is only available in e chat buffers"))
  (let ((block-id (or (e-chat-transcript--block-at-point)
                      (e-chat-transcript--last-rendered-block-id))))
    (unless block-id
      (user-error "No rendered e chat blocks"))
    (e-chat-response-navigation-mode 1)
    (e-chat-transcript--focus-block block-id)))

(defun e-chat-enter-response-navigation ()
  "Enter response navigation mode in the current e chat buffer.
Keep the historical command name as a public transcript command; the
implementation and owner-to-owner call path use the namespaced port above."
  (interactive)
  (e-chat-transcript-enter-response-navigation))

(defun e-chat-response-navigation-next ()
  "Focus the next rendered turn block."
  (interactive)
  (e-chat-transcript--move-focused-block 1))

(defun e-chat-response-navigation-previous ()
  "Focus the previous rendered turn block."
  (interactive)
  (e-chat-transcript--move-focused-block -1))

(defun e-chat-response-navigation-activate ()
  "Activate the focused block according to its kind."
  (interactive)
  (let ((block (e-chat-transcript--focused-block)))
    (pcase (plist-get block :kind)
      ('activity
       (e-chat-transcript--open-tool-list block))
      ('activity-summary
       (e-chat-transcript--toggle-activity-summary-children block))
      ('activity-tool-batch
       (e-chat-transcript--open-tool-list block))
      ('system
       (if-let ((details-text (plist-get block :details-text)))
           (e-chat-transcript--toggle-block-details-text block details-text)
         (e-chat-transcript--enter-block-view block)))
      (_
       (if-let ((details-text (plist-get block :details-text)))
           (e-chat-transcript--toggle-block-details-text block details-text)
         (e-chat-transcript--enter-block-view block))))))

(defun e-chat-response-navigation-insert ()
  "Leave response navigation and focus the composer."
  (interactive)
  (e-chat-transcript-leave-navigation)
  (e-chat-surface-enter-composer-input-state))

(defun e-chat-response-navigation-copy ()
  "Copy the focused block's action text."
  (interactive)
  (let ((text (e-chat-transcript--block-action-text (e-chat-transcript--focused-block))))
    (kill-new text)
    (message "Copied e chat block")
    text))

(defun e-chat-transcript--open-block-text (block)
  "Open BLOCK action text in a new editable buffer."
  (e-chat-transcript--buffer-with-text "*e-chat-block*" (e-chat-transcript--block-action-text block)))

(defun e-chat-response-navigation-open ()
  "Open the focused block in a new editable buffer."
  (interactive)
  (e-chat-transcript--open-block-text (e-chat-transcript--focused-block)))

(defun e-chat-transcript--display-details-buffer (text)
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
  (let* ((block (e-chat-transcript--focused-block))
         (turn-id (plist-get block :turn-id)))
    (unless turn-id
      (user-error "Focused e chat block has no turn details"))
    (e-chat-transcript--display-details-buffer
     (e-chat-transcript--turn-details-text
      turn-id
      (plist-get block :details-text)))))

(defun e-chat-transcript--first-live-block-if (predicate)
  "Return the first live rendered block id whose record satisfies PREDICATE."
  (cl-find-if
   (lambda (block-id)
     (when-let ((record (e-chat-transcript--live-block-record block-id)))
       (funcall predicate record)))
   e-chat-transcript--block-order))

(defun e-chat-response-navigation-toggle-hidden ()
  "Reveal or hide messages kept out of the clean transcript.
A calibration follow-up hides the superseded first attempt and the
machine-authored corrective prompt so the reply reads as one answer.  This
exposes them as dimmed, focusable audit blocks so the user can inspect what was
removed, and hides them again on a second press.  Focus lands on the first
revealed block when revealing, or on the block that was focused when hiding."
  (interactive)
  (unless e-chat-response-navigation-mode
    (user-error "Response navigation is not active"))
  (let* ((block (ignore-errors (e-chat-transcript--focused-block)))
         (turn-id (plist-get block :turn-id))
         (revealing (not e-chat-transcript--reveal-hidden)))
    (setq e-chat-transcript--reveal-hidden revealing)
    (e-chat-transcript--rerender-transcript)
    (e-chat-response-navigation-mode 1)
    (let ((target
           (or (and revealing
                    (e-chat-transcript--first-live-block-if
                     (lambda (record) (eq (plist-get record :kind) 'hidden))))
               (and turn-id
                    (e-chat-transcript--first-live-block-if
                     (lambda (record)
                       (and (equal (plist-get record :turn-id) turn-id)
                            (not (eq (plist-get record :kind) 'hidden))))))
               (e-chat-transcript--last-rendered-block-id))))
      (if target
          (e-chat-transcript--focus-block target)
        (e-chat-response-navigation-mode -1)
        (message "No rendered e chat blocks to focus")))))

(defun e-chat-copy-latest-response ()
  "Copy the latest final assistant response."
  (interactive)
  (with-current-buffer (e-chat-surface-transcript-buffer)
    (let ((text (e-chat-transcript--block-action-text (e-chat-transcript--latest-final-block))))
      (kill-new text)
      (message "Copied latest e chat response")
      text)))

(defun e-chat-open-latest-response ()
  "Open the latest final assistant response in an editable buffer."
  (interactive)
  (with-current-buffer (e-chat-surface-transcript-buffer)
    (e-chat-transcript--open-block-text (e-chat-transcript--latest-final-block))))

(defun e-chat-transcript--enter-block-view (block)
  "Enter block-local view mode for BLOCK."
  (let* ((block-id (plist-get block :id))
         (bounds (e-chat-transcript--block-view-bounds block)))
    (e-chat-response-navigation-mode -1)
    (setq e-chat-transcript--focused-block-id block-id)
    (setq e-chat-transcript--focused-turn-id (plist-get block :turn-id))
    (setq e-chat-transcript--block-view-block-id block-id)
    (e-chat-block-view-mode 1)
    (goto-char (car bounds))))

(defun e-chat-transcript--block-view-block ()
  "Return block active in block view mode."
  (or (and e-chat-transcript--block-view-block-id
           (hash-table-p e-chat-transcript--block-registry)
           (gethash e-chat-transcript--block-view-block-id e-chat-transcript--block-registry))
      (user-error "No e chat block view is active")))

(defun e-chat-transcript--block-view-clamp-point ()
  "Keep point inside the active block content bounds."
  (let ((bounds (e-chat-transcript--block-view-bounds (e-chat-transcript--block-view-block))))
    (when (< (point) (car bounds))
      (goto-char (car bounds)))
    (when (> (point) (cdr bounds))
      (goto-char (cdr bounds)))))

(defun e-chat-transcript--block-view-keep-region-active ()
  "Keep an active block-view region active after modal motion."
  (when (region-active-p)
    (setq deactivate-mark nil)))

(defun e-chat-block-view-left ()
  "Move left inside the focused block."
  (interactive)
  (let ((bounds (e-chat-transcript--block-view-bounds (e-chat-transcript--block-view-block))))
    (when (> (point) (car bounds))
      (backward-char 1)))
  (e-chat-transcript--block-view-keep-region-active))

(defun e-chat-block-view-right ()
  "Move right inside the focused block."
  (interactive)
  (let ((bounds (e-chat-transcript--block-view-bounds (e-chat-transcript--block-view-block))))
    (when (< (point) (cdr bounds))
      (forward-char 1)))
  (e-chat-transcript--block-view-keep-region-active))

(defun e-chat-block-view-down ()
  "Move down inside the focused block."
  (interactive)
  (forward-line 1)
  (e-chat-transcript--block-view-clamp-point)
  (e-chat-transcript--block-view-keep-region-active))

(defun e-chat-block-view-up ()
  "Move up inside the focused block."
  (interactive)
  (forward-line -1)
  (e-chat-transcript--block-view-clamp-point)
  (e-chat-transcript--block-view-keep-region-active))

(defun e-chat-block-view-beginning ()
  "Move to the beginning of the focused block content."
  (interactive)
  (goto-char (car (e-chat-transcript--block-view-bounds (e-chat-transcript--block-view-block))))
  (e-chat-transcript--block-view-keep-region-active))

(defun e-chat-block-view-end ()
  "Move to the end of the focused block content."
  (interactive)
  (goto-char (cdr (e-chat-transcript--block-view-bounds (e-chat-transcript--block-view-block))))
  (e-chat-transcript--block-view-keep-region-active))

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
                (e-chat-transcript--block-action-text (e-chat-transcript--block-view-block)))))
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
    (let ((block-id e-chat-transcript--block-view-block-id))
      (e-chat-block-view-mode -1)
      (e-chat-response-navigation-mode 1)
      (e-chat-transcript--focus-block block-id))))

(defun e-chat-block-view-insert ()
  "Leave block view and focus the composer."
  (interactive)
  (e-chat-transcript-leave-navigation)
  (e-chat-surface-enter-composer-input-state))

(defun e-chat-transcript--delete-tool-list (block)
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

(defun e-chat-transcript--open-tool-list (block)
  "Open a collapsed tool-call list for activity BLOCK."
  (let ((items (plist-get block :tool-items)))
    (unless items
      (user-error "Focused activity block has no tool calls"))
    (e-chat-transcript--delete-tool-list block)
    (let* ((end-marker (plist-get block :end-marker))
           (end (and (markerp end-marker) (marker-position end-marker))))
      (unless end
        (user-error "Focused activity block has no insertion point"))
      (let ((inhibit-read-only t))
        (goto-char end)
        (let ((start (point)))
          (e-chat-transcript--insert-protected "\n" 'e-chat-system-face
                                    '(e-chat-tool-list t))
          (cl-loop for item in items
                   for index from 0
                   do
                   (let ((item-start (point)))
                     (e-chat-transcript--insert-protected
                      (format "  %d. %s\n" (1+ index)
                              (plist-get item :call))
                      'e-chat-system-face
                      `(e-chat-tool-list t e-chat-tool-index ,index))
                     (plist-put item :start-marker (copy-marker item-start nil))
                     (plist-put item :end-marker (copy-marker (point) nil))))
          (plist-put block :tool-list-start-marker (copy-marker start nil))
          (plist-put block :tool-list-end-marker (copy-marker (point) nil)))))
    (let ((block-id (plist-get block :id)))
      (e-chat-response-navigation-mode -1)
      (setq e-chat-transcript--focused-block-id block-id)
      (setq e-chat-transcript--focused-turn-id (plist-get block :turn-id))
      (setq e-chat-transcript--tool-list-block-id block-id)
      (setq e-chat-transcript--tool-list-index 0)
      (e-chat-tool-list-mode 1)
      (e-chat-transcript--focus-tool-list-item))))

(defun e-chat-transcript--tool-list-block ()
  "Return active tool-list block."
  (or (and e-chat-transcript--tool-list-block-id
           (hash-table-p e-chat-transcript--block-registry)
           (gethash e-chat-transcript--tool-list-block-id e-chat-transcript--block-registry))
      (user-error "No e chat tool list is active")))

(defun e-chat-transcript--focus-tool-list-item ()
  "Highlight the selected tool-list item."
  (let* ((block (e-chat-transcript--tool-list-block))
         (items (plist-get block :tool-items))
         (item (nth e-chat-transcript--tool-list-index items))
         (start (and item
                     (markerp (plist-get item :start-marker))
                     (marker-position (plist-get item :start-marker))))
         (end (and item
                   (markerp (plist-get item :end-marker))
                   (marker-position (plist-get item :end-marker)))))
    (unless (and start end)
      (user-error "No e chat tool item to focus"))
    (unless (overlayp e-chat-transcript--tool-list-overlay)
      (setq e-chat-transcript--tool-list-overlay (make-overlay start end nil t nil)))
    (move-overlay e-chat-transcript--tool-list-overlay start end)
    (overlay-put e-chat-transcript--tool-list-overlay 'face 'e-chat-focused-turn-face)
    (goto-char start)))

(defun e-chat-tool-list-next ()
  "Focus the next tool call in the active tool list."
  (interactive)
  (let* ((items (plist-get (e-chat-transcript--tool-list-block) :tool-items))
         (max-index (1- (length items))))
    (setq e-chat-transcript--tool-list-index (min max-index
                                       (1+ e-chat-transcript--tool-list-index)))
    (e-chat-transcript--focus-tool-list-item)))

(defun e-chat-tool-list-previous ()
  "Focus the previous tool call in the active tool list."
  (interactive)
  (setq e-chat-transcript--tool-list-index (max 0 (1- e-chat-transcript--tool-list-index)))
  (e-chat-transcript--focus-tool-list-item))

(defun e-chat-tool-list-open-output ()
  "Open the selected tool output in a read-only buffer."
  (interactive)
  (let* ((block (e-chat-transcript--tool-list-block))
         (item (nth e-chat-transcript--tool-list-index (plist-get block :tool-items)))
         (output (or (plist-get item :output) ""))
         (origin (current-buffer)))
    (let ((buffer (get-buffer-create e-chat-tool-output-buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert output))
        (e-chat-tool-output-mode)
        (setq e-chat-transcript--tool-output-origin-buffer origin)
        (goto-char (point-min)))
      (display-buffer buffer)
      buffer)))

(defun e-chat-tool-list-back ()
  "Collapse the tool list and return to block navigation."
  (interactive)
  (let* ((block (e-chat-transcript--tool-list-block))
         (block-id (plist-get block :id)))
    (e-chat-transcript--delete-tool-list block)
    (e-chat-tool-list-mode -1)
    (e-chat-response-navigation-mode 1)
    (e-chat-transcript--focus-block block-id)))

(defun e-chat-tool-output-back ()
  "Close tool output and return to its originating tool list."
  (interactive)
  (let ((origin e-chat-transcript--tool-output-origin-buffer)
        (buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (when (buffer-live-p origin)
      (pop-to-buffer origin))))

(defun e-chat-transcript--message-entry (message)
  "Return a rendered entry for durable MESSAGE."
  (let ((role (plist-get message :role))
        (content (plist-get message :content)))
    (pcase role
      ('user (cons "You" content))
      ('assistant (cons "Assistant" content))
      ('tool (cons "Tool" (format "%S" content)))
      (_ (cons (format "%s" role) (format "%S" content))))))

(defun e-chat-transcript--hidden-message-entry (message)
  "Return a dimmed audit entry for hidden durable MESSAGE."
  (let ((role (plist-get message :role))
        (content (plist-get message :content)))
    (cons (pcase role
            ('assistant (concat e-chat-transcript--hidden-entry-title-prefix
                                " · superseded answer"))
            ('user (concat e-chat-transcript--hidden-entry-title-prefix
                           " · calibration prompt"))
            (_ (format "%s · %s"
                       e-chat-transcript--hidden-entry-title-prefix role)))
          (if (stringp content) content (format "%S" content)))))


(defun e-chat-transcript--render-durable-message
    (message turn-id &optional ensure-composer details-text)
  "Render durable MESSAGE for TURN-ID and return its entry data.
Hidden messages remain in the buffer-local message projection but are made
invisible in the normal reading view.  Audit reveal mode deliberately uses its
separate dimmed representation instead."
  (let* ((hidden (e-harness-message-hidden-p message))
         (presentation
          (and (eq (plist-get message :role) 'assistant)
               (not hidden)
               (e-chat-service-message-presentation
                e-chat-harness e-chat-session-id message)))
         (entry (if (and hidden e-chat-transcript--reveal-hidden)
                    (e-chat-transcript--hidden-message-entry message)
                  (if presentation
                      (cons "Assistant" (plist-get presentation :content))
                    (e-chat-transcript--message-entry message)))))
    (e-chat-transcript--insert-entry
     (car entry) (cdr entry) ensure-composer turn-id
     details-text
     (plist-get message :id)
     (and hidden (not e-chat-transcript--reveal-hidden))
     (and presentation t))
    entry))

(defun e-chat-transcript--tail-messages (messages limit)
  "Return at most LIMIT trailing MESSAGES."
  (if (and (integerp limit)
           (> limit 0)
           (> (length messages) limit))
      (nthcdr (- (length messages) limit) messages)
    messages))

(cl-defun e-chat-transcript--render-session
    (&optional (messages nil messages-supplied-p)
               (replay-activity-events nil replay-activity-events-supplied-p))
  "Render the attached session transcript in the current buffer.
When MESSAGES is supplied, render that message list instead of the
attached session's full transcript.  REPLAY-ACTIVITY-EVENTS is the matching
service snapshot when supplied."
  (let* ((messages (if messages-supplied-p
                       messages
                     (e-chat-service-messages
                      e-chat-harness e-chat-session-id)))
         (turn-index 0)
         turn-id)
    (ignore replay-activity-events replay-activity-events-supplied-p)
    (dolist (message messages)
      (when (or (plist-get message :turn-id)
                (not turn-id)
                (eq (plist-get message :role) 'user))
        (let ((next-turn-id
               (or (plist-get message :turn-id)
                   (format "replayed-turn-%d" (1+ turn-index)))))
          (setq turn-index (1+ turn-index))
          (setq turn-id next-turn-id)))
      (let* ((message-selected-p
              (e-chat-transcript--message-selected-participant-p message))
             (render-turn-id
              (if message-selected-p
                  turn-id
                (e-chat-transcript--observed-turn-id turn-id message))))
        (unless (eq (plist-get message :role) 'tool)
          ;; Tool messages are model context; activity owns their compact
          ;; presentation.  Durable user/assistant/system rows remain wholly
          ;; transcript-owned.
          (e-chat-transcript--render-durable-message
           message render-turn-id message-selected-p))))))

(defun e-chat-transcript--activity-bounds ()
  "Return the current transient activity projection bounds, or nil."
  (let ((start (and (markerp e-chat-transcript--activity-start-marker)
                    (marker-position e-chat-transcript--activity-start-marker)))
        (end (and (markerp e-chat-transcript--activity-end-marker)
                  (marker-position e-chat-transcript--activity-end-marker))))
    (when (and start end (< start end))
      (cons start end))))

(defun e-chat-transcript--activity-progress-bounds ()
  "Return the mutable progress-tail bounds, or nil."
  (let ((start (and (markerp e-chat-transcript--activity-progress-start-marker)
                    (marker-position
                     e-chat-transcript--activity-progress-start-marker)))
        (end (and (markerp e-chat-transcript--activity-progress-end-marker)
                  (marker-position
                   e-chat-transcript--activity-progress-end-marker))))
    (when (and start end (< start end))
      (cons start end))))

(defun e-chat-transcript--copy-activity-display-properties
    (start text &optional text-start text-end)
  "Copy display properties from activity TEXT into the buffer at START.
When TEXT-START and TEXT-END are supplied, update only that changed slice of
TEXT.  Existing properties outside the slice belong to the unchanged
projection and must remain untouched during a progress redraw."
  (let* ((text-start (or text-start 0))
         (text-end (or text-end (length text)))
         (index text-start)
         (limit text-end))
    (while (< index limit)
      (let* ((next (min limit (or (next-property-change index text) limit)))
             (display (get-text-property index 'display text))
             (buffer-start (+ start index))
             (buffer-end (+ start next)))
        (if display
            (add-text-properties buffer-start buffer-end `(display ,display))
          (remove-text-properties buffer-start buffer-end '(display nil)))
        (setq index next)))))

(defun e-chat-transcript--common-prefix-length (old-text new-text)
  "Return common prefix length for OLD-TEXT and NEW-TEXT."
  (let ((index 0)
        (limit (min (length old-text) (length new-text))))
    (while (and (< index limit)
                (= (aref old-text index) (aref new-text index)))
      (setq index (1+ index)))
    index))

(defun e-chat-transcript--common-suffix-length
    (old-text new-text prefix-length)
  "Return common suffix length after PREFIX-LENGTH has been reserved."
  (let* ((old-length (length old-text))
         (new-length (length new-text))
         (limit (min (- old-length prefix-length)
                     (- new-length prefix-length)))
         (suffix 0))
    (while (and (< suffix limit)
                (= (aref old-text (- old-length suffix 1))
                   (aref new-text (- new-length suffix 1))))
      (setq suffix (1+ suffix)))
    suffix))

(defun e-chat-transcript--replace-activity-region-bounded
    (start end new-text)
  "Replace activity START through END with NEW-TEXT under bounded cost."
  (let ((marker (copy-marker end t)))
    (unwind-protect
        (progn
          (replace-region-contents
           start end
           (lambda () new-text)
           e-chat-running-status-diff-max-seconds
           (length new-text))
          (marker-position marker))
      (set-marker marker nil))))

(defun e-chat-transcript--replace-activity-region-diffing
    (start end old-text new-text)
  "Replace activity START through END, touching only changed text."
  (let* ((prefix-length
          (e-chat-transcript--common-prefix-length old-text new-text))
         (suffix-length
          (e-chat-transcript--common-suffix-length
           old-text new-text prefix-length))
         (replace-start (+ start prefix-length))
         (replace-end (- end suffix-length))
         (new-replace-end (- (length new-text) suffix-length)))
    (unless (and (= replace-start replace-end)
                 (= prefix-length new-replace-end))
      (goto-char replace-start)
      (delete-region replace-start replace-end)
      (insert (substring new-text prefix-length new-replace-end)))
    (+ start (length new-text))))

(defun e-chat-transcript--replace-activity-region
    (start end new-text)
  "Replace activity START through END with NEW-TEXT."
  (let ((old-text (buffer-substring-no-properties start end)))
    (cond
     ((string= old-text new-text) end)
     ((>= (max (length old-text) (length new-text))
          e-chat-running-status-diff-max-chars)
      (e-chat-transcript--replace-activity-region-bounded
       start end new-text))
     (t
      (e-chat-transcript--replace-activity-region-diffing
       start end old-text new-text)))))

(defun e-chat-transcript--activity-display-text (data)
  "Return display text represented by semantic activity DATA."
  (let ((prefix (or (plist-get data :prefix-text) ""))
        (text (plist-get data :display-text)))
    (cond
     ;; Activity DATA's :display-text is already the complete semantic
     ;; projection; :prefix-text is retained only for the progress-tail text
     ;; and the no-text spinner case.
     (text text)
     ((or (not (string-empty-p prefix))
          (plist-get data :progress-p))
      (concat prefix
              (when (plist-get data :progress-p)
                (e-chat-transcript--entry-text
                 "Assistant"
                 (or (plist-get data :progress-glyph) "Thinking...")))))
     (t nil))))

(defun e-chat-transcript--activity-block-id (turn-id)
  "Return the display block id for TURN-ID's activity projection."
  (if (and e-chat-transcript--activity-block-id
           (equal turn-id e-chat-transcript--activity-turn-id))
      e-chat-transcript--activity-block-id
    (setq e-chat-transcript--activity-block-id
          (e-chat-transcript--next-block-id))
    (setq e-chat-transcript--activity-turn-id turn-id)
    e-chat-transcript--activity-block-id))

(defun e-chat-transcript--activity-progress-range (data text)
  "Return the progress range for semantic activity DATA and TEXT.
Activity identifies a live tail by its text, not by buffer offsets.  This
owner resolves that value into positions only while applying its own
projection, keeping marker and bounds representation private to transcript."
  (when (and text (plist-get data :progress-p))
    (let ((tail (plist-get data :progress-tail))
          (prefix (or (plist-get data :prefix-text) ""))
          start)
      (when (and (stringp tail) (not (string-empty-p tail)))
        (let ((from 0))
          (while (string-match (regexp-quote tail) text from)
            (setq start (match-beginning 0)
                  from (1+ (match-beginning 0))))))
      (setq start (or start (length prefix)))
      (cons start (length text)))))

(defun e-chat-transcript--apply-activity-markdown-lines (start end)
  "Apply deterministic Markdown to activity lines touched by START through END.
Expanding to complete lines keeps delimiters paired when a streamed update
changes only a suffix.  Activity projections are compact and bounded, while
unchanged lines remain untouched during ordinary progress ticks.  This hot UI
path deliberately avoids instantiating a major mode or running mode hooks."
  (when (< start end)
    (let ((line-start
           (save-excursion
             (goto-char start)
             (line-beginning-position)))
          (line-end
           (save-excursion
             (goto-char (1- end))
             (min (point-max) (1+ (line-end-position))))))
      ;; Activity text must be stable across user `markdown-mode'
      ;; configurations and cheap enough for progress-timer redraws.  Clear
      ;; properties from the touched lines, then use only E's bounded
      ;; deterministic presenter.  In particular, reasoning status delimiters
      ;; remain presentation syntax rather than visible transcript content.
      (e-chat-clear-markdown-presentation line-start line-end)
      (e-chat-transcript--apply-deterministic-markdown line-start line-end)
      ;; Restore the activity base face as the background style while
      ;; preserving strong/code/link faces on top of it.
      (add-text-properties
       line-start line-end '(font-lock-face e-chat-system-face)))))

(defun e-chat-transcript--apply-activity-region
    (turn-id data start end &optional property-start property-end)
  "Apply semantic activity DATA to the transcript region START through END."
  (setq e-chat-transcript--activity-turn-id turn-id)
  (setq e-chat-transcript--activity-start-marker
        (copy-marker start nil))
  (setq e-chat-transcript--activity-end-marker
        (copy-marker end nil))
  (let* ((text (e-chat-transcript--activity-display-text data))
         (property-start (or property-start 0))
         (property-end (or property-end (length text)))
         (property-buffer-start (+ start property-start))
         (property-buffer-end (+ start property-end)))
    (when-let ((progress-range
                (e-chat-transcript--activity-progress-range data text)))
      (setq e-chat-transcript--activity-progress-start-marker
            (copy-marker (+ start (car progress-range)) nil))
      (setq e-chat-transcript--activity-progress-end-marker
            (copy-marker (+ start (cdr progress-range)) nil)))
    (when (< property-buffer-start property-buffer-end)
      (e-chat-transcript--copy-activity-display-properties
       start text property-start property-end)
      (e-chat-transcript--mark-protected property-buffer-start property-buffer-end)
      (remove-text-properties
       property-buffer-start property-buffer-end
       '(font-lock-face nil
         e-chat-progress-turn-id nil
         e-chat-transient-turn-id nil
         e-chat-turn-id nil
         e-chat-block-id nil))
      (if-let ((data-text (plist-get data :display-text)))
          (let* ((block-id (e-chat-transcript--activity-block-id turn-id))
                 (properties `(e-chat-transient-turn-id ,turn-id
                               e-chat-turn-id ,turn-id
                               e-chat-block-id ,block-id)))
            (add-text-properties
             property-buffer-start property-buffer-end
             `(font-lock-face e-chat-system-face ,@properties))
            (e-chat-transcript--apply-activity-markdown-lines
             property-buffer-start property-buffer-end)
            (e-chat-transcript--apply-activity-separator-face
             property-buffer-start property-buffer-end))
        (add-text-properties
         property-buffer-start property-buffer-end
         `(font-lock-face e-chat-assistant-face
           e-chat-progress-turn-id ,turn-id))))
    ;; Block metadata describes the whole semantic projection, so refresh it
    ;; even when a redraw changed only activity details or display properties.
    (when-let ((data-text (plist-get data :display-text)))
      ;; Activity owns the source records.  Keep transcript block metadata
      ;; detached from the semantic descriptor lists so a later activity
      ;; update cannot mutate a transcript-owned projection by aliasing it.
      (let ((block-id (e-chat-transcript--activity-block-id turn-id))
            (tools (copy-tree (plist-get data :tools)))
            (children (copy-tree (plist-get data :children))))
        (e-chat-transcript--update-block-bounds
         block-id turn-id start end
         (plist-get data :kind)
         (string-trim-right data-text)
         start end
         tools
         (plist-get data :details))
        (let ((record (e-chat-transcript--block-record block-id turn-id)))
          (plist-put record :details-text (plist-get data :details))
          (plist-put record :tool-items tools)
          (plist-put record :child-records children)))))
  (e-chat-surface-set-running-status-bounds
   (e-chat-transcript--activity-bounds)))

(defun e-chat-transcript--delete-activity-projection ()
  "Delete the current transient activity projection and its metadata."
  (when-let ((bounds (e-chat-transcript--activity-bounds)))
    (let ((inhibit-read-only t))
      (delete-region (car bounds) (cdr bounds))))
  (when e-chat-transcript--activity-block-id
    (e-chat-transcript--remove-block-record
     e-chat-transcript--activity-block-id))
  (setq e-chat-transcript--activity-block-id nil
        e-chat-transcript--activity-turn-id nil
        e-chat-transcript--activity-start-marker nil
        e-chat-transcript--activity-end-marker nil
        e-chat-transcript--activity-progress-start-marker nil
        e-chat-transcript--activity-progress-end-marker nil)
  (e-chat-surface-set-running-status-bounds nil))

(defun e-chat-transcript--render-activity-projection (turn-id data)
  "Render semantic activity DATA for TURN-ID in the current transcript.
Return the size of the resulting projection as a scalar.  The transcript
keeps block ids, markers, protection, and navigation state private; callers
must not need to inspect its representation to schedule their own work."
  (let* ((text (e-chat-transcript--activity-display-text data))
         (bounds (e-chat-transcript--activity-bounds))
         (same-turn (equal turn-id e-chat-transcript--activity-turn-id))
         (navigation-state
          (e-chat-transcript--capture-navigation-state))
         (display-state
          (e-chat-surface-capture-running-status-display-state))
         (initial-tail-windows
          (unless display-state
            (e-chat-surface-capture-output-tail-windows))))
    (if (and same-turn bounds text)
        (let* ((inhibit-read-only t)
               (start (car bounds))
               (end (cdr bounds))
               (old-text (buffer-substring-no-properties start end))
               (prefix-length
                (e-chat-transcript--common-prefix-length old-text text))
               (suffix-length
                (e-chat-transcript--common-suffix-length
                 old-text text prefix-length))
               (property-start prefix-length)
               (property-end (- (length text) suffix-length))
               new-end)
          ;; The bounded fallback may rewrite the complete region.  In that
          ;; case refresh properties across the replacement; ordinary
          ;; progress ticks keep the unchanged prefix/suffix untouched.
          (when (>= (max (length old-text) (length text))
                    e-chat-running-status-diff-max-chars)
            (setq property-start 0
                  property-end (length text)))
          (setq new-end
                (e-chat-transcript--replace-activity-region
                 start end text))
          (e-chat-transcript--apply-activity-region
           turn-id data start new-end property-start property-end))
      (e-chat-transcript--delete-activity-projection)
      (when text
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (or (bobp) (bolp))
            (insert "\n"))
          (e-chat-transcript--maybe-insert-response-separator turn-id 'agent)
          (let ((start (point)))
            (insert text)
            (e-chat-transcript--apply-activity-region
             turn-id data start (point))))))
    (e-chat-transcript--restore-navigation-state navigation-state)
    (unless navigation-state
      (if display-state
          (e-chat-surface-restore-running-status-display-state display-state)
        (e-chat-surface-restore-output-tail-windows initial-tail-windows)))
    (length (or text ""))))

(defun e-chat-transcript--update-message-details (message-id details)
  "Update DETAILS on the rendered durable MESSAGE-ID projection."
  (when-let* ((block-id (e-chat-transcript--message-block-id message-id))
              (record (and (hash-table-p e-chat-transcript--block-registry)
                           (gethash block-id e-chat-transcript--block-registry))))
    (plist-put record :details-text details)
    t))



;;; Public transcript contract

(defun e-chat-transcript-event-selected-participant-p (event)
  "Return whether projected EVENT belongs to this chat's participant."
  (e-chat-transcript--event-selected-participant-p event))

(defun e-chat-transcript-message-selected-participant-p (message)
  "Return whether projected MESSAGE belongs to this chat's participant."
  (e-chat-transcript--message-selected-participant-p message))

(defun e-chat-transcript-observed-turn-id (turn-id event)
  "Return the isolated presentation id for unselected EVENT."
  (e-chat-transcript--observed-turn-id turn-id event))

(defun e-chat-transcript-presentation-turn-id (turn-id event)
  "Return EVENT's selected or isolated presentation id."
  (e-chat-transcript--presentation-turn-id turn-id event))

(defun e-chat-transcript-session-summary-preview (session)
  "Return bounded summary text for SESSION metadata."
  (e-chat-transcript--session-summary-preview session))

(defun e-chat-transcript-session-replay-message-count (messages)
  "Return the bounded replay count for MESSAGES."
  (e-chat-transcript--session-replay-message-count messages))

(defun e-chat-transcript-validated-replay-limit (value option)
  "Validate positive replay LIMIT VALUE for OPTION."
  (e-chat-transcript--validated-replay-limit value option))

(defun e-chat-transcript-render-session (&optional messages activity-events)
  "Render the attached transcript, optionally from bounded snapshots."
  (if messages
      (e-chat-transcript--render-session messages activity-events)
      (e-chat-transcript--render-session)))

(defun e-chat-transcript-render-session-loading (session)
  "Render cheap loading state for unloaded indexed SESSION."
  (when-let ((summary (e-chat-transcript--session-summary-preview session)))
    (unless (string-empty-p summary)
      (e-chat-transcript--insert-entry "You" summary nil)))
  (e-chat-transcript--insert-protected
   (format "%s Loading transcript...\n\n" e-chat-transcript--system-glyph)
   'e-chat-activity-face))

(defun e-chat-transcript-system-glyph ()
  "Return the system glyph used by transcript-owned compact messages."
  e-chat-transcript--system-glyph)

(defun e-chat-transcript-user-glyph ()
  "Return the durable user-entry glyph."
  e-chat-transcript--user-glyph)

(defun e-chat-transcript-assistant-glyph ()
  "Return the durable assistant-entry glyph."
  e-chat-transcript--assistant-glyph)

(defun e-chat-transcript-turn-separator ()
  "Return the configured separator between independent turns."
  e-chat-transcript--turn-separator)

(defun e-chat-transcript-response-separator ()
  "Return the configured separator between prompt and response entries."
  e-chat-transcript--response-separator)

(defun e-chat-transcript-render-replay (messages &optional activity-events)
  "Render bounded transcript replay from MESSAGES and ACTIVITY-EVENTS."
  (e-chat-transcript--render-session-replay messages activity-events))

(defun e-chat-transcript-rerender ()
  "Rebuild the current transcript while preserving its paired composer."
  (e-chat-transcript--rerender-transcript))

(defun e-chat-transcript-rerender-assistant-blocks ()
  "Rerender assistant blocks through the transcript owner."
  (e-chat-transcript--rerender-assistant-blocks))

(defun e-chat-transcript-insert-protected (text &optional face properties)
  "Insert protected transcript TEXT with optional FACE and PROPERTIES."
  (e-chat-transcript--insert-protected text face properties))

(defun e-chat-transcript-insert-formatted-assistant (text)
  "Insert assistant TEXT with the transcript's settled presentation.

This is the embedding-shell boundary for short assistant answers (for
example, the chat starter).  Markdown/font presentation and the final face
remain transcript-owned; callers do not pass buffer positions to individual
formatting helpers."
  (let ((start (point))
        (inhibit-read-only t))
    (insert (string-trim-right (or text "")) "\n")
    (e-chat-transcript--apply-assistant-markdown start (point))
    (e-chat-transcript--apply-final-assistant-face start (point))
    (point)))

(defun e-chat-transcript-insert-activity-entry (text)
  "Insert an activity TEXT entry with transcript-owned presentation."
  (let ((start (point)))
    (e-chat-transcript--insert-protected text 'e-chat-system-face)
    (e-chat-transcript--apply-activity-separator-face start (point))
    (point)))

(defun e-chat-transcript-reset ()
  "Reset buffer-local transcript projection and navigation state."
  (dolist (variable '(e-chat-transcript--turn-registry
                      e-chat-transcript--block-registry
                      e-chat-transcript--message-block-index))
    (set (make-local-variable variable) (make-hash-table :test 'equal)))
  (setq-local e-chat-transcript--block-order nil
              e-chat-transcript--block-counter 0
              e-chat-transcript--focused-turn-id nil
              e-chat-transcript--focused-block-id nil
              e-chat-transcript--latest-final-block-id nil
              e-chat-transcript--last-rendered-turn-id nil
              e-chat-transcript--last-rendered-side nil
              e-chat-transcript--block-view-block-id nil
              e-chat-transcript--tool-list-block-id nil
              e-chat-transcript--tool-list-index 0
              e-chat-transcript--tool-list-overlay nil
              e-chat-transcript--tool-output-origin-buffer nil)
  t)

(defun e-chat-transcript-block-at-point (&optional position)
  "Return the rendered block id at POSITION in the current transcript."
  (if position
      (or (get-text-property position 'e-chat-block-id)
          (get-text-property (max (point-min) (1- position)) 'e-chat-block-id))
    (e-chat-transcript--block-at-point)))

(defun e-chat-transcript-turn-id-at-point (&optional position)
  "Return the semantic turn id rendered at POSITION, or nil.
This query deliberately exposes only identity; transcript block records stay
private to the navigation owner."
  (let ((position (or position (point))))
    (or (get-text-property position 'e-chat-turn-id)
        (and (> position (point-min))
             (get-text-property (1- position) 'e-chat-turn-id)))))

(defun e-chat-transcript-focused-block (&optional buffer)
  "Return semantic metadata for the block focused in BUFFER.
The projection contains display text, kind, turn identity, detail visibility,
and child kinds only.  Mutable block records, markers, and registries remain
private to the transcript owner."
  (with-current-buffer (or buffer (current-buffer))
    (when-let ((block
                (or (ignore-errors (e-chat-transcript--focused-block))
                    (and (e-chat-transcript--block-at-point)
                         (gethash (e-chat-transcript--block-at-point)
                                  e-chat-transcript--block-registry)))))
      (let ((children (plist-get block :children)))
        (let* ((bounds (e-chat-transcript--block-entry-bounds block))
               (display-text
                (and bounds
                     (buffer-substring-no-properties
                      (car bounds) (cdr bounds)))))
          (list :block-id (plist-get block :id)
                :turn-id (plist-get block :turn-id)
                :kind (plist-get block :kind)
                :action-text (e-chat-transcript--block-action-text block)
                :display-text display-text
                :details-text (plist-get block :details-text)
                :details-visible-p (and (e-chat-transcript--block-details-visible-p block) t)
                :display-hidden-p (and (e-chat-transcript--block-display-hidden-p block) t)
                :tool-count (length (plist-get block :tool-items))
                :child-count (length children)
                :child-kinds
                (mapcar
                 (lambda (child-id)
                   (when-let ((child (e-chat-transcript--live-block-record child-id)))
                     (list :kind (plist-get child :kind)
                           :action-text
                           (e-chat-transcript--block-action-text child)
                           :display-text
                           (let ((child-bounds
                                  (e-chat-transcript--block-entry-bounds child)))
                             (and child-bounds
                                  (buffer-substring-no-properties
                                   (car child-bounds) (cdr child-bounds))))
                           :turn-id (plist-get child :turn-id))))
                 children)))))))

(defun e-chat-transcript-message-hidden-p (message-id &optional buffer)
  "Return non-nil when MESSAGE-ID is hidden in BUFFER's transcript projection."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((block-id (e-chat-transcript--message-block-id message-id))
                (block (and (hash-table-p e-chat-transcript--block-registry)
                            (gethash block-id e-chat-transcript--block-registry)))
                (bounds (e-chat-transcript--block-layout-bounds block)))
      (and (e-chat-transcript--block-display-hidden-p block)
           ;; The overlay is an implementation detail and may not be active in
           ;; a batch redisplay context.  The semantic projection state is the
           ;; authoritative answer for callers.
           bounds
           t))))

(defun e-chat-transcript-focused-turn-id (&optional buffer)
  "Return the focused navigation turn identity in BUFFER, or nil.
When no explicit navigation focus has been captured, report the semantic turn
represented at point.  This keeps the query useful for standalone transcript
owners while preserving an explicit focus transition when one exists."
  (with-current-buffer (or buffer (current-buffer))
    (or e-chat-transcript--focused-turn-id
        (e-chat-transcript-turn-id-at-point))))

(defun e-chat-transcript-reveal-hidden-p (&optional buffer)
  "Return whether BUFFER currently reveals hidden transcript entries."
  (with-current-buffer (or buffer (current-buffer))
    (and e-chat-transcript--reveal-hidden t)))

(defun e-chat-transcript-insert-entry
    (title content &optional ensure-composer turn-id details-text message-id
           hidden assistant-presented)
  "Insert one protected transcript entry through the transcript owner."
  (e-chat-transcript--insert-entry
   title content ensure-composer turn-id details-text message-id hidden
   assistant-presented))

(defun e-chat-transcript-render-durable-message
    (message turn-id &optional ensure-composer details-text)
  "Render durable MESSAGE as a transcript block for TURN-ID."
  (e-chat-transcript--render-durable-message
   message turn-id ensure-composer details-text))

(defun e-chat-transcript-reconcile-message-display (message)
  "Apply MESSAGE's current visibility/details to its rendered block."
  (e-chat-transcript--reconcile-message-display message))

(defun e-chat-transcript-cancel-markdown-presentation (&optional buffer)
  "Cancel deferred Markdown presentation in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-transcript--cancel-pending-markdown-presentation)))

(defun e-chat-transcript-set-preview-p (preview &optional buffer)
  "Set whether BUFFER is a transient transcript PREVIEW."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local e-chat-transcript--preview-buffer (and preview t))))

(defun e-chat-transcript-preview-p (&optional buffer)
  "Return non-nil when BUFFER is a transient transcript preview."
  (with-current-buffer (or buffer (current-buffer))
    e-chat-transcript--preview-buffer))

(defun e-chat-transcript-project-activity (turn-id data)
  "Render semantic transient activity DATA for TURN-ID.
DATA contains display text, progress-tail text, progress state, semantic block
kind, tool descriptors, child descriptors, and optional details.  It is a
value projection, not a transcript record: the transcript owner keeps all
markers, block ids, protection, and navigation restoration internal; activity
callers never receive that mutable representation."
  (e-chat-transcript--render-activity-projection turn-id data))

(defun e-chat-transcript-render-activity-notice
    (text &optional ensure-composer turn-id details-text)
  "Render activity-owned TEXT as a durable System notice.

Activity owns the decision and state for failure/cancellation notices.  This
semantic transcript port owns insertion order, protection, block metadata, and
composer restoration without exposing those representation details to the
activity owner."
  (e-chat-transcript--insert-entry
   "System" text ensure-composer turn-id details-text))

(defun e-chat-transcript-remove-activity ()
  "Delete the current transient activity projection."
  (e-chat-transcript--delete-activity-projection))

(defun e-chat-transcript-update-message-details (message-id details)
  "Update DETAILS on the durable projection for MESSAGE-ID."
  (e-chat-transcript--update-message-details message-id details))

(defun e-chat-transcript-refresh-keymaps ()
  "Refresh transcript navigation and tool-view keymaps after reload."
  (setq e-chat-response-navigation-mode-map
        (e-chat-transcript--make-response-navigation-mode-map
         e-chat-response-navigation-mode-map))
  (setq e-chat-block-view-mode-map
        (e-chat-transcript--make-block-view-mode-map
         e-chat-block-view-mode-map))
  (setq e-chat-tool-list-mode-map
        (e-chat-transcript--make-tool-list-mode-map
         e-chat-tool-list-mode-map))
  (setq e-chat-tool-output-mode-map
        (e-chat-transcript--make-tool-output-mode-map
         e-chat-tool-output-mode-map)))

(defun e-chat-transcript-has-navigable-blocks-p (&optional position)
  "Return non-nil when the transcript has a block usable for navigation."
  (or (e-chat-transcript-block-at-point position)
      (e-chat-transcript--last-rendered-block-id)))

(defun e-chat-transcript-leave-navigation ()
  "Leave transcript navigation modes and collapse audit-only presentation."
  (when (region-active-p)
    (deactivate-mark t))
  (when e-chat-tool-list-mode
    (e-chat-tool-list-mode -1))
  (when e-chat-block-view-mode
    (e-chat-block-view-mode -1))
  (when e-chat-response-navigation-mode
    (e-chat-response-navigation-mode -1))
  (when e-chat-transcript--reveal-hidden
    (setq e-chat-transcript--reveal-hidden nil)
    (e-chat-transcript--rerender-transcript)))

(defun e-chat-transcript--capture-navigation-state ()
  "Capture transcript navigation state before an activity redraw."
  (cond
   (e-chat-tool-list-mode
    (list :mode 'tool-list
          :block-id e-chat-transcript--tool-list-block-id
          :index e-chat-transcript--tool-list-index))
   (e-chat-block-view-mode
    (let* ((block-id e-chat-transcript--block-view-block-id)
           (block (e-chat-transcript--live-block-record block-id))
           (bounds (and block (e-chat-transcript--block-view-bounds block))))
      (list :mode 'block-view
            :block-id block-id
            :offset (and bounds (max 0 (- (point) (car bounds)))))))
   (e-chat-response-navigation-mode
    (list :mode 'response-navigation
          :block-id e-chat-transcript--focused-block-id))))

(defun e-chat-transcript--restore-navigation-state (state)
  "Restore transcript navigation STATE after an activity redraw."
  (pcase (plist-get state :mode)
    ('response-navigation
     (when (e-chat-transcript--live-block-record (plist-get state :block-id))
       (e-chat-response-navigation-mode 1)
       (e-chat-transcript--focus-block (plist-get state :block-id))))
    ('block-view
     (let* ((block-id (plist-get state :block-id))
            (block (e-chat-transcript--live-block-record block-id))
            (bounds (and block (e-chat-transcript--block-view-bounds block))))
       (when bounds
         (e-chat-response-navigation-mode -1)
         (setq e-chat-transcript--focused-block-id block-id)
         (setq e-chat-transcript--focused-turn-id (plist-get block :turn-id))
         (setq e-chat-transcript--block-view-block-id block-id)
         (e-chat-block-view-mode 1)
         (goto-char (min (cdr bounds)
                         (+ (car bounds)
                            (or (plist-get state :offset) 0)))))))
    ('tool-list
     (let* ((block-id (plist-get state :block-id))
            (block (e-chat-transcript--live-block-record block-id))
            (items (and block (plist-get block :tool-items))))
       (cond
        (items
         (let ((index (min (max 0 (or (plist-get state :index) 0))
                           (1- (length items)))))
           (e-chat-transcript--open-tool-list block)
           (setq e-chat-transcript--tool-list-index index)
           (e-chat-transcript--focus-tool-list-item)))
        (block
         (e-chat-response-navigation-mode 1)
         (e-chat-transcript--focus-block block-id)))))))

(provide 'e-chat-transcript)

;;; e-chat-transcript.el ends here
