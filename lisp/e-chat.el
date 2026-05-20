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
(require 'subr-x)
(require 'e-emacs-base)
(require 'e-harness)
(require 'e-openai)
(require 'e-session)

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

(defcustom e-chat-submit-backend-delay 0.05
  "Seconds to delay backend work after rendering a submitted human turn."
  :type 'number
  :group 'e-chat)

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

(defun e-chat--apply-owned-face-defaults ()
  "Apply face defaults that should update during live reload."
  (set-face-attribute 'e-chat-separator-face nil
                      :foreground "#7f8a99"
                      :background "#202833"
                      :extend t))

(e-chat--apply-owned-face-defaults)

(defface e-chat-title-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1 :extend t))
  "Face used for the chat buffer title."
  :group 'e-chat)

(defface e-chat-focused-turn-face
  '((t :inherit highlight :extend t))
  "Face used for the focused turn in response navigation mode."
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

(defconst e-chat--system-face-spec
  '((t :inherit default
       :foreground "#ded2ec"
       :background "#312b3c"
       :extend t))
  "Default face spec for compact system chat blocks.")

(defun e-chat--refresh-face-specs ()
  "Refresh chat face defaults after live reload."
  (face-spec-set 'e-chat-user-face e-chat--user-face-spec)
  (face-spec-set 'e-chat-assistant-face e-chat--assistant-face-spec)
  (face-spec-set 'e-chat-system-face e-chat--system-face-spec))

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

(defconst e-chat--user-glyph ">"
  "Glyph shown before user-authored chat blocks.")

(defconst e-chat--assistant-glyph "●"
  "Glyph shown before assistant chat blocks.")

(defconst e-chat--system-glyph "·"
  "Glyph shown before compact system chat blocks.")

(defconst e-chat--composer-glyph "❯ "
  "Glyph shown before editable e chat composer text.")

(defconst e-chat--composer-separator
  "────────────────────────────────────────────────────────────────"
  "Separator shown above the e chat composer.")

(defconst e-chat--title "E Agent Session"
  "Title shown at the top of e chat buffers.")

(defconst e-chat--reasoning-effort-values
  '("" "minimal" "low" "medium" "high" "xhigh")
  "Reasoning effort values offered by the chat presentation.")

(defvar e-chat--test-window-body-height nil
  "Test override for the visible chat window height.")

(defvar e-chat--test-transcript-screen-lines nil
  "Test override for transcript screen-line height.")

(defvar e-chat--refresh-visible-composers-in-progress nil
  "Non-nil while visible e chat composers are being refreshed.")

(defvar e-chat--default-sessions nil
  "Default persistent session store used by chat commands.")

(defconst e-chat--protected-properties
  '(read-only t
    e-chat-protected t
    front-sticky (read-only e-chat-protected field)
    rear-nonsticky (read-only e-chat-protected field)
    field e-chat-transcript)
  "Text properties applied to protected e chat presentation text.")

(defun e-chat--make-response-navigation-mode-map ()
  "Return the keymap for `e-chat-response-navigation-mode'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'e-chat-response-navigation-next)
    (define-key map (kbd "k") #'e-chat-response-navigation-previous)
    (define-key map (kbd "RET") #'e-chat-response-navigation-expand)
    (define-key map (kbd "i") #'e-chat-response-navigation-insert)
    map))

(defvar e-chat-response-navigation-mode-map
  (e-chat--make-response-navigation-mode-map)
  "Keymap for response navigation inside `e-chat-mode'.")

(defun e-chat--make-mode-map (&optional map)
  "Return MAP configured as the local keymap for `e-chat-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "<escape>") #'e-chat-enter-response-navigation)
    (define-key map (kbd "C-c C-c") #'e-chat-submit)
    (define-key map (kbd "RET") #'newline)
    (define-key map (kbd "C-c C-k") #'e-chat-abort)
    (define-key map (kbd "C-c C-r") #'e-chat-reset)
    (define-key map (kbd "C-c C-x") #'e-chat-show-context)
    map))

(defvar e-chat-mode-map (e-chat--make-mode-map)
  "Keymap for `e-chat-mode'.")

(setq e-chat-mode-map (e-chat--make-mode-map e-chat-mode-map))

(define-derived-mode e-chat-mode text-mode "e-chat"
  "Major mode for e chat buffers.")

(define-minor-mode e-chat-response-navigation-mode
  "Navigate rendered turn blocks in an e chat buffer."
  :lighter " Nav"
  :keymap e-chat-response-navigation-mode-map
  (unless e-chat-response-navigation-mode
    (setq e-chat--focused-turn-id nil)
    (setq e-chat--focused-block-id nil)
    (when (overlayp e-chat--focused-turn-overlay)
      (delete-overlay e-chat--focused-turn-overlay))))

(defun e-chat--disable-modal-editing ()
  "Disable local modal editing state for the chat buffer when available."
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1)))

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

(defun e-chat--default-harness ()
  "Create the default Codex-backed chat harness."
  (let ((harness (e-openai-create-harness
                  :provider e-openai-default-provider
                  :sessions (e-chat--default-session-store))))
    (e-harness-activate-layer harness (e-emacs-base-layer-create))
    harness))

(defun e-chat--default-session-store ()
  "Return the default persistent chat session store."
  (let ((directory (file-name-as-directory
                    (expand-file-name e-session-directory))))
    (unless (and (e-session-store-p e-chat--default-sessions)
                 (equal (e-session-store-directory e-chat--default-sessions)
                        directory))
      (setq e-chat--default-sessions
            (e-session-persistent-store-create directory)))
    e-chat--default-sessions))

(defun e-chat--ensure-session (harness session-id)
  "Ensure SESSION-ID exists in HARNESS."
  (condition-case nil
      (e-harness-create-session harness :id session-id)
    (e-session-duplicate
     nil)))

(defun e-chat--session-store ()
  "Return the current chat buffer's session store."
  (unless e-chat-harness
    (user-error "This buffer is not attached to an e chat session"))
  (e-harness-sessions e-chat-harness))

(defun e-chat--short-session-id (session-id)
  "Return a compact SESSION-ID for display."
  (if (> (length session-id) 12)
      (substring session-id 0 12)
    session-id))

(defun e-chat--session-buffer-name (store session-id)
  "Return the buffer name for SESSION-ID in STORE."
  (format "*e-chat:%s*"
          (or (ignore-errors (e-session-display-title store session-id))
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
      (e-harness-sessions e-chat-harness)
      e-chat-session-id)
     t)))

(defun e-chat--subscribe (harness buffer)
  "Subscribe BUFFER to HARNESS chat events."
  (e-harness-subscribe
   harness
   (lambda (event)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (eq e-chat-harness harness)
           (e-chat--render-event event)))))))

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

(defun e-chat--composer-active-p ()
  "Return non-nil when the current buffer has an active composer."
  (and (markerp e-chat--transcript-end-marker)
       (marker-position e-chat--transcript-end-marker)
       (markerp e-chat--composer-start-marker)
       (marker-position e-chat--composer-start-marker)))

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
        (delete-region start (point-max))
        (set-marker e-chat--transcript-end-marker nil)
        (set-marker e-chat--composer-start-marker nil)
        (when (markerp e-chat--composer-spacer-marker)
          (set-marker e-chat--composer-spacer-marker nil))
        t))))

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

(defun e-chat--composer-spacer-lines ()
  "Return how many protected blank lines should precede the composer."
  (let ((height (e-chat--visible-height)))
    (if (not height)
        0
      (max 0 (- height
                (e-chat--transcript-screen-lines)
                4)))))

(defun e-chat--insert-composer ()
  "Insert an editable composer at the end of the current chat buffer."
  (e-chat--disable-completion)
  (e-chat--delete-composer)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (or (bobp) (bolp))
      (insert "\n"))
    (setq e-chat--composer-spacer-marker (point-marker))
    (set-marker-insertion-type e-chat--composer-spacer-marker nil)
    (let ((spacer-lines (e-chat--composer-spacer-lines)))
      (when (> spacer-lines 0)
        (e-chat--insert-protected
         (make-string spacer-lines ?\n)
         'e-chat-separator-face)))
    (setq e-chat--transcript-end-marker (point-marker))
    (set-marker-insertion-type e-chat--transcript-end-marker nil)
    (e-chat--insert-protected
     (concat e-chat--composer-separator "\n")
     'e-chat-separator-face)
    (e-chat--insert-protected
     e-chat--composer-glyph
     'e-chat-composer-face
     '(e-chat-composer t))
    (setq e-chat--composer-start-marker (point-marker))
    (set-marker-insertion-type e-chat--composer-start-marker nil)
    (goto-char e-chat--composer-start-marker)
    (e-chat--show-composer)))

(defun e-chat--insert-pending-separator ()
  "Insert protected bottom separator chrome without an editable composer."
  (e-chat--delete-composer)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (or (bobp) (bolp))
      (insert "\n"))
    (setq e-chat--composer-spacer-marker (point-marker))
    (set-marker-insertion-type e-chat--composer-spacer-marker nil)
    (let ((spacer-lines (e-chat--composer-spacer-lines)))
      (when (> spacer-lines 0)
        (e-chat--insert-protected
         (make-string spacer-lines ?\n)
         'e-chat-separator-face)))
    (setq e-chat--transcript-end-marker (point-marker))
    (set-marker-insertion-type e-chat--transcript-end-marker nil)
    (e-chat--insert-protected
     (concat e-chat--composer-separator "\n")
     'e-chat-separator-face)
    (goto-char (point-max))))

(defun e-chat--ensure-composer ()
  "Ensure the current chat buffer has an active composer."
  (unless (e-chat--composer-active-p)
    (e-chat--insert-composer)))

(defun e-chat--composer-text ()
  "Return the current editable composer text."
  (unless (e-chat--composer-active-p)
    (user-error "No active e chat composer"))
  (string-trim
   (buffer-substring-no-properties e-chat--composer-start-marker
                                   (point-max))))

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
                              :intermittent-entries nil
                              :transient-start-marker nil
                              :transient-end-marker nil
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
                            :start-marker nil
                            :end-marker nil)))
          (puthash block-id record registry)
          (setq e-chat--block-order (append e-chat--block-order (list block-id)))
          record))))

(defun e-chat--set-turn-time (turn-id field value)
  "Set TURN-ID timing FIELD to VALUE when both are available."
  (when (and turn-id value)
    (plist-put (e-chat--turn-record turn-id) field value)))

(defun e-chat--update-block-bounds (block-id turn-id start end)
  "Set BLOCK-ID bounds for TURN-ID to START through END."
  (when block-id
    (e-chat--turn-record turn-id)
    (let ((record (e-chat--block-record block-id turn-id)))
      (plist-put record :start-marker (copy-marker start nil))
      (plist-put record :end-marker (copy-marker end nil)))))

(defun e-chat--block-at-point ()
  "Return the rendered block id at point, or nil."
  (or (get-text-property (point) 'e-chat-block-id)
      (get-text-property (max (point-min) (1- (point))) 'e-chat-block-id)))

(defun e-chat--last-rendered-block-id ()
  "Return the most recent rendered block id before the composer."
  (car (last e-chat--block-order)))

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
  (let ((remaining e-chat--block-order)
        (index 0)
        found)
    (while (and remaining (not found))
      (if (equal (car remaining) e-chat--focused-block-id)
          (setq found index)
        (setq index (1+ index)
              remaining (cdr remaining))))
    (unless found
      (user-error "Focused e chat block is no longer rendered"))
    (let ((next-index (max 0 (min (1- (length e-chat--block-order))
                                  (+ found step)))))
      (e-chat--focus-block (nth next-index e-chat--block-order)))))

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
        (format "%.2fs" (- ended-seconds started-seconds))
      "unknown")))

(defun e-chat--indent-detail-text (text)
  "Return TEXT with each line indented for expanded turn details."
  (concat "  " (replace-regexp-in-string "\n" "\n  " (string-trim-right text))))

(defun e-chat--intermittent-entry-text (entry)
  "Return display text for intermittent turn ENTRY."
  (format "%s\n%s"
          (plist-get entry :title)
          (plist-get entry :content)))

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

(defun e-chat--transient-text (record)
  "Return visible transient text for RECORD."
  (when-let ((entries (plist-get record :intermittent-entries)))
    (concat
     (mapconcat #'e-chat--intermittent-entry-text entries "\n\n")
     "\n\n")))

(defun e-chat--add-intermittent-entry (record title content &optional append)
  "Add intermittent TITLE and CONTENT to RECORD.
When APPEND is non-nil, merge CONTENT into the previous entry with TITLE."
  (when (and record content (not (string-empty-p content)))
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
                           (list (list :title title :content content))))))))

(defun e-chat--delete-turn-transient (record)
  "Delete the currently visible transient block for RECORD."
  (let ((start (plist-get record :transient-start-marker))
        (end (plist-get record :transient-end-marker)))
    (when (and (markerp start)
               (markerp end)
               (marker-position start)
               (marker-position end))
      (let ((inhibit-read-only t))
        (delete-region start end)))
    (plist-put record :transient-start-marker nil)
    (plist-put record :transient-end-marker nil)))

(defun e-chat--render-turn-transient (turn-id record)
  "Render RECORD's intermittent entries as a temporary block for TURN-ID."
  (e-chat--delete-turn-transient record)
  (unless (plist-get record :final-rendered)
    (when-let ((text (e-chat--transient-text record)))
      (let ((had-composer (e-chat--delete-composer)))
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (or (bobp) (bolp))
            (insert "\n"))
          (let ((start (point)))
            (e-chat--insert-protected
             text
             'e-chat-system-face
             `(e-chat-transient-turn-id ,turn-id))
            (plist-put record :transient-start-marker (copy-marker start nil))
            (plist-put record :transient-end-marker (copy-marker (point) nil))))
        (when had-composer
          (e-chat--insert-composer))))))

(defun e-chat--append-intermittent-entry (turn-id title content &optional append)
  "Append intermittent TITLE and CONTENT to TURN-ID.
When APPEND is non-nil, merge CONTENT into the previous entry with TITLE."
  (when (and turn-id content (not (string-empty-p content)))
    (let ((record (e-chat--turn-record turn-id)))
      (e-chat--add-intermittent-entry record title content append)
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

(defun e-chat--record-tool-message (turn-id message)
  "Record tool MESSAGE as expandable details for TURN-ID."
  (when-let ((record (e-chat--turn-record turn-id)))
    (pcase (plist-get message :role)
      ('tool-call
       (e-chat--add-intermittent-entry
        record
        "Tool call"
        (e-chat--format-tool-call (plist-get message :content))))
      ('tool
       (e-chat--add-intermittent-entry
        record
        "Tool"
        (format "%S" (plist-get message :content)))))))

(defun e-chat--record-replayed-message-time (record message)
  "Record MESSAGE's replay timestamp into RECORD."
  (when-let ((created-at (plist-get message :created-at)))
    (unless (plist-get record :started-at)
      (plist-put record :started-at created-at))
    (plist-put record :ended-at created-at)))

(defun e-chat--finalize-turn-display (turn-id)
  "Replace visible transient TURN-ID display with the final response."
  (when-let ((record (e-chat--turn-record turn-id)))
    (e-chat--delete-turn-transient record)
    (plist-put record :final-rendered t)))

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

(defun e-chat--block-details-visible-p (block)
  "Return non-nil when BLOCK has visible expanded detail text."
  (let ((start (plist-get block :details-start-marker))
        (end (plist-get block :details-end-marker)))
    (and (markerp start)
         (markerp end)
         (marker-position start)
         (marker-position end)
         (< (marker-position start) (marker-position end)))))

(defun e-chat--turn-details-text (turn-id record)
  "Return expanded details text for TURN-ID using RECORD."
  (concat
   (or (e-chat--intermittent-details-text record) "")
   (format "  Turn: %s\n  Started: %s\n  Ended: %s\n  Duration: %s\n\n"
           turn-id
           (e-chat--format-time-value (plist-get record :started-at))
           (e-chat--format-time-value (plist-get record :ended-at))
           (e-chat--format-duration (plist-get record :started-at)
                                    (plist-get record :ended-at)))))

(defun e-chat--insert-block-details (block turn-id record)
  "Insert expanded details for BLOCK and TURN-ID using RECORD."
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
         (e-chat--turn-details-text turn-id record)
         'e-chat-system-face
         '(e-chat-turn-details t))
        (plist-put block :details-start-marker (copy-marker start nil))
        (plist-put block :details-end-marker (copy-marker (point) nil))))
    (when had-composer
      (goto-char (point-max))
      (e-chat--insert-composer))))

(defun e-chat--entry-face (title)
  "Return face for chat entry TITLE."
  (pcase title
    ("You" 'e-chat-user-face)
    ("Assistant" 'e-chat-assistant-face)
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

(defun e-chat--insert-entry (title content &optional ensure-composer turn-id)
  "Insert a protected chat entry with TITLE and CONTENT.
When ENSURE-COMPOSER is non-nil, recreate the composer after inserting.
TURN-ID tags the rendered entry for response navigation."
  (let ((had-composer (e-chat--delete-composer))
        (block-id (and turn-id (e-chat--next-block-id))))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (or (bobp) (bolp))
        (insert "\n"))
      (let ((start (point)))
        (e-chat--insert-protected
         (e-chat--entry-text title content)
         (e-chat--entry-face title)
         (when block-id
           `(e-chat-turn-id ,turn-id
             e-chat-block-id ,block-id)))
        (e-chat--update-block-bounds block-id turn-id start (point))))
    (when (or ensure-composer had-composer)
      (e-chat--insert-composer))))

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

(defun e-chat-response-navigation-expand ()
  "Expand metadata under the focused turn block."
  (interactive)
  (unless e-chat--focused-turn-id
    (user-error "No focused e chat turn"))
  (let* ((block (gethash e-chat--focused-block-id e-chat--block-registry))
         (turn-id (plist-get block :turn-id))
         (record (gethash turn-id e-chat--turn-registry)))
    (if (e-chat--block-details-visible-p block)
        (e-chat--delete-block-details block)
      (e-chat--insert-block-details block turn-id record))))

(defun e-chat-response-navigation-insert ()
  "Leave response navigation and focus the composer."
  (interactive)
  (e-chat-response-navigation-mode -1)
  (e-chat--show-composer))

(defun e-chat--refresh-composer-position ()
  "Refresh composer spacer for the current visible window."
  (when (e-chat--composer-active-p)
    (let ((text (buffer-substring-no-properties e-chat--composer-start-marker
                                                (point-max))))
      (e-chat--delete-composer)
      (e-chat--insert-composer)
      (insert text)
      (e-chat--show-composer))))

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
        (recenter -1)))))

(defun e-chat--clear ()
  "Clear and initialize the current chat buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq e-chat--transcript-end-marker nil)
    (setq e-chat--composer-start-marker nil)
    (setq e-chat--composer-spacer-marker nil)
    (setq e-chat--turn-registry (make-hash-table :test 'equal))
    (setq e-chat--block-registry (make-hash-table :test 'equal))
    (setq e-chat--block-order nil)
    (setq e-chat--block-counter 0)
    (setq e-chat--focused-turn-id nil)
    (setq e-chat--focused-block-id nil)
    (when (overlayp e-chat--focused-turn-overlay)
      (delete-overlay e-chat--focused-turn-overlay))
    (setq e-chat--focused-turn-overlay nil)
    (let ((title (and e-chat-harness
                      e-chat-session-id
                      (ignore-errors
                        (e-session-display-title
                         (e-harness-sessions e-chat-harness)
                         e-chat-session-id)))))
      (e-chat--insert-protected
       (if title
           (format "%s\n%s\n\n" e-chat--title title)
         (concat e-chat--title "\n\n"))
       'e-chat-title-face))
    (e-chat--insert-composer)))

(defun e-chat--set-status (status)
  "Set chat buffer STATUS."
  (setq header-line-format
        (if (and e-chat-harness e-chat-session-id)
            (let* ((title (ignore-errors
                            (e-session-display-title
                             (e-harness-sessions e-chat-harness)
                             e-chat-session-id)))
                   (options (ignore-errors
                              (e-harness--turn-options
                               e-chat-harness
                               e-chat-session-id)))
                   (model (plist-get options :model))
                   (effort (plist-get options :reasoning-effort)))
              (format "E Chat: %s - %s - %s/%s"
                      status
                      title
                      (or model "model unset")
                      (or effort "effort unset")))
          (format "E Chat: %s" status))))

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
  (pcase (plist-get event :type)
    ('turn-started
     (e-chat--set-turn-time (plist-get event :turn-id)
                             :started-at
                             (plist-get event :created-at))
     (e-chat--set-status (format "running %s" (plist-get event :turn-id))))
    ('turn-finished
     (e-chat--set-turn-time (plist-get event :turn-id)
                             :ended-at
                             (plist-get event :created-at))
     (e-chat--set-status "done")
     (e-chat--ensure-composer)
     (e-chat--refresh-composer-position))
    ('turn-failed
     (e-chat--set-turn-time (plist-get event :turn-id)
                             :ended-at
                             (plist-get event :created-at))
     (e-chat--set-status "error")
     (e-chat--insert-entry
      "System"
      (format "Turn failed: %s"
              (plist-get (plist-get event :payload) :error))
      t
      (plist-get event :turn-id)))
    ('turn-cancelled
     (e-chat--set-turn-time (plist-get event :turn-id)
                             :ended-at
                             (plist-get event :created-at))
     (e-chat--set-status "cancelled")
     (e-chat--insert-entry "System" "Turn cancelled" t
                           (plist-get event :turn-id)))
    ('message-added
     (let* ((message (plist-get (plist-get event :payload) :message))
            (entry (e-chat--message-entry message)))
       (when (eq (plist-get message :role) 'assistant)
         (e-chat--finalize-turn-display (plist-get event :turn-id)))
       (e-chat--insert-entry (car entry) (cdr entry) nil
                             (plist-get event :turn-id))))
    ('assistant-delta
     (e-chat--set-status "streaming"))
    ('reasoning-delta
     (e-chat--set-status "reasoning")
     (e-chat--append-intermittent-entry
      (plist-get event :turn-id)
      "Reasoning"
      (plist-get (plist-get event :payload) :content)
      t))
    ('tool-started
     (e-chat--set-status "tool")
     (e-chat--append-intermittent-entry
      (plist-get event :turn-id)
      "Tool call"
      (e-chat--format-tool-call (plist-get event :payload))))
    ('tool-finished
     (e-chat--set-status "tool done")
     (when-let ((record (e-chat--existing-turn-record
                         (plist-get event :turn-id))))
       (e-chat--add-intermittent-entry
        record
        "Tool"
        (format "%S" (plist-get (plist-get event :payload) :result)))
       (e-chat--render-turn-transient (plist-get event :turn-id) record)))
    ('backend-empty-output
     (e-chat--set-status "done"))
    ('session-reset
     (e-chat--set-status "idle")
     (e-chat--insert-entry "System" "Session reset" t))
    (_
     (e-chat--insert-entry "System" (format "Event: %S" event) t))))

(defun e-chat--render-session ()
  "Render the attached session transcript in the current buffer."
  (let ((turn-index 0)
        turn-id
        record)
    (dolist (message (e-harness-messages e-chat-harness e-chat-session-id))
      (when (or (not turn-id)
                (eq (plist-get message :role) 'user))
        (setq turn-index (1+ turn-index))
        (setq turn-id (format "replayed-turn-%d" turn-index))
        (setq record (e-chat--turn-record turn-id)))
      (e-chat--record-replayed-message-time record message)
      (if (e-chat--tool-message-p message)
          (e-chat--record-tool-message turn-id message)
        (let ((entry (e-chat--message-entry message)))
          (e-chat--insert-entry (car entry) (cdr entry) t turn-id))))))

(cl-defun e-chat-open (&key harness session-id new-session)
  "Attach and return an e chat buffer.
HARNESS, SESSION-ID, and NEW-SESSION are injectable for presentation tests and
reload.  User-facing commands should call `e-chat-new' or `e-chat-resume'."
  (let* ((chat-harness (or harness (e-chat--default-harness)))
         (session (when (or new-session (not session-id))
                    (e-harness-create-session chat-harness)))
         (chat-session-id (or session-id
                              (plist-get session :id)
                              e-chat-default-session-id))
         (buffer (or (e-chat--find-session-buffer chat-session-id)
                     (get-buffer-create
                      (e-chat--session-buffer-name
                       (e-harness-sessions chat-harness)
                       chat-session-id)))))
    (e-chat--attach-buffer buffer chat-harness chat-session-id)
    buffer))

(defun e-chat--attach-buffer (buffer harness session-id)
  "Attach BUFFER to HARNESS and SESSION-ID."
  (e-chat--ensure-session harness session-id)
  (with-current-buffer buffer
    (e-chat-mode)
    (e-chat--disable-modal-editing)
    (e-chat--disable-completion)
    (e-chat--ensure-window-refresh-hook)
    (setq-local e-chat-harness harness)
    (setq-local e-chat-session-id session-id)
    (e-chat--rename-buffer-for-session)
    (e-chat--clear)
    (e-chat--render-session)
    (e-chat--set-status "idle")
    (e-chat--subscribe harness buffer))
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
(defun e-chat-new ()
  "Create and open a new persisted e chat session."
  (interactive)
  (let ((buffer (e-chat-open :new-session t)))
    (when (called-interactively-p 'interactive)
      (pop-to-buffer buffer))
    buffer))

(defun e-chat--session-choice-label (session)
  "Return completion label for SESSION metadata."
  (format "%s  [%s]"
          (plist-get session :title)
          (plist-get session :id)))

;;;###autoload
(defun e-chat-resume ()
  "Resume a recent persisted e chat session."
  (interactive)
  (let* ((harness (e-chat--default-harness))
         (sessions (e-session-list (e-harness-sessions harness))))
    (unless sessions
      (user-error "No e chat sessions to resume"))
    (let* ((labels (mapcar #'e-chat--session-choice-label sessions))
           (selected (completing-read "Resume e session: " labels nil t))
           (index (cl-position selected labels :test #'equal))
           (session (nth index sessions))
           (buffer (e-chat-open :harness harness
                                :session-id (plist-get session :id))))
      (when (called-interactively-p 'interactive)
        (pop-to-buffer buffer))
      buffer)))

;;;###autoload
(defun e-chat-rename (name)
  "Rename the current e chat session to NAME."
  (interactive
   (let* ((store (e-chat--session-store))
          (current (ignore-errors
                     (plist-get
                      (e-session-get store e-chat-session-id)
                      :name))))
     (list (read-string "Session name: " current))))
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-session-rename (e-harness-sessions e-chat-harness) e-chat-session-id name)
  (e-chat--rename-buffer-for-session)
  (e-chat--clear)
  (e-chat--render-session)
  (e-chat--set-status "idle")
  (current-buffer))

;;;###autoload
(defun e-chat-set-model (model)
  "Set MODEL for the current chat session."
  (interactive
   (let* ((options (and e-chat-harness
                       e-chat-session-id
                       (ignore-errors
                         (e-harness--turn-options
                          e-chat-harness
                          e-chat-session-id))))
          (current (plist-get options :model)))
     (list (read-string "Model: " current nil current))))
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-harness-set-session-model e-chat-harness e-chat-session-id model)
  (e-chat--set-status "idle")
  (message "Set e chat model to %s" (if (string-empty-p model) "default" model)))

;;;###autoload
(defun e-chat-set-effort (effort)
  "Set reasoning EFFORT for the current chat session."
  (interactive
   (let* ((options (and e-chat-harness
                       e-chat-session-id
                       (ignore-errors
                         (e-harness--turn-options
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
  (e-harness-set-session-reasoning-effort e-chat-harness e-chat-session-id effort)
  (e-chat--set-status "idle")
  (message "Set e chat effort to %s"
           (if (string-empty-p effort) "default" effort)))

;;;###autoload
(defun e-chat-show-context ()
  "Show the current chat session context in a read-only temp buffer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat--display-context-buffer
   (e-harness-context e-chat-harness e-chat-session-id)
   e-chat-session-id))

;;;###autoload
(defun e-chat-submit (&optional prompt)
  "Submit PROMPT or the current editable prompt text."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (setq prompt (or prompt (e-chat--composer-text)))
  (when (string-empty-p prompt)
    (user-error "Prompt must not be empty"))
  (e-chat--delete-composer)
  (e-chat--set-status "queued")
  (e-harness-prompt-async e-chat-harness e-chat-session-id prompt
                          :delay e-chat-submit-backend-delay)
  (e-chat--insert-pending-separator)
  (redisplay t))

;;;###autoload
(defun e-chat-abort ()
  "Abort the active turn for the current chat buffer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-harness-abort e-chat-harness e-chat-session-id))

;;;###autoload
(defun e-chat-reset ()
  "Reset the current chat session and rendered buffer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat--clear)
  (e-harness-reset e-chat-harness e-chat-session-id))

(provide 'e-chat)

;;; e-chat.el ends here
