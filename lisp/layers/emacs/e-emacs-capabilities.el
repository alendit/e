;;; e-emacs-capabilities.el --- Emacs capabilities for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capability constructors for Emacs awareness, buffer access, buffer mutation,
;; elisp evaluation, and future selection context.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-context)
(require 'e-emacs-tools)
(require 'e-layers)
(require 'e-skills)
(require 'e-tools)
(require 'e-workspaces)

(defconst e-emacs-base-instructions
  "You are running inside Emacs. Use buffer tools for buffer inspection and live buffer edits. Buffer edits do not save files; call save_buffer when persistence is required.

When editing a file that may be open in Emacs, prefer live buffer tools over direct file writes. If you write a file directly and a live file-backed buffer exists for it, sync or reload the buffer before reporting completion. A direct file write leaves the live buffer's visited-modtime stale even when the bytes happen to match, so Emacs will still force a revert; do not treat equal content as proof of coherence. The real coherence test is per buffer: compare the buffer's text to disk and check the visited-modtime, rather than trusting a status summary that compares content alone. If you edit a live file-backed buffer and the change should persist, save the buffer.

Before finalizing, verify presentation, not just content. Confirm the resource you edited is the buffer actually displayed to the user -- another live buffer with a similar name may be the visible one, and a write to the right URI does not imply the right buffer is on screen. Check every visible buffer that corresponds to a modified resource for coherence; reload any that diverge."
  "Default instructions contributed by Emacs awareness capabilities.")

(defun e-emacs-capabilities-visible-buffer-context ()
  "Return a readable summary of visible Emacs buffers."
  (let ((buffers (e-emacs-tools-buffer-metadata-list t)))
    (concat
     "Visible Emacs buffers:\n"
     (if buffers
         (mapconcat
          (lambda (buffer)
            (format "- %s mode=%s file=%s modified=%s visible=%s"
                    (plist-get buffer :name)
                    (plist-get buffer :mode)
                    (or (plist-get buffer :file) "nil")
                    (if (plist-get buffer :modified) "true" "false")
                    (if (plist-get buffer :visible) "true" "false")))
          buffers
          "\n")
       "- none"))))

(defun e-emacs-visible-buffers-context-provider ()
  "Return the visible-buffer context provider for Emacs awareness."
  (e-context-provider-create
   :name 'visible-buffers
   :priority 320
   :cache-placement 'dynamic-context
   :build (cl-function
           (lambda (&key harness session-id turn-id)
             (ignore harness session-id turn-id)
             (list (list :role 'system
                         :content
                         (e-emacs-capabilities-visible-buffer-context)))))))

(defun e-emacs-awareness-capability-create ()
  "Create an Emacs awareness capability."
  (e-capability-create
   :id 'emacs-awareness
   :name "Emacs Awareness"
   :instruction-priority 300
   :instructions e-emacs-base-instructions
   :context-providers (list (e-emacs-visible-buffers-context-provider))))

(defun e-buffer-read-capability-create ()
  "Create a capability for reading live Emacs buffers."
  (e-capability-create
   :id 'buffer-read
   :name "Buffer Read"
   :tools (list #'e-emacs-tools-register-list-buffers)
   :resource-methods (list #'e-emacs-tools-register-buffer-read-resource)))

(defun e-buffer-edit-capability-create ()
  "Create a capability for mutating live Emacs buffers."
  (e-capability-create
   :id 'buffer-edit
   :name "Buffer Edit"
   :tools (list #'e-emacs-tools-register-save-buffer)
   :resource-methods (list #'e-emacs-tools-register-buffer-resource)))

(defconst e-elisp-eval-instructions
  (string-join
   '("When using run_elisp, evaluated code may compose currently active tools through the context-bound API:"
     ""
     "(e-tools-call NAME ARGUMENTS &optional OPTIONS)"
     "(e-tools-call! NAME ARGUMENTS &optional OPTIONS)"
     ""
     "Example:"
     "(let* ((hits (e-tools-call! \"search\" '(:uri \"file://\" :query \"TODO\" :literal t :limit 5))))"
     "  (mapcar (lambda (hit) (e-tools-call! \"read\" (list :uri (plist-get hit :uri))))"
     "          (plist-get hits :matches)))"
     ""
     "Use this when several tool calls are needed to compute one answer, later calls depend on earlier results, many similar resources need the same read/search/edit operation, or the result should be filtered, grouped, or summarized before returning."
     ""
     "Do not use nested tool calls when a single direct tool call is enough, when the user should see each mutating decision before the next step, or when an ordinary top-level tool sequence would be clearer."
     ""
     "Only currently active tools are callable. Nested tool calls are visible as activity and run through the harness lifecycle.")
   "\n")
  "Instructions for explicit Elisp evaluation and active tool chaining.")

(defun e-elisp-eval-capability-create ()
  "Create a capability for explicit Emacs Lisp evaluation."
  (e-capability-create
   :id 'elisp-eval
   :name "Elisp Eval"
   :instructions e-elisp-eval-instructions
   :tools (list #'e-emacs-tools-register-elisp-eval)))

(defconst e-workspace-awareness-instructions
  "Workspace-aware Emacs shells carry a workspace affinity. Prefer workspace_state, workspace_focus_buffer, and workspace_show_shell before raw switch-to-buffer, pop-to-buffer, or display-buffer calls."
  "Instructions for workspace-aware Emacs operation.")

(defconst e-workspace-awareness-display-skill
  (string-join
   '("# Workspace-aware Emacs display"
     ""
     "Use this guidance when model-written Elisp needs to show, focus, or move e-owned buffers in Emacs."
     ""
     "## First choice"
     ""
     "- Call `workspace_state` before changing visibility when workspace ownership is unclear."
     "- Call `workspace_focus_buffer` to focus a named buffer in the active shell workspace."
     "- Call `workspace_show_shell` to return to the active e shell in its workspace."
     ""
     "## Direct Elisp"
     ""
     "- Prefer `e-workspace-pop-to-buffer`, `e-workspace-switch-to-buffer`, and `e-workspace-display-buffer` over raw `pop-to-buffer`, `switch-to-buffer`, or `display-buffer` for e-owned buffers."
     "- Preserve buffer workspace affinity with `e-buffer-workspace`, `e-buffer-set-workspace`, and `e-buffer-ensure-workspace` when creating companion buffers such as input or result panes."
     "- Keep display actions frame-scoped; avoid `reusable-frames` or `lru-frames` values that can reuse windows from unrelated frames or workspaces."
     ""
     "## Ownership"
     ""
     "- Presentation shells own workspace affinity. Harness and provider code should not infer or rewrite workspace identity unless a shell API requests it."
     "- Unexpected cross-workspace display should surface clearly instead of silently falling back to global window reuse.")
   "\n")
  "Detailed guidance for workspace-aware Emacs display operations.")

(defun e-workspace-awareness--shell-buffer ()
  "Return the buffer that should be treated as the active shell buffer."
  (let ((selected (window-buffer (selected-window)))
        (current (current-buffer)))
    (cond
     ((and (buffer-live-p selected)
           (e-buffer-workspace selected))
      selected)
     ((and (buffer-live-p current)
           (e-buffer-workspace current))
      current)
     ((buffer-live-p selected)
      selected)
     (t current))))

(defun e-workspace-awareness--state ()
  "Return workspace state for the current Emacs shell context."
  (let* ((shell-buffer (e-workspace-awareness--shell-buffer))
         (current (e-workspace-current))
         (shell (or (and (buffer-live-p shell-buffer)
                         (e-buffer-workspace shell-buffer))
                    current)))
    (list :current current
          :shell shell
          :match (and (e-workspace-token-p current)
                      (e-workspace-token-p shell)
                      (e-workspace-equal-p current shell))
          :shell-buffer (and (buffer-live-p shell-buffer)
                             (buffer-name shell-buffer)))))

(defun e-workspace-awareness-context ()
  "Return a readable workspace-awareness summary."
  (let* ((state (e-workspace-awareness--state))
         (current (plist-get state :current))
         (shell (plist-get state :shell)))
    (concat
     "Workspace awareness:\n"
     (format "- current=%s\n" (e-workspace-format current))
     (format "- shell=%s\n" (e-workspace-format shell))
     (format "- match=%s\n" (if (plist-get state :match) "true" "false"))
     (format "- shell_buffer=%s\n" (or (plist-get state :shell-buffer)
                                       "none"))
     "- guidance: prefer workspace_state, workspace_focus_buffer, and workspace_show_shell before raw switch-to-buffer, pop-to-buffer, or display-buffer; direct Elisp should use e-workspace-* helpers.")))

(defun e-workspace-awareness-context-provider ()
  "Return the workspace-awareness context provider."
  (e-context-provider-create
   :name 'workspace-awareness
   :priority 330
   :cache-placement 'dynamic-context
   :build (cl-function
           (lambda (&key harness session-id turn-id)
             (ignore harness session-id turn-id)
             (list (list :role 'system
                         :content
                         (e-workspace-awareness-context)))))))

(defun e-workspace-awareness--tool-state ()
  "Return model-facing workspace state."
  (let* ((state (e-workspace-awareness--state))
         (current (plist-get state :current))
         (shell (plist-get state :shell)))
    (list :current (e-workspace-format current)
          :shell (e-workspace-format shell)
          :match (and (plist-get state :match) t)
          :shell_buffer (plist-get state :shell-buffer))))

(defun e-workspace-awareness--argument-string (arguments key)
  "Return required string argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp key)))
    value))

(defun e-workspace-awareness--buffer-target-workspace (buffer)
  "Return the workspace target for BUFFER."
  (or (e-buffer-workspace buffer)
      (plist-get (e-workspace-awareness--state) :shell)
      (e-workspace-current)))

(defun e-workspace-awareness--focus-buffer (buffer &optional add-to-workspace)
  "Focus BUFFER through workspace-aware display helpers.
When ADD-TO-WORKSPACE is non-nil, add BUFFER to its target workspace first."
  (let ((workspace (e-workspace-awareness--buffer-target-workspace buffer)))
    (when add-to-workspace
      (e-workspace-add-buffer buffer workspace))
    (e-workspace-pop-to-buffer buffer :workspace workspace)
    (list :buffer (buffer-name buffer)
          :workspace (e-workspace-format workspace)
          :focused t)))

(defun e-workspace-awareness-register-state (registry)
  "Register the workspace_state tool in REGISTRY."
  (e-tools-register
   registry
   :name "workspace_state"
   :description "Return the current Emacs workspace and active e shell workspace affinity."
   :parameters '(:type "object"
                 :properties nil)
   :handler (lambda (_arguments)
              (e-workspace-awareness--tool-state))))

(defun e-workspace-awareness-register-focus-buffer (registry)
  "Register the workspace_focus_buffer tool in REGISTRY."
  (e-tools-register
   registry
   :name "workspace_focus_buffer"
   :description "Focus a named Emacs buffer in the active e shell workspace."
   :parameters '(:type "object"
                 :properties (:buffer (:type "string")
                              :add_to_workspace (:type "boolean"))
                 :required ["buffer"])
   :handler
   (lambda (arguments)
     (let* ((name (e-workspace-awareness--argument-string arguments :buffer))
            (buffer (or (get-buffer name)
                        (user-error "No buffer named %s" name))))
       (e-workspace-awareness--focus-buffer
        buffer
        (plist-get arguments :add_to_workspace))))))

(defun e-workspace-awareness-register-show-shell (registry)
  "Register the workspace_show_shell tool in REGISTRY."
  (e-tools-register
   registry
   :name "workspace_show_shell"
   :description "Show the active e shell buffer in its workspace."
   :parameters '(:type "object"
                 :properties nil)
   :handler
   (lambda (_arguments)
     (let ((buffer (e-workspace-awareness--shell-buffer)))
       (unless (buffer-live-p buffer)
         (user-error "No active shell buffer"))
       (e-workspace-awareness--focus-buffer buffer)))))

(defun e-workspace-awareness-capability-create ()
  "Create a capability for workspace-aware Emacs shells."
  (e-capability-with-skills-create
   :id 'workspace-awareness
   :name "Workspace Awareness"
   :instructions e-workspace-awareness-instructions
   :context-providers (list (e-workspace-awareness-context-provider))
   :tools (list #'e-workspace-awareness-register-state
                #'e-workspace-awareness-register-focus-buffer
                #'e-workspace-awareness-register-show-shell)
   :skills
   (list
    (e-skill-spec-create
     :name "workspace-display"
     :description "Write Elisp that displays e buffers in their owning workspace."
     :content e-workspace-awareness-display-skill))))

(defun e-selection-context-capability-create ()
  "Create a placeholder capability for future selection context."
  (e-capability-create
   :id 'selection-context
   :name "Selection Context"))

(defun e-emacs-layer-create ()
  "Create the conservative Emacs layer preset."
  (e-layer-create
   :id 'emacs
   :name "Emacs"
   :capabilities (list (e-emacs-awareness-capability-create)
                       (e-buffer-read-capability-create)
                       (e-selection-context-capability-create))))

(defun e-emacs-operator-layer-create ()
  "Create the Emacs operator layer preset."
  (let ((emacs-layer (e-emacs-layer-create)))
    (e-layer-create
     :id 'emacs-operator
     :name "Emacs Operator"
     :capabilities (append
                    (e-layer-capabilities emacs-layer)
                    (list (e-buffer-edit-capability-create)
                          (e-elisp-eval-capability-create)
                          (e-workspace-awareness-capability-create))))))

(provide 'e-emacs-capabilities)

;;; e-emacs-capabilities.el ends here
