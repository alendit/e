;;; e-chat-output-mode.el --- Chat output markup mode capability for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Output mode selects the markup the assistant emits and the chat transcript
;; renders: the default `markdown', or `org' for users whose knowledge base is
;; Org.  It is capability configuration, reusing `e-capability-config' for
;; global and project (directory-local) precedence, plus an optional per-session
;; override stored as durable capability state.
;;
;; This module owns mode resolution and the model-facing instruction.  When the
;; resolved mode is `org', a context provider contributes a system fragment that
;; tells the model to emit Org markup instead of Markdown.  When `markdown', the
;; capability contributes nothing and behaves exactly as before.  The chat
;; presentation shell resolves the same mode to pick its rendering branch, so
;; the instruction and the renderer never disagree.
;;
;; Output mode is orthogonal to `output-style' (prose voice): the two compose.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-context)
(require 'e-session)
(require 'subr-x)

(declare-function e-harness-effective-capability-config "e-harness"
                  (harness capability-id options &key session-id directory
                           overrides))
(declare-function e-harness-sessions "e-harness" (harness))

(defconst e-chat-output-mode-instruction-priority 262
  "Instruction priority for the `chat-output-mode' capability.
Output mode is late-binding markup guidance.  It sorts just after
`output-style' (260) so voice and markup guidance stay adjacent at the tail of
the system channel.")

(defconst e-chat-output-mode-values '(markdown org)
  "Supported output markup modes.")

(defconst e-chat-output-mode--org-instructions
  "Emit Org markup in your responses, not Markdown. Use `*'/`**' headings, `-'/`1.' lists, and `#+begin_src LANG' ... `#+end_src' source blocks. Write links as Org links `[[target][description]]' (or `[[target]]' when the target reads well on its own), never Markdown `[text](url)': the reader opens these directly in Org. Use `=verbatim=' or `~code~' for inline code and Org emphasis (`*bold*', `/italic/') rather than Markdown emphasis. This shapes only markup, not correctness or the requested prose voice."
  "Model-facing instruction contributed when the resolved output mode is `org'.")

(defun e-chat-output-mode--mode-value-p (value)
  "Return non-nil when VALUE is a supported output mode."
  (and (symbolp value) (memq value e-chat-output-mode-values) t))

(defconst e-chat-output-mode--config-options
  (list (e-capability-config-option-create
         :key :mode
         :type 'symbol
         :default 'markdown
         :documentation
         "Assistant output markup mode: `markdown' (default) or `org'.
When `org', the model is told to emit Org markup and the chat transcript
renders assistant responses with Org fontification and clickable Org links."
         :validator #'e-chat-output-mode--mode-value-p))
  "Config option specs owned by the `chat-output-mode' capability.")

(defun e-chat-output-mode--config-mode (&optional harness session-id directory)
  "Return the configured mode from global/project/runtime config.
When HARNESS is non-nil, harness-local runtime config and the session project
root participate; otherwise resolve against DIRECTORY only."
  (let ((root (or directory default-directory)))
    (plist-get
     (if (and harness (fboundp 'e-harness-effective-capability-config))
         (e-harness-effective-capability-config
          harness 'chat-output-mode e-chat-output-mode--config-options
          :session-id session-id :directory root)
       (e-capability-config-resolve
        'chat-output-mode e-chat-output-mode--config-options
        :directory root))
     :mode)))

(defun e-chat-output-mode-session-get (harness session-id)
  "Return the per-session output mode override for SESSION-ID, or nil."
  (when (and harness session-id)
    (let ((mode (plist-get
                 (ignore-errors
                   (e-session-capability-state
                    (e-harness-sessions harness) session-id 'chat-output-mode))
                 :mode)))
      (and (e-chat-output-mode--mode-value-p mode) mode))))

(defun e-chat-output-mode-resolve (&optional harness session-id directory)
  "Return the effective output mode symbol.
A per-session override wins over global/project/runtime config; the default is
`markdown'."
  (or (e-chat-output-mode-session-get harness session-id)
      (e-chat-output-mode--config-mode harness session-id directory)
      'markdown))

(defun e-chat-output-mode-session-set (harness session-id mode)
  "Set the per-session output MODE override for SESSION-ID.
A nil MODE clears the override, falling back to configured resolution.  Signal
on an unknown non-nil MODE."
  (when (and mode (not (e-chat-output-mode--mode-value-p mode)))
    (error "Unknown output mode `%s'; known modes: %s"
           mode
           (mapconcat #'symbol-name e-chat-output-mode-values ", ")))
  (e-session-set-capability-state
   (e-harness-sessions harness) session-id 'chat-output-mode
   (and mode (list :mode mode)))
  mode)

(cl-defun e-chat-output-mode-context-provider
    (&key harness session-id _turn-id _context-purpose)
  "Return the Org-output instruction when the resolved mode is `org'."
  (when (eq (e-chat-output-mode-resolve harness session-id) 'org)
    (list (list :role 'system
                :content e-chat-output-mode--org-instructions))))

(defun e-chat-output-mode-capability-create (&optional _directory)
  "Create the `chat-output-mode' capability.
The capability registers its config option and contributes the Org-output
instruction through a context provider so a per-session toggle takes effect
without recreating capabilities."
  (e-capability-config-register-options
   'chat-output-mode e-chat-output-mode--config-options)
  (e-capability-create
   :id 'chat-output-mode
   :name "Chat Output Mode"
   :context-providers
   (list (e-context-provider-create
          :name 'chat-output-mode
          :priority e-chat-output-mode-instruction-priority
          :cache-placement 'stable-context
          :build #'e-chat-output-mode-context-provider))
   :config-options e-chat-output-mode--config-options))

(defun e-chat-output-mode--set-config (mode)
  "Set the global output MODE in `e-capability-config'.
A nil MODE clears the global override.  Signal on an unknown non-nil MODE."
  (when (and mode (not (e-chat-output-mode--mode-value-p mode)))
    (error "Unknown output mode `%s'; known modes: %s"
           mode
           (mapconcat #'symbol-name e-chat-output-mode-values ", ")))
  (let ((plist (cdr (assq 'chat-output-mode e-capability-config))))
    (setf (alist-get 'chat-output-mode e-capability-config)
          (plist-put plist :mode mode)))
  mode)

(defun e-chat-output-mode-set (mode)
  "Interactively set the global output MODE.
Choosing the empty selection clears the override."
  (interactive
   (let* ((choices (mapcar #'symbol-name e-chat-output-mode-values))
          (choice (completing-read
                   "Output mode (empty to clear): " choices nil t)))
     (list (and (not (string-empty-p choice)) (intern choice)))))
  (e-chat-output-mode--set-config mode)
  (message
   (if mode
       "Output mode set to `%s'; new turns pick it up."
     "Output mode cleared; new turns use the default markdown.")
   mode))

(provide 'e-chat-output-mode)

;;; e-chat-output-mode.el ends here
