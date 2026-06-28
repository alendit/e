;;; e-org-canvas-capabilities.el --- Org Canvas capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Org Canvas contributes gated model instructions, current editor context, and
;; safe Org visibility tools for sessions marked with :org-canvas metadata.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-chat-session)
(require 'e-context)
(require 'e-harness)
(require 'e-layers)
(require 'e-session)
(require 'e-tools)
(require 'e-workspaces)
(require 'org)
(require 'seq)
(require 'subr-x)

(defconst e-org-canvas--thread-instructions
  "Scope: thread. We are talking about the topic under the cursor. Use the captured point, heading path, visible window, and current document context to infer the relevant topic boundary. Prefer edits near or under that topic unless the user explicitly asks for a broader document change."
  "Model-facing instructions for Org Canvas thread scope.")

(defconst e-org-canvas--document-instructions
  "Scope: document. Consider the whole Org document. Add, reorganize, summarize, or enrich information in the appropriate location across the document. Use visibility tools to inspect or reveal relevant sections as needed."
  "Model-facing instructions for Org Canvas document scope.")

(defvar e-org-canvas-harness)
(defvar e-org-canvas-session-id)

(defun e-org-canvas--file-uri (file)
  "Return canonical file URI for FILE."
  (concat "file://" (expand-file-name file)))

(defun e-org-canvas--uri-file-name (uri)
  "Return local filename for URI, or nil."
  (when (and (stringp uri) (string-prefix-p "file://" uri))
    (expand-file-name (substring uri (length "file://")))))

(defun e-org-canvas--buffer-matches-uri-p (buffer uri)
  "Return non-nil when BUFFER still represents Org Canvas URI."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (equal (if buffer-file-name
                    (e-org-canvas--file-uri buffer-file-name)
                  (concat "buffer://" (buffer-name)))
                uri))))

(defun e-org-canvas-session-metadata (harness session-id)
  "Return Org Canvas metadata for HARNESS SESSION-ID, or nil."
  (plist-get
   (plist-get (e-session-get (e-harness-sessions harness) session-id)
              :metadata)
   :org-canvas))

(defun e-org-canvas-session-p (harness session-id)
  "Return non-nil when HARNESS SESSION-ID is an Org Canvas session."
  (and (e-org-canvas-session-metadata harness session-id) t))

(defun e-org-canvas-session-buffer (harness session-id)
  "Return the live Org Canvas buffer for HARNESS SESSION-ID, if available."
  (when-let ((metadata (e-org-canvas-session-metadata harness session-id)))
    (or (e-workspace-find-buffer
         (lambda (buffer)
           (with-current-buffer buffer
             (and (bound-and-true-p e-org-canvas-mode)
                  (eq e-org-canvas-harness harness)
                  (equal e-org-canvas-session-id session-id))))
         :prefer-visible t)
        (when-let* ((buffer-name (plist-get metadata :buffer-name))
                    (buffer (get-buffer buffer-name)))
          (and (e-org-canvas--buffer-matches-uri-p
                buffer
                (plist-get metadata :uri))
               buffer))
        (when-let ((file (e-org-canvas--uri-file-name
                          (plist-get metadata :uri))))
          (or (find-buffer-visiting file)
              (and (file-readable-p file)
                   (find-file-noselect file))))
        (when-let ((attachment
                    (seq-find
                     (lambda (candidate)
                       (equal (plist-get candidate :uri)
                              (plist-get metadata :uri)))
                     (e-chat-session-attachments harness session-id))))
          (e-chat-session--attachment-live-buffer attachment)))))

(defun e-org-canvas--inside-heading-p ()
  "Return non-nil when point is in an Org heading or subtree."
  (and (derived-mode-p 'org-mode)
       (not (org-before-first-heading-p))))

(defun e-org-canvas--heading-visibility ()
  "Return a compact visibility symbol for the current heading."
  (let ((after-heading (min (point-max) (1+ (line-end-position)))))
    (if (get-char-property after-heading 'invisible)
        'folded
      'open)))

(defun e-org-canvas--heading-path ()
  "Return the current Org outline path, or nil."
  (when (e-org-canvas--inside-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (org-get-outline-path t))))

(defun e-org-canvas--subtree-bounds ()
  "Return current Org subtree bounds as (START . END), or nil."
  (when (e-org-canvas--inside-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (cons (point) (save-excursion (org-end-of-subtree t t))))))

(defun e-org-canvas--window-range ()
  "Return visible window range as (START . END), falling back to the buffer."
  (if-let ((window (get-buffer-window (current-buffer) t)))
      (cons (window-start window) (window-end window t))
    (cons (point-min) (point-max))))

;;;###autoload
(defun e-org-canvas-capture-focus (&optional scope)
  "Capture a generic Org Canvas focus object for SCOPE at point."
  (let* ((bounds (e-org-canvas--subtree-bounds))
         (range (e-org-canvas--window-range)))
    (list :kind 'position
          :scope (or scope 'thread)
          :buffer-name (buffer-name)
          :uri (if buffer-file-name
                   (e-org-canvas--file-uri buffer-file-name)
                 (concat "buffer://" (buffer-name)))
          :point (point)
          :heading-path (e-org-canvas--heading-path)
          :subtree-start (car bounds)
          :subtree-end (cdr bounds)
          :visibility (and bounds (e-org-canvas--heading-visibility))
          :window-start (car range)
          :window-end (cdr range)
          :annotation-id nil)))

(defun e-org-canvas--heading-state-at-point ()
  "Return compact Org heading state at point."
  (save-excursion
    (org-back-to-heading t)
    (list :level (org-current-level)
          :title (org-get-heading t t t t)
          :point (point)
          :path (org-get-outline-path t)
          :visibility (e-org-canvas--heading-visibility))))

(defun e-org-canvas-visibility-state (&optional buffer)
  "Return compact visibility state for Org BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (org-with-wide-buffer
       (goto-char (point-min))
       (let (headings)
         (while (re-search-forward org-heading-regexp nil t)
           (push (e-org-canvas--heading-state-at-point) headings))
         (nreverse headings))))))

(defun e-org-canvas--format-heading-path (path)
  "Return readable heading PATH."
  (if path
      (string-join path " / ")
    "none"))

(defun e-org-canvas--format-visibility-state (state)
  "Return readable visibility STATE."
  (if state
      (mapconcat
       (lambda (heading)
         (format "- L%s %s [%s] point=%s path=%s"
                 (plist-get heading :level)
                 (plist-get heading :title)
                 (plist-get heading :visibility)
                 (plist-get heading :point)
                 (e-org-canvas--format-heading-path
                  (plist-get heading :path))))
       state
       "\n")
    "- no headings"))

(defun e-org-canvas--focus-summary (focus)
  "Return readable FOCUS metadata."
  (format
   "focus-kind=%s scope=%s point=%s heading-path=%s subtree=%s..%s visibility=%s window=%s..%s"
   (plist-get focus :kind)
   (plist-get focus :scope)
   (plist-get focus :point)
   (e-org-canvas--format-heading-path (plist-get focus :heading-path))
   (or (plist-get focus :subtree-start) "nil")
   (or (plist-get focus :subtree-end) "nil")
   (or (plist-get focus :visibility) "nil")
   (or (plist-get focus :window-start) "nil")
   (or (plist-get focus :window-end) "nil")))

(cl-defun e-org-canvas-context-provider (&key harness session-id _turn-id)
  "Return Org Canvas context for HARNESS SESSION-ID when gated metadata exists."
  (when (and harness session-id (e-org-canvas-session-p harness session-id))
    (let* ((metadata (e-org-canvas-session-metadata harness session-id))
           (buffer (e-org-canvas-session-buffer harness session-id))
           (scope (or (plist-get metadata :last-scope) 'thread))
           (focus (or (plist-get metadata :last-focus)
                      (and (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (e-org-canvas-capture-focus scope)))))
           (instructions (if (eq scope 'document)
                             e-org-canvas--document-instructions
                           e-org-canvas--thread-instructions))
           (visibility (and (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (e-org-canvas-visibility-state)))))
      (list
       (list :role 'system
             :content
             (string-join
              (list
               "Org Canvas mode is active for this session."
               "Treat the Org buffer as the main interaction surface and primary output target. Put durable results in the Org document as structured Org content. Keep the final assistant message brief."
               "Use lists and nested sublists for prose itemization. Use tables for short-cell data that benefits from column scanning, not for sentences or paragraphs; when table cells may grow, consider Org table width cookies such as | <20> | to keep columns readable."
               "Write durable output to document-uri below (the canonical canvas resource); it matches the <canvas> attachment uri. Do not write to the *e-org-canvas:...* / *e-org-canvas-input:...* helper buffers -- they are editor chrome, not the document, and editing them has no effect on the canvas."
               "Preserve the reader's fold state by default: the user may be mid-read, and collapsing or reflowing the buffer out from under them loses their place. Do not collapse the whole document (org_canvas_overview) or otherwise churn visibility as a routine post-edit step. Only change fold state when the user explicitly asks, or when you are deliberately presenting a fresh overview; when you must reveal something, reveal just the relevant subtree (org_canvas_show_context / org_canvas_cycle_heading) and leave the rest as the user left it. Editing or reverting can churn fold state as a side effect -- when it does, restore the user's prior visibility (re-reveal the section you changed) rather than collapsing everything."
               instructions
               (format "document-uri=%s buffer=%s point=%s"
                       (plist-get metadata :uri)
                       (plist-get metadata :buffer-name)
                       (or (plist-get focus :point) "nil"))
               (format "heading-path=%s"
                       (e-org-canvas--format-heading-path
                        (plist-get focus :heading-path)))
               (e-org-canvas--focus-summary focus)
               "Visibility state:"
               (e-org-canvas--format-visibility-state visibility))
              "\n"))))))

(defun e-org-canvas--require-tool-session (harness session-id)
  "Return the Org canvas buffer for HARNESS SESSION-ID or signal."
  (unless (and harness session-id (e-org-canvas-session-p harness session-id))
    (user-error "Not an Org Canvas session"))
  (or (e-org-canvas-session-buffer harness session-id)
      (user-error "Org Canvas session has no live Org buffer")))

(defun e-org-canvas--tool-target-point (arguments)
  "Return optional point from tool ARGUMENTS."
  (let ((point (plist-get arguments :point)))
    (and (numberp point) point)))

(defun e-org-canvas--tool-target-heading-path (arguments)
  "Return optional heading path from tool ARGUMENTS."
  (let ((path (or (plist-get arguments :heading_path)
                  (plist-get arguments :heading-path))))
    (cond
     ((vectorp path)
      (setq path (append path nil)))
     ((stringp path)
      (setq path (split-string path "/" t "[[:space:]\n]+"))))
    (when (and (listp path)
               (cl-every #'stringp path))
      path)))

(defun e-org-canvas--goto-heading-path (path)
  "Move point to Org heading PATH or signal a user error."
  (let ((target (mapcar #'string-trim path)))
    (goto-char (point-min))
    (catch 'found
      (while (re-search-forward org-heading-regexp nil t)
        (when (equal (e-org-canvas--heading-path) target)
          (org-back-to-heading t)
          (throw 'found (point))))
      (user-error "No Org heading matches path: %s"
                  (e-org-canvas--format-heading-path target)))))

(defun e-org-canvas--show-all ()
  "Show all Org headings without hard dependency on one Org fold API version."
  (funcall (if (fboundp 'org-fold-show-all)
               #'org-fold-show-all
             (intern "org-show-all"))))

(defun e-org-canvas--show-subtree ()
  "Show the current Org subtree across supported Org fold API versions."
  (funcall (if (fboundp 'org-fold-show-subtree)
               #'org-fold-show-subtree
             (intern "org-show-subtree"))))

(defun e-org-canvas--goto-tool-target (arguments)
  "Move point to the tool target described by ARGUMENTS."
  (if-let ((path (e-org-canvas--tool-target-heading-path arguments)))
      (e-org-canvas--goto-heading-path path)
    (when-let ((point (e-org-canvas--tool-target-point arguments)))
      (goto-char (max (point-min) (min (point-max) point))))))

(defun e-org-canvas--visibility-state-tool (buffer _arguments)
  "Return visibility state for BUFFER."
  (with-current-buffer buffer
    (e-org-canvas--format-visibility-state
     (e-org-canvas-visibility-state buffer))))

(defun e-org-canvas--show-all-tool (buffer _arguments)
  "Show all headings in BUFFER."
  (with-current-buffer buffer
    (e-org-canvas--show-all)
    "Shown all headings in the Org Canvas buffer."))

(defun e-org-canvas--overview-tool (buffer _arguments)
  "Collapse BUFFER to an overview."
  (with-current-buffer buffer
    (org-overview)
    "Collapsed the Org Canvas buffer to overview."))

(defun e-org-canvas--show-context-tool (buffer arguments)
  "Reveal context in BUFFER using ARGUMENTS."
  (with-current-buffer buffer
    (save-excursion
      (e-org-canvas--goto-tool-target arguments)
      (org-reveal)
      (when (e-org-canvas--inside-heading-p)
        (e-org-canvas--show-subtree)))
    "Revealed Org Canvas context."))

(defun e-org-canvas--cycle-heading-tool (buffer arguments)
  "Cycle one heading in BUFFER using ARGUMENTS."
  (with-current-buffer buffer
    (save-excursion
      (e-org-canvas--goto-tool-target arguments)
      (unless (e-org-canvas--inside-heading-p)
        (user-error "No Org heading at target"))
      (org-back-to-heading t)
      (pcase (or (plist-get arguments :operation) "cycle")
        ("show" (e-org-canvas--show-subtree))
        ("hide" (outline-hide-subtree))
        ("reveal" (org-reveal))
        (_ (org-cycle))))
    "Updated Org Canvas heading visibility."))

(defun e-org-canvas--register-tool
    (registry name description parameters harness session-id handler)
  "Register an Org Canvas tool in REGISTRY."
  (e-tools-register
   registry
   :name name
   :description description
   :parameters parameters
   :handler
   (lambda (arguments)
     (funcall handler
              (e-org-canvas--require-tool-session harness session-id)
              arguments))))

(defun e-org-canvas-register-tools (registry &rest context)
  "Register Org Canvas visibility tools in REGISTRY.
CONTEXT carries :harness and :session-id from the active turn."
  (let ((harness (plist-get context :harness))
        (session-id (plist-get context :session-id))
        (empty-object '(:type "object" :properties ())))
    (e-org-canvas--register-tool
     registry
     "org_canvas_visibility_state"
     "Return outline and visibility data for the current Org Canvas buffer."
     empty-object
     harness session-id
     #'e-org-canvas--visibility-state-tool)
    (e-org-canvas--register-tool
     registry
     "org_canvas_show_context"
     "Reveal ancestors and current subtree around an optional point or heading path."
     '(:type "object"
       :properties (:point (:type "number")
                    :heading_path (:type "array"
                                   :items (:type "string"))))
     harness session-id
     #'e-org-canvas--show-context-tool)
    (e-org-canvas--register-tool
     registry
     "org_canvas_cycle_heading"
     "Cycle, show, hide, or reveal one Org heading or subtree by point or heading path."
     '(:type "object"
       :properties (:point (:type "number")
                    :heading_path (:type "array"
                                   :items (:type "string"))
                    :operation (:type "string")))
     harness session-id
     #'e-org-canvas--cycle-heading-tool)
    (e-org-canvas--register-tool
     registry
     "org_canvas_show_all"
     "Show all headings in the current Org Canvas buffer."
     empty-object
     harness session-id
     #'e-org-canvas--show-all-tool)
    (e-org-canvas--register-tool
     registry
     "org_canvas_overview"
     "Collapse the current Org Canvas buffer to an overview."
     empty-object
     harness session-id
     #'e-org-canvas--overview-tool)))

(defun e-org-canvas-capability-create ()
  "Create the Org Canvas gated capability."
  (e-capability-create
   :id 'org-canvas
   :name "Org Canvas"
   :context-providers
   (list (e-context-provider-create
          :name 'org-canvas
          :priority 118
          :cache-placement 'dynamic-context
          :build #'e-org-canvas-context-provider))
   :tools (list #'e-org-canvas-register-tools)))

(defun e-org-canvas-layer-create ()
  "Create the Org Canvas layer."
  (e-layer-create
   :id 'org-canvas
   :name "Org Canvas"
   :capabilities (list (e-org-canvas-capability-create))))

(provide 'e-org-canvas-capabilities)

;;; e-org-canvas-capabilities.el ends here
