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
(require 'e-workspaces)
(require 'org)
(require 'org-element)
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

(defun e-org-canvas--metadata-ref (metadata)
  "Return stable Org Canvas reference from session METADATA."
  (or (plist-get metadata :org-canvas-ref)
      (plist-get metadata :org-canvas)))

(defun e-org-canvas-session-metadata (harness session-id)
  "Return Org Canvas stable reference for HARNESS SESSION-ID, or nil."
  (e-org-canvas--metadata-ref
   (plist-get (e-session-get (e-harness-sessions harness) session-id)
              :metadata)))

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

(defun e-org-canvas--last-prompt-metadata (harness session-id)
  "Return metadata from the last Org Canvas user prompt for SESSION-ID."
  (when (and harness session-id)
    (let ((message
           (cl-find-if
            (lambda (candidate)
              (and (eq (plist-get candidate :role) 'user)
                   (plist-get (plist-get candidate :metadata)
                              :org-canvas-scope)))
            (reverse (e-harness-messages harness session-id)))))
      (plist-get message :metadata))))

(defun e-org-canvas--context-provider-messages
    (metadata scope focus visibility)
  "Return Org Canvas context messages from cached or live state."
  (let ((instructions (if (eq scope 'document)
                          e-org-canvas--document-instructions
                        e-org-canvas--thread-instructions)))
    (list
     (list :role 'system
           :content
           (string-join
            (list
             "Org Canvas mode is active for this session."
             "Treat the Org buffer as the main interaction surface and primary output target. Put durable results in the Org document as structured Org content. Keep the final assistant message brief."
             "When editing Org prose, preserve sentence-per-line style: put each sentence on its own physical line and do not hard-wrap sentences to an artificial fill column; let Emacs visual-line/display wrapping handle width."
             "Use lists and nested sublists for prose itemization. Use tables for short-cell data that benefits from column scanning, not for sentences or paragraphs; when table cells may grow, consider Org table width cookies such as | <20> | to keep columns readable."
             "Write durable output to document-uri below (the canonical canvas resource); it matches the <canvas> attachment uri. Do not write to the *e-org-canvas:...* / *e-org-canvas-input:...* helper buffers -- they are editor chrome, not the document, and editing them has no effect on the canvas."
             "Preserve the reader's fold state by default: the user may be mid-read, and collapsing or reflowing the buffer out from under them loses their place. Do not collapse the whole document or otherwise churn visibility as a routine post-edit step. Only change fold state when the user explicitly asks, or when you are deliberately presenting a fresh overview; when you must reveal something, reveal just the relevant subtree with org-canvas actions and leave the rest as the user left it. Editing or reverting can churn fold state as a side effect -- when it does, restore the user's prior visibility (re-reveal the section you changed) rather than collapsing everything."
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
            "\n")))))

(cl-defun e-org-canvas-context-provider
    (&key harness session-id _turn-id _context-purpose)
  "Return Org Canvas context for HARNESS SESSION-ID when gated metadata exists."
  (when (and harness session-id (e-org-canvas-session-p harness session-id))
    (let* ((metadata (e-org-canvas-session-metadata harness session-id))
           (buffer (e-org-canvas-session-buffer harness session-id))
           (prompt-metadata (e-org-canvas--last-prompt-metadata
                             harness session-id))
           (scope (or (plist-get prompt-metadata :org-canvas-scope)
                      (plist-get (plist-get prompt-metadata :org-canvas-focus)
                                 :scope)
                      'thread))
           (focus (or (and (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (e-org-canvas-capture-focus scope)))
                      (plist-get prompt-metadata :org-canvas-focus)))
           (visibility (and (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (e-org-canvas-visibility-state)))))
      (e-org-canvas--context-provider-messages
       metadata scope focus visibility))))

(cl-defun e-org-canvas-context-snapshot-provider
    (&key harness session-id _turn-id _context-purpose)
  "Return cached Org Canvas context for optional status/snapshot callers."
  (when (and harness session-id (e-org-canvas-session-p harness session-id))
    (let* ((metadata (e-org-canvas-session-metadata harness session-id))
           (prompt-metadata (e-org-canvas--last-prompt-metadata
                             harness session-id))
           (focus (plist-get prompt-metadata :org-canvas-focus))
           (scope (or (plist-get prompt-metadata :org-canvas-scope)
                      (plist-get focus :scope)
                      'thread)))
      (e-org-canvas--context-provider-messages
       metadata scope focus nil))))

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


(defconst e-org-canvas--sentence-abbreviations
  '("e.g" "i.e" "etc" "vs" "cf" "al" "resp" "approx" "no" "vol" "pp" "sec"
    "mr" "mrs" "ms" "dr" "prof" "sr" "jr" "st" "inc" "ltd" "co" "fig" "eq")
  "Lowercased abbreviations whose trailing period does not end a sentence.
A single letter (an initial such as the J. in \"Julia J. Neumann\") is treated
like an abbreviation regardless of this list.")

(defun e-org-canvas--sentence-start-char-p (char)
  "Return non-nil when CHAR can begin a new prose sentence.
Uppercase letters, digits, and Org inline-markup openers all qualify, so a
break is only taken when what follows looks like a fresh sentence rather than a
mid-sentence lowercase continuation."
  (or (and (>= char ?A) (<= char ?Z))
      (and (>= char ?0) (<= char ?9))
      (memq char '(?= ?* ?/ ?~ ?_ ?\[ ?\( ?\" ?'))))

(defun e-org-canvas--sentence-abbrev-before-p (text pos)
  "Return non-nil when the word ending at POS in TEXT is an initial or abbrev.
POS is the index of the terminating period.  This guards decimals and dotted
tokens too, because the preceding word scan includes digits and interior dots."
  (let ((index pos))
    (while (and (> index 0)
                (let ((char (aref text (1- index))))
                  (or (and (>= char ?a) (<= char ?z))
                      (and (>= char ?A) (<= char ?Z))
                      (and (>= char ?0) (<= char ?9))
                      (memq char '(?. ?-)))))
      (setq index (1- index)))
    (let ((word (substring text index pos)))
      (or (= (length word) 1)
          (member (downcase (string-trim-right word "[.]+"))
                  e-org-canvas--sentence-abbreviations)))))

(defun e-org-canvas--sentence-protected-ranges (text)
  "Return (START . END) spans in TEXT where a sentence break is forbidden.
Inline code, verbatim, and links can contain periods that must not be read as
sentence terminators."
  (let ((case-fold-search nil) ranges)
    (dolist (regexp '("\\[\\[\\(?:[^][]\\|\\]\\[\\)*\\]\\]"
                      "=[^=\n]+="
                      "~[^~\n]+~"))
      (let ((start 0))
        (while (string-match regexp text start)
          (push (cons (match-beginning 0) (match-end 0)) ranges)
          (setq start (match-end 0)))))
    ranges))

(defun e-org-canvas--sentence-in-protected-p (ranges pos)
  "Return non-nil when POS falls inside one of the protected RANGES."
  (cl-some (lambda (range) (and (>= pos (car range)) (< pos (cdr range))))
           ranges))

(defun e-org-canvas--sentence-break-end (text pos ranges)
  "Return the index just past a sentence ending at POS in TEXT, or nil.
POS is the index of a terminating punctuation char.  RANGES are protected spans.
A break is taken only when the punctuation run is followed by whitespace and a
sentence-starting character, and the word before it is not an initial or abbrev."
  (unless (e-org-canvas--sentence-in-protected-p ranges pos)
    (let ((len (length text)) (index pos))
      (while (and (< index len) (memq (aref text index) '(?. ?! ??)))
        (setq index (1+ index)))
      (while (and (< index len) (memq (aref text index) '(?\" ?' ?\) ?\] ?})))
        (setq index (1+ index)))
      (cond
       ((= index len) nil)
       ((memq (aref text index) '(?\s ?\t))
        (let ((next index))
          (while (and (< next len) (memq (aref text next) '(?\s ?\t)))
            (setq next (1+ next)))
          (and (< next len)
               (e-org-canvas--sentence-start-char-p (aref text next))
               (not (e-org-canvas--sentence-abbrev-before-p text pos))
               index)))
       (t nil)))))

(defun e-org-canvas--split-sentences (text)
  "Split TEXT into a list of sentences for sentence-per-line prose.
Returns TEXT trimmed as a single element when no interior break is found."
  (let* ((case-fold-search nil) (len (length text)) (start 0) (pos 0)
         (ranges (e-org-canvas--sentence-protected-ranges text)) result)
    (while (< pos len)
      (let ((break-end (and (memq (aref text pos) '(?. ?! ??))
                            (e-org-canvas--sentence-break-end text pos ranges))))
        (if break-end
            (progn
              (push (string-trim (substring text start break-end)) result)
              (setq pos break-end)
              (while (and (< pos len) (memq (aref text pos) '(?\s ?\t)))
                (setq pos (1+ pos)))
              (setq start pos))
          (setq pos (1+ pos)))))
    (when (< start len)
      (push (string-trim (substring text start)) result))
    (or (nreverse (delete "" result)) (list (string-trim text)))))

(defun e-org-canvas--paragraph-regions (buffer)
  "Return prose paragraph (BEGIN . END) regions in BUFFER, latest first.
Only `paragraph' elements are collected, so source blocks, tables, example and
verse blocks, and keywords are left untouched.  Regions are sorted descending so
editing one does not shift the positions of those not yet processed."
  (with-current-buffer buffer
    (let (regions)
      (org-element-map (org-element-parse-buffer) 'paragraph
        (lambda (element)
          (let ((begin (org-element-property :contents-begin element))
                (end (org-element-property :contents-end element)))
            (when (and begin end)
              (push (cons begin end) regions)))))
      (sort regions (lambda (a b) (> (car a) (car b)))))))

(defun e-org-canvas--reflow-region (begin end)
  "Rewrite the paragraph text between BEGIN and END to one sentence per line.
Joins the existing physical lines, re-splits into sentences, and indents
continuation lines to the paragraph's body column so list items stay attached.
Returns non-nil when the buffer text actually changed."
  (let* ((indent (save-excursion (goto-char begin) (current-column)))
         (prefix (make-string indent ?\s))
         (raw (buffer-substring-no-properties begin end))
         (trailing-newline (string-suffix-p "\n" raw))
         (body (string-trim-right raw "\n"))
         (flowed (mapconcat #'string-trim (split-string body "\n") " "))
         (sentences (e-org-canvas--split-sentences flowed))
         (rebuilt (concat (mapconcat #'identity sentences (concat "\n" prefix))
                          (if trailing-newline "\n" ""))))
    (unless (string= rebuilt raw)
      (goto-char begin)
      (delete-region begin end)
      (insert rebuilt)
      t)))

(defun e-org-canvas-reflow-sentences-in-buffer (&optional buffer)
  "Rewrite prose in BUFFER (or current buffer) to one sentence per physical line.
Operates only on Org prose paragraphs, preserves outline visibility and point,
and returns the number of paragraphs whose text changed."
  (with-current-buffer (or buffer (current-buffer))
    (unless (derived-mode-p 'org-mode)
      (user-error "Sentence reflow only applies to Org buffers"))
    (let ((changed 0))
      (org-save-outline-visibility t
        (save-excursion
          (dolist (region (e-org-canvas--paragraph-regions (current-buffer)))
            (when (e-org-canvas--reflow-region (car region) (cdr region))
              (setq changed (1+ changed))))))
      changed)))

(defun e-org-canvas--reflow-sentences-tool (buffer _arguments)
  "Reflow prose in the Org Canvas BUFFER to one sentence per line."
  (let ((changed (e-org-canvas-reflow-sentences-in-buffer buffer)))
    (if (zerop changed)
        "Prose already uses one sentence per line; no changes made."
      (format "Reflowed %d paragraph%s to one sentence per line."
              changed (if (= changed 1) "" "s")))))

(defun e-org-canvas--action (description handler &optional parameters)
  "Return an Org Canvas action descriptor for HANDLER."
  (e-action-cheap-create
   :owner 'org-canvas
   :runner (lambda (arguments context)
             (funcall handler
                      (e-org-canvas--require-tool-session
                       (plist-get context :harness)
                       (plist-get context :session-id))
                      arguments))
   :description description
   :parameters (or parameters '(:type "object" :properties nil))
   :requires-session t))

(defun e-org-canvas--actions ()
  "Return Org Canvas action plist."
  (let ((empty-object '(:type "object" :properties ())))
    (list
     :visibility-state
     (e-org-canvas--action
      "Return outline and visibility data for the current Org Canvas buffer."
      #'e-org-canvas--visibility-state-tool
      empty-object)
     :show-context
     (e-org-canvas--action
      "Reveal ancestors and current subtree around an optional point or heading path."
      #'e-org-canvas--show-context-tool
      '(:type "object"
        :properties (:point (:type "number")
                     :heading_path (:type "array"
                                    :items (:type "string")))))
     :cycle-heading
     (e-org-canvas--action
      "Cycle, show, hide, or reveal one Org heading or subtree by point or heading path."
      #'e-org-canvas--cycle-heading-tool
      '(:type "object"
        :properties (:point (:type "number")
                     :heading_path (:type "array"
                                    :items (:type "string"))
                     :operation (:type "string"))))
     :show-all
     (e-org-canvas--action
      "Show all headings in the current Org Canvas buffer."
      #'e-org-canvas--show-all-tool
      empty-object)
     :overview
     (e-org-canvas--action
      "Collapse the current Org Canvas buffer to an overview."
      #'e-org-canvas--overview-tool
      empty-object)
     :reflow-sentences
     (e-org-canvas--action
      "Rewrite Org prose in the current canvas to one sentence per physical line, indenting list-item continuations and leaving code blocks, tables, links, and inline code untouched."
      #'e-org-canvas--reflow-sentences-tool
      empty-object))))

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
          :build #'e-org-canvas-context-provider
          :snapshot-build #'e-org-canvas-context-snapshot-provider))
   :actions (e-org-canvas--actions)))

(defun e-org-canvas-layer-create ()
  "Create the Org Canvas layer."
  (e-layer-create
   :id 'org-canvas
   :name "Org Canvas"
   :capabilities (list (e-org-canvas-capability-create))))

(provide 'e-org-canvas-capabilities)

;;; e-org-canvas-capabilities.el ends here
