;;; e-output-style.el --- Output style guidance capability for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Output style is a named block of behavioral guidance that strongly shapes
;; how the model writes its prose responses.  It rides the existing system
;; channel as a high-priority guidance capability: its effective instructions
;; are the active style's prose, injected as a `static-prefix' system fragment
;; ahead of conversation turns.
;;
;; Selecting a style is capability configuration, reusing `e-capability-config'
;; for global, project (directory-local), and construction-time precedence.  A
;; style is inert until selected: with no `:style' configured the capability
;; contributes nothing.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-capability-config)

(defconst e-output-style-instruction-priority 260
  "Instruction priority for the `output-style' capability.
Output style is strong, late-binding voice guidance.  It must sort after the
operational guidance (file/shell safety, tool contracts) so it shapes prose
without overriding correctness rules.  Lower priority sorts earlier; the OS
base guidance is 230 and the capability default is 200, so 260 lands the style
after operational guidance while remaining a leading `static-prefix' system
fragment (good for prompt-cache stability).")

(defcustom e-output-style-registry
  (list
   (cons 'concise
         (list :name "Concise"
               :instructions
               "Write concise, direct prose.  Lead with the answer or
conclusion, then add only the support that is needed.  Drop preamble,
restatement of the question, and filler.  Prefer short sentences and tight
paragraphs.  When a list is clearer than prose, use one."))
   (cons 'explanatory
         (list :name "Explanatory"
               :instructions
               "Write in a teaching voice.  Explain your reasoning, the
tradeoffs you weighed, and why you chose this approach as you work, so the
reader learns the underlying ideas and not just the result.  Surface
assumptions and name alternatives you rejected.  Stay concrete; ground
explanations in the specifics at hand rather than abstractions."))
   (cons 'hemingway
         (list :name "Hemingway"
               :instructions
               "Write in a clean, strong prose style.  This is not a request to
imitate Ernest Hemingway, mention his works, or cosplay his voice.  Use plain
declarative sentences.  Put one idea in each.  Choose short, common words over
long ones.  Prefer strong verbs to adjectives and adverbs.  Avoid flowery,
ornate, or unnecessary language.  Cut every word that does not carry weight.  Do
not hedge or pad.  Let the facts stand on their own."))
   (cons 'hemingway-suggested-v2
         (list :name "Hemingway Suggested v2"
               :instructions
               "Write in a clean, strong prose style.  This is not a request to
imitate Ernest Hemingway, mention his works, or cosplay his voice.  Use plain
declarative sentences.  Put one clear claim or action in each sentence.  Choose
short, common words over long ones.  Prefer concrete facts and direct verbs over
abstraction, adjectives, and adverbs.  Avoid flowery, ornate, or unnecessary
language.  Cut padding and throat-clearing.  Do not hedge when the evidence is
clear.  If uncertainty matters, state it plainly.  Let the facts carry the
force."))
   (cons 'hemingway-v3
         (list :name "Hemingway v3"
               :instructions
               "Write in a clean, strong prose style.
This is not a request to imitate Ernest Hemingway, mention his works, or cosplay his voice.
Use plain declarative sentences.
Put one clear claim or action in each sentence.
Choose short, common words over long ones.
Prefer concrete facts and direct verbs over abstraction, adjectives, adverbs, and needless jargon.
Use project terms when they name real things, but choose plain words when they work.
Avoid flowery, ornate, or unnecessary language.
Cut padding and throat-clearing.
Do not hedge when the evidence is clear.
If uncertainty matters, state it plainly.
Let the facts carry the force."))
   (cons 'caveman
         (list :name "Caveman"
               :instructions
               "Respond in ultra-compressed prose while keeping full technical
accuracy.  Drop filler, pleasantries, hedging, and tool-call narration.  Prefer
fragments when clear.  Use short synonyms: big, not extensive; fix, not
implement a solution for.  Keep technical terms, code, API names, CLI commands,
commit keywords, paths, and exact error strings unchanged.  Preserve the user's
dominant language.  Do not name or announce the style.  Use the pattern: thing,
action, reason, next step.  No decorative tables or emoji unless they add real
clarity.  Quote only the shortest decisive error line unless the user asks for
more.  Use normal prose for security warnings, irreversible confirmations, and
multi-step sequences where compression would create ambiguity.")))
  "Registry of named output styles.
Each entry maps a style id symbol to a plist with `:name' and `:instructions'.
Users add styles by registering them via `e-output-style-register'."
  :type '(alist :key-type symbol
                :value-type (plist :key-type symbol :value-type string))
  :group 'e)

(cl-defun e-output-style-register (id &key name instructions)
  "Register or replace output style ID with NAME and INSTRUCTIONS."
  (unless (symbolp id)
    (signal 'wrong-type-argument (list 'symbolp id)))
  (unless (and (stringp instructions) (not (string-empty-p instructions)))
    (signal 'wrong-type-argument (list 'stringp instructions)))
  (setf (alist-get id e-output-style-registry)
        (list :name (or name (symbol-name id))
              :instructions instructions))
  id)

(defun e-output-style-ids ()
  "Return the list of registered output style ids."
  (mapcar #'car e-output-style-registry))

(defun e-output-style--entry (style-id)
  "Return the registry entry plist for STYLE-ID, or nil."
  (alist-get style-id e-output-style-registry))

(defun e-output-style--resolve (style-id)
  "Return the instructions string for STYLE-ID.
Signal an error naming STYLE-ID and the known ids when it is unregistered."
  (let ((entry (e-output-style--entry style-id)))
    (unless entry
      (error "Unknown output style `%s'; known styles: %s"
             style-id
             (mapconcat #'symbol-name (e-output-style-ids) ", ")))
    (plist-get entry :instructions)))

(defconst e-output-style--config-options
  (list (e-capability-config-option-create
         :key :style
         :type 'symbol
         :default nil
         :documentation
         "Active output style id; one of the ids in `e-output-style-registry'.
nil leaves the model's default voice."
         :validator
         (lambda (value)
           (or (null value)
               (and (symbolp value) (e-output-style--entry value))))))
  "Config option specs owned by the `output-style' capability.")

(defun e-output-style--configured-id (&optional directory)
  "Return the configured output style id for DIRECTORY, or nil."
  (plist-get
   (e-capability-config-resolve
    'output-style e-output-style--config-options
    :directory (or directory default-directory))
   :style))

(defun e-output-style-capability-create (&optional directory)
  "Create the `output-style' capability resolved for DIRECTORY.
The capability's instructions are the active style's prose, or nil when no
style is configured (the capability then contributes nothing)."
  (e-capability-config-register-options
   'output-style e-output-style--config-options)
  (let* ((style-id (e-output-style--configured-id directory))
         (instructions (and style-id (e-output-style--resolve style-id))))
    (e-capability-create
     :id 'output-style
     :name "Output Style"
     :instructions instructions
     :instruction-priority e-output-style-instruction-priority
     :config-options e-output-style--config-options)))

(defun e-output-style--set-config (style-id)
  "Set the active output style to STYLE-ID in `e-capability-config'.
A nil STYLE-ID clears the style.  Signal on an unknown non-nil id."
  (when style-id
    (e-output-style--resolve style-id))
  (let ((plist (cdr (assq 'output-style e-capability-config))))
    (setf (alist-get 'output-style e-capability-config)
          (plist-put plist :style style-id)))
  style-id)

(defun e-output-style--describe-string (&optional directory)
  "Return a description of the active output style for DIRECTORY."
  (let ((style-id (e-output-style--configured-id directory)))
    (if style-id
        (format "Active output style: %s\n\n%s"
                style-id (e-output-style--resolve style-id))
      "No output style is active; the model uses its default voice.")))

(defun e-output-style-set (style-id)
  "Interactively set the active output style to STYLE-ID.
Choosing the empty selection clears the active style."
  (interactive
   (let* ((choices (mapcar (lambda (id) (symbol-name id))
                           (e-output-style-ids)))
          (choice (completing-read
                   "Output style (empty to clear): " choices nil t)))
     (list (and (not (string-empty-p choice)) (intern choice)))))
  (e-output-style--set-config style-id)
  (message
   (if style-id
       "Output style set to `%s'; new turns pick it up."
     "Output style cleared; new turns use the default voice.")
   style-id))

(defun e-output-style-describe (&optional directory)
  "Describe the active output style for DIRECTORY."
  (interactive (list default-directory))
  (let ((text (e-output-style--describe-string directory)))
    (if (called-interactively-p 'interactive)
        (with-help-window "*e-output-style*"
          (princ text))
      text)))

(provide 'e-output-style)

;;; e-output-style.el ends here
