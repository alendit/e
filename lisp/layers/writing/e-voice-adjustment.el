;;; e-voice-adjustment.el --- Detect and cache LLM writing tells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The voice-adjustment capability formalizes a two-phase workflow for keeping
;; agent-authored prose in a plain, direct voice instead of the ceremonial
;; "LLM voice."
;;
;;   Phase 1 (first pass, avoidance): before drafting, the main agent sees a
;;   small cached list of the tells it has most recently had to correct, so it
;;   avoids them up front rather than writing then rewriting.
;;
;;   Phase 2 (detection, rewrite, learning): when prose still carries a tell --
;;   grandiose framing, bolded-not stress, em-dash restatement, formulaic
;;   scaffolding, antithesis kickers, intensifier tics, catalog/system-tour
;;   prose -- the agent flags it, rewrites the passage plainly, and records the
;;   tell so it feeds the next first pass.
;;
;; The recorded tells live in a persistent, least-recently-used store capped at
;; a small size (10 by default).  Recording an existing tell refreshes it and
;; moves it to the front; a new tell past the cap evicts the least-recently
;; used entry.  This keeps the first-pass guidance short and current rather than
;; an ever-growing checklist.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-actions)
(require 'e-capabilities)
(require 'e-context)
(require 'e-layers)
(require 'e-tools)

(defgroup e-voice-adjustment nil
  "Voice adjustment: detect, rewrite, and cache LLM writing tells."
  :group 'e
  :prefix "e-voice-adjustment-")

(defcustom e-voice-adjustment-store-file
  (locate-user-emacs-file "e/voice-tells.eld")
  "File persisting the least-recently-used cache of detected writing tells.
Set to nil to keep the cache in memory only for the session."
  :type '(choice (const :tag "In-memory only" nil) file)
  :group 'e-voice-adjustment)

(defcustom e-voice-adjustment-max-tells 10
  "Maximum number of cached tells retained in the LRU store."
  :type 'integer
  :group 'e-voice-adjustment)

(defconst e-voice-adjustment-instructions
  (string-join
   '("The voice-adjustment capability keeps prose you author in a plain, direct voice and out of the ceremonial \"LLM voice.\" It runs in two phases and learns across turns."
     "First pass (avoidance): before drafting reader-facing prose, consult the cached tells below (read e://voice-adjustment/tells or call voice_tells_list) and write so the draft does not exhibit them. These are the moves you most recently had to correct; avoiding them up front beats writing then rewriting."
     "Detection and rewrite: when a passage still reads as an LLM tell -- grandiose or epigrammatic framing dressing a plain fact as a Principle, bolded-not stress on a plain negative, em-dash restatement that says the same thing twice, formulaic scaffolding (\"This is what X\", \"It also answers\", \"This refines the earlier\"), antithesis kickers (\"X, not Y\"), intensifier tics (repeated \"exactly\"/\"precisely\"), tidy aphoristic closers, catalog or system-tour prose -- rewrite it in plain engineer voice: state the fact once, let it carry its own weight, cut the stress and the restatement."
     "Learning: after you correct a tell, record it with voice_tells_record (a short label plus a one-line description). Recording refreshes an existing tell and moves it to the front; a genuinely new tell is added. The store is capped and least-recently-used, so keep labels stable (reuse the same label for the same move) rather than inventing a near-duplicate each time."
     "Keep this scoped to prose a reader will see. Do not apply it to code, identifiers, quoted source text, or a verbatim requirement.")
   "\n")
  "Instructions contributed by the voice-adjustment capability.")

;;;; Persistent least-recently-used store

(defvar e-voice-adjustment--tells nil
  "In-memory cache of tells, most-recently-used first.
Each entry is a plist with :key, :label, :description, :count, :last.")

(defvar e-voice-adjustment--loaded nil
  "Non-nil once the persistent store has been hydrated this session.")

(defun e-voice-adjustment--timestamp ()
  "Return an ISO-8601 UTC timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun e-voice-adjustment--normalize-key (label)
  "Return a stable comparison key for tell LABEL."
  (let ((text (string-trim (downcase (or label "")))))
    (replace-regexp-in-string "[[:space:]]+" " " text)))

(defun e-voice-adjustment--load ()
  "Hydrate `e-voice-adjustment--tells' from disk once per session."
  (unless e-voice-adjustment--loaded
    (setq e-voice-adjustment--loaded t)
    (when (and e-voice-adjustment-store-file
               (file-readable-p e-voice-adjustment-store-file))
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents e-voice-adjustment-store-file)
          (let ((data (read (current-buffer))))
            (when (listp data)
              (setq e-voice-adjustment--tells data)))))))
  e-voice-adjustment--tells)

(defun e-voice-adjustment--write ()
  "Persist `e-voice-adjustment--tells' to disk atomically.
No-op when persistence is disabled (`e-voice-adjustment-store-file' nil)."
  (when e-voice-adjustment-store-file
    (ignore-errors
      (make-directory (file-name-directory e-voice-adjustment-store-file) t)
      (let ((tmp (make-temp-file
                  (expand-file-name
                   ".voice-tells-"
                   (file-name-directory e-voice-adjustment-store-file)))))
        (with-temp-file tmp
          (let ((print-length nil) (print-level nil))
            (prin1 e-voice-adjustment--tells (current-buffer))))
        (rename-file tmp e-voice-adjustment-store-file t)))))

(defun e-voice-adjustment--record (label description)
  "Record a tell LABEL with DESCRIPTION, refreshing LRU order.
An existing tell (matched by normalized LABEL) is refreshed and moved to the
front; a new tell is prepended and the store is truncated to
`e-voice-adjustment-max-tells', evicting the least-recently-used entries."
  (e-voice-adjustment--load)
  (let* ((label (string-trim (or label "")))
         (_ (when (string-empty-p label)
              (user-error "voice_tells_record requires a non-empty label")))
         (key (e-voice-adjustment--normalize-key label))
         (existing (seq-find (lambda (tell)
                               (equal key (plist-get tell :key)))
                             e-voice-adjustment--tells))
         (now (e-voice-adjustment--timestamp)))
    (setq e-voice-adjustment--tells
          (seq-remove (lambda (tell) (equal key (plist-get tell :key)))
                      e-voice-adjustment--tells))
    (let ((entry (list :key key
                       :label label
                       :description (and description
                                         (not (string-empty-p
                                               (string-trim description)))
                                         (string-trim description))
                       :count (1+ (or (and existing (plist-get existing :count))
                                      0))
                       :last now)))
      ;; Keep the earlier description when a refresh omits one.
      (when (and existing (not (plist-get entry :description)))
        (setq entry (plist-put entry :description
                               (plist-get existing :description))))
      (push entry e-voice-adjustment--tells))
    (when (> (length e-voice-adjustment--tells) e-voice-adjustment-max-tells)
      (setq e-voice-adjustment--tells
            (seq-take e-voice-adjustment--tells e-voice-adjustment-max-tells)))
    (e-voice-adjustment--write)
    (list :key key
          :label label
          :retained (length e-voice-adjustment--tells))))

(defun e-voice-adjustment--list ()
  "Return cached tells, most-recently-used first."
  (e-voice-adjustment--load)
  (list :max e-voice-adjustment-max-tells
        :count (length e-voice-adjustment--tells)
        :tells (copy-sequence e-voice-adjustment--tells)))

(defun e-voice-adjustment--clear ()
  "Drop every cached tell and clear the persistent store."
  (setq e-voice-adjustment--tells nil
        e-voice-adjustment--loaded t)
  (e-voice-adjustment--write)
  (list :count 0))

;;;; First-pass context

(defun e-voice-adjustment--format-tells (tells)
  "Return a compact human-readable rendering of cached TELLS."
  (if (null tells)
      "  (none recorded yet)"
    (mapconcat
     (lambda (tell)
       (let ((label (plist-get tell :label))
             (description (plist-get tell :description)))
         (if description
             (format "  - %s: %s" label description)
           (format "  - %s" label))))
     tells
     "\n")))

(cl-defun e-voice-adjustment--context-provider
    (&key _harness _session-id _turn-id _context-purpose)
  "Return first-pass tell-avoidance context, or nil when the cache is empty."
  (let ((tells (plist-get (e-voice-adjustment--list) :tells)))
    (when tells
      (list
       (list :role 'system
             :content
             (concat
              "Voice-adjustment first pass. You have recently had to correct "
              "these writing tells. Draft reader-facing prose so it does not "
              "exhibit them; do not write then rewrite. Recently-corrected "
              "tells, most recent first:\n"
              (e-voice-adjustment--format-tells tells)))))))

;;;; Resource, tools, actions

(defun e-voice-adjustment--register-resources (store capability)
  "Register the readable cached-tells resource for CAPABILITY in STORE."
  (e-store-register
   store (e-capability-id capability) "tells"
   :description "Cached least-recently-used list of detected writing tells."
   :reader
   (lambda (&rest _)
     (let ((tells (plist-get (e-voice-adjustment--list) :tells)))
       (concat "# Cached writing tells (LRU, max "
               (number-to-string e-voice-adjustment-max-tells) ")\n\n"
               (e-voice-adjustment--format-tells tells) "\n")))))

(defconst e-voice-adjustment--record-parameters
  '(:type "object"
    :required ["label"]
    :properties
    (:label (:type "string"
             :description "Short stable name for the tell, e.g. \"bolded-not stress\". Reuse the same label for the same move.")
     :description (:type "string"
                   :description "One-line description of what the tell is and why it reads as an LLM voice.")))
  "Parameters for the voice_tells_record tool/action.")

(defun e-voice-adjustment-register-tool (registry &rest _context)
  "Register voice-adjustment model-facing tools in REGISTRY."
  (e-tools-register
   registry
   :name "voice_tells_record"
   :description "Record a detected writing tell after correcting it, refreshing the least-recently-used cache used for the next first pass."
   :parameters e-voice-adjustment--record-parameters
   :blocking-class 'cheap
   :work
   (e-tools-cheap-work
    "tool.voice-tells-record"
    (lambda (arguments)
      (e-voice-adjustment--record
       (plist-get arguments :label)
       (plist-get arguments :description)))))
  (e-tools-register
   registry
   :name "voice_tells_list"
   :description "List the cached least-recently-used writing tells to avoid on this draft."
   :parameters '(:type "object" :properties nil)
   :blocking-class 'cheap
   :work
   (e-tools-cheap-work
    "tool.voice-tells-list"
    (lambda (_arguments)
      (e-voice-adjustment--list)))))

(defun e-voice-adjustment--action (id parameters runner)
  "Return a cheap voice-adjustment action ID with PARAMETERS and RUNNER."
  (e-action-cheap-create
   :id id
   :owner 'voice-adjustment
   :parameters parameters
   :runner runner))

(defun e-voice-adjustment-capability-create ()
  "Create the voice-adjustment capability."
  (e-capability-create
   :id 'voice-adjustment
   :name "Voice Adjustment"
   :instruction-priority 210
   :instructions e-voice-adjustment-instructions
   :tools (list #'e-voice-adjustment-register-tool)
   :resources (list #'e-voice-adjustment--register-resources)
   :context-providers
   (list (e-context-provider-create
          :name 'voice-adjustment
          :priority 210
          :cache-placement 'stable-context
          :build #'e-voice-adjustment--context-provider))
   :actions
   (list
    :record
    (e-voice-adjustment--action
     "voice_tells_record" e-voice-adjustment--record-parameters
     (lambda (arguments _context)
       (e-voice-adjustment--record
        (plist-get arguments :label)
        (plist-get arguments :description))))
    :list
    (e-voice-adjustment--action
     "voice_tells_list" '(:type "object" :properties nil)
     (lambda (_arguments _context)
       (e-voice-adjustment--list)))
    :clear
    (e-voice-adjustment--action
     "voice_tells_clear" '(:type "object" :properties nil)
     (lambda (_arguments _context)
       (e-voice-adjustment--clear))))))

(defun e-voice-adjustment-layer-create ()
  "Create the writing layer packaging the voice-adjustment capability."
  (e-layer-create
   :id 'writing
   :name "Writing"
   :capabilities (list (e-voice-adjustment-capability-create))))

(provide 'e-voice-adjustment)

;;; e-voice-adjustment.el ends here
