;;; e-voice-adjustment.el --- Detect and cache LLM writing tells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The voice-adjustment capability keeps agent-authored prose in a plain,
;; direct voice instead of the ceremonial "LLM voice."  Its active check and
;; learning workflow is discoverable on demand rather than run on every turn.
;;
;;   Passive avoidance: the main agent sees only a compact list of cached tell
;;   labels, so it can avoid known habits without running a separate check.
;;
;;   On-demand check and learning: when explicitly requested and prose carries
;;   a tell --
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
(require 'e-skills)
(require 'e-voice-storage)

(defgroup e-voice-adjustment nil
  "Voice adjustment: detect, rewrite, and cache LLM writing tells."
  :group 'e
  :prefix "e-voice-adjustment-")

(defcustom e-voice-adjustment-max-tells 128
  "Maximum number of cached tells retained in the LRU store."
  :type 'integer
  :group 'e-voice-adjustment)

(defconst e-voice-adjustment-guide
  (string-join
   '("# Voice adjustment"
     ""
     "Use this workflow only when the user asks for a voice check or rewrite, or another explicit instruction requires one. Do not run an extra voice-check pass on ordinary responses."
     ""
     "The harness may include one compact line of cached tell labels. Avoid those habits while drafting reader-facing prose, but do not treat that passive reminder as a request to run this workflow. Read e://voice-adjustment/tells for the full cached descriptions."
     ""
     "## Check and rewrite"
     ""
     "Look for grandiose framing, bolded-not stress, em-dash restatement, formulaic scaffolding, antithesis kickers, intensifier tics, tidy aphoristic closers, and catalog or system-tour prose. Rewrite plainly: state the fact once, cut the stress and restatement, and let the fact carry its own weight."
     ""
     "Keep the check scoped to prose a reader will see. Do not apply it to code, identifiers, quoted source text, or a verbatim requirement."
     ""
     "## Actions"
     ""
     "Call actions from run_elisp; there are no dedicated model-facing voice tools:"
     ""
     "- List cached tells: (e-actions-call 'voice-adjustment :list nil)"
     "- Record a corrected tell: (e-actions-call 'voice-adjustment :record '(:label \"stable label\" :description \"One-line description.\"))"
     "- Clear cached tells only when explicitly requested: (e-actions-call 'voice-adjustment :clear nil)"
     ""
     "Recording refreshes an existing normalized label and moves it to the front. A new label is prepended. The least-recently-used store is capped, so reuse a stable label instead of creating near-duplicates.")
   "\n")
  "Detailed on-demand voice-adjustment guide.")

;;;; Persistent least-recently-used store

(defvar e-voice-adjustment--tells nil
  "In-memory cache of tells, most-recently-used first.
Each entry is a plist with :key, :label, :description, :count, :last.")

(defvar e-voice-adjustment--loaded nil
  "Non-nil once the persistent store has been hydrated this session.")

(defvar e-voice-adjustment-storage nil
  "Optional voice-owned SQLite storage port.")

(defun e-voice-adjustment-configure-storage (storage)
  "Install voice STORAGE, or nil while no runtime is active."
  (unless (or (null storage) (e-voice-storage-p storage))
    (signal 'wrong-type-argument (list 'e-voice-storage-p storage)))
  (setq e-voice-adjustment-storage storage
        e-voice-adjustment--loaded nil
        e-voice-adjustment--tells nil))

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
    (when e-voice-adjustment-storage
      (setq e-voice-adjustment--tells
            (plist-get
             (e-voice-storage-list e-voice-adjustment-storage
                                   e-voice-adjustment-max-tells)
             :tells))))
  e-voice-adjustment--tells)

(defun e-voice-adjustment--record (label description)
  "Record a tell LABEL with DESCRIPTION, refreshing LRU order.
An existing tell (matched by normalized LABEL) is refreshed and moved to the
front; a new tell is prepended and the store is truncated to
`e-voice-adjustment-max-tells', evicting the least-recently-used entries."
  (e-voice-adjustment--load)
  (let* ((label (string-trim (or label "")))
         (_ (when (string-empty-p label)
              (user-error "Voice-adjustment record requires a non-empty label")))
         (key (e-voice-adjustment--normalize-key label))
         (existing (seq-find (lambda (tell)
                               (equal key (plist-get tell :key)))
                             e-voice-adjustment--tells))
         (now (e-voice-adjustment--timestamp)))
    (if e-voice-adjustment-storage
        (let ((result
               (e-voice-storage-record
                e-voice-adjustment-storage key label
                (and description
                     (not (string-empty-p (string-trim description)))
                     (string-trim description))
                now e-voice-adjustment-max-tells)))
          (setq e-voice-adjustment--tells (plist-get result :tells)))
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
              (seq-take e-voice-adjustment--tells
                        e-voice-adjustment-max-tells)))
      )
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
  (when e-voice-adjustment-storage
    (e-voice-storage-clear e-voice-adjustment-storage))
  (setq e-voice-adjustment--tells nil
        e-voice-adjustment--loaded t)
  (list :count 0))

;;;; Compact passive context

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
  "Return compact tell-avoidance context, or nil when the cache is empty."
  (let ((tells (plist-get (e-voice-adjustment--list) :tells)))
    (when tells
      (list
       (list :role 'system
             :content
             (format
              "Reader-facing prose: avoid cached writing tells: %s."
              (mapconcat (lambda (tell) (plist-get tell :label))
                         tells "; ")))))))

;;;; Resources and actions

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
  "Parameters for the voice-adjustment record action.")

(defun e-voice-adjustment--action (id description parameters runner)
  "Return a cheap voice-adjustment action ID described by DESCRIPTION.
PARAMETERS is its input schema and RUNNER implements the action."
  (e-action-cheap-create
   :id id
   :owner 'voice-adjustment
   :description description
   :parameters parameters
   :runner runner))

(defun e-voice-adjustment-capability-create ()
  "Create the voice-adjustment capability."
  (e-capability-with-skills-create
   :id 'voice-adjustment
   :name "Voice Adjustment"
   :instruction-priority 210
   :skill-heading "Voice adjustment is available on request:"
   :skills
   (list
    (e-skill-spec-create
     :name "voice-adjustment"
     :description "Check requested prose and manage cached writing tells."
     :content e-voice-adjustment-guide))
   :resources (list #'e-voice-adjustment--register-resources)
   :context-providers
   (list (e-context-provider-create
          :name 'voice-adjustment
          :priority 210
          :cache-placement 'dynamic-context
          :build #'e-voice-adjustment--context-provider))
   :actions
   (list
    :record
    (e-voice-adjustment--action
     "voice_adjustment_record"
     "Record a corrected writing tell and refresh its LRU position."
     e-voice-adjustment--record-parameters
     (lambda (arguments _context)
       (e-voice-adjustment--record
        (plist-get arguments :label)
        (plist-get arguments :description))))
    :list
    (e-voice-adjustment--action
     "voice_adjustment_list"
     "List cached writing tells, most recently corrected first."
     '(:type "object" :properties nil)
     (lambda (_arguments _context)
       (e-voice-adjustment--list)))
    :clear
    (e-voice-adjustment--action
     "voice_adjustment_clear"
     "Clear every cached writing tell."
     '(:type "object" :properties nil)
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
