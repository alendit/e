;;; e-structured-blocks.el --- Structured-block display registry for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A capability may emit a structured, non-prose block inside otherwise
;; free-text assistant output (a reasoning mark, a collapsible tool-plan, a
;; citation) that a presentation shell should locate and treat specially.
;; Without a shared seam, the shell would have to know the specific
;; capability's block syntax, pointing a dependency arrow from presentation
;; toward capability policy -- the wrong direction.
;;
;; This registry inverts that dependency.  A capability registers a block
;; `:kind' plus how to find it (`:matcher') and how it should be displayed
;; (`:display'); a shell asks the single core transform, `e-structured-blocks-render',
;; for display-ready text and gets it back unchanged when no kind matches.
;; Neither side knows the other; both depend only on this module.
;;
;; This module intentionally requires no shell or capability code, so loading
;; it can never pull in presentation or specific-capability behavior.

;;; Code:

(require 'cl-lib)

(define-error 'e-structured-blocks-duplicate-kind
  "Duplicate structured-block kind")

(defconst e-structured-blocks-displays '(hidden inline collapsed)
  "Valid `:display' dispositions for a structured-block spec.
Only `hidden' has real effect this slice: matched text is removed from
display output.  `inline' and `collapsed' are declared but inert -- matched
text passes through unchanged; folding UI for `collapsed' is a later slice.")

(cl-defstruct (e-structured-block
               (:constructor e-structured-block--create
                             (&key kind matcher display parser)))
  "A capability-registered structured-block spec.
KIND is a symbol naming the block kind.  MATCHER is a function of one
argument, the assistant content string, returning a list of match plists
`(:start START :end END)' using 0-based string offsets into that content,
sorted ascending and non-overlapping.  DISPLAY is one of
`e-structured-blocks-displays'.  PARSER, when non-nil, is a function of one
argument, the exact matched substring (including delimiters), returning the
block's parsed fields."
  kind
  matcher
  display
  parser)

(defun e-structured-block-create (&rest args)
  "Create an `e-structured-block' spec from keyword ARGS, validating its shape."
  (let ((spec (apply #'e-structured-block--create args)))
    (unless (and (symbolp (e-structured-block-kind spec))
                (e-structured-block-kind spec))
      (signal 'wrong-type-argument (list 'symbolp (e-structured-block-kind spec))))
    (unless (functionp (e-structured-block-matcher spec))
      (signal 'wrong-type-argument
              (list 'functionp (e-structured-block-matcher spec))))
    (unless (memq (e-structured-block-display spec) e-structured-blocks-displays)
      (signal 'wrong-type-argument
              (list 'e-structured-blocks-displays
                    (e-structured-block-display spec))))
    (when (and (e-structured-block-parser spec)
              (not (functionp (e-structured-block-parser spec))))
      (signal 'wrong-type-argument (list 'functionp (e-structured-block-parser spec))))
    spec))

(cl-defstruct (e-structured-blocks-registry
               (:constructor e-structured-blocks-registry-create))
  (blocks (make-hash-table :test 'eq)))

(defun e-structured-blocks-register (registry spec)
  "Register structured-block SPEC in REGISTRY and return SPEC.
Signals `e-structured-blocks-duplicate-kind' when SPEC's kind is already
registered in REGISTRY."
  (unless (e-structured-block-p spec)
    (signal 'wrong-type-argument (list 'e-structured-block-p spec)))
  (let ((kind (e-structured-block-kind spec)))
    (when (e-structured-blocks-for-kind registry kind)
      (signal 'e-structured-blocks-duplicate-kind
              (list (format "Duplicate structured-block kind %S" kind))))
    (puthash kind spec (e-structured-blocks-registry-blocks registry))
    spec))

(defun e-structured-blocks-unregister (registry kind)
  "Remove the structured-block spec for KIND from REGISTRY, if present."
  (remhash kind (e-structured-blocks-registry-blocks registry))
  kind)

(defun e-structured-blocks-registry-reset (registry)
  "Remove every registered structured-block spec from REGISTRY.
Intended for reload paths that rebuild the active registry from scratch."
  (clrhash (e-structured-blocks-registry-blocks registry))
  registry)

(defun e-structured-blocks-for-kind (registry kind)
  "Return the structured-block spec registered for KIND in REGISTRY, or nil."
  (gethash kind (e-structured-blocks-registry-blocks registry)))

(defun e-structured-blocks-list (registry)
  "Return every structured-block spec registered in REGISTRY."
  (let (specs)
    (maphash (lambda (_kind spec) (push spec specs))
             (e-structured-blocks-registry-blocks registry))
    (nreverse specs)))

(defun e-structured-blocks--matches (spec content)
  "Return SPEC's matcher matches against CONTENT, validated for shape."
  (mapcar (lambda (match)
            (unless (and (integerp (plist-get match :start))
                        (integerp (plist-get match :end)))
              (signal 'wrong-type-argument (list 'e-structured-block-match match)))
            match)
          (funcall (e-structured-block-matcher spec) content)))

(defun e-structured-blocks--remove-ranges (content ranges)
  "Return CONTENT with each (START . END) pair in RANGES removed.
RANGES must be sorted ascending and non-overlapping."
  (let ((cursor 0)
        (pieces nil))
    (dolist (range ranges)
      (push (substring content cursor (car range)) pieces)
      (setq cursor (cdr range)))
    (push (substring content cursor) pieces)
    (apply #'concat (nreverse pieces))))

(defun e-structured-blocks-render (content registry)
  "Return a plist `:text' and `:blocks' for CONTENT against REGISTRY.
`:text' is CONTENT with every `hidden' block removed; `inline' and
`collapsed' blocks pass through unchanged in `:text' this slice.  `:blocks'
is every matched block, each a plist `(:kind :start :end :text :parsed)',
in ascending content order.  With no registered kinds, `:text' is CONTENT
unchanged, byte-for-byte."
  (let ((specs (and registry (e-structured-blocks-list registry))))
    (if (null specs)
        (list :text content :blocks nil)
      (let (blocks hidden-ranges)
        (dolist (spec specs)
          (dolist (match (e-structured-blocks--matches spec content))
            (let* ((start (plist-get match :start))
                   (end (plist-get match :end))
                   (text (substring content start end))
                   (parser (e-structured-block-parser spec)))
              (push (list :kind (e-structured-block-kind spec)
                          :start start
                          :end end
                          :text text
                          :parsed (and parser (funcall parser text)))
                    blocks)
              (when (eq (e-structured-block-display spec) 'hidden)
                (push (cons start end) hidden-ranges)))))
        (setq blocks (sort blocks (lambda (a b) (< (plist-get a :start)
                                                   (plist-get b :start)))))
        (setq hidden-ranges (sort hidden-ranges (lambda (a b) (< (car a) (car b)))))
        (list :text (e-structured-blocks--remove-ranges content hidden-ranges)
              :blocks blocks)))))

(provide 'e-structured-blocks)

;;; e-structured-blocks.el ends here
