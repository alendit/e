;;; e-keymap-hints.el --- Reusable key hint footers for e buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A small reusable helper for rendering a compact "[key] label" hint footer at
;; the bottom of an e presentation buffer, the same affordance the chat starter
;; and picker surfaces show by hand.  Callers pass an explicit list of bindings
;; so the footer text stays intentional and ordered; a keymap can also be
;; distilled into that list when a mode wants its footer to track its map.
;;
;; This lives in e core presentation so any shell -- task queue, picker,
;; starter, future list buffers -- can render one consistent footer instead of
;; hand-formatting key legends.

;;; Code:

(require 'cl-lib)

(defface e-keymap-hints-face
  '((t :inherit shadow))
  "Face used for the key hint footer text."
  :group 'e)

(defun e-keymap-hints--normalize (binding)
  "Return BINDING as a (KEY . LABEL) cons.
BINDING may be a cons (KEY . LABEL) or a two-element list (KEY LABEL)."
  (pcase binding
    (`(,key ,label) (cons key label))
    (`(,key . ,label) (cons key label))
    (_ (signal 'wrong-type-argument (list 'e-keymap-hints binding)))))

(cl-defun e-keymap-hints-string (bindings &key (separator "  "))
  "Return a compact key hint string for BINDINGS.
BINDINGS is an ordered list of (KEY . LABEL) or (KEY LABEL) entries; KEY and
LABEL are strings.  Each entry renders as \"[KEY] LABEL\" and entries are joined
with SEPARATOR.  Nil entries are skipped so callers can conditionally omit
bindings inline."
  (mapconcat
   (lambda (binding)
     (let ((pair (e-keymap-hints--normalize binding)))
       (format "[%s] %s" (car pair) (cdr pair))))
   (delq nil bindings)
   separator))

(cl-defun e-keymap-hints-insert (bindings &key (face 'e-keymap-hints-face)
                                          (separator "  "))
  "Insert a key hint footer for BINDINGS at point, propertized with FACE.
Inserts the joined hint string followed by a newline.  SEPARATOR is passed to
`e-keymap-hints-string'.  Returns the inserted string."
  (let ((text (e-keymap-hints-string bindings :separator separator))
        (start (point)))
    (insert text "\n")
    (when face
      (add-text-properties start (1- (point)) (list 'font-lock-face face)))
    text))

(defun e-keymap-hints-from-keymap (keymap labels)
  "Return an ordered hint list distilled from KEYMAP using LABELS.
LABELS is an alist of (COMMAND . LABEL).  For each command in LABELS order, the
first key sequence bound to it in KEYMAP (searched in isolation from global and
minor-mode maps) is looked up and rendered as its key;
a command with no binding is skipped.  This lets a mode's footer track its own
keymap while keeping human-ordered, human-worded labels under caller control."
  (delq nil
        (mapcar
         (lambda (entry)
           (let* ((command (car entry))
                  (label (cdr entry))
                  (keys (where-is-internal command (list keymap) t)))
             (when keys
               (cons (key-description keys) label))))
         labels)))

(provide 'e-keymap-hints)

;;; e-keymap-hints.el ends here
