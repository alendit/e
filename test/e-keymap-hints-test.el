;;; e-keymap-hints-test.el --- Tests for reusable key hint footers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for `e-keymap-hints': the reusable "[key] label" footer helper.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-keymap-hints)

(ert-deftest e-keymap-hints-test-string-formats-and-joins ()
  "Bindings render as [key] label joined by the separator."
  (should (equal (e-keymap-hints-string '(("RET" . "open") ("q" . "quit")))
                 "[RET] open  [q] quit"))
  (should (equal (e-keymap-hints-string '(("RET" . "open") ("q" . "quit"))
                                        :separator " | ")
                 "[RET] open | [q] quit")))

(ert-deftest e-keymap-hints-test-accepts-list-pairs ()
  "Two-element list entries are accepted like conses."
  (should (equal (e-keymap-hints-string '(("g" "refresh")))
                 "[g] refresh")))

(ert-deftest e-keymap-hints-test-skips-nil-entries ()
  "Nil entries let callers omit bindings inline."
  (should (equal (e-keymap-hints-string (list '("a" . "one") nil '("b" . "two")))
                 "[a] one  [b] two")))

(ert-deftest e-keymap-hints-test-insert-adds-footer-line ()
  "Insert writes the footer plus a trailing newline and faces it."
  (with-temp-buffer
    (e-keymap-hints-insert '(("RET" . "open")))
    (let ((text (buffer-string)))
      (should (equal text "[RET] open\n"))
      (should (eq (get-text-property (point-min) 'font-lock-face)
                  'e-keymap-hints-face)))))

(ert-deftest e-keymap-hints-test-from-keymap-tracks-bindings ()
  "Distilling a keymap picks up the current key for each labelled command."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'ignore)
    (define-key map (kbd "RET") #'undo)
    (should (equal (e-keymap-hints-from-keymap
                    map '((ignore . "refresh") (undo . "undo")))
                   '(("g" . "refresh") ("RET" . "undo"))))))

(ert-deftest e-keymap-hints-test-from-keymap-skips-unbound ()
  "A labelled command with no binding is dropped from the hint list."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'ignore)
    (should (equal (e-keymap-hints-from-keymap
                    map '((ignore . "refresh") (undo . "undo")))
                   '(("g" . "refresh"))))))

(provide 'e-keymap-hints-test)

;;; e-keymap-hints-test.el ends here
