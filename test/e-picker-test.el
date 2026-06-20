;;; e-picker-test.el --- Tests for e picker primitive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the reusable shell picker primitive.

;;; Code:

(require 'ert)
(require 'e-picker)

(ert-deftest e-picker-test-filter-substring-matches-candidate-keys ()
  "The default picker filter matches candidate keys case-insensitively."
  (let ((candidates '((:id alpha :title "Alpha Session")
                      (:id beta :title "Beta Session"))))
    (should (equal (mapcar (lambda (candidate)
                             (plist-get candidate :id))
                           (e-picker-filter-substring
                            "alp"
                            candidates
                            (lambda (candidate)
                              (plist-get candidate :title))))
                   '(alpha)))
    (should (equal (mapcar (lambda (candidate)
                             (plist-get candidate :id))
                           (e-picker-filter-substring
                            ""
                            candidates
                            (lambda (candidate)
                              (plist-get candidate :title))))
                   '(alpha beta)))))

(ert-deftest e-picker-test-make-line-aligns-right-metadata ()
  "Candidate line helper keeps left text and right metadata in one row."
  (let ((line (e-picker-make-line "Session title" "ctx 10%" 32)))
    (should (string-prefix-p "Session title" line))
    (should (string-suffix-p "ctx 10%" line))
    (should (= (string-width line) 32))))

(ert-deftest e-picker-test-open-renders-posframe-and-selects-current-candidate ()
  "Opening with posframe renders a buffer and RET selects the highlighted row."
  (let (shown hidden selected)
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (buffer &rest _args)
                 (setq shown buffer)))
              ((symbol-function 'posframe-hide)
               (lambda (buffer)
                 (setq hidden buffer))))
      (let ((buffer (e-picker-open
                     :name 'test-open
                     :title "Pick one"
                     :candidates '("alpha" "beta")
                     :candidate-key #'identity
                     :candidate-line #'identity
                     :on-select (lambda (candidate)
                                  (setq selected candidate)))))
        (should (eq shown buffer))
        (with-current-buffer buffer
          (should (derived-mode-p 'e-picker-mode))
          (should (string-match-p "Pick one" (buffer-string)))
          (should (string-match-p "alpha" (buffer-string)))
          (e-picker-next)
          (e-picker-select))
        (should (eq hidden buffer))
        (should (equal selected "beta"))))))

(ert-deftest e-picker-test-action-can-keep-picker-open ()
  "Action keys can run against the selected candidate without closing."
  (let (hidden acted)
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (_buffer &rest _args) t))
              ((symbol-function 'posframe-hide)
               (lambda (buffer)
                 (setq hidden buffer))))
      (let ((buffer (e-picker-open
                     :name 'test-action
                     :title "Act"
                     :candidates '("alpha")
                     :candidate-key #'identity
                     :candidate-line #'identity
                     :actions (list (cons ?o (lambda (candidate)
                                               (setq acted candidate)
                                               t)))
                     :on-select #'ignore)))
        (with-current-buffer buffer
          (e-picker-dispatch-action ?o))
        (should (equal acted "alpha"))
        (should-not hidden)))))

(ert-deftest e-picker-test-terminal-fallback-selects-with-completing-read ()
  "When posframe is unavailable, the picker falls back to completing-read."
  (let (selected prompt labels)
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (read-prompt collection &rest _args)
                 (setq prompt read-prompt)
                 (setq labels (all-completions "" collection))
                 "beta")))
      (should-not
       (e-picker-open
        :name 'test-fallback
        :title "Fallback"
        :candidates '("alpha" "beta")
        :candidate-key #'identity
        :candidate-line #'identity
        :on-select (lambda (candidate)
                     (setq selected candidate))))
      (should (equal prompt "Fallback: "))
      (should (equal labels '("alpha" "beta")))
      (should (equal selected "beta")))))

(provide 'e-picker-test)

;;; e-picker-test.el ends here
