;;; e-picker-test.el --- Tests for e picker primitive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the reusable shell picker primitive.

;;; Code:

(require 'ert)
(require 'e-picker)

(defvar evil-local-mode nil)
(defvar evil-state nil)

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

(ert-deftest e-picker-test-mode-map-binds-j-k-for-navigation ()
  "The picker keymap uses j/k, not n/p, for candidate navigation."
  (should (eq (lookup-key e-picker-mode-map (kbd "j")) #'e-picker-next))
  (should (eq (lookup-key e-picker-mode-map (kbd "k")) #'e-picker-previous))
  (should-not (lookup-key e-picker-mode-map (kbd "n")))
  (should-not (lookup-key e-picker-mode-map (kbd "p"))))

(ert-deftest e-picker-test-mode-map-binds-return-to-select ()
  "Both terminal RET and GUI return select the current candidate."
  (should (eq (lookup-key e-picker-mode-map (kbd "RET")) #'e-picker-select))
  (should (eq (lookup-key e-picker-mode-map (kbd "<return>"))
              #'e-picker-select)))

(ert-deftest e-picker-test-mode-neutralizes-evil ()
  "Picker buffers keep Evil from intercepting j/k/RET navigation keys."
  (let ((buffer (get-buffer-create "*e-picker-evil-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'evil-local-mode)
                   (lambda (argument)
                     (setq-local evil-local-mode
                                 (not (and (numberp argument)
                                           (< argument 0))))
                     (unless evil-local-mode
                       (setq-local evil-state nil)))))
          (with-current-buffer buffer
            (setq-local evil-local-mode t)
            (setq-local evil-state 'normal)
            (e-picker-mode)
            (should-not evil-local-mode)
            (should-not evil-state)
            (should (eq (lookup-key e-picker-mode-map (kbd "RET"))
                        #'e-picker-select))
            (should (eq (lookup-key e-picker-mode-map (kbd "j"))
                        #'e-picker-next))
            (should (eq (lookup-key e-picker-mode-map (kbd "k"))
                        #'e-picker-previous))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-picker-test-evil-local-mode-hook-does-not-recurse ()
  "Disabling Evil in picker mode does not recurse through Evil's hook."
  (let ((buffer (get-buffer-create "*e-picker-evil-recursion-test*"))
        (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'evil-local-mode)
                   (lambda (argument)
                     (setq calls (1+ calls))
                     (when (> calls 3)
                       (error "recursive evil-local-mode disable"))
                     (if (and (numberp argument) (< argument 0))
                         (progn
                           (setq-local evil-local-mode t)
                           (run-hooks 'evil-local-mode-hook)
                           (setq-local evil-local-mode nil)
                           (setq-local evil-state nil))
                       (setq-local evil-local-mode t)))))
          (with-current-buffer buffer
            (setq-local evil-local-mode t)
            (setq-local evil-state 'normal)
            (e-picker-mode)
            (should (= calls 1))
            (should-not evil-local-mode)
            (should-not evil-state)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-picker-test-navigation-keeps-point-on-selected-row ()
  "Moving with j/k moves point to the selected picker candidate row."
  (let (selected)
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (_buffer &rest _args) 'picker-frame))
              ((symbol-function 'e-picker--focus-frame)
               (lambda (_frame) nil)))
      (let ((buffer (e-picker-open
                     :name 'test-selected-point
                     :title "Pick"
                     :candidates '("alpha" "beta" "gamma")
                     :candidate-key #'identity
                     :candidate-line #'identity
                     :on-select (lambda (candidate)
                                  (setq selected candidate)))))
        (unwind-protect
            (with-current-buffer buffer
              (should (looking-at-p "> alpha"))
              (e-picker-next)
              (should (looking-at-p "> beta"))
              (e-picker-previous)
              (should (looking-at-p "> alpha"))
              (e-picker-next)
              (e-picker-select)
              (should (equal selected "beta")))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest e-picker-test-navigation-does-not-recompute-candidate-lines ()
  "Moving with j/k updates selection without rebuilding every candidate row."
  (let ((line-calls 0))
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (_buffer &rest _args) 'picker-frame))
              ((symbol-function 'e-picker--focus-frame)
               (lambda (_frame) nil)))
      (let ((buffer (e-picker-open
                     :name 'test-navigation-no-rerender
                     :title "Pick"
                     :candidates '("alpha" "beta" "gamma")
                     :candidate-key #'identity
                     :candidate-line (lambda (candidate)
                                       (setq line-calls (1+ line-calls))
                                       candidate)
                     :preview (lambda (candidate buffer)
                                (with-current-buffer buffer
                                  (insert "Preview for " candidate)))
                     :on-select #'ignore)))
        (unwind-protect
            (with-current-buffer buffer
              (should (= line-calls 3))
              (setq line-calls 0)
              (e-picker-next)
              (should (= line-calls 0))
              (should (looking-at-p "> beta"))
              (should (string-match-p "| Preview for beta" (buffer-string)))
              (should-not (string-match-p "| Preview for alpha"
                                           (buffer-string))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest e-picker-test-initial-candidate-limit-defers-row-rendering ()
  "A picker can render only an initial row window and expand at the bottom."
  (let ((line-calls 0)
        (candidates (number-sequence 1 40)))
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (_buffer &rest _args) 'picker-frame))
              ((symbol-function 'e-picker--focus-frame)
               (lambda (_frame) nil)))
      (let ((buffer (e-picker-open
                     :name 'test-limited-initial-render
                     :title "Pick"
                     :candidates candidates
                     :candidate-key #'number-to-string
                     :candidate-line (lambda (candidate)
                                       (setq line-calls (1+ line-calls))
                                       (format "candidate %02d" candidate))
                     :on-select #'ignore
                     :initial-candidate-limit 15
                     :candidate-limit-step 15)))
        (unwind-protect
            (with-current-buffer buffer
              (should (= line-calls 15))
              (should (string-match-p "candidate 15" (buffer-string)))
              (should-not (string-match-p "candidate 16" (buffer-string)))
              (setq line-calls 0)
              (dotimes (_ 15)
                (e-picker-next))
              (should (= e-picker--selection 15))
              (should (= line-calls 30))
              (should (string-match-p "^> candidate 16" (buffer-string))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest e-picker-test-navigation-boundary-does-not-refresh-preview ()
  "Pressing j/k at a list boundary does not rerender the preview."
  (let ((preview-calls 0))
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (_buffer &rest _args) 'picker-frame))
              ((symbol-function 'e-picker--focus-frame)
               (lambda (_frame) nil)))
      (let ((buffer (e-picker-open
                     :name 'test-navigation-boundary
                     :title "Pick"
                     :candidates '("alpha")
                     :candidate-key #'identity
                     :candidate-line #'identity
                     :preview (lambda (candidate buffer)
                                (setq preview-calls (1+ preview-calls))
                                (with-current-buffer buffer
                                  (insert candidate)))
                     :on-select #'ignore)))
        (unwind-protect
            (with-current-buffer buffer
              (should (= preview-calls 1))
              (setq preview-calls 0)
              (e-picker-next)
              (should (= preview-calls 0))
              (should (looking-at-p "> alpha")))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest e-picker-test-preview-update-keeps-row-starts-stable ()
  "Preview updates with different text lengths do not corrupt row prefixes."
  (cl-letf (((symbol-function 'e-picker--posframe-available-p)
             (lambda () t))
            ((symbol-function 'posframe-show)
             (lambda (_buffer &rest _args) 'picker-frame))
            ((symbol-function 'e-picker--focus-frame)
             (lambda (_frame) nil)))
    (let ((buffer (e-picker-open
                   :name 'test-preview-stable-row-starts
                   :title "Pick"
                   :candidates '("alpha" "beta" "gamma")
                   :candidate-key #'identity
                   :candidate-line (lambda (candidate)
                                     (format "%s Default Chat" candidate))
                   :preview (lambda (candidate buffer)
                              (with-current-buffer buffer
                                (pcase candidate
                                  ("alpha" (insert "A much longer preview"))
                                  ("beta" (insert "B"))
                                  ("gamma" (insert "Gamma preview")))))
                   :on-select #'ignore)))
      (unwind-protect
          (with-current-buffer buffer
            (e-picker-next)
            (e-picker-next)
            (goto-char (point-min))
            (should (re-search-forward "^> gamma Default Chat" nil t))
            (should-not (string-match-p "Default > Chat" (buffer-string)))
            (should-not (string-match-p "\\sw> \\sw" (buffer-string))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest e-picker-test-preview-renders-in-right-pane ()
  "Picker previews render in the right side of the candidate rows."
  (cl-letf (((symbol-function 'e-picker--posframe-available-p)
             (lambda () t))
            ((symbol-function 'posframe-show)
             (lambda (_buffer &rest _args) 'picker-frame))
            ((symbol-function 'e-picker--focus-frame)
             (lambda (_frame) nil)))
    (let ((buffer (e-picker-open
                   :name 'test-right-preview
                   :title "Pick"
                   :candidates '("alpha" "beta")
                   :candidate-key #'identity
                   :candidate-line #'identity
                   :preview (lambda (candidate buffer)
                              (with-current-buffer buffer
                                (insert "Preview for " candidate)))
                   :on-select #'ignore)))
      (unwind-protect
          (with-current-buffer buffer
            (should-not (string-match-p "^Preview$" (buffer-string)))
            (goto-char (point-min))
            (should (re-search-forward
                     "^> alpha[^\n]* | Preview for alpha *$"
                     nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest e-picker-test-selection-overlay-stays-in-left-pane ()
  "The selection overlay highlights only the candidate cell, not preview text."
  (cl-letf (((symbol-function 'e-picker--posframe-available-p)
             (lambda () t))
            ((symbol-function 'posframe-show)
             (lambda (_buffer &rest _args) 'picker-frame))
            ((symbol-function 'e-picker--focus-frame)
             (lambda (_frame) nil)))
    (let ((buffer (e-picker-open
                   :name 'test-left-overlay
                   :title "Pick"
                   :candidates '("alpha" "beta")
                   :candidate-key #'identity
                   :candidate-line #'identity
                   :preview (lambda (candidate buffer)
                              (with-current-buffer buffer
                                (insert "Preview for " candidate)))
                   :on-select #'ignore)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((highlighted (buffer-substring
                                (overlay-start e-picker--selection-overlay)
                                (overlay-end e-picker--selection-overlay))))
              (should (string-match-p "> alpha" highlighted))
              (should-not (string-match-p "Preview for alpha" highlighted)))
            (e-picker-next)
            (let ((highlighted (buffer-substring
                                (overlay-start e-picker--selection-overlay)
                                (overlay-end e-picker--selection-overlay))))
              (should (string-match-p "> beta" highlighted))
              (should-not (string-match-p "Preview for beta" highlighted))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest e-picker-test-open-renders-posframe-focuses-it-and-selects-current-candidate ()
  "Opening with posframe focuses the picker so navigation keys reach its map."
  (let (shown focused hidden selected)
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (buffer &rest _args)
                 (setq shown buffer)
                 'picker-frame))
              ((symbol-function 'e-picker--focus-frame)
               (lambda (frame)
                 (setq focused frame)))
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
        (should (eq focused 'picker-frame))
        (with-current-buffer buffer
          (should (derived-mode-p 'e-picker-mode))
          (should (eq e-picker--frame 'picker-frame))
          (should (string-match-p "Pick one" (buffer-string)))
          (should (string-match-p "alpha" (buffer-string)))
          (e-picker-next)
          (e-picker-select))
        (should (eq hidden buffer))
        (should (equal selected "beta"))))))

(ert-deftest e-picker-test-navigation-does-not-refocus-picker-frame ()
  "Navigation does not repeatedly move input focus while handling keys."
  (let (focused)
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (_buffer &rest _args)
                 'picker-frame))
              ((symbol-function 'e-picker--focus-frame)
               (lambda (frame)
                 (push frame focused))))
      (let ((buffer (e-picker-open
                     :name 'test-refocus
                     :title "Refocus"
                     :candidates '("alpha" "beta")
                     :candidate-key #'identity
                     :candidate-line #'identity
                     :on-select #'ignore)))
        (with-current-buffer buffer
          (setq focused nil)
          (e-picker-next)
          (should-not focused))))))

(ert-deftest e-picker-test-kill-unshown-buffer-does-not-delete-posframe ()
  "Killing an unshown picker buffer does not call into posframe deletion."
  (let ((buffer (get-buffer-create "*e-picker-unshown-delete-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'posframe-delete)
                   (lambda (_buffer)
                     (error "unexpected posframe deletion"))))
          (with-current-buffer buffer
            (e-picker-mode))
          (kill-buffer buffer)
          (setq buffer nil))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-picker-test-action-can-keep-picker-open ()
  "Action keys are bound in the picker map and can keep the picker open."
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
          (let ((command (lookup-key (current-local-map) (kbd "o"))))
            (should (commandp command))
            (call-interactively command)))
        (should (equal acted "alpha"))
        (should-not hidden)))))

(ert-deftest e-picker-test-kill-buffer-deletes-posframe ()
  "Killing a picker buffer tears down the cached posframe."
  (let (deleted)
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (_buffer &rest _args) t))
              ((symbol-function 'posframe-delete)
               (lambda (buffer)
                 (setq deleted buffer))))
      (let ((buffer (e-picker-open
                     :name 'test-teardown
                     :title "Teardown"
                     :candidates '("alpha")
                     :candidate-key #'identity
                     :candidate-line #'identity
                     :on-select #'ignore)))
        (kill-buffer buffer)
        (should (eq deleted buffer))))))

(ert-deftest e-picker-test-terminal-fallback-selects-with-completing-read ()
  "When posframe is unavailable, the picker falls back to completing-read."
  (let (selected prompt labels annotation)
    (cl-letf (((symbol-function 'e-picker--posframe-available-p)
               (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (read-prompt collection &rest _args)
                 (setq prompt read-prompt)
                 (setq labels (all-completions "" collection))
                 (setq annotation
                       (alist-get 'annotation-function
                                  (cdr (funcall collection "" nil
                                                'metadata))))
                 "beta")))
      (should-not
       (e-picker-open
        :name 'test-fallback
        :title "Fallback"
        :candidates '((:key "alpha" :line "Alpha row")
                      (:key "beta" :line "Beta row"))
        :candidate-key (lambda (candidate)
                         (plist-get candidate :key))
        :candidate-line (lambda (candidate)
                          (plist-get candidate :line))
        :on-select (lambda (candidate)
                     (setq selected candidate))))
      (should (equal prompt "Fallback: "))
      (should (equal labels '("alpha" "beta")))
      (should (functionp annotation))
      (should (equal (funcall annotation "beta") " Beta row"))
      (should (equal selected '(:key "beta" :line "Beta row"))))))

(provide 'e-picker-test)

;;; e-picker-test.el ends here
