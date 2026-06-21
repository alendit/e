;;; e-workspaces-test.el --- Tests for workspace awareness support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for presentation-owned workspace backend detection and dispatch.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-workspaces)

(ert-deftest e-workspaces-test-single-backend-current-token ()
  "The fallback backend exposes one live workspace."
  (let ((e-workspace-awareness-backend-priority '(single)))
    (let ((token (e-workspace-current)))
      (should (e-workspace-token-p token))
      (should (eq (e-workspace-token-backend token) 'single))
      (should (equal (e-workspace-token-id token) 'single))
      (should (equal (e-workspace-token-name token) "single"))
      (should (eq (e-workspace-token-frame token) (selected-frame)))
      (should (e-workspace-live-p token))
      (should (e-workspace-buffer-member-p (current-buffer) token))
      (should (eq (e-workspace-add-buffer (current-buffer) token)
                  (current-buffer))))))

(ert-deftest e-workspaces-test-token-equality-uses-backend-id-and-frame ()
  "Workspace tokens compare by backend, id, and frame."
  (let* ((frame (selected-frame))
         (same-a (make-e-workspace-token
                  :backend 'doom :id "dev" :name "Dev" :frame frame))
         (same-b (make-e-workspace-token
                  :backend 'doom :id "dev" :name "Development" :frame frame))
         (other-name (make-e-workspace-token
                      :backend 'doom :id "docs" :name "Dev" :frame frame))
         (other-backend (make-e-workspace-token
                         :backend 'tab-bar :id "dev" :name "Dev" :frame frame)))
    (should (e-workspace-equal-p same-a same-b))
    (should-not (e-workspace-equal-p same-a other-name))
    (should-not (e-workspace-equal-p same-a other-backend))
    (should-not (e-workspace-equal-p same-a nil))))

(ert-deftest e-workspaces-test-doom-backend-dispatches-through-workspace-api ()
  "Doom workspace functions are used when available."
  (let ((e-workspace-awareness-backend-priority '(doom single))
        (switches nil)
        (added nil)
        (buffer (current-buffer)))
    (cl-letf (((symbol-function '+workspace-current-name)
               (lambda () "research"))
              ((symbol-function '+workspace-exists-p)
               (lambda (name) (equal name "research")))
              ((symbol-function '+workspace-switch)
               (lambda (name) (push name switches)))
              ((symbol-function '+workspace-buffer-list)
               (lambda () (list buffer)))
              ((symbol-function 'persp-add-buffer)
               (lambda (candidate) (push candidate added))))
      (let ((token (e-workspace-current)))
        (should (eq (e-workspace-token-backend token) 'doom))
        (should (equal (e-workspace-token-id token) "research"))
        (should (e-workspace-live-p token))
        (should (e-workspace-buffer-member-p buffer token))
        (should (eq (e-workspace-add-buffer buffer token) buffer))
        (should (equal added (list buffer)))
        (should (e-workspace-switch token))
        (should (equal switches '("research")))))))

(ert-deftest e-workspaces-test-tab-bar-backend-dispatches-through-tab-api ()
  "Tab-bar workspaces are detected without Doom or persp functions."
  (let ((e-workspace-awareness-backend-priority '(tab-bar single))
        (switches nil))
    (cl-letf (((symbol-function 'tab-bar--current-tab)
               (lambda () '((name . "docs") (explicit-name . t))))
              ((symbol-function 'tab-bar-switch-to-tab)
               (lambda (name) (push name switches))))
      (let ((token (e-workspace-current)))
        (should (eq (e-workspace-token-backend token) 'tab-bar))
        (should (equal (e-workspace-token-id token) "docs"))
        (should (e-workspace-live-p token))
        (should (e-workspace-buffer-member-p (current-buffer) token))
        (should (e-workspace-switch token))
        (should (equal switches '("docs")))))))

(ert-deftest e-workspaces-test-visible-window-respects-token-frame ()
  "Visible window lookup is scoped to the token frame."
  (let ((e-workspace-awareness-backend-priority '(single))
        (buffer (window-buffer (selected-window))))
    (should (eq (window-buffer (selected-window)) buffer))
    (should (eq (e-workspace-visible-window buffer (e-workspace-current))
                (selected-window)))))

(ert-deftest e-workspaces-test-display-buffer-switches-and-adds-target-workspace ()
  "Workspace display switches to and admits buffers into the target workspace."
  (let ((buffer (get-buffer-create "e-workspace-display-target"))
        (token (make-e-workspace-token
                :backend 'doom
                :id "research"
                :name "research"
                :frame (selected-frame)))
        switches
        additions
        events
        display-action)
    (unwind-protect
        (cl-letf (((symbol-function 'e-workspace-visible-window)
                   (lambda (_buffer _token) nil))
                  ((symbol-function 'e-workspace-buffer-member-p)
                   (lambda (candidate workspace)
                     (setq events
                           (append events
                                   (list (list 'member candidate workspace))))
                     nil))
                  ((symbol-function 'e-workspace-add-buffer)
                   (lambda (candidate workspace)
                     (setq events
                           (append events
                                   (list (list 'add candidate workspace))))
                     (push (list candidate workspace) additions)
                     candidate))
                  ((symbol-function 'e-workspace-switch)
                   (lambda (workspace)
                     (setq events
                           (append events
                                   (list (list 'switch workspace))))
                     (push workspace switches)
                     t))
                  ((symbol-function 'display-buffer)
                   (lambda (candidate action)
                     (setq events
                           (append events
                                   (list (list 'display candidate))))
                     (setq display-action action)
                     (display-buffer-same-window candidate nil))))
          (should (window-live-p
                   (e-workspace-display-buffer buffer :workspace token)))
          (should (equal switches (list token)))
          (should (equal additions (list (list buffer token))))
          (should (equal events
                         (list (list 'switch token)
                               (list 'member buffer token)
                               (list 'add buffer token)
                               (list 'display buffer))))
          (should (memq 'display-buffer-reuse-window (car display-action)))
          (should (equal (alist-get 'reusable-frames (cdr display-action)) nil))
          (should (equal (alist-get 'lru-frames (cdr display-action)) nil))
          (should (equal (alist-get 'inhibit-switch-frame (cdr display-action))
                         t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-workspaces-test-visible-window-does-not-cross-workspaces ()
  "Visible window lookup does not reuse a current-workspace window for another token."
  (let* ((frame (selected-frame))
         (buffer (window-buffer (selected-window)))
         (current (make-e-workspace-token
                   :backend 'doom
                   :id "current"
                   :name "current"
                   :frame frame))
         (target (make-e-workspace-token
                  :backend 'doom
                  :id "target"
                  :name "target"
                  :frame frame)))
    (cl-letf (((symbol-function 'e-workspace-current)
               (lambda (&optional _frame) current)))
      (should (eq (e-workspace-visible-window buffer current)
                  (selected-window)))
      (should-not (e-workspace-visible-window buffer target)))))

(ert-deftest e-workspaces-test-switch-to-buffer-selects-visible-workspace-window ()
  "Workspace switch helper reuses an already visible workspace window."
  (let ((buffer (get-buffer-create "e-workspace-visible-target"))
        (token (e-workspace-current))
        selected)
    (unwind-protect
        (cl-letf (((symbol-function 'e-workspace-visible-window)
                   (lambda (_buffer _token) (selected-window)))
                  ((symbol-function 'select-window)
                   (lambda (window &optional _norecord)
                     (setq selected window)
                     window)))
          (should (eq (e-workspace-switch-to-buffer buffer :workspace token)
                      buffer))
          (should (eq selected (selected-window))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-workspaces-test)

;;; e-workspaces-test.el ends here
