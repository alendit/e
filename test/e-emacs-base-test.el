;;; e-emacs-base-test.el --- Tests for the emacs-base layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the MVP Emacs base layer.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-emacs-base)
(require 'e-harness)

(ert-deftest e-emacs-base-test-layer-registers-default-tools ()
  "The emacs-base layer registers the MVP tool surface."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (e-emacs-base-layer-create)))
    (e-harness-activate-layer harness layer)
    (should (equal (mapcar (lambda (definition)
                             (plist-get definition :name))
                           (e-tools-definitions (e-harness-tools harness)))
                   '("list_buffers"
                     "read_buffer"
                     "write_buffer"
                     "edit_buffer"
                     "save_buffer"
                     "run_elisp")))))

(ert-deftest e-emacs-base-test-visible-buffer-context-uses-visible-windows ()
  "Visible-buffer context includes visible buffers and excludes hidden ones."
  (let ((visible (generate-new-buffer " *e-visible-context*"))
        (hidden (generate-new-buffer " *e-hidden-context*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer visible)
          (with-current-buffer hidden
            (text-mode))
          (let* ((provider (e-emacs-base-visible-buffers-context-provider))
                 (messages (e-context-provider-build
                            provider
                            :harness nil
                            :session-id "session-1"
                            :turn-id "turn-1"))
                 (content (plist-get (car messages) :content)))
            (should (string-match-p (regexp-quote (buffer-name visible))
                                    content))
            (should-not (string-match-p (regexp-quote (buffer-name hidden))
                                        content))))
      (kill-buffer visible)
      (kill-buffer hidden))))

(provide 'e-emacs-base-test)

;;; e-emacs-base-test.el ends here
