;;; e-debug-test.el --- Tests for standing debug agent session -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the debug agent shell's standing session resolver.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-chat)
(require 'e-debug)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-session)
(require 'e-shells)

(defmacro e-debug-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest e-debug-test-ensure-session-reuses-standing-session ()
  "The debug resolver reuses the same standing session."
  (e-debug-test--with-empty-harness-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions (e-session-store-create)))
          (e-debug--session-id nil))
      (cl-letf (((symbol-function 'e-debug--default-harness)
                 (lambda () harness)))
        (let ((first (e-debug--ensure-session))
              (second (e-debug--ensure-session)))
          (should (equal second first))
          (should (= (length (e-harness-session-list harness)) 1))
          (should (equal (plist-get
                          (plist-get (e-session-get
                                      (e-harness-sessions harness)
                                      first)
                                     :metadata)
                          :source)
                         'e-debug)))))))

(ert-deftest e-debug-test-ensure-session-rediscovers-existing-session ()
  "The debug resolver finds an existing debug session when its cache is empty."
  (e-debug-test--with-empty-harness-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions (e-session-store-create)))
          (e-debug--session-id nil))
      (cl-letf (((symbol-function 'e-debug--default-harness)
                 (lambda () harness)))
        (let ((created (e-debug--ensure-session)))
          (setq e-debug--session-id nil)
          (should (equal (e-debug--ensure-session) created))
          (should (= (length (e-harness-session-list harness)) 1)))))))

(ert-deftest e-debug-test-command-opens-and-shows-standing-session ()
  "The `e-debug' command opens the standing debug session through chat UI."
  (e-debug-test--with-empty-harness-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions (e-session-store-create)))
          shown-buffer
          (e-debug--session-id nil))
      (cl-letf (((symbol-function 'e-debug--default-harness)
                 (lambda () harness))
                ((symbol-function 'e-debug--show-buffer)
                 (lambda (buffer)
                   (setq shown-buffer buffer))))
        (let ((buffer (e-debug)))
          (should (eq shown-buffer buffer))
          (with-current-buffer buffer
            (should (derived-mode-p 'e-chat-mode))
            (should (eq e-chat-harness harness))
            (should (equal e-chat-session-id e-debug--session-id))))))))

(ert-deftest e-debug-test-tab-display-strategy-opens-tab-before-buffer ()
  "The tab display strategy opens a tab before showing the chat buffer."
  (let ((buffer (generate-new-buffer " *e-debug-test*"))
        (e-debug-display-strategy 'tab)
        events)
    (unwind-protect
        (cl-letf (((symbol-function 'tab-bar-new-tab)
                   (lambda (&rest _args)
                     (push 'tab events)))
                  ((symbol-function 'e-chat--pop-to-buffer)
                   (lambda (shown)
                     (push (list 'show shown) events))))
          (e-debug--show-buffer buffer)
          (should (equal (nreverse events)
                         (list 'tab (list 'show buffer)))))
      (kill-buffer buffer))))

(ert-deftest e-debug-test-shell-manifest-exposes-debug-command ()
  "The debug shell exposes the standing debug command."
  (let* ((shell (e-debug-shell))
         (command (e-shell-command-by-id shell 'open)))
    (should (eq (e-shell-id shell) 'debug))
    (should (equal (e-shell-required-capabilities shell)
                   '(chat-session debug-agent)))
    (should command)
    (should (eq (e-shell-command-interactive command) 'e-debug))
    (should (commandp (e-shell-command-interactive command)))))

(provide 'e-debug-test)

;;; e-debug-test.el ends here
