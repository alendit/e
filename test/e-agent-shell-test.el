;;; e-agent-shell-test.el --- Tests for Agent Shell adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the narrow Agent Shell adapter boundary.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-agent-shell)

(defvar agent-shell-agent-configs)
(defvar agent-shell--state)

(ert-deftest e-agent-shell-test-missing-package-diagnostic ()
  "Adapter reports a clear error when Agent Shell is unavailable."
  (cl-letf (((symbol-function 'e-agent-shell-available-p)
             (lambda () nil)))
    (should-error
     (e-agent-shell-start-worker :project-root default-directory)
     :type 'e-agent-shell-unavailable)))

(ert-deftest e-agent-shell-test-start-status-send-subscribe-and-interrupt ()
  "Adapter isolates Agent Shell calls behind E-owned functions."
  (let ((buffer (get-buffer-create " *agent-shell-stub*"))
        (agent-shell-agent-configs
         '(((:identifier . "codex")
            (:name . "Codex"))))
        start-args
        inserted
        subscribed
        interrupted)
    (unwind-protect
        (cl-letf (((symbol-function 'e-agent-shell--ensure-available)
                   (lambda () t))
                  ((symbol-function 'agent-shell--start)
                   (lambda (&rest args)
                     (let ((rest args))
                       (while rest
                         (unless (memq (car rest)
                                       '(:config :no-focus :new-session
                                         :session-strategy))
                           (error "unexpected Agent Shell start keyword %S"
                                  (car rest)))
                         (setq rest (cddr rest))))
                     (setq start-args args)
                     buffer))
                  ((symbol-function 'agent-shell-status)
                   (lambda () 'busy))
                  ((symbol-function 'agent-shell-insert)
                   (lambda (&rest args)
                     (unless (equal (cl-loop for (key _value) on args by #'cddr
                                             collect key)
                                    '(:text :shell-buffer :submit))
                       (error "unexpected Agent Shell insert arguments %S"
                              args))
                     (setq inserted args)))
                  ((symbol-function 'agent-shell-subscribe-to)
                   (lambda (&rest args)
                     (unless (equal (cl-loop for (key _value) on args by #'cddr
                                             collect key)
                                    '(:shell-buffer :event :on-event))
                       (error "unexpected Agent Shell subscribe arguments %S"
                              args))
                     (push args subscribed)
                     (list :subscription (plist-get args :event))))
                  ((symbol-function 'agent-shell-interrupt)
                   (lambda (&optional force)
                     (setq interrupted (list (current-buffer) force))
                     t)))
          (should (eq (e-agent-shell-start-worker
                       :project-root "/tmp/project"
                       :agent-id "codex"
                       :background t)
                      buffer))
          (should (equal start-args
                         '(:config ((:identifier . "codex")
                                    (:name . "Codex"))
                           :no-focus t
                           :new-session t)))
          (should (eq (e-agent-shell-status buffer) 'busy))
          (e-agent-shell-send-prompt buffer "do work")
          (should (equal inserted
                         (list :text "do work" :shell-buffer buffer
                               :submit t)))
          (let ((subscriptions (e-agent-shell-subscribe buffer #'ignore)))
            (should (= (length subscriptions) 8))
            (should (equal (mapcar (lambda (args) (plist-get args :event))
                                   (reverse subscribed))
                           '(input-submitted permission-request
                             permission-response tool-call-update file-write
                             turn-complete error clean-up)))
            (should (equal subscriptions
                           '((:subscription input-submitted)
                             (:subscription permission-request)
                             (:subscription permission-response)
                             (:subscription tool-call-update)
                             (:subscription file-write)
                             (:subscription turn-complete)
                             (:subscription error)
                             (:subscription clean-up))))
            (dolist (args subscribed)
              (should (eq (plist-get args :shell-buffer) buffer))
              (should (functionp (plist-get args :on-event)))))
          (should (e-agent-shell-interrupt buffer :force t))
          (should (equal interrupted (list buffer t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-agent-shell-test-dead-buffer-status-short-circuits ()
  "Dead buffers report dead status without entering the buffer."
  (let ((buffer (get-buffer-create " *agent-shell-dead*")))
    (kill-buffer buffer)
    (cl-letf (((symbol-function 'e-agent-shell--ensure-available)
               (lambda () t)))
      (should (eq (e-agent-shell-status buffer) 'dead)))))

(ert-deftest e-agent-shell-test-adopt-buffer-validates-metadata ()
  "Adoption only accepts Agent Shell buffers with real state."
  (let ((buffer (get-buffer-create " *agent-shell-adopt*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'agent-shell-mode)
            (setq-local agent-shell-project-root "/tmp/project")
            (setq-local agent-shell--state
                        (list :buffer buffer
                              :agent-config '((:identifier . "codex")
                                              (:name . "Codex"))
                              :session (list :id "session-1")))
            (setq-local agent-shell--transcript-file "/tmp/transcript.jsonl"))
          (cl-letf (((symbol-function 'e-agent-shell--ensure-available)
                     (lambda () t)))
            (should (equal (e-agent-shell-adopt-buffer buffer)
                           (list :shell-buffer buffer
                                 :project-root "/tmp/project"
                                 :agent-id "codex"
                                 :agent-session-id "session-1"
                                 :transcript-file "/tmp/transcript.jsonl"))))
          (with-current-buffer buffer
            (kill-local-variable 'major-mode)
            (kill-local-variable 'agent-shell--state)
            (setq-local agent-shell-config "codex")
            (setq-local agent-shell--session-id "session-1"))
          (cl-letf (((symbol-function 'e-agent-shell--ensure-available)
                     (lambda () t)))
            (should-error
             (e-agent-shell-adopt-buffer buffer)
             :type 'e-agent-shell-non-adoptable)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-agent-shell-test-adopt-buffer-rejects-spoofed-state ()
  "Adoption rejects non-Agent-Shell buffers with spoofed state."
  (let ((buffer (get-buffer-create " *agent-shell-spoofed-state*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local agent-shell-project-root "/tmp/project")
            (setq-local agent-shell--state
                        (list :buffer buffer
                              :agent-config '((:identifier . "codex"))
                              :session (list :id "session-1"))))
          (cl-letf (((symbol-function 'e-agent-shell--ensure-available)
                     (lambda () t)))
            (should-error
             (e-agent-shell-adopt-buffer buffer)
             :type 'e-agent-shell-non-adoptable)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-agent-shell-test)

;;; e-agent-shell-test.el ends here
