;;; e-current-config-e2e-test.el --- Current configuration startup checks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Focused checks for a private Emacs started through the developer's normal
;; user configuration.  These checks do not issue provider requests and never
;; access the user's runtime database.

;;; Code:

(require 'ert)
(require 'e-e2e-bootstrap)
(require 'e-backend)
(require 'e-core)
(require 'e-default-harnesses)
(require 'e-harness-registry)
(require 'e-harness-instances)
(require 'e-layers)
(require 'e-project-local)
(require 'e-subagent-runner)

(defvar e-current-config-e2e-test--output nil
  "Dynamically bound buffer collecting the current-config ERT report.")

(defconst e-current-config-e2e-test--state-directory
  (file-name-as-directory
   (or (getenv "E_CURRENT_CONFIG_E2E_STATE_DIR")
       (error "E_CURRENT_CONFIG_E2E_STATE_DIR is required")))
  "Temporary directory for current-config E2E state.")

(defun e-current-config-e2e-test--prepare-recentf-snapshot ()
  "Redirect Recentf persistence to a test-owned snapshot."
  (let* ((source (and (boundp 'recentf-save-file) recentf-save-file))
         (snapshot
          (expand-file-name "recentf" e-current-config-e2e-test--state-directory)))
    (when (and source (file-readable-p source))
      (copy-file source snapshot t))
    (setq recentf-save-file snapshot)))

(with-eval-after-load 'recentf
  (e-current-config-e2e-test--prepare-recentf-snapshot))

(defun e-current-config-e2e-test--print (format-string &rest arguments)
  "Append FORMAT-STRING with ARGUMENTS to the current ERT report."
  (when (buffer-live-p e-current-config-e2e-test--output)
    (with-current-buffer e-current-config-e2e-test--output
      (goto-char (point-max))
      (insert (apply #'format format-string arguments)))))

(ert-deftest e-current-config-e2e-test-first-file-hooks-succeed ()
  "The first file opens with the current configuration's persisted state."
  (let ((file
         (expand-file-name
          "first-file.org" e-current-config-e2e-test--state-directory)))
    (with-temp-file file
      (insert "#+title: Current-config first file\n"))
    (let ((buffer (find-file-noselect file)))
      (unwind-protect
          (progn
            (should (buffer-live-p buffer))
            (when (boundp 'doom-first-file-hook)
              (should (featurep 'recentf))
              (should (bound-and-true-p recentf-mode))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest e-current-config-e2e-test-runtime-loaded-from-checkout ()
  "Normal user startup loads a ready e runtime from this checkout."
  (should (e-e2e-current-config-p))
  (should e-e2e-current-config-loaded-e-p)
  (should (featurep 'e))
  (should (file-equal-p (e-source-directory) e-e2e-project-root))
  (should (eq (plist-get (e-core-status) :state) 'ready)))

(ert-deftest e-current-config-e2e-test-default-harness-is-configured ()
  "The current config can construct its default harness without a request."
  (should (memq :chat-default (e-harness-registry-list)))
  (let* ((harness (e-harness-registry-get-or-create :chat-default))
         (backend (e-harness-backend harness)))
    (should (e-harness-p harness))
    (should (e-backend-p backend))
    (should-not (equal (e-backend--name backend)
                       "Unconfigured default chat backend"))
    (should (e-harness-effective-layer-ids harness))))

(ert-deftest e-current-config-e2e-test-default-projects-integrate ()
  "Every configured default project is reachable through project policy."
  (dolist (root (e-default-project-roots))
    (should (file-directory-p root))
    (should (e-project-local--root-allowed-p root))
    (let ((inspection (e-project-local--inspection root)))
      (when (plist-get inspection :has-extensions)
        (should (e-layer-p (e-project-local-prime-project root)))))))

(ert-deftest e-current-config-e2e-test-slack-layer-reaches-tool-user ()
  "The configured Slack layer can be activated on the shared tool-user type."
  (unless (e-layer-get 'slack-mcp)
    (ert-skip "Current configuration does not declare a Slack MCP layer"))
  (e-subagent-configure-type
   :tool-user
   :enable-layers '("slack-mcp")
   :layer-config '(("slack-mcp" :progressive t)))
  (let* ((instance (e-harness-instance-get :tool-user))
         (harness (e-harness-instance-get-or-create :tool-user))
         (capability-ids
          (mapcar #'e-capability-id
                  (e-harness-effective-capabilities harness))))
    (should (e-harness-instance-subagent-p instance))
    (should (memq 'slack-mcp (e-harness-enabled-layer-ids harness)))
    (should (memq 'slack-mcp capability-ids))
    (should
     (eq (plist-get (e-harness-capability-config harness 'slack-mcp)
                    :progressive)
         t))))

(defun e-current-config-e2e-test-run-to-file (path &optional selector)
  "Run current-config ERT SELECTOR, write its report to PATH, and return status."
  (let ((selector (or selector "^e-current-config-e2e-test-"))
        (e-current-config-e2e-test--output
         (generate-new-buffer " *e current-config ERT report*")))
    (unwind-protect
        (progn
          (e-current-config-e2e-test--print
           "Current-config E2E: Emacs %s, init %S, e %s\n"
           emacs-version e-e2e-current-config-user-init-file
           (e-source-directory))
          (let* ((standard-output e-current-config-e2e-test--output)
                 (stats (ert-run-tests-batch selector))
                 (unexpected (ert-stats-completed-unexpected stats))
                 (exit (if (zerop unexpected) 0 1)))
            (e-current-config-e2e-test--print
             "Current-config E2E complete: %d tests, %d unexpected.\n"
             (ert-stats-total stats) unexpected)
            (with-temp-file path
              (insert-buffer-substring e-current-config-e2e-test--output))
            exit))
      (kill-buffer e-current-config-e2e-test--output))))

(provide 'e-current-config-e2e-test)

;;; e-current-config-e2e-test.el ends here
