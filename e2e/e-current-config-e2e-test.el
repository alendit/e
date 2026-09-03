;;; e-current-config-e2e-test.el --- Current configuration compatibility -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Focused checks for a private Emacs started through the developer's normal
;; user configuration.  The default compatibility selector sends no provider
;; request.  The explicit live selector exercises the configured default chat
;; backend against test-owned runtime state.

;;; Code:

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'e-e2e-bootstrap)
(require 'e-backend)
(require 'e-board)
(require 'e-board-e2e-support)
(require 'e-capabilities)
(require 'e-context)
(require 'e-context-lifetime)
(require 'e-core)
(require 'e-default-harnesses)
(require 'e-harness-registry)
(require 'e-project-local)
(require 'e-session)

(defvar e-current-config-e2e-test--output nil
  "Dynamically bound buffer collecting the current-config ERT report.")

(defconst e-current-config-e2e-test--state-directory
  (file-name-as-directory
   (or (getenv "E_CURRENT_CONFIG_E2E_STATE_DIR")
       (error "E_CURRENT_CONFIG_E2E_STATE_DIR is required")))
  "Temporary directory for current-config E2E persistence snapshots.")

(defun e-current-config-e2e-test--prepare-recentf-snapshot ()
  "Redirect Recentf persistence to a test-owned snapshot.
Copy the configured cache before the first file loads it, so the compatibility
test observes the same startup state without allowing its private daemon to
rewrite the user's file."
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

(defun e-current-config-e2e-test--assert-reasoning-is-combined
    (harness session-id board)
  "Assert any real-adapter reasoning is one combined record per request."
  (let* ((types '(reasoning-delta reasoning-raw-delta))
         (activities
          (seq-filter
           (lambda (event) (memq (plist-get event :event-type) types))
           (e-session-activity-events (e-harness-sessions harness) session-id)))
         (board-reasoning
          (seq-filter
           (lambda (message)
             (memq (e-board-message-activity-kind message) types))
           (e-board-messages board)))
         activity-keys
         board-keys)
    (dolist (event activities)
      (let* ((payload (plist-get event :payload))
             (key (list (plist-get event :event-type)
                        (plist-get payload :provider-request-id))))
        (should (stringp (plist-get payload :content)))
        (should (eq (plist-get payload :content-mode) 'snapshot))
        (should (plist-get payload :combined))
        (should-not (member key activity-keys))
        (push key activity-keys)))
    (dolist (message board-reasoning)
      (let* ((attributes (e-board-message-attributes message))
             (key (list (e-board-message-activity-kind message)
                        (plist-get attributes :provider-request-id))))
        (should (stringp (e-board-message-content message)))
        (should (eq (plist-get attributes :content-mode) 'snapshot))
        (should (plist-get attributes :combined))
        (should-not (member key board-keys))
        (push key board-keys)))
    (should (= (length activities) (length board-reasoning)))))

(ert-deftest e-current-config-e2e-test-first-file-hooks-succeed ()
  "The first file opens with the current configuration's real persisted state."
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
  "Every configured default project is reachable through project-local policy."
  (dolist (root (e-default-project-roots))
    (should (file-directory-p root))
    (should (e-project-local--root-allowed-p root))
    (let ((inspection (e-project-local--inspection root)))
      (when (plist-get inspection :has-extensions)
        (should (e-layer-p (e-project-local-prime-project root)))))))

(ert-deftest e-current-config-live-e2e-test-basic-assistant-response ()
  "A first chat turn completes through the machine's configured adapter.

The private current-config Emacs owns its runtime directory.  This test takes
the backend and model defaults from the actual `:chat-default' factory loaded
by normal startup, adds only test-owned context, submits through the Board/chat
boundary, and fails normally when configuration, transport, curation,
persistence, or response delivery fails.  It never skips."
  (should (e-e2e-current-config-p))
  (should e-e2e-current-config-loaded-e-p)
  (let* ((configured-harness
          (e-harness-registry-get-or-create :chat-default))
         (backend (e-harness-backend configured-harness))
         (nonce (format "E-CURRENT-%08x" (random #x100000000)))
         (provider
          (e-context-provider-create
           :name 'current-config-e2e-dynamic-context
           :cache-placement 'dynamic-context
           :build
           (lambda (&rest _)
             (list
              (list :role 'system
                    :content
                    (format "The required reply token is %s." nonce))))))
         (capability
          (e-capability-create
           :id 'current-config-e2e-dynamic-context
           :context-providers (list provider)))
         (harness
          (e-harness-create
           :backend backend
           :default-options
           (copy-tree (e-harness-default-options configured-harness))
           :project-root
           (e-harness-default-project-root configured-harness)
           :sessions (e-harness-sessions configured-harness)
           :intrinsic-capabilities (list capability)))
         session-id)
    (should (e-harness-p harness))
    (should (e-backend-p backend))
    (should-not (equal (e-backend--name backend)
                       "Unconfigured default chat backend"))
    (should (e-context-lifetime-shadow-enabled-p))
    (e-current-config-e2e-test--print
     "Current-config live backend: %s\n" (e-backend--name backend))
    (unwind-protect
        (progn
          (setq session-id
                (e-board-e2e-create-session
                 harness
                 :metadata
                 (list :project-root
                       e-current-config-e2e-test--state-directory)))
          (let* ((result
                  (e-board-e2e-prompt-batch
                   harness session-id
                   "Use the supplied test context. Reply with its required token and no extra words."))
                 (assistant
                  (or (plist-get result :assistant-content)
                      (plist-get (plist-get result :result)
                                 :assistant-content)
                      ""))
                 (messages
                  (e-session-messages (e-harness-sessions harness) session-id))
                 (binding (e-chat-service-binding harness session-id))
                 (board
                  (e-board-registry-board-source-board
                   (e-chat-service-binding-board binding))))
            (should (eq (plist-get result :status) 'done))
            (should (string-match-p (regexp-quote nonce) assistant))
            (should
             (seq-some
              (lambda (message)
                (and (eq (plist-get message :role) 'assistant)
                     (string-match-p
                      (regexp-quote nonce)
                      (format "%s" (plist-get message :content)))))
              messages))
            (should
             (seq-some
              (lambda (message)
                (and (eq (e-board-message-kind message) 'output)
                     (string-match-p
                      (regexp-quote nonce)
                      (format "%s" (e-board-message-content message)))))
              (e-board-messages board)))
            (e-current-config-e2e-test--assert-reasoning-is-combined
             harness session-id board)))
      (e-board-e2e-reset-runtime))))

(defun e-current-config-e2e-test-run-to-file (path &optional selector)
  "Run current-config ERT SELECTOR, write its report to PATH, and return status."
  (let ((selector (or selector "^e-current-config-e2e-test-"))
        (e-current-config-e2e-test--output
         (generate-new-buffer " *e current-config ERT report*")))
    (unwind-protect
        (progn
          (e-current-config-e2e-test--print
           "Current-config E2E: Emacs %s, init %S, e %s\n"
           emacs-version
           e-e2e-current-config-user-init-file
           (e-source-directory))
          (let ((original-message (symbol-function 'message)))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest arguments)
                         (when format-string
                           (e-current-config-e2e-test--print
                            "%s\n"
                            (apply #'format-message format-string arguments)))
                         (apply original-message format-string arguments))))
              (let* ((standard-output e-current-config-e2e-test--output)
                     (stats (ert-run-tests-batch selector))
                     (unexpected (ert-stats-completed-unexpected stats))
                     (exit (if (zerop unexpected) 0 1)))
                (e-current-config-e2e-test--print
                 "Current-config E2E complete: %d tests, %d unexpected.\n"
                 (ert-stats-total stats) unexpected)
                (with-temp-file path
                  (insert-buffer-substring e-current-config-e2e-test--output))
                exit))))
      (kill-buffer e-current-config-e2e-test--output))))

(provide 'e-current-config-e2e-test)

;;; e-current-config-e2e-test.el ends here
