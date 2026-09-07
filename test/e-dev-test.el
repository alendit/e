;;; e-dev-test.el --- Tests for e development reload -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for interactive development reload behavior.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-default-harnesses)
(require 'e-debug)
(require 'e-dev)
(require 'e-dev-layer)
(require 'e-dev-perf)
(require 'e-dev-profile)
(require 'e-harness)
(require 'e-openai)
(require 'e-work)
(require 'e-sqlite-test-store-support
         (expand-file-name
          "e-sqlite-test-store-support.el"
          (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest e-dev-test-mark-reload-required-records-status-and-clear ()
  "Reload-required notification records pending restart intent."
  (let ((e-dev--reload-required-entries nil))
    (let ((status (e-dev-mark-reload-required
                   "core shape changed"
                   ["lisp/core/e-harness.el"]
                   'restart)))
      (should (eq (plist-get status :required) t))
      (should (= (plist-get status :count) 1))
      (let ((entry (car (plist-get status :entries))))
        (should (string-match-p "reload-required-"
                                (plist-get entry :id)))
        (should (equal (plist-get entry :reason)
                       "core shape changed"))
        (should (equal (plist-get entry :files)
                       '("lisp/core/e-harness.el")))
        (should (eq (plist-get entry :scope) 'restart))))
    (let ((status (e-dev-clear-reload-required)))
      (should-not (plist-get status :required))
      (should (= (plist-get status :count) 0)))))

(ert-deftest e-dev-test-extension-reload-preserves-restart-requirements ()
  "A supported reload clears only entries that it can actually satisfy."
  (let ((e-dev--reload-required-entries
         '((:reason "shell" :scope reloadable)
           (:reason "core" :scope restart)
           (:reason "old core" :scope full))))
    (e-dev--clear-reloadable-required-entries)
    (should (equal (mapcar (lambda (entry) (plist-get entry :reason))
                           e-dev--reload-required-entries)
                   '("core" "old core")))))

(ert-deftest e-dev-test-dev-layer-exposes-reload-required-actions ()
  "The e-dev layer exposes lightweight reload notification actions."
  (let* ((e-dev--reload-required-entries nil)
         (layer (e-dev-layer-create))
         (capability
          (cl-find-if
           (lambda (capability)
             (eq (e-capability-id capability) 'e-dev))
           (e-layer-capabilities layer)))
         (actions (and capability (e-capability-actions capability)))
         (mark (plist-get actions :mark-reload-required))
         (status (plist-get actions :reload-required-status)))
    (should capability)
    (should (stringp (e-capability-instructions capability)))
    (should (string-match-p "mark-reload-required"
                            (e-capability-instructions capability)))
    (should (e-action-p mark))
    (should (e-action-p status))
    (let* ((handle (e-work-start
                    (e-action-work mark)
                    '(:reason "needs restart"
                      :files ["lisp/dev/e-dev.el"]
                      :scope "restart")))
           (result (e-work-handle-result handle)))
      (should (eq (plist-get result :required) t))
      (should (= (plist-get result :count) 1)))
    (let* ((handle (e-work-start (e-action-work status) nil))
           (result (e-work-handle-result handle)))
      (should (eq (plist-get result :required) t))
      (should (= (plist-get result :count) 1)))))

(ert-deftest e-dev-test-gpt56-serializes-instructions-as-text ()
  "The real e-dev capability maps to a string-valued Responses text block."
  (let* ((capability
          (cl-find-if
           (lambda (candidate)
             (eq (e-capability-id candidate) 'e-dev))
           (e-layer-capabilities (e-dev-layer-create))))
         (context (e-capabilities-context (list capability)))
         (messages (plist-get context :messages))
         (segments (plist-get context :segments))
         (body
          (e-openai-codex-request-body
           :messages messages
           :options
           (list :model "gpt-5.6-sol"
                 :prompt-cache-key "e-dev-test"
                 :segments segments)))
         (input (append (plist-get body :input) nil))
         (text-blocks
          (cl-loop for item in input
                   append (append (plist-get item :content) nil))))
    (should (= (length input) 1))
    (should (equal (plist-get (car input) :role) "developer"))
    (should (= (length text-blocks) 1))
    (should (cl-every (lambda (block)
                        (stringp (plist-get block :text)))
                      text-blocks))))

(ert-deftest e-dev-test-reloadable-source-files-exclude-core-and-adapters ()
  "Reload discovery includes loaded seams but excludes cardinal runtime code."
  (let ((files (e-dev--reloadable-source-files default-directory)))
    (should (member (expand-file-name "lisp/shells/chat/e-chat.el"
                                      default-directory)
                    files))
    (should (member (expand-file-name "lisp/defaults/e-default-layers.el"
                                      default-directory)
                    files))
    (should-not (seq-some
                 (lambda (file)
                   (or (string-match-p "/lisp/core/" file)
                       (string-match-p "/lisp/adapters/" file)))
                 files))))

(ert-deftest e-dev-test-reload-restores-extension-entrypoints ()
  "Reload restores entry points owned by supported extension seams."
  (fmakunbound 'e-chat)
  (fmakunbound 'e-chat-new)
  (fmakunbound 'e-chat-resume)
  (fmakunbound 'e-chat-rename)
  (fmakunbound 'e-chat-set-model)
  (fmakunbound 'e-chat-set-effort)
  (fmakunbound 'e-chat-open)
  (fmakunbound 'e-base-layer-create)
  (fmakunbound 'e-emacs-base-layer-create)
  (fmakunbound 'e-runtime-context-capability-create)
  (fmakunbound 'e-chat-shell)
  (fmakunbound 'e-dev-profile-start)
  (fmakunbound 'e-dev-profile-stop)
  (fmakunbound 'e-dev-profile-report)
  (fmakunbound 'e-dev-profile-open-latest)
  (fmakunbound 'e-dev-perf-run)
  (fmakunbound 'e-dev-perf-run-scenario)
  (fmakunbound 'e-dev-perf-report)
  (fmakunbound 'e-dev-perf-list-scenarios)
  (fmakunbound 'e-dev-perf-update-baseline)
  (e-dev-reload default-directory)
  (should (commandp 'e-chat))
  (should (commandp 'e-chat-new))
  (should (commandp 'e-chat-resume))
  (should (commandp 'e-chat-rename))
  (should (commandp 'e-chat-set-model))
  (should (commandp 'e-chat-set-effort))
  (should (fboundp 'e-chat-open))
  (should (fboundp 'e-base-layer-create))
  (should (fboundp 'e-emacs-base-layer-create))
  (should (fboundp 'e-runtime-context-capability-create))
  (should (fboundp 'e-chat-shell))
  (should (commandp 'e-dev-profile-start))
  (should (commandp 'e-dev-profile-stop))
  (should (commandp 'e-dev-profile-report))
  (should (commandp 'e-dev-profile-open-latest))
  (should (commandp 'e-dev-perf-run))
  (should (commandp 'e-dev-perf-run-scenario))
  (should (commandp 'e-dev-perf-report))
  (should (commandp 'e-dev-perf-list-scenarios))
  (should (commandp 'e-dev-perf-update-baseline))
  (should (eq (e-shell-id (e-shell-get 'chat)) 'chat)))

(ert-deftest e-dev-test-reload-refreshes-defaults ()
  "Reload reapplies changed default options."
  (let ((e-openai-default-model e-openai-default-model))
    (setq e-openai-default-model "gpt-5.4")
    (setq e-default-layer-specs
          '((:id e
             :name "e"
             :summary "Runtime self-management commands."
             :feature e-layer
             :factory e-core-layer-create)
            (:id e-dev
             :name "e Dev"
             :summary "Development context inspection tools."
             :feature e-dev-layer
             :factory e-dev-layer-create)
            (:id harness-base
             :name "Harness Base"
             :summary "Harness-owned support resources and tool lifecycle guards."
             :feature e-harness-base
             :factory e-harness-base-layer-create)
            (:id os-base
             :name "OS Base"
             :summary "Workspace file and shell tools."
             :feature e-base
             :factory e-base-layer-create)
            (:id emacs-base
             :name "Emacs Base"
             :summary "Live Emacs buffer awareness and editing tools."
             :feature e-emacs-base
             :factory e-emacs-base-layer-create)))
    (setq e-default-chat-layer-ids '(agents-std-context harness-base e os-base emacs-base))
    (setq e-debug-display-strategy 'tab)
    (let ((e-default-chat-harness-factory
           (lambda (&rest args)
             (e-harness-create
              :backend (e-backend-fake-create :items nil)
              :sessions (plist-get args :sessions))))
          (e-startup-shell-hook
           (cons (lambda ()
                   (e-harness-registry-get-or-create :chat-default))
                 e-startup-shell-hook)))
      (e-dev-reload default-directory))
    ;; Provider defaults belong to a restart-required adapter module and are not
    ;; rewritten by an extension reload.
    (should (equal e-openai-default-model "gpt-5.4"))
    (should (e-layer-get 'agents-std-context))
    (should (equal e-default-chat-layer-ids
                   '(agents-std-context harness-base process-reporting
                                        harness-advanced e os-base emacs-base
                                        resource-toc web annotations org-canvas
                                        project-local writing
                                        subagents-parent)))
    (should (eq e-debug-display-strategy 'popup))
    (let ((harness (e-harness-registry-get-or-create :chat-default)))
      (should (equal (e-harness-enabled-layer-ids harness)
                     '(agents-std-context harness-base process-reporting
                                          harness-advanced e os-base emacs-base
                                          resource-toc web annotations org-canvas
                                          project-local writing
                                          subagents-parent)))
      (should (equal (e-harness-effective-layer-ids harness)
                     '(agents-std-context harness-base process-reporting
                                          harness-advanced e resource-discovery
                                          os-base async-control emacs-base resource-toc
                                          web annotations org-canvas project-local
                                          writing subagents-parent))))))

(provide 'e-dev-test)

;;; e-dev-test.el ends here
