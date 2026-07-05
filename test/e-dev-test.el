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
(require 'e-harness)
(require 'e-openai)

(ert-deftest e-dev-test-clears-removed-sync-work-api-functions ()
  "Reload cleanup unbinds stale definitions for removed sync Work APIs."
  (let ((symbols '(e-backend-stream
                   e-loop-run-turn
                   e-harness-compact-session
                   e-harness-follow-up
                   e-harness-prompt
                   e-harness-wait
                   e-tools-execute)))
    (dolist (symbol symbols)
      (fset symbol (lambda (&rest _args) :stale)))
    (unwind-protect
        (progn
          (e-dev--clear-obsolete-functions)
          (dolist (symbol symbols)
            (should-not (fboundp symbol))))
      (dolist (symbol symbols)
        (when (fboundp symbol)
          (fmakunbound symbol))))))

(ert-deftest e-dev-test-mark-reload-required-records-status-and-clear ()
  "Reload-required notification records pending full reload intent."
  (let ((e-dev--reload-required-entries nil))
    (let ((status (e-dev-mark-reload-required
                   "core shape changed"
                   ["lisp/core/e-harness.el"]
                   'full)))
      (should (eq (plist-get status :required) t))
      (should (= (plist-get status :count) 1))
      (let ((entry (car (plist-get status :entries))))
        (should (string-match-p "reload-required-"
                                (plist-get entry :id)))
        (should (equal (plist-get entry :reason)
                       "core shape changed"))
        (should (equal (plist-get entry :files)
                       '("lisp/core/e-harness.el")))
        (should (eq (plist-get entry :scope) 'full))))
    (let ((status (e-dev-clear-reload-required)))
      (should-not (plist-get status :required))
      (should (= (plist-get status :count) 0)))))

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
    (should (e-action-p mark))
    (should (e-action-p status))
    (let ((result (funcall (e-action-caller mark)
                           nil
                           '(:reason "needs full reload"
                             :files ["lisp/dev/e-dev.el"]
                             :scope "full"))))
      (should (eq (plist-get result :required) t))
      (should (= (plist-get result :count) 1)))
    (let ((result (funcall (e-action-caller status) nil nil)))
      (should (eq (plist-get result :required) t))
      (should (= (plist-get result :count) 1)))))

(ert-deftest e-dev-test-reload-restores-mvp-entrypoints ()
  "Reload loads the MVP modules and restores their entry points."
  (fmakunbound 'e-chat)
  (fmakunbound 'e-chat-new)
  (fmakunbound 'e-chat-resume)
  (fmakunbound 'e-chat-rename)
  (fmakunbound 'e-chat-set-model)
  (fmakunbound 'e-chat-set-effort)
  (fmakunbound 'e-chat-open)
  (fmakunbound 'e-base-layer-create)
  (fmakunbound 'e-emacs-base-layer-create)
  (fmakunbound 'e-layer-create)
  (fmakunbound 'e-runtime-context-capability-create)
  (fmakunbound 'e-operation-create)
  (fmakunbound 'e-resource-method-create)
  (fmakunbound 'e-resources-call)
  (fmakunbound 'e-shell-create)
  (fmakunbound 'e-shell-command-create)
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
  (should (fboundp 'e-layer-create))
  (should (fboundp 'e-runtime-context-capability-create))
  (should (fboundp 'e-operation-create))
  (should (fboundp 'e-resource-method-create))
  (should (fboundp 'e-resources-call))
  (should (fboundp 'e-shell-create))
  (should (fboundp 'e-shell-command-create))
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
  (should (equal e-openai-default-model "gpt-5.5"))
  (should (e-layer-get 'agents-std-context))
  (should (equal e-default-chat-layer-ids
                 '(agents-std-context harness-base harness-advanced e os-base
                                      emacs-base web text-editing org-canvas
                                      project-local)))
  (should (eq e-debug-display-strategy 'popup))
  (let ((harness (e-harness-registry-get-or-create :chat-default)))
    (should (equal (e-harness-enabled-layer-ids harness)
                   '(agents-std-context harness-base harness-advanced e os-base
                                        emacs-base web text-editing org-canvas
                                        project-local)))
    (should (equal (e-harness-effective-layer-ids harness)
                   '(agents-std-context harness-base harness-advanced e os-base
                                        emacs-base web text-editing org-canvas
                                        project-local)))))

(provide 'e-dev-test)

;;; e-dev-test.el ends here
