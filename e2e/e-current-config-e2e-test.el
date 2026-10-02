;;; e-current-config-e2e-test.el --- Current configuration startup checks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Focused checks for a private Emacs started through the developer's normal
;; user configuration.  These checks do not issue provider requests and never
;; access the user's runtime database.

;;; Code:

(require 'ert)
(require 'bytecomp)
(require 'cl-lib)
(require 'seq)
(require 'e-e2e-bootstrap)
(require 'e-actions)
(require 'e-async-control)
(require 'e-backend)
(require 'e-board)
(require 'e-board-orchestration-actions)
(require 'e-board-sqlite-service)
(require 'e-core)
(require 'e-default-harnesses)
(require 'e-chat-service)
(require 'e-anthropic)
(require 'e-openai)
(require 'e-cron)
(require 'e-harness-registry)
(require 'e-harness-instances)
(require 'e-layers)
(require 'e-project-local)
(require 'e-runtime-sqlite)
(require 'e-session-sqlite)
(require 'e-session-storage)
(require 'e-subagent-runner)
(require 'e-task-queue)
(require 'e-tools)
(require 'e-await-tool)
(require 'e-waitable)
(require 'e-work)

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

(defun e-current-config-e2e-test--grimoire-root ()
  "Return the configured Grimoire root, requiring its topic layer source."
  (or
   (seq-find
    (lambda (root)
      (file-readable-p
       (expand-file-name ".e/layers/topic/layer.el" root)))
    (e-default-project-roots))
   (ert-fail "Current configuration has no configured Grimoire topic layer")))

(defun e-current-config-e2e-test--grimoire-sources (root)
  "Return the exact configured Grimoire consumer sources below ROOT."
  (mapcar
   (lambda (relative)
     (let ((file (expand-file-name relative root)))
       (unless (file-readable-p file)
         (ert-fail (format "Configured Grimoire source is missing: %s" file)))
       file))
   '(".e/layers/topic/capabilities/topic.el"
     ".e/layers/topic/commit.el"
     ".e/layers/topic/session.el"
     ".e/layers/topic/daily-coordinator.el"
     ".e/layers/topic/daily-run.el"
     ".e/layers/topic/cron.el"
     ".e/layers/topic/shells/topic.el")))

(defun e-current-config-e2e-test--load-and-compile-grimoire (root)
  "Load and byte-compile the exact configured Grimoire consumer at ROOT."
  (let* ((sources (e-current-config-e2e-test--grimoire-sources root))
         (compile-directory
          (expand-file-name "grimoire-byte-compile/"
                            e-current-config-e2e-test--state-directory))
         (load-path (cons (expand-file-name ".e/layers/topic" root)
                          load-path)))
    (make-directory compile-directory t)
    (dolist (source sources)
      (load-file source))
    ;; Compile byte-for-byte copies.  Current-config gates must never refresh
    ;; or create artifacts inside a configured consumer checkout.
    (let ((byte-compile-error-on-warn t)
          ;; Emacs 31 obsoletes compatibility macros still supported by older
          ;; Emacsen.  Keep that migration separate, but reject every other
          ;; warning from the configured consumer, including unused arguments.
          (byte-compile-warnings '(not obsolete)))
      (dolist (source sources)
        (let ((copy
               (expand-file-name
                (format "%s-%s.el"
                        (file-name-base source)
                        (substring (secure-hash 'sha256 source) 0 12))
                compile-directory)))
          (copy-file source copy t)
          (should (byte-compile-file copy)))))
    (should (file-equal-p
             (symbol-file 'grimoire-topic-daily 'defun)
             (nth 0 sources)))
    (should (file-equal-p
             (symbol-file 'grimoire-topic--ensure-daily-owner-board-start
                          'defun)
             (nth 2 sources)))
    (should (file-equal-p
             (symbol-file 'grimoire-daily-run-start 'defun)
             (nth 4 sources)))
    (should (file-equal-p
             (symbol-file 'grimoire-daily-update-cron-register 'defun)
             (nth 5 sources)))
    (should (file-equal-p
             (symbol-file 'grimoire-daily 'defun)
             (nth 6 sources)))
    sources))

(ert-deftest e-current-config-e2e-test-grimoire-cron-is-publish-supervised ()
  "Cold startup creates the supervisor Board before arming its publish-only schedule."
  (let* ((repo-root (file-name-as-directory
                     (file-truename
                      (e-current-config-e2e-test--grimoire-root))))
         (schedule nil)
         (registration nil)
         (deadline (+ (float-time) 20.0)))
    ;; Registration and SQLite hydration are independent asynchronous events.
    ;; Re-read both the live schedule and the process-local Board-create record
    ;; on every turn; a one-shot schedule snapshot can observe the pre-creation
    ;; disabled state forever.
    (while (and (< (float-time) deadline)
                (not
                 (progn
                   (setq schedule (e-cron-get 'grimoire-daily-update)
                         registration
                         (and (boundp
                               'grimoire-daily-update-cron--registrations)
                              (gethash
                               repo-root
                               grimoire-daily-update-cron--registrations)))
                   (and schedule registration
                        (e-cron-schedule-storage-ready-p schedule)
                        (e-cron-schedule-enabled schedule)
                        (eq (plist-get registration :health) 'healthy)
                        (eq (plist-get registration :board-state) 'finished)
                        (let ((board-create
                               (plist-get registration :board-create)))
                          (and board-create
                               (eq (plist-get (e-work-status board-create) :state)
                                   'finished)))))))
      (accept-process-output nil 0.05))
    (setq schedule (e-cron-get 'grimoire-daily-update)
          registration
          (and (boundp 'grimoire-daily-update-cron--registrations)
               (gethash repo-root grimoire-daily-update-cron--registrations)))
    (should schedule)
    (should registration)
    (should (eq (plist-get registration :board-state) 'finished))
    (should (eq (plist-get registration :health) 'healthy))
    (should (eq (plist-get
                 (e-work-status (plist-get registration :board-create)) :state)
                'finished))
    (should (e-cron-schedule-enabled schedule))
    (should (e-cron-schedule-storage-ready-p schedule))
    (should (e-cron-schedule-next-fire schedule))
    (should (time-less-p (current-time)
                         (e-cron-schedule-next-fire schedule)))
    (let* ((metadata (e-cron-schedule-metadata schedule))
           (action (plist-get metadata :action-spec))
           (target (plist-get metadata :publication-target)))
      (should (eq (e-cron-schedule-catch-up schedule) 'skip))
      (should (plist-get metadata :publication-target-explicit))
      (should (equal (plist-get metadata :observer-policy)
                     'weekday-work-hours))
      (should (equal (plist-get target :kind) 'sqlite-board))
      (should (stringp (plist-get target :board-id)))
      (should (equal (plist-get registration :board-id)
                     (plist-get target :board-id)))
      (should (equal (car action) :publish))
      (should-not (e-cron-schedule-guard schedule))
      ;; The same runtime SQLite authority must contain the created,
      ;; inspectable supervisory Board before this health assertion passes.
      (let* ((harness (e-chat-service-default-harness))
             (runtime (e-session-storage-runtime-store
                       (e-harness-sessions harness)))
             (service (e-board-sqlite-service-create runtime))
             (board
              (e-work-with-batch-await
                (e-work-await-batch
                 (e-board-sqlite-service-board-get-start
                  service (plist-get target :board-id))
                 :timeout 10.0))))
        (should (equal (plist-get board :board-id)
                       (plist-get target :board-id)))))))

(defun e-current-config-e2e-test--initialize-git-repo (repo)
  "Create a minimal clean disposable Grimoire repository at REPO."
  (make-directory (expand-file-name ".e/layers/topic" repo) t)
  (make-directory (expand-file-name "scripts" repo) t)
  (make-directory (expand-file-name "daily" repo) t)
  (with-temp-file (expand-file-name ".e/layers/topic/layer.el" repo)
    (insert ";;; disposable current-config topic layer marker\n"))
  ;; Keep the exact configured Daily consumer's runtime-owned collector
  ;; executable without reading the real Grimoire checkout's live scripts.
  ;; The fixture settles immediately and materializes the same declared JSON
  ;; shape, so this E2E proves Board/work dispatch rather than host session
  ;; discovery.
  (with-temp-file (expand-file-name "scripts/agentic-sessions.py" repo)
    (insert
     "#!/usr/bin/env python3\n"
     "import argparse, json, os, tempfile\n"
     "parser = argparse.ArgumentParser()\n"
     "parser.add_argument('--since', required=True)\n"
     "parser.add_argument('--date', required=True)\n"
     "parser.add_argument('--output', required=True)\n"
     "args = parser.parse_args()\n"
     "os.makedirs(os.path.dirname(args.output), exist_ok=True)\n"
     "payload = {'date': args.date, 'since': args.since,\n"
     "           'days': [{'date': args.date}], 'claude': [],\n"
     "           'codex': [], 'cursor': [], 'e': [], 'errors': []}\n"
     "directory = os.path.dirname(args.output) or '.'\n"
     "fd, temporary = tempfile.mkstemp(prefix='.agentic-', dir=directory, text=True)\n"
     "try:\n"
     "    with os.fdopen(fd, 'w', encoding='utf-8') as stream:\n"
     "        json.dump(payload, stream)\n"
     "        stream.flush()\n"
     "        os.fsync(stream.fileno())\n"
     "    os.replace(temporary, args.output)\n"
     "finally:\n"
     "    if os.path.exists(temporary):\n"
     "        os.unlink(temporary)\n"))
  (with-temp-file (expand-file-name "daily/2099-01-01.org" repo)
    (insert "#+title: Daily 2099-01-01\n\n* Log\n"))
  (dolist (arguments
           '(("init" "-q")
             ("config" "user.name" "Current Config Test")
             ("config" "user.email" "current-config@example.invalid")
             ("add" ".")
             ("commit" "-qm" "baseline")))
    (should (zerop (apply #'process-file "git" nil nil nil
                          "-C" repo arguments)))))

(defun e-current-config-e2e-test--await-reference (reference)
  "Pass exact work REFERENCE to `await' and return its structured result."
  (let ((registry (e-tools-registry-create)) result unexpected)
    (e-await-tool-register registry)
    (e-tools-start
     registry
     (list :id "current-config-await" :name "await"
           :arguments (list :refs (vector reference) :timeout 15))
     :on-done (lambda (value) (setq result value))
     :on-error (lambda (error) (setq unexpected error)))
    (let ((deadline (+ (float-time) 20.0)))
      (while (and (not result) (not unexpected) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (when unexpected
      (signal (car unexpected) (cdr unexpected)))
    (or result (ert-fail "Timed out awaiting configured Daily action"))))

(defun e-current-config-e2e-test--retire-private-harness (harness)
  "Retire HARNESS bindings before the private test process exits."
  (when-let* ((bindings (gethash harness e-chat-service--bindings)))
    (maphash (lambda (_session-id binding)
               (ignore-errors (e-chat-service--retire-binding binding)))
             bindings)))

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

(ert-deftest e-current-config-e2e-test-package-command-inventory-is-current ()
  "Normal startup exposes the replacement command and no retired command."
  (let ((board-activity
         (plist-get e-e2e-current-config-package-command-state
                    :board-activity))
        (retired
         (plist-get e-e2e-current-config-package-command-state
                    :retired-subagent-list))
        (retired-runs
         (plist-get e-e2e-current-config-package-command-state
                    :retired-board-runs-list)))
    (should (plist-get board-activity :commandp))
    (should-not (plist-get retired :commandp))
    (should-not (plist-get retired-runs :commandp))))

(ert-deftest e-current-config-e2e-test-default-harness-is-configured ()
  "The current config selects cached Opus 5.5 chat and keeps GPT-6 subagents."
  (should (memq :chat-default (e-harness-registry-list)))
  (let* ((harness (e-harness-registry-get-or-create :chat-default))
         (backend (e-harness-backend harness))
         (options (e-harness-default-options harness))
         (profile (e-anthropic-provider-profile
                   'eng-ai-gateway-opus-5-5)))
    (should (e-harness-p harness))
    (should (e-backend-p backend))
    (should (equal (plist-get (e-default-chat-harness-spec) :factory)
                   'dvorona/e-default-chat-harness-create-anthropic))
    (should (eq e-anthropic-default-provider 'eng-ai-gateway-opus-5-5))
    (should (equal e-anthropic-default-model "claude-opus-5-5"))
    (should (equal (custom--standard-value 'e-anthropic-default-model)
                   "claude-opus-5-5"))
    (should (equal (plist-get profile :default-model) "claude-opus-5-5"))
    (should (= (plist-get profile :max-tokens) 128000))
    (should (equal (plist-get profile :env-key) "ENG_AI_MODEL_GW_KEY"))
    (should (equal (e-backend--name backend)
                   "Engineering AI Model Gateway / Opus 5.5 (Messages)"))
    (should (equal (plist-get options :model) "claude-opus-5-5"))
    (should (= (plist-get options :max-tokens) 128000))
    (should (eq (plist-get options :prompt-cache) t))
    (should (equal (plist-get options :prompt-cache-ttl) "1h"))
    (should (e-harness-effective-layer-ids harness))
    (dolist (case '((eng-ai-gateway-opus "claude-opus-4-8")
                    (eng-ai-gateway-sonnet "claude-sonnet-5")
                    (eng-ai-gateway-haiku "claude-haiku-4-5-20251001")
                    (eng-ai-gateway-fable "claude-fable-5")))
      (should (equal (plist-get (e-anthropic-provider-profile (car case))
                                :default-model)
                     (cadr case))))
    (should (eq e-openai-default-provider 'codex))
    (should (equal e-openai-default-model "gpt-6-sol"))
    (dolist (case '((:tool-user "gpt-6-sol" tool-user "high")
                    (:fast-tool-user "gpt-6-luna" fast-tool-user "max")
                    (:plan-implementer "gpt-6-sol" plan-implementer nil)))
      (let* ((instance (e-harness-instance-get (car case)))
             (metadata (e-harness-instance-metadata instance)))
        (should (e-harness-instance-subagent-p instance))
        (should (equal (plist-get metadata :model) (cadr case)))
        (should (eq (plist-get metadata :role) (nth 2 case)))
        (when (nth 3 case)
          (should (equal (plist-get metadata :reasoning-effort)
                         (nth 3 case))))))))

(ert-deftest e-current-config-e2e-test-default-projects-integrate ()
  "Every configured default project is reachable through project policy."
  (dolist (root (e-default-project-roots))
    (should (file-directory-p root))
    (should (e-project-local--root-allowed-p root))
    (let ((inspection (e-project-local--inspection root)))
      (when (plist-get inspection :has-extensions)
        (should (e-layer-p (e-project-local-prime-project root)))))))

(ert-deftest e-current-config-e2e-test-grimoire-daily-is-board-first-and-awaitable ()
  "The exact configured Daily consumer starts through one awaitable Board action."
  (let* ((configured-root (e-current-config-e2e-test--grimoire-root))
         (_sources
          (e-current-config-e2e-test--load-and-compile-grimoire
           configured-root))
         (repo (expand-file-name "grimoire-consumer/"
                                 e-current-config-e2e-test--state-directory))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-subagent--configured-harnesses
          (make-hash-table :test 'eq :weakness 'key))
         (e-subagent-runner--live-owner (e-subagent-live-create))
         (e-work--detached-handles (make-hash-table :test 'equal))
         (e-waitable--resolvers (make-hash-table :test 'equal))
         sessions harness daily-capability board-capability daily-file)
    (unwind-protect
        (progn
          (e-current-config-e2e-test--initialize-git-repo repo)
          (setq sessions
                (e-runtime-sqlite-session-store (e-default-runtime))
                harness
                (e-harness-create
                 :backend (e-backend-fake-create :items nil :delay 0.01)
                 :sessions sessions :enabled-layer-ids nil))
          (e-session-enable sessions)
          (e-harness-instance-register
           :id :chat-default :kind 'chat :harness-id :chat-default
           :factory (lambda () harness) :default t)
          (should (e-layer-get 'slack-mcp))
          (dolist (id '(:tool-user :fast-tool-user))
            (e-harness-instance-register
             :id id :kind (intern (substring (symbol-name id) 1))
             :subagent t
             :factory
             (lambda ()
               (e-harness-create
                :backend (e-backend-fake-create :items nil :delay 0.01)
                :sessions sessions :enabled-layer-ids nil))))
          (setq board-capability (e-board-capability-create)
                daily-capability
                (grimoire-daily-run-capability-create
                 (expand-file-name ".e/layers/topic/" repo)))
          (dolist (key '(:list :status :read :list-runs :run-status))
            (let ((action (e-capabilities-action-spec board-capability key)))
              (should (e-action-p action))
              (should (and (stringp (e-action-description action))
                           (not (string-empty-p
                                 (e-action-description action)))))))
          (e-harness-activate-capability harness board-capability)
          (e-harness-activate-capability harness daily-capability)
          (e-async-control-register-work-resolver)
          (cl-letf (((symbol-function 'e-chat-service-default-harness)
                     (lambda () harness))
                    ((symbol-function 'e-subagent-direct-runner)
                     (lambda (&rest _arguments) (list :cancel #'ignore))))
            (let* ((reference
                    (e-actions-call
                     'daily-run :start '(:date "2099-01-02")
                     (list :harness harness
                           :turn-id "current-config-daily")))
                   (awaited
                    (progn
                      (should (string-match-p "\\`work:[^[:space:]]+\\'"
                                              reference))
                      (e-current-config-e2e-test--await-reference reference)))
                   (content (plist-get awaited :content))
                   (results (plist-get content :results))
                   (entry (aref results 0))
                   (started (plist-get entry :result)))
              (should (eq (plist-get awaited :status) 'ok))
              (should (plist-get content :settled))
              (should (vectorp results))
              (should (= (length results) 1))
              (should (equal (plist-get entry :ref) reference))
              (unless (equal (plist-get entry :state) "finished")
                (ert-fail (format "Configured Daily action failed: %S" entry)))
              (should (equal (plist-get started :status) "dispatched"))
              (should (eq (plist-get started :foreground) :json-false))
              (should (= (plist-get started :selected) 7))
              (should (vectorp (plist-get started :task-keys)))
              (should (= (length (plist-get started :task-keys)) 7))
              (setq daily-file (expand-file-name "daily/2099-01-02.org" repo))
              (let ((text (with-temp-buffer
                            (insert-file-contents daily-file)
                            (buffer-string))))
                (should (= (let ((start 0) (count 0))
                             (while (string-match "^:E_BOARD:" text start)
                               (setq count (1+ count) start (match-end 0)))
                             count)
                           1))
                (should-not (string-match-p "^:E_\\(?:UPDATE_\\)?SESSION:"
                                            text)))
              (let* ((board-id (plist-get started :board-id))
                     (run-id (plist-get started :run-id))
                     (binding
                      (e-work-with-batch-await
                        (e-work-await-batch
                         (e-chat-service-open-board-owner-start board-id harness)
                         :timeout 5.0)))
                     (target (e-chat-service-publication-target binding))
                     (projection
                      (e-work-with-batch-await
                        (e-work-await-batch
                         (e-board-orchestration-actions-run-projection
                          target run-id)
                         :timeout 5.0))))
                (should (equal (plist-get projection :run-id) run-id))
                (should (= (length (plist-get projection :tasks)) 7))
                (should
                 (cl-every
                  (lambda (task)
                    (memq (plist-get task :state) '(pending queued running)))
                  (plist-get projection :tasks)))
                (let* ((queue-id
                        (grimoire-daily-run--queue-id board-id run-id))
                       (queue
                        (plist-get
                         (gethash queue-id grimoire-daily-run--task-queues)
                         :queue)))
                  (should (e-task-queue-p queue))
                  (should
                   (eq (e-task-queue-storage queue)
                       (e-runtime-sqlite-task-storage
                        (e-default-runtime))))))))
          (dolist (command
                   (list (intern (concat "e-" "subagents-list-buffer"))
                         (intern (concat "e-board-" "runs-list-buffer"))))
            (should-not (commandp command))))
      (when harness
        (e-current-config-e2e-test--retire-private-harness harness)))))

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
                 (total (ert-stats-total stats))
                 (unexpected (ert-stats-completed-unexpected stats))
                 (exit (if (and (> total 0) (zerop unexpected)) 0 1)))
            (e-current-config-e2e-test--print
             "Current-config E2E complete: %d tests, %d unexpected.\n"
             total unexpected)
            (seq-doseq (test (ert--stats-tests stats))
              (let ((result (ert-test-most-recent-result test)))
                (when (ert-test-failed-p result)
                  (e-current-config-e2e-test--print
                   "FAILED %S: %S\n"
                   (ert-test-name test)
                   (ert-test-failed-condition result)))))
            (when (zerop total)
              (e-current-config-e2e-test--print
               "Current-config E2E failed: selector matched zero tests.\n"))
            (with-temp-file path
              (insert-buffer-substring e-current-config-e2e-test--output))
            exit))
      (kill-buffer e-current-config-e2e-test--output))))

(provide 'e-current-config-e2e-test)

;;; e-current-config-e2e-test.el ends here
