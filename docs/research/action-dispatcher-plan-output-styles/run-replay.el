;;; run-replay.el --- Generate action-dispatcher plan comparison artifacts -*- lexical-binding: t; no-byte-compile: t; -*-

;; Loaded into the running Emacs with emacsclient.  This avoids interactive
;; run_elisp blocking-load limits while using the already configured live E
;; backend.  It is a one-off replay driver that resolves live harness/session
;; symbols at load time in a configured Emacs, so it is excluded from the
;; byte-compiled fileset (`no-byte-compile' above) rather than carrying
;; declare-function stubs for code it never ships with.

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defvar e-action-dispatcher-plan-output-styles-results nil)

(defun e-action-dispatcher-plan-output-styles--json-write (path value)
  (with-temp-file path
    (insert (json-encode value))))

(defun e-action-dispatcher-plan-output-styles-run-one (variant style)
  "Run one output STYLE comparison VARIANT and return metrics plist."
  (let* ((root (file-truename "/Users/dimitrivorona/projects/elisp/e/"))
         (out-root (expand-file-name "docs/research/action-dispatcher-plan-output-styles/" root))
         (variant-dir (expand-file-name (symbol-name variant) out-root))
         (artifact (expand-file-name "plan.org" variant-dir))
         (context-file (expand-file-name "sources/codex-replay-context.org" out-root))
         (context (with-temp-buffer
                    (insert-file-contents context-file)
                    (buffer-string)))
         (prompt (format "Generate docs/feats/43-action-dispatcher/plan.org as Codex likely would have generated it at the replay cut point, but write it only to `%s`.

Do not edit `docs/feats/43-action-dispatcher/plan.org`.
Do not edit `docs/feats/index.org`.
Do not include this instruction text in the plan.

Use this saved Codex replay context. The original control plan is intentionally withheld from this prompt.

%s" artifact context))
         (original-config (copy-tree e-capability-config))
         (chat (e-harness-registry-get :chat-default))
         (backend (and chat (e-harness-backend chat)))
         (default-options (and chat (copy-tree (e-harness-default-options chat))))
         (context-strategy (and chat (e-harness-context-strategy chat)))
         (base-layer-ids (and chat (copy-sequence (e-harness-enabled-layer-ids chat))))
         (guidance (e-output-style--resolve style))
         (started (current-time))
         session-id harness preflight result error ended metrics session)
    (unless backend
      (error "No live :chat-default harness backend available"))
    (make-directory variant-dir t)
    (with-temp-file (expand-file-name "prompt.txt" variant-dir)
      (insert prompt))
    (with-temp-file (expand-file-name "guidance.txt" variant-dir)
      (insert guidance))
    (unwind-protect
        (progn
          (setq e-capability-config `((output-style :style ,style)))
          (setq harness
                (e-harness-create
                 :backend backend
                 :context-strategy context-strategy
                 :default-options default-options
                 :sessions (e-default-session-store)
                 :enabled-layer-ids base-layer-ids
                 :intrinsic-capabilities (e-default-chat--chat-session-capabilities)
                 :project-root root))
          (e-default-chat-sync-harness-layers harness base-layer-ids root)
          (setq preflight
                (list :global-config e-capability-config
                      :effective-output-style-instructions
                      (cl-loop for cap in (e-harness-active-capabilities harness)
                               when (eq (e-capability-id cap) 'output-style)
                               collect (e-capability-instructions cap))
                      :expected-guidance guidance))
          (plist-put preflight :matches-expected
                     (equal (plist-get preflight :effective-output-style-instructions)
                            (list guidance)))
          (with-temp-file (expand-file-name "effective-guidance.txt" variant-dir)
            (prin1 preflight (current-buffer)))
          (unless (plist-get preflight :matches-expected)
            (error "Output style preflight failed for %S: %S" variant preflight))
          (setq session-id
                (format "action-dispatcher-plan-%s-%s"
                        (symbol-name variant)
                        (format-time-string "%Y%m%dT%H%M%S")))
          (e-harness-create-session
           harness
           :id session-id
           :metadata (list :project-root root
                           :name (format "Action dispatcher plan %s"
                                         (symbol-name variant))))
          (condition-case err
              (setq result (e-harness-prompt harness session-id prompt))
            (error
             (setq error (list (symbol-name (car err))
                               (error-message-string err)))))
          (setq ended (current-time))
          (setq session (e-session-get (e-harness-sessions harness) session-id))
          (setq metrics
                (list :variant (symbol-name variant)
                      :style (symbol-name style)
                      :guidance guidance
                      :preflight preflight
                      :session-id session-id
                      :artifact artifact
                      :artifact-exists (file-exists-p artifact)
                      :started-at (format-time-string "%FT%TZ" started t)
                      :ended-at (format-time-string "%FT%TZ" ended t)
                      :duration-seconds (float-time (time-subtract ended started))
                      :message-count (length (plist-get session :messages))
                      :error error
                      :result (and result (format "%S" result))))
          (e-action-dispatcher-plan-output-styles--json-write
           (expand-file-name "metrics.json" variant-dir)
           metrics)
          (when error
            (error "Run failed for %S: %S" variant error))
          metrics)
      (setq e-capability-config original-config))))

(defun e-action-dispatcher-plan-output-styles-run-all ()
  "Run all comparison styles and write runs.json."
  (let* ((root (file-truename "/Users/dimitrivorona/projects/elisp/e/"))
         (out-root (expand-file-name "docs/research/action-dispatcher-plan-output-styles/" root))
         (styles '((hemingway-current . hemingway)
                   (hemingway-suggested-v2 . hemingway-suggested-v2)
                   (hemingway-v3 . hemingway-v3)
                   (caveman . caveman)))
         (results nil))
    (dolist (entry styles)
      (push (e-action-dispatcher-plan-output-styles-run-one (car entry) (cdr entry))
            results))
    (setq results (nreverse results))
    (setq e-action-dispatcher-plan-output-styles-results results)
    (e-action-dispatcher-plan-output-styles--json-write
     (expand-file-name "runs.json" out-root)
     (list :generated-at (format-time-string "%FT%TZ" nil t)
           :source-session "019f0f94-aafb-7533-9051-952b4d411baa"
           :config-method "serialized global e-capability-config binding, verified before each turn"
           :results results))
    results))

(defun e-action-dispatcher-plan-output-styles-run-one-name (name)
  "Run comparison variant NAME as a string."
  (pcase name
    ("hemingway-current"
     (e-action-dispatcher-plan-output-styles-run-one 'hemingway-current 'hemingway))
    ("hemingway-suggested-v2"
     (e-action-dispatcher-plan-output-styles-run-one 'hemingway-suggested-v2 'hemingway-suggested-v2))
    ("hemingway-v3"
     (e-action-dispatcher-plan-output-styles-run-one 'hemingway-v3 'hemingway-v3))
    ("caveman"
     (e-action-dispatcher-plan-output-styles-run-one 'caveman 'caveman))
    (_ (error "Unknown variant %S" name))))

(provide 'run-replay)
;;; run-replay.el ends here
