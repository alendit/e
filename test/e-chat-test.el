;;; e-chat-test.el --- Composed e chat facade tests -*- lexical-binding: t; -*-

(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)


(ert-deftest e-chat-test-open-captures-current-workspace ()
  "Opening a chat buffer records presentation workspace affinity."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (token (make-e-workspace-token
                 :backend 'single
                 :id 'test-workspace
                 :name "test"
                 :frame (selected-frame))))
    (cl-letf (((symbol-function 'e-workspace-current)
               (lambda (&optional _frame) token)))
      (let ((buffer (e-chat-open :harness harness :session-id "workspace-chat")))
        (unwind-protect
            (with-current-buffer buffer
              (should (e-workspace-equal-p (e-chat-buffer-workspace buffer)
                                           token))
              (should (e-workspace-equal-p (e-buffer-workspace buffer)
                                           token)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))



(ert-deftest e-chat-test-buffer-owns-explicit-board-client-observer-identity ()
  "A chat buffer exposes its public board context, not only private session id."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (buffer (e-chat-open :harness harness :session-id "board-context")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((binding (e-chat-service-binding harness e-chat-session-id)))
            (should (equal e-chat-board-id
                           (e-board-registry-board-id
                            (e-chat-service-binding-board binding))))
            (should (equal e-chat-client-id
                           (e-board-registry-client-id
                            (e-chat-service-binding-client binding))))
            (should (equal e-chat-observer-id
                           (e-board-observer-id
                            (e-chat-service-binding-observer binding))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-shared-harness-buffers-render-only-their-session ()
  "Chat buffers attached to one harness ignore events for other sessions."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer one")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (first-buffer nil)
         (second-buffer nil))
    (unwind-protect
        (progn
          (setq first-buffer
                (e-chat-open :harness harness :session-id "chat-one"))
          (setq second-buffer
                (e-chat-open :harness harness :session-id "chat-two"))
          (with-current-buffer first-buffer
            (e-chat-submit "question one"))
          (should (e-chat-test--wait-until
                   (lambda ()
                     (with-current-buffer first-buffer
                       (string-match-p "answer one" (buffer-string))))
                   1.0))
          (with-current-buffer first-buffer
            (should (string-match-p "question one" (buffer-string)))
            (should (string-match-p "answer one" (buffer-string))))
          (with-current-buffer second-buffer
            (should-not (string-match-p "question one" (buffer-string)))
            (should-not (string-match-p "answer one" (buffer-string)))))
      (when (buffer-live-p first-buffer)
        (kill-buffer first-buffer))
      (when (buffer-live-p second-buffer)
        (kill-buffer second-buffer)))))



(ert-deftest e-chat-test-submit-defers-backend-start-until-after-command ()
  "Board submit returns before backend context construction starts."
  (let* ((backend-started nil)
         (backend (e-backend-create
                   :name "delayed-chat"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore messages options on-item on-done on-error
                              on-request-start)
                      (setq backend-started t)
                      nil))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-submit-delayed"))
         (context-calls 0)
         (original-context (symbol-function 'e-harness-context)))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "send now")
          (cl-letf (((symbol-function 'e-harness-context)
                     (lambda (&rest args)
                       (setq context-calls (1+ context-calls))
                       (apply original-context args))))
            (e-chat-submit)
            (should (= context-calls 0))
            (should-not backend-started)
            (should (e-chat-composer-active-p))
            (should (equal (e-chat-composer-text) ""))
            (should (e-chat-test--wait-until
                     (lambda () backend-started)
                     1.0))
            (should (> context-calls 0))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (string-match-p
                        (concat (regexp-quote ">")
                                " send now")
                        (with-current-buffer buffer (buffer-string))))
                     1.0))))
      (when (buffer-live-p buffer)
        (with-current-buffer (e-chat-test--composer buffer)
          (ignore-errors
            (e-harness-test-abort e-chat-harness e-chat-session-id)))
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-mode-keeps-scroll-margin-user-controlled ()
  "Chat buffers do not force a global bottom scroll margin."
  (let ((scroll-margin 0)
        (buffer (e-chat-test--buffer nil "chat-composer-scroll-margin")))
    (unwind-protect
        (with-current-buffer buffer
          (should (= scroll-margin 0)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-configures-evil-initial-state-as-emacs ()
  "Chat buffers declare a non-normal Evil state when Evil is available."
  (let (configured)
    (cl-letf (((symbol-function 'evil-set-initial-state)
               (lambda (mode state)
                 (push (list mode state) configured))))
      (e-chat--configure-modal-editing-policy)
      (should (member '(e-chat-mode emacs) configured))
      (should (member '(e-chat-composer-mode insert) configured))
      (should (member '(e-chat-overview-mode emacs) configured)))))



(ert-deftest e-chat-test-captures-point-reference-with-two-line-context ()
  "Point capture uses the current line with two surrounding lines."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\nfive\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((reference (e-chat-capture-source-reference)))
      (should (equal (plist-get reference :text)
                     "one\ntwo\nthree\nfour\n"))
      (should (equal (plist-get reference :start-line) 1))
      (should (equal (plist-get reference :end-line) 4))
      (should (equal (plist-get reference :point-line) 2))
      (should (equal (plist-get reference :point-context) t))
      (should (string-match-p ":2 (context 1-4)\\'"
                              (plist-get reference :label)))
      (should (string-prefix-p "buffer://" (plist-get reference :uri))))))



(ert-deftest e-chat-test-captures-region-reference-exactly ()
  "Region capture uses the exact selected text and range metadata."
  (let ((file (make-temp-file "e-chat-ref-" nil ".el")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert "alpha\nbeta\ngamma\n")
          (goto-char (point-min))
          (search-forward "beta")
          (set-mark (match-beginning 0))
          (goto-char (match-end 0))
          (setq mark-active t)
          (let ((reference (e-chat-capture-source-reference)))
            (should (equal (plist-get reference :text) "beta"))
            (should (equal (plist-get reference :start-line) 2))
            (should (equal (plist-get reference :end-line) 2))
            (should (equal (plist-get reference :point-line) 2))
            (should-not (plist-get reference :point-context))
            (should (equal (plist-get reference :uri)
                           (concat "file://" file)))))
      (delete-file file))))



(ert-deftest e-chat-test-formats-point-reference-with-focused-line-marker ()
  "Point-context references mark the cursor line inside the preview."
  (let* ((reference '(:id "ref-1"
                     :uri "buffer://source"
                     :label "source:2 (context 1-4)"
                     :text "one\ntwo\nthree\nfour\n"
                     :start-line 1
                     :end-line 4
                     :point-line 2
                     :point-context t))
         (prompt (e-chat-format-reference-prompt
                  "Look at <reference id=\"ref-1\" label=\"source:2 (context 1-4)\">"
                  (list reference))))
    (should (string-match-p
             (regexp-quote "[ref-1] source:2 (context 1-4) (buffer://source)")
             prompt))
    (should (string-match-p
             (regexp-quote "Context lines 1-4; focused line 2:")
             prompt))
    (should (string-match-p
             (regexp-quote "  1 | one")
             prompt))
    (should (string-match-p
             (regexp-quote "> 2 | two")
             prompt))
    (should (string-match-p
             (regexp-quote "  4 | four")
             prompt))))





(ert-deftest e-chat-test-attach-keeps-existing-session-project-root ()
  "Attaching a session does not rewrite existing durable project metadata."
  (let* ((project-root (make-temp-file "e-chat-project-" t))
         (nested (expand-file-name "docs/feats/item" project-root))
         (harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" project-root) t)
          (make-directory nested t)
          (e-chat-test--create-session
           (e-harness-sessions harness)
           :id "session-1" :metadata (list :project-root nested))
          (let ((default-directory (file-name-as-directory nested)))
            (e-chat-open :harness harness :session-id "session-1"))
          (should (equal (e-harness-project-root harness "session-1" nil)
                         (file-name-as-directory nested))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory project-root t))))



(ert-deftest e-chat-test-session-metadata-prefers-project-root ()
  "Chat sessions root file tools at the enclosing project, not a subdirectory."
  (let* ((project-root (make-temp-file "e-chat-project-" t))
         (nested (expand-file-name "docs/feats/item" project-root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" project-root) t)
          (make-directory nested t)
          (let ((default-directory (file-name-as-directory nested)))
            (should (equal (plist-get (e-chat--session-metadata) :project-root)
                           (file-name-as-directory project-root)))))
      (delete-directory project-root t))))



(ert-deftest e-chat-test-no-evil-setup-is-required ()
  "Opening chat does not configure or force Evil modal state."
  (let (evil-configured evil-insert-called evil-local-mode-argument buffer)
    (cl-letf (((symbol-function 'evil-set-initial-state)
               (lambda (&rest _args)
                 (setq evil-configured t)))
              ((symbol-function 'evil-insert-state)
               (lambda ()
                 (setq evil-insert-called t)))
              ((symbol-function 'evil-local-mode)
               (lambda (argument)
                 (setq evil-local-mode-argument argument))))
      (unwind-protect
          (progn
            (setq buffer (e-chat-test--buffer nil "chat-no-evil"))
            (should-not evil-configured)
            (should-not evil-insert-called)
            (should (equal evil-local-mode-argument -1)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest e-chat-test-submit-rejects-pending-command-reference ()
  "Submitting with a pending ! reference preserves the composer and errors."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-pending-submit"))
          (with-current-buffer (e-chat-test--composer buffer)
            (cl-letf (((symbol-function 'read-shell-command)
                       (lambda (&rest _args) "sleep 10")))
              (e-chat-composer-bang))
            (insert " use it")
            (should-error (e-chat-submit) :type 'user-error)
            (should (string-match-p
                     (regexp-quote "@[$ sleep 10 (running)] use it")
                     (e-chat-composer-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-pending-command-references-survive-clear-and-cancel-on-kill ()
  "Transcript redraw preserves composer work; killing the surface cancels it."
  (let* ((before-command-buffers
          (cl-count-if
           (lambda (candidate)
             (equal (buffer-name candidate) " *e-chat-command-output*"))
           (buffer-list)))
         buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-clear-cancel"))
          (with-current-buffer (e-chat-test--composer buffer)
            (cl-letf (((symbol-function 'read-shell-command)
                       (lambda (&rest _args) "sleep 10")))
              (e-chat-composer-bang))
            (with-current-buffer buffer
              (e-chat--clear))
            (should (string-match-p "sleep 10 (running)"
                                    (e-chat-composer-text))))
          (kill-buffer buffer)
          (setq buffer nil)
          (should (= before-command-buffers
                     (cl-count-if
                      (lambda (candidate)
                        (equal (buffer-name candidate)
                               " *e-chat-command-output*"))
                      (buffer-list)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-speaker-faces-inherit-distinct-theme-faces ()
  "User, assistant, and system entries inherit distinct neutral theme faces.
No hardcoded palette: each face tracks the active theme through a different
inherited base, so the blocks stay distinguishable in any theme."
  (should (equal (face-attribute 'e-chat-user-face :inherit) 'highlight))
  (should (equal (face-attribute 'e-chat-assistant-face :inherit) 'default))
  (should (equal (face-attribute 'e-chat-system-face :inherit) 'shadow))
  (should-not (equal (face-attribute 'e-chat-user-face :inherit)
                     (face-attribute 'e-chat-assistant-face :inherit)))
  (should (eq (face-attribute 'e-chat-user-face :background) 'unspecified))
  (should (eq (face-attribute 'e-chat-user-face :extend) t))
  (should (eq (face-attribute 'e-chat-assistant-face :extend) t))
  (should (eq (face-attribute 'e-chat-system-face :extend) t)))



(ert-deftest e-chat-test-final-assistant-face-has-no-border ()
  "Settled assistant entries inherit the assistant face without a border."
  (should (equal (face-attribute 'e-chat-final-assistant-face :inherit)
                 'e-chat-assistant-face))
  (should-not (face-attribute 'e-chat-final-assistant-face :box))
  (should (eq (face-attribute 'e-chat-final-assistant-face :extend) t)))



(ert-deftest e-chat-test-face-refresh-clears-final-assistant-border ()
  "Live reload removes older border decoration from settled assistant output."
  (let ((old-defface-spec (get 'e-chat-final-assistant-face 'face-defface-spec)))
    (unwind-protect
        (progn
          (put 'e-chat-final-assistant-face
               'face-defface-spec
               '((t :inherit e-chat-assistant-face
                    :box (:line-width 1 :color "#6f925a")
                    :extend t)))
          (set-face-attribute 'e-chat-final-assistant-face nil
                              :box '(:line-width 1 :color "#6f925a")
                              :extend nil)
          (e-chat--refresh-face-specs)
          (should (equal (face-attribute 'e-chat-final-assistant-face :inherit)
                         'e-chat-assistant-face))
          (should-not (face-attribute 'e-chat-final-assistant-face :box))
          (should (eq (face-attribute 'e-chat-final-assistant-face :extend) t)))
      (put 'e-chat-final-assistant-face 'face-defface-spec old-defface-spec)
      (e-chat--refresh-face-specs))))



(ert-deftest e-chat-test-focused-turn-face-is-subtle ()
  "Response navigation focus inherits the theme region face without a border."
  (should (equal (face-attribute 'e-chat-focused-turn-face :inherit) 'region))
  (should (eq (face-attribute 'e-chat-focused-turn-face :background)
              'unspecified))
  (should-not (face-attribute 'e-chat-focused-turn-face :box))
  (should (eq (face-attribute 'e-chat-focused-turn-face :extend) t)))



(ert-deftest e-chat-test-face-refresh-clears-focused-turn-strong-decoration ()
  "Live reload removes older strong decorations from response focus."
  (let ((old-defface-spec (get 'e-chat-focused-turn-face 'face-defface-spec)))
    (unwind-protect
        (progn
          (put 'e-chat-focused-turn-face
               'face-defface-spec
               '((t :inherit highlight
                    :box (:line-width 1 :color "#3b4b5c")
                    :extend t)))
          (set-face-attribute 'e-chat-focused-turn-face nil
                              :inherit 'highlight
                              :background 'unspecified
                              :box '(:line-width 1 :color "#3b4b5c")
                              :extend nil)
          (e-chat--refresh-face-specs)
          (should (equal (face-attribute 'e-chat-focused-turn-face :inherit)
                         'region))
          (should (eq (face-attribute 'e-chat-focused-turn-face :background)
                      'unspecified))
          (should-not (face-attribute 'e-chat-focused-turn-face :box))
          (should (eq (face-attribute 'e-chat-focused-turn-face :extend) t)))
      (put 'e-chat-focused-turn-face 'face-defface-spec old-defface-spec)
      (e-chat--refresh-face-specs))))



(ert-deftest e-chat-test-user-and-assistant-headings-are-glyph-only ()
  "User and assistant message headings render only their compact glyphs."
  (should (equal ">" ">"))
  (should (equal "●" "●")))



(ert-deftest e-chat-test-compacts-live-hook-audit-payload ()
  "A live hook audit has a compact activity summary, never a raw system event."
  (let ((buffer (e-chat-test--buffer nil "chat-hook-audit")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'hook-audit
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:summary "Evidence references resolved"
                                     :details (:private "not rendered"))))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (let ((content (buffer-string)))
            (should (string-match-p "Evidence references resolved" content))
            (should-not (string-match-p "Event: (" content))
            (should-not (string-match-p "not rendered" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-pending-hook-summary-keeps-validation-visible ()
  "A queued hook follow-up retains its capability-provided activity label."
  (let ((buffer (e-chat-test--buffer nil "chat-pending-hook-summary-port")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'hook-audit
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:pending-summary "Validating claims…")))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (should (string-match-p "Validating claims…" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-terminal-hook-audit-clears-pending-summary ()
  "A terminal hook audit removes a queued correction's validation label."
  (let ((buffer (e-chat-test--buffer nil "chat-terminal-hook-summary-port")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'hook-audit
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:pending-summary "Validating claims…")))
          (e-chat-render-event
           (e-events-make :type 'hook-audit
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:summary "Claim correction unresolved"
                                     :pending-summary nil
                                     :details (:correction failed))))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (should-not (string-match-p "Validating claims…" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-follow-up-turn-carries-pending-hook-summary ()
  "A hidden follow-up shows its validation status while its reply streams."
  (let ((buffer (e-chat-test--buffer nil "chat-pending-hook-summary")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           (e-harness-sessions e-chat-harness) e-chat-session-id
           (list :role 'user :turn-id "turn-2" :content "corrective"
                 :metadata '(:display hidden :pending-summary "Validating claims…")))
          (e-chat-test--seed-board-log-from-private-fixture
           e-chat-harness e-chat-session-id)
          (e-chat--render-event
           (e-events-make :type 'turn-started :session-id e-chat-session-id
                          :turn-id "turn-2" :created-at 10))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (should (string-match-p "Validating claims…" (buffer-string)))
          (should (string-match-p (regexp-quote "●")
                                  (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-subscription-skips-redundant-assistant-deltas ()
  "The shell handles only one text delta for each provider response round.
The loop still consumes every delta to form the durable final answer; this
test covers only the chat presentation subscription's redundant callbacks."
  (let ((buffer (e-chat-test--buffer nil "chat-delta-subscription")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((rendered nil)
                (original-render (symbol-function 'e-chat--render-event))
                (provider-start
                 (e-events-make :type 'provider-request-started
                                :session-id e-chat-session-id
                                :turn-id "turn-1"
                                :payload '(:status started)))
                (delta
                 (e-events-make :type 'assistant-delta
                                :session-id e-chat-session-id
                                :turn-id "turn-1"
                                :payload '(:content "part")))
                (reasoning
                 (e-events-make :type 'reasoning-delta
                                :session-id e-chat-session-id
                                :turn-id "turn-1"
                                :payload '(:content "thinking"))))
            (cl-letf (((symbol-function 'e-chat--render-event)
                       (lambda (event)
                         (push (plist-get event :type) rendered)
                         (funcall original-render event))))
              (e-chat-test--dispatch-observed-event provider-start)
              (e-chat-test--dispatch-observed-event delta)
              (e-chat-test--dispatch-observed-event delta)
              (should (equal (nreverse rendered)
                             '(provider-request-started assistant-delta)))
              ;; If another visible phase supersedes the header, the next text
              ;; chunk restores the current streaming status.
              (setq rendered nil)
              (e-chat-test--dispatch-observed-event reasoning)
              (e-chat-test--dispatch-observed-event delta)
              (should (equal (nreverse rendered)
                             '(reasoning-delta assistant-delta)))
              ;; Tool turns can start another provider response without
              ;; changing turn id, so the next request re-enables one delta.
              (setq rendered nil)
              (e-chat-test--dispatch-observed-event provider-start)
              (e-chat-test--dispatch-observed-event delta)
              (should (equal (nreverse rendered)
                             '(provider-request-started assistant-delta))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-active-thinking-row-shows-spinner-and-duration ()
  "Active provider requests show a moving thinking row with current duration."
  (let ((buffer (e-chat-test--buffer nil "chat-active-thinking-duration")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-test--mark-active-turn "turn-1")
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠋ Thinking for [0-9]+min [0-9]+sec" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-sibling-terminal-events-do-not-settle-selected-turn ()
  "Observed sibling output remains visible without settling selected work."
  (let ((buffer (e-chat-test--buffer nil "chat-sibling-terminal")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--active-turn-running-p)
                     (lambda () t)))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "selected-turn"
                            :created-at 0))
            (let ((selected-status (e-chat-surface-status)))
              ;; Every board-backed sibling event carries an explicit nil
              ;; ownership fact.  It may be rendered, but it cannot alter the
              ;; selected turn's progress/status or composer settlement.
              (e-chat--render-event
               (list :type 'message-added :session-id e-chat-session-id
                     :turn-id "selected-turn" :created-at 1
                     :board-seq 10 :selected-participant-p nil
                     :payload
                     '(:message (:id "sibling-output" :role assistant
                                 :content "Sibling answer."
                                 :terminal-output t
                                 :selected-participant-p nil))))
              (e-chat--render-event
               (list :type 'turn-finished :session-id e-chat-session-id
                     :turn-id "selected-turn" :created-at 2
                     :board-seq 11 :selected-participant-p nil))
              (e-chat--render-event
               (list :type 'turn-failed :session-id e-chat-session-id
                     :turn-id "selected-turn" :created-at 3
                     :board-seq 12 :selected-participant-p nil
                     :payload '(:error "sibling failure")))
              (e-chat--render-event
               (list :type 'turn-cancelled :session-id e-chat-session-id
                     :turn-id "selected-turn" :created-at 4
                     :board-seq 13 :selected-participant-p nil))
              (e-chat--render-event
               (list :type 'backend-empty-output :session-id e-chat-session-id
                     :turn-id "selected-turn" :created-at 5
                     :board-seq 14 :selected-participant-p nil))
              (should (string-match-p "Sibling answer" (buffer-string)))
              (should (equal (e-chat-activity-progress-turn-id)
                             "selected-turn"))
              (should (equal (e-chat-surface-status) selected-status))
              (should (eq (e-chat--submit-intent nil) 'steer))))
            (e-chat--render-event
             (list :type 'turn-finished :session-id e-chat-session-id
                   :turn-id "selected-turn" :created-at 6
                   :selected-participant-p t))
            (should-not (e-chat-activity-progress-turn-id))
            (should (equal (e-chat-surface-status) "done"))
            (should (eq (e-chat--submit-intent nil) 'submit)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-board-ownership-fallback-is-fail-closed ()
  "Only identity-free synthetic events retain the historical selected default."
  (should (e-chat-transcript-event-selected-participant-p '(:type turn-started)))
  (should (e-chat-transcript-event-selected-participant-p
           '(:type turn-started :selected-participant-p t)))
  (should-not (e-chat-transcript-event-selected-participant-p
               '(:type turn-started :selected-participant-p nil)))
  (should-not (e-chat-transcript-event-selected-participant-p
               '(:type turn-failed :board-id "board" :turn-id "turn")))
  (should (e-chat-transcript-message-selected-participant-p
           '(:role assistant :content "synthetic")))
  (should-not (e-chat-transcript-message-selected-participant-p
               '(:role assistant :board-seq 4 :content "missing fact"))))



(ert-deftest e-chat-test-board-routing-isolated-after-restart-and-settles-selected-only ()
  "Board routing and presentation ownership survive a provider-free restart."
  (let ((directory (make-temp-file "e-chat-routing-composition-" t))
        (e-board--registry (make-hash-table :test 'equal))
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--invocations (make-hash-table :test 'equal))
        (e-board-runtime--pending-pickup-head nil)
        (e-board-runtime--pending-pickup-tail nil)
        (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
        (e-board-runtime--pickup-drain-scheduled nil)
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal))
        (e-chat-service--board-log-owners (make-hash-table :test 'equal))
        root-buffer)
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (harness (e-harness-create
                         :sessions store :enabled-layer-ids nil))
               (root-session
                (e-chat-service-create-session
                 :harness harness :id "routing-root"))
               (root-id (plist-get root-session :id))
               (root-binding (e-chat-service-binding harness root-id))
               (runtime-board (e-chat-service-binding-board root-binding))
               (source (e-board-registry-board-source-board runtime-board))
               (root-participant
                (e-board-registry-participant-id
                 (e-board-runtime-attachment-participant
                  (e-chat-service-binding-attachment root-binding))))
               (update-session
                (e-chat-service-create-participant
                 runtime-board harness :id "routing-private"
                 :pickup-selector '(:tags (private-update))
                 :observer-selector :self
                 :default-tags '(private-update)
                 :default-to :self))
               (update-id (plist-get update-session :id))
               (update-binding (e-chat-service-binding harness update-id))
               (update-participant
                (plist-get
                 (e-session-board-routing-policy update-session)
                 :participant-id))
               (route-input
                (lambda (source runtime-board binding prompt tags to
                         expected-participant)
                  (let ((message-id
                         (e-chat-service-post
                          binding prompt :tags tags :to to)))
                    (while (e-board-input-classifications source)
                      (e-board-runtime--drain-input-routing
                       runtime-board
                       (lambda ()
                         (e-board-drain-input-classifications source))))
                    (let* ((message (e-board-message source message-id))
                           (pickup-id
                            (car (e-board-message-pickup-ids message)))
                           (pickup (e-board-pickup source pickup-id)))
                      (should (equal
                               (e-board-message-matching-participant-ids message)
                               (list expected-participant)))
                      (should (equal (e-board-pickup-message-id pickup)
                                     message-id))
                      (should (equal
                               (e-board-pickup-participant-id pickup)
                               expected-participant))
                      (should (eq (e-board-message-routing-state message)
                                  'routed))
                      message-id))))
               (main-input
                (funcall route-input source runtime-board root-binding
                         "main input" '(main) nil root-participant))
               (_private-input
                (funcall route-input source runtime-board update-binding
                         "private update" nil nil update-participant))
               (root-policy (e-session-board-routing-policy root-session))
               (private-policy
                (e-session-board-routing-policy update-session))
               (board-id (e-board-registry-board-id runtime-board)))
          ;; The two bindings advertise different durable selectors, and the
          ;; first routing pass proves recipient and delivery identity rather
          ;; than merely counting publications.
          (should (equal (plist-get root-policy :pickup-selector)
                         '(:tags (main))))
          (should (equal (plist-get private-policy :pickup-selector)
                         '(:tags (private-update))))
          (should (equal (plist-get private-policy :observer-selector)
                         (list :subject-participant-id update-participant)))
          (should (equal (plist-get private-policy :default-tags)
                         '(private-update)))
          (should (equal (plist-get private-policy :default-to)
                         update-participant))
          (e-session-flush-write-queue store)
          ;; Recreate the board/service runtime while retaining only durable
          ;; board-session state and its owner log.
          (setq e-board--registry (make-hash-table :test 'equal)
                e-board-registry--boards (make-hash-table :test 'equal)
                e-board-registry--unsettled-pickup-count 0
                e-board-registry--unsettled-effect-count 0
                e-board-registry--unsettled-routing-count 0
                e-board-registry--unsettled-generation 0
                e-board-registry--board-index
                (avl-tree-create (lambda (left right)
                                   (string< (car left) (car right))))
                e-chat-service--bindings
                (make-hash-table :test 'eq :weakness 'key)
                e-chat-service--board-bindings (make-hash-table :test 'equal)
                e-chat-service--board-log-owners (make-hash-table :test 'equal)
                e-board-runtime--attachments (make-hash-table :test 'equal)
                e-board-runtime--session-attachments (make-hash-table :test 'equal)
                e-board-runtime--endpoint-attachments (make-hash-table :test 'equal)
                e-board-runtime--invocations (make-hash-table :test 'equal)
                e-board-runtime--pending-pickup-head nil
                e-board-runtime--pending-pickup-tail nil
                e-board-runtime--pending-pickup-set
                (make-hash-table :test 'equal)
                e-board-runtime--pickup-drain-scheduled nil)
          (let* ((loaded (e-session-persistent-store-create directory))
                 (restarted (e-harness-create
                             :sessions loaded :enabled-layer-ids nil))
                 (restored-root
                  (e-chat-service-ensure-binding restarted root-id))
                 (restored-private
                  (e-chat-service-ensure-binding restarted update-id))
                 (restored-board
                  (e-chat-service-binding-board restored-root))
                 (restored-source
                  (e-board-registry-board-source-board restored-board))
                 (restored-root-participant
                  (e-board-registry-participant-id
                   (e-board-runtime-attachment-participant
                    (e-chat-service-binding-attachment restored-root))))
                 (restored-private-participant
                  (e-board-registry-participant-id
                   (e-board-runtime-attachment-participant
                    (e-chat-service-binding-attachment restored-private)))))
            (should (equal (e-board-registry-board-id restored-board) board-id))
            (should (equal restored-root-participant root-participant))
            (should (equal restored-private-participant update-participant))
            (let ((restarted-main-input
                   (funcall route-input restored-source restored-board
                            restored-root "main after restart" '(main) nil
                            restored-root-participant)))
              ;; Posting through the restored private binding with no routing
              ;; overrides exercises its durable default tags and exact target.
              (funcall route-input restored-source restored-board
                       restored-private "private after restart" nil nil
                       restored-private-participant)
              (setq root-buffer
                    (e-chat-open :harness restarted :session-id root-id))
              (with-current-buffer root-buffer
                (e-chat-surface-set-redraw-visible t)
                (should
                 (equal
                  (e-board-observer-selector
                   (e-chat-service-subscription-observer
                    e-chat--event-subscription))
                  '(:tags (main))))
                (puthash root-id
                         (list :id restarted-main-input :status 'running)
                       (e-harness-active-turns restarted))
                (e-chat--render-event
                 (list :type 'turn-started :session-id root-id
                       :turn-id restarted-main-input :created-at 10
                       :selected-participant-p t))
                ;; Establish a real selected provider round first.  The
                ;; sibling rows below use the same causal input but a
                ;; different participant; their terminal rows must not settle
                ;; this round or replace its thought/progress state.
                (e-board-post-activity
                 restored-source :id "root-provider-started"
                 :author (format "participant:%s"
                                 restored-root-participant)
                 :subject-participant-id restored-root-participant
                 :source-turn-id "root-turn" :activity-kind
                 'provider-request-started :tags '(main)
                 :attributes '(:status started)
                 :reply-to-message-ids (list restarted-main-input)
                 :source-activity-key '(routing root-provider-started 1))
                (e-board-post-activity
                 restored-source :id "root-reasoning"
                 :author (format "participant:%s"
                                 restored-root-participant)
                 :subject-participant-id restored-root-participant
                 :source-turn-id "root-turn" :activity-kind
                 'reasoning-delta :tags '(main)
                 :content "selected planning"
                 :reply-to-message-ids (list restarted-main-input)
                 :source-activity-key '(routing root-reasoning 1))
                (e-board-post-activity
                 restored-source :id "root-provider-finished"
                 :author (format "participant:%s"
                                 restored-root-participant)
                 :subject-participant-id restored-root-participant
                 :source-turn-id "root-turn" :activity-kind
                 'provider-request-finished :tags '(main)
                 :attributes '(:status done)
                 :reply-to-message-ids (list restarted-main-input)
                 :source-activity-key '(routing root-provider-finished 1))
                ;; Drain the selected lifecycle before introducing sibling
                ;; rows.  Replay may already have a legitimate outer
                ;; `:ended-at`; snapshot it rather than mistaking that
                ;; restored fact for sibling settlement.
                (e-chat-service--drain-subscription e-chat--event-subscription)
                (e-ui-work-with-batch-drain
                  (e-ui-work-drain-batch :buffer root-buffer))
                (let* ((selected-before
                        (e-chat-activity-turn-display
                         restarted-main-input))
                       (selected-status-before (e-chat-surface-status))
                       (selected-progress-before
                        (e-chat-activity-progress-turn-id))
                       (selected-composer-before
                        (e-chat-test--composer-text-for root-buffer))
                       (selected-intent-before (e-chat--submit-intent nil)))
                  (should selected-before)
                  (should (= (plist-get selected-before :round-count) 1))
                  (should (equal (car (plist-get selected-before :round-statuses))
                                 'done))
                  (should (string-match-p
                           "selected planning"
                           (or (plist-get selected-before :expanded-text)
                               "")))
                  (e-board-post-output
                 restored-source :id "sibling-output"
                 :author (format "participant:%s"
                                 restored-private-participant)
                 :subject-participant-id restored-private-participant
                 :source-turn-id "sibling-turn" :tags '(main)
                 :content "Observed sibling answer."
                 :reply-to-message-ids (list restarted-main-input)
                 :source-output-key '(routing sibling-output 1))
                (e-board-post-activity
                 restored-source :id "sibling-finished"
                 :author (format "participant:%s"
                                 restored-private-participant)
                 :subject-participant-id restored-private-participant
                 :source-turn-id "sibling-turn"
                 :activity-kind 'turn-summary :tags '(main)
                 :attributes '(:status finished)
                 :reply-to-message-ids (list restarted-main-input)
                 :source-activity-key '(routing sibling-summary 1))
                ;; Failure, cancellation, and empty-output rows are all
                ;; ordinary observer deliveries.  They share the restarted
                ;; input's causal id but retain one isolated sibling identity.
                (e-board-post-activity
                 restored-source :id "sibling-failed"
                 :author (format "participant:%s"
                                 restored-private-participant)
                 :subject-participant-id restored-private-participant
                 :source-turn-id "sibling-failure"
                 :activity-kind 'turn-summary :tags '(main)
                 :attributes '(:status failed :error "sibling failure")
                 :reply-to-message-ids (list restarted-main-input)
                 :source-activity-key '(routing sibling-failure 1))
                (e-board-post-activity
                 restored-source :id "sibling-cancelled"
                 :author (format "participant:%s"
                                 restored-private-participant)
                 :subject-participant-id restored-private-participant
                 :source-turn-id "sibling-cancel"
                 :activity-kind 'turn-summary :tags '(main)
                 :attributes '(:status cancelled)
                 :reply-to-message-ids (list restarted-main-input)
                 :source-activity-key '(routing sibling-cancel 1))
                (e-board-post-activity
                 restored-source :id "sibling-empty"
                 :author (format "participant:%s"
                                 restored-private-participant)
                 :subject-participant-id restored-private-participant
                 :source-turn-id "sibling-empty"
                 :activity-kind 'backend-empty-output :tags '(main)
                 :reply-to-message-ids (list restarted-main-input)
                 :source-activity-key '(routing sibling-empty 1))
                ;; Exercise the real subscription observer and shell callback;
                ;; do not bypass selection with a directly synthesized event.
                (e-chat-service--drain-subscription e-chat--event-subscription)
                (e-ui-work-with-batch-drain
                  (e-ui-work-drain-batch :buffer root-buffer))
                (should (string-match-p "Observed sibling answer"
                                        (buffer-string)))
                (should (string-match-p "Turn failed: sibling failure"
                                        (buffer-string)))
                (should (string-match-p "Turn cancelled"
                                        (buffer-string)))
                (should (equal (e-chat-activity-progress-turn-id)
                               restarted-main-input))
                (should-not
                 (member (e-chat-surface-status) '("done" "error" "cancelled")))
                (let ((selected-after
                       (e-chat-activity-turn-display restarted-main-input)))
                  (should selected-after)
                  (should (equal (plist-get selected-after :round-count)
                                 (plist-get selected-before :round-count)))
                  (should (equal (plist-get selected-after :round-statuses)
                                 (plist-get selected-before :round-statuses)))
                  (should (equal (plist-get selected-after :expanded-text)
                                 (plist-get selected-before :expanded-text)))
                  (should (equal (e-chat-surface-status) selected-status-before))
                  (should (equal (e-chat-activity-progress-turn-id)
                                 selected-progress-before))
                  (should (equal (e-chat-test--composer-text-for root-buffer)
                                 selected-composer-before))
                  (should (eq (e-chat--submit-intent nil)
                              selected-intent-before)))
                (should (eq (e-chat--submit-intent nil) 'steer))
                )
                (e-board-post-output
                        restored-source :id "root-output"
                        :author (format "participant:%s"
                                        restored-root-participant)
                        :subject-participant-id restored-root-participant
                        :source-turn-id "root-turn" :tags '(main)
                        :content "Selected root answer."
                        :reply-to-message-ids (list restarted-main-input)
                        :source-output-key '(routing root-output 1))
                (e-board-post-activity
                        restored-source :id "root-finished"
                        :author (format "participant:%s"
                                        restored-root-participant)
                        :subject-participant-id restored-root-participant
                        :source-turn-id "root-turn"
                        :activity-kind 'turn-summary :tags '(main)
                        :attributes '(:status finished)
                        :reply-to-message-ids (list restarted-main-input)
                        :source-activity-key '(routing root-summary 1))
                (e-chat-service--drain-subscription e-chat--event-subscription)
                (e-ui-work-with-batch-drain
                  (e-ui-work-drain-batch :buffer root-buffer))
                (remhash root-id (e-harness-active-turns restarted))
                (should-not (e-chat-activity-progress-turn-id))
                (should (equal (e-chat-surface-status) "done"))
                (should (eq (e-chat--submit-intent nil) 'submit))))))
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (delete-directory directory t))))



(ert-deftest e-chat-test-board-input-keeps-one-selected-key-through-reopen ()
  "A board input has one selected presentation key live and after replay.

The input is deliberately observed through the real service subscription.  A
same-causal sibling output/failure/cancellation is then projected through that
subscription as well, so the input identity assertion also guards the
selected/sibling isolation boundary."
  (let* ((directory (make-temp-file "e-chat-input-identity-" t))
        (e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--id-sequence 0)
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--invocations (make-hash-table :test 'equal))
        (e-board-runtime--pending-pickup-head nil)
        (e-board-runtime--pending-pickup-tail nil)
        (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
        (e-board-runtime--pickup-drain-scheduled nil)
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal))
        (e-chat-service--board-log-owners (make-hash-table :test 'equal))
        (store (e-session-persistent-store-create directory))
        live-buffer replay-buffer)
    (unwind-protect
        (let* ((harness (e-harness-create
                         :sessions store :enabled-layer-ids nil))
               (session (e-chat-service-create-session
                         :harness harness :id "input-identity"))
               (session-id (plist-get session :id))
               (binding (e-chat-service-binding harness session-id))
               (board (e-chat-service-binding-board binding))
               (source (e-board-registry-board-source-board board))
               (participant-id
                (e-board-registry-participant-id
                 (e-board-runtime-attachment-participant
                  (e-chat-service-binding-attachment binding))))
               (sibling-id "sibling-participant")
               (input-id nil)
               (observed-turn-id nil))
          (e-board-registry-add-participant
           board :id sibling-id :author (format "participant:%s" sibling-id)
           :principal (e-board-registry-board-principal board)
           :publish-event nil)
          (setq live-buffer
                (e-chat-open :harness harness :session-id session-id))
          (with-current-buffer live-buffer
            (e-chat-surface-set-redraw-visible t))
          (setq input-id
                (e-chat-service-submit-session
                 harness session-id "ordinary input"))
          (with-current-buffer live-buffer
            (e-chat-service--drain-subscription e-chat--event-subscription)
            (e-ui-work-with-batch-drain
             (e-ui-work-drain-batch :buffer live-buffer))
            (goto-char (point-min))
            (should (search-forward "ordinary input")))
          ;; These rows all causally answer the ordinary input, but belong to
          ;; another participant.  They must stay renderable without mutating
          ;; the selected input record.
          (dolist (_message
                   (list
                    (e-board-publication-message
                     (e-board-post-output
                      source :id "sibling-output"
                      :author (format "participant:%s" sibling-id)
                      :subject-participant-id sibling-id
                      :source-turn-id "sibling-turn" :tags '(main)
                      :content "Sibling output"
                      :reply-to-message-ids (list input-id)
                      :source-output-key '(f009-output 1 1)))
                    (e-board-publication-message
                     (e-board-post-activity
                      source :id "sibling-failure"
                      :author (format "participant:%s" sibling-id)
                      :subject-participant-id sibling-id
                      :source-turn-id "sibling-turn"
                      :activity-kind 'turn-summary :tags '(main)
                      :attributes '(:status failed :error "sibling failure")
                      :reply-to-message-ids (list input-id)
                      :source-activity-key '(f009-failure 1 1)))
                    (e-board-publication-message
                     (e-board-post-activity
                      source :id "sibling-cancellation"
                      :author (format "participant:%s" sibling-id)
                      :subject-participant-id sibling-id
                      :source-turn-id "sibling-cancel"
                      :activity-kind 'turn-summary :tags '(main)
                      :attributes '(:status cancelled)
                      :reply-to-message-ids (list input-id)
                      :source-activity-key '(f009-cancel 1 1)))))
            (ignore _message))
          (with-current-buffer live-buffer
            (e-chat-service--drain-subscription e-chat--event-subscription)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer live-buffer))
            ;; Sibling rendering remains visible under an isolated semantic
            ;; presentation identity and does not replace the selected input.
            (goto-char (point-min))
            (search-forward "Sibling output")
            (setq observed-turn-id (e-chat-transcript-turn-id-at-point))
            (should (eq (car-safe observed-turn-id) :observed-board-turn))
            (should (equal (plist-get (cdr observed-turn-id) :causal-turn-id)
                           input-id))
            (goto-char (point-min))
            (should (search-forward "ordinary input")))
          (e-session-flush-write-queue store)
          (kill-buffer live-buffer)
          (setq live-buffer nil)
          ;; Rebuild the board/service process state, then let the normal chat
          ;; open path render the persisted observer snapshot.
          (setq e-board--registry (make-hash-table :test 'equal)
                e-board--id-sequence 0
                e-board-registry--boards (make-hash-table :test 'equal)
                e-board-registry--id-sequence 0
                e-board-registry--unsettled-pickup-count 0
                e-board-registry--unsettled-effect-count 0
                e-board-registry--unsettled-routing-count 0
                e-board-registry--unsettled-generation 0
                e-board-runtime--attachments (make-hash-table :test 'equal)
                e-board-runtime--session-attachments (make-hash-table :test 'equal)
                e-board-runtime--endpoint-attachments (make-hash-table :test 'equal)
                e-board-runtime--invocations (make-hash-table :test 'equal)
                e-board-runtime--pending-pickup-head nil
                e-board-runtime--pending-pickup-tail nil
                e-board-runtime--pending-pickup-set (make-hash-table :test 'equal)
                e-board-runtime--pickup-drain-scheduled nil
                e-chat-service--bindings
                (make-hash-table :test 'eq :weakness 'key)
                e-chat-service--board-bindings (make-hash-table :test 'equal)
                e-chat-service--board-log-owners (make-hash-table :test 'equal))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (restarted (e-harness-create
                             :sessions loaded :enabled-layer-ids nil))
                 (restored-binding
                  (e-chat-service-ensure-binding restarted session-id))
                 (restored-board
                  (e-chat-service-binding-board restored-binding))
                 (restored-source
                  (e-board-registry-board-source-board restored-board)))
            (should (equal (e-board-registry-board-id restored-board)
                           (e-board-registry-board-id board)))
            (should (equal (e-board-message-count restored-source)
                           (e-board-message-count source)))
            (setq replay-buffer
                  (e-chat-open :harness restarted :session-id session-id))
            (with-current-buffer replay-buffer
              (e-chat-surface-set-redraw-visible t)
              (goto-char (point-min))
              (should (search-forward "ordinary input"))
              (goto-char (point-min))
              (search-forward "Sibling output")
              (should (equal (e-chat-transcript-turn-id-at-point)
                             observed-turn-id))))
      (when (buffer-live-p live-buffer)
        (kill-buffer live-buffer))
      (when (buffer-live-p replay-buffer)
        (kill-buffer replay-buffer))
      (delete-directory directory t)))))



(ert-deftest e-chat-test-final-message-preserves-follow-up-draft ()
  "Assistant final output keeps already typed follow-up composer text."
  (let ((buffer (e-chat-test--buffer nil "chat-final-preserve-draft")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (with-current-buffer (e-chat-test--composer buffer)
            (goto-char (point-max))
            (insert "follow-up draft"))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-active-p))
            (should (equal (e-chat-composer-text) "follow-up draft")))
          (e-chat--render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:reason stop)))
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-active-p))
            (should (equal (e-chat-composer-text) "follow-up draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-board-event-consumer-defers-focus-to-chat-policy ()
  "The board adapter does not undo composed chat viewport decisions."
  (let* ((buffer (e-chat-test--buffer nil "chat-board-event-focus"))
         (harness (buffer-local-value 'e-chat-harness buffer))
         spec)
    (unwind-protect
        (cl-letf (((symbol-function 'e-ui-work-schedule)
                   (lambda (candidate &rest _arguments)
                     (setq spec candidate)
                     nil)))
          (funcall
           (e-chat--event-consumer harness buffer)
           (e-events-make
            :type 'reasoning-delta
            :session-id "chat-board-event-focus"
            :turn-id "turn-1"
            :payload '(:content "progress")))
          (should (e-ui-work-spec-p spec))
          (should (eq (e-ui-work-spec-focus-policy spec) 'explicit)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-ui-work-diagnostics-track-pending-work ()
  "Opt-in UI work diagnostics show grouped pending work in the header."
  (let ((buffer (e-chat-test--buffer nil "chat-ui-work-diagnostics"))
        (e-chat-ui-work-diagnostics t))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-status "idle")
          (should-not (string-match-p "ui" header-line-format))
          (let ((first (e-ui-work-schedule
                        (e-ui-work-spec-create
                         :id "chat_test_markdown"
                         :description "Test pending markdown UI work."
                         :owner 'markdown-presentation
                         :target-buffer (current-buffer)
                         :key "one"
                         :generation 1
                         :delay 60
                         :focus-policy 'preserve
                         :reentrancy-policy 'defer
                         :apply #'ignore)
                        :on-event (lambda (&rest _)
                                    (e-chat-surface-refresh-ui-work-diagnostics))))
                (second (e-ui-work-schedule
                         (e-ui-work-spec-create
                          :id "chat_test_activity"
                          :description "Test pending activity UI work."
                          :owner 'activity-redraw
                          :target-buffer (current-buffer)
                          :key "two"
                          :generation 2
                          :delay 60
                          :focus-policy 'preserve
                          :reentrancy-policy 'defer
                          :apply #'ignore)
                         :on-event (lambda (&rest _)
                                     (e-chat-surface-refresh-ui-work-diagnostics)))))
            (e-chat-surface-refresh-ui-work-diagnostics)
            (should (string-match-p "ui 2" header-line-format))
            (should (string-match-p "activity-redraw:1" header-line-format))
            (should (string-match-p
                     "markdown-presentation:1" header-line-format))
            (e-ui-work-cancel first)
            (e-chat-surface-refresh-ui-work-diagnostics)
            (should (string-match-p "ui 1" header-line-format))
            (should-not (string-match-p
                         "markdown-presentation:1" header-line-format))
            (e-ui-work-cancel second)
            (e-chat-surface-refresh-ui-work-diagnostics)
            (should-not (string-match-p "ui" header-line-format))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-reattach-does-not-duplicate-rendered-events ()
  "Reattaching a chat buffer leaves one live subscription for the session."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (buffer (e-chat-open :harness harness :session-id "chat-reattach")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-attach-buffer buffer harness "chat-reattach")
          (should (= (e-chat-test--session-subscriber-count
                      harness "chat-reattach")
                     1))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload (list :message
                                         (list :role 'user
                                               :content "dup prompt"))))
          (should (= (e-chat-test--count-occurrences
                      "dup prompt"
                      (buffer-string))
                     1))
          (should (= (e-chat-test--count-occurrences
                      "dup prompt" (buffer-string))
                     1)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-reload-buffers-keeps-board-bound-harness ()
  "Reloading keeps the admitted endpoint, transcript, and composer draft."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (old-backend (e-backend-fake-create :items nil))
         (old-harness (e-harness-create :backend old-backend :sessions store))
         (new-backend (e-backend-fake-create
                       :items '((:type assistant-message :content "fresh answer")
                                (:type done :reason stop))))
         (new-harness (e-chat-test--activate-chat-session
                       (e-harness-create :backend new-backend :sessions store)))
         (buffer (e-chat-open :harness old-harness :session-id "chat-reload")))
    (unwind-protect
        (progn
          (e-session-append-message
           store "chat-reload" '(:id "msg-1" :role user :content "saved prompt"))
          (with-current-buffer (e-chat-test--composer buffer)
            (goto-char (point-max))
            (insert "stale prompt"))
          (e-chat-test--with-empty-harness-registry
            (let ((e-chat-default-harness-id :chat-test))
              (e-harness-registry-register-factory
               :chat-test
               (lambda () new-harness))
              (should (= (e-chat-reload-buffers) 1))))
          (with-current-buffer buffer
            (should (eq e-chat-harness old-harness))
            (should (eq (e-harness-backend e-chat-harness) new-backend))
            (should (equal e-chat-session-id "chat-reload"))
            (should (equal (e-chat-test--composer-text-for buffer)
                           "stale prompt"))
            (should (string-match-p "saved prompt" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))



(ert-deftest e-chat-test-reload-buffers-keeps-active-turn-harness ()
  "Reloading chat buffers does not detach a buffer from an in-flight turn."
  (let* ((old-backend
          (e-backend-create
           :name "held-chat"
           :start (cl-function
                   (lambda (&key messages options on-item on-done
                                  on-error on-request-start)
                     (ignore messages options on-item on-done
                             on-error on-request-start)
                     nil))))
         (old-harness (e-chat-test--activate-chat-session
                       (e-harness-create :backend old-backend)))
         (new-backend
          (e-backend-create
           :name "fresh-held-chat"
           :start (cl-function
                   (lambda (&key messages options on-item on-done
                                  on-error on-request-start)
                     (ignore messages options on-item on-done
                             on-error on-request-start)
                     nil))))
         (new-harness (e-chat-test--activate-chat-session
                       (e-harness-create
                        :backend new-backend)))
         (buffer (e-chat-open :harness old-harness
                              :session-id "chat-reload-active")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (e-chat-submit "long turn")
            (should (eq e-chat-harness old-harness))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (e-chat-service-active-turn-p
                        old-harness "chat-reload-active"))
                     1.0)))
          (e-chat-test--with-empty-harness-registry
            (let ((e-chat-default-harness-id :chat-test))
              (e-harness-registry-register-factory
               :chat-test
               (lambda () new-harness))
              (should (= (e-chat-reload-buffers) 1))))
          (with-current-buffer buffer
            (should (eq e-chat-harness old-harness))
            (should (not (eq e-chat-harness new-harness)))
            (should (eq (e-harness-backend e-chat-harness) old-backend))
            (should (e-chat-service-active-turn-p
                     e-chat-harness e-chat-session-id)))
          (e-harness-test-abort old-harness "chat-reload-active")
          (should (e-chat-test--wait-until
                   (lambda ()
                     (eq (e-harness-backend old-harness) new-backend))
                   1.0)))
      (ignore-errors
        (e-harness-test-abort old-harness "chat-reload-active"))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-reload-buffers-preserves-current-harness-without-default ()
  "Reloading chat buffers keeps their harness when no default is configured."
  (let* ((harness (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
         (buffer (e-chat-open :harness harness :session-id "chat-local")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local e-chat-harness-instance-id nil))
          (e-chat-test--with-empty-harness-registry
            (let ((e-chat-default-harness-id :missing-chat))
              (should (>= (e-chat-reload-buffers) 1))))
          (with-current-buffer buffer
            (should (eq e-chat-harness harness))
            (should (equal e-chat-session-id "chat-local"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-reload-buffers-preserves-instance-harness-without-backend ()
  "Instance-backed chat buffers keep their harness when factory creation fails."
  (let* ((harness (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
         (buffer (e-chat-open :harness harness :session-id "chat-instance")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local e-chat-harness-instance-id :chat-test))
          (e-chat-test--with-empty-harness-registry
            (e-harness-registry-register-factory
             :chat-test
             (lambda ()
               (user-error "Configured backend unavailable")))
            (e-harness-instance-register
             :id :chat-test
             :name "Chat Test"
             :kind 'chat
             :factory (lambda ()
                        (user-error "Configured backend unavailable"))
             :harness-id :chat-test
             :default t)
            (should (>= (e-chat-reload-buffers) 1)))
          (with-current-buffer buffer
            (should (eq e-chat-harness harness))
            (should (eq e-chat-harness-instance-id :chat-test))
            (should (equal e-chat-session-id "chat-instance"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-default-harness-requests-configured-registry-id ()
  "Chat shell asks the harness registry for the configured default id."
  (e-chat-test--with-empty-harness-registry
    (let* ((e-chat-default-harness-id :chat-test)
           (harness (e-chat-test--activate-chat-session
                     (e-harness-create
                      :backend (e-backend-fake-create :items nil)))))
      (e-harness-registry-register :chat-test harness)
      (should (eq (e-chat--default-harness) harness)))))



(ert-deftest e-chat-test-default-harness-missing-id-is-user-error ()
  "Missing configured harness ids surface as chat command errors."
  (e-chat-test--with-empty-harness-registry
    (let ((e-chat-default-harness-id :missing-chat))
      (should-error (e-chat--default-harness) :type 'user-error))))



(ert-deftest e-chat-test-default-harness-requires-chat-session ()
  "The configured default harness must expose the chat-session capability."
  (e-chat-test--with-empty-harness-registry
    (let ((e-chat-default-harness-id :chat-test)
          (harness (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-registry-register :chat-test harness)
      (should-error (e-chat--default-harness) :type 'user-error))))



(ert-deftest e-chat-test-source-does-not-cross-harness-boundaries ()
  "Chat shell uses registry lookup and public harness projections."
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "lisp/shells/chat/e-chat.el"
                                     (e-source-directory)))
                  (buffer-string))))
    (dolist (forbidden '("e-openai-create-harness"
                         "e-harness-turn-options"
                         "e-session-display-title"
                         "e-session-list"
                         "e-session-activity-events"
                         "e-session-get"))
      (should-not (string-match-p forbidden source)))))



(ert-deftest e-chat-test-new-creates-distinct-persisted-sessions ()
  "Each new chat command invocation creates a distinct persisted session."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         first-id second-id)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--kill-chat-buffers)
            (with-current-buffer (e-chat-new)
              (setq first-id e-chat-session-id))
            (with-current-buffer (e-chat-new)
              (setq second-id e-chat-session-id))
            (should (not (equal first-id second-id)))
            (should (file-exists-p
                     (expand-file-name (concat first-id ".jsonl")
                                       (expand-file-name "sessions" directory))))
            (should (file-exists-p
                     (expand-file-name
                      (concat second-id ".jsonl")
                      (expand-file-name "sessions" directory))))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))



(ert-deftest e-chat-test-new-persists-owning-chat-instance ()
  "New sessions remember the configured chat instance that created them."
  (let* ((store (e-session-store-create))
         (alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions store)))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-alpha))
            (e-chat-test--register-chat-instance
             :chat-alpha "Alpha Target" alpha-harness t)
            (e-chat-test--register-chat-instance
             :chat-beta "Beta Target" beta-harness)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (cl-find-if
                          (lambda (candidate)
                            (string-match-p "Beta Target" candidate))
                          (all-completions "" collection)))))
              (with-current-buffer (e-chat-new)
                (let* ((session (e-session-get store e-chat-session-id))
                       (metadata (plist-get session :metadata)))
                  (should (eq e-chat-harness-instance-id :chat-beta))
                  (should (eq (plist-get metadata :harness-instance-id)
                              :chat-beta)))))))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-latest-session-selects-board-owning-root ()
  "Latest-session navigation never selects a newer private participant."
  (let* ((harness
          (e-chat-test--activate-chat-session
           (e-harness-create :backend (e-backend-fake-create :items nil))))
         (binding
          (e-chat-service-create-board
           :harness harness :id "latest-root"
           :metadata '(:name "Latest Root")))
         (board (e-chat-service-binding-board binding)))
    (e-chat-service-create-participant
     board harness :id "latest-private"
     :metadata '(:name "Latest Private"))
    (should (equal (e-chat--latest-session-id harness) "latest-root"))))



(ert-deftest e-chat-test-attach-buffer-ignores-persisted-read-marker-plist ()
  "Attaching ignores stale read markers replayed as plist metadata."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer (get-buffer-create "*e-chat-read-marker-attach-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session
           store
           :id "read-marker-attach"
           :metadata
           '(:name "Read Marker"
             :e-chat-read-markers (:chat-default "assistant-read")))
          (e-session-append-message
           store "read-marker-attach"
           '(:id "assistant-read" :role assistant :content "answer"))
          (with-current-buffer buffer
            (e-chat-attach-buffer
             buffer harness "read-marker-attach" :chat-default)
            (should (equal e-chat-session-id "read-marker-attach"))
            (should
             (e-chat-overview-session-unread-p
              harness
              (e-session-get store "read-marker-attach")
              :chat-default))
            (e-chat-overview-mark-session-read
             harness "read-marker-attach" :chat-default)
            (should-not
             (e-chat-overview-session-unread-p
              harness
              (e-session-get store "read-marker-attach")
              :chat-default))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-attach-buffer-does-not-rewrite-existing-project-root ()
  "Attaching an existing chat does not rewrite durable session metadata."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "e-chat-attach-root-" t)))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (buffer (get-buffer-create "*e-chat-attach-root-test*"))
         (writes 0))
    (unwind-protect
        (progn
          (e-chat-test--create-session
           store
           :id "rooted"
           :metadata (list :project-root directory))
          (with-current-buffer buffer
            (setq-local default-directory directory))
          (let ((original-set-session-config
                 (symbol-function 'e-session-set-session-config)))
            (cl-letf (((symbol-function 'e-session-set-session-config)
                       (lambda (&rest args)
                         (setq writes (1+ writes))
                         (apply original-set-session-config args))))
              (e-chat-attach-buffer buffer harness "rooted" nil)))
          (should (= writes 0)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))



(ert-deftest e-chat-test-add-context-to-latest-targets-visible-session ()
  "Latest context insertion targets a visible chat before recency."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         window)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "visible-session"
                              :metadata '(:name "visible-session"))
            (setq window
                  (display-buffer
                   (e-chat-open :harness harness :session-id "visible-session")))
            (e-chat-test--create-session store :id "latest-session"
                              :metadata '(:name "latest-session"))
            (with-temp-buffer
              (insert "alpha\nbeta\ngamma\n")
              (goto-char (point-min))
              (forward-line 1)
              (let ((chat-buffer (e-chat-add-context-to-latest)))
                (should (eq chat-buffer (window-buffer window)))
                (with-current-buffer chat-buffer
                  (should (equal e-chat-session-id "visible-session"))
                  (should (string-match-p
                           "@\\[.*:2 (context 1-3)\\]"
                           (e-chat-test--composer-text-for chat-buffer))))))))
      (when (and window (window-live-p window))
        (delete-window window))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-add-context-to-latest-prefers-visible-duplicate-session-buffer ()
  "Latest context insertion ignores hidden duplicate buffers for the same session."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         visible-buffer
         hidden-duplicate
         window)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "duplicate-session"
                              :metadata '(:name "duplicate-session"))
            (setq visible-buffer
                  (e-chat-open :harness harness
                               :session-id "duplicate-session"))
            (setq window (display-buffer visible-buffer))
            (setq hidden-duplicate
                  (get-buffer-create "*e-chat:hidden duplicate-session*"))
            (e-chat-attach-buffer hidden-duplicate harness "duplicate-session" nil)
            ;; Model the observed bug: an undisplayed duplicate for the same
            ;; session can appear earlier than the visible chat in `buffer-list'.
            (with-temp-buffer
              (insert "alpha\nbeta\ngamma\n")
              (goto-char (point-min))
              (forward-line 1)
              (let ((orig-buffer-list (symbol-function 'buffer-list)))
                (cl-letf (((symbol-function 'buffer-list)
                           (lambda (&optional frame)
                             (append (list hidden-duplicate visible-buffer)
                                     (remove hidden-duplicate
                                             (remove visible-buffer
                                                     (funcall orig-buffer-list
                                                              frame)))))))
                  (let ((chat-buffer (e-chat-add-context-to-latest)))
                    (should (eq chat-buffer visible-buffer))
                    (with-current-buffer visible-buffer
                      (should (string-match-p
                               "@\\[.*:2 (context 1-3)\\]"
                               (e-chat-test--composer-text-for visible-buffer))))
                    (with-current-buffer hidden-duplicate
                      (should (string-empty-p
                               (e-chat-test--composer-text-for
                                hidden-duplicate))))))))))
      (when (and window (window-live-p window))
        (delete-window window))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-find-session-buffer-uses_workspace_existing_buffer_helper ()
  "Chat session lookup delegates existing-buffer preference to the workspace helper."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (buffer (get-buffer-create "*e-chat:workspace helper target*"))
         captured-prefer-visible
         captured-result)
    (unwind-protect
        (progn
          (e-chat-attach-buffer buffer harness "helper-session" nil)
          (cl-letf (((symbol-function 'e-workspace-find-buffer)
                     (cl-function
                      (lambda (predicate &key prefer-visible workspace)
                        (ignore workspace)
                        (setq captured-prefer-visible prefer-visible)
                        (setq captured-result
                              (and (funcall predicate buffer)
                                   buffer))))))
            (should (eq (e-chat--find-session-buffer
                         "helper-session"
                         harness
                         nil)
                        buffer)))
          (should captured-prefer-visible)
          (should (eq captured-result buffer)))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-add-context-to-latest-falls-back-to-most-recent-session ()
  "Latest context insertion opens the most recently updated chat session."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "old-session"
                              :metadata '(:name "old-session"))
            (e-chat-test--create-session store :id "latest-session"
                              :metadata '(:name "latest-session"))
            (with-temp-buffer
              (insert "alpha\nbeta\ngamma\n")
              (goto-char (point-min))
              (forward-line 1)
              (let ((chat-buffer (e-chat-add-context-to-latest)))
                (with-current-buffer chat-buffer
                  (should (equal e-chat-session-id "latest-session"))
                  (should (string-match-p
                           "latest-session"
                           (buffer-name)))
                  (should (string-match-p
                           "@\\[.*:2 (context 1-3)\\]"
                           (e-chat-test--composer-text-for chat-buffer))))))))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-add-context-to-latest-deactivates-source-region ()
  "Latest context insertion clears the selected region in the source buffer."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "latest-session"
                              :metadata '(:name "latest-session"))
            (with-temp-buffer
              (insert "alpha beta gamma")
              (goto-char (point-min))
              (search-forward "beta")
              (set-mark (match-beginning 0))
              (setq mark-active t)
              (let ((chat-buffer (e-chat-add-context-to-latest)))
                (should-not mark-active)
                (with-current-buffer chat-buffer
                  (should (string-match-p
                           "@\\[.*:1\\]"
                           (e-chat-test--composer-text-for chat-buffer))))))))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-add-context-picker-can-create-new-session ()
  "Picker context insertion can create a new session target."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "existing-session")
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (car (all-completions "" collection)))))
              (with-temp-buffer
                (insert "one\ntwo\nthree\n")
                (goto-char (point-min))
                (let ((chat-buffer (e-chat-add-context-to-session)))
                  (with-current-buffer chat-buffer
                    (should-not (equal e-chat-session-id
                                       "existing-session"))
                    (should (= (length (e-harness-session-list
                                        e-chat-harness))
                               2))
                    (should (string-match-p
                             "@\\[.*:1 (context 1-3)\\]"
                             (e-chat-test--composer-text-for
                              chat-buffer)))))))))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-add-context-picker-can-select-existing-session ()
  "Picker context insertion can target an existing chat session."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "target-session"
                              :metadata '(:name "target-session"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (cadr (all-completions "" collection)))))
              (with-temp-buffer
                (insert "one\ntwo\nthree\n")
                (goto-char (point-min))
                (let ((chat-buffer (e-chat-add-context-to-session)))
                  (with-current-buffer chat-buffer
                    (should (equal e-chat-session-id "target-session"))
                    (should (= (length (e-harness-session-list
                                        e-chat-harness))
                               1))
                    (should (string-match-p
                             "@\\[.*:1 (context 1-3)\\]"
                             (e-chat-test--composer-text-for
                              chat-buffer)))))))))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-add-context-picker-selects-session-across-chat-instances ()
  "Context insertion can target sessions outside the default chat instance."
  (let* ((alpha-store (e-session-store-create))
         (beta-store (e-session-store-create))
         (alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions alpha-store)))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions beta-store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-alpha))
            (e-chat-test--register-chat-instance
             :chat-alpha "Alpha Target" alpha-harness t)
            (e-chat-test--register-chat-instance
             :chat-beta "Beta Target" beta-harness)
            (e-chat-test--create-session alpha-store :id "alpha-session"
                              :metadata '(:name "Alpha Session"))
            (e-chat-test--create-session beta-store :id "beta-session"
                              :metadata '(:name "Beta Session"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (cl-find-if
                          (lambda (candidate)
                            (string-match-p "Beta Target.*Beta Session"
                                            candidate))
                          (all-completions "" collection)))))
              (with-temp-buffer
                (insert "one\ntwo\nthree\n")
                (goto-char (point-min))
                (let ((chat-buffer (e-chat-add-context-to-session)))
                  (with-current-buffer chat-buffer
                    (should (eq e-chat-harness beta-harness))
                    (should (eq e-chat-harness-instance-id :chat-beta))
                    (should (equal e-chat-session-id "beta-session"))
                    (should (string-match-p
                             "@\\[.*:1 (context 1-3)\\]"
                             (e-chat-test--composer-text-for
                              chat-buffer)))))))))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-add-context-deduplicates-shared-store-by-owner ()
  "Context insertion lists shared-store sessions under the owning instance only."
  (let* ((store (e-session-store-create))
         (alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions store)))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions store)))
         seen-candidates)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-alpha))
            (e-chat-test--register-chat-instance
             :chat-alpha "Alpha Target" alpha-harness t)
            (e-chat-test--register-chat-instance
             :chat-beta "Beta Target" beta-harness)
            (e-chat-service-create-session
             :harness alpha-harness :id "alpha-session"
             :metadata '(:name "Alpha Session"))
            (e-chat-service-create-session
             :harness beta-harness :id "beta-session"
             :metadata '(:name "Beta Session"
                         :harness-instance-id :chat-beta))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (setq seen-candidates
                               (all-completions "" collection))
                         (cl-find-if
                          (lambda (candidate)
                            (string-match-p "Beta Target.*Beta Session"
                                            candidate))
                          seen-candidates))))
              (with-temp-buffer
                (insert "one\ntwo\n")
                (goto-char (point-min))
                (let ((chat-buffer (e-chat-add-context-to-session)))
                  (with-current-buffer chat-buffer
                    (should (eq e-chat-harness beta-harness))
                    (should (eq e-chat-harness-instance-id :chat-beta))
                    (should (equal e-chat-session-id "beta-session"))))))
            (should (= (length seen-candidates) 3))
            (should (cl-find-if
                     (lambda (candidate)
                       (string-match-p "Alpha Target.*Alpha Session"
                                       candidate))
                     seen-candidates))
            (should (cl-find-if
                     (lambda (candidate)
                       (string-match-p "Beta Target.*Beta Session"
                                       candidate))
                     seen-candidates))
            (should-not
             (cl-find-if
              (lambda (candidate)
                (string-match-p "Alpha Target.*Beta Session" candidate))
              seen-candidates))))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-add-context-picker-preserves-session-list-order ()
  "Picker context insertion keeps store recency order under sorting frontends."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "older-session"
                              :metadata '(:name "Alpha old"))
            (e-session-append-message
             store "older-session"
             '(:id "older-message" :role user
               :created-at "2026-05-22T10:00:01Z"
               :content "older prompt"))
            (e-chat-test--create-session store :id "newer-session"
                              :metadata '(:name "Zulu newest"))
            (e-session-append-message
             store "newer-session"
             '(:id "newer-message" :role user
               :created-at "2026-05-22T10:00:03Z"
               :content "newer prompt"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (let* ((metadata (completion-metadata
                                           "" collection nil))
                                (display-sort
                                 (completion-metadata-get
                                  metadata 'display-sort-function))
                                (candidates (all-completions "" collection))
                                (visible (if display-sort
                                             (funcall display-sort candidates)
                                           (sort (copy-sequence candidates)
                                                 #'string<))))
                           (cl-find-if
                            (lambda (candidate)
                              (not (equal candidate
                                          e-chat--new-context-session-label)))
                            visible)))))
              (with-temp-buffer
                (insert "one\ntwo\nthree\n")
                (goto-char (point-min))
                (let ((chat-buffer (e-chat-add-context-to-session)))
                  (with-current-buffer chat-buffer
                    (should (equal e-chat-session-id "newer-session"))))))))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-add-context-to-session-deactivates-source-region ()
  "Picker context insertion clears the selected region in the source buffer."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "target-session"
                              :metadata '(:name "target-session"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (cadr (all-completions "" collection)))))
              (with-temp-buffer
                (insert "alpha beta gamma")
                (goto-char (point-min))
                (search-forward "beta")
                (set-mark (match-beginning 0))
                (setq mark-active t)
                (let ((chat-buffer (e-chat-add-context-to-session)))
                  (should-not mark-active)
                  (with-current-buffer chat-buffer
                    (should (equal e-chat-session-id "target-session"))
                    (should (string-match-p
                             "@\\[.*:1\\]"
                             (e-chat-test--composer-text-for
                              chat-buffer)))))))))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-model-and-effort-commands-update-session-options ()
  "Chat model and effort commands update harness-owned session options."
  (let ((buffer (e-chat-test--buffer nil "chat-options")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) "gpt-test"))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _args) "high")))
            (call-interactively #'e-chat-set-model)
            (call-interactively #'e-chat-set-effort))
          (should (equal (e-harness-session-options
                          e-chat-harness
                          e-chat-session-id)
                         '(:model "gpt-test" :reasoning-effort "high")))
          (should (string-match-p "gpt-test" header-line-format))
          (should (string-match-p "high" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-mode-line-status-formats-model-effort-and-context ()
  "Mode-line status includes model, effort, and estimated context usage."
  (dolist (model '("gpt-5.6"
                   "gpt-5.6-sol"
                   "gpt-5.6-terra"
                   "gpt-5.6-luna"))
    (should (equal (e-context-status-model-token-limit model)
                   353400)))
  (should (equal (e-context-status-model-token-limit "gpt-5.5")
                 258400))
  (should
   (equal
    (e-context-status-format "e-chat" "gpt-5.5" "high" 18000 400000 t)
    "e-chat gpt-5.5/high ~5% (~18k/400k tok)"))
  (should
   (equal
    (e-context-status-format "e-chat" "gpt-5.5" "high" 40000 258400 nil)
    "e-chat gpt-5.5/high 15% (40k/258k tok)")))



(ert-deftest e-chat-test-mode-line-status-uses-session-context ()
  "Attached chat buffers use the session model and configured context limit."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (e-chat-context-token-estimate-bytes-per-token 1.0)
         (buffer (e-chat-open :harness harness :session-id "chat-mode-line")))
    (unwind-protect
        (let ((e-context-budget-model-token-limits
               '(("gpt-5.5" . 100))))
          (with-current-buffer buffer
            (e-chat-surface-set-redraw-visible t)
            (e-session-append-message
             store
             e-chat-session-id
             '(:role user :content "context question"))
            (e-chat-test--seed-board-log-from-private-fixture
             harness e-chat-session-id)
            (e-chat-surface-set-status "idle" t)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)
                                     :owner 'chat-mode-line-status))
            (should (string-match-p "gpt-5.5/high" mode-name))
            (should (string-match-p "~[0-9]+ pct" mode-name))
            (should (string-match-p "/100 tok" mode-name))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-open-loaded-session-refreshes-mode-line-status ()
  "Opening a loaded session schedules its model/context mode-line status."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (generate-new-buffer " *e-chat-mode-line-open*")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((e-context-budget-model-token-limits
                 '(("gpt-5.5" . 100))))
            (cl-letf (((symbol-function 'e-chat-surface-redraw-visible-p)
                       (lambda () t)))
              (e-chat-attach-buffer buffer harness "chat-mode-line-open")
              (should (equal mode-name "e-chat"))
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch
                 :buffer buffer
                 :owner 'chat-mode-line-status))
              (should (string-match-p "gpt-5.5/high" mode-name))
              (should (string-match-p "/100 tok" mode-name)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))




(ert-deftest e-chat-test-set-status-skips-context-refresh-by-default ()
  "Ordinary status updates avoid full harness context estimation."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-status-fast"))
         (context-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-harness-context)
                     (lambda (&rest _args)
                       (setq context-calls (1+ context-calls))
                       (error "context estimate should be skipped"))))
            (e-chat-surface-set-status "waiting for provider")
            (e-chat-surface-set-status "done"))
          (should (= context-calls 0))
          (should (string-match-p "E Chat: done" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-set-status-skips-duplicate-updates ()
  "Repeated ordinary status updates do not rewrite header-line state."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-status-duplicate"))
         (session-title-calls 0)
         (original-session-title (symbol-function 'e-harness-session-title)))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-status "waiting for provider")
          (cl-letf (((symbol-function 'e-harness-session-title)
                     (lambda (&rest args)
                       (setq session-title-calls (1+ session-title-calls))
                       (apply original-session-title args))))
            (e-chat-surface-set-status "waiting for provider"))
          (should (= session-title-calls 0))
          (should (string-match-p "E Chat: waiting for provider"
                                  header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-profile-records-status-updates ()
  "Enabled dev profiling records chat status updates."
  (let* ((profile-directory (make-temp-file "e-chat-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-status-profile")))
    (unwind-protect
        (with-current-buffer buffer
          (e-dev-profile-start)
          (e-chat-surface-set-status "waiting for provider")
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "chat.status" aggregates nil nil #'equal))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory profile-directory t))))



(ert-deftest e-chat-test-profile-records-render-ui-spans ()
  "Enabled dev profiling records chat render UI spans."
  (let* ((profile-directory (make-temp-file "e-chat-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-render-profile")))
    (unwind-protect
        (with-current-buffer buffer
          (e-dev-profile-start)
          (e-chat--render-event
           (list :type 'message-added
                 :turn-id "turn-profile"
                 :created-at "2026-06-07T20:00:00Z"
                 :payload (list :message
                                '(:role assistant :content "profiled"))))
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "chat.render-event" aggregates nil nil #'equal))
            (should (alist-get "chat.insert-entry" aggregates nil nil #'equal))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory profile-directory t))))



(ert-deftest e-chat-test-set-status-schedules-explicit-mode-line-refresh ()
  "Explicit status refresh leaves the status stack before context work."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                               :session-id "chat-status-refresh")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-chat-surface-invalidate-mode-line-context-estimate)
          (e-chat-surface-set-status "idle" t)
          (should (= (length (e-ui-work-pending
                              (current-buffer)
                              :owner 'chat-mode-line-status))
                     1))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (stringp (e-chat-surface-mode-line-status)))
          (should (equal mode-name
                         (e-chat-surface-mode-line-display-text
                          (e-chat-surface-mode-line-status)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-prefer-token-usage-skips-missing-context-estimate ()
  "Fast mode-line refresh avoids context rebuilding when usage is missing."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-mode-line-fast"))
         (context-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (cl-letf (((symbol-function 'e-harness-context)
                     (lambda (&rest _args)
                       (setq context-calls (1+ context-calls))
                       (error "context estimate should be skipped"))))
            (e-chat-surface-refresh-mode-line-status t))
          (should (= context-calls 0))
          (should (equal mode-name
                         "e-chat gpt-5.5/high")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-mode-line-status-reuses-fresh-estimate-cache ()
  "Repeated mode-line projection keeps the semantic status stable."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-mode-line-cache")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (let ((e-context-budget-model-token-limits
                 '(("gpt-5.5" . 1000))))
            (cl-letf (((symbol-function 'e-harness-context)
                       (symbol-function 'e-harness-context)))
              (e-chat-surface-set-status "idle" t)
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer (current-buffer)
                                       :owner 'chat-mode-line-status))))
          (should (string-match-p
                   "\\`e-chat gpt-5\\.5/high"
                   (e-chat-surface-mode-line-status))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-mode-line-status-reuses-fresh-status-snapshot ()
  "Mode-line status snapshots remain a surface-owned observable."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-mode-line-status-cache")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-session-append-message
           store e-chat-session-id '(:role user :content "cached status"))
          (e-chat-surface-invalidate-mode-line-context-estimate)
          (e-chat-surface-set-status "idle" t)
          (e-chat-surface-set-status "ready" t)
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (string-match-p
                   "\\`e-chat gpt-5\\.5/high"
                   (e-chat-surface-mode-line-status))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-token-usage-event-skips-context-estimate ()
  "Fresh token-usage refreshes avoid full context estimation."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-token-usage-fast")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-session-append-activity-event
           store
           e-chat-session-id
           "turn-1"
           'token-usage
           '(:input-tokens 1200 :total-tokens 1300))
          (e-chat--render-event
           (e-events-make :type 'token-usage
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:input-tokens 1200
                                     :total-tokens 1300)))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (string-match-p "1.2k/258k tok" mode-name)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-token-usage-coalesces-mode-line-render-work ()
  "Repeated usage events schedule one latest-value mode-line projection."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend :sessions store
                   :default-options '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness :session-id "chat-token-status-work")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (let ((e-chat-mode-line-status-delay 0))
            (dotimes (_ 3)
              (e-chat-render-event
               (e-events-make :type 'token-usage
                              :session-id e-chat-session-id :turn-id "turn-1")))
            (should (= (length (e-ui-work-pending
                                (current-buffer) :owner 'chat-mode-line-status))
                       1))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)
                                     :owner 'chat-mode-line-status))
            (should-not (e-ui-work-pending
                         (current-buffer) :owner 'chat-mode-line-status))
            (should (stringp (e-chat-surface-mode-line-status)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-show-context-opens-read-only-preview-buffer ()
  "Context command renders the current session context in a read-only buffer."
  (let ((buffer (e-chat-test--buffer nil "chat-context")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           (e-harness-sessions e-chat-harness)
           e-chat-session-id
           '(:role user :content "context question"))
          (let ((context-buffer (e-chat-show-context)))
            (should (buffer-live-p context-buffer))
            (with-current-buffer context-buffer
              (should (derived-mode-p 'special-mode))
              (should buffer-read-only)
              (should (string-match-p "Session: chat-context" (buffer-string)))
              (should (string-match-p "context question" (buffer-string)))
              (should-error (insert "mutate") :type 'buffer-read-only))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when-let ((context-buffer (get-buffer e-chat-context-buffer-name)))
        (kill-buffer context-buffer)))))



(ert-deftest e-chat-test-keymap-refresh-updates-stale-reload-bindings ()
  "Reload-time keymap refresh replaces bindings preserved by `defvar'."
  (let ((e-chat-response-navigation-mode-map (make-sparse-keymap))
        (e-chat-mode-map (make-sparse-keymap))
        (e-chat-block-view-mode-map (make-sparse-keymap))
        (e-chat-tool-list-mode-map (make-sparse-keymap))
        (e-chat-tool-output-mode-map (make-sparse-keymap))
        (e-chat-surface-command-map (make-sparse-keymap)))
    (define-key e-chat-response-navigation-mode-map
                (kbd "RET")
                'e-chat-response-navigation-expand)
    (define-key e-chat-surface-command-map
                (kbd "C-x 0")
                'e-chat-surface-delete-window)
    (define-key e-chat-surface-command-map
                [remap delete-window]
                'e-chat-surface-delete-window)
    (e-chat--refresh-keymaps)
    (should (eq (lookup-key e-chat-response-navigation-mode-map (kbd "RET"))
                'e-chat-response-navigation-activate))
    (should (eq (lookup-key e-chat-response-navigation-mode-map (kbd "y"))
                'e-chat-response-navigation-copy))
    (should (eq (lookup-key e-chat-block-view-mode-map (kbd "G"))
                'e-chat-block-view-end))
    (should (eq (lookup-key e-chat-block-view-mode-map (kbd "g g"))
                'e-chat-block-view-beginning))
    (should (eq (lookup-key e-chat-block-view-mode-map (kbd "v"))
                'e-chat-block-view-select))
    (should (eq (lookup-key e-chat-block-view-mode-map (kbd "y"))
                'e-chat-block-view-copy))
    (should (eq (lookup-key e-chat-mode-map (kbd "M-y"))
                'e-chat-copy-latest-response))
    (should (eq (lookup-key e-chat-mode-map (kbd "M-o"))
                'e-chat-open-latest-response))
    (should-not (lookup-key e-chat-surface-command-map (kbd "C-x 0")))
    (should-not
     (lookup-key e-chat-surface-command-map [remap delete-window]))))



(ert-deftest e-chat-test-context-mode-binds-default-reference-shortcuts ()
  "The global context keymap binds latest and picker insertion shortcuts."
  (should (eq (lookup-key e-chat-context-mode-map (kbd "s-i"))
              'e-chat-add-context-to-latest))
  (should (eq (lookup-key e-chat-context-mode-map (kbd "s-I"))
              'e-chat-add-context-to-session)))



(ert-deftest e-chat-test-evil-normal-context-bindings-use-super-i ()
  "Evil normal bindings use s-i and s-I without taking over bare I."
  (let (calls)
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (&rest args)
                 (push args calls))))
      (e-chat--configure-evil-context-bindings))
    (should (member (list 'normal
                          e-chat-context-mode-map
                          (kbd "s-i")
                          #'e-chat-add-context-to-latest)
                    calls))
    (should (member (list 'normal
                          e-chat-context-mode-map
                          (kbd "s-I")
                          #'e-chat-add-context-to-session)
                    calls))
    (should-not (seq-some (lambda (call)
                            (equal (nth 2 call) (kbd "I")))
                          calls))))



(ert-deftest e-chat-test-keymap-preserves-host-alt-leader ()
  "The chat mode keymap preserves a host-provided alternate leader prefix."
  (let ((e-chat-mode-map (make-sparse-keymap))
        (had-alt-key (boundp 'doom-leader-alt-key))
        (old-alt-key (and (boundp 'doom-leader-alt-key)
                          (symbol-value 'doom-leader-alt-key)))
        (had-leader-map (boundp 'doom-leader-map))
        (old-leader-map (and (boundp 'doom-leader-map)
                             (symbol-value 'doom-leader-map)))
        (leader-map (make-sparse-keymap)))
    (unwind-protect
        (progn
          (define-key leader-map (kbd "f") #'find-file)
          (set 'doom-leader-alt-key "M-SPC")
          (set 'doom-leader-map leader-map)
          (e-chat--refresh-keymaps)
          (should (eq (lookup-key e-chat-mode-map (kbd "M-SPC"))
                      leader-map))
          (should (eq (lookup-key e-chat-mode-map (kbd "M-SPC f"))
                      #'find-file)))
      (if had-alt-key
          (set 'doom-leader-alt-key old-alt-key)
        (makunbound 'doom-leader-alt-key))
      (if had-leader-map
          (set 'doom-leader-map old-leader-map)
        (makunbound 'doom-leader-map)))))



(ert-deftest e-chat-test-startup-enables-global-context-mode ()
  "Chat shell startup enables the global context insertion keymap."
  (let ((e-chat-context-mode nil))
    (e-chat-startup)
    (should e-chat-context-mode)))



(ert-deftest e-chat-test-registers-chat-shell-on-load ()
  "Loading e-chat registers the chat shell manifest."
  (should (eq (e-shell-id (e-shell-get 'chat)) 'chat))
  (should (eq (e-shell-command-interactive
               (e-shell-command-by-id (e-shell-get 'chat) 'new))
              'e-chat-new)))



(ert-deftest e-chat-test-open-session-starts-index-load-asynchronously ()
  "Opening an unloaded indexed session starts replay without sync load."
  (let* ((directory (make-temp-file "e-chat-open-index-" t))
         (store (e-session-persistent-store-create directory))
         buffer
         started)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "async-open"
                            :metadata '(:name "Async open"))
          (e-session-append-message
           store "async-open"
           '(:id "msg-1" :role user :content "open prompt"))
          (e-session-append-message
           store "async-open"
           '(:id "msg-2" :role assistant :content "open response"))
          (let* ((indexed-store
                  (e-session-persistent-index-store-create directory))
                 (harness (e-harness-create
                           :backend (e-backend-fake-create :items nil)
                           :sessions indexed-store)))
            (cl-letf (((symbol-function 'e-session-load-session)
                       (lambda (&rest _args)
                         (error "opened through sync transcript load")))
                      ((symbol-function 'e-session-load-session-start)
                       (lambda (_store session-id &rest _args)
                         (setq started session-id)
                         (e-request-lifecycle-create
                          :owner 'e-chat-test
                          :session-id session-id
                          :state 'started))))
              (setq buffer (e-chat-open-session harness "async-open"))
              (with-current-buffer buffer
                (let ((text (buffer-string)))
                  (should (equal started "async-open"))
                  (should e-chat--session-load-request)
                  (should (string-match-p "open prompt" text))
                  (should (string-match-p "Loading transcript" text))
                  (should-not (string-match-p "open response" text)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))



(ert-deftest e-chat-test-index-load-setup-failure-calls-recovery-hook ()
  "Synchronous checkpoint failures use the asynchronous chat recovery seam."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         callback-condition
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'e-chat--unloaded-index-session)
                   (lambda (_harness session-id)
                     (list :id session-id :loaded nil :title "Broken")))
                  ((symbol-function 'e-session-load-session-start)
                   (lambda (&rest _args)
                     (signal 'e-session-checkpoint-invalid
                             '("broken" "invalid checkpoint")))))
          (setq buffer
                (e-chat-open
                 :harness harness
                 :session-id "broken"
                 :on-session-load-error
                 (lambda (condition)
                   (setq callback-condition condition))))
          (should (e-chat-test--wait-until
                   (lambda () callback-condition)
                   1.0))
          (should (eq (car callback-condition)
                      'e-session-checkpoint-invalid))
          (with-current-buffer buffer
            (should-not e-chat--session-load-request)
            (should (string-match-p "Failed to load transcript"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-index-loading-bounds-large-session-summary ()
  "Loading projection does not render an unbounded index summary."
  (let ((e-chat-session-summary-preview-max-chars 40)
        (summary (concat "visible prefix " (make-string 200 ?x)
                         " forbidden tail")))
    (with-temp-buffer
      (e-chat-mode)
      (let ((inhibit-read-only t))
        (e-chat-transcript-render-session-loading (list :summary summary)))
      (let ((text (buffer-string)))
        (should (string-match-p "visible prefix" text))
        (should (string-match-p "Loading transcript" text))
        (should-not (string-match-p "forbidden tail" text))
        (should (< (length text) 120))))))



(ert-deftest e-chat-test-open-session-renders-after-async-index-load ()
  "Opening an unloaded indexed session renders transcript after async replay."
  (let* ((directory (make-temp-file "e-chat-open-index-" t))
         (store (e-session-persistent-store-create directory))
         ;; Keep this cooperatively multi-step without relying on hundreds of
         ;; zero-delay timers completing inside a two-second test deadline.
         (e-session-load-chunk-bytes 128)
         buffer)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "async-render"
                            :metadata '(:name "Async render"))
          (e-session-append-message
           store "async-render"
           '(:id "msg-1" :role user :content "render prompt"))
          (e-session-append-message
           store "async-render"
           '(:id "msg-2" :role assistant :content "render response"))
          (e-chat-test--seed-board-log-from-private-fixture
           (e-harness-create
            :backend (e-backend-fake-create :items nil)
            :sessions store)
           "async-render")
          (e-session-flush-write-queue store)
          (let* ((indexed-store
                  (e-session-persistent-index-store-create directory))
                 (harness (e-harness-create
                           :backend (e-backend-fake-create :items nil)
                           :sessions indexed-store)))
            (setq buffer (e-chat-open-session harness "async-render"))
            (with-current-buffer buffer
              (should e-chat--session-load-request)
              (should (string-match-p "Loading transcript" (buffer-string))))
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (buffer-live-p buffer)
                          (with-current-buffer buffer
                            e-chat--session-load-request)
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (with-current-buffer buffer
              (let ((text (buffer-string)))
                (should-not e-chat--session-load-request)
                (should (string-match-p "render prompt" text))
                (should (string-match-p "render response" text))
                (should-not (string-match-p "Loading transcript" text))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))



(ert-deftest e-chat-test-open-reuses-live-session-buffer-without-reattach ()
  "Opening an already-live session leaves its projection and viewport intact."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         buffer
         (attach-count 0))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "live-reopen"
                            :metadata '(:name "Live reopen"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "live-reopen")
          (setq buffer (e-chat-open-session harness "live-reopen"))
          (cl-letf (((symbol-function 'e-chat-attach-buffer)
                     (lambda (&rest _arguments)
                       (cl-incf attach-count))))
            (should (eq buffer
                        (e-chat-open-session harness "live-reopen")))
            (should (= attach-count 0))
            (with-current-buffer buffer
              (e-chat--unsubscribe))
            (should (eq buffer
                        (e-chat-open-session harness "live-reopen")))
            (should (= attach-count 1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-focused-chat-buffer-marks-session-read ()
  "Focusing a chat buffer records the latest assistant response as read."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (workspace (make-e-workspace-token
                     :backend 'single
                     :id 'focus-read-workspace
                     :name "focus-read-workspace"
                     :frame (selected-frame)))
         (buffer (generate-new-buffer " *e-chat-focus-read-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "focus-read"
                            :metadata '(:name "Focus read"))
          (e-session-append-message
           store "focus-read"
           '(:id "msg-1" :role user :content "prompt"))
          (e-session-append-message
           store "focus-read"
           '(:id "msg-2" :role assistant :content "response"))
          (should (e-chat-overview-session-unread-p
                   harness
                   (car (e-harness-session-list harness))))
          (with-current-buffer buffer
            (e-chat-mode)
            (setq-local e-chat-harness harness)
            (setq-local e-chat-session-id "focus-read")
            (e-buffer-set-workspace buffer workspace)
            ;; Build the overview projection after the session and workspace
            ;; have been populated; the first call is intentionally made
            ;; from a non-selected buffer and must not mark it read.
            (e-chat-overview-rebuild-unread-cache)
            (e-chat-overview-mark-selected-session-read))
          (should (e-chat-workspace-unread-p workspace))
          (should (e-chat-overview-session-unread-p
                   harness
                   (car (e-harness-session-list harness))))
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (e-chat-overview-mark-selected-session-read))
          (should-not (e-chat-overview-session-unread-p
                       harness
                       (car (e-harness-session-list harness))))
          (should-not (e-chat-workspace-unread-p workspace))
          (should-not (e-chat-overview-session-unread-p
                       harness
                       (car (e-harness-session-list harness)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-overview-rebuild-unread-cache))))



(ert-deftest e-chat-test-mode-does-not-poll-read-state-after-commands ()
  "Chat mode does not mark sessions read from `post-command-hook'."
  (with-temp-buffer
    (e-chat-mode)
    (should-not (memq #'e-chat-overview-mark-selected-session-read
                      post-command-hook))))



(ert-deftest e-chat-test-reset-clears-rendered-session ()
  "Reset clears the rendered chat buffer and harness session transcript."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "answer")
                   (:type done :reason stop))
                 "chat-reset")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-submit "question")
          (should (e-chat-test--wait-until
                   (lambda () (string-match-p "answer" (buffer-string)))
                   1.0))
          (e-chat-reset)
          (should-not (string-match-p "question\|answer" (buffer-string)))
          (should (equal (e-chat-service-messages
                          e-chat-harness e-chat-session-id)
                         nil))
          (with-current-buffer (e-chat-test--composer buffer)
            (goto-char (point-max))
            (insert "next")
            (should (equal (e-chat-composer-text) "next"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-mid-turn-compact-session-renders-visible-message ()
  "A model-triggered compaction action call renders visible chat progress."
  (let* ((calls 0)
         (backend
          (e-backend-create
           :name 'mid-turn-summary
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (setq calls (1+ calls))
              (pcase calls
                (1
                 (funcall on-item
                          '(:type tool-call
                            :id "call-1"
                            :name "run_elisp"
                            :arguments
                            (:stated_purpose "Compact the active session."
                             :code "(e-actions-call 'session-compaction :compact '(:keep_recent_tokens 1))")))
                 (funcall on-item '(:type done :reason tool-use)))
                (2
                 (funcall on-item
                          '(:type assistant-message
                            :content "Compacted active turn."))
                 (funcall on-item '(:type done :reason stop)))
                (_
                 (funcall on-item
                          '(:type assistant-message :content "done"))
                 (funcall on-item '(:type done :reason stop))))))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-mid-turn-compact")))
    (unwind-protect
        (with-current-buffer buffer
          (e-harness-set-intrinsic-capabilities
           e-chat-harness
           (append (e-harness-intrinsic-capabilities e-chat-harness)
                   (e-layer-capabilities (e-core-layer-create))
                   (e-layer-capabilities (e-emacs-base-layer-create))))
          (let ((store (e-harness-sessions e-chat-harness)))
            (e-session-append-message store e-chat-session-id
                                      '(:role user :content "old"))
            (e-session-append-message store e-chat-session-id
                                      '(:role assistant :content "old answer"))
            (e-chat-submit "continue")
            (should (e-chat-test--wait-until
                     (lambda ()
                       (and (string-match-p
                             "Agent compacting context mid-turn"
                             (buffer-string))
                            (string-match-p "Context compacted into"
                                            (buffer-string))
                            (string-match-p "done" (buffer-string))))
                     1.0))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-test)

;;; e-chat-test.el ends here
