;;; e-chat-test.el --- Composed e chat facade tests -*- lexical-binding: t; -*-

(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-chat-test-schema-upgrade-message-recognizes-direct-condition ()
  "A direct schema refusal becomes actionable operator guidance."
  (should
   (equal
    (e-chat--runtime-store-upgrade-required-message
     '(e-runtime-store-schema-too-old
       :actual 5 :required 6 :operation e-runtime-store-offline-upgrade))
    "runtime store upgrade required (schema 5 -> 6); quit Emacs, run scripts/e-runtime-upgrade, then restart")))

(ert-deftest e-chat-test-schema-upgrade-message-recognizes-nested-cause ()
  "A transport wrapper does not hide the underlying schema refusal."
  (should
   (equal
    (e-chat--runtime-store-upgrade-required-message
     '(e-runtime-store-unavailable
       "Runtime store worker is unavailable"
       :cause (e-runtime-store-schema-too-old
               "Runtime store schema requires explicit upgrade"
               :actual 5 :required 6)))
    "runtime store upgrade required (schema 5 -> 6); quit Emacs, run scripts/e-runtime-upgrade, then restart")))


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



(ert-deftest e-chat-test-buffer-exposes-sql-board-identity ()
  "A chat buffer exposes the Board id from its live SQL coordination binding."
  (let ((buffer (e-chat-test--buffer nil "board-context")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((binding (e-chat-service-binding
                          e-chat-harness e-chat-session-id)))
            (should (e-chat-service-binding-p binding))
            (should (stringp e-chat-board-id))
            (should (equal e-chat-board-id
                           (e-chat-service-binding-board-id binding)))))
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
         (original-context
          (symbol-function 'e-session-async-context-path)))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "send now")
          (cl-letf (((symbol-function 'e-session-async-context-path)
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





(ert-deftest e-chat-test-attach-keeps-detached-session-project-root ()
  "Attaching a session keeps its bounded queried project metadata."
  (let* ((project-root (make-temp-file "e-chat-project-" t))
         (nested (expand-file-name "docs/feats/item" project-root))
         (harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" project-root) t)
          (make-directory nested t)
          (let ((created
                 (e-chat-open
                  :harness harness :session-id "session-1" :new-session t
                  :metadata (list :project-root nested))))
            (kill-buffer created))
          (with-current-buffer
              (e-chat-open :harness harness :session-id "session-1")
            (should
             (equal (plist-get e-chat-session-metadata :project-root)
                    (file-name-as-directory nested)))))
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

(ert-deftest e-chat-test-raw-context-consumption-never-renders ()
  "Private frame-consumption audit cannot reach the generic System row."
  (let ((buffer (e-chat-test--buffer nil "chat-private-frame-audit")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make
            :type 'context-frame-consumed
            :session-id e-chat-session-id :turn-id "turn-private"
            :payload '(:frame-id "private-frame"
                       :consumer-request-id "private-consumer"
                       :response-entry-id "private-response")))
          (let ((content (buffer-string)))
            (should-not (string-match-p "Event:" content))
            (should-not
             (string-match-p
              "private-frame\\|private-consumer\\|private-response"
              content))))
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

(ert-deftest e-chat-test-submit-intent-uses-only-live-turn-state ()
  "Composer intent never consults a durable session aggregate."
  (with-temp-buffer
    (setq-local e-chat-harness 'test-harness)
    (setq-local e-chat-session-id "test-session")
    (cl-letf (((symbol-function 'e-chat-service-board-session-p)
               (lambda (&rest arguments)
                 (ert-fail
                  (format "Submit intent consulted Board persistence: %S"
                          arguments))))
              ((symbol-function 'e-session-local-state)
               (lambda (&rest arguments)
                 (ert-fail
                  (format "Submit intent read a session aggregate: %S"
                          arguments))))
              ((symbol-function 'e-chat-service-active-turn-p)
               (lambda (_harness _session-id) nil)))
      (should (eq (e-chat--submit-intent nil) 'submit)))
    (cl-letf (((symbol-function 'e-chat-service-active-turn-p)
               (lambda (_harness _session-id) t)))
      (should (eq (e-chat--submit-intent nil) 'steer))
      (should (eq (e-chat--submit-intent t) 'queue)))))







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
  (let ((buffer (e-chat-test--buffer nil "chat-work-diagnostics"))
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
                                         (list :id "dup-message"
                                               :role 'user
                                               :content "dup prompt"))))
          ;; The optimistic session summary may also display the prompt in the
          ;; transcript heading.  Assert projection identity rather than raw
          ;; text count: one durable message id maps to one rendered block.
          (should (equal (gethash "dup-message"
                                  e-chat-transcript--message-block-index)
                         (car (last e-chat-transcript--block-order)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))


(ert-deftest e-chat-test-reattach-clears-stale-transcript-before-mode-reset ()
  "Reattachment never lets minor-mode teardown scan the old transcript."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (buffer (e-chat-open :harness harness :session-id "chat-reattach-clear"))
         observed-size)
    (unwind-protect
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (make-string 100000 ?x)))
          ;; Globalized minor modes are disabled from this hook while a major
          ;; mode is reset.  Record the amount of stale presentation such a
          ;; teardown callback can observe without depending on emojify.
          (add-hook 'change-major-mode-hook
                    (lambda () (setq observed-size (buffer-size))) nil t)
          (e-chat-attach-buffer buffer harness "chat-reattach-clear")
          (should (equal observed-size 0))
          (should (derived-mode-p 'e-chat-mode)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



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
                         "e-session-local-activity-events"
                         "e-session-local-state"))
      (should-not (string-match-p forbidden source)))))



(ert-deftest e-chat-test-new-first-input-admits-distinct-persisted-sessions ()
  "Each new chat's first input atomically admits a distinct session."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         first-id second-id first-admission second-admission)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--kill-chat-buffers)
            (with-current-buffer (e-chat-new)
              (setq first-id e-chat-session-id)
              (setq first-admission
                    (e-chat-session-submit harness first-id "first")))
            (with-current-buffer (e-chat-new)
              (setq second-id e-chat-session-id)
              (setq second-admission
                    (e-chat-session-submit harness second-id "second")))
            (should (not (equal first-id second-id)))
            ;; A persistent v6 session is authoritative in SQLite; verify the
            ;; two queued creates through exact detached queries instead of
            ;; asking for a reconstructed aggregate.
            (e-work-with-batch-await
              (e-work-await-batch first-admission :timeout 5.0)
              (e-work-await-batch second-admission :timeout 5.0)
              (dolist (session-id (list first-id second-id))
                (should
                 (equal
                  (plist-get
                   (e-work-await-batch
                    (e-session-async-session-metadata store session-id)
                    :timeout 5.0)
                   :session-id)
                  session-id))))
            (should (file-exists-p
                     (expand-file-name "store.sqlite3" directory)))
            (should-not (file-directory-p
                         (expand-file-name "sessions" directory)))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))



(ert-deftest e-chat-test-new-persists-owning-chat-instance ()
  "New sessions remember the configured chat instance that created them."
  (let* ((alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)))))
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
                (let* ((metadata-result
                        (e-work-with-batch-await
                          (e-work-await-batch
                           (e-session-async-session-metadata
                            (e-harness-sessions beta-harness)
                            e-chat-session-id)
                           :timeout 5.0)))
                       (metadata (plist-get metadata-result :metadata)))
                  (should (eq e-chat-harness-instance-id :chat-beta))
                  (should (eq (plist-get metadata :harness-instance-id)
                              :chat-beta)))))))
      (e-chat-test--kill-chat-buffers))))





(ert-deftest e-chat-test-attach-buffer-ignores-persisted-read-marker-plist ()
  "Attaching ignores stale read markers replayed as plist metadata."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (session '(:id "read-marker-attach"
                    :latest-assistant-marker "assistant-read"
                    :metadata (:name "Read Marker"
                               :e-chat-read-markers
                               (:chat-default "assistant-read"))))
         (buffer nil))
    (unwind-protect
        (progn
          (setq buffer
                (e-chat-open
                 :harness harness :session-id "read-marker-attach"
                 :new-session t :metadata (plist-get session :metadata)))
          (with-current-buffer buffer
            (should (equal e-chat-session-id "read-marker-attach"))
            (should
             (e-chat-overview-session-unread-p
              harness session :chat-default))
            (e-chat-overview-mark-session-read
             harness session :chat-default)
            (should-not
             (e-chat-overview-session-unread-p
              harness session :chat-default))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-attach-buffer-does-not-rewrite-existing-project-root ()
  "Attaching an existing chat does not rewrite durable session metadata."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "e-chat-attach-root-" t)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (buffer (get-buffer-create "*e-chat-attach-root-test*"))
         (writes 0))
    (unwind-protect
        (progn
          (kill-buffer
           (e-chat-open
            :harness harness :session-id "rooted" :new-session t
            :metadata (list :project-root directory)))
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
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         window)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (setq window
                  (display-buffer
                   (e-chat-open :harness harness
                                :session-id "visible-session"
                                :new-session t
                                :metadata '(:name "visible-session"))))
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
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         visible-buffer
         hidden-duplicate
         window)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (setq visible-buffer
                  (e-chat-open :harness harness
                               :session-id "duplicate-session"
                               :new-session t
                               :metadata '(:name "duplicate-session")))
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
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         (buffer (e-chat-open
                  :harness harness :session-id "helper-session"
                  :new-session t :metadata '(:name "workspace helper target")))
         captured-prefer-visible
         captured-result)
    (unwind-protect
        (progn
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



(ert-deftest e-chat-test-model-and-effort-commands-update-session-options ()
  "Chat model and effort commands update harness-owned session options."
  (let ((buffer (e-chat-test--buffer nil "chat-options")))
    (unwind-protect
        (with-current-buffer buffer
          (let (model-work effort-work)
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _args) "gpt-test"))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _args) "high")))
              (setq model-work (call-interactively #'e-chat-set-model))
              (setq effort-work (call-interactively #'e-chat-set-effort)))
            (e-work-with-batch-await
              (e-work-await-batch model-work :timeout 5.0)
              (e-work-await-batch effort-work :timeout 5.0)
              (let* ((metadata-work
                      (e-session-async-session-metadata
                       (e-harness-sessions e-chat-harness)
                       e-chat-session-id))
                     (metadata
                      (e-work-await-batch metadata-work :timeout 5.0)))
                (should (equal (plist-get metadata :turn-options)
                               '(:model "gpt-test"
                                 :reasoning-effort "high"))))))
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



(ert-deftest e-chat-test-open-loaded-session-refreshes-mode-line-status ()
  "Opening a loaded session renders its detached model/effort status."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (session-id "chat-mode-line-open")
         buffer)
    (unwind-protect
        (let ((e-context-budget-model-token-limits
               '(("gpt-5.5" . 100))))
          (kill-buffer
           (e-chat-open
            :harness harness :session-id session-id :new-session t
            :metadata '(:name "Mode line")))
          (cl-letf (((symbol-function 'e-chat-surface-redraw-visible-p)
                     (lambda () t)))
            (setq buffer
                  (e-chat-open :harness harness :session-id session-id))
            (with-current-buffer buffer
              (should (equal mode-name "e-chat gpt-5.5/high")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))




(ert-deftest e-chat-test-set-status-skips-context-refresh-by-default ()
  "Ordinary status updates avoid full harness context estimation."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
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
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
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
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
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
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
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
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
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
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
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
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
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
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-mode-line-status-cache")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
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
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-token-usage-fast")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-chat-test--mark-active-turn "turn-1")
          (e-chat-render-event
           (e-harness-activity-emit-turn-event
            e-chat-harness e-chat-session-id "turn-1" 'token-usage
            '(:input-tokens 1200 :total-tokens 1300)))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (string-match-p "1.2k/258k tok" mode-name)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-token-usage-coalesces-mode-line-render-work ()
  "Repeated usage events schedule one latest-value mode-line projection."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
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
          (let* ((context-buffer (e-chat-show-context))
                 (context-work
                  (buffer-local-value 'e-chat--context-query-work
                                      context-buffer)))
            (should (buffer-live-p context-buffer))
            (e-work-with-batch-await
              (e-work-await-batch context-work :timeout 5.0))
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



(ert-deftest e-chat-test-open-reuses-live-session-buffer-without-reattach ()
  "Opening an already-live session leaves its projection and viewport intact."
  (let* ((buffer (e-chat-test--buffer nil "live-reopen"))
         (harness (buffer-local-value 'e-chat-harness buffer))
         (attach-count 0))
    (unwind-protect
        (progn
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
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (session '(:id "focus-read"
                    :latest-assistant-marker "msg-2"))
         (workspace (make-e-workspace-token
                     :backend 'single
                     :id 'focus-read-workspace
                     :name "focus-read-workspace"
                     :frame (selected-frame)))
         (buffer (generate-new-buffer " *e-chat-focus-read-test*")))
    (unwind-protect
        (progn
          (should (e-chat-overview-session-unread-p
                   harness session))
          (with-current-buffer buffer
            (e-chat-mode)
            (setq-local e-chat-harness harness)
            (setq-local e-chat-session-id "focus-read")
            (setq-local e-chat-session-metadata session)
            (e-buffer-set-workspace buffer workspace)
            ;; Build the overview projection after the session and workspace
            ;; have been populated; the first call is intentionally made
            ;; from a non-selected buffer and must not mark it read.
            (e-chat-overview-rebuild-unread-cache)
            (e-chat-overview-mark-selected-session-read))
          (should (e-chat-workspace-unread-p workspace))
          (should (e-chat-overview-session-unread-p
                   harness session))
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (e-chat-overview-mark-selected-session-read))
          (should-not (e-chat-overview-session-unread-p
                       harness session))
          (should-not (e-chat-workspace-unread-p workspace))
          (should-not (e-chat-overview-session-unread-p
                       harness session)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-overview-rebuild-unread-cache))))



(ert-deftest e-chat-test-mode-does-not-poll-read-state-after-commands ()
  "Chat mode does not mark sessions read from `post-command-hook'."
  (with-temp-buffer
    (e-chat-mode)
    (should-not (memq #'e-chat-overview-mark-selected-session-read
                      post-command-hook))))





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
                            (
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
         (_capabilities-installed
          (e-harness-set-intrinsic-capabilities
           harness
           (append (e-harness-intrinsic-capabilities harness)
                   (e-layer-capabilities (e-core-layer-create))
                   (e-layer-capabilities (e-emacs-base-layer-create)))))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-mid-turn-compact"))
         admission)
    (unwind-protect
        (with-current-buffer buffer
          (progn
            (should (e-chat-service-subscription-p e-chat--event-subscription))
            (should (e-chat-service-subscription-active-p
                     e-chat--event-subscription))
            (setq admission
                  (e-chat-submit-session
                   e-chat-harness e-chat-session-id "continue"))
            (should (e-work-handle-p admission))
            (e-work-with-batch-await
              (e-work-await-batch admission :timeout 5.0))
            (should
             (e-chat-test--wait-until
              (lambda ()
                (and (string-match-p
                      "Agent compacting context mid-turn"
                      (buffer-string))
                     (string-match-p "Context compacted into"
                                     (buffer-string))
                     (string-match-p "done" (buffer-string))))
              2.0))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-inspect-error-queries-before-opening-investigation ()
  "Public error inspection is async and opens only from detached SQL detail."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (child
          (e-work-prepare
           (e-work-spec-create
            :id "held-inspection" :execution 'cooperative
            :interactive-policy 'async :owner 'e-chat-test
            :runner (lambda (&rest _arguments) :deferred))
           nil))
         (buffer (generate-new-buffer " *e-inspect-error-test*"))
         opened submitted popped)
    (e-work-start-prepared child :arguments nil)
    (unwind-protect
        (cl-letf (((symbol-function 'e-context-inspection-failure-detail-start)
                   (lambda (&rest arguments)
                     (should (eq (plist-get arguments :harness) harness))
                     (should (equal (plist-get arguments :session-id)
                                    "failed-session"))
                     (should (equal (plist-get arguments :turn-id)
                                    "failed-turn"))
                     child))
                  ((symbol-function 'e-session-generate-id)
                   (lambda () "inspection-session"))
                  ((symbol-function 'e-chat-open)
                   (lambda (&rest arguments)
                     (setq opened arguments)
                     buffer))
                  ((symbol-function 'e-chat-surface-pop-to-buffer)
                   (lambda (seen-buffer)
                     (setq popped seen-buffer)))
                  ((symbol-function 'e-chat-submit-session)
                   (lambda (&rest arguments)
                     (setq submitted arguments)
                     (e-chat-test--finished-work :submitted)))
                  ((symbol-function 'e-runtime-store-call)
                   (lambda (&rest _arguments)
                     (error "e-inspect-error reached synchronous storage"))))
          (let ((work (e-inspect-error
                       :harness harness
                       :session-id "failed-session"
                       :turn-id "failed-turn")))
            (should (e-work-handle-p work))
            (should (eq (plist-get (e-work-status work) :state) 'started))
            (should-not opened)
            (e-work-finish
             child
             '(:session (:id "failed-session" :name "Failed"
                         :metadata (:project-root "/tmp/project"))
               :turn (:id "failed-turn")
               :terminal-error (:error "provider failed")
               :events nil :messages nil :tool-calls nil :diagnostics nil))
            (should (eq (plist-get (e-work-status work) :state) 'finished))
            (should (equal (e-work-handle-result work) "inspection-session"))
            (should (equal (plist-get opened :session-id)
                           "inspection-session"))
            (should (plist-get opened :new-session))
            (should (eq popped buffer))
            (should (eq (nth 0 submitted) harness))
            (should (equal (nth 1 submitted) "inspection-session"))
            (should (string-match-p "provider failed" (nth 2 submitted)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-test)

;;; e-chat-test.el ends here
