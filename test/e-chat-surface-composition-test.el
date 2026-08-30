;;; e-chat-surface-composition-test.el --- Public chat surface composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shell, window, workspace, activation, and display composition scenarios.

;;; Code:

(require 'ert)
(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-chat-test-delete-other-windows-keeps-atomic-surface ()
  "Native C-x 1 keeps both constituents and removes external windows."
  (let* ((configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-atom-delete-others"))
         (external-buffer (generate-new-buffer " *e-chat other external*"))
         transcript-window composer-window external-window)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq transcript-window (selected-window))
          (setq external-window (split-window transcript-window nil 'right))
          (set-window-buffer external-window external-buffer)
          (set-window-buffer transcript-window buffer)
          (with-current-buffer buffer
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t)))
          (with-selected-window composer-window
            (should (eq (key-binding (kbd "C-x 1"))
                        #'delete-other-windows))
            (call-interactively (key-binding (kbd "C-x 1"))))
          (should (window-live-p transcript-window))
          (should (window-live-p composer-window))
          (should-not (window-live-p external-window))
          (should (= (length (window-list nil 'nomini)) 2))
          (should (eq (window-atom-root transcript-window)
                      (window-atom-root composer-window))))
      (set-window-configuration configuration)
      (when (buffer-live-p external-buffer)
        (kill-buffer external-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-unrelated-pop-to-buffer-preserves-surface-windows ()
  "Generic pop-up display cannot replace either chat surface constituent."
  (let* ((configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-popup-window-ownership"))
         (popup (generate-new-buffer "*e-chat unrelated popup*"))
         transcript-window composer-window popup-window composer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq transcript-window (selected-window))
          (set-window-buffer transcript-window buffer)
          (with-current-buffer buffer
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t)))
          (setq composer (window-buffer composer-window))
          (pop-to-buffer popup)
          (setq popup-window (selected-window))
          (should (window-live-p transcript-window))
          (should (window-live-p composer-window))
          (should (eq (window-buffer transcript-window) buffer))
          (should (eq (window-buffer composer-window) composer))
          (should (eq (window-dedicated-p transcript-window) 'soft))
          (should (eq (window-dedicated-p composer-window) 'soft))
          (should (eq (window-atom-root transcript-window)
                      (window-atom-root composer-window)))
          (should-not (memq popup-window
                            (list transcript-window composer-window)))
          (should-not (window-atom-root popup-window)))
      (set-window-configuration configuration)
      (when (buffer-live-p popup)
        (kill-buffer popup))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-window-state-restoration-recovers-surface-pair ()
  "Writable window state restores the native atomic chat surface."
  (should (eq (cdr (assq 'window-atom window-persistent-parameters))
              'writable))
  (let* ((configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-pair-restoration"))
         (external-buffer (generate-new-buffer " *e-chat restore external*"))
         transcript-window composer-window external-window state composer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq transcript-window (selected-window))
          (setq external-window (split-window transcript-window nil 'right))
          (set-window-buffer external-window external-buffer)
          (set-window-buffer transcript-window buffer)
          (with-current-buffer buffer
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window)))
          (setq composer (window-buffer composer-window))
          (setq state (window-state-get (frame-root-window) t))
          (delete-window composer-window)
          (window-state-put state (frame-root-window) 'safe)
          (setq transcript-window (get-buffer-window buffer t))
          (setq composer-window (get-buffer-window composer t))
          (should (window-live-p transcript-window))
          (should (window-live-p composer-window))
          (should (eq (window-dedicated-p transcript-window) 'soft))
          (should (eq (window-dedicated-p composer-window) 'soft))
          (with-current-buffer buffer
            (should (eq (e-chat-surface-composer-window transcript-window)
                        composer-window)))
          (should (window-atom-root transcript-window))
          (should (eq (window-atom-root transcript-window)
                      (window-atom-root composer-window))))
      (set-window-configuration configuration)
      (when (buffer-live-p external-buffer)
        (kill-buffer external-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-post-command-preserves-scrollback-position ()
  "Plain post-command handling does not force readback back to the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-post-command"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (goto-char (point-min))
            (set-window-start window (point-min))
            (let ((before-point (point))
                  (before-start (window-start window)))
              (run-hooks 'post-command-hook)
              (should (= (point) before-point))
              (should (= (window-start window) before-start)))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-interactive-new-uses-selected-window-by-default ()
  "Interactive new chat opens in the selected window without a prefix argument."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         buffer
         selected-buffer)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test)
                (current-prefix-arg nil))
            (e-harness-registry-register :chat-test harness)
            (cl-letf (((symbol-function 'called-interactively-p)
                       (lambda (_kind) t))
                      ((symbol-function 'e-workspace-switch-to-buffer)
                       (lambda (display-buffer &rest _args)
                         (setq selected-buffer display-buffer)
                         display-buffer))
                      ((symbol-function 'e-workspace-pop-to-buffer)
                       (lambda (&rest _args)
                         (ert-fail "Default e-chat-new should not use pop display"))))
              (setq buffer (call-interactively #'e-chat-new))
              (should (eq selected-buffer buffer)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-interactive-new-with-prefix-uses-pop-display ()
  "Interactive new chat uses the pop display path with a prefix argument."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         buffer
         popped-buffer)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test)
                (current-prefix-arg '(4)))
            (e-harness-registry-register :chat-test harness)
            (cl-letf (((symbol-function 'called-interactively-p)
                       (lambda (_kind) t))
                      ((symbol-function 'e-workspace-switch-to-buffer)
                       (lambda (&rest _args)
                         (ert-fail "Prefix e-chat-new should not use switch display")))
                      ((symbol-function 'e-workspace-pop-to-buffer)
                       (lambda (display-buffer &rest _args)
                         (setq popped-buffer display-buffer)
                         display-buffer)))
              (setq buffer (call-interactively #'e-chat-new))
              (should (eq popped-buffer buffer)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-interactive-new-neutralizes-evil-after-display ()
  "Interactive display does not leave the chat buffer in Evil normal state."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         buffer)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (cl-letf (((symbol-function 'called-interactively-p)
                       (lambda (_kind) t))
                      ((symbol-function 'evil-local-mode)
                       (lambda (argument)
                         (setq-local evil-local-mode
                                     (not (and (numberp argument)
                                               (< argument 0))))
                         (unless evil-local-mode
                           (setq-local evil-state nil))))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (display-buffer &rest _args)
                         (setq buffer display-buffer)
                         (with-current-buffer display-buffer
                           (setq-local evil-local-mode t)
                           (setq-local evil-state 'normal)
                           (goto-char (point-min)))
                         display-buffer)))
              (setq buffer (e-chat-new))
              (with-current-buffer buffer
                (should-not evil-local-mode)
                (should-not evil-state))
              (with-current-buffer (e-chat-test--composer buffer)
                (should-not evil-local-mode)
                (should-not evil-state)
                (should (e-chat-composer-point-in-composer-p))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-reload-refreshes-every-visible-surface-instance ()
  "Reload protects every visible atom for one transcript buffer."
  (let* ((configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-reload-visible-surfaces"))
         first-transcript-window second-transcript-window composer windows)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq first-transcript-window (selected-window))
          (setq second-transcript-window
                (split-window first-transcript-window nil 'right))
          (set-window-buffer first-transcript-window buffer)
          (set-window-buffer second-transcript-window buffer)
          (with-current-buffer buffer
            (e-chat-surface-display-composer first-transcript-window)
            (e-chat-surface-display-composer second-transcript-window)
            (setq composer (e-chat-surface-composer-buffer)))
          (setq windows
                (append (get-buffer-window-list buffer nil t)
                        (get-buffer-window-list composer nil t)))
          (should (= (length windows) 4))
          (dolist (window windows)
            (set-window-dedicated-p window nil))
          (with-current-buffer buffer
            (e-chat-attach-buffer
             buffer e-chat-harness e-chat-session-id
             e-chat-harness-instance-id))
          (setq windows
                (append (get-buffer-window-list buffer nil t)
                        (get-buffer-window-list composer nil t)))
          (should (= (length windows) 4))
          (dolist (window windows)
            (should (eq (window-dedicated-p window) 'soft)))
          (dolist (transcript-window
                   (get-buffer-window-list buffer nil t))
            (with-current-buffer buffer
              (should (window-live-p
                       (e-chat-surface-composer-window
                        transcript-window))))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-after-display-active-turn-focuses-latest-output ()
  "Displaying a running chat tails to the active output, not stale scrollback."
  (let ((buffer (e-chat-test--buffer nil "chat-display-active-output"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 10))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 11))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (goto-char (point-min))
            (set-window-point window (point))
            (set-window-start window (point))
            (e-chat-surface-after-display-buffer buffer)
            (let ((latest-output-end (cdr (e-chat-surface-running-status-bounds)))
                  (output-position (e-chat-surface-output-follow-position)))
              (should latest-output-end)
              (should (= (point) output-position))
              (should (= (window-point window) output-position))
              (should (eq (window-buffer (selected-window))
                          (e-chat-test--composer buffer))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-after-display-settled-composed-chat-shows-latest-output ()
  "Displaying a settled composed chat shows its newest transcript output."
  (let* ((history (mapconcat (lambda (number)
                               (format "history line %d" number))
                             (number-sequence 1 300)
                             "\n"))
         (buffer (e-chat-test--buffer nil "chat-display-settled-output"))
         transcript-window composer-window)
    (unwind-protect
        (progn
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn "turn-1" 10 11 "question" history)
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t))
            ;; Selecting the composer changes `current-buffer'; continue the
            ;; viewport assertions in the owning transcript.
            (set-buffer buffer)
            (set-window-point transcript-window (point-min))
            (set-window-start transcript-window (point-min))
            (e-chat-surface-set-window-output-follow transcript-window nil)
            (should (< (window-point transcript-window) (point-max)))
            (e-chat-surface-after-display-buffer buffer)
            (should (eq (selected-window) composer-window))
            (should (= (window-point transcript-window) (point-max)))
            (should (e-chat-surface-window-follows-output-p transcript-window))))
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-evil-local-mode-hook-disables-reactivation ()
  "If Evil local mode is reactivated, chat mode turns it off again."
  (let ((buffer (e-chat-test--buffer nil "chat-evil-reactivation")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local evil-local-mode t)
          (setq-local evil-state 'normal)
          (run-hooks 'evil-local-mode-hook)
          (should-not evil-local-mode)
          (should-not evil-state))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-add-context-display-uses-source-workspace_below_selected ()
  "Displayed context insertion uses the source workspace, not stale chat affinity."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (source-workspace (make-e-workspace-token
                            :backend 'single
                            :id 'source
                            :name "source"
                            :frame (selected-frame)))
         (foreign-workspace (make-e-workspace-token
                             :backend 'single
                             :id 'foreign
                             :name "foreign"
                             :frame (selected-frame)))
         chat-buffer
         captured-buffer
         captured-workspace
         captured-action
         captured-select)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "workspace-session"
                              :metadata '(:name "workspace-session"))
            (setq chat-buffer
                  (e-chat-open :harness harness
                               :session-id "workspace-session"))
            (e-buffer-set-workspace chat-buffer foreign-workspace)
            (with-temp-buffer
              (insert "alpha
beta
gamma
")
              (let ((reference (e-chat-composer-capture-context-reference-for-command)))
                (cl-letf (((symbol-function 'e-workspace-display-buffer)
                           (cl-function
                            (lambda (buffer &key workspace action select
                                            side-window-ok)
                              (ignore side-window-ok)
                              (setq captured-buffer buffer)
                              (setq captured-workspace workspace)
                              (setq captured-action action)
                              (setq captured-select select)
                              (selected-window)))))
                  (should (eq (e-chat-add-context-reference-to-session
                               reference
                               harness
                               "workspace-session"
                               t
                               nil
                               source-workspace)
                              chat-buffer)))))
            (should (eq captured-buffer chat-buffer))
            (should (eq captured-workspace source-workspace))
            (should captured-select)
            (should (memq 'display-buffer-below-selected captured-action))
            (should-not (memq 'display-buffer-use-some-window captured-action))))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-rename-updates-session-and-buffer-display ()
  "Renaming updates persistent metadata and the attached buffer name."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer (e-chat-open :harness harness :session-id "rename-me")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) "Renamed session")))
            (call-interactively #'e-chat-rename))
          (should (equal (e-session-display-title store "rename-me")
                         "Renamed session"))
          (should (string-match-p "Renamed session" (buffer-name)))
          (should (string-match-p "Renamed session" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-chat-test-derived-title-updates-attached-buffer-display ()
  "Derived session titles refresh attached presentation surfaces."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer (e-chat-open :harness harness :session-id "derived-title"))
         (prompt "Derived title update"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (e-chat-submit prompt)
            (should (e-chat-test--wait-until
                     (lambda ()
                       (and (equal (e-session-display-title
                                    store "derived-title")
                                   prompt)
                            (string-match-p (regexp-quote prompt)
                                            (buffer-name))
                            (string-match-p (regexp-quote prompt)
                                            header-line-format)
                            (string-match-p
                             (regexp-quote prompt)
                             (buffer-substring-no-properties
                              (point-min) (min (point-max) 160)))))
                     1.0))
            (let ((title (e-session-display-title store "derived-title"))
                  (text (buffer-substring-no-properties
                         (point-min)
                         (min (point-max) 160))))
              (should (equal title prompt))
              (should (string-match-p (regexp-quote title) (buffer-name)))
              (should (string-match-p (regexp-quote title)
                                      header-line-format))
              (should (string-match-p (regexp-quote title) text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-mode-line-window-uses-configured-limit ()
  "The mode line ignores a larger provider maximum when e has a lower limit."
  (let ((e-context-budget-model-token-limits
         '(("gpt-5.6-sol" . 353400)))
        (provider-calls 0))
    (cl-letf (((symbol-function 'e-anthropic-context-window)
               (lambda (_model)
                 (setq provider-calls (1+ provider-calls))
                 1050000)))
      (should (equal (e-context-status-model-token-limit "gpt-5.6-sol")
                     353400))
      (should (= provider-calls 0)))))

(ert-deftest e-chat-test-mode-line-status-unknown-window-shows-question-mark ()
  "When no model limit is configured, the mode line shows `?'."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness :session-id "chat-mode-line-unknown")))
    (unwind-protect
        (let ((e-context-budget-model-token-limits nil))
          (with-current-buffer buffer
            (e-chat-surface-set-redraw-visible t)
            (e-session-append-message
             store e-chat-session-id '(:role user :content "q"))
            (e-chat-surface-set-status "idle" t)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)
                                     :owner 'chat-mode-line-status))
            (should (string-match-p "gpt-5.5/high" mode-name))
            (should (string-match-p "/? tok" mode-name))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-shell-descriptor-advertises-chat-surface ()
  "The chat presentation publishes a generic shell manifest."
  (let* ((shell (e-chat-shell))
         (command-ids (mapcar #'e-shell-command-id
                              (e-shell-commands shell)))
         (keymaps (e-shell-keymaps shell)))
    (should (eq (e-shell-id shell) 'chat))
    (should (equal (e-shell-required-capabilities shell) '(chat-session)))
    (dolist (command-id '(new
                          resume
                          switch-session
                          active-sessions
                          overview
                          sidebar-toggle
                          overview-close
                          rename
                          set-model
                          set-effort
                          show-context
                          submit
                          abort
                          reset
                          enter-response-navigation
                          response-navigation-next
                          response-navigation-previous
                          response-navigation-activate
                          response-navigation-copy
                          response-navigation-open
                          response-navigation-details
                          response-navigation-insert
                          open-latest-response
                          copy-latest-response
                          inspect-error
                          add-context-to-latest
                          add-context-to-session))
      (should (memq command-id command-ids)))
    (should (eq (plist-get (car keymaps) :keymap) e-chat-mode-map))
    (let ((context-keymap (cl-find 'context keymaps
                                   :key (lambda (entry)
                                          (plist-get entry :id)))))
      (should (eq (plist-get context-keymap :keymap) e-chat-context-mode-map))
      (should (eq (plist-get context-keymap :scope) 'global))
      (should (eq (plist-get context-keymap :mode) 'e-chat-context-mode)))
    (should (eq (plist-get (cl-find 'response-navigation keymaps
                                    :key (lambda (entry)
                                           (plist-get entry :id)))
                           :keymap)
                e-chat-response-navigation-mode-map))))

(ert-deftest e-chat-test-installs-surface-activation-hooks ()
  "Chat activation follows selection and settled window-buffer changes."
  (cl-progv '(window-selection-change-functions
              window-configuration-change-hook
              window-buffer-change-functions
              buffer-list-update-hook
              minibuffer-exit-hook
              persp-activated-functions)
      '(nil nil nil nil nil nil)
    (e-chat-startup)
    (should (memq #'e-chat-overview-mark-selected-session-read
                  window-selection-change-functions))
    (should (memq #'e-chat-surface-activate-selected
                  window-selection-change-functions))
    (should-not (memq #'e-chat-surface-activate-selected
                      window-configuration-change-hook))
    (should
     (memq #'e-chat-surface-activate-after-window-change
           window-buffer-change-functions))
    (should (memq #'e-chat-activity-flush-deferred-redraws-after-minibuffer
                  minibuffer-exit-hook))
    (should (memq #'e-chat-overview-mark-selected-session-read
                  persp-activated-functions))
    (should-not (memq #'e-chat-surface-activate-selected
                      window-configuration-change-hook))))

(ert-deftest e-chat-test-open-from-side-window-uses-normal-window ()
  "Opening a session from a side window displays in a normal window.
Regression: from the overview sidebar (a side window) `pop-to-buffer' tried
to split the side window and signalled \"Cannot split side window or parent of
side window\", which blocked opening new sessions."
  (let* ((chat (e-chat-test--buffer nil "chat-side-window"))
         (sidebar (get-buffer-create "*e-chat-test-sidebar*"))
         (side-window
          (display-buffer-in-side-window sidebar '((side . left) (slot . -1)))))
    (unwind-protect
        (progn
          (should (window-live-p side-window))
          (select-window side-window)
          (should (e-chat-surface-side-window-p))
          ;; Must not error, and must not try to host the chat in the side
          ;; window.
          (e-chat-surface-pop-to-buffer chat)
          (let ((shown (get-buffer-window chat t)))
            (should (window-live-p shown))
            (should-not (window-parameter shown 'window-side)))
          ;; The same-window path is side-window safe too.
          (select-window side-window)
          (e-chat-surface-switch-to-buffer chat)
          (let ((shown (get-buffer-window chat t)))
            (should (window-live-p shown))
            (should-not (window-parameter shown 'window-side))))
      (when (window-live-p side-window)
        (delete-window side-window))
      (when (buffer-live-p sidebar)
        (kill-buffer sidebar))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

(ert-deftest e-chat-test-open-when-every-window-is-a-side-window ()
  "Display creates a normal window when every window is a side window.
Regression: a frame whose only windows are side popups has nowhere to split,
so display must split the frame root to make an ordinary window rather than
signalling \"Cannot split side window or parent of side window\" -- and rather
than commandeering the side window in place, which would leave the frame with
no main window and break ordinary commands like \\[split-window-right].  An
all-side-window frame cannot be built in batch (Emacs refuses to delete the
last normal window), so the no-normal-window condition is stubbed."
  (let* ((chat (e-chat-test--buffer nil "chat-all-side"))
         (sidebar (get-buffer-create "*e-chat-test-only-side*"))
         (side-window
          (display-buffer-in-side-window sidebar '((side . left) (slot . -1)))))
    (unwind-protect
        (cl-letf (((symbol-function 'e-chat-surface-non-side-window) (lambda (&rest _) nil)))
          (should (window-live-p side-window))
          (select-window side-window)
          (should (e-chat-surface-side-window-p))
          ;; Must not error; must create a fresh normal (non-side) window.
          (e-chat-surface-pop-to-buffer chat)
          (let ((shown (get-buffer-window chat t)))
            (should (window-live-p shown))
            (should-not (window-parameter shown 'window-side))
            ;; The side window keeps its own buffer; it was not commandeered.
            (should (eq (window-buffer side-window) sidebar))))
      (when (window-live-p side-window)
        (delete-window side-window))
      (when (buffer-live-p sidebar)
        (kill-buffer sidebar))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

(ert-deftest e-chat-test-new-prompts-for-chat-instance-when-multiple-exist ()
  "New chat selection opens the chosen chat harness instance."
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
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (cl-find-if
                          (lambda (candidate)
                            (string-match-p "Beta Target" candidate))
                          (all-completions "" collection)))))
              (with-current-buffer (e-chat-new)
                (should (eq e-chat-harness beta-harness))
                (should (eq e-chat-harness-instance-id :chat-beta))
                (should (= (length (e-harness-session-list beta-harness)) 1))
                (should (= (length (e-harness-session-list alpha-harness)) 0))))))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-open-prunes-hidden-empty-duplicate-session-buffer ()
  "Opening a session removes hidden empty duplicate buffers for that session."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         visible-buffer
         hidden-duplicate
         window)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "dedupe-session"
                            :metadata '(:name "dedupe-session"))
          (setq visible-buffer
                (e-chat-open :harness harness :session-id "dedupe-session"))
          (setq window (display-buffer visible-buffer))
          (setq hidden-duplicate
                (get-buffer-create "*e-chat:hidden dedupe-session*"))
          (e-chat-attach-buffer hidden-duplicate harness "dedupe-session" nil)
          (should (buffer-live-p hidden-duplicate))
          (should (eq (e-chat-open :harness harness
                                   :session-id "dedupe-session")
                      visible-buffer))
          (should-not (buffer-live-p hidden-duplicate)))
      (when (and window (window-live-p window))
        (delete-window window))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-active-steering-stores-pending-input ()
  "Plain active steering stores pending input and clears the composer."
  (let* ((backend (e-backend-create
                   :name "held-chat"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore messages options on-item on-done
                                     on-error on-request-start)
                             nil))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-steer-pending")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "first")
          (let ((turn-id (e-chat-submit)))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (e-chat-service-active-turn-p
                        harness e-chat-session-id))
                     1.0))
            (goto-char (point-max))
            (insert "focus here")
            (should (equal (e-chat-submit) turn-id))
            (should (equal (e-chat-composer-text) ""))
            (should (string-match-p
                     "steered"
                     (format "%s"
                             (buffer-local-value 'header-line-format buffer))))
            (should
             (e-chat-test--wait-until
              (lambda ()
                (let ((entry (gethash e-chat-session-id
                                      (e-harness-active-turns harness))))
                  (when-let ((item (car (e-harness-turn-state-pending-steering
                                         entry))))
                    (and (equal (plist-get item :prompt) "focus here")
                         (eq (plist-get (plist-get item :metadata)
                                        :submit-mode)
                             'steering)))))
              1.0))))
      (when (buffer-live-p buffer)
        (with-current-buffer (e-chat-test--composer buffer)
          (ignore-errors
            (e-harness-test-abort e-chat-harness e-chat-session-id)))
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-hides-empty-output-diagnostic ()
  "The chat buffer renders empty backend output as an error."
  (let ((buffer (e-chat-test--buffer
                 '((:type done :reason stop))
                 "chat-empty-output")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-submit "hello")
          (should (e-chat-test--wait-until
                   (lambda ()
                     (string-match-p "Backend returned no assistant output"
                                     (buffer-string)))
                   1.0))
          (should-not (string-match-p "✅ Done" (buffer-string)))
          (should-not (seq-some
                       (lambda (message)
                         (eq (plist-get message :role) 'assistant))
                       (e-chat-service-messages
                        e-chat-harness e-chat-session-id)))
          (should (string-match-p "E Chat: error" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-provider-round-trip-renders-thinking-and-thought ()
  "Provider request boundaries render active and completed thinking lines."
  (let ((buffer (e-chat-test--buffer nil "chat-provider-round-trip")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started)))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          (should (string-match-p "⠋ Thinking for 0min 8sec"
                                  (buffer-string)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 63
                          :payload '(:status done)))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p "Thought for 1min 3sec" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-durable-entry-stays-above-stale-running-status ()
  "Durable entries stay above visible transient output with no progress owner."
  (let ((buffer (e-chat-test--buffer nil "chat-stale-running-order")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-chat-render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:content "thought")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (e-chat-activity-stop-progress "turn-1")
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:message (:role user
                                                :content "continue"))))
          (e-chat-render-event
           (e-events-make :type 'compaction-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:reason auto)))
          (let ((user-pos (save-excursion
                            (goto-char (point-min))
                            (search-forward "continue")))
                (system-pos (save-excursion
                              (goto-char (point-min))
                              (search-forward "Auto-compaction started")))
                (thought-pos (save-excursion
                               (goto-char (point-min))
                               (search-forward "thought"))))
            (should (< user-pos system-pos))
            (should (< system-pos thought-pos))
            (with-current-buffer (e-chat-test--composer buffer)
              (should (e-chat-composer-active-p)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-provider-anchor-candidates-do-not-render-transcript-events ()
  "Provider anchor candidates are internal cache state, not chat transcript text."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-provider-anchor-event")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make
            :type 'provider-anchor-candidate
            :session-id e-chat-session-id
            :turn-id "turn-1"
            :payload '(:type provider-anchor-candidate
                       :provider-id openai
                       :metadata (:response-id "resp_123"))))
          (should-not (string-match-p "provider-anchor-candidate"
                                      (buffer-string)))
          (should-not (string-match-p "Event:" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-surface-composition-test)

;;; e-chat-surface-composition-test.el ends here
