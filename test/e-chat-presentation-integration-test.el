;;; e-chat-presentation-integration-test.el --- Composed presentation integration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise the composed e-chat facade, its owner transitions, and
;; graphical/window behavior.  Owner contract tests live in the corresponding
;; e-chat-*-test.el files and load only their minimal collaborators.

;;; Code:

(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-chat-test-composed-surface-keeps-draft-outside-transcript ()
  "Transcript rendering must not recreate or alter the separate composer."
  (let* ((buffer (e-chat-test--buffer nil "chat-composed-surface")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((composer (e-chat-surface-composer-buffer))
                (draft "keep this unsent draft")
                composer-tick)
            (should (buffer-live-p composer))
            (should (eq (e-chat-surface-transcript-buffer composer)
                        buffer))
            (with-current-buffer composer
              (goto-char (point-max))
              (insert draft)
              (setq composer-tick (buffer-chars-modified-tick)))
            (e-chat-render-event
             (e-events-make :type 'message-added
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:message (:role user
                                                  :content "render this"))))
            (should-not (string-match-p (regexp-quote (e-chat-composer-glyph))
                                        (buffer-string)))
            (should (string-match-p "render this" (buffer-string)))
            (with-current-buffer composer
              (should (equal (e-chat-composer-text) draft))
              (should (= (buffer-chars-modified-tick) composer-tick)))
            (e-chat-composer-insert-context-reference
             '(:uri "file:///tmp/context" :label "context" :text "source"))
            (with-current-buffer composer
              (should (string-match-p "@\\[context\\]"
                                      (e-chat-composer-text))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-composer-window-cycle-skips-transcript ()
  "C-x o treats a composed transcript and composer as one chat surface."
  (let* ((buffer (e-chat-test--buffer nil "chat-composer-window-cycle"))
         transcript-window composer-window external-window external-buffer)
    (unwind-protect
        (progn
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t))
            (setq external-window (split-window composer-window nil 'right))
            (setq external-buffer (generate-new-buffer " *e-chat external*"))
            (set-window-buffer external-window external-buffer)
            (select-window composer-window)
            (with-current-buffer (window-buffer composer-window)
              (call-interactively (key-binding (kbd "C-x o"))))
            (should (eq (selected-window) external-window))
            (should-not (eq (selected-window) transcript-window))))
      (when (window-live-p external-window)
        (delete-window external-window))
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p external-buffer)
        (kill-buffer external-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-delete-composer-window-closes-surface-pair ()
  "Native C-x 0 in the composer closes the complete atomic surface."
  (let* ((configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-pair-delete"))
         (external-buffer (generate-new-buffer " *e-chat pair external*"))
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
          (should (eq (window-atom-root transcript-window)
                      (window-atom-root composer-window)))
          (with-selected-window composer-window
            (should (eq (key-binding (kbd "C-x 0"))
                        #'delete-window))
            (call-interactively (key-binding (kbd "C-x 0"))))
          (should-not (window-live-p transcript-window))
          (should-not (window-live-p composer-window))
          (should (window-live-p external-window))
          (should (eq (selected-window) external-window))
          (should-not (get-buffer-window buffer t)))
      (set-window-configuration configuration)
      (when (buffer-live-p external-buffer)
        (kill-buffer external-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



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



(ert-deftest e-chat-test-split-from-composer-keeps-external-window ()
  "A surface split replaces its transient unpaired composer view."
  (let* ((configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-pair-split"))
         (replacement (generate-new-buffer "*e-chat split replacement*"))
         (workspace (make-e-workspace-token
                     :backend 'single :id 'single :name "single"
                     :frame (selected-frame)))
         transcript-window composer-window external-window composer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq transcript-window (selected-window))
          (set-window-buffer transcript-window buffer)
          (with-current-buffer buffer
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t)))
          (setq composer (window-buffer composer-window))
          (should (eq (window-atom-root transcript-window)
                      (window-atom-root composer-window)))
          (cl-letf (((symbol-function 'e-workspace-current)
                     (lambda (&optional _frame) workspace))
                    ((symbol-function 'e-workspace-buffer-member-p)
                     (lambda (candidate _workspace)
                       (eq candidate replacement)))
                    ((symbol-function 'e-workspace-add-buffer)
                     (lambda (&rest _arguments)
                       (ert-fail "split should reuse a workspace buffer"))))
            (with-selected-window composer-window
              (should (eq (key-binding (kbd "C-x 3"))
                          #'e-chat-surface-split-window-right))
              (setq external-window
                    (call-interactively (key-binding (kbd "C-x 3"))))))
          (should (window-live-p external-window))
          (should (eq (window-buffer external-window) replacement))
          (should (eq (window-dedicated-p transcript-window) 'soft))
          (should (eq (window-dedicated-p composer-window) 'soft))
          (should-not (window-dedicated-p external-window))
          (should-not (window-atom-root external-window))
          (with-current-buffer buffer
            (should (eq (e-chat-surface-composer-window transcript-window)
                        composer-window)))
          (should (window-live-p external-window))
          (should (= (length (window-list nil 'nomini)) 3))
          (should-not (eq (window-buffer external-window) composer)))
      (set-window-configuration configuration)
      (when (buffer-live-p replacement)
        (kill-buffer replacement))
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



(ert-deftest e-chat-test-after-display-clears-chat-navigation-modes ()
  "Displaying chat returns it to a plain composer input state."
  (let ((buffer (e-chat-test--buffer nil "chat-display-input-state")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (e-chat-tool-list-mode 1)
          (e-chat-after-display-buffer buffer)
          (should-not e-chat-tool-list-mode)
          (should-not e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-point-in-composer-p))))
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



(ert-deftest e-chat-test-loaded-session-reprojection-does-not-tail-scrollback ()
  "Session replay does not tail a transcript physically showing scrollback."
  (let* ((history (mapconcat (lambda (number)
                               (format "settled history line %d" number))
                             (number-sequence 1 300)
                             "\n"))
         (buffer (e-chat-test--buffer nil "chat-loaded-scrollback"))
         transcript-window
         composer-window)
    (unwind-protect
        (progn
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (let ((store (e-chat-service-session-store e-chat-harness)))
              (e-session-append-message
               store e-chat-session-id
               '(:id "msg-1" :role user :content "loaded question"))
              (e-session-append-message
               store e-chat-session-id
               `(:id "msg-2" :role assistant :content ,history))
              (e-chat-test--seed-board-log-from-private-fixture
               e-chat-harness e-chat-session-id))
            (e-chat-attach-buffer
             buffer e-chat-harness e-chat-session-id
             e-chat-harness-instance-id)
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t))
            (set-buffer buffer)
            (e-chat-surface-show-latest-output transcript-window)
            (should (e-chat-surface-window-follows-output-p transcript-window))
            ;; `scroll-other-window' and host restoration can move the paired
            ;; transcript while leaving its composer selected.  The stored
            ;; live-output flag is intentionally not consulted by a full
            ;; projection replacement; the physical pre-replay viewport is
            ;; the complete fact that operation needs.
            (set-window-point transcript-window (point-min))
            (set-window-start transcript-window (point-min))
            (redisplay t)
            (should (eq (selected-window) composer-window))
            (should (= (window-point transcript-window) (point-min)))
            (e-chat-attach-buffer
             buffer e-chat-harness e-chat-session-id
             e-chat-harness-instance-id)
            (should (< (window-point transcript-window) (point-max)))
            (should (= (window-start transcript-window) (point-min)))))
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



(ert-deftest e-chat-test-progress-rerender-touches-only-active-tail ()
  "Progress redraw leaves completed activity rounds physically untouched."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-active-tail")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-test--mark-active-turn "turn-1")
          (dotimes (index 24)
            (let ((started-at (* index 2)))
              (e-chat-render-event
               (e-events-make :type 'provider-request-started
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :created-at started-at
                              :payload '(:status started)))
              (unless (= index 23)
                (e-chat-render-event
                 (e-events-make :type 'provider-request-finished
                                :session-id e-chat-session-id
                                :turn-id "turn-1"
                                :created-at (1+ started-at)
                                :payload '(:status done))))))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)))
          (let* ((tail-start (car (e-chat-surface-running-status-bounds)))
                 (stable-prefix
                  (buffer-substring-no-properties (point-min) tail-start))
                 changes)
            (add-hook 'before-change-functions
                      (lambda (start end)
                        (push (cons start end) changes))
                      nil t)
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional _time) 60.0)))
              (e-chat-activity-advance-progress)
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer (current-buffer))))
            (should changes)
            (should (cl-every (lambda (change)
                                (>= (car change) tail-start))
                              changes))
            (should (equal stable-prefix
                           (buffer-substring-no-properties
                            (point-min) tail-start)))
            (should (string-match-p "Thinking for 0min 14sec"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-replayed-stale-provider-activity-stays-off-tail ()
  "Replayed non-terminal provider activity is hidden when the turn is not active."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-chat-test--create-session
           store :id "chat-provider-stale-replay")
          (e-session-append-message
           store "chat-provider-stale-replay"
           '(:role user :content "inspect" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-provider-stale-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-provider-stale-replay" "turn-1" 'provider-request-started
           '(:status started))
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-provider-stale-replay"))
          (with-current-buffer buffer
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (let ((content (buffer-string)))
              (should (string-match-p "inspect" content))
            (should-not (string-match-p "Thinking for" content)))
            (should-not (e-chat-activity-progress-turn-id))
            (should-not (plist-get (e-chat-activity-progress-state)
                                   :interval-active-p))
            (with-current-buffer (e-chat-test--composer buffer)
              (should (e-chat-composer-active-p)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-progress-rerender-preserves-scrollback-focus ()
  "Progress redraws preserve point and window focus when reading scrollback."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-scroll-focus"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-2"
                            :created-at 20))
            (e-chat-test--mark-active-turn "turn-2")
            (with-current-buffer (e-chat-test--composer buffer)
              (goto-char (point-max))
              (insert "follow-up draft"))
            (goto-char (point-min))
            (set-window-point window (point))
            (set-window-start window (point))
            (let ((before-point (point))
                  (before-window-point (window-point window))
                  (before-window-start (window-start window)))
              (e-chat-activity-advance-progress)
              (should (= (point) before-point))
              (should (= (window-point window) before-window-point))
              (should (= (window-start window) before-window-start))
              (with-current-buffer (e-chat-test--composer buffer)
                (should (e-chat-composer-active-p))
                (should (equal (e-chat-composer-text)
                               "follow-up draft"))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-activity-rerender-keeps-running-status-tail ()
  "Activity redraws keep following output when focus was at the active tail."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-status-tail"))
        (window nil)
        (composer-window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (setq composer-window
                (with-current-buffer buffer
                  (e-chat-surface-display-composer window t)))
          (select-window composer-window)
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
            (e-chat-render-event
             (e-events-make :type 'reasoning-delta
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:content "first chunk")))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (e-chat-surface-show-latest-output)
            (e-chat-surface-set-window-output-follow window t)
            (let ((old-tail (cdr (e-chat-surface-running-status-bounds)))
                  (old-output (e-chat-surface-output-follow-position)))
              (should old-tail)
              (should (= (window-point window) old-output))
              (e-chat-render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nsecond chunk")))
              (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
              (let ((new-tail (cdr (e-chat-surface-running-status-bounds)))
                    (new-output (e-chat-surface-output-follow-position)))
                (should new-tail)
                (should (> new-tail old-tail))
                (should (= (window-point window) new-output))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-deferred-activity-redraw-preserves-scrollback-focus ()
  "Deferred activity redraw keeps point and window focus in scrollback."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-scroll-focus"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-2"
                            :created-at 20))
            (e-chat-render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-2"
                            :created-at 21
                            :payload '(:type tool-call
                                        :id "call-1"
                                        :name "read")))
            (goto-char (point-min))
            (set-window-point window (point))
            (set-window-start window (point))
            (e-chat-surface-set-window-output-follow window nil)
            (let ((before-window-point (window-point window))
                  (before-window-start (window-start window)))
              (e-chat-activity-run-pending-redraw)
              (should (= (window-point window) before-window-point))
              (should (= (window-start window) before-window-start))
              (should (string-match-p "1 tool call" (buffer-string))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-response-navigation-details-shows-intermittent-events ()
  "Details buffer shows intermittent reasoning before metadata."
  (let ((buffer (e-chat-test--buffer nil "chat-intermittent-expand")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type reasoning-delta
                                      :content "Need current buffer state.")))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (e-chat-render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:reason stop)))
          (call-interactively #'e-chat-enter-response-navigation)
          (let ((details (e-chat-response-navigation-details)))
            (with-current-buffer details
              (should (string-match-p
                       "Reasoning\n  Need current buffer state\\.\n\n  Turn: turn-1"
                       (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))



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



(ert-deftest e-chat-test-active-session-preview-renders-index-session-tail ()
  "Active-session preview renders a loaded index session through the chat path."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         candidate)
    (e-chat-test--create-session store :id "indexed-active"
                      :metadata '(:name "Indexed active"))
    (e-session-append-message
     store "indexed-active"
     '(:id "msg-1" :role user :content "first prompt"))
    (e-session-append-message
     store "indexed-active"
     '(:id "msg-2" :role assistant :content "first response"))
    (e-session-append-message
     store "indexed-active"
     '(:id "msg-3" :role user :content "last prompt"))
    (e-session-append-message
     store "indexed-active"
     '(:id "msg-4" :role assistant :content "last response"))
    (setq candidate
          (list :harness harness
                :session (car (e-harness-session-list harness))
                :session-id "indexed-active"))
    (let ((e-chat-resume-preview-message-limit 2))
      (with-temp-buffer
        (e-chat-overview-active-session-preview candidate (current-buffer))
        (let ((text (buffer-string)))
          (should-not (string-match-p "first prompt" text))
          (should-not (string-match-p "first response" text))
          (should (string-match-p "last prompt" text))
          (should (string-match-p "last response" text)))))))



(ert-deftest e-chat-test-open-loaded-session-renders-initial-tail ()
  "Opening a large loaded session renders a tail plus omitted-history marker."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-chat-session-replay-message-limit 2)
         buffer)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "loaded-tail"
                            :metadata '(:name "Loaded tail"))
          (dolist (message
                   '((:id "msg-1" :role user :content "first prompt")
                     (:id "msg-2" :role assistant :content "first response")
                     (:id "msg-3" :role user :content "middle prompt")
                     (:id "msg-4" :role user :content "last prompt")
                     (:id "msg-5" :role assistant :content "last response")))
            (e-session-append-message store "loaded-tail" message))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "loaded-tail")
          (setq buffer (e-chat-open-session harness "loaded-tail"))
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              ;; A loaded session renders directly; the observable contract
              ;; is that no asynchronous loading placeholder remains.
              (should-not (string-match-p "Loading transcript" text))
              (should (string-match-p
                       "3 earlier transcript messages omitted" text))
              (should-not (string-match-p "first prompt" text))
              (should-not (string-match-p "middle prompt" text))
              (should (string-match-p "last prompt" text))
              (should (string-match-p "last response" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



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

(ert-deftest e-chat-test-killing-duplicated-transcript-tears-down-composer ()
  "Killing a multiply displayed transcript safely tears down its atom."
  (let* ((configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-pair-kill"))
         first-transcript-window second-transcript-window composer-window
         composer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq first-transcript-window (selected-window))
          (set-window-buffer first-transcript-window buffer)
          (setq second-transcript-window
                (split-window first-transcript-window nil 'right))
          (set-window-buffer second-transcript-window buffer)
          (with-current-buffer buffer
            (setq composer-window
                  (e-chat-surface-display-composer
                   second-transcript-window)))
          (setq composer (window-buffer composer-window))
          (should (= (length (get-buffer-window-list buffer nil t)) 2))
          (should (eq (window-dedicated-p composer-window) 'soft))
          (kill-buffer buffer)
          (setq buffer nil)
          (should-not (buffer-live-p composer))
          (dolist (window (list first-transcript-window
                                second-transcript-window
                                composer-window))
            (when (window-live-p window)
              (should-not (window-atom-root window)))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-composer-navigation-routes-keys-to-transcript ()
  "Composer navigation selects the transcript and activates its keymap."
  (let* ((buffer (e-chat-test--buffer nil "chat-composer-navigation"))
         transcript-window composer-window details-buffer)
    (unwind-protect
        (save-window-excursion
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn
             "turn-1" 10 11 "question" "answer")
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t)))
          (with-current-buffer (window-buffer composer-window)
            (call-interactively #'e-chat-composer-enter-navigation))
          (should (eq (selected-window) transcript-window))
          (should (eq (window-buffer transcript-window) buffer))
          (with-current-buffer buffer
            (should e-chat-response-navigation-mode)
            (should (eq (key-binding (kbd "RET"))
                        #'e-chat-response-navigation-activate))
            (should (eq (key-binding (kbd "d"))
                        #'e-chat-response-navigation-details))
            (setq details-buffer
                  (call-interactively (key-binding (kbd "d"))))
            (should (buffer-live-p details-buffer))
            (call-interactively (key-binding (kbd "RET")))
            (should e-chat-block-view-mode)
            (should-not e-chat-response-navigation-mode)))
      (when (buffer-live-p details-buffer)
        (kill-buffer details-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-open-creates-protected-transcript-and-composer ()
  "Opening chat creates protected transcript text and editable composer text."
  (let ((buffer (e-chat-test--buffer nil "chat-open")))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'e-chat-mode))
          (goto-char (point-min))
          (should (looking-at-p (regexp-quote "E Agent Session")))
          (should (eq (get-text-property (point-min) 'font-lock-face)
                      'e-chat-title-face))
          (should-not (string-match-p "^e chat$" (buffer-string)))
          (should (get-text-property (point-min) 'read-only))
          (goto-char (point-min))
          (should-error (insert "mutate") :type 'buffer-read-only)
          (should-not (string-match-p (regexp-quote (e-chat-composer-glyph))
                                      (buffer-string)))
          (with-current-buffer (e-chat-test--composer buffer)
            (should (derived-mode-p 'e-chat-composer-mode))
            (should (number-or-marker-p (e-chat-composer-start-position)))
            (goto-char (point-max))
            (insert "editable")
            (should (equal (e-chat-composer-text) "editable"))
            (should (string-match-p (regexp-quote (e-chat-composer-glyph))
                                    (buffer-string)))
            (should (get-text-property
                     (1- (e-chat-composer-start-position))
                     'e-chat-composer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-composer-has-protected-prompt-glyph ()
  "The separate composer has a protected prompt glyph."
  (let ((buffer (e-chat-test--buffer nil "chat-separator")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-min))
          (should (looking-at-p (regexp-quote (e-chat-composer-glyph))))
          (should (eq (get-text-property (point) 'font-lock-face)
                      'e-chat-composer-face))
          (should (get-text-property (point) 'read-only)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-submit-multiline-composer-and-render-response ()
  "The composer submits multiline text and chat renders message blocks."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "hello back")
                   (:type done :reason stop))
                 "chat-submit")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "first line\nsecond line")
          (e-chat-submit)
          (should (e-chat-test--wait-until
                   (lambda () (string-match-p "hello back" (with-current-buffer buffer (buffer-string))))
                   1.0))
          (let ((content (with-current-buffer buffer (buffer-string))))
            (should (string-match-p (concat (regexp-quote (e-chat-transcript-user-glyph))
                                            " first line\nsecond line")
                                    content))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                             " hello back")
                     content))
            (should-not (string-match-p "Turn started" content))
            (should-not (string-match-p "Turn finished" content))
            (should-not (string-match-p "Backend returned no assistant output"
                                        content)))
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-min))
              (search-forward (concat (e-chat-transcript-user-glyph) " first line"))
              (should (eq (get-text-property (point) 'font-lock-face)
                          'e-chat-user-face))
              (search-forward "hello back")
              (should-not (eq (get-text-property (point) 'font-lock-face)
                              'e-chat-assistant-face))))
          (should (equal (e-chat-composer-text) "")))
      (when (buffer-live-p buffer)
        (with-current-buffer (e-chat-test--composer buffer)
          (ignore-errors
            (e-harness-test-abort e-chat-harness e-chat-session-id)))
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-composer-input-collapses-revealed-hidden ()
  "Returning to the composer collapses revealed hidden messages.
The default reading view is the clean transcript, so leaving inspection mode
must drop any revealed hidden blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-hidden-reveal-composer")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal-composer"
           (list :id "m-visible" :role 'assistant :turn-id "turn-1"
                 :content "revised answer"))
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal-composer"
           (list :id "m-first" :role 'assistant :turn-id "turn-1"
                 :content "superseded first attempt" :display 'hidden))
          (e-chat-test--seed-board-log-from-private-fixture
           e-chat-harness e-chat-session-id)
          (e-chat-clear)
          (e-chat-transcript-render-session)
          (e-chat-test--focus-block-containing "revised answer")
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should (string-match-p "superseded first attempt" (buffer-string)))
          (call-interactively #'e-chat-response-navigation-insert)
          (should-not (e-chat-transcript-reveal-hidden-p))
          (should (e-chat-test--message-display-hidden-p "m-first")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-submit-immediately-clears-composer-and-renders-user-turn ()
  "Submitting clears the composer while board observation shows the user turn."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "later")
                   (:type done :reason stop))
                 "chat-submit-immediate")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "send now")
          (e-chat-submit)
          (should (equal (e-chat-composer-text) ""))
          (should (e-chat-test--wait-until
                   (lambda () (string-match-p "send now" (with-current-buffer buffer (buffer-string))))
                   1.0))
          (let ((content (with-current-buffer buffer (buffer-string))))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-user-glyph))
                             " send now")
                     content))
            (should-not (string-match-p
                         (concat (regexp-quote (e-chat-composer-glyph))
                                 "send now")
                         content)))
          (should (e-chat-composer-active-p))
          (should (equal (e-chat-composer-text) "")))
      (when (buffer-live-p buffer)
        (with-current-buffer (e-chat-test--composer buffer)
          (ignore-errors
            (e-harness-test-abort e-chat-harness e-chat-session-id)))
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-active-prefix-submit-queues-running-turn ()
  "Prefix submit during a running turn queues the composer text."
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
                              :session-id "chat-active-queue"))
         queued)
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "first")
          (e-chat-submit)
          (should (e-chat-test--wait-until
                   (lambda ()
                     (e-chat-service-active-turn-p harness e-chat-session-id))
                   1.0))
          (goto-char (point-max))
          (insert "next ")
          (let ((reference
                 (e-chat-composer-insert-context-reference
                  '(:id "ref-1"
                    :uri "buffer://source"
                    :label "source:2"
                    :text "two"
                    :start-line 2
                    :end-line 2
                    :point-line 2))))
            (insert " prompt")
            (cl-letf (((symbol-function 'e-chat-service-queue-session)
                       (cl-function
                        (lambda (_harness session-id prompt
                                 &key references metadata)
                          (setq queued
                                (list session-id prompt references metadata))
                          "queue-id"))))
              (e-chat-submit '(4)))
            (should (equal (car queued) e-chat-session-id))
            (should (string-match-p
                     "next <reference id=\"ref-1\" label=\"source:2\"> prompt"
                     (cadr queued)))
            (should (equal (caddr queued) (list reference)))
            (should (equal (plist-get (cadddr queued) :submit-mode)
                           'queued))
            (should (equal (plist-get (cadddr queued) :references)
                           (list reference))))
          (should (equal (e-chat-composer-text) "")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-queued-prompts-survive-composer-refresh-and-final-insertion ()
  "Queue chrome survives spacer refresh and final assistant insertion."
  (let ((buffer (e-chat-test--buffer nil "chat-queue-refresh")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (cl-letf (((symbol-function 'e-chat-service-queued-inputs)
                     (lambda (&rest _) '((:prompt "second")))))
            (goto-char (point-max))
            (insert "draft")
            (e-chat-composer-insert-queued-prompts)
            (should (string-match-p "Queued prompts" (buffer-string)))
            (should (string-match-p "1\\. second" (buffer-string)))
            (should (equal (e-chat-composer-text) "draft"))
            (with-current-buffer buffer
              (e-chat-transcript-insert-entry "Assistant" "final answer" t "turn-final")
              (should (string-match-p "final answer" (buffer-string))))
            (should (string-match-p "Queued prompts" (buffer-string)))
            (should (string-match-p "1\\. second" (buffer-string)))
            (should (equal (e-chat-composer-text) "draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-empty-queue-removes-list-without-deleting-composer ()
  "Clearing the queue removes queue chrome and preserves composer text."
  (let ((buffer (e-chat-test--buffer nil "chat-queue-empty"))
        (queued '((:prompt "second"))))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (cl-letf (((symbol-function 'e-chat-service-queued-inputs)
                     (lambda (&rest _) queued)))
            (e-chat-composer-insert-queued-prompts)
            (goto-char (point-max))
            (insert "draft")
            (should (string-match-p "Queued prompts" (buffer-string)))
            (setq queued nil)
            (e-chat-composer-insert-queued-prompts)
            (should-not (string-match-p "Queued prompts" (buffer-string)))
            (should (equal (e-chat-composer-text) "draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-return-inserts-newline-in-composer ()
  "RET inserts a newline instead of submitting the prompt."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "unexpected")
                   (:type done :reason stop))
                 "chat-ret")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "one")
          (call-interactively (lookup-key e-chat-mode-map (kbd "RET")))
          (insert "two")
          (should (equal (e-chat-composer-text) "one\ntwo"))
          (should (equal (e-harness-messages e-chat-harness e-chat-session-id)
                         nil)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-control-p-stays-inside-composer ()
  "C-p from the first composer line does not move point into transcript text."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-c-p")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (e-chat-composer-start-position))
          (let ((composer-start (e-chat-composer-start-position)))
            (should-error (call-interactively (key-binding (kbd "C-p")))
                          :type 'beginning-of-buffer)
            (should (>= (point) composer-start))
            (should-not (get-text-property (point) 'e-chat-protected))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-post-command-without-edit-does-not-scroll-composer ()
  "Plain navigation commands do not force the window back to the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-no-edit-scroll"))
        recenter-called)
    (unwind-protect
        (cl-letf (((symbol-function 'recenter)
                   (lambda (&rest _ignored)
                     (setq recenter-called t))))
          (with-current-buffer buffer
            (run-hooks 'post-command-hook)
            (should-not recenter-called)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-composer-focus-enters-evil-insert-state ()
  "Showing the separate composer requests Evil's insert state."
  (let (entered)
    (cl-letf (((symbol-function 'evil-insert-state)
               (lambda () (setq entered t))))
      (with-temp-buffer
        (e-chat-composer-enter-input-state)))
    (should entered)))



(ert-deftest e-chat-test-submits-composer-text-with-inline-references ()
  "Submitting converts inline reference atoms into ordered prompt context."
  (let ((buffer (e-chat-test--buffer nil "chat-reference-submit")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "Look at ")
          (e-chat-composer-insert-context-reference
           '(:id "ref-1"
             :uri "buffer://source"
             :label "source:2-4"
             :text "two\nthree\nfour\n"
             :start-line 2
             :end-line 4
             :point-line 3))
          (insert ", then explain it.")
          (e-chat-submit)
          (should (e-chat-test--wait-until
                   (lambda ()
                     (e-chat-service-messages
                      e-chat-harness e-chat-session-id))
                   1.0))
          (let* ((message (car (e-chat-service-messages
                                e-chat-harness e-chat-session-id)))
                 (content (plist-get message :content))
                 (metadata (plist-get message :metadata)))
            (should (string-match-p
                     "Look at <reference id=\"ref-1\" label=\"source:2-4\">"
                     content))
            (should (string-match-p
                     "\\[ref-1\\] source:2-4 (buffer://source)"
                     content))
            (should (string-match-p "two\nthree\nfour" content))
            (should (equal (plist-get metadata :references)
                           '((:id "ref-1"
                              :uri "buffer://source"
                              :label "source:2-4"
                              :text "two\nthree\nfour\n"
                              :start-line 2
                              :end-line 4
                              :point-line 3))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-composer-delete-respects-inline-reference-boundaries ()
  "Delete reference atoms only on the side selected by the delete command."
  (let* ((buffer (e-chat-test--buffer nil "chat-reference-delete"))
         composer)
    (unwind-protect
        (progn
          (setq composer (e-chat-surface-composer-buffer buffer))
          (with-current-buffer composer
            (goto-char (point-max))
            (insert "before ")
            (e-chat-composer-insert-context-reference
             '(:id "ref-1"
               :uri "buffer://source"
               :label "source:2"
               :text "two"
               :start-line 2
               :end-line 2
               :point-line 2))
            (insert " after")
            ;; Forward delete after the atom and backspace before it should
            ;; operate on ordinary text, not reach across the boundary.
            (search-backward " after")
            (call-interactively (key-binding (kbd "C-d")))
            (should (equal (e-chat-composer-text) "before @[source:2]after"))
            (search-backward "@[source:2]")
            (call-interactively (key-binding (kbd "DEL")))
            (should (equal (e-chat-composer-text) "before@[source:2]after"))
            ;; Forward delete at the atom removes the whole atom.
            (call-interactively (key-binding (kbd "C-d")))
            (should (equal (e-chat-composer-text) "beforeafter"))
            (let ((inhibit-read-only t))
              (delete-region (e-chat-composer-start-position) (point-max)))
            (insert "again ")
            (e-chat-composer-insert-context-reference
             '(:id "ref-2"
               :uri "buffer://source"
               :label "source:3"
               :text "three"
               :start-line 3
               :end-line 3
               :point-line 3))
            (insert " tail")
            ;; Backspace after the atom removes the whole atom.
            (search-backward " tail")
            (call-interactively (key-binding (kbd "DEL")))
            (should (equal (e-chat-composer-text) "again  tail"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-composer-disables-completion ()
  "The composer does not trigger completion UI or completion-at-point."
  (let (company-mode-argument corfu-mode-argument buffer)
    (cl-letf (((symbol-function 'company-mode)
               (lambda (argument)
                 (setq company-mode-argument argument)))
              ((symbol-function 'corfu-mode)
               (lambda (argument)
                 (setq corfu-mode-argument argument)
                 (kill-local-variable 'completion-in-region-function))))
      (unwind-protect
          (progn
            (setq buffer (e-chat-test--buffer nil "chat-no-completion"))
            (with-current-buffer buffer
              (should (local-variable-p 'completion-at-point-functions))
              (should-not completion-at-point-functions)
              (should (local-variable-p 'completion-in-region-function))
              (should (eq completion-in-region-function #'ignore))
              (should (equal company-mode-argument -1))
              (should (equal corfu-mode-argument -1))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest e-chat-test-composer-prefix-shortcuts-fall-back-literally ()
  "Prefix shortcuts self-insert when they do not meet trigger conditions."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-literal"))
          (with-current-buffer (e-chat-test--composer buffer)
            (insert "hello")
            (let ((last-command-event ?!))
              (e-chat-composer-bang))
            (insert "user")
            (let ((last-command-event ?@))
              (e-chat-composer-at))
            (insert "path")
            (let ((last-command-event ?/))
              (e-chat-composer-slash))
            (should (equal (e-chat-composer-text)
                           "hello!user@path/"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-composer-prefix-shortcuts-no-op-outside-composer ()
  "Prefix shortcut commands do nothing outside an active composer."
  (with-temp-buffer
    (e-chat-mode)
    (let ((before (buffer-string)))
      (should-not (e-chat-composer-bang))
      (should-not (e-chat-composer-at))
      (should-not (e-chat-composer-slash))
      (should (equal (buffer-string) before)))))



(ert-deftest e-chat-test-composer-slash-uses-inline-completion-popup ()
  "Word-boundary / selects prompts through an inline composer popup."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-slash-inline"))
          (with-current-buffer (e-chat-test--composer buffer)
            (let* ((prompt (e-prompt-spec-create
                            :name "review"
                            :description "Review code."
                            :parameters nil
                            :template "Review now."))
                   (capability (e-capability-with-prompts-create
                                :id 'review-prompts
                                :name "Review Prompts"
                                :instructions "Use review prompts."
                                :prompts (list prompt)))
                   (unread-command-events (list ?\r)))
              (e-harness-activate-capability e-chat-harness capability)
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _args)
                           (error "composer / must not use completing-read"))))
                (e-chat-composer-slash)))
            (should (equal (e-chat-composer-text) "Review now."))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-failed-turn-clears-transient-before-next-prompt ()
  "A failed turn drops its transient activity so the next prompt renders.
Regression: after turn-failed the active \"Thinking...\"/\"Thought for ...\"
block and its separators lingered, and the next submitted prompt rendered into
the orphaned region and appeared to vanish."
  (let ((buffer (e-chat-test--buffer nil "chat-failed-then-prompt")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1" :created-at 0))
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1" :created-at 0
                            :payload '(:status started)))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          ;; The active thinking transient is on screen.
          (should (string-match-p "Thinking for" (buffer-string)))
          ;; Turn fails.
          (e-chat-render-event
           (e-events-make :type 'turn-failed
                          :session-id e-chat-session-id
                          :turn-id "turn-1" :created-at 9
                          :payload '(:error "boom")))
          ;; The live "Thinking" transient is gone; the failure entry plus the
          ;; settled duration summary are shown.  The summary persists exactly
          ;; like a normally-completed turn, so the abnormal end still reports
          ;; duration and tool-call counts.
          (let ((content (buffer-string)))
            (should (string-match-p "Turn failed: boom" content))
            (should (string-match-p "Turn took 0min 9sec\\." content))
            (should-not (string-match-p "Thinking for" content))
            (should-not (string-match-p "Thought for" content)))
          ;; The active progress indicator is genuinely stopped: a live progress
          ;; marker would make the next turn's redraw delete a region that
          ;; swallows freshly rendered content (the reported symptom).  The
          ;; running-status/transient markers now point at the persistent
          ;; summary, as they do for any settled turn, so they are not checked.
          (should-not (plist-get (e-chat-activity-progress-state) :turn-id))
          ;; A new prompt for a fresh turn renders and stays visible.
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-2" :created-at 10
                          :payload '(:message (:role user
                                               :content "retry please"))))
          (let ((content (buffer-string)))
            (should (string-match-p "retry please" content))
            (should-not (string-match-p "Thinking for" content))
            (should-not (string-match-p "Thought for" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-progress-rerender-keeps-editable-composer-draft ()
  "Progress redraws keep the follow-up composer editable with draft text."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-editable-composer")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-active-p))
            (goto-char (point-max))
            (insert "follow-up draft"))
          (e-chat-activity-advance-progress)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-active-p))
            (should (equal (e-chat-composer-text) "follow-up draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-progress-rerender-preserves-context-reference ()
  "Progress redraws keep inline context reference properties in the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-context-reference")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (let* ((composer (e-chat-test--composer buffer))
                 (reference
                  (with-current-buffer composer
                    (goto-char (point-max))
                    (insert "Review ")
                    (prog1
                        (e-chat-composer-insert-context-reference
                         '(:id "ref-1"
                           :uri "buffer://source"
                           :label "source:2"
                           :text "two"
                           :start-line 2
                           :end-line 2
                           :point-line 2))
                      (insert " before replying.")))))
            (e-chat-activity-advance-progress)
            (with-current-buffer composer
              (should (e-chat-composer-active-p))
              (let ((document (e-chat-composer-document)))
                (should (equal (plist-get document :text)
                               "Review <reference id=\"ref-1\" label=\"source:2\"> before replying."))
                (should (equal (plist-get document :references)
                               (list reference))))
              (goto-char (e-chat-composer-start-position))
              (search-forward "@[source:2]")
              (should (equal (get-text-property (match-beginning 0)
                                                'e-chat-context-reference)
                             reference)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-submit-keeps-follow-up-composer-editable ()
  "Submitting a prompt leaves an empty editable follow-up composer."
  (let* ((backend (e-backend-fake-create :items nil :delay 1.0))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open
                  :harness harness
                  :session-id "chat-submit-follow-up-composer")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "Initial prompt")
          (e-chat-submit)
          (should (e-chat-composer-active-p))
          (should (equal (e-chat-composer-text) ""))
          (goto-char (point-max))
          (insert "follow-up draft")
          (should (equal (e-chat-composer-text) "follow-up draft"))
          (e-chat-activity-advance-progress)
          (should (equal (e-chat-composer-text) "follow-up draft")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-progress-redraw-updates-without-composer-rebuild ()
  "Clean progress redraws update the running status without composer churn."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-no-composer-rebuild")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (let ((composer (e-chat-test--composer buffer))
                tick)
            (with-current-buffer composer
              (goto-char (point-max))
              (insert "follow-up draft")
              (setq tick (buffer-chars-modified-tick)))
            (e-chat-activity-advance-progress)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (with-current-buffer composer
              (should (= (buffer-chars-modified-tick) tick))
              (should (e-chat-composer-active-p))
              (should (equal (e-chat-composer-text)
                             "follow-up draft")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-block-view-i-returns-to-composer ()
  "Pressing i in block view leaves navigation and focuses the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-fold")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (call-interactively #'e-chat-block-view-insert)
          (should-not e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-point-in-composer-p))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-response-navigation-i-returns-to-composer ()
  "Pressing i leaves navigation mode and focuses the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-insert")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "i")))
          (should-not e-chat-response-navigation-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-point-in-composer-p))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-response-navigation-escape-returns-to-composer ()
  "Escape leaves navigation mode and focuses the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-escape")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (should (eq (lookup-key e-chat-response-navigation-mode-map
                                  (kbd "<escape>"))
                      #'e-chat-response-navigation-insert))
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "<escape>")))
          (should-not e-chat-response-navigation-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-point-in-composer-p))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-reload-buffers-preserves-composer-draft ()
  "Reloading chat buffers keeps unsent composer draft text."
  (let* ((harness (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
         (buffer (e-chat-open :harness harness :session-id "chat-reload-draft")))
    (unwind-protect
        (progn
          (with-current-buffer (e-chat-test--composer buffer)
            (goto-char (point-max))
            (insert "draft before reload")
            (should (equal (e-chat-composer-text) "draft before reload")))
          (e-chat-test--with-empty-harness-registry
            (let ((e-chat-default-harness-id :missing-chat))
              (should (>= (e-chat-reload-buffers) 1))))
          (with-current-buffer buffer
            (should (eq e-chat-harness harness)))
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-active-p))
            (should (equal (e-chat-composer-text) "draft before reload"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



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



(ert-deftest e-chat-test-composer-meta-actions-target-latest-response ()
  "Composer M-y and M-o target the latest final assistant response block."
  (unless (fboundp 'markdown-mode)
    (define-derived-mode markdown-mode text-mode "Markdown"))
  (let ((buffer (e-chat-test--buffer nil "chat-meta-actions"))
        (opened nil))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (e-chat-test--render-turn "turn-1" 10 11 "first" "old final")
            (e-chat-test--render-turn "turn-2" 20 21 "second" "latest final"))
          (with-current-buffer (e-chat-test--composer buffer)
            (goto-char (e-chat-composer-start-position))
            (call-interactively
             (lookup-key e-chat-composer-mode-map (kbd "M-y"))))
          (should (equal (current-kill 0) "latest final"))
          (setq opened
                (with-current-buffer (e-chat-test--composer buffer)
                  (call-interactively
                   (lookup-key e-chat-composer-mode-map (kbd "M-o")))))
          (should (eq (window-buffer (selected-window)) opened))
          (with-current-buffer opened
            (should (derived-mode-p 'markdown-mode))
            (should (equal (buffer-string) "latest final"))))
      (when (buffer-live-p opened)
        (kill-buffer opened))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-mode-map-binds-c-w-to-region-or-word-kill ()
  "The composer keymap binds \\`C-w' to the region/word kill command."
  (should (eq (lookup-key e-chat-mode-map (kbd "C-w"))
              'e-chat-kill-region-or-backward-word)))



(ert-deftest e-chat-test-c-w-kills-backward-word-without-region ()
  "Without an active region, \\`C-w' kills the previous word."
  (with-temp-buffer
    (insert "hello world")
    (deactivate-mark)
    (e-chat-kill-region-or-backward-word 1)
    (should (string= (buffer-string) "hello "))))



(ert-deftest e-chat-test-c-w-kills-region-when-active ()
  "With an active region, \\`C-w' kills the region."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (push-mark (point) t t)
    (goto-char (+ (point-min) 5))
    (activate-mark)
    (e-chat-kill-region-or-backward-word 1)
    (should (string= (buffer-string) " world"))))



(ert-deftest e-chat-test-evil-composer-bindings-reclaim-shadowed-keys ()
  "Evil-local composer bindings route Escape from every editing state."
  (let (calls)
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (&rest args)
                 (push args calls))))
      (e-chat-startup))
    (dolist (state '(insert emacs))
      (should (member (list state
                            e-chat-composer-mode-map
                            (kbd "C-w")
                            #'e-chat-kill-region-or-backward-word)
                      calls)))
    (dolist (state '(insert normal emacs))
      (should (member (list state
                            e-chat-composer-mode-map
                            (kbd "<escape>")
                            #'e-chat-composer-enter-navigation)
                      calls)))))



(ert-deftest e-chat-test-evil-composer-bindings-noop-without-evil ()
  "Composer Evil rebinding is a no-op when Evil is unavailable."
  (let ((orig-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (unless (eq sym 'evil-define-key*)
                   (funcall orig-fboundp sym)))))
      ;; Should not error when Evil is absent; startup owns this setup.
      (e-chat-startup)
      (should t))))

(ert-deftest e-chat-test-turns-and-responses-have-stable-separators ()
  "Rendered turns use explicit separator text outside navigable blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-turn-separators")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "two")
          (should (stringp (e-chat-transcript-turn-separator)))
          (should (stringp (e-chat-transcript-response-separator)))
          (let ((content (buffer-string)))
            (should (= (e-chat-test--count-occurrences
                        (e-chat-transcript-turn-separator) content)
                       1))
            (should (= (e-chat-test--count-font-lock-face-runs
                        'e-chat-separator-face (point-min) (point-max))
                       2))
            (should (equal (e-chat-transcript-response-separator)
                           (e-chat-composer-separator)))
            (should (string-match-p
                     (concat "one\\(.\\|\n\\)*"
                             (regexp-quote (e-chat-transcript-turn-separator))
                             "\\(.\\|\n\\)*"
                             (regexp-quote (e-chat-transcript-user-glyph))
                             " second")
                     content)))
          (goto-char (point-min))
          (search-forward (e-chat-transcript-turn-separator))
          (should (eq (get-text-property (line-beginning-position)
                                         'font-lock-face)
                      'e-chat-turn-separator-face))
          (should (get-text-property (line-beginning-position) 'read-only))
          (should-not (get-text-property (line-beginning-position)
                                         'e-chat-block-id))
          (goto-char (point-min))
          (search-forward (e-chat-transcript-response-separator))
          (should (eq (get-text-property (line-beginning-position)
                                         'font-lock-face)
                      'e-chat-separator-face))
          (should (get-text-property (line-beginning-position) 'read-only))
          (should-not (get-text-property (line-beginning-position)
                                         'e-chat-block-id)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-navigation-excludes-separators-and-does-not-reflow ()
  "Block navigation changes focus without adding/removing separator text."
  (let ((buffer (e-chat-test--buffer nil "chat-navigation-separator-stability")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "two")
          (should (stringp (e-chat-transcript-turn-separator)))
          (should (stringp (e-chat-transcript-response-separator)))
          (let ((content-before (buffer-string))
                (lines-before (count-lines (point-min) (point-max))))
            (goto-char (point-min))
            (search-forward "two")
            (call-interactively #'e-chat-enter-response-navigation)
            (should-not (string-match-p
                         (regexp-quote (e-chat-transcript-turn-separator))
                         (e-chat-test--focused-turn-text)))
            (should-not (string-match-p
                         (regexp-quote (e-chat-transcript-response-separator))
                         (e-chat-test--focused-turn-text)))
            (call-interactively
             (lookup-key e-chat-response-navigation-mode-map (kbd "k")))
            (should-not (string-match-p
                         (regexp-quote (e-chat-transcript-turn-separator))
                         (e-chat-test--focused-turn-text)))
            (should-not (string-match-p
                         (regexp-quote (e-chat-transcript-response-separator))
                         (e-chat-test--focused-turn-text)))
            (should (equal (buffer-string) content-before))
            (should (= (count-lines (point-min) (point-max))
                       lines-before))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-navigation-reveal-hidden-shows-and-focuses ()
  "Pressing `h' in navigation mode reveals hidden messages as focusable blocks.
The superseded first attempt and the machine-authored corrective prompt are
hidden from the clean transcript, but the ESC inspection mode must expose them
on demand so the user can audit what the calibration follow-up removed."
  (let ((buffer (e-chat-test--buffer nil "chat-hidden-reveal")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal"
           (list :id "m-visible" :role 'assistant :turn-id "turn-1"
                 :content "revised answer"))
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal"
           (list :id "m-first" :role 'assistant :turn-id "turn-1"
                 :content "superseded first attempt" :display 'hidden))
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal"
           (list :id "m-prompt" :role 'user :turn-id "turn-1"
                 :content "machine corrective prompt"
                 :metadata '(:display hidden)))
          (e-chat-test--seed-board-log-from-private-fixture
           e-chat-harness e-chat-session-id)
          (e-chat-clear)
          (e-chat-transcript-render-session)
          (should (e-chat-test--message-display-hidden-p "m-first"))
          (should (e-chat-test--message-display-hidden-p "m-prompt"))
          (e-chat-test--focus-block-containing "revised answer")
          (should-not (e-chat-transcript-reveal-hidden-p))
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should (e-chat-transcript-reveal-hidden-p))
          (should (string-match-p "superseded first attempt" (buffer-string)))
          (should (string-match-p "machine corrective prompt" (buffer-string)))
          ;; A revealed hidden message is a real navigable block.
          (e-chat-test--focus-block-containing "superseded first attempt")
          (should (eq (plist-get (e-chat-test--focused-block) :kind)
                      'hidden))
          (should e-chat-response-navigation-mode))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-navigation-reveal-hidden-toggles-off ()
  "Pressing `h' twice hides the revealed messages again.
Reveal is a temporary inspection affordance; toggling it off restores the clean
one-answer transcript."
  (let ((buffer (e-chat-test--buffer nil "chat-hidden-reveal-off")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal-off"
           (list :id "m-visible" :role 'assistant :turn-id "turn-1"
                 :content "revised answer"))
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal-off"
           (list :id "m-first" :role 'assistant :turn-id "turn-1"
                 :content "superseded first attempt" :display 'hidden))
          (e-chat-test--seed-board-log-from-private-fixture
           e-chat-harness e-chat-session-id)
          (e-chat-clear)
          (e-chat-transcript-render-session)
          (e-chat-test--focus-block-containing "revised answer")
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should (string-match-p "superseded first attempt" (buffer-string)))
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should-not (e-chat-transcript-reveal-hidden-p))
          (should (e-chat-test--message-display-hidden-p "m-first")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-assistant-markdown-renders-with-text-properties ()
  "Assistant messages keep Markdown text and use markdown-mode faces."
  (skip-unless (require 'markdown-mode nil t))
  (let ((buffer (e-chat-test--buffer nil "chat-markdown")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-transcript-insert-entry
           "Assistant"
           "## Heading\nUse **bold**, *italic*, and `code`.\n- item\n\n```elisp\n(message \"hi\")\n```\n\n[docs](https://example.test)")
          (let ((content (buffer-string)))
            (should (string-match-p "## Heading" content))
            (should (string-match-p "\\*\\*bold\\*\\*" content))
            (should (string-match-p "`code`" content))
            (should (string-match-p "```elisp" content))
            (should (string-match-p "\\[docs\\](https://example.test)" content)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "##")
            (should-not (get-text-property (1- (point)) 'invisible))
            (should (memq 'markdown-header-face-2
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "Heading")
            (should (memq 'markdown-header-face-2
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (should-not (eq (get-text-property (1- (point)) 'font-lock-face)
                            'e-chat-assistant-face))
            (search-forward "bold")
            (should (memq 'markdown-bold-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-backward "**")
            (should (memq 'markdown-bold-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))
            (search-forward "italic")
            (should (memq 'markdown-italic-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-backward "*")
            (should (memq 'markdown-italic-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))
            (search-forward "code")
            (should (memq 'markdown-inline-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-backward "`")
            (should (memq 'markdown-inline-code-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))
            (search-forward "- item")
            (should (memq 'markdown-list-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "```elisp")
            (should (memq 'markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (should-not (get-text-property (1- (point)) 'invisible))
            (search-forward "(message \"hi\")")
            (should (memq 'markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "```")
            (should (memq 'markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "docs")
            (should (memq 'markdown-link-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (should (equal (get-text-property (1- (point)) 'help-echo)
                           "https://example.test"))
            (should (equal (get-text-property (1- (point)) 'e-chat-link-url)
                           "https://example.test"))
            (search-backward "[")
            (should (memq 'markdown-link-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-assistant-org-output-mode-renders-org-links ()
  "With output mode `org' the renderer uses Org faces and clickable Org links."
  (let ((buffer (e-chat-test--buffer nil "chat-org-output")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-output-mode-session-set
           e-chat-harness e-chat-session-id 'org)
          (should (eq (e-chat-output-mode-resolve
                       e-chat-harness e-chat-session-id)
                      'org))
          (e-chat-transcript-insert-entry
           "Assistant"
           "* Heading
See [[https://example.test][docs]] and [[file:notes.org]].")
          (save-excursion
            (goto-char (point-min))
            (search-forward "docs")
            (should (memq 'e-chat-markdown-link-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (should (equal (get-text-property (1- (point)) 'e-chat-link-url)
                           "https://example.test"))
            ;; The [[...][ bracket syntax around the description is concealed.
            (search-backward "[[https")
            (should (get-text-property (point) 'invisible))
            (goto-char (point-min))
            (search-forward "notes.org")
            (should (equal (get-text-property (1- (point)) 'e-chat-link-url)
                           "file:notes.org"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-resource-link-opens-read-only-buffer ()
  "Clicking a session resource link opens its content read-only."
  (let ((buffer (e-chat-test--buffer nil "chat-resource-link"))
        opened-uri
        opened-buffer)
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-output-mode-session-set
           e-chat-harness e-chat-session-id 'org)
          (e-chat-transcript-insert-entry
           "Assistant"
           "See [[session://e/sessions/session-1/messages][discussion]].")
          (goto-char (point-min))
          (search-forward "discussion")
          (cl-letf (((symbol-function 'e-resources-read)
                     (lambda (_registry uri &optional _range)
                       (setq opened-uri uri)
                       "resource body")))
            (setq opened-buffer (e-chat-open-link)))
          (should (equal opened-uri
                         "session://e/sessions/session-1/messages"))
          (should (buffer-live-p opened-buffer))
          (with-current-buffer opened-buffer
            (should (derived-mode-p 'special-mode))
            (should buffer-read-only)
            (should (equal (buffer-string) "resource body"))))
      (when (buffer-live-p opened-buffer)
        (kill-buffer opened-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-set-output-mode-rerenders-visible-blocks ()
  "Toggling output mode re-renders visible final assistant blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-output-toggle")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-transcript-insert-entry
           "Assistant"
           "See [[https://example.test][docs]]."
           nil
           "turn-org-toggle")
          ;; Default markdown mode leaves the Org link text literal.
          (save-excursion
            (goto-char (point-min))
            (search-forward "docs")
            (should-not (get-text-property (1- (point)) 'e-chat-link-url)))
          (e-chat-set-output-mode 'org)
          (save-excursion
            (goto-char (point-min))
            (search-forward "docs")
            (should (equal (get-text-property (1- (point)) 'e-chat-link-url)
                           "https://example.test"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-final-response-uses-visual-marker-face ()
  "Final assistant output is visually distinguished without a text label."
  (let ((buffer (e-chat-test--buffer nil "chat-final-face")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (goto-char (point-min))
          (search-forward "Final answer.")
          (should (memq 'e-chat-final-assistant-face
                        (ensure-list
                         (get-text-property (1- (point)) 'face))))
          (should-not (string-match-p "\nFinal\n" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-final-response-keeps-markdown-faces ()
  "Settled assistant styling preserves Markdown presentation faces."
  (let ((buffer (e-chat-test--buffer nil "chat-final-markdown")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-transcript-insert-entry
           "Assistant"
           "Use **bold** and `code`."
           nil
           "turn-final-md")
          (goto-char (point-min))
          (search-forward "bold")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should (memq 'e-chat-markdown-strong-face faces))
            (should-not (eq (get-text-property (1- (point)) 'font-lock-face)
                            'e-chat-final-assistant-face)))
          (search-forward "code")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should (memq 'e-chat-markdown-code-face faces))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-response-navigation-j-and-k-move-focus ()
  "Response navigation j/k move between turn blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-move")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "k")))
          (should (string-match-p
                   (concat (regexp-quote (e-chat-transcript-user-glyph)) " second")
                   (e-chat-test--focused-turn-text)))
          (should-not (string-match-p
                       (concat (regexp-quote (e-chat-transcript-assistant-glyph)) " two")
                       (e-chat-test--focused-turn-text)))
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "j")))
          (should (string-match-p
                   (concat (regexp-quote (e-chat-transcript-assistant-glyph)) " two")
                   (e-chat-test--focused-turn-text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-response-navigation-ret-enters-block-view ()
  "RET on a final block enters block-local view and ESC returns to navigation."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-expand")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (should (equal (plist-get (e-chat-test--focused-block) :action-text)
                         "two"))
          (call-interactively #'e-chat-block-view-back)
          (should-not e-chat-block-view-mode)
          (should e-chat-response-navigation-mode)
          (should (equal (e-chat-transcript-focused-turn-id) "turn-2")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-block-view-can-select-and-copy-text ()
  "Block view v starts a region, h/l keep it active, and y copies it."
  (let ((buffer (e-chat-test--buffer nil "chat-block-view-select")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "alpha beta")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "v")))
          (dotimes (_ 5)
            (call-interactively
             (lookup-key e-chat-block-view-mode-map (kbd "l"))))
          (should (region-active-p))
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "y")))
          (should (equal (current-kill 0) "alpha")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-block-view-esc-clears-selection-before-exiting ()
  "In block-view selection mode, ESC resets selection before returning to nav."
  (let ((buffer (e-chat-test--buffer nil "chat-block-view-selection-esc")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "alpha beta")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "v")))
          (dotimes (_ 5)
            (call-interactively
             (lookup-key e-chat-block-view-mode-map (kbd "l"))))
          (should (region-active-p))
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "<escape>")))
          (should-not (region-active-p))
          (should e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "<escape>")))
          (should-not e-chat-block-view-mode)
          (should e-chat-response-navigation-mode))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-response-navigation-copy-and-open-use-block-content ()
  "Copy and open actions use the focused block action text without chrome."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-actions"))
        (opened nil))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first prompt" "final text")
          (e-chat-test--focus-block-containing "first prompt")
          (should (eq (plist-get (e-chat-test--focused-block) :kind) 'user))
          (call-interactively #'e-chat-response-navigation-copy)
          (should (equal (current-kill 0) "first prompt"))
          (setq opened (e-chat-response-navigation-open))
          (with-current-buffer opened
            (should (derived-mode-p 'text-mode))
            (should-not buffer-read-only)
            (should (equal (buffer-string) "first prompt")))
          (with-current-buffer buffer
            (e-chat-test--focus-block-containing "final text")
            (should (eq (plist-get (e-chat-test--focused-block) :kind) 'final))
            (call-interactively #'e-chat-response-navigation-copy)
            (should (equal (current-kill 0) "final text"))))
      (when (buffer-live-p opened)
        (kill-buffer opened))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-response-navigation-replayed-session-uses-synthetic-turns ()
  "Replayed messages without turn metadata remain navigable."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-nav-replay")
          (e-session-append-message
           store "chat-nav-replay"
           '(:role user
             :content "old first"
             :created-at "1970-01-01T00:00:10Z"))
          (e-session-append-message
           store "chat-nav-replay"
           '(:role assistant
             :content "old one"
             :created-at "1970-01-01T00:00:12Z"))
          (e-session-append-message
           store "chat-nav-replay"
           '(:role user
             :content "old second"
             :created-at "1970-01-01T00:00:20Z"))
          (e-session-append-message
           store "chat-nav-replay"
           '(:role assistant
             :content "old two"
             :created-at "1970-01-01T00:00:22Z"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-nav-replay")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-nav-replay"))
          (with-current-buffer buffer
            (call-interactively #'e-chat-enter-response-navigation)
            (should
             (equal (e-chat-transcript-focused-turn-id)
                    (plist-get
                     (seq-find
                      (lambda (message)
                        (equal (plist-get message :content) "old second"))
                      (e-chat-service-messages harness "chat-nav-replay"))
                     :turn-id)))
            (let ((details (e-chat-response-navigation-details)))
              (with-current-buffer details
                (should (string-match-p
                         "  Started: 1970-01-01T00:00:20Z"
                         (buffer-string)))
                (should (string-match-p
                         "  Ended: 1970-01-01T00:00:22Z"
                         (buffer-string)))
                (should (string-match-p "  Duration: 0min 2sec"
                                        (buffer-string)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))



(ert-deftest e-chat-test-replay-render-never-reads-private-transcript-indexes ()
  "Opening durable history renders only the board-derived projection."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         buffer)
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "board-only-render")
          (e-session-append-message
           store "board-only-render"
           '(:id "private-user" :role user :content "board prompt"))
          (e-session-append-message
           store "board-only-render"
           '(:id "private-output" :role assistant :content "board answer"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "board-only-render")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "board-only-render"))
          (cl-letf (((symbol-function 'e-harness-messages)
                     (lambda (&rest _) (error "private transcript read")))
                    ((symbol-function 'e-session-messages)
                     (lambda (&rest _) (error "private message index read")))
                    ((symbol-function 'e-session-activity-events)
                     (lambda (&rest _) (error "private activity index read"))))
            (with-current-buffer buffer
              (e-chat-clear)
              (e-chat-transcript-render-session)
              (should (string-match-p "board prompt" (buffer-string)))
              (should (string-match-p "board answer" (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-resume-preview-for-index-session-avoids-transcript-load ()
  "Resume previews render metadata when a persistent transcript is not loaded."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get
                      (e-chat-test--create-session store
                                        :id "indexed-preview"
                                        :metadata '(:name "Indexed preview"))
                      :id))
         (backend (e-backend-fake-create :items nil)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "indexed preview hello"))
          (let* ((indexed-store (e-session-persistent-index-store-create directory))
                 (harness (e-chat-test--activate-chat-session
                           (e-harness-create :backend backend
                                             :sessions indexed-store)))
                 (session (car (e-harness-session-list harness)))
                 (loaded nil))
            (should-not (plist-get session :loaded))
            (cl-letf (((symbol-function 'e-session-load-session)
                       (lambda (&rest _args)
                         (setq loaded t)
                         (error "preview loaded transcript"))))
              (let ((preview (e-chat-overview-render-resume-preview harness session)))
                (should-not loaded)
                (with-current-buffer preview
                  (let ((text (buffer-string)))
                    (should buffer-read-only)
                    (should (equal e-chat-session-id "indexed-preview"))
                    (should-not (string-match-p
                                 (regexp-quote (e-chat-composer-glyph))
                                 text))
                    (should (string-match-p "Indexed preview" text))
                    (should (string-match-p "indexed preview hello" text)))))
              (let ((preview (e-chat-overview-render-resume-preview harness session)))
                (should-not loaded)
                (with-current-buffer preview
                  (let ((text (buffer-string)))
                    (should buffer-read-only)
                    (should (equal e-chat-session-id "indexed-preview"))
                    (should-not (string-match-p
                                 (regexp-quote (e-chat-composer-glyph))
                                 text))
                    (should (string-match-p "Indexed preview" text))))))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))



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



(ert-deftest e-chat-test-add-context-clears-target-block-view-mode ()
  "Context insertion into a chat target exits stale block view state."
  (let ((buffer (e-chat-test--buffer nil "chat-context-block-view"))
        (reference '(:uri "buffer://source"
                     :label "source:1"
                     :text "source"
                     :start-line 1
                     :end-line 1
                     :point-line 1)))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-block-view-mode)
          (e-chat-add-context-reference-to-session
           reference
           e-chat-harness
           e-chat-session-id)
          (should-not e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-point-in-composer-p)))
          (should-not (eq (key-binding (kbd "h") t)
                          #'e-chat-block-view-left)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-add-context-clears-target-response-navigation-mode ()
  "Context insertion into a chat target exits stale response navigation state."
  (let ((buffer (e-chat-test--buffer nil "chat-context-response-nav"))
        (reference '(:uri "buffer://source"
                     :label "source:1"
                     :text "source"
                     :start-line 1
                     :end-line 1
                     :point-line 1)))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (should e-chat-response-navigation-mode)
          (e-chat-add-context-reference-to-session
           reference
           e-chat-harness
           e-chat-session-id)
          (should-not e-chat-response-navigation-mode)
          (should-not e-chat-block-view-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-point-in-composer-p)))
          (should-not (eq (key-binding (kbd "j") t)
                          #'e-chat-response-navigation-next)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-token-usage-events-update-mode-line-without-transcript ()
  "Token usage events update status without rendering system transcript blocks."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness :session-id "chat-token-usage-event")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-session-append-activity-event
           store
           e-chat-session-id
           "turn-1"
           'token-usage
           '(:input-tokens 54581
             :cached-input-tokens 30720
             :output-tokens 154
             :reasoning-output-tokens 0
             :total-tokens 54735))
          (e-chat-render-event
           (e-events-make :type 'token-usage
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:input-tokens 54581
                                     :cached-input-tokens 30720
                                     :output-tokens 154
                                     :reasoning-output-tokens 0
                                     :total-tokens 54735)))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (equal (e-chat-surface-mode-line-status)
                         "e-chat gpt-5.5/high 21% (55k/258k tok)"))
          (should-not (string-match-p "token-usage" (buffer-string)))
          (should-not (string-match-p "Event:" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-inspect-error-targets-newest-failure-outside-block ()
  "e-inspect-error falls back to the newest persisted failed turn."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         prompt)
    (e-harness-create-session harness :id "older-session")
    (e-session-append-message
     store "older-session"
     '(:id "older-msg" :role user :content "older" :turn-id "older-turn"))
    (e-session-append-activity-event
     store "older-session" "older-turn" 'turn-failed
     '(:error "older failure"))
    (e-harness-create-session harness :id "newer-session")
    (e-session-append-message
     store "newer-session"
     '(:id "newer-msg" :role user :content "newer" :turn-id "newer-turn"))
    (e-session-append-activity-event
     store "newer-session" "newer-turn" 'turn-failed
     '(:error "newer failure"))
    (cl-letf (((symbol-function 'e-chat-create-session)
               (lambda (&rest _args) '(:id "inspection-session")))
              ((symbol-function 'e-chat-open-session)
               (lambda (&rest _args) nil))
              ((symbol-function 'e-chat-submit-session)
               (lambda (_harness _session-id submitted-prompt &rest _args)
                 (setq prompt submitted-prompt))))
      (e-inspect-error :harness harness)
      (should (string-match-p "newer-session" prompt))
      (should (string-match-p "newer-turn" prompt)))))



(ert-deftest e-chat-test-open-loaded-session-replay-remains-bounded ()
  "Loaded-session replay never backfills omitted transcript history."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-chat-session-replay-message-limit 2)
         buffer)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "loaded-bounded"
                            :metadata '(:name "Loaded bounded"))
          (dolist (message
                   '((:id "msg-1" :role user :content "first prompt")
                     (:id "msg-2" :role assistant :content "first response")
                     (:id "msg-3" :role user :content "middle prompt")
                     (:id "msg-4" :role user :content "last prompt")
                     (:id "msg-5" :role assistant :content "last response")))
            (e-session-append-message store "loaded-bounded" message))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "loaded-bounded")
          (setq buffer (e-chat-open-session harness "loaded-bounded"))
          (with-current-buffer (e-chat-test--composer buffer)
            (goto-char (point-max))
            (insert "next")
            (should (equal (e-chat-composer-text) "next")))
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should (string-match-p
                       "3 earlier transcript messages omitted" text))
              (should-not (string-match-p "first prompt" text))))
          ;; Drain any deferred presentation work.  Omitted history must not
          ;; reappear after the initial paint has returned.
          (with-current-buffer buffer
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (let ((text (buffer-string)))
              (should (string-match-p
                       "3 earlier transcript messages omitted" text))
              (should-not (string-match-p "first prompt" text))
              (should-not (string-match-p "first response" text))
              (should-not (string-match-p "middle prompt" text))
              (should (string-match-p "last prompt" text))
              (should (string-match-p "last response" text))
              (should (equal (e-chat-test--composer-text-for buffer) "next")))
            (e-chat-transcript-rerender)
            (let ((text (buffer-string)))
              (should (string-match-p
                       "3 earlier transcript messages omitted" text))
              (should-not (string-match-p "first prompt" text))
              (should-not (string-match-p "middle prompt" text))
              (should (string-match-p "last response" text))
              (should (equal (e-chat-test--composer-text-for buffer) "next")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-activity-keeps-single-response-separator ()
  "A running turn keeps one user/assistant separator after final output."
  (let ((buffer (e-chat-test--buffer nil "chat-running-response-separator")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:message (:role user
                                                :content "inspect"))))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "read")))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "done"))))
          (should (stringp (e-chat-transcript-response-separator)))
          (should (= (e-chat-test--count-font-lock-face-runs
                      'e-chat-separator-face
                      (point-min)
                      (point-max))
                     1))
          (should-not (string-match-p "2 tool calls" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-submit-does-not-render-assistant-before-async-provider-completes ()
  "Chat submit stays responsive while the provider request is still running."
  (let* ((finish nil)
         (backend (e-backend-create
                   :name "held-chat"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore messages options on-error)
                      (funcall on-request-start (e-backend-request-create))
                      (setq finish
                            (lambda ()
                              (funcall on-item
                                       '(:type assistant-message
                                         :content "late answer"))
                              (funcall on-item
                                       '(:type done :reason stop))
                              (funcall on-done '(:status done))))
                      nil))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-async-submit")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "send async")
          (e-chat-submit)
          (should (e-chat-test--wait-until (lambda () finish) 2.0))
          (should (e-chat-test--wait-until
                   (lambda ()
                     (string-match-p
                      (concat (regexp-quote (e-chat-transcript-user-glyph))
                              " send async")
                      (with-current-buffer buffer (buffer-string))))
                   2.0))
          (should-not (string-match-p "late answer" (with-current-buffer buffer (buffer-string))))
          (should (string-match-p
                   "\\(?:queued\\|waiting for provider\\)"
                   (with-current-buffer buffer
                     (format "%s" header-line-format))))
          (funcall finish)
          (should (e-chat-test--wait-until
                   (lambda () (string-match-p "late answer" (with-current-buffer buffer (buffer-string))))
                   2.0))
          (should (e-chat-test--wait-until
                   (lambda ()
                     (with-current-buffer buffer
                       (string-match-p "done"
                                       (format "%s" header-line-format))))
                   2.0)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-active-submit-steers-running-turn ()
  "Plain submit during a running turn steers instead of starting a new turn."
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
                              :session-id "chat-active-steer"))
         steered)
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "first")
          (e-chat-submit)
          (should (e-chat-test--wait-until
                   (lambda ()
                     (e-chat-service-active-turn-p harness e-chat-session-id))
                   1.0))
          (goto-char (point-max))
          (insert "focus ")
          (let ((reference
                 (e-chat-composer-insert-context-reference
                  '(:id "ref-1"
                    :uri "buffer://source"
                    :label "source:2"
                    :text "two"
                    :start-line 2
                    :end-line 2
                    :point-line 2))))
            (insert " here")
            (cl-letf (((symbol-function 'e-chat-service-steer-session)
                       (lambda (_harness session-id prompt &key metadata)
                         (setq steered (list session-id prompt metadata))
                         :accepted)))
              (e-chat-submit))
            (should (equal (car steered) e-chat-session-id))
            (should (string-match-p
                     "focus <reference id=\"ref-1\" label=\"source:2\"> here"
                     (cadr steered)))
            (should (equal (plist-get (caddr steered) :submit-mode)
                           'steering))
            (should (equal (plist-get (caddr steered) :references)
                           (list reference))))
          (should (equal (e-chat-composer-text) "")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



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
                  (when-let ((item (car (e-harness--pending-steering-items
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



(ert-deftest e-chat-test-abort-cancels-active-tool-request ()
  "The chat abort command cancels an active tool request."
  (let* ((tool-callbacks nil)
         (tool-cancelled nil)
         (backend
          (e-backend-create
           :name "chat-tool-abort"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-error on-request-start)
              (funcall on-item
                       '(:type tool-call
                         :id "call-1"
                         :name "held-tool"
                         :arguments (:stated_purpose "Hold this tool call."
                                     :text "hi")))
              (funcall on-item '(:type done :reason tool-use))
              (funcall on-done '(:status done))
              nil))))
         (tools (e-tools-registry-create))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-tool-abort")))
    (e-tools-test-register
     tools
     :name "held-tool"
     :description "Hold."
     :start
     (cl-function
      (lambda (&key arguments on-done on-error on-request-start)
        (ignore arguments on-error)
        (setq tool-callbacks (list :on-done on-done))
        (let ((request
               (e-tools-request-create
                :cancel (lambda ()
                          (setq tool-cancelled t)
                          t))))
          (funcall on-request-start request)
          request))))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-harness-tools)
                     (lambda (_harness &optional _session-id _turn-id) tools)))
            (e-chat-submit "run held tool")
            (should (e-chat-test--wait-until (lambda () tool-callbacks) 1.0))
            (e-chat-abort)
            (funcall (plist-get tool-callbacks :on-done) "late result")
            (should (e-chat-test--wait-until
                     (lambda ()
                       (not (plist-get
                             (e-chat-service-state harness e-chat-session-id)
                             :active-turn)))
                     1.0))
            (should tool-cancelled)
            (should (e-chat-test--wait-until
                     (lambda ()
                       (string-match-p "Turn cancelled" (buffer-string)))
                     1.0))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :role))
                                   (e-chat-service-messages
                                    harness e-chat-session-id))
                           '(user)))
            (should (seq-some
                     (lambda (event)
                       (eq (plist-get event :event-type) 'turn-cancelled))
                     (e-chat-service-activity-events
                      harness e-chat-session-id)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-hides-transient-tool-entries ()
  "Tool progress stays out of the transcript after the turn settles."
  (let ((buffer (e-chat-test--buffer nil "chat-events")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-failed
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:error "boom")))
          (e-chat-render-event
           (e-events-make :type 'turn-cancelled
                          :session-id e-chat-session-id
                          :turn-id "turn-2"))
          (e-chat-render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-3"
                          :payload '(:result (:status ok))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-system-glyph))
                             " System\nTurn failed: boom")
                     content))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-system-glyph))
                             " System\nTurn cancelled")
                     content))
            (should-not (string-match-p "(:status ok)" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-turn-started-shows-moving-assistant-progress ()
  "An active assistant turn shows a protected moving glyph indicator."
  (let ((buffer (e-chat-test--buffer nil "chat-progress")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                             " ⠋")
                     content))
            (goto-char (point-min))
            (search-forward "⠋")
            (should (get-text-property (1- (point)) 'read-only)))
          (e-chat-activity-advance-progress)
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (should (string-match-p
                   (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                           " ⠙")
                   (buffer-string)))
          (should (>= e-chat-progress-interval 0.5))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (should (equal (e-chat-activity-progress-turn-id) "turn-1"))
          (e-chat-render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                             " Final answer.")
                     content))
            (should-not (string-match-p
                         (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                                 " [⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]")
                         content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-provider-wait-and-stream-statuses ()
  "Provider lifecycle and stream events update the compact running status."
  (let ((buffer (e-chat-test--buffer nil "chat-provider-status")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:provider codex
                                      :transport url-retrieve
                                      :url-host "example.test"
                                      :url-path "/codex/responses"
                                      :timeout-seconds 180
                                      :status started)))
          (should (string-match-p "waiting for provider"
                                  header-line-format))
          (should (equal (e-chat-activity-progress-turn-id) "turn-1"))
          (e-chat-render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type reasoning-delta
                                      :content "thinking")))
          (should (string-match-p "reasoning" header-line-format))
          (e-chat-render-event
           (e-events-make :type 'assistant-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type assistant-delta
                                      :content "answer")))
          (should (string-match-p "streaming" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-turn-steered-schedules-visible-indicator ()
  "A steering event schedules a visible active-turn indicator."
  (let ((buffer (e-chat-test--buffer nil "chat-steered-indicator")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'turn-steered
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:prompt-preview "focus on the count")))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (should (string-match-p "steered" header-line-format))
          (should (string-match-p "Steered: focus on the count"
                                  (buffer-string))))
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



(ert-deftest e-chat-test-progress-rerender-updates-active-thinking-row ()
  "Progress redraw updates the active thinking row without duplicating it."
  (let ((buffer (e-chat-test--buffer nil "chat-active-thinking-rerender")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started))))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 15.0)))
            (e-chat-activity-advance-progress)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠙ Thinking for 0min 15sec" content))
            (should (= (e-chat-test--count-occurrences
                        "Thinking for" content)
                       1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-progress-rerender-updates-between-provider-and-tool ()
  "Progress redraw keeps counting after a provider settles within a live turn."
  (let ((buffer (e-chat-test--buffer nil "chat-between-step-progress")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-test--mark-active-turn "turn-1")
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:status done)))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat-activity-advance-progress)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠙ Working for 0min 8sec" content))
            (should-not (string-match-p
                         "Thought for 0min 1sec" content)))
          (e-chat-activity-stop-progress "turn-1")
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Thought for 0min 1sec" content))
            (should-not (string-match-p
                         "Working for" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-progress-interval-stops-when-harness-turn-settled ()
  "Progress ticks stop when the harness no longer has the progress turn active."
  (let ((buffer (e-chat-test--buffer nil "chat-stale-progress-turn")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (puthash e-chat-session-id
                   '(:id "turn-1" :status done)
                   (e-harness-active-turns e-chat-harness))
          (e-chat-activity-advance-progress)
          (should-not (e-chat-activity-progress-turn-id))
          (should-not (plist-get (e-chat-activity-progress-state)
                                 :interval-active-p))
          (should-not (string-match-p "Thinking for" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-tool-count-renders-on-thinking-row ()
  "A round's tool count renders on the same line as its activity row.
Once a tool completes, the left cell settles back to \"Thought for ...\"."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-count-thinking-row")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:id "call-1" :name "read")))
          (e-chat-render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:id "call-1" :result "done")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Thought for 0min 10sec +1 tool call" content))
            (should-not (string-match-p
                         "Thought for 0min 10sec\n\n1 tool call"
                         content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-running-tool-row-names-tool-and-ticks ()
  "While a tool runs, the activity row shows a live spinner, name, and duration."
  (let ((buffer (e-chat-test--buffer nil "chat-running-tool-row")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          ;; Freeze the clock so the elapsed duration is deterministic.
          (cl-letf (((symbol-function 'float-time)
                     (lambda () 25)))
            (e-chat-render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 20
                            :payload '(:id "call-1" :name "bash")))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (let ((content (buffer-string)))
              ;; The running tool is named with its live elapsed time, and the
              ;; frozen "Thought for" text is gone from the row while it runs.
              (should (string-match-p "Running bash for 0min 5sec" content))
              (should (string-match-p "1 tool call" content))
              (should-not (string-match-p "Thought for" content))))
          ;; When the tool finishes, the row settles back to the thought text.
          (e-chat-render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 30
                          :payload '(:id "call-1" :result "ok")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p "Thought for 0min 10sec" content))
            (should-not (string-match-p "Running bash" content))
            ;; The finished tool's name and run duration stay on the summary
            ;; row (started at 20, finished at 30 -> 0min 10sec).
            (should (string-match-p "1 tool call (bash) for 0min 10sec"
                                    content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-activity-redraw-does-not-recenter ()
  "Deferred activity redraws do not enter redisplay through `recenter'."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-redraw-no-recenter"))
        (window nil)
        recenter-called)
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started)))
            (when-let ((bounds (e-chat-surface-running-status-bounds)))
              (goto-char (cdr bounds))
              (set-window-point window (point))))
            (cl-letf (((symbol-function 'recenter)
                       (lambda (&rest _ignored)
                         (setq recenter-called t))))
              (with-current-buffer buffer
                (e-chat-test--run-pending-ui-work
                 'activity-redraw "turn-1")))
          (should-not recenter-called))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-running-tool-row-collapses-parallel-tools ()
  "Several tools running at once collapse into a counted running row."
  (let ((buffer (e-chat-test--buffer nil "chat-running-tools-parallel")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (dolist (tool '(("call-1" "read")
                          ("call-2" "grep")))
            (e-chat-render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 11
                            :payload (list :id (nth 0 tool)
                                           :name (nth 1 tool)))))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p "Running 2 tools (read, grep)" content))
            (should (string-match-p "2 tool calls" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-tool-progress-renders-on-running-tool-row ()
  "Streaming tool progress updates the running activity row."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-progress-row")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:id "call-1" :name "bash")))
          (e-chat-render-event
           (e-events-make :type 'tool-progress
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:tool-call-id "call-1"
                                     :bytes 128
                                     :lines 4
                                     :preview "installing\n")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "1 tool call (bash), 128 bytes output" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-activity-rounds-have-subtle-separators ()
  "Multiple intermittent rounds are separated inside the activity block."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-round-separators")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 20
                          :payload '(:status started)))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat "Thought for 0min 10sec\n"
                             (make-string 64 ?┈)
                             "\n⠋ Thinking for")
                     content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-activity-separator-is-quieter-than-response-separator ()
  "Activity round dividers are quieter than the prompt/agent separator."
  (should (boundp 'e-chat-activity-separator))
  (should (equal (e-chat-transcript-response-separator)
                 (e-chat-composer-separator)))
  (should (equal e-chat-activity-separator
                 (make-string 64 ?┈)))
  (should-not (equal e-chat-activity-separator
                     (e-chat-transcript-response-separator))))



(ert-deftest e-chat-test-activity-separator-uses-dim-activity-face ()
  "Activity round dividers use a dim face inside the activity block."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-separator-face")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 20
                          :payload '(:status started)))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (goto-char (point-min))
          (search-forward e-chat-activity-separator)
          (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                      'e-chat-activity-separator-face)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-reasoning-has-space-after-thought-row ()
  "Reasoning text has a small visual gap after the thought row."
  (let ((buffer (e-chat-test--buffer nil "chat-reasoning-spacer")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started)))
            (e-chat-render-event
             (e-events-make :type 'reasoning-delta
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 1
                            :payload '(:content "planning")))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠋ Thinking for 0min 8sec\n\nplanning"
                     content))
            (should-not (string-match-p
                         "⠋ Thinking for 0min 8sec\nplanning"
                         content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-final-turn-collapses-progress-to-summary ()
  "Settled activity collapses to a navigable turn summary."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-summary"))
        (details-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat-test-activity-details-text)
                     (lambda (&rest _args)
                       (setq details-calls (1+ details-calls))
                       "eager details")))
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started)))
            (e-chat-render-event
             (e-events-make :type 'provider-request-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 63
                            :payload '(:status done)))
            (dotimes (index 2)
              (e-chat-render-event
               (e-events-make :type 'tool-started
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :created-at 64
                              :payload (list :type 'tool-call
                                             :id (format "call-%d" index)
                                             :name "read"))))
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 160
                            :payload '(:status started)))
            (e-chat-render-event
             (e-events-make :type 'provider-request-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 205
                            :payload '(:status done)))
            (e-chat-render-event
             (e-events-make :type 'message-added
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 205
                            :payload '(:message (:role assistant
                                                  :content "Final answer."))))
            (e-chat-render-event
             (e-events-make :type 'turn-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 205))
            (should (= details-calls 0)))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Turn took 3min 25sec, 2 tool calls\\." content))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                             " Final answer\\.\n\n"
                             "Turn took 3min 25sec, 2 tool calls\\.")
                     content))
            (should-not (string-match-p "Thought for 1min 3sec" content)))
          (e-chat-test--focus-block-containing "Turn took 3min 25sec")
          (should (eq (plist-get (e-chat-test--focused-block) :kind)
                      'activity-summary))
          (call-interactively #'e-chat-response-navigation-activate)
          (let ((content (buffer-string)))
            (should (string-match-p "Thought for 1min 3sec" content))
            (should (string-match-p "2 tool calls" content))
            (should (string-match-p "Thought for 0min 45sec" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-action-activity-renders-in-summary ()
  "Action activity events render in the settled turn summary."
  (let ((buffer (e-chat-test--buffer nil "chat-action-summary")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'action-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:capability-id workspace-awareness
                                      :action :focus-buffer)))
          (e-chat-render-event
           (e-events-make :type 'action-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:capability-id workspace-awareness
                                      :action :focus-buffer
                                      :status ok
                                      :result (:content "focused"))))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:message (:role assistant
                                                :content "Done."))))
          (e-chat-render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Turn took 0min 2sec, 1 action\\." content))
            (should-not (string-match-p "1 tool call" content)))
          (e-chat-test--focus-block-containing "Turn took 0min 2sec")
          (call-interactively #'e-chat-response-navigation-activate)
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Action: workspace-awareness/focus-buffer"
                     content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-replayed-active-provider-activity-restores-progress ()
  "Replayed active provider activity restores the running progress block."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-provider-active-replay")
          (e-session-append-message
           store "chat-provider-active-replay"
           '(:role user :content "inspect" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-provider-active-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-provider-active-replay" "turn-1" 'provider-request-started
           '(:status started))
          (puthash "chat-provider-active-replay"
                   '(:id "turn-1" :status running)
                   (e-harness-active-turns harness))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-provider-active-replay")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-provider-active-replay"))
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional _time) 8.0)))
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer (current-buffer))))
            (let ((content (buffer-string)))
              (should (string-match-p
                       "Thinking for 0min [0-9]+sec" content)))
            (should (equal (e-chat-activity-progress-turn-id) "turn-1"))
            (should (plist-get (e-chat-activity-progress-state)
                               :interval-active-p))
            (with-current-buffer (e-chat-test--composer buffer)
              (should (e-chat-composer-active-p)))))
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



(ert-deftest e-chat-test-activity-rerender-preserves-running-status-focus ()
  "Activity redraws preserve point/window focus inside active output."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-status-focus"))
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
            (e-chat-render-event
             (e-events-make :type 'reasoning-delta
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:content "first chunk")))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (goto-char (point-min))
            (search-forward "first chunk")
            (backward-word 1)
            (set-window-point window (point))
            (set-window-start window (point))
            (e-chat-surface-set-window-output-follow window nil)
            (let ((before-point (point))
                  (before-window-point (window-point window))
                  (before-window-start (window-start window)))
              (e-chat-render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nsecond chunk")))
              (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
              (should (= (point) before-point))
              (should (= (window-point window) before-window-point))
              (should (= (window-start window) before-window-start))
              (should (looking-at-p "chunk")))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-activity-rerender-does-not-delete-whole-status ()
  "Streamed activity redraws update changed status text without full deletion."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-status-minimal-delete")))
    (unwind-protect
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
          (e-chat-render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:content "first chunk")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((bounds (e-chat-surface-running-status-bounds))
                (delete-region-fn (symbol-function 'delete-region))
                (full-status-deletes 0))
            (should bounds)
            (cl-letf (((symbol-function 'delete-region)
                       (lambda (start end)
                         (when (and (= start (car bounds))
                                    (= end (cdr bounds)))
                           (setq full-status-deletes
                                 (1+ full-status-deletes)))
                         (funcall delete-region-fn start end))))
              (e-chat-render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nsecond chunk")))
              (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
            (should (= full-status-deletes 0))
            (should (string-match-p "first chunk\nsecond chunk"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-profile-records-pending-activity-redraw ()
  "Enabled dev profiling records the actual deferred activity redraw body."
  (let ((buffer (e-chat-test--buffer nil "chat-profile-activity-redraw"))
        (profile-directory (make-temp-file "e-chat-profile-" t))
        (e-dev-profile-directory nil)
        (e-dev-profile--enabled nil)
        (e-dev-profile--current-file nil)
        (e-dev-profile--latest-file nil))
    (setq e-dev-profile-directory profile-directory)
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "read")))
          (e-dev-profile-start)
          (e-chat-activity-run-pending-redraw)
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "chat.activity-redraw" aggregates nil nil #'equal))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory profile-directory t))))



(ert-deftest e-chat-test-running-activity-is-navigable-while-progress-active ()
  "Running activity summary blocks are navigable before the turn settles."
  (let ((buffer (e-chat-test--buffer nil "chat-running-activity-nav")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "read")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (goto-char (point-min))
          (search-forward "1 tool call")
          (call-interactively #'e-chat-enter-response-navigation)
          (should (eq (plist-get (e-chat-test--focused-block) :kind)
                      'activity))
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-tool-list-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-active-p))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-progress-rerender-preserves-response-navigation ()
  "Progress redraws keep response navigation focused on the same block."
  (let ((buffer (e-chat-test--buffer nil "chat-running-nav-preserve")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-2"
                          :created-at 20))
          (goto-char (point-min))
          (search-forward "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (let ((focused-block-id (e-chat-transcript-focused-block))
                (focused-point (point)))
            (e-chat-activity-advance-progress)
            (should e-chat-response-navigation-mode)
            (should (equal (e-chat-transcript-focused-block) focused-block-id))
            (should (= (point) focused-point))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-mode-line-status-prefers-provider-token-usage ()
  "Mode-line status uses provider token usage before estimated context size."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness :session-id "chat-mode-line-usage")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-session-append-activity-event
           store
           e-chat-session-id
           "turn-1"
           'token-usage
           '(:input-tokens 202598
             :cached-input-tokens 7552
             :output-tokens 419
             :reasoning-output-tokens 139
             :total-tokens 203017))
          (e-chat-surface-set-status "idle" t)
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (equal (e-chat-surface-mode-line-status)
                         "e-chat gpt-5.5/high 78% (203k/258k tok)")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-token-usage-before-compaction-uses-context-estimate ()
  "After compaction, stale provider usage does not hide compacted context size."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (e-chat-context-token-estimate-bytes-per-token 1.0))
    (with-temp-buffer
      (e-chat-mode)
      (setq-local e-current-harness harness)
      (setq-local e-chat-harness harness)
      (setq-local e-chat-session-id "chat-compacted-usage")
      (e-chat-surface-set-redraw-visible t)
      (e-chat-test--create-session store :id e-chat-session-id)
      (e-session-append-message
       store
       e-chat-session-id
       (list :id "old"
             :role 'user
             :content (make-string 1000 ?x)))
      (e-session-append-message
       store
       e-chat-session-id
       '(:id "kept" :role user :content "kept suffix"))
      (e-session-append-activity-event
       store
       e-chat-session-id
       "turn-1"
       'token-usage
       '(:input-tokens 202598
         :cached-input-tokens 7552
         :output-tokens 419
         :reasoning-output-tokens 139
         :total-tokens 203017))
      (e-session-append-compaction
       store
       e-chat-session-id
       "summary"
       :first-kept-entry-id "kept")
      (e-chat-surface-set-status "idle" t)
      (e-ui-work-with-batch-drain
        (e-ui-work-drain-batch :buffer (current-buffer)
                               :owner 'chat-mode-line-status))
      (should (string-match-p "~[0-9]+ pct" mode-name))
      (should-not (string-match-p "203k/258k tok" mode-name)))))



(ert-deftest e-chat-test-set-status-skips-tool-option-materialization ()
  "Ordinary status updates avoid building full turn options."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-status-lightweight"))
         (turn-option-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-harness-turn-options)
                     (lambda (&rest _args)
                       (setq turn-option-calls (1+ turn-option-calls))
                       (error "full turn options should be skipped"))))
            (e-chat-surface-set-status "waiting for provider")
            (e-chat-surface-set-status "done"))
          (should (= turn-option-calls 0))
          (should (string-match-p "gpt-5.5/high" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-profile-records-submit-action ()
  "Enabled dev profiling records the chat submit command."
  (let* ((profile-directory (make-temp-file "e-chat-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (store (e-session-store-create))
         (backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-submit-profile")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "profile submit")
          (e-dev-profile-start)
          (e-chat-submit)
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates))
                 (records
                  (e-dev-profile--read-json-lines e-dev-profile--latest-file))
                 (submit-record
                  (seq-find (lambda (record)
                              (equal (alist-get 'event record) "chat.submit"))
                            records))
                 (metadata (alist-get 'metadata submit-record)))
            (should (alist-get "chat.submit" aggregates nil nil #'equal))
            (should submit-record)
            (should (equal (alist-get 'session-id submit-record)
                           "chat-submit-profile"))
            (should (equal (alist-get 'intent metadata) "submit"))
            (should (= (alist-get 'prompt-chars metadata) 14))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory profile-directory t))))



(ert-deftest e-chat-test-token-usage-event-skips-tool-option-materialization ()
  "Fresh token-usage mode-line refresh avoids full tool option materialization."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-token-usage-lightweight"))
         (turn-option-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-session-append-activity-event
           store
           e-chat-session-id
           "turn-1"
           'token-usage
           '(:input-tokens 1200 :total-tokens 1300))
          (cl-letf (((symbol-function 'e-harness-turn-options)
                     (lambda (&rest _args)
                       (setq turn-option-calls (1+ turn-option-calls))
                       (error "full turn options should be skipped"))))
            (e-chat-render-event
             (e-events-make :type 'token-usage
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:input-tokens 1200
                                       :total-tokens 1300))))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (= turn-option-calls 0))
          (should (string-match-p "1.2k/258k tok" mode-name)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-compaction-finished-refreshes-context-estimate ()
  "Finished compactions immediately refresh stale context estimates."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (e-chat-context-token-estimate-bytes-per-token 1.0)
         (buffer (e-chat-open :harness harness
                              :session-id "chat-compaction-refresh")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-session-append-message
           store
           e-chat-session-id
           (list :id "old"
                 :role 'user
                 :content (make-string 1000 ?x)))
          (e-session-append-message
           store
           e-chat-session-id
           '(:id "kept" :role user :content "kept suffix"))
          (e-chat-surface-set-status "idle" t)
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (string-match-p "~[0-9]+ pct" mode-name))
          (let ((before mode-name))
            (e-session-append-compaction
             store
             e-chat-session-id
             "summary"
             :first-kept-entry-id "kept")
            (e-chat-render-event
             (e-events-make :type 'compaction-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-compact"
                            :payload '(:compaction-id "compaction-1"
                                       :first-kept-entry-id "kept")))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)
                                     :owner 'chat-mode-line-status))
            (should (string-match-p "~[0-9]+ pct" mode-name))
            (should-not (equal mode-name before))))
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



(ert-deftest e-chat-test-compact-session-command-renders_activity ()
  "Manual compaction command renders visible progress and writes a summary."
  (let* ((backend (e-backend-create
                   :name 'summary
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (ignore messages options)
                      (funcall on-item
                               '(:type assistant-message
                                 :content "Compacted summary."))))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness :session-id "chat-compact")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((store (e-harness-sessions e-chat-harness)))
            (e-session-append-message store e-chat-session-id
                                      '(:role user :content "old"))
            (e-session-append-message store e-chat-session-id
                                      '(:role assistant :content "old answer"))
            (e-session-append-message store e-chat-session-id
                                      '(:role user :content "new"))
            (e-chat-compact-session)
            (should-not (e-session-compactions store e-chat-session-id))
            (should
             (e-chat-test--wait-until
              (lambda ()
                (e-session-compactions store e-chat-session-id))))
            (should (equal (plist-get
                            (car (e-session-compactions
                                  store e-chat-session-id))
                            :summary)
                           "Compacted summary."))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (and (string-match-p "Context compaction started"
                                            (buffer-string))
                            (string-match-p "Context compacted into"
                                            (buffer-string))))
                     1.0))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-auto-compaction-renders-distinct-label ()
  "Auto-compaction events render with a distinct visible label."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness :session-id "chat-auto-label")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((store (e-harness-sessions e-chat-harness)))
            (e-harness--emit-turn-event
             e-chat-harness e-chat-session-id "turn-auto" 'compaction-started
             '(:reason auto))
            (e-session-append-compaction
             store e-chat-session-id "summary"
             :metadata '(:reason auto))
            (e-harness--emit-turn-event
             e-chat-harness e-chat-session-id "turn-auto" 'compaction-finished
             '(:compaction-id "compaction-auto" :reason auto))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (and (string-match-p "Auto-compaction started"
                                            (buffer-string))
                            (string-match-p "Auto-compacted context into"
                                            (buffer-string))))
                     1.0))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-overview-mode-neutralizes-evil ()
  "Overview mode keeps Evil from intercepting sidebar navigation keys."
  (let ((buffer (get-buffer-create "*e-chat-overview-evil-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'evil-local-mode)
                   (lambda (argument)
                     (setq-local evil-local-mode
                                 (not (and (numberp argument)
                                           (< argument 0))))
                     (unless evil-local-mode
                       (setq-local evil-state nil)))))
          (with-current-buffer buffer
            (setq-local evil-local-mode t)
            (setq-local evil-state 'normal)
            (e-chat-overview-mode)
            (should-not evil-local-mode)
            (should-not evil-state)
            (should (eq (lookup-key e-chat-overview-mode-map (kbd "RET"))
                        #'e-chat-overview-open-session))
            (should (eq (lookup-key e-chat-overview-mode-map (kbd "j"))
                        #'e-chat-overview-next-session))
            (should (eq (lookup-key e-chat-overview-mode-map (kbd "k"))
                        #'e-chat-overview-previous-session))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-overview-mode-disables-undo ()
  "Overview mode disables undo so repeated re-renders do not accrue history."
  (let ((buffer (get-buffer-create "*e-chat-overview-undo-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-overview-mode)
          (should (eq buffer-undo-list t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-resume-selects-existing-session ()
  "Resuming uses completing-read over persisted sessions and renders transcript."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "resume-me")
          (e-session-append-message
           store "resume-me" '(:id "msg-1" :role user :content "saved hello"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _args)
                       (car collection))))
            (e-chat-test--with-empty-harness-registry
              (let ((e-chat-default-harness-id :chat-test))
                (e-harness-registry-register :chat-test harness)
                (with-current-buffer (e-chat-resume)
                  (should (equal e-chat-session-id "resume-me"))
                  (should (string-match-p "saved hello" (buffer-string))))))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))



(ert-deftest e-chat-test-resume-selects-session-across-chat-instances ()
  "Resume candidates include sessions from every configured chat instance."
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
              (with-current-buffer (e-chat-resume)
                (should (eq e-chat-harness beta-harness))
                (should (eq e-chat-harness-instance-id :chat-beta))
                (should (equal e-chat-session-id "beta-session"))))))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-session-candidates-deduplicate-shared-store-by-owner ()
  "Shared-store sessions appear once under their owning chat instance."
  (let* ((store (e-session-store-create))
         (alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions store)))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions store))))
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
         :metadata '(:name "Beta Session" :harness-instance-id :chat-beta))
        (let ((candidates (e-chat-overview-session-candidates)))
          (should (= (length candidates) 2))
          (should (cl-find-if
                   (lambda (candidate)
                     (and (equal (plist-get candidate :session-id)
                                 "alpha-session")
                          (eq (plist-get candidate :instance-id)
                              :chat-alpha)))
                   candidates))
          (should (cl-find-if
                   (lambda (candidate)
                     (and (equal (plist-get candidate :session-id)
                                 "beta-session")
                          (eq (plist-get candidate :instance-id)
                              :chat-beta)))
                   candidates))
          (should-not
           (cl-find-if
            (lambda (candidate)
              (and (equal (plist-get candidate :session-id)
                          "beta-session")
                   (eq (plist-get candidate :instance-id)
                       :chat-alpha)))
            candidates)))))))



(ert-deftest e-chat-test-session-candidates-include-only-board-root-sessions ()
  "Worker, participant, and pre-board sessions stay out of chat candidates.
Private execution sessions are available through their owning board or worker
surface; switch, resume, active-sessions, and overview list only root chats."
  (let* ((store (e-session-store-create))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions store))))
    (e-chat-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :chat-alpha))
        (e-chat-test--register-chat-instance
         :chat-alpha "Alpha Target" harness t)
        (let* ((binding
                (e-chat-service-create-board
                 :harness harness :id "top-level"
                 :metadata '(:name "Top Level")))
               (board (e-chat-service-binding-board binding)))
          (e-chat-service-create-participant
           board harness :id "private-participant"
           :metadata '(:name "Private Participant")))
        (e-chat-test--create-session store :id "child-by-parent"
                          :metadata '(:name "Child"
                                      :parent-session-id "top-level"))
        (e-chat-test--create-session store :id "child-by-role"
                          :metadata '(:name "Reviewer"
                                      :subagent-role "reviewer"))
        (e-chat-test--create-session store :id "queued-task"
                          :metadata '(:name "Queue worker"
                                      :task-queue-task-id "tsk_000001"))
        (e-session-create store :id "pre-board"
                          :metadata '(:name "Unsupported old session"))
        (let ((ids (mapcar (lambda (candidate)
                             (plist-get candidate :session-id))
                           (e-chat-overview-session-candidates))))
          (should (member "top-level" ids))
          (should-not (member "private-participant" ids))
          (should-not (member "child-by-parent" ids))
          (should-not (member "child-by-role" ids))
          (should-not (member "queued-task" ids))
          (should-not (member "pre-board" ids)))))))



(ert-deftest e-chat-test-session-candidates-exclude-indexed-worker-sessions ()
  "Resume candidates classify indexed workers and private participants."
  (let* ((directory (make-temp-file "e-chat-index-candidates-" t))
         (writer (e-session-persistent-store-create directory))
         (writer-harness
          (e-chat-test--activate-chat-session
           (e-harness-create
            :backend (e-backend-fake-create :items nil)
            :sessions writer))))
    (unwind-protect
        (progn
          (let* ((binding
                  (e-chat-service-create-board
                   :harness writer-harness :id "top-level"
                   :metadata '(:name "Top Level")))
                 (board (e-chat-service-binding-board binding)))
            (e-chat-service-create-participant
             board writer-harness :id "private-participant"
             :metadata '(:name "Private Participant")))
          (e-chat-test--create-session
           writer :id "worker"
           :metadata '(:parent-session-id "top-level"
                       :subagent-role "tool-user"
                       :subagent-label "nested work"))
          (let* ((store (e-session-persistent-index-store-create directory))
                 (harness
                  (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions store))))
            (e-chat-test--with-empty-harness-registry
              (let ((e-chat-default-harness-id :chat-alpha))
                (e-chat-test--register-chat-instance
                 :chat-alpha "Alpha Target" harness t)
                (should (equal
                         (mapcar (lambda (candidate)
                                   (plist-get candidate :session-id))
                                 (e-chat-overview-session-candidates))
                         '("top-level")))))))
      (delete-directory directory t))))



(ert-deftest e-chat-test-session-candidates-order-newest-message-first ()
  "Switch-session candidates list newest last message first."
  (let* ((store (e-session-store-create))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions store))))
    (e-chat-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :chat-alpha))
        (e-chat-test--register-chat-instance
         :chat-alpha "Alpha Target" harness t)
        ;; Create oldest-to-newest, but message recency is the reverse of
        ;; creation order so a creation- or touch-only sort would disagree.
        (e-chat-test--create-session store :id "stale-session")
        (e-chat-test--create-session store :id "fresh-session")
        (e-chat-test--create-session store :id "middle-session")
        ;; Append newest-message session first and oldest last, so the touch
        ;; sequence runs opposite to message recency.  A sort keyed on
        ;; :updated-seq would invert the list; the message-time sort must not.
        (e-session-append-message
         store "fresh-session"
         '(:role user :content "new" :created-at "1970-01-01T01:00:00Z"))
        (e-session-append-message
         store "middle-session"
         '(:role user :content "mid" :created-at "1970-01-01T00:05:00Z"))
        (e-session-append-message
         store "stale-session"
         '(:role user :content "old" :created-at "1970-01-01T00:00:10Z"))
        (let ((ids (mapcar (lambda (candidate)
                             (plist-get candidate :session-id))
                           (e-chat-overview-session-candidates))))
          (should (equal ids
                         '("fresh-session" "middle-session"
                           "stale-session"))))))))



(ert-deftest e-chat-test-resume-reader-uses-consult-preview-when-available ()
  "Resume selection uses Consult preview state when Consult is available."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         selected-state selected-sort)
    (e-chat-test--create-session store :id "resume-me")
    (e-session-append-message
     store "resume-me" '(:id "msg-1" :role user :content "saved hello"))
    (let ((original-require (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'consult)
                       t
                     (funcall original-require feature filename noerror))))
                ((symbol-function 'consult--read)
                 (lambda (collection &rest options)
                   (setq selected-state (plist-get options :state))
                   (setq selected-sort (plist-get options :sort))
                   (car collection))))
        (let* ((sessions (e-harness-session-list harness))
               (candidates
                (mapcar (lambda (session)
                          (list :harness harness
                                :session session
                                :session-id (plist-get session :id)))
                        sessions))
               (labels
                (mapcar #'e-chat-overview-session-candidate-label
                        candidates))
               (candidate
                (e-chat-overview-read-session-candidate candidates)))
          (should (equal (plist-get candidate :session-id) "resume-me"))
          (should (functionp selected-state))
          (should (eq selected-sort nil)))))))



(ert-deftest e-chat-test-overview-renders-sessions-in-recency-order ()
  "Overview rows render latest sessions first and mark unread sessions."
  (let* ((directory (make-temp-file "e-chat-overview-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
        (harness (e-chat-test--activate-chat-session
                  (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "older-session"
                            :metadata '(:name "Older"))
          (e-session-append-message
           store "older-session"
           '(:id "old-assistant" :role assistant :content "older answer"))
          (e-chat-test--create-session store :id "newer-session"
                            :metadata '(:name "Newer"))
          (e-session-append-message
           store "newer-session"
           '(:id "new-assistant" :role assistant :content "newer answer"))
          (let ((buffer (get-buffer-create "*e-chat-overview-test*")))
            (unwind-protect
                (with-current-buffer buffer
                  (e-chat-overview-mode)
                  (e-chat-overview-render harness)
                  (let* ((text (buffer-string))
                         (newer-pos (string-match-p "Newer" text))
                         (older-pos (string-match-p "Older" text)))
                    (should newer-pos)
                    (should older-pos)
                    (should (< newer-pos older-pos))
                    (should (string-match-p "! Newer" text))
                    (should (string-match-p "! Older" text))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))



(ert-deftest e-chat-test-direct-overview-renders-only-board-owning-roots ()
  "A direct-harness overview never renders a private board participant."
  (let* ((harness
          (e-chat-test--activate-chat-session
           (e-harness-create :backend (e-backend-fake-create :items nil))))
         (binding
          (e-chat-service-create-board
           :harness harness :id "overview-root"
           :metadata '(:name "Overview Owner")))
         (board (e-chat-service-binding-board binding))
         (buffer (get-buffer-create "*e-chat-overview-roots-test*")))
    (unwind-protect
        (progn
          (e-chat-service-create-participant
           board harness :id "overview-private"
           :metadata '(:name "Overview Private"))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-overview-render harness)
            (let ((text (buffer-string)))
              (should (= (e-chat-test--count-occurrences
                          "Overview Owner" text)
                         1))
              (should-not (string-match-p "Overview Private" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-overview-compacts-multiline-session-summary ()
  "Overview rows do not expand raw prompt context into the sidebar."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (buffer (get-buffer-create "*e-chat-overview-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "messy-summary")
          (e-session-append-message
           store "messy-summary"
           '(:id "messy-user"
             :role user
             :content "<reference id=\"source\" label=\"very-long-reference-name\">Ask about sidebar</reference>\n\nReferences:\n[source] plan.org"))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-overview-render harness)
            (let ((text (buffer-string)))
              (should (string-match-p "Ask about sidebar" text))
              (should-not (string-match-p "<reference" text))
              (should-not (string-match-p "References:" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-overview-styles-session-row-regions ()
  "Overview rows style title, metadata, and summary as distinct regions."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (buffer (get-buffer-create "*e-chat-overview-style-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "styled-session"
                            :metadata '(:name "Styled Session"))
          (e-session-append-message
           store "styled-session"
           '(:id "styled-user"
             :role user
             :content "summary line"
             :created-at "2026-05-26T21:24:00Z"))
          (e-session-append-message
           store "styled-session"
           '(:id "styled-assistant"
             :role assistant
             :content "answer"
             :created-at "2026-05-26T21:25:42Z"))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-overview-render harness)
            (let ((text (buffer-string)))
              (should (string-match-p "\n\n\\'" text))
              (goto-char (point-min))
              (should (eq (get-text-property (point) 'font-lock-face)
                          'e-chat-overview-unread-face))
              (search-forward "Styled Session")
              (should (eq (get-text-property (match-beginning 0)
                                             'font-lock-face)
                          'e-chat-overview-title-face))
              (search-forward "05-26 21:25")
              (should (eq (get-text-property (match-beginning 0)
                                             'font-lock-face)
                          'e-chat-overview-meta-face))
              (search-forward "summary line")
              (should (eq (get-text-property (match-beginning 0)
                                             'font-lock-face)
                          'e-chat-overview-summary-face))
              (should-not (get-text-property (match-beginning 0)
                                             'mouse-face)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-overview-hides-summary-when-title-is-derived ()
  "Overview rows do not repeat summaries that already produced the title."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (buffer (get-buffer-create "*e-chat-overview-duplicate-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "derived-title")
          (e-session-append-message
           store "derived-title"
           '(:id "derived-user"
             :role user
             :content "this prompt is long enough to become a truncated derived title"
             :created-at "2026-05-26T21:25:42Z"))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-overview-render harness)
            (let ((text (buffer-string)))
              (should (string-match-p "this prompt is long enoug..." text))
              (should-not (string-match-p "truncated derived title" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-overview-j-k-move-by-session-and-preview ()
  "Overview j/k navigation targets whole session rows and opens a preview."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (buffer (get-buffer-create "*e-chat-overview-nav-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "older")
          (e-session-append-message
           store "older"
           '(:id "older-user"
             :role user
             :content "older prompt"
             :created-at "2026-05-26T21:24:00Z"))
          (e-chat-test--create-session store :id "newer")
          (e-session-append-message
           store "newer"
           '(:id "newer-user"
             :role user
             :content "newer prompt"
             :created-at "2026-05-26T21:25:00Z"))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-overview-render harness)
            (goto-char (point-min))
            (should (equal (e-chat-overview-session-id-at-point) "newer"))
            (e-chat-overview-next-session)
            (should (equal (e-chat-overview-session-id-at-point) "older"))
            (with-current-buffer
                (e-chat-overview-resume-preview-buffer-name)
              (should (string-match-p "older prompt" (buffer-string))))
            (e-chat-overview-previous-session)
            (should (equal (e-chat-overview-session-id-at-point) "newer"))
            (with-current-buffer
                (e-chat-overview-resume-preview-buffer-name)
              (should (string-match-p "newer prompt" (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when-let ((preview
                  (get-buffer (e-chat-overview-resume-preview-buffer-name))))
        (kill-buffer preview)))))



(ert-deftest e-chat-test-overview-renders-and-opens-owning-chat-instance ()
  "Overview rows carry owning instance metadata when session ids collide."
  (let* ((alpha-store (e-session-store-create))
         (beta-store (e-session-store-create))
         (alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions alpha-store)))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions beta-store)))
         (buffer (get-buffer-create "*e-chat-overview-instances-test*")))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-alpha))
            (e-chat-test--register-chat-instance
             :chat-alpha "Alpha Target" alpha-harness t)
            (e-chat-test--register-chat-instance
             :chat-beta "Beta Target" beta-harness)
            (e-chat-test--create-session alpha-store :id "shared-session"
                              :metadata '(:name "Alpha Session"))
            (e-chat-test--create-session beta-store :id "shared-session"
                              :metadata '(:name "Beta Session"))
            (with-current-buffer buffer
              (e-chat-overview-mode)
              (e-chat-overview-render)
              (let ((text (buffer-string)))
                (should (string-match-p "Alpha Target" text))
                (should (string-match-p "Beta Target" text)))
              (goto-char (point-min))
              (search-forward "Beta Target")
              (let ((chat-buffer (e-chat-overview-open-session)))
                (with-current-buffer chat-buffer
                  (should (eq e-chat-harness beta-harness))
                  (should (eq e-chat-harness-instance-id :chat-beta))
                  (should (equal e-chat-session-id "shared-session")))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-overview-deduplicates-shared-store-by-owner ()
  "Overview rows show shared-store sessions only under their owning instance."
  (let* ((store (e-session-store-create))
         (alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions store)))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions store)))
         (buffer (get-buffer-create "*e-chat-overview-shared-store-test*")))
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
            (with-current-buffer buffer
              (e-chat-overview-mode)
              (e-chat-overview-render)
              (let ((text (buffer-string)))
                (should (= (e-chat-test--count-occurrences
                            "Alpha Session" text)
                           1))
                (should (= (e-chat-test--count-occurrences
                            "Beta Session" text)
                           1))
                (should (string-match-p "Alpha Target.*Alpha Session" text))
                (should (string-match-p "Beta Target.*Beta Session" text))
                (should-not
                 (string-match-p "Alpha Target.*Beta Session" text))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-chat-buffers))))



(ert-deftest e-chat-test-sidebar-toggle-opens-and-closes-overview ()
  "The planned sidebar toggle command toggles the overview side window."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (e-chat-overview-buffer-name "*e-chat-overview-toggle-test*")
         opened-buffer)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "toggle-me"
                              :metadata '(:name "Toggle Me"))
            (should (commandp 'e-chat-sidebar-toggle))
            (e-chat-sidebar-toggle)
            (setq opened-buffer (get-buffer e-chat-overview-buffer-name))
            (should (buffer-live-p opened-buffer))
            (should (get-buffer-window opened-buffer t))
            (should (eq (window-buffer (selected-window)) opened-buffer))
            (e-chat-sidebar-toggle)
            (should-not (buffer-live-p opened-buffer))
            (should-not (get-buffer-window opened-buffer t))))
      (when (buffer-live-p opened-buffer)
        (kill-buffer opened-buffer)))))



(ert-deftest e-chat-test-active-sessions-builds-picker-spec ()
  "The active sessions command uses e-picker with chat session callbacks."
  (let* ((harness-a (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions (e-session-store-create)))
         (harness-b (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions (e-session-store-create)))
         (session-a '(:id "alpha-session"
                      :title "Alpha Session"
                      :summary "Alpha summary"
                      :message-count 1
                      :messages ((:id "alpha-user"
                                   :role user
                                   :content "Alpha prompt")
                                  (:id "alpha-assistant"
                                   :role assistant
                                   :content "Alpha final response"))
                      :created-at "2026-06-20T10:00:00Z"
                      :loaded t))
         (session-b '(:id "beta-session"
                      :title "Beta Session"
                      :summary "Beta summary"
                      :message-count 2
                      :latest-assistant-marker "beta-assistant"
                      :messages ((:id "beta-user"
                                   :role user
                                   :content "Beta prompt")
                                  (:id "beta-assistant"
                                   :role assistant
                                   :content "Beta final response"))
                      :created-at "2026-06-20T11:00:00Z"
                      :loaded t))
         (candidates (list (list :harness harness-a
                                 :session session-a
                                 :session-id "alpha-session")
                           (list :harness harness-b
                                 :session session-b
                                 :session-id "beta-session"
                                 :instance-id :beta)))
         spec preview-text opened)
    (cl-letf (((symbol-function 'e-chat-overview-active-session-candidates)
               (lambda () candidates))
              ((symbol-function 'e-chat-service-active-turn-p)
               (lambda (_harness session-id)
                 (equal session-id "alpha-session")))
              ((symbol-function 'e-context-status-text)
               (lambda (&rest _args) "ctx model/effort 10%"))
              ((symbol-function 'e-picker-open)
               (lambda (&rest args)
                 (setq spec args)
                 nil))
              ((symbol-function 'e-chat-open-session)
               (lambda (harness session-id display &optional instance-id)
                 (setq opened
                       (list :harness harness
                             :session-id session-id
                             :display display
                             :instance-id instance-id)))))
      (e-chat-active-sessions)
      (should (eq (plist-get spec :name) 'active-sessions))
      (should (= (plist-get spec :initial-candidate-limit) 15))
      (should (= (plist-get spec :candidate-limit-step) 15))
      (should (equal (funcall (plist-get spec :candidates)) candidates))
      (should (string-match-p
               "Beta Session"
               (funcall (plist-get spec :candidate-key)
                        (cadr candidates))))
      (should (string-match-p
               "ctx model/effort"
               (funcall (plist-get spec :candidate-line)
                        (cadr candidates))))
      (should (string-prefix-p
               "◆ Alpha Session"
               (funcall (plist-get spec :candidate-line)
                        (car candidates))))
      (should (string-prefix-p
               "● Beta Session"
               (funcall (plist-get spec :candidate-line)
                        (cadr candidates))))
      (should-not (string-match-p
                   "!"
                   (funcall (plist-get spec :candidate-line)
                            (cadr candidates))))
      (with-temp-buffer
        (funcall (plist-get spec :preview) (car candidates) (current-buffer))
        (setq preview-text (buffer-string))
        (should (string-match-p "Alpha prompt" preview-text))
        (should (string-match-p "Alpha final response" preview-text))
        (goto-char (point-min))
        (should (re-search-forward "Alpha final response" nil t))
        (should (memq 'e-chat-final-assistant-face
                      (ensure-list (get-text-property
                                    (match-beginning 0)
                                    'face))))
        (should-not (get-text-property (match-beginning 0) 'read-only))
        (should-not (get-text-property (match-beginning 0) 'field))
        (should-not (get-text-property (match-beginning 0) 'e-chat-block-id)))
      (funcall (plist-get spec :on-select) (cadr candidates))
      (should (eq (plist-get opened :harness) harness-b))
      (should (equal (plist-get opened :session-id) "beta-session"))
      (should (eq (plist-get opened :display) t))
      (should (eq (plist-get opened :instance-id) :beta)))))



(ert-deftest e-chat-test-active-session-line-reuses-fresh-status-snapshot ()
  "Active-session picker rows reuse fresh context-status snapshots."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (candidate (list :harness harness
                          :session '(:id "picker-status-cache"
                                      :title "Picker Status"
                                      :message-count 1
                                      :messages ((:id "picker-user"
                                                  :role user
                                                  :content "prompt"))
                                      :loaded t)
                          :session-id "picker-status-cache"))
         (status-cache (make-hash-table :test #'equal))
         (calls 0))
    (e-chat-test--create-session store :id "picker-status-cache")
    (cl-letf (((symbol-function 'e-context-budget-status)
               (lambda (&rest _args)
                 (setq calls (1+ calls))
                 '(:model "gpt-5.5"
                   :reasoning-effort "high"
                   :used-tokens 123
                   :window 1000
                   :approximate t)))
              ((symbol-function 'e-chat-overview-session-unread-p)
               (lambda (&rest _args) nil)))
      (let ((e-context-status-estimate-cache-seconds 100))
        (should (string-match-p
                 "ctx gpt-5.5/high ~13% (~123/1k tok)"
                 (e-chat-overview-active-session-line candidate status-cache)))
        (should (string-match-p
                 "ctx gpt-5.5/high ~13% (~123/1k tok)"
                 (e-chat-overview-active-session-line candidate status-cache)))))
    (should (= calls 1))))



(ert-deftest e-chat-test-active-session-preview-avoids-unloaded-index-session-load ()
  "Active-session preview renders metadata for unloaded index sessions."
  (let* ((directory (make-temp-file "e-chat-active-" t))
         (store (e-session-persistent-store-create directory))
         (e-chat-session-summary-preview-max-chars 6)
         loaded)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "unloaded-active"
                            :metadata '(:name "Unloaded active"))
          (e-session-append-message
           store "unloaded-active"
           '(:id "msg-1" :role user :content "last prompt"))
          (e-session-append-message
           store "unloaded-active"
           '(:id "msg-2" :role assistant :content "last response"))
          (let* ((indexed-store
                  (e-session-persistent-index-store-create directory))
                 (harness (e-harness-create
                           :backend (e-backend-fake-create :items nil)
                           :sessions indexed-store))
                 (session (car (e-harness-session-list harness)))
                 (candidate
                  (list :harness harness
                        :session session
                        :session-id "unloaded-active")))
            (should-not (plist-get session :loaded))
            (cl-letf (((symbol-function 'e-session-load-session)
                       (lambda (&rest _args)
                         (setq loaded t)
                         (error "preview loaded transcript"))))
              (with-temp-buffer
                (e-chat-overview-active-session-preview candidate (current-buffer))
                (let ((text (buffer-string)))
                  (should-not loaded)
                  (should (string-match-p "last p…" text))
                  (should-not (string-match-p "last prompt" text))
                  (should-not (string-match-p "last response" text)))))))
      (delete-directory directory t))))



(ert-deftest e-chat-test-active-session-preview-marks-session-read ()
  "Showing a session in the active-session preview records its latest response."
  (let* ((store (e-session-store-create))
        (harness (e-harness-create
                  :backend (e-backend-fake-create :items nil)
                  :sessions store))
        candidate)
    (e-chat-test--create-session store :id "preview-read"
                      :metadata '(:name "Preview read"))
    (e-session-append-message
     store "preview-read"
     '(:id "msg-1" :role user :content "prompt"))
    (e-session-append-message
     store "preview-read"
     '(:id "msg-2" :role assistant :content "response"))
    (setq candidate
          (list :harness harness
                :session (car (e-harness-session-list harness))
                :session-id "preview-read"))
    (should (e-chat-overview-session-unread-p
             harness
             (plist-get candidate :session)))
    (with-temp-buffer
      (e-chat-overview-active-session-preview candidate (current-buffer)))
    (should-not (e-chat-overview-session-unread-p
                 harness
                 (plist-get candidate :session)))))



(ert-deftest e-chat-test-board-progress-uses-presentation-turn-identity ()
  "Board-private turn ids do not make presentation progress look stale."
  (let ((buffer (e-chat-test--buffer nil "chat-board-progress-identity"))
        harness
        session-id)
    (unwind-protect
        (with-current-buffer buffer
          (setq harness e-chat-harness
                session-id e-chat-session-id)
          (let* ((binding (e-chat-service-binding harness session-id))
                 (attachment (e-chat-service-binding-attachment binding))
                 (participant-id
                  (e-board-registry-participant-id
                   (e-board-runtime-attachment-participant attachment)))
                 (board (e-board-registry-board-source-board
                         (e-chat-service-binding-board binding)))
                 (presentation-turn-id "msg-progress-input")
                 (source-turn-id '(board participant source-turn))
                 (author (format "participant:%s" participant-id)))
            (e-board-post-input
             board :id presentation-turn-id :author "test-client" :tags '(main)
             :content "inspect" :source-input-key '(test-progress 1 0))
            (puthash session-id
                     (list :id source-turn-id :status 'running)
                     (e-harness-active-turns harness))
            (e-board-post-activity
             board :id "progress-turn-started" :author author
             :subject-participant-id participant-id
             :source-turn-id source-turn-id :activity-kind 'turn-started
             :tags '(main) :reply-to-message-ids (list presentation-turn-id)
             :source-activity-key '(test-progress 1 1))
            (e-board-post-activity
             board :id "progress-provider-started" :author author
             :subject-participant-id participant-id
             :source-turn-id source-turn-id
             :activity-kind 'provider-request-started :tags '(main)
             :attributes '(:status started)
             :reply-to-message-ids (list presentation-turn-id)
             :source-activity-key '(test-progress 1 2))
            (e-board-post-activity
             board :id "progress-provider-finished" :author author
             :subject-participant-id participant-id
             :source-turn-id source-turn-id
             :activity-kind 'provider-request-finished :tags '(main)
             :attributes '(:status done)
             :reply-to-message-ids (list presentation-turn-id)
             :source-activity-key '(test-progress 1 3))
            (e-chat-test--dispatch-observed-event
             (list :type 'turn-started
                   :session-id session-id
                   :turn-id presentation-turn-id
                   :board-id (e-board-registry-board-id
                              (e-chat-service-binding-board binding))
                   :subject-participant-id participant-id
                   :selected-participant-p t
                   :source-turn-id source-turn-id
                   :created-at 0))
            (e-chat-test--dispatch-observed-event
             (list :type 'provider-request-started
                   :session-id session-id
                   :turn-id presentation-turn-id
                   :board-id (e-board-registry-board-id
                              (e-chat-service-binding-board binding))
                   :subject-participant-id participant-id
                   :selected-participant-p t
                   :source-turn-id source-turn-id
                   :created-at 1
                   :payload '(:status started)))
            (e-chat-test--dispatch-observed-event
             (list :type 'provider-request-finished
                   :session-id session-id
                   :turn-id presentation-turn-id
                   :board-id (e-board-registry-board-id
                              (e-chat-service-binding-board binding))
                   :subject-participant-id participant-id
                   :selected-participant-p t
                   :source-turn-id source-turn-id
                   :created-at 2
                   :payload '(:status done)))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (should (equal (e-chat-activity-progress-turn-id)
                           presentation-turn-id))
            (should
             (equal
              (plist-get
               (e-chat-service-active-turn harness session-id)
               :id)
              presentation-turn-id))
            (should
             (equal
              (plist-get
               (plist-get (e-chat-service-state harness session-id)
                          :active-turn)
               :id)
              presentation-turn-id))
            (should
             (equal
              (plist-get
               (gethash session-id
                        (e-chat-service-active-turns harness))
               :id)
              presentation-turn-id))
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional _time) 8.0)))
              (e-chat-activity-advance-progress)
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer (current-buffer))))
            (should (equal (e-chat-activity-progress-turn-id)
                           presentation-turn-id))
            (should (plist-get (e-chat-activity-progress-state)
                               :interval-active-p))
            (should (string-match-p
                     "Working for [0-9]+min [0-9]+sec" (buffer-string)))))
      (when (and harness session-id)
        (remhash session-id (e-harness-active-turns harness)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-active-sessions-errors-without-candidates ()
  "The active sessions command reports an empty session list."
  (cl-letf (((symbol-function 'e-chat-overview-session-candidates)
             (lambda () nil)))
    (should-error (e-chat-active-sessions) :type 'user-error)))

;;; e-chat-presentation-integration-test--end

(provide 'e-chat-presentation-integration-test)

;;; e-chat-presentation-integration-test.el ends here
