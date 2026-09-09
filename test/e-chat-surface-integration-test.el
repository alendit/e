;;; e-chat-surface-integration-test.el --- Chat surface integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic batch integration coverage for the interactive chat surface.
;; It uses the fake backend and real Emacs buffer/window objects, but it does
;; not claim graphical redisplay or user-visible viewport behavior.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-backend)
(require 'e-board-runtime)
(require 'e-chat)
(require 'e-chat-session)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-task-queue)
(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(load (expand-file-name
       "../e2e/e-board-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(load (expand-file-name
       "e-test-environment-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(declare-function e-board-e2e-reset-runtime "e-board-e2e-support")
(declare-function e-board-e2e-drain-session "e-board-e2e-support")
(declare-function e-board-e2e-wait-batch "e-board-e2e-support")
(declare-function e-board-e2e-wait-until "e-board-e2e-support")
(declare-function evil-mode "evil-core")
(declare-function evil-insert-state "evil-states")
(declare-function evil-local-mode "evil-core")
(defvar evil-state)

(defun e-chat-surface-integration--load-evil ()
  "Load Evil only when it is available in the isolated test environment."
  (or (featurep 'evil) (require 'evil nil t)))

(ert-deftest e-chat-surface-integration-test-composer-submits-below-transcript ()
  "A displayed chat keeps input in its pane and responses in its transcript."
  (e-board-e2e-reset-runtime)
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "surface answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (session-id "chat-surface-e2e")
         (buffer (e-chat-open :harness harness :session-id session-id))
         (window-configuration (current-window-configuration)))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (e-chat-surface-after-display-buffer buffer)
          (with-current-buffer buffer
            (let* ((composer (e-chat-surface-composer-buffer))
                   (transcript-window (get-buffer-window buffer t))
                   (composer-window
                    (e-chat-surface-composer-window transcript-window)))
              (should (window-live-p transcript-window))
              (should (window-live-p composer-window))
              (should (eq (window-buffer composer-window) composer))
              (should (> (nth 1 (window-edges composer-window))
                         (nth 1 (window-edges transcript-window))))
              (should (= (window-body-height composer-window) 4))
              (should buffer-read-only)
              (with-current-buffer composer
                (should (e-chat-composer-start-position)))
              ;; Escape always leaves the composer.  An empty transcript has
              ;; no response-navigation block yet, but it is still the
              ;; correct focus target.
              (select-window composer-window)
              (with-current-buffer composer
                (e-chat-composer-enter-navigation))
              (should (eq (selected-window) transcript-window))
              (with-current-buffer buffer
                (should-not e-chat-response-navigation-mode))
              (select-window composer-window)
              (with-current-buffer composer
                (goto-char (point-max))
                (insert "surface prompt")
                (e-chat-submit))
              (e-board-e2e-drain-session harness session-id)
              (should (equal (plist-get
                              (e-board-e2e-wait-batch harness session-id 1.0)
                              :status)
                             'done))
              (should
               (e-board-e2e-wait-until
                (lambda ()
                  (with-current-buffer buffer
                    (string-match-p "surface answer" (buffer-string))))
                1.0))
              (with-current-buffer buffer
                (should (string-match-p "surface prompt" (buffer-string)))
                (should (string-match-p "surface answer" (buffer-string))))
              (with-current-buffer composer
                (should (equal (e-chat-composer-text) "")))
              (select-window composer-window)
              (with-current-buffer composer
                (e-chat-composer-enter-navigation))
              (should (eq (selected-window) transcript-window))
              (with-current-buffer buffer
                (let ((last-command-event ?x))
                  (should-error (self-insert-command 1)
                                :type 'buffer-read-only))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (set-window-configuration window-configuration)
      (e-chat-test--kill-chat-buffers)
      ;; This test exercises the process-global Board runtime and may leave
      ;; zero-delay observer/input callbacks queued when the old window
      ;; configuration is restored.  Model process teardown only after that
      ;; restoration so later owner tests cannot inherit those callbacks.
      (e-board-e2e-reset-runtime))))

(ert-deftest e-chat-surface-integration-test-evil-escape-routes-transcript-commands ()
  "One real Evil Escape moves input focus to transcript navigation commands."
  (e-test-require-capability
   (e-chat-surface-integration--load-evil)
   "Required test-only Evil package is unavailable")
  (let* ((evil-mode-was-enabled (bound-and-true-p evil-mode))
         (backend (e-backend-fake-create
                   :items '((:type assistant-message :content "evil answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (session-id "chat-surface-evil-e2e")
         buffer
         (window-configuration (current-window-configuration))
         details-buffer)
    (unwind-protect
        (progn
          (evil-mode 1)
          (e-chat-startup)
          (e-board-e2e-reset-runtime)
          (setq buffer (e-chat-open :harness harness :session-id session-id))
          (switch-to-buffer buffer)
          (e-chat-surface-after-display-buffer buffer)
          (with-current-buffer buffer
            (let* ((composer (e-chat-surface-composer-buffer))
                   (transcript-window (get-buffer-window buffer t))
                   (composer-window
                    (e-chat-surface-composer-window transcript-window)))
              (select-window composer-window)
              (with-current-buffer composer
                (evil-local-mode 1)
                (evil-insert-state)
                (goto-char (point-max))
                (insert "evil prompt")
                (e-chat-submit))
              (e-board-e2e-drain-session harness session-id)
              (should (equal (plist-get
                              (e-board-e2e-wait-batch harness session-id 1.0)
                              :status)
                             'done))
              (should
               (e-board-e2e-wait-until
                (lambda ()
                  (with-current-buffer buffer
                    (string-match-p "evil answer" (buffer-string))))
                1.0))
              (select-window composer-window)
              (with-current-buffer composer
                (evil-insert-state)
                (should (eq evil-state 'insert))
                (should (eq (key-binding (kbd "<escape>"))
                            #'e-chat-composer-enter-navigation)))
              (execute-kbd-macro (kbd "<escape>"))
              (should (eq (selected-window) transcript-window))
              (should (eq (current-buffer) buffer))
              (should e-chat-response-navigation-mode)
              (should (eq (key-binding (kbd "RET"))
                          #'e-chat-response-navigation-activate))
              (should (eq (key-binding (kbd "d"))
                          #'e-chat-response-navigation-details))
              ;; Batch Emacs proves the active public command bindings; the
              ;; graphical tier owns command-loop dispatch and redisplay.
              (setq details-buffer
                    (call-interactively (key-binding (kbd "d"))))
              (should (buffer-live-p details-buffer))
              (with-current-buffer details-buffer
                (should (string-match-p "Turn:" (buffer-string)))))))
      (when (buffer-live-p details-buffer)
        (kill-buffer details-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (set-window-configuration window-configuration)
      (unless evil-mode-was-enabled
        (evil-mode -1)))))

(ert-deftest e-chat-surface-integration-test-insert-state-requires-local-evil ()
  "Loaded Evil changes state only for a composer with local Evil enabled."
  (e-test-require-feature 'evil 'evil)
  (let ((buffer (generate-new-buffer " *e-chat-local-evil-state-test*"))
        (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'evil-insert-state)
                     (lambda () (setq calls (1+ calls)))))
            (setq-local evil-local-mode nil)
            (e-chat-composer--enter-insert-state)
            (should (= calls 0))
            (setq-local evil-local-mode t)
            (e-chat-composer--enter-insert-state)
            (should (= calls 1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-surface-integration-test-new-chat-is-board-native ()
  "The real fresh-chat command creates only board-native persistent state."
  (let* ((e-board--registry (make-hash-table :test 'equal))
         (e-board-registry--boards (make-hash-table :test 'equal))
         (e-board-registry--unsettled-pickup-count 0)
         (e-board-registry--unsettled-effect-count 0)
         (e-board-registry--unsettled-routing-count 0)
         (e-board-registry--unsettled-generation 0)
         (e-board-runtime--attachments (make-hash-table :test 'equal))
         (e-board-runtime--session-attachments (make-hash-table :test 'equal))
         (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
         (e-board-runtime--admission-open-p t)
         (e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-chat-default-harness-id :chat-e2e)
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         buffer)
    (e-board-e2e-reset-runtime)
    (e-harness-activate-capability
     harness (e-chat-session-capability-create))
    (e-harness-registry-register :chat-e2e harness)
    (unwind-protect
        (progn
          (setq buffer (e-chat-new))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (let* ((board (e-board-registry-get e-chat-board-id))
                   (session (e-session-local-state (e-harness-sessions harness)
                                           e-chat-session-id))
                   (state (plist-get session :board-session-state)))
              (should (equal e-chat-harness harness))
              (should (eq (e-board-registry-board-state board) 'active))
              (should (equal (plist-get state :board-id) e-chat-board-id))
              (should (equal (plist-get state :principal)
                             (e-board-registry-board-principal board)))
              (should (equal (plist-get state :association-role) "owner")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-window-buffer-change-defers-composer-creation ()
  "A generic chat buffer change restores its paired composer after redisplay."
  (let* ((buffer (e-chat-test--buffer nil "chat-deferred-restored-window"))
         (transcript-window (display-buffer buffer))
         (composer (e-chat-surface-composer-buffer buffer)))
    (unwind-protect
        (progn
          (select-window transcript-window)
          (set-frame-parameter nil 'e-chat-selected-surface nil)
          (e-chat-surface-activate-after-window-change
           (selected-frame))
          (should (e-ui-work-pending buffer))
          (with-current-buffer buffer
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer buffer)))
          (should-not (e-ui-work-pending buffer))
          (should (eq (window-buffer (selected-window)) composer)))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-switch-to-buffer-restores-composed-surface ()
  "Generic buffer switching restores a composed chat's editable surface."
  (let* ((origin (get-buffer-create " *e-chat switch origin*"))
         (buffer (e-chat-test--buffer nil "chat-switch-composed-surface"))
         (window (selected-window))
         composer)
    (unwind-protect
        (progn
          (delete-other-windows window)
          (switch-to-buffer origin)
          (switch-to-buffer buffer)
          (set-frame-parameter nil 'e-chat-selected-surface nil)
          (e-chat-surface-activate-after-window-change
           (selected-frame))
          (with-current-buffer buffer
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer buffer)))
          (setq composer (e-chat-surface-composer-buffer buffer))
          (should (buffer-live-p composer))
          (should (eq (window-buffer (selected-window)) composer))
          (should (eq (get-buffer-window buffer t) window))
          (should (eq (window-atom-root window)
                      (window-atom-root (selected-window)))))
      (when (buffer-live-p origin)
        (kill-buffer origin))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-loaded-session-reprojection-restores-following-tail ()
  "Async session replay keeps an activated transcript at its new output tail."
  (let* ((history (mapconcat (lambda (number)
                               (format "loaded history line %d" number))
                             (number-sequence 1 300)
                             "\n"))
         (buffer (e-chat-test--buffer nil "chat-loaded-following-tail"))
         transcript-window
         composer-window
         loading-tail)
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
            ;; Restart first displays and activates a short loading projection.
            (let ((inhibit-read-only t))
              (e-chat-clear t)
              (e-chat-transcript-render-session-loading
               '(:summary "loaded question")))
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t))
            (set-buffer buffer)
            (e-chat-surface-after-display-buffer buffer)
            (setq loading-tail (point-max))
            (should (= (window-point transcript-window) loading-tail))
            (should (e-chat-surface-window-follows-output-p transcript-window))
            ;; Load completion clears that projection and inserts the actual
            ;; transcript without crossing another display/focus boundary.
            (e-chat-attach-buffer
             buffer e-chat-harness e-chat-session-id
             e-chat-harness-instance-id)
            (should (> (point-max) loading-tail))
            (should (= (window-point transcript-window) (point-max)))
            (should (>= (window-end transcript-window t) (point-max)))
            (should (eq (selected-window) composer-window))))
      (set-frame-parameter nil 'e-chat-selected-surface nil)
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-pinned-short-transcript-aligns-output-bottom ()
  "A short composed transcript uses window-local space above pinned output."
  (let* ((configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-short-output-bottom"))
         transcript-window composer-window spacer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq transcript-window (selected-window))
          (set-window-buffer transcript-window buffer)
          (with-current-buffer buffer
            (e-chat-test--render-turn "turn-1" 10 11 "question" "answer")
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t))
            (set-buffer buffer)
            (e-chat-surface-show-latest-output transcript-window)
            (setq spacer
                  (cl-find-if
                   (lambda (overlay)
                     (and (overlay-get overlay 'before-string)
                          (eq (overlay-get overlay 'window)
                              transcript-window)))
                   (overlays-at (point-min))))
            (should (overlayp spacer))
            (should (eq (overlay-get spacer 'window) transcript-window))
            (should (> (length (overlay-get spacer 'before-string)) 0))
            (should (e-chat-surface-window-follows-output-p transcript-window))
            (e-chat-surface-set-window-output-follow transcript-window nil)
            (should-not (overlay-buffer spacer))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-surface-activation-survives-late-window-restore ()
  "One deferred surface activation wins a host restoring stale scrollback."
  (let* ((history (mapconcat (lambda (number)
                               (format "history line %d" number))
                             (number-sequence 1 300)
                             "\n"))
         (buffer (e-chat-test--buffer nil "chat-surface-activation-late"))
         transcript-window composer-window)
    (unwind-protect
        (progn
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn "turn-1" 10 11 "question" history)
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t))
            (set-buffer buffer)
            (let ((stale-point (point-min))
                  (tail (point-max))
                  (surface (cons buffer transcript-window)))
              (set-window-point transcript-window stale-point)
              (set-window-start transcript-window stale-point)
              (e-chat-surface-set-window-output-follow transcript-window nil)
              (e-chat-surface-activate surface)
              (should (e-ui-work-pending
                       buffer :owner 'surface-activation))
              (should (= (window-point transcript-window) tail))
              ;; Doom workspace restoration can put the old point back after
              ;; activation returns.  The one deferred retry wins that race.
              (set-window-point transcript-window stale-point)
              (set-window-start transcript-window stale-point)
              (should
               (e-chat-test--wait-until
                (lambda ()
                  (= (window-point transcript-window) tail))
                0.2)))))
      (set-frame-parameter nil 'e-chat-selected-surface nil)
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-entering-surface-pair-focuses-composer ()
  "Entering a chat pair focuses input without breaking transcript navigation."
  (let* ((configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-pair-entry-focus"))
         (external-buffer (generate-new-buffer " *e-chat focus external*"))
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
                  (e-chat-surface-display-composer transcript-window)))
          (should (eq (window-atom-root transcript-window)
                      (window-atom-root composer-window)))
          ;; Entering through the transcript routes input to the composer.
          (select-window external-window)
          (set-frame-parameter nil 'e-chat-selected-surface nil)
          (select-window transcript-window)
          (e-chat-surface-activate-selected)
          (should (eq (selected-window) composer-window))
          ;; Once inside the surface, selecting the transcript is explicit
          ;; response navigation and must not be redirected back to input.
          (select-window transcript-window)
          (e-chat-surface-activate-selected)
          (should (eq (selected-window) transcript-window)))
      (set-frame-parameter nil 'e-chat-selected-surface nil)
      (set-window-configuration configuration)
      (when (buffer-live-p external-buffer)
        (kill-buffer external-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-selection-activates-transcript-once ()
  "Entering a composer tails its transcript, but staying there preserves scrollback."
  (let* ((history (mapconcat (lambda (number)
                               (format "history line %d" number))
                             (number-sequence 1 300)
                             "\n"))
         (buffer (e-chat-test--buffer nil "chat-composer-surface-entry"))
         transcript-window composer-window)
    (unwind-protect
        (progn
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn "turn-1" 10 11 "question" history)
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t))
            (set-buffer buffer)
            (let ((surface (cons buffer transcript-window))
                  (tail (point-max)))
              (set-window-point transcript-window (point-min))
              (set-window-start transcript-window (point-min))
              (e-chat-surface-set-window-output-follow transcript-window nil)
              (set-frame-parameter nil 'e-chat-selected-surface nil)
              (e-chat-surface-activate-selected)
              (should (equal (e-chat-surface-selected-chat-surface) surface))
              (should (= (window-point transcript-window) tail))
              (should (eq (selected-window) composer-window))
              (e-ui-work-cancel-matching buffer 'surface-activation)
              (set-window-point transcript-window (point-min))
              (set-window-start transcript-window (point-min))
              (e-chat-surface-set-window-output-follow transcript-window nil)
              (e-chat-surface-activate-selected)
              (should (= (window-point transcript-window) (point-min)))
              (should-not (e-ui-work-pending
                           buffer :owner 'surface-activation))))
      (set-frame-parameter nil 'e-chat-selected-surface nil)
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))))

(provide 'e-chat-surface-integration-test)

;;; e-chat-surface-integration-test.el ends here
