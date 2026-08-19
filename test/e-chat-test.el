;;; e-chat-test.el --- Tests for e chat presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the chat buffer presentation shell.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-bayesian-reasoning)
(require 'e-chat)
(require 'e-chat-session)
(require 'e-context-inspection)
(require 'e-dev-profile)
(require 'e-emacs-base)
(require 'e-events)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(load (expand-file-name "e-tools-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-layer)
(require 'e-prompts)
(require 'e-request)
(require 'e-store)
(require 'e-work)
(require 'e-ui-work)
(require 'e-tools)

(defvar evil-local-mode)
(defvar evil-state)
(defvar doom-leader-alt-key)
(defvar doom-leader-map)
(defvar persp-activated-functions)

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

(defun e-chat-test--buffer (&optional items session-id)
  "Return a chat buffer backed by fake backend ITEMS and SESSION-ID.
The returned buffer is not displayed in a window, so redraw gating would
otherwise defer progress and activity repaints.  Force the visible path for
tests, matching how the buffer behaves when shown to a user."
  (let* ((backend (e-backend-fake-create :items items))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id (or session-id "chat-test"))))
    (with-current-buffer buffer
      (setq e-chat--assume-redraw-visible t))
    buffer))

(defun e-chat-test--create-session (store &rest arguments)
  "Create one board-native test session in STORE from ARGUMENTS."
  (let* ((session (apply #'e-session-create store arguments))
         (session-id (plist-get session :id)))
    (e-session-declare-board-state
     store session-id (format "chat:%s" session-id)
     (format "test-board:%s" session-id))
    session))

(ert-deftest e-chat-test-composed-surface-keeps-draft-outside-transcript ()
  "Transcript rendering must not recreate or alter the separate composer."
  (let* ((e-chat--surface-composition-enabled t)
         (buffer (e-chat-test--buffer nil "chat-composed-surface")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((composer e-chat--surface-composer-buffer)
                (draft "keep this unsent draft")
                composer-tick)
            (should (buffer-live-p composer))
            (should (eq (buffer-local-value 'e-chat--surface-transcript-buffer
                                            composer)
                        buffer))
            (with-current-buffer composer
              (goto-char (point-max))
              (insert draft)
              (setq composer-tick (buffer-chars-modified-tick)))
            (e-chat--render-event
             (e-events-make :type 'message-added
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:message (:role user
                                                  :content "render this"))))
            (should-not (string-match-p (regexp-quote e-chat--composer-glyph)
                                        (buffer-string)))
            (should (string-match-p "render this" (buffer-string)))
            (with-current-buffer composer
              (should (equal (e-chat--composer-text) draft))
              (should (= (buffer-chars-modified-tick) composer-tick)))
            (e-chat--insert-context-reference
             '(:uri "file:///tmp/context" :label "context" :text "source"))
            (with-current-buffer composer
              (should (string-match-p "@\\[context\\]"
                                      (e-chat--composer-text))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-window-cycle-skips-transcript ()
  "C-x o treats a composed transcript and composer as one chat surface."
  (let* ((e-chat--surface-composition-enabled t)
         (buffer (e-chat-test--buffer nil "chat-composer-window-cycle"))
         transcript-window composer-window external-window external-buffer)
    (unwind-protect
        (progn
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (setq composer-window
                  (e-chat--surface-display-composer transcript-window t))
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
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
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
                  (e-chat--surface-display-composer transcript-window t)))
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
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
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
                  (e-chat--surface-display-composer transcript-window t)))
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
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
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
                  (e-chat--surface-display-composer transcript-window t)))
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
            (should (eq (e-chat--surface-composer-window transcript-window)
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
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
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
                  (e-chat--surface-display-composer transcript-window t)))
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

(ert-deftest e-chat-test-display-repairs-overwritten-composer-constituent ()
  "Redisplaying a saved damaged atom restores its composer slot in place."
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-repair-composer-slot"))
         (intruder (generate-new-buffer "*e-chat composer intruder*"))
         transcript-window composer-window repaired-window composer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq transcript-window (selected-window))
          (set-window-buffer transcript-window buffer)
          (with-current-buffer buffer
            (setq composer-window
                  (e-chat--surface-display-composer transcript-window)))
          (setq composer (window-buffer composer-window))
          ;; Model a corrupted window state produced before surface leaves were
          ;; dedicated.  The atom itself remains structurally intact.
          (set-window-dedicated-p composer-window nil)
          (set-window-buffer composer-window intruder)
          (with-current-buffer buffer
            (setq repaired-window
                  (e-chat--surface-display-composer transcript-window)))
          (should (eq repaired-window composer-window))
          (should (eq (window-buffer composer-window) composer))
          (should (eq (window-dedicated-p transcript-window) 'soft))
          (should (eq (window-dedicated-p composer-window) 'soft))
          (should (= (length (window-list nil 'nomini)) 2)))
      (set-window-configuration configuration)
      (when (buffer-live-p intruder)
        (kill-buffer intruder))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-window-state-restoration-recovers-surface-pair ()
  "Writable window state restores the native atomic chat surface."
  (should (eq (cdr (assq 'window-atom window-persistent-parameters))
              'writable))
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
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
                  (e-chat--surface-display-composer transcript-window)))
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
            (should (eq (e-chat--surface-composer-window transcript-window)
                        composer-window)))
          (should (window-atom-root transcript-window))
          (should (eq (window-atom-root transcript-window)
                      (window-atom-root composer-window))))
      (set-window-configuration configuration)
      (when (buffer-live-p external-buffer)
        (kill-buffer external-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-legacy-ordinary-surface-upgrades-to-atom ()
  "Displaying a persisted ordinary pair upgrades it inside the chat shell."
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-legacy-pair-upgrade"))
         transcript-window legacy-window composer-window composer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq transcript-window (selected-window))
          (set-window-buffer transcript-window buffer)
          (with-current-buffer buffer
            (setq composer (e-chat--surface-ensure-composer)))
          (setq legacy-window
                (split-window transcript-window
                              (- e-chat-composer-window-min-height)
                              'below))
          (set-window-buffer legacy-window composer)
          (should-not (window-atom-root transcript-window))
          (should-not (window-atom-root legacy-window))
          (with-current-buffer buffer
            (setq composer-window
                  (e-chat--surface-display-composer transcript-window)))
          (should-not (window-live-p legacy-window))
          (should (eq (window-buffer composer-window) composer))
          (should (eq (window-atom-root transcript-window)
                      (window-atom-root composer-window))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-navigation-routes-keys-to-transcript ()
  "Composer navigation selects the transcript and activates its keymap."
  (let* ((e-chat--surface-composition-enabled t)
         (buffer (e-chat-test--buffer nil "chat-composer-navigation"))
         transcript-window composer-window details-buffer)
    (unwind-protect
        (save-window-excursion
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn
             "turn-1" 10 11 "question" "answer")
            (setq composer-window
                  (e-chat--surface-display-composer transcript-window t)))
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

(ert-deftest e-chat-test-window-buffer-change-defers-composer-creation ()
  "A generic chat buffer change creates its composer after redisplay work."
  (let* ((e-chat--surface-composition-enabled t)
         (buffer (e-chat-test--buffer nil "chat-deferred-restored-window"))
         (transcript-window (display-buffer buffer)))
    (unwind-protect
        (let ((calls 0))
          (select-window transcript-window)
          (set-frame-parameter nil e-chat--selected-surface-frame-parameter nil)
          (cl-letf (((symbol-function 'e-chat--surface-display-composer)
                     (lambda (&rest _arguments)
                       (setq calls (1+ calls))
                       nil)))
            (e-chat--activate-selected-surface-after-window-buffer-change
             (selected-frame))
            (should (= calls 0))
            (with-current-buffer buffer
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer buffer)))
            (should (= calls 1))))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun e-chat-test--kill-chat-buffers ()
  "Kill all live e chat buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'e-chat-mode)
          (kill-buffer buffer))))))

(defun e-chat-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun e-chat-test--dispatch-observed-event (event)
  "Deliver board-observed EVENT through the current chat subscription."
  (funcall (e-chat-service-subscription-function e-chat--event-subscription)
           event)
  (e-ui-work-with-batch-drain
    (e-ui-work-drain-batch :buffer (current-buffer))))

(defmacro e-chat-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     ,@body))

(defun e-chat-test--activate-chat-session (harness)
  "Activate the chat-session capability in HARNESS."
  (e-harness-activate-capability
   harness
   (e-chat-session-capability-create))
  harness)

(defun e-chat-test--mark-active-turn (turn-id &optional status)
  "Mark TURN-ID as the current active turn in the test chat buffer."
  (let* ((binding (e-chat-service-binding e-chat-harness e-chat-session-id))
         (attachment (and binding
                          (e-chat-service-binding-attachment binding)))
         (participant-id
          (and attachment
               (e-board-registry-participant-id
                (e-board-runtime-attachment-participant attachment)))))
    (when participant-id
      (puthash (list participant-id turn-id)
               turn-id
               (e-chat-service-binding-turn-map binding))))
  (puthash e-chat-session-id
           (list :id turn-id :status (or status 'running))
           (e-harness-active-turns e-chat-harness)))

(defun e-chat-test--register-chat-instance
    (id name harness &optional default)
  "Register chat instance ID named NAME backed by HARNESS."
  (e-harness-registry-register id harness)
  (e-harness-instance-register
   :id id
   :name name
   :kind 'chat
   :harness-id id
   :default default))

(defun e-chat-test--render-turn (turn-id start-time end-time prompt response)
  "Render TURN-ID with START-TIME, END-TIME, PROMPT, and RESPONSE."
  (e-chat--render-event
   (e-events-make :type 'turn-started
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at start-time))
  (e-chat--render-event
   (e-events-make :type 'message-added
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at start-time
                  :payload (list :message
                                 (list :role 'user :content prompt))))
  (e-chat--render-event
   (e-events-make :type 'message-added
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at end-time
                  :payload (list :message
                                 (list :role 'assistant :content response))))
  (e-chat--render-event
   (e-events-make :type 'turn-finished
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at end-time
                  :payload '(:reason stop))))

(defun e-chat-test--focused-turn-bounds ()
  "Return focused turn overlay bounds."
  (list (overlay-start e-chat--focused-turn-overlay)
        (overlay-end e-chat--focused-turn-overlay)))

(defun e-chat-test--focused-turn-text ()
  "Return focused overlay text."
  (buffer-substring-no-properties
   (overlay-start e-chat--focused-turn-overlay)
   (overlay-end e-chat--focused-turn-overlay)))

(defun e-chat-test--focused-block ()
  "Return the currently focused block record."
  (gethash e-chat--focused-block-id e-chat--block-registry))

(defun e-chat-test--focus-block-containing (text)
  "Enter response navigation at rendered block containing TEXT."
  (goto-char (point-min))
  (search-forward text)
  (call-interactively #'e-chat-enter-response-navigation)
  (e-chat-test--focused-block))

(defun e-chat-test--message-display-hidden-p (message-id)
  "Return non-nil when MESSAGE-ID is locally projected as hidden."
  (let* ((block-id (e-chat--message-block-id message-id))
         (block (and block-id
                     (gethash block-id e-chat--block-registry)))
         (start (and block (marker-position (plist-get block :start-marker)))))
    (and start
         (e-chat--block-display-hidden-p block)
         (invisible-p start))))

(defun e-chat-test--kill-buffer-name (name)
  "Kill buffer NAME when it exists."
  (when-let ((buffer (get-buffer name)))
    (kill-buffer buffer)))

(defun e-chat-test--turn-bounds (turn-id)
  "Return registered bounds for TURN-ID."
  (let ((turn (gethash turn-id e-chat--turn-registry)))
    (list (marker-position (plist-get turn :start-marker))
          (marker-position (plist-get turn :end-marker)))))

(defun e-chat-test--count-occurrences (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (let ((count 0)
        (start 0))
    (while (and (not (string-empty-p needle))
                (string-match (regexp-quote needle) text start))
      (setq start (match-end 0))
      (setq count (1+ count)))
    count))

(defun e-chat-test--count-font-lock-face-runs (face &optional start end)
  "Return the number of contiguous `font-lock-face' runs using FACE."
  (let ((count 0)
        (limit (or end (point-max)))
        (pos (or start (point-min)))
        next)
    (while (< pos limit)
      (setq next (or (next-single-property-change pos 'font-lock-face
                                                  nil limit)
                     limit))
      (when (eq (get-text-property pos 'font-lock-face) face)
        (setq count (1+ count)))
      (setq pos next))
    count))

(defun e-chat-test--pending-ui-work (&optional owner key)
  "Return pending UI work for the current buffer, optionally narrowed."
  (e-ui-work-pending (current-buffer)
                     :owner owner
                     :key (or key :any)))

(defun e-chat-test--live-work-handle-p (handle)
  "Return non-nil when HANDLE is a live nonterminal work handle."
  (and (e-work-handle-p handle)
       (not (e-request-terminal-p (e-work-handle-lifecycle handle)))))

(defun e-chat-test--run-pending-ui-work (owner &optional key)
  "Run one pending UI work job for OWNER and optional KEY in current buffer."
  (let ((job (cl-find-if
              (lambda (job)
                (let ((spec (e-ui-work-job-spec job)))
                  (and (eq (e-ui-work-spec-owner spec) owner)
                       (or (null key)
                           (equal (e-ui-work-spec-key spec) key)))))
              e-ui-work--pending-jobs)))
    (should job)
    (e-ui-work--run-job-now job)))

(defun e-chat-test--session-subscriber-count (harness session-id)
  "Return HARNESS subscriber count for SESSION-ID."
  (length
   (cl-remove-if-not
    (lambda (subscriber)
      (equal (plist-get subscriber :session-id) session-id))
    (e-harness-subscribers harness))))

(defun e-chat-test--seed-board-log-from-private-fixture (harness session-id)
  "Translate an old private test fixture into its explicit durable board log.
Production presentation never performs this compatibility translation."
  (let* ((store (e-harness-sessions harness))
         (_ (unless (plist-get (e-session-get store session-id)
                               :board-session-state)
              (e-session-declare-board-state
               store session-id (format "chat:%s" session-id)
               (format "test-board:%s" session-id))))
         (binding (e-chat-service-ensure-binding harness session-id))
         (board (and binding
                     (e-board-registry-board-source-board
                      (e-chat-service-binding-board binding))))
         (participant-id
          (if binding
              (e-board-registry-participant-id
               (e-board-runtime-attachment-participant
                (e-chat-service-binding-attachment binding)))
            (format "fixture:%s" session-id)))
         (sequence 0)
         (turn-inputs (make-hash-table :test 'equal))
         envelopes)
    (dolist (message (e-session-messages store session-id))
      (unless (memq (plist-get message :role) '(tool-call tool))
        (let* ((user-p (eq (plist-get message :role) 'user))
               (turn-id (plist-get message :turn-id))
               (stored-id (plist-get message :id))
               (message-id
                (if (and user-p turn-id (stringp stored-id)
                         (string-match-p
                          "\\`[0-9A-HJKMNP-TV-Z]\\{26\\}\\'" stored-id))
                    turn-id
                  (or stored-id turn-id)))
               (reply-id (and (not user-p) turn-id
                              (gethash turn-id turn-inputs))))
          (when (and user-p turn-id)
            (puthash turn-id message-id turn-inputs))
          (push (list :id message-id
                    :kind (if user-p 'input 'output)
                    :author (if (eq (plist-get message :role) 'user)
                                "fixture-client" "fixture-participant")
                    :tags '(main) :content (plist-get message :content)
                    :reference (plist-get message :references)
                    :attributes
                    (let ((attributes
                           (copy-tree (plist-get message :metadata))))
                      (if-let ((display (plist-get message :display)))
                          (plist-put attributes :display display)
                        attributes))
                    :source-input-key
                    (and user-p
                         (list 'fixture session-id (cl-incf sequence)))
                    :source-output-key
                    (and (not user-p)
                         (list 'fixture session-id (cl-incf sequence)))
                    :source-turn-id turn-id
                    :created-at (plist-get message :created-at)
                    :reply-to-message-ids (and reply-id (list reply-id))
                    :routing-state 'historical)
                envelopes))))
    (dolist (event (e-session-activity-events store session-id))
      (push (list :id (or (plist-get event :id)
                          (format "fixture-activity-%d" (1+ sequence)))
                  :kind 'activity
                  :author (format "participant:%s" participant-id)
                  :tags '(main) :attributes (copy-tree (plist-get event :payload))
                  :subject-participant-id participant-id
                  :source-turn-id (plist-get event :turn-id)
                  :reply-to-message-ids
                  (when-let ((input-id (gethash (plist-get event :turn-id)
                                                turn-inputs)))
                    (list input-id))
                  :activity-kind (plist-get event :event-type)
                  :created-at (plist-get event :created-at)
                  :source-activity-key
                  (list participant-id 1 (cl-incf sequence)))
            envelopes))
    (setq envelopes (nreverse envelopes))
    (if board
        (dolist (envelope envelopes)
          (unless (e-board-message board (plist-get envelope :id))
            (e-board-import-message board envelope)))
      (unless (plist-get (e-session-get store session-id) :board-session-state)
        (e-session-declare-board-state
         store session-id (format "chat:%s" session-id)
         (format "test-board:%s" session-id)))
      (dolist (envelope envelopes)
        (e-session-append-board-message store session-id envelope)))
    (when binding
      (while (< (e-board-observer-next-index
                 (e-chat-service-binding-observer binding))
                (e-board-message-count board))
        (e-chat-service--drain-observer binding)))
    envelopes))

(ert-deftest e-chat-test-open-creates-protected-transcript-and-composer ()
  "Opening chat creates protected transcript text and editable composer text."
  (let ((buffer (e-chat-test--buffer nil "chat-open")))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'e-chat-mode))
          (goto-char (point-min))
          (should (looking-at-p (regexp-quote e-chat--title)))
          (should (eq (get-text-property (point-min) 'font-lock-face)
                      'e-chat-title-face))
          (should-not (string-match-p "^e chat$" (buffer-string)))
          (should (markerp e-chat--composer-start-marker))
          (should (marker-position e-chat--composer-start-marker))
          (should (get-text-property (point-min) 'read-only))
          (goto-char (point-min))
          (should-error (insert "mutate") :type 'text-read-only)
          (goto-char (point-max))
          (insert "editable")
          (should (equal (e-chat--composer-text) "editable"))
          (should (string-match-p (regexp-quote e-chat--composer-glyph)
                                  (buffer-string)))
          (should (get-text-property
                   (1- (marker-position e-chat--composer-start-marker))
                   'e-chat-composer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-has-visible-separator-line ()
  "The composer has a protected, visible separator above its prompt."
  (let ((buffer (e-chat-test--buffer nil "chat-separator")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char e-chat--composer-start-marker)
          (forward-line -1)
          (let ((line-start (line-beginning-position))
                (line-end (line-end-position)))
            (should (string-match-p "─"
                                    (buffer-substring-no-properties
                                     line-start line-end)))
            (should (eq (get-text-property line-start 'font-lock-face)
                        'e-chat-separator-face))
            (should (get-text-property line-start 'read-only))
            ;; The separator inherits a neutral theme face rather than a fixed
            ;; palette, so it tracks the active theme.
            (should (equal (face-attribute 'e-chat-separator-face :inherit)
                           'shadow))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-turns-and-responses-have-stable-separators ()
  "Rendered turns use explicit separator text outside navigable blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-turn-separators")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "two")
          (should (boundp 'e-chat--turn-separator))
          (should (boundp 'e-chat--response-separator))
          (let ((content (buffer-string)))
            (should (= (e-chat-test--count-occurrences
                        e-chat--turn-separator content)
                       1))
            (should (= (e-chat-test--count-font-lock-face-runs
                        'e-chat-separator-face
                        (point-min)
                        (marker-position e-chat--composer-spacer-marker))
                       2))
            (should (equal e-chat--response-separator
                           e-chat--composer-separator))
            (should (string-match-p
                     (concat "one\\(.\\|\n\\)*"
                             (regexp-quote e-chat--turn-separator)
                             "\\(.\\|\n\\)*"
                             (regexp-quote e-chat--user-glyph)
                             " second")
                     content)))
          (goto-char (point-min))
          (search-forward e-chat--turn-separator)
          (should (eq (get-text-property (line-beginning-position)
                                         'font-lock-face)
                      'e-chat-turn-separator-face))
          (should (get-text-property (line-beginning-position) 'read-only))
          (should-not (get-text-property (line-beginning-position)
                                         'e-chat-block-id))
          (goto-char (point-min))
          (search-forward e-chat--response-separator)
          (should (eq (get-text-property (line-beginning-position)
                                         'font-lock-face)
                      'e-chat-separator-face))
          (should (get-text-property (line-beginning-position) 'read-only))
          (should-not (get-text-property (line-beginning-position)
                                         'e-chat-block-id)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-activity-keeps-single-response-separator ()
  "A running turn keeps one user/assistant separator after final output."
  (let ((buffer (e-chat-test--buffer nil "chat-running-response-separator")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:message (:role user
                                                :content "inspect"))))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "read")))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "done"))))
          (should (boundp 'e-chat--response-separator))
          (should (= (e-chat-test--count-font-lock-face-runs
                      'e-chat-separator-face
                      (point-min)
                      (or (and (markerp e-chat--composer-spacer-marker)
                               (marker-position e-chat--composer-spacer-marker))
                          (point-max)))
                     1))
          (should-not (string-match-p "2 tool calls" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-navigation-excludes-separators-and-does-not-reflow ()
  "Block navigation changes focus without adding/removing separator text."
  (let ((buffer (e-chat-test--buffer nil "chat-navigation-separator-stability")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "two")
          (should (boundp 'e-chat--turn-separator))
          (should (boundp 'e-chat--response-separator))
          (let ((content-before (buffer-string))
                (lines-before (count-lines (point-min) (point-max))))
            (goto-char (point-min))
            (search-forward "two")
            (call-interactively #'e-chat-enter-response-navigation)
            (should-not (string-match-p
                         (regexp-quote e-chat--turn-separator)
                         (e-chat-test--focused-turn-text)))
            (should-not (string-match-p
                         (regexp-quote e-chat--response-separator)
                         (e-chat-test--focused-turn-text)))
            (call-interactively
             (lookup-key e-chat-response-navigation-mode-map (kbd "k")))
            (should-not (string-match-p
                         (regexp-quote e-chat--turn-separator)
                         (e-chat-test--focused-turn-text)))
            (should-not (string-match-p
                         (regexp-quote e-chat--response-separator)
                         (e-chat-test--focused-turn-text)))
            (should (equal (buffer-string) content-before))
            (should (= (count-lines (point-min) (point-max))
                       lines-before))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-separator-faces-refresh-owned-defaults ()
  "Live reload reapplies package-owned chat separator face defaults."
  (should (facep 'e-chat-turn-separator-face))
  (should (facep 'e-chat-response-separator-face))
  (unwind-protect
      (progn
        (set-face-attribute 'e-chat-turn-separator-face nil
                            :foreground 'unspecified
                            :background 'unspecified
                            :box '(:line-width 1 :color "#ffffff"))
        (set-face-attribute 'e-chat-response-separator-face nil
                            :foreground 'unspecified
                            :background 'unspecified
                            :box '(:line-width 1 :color "#ffffff"))
        (e-chat--refresh-face-specs)
        (should (equal (face-attribute 'e-chat-turn-separator-face :inherit)
                       'shadow))
        (should (eq (face-attribute 'e-chat-turn-separator-face :foreground)
                    'unspecified))
        (should (eq (face-attribute 'e-chat-turn-separator-face :background)
                    'unspecified))
        (should-not (face-attribute 'e-chat-turn-separator-face :box))
        (should (equal (face-attribute 'e-chat-response-separator-face :inherit)
                       'shadow))
        (should-not (face-attribute 'e-chat-response-separator-face :box)))
    (e-chat--refresh-face-specs)))

(ert-deftest e-chat-test-submit-multiline-composer-and-render-response ()
  "The composer submits multiline text and chat renders message blocks."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "hello back")
                   (:type done :reason stop))
                 "chat-submit")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "first line\nsecond line")
          (e-chat-submit)
          (should (e-chat-test--wait-until
                   (lambda () (string-match-p "hello back" (buffer-string)))
                   1.0))
          (let ((content (buffer-string)))
            (should (string-match-p (concat (regexp-quote e-chat--user-glyph)
                                            " first line\nsecond line")
                                    content))
            (should (string-match-p
                     (concat (regexp-quote e-chat--assistant-glyph)
                             " hello back")
                     content))
            (should-not (string-match-p "Turn started" content))
            (should-not (string-match-p "Turn finished" content))
            (should-not (string-match-p "Backend returned no assistant output"
                                        content)))
          (save-excursion
            (goto-char (point-min))
            (search-forward (concat e-chat--user-glyph " first line"))
            (should (eq (get-text-property (point) 'font-lock-face)
                        'e-chat-user-face))
            (search-forward "hello back")
            (should-not (eq (get-text-property (point) 'font-lock-face)
                            'e-chat-assistant-face)))
          (should (equal (e-chat--composer-text) "")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (ignore-errors
            (e-harness-test-abort e-chat-harness e-chat-session-id)))
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

(ert-deftest e-chat-test-render-session-skips-hidden-messages ()
  "A message flagged `:display' `hidden' is invisible in the clean transcript.
A superseded first attempt stays in the store for audit but the shell shows
only the visible reply while retaining a durable-to-rendered projection."
  (let ((buffer (e-chat-test--buffer nil "chat-hidden-render")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-render"
           (list :id "m-visible" :role 'assistant :turn-id "turn-1"
                 :content "visible answer"))
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-render"
           (list :id "m-hidden" :role 'assistant :turn-id "turn-1"
                 :content "superseded first attempt" :display 'hidden))
          (e-chat-test--seed-board-log-from-private-fixture
           e-chat-harness e-chat-session-id)
          (e-chat--clear)
          (e-chat--render-session)
          (let* ((visible-id (e-chat--message-block-id "m-visible"))
                 (hidden-id (e-chat--message-block-id "m-hidden")))
            (should visible-id)
            (should hidden-id)
            (should (e-chat--live-block-record visible-id))
            (should-not (e-chat--live-block-record hidden-id))
            (should (e-chat-test--message-display-hidden-p "m-hidden"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-message-updated-reconciles-rendered-reply-locally ()
  "Flipping a rendered message hidden/visible updates its block without replay.
The bayesian follow-up hides the first attempt after it was already shown, so
the shell must react to the `message-updated' event without rebuilding a long
transcript."
  (let ((buffer (e-chat-test--buffer nil "chat-hidden-update")))
    (unwind-protect
        (with-current-buffer buffer
          (setq e-chat--assume-redraw-visible t)
          (let ((message (list :id "message-1" :role 'assistant
                               :content "first attempt reply")))
            (e-chat--render-event
             (e-events-make :type 'message-added
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload (list :message message)))
            (should (string-match-p "first attempt reply" (buffer-string)))
            (let ((rerenders 0)
                  (message-id (plist-get message :id)))
              (cl-letf (((symbol-function 'e-chat--rerender-transcript)
                         (lambda () (setq rerenders (1+ rerenders)))) )
                (e-chat--render-event
                 (e-events-make
                  :type 'message-updated :session-id e-chat-session-id
                  :turn-id "turn-1"
                  :payload (list :message
                                 (plist-put (copy-sequence message)
                                            :display 'hidden))))
                (let ((block-id (e-chat--message-block-id message-id)))
                  (should block-id)
                  (should-not (e-chat--live-block-record block-id))
                  (should (e-chat-test--message-display-hidden-p message-id)))
                (e-chat--render-event
                 (e-events-make
                  :type 'message-updated :session-id e-chat-session-id
                  :turn-id "turn-1"
                  :payload (list :message message)))
                (let ((block-id (e-chat--message-block-id message-id)))
                  (should (e-chat--live-block-record block-id))
                  (should-not (e-chat-test--message-display-hidden-p message-id)))
                (should (= rerenders 0))))))
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
          (e-chat--clear)
          (e-chat--render-session)
          (should (e-chat-test--message-display-hidden-p "m-first"))
          (should (e-chat-test--message-display-hidden-p "m-prompt"))
          (e-chat-test--focus-block-containing "revised answer")
          (should-not e-chat--reveal-hidden)
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should e-chat--reveal-hidden)
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
          (e-chat--clear)
          (e-chat--render-session)
          (e-chat-test--focus-block-containing "revised answer")
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should (string-match-p "superseded first attempt" (buffer-string)))
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should-not e-chat--reveal-hidden)
          (should (e-chat-test--message-display-hidden-p "m-first")))
      (when (buffer-live-p buffer)
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
          (e-chat--clear)
          (e-chat--render-session)
          (e-chat-test--focus-block-containing "revised answer")
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should (string-match-p "superseded first attempt" (buffer-string)))
          (call-interactively #'e-chat-response-navigation-insert)
          (should-not e-chat--reveal-hidden)
          (should (e-chat-test--message-display-hidden-p "m-first")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-submit-immediately-clears-composer-and-keeps-separator ()
  "Submitting clears the composer while board observation shows the user turn."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "later")
                   (:type done :reason stop))
                 "chat-submit-immediate")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "send now")
          (e-chat-submit)
          (should (equal (e-chat--composer-text) ""))
          (should (e-chat-test--wait-until
                   (lambda () (string-match-p "send now" (buffer-string)))
                   1.0))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote e-chat--user-glyph)
                             " send now")
                     content))
            (should-not (string-match-p
                         (concat (regexp-quote e-chat--composer-glyph)
                                 "send now")
                         content)))
          (should (string-match-p (regexp-quote e-chat--composer-separator)
                                  (buffer-string)))
          (should (e-chat--composer-active-p))
          (should (equal (e-chat--composer-text) "")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (ignore-errors
            (e-harness-test-abort e-chat-harness e-chat-session-id)))
        (kill-buffer buffer)))))

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
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "send now")
          (cl-letf (((symbol-function 'e-harness-context)
                     (lambda (&rest args)
                       (setq context-calls (1+ context-calls))
                       (apply original-context args))))
            (e-chat-submit)
            (should (= context-calls 0))
            (should-not backend-started)
            (should (string-match-p (regexp-quote e-chat--composer-separator)
                                    (buffer-string)))
            (should (e-chat--composer-active-p))
            (should (equal (e-chat--composer-text) ""))
            (should (e-chat-test--wait-until
                     (lambda () backend-started)
                     1.0))
            (should (> context-calls 0))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (string-match-p
                        (concat (regexp-quote e-chat--user-glyph)
                                " send now")
                        (buffer-string)))
                     1.0))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (ignore-errors
            (e-harness-test-abort e-chat-harness e-chat-session-id)))
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
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "send async")
          (e-chat-submit)
          (should (e-chat-test--wait-until (lambda () finish) 2.0))
          (should (e-chat-test--wait-until
                   (lambda ()
                     (string-match-p
                      (concat (regexp-quote e-chat--user-glyph)
                              " send async")
                      (buffer-string)))
                   2.0))
          (should-not (string-match-p "late answer" (buffer-string)))
          (should (string-match-p "queued" (format "%s" header-line-format)))
          (funcall finish)
          (should (e-chat-test--wait-until
                   (lambda () (string-match-p "late answer" (buffer-string)))
                   2.0))
          (should (e-chat-test--wait-until
                   (lambda ()
                     (string-match-p "done"
                                     (format "%s" header-line-format)))
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
        (with-current-buffer buffer
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
                 (e-chat--insert-context-reference
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
          (should (equal (e-chat--composer-text) "")))
      (when (buffer-live-p buffer)
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
        (with-current-buffer buffer
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
                 (e-chat--insert-context-reference
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
          (should (equal (e-chat--composer-text) "")))
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
        (with-current-buffer buffer
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
            (should (equal (e-chat--composer-text) ""))
            (should (string-match-p "steered" (format "%s" header-line-format)))
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
        (with-current-buffer buffer
          (ignore-errors
            (e-harness-test-abort e-chat-harness e-chat-session-id)))
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-queued-prompts-render-above-composer ()
  "Queued prompts appear in bottom chrome above the composer separator."
  (let ((buffer (e-chat-test--buffer nil "chat-queue-render")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat-service-queued-inputs)
                     (lambda (&rest _)
                       '((:prompt "second line\ncontinued")
                         (:prompt "third")))))
            (e-chat--refresh-composer-position)
            (let* ((content (buffer-string))
                   (queue-pos (and (markerp e-chat--queue-start-marker)
                                   (marker-position
                                    e-chat--queue-start-marker)))
                   (composer-pos (and (markerp e-chat--composer-start-marker)
                                      (marker-position
                                       e-chat--composer-start-marker))))
              (should queue-pos)
              (should composer-pos)
              (should (< queue-pos composer-pos))
              (should (string-match-p "Queued prompts" content))
              (should (string-match-p "1\\. second line continued" content))
              (should (string-match-p "2\\. third" content)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-queued-prompts-survive-composer-refresh-and-final-insertion ()
  "Queue chrome survives spacer refresh and final assistant insertion."
  (let ((buffer (e-chat-test--buffer nil "chat-queue-refresh")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat-service-queued-inputs)
                     (lambda (&rest _) '((:prompt "second")))))
            (goto-char (point-max))
            (insert "draft")
            (e-chat--refresh-composer-position)
            (should (string-match-p "Queued prompts" (buffer-string)))
            (should (string-match-p "1\\. second" (buffer-string)))
            (should (equal (e-chat--composer-text) "draft"))
            (e-chat--insert-entry "Assistant" "final answer" t "turn-final")
            (should (string-match-p "final answer" (buffer-string)))
            (should (string-match-p "Queued prompts" (buffer-string)))
            (should (string-match-p "1\\. second" (buffer-string)))
            (should (equal (e-chat--composer-text) "draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-queued-prompts-survive-failure-and-cancel-rendering ()
  "Queue chrome survives terminal error and cancellation entries."
  (let ((buffer (e-chat-test--buffer nil "chat-queue-terminal")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat-service-queued-inputs)
                     (lambda (&rest _) '((:prompt "second")))))
            (goto-char (point-max))
            (insert "draft")
            (e-chat--render-turn-failure
             "turn-failed"
             (current-time)
             '(:error "provider failed")
             t)
            (should (string-match-p "Turn failed: provider failed"
                                    (buffer-string)))
            (should (string-match-p "Queued prompts" (buffer-string)))
            (should (string-match-p "1\\. second" (buffer-string)))
            (should (equal (e-chat--composer-text) "draft"))
            (e-chat--insert-entry "System" "Turn cancelled" t "turn-cancelled")
            (should (string-match-p "Turn cancelled" (buffer-string)))
            (should (string-match-p "Queued prompts" (buffer-string)))
            (should (string-match-p "1\\. second" (buffer-string)))
            (should (equal (e-chat--composer-text) "draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-empty-queue-removes-list-without-deleting-composer ()
  "Clearing the queue removes queue chrome and preserves composer text."
  (let ((buffer (e-chat-test--buffer nil "chat-queue-empty"))
        (queued '((:prompt "second"))))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat-service-queued-inputs)
                     (lambda (&rest _) queued)))
            (e-chat--refresh-composer-position)
            (goto-char (point-max))
            (insert "draft")
            (should (string-match-p "Queued prompts" (buffer-string)))
            (setq queued nil)
            (e-chat--refresh-composer-position)
            (should-not (string-match-p "Queued prompts" (buffer-string)))
            (should (equal (e-chat--composer-text) "draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-return-inserts-newline-in-composer ()
  "RET inserts a newline instead of submitting the prompt."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "unexpected")
                   (:type done :reason stop))
                 "chat-ret")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "one")
          (call-interactively (lookup-key e-chat-mode-map (kbd "RET")))
          (insert "two")
          (should (equal (e-chat--composer-text) "one\ntwo"))
          (should (equal (e-harness-messages e-chat-harness e-chat-session-id)
                         nil)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-control-p-stays-inside-composer ()
  "C-p from the first composer line does not move point into transcript text."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-c-p")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char e-chat--composer-start-marker)
          (let ((composer-start (marker-position e-chat--composer-start-marker)))
            (call-interactively (key-binding (kbd "C-p")))
            (should (>= (point) composer-start))
            (should-not (get-text-property (point) 'e-chat-protected))))
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

(ert-deftest e-chat-test-mode-keeps-scroll-margin-user-controlled ()
  "Chat buffers do not force a global bottom scroll margin."
  (let ((scroll-margin 0)
        (buffer (e-chat-test--buffer nil "chat-composer-scroll-margin")))
    (unwind-protect
        (with-current-buffer buffer
          (should (= scroll-margin 0)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-edit-scrolls-bottom-into-view ()
  "Editing composer text scrolls the active input down after the command."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-edit-scroll"))
        (window nil)
        recenter-argument)
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (cl-letf (((symbol-function 'recenter)
                     (lambda (argument &rest _ignored)
                       (setq recenter-argument argument))))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "typed")
              (run-hooks 'post-command-hook)
              (should (equal recenter-argument -2)))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-self-insert-from-scrollback-targets-composer ()
  "Typing from readback moves to the composer before inserting text."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-self-insert")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let ((this-command 'self-insert-command)
                (last-command-event ?x))
            (run-hooks 'pre-command-hook)
            (call-interactively #'self-insert-command))
          (should (e-chat--point-in-composer-p))
          (should (equal (e-chat--composer-text) "x")))
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

(ert-deftest e-chat-test-show-composer-leaves-bottom-margin ()
  "Composer focus leaves a visible line between point and the mode line."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-recenter"))
        (window nil)
        recenter-argument)
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (cl-letf (((symbol-function 'recenter)
                     (lambda (argument &rest _ignored)
                       (setq recenter-argument argument))))
            (with-current-buffer buffer
              (e-chat--show-composer)))
          (should (equal recenter-argument -2)))
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
                (should-not evil-state)
                (should (e-chat--point-in-composer-p))))))
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
          (e-chat--after-display-buffer buffer)
          (should-not e-chat-tool-list-mode)
          (should-not e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (should (e-chat--point-in-composer-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-switch-to-buffer-restores-composed-surface ()
  "Generic buffer switching restores a composed chat's editable surface."
  (let* ((e-chat--surface-composition-enabled t)
         (origin (get-buffer-create " *e-chat switch origin*"))
         (buffer (e-chat-test--buffer nil "chat-switch-composed-surface"))
         (window (selected-window))
         composer)
    (unwind-protect
        (progn
          (delete-other-windows window)
          (switch-to-buffer origin)
          (switch-to-buffer buffer)
          (set-frame-parameter nil e-chat--selected-surface-frame-parameter nil)
          (e-chat--activate-selected-surface-after-window-buffer-change
           (selected-frame))
          (with-current-buffer buffer
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer buffer)))
          (setq composer
                (buffer-local-value 'e-chat--surface-composer-buffer buffer))
          (should (buffer-live-p composer))
          (should (eq (window-buffer (selected-window)) composer))
          (should (eq (get-buffer-window buffer t) window))
          (should (eq (window-atom-root window)
                      (window-atom-root (selected-window)))))
      (when (buffer-live-p origin)
        (kill-buffer origin))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-reload-preserves-composed-surface-pair ()
  "Reload preserves the atomic pair and its composer draft."
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-reload-composed-surface"))
         transcript-window
         composer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (setq transcript-window (get-buffer-window buffer))
          (with-current-buffer buffer
            (setq composer
                  (window-buffer
                   (e-chat--surface-display-composer transcript-window))))
          (with-current-buffer buffer
            (with-current-buffer composer
              (goto-char (point-max))
              (insert "draft survives reload"))
            (should (= (length
                        (get-buffer-window-list composer nil t))
                       1))
            ;; Reinitializing `e-chat-mode' leaves the atomic window pair and
            ;; composer buffer alive; no window cache needs reconstruction.
            (e-chat--attach-buffer
             buffer e-chat-harness e-chat-session-id
             e-chat-harness-instance-id)
            (e-chat--after-display-buffer buffer)
            (should (eq e-chat--surface-composer-buffer composer))
            (should (eq (e-chat--surface-composer-window transcript-window)
                        (get-buffer-window composer t)))
            (should (eq (buffer-local-value
                         'e-chat--surface-transcript-buffer composer)
                        buffer))
            (setq-local e-chat--mode-line-status
                        "e-chat gpt-5.6-sol/high 18% (64k/353k tok)")
            ;; Host mode-line packages may temporarily replace the transcript's
            ;; display form during reload.  The composer projects semantic
            ;; status and must never forward an evaluable form recursively.
            (setq-local mode-name '(:eval (format-mode-line mode-name)))
            (with-current-buffer composer
              (should (equal (e-chat--surface-composer-mode-name)
                             "e-chat gpt-5.6-sol/high 18%% (64k/353k tok)")))
            (should (= (length
                        (get-buffer-window-list composer nil t))
                       1))
            (with-current-buffer composer
              (should (equal (e-chat--composer-text)
                             "draft survives reload")))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-reload-refreshes-every-visible-surface-instance ()
  "Reload protects and repairs every visible atom for one transcript buffer."
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
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
            (e-chat--surface-display-composer first-transcript-window)
            (e-chat--surface-display-composer second-transcript-window)
            (setq composer e-chat--surface-composer-buffer))
          (setq windows
                (append (get-buffer-window-list buffer nil t)
                        (get-buffer-window-list composer nil t)))
          (should (= (length windows) 4))
          (dolist (window windows)
            (set-window-dedicated-p window nil))
          ;; Model an old persisted atom whose composer slot was overwritten.
          (let ((damaged-composer-window
                 (e-chat--surface-composer-slot-window
                  first-transcript-window)))
            (set-window-buffer damaged-composer-window
                               (get-buffer-create "*e-chat reload intruder*")))
          (with-current-buffer buffer
            (e-chat--attach-buffer
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
                       (e-chat--surface-composer-window
                        transcript-window))))))
      (set-window-configuration configuration)
      (when-let ((intruder (get-buffer "*e-chat reload intruder*")))
        (kill-buffer intruder))
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
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 10))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 11))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (goto-char (point-min))
            (set-window-point window (point))
            (set-window-start window (point))
            (e-chat--after-display-buffer buffer)
            (let ((latest-output-end (cdr (e-chat--running-status-bounds)))
                  (composer-position
                   (marker-position e-chat--composer-start-marker)))
              (should latest-output-end)
              (should composer-position)
              (should (= (point) composer-position))
              (should (= (window-point window) composer-position)))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-after-display-settled-composed-chat-shows-latest-output ()
  "Displaying a settled composed chat shows its newest transcript output."
  (let* ((e-chat--surface-composition-enabled t)
         (history (mapconcat (lambda (number)
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
                  (e-chat--surface-display-composer transcript-window t))
            ;; Selecting the composer changes `current-buffer'; continue the
            ;; viewport assertions in the owning transcript.
            (set-buffer buffer)
            (set-window-point transcript-window (point-min))
            (set-window-start transcript-window (point-min))
            (e-chat--set-window-output-follow-state transcript-window nil)
            (should (< (window-point transcript-window) (point-max)))
            (e-chat--after-display-buffer buffer)
            (should (eq (selected-window) composer-window))
            (should (= (window-point transcript-window) (point-max)))
            (should (plist-get
                     (e-chat--window-output-follow-state transcript-window)
                     :follow))))
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-loaded-session-reprojection-restores-following-tail ()
  "Async session replay keeps an activated transcript at its new output tail."
  (let* ((e-chat--surface-composition-enabled t)
         (history (mapconcat (lambda (number)
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
              (e-chat--clear t)
              (e-chat--render-session-loading
               '(:summary "loaded question")))
            (setq composer-window
                  (e-chat--surface-display-composer transcript-window t))
            (set-buffer buffer)
            (e-chat--after-display-buffer buffer)
            (when (e-work-handle-p e-chat--surface-activation-handle)
              ;; The real asynchronous load completes after this activation
              ;; callback has already settled.
              (e-ui-work-cancel e-chat--surface-activation-handle)
              (setq e-chat--surface-activation-handle nil))
            (setq loading-tail (point-max))
            (should (= (window-point transcript-window) loading-tail))
            (should (plist-get
                     (e-chat--window-output-follow-state transcript-window)
                     :follow))
            ;; Load completion clears that projection and inserts the actual
            ;; transcript without crossing another display/focus boundary.
            (e-chat--attach-buffer
             buffer e-chat-harness e-chat-session-id
             e-chat-harness-instance-id)
            (should (> (point-max) loading-tail))
            (should (= (window-point transcript-window) (point-max)))
            (should (>= (window-end transcript-window t) (point-max)))
            (should (eq (selected-window) composer-window))))
      (set-frame-parameter nil e-chat--selected-surface-frame-parameter nil)
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-loaded-session-reprojection-does-not-tail-scrollback ()
  "Session replay does not tail a transcript physically showing scrollback."
  (let* ((e-chat--surface-composition-enabled t)
         (history (mapconcat (lambda (number)
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
            (e-chat--attach-buffer
             buffer e-chat-harness e-chat-session-id
             e-chat-harness-instance-id)
            (setq composer-window
                  (e-chat--surface-display-composer transcript-window t))
            (set-buffer buffer)
            (e-chat--show-latest-output transcript-window)
            (should (plist-get
                     (e-chat--window-output-follow-state transcript-window)
                     :follow))
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
            (e-chat--attach-buffer
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

(ert-deftest e-chat-test-composer-focus-enters-evil-insert-state ()
  "Showing the separate composer requests Evil's insert state."
  (let (entered)
    (cl-letf (((symbol-function 'evil-insert-state)
               (lambda () (setq entered t))))
      (with-temp-buffer
        (e-chat--composer-enter-insert-state)))
    (should entered)))

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

(ert-deftest e-chat-test-captures-point-reference-with-two-line-context ()
  "Point capture uses the current line with two surrounding lines."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\nfive\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((reference (e-chat--capture-context-reference)))
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
          (let ((reference (e-chat--capture-context-reference)))
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

(ert-deftest e-chat-test-submits-composer-text-with-inline-references ()
  "Submitting converts inline reference atoms into ordered prompt context."
  (let ((buffer (e-chat-test--buffer nil "chat-reference-submit")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "Look at ")
          (e-chat--insert-context-reference
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
  (let* ((e-chat--surface-composition-enabled t)
         (buffer (e-chat-test--buffer nil "chat-reference-delete"))
         composer)
    (unwind-protect
        (progn
          (setq composer
                (buffer-local-value 'e-chat--surface-composer-buffer buffer))
          (with-current-buffer composer
            (goto-char (point-max))
            (insert "before ")
            (e-chat--insert-context-reference
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
            (should (equal (e-chat--composer-text) "before @[source:2]after"))
            (search-backward "@[source:2]")
            (call-interactively (key-binding (kbd "DEL")))
            (should (equal (e-chat--composer-text) "before@[source:2]after"))
            ;; Forward delete at the atom removes the whole atom.
            (call-interactively (key-binding (kbd "C-d")))
            (should (equal (e-chat--composer-text) "beforeafter"))
            (let ((inhibit-read-only t))
              (delete-region e-chat--composer-start-marker (point-max)))
            (insert "again ")
            (e-chat--insert-context-reference
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
            (should (equal (e-chat--composer-text) "again  tail"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
          (with-current-buffer buffer
            (insert "hello")
            (let ((last-command-event ?!))
              (e-chat-composer-bang))
            (insert "user")
            (let ((last-command-event ?@))
              (e-chat-composer-at))
            (insert "path")
            (let ((last-command-event ?/))
              (e-chat-composer-slash))
            (should (equal (e-chat--composer-text)
                           "hello!user@path/"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-slash-only-triggers-at-leading-position ()
  "Composer / expands prompts only as the first non-whitespace input."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-slash-leading"))
          (with-current-buffer buffer
            (let ((prompt (e-prompt-spec-create
                           :name "review"
                           :description "Review code."
                           :parameters nil
                           :template "Review now.")))
              (cl-letf (((symbol-function 'e-chat--prompt-candidates)
                         (lambda ()
                           (list (list :label "review" :prompt prompt)))))
                (let ((unread-command-events (list ?\r)))
                  (e-chat-composer-slash))
                (should (equal (e-chat--composer-text) "Review now.")))
              (delete-region e-chat--composer-start-marker (point-max))
              (insert "please ")
              (cl-letf (((symbol-function 'e-chat--prompt-candidates)
                         (lambda ()
                           (error "non-leading / must not collect prompts"))))
                (let ((last-command-event ?/))
                  (e-chat-composer-slash)))
              (should (equal (e-chat--composer-text) "please /")))))
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

(ert-deftest e-chat-test-composer-bang-inserts-command-output-reference ()
  "Leading ! inserts pending output, then completes the context reference."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-bang"))
          (with-current-buffer buffer
            (let (done)
              (cl-letf (((symbol-function 'read-shell-command)
                         (lambda (&rest _args) "printf hi"))
                        ((symbol-function 'e-chat--run-shell-command-start)
                         (lambda (command directory &rest args)
                           (should (equal command "printf hi"))
                           (should (file-directory-p directory))
                           (setq done (plist-get args :on-done))
                           (e-tools-request-create :cancel (lambda () t)))))
                (e-chat-composer-bang))
              (let* ((document (e-chat--composer-document))
                     (references (plist-get document :references)))
                (should (= (length references) 1))
                (should (e-chat--pending-context-reference-p
                         (car references)))
                (should (string-match-p
                         (regexp-quote
                          "<reference id=\"ref-1\" label=\"$ printf hi (running)\">")
                         (plist-get document :text))))
              (funcall done (list :output "hi\n" :exit 0)))
            (let* ((document (e-chat--composer-document))
                   (references (plist-get document :references))
                   (submission (plist-get (e-chat--composer-submission)
                                          :prompt)))
              (should (= (length references) 1))
              (should-not (e-chat--pending-context-reference-p
                           (car references)))
              (should (string-match-p
                       (regexp-quote "<reference id=\"ref-1\" label=\"$ printf hi (exit 0)\">")
                       (plist-get document :text)))
              (should (string-match-p (regexp-quote "$ printf hi") submission))
              (should (string-match-p (regexp-quote "Status: exit 0") submission))
              (should (string-match-p (regexp-quote "hi") submission)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-submit-rejects-pending-command-reference ()
  "Submitting with a pending ! reference preserves the composer and errors."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-pending-submit"))
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'read-shell-command)
                       (lambda (&rest _args) "printf hi"))
                      ((symbol-function 'e-chat--run-shell-command-start)
                       (lambda (&rest _args)
                         (e-tools-request-create :cancel (lambda () t)))))
              (e-chat-composer-bang))
            (insert " use it")
            (should-error (e-chat-submit) :type 'user-error)
            (should (string-match-p
                     (regexp-quote "@[$ printf hi (running)] use it")
                     (e-chat--composer-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-pending-command-references-cancel-on-clear-and-kill ()
  "Pending ! command processes are cancelled when chat buffers go away."
  (let (buffer clear-cancelled kill-cancelled)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-clear-cancel"))
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'read-shell-command)
                       (lambda (&rest _args) "sleep 10"))
                      ((symbol-function 'e-chat--run-shell-command-start)
                       (lambda (&rest _args)
                         (e-tools-request-create
                          :cancel (lambda ()
                                    (setq clear-cancelled t)
                                    t)))))
              (e-chat-composer-bang))
            (e-chat--clear)
            (should clear-cancelled))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))
          (setq buffer (e-chat-test--buffer nil "chat-prefix-kill-cancel"))
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'read-shell-command)
                       (lambda (&rest _args) "sleep 10"))
                      ((symbol-function 'e-chat--run-shell-command-start)
                       (lambda (&rest _args)
                         (e-tools-request-create
                          :cancel (lambda ()
                                    (setq kill-cancelled t)
                                    t)))))
              (e-chat-composer-bang)))
          (kill-buffer buffer)
          (setq buffer nil)
          (should kill-cancelled))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-bang-captures-nonzero-timeout-and-truncation ()
  "Command references preserve failure, timeout, and truncation state as text."
  (let* ((e-chat-command-output-timeout 7)
         (reference (e-chat--command-output-reference
                     "broken"
                     (list :output "stderr\n"
                           :exit 2
                           :truncated t)))
         (timeout-reference (e-chat--command-output-reference
                             "slow"
                             (list :output "partial\n"
                                   :timed-out t))))
    (should (string-match-p (regexp-quote "$ broken (exit 2)")
                            (plist-get reference :label)))
    (should (string-match-p (regexp-quote "Status: exit 2")
                            (plist-get reference :text)))
    (should (string-match-p (regexp-quote "Output was truncated.")
                            (plist-get reference :text)))
    (should (string-match-p (regexp-quote "stderr")
                            (plist-get reference :text)))
    (should (string-match-p (regexp-quote "$ slow (timed out after 7s)")
                            (plist-get timeout-reference :label)))
    (should (string-match-p (regexp-quote "Status: timed out after 7s")
                            (plist-get timeout-reference :text)))))

(ert-deftest e-chat-test-composer-bang-truncates-real-command-output ()
  "Shell command capture caps oversized output with a visible marker."
  (let ((e-chat-command-output-max-bytes 5)
        (e-chat-command-output-timeout 5))
    (let ((result (e-chat--run-shell-command "printf 0123456789" temporary-file-directory)))
      (should (equal (plist-get result :exit) 0))
      (should (plist-get result :truncated))
      (should (string-prefix-p "01234" (plist-get result :output)))
      (should (string-match-p (regexp-quote "[Command output truncated]")
                              (plist-get result :output))))))

(ert-deftest e-chat-test-sync-command-output-rejects-hot-path ()
  "The synchronous command-output helper fails before starting a shell command."
  (let (started)
    (cl-letf (((symbol-function 'e-chat--run-shell-command-start)
               (lambda (&rest _args)
                 (setq started t)
                 (error "shell command should not start"))))
      (let ((err (should-error
                  (e-request-with-hot-path 'chat-sync-command
                    (e-chat--run-shell-command
                     "printf done"
                     temporary-file-directory))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err)
                       '(e-chat--run-shell-command chat-sync-command))))
      (should-not started))))

(ert-deftest e-chat-test-composer-prefix-cancel-keeps-literal-character ()
  "Cancelling a prefix popup leaves the typed prefix in the composer."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-cancel"))
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'read-shell-command)
                       (lambda (&rest _args) (signal 'quit nil))))
              (e-chat-composer-bang))
            (insert " ")
            (let ((unread-command-events (list ?\C-g)))
              (cl-letf (((symbol-function 'e-chat--project-file-candidates)
                       (lambda () (list (list :label "a.txt" :path "/tmp/a.txt")))))
                (e-chat-composer-at)))
            (insert " ")
            (let ((unread-command-events (list ?\C-g)))
              (cl-letf (((symbol-function 'e-chat--prompt-candidates)
                       (lambda () (list (list :label "review" :prompt 'prompt)))))
                (let ((last-command-event ?/))
                  (e-chat-composer-slash))))
            (should (equal (e-chat--composer-text) "! @ /"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-empty-prefix-candidates-keep-literal-character ()
  "Empty file and prompt candidate sets leave the typed prefix literal."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-empty"))
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'e-chat--project-file-candidates)
                       (lambda () nil))
                      ((symbol-function 'e-chat--prompt-candidates)
                       (lambda () nil)))
              (e-chat-composer-at)
              (insert " ")
              (let ((last-command-event ?/))
                (e-chat-composer-slash)))
            (should (equal (e-chat--composer-text) "@ /"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-at-lists-git-project-files ()
  "Project file candidates come from git and honor exclude-standard rules."
  (let ((directory (make-temp-file "e-chat-prefix-files-" t)))
    (unwind-protect
        (progn
          (process-file "git" nil nil nil "-C" directory "init")
          (with-temp-file (expand-file-name ".gitignore" directory)
            (insert "ignored.txt\n"))
          (with-temp-file (expand-file-name "keep.txt" directory)
            (insert "keep\n"))
          (make-directory (expand-file-name "sub" directory))
          (with-temp-file (expand-file-name "sub/nested.el" directory)
            (insert "(message \"nested\")\n"))
          (with-temp-file (expand-file-name "ignored.txt" directory)
            (insert "ignored\n"))
          (cl-letf (((symbol-function 'e-chat--workspace-roots)
                     (lambda () (list directory))))
            (let ((labels (mapcar (lambda (candidate)
                                    (plist-get candidate :label))
                                  (e-chat--project-file-candidates-sync))))
              (should (member "keep.txt" labels))
              (should (member "sub/nested.el" labels))
              (should-not (member "ignored.txt" labels)))))
      (delete-directory directory t))))

(ert-deftest e-chat-test-composer-at-does-not-fallback-in-ignored-only-git-root ()
  "Git-backed candidate listing does not expose ignored files by fallback scan."
  (let ((directory (make-temp-file "e-chat-prefix-ignored-" t)))
    (unwind-protect
        (progn
          (process-file "git" nil nil nil "-C" directory "init")
          (with-temp-file (expand-file-name ".gitignore" directory)
            (insert "*\n"))
          (with-temp-file (expand-file-name "ignored.txt" directory)
            (insert "ignored\n"))
          (cl-letf (((symbol-function 'e-chat--workspace-roots)
                     (lambda () (list directory))))
            (let ((labels (mapcar (lambda (candidate)
                                    (plist-get candidate :label))
                                  (e-chat--project-file-candidates-sync))))
              (should-not (member "ignored.txt" labels)))))
      (delete-directory directory t))))

(ert-deftest e-chat-test-composer-at-skips-missing-workspace-roots ()
  "Project file completion ignores stale workspace roots and keeps live roots."
  (let ((directory (make-temp-file "e-chat-prefix-live-root-" t))
        (missing (expand-file-name "missing-root" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "keep.txt" directory)
            (insert "keep\n"))
          (when (file-exists-p missing)
            (delete-directory missing t))
          (cl-letf (((symbol-function 'e-chat--workspace-roots)
                     (lambda () (list directory missing))))
            (let ((labels (mapcar (lambda (candidate)
                                    (plist-get candidate :label))
                                  (e-chat--project-file-candidates-sync))))
              (should (member "keep.txt" labels)))))
      (delete-directory directory t))))

(ert-deftest e-chat-test-composer-at-uses-fd-before-recursive-fallback ()
  "Non-git project file completion prefers fd before recursive Lisp fallback."
  (let ((directory (file-name-as-directory
                    (make-temp-file "e-chat-prefix-fd-root-" t))))
    (unwind-protect
        (cl-letf (((symbol-function 'e-chat--workspace-roots)
                   (lambda () (list directory)))
                  ((symbol-function 'e-chat--git-file-candidates)
                   (lambda (_root _limit) nil))
                  ((symbol-function 'e-chat--fd-file-candidates)
                   (lambda (root limit)
                     (should (equal root directory))
                     (should (= limit e-chat-project-file-candidate-limit))
                     (list (expand-file-name "from-fd.txt" root))))
                  ((symbol-function 'e-chat--fallback-file-candidates)
                   (lambda (&rest _args)
                     (error "recursive fallback should not run when fd succeeds"))))
          (let ((labels (mapcar (lambda (candidate)
                                  (plist-get candidate :label))
                                (e-chat--project-file-candidates-sync))))
            (should (equal labels '("from-fd.txt")))))
      (delete-directory directory t))))

(ert-deftest e-chat-test-sync-project-file-candidates-rejects-hot-path ()
  "The synchronous composer file scanner fails before process work in hot paths."
  (let (started)
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _args)
                 (setq started t)
                 (error "process-file should not run"))))
      (let ((err (should-error
                  (e-request-with-hot-path 'chat-file-candidates
                    (e-chat--project-file-candidates-sync))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err)
                       '(e-chat--project-file-candidates-sync
                         chat-file-candidates))))
      (should-not started))))

(ert-deftest e-chat-test-fd-file-candidates-invokes-fd-for-files ()
  "fd candidate collection asks fd for hidden, non-.git regular files."
  (let ((directory (file-name-as-directory
                    (make-temp-file "e-chat-prefix-fd-command-" t)))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'e-chat--fd-executable)
                   (lambda () "fd"))
                  ((symbol-function 'process-file)
                   (lambda (program _infile _destination _display &rest args)
                     (setq calls (cons program args))
                     (insert ".hidden\n")
                     (insert "visible.txt\n")
                     0)))
          (should (equal (e-chat--fd-file-candidates directory 1)
                         (list (expand-file-name ".hidden" directory))))
          (should (equal (car calls) "fd"))
          (should (member "--type" (cdr calls)))
          (should (member "file" (cdr calls)))
          (should (member "--hidden" (cdr calls)))
          (should (member "--exclude" (cdr calls)))
          (should (member ".git" (cdr calls)))
          (should (member "--base-directory" (cdr calls)))
          (should (member directory (cdr calls))))
      (delete-directory directory t))))

(ert-deftest e-chat-test-composer-at-file-candidates-refresh-asynchronously ()
  "Public composer file candidates return cached snapshots and refresh later."
  (let ((buffer (e-chat-test--buffer nil "chat-prefix-files-async"))
        (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--project-file-candidates-sync)
                     (lambda ()
                       (setq calls (1+ calls))
                       (list (list :label "async.txt"
                                   :path "/tmp/async.txt"
                                   :root "/tmp/")))))
            (should-not (e-chat--project-file-candidates))
            (should (= calls 0))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (e-chat--project-file-candidate-cache-hit-p
                        (e-chat--project-file-candidate-cache-key)))))
            (should (= calls 1))
            (should (equal (mapcar (lambda (candidate)
                                     (plist-get candidate :label))
                                   (e-chat--project-file-candidates))
                           '("async.txt")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-at-shows-loading-file-candidates ()
  "Composer @ exposes a loading row while file candidates refresh."
  (let ((buffer (e-chat-test--buffer nil "chat-prefix-files-loading"))
        (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--project-file-candidates-sync)
                     (lambda ()
                       (setq calls (1+ calls))
                       nil)))
            (let ((labels (mapcar (lambda (candidate)
                                    (plist-get candidate :label))
                                  (e-chat--at-candidates))))
              (should (member "files: loading..." labels))
              (should (= calls 0)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-at-file-candidate-refresh-is-latest-only ()
  "Stale composer file candidate refreshes cannot replace newer roots."
  (let ((buffer (e-chat-test--buffer nil "chat-prefix-files-latest"))
        (root-a (file-name-as-directory
                 (make-temp-file "e-chat-prefix-root-a-" t)))
        (root-b (file-name-as-directory
                 (make-temp-file "e-chat-prefix-root-b-" t)))
        roots)
    (unwind-protect
        (with-current-buffer buffer
          (setq roots (list root-a))
          (cl-letf (((symbol-function 'e-chat--workspace-roots)
                     (lambda () roots))
                    ((symbol-function 'e-chat--project-file-candidates-sync)
                     (lambda ()
                       (list (list :label (file-name-nondirectory
                                           (directory-file-name
                                            (car roots)))
                                   :path (expand-file-name "file.txt"
                                                           (car roots))
                                   :root (car roots))))))
            (should-not (e-chat--project-file-candidates))
            (setq roots (list root-b))
            (should-not (e-chat--project-file-candidates))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (let ((candidate
                              (car (e-chat--project-file-candidates))))
                         (equal (plist-get candidate :root) root-b)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root-a t)
      (delete-directory root-b t))))

(ert-deftest e-chat-test-composer-at-file-candidate-refresh-cancels-on-kill ()
  "Killing a chat buffer cancels pending composer file candidate refresh."
  (let ((buffer (e-chat-test--buffer nil "chat-prefix-files-kill"))
        (calls 0))
    (with-current-buffer buffer
      (cl-letf (((symbol-function 'e-chat--project-file-candidates-sync)
                 (lambda ()
                   (setq calls (1+ calls))
                   nil)))
        (e-chat--project-file-candidates)
        (kill-buffer buffer)
        (accept-process-output nil 0.05)
        (should (= calls 0))))))

(ert-deftest e-chat-test-inline-completion-matches-case-insensitive-fuzzy ()
  "Inline completion filters labels by case-insensitive ordered characters."
  (let ((candidates '((:label ".gitignore")
                      (:label ".github/workflows/test.yml")
                      (:label "src/example.el"))))
    (should (equal (mapcar (lambda (candidate)
                             (plist-get candidate :label))
                           (e-chat--inline-completion-matches candidates "GIT"))
                   '(".gitignore" ".github/workflows/test.yml")))
    (should (equal (mapcar (lambda (candidate)
                             (plist-get candidate :label))
                           (e-chat--inline-completion-matches candidates "sre"))
                   '("src/example.el")))))

(ert-deftest e-chat-test-inline-completion-render-keeps-prompt-on-current-line ()
  "Inline completion renders the prompt at point instead of below the composer."
  (let ((text (e-chat--inline-completion-render
               "@ file: "
               '((:label ".gitignore")
                 (:label ".github/workflows/test.yml"))
               0
               "git")))
    (should (string-prefix-p "@ file: git\n> .gitignore" text))
    (should-not (string-prefix-p "\n" text))))

(ert-deftest e-chat-test-inline-completion-del-deletes-filter-character ()
  "DEL removes the previous inline-completion filter character."
  (let ((keys (list ?A ?Z ?\177 ?B ?\r ?\C-g)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-key)
                 (lambda ()
                   (pop keys))))
        (should (equal (e-chat--inline-completion-select
                        "@ file: "
                        '((:label "ab.txt")
                          (:label "ac.txt")))
                       '(:label "ab.txt")))))))

(ert-deftest e-chat-test-composer-at-lists-files-resources-and-capabilities ()
  "Composer @ candidates include files, active resources, and capabilities."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-at-candidates"))
          (with-current-buffer buffer
            (let ((capability
                   (e-capability-create
                    :id 'reference-capability
                    :name "Reference Capability"
                    :instructions "Use references."
                    :resources
                    (list (lambda (store capability)
                            (e-store-register
                             store
                             (e-capability-id capability)
                             "refs/guide.md"
                             :description "Reference guide."
                             :content "Guide content."))))))
              (e-harness-activate-capability e-chat-harness capability)
              (cl-letf (((symbol-function 'e-chat--project-file-candidates)
                         (lambda ()
                           (list (list :label "src/example.el"
                                       :path "/tmp/example.el")))))
                (let* ((candidates (e-chat--at-candidates))
                       (labels (mapcar (lambda (candidate)
                                         (plist-get candidate :label))
                                       candidates)))
                  (should (member "file: src/example.el" labels))
                  (should (member "resource: e://reference-capability/refs/guide.md - Reference guide."
                                  labels))
                  (should (member "capability: reference-capability - Reference Capability"
                                  labels)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-at-inserts-file-reference ()
  "Word-boundary @ inserts a selected project file as an inline reference."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-at"))
          (with-current-buffer buffer
            (insert "see ")
            (let ((unread-command-events (list ?\r)))
              (cl-letf (((symbol-function 'e-chat--at-candidates)
                         (lambda ()
                           (list (list :label "src/example.el"
                                       :kind 'file
                                       :path "/tmp/example.el"))))
                        ((symbol-function 'e-chat--read-file-reference-text)
                         (lambda (path)
                           (should (equal path "/tmp/example.el"))
                           "(message \"hi\")\n")))
                (e-chat-composer-at)))
            (let* ((document (e-chat--composer-document))
                   (references (plist-get document :references))
                   (submission (plist-get (e-chat--composer-submission)
                                          :prompt)))
              (should (= (length references) 1))
              (should (string-match-p (regexp-quote "src/example.el")
                                      (plist-get document :text)))
              (should (string-match-p (regexp-quote "file:///tmp/example.el")
                                      submission))
              (should (string-match-p (regexp-quote "(message \"hi\")")
                                      submission)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-at-uses-inline-completion-popup ()
  "Word-boundary @ selects files through an inline composer popup."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-at-inline"))
          (with-current-buffer buffer
            (insert "see ")
            (let ((unread-command-events (list ?\r)))
              (cl-letf (((symbol-function 'e-chat--at-candidates)
                         (lambda ()
                           (list (list :label "src/example.el"
                                       :kind 'file
                                       :path "/tmp/example.el"))))
                        ((symbol-function 'completing-read)
                         (lambda (&rest _args)
                           (error "composer @ must not use completing-read")))
                        ((symbol-function 'e-chat--read-file-reference-text)
                         (lambda (_path) "(message \"hi\")\n")))
                (e-chat-composer-at)))
            (let ((submission (plist-get (e-chat--composer-submission)
                                         :prompt)))
              (should (string-match-p (regexp-quote "file:///tmp/example.el")
                                      submission)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-at-disambiguates-duplicate-workspace-files ()
  "Duplicate relative file labels remain selectable across workspace roots."
  (let ((primary (file-name-as-directory
                  (make-temp-file "e-chat-prefix-primary-" t)))
        (secondary (file-name-as-directory
                    (make-temp-file "e-chat-prefix-secondary-" t)))
        buffer)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "same.txt" primary)
            (insert "primary\n"))
          (with-temp-file (expand-file-name "same.txt" secondary)
            (insert "secondary\n"))
          (setq buffer (e-chat-test--buffer nil "chat-prefix-duplicate-files"))
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'e-chat--workspace-roots)
                       (lambda () (list primary secondary))))
              (let* ((candidates (e-chat--project-file-candidates-sync))
                     (secondary-path (expand-file-name "same.txt" secondary))
                     (secondary-candidate
                      (cl-find secondary-path candidates
                               :key (lambda (candidate)
                                      (plist-get candidate :path))
                               :test #'equal))
                     (secondary-label (plist-get secondary-candidate :label)))
                (should secondary-candidate)
                (should-not (equal secondary-label "same.txt"))
                (let ((unread-command-events (list ?\C-n ?\r)))
                  (cl-letf (((symbol-function 'e-chat--project-file-candidates)
                             (lambda () candidates)))
                    (e-chat-composer-at)))
                (let ((submission (plist-get (e-chat--composer-submission)
                                             :prompt)))
                  (should (string-match-p
                           (regexp-quote (concat "file://" secondary-path))
                           submission))
                  (should (string-match-p (regexp-quote "secondary")
                                          submission)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory primary t)
      (delete-directory secondary t))))

(ert-deftest e-chat-test-composer-at-inserts-resource-reference ()
  "Selecting an e:// resource inserts URI, description, and content context."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-at-resource"))
          (with-current-buffer buffer
            (let ((capability
                   (e-capability-create
                    :id 'reference-capability
                    :name "Reference Capability"
                    :resources
                    (list (lambda (store capability)
                            (e-store-register
                             store
                             (e-capability-id capability)
                             "refs/guide.md"
                             :description "Reference guide."
                             :content "Guide content."))))))
              (e-harness-activate-capability e-chat-harness capability)
              (insert "see ")
              (let ((unread-command-events (list ?\r)))
                (cl-letf (((symbol-function 'e-chat--project-file-candidates)
                           (lambda () nil)))
                  (e-chat-composer-at)))
              (let ((submission (plist-get (e-chat--composer-submission)
                                           :prompt)))
                (should (string-match-p
                         (regexp-quote "e://reference-capability/refs/guide.md")
                         submission))
                (should (string-match-p (regexp-quote "Reference guide.")
                                        submission))
                (should (string-match-p (regexp-quote "Guide content.")
                                        submission))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-at-inserts-capability-reference ()
  "Selecting a capability inserts guidance and a lean resource list."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-at-capability"))
          (with-current-buffer buffer
            (let ((capability
                   (e-capability-create
                    :id 'reference-capability
                    :name "Reference Capability"
                    :instructions "Use references."
                    :resources
                    (list (lambda (store capability)
                            (e-store-register
                             store
                             (e-capability-id capability)
                             "refs/guide.md"
                             :description "Reference guide."
                             :content "Guide content."))))))
              (e-harness-activate-capability e-chat-harness capability)
              (let ((unread-command-events (list ?\C-n ?\r)))
                (cl-letf (((symbol-function 'e-chat--project-file-candidates)
                           (lambda () nil)))
                  (e-chat-composer-at)))
              (let ((submission (plist-get (e-chat--composer-submission)
                                           :prompt)))
                (should (string-match-p
                         (regexp-quote "The user referenced capability `reference-capability` with @.")
                         submission))
                (should (string-match-p
                         (regexp-quote "consider using the context, actions, tools, or resources provided by this capability")
                         submission))
                (should (string-match-p
                         (regexp-quote "- e://reference-capability/refs/guide.md: Reference guide.")
                         submission))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-at-truncates-file-reference-text ()
  "File reference text is capped with a visible truncation marker."
  (let ((e-chat-file-reference-max-bytes 5)
        (path (make-temp-file "e-chat-file-reference")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "0123456789"))
          (let ((text (e-chat--read-file-reference-text path)))
            (should (string-prefix-p "01234" text))
            (should (string-match-p (regexp-quote "[File reference truncated]")
                                    text))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest e-chat-test-composer-at-bounds-file-reference-read ()
  "File references read only enough bytes to determine truncation."
  (let (args)
    (cl-letf (((symbol-function 'insert-file-contents-literally)
               (lambda (&rest actual-args)
                 (setq args actual-args)
                 (insert "12345"))))
      (let ((e-chat-file-reference-max-bytes 4))
        (should (string-match-p
                 (regexp-quote "[File reference truncated]")
                 (e-chat--read-file-reference-text "/tmp/example.txt")))))
    (should (equal args '("/tmp/example.txt" nil 0 5)))))

(ert-deftest e-chat-test-composer-slash-expands-leading-selected-prompt ()
  "Leading / expands the selected prompt as editable composer text."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-slash"))
          (with-current-buffer buffer
            (let* ((prompt (e-prompt-spec-create
                            :name "review"
                            :description "Review code."
                            :parameters
                            (list (e-prompt-parameter-create
                                   :name "focus"
                                   :description "Focus."))
                            :template "Review ${focus}."))
                   (capability (e-capability-with-prompts-create
                                :id 'review-prompts
                                :name "Review Prompts"
                                :instructions "Use review prompts."
                                :prompts (list prompt))))
              (e-harness-activate-capability e-chat-harness capability)
              (let ((unread-command-events (list ?\r)))
                (cl-letf (((symbol-function 'e-chat--collect-prompt-arguments)
                           (lambda (selected)
                             (should (eq selected prompt))
                             '(("focus" . "regressions")))))
                  (e-chat-composer-slash))))
            (should (equal (e-chat--composer-text)
                           "Review regressions."))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-slash-uses-inline-completion-popup ()
  "Word-boundary / selects prompts through an inline composer popup."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-slash-inline"))
          (with-current-buffer buffer
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
            (should (equal (e-chat--composer-text) "Review now."))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-slash-render-error-does-not-insert ()
  "Prompt render errors surface as user errors without partial insertion."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-slash-error"))
          (with-current-buffer buffer
            (let ((prompt (e-prompt-spec-create
                           :name "needs-topic"
                           :description "Needs topic."
                           :parameters
                           (list (e-prompt-parameter-create
                                  :name "topic"
                                  :description "Topic."))
                           :template "Topic ${topic}.")))
              (let ((unread-command-events (list ?\r)))
                (cl-letf (((symbol-function 'e-chat--prompt-candidates)
                         (lambda () (list (list :label "needs-topic"
                                                :prompt prompt))))
                        ((symbol-function 'e-chat--collect-prompt-arguments)
                         (lambda (_prompt) nil)))
                  (should-error (e-chat-composer-slash) :type 'user-error)
                  (should (string-empty-p (e-chat--composer-text))))))))
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

(ert-deftest e-chat-test-owned-face-defaults-refresh-separator-face ()
  "Live reload reapplies package-owned separator face defaults."
  (unwind-protect
      (progn
        (set-face-attribute 'e-chat-separator-face nil
                            :inherit 'unspecified
                            :foreground "#ffffff"
                            :background "#000000")
        (e-chat--apply-owned-face-defaults)
        (should (equal (face-attribute 'e-chat-separator-face :inherit)
                       'shadow))
        (should (eq (face-attribute 'e-chat-separator-face :foreground)
                    'unspecified))
        (should (eq (face-attribute 'e-chat-separator-face :background)
                    'unspecified)))
    (e-chat--apply-owned-face-defaults)))

(ert-deftest e-chat-test-user-and-assistant-headings-are-glyph-only ()
  "User and assistant message headings render only their compact glyphs."
  (should (equal e-chat--user-glyph ">"))
  (should (equal e-chat--assistant-glyph "●")))

(ert-deftest e-chat-test-assistant-markdown-renders-with-text-properties ()
  "Assistant messages keep Markdown text and use markdown-mode faces."
  (skip-unless (require 'markdown-mode nil t))
  (let ((buffer (e-chat-test--buffer nil "chat-markdown")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--insert-entry
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
          (should (eq (e-chat--output-mode) 'org))
          (e-chat--insert-entry
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
          (e-chat--insert-entry
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
          (e-chat--insert-entry
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
                         :arguments (:text "hi")))
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
          (e-chat--render-event
           (e-events-make :type 'turn-failed
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:error "boom")))
          (e-chat--render-event
           (e-events-make :type 'turn-cancelled
                          :session-id e-chat-session-id
                          :turn-id "turn-2"))
          (e-chat--render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-3"
                          :payload '(:result (:status ok))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote e-chat--system-glyph)
                             " System\nTurn failed: boom")
                     content))
            (should (string-match-p
                     (concat (regexp-quote e-chat--system-glyph)
                             " System\nTurn cancelled")
                     content))
            (should-not (string-match-p "(:status ok)" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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

(ert-deftest e-chat-test-settled-summary-includes-generic-message-detail ()
  "A capability-declared message summary appears in the settled summary."
  (let ((record '(:started-at 10 :ended-at 772 :has-provider-activity t
                  :action-count 9)))
    (plist-put
     record :message-details
     (list
      (cons "message-1"
            (list (e-message-detail-create
                   :id 'claims :summary "2 claims"
                   :body "Claims\n- one\n- two")))))
    (should (equal (e-chat--activity-summary-text record)
                   "Turn took 12min 42sec, 9 actions (2 claims)."))))

(ert-deftest e-chat-test-final-claim-message-renders-and-expands-details ()
  "A final claim stays intact, advertises details, and expands with RET."
  (let ((buffer (e-chat-test--buffer nil "chat-final-claim-details"))
        (content
         (concat
          "Yes. It used the Bayesian `#+begin_reasoning` structure.\n\n"
          "#+begin_reasoning\n"
          "claim: The daily was populated\n"
          "confidence: high\n"
          "alternatives: stale note state\n"
          "evidence: src:ABCDEF12\n"
          "#+end_reasoning\n")))
    (unwind-protect
        (with-current-buffer buffer
          (e-harness-activate-capability
           e-chat-harness (e-bayesian-reasoning-capability-create))
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1" :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1" :created-at 10
                          :payload nil))
          (e-chat--render-event
           (e-events-make
            :type 'message-added :session-id e-chat-session-id
            :turn-id "turn-1" :created-at 22
            :payload (list :message
                           (list :id "message-1" :role 'assistant
                                 :turn-id "turn-1" :content content))))
          (e-chat--render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1" :created-at 22))
          (let ((rendered (buffer-string)))
            (should (string-match-p
                     (regexp-quote
                      "Yes. It used the Bayesian `#+begin_reasoning` structure.")
                     rendered))
            (should-not (string-match-p
                         (regexp-quote "\n#+begin_reasoning\n") rendered))
            (should (string-match-p
                     (regexp-quote "Turn took 0min 12sec (1 claim).")
                     rendered)))
          (e-chat-test--focus-block-containing "Yes. It used the Bayesian")
          (let ((block (e-chat--focused-block)))
            (should (eq (plist-get block :kind) 'final))
            (e-chat-response-navigation-activate)
            (should (e-chat--block-details-visible-p block))
            (should (string-match-p "Claims\n- The daily was populated"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-pending-hook-summary-keeps-validation-visible ()
  "A queued hook follow-up retains its capability-provided activity label."
  (let ((record '(:started-at 10 :ended-at 772 :has-provider-activity t
                  :final-rendered t)))
    (e-chat--record-hook-audit
     record '(:pending-summary "Validating claims…") 'activity)
    (should (string-match-p
             "Validating claims…"
             (plist-get (e-chat--running-status-data "turn-1" record) :text)))))

(ert-deftest e-chat-test-terminal-hook-audit-clears-pending-summary ()
  "A terminal hook audit removes a queued correction's validation label."
  (let ((record '(:started-at 10 :ended-at 772 :has-provider-activity t
                  :final-rendered t)))
    (e-chat--record-hook-audit
     record '(:pending-summary "Validating claims…") 'activity)
    (e-chat--record-hook-audit
     record '(:summary "Claim correction unresolved"
              :pending-summary nil
              :details (:correction failed))
     'activity)
    (should-not (plist-get record :pending-hook-summary))))

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
          (should (string-match-p (regexp-quote e-chat--assistant-glyph)
                                  (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-failed-turn-expands-full-error-inline ()
  "RET on a focused failed system block expands provider details inline."
  (let ((buffer (e-chat-test--buffer nil "chat-failed-details")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make
            :type 'turn-failed
            :session-id e-chat-session-id
            :turn-id "turn-1"
            :created-at 10
            :payload
            '(:error "OpenAI request failed: (error http 400)"
              :details (:type "response.failed"
                        :response
                        (:error
                         (:code "context_length_exceeded"
                          :message
                          "Your input exceeds the context window."))))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Turn failed: OpenAI request failed: (error http 400)"
                     content))
            (should-not (string-match-p "context_length_exceeded" content)))
          (e-chat-test--focus-block-containing "Turn failed")
          (should (eq (plist-get (e-chat-test--focused-block) :kind)
                      'system))
          (e-chat-response-navigation-activate)
          (let ((content (buffer-string))
                (block (e-chat-test--focused-block)))
            (should e-chat-block-view-mode)
            (should-not e-chat-response-navigation-mode)
            (should (e-chat--block-details-visible-p block))
            (should (<= (marker-position (plist-get block :details-start-marker))
                        (point)))
            (should (<= (point)
                        (marker-position (plist-get block :details-end-marker))))
            (when (e-chat--composer-active-p)
              (should (< (point)
                         (marker-position e-chat--composer-start-marker))))
            (should-not (get-buffer e-chat-details-buffer-name))
            (should (string-match-p "OpenAI request failed" content))
            (should (string-match-p "context_length_exceeded" content))
            (should (string-match-p
                     "Your input exceeds the context window"
                     content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-turn-started-shows-moving-assistant-progress ()
  "An active assistant turn shows a protected moving glyph indicator."
  (let ((buffer (e-chat-test--buffer nil "chat-progress")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
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
                     (concat (regexp-quote e-chat--assistant-glyph)
                             " ⠋")
                     content))
            (goto-char (point-min))
            (search-forward "⠋")
            (should (get-text-property (1- (point)) 'read-only)))
          (e-chat--advance-progress-indicator)
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph)
                           " ⠙")
                   (buffer-string)))
          (should (>= e-chat-progress-interval 0.5))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (should (equal e-chat--progress-turn-id "turn-1"))
          (e-chat--render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote e-chat--assistant-glyph)
                             " Final answer.")
                     content))
            (should-not (string-match-p
                         (concat (regexp-quote e-chat--assistant-glyph)
                                 " [⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]")
                         content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-provider-wait-and-stream-statuses ()
  "Provider lifecycle and stream events update the compact running status."
  (let ((buffer (e-chat-test--buffer nil "chat-provider-status")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
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
          (should (equal e-chat--progress-turn-id "turn-1"))
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type reasoning-delta
                                      :content "thinking")))
          (should (string-match-p "reasoning" header-line-format))
          (e-chat--render-event
           (e-events-make :type 'assistant-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type assistant-delta
                                      :content "answer")))
          (should (string-match-p "streaming" header-line-format)))
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

(ert-deftest e-chat-test-turn-steered-schedules-visible-indicator ()
  "A steering event schedules a visible active-turn indicator."
  (let ((buffer (e-chat-test--buffer nil "chat-steered-indicator")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
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

(ert-deftest e-chat-test-format-duration-uses-minutes-and-seconds ()
  "Turn progress durations use minute/second labels."
  (should (equal (e-chat--format-duration 0 3) "0min 3sec"))
  (should (equal (e-chat--format-duration 0 45) "0min 45sec"))
  (should (equal (e-chat--format-duration 0 63) "1min 3sec"))
  (should (equal (e-chat--format-duration 0 205) "3min 25sec")))

(ert-deftest e-chat-test-provider-round-trip-renders-thinking-and-thought ()
  "Provider request boundaries render active and completed thinking lines."
  (let ((buffer (e-chat-test--buffer nil "chat-provider-round-trip")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                     (lambda (&optional _time) 8.0)))
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started)))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          (should (string-match-p "⠋ Thinking for 0min 8sec"
                                  (buffer-string)))
          (e-chat--render-event
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

(ert-deftest e-chat-test-failed-turn-clears-transient-before-next-prompt ()
  "A failed turn drops its transient activity so the next prompt renders.
Regression: after turn-failed the active \"Thinking...\"/\"Thought for ...\"
block and its separators lingered, and the next submitted prompt rendered into
the orphaned region and appeared to vanish."
  (let ((buffer (e-chat-test--buffer nil "chat-failed-then-prompt")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                     (lambda (&optional _time) 8.0)))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1" :created-at 0))
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1" :created-at 0
                            :payload '(:status started)))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          ;; The active thinking transient is on screen.
          (should (string-match-p "Thinking for" (buffer-string)))
          ;; Turn fails.
          (e-chat--render-event
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
          (should-not (and (markerp e-chat--progress-start-marker)
                           (marker-position e-chat--progress-start-marker)))
          ;; A new prompt for a fresh turn renders and stays visible.
          (e-chat--render-event
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

(ert-deftest e-chat-test-active-thinking-row-shows-spinner-and-duration ()
  "Active provider requests show a moving thinking row with current duration."
  (let ((buffer (e-chat-test--buffer nil "chat-active-thinking-duration")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                     (lambda (&optional _time) 8.0)))
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
              (e-ui-work-drain-batch :buffer (current-buffer))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠋ Thinking for 0min 8sec" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-updates-active-thinking-row ()
  "Progress redraw updates the active thinking row without duplicating it."
  (let ((buffer (e-chat-test--buffer nil "chat-active-thinking-rerender")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                     (lambda (&optional _time) 8.0)))
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
                            :payload '(:status started))))
          (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                     (lambda (&optional _time) 15.0)))
            (e-chat--advance-progress-indicator)
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

(ert-deftest e-chat-test-progress-rerender-touches-only-active-tail ()
  "Progress redraw leaves completed activity rounds physically untouched."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-active-tail")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-test--mark-active-turn "turn-1")
          (dotimes (index 24)
            (let ((started-at (* index 2)))
              (e-chat--render-event
               (e-events-make :type 'provider-request-started
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :created-at started-at
                              :payload '(:status started)))
              (unless (= index 23)
                (e-chat--render-event
                 (e-events-make :type 'provider-request-finished
                                :session-id e-chat-session-id
                                :turn-id "turn-1"
                                :created-at (1+ started-at)
                                :payload '(:status done))))))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)))
          (let* ((tail-start (marker-position e-chat--progress-start-marker))
                 (stable-prefix
                  (buffer-substring-no-properties (point-min) tail-start))
                 changes)
            (add-hook 'before-change-functions
                      (lambda (start end)
                        (push (cons start end) changes))
                      nil t)
            (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                       (lambda (&optional _time) 60.0)))
              (e-chat--advance-progress-indicator)
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

(ert-deftest e-chat-test-live-activity-bounds-rounds-with-complete-details ()
  "Live activity is bounded while settled details retain every round."
  (let* ((e-chat-live-activity-round-limit 5)
         (rounds
          (cl-loop
           for index from 1 to 8
           collect
           `(:kind round
             :round ,index
             :started-at ,(* index 2)
             :ended-at ,(1+ (* index 2))
             :status done
             :reasoning ((:content ,(format "reasoning-%d" index)))
             :tool-batches
             ((:items ((:id ,(format "call-%d" index)
                        :call ,(format "tool-%d" index)
                        :output ,(format "output-%d" index))))))))
         (record (list :id "turn-1" :activity-records rounds))
         (data (e-chat--activity-record-transient-data record))
         (text (plist-get data :text))
         (expanded (e-chat--activity-expanded-text record))
         (live-tools (e-chat--activity-tool-items record t))
         (all-tools (e-chat--activity-tool-items record)))
    (should (= (plist-get data :omitted-round-count) 3))
    (should (string-match-p "3 earlier activity rounds omitted" text))
    (should-not (string-match-p "reasoning-1" text))
    (should (string-match-p "reasoning-4" text))
    (should (string-match-p "reasoning-8" text))
    (should (string-match-p "reasoning-1" expanded))
    (should-not (string-match-p "activity rounds omitted" expanded))
    (should (= (length live-tools) 5))
    (should (equal (plist-get (car live-tools) :call) "tool-4"))
    (should (= (length all-tools) 8))))

(ert-deftest e-chat-test-progress-rerender-updates-between-provider-and-tool ()
  "Progress redraw keeps counting after a provider settles within a live turn."
  (let ((buffer (e-chat-test--buffer nil "chat-between-step-progress")))
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
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:status done)))
          (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                     (lambda (&optional _time) 8.0)))
            (e-chat--advance-progress-indicator)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠙ Working for 0min 8sec" content))
            (should-not (string-match-p
                         "Thought for 0min 1sec" content)))
          (e-chat--stop-progress-indicator "turn-1")
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Thought for 0min 1sec" content))
            (should-not (string-match-p
                         "Working for" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
            (e-chat-service--drain-subscription e-chat--event-subscription)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (should (equal e-chat--progress-turn-id presentation-turn-id))
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
            (let* ((record
                    (e-chat--existing-turn-record presentation-turn-id))
                   (started-at
                    (plist-get (e-chat--last-round-record record) :started-at)))
              (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                         (lambda (&optional _time) (+ started-at 8.0))))
                (e-chat--advance-progress-indicator)
                (e-ui-work-with-batch-drain
                  (e-ui-work-drain-batch :buffer (current-buffer)))))
            (should (equal e-chat--progress-turn-id presentation-turn-id))
            (should (e-chat-test--live-work-handle-p
                     e-chat--progress-interval-handle))
            (should (string-match-p
                     "Working for 0min 8sec" (buffer-string)))))
      (when (and harness session-id)
        (remhash session-id (e-harness-active-turns harness)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-active-activity-restores-missing-progress-interval ()
  "Rendering active activity restarts a missing progress interval."
  (let ((buffer (e-chat-test--buffer nil "chat-active-thinking-interval-restore")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                     (lambda (&optional _time) 8.0)))
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
                            :payload '(:status started))))
          (e-chat--cancel-progress-interval)
          (setq e-chat--progress-turn-id nil)
          (setq e-chat--progress-frame 0)
          (setq e-chat--progress-next-tick-time nil)
          (let ((record (e-chat--existing-turn-record "turn-1")))
            (should (e-chat--active-activity-p record))
            (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                       (lambda (&optional _time) 15.0)))
              (e-chat--render-turn-transient "turn-1" record))
            (should (equal e-chat--progress-turn-id "turn-1"))
            (should (e-chat-test--live-work-handle-p
                     e-chat--progress-interval-handle))
            (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                       (lambda (&optional _time) 16.0)))
              (e-chat--advance-progress-indicator)
              (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
            (let ((content (buffer-string)))
              (should (string-match-p "⠙ Thinking for 0min 16sec" content))
              (should (= (e-chat-test--count-occurrences
                          "Thinking for" content)
                         1)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-interval-stops-when-harness-turn-settled ()
  "Progress ticks stop when the harness no longer has the progress turn active."
  (let ((buffer (e-chat-test--buffer nil "chat-stale-progress-turn")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (puthash e-chat-session-id
                   '(:id "turn-1" :status done)
                   (e-harness-active-turns e-chat-harness))
          (e-chat--advance-progress-indicator)
          (should-not e-chat--progress-turn-id)
          (should-not (e-chat-test--live-work-handle-p
                       e-chat--progress-interval-handle))
          (should-not (string-match-p "Thinking for" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-orphaned-progress-interval-settles-itself ()
  "A progress interval not owned by its buffer-local state settles itself."
  (let ((buffer (e-chat-test--buffer nil "chat-orphan-progress-interval"))
        handle
        timer)
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--start-progress-indicator "turn-1")
          (setq handle e-chat--progress-interval-handle)
          (setq timer (plist-get (e-work-handle-metadata handle) :timer))
          (setq e-chat--progress-interval-handle nil)
          (funcall (timer--function timer))
          (should (e-request-terminal-p
                   (e-work-handle-lifecycle handle))))
      (when (timerp timer)
        (cancel-timer timer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-tool-count-renders-on-thinking-row ()
  "A round's tool count renders on the same line as its activity row.
Once a tool completes, the left cell settles back to \"Thought for ...\"."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-count-thinking-row")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:id "call-1" :name "read")))
          (e-chat--render-event
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
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          ;; Freeze the clock so the elapsed duration is deterministic.
          (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                     (lambda () 25)))
            (e-chat--render-event
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
          (e-chat--render-event
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
            (when-let ((bounds (e-chat--running-status-bounds)))
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

(ert-deftest e-chat-test-activity-redraw-coalesces-reentrant-run ()
  "A redraw requested during an activity redraw is deferred, not nested."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-redraw-reentrant"))
        render-count
        reentered)
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (cl-letf (((symbol-function 'e-chat--render-turn-transient)
                     (lambda (turn-id _record)
                       (setq render-count (1+ (or render-count 0)))
                       (unless reentered
                         (setq reentered t)
                         (e-chat--request-activity-redraw turn-id 'activity)
                         (e-chat--run-pending-activity-redraw)))))
            (e-chat-test--run-pending-ui-work
             'activity-redraw "turn-1"))
          (should (= render-count 1))
          (should (equal e-chat--pending-activity-redraw-turn-id "turn-1"))
          (should (e-chat-test--live-work-handle-p
                   e-chat--pending-activity-redraw-handle))
          (should (e-chat-test--pending-ui-work
                   'activity-redraw "turn-1")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (e-chat--cancel-pending-activity-redraw))
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-tool-row-collapses-parallel-tools ()
  "Several tools running at once collapse into a counted running row."
  (let ((buffer (e-chat-test--buffer nil "chat-running-tools-parallel")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (dolist (tool '(("call-1" "read")
                          ("call-2" "grep")))
            (e-chat--render-event
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
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:id "call-1" :name "bash")))
          (e-chat--render-event
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
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat--render-event
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
  (should (boundp 'e-chat--activity-separator))
  (should (equal e-chat--response-separator
                 e-chat--composer-separator))
  (should (equal e-chat--activity-separator
                 (make-string 64 ?┈)))
  (should-not (equal e-chat--activity-separator
                     e-chat--response-separator)))

(ert-deftest e-chat-test-activity-separator-uses-dim-activity-face ()
  "Activity round dividers use a dim face inside the activity block."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-separator-face")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 20
                          :payload '(:status started)))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (goto-char (point-min))
          (search-forward e-chat--activity-separator)
          (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                      'e-chat-activity-separator-face)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-reasoning-has-space-after-thought-row ()
  "Reasoning text has a small visual gap after the thought row."
  (let ((buffer (e-chat-test--buffer nil "chat-reasoning-spacer")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                     (lambda (&optional _time) 8.0)))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started)))
            (e-chat--render-event
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

(ert-deftest e-chat-test-provider-error-settles-thinking-line ()
  "Provider errors turn open thinking into a failed thought line."
  (let ((buffer (e-chat-test--buffer nil "chat-provider-error-thinking")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:status error)))
          (e-chat--render-event
           (e-events-make :type 'turn-failed
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:error "provider failed")))
          (let* ((record (e-chat--existing-turn-record "turn-1"))
                 (round (car (plist-get record :activity-records))))
            (should (equal (plist-get round :status) 'failed))
            (should (string-match-p
                     "Thought failed after 0min 12sec"
                     (e-chat--activity-expanded-text record))))
          (let ((content (buffer-string)))
            (should (string-match-p "Turn failed: provider failed" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))
            ;; The abnormal end still surfaces the settled duration summary.
            (should (string-match-p "Turn took 0min 12sec\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-turn-cancelled-settles-thinking-line ()
  "Turn cancellation turns open thinking into a cancelled thought line."
  (let ((buffer (e-chat-test--buffer nil "chat-cancelled-thinking")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'turn-cancelled
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12))
          (let* ((record (e-chat--existing-turn-record "turn-1"))
                 (round (car (plist-get record :activity-records))))
            (should (equal (plist-get round :status) 'cancelled))
            (should (string-match-p
                     "Thought cancelled after 0min 12sec"
                     (e-chat--activity-expanded-text record))))
          (let ((content (buffer-string)))
            (should (string-match-p "Turn cancelled" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))
            ;; The abnormal end still surfaces the settled duration summary.
            (should (string-match-p "Turn took 0min 12sec\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-final-turn-collapses-progress-to-summary ()
  "Settled activity collapses to a navigable turn summary."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-summary"))
        (details-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--activity-summary-details-text)
                     (lambda (&rest _args)
                       (setq details-calls (1+ details-calls))
                       "eager details")))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started)))
            (e-chat--render-event
             (e-events-make :type 'provider-request-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 63
                            :payload '(:status done)))
            (dotimes (index 2)
              (e-chat--render-event
               (e-events-make :type 'tool-started
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :created-at 64
                              :payload (list :type 'tool-call
                                             :id (format "call-%d" index)
                                             :name "read"))))
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 160
                            :payload '(:status started)))
            (e-chat--render-event
             (e-events-make :type 'provider-request-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 205
                            :payload '(:status done)))
            (e-chat--render-event
             (e-events-make :type 'message-added
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 205
                            :payload '(:message (:role assistant
                                                  :content "Final answer."))))
            (e-chat--render-event
             (e-events-make :type 'turn-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 205))
            (should (= details-calls 0)))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Turn took 3min 25sec, 2 tool calls\\." content))
            (should (string-match-p
                     (concat (regexp-quote e-chat--assistant-glyph)
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

(ert-deftest e-chat-test-intermediate-assistant-keeps-turn-progress-active ()
  "Assistant messages do not settle presentation before the terminal event."
  (let ((buffer (e-chat-test--buffer nil "chat-intermediate-assistant")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:message (:role assistant
                                                :content "First answer."))))
          (let ((record (e-chat--existing-turn-record "turn-1")))
            (should (equal e-chat--progress-turn-id "turn-1"))
            (should-not (plist-get record :ended-at))
            (should-not (plist-get record :final-rendered))
            (should (string-match-p "First answer" (buffer-string)))
            (should-not (string-match-p "Turn took" (buffer-string))))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 3))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 5
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 5
                          :payload '(:message (:role assistant
                                                :content "Corrected answer."))))
          (should (equal e-chat--progress-turn-id "turn-1"))
          (should-not (string-match-p "Turn took" (buffer-string)))
          (e-chat--render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 6
                          :payload '(:reason stop)))
          (let ((record (e-chat--existing-turn-record "turn-1"))
                (content (buffer-string)))
            (should-not e-chat--progress-turn-id)
            (should (= (plist-get record :ended-at) 6))
            (should (plist-get record :final-rendered))
            (should (= (save-excursion
                         (goto-char (point-min))
                         (how-many "Turn took" (point-min) (point-max)))
                       1))
            (should (string-match-p
                     (concat "Corrected answer\\.\n\n"
                             "Turn took 0min 6sec\\.")
                     content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-action-activity-renders-in-summary ()
  "Action activity events render in the settled turn summary."
  (let ((buffer (e-chat-test--buffer nil "chat-action-summary")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'action-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:capability-id workspace-awareness
                                      :action :focus-buffer)))
          (e-chat--render-event
           (e-events-make :type 'action-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:capability-id workspace-awareness
                                      :action :focus-buffer
                                      :status ok
                                      :result (:content "focused"))))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:message (:role assistant
                                                :content "Done."))))
          (e-chat--render-event
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

(ert-deftest e-chat-test-activity-summary-expands-to-child-blocks ()
  "Settled activity summary expansion creates navigable child blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-summary-children")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:content "planning")))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 63
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 64
                          :payload '(:id "call-1" :name "read")))
          (e-chat--render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 65
                          :payload '(:tool-call (:id "call-1" :name "read")
                                      :result (:status ok
                                               :content "contents"))))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 160))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 205
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 205
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (e-chat--render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 205))
          (goto-char (point-max))
          (insert "follow-up draft")
          (e-chat-test--focus-block-containing "Turn took 3min 25sec")
          (let* ((summary (e-chat-test--focused-block))
                 (summary-id (plist-get summary :id)))
            (call-interactively #'e-chat-response-navigation-activate)
            (let* ((children (plist-get summary :children))
                   (child-kinds
                    (mapcar
                     (lambda (block-id)
                       (plist-get (gethash block-id e-chat--block-registry)
                                  :kind))
                     children))
                   (summary-index (cl-position summary-id e-chat--block-order
                                               :test #'equal)))
              (should (equal child-kinds
                             '(activity-thought activity-reasoning
                               activity-tool-batch activity-thought)))
              (should (equal (cl-subseq e-chat--block-order
                                        (1+ summary-index)
                                        (+ 1 summary-index
                                           (length children)))
                             children))
              (should (string-match-p "follow-up draft"
                                      (buffer-string)))
              (e-chat--focus-block (car children))
              (should (equal (call-interactively
                              #'e-chat-response-navigation-copy)
                             "Thought for 1min 3sec"))
              (e-chat--focus-block (cadr children))
              (should (equal (call-interactively
                              #'e-chat-response-navigation-copy)
                             "planning"))
              (e-chat--focus-block (nth 2 children))
              (call-interactively #'e-chat-response-navigation-activate)
              (should e-chat-tool-list-mode)
              (should (= (length (plist-get (e-chat--tool-list-block)
                                            :tool-items))
                         1))
              (e-chat-tool-list-back)
              (e-chat--focus-block summary-id)
              (call-interactively #'e-chat-response-navigation-activate)
              (dolist (child children)
                (should-not (gethash child e-chat--block-registry))
                (should-not (member child e-chat--block-order))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-activity-records-track-round-ownership ()
  "Provider, reasoning, and tool events build semantic round records."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-records")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:content "planning")))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 63
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 64
                          :payload '(:id "call-1"
                                      :name "read"
                                      :arguments (:path "file"))))
          (e-chat--render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 65
                          :payload '(:tool-call (:id "call-1"
                                                :name "read")
                                      :result (:status ok
                                               :content "contents"))))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 160
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 205
                          :payload '(:status done)))
          (let* ((record (e-chat--existing-turn-record "turn-1"))
                 (rounds (plist-get record :activity-records))
                 (first-round (car rounds))
                 (second-round (cadr rounds))
                 (first-tool-batch
                  (car (plist-get first-round :tool-batches)))
                 (first-tool
                  (car (plist-get first-tool-batch :items))))
            (should (= (length rounds) 2))
            (should (equal (plist-get first-round :kind) 'round))
            (should (equal (plist-get first-round :round) 1))
            (should (equal (plist-get first-round :status) 'done))
            (should (equal (plist-get first-round :started-at) 0))
            (should (equal (plist-get first-round :ended-at) 63))
            (should (equal (plist-get (car (plist-get first-round :reasoning))
                                      :content)
                           "planning"))
            (should (equal (plist-get first-tool :id) "call-1"))
            (should (string-match-p "read" (plist-get first-tool :call)))
            (should (string-match-p "contents" (plist-get first-tool :output)))
            (should (equal (plist-get second-round :round) 2))
            (should (null (plist-get second-round :tool-batches)))
            (plist-put record :intermittent-entries nil)
            (plist-put record :ended-at 205)
            (plist-put record :final-rendered t)
            (should (equal (e-chat--activity-summary-text record)
                           "Turn took 3min 25sec, 1 tool call."))
            (should (string-match-p
                     "Thought for 1min 3sec"
                     (e-chat--activity-expanded-text record)))
            (should (= (length (e-chat--activity-tool-items record)) 1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-replayed-provider-activity-restores-summary ()
  "Replayed provider boundary events restore settled summary plus expansion."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-provider-replay")
          (e-session-append-message
           store "chat-provider-replay"
           '(:role user :content "inspect" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'provider-request-started
           '(:status started))
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'provider-request-finished
           '(:status done))
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'tool-started
           '(:type tool-call :id "call-1" :name "buffer-read"))
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'tool-finished
           '(:tool-call (:type tool-call :id "call-1" :name "buffer-read")
             :result (:status ok :content "scratch contents")))
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'turn-finished nil)
          (e-session-append-message
           store "chat-provider-replay"
           '(:role assistant :content "Final answer." :turn-id "turn-1"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-provider-replay")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-provider-replay"))
          (with-current-buffer buffer
            (let ((content (buffer-string)))
              (should (string-match-p
                       "Turn took [0-9]+min [0-9]+sec, 1 tool call\\."
                       content))
              (should-not (string-match-p "Thought for" content)))
            (e-chat-test--focus-block-containing "Turn took")
            (call-interactively #'e-chat-response-navigation-activate)
            (let ((content (buffer-string)))
              (should (string-match-p "Thought for [0-9]+min [0-9]+sec"
                                      content))
              (should (string-match-p "1 tool call" content)))
            (let* ((record (e-chat--existing-turn-record "turn-1"))
                   (rounds (plist-get record :activity-records))
                   (tool-batch (car (plist-get (car rounds) :tool-batches)))
                   (summary (e-chat-test--focused-block))
                   (child-kinds
                    (mapcar
                     (lambda (block-id)
                       (plist-get (gethash block-id e-chat--block-registry)
                                  :kind))
                     (plist-get summary :children))))
              (should (= (length rounds) 1))
              (should (equal (plist-get (car rounds) :status) 'done))
              (should (= (length (plist-get tool-batch :items)) 1))
              (should (equal child-kinds
                             '(activity-thought activity-tool-batch))))))
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
            (cl-letf (((symbol-function 'e-chat--current-time-seconds)
                       (lambda (&optional _time) 8.0)))
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer (current-buffer))))
            (let ((content (buffer-string)))
              (should (string-match-p
                       "Thinking for 0min [0-9]+sec" content)))
            (should (equal e-chat--progress-turn-id "turn-1"))
            (should (e-chat-test--live-work-handle-p
                     e-chat--progress-interval-handle))
            (should (e-chat--composer-active-p))))
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
            (should-not e-chat--progress-turn-id)
            (should-not (e-chat-test--live-work-handle-p
                         e-chat--progress-interval-handle))
            (should (e-chat--composer-active-p))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-late-progress-tick-shows-emacs-blocked-status ()
  "A delayed progress interval tick only reports a local Emacs stall."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-stall"))
        (e-chat-progress-interval 0.5))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (setq e-chat--progress-next-tick-time
                (- (float-time) 7.0))
          (e-chat--advance-progress-indicator)
          (should (string-match-p
                   "Emacs was blocked for [0-9]+s; checking turn state"
                   header-line-format))
          (should (equal e-chat--progress-turn-id "turn-1")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-keeps-editable-composer-draft ()
  "Progress redraws keep the follow-up composer editable with draft text."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-editable-composer")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (should (e-chat--composer-active-p))
          (goto-char (point-max))
          (insert "follow-up draft")
          (e-chat--advance-progress-indicator)
          (should (e-chat--composer-active-p))
          (should (equal (e-chat--composer-text) "follow-up draft")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-durable-entry-stays-above-stale-running-status ()
  "Durable entries stay above visible transient output with no progress owner."
  (let ((buffer (e-chat-test--buffer nil "chat-stale-running-order")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:content "thought")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (setq e-chat--progress-turn-id nil)
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:message (:role user
                                                :content "continue"))))
          (e-chat--render-event
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
                               (search-forward "thought")))
                (composer-pos (marker-position e-chat--composer-start-marker)))
            (should (< user-pos system-pos))
            (should (< system-pos thought-pos))
            (should (< thought-pos composer-pos))
            (should (e-chat--composer-active-p))))
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
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-2"
                            :created-at 20))
            (e-chat-test--mark-active-turn "turn-2")
            (goto-char (point-max))
            (insert "follow-up draft")
            (goto-char (point-min))
            (set-window-point window (point))
            (set-window-start window (point))
            (let ((before-point (point))
                  (before-window-point (window-point window))
                  (before-window-start (window-start window)))
              (e-chat--advance-progress-indicator)
              (should (= (point) before-point))
              (should (= (window-point window) before-window-point))
              (should (= (window-start window) before-window-start))
              (should (e-chat--composer-active-p))
              (should (equal (e-chat--composer-text) "follow-up draft")))))
      (when (window-live-p window)
        (delete-window window))
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
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 10))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 11))
            (e-chat--render-event
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
            (e-chat--set-window-output-follow-state window nil)
            (let ((before-point (point))
                  (before-window-point (window-point window))
                  (before-window-start (window-start window)))
              (e-chat--render-event
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

(ert-deftest e-chat-test-activity-rerender-keeps-running-status-tail ()
  "Activity redraws keep following output when focus was at the active tail."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-status-tail"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (select-window window)
          (with-current-buffer buffer
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 10))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 11))
            (e-chat--render-event
             (e-events-make :type 'reasoning-delta
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:content "first chunk")))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (e-chat--show-latest-output)
            (let ((old-tail (cdr (e-chat--running-status-bounds)))
                  (old-composer (marker-position e-chat--composer-start-marker)))
              (should old-tail)
              (should old-composer)
              (should (= (point) old-composer))
              (should (= (window-point window) old-composer))
              (e-chat--render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nsecond chunk")))
              (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
              (let ((new-tail (cdr (e-chat--running-status-bounds)))
                    (new-composer (marker-position e-chat--composer-start-marker)))
                (should new-tail)
                (should new-composer)
                (should (> new-tail old-tail))
                (should (= (point) new-composer))
                (should (= (window-point window) new-composer))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composed-progress-follows-tail-viewports-only ()
  "Composer focus follows a tailing transcript but preserves scrollback."
  (let* ((e-chat--surface-composition-enabled t)
         (history (mapconcat (lambda (number)
                               (format "history line %d" number))
                             (number-sequence 1 300)
                             "\n"))
         (buffer (e-chat-test--buffer nil "chat-composed-progress-viewport"))
         transcript-window composer-window composer)
    (unwind-protect
        (progn
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn "history" 1 2 "Earlier" history)
            (setq composer-window
                  (e-chat--surface-display-composer transcript-window t))
            (setq composer (window-buffer composer-window))
            ;; Selecting the input pane changes `current-buffer'; transcript
            ;; events still render in its owning transcript buffer.
            (set-buffer buffer)
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 10))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 11))
            (e-chat--render-event
             (e-events-make :type 'reasoning-delta
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:content "first progress")))
            (e-chat--cancel-pending-activity-redraw)
            (e-chat--render-turn-transient
             "turn-1" (e-chat--existing-turn-record "turn-1"))
            (e-chat--show-latest-output)
            ;; The normal UI redraw does this before the next scheduled
            ;; update.  Batch ERT has no redisplay loop of its own.
            (redisplay t)
            (let ((old-tail (cdr (e-chat--running-status-bounds))))
              (should (eq (selected-window) composer-window))
              (should (eq (window-buffer (selected-window)) composer))
              (should (>= (window-end transcript-window t) old-tail))
              ;; A command that moves the paired transcript toward older
              ;; output must unpin it even when the old tail remains visible.
              ;; Model that direction at the pre/post-command boundary; the
              ;; graphical test drives the real one-line scroll command.
              (set-window-parameter
               transcript-window e-chat--output-follow-command-window-parameter
               (list :buffer buffer
                     :window-start (1+ (window-start transcript-window))))
              (cl-letf (((symbol-function 'e-chat--window-reaches-output-p)
                         (lambda (_window _tail) t)))
                (with-current-buffer composer
                  (e-chat--post-command)))
              (should-not
               (plist-get
                (e-chat--window-output-follow-state transcript-window)
                :follow))
              (should-not
               (window-parameter
                transcript-window
                e-chat--output-follow-command-window-parameter))
              (e-chat--set-window-output-follow-state transcript-window t)
              (e-chat--render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nsecond progress")))
              (e-chat--cancel-pending-activity-redraw)
              (e-chat--render-turn-transient
               "turn-1" (e-chat--existing-turn-record "turn-1"))
              (should (= (window-point transcript-window)
                         (cdr (e-chat--running-status-bounds))))
              (should (eq (selected-window) composer-window))
              (with-selected-window transcript-window
                (goto-char (point-min))
                (set-window-point transcript-window (point))
                (set-window-start transcript-window (point)))
              (redisplay t)
              (should (eq (selected-window) composer-window))
              ;; `scroll-other-window' and mouse-wheel commands can move the
              ;; paired transcript while the composer remains selected.  Its
              ;; normal post-command boundary must observe that viewport.
              ;; Batch Emacs does not maintain an independent display matrix
              ;; for the unselected transcript, so model the scrolled viewport
              ;; at the existing physical-tail predicate boundary.
              (cl-letf (((symbol-function 'e-chat--window-reaches-output-p)
                         (lambda (window _tail)
                           (not (eq window transcript-window)))))
                (with-current-buffer composer
                  (e-chat--post-command)))
              (should-not
               (plist-get
                (e-chat--window-output-follow-state transcript-window)
                :follow))
              (e-chat--render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nthird progress")))
              (e-chat--cancel-pending-activity-redraw)
              (e-chat--render-turn-transient
               "turn-1" (e-chat--existing-turn-record "turn-1"))
              (should (= (window-start transcript-window) (point-min)))
              (should (= (window-point transcript-window) (point-min)))
              (should (eq (selected-window) composer-window))
              ;; Returning the paired transcript to its output tail repins it,
              ;; even though the composer remains the selected constituent.
              (set-window-start
               transcript-window (car (e-chat--running-status-bounds)))
              (cl-letf (((symbol-function 'e-chat--window-reaches-output-p)
                         (lambda (window _tail)
                           (eq window transcript-window))))
                (with-current-buffer composer
                  (e-chat--post-command)))
              (should
               (plist-get
                (e-chat--window-output-follow-state transcript-window)
                :follow))
              (e-chat--render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nfourth progress")))
              (e-chat--cancel-pending-activity-redraw)
              (e-chat--render-turn-transient
               "turn-1" (e-chat--existing-turn-record "turn-1"))
              (should (= (window-point transcript-window)
                         (cdr (e-chat--running-status-bounds))))
              (should (eq (selected-window) composer-window))))
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))))

(ert-deftest e-chat-test-pinned-short-transcript-aligns-output-bottom ()
  "A short composed transcript uses window-local space above pinned output."
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
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
                  (e-chat--surface-display-composer transcript-window t))
            (set-buffer buffer)
            (e-chat--show-latest-output transcript-window)
            (setq spacer
                  (cl-find-if
                   (lambda (overlay)
                     (eq (overlay-get
                          overlay e-chat--output-bottom-spacer-property)
                         transcript-window))
                   (overlays-at (point-min))))
            (should (overlayp spacer))
            (should (eq (overlay-get spacer 'window) transcript-window))
            (should (> (length (overlay-get spacer 'before-string)) 0))
            (should (plist-get
                     (e-chat--window-output-follow-state transcript-window)
                     :follow))
            (e-chat--set-window-output-follow-state transcript-window nil)
            (should-not (overlay-buffer spacer))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-surface-activation-survives-late-window-restore ()
  "One deferred surface activation wins a host restoring stale scrollback."
  (let* ((e-chat--surface-composition-enabled t)
         (history (mapconcat (lambda (number)
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
                  (e-chat--surface-display-composer transcript-window t))
            (set-buffer buffer)
            (let ((stale-point (point-min))
                  (tail (point-max))
                  (surface (cons buffer transcript-window)))
              (set-window-point transcript-window stale-point)
              (set-window-start transcript-window stale-point)
              (e-chat--set-window-output-follow-state transcript-window nil)
              (e-chat--activate-surface surface)
              (should (e-chat-test--live-work-handle-p
                       e-chat--surface-activation-handle))
              (let ((job (car (e-chat-test--pending-ui-work
                               'surface-activation transcript-window))))
                (should job)
                (should (eq (plist-get job :key) transcript-window))
                (should (equal (plist-get job :generation)
                               e-chat--surface-activation-generation)))
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
      (set-frame-parameter nil e-chat--selected-surface-frame-parameter nil)
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-selection-activates-transcript-once ()
  "Entering a composer tails its transcript, but staying there preserves scrollback."
  (let* ((e-chat--surface-composition-enabled t)
         (history (mapconcat (lambda (number)
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
                  (e-chat--surface-display-composer transcript-window t))
            (set-buffer buffer)
            (let ((surface (cons buffer transcript-window))
                  (tail (point-max)))
              (set-window-point transcript-window (point-min))
              (set-window-start transcript-window (point-min))
              (e-chat--set-window-output-follow-state transcript-window nil)
              (set-frame-parameter nil e-chat--selected-surface-frame-parameter nil)
              (e-chat--activate-selected-surface-on-selection)
              (should (equal (e-chat--selected-chat-surface) surface))
              (should (= (window-point transcript-window) tail))
              (should (eq (selected-window) composer-window))
              (when (e-work-handle-p e-chat--surface-activation-handle)
                (e-ui-work-cancel e-chat--surface-activation-handle)
                (setq e-chat--surface-activation-handle nil))
              (set-window-point transcript-window (point-min))
              (set-window-start transcript-window (point-min))
              (e-chat--set-window-output-follow-state transcript-window nil)
              (e-chat--activate-selected-surface-on-selection)
              (should (= (window-point transcript-window) (point-min)))
              (should-not e-chat--surface-activation-handle))))
      (set-frame-parameter nil e-chat--selected-surface-frame-parameter nil)
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-entering-surface-pair-focuses-composer ()
  "Entering a chat pair focuses input without breaking transcript navigation."
  (let* ((e-chat--surface-composition-enabled t)
         (configuration (current-window-configuration))
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
                  (e-chat--surface-display-composer transcript-window)))
          (should (eq (window-atom-root transcript-window)
                      (window-atom-root composer-window)))
          ;; Entering through the transcript routes input to the composer.
          (select-window external-window)
          (set-frame-parameter nil e-chat--selected-surface-frame-parameter nil)
          (select-window transcript-window)
          (e-chat--activate-selected-surface-on-selection)
          (should (eq (selected-window) composer-window))
          ;; Once inside the surface, selecting the transcript is explicit
          ;; response navigation and must not be redirected back to input.
          (select-window transcript-window)
          (e-chat--activate-selected-surface-on-selection)
          (should (eq (selected-window) transcript-window)))
      (set-frame-parameter nil e-chat--selected-surface-frame-parameter nil)
      (set-window-configuration configuration)
      (when (buffer-live-p external-buffer)
        (kill-buffer external-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-activity-rerender-does-not-delete-whole-status ()
  "Streamed activity redraws update changed status text without full deletion."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-status-minimal-delete")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11))
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:content "first chunk")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((bounds (e-chat--running-status-bounds))
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
              (e-chat--render-event
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

(ert-deftest e-chat-test-progress-rerender-strips-composer-presentation-properties ()
  "Progress redraws do not leak transcript presentation into composer text."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-composer-properties")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (should (e-chat--composer-active-p))
          (goto-char (point-max))
          (let ((start (point)))
            (insert "follow-up draft")
            (add-text-properties
             start
             (point)
             '(font-lock-face e-chat-user-face
               e-chat-block-id "leaked-block"
               e-chat-turn-id "leaked-turn"))
            (e-chat--advance-progress-indicator)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (goto-char e-chat--composer-start-marker)
            (search-forward "follow-up draft")
            (let ((position (match-beginning 0)))
              (should-not (get-text-property position 'font-lock-face))
              (should-not (get-text-property position 'e-chat-block-id))
              (should-not (get-text-property position 'e-chat-turn-id)))
            (should (equal (e-chat--composer-text) "follow-up draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-context-reference ()
  "Progress redraws keep inline context reference properties in the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-context-reference")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (goto-char (point-max))
          (insert "Review ")
          (let ((reference
                 (e-chat--insert-context-reference
                  '(:id "ref-1"
                    :uri "buffer://source"
                    :label "source:2"
                    :text "two"
                    :start-line 2
                    :end-line 2
                    :point-line 2))))
            (insert " before replying.")
            (e-chat--advance-progress-indicator)
            (should (e-chat--composer-active-p))
            (let ((document (e-chat--composer-document)))
              (should (equal (plist-get document :text)
                             "Review <reference id=\"ref-1\" label=\"source:2\"> before replying."))
              (should (equal (plist-get document :references)
                             (list reference))))
            (goto-char e-chat--composer-start-marker)
            (search-forward "@[source:2]")
            (should (equal (get-text-property (match-beginning 0)
                                              'e-chat-context-reference)
                           reference))))
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
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "Initial prompt")
          (e-chat-submit)
          (should (e-chat--composer-active-p))
          (should (equal (e-chat--composer-text) ""))
          (goto-char (point-max))
          (insert "follow-up draft")
          (should (equal (e-chat--composer-text) "follow-up draft"))
          (e-chat--advance-progress-indicator)
          (should (equal (e-chat--composer-text) "follow-up draft")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
          (goto-char (point-max))
          (insert "follow-up draft")
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (should (e-chat--composer-active-p))
          (should (equal (e-chat--composer-text) "follow-up draft"))
          (e-chat--render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:reason stop)))
          (should (e-chat--composer-active-p))
          (should (equal (e-chat--composer-text) "follow-up draft")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-turn-finished-does-not-read-private-transcript ()
  "Turn finished does not recover output from the private transcript."
  (let ((buffer (e-chat-test--buffer nil "chat-missed-final")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-session-append-message
           (e-harness-sessions e-chat-harness)
           e-chat-session-id
           '(:role assistant
             :content "Recovered final answer."
             :turn-id "turn-1"))
          (e-chat--render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:reason stop)))
          (should-not (string-match-p "Recovered final answer"
                                      (buffer-string)))
          (should-not (plist-get
                       (gethash "turn-1" e-chat--turn-registry)
                       :final-rendered)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-activity-schedules-deferred-redraw ()
  "Running turn activity coalesces near-future redraw work."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-redraw"))
        (redraws 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--render-turn-transient)
                     (lambda (&rest _args)
                       (setq redraws (1+ redraws)))))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 10))
            (setq redraws 0)
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:type tool-call
                                        :id "call-1"
                                        :name "read"
                                        :arguments (:uri "file://x"))))
            (let ((handle e-chat--pending-activity-redraw-handle))
              (should (e-chat-test--live-work-handle-p handle))
              (let ((job (car (e-chat-test--pending-ui-work
                               'activity-redraw "turn-1"))))
                (should job)
                (should (equal (plist-get job :key) "turn-1")))
              (should (= redraws 0))
              (e-chat--render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "thinking")))
              (should (eq e-chat--pending-activity-redraw-handle handle))
              (should (= redraws 0))
              (e-chat--run-pending-activity-redraw)
              (should (= redraws 1))
              (should-not e-chat--pending-activity-redraw-handle)
              (should-not (e-chat-test--pending-ui-work
                           'activity-redraw "turn-1")))))
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

(ert-deftest e-chat-test-hidden-buffer-defers-activity-redraw ()
  "A chat buffer shown in no window withholds its activity redraw."
  (let ((buffer (e-chat-test--buffer nil "chat-hidden-redraw"))
        (redraws 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq e-chat--assume-redraw-visible nil)
          (cl-letf (((symbol-function 'e-chat--render-turn-transient)
                     (lambda (&rest _args)
                       (setq redraws (1+ redraws)))))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 10))
            (setq redraws 0)
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:type tool-call
                                        :id "call-1"
                                        :name "read"
                                        :arguments (:uri "file://x"))))
            ;; Hidden: no work scheduled, redraw remembered for later.
            (should-not e-chat--pending-activity-redraw-handle)
            (should (equal (car e-chat--deferred-activity-redraw) "turn-1"))
            (should (= redraws 0))
            ;; Becoming visible flushes the withheld redraw.
            (setq e-chat--assume-redraw-visible t)
            (e-chat--flush-deferred-activity-redraws)
            (should-not e-chat--deferred-activity-redraw)
            (should (e-chat-test--live-work-handle-p
                     e-chat--pending-activity-redraw-handle))
            (e-chat--run-pending-activity-redraw)
            (should (= redraws 1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-active-minibuffer-defers-activity-redraw ()
  "An active minibuffer withholds chat repainting until it exits."
  (let ((buffer (e-chat-test--buffer nil "chat-minibuffer-redraw"))
        (minibuffer-active t)
        (redraws 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq e-chat--assume-redraw-visible t)
          (cl-letf (((symbol-function 'active-minibuffer-window)
                     (lambda () (and minibuffer-active (selected-window))))
                    ((symbol-function 'e-chat--render-progress-indicator)
                     (lambda (&rest _args)
                       (setq redraws (1+ redraws)))))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 10))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat--cancel-pending-activity-redraw)
            (setq redraws 0)
            (e-chat--advance-progress-indicator)
            (should-not e-chat--pending-activity-redraw-handle)
            (should (equal e-chat--deferred-activity-redraw
                           '("turn-1" . progress)))
            (should (= redraws 0))
            (setq minibuffer-active nil)
            (e-chat--flush-deferred-activity-redraws)
            (should-not e-chat--deferred-activity-redraw)
            (should (e-chat-test--live-work-handle-p
                     e-chat--pending-activity-redraw-handle))
            (e-chat--run-pending-activity-redraw)
            (should (= redraws 1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-ui-work-diagnostics-track-pending-work ()
  "Opt-in UI work diagnostics show grouped pending work in the header."
  (let ((buffer (e-chat-test--buffer nil "chat-ui-work-diagnostics"))
        (e-chat-ui-work-diagnostics t))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--set-status "idle")
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
                                    (e-chat--refresh-ui-work-diagnostics))))
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
                                     (e-chat--refresh-ui-work-diagnostics)))))
            (e-chat--refresh-ui-work-diagnostics)
            (should (string-match-p "ui 2" header-line-format))
            (should (string-match-p "activity-redraw:1" header-line-format))
            (should (string-match-p
                     "markdown-presentation:1" header-line-format))
            (e-ui-work-cancel first)
            (e-chat--refresh-ui-work-diagnostics)
            (should (string-match-p "ui 1" header-line-format))
            (should-not (string-match-p
                         "markdown-presentation:1" header-line-format))
            (e-ui-work-cancel second)
            (e-chat--refresh-ui-work-diagnostics)
            (should-not (string-match-p "ui" header-line-format))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-stale-activity-redraw-generation-preserves-newer-job ()
  "A stale deferred redraw callback cannot clear a newer pending redraw."
  (let ((buffer (e-chat-test--buffer nil "chat-stale-activity-redraw"))
        (redraws 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq e-chat--activity-redraw-generation 2)
          (setq e-chat--pending-activity-redraw-turn-id "turn-new")
          (setq e-chat--pending-activity-redraw-kind 'activity)
          (setq e-chat--pending-activity-redraw-generation 2)
          (cl-letf (((symbol-function 'e-chat--render-turn-transient)
                     (lambda (&rest _args)
                       (setq redraws (1+ redraws)))))
            (e-chat--run-pending-activity-redraw 1)
            (should (= redraws 0))
            (should (equal e-chat--pending-activity-redraw-turn-id
                           "turn-new"))
            (should (equal e-chat--pending-activity-redraw-generation 2))))
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
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "read")))
          (e-dev-profile-start)
          (e-chat--run-pending-activity-redraw)
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "chat.activity-redraw" aggregates nil nil #'equal))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory profile-directory t))))

(ert-deftest e-chat-test-deferred-activity-redraw-preserves-scrollback-focus ()
  "Deferred activity redraw keeps point and window focus in scrollback."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-scroll-focus"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-2"
                            :created-at 20))
            (e-chat--render-event
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
            (let ((before-point (point))
                  (before-window-point (window-point window))
                  (before-window-start (window-start window)))
              (e-chat--run-pending-activity-redraw)
              (should (= (point) before-point))
              (should (= (window-point window) before-window-point))
              (should (= (window-start window) before-window-start))
              (should (string-match-p "1 tool call" (buffer-string))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-frame-schedules-deferred-redraw ()
  "Advancing the assistant progress indicator coalesces repaint work."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-redraw"))
        (redraws 0))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (cl-letf (((symbol-function 'e-chat--render-progress-indicator)
                     (lambda (&rest _args)
                       (setq redraws (1+ redraws)))))
            (e-chat--advance-progress-indicator)
            (should (= redraws 0))
            (should (e-chat-test--live-work-handle-p
                     e-chat--pending-activity-redraw-handle))
            (should (e-chat-test--pending-ui-work
                     'activity-redraw "turn-1"))
            (e-chat--run-pending-activity-redraw)
            (should-not (e-chat-test--pending-ui-work
                         'activity-redraw "turn-1"))
            (should (= redraws 1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-redraw-updates-without-composer-rebuild ()
  "Clean progress redraws update the running status without composer churn."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-no-composer-rebuild")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (goto-char (point-max))
          (insert "follow-up draft")
          (let ((delete-composer (symbol-function 'e-chat--delete-composer))
                (insert-composer (symbol-function 'e-chat--insert-composer))
                (deletes 0)
                (inserts 0))
            (cl-letf (((symbol-function 'e-chat--delete-composer)
                       (lambda (&rest args)
                         (setq deletes (1+ deletes))
                         (apply delete-composer args)))
                      ((symbol-function 'e-chat--insert-composer)
                       (lambda (&rest args)
                         (setq inserts (1+ inserts))
                         (apply insert-composer args))))
              (e-chat--advance-progress-indicator)
              (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
            (should (= deletes 0))
            (should (= inserts 0))
            (should (e-chat--composer-active-p))
            (should (equal (e-chat--composer-text) "follow-up draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-replace-region-minimally-skips-unchanged-text ()
  "Re-rendering identical running-status text edits nothing."
  (let ((buffer (e-chat-test--buffer nil "chat-replace-region-noop")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((inhibit-read-only t)
                (changes 0))
            (goto-char (point-max))
            (let ((start (point)))
              (insert "a large activity block of text")
              (let ((end (point)))
                (add-hook 'after-change-functions
                          (lambda (&rest _) (setq changes (1+ changes)))
                          nil t)
                ;; Identical text: no buffer modification, end unchanged.
                (should (= (e-chat--replace-region-text-minimally
                            start end "a large activity block of text")
                           end))
                (should (= changes 0))
                ;; A real change still edits only the differing span.
                (should (= (e-chat--replace-region-text-minimally
                            start end "a large activity block of TEXT")
                           end))
                (should (> changes 0))
                (should (string-match-p
                         "a large activity block of TEXT"
                         (buffer-string)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-large-transient-block-throttles-redraw-delay ()
  "A large visible transient block lengthens the activity redraw delay."
  (let ((buffer (e-chat-test--buffer nil "chat-redraw-throttle"))
        (e-chat-activity-redraw-delay 0.05)
        (e-chat-activity-redraw-large-block-chars 20)
        (e-chat-activity-redraw-large-block-factor 4.0))
    (unwind-protect
        (with-current-buffer buffer
          ;; No running-status region: default delay.
          (should (= (e-chat--activity-redraw-delay) 0.05))
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (let ((start (point)))
              (insert (make-string 100 ?x))
              (setq e-chat--running-status-start-marker (copy-marker start))
              (setq e-chat--running-status-end-marker (copy-marker (point)))))
          ;; Region past the large-block threshold: delay is multiplied.
          (should (= (e-chat--activity-redraw-delay) 0.2)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-replace-region-bounded-updates-large-block ()
  "A region past the diff threshold updates through the bounded path."
  (let ((buffer (e-chat-test--buffer nil "chat-replace-region-bounded"))
        (e-chat-running-status-diff-max-chars 64))
    (unwind-protect
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (let* ((start (point))
                   (old (make-string 200 ?x))
                   (new (concat (make-string 200 ?x) "-tail")))
              (insert old)
              (let ((end (point)))
                (should (= (e-chat--replace-region-text-minimally
                            start end new)
                           (+ start (length new))))
                (should (string-match-p
                         (concat (regexp-quote new) "\\'")
                         (buffer-substring start (point-max))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-final-response-uses-visual-marker-face ()
  "Final assistant output is visually distinguished without a text label."
  (let ((buffer (e-chat-test--buffer nil "chat-final-face")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
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
          (e-chat--insert-entry
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

(ert-deftest e-chat-test-long-final-response-defers-markdown-presentation ()
  "Long assistant text appears before deferred Markdown presentation runs."
  (let ((buffer (e-chat-test--buffer nil "chat-final-markdown-deferred"))
        (e-chat-deferred-markdown-threshold-bytes 8))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--insert-entry
           "Assistant"
           "Use **bold** and `code`."
           nil
           "turn-final-md")
          (goto-char (point-min))
          (search-forward "bold")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should-not (memq 'e-chat-markdown-strong-face faces)))
          (let ((pending (e-chat-test--pending-ui-work
                          'markdown-presentation)))
            (should (= (length pending) 1))
            (should (equal (plist-get (car pending) :key)
                           e-chat--markdown-presentation-generation))
            (should (e-chat-test--live-work-handle-p
                     (plist-get (car pending) :handle))))
          (should (e-chat-test--wait-until
                   (lambda ()
                     (not (e-chat-test--pending-ui-work
                           'markdown-presentation)))))
          (goto-char (point-min))
          (search-forward "bold")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should (memq 'e-chat-markdown-strong-face faces)))
          (search-forward "code")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should (memq 'e-chat-markdown-code-face faces))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-deferred-markdown-renders-in-line-chunks ()
  "Deferred Markdown presentation honors the configured line chunk budget."
  (let ((buffer (e-chat-test--buffer nil "chat-final-markdown-chunks"))
        (e-chat-deferred-markdown-threshold-bytes 8)
        (e-chat-deferred-markdown-chunk-lines 1)
        (chunks 0))
    (unwind-protect
        (with-current-buffer buffer
          (let ((original (symbol-function 'e-chat--apply-markdown-line-faces)))
            (cl-letf (((symbol-function 'e-chat--apply-markdown-line-faces)
                       (lambda (start end)
                         (setq chunks (1+ chunks))
                         (funcall original start end))))
              (e-chat--insert-entry
               "Assistant"
               "Use **one**\nUse **two**\nUse **three**.")
              (should (e-chat-test--pending-ui-work
                       'markdown-presentation))
              (should
               (e-chat-test--wait-until
                (lambda ()
                  (not (e-chat-test--pending-ui-work
                        'markdown-presentation)))))
              (should (> chunks 1))
              (goto-char (point-min))
              (search-forward "three")
              (let ((faces (ensure-list
                            (get-text-property
                             (match-beginning 0) 'face))))
                (should (memq 'e-chat-markdown-strong-face faces))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-deferred-markdown-cancels-on-clear ()
  "Deferred Markdown callbacks do not apply to stale cleared buffers."
  (let ((buffer (e-chat-test--buffer nil "chat-markdown-cancel"))
        (e-chat-deferred-markdown-threshold-bytes 8)
        (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat--apply-assistant-markdown)
                     (lambda (&rest _args)
                       (setq calls (1+ calls)))))
            (e-chat--insert-entry
             "Assistant"
             "Use **bold** and `code`.")
            (should (e-chat-test--pending-ui-work
                     'markdown-presentation))
            (e-chat--clear)
            (accept-process-output nil 0.05)
            (should (= calls 0))
            (should-not (e-chat-test--pending-ui-work
                         'markdown-presentation))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-details-shows-intermittent-events ()
  "Details buffer shows intermittent reasoning before metadata."
  (let ((buffer (e-chat-test--buffer nil "chat-intermittent-expand")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type reasoning-delta
                                      :content "Need current buffer state.")))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (e-chat--render-event
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

(ert-deftest e-chat-test-response-navigation-starts-at-latest-from-composer ()
  "Response navigation starts at the latest turn when point is in the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-latest")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (goto-char (point-max))
          (call-interactively #'e-chat-enter-response-navigation)
          (should e-chat-response-navigation-mode)
          (should (equal (plist-get (gethash e-chat--focused-block-id
                                             e-chat--block-registry)
                                    :turn-id)
                         "turn-2"))
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph) " two")
                   (e-chat-test--focused-turn-text)))
          (should-not (string-match-p
                       (concat (regexp-quote e-chat--user-glyph) " second")
                       (e-chat-test--focused-turn-text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-focus-excludes-composer-spacer ()
  "Focused turn highlight stops before the visible composer spacer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-spacer"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (let ((e-chat--test-window-body-height 60)
                  (e-chat--test-transcript-screen-lines 4))
              (e-chat-test--render-turn "turn-1" 10 11 "first" "one"))
            (goto-char (point-max))
            (call-interactively #'e-chat-enter-response-navigation)
            (should (equal (plist-get (gethash e-chat--focused-block-id
                                               e-chat--block-registry)
                                      :turn-id)
                           "turn-1"))
            (should (<= (overlay-end e-chat--focused-turn-overlay)
                        (marker-position e-chat--transcript-end-marker)))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-starts-at-turn-under-point ()
  "Response navigation starts at the rendered turn under point."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-under-point")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (goto-char (point-min))
          (search-forward "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (should (equal (plist-get (gethash e-chat--focused-block-id
                                             e-chat--block-registry)
                                    :turn-id)
                         "turn-1"))
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph) " one")
                   (e-chat-test--focused-turn-text)))
          (should-not (string-match-p
                       (concat (regexp-quote e-chat--user-glyph) " first")
                       (e-chat-test--focused-turn-text))))
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
                   (concat (regexp-quote e-chat--user-glyph) " second")
                   (e-chat-test--focused-turn-text)))
          (should-not (string-match-p
                       (concat (regexp-quote e-chat--assistant-glyph) " two")
                       (e-chat-test--focused-turn-text)))
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "j")))
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph) " two")
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
          (should (equal e-chat--focused-turn-id "turn-2")))
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
          (should (>= (point) (marker-position e-chat--composer-start-marker))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-block-view-gg-and-g-move-within-focused-block ()
  "Block view gg/G move to the focused block content text bounds."
  (let ((buffer (e-chat-test--buffer nil "chat-block-view-goto")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one\ntwo\nthree")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (let ((bounds (e-chat--block-content-bounds
                         (e-chat--block-view-block))))
            (goto-char (+ (car bounds) 4))
            (call-interactively
             (lookup-key e-chat-block-view-mode-map (kbd "G")))
            (should (= (point) (cdr bounds)))
            (should (equal (char-before) ?e))
            (should (equal (char-after) ?\n))
            (call-interactively
             (lookup-key e-chat-block-view-mode-map (kbd "g g")))
            (should (= (point) (car bounds)))))
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
          (should (>= (point) (marker-position e-chat--composer-start-marker))))
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
          (should (>= (point) (marker-position e-chat--composer-start-marker))))
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

(ert-deftest e-chat-test-reattach-does-not-duplicate-rendered-events ()
  "Reattaching a chat buffer leaves one live subscription for the session."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (buffer (e-chat-open :harness harness :session-id "chat-reattach")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--attach-buffer buffer harness "chat-reattach")
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
          (should (= (length e-chat--block-order) 1)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-details-open-read-only-buffer ()
  "Details command opens turn metadata and tool details outside the chat buffer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-details")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "inspect" "done")
          (e-chat--record-activity-event
           "turn-1"
           '(:event-type tool-started
             :payload (:type tool-call :name "buffer-read"
                       :arguments (:buffer "*scratch*"))))
          (e-chat--record-activity-event
           "turn-1"
           '(:event-type tool-finished
             :payload (:result (:status ok :content "scratch contents"))))
          (e-chat-test--focus-block-containing "done")
          (let ((details (e-chat-response-navigation-details)))
            (with-current-buffer details
              (should (derived-mode-p 'special-mode))
              (should buffer-read-only)
              (should (string-match-p "Turn: turn-1" (buffer-string)))
              (should (string-match-p "buffer-read" (buffer-string)))
              (should (string-match-p "scratch contents" (buffer-string)))
              (should-error (insert "mutate") :type 'buffer-read-only))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))

(ert-deftest e-chat-test-running-activity-is-navigable-while-progress-active ()
  "Running activity summary blocks are navigable before the turn settles."
  (let ((buffer (e-chat-test--buffer nil "chat-running-activity-nav")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
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
          (should (e-chat--composer-active-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-response-navigation ()
  "Progress redraws keep response navigation focused on the same block."
  (let ((buffer (e-chat-test--buffer nil "chat-running-nav-preserve")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-2"
                          :created-at 20))
          (goto-char (point-min))
          (search-forward "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (let ((focused-block-id e-chat--focused-block-id)
                (focused-point (point)))
            (e-chat--advance-progress-indicator)
            (should e-chat-response-navigation-mode)
            (should (equal e-chat--focused-block-id focused-block-id))
            (should (= (point) focused-point))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-block-view ()
  "Progress redraws keep block view point inside the active block."
  (let ((buffer (e-chat-test--buffer nil "chat-running-block-preserve")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one two three")
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-2"
                          :created-at 20))
          (goto-char (point-min))
          (search-forward "one two")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (let* ((block-id e-chat--block-view-block-id)
                 (bounds (e-chat--block-content-bounds
                          (e-chat--block-view-block)))
                 (target-point (+ (car bounds) 4)))
            (goto-char target-point)
            (e-chat--advance-progress-indicator)
            (should e-chat-block-view-mode)
            (should (equal e-chat--block-view-block-id block-id))
            (should (= (point) target-point))
            (let ((updated-bounds (e-chat--block-content-bounds
                                   (e-chat--block-view-block))))
              (should (<= (car updated-bounds) (point)))
              (should (<= (point) (cdr updated-bounds))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-running-tool-list ()
  "Progress redraws keep a running activity tool list and selected item."
  (let ((buffer (e-chat-test--buffer nil "chat-running-tool-list-preserve")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (dolist (tool '(("call-1" "read")
                          ("call-2" "write")))
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload (list :type 'tool-call
                                           :id (nth 0 tool)
                                           :name (nth 1 tool)))))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (goto-char (point-min))
          (search-forward "2 tool calls")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-tool-list-mode)
          (call-interactively #'e-chat-tool-list-next)
          (should (= e-chat--tool-list-index 1))
          (e-chat--advance-progress-indicator)
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (should e-chat-tool-list-mode)
          (should (= e-chat--tool-list-index 1))
          (should (string-match-p "write"
                                  (buffer-substring-no-properties
                                   (overlay-start e-chat--tool-list-overlay)
                                   (overlay-end e-chat--tool-list-overlay)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-finalizing-tool-activity-removes-dead-blocks ()
  "Completed tool activity does not leave zero-width blocks in navigation."
  (let ((buffer (e-chat-test--buffer nil "chat-finalize-tool-activity")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload (list :message
                                         (list :role 'user
                                               :content "inspect prompt"))))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload (list :type 'tool-call
                                         :id "call-1"
                                         :name "read")))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload (list :message
                                         (list :role 'assistant
                                               :content "final answer"))))
          (e-chat--render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12))
          (should (cl-every #'e-chat--live-block-record e-chat--block-order))
          (should-not (cl-some
                       (lambda (block-id)
                         (eq (plist-get (gethash block-id e-chat--block-registry)
                                        :kind)
                             'activity))
                       e-chat--block-order)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-skips-dead-activity-to-user-block ()
  "Block navigation from a final response can reach and enter the user prompt."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-skip-dead-activity")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload (list :message
                                         (list :role 'user
                                               :content "copy this prompt"))))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload (list :type 'tool-call
                                         :id "call-1"
                                         :name "read")))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload (list :message
                                         (list :role 'assistant
                                               :content "final answer"))))
          (e-chat-test--focus-block-containing "final answer")
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "k")))
          (should (eq (plist-get (e-chat-test--focused-block) :kind) 'user))
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-block-view-mode)
          (should (equal (e-chat--block-action-text (e-chat--block-view-block))
                         "copy this prompt")))
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
             (equal e-chat--focused-turn-id
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
              (e-chat--clear)
              (e-chat--render-session)
              (should (string-match-p "board prompt" (buffer-string)))
              (should (string-match-p "board answer" (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-replayed-failed-turn-expands-inline ()
  "Replayed turn-failed activity renders as a compact expandable block."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-failed-replay")
          (e-session-append-message
           store "chat-failed-replay"
           '(:role user :content "too much context" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-failed-replay" "turn-1" 'turn-failed
           '(:error "OpenAI request failed: (error http 400)"
             :details (:type "response.failed"
                       :response
                       (:error
                        (:code "context_length_exceeded"
                         :message
                         "Your input exceeds the context window.")))))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-failed-replay")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-failed-replay"))
          (with-current-buffer buffer
            (let ((content (buffer-string)))
              (should (string-match-p "too much context" content))
              (should (string-match-p
                       "Turn failed: OpenAI request failed: (error http 400)"
                       content))
              (should-not (string-match-p "context_length_exceeded" content)))
            (e-chat-test--focus-block-containing "Turn failed")
            (e-chat-response-navigation-activate)
            (let ((content (buffer-string))
                  (block (e-chat-test--focused-block)))
              (should e-chat-block-view-mode)
              (should-not e-chat-response-navigation-mode)
              (should (e-chat--block-details-visible-p block))
              (should (<= (marker-position (plist-get block :details-start-marker))
                          (point)))
              (should (<= (point)
                          (marker-position (plist-get block :details-end-marker))))
              (when (e-chat--composer-active-p)
                (should (< (point)
                           (marker-position e-chat--composer-start-marker))))
              (should-not (get-buffer e-chat-details-buffer-name))
              (should (string-match-p "context_length_exceeded" content)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))

(ert-deftest e-chat-test-replayed-failed-provider-start-settles-thinking ()
  "Replay settles provider-started plus turn-failed into a failed thought."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-failed-thinking-replay")
          (e-session-append-message
           store "chat-failed-thinking-replay"
           '(:role user :content "fail" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-failed-thinking-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-failed-thinking-replay" "turn-1"
           'provider-request-started
           '(:status started))
          (e-session-append-activity-event
           store "chat-failed-thinking-replay" "turn-1" 'turn-failed
           '(:error "provider failed"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-failed-thinking-replay")
          (setq buffer (e-chat-open
                        :harness harness
                        :session-id "chat-failed-thinking-replay"))
          (with-current-buffer buffer
            (let* ((record (e-chat--existing-turn-record "turn-1"))
                   (round (car (plist-get record :activity-records))))
              (should (equal (plist-get round :status) 'failed))
              (should (string-match-p
                       "Thought failed after [0-9]+min [0-9]+sec"
                       (e-chat--activity-expanded-text record))))
            (let ((content (buffer-string)))
              (should (string-match-p "Turn failed: provider failed" content))
              (should-not (string-match-p "Thinking\\.\\.\\." content)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-uses-bottom-spacer-when-buffer-is-visible ()
  "Displayed chat buffers keep the composer visually near the window bottom."
  (let ((buffer (e-chat-test--buffer nil "chat-bottom"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (let ((e-chat--test-window-body-height 12))
              (e-chat--refresh-composer-position))
            (should (markerp e-chat--composer-spacer-marker))
            (should (marker-position e-chat--composer-spacer-marker))
            (should (< (marker-position e-chat--composer-spacer-marker)
                       (marker-position e-chat--composer-start-marker)))
            (should (string-match-p "\n\n\n"
                                    (buffer-substring-no-properties
                                     e-chat--composer-spacer-marker
                                     e-chat--composer-start-marker)))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-spacer-shrinks-for-multiline-input ()
  "Growing composer input reduces the protected bottom spacer."
  (let ((buffer (e-chat-test--buffer nil "chat-bottom-multiline"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (let ((e-chat--test-window-body-height 12)
                  (e-chat--test-transcript-screen-lines 4))
              (e-chat--refresh-composer-position))
            (let ((single-line-spacer
                   (count-lines e-chat--composer-spacer-marker
                                e-chat--transcript-end-marker)))
              (goto-char (point-max))
              (insert "one\ntwo\nthree")
              (let ((e-chat--test-window-body-height 12)
                    (e-chat--test-transcript-screen-lines 4))
                (e-chat--refresh-composer-position))
              (let ((multiline-spacer
                     (count-lines e-chat--composer-spacer-marker
                                  e-chat--transcript-end-marker)))
                (should (< multiline-spacer single-line-spacer))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-layout-cache-skips-repeated-line-counts ()
  "Pure composer refreshes reuse cached spacer geometry."
  (let ((buffer (e-chat-test--buffer nil "chat-bottom-cache"))
        (transcript-line-counts 0)
        (composer-line-counts 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq e-chat--composer-layout-cache nil)
          (cl-letf (((symbol-function 'e-chat--transcript-screen-lines)
                     (lambda (&optional _limit)
                       (setq transcript-line-counts
                             (1+ transcript-line-counts))
                       4))
                    ((symbol-function 'e-chat--screen-lines)
                     (lambda (&rest _args)
                       (setq composer-line-counts
                             (1+ composer-line-counts))
                       2)))
            (let ((e-chat--test-window-body-height 12))
              (e-chat--refresh-composer-position)
              (e-chat--refresh-composer-position)
              (should (= transcript-line-counts 1))
              (should (= composer-line-counts 1))
              (e-chat--insert-entry "System" "transcript changed" t)
              (should (= transcript-line-counts 2))
              (should (= composer-line-counts 2))
              (e-chat--refresh-composer-position)
              (should (= transcript-line-counts 2))
              (should (= composer-line-counts 2)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-spacer-line-count-is-window-bounded ()
  "Positioning the composer scans at most a window of transcript, not all of it."
  (let ((buffer (e-chat-test--buffer nil "chat-screen-bound"))
        (window nil)
        (max-region-lines 0))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (should (window-live-p (e-chat--visible-window)))
            ;; Build a tall, non-wrapping transcript.
            (dotimes (i 200)
              (e-chat--insert-entry "System" (format "line %d" i) t))
            (cl-letf* ((orig (symbol-function 'count-screen-lines))
                       ((symbol-function 'count-screen-lines)
                        (lambda (beg end &rest args)
                          (setq max-region-lines
                                (max max-region-lines (count-lines beg end)))
                          (apply orig beg end args))))
              (let ((e-chat--test-window-body-height 12))
                (e-chat--refresh-composer-position)))
            ;; The transcript is 200 lines tall; the spacer math must not walk
            ;; more than a window's worth of it.
            (should (<= max-region-lines 13))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-transcript-screen-lines-cap-matches-exact ()
  "A short transcript counts identically with or without a line cap."
  (let ((buffer (e-chat-test--buffer nil "chat-screen-exact"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (should (window-live-p (e-chat--visible-window)))
            (e-chat--insert-entry "System" "one" t)
            (e-chat--insert-entry "System" "two" t)
            (e-chat--insert-entry "System" "three" t)
            (goto-char (point-max))
            (should (= (e-chat--transcript-screen-lines)
                       (e-chat--transcript-screen-lines 200)))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-skips-spacer-when-wrapped-content-fills-window ()
  "Wrapped long transcript content does not create a middle-of-buffer spacer."
  (let ((buffer (e-chat-test--buffer nil "chat-long"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat--insert-entry
             "Assistant"
             (make-string 400 ?x)
             t)
            (let ((e-chat--test-window-body-height 12)
                  (e-chat--test-transcript-screen-lines 20))
              (e-chat--refresh-composer-position))
            (should (not (string-match-p
                          "\n\n\n"
                          (buffer-substring-no-properties
                           e-chat--composer-spacer-marker
                           e-chat--transcript-end-marker))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-window-change-refreshes-visible-composer-spacer ()
  "Window changes recalculate the composer spacer for visible chat buffers."
  (let ((buffer (e-chat-test--buffer nil "chat-resize"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (let ((e-chat--test-window-body-height 18))
              (e-chat--refresh-composer-position))
            (let ((before (count-lines e-chat--composer-spacer-marker
                                       e-chat--transcript-end-marker)))
              (let ((e-chat--test-window-body-height 8))
                (e-chat--refresh-visible-composers))
              (let ((after (count-lines e-chat--composer-spacer-marker
                                        e-chat--transcript-end-marker)))
                (should (< after before))))))
      (when (window-live-p window)
        (delete-window window))
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
          (with-current-buffer buffer
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
            (should (string-match-p "stale prompt" (buffer-string)))
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

(ert-deftest e-chat-test-reload-buffers-preserves-composer-draft ()
  "Reloading chat buffers keeps unsent composer draft text."
  (let* ((harness (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
         (buffer (e-chat-open :harness harness :session-id "chat-reload-draft")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "draft before reload")
            (should (equal (e-chat--composer-text) "draft before reload")))
          (e-chat-test--with-empty-harness-registry
            (let ((e-chat-default-harness-id :missing-chat))
              (should (>= (e-chat-reload-buffers) 1))))
          (with-current-buffer buffer
            (should (eq e-chat-harness harness))
            (should (e-chat--composer-active-p))
            (should (equal (e-chat--composer-text) "draft before reload"))))
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
                         "e-harness--turn-options"
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
        (let ((candidates (e-chat--session-candidates)))
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
  "Worker and pre-board sessions never appear among chat session candidates.
Subagents and task-queue sessions are available through their own surfaces; the
switch, resume, active-sessions, and overview surfaces list only root chats."
  (let* ((store (e-session-store-create))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions store))))
    (e-chat-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :chat-alpha))
        (e-chat-test--register-chat-instance
         :chat-alpha "Alpha Target" harness t)
        (e-chat-test--create-session store :id "top-level"
                          :metadata '(:name "Top Level"))
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
        (should-not
         (e-chat--harness-session-active-turn-p harness "pre-board"))
        (let ((ids (mapcar (lambda (candidate)
                             (plist-get candidate :session-id))
                           (e-chat--session-candidates))))
          (should (member "top-level" ids))
          (should-not (member "child-by-parent" ids))
          (should-not (member "child-by-role" ids))
          (should-not (member "queued-task" ids))
          (should-not (member "pre-board" ids)))))))

(ert-deftest e-chat-test-session-candidates-exclude-indexed-worker-sessions ()
  "Resume candidates classify unloaded sessions from persisted metadata."
  (let* ((directory (make-temp-file "e-chat-index-candidates-" t))
         (writer (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-chat-test--create-session writer :id "top-level"
                                       :metadata '(:name "Top Level"))
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
                                 (e-chat--session-candidates))
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
                           (e-chat--session-candidates))))
          (should (equal ids
                         '("fresh-session" "middle-session"
                           "stale-session"))))))))

(ert-deftest e-chat-test-resume-preview-state-renders-selected-session ()
  "Resume completion preview renders the highlighted session in a preview buffer."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (origin (get-buffer-create "chat-resume-preview-origin")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "preview-me")
          (e-session-append-message
           store "preview-me" '(:id "msg-1" :role user :content "preview hello"))
          (let* ((sessions (e-harness-session-list harness))
                 (labels (mapcar #'e-chat--session-choice-label sessions))
                 (state (e-chat--resume-preview-state harness sessions labels)))
            (switch-to-buffer origin)
            (funcall state 'preview (car labels))
            (let ((preview (get-buffer e-chat--resume-preview-buffer-name)))
              (should preview)
              (should (eq (window-buffer (selected-window)) preview))
              (with-current-buffer preview
                (should (equal e-chat-session-id "preview-me"))
                (should (string-match-p "preview hello" (buffer-string)))))
            (funcall state 'exit nil)
            (should (eq (window-buffer (selected-window)) origin))
            (should-not (get-buffer e-chat--resume-preview-buffer-name))))
      (e-chat-test--kill-chat-buffers)
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest e-chat-test-resume-preview-renders-only-tail-messages ()
  "Resume previews render a small transcript tail for responsive selection."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (origin (get-buffer-create "chat-resume-preview-tail-origin"))
         (e-chat-resume-preview-message-limit 2))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "preview-tail"
                            :metadata '(:name "Tail preview"))
          (dotimes (index 6)
            (e-session-append-message
             store
             "preview-tail"
             (list :id (format "msg-%d" index)
                   :role (if (cl-evenp index) 'user 'assistant)
                   :content (format "preview message %d" index))))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "preview-tail")
          (let* ((sessions (e-harness-session-list harness))
                 (labels (mapcar #'e-chat--session-choice-label sessions))
                 (state (e-chat--resume-preview-state harness sessions labels)))
            (switch-to-buffer origin)
            (funcall state 'preview (car labels))
            (let ((preview (get-buffer e-chat--resume-preview-buffer-name)))
              (should preview)
              (with-current-buffer preview
                (let ((text (buffer-string)))
                  (should-not (string-match-p "preview message 0" text))
                  (should-not (string-match-p "preview message 3" text))
                  (should (string-match-p "preview message 4" text))
                  (should (string-match-p "preview message 5" text)))))
            (funcall state 'exit nil)))
      (e-chat-test--kill-chat-buffers)
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

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
              (let ((preview (e-chat--render-resume-preview harness session)))
                (should-not loaded)
                (with-current-buffer preview
                  (let ((text (buffer-string)))
                    (should buffer-read-only)
                    (should (equal e-chat-session-id "indexed-preview"))
                    (should-not (string-match-p
                                 (regexp-quote e-chat--composer-glyph)
                                 text))
                    (should (string-match-p "Indexed preview" text))
                    (should (string-match-p "indexed preview hello" text)))))
              (let ((preview (e-chat--render-resume-preview harness session)))
                (should-not loaded)
                (with-current-buffer preview
                  (let ((text (buffer-string)))
                    (should buffer-read-only)
                    (should (equal e-chat-session-id "indexed-preview"))
                    (should-not (string-match-p
                                 (regexp-quote e-chat--composer-glyph)
                                 text))
                    (should (string-match-p "Indexed preview" text))))))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

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
               (labels (mapcar #'e-chat--session-choice-label sessions)))
          (should (equal (e-chat--read-session-choice harness sessions)
                         (car labels)))
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
                  (e-chat-overview--render harness)
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
            (e-chat-overview--render harness)
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
            (e-chat-overview--render harness)
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
            (e-chat-overview--render harness)
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
            (e-chat-overview--render harness)
            (goto-char (point-min))
            (should (equal (e-chat-overview--session-id-at-point) "newer"))
            (e-chat-overview-next-session)
            (should (equal (e-chat-overview--session-id-at-point) "older"))
            (with-current-buffer e-chat--resume-preview-buffer-name
              (should (string-match-p "older prompt" (buffer-string))))
            (e-chat-overview-previous-session)
            (should (equal (e-chat-overview--session-id-at-point) "newer"))
            (with-current-buffer e-chat--resume-preview-buffer-name
              (should (string-match-p "newer prompt" (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when-let ((preview (get-buffer e-chat--resume-preview-buffer-name)))
        (kill-buffer preview)))))

(ert-deftest e-chat-test-overview-open-session-marks-session-read ()
  "Opening from overview records the selected session read marker."
  (let* ((directory (make-temp-file "e-chat-overview-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (e-chat--read-markers (make-hash-table :test #'eq)))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "read-me"
                            :metadata '(:name "Read Me"))
          (e-session-append-message
           store "read-me"
           '(:id "assistant-read" :role assistant :content "answer"))
          (let ((buffer (get-buffer-create "*e-chat-overview-test*")))
            (unwind-protect
                (with-current-buffer buffer
                  (e-chat-overview-mode)
                  (e-chat-overview--render harness)
                  (goto-char (point-min))
                  (let ((chat-buffer (e-chat-overview-open-session)))
                    (with-current-buffer chat-buffer
                      (should (equal e-chat-session-id "read-me")))
                         (should (equal
                                  (e-chat-overview--read-marker
                                   "read-me" harness)
                                  "assistant-read"))
                    (should-not
                     (plist-member
                      (plist-get (e-session-get store "read-me") :metadata)
                      :e-chat-read-markers))
                    (e-chat-overview--render harness)
                    (should-not (string-match-p
                                 "! Read Me"
                                 (buffer-string)))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-chat-test-attach-buffer-ignores-persisted-read-marker-plist ()
  "Attaching ignores stale read markers replayed as plist metadata."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer (get-buffer-create "*e-chat-read-marker-attach-test*"))
         (e-chat--read-markers (make-hash-table :test #'eq)))
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
            (e-chat--attach-buffer
             buffer harness "read-marker-attach" :chat-default)
            (should (equal e-chat-session-id "read-marker-attach"))
            (should-not
             (e-chat-overview--read-marker
              "read-marker-attach" harness :chat-default))
            (e-chat-overview--mark-session-read
             harness "read-marker-attach" :chat-default)
            (should (equal
                     (e-chat-overview--read-marker
                      "read-marker-attach" harness :chat-default)
                     "assistant-read"))))
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
              (e-chat--attach-buffer buffer harness "rooted" nil)))
          (should (= writes 0)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

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
              (e-chat-overview--render)
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
              (e-chat-overview--render)
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
            (should (e-chat-overview--visible-window))
            (should (eq (window-buffer (selected-window)) opened-buffer))
            (e-chat-sidebar-toggle)
            (should-not (buffer-live-p opened-buffer))
            (should-not (e-chat-overview--visible-window))))
      (when (buffer-live-p opened-buffer)
        (kill-buffer opened-buffer)))))

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
                           (buffer-substring-no-properties
                            e-chat--composer-start-marker
                            (point-max)))))))))
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
            (e-chat--attach-buffer hidden-duplicate harness "duplicate-session" nil)
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
                               (buffer-substring-no-properties
                                e-chat--composer-start-marker
                                (point-max)))))
                    (with-current-buffer hidden-duplicate
                      (should (string-empty-p
                               (buffer-substring-no-properties
                                e-chat--composer-start-marker
                                (point-max)))))))))))
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
          (e-chat--attach-buffer buffer harness "helper-session" nil)
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
          (e-chat--attach-buffer hidden-duplicate harness "dedupe-session" nil)
          (should (buffer-live-p hidden-duplicate))
          (should (eq (e-chat-open :harness harness
                                   :session-id "dedupe-session")
                      visible-buffer))
          (should-not (buffer-live-p hidden-duplicate)))
      (when (and window (window-live-p window))
        (delete-window window))
      (e-chat-test--kill-chat-buffers))))

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
              (let ((reference (e-chat--capture-context-reference-for-command)))
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
                  (should (eq (e-chat--add-context-reference-to-session
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
                           (buffer-substring-no-properties
                            e-chat--composer-start-marker
                            (point-max)))))))))
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
                           (buffer-substring-no-properties
                            e-chat--composer-start-marker
                            (point-max)))))))))
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
          (e-chat--add-context-reference-to-session
           reference
           e-chat-harness
           e-chat-session-id)
          (should-not e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (should (e-chat--point-in-composer-p))
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
          (e-chat--add-context-reference-to-session
           reference
           e-chat-harness
           e-chat-session-id)
          (should-not e-chat-response-navigation-mode)
          (should-not e-chat-block-view-mode)
          (should (e-chat--point-in-composer-p))
          (should-not (eq (key-binding (kbd "j") t)
                          #'e-chat-response-navigation-next)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
                             (buffer-substring-no-properties
                              e-chat--composer-start-marker
                              (point-max))))))))))
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
                             (buffer-substring-no-properties
                             e-chat--composer-start-marker
                             (point-max))))))))))
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
                             (buffer-substring-no-properties
                              e-chat--composer-start-marker
                              (point-max))))))))))
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
                   (e-harness-create :backend backend :sessions store)))
         (timestamps '("2026-05-22T10:00:00Z"
                       "2026-05-22T10:00:01Z"
                       "2026-05-22T10:00:02Z"
                       "2026-05-22T10:00:03Z")))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (cl-letf (((symbol-function 'e-session--timestamp)
                       (lambda (&optional _time)
                         (prog1 (car timestamps)
                           (setq timestamps (cdr timestamps))))))
              (e-chat-test--create-session store :id "older-session"
                                :metadata '(:name "Alpha old"))
              (e-session-append-message
               store "older-session"
               '(:id "older-message" :role user :content "older prompt"))
              (e-chat-test--create-session store :id "newer-session"
                                :metadata '(:name "Zulu newest"))
              (e-session-append-message
               store "newer-session"
               '(:id "newer-message" :role user :content "newer prompt")))
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
                             (buffer-substring-no-properties
                              e-chat--composer-start-marker
                              (point-max))))))))))
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
    (should (equal (e-chat--model-context-token-limit model)
                   353400)))
  (should (equal (e-chat--model-context-token-limit "gpt-5.5")
                 258400))
  (should
   (equal
    (e-chat--format-mode-line-status "gpt-5.5" "high" 18000 400000 t)
    "e-chat gpt-5.5/high ~5% (~18k/400k tok)"))
  (should
   (equal
    (e-chat--format-mode-line-status "gpt-5.5" "high" 40000 258400 nil)
    "e-chat gpt-5.5/high 15% (40k/258k tok)")))

(ert-deftest e-chat-test-mode-line-window-uses-configured-limit ()
  "The mode line ignores a larger provider maximum when e has a lower limit."
  (let ((e-context-budget-model-token-limits
         '(("gpt-5.6-sol" . 353400)))
        (provider-calls 0))
    (cl-letf (((symbol-function 'e-anthropic-context-window)
               (lambda (_model)
                 (setq provider-calls (1+ provider-calls))
                 1050000)))
      (should (equal (e-chat--model-context-window "gpt-5.6-sol")
                     353400))
      (should (= provider-calls 0)))))

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
        (cl-letf (((symbol-function 'e-chat--model-context-window)
                   (lambda (model) (and (equal model "gpt-5.5") 100))))
          (with-current-buffer buffer
            (setq-local e-chat--assume-redraw-visible t)
            (e-session-append-message
             store
             e-chat-session-id
             '(:role user :content "context question"))
            (e-chat-test--seed-board-log-from-private-fixture
             harness e-chat-session-id)
            (e-chat--set-status "idle" t)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)
                                     :owner 'chat-mode-line-status))
            (should (string-match-p "gpt-5.5/high" mode-name))
            (should (string-match-p "~[0-9]+%" mode-name))
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
          (cl-letf (((symbol-function 'e-chat--model-context-window)
                     (lambda (model)
                       (and (equal model "gpt-5.5") 100)))
                    ((symbol-function 'e-chat--redraw-visible-p)
                     (lambda () t)))
            (e-chat--attach-buffer buffer harness "chat-mode-line-open")
            (should (equal mode-name "e-chat"))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch
               :buffer buffer
               :owner 'chat-mode-line-status))
            (should (string-match-p "gpt-5.5/high" mode-name))
            (should (string-match-p "/100 tok" mode-name))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
        (cl-letf (((symbol-function 'e-chat--model-context-window)
                   (lambda (_model) nil)))
          (with-current-buffer buffer
            (setq-local e-chat--assume-redraw-visible t)
            (e-session-append-message
             store e-chat-session-id '(:role user :content "q"))
            (e-chat--set-status "idle" t)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)
                                     :owner 'chat-mode-line-status))
            (should (string-match-p "gpt-5.5/high" mode-name))
            (should (string-match-p "/? tok" mode-name))))
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
          (setq-local e-chat--assume-redraw-visible t)
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
          (e-chat--set-status "idle" t)
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (equal e-chat--mode-line-status
                         "e-chat gpt-5.5/high 78% (203k/258k tok)")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-token-usage-before-compaction-uses-context-estimate ()
  "After compaction, stale provider usage does not hide compacted context size."
  (let* ((timestamps '("2026-05-25T10:00:00Z"
                       "2026-05-25T10:00:01Z"
                       "2026-05-25T10:00:02Z"
                       "2026-05-25T10:00:03Z"
                       "2026-05-25T10:00:04Z"))
         (store (e-session-store-create))
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
      (setq-local e-chat--assume-redraw-visible t)
      (cl-letf (((symbol-function 'e-session--timestamp)
                 (lambda (&optional _time)
                   (prog1 (car timestamps)
                     (setq timestamps (cdr timestamps))))))
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
         :first-kept-entry-id "kept"))
      (e-chat--set-status "idle" t)
      (e-ui-work-with-batch-drain
        (e-ui-work-drain-batch :buffer (current-buffer)
                               :owner 'chat-mode-line-status))
      (should (string-match-p "~[0-9]+%" mode-name))
      (should-not (string-match-p "203k/258k tok" mode-name)))))


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
            (e-chat--set-status "waiting for provider")
            (e-chat--set-status "done"))
          (should (= context-calls 0))
          (should (string-match-p "E Chat: done" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
            (e-chat--set-status "waiting for provider")
            (e-chat--set-status "done"))
          (should (= turn-option-calls 0))
          (should (string-match-p "gpt-5.5/high" header-line-format)))
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
          (e-chat--set-status "waiting for provider")
          (cl-letf (((symbol-function 'e-harness-session-title)
                     (lambda (&rest args)
                       (setq session-title-calls (1+ session-title-calls))
                       (apply original-session-title args))))
            (e-chat--set-status "waiting for provider"))
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
          (e-chat--set-status "waiting for provider")
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "chat.status" aggregates nil nil #'equal))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory profile-directory t))))

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
        (with-current-buffer buffer
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
                               :session-id "chat-status-refresh"))
         (refresh-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local e-chat--assume-redraw-visible t)
          (e-chat--invalidate-mode-line-context-estimate)
          (cl-letf (((symbol-function 'e-chat--refresh-mode-line-status)
                     (lambda (&rest _args)
                       (setq refresh-calls (1+ refresh-calls))
                       (setq-local mode-name "refreshed mode line"))))
            (e-chat--set-status "idle" t)
            (should (= refresh-calls 0))
            (should (= (length (e-ui-work-pending
                                (current-buffer)
                                :owner 'chat-mode-line-status))
                       1))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)
                                     :owner 'chat-mode-line-status)))
          (should (= refresh-calls 1))
          (should (equal mode-name "refreshed mode line")))
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
          (setq-local e-chat--assume-redraw-visible t)
          (cl-letf (((symbol-function 'e-harness-context)
                     (lambda (&rest _args)
                       (setq context-calls (1+ context-calls))
                       (error "context estimate should be skipped"))))
            (e-chat--refresh-mode-line-status t))
          (should (= context-calls 0))
          (should (equal mode-name
                         "e-chat gpt-5.5/high")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-mode-line-status-reuses-fresh-estimate-cache ()
  "Fresh mode-line estimate cache hits avoid context rebuilding."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-mode-line-cache"))
         (context-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local e-chat--assume-redraw-visible t)
          (setq-local e-chat--mode-line-context-estimate-cache
                      (cons (list :tokens 123
                                  :time (float-time)
                                  :estimate-cache-keyed t
                                  :estimate-cache-key
                                  (e-chat--mode-line-context-estimate-key))
                            nil))
          (setq-local e-chat--mode-line-context-status-cache (cons nil nil))
          (cl-letf (((symbol-function 'e-chat--model-context-window)
                     (lambda (model) (and (equal model "gpt-5.5") 1000)))
                    ((symbol-function 'e-harness-context)
                     (lambda (&rest _args)
                       (setq context-calls (1+ context-calls))
                       (error "fresh estimate cache should skip context"))))
            (e-chat--set-status "idle" t)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)
                                     :owner 'chat-mode-line-status)))
          (should (= context-calls 0))
          (should (equal e-chat--mode-line-status
                         "e-chat gpt-5.5/high ~13% (~123/1k tok)")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-mode-line-status-reuses-fresh-status-snapshot ()
  "Fresh mode-line status snapshots skip context and model-window rebuilding."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-mode-line-status-cache"))
         (context-calls 0)
         (window-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local e-chat--assume-redraw-visible t)
          (setq-local e-chat--mode-line-context-estimate-cache (cons nil nil))
          (setq-local e-chat--mode-line-context-status-cache (cons nil nil))
          (e-session-append-message
           store e-chat-session-id '(:role user :content "cached status"))
          (let ((original-context (symbol-function 'e-harness-context)))
            (cl-letf (((symbol-function 'e-chat--model-context-window)
                       (lambda (model)
                         (setq window-calls (1+ window-calls))
                         (and (equal model "gpt-5.5") 1000)))
                      ((symbol-function 'e-harness-context)
                       (lambda (&rest args)
                         (setq context-calls (1+ context-calls))
                         (apply original-context args))))
              (let ((e-chat-mode-line-context-estimate-cache-seconds 100))
                (e-chat--set-status "idle" t)
                (e-chat--set-status "ready" t)
                (e-ui-work-with-batch-drain
                  (e-ui-work-drain-batch :buffer (current-buffer)
                                         :owner 'chat-mode-line-status)))))
          (should (= context-calls 1))
          (should (= window-calls 1))
          (should (equal e-chat--mode-line-status
                         "e-chat gpt-5.5/high ~2% (~16/1k tok)")))
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
          (setq-local e-chat--assume-redraw-visible t)
          (e-session-append-activity-event
           store
           e-chat-session-id
           "turn-1"
           'token-usage
           '(:input-tokens 1200 :total-tokens 1300))
          (cl-letf (((symbol-function 'e-chat--context-token-estimate)
                     (lambda (&rest _args)
                       (error "context estimate should be skipped"))))
            (e-chat--render-event
             (e-events-make :type 'token-usage
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:input-tokens 1200
                                       :total-tokens 1300))))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (string-match-p "1.2k/258k tok" mode-name)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
          (setq-local e-chat--assume-redraw-visible t)
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
            (e-chat--render-event
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

(ert-deftest e-chat-test-token-usage-coalesces-mode-line-render-work ()
  "Repeated usage events schedule one latest-value mode-line projection."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend :sessions store
                   :default-options '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness :session-id "chat-token-status-work"))
         (refreshes 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local e-chat--assume-redraw-visible t)
          (let ((e-chat-mode-line-status-delay 0))
            (cl-letf (((symbol-function 'e-chat--refresh-mode-line-status)
                       (lambda (&rest _args) (setq refreshes (1+ refreshes)))))
              (dotimes (_ 3)
                (e-chat--render-event
                 (e-events-make :type 'token-usage
                                :session-id e-chat-session-id :turn-id "turn-1")))
              (should (= (length (e-ui-work-pending
                                  (current-buffer) :owner 'chat-mode-line-status))
                         1))
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer (current-buffer)
                                       :owner 'chat-mode-line-status))
              (should (= refreshes 1)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-compaction-finished-refreshes-context-estimate ()
  "Finished compactions immediately refresh stale context estimates."
  (let* ((timestamps '("2026-05-25T10:00:00Z"
                       "2026-05-25T10:00:01Z"
                       "2026-05-25T10:00:02Z"
                       "2026-05-25T10:00:03Z"))
         (store (e-session-store-create))
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
          (setq-local e-chat--assume-redraw-visible t)
          (cl-letf (((symbol-function 'e-session--timestamp)
                     (lambda (&optional _time)
                       (prog1 (car timestamps)
                         (setq timestamps (cdr timestamps))))))
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
            (e-chat--set-status "idle" t)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)
                                     :owner 'chat-mode-line-status))
            (should (string-match-p "~[0-9]+%" mode-name))
            (let ((before mode-name))
              (e-session-append-compaction
               store
               e-chat-session-id
               "summary"
               :first-kept-entry-id "kept")
              (e-chat--render-event
               (e-events-make :type 'compaction-finished
                              :session-id e-chat-session-id
                              :turn-id "turn-compact"
                              :payload '(:compaction-id "compaction-1"
                                         :first-kept-entry-id "kept")))
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer (current-buffer)
                                       :owner 'chat-mode-line-status))
              (should (string-match-p "~[0-9]+%" mode-name))
              (should-not (equal mode-name before)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-tool-activity-compacts-large-result-display ()
  "Chat activity keeps a bounded tool result preview."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-activity-preview")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((e-chat-tool-activity-preview-bytes 8))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"))
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:id "call-1" :name "bash")))
            (e-chat--render-event
             (e-events-make :type 'tool-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload
                            (list :tool-call '(:id "call-1" :name "bash")
                                  :result
                                  (list :tool-call-id "call-1"
                                        :name "bash"
                                        :status 'ok
                                        :content (make-string 64 ?x)
                                        :metadata '(:tmp-uri "tmp://full.txt"))))))
          (let* ((record (e-chat--existing-turn-record "turn-1"))
                 (item (car (e-chat--activity-tool-items record)))
                 (output (plist-get item :output)))
            (should (string-match-p "xxxxxxxx" output))
            (should (string-match-p "Tool result preview truncated" output))
            (should (string-match-p "tmp://full.txt" output))
            (should-not (string-match-p (make-string 32 ?x) output))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-tool-activity-uses-bounded-structured-preview ()
  "Chat activity does not force full model text for structured tool results."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-structured-preview")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((e-chat-tool-activity-preview-bytes 32))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"))
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:id "call-1" :name "structured")))
            (e-chat--render-event
             (e-events-make :type 'tool-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload
                            (list :tool-call '(:id "call-1" :name "structured")
                                  :result
                                  (list :tool-call-id "call-1"
                                        :name "structured"
                                        :status 'ok
                                        :content (list :items (number-sequence 1 100)
                                                       :body (make-string 200 ?x)))))))
          (let* ((record (e-chat--existing-turn-record "turn-1"))
                 (item (car (e-chat--activity-tool-items record)))
                 (output (plist-get item :output)))
            (should (string-match-p "Tool result preview truncated" output))
            (should-not (string-match-p (make-string 80 ?x) output))))
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
          (setq-local e-chat--assume-redraw-visible t)
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
          (e-chat--render-event
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
          (should (equal e-chat--mode-line-status
                         "e-chat gpt-5.5/high 21% (55k/258k tok)"))
          (should-not (string-match-p "token-usage" (buffer-string)))
          (should-not (string-match-p "Event:" (buffer-string))))
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
          (e-chat--render-event
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

(ert-deftest e-chat-test-composer-meta-actions-target-latest-response ()
  "Composer M-y and M-o target the latest final assistant response block."
  (unless (fboundp 'markdown-mode)
    (define-derived-mode markdown-mode text-mode "Markdown"))
  (let ((buffer (e-chat-test--buffer nil "chat-meta-actions"))
        (opened nil))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (e-chat-test--render-turn "turn-1" 10 11 "first" "old final")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "latest final")
          (goto-char e-chat--composer-start-marker)
          (call-interactively
           (lookup-key e-chat-mode-map (kbd "M-y")))
          (should (equal (current-kill 0) "latest final"))
          (setq opened
                (call-interactively
                 (lookup-key e-chat-mode-map (kbd "M-o"))))
          (should (eq (window-buffer (selected-window)) opened))
          (with-current-buffer opened
            (should (derived-mode-p 'markdown-mode))
            (should (equal (buffer-string) "latest final"))))
      (when (buffer-live-p opened)
        (kill-buffer opened))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-keymap-refresh-updates-stale-reload-bindings ()
  "Reload-time keymap refresh replaces bindings preserved by `defvar'."
  (let ((e-chat-response-navigation-mode-map (make-sparse-keymap))
        (e-chat-mode-map (make-sparse-keymap))
        (e-chat-block-view-mode-map (make-sparse-keymap))
        (e-chat-tool-list-mode-map (make-sparse-keymap))
        (e-chat-tool-output-mode-map (make-sparse-keymap))
        (e-chat--surface-command-map (make-sparse-keymap)))
    (define-key e-chat-response-navigation-mode-map
                (kbd "RET")
                'e-chat-response-navigation-expand)
    (define-key e-chat--surface-command-map
                (kbd "C-x 0")
                'e-chat-surface-delete-window)
    (define-key e-chat--surface-command-map
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
    (should-not (lookup-key e-chat--surface-command-map (kbd "C-x 0")))
    (should-not
     (lookup-key e-chat--surface-command-map [remap delete-window]))))

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
      (e-chat--configure-evil-composer-bindings))
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
      ;; Should not error when `evil-define-key*' is absent.
      (should-not (e-chat--configure-evil-composer-bindings)))))

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

(ert-deftest e-chat-test-inspect-error-targets-failed-turn-at-point ()
  "e-inspect-error prefers a failed turn under point in a chat buffer."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         captured)
    (e-chat-test--create-session
     store :id "failed-session" :metadata '(:project-root "/tmp/project/"))
    (e-session-append-message
     store "failed-session"
     '(:id "msg-1" :role user :content "broken" :turn-id "failed-turn"))
    (e-session-append-activity-event
     store "failed-session" "failed-turn" 'turn-failed
     '(:error "failed at point" :details (:status 520)))
    (with-current-buffer (e-chat-open :harness harness
                                      :session-id "failed-session")
      (e-chat--render-turn-failure
       "failed-turn"
       "2026-06-10T00:00:00Z"
       '(:error "failed at point" :details (:status 520))
       t)
      (goto-char (point-min))
      (search-forward "Turn failed")
      (cl-letf (((symbol-function 'e-chat-create-session)
                 (lambda (&rest args)
                   (setq captured (plist-put captured :create-args args))
                   '(:id "inspection-session")))
                ((symbol-function 'e-chat-open-session)
                 (lambda (_harness session-id &optional _display)
                   (setq captured (plist-put captured :opened-session
                                             session-id))
                   (current-buffer)))
                ((symbol-function 'e-chat-submit-session)
                 (lambda (_harness session-id prompt
                          &key references delay metadata)
                   (setq captured
                         (append captured
                                 (list :submitted-session session-id
                                       :prompt prompt
                                       :references references
                                       :delay delay
                                       :metadata metadata))))))
        (e-inspect-error :harness harness)
        (should (equal (plist-get captured :submitted-session)
                       "inspection-session"))
        (should (string-match-p "failed-session"
                                (plist-get captured :prompt)))
        (should (string-match-p "failed-turn"
                                (plist-get captured :prompt)))
        (should (equal (plist-get (plist-get captured :metadata)
                                  :source-session-id)
                       "failed-session"))
        (should (equal (plist-get (plist-get captured :metadata)
                                  :source-turn-id)
                       "failed-turn"))))))

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
    (cl-letf (((symbol-function 'e-chat--session-candidates)
               (lambda () candidates))
              ((symbol-function 'e-chat-service-active-turn-p)
               (lambda (_harness session-id)
                 (equal session-id "alpha-session")))
              ((symbol-function 'e-context-status-text)
               (lambda (&rest _args) "ctx model/effort 10%"))
              ((symbol-function 'e-chat-overview--session-unread-p)
               (lambda (_harness session &optional _instance-id)
                 (equal (plist-get session :id) "beta-session")))
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
              ((symbol-function 'e-chat-overview--session-unread-p)
               (lambda (&rest _args) nil)))
      (let ((e-context-status-estimate-cache-seconds 100))
        (should (string-match-p
                 "ctx gpt-5.5/high ~13% (~123/1k tok)"
                 (e-chat--active-session-line candidate status-cache)))
        (should (string-match-p
                 "ctx gpt-5.5/high ~13% (~123/1k tok)"
                 (e-chat--active-session-line candidate status-cache)))))
    (should (= calls 1))))

(ert-deftest e-chat-test-active-session-line-can-use-stale-status-snapshot ()
  "Active-session picker rows can serve stale cached status without rebuilding."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (candidate (list :harness harness
                          :session '(:id "picker-stale-status-cache"
                                      :title "Picker Stale Status"
                                      :message-count 1
                                      :loaded t)
                          :session-id "picker-stale-status-cache"))
         (status-cache (make-hash-table :test #'equal))
         (calls 0))
    (e-chat-test--create-session store :id "picker-stale-status-cache")
    (let* ((status-key (e-chat--active-session-status-key candidate))
           (status-cache-cell (puthash status-key
                                       (cons
                                        (list :text "ctx cached"
                                              :time 0.0
                                              :snapshot-cache-keyed t
                                              :snapshot-cache-key
                                              (list :status-key status-key
                                                    :prefer-token-usage t
                                                    :estimate-context nil))
                                        nil)
                                       status-cache)))
      (cl-letf (((symbol-function 'e-context-budget-status)
                 (lambda (&rest _args)
                   (setq calls (1+ calls))
                   (error "stale picker status should not rebuild")))
                ((symbol-function 'e-chat-overview--session-unread-p)
                 (lambda (&rest _args) nil)))
        (let ((e-context-status-estimate-cache-seconds 1))
          (should (string-match-p
                   "stale ctx cached"
                   (e-chat--active-session-line candidate status-cache)))))
      (should (eq status-cache-cell
                  (gethash status-key status-cache))))
    (should (= calls 0))))

(ert-deftest e-chat-test-active-session-open-ignores-preview-buffer-and-focuses_workspace ()
  "Opening from active sessions ignores the picker preview and focuses session affinity."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (target-workspace (make-e-workspace-token
                            :backend 'single
                            :id 'target
                            :name "target"
                            :frame (selected-frame)))
         (current-workspace (make-e-workspace-token
                             :backend 'single
                             :id 'current
                             :name "current"
                             :frame (selected-frame)))
         (session '(:id "workspace-active"
                    :title "Workspace Active"
                    :summary "Prompt text"
                    :message-count 1
                    :messages ((:role user :content "Prompt text"))
                    :loaded t))
         (candidate (list :harness harness
                          :session session
                          :session-id "workspace-active"))
         canonical
         preview
         captured-buffer
         captured-workspace)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "workspace-active"
                            :metadata '(:name "Workspace Active"))
          (e-session-append-message
           store
           "workspace-active"
           '(:id "msg-1" :role user :content "Prompt text"))
          (setq canonical
                (e-chat-open :harness harness
                             :session-id "workspace-active"))
          (e-buffer-set-workspace canonical target-workspace)
          (setq preview (get-buffer-create " *e-chat-active-preview*"))
          (e-chat--active-session-preview candidate preview)
          (let ((orig-buffer-list (symbol-function 'buffer-list)))
            (cl-letf (((symbol-function 'buffer-list)
                       (lambda (&optional frame)
                         (append (list preview canonical)
                                 (remove preview
                                         (remove canonical
                                                 (funcall orig-buffer-list
                                                          frame))))))
                      ((symbol-function 'e-workspace-current)
                       (lambda (&optional _frame) current-workspace))
                      ((symbol-function 'e-workspace-display-buffer)
                       (cl-function
                        (lambda (buffer &key workspace action select
                                        side-window-ok)
                          (ignore action select side-window-ok)
                          (setq captured-buffer buffer)
                          (setq captured-workspace workspace)
                          (selected-window)))))
              (should (eq (e-chat--active-session-open candidate)
                          canonical))))
          (should (eq captured-buffer canonical))
          (should (eq captured-workspace target-workspace)))
      (when (buffer-live-p preview)
        (kill-buffer preview))
      (when (buffer-live-p canonical)
        (kill-buffer canonical)))))

(ert-deftest e-chat-test-active-sessions-filters-empty-sessions ()
  "The active sessions picker omits sessions with no user prompts."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions (e-session-store-create)))
         (empty '(:id "empty-session"
                  :title "Empty"
                  :summary nil
                  :message-count 0
                  :messages nil
                  :loaded t))
         (untitled-index-empty '(:id "untitled-index-empty"
                                 :title "Untitled 2026-06-20T12:00:00Z"
                                 :summary "Untitled 2026-06-20T12:00:00Z"
                                 :message-count 0
                                 :messages nil))
         (assistant-only '(:id "assistant-only"
                           :title "Assistant only"
                           :summary nil
                           :message-count 1
                           :messages ((:role assistant
                                        :content "answer without prompt"))
                           :loaded t))
         (prompted '(:id "prompted-session"
                     :title "Prompted"
                     :summary "Prompt text"
                     :message-count 1
                     :messages ((:role user :content "Prompt text"))
                     :loaded t))
         spec)
    (cl-letf (((symbol-function 'e-chat--session-candidates)
               (lambda ()
                 (list (list :harness harness
                             :session empty
                             :session-id "empty-session")
                       (list :harness harness
                             :session untitled-index-empty
                             :session-id "untitled-index-empty")
                       (list :harness harness
                             :session assistant-only
                             :session-id "assistant-only")
                       (list :harness harness
                             :session prompted
                             :session-id "prompted-session"))))
              ((symbol-function 'e-picker-open)
               (lambda (&rest args)
                 (setq spec args)
                 nil)))
      (e-chat-active-sessions)
      (should (equal (mapcar (lambda (candidate)
                               (plist-get candidate :session-id))
                             (funcall (plist-get spec :candidates)))
                     '("prompted-session"))))))

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
        (e-chat--active-session-preview candidate (current-buffer))
        (let ((text (buffer-string)))
          (should-not (string-match-p "first prompt" text))
          (should-not (string-match-p "first response" text))
          (should (string-match-p "last prompt" text))
          (should (string-match-p "last response" text)))))))

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
                (e-chat--active-session-preview candidate (current-buffer))
                (let ((text (buffer-string)))
                  (should-not loaded)
                  (should (string-match-p "last p…" text))
                  (should-not (string-match-p "last prompt" text))
                  (should-not (string-match-p "last response" text)))))))
      (delete-directory directory t))))

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

(ert-deftest e-chat-test-index-loading-bounds-large-session-summary ()
  "Loading projection does not render an unbounded index summary."
  (let ((e-chat-session-summary-preview-max-chars 40)
        (summary (concat "visible prefix " (make-string 200 ?x)
                         " forbidden tail")))
    (with-temp-buffer
      (e-chat-mode)
      (let ((inhibit-read-only t))
        (e-chat--render-session-loading (list :summary summary)))
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
              (should-not e-chat--session-load-request)
              (should (string-match-p
                       "3 earlier transcript messages omitted" text))
              (should-not (string-match-p "first prompt" text))
              (should-not (string-match-p "middle prompt" text))
              (should (string-match-p "last prompt" text))
              (should (string-match-p "last response" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
          (cl-letf (((symbol-function 'e-chat--attach-buffer)
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
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "next")
            (should (equal (e-chat--composer-text) "next"))
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
              (should (equal (e-chat--composer-text) "next")))
            (e-chat--rerender-transcript)
            (let ((text (buffer-string)))
              (should (string-match-p
                       "3 earlier transcript messages omitted" text))
              (should-not (string-match-p "first prompt" text))
              (should-not (string-match-p "middle prompt" text))
              (should (string-match-p "last response" text))
              (should (equal (e-chat--composer-text) "next")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-loaded-session-replay-bounds-activity-events ()
  "Loaded-session replay retains only a bounded activity-event tail."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         buffer)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "loaded-activity"
                            :metadata '(:name "Loaded activity"))
          (e-session-append-message
           store "loaded-activity"
           '(:id "msg-1" :role user :content "inspect" :turn-id "turn-1"))
          (dotimes (index 10)
            (let ((call-id (format "call-%d" index)))
              (e-session-append-activity-event
               store "loaded-activity" "turn-1" 'tool-started
               `(:type tool-call :id ,call-id :name "read"))
              (e-session-append-activity-event
               store "loaded-activity" "turn-1" 'tool-finished
               `(:tool-call (:type tool-call :id ,call-id :name "read")
                 :result (:status ok :content ,call-id)))))
          (e-session-append-activity-event
           store "loaded-activity" "turn-1" 'turn-finished nil)
          (e-session-append-message
           store "loaded-activity"
           '(:id "msg-2" :role assistant :content "done" :turn-id "turn-1"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "loaded-activity")
          (let ((e-chat-session-replay-activity-event-limit 5))
            (setq buffer (e-chat-open-session harness "loaded-activity")))
          (with-current-buffer buffer
            (let* ((turn-id
                    (plist-get
                     (car (e-chat-service-messages harness "loaded-activity"))
                     :turn-id))
                   (record (e-chat--existing-turn-record turn-id)))
              (should record)
              (should (= (length (e-chat--activity-tool-items record)) 2)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
    (should (e-chat-overview--session-unread-p
             harness
             (plist-get candidate :session)))
    (with-temp-buffer
      (e-chat--active-session-preview candidate (current-buffer)))
    (should-not (e-chat-overview--session-unread-p
                 harness
                 (plist-get candidate :session)))))

(ert-deftest e-chat-test-session-id-lookup-does-not-list-session-catalog ()
  "Unread lookup resolves one session without listing and sorting its catalog."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create :sessions store :enabled-layer-ids nil)))
    (dotimes (index 512)
      (e-session-create store :id (format "catalog-%03d" index)))
    (cl-letf (((symbol-function 'e-harness-session-list)
               (lambda (&rest _args) (error "catalog scan"))))
      (should (equal (plist-get
                      (e-chat-overview--session-for-id harness "catalog-511")
                      :id)
                     "catalog-511")))))

(ert-deftest e-chat-test-only-unread-relevant-events-refresh-unread-cache ()
  "Tool activity skips unread work while assistant output refreshes it once."
  (with-temp-buffer
    (e-chat-mode)
    (setq-local e-chat-session-id "unread-events")
    (setq-local e-chat-harness
                (e-harness-create :enabled-layer-ids nil))
    (let ((updates 0)
          (inhibit-read-only t))
      (cl-letf (((symbol-function 'e-chat--workspace-unread-cache-update-buffer)
                 (lambda (&rest _args) (setq updates (1+ updates)))))
        (e-chat--render-event
         '(:type tool-started :session-id "unread-events" :turn-id "turn-1"
           :created-at 1.0 :payload (:id "tool-1" :name "fake")))
        (should (= updates 0))
        (e-chat--render-event
         '(:type message-added :session-id "unread-events" :turn-id "turn-1"
           :created-at 2.0
           :payload (:message (:id "answer" :role assistant
                               :content "done" :turn-id "turn-1"))))
        (should (= updates 1))))))

(ert-deftest e-chat-test-workspace-unread-indicator-follows-chat-affinity ()
  "Workspace unread markers follow chat buffer workspace affinity."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (workspace (make-e-workspace-token
                     :backend 'single
                     :id 'target
                     :name "target"
                     :frame (selected-frame)))
         (other-workspace (make-e-workspace-token
                           :backend 'single
                           :id 'other
                           :name "other"
                           :frame (selected-frame)))
         (buffer (generate-new-buffer " *e-chat-workspace-unread-test*")))
    (unwind-protect
        (progn
          (e-chat--workspace-unread-cache-invalidate)
          (e-chat--refresh-face-specs)
          (e-chat-test--create-session store :id "workspace-unread"
                            :metadata '(:name "Workspace unread"))
          (e-session-append-message
           store "workspace-unread"
           '(:id "msg-1" :role user :content "prompt"))
          (e-session-append-message
           store "workspace-unread"
           '(:id "msg-2" :role assistant :content "response"))
          (with-current-buffer buffer
            (e-chat-mode)
            (setq-local e-chat-harness harness)
            (setq-local e-chat-session-id "workspace-unread")
            (e-buffer-set-workspace buffer workspace))
          (should (e-chat-workspace-unread-p workspace))
          (cl-letf (((symbol-function 'e-chat--buffer-unread-p)
                     (lambda (&rest _args)
                       (error "cached unread lookup should not scan buffers"))))
            (should (e-chat-workspace-unread-p "target"))
            (should-not (e-chat-workspace-unread-p other-workspace))
            (should (equal (substring-no-properties
                            (e-chat-workspace-unread-indicator workspace))
                           "●"))
            (should (eq (get-text-property
                         0
                         'font-lock-face
                         (e-chat-workspace-unread-indicator workspace))
                        'e-chat-workspace-unread-face)))
          (e-chat-overview--mark-session-read
           harness
           (e-chat-overview--session-for-id harness "workspace-unread"))
          (should-not (e-chat-workspace-unread-p workspace))
          (should-not (e-chat-workspace-unread-indicator workspace)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat--workspace-unread-cache-invalidate))))

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
          (e-chat--workspace-unread-cache-invalidate)
          (e-chat-test--create-session store :id "focus-read"
                            :metadata '(:name "Focus read"))
          (e-session-append-message
           store "focus-read"
           '(:id "msg-1" :role user :content "prompt"))
          (e-session-append-message
           store "focus-read"
           '(:id "msg-2" :role assistant :content "response"))
          (should (e-chat-overview--session-unread-p
                   harness
                   (car (e-harness-session-list harness))))
          (with-current-buffer buffer
            (e-chat-mode)
            (setq-local e-chat-harness harness)
            (setq-local e-chat-session-id "focus-read")
            (e-buffer-set-workspace buffer workspace)
            (e-chat--mark-buffer-session-read-if-selected))
          (should (e-chat-workspace-unread-p workspace))
          (should (e-chat-overview--session-unread-p
                   harness
                   (car (e-harness-session-list harness))))
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (e-chat--mark-buffer-session-read-if-selected))
          (should-not (e-chat-overview--session-unread-p
                       harness
                       (car (e-harness-session-list harness))))
          (should-not (e-chat-workspace-unread-p workspace))
          (should (equal
                   (e-chat-overview--read-marker "focus-read" harness)
                   "msg-2")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat--workspace-unread-cache-invalidate))))

(ert-deftest e-chat-test-mode-does-not-poll-read-state-after-commands ()
  "Chat mode does not mark sessions read from `post-command-hook'."
  (with-temp-buffer
    (e-chat-mode)
    (should-not (memq #'e-chat--mark-buffer-session-read-if-selected
                      post-command-hook))
    (should-not (memq #'e-chat--mark-selected-session-read
                      post-command-hook))))

(ert-deftest e-chat-test-installs-surface-activation-hooks ()
  "Chat activation follows selection and settled window-buffer changes."
  (cl-progv '(window-selection-change-functions
              window-configuration-change-hook
              window-buffer-change-functions
              buffer-list-update-hook
              minibuffer-exit-hook
              persp-activated-functions)
      '((e-chat--tail-selected-active-turn)
        (e-chat--tail-selected-active-turn)
        nil
        (e-chat--activate-selected-surface-after-buffer-switch)
        nil
        (e-chat--tail-selected-active-turn
         e-chat--activate-selected-surface-after-workspace-switch))
    (e-chat--ensure-window-selection-hook)
    (should-not (memq 'e-chat--tail-selected-active-turn
                      window-selection-change-functions))
    (should-not (memq 'e-chat--tail-selected-active-turn
                      window-configuration-change-hook))
    (should-not (memq 'e-chat--tail-selected-active-turn
                      persp-activated-functions))
    (should (memq #'e-chat--mark-selected-session-read
                  window-selection-change-functions))
    (should (memq #'e-chat--activate-selected-surface-on-selection
                  window-selection-change-functions))
    (should-not (memq #'e-chat--activate-selected-surface-on-selection
                      window-configuration-change-hook))
    (should
     (memq #'e-chat--activate-selected-surface-after-window-buffer-change
           window-buffer-change-functions))
    (should-not (memq 'e-chat--activate-selected-surface-after-buffer-switch
                      buffer-list-update-hook))
    (should (memq #'e-chat--flush-deferred-activity-redraws-after-minibuffer
                  minibuffer-exit-hook))
    (should (memq #'e-chat--mark-selected-session-read
                  persp-activated-functions))
    (should-not
     (memq 'e-chat--activate-selected-surface-after-workspace-switch
           persp-activated-functions))))

(ert-deftest e-chat-test-active-sessions-errors-without-candidates ()
  "The active sessions command reports an empty session list."
  (cl-letf (((symbol-function 'e-chat--session-candidates)
             (lambda () nil)))
    (should-error (e-chat-active-sessions) :type 'user-error)))

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
          (goto-char (point-max))
          (insert "next")
          (should (equal (e-chat--composer-text) "next")))
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
                            (:code "(e-actions-call 'session-compaction :compact '(:keep_recent_tokens 1))")))
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
          (should (e-chat--side-window-p))
          ;; Must not error, and must not try to host the chat in the side
          ;; window.
          (e-chat--pop-to-buffer chat)
          (let ((shown (get-buffer-window chat t)))
            (should (window-live-p shown))
            (should-not (window-parameter shown 'window-side)))
          ;; The same-window path is side-window safe too.
          (select-window side-window)
          (e-chat--switch-to-buffer chat)
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
        (cl-letf (((symbol-function 'e-chat--non-side-window) (lambda (&rest _) nil)))
          (should (window-live-p side-window))
          (select-window side-window)
          (should (e-chat--side-window-p))
          ;; Must not error; must create a fresh normal (non-side) window.
          (e-chat--pop-to-buffer chat)
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

(defun e-chat-test--record-with-round ()
  "Return a fresh turn record carrying one active provider round."
  (let ((record (list :id "turn-1"
                      :activity-records nil
                      :intermittent-entries nil)))
    (e-chat--append-activity-record
     record
     (list :kind 'round
           :round 1
           :status 'active
           :reasoning nil
           :tool-batches nil))
    record))

(ert-deftest e-chat-test-run-elisp-tool-name-shows-single-action ()
  "A run_elisp tool row names the capability/action it invoked."
  (let ((record (e-chat-test--record-with-round)))
    (e-chat--record-tool-started
     record
     '(:id "call-1" :name "run_elisp" :arguments (:code "..."))
     'activity 0)
    (e-chat--record-action-started
     record
     '(:parent-tool-call-id "call-1"
       :capability-id "elisp-job"
       :action :run-batch)
     'activity)
    (let ((item (car (e-chat--round-tool-items
                      (car (e-chat--activity-records record))))))
      (should (equal (e-chat--tool-item-name item)
                     "run_elisp (elisp-job/run-batch)")))))

(ert-deftest e-chat-test-run-elisp-tool-name-counts-multiple-actions ()
  "A run_elisp tool row counts distinct actions when more than one runs."
  (let ((record (e-chat-test--record-with-round)))
    (e-chat--record-tool-started
     record
     '(:id "call-1" :name "run_elisp" :arguments (:code "..."))
     'activity 0)
    (e-chat--record-action-started
     record
     '(:parent-tool-call-id "call-1"
       :capability-id "elisp-job"
       :action :run-batch)
     'activity)
    (e-chat--record-action-started
     record
     '(:parent-tool-call-id "call-1"
       :capability-id "workspace-awareness"
       :action :focus)
     'activity)
    (let ((item (car (e-chat--round-tool-items
                      (car (e-chat--activity-records record))))))
      (should (equal (e-chat--tool-item-name item)
                     "run_elisp (2 actions)")))))

(ert-deftest e-chat-test-run-elisp-tool-name-dedups-repeated-action ()
  "A run_elisp tool row keeps the single name when one action repeats."
  (let ((record (e-chat-test--record-with-round)))
    (e-chat--record-tool-started
     record
     '(:id "call-1" :name "run_elisp" :arguments (:code "..."))
     'activity 0)
    (dotimes (_ 2)
      (e-chat--record-action-started
       record
       '(:parent-tool-call-id "call-1"
         :capability-id "elisp-job"
         :action :run-batch)
       'activity))
    (let ((item (car (e-chat--round-tool-items
                      (car (e-chat--activity-records record))))))
      (should (equal (e-chat--tool-item-name item)
                     "run_elisp (elisp-job/run-batch)")))))

(ert-deftest e-chat-test-top-level-action-leaves-tool-name-unchanged ()
  "An action with no parent tool call does not alter any tool row."
  (let ((record (e-chat-test--record-with-round)))
    (e-chat--record-tool-started
     record
     '(:id "call-1" :name "read" :arguments (:path "file"))
     'activity 0)
    (e-chat--record-action-started
     record
     '(:capability-id "elisp-job" :action :run-batch)
     'activity)
    (let ((item (car (e-chat--round-tool-items
                      (car (e-chat--activity-records record))))))
      (should (equal (e-chat--tool-item-name item) "read")))))

(ert-deftest e-chat-test-replayed-run-elisp-action-name-renders ()
  "Replaying a run_elisp tool with a nested action names the action in-buffer."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-run-elisp-replay")
          (e-session-append-message
           store "chat-run-elisp-replay"
           '(:role user :content "run it" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'provider-request-started
           '(:status started))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'tool-started
           '(:type tool-call :id "call-1" :name "run_elisp"))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'action-started
           '(:parent-tool-call-id "call-1"
             :capability-id "elisp-job"
             :action :run-batch
             :status started))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'tool-finished
           '(:tool-call (:type tool-call :id "call-1" :name "run_elisp")
             :result (:status ok :content "done")))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'provider-request-finished
           '(:status done))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'turn-finished nil)
          (e-session-append-message
           store "chat-run-elisp-replay"
           '(:role assistant :content "Final answer." :turn-id "turn-1"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-run-elisp-replay")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-run-elisp-replay"))
          (with-current-buffer buffer
            (let* ((record (e-chat--existing-turn-record "turn-1"))
                   (round (car (e-chat--activity-records record))))
              (should (string-match-p
                       "1 tool call (run_elisp (elisp-job/run-batch))"
                       (e-chat--activity-round-visible-text round))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-test)

;;; e-chat-test.el ends here
