;;; e-chat-composer-composition-test.el --- Public chat composer composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Composer editing, submit, context-reference, completion, and key behavior.

;;; Code:

(require 'ert)
(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

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
  (let ((buffer (e-chat-test--buffer nil "chat-composer-evil-focus"))
        entered)
    (unwind-protect
        (let ((composer (e-chat-test--composer buffer)))
          (with-current-buffer composer
            (setq-local evil-local-mode t))
          (cl-letf (((symbol-function 'evil-insert-state)
                     (lambda () (setq entered t))))
            (with-current-buffer buffer
              (e-chat-composer-enter-input-state)))
          (should entered))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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

(provide 'e-chat-composer-composition-test)

;;; e-chat-composer-composition-test.el ends here
