;;; e-org-canvas-test.el --- Tests for Org Canvas shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the Org Canvas presentation shell and capability layer.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'e)
(require 'e-backend)
(require 'e-chat)
(require 'e-chat-session)
(require 'e-context-status)
(require 'e-default-harnesses)
(require 'e-events)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-layers)
(require 'e-session)
(require 'e-shells)
(require 'e-tools)

(require 'e-org-canvas nil t)
(require 'e-org-canvas-capabilities nil t)

(defmacro e-org-canvas-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal)))
     ,@body))

(defun e-org-canvas-test--harness (&optional with-org-canvas)
  "Return a fake harness with chat-session and optional Org Canvas capability."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (when with-org-canvas
      (e-harness-activate-layer harness (e-org-canvas-layer-create)))
    harness))

(defun e-org-canvas-test--kill-chat-buffers ()
  "Kill all live e chat buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'e-chat-mode)
          (kill-buffer buffer))))))

(defun e-org-canvas-test--org-file (directory name)
  "Create an Org file NAME in DIRECTORY and return its path."
  (let ((file (expand-file-name name directory)))
    (write-region "* One\nbody\n" nil file nil 'silent)
    file))

(defun e-org-canvas-test--session-with-file (harness session-id file)
  "Create an Org Canvas SESSION-ID for FILE in HARNESS."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (org-mode)
      (e-harness-create-session
       harness
       :id session-id
       :metadata (list :project-root (file-name-as-directory
                                      (file-name-directory file))))
      (e-org-canvas--mark-session
       harness session-id buffer :scope 'thread :target-folder nil)
      session-id)))

(ert-deftest e-org-canvas-test-exposes-entrypoints ()
  "The package exposes Org Canvas commands and modes."
  (dolist (symbol '(e-org-canvas-open-for-current-buffer
                    e-org-canvas-new-file
                    e-org-canvas-new-buffer
                    e-org-canvas-prompt-thread
                    e-org-canvas-prompt-document
                    e-org-canvas-prompt
                    e-org-canvas-reopen-last-prompt
                    e-org-canvas-respond-to-threads
                    e-org-canvas-list-sessions
                    e-org-canvas-list-project-sessions
                    e-org-canvas-resume
                    e-org-canvas-mode
                    e-org-canvas-input-mode))
    (should (commandp symbol))))

(ert-deftest e-org-canvas-test-open-current-buffer-creates-session-metadata-and-displays-chat ()
  "Opening an Org buffer creates Org Canvas metadata and displays chat."
  (let ((harness (e-org-canvas-test--harness)))
    (unwind-protect
        (e-org-canvas-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :org-canvas-test))
            (e-harness-registry-register :org-canvas-test harness)
            (with-temp-buffer
              (rename-buffer "org-canvas-source" t)
              (org-mode)
              (insert "* Topic\nBody\n")
              (let ((chat-buffer (e-org-canvas-open-for-current-buffer)))
                (should (buffer-live-p chat-buffer))
                (should (eq (window-buffer (selected-window))
                            (current-buffer)))
                (let ((source-window (get-buffer-window (current-buffer) t))
                      (chat-window (get-buffer-window chat-buffer t)))
                  (should (window-live-p source-window))
                  (should (window-live-p chat-window))
                  (should (> (nth 1 (window-edges chat-window))
                             (nth 1 (window-edges source-window)))))
                (should e-org-canvas-mode)
                (should e-chat-context-mode-suppressed)
                (with-current-buffer chat-buffer
                  (let* ((session (e-session-get
                                   (e-harness-sessions e-chat-harness)
                                   e-chat-session-id))
                         (metadata (plist-get session :metadata))
                         (org-canvas (plist-get metadata :org-canvas))
                         (attachment (car (e-chat-session-attachments
                                           e-chat-harness
                                           e-chat-session-id))))
                    (should (plist-get attachment :canvas))
                    (should (equal (plist-get org-canvas :uri)
                                   "buffer://org-canvas-source"))
                    (should (equal (plist-get org-canvas :buffer-name)
                                   "org-canvas-source"))
                    (should (equal (plist-get org-canvas :mode) 'org))
                    (should (plist-get org-canvas :root))))))))
      (e-org-canvas-test--kill-chat-buffers))))

(ert-deftest e-org-canvas-test-open-reuses-session-by-file-uri ()
  "Reopening the same file-backed Org canvas reuses its existing session."
  (let ((directory (make-temp-file "e-org-canvas-" t))
        (harness (e-org-canvas-test--harness)))
    (unwind-protect
        (e-org-canvas-test--with-empty-harness-registry
          (let* ((e-chat-default-harness-id :org-canvas-test)
                 (file (e-org-canvas-test--org-file directory "notes.org"))
                 first-id second-id)
            (e-harness-registry-register :org-canvas-test harness)
            (with-current-buffer (find-file-noselect file)
              (setq first-id
                    (with-current-buffer
                        (e-org-canvas-open-for-current-buffer)
                      e-chat-session-id))
              (kill-buffer (current-buffer)))
            (with-current-buffer (find-file-noselect file)
              (setq second-id
                    (with-current-buffer
                        (e-org-canvas-open-for-current-buffer)
                      e-chat-session-id)))
            (should (equal second-id first-id))
            (should (= (length (e-harness-session-list harness)) 1))))
      (e-org-canvas-test--kill-chat-buffers)
      (dolist (buffer (buffer-list))
        (when (and (buffer-file-name buffer)
                   (file-in-directory-p (buffer-file-name buffer) directory))
          (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest e-org-canvas-test-open-existing-session-displays-chat-below-source ()
  "Reopening an existing Org Canvas session displays its chat below the source."
  (let ((directory (make-temp-file "e-org-canvas-" t))
        (harness (e-org-canvas-test--harness))
        (original-window (selected-window))
        (original-buffer (window-buffer))
        source
        chat-buffer)
    (unwind-protect
        (e-org-canvas-test--with-empty-harness-registry
          (let* ((e-chat-default-harness-id :org-canvas-test)
                 (file (e-org-canvas-test--org-file directory "notes.org")))
            (e-harness-registry-register :org-canvas-test harness)
            (delete-other-windows)
            (setq source (find-file-noselect file))
            (set-window-buffer original-window source)
            (select-window original-window)
            (with-current-buffer source
              (e-org-canvas-open-for-current-buffer))
            (delete-other-windows)
            (set-window-buffer original-window source)
            (select-window original-window)
            (setq chat-buffer
                  (with-current-buffer source
                    (e-org-canvas-open-for-current-buffer)))
            (let ((source-window (get-buffer-window source t))
                  (chat-window (get-buffer-window chat-buffer t)))
              (should (window-live-p source-window))
              (should (window-live-p chat-window))
              (should (eq (selected-window) source-window))
              (should (> (nth 1 (window-edges chat-window))
                         (nth 1 (window-edges source-window)))))))
      (when (window-live-p original-window)
        (select-window original-window)
        (set-window-buffer original-window original-buffer))
      (delete-other-windows)
      (when (buffer-live-p source)
        (kill-buffer source))
      (e-org-canvas-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-org-canvas-test-new-file-directory-starts-unsaved-folder-canvas ()
  "Selecting a directory creates an unsaved Org Canvas targeting that folder."
  (let ((directory (file-name-as-directory
                    (make-temp-file "e-org-canvas-directory-" t)))
        (harness (e-org-canvas-test--harness)))
    (unwind-protect
        (e-org-canvas-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :org-canvas-test))
            (e-harness-registry-register :org-canvas-test harness)
            (let ((chat-buffer (e-org-canvas-new-file directory)))
              (with-current-buffer chat-buffer
                (let* ((session (e-session-get
                                 (e-harness-sessions e-chat-harness)
                                 e-chat-session-id))
                       (org-canvas (plist-get
                                    (plist-get session :metadata)
                                    :org-canvas))
                       (target-buffer (get-buffer
                                       (plist-get org-canvas :buffer-name))))
                  (should (plist-get org-canvas :needs-file-name))
                  (should (equal (plist-get org-canvas :target-folder)
                                 directory))
                  (should (string-prefix-p
                           "buffer://"
                           (plist-get org-canvas :uri)))
                  (should (buffer-live-p target-buffer))
                  (with-current-buffer target-buffer
                    (should (derived-mode-p 'org-mode))
                    (should-not buffer-file-name)
                    (should (equal default-directory directory)))
                  (should (eq (window-buffer (selected-window))
                              target-buffer)))))))
      (e-org-canvas-test--kill-chat-buffers)
      (dolist (buffer (buffer-list))
        (when (string-prefix-p "*e-org-canvas:" (buffer-name buffer))
          (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest e-org-canvas-test-mode-owns-prompt-keys-and-suppresses-chat-context ()
  "Org Canvas mode owns s-i/S-i without disabling chat context elsewhere."
  (with-temp-buffer
    (org-mode)
    (let ((global-map e-chat-context-mode-map))
      (should (eq (lookup-key global-map (kbd "s-i"))
                  'e-chat-add-context-to-latest))
      (e-org-canvas-mode 1)
      (should e-chat-context-mode-suppressed)
      (should (assq 'e-chat-context-mode minor-mode-overriding-map-alist))
      (should (eq (lookup-key e-org-canvas-mode-map (kbd "s-i"))
                  'e-org-canvas-prompt-thread))
      (should (eq (lookup-key e-org-canvas-mode-map (kbd "s-I"))
                  'e-org-canvas-prompt-document))
      (e-org-canvas-mode -1)
      (should-not e-chat-context-mode-suppressed)
      (should-not (assq 'e-chat-context-mode
                        minor-mode-overriding-map-alist)))))

(ert-deftest e-org-canvas-test-mode-shows-major-slot_status_indicator ()
  "Org Canvas mode marks the major-mode slot and restores it on disable."
  (with-temp-buffer
    (org-mode)
    (let ((original-mode-name mode-name))
      (e-org-canvas-mode 1)
      (should (equal mode-name "Org Canvas"))
      (e-org-canvas-mode -1)
      (should (equal mode-name original-mode-name)))))

(ert-deftest e-org-canvas-test-mode-restores_prior_buffer_local_mode_name ()
  "Org Canvas mode preserves a pre-existing buffer-local `mode-name'."
  (with-temp-buffer
    (org-mode)
    (setq-local mode-name "Custom Org")
    (e-org-canvas-mode 1)
    (should (equal mode-name "Org Canvas"))
    (e-org-canvas-mode -1)
    (should (equal mode-name "Custom Org"))))

(ert-deftest e-org-canvas-test-mode-shows-context-state-indicator ()
  "Attached Org Canvas buffers show model, effort, and token context."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (e-context-status-model-token-limits '(("gpt-5.5" . 100)))
         (e-context-status-estimate-bytes-per-token 1.0))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (with-temp-buffer
      (org-mode)
      (e-session-create store :id "org-canvas-status")
      (e-session-append-message
       store "org-canvas-status" '(:role user :content "context question"))
      (setq-local e-org-canvas-harness harness)
      (setq-local e-org-canvas-session-id "org-canvas-status")
      (e-org-canvas-mode 1)
      (should (string-match-p "Org Canvas gpt-5.5/high" mode-name))
      (should (string-match-p "~[0-9]+%" mode-name))
      (should (string-match-p "/100 tok" mode-name))
      (e-org-canvas-mode -1))))

(ert-deftest e-org-canvas-test-mode-refreshes-indicator-on-token-usage ()
  "Org Canvas indicator updates from durable provider token usage events."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high"))))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (with-temp-buffer
      (org-mode)
      (e-session-create store :id "org-canvas-usage")
      (setq-local e-org-canvas-harness harness)
      (setq-local e-org-canvas-session-id "org-canvas-usage")
      (e-org-canvas-mode 1)
      (e-session-append-activity-event
       store "org-canvas-usage" "turn-1" 'token-usage
       '(:input-tokens 202598 :total-tokens 203017))
      (e-harness--emit
       harness
       (e-events-make :type 'token-usage
                      :session-id "org-canvas-usage"
                      :turn-id "turn-1"))
      (should (equal mode-name "Org Canvas gpt-5.5/high 78% (203k/258k tok)"))
      (e-org-canvas-mode -1))))

(ert-deftest e-org-canvas-test-compact-uses-session-action ()
  "Org Canvas compaction delegates to the shared session compaction action."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (calls nil))
    (cl-letf (((symbol-function 'e-chat-session-compact)
               (lambda (h session-id &rest args)
                 (push (list h session-id args) calls))))
      (with-temp-buffer
        (org-mode)
        (setq-local e-org-canvas-harness harness)
        (setq-local e-org-canvas-session-id "org-canvas-compact")
        (e-org-canvas-compact)
        (e-org-canvas-mode -1)))
    (should (equal (length calls) 1))
    (should (equal (nth 1 (car calls)) "org-canvas-compact"))))

(ert-deftest e-org-canvas-test-mode-installs-buffer-local-evil-prompt_keys ()
  "Org Canvas mode overrides Evil normal-state chat context bindings locally."
  (with-temp-buffer
    (org-mode)
    (let (calls)
      (cl-letf (((symbol-function 'evil-local-set-key)
                 (lambda (state key command)
                   (push (list state key command) calls))))
        (e-org-canvas-mode 1)
        (should (member (list 'normal
                              (kbd "s-i")
                              'e-org-canvas-prompt-thread)
                        calls))
        (should (member (list 'normal
                              (kbd "s-I")
                              'e-org-canvas-prompt-document)
                        calls))
        (setq calls nil)
        (e-org-canvas-mode -1)
        (should (member (list 'normal (kbd "s-i") nil) calls))
        (should (member (list 'normal (kbd "s-I") nil) calls))))))

(ert-deftest e-org-canvas-test-startup-refreshes-existing-mode-evil_keys ()
  "Startup refresh reapplies live Org Canvas buffer-local presentation state."
  (with-temp-buffer
    (org-mode)
    (setq-local e-org-canvas-mode t)
    (setq-local mode-name "Org")
    (let (calls)
      (cl-letf (((symbol-function 'evil-local-set-key)
                 (lambda (state key command)
                   (push (list state key command) calls))))
        (e-org-canvas-startup)
        (should (member (list 'normal
                              (kbd "s-i")
                              'e-org-canvas-prompt-thread)
                        calls))
        (should (member (list 'normal
                              (kbd "s-I")
                              'e-org-canvas-prompt-document)
                        calls))
        (should (equal mode-name "Org Canvas"))))))

(ert-deftest e-org-canvas-test-focus-captures-heading-path-and_visibility ()
  "Focus capture records positional heading and visibility context."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Child\nBody\n* Other\n")
    (goto-char (point-min))
    (search-forward "Child")
    (beginning-of-line)
    (outline-hide-subtree)
    (let ((focus (e-org-canvas-capture-focus 'thread)))
      (should (eq (plist-get focus :kind) 'position))
      (should (equal (plist-get focus :scope) 'thread))
      (should (equal (plist-get focus :heading-path) '("Parent" "Child")))
      (should (integerp (plist-get focus :subtree-start)))
      (should (integerp (plist-get focus :subtree-end)))
      (should (eq (plist-get focus :visibility) 'folded))
      (should (integerp (plist-get focus :window-start)))
      (should (integerp (plist-get focus :window-end))))))

(ert-deftest e-org-canvas-test-context-provider-is-gated-by-session-metadata ()
  "Org Canvas context appears only for sessions marked as Org Canvas."
  (let ((harness (e-org-canvas-test--harness t)))
    (with-temp-buffer
      (rename-buffer "org-canvas-context" t)
      (org-mode)
      (insert "* Topic\nBody\n")
      (e-harness-create-session harness :id "plain")
      (e-harness-create-session harness :id "org")
      (e-org-canvas--mark-session
       harness "org" (current-buffer) :scope 'thread :target-folder nil)
      (let ((plain (plist-get (e-harness-context harness "plain")
                              :messages))
            (org (plist-get (e-harness-context harness "org")
                            :messages)))
        (should-not
         (string-match-p "Org Canvas"
                         (mapconcat (lambda (message)
                                      (or (plist-get message :content) ""))
                                    plain "\n")))
        (should
         (string-match-p "Org Canvas"
                         (mapconcat (lambda (message)
                                      (or (plist-get message :content) ""))
                                    org "\n")))
        (should
         (string-match-p "topic under the cursor"
                         (mapconcat (lambda (message)
                                      (or (plist-get message :content) ""))
                                    org "\n")))))))

(ert-deftest e-org-canvas-test-context-provider-is-dynamic-cache-placement ()
  "Org Canvas live buffer context is placed after stable cacheable context."
  (let ((provider (car (e-capability-context-providers
                        (e-org-canvas-capability-create)))))
    (should (eq (e-context-provider-cache-placement provider)
                'dynamic-context))))

(ert-deftest e-org-canvas-test-document-context-uses-whole-document-scope ()
  "Document-scope context asks the model to consider the full Org document."
  (let ((harness (e-org-canvas-test--harness t)))
    (with-temp-buffer
      (rename-buffer "org-canvas-document-context" t)
      (org-mode)
      (insert "* Topic\nBody\n")
      (e-harness-create-session harness :id "org")
      (e-org-canvas--mark-session
       harness "org" (current-buffer) :scope 'document :target-folder nil)
      (let ((content (mapconcat
                      (lambda (message) (or (plist-get message :content) ""))
                      (plist-get (e-harness-context harness "org") :messages)
                      "\n")))
        (should (string-match-p "whole Org document" content))
        (should (string-match-p "point=" content))
        ;; Guidance must point durable writes at document-uri and warn off the
        ;; *e-org-canvas* / *e-org-canvas-input* helper buffers.
        (should (string-match-p "document-uri" content))
        (should (string-match-p "helper buffers" content))))))

(ert-deftest e-org-canvas-test-submit-records-scope-focus-and_canvas_metadata ()
  "Prompt submission records Org Canvas turn metadata through chat-session."
  (let ((harness (e-org-canvas-test--harness)))
    (with-temp-buffer
      (rename-buffer "org-canvas-submit" t)
      (org-mode)
      (insert "* Topic\nBody\n")
      (e-harness-create-session harness :id "session-1")
      (e-org-canvas--mark-session
       harness "session-1" (current-buffer) :scope 'thread :target-folder nil)
      (let (call)
        (cl-letf (((symbol-function 'e-chat-session-submit)
                   (lambda (&rest args)
                     (setq call args)
                     "turn-1")))
          (should (equal
                   (e-org-canvas-submit-prompt
                    harness "session-1" "expand this" 'thread)
                   "turn-1")))
        (should (equal (nth 2 call) "expand this"))
        (let ((metadata (plist-get (nthcdr 3 call) :metadata)))
          (should (equal (plist-get metadata :org-canvas-scope) 'thread))
          (should (equal (plist-get metadata :org-canvas-uri)
                         "buffer://org-canvas-submit"))
          (should (plist-get metadata :org-canvas-focus)))))))

(ert-deftest e-org-canvas-test-input-pane-uses_chat_composer_surface ()
  "Input panes reuse chat composer chrome without status/header text."
  (with-temp-buffer
    (rename-buffer "org-canvas-composer" t)
    (insert "* Topic\nBody\n")
    (org-mode)
    (let ((buffer (e-org-canvas--input-buffer
                   :harness nil
                   :session-id "session-1"
                   :scope 'document
                   :target-buffer (current-buffer))))
      (unwind-protect
          (with-current-buffer buffer
            (should (derived-mode-p 'e-org-canvas-input-mode))
            (should (derived-mode-p 'e-chat-mode))
            (should (e-chat--composer-active-p))
            (save-excursion
              (goto-char (point-min))
              (should (search-forward e-chat--composer-glyph nil t)))
            (should (equal e-org-canvas-input--session-id "session-1"))
            (should (equal e-org-canvas-input--scope 'document))
            (should-not (string-match-p "Scope:" (buffer-string)))
            (should-not (string-match-p "Status:" (buffer-string)))
            (should-not (string-match-p "Prompt:" (buffer-string)))
            (should (eq (lookup-key e-org-canvas-input-mode-map (kbd "C-c C-c"))
                        'e-org-canvas-input-submit))
            (should (eq (lookup-key e-org-canvas-input-mode-map (kbd "C-c C-k"))
                        'e-org-canvas-input-cancel))
            (should (eq (lookup-key e-org-canvas-input-mode-map (kbd "C-c C-s"))
                        'e-org-canvas-input-switch-scope))
            (should (eq (lookup-key e-org-canvas-input-mode-map (kbd "C-c C-o"))
                        'e-org-canvas-input-open-session)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest e-org-canvas-test-input-open-session-reveals_backing_chat ()
  "Opening the session from an input pane reveals the normal backing chat."
  (let ((harness (e-org-canvas-test--harness))
        (original-window (selected-window))
        (original-buffer (window-buffer))
        source
        chat
        input
        opened)
    (unwind-protect
        (e-org-canvas-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :org-canvas-test))
            (e-harness-registry-register :org-canvas-test harness)
            (delete-other-windows)
            (setq source (get-buffer-create "org-canvas-input-open-source"))
            (with-current-buffer source
              (org-mode)
              (insert "* Topic\nBody\n"))
            (set-window-buffer original-window source)
            (select-window original-window)
            (e-harness-create-session
             harness
             :id "session-1"
             :metadata (list :project-root default-directory))
            (e-org-canvas--mark-session
             harness "session-1" source :scope 'thread :target-folder nil)
            (e-session-append-message
             (e-harness-sessions harness)
             "session-1"
             '(:id "msg-1" :role user :content "Existing backing chat history"))
            (setq chat (e-chat-open :harness harness :session-id "session-1"))
            (setq input
                  (e-org-canvas--input-buffer
                   :harness harness
                   :session-id "session-1"
                   :scope 'thread
                   :target-buffer source))
            (set-window-buffer original-window input)
            (select-window original-window)
            (setq opened
                  (with-current-buffer input
                    (e-org-canvas-input-open-session)))
            (should (eq opened chat))
            (should (eq (window-buffer (selected-window)) chat))
            (should-not (eq opened input))
            (should-not (get-buffer-window input t))
            (with-current-buffer opened
              (goto-char (point-min))
              (should (search-forward "Existing backing chat history" nil t)))))
      (when (window-live-p original-window)
        (select-window original-window)
        (set-window-buffer original-window original-buffer))
      (delete-other-windows)
      (dolist (buffer (list input chat source))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (e-org-canvas-test--kill-chat-buffers))))

(ert-deftest e-org-canvas-test-prompt-scope-selects_input_pane_for_typing ()
  "Prompt commands select the editable input pane at the prompt body."
  (let ((harness (e-org-canvas-test--harness))
        input
        expected-uri)
    (e-harness-create-session harness :id "session-1")
    (with-temp-buffer
      (org-mode)
      (let ((target (current-buffer))
            (window (selected-window)))
        (setq expected-uri (concat "buffer://" (buffer-name target)))
        (cl-letf (((symbol-function 'e-org-canvas--ensure-current-session)
                   (lambda () (list harness "session-1" target)))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _args)
                     (setq input buffer)
                     (set-window-buffer window buffer)
                     window)))
          (e-org-canvas--prompt-scope 'thread))))
    (unwind-protect
        (with-current-buffer input
          (should (derived-mode-p 'e-org-canvas-input-mode))
          (should (equal (current-buffer) (window-buffer (selected-window))))
          (should (e-chat--point-in-composer-p))
          (should (= (point) (point-max)))
          (let ((reference (get-text-property
                            e-chat--composer-start-marker
            'e-chat-context-reference)))
            (should reference)
            (should (equal (plist-get reference :uri)
                           expected-uri))))
      (when (buffer-live-p input)
        (kill-buffer input)))))

(ert-deftest e-org-canvas-test-prompt-scope-hides_visible_backing_chat_buffer ()
  "Org Canvas prompting does not leave the backing chat buffer visible too."
  (let ((harness (e-org-canvas-test--harness))
        (original-window (selected-window))
        original-buffer
        target
        chat
        chat-window
        input)
    (setq original-buffer (window-buffer original-window))
    (unwind-protect
        (e-org-canvas-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :org-canvas-test))
            (e-harness-registry-register :org-canvas-test harness)
            (delete-other-windows)
            (setq target (get-buffer-create "org-canvas-visible-chat-source"))
            (with-current-buffer target
              (org-mode)
              (insert "* Topic\nBody\n"))
            (set-window-buffer original-window target)
            (select-window original-window)
            (setq chat
                  (with-current-buffer target
                    (e-org-canvas-open-for-current-buffer)))
            (setq chat-window (split-window original-window nil 'below))
            (set-window-buffer chat-window chat)
            (should (get-buffer-window chat t))
            (with-current-buffer target
              (setq input (e-org-canvas--prompt-scope 'thread)))
            (should (buffer-live-p chat))
            (should-not (get-buffer-window chat t))
            (should (get-buffer-window target t))
            (should (get-buffer-window input t))))
      (when (window-live-p original-window)
        (select-window original-window))
      (delete-other-windows)
      (when (buffer-live-p original-buffer)
        (set-window-buffer (selected-window) original-buffer))
      (dolist (buffer (list input chat target))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (e-org-canvas-test--kill-chat-buffers))))

(ert-deftest e-org-canvas-test-prompt-thread-clears_source_region_after_done ()
  "A source selection used for s-i is cleared when the Org Canvas turn is done."
  (let ((harness (e-org-canvas-test--harness))
        input)
    (e-harness-create-session harness :id "session-1")
    (with-temp-buffer
      (rename-buffer "org-canvas-selected-source" t)
      (org-mode)
      (insert "* Topic\nalpha beta gamma\n")
      (goto-char (point-min))
      (search-forward "beta")
      (set-mark (match-beginning 0))
      (setq mark-active t)
      (let ((target (current-buffer))
            (window (selected-window)))
        (e-org-canvas--mark-session
         harness "session-1" target :scope 'thread :target-folder nil)
        (cl-letf (((symbol-function 'e-org-canvas--ensure-current-session)
                   (lambda () (list harness "session-1" target)))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _args)
                     (setq input buffer)
                     (set-window-buffer window buffer)
                     window))
                  ((symbol-function 'select-window)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _args)
                     (set-window-buffer window buffer)
                     buffer))
                  ((symbol-function 'e-chat-session-submit)
                   (lambda (&rest _args) "turn-1")))
          (unwind-protect
              (progn
                (e-org-canvas--prompt-scope 'thread)
                (with-current-buffer input
                  (goto-char (point-max))
                  (insert "expand this")
                  (e-org-canvas-input-submit))
                (e-harness--emit
                 harness
                 (e-events-make
                  :type 'turn-finished
                  :session-id "session-1"
                  :turn-id "turn-1"
                  :created-at 0
                  :payload '(:reason done)))
                (should-not mark-active))
            (when (buffer-live-p input)
              (kill-buffer input))))))))

(ert-deftest e-org-canvas-test-input-escape_closes_composer_without_changing_chat ()
  "Esc closes Org Canvas input while leaving normal chat Esc navigation intact."
  (let ((target (get-buffer-create "org-canvas-escape-target"))
        input)
    (unwind-protect
        (progn
          (with-current-buffer target
            (org-mode)
            (insert "* Topic\n"))
          (setq input
                (e-org-canvas--input-buffer
                 :harness nil
                 :session-id "session-1"
                 :scope 'document
                 :target-buffer target))
          (should (eq (lookup-key e-chat-mode-map (kbd "<escape>"))
                      'e-chat-enter-response-navigation))
          (should (eq (lookup-key e-org-canvas-input-mode-map (kbd "<escape>"))
                      'e-org-canvas-input-cancel))
          (set-window-buffer (selected-window) input)
          (with-current-buffer input
            (goto-char (point-max))
            (insert "discard this")
            (call-interactively
             (lookup-key e-org-canvas-input-mode-map (kbd "<escape>"))))
          (should-not (buffer-live-p input))
          (should (eq (window-buffer (selected-window)) target))
          (should (eq (lookup-key e-chat-mode-map (kbd "<escape>"))
                      'e-chat-enter-response-navigation)))
      (when (buffer-live-p input)
        (kill-buffer input))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest e-org-canvas-test-input-submit_uses_chat_composer_and_records_metadata ()
  "Submitting the transient composer records Org Canvas turn metadata."
  (let* ((harness (e-org-canvas-test--harness))
         call
         buffer)
    (e-harness-create-session harness :id "session-1")
    (unwind-protect
        (with-temp-buffer
          (rename-buffer "org-canvas-submit-input" t)
          (org-mode)
          (insert "* Topic\nBody\n")
          (e-org-canvas--mark-session
           harness "session-1" (current-buffer)
           :scope 'thread :target-folder nil)
          (setq buffer
                (e-org-canvas--input-buffer
                 :harness harness
                 :session-id "session-1"
                 :scope 'thread
                 :target-buffer (current-buffer)))
          (cl-letf (((symbol-function 'e-chat-session-submit)
                     (lambda (&rest args)
                       (setq call args)
                       "turn-1"))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (&rest _args) nil)))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "expand this")
              (e-org-canvas-input-submit)
              (should (equal e-org-canvas-input--active-turn-id "turn-1")))
            (should (string-match-p "expand this" (nth 2 call)))
            (let ((references (plist-get (nthcdr 3 call) :references))
                  (metadata (plist-get (nthcdr 3 call) :metadata)))
              (should (= (length references) 1))
              (should (equal (plist-get metadata :org-canvas-scope) 'thread))
              (should (plist-get metadata :org-canvas-focus)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-org-canvas-test-input-submit_replays_synchronous_progress ()
  "Progress emitted during submit remains visible after the composer is removed."
  (let* ((harness (e-org-canvas-test--harness))
         buffer)
    (e-harness-create-session harness :id "session-1")
    (unwind-protect
        (with-temp-buffer
          (rename-buffer "org-canvas-submit-progress" t)
          (org-mode)
          (insert "* Topic\nBody\n")
          (e-org-canvas--mark-session
           harness "session-1" (current-buffer)
           :scope 'thread :target-folder nil)
          (setq buffer
                (e-org-canvas--input-buffer
                 :harness harness
                 :session-id "session-1"
                 :scope 'document
                 :target-buffer (current-buffer)))
          (cl-letf (((symbol-function 'e-chat-session-submit)
                     (lambda (&rest _args)
                       (e-harness--emit
                        harness
                        (e-events-make
                         :type 'turn-started
                         :session-id "session-1"
                         :turn-id "turn-1"
                         :created-at 0))
                       (e-harness--emit
                        harness
                        (e-events-make
                         :type 'provider-request-started
                         :session-id "session-1"
                         :turn-id "turn-1"
                         :created-at 0
                         :payload '(:status started)))
                       "turn-1"))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (&rest _args) nil)))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "expand this")
              (e-org-canvas-input-submit)
              (e-chat--run-pending-activity-redraw)
              (should (equal e-org-canvas-input--active-turn-id "turn-1"))
              (should-not (e-chat--composer-active-p))
              (should (string-match-p "Thinking" (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-org-canvas-test-input-progress_keeps_latest_status_visible ()
  "Submitted input panes keep the newest progress visible in their window."
  (let* ((harness (e-org-canvas-test--harness))
         (target (get-buffer-create "org-canvas-progress-target"))
         input
         window
         original-window
         original-buffer)
    (e-harness-create-session harness :id "session-1")
    (unwind-protect
        (progn
          (setq original-window (selected-window))
          (setq original-buffer (window-buffer original-window))
          (delete-other-windows)
          (with-current-buffer target
            (org-mode)
            (erase-buffer)
            (insert "* Topic\nBody\n"))
          (setq input
                (e-org-canvas--input-buffer
                 :harness harness
                 :session-id "session-1"
                 :scope 'document
                 :target-buffer target))
          (setq window (split-window (selected-window) -4 'below))
          (set-window-buffer window input)
          (with-current-buffer input
            (setq-local e-org-canvas-input--active-turn-id "turn-1")
            (e-org-canvas--input-enter-result-state))
          (e-harness--emit
           harness
           (e-events-make
            :type 'turn-started
            :session-id "session-1"
            :turn-id "turn-1"
            :created-at 0))
          (dotimes (index 8)
            (e-harness--emit
             harness
             (e-events-make
              :type 'reasoning-delta
              :session-id "session-1"
              :turn-id "turn-1"
              :created-at (1+ index)
              :payload (list :type 'reasoning-delta
                             :content (format "step-%d\n" index)))))
          (with-current-buffer input
            (e-chat--run-pending-activity-redraw))
          (with-current-buffer input
            (should (save-excursion
                      (goto-char (window-start window))
                      (search-forward "step-7" (window-end window t) t)))))
      (when (window-live-p window)
        (delete-window window))
      (when (window-live-p original-window)
        (select-window original-window)
        (when (buffer-live-p original-buffer)
          (set-window-buffer original-window original-buffer)))
      (dolist (buffer (list input target))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest e-org-canvas-test-input-pane_shows_done_on_terminal_turn_without_final_message ()
  "Submitted input panes show a done line when a turn has no assistant output."
  (let* ((harness (e-org-canvas-test--harness))
         (buffer (e-org-canvas--input-buffer
                  :harness harness
                  :session-id "session-1"
                  :scope 'thread
                  :target-buffer (current-buffer))))
    (unwind-protect
        (let (subscription)
          (with-current-buffer buffer
            (setq subscription e-org-canvas-input--subscription)
            (setq-local e-org-canvas-input--active-turn-id "turn-1"))
          (e-harness--emit
           harness
           (e-events-make
            :type 'turn-started
            :session-id "session-1"
            :turn-id "turn-1"
            :created-at 0))
          (e-harness--emit
           harness
           (e-events-make
            :type 'turn-finished
            :session-id "session-1"
            :turn-id "turn-1"
            :payload '(:reason done)))
          (with-current-buffer buffer
            (should-not (string-match-p "Status:" (buffer-string)))
            (should-not (e-chat--composer-active-p))
            (should (string-match-p "✓ Done" (buffer-string)))
            (should (timerp e-org-canvas-input--close-timer)))
          (should-not (memq subscription (e-harness-subscribers harness))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-org-canvas-test-input-pane_schedules_final_message_auto_close ()
  "Assistant output remains briefly in the input pane before auto-close."
  (let* ((harness (e-org-canvas-test--harness))
         (target (get-buffer-create "org-canvas-result-target"))
         (buffer (e-org-canvas--input-buffer
                  :harness harness
                  :session-id "session-1"
                  :scope 'thread
                  :target-buffer target)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local e-org-canvas-input--active-turn-id "turn-1")
            (e-chat--delete-composer))
          (e-harness--emit
           harness
           (e-events-make
            :type 'turn-started
            :session-id "session-1"
            :turn-id "turn-1"
            :created-at 0))
          (e-harness--emit
           harness
           (e-events-make
            :type 'message-added
            :session-id "session-1"
            :turn-id "turn-1"
            :created-at 1
            :payload '(:message (:role assistant
                                  :content "Here is the result."))))
          (e-harness--emit
           harness
           (e-events-make
            :type 'turn-finished
            :session-id "session-1"
            :turn-id "turn-1"
            :created-at 2
            :payload '(:reason done)))
          (with-current-buffer buffer
            (should (string-match-p "Here is the result." (buffer-string)))
            (should-not (string-match-p "✓ Done" (buffer-string)))
            (should-not (e-chat--composer-active-p))
            (should (timerp e-org-canvas-input--close-timer)))
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (call-interactively #'e-org-canvas-input-close-result))
          (should-not (buffer-live-p buffer))
          (should (eq (window-buffer (selected-window)) target)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest e-org-canvas-test-input-pane_follows_bottom_on_timer_redraw ()
  "Timer-driven running-status redraws keep the input pane pinned to output.
Regression: progress redraws bypass harness event dispatch, so the pane
relied on `e-chat--running-status-rendered-hook' to follow the bottom."
  (let* ((harness (e-org-canvas-test--harness))
         (target (get-buffer-create "org-canvas-follow-target"))
         (buffer (e-org-canvas--input-buffer
                  :harness harness
                  :session-id "session-1"
                  :scope 'thread
                  :target-buffer target)))
    (unwind-protect
        (progn
          ;; Hook is wired buffer-locally for the input pane.
          (with-current-buffer buffer
            (should (memq #'e-org-canvas--input-follow-bottom-on-redraw
                          e-chat--running-status-rendered-hook)))
          ;; Display the pane in a NON-selected window: only then does
          ;; `window-point' stay decoupled from buffer point, so the test
          ;; observes the follow hook rather than incidental point movement.
          (let* ((root (selected-window))
                 (window (split-window root nil 'below)))
            (set-window-buffer window buffer)
            (with-current-buffer buffer
              ;; Result state: composer is gone and its reinsertion is
              ;; inhibited, so only the follow hook can move the window.
              (e-org-canvas--input-enter-result-state)
              (setq-local e-org-canvas-input--active-turn-id "turn-1")
              ;; Mark an active progress turn so the redraw emits a line.
              (setq-local e-chat--progress-turn-id "turn-1")
              (setq-local e-chat--progress-frame 0)
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert (make-string 200 ?\n)))
              ;; Park the window at the very top, away from the output tail.
              (set-window-point window (point-min))
              (set-window-start window (point-min))
              (e-chat--render-running-status
               "turn-1"
               (e-chat--turn-record "turn-1"))
              ;; Follow hook should have dragged the window to the last line.
              (let ((bottom (save-excursion
                              (goto-char (point-max))
                              (skip-chars-backward "\n")
                              (line-beginning-position))))
                (should (>= (window-point window) bottom))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest e-org-canvas-test-input-cancel-aborts-active-tool-request ()
  "Cancelling a submitted input pane aborts the active Org Canvas turn."
  (let* ((e-chat-submit-backend-delay 0)
         (tool-callbacks nil)
         (tool-cancelled nil)
         (backend
          (e-backend-create
           :name "org-canvas-tool-abort"
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
         (target (get-buffer-create "org-canvas-abort-target"))
         (input nil))
    (e-tools-register
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
        (progn
          (e-harness-create-session harness :id "session-1")
          (with-current-buffer target
            (org-mode)
            (erase-buffer)
            (insert "* Topic\nBody\n")
            (e-org-canvas--mark-session
             harness "session-1" target :scope 'thread :target-folder nil))
          (setq input
                (e-org-canvas--input-buffer
                 :harness harness
                 :session-id "session-1"
                 :scope 'document
                 :target-buffer target))
          (cl-letf (((symbol-function 'e-harness-tools)
                     (lambda (_harness &optional _session-id _turn-id) tools))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (&rest _args) nil)))
            (with-current-buffer input
              (goto-char (point-max))
              (insert "run held tool")
              (e-org-canvas-input-submit)))
          (should tool-callbacks)
          (with-current-buffer input
            (e-org-canvas-input-cancel))
          (funcall (plist-get tool-callbacks :on-done) "late result")
          (should (equal (plist-get
                          (e-harness-wait harness "session-1" 0.1)
                          :status)
                         'cancelled))
          (should tool-cancelled))
      (when (buffer-live-p input)
        (kill-buffer input))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest e-org-canvas-test-reopen-last-prompt-restores_scope_and_text ()
  "Reopening a prior prompt restores its scope and draft body."
  (let ((harness (e-org-canvas-test--harness))
        input)
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user
       :content "revise the outline"
       :metadata (:org-canvas-scope document)))
    (with-temp-buffer
      (org-mode)
      (let ((target (current-buffer)))
        (cl-letf (((symbol-function 'e-org-canvas--ensure-current-session)
                   (lambda () (list harness "session-1" target)))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _args)
                     (setq input buffer)
                     (set-window-buffer (selected-window) buffer)
                     (selected-window))))
          (e-org-canvas-reopen-last-prompt))))
    (unwind-protect
        (with-current-buffer input
          (should (derived-mode-p 'e-org-canvas-input-mode))
          (should (equal e-org-canvas-input--scope 'document))
          (should (equal (e-chat--composer-text)
                         "revise the outline")))
      (when (buffer-live-p input)
        (kill-buffer input)))))

(ert-deftest e-org-canvas-test-new-buffer_remembers_project_folder_defaults ()
  "New unsaved Org canvases remember the chosen folder per project."
  (let ((root (file-name-as-directory (make-temp-file "e-org-canvas-root-" t)))
        (folder (file-name-as-directory (make-temp-file "e-org-canvas-folder-" t))))
    (unwind-protect
        (let ((e-org-canvas--project-folders (make-hash-table :test 'equal)))
          (should (equal (e-org-canvas--project-folder-default root) root))
          (e-org-canvas--remember-project-folder root folder)
          (should (equal (e-org-canvas--project-folder-default root) folder)))
      (delete-directory root t)
      (delete-directory folder t))))

(ert-deftest e-org-canvas-test-first_prompt_saves_safe_suggested_name ()
  "The first prompt for a new unsaved canvas saves a safe suggested Org file."
  (let ((directory (file-name-as-directory
                    (make-temp-file "e-org-canvas-target-" t)))
        (harness (e-org-canvas-test--harness)))
    (unwind-protect
        (with-temp-buffer
          (rename-buffer "unsaved-org-canvas" t)
          (org-mode)
          (e-harness-create-session harness :id "session-1")
          (e-org-canvas--mark-session
           harness "session-1" (current-buffer)
           :scope 'thread
           :target-folder directory
           :needs-file-name t)
          (cl-letf (((symbol-function 'e-org-canvas--suggest-file-name)
                     (lambda (_harness _session-id _prompt _buffer)
                       "Project Notes")))
            (e-org-canvas--maybe-save-new-buffer
             harness "session-1" "make project notes"))
          (should (equal (file-name-nondirectory buffer-file-name)
                         "project-notes.org"))
          (should (file-exists-p buffer-file-name))
          (let* ((metadata (plist-get
                            (e-session-get (e-harness-sessions harness)
                                           "session-1")
                            :metadata))
                 (org-canvas (plist-get metadata :org-canvas))
                 (attachment (car (e-chat-session-attachments
                                   harness "session-1"))))
            (should-not (plist-get org-canvas :needs-file-name))
            (should (string-prefix-p "file://" (plist-get org-canvas :uri)))
            (should (equal (plist-get attachment :uri)
                           (plist-get org-canvas :uri)))))
      (when-let ((buffer (find-buffer-visiting
                          (expand-file-name "project-notes.org" directory))))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-org-canvas-test-first_prompt_asks_backend_for_file_name ()
  "The first prompt for a new unsaved canvas asks the backend for a file name."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "e-org-canvas-target-" t)))
         captured-messages
         (backend
          (e-backend-create
           :name "file-name-suggester"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore options)
              (setq captured-messages messages)
              (funcall on-item
                       '(:type assistant-message
                         :content "knowledge-agenda-system"))))))
         (harness (e-org-canvas-test--harness)))
    (setf (e-harness-backend harness) backend)
    (unwind-protect
        (with-temp-buffer
          (rename-buffer "unsaved-org-canvas-backend" t)
          (org-mode)
          (e-harness-create-session harness :id "session-1")
          (e-org-canvas--mark-session
           harness "session-1" (current-buffer)
           :scope 'thread
           :target-folder directory
           :needs-file-name t)
          (e-org-canvas--maybe-save-new-buffer
           harness
           "session-1"
           "Let's design a knowledge and agenda management system for the repo")
          (should (equal (file-name-nondirectory buffer-file-name)
                         "knowledge-agenda-system.org"))
          (should captured-messages)
          (should (string-match-p
                   "knowledge and agenda management system"
                   (plist-get (cadr captured-messages) :content))))
      (when-let ((buffer (find-buffer-visiting
                          (expand-file-name
                           "knowledge-agenda-system.org"
                           directory))))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-org-canvas-test-first_prompt_empty_backend_does_not_use_prompt_as_file_name ()
  "Empty backend suggestions fall back locally instead of slugging the prompt."
  (let ((directory (file-name-as-directory
                    (make-temp-file "e-org-canvas-target-" t)))
        (harness (e-org-canvas-test--harness)))
    (unwind-protect
        (with-temp-buffer
          (rename-buffer "unsaved-org-canvas-empty-backend" t)
          (org-mode)
          (e-harness-create-session harness :id "session-1")
          (e-org-canvas--mark-session
           harness "session-1" (current-buffer)
           :scope 'thread
           :target-folder directory
           :needs-file-name t)
          (e-org-canvas--maybe-save-new-buffer
           harness
           "session-1"
           "Let's design a knowledge and agenda management system for the repo")
          (should (equal (file-name-nondirectory buffer-file-name)
                         "org-canvas.org")))
      (when-let ((buffer (find-buffer-visiting
                          (expand-file-name "org-canvas.org" directory))))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-org-canvas-test-first_prompt_file_name_request_omits_tools ()
  "File name suggestion requests use the session model options without tools."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "e-org-canvas-target-" t)))
         backend-called
         captured-options
         (backend
          (e-backend-create
           :name "file-name-suggester"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages)
              (setq backend-called t)
              (setq captured-options options)
              (funcall on-item
                       '(:type assistant-message
                         :content "tool-free-title"))))))
         (harness (e-org-canvas-test--harness t)))
    (setf (e-harness-backend harness) backend)
    (unwind-protect
        (with-temp-buffer
          (rename-buffer "unsaved-org-canvas-no-tools" t)
          (org-mode)
          (e-harness-create-session harness :id "session-1")
          (e-harness-set-session-model harness "session-1" "model-for-names")
          (e-org-canvas--mark-session
           harness "session-1" (current-buffer)
           :scope 'thread
           :target-folder directory
           :needs-file-name t)
          (should (plist-get
                   (e-harness-turn-options harness "session-1")
                   :tools))
          (e-org-canvas--maybe-save-new-buffer
           harness
           "session-1"
           "Name this canvas")
          (should (equal (file-name-nondirectory buffer-file-name)
                         "tool-free-title.org"))
          (should backend-called)
          (should (equal (plist-get captured-options :model)
                         "model-for-names"))
          (should-not (plist-get captured-options :tools)))
      (when-let ((buffer (find-buffer-visiting
                          (expand-file-name "tool-free-title.org" directory))))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-org-canvas-test-unsafe_or_existing_file_names_require_confirmation ()
  "Unsafe, ambiguous, or overwrite-prone file suggestions are not auto-saved."
  (let ((directory (file-name-as-directory
                    (make-temp-file "e-org-canvas-target-" t))))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "taken.org" directory)
                        nil 'silent)
          (should-not
           (e-org-canvas--safe-suggested-file
            directory "../escape.org"))
          (should-not
           (e-org-canvas--safe-suggested-file
            directory "/tmp/absolute.org"))
          (should-not
           (e-org-canvas--safe-suggested-file
            directory "taken.org"))
          (should
           (equal (file-name-nondirectory
                   (e-org-canvas--safe-suggested-file
                    directory "Meeting Notes"))
                  "meeting-notes.org")))
      (delete-directory directory t))))

(ert-deftest e-org-canvas-test-manual_file_selection_stays_in_target_folder ()
  "Manual file fallback accepts only direct .org children of the target folder."
  (let ((directory (file-name-as-directory
                    (make-temp-file "e-org-canvas-target-" t)))
        (outside (file-name-as-directory
                  (make-temp-file "e-org-canvas-outside-" t))))
    (unwind-protect
        (progn
          (should (equal (e-org-canvas--manual-target-file
                          directory
                          (expand-file-name "manual.org" directory))
                         (expand-file-name "manual.org" directory)))
          (should-error
           (e-org-canvas--manual-target-file
            directory
            (expand-file-name "escape.org" outside))
           :type 'user-error)
          (should-error
           (e-org-canvas--manual-target-file
            directory
            "../escape.org")
           :type 'user-error)
          (should-error
           (e-org-canvas--manual-target-file
            directory
            "manual.txt")
           :type 'user-error))
      (delete-directory directory t)
      (delete-directory outside t))))

(ert-deftest e-org-canvas-test-first_prompt_rejects_unsafe_manual_file_fallback ()
  "Unsafe model suggestions cannot fall back to arbitrary manual write paths."
  (let ((directory (file-name-as-directory
                    (make-temp-file "e-org-canvas-target-" t)))
        (outside (file-name-as-directory
                  (make-temp-file "e-org-canvas-outside-" t)))
        (harness (e-org-canvas-test--harness)))
    (unwind-protect
        (with-temp-buffer
          (rename-buffer "unsaved-org-canvas-unsafe-manual" t)
          (org-mode)
          (e-harness-create-session harness :id "session-1")
          (e-org-canvas--mark-session
           harness "session-1" (current-buffer)
           :scope 'thread
           :target-folder directory
           :needs-file-name t)
          (cl-letf (((symbol-function 'e-org-canvas--suggest-file-name)
                     (lambda (_harness _session-id _prompt _buffer)
                       "../escape"))
                    ((symbol-function 'read-file-name)
                     (lambda (&rest _args)
                       (expand-file-name "escape.org" outside))))
            (should-error
             (e-org-canvas--maybe-save-new-buffer
              harness "session-1" "save this")
             :type 'user-error))
          (should-not buffer-file-name))
      (delete-directory directory t)
      (delete-directory outside t))))

(ert-deftest e-org-canvas-test-session_candidates_filter_by_file_and_project ()
  "Org Canvas session discovery can filter by file URI and project root."
  (let ((directory (make-temp-file "e-org-canvas-project-" t))
        (harness (e-org-canvas-test--harness)))
    (unwind-protect
        (let* ((file-a (e-org-canvas-test--org-file directory "a.org"))
               (file-b (e-org-canvas-test--org-file directory "b.org")))
          (e-org-canvas-test--session-with-file harness "a-1" file-a)
          (e-org-canvas-test--session-with-file harness "a-2" file-a)
          (e-org-canvas-test--session-with-file harness "b-1" file-b)
          (should (equal (sort (mapcar (lambda (session)
                                          (plist-get session :id))
                                        (e-org-canvas--session-candidates
                                         harness :file file-a))
                                #'string<)
                         '("a-1" "a-2")))
          (should (equal (sort (mapcar #'car
                                       (e-org-canvas--sessions-by-file
                                        harness
                                        :project-root directory))
                               #'string<)
                         (sort (list (concat "file://" file-a)
                                     (concat "file://" file-b))
                               #'string<))))
      (dolist (buffer (buffer-list))
        (when (and (buffer-file-name buffer)
                   (file-in-directory-p (buffer-file-name buffer) directory))
          (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest e-org-canvas-test-resume_reopens_file_and_enables_mode ()
  "Resuming an Org Canvas session visits the file and restores presentation state."
  (let ((directory (make-temp-file "e-org-canvas-resume-" t))
        (harness (e-org-canvas-test--harness)))
    (unwind-protect
        (let ((file (e-org-canvas-test--org-file directory "resume.org")))
          (e-org-canvas-test--session-with-file harness "session-1" file)
          (when-let ((buffer (find-buffer-visiting file)))
            (kill-buffer buffer))
          (let ((buffer (e-org-canvas-resume-session harness "session-1")))
            (should (buffer-live-p buffer))
            (should (eq (window-buffer (selected-window)) buffer))
            (with-current-buffer buffer
              (should (derived-mode-p 'org-mode))
              (should e-org-canvas-mode))))
      (dolist (buffer (buffer-list))
        (when (and (buffer-file-name buffer)
                   (file-in-directory-p (buffer-file-name buffer) directory))
          (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest e-org-canvas-test-visibility_tools_operate_on_session_canvas ()
  "Org visibility tools operate on the session's live Org canvas buffer."
  (let ((harness (e-org-canvas-test--harness t)))
    (with-temp-buffer
      (rename-buffer "org-canvas-tools" t)
      (org-mode)
      (insert "* Parent\n** Child\nBody\n* Other\n")
      (goto-char (point-min))
      (e-harness-create-session harness :id "org")
      (e-org-canvas--mark-session
       harness "org" (current-buffer) :scope 'thread :target-folder nil)
      (let ((tools (e-harness-tools harness "org" nil)))
        (e-tools-execute tools '(:name "org_canvas_overview" :arguments nil))
        (should (eq (get-char-property (line-end-position) 'invisible)
                    'outline))
        (e-tools-execute tools '(:name "org_canvas_show_all" :arguments nil))
        (should-not (get-char-property (line-end-position) 'invisible))
        (let ((state (e-tools-execute
                      tools
                      '(:name "org_canvas_visibility_state"
                        :arguments nil))))
          (should (string-match-p "Parent"
                                  (e-tools-result-content-text state))))))))

(ert-deftest e-org-canvas-test-visibility_tools_accept_heading_path_targets ()
  "Org visibility tools can target headings by path as well as point."
  (let ((harness (e-org-canvas-test--harness t)))
    (with-temp-buffer
      (rename-buffer "org-canvas-heading-path-tools" t)
      (org-mode)
      (insert "* Parent\n** Child\nBody\n* Other\n")
      (goto-char (point-min))
      (e-harness-create-session harness :id "org")
      (e-org-canvas--mark-session
       harness "org" (current-buffer) :scope 'thread :target-folder nil)
      (let ((tools (e-harness-tools harness "org" nil)))
        (e-tools-execute
         tools
         '(:name "org_canvas_cycle_heading"
           :arguments (:heading_path ("Parent" "Child")
                       :operation "hide")))
        (goto-char (point-min))
        (re-search-forward "^\\*\\* Child")
        (should (eq (get-char-property (line-end-position) 'invisible)
                    'outline))
        (let ((result
               (e-tools-execute
                tools
                '(:name "org_canvas_show_context"
                  :arguments (:heading_path ("Parent" "Child"))))))
          (should (string-match-p
                   "Revealed Org Canvas context"
                   (e-tools-result-content-text result)))
          (should-not (get-char-property (line-end-position) 'invisible)))))))

(ert-deftest e-org-canvas-test-visibility_tools_fail_outside_org_canvas ()
  "Org visibility tools report explicit errors for ordinary chat sessions."
  (let ((harness (e-org-canvas-test--harness t)))
    (e-harness-create-session harness :id "plain")
    (let* ((tools (e-harness-tools harness "plain" nil))
           (result (e-tools-execute
                    tools
                    '(:name "org_canvas_show_all" :arguments nil))))
      (should (eq (plist-get result :status) 'error))
      (should (string-match-p "Not an Org Canvas session"
                              (e-tools-result-content-text result))))))

(ert-deftest e-org-canvas-test-shell_and_default_layer_registration ()
  "Org Canvas is discoverable as a shell and default gated layer."
  (should (memq 'org-canvas
                (mapcar (lambda (spec) (plist-get spec :id))
                        e-default-layer-specs)))
  (should (memq 'org-canvas e-default-chat-layer-ids))
  (let* ((shell (e-org-canvas-shell))
         (command-ids (mapcar #'e-shell-command-id
                              (e-shell-commands shell))))
    (should (eq (e-shell-id shell) 'org-canvas))
    (should (equal (e-shell-required-capabilities shell)
                   '(chat-session org-canvas)))
    (dolist (command-id '(open-for-current-buffer
                          new-file
                          new-buffer
                          prompt-thread
                          prompt-document
                          prompt
                          reopen-last-prompt
                          respond-to-threads
                          list-sessions
                          list-project-sessions
                          resume))
      (should (memq command-id command-ids))))
  (should (eq (e-shell-id (e-shell-get 'org-canvas)) 'org-canvas)))

(ert-deftest e-org-canvas-test-open-threads-filters-resolved ()
  "Only threads lacking a verdict and terminal status are treated as open."
  (should (e-org-canvas--thread-open-p '(:thread-id "a" :verdict nil
                                         :status "open")))
  (should (e-org-canvas--thread-open-p '(:thread-id "b" :verdict nil
                                         :status "in-progress")))
  (should-not (e-org-canvas--thread-open-p '(:thread-id "c"
                                             :verdict "accepted"
                                             :status "open")))
  (should-not (e-org-canvas--thread-open-p '(:thread-id "d" :verdict nil
                                             :status "resolved")))
  (should-not (e-org-canvas--thread-open-p '(:thread-id "e" :verdict nil
                                             :status "closed"))))

(ert-deftest e-org-canvas-test-threads-prompt-enumerates-open-threads ()
  "The seeded prompt lists each open thread with its id, region, and text."
  (let ((prompt (e-org-canvas--threads-prompt
                 "/tmp/notes.org"
                 '((:thread-id "t-1" :start 5 :end 9 :proposal "tighten intro")
                   (:thread-id "t-2" :start 20 :end 30 :proposal nil)))))
    (should (string-match-p "Open threads (2)" prompt))
    (should (string-match-p "thread t-1 \\[chars 5\\.\\.9\\]: tighten intro"
                            prompt))
    (should (string-match-p "thread t-2 \\[chars 20\\.\\.30\\]: (no text)"
                            prompt))
    (should (string-match-p "annotation_resolve" prompt))))

(ert-deftest e-org-canvas-test-respond-to-threads-seeds-document-prompt ()
  "Responding to threads opens a document-scoped pane listing open threads."
  (let ((harness (e-org-canvas-test--harness))
        input)
    (e-harness-create-session harness :id "session-1")
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/tmp/e-org-canvas-threads.org")
      (let ((target (current-buffer)))
        (cl-letf (((symbol-function 'e-org-canvas--ensure-current-session)
                   (lambda () (list harness "session-1" target)))
                  ((symbol-function 'e-org-canvas--open-threads)
                   (lambda (_file)
                     '((:thread-id "t-1" :start 1 :end 4
                        :proposal "clarify scope"))))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _args)
                     (setq input buffer)
                     (set-window-buffer (selected-window) buffer)
                     (selected-window))))
          (e-org-canvas-respond-to-threads))))
    (unwind-protect
        (with-current-buffer input
          (should (derived-mode-p 'e-org-canvas-input-mode))
          (should (equal e-org-canvas-input--scope 'document))
          (should (string-match-p "clarify scope" (e-chat--composer-text))))
      (when (buffer-live-p input)
        (kill-buffer input)))))

(ert-deftest e-org-canvas-test-respond-to-threads-requires-saved-file ()
  "An unsaved canvas has no file-keyed threads to answer."
  (let ((harness (e-org-canvas-test--harness)))
    (e-harness-create-session harness :id "session-1")
    (with-temp-buffer
      (org-mode)
      (let ((target (current-buffer)))
        (cl-letf (((symbol-function 'e-org-canvas--ensure-current-session)
                   (lambda () (list harness "session-1" target))))
          (should-error (e-org-canvas-respond-to-threads)
                        :type 'user-error))))))

(provide 'e-org-canvas-test)

;;; e-org-canvas-test.el ends here
