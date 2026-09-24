;;; e-canvas-test.el --- Tests for e canvas shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the canvas presentation shell.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-canvas)
(require 'e-chat-session)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-session-async)
(require 'e-session-sqlite)
(require 'e-shells)
(require 'e-work)

(defmacro e-canvas-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal)))
     ,@body))

(defun e-canvas-test--harness (&optional sessions)
  "Return a fake harness with chat-session capability and SESSIONS."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil)
                  :sessions sessions)))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    harness))

(defun e-canvas-test--await (work)
  "Observe request-scoped WORK from this explicit test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 5.0)))

(cl-defmacro e-canvas-test--with-sqlite-harness ((harness store) &rest body)
  "Run BODY with a disposable SQLite STORE and public chat HARNESS."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((directory (make-temp-file "e-canvas-test-store-" t))
          (,store (e-session-sqlite-store-create directory))
          (,harness (e-canvas-test--harness ,store)))
     (unwind-protect
         (progn
           (e-session-enable ,store)
           ,@body)
       (e-canvas-test--kill-chat-buffers)
       (ignore-errors (e-session-sqlite-store-close ,store))
       (delete-directory directory t))))

(defun e-canvas-test--create-sql-session (harness session-id metadata)
  "Create and admit SESSION-ID with detached METADATA in HARNESS."
  (e-chat-service-create-session-start
   :harness harness :id session-id :metadata metadata)
  (e-canvas-test--await
   (e-chat-service-binding-start harness session-id nil t)))

(defun e-canvas-test--chat-metadata (chat-buffer store)
  "Return CHAT-BUFFER's detached SQLite metadata from STORE."
  (with-current-buffer chat-buffer
    (when (e-work-handle-p e-chat--session-readiness-work)
      (e-canvas-test--await e-chat--session-readiness-work))
    (plist-get
     (e-canvas-test--await
      (e-session-async-session-metadata store e-chat-session-id))
     :metadata)))

(defun e-canvas-test--kill-chat-buffers ()
  "Kill all live e chat buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'e-chat-mode)
          (kill-buffer buffer))))))

(ert-deftest e-canvas-test-new-persistent-session-defers-dependent-mutations-until-admission ()
  "Canvas attachment and metadata wait for the new-session commit acknowledgement."
  (let* ((creation
          (e-work-start
           (e-work-spec-create
            :id "canvas-test-pending-create" :execution 'cooperative
            :interactive-policy 'async :owner 'e-canvas-test
            :runner (lambda (_handle _arguments _context) :deferred))
           nil))
         (source (generate-new-buffer " *e-canvas-causal-source*"))
         (chat (generate-new-buffer " *e-canvas-causal-chat*"))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         calls open-arguments
         (kind
          (e-canvas-kind-create
           :name "causal-canvas"
           :harness-function (lambda (_buffer) harness)
           :prepare-buffer-function #'ignore
           :prepare-harness-function (lambda (value _buffer _options) value)
           :attachment-function
           (lambda (_buffer)
             (push 'attachment calls)
             '(:uri "buffer://causal" :canvas t))
           :session-reference-function (lambda (&rest _) nil)
           :initialize-session-function
           (lambda (_harness _session-id _buffer _options)
             (push 'initialize calls))
           :bind-session-function
           (lambda (_harness _session-id _buffer _options)
             (push 'bind calls))
           :present-session-function
           (lambda (_buffer returned-chat _display)
             (push 'present calls)
             returned-chat))))
    (unwind-protect
        (cl-letf (((symbol-function 'e-chat-open)
                   (lambda (&rest arguments)
                     (setq open-arguments arguments)
                     (with-current-buffer chat
                       (setq-local e-chat--session-readiness-work creation))
                     chat))
                  ((symbol-function 'e-chat-session-attach-context)
                   (lambda (&rest _arguments)
                     (push 'attach calls))))
          (should (eq (e-canvas--create-and-open-session
                       kind harness source nil t)
                      chat))
          (should (equal (reverse (copy-sequence calls))
                         '(attachment bind present)))
          (should
           (equal (plist-get (plist-get open-arguments :metadata) :project-root)
                  (file-name-as-directory
                   (expand-file-name
                    (with-current-buffer source default-directory)))))
          (e-work-finish creation '(:session-id "causal-session"))
          (should (equal (reverse (copy-sequence calls))
                         '(attachment bind present attach initialize))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat)))))

(ert-deftest e-canvas-test-new-file-session-project-root-comes-from-canvas-buffer ()
  "Ambient directories cannot leak into a file-backed Canvas admission."
  (let* ((project (make-temp-file "e-canvas-project-" t))
         (nested (expand-file-name "daily" project))
         (file (expand-file-name "today.org" nested))
         (source (find-file-noselect file))
         (ambient (make-temp-file "e-canvas-ambient-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" project))
          (with-current-buffer source
            (setq default-directory (file-name-as-directory nested)))
          (let ((default-directory (file-name-as-directory ambient)))
            (should
             (equal
              (plist-get (e-canvas--initial-session-metadata source)
                         :project-root)
              (file-name-as-directory project)))))
      (when (buffer-live-p source) (kill-buffer source))
      (delete-directory project t)
      (delete-directory ambient t))))

(ert-deftest e-canvas-test-open-current-buffer-creates-canvas-session ()
  "Opening from the current buffer creates a chat session with canvas context."
  (e-canvas-test--with-sqlite-harness (harness store)
    (e-canvas-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :canvas-test))
        (e-harness-registry-register :canvas-test harness)
        (with-temp-buffer
          (rename-buffer "canvas-source" t)
          (insert "canvas body")
          (let* ((chat-buffer (e-canvas-open-for-current-buffer))
                 (metadata (e-canvas-test--chat-metadata chat-buffer store))
                 (attachment
                  (car (e-chat-session-metadata-attachments metadata))))
            (should (buffer-live-p chat-buffer))
            (with-current-buffer chat-buffer
              (should (derived-mode-p 'e-chat-mode)))
            (should (plist-get attachment :canvas))
            (should (equal (plist-get attachment :uri)
                           "buffer://canvas-source"))
            (should (string-match-p
                     "canvas body"
                     (e-chat-session--attachment-content attachment)))))))))

(ert-deftest e-canvas-test-open-current-buffer-reveals-existing-canvas-session ()
  "Opening from an attached canvas buffer reuses the existing chat session."
  (e-canvas-test--with-sqlite-harness (harness store)
    (e-canvas-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :canvas-test))
        (e-harness-registry-register :canvas-test harness)
        (with-temp-buffer
          (rename-buffer "canvas-existing" t)
          (insert "canvas body")
          (let* ((attachment
                  (e-chat-session--normalize-attachment
                   (e-canvas--buffer-attachment (current-buffer)) t))
                 (metadata
                  (list :context-references
                        (list :chat-session
                              (list :attachments (list attachment))))))
            (e-canvas-test--create-sql-session harness "session-1" metadata)
            (setq-local e-canvas-harness harness)
            (setq-local e-canvas-session-id "session-1")
            (let ((chat-buffer (e-canvas-open-for-current-buffer)))
              (should (buffer-live-p chat-buffer))
              (e-canvas-test--chat-metadata chat-buffer store)
              (with-current-buffer chat-buffer
                (should (derived-mode-p 'e-chat-mode))
                (should (equal e-chat-session-id "session-1"))))))))))

(ert-deftest e-canvas-test-open-does-not-enumerate-durable-sessions ()
  "A generic Canvas open creates directly without a process-local catalog."
  (e-canvas-test--with-sqlite-harness (harness store)
    (e-canvas-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :canvas-test))
        (e-harness-registry-register :canvas-test harness)
        (with-temp-buffer
          (rename-buffer "uncatalogued-canvas" t)
          (cl-letf (((symbol-function 'e-harness-session-list)
                     (lambda (&rest _)
                       (error "Canvas enumerated a process-local catalog"))))
            (let ((chat-buffer (e-canvas-open-for-current-buffer)))
              (should (buffer-live-p chat-buffer))
              (e-canvas-test--chat-metadata chat-buffer store))))))))

(ert-deftest e-canvas-test-open-does-not-read-unrelated-local-session-state ()
  "A generic Canvas open does not inspect unrelated process-local state."
  (e-canvas-test--with-sqlite-harness (harness store)
    (e-canvas-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :canvas-test))
        (e-harness-registry-register :canvas-test harness)
        (with-temp-buffer
          (rename-buffer "clean-canvas" t)
          (cl-letf (((symbol-function 'e-session-local-state)
                     (lambda (&rest _)
                       (error "Canvas read process-local session state"))))
            (let ((chat-buffer (e-canvas-open-for-current-buffer)))
              (should (buffer-live-p chat-buffer))
              (e-canvas-test--chat-metadata chat-buffer store))))))))

(ert-deftest e-canvas-test-new-file-uses-file-backed-buffer-as-canvas ()
  "Creating a file canvas attaches its visited buffer and file URI."
  (let ((file (make-temp-file "e-canvas-" nil ".txt")))
    (unwind-protect
        (e-canvas-test--with-sqlite-harness (harness store)
          (e-canvas-test--with-empty-harness-registry
            (let ((e-chat-default-harness-id :canvas-test))
              (e-harness-registry-register :canvas-test harness)
              (write-region "file canvas body" nil file nil 'silent)
              (let* ((chat-buffer (e-canvas-new-file file))
                     (metadata (e-canvas-test--chat-metadata chat-buffer store))
                     (attachment
                      (car (e-chat-session-metadata-attachments metadata))))
                (should (plist-get attachment :canvas))
                (should (equal (plist-get attachment :uri)
                               (concat "file://" file)))
                (should (get-buffer (plist-get attachment :buffer-name)))))))
      (when-let* ((buffer (find-buffer-visiting file)))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest e-canvas-test-attach-can-target-new-session ()
  "Manual attachment always offers an explicit new-session target."
  (e-canvas-test--with-sqlite-harness (harness store)
    (e-canvas-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :canvas-test))
        (e-harness-registry-register :canvas-test harness)
        (e-canvas-test--create-sql-session harness "existing" nil)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _args)
                     (should (equal (car collection) "[New e session]"))
                     (car collection))))
          (with-temp-buffer
            (rename-buffer "canvas-new-target" t)
            (e-canvas-test--await (e-canvas-attach-current-buffer))
            (let ((chat-buffer
                   (seq-find
                    (lambda (buffer)
                      (with-current-buffer buffer
                        (and (derived-mode-p 'e-chat-mode)
                             (not (equal e-chat-session-id "existing")))))
                    (buffer-list))))
              (should (buffer-live-p chat-buffer))
              (let* ((metadata (e-canvas-test--chat-metadata chat-buffer store))
                     (attachment
                      (car (e-chat-session-metadata-attachments metadata))))
                (should (equal (plist-get attachment :uri)
                               "buffer://canvas-new-target"))))))))))

(ert-deftest e-canvas-test-attach-current-buffer-to-selected-sql-session ()
  "Manual attachment selects a detached SQL root and persists its context."
  (e-canvas-test--with-sqlite-harness (harness store)
    (e-canvas-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :canvas-test))
        (e-harness-registry-register :canvas-test harness)
        (e-canvas-test--create-sql-session
         harness "target-session" '(:name "Canvas Owner"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _args)
                     (should (= (length collection) 2))
                     (let ((target
                            (seq-find
                             (lambda (label)
                               (string-match-p "Canvas Owner" label))
                             collection)))
                       (should target)
                       target))))
          (with-temp-buffer
            (rename-buffer "canvas-extra" t)
            (insert "extra context")
            (e-canvas-test--await (e-canvas-attach-current-buffer))
            (let ((deadline (+ (float-time) 2.0))
                  attachment)
              (while (and (not attachment) (< (float-time) deadline))
                (setq attachment
                      (car
                       (e-chat-session-metadata-attachments
                        (plist-get
                         (e-canvas-test--await
                          (e-session-async-session-metadata
                           store "target-session"))
                         :metadata))))
                (unless attachment
                  (accept-process-output nil 0.01)))
              (should attachment)
              (should-not (plist-get attachment :canvas))
              (should (equal (plist-get attachment :uri)
                             "buffer://canvas-extra")))))))))

(ert-deftest e-canvas-test-shell-descriptor-advertises-canvas-surface ()
  "Canvas shell publishes a generic shell manifest."
  (let* ((shell (e-canvas-shell))
         (command-ids (mapcar #'e-shell-command-id
                              (e-shell-commands shell))))
    (should (eq (e-shell-id shell) 'canvas))
    (should (equal (e-shell-required-capabilities shell) '(chat-session)))
    (dolist (command-id '(open-for-current-buffer
                          new-buffer
                          new-file
                          attach-current-buffer
                          attach-file
                          reveal-canvas))
      (should (memq command-id command-ids)))))

(ert-deftest e-canvas-test-registers-canvas-shell-on-load ()
  "Loading e-canvas registers the canvas shell manifest."
  (should (eq (e-shell-id (e-shell-get 'canvas)) 'canvas))
  (should (eq (e-shell-command-interactive
               (e-shell-command-by-id
                (e-shell-get 'canvas)
                'open-for-current-buffer))
              'e-canvas-open-for-current-buffer)))

(provide 'e-canvas-test)

;;; e-canvas-test.el ends here
