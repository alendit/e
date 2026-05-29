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
(require 'e-shells)

(defmacro e-canvas-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal)))
     ,@body))

(defun e-canvas-test--harness ()
  "Return a fake harness with chat-session capability active."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    harness))

(defun e-canvas-test--kill-chat-buffers ()
  "Kill all live e chat buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'e-chat-mode)
          (kill-buffer buffer))))))

(ert-deftest e-canvas-test-open-current-buffer-creates-canvas-session ()
  "Opening from the current buffer creates a chat session with canvas context."
  (let ((harness (e-canvas-test--harness)))
    (unwind-protect
        (e-canvas-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :canvas-test))
            (e-harness-registry-register :canvas-test harness)
            (with-temp-buffer
              (rename-buffer "canvas-source" t)
              (insert "canvas body")
              (let ((chat-buffer (e-canvas-open-for-current-buffer)))
                (should (buffer-live-p chat-buffer))
                (with-current-buffer chat-buffer
                  (let* ((attachments (e-chat-session-attachments
                                       e-chat-harness
                                       e-chat-session-id))
                         (attachment (car attachments)))
                    (should (derived-mode-p 'e-chat-mode))
                    (should (plist-get attachment :canvas))
                    (should (equal (plist-get attachment :uri)
                                   "buffer://canvas-source"))
                    (should (string-match-p
                             "canvas body"
                             (plist-get
                              (car (plist-get
                                    (e-chat-session-context
                                     e-chat-harness
                                     e-chat-session-id)
                                    :messages))
                              :content)))))))))
      (e-canvas-test--kill-chat-buffers))))

(ert-deftest e-canvas-test-open-current-buffer-reveals-existing-canvas-session ()
  "Opening from an attached canvas buffer reuses the existing chat session."
  (let ((harness (e-canvas-test--harness)))
    (unwind-protect
        (e-canvas-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :canvas-test))
            (e-harness-registry-register :canvas-test harness)
            (with-temp-buffer
              (rename-buffer "canvas-existing" t)
              (insert "canvas body")
              (e-harness-create-session harness :id "session-1")
              (e-chat-session-attach-context
               harness
               "session-1"
               (e-canvas--buffer-attachment (current-buffer))
               :canvas t)
              (let ((chat-buffer (e-canvas-open-for-current-buffer)))
                (should (buffer-live-p chat-buffer))
                (with-current-buffer chat-buffer
                  (should (derived-mode-p 'e-chat-mode))
                  (should (equal e-chat-session-id "session-1")))
                (should (= (length (e-harness-session-list harness)) 1))))))
      (e-canvas-test--kill-chat-buffers))))

(ert-deftest e-canvas-test-new-file-uses-file-backed-buffer-as-canvas ()
  "Creating a file canvas attaches its visited buffer and file URI."
  (let ((file (make-temp-file "e-canvas-" nil ".txt"))
        (harness (e-canvas-test--harness)))
    (unwind-protect
        (e-canvas-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :canvas-test))
            (e-harness-registry-register :canvas-test harness)
            (write-region "file canvas body" nil file nil 'silent)
            (let ((chat-buffer (e-canvas-new-file file)))
              (with-current-buffer chat-buffer
                (let* ((attachment (car (e-chat-session-attachments
                                         e-chat-harness
                                         e-chat-session-id))))
                  (should (plist-get attachment :canvas))
                  (should (equal (plist-get attachment :uri)
                                 (concat "file://" file)))
                  (should (get-buffer (plist-get attachment :buffer-name))))))))
      (e-canvas-test--kill-chat-buffers)
      (when-let ((buffer (find-buffer-visiting file)))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest e-canvas-test-attach-current-buffer-to-selected-session ()
  "Manual attachment adds the current buffer as non-canvas live context."
  (let ((harness (e-canvas-test--harness)))
    (unwind-protect
        (e-canvas-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :canvas-test))
            (e-harness-registry-register :canvas-test harness)
            (e-harness-create-session harness :id "target-session")
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (car collection))))
              (with-temp-buffer
                (rename-buffer "canvas-extra" t)
                (insert "extra context")
                (e-canvas-attach-current-buffer)
                (let ((attachment (car (e-chat-session-attachments
                                        harness
                                        "target-session"))))
                  (should-not (plist-get attachment :canvas))
                  (should (equal (plist-get attachment :uri)
                                 "buffer://canvas-extra")))))))
      (e-canvas-test--kill-chat-buffers))))

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
