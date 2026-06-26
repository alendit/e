;;; e-emacs-capabilities-test.el --- Tests for Emacs capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for Emacs capability splits.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-capabilities)
(require 'e-emacs-capabilities)
(require 'e-resources)
(require 'e-store)
(require 'e-tools)

(defun e-emacs-capabilities-test--tool-names (capability)
  "Return tool names registered by CAPABILITY."
  (let ((registry (e-tools-registry-create)))
    (e-capabilities-register-tools capability registry)
    (mapcar (lambda (definition)
              (plist-get definition :name))
            (e-tools-definitions registry))))

(defun e-emacs-capabilities-test--resources (capability)
  "Return resource registry registered by CAPABILITY."
  (let ((registry (e-resources-registry-create)))
    (e-capabilities-register-resource-methods capability registry)
    registry))

(ert-deftest e-emacs-capabilities-test-awareness-contributes-context ()
  "Emacs awareness contributes instructions and visible-buffer context."
  (let* ((capability (e-emacs-awareness-capability-create))
         (messages (e-capabilities-context-messages
                    (list capability)
                    :harness nil
                    :session-id "session-1"
                    :turn-id "turn-1")))
    (should (string-prefix-p e-emacs-base-instructions
                             (plist-get (car messages) :content)))
    (should (= (e-capability-instruction-priority capability) 300))
    (should (= (e-context-provider-priority
                (car (e-capability-context-providers capability)))
               320))
    (should (eq (e-context-provider-cache-placement
                 (car (e-capability-context-providers capability)))
                'dynamic-context))
    (should (string-match-p "Visible Emacs buffers:"
                            (plist-get (cadr messages) :content)))))

(ert-deftest e-emacs-capabilities-test-buffer-read-registers-read-tools ()
  "Buffer read capability registers listing tools and read-only resources."
  (should (equal (e-emacs-capabilities-test--tool-names
                  (e-buffer-read-capability-create))
                 '("list_buffers")))
  (let* ((buffer (generate-new-buffer " *e-cap-buffer-read*"))
         (resources (e-emacs-capabilities-test--resources
                     (e-buffer-read-capability-create))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "content"))
          (should (equal (plist-get
                          (e-resources-read
                           resources
                           (concat "buffer://" (buffer-name buffer)))
                          :content)
                         "content"))
          (should (equal (mapcar #'e-operation-id
                                 (e-resources-operations resources))
                         '(read glob search)))
          (should-error
           (e-resources-write
            resources
            (concat "buffer://" (buffer-name buffer))
            "new")
           :type 'e-resources-unsupported-operation))
      (kill-buffer buffer))))

(ert-deftest e-emacs-capabilities-test-buffer-edit-registers-edit-tools ()
  "Buffer edit capability registers save tools and writable resources."
  (should (equal (e-emacs-capabilities-test--tool-names
                  (e-buffer-edit-capability-create))
                 '("save_buffer")))
  (let* ((buffer (generate-new-buffer " *e-cap-buffer-edit*"))
         (resources (e-emacs-capabilities-test--resources
                     (e-buffer-edit-capability-create))))
    (unwind-protect
        (progn
          (e-resources-write
           resources
           (concat "buffer://" (buffer-name buffer))
           "old")
          (e-resources-edit
           resources
           (concat "buffer://" (buffer-name buffer))
           '((:oldText "old" :newText "new")))
          (should (equal (with-current-buffer buffer (buffer-string))
                         "new"))
          (should (equal (mapcar #'e-operation-id
                                 (e-resources-operations resources))
                         '(read write edit glob search))))
      (kill-buffer buffer))))

(ert-deftest e-emacs-capabilities-test-elisp-eval-registers-run-elisp ()
  "Elisp eval capability registers run_elisp only."
  (should (equal (e-emacs-capabilities-test--tool-names
                  (e-elisp-eval-capability-create))
                 '("run_elisp"))))

(ert-deftest e-emacs-capabilities-test-elisp-eval-documents-tool-chaining ()
  "Elisp eval contributes guidance for chaining active tools from Lisp."
  (let* ((capability (e-elisp-eval-capability-create))
         (instructions (e-capability-instructions capability)))
    (should (string-match-p "(e-tools-call NAME ARGUMENTS" instructions))
    (should (string-match-p "(e-tools-call! NAME ARGUMENTS" instructions))
    (should (string-match-p "e-tools-call! \"search\"" instructions))
    (should (string-match-p "e-tools-call! \"read\"" instructions))
    (should (string-match-p "currently active tools" instructions))
    (should (string-match-p "several tool calls" instructions))
    (should (string-match-p "single direct tool call" instructions))
    (should (string-match-p "visible as activity" instructions))))

(ert-deftest e-emacs-capabilities-test-workspace-awareness-context ()
  "Workspace awareness reports current and shell workspace state."
  (let* ((token (make-e-workspace-token
                 :backend 'single
                 :id 'test-workspace
                 :name "test"
                 :frame (selected-frame)))
         (buffer (get-buffer-create " *e-workspace-context*")))
    (unwind-protect
        (progn
          (e-buffer-set-workspace buffer token)
          (cl-letf (((symbol-function 'e-workspace-current)
                     (lambda (&optional _frame) token)))
            (with-current-buffer buffer
              (let* ((capability (e-workspace-awareness-capability-create))
                     (messages (e-capabilities-context-messages
                                (list capability)
                                :harness nil
                                :session-id "session-1"
                                :turn-id "turn-1"))
                     (message (cl-find-if
                               (lambda (candidate)
                                 (string-match-p
                                  "Workspace awareness:"
                                  (plist-get candidate :content)))
                               messages))
                     (content (plist-get message :content)))
                (should message)
                (should (string-match-p "Workspace awareness:" content))
                (should (string-match-p "current=single:test" content))
                (should (string-match-p "shell=single:test" content))
                (should (string-match-p "prefer workspace_state" content))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-emacs-capabilities-test-workspace-awareness-registers-actions ()
  "Workspace awareness registers the small model-facing action set."
  (should (equal (e-emacs-capabilities-test--tool-names
                  (e-workspace-awareness-capability-create))
                 '("workspace_state"
                   "workspace_focus_buffer"
                   "workspace_show_shell"))))

(ert-deftest e-emacs-capabilities-test-workspace-focus-buffer-action ()
  "The focus action routes a named buffer through the workspace display helper."
  (let* ((capability (e-workspace-awareness-capability-create))
         (registry (e-tools-registry-create))
         (buffer (get-buffer-create " *e-workspace-focus*"))
         (shell (get-buffer-create " *e-workspace-focus-shell*"))
         (token (make-e-workspace-token
                 :backend 'single
                 :id 'focus-workspace
                 :name "focus"
                 :frame (selected-frame)))
         focused)
    (unwind-protect
        (progn
          (e-capabilities-register-tools capability registry)
          (e-buffer-set-workspace buffer token)
          (e-buffer-set-workspace shell token)
          (with-current-buffer shell
            (cl-letf (((symbol-function 'e-workspace-pop-to-buffer)
                       (lambda (candidate &key workspace)
                         (setq focused (list candidate workspace))
                         candidate)))
              (let ((result (e-tools-execute
                             registry
                             '(:id "call-1"
                               :name "workspace_focus_buffer"
                               :arguments (:buffer " *e-workspace-focus*")))))
                (should (eq (plist-get result :status) 'ok))
                (should (equal (plist-get (plist-get result :content) :buffer)
                               " *e-workspace-focus*"))
                (should (equal focused (list buffer token)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p shell)
        (kill-buffer shell)))))

(ert-deftest e-emacs-capabilities-test-workspace-focus-buffer-preserves-target-affinity ()
  "The focus action preserves a buffer's own workspace over stale shell state."
  (let* ((capability (e-workspace-awareness-capability-create))
         (registry (e-tools-registry-create))
         (buffer (get-buffer-create " *e-workspace-focus-owned*"))
         (shell (get-buffer-create " *e-workspace-focus-owner-shell*"))
         (buffer-token (make-e-workspace-token
                        :backend 'doom
                        :id "buffer"
                        :name "buffer"
                        :frame (selected-frame)))
         (shell-token (make-e-workspace-token
                       :backend 'doom
                       :id "shell"
                       :name "shell"
                       :frame (selected-frame)))
         focused)
    (unwind-protect
        (progn
          (e-capabilities-register-tools capability registry)
          (e-buffer-set-workspace buffer buffer-token)
          (e-buffer-set-workspace shell shell-token)
          (with-current-buffer shell
            (cl-letf (((symbol-function 'e-workspace-pop-to-buffer)
                       (lambda (candidate &key workspace)
                         (setq focused (list candidate workspace))
                         candidate)))
              (let ((result (e-tools-execute
                             registry
                             '(:id "call-1"
                               :name "workspace_focus_buffer"
                               :arguments (:buffer " *e-workspace-focus-owned*")))))
                (should (eq (plist-get result :status) 'ok))
                (should (equal focused (list buffer buffer-token)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p shell)
        (kill-buffer shell)))))

(ert-deftest e-emacs-capabilities-test-workspace-awareness-skill-is-readable ()
  "Workspace awareness registers workspace display guidance as an e:// skill."
  (let* ((store (e-store-create))
         (capability (e-workspace-awareness-capability-create)))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://workspace-awareness/skills/workspace-display")))
    (let ((content (e-store-read
                    store
                    "e://workspace-awareness/skills/workspace-display"
                    nil)))
      (should (string-match-p "Workspace-aware Emacs display" content))
      (should (string-match-p "workspace_focus_buffer" content))
      (should (string-match-p "active shell workspace" content))
      (should (string-match-p "e-workspace-pop-to-buffer" content))
      (should (string-match-p "Presentation shells own workspace affinity"
                              content)))))

(ert-deftest e-emacs-capabilities-test-awareness-config-skill-is-readable ()
  "Emacs awareness registers config guidance as an e:// skill."
  (let* ((store (e-store-create))
         (capability (e-emacs-awareness-capability-create)))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://emacs-awareness/skills/emacs-config")))
    (let ((content (e-store-read
                    store
                    "e://emacs-awareness/skills/emacs-config"
                    nil)))
      (should (string-match-p "Emacs and Doom configuration" content))
      (should (string-match-p "config.org" content))
      (should (string-match-p "locate-library" content))
      (should (string-match-p "doom-user-dir" content))
      (should (string-match-p "tangle" content)))))

(ert-deftest e-emacs-capabilities-test-selection-context-placeholder ()
  "Selection context exists as a no-op capability placeholder."
  (let ((capability (e-selection-context-capability-create)))
    (should (eq (e-capability-id capability) 'selection-context))
    (should-not (e-capability-tools capability))
    (should-not (e-capability-context-providers capability))))

(ert-deftest e-emacs-capabilities-test-layer-presets ()
  "Emacs presets compose conservative and operator capabilities."
  (should (equal (mapcar #'e-capability-id
                         (e-layer-capabilities (e-emacs-layer-create)))
                 '(emacs-awareness buffer-read selection-context)))
  (should (equal (mapcar #'e-capability-id
                         (e-layer-capabilities (e-emacs-operator-layer-create)))
                 '(emacs-awareness
                   buffer-read
                   selection-context
                   buffer-edit
                   elisp-eval
                   workspace-awareness))))

(provide 'e-emacs-capabilities-test)

;;; e-emacs-capabilities-test.el ends here
