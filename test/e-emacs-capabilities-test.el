;;; e-emacs-capabilities-test.el --- Tests for Emacs capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for Emacs capability splits.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-capabilities)
(require 'e-emacs-capabilities)
(require 'e-tools)

(defun e-emacs-capabilities-test--tool-names (capability)
  "Return tool names registered by CAPABILITY."
  (let ((registry (e-tools-registry-create)))
    (e-capabilities-register-tools capability registry)
    (mapcar (lambda (definition)
              (plist-get definition :name))
            (e-tools-definitions registry))))

(ert-deftest e-emacs-capabilities-test-awareness-contributes-context ()
  "Emacs awareness contributes instructions and visible-buffer context."
  (let* ((capability (e-emacs-awareness-capability-create))
         (messages (e-capabilities-context-messages
                    (list capability)
                    :harness nil
                    :session-id "session-1"
                    :turn-id "turn-1")))
    (should (equal (plist-get (car messages) :content)
                   e-emacs-base-instructions))
    (should (string-match-p "Visible Emacs buffers:"
                            (plist-get (cadr messages) :content)))))

(ert-deftest e-emacs-capabilities-test-buffer-read-registers-read-tools ()
  "Buffer read capability registers listing and read tools."
  (should (equal (e-emacs-capabilities-test--tool-names
                  (e-buffer-read-capability-create))
                 '("list_buffers" "read_buffer"))))

(ert-deftest e-emacs-capabilities-test-buffer-edit-registers-edit-tools ()
  "Buffer edit capability registers mutation and save tools."
  (should (equal (e-emacs-capabilities-test--tool-names
                  (e-buffer-edit-capability-create))
                 '("write_buffer" "edit_buffer" "save_buffer"))))

(ert-deftest e-emacs-capabilities-test-elisp-eval-registers-run-elisp ()
  "Elisp eval capability registers run_elisp only."
  (should (equal (e-emacs-capabilities-test--tool-names
                  (e-elisp-eval-capability-create))
                 '("run_elisp"))))

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
                   elisp-eval))))

(provide 'e-emacs-capabilities-test)

;;; e-emacs-capabilities-test.el ends here
