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
(require 'e-resources)
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
    (should (equal (plist-get (car messages) :content)
                   e-emacs-base-instructions))
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
                         "new")))
      (kill-buffer buffer))))

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
