;;; e-base-test.el --- Tests for base layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the base layer bundle.

;;; Code:

(require 'ert)
(require 'seq)
(require 'e)
(require 'e-backend)
(require 'e-base)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-tools)

(ert-deftest e-base-test-layer-registers-os-base-tools ()
  "The OS base layer registers the workspace tool surface."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (e-base-layer-create default-directory)))
    (should (eq (e-layer-id layer) 'os-base))
    (should (equal (e-layer-name layer) "OS Base"))
    (e-harness-activate-layer harness layer)
    (should (equal (mapcar (lambda (definition)
                             (plist-get definition :name))
                           (e-tools-definitions (e-harness-tools harness)))
                   '("read" "write" "edit" "resource_sync_status" "bash")))))

(ert-deftest e-base-test-layer-activates-file-capabilities ()
  "The OS base layer is a preset over file and process capabilities."
  (let ((layer (e-base-layer-create default-directory)))
    (should (eq (e-layer-id layer) 'os-base))
    (should (equal (mapcar #'e-capability-id
                           (e-layer-capabilities layer))
                   '(base-guidance
                     file-handling
                     shell-process)))))

(ert-deftest e-base-test-guidance-stays-os-focused ()
  "The OS base layer does not contribute Emacs buffer guidance."
  (let* ((layer (e-base-layer-create default-directory))
         (guidance (seq-find (lambda (capability)
                               (eq (e-capability-id capability) 'base-guidance))
                             (e-layer-capabilities layer)))
         (instructions (e-capability-instructions guidance)))
    (should (string-match-p "file and shell tools" instructions))
    (should-not (string-match-p "Emacs buffer" instructions))))

(ert-deftest e-base-test-layer-captures-directory-for-relative-paths ()
  "The OS base layer resolves relative paths against the captured directory."
  (let* ((directory (make-temp-file "e-base-layer-" t))
         (file (expand-file-name "sample.txt" directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (let ((default-directory "/tmp/"))
                  (e-base-layer-create directory))))
    (unwind-protect
        (progn
          (write-region "captured" nil file nil 'silent)
          (e-harness-activate-layer harness layer)
          (should
           (equal (plist-get
                   (e-tools-execute
                    (e-harness-tools harness)
                    '(:id "call-1"
                      :name "read"
                      :arguments (:uri "file://sample.txt")))
                   :content)
                  "captured")))
      (delete-directory directory t))))

(provide 'e-base-test)

;;; e-base-test.el ends here
