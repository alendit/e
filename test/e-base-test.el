;;; e-base-test.el --- Tests for base layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the base layer bundle.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-base)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-tools)

(ert-deftest e-base-test-layer-registers-base-tools ()
  "The base layer registers the base tool surface."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (e-base-layer-create default-directory)))
    (e-harness-activate-layer harness layer)
    (should (equal (mapcar (lambda (definition)
                             (plist-get definition :name))
                           (e-tools-definitions (e-harness-tools harness)))
                   '("read" "write" "edit" "bash")))))

(ert-deftest e-base-test-layer-activates-file-capabilities ()
  "The base layer is a preset over file and process capabilities."
  (let ((layer (e-base-layer-create default-directory)))
    (should (equal (mapcar #'e-capability-id
                           (e-layer-capabilities layer))
                   '(base-guidance
                     file-inspection
                     file-mutation
                     shell-process)))))

(ert-deftest e-base-test-layer-captures-directory-for-relative-paths ()
  "The base layer resolves relative paths against the captured directory."
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
