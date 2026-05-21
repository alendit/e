;;; e-file-capabilities-test.el --- Tests for file capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for workspace file and process capability splits.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-file-capabilities)
(require 'e-tools)

(defun e-file-capabilities-test--tool-names (capability)
  "Return tool names registered by CAPABILITY."
  (let ((registry (e-tools-registry-create)))
    (e-capabilities-register-tools capability registry)
    (mapcar (lambda (definition)
              (plist-get definition :name))
            (e-tools-definitions registry))))

(ert-deftest e-file-capabilities-test-file-inspection-registers-read-only ()
  "The file-inspection capability registers only the read tool."
  (should (equal (e-file-capabilities-test--tool-names
                  (e-file-inspection-capability-create default-directory))
                 '("read"))))

(ert-deftest e-file-capabilities-test-file-mutation-registers-write-and-edit ()
  "The file-mutation capability registers write and edit tools."
  (should (equal (e-file-capabilities-test--tool-names
                  (e-file-mutation-capability-create default-directory))
                 '("write" "edit"))))

(ert-deftest e-file-capabilities-test-shell-process-registers-bash-only ()
  "The shell-process capability registers only the bash tool."
  (should (equal (e-file-capabilities-test--tool-names
                  (e-shell-process-capability-create default-directory))
                 '("bash"))))

(provide 'e-file-capabilities-test)

;;; e-file-capabilities-test.el ends here
