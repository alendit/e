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
(require 'e-resources)
(require 'e-tools)

(defun e-file-capabilities-test--tool-names (capability)
  "Return tool names registered by CAPABILITY."
  (let ((registry (e-tools-registry-create)))
    (e-capabilities-register-tools capability registry)
    (mapcar (lambda (definition)
              (plist-get definition :name))
            (e-tools-definitions registry))))

(defun e-file-capabilities-test--resources (capability)
  "Return resource registry registered by CAPABILITY."
  (let ((registry (e-resources-registry-create)))
    (e-capabilities-register-resource-methods capability registry)
    registry))

(ert-deftest e-file-capabilities-test-file-inspection-registers-read-only ()
  "The file-inspection capability registers read-only file resources."
  (let* ((directory (make-temp-file "e-file-cap-read-" t))
         (file (expand-file-name "sample.txt" directory))
         (resources (e-file-capabilities-test--resources
                     (e-file-inspection-capability-create directory))))
    (unwind-protect
        (progn
          (write-region "content" nil file nil 'silent)
          (should (equal (e-resources-read resources "file://sample.txt")
                         "content"))
          (should-error
           (e-resources-write resources "file://sample.txt" "new")
           :type 'e-resources-unsupported-operation))
      (delete-directory directory t))))

(ert-deftest e-file-capabilities-test-file-mutation-registers-write-and-edit ()
  "The file-mutation capability registers writable file resources."
  (let* ((directory (make-temp-file "e-file-cap-write-" t))
         (file (expand-file-name "sample.txt" directory))
         (resources (e-file-capabilities-test--resources
                     (e-file-mutation-capability-create directory))))
    (unwind-protect
        (progn
          (e-resources-write resources "file://sample.txt" "old")
          (e-resources-edit resources
                            "file://sample.txt"
                            '((:oldText "old" :newText "new")))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "new")))
      (delete-directory directory t))))

(ert-deftest e-file-capabilities-test-file-inspection-registers-sync-status-tool ()
  "File inspection exposes a resource coherence status tool."
  (should (equal (e-file-capabilities-test--tool-names
                  (e-file-inspection-capability-create default-directory))
                 '("resource_sync_status"))))

(ert-deftest e-file-capabilities-test-file-mutation-registers-sync-status-tool ()
  "File mutation also exposes resource coherence status."
  (should (equal (e-file-capabilities-test--tool-names
                  (e-file-mutation-capability-create default-directory))
                 '("resource_sync_status"))))

(ert-deftest e-file-capabilities-test-shell-process-registers-bash-only ()
  "The shell-process capability registers only the bash tool."
  (should (equal (e-file-capabilities-test--tool-names
                  (e-shell-process-capability-create default-directory))
                 '("bash"))))

(provide 'e-file-capabilities-test)

;;; e-file-capabilities-test.el ends here
