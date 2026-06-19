;;; e-file-capabilities-test.el --- Tests for file capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for workspace file and process capability splits.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-file-capabilities)
(require 'e-harness)
(require 'e-resources)
(require 'e-session)
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

(ert-deftest e-file-capabilities-test-file-handling-registers-read-write-edit ()
  "The file-handling capability registers read/write/edit file resources."
  (let* ((directory (make-temp-file "e-file-cap-" t))
         (file (expand-file-name "sample.txt" directory))
         (resources (e-file-capabilities-test--resources
                     (e-file-handling-capability-create directory))))
    (unwind-protect
        (progn
          (e-resources-write resources "file://sample.txt" "old")
          (should (equal (e-resources-read resources "file://sample.txt")
                         "old"))
          (e-resources-edit resources
                            "file://sample.txt"
                            '((:oldText "old" :newText "new")))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "new")))
      (delete-directory directory t))))

(ert-deftest e-file-capabilities-test-file-handling-registers-sync-status-tool ()
  "File handling exposes a resource coherence status tool."
  (should (equal (e-file-capabilities-test--tool-names
                  (e-file-handling-capability-create default-directory))
                 '("resource_sync_status"))))

(ert-deftest e-file-capabilities-test-shell-process-registers-bash-only ()
  "The shell-process capability registers only the bash tool."
  (should (equal (e-file-capabilities-test--tool-names
                  (e-shell-process-capability-create default-directory))
                 '("bash"))))

(ert-deftest e-file-capabilities-test-handling-resolves-secondary-workspace-root ()
  "File handling resolves absolute paths into a configured secondary root.
The capability asks `e-harness-workspace-roots' at registration time, so a
session whose primary root has configured extras can edit files in those
extras."
  (let* ((primary (file-name-as-directory (make-temp-file "e-fc-primary-" t)))
         (secondary (file-name-as-directory (make-temp-file "e-fc-secondary-" t)))
         (sec-file (expand-file-name "note.txt" secondary))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-workspace-roots-alist (list (cons primary (list secondary))))
         (resources (e-resources-registry-create)))
    (unwind-protect
        (progn
          (e-harness-create-session
           harness :id "s1" :metadata (list :project-root primary))
          (e-capabilities-register-resource-methods
           (e-file-handling-capability-create primary)
           resources
           :harness harness :session-id "s1")
          (e-resources-write resources (concat "file://" sec-file) "hello")
          (should (equal (with-temp-buffer
                           (insert-file-contents sec-file)
                           (buffer-string))
                         "hello")))
      (delete-directory primary t)
      (delete-directory secondary t))))

(provide 'e-file-capabilities-test)

;;; e-file-capabilities-test.el ends here
