;;; e-harness-base-test.el --- Tests for harness-base layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for harness-owned support capabilities.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-hooks)
(require 'e-operations)
(require 'e-resources)

(defun e-harness-base-test--tmp-read-method-p (resources)
  "Return non-nil when RESOURCES includes a tmp:// read method."
  (cl-some (lambda (method)
             (equal (e-resource-method-scheme method) "tmp"))
           (e-resources-methods-for-operation resources e-operation-read)))

(ert-deftest e-harness-base-test-require-and-create-layer ()
  "The harness-base layer bundles harness-owned support capabilities."
  (should (require 'e-harness-base nil t))
  (let ((layer (e-harness-base-layer-create)))
    (should (eq (e-layer-id layer) 'harness-base))
    (should (equal (e-layer-name layer) "Harness Base"))
    (should (equal (mapcar #'e-capability-id
                           (e-layer-capabilities layer))
                   '(session-tmp-resources
                     tool-output-truncation)))))

(ert-deftest e-harness-base-test-activation-adds-tmp-resource-and-hook ()
  "Activating harness-base exposes tmp:// and the post-tool hook."
  (should (require 'e-harness-base nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (e-harness-base-layer-create)))
    (e-harness-activate-layer harness layer)
    (should (e-harness-base-test--tmp-read-method-p
             (e-harness-resources harness "session-1" "turn-1")))
    (should (equal (mapcar #'e-hook-id
                           (e-hooks-for-point
                            (e-harness-hooks harness)
                            :post-tool-call))
                   '("50-tool-output-truncation")))
    (e-harness-deactivate-layer harness 'harness-base)
    (should-not (e-harness-base-test--tmp-read-method-p
                 (e-harness-resources harness "session-1" "turn-1")))
    (should-not (e-hooks-for-point
                 (e-harness-hooks harness)
                 :post-tool-call))))

(provide 'e-harness-base-test)

;;; e-harness-base-test.el ends here
