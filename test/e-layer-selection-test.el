;;; e-layer-selection-test.el --- Tests for layer selection capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for generic layer selection actions.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-layer)
(require 'e-layer-selection)
(require 'e-layers)
(require 'e-resources)

(defmacro e-layer-selection-test--with-empty-layer-registry (&rest body)
  "Run BODY with an isolated layer registry."
  (declare (indent 0) (debug t))
  `(let ((e-layer--registry (make-hash-table :test 'eq)))
     ,@body))

(ert-deftest e-layer-selection-test-capability-exposes-layer-actions ()
  "The e layer owns generic layer selection actions."
  (let ((capability (e-layer-selection-capability-create))
        (layer (e-core-layer-create)))
    (should (eq (e-capability-id capability) 'layer-selection))
    (should (e-action-p (e-capabilities-action-spec capability :enable)))
    (should (e-action-p (e-capabilities-action-spec capability :disable)))
    (should (e-action-p (e-capabilities-action-spec capability :toggle)))
    (should (eq (e-layer-id layer) 'e))
    (should (equal (mapcar #'e-capability-id (e-layer-capabilities layer))
                   '(action-descriptions
                     e-runtime-context
                     layer-selection
                     context-inspection
                     session-compaction)))))

(ert-deftest e-layer-selection-test-runtime-context-explains-e ()
  "The default e layer explains the e runtime independently of projects."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (e-core-layer-create)))
    (setf (e-harness-intrinsic-capabilities harness)
          (e-layer-capabilities layer))
    (e-harness-create-session harness :id "session-1")
    (let ((content (mapconcat
                    (lambda (message)
                      (plist-get message :content))
                    (plist-get (e-harness-context harness "session-1")
                               :messages)
                    "\n\n")))
      (should (string-match-p "Emacs-hosted agent runtime" content))
      (should (string-match-p "e://e/refs/runtime.md" content))
      (should (string-match-p "e://e/refs/architecture.md" content)))
    (let ((resources (e-harness-resources harness "session-1")))
      (should (string-match-p
               "capabilities"
               (e-resources-read resources "e://e/refs/runtime.md" nil)))
      (should (string-match-p
               "harness"
               (e-resources-read resources "e://e/refs/architecture.md" nil))))))

(ert-deftest e-layer-selection-test-toggles-known-layer-on-harness ()
  "Layer selection can enable and disable a registered layer by id."
  (e-layer-selection-test--with-empty-layer-registry
    (e-layer-register
     (e-layer-spec-create
      :id 'optional
      :name "Optional"
      :factory (lambda ()
                 (e-layer-create :id 'optional :name "Optional"))))
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (should (equal (plist-get
                      (e-layer-selection-toggle harness 'optional)
                      :status)
                     'enabled))
      (should (equal (e-harness-enabled-layer-ids harness)
                     '(optional)))
      (should (equal (plist-get
                      (e-layer-selection-enable harness 'optional)
                      :status)
                     'already-enabled))
      (should (equal (e-harness-enabled-layer-ids harness)
                     '(optional)))
      (should (equal (plist-get
                      (e-layer-selection-toggle harness 'optional)
                      :status)
                     'disabled))
      (should-not (e-harness-enabled-layer-ids harness)))))

(provide 'e-layer-selection-test)

;;; e-layer-selection-test.el ends here
