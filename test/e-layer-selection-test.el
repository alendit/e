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
    (should (functionp (e-capabilities-action capability :enable)))
    (should (functionp (e-capabilities-action capability :disable)))
    (should (functionp (e-capabilities-action capability :toggle)))
    (should (eq (e-layer-id layer) 'e))
    (should (equal (mapcar #'e-capability-id (e-layer-capabilities layer))
                   '(layer-selection context-inspection)))))

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
      (should (equal (mapcar #'e-layer-id
                             (e-harness-active-layers harness))
                     '(optional)))
      (should (equal (plist-get
                      (e-layer-selection-enable harness 'optional)
                      :status)
                     'already-enabled))
      (should (equal (mapcar #'e-layer-id
                             (e-harness-active-layers harness))
                     '(optional)))
      (should (equal (plist-get
                      (e-layer-selection-toggle harness 'optional)
                      :status)
                     'disabled))
      (should-not (e-harness-active-layers harness)))))

(provide 'e-layer-selection-test)

;;; e-layer-selection-test.el ends here
