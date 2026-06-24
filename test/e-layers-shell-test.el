;;; e-layers-shell-test.el --- Tests for layer shell commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for generic layer shell command exposure.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-layer-selection)
(require 'e-layers)
(require 'e-layers-shell)
(require 'e-shells)

(defmacro e-layers-shell-test--with-empty-layer-registry (&rest body)
  "Run BODY with an isolated layer registry."
  (declare (indent 0) (debug t))
  `(let ((e-layer--registry (make-hash-table :test 'eq)))
     ,@body))

(ert-deftest e-layers-shell-test-manifest-exposes-toggle-command ()
  "The layers shell exposes generic layer selection commands."
  (let* ((shell (e-layers-shell))
         (toggle (e-shell-command-by-id shell 'toggle)))
    (should (eq (e-shell-id shell) 'layers))
    (should (equal (e-shell-required-capabilities shell)
                   '(layer-selection)))
    (should toggle)
    (should (eq (e-shell-command-interactive toggle)
                'e-layers-toggle))
    (should (commandp (e-shell-command-interactive toggle)))))

(ert-deftest e-layers-shell-test-toggle-command-uses-current-harness ()
  "The layer toggle command operates on the generic current harness."
  (e-layers-shell-test--with-empty-layer-registry
    (e-layer-register
     (e-layer-spec-create
      :id 'optional
      :name "Optional"
      :factory (lambda ()
                 (e-layer-create :id 'optional :name "Optional"))))
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (with-temp-buffer
        (setq-local e-current-harness harness)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _args)
                     (car collection))))
          (e-layers-toggle)
          (should (e-harness-layer-enabled-p harness 'optional))
          (e-layers-toggle)
          (should-not (e-harness-layer-enabled-p harness 'optional)))))))

(ert-deftest e-layers-shell-test-interactive-commands-report-status ()
  "Interactive layer commands report status without failing."
  (e-layers-shell-test--with-empty-layer-registry
    (e-layer-register
     (e-layer-spec-create
      :id 'optional
      :name "Optional"
      :factory (lambda ()
                 (e-layer-create :id 'optional :name "Optional"))))
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
          messages)
      (with-temp-buffer
        (setq-local e-current-harness harness)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _args)
                     (car collection)))
                  ((symbol-function 'called-interactively-p)
                   (lambda (&optional _kind) t))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (call-interactively #'e-layers-enable)
          (should (e-harness-layer-enabled-p harness 'optional))
          (call-interactively #'e-layers-disable)
          (should-not (e-harness-layer-enabled-p harness 'optional))
          (call-interactively #'e-layers-toggle)
          (should (e-harness-layer-enabled-p harness 'optional))
          (should (member "Enabled optional layer" messages))
          (should (member "Disabled optional layer" messages)))))))

(provide 'e-layers-shell-test)

;;; e-layers-shell-test.el ends here
