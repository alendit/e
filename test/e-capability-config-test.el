;;; e-capability-config-test.el --- Tests for capability config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for capability-scoped configuration.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-capability-config)

(defconst e-capability-config-test--options
  (list
   (e-capability-config-option-create
    :key :value
    :default "default"
    :validator #'stringp)
   (e-capability-config-option-create
    :key :items
    :default nil
    :normalizer #'e-capability-config-string-list
    :validator #'e-capability-config-string-list-p))
  "Option specs for capability config tests.")

(ert-deftest e-capability-config-test-safe-local-value-shape ()
  "Directory-local values must be simple capability plists."
  (should
   (e-capability-config-safe-local-value-p
    '((dummy-config :value "project" :items ("one" "two")))))
  (should-not
   (e-capability-config-safe-local-value-p
    '((dummy-config :value "project" :dangling))))
  (should-not
   (e-capability-config-safe-local-value-p
    '((dummy-config value "project")))))

(ert-deftest e-capability-config-test-resolve-precedence ()
  "Explicit overrides win over directory-local, global, and default values."
  (let ((directory (make-temp-file "e-capability-config-" t)))
    (unwind-protect
        (progn
          (write-region
           "((nil . ((e-capability-config . ((dummy-config :value \"project\" :items \"project-item\"))))))"
           nil
           (expand-file-name ".dir-locals.el" directory)
           nil
           'silent)
          (let ((e-capability-config
                 '((dummy-config :value "global" :items ("global-item")))))
            (should
             (equal
              (e-capability-config-resolve
               'dummy-config
               e-capability-config-test--options
               :directory directory
               :overrides '(:value "explicit"))
              '(:value "explicit" :items ("project-item"))))))
      (delete-directory directory t))))

(ert-deftest e-capability-config-test-unknown-active-option-errors ()
  "Unknown active config keys fail at resolution time."
  (should-error
   (e-capability-config-resolve
    'dummy-config
    e-capability-config-test--options
    :overrides '(:unknown t))
   :type 'e-capability-config-unknown-option))

(ert-deftest e-capability-config-test-invalid-value-errors ()
  "Invalid configured values fail at resolution time."
  (should-error
   (e-capability-config-resolve
    'dummy-config
    e-capability-config-test--options
    :overrides '(:value 42))
   :type 'e-capability-config-invalid-value))

(ert-deftest e-capability-config-test-registers-option-specs ()
  "Capability owners can register option metadata for UI commands."
  (let ((e-capability-config-known-options nil))
    (should
     (eq
      (e-capability-config-register-options
       'dummy-config e-capability-config-test--options)
      e-capability-config-test--options))
    (should
     (eq
      (e-capability-config-registered-options 'dummy-config)
      e-capability-config-test--options))))

(provide 'e-capability-config-test)

;;; e-capability-config-test.el ends here
