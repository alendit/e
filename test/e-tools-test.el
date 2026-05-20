;;; e-tools-test.el --- Tests for e tool registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for pure tool registration and dispatch.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-tools)

(ert-deftest e-tools-test-register-and-execute ()
  "Registered tools execute through structured calls."
  (let ((registry (e-tools-registry-create)))
    (e-tools-register registry
                      :name "echo"
                      :description "Return the input text."
                      :handler (lambda (arguments)
                                 (plist-get arguments :text)))
    (should (equal (e-tools-execute registry
                                    '(:id "call-1" :name "echo" :arguments (:text "hi")))
                   '(:tool-call-id "call-1"
                     :name "echo"
                     :status ok
                     :content "hi"
                     :metadata nil)))))

(ert-deftest e-tools-test-definitions-are-backend-neutral-function-tools ()
  "Registered tools expose backend-neutral function definitions."
  (let ((registry (e-tools-registry-create)))
    (e-tools-register registry
                      :name "noop"
                      :description "Accept no arguments."
                      :parameters '(:type "object" :properties nil)
                      :handler (lambda (_arguments) "now"))
    (let* ((definitions (e-tools-definitions registry))
           (parameters (plist-get (car definitions) :parameters)))
      (should (equal (plist-get parameters :type) "object"))
      (should (hash-table-p (plist-get parameters :properties)))
      (should (string-match-p
               "\"properties\":{}"
               (json-encode definitions)))
      (should (equal (car definitions)
                     `(:type "function"
                       :name "noop"
                       :description "Accept no arguments."
                       :parameters ,parameters
                       :strict :json-false))))))

(ert-deftest e-tools-test-missing-tool-returns-structured-error ()
  "Unknown tools return structured error results."
  (let ((registry (e-tools-registry-create)))
    (should (equal (e-tools-execute registry
                                    '(:id "call-1" :name "missing" :arguments nil))
                   '(:tool-call-id "call-1"
                     :name "missing"
                     :status error
                     :content "Unknown tool: missing"
                     :metadata (:error e-tool-missing))))))

(provide 'e-tools-test)

;;; e-tools-test.el ends here
