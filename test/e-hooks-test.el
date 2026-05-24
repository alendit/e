;;; e-hooks-test.el --- Tests for e hook registries -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for capability-contributed lifecycle hooks.

;;; Code:

(require 'ert)
(require 'e)

(ert-deftest e-hooks-test-create-validates-shape ()
  "Hooks require string ids, keyword points, and function handlers."
  (should (require 'e-hooks nil t))
  (should (e-hook-p
           (e-hook-create
            :id "50-example"
            :point :post-tool-call
            :handler #'identity)))
  (should-error (e-hook-create :id 'not-string
                               :point :post-tool-call
                               :handler #'identity)
                :type 'wrong-type-argument)
  (should-error (e-hook-create :id "50-example"
                               :point 'post-tool-call
                               :handler #'identity)
                :type 'wrong-type-argument)
  (should-error (e-hook-create :id "50-example"
                               :point :post-tool-call
                               :handler "not-function")
                :type 'wrong-type-argument))

(ert-deftest e-hooks-test-registry-runs-hooks-in-id-order ()
  "Hook registries run hooks for a point in lexicographical id order."
  (should (require 'e-hooks nil t))
  (let ((registry (e-hooks-registry-create))
        (seen nil))
    (e-hooks-register
     registry
     (e-hook-create
      :id "90-third"
      :point :post-tool-call
      :handler (lambda (value context)
                 (push (list :third value context) seen)
                 (concat value "3"))))
    (e-hooks-register
     registry
     (e-hook-create
      :id "10-first"
      :point :post-tool-call
      :handler (lambda (value context)
                 (push (list :first value context) seen)
                 (concat value "1"))))
    (e-hooks-register
     registry
     (e-hook-create
      :id "50-second"
      :point :post-tool-call
      :handler (lambda (value context)
                 (push (list :second value context) seen)
                 (concat value "2"))))
    (should (equal (mapcar #'e-hook-id
                           (e-hooks-for-point registry :post-tool-call))
                   '("10-first" "50-second" "90-third")))
    (should (equal (e-hooks-run-reduce registry :post-tool-call "v" '(:ctx t))
                   "v123"))
    (should (equal (mapcar #'car (nreverse seen))
                   '(:first :second :third)))))

(ert-deftest e-hooks-test-registry-rejects-duplicate-ids-per-point ()
  "Duplicate hook ids at the same point are ambiguous and rejected."
  (should (require 'e-hooks nil t))
  (let ((registry (e-hooks-registry-create)))
    (e-hooks-register
     registry
     (e-hook-create
      :id "50-same"
      :point :post-tool-call
      :handler #'ignore))
    (should-error
     (e-hooks-register
      registry
      (e-hook-create
       :id "50-same"
       :point :post-tool-call
       :handler #'ignore))
     :type 'e-hooks-duplicate-id)
    (should
     (e-hooks-register
      registry
      (e-hook-create
       :id "50-same"
       :point :pre-tool-call
       :handler #'ignore)))))

(provide 'e-hooks-test)

;;; e-hooks-test.el ends here
