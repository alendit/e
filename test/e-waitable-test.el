;;; e-waitable-test.el --- Tests for the waitable resolver registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for reference parsing and resolver lookup.

;;; Code:

(require 'ert)
(require 'e-waitable)

(defmacro e-waitable-test--with-clean-registry (&rest body)
  "Run BODY with a fresh, isolated resolver registry."
  (declare (indent 0))
  `(let ((e-waitable--resolvers (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest e-waitable-test-parse-reference ()
  "A reference splits into scheme and local id, rejecting malformed forms."
  (should (equal (e-waitable-parse-reference "subagent:sub_1")
                 '("subagent" . "sub_1")))
  ;; Extra colons belong to the local id.
  (should (equal (e-waitable-parse-reference "job:a:b")
                 '("job" . "a:b")))
  (should-not (e-waitable-parse-reference "noscheme"))
  (should-not (e-waitable-parse-reference ":only-id"))
  (should-not (e-waitable-parse-reference "scheme:"))
  (should-not (e-waitable-parse-reference nil)))

(ert-deftest e-waitable-test-resolve-dispatches-to-scheme ()
  "Resolution calls the registered resolver with the local id."
  (e-waitable-test--with-clean-registry
    (let (seen)
      (e-waitable-register-resolver
       "fake"
       (lambda (id) (setq seen id) (list :marker id)))
      (should (equal (plist-get (e-waitable-resolve "fake:xyz") :handle)
                     '(:marker "xyz")))
      (should (equal seen "xyz")))))

(ert-deftest e-waitable-test-resolve-reports-errors-not-signals ()
  "Malformed, unknown-scheme, and unknown-id references are per-reference errors."
  (e-waitable-test--with-clean-registry
    (e-waitable-register-resolver "fake" (lambda (_id) nil))
    ;; Malformed.
    (should (plist-get (e-waitable-resolve "bogus") :error))
    ;; Unknown scheme.
    (should (string-match-p "unknown reference scheme"
                            (plist-get (e-waitable-resolve "other:1") :error)))
    ;; Known scheme, resolver returns nil -> unknown id.
    (should (string-match-p "unknown fake id"
                            (plist-get (e-waitable-resolve "fake:1") :error)))))

(ert-deftest e-waitable-test-reregister-replaces ()
  "Re-registering a scheme replaces its resolver."
  (e-waitable-test--with-clean-registry
    (e-waitable-register-resolver "fake" (lambda (_id) (list :v 1)))
    (e-waitable-register-resolver "fake" (lambda (_id) (list :v 2)))
    (should (equal (plist-get (e-waitable-resolve "fake:x") :handle)
                   '(:v 2)))))

(provide 'e-waitable-test)

;;; e-waitable-test.el ends here
