;;; e-debug-test.el --- Tests for standing debug agent session -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the debug agent shell's standing session resolver.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-debug)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-session)

(defmacro e-debug-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest e-debug-test-ensure-session-reuses-standing-session ()
  "The debug resolver reuses the same standing session."
  (e-debug-test--with-empty-harness-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions (e-session-store-create)))
          (e-debug--session-id nil))
      (cl-letf (((symbol-function 'e-debug--default-harness)
                 (lambda () harness)))
        (let ((first (e-debug--ensure-session))
              (second (e-debug--ensure-session)))
          (should (equal second first))
          (should (= (length (e-harness-session-list harness)) 1))
          (should (equal (plist-get
                          (plist-get (e-session-get
                                      (e-harness-sessions harness)
                                      first)
                                     :metadata)
                          :source)
                         'e-debug)))))))

(ert-deftest e-debug-test-ensure-session-rediscovers-existing-session ()
  "The debug resolver finds an existing debug session when its cache is empty."
  (e-debug-test--with-empty-harness-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions (e-session-store-create)))
          (e-debug--session-id nil))
      (cl-letf (((symbol-function 'e-debug--default-harness)
                 (lambda () harness)))
        (let ((created (e-debug--ensure-session)))
          (setq e-debug--session-id nil)
          (should (equal (e-debug--ensure-session) created))
          (should (= (length (e-harness-session-list harness)) 1)))))))

(provide 'e-debug-test)

;;; e-debug-test.el ends here
