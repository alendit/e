;;; e-context-test.el --- Tests for e context strategies -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for provider-neutral context construction.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-context)
(require 'e-session)

(ert-deftest e-context-test-transcript-stack-preserves-session-order ()
  "Transcript-stack builds backend context from ordered session messages."
  (let ((store (e-session-store-create))
        (strategy (e-context-transcript-stack-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:id "m1" :role user :content "hi"))
    (e-session-append-message store "session-1" '(:id "m2" :role assistant :content "hello"))
    (e-session-append-message store "session-1" '(:id "m3" :role tool :content (:status ok)))
    (should
     (equal (e-context-build strategy
                             :sessions store
                             :session-id "session-1"
                             :options '(:model "fake"))
            '(:strategy transcript-stack
              :messages ((:id "m1" :role user :content "hi")
                         (:id "m2" :role assistant :content "hello")
                         (:id "m3" :role tool :content (:status ok)))
              :options (:model "fake"))))))

(ert-deftest e-context-test-rejects-missing-builder ()
  "Context strategies need a build function."
  (let ((strategy (e-context-create :name 'broken :build nil)))
    (should-error
     (e-context-build strategy
                      :sessions (e-session-store-create)
                      :session-id "session-1"
                      :options nil)
     :type 'wrong-type-argument)))

(provide 'e-context-test)

;;; e-context-test.el ends here
