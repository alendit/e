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
              :messages ((:role user :content "hi")
                         (:role assistant :content "hello")
                         (:role tool :content (:status ok)))
              :options (:model "fake"))))))

(ert-deftest e-context-test-transcript-stack-uses-latest-compaction-suffix ()
  "Transcript-stack sends one compaction summary plus the kept suffix."
  (let ((store (e-session-store-create))
        (strategy (e-context-transcript-stack-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:id "m1" :role user :content "old"))
    (e-session-append-message store "session-1" '(:id "m2" :role assistant :content "old answer"))
    (let ((boundary
           (e-session-append-message
            store "session-1" '(:id "m3" :role user :content "keep this"))))
      (e-session-append-message store "session-1"
                                '(:id "m4" :role assistant :content "kept answer"))
      (e-session-append-compaction
       store "session-1" "Earlier work summary."
       :first-kept-entry-id (plist-get boundary :id))
      (should
       (equal (plist-get
               (e-context-build strategy
                                :sessions store
                                :session-id "session-1"
                                :options nil)
               :messages)
              '((:role system :content "Earlier work summary.")
                (:role user :content "keep this")
                (:role assistant :content "kept answer")))))))

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
