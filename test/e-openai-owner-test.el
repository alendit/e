;;; e-openai-owner-test.el --- Direct OpenAI owner contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests load the extracted OpenAI owners without the e-openai facade.
;; Composed request/lifecycle behavior remains covered by e-openai-test.el.

;;; Code:

(require 'ert)
(require 'e-openai-chat-completions)
(require 'e-openai-compaction)
(require 'e-openai-decoder)
(require 'e-openai-diagnostics)
(require 'e-openai-http)
(require 'e-openai-profile)
(require 'e-openai-responses)
(load (expand-file-name
       "e-test-environment-support.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-openai-owner-test-loads-without-facade ()
  "The owner contracts are loadable without composing the provider facade."
  (e-test-run-fresh-owner-isolation
   "OpenAI owner isolation"
   '(e-openai-chat-completions e-openai-compaction e-openai-decoder
     e-openai-diagnostics e-openai-http e-openai-profile e-openai-responses)
   '(e-openai)
   '((fboundp 'e-openai-provider-profile)
     (fboundp 'e-openai-codex-request-body)
     (fboundp 'e-openai-chat-completion-request-body)
     (fboundp 'e-openai-decoder-event-items)
     (fboundp 'e-openai-diagnostics-bounded-text)
     (fboundp 'e-openai-http-request-start)
     (fboundp 'e-openai-compaction-eligible-p))))

(ert-deftest e-openai-owner-test-profile-returns-semantic-capabilities ()
  "Profile policy exposes normalized capability values, not owner state."
  (let* ((profile (e-openai-provider-profile 'openai))
         (capabilities
          (e-openai-profile-context-capabilities profile nil
                                                  :provider 'openai)))
    (should (eq (e-openai-provider-wire-api profile) 'responses))
    (should (eq (plist-get capabilities :observation-delivery)
                'inherited))
    (should (eq (plist-get capabilities :continuation) 'linear))))

(ert-deftest e-openai-owner-test-responses-and-chat-mappings-are-separate ()
  "The two wire variants produce their own stable request shapes."
  (let ((responses
         (e-openai-codex-request-body
          :messages '((:role user :content "hello"))
          :options '(:model "gpt-5.5")
          :tools nil))
        (chat
         (e-openai-chat-completion-request-body
          :messages '((:role user :content "hello"))
          :options '(:model "gpt-4o")
          :tools nil)))
    (should (equal (plist-get responses :model) "gpt-5.5"))
    (should (vectorp (plist-get responses :input)))
    (should (equal (plist-get chat :model) "gpt-4o"))
    (should (vectorp (plist-get chat :messages)))
    (should-not (plist-member chat :input))))

(ert-deftest e-openai-owner-test-decoder-returns-semantic-event-items ()
  "Decoder output is a bounded generic event projection."
  (should
   (equal
    (e-openai-decoder-event-items
     '(:type "response.output_text.delta" :delta "ok") nil)
    '((:type assistant-delta :content "ok")))))

(ert-deftest e-openai-owner-test-diagnostics-bound-and-redact-url-query ()
  "Diagnostics bound text and retain only safe URL metadata."
  (let ((bounded (e-openai-diagnostics-bounded-text (make-string 5000 ?x)))
        (metadata
         (e-openai-diagnostics-url-metadata
          "https://example.test/path?token=secret")))
    (should (stringp bounded))
    (should (< (string-bytes bounded) 5000))
    (should (equal metadata '(:url-host "example.test" :url-path "/path")))))

(ert-deftest e-openai-owner-test-compaction-policy-is-profile-shaped ()
  "Compaction eligibility is decided from explicit provider identity."
  (let ((profile (e-openai-provider-profile 'openai)))
    (should (e-openai-compaction-eligible-p
             'openai profile nil nil #'ignore))
    (should-not (e-openai-compaction-eligible-p
                 'openai profile "https://other.example.test/v1" nil #'ignore))))

(ert-deftest e-openai-owner-test-http-keeps-response-envelope-private ()
  "HTTP response construction stays inside the transport owner."
  (let ((response (e-openai-http--response-create
                   :status 503 :retry-after 1
                   :body "{\"error\":{\"message\":\"busy\"}}")))
    (should (e-openai-http-response-p response))
    (should (= (e-openai-http-response-status response) 503))
    (should (= (e-openai-http-response-retry-after response) 1))))

(provide 'e-openai-owner-test)

;;; e-openai-owner-test.el ends here
