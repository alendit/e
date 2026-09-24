;;; e-openai-test.el --- Tests for e OpenAI/Codex backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for OpenAI/Codex auth, request mapping, and stream parsing.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-json)
(require 'e-backend)
(require 'e-dev-profile)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-loop)
(require 'e-openai)
(require 'url-http)

(load (expand-file-name "e-openai-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-openai-test-auth-file-uses-codex-home ()
  "Codex auth file resolution honors CODEX_HOME."
  (let ((process-environment
         (cons "CODEX_HOME=/tmp/e-codex-home" process-environment)))
    (should (equal (e-openai-codex-auth-file)
                   "/tmp/e-codex-home/auth.json"))))

(ert-deftest e-openai-test-http-timeout-default-is-disabled ()
  "Buffered HTTP reasoning responses have no unsafe implicit deadline."
  (should-not (default-value 'e-openai-request-timeout-seconds)))

(ert-deftest e-openai-test-codex-gpt6-models-use-supported-session-options ()
  "Codex defaults to Sol, accepts Luna, and omits unsupported SQL anchors."
  (let ((sol (e-openai-codex-create-harness))
        (luna (e-openai-codex-create-harness :model "gpt-6-luna")))
    (should (equal (plist-get (e-harness-default-options sol) :model)
                   "gpt-6-sol"))
    (should (equal (plist-get (e-harness-default-options luna) :model)
                   "gpt-6-luna"))
    (should-not (plist-get (e-harness-default-options sol)
                           :provider-continuation))
    (should-not (plist-get (e-harness-default-options luna)
                           :provider-continuation))))

(ert-deftest e-openai-test-read-auth-token-and-account-id ()
  "Auth parsing extracts the access token and account id."
  (let* ((token (e-openai-test--jwt))
         (auth (list :tokens (list :access_token token
                                   :refresh_token "refresh"))))
    (should (equal (e-openai-codex-auth-access-token auth) token))
    (should (equal (e-openai-codex-auth-account-id auth) "acct-test"))))

(ert-deftest e-openai-test-codex-harness-runs-minimal-prompt-flow ()
  "The Codex harness helper can run prompt to persisted assistant message."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (e-json-serialize
                                     (list :tokens
                                           (list :access_token token
                                                 :refresh_token "refresh")))))
         (harness
          (e-openai-codex-create-harness
           :auth-file auth-file
           :model "gpt-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers body)
              "data: {\"type\":\"response.output_text.done\",\"text\":\"real-ish answer\"}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-harness-test-prompt-batch harness "session-1" "question")
          (should (equal (mapcar (lambda (message) (plist-get message :role))
                                 (e-harness-messages harness "session-1"))
                         '(user assistant)))
          (should (equal (plist-get (cadr (e-harness-messages harness "session-1"))
                                    :content)
                         "real-ish answer")))
      (delete-file auth-file))))

(provide 'e-openai-test)

;;; e-openai-test.el ends here
