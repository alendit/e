;;; e-compaction-test.el --- Tests for e context compaction -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for provider-neutral compaction preparation.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-compaction)
(require 'e-session)

(ert-deftest e-compaction-test-prepare-chooses-user-boundary ()
  "Compaction preparation keeps a suffix starting at a user message."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (e-session-append-message store "session-1" '(:role assistant :content "old answer"))
    (let ((boundary
           (e-session-append-message
            store "session-1" '(:role user :content "keep"))))
      (e-session-append-message store "session-1" '(:role tool-call :content (:name "x")))
      (e-session-append-message store "session-1" '(:role tool :content "tool output"))
      (let ((preparation (e-compaction-prepare
                          store "session-1" :keep-recent-tokens 1)))
        (should (equal (plist-get preparation :first-kept-entry-id)
                       (plist-get boundary :id)))
        (should (string-match-p "old answer"
                                (plist-get preparation :summary-input)))
        (should-not (string-match-p "tool output"
                                    (plist-get preparation :summary-input)))))))

(ert-deftest e-compaction-test-prepare-truncates-tool-results-and-records-resources ()
  "Summarization input truncates large tool output and metadata tracks resources."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (e-session-append-message
     store "session-1"
     (list :role 'tool
           :content (make-string (+ e-compaction-tool-result-character-limit 10) ?x)
           :metadata
           '(:tool-usage
             ((:kind resource-usage
               :tool "read_file"
               :resources ((:uri "file:///tmp/a.el" :operation read)))))))
    (e-session-append-message store "session-1" '(:role user :content "keep"))
    (let* ((preparation (e-compaction-prepare
                         store "session-1" :keep-recent-tokens 1))
           (metadata (plist-get preparation :metadata))
           (resources (plist-get metadata :affected-resources)))
      (should (string-match-p "\\[truncated 10 characters\\]"
                              (plist-get preparation :summary-input)))
      (should (equal (plist-get (car resources) :uri)
                     "file:///tmp/a.el"))
      (should (equal (plist-get (car resources) :operation) 'read)))))

(ert-deftest e-compaction-test-summary-messages-include-previous-summary-on-repeat ()
  "Repeated compaction prompt includes previous summary once."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1" '(:role user :content "old"))
    (let ((boundary
           (e-session-append-message
            store "session-1" '(:role user :content "middle"))))
      (e-session-append-compaction
       store "session-1" "Previous summary."
       :first-kept-entry-id (plist-get boundary :id))
      (e-session-append-message store "session-1" '(:role assistant :content "middle answer"))
      (e-session-append-message store "session-1" '(:role user :content "latest"))
      (let* ((preparation (e-compaction-prepare
                           store "session-1" :keep-recent-tokens 1))
             (messages (e-compaction-summary-messages preparation))
             (prompt (plist-get (cadr messages) :content)))
        (should (string-match-p "Previous summary:\nPrevious summary\\." prompt))
        (should (string-match-p "middle answer" prompt))
        (should-not (string-match-p "old" (plist-get preparation :summary-input)))))))

(provide 'e-compaction-test)

;;; e-compaction-test.el ends here
