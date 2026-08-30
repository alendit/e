;;; e-session-codec-test.el --- Direct durable codec contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Codec tests load the durable schema and replay owner directly.  They keep
;; JSON normalization and replay decoding independent from the public facade;
;; aggregate state is observed only through its semantic query operations.

;;; Code:

(require 'ert)
(require 'e-session-codec)

(ert-deftest e-session-codec-test-normalizes-json-message-shape ()
  "JSON-decoded message values regain their semantic symbol types."
  (let ((message (e-session-codec--normalize-message
                  '(:role "assistant" :origin "shell" :display "expanded"))))
    (should (eq (plist-get message :role) 'assistant))
    (should (eq (plist-get message :origin) 'shell))
    (should (eq (plist-get message :display) 'expanded))))

(ert-deftest e-session-codec-test-context-sequences-are-explicit-arrays ()
  "A one-element keyword-plist sequence is not flattened into a JSON object."
  (let* ((record '(:messages ((:role user :content "hello"))))
         (encoded (e-session-codec--context-record-for-json record))
         (messages (plist-get encoded :messages)))
    (should (vectorp messages))
    (should (equal (aref messages 0) '(:role user :content "hello")))))

(ert-deftest e-session-codec-test-context-schema-is-deterministic ()
  "The codec accepts fixed context types and rejects unknown ones."
  (let ((record '(:record-version 2 :type context-generation :id "generation-1"
                  :checkpoint nil :covered-session-boundary "root")))
    (should (e-session-codec--normalize-context-record
             'context-generation record 2 t))
    (should-error
     (e-session-codec--normalize-context-record 'unknown-context record)
     :type 'e-session-codec-error)))

(ert-deftest e-session-codec-test-replay-decodes-into-aggregate-contract ()
  "Replay decoding returns detached semantic data without applying it."
  (let* ((record
          '(:type "message" :session-id "codec-session"
            :timestamp "2026-08-29T00:00:01Z" :id "message-1"
            :parent-id "codec-session-root"
            :message (:id "message-1" :role "user" :content "hello")))
         (decoded (e-session-codec-replay-record record)))
    (should (equal (plist-get decoded :session-id) "codec-session"))
    (should (eq (plist-get (plist-get decoded :message) :role) 'user))
    (should (equal (plist-get (plist-get decoded :message) :id) "message-1"))
    (should (plist-member decoded :message))))

(provide 'e-session-codec-test)

;;; e-session-codec-test.el ends here
