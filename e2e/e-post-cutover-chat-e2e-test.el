;;; e-post-cutover-chat-e2e-test.el --- Post-cutover chat E2E -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic cross-owner coverage for a first curation-only OpenAI
;; Responses result after an actual offline cutover.  The injected HTTP
;; requester is the only fake boundary; migration, default composition,
;; SQLite, Board/chat delivery, harness loop, and Responses serialization are
;; production implementations.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'seq)
(require 'e-board)
(require 'e-board-registry)
(require 'e-chat-service)
(require 'e-context-lifetime)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-openai)
(require 'e-runtime-migration)
(require 'e-session)
(load (expand-file-name
       "e-board-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(load (expand-file-name
       "e-post-cutover-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(defun e-post-cutover-chat-e2e--json-body (body)
  "Decode a Responses JSON BODY into a keyword plist."
  (json-parse-string body
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(defun e-post-cutover-chat-e2e--sse (&rest events)
  "Return one deterministic SSE response containing JSON EVENTS."
  (mapconcat (lambda (event)
               (format "data: %s\n\n" (json-encode event)))
             events ""))

(defun e-post-cutover-chat-e2e--input-items (body)
  "Return BODY's Responses input as a list."
  (append (plist-get body :input) nil))

(defun e-post-cutover-chat-e2e--items (body type &optional call-id)
  "Return BODY input items matching TYPE and optional CALL-ID."
  (seq-filter
   (lambda (item)
     (and (equal (plist-get item :type) type)
          (or (null call-id)
              (equal (plist-get item :call_id) call-id))))
   (e-post-cutover-chat-e2e--input-items body)))

(defun e-post-cutover-chat-e2e--curation-tool-p (tool)
  "Return non-nil when TOOL is the reserved context-curate carrier."
  (equal (plist-get tool :name) "context-curate"))

(defun e-post-cutover-chat-e2e--assert-clean-messages
    (messages expected-answer)
  "Assert MESSAGES contain EXPECTED-ANSWER and no provider replay carrier."
  (let ((printed (prin1-to-string messages)))
    (should (seq-find
             (lambda (message)
               (and (eq (plist-get message :role) 'assistant)
                    (equal (plist-get message :content) expected-answer)))
             messages))
    (dolist (wire-fragment '("context-curate" "function_call_output"
                             "provider-replay" "curation-call"))
      (should-not (string-match-p wire-fragment printed)))
    (should-not
     (seq-find (lambda (message)
                 (memq (plist-get message :role) '(tool tool-call)))
               messages))))

(defun e-post-cutover-chat-e2e--assert-board-output
    (harness session-id expected-answer)
  "Assert SESSION-ID's Board contains EXPECTED-ANSWER as output."
  (let* ((binding (e-chat-service-binding harness session-id))
         (source (e-board-registry-board-source-board
                  (e-chat-service-binding-board binding))))
    (should
     (seq-find
      (lambda (message)
        (and (eq (e-board-message-kind message) 'output)
             (equal (e-board-message-content message) expected-answer)))
      (e-board-messages source)))))

(defun e-post-cutover-chat-e2e--assert-no-tool-lifecycle (store session-id)
  "Assert SESSION-ID has no durable model-facing tool lifecycle in STORE."
  (should-not
   (seq-find
    (lambda (event)
      (memq (plist-get event :event-type) '(tool-started tool-finished)))
    (e-session-activity-events store session-id))))

(defun e-post-cutover-chat-e2e--run-variant (mode)
  "Run the post-cutover curation-only scenario for continuation MODE."
  (let* ((base (make-temp-file
                (format "e-post-cutover-chat-%s-" mode) t))
         (root (expand-file-name "e" base))
         (source (expand-file-name "e-copy" base))
         (backup (expand-file-name "e.backup" base))
         (session-id (format "post-cutover-chat-%s" mode))
         (untouched-id (e-post-cutover-e2e--session-id 42))
         (provider-id (intern (format "post-cutover-%s-e2e" mode)))
         (first-answer (format "CURATION-%s-ANSWER" mode))
         (later-answer (format "LATER-%s-ANSWER" mode))
         (curation-call-id (format "curation-call-%s" mode))
         (curation-response-id (format "curation-response-%s" mode))
         (process-environment
          (cons "E_POST_CUTOVER_E2E_TOKEN=test-only" process-environment))
         (e-session-directory (expand-file-name "sessions" root))
         (e-context-lifetime-shadow-projection-enabled t)
         (e-harness-auto-compaction-enabled nil)
         (e-default--runtime nil)
         (e-default--chat-sessions nil)
         (e-runtime-sqlite--live-composition nil)
         (e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-registry--generations (make-hash-table :test 'equal))
         (e-harness-registry--invalidation-events
          (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-harness-instance--generation 0)
         (e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
         (e-board-registry--boards (make-hash-table :test 'equal))
         (e-board-registry--id-sequence 0)
         (e-board-registry--unsettled-pickup-count 0)
         (e-board-registry--unsettled-effect-count 0)
         (e-board-registry--unsettled-routing-count 0)
         (e-board-registry--unsettled-generation 0)
         (e-board-runtime--attachments (make-hash-table :test 'equal))
         (e-board-runtime--session-attachments (make-hash-table :test 'equal))
         (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
         (e-board-runtime--invocations (make-hash-table :test 'equal))
         (e-board-runtime--producer-bindings (make-hash-table :test 'equal))
         (e-board-runtime--producer-inputs (make-hash-table :test 'equal))
         (e-board-runtime--producer-deliveries (make-hash-table :test 'equal))
         (e-board-runtime--producer-turns (make-hash-table :test 'equal))
         (e-board--post-storage-barrier-callbacks
          (make-hash-table :test 'eq :weakness 'key))
         (e-chat-service--bindings
          (make-hash-table :test 'eq :weakness 'key))
         (e-chat-service--board-bindings (make-hash-table :test 'equal))
         (e-board-runtime--admission-open-p t)
         (e-board-runtime--quiescence-current nil)
         (e-board-runtime--pending-pickup-head nil)
         (e-board-runtime--pending-pickup-tail nil)
         (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
         (e-board-runtime--pickup-drain-scheduled nil)
         (e-openai-model-providers
          (list
           (list provider-id
                 :name (format "Post-cutover %s E2E" mode)
                 :base-url "https://post-cutover.example.test/v1"
                 :env-key "E_POST_CUTOVER_E2E_TOKEN"
                 :wire-api 'responses
                 :responses-transport 'http
                 :response-store t
                 :responses-context-layout 'developer-input
                 :include-encrypted-reasoning t
                 :observation-delivery 'request-local-replaceable
                 :continuation (eq mode 'anchored)
                 :requires-openai-auth nil)))
         requests
         (request-count 0)
         (transport
          (cl-function
           (lambda (&key body &allow-other-keys)
             (let ((parsed (e-post-cutover-chat-e2e--json-body body)))
               (setq requests (append requests (list parsed))
                     request-count (1+ request-count))
               (pcase request-count
                 (1
                  (e-post-cutover-chat-e2e--sse
                   (list
                    (cons 'type "response.output_item.done")
                    (cons
                     'item
                     (list (cons 'type "function_call")
                           (cons 'call_id curation-call-id)
                           (cons 'name "context-curate")
                           (cons 'arguments
                                 "{\"keep\":[1],\"summaries\":[]}"))))
                   (list
                    (cons 'type "response.completed")
                    (cons 'response
                          (list (cons 'id curation-response-id)
                                (cons 'status "completed"))))))
                 (2
                  (e-post-cutover-chat-e2e--sse
                   (list (cons 'type "response.output_text.done")
                         (cons 'text first-answer))
                   (list
                    (cons 'type "response.completed")
                    (cons 'response
                          (list (cons 'id
                                      (format "answer-response-%s" mode))
                                (cons 'status "completed"))))))
                 (3
                  (e-post-cutover-chat-e2e--sse
                   (list (cons 'type "response.output_text.done")
                         (cons 'text later-answer))
                   (list
                    (cons 'type "response.completed")
                    (cons 'response
                          (list (cons 'id
                                      (format "later-response-%s" mode))
                                (cons 'status "completed"))))))
                 (_ (error "Unexpected provider request %d" request-count)))))))
         (e-default-chat-harness-factory
          (lambda (&rest arguments)
            (e-openai-create-harness
             :provider provider-id
             :model "gpt-post-cutover-e2e"
             :request-function transport
             :sessions (plist-get arguments :sessions)))))
    (setenv "E_RUNTIME_STATE_DIRECTORY" nil)
    (unwind-protect
        (progn
          (e-post-cutover-e2e--make-legacy-root root)
          (copy-directory root source nil nil nil)
          (let ((report (e-runtime-migration-cutover source root backup)))
            (should (eq (plist-get report :operation) 'cutover))
            (should (file-regular-p (expand-file-name "store.sqlite3" root)))
            (should (file-directory-p backup)))
          (e-board-e2e-reset-runtime)
          (e-default-harnesses-register
           '((:id :chat-default
              :name "Default Chat"
              :kind chat
              :default t
              :factory e-default-chat-harness-create
              :sync e-default-chat-harness-sync)))
          (let* ((harness (e-harness-registry-get-or-create :chat-default))
                 (store (e-harness-sessions harness)))
            (should (= (length (e-session-list store))
                       e-post-cutover-e2e--session-count))
            (should-not (e-post-cutover-e2e--loaded-p store untouched-id))
            (should
             (equal
              (plist-get (e-chat-service-create-session
                          :harness harness :id session-id)
                         :id)
              session-id))
            (e-board-e2e-prompt-batch harness session-id "Hi")
            (should (= request-count 2))
            (should (= (length (e-session-context-curations store session-id))
                       1))
            (should-not
             (e-session-tool-followup-classifications store session-id))
            (e-post-cutover-chat-e2e--assert-no-tool-lifecycle
             store session-id)
            (e-post-cutover-chat-e2e--assert-clean-messages
             (e-session-messages store session-id) first-answer)
            (e-post-cutover-chat-e2e--assert-board-output
             harness session-id first-answer)
            (should-not (e-post-cutover-e2e--loaded-p store untouched-id))
            (let* ((first (nth 0 requests))
                   (ack (nth 1 requests))
                   (curation-tools
                    (seq-filter #'e-post-cutover-chat-e2e--curation-tool-p
                                (append (plist-get first :tools) nil)))
                   (calls
                    (e-post-cutover-chat-e2e--items
                     ack "function_call" curation-call-id))
                   (outputs
                    (e-post-cutover-chat-e2e--items
                     ack "function_call_output" curation-call-id)))
              (should (= (length curation-tools) 1))
              (should (= (length calls) (if (eq mode 'stateless) 1 0)))
              (should (= (length outputs) 1))
              (if (eq mode 'anchored)
                  (progn
                    (should (equal (plist-get ack :previous_response_id)
                                   curation-response-id))
                    (should (= (length
                                (e-post-cutover-chat-e2e--input-items ack))
                               1)))
                (should-not (plist-member ack :previous_response_id))
                (let ((input (e-post-cutover-chat-e2e--input-items ack)))
                  (should (< (seq-position input (car calls) #'eq)
                             (seq-position input (car outputs) #'eq))))))
            ;; Model a cold process boundary while preserving the same
            ;; canonical SQLite root and injected deterministic transport.
            (e-default-runtime-close)
            (e-harness-registry-clear-instance :chat-default)
            (e-board-e2e-reset-runtime)
            (let* ((reopened
                    (e-harness-registry-get-or-create :chat-default))
                   (reopened-store (e-harness-sessions reopened)))
              (should (= (length (e-session-list reopened-store))
                         (1+ e-post-cutover-e2e--session-count)))
              (should-not
               (e-post-cutover-e2e--loaded-p reopened-store session-id))
              (e-chat-service-ensure-binding reopened session-id)
              (e-post-cutover-chat-e2e--assert-clean-messages
               (e-session-messages reopened-store session-id) first-answer)
              (should (= (length
                          (e-session-context-curations
                           reopened-store session-id))
                         1))
              (e-post-cutover-chat-e2e--assert-no-tool-lifecycle
               reopened-store session-id)
              (e-board-e2e-prompt-batch reopened session-id "Later")
              (should (= request-count 3))
              (let ((later (nth 2 requests)))
                (if (eq mode 'anchored)
                    (progn
                      (should
                       (equal
                        (plist-get later :previous_response_id)
                        (format "answer-response-%s" mode)))
                      (should-not
                       (equal (plist-get later :previous_response_id)
                              curation-response-id)))
                  (should-not (plist-member later :previous_response_id)))
                (should-not
                 (seq-find
                  (lambda (item)
                    (or (equal (plist-get item :call_id) curation-call-id)
                        (and (equal (plist-get item :type) "function_call")
                             (equal (plist-get item :name)
                                    "context-curate"))))
                  (e-post-cutover-chat-e2e--input-items later))))
              (e-post-cutover-chat-e2e--assert-clean-messages
               (e-session-messages reopened-store session-id) later-answer)
              (should (= (length
                          (e-session-context-curations
                           reopened-store session-id))
                         1))
              (e-post-cutover-chat-e2e--assert-board-output
               reopened session-id later-answer)
              (should-not
               (e-post-cutover-e2e--loaded-p reopened-store untouched-id)))))
      (e-default-runtime-close)
      (e-board-e2e-reset-runtime)
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest e-post-cutover-chat-e2e-test-curation-only-first-response ()
  "Post-cutover chat carries one curation ack in both continuation modes."
  (dolist (mode '(stateless anchored))
    (e-post-cutover-chat-e2e--run-variant mode)))

(provide 'e-post-cutover-chat-e2e-test)

;;; e-post-cutover-chat-e2e-test.el ends here
