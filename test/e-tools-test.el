;;; e-tools-test.el --- Tests for e tool registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for pure tool registration and dispatch.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-tools)
(load (expand-file-name "e-tools-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-work)

(defun e-tools-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun e-tools-test--execute-with-context (registry call context)
  "Execute CALL against REGISTRY with CONTEXT and return the structured result."
  (let (result failure)
    (e-tools-start
     registry
     call
     :context context
     :on-done (lambda (value) (setq result value))
     :on-error (lambda (err) (setq failure err)))
    (should (e-tools-test--wait-until (lambda () (or result failure))))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(ert-deftest e-tools-test-register-and-execute ()
  "Registered tools execute through structured calls."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "echo"
                      :description "Return the input text."
                      :handler (lambda (arguments)
                                 (plist-get arguments :text)))
    (should (equal (e-tools-execute-batch registry
                                    '(:id "call-1" :name "echo" :arguments (:text "hi")))
                   '(:tool-call-id "call-1"
                     :name "echo"
                     :status ok
	                     :content "hi"
	                     :metadata nil)))))

(ert-deftest e-tools-test-start-prefers-work-spec ()
  "Work-backed tools expose a work handle through the existing request shape."
  (let ((registry (e-tools-registry-create))
        request
        result)
    (e-tools-test-register
     registry
     :name "work_echo"
     :description "Return text through work."
     :work (e-work-spec-create
            :id "work_echo"
            :execution 'render
            :interactive-policy 'async
            :runner (lambda (arguments _context)
                      (plist-get arguments :text))))
    (e-tools-start
     registry
     '(:id "call-work" :name "work_echo" :arguments (:text "hi" :delay 0))
     :on-request-start (lambda (value) (setq request value))
     :on-done (lambda (value) (setq result value)))
    (should (e-tools-request-p request))
    (should (eq (plist-get (e-tools-request-metadata request) :transport)
                'work))
    (should (e-work-handle-p
             (plist-get (e-tools-request-metadata request) :work-handle)))
    (should (e-tools-test--wait-until (lambda () result)))
    (should (equal (plist-get result :content) "hi"))))

(ert-deftest e-tools-test-definitions-are-backend-neutral-function-tools ()
  "Registered tools expose backend-neutral function definitions."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "noop"
                      :description "Accept no arguments."
                      :parameters '(:type "object" :properties nil)
                      :handler (lambda (_arguments) "now"))
    (let* ((definitions (e-tools-definitions registry))
           (parameters (plist-get (car definitions) :parameters)))
      (should (equal (plist-get parameters :type) "object"))
      (should (null (plist-get parameters :properties)))
      (should-not (plist-get parameters :required))
      (should (equal (car definitions)
                     `(:type "function"
                       :name "noop"
                       :description "Accept no arguments."
                       :parameters ,parameters
                       :strict :json-false))))))

(ert-deftest e-tools-test-definitions-preserve-declared-argument-schema ()
  "Provider definitions expose only each tool's declared argument schema."
  (let ((registry (e-tools-registry-create))
        (strict-parameters
         '(:type "object"
           :properties (:path (:type "string"))
           :required ["path"]
           :additionalProperties :json-false)))
    (e-tools-test-register
     registry :name "empty" :description "No arguments."
     :parameters '(:type "object" :properties nil)
     :handler (lambda (_arguments) "ok"))
    (e-tools-test-register
     registry :name "strict" :description "Strict input."
     :parameters strict-parameters
     :handler (lambda (_arguments) "ok"))
    (let* ((definitions (e-tools-definitions registry))
           (empty (cl-find "empty" definitions :key (lambda (item)
                                                      (plist-get item :name))
                           :test #'equal))
           (strict (cl-find "strict" definitions :key (lambda (item)
                                                        (plist-get item :name))
                            :test #'equal))
           (empty-parameters (plist-get empty :parameters))
           (strict-parameters* (plist-get strict :parameters)))
      (should (null (plist-get empty-parameters :properties)))
      (should-not (plist-get empty-parameters :required))
      (should (equal (plist-get strict-parameters* :additionalProperties)
                     :json-false))
      (should (equal (plist-get strict-parameters* :required) ["path"]))
      (should (equal strict-parameters* strict-parameters)))))

(ert-deftest e-tools-test-prepare-call-validates-native-arguments ()
  "Prepared calls validate and preserve only native operation arguments."
  (let ((registry (e-tools-registry-create))
        seen)
    (e-tools-test-register
     registry :name "inspect" :description "Inspect input."
     :parameters '(:type "object"
                   :properties (:text (:type "string"))
                   :required ["text"]
                   :additionalProperties :json-false)
     :handler (lambda (arguments)
                (setq seen arguments)
                "ok"))
    (let ((prepared
           (e-tools-prepare-call
            registry
            '(:id "call-native" :name "inspect"
              :arguments (:text "hello")))))
      (should (equal (plist-get prepared :arguments) '(:text "hello")))
      (should (equal (plist-get (e-tools-test--execute-with-context
                                 registry prepared nil)
                                :status)
                     'ok))
      (should (equal seen '(:text "hello"))))))

(ert-deftest e-tools-test-result-content-text-serializes-structured-content ()
  "Tool result content text is the provider-visible representation."
  (should (equal (e-tools-result-content-text "plain")
                 "plain"))
  (should (equal (e-tools-result-content-text '(:ok t :items [1 2]))
                 "{\"ok\":true,\"items\":[1,2]}"))
  (should (equal (e-tools-result-content-text
                  '(:empty-object nil :empty-array []
                    :false :json-false :null :json-null))
                 "{\"empty-object\":{},\"empty-array\":[],\"false\":false,\"null\":null}"))
  (let* ((content '(:summary "Daily — complete"))
         (text (e-tools-result-content-text content))
         (request-text (e-json-serialize (list :output text))))
    (should (multibyte-string-p text))
    (should (equal (e-json-parse-string text) content))
    (should (equal (e-json-parse-string request-text)
                   (list :output text)))))

(ert-deftest e-tools-test-result-content-preview-bounds-structured-materialization ()
  "Display previews bound traversal without changing model-facing text."
  (let* ((large (list :items (number-sequence 1 100)
                      :body (make-string 200 ?x)))
         (preview (e-tools-result-content-preview large 64 5 3)))
    (should (plist-get preview :truncated))
    (should (<= (plist-get preview :shown-bytes) 64))
    (should-not (string-match-p (make-string 80 ?x)
                                (plist-get preview :text)))
    (should-error (e-tools-result-content-text large)
                  :type 'e-tools-invalid-result-content)))

(ert-deftest e-tools-test-missing-tool-returns-structured-error ()
  "Unknown tools return structured error results."
  (let ((registry (e-tools-registry-create)))
    (should (equal (e-tools-execute-batch registry
                                    '(:id "call-1" :name "missing" :arguments nil))
                   '(:tool-call-id "call-1"
                     :name "missing"
                     :status error
                     :content "Unknown tool: missing"
                     :metadata (:error e-tool-missing))))))

(ert-deftest e-tools-test-start-delivers-async-tool-result ()
  "Async-only tools deliver structured results through callbacks."
  (let ((registry (e-tools-registry-create))
        result
        request-started)
    (e-tools-test-register registry
                      :name "later"
                      :description "Return later."
                      :start
                      (cl-function
                       (lambda (&key arguments on-done on-error
                                      on-request-start)
                         (ignore on-error)
                         (let ((request
                                (e-tools-request-create
                                 :metadata '(:source async-test))))
                           (setq request-started request)
                           (funcall on-request-start request)
                           (run-at-time
                            0.01 nil
                            (lambda ()
                              (funcall on-done
                                       (plist-get arguments :text))))
                           request))))
    (let ((request
           (e-tools-start
            registry
            '(:id "call-1" :name "later" :arguments (:text "done"))
            :on-done (lambda (value) (setq result value)))))
      (should (e-tools-request-p request))
      (should (eq (plist-get (e-tools-request-metadata request) :request)
                  request-started))
      (should (null result))
      (should (e-tools-test--wait-until (lambda () result)))
      (should (equal result
                     '(:tool-call-id "call-1"
                       :name "later"
                       :status ok
                       :content "done"
                       :metadata nil))))))

(ert-deftest e-tools-test-start-binds-current-context-for-tool-start ()
  "Async tool implementations can inspect the current harness-owned context."
  (let ((registry (e-tools-registry-create))
        seen-context
        result)
    (e-tools-test-register registry
                      :name "context"
                      :description "Capture context."
                      :start
                      (cl-function
                       (lambda (&key on-done &allow-other-keys)
                         (setq seen-context (e-tools-current-context))
                         (funcall on-done "done"))))
    (e-tools-start
     registry
     '(:id "call-1" :name "context" :arguments nil)
     :context '(:session-id "session-1" :turn-id "turn-1")
     :on-done (lambda (value) (setq result value)))
    (should (equal (plist-get seen-context :session-id) "session-1"))
    (should (equal (plist-get seen-context :turn-id) "turn-1"))
    (should (equal (plist-get (plist-get seen-context :tool-call) :id)
                   "call-1"))
    (should (equal result
                   '(:tool-call-id "call-1"
                     :name "context"
                     :status ok
                     :content "done"
                     :metadata nil)))))

(ert-deftest e-tools-test-current-context-summary-omits-live-runtime-objects ()
  "The context summary exposes scalar inspection data without live state."
  (let ((registry (e-tools-registry-create))
        summary)
    (e-tools-test-register registry
                      :name "alpha"
                      :description "Return ok."
                      :handler (lambda (_arguments) "ok"))
    (e-tools-test-register registry
                      :name "inspect"
                      :description "Capture the safe context summary."
                      :start
                      (cl-function
                       (lambda (&key on-done &allow-other-keys)
                         (setq summary
                               (e-tools-current-context-summary))
                         (funcall on-done "ok"))))
    (e-tools-test--execute-with-context
     registry
     '(:id "call-1" :name "inspect" :arguments nil)
     (list :harness (list :live "runtime")
           :session-id "session-1"
           :turn-id "turn-1"
           :deadline 42
           :nested t
           :parent-tool-call-id "parent-1"))
    (should (equal summary
                   '(:session-id "session-1"
                     :turn-id "turn-1"
                     :tool-call-id "call-1"
                     :tool-call-name "inspect"
                     :deadline 42
                     :nested t
                     :parent-tool-call-id "parent-1"
                     :tool-names ["alpha" "inspect"])))
    (should-not (plist-member summary :harness))
    (should-not (plist-member summary :tools))
    (should-not (plist-member summary :capabilities))))

(ert-deftest e-tools-test-start-ignores-request-after-synchronous-settlement ()
  "A request returned after `on-done' must not be published as active work."
  (let ((registry (e-tools-registry-create))
        request
        returned
        result)
    (e-tools-test-register registry
                      :name "immediate"
                      :description "Finish before returning a request."
                      :start
                      (cl-function
                       (lambda (&key on-done &allow-other-keys)
                         (funcall on-done "done")
                         (e-tools-request-create
                          :metadata '(:source stale-after-done)))))
    (setq returned
          (e-tools-start
           registry
           '(:id "call-1" :name "immediate" :arguments nil)
           :on-request-start (lambda (value) (setq request value))
           :on-done (lambda (value) (setq result value))))
    (should (e-tools-request-p returned))
    (should-not request)
    (should (equal result
                   '(:tool-call-id "call-1"
                     :name "immediate"
                     :status ok
                     :content "done"
                     :metadata nil)))))

(ert-deftest e-tools-test-start-passes-through-structured-tool-results ()
  "Async tools can return an already-structured result with metadata."
  (let ((registry (e-tools-registry-create))
        result)
    (e-tools-test-register registry
                      :name "structured"
                      :description "Return structured."
                      :start
                      (cl-function
                       (lambda (&key on-done &allow-other-keys)
                         (funcall on-done
                                  '(:tool-call-id "call-1"
                                    :name "structured"
                                    :status ok
                                    :content "preview"
                                    :metadata (:truncated t))))))
    (e-tools-start
     registry
     '(:id "call-1" :name "structured" :arguments nil)
     :on-done (lambda (value) (setq result value)))
    (should (equal result
                   '(:tool-call-id "call-1"
                     :name "structured"
                     :status ok
                     :content "preview"
                     :metadata (:truncated t))))))

(ert-deftest e-tools-test-start-on-event-does-not-break-legacy-start-tools ()
  "Progress callbacks do not break existing async tools."
  (let ((registry (e-tools-registry-create))
        result)
    (e-tools-test-register registry
                      :name "legacy"
                      :description "Return without progress support."
                      :start
                      (cl-function
                       (lambda (&key arguments on-done on-error
                                      on-request-start)
                         (ignore on-error on-request-start)
                         (funcall on-done
                                  (plist-get arguments :text)))))
    (e-tools-start
     registry
     '(:id "call-1" :name "legacy" :arguments (:text "done"))
     :on-event #'ignore
     :on-done (lambda (value) (setq result value)))
    (should (equal result
                   '(:tool-call-id "call-1"
                     :name "legacy"
                     :status ok
                     :content "done"
                     :metadata nil)))))

(ert-deftest e-tools-test-execute-batch-waits-for-async-only-tool ()
  "The explicit batch execute wrapper waits for async-only tools."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "later"
                      :description "Return later."
                      :start
                      (cl-function
                       (lambda (&key arguments on-done on-error
                                      on-request-start)
                         (ignore on-error on-request-start)
                         (run-at-time
                          0.01 nil
                          (lambda ()
                            (funcall on-done
                                     (plist-get arguments :text))))
                         nil)))
    (should (equal (e-tools-execute-batch
                    registry
                    '(:id "call-1" :name "later" :arguments (:text "done")))
                   '(:tool-call-id "call-1"
                     :name "later"
                     :status ok
                     :content "done"
                     :metadata nil)))))

(ert-deftest e-tools-test-cheap-work-settles-through-canonical-lifecycle ()
  "Cheap work executes synchronously through the canonical work lifecycle."
  (let ((registry (e-tools-registry-create))
        called
        result)
    (e-tools-test-register registry
                      :name "sync"
                      :description "Return now."
                      :handler (lambda (_arguments)
                                 (setq called t)
                                 "now"))
    (let ((request
           (e-tools-start
            registry
            '(:id "call-1" :name "sync" :arguments nil)
            :on-done (lambda (value) (setq result value)))))
      (should (e-tools-request-p request))
      (should called)
      (should (equal (plist-get result :content) "now")))))

(ert-deftest e-tools-test-canonical-carrier-not-legacy-class-drives-dispatch ()
  "A canonical work carrier, not legacy handler inference, drives dispatch."
  (let ((registry (e-tools-registry-create))
        result)
    (e-tools-test-register registry
                      :name "network-only"
                      :description "Pretend to wait on a network."
                      :blocking-class 'network
                      :handler (lambda (_arguments) "late"))
    (e-tools-start
     registry
     '(:id "call-1" :name "network-only" :arguments nil)
     :context '(:interactive t)
     :on-done (lambda (value) (setq result value)))
    (should (equal result
                   '(:tool-call-id "call-1"
                     :name "network-only"
                     :status ok
                     :content "late"
                     :metadata nil)))))

(ert-deftest e-tools-test-long-sync-handler-allowed-for-explicit-batch-execute ()
  "Long blocking sync handlers are only available through explicit batch execute."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "batch-network"
                      :description "Pretend batch network work."
                      :blocking-class 'network
                      :handler (lambda (_arguments) "ok"))
    (should (equal (e-tools-execute-batch
                    registry
                    '(:id "call-1" :name "batch-network" :arguments nil))
                   '(:tool-call-id "call-1"
                     :name "batch-network"
                     :status ok
                     :content "ok"
                     :metadata nil)))))

(ert-deftest e-tools-test-long-async-batch-execute-rejected-in-hot-path ()
  "Batch execution cannot synchronously wait in hot paths."
  (let ((registry (e-tools-registry-create))
        started)
    (e-tools-test-register registry
                      :name "async-network"
                      :description "Pretend async network work."
                      :blocking-class 'network
                      :start (lambda (&rest _args)
                               (setq started t)
                               (e-tools-request-create)))
    (should-error
     (e-request-with-hot-path 'tool-batch-execute
       (e-tools-execute-batch
        registry
        '(:id "call-1" :name "async-network" :arguments nil)))
     :type 'e-tools-batch-execute-not-allowed)
    (should-not started)))

(ert-deftest e-tools-test-long-async-context-execute-rejected-in-hot-path ()
  "Context-aware sync execution cannot wait for long async tools in hot paths."
  (let ((registry (e-tools-registry-create))
        started)
    (e-tools-test-register registry
                      :name "async-process"
                      :description "Pretend async process work."
                      :blocking-class 'process
                      :start (lambda (&rest _args)
                               (setq started t)
                               (e-tools-request-create)))
    (should-error
     (e-request-with-hot-path 'tool-context-execute
       (e-tools--execute-batch-with-context
        registry
        '(:id "call-1" :name "async-process" :arguments nil)
        nil))
     :type 'e-tools-batch-execute-not-allowed)
    (should-not started)))

(ert-deftest e-tools-test-handler-errors-return-structured-results ()
  "Tool handler errors remain structured tool results."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "boom"
                      :description "Fail."
                      :handler (lambda (_arguments)
                                 (error "tool exploded")))
    (should (equal (e-tools-execute-batch
                    registry
                    '(:id "call-1" :name "boom" :arguments nil))
                   '(:tool-call-id "call-1"
                     :name "boom"
                     :status error
                     :content "tool exploded"
                     :metadata (:error error))))))

(ert-deftest e-tools-test-handler-noncanonical-result-is-a-tool-error ()
  "A handler cannot leak arbitrary Elisp through structured model content."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                           :name "bad-result"
                           :description "Return a noncanonical object."
                           :handler (lambda (_arguments)
                                      '(:kind file)))
    (let ((result (e-tools-execute-batch
                   registry
                   '(:id "call-1" :name "bad-result" :arguments nil))))
      (should (equal (plist-get result :status) 'error))
      (should (equal (plist-get (plist-get result :metadata) :error)
                     'e-tools-invalid-result-content))
      (should (string-match-p "canonical JSON"
                              (plist-get result :content))))))

(ert-deftest e-tools-test-handler-quit-returns-structured-results ()
  "Tool handler quits remain structured tool results."
  (let ((registry (e-tools-registry-create))
        result)
    (e-tools-test-register registry
                      :name "quit"
                      :description "Quit."
                      :handler (lambda (_arguments)
                                 (signal 'quit nil)))
    (e-tools-start
     registry
     '(:id "call-1" :name "quit" :arguments nil)
     :on-done (lambda (value) (setq result value)))
    (should (e-tools-test--wait-until (lambda () result)))
    (should (equal result
                   '(:tool-call-id "call-1"
                     :name "quit"
                     :status error
                     :content "Quit"
                     :metadata (:error quit))))))

(ert-deftest e-tools-test-start-deadline-settles-stalled-legacy-tool ()
  "Legacy async :start tools inherit context deadlines and fail visibly."
  (let ((registry (e-tools-registry-create))
        cancelled
        result)
    (e-tools-test-register
     registry
     :name "stall"
     :description "Never calls back."
     :start (cl-function
             (lambda (&key on-request-start &allow-other-keys)
               (funcall
                on-request-start
                (e-tools-request-create
                 :cancel (lambda ()
                           (setq cancelled t)
                           t)))
               nil)))
    (e-tools-start
     registry
     '(:id "call-1" :name "stall" :arguments nil)
     :context (list :deadline (+ (float-time) 0.02))
     :on-done (lambda (value)
                (setq result value)))
    (should (e-tools-test--wait-until (lambda () result) 1))
    (should cancelled)
    (should (equal (plist-get result :status) 'error))
    (should (eq (plist-get (plist-get result :metadata) :error)
                'e-work-deadline-exceeded))
    (should (string-match-p "deadline" (plist-get result :content)))))

(ert-deftest e-tools-test-current-registry-requires-tool-context ()
  "Nested tool APIs fail clearly outside an active tool context."
  (should-error (e-tools-current-registry)
                :type 'e-tools-no-active-registry)
  (should-error (e-tools-call "missing" nil)
                :type 'e-tools-no-active-registry))

(ert-deftest e-tools-test-available-lists-active-tools-from-context ()
  "Nested tool code can inspect compact active tool descriptors."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "outer"
                      :description "Return available tools."
                      :metadata '(:capability test)
                      :handler (lambda (_arguments)
                                 (e-tools-available)))
    (e-tools-test-register registry
                      :name "inner"
                      :description "Inner tool."
                      :parameters '(:type "object"
                                    :properties (:text (:type "string")))
                      :metadata '(:capability nested)
                      :handler (lambda (_arguments)
                                 "inner"))
    (should
     (equal (plist-get
             (e-tools-test--execute-with-context
              registry
              '(:id "outer-1" :name "outer" :arguments nil)
              (list :tools registry))
             :content)
            [(:name "outer"
               :description "Return available tools."
               :parameters (:type "object" :properties nil))
              (:name "inner"
               :description "Inner tool."
               :parameters (:type "object"
                            :properties (:text (:type "string"))))]))))

(ert-deftest e-tools-test-call-executes-active-tool-from-context ()
  "Nested tool code can call another active tool and receive its result."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "outer"
                      :description "Call inner."
                      :handler (lambda (_arguments)
                                 (let ((result (e-tools-call "inner"
                                                              '(:text "hi"))))
                                   (list :tool-name (plist-get result :name)
                                         :status (symbol-name
                                                  (plist-get result :status))
                                         :content (plist-get result :content)))))
    (e-tools-test-register registry
                      :name "inner"
                      :description "Return text."
                      :handler (lambda (arguments)
                                 (upcase (plist-get arguments :text))))
    (should
     (equal (plist-get
             (e-tools-test--execute-with-context
              registry
              '(:id "outer-1" :name "outer" :arguments nil)
              (list :tools registry))
             :content)
            '(:tool-name "inner" :status "ok" :content "HI")))))

(ert-deftest e-tools-test-call-rejects-long-nested-tool-without-executor ()
  "Default nested calls fail fast for long-class tools."
  (let ((registry (e-tools-registry-create))
        started)
    (e-tools-test-register registry
                      :name "outer"
                      :description "Call long inner."
                      :handler (lambda (_arguments)
                                 (let ((result (e-tools-call "inner" nil)))
                                   (list :status (symbol-name
                                                  (plist-get result :status))
                                         :message (plist-get result :content)))))
    (e-tools-test-register registry
                      :name "inner"
                      :description "Long async inner."
                      :blocking-class 'process
                      :start
                      (lambda (&rest _args)
                        (setq started t)
                        nil))
    (should
     (equal (plist-get
             (e-tools-test--execute-with-context
              registry
              '(:id "outer-1" :name "outer" :arguments nil)
              (list :tools registry))
             :content)
            '(:status "error"
              :message "Nested tool inner is process-class and cannot run synchronously inside another tool; call it as a top-level tool instead.")))
    (should-not started)))

(ert-deftest e-tools-test-call-bang-returns-content-or-signals-tool-error ()
  "The bang variant unwraps ok content and signals structured tool errors."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "outer"
                      :description "Call inner."
                      :handler
                      (lambda (_arguments)
                        (list :ok (e-tools-call! "inner" '(:text "hi"))
                              :error (condition-case err
                                         (e-tools-call! "missing" nil)
                                       (e-tools-nested-tool-error
                                        (let ((result (cadr err)))
                                          (list :name (plist-get result :name)
                                                :status (symbol-name
                                                         (plist-get result :status))
                                                :content (plist-get result :content)
                                                :error "e-tool-missing")))))))
    (e-tools-test-register registry
                      :name "inner"
                      :description "Return text."
                      :handler (lambda (arguments)
                                 (plist-get arguments :text)))
    (should
     (equal (plist-get
             (e-tools-test--execute-with-context
              registry
              '(:id "outer-1" :name "outer" :arguments nil)
              (list :tools registry))
             :content)
            '(:ok "hi"
              :error (:name "missing"
                      :status "error"
                      :content "Unknown tool: missing"
                      :error "e-tool-missing"))))))

(ert-deftest e-tools-test-call-rejects-recursive-self-call-by-default ()
  "Nested calls reject accidental recursion into the current tool."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "outer"
                      :description "Call itself."
                      :handler (lambda (_arguments)
                                 (e-tools-call "outer" nil)))
    (should
     (equal (e-tools-test--execute-with-context
             registry
             '(:id "outer-1" :name "outer" :arguments nil)
             (list :tools registry))
            '(:tool-call-id "outer-1"
              :name "outer"
              :status error
              :content "Recursive nested tool call rejected: outer"
              :metadata (:error e-tools-recursive-call))))))

(ert-deftest e-tools-test-call-can-explicitly-allow-recursive-name ()
  "Callers can opt into a same-name nested call for deliberate cases."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "outer"
                      :description "Call itself when requested."
                      :handler
                      (lambda (arguments)
                        (if (plist-get arguments :inner)
                            "inner"
                          (let ((result (e-tools-call
                                         "outer"
                                         '(:inner t)
                                         '(:allow-recursive t))))
                            (plist-get result :content)))))
    (should
     (equal (plist-get
             (e-tools-test--execute-with-context
              registry
              '(:id "outer-1" :name "outer" :arguments nil)
              (list :tools registry))
             :content)
            "inner"))))

(ert-deftest e-tools-test-call-enforces-default-nested-budget ()
  "Nested tool calls are bounded by the context budget."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register registry
                      :name "outer"
                      :description "Call many tools."
                      :handler (lambda (_arguments)
                                 (dotimes (_ 21)
                                   (e-tools-call! "inner" nil))
                                 "unreached"))
    (e-tools-test-register registry
                      :name "inner"
                      :description "Return ok."
                      :handler (lambda (_arguments) "ok"))
    (should
     (equal (e-tools-test--execute-with-context
             registry
             '(:id "outer-1" :name "outer" :arguments nil)
             (list :tools registry))
            '(:tool-call-id "outer-1"
              :name "outer"
              :status error
              :content "Nested tool call budget exceeded"
              :metadata (:error e-tools-nested-tool-budget-exceeded))))))

(ert-deftest e-tools-test-call-preserves-harness-session-turn-context ()
  "Nested calls receive the original harness, session, and turn context."
  (let ((registry (e-tools-registry-create))
        inner-context)
    (e-tools-test-register registry
                      :name "outer"
                      :description "Call inner."
                      :handler (lambda (_arguments)
                                 (e-tools-call! "inner" nil)))
    (e-tools-test-register registry
                      :name "inner"
                      :description "Capture context."
                      :handler (lambda (_arguments)
                                 (setq inner-context
                                       (e-tools-current-context))
                                 "ok"))
    (e-tools-test--execute-with-context
     registry
     '(:id "outer-1" :name "outer" :arguments nil)
     (list :tools registry
           :harness 'harness
           :session-id "session-1"
           :turn-id "turn-1"))
    (should (equal (plist-get inner-context :harness) 'harness))
    (should (equal (plist-get inner-context :session-id) "session-1"))
    (should (equal (plist-get inner-context :turn-id) "turn-1"))
    (should (equal (plist-get (plist-get inner-context :tool-call) :id)
                   "outer-1/nested-1"))))

(ert-deftest e-tools-test-start-malformed-argument-fails-as-tool-result ()
  "A noncanonical argument yields a tool-error result, not a turn abort."
  (let ((registry (e-tools-registry-create))
        result)
    (e-tools-test-register registry
                      :name "edit"
                      :description "Reject malformed edits."
                      :parameters '(:type "object"
                                    :properties (:edits (:type "array")))
                      :handler (lambda (arguments)
                                 ;; A well-formed call would pass a list; a
                                 ;; malformed string must not reach here as an
                                 ;; unhandled signal.
                                 (unless (listp (plist-get arguments :edits))
                                   (signal 'wrong-type-argument
                                           (list 'listp
                                                 (plist-get arguments :edits))))
                                 "ok"))
    (e-tools-start
     registry
     '(:id "call-1" :name "edit"
       :arguments (:edits "[{\"oldText\": \"a\", "))
     :on-done (lambda (value) (setq result value)))
    (e-tools-test--wait-until (lambda () result))
    (should result)
    (should (eq (plist-get result :status) 'error))))

(ert-deftest e-tools-test-register-rejects-noncanonical-schemas ()
  "Registration rejects alternate object and array containers in schemas."
  (let ((registry (e-tools-registry-create))
        (work (e-tools-cheap-work "test.schema" (lambda (_arguments) "ok"))))
    (dolist (schema (list '(:type "object" :properties ((:text . (:type "string"))))
                       '(:type "object" :properties #s(hash-table test equal data ()))
                       '(:type "object" :properties (:items (:type "array" :items ((:type "string")))))))
      (should-error
       (e-tools-register registry :name "invalid" :description "Invalid."
                         :parameters schema :work work)
       :type 'e-tools-invalid-definition))))

(ert-deftest e-tools-test-prepare-call-rejects-noncanonical-json-shapes ()
  "Dispatch rejects list arrays, stringified objects, and hash objects."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register
     registry :name "inspect" :description "Inspect input."
     :parameters (list :type "object"
                       :properties
                       (list :items (list :type "array"
                                           :items '(:type "object"
                                                    :properties (:value
                                                                 (:type "string"))))
                             :object '(:type "object"
                                       :properties (:value (:type "string")))))
     :handler (lambda (_arguments) "ok"))
    (let ((hash (make-hash-table :test 'equal)))
      (puthash "value" "x" hash)
      (dolist (arguments (list '(:items ((:value "x")))
                              '(:items "[{\"value\":\"x\"}]")
                              (list :object hash)))
        (should-error
         (e-tools-prepare-call
          registry (list :id "call-1" :name "inspect" :arguments arguments))
         :type 'e-tools-invalid-arguments)))))

(ert-deftest e-tools-test-start-preserves-canonical-arguments-for-dispatch ()
  "A provider-decoded canonical argument reaches the handler unchanged."
  (let* ((registry (e-tools-registry-create))
         seen
         (arguments '(:items [(:value "x" :missing :json-null)]
                      :object (:value "y" :empty [] :false :json-false)))
         (schema (list :type "object"
                       :properties
                       (list :items (list :type "array"
                                           :items (list :type "object"
                                                         :properties
                                                         (list :value
                                                               '(:type "string")
                                                               :missing
                                                               '(:type "null"))))
                             :object (list :type "object"
                                           :properties
                                           (list :value '(:type "string")
                                                 :empty '(:type "array")
                                                 :false '(:type "boolean")))))))
    (e-tools-test-register
     registry :name "inspect" :description "Capture canonical input."
     :parameters schema
     :handler (lambda (value) (setq seen value) "ok"))
    (e-tools-execute-batch
     registry (list :id "call-1" :name "inspect" :arguments arguments))
    (should (equal seen arguments))))

(ert-deftest e-tools-test-prepare-call-validates-supported-schema-keywords ()
  "Runtime validation covers compact consumer schemas before dispatch."
  (let ((registry (e-tools-registry-create)))
    (e-tools-test-register
     registry
     :name "compact"
     :description "Validate compact input."
     :parameters
     '(:type "object"
       :properties
       (:signal (:type "string" :enum ["ok"])
        :note (:type "string" :minLength 1 :maxLength 5
               :nonBlank t :singleLine t)
        :count (:type "integer")
        :enabled (:type "boolean"))
       :required ["signal" "note"]
       :additionalProperties :json-false)
     :handler (lambda (_arguments) "ok"))
    (should
     (equal
      (plist-get
       (e-tools-prepare-call
        registry
        '(:id "call-1" :name "compact"
          :arguments (
                      :signal "ok" :note "short" :count 1 :enabled t)))
       :arguments)
      '(:signal "ok" :note "short" :count 1 :enabled t)))
    (dolist (arguments
             '(()
               ( :note "short")
               ( :signal 1 :note "short")
               ( :signal "bad" :note "short")
               ( :signal "ok" :note "")
               ( :signal "ok" :note "      ")
               ( :signal "ok" :note "longer")
               ( :signal "ok" :note "a\nb")
               ( :signal "ok" :note "short" :count 1.5)
               ( :signal "ok" :note "short" :enabled yes)
               ( :signal "ok" :note "short" :extra t)))
      (should-error
       (e-tools-prepare-call
        registry (list :id "call-1" :name "compact" :arguments arguments))
       :type 'e-tools-invalid-arguments))))

(provide 'e-tools-test)

;;; e-tools-test.el ends here
