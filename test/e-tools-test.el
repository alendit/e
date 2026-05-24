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

(defun e-tools-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(ert-deftest e-tools-test-register-and-execute ()
  "Registered tools execute through structured calls."
  (let ((registry (e-tools-registry-create)))
    (e-tools-register registry
                      :name "echo"
                      :description "Return the input text."
                      :handler (lambda (arguments)
                                 (plist-get arguments :text)))
    (should (equal (e-tools-execute registry
                                    '(:id "call-1" :name "echo" :arguments (:text "hi")))
                   '(:tool-call-id "call-1"
                     :name "echo"
                     :status ok
                     :content "hi"
                     :metadata nil)))))

(ert-deftest e-tools-test-definitions-are-backend-neutral-function-tools ()
  "Registered tools expose backend-neutral function definitions."
  (let ((registry (e-tools-registry-create)))
    (e-tools-register registry
                      :name "noop"
                      :description "Accept no arguments."
                      :parameters '(:type "object" :properties nil)
                      :handler (lambda (_arguments) "now"))
    (let* ((definitions (e-tools-definitions registry))
           (parameters (plist-get (car definitions) :parameters)))
      (should (equal (plist-get parameters :type) "object"))
      (should (hash-table-p (plist-get parameters :properties)))
      (should (string-match-p
               "\"properties\":{}"
               (json-encode definitions)))
      (should (equal (car definitions)
                     `(:type "function"
                       :name "noop"
                       :description "Accept no arguments."
                       :parameters ,parameters
                       :strict :json-false))))))

(ert-deftest e-tools-test-result-content-text-serializes-structured-content ()
  "Tool result content text is the provider-visible representation."
  (should (equal (e-tools-result-content-text "plain")
                 "plain"))
  (should (equal (e-tools-result-content-text '(:ok t :items [1 2]))
                 "{\"items\":[1,2],\"ok\":true}")))

(ert-deftest e-tools-test-missing-tool-returns-structured-error ()
  "Unknown tools return structured error results."
  (let ((registry (e-tools-registry-create)))
    (should (equal (e-tools-execute registry
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
    (e-tools-register registry
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
      (should (eq request request-started))
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
    (e-tools-register registry
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

(ert-deftest e-tools-test-start-passes-through-structured-tool-results ()
  "Async tools can return an already-structured result with metadata."
  (let ((registry (e-tools-registry-create))
        result)
    (e-tools-register registry
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

(ert-deftest e-tools-test-execute-waits-for-async-only-tool ()
  "The sync execute wrapper waits for async-only tools."
  (let ((registry (e-tools-registry-create)))
    (e-tools-register registry
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
    (should (equal (e-tools-execute
                    registry
                    '(:id "call-1" :name "later" :arguments (:text "done")))
                   '(:tool-call-id "call-1"
                     :name "later"
                     :status ok
                     :content "done"
                     :metadata nil)))))

(ert-deftest e-tools-test-start-adapts-sync-handler-and-can-cancel-queued ()
  "Sync handlers can start asynchronously and be cancelled before execution."
  (let ((registry (e-tools-registry-create))
        called
        result)
    (e-tools-register registry
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
      (should (e-tools-cancel-request request))
      (accept-process-output nil 0.05)
      (should (null called))
      (should (null result)))))

(ert-deftest e-tools-test-handler-errors-return-structured-results ()
  "Tool handler errors remain structured tool results."
  (let ((registry (e-tools-registry-create)))
    (e-tools-register registry
                      :name "boom"
                      :description "Fail."
                      :handler (lambda (_arguments)
                                 (error "tool exploded")))
    (should (equal (e-tools-execute
                    registry
                    '(:id "call-1" :name "boom" :arguments nil))
                   '(:tool-call-id "call-1"
                     :name "boom"
                     :status error
                     :content "tool exploded"
                     :metadata (:error error))))))

(ert-deftest e-tools-test-handler-quit-returns-structured-results ()
  "Tool handler quits remain structured tool results."
  (let ((registry (e-tools-registry-create))
        result)
    (e-tools-register registry
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

(provide 'e-tools-test)

;;; e-tools-test.el ends here
