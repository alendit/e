;;; e-harness-nested-tools-test.el --- Harness nested tool tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Focused tests for harness-specific nested tool execution policy.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-tools)
(load (expand-file-name "e-tools-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(ert-deftest e-harness-nested-tools-test-rejects-long-nested-tool ()
  "Long nested tools return a structured error instead of blocking."
  (let* ((harness (e-harness-create
                   :backend (e-backend-create :name 'unused)))
         (registry (e-tools-registry-create))
         (started-long nil)
         result)
    (e-harness-create-session harness :id "session-1")
    (e-tools-test-register
     registry
     :name "outer"
     :description "Call long nested tool."
     :handler
     (lambda (_arguments)
       (condition-case err
           (e-tools-call! "long_network_tool" nil)
         (e-tools-nested-tool-error
          (let ((nested (cadr err)))
            (list :status (symbol-name (plist-get nested :status))
                  :content (plist-get nested :content)
                  :metadata
                  (list :error
                        (symbol-name
                         (plist-get (plist-get nested :metadata) :error))
                        :blocking-class
                        (symbol-name
                         (plist-get (plist-get nested :metadata)
                                    :blocking-class)))))))))
    (e-tools-test-register
     registry
     :name "long_network_tool"
     :description "Long network tool."
     :blocking-class 'network
     :start
     (cl-function
      (lambda (&key on-done &allow-other-keys)
        (setq started-long t)
        (funcall on-done "late"))))
    (e-tools-start
     registry
     '(:id "outer-1" :name "outer" :arguments nil)
     :context
     (list :harness harness
           :session-id "session-1"
           :turn-id "turn-1"
           :tools registry
           :tool-executor
           (lambda (call options context)
             (e-harness-turn--execute-nested-tool
              harness "session-1" "turn-1" registry call options context)))
     :on-done (lambda (value) (setq result value)))
    (let ((deadline (+ (float-time) 1)))
      (while (and (not result) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should-not started-long)
    (should (equal (plist-get result :status) 'ok))
    (let ((nested (plist-get result :content)))
      (should (equal (plist-get nested :status) "error"))
      (should (string-match-p "cannot run synchronously inside another tool"
                              (plist-get nested :content)))
      (should (equal (plist-get nested :metadata)
                     '(:error "e-nested-long-tool-rejected"
                       :blocking-class "network"))))))

(ert-deftest e-harness-nested-tools-test-rejects-async-nested-tool ()
  "Async-only nested tools return a structured error and are not started."
  (let* ((harness (e-harness-create
                   :backend (e-backend-create :name 'unused)))
         (registry (e-tools-registry-create))
         (started-async nil)
         result)
    (e-harness-create-session harness :id "session-1")
    (e-tools-test-register
     registry
     :name "outer"
     :description "Call async nested tool."
     :handler
     (lambda (_arguments)
       (condition-case err
           (e-tools-call! "async_tool" nil)
         (e-tools-nested-tool-error
          (let ((nested (cadr err)))
            (list :status (symbol-name (plist-get nested :status))
                  :content (plist-get nested :content)
                  :metadata
                  (list :error
                        (symbol-name
                         (plist-get (plist-get nested :metadata) :error))
                        :blocking-class
                        (symbol-name
                         (plist-get (plist-get nested :metadata)
                                    :blocking-class)))))))))
    (e-tools-test-register
     registry
     :name "async_tool"
     :description "Async tool."
     :blocking-class 'cheap
     :start
     (cl-function
      (lambda (&key on-done &allow-other-keys)
        (setq started-async t)
        (funcall on-done "late"))))
    (e-tools-start
     registry
     '(:id "outer-1" :name "outer" :arguments nil)
     :context
     (list :harness harness
           :session-id "session-1"
           :turn-id "turn-1"
           :tools registry
           :tool-executor
           (lambda (call options context)
             (e-harness-turn--execute-nested-tool
              harness "session-1" "turn-1" registry call options context)))
     :on-done (lambda (value) (setq result value)))
    (let ((deadline (+ (float-time) 1)))
      (while (and (not result) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should-not started-async)
    (should (equal (plist-get result :status) 'ok))
    (let ((nested (plist-get result :content)))
      (should (equal (plist-get nested :status) "error"))
      (should (string-match-p "async-backed"
                              (plist-get nested :content)))
      (should (equal (plist-get nested :metadata)
                     '(:error "e-tools-nested-async-tool-rejected"
                       :blocking-class "cheap"))))))

(provide 'e-harness-nested-tools-test)

;;; e-harness-nested-tools-test.el ends here
