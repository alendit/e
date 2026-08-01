;;; e-await-tool-test.el --- Tests for the model-facing await tool -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the await tool over a fake waitable scheme.

;;; Code:

(require 'ert)
(require 'e-await-tool)
(require 'e-tools)
(require 'e-waitable)
(require 'e-work)

(defun e-await-tool-test--pending-handle ()
  "Return a fresh non-terminal handle on the render carrier."
  (e-work-start
   (e-work-spec-create
    :id "await-test-pending"
    :execution 'render
    :interactive-policy 'async
    :runner (lambda (_arguments _context) :never))
   '(:delay 600)))

(defmacro e-await-tool-test--with-scheme (bindings &rest body)
  "Run BODY with a clean resolver registry and BINDINGS registered.
BINDINGS is an alist of (LOCAL-ID . HANDLE) under the \"fake\" scheme."
  (declare (indent 1))
  `(let ((e-waitable--resolvers (make-hash-table :test 'equal))
         (table (make-hash-table :test 'equal)))
     (dolist (entry ,bindings)
       (puthash (car entry) (cdr entry) table))
     (e-waitable-register-resolver
      "fake" (lambda (id) (gethash id table)))
     ,@body))

(defun e-await-tool-test--run (arguments)
  "Run the await tool with ARGUMENTS and return its structured result."
  (let ((registry (e-tools-registry-create))
        result)
    (e-await-tool-register registry)
    (e-tools-start
     registry
     (list :id "call-1" :name "await" :arguments arguments)
     :on-done (lambda (value) (setq result value)))
    result))

(ert-deftest e-await-tool-test-registered-as-model-facing-tool ()
  "Await is a model-facing tool (unlike the subagents actions)."
  (let ((registry (e-tools-registry-create)))
    (e-await-tool-register registry)
    (should (gethash "await" (e-tools-registry-tools registry)))))

(ert-deftest e-await-tool-test-is-an-invocation-only-aggregation ()
  "Await does not create synthetic executable work for its aggregation."
  (let ((registry (e-tools-registry-create)))
    (e-await-tool-register registry)
    (let ((tool (gethash "await" (e-tools-registry-tools registry))))
      (should (plist-get tool :invocation-only))
      (should (functionp (plist-get tool :start)))
      (should-not (plist-get tool :work)))))

(ert-deftest e-await-tool-test-uses-injected-board-aggregation-port ()
  "Await delegates live aggregation to the injected board subscription port."
  (let ((handle (e-await-tool-test--pending-handle)))
    (unwind-protect
        (e-await-tool-test--with-scheme (list (cons "a" handle))
          (let ((registry (e-tools-registry-create))
                received result)
            (e-await-tool-register registry)
            (e-tools-start
             registry
             '(:id "c" :name "await" :arguments (:refs ["fake:a"]))
             :context
             (list :board-subscribe-aggregation
                   (lambda (handles mode timeout callback)
                     (setq received (list handles mode timeout))
                     (funcall callback 'complete)
                     (lambda () t)))
             :on-done (lambda (value) (setq result value)))
            (should (eq (car (car received)) handle))
            (should (eq (cadr received) 'all))
            (should (plist-get (plist-get result :content) :settled))))
      (e-work-cancel handle))))

(ert-deftest e-await-tool-test-schema-stays-minimal ()
  "Await relies on async action guidance instead of duplicating it in schema."
  (let ((registry (e-tools-registry-create)))
    (e-await-tool-register registry)
    (let* ((tool (gethash "await" (e-tools-registry-tools registry)))
           (parameters (plist-get tool :parameters))
           (properties (plist-get parameters :properties)))
      (should (equal (plist-get tool :description)
                     "Wait for async work references to settle without blocking Emacs."))
      (should (equal (plist-get parameters :required) ["refs"]))
      (should-not (plist-get (plist-get properties :refs) :description))
      (should-not (plist-get (plist-get properties :mode) :description))
      (should-not (plist-get (plist-get properties :timeout) :description)))))

(ert-deftest e-await-tool-test-settles-on-terminal ()
  "Await settles with a report when all references become terminal."
  (let ((a (e-await-tool-test--pending-handle)))
    (unwind-protect
        (e-await-tool-test--with-scheme (list (cons "a" a))
          (let* ((registry (e-tools-registry-create))
                 result)
            (e-await-tool-register registry)
            (e-tools-start
             registry
             '(:id "c" :name "await" :arguments (:refs ["fake:a"] :timeout 30))
             :on-done (lambda (v) (setq result v)))
            ;; Async: not settled until the handle finishes.
            (should-not result)
            (e-work-finish a '(:summary "done" :outputs [:x]))
            (let ((content (plist-get result :content)))
              (should (plist-get content :settled))
              (should (eq (plist-get content :reason) 'complete))
              (let ((entry (car (plist-get content :results))))
                (should (equal (plist-get entry :ref) "fake:a"))
                (should (eq (plist-get entry :state) 'finished))
                (should (equal (plist-get entry :summary) "done"))))))
      (e-work-cancel a))))

(ert-deftest e-await-tool-test-timeout-reports-pending ()
  "On timeout the report is unsettled and lists the pending reference."
  (let ((a (e-await-tool-test--pending-handle)))
    (unwind-protect
        (e-await-tool-test--with-scheme (list (cons "a" a))
          (let* ((registry (e-tools-registry-create))
                 result)
            (e-await-tool-register registry)
            (e-tools-start
             registry
             '(:id "c" :name "await" :arguments (:refs ["fake:a"] :timeout 0.05))
             :on-done (lambda (v) (setq result v)))
            (sleep-for 0.2)
            (let ((content (plist-get result :content)))
              (should-not (plist-get content :settled))
              (should (eq (plist-get content :reason) 'timed-out))
              (should (memq (plist-get (car (plist-get content :results)) :state)
                            '(started progress))))))
      (e-work-cancel a))))

(ert-deftest e-await-tool-test-unknown-reference-is-per-reference-error ()
  "An unknown reference is a report entry, not a whole-call failure."
  (let ((a (e-await-tool-test--pending-handle)))
    (unwind-protect
        (e-await-tool-test--with-scheme (list (cons "a" a))
          (let* ((registry (e-tools-registry-create))
                 result)
            (e-await-tool-register registry)
            (e-tools-start
             registry
             '(:id "c" :name "await"
               :arguments (:refs ["fake:a" "fake:missing" "bogus"] :timeout 30))
             :on-done (lambda (v) (setq result v)))
            ;; The resolvable reference still gates completion.
            (should-not result)
            (e-work-finish a '(:summary "ok"))
            (let* ((content (plist-get result :content))
                   (results (plist-get content :results))
                   (errors (cl-remove-if-not
                            (lambda (r) (eq (plist-get r :status) 'error))
                            results)))
              (should (plist-get content :settled))
              (should (= (length errors) 2)))))
      (e-work-cancel a))))

(ert-deftest e-await-tool-test-any-mode-settles-at-first ()
  "MODE any settles at the first terminal reference."
  (let ((a (e-await-tool-test--pending-handle))
        (b (e-await-tool-test--pending-handle)))
    (unwind-protect
        (e-await-tool-test--with-scheme (list (cons "a" a) (cons "b" b))
          (let* ((registry (e-tools-registry-create))
                 result)
            (e-await-tool-register registry)
            (e-tools-start
             registry
             '(:id "c" :name "await"
               :arguments (:refs ["fake:a" "fake:b"] :mode "any" :timeout 30))
             :on-done (lambda (v) (setq result v)))
            (should-not result)
            (e-work-finish a '(:summary "first"))
            (should (plist-get (plist-get result :content) :settled))))
      (e-work-cancel a)
      (e-work-cancel b))))

(provide 'e-await-tool-test)

;;; e-await-tool-test.el ends here
