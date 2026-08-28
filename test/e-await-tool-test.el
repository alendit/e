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

(ert-deftest e-await-tool-test-is-an-invocation-subscription-work ()
  "Await does not create synthetic executable work for its aggregation."
  (let ((registry (e-tools-registry-create)))
    (e-await-tool-register registry)
    (let ((tool (gethash "await" (e-tools-registry-tools registry))))
      (should-not (plist-get tool :invocation-only))
      (should-not (plist-get tool :start))
      (should (e-work-spec-p (plist-get tool :work)))
      (should (equal (e-work-spec-id (plist-get tool :work))
                     "tool.await.invocation-subscription")))))

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
                   (lambda (handles mode timeout callback &optional context)
                     (setq received (list handles mode timeout))
                     (should (plist-get context :tool-call))
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

(ert-deftest e-await-tool-test-unknown-reference-rejects-the-whole-request ()
  "Every await reference must resolve before any subscription is installed."
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
            (should (eq (plist-get result :status) 'error))
            (should (eq (plist-get (plist-get result :metadata) :error)
                        'e-await-tool-invalid-request))))
      (e-work-cancel a))))

(ert-deftest e-await-tool-test-invalid-explicit-timeout-rejects ()
  "Await rejects invalid explicit timeout values instead of clamping them."
  (let ((handle (e-await-tool-test--pending-handle)))
    (unwind-protect
        (e-await-tool-test--with-scheme (list (cons "a" handle))
          (let ((result (e-await-tool-test--run
                         '(:refs ["fake:a"] :timeout 901))))
            (should (eq (plist-get result :status) 'error))
            (should (eq (plist-get (plist-get result :metadata) :error)
                        'e-await-tool-invalid-request))))
      (e-work-cancel handle))))

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

(ert-deftest e-await-tool-test-rejects-reference-set-over-hard-cap ()
  "Reference resolution work is bounded before any resolver is called."
  (let ((calls 0)
        result)
    (let ((e-waitable--resolvers (make-hash-table :test 'equal)))
      (e-waitable-register-resolver
       "fake" (lambda (_id) (setq calls (1+ calls)) nil))
      (setq result
            (e-await-tool-test--run
             (list :refs
                   (vconcat
                    (mapcar (lambda (index) (format "fake:%d" index))
                            (number-sequence 0 e-await-tool-max-references)))))))
    (should (= calls 0))
    (should (eq (plist-get result :status) 'error))))

(ert-deftest e-await-tool-test-large-results-become-reference-tombstones ()
  "Await never embeds a result beyond its fixed callback report budget."
  (let ((handle (e-await-tool-test--pending-handle)))
    (unwind-protect
        (e-await-tool-test--with-scheme (list (cons "large" handle))
          (let* ((registry (e-tools-registry-create))
                 result)
            (e-await-tool-register registry)
            (e-tools-start
             registry
             '(:id "c" :name "await" :arguments (:refs ["fake:large"]))
             :on-done (lambda (value) (setq result value)))
            (e-work-finish
             handle
             (make-string (1+ e-await-tool-max-inline-result-bytes) ?x))
            (let* ((entry (car (plist-get (plist-get result :content) :results)))
                   (reported (plist-get entry :result)))
              (should (plist-get reported :omitted))
              (should (equal (plist-get reported :result-ref) "fake:large")))))
      (e-work-cancel handle))))

(ert-deftest e-await-tool-test-file-content-never-exposes-host-path ()
  "Await tombstones file-backed content even when its carrier is small."
  (let ((handle (e-await-tool-test--pending-handle))
        (path "/private/opaque/tool-output.txt"))
    (unwind-protect
        (e-await-tool-test--with-scheme (list (cons "file" handle))
          (let ((registry (e-tools-registry-create))
                result)
            (e-await-tool-register registry)
            (e-tools-start
             registry
             '(:id "c" :name "await" :arguments (:refs ["fake:file"]))
             :on-done (lambda (value) (setq result value)))
            (e-work-finish
             handle
             (e-tools-file-content-create
              :path path :preview "small" :original-bytes 1000000
              :original-lines 1 :preview-bytes 5 :preview-lines 1))
            (let* ((entry (car (plist-get (plist-get result :content) :results)))
                   (reported (plist-get entry :result)))
              (should (plist-get reported :omitted))
              (should-not (string-match-p
                           (regexp-quote path) (prin1-to-string entry))))))
      (e-work-cancel handle))))

(provide 'e-await-tool-test)

;;; e-await-tool-test.el ends here

(ert-deftest e-await-tool-test-timeout-includes-progress-without-cancelling ()
  "A timed-out await exposes pending evidence and leaves the work live."
  (let ((handle (e-await-tool-test--pending-handle)))
    (unwind-protect
        (e-await-tool-test--with-scheme (list (cons "a" handle))
          (e-work-progress handle
                           '(:subagent-id "sub_000001" :sequence 7
                             :event tool-finished :summary "Finished focused ERT"
                             :at 0.0))
          (let* ((registry (e-tools-registry-create))
                 result)
            (e-await-tool-register registry)
            (e-tools-start
             registry
             '(:id "c" :name "await" :arguments (:refs ["fake:a"] :timeout 0.05))
             :on-done (lambda (value) (setq result value)))
            (sleep-for 0.2)
            (let ((entry (car (plist-get (plist-get result :content) :results))))
              (should (eq (plist-get (plist-get result :content) :reason) 'timed-out))
              (should (= (plist-get entry :progress-sequence) 7))
              (should (equal (plist-get (plist-get entry :progress) :summary)
                             "Finished focused ERT"))
              (should (numberp (plist-get entry :progress-age-seconds)))
              (should (memq (plist-get (e-work-status handle) :state)
                            '(started progress))))))
      (e-work-cancel handle))))

(ert-deftest e-await-tool-test-long-operation-crosses-supervision-windows-without-cancellation ()
  "Each timed-out supervision window leaves a progressing operation live."
  (let ((handle (e-await-tool-test--pending-handle)))
    (unwind-protect
        (e-await-tool-test--with-scheme (list (cons "long" handle))
          (cl-labels ((window ()
                        (let ((registry (e-tools-registry-create))
                              callback result)
                          (e-await-tool-register registry)
                          (e-tools-start
                           registry
                           '(:id "window" :name "await"
                             :arguments (:refs ["fake:long"] :timeout 90))
                           :context
                           (list :board-subscribe-aggregation
                                 (lambda (_handles _mode _timeout settle &optional _context)
                                   (setq callback settle)
                                   (lambda () t)))
                           :on-done (lambda (value) (setq result value)))
                          (funcall callback 'timed-out)
                          result)))
            (e-work-progress handle
                             '(:subagent-id "sub_000001" :sequence 1
                               :event tool-started :summary "Started long build" :at 0.0))
            (let ((first (window)))
              (should (eq (plist-get (plist-get first :content) :reason) 'timed-out))
              (should (= (plist-get (car (plist-get (plist-get first :content) :results))
                                    :progress-sequence)
                         1)))
            (e-work-progress handle
                             '(:subagent-id "sub_000001" :sequence 2
                               :event tool-finished :summary "Finished build phase" :at 1.0))
            (let ((second (window)))
              (should (eq (plist-get (plist-get second :content) :reason) 'timed-out))
              (should (= (plist-get (car (plist-get (plist-get second :content) :results))
                                    :progress-sequence)
                         2)))
            (should (memq (plist-get (e-work-status handle) :state) '(started progress)))))
      (e-work-cancel handle))))
