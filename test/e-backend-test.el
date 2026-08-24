;;; e-backend-test.el --- Tests for e backends -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for backend adapter contracts.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-request)

(ert-deftest e-backend-test-fake-streams-items ()
  "Fake backends synchronously stream configured items."
  (let ((backend (e-backend-fake-create
                  :name "fake"
                  :items '((:type assistant-delta :content "hi")
                           (:type done :reason stop))))
        (seen nil))
    (e-backend-stream-batch backend
                      :messages '((:role user :content "hello"))
                      :options '(:model "fake")
                      :on-item (lambda (item) (push item seen)))
    (should (equal (nreverse seen)
                   '((:type assistant-delta :content "hi")
                     (:type done :reason stop))))))

(ert-deftest e-backend-test-rejects-missing-streamer ()
  "Backends need a stream function."
  (let ((backend (e-backend-create :name "bad" :stream nil)))
    (should-error
     (e-backend-stream-batch backend
                       :messages nil
                       :options nil
                       :on-item #'ignore)
     :type 'wrong-type-argument)))

(ert-deftest e-backend-test-fake-exposes-cancellable-request ()
  "Fake backends can expose a cancellable request handle."
  (let ((cancelled nil)
        (request nil))
    (let ((backend (e-backend-fake-create
                    :items '((:type done :reason stop))
                    :cancel-function (lambda () (setq cancelled t)))))
      (e-backend-stream-batch backend
                        :messages nil
                        :options nil
                        :on-item #'ignore
                        :on-request-start (lambda (handle)
                                            (setq request handle)))
      (should (e-backend-request-p request))
      (should (e-backend-cancel-request request))
      (should cancelled))))

(ert-deftest e-backend-test-fake-starts-asynchronously ()
  "Fake backends can deliver stream items through the async start contract."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "ok")
                            (:type done :reason stop))))
         (seen nil)
         (settled nil)
         (request nil))
    (e-backend-start backend
                     :messages '((:role user :content "hello"))
                     :options '(:model "fake")
                     :on-item (lambda (item) (push item seen))
                     :on-done (lambda (result) (setq settled result))
                     :on-error (lambda (err) (setq settled (list :error err)))
                     :on-request-start (lambda (handle)
                                         (setq request handle)))
    (should (e-backend-request-p request))
    (should (null seen))
    (while (not settled)
      (accept-process-output nil 0.01))
    (should (equal (nreverse seen)
                   '((:type assistant-message :content "ok")
                     (:type done :reason stop))))
    (should (equal (plist-get settled :status) 'done))))

(ert-deftest e-backend-test-sync-stream-wrapper-waits-for-async-backend ()
  "The synchronous stream wrapper can consume async-only backend adapters."
  (let ((backend (e-backend-create
                  :name "async-only"
                  :start
                  (cl-function
                   (lambda (&key messages options on-item on-done on-error
                                  on-request-start)
                     (ignore messages options on-error on-request-start)
                     (run-at-time
                      0 nil
                      (lambda ()
                        (funcall on-item
                                 '(:type assistant-message :content "ok"))
                        (funcall on-item '(:type done :reason stop))
                        (funcall on-done '(:status done))))
                     nil))))
        (seen nil))
    (e-backend-stream-batch backend
                      :messages nil
                      :options nil
                      :on-item (lambda (item) (push item seen)))
    (should (equal (nreverse seen)
                   '((:type assistant-message :content "ok")
                     (:type done :reason stop))))))

(ert-deftest e-backend-test-sync-stream-wrapper-rejects-hot-path ()
  "The synchronous stream wrapper cannot run inside marked hot paths."
  (let ((started nil))
    (let ((backend (e-backend-create
                    :name "async-only"
                    :start
                    (cl-function
                     (lambda (&key on-done &allow-other-keys)
                       (setq started t)
                       (funcall on-done '(:status done)))))))
      (let ((err (should-error
                  (e-request-with-hot-path 'backend-stream
                    (e-backend-stream-batch backend
                                      :messages nil
                                      :options nil
                                      :on-item #'ignore))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err) '(e-backend-stream-batch backend-stream))))
      (should-not started))))

(ert-deftest e-backend-test-context-capabilities-default-to-stateless-values ()
  "Backends without a declaration expose conservative semantic defaults."
  (let ((backend (e-backend-create :name "legacy")))
    (should (equal (e-backend-context-capabilities backend '(:model "fake"))
                   '(:continuation none
                     :observation-delivery inherited
                     :prefix-cache none
                     :provider-compaction none
                     :reasoning-state none)))))

(ert-deftest e-backend-test-context-capabilities-are-resolved-from-options ()
  "A backend capability resolver receives backend-neutral effective options."
  (let (seen)
    (let ((backend
           (e-backend-create
            :name "capable"
            :context-capabilities
            (lambda (options)
              (setq seen options)
              (list :continuation 'branchable
                    :observation-delivery 'request-local-replaceable
                    :prefix-cache 'explicit
                    :provider-compaction 'opaque
                    :reasoning-state 'replayable))
            :provider-compaction
            (lambda (&rest _args)
              '(:output [] :usage nil)))))
      (let ((options '(:model "test" :session-id "s1")))
        (should
         (equal (e-backend-context-capabilities backend options)
                '(:continuation branchable
                  :observation-delivery request-local-replaceable
                  :prefix-cache explicit
                  :provider-compaction opaque
                  :reasoning-state replayable)))
        (should (eq seen options))))))

(ert-deftest e-backend-test-opaque-compaction-requires-an-operation ()
  "An opaque capability cannot be advertised without an adapter operation."
  (let ((backend
         (e-backend-create
          :name "missing-compaction"
          :context-capabilities '(:provider-compaction opaque))))
    (should-error
     (e-backend-context-capabilities backend nil)
     :type 'e-backend-invalid-context-capabilities)))

(ert-deftest e-backend-test-provider-compaction-result-is-detached-and-bounded ()
  "The generic result boundary detaches opaque output and validates usage."
  (let* ((output '((:opaque "provider-state"
                    :nested [(:value "nested-provider-state")])))
         (result (e-backend-provider-compaction-result
                  (list :output output
                        :usage '(:input-tokens 3 :total-tokens 4)))))
    (should-not (eq output
                    (e-backend-provider-compaction-result-output result)))
    (should (equal (e-backend-provider-compaction-result-output result)
                   output))
    (setf (plist-get (car output) :opaque) "mutated")
    (setf (plist-get (aref (plist-get (car output) :nested) 0) :value)
          "mutated-nested")
    (should (equal (plist-get
                    (car (e-backend-provider-compaction-result-output result))
                    :opaque)
                   "provider-state"))
    (should (equal
             (plist-get
              (aref
               (plist-get
                (car (e-backend-provider-compaction-result-output result))
                :nested)
               0)
              :value)
             "nested-provider-state"))
    (dolist (bad
             (list '(:output "not-an-array" :usage nil)
                   '(:output [] :usage (:unknown 1))
                   '(:output [] :usage (:input-tokens -1))))
      (should-error
       (e-backend-provider-compaction-result bad)
       :type 'e-backend-invalid-provider-compaction-result))))

(ert-deftest e-backend-test-provider-compaction-batch-rejects-async-handle ()
  "The batch contract never treats an async request handle as a result."
  (let ((backend
         (e-backend-create
          :name "async-compaction"
          :provider-compaction
          (lambda (&rest _args)
            (e-backend-request-create :metadata '(:async t))))))
    (should-error
     (e-backend-provider-compaction-batch backend :messages nil :options nil)
     :type 'e-backend-invalid-provider-compaction-result)))

(ert-deftest e-backend-test-provider-compaction-start-settles-once ()
  "The async contract validates one terminal result and ignores later callbacks."
  (let ((done-count 0)
        (error-count 0)
        (backend
         (e-backend-create
          :name "async-compaction"
          :provider-compaction
          (lambda (&rest args)
            (funcall (plist-get args :on-done)
                     '(:output [] :usage nil))
            (funcall (plist-get args :on-error) '(:late-error t))
            (e-backend-request-create :metadata '(:async t))))))
    (e-backend-provider-compaction-start
     backend :messages nil :options nil
     :on-done (lambda (_result) (setq done-count (1+ done-count)))
     :on-error (lambda (_error) (setq error-count (1+ error-count))))
    (should (= done-count 1))
    (should (= error-count 0))))

(ert-deftest e-backend-test-context-capabilities-nil-resolver-is-stateless ()
  "A resolver that declares no profile gets the conservative defaults."
  (let ((backend (e-backend-create
                  :name "empty-capabilities"
                  :context-capabilities (lambda (_options) nil))))
    (should (equal (e-backend-context-capabilities backend nil)
                   '(:continuation none
                     :observation-delivery inherited
                     :prefix-cache none
                     :provider-compaction none
                     :reasoning-state none)))))

(ert-deftest e-backend-test-context-capabilities-reject-unknown-semantic-values ()
  "Unknown values and misspelled capability keys fail visibly."
  (dolist (declaration
           (list '(:continuation maybe)
                 '(:observation-delivery replaceable)
                 '(:prefix-cache provider)
                 '(:provider-compaction readable)
                 '(:reasoning-state guessed)
                 '(:continuation none :observation-delivary inherited)))
    (let ((backend (e-backend-create
                    :name "invalid"
                    :context-capabilities declaration)))
      (should-error
       (e-backend-context-capabilities backend nil)
       :type 'e-backend-invalid-context-capabilities))))

(ert-deftest e-backend-test-observation-delivery-is-kind-scoped ()
  "A replaceable canvas does not authorize dropping inherited tool results."
  (let* ((declaration
          '(:continuation linear
            :observation-delivery
            ((:kind current-state :mode request-local-replaceable)
             (:kind dynamic-context :mode request-local-replaceable)
             (:kind tool-result :mode inherited))
            :reserved-effect-carrier context-promote-wire))
         (backend (e-backend-create
                   :name "kind-scoped"
                   :context-capabilities declaration))
         (capabilities (e-backend-context-capabilities backend nil)))
    (should (equal
             (e-backend-observation-delivery-for-kind
              capabilities 'current-state)
             'request-local-replaceable))
    (should (equal
             (e-backend-observation-delivery-for-kind
              capabilities 'dynamic-context)
             'request-local-replaceable))
    (should (equal
             (e-backend-observation-delivery-for-kind
              capabilities 'tool-result)
             'inherited))
    (should (equal
             (e-backend-observation-delivery-for-kind
              capabilities 'trace)
             'inherited))
    (should (eq (plist-get capabilities :reserved-effect-carrier)
                'context-promote-wire))))

(ert-deftest e-backend-test-legacy-replaceable-scalar-is-canvas-only ()
  "The legacy scalar capability remains conservative for other kinds."
  (let* ((backend (e-backend-create
                   :name "legacy-replaceable"
                   :context-capabilities
                   '(:observation-delivery request-local-replaceable)))
         (capabilities (e-backend-context-capabilities backend nil)))
    (should (eq (e-backend-observation-delivery-for-kind
                 capabilities 'current-state)
                'request-local-replaceable))
    (should (eq (e-backend-observation-delivery-for-kind
                 capabilities 'tool-result)
                'inherited))))

(provide 'e-backend-test)

;;; e-backend-test.el ends here
