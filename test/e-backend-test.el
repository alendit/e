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
    (e-backend-stream backend
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
     (e-backend-stream backend
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
      (e-backend-stream backend
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
    (e-backend-stream backend
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
                    (e-backend-stream backend
                                      :messages nil
                                      :options nil
                                      :on-item #'ignore))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err) '(e-backend-stream backend-stream))))
      (should-not started))))

(provide 'e-backend-test)

;;; e-backend-test.el ends here
