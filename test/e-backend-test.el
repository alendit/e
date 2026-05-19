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

(provide 'e-backend-test)

;;; e-backend-test.el ends here
