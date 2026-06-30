;;; e-tool-output-truncation-test.el --- Tests for tool output truncation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the post-tool-call context protection hook.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-hooks)
(require 'e-raw-results)
(require 'e-resources)
(require 'e-session-tmp-resources)

(defvar e-tool-output-truncation-max-bytes)
(defvar e-tool-output-truncation-max-lines)

(declare-function e-tool-output-truncation-capability-create
                  "e-tool-output-truncation")
(declare-function e-tool-output-truncation-post-tool-call
                  "e-tool-output-truncation")

(defun e-tool-output-truncation-test--harness ()
  "Return a harness with tmp resources active."
  (e-harness-create
   :backend (e-backend-fake-create :items nil)
   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))

(defun e-tool-output-truncation-test--context (harness)
  "Return hook context for HARNESS."
  (list :harness harness
        :session-id "session-1"
        :turn-id "turn-1"))

(ert-deftest e-tool-output-truncation-test-small-output-unchanged ()
  "Outputs within byte and line limits are returned unchanged."
  (should (require 'e-tool-output-truncation nil t))
  (let* ((harness (e-tool-output-truncation-test--harness))
         (result '(:tool-call-id "call-1"
                   :name "echo"
                   :status ok
                   :content "small"
                   :metadata (:kept t))))
    (let ((e-tool-output-truncation-max-bytes 50)
          (e-tool-output-truncation-max-lines 10))
      (should (eq (e-tool-output-truncation-post-tool-call
                   result
                   (e-tool-output-truncation-test--context harness))
                  result)))))

(ert-deftest e-tool-output-truncation-test-byte-overflow-is-persisted ()
  "Outputs over the byte limit are previewed and persisted to tmp://."
  (should (require 'e-tool-output-truncation nil t))
  (let* ((harness (e-tool-output-truncation-test--harness))
         (result '(:tool-call-id "call-1"
                   :name "bash"
                   :status ok
                   :content "abcdefghijklmnopqrstuvwxyz"
                   :metadata (:existing yes))))
    (let* ((e-tool-output-truncation-max-bytes 10)
           (e-tool-output-truncation-max-lines 2000)
           (truncated
            (e-tool-output-truncation-post-tool-call
             result
             (e-tool-output-truncation-test--context harness)))
           (metadata (plist-get truncated :metadata))
           (uri (plist-get metadata :tmp-uri))
           (reference (plist-get metadata :raw-result-reference)))
      (should (not (eq truncated result)))
      (should (plist-get metadata :truncated))
      (should (equal (plist-get metadata :existing) 'yes))
      (should (equal uri (plist-get reference :uri)))
      (should (eq (plist-get reference :storage) 'session-tmp))
      (should (equal (plist-get reference :owner)
                     '(:kind tool-result
                       :turn-id "turn-1"
                       :tool-call-id "call-1"
                       :tool-name "bash")))
      (should (equal (plist-get metadata :original-bytes) 26))
      (should (equal (plist-get metadata :shown-bytes) 10))
      (should (equal (plist-get reference :preview) "abcdefghij"))
      (should (string-prefix-p "abcdefghij" (plist-get truncated :content)))
      (should (string-match-p (regexp-quote uri) (plist-get truncated :content)))
      (should (equal (e-resources-read
                      (e-harness-resources harness "session-1" "turn-1")
                      uri
                      nil)
                     "abcdefghijklmnopqrstuvwxyz")))))

(ert-deftest e-tool-output-truncation-test-line-overflow-is-persisted ()
  "Outputs over the line limit are previewed and persisted to tmp://."
  (should (require 'e-tool-output-truncation nil t))
  (let* ((harness (e-tool-output-truncation-test--harness))
         (content "one\ntwo\nthree\nfour\n")
         (result (list :tool-call-id "call-2"
                       :name "read"
                       :status 'ok
                       :content content
                       :metadata nil)))
    (let* ((e-tool-output-truncation-max-bytes 1000)
           (e-tool-output-truncation-max-lines 2)
           (truncated
            (e-tool-output-truncation-post-tool-call
             result
             (e-tool-output-truncation-test--context harness)))
           (metadata (plist-get truncated :metadata)))
      (should (plist-get metadata :truncated))
      (should (equal (plist-get metadata :original-lines) 4))
      (should (equal (plist-get metadata :shown-lines) 2))
      (should (string-prefix-p "one\ntwo\n" (plist-get truncated :content)))
      (should-not (string-prefix-p content (plist-get truncated :content))))))

(ert-deftest e-tool-output-truncation-test-without-session-uses-raw-result-store ()
  "Large outputs without an owning session are persisted to raw-result://."
  (should (require 'e-tool-output-truncation nil t))
  (let* ((directory (make-temp-file "e-tool-raw-results-test-" t))
         (e-raw-results-directory directory)
         (result '(:tool-call-id "call-raw"
                   :name "external"
                   :status ok
                   :content "abcdefghijklmnopqrstuvwxyz"
                   :metadata nil)))
    (unwind-protect
        (let* ((e-tool-output-truncation-max-bytes 10)
               (e-tool-output-truncation-max-lines 2000)
               (truncated
                (e-tool-output-truncation-post-tool-call
                 result
                 '(:turn-id "turn-raw")))
               (metadata (plist-get truncated :metadata))
               (uri (plist-get metadata :tmp-uri))
               (reference (plist-get metadata :raw-result-reference)))
          (should (plist-get metadata :truncated))
          (should (string-prefix-p "raw-result://" uri))
          (should (equal uri (plist-get reference :uri)))
          (should (eq (plist-get reference :storage) 'raw-result-store))
          (should (equal (plist-get reference :cleanup-lifetime)
                         'raw-result-store))
          (should (equal (plist-get reference :owner)
                         '(:kind tool-result
                           :turn-id "turn-raw"
                           :tool-call-id "call-raw"
                           :tool-name "external")))
          (should (string-match-p (regexp-quote uri)
                                  (plist-get truncated :content)))
          (should (equal (e-raw-results-read uri)
                         "abcdefghijklmnopqrstuvwxyz")))
      (delete-directory directory t))))

(ert-deftest e-tool-output-truncation-test-structured-content-uses-shared-text ()
  "Structured content is measured using provider-visible text."
  (should (require 'e-tool-output-truncation nil t))
  (let* ((harness (e-tool-output-truncation-test--harness))
         (result '(:tool-call-id "call-3"
                   :name "structured"
                   :status ok
                   :content (:ok t :items [1 2 3])
                   :metadata nil)))
    (let* ((e-tool-output-truncation-max-bytes 10)
           (e-tool-output-truncation-max-lines 2000)
           (truncated
            (e-tool-output-truncation-post-tool-call
             result
             (e-tool-output-truncation-test--context harness))))
      (should (plist-get (plist-get truncated :metadata) :truncated))
      (should (string-prefix-p "{\"items\"" (plist-get truncated :content))))))

(ert-deftest e-tool-output-truncation-test-already-truncated-result-is-unchanged ()
  "Already truncated results are not truncated a second time."
  (should (require 'e-tool-output-truncation nil t))
  (let* ((harness (e-tool-output-truncation-test--harness))
         (result '(:tool-call-id "call-4"
                   :name "bash"
                   :status ok
                   :content "preview"
                   :metadata (:truncated t :tmp-uri "tmp://existing.txt"))))
    (let ((e-tool-output-truncation-max-bytes 1)
          (e-tool-output-truncation-max-lines 1))
      (should (eq (e-tool-output-truncation-post-tool-call
                   result
                   (e-tool-output-truncation-test--context harness))
                  result)))))

(ert-deftest e-tool-output-truncation-test-capability-contributes-post-hook ()
  "The truncation capability contributes the post-tool-call hook."
  (should (require 'e-tool-output-truncation nil t))
  (let ((registry (e-hooks-registry-create)))
    (e-capabilities-register-hooks
     (e-tool-output-truncation-capability-create)
     registry)
    (should (equal (mapcar #'e-hook-id
                           (e-hooks-for-point registry :post-tool-call))
                   '("50-tool-output-truncation")))))

(provide 'e-tool-output-truncation-test)

;;; e-tool-output-truncation-test.el ends here
