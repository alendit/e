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
(require 'e-raw-results-storage-sqlite)
(require 'e-runtime-store)
(require 'e-resources)
(require 'e-session-tmp-resources)
(require 'e-tool-invocation-details)

(defvar e-tool-output-truncation-max-bytes)
(defvar e-tool-output-truncation-max-lines)

(defmacro e-tool-output-truncation-test--with-raw-storage (&rest body)
  "Run BODY with a disposable runtime-level raw-result adapter."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "e-tool-raw-runtime-" t))
          (runtime (e-runtime-store-open directory))
          (e-raw-results-storage
           (e-raw-results-storage-sqlite-create runtime)))
     (unwind-protect (progn ,@body)
       (e-runtime-store-close runtime)
       (delete-directory directory t))))

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

(ert-deftest e-tool-output-truncation-test-reuses-invocation-details-uri ()
  "A session-owned result reuses its complete details artifact for previewing."
  (should (require 'e-tool-output-truncation nil t))
  (let* ((harness (e-tool-output-truncation-test--harness))
         (call '(:id "call-details" :name "echo"
                 :stated-purpose "Keep the complete result available"
                 :arguments (:text "full")))
         (semantic-result '(:tool-call-id "call-details"
                            :name "echo"
                            :status ok
                            :content "abcdefghijklmnopqrstuvwxyz"
                            :metadata (:semantic t)))
         (context (list :harness harness
                        :session-id "session-1"
                        :turn-id "turn-1"
                        :tool-call call
                        :invocation-details-uri nil))
         (result
          (e-tool-invocation-details--post-tool-call
           semantic-result context))
         (details-uri
          (plist-get (plist-get result :metadata)
                     :invocation-details-uri)))
    (unwind-protect
        (let* ((e-tool-output-truncation-max-bytes 10)
               (e-tool-output-truncation-max-lines 2000)
               (truncated
                (e-tool-output-truncation-post-tool-call
                 result
                 context))
               (metadata (plist-get truncated :metadata))
               (reference (plist-get metadata :raw-result-reference))
               (root (e-session-tmp-directory harness "session-1")))
          (should (plist-get metadata :truncated))
          (should (equal (plist-get context :invocation-details-uri)
                         details-uri))
          (should (equal (plist-get metadata :tmp-uri) details-uri))
          (should (equal (plist-get reference :uri) details-uri))
          (should (eq (plist-get reference :storage) 'session-tmp))
          (should (equal (plist-get reference :original-bytes) 26))
          (should (equal (plist-get reference :preview) "abcdefghij"))
          (should (equal (plist-get reference :preview-bytes) 10))
          (should (file-exists-p
                   (expand-file-name
                    (substring details-uri (length "tmp://"))
                    root)))
          (should-not (file-exists-p
                       (expand-file-name
                        "tool-results/turn-1/echo-call-details.txt"
                        root)))
          (let ((archived
                 (e-tool-invocation-details-decode
                  (e-resources-read
                   (e-harness-resources harness "session-1" "turn-1")
                   details-uri nil))))
            (should (equal (plist-get (plist-get archived :result) :content)
                           "abcdefghijklmnopqrstuvwxyz"))))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-output-truncation-test-does-not-trust-forged-details-uri ()
  "Only the canonical details path may replace owned full-result storage."
  (should (require 'e-tool-output-truncation nil t))
  (let* ((harness (e-tool-output-truncation-test--harness))
         (result '(:tool-call-id "call-forged"
                   :name "echo"
                   :status ok
                   :content "abcdefghijklmnopqrstuvwxyz"
                   :metadata
                   (:invocation-details-uri
                    "tmp://tool-invocations/turn-1/call-forged.json"))))
    (unwind-protect
        (let* ((e-tool-output-truncation-max-bytes 10)
               (e-tool-output-truncation-max-lines 2000)
               (truncated
                (e-tool-output-truncation-post-tool-call
                 result
                 (e-tool-output-truncation-test--context harness)))
               (uri (plist-get (plist-get truncated :metadata) :tmp-uri)))
          (should (equal uri
                         "tmp://tool-results/turn-1/echo-call-forged.txt"))
          (should (equal
                   (e-resources-read
                    (e-harness-resources harness "session-1" "turn-1")
                    uri nil)
                   "abcdefghijklmnopqrstuvwxyz")))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-output-truncation-test-without-session-uses-raw-result-store ()
  "Large outputs without an owning session are persisted to raw-result://."
  (should (require 'e-tool-output-truncation nil t))
  (e-tool-output-truncation-test--with-raw-storage
  (let* ((directory (make-temp-file "e-tool-raw-results-test-" t))
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
          ;; The hook is intentionally enqueue-and-return.  Observe it only
          ;; through this explicit test storage boundary; the queued read is
          ;; ordered after the write by the runtime FIFO.
          (should (equal (plist-get
                          (e-raw-results-storage-read
                           e-raw-results-storage uri)
                          :content)
                         "abcdefghijklmnopqrstuvwxyz")))
      (delete-directory directory t)))))

(ert-deftest e-tool-output-truncation-test-file-content-imports-without-session ()
  "A non-session file carrier is copied to raw results without path exposure."
  (should (require 'e-tool-output-truncation nil t))
  (e-tool-output-truncation-test--with-raw-storage
  (let* ((directory (make-temp-file "e-tool-file-raw-results-" t))
         (source (make-temp-file "e-tool-file-content-" nil ".txt"))
         (content "abcdefghijklmnopqrstuvwxyz")
         (carrier
          (e-tools-file-content-create
           :path source :preview "abcdefghij"
           :original-bytes 26 :original-lines 1
           :preview-bytes 10 :preview-lines 1 :owned t))
         (result (list :tool-call-id "call-file"
                       :name "external" :status 'ok
                       :content carrier :metadata nil)))
    (unwind-protect
        (progn
          (write-region content nil source nil 'silent)
          (let* ((e-tool-output-truncation-max-bytes 10)
                 (e-tool-output-truncation-max-lines 2000)
                 (truncated
                  (e-tool-output-truncation-post-tool-call
                   result '(:turn-id "turn-file")))
                 (metadata (plist-get truncated :metadata))
                 (uri (plist-get metadata :tmp-uri)))
            (should (string-prefix-p "raw-result://" uri))
            (should (equal (plist-get
                            (e-raw-results-storage-read
                             e-raw-results-storage uri)
                            :content)
                           content))
            (should-not (file-exists-p source))
            (should-not (string-match-p (regexp-quote source)
                                        (plist-get truncated :content)))))
      (when (file-exists-p source) (delete-file source))
      (delete-directory directory t)))))

(ert-deftest e-tool-output-truncation-test-incomplete-carrier-preview-stays-referenced ()
  "A partial carrier preview stays truncated after presentation limits grow."
  (should (require 'e-tool-output-truncation nil t))
  (e-tool-output-truncation-test--with-raw-storage
  (let* ((directory (make-temp-file "e-tool-file-partial-preview-" t))
         (source (make-temp-file "e-tool-file-content-" nil ".txt"))
         (content "abcdefghijklmnopqrst")
         (carrier
          (e-tools-file-content-create
           :path source :preview "abcdefghij"
           :original-bytes 20 :original-lines 1
           :preview-bytes 10 :preview-lines 1 :owned t))
         (result (list :tool-call-id "call-partial"
                       :name "external" :status 'ok
                       :content carrier :metadata nil)))
    (unwind-protect
        (progn
          (write-region content nil source nil 'silent)
          (let* ((e-tool-output-truncation-max-bytes 100)
                 (e-tool-output-truncation-max-lines 100)
                 (truncated
                  (e-tool-output-truncation-post-tool-call
                   result '(:turn-id "turn-partial")))
                 (metadata (plist-get truncated :metadata))
                 (uri (plist-get metadata :tmp-uri)))
            (should (plist-get metadata :truncated))
            (should (string-prefix-p "raw-result://" uri))
            (should (string-match-p (regexp-quote uri)
                                    (plist-get truncated :content)))
            (should (equal (plist-get
                            (e-raw-results-storage-read
                             e-raw-results-storage uri)
                            :content)
                           content))))
      (when (file-exists-p source) (delete-file source))
      (delete-directory directory t)))))

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
                           (e-hooks-for-point registry :tool-result-presentation))
                   '("50-tool-output-truncation")))))

(provide 'e-tool-output-truncation-test)

;;; e-tool-output-truncation-test.el ends here
