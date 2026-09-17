;;; e-tool-invocation-details-test.el --- Tests for tool detail archives -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owner tests for the harness-owned, temporary invocation-details boundary.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-json)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-harness-base)
(require 'e-loop)
(require 'e-session-tmp-resources)
(require 'e-tool-invocation-details)
(require 'e-tools)
(load (expand-file-name "e-tools-test-support.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil nil t)
(load (expand-file-name "e-harness-test-support.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil nil t)

(defun e-tool-invocation-details-test--call (&optional arguments)
  "Return a valid ordinary model-facing CALL with ARGUMENTS."
  (list :id "call/one" :name "echo"
        :arguments (or arguments '(:text "hello"))))

(defun e-tool-invocation-details-test--result
    (&optional content metadata status)
  "Return a structured RESULT for the test call."
  (list :tool-call-id "call/one"
        :name "echo"
        :status (or status 'ok)
        :content (or content "done")
        :metadata metadata))

(defun e-tool-invocation-details-test--read-uri (harness session-id uri)
  "Read URI from HARNESS SESSION-ID without using the model resource API."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      (substring uri (length "tmp://"))
      (e-session-tmp-directory harness session-id)))
    (buffer-string)))

(defun e-tool-invocation-details-test--tools-capability ()
  "Return a capability registering the ordinary echo test tool."
  (e-capability-create
   :id 'invocation-details-test-tools
   :tools
   (list
    (lambda (registry)
      (e-tools-test-register
       registry
       :name "echo"
       :description "Echo text."
       :parameters '(:type "object"
                     :properties (:text (:type "string"))
                     :required ["text"]
                     :additionalProperties :json-false)
       :handler (lambda (arguments)
                  (plist-get arguments :text)))))))

(cl-defun e-tool-invocation-details-test--run-two-round-tool
    (arguments &key handler)
  "Run one provider tool turn followed by a settled assistant response.
Return the harness, request count, and final turn result.  HANDLER is the
ordinary tool implementation used by the test capability."
  (let* ((request-count 0)
         (backend
          (e-backend-create
           :name "invocation-details-test-two-round"
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (cl-incf request-count)
              (if (= request-count 1)
                  (progn
                    (funcall
                     on-item
                     (list :type 'tool-call
                           :id "scripted-call"
                           :name "echo"
                           :arguments arguments))
                    (funcall on-item '(:type done :reason tool-use)))
                (funcall on-item
                         '(:type assistant-message :content "settled"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools-capability
          (e-capability-create
           :id 'invocation-details-test-two-round-tools
           :tools
           (list
            (lambda (registry)
              (e-tools-test-register
               registry
               :name "echo"
               :description "Echo text."
               :parameters '(:type "object"
                             :properties (:text (:type "string"))
                             :required ["text"]
                             :additionalProperties :json-false)
               :handler (or handler
                            (lambda (tool-arguments)
                              (plist-get tool-arguments :text))))))))
         (base (e-harness-base-layer-create))
         (harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities
           (append (e-layer-capabilities base)
                   (list tools-capability)))))
    (e-harness-create-session harness :id "session-1")
    (let ((result (e-harness-test-prompt-batch
                   harness "session-1" "invoke the test tool")))
      (list :harness harness :request-count request-count :result result))))

(defun e-tool-invocation-details-test--tool-result (harness &optional session-id)
  "Return the first projected tool result in HARNESS SESSION-ID."
  (plist-get
   (cl-find 'tool (e-harness-messages harness (or session-id "session-1"))
            :key (lambda (message) (plist-get message :role)))
   :content))

(defun e-tool-invocation-details-test--tool-call (harness)
  "Return the first projected tool call in HARNESS SESSION-1."
  (plist-get
   (cl-find 'tool-call (e-harness-messages harness "session-1")
            :key (lambda (message) (plist-get message :role)))
   :content))

(ert-deftest e-tool-invocation-details-test-success-round-trip-is-canonical ()
  "A successful invocation encodes and decodes its exact semantic shape."
  (let* ((document
          (e-tool-invocation-details--document
           (e-tool-invocation-details-test--call)
           (e-tool-invocation-details-test--result
            '(:items [1 2] :text "done")
            '(:semantic t :note "portable"))))
         (encoded (e-tool-invocation-details-encode document))
         (decoded (e-tool-invocation-details-decode encoded)))
    (should (string-match-p "\\\"version\\\":1" encoded))
    (should (string-match-p "\\\"arguments\\\"" encoded))
    (should-not (string-match-p "received_arguments" encoded))
    (should (equal (plist-get decoded :version) 1))
    (should (equal (plist-get (plist-get decoded :result) :status) "ok"))
    (should (equal (plist-get (plist-get decoded :result) :metadata)
                   '(:note "portable" :semantic t)))))

(ert-deftest e-tool-invocation-details-test-preserves-canonical-empty-and-sentinels ()
  "Temporary invocation JSON preserves empty objects, arrays, false, and null."
  (let* ((empty-object (make-hash-table :test 'equal))
         (arguments (list :empty-object empty-object
                          :empty-array []
                          :false e-json-false
                          :null e-json-null
                          :objects (vector (list :value e-json-null))))
         (document
          (e-tool-invocation-details--document
           (e-tool-invocation-details-test--call arguments)
           (e-tool-invocation-details-test--result)))
         (encoded (e-tool-invocation-details-encode document))
         (decoded (e-tool-invocation-details-decode encoded))
         (round-trip (plist-get decoded :arguments)))
    (should (e-json-value-p (e-json-parse-string encoded)))
    (should (null (plist-get round-trip :empty-object)))
    (should (equal (plist-get round-trip :empty-array) []))
    (should (eq (plist-get round-trip :false) e-json-false))
    (should (eq (plist-get round-trip :null) e-json-null))
    (should (vectorp (plist-get round-trip :objects)))
    (should (eq (plist-get (aref (plist-get round-trip :objects) 0) :value)
                e-json-null))))

(ert-deftest e-tool-invocation-details-test-rejected-call-keeps-received-only ()
  "Rejected raw arguments are archived separately from executed arguments."
  (let* ((call (list :id "rejected" :name "echo"))
         (received '(:text "received"))
         (document
          (e-tool-invocation-details--document
           call
           (e-tool-invocation-details-test--result
            "rejected" '(:error e-tools-invalid-arguments) 'error)
           t
           received))
         (encoded (e-tool-invocation-details-encode document))
         (decoded (e-tool-invocation-details-decode encoded)))
    (should (string-match-p "received_arguments" encoded))
    (should-not (string-match-p "\\\"arguments\\\"" encoded))
    (should (equal (plist-get decoded :received-arguments) received))
    (should-not (plist-member decoded :arguments))))

(ert-deftest e-tool-invocation-details-test-rejects-invalid-shapes ()
  "The version-one document has no missing, extra, or ambiguous fields."
  (dolist (document
           (list
            '(:version 1 :tool-call-id "c" :tool "echo"
              :result (:status ok :content "x" :metadata nil))
            '(:version 1 :tool-call-id "c" :tool "echo" :arguments nil :received-arguments nil
              :result (:status ok :content "x" :metadata nil))
            '(:version 1 :tool-call-id "c" :tool "echo" :arguments nil :result
              (:status ok :content "x" :metadata nil) :unexpected t)))
    (should-error (e-tool-invocation-details-encode document)
                  :type 'e-tool-invocation-details-invalid)))

(ert-deftest e-tool-invocation-details-test-drops-only-nonportable-metadata ()
  "Metadata may omit live values, while core arguments/content fail closed."
  (let* ((cycle (list :cycle nil))
         (_ (setcar (cdr cycle) cycle))
         (encoded
          (e-tool-invocation-details-encode
           (e-tool-invocation-details--document
            (e-tool-invocation-details-test--call)
            (e-tool-invocation-details-test--result
             "portable"
             (list :keep "yes"
                   :nested (vector "yes" cycle)
                   :buffer (current-buffer)
                   :cycle cycle))))))
    (should (string-match-p "\\\"keep\\\":\\\"yes\\\"" encoded))
    (should (string-match-p "\\\"nested\\\":\\\[\\\"yes\\\"\\\]" encoded))
    (should-not (string-match-p "buffer" encoded)))
  (should-error
   (e-tool-invocation-details-encode
    (e-tool-invocation-details--document
     (e-tool-invocation-details-test--call)
     (e-tool-invocation-details-test--result (current-buffer))))
   :type 'e-tool-invocation-details-nonportable)
  (should-error
   (e-tool-invocation-details-encode
    (e-tool-invocation-details--document
     (e-tool-invocation-details-test--call (list :value (current-buffer)))
     (e-tool-invocation-details-test--result)))
   :type 'e-tool-invocation-details-nonportable))

(ert-deftest e-tool-invocation-details-test-safe-path-fragments-do-not-alias ()
  "Unsafe and long identifiers receive stable collision-resistant suffixes."
  (let ((first (e-tool-invocation-details-relative-name "a/b" "call"))
        (second (e-tool-invocation-details-relative-name "a?b" "call"))
        (dot (e-tool-invocation-details-relative-name "." ".."))
        (upper (e-tool-invocation-details-relative-name "turn" "Call"))
        (lower (e-tool-invocation-details-relative-name "turn" "call"))
        (long (e-tool-invocation-details-relative-name
               (make-string 200 ?x) "call")))
    (should-not (equal first second))
    (should-not (equal upper lower))
    (should-not (string-match-p "/\.\.?/\|/\.\.?\.json\'" dot))
    (should-not (string-match-p "[/\\]" (file-name-nondirectory first)))
    (should-not (string-match-p "[/\\]" (file-name-nondirectory second)))
    (should (string-match-p "\\.json\\'" long))
    (should (< (length long) 180))))

(ert-deftest e-tool-invocation-details-test-reserved-and-case-paths-round-trip ()
  "Reserved components and case aliases write distinct readable artifacts."
  (let* ((session-id "invocation-details-path-aliases")
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         uris)
    (unwind-protect
        (progn
          (dolist (spec '((".." ".") ("turn" "Call") ("turn" "call")))
            (let* ((turn-id (car spec))
                   (call-id (cadr spec))
                   (call (e-tool-invocation-details-test--call)))
              (setq call (plist-put call :id call-id))
              (push (cons call-id
                          (e-tool-invocation-details-write
                           harness session-id turn-id call
                           (e-tool-invocation-details-test--result)))
                    uris)))
          (should (= (length (delete-dups (mapcar #'cdr uris))) 3))
          (dolist (entry uris)
            (should
             (equal
              (plist-get
               (e-tool-invocation-details-decode
                (e-tool-invocation-details-test--read-uri
                 harness session-id (cdr entry)))
               :tool-call-id)
              (car entry)))))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-invocation-details-test-write-readback-and-cleanup ()
  "The writer owns one safe tmp URI and cleanup removes its artifact."
  (let* ((session-id "invocation-details-write-readback")
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (uri (e-tool-invocation-details-write
               harness session-id "turn/one"
               (e-tool-invocation-details-test--call)
               (e-tool-invocation-details-test--result)))
         (root (e-session-tmp-directory harness session-id))
         (path (expand-file-name (substring uri (length "tmp://")) root)))
    (unwind-protect
        (progn
          (should (string-prefix-p "tmp://tool-invocations/" uri))
          (should (file-exists-p path))
          (should (equal
                   (plist-get
                    (e-tool-invocation-details-decode
                     (e-tool-invocation-details-test--read-uri
                      harness session-id uri))
                    :tool-call-id)
                   "call/one"))
          (should (file-directory-p root)))
      (e-session-tmp-cleanup-harness harness))
    (should-not (file-exists-p root))))

(ert-deftest e-tool-invocation-details-test-hook-archives-before-presentation ()
  "The lifecycle hook adds a URI while preserving the semantic result."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (registry (e-tools-registry-create))
         (result (e-tool-invocation-details-test--result
                  (make-string 80 ?x)
                  '(:semantic t)))
         (context (list :harness harness
                        :session-id "session-1"
                        :turn-id "turn-1"
                        :tools registry))
         hooked)
    (e-tools-test-register
     registry :name "echo" :description "Echo."
     :parameters '(:type "object" :properties (:text (:type "string")))
     :handler (lambda (_arguments) "ok"))
    (setq hooked
          (e-tool-invocation-details--post-tool-call
           result
           (plist-put context :tool-call
                      (e-tool-invocation-details-test--call))))
    (unwind-protect
        (let* ((uri (plist-get (plist-get hooked :metadata)
                               :invocation-details-uri))
               (archived
                (e-tool-invocation-details-decode
                 (e-tool-invocation-details-test--read-uri
                  harness "session-1" uri))))
          (should (stringp uri))
          (should (equal (plist-get hooked :content)
                         (plist-get result :content)))
          (should (equal (plist-get (plist-get archived :result) :content)
                         (make-string 80 ?x)))
          (should (equal (plist-get (plist-get archived :result) :metadata)
                         '(:semantic t)))
          (should (eq
                   (e-tool-invocation-details--post-tool-call
                    result (plist-put (copy-sequence context)
                                      :nested t))
                   result)))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-invocation-details-test-streams-and-cleans-file-content ()
  "An owned file carrier is archived completely and consumed after success."
  (let* ((session-id "invocation-details-file-content")
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (source (make-temp-file "e-invocation-details-file-" nil ".txt"))
         (complete "one\ntwo\nthree\n\"quoted\"\\slash\tend")
         (carrier
          (e-tools-file-content-create
           :path source :preview "one\ntwo\n"
           :original-bytes (string-bytes complete)
           :original-lines 4 :preview-bytes 8 :preview-lines 2 :owned t))
         (call (e-tool-invocation-details-test--call))
         (result (e-tool-invocation-details-test--result
                  carrier '(:semantic t) 'ok))
         (context (list :harness harness
                        :session-id session-id
                        :turn-id "turn-1"
                        :tool-call call
                        :invocation-details-uri nil))
         archived)
    (unwind-protect
        (progn
          (write-region complete nil source nil 'silent)
          (setq archived
                (e-tool-invocation-details--post-tool-call result context))
          (should-not (file-exists-p source))
          (should (eq (plist-get archived :content) carrier))
          (should-not (e-tools-file-content-owned carrier))
          (let* ((uri (plist-get (plist-get archived :metadata)
                                 :invocation-details-uri))
                 (details
                  (e-tool-invocation-details-decode
                   (e-tool-invocation-details-test--read-uri
                    harness session-id uri))))
            (should (equal
                     (plist-get (plist-get details :result) :content)
                     complete))))
      (when (file-exists-p source)
        (delete-file source))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-invocation-details-test-streams-unicode-across-chunk-boundary ()
  "A split UTF-8 sequence remains readable semantic content."
  (let* ((session-id "invocation-details-unicode-boundary")
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (source (make-temp-file "e-invocation-details-unicode-" nil ".txt"))
         ;; 65,535 ASCII bytes put the first byte of EURO SIGN at the end of
         ;; the validator's 65,536-byte source chunk.
         (complete (concat (make-string 65535 ?x) "€end"))
         (carrier
          (e-tools-file-content-create
           :path source :preview "xxxxxxxx"
           :original-bytes (string-bytes complete)
           :original-lines 1 :preview-bytes 8 :preview-lines 1 :owned t))
         (call (e-tool-invocation-details-test--call))
         (result (e-tool-invocation-details-test--result
                  carrier nil 'ok))
         (context (list :harness harness :session-id session-id
                        :turn-id "turn-1" :tool-call call))
         archived)
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region complete nil source nil 'silent))
          (setq archived
                (e-tool-invocation-details--post-tool-call result context))
          (let* ((uri (plist-get (plist-get archived :metadata)
                                 :invocation-details-uri))
                 (details
                  (e-tool-invocation-details-decode
                   (e-tool-invocation-details-test--read-uri
                    harness session-id uri))))
            (should (equal
                     (plist-get (plist-get details :result) :content)
                     complete))))
      (when (file-exists-p source) (delete-file source))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-invocation-details-test-structured-encoding-shape-is-semantic ()
  "A legitimate data/encoding content object is never treated as codec data."
  (let* ((content '(:data "literal" :encoding "base64-utf8-bytes"))
         (document
          (e-tool-invocation-details--document
           (e-tool-invocation-details-test--call)
           (e-tool-invocation-details-test--result content nil 'ok)))
         (decoded
          (e-tool-invocation-details-decode
           (e-tool-invocation-details-encode document))))
    (should (equal (plist-get (plist-get decoded :result) :content)
                   '(:data "literal" :encoding "base64-utf8-bytes")))))

(ert-deftest e-tool-invocation-details-test-lifecycle-archives-once-before-truncation ()
  "Harness lifecycle archives the full semantic result before its preview hook."
  (let* ((base (e-harness-base-layer-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (append (e-layer-capabilities base)
                           (list (e-tool-invocation-details-test--tools-capability)))))
         result
         failure)
    (e-harness-create-session harness :id "session-1")
    (let ((e-tool-output-truncation-max-bytes 8)
          (e-tool-output-truncation-max-lines 1000))
      (e-tool-lifecycle-start-call
       (e-harness-tool-lifecycle harness "session-1" "turn-1")
       '(:id "call-1" :name "echo"
         :arguments (:text "0123456789abcdefghijklmnopqrstuvwxyz"))
       :on-done (lambda (value) (setq result value))
       :on-error (lambda (err) (setq failure err)))
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (not (or result failure))
                    (< (float-time) deadline))
          (accept-process-output nil 0.01))))
    (unwind-protect
        (progn
          (should (or result failure))
          (when failure (signal (car failure) (cdr failure)))
          (let* ((metadata (plist-get result :metadata))
                 (uri (plist-get metadata :invocation-details-uri))
                 (archived
                  (e-tool-invocation-details-decode
                   (e-tool-invocation-details-test--read-uri
                    harness "session-1" uri))))
            (should (plist-get metadata :truncated))
            (should (stringp uri))
            (should (> (length (plist-get (plist-get archived :result)
                                         :content))
                       8))))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-invocation-details-test-loop-structured-semantic-error-archives-executed-arguments ()
  "An executed structured tool error archives arguments and semantic details."
  (let* ((run
          (e-tool-invocation-details-test--run-two-round-tool
           '(
             :text "bad-input")
           :handler
           (lambda (_arguments)
             (e-tools-result-create
              '(:id "scripted-call" :name "echo")
              'error
              '(:code "semantic-error" :message "not acceptable")
              '(:category validation :portable t)))))
         (harness (plist-get run :harness)))
    (unwind-protect
        (let* ((result (e-tool-invocation-details-test--tool-result harness))
               (metadata (plist-get result :metadata))
               (uri (plist-get metadata :invocation-details-uri)))
          (should (= (plist-get run :request-count) 2))
          (should (eq (plist-get result :status) 'error))
          (should (stringp uri))
          (let* ((archived
                  (e-tool-invocation-details-decode
                   (e-tool-invocation-details-test--read-uri
                    harness "session-1" uri)))
                 (archived-result (plist-get archived :result))
                 (archived-content (plist-get archived-result :content))
                 (archived-metadata (plist-get archived-result :metadata)))
            (should (equal (plist-get archived :arguments)
                           '(:text "bad-input")))
            (should-not (plist-member archived :received-arguments))
            (should (equal (plist-get archived-result :status) "error"))
            (should (equal (plist-get archived-content :code)
                           "semantic-error"))
            (should (equal (plist-get archived-content :message)
                           "not acceptable"))
            (should (equal (plist-get archived-metadata :category)
                           "validation"))
            (should (eq (plist-get archived-metadata :portable) t))))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-invocation-details-test-lifecycle-deadline-archives-timeout-result ()
  "A deadline timeout archives executed arguments and its final error result."
  (let* ((cancelled nil)
         (result nil)
         (failure nil)
         (base (e-harness-base-layer-create))
         (stall-capability
          (e-capability-create
           :id 'invocation-details-test-stall-tool
           :tools
           (list
            (lambda (registry)
              (e-tools-test-register
               registry
               :name "stall"
               :description "Wait until the deadline."
               :parameters '(:type "object"
                             :properties (:text (:type "string"))
                             :required ["text"]
                             :additionalProperties :json-false)
               :start
               (cl-function
                (lambda (&key on-request-start &allow-other-keys)
                  (funcall
                   on-request-start
                   (e-tools-request-create
                    :cancel (lambda ()
                              (setq cancelled t)
                              t)))
                  nil)))))))
         (harness
          (e-harness-create
           :backend (e-backend-fake-create :items nil)
           :intrinsic-capabilities
           (append (e-layer-capabilities base)
                   (list stall-capability)))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (let ((e-harness-provider-request-deadline-seconds 0.03))
            (e-tool-lifecycle-start-call
             (e-harness-tool-lifecycle harness "session-1" "turn-1")
             '(:id "timeout-call" :name "stall"
               :arguments (:text "timeout"))
             :on-done (lambda (value) (setq result value))
             :on-error (lambda (err) (setq failure err))))
          (let ((deadline (+ (float-time) 1.0)))
            (while (and (not (or result failure))
                        (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should (or result failure))
          (when failure
            (signal (car failure) (cdr failure)))
          (should cancelled)
          (let* ((metadata (plist-get result :metadata))
                 (uri (plist-get metadata :invocation-details-uri)))
            (should (eq (plist-get result :status) 'error))
            (should (eq (plist-get metadata :error)
                        'e-work-deadline-exceeded))
            (should (stringp uri))
            (let* ((archived
                    (e-tool-invocation-details-decode
                     (e-tool-invocation-details-test--read-uri
                      harness "session-1" uri)))
                   (archived-result (plist-get archived :result)))
              (should (equal (plist-get archived :arguments)
                             '(:text "timeout")))
              (should-not (plist-member archived :received-arguments))
              (should (equal (plist-get archived-result :status) "error"))
              (should (equal (plist-get (plist-get archived-result :metadata)
                                       :error)
                             "e-work-deadline-exceeded"))
              (should (string-match-p
                       "deadline"
                       (plist-get archived-result :content))))))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-invocation-details-test-abort-archives-cancellation-once ()
  "Aborting an active top-level tool archives its final cancellation once."
  (let* ((session-id "invocation-details-abort")
         (tool-callbacks nil)
         (tool-cancelled nil)
         (backend
          (e-backend-create
           :name "invocation-details-abort"
           :start
           (cl-function
            (lambda (&key on-item on-done &allow-other-keys)
              (funcall
               on-item
               '(:type tool-call
                 :id "abort-call"
                 :name "held-tool"
                 :arguments (
                             :text "cancel-me")))
              (funcall on-item '(:type done :reason tool-use))
              (funcall on-done '(:status done))
              nil))))
         (tools-capability
          (e-capability-create
           :id 'invocation-details-abort-tools
           :tools
           (list
            (lambda (registry)
              (e-tools-test-register
               registry
               :name "held-tool"
               :description "Hold."
               :parameters '(:type "object"
                             :properties (:text (:type "string"))
                             :required ["text"]
                             :additionalProperties :json-false)
               :start
               (cl-function
                (lambda (&key on-request-start on-done &allow-other-keys)
                  (setq tool-callbacks (list :on-done on-done))
                  (funcall
                   on-request-start
                   (e-tools-request-create
                    :cancel (lambda ()
                              (setq tool-cancelled t)
                              t)))
                  nil)))))))
         (base (e-harness-base-layer-create))
         (harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities
           (append (e-layer-capabilities base)
                   (list tools-capability))))
         (events nil))
    (unwind-protect
        (progn
          (e-harness-activity-subscribe
           harness (lambda (event) (push event events)))
          (e-harness-create-session harness :id session-id)
          (e-harness-test-prompt-async harness session-id "cancel this")
          (should tool-callbacks)
          (e-harness-test-abort harness session-id)
          ;; A late callback from the cancelled operation must not overwrite
          ;; the cancellation result or create a second archive.
          (funcall (plist-get tool-callbacks :on-done) "late result")
          (should (eq (plist-get (e-harness-wait-batch
                                  harness session-id 0.1)
                                 :status)
                      'cancelled))
          (should tool-cancelled)
          (let* ((messages (e-harness-messages harness session-id))
                 (tool-result (plist-get (nth 2 messages) :content))
                 (metadata (plist-get tool-result :metadata))
                 (uri (plist-get metadata :invocation-details-uri))
                 (archive
                  (progn
                    (should (stringp uri))
                    (e-tool-invocation-details-decode
                     (e-tool-invocation-details-test--read-uri
                      harness session-id uri))))
                 (json-files
                  (directory-files-recursively
                   (e-session-tmp-directory harness session-id)
                   "\\.json\\'")))
            (should (equal (mapcar (lambda (message) (plist-get message :role))
                                   messages)
                           '(user tool-call tool)))
            (should (eq (plist-get tool-result :status) 'error))
            (should (equal (plist-get tool-result :content) "Cancelled"))
            (should (equal (plist-get archive :tool-call-id) "abort-call"))
            (should (equal (plist-get archive :arguments)
                           '(:text "cancel-me")))
            (should (equal (plist-get (plist-get archive :result) :content)
                           "Cancelled"))
            (should (= (length json-files) 1)))
          (should (= (length
                      (cl-remove-if-not
                       (lambda (event)
                         (eq (plist-get event :type) 'tool-finished))
                       events))
                     1)))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-invocation-details-test-nested-host-call-is-not-archived ()
  "A host-authored nested call has no detail artifact of its own."
  (let* ((session-id "invocation-details-nested-host")
         (request-count 0)
         (nested-result nil)
         (backend
          (e-backend-create
           :name "invocation-details-nested"
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (cl-incf request-count)
              (if (= request-count 1)
                  (progn
                    (funcall
                     on-item
                     '(:type tool-call
                       :id "outer-call"
                       :name "outer"
                       :arguments (
                                   :text "outer-input")))
                    (funcall on-item '(:type done :reason tool-use)))
               (funcall on-item
                         '(:type assistant-message :content "settled"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools-capability
          (e-capability-create
           :id 'invocation-details-nested-tools
           :tools
           (list
            (lambda (registry)
              (e-tools-test-register
               registry
               :name "outer"
               :description "Run the outer operation."
               :parameters '(:type "object"
                             :properties (:text (:type "string"))
                             :required ["text"]
                             :additionalProperties :json-false)
               :handler (lambda (_arguments)
                          (setq nested-result
                                (e-tools-call "inner" '(:text "nested")))
                          "outer-done")
               :metadata nil)
              (e-tools-test-register
               registry
               :name "inner"
               :description "Run the nested operation."
               :parameters '(:type "object"
                             :properties (:text (:type "string"))
                             :required ["text"]
                             :additionalProperties :json-false)
               :handler (lambda (arguments)
                          (plist-get arguments :text)))))))
         (base (e-harness-base-layer-create))
         (harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities
           (append (e-layer-capabilities base)
                   (list tools-capability)))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id session-id)
          (e-harness-test-prompt-batch harness session-id "run outer")
          (should (= request-count 2))
          (should (eq (plist-get nested-result :status) 'ok))
          (should-not (plist-member (plist-get nested-result :metadata)
                                    :invocation-details-uri))
          (let* ((tool-result
                  (e-tool-invocation-details-test--tool-result
                   harness session-id))
                 (uri (plist-get (plist-get tool-result :metadata)
                                 :invocation-details-uri))
                 (archive
                  (progn
                    (should (stringp uri))
                    (e-tool-invocation-details-decode
                     (e-tool-invocation-details-test--read-uri
                      harness session-id uri))))
                 (json-files
                  (directory-files-recursively
                   (e-session-tmp-directory harness session-id)
                   "\\.json\\'")))
            (should (equal (plist-get archive :tool-call-id) "outer-call"))
            (should (equal (plist-get (plist-get archive :result) :content)
                           "outer-done"))
            (should (= (length json-files) 1))))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-tool-invocation-details-test-loop-rejected-operation-archives-detached-arguments ()
  "A rejected provider operation is bounded and archived without dispatch."
  (let* ((request-count 0)
         (handler-called nil)
         (backend
          (e-backend-create
           :name "invalid-operation-invocation-details"
           :stream
           (cl-function
            (lambda (&key messages options on-item &allow-other-keys)
              (ignore options)
              (cl-incf request-count)
              (if (= request-count 1)
                  (progn
                    (should (member 'user
                                    (mapcar (lambda (message)
                                              (plist-get message :role))
                                            messages)))
                    (funcall
                     on-item
                     '(:type tool-call
                       :id "invalid-operation-call"
                       :name "echo"
                       :arguments (
                                   :text "visible"
                                   :secret "RECEIVED-ONLY")))
                    (funcall on-item '(:type done :reason tool-use)))
                (should (equal (last (mapcar (lambda (message)
                                               (plist-get message :role))
                                             messages)
                                     3)
                               '(tool-call system tool))))
                (funcall on-item '(:type assistant-message :content "settled"))
                (funcall on-item '(:type done :reason stop))))))
         (tools-capability
          (e-capability-create
           :id 'invalid-operation-details-tools
           :tools
           (list
            (lambda (registry)
              (e-tools-test-register
               registry
               :name "echo"
               :description "Echo text."
               :parameters '(:type "object"
                             :properties (:text (:type "string"))
                             :required ["text"]
                             :additionalProperties :json-false)
               :handler (lambda (_arguments)
                          (setq handler-called t)
                          "must-not-run"))))))
         (base (e-harness-base-layer-create))
         (harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities
           (append (e-layer-capabilities base)
                   (list tools-capability)))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-harness-test-prompt-batch harness "session-1" "trigger rejection")
          (should (= request-count 2))
          (should-not handler-called)
          (let* ((messages (e-harness-messages harness "session-1"))
                 (tool-call-message
                  (cl-find 'tool-call messages
                           :key (lambda (message)
                                  (plist-get message :role))))
                 (tool-result-message
                  (cl-find 'tool messages
                           :key (lambda (message)
                                  (plist-get message :role))))
                 (call (plist-get tool-call-message :content))
                 (result (plist-get tool-result-message :content))
                 (uri (plist-get (plist-get result :metadata)
                                 :invocation-details-uri))
                 (archived
                  (e-tool-invocation-details-decode
                   (e-tool-invocation-details-test--read-uri
                    harness "session-1" uri)))
                 (activity
                  (e-harness-session-activity-events harness "session-1")))
            (should (equal (plist-get (plist-get call :arguments) :text)
                           "visible"))
            (should-not (plist-member (plist-get call :arguments) :secret))
            (should (eq (plist-get result :status) 'error))
            (should (eq (plist-get (plist-get result :metadata) :error)
                        'e-tools-invalid-arguments))
            (should (stringp uri))
            (should (equal
                     (plist-get (plist-get archived :received-arguments) :text)
                     "visible"))
            (should (equal
                     (plist-get (plist-get archived :received-arguments) :secret)
                     "RECEIVED-ONLY"))
            (should-not (plist-member archived :arguments))
            (should-not (string-match-p
                         (regexp-quote "RECEIVED-ONLY")
                         (prin1-to-string messages)))
            (should-not (string-match-p
                         (regexp-quote "RECEIVED-ONLY")
                         (prin1-to-string activity)))))
      (e-session-tmp-cleanup-harness harness))))

(provide 'e-tool-invocation-details-test)

;;; e-tool-invocation-details-test.el ends here
