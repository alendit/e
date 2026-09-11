;;; e-harness-composition-test-support.el --- Harness composition fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared value fixtures for public harness composition suites.  These helpers
;; construct bounded test inputs; semantic assertions remain in each suite.

;;; Code:

(require 'ert)
(require 'seq)
(require 'e)
(require 'e-actions)
(require 'e-backend)
(require 'e-chat-session)
(require 'e-base)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-context)
(require 'e-dev-profile)
(load (expand-file-name "e-tools-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-emacs-tools)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-layers)
(require 'e-operations)
(require 'e-prompts)
(require 'e-request)
(require 'e-resources)
(require 'e-skills)
(require 'e-store)

(defconst e-harness-test--capability-config-options
  (list
   (e-capability-config-option-create
    :key :value
    :default "default"
    :validator #'stringp)
   (e-capability-config-option-create
    :key :items
    :default nil
    :normalizer #'e-capability-config-string-list
    :validator #'e-capability-config-string-list-p))
  "Option specs for harness capability config tests.")

(defmacro e-harness-test--with-empty-layer-registry (&rest body)
  "Run BODY with an isolated layer registry."
  (declare (indent 0) (debug t))
  `(let ((e-layer--registry (make-hash-table :test 'eq)))
     ,@body))

(defun e-harness-test--curation-frame
    (&optional generation-id frame-id value observation-id source-ref
              source-fingerprint)
  "Return one small live frame for harness commit-boundary tests."
  (e-context-lifetime-frame-create
   :id (or frame-id "frame:harness-test")
   :generation-id (or generation-id "generation:harness-test")
   :consumer-request-id "consumer:harness-test"
   :observations
   (list
    (list :observation-id (or observation-id "observation:harness-test")
          :kind "current-state"
          :source-entry-ref (or source-ref "source:harness-test")
          :source-fingerprint
          (or source-fingerprint "fingerprint:harness-test")
          :effective-delivery "inherited"
          :body (list :role 'system
                      :content (or value "HARNESS-EXACT-VALUE"))))))

(defun e-harness-test--tool-result-curation-frame
    (generation-id &optional frame-id call-id value observation-id source-ref
                  source-fingerprint)
  "Return one live tool-result source frame for erasure tests."
  (let ((call-id (or call-id "call:harness-tool-result")))
    (e-context-lifetime-frame-create
     :id (or frame-id "frame:harness-tool-result")
     :generation-id generation-id
     :consumer-request-id "consumer:harness-tool-result"
     :observations
     (list
      (list :observation-id (or observation-id "observation:harness-tool-result")
            :kind "tool-result"
            :source-entry-ref (or source-ref "source:harness-tool-result")
            :source-fingerprint
            (or source-fingerprint "fingerprint:harness-tool-result")
            :effective-delivery "inherited"
            :body (list :role 'tool
                        :content (list :tool-call-id call-id
                                        :name "inspect"
                                        :status 'ok
                                        :content
                                        (or value "HARNESS-TOOL-RESULT"))))))))

(defun e-harness-test--two-tool-result-curation-frame (generation-id)
  "Return a two-source tool-result frame for mixed curation tests."
  (e-context-lifetime-frame-create
   :id "frame:harness-mixed-tool-results"
   :generation-id generation-id
   :consumer-request-id "consumer:harness-mixed-tool-results"
   :observations
   (list
    (list :observation-id "observation:harness-mixed-1"
          :kind "tool-result"
          :source-entry-ref "source:harness-mixed-1"
          :source-fingerprint "fingerprint:harness-mixed-1"
          :effective-delivery "inherited"
          :body (list :role 'tool
                      :content (list :tool-call-id "call:harness-mixed-1"
                                      :name "inspect"
                                      :status 'ok
                                      :content "HARNESS-MIXED-ONE")))
    (list :observation-id "observation:harness-mixed-2"
          :kind "tool-result"
          :source-entry-ref "source:harness-mixed-2"
          :source-fingerprint "fingerprint:harness-mixed-2"
          :effective-delivery "inherited"
          :body (list :role 'tool
                      :content (list :tool-call-id "call:harness-mixed-2"
                                      :name "inspect"
                                      :status 'ok
                                      :content "HARNESS-MIXED-TWO"))))))

(defun e-harness-test--rate-limited-backend (failures)
  "Return a backend that fails with HTTP 429 FAILURES times, then succeeds.
Counts attempts in the returned (BACKEND . COUNTER) cons's cdr."
  (let ((counter (list 0)))
    (cons
     (e-backend-create
      :name "rate-limited"
      :normalize-error-details
      (lambda (_message details _condition)
        (append details '(:retryable t :retry-reason rate-limit)))
      :stream
      (cl-function
       (lambda (&key messages options on-item)
         (ignore messages options)
         (cl-incf (car counter))
         (if (<= (car counter) failures)
             (funcall on-item
                      '(:type backend-error
                        :content "429: Rate limit exceeded for api_key: abc"
                        :payload (:status 429)))
           (funcall on-item '(:type assistant-message :content "recovered"))
           (funcall on-item '(:type done :reason stop))))))
     counter)))

(defun e-harness-test--run-final-refresh-anchor-scenario
    (kind second-candidate-p)
  "Run a real harness refresh scenario for identity KIND.
Return request options, persisted anchors, and the final context."
  (let* ((stable-content "STABLE-A")
         (current-state "STATE-A")
         (tool-version "TOOL-A")
         (request-count 0)
         (requests nil)
         (harness nil)
         (backend
          (e-backend-create
           :name (format "anchor-refresh-%s" kind)
           :context-capabilities
           '(:continuation linear
             :observation-delivery request-local-replaceable)
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages)
              (push (copy-tree options) requests)
              (cl-incf request-count)
              (if (= request-count 1)
                  (progn
                    (funcall
                     on-item
                     '(:type tool-call
                       :id "refresh-1"
                       :name "refresh-anchor"
                       :arguments ()))
                    (funcall
                     on-item
                     '(:type provider-anchor-candidate
                       :provider-id openai
                       :metadata (:response-id "resp-A")))
                    (funcall on-item '(:type done :reason stop)))
                (funcall on-item
                         '(:type assistant-message :content "answer-B"))
                (when second-candidate-p
                  (funcall
                   on-item
                   '(:type provider-anchor-candidate
                     :provider-id openai
                     :metadata (:response-id "resp-B"))))
                (funcall on-item '(:type done :reason stop)))))))
         (stable-provider
          (e-context-provider-create
           :name 'anchor-refresh-stable
           :cache-placement 'stable-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content stable-content)))))
         (dynamic-provider
          (e-context-provider-create
           :name 'anchor-refresh-current
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (capability
          (e-capability-create
           :id 'anchor-refresh-capability
           :instructions "stable policy"
           :context-providers (list stable-provider dynamic-provider)
           :tools
           (list
            (lambda (registry)
              (e-tools-register
               registry
               :name "refresh-anchor"
               :description (format "Refresh %s" tool-version)
               :work
               (e-tools-cheap-work
                "test.anchor-refresh"
                (lambda (_arguments)
                  (pcase kind
                    ('stable
                     (setq stable-content "STABLE-B"))
                    ('current-state
                     (setq current-state "STATE-B"))
                    ('tool-schema
                     (setq tool-version "TOOL-B"))
                    ('provider-option
                     (setf (e-harness-default-options harness)
                           (plist-put
                            (copy-sequence
                             (e-harness-default-options harness))
                            :max-tokens
                            123)))
                    ('compaction
                     (let* ((store (e-harness-sessions harness))
                            (first-entry
                             (car (e-session-local-current-path
                                   store "session-1"))))
                       (e-session-append-compaction
                        store "session-1" "refresh summary"
                        :first-kept-entry-id
                        (plist-get first-entry :id)))))
                  (e-tools-result-create
                   (plist-get (e-tools-current-context) :tool-call)
                   'ok
                   "refreshed"
                   '(:refresh-context t)))))))))
         )
    (setq harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities (list capability)
           :default-options
           '(:model "gpt-test"
             :provider-continuation t
             :provider-anchor-provider-id openai)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "refresh")
    (list :requests (nreverse requests)
          :anchors (e-session-local-provider-anchors
                    (e-harness-sessions harness) "session-1")
          :context (e-harness-turn-context
                    harness "session-1" "after-refresh"))))

(defun e-harness-test--append-compaction-curation
    (store session-id generation-id suffix)
  "Append one valid curation for GENERATION-ID to SESSION-ID.
SUFFIX makes the runtime identities and fact unique to the owning test."
  (let* ((frame-id (format "frame-compaction-%s" suffix))
         (consumer-id (format "consumer-compaction-%s" suffix))
         (response-id (format "response-compaction-%s" suffix))
         (observation-id (format "observation-compaction-%s" suffix))
         (frame
          (e-harness-test--curation-frame
           generation-id frame-id
           (format "RAW-COMPACTION-%s" suffix)
           observation-id
           (format "external:compaction-%s" suffix)
           (format "compaction-fingerprint-%s" suffix)))
         (curation
          (plist-get
           (e-context-lifetime-prepare-curation-disposition
            frame
            (list :keep nil
                  :summaries
                  (list (list :sources '(1)
                              :text (format "selected-%s" suffix))))
            response-id
            1.0)
           :record)))
    (e-session-append-context-curation-package
     store session-id (list :promotion curation :erasure nil))))

(provide 'e-harness-composition-test-support)

;;; e-harness-composition-test-support.el ends here
