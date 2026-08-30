;;; e-openai-compaction.el --- OpenAI provider compaction -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the proven OpenAI Responses compact endpoint mapping and lifecycle.
;; It consumes portable messages and returns opaque provider output; ordinary
;; request composition remains in e-openai-responses.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'e-backend)
(require 'e-openai-diagnostics)
(require 'e-openai-profile)
(require 'e-openai-responses)
(require 'e-openai-http)

(defun e-openai-compaction-eligible-p
    (provider profile base-url request-function compaction-request-function)
  "Return non-nil when PROFILE proves the public compact endpoint.

The compact operation is deliberately narrower than ordinary Responses
continuation.  It requires the first-party API base URL and Responses wire
format.  A separate injected compaction requester is a test seam for that
known endpoint; an arbitrary ordinary request override is not evidence for
the compact capability."
  (and (eq (e-openai-provider-wire-api profile) 'responses)
       (or (eq provider 'openai)
           (eq (plist-get profile :provider-compaction) 'opaque))
       (e-openai-profile-context-base-url-equal-p
        (or base-url (plist-get profile :base-url))
        e-openai-api-default-base-url)
       (or (null request-function)
           compaction-request-function)))

(defun e-openai-compaction--input (messages)
  "Return Responses input items for portable MESSAGES.

MESSAGES have already crossed the provider-neutral portable projection.  This
adapter mapping adds no replay, anchor, diagnostic, or current-state fields."
  (vconcat (mapcar (lambda (message)
                     (e-openai-responses-input-message message))
                   messages)))

(defun e-openai-compaction--headers (profile auth-file session-id)
  "Return JSON response headers for PROFILE's compact endpoint."
  (let ((headers (e-openai-profile-headers :profile profile
                                    :auth-file auth-file
                                    :session-id session-id)))
    (cons '("Accept" . "application/json")
          (seq-remove (lambda (header)
                        (equal (car header) "Accept"))
                      headers))))

(defun e-openai-compaction--usage (usage)
  "Return bounded generic usage from OpenAI compact USAGE."
  (when (listp usage)
    (let (result)
      (dolist (mapping '((:input_tokens . :input-tokens)
                         (:output_tokens . :output-tokens)
                         (:total_tokens . :total-tokens)))
        (when-let ((value (plist-get usage (car mapping))))
          (unless (and (integerp value) (>= value 0))
            (signal 'e-openai-provider-invalid
                    (list "Invalid compact usage" usage)))
          (setq result (append result (list (cdr mapping) value)))))
      result)))

(defun e-openai-compaction--decode (response)
  "Decode one complete OpenAI compact RESPONSE into the generic result."
  (let* ((body (e-openai-http-response-body-text response))
         ;; `json-parse-string' represents both an empty object and an empty
         ;; plist as nil.  Reject an object-valued output before that loss of
         ;; shape so only the documented array is accepted.
         (object-output-p
          (and (stringp body)
               (string-match-p
                "\\\"output\\\"[[:space:]]*:[[:space:]]*{"
                body)))
         (parsed
          (if (and (listp body)
                   (or (null body) (keywordp (car body))))
            body
            (json-parse-string body
                               :object-type 'plist
                               :array-type 'list
                               :null-object :json-null
                               :false-object :json-false)))
         (object (plist-get parsed :object))
         (output (plist-get parsed :output)))
    (when (e-openai-http-error-p response)
      (signal 'e-openai-provider-invalid
              (list "OpenAI compact request failed" response)))
    (when object-output-p
      (signal 'e-openai-provider-invalid
              (list "OpenAI compact output is an object" parsed)))
    (unless (member object '("response.compaction" response.compaction))
      (signal 'e-openai-provider-invalid
              (list "Unexpected OpenAI compact object" object)))
    (unless (plist-member parsed :output)
      (signal 'e-openai-provider-invalid
              (list "OpenAI compact response has no output field" parsed)))
    (when (eq output :json-null)
      (signal 'e-openai-provider-invalid
              (list "OpenAI compact response has no output" parsed)))
    (unless (or (vectorp output)
                (and (listp output)
                     (or (null output)
                         (not (keywordp (car output))))))
      (signal 'e-openai-provider-invalid
              (list "OpenAI compact output is not an array" output)))
    (list :output output
          :usage (e-openai-compaction--usage
                  (plist-get parsed :usage)))))

(cl-defun e-openai-compaction-run
    (profile auth-file base-url model compaction-request-function
             &key messages options on-done on-error)
  "Run the public OpenAI compact endpoint for portable MESSAGES."
  (let* ((url (concat (string-remove-suffix "/"
                                           (or base-url
                                               (plist-get profile :base-url)))
                      "/responses/compact"))
         (body-data (list :model (or (plist-get options :model)
                                     model
                                     (plist-get profile :default-model)
                                     e-openai-default-model)
                          :input (e-openai-compaction--input messages)))
         (body (json-encode body-data))
         (headers (e-openai-compaction--headers
                   profile auth-file (plist-get options :session-id)))
         (requester (or compaction-request-function
                        #'e-openai-http-request)))
    (if (or on-done on-error)
        (if compaction-request-function
            (let ((cancelled nil)
                  (timer nil)
                  request)
              (setq request
                    (e-backend-request-create
                     :cancel (lambda ()
                               (setq cancelled t)
                               (when (timerp timer)
                                 (cancel-timer timer))
                               t)
                     :metadata (list :transport 'injected-compaction
                                     :url url)))
              (setq timer
                    (run-at-time
                     0 nil
                     (lambda ()
                       (unless cancelled
                         (condition-case err
                             (funcall on-done
                                      (e-openai-compaction--decode
                                       (funcall requester
                                                :url url
                                                :headers headers
                                                :body body)))
                           (error
                            (when on-error
                              (funcall on-error err))))))))
              request)
          (e-openai-http-request-start
           :url url
           :headers headers
           :body body
           :on-complete
           (lambda (response)
             (condition-case err
                 (funcall on-done
                          (e-openai-compaction--decode response))
               (error
                (when on-error (funcall on-error err)))))
           :on-error on-error))
       (e-openai-compaction--decode
       (funcall requester :url url :headers headers :body body)))))



(provide 'e-openai-compaction)

;;; e-openai-compaction.el ends here
