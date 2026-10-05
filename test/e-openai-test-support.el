;;; e-openai-test-support.el --- Shared OpenAI test fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Small value fixtures shared by composed and owner-mechanism OpenAI tests.

;;; Code:

(require 'cl-lib)
(require 'e-json)
(require 'e-backend)

(defvar url-current-object)
(defvar url-extensions-header)
(defvar url-http-attempt-keepalives)
(defvar url-http-data)
(defvar url-http-extra-headers)
(defvar url-http-method)
(defvar url-http-proxy)
(defvar url-http-real-basic-auth-storage)
(defvar url-http-referer)
(defvar url-http-target-url)
(defvar url-http-version)
(defvar url-mime-encoding-string)

(defmacro e-openai-test--with-prefix-cache-capability (capability &rest body)
  "Run direct request-renderer tests under test-owned CAPABILITY evidence.
Production adapters derive these values from the endpoint and model."
  (declare (indent 1) (debug t))
  `(let ((e-openai-profile--effective-prefix-cache ,capability)
         (e-openai-profile--effective-cache-routing-key nil)
         (e-openai-profile--effective-cache-retention nil)
         (e-openai-profile--effective-cache-capability-evidence 'test-owned))
     ,@body))

(defmacro e-openai-test--with-curation-capability (&rest body)
  "Run a renderer-only schema assertion with synthetic test-owned evidence."
  (declare (indent 0) (debug t))
  `(let ((e-openai-profile--effective-context-curation-capability
          'context-curate-wire))
     ,@body))

(defun e-openai-test--jwt ()
  "Return a fake JWT with a Codex account-id claim."
  (let* ((payload (e-json-serialize
                   '(:https://api.openai.com/auth
                     (:chatgpt_account_id "acct-test"))))
         (encoded (base64-encode-string payload 'no-line-break)))
    (setq encoded (string-replace "+" "-" encoded))
    (setq encoded (string-replace "/" "_" encoded))
    (setq encoded (replace-regexp-in-string "=+$" "" encoded))
    (format "header.%s.signature" encoded)))

(defun e-openai-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun e-openai-test--without-explicit-cache-fields (body)
  "Return JSON-like BODY without provider-specific explicit cache fields."
  (let ((copy (e-json-parse-string (e-json-serialize body))))
    (cl-remf copy :prompt_cache_options)
    (dolist (item (append (plist-get copy :input) nil))
      (dolist (content (append (plist-get item :content) nil))
        (when (listp content)
          (cl-remf content :prompt_cache_breakpoint))))
    copy))

(defun e-openai-test--assert-observation-delivery (capabilities replaceable)
  "Assert per-kind delivery in CAPABILITIES according to REPLACEABLE."
  (dolist (kind '(current-state dynamic-context))
    (should
     (eq (e-backend-observation-delivery-for-kind capabilities kind)
         (if replaceable 'request-local-replaceable 'inherited))))
  (dolist (kind '(tool-result trace retrieved-excerpt))
    (should
     (eq (e-backend-observation-delivery-for-kind capabilities kind)
         'inherited))))

(provide 'e-openai-test-support)

;;; e-openai-test-support.el ends here
