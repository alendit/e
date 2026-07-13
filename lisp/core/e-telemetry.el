;;; e-telemetry.el --- Safe durable telemetry previews for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared redaction and bounded-preview helpers for durable telemetry.  Domain
;; features may link these previews, but raw arguments and results stay outside
;; observability records.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst e-telemetry-preview-max-bytes 4096
  "Default maximum UTF-8 bytes retained in one telemetry preview.")

(defconst e-telemetry-redacted-value "[REDACTED]"
  "Replacement used for values identified as sensitive.")

(defun e-telemetry--sensitive-key-p (key)
  "Return non-nil when KEY names a sensitive value."
  (let ((name (downcase (string-remove-prefix ":" (format "%s" key)))))
    (string-match-p
     (rx (or "authorization" "proxy-authorization" "api-key" "api_key"
             "apikey" "access-token" "access_token" "refresh-token"
             "refresh_token" "password" "passwd" "secret" "credential"
             "cookie" "set-cookie" "token"))
     name)))

(defun e-telemetry-redact-string (text)
  "Return TEXT with common inline credential forms redacted."
  (let ((case-fold-search t)
        (redacted text))
    (setq redacted
          (replace-regexp-in-string
           "\\b\\(https?://[^/:@[:space:]]+:\\)[^@/[:space:]]+@"
           (lambda (match)
             (save-match-data
               (if (string-match ":" match)
                   (concat (substring match 0 (1+ (match-beginning 0)))
                           e-telemetry-redacted-value "@")
                 e-telemetry-redacted-value)))
           redacted t t))
    (setq redacted
          (replace-regexp-in-string
           "\\_<\\(Bearer\\|Basic\\)\\_>[[:space:]]+[^[:space:]'\"]+"
           (lambda (match)
             (save-match-data
               (concat (car (split-string match)) " "
                       e-telemetry-redacted-value)))
           redacted t t))
    (replace-regexp-in-string
     "\\b\\(api[_-]?key\\|access[_-]?token\\|refresh[_-]?token\\|password\\|passwd\\|secret\\|token\\)\\b[[:space:]]*\\([=:]\\)[[:space:]]*['\"]?[^[:space:],;}\"']+"
     (lambda (match)
       (save-match-data
         (if (string-match "[=:]" match)
             (concat (substring match 0 (1+ (match-beginning 0)))
                     e-telemetry-redacted-value)
           e-telemetry-redacted-value)))
     redacted t t)))

(defun e-telemetry-redact-value (value &optional seen)
  "Return a copy of VALUE with sensitive fields and strings redacted.
SEEN is an internal cycle guard."
  (let ((seen (or seen (make-hash-table :test 'eq))))
    (cond
     ((stringp value) (e-telemetry-redact-string value))
     ((or (null value) (numberp value) (symbolp value)) value)
     ((gethash value seen) "[CYCLE]")
     ((hash-table-p value)
      (puthash value t seen)
      (let ((copy (make-hash-table :test (hash-table-test value))))
        (maphash
         (lambda (key item)
           (puthash key
                    (if (e-telemetry--sensitive-key-p key)
                        e-telemetry-redacted-value
                      (e-telemetry-redact-value item seen))
                    copy))
         value)
        copy))
     ((vectorp value)
      (puthash value t seen)
      (vconcat (mapcar (lambda (item)
                         (e-telemetry-redact-value item seen))
                       value)))
     ((consp value)
      (puthash value t seen)
      (if (and (proper-list-p value)
               (cl-evenp (length value))
               (cl-loop for (key _item) on value by #'cddr
                        always (or (keywordp key) (symbolp key) (stringp key))))
          (let ((tail value)
                result)
            (while tail
              (let ((key (pop tail))
                    (item (pop tail)))
                (setq result
                      (append result
                              (list key
                                    (if (e-telemetry--sensitive-key-p key)
                                        e-telemetry-redacted-value
                                      (e-telemetry-redact-value item seen)))))))
            result)
        (mapcar (lambda (item) (e-telemetry-redact-value item seen)) value)))
     (t (format "%s" value)))))

(defun e-telemetry--byte-prefix (text max-bytes)
  "Return a prefix of TEXT no larger than MAX-BYTES UTF-8 bytes."
  (let ((bytes 0)
        (index 0))
    (while (and (< index (length text))
                (let ((next (string-bytes (substring text index (1+ index)))))
                  (when (<= (+ bytes next) max-bytes)
                    (setq bytes (+ bytes next))
                    t)))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-telemetry-preview (value &optional max-bytes)
  "Return bounded redacted printable telemetry metadata for VALUE."
  (let* ((text (prin1-to-string value))
         (original-bytes (string-bytes text))
         (redacted-text (prin1-to-string (e-telemetry-redact-value value)))
         (redacted-bytes (string-bytes redacted-text))
         (limit (max 0 (or max-bytes e-telemetry-preview-max-bytes)))
         (truncated (> redacted-bytes limit))
         (content (if truncated
                      (e-telemetry--byte-prefix redacted-text limit)
                    redacted-text)))
    (list :content content
          :redaction-policy 'telemetry-preview-v1
          :redacted (not (equal text redacted-text))
          :truncated truncated
          :original-bytes original-bytes
          :redacted-bytes redacted-bytes
          :shown-bytes (string-bytes content))))

(provide 'e-telemetry)

;;; e-telemetry.el ends here
