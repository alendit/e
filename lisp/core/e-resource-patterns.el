;;; e-resource-patterns.el --- Portable resource pattern facade -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared syntax helpers for resource glob and search operations.  This module
;; owns the public facade pattern language; resource schemes own enumeration,
;; storage, and backend execution.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(define-error 'e-resource-pattern-invalid
  "Resource pattern is invalid")

(defun e-resource-pattern--invalid (format-string &rest args)
  "Signal an invalid resource pattern using FORMAT-STRING and ARGS."
  (signal 'e-resource-pattern-invalid
          (list (apply #'format format-string args))))

(defun e-resource-pattern--glob-pattern (pattern)
  "Return normalized glob PATTERN."
  (let ((pattern (or pattern "*")))
    (if (string-empty-p pattern) "*" pattern)))

(defun e-resource-pattern--glob-segments (pattern)
  "Return validated facade glob PATTERN segments."
  (let ((pattern (e-resource-pattern--glob-pattern pattern)))
    (when (string-prefix-p "/" pattern)
      (e-resource-pattern--invalid
       "Resource glob patterns are relative and must not start with /: %s"
       pattern))
    (let ((segments (split-string pattern "/" nil)))
      (when (member "" segments)
        (e-resource-pattern--invalid
         "Resource glob patterns must not contain empty path segments: %s"
         pattern))
      (dolist (segment segments)
        (when (and (string-match-p "\\*\\*" segment)
                   (not (string= segment "**")))
          (e-resource-pattern--invalid
           "** is only supported as a complete path segment: %s"
           pattern)))
      segments)))

(defun e-resource-pattern--glob-segment-regexp (segment)
  "Return regexp for one non-** glob SEGMENT."
  (mapconcat
   (lambda (char)
     (if (= char ?*)
         "[^/]*"
       (regexp-quote (char-to-string char))))
   segment
   ""))

(defun e-resource-pattern--glob-segment-fd-glob (segment)
  "Return fd glob syntax for one non-** facade glob SEGMENT."
  (mapconcat
   (lambda (char)
     (let ((text (char-to-string char)))
       (cond
        ((= char ?*) "*")
        ((member text '("\\" "?" "[" "]" "{" "}" "!"))
         (concat "\\" text))
        (t text))))
   segment
   ""))

(defun e-resource-pattern-compile-glob (pattern)
  "Compile facade glob PATTERN to an Emacs regexp."
  (let* ((segments (e-resource-pattern--glob-segments pattern))
         (index 0)
         (last-index (1- (length segments)))
         parts)
    (dolist (segment segments)
      (push
       (cond
        ((string= segment "**")
         (if (= index last-index)
             ".*"
           "\\(?:.*/\\)?"))
        (t
         (concat (e-resource-pattern--glob-segment-regexp segment)
                 (if (= index last-index) "" "/"))))
       parts)
      (setq index (1+ index)))
    (concat "\\`" (apply #'concat (nreverse parts)) "\\'")))

(defun e-resource-pattern-glob-fd-pattern (pattern)
  "Translate facade glob PATTERN to fd --glob syntax."
  (mapconcat
   (lambda (segment)
     (if (string= segment "**")
         "**"
       (e-resource-pattern--glob-segment-fd-glob segment)))
   (e-resource-pattern--glob-segments pattern)
   "/"))

(defun e-resource-pattern-glob-max-depth (pattern)
  "Return the maximum path depth for facade glob PATTERN, or nil if unbounded."
  (let ((segments (e-resource-pattern--glob-segments pattern)))
    (unless (member "**" segments)
      (length segments))))

(defun e-resource-pattern-glob-match-p (pattern name &optional case-sensitive)
  "Return non-nil when facade glob PATTERN matches resource NAME.
CASE-SENSITIVE defaults to non-nil."
  (let ((case-fold-search (not (if (null case-sensitive) t case-sensitive))))
    (string-match-p (e-resource-pattern-compile-glob pattern) name)))

(defun e-resource-pattern--trimmed-query (query)
  "Return trimmed search QUERY or signal if it is empty."
  (unless (stringp query)
    (e-resource-pattern--invalid "Resource search query must be a string"))
  (let ((trimmed (string-trim query)))
    (when (string-empty-p trimmed)
      (e-resource-pattern--invalid "Resource search query must not be empty"))
    trimmed))

(defun e-resource-pattern--rg-quote (text)
  "Quote TEXT for Rust/rg regex syntax."
  (mapconcat
   (lambda (char)
     (let ((text (char-to-string char)))
       (if (member text '("\\" "." "^" "$" "|" "?" "*" "+"
                          "(" ")" "[" "]" "{" "}"))
           (concat "\\" text)
         text)))
   text
   ""))

(defun e-resource-pattern--search-part-regexp (part quote-function)
  "Return regexp for one facade search PART using QUOTE-FUNCTION."
  (mapconcat
   (lambda (char)
     (if (= char ?*)
         "[^[:space:]\n\r]*"
       (funcall quote-function (char-to-string char))))
   part
   ""))

(defun e-resource-pattern--search-regexp (query options quote-function)
  "Compile facade search QUERY with OPTIONS using QUOTE-FUNCTION."
  (let* ((parts (split-string (e-resource-pattern--trimmed-query query)
                              "[ \t\r\n]+"
                              t))
         (gap (if (plist-get options :multiline)
                  "[[:space:]\n\r]+"
                "[ \t]+"))
         (body (mapconcat
                (lambda (part)
                  (e-resource-pattern--search-part-regexp
                   part
                   quote-function))
                parts
                gap)))
    (if (plist-get options :whole-word)
        (concat "\\b" body "\\b")
      body)))

(defun e-resource-pattern-search-emacs-regexp (query options)
  "Compile facade search QUERY with OPTIONS to an Emacs regexp."
  (e-resource-pattern--search-regexp query options #'regexp-quote))

(defun e-resource-pattern-search-rg-regexp (query options)
  "Compile facade search QUERY with OPTIONS to an rg-compatible regexp."
  (e-resource-pattern--search-regexp query options #'e-resource-pattern--rg-quote))

(provide 'e-resource-patterns)

;;; e-resource-patterns.el ends here
