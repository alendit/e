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
(require 'seq)
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

(defun e-resource-pattern-search-terms (query)
  "Return normalized search terms from QUERY."
  (split-string (e-resource-pattern--trimmed-query query) "[ \t\r\n]+" t))

(defun e-resource-pattern--search-regexp (query options quote-function)
  "Compile facade search QUERY with OPTIONS using QUOTE-FUNCTION."
  (let* ((parts (e-resource-pattern-search-terms query))
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

(defcustom e-resource-pattern-default-search-limit 1000
  "Default maximum ranked search matches."
  :type 'integer
  :group 'e)

(defun e-resource-pattern-search-limit (limit)
  "Return normalized search LIMIT."
  (cond
   ((null limit) e-resource-pattern-default-search-limit)
   ((and (numberp limit) (> limit 0)) (truncate limit))
   (t (signal 'wrong-type-argument (list 'positive-number-p limit)))))

(defun e-resource-pattern--search-term-regexp (term options quote-function)
  "Return regexp for one search TERM using OPTIONS and QUOTE-FUNCTION."
  (let ((body (e-resource-pattern--search-part-regexp term quote-function)))
    (if (plist-get options :whole-word)
        (concat "\\b" body "\\b")
      body)))

(defun e-resource-pattern-search-rg-prefilter-regexp (query options)
  "Compile QUERY to an rg regexp that finds candidate lines.
The regexp matches any single query term.  Callers must rank and filter
candidate text with `e-resource-pattern-search-matches-in-text'."
  (mapconcat
   (lambda (term)
     (e-resource-pattern--search-term-regexp
      term options #'e-resource-pattern--rg-quote))
   (e-resource-pattern-search-terms query)
   "|"))

(defun e-resource-pattern--search-term-matches (text query options)
  "Return term match data when TEXT contains every term in QUERY."
  (let ((case-fold-search (not (plist-get options :case-sensitive)))
        (terms (e-resource-pattern-search-terms query))
        matches)
    (catch 'missing
      (dolist (term terms)
        (let ((regexp (e-resource-pattern--search-term-regexp
                       term options #'regexp-quote))
              first-start first-end
              (count 0)
              (position 0))
          (while (and (< position (length text))
                      (string-match regexp text position))
            (setq count (1+ count))
            (unless first-start
              (setq first-start (match-beginning 0)
                    first-end (match-end 0)))
            (setq position (match-end 0))
            (when (= (match-beginning 0) (match-end 0))
              (setq position (1+ position))))
          (unless first-start
            (throw 'missing nil))
          (push (list :term term
                      :start first-start
                      :end first-end
                      :count count)
                matches)))
      (nreverse matches))))

(defun e-resource-pattern--search-position-line-column (text position start-line)
  "Return line and column for POSITION in TEXT starting at START-LINE."
  (let ((line start-line)
        (line-start 0)
        (index 0))
    (while (< index position)
      (when (= (aref text index) ?\n)
        (setq line (1+ line)
              line-start (1+ index)))
      (setq index (1+ index)))
    (cons line (1+ (- position line-start)))))

(defun e-resource-pattern--search-line-boundaries (text position)
  "Return the current line boundaries around POSITION in TEXT."
  (let ((start position)
        (end position)
        (length (length text)))
    (while (and (> start 0)
                (/= (aref text (1- start)) ?\n))
      (setq start (1- start)))
    (while (and (< end length)
                (/= (aref text end) ?\n))
      (setq end (1+ end)))
    (cons start end)))

(defun e-resource-pattern--search-uri-score (uri name query options)
  "Return a small score boost when URI or NAME contains QUERY terms."
  (let ((haystack (concat (or name "") "\n" (or uri ""))))
    (if (e-resource-pattern--search-term-matches haystack query options)
        120
      0)))

(defun e-resource-pattern-search-score (text query options &optional uri name)
  "Return ranked lexical match data for TEXT and QUERY, or nil.
All query terms must occur in TEXT.  Higher scores are better."
  (when-let ((matches (e-resource-pattern--search-term-matches text query options)))
    (let* ((starts (mapcar (lambda (match) (plist-get match :start)) matches))
           (ends (mapcar (lambda (match) (plist-get match :end)) matches))
           (first (apply #'min starts))
           (last (apply #'max ends))
           (span (- last first))
           (counts (apply #'+ (mapcar (lambda (match)
                                        (plist-get match :count))
                                      matches)))
           (case-fold-search (not (plist-get options :case-sensitive)))
           (phrase-bonus (if (string-match-p
                               (e-resource-pattern-search-emacs-regexp
                                query options)
                               text)
                              1000
                            0))
           (proximity-bonus (max 0 (- 300 span)))
           (count-bonus (min 150 (* 15 counts)))
           (uri-bonus (e-resource-pattern--search-uri-score uri name query options))
           (score (+ 100 phrase-bonus proximity-bonus count-bonus uri-bonus)))
      (list :score score
            :column (1+ first)
            :matched-terms (vconcat (mapcar (lambda (match)
                                               (plist-get match :term))
                                             matches))))))

(defun e-resource-pattern-search-matches-in-text
    (uri text query options &optional name start-line)
  "Return one ranked lexical search match for URI in TEXT.
All query terms may occur anywhere in TEXT.  The reported line points at the
first matching term, and `:text' is that line as a compact snippet."
  (when-let ((score (e-resource-pattern-search-score text query options uri name)))
    (let* ((line-number (or start-line 1))
           (position (1- (plist-get score :column)))
           (line-column
            (e-resource-pattern--search-position-line-column
             text position line-number))
           (line-boundaries
            (e-resource-pattern--search-line-boundaries text position)))
      (list (list :uri uri
                  :line (car line-column)
                  :column (cdr line-column)
                  :text (substring text
                                   (car line-boundaries)
                                   (cdr line-boundaries))
                  :score (plist-get score :score)
                  :matched-terms (plist-get score :matched-terms))))))

(defun e-resource-pattern-rank-search-matches (matches &optional limit)
  "Return MATCHES sorted by score and annotated with ranks."
  (let* ((sorted (sort (copy-sequence matches)
                       (lambda (left right)
                         (let ((left-score (or (plist-get left :score) 0))
                               (right-score (or (plist-get right :score) 0)))
                           (if (= left-score right-score)
                               (string-lessp
                                (format "%s:%s:%s"
                                        (plist-get left :uri)
                                        (plist-get left :line)
                                        (plist-get left :column))
                                (format "%s:%s:%s"
                                        (plist-get right :uri)
                                        (plist-get right :line)
                                        (plist-get right :column)))
                             (> left-score right-score))))))
         (selected (if limit (seq-take sorted limit) sorted))
         (rank 1)
         ranked)
    (dolist (match selected (nreverse ranked))
      (push (append (copy-sequence match) (list :rank rank)) ranked)
      (setq rank (1+ rank)))))

(provide 'e-resource-patterns)

;;; e-resource-patterns.el ends here
