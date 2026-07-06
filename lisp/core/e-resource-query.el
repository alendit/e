;;; e-resource-query.el --- Shared resource query controls for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Helpers for resource glob and search implementations that support common
;; sort, date-filter, and resource-candidate limit controls.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(define-error 'e-resource-query-unsupported-control
  "Resource query control is not supported")
(define-error 'e-resource-query-invalid-timestamp
  "Resource query timestamp is invalid")

(defun e-resource-query--string (value)
  "Return VALUE as a string for comparison."
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun e-resource-query-normalize-sort-order (sort-order)
  "Return normalized SORT-ORDER symbol.
Nil defaults to `desc'.  Accepted string values are `asc' and `desc'."
  (cond
   ((null sort-order) 'desc)
   ((and (stringp sort-order) (string= sort-order "asc")) 'asc)
   ((and (stringp sort-order) (string= sort-order "desc")) 'desc)
   (t
    (signal 'wrong-type-argument
            (list '(member "asc" "desc") sort-order)))))

(defun e-resource-query-parse-time (value control)
  "Parse timestamp VALUE for CONTROL and return an Emacs time value."
  (unless (stringp value)
    (signal 'wrong-type-argument (list 'stringp value)))
  (condition-case nil
      (let ((time (date-to-time value)))
        (unless time
          (signal 'error nil))
        time)
    (error
     (signal 'e-resource-query-invalid-timestamp
             (list (format "Invalid %s timestamp: %s" control value))))))

(defun e-resource-query--time (value)
  "Return VALUE normalized as an Emacs time, or nil."
  (cond
   ((null value) nil)
   ((stringp value) (ignore-errors (date-to-time value)))
   ((numberp value) (seconds-to-time value))
   ((consp value) value)
   (t nil)))

(defun e-resource-query--field-value (entry field field-functions)
  "Return FIELD value for ENTRY using FIELD-FUNCTIONS or standard places."
  (let ((key (intern (concat ":" field))))
    (cond
     ((equal field "default") nil)
     ((alist-get field field-functions nil nil #'equal)
      (funcall (alist-get field field-functions nil nil #'equal) entry))
     ((plist-member entry key) (plist-get entry key))
     ((plist-member (plist-get entry :metadata) key)
      (plist-get (plist-get entry :metadata) key))
     (t nil))))

(defun e-resource-query--control-present-p (options keys)
  "Return non-nil if OPTIONS contains any of KEYS."
  (seq-some (lambda (key) (plist-member options key)) keys))

(defun e-resource-query--unsupported (scheme control supported)
  "Signal unsupported CONTROL for SCHEME naming SUPPORTED values."
  (signal 'e-resource-query-unsupported-control
          (list (format "Resource scheme %s does not support query control %s%s"
                        scheme
                        control
                        (if supported
                            (format "; supported values: %s"
                                    (string-join supported ", "))
                          "")))))

(defun e-resource-query-validate-no-advanced
    (scheme sort-by sort-order created-after created-before updated-after updated-before)
  "Signal if SCHEME received any advanced glob query control."
  (when sort-by
    (e-resource-query--unsupported scheme "sort-by" nil))
  (when sort-order
    (e-resource-query--unsupported scheme "sort-order" nil))
  (dolist (pair `(("created-after" . ,created-after)
                  ("created-before" . ,created-before)
                  ("updated-after" . ,updated-after)
                  ("updated-before" . ,updated-before)))
    (when (cdr pair)
      (e-resource-query--unsupported scheme (car pair) nil))))

(defun e-resource-query-validate-search-no-advanced (scheme options)
  "Signal if SCHEME received advanced search query controls in OPTIONS."
  (dolist (pair '((:resource-sort-by . "resource-sort-by")
                  (:resource-sort-order . "resource-sort-order")
                  (:resource-limit . "resource-limit")
                  (:created-after . "created-after")
                  (:created-before . "created-before")
                  (:updated-after . "updated-after")
                  (:updated-before . "updated-before")))
    (when (plist-member options (car pair))
      (e-resource-query--unsupported scheme (cdr pair) nil))))

(defun e-resource-query-resource-limit (limit &optional default)
  "Return normalized resource LIMIT.
When LIMIT is nil, return DEFAULT."
  (cond
   ((null limit) default)
   ((and (numberp limit) (> limit 0)) (truncate limit))
   (t (signal 'wrong-type-argument (list 'positive-number-p limit)))))

(cl-defun e-resource-query-apply
    (entries scheme supported-sort-fields supported-date-fields
             &key sort-by sort-order created-after created-before
             updated-after updated-before field-functions)
  "Return ENTRIES filtered and sorted using shared resource query controls.
SCHEME is used in clear unsupported-control errors.
SUPPORTED-SORT-FIELDS and SUPPORTED-DATE-FIELDS are string lists.
FIELD-FUNCTIONS is an alist of string field names to functions taking one entry."
  (let* ((sort-by (or sort-by "default"))
         (sort-order (e-resource-query-normalize-sort-order sort-order))
         (date-controls `(("created-at" "created-after" ,created-after >=)
                          ("created-at" "created-before" ,created-before <=)
                          ("updated-at" "updated-after" ,updated-after >=)
                          ("updated-at" "updated-before" ,updated-before <=)))
         (result (copy-sequence entries)))
    (unless (member sort-by supported-sort-fields)
      (e-resource-query--unsupported scheme "sort-by" supported-sort-fields))
    (dolist (control date-controls)
      (pcase-let ((`(,field ,name ,value ,_) control))
        (when value
          (unless (member field supported-date-fields)
            (e-resource-query--unsupported scheme name supported-date-fields)))))
    (dolist (control date-controls)
      (pcase-let ((`(,field ,name ,value ,op) control))
        (when value
          (let ((bound (e-resource-query-parse-time value name)))
            (setq result
                  (seq-filter
                   (lambda (entry)
                     (when-let ((entry-time
                                 (e-resource-query--time
                                  (e-resource-query--field-value
                                   entry field field-functions))))
                       (pcase op
                         ('>= (not (time-less-p entry-time bound)))
                         ('<= (not (time-less-p bound entry-time))))))
                   result))))))
    (unless (equal sort-by "default")
      (setq result
            (sort result
                  (lambda (left right)
                    (let ((left-value (e-resource-query--field-value
                                       left sort-by field-functions))
                          (right-value (e-resource-query--field-value
                                        right sort-by field-functions)))
                      (cond
                       ((and (null left-value) (null right-value)) nil)
                       ((null left-value) nil)
                       ((null right-value) t)
                       ((or (string-match-p "-at\\'" sort-by)
                            (member sort-by '("created-at" "updated-at"
                                               "last-message-at")))
                        (let ((left-time (e-resource-query--time left-value))
                              (right-time (e-resource-query--time right-value)))
                          (if (eq sort-order 'asc)
                              (time-less-p left-time right-time)
                            (time-less-p right-time left-time))))
                       ((and (numberp left-value) (numberp right-value))
                        (if (eq sort-order 'asc)
                            (< left-value right-value)
                          (> left-value right-value)))
                       (t
                        (let ((left-string (e-resource-query--string left-value))
                              (right-string (e-resource-query--string right-value)))
                          (if (eq sort-order 'asc)
                              (string-lessp left-string right-string)
                            (string-lessp right-string left-string))))))))))
    result))

(defun e-resource-query-apply-search
    (entries scheme supported-sort-fields supported-date-fields options
             &optional field-functions)
  "Return search candidate ENTRIES after resource-level controls in OPTIONS."
  (let ((sort-by (plist-get options :resource-sort-by))
        (sort-order (plist-get options :resource-sort-order))
        (resource-limit (e-resource-query-resource-limit
                         (plist-get options :resource-limit))))
    (when (and sort-order (null sort-by))
      (setq sort-by "default"))
    (let ((result (e-resource-query-apply
                   entries
                   scheme
                   supported-sort-fields
                   supported-date-fields
                   :sort-by sort-by
                   :sort-order sort-order
                   :created-after (plist-get options :created-after)
                   :created-before (plist-get options :created-before)
                   :updated-after (plist-get options :updated-after)
                   :updated-before (plist-get options :updated-before)
                   :field-functions field-functions)))
      (if resource-limit
          (seq-take result resource-limit)
        result))))

(provide 'e-resource-query)

;;; e-resource-query.el ends here
