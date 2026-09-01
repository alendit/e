;;; e-runtime-store-codec.el --- Exact values for the runtime-store protocol -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Canonical, versioned tagged values shared by the interactive adapter and
;; its subordinate worker.  This is transport shape only; domain modules still
;; decide which values are meaningful for their commands.

;;; Code:

(require 'cl-lib)

(define-error 'e-runtime-store-codec-error "Runtime store value is invalid")

(defconst e-runtime-store-codec-version 1)

(defun e-runtime-store-codec--finite-number-p (value)
  "Return non-nil when VALUE is a finite supported number."
  (or (integerp value)
      (and (floatp value)
           (= value value)
           (not (= (abs value) 1.0e+INF)))))

(defun e-runtime-store-codec--key-bytes (value)
  "Return canonical sorting bytes for encoded VALUE."
  (encode-coding-string (prin1-to-string value) 'utf-8-unix))

(defun e-runtime-store-codec--encode-cons (value active)
  "Return compact tagged cons VALUE while rejecting cycles with ACTIVE."
  (let ((cursor value) nodes items)
    (unwind-protect
        (progn
          (while (consp cursor)
            (when (gethash cursor active)
              (signal 'e-runtime-store-codec-error
                      (list "Cyclic value" value)))
            (puthash cursor t active)
            (push cursor nodes)
            (push (e-runtime-store-codec--encode (car cursor) active) items)
            (setq cursor (cdr cursor)))
          (if (null cursor)
              (vector 'list (vconcat (nreverse items)))
            (vector 'list* (vconcat (nreverse items))
                    (e-runtime-store-codec--encode cursor active))))
      (dolist (node nodes) (remhash node active)))))

(defun e-runtime-store-codec--encode (value active)
  "Return tagged VALUE while using ACTIVE for cycle rejection."
  (cond
   ((null value) [nil])
   ((eq value t) [true])
   ((eq value :json-false) [false])
   ((integerp value) (vector 'integer (number-to-string value)))
   ((floatp value)
    (unless (e-runtime-store-codec--finite-number-p value)
      (signal 'e-runtime-store-codec-error (list "Non-finite number" value)))
    ;; `prin1-to-string' retains the decimal marker for integral floats (1.0),
    ;; unlike %g, while remaining round-trip precise for Emacs floats.
    (vector 'float (prin1-to-string value)))
   ((stringp value) (vector 'string (copy-sequence value)))
   ((keywordp value) (vector 'keyword (symbol-name value)))
   ((symbolp value) (vector 'symbol (symbol-name value)))
   ((consp value)
    (e-runtime-store-codec--encode-cons value active))
   ((or (vectorp value) (hash-table-p value))
    (when (gethash value active)
      (signal 'e-runtime-store-codec-error (list "Cyclic value" value)))
    (puthash value t active)
    (unwind-protect
        (if (vectorp value)
            (vector 'vector
                    (vconcat
                     (mapcar (lambda (item)
                               (e-runtime-store-codec--encode item active))
                             (append value nil))))
          (let (entries)
            (maphash
             (lambda (key item)
               (push (vector (e-runtime-store-codec--encode key active)
                             (e-runtime-store-codec--encode item active))
                     entries))
             value)
            (setq entries
                  (sort entries
                        (lambda (left right)
                          (string<
                           (e-runtime-store-codec--key-bytes (aref left 0))
                           (e-runtime-store-codec--key-bytes (aref right 0))))))
            (vector 'map (hash-table-test value) (vconcat entries))))
      (remhash value active)))
   (t
    (signal 'e-runtime-store-codec-error
            (list "Unsupported durable value" value)))))

(defun e-runtime-store-codec-encode (value)
  "Return canonical unibyte tagged encoding for VALUE."
  (let ((print-circle nil)
        (print-level nil)
        (print-length nil))
    (encode-coding-string
     (prin1-to-string
      (vector 'e-runtime-store-value e-runtime-store-codec-version
              (e-runtime-store-codec--encode
               value (make-hash-table :test 'eq))))
     'utf-8-unix)))

(defun e-runtime-store-codec--decode (value)
  "Decode one tagged VALUE."
  (unless (and (vectorp value) (> (length value) 0))
    (signal 'e-runtime-store-codec-error (list "Invalid tagged value" value)))
  (let ((tag (aref value 0)))
    (cond
     ((eq tag 'nil)
      (unless (= (length value) 1)
        (signal 'e-runtime-store-codec-error (list "Invalid nil" value)))
      nil)
     ((eq tag 'true) t)
     ((eq tag 'false) :json-false)
     ((eq tag 'integer)
      (let ((text (and (= (length value) 2) (aref value 1))))
        (unless (and (stringp text) (string-match-p "\\`-?[0-9]+\\'" text))
          (signal 'e-runtime-store-codec-error (list "Invalid integer" value)))
        (string-to-number text)))
     ((eq tag 'float)
      (let* ((text (and (= (length value) 2) (aref value 1)))
             (number (and (stringp text) (string-to-number text))))
        (unless (and (floatp number)
                     (e-runtime-store-codec--finite-number-p number))
          (signal 'e-runtime-store-codec-error (list "Invalid float" value)))
        number))
     ((eq tag 'string)
      (let ((text (and (= (length value) 2) (aref value 1))))
        (unless (stringp text)
          (signal 'e-runtime-store-codec-error (list "Invalid string" value)))
        (copy-sequence text)))
     ((memq tag '(keyword symbol))
      (let ((name (and (= (length value) 2) (aref value 1))))
        (unless (and (stringp name)
                     (or (eq tag 'symbol) (string-prefix-p ":" name)))
          (signal 'e-runtime-store-codec-error (list "Invalid symbol" value)))
        (intern name)))
     ((memq tag '(vector list))
      (let ((items (and (= (length value) 2) (aref value 1))))
        (unless (vectorp items)
          (signal 'e-runtime-store-codec-error (list "Invalid sequence" value)))
        (let ((decoded
               (mapcar #'e-runtime-store-codec--decode (append items nil))))
          (if (eq tag 'vector) (vconcat decoded) decoded))))
     ((eq tag 'list*)
      (let ((items (and (= (length value) 3) (aref value 1)))
            (tail (and (= (length value) 3) (aref value 2))))
        (unless (vectorp items)
          (signal 'e-runtime-store-codec-error
                  (list "Invalid dotted list" value)))
        (append (mapcar #'e-runtime-store-codec--decode (append items nil))
                (e-runtime-store-codec--decode tail))))
     ((eq tag 'map)
      (let* ((test (and (= (length value) 3) (aref value 1)))
             (entries (and (= (length value) 3) (aref value 2))))
        (unless (and (memq test '(eq eql equal equal-including-properties))
                     (vectorp entries))
          (signal 'e-runtime-store-codec-error (list "Invalid map" value)))
        (let ((table (make-hash-table :test test)))
          (dolist (entry (append entries nil) table)
            (unless (and (vectorp entry) (= (length entry) 2))
              (signal 'e-runtime-store-codec-error
                      (list "Invalid map entry" entry)))
            (puthash (e-runtime-store-codec--decode (aref entry 0))
                     (e-runtime-store-codec--decode (aref entry 1)) table)))))
     (t
      (signal 'e-runtime-store-codec-error
              (list "Unknown value tag" value))))))

(defun e-runtime-store-codec-decode (bytes)
  "Decode canonical tagged BYTES and reject trailing input."
  (unless (stringp bytes)
    (signal 'wrong-type-argument (list 'stringp bytes)))
  (condition-case err
      (pcase-let* ((text (decode-coding-string bytes 'utf-8-unix))
                   (`(,form . ,position) (read-from-string text)))
        (unless (string-match-p "\\`[[:space:]]*\\'" (substring text position))
          (signal 'e-runtime-store-codec-error
                  (list "Trailing value bytes" position (length text)
                        (substring text position
                                   (min (length text) (+ position 40))))))
        (pcase form
          (`[e-runtime-store-value ,version ,payload]
           (unless (= version e-runtime-store-codec-version)
             (signal 'e-runtime-store-codec-error
                     (list "Unknown codec version" version)))
           (e-runtime-store-codec--decode payload))
          (_ (signal 'e-runtime-store-codec-error
                     (list "Invalid value envelope" form)))))
    (e-runtime-store-codec-error (signal (car err) (cdr err)))
    (error (signal 'e-runtime-store-codec-error (list err)))))

(provide 'e-runtime-store-codec)

;;; e-runtime-store-codec.el ends here
