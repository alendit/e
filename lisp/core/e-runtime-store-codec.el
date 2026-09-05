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
(define-error 'e-runtime-store-codec-too-large
  "Runtime store canonical value exceeds its byte limit"
  'e-runtime-store-codec-error)

(defconst e-runtime-store-codec-version 1)

(defconst e-runtime-store-codec-protocol-canonical-byte-limit
  (* 68 1024 1024)
  "Maximum canonical bytes in one runtime-store protocol value.")

(defconst e-runtime-store-codec-protocol-wire-byte-limit
  (1+ (* 4 (ceiling e-runtime-store-codec-protocol-canonical-byte-limit 3)))
  "Maximum base64 protocol bytes, including the newline delimiter.")

(defconst e-runtime-store-codec-checkpoint-canonical-byte-limit (* 1024 1024)
  "Maximum canonical bytes in one rebuildable session checkpoint value.")

(defconst e-runtime-store-codec-catalog-canonical-byte-limit (* 1024 1024)
  "Maximum canonical bytes in one rebuildable session catalog projection.")

(defun e-runtime-store-codec-wire-byte-count (canonical-byte-count)
  "Return newline-framed base64 bytes for CANONICAL-BYTE-COUNT.

CANONICAL-BYTE-COUNT is measured after tagged UTF-8 encoding; the result is
the exact ASCII wire allocation including its one newline delimiter."
  (unless (and (integerp canonical-byte-count)
               (>= canonical-byte-count 0))
    (signal 'wrong-type-argument (list 'natnump canonical-byte-count)))
  (1+ (* 4 (ceiling canonical-byte-count 3))))

(defun e-runtime-store-codec--canonical-byte-limit-error (limit bytes)
  "Signal that canonical output reached LIMIT at BYTES.

BYTES is the first observed byte count beyond LIMIT, rather than a claim about
the total size of an intentionally unmaterialized value."
  (signal 'e-runtime-store-codec-too-large
          (list "Canonical value exceeds byte limit"
                :limit limit :observed-at-least bytes)))

(defconst e-runtime-store-codec--reader-escaped-character-regexp
  (regexp-opt '("\"" "\\") t)
  "Regexp for reader string characters that take one extra output byte.")

(defconst e-runtime-store-codec--unibyte-high-octet-regexp
  (concat "[" (unibyte-string 128) "-" (unibyte-string 255) "]")
  "Regexp for unibyte octets printed as four-byte octal escapes.")

(defconst e-runtime-store-codec--multibyte-raw-byte-regexp
  (concat "[" (string #x3fff80) "-" (string #x3fffff) "]")
  "Regexp for Emacs multibyte raw-byte characters.")

(defun e-runtime-store-codec--string-has-text-properties-p (value)
  "Return non-nil when string VALUE carries presentation text properties."
  (let ((position 0)
        (length (length value))
        properties)
    (while (and (< position length) (not properties))
      (setq properties (text-properties-at position value)
            position (next-property-change position value length)))
    properties))

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
   ((stringp value)
    ;; Text properties are presentation state, not a durable value grammar.
    ;; Rejecting them keeps the private tagged printer form closed enough for
    ;; bounded exact measurement; prior stored values remain decoder-readable.
    (when (e-runtime-store-codec--string-has-text-properties-p value)
      (signal 'e-runtime-store-codec-error
              ;; Do not retain an arbitrarily large rejected string in the
              ;; error condition: callers need its category and size, not its
              ;; presentation payload.
              (list "Text properties are not durable"
                    :string-bytes (string-bytes value))))
    (vector 'string (copy-sequence value)))
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

(defun e-runtime-store-codec--form (value)
  "Return the private tagged form whose printed bytes encode VALUE."
  (vector 'e-runtime-store-value e-runtime-store-codec-version
          (e-runtime-store-codec--encode
          value (make-hash-table :test 'eq))))

(defun e-runtime-store-codec-freeze-bounded (value limit)
  "Return a deep immutable snapshot of closed durable VALUE within LIMIT.

The exact codec measurer first rejects cycles and oversized values without a
second complete representation.  Only after that admission check does this
function copy mutable strings, cons cells, vectors, and hash entries.  Shared
acyclic descendants remain shared in the copy, while every mutable descendant
is detached from the caller."
  (e-runtime-store-codec-measure-bounded value limit)
  (let ((active (make-hash-table :test 'eq))
        (memo (make-hash-table :test 'eq)))
    (cl-labels
        ((freeze (item)
           (cond
            ((stringp item) (copy-sequence item))
            ((consp item)
             (cond ((gethash item active)
                    (signal 'e-runtime-store-codec-error (list "Cyclic value")))
                   ((gethash item memo))
                   (t
                    (puthash item t active)
                    (let ((copy (cons nil nil)))
                      (puthash item copy memo)
                      (setcar copy (freeze (car item)))
                      (setcdr copy (freeze (cdr item)))
                      (remhash item active)
                      copy))))
            ((vectorp item)
             (cond ((gethash item active)
                    (signal 'e-runtime-store-codec-error (list "Cyclic value")))
                   ((gethash item memo))
                   (t
                    (puthash item t active)
                    (let ((copy (make-vector (length item) nil)))
                      (puthash item copy memo)
                      (dotimes (index (length item))
                        (aset copy index (freeze (aref item index))))
                      (remhash item active)
                      copy))))
            ((hash-table-p item)
             (cond ((gethash item active)
                    (signal 'e-runtime-store-codec-error (list "Cyclic value")))
                   ((gethash item memo))
                   (t
                    (puthash item t active)
                    (let ((copy (make-hash-table :test (hash-table-test item)
                                                 :size (hash-table-count item))))
                      (puthash item copy memo)
                      (maphash (lambda (key entry)
                                 (puthash (freeze key) (freeze entry) copy)) item)
                      (remhash item active)
                      copy))))
            (t item))))
      (freeze value))))

(defun e-runtime-store-codec--print-form (form)
  "Return canonical unibyte printed FORM.

FORM must already be the private tagged value built by
`e-runtime-store-codec--form'."
  (let ((print-circle nil)
        (print-level nil)
        (print-length nil)
        ;; Pin the reader syntax counted below instead of inheriting a
        ;; caller's temporary print preferences.
        (print-escape-newlines nil)
        (print-escape-control-characters nil)
        (print-escape-nonascii nil)
        (print-escape-multibyte nil)
        (print-quoted t)
        (print-gensym nil)
        (print-base 10)
        (print-radix nil)
        (print-readably nil))
    (encode-coding-string
     (prin1-to-string form)
     'utf-8-unix)))

(defun e-runtime-store-codec--string-byte-count-bounded
    (value limit bytes)
  "Return BYTE count after reader-printing property-free string VALUE.

VALUE is already accepted by the closed tagged codec grammar.  The current
canonical reader syntax writes multibyte characters directly as UTF-8, quotes
and backslashes with one extra ASCII byte, and unibyte octets >= 128 as a
four-byte octal escape.  Emacs multibyte raw-byte characters also print as
four-byte octal escapes.  Native regexp counting over one disposable buffer
avoids a Lisp call per input byte and never builds the escaped representation."
  (let ((multibyte (multibyte-string-p value))
        escaped)
    ;; `count-matches' scans in C.  Keep the original representation in the
    ;; scratch buffer: a multibyte character must not become an unibyte UTF-8
    ;; octet before we decide whether it needs octal escaping.
    (with-temp-buffer
      (set-buffer-multibyte multibyte)
      (insert value)
      (setq escaped
            (count-matches e-runtime-store-codec--reader-escaped-character-regexp
                           (point-min) (point-max)))
      (setq escaped
            (+ escaped
               (if multibyte
                   ;; A raw-byte character occupies two bytes in its source
                   ;; multibyte string but prints as four ASCII octal bytes.
                   (* 2
                      (count-matches
                       e-runtime-store-codec--multibyte-raw-byte-regexp
                       (point-min) (point-max)))
                 ;; A high unibyte octet occupies one source byte and prints
                 ;; as four ASCII octal bytes.
                 (* 3
                    (count-matches e-runtime-store-codec--unibyte-high-octet-regexp
                                   (point-min) (point-max)))))))
    ;; Reader string delimiters plus literal UTF-8 bytes and escape growth.
    (setq bytes (+ bytes 2 (string-bytes value) escaped))
    (when (> bytes limit)
      (e-runtime-store-codec--canonical-byte-limit-error limit bytes))
    bytes))

(defun e-runtime-store-codec--small-atom-byte-count (value)
  "Return canonical UTF-8 reader bytes for one fixed tagged-form atom VALUE."
  ;; The tagged form owns only its fixed symbols and codec version integer.
  ;; Keeping this tiny fallback on the actual printer makes any future fixed
  ;; atom spelling exact without reintroducing large value allocation.
  (string-bytes
   (encode-coding-string (prin1-to-string value) 'utf-8-unix)))

(defun e-runtime-store-codec--measure-form-bounded (form limit)
  "Return FORM's UTF-8 byte count, stopping once it exceeds LIMIT.

The private tagged form contains only fixed atoms, vectors, and property-free
strings.  Walk that closed reader grammar directly, rejecting an oversized
canonical representation before its complete escaped string exists.  LIMIT is
a nonnegative integer byte count."
  (unless (and (integerp limit) (>= limit 0))
    (signal 'wrong-type-argument (list 'natnump limit)))
  (let ((bytes 0)
        ;; Keep the small fixed-atom printer under exactly the same canonical
        ;; syntax settings as `e-runtime-store-codec--print-form'.
        (print-circle nil)
        (print-level nil)
        (print-length nil)
        (print-escape-newlines nil)
        (print-escape-control-characters nil)
        (print-escape-nonascii nil)
        (print-escape-multibyte nil)
        (print-quoted t)
        (print-gensym nil)
        (print-base 10)
        (print-radix nil)
        (print-readably nil))
    (cl-labels
        ((add (count)
           (setq bytes (+ bytes count))
           (when (> bytes limit)
             (e-runtime-store-codec--canonical-byte-limit-error limit bytes)))
         (walk (value)
           (cond
            ((stringp value)
             (setq bytes
                   (e-runtime-store-codec--string-byte-count-bounded
                    value limit bytes)))
            ((or (symbolp value) (integerp value))
             (add (e-runtime-store-codec--small-atom-byte-count value)))
            ((vectorp value)
             (add 1)                    ; [
             (dotimes (index (length value))
               (when (> index 0) (add 1)) ; separating space
               (walk (aref value index)))
             (add 1))                    ; ]
            (t
             (signal 'e-runtime-store-codec-error
                     (list "Invalid private tagged form" value))))))
      (walk form))
    bytes))

(defun e-runtime-store-codec-measure-bounded (value limit)
  "Return VALUE's exact canonical byte count without building its tagged form.

This mirrors the closed tagged grammar used by `e-runtime-store-codec-encode'.
It is the admission-side counterpart to encoding: callers can reject an
oversized original value before allocating its complete transformed form or
printed canonical string.  Hash-entry order affects bytes only by permutation,
so measuring entries directly preserves the exact total without materializing
the encoder's sort keys."
  (unless (and (integerp limit) (>= limit 0))
    (signal 'wrong-type-argument (list 'natnump limit)))
  (let ((bytes 0) (active (make-hash-table :test 'eq)))
    (cl-labels
        ((add (count) (setq bytes (+ bytes count))
              (when (> bytes limit)
                (e-runtime-store-codec--canonical-byte-limit-error limit bytes)))
         (atom (thing)
           (if (stringp thing)
               (setq bytes (e-runtime-store-codec--string-byte-count-bounded thing limit bytes))
             (add (e-runtime-store-codec--small-atom-byte-count thing))))
         (start (tag) (add 1) (atom tag))
         (item () (add 1))
         (finish () (add 1))
         (encoded-list (list)
           ;; Count in canonical order without premarking the spine or
           ;; allocating per-element closures.  The list/list* tag differs by
           ;; one byte; that correction is applied only if a dotted tail is
           ;; reached, while byte accounting itself streams each head.
           (let ((cursor list) (first t))
             (unwind-protect
                 (progn
                   (start 'list) (item) (add 1)
                   (while (consp cursor)
                     (when (gethash cursor active)
                       (signal 'e-runtime-store-codec-error (list "Cyclic value" list)))
                     (puthash cursor t active)
                     (unless first (item)) (setq first nil)
                     (encoded (car cursor))
                     (setq cursor (cdr cursor)))
                   (finish)
                   (unless (null cursor)
                     (add 1) (item) (encoded cursor))
                   (finish))
               (let ((node list))
                 (while (and (consp node) (gethash node active))
                   (remhash node active) (setq node (cdr node)))))))
         (encoded (thing)
           (cond
            ((null thing) (start 'nil) (finish))
            ((eq thing t) (start 'true) (finish))
            ((eq thing :json-false) (start 'false) (finish))
            ((integerp thing) (start 'integer) (item) (atom (number-to-string thing)) (finish))
            ((floatp thing) (unless (e-runtime-store-codec--finite-number-p thing)
                              (signal 'e-runtime-store-codec-error (list "Non-finite number" thing)))
             (start 'float) (item) (atom (prin1-to-string thing)) (finish))
            ((stringp thing) (when (e-runtime-store-codec--string-has-text-properties-p thing)
                                (signal 'e-runtime-store-codec-error (list "Text properties are not durable")))
             (start 'string) (item) (atom thing) (finish))
            ((keywordp thing) (start 'keyword) (item) (atom (symbol-name thing)) (finish))
            ((symbolp thing) (start 'symbol) (item) (atom (symbol-name thing)) (finish))
            ((consp thing) (encoded-list thing))
            ((vectorp thing)
             (when (gethash thing active) (signal 'e-runtime-store-codec-error (list "Cyclic value" thing)))
             (puthash thing t active)
             (unwind-protect (progn (start 'vector) (item) (add 1)
                                    (dotimes (i (length thing)) (when (> i 0) (item)) (encoded (aref thing i)))
                                    (finish) (finish))
               (remhash thing active)))
            ((hash-table-p thing)
             (when (gethash thing active) (signal 'e-runtime-store-codec-error (list "Cyclic value" thing)))
             (puthash thing t active)
             (unwind-protect (progn (start 'map) (item) (atom (hash-table-test thing)) (item) (add 1)
                                    (let ((first t))
                                      (maphash (lambda (key value)
                                                 (unless first (item)) (setq first nil)
                                                 (add 1) (encoded key) (item) (encoded value) (finish)) thing))
                                    (finish) (finish))
               (remhash thing active)))
            (t (signal 'e-runtime-store-codec-error (list "Unsupported durable value" thing))))))
      (start 'e-runtime-store-value) (item) (atom e-runtime-store-codec-version) (item) (encoded value) (finish))
    bytes))

(defun e-runtime-store-codec-encode (value)
  "Return canonical unibyte tagged encoding for VALUE."
  (e-runtime-store-codec--print-form (e-runtime-store-codec--form value)))

(defun e-runtime-store-codec-encode-bounded (value limit)
  "Return canonical encoding for VALUE when it fits LIMIT UTF-8 bytes.

The bound is measured against the exact tagged canonical representation.  A
value that exceeds LIMIT signals `e-runtime-store-codec-too-large' before its
complete oversized representation is constructed."
  (let ((form (e-runtime-store-codec--form value)))
    (e-runtime-store-codec--measure-form-bounded form limit)
    (e-runtime-store-codec--print-form form)))

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
