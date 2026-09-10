;;; e-session-board-policy.el --- Declarative board-routing policy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure bounded validation, copying, normalization, and wire-size policy for
;; durable board-routing values.  Aggregate association state remains in the
;; session aggregate; this owner has no session mutation state.

;;; Code:

(require 'cl-lib)
(require 'e-board-selector)
(require 'e-session-codec)
(require 'subr-x)

(defun e-session-board-policy--keyword-plist-p (value)
  "Return non-nil when VALUE has keyword plist shape."
  (and (proper-list-p value)
       (let ((tail value)
             (valid t))
         (while (and valid tail)
           (setq valid
                 (and (consp tail)
                      (keywordp (car tail))
                      (consp (cdr tail))))
           (setq tail (cddr tail)))
         valid)))

(define-error 'e-session-board-routing-invalid
  "Invalid board routing policy value")

(defconst e-session-board-policy--keys
  '(:participant-id :pickup-selector :observer-selector :default-tags
    :default-to)
  "Complete durable fields for one board participant routing policy.")

(defconst e-session-board-policy--selector-keys
  '(:kind :activity-kind :to :author :subject-participant-id :attributes
    :tags :tags-all :tags-any)
  "JSON-shaped declarative selector keys admitted to routing policy.")

(defconst e-session-board-policy--node-budget 8192
  "Maximum structural nodes admitted by a board routing policy.

This is a domain budget for the declarative policy, not a nesting-depth cap.
It is deliberately independent of `e-board' so session replay can account for
the same bounded policy before encoding or mutation.  The representative
board policies are far below this ceiling.")

(defconst e-session-board-policy--byte-budget (* 64 1024)
  "Maximum UTF-8 bytes accounted for by a board routing policy.

The value follows the existing board metadata/attribute scale while keeping
the session admission boundary independent of the board implementation.")

(defconst e-session-board-policy--minimum-byte-budget 64
  "Smallest useful encoded budget for a complete routing policy.

Below this structural floor the policy can be rejected from the cheap
pre-encoding walk without invoking the codec's JSON escaping path.")

(defconst e-session-board-policy--categorical-byte-limit 128
  "Maximum UTF-8 bytes for one routing value that normalization interns.")

(defun e-session-board-policy--categorical-value-p (value)
  "Return non-nil when VALUE is a bounded string or symbol category."
  (and (or (stringp value) (symbolp value))
       (not (and (symbolp value) (keywordp value)))
       (<= (string-bytes (if (stringp value) value (symbol-name value)))
           e-session-board-policy--categorical-byte-limit)))

(defvar e-session-board-policy--budget-visit-count 0
  "Number of nodes visited by the most recent routing-policy budget walk.
This is an internal diagnostic hook used by bounded-admission tests; callers
must not use it as policy state.")

(defun e-session-board-policy--value-budget-valid-p
    (value &optional preserve-counter ignore-byte-budget)
  "Return non-nil when VALUE fits the routing-policy admission budget.

Account iteratively so hostile deep or cyclic values are rejected before
`json-encode' or session mutation.  In addition to string payloads, account
symbol names, numeric spellings, and a small canonical structural overhead.
This is a preflight estimate; the encoded policy receives an exact canonical
UTF-8 byte check after its reversible attribute encoding."
  (unless preserve-counter
    (setq e-session-board-policy--budget-visit-count 0))
  (let ((pending (list (list :value value)))
        (visiting (make-hash-table :test 'eq))
        (nodes 0)
        (bytes 0)
        (valid t))
    (while (and valid pending)
      (let ((task (pop pending)))
        (if (eq (car task) :leave)
            (remhash (cdr task) visiting)
          (let ((current (cadr task)))
            (setq nodes (1+ nodes))
            (cl-incf e-session-board-policy--budget-visit-count)
            (when (> nodes e-session-board-policy--node-budget)
              (setq valid nil))
            (cond
             ((stringp current)
              ;; Quotes are part of the JSON spelling; escapes are charged by
              ;; the exact post-encoding check below.
              (setq bytes (+ bytes 2 (string-bytes current))))
             ((numberp current)
              (setq bytes (+ bytes (string-bytes
                                    (number-to-string current)))))
             ((symbolp current)
              (setq bytes (+ bytes 2 (string-bytes (symbol-name current)))))
             ((null current)
              (setq bytes (+ bytes 4)))
             ((eq current t)
              (setq bytes (+ bytes 4)))
             ((or (vectorp current) (consp current))
              (when (gethash current visiting)
                (setq valid nil))
              (unless (gethash current visiting)
                (puthash current t visiting)
                (push (cons :leave current) pending)
                ;; Every container contributes delimiters.  Individual
                ;; separators are charged by each child below conservatively
                ;; through the node count and the final exact check.
                (setq bytes (+ bytes 2))
                (if (vectorp current)
                    (let* ((count (length current))
                           (remaining
                            (- e-session-board-policy--node-budget
                               nodes
                               (length pending))))
                      ;; Do not enqueue a caller-controlled vector wider than
                      ;; the remaining structural allowance.  Reject before
                      ;; allocating a task per element.
                      (if (> count remaining)
                          (setq valid nil)
                        (let ((index (1- count)))
                          (while (>= index 0)
                            (push (list :value (aref current index)) pending)
                            (setq index (1- index))))))
                  (push (list :value (cdr current)) pending)
                  (push (list :value (car current)) pending))))
             (t
              ;; Function objects, hash tables, buffers, markers, and other
              ;; process-local objects are not durable selector data.
              (setq valid nil)))
            (when (and (not ignore-byte-budget)
                       (> bytes e-session-board-policy--byte-budget))
              (setq valid nil))))))
    valid))

(defun e-session-board-policy--json-value-p (value &optional visiting)
  "Return non-nil when VALUE is a finite JSON-shaped Lisp value.
Functions, hash tables, and cyclic values are deliberately not durable board
policy.  VISITING is the active identity set used to reject cycles without
accepting an executable selector predicate by accident."
  (let ((visiting (or visiting (make-hash-table :test 'eq)))
        (leave-marker (make-symbol "routing-leave"))
        (pending (list value))
        (valid t))
    ;; Keep this admission walk iterative.  A structural budget is useful
    ;; only when a hostile but finite nested value cannot exhaust the Lisp
    ;; evaluator before the budget is consulted.
    (while (and valid pending)
      (let ((current (pop pending)))
        (if (and (consp current) (eq (car current) leave-marker))
            (remhash (cdr current) visiting)
          (cond
           ((or (null current) (eq current t) (numberp current)
                (stringp current)) nil)
           ;; Symbols are data in selectors, even when their names are also
           ;; callable functions.  Only executable objects/forms are rejected.
           ((and (symbolp current) (not (keywordp current))) nil)
           ((functionp current) (setq valid nil))
           ((and (consp current) (memq (car current) '(lambda function)))
            (setq valid nil))
           ((or (vectorp current) (consp current))
            (if (gethash current visiting)
                (setq valid nil)
              (puthash current t visiting)
              (push (cons leave-marker current) pending)
              (cond
               ((vectorp current)
                (let ((index (1- (length current))))
                  (while (>= index 0)
                    (push (aref current index) pending)
                    (setq index (1- index)))))
               ((e-session-board-policy--keyword-plist-p current)
                (let ((tail current))
                  (while tail
                    (pop tail)
                    (push (pop tail) pending))))
               ((proper-list-p current)
                (dolist (item (reverse current))
                  (push item pending)))
               ((or (keywordp (car current))
                    (stringp (car current)))
                (push (cdr current) pending))
               (t
                (setq valid nil)))))
           (t (setq valid nil))))))
    valid))

(defun e-session-board-policy--json-string-byte-size (value)
  "Return exact `json-encode' byte length for string VALUE without encoding it.

Multibyte Unicode, including U+2028 and U+2029, passes through unchanged.
JSON punctuation and controls use Emacs's short or six-byte escapes.  Raw
unibyte non-ASCII bytes use the double-backslash octal spelling emitted by
Emacs's legacy JSON encoder."
  (let ((bytes (+ 2 (string-bytes value)))
        (unibyte-p (not (multibyte-string-p value)))
        (index 0)
        (length (length value)))
    (while (< index length)
      (let ((character (aref value index)))
        (setq bytes
              (+ bytes
                 (cond
                  ((or (= character ?\") (= character ?\\)) 1)
                  ((memq character '(8 9 10 12 13)) 1)
                  ((< character 32) 5)
                  ((and unibyte-p (>= character 128)) 4)
                  (t 0)))))
      (setq index (1+ index)))
    bytes))

(defun e-session-board-policy--json-scalar-byte-size (value)
  "Return exact JSON bytes for scalar VALUE without a payload-sized string."
  (cond
   ((null value) 4)
   ((eq value t) 4)
   ((eq value json-false) 5)
   ((numberp value) (string-bytes (number-to-string value)))
   ((stringp value) (e-session-board-policy--json-string-byte-size value))
   ((symbolp value)
    (let ((bytes
           (e-session-board-policy--json-string-byte-size
            (symbol-name value))))
      ;; `json-encode' omits the leading colon of keyword spellings.
      (if (keywordp value) (1- bytes) bytes)))
   (t
    (signal 'e-session-board-routing-invalid
            (list "Unsupported routing scalar" value)))))

(defun e-session-board-policy--json-byte-size (value)
  "Return the canonical UTF-8 JSON byte size of finite VALUE.
Container traversal is iterative so a policy below the structural node budget
cannot overflow the Lisp evaluator merely while measuring its representation.
Scalar values use Emacs's canonical JSON escaping; unsupported dotted pairs
signal `e-session-board-routing-invalid'."
  (let ((pending (list (list :value value)))
        (visiting (make-hash-table :test 'eq))
        (leave-marker (make-symbol "routing-json-leave"))
        (bytes 0))
    (while pending
      (let ((task (pop pending)))
        (if (eq (car task) leave-marker)
            (remhash (cdr task) visiting)
          (let ((current (cadr task)))
            (cond
             ((or (null current) (eq current t) (numberp current)
                  (stringp current) (symbolp current))
              (setq bytes
                    (+ bytes
                       (e-session-board-policy--json-scalar-byte-size
                        current))))
             ((or (vectorp current) (consp current))
              (when (gethash current visiting)
                (signal 'e-session-board-routing-invalid
                        (list "Cyclic routing value" current)))
              (puthash current t visiting)
              (push (cons leave-marker current) pending)
              (cond
               ((vectorp current)
                (let ((count (length current))
                      (index (1- (length current))))
                  (setq bytes (+ bytes 2 (max 0 (1- count))))
                  (while (>= index 0)
                    (push (list :value (aref current index)) pending)
                    (setq index (1- index)))))
               ((e-session-board-policy--keyword-plist-p current)
                (let ((tail current)
                      (count 0))
                  (while tail
                    (setq count (1+ count))
                    (push (list :value (pop tail)) pending)
                    (push (list :value (pop tail)) pending))
                  (setq bytes (+ bytes 2 count (max 0 (1- count))))))
               ((proper-list-p current)
                (let ((count (length current)))
                  (setq bytes (+ bytes 2 (max 0 (1- count))))
                  (dolist (item (reverse current))
                    (push (list :value item) pending))))
               (t
                (signal 'e-session-board-routing-invalid
                        (list "Unsupported dotted routing value" current)))))
             (t
              (signal 'e-session-board-routing-invalid
                      (list "Unsupported routing value" current))))))))
    bytes))

(defun e-session-board-policy--json-value-valid-p (value &optional budgeted-p)
  "Return non-nil when VALUE is finite and encodable as JSON."
  (and (or budgeted-p
           (e-session-board-policy--value-budget-valid-p value))
       (e-session-board-policy--json-value-p value)
       (condition-case nil
           (progn
             ;; Measure the canonical spelling without encoding the whole
             ;; value.  Direct scalar counting retains exact Emacs escaping
             ;; while containers are traversed iteratively.
             (e-session-board-policy--json-byte-size value)
             t)
         (error nil))))

(defun e-session-board-policy--encoded-attribute-p (value)
  "Return non-nil when VALUE already has the durable attribute wrapper."
  (cond
   ((vectorp value)
    (and (= (length value) 2)
         (equal (aref value 0) e-session-codec--routing-attributes-tag)))
   ((consp value)
    (and (consp (cdr value))
         (null (cddr value))
         (equal (car value) e-session-codec--routing-attributes-tag)))))

(defun e-session-board-policy--wire-attribute-json-byte-size (value)
  "Return exact JSON bytes of VALUE's virtual durable attribute encoding.

This follows `e-session-codec--wire-attribute-value' without constructing its
payload-sized vector tree.  The caller has already established the routing
node bound, but the active identity set still rejects an invariant cycle."
  (let ((pending (list (list :value value)))
        (visiting (make-hash-table :test 'eq))
        (leave-marker (make-symbol "routing-wire-leave"))
        (bytes 0))
    (while pending
      (let ((task (pop pending)))
        (if (eq (car task) leave-marker)
            (remhash (cdr task) visiting)
          (let ((current (cadr task)))
            (cond
             ((or (null current) (eq current t) (numberp current)
                  (stringp current))
              (setq bytes
                    (+ bytes
                       (e-session-board-policy--json-scalar-byte-size
                        current))))
             ((symbolp current)
              ;; ["symbol", SYMBOL-NAME]
              (setq bytes
                    (+ bytes 3
                       (e-session-board-policy--json-string-byte-size "symbol")
                       (e-session-board-policy--json-string-byte-size
                        (symbol-name current)))))
             ((or (vectorp current) (consp current))
              (when (gethash current visiting)
                (signal 'e-session-board-routing-invalid
                        (list "Cyclic routing attribute" current)))
              (puthash current t visiting)
              (push (cons leave-marker current) pending)
              (cond
               ((vectorp current)
                ;; ["vector", [ITEM...]]
                (let ((count (length current)))
                  (setq bytes
                        (+ bytes 5
                           (e-session-board-policy--json-string-byte-size
                            "vector")
                           (max 0 (1- count))))
                  (dotimes (index count)
                    (push (list :value (aref current index)) pending))))
               ((e-session-board-policy--keyword-plist-p current)
                ;; ["plist", [[KEY, VALUE]...]]
                (let ((tail current)
                      (count 0))
                  (while tail
                    (let ((key (pop tail))
                          (entry (pop tail)))
                      (setq count (1+ count)
                            bytes (+ bytes 3
                                     (e-session-board-policy--json-scalar-byte-size
                                      key)))
                      (push (list :value entry) pending)))
                  (setq bytes
                        (+ bytes 5
                           (e-session-board-policy--json-string-byte-size
                            "plist")
                           (max 0 (1- count))))))
               ((proper-list-p current)
                ;; ["list", [ITEM...]]
                (let ((tail current)
                      (count 0))
                  (while tail
                    (setq count (1+ count))
                    (push (list :value (pop tail)) pending))
                  (setq bytes
                        (+ bytes 5
                           (e-session-board-policy--json-string-byte-size
                            "list")
                           (max 0 (1- count))))))
               (t
                ;; ["cons", [CAR, CDR]]
                (setq bytes
                      (+ bytes 6
                         (e-session-board-policy--json-string-byte-size
                          "cons")))
                (push (list :value (car current)) pending)
                (push (list :value (cdr current)) pending))))
             (t
              (signal 'e-session-board-routing-invalid
                      (list "Unsupported routing attribute" current))))))))
    bytes))

(defun e-session-board-policy--wire-selector-json-byte-size (selector)
  "Return exact durable JSON bytes for already-validated SELECTOR."
  (if (null selector)
      (e-session-board-policy--json-byte-size nil)
    (let ((tail selector)
          (count 0)
          (bytes 2))
      (while tail
        (let ((key (pop tail))
              (value (pop tail)))
          (setq count (1+ count)
                bytes (+ bytes 1
                         (e-session-board-policy--json-scalar-byte-size key)))
          (setq bytes
                (+ bytes
                   (if (and (eq key :attributes)
                            (not (e-session-board-policy--encoded-attribute-p
                                  value)))
                       ;; [ROUTING-TAG, WIRE-ATTRIBUTE]
                       (+ 3
                          (e-session-board-policy--json-string-byte-size
                           e-session-codec--routing-attributes-tag)
                          (e-session-board-policy--wire-attribute-json-byte-size
                           value))
                     (e-session-board-policy--json-byte-size value))))))
      (+ bytes (max 0 (1- count))))))

(defun e-session-board-policy-wire-json-byte-size (policy)
  "Return exact durable JSON bytes for validated routing POLICY.

The measurement walks caller state and accounts for the codec's tagged
selector attributes without constructing a canonical policy copy."
  (let ((tail policy)
        (count 0)
        (bytes 2))
    (while tail
      (let ((key (pop tail))
            (value (pop tail)))
        (setq count (1+ count)
              bytes (+ bytes 1
                       (e-session-board-policy--json-scalar-byte-size key)))
        (setq bytes
              (+ bytes
                 (if (memq key '(:pickup-selector :observer-selector))
                     (e-session-board-policy--wire-selector-json-byte-size
                      value)
                   (e-session-board-policy--json-byte-size value))))))
    (+ bytes (max 0 (1- count)))))

(defun e-session-board-policy--tag-list-valid-p (value)
  "Return non-nil when VALUE is a list of declarative tag atoms."
  (and (proper-list-p value)
       (cl-every #'e-session-board-policy--categorical-value-p value)))

(defun e-session-board-policy--selector-valid-p
    (selector &optional budgeted-p)
  "Return non-nil when SELECTOR is declarative and JSON-shaped."
  (and (or budgeted-p
           (e-session-board-policy--value-budget-valid-p selector))
       (e-session-board-policy--keyword-plist-p selector)
       (let ((tail selector)
             seen
             (valid t))
         (while (and valid tail)
          (let ((key (pop tail))
                (value (pop tail)))
             (setq valid
                   (and (memq key e-session-board-policy--selector-keys)
                        (not (memq key seen))
                        (cond
                         ((memq key '(:tags :tags-all :tags-any))
                         (e-session-board-policy--tag-list-valid-p value))
                         ((memq key '(:kind :activity-kind))
                          (e-session-board-policy--categorical-value-p value))
                         ((memq key '(:to :author :subject-participant-id))
                          (stringp value))
                         ((eq key :attributes)
                          (and (e-board-selector-attributes-valid-p value)
                               (e-session-board-policy--json-value-valid-p
                                value t)))
                         (t nil))))
             (push key seen)))
         valid)))

(defun e-session-board-routing-policy-valid-p (policy)
  "Return non-nil when POLICY has exactly the complete durable shape."
  ;; Budget the complete caller value before any plist, selector, tag, or
  ;; attribute grammar walk.  This is the admission cutoff for rejected input
  ;; as well as accepted policy.
  (and (>= e-session-board-policy--byte-budget
           e-session-board-policy--minimum-byte-budget)
       (e-session-board-policy--value-budget-valid-p
        policy nil t)
       (e-session-board-policy--keyword-plist-p policy)
       (let ((tail policy)
             seen
             (valid t))
         (while (and valid tail)
           (let ((key (pop tail))
                 (value (pop tail)))
             (setq valid
                   (and (memq key e-session-board-policy--keys)
                        (not (memq key seen))
                        (cond
                         ((eq key :participant-id)
                          (and (stringp value)
                               (not (string-empty-p value))))
                         ((memq key '(:pickup-selector :observer-selector))
                          (e-session-board-policy--selector-valid-p
                           value t))
                         ((eq key :default-tags)
                          (e-session-board-policy--tag-list-valid-p value))
                         ((eq key :default-to)
                          (or (null value) (stringp value)))
                         (t nil))))
             (push key seen)))
         (and valid
              (= (length seen) (length e-session-board-policy--keys))
              (e-session-board-policy--json-value-p policy)
              ;; Attribute selectors are tagged reversibly for persistence;
              ;; measure that exact virtual wire form without materializing a
              ;; second caller-sized policy before admission.
              (condition-case nil
                  (<=
                   (e-session-board-policy-wire-json-byte-size policy)
                   e-session-board-policy--byte-budget)
                (error nil))))))

(defun e-session-board-policy--normalize-selector (selector)
  "Return SELECTOR in the in-memory symbol form used by board matchers."
  (let ((selector (e-session-board-routing-policy-copy-value selector)))
    (dolist (key '(:kind :activity-kind))
      (when (stringp (plist-get selector key))
        (plist-put selector key (intern (plist-get selector key)))))
    (dolist (key '(:tags :tags-all :tags-any))
      (when (plist-member selector key)
        (plist-put selector key
                   (mapcar (lambda (tag)
                             (if (stringp tag) (intern tag) tag))
                           (plist-get selector key)))))
      selector))

(defun e-session-board-policy--normalize-owned-selector (selector)
  "Normalize frozen SELECTOR while copying only its bounded schema spines."
  (when selector
    (let ((selector (copy-sequence selector)))
      (dolist (key '(:kind :activity-kind))
        (when (stringp (plist-get selector key))
          (plist-put selector key (intern (plist-get selector key)))))
      (dolist (key '(:tags :tags-all :tags-any))
        (when (plist-member selector key)
          (plist-put selector key
                     (mapcar (lambda (tag)
                               (if (stringp tag) (intern tag) tag))
                             (plist-get selector key)))))
      selector)))

(defun e-session-board-routing-policy-copy-value (value)
  "Deep-copy JSON-shaped board routing VALUE, including strings.
Use an explicit task stack so an admitted finite policy does not consume the
Lisp call stack merely while detaching nested selector data.  Cycles signal the
same invalid-policy condition as the admission walk."
  (let ((pending (list (list :value value)))
        (results nil)
        (visiting (make-hash-table :test 'eq)))
    (while pending
      (let ((task (pop pending)))
        (pcase (car task)
          (:leave
           (remhash (cadr task) visiting))
          (:assemble-vector
           (let (items)
             (dotimes (_ (cadr task))
               (push (pop results) items))
             (push (vconcat items) results)))
          (:assemble-cons
           (let ((cdr-value (pop results))
                 (car-value (pop results)))
             (push (cons car-value cdr-value) results)))
          (:value
           (let ((current (cadr task)))
             (cond
              ((stringp current)
               (push (copy-sequence current) results))
              ((or (null current) (eq current t) (numberp current)
                   (symbolp current))
               (push current results))
              ((or (vectorp current) (consp current))
               (when (gethash current visiting)
                 (signal 'e-session-board-routing-invalid
                         (list "Cyclic routing value" current)))
               (puthash current t visiting)
               (push (list :leave current) pending)
               (push (list :assemble-vector (length current)) pending)
               (if (vectorp current)
                   (let ((index (1- (length current))))
                     (while (>= index 0)
                       (push (list :value (aref current index)) pending)
                       (setq index (1- index))))
                 ;; A cons is always copied as its car/cdr pair.  This avoids
                 ;; calling `proper-list-p' while traversing a deep value.
                 (pop pending)
                 (push (list :assemble-cons) pending)
                 (push (list :value (cdr current)) pending)
                 (push (list :value (car current)) pending)))
              (t
              (signal 'e-session-board-routing-invalid
                       (list "Unsupported routing value" current)))))))))
    (car results)))

(defun e-session-board-routing-policy-normalize (policy)
  "Return detached POLICY with replayed tag/kind values normalized."
  (when policy
    (let ((policy (e-session-board-routing-policy-copy-value policy)))
      (dolist (key '(:pickup-selector :observer-selector))
        (plist-put policy key
                   (e-session-board-policy--normalize-selector
                    (plist-get policy key))))
      (when (plist-member policy :default-tags)
        (plist-put policy :default-tags
                   (mapcar (lambda (tag)
                             (if (stringp tag) (intern tag) tag))
                           (plist-get policy :default-tags))))
      policy)))

(defun e-session-board-routing-policy-normalize-owned (policy)
  "Normalize frozen POLICY without copying its variable producer leaves.

POLICY must already have passed `e-session-board-routing-policy-valid-p'.
Only the fixed policy and selector plists plus bounded tag-list spines are
copied.  Identity strings and selector attribute graphs remain shared by
identity with the sealed producer graph."
  (when policy
    (let ((policy (copy-sequence policy)))
      (dolist (key '(:pickup-selector :observer-selector))
        (plist-put policy key
                   (e-session-board-policy--normalize-owned-selector
                    (plist-get policy key))))
      (when (plist-member policy :default-tags)
        (plist-put policy :default-tags
                   (mapcar (lambda (tag)
                             (if (stringp tag) (intern tag) tag))
                           (plist-get policy :default-tags))))
      policy)))


(provide 'e-session-board-policy)

;;; e-session-board-policy.el ends here
