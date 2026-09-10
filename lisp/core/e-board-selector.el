;;; e-board-selector.el --- Pure declarative Board selector grammar -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Validate and normalize the data-only portion of Board selectors.  This
;; module owns no Board identity, messages, subscriptions, or runtime state, so
;; session admission and the SQLite Board service can depend on the grammar
;; without depending on the retired in-memory engine.

;;; Code:

(require 'cl-lib)

(defun e-board-selector--proper-list-p (value)
  "Return non-nil when VALUE is a finite proper list."
  (let ((tail value)
        (seen (make-hash-table :test 'eq))
        (valid t))
    (while (and valid (consp tail))
      (if (gethash tail seen)
          (setq valid nil)
        (puthash tail t seen)
        (setq tail (cdr tail))))
    (and valid (null tail))))

(defun e-board-selector--attribute-value-valid-p (value)
  "Return non-nil when nested selector attribute VALUE is reversible data."
  (let ((pending (list (list :value value)))
        (visiting (make-hash-table :test 'eq))
        (leave-marker (make-symbol "board-selector-attribute-leave"))
        (valid t))
    (while (and valid pending)
      (let ((task (pop pending)))
        (if (eq (car task) leave-marker)
            (remhash (cdr task) visiting)
          (let ((current (cadr task)))
            (cond
             ((or (null current) (eq current t) (numberp current)
                  (stringp current) (symbolp current)) nil)
             ((functionp current)
              (setq valid nil))
             ((and (consp current)
                   (memq (car current) '(lambda function)))
              (setq valid nil))
             ((or (vectorp current) (consp current))
              (if (gethash current visiting)
                  (setq valid nil)
                (puthash current t visiting)
                (push (cons leave-marker current) pending)
                (if (vectorp current)
                    (let ((index (1- (length current))))
                      (while (>= index 0)
                        (push (list :value (aref current index)) pending)
                        (setq index (1- index))))
                  (push (list :value (cdr current)) pending)
                  (push (list :value (car current)) pending))))
             (t
              (setq valid nil)))))))
    valid))

(defun e-board-selector-attributes-valid-p (attributes)
  "Return non-nil when ATTRIBUTES has the declarative selector grammar.
The top level is nil, an even keyword plist, or a proper alist of keyword
key/value conses.  Nested values may contain only finite reversible data."
  (cond
   ((null attributes) t)
   ((not (e-board-selector--proper-list-p attributes)) nil)
   ((keywordp (car attributes))
    (let ((tail attributes)
          seen
          (valid t))
      (while (and valid tail)
        (if (not (consp (cdr tail)))
            (setq valid nil)
          (let ((key (pop tail))
                (value (pop tail)))
            (setq valid
                  (and (keywordp key)
                       (not (memq key seen))
                       (e-board-selector--attribute-value-valid-p value)))
            (push key seen))))
      valid))
   (t
    (cl-every
     (lambda (pair)
       (and (consp pair)
            (keywordp (car pair))
            (e-board-selector--attribute-value-valid-p (cdr pair))))
     attributes))))

(defun e-board-selector-attribute-clauses (attributes)
  "Return ATTRIBUTES as canonical key/value conses after validation."
  (unless (e-board-selector-attributes-valid-p attributes)
    (signal 'wrong-type-argument (list 'board-selector-attributes attributes)))
  (cond
   ((null attributes) nil)
   ((keywordp (car attributes))
    (let (clauses)
      (while attributes
        (let ((key (pop attributes))
              (value (pop attributes)))
          (push (cons key value) clauses)))
      (nreverse clauses)))
   (t
    (mapcar (lambda (pair)
              (cons (car pair) (cdr pair)))
            attributes))))

(provide 'e-board-selector)

;;; e-board-selector.el ends here
