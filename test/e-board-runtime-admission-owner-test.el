;;; e-board-runtime-admission-owner-test.el --- Runtime admission owner tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Direct contract tests for the runtime-specific admission lifetime index.
;; Board mutation remains covered by the Board composition suites; this file
;; checks only exact attachment/Board ownership, fencing, and ordinary cleanup.

;;; Code:

(require 'ert)
(require 'e-board-runtime-admission)
(require 'e-board-state)

(defun e-board-runtime-admission-owner-test--board (id)
  "Return the smallest Board value needed by the lower admission contract."
  (e-board-state-create
   :id id
   :next-seq 0
   :event-message-count (make-hash-table :test 'eql)
   :event-message-prefix-high-watermark 0
   :event-node-index (make-hash-table :test 'eq)
   :pending-admissions (make-hash-table :test 'eq)))

(defun e-board-runtime-admission-owner-test--handle (id)
  "Return a minimal prepared Work handle for ID."
  (e-work-prepare
   (e-work-spec-create
    :id id :execution 'cheap :interactive-policy 'cheap
    :runner (lambda (_arguments _context) :done))
   nil))

(defmacro e-board-runtime-admission-owner-test--isolated (&rest body)
  "Run BODY with fresh runtime-admission indexes and no runtime facade."
  (declare (indent 0) (debug t))
  `(let ((e-board-runtime-admission--pending-admissions
          (make-hash-table :test 'eq))
         (e-board-runtime-admission--pending-by-board
          (make-hash-table :test 'eq))
         (e-board-runtime-admission--pending-by-attachment
          (make-hash-table :test 'eq)))
     ,@body))

(defun e-board-runtime-admission-owner-test--counts ()
  "Return the primary, Board, and attachment index sizes."
  (list (hash-table-count e-board-runtime-admission--pending-admissions)
        (hash-table-count e-board-runtime-admission--pending-by-board)
        (hash-table-count e-board-runtime-admission--pending-by-attachment)))

(ert-deftest e-board-runtime-admission-owner-has-no-upward-runtime-load ()
  "The direct owner has no dependency on the runtime composition facade."
  (let ((source (locate-library "e-board-runtime-admission")))
    (should source)
    (with-temp-buffer
      (insert-file-contents source)
      (should-not
       (re-search-forward
        "(require[[:space:]]+'e-board-runtime[[:space:]])" nil t)))))

(ert-deftest e-board-runtime-admission-owner-error-contract-is-standalone ()
  "A bare runtime-admission owner exposes its stable typed error contract."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board "owner-error"))
           (replacement (e-board-runtime-admission-owner-test--board
                         "owner-error-replacement"))
           (attachment (list 'attachment 'error))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "error-work")))
           typed generic)
      (e-board-runtime-admission-remember attachment board admission nil 1)
      (condition-case err
          (e-board-runtime-admission-remember attachment replacement admission nil 2)
        (e-board-runtime-error
         (setq typed err
               generic (memq 'error
                             (get 'e-board-runtime-error 'error-conditions))))
        (error (setq generic err)))
      (should typed)
      (should generic)
      (e-board-runtime-admission-complete admission)
      (should (equal (e-board-runtime-admission-owner-test--counts)
                     '(0 0 0))))))

(ert-deftest e-board-runtime-admission-owner-remembers-exact-lifetime ()
  "One admission is indexed by its exact Board and attachment objects."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board "owner-index"))
           (attachment (list 'attachment 1))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "owner-work"))))
      (let ((record
             (e-board-runtime-admission-remember
              attachment board admission t 7)))
        (should (eq record (gethash admission
                                    e-board-runtime-admission--pending-admissions)))
        (should (eq record
                    (gethash admission
                             (gethash board
                                      e-board-runtime-admission--pending-by-board))))
        (should (eq record
                    (gethash admission
                             (gethash attachment
                                      e-board-runtime-admission--pending-by-attachment))))
        (should (= (e-board-runtime-admission-record-generation record) 7)))
      (e-board-runtime-admission-finish admission)
      (e-board-runtime-admission-complete admission)
      (should (equal (e-board-runtime-admission-owner-test--counts)
                     '(0 0 0))))))

(ert-deftest e-board-runtime-admission-owner-fences-only-exact-attachment ()
  "Attachment fencing leaves a same-Board sibling lifetime untouched."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board "owner-fence"))
           (first-attachment (list 'attachment 'first))
           (second-attachment (list 'attachment 'second))
           (first (e-board-admission-work-token
                   (e-board-runtime-admission-owner-test--handle "first")))
           (second (e-board-admission-work-token
                    (e-board-runtime-admission-owner-test--handle "second"))))
      (e-board-runtime-admission-remember first-attachment board first t 1)
      (e-board-runtime-admission-remember second-attachment board second t 2)
      (e-board-runtime-admission-fence first-attachment board)
      (let ((first-record (gethash first e-board-runtime-admission--pending-admissions))
            (second-record (gethash second e-board-runtime-admission--pending-admissions)))
        (should (e-board-runtime-admission-record-cancel-requested-p first-record))
        (should-not (e-board-runtime-admission-record-cancel-requested-p
                     second-record)))
      (e-board-runtime-admission-finish first)
      (e-board-runtime-admission-finish second)
      (e-board-runtime-admission-abort board first)
      (e-board-runtime-admission-abort board second)
      (should (equal (e-board-runtime-admission-owner-test--counts)
                     '(0 0 0))))))

(ert-deftest e-board-runtime-admission-owner-board-retry-is-explicit ()
  "An explicit Board retry handles all of that Board's staged lifetimes."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board "owner-retry"))
           (attachment (list 'attachment 'retry))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "retry-work"))))
      (e-board-runtime-admission-remember attachment board admission nil 3)
      (should (= (length (e-board-runtime-admission-retry nil board)) 1))
      (should (equal (e-board-runtime-admission-owner-test--counts)
                     '(0 0 0))))))

(ert-deftest e-board-runtime-admission-owner-equal-identities-do-not-transfer ()
  "An equal-looking replacement attachment never inherits an old record."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board "owner-replace"))
           (old-attachment (list 'attachment "same"))
           (new-attachment (list 'attachment "same"))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "replace-work"))))
      (e-board-runtime-admission-remember old-attachment board admission nil 4)
      (should-not (gethash new-attachment
                           e-board-runtime-admission--pending-by-attachment))
      (e-board-runtime-admission-abort board admission)
      (should (equal (e-board-runtime-admission-owner-test--counts)
                     '(0 0 0))))))

(ert-deftest e-board-runtime-admission-owner-preserves-replacement-bucket ()
  "Cleanup of an old record never removes a replacement object-local bucket."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board "owner-replace"))
           (attachment (list 'attachment 'old))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "replace-work")))
           (replacement (make-hash-table :test 'eq)))
      (e-board-runtime-admission-remember attachment board admission nil 4)
      (puthash attachment replacement
               e-board-runtime-admission--pending-by-attachment)
      (e-board-runtime-admission-complete admission)
      (should (eq (gethash attachment
                            e-board-runtime-admission--pending-by-attachment)
                  replacement))
      (should-not (gethash admission replacement))
      (should (= (hash-table-count e-board-runtime-admission--pending-admissions)
                 0)))))

(provide 'e-board-runtime-admission-owner-test)

;;; e-board-runtime-admission-owner-test.el ends here
