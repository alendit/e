;;; e-board-runtime-admission-owner-test.el --- Direct runtime admission owner tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests exercise the concrete runtime-admission catalog owner without
;; loading `e-board-runtime'.  The latter remains the composition suite's
;; responsibility; this file checks exact catalog identity, partial
;; publication recovery, and replacement fencing at the owner boundary.

;;; Code:

(require 'ert)
(require 'e-board-runtime-admission)
(require 'e-board-state)

(defun e-board-runtime-admission-owner-test--board (id)
  "Return the smallest Board value needed by the admission owner.

The owner contract must be runnable without the Board policy facade.  Runtime
catalog tests only need an exact pending-admission table and the lower Board
admission completion contract, so do not load `e-board' merely for a fixture."
  (e-board-state-create
   :id id
   :next-seq 0
   :event-message-count (make-hash-table :test 'eql)
   :event-message-prefix-high-watermark 0
   :event-node-index (make-hash-table :test 'eq)
   :pending-admissions (make-hash-table :test 'eq)))

(defun e-board-runtime-admission-owner-test--handle (id)
  "Return a minimal prepared Work HANDLE for ID."
  (e-work-prepare
   (e-work-spec-create
    :id id :execution 'cheap :interactive-policy 'cheap
    :runner (lambda (_arguments _context) :done))
   nil))

(defmacro e-board-runtime-admission-owner-test--isolated (&rest body)
  "Run BODY with fresh runtime-admission catalogs and no runtime facade."
  (declare (indent 0) (debug t))
  `(let ((e-board-runtime-admission--pending-admissions
          (make-hash-table :test 'eq))
         (e-board-runtime-admission--pending-by-board
          (make-hash-table :test 'eq))
         (e-board-runtime-admission--pending-by-attachment
          (make-hash-table :test 'eq))
         (e-board-runtime-admission--recovery
          (make-hash-table :test 'eq))
         (e-board-runtime-admission--recovery-by-board
          (make-hash-table :test 'eq))
         (e-board-runtime-admission--recovery-by-attachment
          (make-hash-table :test 'eq))
         (e-board--registry (make-hash-table :test 'equal)))
     ,@body))

(defun e-board-runtime-admission-owner-test--catalog-counts ()
  "Return the six runtime catalog sizes in stable owner order."
  (list (hash-table-count e-board-runtime-admission--pending-admissions)
        (hash-table-count e-board-runtime-admission--pending-by-board)
        (hash-table-count e-board-runtime-admission--pending-by-attachment)
        (hash-table-count e-board-runtime-admission--recovery)
        (hash-table-count e-board-runtime-admission--recovery-by-board)
        (hash-table-count e-board-runtime-admission--recovery-by-attachment)))

(ert-deftest e-board-runtime-admission-owner-has-no-upward-runtime-load ()
  "The direct owner source has no upward runtime composition dependency.

The test suite can be discovered together with the runtime composition suites,
so checking the global feature list would make this contract depend on test
order.  Inspect the loaded owner's literal imports instead; the separate
fresh-load gate exercises evaluation in a clean Emacs process."
  (let ((source (locate-library "e-board-runtime-admission")))
    (should source)
    (with-temp-buffer
      (insert-file-contents source)
      (should-not
       (re-search-forward
        "(require[[:space:]]+'e-board-runtime[[:space:]])" nil t)))))

(ert-deftest e-board-runtime-admission-owner-error-contract-is-standalone ()
  "A bare runtime-admission owner exposes a typed error below the facade.

The owner must classify its in-flight retry condition as both the concrete
runtime error and an ordinary `error', without loading `e-board-runtime'."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board "owner-error"))
           (attachment (list 'attachment 'error))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "error-work")))
           typed generic)
      (e-board-runtime-admission-remember attachment board admission t 1)
      (condition-case err
          (e-board-runtime-admission-retry attachment board)
        (e-board-runtime-error
         (setq typed err
               generic (memq 'error
                             (get 'e-board-runtime-error 'error-conditions))))
        (error (setq generic err)))
      (should typed)
      (should generic)
      (setf (e-board-runtime-admission-record-in-flight-p
             (gethash admission e-board-runtime-admission--pending-admissions))
            nil)
      (e-board-runtime-admission-complete admission)
      (should (equal (e-board-runtime-admission-owner-test--catalog-counts)
                     '(0 0 0 0 0 0))))))

(ert-deftest e-board-runtime-admission-owner-publishes-and-removes-exact-six-catalogs ()
  "One admission owns all primary and secondary catalog memberships."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board "owner-catalog"))
           (attachment (list 'attachment 1))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "owner-work"))))
      (e-board-runtime-admission-remember attachment board admission t 9)
      (should (equal (e-board-runtime-admission-owner-test--catalog-counts)
                     '(1 1 1 1 1 1)))
      (let ((record (gethash admission e-board-runtime-admission--pending-admissions))
            (board-bucket
             (gethash board e-board-runtime-admission--pending-by-board))
            (attachment-bucket
             (gethash attachment e-board-runtime-admission--pending-by-attachment)))
        (should record)
        (should (eq (gethash admission board-bucket) record))
        (should (eq (gethash admission attachment-bucket) record))
        (should (eq (gethash admission e-board-runtime-admission--recovery)
                    record)))
      (e-board-runtime-admission-complete admission)
      (should (equal (e-board-runtime-admission-owner-test--catalog-counts)
                     '(0 0 0 0 0 0))))))

(ert-deftest e-board-runtime-admission-owner-repairs-postmutation-bucket-publication ()
  "A bucket recorded before a signalling publication remains recoverable."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board "owner-partial"))
           (attachment (list 'attachment 2))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "partial-work")))
           (table e-board-runtime-admission--pending-by-attachment)
           (original-puthash (symbol-function 'puthash))
           condition)
      (cl-letf (((symbol-function 'puthash)
                 (lambda (key value destination)
                   (if (and (eq destination table)
                            (eq key attachment))
                       (prog1 (funcall original-puthash key value destination)
                         (error "attachment bucket publication after mutation"))
                     (funcall original-puthash key value destination)))))
        (setq condition
              (condition-case err
                  (progn
                    (e-board-runtime-admission-remember
                     attachment board admission t 10)
                    nil)
                (error err))))
      (should condition)
      ;; The source admission, not the failed puthash call, remains the
      ;; authority for the exact bucket and all already-published scopes.
      (should (gethash admission e-board-runtime-admission--recovery))
      (e-board-runtime-admission-complete admission)
      (should (equal (e-board-runtime-admission-owner-test--catalog-counts)
                     '(0 0 0 0 0 0))))))

(ert-deftest e-board-runtime-admission-owner-cleans-each-scope-before-or-after-publication ()
  "Every ordinary/recovery Board/attachment bucket is retryable at both edges."
  (e-board-runtime-admission-owner-test--isolated
    (dolist (failure-mode '(before after))
      (dolist (scope '(recovery-board recovery-attachment board attachment))
        (let* ((board (e-board-runtime-admission-owner-test--board
                       (format "owner-%s-%s" scope failure-mode)))
               (attachment (list 'attachment scope failure-mode))
               (admission
                (e-board-admission-work-token
                 (e-board-runtime-admission-owner-test--handle
                  (format "scope-%s-%s" scope failure-mode))))
               (table
                (pcase scope
                  ('recovery-board
                   e-board-runtime-admission--recovery-by-board)
                  ('recovery-attachment
                   e-board-runtime-admission--recovery-by-attachment)
                  ('board e-board-runtime-admission--pending-by-board)
                  ('attachment e-board-runtime-admission--pending-by-attachment)))
               (target-key (if (memq scope '(recovery-board board))
                               board attachment))
               (original-puthash (symbol-function 'puthash))
               signalled)
          (cl-letf (((symbol-function 'puthash)
                     (lambda (key value destination)
                       (if (and (not signalled)
                                (eq key target-key)
                                (eq destination table))
                           (progn
                             (when (eq failure-mode 'after)
                               (funcall original-puthash key value destination))
                             (setq signalled t)
                             (error "scope publication fault"))
                         (funcall original-puthash key value destination)))))
            (should-error
             (e-board-runtime-admission-remember
              attachment board admission t 13)))
          ;; Completion can use the bucket recorded before publication even
          ;; when the outer table never received a member.
          (e-board-runtime-admission-complete admission)
          (should (equal (e-board-runtime-admission-owner-test--catalog-counts)
                         '(0 0 0 0 0 0))))))))

(ert-deftest e-board-runtime-admission-owner-preserves-replacement-bucket ()
  "Cleanup of an old record never removes a replacement outer bucket."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board "owner-replacement"))
           (attachment (list 'attachment 3))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "replacement-work")))
           (replacement (make-hash-table :test 'eq)))
      (e-board-runtime-admission-remember attachment board admission t 11)
      (puthash board replacement e-board-runtime-admission--pending-by-board)
      (e-board-runtime-admission-complete admission)
      (should (eq (gethash board e-board-runtime-admission--pending-by-board)
                  replacement))
      (should-not (gethash admission replacement))
      ;; The old record is otherwise fully acknowledged; only the externally
      ;; replaced bucket survives, as required by exact identity fencing.
      (should (= (hash-table-count e-board-runtime-admission--pending-admissions)
                 0)))))

(ert-deftest e-board-runtime-admission-owner-primary-only-recovery-is-retryable ()
  "A primary-only recovery record keeps an exact retry handle across two faults."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board
                   "owner-primary-only"))
           (attachment (list 'attachment 'primary-only))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "primary-work")))
           (original-puthash (symbol-function 'puthash))
           (original-remhash (symbol-function 'remhash))
           publication-fault removal-fault)
      ;; Stop publication before the first secondary index.  The primary and
      ;; admission-local record handle are nevertheless already authoritative.
      (cl-letf (((symbol-function 'puthash)
                 (lambda (key value table)
                   (if (and (eq table e-board-runtime-admission--recovery-by-board)
                            (eq key board))
                       (progn
                         (setq publication-fault t)
                         (error "recovery board before"))
                     (funcall original-puthash key value table)))))
        (should-error
         (e-board-runtime-admission-remember
          attachment board admission t 4)))
      (should publication-fault)
      (should (gethash admission e-board-runtime-admission--recovery))
      ;; A pre-mutation primary removal fault must leave the exact recovery
      ;; record discoverable through the admission object, not a global scan.
      (cl-letf (((symbol-function 'remhash)
                 (lambda (key table)
                   (if (and (eq table e-board-runtime-admission--recovery)
                            (eq key admission)
                            (not removal-fault))
                       (progn
                         (setq removal-fault t)
                         (error "recovery primary before"))
                     (funcall original-remhash key table)))))
        (should-error (e-board-runtime-admission-complete admission)))
      (should removal-fault)
      (should (gethash admission e-board-runtime-admission--recovery))
      (e-board-runtime-admission-complete admission)
      (should (equal (e-board-runtime-admission-owner-test--catalog-counts)
                     '(0 0 0 0 0 0))))))

(ert-deftest e-board-runtime-admission-owner-final-removal-postcondition-wins ()
  "A recovery remhash signal after exact removal is an acknowledged inverse."
  (e-board-runtime-admission-owner-test--isolated
    (let* ((board (e-board-runtime-admission-owner-test--board
                   "owner-final-postcondition"))
           (attachment (list 'attachment 'final))
           (admission
            (e-board-admission-work-token
             (e-board-runtime-admission-owner-test--handle "final-work")))
           (original-remhash (symbol-function 'remhash))
           signalled)
      (e-board-runtime-admission-remember attachment board admission nil 5)
      (cl-letf (((symbol-function 'remhash)
                 (lambda (key table)
                   (prog1 (funcall original-remhash key table)
                     (when (and (eq table e-board-runtime-admission--recovery)
                                (eq key admission)
                                (not signalled))
                       (setq signalled t)
                       (error "recovery primary after"))))))
        (e-board-runtime-admission-complete admission))
      (should signalled)
      (should (equal (e-board-runtime-admission-owner-test--catalog-counts)
                     '(0 0 0 0 0 0))))))

(provide 'e-board-runtime-admission-owner-test)

;;; e-board-runtime-admission-owner-test.el ends here
