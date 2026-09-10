;;; e-cron-actions-test.el --- Board-bound cron action tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-cron)
(require 'e-cron-actions)
(require 'e-cron-storage-sqlite)
(load (expand-file-name "e-board-producer-test-support.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(cl-defmacro e-cron-actions-test--with-board ((target) &body body)
  "Run BODY with isolated cron state and disposable SQL TARGET."
  (declare (indent 1) (debug ((symbolp) body)))
  `(let ((e-cron--schedules (make-hash-table :test 'equal))
         (e-cron--state (make-hash-table :test 'equal))
         (e-cron--state-loaded t)
         (e-cron--active-firings (make-hash-table :test 'equal))
         (e-cron-storage nil)
         (e-cron-state-file nil)
         (e-cron-actions-publication-target nil))
     (e-board-producer-test-with-target (,target)
       ,@body)))

(defun e-cron-actions-test--wait-for (predicate &optional timeout)
  "Wait at this explicit test boundary until PREDICATE succeeds."
  (let ((deadline (+ (float-time) (or timeout 5.0))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(ert-deftest e-cron-actions-test-register-requires-sql-target ()
  "Schedule registration fails visibly without a SQLite target."
  (let ((e-cron-actions-publication-target nil))
    (should-error
     (e-cron-actions-register
      :id 'missing :when '(:every 60)
      :action '(:publish (:content "tick")))
     :type 'wrong-type-argument)))

(ert-deftest e-cron-actions-test-fire-publishes-fact ()
  "A cron fire queues and publishes exactly one descriptive board fact."
  (e-cron-actions-test--with-board (target)
    (let ((schedule
           (e-cron-actions-register
            :id 'refresh :when '(:every 60) :publication-target target
            :action '(:publish (:content "refresh" :tags (maintenance)
                                :attributes (:scope sources))))))
      (e-cron-fire schedule)
      (let ((record (car (e-board-producer-test-records target))))
        (should (equal (plist-get record :content) "refresh"))
        (should (equal (plist-get record :tags) '(cron maintenance)))
        (should (equal (plist-get (plist-get record :attributes) :scope)
                       'sources))))))

(ert-deftest e-cron-actions-test-retires-arbitrary-coordination-actions ()
  "Legacy callback, queue, and wake action forms are rejected."
  (e-cron-actions-test--with-board (target)
    (dolist (action (list '(:enqueue (:prompt "x"))
                          '(:wake trigger)
                          (list :call #'ignore)
                          #'ignore))
      (should-error
       (e-cron-actions-register
        :id (gensym "retired") :when '(:every 60)
        :publication-target target :action action)
       :type 'e-cron-actions-invalid-action))))

(ert-deftest e-cron-actions-test-configured-target-is-explicit-default ()
  "A configured SQL target is the only default used by later registrations."
  (e-cron-actions-test--with-board (target)
    (e-cron-actions-configure-publication-target target)
    (let ((schedule
           (e-cron-actions-register
            :id 'configured :when '(:every 60)
            :action '(:publish (:content "tick")))))
      (e-cron-fire schedule)
      (should (equal (plist-get (car (e-board-producer-test-records target))
                                :content)
                     "tick")))))

(ert-deftest e-cron-actions-test-durable-firing-waits-for-held-sql-publication ()
  "A claimed firing remains live until its Board transaction commits."
  (let* ((stall-directory (make-temp-file "e-cron-board-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         (e-cron--schedules (make-hash-table :test 'equal))
         (e-cron--active-firings (make-hash-table :test 'equal)))
    (unwind-protect
        (e-board-producer-test-with-target (target _service _board-id runtime)
          (let* ((storage (e-cron-storage-sqlite-create runtime))
                 (e-cron-storage storage)
                 (schedule
                  (e-cron-actions-register
                   :id 'held-board :when '(:every 60) :enabled nil
                   :publication-target target
                   :action '(:publish (:content "held SQL")))))
            ;; This ordered read waits for asynchronous registration only at
            ;; the explicit test boundary.
            (e-cron-storage-cadence storage 'held-board)
            (write-region
             "hold" nil
             (expand-file-name "board-record-append.hold" stall-directory)
             nil 'silent)
            (e-cron-fire schedule)
            (should
             (e-cron-actions-test--wait-for
              (lambda ()
                (file-exists-p
                 (expand-file-name "board-record-append.ready"
                                   stall-directory)))))
            (should (= (hash-table-count e-cron--active-firings) 1))
            (should
             (eq (plist-get
                  (car (plist-get
                        (e-cron-storage-cadence storage 'held-board)
                        :unresolved))
                  :state)
                 'claimed))
            (write-region
             "release" nil
             (expand-file-name "board-record-append.release" stall-directory)
             nil 'silent)
            (should
             (e-cron-actions-test--wait-for
              (lambda () (zerop (hash-table-count e-cron--active-firings)))))
            (should
             (e-cron-actions-test--wait-for
              (lambda ()
                (null
                 (plist-get (e-cron-storage-cadence storage 'held-board)
                            :unresolved)))))))
      (delete-directory stall-directory t))))

(ert-deftest e-cron-actions-test-failed-sql-publication-does-not-ack-done ()
  "A failed Board transaction settles its claimed firing as failed."
  (let* ((stall-directory (make-temp-file "e-cron-board-fail-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         (e-cron--schedules (make-hash-table :test 'equal))
         (e-cron--active-firings (make-hash-table :test 'equal)))
    (unwind-protect
        (e-board-producer-test-with-target (_target service _board-id runtime)
          (let* ((storage (e-cron-storage-sqlite-create runtime))
                 (e-cron-storage storage)
                 (missing-target
                  (e-board-sqlite-publication-target-create
                   service "missing-cron-board"))
                 (schedule
                  (e-cron-actions-register
                   :id 'failed-board :when '(:every 60) :enabled nil
                   :publication-target missing-target
                   :action '(:publish (:content "must fail")))))
            (e-cron-storage-cadence storage 'failed-board)
            (write-region
             "hold" nil
             (expand-file-name "board-record-append.hold" stall-directory)
             nil 'silent)
            (e-cron-fire schedule)
            (should
             (e-cron-actions-test--wait-for
              (lambda ()
                (file-exists-p
                 (expand-file-name "board-record-append.ready"
                                   stall-directory)))))
            (should (= (hash-table-count e-cron--active-firings) 1))
            (write-region
             "release" nil
             (expand-file-name "board-record-append.release" stall-directory)
             nil 'silent)
            (should
             (e-cron-actions-test--wait-for
              (lambda () (zerop (hash-table-count e-cron--active-firings)))))
            (e-runtime-store-close runtime)
            (let ((database
                   (sqlite-open (e-runtime-store--database-file runtime))))
              (unwind-protect
                  (let* ((row
                          (car
                           (sqlite-select
                            database
                            "SELECT state FROM cron_firings WHERE schedule_id='failed-board'")))
                         (state (if (vectorp row) (aref row 0) (car row))))
                    (should (equal state "failed")))
                (sqlite-close database)))))
      (delete-directory stall-directory t))))

(provide 'e-cron-actions-test)

;;; e-cron-actions-test.el ends here
