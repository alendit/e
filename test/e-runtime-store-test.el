;;; e-runtime-store-test.el --- SQLite runtime-store adapter scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-runtime-store)
(require 'e-runtime-store-worker)

(cl-defmacro e-runtime-store-test--with-store ((store directory) &rest body)
  "Run BODY with STORE owning a disposable DIRECTORY."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-runtime-store-test-" t))
          (,store (e-runtime-store-open ,directory)))
     (unwind-protect (progn ,@body)
       (ignore-errors (e-runtime-store-close ,store))
       (delete-directory ,directory t))))

(defun e-runtime-store-test--wait-terminal (request)
  "Drive the event loop until REQUEST reaches a terminal state."
  (let ((deadline (+ (float-time) 5.0)))
    (while (and (memq (e-runtime-store-request--state request)
                      '(queued submitted))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))))

(ert-deftest e-runtime-store-codec-round-trips-exact-tagged-values ()
  "Exact nested Lisp values survive and unsupported live values fail early."
  (let* ((map (make-hash-table :test 'equal))
         (value (list nil t :json-false 'symbol :keyword "λ🧵" 42 1.5
                      '(a . b) [nil :x])))
    (puthash "list" value map)
    (let ((decoded (e-runtime-store-codec-decode
                    (e-runtime-store-codec-encode map))))
      (should (equal (gethash "list" decoded) value))
      (should (eq (hash-table-test decoded) 'equal)))
    (should-error (e-runtime-store-codec-encode (current-buffer))
                  :type 'e-runtime-store-codec-error)
    (let ((cycle (list 'x)))
      (setcdr cycle cycle)
      (should-error (e-runtime-store-codec-encode cycle)
                    :type 'e-runtime-store-codec-error))))

(ert-deftest e-runtime-store-s2-serializes-ordinary-owner-writes ()
  "The single worker assigns monotonic positions without a command ledger."
  (e-runtime-store-test--with-store (store directory)
    (should (= (plist-get
                (e-runtime-store-call
                 store 'write '(:op session-append :session-id "s"
                                :record (:value one)))
                :revision)
               1))
    (should (= (plist-get
                (e-runtime-store-call
                 store 'write '(:op session-append :session-id "s"
                                :record (:value two)))
                :revision)
               2))
    (let ((page (e-runtime-store-call
                 store 'read '(:op session-record-page :session-id "s"))))
      (should (equal (mapcar (lambda (entry)
                              (plist-get (plist-get entry :value) :value))
                            (plist-get page :records))
                     '(one two))))
    (e-runtime-store-close store)
    (let ((database (sqlite-open (expand-file-name "store.sqlite3" directory))))
      (unwind-protect
          (should-not
           (sqlite-select
            database
            "SELECT name FROM sqlite_master WHERE type='table' AND name IN ('writer_commands','owner_revisions')"))
        (sqlite-close database)))))

(ert-deftest e-runtime-store-s2-rejects-a-second-live-runtime ()
  "One live worker exclusively owns one physical database."
  (e-runtime-store-test--with-store (store directory)
    (should-error (e-runtime-store-open directory :runtime-id "second")
                  :type 'e-runtime-store-owner-active)
    (should (file-exists-p
             (expand-file-name "store.sqlite3.owner" directory)))
    (should (e-runtime-store-live-p store))))

(ert-deftest e-runtime-store-s2-idle-worker-loss-requires-reopen ()
  "Worker loss freezes the store; reopen reloads canonical committed state."
  (e-runtime-store-test--with-store (store directory)
    (e-runtime-store-call
     store 'write '(:op session-append :session-id "s" :record (:value one)))
    (delete-process (e-runtime-store--process store))
    (while (e-runtime-store--live-p store)
      (accept-process-output nil 0.01))
    (should-error
     (e-runtime-store-call
      store 'write '(:op session-append :session-id "s" :record (:value two)))
     :type 'e-runtime-store-unavailable)
    (should (plist-get (e-runtime-store-status store) :unavailable))
    (e-runtime-store-close store)
    (setq store (e-runtime-store-open directory))
    (let ((page (e-runtime-store-call
                 store 'read '(:op session-record-page :session-id "s"))))
      (should (= (length (plist-get page :records)) 1)))
    (should (= (plist-get
                (e-runtime-store-call
                 store 'write '(:op session-append :session-id "s"
                                :record (:value two)))
                :revision)
               2))))

(ert-deftest e-runtime-store-s2-worker-exit-before-write-response-fails-once ()
  "Worker exit before a write response fails once; reopen reads canonical state."
  (let* ((directory (make-temp-file "e-runtime-store-write-exit-" t))
         (store (e-runtime-store-open directory))
         (process (e-runtime-store--process store))
         (captured "")
         (done-count 0)
         (error-count 0)
         request)
    (unwind-protect
        (progn
          (set-process-filter
           process
           (lambda (worker text)
             (setq captured (concat captured text))
             (when (string-match-p "\n" captured)
               (set-process-filter worker #'ignore)
               (delete-process worker))))
          (setq request
                (e-runtime-store-submit
                 store 'write
                 '(:op session-append :session-id "write-exit"
                   :record (:value once))
                 :on-done (lambda (_result) (cl-incf done-count))
                 :on-error (lambda (_error) (cl-incf error-count))))
          (e-runtime-store-test--wait-terminal request)
          (should (eq (e-runtime-store-request--state request) 'failed))
          (should (= done-count 0))
          (should (= error-count 1))
          (should-error
           (e-runtime-store-call store 'read '(:op status))
           :type 'e-runtime-store-unavailable)
          (e-runtime-store-close store)
          (setq store (e-runtime-store-open directory))
          (let ((page (e-runtime-store-call
                       store 'read
                       '(:op session-record-page :session-id "write-exit"))))
            (should (= (length (plist-get page :records)) 1))))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s2-worker-exit-before-read-response-fails-once ()
  "Worker exit before a read response fails exactly once and freezes the store."
  (e-runtime-store-test--with-store (store directory)
    (e-runtime-store-call
     store 'write '(:op session-append :session-id "read-loss"
                    :record (:value present)))
    (let* ((process (e-runtime-store--process store))
           (captured "")
           (done-count 0)
           (error-count 0)
           request)
      (set-process-filter
       process
       (lambda (worker text)
         (setq captured (concat captured text))
         (when (string-match-p "\n" captured)
           (set-process-filter worker #'ignore)
           (delete-process worker))))
      (setq request
            (e-runtime-store-submit
             store 'read '(:op session-record-page :session-id "read-loss")
             :on-done (lambda (_result) (cl-incf done-count))
             :on-error (lambda (_error) (cl-incf error-count))))
      (e-runtime-store-test--wait-terminal request)
      (should (eq (e-runtime-store-request--state request) 'failed))
      (should (= done-count 0))
      (should (= error-count 1))
      (should (plist-get (e-runtime-store-status store) :unavailable)))))

(ert-deftest e-runtime-store-s2-timeout-freezes-without-late-success ()
  "A submitted timeout fails once and requires explicit close/reopen."
  (let* ((directory (make-temp-file "e-runtime-store-timeout-" t))
         (store (e-runtime-store-open directory))
         (process (e-runtime-store--process store))
         (done-count 0)
         (error-count 0)
         request)
    (unwind-protect
        (progn
          ;; Discard the complete acknowledgement while leaving the worker
          ;; alive, reproducing an ambiguous response timeout deterministically.
          (set-process-filter process #'ignore)
          (setq request
                (e-runtime-store-submit
                 store 'write
                 '(:op session-append :session-id "timeout"
                   :record (:value maybe-committed))
                 :on-done (lambda (_result) (cl-incf done-count))
                 :on-error (lambda (_error) (cl-incf error-count))))
          (should-error (e-runtime-store-await store request 0.02)
                        :type 'e-runtime-store-timeout)
          (accept-process-output nil 0.05)
          (should (eq (e-runtime-store-request--state request) 'failed))
          (should (= done-count 0))
          (should (= error-count 1))
          (should-not (process-live-p process))
          (should (plist-get (e-runtime-store-status store) :unavailable))
          (should-error
           (e-runtime-store-call store 'read '(:op status))
           :type 'e-runtime-store-unavailable)
          (e-runtime-store-close store)
          (setq store (e-runtime-store-open directory))
          (let ((page
                 (e-runtime-store-call
                  store 'read
                  '(:op session-record-page :session-id "timeout"))))
            ;; The database, not transport retry, decides whether the original
            ;; transaction committed before acknowledgement was lost.
            (should (<= (length (plist-get page :records)) 1))))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s2-close-settles-owned-requests-once ()
  "Close fails active and queued work once and ignores a late response."
  (let* ((directory (make-temp-file "e-runtime-store-close-" t))
         (store (e-runtime-store-open directory))
         (process (e-runtime-store--process store))
         (ordinary-filter (process-filter process))
         (captured "")
         (done-count 0)
         (error-count 0)
         requests)
    (unwind-protect
        (progn
          (set-process-filter
           process
           (lambda (_worker text)
             (setq captured (concat captured text))))
          (setq requests
                (list
                 (e-runtime-store-submit
                  store 'write
                  '(:op session-append :session-id "close" :record (:value one))
                  :on-done (lambda (_result) (cl-incf done-count))
                  :on-error (lambda (_error) (cl-incf error-count)))
                 (e-runtime-store-submit
                  store 'write
                  '(:op session-append :session-id "close" :record (:value two))
                  :on-done (lambda (_result) (cl-incf done-count))
                  :on-error (lambda (_error) (cl-incf error-count)))
                 (e-runtime-store-submit
                  store 'read '(:op session-record-page :session-id "close")
                  :on-done (lambda (_result) (cl-incf done-count))
                  :on-error (lambda (_error) (cl-incf error-count)))))
          (let ((deadline (+ (float-time) 5.0)))
            (while (and (not (string-match-p "\n" captured))
                        (< (float-time) deadline))
              (accept-process-output process 0.01)))
          (should (string-match-p "\n" captured))
          (should (eq (e-runtime-store-request--state (car requests))
                      'submitted))
          (e-runtime-store-close store)
          (should (= done-count 0))
          (should (= error-count 3))
          (dolist (request requests)
            (should (eq (e-runtime-store-request--state request) 'failed))
            (should (eq (car (e-runtime-store-request--error request))
                        'e-runtime-store-unavailable)))
          (should (= (hash-table-count (e-runtime-store--pending store)) 0))
          (should (= (plist-get (e-runtime-store-status store) :pending-count) 0))
          ;; Model a filter invocation already queued before close detached the
          ;; process.  It cannot change settlement or publish success.
          (funcall ordinary-filter process captured)
          (should (= done-count 0))
          (should (= error-count 3))
          (should (= (hash-table-count (e-runtime-store--pending store)) 0)))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s2-failure-callback-cannot-break-fanout ()
  "One signaling error callback cannot leave later requests unsettled."
  (e-runtime-store-test--with-store (store directory)
    (let* ((process (e-runtime-store--process store))
           (first-errors 0)
           (second-errors 0)
           first second)
      (set-process-filter process #'ignore)
      (setq first
            (e-runtime-store-submit
             store 'write
             '(:op session-append :session-id "callback-fanout"
               :record (:value first))
             :on-error
             (lambda (_error)
               (cl-incf first-errors)
               (error "intentional callback failure"))))
      (setq second
            (e-runtime-store-submit
             store 'write
             '(:op session-append :session-id "callback-fanout"
               :record (:value second))
             :on-error (lambda (_error) (cl-incf second-errors))))
      (delete-process process)
      (e-runtime-store-test--wait-terminal second)
      (should (eq (e-runtime-store-request--state first) 'failed))
      (should (eq (e-runtime-store-request--state second) 'failed))
      (should (= first-errors 1))
      (should (= second-errors 1))
      (should (= (plist-get (e-runtime-store-status store) :pending-count) 0))
      (should (string-match-p
               "Failure callback signaled"
               (error-message-string
                (plist-get (e-runtime-store-status store) :last-error)))))))

(ert-deftest e-runtime-store-s2-success-callback-failure-is-visible ()
  "A signaling success callback freezes later work without undoing its commit."
  (e-runtime-store-test--with-store (store directory)
    (let ((done-count 0)
          request)
      (setq request
            (e-runtime-store-submit
             store 'write
             '(:op session-append :session-id "callback-success"
               :record (:value committed))
             :on-done
             (lambda (_result)
               (cl-incf done-count)
               (error "intentional success callback failure"))))
      (should (= (plist-get (e-runtime-store-await store request) :revision) 1))
      (should (= done-count 1))
      (should (eq (e-runtime-store-request--state request) 'committed))
      (should (plist-get (e-runtime-store-status store) :unavailable))
      (should (string-match-p
               "request callback signaled"
               (error-message-string
                (plist-get (e-runtime-store-status store) :last-error))))
      (should-error
       (e-runtime-store-call store 'read '(:op status))
       :type 'e-runtime-store-unavailable))))

(ert-deftest e-runtime-store-s2-permission-failure-precedes-mutation ()
  "A mode failure surfaces before BEGIN and leaves no durable mutation."
  (let ((directory (make-temp-file "e-runtime-store-mode-failure-" t)))
    (unwind-protect
        (progn
          (e-runtime-store-worker--open directory "mode-test")
          (let ((request
                 '(:kind write :body
                         (:op session-append :session-id "mode-failure"
                          :record (:value never)))))
            (cl-letf (((symbol-function
                        'e-runtime-store-worker--permissions)
                       (lambda ()
                         (signal 'file-error '("Synthetic chmod failure")))))
              (should-error (e-runtime-store-worker--write request)
                            :type 'file-error))
            (should (= (caar (sqlite-select
                              e-runtime-store-worker--database
                              "SELECT COUNT(*) FROM session_records"))
                       0))))
      (when e-runtime-store-worker--database
        (sqlite-close e-runtime-store-worker--database)
        (setq e-runtime-store-worker--database nil))
      (e-runtime-store-worker--release-owner)
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s2-cancellation-and-write-priority ()
  "Pre-submit cancellation drops work; submitted work stays in flight."
  (e-runtime-store-test--with-store (store directory)
    (let* ((blocker (e-runtime-store-request--create
                     :id "block" :kind 'read :state 'submitted))
           queued)
      (setf (e-runtime-store--active-request store) blocker)
      (setq queued
            (e-runtime-store-submit
             store 'write '(:op session-append :session-id "cancelled"
                            :record (:never t))))
      (should (eq (e-runtime-store-cancel store queued) 'dropped))
      (should (eq (e-runtime-store-request--state queued) 'cancelled))
      (setf (e-runtime-store--active-request store) nil)
      (let ((submitted
             (e-runtime-store-submit
              store 'write '(:op session-append :session-id "submitted"
                             :record (:value t)))))
        (should (eq (e-runtime-store-cancel store submitted) 'in-flight))
        (should (= (plist-get (e-runtime-store-await store submitted) :revision)
                   1))))))

(ert-deftest e-runtime-store-s2-applies-restrictive-store-permissions ()
  "Database, WAL, and live ownership files are private."
  (e-runtime-store-test--with-store (store directory)
    (e-runtime-store-call
     store 'write '(:op session-append :session-id "s" :record (:x t)))
    (dolist (file (list (expand-file-name "store.sqlite3" directory)
                        (expand-file-name "store.sqlite3-wal" directory)
                        (expand-file-name "store.sqlite3.owner" directory)))
      (should (file-exists-p file))
      (should (= (logand (file-modes file) #o777) #o600)))
    (should (= (logand (file-modes directory) #o777) #o700))))

(ert-deftest e-runtime-store-s2-bounded-large-response-frames-remain-exact ()
  "A multi-megabyte row flushes as one response without more stdin."
  (let ((e-runtime-store-request-timeout 15.0))
    (e-runtime-store-test--with-store (store directory)
      (let* ((large (make-string (* 3 1024 1024) ?x))
             (records (vector (list :id 0 :content large)
                              '(:id 1 :content "tail"))))
        (e-runtime-store-call
         store 'write
         (list :op 'session-append-batch :session-id "frames"
               :records records))
        (let ((after 0) (count 0) last page)
          (while
              (progn
                (setq page
                      (e-runtime-store-call
                       store 'read
                       (list :op 'session-record-page :session-id "frames"
                             :after after :limit 256)))
                (dolist (entry (plist-get page :records))
                  (cl-incf count)
                  (setq last entry))
                (setq after (plist-get page :next))))
          (should (= count 2))
          (should (equal (plist-get (plist-get last :value) :content)
                         "tail")))))))

(provide 'e-runtime-store-test)

;;; e-runtime-store-test.el ends here
