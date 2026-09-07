;;; e-runtime-store-dp6-test.el --- Feature 92 DP6 runtime tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-runtime-store)
(require 'e-session-query)
(require 'e-session-storage-sqlite)

(defun e-runtime-store-dp6-test--session-append-body (session-id record)
  "Return a valid v6 SESSION-ID append command for transport tests."
  (setq record (copy-tree record)
        record (plist-put record :session-id session-id)
        record (plist-put record :type 'session)
        record (plist-put record :id (concat session-id "-root"))
        record (plist-put record :timestamp "2026-09-07T00:00:00Z"))
  (list :op 'session-append :session-id session-id :record record
        :query-delta
        (list :session-id session-id :name session-id :summary nil
              :metadata nil :created-at "2026-09-07T00:00:00Z"
              :updated-at "2026-09-07T00:00:00Z" :last-message-at nil
              :latest-assistant-marker nil :message-count 0
              :current-branch nil :turn-options nil
              :current-head-id (concat session-id "-root")
              :root-event-id (concat session-id "-root")
              :current-context-generation-id nil :board-id nil
              :principal nil :association-role nil :routing-policy nil
              :root-p t :board-output-sequence 0
              :board-activity-sequence 0 :journal-position 1)))

(defun e-runtime-store-dp6-test--store ()
  "Return a disconnected runtime suitable for admission/partition tests."
  (e-runtime-store--create
   :directory temporary-file-directory
   :database-file (expand-file-name "dp6-test.sqlite" temporary-file-directory)
   :runtime-id "dp6-test" :pending (make-hash-table :test 'equal)
   :reservation (e-runtime-store--reservation-create)))

(defun e-runtime-store-dp6-test--drain (store)
  "Release all terminal notification ownership in STORE."
  (while (e-runtime-store--notification-outbox store)
    (e-runtime-store--drain-terminal-notifications store t)))

(defun e-runtime-store-dp6-test--wait-ready (store)
  "Wait for STORE's isolated worker open acknowledgement."
  (let ((deadline (+ (float-time) 8.0)))
    (while (and (not (eq (e-runtime-store--opened-process store)
                         (e-runtime-store--process store)))
                (< (float-time) deadline))
      (sit-for 0.01))
    (should (eq (e-runtime-store--opened-process store)
                (e-runtime-store--process store)))))

(defun e-runtime-store-dp6-test--wait-terminal (request)
  "Wait for isolated REQUEST to reach a terminal state."
  (let ((deadline (+ (float-time) 8.0)))
    (while (and (memq (e-runtime-store-request--state request)
                      '(queued submitted))
                (< (float-time) deadline))
      (sit-for 0.01))
    (should (memq (e-runtime-store-request--state request)
                  '(committed failed cancelled)))))

(ert-deftest e-runtime-store-s92-dp6-one-client-fifo-preserves-read-write-order ()
  "Reads and writes retain exact admission order in the one client FIFO."
  (let ((store (e-runtime-store-dp6-test--store)))
    (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
      (let ((read (e-runtime-store-submit store 'read '(:op integrity)))
            (write (e-runtime-store-submit
                    store 'write '(:op session-delete :session-id "s")))
            (read-two (e-runtime-store-submit store 'read '(:op integrity))))
        (should (equal (e-runtime-store--client-queue store)
                       (list read write read-two)))
        (should (eq (e-runtime-store--next-queued-request store) read))
        (e-runtime-store--fail-all store '(e-runtime-store-error "cleanup"))
        (e-runtime-store-dp6-test--drain store)
        (should (= (e-runtime-store--request-count store) 0))
        (should (= (e-runtime-store--notification-count store) 0))))))

(ert-deftest e-runtime-store-s92-dp6-owner-failure-partitions-one-fifo ()
  "One optimistic owner fails locally while unrelated FIFO order survives."
  (let ((store (e-runtime-store-dp6-test--store)) observed)
    (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore)
              ((symbol-function 'e-runtime-store--fence-worker) #'ignore))
      (let* ((owner '(session . "daily"))
             (active (e-runtime-store--submit-owned
                      store 'write '(:op session-delete :session-id "daily") owner))
             (unrelated-read
              (e-runtime-store-submit store 'read '(:op integrity)))
             (same-owner
              (e-runtime-store--submit-owned
               store 'write '(:op session-delete :session-id "daily") owner))
             (unrelated-write
              (e-runtime-store--submit-owned
               store 'write '(:op session-delete :session-id "healthy")
               '(session . "healthy"))))
        (setf (e-runtime-store--client-queue store)
              (cdr (e-runtime-store--client-queue store))
              (e-runtime-store--active-request store) active
              (e-runtime-store-request--state active) 'submitted)
        (puthash (e-runtime-store-request--id active) active
                 (e-runtime-store--pending store))
        (dolist (request (list active same-owner))
          (e-runtime-store--observe
           request (lambda (settled)
                     (push (e-runtime-store-request--error settled) observed))))
        (e-runtime-store--partition-owner-failure
         store active '(e-runtime-store-timeout "first failure"))
        (should (eq (e-runtime-store-request--state active) 'failed))
        (should (eq (car (e-runtime-store-request--error active))
                    'e-runtime-store-timeout))
        (should (eq (car (e-runtime-store-request--error same-owner))
                    'e-runtime-store-persistence-suspect))
        (should (equal (e-runtime-store--client-queue store)
                       (list unrelated-read unrelated-write)))
        (should-not (e-runtime-store--unavailable store))
        (e-runtime-store-dp6-test--drain store)
        (should (= (length observed) 2))
        ;; Clean retained unrelated owners without changing the assertion.
        (e-runtime-store--fail-all store '(e-runtime-store-error "cleanup"))
        (e-runtime-store-dp6-test--drain store)
        (should (= (e-runtime-store--request-count store) 0))
        (should (= (e-runtime-store--notification-count store) 0))))))

(ert-deftest e-runtime-store-s92-dp6-owner-key-is-private-frozen-and-fails-fast ()
  "Owner metadata stays off wire and a suspect owner cannot enqueue again."
  (let ((store (e-runtime-store-dp6-test--store))
        (owner-id (copy-sequence "mutable-owner")))
    (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore)
              ((symbol-function 'e-runtime-store--fence-worker) #'ignore))
      (let* ((owner (cons 'session owner-id))
             (request (e-runtime-store--submit-owned
                       store 'write
                       '(:op session-delete :session-id "wire-session") owner))
             (frame (e-runtime-store-request--frame request))
             (first (list 'e-runtime-store-timeout
                          (make-string 4096 ?x) :request-id "first")))
        (aset owner-id 0 ?X)
        (should (equal (e-runtime-store-request--owner-key request)
                       '(session . "mutable-owner")))
        (should-not (string-match-p "mutable-owner" frame))
        (setf (e-runtime-store--client-queue store) nil
              (e-runtime-store--active-request store) request
              (e-runtime-store-request--state request) 'submitted)
        (e-runtime-store--partition-owner-failure store request first)
        (let ((error (e-runtime-store-request--error request)))
          (should (eq (car error) 'e-runtime-store-timeout))
          (should-not (eq error first))
          (should (<= (string-bytes (cadr error))
                      e-runtime-store-owner-diagnostic-byte-limit))
          (should (eq (plist-get (cddr error) :operation) 'session-delete))
          (should (eq (plist-get (cddr error) :kind) 'write))
          (should (equal (plist-get (cddr error) :request-id)
                         (e-runtime-store-request--id request))))
        (let* ((status (e-runtime-store-status store))
               (suspect (car (plist-get status :suspect-owners))))
          (should (equal (plist-get suspect :owner-key)
                         '(session . "mutable-owner")))
          (should (<= (string-bytes
                       (plist-get suspect :first-diagnostic))
                      1024)))
        (let ((before (e-runtime-store--request-count store)))
          (should-error
           (e-runtime-store--submit-owned
            store 'write '(:op session-delete :session-id "wire-session")
            '(session . "mutable-owner"))
           :type 'e-runtime-store-persistence-suspect)
          (should (= (e-runtime-store--request-count store) before)))
        (e-runtime-store-dp6-test--drain store)
        (should (= (e-runtime-store--request-count store) 0))))))

(ert-deftest e-runtime-store-s92-dp6-real-owner-failure-retains-unrelated-fifo ()
  "Exhaustion fails one owner and a lazy replacement commits unrelated work."
  (let* ((directory (make-temp-file "e-runtime-store-dp6-partition-" t))
         (marker (make-temp-file "e-runtime-store-dp6-open-fault-"))
         (store (e-runtime-store-open directory))
         requests observed)
    (delete-file marker)
    (unwind-protect
        (progn
          (e-runtime-store-dp6-test--wait-ready store)
          (let ((old-process (e-runtime-store--process store)))
            (set-process-filter old-process #'ignore)
            (setq requests
                  (list
                   (e-runtime-store--submit-owned
                    store 'write
                    (e-runtime-store-dp6-test--session-append-body
                     "daily" '(:value active))
                    '(session . "daily"))
                   (e-runtime-store-submit store 'read '(:op status))
                   (e-runtime-store--submit-owned
                    store 'write
                    (e-runtime-store-dp6-test--session-append-body
                     "daily" '(:value successor))
                    '(session . "daily"))
                   (e-runtime-store--submit-owned
                    store 'write
                    (e-runtime-store-dp6-test--session-append-body
                     "healthy" '(:value durable))
                    '(session . "healthy"))
                   (e-runtime-store-submit
                    store 'read
                    '(:op session-record-page :session-id "healthy"))))
            (dolist (request requests)
              (e-runtime-store--observe
               request
               (lambda (settled)
                 (setq observed
                       (nconc observed
                              (list (e-runtime-store-request--id settled)))))))
            (let ((deadline (+ (float-time) 3.0)))
              (while (and (eq (e-runtime-store-request--state (car requests))
                              'queued)
                          (< (float-time) deadline))
                (sit-for 0.01)))
            (should (eq (e-runtime-store-request--state (car requests))
                        'submitted))
            (let ((process-environment
                   (cons "E_RUNTIME_STORE_TEST_FAULT=after-open-response-formation"
                         (cons (concat "E_RUNTIME_STORE_TEST_FAULT_ONCE_FILE=" marker)
                               process-environment))))
              (delete-process old-process)
              (e-runtime-store-dp6-test--wait-terminal (car requests))))
          (dolist (request requests)
            (e-runtime-store-dp6-test--wait-terminal request))
          (should (eq (e-runtime-store-request--state (nth 0 requests)) 'failed))
          (should (eq (e-runtime-store-request--state (nth 2 requests)) 'failed))
          (should (eq (car (e-runtime-store-request--error (nth 2 requests)))
                      'e-runtime-store-persistence-suspect))
          (dolist (index '(1 3 4))
            (should (eq (e-runtime-store-request--state (nth index requests))
                        'committed)))
          (let* ((page (e-runtime-store-request--result (nth 4 requests)))
                 (records (plist-get page :records)))
            (should (= (length records) 1))
            (should (eq (plist-get (plist-get (seq-elt records 0) :value) :value)
                        'durable)))
          (let ((deadline (+ (float-time) 3.0)))
            (while (and (> (e-runtime-store--notification-count store) 0)
                        (< (float-time) deadline))
              (sit-for 0.01)))
          (should (= (length observed) 5))
          (should (= (e-runtime-store--request-count store) 0))
          (should (= (e-runtime-store--notification-count store) 0))
          (should (= (e-runtime-store--reserved-bytes store) 0))
          (should-not (plist-get (e-runtime-store-status store) :unavailable)))
      (when (file-exists-p marker) (delete-file marker))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-dp6-nil-owner-never-groups ()
  "A failed nil-key request leaves every nil-key successor in FIFO order."
  (let ((store (e-runtime-store-dp6-test--store)))
    (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore)
              ((symbol-function 'e-runtime-store--fence-worker) #'ignore))
      (let ((active (e-runtime-store-submit store 'read '(:op integrity)))
            (next (e-runtime-store-submit store 'read '(:op integrity))))
        (setf (e-runtime-store--client-queue store) (list next)
              (e-runtime-store--active-request store) active
              (e-runtime-store-request--state active) 'submitted)
        (e-runtime-store--partition-owner-failure
         store active '(e-runtime-store-timeout "read failed"))
        (should (eq (e-runtime-store-request--state active) 'failed))
        (should (equal (e-runtime-store--client-queue store) (list next)))
        (should (eq (e-runtime-store-request--state next) 'queued))
        (e-runtime-store--fail-all store '(e-runtime-store-error "cleanup"))
        (e-runtime-store-dp6-test--drain store)))))

(ert-deftest e-runtime-store-s92-dp6-client-capacity-is-exactly-128 ()
  "The active-inclusive client capacity admits 128 and rejects request 129."
  (let ((store (e-runtime-store-dp6-test--store)) requests)
    (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
      (dotimes (_ e-runtime-store-request-capacity)
        (push (e-runtime-store-submit store 'read '(:op integrity)) requests))
      (should (= (length (e-runtime-store--client-queue store)) 128))
      (should-error (e-runtime-store-submit store 'read '(:op integrity))
                    :type 'e-runtime-store-capacity-exhausted)
      (e-runtime-store--fail-all store '(e-runtime-store-error "cleanup"))
      (e-runtime-store-dp6-test--drain store)
      (should (= (e-runtime-store--request-count store) 0))
      (should (= (e-runtime-store--notification-count store) 0)))))

(ert-deftest e-runtime-store-s92-dp6-session-record-and-batch-bounds ()
  "Record, batch count, and complete batch bytes accept exact and reject +1."
  (let* ((empty '(:value ""))
         (overhead (e-runtime-store-codec-measure-bounded
                    empty e-session-storage-record-byte-limit))
         (exact-record
          (list :value (make-string
                        (- e-session-storage-record-byte-limit overhead) ?x)))
         (over-record
          (list :value (make-string
                        (1+ (- e-session-storage-record-byte-limit overhead)) ?x))))
    (should (= (e-runtime-store-codec-measure-bounded
                exact-record e-session-storage-record-byte-limit)
               e-session-storage-record-byte-limit))
    (e-session-storage-sqlite--preflight-session-body
     (list :op 'session-append :session-id "s" :record exact-record))
    (should-error
     (e-session-storage-sqlite--preflight-session-body
      (list :op 'session-append :session-id "s" :record over-record))
     :type 'e-runtime-store-codec-too-large)
    (let ((exact-count (make-vector e-session-storage-batch-record-limit nil))
          (over-count (make-vector (1+ e-session-storage-batch-record-limit) nil)))
      (e-session-storage-sqlite--preflight-session-body
       (list :op 'session-append-batch :session-id "s" :records exact-count))
      (should-error
       (e-session-storage-sqlite--preflight-session-body
        (list :op 'session-append-batch :session-id "s" :records over-count))
       :type 'e-runtime-store-request-too-large))
    (let* ((records (make-vector 9 nil))
           (body (list :op 'session-append-batch :session-id "s" :records records)))
      (dotimes (index 9) (aset records index (list :value "")))
      (let* ((base (e-runtime-store-codec-measure-bounded
                    body e-session-storage-batch-byte-limit))
             (payload (- e-session-storage-batch-byte-limit base))
             (each (/ payload 9))
             (remainder (% payload 9)))
        (dotimes (index 9)
          (setcar (cdr (aref records index))
                  (make-string (+ each (if (< index remainder) 1 0)) ?x))))
      (should (= (e-runtime-store-codec-measure-bounded
                  body e-session-storage-batch-byte-limit)
                 e-session-storage-batch-byte-limit))
      (e-session-storage-sqlite--preflight-session-body body)
      (let* ((last (aref records 8))
             (value (plist-get last :value)))
        (plist-put last :value (concat value "x")))
      (should-error
       (e-session-storage-sqlite--preflight-session-body body)
       :type 'e-runtime-store-codec-too-large))))

(ert-deftest e-runtime-store-s92-dp6-correlated-negative-partitions-owner-in-place ()
  "A negative keyed write response preserves the healthy worker and FIFO tail."
  (let* ((directory (make-temp-file "e-runtime-store-dp6-negative-" t))
         (store (e-runtime-store-open directory))
         worker active same-owner unrelated)
    (unwind-protect
        (progn
          (e-runtime-store-dp6-test--wait-ready store)
          (setq worker (e-runtime-store--process store)
                active
                (e-runtime-store--submit-owned
                 store 'write
                 (list :op 'session-append-batch :session-id "negative"
                       :records
                       (make-vector
                        (1+ e-session-storage-batch-record-limit) nil))
                 '(session . "negative"))
                same-owner
                (e-runtime-store--submit-owned
                 store 'write
                 '(:op session-delete :session-id "negative")
                 '(session . "negative"))
                unrelated
                (e-runtime-store-submit store 'read '(:op status)))
          (dolist (request (list active same-owner unrelated))
            (e-runtime-store-dp6-test--wait-terminal request))
          (should (eq (e-runtime-store-request--state active) 'failed))
          (should (eq (car (e-runtime-store-request--error active))
                      'e-runtime-store-worker-error))
          (should (eq (e-runtime-store-request--state same-owner) 'failed))
          (should (eq (car (e-runtime-store-request--error same-owner))
                      'e-runtime-store-persistence-suspect))
          (should (eq (e-runtime-store-request--state unrelated) 'committed))
          (should (eq worker (e-runtime-store--process store)))
          (should (process-live-p worker))
          (should (eq worker (e-runtime-store--opened-process store)))
          (should-not (e-runtime-store--unavailable store)))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-dp6-owner-errors-and-status-are-bounded-detached ()
  "Owner failure retains bounded correlation facts without caller aliases."
  (let ((store (e-runtime-store-dp6-test--store))
        (message (make-string 4096 ?x)))
    (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore)
              ((symbol-function 'e-runtime-store--fence-worker) #'ignore))
      (let* ((request
              (e-runtime-store--submit-owned
               store 'write '(:op session-delete :session-id "bounded")
               '(session . "bounded")))
             (cause
              (list 'e-runtime-store-timeout message
                    :operation 'wrong-operation :kind 'read
                    :request-id "wrong-request")))
        (setf (e-runtime-store--client-queue store) nil
              (e-runtime-store--active-request store) request
              (e-runtime-store-request--state request) 'submitted)
        (e-runtime-store--partition-owner-failure store request cause)
        (let* ((error (e-runtime-store-request--error request))
               (properties (cddr error)))
          (should (eq (car error) 'e-runtime-store-timeout))
          (should (<= (string-bytes (cadr error))
                      e-runtime-store-owner-diagnostic-byte-limit))
          (should (eq (plist-get properties :operation) 'session-delete))
          (should (eq (plist-get properties :kind) 'write))
          (should (equal (plist-get properties :request-id)
                         (e-runtime-store-request--id request)))
          (should-not (eq error cause)))
        (aset message 0 ?Y)
        (let* ((first (car (plist-get (e-runtime-store-status store)
                                      :suspect-owners)))
               (first-key (cdr (plist-get first :owner-key)))
               (first-diagnostic (plist-get first :first-diagnostic)))
          (aset first-key 0 ?X)
          (aset first-diagnostic 0 ?Z)
          (let ((second (car (plist-get (e-runtime-store-status store)
                                        :suspect-owners))))
            (should (equal (plist-get second :owner-key)
                           '(session . "bounded")))
            (should-not (eq (plist-get first :first-diagnostic)
                            (plist-get second :first-diagnostic)))))
        (e-runtime-store-dp6-test--drain store)
        (should (= (e-runtime-store--request-count store) 0))))))

(ert-deftest e-runtime-store-s92-dp6-owned-admission-validates-before-ownership ()
  "Invalid or undetachable owner keys reject before IDs, queues, or ledgers move."
  (let ((store (e-runtime-store-dp6-test--store)))
    (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
      (dolist (case
               (list (list 'read '(session . "read-owner"))
                     (list 'write '(unknown-domain . "wrong-domain"))
                     (list 'write (cons 'board (make-string 129 ?x)))
                     (list 'write '(session . not-a-string))))
        (should-error
         (e-runtime-store--submit-owned
          store (car case) '(:op status) (cadr case))
         :type 'wrong-type-argument))
      (let ((sequence (e-runtime-store--sequence store))
            (write-prefix (e-runtime-store--write-prefix-sequence store)))
        (cl-letf (((symbol-function 'e-runtime-store--copy-owner-key)
                   (lambda (_owner-key)
                     (signal 'e-runtime-store-error '("copy failed")))))
          (should-error
           (e-runtime-store--submit-owned
            store 'write '(:op session-delete :session-id "copy")
            '(session . "copy"))
           :type 'e-runtime-store-error))
        (should (= (e-runtime-store--sequence store) sequence))
        (should (= (e-runtime-store--write-prefix-sequence store) write-prefix)))
      (should-not (e-runtime-store--client-queue store))
      (should (= (e-runtime-store--request-count store) 0))
      (should (= (e-runtime-store--notification-count store) 0))
      (should (= (e-runtime-store--reserved-bytes store) 0))
      (should (= (e-runtime-store--reservation-used
                  (e-runtime-store--reservation store))
                 0)))))

(provide 'e-runtime-store-dp6-test)

;;; e-runtime-store-dp6-test.el ends here
