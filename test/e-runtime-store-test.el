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

(defconst e-runtime-store-test--source-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this focused runtime-store test source.")

(defun e-runtime-store-test--source (relative)
  "Return repository source RELATIVE to this test directory."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name relative e-runtime-store-test--source-directory))
    (buffer-string)))

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

(ert-deftest e-runtime-store-s2-status-is-constant-cost-and-integrity-explicit ()
  "Ordinary status performs no SQLite scan; explicit integrity still does."
  (let ((e-runtime-store-worker--database 'sentinel)
        selects)
    (cl-letf (((symbol-function 'sqlite-select)
               (lambda (_database sql &rest _arguments)
                 (push sql selects)
                 (list (list "ok")))))
      (let ((status (e-runtime-store-worker--read '(:op status))))
        (should (= (plist-get status :schema-version) 4))
        (should-not (plist-member status :quick-check))
        (should-not selects))
      (let ((integrity
             (e-runtime-store-worker--read '(:op store-integrity))))
        (should (plist-get integrity :ok))
        (should (eq (plist-get integrity :kind) 'quick-check))
        (should (equal selects '("PRAGMA quick_check"))))
      (setq selects nil)
      (let ((integrity
             (e-runtime-store-worker--read
              '(:op store-integrity :full t))))
        (should (plist-get integrity :ok))
        (should (eq (plist-get integrity :kind) 'integrity-check))
        (should (equal selects '("PRAGMA integrity_check")))))))

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
                   :record (:value once))))
          (e-runtime-store-test--wait-terminal request)
          (should (eq (e-runtime-store-request--state request) 'failed))
          (should (eq (car (e-runtime-store-request--error request))
                      'e-runtime-store-unavailable))
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
             store 'read '(:op session-record-page :session-id "read-loss")))
      (e-runtime-store-test--wait-terminal request)
      (should (eq (e-runtime-store-request--state request) 'failed))
      (should (eq (car (e-runtime-store-request--error request))
                  'e-runtime-store-unavailable))
      (should (plist-get (e-runtime-store-status store) :unavailable)))))

(ert-deftest e-runtime-store-s2-timeout-freezes-without-late-success ()
  "A submitted timeout fails once and requires explicit close/reopen."
  (let* ((directory (make-temp-file "e-runtime-store-timeout-" t))
         (store (e-runtime-store-open directory))
         (process (e-runtime-store--process store))
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
                   :record (:value maybe-committed))))
          (let ((timeout
                 (should-error (e-runtime-store-await store request 0.02)
                               :type 'e-runtime-store-timeout)))
            (accept-process-output nil 0.05)
            (should (eq (e-runtime-store-request--state request) 'failed))
            (should-not (process-live-p process))
            (should (plist-get (e-runtime-store-status store) :unavailable))
            (should (eq (plist-get (cddr timeout) :operation)
                        'session-append))
            (dotimes (_ 2)
              (let ((later
                     (should-error
                      (e-runtime-store-call store 'read '(:op status))
                      :type 'e-runtime-store-unavailable)))
                (should (string-match-p
                         "session-append"
                         (error-message-string later)))
                (should (equal (plist-get (cddr later) :cause) timeout)))))
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

(ert-deftest e-runtime-store-s2-queued-timeout-does-not-freeze-active-work ()
  "An unsent timeout cancels only that request and preserves active work."
  (e-runtime-store-test--with-store (store _directory)
    (let* ((process (e-runtime-store--process store))
           (ordinary-filter (process-filter process))
           (captured "")
           active queued)
      (set-process-filter
       process (lambda (_worker text) (setq captured (concat captured text))))
      (setq active
            (e-runtime-store-submit
             store 'write
             '(:op session-append :session-id "queued-timeout"
               :record (:value active))))
      (setq queued
            (e-runtime-store-submit
             store 'write '(:op catalog-put :value ((:id "must-not-land")))))
      (let ((timeout
             (should-error (e-runtime-store-await store queued 0.02)
                           :type 'e-runtime-store-timeout)))
        (should (eq (plist-get (cddr timeout) :operation) 'catalog-put))
        (should (eq (plist-get (cddr timeout) :blocking-operation)
                    'session-append)))
      (should (eq (e-runtime-store-request--state queued) 'failed))
      (should (e-runtime-store-live-p store))
      (should-not (plist-get (e-runtime-store-status store) :unavailable))
      (should-not (memq queued (e-runtime-store--write-queue store)))
      (let ((deadline (+ (float-time) 5.0)))
        (while (and (not (string-match-p "\n" captured))
                    (< (float-time) deadline))
          (accept-process-output process 0.01)))
      (should (string-match-p "\n" captured))
      (set-process-filter process ordinary-filter)
      (funcall ordinary-filter process captured)
      (e-runtime-store-test--wait-terminal active)
      (should (eq (e-runtime-store-request--state active) 'committed))
      (should-not
       (e-runtime-store-call store 'read '(:op catalog-get)))
      (should (e-runtime-store-live-p store)))))

(ert-deftest e-runtime-store-s2-close-settles-owned-requests-once ()
  "Close fails active and queued work once and ignores a late response."
  (let* ((directory (make-temp-file "e-runtime-store-close-" t))
         (store (e-runtime-store-open directory))
         (process (e-runtime-store--process store))
         (ordinary-filter (process-filter process))
         (captured "")
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
                  '(:op session-append :session-id "close" :record (:value one)))
                 (e-runtime-store-submit
                  store 'write
                  '(:op session-append :session-id "close" :record (:value two)))
                 (e-runtime-store-submit
                  store 'read '(:op session-record-page :session-id "close"))))
          (let ((deadline (+ (float-time) 5.0)))
            (while (and (not (string-match-p "\n" captured))
                        (< (float-time) deadline))
              (accept-process-output process 0.01)))
          (should (string-match-p "\n" captured))
          (should (eq (e-runtime-store-request--state (car requests))
                      'submitted))
          (e-runtime-store-close store)
          (dolist (request requests)
            (should (eq (e-runtime-store-request--state request) 'failed))
            (should (eq (car (e-runtime-store-request--error request))
                        'e-runtime-store-unavailable)))
          (should (= (hash-table-count (e-runtime-store--pending store)) 0))
          (should (= (plist-get (e-runtime-store-status store) :pending-count) 0))
          ;; Model a filter invocation already queued before close detached the
          ;; process.  It cannot change terminal settlement.
          (funcall ordinary-filter process captured)
          (dolist (request requests)
            (should (eq (e-runtime-store-request--state request) 'failed)))
          (should (= (hash-table-count (e-runtime-store--pending store)) 0)))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-startup-and-open-failure-settle-selected-request ()
  "Startup and opening failures settle their selected queued request once."
  (dolist (case '((start file-error ("forced startup failure"))
                  (open e-runtime-store-owner-active ("forced open failure"))))
    (pcase-let ((`(,phase ,cause-type ,cause-data) case))
      (let ((store (e-runtime-store--create
                    :directory "/tmp/" :database-file "/tmp/store.sqlite3"
                    :runtime-id (symbol-name phase)
                    :pending (make-hash-table :test 'equal))))
        (cl-letf
            (((symbol-function 'e-runtime-store--start-process)
              (lambda (_store)
                (when (eq phase 'start)
                  (signal cause-type cause-data))
                'started))
             ((symbol-function 'e-runtime-store--ensure-worker-open)
              (lambda (_store)
                (when (eq phase 'open)
                  (signal cause-type cause-data))
                t)))
          (let ((request
                 (e-runtime-store-submit
                  store 'write
                  '(:op session-append :session-id "startup"
                    :record (:value never)))))
            (should (eq (e-runtime-store-request--state request) 'failed))
            (let ((failure
                   (should-error (e-runtime-store-await store request)
                                 :type 'e-runtime-store-unavailable)))
              (should (eq (plist-get (cddr failure) :operation)
                          'session-append))
              (should (eq (plist-get (cddr failure) :kind) 'write))
              (should (equal (plist-get (cddr failure) :request-id)
                             (e-runtime-store-request--id request)))
              (should (equal (plist-get (cddr failure) :cause)
                             (cons cause-type cause-data))))
            (should (eq (e-runtime-store--unavailable-cause store)
                        (e-runtime-store-request--error request)))
            (should-not (e-runtime-store--starting-request store))
            (should-not (e-runtime-store--active-request store))
            (should-not (e-runtime-store--write-queue store))
            (should-not (e-runtime-store--read-queue store))
            (should (= (hash-table-count (e-runtime-store--pending store)) 0))))))))

(ert-deftest e-runtime-store-s92-submission-restarts-the-timeout-interval ()
  "Real scheduler promotion gives a near-expiry request a fresh interval."
  (let* ((store (e-runtime-store--create
                 :runtime-id "phase" :pending (make-hash-table :test 'equal)))
         (clock 0.0)
         open-request candidate sent-kinds request (polls 0))
    (cl-letf
        (((symbol-function 'float-time) (lambda (&optional _time) clock))
         ((symbol-function 'e-runtime-store--start-process)
          (lambda (runtime)
            (setf (e-runtime-store--process runtime) 'phase-worker)
            'phase-worker))
         ((symbol-function 'e-runtime-store--live-p)
          (lambda (_runtime) t))
         ((symbol-function 'process-send-string)
          (lambda (_process _frame)
            (let* ((active (e-runtime-store--active-request store))
                   (kind (e-runtime-store-request--kind active)))
              (push kind sent-kinds)
              (pcase kind
                ('open
                 (setq open-request active
                       candidate (car (e-runtime-store--write-queue store)))
                 ;; The selected domain request stays queue-owned throughout
                 ;; real internal open setup rather than being popped early.
                 (should (eq (e-runtime-store--starting-request store)
                             candidate))
                 (should (eq (e-runtime-store-request--state candidate)
                             'queued))
                 (should (memq candidate (e-runtime-store--write-queue store)))
                 (should-not (eq active candidate)))
                ('write
                 (should (eq active candidate))
                 (should (eq (e-runtime-store-request--state candidate)
                             'submitted))
                 (should (= (e-runtime-store-request--submitted-at candidate)
                            9.9)))))))
         ((symbol-function 'accept-process-output)
          (lambda (&rest _ignored)
            (pcase (cl-incf polls)
              (1
               ;; The open acknowledgement arrives just before the selected
               ;; request's queue interval would expire.
               (setq clock 9.9)
               (e-runtime-store--consume-output
                store
                (e-runtime-store--pack
                 (list :id (e-runtime-store-request--id open-request)
                       :ok t :result '(:opened t)))))
              (2
               (e-runtime-store--consume-output
                store
                (e-runtime-store--pack
                 (list :id (e-runtime-store-request--id candidate)
                       :ok t :result '(:phase submitted)))))))))
      (setq request
            (e-runtime-store-submit
             store 'write
             '(:op session-append :session-id "phase"
               :record (:value retained))))
      (should (eq request candidate))
      (should (eq (e-runtime-store-request--state request) 'submitted))
      (should (equal (nreverse sent-kinds) '(open write)))
      (should (= (e-runtime-store-request--admitted-at request) 0.0))
      (should-not (memq request (e-runtime-store--write-queue store)))
      (should-not (e-runtime-store--starting-request store))
      (should (eq (gethash (e-runtime-store-request--id request)
                           (e-runtime-store--pending store))
                  request))
      ;; At 10.1, the queue interval has elapsed but the submitted interval
      ;; which began at the real dispatch promotion still has almost ten seconds.
      (setq clock 10.1)
      (should (equal (e-runtime-store-await store request 10.0)
                     '(:phase submitted)))
      (should (= polls 2))
      (should (eq (e-runtime-store-request--state request) 'committed)))))

(ert-deftest e-runtime-store-s92-definitive-worker-error-stays-local ()
  "A definitive worker error does not make later persistence unavailable."
  (e-runtime-store-test--with-store (store _directory)
    (let ((failed
           (e-runtime-store-submit store 'read '(:op no-such-operation))))
      (should-error (e-runtime-store-await store failed)
                    :type 'e-runtime-store-worker-error)
      (should (eq (e-runtime-store-request--state failed) 'failed))
      (should (e-runtime-store-live-p store))
      (should-not (plist-get (e-runtime-store-status store) :unavailable))
      (should (= (plist-get
                  (e-runtime-store-call
                   store 'write
                   '(:op session-append :session-id "after-local-error"
                     :record (:value committed)))
                  :revision)
                 1)))))

(ert-deftest e-runtime-store-s92-protocol-failure-preserves-cause-and-recovers ()
  "Malformed and unknown responses freeze once, then recover by reopen."
  (dolist (protocol-cause '(malformed-response unknown-response-id))
    (let* ((directory (make-temp-file "e-runtime-store-protocol-" t))
           (store (e-runtime-store-open directory))
           request sent)
      (unwind-protect
          (progn
            ;; The transport seam leaves the domain request submitted but not
            ;; delivered, so reopening can prove that it was never retried.
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (_process frame) (setq sent frame))))
              (setq request
                    (e-runtime-store-submit
                     store 'write
                     '(:op session-append :session-id "protocol"
                       :record (:value ambiguous)))))
            (should sent)
            (should (eq (e-runtime-store-request--state request) 'submitted))
            (e-runtime-store--consume-output
             store
             (e-runtime-store--pack
              (pcase protocol-cause
                ('malformed-response
                 (list :id (e-runtime-store-request--id request) :ok t))
                ('unknown-response-id
                 '(:id "wrong-response-id" :ok t :result (:ignored t))))))
            (should (eq (e-runtime-store-request--state request) 'failed))
            (let* ((cause (plist-get (e-runtime-store-status store)
                                     :unavailable-cause))
                   (later
                    (should-error
                     (e-runtime-store-call store 'read '(:op status))
                     :type 'e-runtime-store-unavailable)))
              (should (eq (car cause) 'e-runtime-store-protocol-error))
              (should (eq (plist-get (cddr cause) :protocol-cause)
                          protocol-cause))
              (should (eq (plist-get (cddr cause) :operation)
                          'session-append))
              (should (eq (plist-get (cddr cause) :kind) 'write))
              (should (equal (plist-get (cddr cause) :request-id)
                             (e-runtime-store-request--id request)))
              (should (equal (e-runtime-store-request--error request) cause))
              (should (equal (plist-get (cddr later) :cause) cause)))
            (e-runtime-store-close store)
            (setq store (e-runtime-store-open directory))
            (should-not
             (plist-get
              (e-runtime-store-call
               store 'read '(:op session-record-page :session-id "protocol"))
              :records))
            (should (= (plist-get
                        (e-runtime-store-call
                         store 'write
                         '(:op session-append :session-id "protocol"
                           :record (:value recovered)))
                        :revision)
                       1)))
        (ignore-errors (e-runtime-store-close store))
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-s92-submission-surface-has-no-client-hooks ()
  "Terminal request state is the only client observation surface."
  (let* ((first (mapconcat #'identity '("on" "done") "-"))
         (second (mapconcat #'identity '("on" "error") "-"))
         (pattern (concat "\\_<\\(?:" (regexp-quote first) "\\|"
                          (regexp-quote second) "\\)\\_>")))
    (dolist (relative '("../lisp/core/e-runtime-store.el"
                        "e-runtime-store-test.el"))
      (should-not (string-match-p pattern
                                  (e-runtime-store-test--source relative))))
    (should (equal (help-function-arglist #'e-runtime-store-submit)
                   '(store kind body)))))

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
