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

(ert-deftest e-runtime-store-codec-bounded-measure-matches-reader-syntax ()
  "The allocation-light count equals the final canonical reader bytes."
  (let ((unibyte (string-make-unibyte (concat "a" (string 255) "\"\\")))
        (all-octets
         (apply #'unibyte-string (number-sequence 0 255))))
    (dolist (value (list ""
                         "quote=\" slash=\\ newline=\n tab=\t"
                         unibyte
                         all-octets
                         "λ🧵"
                         (list :nested [nil t :json-false "leaf"])))
      (let* ((form (e-runtime-store-codec--form value))
             (printed (e-runtime-store-codec--print-form form))
             (exact (string-bytes printed)))
        (should (= (e-runtime-store-codec--measure-form-bounded form exact)
                   exact))
        (should (equal (e-runtime-store-codec-encode-bounded value exact)
                       printed))
        (should-error
         (e-runtime-store-codec--measure-form-bounded form (1- exact))
         :type 'e-runtime-store-codec-too-large))))
  ;; Text properties are display state and no longer enter the durable tagged
  ;; grammar, so their generic printer syntax cannot bypass bounded counting.
  ;; Existing serialized values remain decoder-readable for recovery.
  (let* ((legacy-value (propertize "display" 'face 'bold))
         (legacy-form
          (vector 'e-runtime-store-value e-runtime-store-codec-version
                  (vector 'string legacy-value)))
         (legacy (e-runtime-store-codec--print-form legacy-form))
         (decoded (e-runtime-store-codec-decode legacy))
         (rejected (propertize (make-string 4096 ?x) 'face 'bold))
         (failure
          (condition-case err
              (progn (e-runtime-store-codec-encode rejected) nil)
            (e-runtime-store-codec-error err))))
    (should (equal decoded legacy-value))
    (should (eq (get-text-property 0 'face decoded) 'bold))
    (should (eq (car failure) 'e-runtime-store-codec-error))
    ;; A failure must not retain or print the arbitrarily sized input.
    (should-not (memq rejected (cdr failure)))
    (should (= (plist-get (cddr failure) :string-bytes)
               (string-bytes rejected)))))

(ert-deftest e-runtime-store-codec-bounded-multibyte-raw-bytes-match-printer ()
  "All Emacs multibyte raw-byte characters use their octal reader bytes."
  (let* ((raw-octets
          (string-to-multibyte
           (apply #'unibyte-string (number-sequence 128 255))))
         ;; Exercise raw bytes embedded among ordinary multibyte text and
         ;; reader-escaped ASCII characters, not as a separate grammar case.
         (raw-bytes (concat "λ\"" raw-octets "\\🧵"))
         (form (e-runtime-store-codec--form raw-bytes))
         (printed (e-runtime-store-codec--print-form form))
         (exact (string-bytes printed)))
    (should (= (e-runtime-store-codec--measure-form-bounded form exact)
               exact))
    (should (equal (e-runtime-store-codec-encode-bounded raw-bytes exact)
                   printed))
    (should-error
     (e-runtime-store-codec-encode-bounded raw-bytes (1- exact))
     :type 'e-runtime-store-codec-too-large)))

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
  "Every malformed complete frame freezes once, then recover by reopen."
  (dolist (protocol-cause '(empty-response malformed-response unknown-response-id))
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
             (pcase protocol-cause
               ('empty-response "\n")
               (_
                (e-runtime-store--pack
                 (pcase protocol-cause
                   ('malformed-response
                    (list :id (e-runtime-store-request--id request) :ok t))
                   ('unknown-response-id
                    '(:id "wrong-response-id" :ok t :result (:ignored t))))))))
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

(ert-deftest e-runtime-store-s92-empty-worker-input-is-not-a-keepalive ()
  "A blank parent request frame reaches the normal malformed-request path."
  (let ((lines '("")) request response closed)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _arguments)
                 (if lines
                     (prog1 (car lines) (setq lines (cdr lines)))
                   (signal 'end-of-file nil))))
              ((symbol-function 'e-runtime-store-worker--emit-response)
               (lambda (candidate-request candidate-response)
                 (setq request candidate-request
                       response candidate-response)))
              ((symbol-function 'e-runtime-store-worker--close)
               (lambda () (setq closed t))))
      (e-runtime-store-worker-main))
    (should closed)
    (should (plist-member request :decode-error))
    (should-not (plist-get request :id))
    (should-not (plist-get response :ok))))

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
      (e-runtime-store-worker--close)
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

(defun e-runtime-store-test--response-line (response)
  "Return unbounded fixture wire text for exact boundary tests.

The fixture deliberately bypasses production packing so one-over inbound wire
tests can present a raw frame that production would refuse to create."
  (base64-encode-string (e-runtime-store-codec-encode response) t))

(defun e-runtime-store-test--submitted-store (request)
  "Return a minimal STORE with submitted REQUEST as its active correlation."
  (let ((store (e-runtime-store--create
                :runtime-id "runtime-bounds"
                :pending (make-hash-table :test 'equal))))
    (setf (e-runtime-store--active-request store) request)
    (puthash (e-runtime-store-request--id request) request
             (e-runtime-store--pending store))
    store))

(ert-deftest e-runtime-store-s92-c04-bounded-codec-and-request-preflight ()
  "Canonical exact/one-over bounds reject before scheduler queue admission."
  (let* ((value (list :payload (make-string 256 ?x)))
         (encoded (e-runtime-store-codec-encode value))
         (exact (string-bytes encoded))
         (printed nil))
    (should (= e-runtime-store-codec-protocol-canonical-byte-limit
               71303168))
    (should (= e-runtime-store-codec-protocol-wire-byte-limit 95070893))
    (should (equal (e-runtime-store-codec-encode-bounded value exact) encoded))
    (cl-letf (((symbol-function 'e-runtime-store-codec--print-form)
               (lambda (_form)
                 (setq printed t)
                 (ert-fail "oversized form was fully printed"))))
      (should-error
       (e-runtime-store-codec-encode-bounded value (1- exact))
       :type 'e-runtime-store-codec-too-large))
    (should-not printed)
    (let* ((canonical-limit 100)
           (wire-limit
            (e-runtime-store-codec-wire-byte-count canonical-limit))
           ;; 101 unpadded raw bytes fit the derived 137-byte wire ceiling for
           ;; a 100-byte canonical limit, so both endpoints must check decoded
           ;; canonical bytes as a distinct domain.
           (one-over-wire (base64-encode-string (make-string 101 ?x) t)))
      (let ((e-runtime-store-codec-protocol-canonical-byte-limit canonical-limit)
            (e-runtime-store-codec-protocol-wire-byte-limit wire-limit))
        (should (= (1+ (string-bytes one-over-wire)) wire-limit))
        (should-error (e-runtime-store--unpack one-over-wire)
                      :type 'e-runtime-store-codec-too-large)
        (should-error (e-runtime-store-worker--unpack one-over-wire)
                      :type 'e-runtime-store-codec-too-large)))
    (let* ((body (list :op 'status :padding (make-string 48 ?x)))
           (prototype (e-runtime-store-request--create
                       :id "bounded:w:1" :kind 'write :body body))
           (canonical-limit
            (string-bytes
             (e-runtime-store-codec-encode
              (e-runtime-store--request-frame prototype))))
           (wire-limit
            (e-runtime-store-codec-wire-byte-count canonical-limit)))
      (let ((e-runtime-store-codec-protocol-canonical-byte-limit canonical-limit)
            (e-runtime-store-codec-protocol-wire-byte-limit wire-limit)
            (store (e-runtime-store--create
                    :runtime-id "bounded" :pending (make-hash-table :test 'equal))))
        (cl-letf (((symbol-function 'e-runtime-store--dispatch-next) #'ignore))
          (let ((request (e-runtime-store-submit store 'write body)))
            (should (eq (e-runtime-store-request--state request) 'queued))
            (should (memq request (e-runtime-store--write-queue store)))
            (should (= (string-bytes (e-runtime-store-request--frame request))
                       canonical-limit))))
      (let ((e-runtime-store-codec-protocol-canonical-byte-limit canonical-limit)
            (e-runtime-store-codec-protocol-wire-byte-limit wire-limit)
            (store (e-runtime-store--create
                    :runtime-id "bounded" :pending (make-hash-table :test 'equal))))
        (cl-letf (((symbol-function 'e-runtime-store--dispatch-next) #'ignore))
          (should-error
           (e-runtime-store-submit
            store 'write
            (plist-put (copy-sequence body) :padding
                       (concat (plist-get body :padding) "x")))
           :type 'e-runtime-store-request-too-large)
          (should-not (e-runtime-store--write-queue store))
          (should-not (e-runtime-store--read-queue store))
          (should (= (hash-table-count (e-runtime-store--pending store)) 0))))))))

(ert-deftest e-runtime-store-s92-c04-preflight-rejects-raw-byte-canonical-overflow ()
  "Preflight rejects raw-byte overflow before base64 rounding can mask it."
  (let* ((raw-bytes (string-to-multibyte (unibyte-string 128 192 255)))
         (body (list :op 'status :content raw-bytes))
         (request (e-runtime-store-request--create
                   :id "raw-byte:w:1" :kind 'write :body body))
         (actual-canonical
          (string-bytes
           (e-runtime-store-codec-encode
            (e-runtime-store--request-frame request))))
         ;; Three raw bytes yield a canonical size of 3k+2.  Therefore the
         ;; one-byte-smaller canonical ceiling has the same rounded wire cap.
         (canonical-limit (1- actual-canonical))
         (wire-limit
          (e-runtime-store-codec-wire-byte-count canonical-limit)))
    (should (= (mod actual-canonical 3) 2))
    (should (= (e-runtime-store-codec-wire-byte-count actual-canonical)
               wire-limit))
    (let ((e-runtime-store-codec-protocol-canonical-byte-limit canonical-limit)
          (e-runtime-store-codec-protocol-wire-byte-limit wire-limit))
      (should-error (e-runtime-store--preflight-request request)
                    :type 'e-runtime-store-request-too-large)
      (should-not (e-runtime-store-request--frame request)))))

(ert-deftest e-runtime-store-s92-c04-request-frame-has-queue-only-lifetime ()
  "A bounded canonical frame survives queueing, never terminal retention."
  (let* ((store (e-runtime-store--create
                 :directory "frame-test" :runtime-id "frame"
                 :pending (make-hash-table :test 'equal)))
         ordinary-wire)
    (cl-letf (((symbol-function 'e-runtime-store--start-process)
               (lambda (candidate)
                 (setf (e-runtime-store--process candidate) 'ordinary-process)
                 'ordinary-process))
              ((symbol-function 'e-runtime-store--ensure-worker-open) #'ignore)
              ((symbol-function 'process-send-string)
               (lambda (_process wire) (setq ordinary-wire wire))))
      (let ((request (e-runtime-store-submit store 'write '(:op status))))
        (should (eq (e-runtime-store-request--state request) 'submitted))
        (should ordinary-wire)
        (should-not (e-runtime-store-request--frame request))
        (e-runtime-store--settle
         store request
         (list :id (e-runtime-store-request--id request)
               :ok t :result '(:sent t)))
        (should (eq (e-runtime-store-request--state request) 'committed))
        (should-not (e-runtime-store-request--frame request)))))
  (let* ((store (e-runtime-store--create
                 :directory "frame-test" :runtime-id "open-frame"
                 :process 'open-process :pending (make-hash-table :test 'equal)))
         open-request open-wire)
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_process wire) (setq open-wire wire)))
              ((symbol-function 'e-runtime-store-await)
               (lambda (_store request &optional _timeout)
                 (setq open-request request)
                 '(:opened t))))
      (e-runtime-store--ensure-worker-open store))
    (should open-wire)
    (should-not (e-runtime-store-request--frame open-request)))
  (let* ((store (e-runtime-store--create
                 :runtime-id "terminal-frame"
                 :pending (make-hash-table :test 'equal)))
         (failed (e-runtime-store-request--create
                  :id "failed" :kind 'write :state 'queued :frame "large"))
         (cancelled (e-runtime-store-request--create
                     :id "cancelled" :kind 'write :state 'queued :frame "large")))
    (e-runtime-store--fail-request store failed
                                   '(e-runtime-store-error "local failure"))
    (should-not (e-runtime-store-request--frame failed))
    (setf (e-runtime-store--write-queue store) (list cancelled))
    (should (eq (e-runtime-store-cancel store cancelled) 'dropped))
    (should-not (e-runtime-store-request--frame cancelled))))

(ert-deftest e-runtime-store-s92-c04-fragmented-wire-bounds-and-reentrant-remainder ()
  "Fragmented exact wire frames settle once across nested filter reentry."
  (let* ((first (e-runtime-store-request--create
                 :id "first" :kind 'read :state 'submitted))
         (second (e-runtime-store-request--create
                  :id "second" :kind 'read :state 'submitted))
         (third (e-runtime-store-request--create
                 :id "third" :kind 'read :state 'submitted))
         (store (e-runtime-store-test--submitted-store first))
         (line-one (e-runtime-store-test--response-line
                    '(:id "first" :ok t :result (:value one))))
         (line-two (e-runtime-store-test--response-line
                    '(:id "second" :ok t :result (:value two))))
         (line-three (e-runtime-store-test--response-line
                      '(:id "third" :ok t :result (:value three))))
         (half (/ (length line-one) 2))
         (dispatches 0))
    (puthash "second" second (e-runtime-store--pending store))
    (puthash "third" third (e-runtime-store--pending store))
    (cl-letf
        (((symbol-function 'e-runtime-store--dispatch-next)
          (lambda (runtime)
            (pcase (cl-incf dispatches)
              (1
               (setf (e-runtime-store--active-request runtime) second)
               ;; The outer filter has already published LINE-TWO as its
               ;; remainder.  Reentry appends LINE-THREE and must consume each
               ;; continuation exactly once.
               (e-runtime-store--consume-output runtime (concat line-three "\n")))
              (2 (setf (e-runtime-store--active-request runtime) third))
              (3 nil)))))
      (e-runtime-store--consume-output store (substring line-one 0 half))
      (e-runtime-store--consume-output
       store (concat (substring line-one half) "\n" line-two "\n")))
    (should (= dispatches 3))
    (dolist (request (list first second third))
      (should (eq (e-runtime-store-request--state request) 'committed)))
    (should (string-empty-p (e-runtime-store--input-fragment store)))
    (let* ((response '(:id "exact-wire" :ok t :result (:value exact)))
           (ordinary (e-runtime-store-test--response-line response))
           (canonical-limit
            (string-bytes (e-runtime-store-codec-encode response)))
           (wire-limit (1+ (string-bytes ordinary)))
           (request (e-runtime-store-request--create
                     :id "exact-wire" :kind 'read :state 'submitted))
           (exact-store (e-runtime-store-test--submitted-store request))
           (split (/ (length ordinary) 2)))
      (let ((e-runtime-store-codec-protocol-canonical-byte-limit canonical-limit)
            (e-runtime-store-codec-protocol-wire-byte-limit wire-limit))
        (cl-letf (((symbol-function 'e-runtime-store--dispatch-next) #'ignore))
          (e-runtime-store--consume-output exact-store
                                           (substring ordinary 0 split))
          (e-runtime-store--consume-output
           exact-store (concat (substring ordinary split) "\n"))))
      (should (eq (e-runtime-store-request--state request) 'committed)))
    (let* ((request (e-runtime-store-request--create
                     :id "one-over" :kind 'read :state 'submitted))
           (overflow-store (e-runtime-store-test--submitted-store request))
           (ordinary (e-runtime-store-test--response-line
                      '(:id "one-over" :ok t :result (:value exact))))
           (wire-limit (1+ (string-bytes ordinary)))
           (over (concat ordinary "A"))
           (split (/ (length over) 2)))
      (let ((e-runtime-store-codec-protocol-wire-byte-limit wire-limit))
        (e-runtime-store--consume-output overflow-store (substring over 0 split))
        (e-runtime-store--consume-output
         overflow-store (concat (substring over split) "\n")))
      (should (eq (e-runtime-store-request--state request) 'failed))
      (should (eq (plist-get (cddr (e-runtime-store-request--error request))
                             :protocol-cause)
                  'response-frame-too-large)))))

(ert-deftest e-runtime-store-s92-c04-overflow-read-is-correlated-write-is-fatal ()
  "A large read gets a small typed response; a write acknowledgement cannot."
  (let* ((canonical-limit 2048)
         (wire-limit (e-runtime-store-codec-wire-byte-count canonical-limit))
         (read-request (e-runtime-store-request--create
                        :id "read-overflow" :kind 'read :state 'submitted
                        :body '(:op oversized-read)))
         (write-request (e-runtime-store-request--create
                         :id "write-overflow" :kind 'write :state 'submitted
                         :body '(:op committed-write)))
         (read-worker-request
          '(:id "read-overflow" :kind read :body (:op oversized-read)))
         (write-worker-request
          '(:id "write-overflow" :kind write :body (:op committed-write)))
         (large-result (list :content (make-string 8192 ?x)))
         read-wire)
    (let ((e-runtime-store-codec-protocol-canonical-byte-limit canonical-limit)
          (e-runtime-store-codec-protocol-wire-byte-limit wire-limit))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (e-runtime-store-worker--emit-response
           read-worker-request
           (list :id "read-overflow" :ok t :result large-result)))
        (setq read-wire (string-trim-right (buffer-string))))
      (let ((response (e-runtime-store--unpack read-wire)))
        (should-not (plist-get response :ok))
        (should (eq (plist-get response :error-symbol)
                    'e-runtime-store-response-too-large)))
      (let ((store (e-runtime-store-test--submitted-store read-request)))
        (cl-letf (((symbol-function 'e-runtime-store--dispatch-next) #'ignore))
          (e-runtime-store--consume-output store (concat read-wire "\n")))
        (should (eq (e-runtime-store-request--state read-request) 'failed))
        (should-not (e-runtime-store--unavailable store))
        (should (eq (car (e-runtime-store-request--error read-request))
                    'e-runtime-store-response-too-large)))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (should-error
           (e-runtime-store-worker--emit-response
            write-worker-request
            (list :id "write-overflow" :ok t :result large-result))
           :type 'e-runtime-store-codec-too-large)
          (should (= (buffer-size) 0)))))))

(ert-deftest e-runtime-store-s92-c04-worker-checkpoint-catalog-and-identity-bounds ()
  "Worker backstops projection limits and pages identities at real boundaries."
  (let ((directory (make-temp-file "e-runtime-store-c04-worker-" t)))
    (unwind-protect
        (progn
          (e-runtime-store-worker--open directory "c04-worker")
          (let* ((checkpoint '(:version 1 :payload "x"))
                 (checkpoint-limit
                  (string-bytes (e-runtime-store-codec-encode checkpoint)))
                 (catalog '((:id "catalog" :title "x")))
                 (catalog-limit
                  (string-bytes (e-runtime-store-codec-encode catalog))))
            (should (= (string-bytes
                        (e-runtime-store-codec-encode
                         '(:version 1 :payload "xx")))
                       (1+ checkpoint-limit)))
            (should (= (string-bytes
                        (e-runtime-store-codec-encode
                         '((:id "catalog" :title "xx"))))
                       (1+ catalog-limit)))
            (let ((e-runtime-store-worker-checkpoint-canonical-byte-limit
                   checkpoint-limit))
              (e-runtime-store-worker--checkpoint-put
               (list :session-id "checkpoint" :value checkpoint))
              (should-error
               (e-runtime-store-worker--checkpoint-put
                (list :session-id "checkpoint"
                      :value '(:version 1 :payload "xx")))
               :type 'e-runtime-store-codec-too-large))
            (let ((e-runtime-store-codec-catalog-canonical-byte-limit
                   catalog-limit))
              (e-runtime-store-worker--catalog-put (list :value catalog))
              (should-error
               (e-runtime-store-worker--catalog-put
                (list :value '((:id "catalog" :title "xx"))))
               :type 'e-runtime-store-codec-too-large))
            ;; The current observed 1,089,332-byte SQLite TEXT catalog is
            ;; 816,999 canonical bytes (an exact no-padding base64 multiple).
            ;; It remains below the 1 MiB derived-projection ceiling.
            (let* ((observed-sqlite-text-bytes 1089332)
                   (observed-canonical-bytes 816999)
                   (template '((:id "catalog" :payload "")))
                   (overhead
                    (string-bytes (e-runtime-store-codec-encode template)))
                   (observed-catalog
                    (list (list :id "catalog" :payload
                                (make-string
                                 (- observed-canonical-bytes overhead) ?x))))
                   (canonical
                    (e-runtime-store-codec-encode observed-catalog)))
              (should (= (string-bytes canonical) observed-canonical-bytes))
              (should (= (string-bytes (base64-encode-string canonical t))
                         observed-sqlite-text-bytes))
              (should (<= (string-bytes canonical)
                          e-runtime-store-codec-catalog-canonical-byte-limit))
              (should (plist-get
                       (e-runtime-store-worker--catalog-put
                        (list :value observed-catalog))
                       :revision))))
          (let ((payload (e-runtime-store-worker--sql-value '(:type "session"))))
            (dolist (session-id '("a" "b" "c"))
              (sqlite-execute
               e-runtime-store-worker--database
               "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
               (vector session-id 1 payload))))
          (let ((e-runtime-store-worker-session-id-page-row-limit 2)
                (e-runtime-store-worker-session-id-page-byte-limit 2))
            (let ((first-page
                   (e-runtime-store-worker--session-id-page
                    '(:cursor nil :limit 2))))
              ;; Three short identities exercise the exact two-row page plus
              ;; one lookahead; the third never leaks into the first result.
              (should (equal (plist-get first-page :ids) '("a" "b")))
              (should (= (plist-get first-page :byte-count) 2))
              (should (equal (plist-get first-page :next) "b"))
              (should (equal
                       (plist-get
                        (e-runtime-store-worker--session-id-page
                         '(:cursor "b" :limit 2))
                        :ids)
                       '("c"))))
          (let ((e-runtime-store-worker-session-id-page-row-limit 3)
                (e-runtime-store-worker-session-id-page-byte-limit 2))
            (let ((byte-page
                   (e-runtime-store-worker--session-id-page
                    '(:cursor nil :limit 3))))
              ;; The exact two-byte page admits A and B; C is the one-byte
              ;; overflow and must be deferred to the cursor successor.
              (should (equal (plist-get byte-page :ids) '("a" "b")))
              (should (= (plist-get byte-page :byte-count) 2))
              (should (equal (plist-get byte-page :next) "b"))
              (should (equal
                       (plist-get
                        (e-runtime-store-worker--session-id-page
                         '(:cursor "b" :limit 3))
                        :ids)
                       '("c"))))))
      (e-runtime-store-worker--close)
      (delete-directory directory t)))))

(provide 'e-runtime-store-test)

;;; e-runtime-store-test.el ends here
