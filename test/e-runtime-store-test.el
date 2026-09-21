;;; e-runtime-store-test.el --- SQLite runtime-store adapter scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'seq)
(require 'e-runtime-store)
(require 'e-runtime-store-worker)
(require 'e-runtime-store-session-worker)
(require 'e-session-query)
(require 'e-task-queue)
(require 'e-task-storage-sqlite)

(cl-defmacro e-runtime-store-test--with-store ((store directory) &rest body)
  "Run BODY with STORE owning a disposable DIRECTORY."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-runtime-store-test-" t))
          (,store (e-runtime-store-open ,directory)))
     ;; Existing DP1--DP4 owner scenarios are about the already-open adapter.
     ;; DP5A separately proves that ordinary submission does not require this
     ;; explicit batch-test observation.
     (e-runtime-store-test--wait-ready ,store)
     (unwind-protect (progn ,@body)
       (ignore-errors (e-runtime-store-close ,store))
       (e-runtime-store-test--cancel-store-timers ,store)
       (e-runtime-store-test--assert-no-store-timers ,store)
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

(defun e-runtime-store-test--wait-terminal-without-await (request)
  "Let normal timers and filters settle REQUEST without invoking await."
  (let ((deadline (+ (float-time) 5.0)))
    (while (and (memq (e-runtime-store-request--state request)
                      '(queued submitted))
                (< (float-time) deadline))
      (sit-for 0.01))))

(defun e-runtime-store-test--fire-current-scheduler (store)
  "Synchronously run STORE's current timer generation in a deterministic test."
  (when (timerp (e-runtime-store--scheduler-timer store))
    (cancel-timer (e-runtime-store--scheduler-timer store)))
  (e-runtime-store--scheduler-fired
   store (e-runtime-store--scheduler-generation store)))

(defun e-runtime-store-test--cancel-store-timers (store)
  "Cancel every private timer owned by disposable STORE, including lost refs."
  (dolist (timer (list (e-runtime-store--scheduler-timer store)
                       (e-runtime-store--notification-timer store)
                       (e-runtime-store--close-finalizer-timer store)))
    (when (timerp timer) (cancel-timer timer)))
  ;; A test may deliberately clear a struct field before exercising a stale
  ;; callback.  Find that callback by its private function and exact STORE
  ;; argument so cleanup still owns it rather than leaving a batch timer live.
  (dolist (timer (append (copy-sequence timer-list)
                         (copy-sequence timer-idle-list)))
    (when (and (timerp timer)
               (memq (timer--function timer)
                     e-runtime-store-test--runtime-timer-functions)
               (memq store (timer--args timer)))
      (cancel-timer timer)))
  (setf (e-runtime-store--scheduler-timer store) nil
        (e-runtime-store--notification-timer store) nil
        (e-runtime-store--close-finalizer-timer store) nil))

(defun e-runtime-store-test--assert-no-store-timers (store)
  "Assert STORE retains no live or stale timer after fixture teardown."
  (dolist (timer (list (e-runtime-store--scheduler-timer store)
                       (e-runtime-store--notification-timer store)
                       (e-runtime-store--close-finalizer-timer store)))
    (should-not (timerp timer)))
  (should-not
   (seq-some
    (lambda (timer)
      (and (memq (timer--function timer)
                 e-runtime-store-test--runtime-timer-functions)
           (memq store (timer--args timer))))
    (append timer-list timer-idle-list))))

(defconst e-runtime-store-test--runtime-timer-functions
  '(e-runtime-store--scheduler-fired
    e-runtime-store--drain-terminal-notifications
    e-runtime-store--finalize-close)
  "Private timer callbacks that must never survive a focused fixture.")

(defun e-runtime-store-test--wait-until (predicate)
  "Drive the isolated event loop until PREDICATE succeeds."
  (let ((deadline (+ (float-time) 5.0)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (sit-for 0.01))
    (should (funcall predicate))))

(defun e-runtime-store-test--runtime-timers ()
  "Return live runtime-store timers even when an owner cleared its field."
  (seq-filter
   (lambda (timer)
     (and (timerp timer)
          (memq (timer--function timer)
                e-runtime-store-test--runtime-timer-functions)))
   (append timer-list timer-idle-list)))

(defun e-runtime-store-test--assert-no-runtime-timer-callbacks ()
  "Assert focused fixtures left no private runtime-store timer callback live."
  (should-not (e-runtime-store-test--runtime-timers)))

(defun e-runtime-store-test--assert-finalized-close (store process callback-count)
  "Assert STORE's asynchronous close reached its fully released terminal state."
  (should (e-runtime-store--closed store))
  (should (= callback-count 1))
  (should-not (e-runtime-store--process store))
  (should-not (e-runtime-store--stderr-buffer store))
  (should-not (e-runtime-store--active-request store))
  (should-not (e-runtime-store--starting-request store))
  (should-not (e-runtime-store--recovering-request store))
  (should-not (e-runtime-store--recovery-cause store))
  (should-not (e-runtime-store--closing-request store))
  (should-not (process-live-p process))
  (e-runtime-store-test--assert-no-store-timers store))

(defun e-runtime-store-test--wait-ready (store)
  "Explicitly observe STORE's asynchronous cold-open phase in legacy tests."
  (when-let* ((request (e-runtime-store--active-request store))
              ((eq (e-runtime-store-request--kind request) 'open)))
    ;; A fixture fault must reach its `unwind-protect' teardown promptly.
    (e-runtime-store-await store request 5.0)))

(defun e-runtime-store-test--query-state (session-id journal-position)
  "Return a minimal complete v6 query row for SESSION-ID at POSITION."
  (list :session-id session-id :name session-id :summary nil :metadata nil
        :created-at "2026-09-07T00:00:00Z"
        :updated-at "2026-09-07T00:00:00Z"
        :last-message-at nil :latest-assistant-marker nil :message-count 0
        :current-branch nil :turn-options nil
        :current-head-id (concat session-id "-root")
        :root-event-id (concat session-id "-root")
        :current-context-generation-id nil :board-id nil :principal nil
        :association-role nil :routing-policy nil :root-p t
        :board-output-sequence 0 :board-activity-sequence 0
        :journal-position journal-position))

(defun e-runtime-store-test--session-append-body
    (session-id record &optional journal-position)
  "Return a valid v6 SESSION-ID append command for transport tests."
  (let* ((position (or journal-position 1))
         (record (copy-tree record)))
    (setq record (plist-put record :session-id session-id)
          record (plist-put record :type
                            (if (= position 1) 'session 'activity-event))
          record (plist-put record :id
                            (if (= position 1)
                                (concat session-id "-root")
                              (format "%s-event-%d" session-id position)))
          record (plist-put record :timestamp "2026-09-07T00:00:00Z"))
    (list :op 'session-append
          :session-id session-id
          :record record
          :query-delta
          (e-runtime-store-test--query-state session-id position))))

(defun e-runtime-store-test--session-append-batch-body
    (session-id records &optional journal-position)
  "Return a valid v6 SESSION-ID batch append command for transport tests."
  (let* ((final-position (or journal-position (length records)))
         (first-position (1+ (- final-position (length records))))
         (position (1- first-position))
         (records
          (mapcar
           (lambda (record)
             (setq position (1+ position))
             (plist-get
              (e-runtime-store-test--session-append-body
               session-id record position)
              :record))
           (append records nil))))
    (list :op 'session-append-batch
          :session-id session-id
          :records (vconcat records)
          :query-delta
          (e-runtime-store-test--query-state session-id final-position))))

(defun e-runtime-store-test--resource-put-body (lineage-id path content)
  "Return a current durable write command independent of session aggregates."
  (list :op 'resource-put :lineage-id lineage-id :session-id lineage-id
        :path path :content content))

(defun e-runtime-store-test--parent (boot &optional pid process-start)
  "Return a focused worker parent identity with BOOT and optional overrides."
  (list :boot boot :pid (or pid (emacs-pid))
        :process-start
        (or process-start
            (e-runtime-store-ownership--current-process-start))))

(defun e-runtime-store-test--separate-batch-parent-identity ()
  "Return a real scheduler parent identity from a fresh batch Emacs process."
  (let* ((root (expand-file-name ".." e-runtime-store-test--source-directory))
         (core (expand-file-name "lisp/core" root))
         (emacs (expand-file-name invocation-name invocation-directory))
         (form
          "(progn (require 'e-runtime-store) (prin1 (e-runtime-store--parent-identity)))"))
    (with-temp-buffer
      (unless (zerop (call-process emacs nil t nil
                                   "--batch" "-Q" "-L" core "--eval" form))
        (error "Fresh batch Emacs could not produce runtime-store parent identity"))
      ;; Byte-compiler warnings can share the captured stream; the emitted
      ;; canonical plist is the final boot-identity form, not warning text.
      (goto-char (point-max))
      (unless (search-backward "(:boot " nil t)
        (error "Fresh batch Emacs omitted runtime-store parent identity"))
      (let ((read-eval nil))
        (read (current-buffer))))))

(ert-deftest e-runtime-store-s92-parent-boot-is-host-stable-across-emacsen ()
  "Separate Emacs parents share a host boot ID but not process identity."
  (let ((first (e-runtime-store-test--separate-batch-parent-identity))
        (second (e-runtime-store-test--separate-batch-parent-identity)))
    (should (equal (plist-get first :boot) (plist-get second :boot)))
    (should-not (= (plist-get first :pid) (plist-get second :pid)))
    (should-not (equal (plist-get first :process-start)
                       (plist-get second :process-start)))))

(defmacro e-runtime-store-test--with-worker-fault (point &rest body)
  "Run BODY with a disposable one-shot worker fault at POINT."
  (declare (indent 1) (debug (form body)))
  `(let* ((marker (make-temp-file "e-runtime-store-fault-"))
          (process-environment
           (cons (concat "E_RUNTIME_STORE_TEST_FAULT=" ,point)
                 (cons (concat "E_RUNTIME_STORE_TEST_FAULT_ONCE_FILE=" marker)
                       process-environment))))
     (delete-file marker)
     (unwind-protect (progn ,@body)
       (when (file-exists-p marker) (delete-file marker)))))

(ert-deftest e-runtime-store-codec-round-trips-exact-tagged-values ()
  "Exact nested Lisp values survive and unsupported live values fail early."
  (let* ((map (make-hash-table :test 'equal))
         (value (list nil t :json-false :json-null 'symbol :keyword "λ🧵" 42 1.5
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
                         (list :nested [nil t :json-false :json-null "leaf"])))
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
    (let* ((root (list :type "session" :session-id "s" :id "root"
                       :timestamp "2026-09-06T00:00:00Z" :value 'one
                       :journal-position 1))
           (root-state (e-session-query-delta-from-record nil root))
           (message (list :type "message" :session-id "s" :id "message"
                          :parent-id "root" :timestamp "2026-09-06T00:00:01Z"
                          :message '(:role user :content "two") :value 'two
                          :journal-position 2))
           (message-state
            (e-session-query-delta-from-record root-state message)))
      (should (= (plist-get
                  (e-runtime-store-call
                   store 'write (list :op 'session-append :session-id "s"
                                      :record root :query-delta root-state))
                  :position)
                 1))
      (should (= (plist-get
                  (e-runtime-store-call
                   store 'write
                   (list :op 'session-append :session-id "s"
                         :record message :query-delta message-state))
                  :position)
                 2)))
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

(ert-deftest e-runtime-store-s92-held-write-does-not-block-independent-read ()
  "A submitted writer cannot monopolize the prewarmed read-only transport."
  (let* ((directory (make-temp-file "e-runtime-store-read-lane-" t))
         (stall-directory (make-temp-file "e-runtime-store-read-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         store held read)
    (unwind-protect
        (progn
          (setq store (e-runtime-store-open directory))
          (e-runtime-store-test--wait-ready store)
          (e-runtime-store-call
           store 'write
           (e-runtime-store-test--session-append-body "s" '(:value one)))
          (write-region "hold" nil
                        (expand-file-name "session-append.hold" stall-directory)
                        nil 'silent)
          (setq held
                (e-runtime-store-submit
                 store 'write
                 (e-runtime-store-test--session-append-body
                  "s" '(:value two) 2)))
          (let ((deadline (+ (float-time) 2.0))
                (ready (expand-file-name "session-append.ready"
                                         stall-directory)))
            (while (and (not (file-exists-p ready))
                        (< (float-time) deadline))
              (sit-for 0.01))
            (should (file-exists-p ready)))
          (setq read
                (e-runtime-store-submit
                 store 'read
                 '(:op session-record-page :session-id "s" :after 0 :limit 8)))
          (should (eq (e-runtime-store-request--owner-store read)
                      (e-runtime-store--read-client store)))
          (let ((page (e-runtime-store-await store read 1.0)))
            (should (= (length (plist-get page :records)) 1)))
          (should (eq (e-runtime-store-request--state held) 'submitted))
          (write-region "release" nil
                        (expand-file-name "session-append.release" stall-directory)
                        nil 'silent)
          (e-runtime-store-await store held 2.0)
          (should
           (= (length
               (plist-get
                (e-runtime-store-call
                 store 'read
                 '(:op session-record-page :session-id "s" :after 0 :limit 8))
                :records))
              2)))
      (when store (ignore-errors (e-runtime-store-close store)))
      (delete-directory directory t)
      (delete-directory stall-directory t))))

(ert-deftest e-runtime-store-s2-existing-v7-open-does-not-rewrite-indexes ()
  "Ordinary current-schema open verifies without running cleanup DDL."
  (let ((directory (make-temp-file "e-runtime-store-index-test-" t))
        store)
    (cl-labels
        ((redundant-index-p ()
           (let ((database
                  (sqlite-open (expand-file-name "store.sqlite3" directory))))
             (unwind-protect
                 (car (sqlite-select
                       database
                       (concat
                        "SELECT 1 FROM sqlite_master WHERE type='index' "
                        "AND name='session_records_position'")))
               (sqlite-close database)))))
      (unwind-protect
          (progn
            (setq store (e-runtime-store-open directory))
            (e-runtime-store-test--wait-ready store)
            (should-not (redundant-index-p))
            (e-runtime-store-close store)
            (let ((database
                   (sqlite-open (expand-file-name "store.sqlite3" directory))))
              (unwind-protect
                  (sqlite-execute
                   database
                   (concat
                    "CREATE INDEX session_records_position "
                    "ON session_records(session_id, position)"))
                (sqlite-close database)))
            (should (redundant-index-p))
            (setq store (e-runtime-store-open directory))
            (e-runtime-store-test--wait-ready store)
            (should (redundant-index-p)))
        (when store
          (ignore-errors (e-runtime-store-close store))
          (e-runtime-store-test--cancel-store-timers store))
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-s2-status-is-constant-cost-and-integrity-explicit ()
  "Ordinary status performs no SQLite scan; explicit integrity still does."
  (let ((e-runtime-store-worker--database 'sentinel)
        selects)
    (cl-letf (((symbol-function 'sqlite-select)
               (lambda (_database sql &rest _arguments)
                 (push sql selects)
                 (list (list "ok")))))
      (let ((status (e-runtime-store-worker--read '(:op status))))
        (should (= (plist-get status :schema-version) 8))
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

(ert-deftest e-runtime-store-s92-status-exposes-current-and-terminal-latencies ()
  "Status separates FIFO wait, dispatch-to-settlement, and total latency."
  (let* ((active (e-runtime-store-request--create
                  :id "latency:w:1" :kind 'write
                  :body '(:op resource-put) :state 'submitted
                  :admitted-at 1.0 :submitted-at 3.0 :first-submitted-at 3.0))
         (queued (e-runtime-store-request--create
                  :id "latency:r:2" :kind 'read :body '(:op resource-get)
                  :state 'queued :admitted-at 4.0))
         (store (e-runtime-store--create
                 :runtime-id "latency" :active-request active
                 :client-queue (list queued)
                 :pending (make-hash-table :test 'equal))))
    (puthash (e-runtime-store-request--id active) active
             (e-runtime-store--pending store))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) 8.0))
              ((symbol-function 'e-runtime-store--schedule) #'ignore))
      (let* ((latencies (plist-get (e-runtime-store-status store) :latencies))
             (active-status (plist-get latencies :active))
             (queued-status (plist-get latencies :oldest-queued)))
        (should (eq (plist-get latencies :unit) 'milliseconds))
        (should (equal (plist-get active-status :operation) 'resource-put))
        (should (eq (plist-get active-status :phase) 'submitted))
        (should (= (plist-get active-status :phase-age-ms) 5000.0))
        (should (= (plist-get active-status :total-age-ms) 7000.0))
        (should (equal (plist-get queued-status :operation) 'resource-get))
        (should (= (plist-get queued-status :phase-age-ms) 4000.0)))
      (e-runtime-store--settle
       store active '(:id "latency:w:1" :ok t :result (:revision 1)))
      ;; Recording is terminal and exact-once even if another cleanup edge sees
      ;; the same request after its correlated response.
      (e-runtime-store--record-request-latency store active)
      (let* ((latencies (plist-get (e-runtime-store-status store) :latencies))
             (sample (car (plist-get latencies :recent))))
        (should (= (plist-get latencies :recent-count) 1))
        (should (equal (plist-get sample :operation) 'resource-put))
        (should (eq (plist-get sample :outcome) 'committed))
        (should (= (plist-get sample :queue-ms) 2000.0))
        (should (= (plist-get sample :dispatch-to-settlement-ms) 5000.0))
        (should (= (plist-get sample :total-ms) 7000.0))))))

(ert-deftest e-runtime-store-s92-terminal-latency-history-is-bounded ()
  "Terminal observations retain only the configured recent scalar samples."
  (let ((store (e-runtime-store--create :runtime-id "latency-cap"))
        (e-runtime-store--latency-sample-capacity 2))
    (cl-loop for index from 1 to 3
             do (let ((request
                       (e-runtime-store-request--create
                        :id (format "latency-cap:%d" index) :kind 'write
                        :body (list :op (intern (format "operation-%d" index)))
                        :state 'failed :admitted-at 0.0
                        :submitted-at 1.0 :first-submitted-at 1.0)))
                  (cl-letf (((symbol-function 'float-time)
                             (lambda (&optional _time) (+ 1.0 index))))
                    (e-runtime-store--record-request-latency store request))))
    (let* ((latencies (plist-get (e-runtime-store-status store) :latencies))
           (samples (plist-get latencies :recent)))
      (should (= (plist-get latencies :recent-count) 2))
      (should (equal (mapcar (lambda (sample)
                              (plist-get sample :operation))
                            samples)
                     '(operation-3 operation-2))))))

(ert-deftest e-runtime-store-s2-rejects-a-second-live-runtime ()
  "One live worker exclusively owns one physical database."
  (e-runtime-store-test--with-store (store directory)
    (let ((contender (e-runtime-store-open directory :runtime-id "second")))
      (unwind-protect
          (should-error (e-runtime-store-test--wait-ready contender)
                        :type 'e-runtime-store-owner-active)
        ;; The contender never became a durable owner.  Retire its failed
        ;; transport locally instead of enqueueing a graceful close to a
        ;; worker that rejected open; that close can only wait for timeout and
        ;; race the real owner's fixture teardown.
        (ignore-errors (e-runtime-store-shutdown contender))))
    (should (file-exists-p
             (expand-file-name "store.sqlite3.owner" directory)))
    (should (e-runtime-store-live-p store))))

(ert-deftest e-runtime-store-s2-idle-worker-loss-restarts-lazily ()
  "Worker loss while idle restarts on the next submission."
  (e-runtime-store-test--with-store (store directory)
    (let ((runtime-id (e-runtime-store--runtime-id store)))
      (e-runtime-store-call
       store 'write
       (e-runtime-store-test--session-append-body "s" '(:value one)))
    (delete-process (e-runtime-store--process store))
    (while (e-runtime-store--live-p store)
      (accept-process-output nil 0.01))
      (should-not (plist-get (e-runtime-store-status store) :unavailable))
      (should (= (plist-get
                  (e-runtime-store-call
                   store 'write
                   (e-runtime-store-test--session-append-body
                    "s" '(:value two) 2))
                  :high-water)
                 2))
      (let ((page (e-runtime-store-call
                   store 'read '(:op session-record-page :session-id "s"))))
        (should (= (length (plist-get page :records)) 2)))
      (should (equal runtime-id (e-runtime-store--runtime-id store))))))

(ert-deftest e-runtime-store-s92-worker-exit-before-write-response-recovers-once ()
  "Worker exit before a write acknowledgement resolves the same receipt once."
  (let* ((directory (make-temp-file "e-runtime-store-write-exit-" t))
         (store (e-runtime-store-open directory))
         (process (e-runtime-store--process store))
         (captured "")
         request)
    (unwind-protect
        (progn
          (e-runtime-store-test--wait-ready store)
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
                 (e-runtime-store-test--session-append-body
                  "write-exit" '(:value once))))
          (e-runtime-store-test--wait-terminal request)
          (should (eq (e-runtime-store-request--state request) 'committed))
          (should-not (plist-get (e-runtime-store-status store) :unavailable))
          (e-runtime-store-close store)
          (setq store (e-runtime-store-open directory))
          (let ((page (e-runtime-store-call
                       store 'read
                       '(:op session-record-page :session-id "write-exit"))))
            (should (= (length (plist-get page :records)) 1))))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-worker-exit-before-read-response-retries-once ()
  "Worker exit before a read response retries after one replacement."
  (e-runtime-store-test--with-store (store directory)
    (e-runtime-store-call
     store 'write
     (e-runtime-store-test--session-append-body
      "read-loss" '(:value present)))
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
      (should (eq (e-runtime-store-request--state request) 'committed))
      (should-not (plist-get (e-runtime-store-status store) :unavailable)))))

(ert-deftest e-runtime-store-s92-timeout-recovers-committed-write-once ()
  "A lost submitted acknowledgement resolves through its durable receipt."
  (let* ((directory (make-temp-file "e-runtime-store-timeout-" t))
         (store (e-runtime-store-open directory))
         (process (e-runtime-store--process store))
         (body (e-runtime-store-test--session-append-body
                "timeout" '(:value maybe-committed)))
         request)
    (unwind-protect
        (progn
          (e-runtime-store-test--wait-ready store)
          ;; Discard the complete acknowledgement while leaving the worker
          ;; alive, reproducing an ambiguous response timeout deterministically.
          (set-process-filter process #'ignore)
          (setq request
            (e-runtime-store-submit
             store 'write body))
          (e-runtime-store-test--fire-current-scheduler store)
          ;; Recovery may only replay the canonical preflight frame, never
          ;; this caller-owned mutable list.
          (plist-put body :record '(:value caller-mutated))
          ;; Awaiters only observe.  Drive the overdue phase through the
          ;; runtime-owned scheduler, then let its replacement settle.
          (setf (e-runtime-store-request--submitted-at request)
                (- (float-time) e-runtime-store-request-timeout 1))
          (e-runtime-store-test--fire-current-scheduler store)
          (e-runtime-store-test--wait-terminal request)
          (let ((result (e-runtime-store-request--result request)))
            (should (= (plist-get result :high-water) 1))
            (should (eq (e-runtime-store-request--state request) 'committed))
            (should-not (process-live-p process))
            (should-not (plist-get (e-runtime-store-status store) :unavailable)))
          (let ((page
                 (e-runtime-store-call
                  store 'read
                  '(:op session-record-page :session-id "timeout"))))
            (should (= (length (plist-get page :records)) 1))
            (should (eq (plist-get (plist-get (car (plist-get page :records))
                                            :value)
                                   :value)
                        'maybe-committed)))
          (should (= (plist-get
                      (e-runtime-store-call
                       store 'write
                       (e-runtime-store-test--session-append-body
                        "timeout" '(:value after-recovery) 2))
                      :high-water)
                     2)))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-live-competitor-cannot-steal-uncertain-receipt ()
  "Only the live original parent may replay its committed lost acknowledgement."
  (e-runtime-store-test--with-worker-fault "after-commit"
    (let* ((directory (make-temp-file "e-runtime-store-live-replay-" t))
           (database-file (expand-file-name "store.sqlite3" directory))
           (store (e-runtime-store-open directory))
           (process (e-runtime-store--process store))
           (other-parent (e-runtime-store-test--separate-batch-parent-identity))
           request)
      (unwind-protect
          (progn
            (e-runtime-store-test--wait-ready store)
            ;; Keep the test in control until the competing parent has tried
            ;; to claim the receipt; normal await performs the later recovery.
            (set-process-sentinel process #'ignore)
            (setq request
                  (e-runtime-store-submit
                   store 'write
                   (e-runtime-store-test--session-append-body
                    "live-replay" '(:value committed-once))))
            (let ((deadline (+ (float-time) 5.0)))
              (while (and (process-live-p process) (< (float-time) deadline))
                (sleep-for 0.01)))
            (should-not (process-live-p process))
            (let ((db (sqlite-open database-file)))
              (unwind-protect
                  (should (= (e-runtime-store-worker--column
                              (car (sqlite-select db
                                                  "SELECT COUNT(*) FROM runtime_store_receipts")) 0)
                             1))
                (sqlite-close db)))
            (should (equal (plist-get other-parent :boot)
                           (plist-get (e-runtime-store--parent-identity) :boot)))
            (should-not (= (plist-get other-parent :pid) (emacs-pid)))
            (should-error
             (e-runtime-store-worker--open directory "competing-parent" other-parent)
             :type 'e-runtime-store-parent-active)
            (e-runtime-store-worker--close)
            ;; Same runtime and live parent retain the state row and replay
            ;; the receipt, yielding the original result without duplication.
            ;; The test deliberately fenced the real sentinel above, so model
            ;; that external scheduler event directly rather than asking await
            ;; to drive recovery.
            (e-runtime-store--worker-exited store)
            (e-runtime-store-test--wait-terminal-without-await request)
            (should (= (plist-get (e-runtime-store-request--result request)
                                  :high-water)
                       1))
            (should (= (length (plist-get
                                (e-runtime-store-call
                                 store 'read
                                 '(:op session-record-page :session-id "live-replay"))
                                :records))
                       1))
            ;; The following canonical write carries the acknowledgement
            ;; watermark and retires the now-resolved uncertain receipt.
            (should (= (plist-get
                        (e-runtime-store-call
                         store 'write
                         (e-runtime-store-test--session-append-body
                          "live-replay" '(:value after-replay) 2))
                        :high-water)
                       2))
            (let ((db (sqlite-open database-file)))
              (unwind-protect
                  (should (= (e-runtime-store-worker--column
                              (car (sqlite-select db
                                                  "SELECT COUNT(*) FROM runtime_store_receipts WHERE request_id=?"
                                                  (vector (e-runtime-store-request--id request)))) 0)
                             0))
                (sqlite-close db))))
        (ignore-errors (e-runtime-store-close store))
        (e-runtime-store-worker--close)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-s92-acid-worker-fault-sides-replay-once ()
  "Pre-COMMIT, post-COMMIT, and formed-response loss resolve one write once."
  (dolist (point '("before-commit" "after-commit" "after-response-formation"))
    (e-runtime-store-test--with-worker-fault point
      (e-runtime-store-test--with-store (store _directory)
        (let ((request
               (e-runtime-store-submit
                store 'write
                (e-runtime-store-test--session-append-body
                 point (list :value point)))))
          ;; No awaiter drives this recovery: normal scheduler timers and
          ;; filters must replay the exact immutable write by themselves.
          (e-runtime-store-test--wait-terminal-without-await request)
          (should (eq (e-runtime-store-request--state request) 'committed))
          (let ((result (e-runtime-store-request--result request)))
          (should (= (plist-get result :high-water) 1))
          (let ((page (e-runtime-store-call
                       store 'read (list :op 'session-record-page :session-id point))))
            (should (= (length (plist-get page :records)) 1)))
          (should-not (plist-get (e-runtime-store-status store) :unavailable))))))))

(ert-deftest e-runtime-store-s92-c04-retained-bounded-frame-recovers ()
  "A retained legal batch is replayed only from its bounded frame."
  (let ((e-runtime-store-request-timeout 20.0)
        (payload (make-string (* 512 1024) ?x)))
    (e-runtime-store-test--with-worker-fault "after-commit"
      (e-runtime-store-test--with-store (store _directory)
        (let ((result
               (e-runtime-store-call
                store 'write
                (e-runtime-store-test--session-append-batch-body
                 "c04-recovery"
                 (list (list :content payload) (list :content payload))))))
          (should (= (plist-get result :high-water) 2))
          ;; Do not materialize the large read page in the measurement
          ;; process: the production proof here is retained-frame recovery,
          ;; followed by an ordinary, independent durable write.
          (should (= (plist-get
                      (e-runtime-store-call
                       store 'write
                       (e-runtime-store-test--session-append-body
                        "c04-health" '(:value after-recovery)))
                      :high-water)
                     1))
          (should-not (plist-get (e-runtime-store-status store) :unavailable)))))))

(ert-deftest e-runtime-store-s92-repeated-worker-stall-partitions-owner ()
  "A second fault preserves the first cause and fences one owner locally."
  (let ((process-environment
         (cons "E_RUNTIME_STORE_TEST_FAULT=before-commit" process-environment)))
    (e-runtime-store-test--with-store (store _directory)
      (let ((active (e-runtime-store--submit-owned
                     store 'write
                     (e-runtime-store-test--session-append-body
                      "exhaust" '(:value once))
                     '(session . "exhaust")))
            queued first)
        (setq queued (e-runtime-store--submit-owned
                      store 'write
                      (e-runtime-store-test--session-append-body
                       "exhaust" '(:value queued))
                      '(session . "exhaust")))
        (e-runtime-store-test--wait-terminal-without-await active)
        (setq first (e-runtime-store-request--error active))
        (should (eq (e-runtime-store-request--state active) 'failed))
        (should (eq (e-runtime-store-request--state queued) 'failed))
        (should (eq (car (e-runtime-store-request--error queued))
                    'e-runtime-store-persistence-suspect))
        (should-not (plist-get (e-runtime-store-status store) :unavailable))
        (should (equal (plist-get first :request-id)
                       (e-runtime-store-request--id active)))
        (should-error
         (e-runtime-store--submit-owned
          store 'write '(:op session-delete :session-id "exhaust")
          '(session . "exhaust"))
         :type 'e-runtime-store-persistence-suspect)))))

(ert-deftest e-runtime-store-s92-repeated-corruption-exhausts-with-first-cause ()
  "A second malformed response cannot replace the first recovery diagnosis."
  (let* ((first '(e-runtime-store-error "first malformed response"
                                      :cause malformed-response))
         (second '(e-runtime-store-error "second malformed response"
                                       :cause malformed-response))
         (request (e-runtime-store-request--create
                   :id "corrupt:1" :kind 'write :body '(:op session-append)
                   :frame "canonical-frame" :state 'submitted))
         (pending (make-hash-table :test 'equal))
         (store (e-runtime-store--create
                 :directory "/tmp/" :database-file "/tmp/store.sqlite3"
                 :runtime-id "corruption" :pending pending
                 :active-request request :recovery-attempt 1
                 :recovery-cause first)))
    (puthash (e-runtime-store-request--id request) request pending)
    (e-runtime-store--recover-or-fail store second)
    (should (eq (e-runtime-store-request--state request) 'failed))
    (should (equal (e-runtime-store-request--error request) first))
    (should-not (plist-get (e-runtime-store-status store) :unavailable))
    (should (equal (e-runtime-store--last-error store) first))))

(ert-deftest e-runtime-store-s92-replacement-open-loss-settles-original-and-queue ()
  "A replacement open loss consumes the one attempt with the original cause."
  (let* ((directory (make-temp-file "e-runtime-store-replacement-open-" t))
         (store (e-runtime-store-open directory))
         (process (e-runtime-store--process store))
         (marker (make-temp-file "e-runtime-store-open-fault-"))
         active queued)
    (delete-file marker)
    (unwind-protect
        (progn
          (e-runtime-store-test--wait-ready store)
          ;; Suppress the original acknowledgement, then let the worker exit;
          ;; only the replacement subprocess inherits this open-loss seam.
          (set-process-filter process (lambda (_worker _text)))
          (setq active
                (e-runtime-store-submit
                 store 'write
                 (e-runtime-store-test--session-append-body
                  "replacement-open" '(:value once))))
          (setq queued
                (e-runtime-store-submit
                 store 'write
                 (e-runtime-store-test--resource-put-body
                  "replacement-open" "queued" "queued")))
          (e-runtime-store-test--fire-current-scheduler store)
          (let ((process-environment
                 (cons "E_RUNTIME_STORE_TEST_FAULT=after-open-response-formation"
                       (cons (concat "E_RUNTIME_STORE_TEST_FAULT_ONCE_FILE=" marker)
                             process-environment))))
            (delete-process process)
            (e-runtime-store-test--wait-terminal active))
          (should (eq (e-runtime-store-request--state active) 'failed))
          (e-runtime-store-test--wait-terminal-without-await queued)
          (should (eq (e-runtime-store-request--state queued) 'committed))
          (should (= (e-runtime-store--recovery-attempt store) 0))
          (should (equal (plist-get (cddr (e-runtime-store-request--error active))
                                    :request-id)
                         (e-runtime-store-request--id active)))
          (should-not (plist-get (e-runtime-store-status store) :unavailable)))
      (when (file-exists-p marker) (delete-file marker))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-replacement-open-contention-keeps-first-cause ()
  "Replacement ownership failure settles owned work once with the initial cause."
  (let* ((active (e-runtime-store-request--create
                  :id "replace:active" :kind 'write :body '(:op resource-put)
                  :frame "exact" :state 'submitted))
         (queued (e-runtime-store-request--create
                  :id "replace:queued" :kind 'write :body '(:op resource-put)
                  :state 'queued))
         (pending (make-hash-table :test 'equal))
         (store (e-runtime-store--create
                 :directory "/tmp/" :runtime-id "replacement-contention"
                 :pending pending :active-request active :client-queue (list queued)))
         (first (list 'e-runtime-store-timeout "first" :request-id "replace:active")))
    (puthash (e-runtime-store-request--id active) active pending)
    (cl-letf (((symbol-function 'e-runtime-store--fence-worker) #'ignore)
              ((symbol-function 'e-runtime-store--schedule) #'ignore)
              ((symbol-function 'e-runtime-store--start-process)
               (lambda (candidate) (setf (e-runtime-store--process candidate) 'replacement)))
              ((symbol-function 'e-runtime-store--ensure-worker-open)
               (lambda (_store)
                 (signal 'e-runtime-store-parent-active '("replacement contention")))))
      (e-runtime-store--recover-active store first)
      (e-runtime-store--scheduler-fired
       store (e-runtime-store--scheduler-generation store)))
    (should (eq (e-runtime-store-request--state active) 'failed))
    (should (eq (e-runtime-store-request--state queued) 'queued))
    (should (equal (e-runtime-store-request--error active) first))
    (should-not (e-runtime-store-request--error queued))
    (should (= (e-runtime-store--recovery-attempt store) 0))
    (should-not (plist-get (e-runtime-store-status store) :unavailable))
    (e-runtime-store--fail-all store '(e-runtime-store-error "cleanup"))
    (e-runtime-store--drain-terminal-notifications store t)))

(ert-deftest e-runtime-store-s92-close-recovery-retains-one-retired-state ()
  "Close loss on either COMMIT side resolves its original retirement identity."
  (dolist (point '("before-retirement-commit" "after-retirement-commit"
                   "after-response-formation"))
    (e-runtime-store-test--with-worker-fault point
      (let* ((directory (make-temp-file "e-runtime-store-close-recovery-" t))
             (store (e-runtime-store-open directory))
             (database (expand-file-name "store.sqlite3" directory))
             close-id)
        (unwind-protect
            (progn
              (let ((close-request (e-runtime-store--close-start store)))
                (setq close-id (e-runtime-store-request--id close-request))
                ;; The close handle is returned while cold open/retirement are
                ;; still pending; no caller await drives either acknowledgement.
                (should (eq (e-runtime-store-request--state close-request) 'queued))
                (e-runtime-store-test--wait-terminal-without-await close-request)
                (should (eq (e-runtime-store-request--state close-request)
                            'committed))
                (sit-for 0.01))
              (setq store nil)
              (let ((db (sqlite-open database)))
                (unwind-protect
                    (progn
                      (let ((state (car (sqlite-select
                                         db
                                         "SELECT retired,retirement_request_id FROM runtime_store_state"))))
                        ;; Both retirement-COMMIT sides replay exactly this
                        ;; immutable close identity, never a newly minted
                        ;; close request after acknowledgement loss.
                        (should (equal (e-runtime-store-worker--column state 0)
                                       1))
                        (should (equal (e-runtime-store-worker--column state 1)
                                       close-id)))
                      (should (equal (car (car (sqlite-select db
                                                               "SELECT COUNT(*) FROM runtime_store_receipts")))
                                     0)))
                  (sqlite-close db))))
          (when store (ignore-errors (e-runtime-store-close store)))
          (delete-directory directory t))))))

(ert-deftest e-runtime-store-s92-c06-async-close-finalizes-owned-handle-once ()
  "Close handles settle without await and notify only after full finalization."
  (cl-labels
      ((exercise (ready expected-state)
         (let* ((directory (make-temp-file "e-runtime-store-c06-async-close-" t))
                (store (e-runtime-store-open directory))
                (callback-count 0) process close-request)
           (unwind-protect
               (progn
                 (when ready (e-runtime-store-test--wait-ready store))
                 (setq process (e-runtime-store--process store)
                       close-request (e-runtime-store--close-start store))
                 (e-runtime-store--observe
                  close-request
                  (lambda (_request)
                    ;; This can only run after process/ref/timer retirement.
                    (should (e-runtime-store--closed store))
                    (should-not (e-runtime-store--process store))
                    (cl-incf callback-count)))
                 (e-runtime-store-test--wait-terminal-without-await close-request)
                 (let ((deadline (+ (float-time) 5.0)))
                   (while (and (not (e-runtime-store--closed store))
                               (< (float-time) deadline))
                     (sit-for 0.01)))
                 (should (eq (e-runtime-store-request--state close-request)
                             expected-state))
                 (e-runtime-store-test--assert-finalized-close
                  store process callback-count)
                 ;; A later drain/finalizer cannot redeliver the close observer.
                 (sit-for 0.01)
                 (should (= callback-count 1)))
             (unless (e-runtime-store--closed store)
               (ignore-errors (e-runtime-store-close store)))
             (e-runtime-store-test--cancel-store-timers store)
             (delete-directory directory t)))))
    ;; Cold and ready-idle retirement need no compatibility await.
    (exercise nil 'committed)
    (exercise t 'committed)
    ;; Failure before the opening acknowledgement owns and settles the same
    ;; close handle, rather than leaving it outside `fail-all'.
    (e-runtime-store-test--with-worker-fault "after-open-response-formation"
      (exercise nil 'failed))
    ;; Either side of retirement COMMIT resolves the original close identity.
    (dolist (point '("before-retirement-commit" "after-retirement-commit"))
      (e-runtime-store-test--with-worker-fault point
        (exercise t 'committed)))))

(ert-deftest e-runtime-store-s92-c06-close-control-respects-cap-and-releases ()
  "A close handle owns one cap token and cannot coexist past a full cap."
  (let* ((e-runtime-store-request-capacity 1)
         (e-runtime-store-notification-capacity 1)
         (store (e-runtime-store--create :runtime-id "close-cap"
                                         :pending (make-hash-table :test 'equal)))
         (domain (e-runtime-store-submit store 'read '(:op status))))
    (unwind-protect
        (progn
          (should-error (e-runtime-store--close-start store)
                        :type 'e-runtime-store-capacity-exhausted)
          (should (= (e-runtime-store--request-count store) 1))
          (should (= (e-runtime-store--notification-count store) 1))
          (e-runtime-store-cancel store domain)
          (e-runtime-store--drain-terminal-notifications store)
          (let ((close (e-runtime-store--close-start store)))
            (should close)
            (should (= (e-runtime-store--request-count store) 1))
            (should (= (e-runtime-store--notification-count store) 1))
            (e-runtime-store--finalize-close store)
            (should (= (e-runtime-store--request-count store) 0))
            (should (= (e-runtime-store--notification-count store) 0))))
      (e-runtime-store-test--cancel-store-timers store))))

(ert-deftest e-runtime-store-s92-c06-close-frame-reserves-exact-bytes-and-rolls-back ()
  "Close admits its exact immutable frame or leaves all state retryable."
  (let* ((prototype-store (e-runtime-store--create
                           :runtime-id "close-bytes"
                           :pending (make-hash-table :test 'equal)))
         (prototype (e-runtime-store-request--create
                     :id "close-bytes:close:1" :kind 'close :body nil))
         (bytes (string-bytes
                 (e-runtime-store-codec-encode
                  (e-runtime-store--request-frame prototype-store prototype)))))
    ;; Exact local/shared capacity retains the frame until final close delivery.
    (let* ((e-runtime-store-retained-byte-capacity bytes)
           (reservation (e-runtime-store--reservation-create :limit bytes))
           (store (e-runtime-store--create
                   :runtime-id "close-bytes" :reservation reservation
                   :pending (make-hash-table :test 'equal))))
      (unwind-protect
          (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
            (let ((close (e-runtime-store--close-start store)))
              (should (= (e-runtime-store-request--retained-bytes close) bytes))
              (should (= (e-runtime-store--reserved-bytes store) bytes))
              (should (= (e-runtime-store--reservation-used reservation) bytes))
              (should (= (e-runtime-store--request-count store) 1))
              (should (= (e-runtime-store--notification-count store) 1))
              (should (stringp (e-runtime-store-request--frame close)))
              (e-runtime-store--finalize-close store)
              (should-not (e-runtime-store-request--frame close))
              (should (= (e-runtime-store--reserved-bytes store) 0))
              (should (= (e-runtime-store--reservation-used reservation) 0))
              (should (= (e-runtime-store--request-count store) 0))
              (should (= (e-runtime-store--notification-count store) 0))))
        (e-runtime-store-test--cancel-store-timers store)))
    ;; One-over local and shared-byte admission has no close/fence/adapter
    ;; side effect and can retry once the limiting budget is restored.
    (dolist (domain '(local shared))
      (let* ((e-runtime-store-retained-byte-capacity
              (if (eq domain 'local) (1- bytes) e-runtime-store-retained-byte-capacity))
             (reservation (e-runtime-store--reservation-create
                           :limit bytes :used (if (eq domain 'shared) 1 0)))
             (store (e-runtime-store--create
                     :runtime-id "close-bytes" :process 'unfenced-process
                     :opened-process 'unfenced-process :reservation reservation
                     :pending (make-hash-table :test 'equal))))
        (unwind-protect
            (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
              (should-error (e-runtime-store-close store)
                            :type 'e-runtime-store-capacity-exhausted)
              (should-not (e-runtime-store--closing-request store))
              (should-not (e-runtime-store--active-request store))
              (should (eq (e-runtime-store--process store) 'unfenced-process))
              (should (eq (e-runtime-store--opened-process store) 'unfenced-process))
              (should (= (e-runtime-store--sequence store) 0))
              (should (= (e-runtime-store--request-count store) 0))
              (should (= (e-runtime-store--notification-count store) 0))
              (should (= (e-runtime-store--reserved-bytes store) 0))
              (should (= (e-runtime-store--reservation-used reservation)
                         (if (eq domain 'shared) 1 0)))
              (let ((e-runtime-store-retained-byte-capacity bytes))
                (when (eq domain 'shared)
                  (setf (e-runtime-store--reservation-used reservation) 0))
                (should (e-runtime-store--close-start store)))
              (should (= (e-runtime-store--request-count store) 1)))
          (e-runtime-store--finalize-close store)
          (e-runtime-store-test--cancel-store-timers store))))))

(ert-deftest e-runtime-store-s92-c06-partial-close-send-recovers-same-id-once ()
  "Ambiguous close send fences, replays its retained identity, then exhausts once."
  (let* ((store (e-runtime-store--create
                 :directory "/tmp/close-partial/" :runtime-id "close-partial"
                 :process 'old-worker :opened-process 'old-worker
                 :pending (make-hash-table :test 'equal)))
         (fenced 0) replay-wire close first)
    (unwind-protect
        (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore)
                  ((symbol-function 'e-runtime-store--fence-worker)
                   (lambda (_store) (cl-incf fenced)))
                  ((symbol-function 'e-runtime-store--schedule-close-finalization) #'ignore)
                  ((symbol-function 'process-send-string)
                   (lambda (_process _wire)
                     (signal 'file-error '("simulated partial close write")))))
          (setq close (e-runtime-store--close-start store))
          (e-runtime-store--dispatch-close store)
          (setq first (e-runtime-store--recovery-cause store))
          (should (= fenced 1))
          (should (eq (e-runtime-store--recovering-request store) close))
          (should (eq (e-runtime-store--active-request store) close))
          (should (stringp (e-runtime-store-request--frame close)))
          ;; Model replacement open acknowledgement and capture its exact
          ;; replay frame.  The DP4 transaction-side before/after-COMMIT close
          ;; fault scenarios exercise this same identity on the real worker.
          (let ((open (e-runtime-store-request--create
                       :id "close-partial:open:replacement" :kind 'open
                       :state 'submitted)))
            (setf (e-runtime-store--active-request store) open
                  (e-runtime-store--open-control-request store) open)
            (puthash (e-runtime-store-request--id open) open
                     (e-runtime-store--pending store))
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (_process wire) (setq replay-wire wire))))
              (e-runtime-store--settle
               store open
               (list :id (e-runtime-store-request--id open)
                     :ok t :result '(:opened t)))))
          (should (equal (plist-get (e-runtime-store--unpack
                                     (string-remove-suffix "\n" replay-wire))
                                    :id)
                         (e-runtime-store-request--id close)))
          ;; A second ambiguous incident consumes no second recovery attempt
          ;; and preserves the first typed close transport cause.
          (e-runtime-store--recover-or-fail
           store '(e-runtime-store-protocol-error "late corrupt close response"))
          (should (eq (e-runtime-store-request--state close) 'failed))
          (should (equal (e-runtime-store-request--error close) first))
          (should (equal (plist-get first :request-id)
                         (e-runtime-store-request--id close)))
          ;; The second symptom is recorded for diagnosis, but must not start
          ;; or fence a second replacement worker.
          (should (= fenced 1))
          (should-not (e-runtime-store--unavailable store)))
      (e-runtime-store-test--cancel-store-timers store)
      (e-runtime-store-test--assert-no-store-timers store))))

(ert-deftest e-runtime-store-s92-c06-public-reopen-after-open-rollback ()
  "Every pre-send setup rollback and terminal close permit real same-dir reopen."
  (dolist (phase '(setup encode start))
    (let* ((directory (make-temp-file "e-runtime-store-public-reopen-" t))
           (runtime-id (format "public-reopen-%s" phase))
           failed-store reopened)
      (unwind-protect
          (let ((original-prepare (symbol-function 'e-runtime-store--prepare-open-control)))
            (cl-letf (((symbol-function 'e-runtime-store--prepare-open-control)
                       (lambda (store)
                         (setq failed-store store)
                         (funcall original-prepare store))))
              (pcase phase
                ('setup
                 (cl-letf (((symbol-function 'e-runtime-store--open-control-frame)
                            (lambda (&rest _args)
                              (signal 'file-error '("forced open setup failure")))))
                   (should-error (e-runtime-store-open directory :runtime-id runtime-id)
                                 :type 'file-error)))
                ('encode
                 (cl-letf (((symbol-function 'e-runtime-store-codec-encode)
                            (lambda (&rest _args)
                              (signal 'file-error '("forced open encode failure")))))
                   (should-error (e-runtime-store-open directory :runtime-id runtime-id)
                                 :type 'file-error)))
                ('start
                 (cl-letf (((symbol-function 'e-runtime-store--start-process)
                            (lambda (&rest _args)
                              (signal 'file-error '("forced worker start failure")))))
                   (should-error (e-runtime-store-open directory :runtime-id runtime-id)
                                 :type 'file-error)))))
            (should (e-runtime-store--closed failed-store))
            (should-not (e-runtime-store--open-control-request failed-store))
            (should-not (e-runtime-store--active-request failed-store))
            (should (= (hash-table-count (e-runtime-store--pending failed-store)) 0))
            (should-not (e-runtime-store--process failed-store))
            (should-not (e-runtime-store--opened-process failed-store))
            (should-not (e-runtime-store--stderr-buffer failed-store))
            (should-not (e-runtime-store--input-fragment failed-store))
            (e-runtime-store-test--assert-no-store-timers failed-store)
            ;; This is a real public reopen on the same ownership directory,
            ;; not a fake-store prepare helper.
            (setq reopened (e-runtime-store-open directory :runtime-id runtime-id))
            (e-runtime-store-test--wait-ready reopened)
            (should (e-runtime-store-call reopened 'read '(:op store-metrics)))
            (e-runtime-store-close reopened)
            (setq reopened (e-runtime-store-open directory :runtime-id runtime-id))
            (e-runtime-store-test--wait-ready reopened)
            (should (e-runtime-store-call reopened 'read '(:op store-metrics))))
        (when reopened (ignore-errors (e-runtime-store-close reopened)))
        (when failed-store (e-runtime-store-test--cancel-store-timers failed-store))
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-s92-c06-singleton-open-does-not-consume-client-cap ()
  "A cap-one cold submission progresses beside exactly one tokenless open."
  (let* ((e-runtime-store-request-capacity 1)
         (e-runtime-store-notification-capacity 1)
         (directory (make-temp-file "e-runtime-store-open-cap-" t))
         store request open)
    (unwind-protect
        (progn
          (setq store (e-runtime-store-open directory)
                open (e-runtime-store--active-request store))
          (should (eq (e-runtime-store-request--kind open) 'open))
          ;; The causally-required singleton has no client admission or
          ;; notification ownership, so it cannot reduce the advertised cap.
          (should (= (e-runtime-store--request-count store) 0))
          (should (= (e-runtime-store--notification-count store) 0))
          (setq request (e-runtime-store-submit store 'read '(:op status)))
          (should (= (e-runtime-store--request-count store) 1))
          (should (= (e-runtime-store--notification-count store) 1))
          (e-runtime-store--ensure-worker-open store)
          (should (eq (e-runtime-store--active-request store) open))
          (should-error (e-runtime-store-submit store 'read '(:op status))
                        :type 'e-runtime-store-capacity-exhausted)
          ;; No await drives this: normal worker/filter/scheduler progress
          ;; resolves the single domain request and leaves no client budget.
          (e-runtime-store-test--wait-terminal-without-await request)
          (should (eq (e-runtime-store-request--state request) 'committed))
          (let ((deadline (+ (float-time) 1.0)))
            (while (and (or (> (e-runtime-store--request-count store) 0)
                            (> (e-runtime-store--notification-count store) 0))
                        (< (float-time) deadline))
              (sit-for 0.01)))
          (should (= (e-runtime-store--request-count store) 0))
          (should (= (e-runtime-store--notification-count store) 0)))
      (when store (ignore-errors (e-runtime-store-close store)))
      (when store (e-runtime-store-test--cancel-store-timers store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-c06-singleton-open-failure-releases-client-cap ()
  "Failed singleton open owns no stranded client slot or notification token."
  (e-runtime-store-test--with-worker-fault "after-open-response-formation"
    (let* ((e-runtime-store-request-capacity 1)
           (e-runtime-store-notification-capacity 1)
           (directory (make-temp-file "e-runtime-store-open-fail-cap-" t))
           store request)
      (unwind-protect
          (progn
            (setq store (e-runtime-store-open directory)
                  request (e-runtime-store-submit store 'read '(:op status)))
            (e-runtime-store-test--wait-terminal-without-await request)
            (should (eq (e-runtime-store-request--state request) 'failed))
            (let ((deadline (+ (float-time) 1.0)))
              (while (and (or (> (e-runtime-store--request-count store) 0)
                              (> (e-runtime-store--notification-count store) 0))
                          (< (float-time) deadline))
                (sit-for 0.01)))
            (should (= (e-runtime-store--request-count store) 0))
            (should (= (e-runtime-store--notification-count store) 0))
            (should-not (e-runtime-store--active-request store)))
        (when store (ignore-errors (e-runtime-store-close store)))
        (when store (e-runtime-store-test--cancel-store-timers store))
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-s92-c06-open-control-exact-bound-preflights-before-worker ()
  "Open control uses its own exact bound before process creation or encoding."
  (let* ((store (e-runtime-store--create
                 :directory "/tmp/open-bound/" :runtime-id "open-bound"
                 :pending (make-hash-table :test 'equal)))
         (prototype (e-runtime-store-request--create :id "open-bound:open:1"
                                                     :kind 'open))
         (value (list :id (e-runtime-store-request--id prototype)
                      :kind 'open :directory "/tmp/open-bound/"
                      :runtime-id "open-bound" :parent-identity nil))
         (exact (string-bytes (e-runtime-store-codec-encode value)))
         (wire (e-runtime-store-codec-wire-byte-count exact))
         (started 0) encoded)
    (cl-letf (((symbol-function 'e-runtime-store--open-control-frame)
               (lambda (_store _request) value)))
      (let ((e-runtime-store-open-control-canonical-byte-limit exact)
            (e-runtime-store-open-control-wire-byte-limit wire))
        (let ((control (e-runtime-store--prepare-open-control store)))
          (should (= (e-runtime-store-request--frame-bytes control) exact))
          (should (= (e-runtime-store-codec-wire-byte-count
                      (string-bytes (e-runtime-store-request--frame control)))
                     wire))
          (e-runtime-store--release-open-control store control)))
      (let ((e-runtime-store-open-control-canonical-byte-limit (1- exact))
            (e-runtime-store-open-control-wire-byte-limit
             (e-runtime-store-codec-wire-byte-count (1- exact))))
        (cl-letf (((symbol-function 'e-runtime-store-codec-encode)
                   (lambda (&rest _args) (setq encoded t) "must-not-encode"))
                  ((symbol-function 'e-runtime-store--start-process)
                   (lambda (&rest _args) (cl-incf started))))
          (should-error (e-runtime-store--prepare-open-control store)
                        :type 'e-runtime-store-codec-too-large)
          (should-not encoded)
          (should (= started 0))
          (should-not (e-runtime-store--open-control-request store)))))
    ;; The public cold-open path performs the same measure before it launches
    ;; its worker, not merely the private helper used by scheduler tests.
    (let ((directory (make-temp-file "e-runtime-store-open-too-large-" t)))
      (unwind-protect
          (let ((e-runtime-store-open-control-canonical-byte-limit (1- exact))
                (e-runtime-store-open-control-wire-byte-limit
                 (e-runtime-store-codec-wire-byte-count (1- exact))))
            (cl-letf (((symbol-function 'e-runtime-store--open-control-frame)
                       (lambda (_store _request) value))
                      ((symbol-function 'e-runtime-store--start-process)
                       (lambda (&rest _args) (cl-incf started))))
              (should-error (e-runtime-store-open directory :runtime-id "public-open-bound")
                            :type 'e-runtime-store-codec-too-large)
              (should (= started 0))))
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-s92-c06-open-control-send-failure-fences-first-cause ()
  "A possible partial open write fences before selected-client settlement."
  (let* ((selected (e-runtime-store-request--create
                    :id "open-send:w:1" :kind 'write :body '(:op status)
                    :operation 'status :state 'queued))
         (store (e-runtime-store--create
                 :directory "/tmp/open-send/" :runtime-id "open-send"
                 :process 'old-worker :starting-request selected
                 :client-queue (list selected) :pending (make-hash-table :test 'equal)))
         (fenced 0))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (&rest _args)
                     (signal 'file-error '("simulated partial open write"))))
                  ((symbol-function 'e-runtime-store--fence-worker)
                   (lambda (_store) (cl-incf fenced)))
                  ((symbol-function 'e-runtime-store--schedule) #'ignore))
          (e-runtime-store--ensure-worker-open store)
          (should (= fenced 1))
          (should-not (e-runtime-store--open-control-request store))
          (should-not (e-runtime-store--active-request store))
          (should (= (hash-table-count (e-runtime-store--pending store)) 0))
          (should (eq (e-runtime-store-request--state selected) 'failed))
          (should (eq (car (e-runtime-store-request--error selected))
                      'e-runtime-store-unavailable))
          (should (eq (car (plist-get (cddr (e-runtime-store-request--error selected))
                                      :cause))
                      'file-error)))
      (e-runtime-store-test--cancel-store-timers store)
      (e-runtime-store-test--assert-no-store-timers store))))

(ert-deftest e-runtime-store-s92-c06-open-control-teardown-reopen-matrix ()
  "Every terminal owner releases open control; stale firing itself is inert."
  (cl-labels
      ((make-store (name)
         (e-runtime-store--create
          :directory "/tmp/open-teardown/" :runtime-id name :process 'old-worker
          :pending (make-hash-table :test 'equal)))
       (install (store)
         (let ((control (e-runtime-store--prepare-open-control store)))
           (setf (e-runtime-store--active-request store) control)
           (puthash (e-runtime-store-request--id control) control
                    (e-runtime-store--pending store))
           control))
       (assert-released (store)
         (should-not (e-runtime-store--open-control-request store))
         (should-not (e-runtime-store--active-request store))
         (should (= (hash-table-count (e-runtime-store--pending store)) 0))
         (e-runtime-store-test--cancel-store-timers store)
         (e-runtime-store-test--assert-no-store-timers store))
       (assert-later-open (name)
         (let* ((later (make-store (concat name "-later")))
                (control (e-runtime-store--prepare-open-control later)))
           (should (stringp (e-runtime-store-request--frame control)))
           (e-runtime-store--release-open-control later control))))
    (dolist (case '(success typed-failure timeout malformed worker-exit
                            recovery-exhaustion close reset-equivalent stale-generation))
      (let* ((store (make-store (format "open-%s" case)))
             (control (install store)))
        (unwind-protect
            (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore)
                      ((symbol-function 'e-runtime-store--fence-worker) #'ignore))
              (pcase case
                ('success
                 (e-runtime-store--settle
                  store control
                  (list :id (e-runtime-store-request--id control)
                        :ok t :result '(:opened t))))
                ('typed-failure
                 (e-runtime-store--settle
                  store control
                  (list :id (e-runtime-store-request--id control)
                        :ok nil :error-symbol 'e-runtime-store-error
                        :error-data '("typed open failure"))))
                ('timeout
                 (setf (e-runtime-store-request--submitted-at control) 0.0)
                 (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 61.0)))
                   (e-runtime-store--recover-overdue-active store 60.0)))
                ('malformed (e-runtime-store--consume-response-line store "not-a-frame"))
                ('worker-exit (e-runtime-store--worker-exited store))
                ('recovery-exhaustion
                 (setf (e-runtime-store--recovering-request store)
                       (e-runtime-store-request--create :id "replay" :kind 'write
                                                        :state 'submitted :frame "frame"))
                 (e-runtime-store--recovery-exhausted
                  store '(e-runtime-store-timeout "exhausted")))
                ('close (e-runtime-store--finalize-close store))
                ;; There is no public reset API in DP5A; unavailable freeze is
                ;; its concrete reset-equivalent lifecycle owner.
                ('reset-equivalent
                 (e-runtime-store--freeze-and-stop store
                                                   '(e-runtime-store-error "reset")))
                ('stale-generation
                 (setf (e-runtime-store--scheduler-generation store) 2)
                 (e-runtime-store--scheduler-fired store 1)
                 (should (eq (e-runtime-store--open-control-request store) control))
                 ;; The stale timer is inert; the current owner teardown does
                 ;; the release rather than the stale callback.
                 (e-runtime-store--freeze-and-stop store
                                                   '(e-runtime-store-error "current teardown"))))
              (assert-released store)
              (assert-later-open (format "open-%s" case)))
          (e-runtime-store-test--cancel-store-timers store))))))

(ert-deftest e-runtime-store-s92-c06-full-client-cap-keeps-one-open-control ()
  "128 cold clients retain their full cap beside exactly one tokenless open."
  (let* ((e-runtime-store-request-capacity 128)
         (e-runtime-store-notification-capacity 128)
         (store (e-runtime-store--create
                 :directory "/tmp/full-open-cap/" :runtime-id "full-open-cap"
                 :process 'open-worker :pending (make-hash-table :test 'equal)))
         (requests nil) (sends 0) fenced)
    (unwind-protect
        (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore)
                  ((symbol-function 'process-send-string)
                   (lambda (&rest _args) (cl-incf sends)))
                  ((symbol-function 'e-runtime-store--fence-worker)
                   (lambda (&rest _args) (setq fenced t))))
          (dotimes (_ 128)
            (push (e-runtime-store-submit store 'read '(:op status)) requests))
          (should (= (e-runtime-store--request-count store) 128))
          (should (= (e-runtime-store--notification-count store) 128))
          (e-runtime-store--ensure-worker-open store)
          (let ((control (e-runtime-store--active-request store)))
            (should (eq (e-runtime-store-request--kind control) 'open))
            (should (eq control (e-runtime-store--open-control-request store)))
            (should (= sends 1))
            (e-runtime-store--ensure-worker-open store)
            (should (= sends 1)))
          (should-error (e-runtime-store-submit store 'read '(:op status))
                        :type 'e-runtime-store-capacity-exhausted)
          (should-error (e-runtime-store--close-start store)
                        :type 'e-runtime-store-capacity-exhausted)
          (should-not fenced)
          ;; Releasing one pre-submit client makes room for the close itself:
          ;; it occupies exactly slot/token 128, never an extra control slot.
          (e-runtime-store-cancel store (car requests))
          (e-runtime-store--drain-terminal-notifications store)
          (should (= (e-runtime-store--request-count store) 127))
          (let ((close (e-runtime-store--close-start store)))
            (should close)
            (should (= (e-runtime-store--request-count store) 128))
            (should (= (e-runtime-store--notification-count store) 128))))
      (e-runtime-store--fail-all store '(e-runtime-store-error "fixture teardown"))
      (e-runtime-store-test--cancel-store-timers store)
      (e-runtime-store-test--assert-no-store-timers store))))

(ert-deftest e-runtime-store-s92-c06-fixture-cleans-cleared-runtime-timer-ref ()
  "Timer cleanup finds a private callback after its fake-store field is nil."
  (let* ((store (e-runtime-store--create :runtime-id "timer-cleanup"
                                         :pending (make-hash-table :test 'equal)))
         (timer (run-at-time 60 nil #'e-runtime-store--scheduler-fired store 1)))
    (unwind-protect
        (progn
          ;; Simulate a lifecycle test that clears the struct before teardown.
          (setf (e-runtime-store--scheduler-timer store) nil)
          (should (memq timer (e-runtime-store-test--runtime-timers)))
          (e-runtime-store-test--cancel-store-timers store)
          (e-runtime-store-test--assert-no-store-timers store))
      (when (timerp timer) (cancel-timer timer)))))

(ert-deftest e-runtime-store-s92-c06-close-yields-before-close-observer ()
  "Close drains sixteen prior completions, yields, then notifies close last."
  (let* ((store (e-runtime-store--create :runtime-id "close-page"
                                         :pending (make-hash-table :test 'equal)))
         (delivered 0) heartbeat close-called heartbeat-timer)
    (unwind-protect
        (progn
          (dotimes (n 17)
            (let ((request (e-runtime-store-request--create
                            :id (format "page:%d" n) :kind 'read :state 'submitted
                            :retained-bytes 0 :notification 'queued)))
              (e-runtime-store--observe request (lambda (_r) (cl-incf delivered)))
              (push request (e-runtime-store--notification-outbox store))))
          (setq heartbeat-timer
                (run-at-time 0 nil (lambda () (setq heartbeat (= delivered 16)))))
          (let ((close (e-runtime-store-request--create :id "close:page" :kind 'close
                                                        :state 'submitted :notification 'reserved)))
            (setf (e-runtime-store--closing-request store) close)
            (e-runtime-store--observe close (lambda (_r) (setq close-called (= delivered 17))))
            (e-runtime-store--finalize-close store)
            (should (= delivered 16))
            (should-not close-called)
            (sit-for 0.03)
            (should heartbeat)
            (should close-called)))
      (when (timerp heartbeat-timer) (cancel-timer heartbeat-timer))
      (e-runtime-store-test--cancel-store-timers store))))

(ert-deftest e-runtime-store-s92-exhausted-close-keeps-unretired-state-and-receipt ()
  "A failed close retry leaves its rollback state and uncertain receipt owned."
  (e-runtime-store-test--with-worker-fault "before-retirement-commit"
    (let* ((directory (make-temp-file "e-runtime-store-close-exhausted-" t))
           (database-file (expand-file-name "store.sqlite3" directory))
           (store (e-runtime-store-open directory))
           (prior-boot (copy-tree (e-runtime-store--parent-identity))))
      (plist-put prior-boot :boot
                 (concat "controlled-prior-boot-"
                         (plist-get prior-boot :boot)))
      (unwind-protect
          (progn
            ;; A successfully acknowledged write is still the one uncertain
            ;; receipt until a later write advertises its acknowledgement.
            (should (= (plist-get
                        (e-runtime-store-call
                         store 'write
                         (e-runtime-store-test--session-append-body
                          "close-exhausted"
                          '(:value uncertain-before-close)))
                        :high-water)
                       1))
            ;; The first close worker rolls its retirement transaction back;
            ;; replacement setup then fails, spending the only retry.
            (cl-letf (((symbol-function 'e-runtime-store--ensure-worker-open)
                       (lambda (&rest _arguments)
                         (signal 'e-runtime-store-error
                                 '("forced exhausted close replacement")))))
              (should-error (e-runtime-store-close store)
                            :type 'e-runtime-store-unavailable))
            (setq store nil)
            (let ((db (sqlite-open database-file)))
              (unwind-protect
                  (progn
                    (should (= (e-runtime-store-worker--column
                                (car (sqlite-select db
                                                    "SELECT retired FROM runtime_store_state")) 0)
                               0))
                    (should (= (e-runtime-store-worker--column
                                (car (sqlite-select db
                                                    "SELECT COUNT(*) FROM runtime_store_receipts")) 0)
                               1)))
                (sqlite-close db)))
            ;; Only explicit prior-boot proof can now replace the unretired
            ;; owner, and that same safe transaction reclaims its receipt.
            (e-runtime-store-worker--open directory "safe-close-cleanup" prior-boot)
            (should (= (e-runtime-store-worker--column
                        (car (sqlite-select e-runtime-store-worker--database
                                            "SELECT COUNT(*) FROM runtime_store_receipts")) 0)
                       0))
            (e-runtime-store-worker--close))
        (when store (ignore-errors (e-runtime-store-close store)))
        (e-runtime-store-worker--close)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-s92-receipt-collision-watermark-and-retirement ()
  "Receipts reject semantic id reuse, retain the active one, then retire cleanly."
  (let ((directory (make-temp-file "e-runtime-store-receipt-" t)))
    (unwind-protect
        (progn
          (e-runtime-store-worker--open directory "receipt-runtime")
          (let ((first (list :id "receipt:1" :kind 'write :write-prefix 1
                             :ack-prefix 0 :body
                             (e-runtime-store-test--session-append-body
                              "receipt" '(:value first))))
                (second (list :id "receipt:2" :kind 'write :write-prefix 2
                              :ack-prefix 1 :body
                              (e-runtime-store-test--session-append-body
                               "receipt" '(:value second) 2))))
            (should (= (plist-get (e-runtime-store-worker--write first)
                                  :high-water)
                       1))
            ;; The unacknowledged first receipt is the only replay authority.
            (should (= (e-runtime-store-worker--column
                        (car (sqlite-select e-runtime-store-worker--database
                                            "SELECT COUNT(*) FROM runtime_store_receipts")) 0)
                       1))
            (should-error
             (e-runtime-store-worker--write
              (list :id "receipt:1" :kind 'write :write-prefix 1 :ack-prefix 0
                    :body (e-runtime-store-test--session-append-body
                           "receipt" '(:value collision))))
             :type 'e-runtime-store-worker-error)
            ;; The receipt key is the id, but every replay-semantic frame
            ;; field participates in the fingerprint.
            (should-error
             (e-runtime-store-worker--write
              (list :id "receipt:1" :kind 'write :write-prefix 1 :ack-prefix 1
                    :body (e-runtime-store-test--session-append-body
                           "receipt" '(:value first))))
             :type 'e-runtime-store-worker-error)
            ;; The observed-prefix watermark retires only the earlier receipt.
            (should (= (plist-get (e-runtime-store-worker--write second)
                                  :high-water)
                       2))
            (should (= (e-runtime-store-worker--column
                        (car (sqlite-select e-runtime-store-worker--database
                                            "SELECT COUNT(*) FROM runtime_store_receipts WHERE request_id='receipt:1'")) 0)
                       0))
            (should (= (e-runtime-store-worker--column
                        (car (sqlite-select e-runtime-store-worker--database
                                            "SELECT COUNT(*) FROM runtime_store_receipts WHERE request_id='receipt:2'")) 0)
                       1))))
      (e-runtime-store-worker--close)
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-receipt-result-bound-rolls-back-mutation ()
  "Only a full fitting success response may commit its mutation and receipt."
  (let ((directory (make-temp-file "e-runtime-store-result-bound-" t))
        (limit 1024) exact one-over)
    (unwind-protect
        (let ((e-runtime-store-codec-protocol-canonical-byte-limit limit))
          (let* ((exact-request
                  (list :id "exact:1" :kind 'write :write-prefix 1 :ack-prefix 0
                        :body (e-runtime-store-test--session-append-body
                               "result-exact" '(:value commits-once))))
                 ;; Keep the ID byte length equal, so adding one ASCII result
                 ;; byte crosses precisely the same success-response boundary.
                 (one-over-request
                  (list :id "overx:1" :kind 'write :write-prefix 1 :ack-prefix 0
                        :body (e-runtime-store-test--session-append-body
                               "result-over" '(:value must-roll-back)))))
            ;; Measure the complete response that the original and replacement
            ;; worker must actually pack, never a bare SQLite receipt payload.
            (setq exact "")
            (while (< (string-bytes
                       (e-runtime-store-codec-encode
                        (list :id (plist-get exact-request :id) :ok t
                              :result exact)))
                      limit)
              (setq exact (concat exact "x")))
            (setq one-over (concat exact "x"))
            (should (= (string-bytes
                        (e-runtime-store-codec-encode
                         (list :id (plist-get exact-request :id) :ok t
                               :result exact)))
                       limit))
            (should-error
             (e-runtime-store-worker--bounded-receipt-result
              one-over-request one-over)
             :type 'e-runtime-store-codec-too-large)
            (e-runtime-store-worker--open directory "result-bound")
            (cl-labels
                ((emit (request response)
                   (with-temp-buffer
                     (let ((standard-output (current-buffer)))
                       (e-runtime-store-worker--emit-response request response))
                     (e-runtime-store-worker--unpack
                      (string-trim (buffer-string))))))
              (cl-letf (((symbol-function 'e-runtime-store-worker--write-dispatch)
                         (lambda (body)
                           (e-runtime-store-worker--session-append body)
                           exact)))
                (let ((original (e-runtime-store-worker--response exact-request)))
                  (should (plist-get original :ok))
                  (should (equal (plist-get original :result) exact))
                  (should (equal (emit exact-request original) original))
                  ;; Receipt replay must emit the same full fitting response
                  ;; without applying the domain mutation again.
                  (let ((replay (e-runtime-store-worker--response exact-request)))
                    (should (equal replay original))
                    (should (equal (emit exact-request replay) replay)))))
              (cl-letf (((symbol-function 'e-runtime-store-worker--write-dispatch)
                         (lambda (body)
                           (e-runtime-store-worker--session-append body)
                           one-over)))
                (let ((rejected
                       (e-runtime-store-worker--response one-over-request)))
                  (should-not (plist-get rejected :ok))
                  ;; The original error response is itself still transportable;
                  ;; no write receipt may be left for an unacknowledgeable value.
                  (should (equal (emit one-over-request rejected) rejected))))
              (should (= (length (plist-get
                                  (e-runtime-store-worker--read
                                   '(:op session-record-page :session-id "result-exact"))
                                  :records))
                         1))
              (should (= (length (plist-get
                                  (e-runtime-store-worker--read
                                   '(:op session-record-page :session-id "result-over"))
                                  :records))
                         0))
              (should (= (e-runtime-store-worker--column
                          (car (sqlite-select e-runtime-store-worker--database
                                              "SELECT COUNT(*) FROM runtime_store_receipts WHERE request_id='exact:1'")) 0)
                         1))
              (should (= (e-runtime-store-worker--column
                          (car (sqlite-select e-runtime-store-worker--database
                                              "SELECT COUNT(*) FROM runtime_store_receipts WHERE request_id='overx:1'")) 0)
                         0)))))
      (e-runtime-store-worker--close)
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-runtime-state-parent-lifetime-and-clean-cycles ()
  "State replacement preserves live replay authority and reclaims safe owners."
  (let ((directory (make-temp-file "e-runtime-store-cycles-" t)))
    (unwind-protect
        (let* ((live (e-runtime-store--parent-identity))
               ;; A real distinct batch Emacs carries the same host boot token
               ;; but a separate parent PID/start identity.  It must not be
               ;; mistaken for a previous boot while LIVE remains present.
               (other-emacs (e-runtime-store-test--separate-batch-parent-identity))
               (start (plist-get live :process-start))
               ;; This is deliberately a controlled persisted prior-boot
               ;; record, not a second live Emacs' per-process nonce.
               (prior (copy-tree live))
               (pid-reused-old (e-runtime-store-test--parent
                                (plist-get live :boot) (emacs-pid)
                                '(impossible-prior-start)))
               (pid-reused (e-runtime-store-test--parent
                            (plist-get live :boot) (emacs-pid) start))
               (dead (e-runtime-store-test--parent
                      (plist-get live :boot) 999999 '(dead-parent-start)))
               (dead-replacement (copy-tree live)))
          (should (equal (plist-get live :boot)
                         (plist-get other-emacs :boot)))
          (should-not (= (plist-get live :pid) (plist-get other-emacs :pid)))
          (plist-put prior :boot (concat "controlled-prior-boot-"
                                         (plist-get live :boot)))
          ;; A live unretired predecessor cannot be overwritten or orphaned.
          (e-runtime-store-worker--open directory "live-predecessor" live)
          (e-runtime-store-worker--write
           (list :id "live:1" :kind 'write :write-prefix 1 :ack-prefix 0
                 :body (e-runtime-store-test--session-append-body
                        "live" '(:value retained))))
          (e-runtime-store-worker--close)
          (should-error
           (e-runtime-store-worker--open directory "must-refuse" other-emacs)
           :type 'e-runtime-store-parent-active)
          (let ((db (sqlite-open (expand-file-name "store.sqlite3" directory))) )
            (unwind-protect
                (progn
                  (should (equal (e-runtime-store-worker--column
                                  (car (sqlite-select db "SELECT runtime_id FROM runtime_store_state")) 0)
                                 "live-predecessor"))
                  (should (= (e-runtime-store-worker--column
                              (car (sqlite-select db "SELECT COUNT(*) FROM runtime_store_receipts")) 0) 1)))
              (sqlite-close db)))
          ;; Same runtime and parent is a replacement worker, so its uncertain
          ;; receipt remains replay authority.
          (e-runtime-store-worker--open directory "live-predecessor" live)
          (should (= (e-runtime-store-worker--column
                      (car (sqlite-select e-runtime-store-worker--database
                                          "SELECT COUNT(*) FROM runtime_store_receipts")) 0) 1))
          (e-runtime-store-worker--close)
          ;; A prior parent boot proves safe replacement and reclaims all old
          ;; receipts before the singleton can name the new runtime.
          (e-runtime-store-worker--open directory "prior-boot" prior)
          (should (= (e-runtime-store-worker--column
                      (car (sqlite-select e-runtime-store-worker--database
                                          "SELECT COUNT(*) FROM runtime_store_receipts")) 0) 0))
          (e-runtime-store-worker--close)
          ;; PID reuse uses process-start, not a live-looking PID alone.
          (e-runtime-store-worker--open directory "pid-reused-old" pid-reused-old)
          (e-runtime-store-worker--close)
          (e-runtime-store-worker--open directory "pid-reused" pid-reused)
          ;; Retired runtime state may be replaced by a new parent without
          ;; consulting the old live parent identity.
          (e-runtime-store-worker--retire
           '(:id "pid-reused:close" :kind close :write-prefix 0 :ack-prefix 0
             :body nil))
          (e-runtime-store-worker--close)
          ;; A same-boot dead PID is also proof, and its uncertain receipt is
          ;; reclaimed before the singleton ceases to name that predecessor.
          (e-runtime-store-worker--open directory "dead-predecessor" dead)
          (e-runtime-store-worker--write
           (list :id "dead:1" :kind 'write :write-prefix 1 :ack-prefix 0
                 :body (e-runtime-store-test--session-append-body
                        "dead" '(:value uncertain))))
          (e-runtime-store-worker--close)
          (e-runtime-store-worker--open directory "dead-replacement" dead-replacement)
          (should (= (e-runtime-store-worker--column
                      (car (sqlite-select e-runtime-store-worker--database
                                          "SELECT COUNT(*) FROM runtime_store_receipts")) 0) 0))
          (e-runtime-store-worker--retire
           '(:id "dead-replacement:close" :kind close :write-prefix 0
             :ack-prefix 0 :body nil))
          (e-runtime-store-worker--close)
          (dotimes (index 3)
            (let ((store (e-runtime-store-open
                          directory :runtime-id (format "clean-%d" index))))
              (unwind-protect
                  (should (> (plist-get
                              (e-runtime-store-call
                               store 'write
                              (e-runtime-store-test--session-append-body
                                "clean" (list :value index) (1+ index)))
                              :high-water)
                             0))
                (e-runtime-store-close store)))
            (let ((db (sqlite-open (expand-file-name "store.sqlite3" directory))))
              (unwind-protect
                  (progn
                    (should (= (e-runtime-store-worker--column
                                (car (sqlite-select
                                      db "SELECT COUNT(*) FROM runtime_store_state")) 0)
                               1))
                    ;; Every retired replacement owns zero receipts; a receipt
                    ;; is never left without the singleton that can replay it.
                    (should (= (e-runtime-store-worker--column
                                (car (sqlite-select
                                      db "SELECT COUNT(*) FROM runtime_store_receipts")) 0)
                               0)))
                (sqlite-close db)))))
      (e-runtime-store-worker--close)
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
             (e-runtime-store-test--session-append-body
              "queued-timeout" '(:value active))))
      (setq queued
            (e-runtime-store-submit
             store 'write
             (e-runtime-store-test--resource-put-body
              "queued-timeout" "must-not-land" "queued")))
      (e-runtime-store-test--fire-current-scheduler store)
      ;; Queue expiry is a scheduler transition, not an await timeout.  Keep
      ;; the active write inside its fresh submitted phase while advancing only
      ;; the queued request's admission clock.
      (setf (e-runtime-store-request--admitted-at queued) 0.0)
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) 61.0)))
        (e-runtime-store--expire-overdue-queued store 60.0))
      (should (eq (e-runtime-store-request--state queued) 'failed))
      (should (e-runtime-store-live-p store))
      (should-not (plist-get (e-runtime-store-status store) :unavailable))
      (should-not (memq queued (e-runtime-store--client-queue store)))
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
       (e-runtime-store-call
        store 'read
        '(:op resource-get :lineage-id "queued-timeout" :path "must-not-land")))
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
          (e-runtime-store-test--wait-ready store)
          (set-process-filter
           process
           (lambda (_worker text)
             (setq captured (concat captured text))))
          (setq requests
                (list
                 (e-runtime-store-submit
                  store 'write
                  (e-runtime-store-test--session-append-body
                   "close" '(:value one)))
                 (e-runtime-store-submit
                  store 'write
                  (e-runtime-store-test--session-append-body
                   "close" '(:value two)))
                 (e-runtime-store-submit
                  store 'read '(:op session-record-page :session-id "close"))))
          (e-runtime-store-test--fire-current-scheduler store)
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
                t))
             ((symbol-function 'e-runtime-store--schedule) #'ignore))
          (let ((request
                 (e-runtime-store-submit
                  store 'write
                  (e-runtime-store-test--session-append-body
                   "startup" '(:value never)))))
            (e-runtime-store--scheduler-fired
             store (e-runtime-store--scheduler-generation store))
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
            (should (eq (e-runtime-store--last-error store)
                        (e-runtime-store-request--error request)))
            (should-not (e-runtime-store--unavailable store))
            (should-not (e-runtime-store--starting-request store))
            (should-not (e-runtime-store--active-request store))
            (should-not (e-runtime-store--client-queue store))
            (should (= (hash-table-count (e-runtime-store--pending store)) 0))))))))

(ert-deftest e-runtime-store-worker-symlink-build-resolves-real-core-directory ()
  "A generated worker tree may omit a newer sibling dependency."
  (let* ((core-directory
          (expand-file-name "../lisp/core" e-runtime-store-test--source-directory))
         (build-directory (make-temp-file "e-runtime-store-worker-build-" t))
         (directory (make-temp-file "e-runtime-store-worker-db-" t))
         (worker-file (expand-file-name "e-runtime-store-worker.el"
                                       build-directory))
         (codec-file (expand-file-name "e-runtime-store-codec.el"
                                      build-directory))
         (process-environment (copy-sequence process-environment))
         store)
    (unwind-protect
        (progn
          ;; This is the generated-package shape that exposed the regression:
          ;; selected runtime modules are symlinks, but the newly added sibling
          ;; is absent from the build directory.
          (make-symbolic-link
           (expand-file-name "e-runtime-store-worker.el" core-directory)
           worker-file)
          (make-symbolic-link
           (expand-file-name "e-runtime-store-codec.el" core-directory)
           codec-file)
          (should-not (file-exists-p (expand-file-name "e-json.el"
                                                        build-directory)))
          (setenv "E_RUNTIME_STORE_TEST_WORKER_FILE" worker-file)
          (let ((load-path (cons build-directory load-path)))
            (should
             (member (file-name-as-directory
                      (file-truename core-directory))
                     (e-runtime-store--command)))
            (setq store (e-runtime-store-open directory))
            (e-runtime-store-test--wait-ready store)
            (should (eq (plist-get (e-runtime-store-status store) :startup)
                        'ready))
            (should (= (plist-get
                        (e-runtime-store-call store 'read '(:op store-metrics))
                        :schema-version)
                       8))))
      (when store (ignore-errors (e-runtime-store-close store)))
      (when (file-directory-p build-directory)
        (delete-directory build-directory t))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-worker-pre-main-failure-preserves-bounded-diagnostic ()
  "A pre-main child failure reports exit status and bounded stderr."
  (let* ((worker-file (make-temp-file "e-runtime-store-bad-worker-" nil ".el"))
         (directory (make-temp-file "e-runtime-store-bad-worker-db-" t))
         (marker "synthetic pre-main startup failure")
         (payload (concat marker " "
                         (make-string
                          (+ e-runtime-store-startup-diagnostic-byte-limit 512)
                          ?x)))
         (process-environment (copy-sequence process-environment))
         store request)
    (unwind-protect
        (progn
          (write-region (format "(error %S)\n" payload)
                        nil worker-file nil 'silent)
          (setenv "E_RUNTIME_STORE_TEST_WORKER_FILE" worker-file)
          (setq store (e-runtime-store-open directory)
                request (e-runtime-store-submit store 'read '(:op store-metrics)))
          (let* ((failure
                  (should-error (e-runtime-store-await store request 5.0)
                                :type 'e-runtime-store-unavailable))
                 (cause (plist-get (cddr failure) :cause))
                 (cause-data (cddr cause))
                 (stderr (plist-get cause-data :stderr)))
            (should-not (string-match-p "empty-response"
                                        (error-message-string failure)))
            (should (= (plist-get (cddr failure) :exit-status) 1))
            (should (string-match-p
                     (regexp-quote marker)
                     (plist-get (cddr failure) :stderr)))
            (should (eq (car cause) 'e-runtime-store-timeout))
            (should (eq (plist-get cause-data :process-status) 'exit))
            (should (= (plist-get cause-data :exit-status) 1))
            (should (string-match-p (regexp-quote marker) stderr))
            (should (plist-get cause-data :stderr-truncated))
            (should (<= (string-bytes stderr)
                        e-runtime-store-startup-diagnostic-byte-limit))))
      (when store (ignore-errors (e-runtime-store-close store)))
      (when (file-exists-p worker-file) (delete-file worker-file))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-s92-submission-restarts-the-timeout-interval ()
  "Real scheduler promotion gives a near-expiry request a fresh interval."
  (let* ((store (e-runtime-store--create
                 :runtime-id "phase" :pending (make-hash-table :test 'equal)))
         (clock 0.0)
         open-request candidate sent-kinds request)
    (cl-letf
        (((symbol-function 'float-time) (lambda (&optional _time) clock))
         ((symbol-function 'e-runtime-store--start-process)
          (lambda (runtime)
            (setf (e-runtime-store--process runtime) 'phase-worker)
            'phase-worker))
         ((symbol-function 'e-runtime-store--live-p)
          (lambda (_runtime) t))
         ((symbol-function 'e-runtime-store--schedule) #'ignore)
         ((symbol-function 'process-send-string)
          (lambda (_process _frame)
            (let* ((active (e-runtime-store--active-request store))
                   (kind (e-runtime-store-request--kind active)))
              (push kind sent-kinds)
              (pcase kind
                ('open
                 (setq open-request active
                       candidate (car (e-runtime-store--client-queue store)))
                 ;; The selected domain request stays queue-owned throughout
                 ;; real internal open setup rather than being popped early.
                 (should (eq (e-runtime-store--starting-request store)
                             candidate))
                 (should (eq (e-runtime-store-request--state candidate)
                             'queued))
                 (should (memq candidate (e-runtime-store--client-queue store)))
                 (should-not (eq active candidate)))
                ('write
                 (should (eq active candidate))
                 (should (eq (e-runtime-store-request--state candidate)
                             'submitted))
                 (should (= (e-runtime-store-request--submitted-at candidate)
                            9.9))))))))
      (setq request
            (e-runtime-store-submit
             store 'write
             (e-runtime-store-test--session-append-body
              "phase" '(:value retained))))
      ;; Submit only reserves and queues.  Each following state transition is
      ;; a scheduler or worker event, never an awaiter-side promotion.
      (e-runtime-store--scheduler-fired
       store (e-runtime-store--scheduler-generation store))
      ;; The open acknowledgement arrives just before the selected request's
      ;; queue interval would expire.
      (setq clock 9.9)
      (e-runtime-store--consume-output
       store
       (e-runtime-store--pack
        (list :id (e-runtime-store-request--id open-request)
              :ok t :result '(:opened t))))
      (e-runtime-store--scheduler-fired
       store (e-runtime-store--scheduler-generation store))
      (should (eq request candidate))
      (should (eq (e-runtime-store-request--state request) 'submitted))
      (should (equal (nreverse sent-kinds) '(open write)))
      (should (= (e-runtime-store-request--admitted-at request) 0.0))
      (should-not (memq request (e-runtime-store--client-queue store)))
      (should-not (e-runtime-store--starting-request store))
      (should (eq (gethash (e-runtime-store-request--id request)
                           (e-runtime-store--pending store))
                  request))
      ;; At 10.1, the queue interval has elapsed but the submitted interval
      ;; which began at the real dispatch promotion still has almost ten seconds.
      (setq clock 10.1)
      (e-runtime-store--consume-output
       store
       (e-runtime-store--pack
        (list :id (e-runtime-store-request--id candidate)
              :ok t :result '(:phase submitted))))
      (should (equal (e-runtime-store-await store request 10.0)
                     '(:phase submitted)))
      (should (eq (e-runtime-store-request--state request) 'committed)))))

(ert-deftest e-runtime-store-s92-queued-waiter-observes-earlier-active-deadline ()
  "A queued waiter starts one recovery at active deadline before its own age."
  (let* ((clock 61.0)
         (active (e-runtime-store-request--create
                  :id "active:1" :kind 'write :body '(:op resource-put)
                  :state 'submitted :submitted-at 0.0))
         (queued (e-runtime-store-request--create
                  :id "queued:2" :kind 'write :body '(:op resource-put)
                  :state 'queued :admitted-at 10.0))
         (pending (make-hash-table :test 'equal))
         (store (e-runtime-store--create
                 :runtime-id "deadline" :pending pending :active-request active
                 :client-queue (list queued)))
         (transitions 0) causes)
    (puthash (e-runtime-store-request--id active) active pending)
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) clock))
              ((symbol-function 'e-runtime-store--live-p) (lambda (_store) t))
              ((symbol-function 'e-runtime-store--schedule) #'ignore)
              ((symbol-function 'e-runtime-store--recover-or-fail)
               (lambda (_store cause)
                 (cl-incf transitions)
                 (push cause causes)
                 ;; Successful replacement leaves queued ownership and order
                 ;; intact until the active request has definitively resolved.
                 (setf (e-runtime-store-request--submitted-at active) clock
                       (e-runtime-store-request--state queued) 'committed
                       (e-runtime-store-request--result queued) :continued))))
      (e-runtime-store--scheduler-fired store
                                        (e-runtime-store--scheduler-generation store))
      (should (eq (e-runtime-store-request--result queued) :continued)))
    (should (= transitions 1))
    (should (memq queued (e-runtime-store--client-queue store)))
    (should (equal (plist-get (cddr (car causes)) :request-id) "active:1"))))

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
                   (e-runtime-store-test--session-append-body
                    "after-local-error" '(:value committed)))
                  :high-water)
                 1)))))

(ert-deftest e-runtime-store-s92-protocol-failure-preserves-cause-and-recovers ()
  "Malformed and wrong responses fence once, then replay the active identity."
  (dolist (protocol-cause '(empty-response malformed-response unknown-response-id))
    (let* ((directory (make-temp-file "e-runtime-store-protocol-" t))
           (store (e-runtime-store-open directory))
           request sent)
      (unwind-protect
          (progn
            (e-runtime-store-test--wait-ready store)
            ;; The transport seam leaves the domain request submitted but not
            ;; delivered, so reopening can prove that it was never retried.
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (_process frame) (setq sent frame))))
              (setq request
                    (e-runtime-store-submit
                     store 'write
                     (e-runtime-store-test--session-append-body
                      "protocol" '(:value ambiguous))))
              (e-runtime-store-test--fire-current-scheduler store))
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
            (should (= (plist-get (e-runtime-store-await store request)
                                  :high-water)
                       1))
            (should (eq (e-runtime-store-request--state request) 'committed))
            (should-not (plist-get (e-runtime-store-status store) :unavailable))
            (should (= (length (plist-get
                                (e-runtime-store-call
                                 store 'read '(:op session-record-page :session-id "protocol"))
                                :records))
                       1))
            (should (= (plist-get
                        (e-runtime-store-call
                         store 'write
                         (e-runtime-store-test--session-append-body
                          "protocol" '(:value recovered) 2))
                        :high-water)
                       2)))
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
    ;; Byte compilation is allowed to erase argument names (Emacs 31 reports
    ;; them as ARG1..ARG3), but it preserves the public arity that excludes
    ;; per-request callback hooks.
    (should (equal (func-arity #'e-runtime-store-submit) '(3 . 3)))))

(ert-deftest e-runtime-store-s2-permission-failure-precedes-mutation ()
  "A mode failure surfaces before BEGIN and leaves no durable mutation."
  (let ((directory (make-temp-file "e-runtime-store-mode-failure-" t)))
    (unwind-protect
        (progn
          (e-runtime-store-worker--open directory "mode-test")
          (let ((request
                 (list :kind 'write :body
                       (e-runtime-store-test--session-append-body
                        "mode-failure" '(:value never)))))
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
             store 'write
             (e-runtime-store-test--session-append-body
              "cancelled" '(:never t))))
      (should (eq (e-runtime-store-cancel store queued) 'dropped))
      (should (eq (e-runtime-store-request--state queued) 'cancelled))
      (setf (e-runtime-store--active-request store) nil)
      (let ((submitted (e-runtime-store-request--create
                        :id "submitted" :kind 'write :state 'submitted)))
        (should (eq (e-runtime-store-cancel store submitted) 'in-flight))))))

(ert-deftest e-runtime-store-s2-applies-restrictive-store-permissions ()
  "Database, WAL, and live ownership files are private."
  (e-runtime-store-test--with-store (store directory)
    (e-runtime-store-call
     store 'write
     (e-runtime-store-test--session-append-body "s" '(:x t)))
    (dolist (file (list (expand-file-name "store.sqlite3" directory)
                        (expand-file-name "store.sqlite3-wal" directory)
                        (expand-file-name "store.sqlite3.owner" directory)))
      (should (file-exists-p file))
      (should (= (logand (file-modes file) #o777) #o600)))
    (should (= (logand (file-modes directory) #o777) #o700))))

(ert-deftest e-runtime-store-s2-bounded-large-response-frames-remain-exact ()
  "A large legal session page flushes as one response without more stdin."
  (let ((e-runtime-store-request-timeout 15.0))
    (e-runtime-store-test--with-store (store directory)
      (let* ((large (make-string (* 512 1024) ?x))
             (records (vector (list :content large)
                              '(:content "tail"))))
        (e-runtime-store-call
         store 'write
         (e-runtime-store-test--session-append-batch-body
          "frames" records))
        (let ((page
               (e-runtime-store-call
                store 'read
                '(:op session-record-page :session-id "frames"
                  :after 0 :limit 2))))
          (should (= (length (plist-get page :records)) 2))
          (let ((content
                 (plist-get (plist-get (car (plist-get page :records))
                                       :value)
                            :content)))
            (should (= (string-bytes content) (string-bytes large)))
            (should (equal (secure-hash 'sha256 content)
                           (secure-hash 'sha256 large))))
          (should (equal
                   (plist-get (plist-get (cadr (plist-get page :records))
                                         :value)
                              :content)
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
                       :id "bounded:w:1" :kind 'write :body body :write-prefix 1))
           (prototype-store (e-runtime-store--create
                             :runtime-id "bounded" :pending (make-hash-table :test 'equal)))
           (canonical-limit
            (string-bytes
             (e-runtime-store-codec-encode
              (e-runtime-store--request-frame prototype-store prototype))))
           (wire-limit
            (e-runtime-store-codec-wire-byte-count canonical-limit)))
      (let ((e-runtime-store-codec-protocol-canonical-byte-limit canonical-limit)
            (e-runtime-store-codec-protocol-wire-byte-limit wire-limit)
            (store (e-runtime-store--create
                    :runtime-id "bounded" :pending (make-hash-table :test 'equal))))
        (unwind-protect
            (cl-letf (((symbol-function 'e-runtime-store--dispatch-next) #'ignore))
              (let ((request (e-runtime-store-submit store 'write body)))
                (should (eq (e-runtime-store-request--state request) 'queued))
                (should (memq request (e-runtime-store--client-queue store)))
                (should (= (string-bytes (e-runtime-store-request--frame request))
                           canonical-limit))))
          (e-runtime-store-test--cancel-store-timers store)
          (e-runtime-store-test--assert-no-store-timers store)))
      (let ((e-runtime-store-codec-protocol-canonical-byte-limit canonical-limit)
            (e-runtime-store-codec-protocol-wire-byte-limit wire-limit)
            (store (e-runtime-store--create
                    :runtime-id "bounded" :pending (make-hash-table :test 'equal))))
        (unwind-protect
            (cl-letf (((symbol-function 'e-runtime-store--dispatch-next) #'ignore))
              (should-error
               (e-runtime-store-submit
                store 'write
                (plist-put (copy-sequence body) :padding
                           (concat (plist-get body :padding) "x")))
               :type 'e-runtime-store-request-too-large)
              (should-not (e-runtime-store--client-queue store))
              (should (= (hash-table-count (e-runtime-store--pending store)) 0)))
          (e-runtime-store-test--cancel-store-timers store)
          (e-runtime-store-test--assert-no-store-timers store))))))

(ert-deftest e-runtime-store-s92-c04-direct-measure-parity-and-preencode-rejection ()
  "Direct measurement matches canonical encoding and rejects before encoding."
  (let ((map (make-hash-table :test 'equal))
        values)
    (puthash "wide" [1 "two" (:nested . 3)] map)
    (puthash :multibyte "Grüße" map)
    (setq values
          (list map ["vector" 7 (:dotted . tail)]
                '(one two . three)
                (list :raw (string-to-multibyte (unibyte-string 128 192 255)))
                "naïve\\newline\n"))
    (dolist (value values)
      (let* ((encoded (e-runtime-store-codec-encode value))
             (exact (string-bytes encoded)))
        (should (= (e-runtime-store-codec-measure-bounded value exact) exact))
        (should-error (e-runtime-store-codec-measure-bounded value (1- exact))
                      :type 'e-runtime-store-codec-too-large)))
    (let* ((tail (list 'b))
           (shared (cons tail tail))
           (exact (string-bytes (e-runtime-store-codec-encode shared))))
      ;; Sharing is not a cycle: only the active recursive path is rejected.
      (should (= exact 69))
      (should (= (e-runtime-store-codec-measure-bounded shared exact) exact)))
    (let ((cycle (vector nil))
          (tail (list 'cycle))
          (cyclic-map (make-hash-table :test 'equal)))
      (aset cycle 0 cycle)
      (setcdr tail tail)
      (puthash :self cyclic-map cyclic-map)
      (dolist (value (list cycle tail cyclic-map))
        (should-error (e-runtime-store-codec-measure-bounded value 100000)
                      :type 'e-runtime-store-codec-error)))
    (let* ((body (list :op 'status :padding (make-string 64 ?x)))
           (request (e-runtime-store-request--create
                     :id "direct-measure:r:1" :kind 'read :body body))
           (exact (string-bytes
                   (e-runtime-store-codec-encode
                    (e-runtime-store--request-frame request))))
           (encoded nil))
      (let ((e-runtime-store-codec-protocol-canonical-byte-limit (1- exact))
            (e-runtime-store-codec-protocol-wire-byte-limit
             (e-runtime-store-codec-wire-byte-count (1- exact))))
        (cl-letf (((symbol-function 'e-runtime-store--encode-frame)
                   (lambda (&rest _args) (setq encoded t) "must-not-encode")))
          (should-error (e-runtime-store--preflight-request request)
                        :type 'e-runtime-store-request-too-large)
          (should-not encoded))))
  ;; Cheap independent count/token rejection must happen before measuring a
  ;; large original caller value at all.
  (dolist (saturation '((e-runtime-store-request-capacity . 0)
                        (e-runtime-store-notification-capacity . 0)))
    (let* ((store (e-runtime-store--create
                   :runtime-id "cheap-reject" :pending (make-hash-table :test 'equal)))
           (measured nil))
      (cl-letf (((symbol-function 'e-runtime-store-codec-measure-bounded)
                 (lambda (&rest _args) (setq measured t) 1))
                ((symbol-function 'e-runtime-store--schedule) #'ignore))
        (let ((e-runtime-store-request-capacity
               (if (eq (car saturation) 'e-runtime-store-request-capacity) 0 128))
              (e-runtime-store-notification-capacity
               (if (eq (car saturation) 'e-runtime-store-notification-capacity) 0 128)))
          (should-error
           (e-runtime-store-submit store 'read
                                   (list :op 'status :padding (make-string 4096 ?x)))
           :type 'e-runtime-store-capacity-exhausted)
          (should-not measured)))))))

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

(ert-deftest e-runtime-store-s92-c04-request-frame-survives-until-terminal-resolution ()
  "A bounded canonical frame remains the only replay authority until terminal."
  (let* ((store (e-runtime-store--create
                 :directory "frame-test" :runtime-id "frame"
                 :pending (make-hash-table :test 'equal)))
         ordinary-wire)
    (cl-letf (((symbol-function 'e-runtime-store--start-process)
               (lambda (candidate)
                 (setf (e-runtime-store--process candidate) 'ordinary-process
                       (e-runtime-store--opened-process candidate) 'ordinary-process)
                 'ordinary-process))
              ((symbol-function 'e-runtime-store--ensure-worker-open) #'ignore)
              ((symbol-function 'e-runtime-store--schedule) #'ignore)
              ((symbol-function 'process-send-string)
               (lambda (_process wire) (setq ordinary-wire wire))))
      (let ((request (e-runtime-store-submit store 'write '(:op status))))
        (should (eq (e-runtime-store-request--state request) 'queued))
        (e-runtime-store-test--fire-current-scheduler store)
        (should (eq (e-runtime-store-request--state request) 'submitted))
        (should ordinary-wire)
        (should (e-runtime-store-request--frame request))
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
               (lambda (_process wire) (setq open-wire wire))))
      (e-runtime-store--ensure-worker-open store)
      (setq open-request (e-runtime-store--active-request store)))
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
    (setf (e-runtime-store--client-queue store) (list cancelled))
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
         (half (/ (length line-one) 2)))
    (puthash "second" second (e-runtime-store--pending store))
    (puthash "third" third (e-runtime-store--pending store))
    (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
      (e-runtime-store--consume-output store (substring line-one 0 half))
      (e-runtime-store--consume-output store (concat (substring line-one half) "\n"))
      (setf (e-runtime-store--active-request store) second)
      (e-runtime-store--consume-output store (concat line-two "\n"))
      (setf (e-runtime-store--active-request store) third)
      (e-runtime-store--consume-output store (concat line-three "\n")))
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
      ;; DP5A leaves the bounded transport request scheduler-owned while the
      ;; timer advances DP4 replacement; a caller/filter does not settle it.
      (should (eq (e-runtime-store-request--state request) 'submitted))
      (should (= (e-runtime-store--recovery-attempt overflow-store) 1))
      (e-runtime-store-test--cancel-store-timers overflow-store))))

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

(ert-deftest e-runtime-store-s92-c04-worker-resource-and-identity-bounds ()
  "Worker backstops resource limits and pages identities at real boundaries."
  (let ((directory (make-temp-file "e-runtime-store-c04-worker-" t)))
    (unwind-protect
        (progn
          (e-runtime-store-worker--open directory "c04-worker")
          ;; Catalog/checkpoint replicas are retired in v6.  Bound an actual
          ;; authoritative resource value instead of preserving those APIs in
          ;; the transport suite.
          (let ((e-runtime-store-worker-resource-byte-limit 1))
            (should (= (plist-get
                        (e-runtime-store-worker--resource-put
                         '(:lineage-id "bounds" :session-id "bounds"
                           :path "exact" :content "x"))
                        :bytes)
                       1))
            (should-error
             (e-runtime-store-worker--resource-put
              '(:lineage-id "bounds" :session-id "bounds"
                :path "over" :content "xx"))
             :type 'e-runtime-store-resource-too-large))
          (dolist (session-id '("a" "b" "c"))
            (e-runtime-store-session-worker--write-delta
             e-runtime-store-worker--database session-id
             (e-runtime-store-test--query-state session-id 1)))
          (let ((e-runtime-store-session-worker-page-row-limit 2)
                (e-runtime-store-session-worker-id-page-byte-limit 2))
            (let ((first-page
                   (e-runtime-store-session-worker--id-page
                    e-runtime-store-worker--database
                    '(:cursor nil :limit 2))))
              ;; Three short identities exercise the exact two-row page plus
              ;; one lookahead; the third never leaks into the first result.
              (should (equal (plist-get first-page :ids) '("a" "b")))
              (should (= (plist-get first-page :byte-count) 2))
              (should (equal (plist-get first-page :next) "b"))
              (should (equal
                       (plist-get
                        (e-runtime-store-session-worker--id-page
                         e-runtime-store-worker--database
                         '(:cursor "b" :limit 2))
                        :ids)
                       '("c"))))
          (let ((e-runtime-store-session-worker-page-row-limit 3)
                (e-runtime-store-session-worker-id-page-byte-limit 2))
            (let ((byte-page
                   (e-runtime-store-session-worker--id-page
                    e-runtime-store-worker--database
                    '(:cursor nil :limit 3))))
              ;; The exact two-byte page admits A and B; C is the one-byte
              ;; overflow and must be deferred to the cursor successor.
              (should (equal (plist-get byte-page :ids) '("a" "b")))
              (should (= (plist-get byte-page :byte-count) 2))
              (should (equal (plist-get byte-page :next) "b"))
              (should (equal
                       (plist-get
                        (e-runtime-store-session-worker--id-page
                         e-runtime-store-worker--database
                         '(:cursor "b" :limit 3))
                        :ids)
                       '("c"))))))
      (e-runtime-store-worker--close)
      (delete-directory directory t)))))

(ert-deftest e-runtime-store-s92-c06-submit-is-cold-and-scheduler-owned ()
  "Submission returns its immutable handle before a cold worker is advanced."
  (let* ((store (e-runtime-store--create
                 :directory "/tmp/" :runtime-id "cold"
                 :pending (make-hash-table :test 'equal)))
         (starts 0) (opens 0))
    (unwind-protect
        (cl-letf (((symbol-function 'e-runtime-store--start-process)
               (lambda (runtime)
                 (cl-incf starts)
                 (setf (e-runtime-store--process runtime) 'cold-worker)
                 'cold-worker))
              ((symbol-function 'e-runtime-store--ensure-worker-open)
               (lambda (_runtime) (cl-incf opens))))
          (let* ((body (e-runtime-store-test--session-append-body
                        "cold" '(:value immutable)))
             (request
             (e-runtime-store-submit
              store 'write body)))
        (should (eq (e-runtime-store-request--state request) 'queued))
        (should (e-runtime-store-request--frame request))
        ;; The caller still owns BODY; the runtime keeps only the immutable
        ;; frame and the tiny operation fact used in diagnostics.
        (should-not (e-runtime-store-request--body request))
        (should (eq (e-runtime-store--request-operation request)
                    'session-append))
        (should (= starts 0))
        (should (= opens 0))
        (e-runtime-store-test--fire-current-scheduler store)
        (should (= starts 1))
        (should (= opens 1))
        (should (memq request (e-runtime-store--client-queue store)))))
      (e-runtime-store-test--cancel-store-timers store)
      (e-runtime-store-test--assert-no-store-timers store))))

(ert-deftest e-runtime-store-s92-c06-await-observes-but-watchdog-recovers ()
  "An overdue submitted write recovers from the timer, not an awaiter."
  (let* ((clock 61.0)
         (request (e-runtime-store-request--create
                   :id "watchdog:w:1" :kind 'write :body '(:op status)
                   :state 'submitted :submitted-at 0.0 :frame "retained"))
         (store (e-runtime-store--create
                 :runtime-id "watchdog" :active-request request
                 :pending (make-hash-table :test 'equal)))
         (recoveries 0))
    (puthash (e-runtime-store-request--id request) request
             (e-runtime-store--pending store))
    (unwind-protect
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) clock))
                  ((symbol-function 'sit-for) (lambda (&rest _args) nil))
                  ((symbol-function 'e-runtime-store--recover-or-fail)
                   (lambda (_store _cause) (cl-incf recoveries))))
          (should-error (e-runtime-store-await store request 1.0)
                        :type 'e-runtime-store-timeout)
          (should (= recoveries 0))
          (e-runtime-store--scheduler-fired
           store (e-runtime-store--scheduler-generation store))
          (should (= recoveries 1)))
      (e-runtime-store-test--cancel-store-timers store))))

(ert-deftest e-runtime-store-s92-c06-terminal-observer-is-local-and-one-shot ()
  "Observer failure occurs outside settlement and cannot poison the store."
  (let* ((store (e-runtime-store--create
                 :runtime-id "observe" :pending (make-hash-table :test 'equal)))
         (request (e-runtime-store-request--create
                   :id "observe:r:1" :kind 'read :body '(:op status)
                   :state 'submitted :frame "retained" :retained-bytes 17
                   :notification 'reserved))
         (reservation (e-runtime-store--reservation-create :limit 100 :used 17))
         (calls 0))
    (setf (e-runtime-store--reservation store) reservation
          (e-runtime-store--request-count store) 1
          (e-runtime-store--reserved-bytes store) 17
          (e-runtime-store--notification-count store) 1
          (e-runtime-store--active-request store) request)
    (puthash (e-runtime-store-request--id request) request
             (e-runtime-store--pending store))
    (e-runtime-store--observe request (lambda (_request)
                                        (cl-incf calls)
                                        (error "client failure")))
    (e-runtime-store--settle store request
                             '(:id "observe:r:1" :ok t :result (:ok t)))
    (should (eq (e-runtime-store-request--state request) 'committed))
    (should-not (e-runtime-store--unavailable store))
    (e-runtime-store--drain-terminal-notifications store)
    (should (= calls 1))
    (should (= (e-runtime-store--request-count store) 0))
    (should (= (e-runtime-store--reservation-used reservation) 0))
    (e-runtime-store--drain-terminal-notifications store)
    (should (= calls 1))))

(ert-deftest e-runtime-store-s92-c06-observer-quit-cannot-strand-outbox ()
  "A quitting observer is client-local; the next terminal observer still runs."
  (let* ((reservation (e-runtime-store--reservation-create :limit 10 :used 2))
         (store (e-runtime-store--create
                 :runtime-id "observer-quit" :reservation reservation
                 :request-count 2 :reserved-bytes 2 :notification-count 2))
         (first (e-runtime-store-request--create
                 :id "quit:r:1" :kind 'read :state 'submitted
                 :retained-bytes 1 :notification 'queued))
         (second (e-runtime-store-request--create
                  :id "quit:r:2" :kind 'read :state 'submitted
                  :retained-bytes 1 :notification 'queued))
         (second-calls 0))
    (unwind-protect
        (progn
          (e-runtime-store--observe
           first (lambda (_request) (signal 'quit nil)))
          (e-runtime-store--observe
           second (lambda (_request) (cl-incf second-calls)))
          (setf (e-runtime-store--notification-outbox store) (list first second))
          (e-runtime-store--drain-terminal-notifications store)
          (should (= second-calls 1))
          (should-not (e-runtime-store--notification-outbox store))
          (should (= (e-runtime-store--notification-count store) 0))
          (should (= (e-runtime-store--request-count store) 0))
          (should (= (e-runtime-store--reservation-used reservation) 0)))
      (e-runtime-store-test--cancel-store-timers store)
      (e-runtime-store-test--assert-no-store-timers store))))

(ert-deftest e-runtime-store-s92-c06-terminal-drain-yields-after-one-page ()
  "A 17th terminal observation yields to an independent timer after 16 units."
  (let* ((reservation (e-runtime-store--reservation-create :limit 64 :used 17))
         (store (e-runtime-store--create
                 :runtime-id "drain" :reservation reservation
                 :reserved-bytes 17 :request-count 17 :notification-count 17))
         (requests
          (cl-loop for number from 1 to 17
                   collect (e-runtime-store-request--create
                            :id (format "drain:r:%d" number) :kind 'read
                            :state 'submitted :retained-bytes 1
                            :notification 'queued)))
         (delivered 0) heartbeat heartbeat-timer)
    (unwind-protect
        (progn
          (dolist (request requests)
            (e-runtime-store--observe request
                                      (lambda (_request) (cl-incf delivered))))
          (setf (e-runtime-store--notification-outbox store) requests)
          ;; This timer is queued first.  Its assertion makes the page boundary
          ;; observable: the 17th delivery cannot run until the scheduler yielded.
          (setq heartbeat-timer
                (run-at-time 0 nil (lambda () (setq heartbeat (= delivered 16)))))
          (e-runtime-store--drain-terminal-notifications store)
          (should (= delivered e-runtime-store-notification-drain-limit))
          (should (= (length (e-runtime-store--notification-outbox store)) 1))
          (sit-for 0.02)
          (should heartbeat)
          (should (= delivered 17))
          (should (= (e-runtime-store--request-count store) 0))
          (should (= (e-runtime-store--notification-count store) 0))
          (should (= (e-runtime-store--reservation-used reservation) 0)))
      (when (timerp heartbeat-timer) (cancel-timer heartbeat-timer))
      (when (timerp (e-runtime-store--notification-timer store))
        (cancel-timer (e-runtime-store--notification-timer store))))))

(ert-deftest e-runtime-store-s92-c06-capacity-and-read-detach-release-exactly-once ()
  "Admission reserves count/bytes/token atomically and terminal release is exact."
  (let* ((e-runtime-store-request-capacity 1)
         (e-runtime-store-notification-capacity 1)
         (reservation (e-runtime-store--reservation-create :limit 4096))
         (store (e-runtime-store--create
                 :runtime-id "capacity" :reservation reservation
                 :pending (make-hash-table :test 'equal))))
    (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
      (let ((request (e-runtime-store-submit store 'read '(:op status))))
        (should (= (e-runtime-store--request-count store) 1))
        (should-error (e-runtime-store-submit store 'read '(:op status))
                      :type 'e-runtime-store-capacity-exhausted)
        (should (eq (e-runtime-store-cancel store request) 'dropped))
        ;; Dropped work still owns its reserved completion token until the
        ;; bounded drain releases it; an admitted completion cannot be lost.
        (should (= (e-runtime-store--notification-count store) 1))
        (e-runtime-store--drain-terminal-notifications store)
        (should (= (e-runtime-store--request-count store) 0))
        (should (= (e-runtime-store--reservation-used reservation) 0))))
    ;; Reservation precedes construction of the immutable string, so a
    ;; construction failure must roll that provisional admission back too.
    (let ((store (e-runtime-store--create
                  :runtime-id "capacity-encode" :reservation reservation
                  :pending (make-hash-table :test 'equal))))
      (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore)
                ((symbol-function 'e-runtime-store--encode-frame)
                 (lambda (&rest _args)
                   (signal 'file-error '("forced encode failure")))))
        (should-error (e-runtime-store-submit store 'read '(:op status))
                      :type 'file-error)
        (should (= (e-runtime-store--request-count store) 0))
        (should (= (e-runtime-store--reserved-bytes store) 0))
        (should (= (e-runtime-store--notification-count store) 0))
        (should (= (e-runtime-store--reservation-used reservation) 0)))))
  ;; The shared reservation is composition-owned: two independent runtimes
  ;; cannot each admit a frame which would exceed the one aggregate budget.
  (let* ((probe (e-runtime-store-request--create
                 :id "global-a:r:1" :kind 'read :body '(:op status)))
         (probe-store (e-runtime-store--create
                       :runtime-id "global" :pending (make-hash-table :test 'equal))))
    (e-runtime-store--preflight-request probe-store probe)
    (let* ((bytes (e-runtime-store-request--frame-bytes probe))
           (shared (e-runtime-store--reservation-create :limit bytes))
           (first (e-runtime-store--create
                   :runtime-id "global-a" :reservation shared
                   :pending (make-hash-table :test 'equal)))
           (second (e-runtime-store--create
                    :runtime-id "global-b" :reservation shared
                    :pending (make-hash-table :test 'equal))))
      (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
        (let ((request (e-runtime-store-submit first 'read '(:op status))))
          (should (= (e-runtime-store--reservation-used shared) bytes))
          (should-error (e-runtime-store-submit second 'read '(:op status))
                        :type 'e-runtime-store-capacity-exhausted)
          (e-runtime-store-cancel first request)
          (e-runtime-store--drain-terminal-notifications first)
          (should (= (e-runtime-store--reservation-used shared) 0))))))
  (let* ((store (e-runtime-store--create
                 :runtime-id "detach" :pending (make-hash-table :test 'equal)))
         (request (e-runtime-store-request--create
                   :id "detach:r:1" :kind 'read :body '(:op status)
                   :state 'submitted :frame "retained" :retained-bytes 3
                   :notification 'reserved))
         (called nil))
    (setf (e-runtime-store--active-request store) request
          (e-runtime-store--request-count store) 1
          (e-runtime-store--reserved-bytes store) 3
          (e-runtime-store--notification-count store) 1
          (e-runtime-store--reservation store)
          (e-runtime-store--reservation-create :limit 10 :used 3))
    (puthash "detach:r:1" request (e-runtime-store--pending store))
    (e-runtime-store--observe request (lambda (_request) (setq called t)))
    (should (eq (e-runtime-store-cancel store request) 'detached))
    (should (eq (e-runtime-store-request--state request) 'submitted))
    (e-runtime-store--settle store request
                             '(:id "detach:r:1" :ok t :result (:ok t)))
    ;; Detached reads bypass the outbox and release their exact reservation at
    ;; terminal settlement; no later timer/drain owns an invisible callback.
    (should-not (e-runtime-store--notification-outbox store))
    (should (= (e-runtime-store--notification-count store) 0))
    (should (= (e-runtime-store--request-count store) 0))
    (should (= (e-runtime-store--reserved-bytes store) 0))
    (should (= (e-runtime-store--reservation-used
                (e-runtime-store--reservation store)) 0))
    (e-runtime-store--drain-terminal-notifications store)
    (should-not called)
    (should (= (e-runtime-store--request-count store) 0))))

(ert-deftest e-runtime-store-s92-c07-frame-escrow-handoff-is-exact-on-failure-and-terminal-release ()
  "A failed transfer leaves composition escrow for its sole rollback owner."
  (let* ((body '(:op status))
         (reservation (e-runtime-store--reservation-create :limit (* 1024 1024)))
         (store (e-runtime-store--create
                 :runtime-id "escrow" :reservation reservation
                 :pending (make-hash-table :test 'equal)))
         (escrow (e-runtime-store--measure-frame-escrow store 'read body)))
    ;; The session composition owns this first charge.  Runtime preflight may
    ;; borrow it, but a failure before a retained frame exists must restore it
    ;; rather than making the caller's rollback double-release it.
    (setf (e-runtime-store--reservation-used reservation) escrow)
    (cl-letf (((symbol-function 'e-runtime-store--encode-frame)
               (lambda (&rest _)
                 (signal 'file-error '("injected encode failure")))))
      (should-error (e-runtime-store--submit-with-frame-escrow
                     store 'read body escrow)
                    :type 'file-error))
    (should (= (e-runtime-store--request-count store) 0))
    (should (= (e-runtime-store--notification-count store) 0))
    (should (= (e-runtime-store--reserved-bytes store) 0))
    (should (= (e-runtime-store--reservation-used reservation) escrow))
    ;; The composition now performs its one rollback.
    (cl-decf (e-runtime-store--reservation-used reservation) escrow)
    (should (= (e-runtime-store--reservation-used reservation) 0))
    ;; A successful transfer replaces escrow with the actual frame.  There is
    ;; no second composition release after this point; runtime terminal
    ;; settlement releases that exact surviving allocation once.
    (setf (e-runtime-store--reservation-used reservation) escrow)
    (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
      (let ((request (e-runtime-store--submit-with-frame-escrow
                      store 'read body escrow)))
        (should (<= (e-runtime-store-request--frame-bytes request) escrow))
        ;; Escrow admission has the same queued/correlation partition as the
        ;; public API: correlation begins only at physical dispatch.
        (should-not (gethash (e-runtime-store-request--id request)
                             (e-runtime-store--pending store)))
        (should (= (e-runtime-store--reservation-used reservation)
                   (e-runtime-store-request--frame-bytes request)))
        (should (eq (e-runtime-store-cancel store request) 'dropped))
        (e-runtime-store--drain-terminal-notifications store)
        (should (= (e-runtime-store--request-count store) 0))
        (should (= (e-runtime-store--notification-count store) 0))
        (should (= (e-runtime-store--reservation-used reservation) 0))))))

(ert-deftest e-runtime-store-s92-c06-stale-scheduler-generation-is-inert ()
  "A replaced earliest-deadline callback cannot advance a newer schedule."
  (let* ((store (e-runtime-store--create
                 :runtime-id "stale" :pending (make-hash-table :test 'equal)))
         (runs 0))
    (cl-letf (((symbol-function 'e-runtime-store--dispatch-next)
               (lambda (_store) (cl-incf runs))))
      (setf (e-runtime-store--scheduler-generation store) 4)
      (e-runtime-store--scheduler-fired store 3)
      (should (= runs 0)))))

(ert-deftest e-runtime-store-s92-c06-close-releases-queued-reservations ()
  "Close releases every pre-reserved queue frame and terminal token once."
  (let* ((directory (make-temp-file "e-runtime-store-c06-close-" t))
         (store (e-runtime-store-open directory)))
    (unwind-protect
        (progn
          ;; Keep cold open in flight so the domain request remains queued.
          (let ((request
                 (e-runtime-store-submit
                  store 'write
                  (e-runtime-store-test--session-append-body
                   "close" '(:value queued)))))
            (should (= (e-runtime-store--request-count store) 1))
            (e-runtime-store-close store)
            (should (eq (e-runtime-store-request--state request) 'failed))
            (should (= (e-runtime-store--request-count store) 0))
            (should (= (e-runtime-store--reserved-bytes store) 0))
            (should (= (e-runtime-store--notification-count store) 0))))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-c06-overdue-expiry-pages-and-yields ()
  "Seventeen overdue requests expire in two scheduler turns with a heartbeat."
  (let* ((clock 61.0)
         (requests (cl-loop for n from 1 to 17
                            collect (e-runtime-store-request--create
                                     :id (format "expiry:r:%d" n) :kind 'read
                                     :body '(:op status) :state 'queued
                                     :admitted-at 0.0)))
         (store (e-runtime-store--create :runtime-id "expiry"
                                         :client-queue requests
                                         :pending (make-hash-table :test 'equal)))
         heartbeat heartbeat-timer)
    (unwind-protect
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) clock)))
          (setq heartbeat-timer
                (run-at-time 0 nil
                             (lambda ()
                               (setq heartbeat
                                     (= 16 (cl-count 'failed requests
                                                     :key #'e-runtime-store-request--state))))))
          (e-runtime-store--scheduler-fired store
                                            (e-runtime-store--scheduler-generation store))
          (should (= 16 (cl-count 'failed requests
                                  :key #'e-runtime-store-request--state)))
          (should (= 1 (length (e-runtime-store--client-queue store))))
          (sit-for 0.02)
          (should heartbeat)
          (sit-for 0.02)
          (should (= 17 (cl-count 'failed requests
                                  :key #'e-runtime-store-request--state))))
      (when (timerp (e-runtime-store--scheduler-timer store))
        (cancel-timer (e-runtime-store--scheduler-timer store)))
      (when (timerp heartbeat-timer) (cancel-timer heartbeat-timer)))))

(ert-deftest e-runtime-store-s92-c06-stale-starting-request-never-explains-open ()
  "Cancelled/expired selection clears before every open outcome reselects."
  (dolist (removal '(cancel expire))
    (dolist (open-success '(nil t))
      (dolist (successor '(nil t))
        (let* ((stale (e-runtime-store-request--create
                       :id "start:stale" :kind 'write :body '(:op stale)
                       :state 'queued :admitted-at 0.0))
               (next (and successor
                          (e-runtime-store-request--create
                           :id "start:next" :kind 'write :body '(:op next)
                           ;; The successor was admitted later, so expiry of
                           ;; the selected predecessor cannot terminalize it.
                           :state 'queued :admitted-at 2.0)))
               (open (e-runtime-store-request--create
                      :id "start:open" :kind 'open :state 'submitted))
               (store (e-runtime-store--create
                       :runtime-id "start" :active-request open
                       :starting-request stale :client-queue (delq nil (list stale next))
                       :process 'test-process :pending (make-hash-table :test 'equal)))
               scheduled)
          (cl-letf (((symbol-function 'e-runtime-store--schedule)
                     (lambda (&rest _args) (setq scheduled t)))
                    ((symbol-function 'e-runtime-store--start-process) #'ignore)
                    ((symbol-function 'e-runtime-store--ensure-worker-open) #'ignore)
                    ((symbol-function 'e-runtime-store--preflight-request)
                     (lambda (&rest _args) "frame"))
                    ((symbol-function 'process-send-string) #'ignore))
            (if (eq removal 'cancel)
                (e-runtime-store-cancel store stale)
              (cl-letf (((symbol-function 'float-time) (lambda (&optional _x) 61.0)))
                (e-runtime-store--expire-overdue-queued store 60.0)))
            (should-not (e-runtime-store--starting-request store))
            (e-runtime-store--settle
             store open
             (if open-success
                 '(:id "start:open" :ok t :result (:opened t))
               '(:id "start:open" :ok nil :error-symbol e-runtime-store-error
                 :error-data ("open failure"))))
            (should scheduled)
            (cond
             ((not open-success)
              (if successor
                  (progn
                    (should (eq (e-runtime-store-request--state next) 'failed))
                    (should (eq (plist-get (cddr (e-runtime-store-request--error next))
                                           :operation)
                                'next)))
                (should-not (e-runtime-store--starting-request store))))
             (successor
              ;; The post-open scheduler chooses the surviving queue head,
              ;; not the stale request that was terminal while open was live.
              (setf (e-runtime-store--opened-process store) 'test-process)
              (e-runtime-store--dispatch-next store)
              (should (eq (e-runtime-store--active-request store) next))
              (should (eq (e-runtime-store-request--state next) 'submitted))
              (should-not (e-runtime-store--starting-request store)))
             (t
              (should-not (e-runtime-store--starting-request store))))))))))

(ert-deftest e-runtime-store-s92-c06-startup-failures-select-live-domain-owner ()
  "Every initial-open failure uses the surviving domain request, never open."
  (dolist (removal '(cancel expire))
    (dolist (mechanism '(response worker-exit malformed timeout))
      (let* ((stale (e-runtime-store-request--create
                     :id "startup:stale" :kind 'write :body '(:op stale)
                     :state 'queued :admitted-at 0.0))
             (next (e-runtime-store-request--create
                    :id "startup:next" :kind 'write :body '(:op next)
                    :state 'queued :admitted-at 2.0))
             (open (e-runtime-store-request--create
                    :id "startup:open" :kind 'open :state 'submitted
                    :submitted-at 0.0))
             (store (e-runtime-store--create
                     :runtime-id "startup" :active-request open
                     :starting-request stale :client-queue (list stale next)
                     :pending (make-hash-table :test 'equal))))
        (puthash (e-runtime-store-request--id open) open
                 (e-runtime-store--pending store))
        (unwind-protect
            (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
              (if (eq removal 'cancel)
                  (e-runtime-store-cancel store stale)
                (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 61.0)))
                  (e-runtime-store--expire-overdue-queued store 60.0)))
              (pcase mechanism
                ('response
                 (e-runtime-store--settle
                  store open
                  '(:id "startup:open" :ok nil
                    :error-symbol e-runtime-store-error :error-data ("open failed"))))
                ('worker-exit (e-runtime-store--worker-exited store))
                ('malformed (e-runtime-store--consume-response-line store "not-a-frame"))
                ('timeout
                 (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 61.0)))
                   (e-runtime-store--recover-overdue-active store 60.0))))
              (should (eq (e-runtime-store-request--state next) 'failed))
              (let ((error (e-runtime-store-request--error next)))
                (should (eq (plist-get (cddr error) :operation) 'next))
                (should (eq (plist-get (cddr error) :kind) 'write))
                (should (equal (plist-get (cddr error) :request-id) "startup:next")))
              (should-not (e-runtime-store--starting-request store))
              (should-not (e-runtime-store--unavailable store)))
          (e-runtime-store-test--cancel-store-timers store)
          (e-runtime-store-test--assert-no-store-timers store))))))

(ert-deftest e-runtime-store-schema-upgrade-is-global-not-owner-suspect ()
  "A too-old schema stops transport without blaming queued write owners."
  (let* ((write (e-runtime-store-request--create
                 :id "upgrade:write" :kind 'write :body '(:op board-create)
                 :operation 'board-create :state 'queued :admitted-at 0.0
                 :owner-key '(board . "board-upgrade")))
         (read (e-runtime-store-request--create
                :id "upgrade:read" :kind 'read :body '(:op status)
                :operation 'status :state 'queued :admitted-at 0.0))
         (open (e-runtime-store-request--create
                :id "upgrade:open" :kind 'open :state 'submitted
                :submitted-at 0.0))
         (store (e-runtime-store--create
                 :runtime-id "upgrade" :active-request open
                 :starting-request write :client-queue (list write read)
                 :process 'test-process
                 :pending (make-hash-table :test 'equal)
                 :suspect-owners (make-hash-table :test 'equal))))
    (puthash (e-runtime-store-request--id open) open
             (e-runtime-store--pending store))
    (unwind-protect
        (cl-letf (((symbol-function 'e-runtime-store--schedule) #'ignore))
          (e-runtime-store--settle
           store open
           '(:id "upgrade:open" :ok nil
             :error-symbol e-runtime-store-schema-too-old
             :error-data (:actual 5 :required 7
                          :operation e-runtime-store-offline-upgrade)))
          (should (e-runtime-store--unavailable store))
          (should (eq (car (e-runtime-store--unavailable-cause store))
                      'e-runtime-store-schema-too-old))
          (should (eq (car (e-runtime-store-request--error write))
                      'e-runtime-store-schema-too-old))
          (should (eq (car (e-runtime-store-request--error read))
                      'e-runtime-store-schema-too-old))
          (should (= (hash-table-count
                      (e-runtime-store--suspect-owners store))
                     0))
          (should-not (e-runtime-store--client-queue store)))
      (e-runtime-store-test--cancel-store-timers store))))

(ert-deftest e-runtime-store-task-runnable-claim-is-atomic-and-bounded ()
  "Two claims select one queued task once without reading terminal history."
  (e-runtime-store-test--with-store (store directory)
    (ignore directory)
    (e-runtime-store-call
     store 'write
     (list :op 'task-enqueue :queue-id "atomic"
           :record '(:task-id "task-a" :status queued :prompt "work"
                     :attempt-number 0)))
    (let* ((first
            (e-runtime-store-call
             store 'write
             '(:op task-runnable-claim :queue-id "atomic"
               :started-at "2026-09-06T00:00:00+0000"
               :harness-instance-id "test")))
           (second
            (e-runtime-store-call
             store 'write
             '(:op task-runnable-claim :queue-id "atomic"
               :started-at "2026-09-06T00:00:01+0000"
               :harness-instance-id "test"))))
      (should (plist-get first :claimed-p))
      (should (equal (plist-get first :task-id) "task-a"))
      (should-not (plist-get second :claimed-p))
      (should (equal (plist-get (plist-get first :record) :status)
                     'running)))))

(ert-deftest e-runtime-store-task-snapshot-query-count-is-constant ()
  "A snapshot reads queue, task/latest-attempt rows, and history as sets."
  (let* ((database-file (make-temp-file "e-task-snapshot-"))
         (database (sqlite-open database-file)))
    (unwind-protect
        (progn
          (sqlite-execute database "PRAGMA foreign_keys=ON")
          (e-task-storage-sqlite-worker-initialize database)
          (e-task-storage-sqlite-worker-write
           database '(:op task-queue-open :queue-id "bounded-snapshot"))
          (dotimes (index 4)
            (e-task-storage-sqlite-worker-write
             database
             (list :op 'task-enqueue :queue-id "bounded-snapshot"
                   :record
                   (list :task-id (format "task-%d" index)
                         :status 'queued :prompt (format "work-%d" index))))
            (e-task-storage-sqlite-worker-write
             database
             (list :op 'task-runnable-claim :queue-id "bounded-snapshot"
                   :started-at (format "2026-09-06T00:00:0%dZ" index)
                   :harness-instance-id "actual")))
          (let ((select (symbol-function 'sqlite-select))
                (query-count 0)
                result)
            (cl-letf (((symbol-function 'sqlite-select)
                       (lambda (&rest arguments)
                         (cl-incf query-count)
                         (apply select arguments))))
              (setq result
                    (e-task-storage-sqlite-worker-read
                     database
                     '(:op task-snapshot :queue-id "bounded-snapshot"
                       :limit 16))))
            (should (= query-count 3))
            (should (= (length (plist-get result :records)) 4))
            (should (= (length (plist-get result :attempts)) 4))
            (should
             (seq-every-p
              (lambda (record)
                (equal (plist-get record :harness-instance-id) "actual"))
              (plist-get result :records)))))
      (setq e-task-storage-sqlite-worker--database nil)
      (sqlite-close database)
      (delete-file database-file))))

(ert-deftest e-runtime-store-task-legacy-import-returns-whole-result ()
  "Legacy import completes after all records and stores content only once."
  (let* ((database-file (make-temp-file "e-task-import-"))
         (database (sqlite-open database-file)))
    (unwind-protect
        (progn
          (sqlite-execute database "PRAGMA foreign_keys=ON")
          (e-task-storage-sqlite-worker-initialize database)
          (e-task-storage-sqlite-worker-write
           database '(:op task-queue-open :queue-id "legacy-import"))
          (let ((result
                 (e-task-storage-sqlite-worker-write
                  database
                  '(:op task-import-legacy-snapshot :queue-id "legacy-import"
                    :snapshot
                    (:sequence 9 :paused-p t
                     :order ("first" "second")
                     :records
                     ((:task-id "first" :status queued :revision 7
                       :harness-instance-id "requested" :retries 2
                       :prompt "one")
                      (:task-id "second" :status done :revision 8
                       :outputs ("done") :prompt "two")))))))
            (should (= (plist-get result :records) 2))
            (should (= (plist-get result :sequence) 9))
            (should (plist-get result :paused-p)))
          (let* ((row (car (sqlite-select
                            database
                            "SELECT payload FROM task_records WHERE queue_id=? AND task_id=?"
                            ["legacy-import" "first"])))
                 (payload
                  (e-task-storage-sqlite-worker--unpack
                   (e-task-storage-sqlite-worker--column row 0))))
            (should (equal payload '(:prompt "one")))
            (should-not (plist-member payload :task-id))
            (should-not (plist-member payload :harness-instance-id))))
      (setq e-task-storage-sqlite-worker--database nil)
      (sqlite-close database)
      (delete-file database-file))))

(ert-deftest e-runtime-store-task-queue-retains-only-live-work ()
  "Persistent task history stays in SQLite after one live task settles."
  (e-runtime-store-test--with-store (store directory)
    (ignore directory)
    (let* ((settler nil)
           (storage (e-task-storage-sqlite-create store))
           (queue
            (e-task-queue-create
             :id "live-only" :storage storage
             :default-harness-instance-id "test"
             :runner
             (lambda (_task _harness on-settle)
               (setq settler on-settle)
               (list :cancel #'ignore))))
           optimistic task-id)
      (cl-letf (((symbol-function 'e-harness-instance-get-or-create)
                 (lambda (_instance-id) :test-harness)))
        (setq optimistic (e-task-queue-enqueue queue :prompt "bounded work")
              task-id (plist-get optimistic :task-id))
        (should (plist-get (gethash task-id (e-task-queue-records queue))
                           :persistence-pending))
        (e-runtime-store-test--wait-until (lambda () settler))
        (should (= (hash-table-count (e-task-queue-records queue)) 1))
        (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                    'running))
        (funcall settler :status 'done :outputs '("done"))
        (e-runtime-store-test--wait-until
         (lambda () (= (hash-table-count (e-task-queue-records queue)) 0)))
        (let ((page
               (e-runtime-store-call
                store 'read
                '(:op task-snapshot :queue-id "live-only" :limit 8))))
          (should (= (length (plist-get page :records)) 1))
          (should (equal (plist-get (car (plist-get page :records)) :status)
                         'done)))))))

(ert-deftest e-runtime-store-zz-c06-selector-leaves-no-runtime-timer-callbacks ()
  "The focused owner selector leaves no timer after cleared fixture fields."
  (e-runtime-store-test--assert-no-runtime-timer-callbacks))

(provide 'e-runtime-store-test)

;;; e-runtime-store-test.el ends here
