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

(ert-deftest e-runtime-store-s2-commit-dedup-conflict-and-revision ()
  "Committed commands deduplicate exactly and conflicts abort cleanly."
  (e-runtime-store-test--with-store (store directory)
    (let* ((body '(:op session-append :session-id "s"
                   :expected-revision 0 :record (:type "session" :x nil)))
           (id "stable-command")
           (first (e-runtime-store-call store 'write body id)))
      (should (= (plist-get first :revision) 1))
      (should (equal (e-runtime-store-call store 'write body id) first))
      (should-error
       (e-runtime-store-call
        store 'write '(:op session-append :session-id "s"
                       :expected-revision 1 :record (:type "other")) id)
       :type 'e-runtime-store-command-conflict)
      (should-error
       (e-runtime-store-call
        store 'write '(:op session-append :session-id "s"
                       :expected-revision 0 :record (:type "late")))
       :type 'e-runtime-store-revision-conflict)
      (let ((page (e-runtime-store-call
                   store 'read '(:op session-record-page :session-id "s"))))
        (should (= (length (plist-get page :records)) 1))
        (should (equal (plist-get (car (plist-get page :records)) :value)
                       '(:type "session" :x nil)))))))

(ert-deftest e-runtime-store-s2-rejects-a-second-live-runtime ()
  "One live worker exclusively owns one physical database."
  (e-runtime-store-test--with-store (store directory)
    (should-error (e-runtime-store-open directory :runtime-id "second")
                  :type 'e-runtime-store-owner-active)
    (should (file-exists-p
             (expand-file-name "store.sqlite3.owner" directory)))
    (should-error (e-runtime-store-open directory :runtime-id "third")
                  :type 'e-runtime-store-owner-active)
    (should (e-runtime-store-live-p store))))

(ert-deftest e-runtime-store-s2-restarts-and-reconciles-after-worker-loss ()
  "A worker process crash preserves committed state and permits new commits."
  (e-runtime-store-test--with-store (store directory)
    (e-runtime-store-call
     store 'write '(:op session-append :session-id "s"
                    :expected-revision 0 :record (:position 1)))
    (delete-process (e-runtime-store--process store))
    (while (e-runtime-store--live-p store)
      (accept-process-output nil 0.01))
    (let ((result
           (e-runtime-store-call
            store 'write '(:op session-append :session-id "s"
                           :expected-revision 1 :record (:position 2)))))
      (should (= (plist-get result :revision) 2)))
    (let ((page (e-runtime-store-call
                 store 'read '(:op session-record-page :session-id "s"))))
      (should (equal (mapcar (lambda (entry)
                               (plist-get (plist-get entry :value) :position))
                             (plist-get page :records))
                     '(1 2))))
    (should (equal (plist-get (e-runtime-store-call
                              store 'read '(:op status)) :quick-check)
                   "ok"))))

(ert-deftest e-runtime-store-s2-automatically-reconciles-lost-commit-response ()
  "A post-COMMIT response loss reconciles without an explicit await call."
  (let* ((directory (make-temp-file "e-runtime-store-lost-ack-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (store (e-runtime-store-open directory))
         (process (e-runtime-store--process store))
         (ordinary-filter (process-filter process))
         (captured "")
         (callback-count 0)
         request)
    (unwind-protect
        (progn
          ;; Receipt of any complete response proves the worker has returned
          ;; from its COMMIT-first write path.  Discard that response and kill
          ;; the worker at the transport boundary.
          (set-process-filter
           process
           (lambda (worker text)
             (setq captured (concat captured text))
             (when (string-match-p "\n" captured)
               (set-process-filter worker ordinary-filter)
               (delete-process worker))))
          (setq request
                (e-runtime-store-submit
                 store 'write
                 '(:op session-append :session-id "lost-ack"
                   :expected-revision 0 :record (:value "once"))
                 :id "lost-ack-command"
                 :on-done (lambda (_result) (cl-incf callback-count))))
          ;; Drive only the ordinary Emacs event loop.  The request itself must
          ;; schedule restart/reconciliation; this test never calls await.
          (let ((deadline (+ (float-time) 5.0)))
            (while (and (not (eq (e-runtime-store-request--state request)
                                 'committed))
                        (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should (eq (e-runtime-store-request--state request) 'committed))
          (should (= callback-count 1))
          (should (= (plist-get (e-runtime-store-request--result request)
                                :revision)
                     1))
          (e-runtime-store-close store)
          (should-not (file-exists-p
                       (expand-file-name "store.sqlite3.owner" directory)))
          ;; Test-side physical witness: stable-id reconciliation produced one
          ;; domain row and one command record, rather than replaying mutation.
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (progn
                  (should (= (caar (sqlite-select
                                    database
                                    "SELECT COUNT(*) FROM session_records WHERE session_id='lost-ack'"))
                             1))
                  (should (= (caar (sqlite-select
                                    database
                                    "SELECT COUNT(*) FROM writer_commands WHERE command_id='lost-ack-command'"))
                             1)))
              (sqlite-close database))))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s2-automatically-retries-submitted-read ()
  "A submitted read survives worker loss and invokes its callback once."
  (e-runtime-store-test--with-store (store directory)
    (e-runtime-store-call
     store 'write '(:op session-append :session-id "read-loss"
                    :record (:value "present")))
    (let* ((process (e-runtime-store--process store))
           (ordinary-filter (process-filter process))
           (captured "")
           (callback-count 0)
           request)
      (set-process-filter
       process
       (lambda (worker text)
         (setq captured (concat captured text))
         (when (string-match-p "\n" captured)
           (set-process-filter worker ordinary-filter)
           (delete-process worker))))
      (setq request
            (e-runtime-store-submit
             store 'read
             '(:op session-record-page :session-id "read-loss")
             :on-done (lambda (_result) (cl-incf callback-count))))
      (let ((deadline (+ (float-time) 5.0)))
        (while (and (not (eq (e-runtime-store-request--state request)
                             'committed))
                    (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (eq (e-runtime-store-request--state request) 'committed))
      (should (= callback-count 1))
      (should (= (length
                  (plist-get (e-runtime-store-request--result request)
                             :records))
                 1)))))

(ert-deftest e-runtime-store-s2-internal-open-failure-never-enters-domain-queue ()
  "A failed process-local open handshake cannot become a malformed request."
  (e-runtime-store-test--with-store (store directory)
    (let ((open-request
           (e-runtime-store-request--create
            :id "private-open" :kind 'open :state 'submitted)))
      (puthash "private-open" open-request (e-runtime-store--pending store))
      (setf (e-runtime-store--active-request store) open-request)
      (e-runtime-store--requeue-active store)
      (should (eq (e-runtime-store-request--state open-request) 'failed))
      (should-not (gethash "private-open" (e-runtime-store--pending store)))
      (should-not (e-runtime-store--write-queue store))
      (should-not (e-runtime-store--read-queue store)))))

(ert-deftest e-runtime-store-s2-permission-failure-precedes-mutation ()
  "A mode failure surfaces before BEGIN and leaves no durable command."
  (let ((directory (make-temp-file "e-runtime-store-mode-failure-" t)))
    (unwind-protect
        (progn
          (e-runtime-store-worker--open directory "mode-test")
          (let* ((body '(:op session-append :session-id "mode-failure"
                         :record (:value "never")))
                 (request
                  (list :id "mode-failure-command" :kind 'write :body body
                        :hash (secure-hash
                               'sha256
                               (e-runtime-store-codec-encode body)))))
            (cl-letf (((symbol-function
                        'e-runtime-store-worker--permissions)
                       (lambda ()
                         (signal 'file-error '("Synthetic chmod failure")))))
              (should-error (e-runtime-store-worker--write request)
                            :type 'file-error))
            (should (= (caar (sqlite-select
                              e-runtime-store-worker--database
                              "SELECT COUNT(*) FROM session_records"))
                       0))
            (should (= (caar (sqlite-select
                              e-runtime-store-worker--database
                              "SELECT COUNT(*) FROM writer_commands"))
                       0))))
      (when e-runtime-store-worker--database
        (sqlite-close e-runtime-store-worker--database)
        (setq e-runtime-store-worker--database nil))
      (e-runtime-store-worker--release-owner)
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s2-freezes-effects-after-bounded-worker-losses ()
  "Repeated worker loss freezes new effects at the configured bound."
  (e-runtime-store-test--with-store (store directory)
    (dotimes (_ (1+ e-runtime-store-restart-limit))
      (delete-process (e-runtime-store--process store))
      (while (e-runtime-store--live-p store)
        (accept-process-output nil 0.01))
      (when (<= (e-runtime-store--restart-count store)
                e-runtime-store-restart-limit)
        (e-runtime-store--start-process store)))
    (let ((request
           (e-runtime-store-submit
            store 'write
            '(:op session-append :session-id "frozen" :record (:x t)))))
      (should-error (e-runtime-store-await store request)
                    :type 'e-runtime-store-unavailable))
    (setf (e-runtime-store--restart-count store) 0)
    (let ((page (e-runtime-store-call
                 store 'read
                 '(:op session-record-page :session-id "frozen"))))
      (should-not (plist-get page :records)))))

(ert-deftest e-runtime-store-s2-cancellation-and-write-priority ()
  "Pre-submit cancellation drops work and submitted work requires reconcile."
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
        (should (eq (e-runtime-store-cancel store submitted) 'reconcile))
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
