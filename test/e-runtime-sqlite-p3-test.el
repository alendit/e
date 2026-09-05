;;; e-runtime-sqlite-p3-test.el --- Feature 87 P3 owner scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-cron)
(require 'e-cron-storage-sqlite)
(require 'e-goodnite-storage-sqlite)
(require 'e-raw-results)
(require 'e-raw-results-storage-sqlite)
(require 'e-runtime-sqlite)
(require 'e-task-queue)
(require 'e-task-storage-sqlite)
(require 'e-voice-adjustment)
(require 'e-voice-storage-sqlite)

(cl-defmacro e-runtime-sqlite-p3-test--with-runtime
    ((runtime directory) &rest body)
  "Run BODY with one disposable RUNTIME rooted at DIRECTORY."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-runtime-sqlite-p3-" t))
          (,runtime (e-runtime-store-open ,directory)))
     (unwind-protect (progn ,@body)
       (ignore-errors (e-runtime-store-close ,runtime))
       (delete-directory ,directory t))))

(defun e-runtime-sqlite-p3-test--hold-one-response
    (runtime observation release-delay)
  "Hold RUNTIME's next complete response, run OBSERVATION, then release it."
  (let* ((process (e-runtime-store--process runtime))
         (ordinary-filter (process-filter process))
         (captured "")
         held)
    (set-process-filter
     process
     (lambda (worker text)
       (setq captured (concat captured text))
       (when (and (not held) (string-match-p "\n" captured))
         (setq held t)
         (run-at-time 0 nil observation)
         (run-at-time
          release-delay nil
          (lambda ()
            (set-process-filter worker ordinary-filter)
            (funcall ordinary-filter worker captured))))))))

(ert-deftest e-runtime-sqlite-p3-s7-task-claim-precedes-runner-and-restart-is-uncertain ()
  "Claim ACK gates the runner and a lost live runner never auto-requeues."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-task-storage-sqlite-create runtime))
           (calls 0)
           settle-thunks
           (queue
            (e-task-queue-create
             :id "claims" :storage storage :max-parallel 0
             :runner
             (lambda (_task _harness settle)
               (cl-incf calls)
               (push settle settle-thunks)
               (list :cancel #'ignore))))
           observed-calls)
      (cl-letf (((symbol-function 'e-harness-instance-get-or-create)
                 (lambda (_id) :test-harness)))
        (let* ((record (e-task-queue-enqueue
                        queue :prompt "commit first"
                        :harness-instance-id 'test))
               (task-id (plist-get record :task-id)))
          (setf (e-task-queue-max-parallel queue) 1)
          (e-runtime-sqlite-p3-test--hold-one-response
           runtime (lambda () (setq observed-calls calls)) 0.02)
          (e-task-queue--dispatch queue)
          (should (= observed-calls 0))
          (should (= calls 1))
          (should (equal (plist-get (e-task-queue-get queue task-id)
                                    :attempt-id)
                         (format "%s:a:1" task-id)))
          (let ((snapshot (e-task-storage-snapshot storage "claims")))
            (should (= (length (plist-get snapshot :attempts)) 1))
            (should (eq (plist-get (car (plist-get snapshot :attempts)) :state)
                        'claimed)))
          ;; Process loss leaves the claimed runner effect ambiguous.  A fresh
          ;; queue restores it as interrupted and never invokes a runner.
          (e-runtime-store-close runtime)
          (setq runtime (e-runtime-store-open directory)
                storage (e-task-storage-sqlite-create runtime))
          (let ((restored
                 (e-task-queue-create
                  :id "claims" :storage storage :max-parallel 1
                  :runner (lambda (&rest _args) (cl-incf calls)))))
            (e-task-queue-load restored)
            (should (= calls 1))
            (should (eq (plist-get (e-task-queue-get restored task-id) :status)
                        'interrupted))
            (should (= (length
                        (plist-get (e-task-storage-snapshot storage "claims")
                                   :attempts))
                       1))))))))

(ert-deftest e-runtime-sqlite-p3-s7-task-late-settle-is-fenced-by-attempt ()
  "A late old attempt cannot settle its explicitly resumed successor."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-task-storage-sqlite-create runtime))
           (calls 0) settles
           (queue
            (e-task-queue-create
             :id "late" :storage storage :max-parallel 0
             :runner
             (lambda (_task _harness settle)
               (cl-incf calls)
               (setq settles (append settles (list settle)))
               (list :cancel
                     (lambda () (funcall settle :status 'cancelled))))))
           (record (e-task-queue-enqueue
                    queue :prompt "one effect" :harness-instance-id 'test))
           (task-id (plist-get record :task-id)))
      (cl-letf (((symbol-function 'e-harness-instance-get-or-create)
                 (lambda (_id) :test-harness)))
        (setf (e-task-queue-max-parallel queue) 1)
        (e-task-queue--dispatch queue)
        (should (= calls 1))
        (e-task-queue-pause queue task-id)
        (e-task-queue-resume queue task-id)
        (should (= calls 2))
        (should (string-suffix-p
                 ":a:2" (plist-get (e-task-queue-get queue task-id)
                                    :attempt-id)))
        (funcall (nth 0 settles) :status 'done :outputs '(stale))
        (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                    'running))
        (funcall (nth 1 settles) :status 'done :outputs '(current))
        (should (eq (plist-get (e-task-queue-get queue task-id) :status) 'done))
        (should (equal (e-task-queue-outputs queue task-id) '(current)))
        (should (= (length
                    (plist-get (e-task-storage-snapshot storage "late")
                               :attempts))
                   2))))))

(ert-deftest e-runtime-sqlite-p3-s7-task-cancel-during-claim-prevents-runner ()
  "A reentrant cancellation after claim commit runs before the runner."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-task-storage-sqlite-create runtime))
           (calls 0)
           (queue
            (e-task-queue-create
             :id "cancel-claim" :storage storage :max-parallel 0
             :runner (lambda (&rest _args) (cl-incf calls))))
           (record (e-task-queue-enqueue
                    queue :prompt "must not start" :harness-instance-id 'test))
           (task-id (plist-get record :task-id))
           pause-status-during status-during)
      (cl-letf (((symbol-function 'e-harness-instance-get-or-create)
                 (lambda (_id) :test-harness)))
        (setf (e-task-queue-max-parallel queue) 1)
        (e-runtime-sqlite-p3-test--hold-one-response
         runtime
         (lambda ()
           (setq pause-status-during
                 (plist-get (e-task-queue-pause queue task-id) :status)
                 status-during
                 (plist-get (e-task-queue-cancel queue task-id) :status)))
         0.02)
        (e-task-queue--dispatch queue)
        (should (eq pause-status-during 'queued))
        (should (eq status-during 'queued))
        (should (= calls 0))
        (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                    'cancelled))
        (let* ((snapshot (e-task-storage-snapshot storage "cancel-claim"))
               (durable (car (plist-get snapshot :records)))
               (attempt (car (plist-get snapshot :attempts))))
          (should (eq (plist-get durable :status) 'cancelled))
          (should (eq (plist-get attempt :state) 'cancelled)))))))

(ert-deftest e-runtime-sqlite-p3-s7-task-pause-all-during-claim-prevents-runner ()
  "A pause-all reentering a claim commits pause before runner entry."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-task-storage-sqlite-create runtime))
           (calls 0)
           (queue
            (e-task-queue-create
             :id "pause-claim" :storage storage :max-parallel 0
             :runner
             (lambda (_task _harness settle)
               (cl-incf calls)
               (funcall settle :status 'done)
               nil)))
           (record (e-task-queue-enqueue
                    queue :prompt "pause before start"
                    :harness-instance-id 'test))
           (task-id (plist-get record :task-id))
           status-during)
      (cl-letf (((symbol-function 'e-harness-instance-get-or-create)
                 (lambda (_id) :test-harness)))
        (setf (e-task-queue-max-parallel queue) 1)
        (e-runtime-sqlite-p3-test--hold-one-response
         runtime
         (lambda ()
           (e-task-queue-pause-all queue)
           (setq status-during
                 (plist-get (e-task-queue-get queue task-id) :status)))
         0.02)
        (e-task-queue--dispatch queue)
        (should (eq status-during 'queued))
        (should (= calls 0))
        (should (e-task-queue-paused-p queue))
        (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                    'paused))
        (let* ((snapshot (e-task-storage-snapshot storage "pause-claim"))
               (durable (car (plist-get snapshot :records)))
               (attempt (car (plist-get snapshot :attempts))))
          (should (plist-get snapshot :paused-p))
          (should (eq (plist-get durable :status) 'paused))
          (should-not (plist-get durable :started-at))
          (should (eq (plist-get attempt :state) 'paused)))
        ;; Explicitly releasing the queue gate creates the next attempt and is
        ;; the only point at which the runner may start.
        (e-task-queue-resume-all queue)
        (should (= calls 1))
        (should (eq (plist-get (e-task-queue-get queue task-id) :status) 'done))
        (let ((attempts
               (plist-get (e-task-storage-snapshot storage "pause-claim")
                          :attempts)))
          (should (= (length attempts) 2))
          (should (eq (plist-get (car (last attempts)) :state) 'done)))))))

(ert-deftest e-runtime-sqlite-p3-s7-task-pause-waits-for-owned-settlement ()
  "A pause request retains its slot until the claimed runner confirms it."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-task-storage-sqlite-create runtime))
           (calls 0) (cancels 0) settles
           (queue
            (e-task-queue-create
             :id "pause-confirm" :storage storage :max-parallel 1
             :runner
             (lambda (_task _harness settle)
               (cl-incf calls)
               (setq settles (append settles (list settle)))
               (list :cancel (lambda () (cl-incf cancels)))))))
      (cl-letf (((symbol-function 'e-harness-instance-get-or-create)
                 (lambda (_id) :test-harness)))
        (let* ((first (e-task-queue-enqueue
                       queue :prompt "first" :harness-instance-id 'test))
               (first-id (plist-get first :task-id))
               (second (e-task-queue-enqueue
                        queue :prompt "second" :harness-instance-id 'test))
               (second-id (plist-get second :task-id)))
          (should (= calls 1))
          (should (eq (plist-get (e-task-queue-pause queue first-id) :status)
                      'pausing))
          (should (= cancels 1))
          (should (= calls 1))
          (should (eq (plist-get (e-task-queue-get queue second-id) :status)
                      'queued))
          (should (eq (plist-get (e-task-queue-resume queue first-id) :status)
                      'pausing))
          (funcall (nth 0 settles) :status 'cancelled)
          (should (eq (plist-get (e-task-queue-get queue first-id) :status)
                      'paused))
          (should (= calls 2))
          (should (eq (plist-get (e-task-queue-get queue second-id) :status)
                      'running))
          (funcall (nth 1 settles) :status 'done)
          (e-task-queue-resume queue first-id)
          (should (= calls 3))
          (should (string-suffix-p
                   ":a:2" (plist-get (e-task-queue-get queue first-id)
                                      :attempt-id))))))))

(ert-deftest e-runtime-sqlite-p3-s7-task-pause-failure-is-interrupted ()
  "Missing or failing cancellation never publishes a safely paused task."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let ((storage (e-task-storage-sqlite-create runtime)))
      (cl-letf (((symbol-function 'e-harness-instance-get-or-create)
                 (lambda (_id) :test-harness)))
        (dolist (case '(("missing" . nil) ("failing" . failing)))
          (let* ((id (car case))
                 (mode (cdr case))
                 (queue
                  (e-task-queue-create
                   :id id :storage storage :max-parallel 1
                   :runner
                   (lambda (&rest _args)
                     (and mode
                          (list :cancel
                                (lambda () (error "cancel failed")))))))
                 (record (e-task-queue-enqueue
                          queue :prompt id :harness-instance-id 'test))
                 (task-id (plist-get record :task-id)))
            (should-error (e-task-queue-pause queue task-id) :type 'error)
            (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                        'interrupted))))))))

(ert-deftest e-runtime-sqlite-p3-s7-task-restart-while-pausing-is-interrupted ()
  "A lost cancellation confirmation restores uncertainty, never queued work."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-task-storage-sqlite-create runtime))
           (calls 0)
           (queue
            (e-task-queue-create
             :id "pause-crash" :storage storage :max-parallel 1
             :runner (lambda (&rest _args)
                       (cl-incf calls)
                       (list :cancel #'ignore)))))
      (cl-letf (((symbol-function 'e-harness-instance-get-or-create)
                 (lambda (_id) :test-harness)))
        (let* ((record (e-task-queue-enqueue
                        queue :prompt "uncertain" :harness-instance-id 'test))
               (task-id (plist-get record :task-id)))
          (should (eq (plist-get (e-task-queue-pause queue task-id) :status)
                      'pausing))
          (e-runtime-store-close runtime)
          (setq runtime (e-runtime-store-open directory)
                storage (e-task-storage-sqlite-create runtime))
          (let ((restored
                 (e-task-queue-create
                  :id "pause-crash" :storage storage :max-parallel 1
                  :runner (lambda (&rest _args) (cl-incf calls)))))
            (e-task-queue-load restored)
            (should (= calls 1))
            (should (eq (plist-get (e-task-queue-get restored task-id) :status)
                        'interrupted))))))))

(ert-deftest e-runtime-sqlite-p3-s7-task-runner-signal-is-interrupted ()
  "A signal after claim is uncertain and never eligible for automatic retry."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-task-storage-sqlite-create runtime))
           (calls 0)
           queue)
      (setq queue
            (e-task-queue-create
             :id "runner-signal" :storage storage :max-parallel 1
             :max-retries 2
             :runner
             (lambda (task &rest _args)
               (cl-incf calls)
               ;; Make the record otherwise retry-eligible.  A `failed'
               ;; settlement would immediately queue a second attempt.
               (plist-put
                (gethash (plist-get task :task-id)
                         (e-task-queue-records queue))
                :session-id "retry-eligible-session")
               (error "runner exploded"))))
      (cl-letf (((symbol-function 'e-harness-instance-get-or-create)
                 (lambda (_id) :test-harness)))
        (should-error
         (e-task-queue-enqueue
          queue :prompt "explode" :harness-instance-id 'test)
         :type 'error)
        (let ((record (car (e-task-queue-list queue))))
          (should (= calls 1))
          (should (eq (plist-get record :status) 'interrupted))
          (should (= (plist-get record :retries) 0))
          (should (equal (plist-get record :session-id)
                         "retry-eligible-session"))
          (should (string-match-p "runner exploded"
                                  (plist-get record :error)))
          (e-task-queue--dispatch queue)
          (should (= calls 1)))))))

(ert-deftest e-runtime-sqlite-p3-s7-task-barrier-and-history-delete ()
  "The owner barrier and explicit history deletion are exact."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-task-storage-sqlite-create runtime))
           (queue (e-task-queue-create
                   :id "operator" :storage storage :max-parallel 0
                   :runner #'ignore))
           done failure)
      (e-task-queue-enqueue queue :prompt "retained history")
      (should
       (eq queue
           (e-task-queue-finalize
            queue (lambda (_queue) (setq done t))
            (lambda (condition) (setq failure condition)))))
      (should done)
      (should-not failure)
      ;; Repeating ordinary owner transitions remains serialized by one worker.
      (e-task-queue-pause queue (plist-get (car (e-task-queue-list queue))
                                           :task-id))
      (e-task-queue-resume queue (plist-get (car (e-task-queue-list queue))
                                            :task-id))
      (e-task-queue-pause queue (plist-get (car (e-task-queue-list queue))
                                           :task-id))
      (e-task-queue-resume queue (plist-get (car (e-task-queue-list queue))
                                            :task-id))
      (should (plist-get (e-task-queue-delete-history queue) :deleted))
      (let ((snapshot (e-task-storage-snapshot storage "operator")))
        (should-not (plist-get snapshot :records))
        (should-not (plist-get snapshot :attempts))
        (should (= (plist-get snapshot :sequence) 1)))
      (let ((replacement
             (e-task-queue-enqueue queue :prompt "identity after deletion")))
        (should (equal (plist-get replacement :task-id) "tsk_000002"))))))

(ert-deftest e-runtime-sqlite-p3-s7-cron-claim-gates-effects-and-interruption-is-unsafe ()
  "Cron commits cadence before work and restores an unresolved claim safely."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-cron-storage-sqlite-create runtime))
           (e-cron--schedules (make-hash-table :test 'equal))
           (now (seconds-to-time 1000))
           (e-cron-current-time-function (lambda () now))
           (actions 0) observed-actions
           (schedule
            (e-cron-register
             :id 'claimed :when '(:every 60) :enabled nil :storage storage
             :action (lambda (_schedule) (cl-incf actions) :done))))
      (e-runtime-sqlite-p3-test--hold-one-response
       runtime (lambda () (setq observed-actions actions)) 0.02)
      (e-cron-fire schedule)
      (should (= observed-actions 0))
      (should (= actions 1))
      (let* ((crash-schedule
              (e-cron-register
               :id 'crash :when '(:every 30) :enabled nil :storage storage
               :action (lambda (_schedule) (cl-incf actions))))
             (due (float-time (e-cron-schedule-next-fire crash-schedule)))
             (firing-id "crash:1:manual"))
        (e-cron-storage-claim
         storage 'crash firing-id due 1000.0 1030.0)
        (e-runtime-store-close runtime)
        (setq runtime (e-runtime-store-open directory)
              storage (e-cron-storage-sqlite-create runtime)
              e-cron--schedules (make-hash-table :test 'equal))
        (let ((restored
               (e-cron-register
                :id 'crash :when '(:every 30) :enabled nil :storage storage
                :action (lambda (_schedule) (cl-incf actions)))))
          (should (= actions 1))
          (should (eq (plist-get
                       (car (e-cron-schedule-unresolved-firings restored))
                       :state)
                      'unsafe)))
        (setq e-cron--schedules (make-hash-table :test 'equal))
        (let ((restored
               (e-cron-register
                :id 'crash :when '(:every 30) :enabled nil :storage storage
                :action #'ignore)))
          (should (eq (plist-get
                       (car (e-cron-schedule-unresolved-firings restored))
                       :state)
                      'unsafe)))))))

(ert-deftest e-runtime-sqlite-p3-s7-cron-guard-and-replacement-races ()
  "A skipped guard is durable and a replaced definition is never re-armed."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-cron-storage-sqlite-create runtime))
           (e-cron--schedules (make-hash-table :test 'equal))
           (now (seconds-to-time 2000))
           (e-cron-current-time-function (lambda () now))
           (actions 0) (arms 0))
      (let ((schedule
             (e-cron-register
              :id 'guarded :when '(:every 10) :enabled nil :storage storage
              :guard (lambda () nil)
              :action (lambda (_schedule) (cl-incf actions)))))
        (should-not (e-cron-fire schedule))
        (should (= actions 0))
        (should-not (plist-get (e-cron-storage-cadence storage 'guarded)
                               :unresolved)))
      (let (old)
        (setq old
              (e-cron-register
               :id 'replace :when '(:every 10) :enabled nil :storage storage
               :action
               (lambda (_schedule)
                 (e-cron-register
                  :id 'replace :when '(:every 20) :enabled nil :storage storage
                  :action #'ignore))))
        (setf (e-cron-schedule-enabled old) t)
        ;; Stub the cron-owned arm seam, not the global timer primitive: the
        ;; latter now also drives DP5A's autonomous worker scheduler.
        (cl-letf (((symbol-function 'e-cron--arm)
                   (lambda (&rest _args) (cl-incf arms) 'stub-timer)))
          (e-cron--on-timer 'replace))
        (should (= arms 0))
        (should-not (eq old (e-cron-get 'replace)))))))

(ert-deftest e-runtime-sqlite-p3-s7-cron-replacement-during-claim-skips-old-action ()
  "A definition replaced during claim cannot start its retired action."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-cron-storage-sqlite-create runtime))
           (e-cron--schedules (make-hash-table :test 'equal))
           (now (seconds-to-time 2500))
           (e-cron-current-time-function (lambda () now))
           (old-actions 0)
           (new-actions 0)
           (old
            (e-cron-register
             :id 'claim-replace :when '(:every 10) :enabled nil
             :storage storage
             :action (lambda (_schedule) (cl-incf old-actions))))
           replacement)
      (e-runtime-sqlite-p3-test--hold-one-response
       runtime
       (lambda ()
         (setq replacement
               (e-cron-register
                :id 'claim-replace :when '(:every 20) :enabled nil
                :storage storage
                :action (lambda (_schedule) (cl-incf new-actions)))))
       0.02)
      (should-not (e-cron-fire old))
      (should (= old-actions 0))
      (should (= new-actions 0))
      (should (eq replacement (e-cron-get 'claim-replace)))
      (should-not (eq old replacement))
      (should (= (e-cron-schedule-definition-revision replacement) 2))
      (let ((unresolved
             (plist-get (e-cron-storage-cadence storage 'claim-replace)
                        :unresolved)))
        (should (= (length unresolved) 1))
        (should (equal (plist-get (car unresolved) :firing-id)
                       "claim-replace:1:2510.000000"))
        (should (eq (plist-get (car unresolved) :state) 'unsafe))))))

(ert-deftest e-runtime-sqlite-p3-s7-cron-failure-and-history-delete ()
  "Known action failure settles once and history deletion is explicit."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-cron-storage-sqlite-create runtime))
           (e-cron--schedules (make-hash-table :test 'equal))
           (now (seconds-to-time 3000))
           (e-cron-current-time-function (lambda () now))
           (schedule
            (e-cron-register
             :id 'fails :when '(:every 10) :enabled nil :storage storage
             :action (lambda (_schedule) (error "known action failure")))))
      (should-error (e-cron-fire schedule) :type 'error)
      (should-not (plist-get (e-cron-storage-cadence storage 'fails)
                             :unresolved))
      (should (plist-get (e-cron-delete-history 'fails) :deleted)))))

(ert-deftest e-runtime-sqlite-p3-s8-voice-atomic-lru-restart-and-clear ()
  "Voice tells update and evict atomically, survive restart, and clear."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let ((storage (e-voice-storage-sqlite-create runtime)))
      (e-voice-storage-record storage "a" "A" "first" "t1" 2)
      (e-voice-storage-record storage "b" "B" "second" "t2" 2)
      (e-voice-storage-record storage "a" "A2" nil "t3" 2)
      (e-voice-storage-record storage "c" "C" "third" "t4" 2)
      (let ((tells (plist-get (e-voice-storage-list storage) :tells)))
        (should (equal (mapcar (lambda (tell) (plist-get tell :key)) tells)
                       '("c" "a")))
        (should (= (plist-get (cadr tells) :count) 2))
        (should (equal (plist-get (cadr tells) :description) "first")))
      (e-runtime-store-close runtime)
      (setq runtime (e-runtime-store-open directory)
            storage (e-voice-storage-sqlite-create runtime))
      (should (= (plist-get (e-voice-storage-list storage) :count) 2))
      (e-voice-storage-clear storage)
      (should-not (plist-get (e-voice-storage-list storage) :tells)))))

(ert-deftest e-runtime-sqlite-p3-s8-voice-reentrant-record-clear-stays-canonical ()
  "Held record/clear acknowledgements cannot publish stale live LRU state."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-voice-storage-sqlite-create runtime))
           (e-voice-adjustment-storage storage)
           (e-voice-adjustment--loaded t)
           (e-voice-adjustment--tells nil)
           (e-voice-adjustment--mutation-generation 0))
      ;; The clear is later in worker order but returns inside the record's
      ;; cooperative wait.  The older record result must not repopulate cache.
      (e-runtime-sqlite-p3-test--hold-one-response
       runtime (lambda () (e-voice-adjustment--clear)) 0.02)
      (e-voice-adjustment--record "outer" "must be cleared")
      (should-not (plist-get (e-voice-adjustment--list) :tells))
      (should-not (plist-get (e-voice-storage-list storage) :tells))
      ;; Reverse the order: the later record owns both DB and live projection.
      (e-voice-adjustment--record "seed" "before clear")
      (e-runtime-sqlite-p3-test--hold-one-response
       runtime
       (lambda () (e-voice-adjustment--record "later" "after clear"))
       0.02)
      (e-voice-adjustment--clear)
      (let ((live (plist-get (e-voice-adjustment--list) :tells))
            (durable (plist-get (e-voice-storage-list storage) :tells)))
        (should (equal live durable))
        (should (equal (mapcar (lambda (tell) (plist-get tell :key)) live)
                       '("later"))))
      (e-runtime-store-close runtime)
      (setq runtime (e-runtime-store-open directory)
            storage (e-voice-storage-sqlite-create runtime)
            e-voice-adjustment-storage storage
            e-voice-adjustment--loaded nil
            e-voice-adjustment--tells nil)
      (should
       (equal (plist-get (e-voice-adjustment--list) :tells)
              (plist-get (e-voice-storage-list storage) :tells))))))

(ert-deftest e-runtime-sqlite-p3-s8-goodnite-checkpoint-before-bounded-cleanup ()
  "Goodnite dedupes demand and resumes cleanup after an ACK-only crash."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let ((storage (e-goodnite-storage-sqlite-create runtime)))
      (should-not
       (plist-get (e-goodnite-storage-append
                   storage "same" '(:kind "read" :ts "one"))
                  :duplicate))
      (should
       (plist-get (e-goodnite-storage-append
                   storage "same" '(:kind "read" :ts "two"))
                  :duplicate))
      (dotimes (index 4)
        (e-goodnite-storage-append
         storage (format "event-%d" index) (list :index index)))
      (let ((page (e-goodnite-storage-page storage 0 2)))
        (should (= (length (plist-get page :events)) 2))
        (should (= (plist-get page :next) 2)))
      (e-goodnite-storage-ack storage 2)
      ;; Crash after checkpoint but before physical cleanup.
      (e-runtime-store-close runtime)
      (setq runtime (e-runtime-store-open directory)
            storage (e-goodnite-storage-sqlite-create runtime))
      (let ((page (e-goodnite-storage-page storage 0 2)))
        (should (= (plist-get page :checkpoint) 2))
        (should (= (plist-get (car (plist-get page :events)) :position) 3)))
      (should (plist-get (e-goodnite-storage-cleanup storage 1) :more))
      (should (= (plist-get (e-goodnite-storage-cleanup storage 1) :deleted) 1))
      (e-runtime-store-close runtime)
      (should-error
       (e-goodnite-storage-append storage "failure" '(:kind "read"))
       :type 'e-runtime-store-unavailable))))

(ert-deftest e-runtime-sqlite-p3-s8-goodnite-observations-remain-distinct ()
  "Equal accesses are distinct observations; re-appending one event id dedupes."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-goodnite-storage-sqlite-create runtime))
           (e-goodnite-resources-storage storage)
           (e-goodnite-track-access t)
           (e-goodnite-resources--event-sequence 0)
           (context '(:session-id "s" :turn-id "t"))
           (first (e-goodnite-resources--record-access
                   'search "goodnite://entry" "same" context))
           (second (e-goodnite-resources--record-access
                    'search "goodnite://entry" "same" context)))
      (should-not (equal (plist-get first :event-id)
                         (plist-get second :event-id)))
      (let ((events (plist-get (e-goodnite-storage-page storage 0 10)
                               :events)))
        (should (= (length events) 2))
        (should (equal (mapcar (lambda (event)
                                (plist-get event :position))
                              events)
                       '(1 2))))
      (should
       (plist-get
        (e-goodnite-storage-append
         storage (plist-get first :event-id) '(:duplicate exact-id))
        :duplicate)))))

(ert-deftest e-runtime-sqlite-p3-s8-raw-immutable-fixed-expiry-and-size-boundary ()
  "Raw results dedupe exact content, conflict, expire, and enforce 16 MiB."
  (let ((e-runtime-store-request-timeout 45.0))
    (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
      (let* ((storage (e-raw-results-storage-sqlite-create runtime))
             (uri "raw-result://fixed")
             (first (e-raw-results-storage-put
                     storage uri "same" '(:owner test) 10.0 20.0))
             (duplicate (e-raw-results-storage-put
                         storage uri "same" '(:owner changed) 11.0 99.0)))
        (should-not (plist-get first :duplicate))
        (should (plist-get duplicate :duplicate))
        (should (= (plist-get duplicate :expires-at) 20.0))
        (should-error
         (e-raw-results-storage-put storage uri "different" nil 10.0 20.0)
         :type 'e-raw-results-storage-conflict)
        (let ((exact (make-string (* 16 1024 1024) ?x)))
          (should (= (plist-get
                      (e-raw-results-storage-put
                       storage "raw-result://exact" exact nil 10.0 30.0)
                      :bytes)
                     (string-bytes exact))))
        (should-error
         (e-raw-results-storage-put
          storage "raw-result://over"
          (make-string (1+ (* 16 1024 1024)) ?x) nil 10.0 30.0)
         :type 'e-raw-results-storage-too-large)
        (should-not (e-raw-results-storage-read storage uri 21.0))
        (should (member uri
                        (plist-get (e-raw-results-storage-expire storage 21.0)
                                   :deleted)))))))

(ert-deftest e-runtime-sqlite-p3-s8-raw-import-disposal-and-restart ()
  "Raw imports dispose only after commit and content survives restart."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-raw-results-storage-sqlite-create runtime))
           (uri "raw-result://restart"))
      (should (= (plist-get
                  (e-raw-results-storage-put
                   storage uri "committed-once" nil 1.0 100.0)
                  :bytes)
                 14))
      (let ((source (make-temp-file "e-raw-import-" nil nil "imported"))
            (conflict (make-temp-file "e-raw-conflict-" nil nil "other"))
            (e-raw-results-storage storage))
        (unwind-protect
            (progn
              (let ((reference
                     (e-raw-results-import-file source :id "imported")))
                (should-not (file-exists-p source))
                (should (equal (e-raw-results-read
                                (plist-get reference :uri))
                               "imported")))
              (e-raw-results-write :id "conflict" :content "first")
              (should-error
               (e-raw-results-import-file conflict :id "conflict")
               :type 'e-raw-results-storage-conflict)
              (should (file-exists-p conflict)))
          (when (file-exists-p source) (delete-file source))
          (when (file-exists-p conflict) (delete-file conflict))))
      (e-runtime-store-close runtime)
      (setq runtime (e-runtime-store-open directory)
            storage (e-raw-results-storage-sqlite-create runtime))
      (should (equal (plist-get
                      (e-raw-results-storage-read storage uri 2.0)
                      :content)
                     "committed-once"))
      (should (plist-get (e-raw-results-storage-delete storage uri) :deleted))
      (should-not (e-raw-results-storage-read storage uri 2.0)))))

(ert-deftest e-runtime-sqlite-p3-composition-injects-one-runtime-and-closes-once ()
  "Every owner port borrows the composition's sole physical runtime."
  (let* ((directory (make-temp-file "e-runtime-sqlite-composition-" t))
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         (e-runtime-sqlite--live-composition nil)
         (composition (e-runtime-sqlite-open directory))
         (runtime (e-runtime-sqlite-runtime-store composition)))
    (unwind-protect
        (progn
          (should
           (eq runtime
               (e-session-storage-runtime-store
                (e-runtime-sqlite-session-store composition))))
          ;; The default remains legacy until the full session command grammar
          ;; is migrated; partial opt-in must not expose unsupported facades.
          (should-not
           (e-session-async-enabled-p
            (e-runtime-sqlite-session-store composition)))
          (should
           (eq runtime
               (e-board-storage-runtime
                (e-runtime-sqlite-board-storage composition))))
          (should
           (eq runtime
               (e-task-storage-runtime
                (e-runtime-sqlite-task-storage composition))))
          (should (eq runtime (e-cron-storage-runtime e-cron-storage)))
          (should
           (eq runtime
               (e-voice-storage-runtime e-voice-adjustment-storage)))
          (should
           (eq runtime
               (e-goodnite-storage-runtime e-goodnite-resources-storage)))
          (should
           (eq runtime
               (e-raw-results-storage-runtime e-raw-results-storage)))
          ;; Closing the borrowed session adapter cannot close the runtime.
          (e-session-sqlite-store-close
           (e-runtime-sqlite-session-store composition))
          (should (e-runtime-store-live-p runtime))
          (e-runtime-sqlite-close composition)
          (should-not (e-runtime-store-live-p runtime))
          (should (e-runtime-sqlite-close composition)))
      (ignore-errors (e-runtime-sqlite-close composition))
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p3-composition-constructors-issue-no-domain-requests ()
  "All ordinary owner constructors are pure over the shared transport."
  (let* ((directory (make-temp-file "e-runtime-sqlite-p3-pure-" t))
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         (e-runtime-sqlite--live-composition nil)
         (calls nil)
         composition)
    (unwind-protect
        (let ((real-call (symbol-function 'e-runtime-store-call)))
          ;; Returning nil keeps the pre-change constructor traffic from
          ;; waiting on the worker, making the negative reachability witness
          ;; deterministic while still exercising the full composition root.
          (cl-letf (((symbol-function 'e-runtime-store-call)
                     (lambda (_runtime kind body)
                       (push (list kind (plist-get body :op)) calls)
                       nil)))
            (setq composition (e-runtime-sqlite-open directory)))
          (ignore real-call)
          (should-not calls)
          (should-not
           (e-session-aggregate-session-values
            (e-runtime-sqlite-session-store composition)))
          (should-not
           (e-task-queue-order (e-runtime-sqlite-task-queue composition))))
      (when composition (ignore-errors (e-runtime-sqlite-close composition)))
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p3-composition-borrows-provided-runtime-on-close ()
  "A composition supplied with a transport never becomes its close owner."
  (let* ((directory (make-temp-file "e-runtime-sqlite-borrowed-" t))
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         (e-runtime-sqlite--live-composition nil)
         (runtime (e-runtime-store-open directory))
         composition)
    (unwind-protect
        (progn
          (setq composition (e-runtime-sqlite-open
                             directory :runtime-store runtime))
          (should-not (e-runtime-sqlite--owns-runtime-store composition))
          (e-runtime-sqlite-close composition)
          (should (e-runtime-store-live-p runtime))
          (e-runtime-store-close runtime)
          (should-not (e-runtime-store-live-p runtime)))
      (when composition (ignore-errors (e-runtime-sqlite-close composition)))
      (when (and (e-runtime-store-p runtime)
                 (not (e-runtime-store--closed runtime)))
        (ignore-errors (e-runtime-store-close runtime)))
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p3-composition-rejects-provided-directory-mismatch ()
  "A supplied transport from another directory is rejected without closing it."
  (let* ((runtime-directory (make-temp-file "e-runtime-sqlite-provided-" t))
         (composition-directory (make-temp-file "e-runtime-sqlite-mismatch-" t))
         (e-runtime-sqlite--live-composition nil)
         (runtime (e-runtime-store-open runtime-directory)))
    (unwind-protect
        (progn
          (should-error
           (e-runtime-sqlite-open composition-directory :runtime-store runtime)
           :type 'e-runtime-sqlite-live-composition)
          (should (e-runtime-store-live-p runtime))
          (should-not
           (file-exists-p
            (expand-file-name "store.sqlite3" composition-directory))))
      (when (and (e-runtime-store-p runtime)
                 (not (e-runtime-store--closed runtime)))
        (ignore-errors (e-runtime-store-close runtime)))
      (delete-directory runtime-directory t)
      (delete-directory composition-directory t))))

(ert-deftest e-runtime-sqlite-p3-composition-error-does-not-close-provided-runtime ()
  "A failed owner constructor leaves supplied transport ownership with caller."
  (let* ((directory (make-temp-file "e-runtime-sqlite-error-" t))
         (e-runtime-sqlite--live-composition nil)
         (runtime (e-runtime-store-open directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-sqlite-store-create)
                   (lambda (&rest _arguments)
                     (error "synthetic session owner failure"))))
          (should-error
           (e-runtime-sqlite-open directory :runtime-store runtime)
           :type 'error)
          (should (e-runtime-store-live-p runtime))
          (should-not e-runtime-sqlite--live-composition))
      (when (and (e-runtime-store-p runtime)
                 (not (e-runtime-store--closed runtime)))
        (ignore-errors (e-runtime-store-close runtime)))
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p3-composition-rejects-second-live-owner ()
  "A second directory cannot replace a live composition's injected adapters."
  (let* ((first-directory (make-temp-file "e-runtime-sqlite-first-" t))
         (second-directory (make-temp-file "e-runtime-sqlite-second-" t))
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         (e-runtime-sqlite--live-composition nil)
         first second)
    (unwind-protect
        (progn
          (setq first (e-runtime-sqlite-open first-directory))
          (let ((first-cron e-cron-storage)
                (first-voice e-voice-adjustment-storage)
                (first-goodnite e-goodnite-resources-storage)
                (first-raw e-raw-results-storage))
            (should-error
             (e-runtime-sqlite-open second-directory)
             :type 'e-runtime-sqlite-live-composition)
            (should (eq e-runtime-sqlite--live-composition first))
            (should (eq e-cron-storage first-cron))
            (should (eq e-voice-adjustment-storage first-voice))
            (should (eq e-goodnite-resources-storage first-goodnite))
            (should (eq e-raw-results-storage first-raw))
            (should-not
             (file-exists-p
              (expand-file-name "store.sqlite3" second-directory))))
          (e-runtime-sqlite-close first)
          (setq second (e-runtime-sqlite-open second-directory))
          (should (eq e-runtime-sqlite--live-composition second))
          (should (e-runtime-store-live-p
                   (e-runtime-sqlite-runtime-store second)))
          (e-runtime-sqlite-close second)
          (should-not e-runtime-sqlite--live-composition))
      (when (and first (not (e-runtime-sqlite--closed first)))
        (ignore-errors (e-runtime-sqlite-close first)))
      (when (and second (not (e-runtime-sqlite--closed second)))
        (ignore-errors (e-runtime-sqlite-close second)))
      (delete-directory first-directory t)
      (delete-directory second-directory t))))

(provide 'e-runtime-sqlite-p3-test)

;;; e-runtime-sqlite-p3-test.el ends here
