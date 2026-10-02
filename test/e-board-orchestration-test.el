;;; e-board-orchestration-test.el --- Tests for durable board runs -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'e-board-orchestration)

(defun e-board-orchestration-test--fact (type key payload)
  (list :version 1 :type type :idempotency-key key :payload payload))

(defun e-board-orchestration-test--manifest (&optional tasks deadline)
  (e-board-orchestration-test--fact
   'manifest "manifest-1"
   (list :run-id "run-1"
         :tasks (or tasks (list '(:task-key "required" :required t :accepted-attempt 0)
                                '(:task-key "optional" :required nil :accepted-attempt 0)))
         :deadline (or deadline '(:kind none)))))

(ert-deftest e-board-orchestration-test-fact-schema-is-bounded ()
  "Terminal reports trim text and reject unbounded output collections."
  (let ((report (e-board-orchestration-validate-fact
                 (e-board-orchestration-test--fact
                  'terminal-report "report-1"
                  (list :run-id "run-1" :task-key "task" :attempt 0 :status 'done
                        :summary (make-string 1000 ?x) :outputs [] :error (make-string 1000 ?e))))))
    (should (= (length (plist-get (plist-get report :payload) :summary))
               e-board-orchestration-summary-limit))
    (should (= (length (plist-get (plist-get report :payload) :error))
               e-board-orchestration-error-limit)))
  (should-error
   (e-board-orchestration-validate-fact
    (e-board-orchestration-test--fact
     'terminal-report "many" (list :run-id "run-1" :task-key "task" :attempt 0
                                   :status 'done :outputs (make-list 33 '(:kind file)))))))

(ert-deftest e-board-orchestration-test-oversized-prompt-error-is-bounded ()
  "Reject an oversized prompt without retaining it in the error."
  (let* ((fact
          (e-board-orchestration-test--fact
           'manifest "oversized-prompt"
           (list :run-id "run-1" :tasks nil :deadline '(:kind none)
                 :continuation
                 (list :session-id "coordinator" :publication-key "key"
                       :prompt (make-string
                                (1+ e-board-orchestration-fact-byte-limit)
                                ?x)))))
         (caught
          (condition-case error
              (progn (e-board-orchestration-validate-fact fact) nil)
            (e-board-orchestration-invalid-fact error))))
    (should (eq (car caught) 'e-board-orchestration-invalid-fact))
    (should (eq (cadr caught) :continuation-prompt))
    (should (equal (caddr caught)
                   (list :bytes (1+ e-board-orchestration-fact-byte-limit)
                         :limit e-board-orchestration-fact-byte-limit)))))

(ert-deftest e-board-orchestration-test-wire-roundtrip-preserves-lisp-shapes ()
  "JSON replay preserves enums, task arrays, and opaque output values."
  (let* ((fact
          (e-board-orchestration-test--fact
           'terminal-report "report-1"
           '(:run-id "run-1" :task-key "task" :attempt 0 :status done
             :summary "done"
             :outputs ((:kind artifact :uri "file" :value (:state ready))))))
         (expected (e-board-orchestration-validate-fact fact))
         (fields (e-board-orchestration-fact-record-fields fact)))
    (let* ((attributes (plist-get fields :attributes))
           (replayed
            (json-parse-string (json-encode attributes)
                               :object-type 'plist :array-type 'list
                               :null-object nil :false-object :json-false)))
      (should (equal (e-board-orchestration-fact-from-record
                      (list :record-kind 'fact :tags '(orchestration)
                            :attributes replayed))
                     expected)))))

(ert-deftest e-board-orchestration-test-manifest-descriptor-survives-wire-replay ()
  "Application recovery inputs remain opaque and durable across JSON replay."
  (let* ((descriptor '(:date "2026-09-01" :mode populate
                       :path "daily/2026-09-01.org"
                       :window (:started-at "2026-09-01T09:39:10Z")))
         (fact (e-board-orchestration-test--fact
                'manifest "manifest-descriptor"
                (list :run-id "run-1"
                      :tasks '((:task-key "task" :required t :accepted-attempt 0))
                      :deadline '(:kind none)
                      :descriptor descriptor)))
         (fields (e-board-orchestration-fact-record-fields fact)))
    (let* ((attributes (plist-get fields :attributes))
           (replayed
            (json-parse-string (json-encode attributes)
                               :object-type 'plist :array-type 'list
                               :null-object nil :false-object :json-false)))
      (should
       (equal
        (plist-get
         (plist-get
          (e-board-orchestration-reduce
           (list (list :record-kind 'fact :tags '(orchestration)
                       :attributes replayed)))
          :manifest)
         :descriptor)
        descriptor)))))

(ert-deftest e-board-orchestration-test-manifest-descriptor-is-bounded-plist ()
  "Opaque descriptors remain keyed and bounded board facts."
  (dolist (descriptor (list '(not-a-key "value")
                            '(:odd)
                            '(:value "first" :value "second")
                            (list :value
                                  (make-string
                                   e-board-orchestration-fact-byte-limit ?x))))
    (should-error
     (e-board-orchestration-validate-fact
      (e-board-orchestration-test--fact
       'manifest "bad-descriptor"
       (list :run-id "run-1" :tasks nil :deadline '(:kind none)
             :descriptor descriptor)))
     :type 'e-board-orchestration-invalid-fact))
  (let* ((fact (e-board-orchestration-test--fact
                'manifest "closed-manifest"
                '(:run-id "run-1" :tasks nil :deadline (:kind none)
                  :date "application-field")))
         (payload (plist-get (e-board-orchestration-validate-fact fact) :payload)))
    (should-not (plist-member payload :date))))

(ert-deftest e-board-orchestration-test-live-codec-rejects-pre-wire-facts ()
  "Ordinary Board reads do not retain the stopped-v5 compatibility decoder."
  (should-error
   (e-board-orchestration-fact-from-record
    '(:record-kind fact :tags (orchestration)
      :attributes
      (:orchestration-version 1 :orchestration-type "manifest"
       :orchestration-idempotency-key "manifest-1"
       :orchestration-payload
       (:run-id "run-1" :tasks nil :deadline (:kind "none")))))
   :type 'e-board-orchestration-invalid-fact))

(ert-deftest e-board-orchestration-test-required-and-optional-completion ()
  "Optional work cannot block a successful required group."
  (let* ((facts (list (e-board-orchestration-test--manifest)
                      (e-board-orchestration-test--fact
                       'terminal-report "required-0"
                       '(:run-id "run-1" :task-key "required" :attempt 0 :status done
                         :summary "done" :outputs []))))
         (projection (e-board-orchestration-reduce facts 0)))
    (should (eq (plist-get projection :terminal-status) 'done))))

(ert-deftest e-board-orchestration-test-selects-manifest-attempt ()
  "Only the manifest's accepted attempt settles the task."
  (let* ((facts (list (e-board-orchestration-test--manifest
                       (list '(:task-key "task" :required t :accepted-attempt 1)))
                      (e-board-orchestration-test--fact
                       'terminal-report "task-0"
                       '(:run-id "run-1" :task-key "task" :attempt 0 :status done
                         :summary "old" :outputs []))))
         (projection (e-board-orchestration-reduce facts)))
    (should (eq (plist-get (car (plist-get projection :tasks)) :state) 'pending))
    (should-not (plist-get projection :terminal-status))))

(ert-deftest e-board-orchestration-test-retry-selection-advances-exact-attempt ()
  "A durable retry selection supersedes only the named task attempt."
  (let* ((facts
          (list
           (e-board-orchestration-test--manifest
            (list '(:task-key "task" :required t :accepted-attempt 0)))
           (e-board-orchestration-test--fact
            'task-attempt "task-running-0"
            '(:run-id "run-1" :task-key "task" :attempt 0 :status running))
           (e-board-orchestration-test--fact
            'attempt-selection "task-selected-1"
            '(:run-id "run-1" :task-key "task" :attempt 1))
           (e-board-orchestration-test--fact
            'terminal-report "task-done-0"
            '(:run-id "run-1" :task-key "task" :attempt 0 :status done
              :summary "stale" :outputs []))))
         (projection (e-board-orchestration-reduce facts))
         (task (car (plist-get projection :tasks))))
    (should (= (plist-get task :accepted-attempt) 1))
    (should (eq (plist-get task :state) 'pending))
    (should-not (plist-get task :accepted-report))
    (should-not (plist-get projection :terminal-status))))

(ert-deftest e-board-orchestration-test-conflicting-report-is-visible ()
  "Conflicting accepted reports block a successful terminal projection."
  (let* ((facts (list (e-board-orchestration-test--manifest
                       (list '(:task-key "task" :required t :accepted-attempt 0)))
                      (e-board-orchestration-test--fact
                       'terminal-report "first"
                       '(:run-id "run-1" :task-key "task" :attempt 0 :status done :summary "a" :outputs []))
                      (e-board-orchestration-test--fact
                       'terminal-report "second"
                       '(:run-id "run-1" :task-key "task" :attempt 0 :status failed :summary "b" :outputs []))))
         (projection (e-board-orchestration-reduce facts)))
    (should (plist-get projection :conflicts))
    (should (eq (plist-get projection :terminal-status) 'failed))))

(ert-deftest e-board-orchestration-test-deadline-is-evidence-not-cancellation ()
  "An expired deadline changes only the projection evidence."
  (let* ((projection (e-board-orchestration-reduce
                      (list (e-board-orchestration-test--manifest nil '(:kind at :at 10))) 11))
         (task (car (plist-get projection :tasks))))
    (should (plist-get (plist-get projection :deadline) :expired))
    (should (eq (plist-get task :state) 'pending))
    (should-not (plist-get projection :terminal-status))))

(ert-deftest e-board-orchestration-test-replay-keeps-one-continuation-publication ()
  "Duplicate report replay retains the manifest publication key and acknowledgement."
  (let* ((manifest
          (e-board-orchestration-test--fact
           'manifest "manifest-1"
           '(:run-id "run-1"
             :tasks ((:task-key "task" :required t :accepted-attempt 0))
             :continuation (:session-id "coordinator" :prompt "reconcile"
                            :publication-key "publication-1"))))
         (report
          (e-board-orchestration-test--fact
           'terminal-report "report-1"
           '(:run-id "run-1" :task-key "task" :attempt 0 :status done
             :summary "done" :outputs [])))
         (claim
          (e-board-orchestration-test--fact
           'continuation-claim "publication-1:published"
           '(:run-id "run-1" :publication-key "publication-1"
             :status published)))
         (projection
          (e-board-orchestration-reduce
           (list manifest report report claim))))
      (should (eq (plist-get projection :terminal-status) 'done))
      (should (eq (plist-get (plist-get projection :continuation) :state) 'published))
      (should (= (length (plist-get projection :reports)) 1))))

(ert-deftest e-board-orchestration-test-continuation-waits-for-durable-claim ()
  "A terminal run is waiting until a publication claim commits."
  (let* ((manifest
          (e-board-orchestration-test--fact
           'manifest "manifest-waiting"
           '(:run-id "run-waiting"
             :tasks ((:task-key "task" :required t :accepted-attempt 0))
             :continuation (:session-id "coordinator" :prompt "reconcile"
                            :publication-key "publication-waiting"))))
         (report
          (e-board-orchestration-test--fact
           'terminal-report "report-waiting"
           '(:run-id "run-waiting" :task-key "task" :attempt 0 :status done
             :summary "done" :outputs [])))
         (pending
          (e-board-orchestration-test--fact
           'continuation-claim "claim-waiting-pending"
           '(:run-id "run-waiting" :publication-key "publication-waiting"
             :status pending)))
         (waiting-projection
          (e-board-orchestration-reduce (list manifest report)))
         (pending-projection
          (e-board-orchestration-reduce
           (list manifest report pending))))
    (should (eq (plist-get (plist-get waiting-projection :continuation) :state)
                'waiting))
    (should (eq (plist-get (plist-get pending-projection :continuation) :state)
                'pending))))

(ert-deftest e-board-orchestration-test-continuation-outcome-is-separate-from-claim ()
  "A published admission claim does not imply coordinator execution success."
  (let* ((manifest
          (e-board-orchestration-test--fact
           'manifest "manifest-outcome"
           '(:run-id "run-outcome"
             :tasks ((:task-key "task" :required t :accepted-attempt 0))
             :continuation (:session-id "coordinator" :prompt "reconcile"
                            :publication-key "publication-outcome"))))
         (report
          (e-board-orchestration-test--fact
           'terminal-report "report-outcome"
           '(:run-id "run-outcome" :task-key "task" :attempt 0 :status done
             :summary "done" :outputs [])))
         (claim
          (e-board-orchestration-test--fact
           'continuation-claim "claim-outcome"
           '(:run-id "run-outcome" :publication-key "publication-outcome"
             :status published)))
         (projection
          (e-board-orchestration-reduce (list manifest report claim))))
    (should (eq (plist-get (plist-get projection :continuation) :state)
                'published))
    (should-not (plist-get projection :continuation-outcome))
    (should-not (plist-get (plist-get projection :continuation)
                           :execution-outcome))))

(ert-deftest e-board-orchestration-test-continuation-outcome-replays-idempotently ()
  "One durable successful coordinator outcome survives duplicate replay."
  (let* ((manifest
          (e-board-orchestration-test--fact
           'manifest "manifest-outcome-replay"
           '(:run-id "run-outcome-replay"
             :tasks ((:task-key "task" :required t :accepted-attempt 0))
             :continuation (:session-id "coordinator" :prompt "reconcile"
                            :publication-key "publication-outcome-replay"))))
         (report
          (e-board-orchestration-test--fact
           'terminal-report "report-outcome-replay"
           '(:run-id "run-outcome-replay" :task-key "task" :attempt 0
             :status done :summary "done" :outputs [])))
         (outcome
          (e-board-orchestration-test--fact
           'continuation-outcome
           (e-board-orchestration-continuation-outcome-key
            "run-outcome-replay" "publication-outcome-replay")
           '(:run-id "run-outcome-replay"
             :publication-key "publication-outcome-replay"
             :status done :turn-id "turn-coordinator")))
         (projection
          (e-board-orchestration-reduce
           (list manifest report outcome outcome))))
    (should (equal
             (plist-get (plist-get projection :continuation-outcome) :status)
             'done))
    (should (equal
             (plist-get (plist-get projection :continuation-outcome) :turn-id)
             "turn-coordinator"))
    (should (= (length (plist-get projection :continuation-outcomes)) 1))))

(ert-deftest e-board-orchestration-test-continuation-failure-stays-visible ()
  "Failed or cancelled coordinator outcomes cannot look like success."
  (dolist (status '(failed cancelled))
    (let* ((manifest
            (e-board-orchestration-test--fact
             'manifest (format "manifest-outcome-%s" status)
             (list :run-id (format "run-outcome-%s" status)
                   :tasks '((:task-key "task" :required t :accepted-attempt 0))
                   :continuation
                   (list :session-id "coordinator" :prompt "reconcile"
                         :publication-key (format "publication-outcome-%s" status)))))
           (report
            (e-board-orchestration-test--fact
             'terminal-report (format "report-outcome-%s" status)
             (list :run-id (format "run-outcome-%s" status)
                   :task-key "task" :attempt 0 :status 'done
                   :summary "done" :outputs [])))
           (outcome
            (e-board-orchestration-test--fact
             'continuation-outcome
             (e-board-orchestration-continuation-outcome-key
              (format "run-outcome-%s" status)
              (format "publication-outcome-%s" status))
             (list :run-id (format "run-outcome-%s" status)
                   :publication-key (format "publication-outcome-%s" status)
                   :status status)))
           (projection (e-board-orchestration-reduce
                        (list manifest report outcome))))
      (should (eq (plist-get (plist-get projection :continuation-outcome)
                             :status)
                  status)))))

(ert-deftest e-board-orchestration-test-continuation-view-is-consumer-shaped ()
  "Continuation input carries terminal evidence without recursive manifest data."
  (let* ((manifest
          (e-board-orchestration-test--fact
           'manifest "manifest-1"
           '(:run-id "run-1"
             :tasks ((:task-key "daily" :required t :accepted-attempt 0))
             :continuation (:session-id "coordinator"
                            :prompt "DO NOT EMBED THIS PROMPT"
                            :publication-key "publication-1"))))
         (report
          (e-board-orchestration-test--fact
           'terminal-report "report-1"
           '(:run-id "run-1" :task-key "daily" :attempt 0 :status done
             :summary "applied" :outputs ((:path "daily.org"))
             :result (:source-status unavailable :reason "Slack unavailable")
             :participant-session-id "worker-1")))
         (view
          (e-board-orchestration-continuation-view
           (e-board-orchestration-reduce (list manifest report))))
         (task (car (plist-get view :tasks)))
         (accepted (plist-get task :accepted-report))
         (serialized (prin1-to-string view)))
    (should (equal (plist-get view :run-id) "run-1"))
    (should (eq (plist-get view :terminal-status) 'done))
    (should (eq (plist-get task :state) 'done))
    (should (equal (plist-get accepted :summary) "applied"))
    (should (equal (plist-get accepted :result)
                   '(:source-status unavailable :reason "Slack unavailable")))
    (should (equal (plist-get accepted :outputs) '((:path "daily.org"))))
    (should-not (plist-member view :manifest))
    (should-not (plist-member view :continuation))
    (should-not (string-match-p "DO NOT EMBED THIS PROMPT" serialized))))
