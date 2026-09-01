;;; e-board-orchestration-test.el --- Tests for durable board runs -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'e-board)
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

(ert-deftest e-board-orchestration-test-publication-is-idempotent ()
  "The board source key makes retrying one durable fact a no-op."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "orchestration-test")))
    (let ((fact (e-board-orchestration-test--manifest)))
      (should (eq (e-board-publication-status
                   (e-board-orchestration-publish-fact board fact)) 'posted))
      (should (eq (e-board-publication-status
                   (e-board-orchestration-publish-fact board fact)) 'duplicate))
      (should (= (length (e-board-messages board)) 1)))))

(ert-deftest e-board-orchestration-test-wire-roundtrip-preserves-lisp-shapes ()
  "JSON replay preserves enums, task arrays, and opaque output values."
  (let* ((e-board--registry (make-hash-table :test 'equal))
         (source (e-board-create :id "wire-source"))
         (restored (e-board-create :id "wire-restored"))
         (fact
          (e-board-orchestration-test--fact
           'terminal-report "report-1"
           '(:run-id "run-1" :task-key "task" :attempt 0 :status done
             :summary "done"
             :outputs ((:kind artifact :uri "file" :value (:state ready))))))
         (expected (e-board-orchestration-validate-fact fact)))
    (e-board-orchestration-publish-fact source fact)
    (let* ((attributes (e-board-message-attributes (car (e-board-messages source))))
           (replayed
            (json-parse-string (json-encode attributes)
                               :object-type 'plist :array-type 'list
                               :null-object nil :false-object :json-false)))
      (e-board-post-fact restored :tags '(orchestration) :attributes replayed
                         :source-fact-key '("wire" "report-1" 0))
      (should (equal (e-board-orchestration-fact-from-message
                      (car (e-board-messages restored)))
                     expected)))))

(ert-deftest e-board-orchestration-test-manifest-descriptor-survives-wire-replay ()
  "Application recovery inputs remain opaque and durable across JSON replay."
  (let* ((e-board--registry (make-hash-table :test 'equal))
         (source (e-board-create :id "descriptor-source"))
         (restored (e-board-create :id "descriptor-restored"))
         (descriptor '(:date "2026-09-01" :mode populate
                       :path "daily/2026-09-01.org"
                       :window (:started-at "2026-09-01T09:39:10Z")))
         (fact (e-board-orchestration-test--fact
                'manifest "manifest-descriptor"
                (list :run-id "run-1"
                      :tasks '((:task-key "task" :required t :accepted-attempt 0))
                      :deadline '(:kind none)
                      :descriptor descriptor))))
    (e-board-orchestration-publish-fact source fact)
    (let* ((attributes (e-board-message-attributes (car (e-board-messages source))))
           (replayed
            (json-parse-string (json-encode attributes)
                               :object-type 'plist :array-type 'list
                               :null-object nil :false-object :json-false)))
      (e-board-post-fact restored :tags '(orchestration) :attributes replayed
                         :source-fact-key '("wire" "manifest-descriptor" 0))
      (should
       (equal
        (plist-get
         (plist-get (e-board-orchestration-run-projection restored "run-1")
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

(ert-deftest e-board-orchestration-test-legacy-json-facts-replay ()
  "Pre-wire duplicate-key JSON objects restore into a run projection."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "legacy-wire")))
    (e-board-post-fact
     board :tags '(orchestration)
     :source-fact-key '("legacy" "manifest-1" 0)
     :attributes
     '(:orchestration-version 1 :orchestration-type "manifest"
       :orchestration-idempotency-key "manifest-1"
       :orchestration-payload
       (:run-id "run-1"
        :tasks (:task-key ("required" "required" t "accepted-attempt" 0)
                :task-key ("optional" "required" nil "accepted-attempt" 0))
        :deadline (:kind "none"))))
    (e-board-post-fact
     board :tags '(orchestration)
     :source-fact-key '("legacy" "report-1" 0)
     :attributes
     '(:orchestration-version 1 :orchestration-type "terminal-report"
       :orchestration-idempotency-key "report-1"
       :orchestration-payload
       (:run-id "run-1" :task-key "required" :attempt 0 :status "done"
        :summary "done"
        :outputs (:kind ("artifact" "uri" "file" "outputs"
                         (:kind ("artifact" "uri" "nested")))))))
    (let* ((projection
            (e-board-orchestration-run-projection board "run-1"))
           (report (car (plist-get projection :reports))))
      (should (eq (plist-get projection :terminal-status) 'done))
      (should (= (length (plist-get projection :tasks)) 2))
      (should (equal (plist-get report :outputs)
                     '((:kind artifact :uri "file"
                        :outputs ((:kind artifact :uri "nested")))))))))

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

(ert-deftest e-board-orchestration-test-run-projection-waits-for-restoration ()
  "A run is not missing until its board journal has replayed."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board-orchestration--restoration-states (make-hash-table :test 'equal))
        (board (e-board-create :id "restoring-run")))
    (e-board-orchestration-mark-restoring board)
    (should (eq (plist-get (e-board-orchestration-run-projection board "run-1") :state)
                'not-restored-yet))
    (e-board-orchestration-mark-restored board)
    (should (eq (plist-get (e-board-orchestration-run-projection board "run-1") :state)
                'missing))))

(ert-deftest e-board-orchestration-test-replay-keeps-one-continuation-publication ()
  "Duplicate report replay retains the manifest publication key and acknowledgement."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "continuation-replay")))
    (e-board-orchestration-publish-fact
     board
     (e-board-orchestration-test--fact
      'manifest "manifest-1"
      '(:run-id "run-1"
        :tasks ((:task-key "task" :required t :accepted-attempt 0))
        :continuation (:session-id "coordinator" :prompt "reconcile"
                       :publication-key "publication-1"))))
    (let ((report (e-board-orchestration-test--fact
                   'terminal-report "report-1"
                   '(:run-id "run-1" :task-key "task" :attempt 0 :status done
                     :summary "done" :outputs []))))
      (e-board-orchestration-publish-fact board report)
      (e-board-orchestration-publish-fact board report))
    (e-board-orchestration-publish-fact
     board
     (e-board-orchestration-test--fact
      'continuation-claim "publication-1:published"
      '(:run-id "run-1" :publication-key "publication-1" :status published)))
    (let ((projection (e-board-orchestration-run-projection board "run-1")))
      (should (eq (plist-get projection :terminal-status) 'done))
      (should (eq (plist-get (plist-get projection :continuation) :state) 'published))
      (should (= (length (plist-get projection :reports)) 1)))))
