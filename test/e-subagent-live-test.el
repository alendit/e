;;; e-subagent-live-test.el --- Tests for private live child ownership -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-backend)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-session)
(require 'e-work)
(require 'e-board-observation)
(require 'e-board-sqlite-service)
(require 'e-runtime-store)
(require 'e-session-sqlite)
(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(require 'e-subagent-live)
(require 'e-subagent-runner)

(defun e-subagent-live-test--record (board-id participant-id &optional assignment)
  "Return detached runner context for BOARD-ID/PARTICIPANT-ID."
  (list :board-id board-id :participant-id participant-id
        :type :reviewer :session-id participant-id
        :parent-session-id "parent" :run-id (plist-get assignment :run-id)
        :task-key (plist-get assignment :task-key)
        :attempt (plist-get assignment :attempt)
        :status 'running))

(cl-defun e-subagent-live-test--create-board-session
    (harness &key id metadata)
  "Create and bind one disposable SQL session for HARNESS."
  (let* ((creation
          (e-chat-service-create-session-start
           :harness harness :id id :metadata metadata))
         (binding (e-chat-service-binding-start harness id nil t)))
    (e-work-with-batch-await
      (e-work-await-batch creation :timeout 5.0))
    (e-work-with-batch-await
      (e-work-await-batch binding :timeout 5.0))))

(defun e-subagent-live-test--admit-participant
    (service board-id session-id participant-id)
  "Admit PARTICIPANT-ID to disposable BOARD-ID through SERVICE."
  (let* ((principal (format "chat:%s" session-id))
         (policy (list :participant-id participant-id
                       :pickup-selector '(:tags (subagent))
                       :observer-selector
                       (list :subject-participant-id participant-id)
                       :default-tags '(subagent) :default-to participant-id))
         (session
          (e-board-sqlite-service-session-admission
           :id session-id :metadata (list :name session-id)
           :principal principal :board-id board-id
           :association-role "participant" :routing-policy policy))
         (records (copy-tree (plist-get session :admission-records) t))
         (position 0))
    (dolist (record records)
      (plist-put record :journal-position (cl-incf position)))
    (e-work-with-batch-await
      (e-board-sqlite-service-admit-participant-start
       service session-id board-id records (plist-get session :query-delta)
       (list :id participant-id :name session-id
             :author "subagent-live-test" :principal principal
             :controller principal :role 'participant :state 'active
             :subscription-id (concat "sub-" participant-id)
             :publication-pending nil)))))

(ert-deftest e-subagent-live-test-board-participant-key-and-capability-shape ()
  "Equal participant identities on two Boards remain independent capabilities."
  (let ((owner (e-subagent-live-create))
        (callbacks (list :report (lambda (_value) t))))
    (e-subagent-live-reserve-admission
     owner "board-a" "session-1"
     :work-handle 'work-a :assignment '(:run-id "run-a")
     :callbacks callbacks)
    (e-subagent-live-install
     owner "board-a" "session-1"
     :harness 'harness-a :work-handle 'work-a :cancel #'ignore
     :callbacks callbacks)
    (e-subagent-live-reserve-admission
     owner "board-b" "session-1"
     :work-handle 'work-b :assignment '(:run-id "run-b")
     :callbacks callbacks)
    (e-subagent-live-install
     owner "board-b" "session-1"
     :harness 'harness-b :work-handle 'work-b :cancel #'ignore
     :callbacks callbacks)
    (should (eq 'harness-a
                (e-subagent-live-harness owner "board-a" "session-1")))
    (should (eq 'harness-b
                (e-subagent-live-harness owner "board-b" "session-1")))
    (dolist (board '("board-a" "board-b"))
      (let ((entry (e-subagent-live-get owner board "session-1")))
        (should entry)
        (should-not (plist-member entry :status))
        (should-not (plist-member entry :result))
        (should-not (plist-member entry :outputs))
        (should-not (plist-member entry :assignment))))))

(ert-deftest e-subagent-live-test-progress-is-bounded-and-latest-only ()
  "Progress replacement retains one bounded in-flight snapshot only."
  (let ((owner (e-subagent-live-create)))
    (e-subagent-live-reserve-admission
     owner "board" "session" :work-handle 'work)
    (e-subagent-live-install owner "board" "session" :work-handle 'work)
    (e-subagent-live-record-progress
     owner "board" "session" '(:event tool-started :summary "first"))
    (e-subagent-live-record-progress
     owner "board" "session" '(:event tool-finished :summary "second"))
    (should (equal 'tool-finished
                   (plist-get
                    (e-subagent-live-progress owner "board" "session")
                    :event)))))

(ert-deftest e-subagent-live-test-reference-round-trip-and-retirement ()
  "Waitable identity references round-trip and become unavailable on cleanup."
  (let* ((owner (e-subagent-live-create))
         (reference (e-subagent-live-reference "board:one" "session/one")))
    (should (equal '("board:one" "session/one")
                   (e-subagent-live-reference-identity reference)))
    (e-subagent-live-reserve-admission owner "board:one" "session/one")
    (e-subagent-live-install owner "board:one" "session/one"
                             :work-handle 'work)
    (should (eq 'work
                (e-subagent-live-work-handle owner "board:one" "session/one")))
    (e-subagent-live-remove owner "board:one" "session/one")
    (should-not (e-subagent-live-work-handle owner "board:one" "session/one"))))

(ert-deftest e-subagent-live-test-ad-hoc-terminal-settlement-is-durable-before-cleanup ()
  "Ad-hoc terminal payload is published once, then live state is retired."
  (let* ((owner (e-subagent-live-create))
         (record (e-subagent-live-test--record "board" "session"))
         (published nil))
    (e-subagent-live-reserve-admission owner "board" "session")
    (e-subagent-live-install owner "board" "session")
    (cl-letf (((symbol-function 'e-subagent--publish-board-fact)
               (lambda (_target &rest args) (push args published))))
      (e-subagent--settle owner "board" "session" record 'target nil 'done
                          :summary "bounded summary"
                          :result '(:answer "ok")
                          :outputs '((:kind artifact :uri "tmp://x"))) )
    (should-not (e-subagent-live-get owner "board" "session"))
    (should (= 1 (length published)))
    (let ((attributes (plist-get (car published) :attributes)))
      (should (equal "bounded summary" (plist-get attributes :result-summary)))
      (should (equal '(:answer "ok") (plist-get attributes :result)))
      (should (<= (length
                   (e-runtime-store-codec-encode
                    (list :summary (plist-get attributes :result-summary)
                          :result (plist-get attributes :result)
                          :outputs (plist-get attributes :outputs))))
                  e-subagent--terminal-payload-byte-limit)))))

(ert-deftest e-subagent-live-test-run-bound-report-remains-canonical ()
  "Run-bound settlement publishes the orchestration report as canonical."
  (let* ((owner (e-subagent-live-create))
         (assignment '(:run-id "run" :task-key "task" :attempt 0))
         (record (e-subagent-live-test--record "board" "session" assignment))
         (facts nil)
         (reports nil))
    (e-subagent-live-reserve-admission owner "board" "session")
    (e-subagent-live-install owner "board" "session")
    (cl-letf (((symbol-function 'e-subagent--publish-board-fact)
               (lambda (_target &rest args) (push args facts)))
              ((symbol-function 'e-board-orchestration-actions-publish-terminal)
               (lambda (_target seen-assignment status &rest args)
                 (setq reports (list seen-assignment status args))))
              ((symbol-function 'e-subagent--effective-settlement)
               (lambda (_state status args) (cons status args))))
      (e-subagent--settle owner "board" "session" record 'target nil 'done
                          :summary "canonical" :result '(:answer "ok")))
    (should-not (e-subagent-live-get owner "board" "session"))
    (should (= 1 (length facts)))
    (should (equal '((:run-id "run" :task-key "task" :attempt 0)
                     done
                     (:summary "canonical" :result (:answer "ok") :outputs []
                      :error nil :author (:session-id "session")))
                   reports))))

(ert-deftest e-subagent-live-test-report-callback-does-not-create-live-result-inventory ()
  "Accepted child reports stay in runner closure state, not the live table."
  (let* ((owner (e-subagent-live-create))
         (record (e-subagent-live-test--record "board" "session"))
         (accepted nil)
         (callbacks
          (list :record (lambda () (copy-tree record))
                :report (lambda (value) (setq accepted (copy-tree value)))
                :reported (lambda () accepted))))
    (e-subagent-live-reserve-admission owner "board" "session"
                                       :callbacks callbacks)
    (e-subagent-live-install owner "board" "session"
                             :callbacks callbacks
                             :report-admission
                             (lambda (_assignment proposed) proposed))
    (should (plist-get (e-subagent-report owner "board" "session"
                                          '((:kind artifact)) "summary"
                                          '(:answer "ok"))
                       :reported))
    (should (equal "summary" (plist-get accepted :summary)))
    (should-not (plist-member
                 (e-subagent-live-get owner "board" "session") :result))
    (should-not (plist-member
                 (e-subagent-live-get owner "board" "session") :outputs))))

(ert-deftest e-subagent-live-test-structural-owner-has-no-process-id-generator ()
  "Runner/action ownership has no legacy process-local id or registry surface."
  (dolist (file '("lisp/layers/agents/e-subagent-live.el"
                  "lisp/layers/agents/e-subagent-runner.el"
                  "lisp/layers/agents/e-subagent-actions.el"
                  "lisp/layers/agents/e-subagents.el"))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((source (buffer-string))
            (registry-symbol (concat "e-" "subagent-" "registry"))
            (process-id-field (concat "sub" "agent-id"))
            (generated-id-pattern (concat "sub" "_[0-9]")))
        (should-not (string-match-p registry-symbol source))
        (should-not (string-match-p process-id-field source))
        (should-not (string-match-p generated-id-pattern source))))))

(ert-deftest e-subagent-live-test-cancel-clears-capabilities-and-publishes-audit ()
  "Cancellation invokes the live handle and removes the entry."
  (let* ((owner (e-subagent-live-create))
         (record (e-subagent-live-test--record "board" "session"))
         (cancelled nil)
         (facts nil))
    (e-subagent-live-reserve-admission owner "board" "session")
    (e-subagent-live-install owner "board" "session"
                             :cancel (lambda () (setq cancelled t)))
    (cl-letf (((symbol-function 'e-subagent--publish-board-fact)
               (lambda (_target &rest args) (push args facts)))
              ((symbol-function 'e-subagent--live-record)
               (lambda (_live _board _participant) record)))
      (e-subagent--cancel-and-retire owner "board" 'target "session"
                                     'interrupt "because"))
    (should cancelled)
    (should-not (e-subagent-live-get owner "board" "session"))
    (should facts)))

(ert-deftest e-subagent-runner-test-live-spawn-admission-installs-durable-identity ()
  "A settled admission installs only the admitted child session identity."
  (let ((e-harness-registry--instances (make-hash-table :test 'equal))
        (e-harness-registry--factories (make-hash-table :test 'equal))
        (e-harness-instance--instances (make-hash-table :test 'equal))
        (e-harness-instance--defaults (make-hash-table :test 'equal))
        (e-subagent--configured-harnesses (make-hash-table :test 'eq :weakness 'key))
        (e-chat-test-support-share-sqlite-store t)
        (e-chat-test-support--shared-sqlite-fixture nil)
        (e-work--unsettled-count 0)
        (e-work--unsettled-generation 0)
        (e-work--unsettled-change-functions nil))
    (unwind-protect
        (progn
          (e-harness-instance-register
           :id :reviewer :name "Reviewer" :kind 'reviewer :subagent t
           :description "Use for review."
           :factory (lambda () (e-harness-create
                                 :backend (e-backend-fake-create :items nil))))
          (let ((parent (e-harness-create
                         :backend (e-backend-fake-create :items nil)))
                (owner (e-subagent-live-create)))
            (cl-letf (((symbol-function 'e-harness-test-create-session)
                       #'e-subagent-live-test--create-board-session))
              (e-harness-test-create-session parent :id "parent")
              (let (settle result)
                (setq result
                      (e-subagent-spawn
                       owner parent "parent"
                       :source-turn-id "turn"
                       :type :reviewer
                       :prompt "finish"
                       :runner
                       (lambda (_harness _session _prompt _seed on-settle)
                         (setq settle on-settle)
                         (list :cancel #'ignore))))
                (should (equal (plist-get result :participant-id)
                               (plist-get result :session-id)))
                (should (stringp (plist-get result :await-ref)))
                (let* ((identity (e-subagent-live-reference-identity
                                  (plist-get result :await-ref)))
                       (board-id (nth 0 identity))
                       (participant-id (plist-get result :participant-id)))
                  (should
                   (e-chat-test--wait-until
                    (lambda ()
                      (e-subagent-live-get owner board-id participant-id))
                    5.0))
                  (should (e-subagent-live-harness
                           owner board-id participant-id))
                  (should-not (plist-member
                               (e-subagent-live-get owner board-id participant-id)
                               :status))
                  (funcall settle 'done :summary "done"))
                (should
                 (e-chat-test--wait-until
                  (lambda ()
                    (null (e-subagent-live-get
                           owner
                           (car (e-subagent-live-reference-identity
                                 (plist-get result :await-ref)))
                           (plist-get result :participant-id))))
                  5.0))))))
      (e-chat-test-support--close-sqlite-fixtures))))

(ert-deftest e-subagent-runner-test-ad-hoc-terminal-is-observable-after-reopen ()
  "The real runner path leaves one durable ad-hoc outcome after live cleanup/reopen."
  (let ((e-harness-registry--instances (make-hash-table :test 'equal))
        (e-harness-registry--factories (make-hash-table :test 'equal))
        (e-harness-instance--instances (make-hash-table :test 'equal))
        (e-harness-instance--defaults (make-hash-table :test 'equal))
        (e-subagent--configured-harnesses
         (make-hash-table :test 'eq :weakness 'key))
        (directory (make-temp-file "e-subagent-live-reopen-" t))
        store store-after parent parent-after owner binding binding-after)
    (unwind-protect
        (progn
          (e-harness-instance-register
           :id :reviewer :name "Reviewer" :kind 'reviewer :subagent t
           :description "Use for review."
           :factory (lambda () (e-harness-create
                                 :backend (e-backend-fake-create :items nil))))
          (setq store (e-session-sqlite-store-create directory :asynchronous t)
                parent (e-harness-create
                        :backend (e-backend-fake-create :items nil)
                        :sessions store)
                owner (e-subagent-live-create))
          (cl-letf (((symbol-function 'e-harness-test-create-session)
                     #'e-subagent-live-test--create-board-session))
            (e-harness-test-create-session parent :id "parent")
            (let (settle result)
              (setq result
                    (e-subagent-spawn
                     owner parent "parent" :source-turn-id "turn"
                     :type :reviewer :prompt "finish"
                     :runner
                     (lambda (_harness _session _prompt _seed on-settle)
                       (setq settle on-settle)
                       (list :cancel #'ignore))))
              (let* ((identity (e-subagent-live-reference-identity
                                (plist-get result :await-ref)))
                     (board-id (nth 0 identity))
                     (participant-id (nth 1 identity)))
                (should (equal participant-id (plist-get result :session-id)))
                (setq binding (e-chat-service-binding parent "parent"))
                (should binding)
                (let ((target (e-chat-service-publication-target binding)))
                  (should
                   (e-chat-test--wait-until
                    (lambda ()
                      (and (e-subagent-live-get owner board-id participant-id)
                           (functionp settle)))
                    5.0))
                  ;; The explicit callback is the real runner settlement path;
                  ;; the live owner retains no terminal payload.
                  (funcall settle 'done :summary "reopened summary"
                           :result '(:answer "ok")
                           :outputs '((:kind artifact :uri "tmp://result")))
                  (should
                   (e-chat-test--wait-until
                    (lambda ()
                      (null (e-subagent-live-get owner board-id participant-id)))
                    5.0))
                  ;; Queue observation after settlement: this is the test's
                  ;; explicit boundary for all preceding durable writes.
                  (let (page row)
                    ;; Live cleanup is not the durable commit boundary.  Keep
                    ;; the read detached, but wait for the publication to be
                    ;; visible before asserting the consumer projection.
                    (should
                     (e-chat-test--wait-until
                      (lambda ()
                        (setq page
                              (e-work-with-batch-await
                                (e-work-await-batch
                                 (e-board-observation-activity-page-start
                                  target :limit 8)
                                 :timeout 5.0)))
                        (setq row
                              (cl-find participant-id
                                       (plist-get page :participants)
                                       :test #'equal
                                       :key (lambda (value)
                                              (plist-get value :participant-id))))
                        (and row
                             (eq (plist-get (plist-get row :outcome) :status)
                                 'done)))
                      5.0))
                    (should row)
                    (should (equal (plist-get row :participant-id)
                                   participant-id))
                    (should (eq (plist-get (plist-get row :outcome) :source)
                                'lifecycle))
                    (should (eq (plist-get (plist-get row :outcome) :status)
                                'done))
                    (should (equal (plist-get (plist-get row :outcome) :summary)
                                   "reopened summary"))
                    (e-chat-service-close-board binding)
                    (setq binding nil)
                    (e-session-sqlite-store-close store)
                    (setq store nil
                          store-after
                          (e-session-sqlite-store-create directory
                                                          :asynchronous t)
                          parent-after
                          (e-harness-create
                           :backend (e-backend-fake-create :items nil)
                           :sessions store-after))
                    (let* ((service-after
                            (e-board-sqlite-service-create
                             (e-session-storage-runtime-store store-after)))
                           (target-after
                            (e-board-sqlite-publication-target-create
                             service-after board-id :author "session:parent"))
                           (reopened
                            (e-work-with-batch-await
                              (e-work-await-batch
                               (e-chat-service-open-board-start
                                board-id parent-after "parent")
                               :timeout 5.0)))
                           (page-after
                            (e-work-with-batch-await
                              (e-work-await-batch
                               (e-board-observation-activity-page-start
                                (e-chat-service-publication-target reopened)
                                :limit 8)
                               :timeout 5.0)))
                           (row-after
                            (cl-find participant-id
                                     (plist-get page-after :participants)
                                     :test #'equal
                                     :key (lambda (value)
                                            (plist-get value :participant-id)))))
                      (should row-after)
                      (should (equal (plist-get row-after :participant-id)
                                     participant-id))
                      (should (equal (plist-get row-after :outcome)
                                     (plist-get row :outcome))))))))))
      (ignore-errors
        (when binding-after
          (e-chat-service-close-board binding-after)))
      (ignore-errors
        (when (and parent-after
                   (setq binding-after
                         (e-chat-service-binding parent-after "parent")))
          (e-chat-service-close-board binding-after)))
      (ignore-errors (when store-after (e-session-sqlite-store-close store-after)))
      (ignore-errors (when store (e-session-sqlite-store-close store)))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-subagent-live-test-board-page-is-invariant-to-live-state ()
  "Absent, mismatched, and stale live entries cannot change a durable page."
  (let* ((directory (make-temp-file "e-subagent-live-invariant-" t))
         (runtime (e-runtime-store-open directory))
         (service (e-board-sqlite-service-create runtime))
         (board-id "invariant-board")
         (target (e-board-sqlite-publication-target-create
                  service board-id :author "subagent-live-test"))
         (owner (e-subagent-live-create))
         (participant-id "participant-1"))
    (unwind-protect
        (progn
          (e-work-with-batch-await
            (e-work-await-batch
             (e-board-sqlite-service-board-create-start
              service board-id "subagent-live-test")
             :timeout 5.0))
          (e-subagent-live-test--admit-participant
           service board-id participant-id participant-id)
          (let* ((baseline
                  (e-work-with-batch-await
                    (e-work-await-batch
                     (e-board-observation-activity-page-start target :limit 8)
                     :timeout 5.0)))
                 (assert-same
                  (lambda ()
                    (let ((current
                           (e-work-with-batch-await
                             (e-work-await-batch
                              (e-board-observation-activity-page-start
                               target :limit 8)
                              :timeout 5.0))))
                      (should (equal current baseline))))))
            ;; A same-looking participant on another Board is not a join.
            (e-subagent-live-reserve-admission
             owner "other-board" participant-id :work-handle 'other-work)
            (e-subagent-live-install owner "other-board" participant-id
                                     :work-handle 'other-work)
            (funcall assert-same)
            ;; A different participant on this Board cannot decorate the row.
            (e-subagent-live-reserve-admission
             owner board-id "other-participant" :work-handle 'other-work)
            (e-subagent-live-install owner board-id "other-participant"
                                     :work-handle 'other-work)
            (funcall assert-same)
            ;; Even a matching transient entry has no authority; after it is
            ;; removed, the same detached page remains the durable answer.
            (e-subagent-live-reserve-admission
             owner board-id participant-id :work-handle 'stale-work)
            (e-subagent-live-install owner board-id participant-id
                                     :work-handle 'stale-work)
            (funcall assert-same)
            (e-subagent-live-remove owner board-id participant-id)
            (funcall assert-same)))
      (ignore-errors (e-runtime-store-close runtime))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(provide 'e-subagent-live-test)

;;; e-subagent-live-test.el ends here
