;;; e-board-processing-subscription-test.el --- Processing contract tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-board)

(defmacro e-board-processing-test--with-board (&rest body)
  "Run BODY with an isolated board containing one participant."
  (declare (indent 0))
  `(let ((e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0))
     (let ((board (e-board-create :id "board")))
       (e-board-add-participant board :id "processor"
                                :create-pickup-subscription-id "address")
       ,@body)))

(ert-deftest e-board-processing-test-subscription-contract-defaults-and-replacement ()
  "Normal subscriptions preserve defaults; processing declarations persist." 
  (e-board-processing-test--with-board
    (let ((normal (e-board-subscribe board "processor" '(:tags (normal)) :id "normal"))
          (process (e-board-subscribe board "processor" '(:tags (process)) :id "process"
                                      :delivery 'process :priority 100
                                      :self-delivery t :failure-policy 'consume)))
      (should (eq (e-board-subscription-delivery normal) 'normal))
      (should-not (e-board-subscription-priority normal))
      (should-not (e-board-subscription-self-delivery normal))
      (should-not (e-board-subscription-failure-policy normal))
      (should (eq (e-board-subscription-delivery process) 'process))
      (should (= (e-board-subscription-priority process) 100))
      (should (e-board-subscription-self-delivery process))
      (should (eq (e-board-subscription-failure-policy process) 'consume))
      (let ((replacement
             (e-board-replace-subscription board "process" '(:tags (updated)))))
        (should (eq (e-board-subscription-delivery replacement) 'process))
        (should (= (e-board-subscription-priority replacement) 100))
        (should (e-board-subscription-self-delivery replacement))
        (should (eq (e-board-subscription-failure-policy replacement) 'consume))))))

(ert-deftest e-board-processing-test-invalid-replacement-preserves-active-subscription ()
  "An invalid replacement must not cancel the active subscription."
  (e-board-processing-test--with-board
    (let ((subscription
           (e-board-subscribe board "processor" '(:tags (old)) :id "old")))
      (should-error
       (e-board-replace-subscription board "old" '(:tags (new)) :priority 1)
       :type 'e-board-error)
      (should (eq (e-board-subscription-state subscription) 'active))
      (should (eq (e-board-find-subscription board "old") subscription)))))

(ert-deftest e-board-processing-test-priority-order-is-stable-by-subscription-id ()
  "Processing subscriptions sort by descending priority then subscription id."
  (e-board-processing-test--with-board
    (let ((low (e-board-subscribe board "processor" '(:tags (x)) :id "z"
                                 :delivery 'process :priority 1))
          (first (e-board-subscribe board "processor" '(:tags (x)) :id "a"
                                   :delivery 'process :priority 10))
          (second (e-board-subscribe board "processor" '(:tags (x)) :id "b"
                                    :delivery 'process :priority 10)))
      (should (equal (mapcar #'e-board-subscription-id
                             (e-board-order-processing-subscriptions
                              (list low second first)))
                     '("a" "b" "z"))))))

(ert-deftest e-board-processing-test-normal-subscription-symbol-id-remains-compatible ()
  "Normal subscriptions retain their legacy non-string identifier contract."
  (e-board-processing-test--with-board
    (let ((subscription
           (e-board-subscribe board "processor" '(:tags (normal))
                              :id 'legacy-normal)))
      (should (eq (e-board-subscription-id subscription) 'legacy-normal))
      (should (eq (e-board-find-subscription board 'legacy-normal) subscription)))))

(ert-deftest e-board-processing-test-subscription-ids-are-durable-strings ()
  "Processing subscription ids reject values without a persistent total order."
  (e-board-processing-test--with-board
    (should-error
     (e-board-subscribe board "processor" '(:tags (x)) :id 'symbol
                        :delivery 'process :priority 10)
     :type 'wrong-type-argument)
    (should-error
     (e-board-subscribe board "processor" '(:tags (x)) :id 1
                        :delivery 'process :priority 10)
     :type 'wrong-type-argument)
    (let* ((first-id (copy-sequence "a"))
           (second-id (copy-sequence "b"))
           (first (e-board-subscribe board "processor" '(:tags (x)) :id first-id
                                     :delivery 'process :priority 10))
           (second (e-board-subscribe board "processor" '(:tags (x)) :id second-id
                                      :delivery 'process :priority 10)))
      (aset first-id 0 ?z)
      (aset second-id 0 ?z)
      (should (equal (mapcar #'e-board-subscription-id
                             (e-board-order-processing-subscriptions
                              (list second first)))
                     '("a" "b"))))))

(ert-deftest e-board-processing-test-invalid-subscription-combinations ()
  "Processing-only fields reject incompatible declarations and bad bounds."
  (e-board-processing-test--with-board
    (should-error (e-board-subscribe board "processor" '(:tags (x)) :priority 1)
                  :type 'e-board-error)
    (should-error (e-board-subscribe board "processor" '(:tags (x))
                                     :failure-policy 'consume)
                  :type 'e-board-error)
    (should-error (e-board-subscribe board "processor" '(:tags (x))
                                     :delivery 'process :effect '(:post-input))
                  :type 'e-board-error)
    (should-error (e-board-subscribe board "processor" '(:tags (x))
                                     :delivery 'process
                                     :priority (1+ e-board-processing-priority-max))
                  :type 'wrong-type-argument)
    (should-error (e-board-subscribe board "processor" '(:tags (x))
                                     :delivery 'process :failure-policy 'retry)
                  :type 'wrong-type-argument)))

(ert-deftest e-board-processing-test-record-identities-remain-frozen ()
  "Caller mutation cannot rewrite retained processing identities or envelopes."
  (e-board-processing-test--with-board
    (let* ((chain-id (copy-sequence "chain"))
           (root-id (copy-sequence "root"))
           (candidate-id (copy-sequence "candidate"))
           (caused-by-id (copy-sequence "caused-by"))
           (result-id (copy-sequence "result"))
           (subscription-id (copy-sequence "subscription"))
           (participant-id (copy-sequence "processor"))
           (replacement-id (copy-sequence "replacement"))
           (chain (e-board-record-processing-chain
                   board :id chain-id :root-message-id root-id
                   :candidate-message-id candidate-id :caused-by-message-id caused-by-id
                   :processor-history nil :processing-depth 0))
           (result (e-board-record-processing-result
                    board :id result-id :chain-id chain-id
                    :subscription-id subscription-id :participant-id participant-id
                    :candidate-message-id candidate-id :outcome 'replace
                    :replacement-message-id replacement-id :failure-policy 'pass)))
      (dolist (identity (list chain-id root-id candidate-id caused-by-id result-id
                              subscription-id participant-id replacement-id))
        (aset identity 0 ?x))
      (should (equal (e-board-processing-chain-id chain) "chain"))
      (should (equal (e-board-processing-chain-root-message-id chain) "root"))
      (should (equal (e-board-processing-chain-candidate-message-id chain) "candidate"))
      (should (equal (e-board-processing-chain-caused-by-message-id chain) "caused-by"))
      (should (equal (e-board-processing-result-id result) "result"))
      (should (equal (e-board-processing-result-chain-id result) "chain"))
      (should (equal (e-board-processing-result-subscription-id result) "subscription"))
      (should (equal (e-board-processing-result-participant-id result) "processor"))
      (should (equal (e-board-processing-result-candidate-message-id result) "candidate"))
      (should (equal (e-board-processing-result-replacement-message-id result) "replacement"))
      (let ((chain-envelope (e-board-processing-record-envelope chain))
            (result-envelope (e-board-processing-record-envelope result)))
        (should (equal (plist-get chain-envelope :id) "chain"))
        (should (equal (plist-get chain-envelope :root-message-id) "root"))
        (should (equal (plist-get chain-envelope :candidate-message-id) "candidate"))
        (should (equal (plist-get chain-envelope :caused-by-message-id) "caused-by"))
        (should (equal (plist-get result-envelope :id) "result"))
        (should (equal (plist-get result-envelope :chain-id) "chain"))
        (should (equal (plist-get result-envelope :subscription-id) "subscription"))
        (should (equal (plist-get result-envelope :participant-id) "processor"))
        (should (equal (plist-get result-envelope :candidate-message-id) "candidate"))
        (should (equal (plist-get result-envelope :replacement-message-id)
                       "replacement"))))))

(ert-deftest e-board-processing-test-public-record-paths-cannot-mutate-ledger ()
  "Returned records, notification records, envelopes, and times are detached."
  (e-board-processing-test--with-board
    (let* ((created-at (list (copy-sequence "created")))
           notified
           (board (e-board-create
                   :id "notified"
                   :processing-record-notification-function
                   (lambda (_board record _type) (setq notified record))))
           (chain (e-board-record-processing-chain
                   board :id "chain" :root-message-id "root"
                   :candidate-message-id "candidate" :caused-by-message-id "root"
                   :processor-history (list "processor") :processing-depth 0
                   :created-at created-at))
           (envelope (e-board-processing-record-envelope chain)))
      (setcar created-at "changed")
      (setcar (e-board-processing-chain-processor-history chain) "changed")
      (setcar (e-board-processing-chain-processor-history notified) "changed")
      (setcar (plist-get envelope :processor-history) "changed")
      (setcar (plist-get envelope :created-at) "changed")
      (let ((listed (car (e-board-list-processing-chains board))))
        (setcar (e-board-processing-chain-processor-history listed) "changed"))
      (let ((retained (car (e-board-list-processing-chains board))))
        (should (equal (e-board-processing-chain-processor-history retained)
                       '("processor")))
        (should (equal (e-board-processing-chain-created-at retained)
                       '("created")))))))

(ert-deftest e-board-processing-test-records-round-trip-through-durable-envelopes ()
  "Processing chain and result records retain immutable replay state." 
  (e-board-processing-test--with-board
    (let* ((history (list (cons "processor" "subscription")))
           (chain (e-board-record-processing-chain
                   board :id "chain" :root-message-id "root"
                   :candidate-message-id "candidate" :caused-by-message-id "root"
                   :processor-history history :processing-depth 1 :created-at 1))
           (result (e-board-record-processing-result
                    board :id "result" :chain-id "chain" :subscription-id "subscription"
                    :participant-id "processor" :candidate-message-id "candidate"
                    :outcome 'fail :failure-policy 'pass :failure '(:message "bad")
                    :created-at 2))
           (chain-envelope (e-board-processing-record-envelope chain))
           (result-envelope (e-board-processing-record-envelope result)))
      (setcar history 'changed)
      (should (equal (e-board-processing-chain-processor-history chain)
                     '(("processor" . "subscription"))))
      (let ((replayed (e-board-create :id "replayed")))
        (e-board-import-processing-record replayed chain-envelope)
        (e-board-import-processing-record replayed result-envelope)
        (should (equal (mapcar #'e-board-processing-chain-id
                               (e-board-list-processing-chains replayed))
                       '("chain")))
        (should (equal (mapcar #'e-board-processing-result-outcome
                               (e-board-list-processing-results replayed))
                       '(fail)))
        (should (eq (e-board-processing-result-failure-policy
                     (car (e-board-list-processing-results replayed)))
                    'pass))))))

;;; e-board-processing-subscription-test.el ends here
