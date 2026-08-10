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
