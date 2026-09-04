;;; e-board-sqlite-test.el --- Durable Board and pickup admission scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board-sqlite)
(require 'e-board-pickup-admission)
(require 'e-board-session-association)
(require 'e-board-runtime)
(require 'e-harness)
(require 'e-session)

(cl-defmacro e-board-sqlite-test--with-store
    ((session-store storage directory) &rest body)
  "Run BODY with one shared disposable SQLite runtime."
  (declare (indent 1) (debug ((symbolp symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-board-sqlite-test-" t))
          (,session-store (e-session-sqlite-store-create ,directory))
          (,storage
           (e-board-storage-sqlite-create
            (e-session-storage-runtime-store ,session-store)))
          (e-board--registry (make-hash-table :test 'equal))
          (e-board-registry--boards (make-hash-table :test 'equal)))
     (unwind-protect (progn ,@body)
       (ignore-errors (e-session-sqlite-store-close ,session-store))
       (delete-directory ,directory t))))

(defun e-board-sqlite-test--board (storage &optional id)
  "Return a durable test Board using synchronous classification."
  (e-board-create
   :id (or id "board") :storage storage :trusted-principal "owner"
   :register nil :input-classification-scheduler
   (lambda (drain) (funcall drain))))

(ert-deftest e-board-sqlite-s92-queued-timeout-is-local-to-one-owner ()
  "An unsent Board request leaves the shared session runtime usable."
  (e-board-sqlite-test--with-store (sessions storage _directory)
    (e-session-create sessions :id "before-timeout")
    (let* ((runtime (e-session-storage-runtime-store sessions))
           (process (e-runtime-store--process runtime))
           (ordinary-filter (process-filter process))
           (captured "")
           active)
      (set-process-filter
       process (lambda (_worker text) (setq captured (concat captured text))))
      (setq active
            (e-runtime-store-submit
             runtime 'write
             '(:op session-append :session-id "active-session"
               :record (:value active))))
      (let ((e-runtime-store-request-timeout 0.02))
        (let ((timeout
               (should-error
                (e-board-storage-create-board storage "must-not-land" "owner")
                :type 'e-runtime-store-timeout)))
          (should (eq (plist-get (cddr timeout) :operation) 'board-create))
          (should (eq (plist-get (cddr timeout) :blocking-operation)
                      'session-append))))
      (should (e-runtime-store-live-p runtime))
      (should-not (plist-get (e-runtime-store-status runtime) :unavailable))
      (let ((deadline (+ (float-time) 5.0)))
        (while (and (not (string-match-p "\n" captured))
                    (< (float-time) deadline))
          (accept-process-output process 0.01)))
      (should (string-match-p "\n" captured))
      (set-process-filter process ordinary-filter)
      (funcall ordinary-filter process captured)
      (should (= (plist-get (e-runtime-store-await runtime active) :revision) 1))
      (should-not (e-board-storage-board storage "must-not-land"))
      (should (equal (plist-get (e-session-create sessions :id "after-timeout") :id)
                     "after-timeout"))
      (should (e-board-storage-create-board storage "after-timeout" "owner"))
      (should (e-runtime-store-live-p runtime)))))

(ert-deftest e-board-sqlite-s92-catalog-recovery-keeps-timer-queued-pickup ()
  "A real catalog receipt recovers before a timer-driven Board transition."
  (let* ((marker (make-temp-file "e-board-s92-catalog-fault-"))
         (process-environment
          (cons "E_RUNTIME_STORE_TEST_FAULT=after-commit"
                (cons "E_RUNTIME_STORE_TEST_FAULT_OPERATION=catalog-put"
                      (cons (concat "E_RUNTIME_STORE_TEST_FAULT_ONCE_FILE=" marker)
                            process-environment)))))
    (delete-file marker)
    (unwind-protect
    (e-board-sqlite-test--with-store (sessions storage _directory)
      (let* ((board (e-board-sqlite-test--board storage "catalog-recovery"))
             (_participant (e-board-add-participant
                            board :id "member" :create-pickup-subscription-id "address"))
             (publication (e-board-post-input
                           board :id "input" :to "member" :content "route"
                           :source-input-key '(catalog-recovery 1 1)))
             (runtime (e-session-storage-runtime-store sessions)))
        ;; The helper's synchronous classifier is the normal durable routing
        ;; path and has created a ready pickup before the catalog write.
        (let* ((pickup-id (car (e-board-publication-pickup-ids publication)))
               claimed)
          (run-at-time 0 nil
                       (lambda ()
                         (setq claimed
                               (e-board-pickup-start-delivery board pickup-id))))
          (should (= (plist-get
                      (e-session-storage-sqlite-write-catalog
                       sessions '((:id "recovery")))
                      :revision)
                     1))
          (sit-for 0.05)
          (should claimed)
          (should (eq (e-board-pickup-state (e-board-pickup board pickup-id))
                      'delivering))
          (should-not (plist-get (e-runtime-store-status runtime) :unavailable))
          (should (equal (plist-get (e-session-create sessions :id "after-catalog") :id)
                         "after-catalog")))))
      (when (file-exists-p marker) (delete-file marker)))))

(ert-deftest e-board-sqlite-s5-publication-is-invisible-until-ack ()
  "A reentrant observer cannot see or build on an unacknowledged fact."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let* ((board (e-board-sqlite-test--board storage))
           (runtime (e-session-storage-runtime-store sessions))
           (process (e-runtime-store--process runtime))
           (ordinary-filter (process-filter process))
           (captured "") response-seen message-during mutation-during)
      (set-process-filter
       process
       (lambda (worker text)
         (setq captured (concat captured text))
         (when (and (not response-seen) (string-match-p "\n" captured))
           (setq response-seen t)
           (run-at-time
            0 nil
            (lambda ()
              (setq message-during (e-board-message board "fact")
                    mutation-during
                    (condition-case err
                        (progn
                          (e-board-post-fact
                           board :id "dependent" :content "too early"
                           :source-fact-key '(producer 1 2))
                          'published)
                      (error (car err))))))
           (run-at-time
            0.02 nil
            (lambda ()
              (set-process-filter worker ordinary-filter)
              (funcall ordinary-filter worker captured))))))
      (e-board-post-fact
       board :id "fact" :content "committed"
       :source-fact-key '(producer 1 1))
      (should response-seen)
      (should-not message-during)
      (should (eq mutation-during 'e-board-mutation-frozen))
      (should (equal (e-board-message-content (e-board-message board "fact"))
                     "committed"))
      (should-not (e-board-message board "dependent"))
      (should (= (length (plist-get
                          (e-board-durable-record-page board 0 10 nil)
                          :records))
                 1))
      (should (eq (plist-get (e-board-durability-status board) :backend)
                  'sqlite)))))

(ert-deftest e-board-sqlite-s5-routing-and-pickups-publish-after-ack ()
  "Final routing and pickup identities remain invisible until their ACK."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let (drain)
      (let* ((board
              (e-board-create
               :id "routing" :storage storage :trusted-principal "owner"
               :register nil :input-classification-scheduler
               (lambda (function) (setq drain function))))
             (_participant
              (e-board-add-participant
               board :id "member" :create-pickup-subscription-id "address"))
             (publication
              (e-board-post-input
               board :id "input" :to "member" :content "route"
               :source-input-key '(producer 1 1)))
             (message (e-board-publication-message publication))
             (runtime (e-session-storage-runtime-store sessions))
             (process (e-runtime-store--process runtime))
             (ordinary-filter (process-filter process))
             (captured "") response-seen routing-during pickups-during)
        (set-process-filter
         process
         (lambda (worker text)
           (setq captured (concat captured text))
           (when (and (not response-seen) (string-match-p "\n" captured))
             (setq response-seen t)
             (run-at-time
              0 nil
              (lambda ()
                (setq routing-during (e-board-message-routing-state message)
                      pickups-during
                      (copy-tree (e-board-publication-pickup-ids publication)))))
             (run-at-time
              0.02 nil
              (lambda ()
                (set-process-filter worker ordinary-filter)
                (funcall ordinary-filter worker captured))))))
        (funcall drain)
        (should response-seen)
        (should (eq routing-during 'routing))
        (should-not pickups-during)
        (should (eq (e-board-message-routing-state message) 'routed))
        (should (= (length (e-board-publication-pickup-ids publication)) 1))
        (should (eq (e-board-pickup-state
                     (e-board-pickup
                      board (car (e-board-publication-pickup-ids publication))))
                    'ready))))))

(ert-deftest e-board-sqlite-s5-replay-progress-resumes-by-durable-position ()
  "Stable replay resumes after noisy history without repeating earlier work."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let* ((registry
            (e-board-registry-create
             :id "replay" :principal "owner" :storage storage))
           (board (e-board-registry-board-source-board registry))
           effects)
      (should (e-board-storage-backed-p board))
      (setf (e-board-effect-scheduler board)
            (lambda (effect) (push effect effects))
            (e-board-classification-authorizer board) nil)
      (e-board-registry-add-participant
       registry :id "member" :principal "owner" :subscription-id "address")
      (dotimes (index 39)
        (e-board-post-fact
         board :id (format "noise-%02d" index) :tags '(noise)
         :content index :source-fact-key (list 'noise 1 index)))
      (e-board-post-fact
       board :id "source" :tags '(selected) :content "source"
       :source-fact-key '(source 1 40))
      (should (= (length (plist-get
                          (e-board-durable-record-page
                           board 0 2 '(:kinds (fact) :tags-all (selected)))
                          :records))
                 1))
      (e-board-subscribe
       board "member" '(:kind fact :tags (selected)) :id "stable-replay"
       :effect '(:post-input :content "derived") :start-seq 0)
      ;; One bounded page advances to durable position 32 and schedules the
      ;; remainder; simulate process loss before that successor runs.
      (let ((limit 10) progress)
        (while (and effects (not progress) (> limit 0))
          (cl-decf limit)
          (funcall (pop effects))
          (setq progress
                (e-board-storage-replay-progress
                 storage "replay" (e-board-generation board) "stable-replay")))
        (should (> limit 0))
        (should (= (plist-get progress :position) 32)))
      (e-board-unregister board)
      (setq effects nil
            e-board-registry--boards (make-hash-table :test 'equal))
      (let* ((restored
              (e-board-sqlite-restore-registry-board
               (e-session-storage-runtime-store sessions) "replay"))
             (restored-board
              (e-board-registry-board-source-board restored)))
        (setf (e-board-effect-scheduler restored-board)
              (lambda (effect) (push effect effects))
              (e-board-classification-authorizer restored-board) nil)
        (e-board-registry-set-participant-state restored "member" 'active)
        (e-board-subscribe
         restored-board "member" '(:kind fact :tags (selected))
         :id "stable-replay" :effect '(:post-input :content "derived")
         :start-seq 0)
        (let ((limit 10))
          (while (and effects (> limit 0))
            (cl-decf limit)
            (funcall (pop effects)))
          (should (> limit 0)))
        (should (= (cl-count "derived" (e-board-messages restored-board)
                             :key #'e-board-message-content :test #'equal)
                   1))
        (should (= (plist-get
                    (e-board-storage-replay-progress
                     storage "replay" (e-board-generation restored-board)
                     "stable-replay")
                    :position)
                   40))))))

(ert-deftest e-board-sqlite-s5-replay-claims-position-before-effect ()
  "Durable replay exposes no dependent effect before its progress ACK."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let* ((board (e-board-sqlite-test--board storage "replay-claim"))
           (runtime (e-session-storage-runtime-store sessions))
           (process (e-runtime-store--process runtime))
           (ordinary-filter (process-filter process))
           (captured "") response-held message-during)
      (e-board-add-participant
       board :id "member" :create-pickup-subscription-id "address")
      (e-board-post-fact
       board :id "source" :tags '(selected) :content "source"
       :source-fact-key '(source 1 1))
      (setf (e-board-effect-scheduler board) (lambda (effect) (funcall effect))
            (e-board-classification-authorizer board) nil)
      (set-process-filter
       process
       (lambda (worker text)
         (let* ((request (e-runtime-store--active-request runtime))
                (body (and request
                           (e-runtime-store-request--body request))))
           (if (and (eq (plist-get body :op) 'board-replay-progress-put)
                    (not response-held))
               (progn
                 (setq captured (concat captured text))
                 (when (string-match-p "\n" captured)
                   (setq response-held t)
                   (run-at-time
                    0 nil
                    (lambda ()
                      (setq message-during
                            (cl-find "derived" (e-board-messages board)
                                     :key #'e-board-message-content
                                     :test #'equal))))
                   (run-at-time
                    0.02 nil
                    (lambda ()
                      (set-process-filter worker ordinary-filter)
                      (funcall ordinary-filter worker captured)))))
             (funcall ordinary-filter worker text)))))
      (e-board-subscribe
       board "member" '(:kind fact :tags (selected)) :id "claim-first"
       :effect '(:post-input :content "derived") :start-seq 0)
      (should response-held)
      (should-not message-during)
      (should (= (cl-count "derived" (e-board-messages board)
                           :key #'e-board-message-content :test #'equal)
                 1))
      (should (= (plist-get
                  (e-board-storage-replay-progress
                   storage "replay-claim" (e-board-generation board)
                   "claim-first")
                  :position)
                 1)))))

(ert-deftest e-board-sqlite-s5-publication-source-selectors-and-clear ()
  "Publication is canonical, source conflicts are explicit, and clear audits."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let* ((board (e-board-sqlite-test--board storage))
           (publication
            (e-board-post-fact
             board :id "fact" :author "producer" :tags '(red indexed)
             :attributes '(:scope durable) :content "one"
             :source-fact-key '(producer 1 1)))
           (generation (e-board-generation board)))
      (should (eq (e-board-publication-status publication) 'posted))
      (should (eq (e-board-publication-status
                   (e-board-post-fact
                    board :id "ignored" :author "producer"
                    :tags '(red indexed) :attributes '(:scope durable)
                    :content "one" :source-fact-key '(producer 1 1)))
                  'duplicate))
      (should-error
       (e-board-post-fact
        board :author "producer" :content "different"
        :source-fact-key '(producer 1 1))
       :type 'e-board-storage-conflict)
      (e-board-record-processing-chain
       board :id "chain" :root-message-id "fact"
       :candidate-message-id "fact" :processing-depth 0)
      (let ((page (e-board-durable-record-page
                   board 0 10 '(:kinds (fact) :tags-all (indexed)))))
        (should (= (length (plist-get page :records)) 1))
        (should (equal (plist-get
                        (plist-get (car (plist-get page :records)) :record)
                        :content)
                       "one")))
      (e-board-clear board)
      (should (= (e-board-generation board) (1+ generation)))
      (should-not (e-board-messages board))
      (should (= (length (plist-get
                          (e-board-durable-record-page
                           board 0 10 nil generation)
                          :records))
                 2)))))

(ert-deftest e-board-sqlite-s5-source-signature-map-order-survives-restart ()
  "Canonical source hashes ignore equal map insertion order across restart."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let ((left (make-hash-table :test 'equal))
          (right (make-hash-table :test 'equal)))
      (puthash "alpha" 'one left)
      (puthash "beta" 'two left)
      (puthash "beta" 'two right)
      (puthash "alpha" 'one right)
      (should (equal (e-board-storage-signature-hash left)
                     (e-board-storage-signature-hash right)))
      (let* ((registry
              (e-board-registry-create
               :id "map-source" :principal "owner" :storage storage))
             (board (e-board-registry-board-source-board registry)))
        (setf (e-board-input-classification-scheduler board)
              (lambda (drain) (funcall drain)))
        (e-board-post-fact
         board :id "fact" :content left :source-fact-key '(producer 1 1))
        (e-board-unregister board)
        (setq e-board-registry--boards (make-hash-table :test 'equal))
        (let* ((restored
                (e-board-sqlite-restore-registry-board
                 (e-session-storage-runtime-store sessions) "map-source"))
               (restored-board
                (e-board-registry-board-source-board restored)))
          (should
           (eq (e-board-publication-status
                (e-board-post-fact
                 restored-board :id "ignored" :content right
                 :source-fact-key '(producer 1 1)))
               'duplicate)))))))

(ert-deftest e-board-sqlite-s5-attribute-selector-grammar-is-normalized ()
  "Durable reads accept Board plist/alist attributes and reject malformed data."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let ((board (e-board-sqlite-test--board storage "selectors")))
      (e-board-post-fact
       board :id "fact" :attributes '(:scope durable :nested (a b))
       :content "selected" :source-fact-key '(producer 1 1))
      (dolist (attributes
               '((:scope durable :nested (a b))
                 ((:scope . durable) (:nested a b))))
        (should
         (= (length
             (plist-get
              (e-board-durable-record-page
               board 0 10 (list :kinds '(fact) :attributes attributes))
              :records))
            1)))
      (should-error
       (e-board-durable-record-page
        board 0 10 '(:kinds (fact) :attributes (:scope)))
       :type 'wrong-type-argument))))

(ert-deftest e-board-sqlite-s5-routing-fifo-reroute-and-pickup-lifecycle ()
  "Final routing, explicit reroute, FIFO, retry, and consumption are durable."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let ((board (e-board-sqlite-test--board storage)))
      (e-board-add-participant
       board :id "member" :create-pickup-subscription-id "address")
      (let* ((first (e-board-post-input
                     board :id "one" :to "member" :content "one"
                     :source-input-key '(client 1 1)))
             (second (e-board-post-input
                      board :id "two" :to "member" :content "two"
                      :source-input-key '(client 1 2)))
             (first-id (car (e-board-publication-pickup-ids first)))
             (second-id (car (e-board-publication-pickup-ids second))))
        (should (eq (e-board-pickup-state (e-board-pickup board first-id)) 'ready))
        (should (eq (e-board-pickup-state (e-board-pickup board second-id)) 'pending))
        (e-board-pickup-start-delivery board first-id)
        (e-board-pickup-return-ready board first-id 'uncommitted)
        (e-board-pickup-start-delivery board first-id)
        (should (equal (e-board-pickup-complete-delivery board first-id)
                       second-id))
        (should (eq (e-board-pickup-state (e-board-pickup board second-id)) 'ready))
        (let ((reroute
               (e-board-reroute-input
                board "one" :id "one-reroute"
                :source-input-key '(client 1 3))))
          (should (equal (e-board-message-id
                          (e-board-publication-message reroute))
                         "one-reroute")))
        (should-error
         (e-board-storage-commit-routing
          storage (e-board-id board) (e-board-generation board)
          "one" '(:state unrouted) nil)
         :type 'e-board-storage-conflict)))))

(ert-deftest e-board-sqlite-s5-no-recipient-outcome-is-final ()
  "A no-recipient routing outcome is durable and cannot be rerouted implicitly."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let* ((board (e-board-sqlite-test--board storage "no-recipient"))
           (publication
            (e-board-post-input
             board :id "input" :content "nobody"
             :source-input-key '(client 1 1)))
           (message (e-board-publication-message publication))
           (outcome
            (e-board-storage-routing
             storage (e-board-id board) (e-board-generation board) "input")))
      (should (eq (e-board-message-routing-state message) 'unrouted))
      (should (eq (e-board-message-unrouted-reason message)
                  'no-matching-subscription))
      (should (eq (plist-get (plist-get outcome :outcome) :state) 'unrouted))
      (should (eq (plist-get (plist-get outcome :outcome) :reason)
                  'no-matching-subscription))
      (should-error
       (e-board-storage-commit-routing
        storage (e-board-id board) (e-board-generation board)
        "input" '(:state routed) nil)
       :type 'e-board-storage-conflict))))

(ert-deftest e-board-sqlite-s5-restart-marks-ambiguous-fifo-head-uncertain ()
  "Restore makes the ambiguous head uncertain and its successor ready."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let* ((registry
            (e-board-registry-create
             :id "restart" :principal "owner" :storage storage))
           (board (e-board-registry-board-source-board registry)))
      (setf (e-board-input-classification-scheduler board)
            (lambda (drain) (funcall drain)))
      (e-board-registry-add-participant
       registry :id "member" :principal "owner"
       :subscription-id "address")
      (setf (e-board-classification-authorizer board) nil)
      (let* ((first
              (e-board-post-input
               board :id "first" :to "member" :content "once"
               :source-input-key '(client 1 1)))
             (second
              (e-board-post-input
               board :id "second" :to "member" :content "later"
               :source-input-key '(client 1 2)))
             (first-id (car (e-board-publication-pickup-ids first)))
             (second-id (car (e-board-publication-pickup-ids second))))
        (should (eq (e-board-pickup-state (e-board-pickup board second-id))
                    'pending))
        (e-board-pickup-start-delivery board first-id)
        (e-board-unregister board)
        (setq e-board-registry--boards (make-hash-table :test 'equal))
        (let* ((restored
                (e-board-sqlite-restore-registry-board
                 (e-session-storage-runtime-store sessions) "restart"))
               (restored-board
                (e-board-registry-board-source-board restored)))
          (should (eq (e-board-pickup-state
                       (e-board-pickup restored-board first-id))
                      'uncertain))
          (should (eq (e-board-pickup-state
                       (e-board-pickup restored-board second-id))
                      'ready))
          (should (equal
                   (mapcar (lambda (pickup) (plist-get pickup :delivery-id))
                           (e-board-storage-unresolved-pickups
                            storage "restart" 1 nil 10))
                   (list second-id)))
          (e-board-pickup-start-delivery restored-board second-id)
          (should (eq (e-board-pickup-state
                       (e-board-pickup restored-board second-id))
                      'delivering)))))))

(ert-deftest e-board-sqlite-s5-terminal-routing-retains-pickup-identities ()
  "Restart retains immutable pickup ids without hydrating terminal pickups."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let* ((registry
            (e-board-registry-create
             :id "terminal" :principal "owner" :storage storage))
           (board (e-board-registry-board-source-board registry)))
      (setf (e-board-input-classification-scheduler board)
            (lambda (drain) (funcall drain)))
      (e-board-registry-add-participant
       registry :id "member" :principal "owner" :subscription-id "address")
      (setf (e-board-classification-authorizer board) nil)
      (let* ((publication
              (e-board-post-input
               board :id "input" :to "member" :content "once"
               :source-input-key '(client 1 1)))
             (pickup-ids (copy-tree (e-board-publication-pickup-ids publication)))
             (delivery-id (car pickup-ids)))
        (e-board-pickup-start-delivery board delivery-id)
        (e-board-pickup-complete-delivery board delivery-id)
        (e-board-unregister board)
        (setq e-board-registry--boards (make-hash-table :test 'equal))
        (let* ((restored
                (e-board-sqlite-restore-registry-board
                 (e-session-storage-runtime-store sessions) "terminal"))
               (restored-board
                (e-board-registry-board-source-board restored))
               (message (e-board-message restored-board "input")))
          (should (equal (e-board-message-pickup-ids message) pickup-ids))
          (should-not (e-board-pickup restored-board delivery-id))
          (should-not
           (e-board-storage-unresolved-pickups
            storage "terminal" 1 nil 10)))))))

(ert-deftest e-board-sqlite-s5-aborted-participant-leaves-no-durable-ghost ()
  "Deferred participant abort removes durability and permits exact retry."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let* ((registry
            (e-board-registry-create
             :id "participant-abort" :principal "owner" :storage storage))
           (board (e-board-registry-board-source-board registry))
           (participant
            (e-board-registry-add-participant
             registry :id "member" :principal "owner"
             :subscription-id "address" :publish-event nil)))
      (should (= (length (e-board-storage-participants
                          storage "participant-abort" 1 10))
                 1))
      (e-board-registry-abort-participant-admission registry participant)
      (should-not
       (e-board-storage-participants storage "participant-abort" 1 10))
      (e-board-unregister board)
      (setq e-board-registry--boards (make-hash-table :test 'equal))
      (let* ((restored
              (e-board-sqlite-restore-registry-board
               (e-session-storage-runtime-store sessions) "participant-abort"))
             (restored-board
              (e-board-registry-board-source-board restored)))
        (should-not (e-board-participant restored-board "member"))
        (e-board-registry-add-participant
         restored :id "member" :principal "owner" :subscription-id "address")
        (should (= (length (e-board-storage-participants
                            storage "participant-abort" 1 10))
                   1))))))

(ert-deftest e-board-sqlite-s5-runtime-admission-failure-cleans-participant ()
  "A failed deferred runtime attachment removes its durable participant."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let ((e-board-runtime--attachments (make-hash-table :test 'equal))
          (e-board-runtime--session-attachments (make-hash-table :test 'equal))
          (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
          (e-board-runtime--invocations (make-hash-table :test 'equal))
          (e-board-runtime--admission-open-p t))
      (e-session-create sessions :id "session")
      (let* ((harness (e-harness-create :sessions sessions
                                        :enabled-layer-ids nil))
             (registry
              (e-board-registry-create
               :id "runtime-abort" :principal "owner" :storage storage))
             (source (e-board-registry-board-source-board registry)))
        (cl-letf (((symbol-function 'e-board-runtime--activate-attachment)
                   (lambda (_attachment)
                     (signal 'e-board-runtime-error
                             '("attachment activation rejected")))))
          (should-error
           (e-board-runtime-attach
            registry harness "session" :participant-id "member"
            :principal "owner" :controller "owner"
            :defer-participant-publication t)
           :type 'e-board-runtime-error))
        (should-not (e-board-participant source "member"))
        (should-not
         (e-board-storage-participants storage "runtime-abort" 1 10))
        (let ((attachment
               (e-board-runtime-attach
                registry harness "session" :participant-id "member"
                :principal "owner" :controller "owner"
                :defer-participant-publication t)))
          (should (e-board-runtime-attachment-p attachment))
          (should (= (length (e-board-storage-participants
                              storage "runtime-abort" 1 10))
                     1))
          (e-board-runtime-abort-new-attachment attachment)
          (should-not
           (e-board-storage-participants storage "runtime-abort" 1 10)))))))

(ert-deftest e-board-sqlite-s5-restart-cleans-provisional-participant ()
  "Restart removes a process-lost provisional participant for exact retry."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let* ((registry
            (e-board-registry-create
             :id "provisional" :principal "owner" :storage storage))
           (board (e-board-registry-board-source-board registry)))
      (e-board-registry-add-participant
       registry :id "member" :principal "owner"
       :subscription-id "address" :publish-event nil)
      (should (plist-get
               (car (e-board-storage-participants
                     storage "provisional" 1 10))
               :publication-pending))
      ;; Simulate process loss before the enclosing admission publishes.
      (e-board-unregister board)
      (setq e-board-registry--boards (make-hash-table :test 'equal))
      (let* ((restored
              (e-board-sqlite-restore-registry-board
               (e-session-storage-runtime-store sessions) "provisional"))
             (restored-board
              (e-board-registry-board-source-board restored)))
        (should-not (e-board-participant restored-board "member"))
        (should-not
         (e-board-storage-participants storage "provisional" 1 10))
        (e-board-registry-add-participant
         restored :id "member" :principal "owner"
         :subscription-id "address")
        (should (= (length (e-board-storage-participants
                            storage "provisional" 1 10))
                   1))))))

(ert-deftest e-board-sqlite-s5-missing-board-never-falls-back-to-session-log ()
  "A SQLite session association surfaces its missing Board root."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (let ((session
           (list :id "session"
                 :board-session-state
                 '(:board-id "missing-board" :principal "owner"))))
      (should-error
       (e-board-session-association-restore sessions session)
       :type 'e-board-storage-error))))

(ert-deftest e-board-sqlite-s6-composite-publishes-after-ack-once ()
  "The composite publishes neither owner before its transaction ACK."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (e-session-create sessions :id "session")
    (let ((board (e-board-sqlite-test--board storage)))
      (e-board-add-participant
       board :id "member" :create-pickup-subscription-id "address")
      (let* ((publication
              (e-board-post-input
               board :id "input" :to "member" :content "deliver"
               :source-input-key '(client 1 1)))
             (delivery-id (car (e-board-publication-pickup-ids publication)))
             (runtime (e-session-storage-runtime-store sessions))
             (process (e-runtime-store--process runtime))
             (ordinary-filter (process-filter process))
             (captured "") response-seen state-during events-during
             session-mutation-during board-mutation-during
             session-callback-ran board-callback-ran callback-error)
        (e-board-pickup-start-delivery board delivery-id)
        (set-process-filter
         process
         (lambda (worker text)
           (setq captured (concat captured text))
           (when (and (not response-seen) (string-match-p "\n" captured))
             (setq response-seen t)
             (run-at-time
              0 nil
              (lambda ()
                (setq state-during
                      (e-board-pickup-state
                       (e-board-pickup board delivery-id))
                      events-during
                      (condition-case nil
                          (e-session-activity-events sessions "session")
                        (e-session-persistence-unavailable :frozen))
                      session-mutation-during
                      (condition-case err
                          (progn
                            (e-session-append-activity-event
                             sessions "session" "too-early" 'dependent nil
                             :write-index nil)
                            :published)
                        (error (car err)))
                      board-mutation-during
                      (condition-case err
                          (progn
                            (e-board-post-fact
                             board :id "too-early" :content "blocked"
                             :source-fact-key '(dependent 1 0))
                            :published)
                        (error (car err))))
                ;; These already-admitted Board-owned callbacks must wait for
                ;; both owner publications, even when one mutates the session.
                (e-board--defer-after-storage-barrier
                 board
                 (lambda ()
                   (condition-case err
                       (progn
                         (should
                          (eq (e-board-pickup-state
                               (e-board-pickup board delivery-id))
                              'accepted))
                         (should
                          (= (length
                              (e-session-activity-events sessions "session"))
                             1))
                         (e-session-append-activity-event
                          sessions "session" "after-ack" 'dependent nil
                          :write-index nil)
                         (setq session-callback-ran t))
                     (error (setq callback-error err)))))
                (e-board--defer-after-storage-barrier
                 board
                 (lambda ()
                   (condition-case err
                       (progn
                         (should
                          (eq (e-board-pickup-state
                               (e-board-pickup board delivery-id))
                              'accepted))
                         (e-board-post-fact
                          board :id "after-ack" :content "published"
                          :source-fact-key '(dependent 1 1))
                         (setq board-callback-ran t))
                     (error (setq callback-error err)))))))
             (run-at-time
              0.02 nil
              (lambda ()
                (set-process-filter worker ordinary-filter)
                (funcall ordinary-filter worker captured))))))
        (e-board-pickup-admission-commit
         board delivery-id sessions "session" 'idle)
        (should response-seen)
        (should (eq state-during 'delivering))
        (should (eq events-during :frozen))
        (should (eq session-mutation-during 'e-session-persistence-unavailable))
        (should (eq board-mutation-during 'e-board-mutation-frozen))
        (should (eq (e-board-pickup-state
                     (e-board-pickup board delivery-id))
                    'accepted))
        (let ((deadline (+ (float-time) 5.0)))
          (while (and (not (and session-callback-ran board-callback-ran))
                      (not callback-error)
                      (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (when callback-error
          (signal (car callback-error) (cdr callback-error)))
        (should session-callback-ran)
        (should board-callback-ran)
        (should (= (length (e-session-activity-events sessions "session")) 2))
        (should (equal (e-board-message-content
                        (e-board-message board "after-ack"))
                       "published"))
        (should-not (e-board-message board "too-early"))
        (should-error
         (e-board-storage-admit-pickup
          storage (e-board-id board) (e-board-generation board) delivery-id
          "session" '(:different-record t) 'idle)
         :type 'e-board-storage-conflict)
        (e-session-sqlite-store-close sessions)
        (setq sessions (e-session-sqlite-store-create directory))
        (should (= (length (e-session-activity-events sessions "session")) 2))))))

(ert-deftest e-board-sqlite-s6-composite-invalid-state-has-no-tear ()
  "A pickup that is not claimed publishes neither side of the composite."
  (e-board-sqlite-test--with-store (sessions storage directory)
    (e-session-create sessions :id "session")
    (let ((board (e-board-sqlite-test--board storage)))
      (e-board-add-participant
       board :id "member" :create-pickup-subscription-id "address")
      (let* ((publication
              (e-board-post-input
               board :id "input" :to "member" :content "deliver"
               :source-input-key '(client 1 1)))
             (delivery-id (car (e-board-publication-pickup-ids publication)))
             (pickup (e-board-pickup board delivery-id))
             (admission
              (e-session-board-input-admission-prepare
               sessions "session" delivery-id 'idle
               (e-board-pickup-content pickup))))
        (should-error
         (e-board-storage-admit-pickup
          storage (e-board-id board) (e-board-generation board) delivery-id
          "session" (e-session-board-input-admission-record admission) 'idle)
         :type 'e-board-storage-conflict)
        (should (eq (e-board-pickup-state pickup) 'ready))
        (should-not (e-session-activity-events sessions "session"))))))

(provide 'e-board-sqlite-test)

;;; e-board-sqlite-test.el ends here
