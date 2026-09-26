;;; e-chat-continuation-outcome-test.el --- Continuation outcome adapter tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board-orchestration)
(require 'e-chat-service)
(require 'e-work)
(load (expand-file-name "e-board-producer-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defconst e-chat-continuation-outcome-test--spec
  (e-work-spec-create
   :id "chat-continuation-outcome-test"
   :execution 'cheap
   :interactive-policy 'cheap
   :owner 'e-chat-continuation-outcome-test
   :runner (lambda (arguments _context) arguments)))

(defconst e-chat-continuation-outcome-test--deferred-spec
  (e-work-spec-create
   :id "chat-continuation-outcome-deferred-test"
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'e-chat-continuation-outcome-test
   :runner (lambda (_handle _arguments _context) :deferred)))

(defun e-chat-continuation-outcome-test--finished-work (&optional value)
  "Return a settled Work carrying VALUE."
  (e-work-start e-chat-continuation-outcome-test--spec value))

(defun e-chat-continuation-outcome-test--pending-work ()
  "Return an unsettled Work used to exercise async publication settlement."
  (e-work-start e-chat-continuation-outcome-test--deferred-spec nil))

(defun e-chat-continuation-outcome-test--sql-binding
    (harness service board-id &optional subscribers)
  "Return a live detached SQL binding for disposable Board BOARD-ID."
  (e-chat-service--binding-create
   :harness harness
   :session-id "session-1"
   :board-id board-id
   :participant-id "participant-1"
   :participant-name "Participant One"
   :sqlite-service service
   :lifecycle-state 'active
   :subscribers subscribers
   :executing-turns (make-hash-table :test 'equal)
   :continuation-deliveries (make-hash-table :test 'equal)
   :continuation-turns (make-hash-table :test 'equal)
   :continuation-outcome-inflight (make-hash-table :test 'equal)))

(defun e-chat-continuation-outcome-test--binding (&optional turns)
  "Return a detached binding fixture with continuation TURN correlation."
  (e-chat-service--binding-create
   :harness 'harness
   :session-id "coordinator"
   :board-id "board-1"
   :participant-id "participant-1"
   :sqlite-service 'sqlite-service
   :turn-port 'port
   :lifecycle-state 'active
   :subscribers nil
   :executing-turns (make-hash-table :test 'equal)
   :continuation-deliveries (make-hash-table :test 'equal)
   :continuation-turns (or turns (make-hash-table :test 'equal))
   :continuation-outcome-inflight (make-hash-table :test 'equal)))

(ert-deftest e-chat-continuation-outcome-test-publication-uses-stable-fact-key ()
  "The terminal adapter publishes one generic outcome fact identity."
  (let* ((binding (e-chat-continuation-outcome-test--binding))
         (context '(:run-id "run-1" :publication-key "continue-1"
                    :board-run-generation 1
                    :turn-id "turn-1"))
         fact generation)
    (cl-letf (((symbol-function 'e-board-sqlite-service-orchestration-fact-start)
               (lambda (_service _board-id value &rest arguments)
                 (setq fact (copy-tree value t))
                 (setq generation (plist-get arguments :generation))
                 (e-chat-continuation-outcome-test--finished-work
                  '(:status posted)))))
      (let ((work (e-chat-service--publish-sqlite-continuation-outcome
                   binding context 'done)))
        (should (e-work-handle-p work))
        (should (eq (plist-get (plist-get fact :payload) :status) 'done))
        (should (equal (plist-get fact :idempotency-key)
                       (e-board-orchestration-continuation-outcome-key
                        "run-1" "continue-1")))
        (should (equal (plist-get (plist-get fact :payload) :turn-id)
                       "turn-1"))
        (should (= generation 1))))))

(ert-deftest e-chat-continuation-outcome-test-terminal-event-publishes-once ()
  "A duplicate terminal callback cannot publish a second process-local fact."
  (let* ((turns (make-hash-table :test 'equal))
         (binding (e-chat-continuation-outcome-test--binding turns))
         (calls nil))
    (puthash "turn-1"
             '(:run-id "run-1" :publication-key "continue-1"
               :board-run-generation 1
               :turn-id "turn-1")
             turns)
    (cl-letf (((symbol-function 'e-chat-service--publish-sqlite-continuation-outcome)
               (lambda (_binding context status &optional error)
                 (push (list context status error) calls)
                 (e-chat-continuation-outcome-test--finished-work
                  '(:status posted))))
              ((symbol-function 'e-harness-attached-turn-port-assistant-message)
               (lambda (_port _turn-id)
                 '(:content "coordinator output")))
              ((symbol-function 'e-board-sqlite-service-record-append-start)
               (lambda (&rest _arguments)
                 (e-chat-continuation-outcome-test--finished-work nil)))
              ((symbol-function 'e-board-sqlite-service-notify-delivery-outcome)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'e-chat-service--sql-notify-event)
               (lambda (&rest _arguments) nil)))
      (let ((event '(:type turn-finished :turn-id "turn-1"
                     :payload (:reason completed))))
        (e-chat-service--sql-harness-event binding event)
        (e-chat-service--sql-harness-event binding event))
      (should (= (length calls) 1))
      (should (eq (cadar calls) 'done))
      (should-not (gethash "turn-1" turns)))))

(ert-deftest e-chat-continuation-outcome-test-sync-publication-failure-retains-correlation ()
  "A synchronous publication error keeps the turn available for retry."
  (let* ((turns (make-hash-table :test 'equal))
         (binding (e-chat-continuation-outcome-test--binding turns))
         (attempts 0)
         (failures nil))
    (puthash "turn-1"
             '(:run-id "run-1" :publication-key "continue-1"
               :board-run-generation 1
               :turn-id "turn-1")
             turns)
    (cl-letf (((symbol-function 'e-chat-service--publish-sqlite-continuation-outcome)
               (lambda (&rest _arguments)
                 (cl-incf attempts)
                 (if (= attempts 1)
                     (error "synchronous publication failed")
                   (e-chat-continuation-outcome-test--finished-work
                    '(:status posted)))))
              ((symbol-function 'e-chat-service--sql-note-failure)
               (lambda (_binding error &optional _owner-suspect-p)
                 (push error failures)))
              ((symbol-function 'e-harness-attached-turn-port-assistant-message)
               (lambda (&rest _arguments) '(:content "output")))
              ((symbol-function 'e-board-sqlite-service-record-append-start)
               (lambda (&rest _arguments)
                 (e-chat-continuation-outcome-test--finished-work nil)))
              ((symbol-function 'e-chat-service--sql-notify-event)
               (lambda (&rest _arguments) nil)))
      (let ((event '(:type turn-finished :turn-id "turn-1"
                     :payload (:reason completed))))
        (e-chat-service--sql-harness-event binding event)
        (should (= attempts 1))
        (should (gethash "turn-1" turns))
        (e-chat-service--sql-harness-event binding event)
        (should (= attempts 2))
        (should-not (gethash "turn-1" turns))
        (should failures)))))

(ert-deftest e-chat-continuation-outcome-test-async-publication-failure-retains-and-retries ()
  "An async publication failure retains correlation until a later success."
  (let* ((turns (make-hash-table :test 'equal))
         (binding (e-chat-continuation-outcome-test--binding turns))
         (attempts 0)
         (pending nil)
         (retry nil)
         (failures nil))
    (puthash "turn-1"
             '(:run-id "run-1" :publication-key "continue-1"
               :board-run-generation 1
               :turn-id "turn-1")
             turns)
    (cl-letf (((symbol-function 'e-chat-service--publish-sqlite-continuation-outcome)
               (lambda (&rest _arguments)
                 (cl-incf attempts)
                 (if (= attempts 1)
                     (setq pending
                           (e-chat-continuation-outcome-test--pending-work))
                   (setq retry
                         (e-chat-continuation-outcome-test--pending-work)))))
              ((symbol-function 'e-chat-service--sql-note-failure)
               (lambda (_binding error &optional _owner-suspect-p)
                 (push error failures)))
              ((symbol-function 'e-harness-attached-turn-port-assistant-message)
               (lambda (&rest _arguments) '(:content "output")))
              ((symbol-function 'e-board-sqlite-service-record-append-start)
               (lambda (&rest _arguments)
                 (e-chat-continuation-outcome-test--finished-work nil)))
              ((symbol-function 'e-chat-service--sql-notify-event)
               (lambda (&rest _arguments) nil)))
      (let ((event '(:type turn-finished :turn-id "turn-1"
                     :payload (:reason completed))))
        (e-chat-service--sql-harness-event binding event)
        (e-chat-service--sql-harness-event binding event)
        (should (= attempts 1))
        (should (eq (gethash "turn-1"
                             (e-chat-service-binding-continuation-outcome-inflight
                              binding))
                    pending))
        (should (gethash "turn-1" turns))
        (e-work-fail pending '(e-chat-service-error "async publication failed"))
        (should-not
         (gethash "turn-1"
                  (e-chat-service-binding-continuation-outcome-inflight binding)))
        (should (gethash "turn-1" turns))
        (e-chat-service--sql-harness-event binding event)
        (should (= attempts 2))
        (should (eq (gethash "turn-1"
                             (e-chat-service-binding-continuation-outcome-inflight
                              binding))
                    retry))
        (e-work-finish retry '(:status posted))
        (should-not (gethash "turn-1" turns))
        (should failures)))))

(ert-deftest e-chat-continuation-outcome-test-synchronous-terminal-failure-correlates-submit ()
  "A terminal admission failure before submit returns still gets persisted."
  (let* ((binding (e-chat-continuation-outcome-test--binding))
         (delivery '("board-1" "message-1" "participant-1"))
         (calls nil))
    (puthash delivery
             '(:run-id "run-1" :publication-key "continue-1"
               :board-run-generation 1)
             (e-chat-service-binding-continuation-deliveries binding))
    (puthash delivery 'submitting
             (e-chat-service-binding-executing-turns binding))
    (cl-letf (((symbol-function 'e-chat-service--publish-sqlite-continuation-outcome)
               (lambda (_binding context status &optional error)
                 (push (list context status error) calls)
                 (e-chat-continuation-outcome-test--finished-work
                  '(:status posted))))
              ((symbol-function 'e-board-sqlite-service-notify-delivery-outcome)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'e-chat-service--sql-notify-event)
               (lambda (&rest _arguments) nil)))
      (e-chat-service--sql-harness-event
       binding
       '(:type turn-failed :turn-id "turn-1" :payload (:error "admission")))
      (should (= (length calls) 1))
      (should (eq (cadar calls) 'failed))
      (should-not
       (gethash delivery
                (e-chat-service-binding-continuation-deliveries binding))))))

(ert-deftest e-chat-continuation-outcome-test-outcome-precedes-output-publication-failure ()
  "A failed ancillary output row cannot suppress the terminal outcome."
  (let* ((turns (make-hash-table :test 'equal))
         (binding (e-chat-continuation-outcome-test--binding turns))
         (calls nil))
    (puthash "turn-1"
             '(:run-id "run-1" :publication-key "continue-1"
               :board-run-generation 1
               :turn-id "turn-1")
             turns)
    (cl-letf (((symbol-function 'e-chat-service--publish-sqlite-continuation-outcome)
               (lambda (&rest _arguments)
                 (push t calls)
                 (e-chat-continuation-outcome-test--finished-work
                  '(:status posted))))
              ((symbol-function 'e-harness-attached-turn-port-assistant-message)
               (lambda (&rest _arguments) '(:content "output")))
              ((symbol-function 'e-board-sqlite-service-record-append-start)
               (lambda (&rest _arguments)
                 (error "output publication failed"))))
      (should-error
       (e-chat-service--sql-harness-event
        binding '(:type turn-finished :turn-id "turn-1" :payload nil)))
      (should (= (length calls) 1)))))

(ert-deftest e-chat-continuation-outcome-test-reasoning-summary-board-projection-is-idempotent-and-private-safe ()
  "Only a committed summary becomes one bounded, private-safe Board row."
  (e-board-producer-test-with-target (target service board-id _runtime)
    (let* ((store (e-session-store-create))
           (harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions store))
           (notifications nil)
           (subscription
            (e-chat-service--subscription-create
             :function (lambda (event) (push event notifications))
             :active-p t :state 'active :lifecycle-generation 0))
           (binding
            (e-chat-continuation-outcome-test--sql-binding
             harness service board-id (list subscription)))
           (preview-source
            (concat "  first readable line  \n\n"
                    "second readable line\r\n"
                    "third readable line\n"
                    (make-string 800 ?λ)))
           (committed-event
            (list :type 'reasoning-snapshot-committed
                  :session-id "session-1" :turn-id "turn-1"
                  :activity-entry-id "activity-42"
                  :payload
                  (list :stream-kind 'summary :content preview-source
                        :content-mode 'snapshot :combined t
                        :provider-request-id "provider-secret"
                        :raw-content "raw-secret"
                        :encrypted-content "encrypted-secret"
                        :tool-arguments "tool-arguments-secret"
                        :tool-result "tool-result-secret"
                        :transcript "transcript-secret"
                        :endpoint-token "endpoint-secret")))
           (live-event
            '(:type reasoning-delta :session-id "session-1" :turn-id "turn-1"
              :payload (:stream-kind summary :content "live fragment"))))
      (e-harness-create-session harness :id "session-1")
      ;; A live summary is selected-chat presentation only.  It must not create
      ;; a Board row before the detached post-append event arrives.
      (e-chat-service--sql-harness-event binding live-event)
      (should (= (length notifications) 1))
      (should-not (e-board-producer-test-records target))
      (let ((first (e-chat-service--sql-harness-event binding committed-event)))
        (should (e-work-handle-p first))
        (e-board-producer-test-await first))
      ;; Replayed settlement uses the same session/activity source key and is
      ;; therefore a canonical duplicate rather than a second Board activity.
      (let ((duplicate (e-chat-service--sql-harness-event binding committed-event)))
        (should (e-work-handle-p duplicate))
        (e-board-producer-test-await duplicate))
      (should (= (length notifications) 1))
      (let* ((records (e-board-producer-test-records target))
             (summary-records
              (seq-filter
               (lambda (record)
                 (eq (plist-get record :activity-kind) 'reasoning-summary))
               records))
             (record (car summary-records))
             (content (plist-get record :content))
             (lines (and content (split-string content "\n" nil))))
        (should (= (length summary-records) 1))
        (should (eq (plist-get record :record-kind) 'activity))
        (should (eq (plist-get record :activity-kind) 'reasoning-summary))
        (should (equal (plist-get record :author) "participant:participant-1"))
        (should (equal (plist-get record :subject-participant-id)
                       "participant-1"))
        (should (equal (plist-get record :source-turn-id) "turn-1"))
        (should (equal (plist-get record :attributes) '(:detail-version 1)))
        (should (equal (seq-take lines 2)
                       '("first readable line" "second readable line")))
        (should (<= (length lines) 3))
        (should (cl-every (lambda (line) (not (string-empty-p line))) lines))
        (should (<= (string-bytes content) 1024))
        (should-not
         (seq-some
          (lambda (secret)
            (string-match-p (regexp-quote secret)
                            (prin1-to-string record)))
          '("provider-secret" "raw-secret" "encrypted-secret"
            "tool-arguments-secret" "tool-result-secret"
            "transcript-secret" "endpoint-secret")))))))

(ert-deftest e-chat-continuation-outcome-test-reasoning-summary-board-failure-is-reported ()
  "A failed Board summary append uses the owner-local persistence report."
  (e-board-producer-test-with-target (_target service _board-id _runtime)
    (let* ((store (e-session-store-create))
           (harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions store))
           (failures nil)
           (binding
            (e-chat-continuation-outcome-test--sql-binding
             harness service "missing-board")))
      (e-harness-create-session harness :id "session-1")
      (cl-letf (((symbol-function 'e-chat-service--sql-note-failure)
                 (lambda (_binding error &optional owner-suspect-p)
                   (push (list error owner-suspect-p) failures))))
        (let ((work
               (e-chat-service--sql-harness-event
                binding
                '(:type reasoning-snapshot-committed
                  :session-id "session-1" :turn-id "turn-1"
                  :activity-entry-id "activity-missing"
                  :payload (:stream-kind summary :content "will fail")))))
          (should (e-work-handle-p work))
          (condition-case _error
              (e-board-producer-test-await work)
            (error nil))))
      (should (= (length failures) 1))
      (should (cadar failures))
      (should-not (e-board-producer-test-records _target)))))

(ert-deftest e-chat-continuation-outcome-test-reasoning-summary-source-boundaries ()
  "The post-append bridge keeps harness and provider dependencies separated."
  (let ((root (if (fboundp 'e-source-directory)
                  (e-source-directory)
                default-directory)))
    (let ((harness-source
           (with-temp-buffer
             (insert-file-contents
              (expand-file-name "lisp/core/e-harness-activity.el" root))
             (buffer-string)))
          (chat-source
           (with-temp-buffer
             (insert-file-contents
              (expand-file-name "lisp/core/e-chat-service.el" root))
             (buffer-string))))
      (should-not (string-match-p "\\_<e-board-" harness-source))
      (dolist (provider-symbol '("e-openai" "e-anthropic"
                                 "e-openai-decoder" "e-anthropic-parse-stream"))
        (should-not (string-match-p
                     (concat "\\_<" (regexp-quote provider-symbol) "\\_>")
                     chat-source))))))

(provide 'e-chat-continuation-outcome-test)

;;; e-chat-continuation-outcome-test.el ends here
