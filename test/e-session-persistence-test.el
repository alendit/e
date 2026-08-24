;;; e-session-persistence-test.el --- Tests for async session persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-session)
(require 'e-session-persistence)

(ert-deftest e-session-persistence-test-bundled-writer-script-exists ()
  "The packaged runtime includes the writer beside its owning Lisp module."
  (should (file-readable-p (e-session-persistence--writer-script))))

(ert-deftest e-session-persistence-test-writer-script-ignores-current-buffer ()
  "Writer discovery remains anchored to its library from unrelated buffers."
  (let* ((expected (e-session-persistence--writer-script))
         (buffer-file-name "/tmp/unrelated-project/daily.org")
         (default-directory "/tmp/unrelated-project/"))
    (should (equal (e-session-persistence--writer-script) expected))))

(defun e-session-persistence-test--await-durable (store)
  "Wait in this test process for STORE's asynchronous durability boundary."
  (let ((deadline (+ (float-time) 5.0)) done failure)
    (e-session-finalize
     store (lambda (_value) (setq done t)) (lambda (err) (setq failure err)))
    (while (and (not done) (not failure) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should-not failure)
    (should done)))

(ert-deftest e-session-persistence-test-unsettled-transfer-has-no-false-zero ()
  "Checkpoint timer ownership transfers to the writer outbox atomically."
  (let* ((directory (make-temp-file "e-session-persistence-count-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         callback
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq callback (lambda () (apply function arguments)))
                     (timer-create)))
                  ((symbol-function 'e-session-persistence--ensure)
                   (lambda (_controller) t))
                  ((symbol-function 'e-session-persistence--send)
                   (lambda (_controller request) (push request sent))))
          (e-session-persistence-request-checkpoint controller)
          (should (equal (e-session-persistence-unsettled-state)
                         '(:generation 1 :writes 1)))
          (funcall callback)
          ;; The batch slot remains live alongside the current writer command.
          (should (= (plist-get (e-session-persistence-unsettled-state) :writes)
                     2))
          (let ((request (e-session-persistence-command-request (car sent))))
            (should (equal (plist-get request :op) "reindex"))
            (e-session-persistence--handle-response
             controller (list :id (plist-get request :id) :ok t)))
          (should (= (plist-get (e-session-persistence-unsettled-state) :writes)
                     0)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-prefix-barrier-is-session-scoped ()
  "A controlled writer settles only the named session and record prefix."
  (let* ((directory (make-temp-file "e-session-prefix-barrier-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller nil)
         (sent nil)
         (done nil)
         (failure nil)
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0))
    (unwind-protect
        (progn
          ;; Create both dirty sessions before attaching the controlled writer;
          ;; only their later outbox appends participate in this barrier.
          (e-session-create store :id "session-a")
          (e-session-create store :id "session-b")
          (setq controller (e-session-persistence-enable store))
          (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
                     (lambda (_controller command)
                       (push command sent))))
            (let* ((a1 (e-session-persistence-submit-record
                        controller "session-a"
                        '(:type "context-frame" :id "a-1")))
                   (a2 (e-session-persistence-submit-record
                        controller "session-a"
                        '(:type "context-frame" :id "a-2")))
                   (b1 (e-session-persistence-submit-record
                        controller "session-b"
                        '(:type "context-frame" :id "b-1"))))
              (e-session-persistence-await-record-prefix
               controller "session-a" '("a-1")
               (lambda (value) (setq done value))
               (lambda (error) (setq failure error)))
              (should-not done)
              ;; Another session's terminal failure is not this barrier's
              ;; failure, and a same-session record outside the named prefix is
              ;; not sufficient for success either.
              (e-session-persistence--handle-response
               controller (list :id b1 :ok :json-false :retryable :json-false))
              (should-not done)
              (should-not failure)
              (e-session-persistence--handle-response
               controller (list :id a2 :ok t :result 'written))
              (should-not done)
              (should-not failure)
              (e-session-persistence--handle-response
               controller (list :id a1 :ok t :result 'written))
              (should (equal (plist-get done :session-id) "session-a"))
              (should (equal (plist-get done :entry-ids) '("a-1")))
              (should (= (plist-get done :pending-count) 0))
              (should-not failure)
              (should-not
               (seq-some
                (lambda (command)
                  (member (plist-get
                           (e-session-persistence-command-request command) :op)
                          '("checkpoint" "reindex")))
                sent))
              ;; A fresh unrelated failure still cannot poison a barrier for
              ;; session-a; failure of its named record does.
              (setq done nil failure nil)
              (let ((a3 (e-session-persistence-submit-record
                          controller "session-a"
                          '(:type "context-frame" :id "a-3")))
                    (b2 (e-session-persistence-submit-record
                         controller "session-b"
                         '(:type "context-frame" :id "b-2"))))
                (e-session-persistence-await-record-prefix
                 controller "session-a" '("a-3")
                 (lambda (value) (setq done value))
                 (lambda (error) (setq failure error)))
                (e-session-persistence--handle-response
                 controller
                 (list :id b2 :ok :json-false :retryable :json-false))
                (should-not done)
                (should-not failure)
                (e-session-persistence--handle-response
                 controller
                 (list :id a3 :ok :json-false :retryable :json-false))
                (should-not done)
                (should failure)))))
      (when-let ((process (and controller
                              (e-session-persistence-process controller))))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-prefix-barrier-validates-entry-lifecycle ()
  "Unknown, acknowledged, and late-failed named entries are distinguished."
  (let* ((directory (make-temp-file "e-session-prefix-entry-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller nil)
         (sent nil)
         (done nil)
         (failure nil))
    (unwind-protect
        (progn
          (e-session-create store :id "session-a")
          (e-session-create store :id "session-b")
          (setq controller (e-session-persistence-enable store))
          (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
                     (lambda (_controller command)
                       (push command sent)))
                    ;; Appending the test entries must not schedule unrelated
                    ;; checkpoint commands; the barrier under test is only for
                    ;; the named entry ids.
                    ((symbol-function 'e-session-persistence-request-checkpoint)
                     (lambda (&rest _arguments) nil)))
            (let* ((a1 (e-session-append-message
                        store "session-a"
                        '(:role user :content "a-1")))
                   (b1 (e-session-append-message
                        store "session-b"
                        '(:role user :content "b-1")))
                   (a1-id (plist-get a1 :id))
                   (b1-id (plist-get b1 :id)))
              (should (eq
                       (e-session-persistence--record-state
                        controller "session-a" a1-id)
                       'submitted))
              (should-error
               (e-session-context-lifetime-durability-barrier
                store "session-a" '("missing-entry")
                #'ignore #'ignore)
               :type 'e-session-error)
              ;; The entry is known to the session and its writer command has
              ;; already been acknowledged, so a barrier attached afterwards
              ;; succeeds without an outbox mapping.
              ;; Use the command indexed by A1 rather than relying on the
              ;; controlled writer's newest-first send capture order.
              (let ((a1-command
                     (gethash
                      (list "session-a" a1-id)
                      (e-session-persistence-record-command-ids controller))))
                (e-session-persistence--handle-response
                 controller (list :id a1-command :ok t :result 'written))
                ;; The mapping is removed by the acknowledgement.
                (should-not
                 (gethash
                  (list "session-a" a1-id)
                  (e-session-persistence-record-command-ids controller))))
              (should (eq
                       (e-session-persistence--record-state
                        controller "session-a" a1-id)
                       'acknowledged))
              (setq done nil failure nil)
              (e-session-context-lifetime-durability-barrier
               store "session-a" (list a1-id)
               (lambda (value) (setq done value))
               (lambda (error) (setq failure error)))
              (should done)
              (should-not failure)
              ;; B1 remains pending until it is rejected.  Once rejected, a
              ;; barrier attached after the mapping was removed still fails
              ;; from authoritative terminal-failure state.
              (let ((b1-command
                     (gethash
                      (list "session-b" b1-id)
                      (e-session-persistence-record-command-ids controller))))
                (e-session-persistence--handle-response
                 controller
                 (list :id b1-command
                       :ok :json-false :retryable :json-false)))
              (should (eq
                       (e-session-persistence--record-state
                        controller "session-b" b1-id)
                       'terminal-failed))
              (setq done nil failure nil)
              (e-session-context-lifetime-durability-barrier
               store "session-b" (list b1-id)
               (lambda (value) (setq done value))
               (lambda (error) (setq failure error)))
              (should-not done)
              (should (equal (car failure) 'e-session-persistence-error)))))
      (when-let ((process (and controller
                              (e-session-persistence-process controller))))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-prefix-barrier-preflight-failure-is-not-durable ()
  "A command rejected before outbox admission remains a visible failure."
  (let* ((directory (make-temp-file "e-session-prefix-preflight-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller nil)
         (done nil)
         (failure nil))
    (unwind-protect
        (progn
          ;; Keep the session root out of the controlled writer lifecycle so
          ;; the only state under test is the rejected message append.
          (e-session-create store :id "session-preflight")
          (setq controller (e-session-persistence-enable store))
          (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
                     (lambda (&rest _arguments) nil))
                    ((symbol-function 'e-session-persistence-request-checkpoint)
                     (lambda (&rest _arguments) nil)))
            (let ((e-session-persistence-command-node-limit 8))
              (should-error
               (e-session-append-message
                store "session-preflight"
                '(:id "preflight-entry"
                  :role user
                  :content "This record must fail before outbox admission."))
               :type 'e-session-persistence-command-error))
            ;; Mutation remains in the live session, but the persistence owner
            ;; records why it cannot cross the later context barrier.
            (should (e-session-entry-by-id store "session-preflight"
                                            "preflight-entry"))
            (should (eq
                     (e-session-persistence--record-state
                      controller "session-preflight" "preflight-entry")
                     'preflight-failed))
            (should (= (hash-table-count
                        (e-session-persistence-outbox controller))
                       0))
            (e-session-context-lifetime-durability-barrier
             store "session-preflight" '("preflight-entry")
             (lambda (value) (setq done value))
             (lambda (error) (setq failure error)))
            (should-not done)
            (should (equal (car failure) 'e-session-persistence-error))))
      (when-let ((process (and controller
                              (e-session-persistence-process controller))))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-prefix-barrier-preflight-replacement-fences-success-predecessor ()
  "A rejected replacement fences a predecessor success from logical state."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "preflight-fences-success"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (predecessor-done 0)
         (predecessor-error 0)
         (first-failure nil)
         (second-failure nil)
         (first-count 0)
         (second-count 0)
         (key (list "session-replacement" "same-key")))
    (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'display-warning)
               (lambda (&rest _arguments) nil)))
      (let* ((first
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "predecessor")
               (lambda (&rest _value)
                 (setq predecessor-done (1+ predecessor-done)))
               (lambda (&rest _error)
                 (setq predecessor-error (1+ predecessor-error))))))
        (e-session-persistence-await-record-prefix
         controller "session-replacement" '("same-key")
         (lambda (_value) nil)
         (lambda (error)
           (setq first-failure error
                 first-count (1+ first-count))))
        ;; A second logical waiter proves that terminal reconciliation cleans
        ;; every watcher for the fenced key, not only the first callback.
        (e-session-persistence-await-record-prefix
         controller "session-replacement" '("same-key")
         (lambda (_value) nil)
         (lambda (error)
           (setq second-failure error
                 second-count (1+ second-count))))
        (let ((e-session-persistence-command-byte-limit 8))
          (should-error
           (e-session-persistence-submit-record
            controller "session-replacement"
            '(:type "message" :id "same-key"
              :content "replacement is rejected before admission"))
           :type 'e-session-persistence-command-error))
        (should (eq
                 (e-session-persistence--record-state
                  controller "session-replacement" "same-key")
                 'preflight-failed))
        ;; Ownership is fenced while the predecessor command remains available
        ;; through its physical command index and callback lifecycle.
        (should-not
         (gethash key (e-session-persistence-record-command-ids controller)))
        (should (equal
                 (gethash first
                          (e-session-persistence-record-command-keys controller))
                 key))
        (should (= first-count 1))
        (should (= second-count 1))
        (should first-failure)
        (should second-failure)
        (should (= (hash-table-count
                    (e-session-persistence-record-watchers controller))
                   0))
        ;; The predecessor's physical success remains caller-visible but
        ;; cannot overwrite the newer failed logical attempt.
        (e-session-persistence--handle-response
         controller (list :id first :ok t :result 'old-success))
        (should (= predecessor-done 1))
        (should (= predecessor-error 0))
        (should (= first-count 1))
        (should (= second-count 1))
        (should (eq
                 (e-session-persistence--record-state
                  controller "session-replacement" "same-key")
                 'preflight-failed))
        (should (= (hash-table-count
                    (e-session-persistence-outbox controller))
                   0))))))

(ert-deftest e-session-persistence-test-prefix-barrier-preflight-replacement-fences-failure-predecessor ()
  "A rejected replacement fences a predecessor terminal failure too."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "preflight-fences-failure"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (predecessor-done 0)
         (predecessor-error 0)
         (barrier-failure nil)
         (barrier-count 0)
         (key (list "session-replacement" "same-key")))
    (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'display-warning)
               (lambda (&rest _arguments) nil)))
      (let ((first
             (e-session-persistence-submit-record
              controller "session-replacement"
              '(:type "message" :id "same-key" :content "predecessor")
              (lambda (&rest _value)
                (setq predecessor-done (1+ predecessor-done)))
              (lambda (&rest _error)
                (setq predecessor-error (1+ predecessor-error))))))
        (e-session-persistence-await-record-prefix
         controller "session-replacement" '("same-key")
         (lambda (_value) nil)
         (lambda (error)
           (setq barrier-failure error
                 barrier-count (1+ barrier-count))))
        (let ((e-session-persistence-command-byte-limit 8))
          (should-error
           (e-session-persistence-submit-record
            controller "session-replacement"
            '(:type "message" :id "same-key"
              :content "replacement is rejected before admission"))
           :type 'e-session-persistence-command-error))
        (should (eq
                 (e-session-persistence--record-state
                  controller "session-replacement" "same-key")
                 'preflight-failed))
        (should-not
         (gethash key (e-session-persistence-record-command-ids controller)))
        (should barrier-failure)
        (should (= barrier-count 1))
        (should (= (hash-table-count
                    (e-session-persistence-record-watchers controller))
                   0))
        ;; The old physical rejection still invokes its caller callback, but
        ;; cannot change the terminal state of the replacement attempt.
        (e-session-persistence--handle-response
         controller
         (list :id first :ok :json-false :retryable :json-false))
        (should (= predecessor-done 0))
        (should (= predecessor-error 1))
        (should (= barrier-count 1))
        (should (eq
                 (e-session-persistence--record-state
                  controller "session-replacement" "same-key")
                 'preflight-failed))
        (should (= (hash-table-count
                    (e-session-persistence-outbox controller))
                   0))))))

(ert-deftest e-session-persistence-test-prefix-barrier-waits-for-submitted-ack ()
  "An admitted record remains pending until its explicit writer acknowledgement."
  (let* ((directory (make-temp-file "e-session-prefix-submitted-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller nil)
         (done nil)
         (failure nil))
    (unwind-protect
        (progn
          (e-session-create store :id "session-submitted")
          (setq controller (e-session-persistence-enable store))
          (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
                     (lambda (&rest _arguments) nil))
                    ((symbol-function 'e-session-persistence-request-checkpoint)
                     (lambda (&rest _arguments) nil)))
            (let* ((entry (e-session-append-message
                           store "session-submitted"
                           '(:id "submitted-entry" :role user :content "wait")))
                   (entry-id (plist-get entry :id))
                   (key (list "session-submitted" entry-id))
                   (command-id (gethash
                                key
                                (e-session-persistence-record-command-ids
                                 controller))))
              (should (equal entry-id "submitted-entry"))
              (should (equal
                       (e-session-persistence--record-state
                        controller "session-submitted" entry-id)
                       'submitted))
              ;; A submitted state with a missing command mapping is an
              ;; invalid active record, never an implicit acknowledgement.
              (remhash key (e-session-persistence-record-command-ids controller))
              (e-session-context-lifetime-durability-barrier
               store "session-submitted" (list entry-id)
               (lambda (value) (setq done value))
               (lambda (error) (setq failure error)))
              (should-not done)
              (should (equal (car failure) 'e-session-persistence-error))
              (puthash key command-id
                       (e-session-persistence-record-command-ids controller))
              (setq done nil failure nil)
              (e-session-context-lifetime-durability-barrier
               store "session-submitted" (list entry-id)
               (lambda (value) (setq done value))
               (lambda (error) (setq failure error)))
              (should-not done)
              (should-not failure)
              (e-session-persistence--handle-response
               controller (list :id command-id :ok t :result 'written))
              (should (eq
                       (e-session-persistence--record-state
                        controller "session-submitted" entry-id)
                       'acknowledged))
              (should done)
              (should-not failure))))
      (when-let ((process (and controller
                              (e-session-persistence-process controller))))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-prefix-barrier-accepts-replayed-durable-entry ()
  "A journal-replayed entry is an explicit durable proof without a live command."
  (let* ((directory (make-temp-file "e-session-prefix-replayed-" t))
         (first (e-session-persistent-store-create directory))
         (reopened nil)
         (controller nil)
         (done nil)
         (failure nil))
    (unwind-protect
        (progn
          (e-session-create first :id "session-replayed")
          (e-session-append-message
           first "session-replayed"
           '(:id "replayed-entry" :role user :content "on disk"))
          ;; The direct store has no asynchronous writer.  Explicit migration
          ;; creates the checkpoint proof used by the normal restart loader.
          (e-session-migrate-session-checkpoint first "session-replayed")
          (setq reopened (e-session-persistent-store-create directory)
                controller (e-session-persistence-enable reopened))
          (let ((entry (e-session-entry-by-id
                        reopened "session-replayed" "replayed-entry")))
            (should (eq (plist-get entry :durability-state)
                        'replayed-durable))
            (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
                       (lambda (&rest _arguments) nil))
                      ((symbol-function 'e-session-persistence-request-checkpoint)
                       (lambda (&rest _arguments) nil)))
              (e-session-context-lifetime-durability-barrier
               reopened "session-replayed" '("replayed-entry")
               (lambda (value) (setq done value))
               (lambda (error) (setq failure error)))
              (should done)
              (should-not failure))))
      (dolist (store (list first reopened))
        (when-let ((target (and store
                                (e-session-store-persistence-controller store)
                                (e-session-persistence-process
                                 (e-session-store-persistence-controller store)))))
          (when (process-live-p target) (kill-process target))))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-prefix-barrier-retains-many-terminal-failures ()
  "Terminal failures beyond the old cache size never become unknown successes."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "many-failures"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (count 300)
         done
         failure)
    (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'display-warning)
               (lambda (&rest _arguments) nil)))
      (dotimes (index count)
        (let* ((record-id (format "failed-%03d" index))
               (command-id
                (e-session-persistence-submit-record
                 controller "session-many-failures"
                 (list :type "message" :id record-id))))
          (e-session-persistence--handle-response
           controller
           (list :id command-id :ok :json-false :retryable :json-false))))
      (should (= (hash-table-count
                  (e-session-persistence-record-states controller))
                 count))
      (dolist (record-id '("failed-000" "failed-299"))
        (setq done nil failure nil)
        (e-session-persistence-await-record-prefix
         controller "session-many-failures" (list record-id)
         (lambda (value) (setq done value))
         (lambda (error) (setq failure error)))
        (should-not done)
        (should (equal (car failure) 'e-session-persistence-error))))))

(ert-deftest e-session-persistence-test-prefix-barrier-stale-success-rebinds-replacement ()
  "A stale success cannot settle a logical record after replacement."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "stale-success"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (done nil)
         (failure nil)
         (done-count 0)
         (failure-count 0))
    (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
               (lambda (&rest _arguments) nil)))
      (let* ((first
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "first")))
             (_result
              (e-session-persistence-await-record-prefix
               controller "session-replacement" '("same-key")
               (lambda (value)
                 (setq done value
                       done-count (1+ done-count)))
               (lambda (error)
                 (setq failure error
                       failure-count (1+ failure-count)))))
             (second
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "replacement"))))
        (should-not done)
        (should-not failure)
        ;; The first response is stale: the authoritative logical record now
        ;; points at SECOND, so it must only move the watcher to SECOND.
        (e-session-persistence--handle-response
         controller (list :id first :ok t :result 'stale))
        (should-not done)
        (should-not failure)
        (should (= done-count 0))
        (should (= failure-count 0))
        (should (equal
                 (gethash (list "session-replacement" "same-key")
                          (e-session-persistence-record-command-ids controller))
                 second))
        (e-session-persistence--handle-response
         controller (list :id second :ok t :result 'replacement))
        (should done)
        (should-not failure)
        (should (= done-count 1))
        (should (= failure-count 0))
        ;; A duplicate response has no callback left to settle again.
        (e-session-persistence--handle-response
         controller (list :id second :ok t :result 'duplicate))
        (should (= done-count 1))
        (should (= failure-count 0))))))

(ert-deftest e-session-persistence-test-prefix-barrier-stale-failure-rebinds-replacement ()
  "A stale terminal failure cannot fail a logical record after replacement."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "stale-failure"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (done nil)
         (failure nil)
         (done-count 0)
         (failure-count 0))
    (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
               (lambda (&rest _arguments) nil)))
      (let* ((first
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "first")))
             (_result
              (e-session-persistence-await-record-prefix
               controller "session-replacement" '("same-key")
               (lambda (value)
                 (setq done value
                       done-count (1+ done-count)))
               (lambda (error)
                 (setq failure error
                       failure-count (1+ failure-count)))))
             (second
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "replacement"))))
        (e-session-persistence--handle-response
         controller
         (list :id first :ok :json-false :retryable :json-false))
        (should-not done)
        (should-not failure)
        (should (= done-count 0))
        (should (= failure-count 0))
        (should (eq
                 (e-session-persistence--record-state
                  controller "session-replacement" "same-key")
                 'submitted))
        (e-session-persistence--handle-response
         controller
         (list :id second :ok :json-false :retryable :json-false))
        (should-not done)
        (should failure)
        (should (= done-count 0))
        (should (= failure-count 1))
        ;; The terminal failure callback is consumed exactly once.
        (e-session-persistence--handle-response
         controller
         (list :id second :ok :json-false :retryable :json-false))
        (should (= done-count 0))
        (should (= failure-count 1))))))

(ert-deftest e-session-persistence-test-prefix-barrier-replacement-success-before-old-response ()
  "A replacement can settle before its predecessor responds."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "early-success"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (done nil)
         (failure nil)
         (done-count 0)
         (failure-count 0))
    (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
               (lambda (&rest _arguments) nil)))
      (let* ((first
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "first")))
             (_result
              (e-session-persistence-await-record-prefix
               controller "session-replacement" '("same-key")
               (lambda (value)
                 (setq done value
                       done-count (1+ done-count)))
               (lambda (error)
                 (setq failure error
                       failure-count (1+ failure-count)))))
             (second
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "replacement"))))
        ;; Admission proactively moved the watcher to SECOND; no response for
        ;; FIRST is needed for the replacement to complete the logical wait.
        (should (= (hash-table-count
                    (e-session-persistence-callbacks controller))
                   1))
        (e-session-persistence--handle-response
         controller (list :id second :ok t :result 'replacement))
        (should done)
        (should-not failure)
        (should (= done-count 1))
        (should (= failure-count 0))
        (should (= (hash-table-count
                    (e-session-persistence-record-watchers controller))
                   0))
        ;; FIRST was already detached from the logical barrier.  A late stale
        ;; response remains harmless and cannot settle it a second time.
        (e-session-persistence--handle-response
         controller (list :id first :ok t :result 'late-old))
        (should (= done-count 1))
        (should (= failure-count 0))
        (should (eq
                 (e-session-persistence--record-state
                  controller "session-replacement" "same-key")
                 'acknowledged))))))

(ert-deftest e-session-persistence-test-prefix-barrier-replacement-failure-before-old-response ()
  "A replacement failure settles before its predecessor responds."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "early-failure"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (done nil)
         (failure nil)
         (done-count 0)
         (failure-count 0))
    (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'display-warning)
               (lambda (&rest _arguments) nil)))
      (let* ((first
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "first")))
             (_result
              (e-session-persistence-await-record-prefix
               controller "session-replacement" '("same-key")
               (lambda (value)
                 (setq done value
                       done-count (1+ done-count)))
               (lambda (error)
                 (setq failure error
                       failure-count (1+ failure-count)))))
             (second
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "replacement"))))
        (should (= (hash-table-count
                    (e-session-persistence-callbacks controller))
                   1))
        (e-session-persistence--handle-response
         controller
         (list :id second :ok :json-false :retryable :json-false))
        (should-not done)
        (should failure)
        (should (= done-count 0))
        (should (= failure-count 1))
        (should (= (hash-table-count
                    (e-session-persistence-record-watchers controller))
                   0))
        ;; The old command cannot convert the already-terminal replacement
        ;; into a second failure or a success.
        (e-session-persistence--handle-response
         controller
         (list :id first :ok :json-false :retryable :json-false))
        (should-not done)
        (should (= done-count 0))
        (should (= failure-count 1))
        (should (eq
                 (e-session-persistence--record-state
                  controller "session-replacement" "same-key")
                 'terminal-failed))))))

(ert-deftest e-session-persistence-test-prefix-barrier-multiple-watchers-share-replacement ()
  "Simultaneous barriers on one key all follow the replacement command."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "multiple-watchers"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (first-done nil)
         (second-done nil)
         (first-failure nil)
         (second-failure nil)
         (first-count 0)
         (second-count 0))
    (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
               (lambda (&rest _arguments))))
      (let* ((first
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "first")))
             (_first-result
              (e-session-persistence-await-record-prefix
               controller "session-replacement" '("same-key")
               (lambda (value)
                 (setq first-done value
                       first-count (1+ first-count)))
               (lambda (error) (setq first-failure error))))
             (_second-result
              (e-session-persistence-await-record-prefix
               controller "session-replacement" '("same-key")
               (lambda (value)
                 (setq second-done value
                       second-count (1+ second-count)))
               (lambda (error) (setq second-failure error))))
             (second
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "replacement"))))
        ;; One command entry carries both independent callback pairs; neither
        ;; barrier can overwrite the other during proactive rebinding.
        (should (= (hash-table-count
                    (e-session-persistence-callbacks controller))
                   1))
        (should (= (length
                    (gethash second
                             (e-session-persistence-callbacks controller)))
                   2))
        (e-session-persistence--handle-response
         controller (list :id second :ok t :result 'replacement))
        (should first-done)
        (should second-done)
        (should-not first-failure)
        (should-not second-failure)
        (should (= first-count 1))
        (should (= second-count 1))
        (should (= (hash-table-count
                    (e-session-persistence-record-watchers controller))
                   0))
        ;; The predecessor is no longer registered with either barrier.
        (e-session-persistence--handle-response
         controller (list :id first :ok :json-false :retryable :json-false))
        (should (= first-count 1))
        (should (= second-count 1))))))

(ert-deftest e-session-persistence-test-prefix-barrier-repeated-supersession-is-bounded ()
  "Repeated same-key replacement rebinds one watcher without callback leaks."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "repeated-supersession"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (done nil)
         (failure nil)
         (done-count 0)
         (failure-count 0))
    (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
               (lambda (&rest _arguments) nil)))
      (let* ((first
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "one")))
             (_result
              (e-session-persistence-await-record-prefix
               controller "session-replacement" '("same-key")
               (lambda (value)
                 (setq done value
                       done-count (1+ done-count)))
               (lambda (error)
                 (setq failure error
                       failure-count (1+ failure-count)))))
             (second
              (e-session-persistence-submit-record
               controller "session-replacement"
               '(:type "message" :id "same-key" :content "two"))))
        ;; Each old response rebinds to the current command.  A new command
        ;; is submitted between responses so the path exercises repeated,
        ;; rather than one-time, supersession.
        (e-session-persistence--handle-response
         controller (list :id first :ok t :result 'stale-one))
        (should (= (hash-table-count
                    (e-session-persistence-callbacks controller))
                   1))
        (let ((third
               (e-session-persistence-submit-record
                controller "session-replacement"
                '(:type "message" :id "same-key" :content "three"))))
          (e-session-persistence--handle-response
           controller (list :id second :ok t :result 'stale-two))
          (should (= (hash-table-count
                      (e-session-persistence-callbacks controller))
                     1))
          (let ((fourth
                 (e-session-persistence-submit-record
                  controller "session-replacement"
                  '(:type "message" :id "same-key" :content "four"))))
            (e-session-persistence--handle-response
             controller (list :id third :ok t :result 'stale-three))
            (should (= (hash-table-count
                        (e-session-persistence-callbacks controller))
                       1))
            (should-not done)
            (should-not failure)
            (should (= done-count 0))
            (should (= failure-count 0))
            (e-session-persistence--handle-response
             controller (list :id fourth :ok t :result 'final))
            (should done)
            (should-not failure)
            (should (= done-count 1))
            (should (= failure-count 0))
            (should (= (hash-table-count
                        (e-session-persistence-callbacks controller))
                       0))))))))

(ert-deftest e-session-persistence-test-prefix-barrier-does-not-watch-unrelated-key ()
  "A logical barrier neither settles nor rebinds an unrelated record."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "unrelated-key"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (done nil)
         (failure nil))
    (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
               (lambda (&rest _arguments) nil)))
      (let* ((a-first
              (e-session-persistence-submit-record
               controller "session-a"
               '(:type "message" :id "same-key" :content "a1")))
             (b-first
              (e-session-persistence-submit-record
               controller "session-b"
               '(:type "message" :id "other-key" :content "b1")))
             (_result
              (e-session-persistence-await-record-prefix
               controller "session-a" '("same-key")
               (lambda (value) (setq done value))
               (lambda (error) (setq failure error))))
             (a-second
              (e-session-persistence-submit-record
               controller "session-a"
               '(:type "message" :id "same-key" :content "a2"))))
        ;; The unrelated key can settle independently and must not touch A's
        ;; logical watcher or its authoritative replacement mapping.
        (e-session-persistence--handle-response
         controller (list :id b-first :ok t :result 'b-written))
        (should (eq
                 (e-session-persistence--record-state
                  controller "session-b" "other-key")
                 'acknowledged))
        (should-not done)
        (should-not failure)
        (should (equal
                 (gethash (list "session-a" "same-key")
                          (e-session-persistence-record-command-ids controller))
                 a-second))
        (e-session-persistence--handle-response
         controller (list :id a-first :ok t :result 'a-stale))
        (should-not done)
        (should-not failure)
        (e-session-persistence--handle-response
         controller (list :id a-second :ok t :result 'a-written))
        (should done)
        (should-not failure)))))

(ert-deftest e-session-persistence-test-command-budget-precedes-json-encoding ()
  "An oversized command is rejected before JSON allocates its representation."
  (let ((e-session-persistence-command-byte-limit 8)
        encoded)
    (cl-letf (((symbol-function 'json-encode)
               (lambda (_value) (setq encoded t) "{}")))
      (should-error
       (e-session-persistence--encode-command '(:value "123456789"))
       :type 'e-session-persistence-error)
      (should-not encoded))))

(ert-deftest e-session-persistence-test-json-error-never-enters-outbox ()
  "Opaque runtime state fails before it becomes an unsettled retry obligation."
  (require 'e-board-runtime)
  (let* ((directory (make-temp-file "e-session-persistence-json-error-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (token (e-board-runtime-endpoint-token--create
                 :harness-id :live
                 :harness-object-generation 7
                 :session-id "session"))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         callback-error)
    (unwind-protect
        (progn
          (should-error
           (e-session-persistence--submit
            controller
            (list :op "append" :session-id "session"
                  :record (list :type "message"
                                :metadata (list :board-endpoint-token token)))
            nil
            (lambda (err) (setq callback-error err)))
           :type 'e-session-persistence-command-error)
          (should callback-error)
          (should (= (hash-table-count
                      (e-session-persistence-outbox controller))
                     0))
          (should (= (e-session-store-unsettled-write-count store) 0))
          (should (= (plist-get (e-session-persistence-unsettled-state) :writes)
                     0))
          (should-not (e-session-persistence-retry-timer controller))
          (should (string-match-p
                   "not JSON-encodable"
                   (plist-get (e-session-persistence-status controller)
                              :last-error))))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-retry-reuses-prepared-wire-command ()
  "A queued request is JSON-encoded once and retries reuse the same wire text."
  (let* ((directory (make-temp-file "e-session-persistence-wire-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (original-json-encode (symbol-function 'json-encode))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (encode-count 0)
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'json-encode)
                   (lambda (value)
                     (setq encode-count (1+ encode-count))
                     (funcall original-json-encode value)))
                  ((symbol-function 'e-session-persistence--ensure)
                   (lambda (_controller) t))
                  ((symbol-function 'process-send-string)
                   (lambda (_process wire) (push wire sent))))
          (let* ((id (e-session-persistence--submit
                      controller (list :op "checkpoint")))
                 (command (gethash id
                                   (e-session-persistence-outbox controller))))
            (should (e-session-persistence-command-p command))
            (e-session-persistence--send controller command)
            (should (= encode-count 1))
            (should (= (length sent) 2))
            (should (equal (car sent) (cadr sent)))
            (e-session-persistence--handle-response
             controller (list :id id :ok t))))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-retry-resends-fixed-pages ()
  "A writer restart yields after each fixed retry page."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "test"))
         (e-session-persistence-retry-page-size 2)
         sent continuation)
    (dolist (id '("one" "two" "three"))
      (puthash id (list :id id) (e-session-persistence-outbox controller))
      (e-session-persistence--append-outbox-id controller id))
    (setf (e-session-persistence-retry-cursor controller)
          (e-session-persistence-outbox-head controller))
    (cl-letf (((symbol-function 'e-session-persistence--send)
               (lambda (_controller request) (push (plist-get request :id) sent)))
              ((symbol-function 'e-session-persistence--live-p)
               (lambda (_controller) t))
              ((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq continuation (lambda () (apply function arguments)))
                 (timer-create))))
      (e-session-persistence--resend-page controller)
      (should (equal (nreverse sent) '("one" "two")))
      (should continuation)
      (setq sent nil)
      (funcall continuation)
      (should (equal sent '("three")))
      (should-not (e-session-persistence-retry-cursor controller)))))

(ert-deftest e-session-persistence-test-replacement-replays-before-new-command ()
  "A replacement writer receives pending commands before the newest command."
  (let* ((store (e-session-store-create))
         (controller (e-session-persistence--create
                      :store store :instance-id "replacement"))
         (e-session-persistence-retry-page-size 1)
         (current-process 'writer-one)
         continuations
         sent)
    (setf (e-session-persistence-process controller) current-process)
    (cl-letf (((symbol-function 'e-session-persistence--ensure)
               (lambda (target)
                 (setf (e-session-persistence-process target) current-process)
                 current-process))
              ((symbol-function 'e-session-persistence--send)
               (lambda (target command)
                 (push (list (e-session-persistence-process target)
                             (plist-get
                              (e-session-persistence-command-request command)
                              :id))
                       sent)))
              ((symbol-function 'e-session-persistence--live-p)
               (lambda (_target) t))
              ((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (push (lambda () (apply function arguments)) continuations)
                 (timer-create))))
      (e-session-persistence--submit controller (list :op "first"))
      (setq current-process 'writer-two)
      (e-session-persistence--submit controller (list :op "second"))
      (e-session-persistence--submit controller (list :op "third"))
      (should
       (equal (nreverse (copy-sequence sent))
              '((writer-one "replacement:1")
                (writer-two "replacement:1"))))
      (while continuations
        (funcall (pop continuations)))
      (should
       (equal (nreverse sent)
              '((writer-one "replacement:1")
                (writer-two "replacement:1")
                (writer-two "replacement:2")
                (writer-two "replacement:3")))))))

(ert-deftest e-session-persistence-test-writer-commits-journal-and-catalog ()
  "The writer owns durable JSONL and catalog work outside the Emacs mutation path."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-persistence-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (writes 0)
         (original-write-region (symbol-function 'write-region)))
    (unwind-protect
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest arguments)
                     (setq writes (1+ writes))
                     (apply original-write-region arguments))))
          (e-session-create store :id "session-1")
          (e-session-append-message
           store "session-1" '(:role user :content "writer owned"))
          ;; No session file operation is performed by this Emacs process.
          (should (= writes 0))
          (e-session-persistence-test--await-durable store)
          (should (= writes 0))
          (should (string-match-p "^[0-9A-Z]+:[0-9]+\\'"
                                  (e-session-persistence-submit-record
                                   controller "session-1"
                                   '(:type "session-info" :metadata (:retry t)))))
          (e-session-persistence-test--await-durable store)
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (plist-get (car (e-session-messages loaded "session-1"))
                                      :content)
                           "writer owned"))
            (should (file-exists-p (expand-file-name "index.json" directory)))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-checkpoints-dirty-sessions-before-one-reindex ()
  "Dirty sessions use bounded commands followed by one reindex barrier."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-checkpoint-batch-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (e-session-persistence-command-node-limit 300)
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         (original-submit (symbol-function 'e-session-persistence--submit))
         submitted
         states
         done
         failure)
    (unwind-protect
        (let ((e-session--unsettled-change-function
               (lambda (state) (push (copy-sequence state) states))))
          (cl-letf (((symbol-function 'e-session-persistence--submit)
                     (lambda (target operation &optional on-done on-error
                                              on-queued on-preflight-error)
                        (push (copy-tree operation) submitted)
                        (funcall original-submit target operation
                                 on-done on-error on-queued
                                 on-preflight-error))))
            (dolist (session-id '("one" "two"))
              (e-session-create store :id session-id)
              (dotimes (index 20)
                (e-session-append-board-message
                 store session-id
                 (list :id (format "%s-%d" session-id index)
                       :kind 'output))))
            (let* ((dirty (e-session-checkpoint-dirty-session-ids store))
                   (aggregate
                    (list :op "checkpoint"
                          :sessions
                          (vconcat
                           (mapcar
                            (lambda (session-id)
                              (e-session-checkpoint-manifest store session-id))
                            dirty)))))
              (should (= (length dirty) 2))
              (should-not
               (e-session-persistence--command-within-budget-p aggregate))
              (dolist (session-id dirty)
                (should
                 (e-session-persistence--command-within-budget-p
                  (e-session-persistence--checkpoint-operation
                   controller session-id)))))
            (setq states nil)
            (e-session-finalize
             store
             (lambda (_value) (setq done t))
             (lambda (err) (setq failure err)))
            (let ((deadline (+ (float-time) 5.0)))
              (while (and (not done) (not failure) (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (should-not failure)
            (should done)
            (let* ((operations (nreverse submitted))
                   (checkpoints
                    (seq-filter
                     (lambda (operation)
                       (equal (plist-get operation :op) "checkpoint"))
                     operations))
                   (reindexes
                    (seq-filter
                     (lambda (operation)
                       (equal (plist-get operation :op) "reindex"))
                     operations))
                   (writes
                    (mapcar (lambda (state) (plist-get state :writes))
                            (nreverse states))))
              (should (= (length checkpoints) 2))
              (dolist (operation checkpoints)
                (should (= (length (plist-get operation :sessions)) 1)))
              (should (= (length reindexes) 1))
              (should (= (car (last writes)) 0))
              (should-not (memq 0 (butlast writes))))
            (let ((reopened (e-session-persistent-index-store-create directory)))
              (should (equal (sort (mapcar (lambda (entry)
                                             (plist-get entry :id))
                                           (e-session-list reopened))
                                   #'string<)
                             '("one" "two"))))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-checkpoint-preflight-failure-releases-batch ()
  "A rejected checkpoint leaves its session dirty without wedging quiescence."
  (let* ((directory (make-temp-file "e-session-checkpoint-reject-" t))
         (store (e-session-persistent-index-store-create directory))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         controller
         failure)
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (setq controller (e-session-persistence-enable store))
          (e-session--mark-checkpoint-dirty store "session-1")
          (let ((e-session-persistence-command-node-limit 8))
            (e-session-finalize
             store #'ignore (lambda (err) (setq failure err))))
          (should failure)
          (should (string-match-p "pre-encoding budget"
                                  (error-message-string failure)))
          (should (equal (e-session-checkpoint-dirty-session-ids store)
                         '("session-1")))
          (should (= (e-session-store-unsettled-write-count store) 0))
          (should (= (plist-get (e-session-persistence-unsettled-state) :writes)
                     0))
          (should-not (e-session-persistence-checkpoint-timer controller))
          (should (= (hash-table-count
                      (e-session-persistence-outbox controller))
                     0)))
      (when-let ((process (and controller
                              (e-session-persistence-process controller))))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-stale-catalog-lists-unmigrated-journal ()
  "Catalog reconciliation lists a new journal but resume requires migration."
  (let* ((directory (make-temp-file "e-session-reconcile-" t))
         (first (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create first :id "indexed")
          ;; The direct store wrote an index containing only INDEXED.
          (let ((journal (expand-file-name "sessions/unindexed.jsonl" directory)))
            (make-directory (file-name-directory journal) t)
            (with-temp-file journal
              (insert "{\"type\":\"session\",\"session-id\":\"unindexed\",\"timestamp\":\"2026-07-30T00:00:00Z\"}\n")))
          (let ((indexed (e-session-persistent-index-store-create directory)))
            (should (e-session-get indexed "indexed"))
            (should (seq-find
                     (lambda (session)
                       (equal (plist-get session :id) "unindexed"))
                     (e-session-list indexed)))
            (should-error (e-session-get indexed "unindexed")
                          :type 'e-session-checkpoint-missing)))
      (delete-directory directory t))))

(defun e-session-persistence-test--await-command (controller)
  "Wait for CONTROLLER to receive responses for its queued commands."
  (let ((deadline (+ (float-time) 5.0)))
    (while (and (> (hash-table-count (e-session-persistence-outbox controller)) 0)
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (= (hash-table-count (e-session-persistence-outbox controller)) 0))))

(ert-deftest e-session-persistence-test-restart-deduplicates-before-checkpoint ()
  "A restarted controller does not duplicate an acknowledged pre-checkpoint command."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-persistence-no-checkpoint-" t))
         (store (e-session-persistent-index-store-create directory))
         (first (e-session-persistence--create :store store :instance-id "restart"))
         (second (e-session-persistence--create :store store :instance-id "restart"))
         (journal (expand-file-name "sessions/session-1.jsonl" directory))
         (checkpoint (expand-file-name "sessions/session-1.checkpoint.json" directory)))
    (unwind-protect
        (progn
          (e-session-persistence-submit-record
           first "session-1" '(:type "message" :id "message-1"))
          (e-session-persistence-test--await-command first)
          (should-not (file-exists-p checkpoint))
          (kill-process (e-session-persistence-process first))
          (e-session-persistence-submit-record
           second "session-1" '(:type "message" :id "message-1"))
          (e-session-persistence-test--await-command second)
          (with-temp-buffer
            (insert-file-contents journal)
            (should (= (count-lines (point-min) (point-max)) 1))))
      (dolist (controller (list first second))
        (when-let ((process (e-session-persistence-process controller)))
          (when (process-live-p process) (kill-process process))))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-restart-accepts-missing-and-reordered-command-identities ()
  "A gapped journal does not make an unseen lower command ID durable."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-persistence-gaps-" t))
         (store (e-session-persistent-index-store-create directory))
         (first (e-session-persistence--create
                 :store store :instance-id "writer"))
         (second (e-session-persistence--create
                  :store store :instance-id "writer" :next-sequence 2))
         (journal (expand-file-name "sessions/session-1.jsonl" directory)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory journal) t)
          (with-temp-file journal
            (insert "{\"type\":\"message\",\"writer-command-id\":\"writer:2\"}\n"))
          (e-session-persistence-submit-record
           first "session-1" '(:type "message" :id "message-1"))
          (e-session-persistence-test--await-command first)
          (e-session-persistence-submit-record
           second "session-1" '(:type "message" :id "message-3"))
          (e-session-persistence-test--await-command second)
          (with-temp-buffer
            (insert-file-contents journal)
            (should (equal (mapcar (lambda (line)
                                     (plist-get
                                      (json-parse-string line :object-type 'plist)
                                      :writer-command-id))
                                   (split-string (buffer-string) "\n" t))
                           '("writer:2" "writer:1" "writer:3")))))
      (dolist (controller (list first second))
        (when-let ((process (e-session-persistence-process controller)))
          (when (process-live-p process) (kill-process process))))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-rejects-truncated-journal-tail ()
  "A rejected tail leaves the journal unchanged and settles the command."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-persistence-tail-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence--create :store store :instance-id "writer"))
         (journal (expand-file-name "sessions/session-1.jsonl" directory))
         (tail "{\"type\":\"message\",\"writer-command-id\":\"writer:2\"}\n{")
         failure)
    (unwind-protect
        (progn
          (make-directory (file-name-directory journal) t)
          (with-temp-file journal (insert tail))
          (e-session-persistence--submit
           controller
           '(:op "append" :session-id "session-1"
             :record (:type "message" :id "message-1"))
           nil (lambda (err) (setq failure err)))
          (let ((deadline (+ (float-time) 5.0)))
            (while (and (not failure) (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should failure)
          (should (string-match-p "does not end with a newline"
                                  (error-message-string failure)))
          (should (= (hash-table-count
                      (e-session-persistence-outbox controller))
                     0))
          (with-temp-buffer
            (insert-file-contents journal)
            (should (equal (buffer-string) tail))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-rejects-malformed-journal-tail ()
  "A malformed newline-terminated tail leaves the journal unchanged."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-persistence-malformed-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence--create :store store :instance-id "writer"))
         (journal (expand-file-name "sessions/session-1.jsonl" directory))
         (tail "{\"type\":\"message\",\"writer-command-id\":\"writer:2\"}\nnot json\n")
         failure)
    (unwind-protect
        (progn
          (make-directory (file-name-directory journal) t)
          (with-temp-file journal (insert tail))
          (e-session-persistence--submit
           controller
           '(:op "append" :session-id "session-1"
             :record (:type "message" :id "message-1"))
           nil (lambda (err) (setq failure err)))
          (let ((deadline (+ (float-time) 5.0)))
            (while (and (not failure) (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should failure)
          (should (string-match-p "contains malformed JSONL"
                                  (error-message-string failure)))
          (with-temp-buffer
            (insert-file-contents journal)
            (should (equal (buffer-string) tail))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-retries-keep-journal-order-and-deduplicate ()
  "Replayed writer commands append once and preserve session record order."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-persistence-retry-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (sent nil))
    (unwind-protect
        (progn
          ;; Hold commands in the controller outbox, then deliver every one
          ;; twice as a process restart would.  The writer must retain only
          ;; the first delivery of each globally unique command id.
          (cl-letf (((symbol-function 'e-session-persistence--send)
                     (lambda (_controller request) (push request sent))))
            (e-session-create store :id "session-1")
            (e-session-append-message
             store "session-1" '(:role user :content "ordered retry")))
          (dolist (request (nreverse sent))
            (e-session-persistence--send controller request)
            (e-session-persistence--send controller request))
          (e-session-persistence-test--await-durable store)
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name "sessions/session-1.jsonl" directory))
            (should (= (count-lines (point-min) (point-max)) 2)))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (mapcar (lambda (message) (plist-get message :content))
                                   (e-session-messages loaded "session-1"))
                           '("ordered retry")))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-writer-request-rejection-is-terminal ()
  "A deterministic writer protocol rejection settles instead of retrying forever."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-persistence-reject-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (e-session--unsettled-write-count 0)
         (e-session--unsettled-generation 0)
         failure)
    (unwind-protect
        (progn
          (e-session-persistence--submit
           controller (list :op "unsupported") nil
           (lambda (err) (setq failure err)))
          (let ((deadline (+ (float-time) 5.0)))
            (while (and (not failure) (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should failure)
          (should (= (hash-table-count
                      (e-session-persistence-outbox controller))
                     0))
          (should (= (e-session-store-unsettled-write-count store) 0))
          (should-not (e-session-persistence-retry-timer controller))
          (should (string-match-p
                   "Unsupported writer operation"
                   (plist-get (e-session-persistence-status controller)
                              :last-error))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-checkpoint-indexes-board-identity ()
  "The writer checkpoints board identity without dormant-session policy rows."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-board-index-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-persistence-declare-board-state
           controller "session-1" "principal:owner" "board-1")
          (e-session-persistence-test--await-durable store)
          (let* ((indexed-store
                  (e-session-persistent-index-store-create directory))
                 (entry (car (e-session-list indexed-store))))
            (should (equal (plist-get entry :id) "session-1"))
            (should (equal (plist-get entry :board-id) "board-1"))
            (should (equal (plist-get entry :principal) "principal:owner"))
            (should-not (plist-member entry :state)))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (state (plist-get (e-session-get loaded "session-1")
                                   :board-session-state)))
            (should (equal state
                           '(:board-id "board-1"
                             :principal "principal:owner")))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-checkpoint-folds-message-display-state ()
  "Writer compaction retains the message record behind a later display update."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-display-checkpoint-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-append-message
           store "session-1" '(:id "message-1" :role assistant :content "hi"))
          (e-session-set-message-display
           store "session-1" "message-1" 'hidden)
          (e-session-persistence-test--await-durable store)
          (let* ((journal
                  (expand-file-name "sessions/session-1.jsonl" directory))
                 (checkpoint
                  (e-session--read-checkpoint store "session-1"))
                 (loaded (e-session-persistent-store-create directory))
                 (message (car (e-session-messages loaded "session-1"))))
            (should (= (plist-get checkpoint :journal-byte-offset)
                       (file-attribute-size (file-attributes journal))))
            (should (equal (plist-get message :id) "message-1"))
            (should (eq (plist-get message :display) 'hidden))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-checkpoint-retains-typed-board-id-collisions ()
  "Async compaction preserves every typed board envelope sharing a raw id."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-board-checkpoint-collision-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (session-id "session-1"))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (dolist (envelope '((:id "shared" :kind output)
                              (:id "shared" :record-type processing-chain)
                              (:id "shared" :record-type "processing-chain")
                              (:id "shared" :record-type processing-result)))
            (e-session-append-board-message store session-id envelope))
          (e-session-persistence-test--await-durable store)
          (let ((reopened (e-session-persistent-store-create directory)))
            (should (equal
                     (mapcar #'e-session--board-message-identity
                             (e-session-board-messages reopened session-id))
                     '((board-message . "shared")
                       (processing-chain . "shared")
                       (processing-result . "shared"))))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-checkpoint-retains-first-divergent-ordinary-board-envelope-after-clear ()
  "Async checkpoint replay keeps the first ordinary envelope in each clear domain."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-board-checkpoint-ordinary-clear-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (session-id "session-1"))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (e-session-append-board-message store session-id
                                          '(:id "shared" :kind before))
          (e-session-persistence-submit-record
           controller session-id
           '(:type "board-message" :session-id "session-1"
             :message (:id "shared" :kind before-divergent)))
          (e-session-clear-board-messages store session-id)
          (e-session-append-board-message store session-id
                                          '(:id "shared" :kind after))
          (e-session-persistence-submit-record
           controller session-id
           '(:type "board-message" :session-id "session-1"
             :message (:id "shared" :kind after-divergent)))
          (e-session-persistence-test--await-durable store)
          (let ((reopened (e-session-persistent-store-create directory)))
            (should (equal (e-session-board-messages reopened session-id)
                           '((:id "shared" :kind after :tags nil))))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-checkpoint-reuses-typed-board-ids-after-clear ()
  "Async compaction starts a new typed board identity domain after clear."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-board-checkpoint-clear-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (session-id "session-1"))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (dolist (envelope '((:id "shared" :kind before)
                              (:id "shared" :record-type processing-chain
                               :root-message-id "before")
                              (:id "shared" :record-type processing-result
                               :outcome before)))
            (e-session-append-board-message store session-id envelope))
          (e-session-clear-board-messages store session-id)
          (dolist (envelope '((:id "shared" :kind after)
                              (:id "shared" :record-type processing-chain
                               :root-message-id "after")
                              (:id "shared" :record-type processing-result
                               :outcome after)))
            (e-session-append-board-message store session-id envelope))
          (e-session-persistence-test--await-durable store)
          (let ((reopened (e-session-persistent-store-create directory)))
            (should (equal (e-session-board-messages reopened session-id)
                           '((:id "shared" :kind after :tags nil)
                             (:id "shared" :record-type processing-chain
                              :root-message-id "after" :tags nil)
                             (:id "shared" :record-type processing-result
                              :outcome after :tags nil))))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(provide 'e-session-persistence-test)

;;; e-session-persistence-test.el ends here
