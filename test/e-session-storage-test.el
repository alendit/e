;;; e-session-storage-test.el --- Tests for async session persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'json)
(require 'e-session)
(require 'e-session-aggregate)
(require 'e-session-codec)
(require 'e-session-storage)
(require 'e-session-catalog)
(require 'e-board)

(defun e-session-storage-test--state (store)
  "Return STORE's explicit storage-owned state for mechanism assertions."
  (or (e-session-storage--state-for store)
      (error "Unregistered storage owner: %S" store)))

(ert-deftest e-session-storage-test-bundled-writer-script-exists ()
  "The packaged runtime includes the writer beside its owning Lisp module."
  (should (file-readable-p (e-session-storage--writer-script))))

(ert-deftest e-session-storage-test-writer-script-ignores-current-buffer ()
  "Writer discovery remains anchored to its library from unrelated buffers."
  (let* ((expected (e-session-storage--writer-script))
         (buffer-file-name "/tmp/unrelated-project/daily.org")
         (default-directory "/tmp/unrelated-project/"))
    (should (equal (e-session-storage--writer-script) expected))))

;; These three tests were formerly mixed into the public facade suite.  They
;; are retained under their historical names so the preservation selector
;; remains stable, but now exercise only the storage owner and its aggregate
;; collaborator directly.
(ert-deftest e-session-storage-test-queued-persistent-writes-flush-later ()
  "Queued persistent stores defer disk writes until the queue is flushed."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued))
         (write-count 0)
         (orig-write-region (symbol-function 'write-region)))
    (unwind-protect
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest args)
                     (setq write-count (1+ write-count))
                     (apply orig-write-region args))))
          (let* ((session (e-session-create store :id "queued-session"))
                 (session-id (plist-get session :id)))
            (e-session-append-message
             store session-id
             '(:id "msg-1" :role user :content "queued hello"))
            (should (= write-count 0))
            (should (timerp (e-session-storage--state-write-queue-timer
                             (e-session-storage-test--state store))))
            (should (= (length (e-session-storage--state-write-queue
                                (e-session-storage-test--state store))) 2))
            (should (e-session-storage--state-index-write-pending
                     (e-session-storage-test--state store)))
            (e-session-storage-flush-write-queue store)
            (should (> write-count 0))
            (should-not (e-session-storage--state-write-queue
                         (e-session-storage-test--state store)))
            (should-not (e-session-storage--state-index-write-pending
                         (e-session-storage-test--state store)))
            (let* ((loaded (e-session-persistent-store-create directory))
                   (messages (e-session-aggregate-messages loaded session-id)))
              (should (equal (plist-get (car messages) :content)
                             "queued hello")))))
      (ignore-errors (e-session-storage-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-queued-writes-carry-generation-metadata ()
  "Queued persistent record writes carry generation, sequence, and criticality."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued)))
    (unwind-protect
        (let* ((session (e-session-create store :id "queued-metadata"))
               (session-id (plist-get session :id)))
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "queued metadata"))
          (let ((entries
                 (reverse (e-session-storage--state-write-queue
                           (e-session-storage-test--state store)))))
            (should (= (length entries) 2))
            (dolist (entry entries)
              (should (plist-member entry :session-id))
              (should (plist-member entry :record))
              (should (integerp (plist-get entry :generation)))
              (should (integerp (plist-get entry :sequence)))
              (should (eq (plist-get entry :criticality) 'critical))
              (should (plist-member entry :dependencies)))
            (should (< (plist-get (car entries) :sequence)
                       (plist-get (cadr entries) :sequence))))
          (let ((index-entry
                 (e-session-storage--state-index-write-pending
                  (e-session-storage-test--state store))))
            (should (plist-member index-entry :generation))
            (should (plist-member index-entry :sequence))
            (should (eq (plist-get index-entry :criticality) 'derived))))
      (ignore-errors (e-session-storage-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-abort-created-preserves-unrelated-queued-work ()
  "Application rollback removes only its writes from a shared queue."
  (let* ((directory (make-temp-file "e-session-abort-created-" t))
         (store (e-session-persistent-index-store-create
                 directory :write-mode 'queued)))
    (unwind-protect
        (progn
          (e-session-create store :id "rollback-target")
          (e-session-create store :id "unrelated-session")
          (e-session-append-message
           store "unrelated-session"
           '(:id "unrelated-message" :role user :content "keep me"))
          (let* ((queue-before (copy-sequence
                                (e-session-storage--state-write-queue
                                 (e-session-storage-test--state store))))
                 (target-entries
                  (cl-remove-if-not
                   (lambda (entry)
                     (equal (e-session-storage--queued-entry-session-id entry)
                            "rollback-target"))
                   queue-before))
                 (other-entries
                  (cl-remove-if-not
                   (lambda (entry)
                     (equal (e-session-storage--queued-entry-session-id entry)
                            "unrelated-session"))
                   queue-before))
                 (timer-before
                  (e-session-storage--state-write-queue-timer
                   (e-session-storage-test--state store)))
                 (index-before
                  (e-session-storage--state-index-write-pending
                   (e-session-storage-test--state store)))
                 (unsettled-before
                  (e-session-storage--state-unsettled-write-count
                   (e-session-storage-test--state store))))
            (should target-entries)
            (should other-entries)
            (should (timerp timer-before))
            (should index-before)
            (e-session-abort-created store "rollback-target")
            (should-error (e-session-aggregate-get store "rollback-target")
                          :type 'e-session-missing)
            (should (e-session-aggregate-get store "unrelated-session"))
            (should (equal
                     (e-session-storage--state-index-write-pending
                      (e-session-storage-test--state store))
                     index-before))
            (should (eq (e-session-storage--state-write-queue-timer
                         (e-session-storage-test--state store))
                        timer-before))
            (should (= (e-session-storage--state-unsettled-write-count
                        (e-session-storage-test--state store))
                       (- unsettled-before (length target-entries))))
            (should-not
             (member "rollback-target"
                     (e-session-storage-checkpoint-dirty-session-ids store)))
            (should (member "unrelated-session"
                            (e-session-storage-checkpoint-dirty-session-ids store)))
            (should (equal
                     (mapcar #'e-session-storage--queued-entry-session-id
                             (e-session-storage--state-write-queue
                              (e-session-storage-test--state store)))
                     (mapcar #'e-session-storage--queued-entry-session-id
                             other-entries)))
            (e-session-storage-flush-write-queue store)
            (let ((reopened (e-session-persistent-store-create directory)))
              (should (e-session-aggregate-get reopened "unrelated-session"))
              (should (equal
                       (plist-get
                        (car (e-session-aggregate-messages reopened
                                                           "unrelated-session"))
                        :content)
                       "keep me")))))
      (ignore-errors (e-session-storage-flush-write-queue store))
      (delete-directory directory t))))

(defun e-session-storage-test--await-durable (store)
  "Wait in this test process for STORE's asynchronous durability boundary."
  (let ((deadline (+ (float-time) 5.0)) done failure)
    (e-session-storage-finalize-store
     store (lambda (_value) (setq done t)) (lambda (err) (setq failure err)))
    (while (and (not done) (not failure) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should-not failure)
    (should done)))

(ert-deftest e-session-storage-test-public-append-defers-checkpoint-projection ()
  "Public append projects the latest checkpoint only at the durability boundary."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-deferred-checkpoint-" t))
         (store (e-session-persistent-index-store-create directory))
         (session-id "long-running-session")
         controller)
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          ;; Model the material history from the latency report without turning
          ;; elapsed time into a contract.  The existing command-budget fixture
          ;; covers the larger bounded manifest shape independently.
          (dotimes (index 40)
            (e-session-append-message
             store session-id
             (list :id (format "history-message-%03d" index)
                   :role (if (zerop (% index 2)) 'user 'assistant)
                   :content (format "historical transcript entry %03d" index))))
          (dotimes (index 270)
            (e-session-append-board-message
             store session-id
             (list :id (format "history-board-%03d" index)
                   :kind 'activity)))
          (setq controller (e-session-enable store))
          (let ((original-manifest
                 (symbol-function 'e-session-catalog-checkpoint-manifest))
                (manifest-count 0)
                done failure)
            (cl-letf (((symbol-function 'e-session-catalog-checkpoint-manifest)
                       (lambda (&rest arguments)
                         (setq manifest-count (1+ manifest-count))
                         (apply original-manifest arguments))))
              (e-session-append-message
               store session-id
               '(:id "message-appended-during-turn"
                 :role assistant
                 :content "appended without synchronous projection"))
              (should (= manifest-count 0))
              (e-session-finalize
               store
               (lambda (_value) (setq done t))
               (lambda (err) (setq failure err)))
              (let ((deadline (+ (float-time) 5.0)))
                (while (and (not done) (not failure)
                            (< (float-time) deadline))
                  (accept-process-output nil 0.02)))
              (should-not failure)
              (should done)
              (should (> manifest-count 0))))
          (let* ((reopened (e-session-persistent-index-store-create directory))
                 (messages (e-session-messages reopened session-id)))
            (should (equal (plist-get (car (last messages)) :id)
                           "message-appended-during-turn"))))
      (when-let ((process (and controller
                              (e-session-storage--controller-process controller))))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(defun e-session-storage-test--route-routing-policy (policy)
  "Route symbol and string attribute messages through POLICY's selector.
This is an owner-level routing assertion: it exercises the restored selector
against real board messages instead of comparing only persisted plists."
  (let* ((board (e-board-create))
         (symbol-selector
          (e-session-board-routing-policy-copy-value
           (plist-get policy :pickup-selector)))
         (string-selector
          (e-session-board-routing-policy-copy-value symbol-selector))
         (symbol-attributes
          (plist-get symbol-selector :attributes))
         (string-attributes
          (e-session-board-routing-policy-copy-value symbol-attributes)))
    (plist-put string-attributes :symbol "car")
    (plist-put string-selector :attributes string-attributes)
    (e-board-add-participant board :id "symbol-recipient"
                             :create-pickup-subscription-id "symbol-address")
    (e-board-add-participant board :id "string-recipient"
                             :create-pickup-subscription-id "string-address")
    (e-board-subscribe board "symbol-recipient" symbol-selector
                       :id "symbol-selector")
    (e-board-subscribe board "string-recipient" string-selector
                       :id "string-selector")
    (cl-letf (((symbol-function 'e-board--schedule-input-classification)
               (lambda (board)
                 (e-board-drain-input-classifications board))))
      (dolist (case (list (list 'car "symbol-recipient")
                          (list "car" "string-recipient")))
        (let* ((attributes (e-session-board-routing-policy-copy-value
                            symbol-attributes))
               (_ (plist-put attributes :symbol (car case)))
               (publication
                (e-board-post-input board
                                   :tags '(private)
                                   :attributes attributes))
               (message (e-board-publication-message publication)))
          (should (equal (e-board-message-matching-participant-ids message)
                         (list (cadr case)))))))))

(ert-deftest e-session-storage-test-unsettled-transfer-has-no-false-zero ()
  "Checkpoint timer ownership transfers to the writer outbox atomically."
  (let* ((directory (make-temp-file "e-session-storage-count-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
         (e-session-storage--unsettled-write-count 0)
         (e-session-storage--unsettled-generation 0)
         callback
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq callback (lambda () (apply function arguments)))
                     (timer-create)))
                  ((symbol-function 'e-session-storage--ensure)
                   (lambda (_controller) t))
                  ((symbol-function 'e-session-storage--send)
                   (lambda (_controller request) (push request sent))))
          (e-session-storage--request-checkpoint controller)
          (should (equal (e-session-storage-unsettled-state)
                         '(:generation 1 :writes 1)))
          (funcall callback)
          ;; The batch slot remains live alongside the current writer command.
          (should (= (plist-get (e-session-storage-unsettled-state) :writes)
                     2))
          (let ((request (e-session-storage--command-request (car sent))))
            (should (equal (plist-get request :op) "reindex"))
            (e-session-storage--handle-response
             controller (list :id (plist-get request :id) :ok t)))
          (should (= (plist-get (e-session-storage-unsettled-state) :writes)
                     0)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-command-budget-precedes-json-encoding ()
  "An oversized command is rejected before JSON allocates its representation."
  (let ((e-session-storage-command-byte-limit 8)
        encoded)
    (cl-letf (((symbol-function 'json-encode)
               (lambda (_value) (setq encoded t) "{}")))
      (should-error
       (e-session-storage--encode-command '(:value "123456789"))
       :type 'e-session-storage-error)
      (should-not encoded))))

(ert-deftest e-session-storage-test-admission-preflight-uses-final-id-shape ()
  "Admission preflight measures the command id that submission will use."
  (let* ((directory (make-temp-file "e-session-storage-admission-id-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store)))
    (unwind-protect
        (progn
          (setf (e-session-storage--controller-instance-id controller)
                "controller-instance-with-a-long-but-valid-id"
                (e-session-storage--controller-next-sequence controller) 9)
          (let ((command
                 (e-session-storage--validate-admission
                  controller "admission-session"
                  (list '(:type "session")))))
            (should
             (equal
              (plist-get (e-session-storage--command-request command) :id)
              "controller-instance-with-a-long-but-valid-id:10")))
          (should (= (e-session-storage--controller-next-sequence controller) 9)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-default-node-budget-accepts-bounded-checkpoint ()
  "The structural guard accommodates a valid bounded checkpoint manifest."
  (let* ((store (e-session-store-create))
         (session-id "large-checkpoint"))
    (e-session-aggregate-create store :id session-id)
    ;; Facts are retained independently of the recent board tail.  This is the
    ;; same bounded union shape that exposed the too-small default node limit.
    (dotimes (index 73)
      (e-session-aggregate-append-board-message
       store session-id
       (list :id (format "fact-%03d" index) :kind 'fact)))
    (dotimes (index 256)
      (e-session-aggregate-append-board-message
       store session-id
       (list :id (format "activity-%03d" index) :kind 'activity)))
    (dotimes (index 1100)
      (e-session-aggregate-append-message
       store session-id
       (list :id (format "entry-%04d" index)
             :role 'user
             :content "x")))
    (let* ((manifest (e-session-checkpoint-manifest store session-id))
           (request (list :op "checkpoint" :sessions (vector manifest))))
      (should (= (length (plist-get manifest :board-message-identities)) 329))
      (should (= (length (plist-get manifest :entry-ids)) 1100))
      (should (< (string-bytes (json-encode request))
                 e-session-storage-command-byte-limit))
      (should-not
       (let ((e-session-storage-command-node-limit 4096))
         (e-session-storage--command-within-budget-p request)))
      (should (e-session-storage--command-within-budget-p request)))))

(ert-deftest e-session-storage-test-json-error-never-enters-outbox ()
  "Opaque runtime state fails before it becomes an unsettled retry obligation."
  (require 'e-board-runtime)
  (let* ((directory (make-temp-file "e-session-storage-json-error-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
         (token (e-board-runtime-endpoint-token--create
                 :harness-id :live
                 :harness-object-generation 7
                 :session-id "session"))
         (e-session-storage--unsettled-write-count 0)
         (e-session-storage--unsettled-generation 0)
         callback-error)
    (unwind-protect
        (progn
          (should-error
           (e-session-storage--submit
            controller
            (list :op "append" :session-id "session"
                  :record (list :type "message"
                                :metadata (list :board-endpoint-token token)))
            nil
            (lambda (err) (setq callback-error err)))
           :type 'e-session-storage-command-error)
          (should callback-error)
          (should (= (hash-table-count
                      (e-session-storage--controller-outbox controller))
                     0))
          (should (= (e-session-storage--state-unsettled-write-count
                      (e-session-storage-test--state store)) 0))
          (should (= (plist-get (e-session-storage-unsettled-state) :writes)
                     0))
          (should-not (e-session-storage--controller-retry-timer controller))
          (should (string-match-p
                   "not JSON-encodable"
                   (plist-get (e-session-storage--status controller)
                              :last-error))))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-retry-reuses-prepared-wire-command ()
  "A queued request is JSON-encoded once and retries reuse the same wire text."
  (let* ((directory (make-temp-file "e-session-storage-wire-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
         (original-json-encode (symbol-function 'json-encode))
         (e-session-storage--unsettled-write-count 0)
         (e-session-storage--unsettled-generation 0)
         (encode-count 0)
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'json-encode)
                   (lambda (value)
                     (setq encode-count (1+ encode-count))
                     (funcall original-json-encode value)))
                  ((symbol-function 'e-session-storage--ensure)
                   (lambda (_controller) t))
                  ((symbol-function 'process-send-string)
                   (lambda (_process wire) (push wire sent))))
          (let* ((id (e-session-storage--submit
                      controller (list :op "checkpoint")))
                 (command (gethash id
                                   (e-session-storage--controller-outbox controller))))
            (should (e-session-storage--command-p command))
            (e-session-storage--send controller command)
            (should (= encode-count 1))
            (should (= (length sent) 2))
            (should (equal (car sent) (cadr sent)))
            (e-session-storage--handle-response
             controller (list :id id :ok t))))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-retry-resends-fixed-pages ()
  "A writer restart yields after each fixed retry page."
  (let* ((store (e-session-store-create))
         (controller (e-session-storage--create
                      :store store :instance-id "test"))
         (e-session-storage-retry-page-size 2)
         sent continuation)
    (dolist (id '("one" "two" "three"))
      (puthash id (list :id id) (e-session-storage--controller-outbox controller))
      (e-session-storage--append-outbox-id controller id))
    (setf (e-session-storage--controller-retry-cursor controller)
          (e-session-storage--controller-outbox-head controller))
    (cl-letf (((symbol-function 'e-session-storage--send)
               (lambda (_controller request) (push (plist-get request :id) sent)))
              ((symbol-function 'e-session-storage--live-p)
               (lambda (_controller) t))
              ((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq continuation (lambda () (apply function arguments)))
                 (timer-create))))
      (e-session-storage--resend-page controller)
      (should (equal (nreverse sent) '("one" "two")))
      (should continuation)
      (setq sent nil)
      (funcall continuation)
      (should (equal sent '("three")))
      (should-not (e-session-storage--controller-retry-cursor controller)))))

(ert-deftest e-session-storage-test-replacement-replays-before-new-command ()
  "A replacement writer receives pending commands before the newest command."
  (let* ((store (e-session-store-create))
         (controller (e-session-storage--create
                      :store store :instance-id "replacement"))
         (e-session-storage-retry-page-size 1)
         (current-process 'writer-one)
         continuations
         sent)
    (setf (e-session-storage--controller-process controller) current-process)
    (cl-letf (((symbol-function 'e-session-storage--ensure)
               (lambda (target)
                 (setf (e-session-storage--controller-process target) current-process)
                 current-process))
              ((symbol-function 'e-session-storage--send)
               (lambda (target command)
                 (push (list (e-session-storage--controller-process target)
                             (plist-get
                              (e-session-storage--command-request command)
                              :id))
                       sent)))
              ((symbol-function 'e-session-storage--live-p)
               (lambda (_target) t))
              ((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (push (lambda () (apply function arguments)) continuations)
                 (timer-create))))
      (e-session-storage--submit controller (list :op "first"))
      (setq current-process 'writer-two)
      (e-session-storage--submit controller (list :op "second"))
      (e-session-storage--submit controller (list :op "third"))
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

(ert-deftest e-session-storage-test-writer-commits-journal-and-catalog ()
  "The writer owns durable JSONL and catalog work outside the Emacs mutation path."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-storage-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
         (writes 0)
         (original-write-region (symbol-function 'write-region)))
    (unwind-protect
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest arguments)
                     (setq writes (1+ writes))
                     (apply original-write-region arguments))))
          (e-session-create store :id "session-1")
          (e-session-declare-board-state
           store "session-1" "chat:session-1" "writer-board" "participant"
           '(:participant-id "writer-participant"
             :pickup-selector (:kind input :tags (private)
                              :attributes (:marker car :text "car"))
             :observer-selector (:subject-participant-id "writer-participant"
                                  :attributes (:marker car :text "car"))
             :default-tags (private)
             :default-to "writer-participant"))
          (e-session-append-message
           store "session-1" '(:role user :content "writer owned"))
          ;; No session file operation is performed by this Emacs process.
          (should (= writes 0))
          (e-session-storage-test--await-durable store)
          (should (= writes 0))
          (should (string-match-p ":[0-9]+\\'"
                                  (e-session-storage--submit-record
                                   controller "session-1"
                                   '(:type "session-info" :metadata (:retry t)))))
          (e-session-storage-test--await-durable store)
          (let* ((indexed (e-session-persistent-index-store-create directory))
                 (indexed-policy
                  (e-session-aggregate-board-routing-policy
                   (e-session-aggregate-peek-session indexed "session-1")))
                 (loaded (e-session-persistent-store-create directory)))
            ;; The Node writer has rewritten both the derived index and the
            ;; checkpoint/journal.  Check the index stub first, then force the
            ;; full replay path and compare the reversible attribute meaning.
            (should (equal (plist-get
                            (plist-get indexed-policy :pickup-selector)
                            :attributes)
                           '(:marker car :text "car")))
            (should (equal
                     (e-session-aggregate-board-routing-policy
                      (e-session-aggregate-get loaded "session-1"))
                     (e-session-aggregate-board-routing-policy
                      (e-session-aggregate-peek-session indexed "session-1"))))
            (should (equal (plist-get (car (e-session-aggregate-messages loaded "session-1"))
                                      :content)
                           "writer owned"))
            (should (file-exists-p (expand-file-name "index.json" directory)))))
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-checkpoints-dirty-sessions-before-one-reindex ()
  "Dirty sessions use bounded commands followed by one reindex barrier."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-checkpoint-batch-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
         (e-session-storage-command-node-limit 300)
         (e-session-storage--unsettled-write-count 0)
         (e-session-storage--unsettled-generation 0)
         (original-submit (symbol-function 'e-session-storage--submit))
         submitted
         states
         done
         failure)
    (unwind-protect
        (let ((e-session-storage--unsettled-change-function
               (lambda (state) (push (copy-sequence state) states))))
            (cl-letf (((symbol-function 'e-session-storage--submit)
                     (lambda (target operation &optional on-done on-error)
                        (push (copy-tree operation) submitted)
                        (funcall original-submit target operation
                                 on-done on-error))))
            (dolist (session-id '("one" "two"))
              (e-session-create store :id session-id)
              (dotimes (index 20)
                (e-session-append-board-message
                 store session-id
                 (list :id (format "%s-%d" session-id index)
                       :kind 'output))))
            (let* ((dirty (e-session-storage-checkpoint-dirty-session-ids store))
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
               (e-session-storage--command-within-budget-p aggregate))
              (dolist (session-id dirty)
                (should
                 (e-session-storage--command-within-budget-p
                  (e-session-storage--checkpoint-operation
                   controller session-id)))))
            (setq states nil)
            (e-session-storage-finalize-store
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
                                           (e-session-aggregate-list reopened))
                                   #'string<)
                             '("one" "two"))))))
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-checkpoint-preflight-failure-releases-batch ()
  "A rejected checkpoint leaves its session dirty without wedging quiescence."
  (let* ((directory (make-temp-file "e-session-checkpoint-reject-" t))
         (store (e-session-persistent-index-store-create directory))
         (e-session-storage--unsettled-write-count 0)
         (e-session-storage--unsettled-generation 0)
         controller
         done
         failure)
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (setq controller (e-session-storage-enable store))
          (e-session-storage--mark-checkpoint-dirty store "session-1")
          (let ((e-session-storage-command-node-limit 8))
            (e-session-storage-finalize-store
             store #'ignore (lambda (err) (setq failure err))))
          (should failure)
          (should (string-match-p "pre-encoding budget"
                                  (error-message-string failure)))
          (should (equal (e-session-storage-checkpoint-dirty-session-ids store)
                         '("session-1")))
          (should (= (e-session-storage--state-unsettled-write-count
                      (e-session-storage-test--state store)) 0))
          (should (= (plist-get (e-session-storage-unsettled-state) :writes)
                     0))
          (should-not (e-session-storage--controller-checkpoint-timer controller))
          (should (= (hash-table-count
                      (e-session-storage--controller-outbox controller))
                     0))
          (e-session-append-message
           store "session-1"
           '(:id "newer-after-checkpoint-failure"
             :role user
             :content "retry must project this state"))
          (setq failure nil)
          (e-session-finalize
           store
           (lambda (_value) (setq done t))
           (lambda (err) (setq failure err)))
          (let ((deadline (+ (float-time) 5.0)))
            (while (and (not done) (not failure) (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should-not failure)
          (should done)
          (should-not (e-session-storage-checkpoint-dirty-session-ids store))
          (let* ((reopened
                  (e-session-persistent-index-store-create directory))
                 (messages (e-session-messages reopened "session-1")))
            (should (equal (plist-get (car (last messages)) :id)
                           "newer-after-checkpoint-failure"))))
      (when-let ((process (and controller
                              (e-session-storage--controller-process controller))))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-mutation-during-checkpoint-batch-is-not-lost ()
  "A mutation during an older checkpoint batch survives its later barrier."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-checkpoint-replaced-" t))
         (e-session-storage-checkpoint-delay 60)
         (store (e-session-persistent-index-store-create directory))
         (session-id "mutation-during-checkpoint")
         (original-submit (symbol-function 'e-session-storage--submit))
         controller first-checkpoint-held held-continuation operations
         old-done old-failure new-done new-failure)
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (setq controller (e-session-enable store))
          (cl-letf
              (((symbol-function 'e-session-storage--submit)
                (lambda (target operation &optional on-done on-error)
                  (let ((operation-name (plist-get operation :op)))
                    (push operation-name operations)
                    (if (and (equal operation-name "checkpoint")
                             (not first-checkpoint-held))
                        ;; Submit the old checkpoint in its real writer order,
                        ;; but hold its continuation so the batch remains active
                        ;; while the public mutation installs newer work.
                        (progn
                          (setq first-checkpoint-held t)
                          (funcall
                           original-submit target operation
                           (lambda (value)
                             (setq held-continuation
                                   (lambda () (funcall on-done value))))
                           on-error))
                      (funcall original-submit target operation
                               on-done on-error))))))
            (e-session-finalize
             store
             (lambda (_value) (setq old-done t))
             (lambda (err) (setq old-failure err)))
            (let ((deadline (+ (float-time) 5.0)))
              (while (and (not held-continuation) (not old-failure)
                          (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (should-not old-failure)
            (should held-continuation)
            (e-session-append-message
             store session-id
             '(:id "message-during-old-checkpoint"
               :role assistant
               :content "must survive the next durability boundary"))
            (should (equal
                     (e-session-storage-checkpoint-dirty-session-ids store)
                     (list session-id)))
            (funcall held-continuation)
            (setq held-continuation nil)
            (let ((deadline (+ (float-time) 5.0)))
              (while (and (not old-done) (not old-failure)
                          (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (should-not old-failure)
            (should old-done)
            (should (equal
                     (e-session-storage-checkpoint-dirty-session-ids store)
                     (list session-id)))
            (e-session-finalize
             store
             (lambda (_value) (setq new-done t))
             (lambda (err) (setq new-failure err)))
            (let ((deadline (+ (float-time) 5.0)))
              (while (and (not new-done) (not new-failure)
                          (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (should-not new-failure)
            (should new-done)
            (should-not
             (e-session-storage-checkpoint-dirty-session-ids store))
            (should (equal (nreverse operations)
                           '("checkpoint" "append" "reindex"
                             "checkpoint" "reindex"))))
          (let* ((reopened (e-session-persistent-index-store-create directory))
                 (messages (e-session-messages reopened session-id)))
            (should (equal (plist-get (car (last messages)) :id)
                           "message-during-old-checkpoint"))))
      (when-let ((process (and controller
                              (e-session-storage--controller-process controller))))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-stale-catalog-lists-unmigrated-journal ()
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
            (should (e-session-aggregate-get indexed "indexed"))
            (should (seq-find
                     (lambda (session)
                       (equal (plist-get session :id) "unindexed"))
                     (e-session-aggregate-list indexed)))
            (should-error (e-session-load-session indexed "unindexed")
                          :type 'e-session-checkpoint-missing)))
      (delete-directory directory t))))

(defun e-session-storage-test--await-command (controller)
  "Wait for CONTROLLER to receive responses for its queued commands."
  (let ((deadline (+ (float-time) 5.0)))
    (while (and (> (hash-table-count (e-session-storage--controller-outbox controller)) 0)
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (= (hash-table-count (e-session-storage--controller-outbox controller)) 0))))

(ert-deftest e-session-storage-test-restart-deduplicates-before-checkpoint ()
  "A restarted controller does not duplicate an acknowledged pre-checkpoint command."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-storage-no-checkpoint-" t))
         (store (e-session-persistent-index-store-create directory))
         (first (e-session-storage--create :store store :instance-id "restart"))
         (second (e-session-storage--create :store store :instance-id "restart"))
         (journal (expand-file-name "sessions/session-1.jsonl" directory))
         (checkpoint (expand-file-name "sessions/session-1.checkpoint.json" directory)))
    (unwind-protect
        (progn
          (e-session-storage--submit-record
           first "session-1" '(:type "message" :id "message-1"))
          (e-session-storage-test--await-command first)
          (should-not (file-exists-p checkpoint))
          (kill-process (e-session-storage--controller-process first))
          (e-session-storage--submit-record
           second "session-1" '(:type "message" :id "message-1"))
          (e-session-storage-test--await-command second)
          (with-temp-buffer
            (insert-file-contents journal)
            (should (= (count-lines (point-min) (point-max)) 1))))
      (dolist (controller (list first second))
        (when-let ((process (e-session-storage--controller-process controller)))
          (when (process-live-p process) (kill-process process))))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-restart-accepts-missing-and-reordered-command-identities ()
  "A gapped journal does not make an unseen lower command ID durable."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-storage-gaps-" t))
         (store (e-session-persistent-index-store-create directory))
         (first (e-session-storage--create
                 :store store :instance-id "writer"))
         (second (e-session-storage--create
                  :store store :instance-id "writer" :next-sequence 2))
         (journal (expand-file-name "sessions/session-1.jsonl" directory)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory journal) t)
          (with-temp-file journal
            (insert "{\"type\":\"message\",\"writer-command-id\":\"writer:2\"}\n"))
          (e-session-storage--submit-record
           first "session-1" '(:type "message" :id "message-1"))
          (e-session-storage-test--await-command first)
          (e-session-storage--submit-record
           second "session-1" '(:type "message" :id "message-3"))
          (e-session-storage-test--await-command second)
          (with-temp-buffer
            (insert-file-contents journal)
            (should (equal (mapcar (lambda (line)
                                     (plist-get
                                      (json-parse-string line :object-type 'plist)
                                      :writer-command-id))
                                   (split-string (buffer-string) "\n" t))
                           '("writer:2" "writer:1" "writer:3")))))
      (dolist (controller (list first second))
        (when-let ((process (e-session-storage--controller-process controller)))
          (when (process-live-p process) (kill-process process))))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-rejects-truncated-journal-tail ()
  "A rejected tail leaves the journal unchanged and settles the command."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-storage-tail-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage--create :store store :instance-id "writer"))
         (journal (expand-file-name "sessions/session-1.jsonl" directory))
         (tail "{\"type\":\"message\",\"writer-command-id\":\"writer:2\"}\n{")
         failure)
    (unwind-protect
        (progn
          (make-directory (file-name-directory journal) t)
          (with-temp-file journal (insert tail))
          (e-session-storage--submit
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
                      (e-session-storage--controller-outbox controller))
                     0))
          (with-temp-buffer
            (insert-file-contents journal)
            (should (equal (buffer-string) tail))))
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-rejects-malformed-journal-tail ()
  "A malformed newline-terminated tail leaves the journal unchanged."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-storage-malformed-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage--create :store store :instance-id "writer"))
         (journal (expand-file-name "sessions/session-1.jsonl" directory))
         (tail "{\"type\":\"message\",\"writer-command-id\":\"writer:2\"}\nnot json\n")
         failure)
    (unwind-protect
        (progn
          (make-directory (file-name-directory journal) t)
          (with-temp-file journal (insert tail))
          (e-session-storage--submit
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
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-retries-keep-journal-order-and-deduplicate ()
  "Replayed writer commands append once and preserve session record order."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-storage-retry-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
         (sent nil))
    (unwind-protect
        (progn
          ;; Hold commands in the controller outbox, then deliver every one
          ;; twice as a process restart would.  The writer must retain only
          ;; the first delivery of each globally unique command id.
          (cl-letf (((symbol-function 'e-session-storage--send)
                     (lambda (_controller request) (push request sent))))
            (e-session-create store :id "session-1")
            (e-session-append-message
             store "session-1" '(:role user :content "ordered retry")))
          (dolist (request (nreverse sent))
            (e-session-storage--send controller request)
            (e-session-storage--send controller request))
          (e-session-storage-test--await-durable store)
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name "sessions/session-1.jsonl" directory))
            (should (= (count-lines (point-min) (point-max)) 2)))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (mapcar (lambda (message) (plist-get message :content))
                                   (e-session-aggregate-messages loaded "session-1"))
                           '("ordered retry")))))
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-writer-request-rejection-is-terminal ()
  "A deterministic writer protocol rejection settles instead of retrying forever."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-storage-reject-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
         (e-session-storage--unsettled-write-count 0)
         (e-session-storage--unsettled-generation 0)
         failure)
    (unwind-protect
        (progn
          (e-session-storage--submit
           controller (list :op "unsupported") nil
           (lambda (err) (setq failure err)))
          (let ((deadline (+ (float-time) 5.0)))
            (while (and (not failure) (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should failure)
          (should (= (hash-table-count
                      (e-session-storage--controller-outbox controller))
                     0))
          (should (= (e-session-storage--state-unsettled-write-count
                      (e-session-storage-test--state store)) 0))
          (should-not (e-session-storage--controller-retry-timer controller))
          (should (string-match-p
                   "Unsupported writer operation"
                   (plist-get (e-session-storage--status controller)
                              :last-error))))
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-checkpoint-indexes-board-identity ()
  "The writer checkpoints complete board routing policy through index/replay."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-board-index-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
         (routing-policy
          '(:participant-id "participant-1"
            :pickup-selector
            (:kind input :tags (private)
             :attributes (:symbol car :string "car"))
            :observer-selector (:subject-participant-id "participant-1")
            :default-tags (private)
            :default-to "participant-1")))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-storage-test--route-routing-policy routing-policy)
          (e-session-declare-board-state
           store "session-1" "principal:owner" "board-1"
           "participant" routing-policy)
          (e-session-storage-test--await-durable store)
          (let* ((indexed-store
                 (e-session-persistent-index-store-create directory))
                 (entry (car (e-session-aggregate-list indexed-store))))
            (should (equal (plist-get entry :id) "session-1"))
            (should (equal (plist-get entry :board-id) "board-1"))
            (should (equal (plist-get entry :principal) "principal:owner"))
            (should (equal (plist-get (plist-get entry :board-state)
                                      :association-role)
                           "participant"))
            (should (equal (plist-get (plist-get entry :board-state)
                                      :routing-policy)
                           routing-policy))
            (e-session-storage-test--route-routing-policy
             (plist-get (plist-get entry :board-state) :routing-policy))
            (should-not (plist-member entry :state)))
            (let* ((loaded (e-session-persistent-store-create directory))
                 (state (plist-get (e-session-aggregate-get loaded "session-1")
                                   :board-session-state)))
            (should (equal state
                           '(:board-id "board-1"
                             :principal "principal:owner"
                             :association-role "participant"
                             :routing-policy
                             (:participant-id "participant-1"
                              :pickup-selector
                              (:kind input :tags (private)
                               :attributes (:symbol car :string "car"))
                              :observer-selector
                              (:subject-participant-id "participant-1")
                              :default-tags (private)
                              :default-to "participant-1"))))
            (e-session-storage-test--route-routing-policy
             (e-session-aggregate-board-routing-policy
              (e-session-aggregate-get loaded "session-1")))))
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-checkpoint-folds-message-display-state ()
  "Writer compaction retains the message record behind a later display update."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-display-checkpoint-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-append-message
           store "session-1" '(:id "message-1" :role assistant :content "hi"))
          (e-session-set-message-display
           store "session-1" "message-1" 'hidden)
          (e-session-storage-test--await-durable store)
          (let* ((journal
                  (expand-file-name "sessions/session-1.jsonl" directory))
                 (checkpoint
                  (with-temp-buffer
                    (insert-file-contents
                     (e-session-storage--checkpoint-file store "session-1"))
                    (json-parse-string (buffer-string)
                                       :object-type 'plist
                                       :array-type 'list
                                       :null-object nil
                                       :false-object :json-false)))
                 (loaded (e-session-persistent-store-create directory))
                 (message (car (e-session-aggregate-messages loaded "session-1"))))
            (should (= (plist-get checkpoint :journal-byte-offset)
                       (file-attribute-size (file-attributes journal))))
            (should (equal (plist-get message :id) "message-1"))
            (should (eq (plist-get message :display) 'hidden))))
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-checkpoint-retains-typed-board-id-collisions ()
  "Async compaction preserves every typed board envelope sharing a raw id."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-board-checkpoint-collision-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
         (session-id "session-1"))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (dolist (envelope '((:id "shared" :kind output)
                              (:id "shared" :record-type processing-chain)
                              (:id "shared" :record-type "processing-chain")
                              (:id "shared" :record-type processing-result)))
            (e-session-append-board-message store session-id envelope))
          (e-session-storage-test--await-durable store)
            (let ((reopened (e-session-persistent-store-create directory)))
            (should (equal
                     (mapcar #'e-session-aggregate-board-message-identity
                             (e-session-aggregate-board-messages reopened session-id))
                     '((board-message . "shared")
                       (processing-chain . "shared")
                       (processing-result . "shared"))))))
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-checkpoint-retains-first-divergent-ordinary-board-envelope-after-clear ()
  "Async checkpoint replay keeps the first ordinary envelope in each clear domain."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-board-checkpoint-ordinary-clear-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
         (session-id "session-1"))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (e-session-append-board-message store session-id
                                          '(:id "shared" :kind before))
          (e-session-storage--submit-record
           controller session-id
           '(:type "board-message" :session-id "session-1"
             :message (:id "shared" :kind before-divergent)))
          (e-session-clear-board-messages store session-id)
          (e-session-append-board-message store session-id
                                          '(:id "shared" :kind after))
          (e-session-storage--submit-record
           controller session-id
           '(:type "board-message" :session-id "session-1"
             :message (:id "shared" :kind after-divergent)))
          (e-session-storage-test--await-durable store)
            (let ((reopened (e-session-persistent-store-create directory)))
            (should (equal (e-session-aggregate-board-messages reopened session-id)
                           '((:id "shared" :kind after :tags nil))))))
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-checkpoint-reuses-typed-board-ids-after-clear ()
  "Async compaction starts a new typed board identity domain after clear."
  (skip-unless (executable-find e-session-storage-node-executable))
  (let* ((directory (make-temp-file "e-session-board-checkpoint-clear-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-storage-enable store))
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
          (e-session-storage-test--await-durable store)
            (let ((reopened (e-session-persistent-store-create directory)))
            (should (equal (e-session-aggregate-board-messages reopened session-id)
                           '((:id "shared" :kind after :tags nil)
                             (:id "shared" :record-type processing-chain
                              :root-message-id "after" :tags nil)
                             (:id "shared" :record-type processing-result
                              :outcome after :tags nil))))))
      (when-let ((process (e-session-storage--controller-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-queued-record-criticality-covers-durable-record-types ()
  "Queued write criticality recognizes every persisted durable record type."
  (dolist (type '("session"
                  "session-info"
                  "message"
                  "activity-event"
                  "branch-summary"
                  "compaction"
                  "provider-anchor"
                  "process-report"
                  "context-generation"
                  "context-promotion"
                  "current-branch"
                  "messages-cleared"))
    (should (eq (e-session-storage--queued-record-criticality
                 (list :type type))
                'critical)))
  (should (eq (e-session-storage--queued-record-criticality
               '(:type "derived-preview"))
              'derived)))

(ert-deftest e-session-storage-test-flush-write-queue-drops-stale-generation-records ()
  "Flushing a queued store ignores records from stale queue generations."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued)))
    (unwind-protect
        (let* ((session (e-session-create store :id "stale-queued"))
               (session-id (plist-get session :id)))
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "stale queued"))
          (cl-incf (e-session-storage--state-write-queue-generation
                    (e-session-storage-test--state store)))
          (e-session-storage-flush-write-queue store)
          (let ((loaded (e-session-persistent-index-store-create directory)))
            (should-not
             (cl-find session-id (e-session-aggregate-list loaded)
                      :key (lambda (entry) (plist-get entry :id))
                      :test #'equal))))
      (ignore-errors (e-session-storage-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-flush-write-queue-recovers-stale-derived-index ()
  "Flushing current records rebuilds a stale derived index write."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued)))
    (unwind-protect
        (let* ((session (e-session-create store :id "recover-index"))
               (session-id (plist-get session :id))
               (stale-index
                (copy-sequence
                 (e-session-storage--state-index-write-pending
                  (e-session-storage-test--state store)))))
          (plist-put stale-index
                     :generation
                     (1- (e-session-storage--state-write-queue-generation
                          (e-session-storage-test--state store))))
          (setf (e-session-storage--state-index-write-pending
                 (e-session-storage-test--state store)) stale-index)
          (e-session-storage-flush-write-queue store)
          (let* ((loaded (e-session-persistent-index-store-create directory))
                 (entry (cl-find session-id
                                 (e-session-aggregate-list loaded)
                                 :key (lambda (entry) (plist-get entry :id))
                                 :test #'equal)))
            (should entry)
            (should-not (plist-get entry :loaded))))
      (ignore-errors (e-session-storage-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-flush-write-queue-orders-critical-before-derived-records ()
  "Queued flush writes critical records before non-critical derived records."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued))
         order)
    (unwind-protect
        (let* ((session-entry
                (e-session-storage--queued-write-entry
                 store "ordered-critical" '(:type "session")))
               (derived-entry
                (e-session-storage--queued-write-entry
                 store "ordered-critical" '(:type "derived-preview")))
               (message-entry
                (e-session-storage--queued-write-entry
                 store "ordered-critical" '(:type "message"))))
          (setf (e-session-storage--state-write-queue
                 (e-session-storage-test--state store))
                (list message-entry derived-entry session-entry))
          (e-session-storage--adjust-unsettled-writes store 3)
          (cl-letf (((symbol-function 'e-session-storage--append-record-now)
                     (lambda (_store _session-id record)
                       (push (plist-get record :type) order)))
                    ((symbol-function 'e-session-storage--write-index-now)
                     (lambda (_store)
                       (push 'index order))))
            (e-session-storage-flush-write-queue store))
          (should (equal (nreverse order)
                         '("session" "message" "derived-preview")))
          (should-not (e-session-storage--state-write-queue
                       (e-session-storage-test--state store))))
      (ignore-errors (e-session-storage-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-flush-write-queue-writes-critical-records-before-index ()
  "Queued flush writes critical session records before derived index state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued))
         order)
    (unwind-protect
        (let* ((session (e-session-create store :id "critical-before-index"))
               (session-id (plist-get session :id)))
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "durable before index"))
          (cl-letf (((symbol-function 'e-session-storage--append-record-now)
                     (lambda (_store _session-id record)
                       (push (plist-get record :type) order)))
                    ((symbol-function 'e-session-storage--write-index-now)
                     (lambda (_store)
                       (push 'index order))))
            (e-session-storage-flush-write-queue store))
          (should (equal (nreverse order)
                         '("session" "message" index)))
          (should-not (e-session-storage--state-write-queue
                       (e-session-storage-test--state store)))
          (should-not (e-session-storage--state-index-write-pending
                       (e-session-storage-test--state store))))
      (ignore-errors (e-session-storage-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-flush-write-queue-retries-only-unacknowledged-records ()
  "Queued flush preserves unacknowledged records after a partial write failure."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued))
         (message-failed nil)
         writes)
    (unwind-protect
        (let* ((session (e-session-create store :id "partial-flush"))
               (session-id (plist-get session :id)))
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "retry once"))
          (cl-letf (((symbol-function 'e-session-storage--append-record-now)
                     (lambda (_store _session-id record)
                       (let ((type (plist-get record :type)))
                         (when (and (equal type "message")
                                    (not message-failed))
                           (setq message-failed t)
                           (error "simulated record write failure"))
                         (push type writes))))
                    ((symbol-function 'e-session-storage--write-index-now)
                     (lambda (_store)
                       (push 'index writes))))
            (should-error (e-session-storage-flush-write-queue store)
                          :type 'error)
            (should (equal (nreverse (copy-sequence writes))
                           '("session")))
            (should (= (length (e-session-storage--state-write-queue
                               (e-session-storage-test--state store))) 1))
            (should (equal
                     (plist-get
                      (e-session-storage--queued-entry-record
                       (car (e-session-storage--state-write-queue
                             (e-session-storage-test--state store))))
                      :type)
                     "message"))
            (should (e-session-storage--state-index-write-pending
                     (e-session-storage-test--state store)))
            (e-session-storage-flush-write-queue store)
            (should (equal (nreverse writes)
                           '("session" "message" index)))
            (should-not (e-session-storage--state-write-queue
                         (e-session-storage-test--state store)))
            (should-not (e-session-storage--state-index-write-pending
                         (e-session-storage-test--state store))))
      (ignore-errors (e-session-storage-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-flush-write-queue-settles-post-write-failure ()
  "A record written before its error is not appended a second time."
  (let* ((directory (make-temp-file "e-session-post-write-" t))
         (store (e-session-persistent-index-store-create
                 directory :write-mode 'queued))
         (original-append (symbol-function 'e-session-storage--append-record-now))
         (failed nil))
    (unwind-protect
        (let* ((session (e-session-create store :id "post-write"))
               (session-id (plist-get session :id)))
          (e-session-append-board-message
           store session-id
           '(:id "chain" :record-type processing-chain :created-at "fixed"))
          (cl-letf (((symbol-function 'e-session-storage--append-record-now)
                     (lambda (append-store append-session-id record)
                       (funcall original-append append-store append-session-id record)
                       (when (and (equal (plist-get record :type) "board-message")
                                  (not failed))
                         (setq failed t)
                         (error "simulated post-write failure")))))
            (e-session-storage-flush-write-queue store))
          (should failed)
          (should-not (e-session-storage--state-write-queue
                       (e-session-storage-test--state store)))
            (let ((reopened (e-session-persistent-store-create directory)))
            (should (equal (mapcar #'e-session-aggregate-board-message-identity
                                   (e-session-aggregate-board-messages reopened session-id))
                           '((processing-chain . "chain"))))))
      (ignore-errors (e-session-storage-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-storage-test-flush-write-queue-retries-rebuilt-stale-index ()
  "A rebuilt stale derived index remains pending when index persistence fails."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued))
         (index-failed nil)
         writes)
    (unwind-protect
        (let* ((session (e-session-create store :id "retry-stale-index"))
               (session-id (plist-get session :id))
               (stale-index (copy-sequence
                             (e-session-storage--state-index-write-pending
                             (e-session-storage-test--state store))))
          )
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "retry index"))
          (plist-put stale-index
                     :generation
                     (1- (e-session-storage--state-write-queue-generation
                          (e-session-storage-test--state store))))
          (setf (e-session-storage--state-index-write-pending
                 (e-session-storage-test--state store)) stale-index)
          (cl-letf (((symbol-function 'e-session-storage--append-record-now)
                     (lambda (_store _session-id record)
                       (push (plist-get record :type) writes)))
                    ((symbol-function 'e-session-storage--write-index-now)
                     (lambda (_store)
                       (if index-failed
                           (push 'index writes)
                         (setq index-failed t)
                         (error "simulated index write failure")))))
            (should-error (e-session-storage-flush-write-queue store)
                          :type 'error)
            (should-not (e-session-storage--state-write-queue
                         (e-session-storage-test--state store)))
            (should (e-session-storage--state-index-write-pending
                     (e-session-storage-test--state store)))
            (should (e-session-storage--queued-index-current-p
                     store
                     (e-session-storage--state-index-write-pending
                      (e-session-storage-test--state store))))
            (e-session-storage-flush-write-queue store)
            (should (equal (nreverse writes)
                           '("session" "message" index)))
            (should-not (e-session-storage--state-write-queue
                         (e-session-storage-test--state store)))
            (should-not (e-session-storage--state-index-write-pending
                         (e-session-storage-test--state store))))))
      (ignore-errors (e-session-storage-flush-write-queue store))
      (delete-directory directory t))))

(provide 'e-session-storage-test)

;;; e-session-storage-test.el ends here
