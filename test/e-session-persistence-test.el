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
          (should (= (plist-get (e-session-persistence-unsettled-state) :writes)
                     1))
          (let ((request (e-session-persistence-command-request (car sent))))
            (e-session-persistence--handle-response
             controller (list :id (plist-get request :id) :ok t)))
          (should (= (plist-get (e-session-persistence-unsettled-state) :writes)
                     0)))
      (delete-directory directory t))))

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
