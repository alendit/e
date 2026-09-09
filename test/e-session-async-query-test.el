;;; e-session-async-query-test.el --- Asynchronous session read port tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-session-aggregate)
(require 'e-session-async)
(require 'e-request)
(require 'e-work)

(cl-defmacro e-session-async-query-test--with-held-reads
    ((calls) &rest body)
  "Run BODY with asynchronous storage reads retained in CALLS."
  (declare (indent 1) (debug ((symbolp) body)))
  `(let (,calls)
     (cl-letf (((symbol-function 'e-session-storage-submit)
                (lambda (_store kind request on-settle &optional _escrow)
                  (setq ,calls
                        (append ,calls
                                (list (list :kind kind :request request
                                            :on-settle on-settle))))
                  (list :storage-read (length ,calls))))
               ((symbol-function 'e-runtime-store-call)
                (lambda (&rest _)
                  (error "The asynchronous read port called sync storage")))
               ((symbol-function 'e-runtime-store-await)
                (lambda (&rest _)
                  (error "The asynchronous read port awaited storage"))))
       ,@body)))

(ert-deftest e-session-async-query-test-returns-immediate-read-work ()
  "All application reads enqueue through storage and return before settlement."
  (e-session-async-query-test--with-held-reads (calls)
    (let* ((store (e-session-store-create))
           (query (e-session-async-query-state store "session"))
           (metadata (e-session-async-session-metadata store "session"))
           (association (e-session-async-board-association store "session"))
           (page (e-session-async-query-page
                  store :limit 7 :root-p t
                  :cursor '(:updated-at "2026-09-06T00:00:00Z"
                            :session-id "cursor")))
           (context-path (e-session-async-context-path store "session"))
           (visible (e-session-async-visible-message-page store "session" 3))
           (works (list query metadata association page context-path visible)))
      (should (= (hash-table-count (e-session-store-sessions store)) 0))
      (should (= (length calls) (length works)))
      (dolist (work works)
        (should (e-work-handle-p work))
        (should-not (e-request-terminal-p (e-work-handle-lifecycle work)))
        (should (eq (plist-get (e-work-status work) :state) 'started)))
      (should (equal (mapcar (lambda (call) (plist-get call :kind)) calls)
                     (make-list (length calls) 'read)))
      (should (equal (plist-get (plist-get (nth 0 calls) :request) :op)
                     'session-query-state))
      (should (equal (plist-get (plist-get (nth 3 calls) :request) :op)
                     'session-query-page))
      (should (= (plist-get (plist-get (nth 3 calls) :request) :limit) 7))
      (should (plist-get (plist-get (nth 3 calls) :request) :root-p))
      (should (equal (plist-get (plist-get (nth 4 calls) :request) :op)
                     'session-context-path))
      (should (= (plist-get (plist-get (nth 5 calls) :request) :limit) 3))
      (dolist (call calls)
        (funcall
         (plist-get call :on-settle)
         (if (eq (plist-get (plist-get call :request) :op)
                 'session-query-page)
             '(:rows nil :next nil :limit 7
               :byte-count nil :byte-limit nil)
           (list :detached (vector (list :value "ok"))))
         nil))
      (cl-loop
       for work in works
       for index from 0
       do
       (should (eq (plist-get (e-work-status work) :state) 'finished))
       (if (= index 3)
           (should (equal (plist-get (e-work-status work) :result)
                          '(:rows nil :next nil :limit 7
                            :byte-count nil :byte-limit nil)))
         (should (equal (plist-get (e-work-status work) :result)
                        (list :detached (vector (list :value "ok")))))))
      (should (= (hash-table-count (e-session-store-sessions store)) 0)))))

(ert-deftest e-session-async-query-test-read-results-are-detached ()
  "A vector/list response supplied by storage is detached at the app port."
  (e-session-async-query-test--with-held-reads (calls)
    (let ((work (e-session-async-query-state 'store "session"))
          (value (vector (list :value "before"))))
      (funcall (plist-get (car calls) :on-settle)
               (list :value value) nil)
      (aset value 0 (list :value "after"))
      (should (equal (plist-get (e-work-status work) :result)
                     (list :value (vector (list :value "before"))))))))

(ert-deftest e-session-async-query-test-page-snapshot-does-not-overlay-inflight-association ()
  "A page follows its database snapshot, not Emacs admission timing."
  (e-session-async-query-test--with-held-reads (calls)
    (let* ((store (e-session-store-create))
           (state (e-session-async--state store))
           (create
            (e-session-aggregate-command-prepare
             'create "daily" '(:metadata (:name "Daily"))))
           (association
            (e-session-aggregate-command-prepare
             'board-state "daily"
             '(:principal "chat:daily" :board-id "board:daily")))
           (create-operation
            (e-session-async--operation-create
             :state state :session-id "daily" :command create))
           (association-operation
            (e-session-async--operation-create
             :state state :session-id "daily" :command association))
           (_create-work
            (e-session-async--start-work "daily" create-operation))
           (_association-work
            (e-session-async--start-work "daily" association-operation)))
      (unwind-protect
          (progn
            (e-session-async--add-pending create-operation)
            (e-session-async--add-pending association-operation)
            (let ((page-work
                   (e-session-async-query-page
                    store :limit 8 :root-p t :board-id "board:daily")))
              ;; SQLite answered before either transaction.  The two admitted
              ;; Emacs intents are not evidence that either row is committed.
              (funcall
               (plist-get (car calls) :on-settle)
               '(:rows nil :next nil :limit 8
                 :byte-count 0 :byte-limit 1048576)
               nil)
              (let* ((page (plist-get (e-work-status page-work) :result))
                     (rows (plist-get page :rows)))
                (should (eq (plist-get (e-work-status page-work) :state)
                            'finished))
                (should-not rows)
                (should (= (hash-table-count
                            (e-session-store-sessions store))
                           0)))))
        (e-session-async-reset store)))))

(ert-deftest e-session-async-query-test-read-failure-settles-work ()
  "A storage read failure settles only its request-scoped work handle."
  (e-session-async-query-test--with-held-reads (calls)
    (let ((work (e-session-async-session-metadata 'store "session")))
      (funcall (plist-get (car calls) :on-settle)
               nil '(e-session-storage-error "read failed"))
      (should (eq (plist-get (e-work-status work) :state) 'failed))
      (should (equal (car (plist-get (e-work-status work) :error))
                     'e-session-storage-error)))))

(ert-deftest e-session-async-query-test-context-snapshot-before-commit-does-not-overlay-delayed-ack ()
  "A context query excludes a mutation committed after its SQLite snapshot."
  (e-session-async-query-test--with-held-reads (calls)
    (let* ((store (e-session-store-create))
           (state (e-session-async--state store))
           (work (e-session-async-context-path store "session"))
           (command
            (e-session-aggregate-command-prepare
             'append-message "session"
             '(:message (:role user :content "new prompt"))))
           (operation
            (e-session-async--operation-create
             :state state :session-id "session" :command command)))
      ;; The SELECT entered the database before this mutation committed.  Its
      ;; later acknowledgement cannot change the already-issued snapshot.
      (e-session-async--add-pending operation)
      (funcall
       (plist-get (car calls) :on-settle)
       '(:session-id "session" :current-head-id "root"
         :current-head-path-index 0 :messages nil
         :message-path-indexes nil :context-records nil)
       nil)
      (setf (e-session-async--operation-settled operation) t)
      (e-session-async--remove-pending operation)
      (let ((path (plist-get (e-work-status work) :result)))
        (should-not (plist-get path :messages))
        (should (equal (plist-get path :current-head-id) "root"))))))

(ert-deftest e-session-async-query-test-context-commit-before-snapshot-is-not-duplicated-by-delayed-ack ()
  "A committed context row appears once despite a still-pending local write."
  (e-session-async-query-test--with-held-reads (calls)
    (let* ((store (e-session-store-create))
           (state (e-session-async--state store))
           (message '(:id "committed" :role user :content "prompt"))
           (command
            (e-session-aggregate-command-prepare
             'append-message "daily" (list :message message)))
           (operation
            (e-session-async--operation-create
             :state state :session-id "daily" :command command)))
      (e-session-async--start-work "daily" operation)
      (e-session-async--add-pending operation)
      (let ((work (e-session-async-context-path store "daily")))
        ;; SQLite includes the committed row before the application receives
        ;; the write acknowledgement.  Pending state must not duplicate it.
        (funcall
         (plist-get (car calls) :on-settle)
         (list :session-id "daily" :current-head-id "committed"
               :current-head-path-index 1 :messages (list message)
               :message-path-indexes '(1) :context-records nil
               :high-water 2)
         nil)
        (let ((path (plist-get (e-work-status work) :result)))
          (should (= (length (plist-get path :messages)) 1))
          (should (equal (plist-get (car (plist-get path :messages)) :id)
                         "committed")))))))

(ert-deftest e-session-async-query-test-cancellation-uses-opaque-port ()
  "Cancelling a read delegates to the storage port and settles the work."
  (e-session-async-query-test--with-held-reads (calls)
    (let (cancelled
          (work (e-session-async-visible-message-page 'store "session" 4)))
      (cl-letf (((symbol-function 'e-session-storage-cancel-operation)
                 (lambda (_store operation)
                   (setq cancelled operation)
                   'dropped)))
        (e-work-cancel work))
      (should (equal cancelled '(:storage-read 1)))
      (should (eq (plist-get (e-work-status work) :state) 'cancelled)))))

(ert-deftest e-session-async-query-test-synchronous-settlement-does-not-enroll-stale-operation ()
  "A callback invoked before submit returns cannot leave stale cancellation state."
  (let (cancelled operation)
    (cl-letf (((symbol-function 'e-session-storage-submit)
               (lambda (_store _kind _request on-settle &optional _escrow)
                 (funcall on-settle '(:value "ready") nil)
                 '(:late-storage-read 1)))
              ((symbol-function 'e-session-storage-cancel-operation)
               (lambda (_store pending)
                 (setq cancelled pending)
                 'dropped)))
      (let ((work (e-session-async-query-state 'store "session")))
        (setq operation (e-work-handle-arguments work))
        (should (eq (plist-get (e-work-status work) :state) 'finished))
        (should (equal (plist-get (e-work-status work) :result)
                       '(:value "ready")))))
    (should-not (e-session-async--read-operation-storage-operation operation))
    ;; Terminal work cannot be cancelled, and must not expose the late opaque
    ;; adapter operation to a cancellation path.
    (e-work-cancel (e-session-async--read-operation-work operation))
    (should-not cancelled)))

(ert-deftest e-session-async-query-test-chat-view-uses-one-database-snapshot ()
  "Persistent chat view is one consumer-shaped read with one change cursor."
  (e-session-async-query-test--with-held-reads (calls)
    (let* ((store (e-session-store-create))
           (work (e-session-async-chat-view store "daily" :limit 2)))
      (should (= (hash-table-count (e-session-store-sessions store)) 0))
      (should (= (length calls) 1))
      (should (eq (plist-get (e-work-status work) :state) 'started))
      (should (equal (plist-get (plist-get (car calls) :request) :op)
                     'chat-session-view))
      (funcall
       (plist-get (car calls) :on-settle)
       '(:session-id "daily"
         :metadata (:session-id "daily" :name "Daily")
         :association (:session-id "daily" :board-id "board")
         :messages ((:id "m1" :role user :content "hello")
                    (:id "m2" :role assistant :content "hi"))
         :cursor 2 :through 2 :truncated nil)
       nil)
      (should (eq (plist-get (e-work-status work) :state) 'finished))
      (should (equal (plist-get (e-work-status work) :result)
                     '(:session-id "daily"
                       :metadata (:session-id "daily" :name "Daily")
                       :association (:session-id "daily" :board-id "board")
                       :messages ((:id "m1" :role user :content "hello")
                                  (:id "m2" :role assistant :content "hi"))
                       :cursor 2 :through 2 :truncated nil))))))

(ert-deftest e-session-async-query-test-chat-view-failure-is-request-local ()
  "A failed snapshot fails only its request without domain state."
  (e-session-async-query-test--with-held-reads (calls)
    (let* ((store (e-session-store-create))
           (work (e-session-async-chat-view store "daily" :limit 2))
           cancelled)
      (cl-letf (((symbol-function 'e-session-storage-cancel-operation)
                 (lambda (_store operation)
                   (push operation cancelled)
                   'dropped)))
        (funcall (plist-get (car calls) :on-settle)
                 nil '(e-session-storage-error "association failed")))
      (should (eq (plist-get (e-work-status work) :state) 'failed))
      (should (equal (car (plist-get (e-work-status work) :error))
                     'e-session-storage-error))
      (should (= (hash-table-count (e-session-store-sessions store)) 0))
      (should-not cancelled))))

(provide 'e-session-async-query-test)

;;; e-session-async-query-test.el ends here
