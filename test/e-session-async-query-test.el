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
        (funcall (plist-get call :on-settle)
                 (list :detached (vector (list :value "ok"))) nil))
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

(ert-deftest e-session-async-query-test-page-overlays-inflight-association ()
  "A page observes admitted Board state without waiting for its COMMIT."
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
              ;; SQLite answered before either transaction.  The request-local
              ;; result overlays only the two already-admitted optimistic
              ;; commands; it does not retain their derived row globally.
              (funcall
               (plist-get (car calls) :on-settle)
               '(:rows nil :next nil :limit 8
                 :byte-count 0 :byte-limit 1048576)
               nil)
              (let* ((page (plist-get (e-work-status page-work) :result))
                     (rows (plist-get page :rows)))
                (should (eq (plist-get (e-work-status page-work) :state)
                            'finished))
                (should (= (length rows) 1))
                (should (equal (plist-get (car rows) :session-id) "daily"))
                (should (equal (plist-get (car rows) :board-id)
                               "board:daily"))
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

(ert-deftest e-session-async-query-test-context-cut-keeps-acknowledged-mutation ()
  "A prefetch retains a later admitted write until its result is consumed."
  (e-session-async-query-test--with-held-reads (calls)
    (let* ((store (e-session-store-create))
           (state (e-session-async--state store))
           (work (e-session-async-context-path-base store "session"))
           (command
            (e-session-aggregate-command-prepare
             'append-message "session"
             '(:message (:role user :content "new prompt"))))
           (operation
            (e-session-async--operation-create
             :state state :session-id "session" :command command)))
      ;; The SELECT entered the transport before this mutation.  Simulate the
      ;; write acknowledgement retiring its ordinary pending intent before the
      ;; already-finished query work is consumed.
      (e-session-async--add-pending operation)
      (funcall
       (plist-get (car calls) :on-settle)
       '(:session-id "session" :current-head-id "root"
         :current-head-path-index 0 :messages nil
         :message-path-indexes nil :context-records nil)
       nil)
      (setf (e-session-async--operation-settled operation) t)
      (e-session-async--remove-pending operation)
      (let* ((path (plist-get (e-work-status work) :result))
             (effective
              (e-session-async-context-path-overlay-pending
               store "session" path)))
        (let ((message (car (plist-get effective :messages))))
          (should (eq (plist-get message :type) 'message))
          (should (equal (plist-get message :role) 'user))
          (should (equal (plist-get message :content) "new prompt"))
          (should (equal (plist-get message :parent-id) "root"))
          (should (equal (plist-get message :id)
                         (e-session-aggregate-command-delta-id command))))
        (should (equal (plist-get effective :message-path-indexes) '(1)))
        (should (equal (plist-get effective :current-head-id)
                       (e-session-aggregate-command-delta-id command)))
        (should (= (hash-table-count
                    (e-session-async--state-context-query-cuts state))
                   0))))))

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

(ert-deftest e-session-async-query-test-chat-view-composes-three-bounded-reads ()
  "Persistent chat view starts three reads and settles one detached result."
  (e-session-async-query-test--with-held-reads (calls)
    (let* ((store (e-session-store-create))
           (work (e-session-async-chat-view store "daily" :limit 2)))
      (should (= (hash-table-count (e-session-store-sessions store)) 0))
      (should (= (length calls) 3))
      (should (eq (plist-get (e-work-status work) :state) 'started))
      (dolist (call calls)
        (let ((request (plist-get call :request)))
          (pcase (plist-get request :op)
            ('session-metadata
             (funcall (plist-get call :on-settle)
                      '(:session-id "daily" :name "Daily") nil))
            ('session-board-association
             (funcall (plist-get call :on-settle)
                      '(:session-id "daily" :board-id "board") nil))
            ('session-visible-message-page
             (funcall (plist-get call :on-settle)
                      '(:session-id "daily" :messages
                        ((:id "m1" :role user :content "hello")
                         (:id "m2" :role assistant :content "hi"))
                        :truncated nil)
                      nil)))))
      (should (eq (plist-get (e-work-status work) :state) 'finished))
      (should (equal (plist-get (e-work-status work) :result)
                     '(:session-id "daily"
                       :metadata (:session-id "daily" :name "Daily")
                       :association (:session-id "daily" :board-id "board")
                       :messages ((:id "m1" :role user :content "hello")
                                  (:id "m2" :role assistant :content "hi")
                                  )
                       :truncated nil))))))

(ert-deftest e-session-async-query-test-chat-view-failure-is-request-local ()
  "A failed child read fails the composition without domain state."
  (e-session-async-query-test--with-held-reads (calls)
    (let* ((store (e-session-store-create))
           (work (e-session-async-chat-view store "daily" :limit 2))
           cancelled)
      (cl-letf (((symbol-function 'e-session-storage-cancel-operation)
                 (lambda (_store operation)
                   (push operation cancelled)
                   'dropped)))
        (funcall (plist-get (nth 1 calls) :on-settle)
                 nil '(e-session-storage-error "association failed")))
      (should (eq (plist-get (e-work-status work) :state) 'failed))
      (should (equal (car (plist-get (e-work-status work) :error))
                     'e-session-storage-error))
      (should (= (hash-table-count (e-session-store-sessions store)) 0))
      (should (= (length cancelled) 2)))))

(provide 'e-session-async-query-test)

;;; e-session-async-query-test.el ends here
