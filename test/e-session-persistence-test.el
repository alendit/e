;;; e-session-persistence-test.el --- Tests for async session persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-session)
(require 'e-session-persistence)

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
          (let ((request (car sent)))
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

(ert-deftest e-session-persistence-test-stale-catalog-does-not-hide-new-journal ()
  "Startup supplements a catalog snapshot with journal roots it does not contain."
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
            (should (e-session-get indexed "unindexed"))))
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

(ert-deftest e-session-persistence-test-catalog-port-returns-current-board-rows ()
  "The writer serves bounded preflight pages without transcript reads in Emacs."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-catalog-port-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         result failure)
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-persistence-declare-board-state
           controller "session-1" "principal:owner")
          (e-session-persistence-test--await-durable store)
          (e-session-persistence-catalog-request
           controller '(:operation preflight-page :limit 1)
           (lambda (value) (setq result value))
           (lambda (err) (setq failure err)))
          (while (and (not result) (not failure))
            (accept-process-output (e-session-persistence-process controller) 0.05))
          (should-not failure)
          (let ((row (car (plist-get result :sessions))))
            (should (equal (plist-get row :session-id) "session-1"))
            (should (eq (plist-get row :state) 'dormant))
            (should (equal (plist-get (plist-get row :access-record) :controller)
                           "principal:owner"))
            (should (= (plist-get row :board-output-sequence) 0))
            (should (= (plist-get row :board-activity-sequence) 0))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(provide 'e-session-persistence-test)

;;; e-session-persistence-test.el ends here
