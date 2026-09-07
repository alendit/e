;;; e-chat-daily-query-test.el --- Persistent Daily query presentation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-chat)
(require 'e-chat-composer)
(require 'e-chat-service)
(require 'e-session-async)
(require 'e-session-storage)
(require 'e-work)

(defconst e-chat-daily-query-test--spec
  (e-work-spec-create
   :id "chat-daily-query-test" :execution 'cooperative
   :interactive-policy 'async :owner 'e-chat-daily-query-test
   :runner (lambda (_handle _arguments _context) :deferred)))

(ert-deftest e-chat-daily-query-test-existing-persistent-open-is-immediate ()
  "Existing SQLite attach starts detached reads without domain composition."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         view-work
         buffer
         (forbidden '(e-session-get e-session-messages e-session-load-session
                      e-session-load-session-start e-chat-service-ensure-binding
                      e-chat--ensure-session
                      e-harness-session-title))
         original-functions
         (e-session-async-chat-view
          (lambda (_store _session-id &key _limit)
            (setq view-work
                  (e-work-start e-chat-daily-query-test--spec nil))))
         (e-session-storage-sqlite-p (lambda (_store) t)))
    (dolist (name forbidden)
      (push (cons name (symbol-function name)) original-functions))
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-sqlite-p)
                   e-session-storage-sqlite-p)
                  ((symbol-function 'e-session-async-chat-view)
                   e-session-async-chat-view))
          (dolist (name forbidden)
            (fset name
                  (lambda (&rest _arguments)
                    (error "Persistent Daily open called forbidden %S" name))))
          (setq buffer (e-chat-open :harness harness :session-id "daily"))
          (with-current-buffer buffer
            (should (buffer-live-p buffer))
            (should (e-work-handle-p e-chat--session-query-work))
            (should (eq (plist-get (e-work-status e-chat--session-query-work)
                                   :state)
                        'started))
            (should (buffer-live-p (e-chat-surface-composer-buffer)))
            (should (= (hash-table-count (e-session-store-sessions store)) 0))
            (should (string-match-p "Loading recent messages"
                                    (buffer-string))))
          (e-work-finish
           view-work
           '(:session-id "daily"
             :metadata (:session-id "daily" :name "Daily")
             :association (:session-id "daily" :board-id "board")
             :messages ((:id "m1" :role user :content "hello")
                        (:id "m2" :role assistant :content "world"))))
          (with-current-buffer buffer
            (should-not e-chat--session-query-work)
            (should (= (let ((count 0) (start (point-min)))
                         (while (string-match "hello" (buffer-string) start)
                           (setq count (1+ count)
                                 start (match-end 0)))
                         count)
                       1))
            (should (string-match-p "world" (buffer-string)))
            (should (equal e-chat-board-id "board"))))
      (dolist (entry original-functions)
        (fset (car entry) (cdr entry)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-daily-query-test-stale-view-result-is-presentation-fenced ()
  "A superseded detached view result cannot render into the current buffer."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         works buffer
         (e-session-storage-sqlite-p (lambda (_store) t))
         (e-session-async-chat-view
          (lambda (_store _session-id &key _limit)
            (let ((work (e-work-start e-chat-daily-query-test--spec nil)))
              (setq works (append works (list work)))
              work))))
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-sqlite-p)
                   e-session-storage-sqlite-p)
                  ((symbol-function 'e-session-async-chat-view)
                   e-session-async-chat-view))
          (setq buffer (e-chat-open :harness harness :session-id "daily-stale"))
          (with-current-buffer buffer
            ;; Supersede the first presentation request without touching any
            ;; durable owner state.  The old child remains capable of settling
            ;; and is fenced only by request identity/generation.
            (setq e-chat--session-query-generation
                  (1+ e-chat--session-query-generation))
            (e-chat--clear-query-view)
            (e-chat--start-session-query-view
             buffer harness "daily-stale" e-chat--session-query-generation))
          (should (= (length works) 2))
          (e-work-finish
           (car works)
           '(:session-id "daily-stale"
             :metadata (:session-id "daily-stale" :name "Old")
             :association (:session-id "daily-stale" :board-id "old-board")
             :messages ((:id "old" :role user :content "stale result"))))
          (with-current-buffer buffer
            (should (eq e-chat--session-query-work (cadr works)))
            (should-not (string-match-p "stale result" (buffer-string))))
          (e-work-finish
           (cadr works)
           '(:session-id "daily-stale"
             :metadata (:session-id "daily-stale" :name "Current")
             :association (:session-id "daily-stale" :board-id "current-board")
             :messages ((:id "current" :role user :content "current result"))))
          (with-current-buffer buffer
            (should-not e-chat--session-query-work)
            (should (= (let ((count 0) (start (point-min)))
                         (while (string-match "current result" (buffer-string) start)
                           (setq count (1+ count)
                                 start (match-end 0)))
                         count)
                       1))
            (should-not (string-match-p "stale result" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-daily-query-test-failed-view-is-visible-and-retryable ()
  "A request-local query failure is visible and can be retried."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         works buffer
         (e-session-storage-sqlite-p (lambda (_store) t))
         (e-session-async-chat-view
          (lambda (_store _session-id &key _limit)
            (let ((work (e-work-start e-chat-daily-query-test--spec nil)))
              (setq works (append works (list work)))
              work))))
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-sqlite-p)
                   e-session-storage-sqlite-p)
                  ((symbol-function 'e-session-async-chat-view)
                   e-session-async-chat-view))
          (setq buffer (e-chat-open :harness harness :session-id "daily-retry"))
          (e-work-fail (car works) '(e-session-storage-error "temporary read failure"))
          (with-current-buffer buffer
            (should-not e-chat--session-query-work)
            (should (string-match-p "retry available"
                                    (e-chat-surface-status buffer)))
            (e-chat-retry-session-view)
            (should (eq e-chat--session-query-work (cadr works))))
          (e-work-finish
           (cadr works)
           '(:session-id "daily-retry"
             :metadata (:session-id "daily-retry" :name "Retry")
             :association (:session-id "daily-retry" :board-id nil)
             :messages ((:id "retry" :role assistant :content "retry succeeded"))))
          (with-current-buffer buffer
            (should-not e-chat--session-query-work)
            (should (string-match-p "retry succeeded" (buffer-string)))
            (should (= (hash-table-count (e-session-store-sessions store)) 0))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-daily-query-test)

;;; e-chat-daily-query-test.el ends here
