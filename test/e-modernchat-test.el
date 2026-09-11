;;; e-modernchat-test.el --- SQL-backed modern chat shell tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the detached, bounded modern chat presentation.  Durable chat
;; facts are supplied as SQLite-shaped query results; this file deliberately
;; owns no aggregate Board fixture.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-backend)
(require 'e-chat-service)
(require 'e-harness)
(require 'e-modernchat)
(require 'e-modernchat-view-model)
(require 'e-session-async)
(require 'e-session-sqlite)
(require 'e-session-storage)
(require 'e-work)

(defconst e-modernchat-test--pending-spec
  (e-work-spec-create
   :id "modernchat-test-pending"
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'e-modernchat-test
   :runner (lambda (&rest _arguments) :deferred)))

(defun e-modernchat-test--pending-work ()
  "Return one started work handle settled explicitly by its test."
  (e-work-start e-modernchat-test--pending-spec nil))

(defun e-modernchat-test--finished-work (result)
  "Return one work handle already finished with RESULT."
  (let ((work (e-modernchat-test--pending-work)))
    (e-work-finish work result)
    work))

(defun e-modernchat-test--await (work)
  "Observe request-scoped WORK at this explicit test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 5.0)))

(defun e-modernchat-test--wait-until (predicate &optional timeout)
  "Wait up to TIMEOUT seconds for PREDICATE while pumping async callbacks."
  (let ((deadline (+ (float-time) (or timeout 3.0))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defun e-modernchat-test--harness ()
  "Return a minimal harness for detached presentation tests."
  (e-harness-create
   :backend (e-backend-create :name "noop")
   :enabled-layer-ids nil))

(ert-deftest e-modernchat-test-send-surfaces-admission-failure-and-cancel ()
  "Modern chat retains one bounded visible admission terminal activity."
  (dolist (case '((failed . input-admission-failed)
                  (cancelled . input-admission-cancelled)))
    (with-temp-buffer
      (setq-local e-modernchat-harness 'harness)
      (setq-local e-modernchat-session-id "session")
      (let ((work (e-modernchat-test--pending-work))
            scheduled)
        (cl-letf (((symbol-function 'e-chat-service-submit-session)
                   (lambda (&rest _arguments) work))
                  ((symbol-function 'e-modernchat--schedule-push)
                   (lambda (&optional _buffer) (setq scheduled t))))
          (e-modernchat--handle-ui-action
           '((action . "send-message") (text . "hello")))
          (pcase (car case)
            ('failed
             (e-work-fail work '(e-board-sqlite-error "write rejected")))
            ('cancelled (e-work-cancel work)))
          (should scheduled)
          (should (eq (plist-get e-modernchat--first-admission-failure
                                 :event-type)
                      (cdr case)))
          (should (stringp
                   (plist-get
                    (plist-get e-modernchat--first-admission-failure :payload)
                    :summary))))))))

(ert-deftest e-modernchat-view-model-test-bounds-detached-messages ()
  "The snapshot retains only the requested tail of detached SQL messages."
  (let* ((harness (e-modernchat-test--harness))
         (messages '((:id "m-0" :role user :content "zero")
                     (:id "m-1" :role assistant :content "one")
                     (:id "m-2" :role assistant :content "two")))
         (metadata '(:name "Detached"
                     :project-root "/tmp/project/"
                     :context-references
                     (:chat-session
                      (:attachments ((:uri "file:///tmp/a.org"
                                      :label "a.org"))))))
         (snapshot
          (e-modernchat-view-model-snapshot
           harness "session-1" :session-metadata metadata
           :messages messages :message-limit 2 :activity-limit 0))
         (session (cdr (assq 'session snapshot)))
         (visible (cdr (assq 'messages snapshot)))
         (attachments (cdr (assq 'attachments snapshot))))
    (should (equal (cdr (assq 'name session)) "Detached"))
    (should (= (length visible) 2))
    (should (equal (cdr (assq 'id (aref visible 0))) "m-1"))
    (should (= (length attachments) 1))))

(ert-deftest e-modernchat-view-model-test-omits-hidden-detached-messages ()
  "A hidden durable message never reaches the modern chat viewport."
  (let* ((snapshot
          (e-modernchat-view-model-snapshot
           (e-modernchat-test--harness) "session-1"
           :session-metadata '(:name "Hidden")
           :messages '((:id "hidden" :role assistant :content "private"
                        :display hidden)
                       (:id "visible" :role assistant :content "public"))))
         (messages (cdr (assq 'messages snapshot))))
    (should (= (length messages) 1))
    (should (equal (cdr (assq 'id (aref messages 0))) "visible"))))

(ert-deftest e-modernchat-view-model-test-exposes-generic-hook-audit-summary ()
  "The shell renders generic transient audit metadata."
  (let* ((event '(:message-id "audit-1" :turn-id "turn-1"
                  :event-type hook-audit
                  :created-at "2026-07-30T00:00:00Z"
                  :payload (:summary "Claim check needs revision")))
         (dto (e-modernchat-view-model-activity event)))
    (should (equal (cdr (assq 'id dto)) "audit-1"))
    (should (equal (cdr (assq 'title dto)) "Hook audit"))
    (should (equal (cdr (assq 'summary dto))
                   "Claim check needs revision"))))

(ert-deftest e-modernchat-test-existing-open-queries-after-return ()
  "Existing modern chat opens immediately and applies one detached SQL view."
  (let* ((harness (e-modernchat-test--harness))
         (view-work (e-modernchat-test--pending-work))
         (binding-work (e-modernchat-test--finished-work :bound))
         sent-states subscribed-cursor buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'e-modernchat--ensure-runtime) #'ignore)
                  ((symbol-function 'e-session-storage-sqlite-p)
                   (lambda (_store) t))
                  ((symbol-function 'emacs-egui-create-buffer)
                   (lambda (&rest arguments)
                     (let ((created (generate-new-buffer
                                     (plist-get arguments :buffer-name))))
                       (list :buffer created))))
                  ((symbol-function 'emacs-egui-on)
                   (lambda (&rest _arguments) nil))
                  ((symbol-function 'emacs-egui-send-state)
                   (lambda (_session state) (push state sent-states)))
                  ((symbol-function 'e-session-async-chat-view)
                   (lambda (&rest _arguments) view-work))
                  ((symbol-function 'e-chat-service-binding-start)
                   (lambda (&rest _arguments) binding-work))
                  ((symbol-function 'e-chat-service-subscribe-from-cursor)
                   (lambda (_harness _session-id cursor _function)
                     (setq subscribed-cursor cursor)
                     'subscription))
                  ((symbol-function 'e-chat-service-unsubscribe) #'ignore))
          (setq buffer (e-modernchat-open-session harness "existing" nil))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should (eq e-modernchat--view-work view-work))
            (should-not e-modernchat--view-messages))
          (e-work-finish
           view-work
           '(:session-id "existing"
             :metadata (:name "Existing")
             :association (:session-id "existing" :board-id "board")
             :messages ((:id "m-1" :role assistant :content "loaded"))
             :cursor 17))
          (with-current-buffer buffer
            (should-not e-modernchat--view-work)
            (should (equal (plist-get e-modernchat-session-metadata :name)
                           "Existing"))
            (should (equal (plist-get (car e-modernchat--view-messages) :id)
                           "m-1")))
          (should (equal subscribed-cursor 17))
          (should (>= (length sent-states) 2)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-modernchat-test-public-send-persists-and-renders-from-sql ()
  "A real modern chat input round-trips through SQLite and a detached view."
  (let* ((directory (make-temp-file "e-modernchat-sql-" t))
         (store (e-session-sqlite-store-create directory))
         (harness
          (e-harness-create
           :sessions store
           :backend
           (e-backend-fake-create
            :items '((:type assistant-message :content "SQL modern reply")
                     (:type done :reason stop)))))
         (session-id "modernchat-sql")
         (readiness
          (e-chat-service-create-session-start
           :harness harness :id session-id
           :metadata '(:name "SQL modern chat")))
         (original-submit (symbol-function 'e-chat-service-submit-session))
         sent-states admission buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'e-modernchat--ensure-runtime) #'ignore)
                  ((symbol-function 'emacs-egui-create-buffer)
                   (lambda (&rest arguments)
                     (list :buffer
                           (generate-new-buffer
                            (plist-get arguments :buffer-name)))))
                  ((symbol-function 'emacs-egui-on)
                   (lambda (&rest _arguments) nil))
                  ((symbol-function 'emacs-egui-send-state)
                   (lambda (_session state) (push state sent-states)))
                  ((symbol-function 'e-chat-service-submit-session)
                   (lambda (&rest arguments)
                     (setq admission (apply original-submit arguments)))))
          (setq buffer
                (e-modernchat-open-session
                 harness session-id nil '(:name "SQL modern chat") readiness))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (e-modernchat--handle-ui-action
             '((action . "send-message") (text . "SQL modern prompt"))))
          (should (e-work-handle-p admission))
          (e-modernchat-test--await admission)
          (e-modernchat-test--await readiness)
          (should
           (e-modernchat-test--wait-until
            (lambda ()
              (and (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (and (seq-find
                           (lambda (message)
                             (equal (plist-get message :content)
                                    "SQL modern prompt"))
                           e-modernchat--view-messages)
                          (seq-find
                           (lambda (message)
                             (equal (plist-get message :content)
                                    "SQL modern reply"))
                           e-modernchat--view-messages)))))))
          (let* ((view
                  (e-modernchat-test--await
                   (e-session-async-chat-view store session-id :limit 8)))
                 (contents
                  (mapcar (lambda (message) (plist-get message :content))
                          (plist-get view :messages))))
            (should (equal contents
                           '("SQL modern prompt" "SQL modern reply"))))
          (should (e-chat-service-binding harness session-id))
          (should sent-states))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when-let* ((binding (e-chat-service-binding harness session-id)))
        (ignore-errors (e-chat-service--retire-binding binding)))
      (ignore-errors (e-session-sqlite-store-close store))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(provide 'e-modernchat-test)

;;; e-modernchat-test.el ends here
