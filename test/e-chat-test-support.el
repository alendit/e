;;; e-chat-test-support.el --- Shared e chat test support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared fixtures for owner-level e chat presentation tests.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-bayesian-reasoning)
(require 'e-board-sqlite-service)
(require 'e-chat)
(require 'e-chat-session)
(require 'e-context-inspection)
(require 'e-dev-profile)
(require 'e-emacs-base)
(require 'e-events)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(load (expand-file-name "e-tools-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-layer)
(require 'e-prompts)
(require 'e-request)
(require 'e-session-sqlite)
(require 'e-store)
(require 'e-work)
(require 'e-ui-work)
(require 'e-tools)

(defvar evil-local-mode)
(defvar evil-state)
(defvar doom-leader-alt-key)
(defvar doom-leader-map)
(defvar persp-activated-functions)

;; Shared helpers for owner-level chat presentation tests.

(defvar e-chat-test-support--sqlite-fixtures nil
  "Disposable SQLite stores created for public chat presentation tests.")

(defvar e-chat-test-support--sqlite-harnesses (make-hash-table :test 'eq)
  "Harnesses whose default store was replaced by a disposable SQLite store.")

(defvar e-chat-test-support-share-sqlite-store nil
  "When non-nil, give implicit test harnesses one shared disposable store.
This models production compositions whose independent harnesses publish into
one authoritative runtime database, notably parent and subagent harnesses.")

(defvar e-chat-test-support--shared-sqlite-fixture nil
  "Current shared disposable STORE . DIRECTORY fixture, or nil.")

(defvar e-chat-test-support--opened-sessions (make-hash-table :test 'eq)
  "Session ids first created through each disposable SQLite harness.")

(defvar e-chat-test-support--sql-fixture-active-p nil
  "Non-nil while an SQL-backed public chat test is executing.

The support file is loaded into shared broad-suite processes.  Keeping this
state dynamically scoped prevents its constructor advice from silently
changing unrelated session, harness, resource, and transport tests that run
later in the same Emacs.")

(defun e-chat-test-support--sql-test-p (test)
  "Return non-nil when ERT TEST exercises a public SQL chat surface."
  (let ((name (symbol-name (ert-test-name test))))
    (string-match-p
     (concat "\\`\\(?:e-chat-\\|e-modernchat-\\|e-canvas-\\|"
             "e-org-canvas-\\|e-provider-continuation-\\|"
             "e-debug-test-\\|"
             "e-subagent-runner-test-\\|"
             "e-ui-work-integration-test-chat-\\|"
             "e-runtime-store-recovery-graphical-\\)")
     name)))

(defun e-chat-test-support--sqlite-harness (operation &rest arguments)
  "Call harness constructor OPERATION with a disposable SQLite store.
Only presentation tests that omit an explicit store are adapted.  The public
chat API itself continues to reject non-SQL stores."
  (if (or (not e-chat-test-support--sql-fixture-active-p)
          (plist-member arguments :sessions))
      (apply operation arguments)
    (let* ((fixture
            (or (and e-chat-test-support-share-sqlite-store
                     e-chat-test-support--shared-sqlite-fixture)
                (let* ((directory (make-temp-file "e-chat-test-sql-" t))
                       (store
                        (e-session-sqlite-store-create
                         directory :asynchronous t))
                       (created (cons store directory)))
                  (push created e-chat-test-support--sqlite-fixtures)
                  (when e-chat-test-support-share-sqlite-store
                    (setq e-chat-test-support--shared-sqlite-fixture created))
                  created)))
           (store (car fixture))
           (harness (apply operation (append arguments (list :sessions store)))))
      (puthash harness t e-chat-test-support--sqlite-harnesses)
      (puthash harness (make-hash-table :test 'equal)
               e-chat-test-support--opened-sessions)
      harness)))

(defun e-chat-test-support--open-sql-session (operation &rest arguments)
  "Call public chat OPERATION, creating a disposable session on first open."
  (if (not e-chat-test-support--sql-fixture-active-p)
      (apply operation arguments)
    (let* ((harness (plist-get arguments :harness))
         (session-id (plist-get arguments :session-id))
         (sessions (and harness
                        (gethash harness
                                 e-chat-test-support--opened-sessions)))
         (explicit-new-p (and sessions session-id
                              (plist-get arguments :new-session)))
         (create-p (and sessions session-id
                        (not (gethash session-id sessions))
                        (not explicit-new-p))))
    (when (or explicit-new-p create-p)
      (puthash session-id t sessions)
      (when create-p
        (setq arguments (plist-put arguments :new-session t))))
    (condition-case error
        (let ((buffer (apply operation arguments)))
          ;; Most owner tests exercise settled presentation behavior, not the
          ;; nonblocking-open boundary covered by the graphical suite.  Observe
          ;; readiness here so SQLite callbacks cannot race their assertions.
          (when (and sessions (buffer-live-p buffer))
            (when-let* ((work
                         (buffer-local-value
                          'e-chat--session-query-work buffer)))
              (e-work-with-batch-await
                (e-work-await-batch work :timeout 5.0)))
            (when-let* ((work
                         (buffer-local-value
                          'e-chat--session-readiness-work buffer)))
              (e-work-with-batch-await
                (e-work-await-batch work :timeout 5.0))))
          buffer)
      (error
       (when (or explicit-new-p create-p)
         (remhash session-id sessions))
       (signal (car error) (cdr error)))))))

(defun e-chat-test-support--close-sqlite-fixtures ()
  "Release every disposable public-chat SQLite fixture."
  (e-chat-test--kill-chat-buffers)
  ;; Killing the presentation buffers unsubscribes them, but production keeps
  ;; an unsubscribed live controller for the bounded idle-retirement interval.
  ;; Tests own these harnesses and stores outright, so retire their remaining
  ;; coordination synchronously before closing the worker they reference.
  (maphash
   (lambda (harness _tracked)
     ;; A held fake-provider turn is legitimate test state.  Drop those live
     ;; handles before retiring the binding so no late callback can target the
     ;; fixture after its worker has closed.
     (clrhash (e-harness-prompt-queues harness))
     (clrhash (e-harness-prompt-queue-counts harness))
     (clrhash (e-harness-active-turns harness))
     (when-let* ((bindings (gethash harness e-chat-service--bindings)))
       (let (owned)
         (maphash (lambda (_session-id binding) (push binding owned)) bindings)
         (dolist (binding owned)
           (e-chat-service--retire-binding binding))))
     (should-not (gethash harness e-chat-service--bindings))
     (let (board-leaks)
       (maphash
        (lambda (_runtime boards)
          (maphash
           (lambda (board-id bindings)
             (when (cl-some
                    (lambda (binding)
                      (eq (e-chat-service-binding-harness binding) harness))
                    bindings)
               (push board-id board-leaks)))
           boards))
        e-chat-service--board-bindings)
       (should-not board-leaks)))
   e-chat-test-support--sqlite-harnesses)
  (dolist (fixture e-chat-test-support--sqlite-fixtures)
    (ignore-errors (e-session-sqlite-store-close (car fixture)))
    (when (file-directory-p (cdr fixture))
      (delete-directory (cdr fixture) t)))
  (setq e-chat-test-support--sqlite-fixtures nil)
  (setq e-chat-test-support--shared-sqlite-fixture nil)
  (clrhash e-chat-test-support--sqlite-harnesses)
  (clrhash e-chat-test-support--opened-sessions))

(defun e-chat-test-support--run-test (operation &rest arguments)
  "Call ERT OPERATION with ARGUMENTS and close SQL presentation fixtures."
  (let ((e-chat-test-support--sql-fixture-active-p
         (e-chat-test-support--sql-test-p (car arguments))))
    (if e-chat-test-support--sql-fixture-active-p
        (unwind-protect (apply operation arguments)
          (e-chat-test-support--close-sqlite-fixtures))
      (apply operation arguments))))

(advice-add 'e-harness-create :around #'e-chat-test-support--sqlite-harness)
(advice-add 'e-chat-open :around #'e-chat-test-support--open-sql-session)
(advice-add 'ert-run-test :around #'e-chat-test-support--run-test)



(defun e-chat-test--buffer (&optional items session-id)
  "Return a chat buffer backed by fake backend ITEMS and SESSION-ID.
The returned buffer is not displayed in a window, so redraw gating would
otherwise defer progress and activity repaints.  Force the visible path for
tests, matching how the buffer behaves when shown to a user."
  (let* ((backend (e-backend-fake-create :items items))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id (or session-id "chat-test"))))
    (with-current-buffer buffer
      (e-chat-surface-set-redraw-visible t))
    buffer))



(defun e-chat-test--composer (transcript)
  "Return TRANSCRIPT's live production composer buffer."
  (let ((composer (and (buffer-live-p transcript)
                       (e-chat-surface-composer-buffer transcript))))
    (unless (buffer-live-p composer)
      (error "Chat transcript has no live composer: %S" transcript))
    composer))



(defun e-chat-test--composer-text-for (transcript)
  "Return the editable input text paired with TRANSCRIPT."
  (with-current-buffer (e-chat-test--composer transcript)
    (e-chat-composer-text)))



(defun e-chat-test--create-session (store &rest arguments)
  "Create one test session in STORE from ID and METADATA ARGUMENTS.
SQLite fixtures use the production atomic owner admission.  Local-store tests
create an ordinary detached session and do not synthesize a second Board."
  (let* ((session-id (or (plist-get arguments :id)
                         (e-session-generate-id)))
         (metadata (plist-get arguments :metadata))
         (principal (format "chat:%s" session-id))
         (board-id (format "test-board:%s" session-id)))
    (prog1
        (if (e-session-storage-sqlite-p store)
            (let* ((service
                (e-board-sqlite-service-create
                 (e-session-storage-runtime-store store)))
               (participant-id (format "test-owner:%s" session-id))
               (policy (list :participant-id participant-id
                             :pickup-selector '(:tags (main))
                             :observer-selector '(:tags (main))
                             :default-tags '(main) :default-to nil))
               (session
          (e-board-sqlite-service-session-admission
                 :id session-id :metadata metadata
                 :principal principal :board-id board-id
                 :association-role 'owner :routing-policy policy))
               (records (plist-get session :admission-records))
               (query-delta (plist-get session :query-delta))
               (participant
                (list :id participant-id :author "e-chat-test"
                      :principal principal :controller principal
                      :role 'owner :state 'active
                      :subscription-id (concat "sub_" participant-id)
                      :publication-pending nil))
               (work
                (e-board-sqlite-service-admit-session-owner-start
                 service session-id board-id principal records query-delta
                 participant)))
              (e-work-with-batch-await
                (e-work-await-batch work :timeout 5.0))
              session)
          (let ((created
                 (e-session-create store :id session-id :metadata metadata)))
            (if (e-work-handle-p created)
                (e-work-with-batch-await
                  (e-work-await-batch created :timeout 5.0))
              created)))
      ;; The e-chat-open advice only synthesizes sessions not admitted by the
      ;; fixture itself.  Record explicit SQL admission without reconstructing
      ;; any session state.
      (maphash
       (lambda (harness _tracked)
         (when (eq (e-harness-sessions harness) store)
           (puthash session-id t
                    (gethash harness
                             e-chat-test-support--opened-sessions))))
       e-chat-test-support--sqlite-harnesses))))



(defun e-chat-test--kill-chat-buffers ()
  "Kill all live e chat buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'e-chat-mode)
          (kill-buffer buffer))))))



(defun e-chat-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun e-chat-test--finished-work (result)
  "Return a request-scoped work handle already settled with RESULT."
  (let ((work
         (e-work-prepare
          (e-work-spec-create
           :id "e-chat-test-finished-work" :execution 'cooperative
           :interactive-policy 'async :owner 'e-chat-test
           :runner (lambda (&rest _arguments) :deferred))
          nil)))
    (e-work-start-prepared work :arguments nil)
    (e-work-finish work result)
    work))

(defun e-chat-test--await (value)
  "Observe asynchronous VALUE at this explicit test boundary."
  (if (e-work-handle-p value)
      (e-work-with-batch-await
        (e-work-await-batch value :timeout 5.0))
    value))

(defun e-chat-test--dispatch-observed-event (event)
  "Deliver board-observed EVENT through the current chat subscription."
  (funcall (e-chat-service-subscription-function e-chat--event-subscription)
           event)
  (e-ui-work-with-batch-drain
    (e-ui-work-drain-batch :buffer (current-buffer))))



(defmacro e-chat-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     ,@body))



(defun e-chat-test--activate-chat-session (harness)
  "Activate the chat-session capability in HARNESS."
  (e-harness-activate-capability
   harness
   (e-chat-session-capability-create))
  harness)



(defun e-chat-test--mark-active-turn (turn-id &optional status)
  "Mark TURN-ID as the current active turn in the test chat buffer."
  (puthash e-chat-session-id
           (list :id turn-id :status (or status 'running)
                 :session-query-state
                 (list :session-id e-chat-session-id
                       :metadata nil
                       :turn-options
                       (copy-tree (e-harness-default-options e-chat-harness))
                       :messages nil))
           (e-harness-active-turns e-chat-harness)))



(defun e-chat-test--register-chat-instance
    (id name harness &optional default)
  "Register chat instance ID named NAME backed by HARNESS."
  (e-harness-registry-register id harness)
  (e-harness-instance-register
   :id id
   :name name
   :kind 'chat
   :harness-id id
   :default default))



(defun e-chat-test--render-turn (turn-id start-time end-time prompt response)
  "Render TURN-ID with START-TIME, END-TIME, PROMPT, and RESPONSE."
  (e-chat-render-event
   (e-events-make :type 'turn-started
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at start-time))
  (e-chat-render-event
   (e-events-make :type 'message-added
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at start-time
                  :payload (list :message
                                 (list :role 'user :content prompt))))
  (e-chat-render-event
   (e-events-make :type 'message-added
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at end-time
                  :payload (list :message
                                 (list :role 'assistant :content response))))
  (e-chat-render-event
   (e-events-make :type 'turn-finished
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at end-time
                  :payload '(:reason stop))))



(defun e-chat-test--focused-turn-bounds ()
  "Return the focused block's observable buffer bounds.
The production transcript keeps navigation overlays private; tests use the
semantic block projection and its displayed text instead of that overlay."
  (let ((text (plist-get (e-chat-transcript-focused-block) :display-text)))
    (when text
      (save-excursion
        (let ((end (point-max)))
          (goto-char (point-min))
          (search-forward text nil t)
          (list (match-beginning 0) (match-end 0)))))))



(defun e-chat-test--focused-turn-text ()
  "Return displayed text for the focused transcript block."
  (or (plist-get (e-chat-transcript-focused-block) :display-text) ""))



(defun e-chat-test--focused-block ()
  "Return semantic metadata for the currently focused block."
  (e-chat-transcript-focused-block))



(defun e-chat-test--focus-block-containing (text)
  "Enter response navigation at rendered block containing TEXT."
  (let (position)
    (goto-char (point-min))
    ;; The same prompt can appear in the bounded title projection.  Select the
    ;; occurrence owned by a semantic transcript block, not matching chrome.
    (while (and (not position) (search-forward text nil t))
      (let ((candidate (match-beginning 0)))
        (when (e-chat-transcript-block-at-point candidate)
          (setq position candidate))))
    (unless position
      (ert-fail (format "No transcript block contains %S" text)))
    (goto-char position))
  (call-interactively #'e-chat-enter-response-navigation)
  (e-chat-test--focused-block))



(defun e-chat-test--message-display-hidden-p (message-id)
  "Return non-nil when MESSAGE-ID is locally projected as hidden."
  (e-chat-transcript-message-hidden-p message-id))



(defun e-chat-test--kill-buffer-name (name)
  "Kill buffer NAME when it exists."
  (when-let ((buffer (get-buffer name)))
    (kill-buffer buffer)))



(defun e-chat-test--turn-bounds (turn-id)
  "Return observable bounds for TURN-ID's rendered text."
  (save-excursion
    (goto-char (point-min))
    (let ((start nil)
          (end nil))
      (while (and (not start) (< (point) (point-max)))
        (when (equal (get-text-property (point) 'e-chat-turn-id) turn-id)
          (setq start (point)))
        (goto-char (or (next-single-property-change
                        (point) 'e-chat-turn-id nil (point-max))
                       (point-max))))
      (when start
        (setq end (or (next-single-property-change
                       start 'e-chat-turn-id nil (point-max))
                      (point-max)))
        (list start end)))))



(defun e-chat-test--count-occurrences (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (let ((count 0)
        (start 0))
    (while (and (not (string-empty-p needle))
                (string-match (regexp-quote needle) text start))
      (setq start (match-end 0))
      (setq count (1+ count)))
    count))



(defun e-chat-test--count-font-lock-face-runs (face &optional start end)
  "Return the number of contiguous `font-lock-face' runs using FACE."
  (let ((count 0)
        (limit (or end (point-max)))
        (pos (or start (point-min)))
        next)
    (while (< pos limit)
      (setq next (or (next-single-property-change pos 'font-lock-face
                                                  nil limit)
                     limit))
      (when (eq (get-text-property pos 'font-lock-face) face)
        (setq count (1+ count)))
      (setq pos next))
    count))



(defun e-chat-test--pending-ui-work (&optional owner key)
  "Return pending UI work for the current buffer, optionally narrowed."
  (e-ui-work-pending (current-buffer)
                     :owner owner
                     :key (or key :any)))



(defun e-chat-test--live-work-handle-p (handle)
  "Return non-nil when HANDLE is a live nonterminal work handle."
  (and (e-work-handle-p handle)
       (not (e-request-terminal-p (e-work-handle-lifecycle handle)))))



(defun e-chat-test--run-pending-ui-work (owner &optional key)
  "Run one pending UI work job for OWNER and optional KEY in current buffer."
  (let ((job (cl-find-if
              (lambda (job)
                (let ((spec (e-ui-work-job-spec job)))
                  (and (eq (e-ui-work-spec-owner spec) owner)
                       (or (null key)
                           (equal (e-ui-work-spec-key spec) key)))))
              e-ui-work--pending-jobs)))
    (should job)
    (e-ui-work--run-job-now job)))



(defun e-chat-test--session-subscriber-count (harness session-id)
  "Return HARNESS subscriber count for SESSION-ID."
  (length
   (cl-remove-if-not
    (lambda (subscriber)
      (equal (plist-get subscriber :session-id) session-id))
    (e-harness-subscribers harness))))



(defun e-chat-test--record-with-round ()
  "Return a semantic activity fixture for one active provider round.
The composed suite should exercise activity through its public event contract;
the fixture therefore contains events rather than an owner-private record."
  (list :turn-id "turn-1"
        :events '((:event-type provider-request-started
                   :payload (:status started)))))

(provide 'e-chat-test-support)

;;; e-chat-test-support.el ends here
