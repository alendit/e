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
  "Create one board-native test session in STORE from ARGUMENTS."
  (let* ((session (apply #'e-session-create store arguments))
         (session-id (plist-get session :id)))
    (e-session-declare-board-state
     store session-id (format "chat:%s" session-id)
     (format "test-board:%s" session-id))
    session))



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
  (let* ((binding (e-chat-service-binding e-chat-harness e-chat-session-id))
         (attachment (and binding
                          (e-chat-service-binding-attachment binding)))
         (participant-id
          (and attachment
               (e-board-registry-participant-id
                (e-board-runtime-attachment-participant attachment)))))
    (when participant-id
      (puthash (list participant-id turn-id)
               turn-id
               (e-chat-service-binding-turn-map binding))))
  (puthash e-chat-session-id
           (list :id turn-id :status (or status 'running))
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
  (goto-char (point-min))
  (search-forward text)
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



(defun e-chat-test--seed-board-log-from-private-fixture (harness session-id)
  "Translate an old private test fixture into its explicit durable board log.
Production presentation never performs this compatibility translation."
  (let* ((store (e-harness-sessions harness))
         (_ (unless (plist-get (e-session-get store session-id)
                               :board-session-state)
              (e-session-declare-board-state
               store session-id (format "chat:%s" session-id)
               (format "test-board:%s" session-id))))
         (binding (e-chat-service-ensure-binding harness session-id))
         (board (and binding
                     (e-board-registry-board-source-board
                      (e-chat-service-binding-board binding))))
         (participant-id
          (if binding
              (e-board-registry-participant-id
               (e-board-runtime-attachment-participant
                (e-chat-service-binding-attachment binding)))
            (format "fixture:%s" session-id)))
         (sequence 0)
         (turn-inputs (make-hash-table :test 'equal))
         envelopes)
    (dolist (message (e-session-messages store session-id))
      (unless (memq (plist-get message :role) '(tool-call tool))
        (let* ((user-p (eq (plist-get message :role) 'user))
               (turn-id (plist-get message :turn-id))
               (stored-id (plist-get message :id))
               (message-id
                (if (and user-p turn-id (stringp stored-id)
                         (string-match-p
                          "\\`[0-9A-HJKMNP-TV-Z]\\{26\\}\\'" stored-id))
                    turn-id
                  (or stored-id turn-id)))
               (reply-id (and (not user-p) turn-id
                              (gethash turn-id turn-inputs))))
          (when (and user-p turn-id)
            (puthash turn-id message-id turn-inputs))
          (push (list :id message-id
                    :kind (if user-p 'input 'output)
                    :author (if (eq (plist-get message :role) 'user)
                                "fixture-client" "fixture-participant")
                    :tags '(main) :content (plist-get message :content)
                    :reference (plist-get message :references)
                    :attributes
                    (let ((attributes
                           (copy-tree (plist-get message :metadata))))
                      (if-let ((display (plist-get message :display)))
                          (plist-put attributes :display display)
                        attributes))
                    :source-input-key
                    (and user-p
                         (list 'fixture session-id (cl-incf sequence)))
                    :source-output-key
                    (and (not user-p)
                         (list 'fixture session-id (cl-incf sequence)))
                    :source-turn-id turn-id
                    :created-at (plist-get message :created-at)
                    :subject-participant-id
                    (and (not user-p) participant-id)
                    :reply-to-message-ids (and reply-id (list reply-id))
                    :routing-state 'historical)
                envelopes))))
    (dolist (event (e-session-activity-events store session-id))
      (push (list :id (or (plist-get event :id)
                          (format "fixture-activity-%d" (1+ sequence)))
                  :kind 'activity
                  :author (format "participant:%s" participant-id)
                  :tags '(main) :attributes (copy-tree (plist-get event :payload))
                  :subject-participant-id participant-id
                  :source-turn-id (plist-get event :turn-id)
                  :reply-to-message-ids
                  (when-let ((input-id (gethash (plist-get event :turn-id)
                                                turn-inputs)))
                    (list input-id))
                  :activity-kind (plist-get event :event-type)
                  :created-at (plist-get event :created-at)
                  :source-activity-key
                  (list participant-id 1 (cl-incf sequence)))
            envelopes))
    (setq envelopes (nreverse envelopes))
    (if board
        (dolist (envelope envelopes)
          (unless (e-board-message board (plist-get envelope :id))
            (e-board-import-message board envelope)))
      (unless (plist-get (e-session-get store session-id) :board-session-state)
        (e-session-declare-board-state
         store session-id (format "chat:%s" session-id)
         (format "test-board:%s" session-id)))
      (dolist (envelope envelopes)
        (e-session-append-board-message store session-id envelope)))
    (when binding
      (while (< (e-board-observer-next-index
                 (e-chat-service-binding-observer binding))
                (e-board-message-count board))
        (e-chat-service-drain-binding binding)))
    envelopes))



(defun e-chat-test--record-with-round ()
  "Return a semantic activity fixture for one active provider round.
The composed suite should exercise activity through its public event contract;
the fixture therefore contains events rather than an owner-private record."
  (list :turn-id "turn-1"
        :events '((:event-type provider-request-started
                   :payload (:status started)))))

(provide 'e-chat-test-support)

;;; e-chat-test-support.el ends here
