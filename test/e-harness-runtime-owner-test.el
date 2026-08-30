;;; e-harness-runtime-owner-test.el --- Direct runtime-owner contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests deliberately load the extracted runtime owners without the
;; e-harness application facade.  Composed behavior remains covered by
;; e-harness-test.el and the board/chat integration suites; this file freezes
;; the smaller owner contracts and the explicit attached-turn adapter port.

;;; Code:

(require 'ert)
(require 'e-backend)
(require 'e-events)
(require 'e-harness-state)
(require 'e-harness-activity)
(require 'e-harness-turn-state)
(require 'e-harness-turn)
(require 'e-session)

(defun e-harness-runtime-owner-test--harness (&optional items)
  "Return a fresh harness state with fake backend ITEMS."
  (e-harness-state-create
   :backend (e-backend-fake-create
             :items (or items
                        '((:type assistant-message :content "owner-answer")
                          (:type done :reason stop))))))

(defun e-harness-runtime-owner-test--session (harness &optional id)
  "Create and return session ID in HARNESS."
  (plist-get (e-session-create (e-harness-sessions harness)
                               :id (or id "owner-session"))
             :id))

(defun e-harness-runtime-owner-test--port (harness session-id token)
  "Return an explicit replacement port for HARNESS SESSION-ID and TOKEN."
  (e-harness-attached-turn-port-create
   :harness harness
   :session-id session-id
   :attachment-token token
   :authorizer
   (lambda (candidate-harness candidate-session-id candidate-token)
     (and (eq candidate-harness harness)
          (equal candidate-session-id session-id)
          (equal candidate-token token)))
   :follow-up-publisher
   (lambda (&rest _args) :published)))

(ert-deftest e-harness-runtime-owner-test-loads-without-facade ()
  "Owner modules can be loaded without loading the application facade."
  (when (featurep 'e-harness)
    (ert-skip "fresh-load contract is exercised in an isolated process"))
  (should-not (featurep 'e-harness))
  (let ((harness (e-harness-runtime-owner-test--harness)))
    (should (e-harness-p harness))
    (should (e-harness-capability-state-p
             (e-harness-capability-state harness)))
    (should (e-harness-activity-state-p
             (e-harness-activity-state harness)))
    (should (e-harness-turn-state-p
             (e-harness-turn-state harness)))
    (should (e-harness-context-state-p
             (e-harness-context-state harness)))))

(ert-deftest e-harness-runtime-owner-test-state-substates-have-distinct-owners ()
  "Capability, activity, turn, and context state are explicit substates."
  (let ((harness (e-harness-runtime-owner-test--harness)))
    (should-not (eq (e-harness-capability-state harness)
                    (e-harness-activity-state harness)))
    (should-not (eq (e-harness-activity-state harness)
                    (e-harness-turn-state harness)))
    (should-not (eq (e-harness-turn-state harness)
                    (e-harness-context-state harness)))
    (should (hash-table-p (e-harness-active-turns harness)))
    (should (hash-table-p (e-harness-prompt-queues harness)))
    (should (hash-table-p (e-harness-provider-compaction-candidates harness)))))

(ert-deftest e-harness-runtime-owner-test-activity-observation-is-filtered ()
  "Activity subscribers receive only their selected session's events."
  (let* ((harness (e-harness-runtime-owner-test--harness))
         (session-id "wanted")
         (seen nil)
         (port (e-harness-runtime-owner-test--port
                harness session-id 'token))
         (subscription
          (e-harness-attached-turn-port-observe-activity
           port (lambda (event) (push event seen)))))
    (e-harness-activity-emit-turn-event
     harness "other" "turn-other" 'turn-started nil)
    (e-harness-activity-emit-turn-event
     harness session-id "turn-wanted" 'turn-started nil)
    (should (= (length seen) 1))
    (should (equal (plist-get (car seen) :session-id) "wanted"))
    (should (eq (e-harness-activity-event-class 'turn-started) 'audit))
    (let ((raw (list :tool-call
                     (list :id "call-1" :name "secret-tool"
                           :arguments "detached-arguments"))))
      (e-harness-activity-emit-turn-event
       harness session-id "turn-wanted" 'tool-started raw)
      (let* ((observed (car seen))
             (payload (plist-get observed :payload))
             (call (plist-get payload :tool-call)))
        (should (= (length seen) 2))
        (should (equal (plist-get call :id) "call-1"))
        (should (equal (plist-get call :name) "secret-tool"))
        (should-not (plist-member call :arguments))
        (plist-put call :name "changed")
        (should (equal (plist-get (plist-get raw :tool-call) :name)
                       "secret-tool"))))
    (e-harness-attached-turn-port-stop-observing port subscription)
    (e-harness-activity-emit-turn-event
     harness session-id "turn-wanted-2" 'turn-started nil)
    (should (= (length seen) 2))))

(ert-deftest e-harness-runtime-owner-test-turn-state-queue-is-copying ()
  "Queue observations are semantic copies and update unsettled state."
  (let* ((harness (e-harness-runtime-owner-test--harness))
         (queue-id
          (e-harness-turn-state-enqueue-prompt
           harness "session" "next" '("ref") '(:submit-mode queue))))
    (should (stringp queue-id))
    (let ((items (e-harness-queued-prompts harness "session")))
      (should (= (length items) 1))
      (should (equal (plist-get (car items) :prompt) "next"))
      (setcar items nil)
      (should (equal (plist-get (car (e-harness-queued-prompts harness "session"))
                       :prompt)
                     "next")))
    (should (= (plist-get (e-harness-unsettled-state harness) :queued-inputs) 1))
    (e-harness-turn-state-set-queued-prompts harness "session" nil)
    (should-not (e-harness-queued-prompts harness "session"))
    (should (= (plist-get (e-harness-unsettled-state harness) :queued-inputs) 0))))

(ert-deftest e-harness-runtime-owner-test-attached-port-authorizes-submit-and-output ()
  "An explicit replacement port authorizes submit and exposes final output."
  (let* ((harness (e-harness-runtime-owner-test--harness))
         (session-id (e-harness-runtime-owner-test--session harness))
         (port (e-harness-runtime-owner-test--port harness session-id 'token)))
    (should-error
     (let ((unauthorized
            (e-harness-attached-turn-port-create
             :harness harness :session-id session-id :attachment-token 'wrong
             :authorizer
             (lambda (&rest _args) nil))))
       (e-harness-attached-turn-port-submit-batch unauthorized "unauthorized"))
     :type 'e-harness-board-attachment-required)
    (should-not (gethash session-id (e-harness-active-turns harness)))
    (let* ((turn-id
            (e-harness-attached-turn-port-submit port "question"))
           (settled (e-harness-wait-batch harness session-id))
           (assistant
            (e-harness-attached-turn-port-assistant-message port turn-id)))
      (should (eq (plist-get settled :status) 'done))
      (should (equal (plist-get assistant :content) "owner-answer")))))

(ert-deftest e-harness-runtime-owner-test-attached-port-publishes-follow-up ()
  "A port double receives settlement follow-ups without board internals."
  (let* ((harness (e-harness-runtime-owner-test--harness))
         (session-id (e-harness-runtime-owner-test--session harness))
         (seen nil)
         (port (e-harness-attached-turn-port-create
                :harness harness
                :session-id session-id
                :attachment-token 'token
                :authorizer
                (lambda (_harness candidate-session-id candidate-token)
                  (and (equal candidate-session-id session-id)
                       (eq candidate-token 'token)))
                :follow-up-publisher
                (lambda (candidate-harness candidate-session-id prompt &rest args)
                  (setq seen (list candidate-harness candidate-session-id prompt args))
                  :accepted)))
         (entry (list :id "turn" :status 'running
                      :endpoint-token 'token :attached-turn-port port)))
    (unwind-protect
        (progn
          (e-harness-turn-state-put-active-turn harness session-id entry)
          (should (eq
                   (e-harness-attached-turn-port-publish-follow-up
                    port "follow-up"
                    :references '("reference") :metadata '(:source test)
                    :tags '(board))
                   :accepted))
          (should (equal (nth 2 seen) "follow-up"))
          (should (equal (plist-get (nth 3 seen) :tags) '(board)))
          (should (equal (plist-get (nth 3 seen) :references)
                         '("reference"))))
      (e-harness-turn-state-remove-active-turn harness session-id entry))))

(ert-deftest e-harness-runtime-owner-test-follow-up-port-authorizes-exact-receiver ()
  "A settling follow-up cannot fall back to the active replacement port.

The active entry deliberately retains GOOD while STALE rejects its own token.
Both queue and publication must authorize the receiver supplied by the caller;
neither operation may reach GOOD after STALE has been rejected."
  (let* ((harness (e-harness-runtime-owner-test--harness))
         (session-id (e-harness-runtime-owner-test--session harness))
         (stale-authorized nil)
         (stale-published nil)
         (good-published nil)
         (good
          (e-harness-attached-turn-port-create
           :harness harness :session-id session-id
           :attachment-token 'good-token
           :authorizer (lambda (&rest _args) t)
           :follow-up-publisher
           (lambda (&rest _args)
             (setq good-published t)
             :good)))
         (stale
          (e-harness-attached-turn-port-create
           :harness harness :session-id session-id
           :attachment-token 'stale-token
           :authorizer
           (lambda (&rest _args)
             (setq stale-authorized t)
             nil)
           :follow-up-publisher
           (lambda (&rest _args)
             (setq stale-published t)
             :stale)))
         (entry (list :id "settling" :status 'done
                      :endpoint-token 'good-token
                      :attached-turn-port good)))
    (unwind-protect
        (progn
          (e-harness-turn-state-put-active-turn harness session-id entry)
          (should-error
           (e-harness-attached-turn-port-follow-up stale "queued")
           :type 'e-harness-board-attachment-required)
          (should stale-authorized)
          (should-not (e-harness-queued-prompts harness session-id))
          (should-not good-published)
          (should-not stale-published)
          (should-error
           (e-harness-attached-turn-port-publish-follow-up stale "published")
           :type 'e-harness-board-attachment-required)
          (should stale-authorized)
          (should-not good-published)
          (should-not stale-published)
          (should (stringp
                   (e-harness-attached-turn-port-follow-up good "accepted")))
          (should (= (length (e-harness-queued-prompts harness session-id)) 1))
          (should (eq
                   (e-harness-attached-turn-port-publish-follow-up
                    good "accepted-published")
                   :good))
          (should good-published))
      (e-harness-turn-state-remove-active-turn harness session-id entry))))

(ert-deftest e-harness-runtime-owner-test-attached-port-steer-queue-and-abort ()
  "Steering, queued follow-up, and abort use the same explicit port fence."
  (let* ((harness (e-harness-runtime-owner-test--harness))
         (session-id (e-harness-runtime-owner-test--session harness))
         (port (e-harness-runtime-owner-test--port harness session-id 'token)))
    (e-harness-attached-turn-port-submit port "question" :delay 1.0)
    (let ((observation (e-harness-attached-turn-port-active-turn port)))
      (should (equal (list (car observation)
                           (car (cddr observation)))
                     '(:id :status)))
      (should (eq (plist-get observation :status) 'running))
      (plist-put observation :status 'finished)
      (should (eq (plist-get
                   (e-harness-attached-turn-port-active-turn port) :status)
                  'running)))
    (should (equal
             (e-harness-attached-turn-port-steer port "steer")
             (plist-get (e-harness-attached-turn-port-active-turn port) :id)))
    (should (stringp
             (e-harness-attached-turn-port-queue port "queued")))
    (should (= (length (e-harness-queued-prompts harness session-id)) 1))
    (let ((entry (e-harness-attached-turn-port-abort port)))
      (should (eq (plist-get entry :status) 'cancelled))
      (should (e-harness-runtime-owner-test--wait-until-empty port)))))

(defun e-harness-runtime-owner-test--wait-until-empty (port)
  "Return non-nil after PORT's attached turn settles and clears."
  (let ((deadline (+ (float-time) 2.0)))
    (while (and (e-harness-attached-turn-port-active-turn port)
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (null (e-harness-attached-turn-port-active-turn port))))

(ert-deftest e-harness-runtime-owner-test-attached-port-propagates-error ()
  "Attached batch callers receive backend errors rather than swallowed state."
  (let* ((backend
          (e-backend-create
           :name "error"
           :start
           (cl-function
            (lambda (&key on-error &allow-other-keys)
              (funcall on-error
                       '(e-loop-backend-error "owner backend failed"
                                              (:status 502)))
              nil))))
         (harness (e-harness-state-create :backend backend))
         (session-id (e-harness-runtime-owner-test--session harness))
         (port (e-harness-runtime-owner-test--port harness session-id 'token)))
    (should-error
     (e-harness-attached-turn-port-submit-batch port "question")
     :type 'e-loop-backend-error)
    (should-not (eq (plist-get (e-harness-attached-turn-port-active-turn port)
                              :status)
                    'running))))

(provide 'e-harness-runtime-owner-test)

;;; e-harness-runtime-owner-test.el ends here
