;;; e-board-runtime-admission-test.el --- Board runtime admission tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public composition and owner-boundary tests for enrollment/admission.
;; Keep these scenarios together because their change reason is exact
;; cross-owner admission authority and rollback.

;;; Code:

(require 'ert)
(load (expand-file-name "e-board-runtime-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-board-runtime-test-attached-session-enrolls-prepared-work-before-start ()
  "An attached harness maps turn/tool work to its participant board."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((handle (e-work-prepare
                      (e-work-spec-create
                       :id "tool" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "done"))
                      nil
                      :context
                      '(:session-id "session" :turn-id "turn" :tool-call (:id "call"))))
             (source-board (e-board-registry-board-source-board board))
             (enroll (e-harness-work-enrollment-function harness)))
        (funcall enroll handle (lambda (&rest _args)))
        (should (e-board-observed-work source-board (e-work-handle-id handle)))
        (should (e-board-invocation source-board '("turn" "call")))
        (should-not (e-work-handle-started-p handle))))))

(ert-deftest e-board-runtime-test-exact-invocation-effect-uses-opaque-target ()
  "A terminal board effect reaches only the target's captured loop service."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           reply)
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((source-board (e-board-registry-board-source-board board))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "tool" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "done"))
                      nil
                      :context
                      '(:session-id "session" :turn-id "turn" :tool-call (:id "call"))))
             (enroll (e-harness-work-enrollment-function harness)))
        (funcall enroll handle (lambda (state payload) (setq reply (list state payload))))
        (let ((invocation (e-board-invocation source-board '("turn" "call"))))
          (should invocation)
          (should-not (functionp (e-board-invocation-effect-target invocation))))
        (e-work-start-prepared handle)
        (should-not reply)
        (e-board-drain-terminal-classifications source-board)
        (e-board-drain-effects source-board)
        (should (equal reply '(finished "done")))
        (should (eq (e-board-invocation-state
                     (e-board-invocation source-board '("turn" "call")))
                    'committed))))))

(ert-deftest e-board-runtime-test-invocation-effect-rejects-stale-endpoint ()
  "A qualified invocation cannot call back after its harness generation clears."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (calls 0))
      (e-harness-create-session harness :id "session")
      (e-harness-instance-register
       :id :qualified :kind 'chat :harness-id :live)
      (e-harness-registry-register :live harness)
      (e-board-runtime-attach-instance
       board :qualified "session" :participant-id "participant")
      (let* ((source-board (e-board-registry-board-source-board board))
             (handle
              (e-work-prepare
               (e-work-spec-create
                :id "tool" :execution 'cheap :interactive-policy 'cheap
                :runner (lambda (_arguments _context) "done"))
               nil :context
               '(:session-id "session" :turn-id "turn"
                 :tool-call (:id "call"))))
             (enroll (e-harness-work-enrollment-function harness)))
        (funcall enroll handle (lambda (&rest _arguments) (cl-incf calls)))
        (e-work-start-prepared handle)
        (e-board-drain-terminal-classifications source-board)
        (let* ((invocation (e-board-invocation source-board '("turn" "call")))
               (lease (e-board-invocation-effect-target invocation))
               ;; Terminal runtime invocation entries are removed before
               ;; notification, so retain the exact object for state evidence.
               (runtime-invocation
                (e-board-runtime-invocation-lease--invocation lease)))
          (e-harness-registry-clear-instance :live)
          (e-board-drain-effects source-board)
          (should (= calls 0))
          (should (eq (e-board-invocation-state invocation) 'failed))
          (should (eq (e-board-runtime-invocation-state runtime-invocation)
                      'unavailable))
          (should-not
           (gethash (e-board-runtime-invocation-lease--target lease)
                    e-board-runtime--invocations)))))))

(ert-deftest e-board-runtime-test-await-aggregation-uses-opaque-target ()
  "Await completion uses the captured awaiting call rather than a board closure."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           reply)
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((source-board (e-board-registry-board-source-board board))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "watched" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "done"))
                      nil :context '(:session-id "session" :turn-id "source"
                                     :tool-call (:id "source-call"))))
             (enroll (e-harness-work-enrollment-function harness)))
        (funcall enroll handle nil)
        (let ((cancel
               (e-board-runtime--subscribe-aggregation
                harness (list handle) 'all 30
                (lambda (reason) (setq reply reason))
                '(:session-id "session" :turn-id "await" :tool-call (:id "await-call")))))
          (unwind-protect
              (progn
                (let ((aggregation (e-board-aggregation source-board
                                                         '("await" "await-call"))))
                  (should aggregation)
                  (should-not (functionp
                               (e-board-aggregation-effect-target aggregation))))
                (e-work-start-prepared handle)
                (e-board-drain-terminal-classifications source-board)
                (e-board-drain-effects source-board)
                (should (eq reply 'complete)))
            (funcall cancel)))))))

(ert-deftest e-board-runtime-test-invalid-invocation-rejects-before-enrollment ()
  "An invalid exact invocation leaves its prepared work off the board."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((source-board (e-board-registry-board-source-board board))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "tool" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "never"))
                      nil :context '(:session-id "session" :turn-id "turn")))
             (enroll (e-harness-work-enrollment-function harness)))
        (should-error (funcall enroll handle (lambda (&rest _args)))
                      :type 'e-board-runtime-error)
        (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
        (should-not (e-work-handle-started-p handle))))))

(ert-deftest e-board-runtime-test-missing-source-turn-rejects-before-enrollment ()
  "Board work without participant activity provenance mutates no owner."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((source-board (e-board-registry-board-source-board board))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "invalid" :execution 'cooperative
                       :interactive-policy 'async
                       :runner (lambda (_handle _arguments _context) :deferred))
                      nil :context '(:session-id "session")))
             (enroll (e-harness-work-enrollment-function harness)))
        (should-error (funcall enroll handle nil)
                      :type 'e-board-runtime-invalid-work)
        (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
        (should-not (e-work-handle-activity-observer handle))
        (should-not (e-work-handle-hook-dispatcher handle))
        (should-not (e-work-handle-started-p handle))))))

(ert-deftest e-board-runtime-test-enrollment-rejects-non-signaling-private-reentrant-retirement ()
  "A private observer cannot retire admission and return a stale target."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attachment nil)
           (handle nil)
           (source-board nil)
           (enroll nil)
           (retired nil)
           condition)
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "reentrant-private-admission" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call")))
            enroll (e-harness-work-enrollment-function harness))
      ;; The callback runs after the +1 counter mutation but returns normally.
      ;; The outer adjust therefore has a stale pre-notification value unless
      ;; register-invocation checks exact target/count/attachment authority.
      (let ((e-board-runtime--unsettled-change-function
             (lambda (&rest _state)
               (unless retired
                 (setq retired t)
                 (e-board-runtime-retire-attachment attachment)))))
        (setq condition
              (condition-case err
                  (progn (funcall enroll handle #'ignore) nil)
                (error err))))
      (should retired)
      (should condition)
      (should (eq (car condition) 'e-board-runtime-error))
      (should-not (gethash '("board" "participant" "turn" "call")
                           e-board-runtime--invocations))
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations)
                 0))
      (should (= (hash-table-count
                  (e-board-runtime-attachment-invocation-targets attachment))
                 0))
      (should-not (e-board-runtime--current-active-attachment-p attachment))
      (should (eq (e-board-runtime-attachment-state attachment) 'dormant))
      (should-not e-board-runtime--quiescence-current)
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (should-not (e-work-handle-hook-dispatcher handle))
      (should-not (e-work-handle-activity-observer handle))
      (should-not (e-work-handle-publication-observer handle))
      ;; Retirement removed only the old exact attachment.  A fresh attachment
      ;; can admit the same prepared Work without an orphaned target/relation.
      (let ((replacement
             (e-board-runtime-attach board harness "session"
                                     :participant-id "participant")))
        (should (funcall enroll handle #'ignore))
        (should (e-board-observed-work source-board (e-work-handle-id handle)))
        (should (e-board-invocation source-board '("turn" "call")))
        (e-board-runtime-retire-attachment replacement)))))

(ert-deftest e-board-runtime-test-enrollment-rejects-non-signaling-hook-list-reentrant-retirement ()
  "A hook-list observer cannot retire admission and return a stale target."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attachment nil)
           (handle nil)
           (source-board nil)
           (enroll nil)
           (retired nil)
           condition)
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "reentrant-hook-list-admission" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call")))
            enroll (e-harness-work-enrollment-function harness))
      (let ((e-board-runtime--unsettled-change-functions
             (list
              (lambda (&rest _state)
                (unless retired
                  (setq retired t)
                  (e-board-runtime-retire-attachment attachment))))))
        (setq condition
              (condition-case err
                  (progn (funcall enroll handle #'ignore) nil)
                (error err))))
      (should retired)
      (should condition)
      (should (eq (car condition) 'e-board-runtime-error))
      (should-not (gethash '("board" "participant" "turn" "call")
                           e-board-runtime--invocations))
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations)
                 0))
      (should (= (hash-table-count
                  (e-board-runtime-attachment-invocation-targets attachment))
                 0))
      (should-not (e-board-runtime--current-active-attachment-p attachment))
      (should (eq (e-board-runtime-attachment-state attachment) 'dormant))
      (should-not e-board-runtime--quiescence-current)
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (should-not (e-work-handle-hook-dispatcher handle))
      (should-not (e-work-handle-activity-observer handle))
      (should-not (e-work-handle-publication-observer handle))
      (let ((replacement
             (e-board-runtime-attach board harness "session"
                                     :participant-id "participant")))
        (should (funcall enroll handle #'ignore))
        (should (e-board-observed-work source-board (e-work-handle-id handle)))
        (should (e-board-invocation source-board '("turn" "call")))
        (e-board-runtime-retire-attachment replacement)))))

(ert-deftest e-board-runtime-test-enrollment-rolls-back-private-notification-fault ()
  "A private unsettled observer fault leaves actual enrollment retryable."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attachment nil)
           (handle nil)
           (source-board nil)
           (enroll nil)
           condition
           (notifications 0))
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board))
      (setq handle
            (e-work-prepare
             (e-work-spec-create
              :id "admission" :execution 'cheap :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call"))))
      (setq enroll (e-harness-work-enrollment-function harness))
      (let ((e-board-runtime--unsettled-change-function
             (lambda (&rest _state)
               (cl-incf notifications)
               (error (if (= notifications 1)
                          "private admission notification"
                        "rollback admission notification")))))
        (setq condition
              (condition-case err
                  (progn (funcall enroll handle #'ignore) nil)
                (error err))))
      (should condition)
      (should (equal (error-message-string condition)
                     "private admission notification"))
      (should (= notifications 2))
      (should-not (gethash (list "board" "participant" "turn" "call")
                           e-board-runtime--invocations))
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations) 0))
      (should (= (hash-table-count
                  (e-board-runtime-attachment-invocation-targets attachment))
                 0))
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (should-not (e-work-handle-hook-dispatcher handle))
      (should-not (e-work-handle-activity-observer handle))
      (should-not (e-work-handle-publication-observer handle))
      ;; The same prepared Work can be admitted after the observer is repaired;
      ;; no orphan target or board relation needs attachment retirement first.
      (let ((target (funcall enroll handle #'ignore)))
        (should target)
        (should (e-board-observed-work source-board (e-work-handle-id handle)))
        (should (e-board-invocation source-board '("turn" "call"))))
      (e-board-runtime-retire-attachment attachment))))

(ert-deftest e-board-runtime-test-enrollment-rolls-back-postmutation-hook-fault ()
  "A dispatcher installation fault after mutation removes that dispatcher."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attachment nil)
           (handle nil)
           (source-board nil)
           (original (symbol-function 'e-work-install-hook-dispatcher)))
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "postmutation-hook-admission" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call"))))
      (cl-letf (((symbol-function 'e-work-install-hook-dispatcher)
                 (lambda (&rest arguments)
                   (prog1 (apply original arguments)
                     (error "postmutation dispatcher admission")))))
        (should-error
         (funcall (e-harness-work-enrollment-function harness)
                  handle #'ignore)
         :type 'error))
      (should-not (e-work-handle-hook-dispatcher handle))
      (should-not (e-work-handle-activity-observer handle))
      (should-not (e-work-handle-publication-observer handle))
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (should-not (e-board-invocation source-board '("turn" "call")))
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations) 0))
      (should (funcall (e-harness-work-enrollment-function harness)
                       handle #'ignore))
      (e-board-runtime-retire-attachment attachment))))

(ert-deftest e-board-runtime-test-enrollment-rolls-back-postmutation-activity-fault ()
  "An activity observer fault after mutation removes both Work hooks."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attachment nil)
           (handle nil)
           (source-board nil)
           (original (symbol-function 'e-work-install-activity-observer)))
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "postmutation-activity-admission" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call"))))
      (cl-letf (((symbol-function 'e-work-install-activity-observer)
                 (lambda (&rest arguments)
                   (prog1 (apply original arguments)
                     (error "postmutation activity admission")))))
        (should-error
         (funcall (e-harness-work-enrollment-function harness)
                  handle #'ignore)
         :type 'error))
      (should-not (e-work-handle-hook-dispatcher handle))
      (should-not (e-work-handle-activity-observer handle))
      (should-not (e-work-handle-publication-observer handle))
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (should-not (e-board-invocation source-board '("turn" "call")))
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations) 0))
      (should (funcall (e-harness-work-enrollment-function harness)
                       handle #'ignore))
      (e-board-runtime-retire-attachment attachment))))

(ert-deftest e-board-runtime-test-enrollment-rolls-back-postmutation-publication-fault ()
  "A publication observer fault after mutation leaves no board relation."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attachment nil)
           (handle nil)
           (source-board nil)
           (original (symbol-function 'e-work-install-publication-observer)))
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "postmutation-publication-admission" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call"))))
      (cl-letf (((symbol-function 'e-work-install-publication-observer)
                 (lambda (&rest arguments)
                   (prog1 (apply original arguments)
                     (error "postmutation publication admission")))))
        (should-error
         (funcall (e-harness-work-enrollment-function harness)
                  handle #'ignore)
         :type 'error))
      (should-not (e-work-handle-hook-dispatcher handle))
      (should-not (e-work-handle-activity-observer handle))
      (should-not (e-work-handle-publication-observer handle))
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (should-not (e-board-invocation source-board '("turn" "call")))
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations) 0))
      (should (funcall (e-harness-work-enrollment-function harness)
                       handle #'ignore))
      (e-board-runtime-retire-attachment attachment))))

(ert-deftest e-board-runtime-test-enrollment-rolls-back-hook-list-notification-fault ()
  "A hook-list observer fault rolls back hooks, target, and board state."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attachment nil)
           (handle nil)
           (source-board nil)
           condition)
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "hook-list-admission" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call"))))
      (let ((e-board-runtime--unsettled-change-functions
             (list (lambda (&rest _state) (error "hook-list admission notification")))))
        (should-error
         (funcall (e-harness-work-enrollment-function harness)
                  handle #'ignore)
         :type 'error))
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations) 0))
      (should (= (hash-table-count
                  (e-board-runtime-attachment-invocation-targets attachment))
                 0))
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (should-not (e-work-handle-hook-dispatcher handle))
      (should-not (e-work-handle-activity-observer handle))
      (should-not (e-work-handle-publication-observer handle))
      (should (funcall (e-harness-work-enrollment-function harness)
                       handle #'ignore))
      (e-board-runtime-retire-attachment attachment))))

(ert-deftest e-board-runtime-test-enrollment-board-stage-signal-preserves-replacement ()
  "A board-stage signal cannot cancel a same-key replacement invocation."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (old-harness (e-harness-create))
           (new-harness (e-harness-create))
           old new new-lease handle entered)
      (e-harness-create-session old-harness :id "old")
      (e-harness-create-session new-harness :id "new")
      (setq old (e-board-runtime-attach
                 board old-harness "old" :participant-id "participant")
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "board-stage-signal" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "old" :turn-id "turn" :tool-call (:id "call"))))
      (let ((original (symbol-function 'e-board-enroll-invocation-work)))
        (cl-letf (((symbol-function 'e-board-enroll-invocation-work)
                   (lambda (&rest arguments)
                     (prog1 (apply original arguments)
                       (unless entered
                         (setq entered t)
                         (e-board-runtime-retire-attachment old)
                         (setq new (e-board-runtime-attach
                                     board new-harness "new"
                                     :participant-id "participant")
                               new-lease
                               (e-board-runtime--register-invocation
                                new "turn" "call" #'ignore))
                         (error "board stage replacement signal"))))))
          (should-error
           (funcall (e-harness-work-enrollment-function old-harness)
                    handle #'ignore))))
      (should entered)
      (should new-lease)
      (should (e-board-runtime--invocation-lease-current-p new-lease))
      (should (= e-board-runtime--unsettled-invocation-count 1))
      (should-not (e-board-observed-work
                   (e-board-registry-board-source-board board)
                   (e-work-handle-id handle)))
      (should-not (e-work-handle-publication-observer handle))
      (e-board-runtime--drop-invocation new-lease)
      (e-board-runtime-retire-attachment new))))

(ert-deftest e-board-runtime-test-enrollment-board-stage-return-preserves-replacement ()
  "A board-stage return after replacement cannot commit stale Work authority."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (old-harness (e-harness-create))
           (new-harness (e-harness-create))
           old new new-lease handle entered condition)
      (e-harness-create-session old-harness :id "old")
      (e-harness-create-session new-harness :id "new")
      (setq old (e-board-runtime-attach
                 board old-harness "old" :participant-id "participant")
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "board-stage-return" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "old" :turn-id "turn" :tool-call (:id "call"))))
      (let ((original (symbol-function 'e-board-enroll-invocation-work)))
        (cl-letf (((symbol-function 'e-board-enroll-invocation-work)
                   (lambda (&rest arguments)
                     (prog1 (apply original arguments)
                       (unless entered
                         (setq entered t)
                         (e-board-runtime-retire-attachment old)
                         (setq new (e-board-runtime-attach
                                     board new-harness "new"
                                     :participant-id "participant")
                               new-lease
                               (e-board-runtime--register-invocation
                                new "turn" "call" #'ignore)))))))
          (setq condition
                (condition-case err
                    (progn
                      (funcall (e-harness-work-enrollment-function old-harness)
                               handle #'ignore)
                      nil)
                  (error err)))))
      (should entered)
      (should (eq (car condition) 'e-board-runtime-error))
      (should (e-board-runtime--invocation-lease-current-p new-lease))
      (should (= e-board-runtime--unsettled-invocation-count 1))
      (should-not (e-board-observed-work
                   (e-board-registry-board-source-board board)
                   (e-work-handle-id handle)))
      (should-not (e-work-handle-publication-observer handle))
      (e-board-runtime--drop-invocation new-lease)
      (e-board-runtime-retire-attachment new))))

(ert-deftest e-board-runtime-test-enrollment-retries-index-inverse-in-outer-transaction ()
  "A transient exact index inverse fault is retried without duplicating admission."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           attachment handle source-board
           (original-enroll (symbol-function 'e-board-enroll-invocation-work))
           (original-remove (symbol-function
                             'e-board-admission-remove-index))
           (index-failed t))
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "index-inverse-runtime" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call"))))
      (cl-letf (((symbol-function 'e-board-enroll-invocation-work)
                 (lambda (&rest arguments)
                   (prog1 (apply original-enroll arguments)
                     (error "primary board admission"))))
                ((symbol-function 'e-board-admission-remove-index)
                 (lambda (receipt)
                   (if index-failed
                       (progn
                         (setq index-failed nil)
                         (error "transient index inverse"))
                     (funcall original-remove receipt)))))
        (should-error
         (funcall (e-harness-work-enrollment-function harness)
                  handle #'ignore)))
      ;; The runtime retried the same admission token after the injected
      ;; before-mutation inverse failure, so the worktree is clean and a
      ;; second public enrollment has no duplicate index cell.
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (should-not (e-board-invocation source-board '("turn" "call")))
      (should-not (e-work-handle-publication-observer handle))
      (should (= e-board-runtime--unsettled-invocation-count 0))
      (should (funcall (e-harness-work-enrollment-function harness)
                       handle #'ignore))
      (should (e-board-invocation source-board '("turn" "call")))
      (e-board-runtime-retire-attachment attachment))))

(ert-deftest e-board-runtime-test-retains-pending-admission-after-persistent-index-fault ()
  "A repeated lower-owner inverse fault remains recoverable by exact token."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           attachment handle source-board condition
           (original-enroll (symbol-function 'e-board-enroll-invocation-work))
           (original-remove (symbol-function
                             'e-board-admission-remove-index))
           (inverse-faults 0))
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "persistent-index-inverse" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call"))))
      (cl-letf (((symbol-function 'e-board-enroll-invocation-work)
                 (lambda (&rest arguments)
                   (prog1 (apply original-enroll arguments)
                     (error "persistent primary"))))
                ((symbol-function 'e-board-admission-remove-index)
                 (lambda (_receipt)
                   (cl-incf inverse-faults)
                   (error "persistent index inverse"))))
        (setq condition
              (condition-case err
                  (progn
                    (funcall (e-harness-work-enrollment-function harness)
                             handle #'ignore)
                    nil)
                (error err))))
      (should (equal (error-message-string condition) "persistent primary"))
      (should (= inverse-faults 2))
      ;; The board relation and its exact token are retained rather than being
      ;; replaced by a descriptive-id retry or falsely reported as cleaned.
      (should (= (hash-table-count e-board-runtime--pending-admissions) 1))
      (should (e-board-observed-work source-board (e-work-handle-id handle)))
      (should (e-board-invocation source-board '("turn" "call")))
      (should (e-work-handle-publication-observer handle))
      (should (= e-board-runtime--unsettled-invocation-count 0))
      ;; Once the lower owner is available again, the retained exact token is
      ;; retired before the new admission is staged and no duplicate remains.
      (cl-letf (((symbol-function 'e-board-admission-remove-index)
                 (lambda (receipt) (funcall original-remove receipt))))
        (should (funcall (e-harness-work-enrollment-function harness)
                         handle #'ignore)))
      (should (= (hash-table-count e-board-runtime--pending-admissions) 0))
      (should (e-board-invocation source-board '("turn" "call")))
      (should (= e-board-runtime--unsettled-invocation-count 1))
      (e-board-runtime-retire-attachment attachment))))

(ert-deftest e-board-runtime-test-enrollment-rolls-back-board-postcommit-fault ()
  "A board relation fault after its commit removes only this enrollment."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attachment nil)
           (handle nil)
           (source-board nil)
           (original (symbol-function 'e-board-enroll-invocation-work)))
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "board-admission" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call"))))
      (cl-letf (((symbol-function 'e-board-enroll-invocation-work)
                 (lambda (&rest arguments)
                   (prog1 (apply original arguments)
                     (error "board post-commit admission")))))
        (should-error
         (funcall (e-harness-work-enrollment-function harness)
                  handle #'ignore)
         :type 'error))
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (should-not (e-board-invocation source-board '("turn" "call")))
      (let ((events (e-board-events source-board)))
        (should (= (length events) 1))
        (should (eq (e-board-event-type (car events)) 'participant-added)))
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations) 0))
      (should (= (hash-table-count
                  (e-board-runtime-attachment-invocation-targets attachment))
                 0))
      (should-not (e-work-handle-hook-dispatcher handle))
      (should-not (e-work-handle-activity-observer handle))
      (should-not (e-work-handle-publication-observer handle))
      ;; The wrapper is gone, so retry uses the same intended target cleanly.
      (should (funcall (e-harness-work-enrollment-function harness)
                       handle #'ignore))
      (e-board-runtime-retire-attachment attachment))))

(ert-deftest e-board-runtime-test-enrollment-hook-install-fault-restores-dispatcher ()
  "A later activity-hook fault removes the dispatcher installed earlier."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attachment nil)
           (handle nil)
           (source-board nil)
           (original (symbol-function 'e-work-install-activity-observer)))
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "hook-admission" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call"))))
      (cl-letf (((symbol-function 'e-work-install-activity-observer)
                 (lambda (&rest _arguments)
                   (error "activity hook admission"))))
        (should-error
         (funcall (e-harness-work-enrollment-function harness)
                  handle #'ignore)
         :type 'error))
      (should-not (e-work-handle-hook-dispatcher handle))
      (should-not (e-work-handle-activity-observer handle))
      (should-not (e-work-handle-publication-observer handle))
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations) 0))
      ;; Keep the original binding visible to make the test's intended seam
      ;; explicit; the dynamic override above has already been restored.
      (should (functionp original))
      (should (funcall (e-harness-work-enrollment-function harness)
                       handle #'ignore))
      (e-board-runtime-retire-attachment attachment))))

(ert-deftest e-board-runtime-test-enrollment-preserves-preexisting-target-and-hooks ()
  "Admission failure never clears a target or hook owned by another attempt."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attachment nil)
           (handle nil)
           (dispatcher (lambda (&rest _arguments) nil))
           (source-board nil)
           preexisting-lease)
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach board harness "session"
                                    :participant-id "participant")
            source-board (e-board-registry-board-source-board board)
            handle
            (e-work-prepare
             (e-work-spec-create
              :id "preserve-admission" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) "done"))
             nil :context
             '(:session-id "session" :turn-id "turn"
               :tool-call (:id "call"))))
      (e-work-install-hook-dispatcher
       handle dispatcher '(:cancel deferred :cleanup deferred :settle deferred))
      (should-error
       (funcall (e-harness-work-enrollment-function harness) handle #'ignore)
       :type 'e-work-prepared-start-invalid)
      (should (eq (e-work-handle-hook-dispatcher handle) dispatcher))
      (e-work-remove-hook-dispatcher handle dispatcher)
      (setq preexisting-lease
            (e-board-runtime--register-invocation
             attachment "turn" "call" #'ignore))
      (should-error
       (funcall (e-harness-work-enrollment-function harness) handle #'ignore)
       :type 'e-board-runtime-error)
      (should (eq (gethash
                   (e-board-runtime-invocation-lease--target preexisting-lease)
                   e-board-runtime--invocations)
                  (gethash (e-board-runtime-invocation-lease--target
                            preexisting-lease)
                           (e-board-runtime-attachment-invocation-targets
                            attachment))))
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations) 1))
      (should-not (e-work-handle-hook-dispatcher handle))
      (should-not (e-work-handle-activity-observer handle))
      (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
      (e-board-runtime--drop-invocation preexisting-lease)
      (should (= (plist-get (e-board-runtime-unsettled-state) :invocations) 0))
      (e-board-runtime-retire-attachment attachment))))

(ert-deftest e-board-runtime-test-enrollment-installs-bounded-activity-mailbox ()
  "Board enrollment captures progress before it schedules general hook work."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((handle (e-work-prepare
                      (e-work-spec-create
                       :id "stream" :execution 'cooperative :interactive-policy 'async
                       :runner (lambda (_handle _arguments _context) :deferred))
                      nil
                      :context '(:session-id "session" :turn-id "turn")))
             (enroll (e-harness-work-enrollment-function harness)))
        (unwind-protect
            (progn
              (funcall enroll handle nil)
              (should (equal (plist-get (e-work-handle-context handle) :turn-id)
                             "turn"))
              (should (functionp (e-work-handle-activity-observer handle)))
              (e-work-start-prepared handle)
              (e-work-progress handle '(:step first))
              (let (mailbox)
                (maphash
                 (lambda (_mailbox-id candidate)
                   (when (equal (plist-get candidate :work-id)
                                (e-work-handle-id handle))
                     (setq mailbox candidate)))
                 e-board-runtime--work-activity-mailboxes)
                (should (plist-member mailbox :attachment))
                (should (equal (plist-get mailbox :turn-id) "turn"))
                (should (equal (plist-get mailbox :payload) '(:step first))))
              (e-board-runtime--drain-activity-mailboxes)
              (let ((message (car (last (e-board-messages
                                         (e-board-registry-board-source-board board))))))
                (should (eq (e-board-message-kind message) 'activity))
                (should (equal (e-board-message-source-turn-id message) "turn"))
                (should (equal (e-board-message-source-activity-key message)
                               '("participant" 1 1)))))
          (e-work-cancel handle))))))

(ert-deftest e-board-runtime-test-aggregation-admission-rejects-replaced-lease ()
  "An await admission aborts its exact board projection on replacement.

The wrapper exercises both a normal board return and a board-stage signal.  In
each case the old aggregation is removed by its opaque board token while the
new same-key invocation lease remains live."
  (dolist (stage '(return signal))
    (e-board-runtime-test--with-empty-state
      (let* ((board (e-board-registry-create :id "aggregation-replacement"))
             (old-harness (e-harness-create))
             (new-harness (e-harness-create))
             old new new-lease handle entered condition source-board)
        (e-harness-create-session old-harness :id "old")
        (e-harness-create-session new-harness :id "new")
        (setq old
              (e-board-runtime-attach
               board old-harness "old" :participant-id "participant")
              handle
              (e-work-prepare
               (e-work-spec-create
                :id (format "aggregation-source-%s" stage)
                :execution 'cheap :interactive-policy 'cheap
                :runner (lambda (_arguments _context) "done"))
               nil :context '(:session-id "old" :turn-id "source")))
        (setq source-board (e-board-registry-board-source-board board))
        (funcall (e-harness-work-enrollment-function old-harness) handle nil)
        (let ((original (symbol-function 'e-board-subscribe-aggregation)))
          (cl-letf (((symbol-function 'e-board-subscribe-aggregation)
                     (lambda (&rest arguments)
                       (let ((result (apply original arguments)))
                         (unless entered
                           (setq entered t)
                                 ;; Retirement fences the old runtime lease;
                                 ;; the replacement then takes the same target.
                                 (e-board-runtime-retire-attachment old)
                                 (setq new
                                       (e-board-runtime-attach
                                        board new-harness "new"
                                        :participant-id "participant")
                                       new-lease
                                       (e-board-runtime--register-invocation
                                        new "await" "await-call" #'ignore)))
                         (when (eq stage 'signal)
                           (error "aggregation board-stage signal"))
                         result))))
            (setq condition
                  (condition-case err
                      (progn
                        (e-board-runtime--subscribe-aggregation
                         old-harness (list handle) 'all 30 #'ignore
                         '(:session-id "old" :turn-id "await"
                           :tool-call (:id "await-call")))
                        nil)
                    (error err)))))
        (should entered)
        (should (eq (car condition)
                    (if (eq stage 'signal) 'error 'e-board-runtime-error)))
        (should new)
        (should (e-board-runtime--invocation-lease-current-p new-lease))
        (should (= e-board-runtime--unsettled-invocation-count 1))
        (should-not (e-board-aggregation
                     source-board '("await" "await-call")))
        (e-board-runtime--drop-invocation new-lease)
        (e-board-runtime-retire-attachment new)))))


;;; e-board-runtime-admission-test.el ends here
