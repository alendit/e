;;; e-board-activity-shell-test.el --- Board activity shell and action tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-actions)
(require 'e-backend)
(require 'e-board-activity-shell)
(require 'e-board-orchestration)
(require 'e-board-sqlite-service)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-runtime-store-codec)
(require 'e-session)
(require 'e-session-sqlite)
(require 'e-subagent-live)
(require 'e-work)
(load (expand-file-name "e-board-producer-test-support.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defun e-board-activity-shell-test--wait (predicate)
  "Wait at this explicit presentation-test boundary for PREDICATE."
  (let ((deadline (+ (float-time) 5.0)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun e-board-activity-shell-test--admit
    (service board-id session-id participant-id &optional metadata)
  "Admit PARTICIPANT-ID with optional durable child METADATA."
  (let* ((principal (format "chat:%s" session-id))
         (policy (list :participant-id participant-id
                       :pickup-selector '(:tags (subagent))
                       :observer-selector
                       (list :subject-participant-id participant-id)
                       :default-tags '(subagent)
                       :default-to participant-id))
         (session
          (e-board-sqlite-service-session-admission
           :id session-id
           :metadata (append (list :name session-id) metadata)
           :principal principal :board-id board-id
           :association-role "participant" :routing-policy policy))
         (records (copy-tree (plist-get session :admission-records) t))
         (position 0))
    (dolist (record records)
      (plist-put record :journal-position (cl-incf position)))
    (e-board-producer-test-await
     (e-board-sqlite-service-admit-participant-start
      service session-id board-id records (plist-get session :query-delta)
      (list :id participant-id :name session-id :author "activity-test"
            :principal principal :controller principal :role 'participant
            :state 'active :subscription-id (concat "sub-" participant-id)
            :publication-pending nil)))))

(defun e-board-activity-shell-test--publish-lifecycle
    (target session-id status summary)
  "Publish one runner-shaped lifecycle fact for SESSION-ID."
  (e-board-producer-test-await
   (e-board-sqlite-publication-target-fact-start
    target (format "Subagent %s is %s" session-id status)
    (list 'subagent-lifecycle session-id status)
    :tags (list 'change status)
    :attributes (list :session-id session-id :status status :type 'subagent
                      :summary summary))))

(defun e-board-activity-shell-test--publish-report (target participant-id)
  "Publish one accepted terminal report for PARTICIPANT-ID."
  (e-board-producer-test-await
   (e-board-sqlite-publication-target-orchestration-fact-start
    target
    (list :version 1 :type 'terminal-report
          :idempotency-key "terminal:run-shell:task:0"
          :payload (list :run-id "run-shell" :task-key "task" :attempt 0
                         :status 'done :summary "accepted terminal"
                         :outputs [] :participant-session-id participant-id)))))

(defun e-board-activity-shell-test--row-cells (buffer participant-id)
  "Return BUFFER's rendered cells for PARTICIPANT-ID."
  (with-current-buffer buffer
    (cadr (assoc participant-id tabulated-list-entries))))

(defconst e-board-activity-shell-test--deferred-spec
  (e-work-spec-create
   :id "board-activity-shell-test-query"
   :execution 'cooperative :interactive-policy 'async :owner 'test
   :runner (lambda (_handle _arguments _context) :deferred)))

(ert-deftest e-board-activity-shell-test-list-returns-before-query-settles ()
  "The Board activity buffer returns immediately with a detached request."
  (e-board-producer-test-with-target (target)
    (let* ((work (e-work-start e-board-activity-shell-test--deferred-spec nil))
           (buffer nil))
      (cl-letf (((symbol-function 'e-board-observation-activity-page-start)
                 (lambda (&rest _arguments) work)))
        (setq buffer (e-board-activity-list-buffer :target target :live nil))
        (unwind-protect
            (progn
              (should (buffer-live-p buffer))
              (with-current-buffer buffer
                (should (derived-mode-p 'e-board-activity-shell-mode))
                (should-not tabulated-list-entries)
                (should (eq e-board-activity-shell--request work)))
              (e-work-finish
               work
               (list :board-id "producer-board" :generation 1 :revision 1
                     :participants
                     (list (list :participant-id "participant-1"
                                 :name "Participant" :state 'active))
                     :bytes 1))
              (with-current-buffer buffer
                (should (= (length tabulated-list-entries) 1))
                (should (equal (aref (e-board-activity-shell-test--row-cells
                                      buffer "participant-1") 0)
                               "participant-1"))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest e-board-activity-shell-test-renders-empty-mixed-and-terminal-rows ()
  "One ordered page renders empty, ordinary, and run-bound participants."
  (e-board-producer-test-with-target (target service board-id _runtime)
    (let ((buffer (e-board-activity-list-buffer :target target :live nil)))
      (unwind-protect
          (progn
            (e-board-activity-shell-test--wait
             (lambda ()
               (with-current-buffer buffer
                 (and (null e-board-activity-shell--request)
                      (null tabulated-list-entries)))))
            (e-board-activity-shell-test--admit
             service board-id "01-owner" "01-owner")
            (e-board-activity-shell-test--admit
             service board-id "02-child" "02-child")
            (e-board-activity-shell-test--admit
             service board-id "03-worker" "03-worker"
             '(:board-run-id "run-shell" :board-task-key "task"
               :board-attempt 0))
            (e-board-activity-shell-test--publish-lifecycle
             target "02-child" 'done "ad-hoc complete")
            (e-board-activity-shell-test--publish-report target "03-worker")
            (with-current-buffer buffer
              (e-board-activity-shell-refresh))
            (e-board-activity-shell-test--wait
             (lambda ()
               (with-current-buffer buffer
                 (= (length tabulated-list-entries) 3))))
            (with-current-buffer buffer
              (should (equal (mapcar #'car tabulated-list-entries)
                             '("01-owner" "02-child" "03-worker")))
              (let ((child (e-board-activity-shell-test--row-cells
                            buffer "02-child"))
                    (worker (e-board-activity-shell-test--row-cells
                             buffer "03-worker")))
                (should (equal (aref child 3) "lifecycle/done"))
                (should (equal (aref worker 3) "orchestration/done"))
                (should (equal (aref worker 4) "run-shell"))
                (should (equal (aref worker 5) "task"))
                (should (equal (aref worker 6) "0")))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest e-board-activity-shell-test-live-decoration-is-exact-and-non-authoritative ()
  "Only an exact Board/participant live key decorates a durable row."
  (e-board-producer-test-with-target (target service board-id _runtime)
    (e-board-activity-shell-test--admit
     service board-id "same-session" "same-session")
    (let* ((live (e-subagent-live-create))
           (other-board "other-board")
           (buffer (e-board-activity-list-buffer :target target :live live)))
      (unwind-protect
          (progn
            (e-subagent-live-reserve-admission
             live other-board "same-session" :work-handle 'other-work)
            (e-subagent-live-install live other-board "same-session")
            (e-board-activity-shell-test--wait
             (lambda ()
               (with-current-buffer buffer
                 (= (length tabulated-list-entries) 1))))
            (with-current-buffer buffer
              (should (equal (aref (e-board-activity-shell-test--row-cells
                                    buffer "same-session") 7)
                             "unavailable")))
            (e-subagent-live-reserve-admission
             live board-id "same-session" :work-handle 'exact-work)
            (e-subagent-live-install live board-id "same-session")
            (with-current-buffer buffer
              (e-board-activity-shell--render)
              (should (equal (aref (e-board-activity-shell-test--row-cells
                                    buffer "same-session") 7)
                             "available"))
              (let ((durable (copy-tree
                              (e-board-activity-shell--row "same-session") t)))
                (e-subagent-live-remove live board-id "same-session")
                (e-board-activity-shell--render)
                (should (equal (e-board-activity-shell--row "same-session")
                               durable))
                (should (equal (aref (e-board-activity-shell-test--row-cells
                                      buffer "same-session") 7)
                               "unavailable")))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest e-board-activity-shell-test-reopen-cancels-prior-request ()
  "Reopening the singleton buffer cancels and fences its prior Board read."
  (e-board-producer-test-with-target (target-a service _board-id _runtime)
    (let* ((target-b
            (e-board-sqlite-publication-target-create
             service "second-board" :author "activity-test"))
           (work-a (e-work-start e-board-activity-shell-test--deferred-spec nil))
           (work-b (e-work-start e-board-activity-shell-test--deferred-spec nil))
           (buffer nil))
      (cl-letf (((symbol-function 'e-board-observation-activity-page-start)
                 (lambda (target &rest _arguments)
                   (if (eq target target-a) work-a work-b))))
        (setq buffer (e-board-activity-list-buffer :target target-a :live nil))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (should (eq e-board-activity-shell--request work-a)))
              (e-board-activity-list-buffer :target target-b :live nil)
              (should (eq (plist-get (e-work-status work-a) :state)
                          'cancelled))
              (with-current-buffer buffer
                (should (eq e-board-activity-shell--target target-b))
                (should (eq e-board-activity-shell--request work-b))
                (should-not e-board-activity-shell--page))
              ;; A late completion from the old request cannot overwrite the
              ;; new target, even if a carrier reports after cancellation.
              (e-work-finish
               work-a
               (list :board-id "producer-board" :generation 1 :revision 1
                     :participants
                     (list (list :participant-id "stale" :state 'active))
                     :bytes 1))
              (with-current-buffer buffer
                (should (eq e-board-activity-shell--request work-b))
                (should-not e-board-activity-shell--page))
              (e-work-finish
               work-b
               (list :board-id "second-board" :generation 1 :revision 1
                     :participants
                     (list (list :participant-id "current" :state 'active))
                     :bytes 1))
              (with-current-buffer buffer
                (should (equal (mapcar #'car tabulated-list-entries)
                               '("current"))))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (when (and (e-work-handle-p work-b)
                     (not (e-request-terminal-p
                           (e-work-handle-lifecycle work-b))))
            (e-work-cancel work-b))))))))

(ert-deftest e-board-activity-shell-test-request-failure-is-local ()
  "A failed Board page request leaves a local error and no durable rows."
  (e-board-producer-test-with-target (target)
    (let* ((work (e-work-start e-board-activity-shell-test--deferred-spec nil))
           (buffer nil))
      (cl-letf (((symbol-function 'e-board-observation-activity-page-start)
                 (lambda (&rest _arguments) work)))
        (setq buffer (e-board-activity-list-buffer :target target :live nil))
        (unwind-protect
            (progn
              (e-work-fail work '(e-board-observation-error "held read"))
              (with-current-buffer buffer
                (should-not tabulated-list-entries)
                (should (string-match-p "held read"
                                        e-board-activity-shell--error))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest e-board-activity-shell-test-actions-use-board-and-bounded-raw-read ()
  "List/status/read actions query Board SQL and the bounded raw session port."
  (e-board-producer-test-with-target (target service board-id _runtime)
    (e-board-activity-shell-test--admit
     service board-id "action-participant" "action-participant")
    (let* ((directory (make-temp-file "e-board-activity-raw-" t))
           (store (e-session-sqlite-store-create directory :asynchronous t))
           (harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions store))
           (capability
            (e-capability-create
             :id 'board-observation
             :actions (e-board-observation-parent-alist)))
           (context (list :harness harness :session-id "parent"))
           (raw-participant "raw-participant"))
      (unwind-protect
          (progn
            (e-harness-activate-capability harness capability)
            (e-board-producer-test-await
             (e-session-create store :id raw-participant))
            (e-board-producer-test-await
             (e-session-append-message
              store raw-participant '(:id "raw-1" :role user :content "one")))
            (e-board-producer-test-await
             (e-session-append-message
              store raw-participant
              '(:id "raw-2" :role assistant :content "two")))
            (cl-letf (((symbol-function 'e-board-observation--context-target)
                       (lambda (_context) target)))
              (let* ((list-dispatch
                      (e-actions-dispatch 'board-observation :list nil context))
                     (status-dispatch
                      (e-actions-dispatch
                       'board-observation :status
                       (list :participant-id "action-participant") context))
                     (read-dispatch
                      (e-actions-dispatch
                       'board-observation :read
                       (list :participant-id "action-participant") context))
                     (raw-dispatch
                      (e-actions-dispatch
                       'board-observation :read
                       (list :participant-id raw-participant :raw t :limit 1)
                       context))
                     (listed (e-board-producer-test-await
                              (plist-get list-dispatch :request)))
                     (status (e-board-producer-test-await
                              (plist-get status-dispatch :request)))
                     (read (e-board-producer-test-await
                            (plist-get read-dispatch :request)))
                     (raw (e-board-producer-test-await
                           (plist-get raw-dispatch :request))))
                (should (= (length (plist-get listed :participants)) 1))
                (should (equal (plist-get status :participant-id)
                               "action-participant"))
                (should (equal (plist-get read :participant-id)
                               "action-participant"))
                (should (equal (plist-get (aref (plist-get raw :messages) 0)
                                          :content)
                               "two")))))
        (ignore-errors (e-session-sqlite-store-close store))
        (delete-directory directory t)))))

(ert-deftest e-board-activity-shell-test-row-controls-and-reference-gate ()
  "Controls are commands and retired shell symbols are absent in code paths."
  (let ((buffer (get-buffer-create e-board-activity-shell-buffer-name)))
    (unwind-protect
        (with-current-buffer buffer
          (e-board-activity-shell-mode)
          (dolist (cell '(("RET" . e-board-activity-shell-open-chat)
                          ("d" . e-board-activity-shell-show-details)
                          ("r" . e-board-activity-shell-show-raw)
                          ("p" . e-board-activity-shell-show-progress)
                          ("s" . e-board-activity-shell-steer)
                          ("u" . e-board-activity-shell-send)
                          ("i" . e-board-activity-shell-interrupt)
                          ("k" . e-board-activity-shell-shutdown)
                          ("g" . e-board-activity-shell-refresh)
                          ("n" . e-board-activity-shell-next-page)))
            (let ((binding (keymap-lookup e-board-activity-shell-mode-map
                                          (car cell))))
              (should (eq binding (cdr cell)))
              (should (commandp binding)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))))
  ;; Keep the gate focused on shipped code, test fixtures, E2E fixtures, and
  ;; project-local extension/config roots when present; historical design prose
  ;; may still name the superseded surfaces.  Build every forbidden token from
  ;; fragments so this gate cannot satisfy itself by containing the token.
  (let* ((forbidden-literals
          (list (concat "e-" "subagents-shell")
                (concat "e-" "board-runs-shell")
                (concat "e-" "subagents-list-buffer")
                (concat "e-" "board-runs-list-buffer")
                (concat "e-" "subagent-" "registry")
                (concat "e-" "subagent-" "registry-list")
                (concat "e-" "subagent-" "registry-status")
                (concat "e-" "subagent-" "registry-normalize")
                (concat "e-" "subagent-" "registry-get")
                (concat "e-" "subagent-" "raw-read")
                (concat "sub" "agent-id")))
         (forbidden-regexps
          (list (concat "sub" "_" "[0-9]+")))
         (legacy-generated-id (concat "sub" "_" "000123"))
         (roots (cl-remove-if-not
                 #'file-directory-p
                 '("lisp" "test" "e2e" "extensions" "config")))
         (files (append
                 (apply #'append
                        (mapcar
                         (lambda (root)
                           (directory-files-recursively root "\\.el\\'"))
                         roots))
                 (list "e.el"))))
    ;; Exercise the pattern against a representative retired generated ID so a
    ;; future change cannot quietly turn this into a literal-token check.
    (dolist (regexp forbidden-regexps)
      (should (string-match-p regexp legacy-generated-id)))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((contents (buffer-string)))
          (dolist (needle forbidden-literals)
            (should-not (string-match-p (regexp-quote needle) contents)))
          (dolist (regexp forbidden-regexps)
            (should-not (string-match-p regexp contents))))))))

(provide 'e-board-activity-shell-test)

;;; e-board-activity-shell-test.el ends here
