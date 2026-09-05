;;; e-runtime-store-recovery-behavior-test.el --- Graphical store recovery -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This is deliberately a small graphical composition witness.  The storage
;; work is real SQLite session/Board composition; only the provider stream is
;; controllable so the test remains network-free.

;;; Code:

(require 'cl-lib)
(require 'ert)
(eval-and-compile
  (let ((directory
         (file-name-directory
          (or load-file-name
              (and (boundp 'byte-compile-current-file)
                   byte-compile-current-file)
              buffer-file-name))))
    (add-to-list 'load-path directory)
    (add-to-list 'load-path (expand-file-name ".." directory))))
(require 'e-board)
(require 'e-board-e2e-support)
(require 'e-chat)
(require 'e-chat-service)
(require 'e-harness)
(require 'e-session)
(require 'e-graphical-test-support)

(defun e-runtime-store-recovery-graphical--surface-windows (transcript)
  "Return the visible transcript and composer windows for TRANSCRIPT."
  (let ((transcript-window (get-buffer-window transcript nil))
        (composer-window
         (cl-find-if
          (lambda (window)
            (with-current-buffer (window-buffer window)
              (derived-mode-p 'e-chat-composer-mode)))
          (window-list nil 'nomini))))
    (and (window-live-p transcript-window)
         (window-live-p composer-window)
         (cons transcript-window composer-window))))

(defun e-runtime-store-recovery-graphical--prepare-frame ()
  "Reset and settle the isolated frame before composing the chat surface.

NS applies frame resizing asynchronously.  The native event boundaries here
keep a deferred resize from a preceding graphical fixture from rebuilding the
chat window tree after this test has already started its surface assertion."
  (delete-other-windows)
  (set-frame-size (selected-frame) 140 48)
  (sit-for 0.05)
  (redisplay t)
  (sit-for 0.05)
  (redisplay t))

(defun e-runtime-store-recovery-graphical--stall-file
    (directory operation suffix)
  "Return DIRECTORY's worker stall file for OPERATION and SUFFIX."
  (expand-file-name (format "%s.%s" operation suffix) directory))

(defun e-runtime-store-recovery-graphical--arm-stall (directory operation)
  "Arm the isolated worker stall for OPERATION in DIRECTORY."
  (write-region
   "hold" nil
   (e-runtime-store-recovery-graphical--stall-file
    directory operation "hold")
   nil 'silent))

(defun e-runtime-store-recovery-graphical--release-stall (directory operation)
  "Release the isolated worker stall for OPERATION in DIRECTORY."
  (write-region
   "release" nil
   (e-runtime-store-recovery-graphical--stall-file
    directory operation "release")
   nil 'silent))

(defun e-runtime-store-recovery-graphical--stall-ready-p
    (directory operation)
  "Return non-nil when isolated worker reached OPERATION's stall."
  (file-exists-p
   (e-runtime-store-recovery-graphical--stall-file
    directory operation "ready")))

(defun e-runtime-store-recovery-graphical--runtime-operation-p
    (runtime operation)
  "Return non-nil when RUNTIME has active or queued OPERATION."
  (cl-some
   (lambda (request)
     (eq (e-runtime-store-request--operation request) operation))
   (append (and (e-runtime-store--active-request runtime)
                (list (e-runtime-store--active-request runtime)))
           (e-runtime-store--client-queue runtime))))

(defun e-runtime-store-recovery-graphical--pump-runtime (runtime)
  "Dispatch one bounded worker-output turn for RUNTIME inside server ERT."
  (when-let* ((process (e-runtime-store--process runtime))
              ((process-live-p process)))
    (accept-process-output process 0.01)))

(defun e-runtime-store-recovery-graphical--await-with-pump
    (runtime request &optional _timeout)
  "Boundedly observe REQUEST on RUNTIME inside server-hosted graphical ERT."
  (let ((deadline (+ (float-time) 3.0)))
    (while (and (memq (e-runtime-store-request--state request)
                      '(queued submitted))
                (< (float-time) deadline))
      (e-runtime-store-recovery-graphical--pump-runtime runtime)
      (sit-for 0.01))
    (pcase (e-runtime-store-request--state request)
      ('committed (e-runtime-store-request--result request))
      ('failed
       (let ((error (e-runtime-store-request--error request)))
         (signal (car error) (cdr error))))
      (state
       (ert-fail (format "Timed out observing runtime request in %S" state))))))

(defun e-runtime-store-recovery-graphical--count-string (needle buffer)
  "Return the number of literal NEEDLE occurrences in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (search-forward needle nil t)
          (cl-incf count))
        count))))

(ert-deftest e-runtime-store-recovery-graphical-s92-daily-open-does-not-wait-for-worker ()
  "Public Daily open returns a usable pending surface before SQLite replies."
  (e-board-e2e-reset-runtime)
  (let* ((directory (make-temp-file "e-runtime-store-daily-" t))
         (stall-directory (make-temp-file "e-runtime-store-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         sessions reader runtime stream harness transcript failure-transcript
         unrelated-transcript heartbeat-timer (heartbeat 0) (phase 'setup)
         synchronous-operation)
    (unwind-protect
        (ert-info ((format "DP6B phase: %s" phase))
          (progn
          (setq phase 'create-runtime)
          ;; Match the production default harness, which enables the session
          ;; application adapter before any public chat work is admitted.
          (setq sessions (e-session-sqlite-store-create directory))
          (e-session-enable sessions)
          (setq runtime (e-session-storage-runtime-store sessions)
                stream (e-graphical-test-stream-create)
                harness (e-harness-create
                         :backend (e-graphical-test-stream-backend stream)
                         :sessions sessions))
          (e-runtime-store-recovery-graphical--prepare-frame)
          (setq phase 'open-daily)
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'board-create)
          (let ((started (float-time)))
            (setq heartbeat-timer
                  (run-at-time 0.01 0.01 (lambda () (cl-incf heartbeat))))
            (setq transcript
                  (cl-letf (((symbol-function 'e-runtime-store-await)
                             (lambda (&rest arguments)
                               (signal 'error
                                       (list "Daily path called synchronous runtime await"
                                             arguments)))))
                    (e-chat-open :harness harness :new-session t)))
            (setq phase 'daily-opened)
            (should (< (- (float-time) started) 0.1))
            (should (buffer-live-p transcript))
            (should (string-match-p
                     "pending"
                     (or (e-chat-surface-status transcript) "")))
            ;; `e-chat-open' deliberately returns an undisplayed buffer.  Put
            ;; that returned public surface on the isolated frame before the
            ;; graphical composer assertion below.
            (e-chat-surface-pop-to-buffer transcript)
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--surface-windows transcript))
             1.0 "Daily transcript and composer windows")
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--stall-ready-p
                stall-directory 'board-create))
             2.0 "Daily persistence admission")
            (let* ((session-id
                    (buffer-local-value 'e-chat-session-id transcript))
                   (binding (e-chat-service-binding harness session-id))
                   (board (e-board-registry-board-source-board
                           (e-chat-service-binding-board binding)))
                   (composer (e-chat-surface-composer-buffer transcript))
                   (draft "draft survives persistence stall")
                   (message "Daily classification marker")
                   message-id)
              (should (buffer-live-p composer))
              (let ((surface-windows
                     (e-runtime-store-recovery-graphical--surface-windows
                      transcript)))
                (should surface-windows)
                (should (eq (window-buffer (car surface-windows)) transcript))
                (should (eq (window-buffer (cdr surface-windows)) composer))
                (select-window (cdr surface-windows))
                (e-graphical-test-type-text draft))
              (should
               (e-runtime-store-recovery-graphical--runtime-operation-p
                runtime 'board-participant-put))
              (should
               (e-runtime-store-recovery-graphical--runtime-operation-p
                runtime 'session-append-batch))
              (setq phase 'participant-enqueued)
              (cl-letf (((symbol-function 'e-runtime-store-await)
                         (lambda (_store request &optional _timeout)
                           (setq synchronous-operation
                                 (e-runtime-store--request-operation request))
                           (signal 'error
                                   (list "Daily timer called synchronous await"
                                         synchronous-operation)))))
                (setq message-id
                      (with-timeout
                          (1.0
                           (error "Public e-chat-submit-session blocked behind worker"))
                        (e-chat-submit-session harness session-id message)))
                (setq phase 'classification-wait)
                (condition-case _wait-error
                    (e-graphical-test-wait-until
                     (lambda ()
                       (e-runtime-store-recovery-graphical--runtime-operation-p
                        runtime 'board-routing-put))
                     1.0 "same-Board classification timer admission")
                  (ert-test-failed
                   (ert-fail
                    (format
                     (concat "Timed out waiting for same-Board classification "
                             "timer admission: active=%S queue=%S routing=%S "
                             "scheduled=%S pending=%S synchronous=%S")
                     (and (e-runtime-store--active-request runtime)
                          (e-runtime-store-request--operation
                           (e-runtime-store--active-request runtime)))
                     (mapcar #'e-runtime-store-request--operation
                             (e-runtime-store--client-queue runtime))
                     (length (e-board-input-classifications board))
                     (e-board-input-classification-scheduled board)
                     (e-board-storage--pending-count
                      (e-board-storage board))
                     synchronous-operation)))))
              (when synchronous-operation
                (ert-fail (format "Daily timer awaited %S"
                                  synchronous-operation)))
              (setq phase 'classification-enqueued)
              (e-graphical-test-wait-until (lambda () (> heartbeat 3))
                                           1.0 "independent heartbeat")
              (should-not (e-board-mutation-frozen-p board))
              (setq phase 'heartbeat-live)
              (should (= (e-runtime-store-recovery-graphical--count-string
                          message transcript)
                         0))
              (cl-letf (((symbol-function 'e-runtime-store-await)
                         (lambda (_store request &optional _timeout)
                           (setq synchronous-operation
                                 (e-runtime-store--request-operation request))
                           (signal 'error
                                   (list "Daily release called synchronous await"
                                         synchronous-operation)))))
                (e-runtime-store-recovery-graphical--release-stall
                 stall-directory 'board-create)
                (setq phase 'daily-released)
                (e-graphical-test-wait-until
                 (lambda ()
                   (e-runtime-store-recovery-graphical--pump-runtime runtime)
                   (eq (plist-get
                        (e-work-status
                         (buffer-local-value 'e-chat--session-readiness-work
                                             transcript))
                        :state)
                       'finished))
                 2.0 "Daily session readiness"))
              (when synchronous-operation
                (ert-fail (format "Daily release awaited %S"
                                  synchronous-operation)))
              (setq phase 'daily-ready)
              (should (eq (e-board-message-routing-state
                           (e-board-message board message-id))
                          'routed))
              (e-graphical-test-wait-until
               (lambda ()
                 (= (e-runtime-store-recovery-graphical--count-string
                     message transcript)
                    1))
               1.0 "classified message render")
              (setq phase 'message-rendered)
              (should (= (e-runtime-store-recovery-graphical--count-string
                          message transcript)
                         1))
              (with-current-buffer composer
                (should (string-match-p (regexp-quote draft) (buffer-string)))))
            (when (timerp heartbeat-timer)
              (cancel-timer heartbeat-timer)
              (setq heartbeat-timer nil))

            ;; Exercise the owner-local failure branch on a fresh public chat.
            ;; Hold the session mutation after its Board root and participant
            ;; have committed, then kill only the isolated worker process.
            (let ((failure-id "daily-failure-owner"))
              (setq phase 'failure-open)
              (e-runtime-store-recovery-graphical--arm-stall
               stall-directory 'session-append-batch)
              (setq failure-transcript
                    (e-chat-open :harness harness :session-id failure-id
                                 :new-session t))
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (e-runtime-store-recovery-graphical--stall-ready-p
                  stall-directory 'session-append-batch))
               2.0 "Daily failure session mutation")
              (setq phase 'failure-stalled)
              (let ((failed-process (e-runtime-store--process runtime)))
                (delete-process failed-process)
                (setq phase 'failure-worker-recovery)
                ;; The runtime permits one same-ID recovery attempt.  Let the
                ;; replacement reach the same held mutation, then lose that
                ;; worker too so the owner-local failure becomes definitive.
                (e-graphical-test-wait-until
                 (lambda ()
                   (e-runtime-store-recovery-graphical--pump-runtime runtime)
                   (let ((replacement (e-runtime-store--process runtime)))
                     (and replacement
                          (not (eq replacement failed-process))
                          (process-live-p replacement)
                          (e-runtime-store-recovery-graphical--runtime-operation-p
                           runtime 'session-append-batch))))
                 2.0 "Daily same-ID recovery attempt")
                (delete-process (e-runtime-store--process runtime)))
              (setq phase 'failure-worker-killed)
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (string-match-p
                  (regexp-quote failure-id)
                  (or (e-chat-surface-status failure-transcript) "")))
               2.0 "Daily owner suspect warning")
              (setq phase 'failure-suspect-visible)
              (let ((warning (e-chat-surface-status failure-transcript)))
                (should (string-match-p "suspect" warning))
                (should (<= (string-bytes warning) 1152)))
              ;; The failed owner is now terminally partitioned.  Release the
              ;; operation-level test stall before proving unrelated recovery.
              (e-runtime-store-recovery-graphical--release-stall
               stall-directory 'session-append-batch)
              (setq unrelated-transcript
                    (e-chat-open :harness harness
                                 :session-id "daily-unrelated-owner"
                                 :new-session t))
              (setq phase 'unrelated-opened)
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (eq (plist-get
                      (e-work-status
                       (buffer-local-value 'e-chat--session-readiness-work
                                           unrelated-transcript))
                      :state)
                     'finished))
               3.0 "unrelated session lazy worker replacement")
              (setq phase 'unrelated-finished)
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (and (null (e-runtime-store--active-request runtime))
                      (null (e-runtime-store--client-queue runtime))
                      (null (e-runtime-store--recovering-request runtime))))
               2.0 "unrelated runtime idle before independent readback")
              (e-runtime-store--close-start runtime)
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (e-runtime-store--closed runtime))
               2.0 "idle writer retirement before independent readback")
              (setq phase 'independent-readback)
              (setq reader
                    (cl-letf
                        (((symbol-function 'e-runtime-store-await)
                          #'e-runtime-store-recovery-graphical--await-with-pump))
                      (e-session-sqlite-store-create
                       directory :load-all t)))
              (should
               (equal (plist-get (e-session-get reader
                                                "daily-unrelated-owner")
                                 :id)
                      "daily-unrelated-owner")))
            (setq phase 'readback-complete))))
      (when (buffer-live-p transcript) (kill-buffer transcript))
      (when (buffer-live-p failure-transcript) (kill-buffer failure-transcript))
      (when (buffer-live-p unrelated-transcript) (kill-buffer unrelated-transcript))
      (when (timerp heartbeat-timer) (cancel-timer heartbeat-timer))
      (when (e-graphical-test-stream-p stream)
        (e-graphical-test-stream-cancel stream))
      ;; Never let a failing assertion leave test cleanup waiting behind the
      ;; deliberately stalled external worker.
      (ignore-errors
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'board-create)
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'session-append-batch))
      ;; These disposable runtimes belong only to this graphical process.
      ;; Finalize them directly so a failed assertion cannot be hidden behind
      ;; the synchronous compatibility close observer.
      (when reader
        (ignore-errors
          (e-runtime-store--finalize-close
           (e-session-storage-runtime-store reader))))
      (when runtime
        (ignore-errors (e-runtime-store--finalize-close runtime)))
      (delete-directory directory t)
      (delete-directory stall-directory t))))

(ert-deftest e-runtime-store-recovery-graphical-s92-durable-board-survives-catalog-ack-loss ()
  "A visible durable chat/Board surface remains usable after catalog recovery."
  (let* ((directory (make-temp-file "e-runtime-store-graphical-" t))
         (marker (make-temp-file "e-runtime-store-graphical-fault-"))
         (process-environment
          (cons "E_RUNTIME_STORE_TEST_FAULT=after-commit"
                (cons "E_RUNTIME_STORE_TEST_FAULT_OPERATION=catalog-put"
                      (cons (concat "E_RUNTIME_STORE_TEST_FAULT_ONCE_FILE=" marker)
                            process-environment))))
         sessions stream harness transcript)
    (delete-file marker)
    (unwind-protect
        (progn
          (e-board-e2e-reset-runtime)
          ;; The suite reuses one isolated graphical frame.  Reset its window
          ;; topology before asking the production chat surface to compose its
          ;; transcript/composer pair, so a prior test cannot hide either half.
          (e-runtime-store-recovery-graphical--prepare-frame)
          (setq sessions (e-session-sqlite-store-create directory)
                stream (e-graphical-test-stream-create)
                harness
                (e-harness-create
                 :backend (e-graphical-test-stream-backend stream)
                 :sessions sessions))
          (let* ((session
                  (e-chat-service-create-session
                   :harness harness :id "graphical-runtime-recovery"))
                 (session-id (plist-get session :id))
                 (binding (e-chat-service-binding harness session-id))
                 (registry-board (e-chat-service-binding-board binding))
                 (board (e-board-registry-board-source-board
                         registry-board))
                 (_participant
                  (e-board-registry-add-participant
                   registry-board :id "recovery-member"
                   :subscription-id "recovery-address"
                   :principal
                   (e-board-registry-client-principal
                    (e-chat-service-binding-client binding))))
                 (publication
                 (e-board-post-input
                   board :id "recovery-input" :to "recovery-member" :content "route"
                   :requester-actor (e-board-registry-board-principal registry-board)
                   :source-input-key '(graphical-recovery 1 1)))
                 (pickup-id
                  (progn
                    ;; The durable Board's production scheduler is timer-based;
                    ;; establish its ready head before the *second* timer is
                    ;; deliberately queued behind the catalog recovery.
                    (while (e-board-input-classifications board)
                      (e-board-drain-input-classifications board))
                    (let ((id (car (e-board-publication-pickup-ids publication))))
                      (unless id
                        (ert-fail
                         (format "No durable pickup: state=%S reason=%S"
                                 (e-board-message-routing-state
                                  (e-board-publication-message publication))
                                 (e-board-message-unrouted-reason
                                  (e-board-publication-message publication)))))
                      id)))
                 claimed)
            (setq transcript (e-chat-open-session harness session-id t))
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--surface-windows transcript))
             2.0 "visible durable chat surface")
            ;; The timer queues the production Board storage transition while
            ;; the catalog-put receipt is being resolved after worker loss.
            (run-at-time
             0 nil
             (lambda ()
               (setq claimed
                     (condition-case err
                         (e-board-pickup-start-delivery board pickup-id)
                       (error err)))))
            (should (> (plist-get
                        (e-session-storage-sqlite-write-catalog
                         sessions '((:id "graphical-recovery")))
                        :revision)
                       0))
            (should (equal (e-session-storage-sqlite-read-catalog sessions)
                           '((:id "graphical-recovery"))))
            (e-graphical-test-wait-until
             (lambda () claimed)
             2.0 "durable Board delivery after catalog recovery")
            (unless (e-board-pickup-p claimed)
              (ert-fail (format "Board transition failed: %S" claimed)))
            (should (eq (e-board-pickup-state (e-board-pickup board pickup-id))
                        'delivering))
            (should (file-exists-p marker))
            (should-not
             (plist-get
              (e-runtime-store-status (e-session-storage-runtime-store sessions))
              :unavailable))
            (let ((windows
                   (e-runtime-store-recovery-graphical--surface-windows transcript)))
              (should windows)
              (select-window (cdr windows))
              (e-graphical-test-type-text "surface remains usable")
              (with-current-buffer (window-buffer (cdr windows))
                (should (string-match-p "surface remains usable" (buffer-string))))
              (when (e-graphical-test-screenshot-enabled-p)
                (e-graphical-test-capture-state "runtime-store-recovery-visible")))))
      (when (buffer-live-p transcript) (kill-buffer transcript))
      (when (e-graphical-test-stream-p stream)
        (e-graphical-test-stream-cancel stream))
      (when sessions (ignore-errors (e-session-sqlite-store-close sessions)))
      (when (file-exists-p marker) (delete-file marker))
      (delete-directory directory t))))

(provide 'e-runtime-store-recovery-behavior-test)

;;; e-runtime-store-recovery-behavior-test.el ends here
