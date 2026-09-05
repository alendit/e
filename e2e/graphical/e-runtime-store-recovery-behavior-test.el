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
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-org-canvas)
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

(defun e-runtime-store-recovery-graphical--make-v6-worker ()
  "Return a disposable worker copy that treats v5 stores as unupgraded.

The production worker remains v5 until the explicit DP2 schema package.  This
test-only child lets the startup scenario exercise the already specified v5
diagnostic without changing the DP1 database schema or worker source."
  (let* ((source
          (expand-file-name "lisp/core/e-runtime-store-worker.el"
                            (e-source-directory)))
         (target (make-temp-file "e-runtime-store-worker-v6-" nil ".el")))
    (with-temp-buffer
      (insert-file-contents source)
      (goto-char (point-min))
      (unless (search-forward
               "(defconst e-runtime-store-worker-schema-version 5)" nil t)
        (error "Could not locate the v5 worker schema declaration"))
      (replace-match
       "(defconst e-runtime-store-worker-schema-version 6)")
      (write-region (point-min) (point-max) target nil 'silent))
    target))

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

(ert-deftest e-runtime-store-recovery-graphical-s92-startup-prewarm-is-transport-only ()
  "Graphical startup survives held open and reports an unupgraded v5 store."
  (let* ((directory (make-temp-file "e-runtime-store-startup-v5-" t))
         (stall-directory (make-temp-file "e-runtime-store-startup-stall-" t))
         (process-environment (copy-sequence process-environment))
         (e-default--runtime nil)
         (e-default--runtime-store nil)
         (e-default--chat-sessions nil)
         (e-runtime-sqlite--live-composition nil)
         (worker-file nil)
         (transport nil)
         heartbeat-timer
         (heartbeat 0))
    (setenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY" stall-directory)
    ;; The graphical daemon has already run the normal startup prewarm before
    ;; ERT begins.  Point this isolated scenario at its own fixture directory
    ;; so the v5 fixture and the test-only v6 transport share one owner rather
    ;; than competing with that earlier default-directory worker.
    (setenv "E_RUNTIME_STATE_DIRECTORY" directory)
    (unwind-protect
        (progn
          ;; Establish a nonempty current v5 fixture before the held startup
          ;; open.  All fixture I/O is explicit setup, not startup traffic.
          (let ((fixture nil))
            (unwind-protect
                (cl-letf (((symbol-function 'e-runtime-store-await)
                           #'e-runtime-store-recovery-graphical--await-with-pump))
                  (setq fixture (e-runtime-store-open directory))
                  (e-runtime-store-call
                   fixture 'write
                   '(:op session-append-batch :session-id "startup-fixture"
                     :records [(:type "session" :session-id "startup-fixture"
                                :id "startup-root"
                                :timestamp "2026-09-05T00:00:00Z")]))
                  (e-runtime-store-close fixture))
              (when (and fixture
                         (not (e-runtime-store--closed fixture)))
                (ignore-errors (e-runtime-store-close fixture)))))
          (e-runtime-store-recovery-graphical--prepare-frame)
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'open)
          (setq worker-file
                (e-runtime-store-recovery-graphical--make-v6-worker))
          (setq heartbeat-timer
                (run-at-time 0.01 0.01 (lambda () (cl-incf heartbeat))))
          (let ((started (float-time)))
            (cl-letf (((symbol-function 'e-runtime-store--worker-file)
                       (lambda () worker-file))
                      ((symbol-function 'e-runtime-store--command)
                       (lambda ()
                         (list (e-runtime-store--emacs-program) "--batch" "-Q"
                               "-L" (file-name-directory
                                     (e-runtime-store--worker-file))
                               "-L" (file-name-directory
                                     (expand-file-name
                                      "lisp/core/e-runtime-store-worker.el"
                                      (e-source-directory)))
                               "--eval" "(setq load-prefer-newer t)"
                               "-l" worker-file
                               "--funcall" "e-runtime-store-worker-main"))))
              ;; This is the public startup edge; it must return while the
              ;; private open control is still held by the worker.
              (should-not (e-default--prewarm-runtime)))
            (should (< (- (float-time) started) 0.5)))
          (setq transport e-default--runtime-store)
          (should (e-runtime-store-p transport))
          (should-not e-default--runtime)
          (should-not e-default--chat-sessions)
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (< (float-time) deadline)
                        (not (e-runtime-store-recovery-graphical--stall-ready-p
                              stall-directory 'open)))
              (e-runtime-store-recovery-graphical--pump-runtime transport)
              (sit-for 0.01))
            (unless (e-runtime-store-recovery-graphical--stall-ready-p
                     stall-directory 'open)
              (ert-fail
               (format
                "Startup open did not reach stall: process=%S status=%S stderr=%S last-error=%S"
                (and (e-runtime-store--process transport)
                     (process-status (e-runtime-store--process transport)))
                (and (e-runtime-store--process transport)
                     (process-exit-status (e-runtime-store--process transport)))
                (and (buffer-live-p (e-runtime-store--stderr-buffer transport))
                     (with-current-buffer (e-runtime-store--stderr-buffer transport)
                       (buffer-substring-no-properties
                        (max (point-min) (- (point-max) 2000)) (point-max))))
                (plist-get (e-runtime-store-status transport) :last-error)))))
          (e-graphical-test-wait-until
           (lambda () (> heartbeat 3))
           1.0 "independent startup heartbeats")
          (let* ((active (e-runtime-store--active-request transport))
                 (status (e-runtime-store-status transport))
                 (pending nil)
                 (stderr
                  (and (buffer-live-p
                        (e-runtime-store--stderr-buffer transport))
                       (with-current-buffer
                           (e-runtime-store--stderr-buffer transport)
                         (buffer-substring-no-properties
                          (max (point-min) (- (point-max) 2000))
                          (point-max))))))
            (maphash
             (lambda (_id request)
               (push (list :id (e-runtime-store-request--id request)
                           :kind (e-runtime-store-request--kind request)
                           :operation
                           (e-runtime-store-request--operation request)
                           :state (e-runtime-store-request--state request))
                     pending))
             (e-runtime-store--pending transport))
            (unless (and active
                         (eq (e-runtime-store-request--kind active) 'open)
                         (eq (e-runtime-store-request--state active)
                             'submitted))
              (ert-fail
               (format
                "Held startup open lost before heartbeat gate: active=%S pending=%S queue=%S status=%S process=%S stderr=%S"
                (and active
                     (list :id (e-runtime-store-request--id active)
                           :kind (e-runtime-store-request--kind active)
                           :operation
                           (e-runtime-store-request--operation active)
                           :state (e-runtime-store-request--state active)))
                pending
                (mapcar
                 (lambda (request)
                   (list :id (e-runtime-store-request--id request)
                         :kind (e-runtime-store-request--kind request)
                         :operation
                         (e-runtime-store-request--operation request)
                         :state (e-runtime-store-request--state request)))
                 (e-runtime-store--client-queue transport))
                status
                (and (e-runtime-store--process transport)
                     (list (process-status (e-runtime-store--process transport))
                           (process-exit-status
                            (e-runtime-store--process transport))))
                stderr))))
          (should-not (e-runtime-store--client-queue transport))
          (let (operations)
            (maphash
             (lambda (_id request)
               (push (e-runtime-store-request--kind request) operations))
             (e-runtime-store--pending transport))
            (should (equal operations '(open))))
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'open)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime transport)
             (let ((status (e-runtime-store-status transport)))
               (and (plist-get status :last-error)
                    (not (e-runtime-store--active-request transport)))))
           2.0 "v5 startup diagnostic")
          (let* ((status (e-runtime-store-status transport))
                 (diagnostic (plist-get status :last-error)))
            (should (eq (car diagnostic) 'e-runtime-store-schema-too-old))
            (should (= (plist-get (cdr diagnostic) :actual) 5))
            (should (= (plist-get (cdr diagnostic) :required) 6))
            (should (equal (plist-get (cdr diagnostic) :operation)
                           'e-runtime-store-offline-upgrade)))
          (should-not (e-runtime-store--client-queue transport)))
      (when (timerp heartbeat-timer)
        (cancel-timer heartbeat-timer))
      (e-runtime-store-recovery-graphical--release-stall
       stall-directory 'open)
      (when (e-runtime-store-p e-default--runtime-store)
        (cl-letf (((symbol-function 'e-runtime-store-await)
                   #'e-runtime-store-recovery-graphical--await-with-pump))
          (ignore-errors (e-default-runtime-close))))
      (when (and worker-file (file-exists-p worker-file))
        (delete-file worker-file))
      (delete-directory directory t)
      (delete-directory stall-directory t))))

(ert-deftest e-runtime-store-recovery-graphical-s92-org-canvas-turn-survives-delayed-persistence ()
  "A new public Org Canvas Daily stays usable while SQLite admission waits."
  ;; The graphical runner loads `e' during interactive daemon startup.  Prove
  ;; that this public process has already begun opening its default transport
  ;; before any Daily/harness lookup in the scenario below.
  (should (e-runtime-sqlite-p e-default--runtime))
  (should-not (e-runtime-sqlite--closed e-default--runtime))
  (should (memq
           (plist-get
            (e-runtime-sqlite-status e-default--runtime) :startup)
           '(opening ready)))
  (e-board-e2e-reset-runtime)
  (let* ((directory (make-temp-file "e-runtime-store-org-canvas-" t))
         (canvas-directory (make-temp-file "e-org-canvas-daily-" t))
         (stall-directory (make-temp-file "e-runtime-store-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         sessions reader runtime stream harness target input backing-chat session-id
         failure-target failure-input failure-chat failure-id
         unrelated-target unrelated-input unrelated-chat unrelated-id heartbeat-timer
         first-response second-response
         (heartbeat 0)
         (e-org-canvas-input-auto-close-delay nil)
         synchronous-operation service-events service-subscription)
    (unwind-protect
        (progn
          ;; Server-hosted graphical ERT runs inside an Emacs process filter;
          ;; pump the disposable worker explicitly during synchronous fixture
          ;; setup so nested process-filter suppression cannot manufacture a
          ;; 120-second open/cleanup timeout before the behavior under test.
          (setq sessions
                (cl-letf (((symbol-function 'e-runtime-store-await)
                           #'e-runtime-store-recovery-graphical--await-with-pump))
                  (e-session-sqlite-store-create directory))
                runtime (e-session-storage-runtime-store sessions)
                stream (e-graphical-test-stream-create)
                harness (e-harness-create
                         :backend (e-graphical-test-stream-backend stream)
                         :sessions sessions)
                target
                (find-file-noselect
                 (expand-file-name "2026-09-05.org" canvas-directory)))
          (e-session-enable sessions)
          ;; Match the public default harness's Org Canvas layer.  The injected
          ;; stream replaces only the network backend; context still comes from
          ;; the real gated capability used by production Daily.
          (e-harness-set-intrinsic-capabilities
           harness
           (append (e-harness-intrinsic-capabilities harness)
                   (e-layer-capabilities (e-org-canvas-layer-create))))
          (with-current-buffer target
            (org-mode)
            (insert "* Daily\n"
                    "Project codename: Juniper\n"
                    "Alert color: amber\n"
                    "Amber action: request a human review.\n")
            (save-buffer))
          (e-runtime-store-recovery-graphical--prepare-frame)
          (set-window-buffer (selected-window) target)
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'board-create)
          (setq heartbeat-timer
                (run-at-time 0.01 0.01 (lambda () (cl-incf heartbeat))))
          ;; This is the command Grimoire Daily invokes on a fresh Org buffer.
          ;; No test-only session or Canvas binding exists before this call.
          (setq input
                (with-timeout
                    (1.0 (error "Public Org Canvas Daily open blocked"))
                  (cl-letf (((symbol-function 'e-org-canvas--default-harness)
                             (lambda () harness))
                            ((symbol-function 'e-runtime-store-await)
                             (lambda (_store request &optional _timeout)
                               (setq synchronous-operation
                                     (e-runtime-store-request--operation
                                      request))
                               (error "Org Canvas open awaited %S"
                                      synchronous-operation))))
                    (with-current-buffer target
                      (e-org-canvas-prompt-document)))))
          (should (buffer-live-p input))
          (setq session-id
                (buffer-local-value 'e-org-canvas-input--session-id input))
          (setq backing-chat
                (cl-find-if
                 (lambda (buffer)
                   (with-current-buffer buffer
                     (and (derived-mode-p 'e-chat-mode)
                          (not (derived-mode-p 'e-org-canvas-input-mode))
                          (equal e-chat-session-id session-id))))
                 (buffer-list)))
          (should (stringp session-id))
          (should (buffer-live-p backing-chat))
          (should (equal (buffer-local-value 'e-org-canvas-session-id target)
                         session-id))
          (should (eq (window-buffer (selected-window)) input))
          (setq service-subscription
                (e-chat-service-subscribe
                 harness session-id
                 (lambda (event) (push (plist-get event :type) service-events))))
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-runtime-store-recovery-graphical--stall-ready-p
              stall-directory 'board-create))
           2.0 "Org Canvas new-session admission")
          (should
           (e-runtime-store-recovery-graphical--runtime-operation-p
            runtime 'board-participant-put))
          (should
           (e-runtime-store-recovery-graphical--runtime-operation-p
            runtime 'session-append-batch))
          (with-current-buffer input
            (goto-char (point-max))
            (e-graphical-test-type-text
             (concat "Remember this Daily decision: project Juniper uses alert "
                     "amber. Confirm both.")))
          (should (> heartbeat 3))
          (should-not
           (e-board-mutation-frozen-p
            (e-board-registry-board-source-board
             (e-chat-service-binding-board
              (e-chat-service-binding harness session-id)))))
          (when synchronous-operation
            (ert-fail (format "Org Canvas interactive path awaited %S"
                              synchronous-operation)))
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'board-create)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (eq (plist-get
                  (e-work-status
                   (e-chat-service-binding-readiness-work
                    (e-chat-service-binding harness session-id)))
                  :state)
                 'finished))
           3.0 "Org Canvas session readiness")
          ;; Hold ordinary transcript persistence only after composite
          ;; readiness.  Both provider turns must consume the installed
          ;; projection while their physical writes remain blocked.
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'session-append)
          (with-current-buffer input
            (cl-letf (((symbol-function 'e-runtime-store-await)
                       (lambda (_store request &optional _timeout)
                         (setq synchronous-operation
                               (e-runtime-store-request--operation request))
                         (error "Org Canvas turn awaited %S"
                                synchronous-operation))))
              (with-timeout
                  (1.0 (error "Public first Org Canvas submit blocked"))
                (e-org-canvas-input-submit))))
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-runtime-store-recovery-graphical--runtime-operation-p
              runtime 'board-routing-put))
           1.0 "same-Board classification timer admission")
          (e-graphical-test-wait-until
           (lambda () (e-graphical-test-stream-active-p stream))
           1.0 "Org Canvas provider request after release")
          (let* ((requests (e-graphical-test-stream-requests stream))
                 (wire (prin1-to-string
                        (plist-get (car requests) :messages))))
            (should (= (length requests) 1))
            (should (string-match-p "Org Canvas mode is active" wire))
            (should (string-match-p "document-uri=.*2026-09-05.org" wire))
            (should
             (string-match
              "project \\([[:alpha:]]+\\) uses alert \\([[:alpha:]]+\\)"
              wire))
            ;; Build the fake provider answer from the actual request.  A
            ;; canned stream cannot satisfy this assertion or the next turn.
            (setq first-response
                  (format "Confirmed: project %s uses alert %s."
                          (match-string 1 wire)
                          (match-string 2 wire))))
          (cl-letf (((symbol-function 'e-runtime-store-await)
                     (lambda (_store request &optional _timeout)
                       (unless synchronous-operation
                         (setq synchronous-operation
                               (e-runtime-store-request--operation request)))
                       (error "Org Canvas callback awaited %S"
                              synchronous-operation))))
            (e-graphical-test-stream-emit
             stream '(:type reasoning-delta :content "reading the Daily facts")
             0.01)
            (e-graphical-test-stream-emit
             stream (list :type 'assistant-message :content first-response)
             0.02)
            (e-graphical-test-stream-finish stream 0.03)
            (e-graphical-test-wait-until
             (lambda ()
               (or synchronous-operation
                   (e-graphical-test-stream-failure stream)
                   (not (e-graphical-test-stream-active-p stream))))
             2.0 "Org Canvas provider callback after delayed admission"))
          (when synchronous-operation
            (ert-fail (format "Org Canvas callback awaited %S"
                              synchronous-operation)))
          (should-not (e-graphical-test-stream-failure stream))
          (with-current-buffer input
            (should-not (string-match-p "Turn failed" (buffer-string))))
          (e-graphical-test-wait-until
           (lambda ()
             (with-current-buffer input
               (string-match-p (regexp-quote first-response) (buffer-string))))
           2.0 "first data-dependent Org Canvas response")
          ;; Each Org Canvas prompt is a public one-shot composer.  Reopen the
          ;; Daily prompt for the already-bound session instead of mutating the
          ;; submitted result pane back into a composer.
          (setq input
                (with-timeout
                    (1.0 (error "Public second Org Canvas prompt blocked"))
                  (cl-letf (((symbol-function 'e-org-canvas--default-harness)
                             (lambda () harness))
                            ((symbol-function 'e-runtime-store-await)
                             (lambda (_store request &optional _timeout)
                               (setq synchronous-operation
                                     (e-runtime-store-request--operation
                                      request))
                               (error "Second Org Canvas prompt awaited %S"
                                      synchronous-operation))))
                    (with-current-buffer target
                      (e-org-canvas-prompt-document)))))
          (should (eq (window-buffer (selected-window)) input))
          (with-current-buffer input
            (goto-char (point-max))
            (e-graphical-test-type-text
             (concat "Using the alert from our previous turn, which project "
                     "needs a human review?"))
            (cl-letf (((symbol-function 'e-runtime-store-await)
                       (lambda (_store request &optional _timeout)
                         (setq synchronous-operation
                               (e-runtime-store-request--operation request))
                         (error "Second Org Canvas turn awaited %S"
                                synchronous-operation))))
              (with-timeout
                  (1.0 (error "Public second Org Canvas submit blocked"))
                (e-org-canvas-input-submit))))
          (e-graphical-test-wait-until
           (lambda () (e-graphical-test-stream-active-p stream))
           1.0 "second Org Canvas provider request while SQLite delayed")
          (let* ((requests (e-graphical-test-stream-requests stream))
                 (wire (prin1-to-string
                        (plist-get (cadr requests) :messages))))
            (should (= (length requests) 2))
            (should (string-match-p (regexp-quote first-response) wire))
            (should (string-match-p
                     "which project needs a human review" wire))
            (should
             (string-match
              "Confirmed: project \\([[:alpha:]]+\\) uses alert \\([[:alpha:]]+\\)"
              wire))
            (setq second-response
                  (format "Request a human review for %s because %s requires it."
                          (match-string 1 wire)
                          (match-string 2 wire))))
          (e-graphical-test-stream-emit
           stream (list :type 'assistant-message :content second-response)
           0.01)
          (e-graphical-test-stream-finish stream 0.02)
          (e-graphical-test-wait-until
           (lambda ()
             (or (e-graphical-test-stream-failure stream)
                 (not (e-graphical-test-stream-active-p stream))))
           2.0 "second data-dependent provider completion")
          (should-not (e-graphical-test-stream-failure stream))
          (should (> heartbeat 3))
          (should (e-session-async-pending-p sessions session-id))
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'session-append)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (and (not (e-session-async-pending-p sessions session-id))
                  (null (e-runtime-store--active-request runtime))
                  (null (e-runtime-store--client-queue runtime))))
           3.0 "Org Canvas persistence drain")
          (should (= (e-runtime-store-recovery-graphical--count-string
                      first-response backing-chat)
                     1))
          (should (= (e-runtime-store-recovery-graphical--count-string
                      second-response backing-chat)
                     1))
          (should
           (cl-find-if
            (lambda (event)
              (eq (plist-get event :event-type) 'reasoning-delta))
            (e-session-activity-events sessions session-id)))
          (should
           (cl-find-if
            (lambda (message)
              (equal (plist-get message :content)
                     second-response))
            (e-session-messages sessions session-id)))

          ;; Fail a second real Org Canvas Daily during its session admission.
          ;; Only that session/Board owner becomes suspect; the first Daily and
          ;; a third unrelated Daily remain available.
          (setq failure-target
                (find-file-noselect
                 (expand-file-name "failure.org" canvas-directory)))
          (with-current-buffer failure-target
            (org-mode)
            (insert "* Failure isolation\n")
            (save-buffer))
          (e-runtime-store-recovery-graphical--prepare-frame)
          (set-window-buffer (selected-window) failure-target)
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'session-append-batch)
          (setq failure-input
                (with-timeout
                    (1.0 (error "Failure Daily open blocked"))
                  (cl-letf (((symbol-function 'e-org-canvas--default-harness)
                             (lambda () harness))
                            ((symbol-function 'e-runtime-store-await)
                             (lambda (_store request &optional _timeout)
                               (setq synchronous-operation
                                     (e-runtime-store-request--operation
                                      request))
                               (error "Failure Daily open awaited %S"
                                      synchronous-operation))))
                    (with-current-buffer failure-target
                      (e-org-canvas-prompt-document)))))
          (setq failure-id
                (buffer-local-value
                 'e-org-canvas-input--session-id failure-input)
                failure-chat
                (cl-find-if
                 (lambda (buffer)
                   (with-current-buffer buffer
                     (and (derived-mode-p 'e-chat-mode)
                          (not (derived-mode-p 'e-org-canvas-input-mode))
                          (equal e-chat-session-id failure-id))))
                 (buffer-list)))
          (when synchronous-operation
            (ert-fail (format "Failure Daily open awaited %S"
                              synchronous-operation)))
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-runtime-store-recovery-graphical--stall-ready-p
              stall-directory 'session-append-batch))
           2.0 "failure Daily session mutation")
          (let ((failed-process (e-runtime-store--process runtime)))
            (delete-process failed-process)
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--pump-runtime runtime)
               (let ((replacement (e-runtime-store--process runtime)))
                 (and replacement
                      (not (eq replacement failed-process))
                      (process-live-p replacement)
                      (e-runtime-store-recovery-graphical--runtime-operation-p
                       runtime 'session-append-batch))))
             2.0 "failure Daily same-owner retry")
            (delete-process (e-runtime-store--process runtime)))
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (and (e-session-persistence-suspect sessions failure-id)
                  (buffer-local-value
                   'e-org-canvas--persistence-warning failure-target)))
           2.0 "failure Daily visible owner-local warning")
          (should-not
           (e-session-persistence-suspect sessions session-id))
          (should
           (e-chat-service-binding-first-persistence-error
            (e-chat-service-binding harness failure-id)))
          (let ((warning
                 (buffer-local-value
                  'e-org-canvas--persistence-warning failure-target)))
            (should (string-match-p "persistence suspect" warning))
            (should (<= (string-bytes warning) 1152))
            (should
             (string-match-p
              "persistence suspect"
              (format "%s" (buffer-local-value 'mode-name failure-target)))))

          ;; Release the test stall and prove a new public Canvas session can
          ;; lazily start a worker, persist, and survive an independent reopen.
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'session-append-batch)
          (setq unrelated-target
                (find-file-noselect
                 (expand-file-name "unrelated.org" canvas-directory)))
          (with-current-buffer unrelated-target
            (org-mode)
            (insert "* Unrelated Daily\n")
            (save-buffer))
          (e-runtime-store-recovery-graphical--prepare-frame)
          (set-window-buffer (selected-window) unrelated-target)
          (setq unrelated-input
                (with-timeout
                    (1.0 (error "Unrelated Daily open blocked"))
                  (cl-letf (((symbol-function 'e-org-canvas--default-harness)
                             (lambda () harness))
                            ((symbol-function 'e-runtime-store-await)
                             (lambda (_store request &optional _timeout)
                               (setq synchronous-operation
                                     (e-runtime-store-request--operation
                                      request))
                               (error "Unrelated Daily open awaited %S"
                                      synchronous-operation))))
                    (with-current-buffer unrelated-target
                      (e-org-canvas-prompt-document)))))
          (setq unrelated-id
                (buffer-local-value
                 'e-org-canvas-input--session-id unrelated-input)
                unrelated-chat
                (cl-find-if
                 (lambda (buffer)
                   (with-current-buffer buffer
                     (and (derived-mode-p 'e-chat-mode)
                          (not (derived-mode-p 'e-org-canvas-input-mode))
                          (equal e-chat-session-id unrelated-id))))
                 (buffer-list)))
          (when synchronous-operation
            (ert-fail (format "Unrelated Daily open awaited %S"
                              synchronous-operation)))
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (eq (plist-get
                  (e-work-status
                   (e-chat-service-binding-readiness-work
                    (e-chat-service-binding harness unrelated-id)))
                  :state)
                 'finished))
           3.0 "unrelated Daily lazy worker replacement")
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (and (not (e-session-async-pending-p sessions session-id))
                  (not (e-session-async-pending-p sessions failure-id))
                  (not (e-session-async-pending-p sessions unrelated-id))
                  (null (e-runtime-store--active-request runtime))
                  (null (e-runtime-store--client-queue runtime))
                  (null (e-runtime-store--recovering-request runtime))))
           3.0 "unrelated Daily persistence drain")
          (e-runtime-store--close-start runtime)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-runtime-store--closed runtime))
           2.0 "writer retirement before independent readback")
          (setq reader
                (cl-letf
                    (((symbol-function 'e-runtime-store-await)
                      #'e-runtime-store-recovery-graphical--await-with-pump))
                  (e-session-sqlite-store-create directory :load-all t)))
          (should (equal (plist-get (e-session-get reader unrelated-id) :id)
                         unrelated-id))
          (should
           (plist-get
            (plist-get (e-session-get reader unrelated-id) :metadata)
            :org-canvas-ref)))
      (when (timerp heartbeat-timer) (cancel-timer heartbeat-timer))
      (when (e-graphical-test-stream-p stream)
        (e-graphical-test-stream-cancel stream))
      (when service-subscription
        (e-chat-service-unsubscribe service-subscription))
      (ignore-errors
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'board-create)
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'session-append)
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'session-append-batch))
      (dolist (buffer (list input backing-chat target
                            failure-input failure-chat failure-target
                            unrelated-input unrelated-chat unrelated-target))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (when reader
        (ignore-errors
          (e-runtime-store--finalize-close
           (e-session-storage-runtime-store reader))))
      (when runtime
        (ignore-errors (e-runtime-store--finalize-close runtime)))
      (delete-directory directory t)
      (delete-directory canvas-directory t)
      (delete-directory stall-directory t))))

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
          (setq sessions
                (cl-letf (((symbol-function 'e-runtime-store-await)
                           #'e-runtime-store-recovery-graphical--await-with-pump))
                  (e-session-sqlite-store-create directory)))
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
          (setq sessions
                (cl-letf (((symbol-function 'e-runtime-store-await)
                           #'e-runtime-store-recovery-graphical--await-with-pump))
                  (e-session-sqlite-store-create directory))
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
