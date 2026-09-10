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
(require 'e-openai-decoder)
(require 'e-org-canvas)
(require 'e-session)
(require 'e-runtime-store-codec)
(require 'e-runtime-store-offline)
(require 'sqlite)
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
  ;; An acceptance scenario may exercise the same SQL operation in separate
  ;; phases.  A new hold must not inherit the prior phase's observation or
  ;; release marker.
  (dolist (suffix '("ready" "release"))
    (let ((marker
           (e-runtime-store-recovery-graphical--stall-file
            directory operation suffix)))
      (when (file-exists-p marker)
        (delete-file marker))))
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

(defun e-runtime-store-recovery-graphical--finalize-private-transport
    (transport)
  "Retire isolated TRANSPORT without starting a compatibility close cycle.

The startup witness deliberately leaves its v6 worker fenced after reporting
the v5 diagnostic.  Its runner owns this disposable transport, so teardown
must finish the private state directly instead of asking the compatibility
close wrapper to submit another request to a non-unavailable store."
  (when (and (e-runtime-store-p transport)
             (not (e-runtime-store--closed transport)))
    (e-runtime-store--finalize-close transport)))

(defun e-runtime-store-recovery-graphical--stall-ready-p
    (directory operation)
  "Return non-nil when isolated worker reached OPERATION's stall."
  (file-exists-p
   (e-runtime-store-recovery-graphical--stall-file
    directory operation "ready")))

(defun e-runtime-store-recovery-graphical--read-startup-report (path)
  "Read the bounded startup report at PATH with evaluation disabled."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((read-eval nil))
      (read (current-buffer)))))

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
  "Dispatch one bounded worker-output turn for RUNTIME inside server ERT.
Include its subordinate read connection because server-hosted ERT suppresses
otherwise reentrant process-filter delivery for both transports."
  (when-let* ((process (e-runtime-store--process runtime))
              ((process-live-p process)))
    (accept-process-output process 0.01))
  (when-let* ((reader (e-runtime-store--read-client runtime)))
    (e-runtime-store-recovery-graphical--pump-runtime reader)))

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

(defun e-runtime-store-recovery-graphical--fixture-payload (value)
  "Return VALUE in the v6 session-record payload representation."
  (base64-encode-string (e-runtime-store-codec-encode value) t))

(defun e-runtime-store-recovery-graphical--make-daily-v6-fixture (directory)
  "Create a disposable v6 store with one Daily and 1277 unrelated rows.

The first runtime only creates the production schema.  It is closed before
the direct fixture insert so the test never races SQLite against the worker;
the second runtime is the one observed by the public chat path.  The direct
insert is intentionally test-only and creates no process-local session
aggregate or mirror."
  (let ((bootstrap (e-runtime-store-open directory))
        (database nil)
        (database-file (expand-file-name "store.sqlite3" directory))
        (unrelated-count 1277)
        (daily-id "daily-known")
        (daily-created "2026-09-06T00:00:00Z")
        (daily-updated "2026-09-06T00:00:03Z")
        (committed nil))
    (unwind-protect
        (progn
          ;; A status read is only a bounded schema/open acknowledgement; it
          ;; ensures no worker still owns the file when fixture SQL begins.
          ;; Server-hosted graphical ERT suppresses reentrant filters, so this
          ;; offline fixture boundary must pump both writer and read transports.
          (cl-letf (((symbol-function 'e-runtime-store-await)
                     #'e-runtime-store-recovery-graphical--await-with-pump))
            (e-runtime-store-call bootstrap 'read '(:op status))
            (e-runtime-store-close bootstrap))
          (setq bootstrap nil)
          (setq database (sqlite-open database-file))
          (sqlite-execute database "BEGIN IMMEDIATE")
          (unwind-protect
              (progn
                (sqlite-execute
                 database
                 (concat
                  "INSERT INTO session_records(session_id,position,payload,"
                  "record_type,record_id,record_identity,parent_id,timestamp)"
                  " VALUES(?,?,?,?,?,?,?,?)")
                 (vector daily-id 1
                         (e-runtime-store-recovery-graphical--fixture-payload
                          (list :type "session" :session-id daily-id
                                :id "daily-root" :created-at daily-created
                                :updated-at daily-updated :metadata nil))
                         "session" "daily-root" "daily-root-delta" nil
                         daily-created))
                (dolist
                    (record
                     (list
                      (list :type "message" :session-id daily-id
                            :id "daily-m1" :parent-id "daily-root"
                            :timestamp "2026-09-06T00:00:01Z"
                            :message (list :type 'message :id "daily-m1"
                                           :role 'user :content "Daily user message"
                                           :created-at "2026-09-06T00:00:01Z"
                                           :parent-id "daily-root"))
                      (list :type "message" :session-id daily-id
                            :id "daily-m2" :parent-id "daily-m1"
                            :timestamp "2026-09-06T00:00:02Z"
                            :message (list :type 'message :id "daily-m2"
                                           :role 'assistant
                                           :content "Daily assistant message"
                                           :created-at "2026-09-06T00:00:02Z"
                                           :parent-id "daily-m1"))))
                  (let* ((position (if (equal (plist-get record :id) "daily-m1")
                                       2 3))
                         (record-id (plist-get record :id)))
                    (sqlite-execute
                     database
                     (concat
                      "INSERT INTO session_records(session_id,position,payload,"
                      "record_type,record_id,record_identity,parent_id,timestamp)"
                      " VALUES(?,?,?,?,?,?,?,?)")
                     (vector daily-id position
                             (e-runtime-store-recovery-graphical--fixture-payload
                              record)
                             "message" record-id
                             (format "%s-delta" record-id)
                             (plist-get record :parent-id)
                             (plist-get record :timestamp)))))
                (sqlite-execute
                 database
                 (concat
                  "INSERT INTO session_query_state(session_id,name,summary,"
                  "metadata,created_at,updated_at,last_message_at,"
                  "latest_assistant_marker,message_count,current_branch,"
                  "turn_options,current_head_id,root_event_id,board_id,"
                  "principal,association_role,routing_policy,root_p,"
                  "board_output_sequence,board_activity_sequence,journal_position)"
                  " VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
                 (vector daily-id "Daily" "Fixture Daily" nil daily-created
                         daily-updated "2026-09-06T00:00:02Z" "daily-m2" 2
                         "main" nil "daily-m2" "daily-root" "daily-board"
                         "operator" "owner"
                         (e-runtime-store-recovery-graphical--fixture-payload
                          (list :participant-id "daily-main"
                                :pickup-selector '(:tags (main))
                                :observer-selector '(:tags (main))
                                :default-tags '(main) :default-to nil))
                         1 0 0 3))
                (sqlite-execute
                 database
                 (concat
                  "INSERT INTO boards(board_id,trusted_principal,generation,"
                  "revision,next_position,root_payload) VALUES(?,?,?,?,?,?)")
                 (vector
                  "daily-board"
                  (e-runtime-store-recovery-graphical--fixture-payload
                   "operator")
                  1 2 0
                  (e-runtime-store-recovery-graphical--fixture-payload
                   '(:board-id "daily-board"))))
                (sqlite-execute
                 database
                 (concat
                  "INSERT INTO board_participants(board_id,generation,"
                  "participant_id,payload,revision) VALUES(?,?,?,?,?)")
                 (vector
                  "daily-board" 1 "daily-main"
                  (e-runtime-store-recovery-graphical--fixture-payload
                   (list :id "daily-main" :author "e-chat"
                         :principal "operator" :controller "operator"
                         :role 'owner :state 'dormant
                         :subscription-id "daily-address"
                         :publication-pending nil))
                  1))
                (dotimes (index unrelated-count)
                  (let ((session-id (format "unrelated-%04d" index))
                        (root-id (format "unrelated-%04d-root" index))
                        (timestamp (format "2025-01-01T00:%02d:%02dZ"
                                           (/ index 60) (% index 60))))
                    ;; Keep the scale fixture relationally honest: every
                    ;; query row has its canonical session root.  The roots
                    ;; are unrelated to the requested Daily, so the query
                    ;; path still proves exact-session filtering.
                    (sqlite-execute
                     database
                     (concat
                      "INSERT INTO session_records(session_id,position,payload,"
                      "record_type,record_id,record_identity,parent_id,timestamp)"
                      " VALUES(?,?,?,?,?,?,?,?)")
                     (vector session-id 1
                             (e-runtime-store-recovery-graphical--fixture-payload
                              (list :type "session" :session-id session-id
                                    :id root-id :timestamp timestamp
                                    :created-at timestamp :updated-at timestamp
                                    :metadata nil))
                             "session" root-id nil nil timestamp))
                    (sqlite-execute
                     database
                     (concat
                      "INSERT INTO session_query_state(session_id,name,summary,"
                      "metadata,created_at,updated_at,last_message_at,"
                      "latest_assistant_marker,message_count,current_branch,"
                      "turn_options,current_head_id,root_event_id,board_id,"
                      "principal,association_role,routing_policy,root_p,"
                      "board_output_sequence,board_activity_sequence,journal_position)"
                      " VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
                     (vector session-id (format "Unrelated %04d" index) nil nil
                             timestamp timestamp nil nil 0 "main" nil root-id
                             root-id nil nil nil nil 1 0 0 1))))
                (sqlite-execute database "COMMIT")
                (setq committed t))
            (unless committed
              (ignore-errors (sqlite-execute database "ROLLBACK"))))
          unrelated-count)
      (when database
        (ignore-errors (sqlite-close database)))
      (when (e-runtime-store-p bootstrap)
        (ignore-errors (e-runtime-store--finalize-close bootstrap))))))

(defun e-runtime-store-recovery-graphical--make-upgrade-v5-fixture (directory)
  "Create a nonempty v5 store with observed legacy journal shapes."
  (let* ((database-file (expand-file-name "store.sqlite3" directory))
         (database (sqlite-open database-file))
         (session-id "graphical-v5-existing")
         (rootless-id "graphical-v5-rootless")
         (timestamp "2026-09-07T00:00:00Z")
         (large-content (make-string 9000 ?m))
         (records
          (list
           (list :type "session" :session-id session-id :id "root-1"
                 :timestamp timestamp :created-at timestamp
                 :updated-at timestamp :metadata '(:name "Before upgrade"))
           (list :type "message" :session-id session-id :id "message-1"
                 :parent-id "root-1" :timestamp timestamp
                 :message (list :id "message-1" :role 'user
                                :created-at timestamp :content large-content
                                :provider-payload (make-string 12000 ?p)))
           (list :type "probe" :session-id session-id :id "legacy-probe"
                 :timestamp timestamp :payload "ignored historically")
           (list :type "session" :session-id session-id :id "root-2"
                 :timestamp timestamp :created-at timestamp
                 :updated-at timestamp :name "After replacement"
                 :metadata '(:name "After replacement")))))
    (unwind-protect
        (progn
          (dolist
              (statement
               '("CREATE TABLE store_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
                 "CREATE TABLE session_records (session_id TEXT NOT NULL, position INTEGER NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(session_id, position))"
                 "CREATE TABLE session_checkpoints (session_id TEXT PRIMARY KEY, payload TEXT NOT NULL, revision INTEGER NOT NULL)"
                 "CREATE TABLE catalog_projection (singleton INTEGER PRIMARY KEY CHECK(singleton = 1), payload TEXT NOT NULL, revision INTEGER NOT NULL)"))
            (sqlite-execute database statement))
          (sqlite-execute
           database "INSERT INTO store_meta(key,value) VALUES('schema_version','5')")
          (let ((position 0))
            (dolist (record records)
              (cl-incf position)
              (sqlite-execute
               database
               "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
               (vector session-id position
                       (e-runtime-store-recovery-graphical--fixture-payload
                        record)))))
          (sqlite-execute
           database
           "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
           (vector
            rootless-id 1
            (e-runtime-store-recovery-graphical--fixture-payload
             (list :type "board-message" :session-id rootless-id
                   :message '(:id "rootless-message" :kind "input"
                              :author "client")))))
          ;; These are intentionally stale copies.  The journal is the v5
          ;; authority, so v6 migration must ignore and remove both relations.
          (sqlite-execute
           database
           "INSERT INTO catalog_projection(singleton,payload,revision) VALUES(1,?,1)"
           (vector
            (e-runtime-store-recovery-graphical--fixture-payload
             (list (list :id session-id :name "stale catalog"
                         :summary large-content)))))
          (sqlite-execute
           database
           "INSERT INTO session_checkpoints(session_id,payload,revision) VALUES(?,?,1)"
           (vector
            session-id
            (e-runtime-store-recovery-graphical--fixture-payload
             (list :session-id "wrong-owner" :root '(:id "wrong")))))
          (sqlite-close database)
          (setq database nil)
          (set-file-modes database-file #o600)
          (list :session-id session-id :rootless-id rootless-id))
      (when database (sqlite-close database)))))

(defun e-runtime-store-recovery-graphical--runtime-operations (runtime)
  "Return active and transport-buffered operations for RUNTIME and its reader."
  (append
   (mapcar
    #'e-runtime-store-request--operation
    (append (and (e-runtime-store--active-request runtime)
                 (list (e-runtime-store--active-request runtime)))
            (e-runtime-store--client-queue runtime)))
   (when-let* ((reader (e-runtime-store--read-client runtime)))
     (e-runtime-store-recovery-graphical--runtime-operations reader))))

(ert-deftest e-runtime-store-recovery-graphical-s92-startup-prewarm-is-transport-only ()
  "Graphical startup survives held open and reports an unupgraded v5 store."
  (let* ((directory (getenv "E_RUNTIME_STATE_DIRECTORY"))
         (stall-directory (getenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY"))
         (report-file (getenv "E_GRAPHICAL_E2E_STARTUP_REPORT"))
         (database-file (and directory
                             (expand-file-name "store.sqlite3" directory)))
         transport
         heartbeat-timers
         (heartbeat 0))
    (if (not (equal (getenv "E_GRAPHICAL_E2E_SELECTOR") "startup-prewarm"))
        (ert-skip
         "startup-prewarm requires the runner's pre-e startup fixture")
      (unless (and directory stall-directory report-file
                   (file-directory-p directory)
                   (file-directory-p stall-directory)
                   (file-regular-p report-file))
        (ert-fail
         (format
          "startup-prewarm runner promised fixture paths, but they are missing: directory=%S stall-directory=%S report-file=%S"
          directory stall-directory report-file))))
    ;; Capture the runner-owned transport before any assertion can transfer
    ;; control to the unwind cleanup.
    (setq transport e-default--runtime-store)
    (unwind-protect
        (progn
          ;; The runner created the honest nonempty v5 fixture, open hold, and
          ;; report channel before the daemon loaded `e'.  The report is
          ;; written by the real default prewarm, not by this ERT body.
          (let ((report
                 (e-runtime-store-recovery-graphical--read-startup-report
                  report-file)))
            (should (plist-get report :prewarm-called))
            (should (equal (plist-get report :directory)
                           (file-name-as-directory
                            (expand-file-name directory))))
            (should (equal (plist-get (plist-get report :active) :kind)
                           'open))
            (should (eq (plist-get (plist-get report :active) :state)
                        'submitted))
            (should (equal (plist-get report :pending)
                           '((:kind open :operation nil :state submitted))))
            (should-not (plist-get report :queue)))
          (should (file-regular-p database-file))
          (should (> (file-attribute-size (file-attributes database-file)) 0))
          (should (e-runtime-store-p e-default--runtime-store))
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
          ;; Schedule three independent one-shot callbacks rather than using a
          ;; repeating timer, so progress cannot be explained by one callback
          ;; retaining control of the event loop.
          (setq heartbeat-timers
                (list
                 (run-at-time 0.01 nil (lambda () (cl-incf heartbeat)))
                 (run-at-time 0.02 nil (lambda () (cl-incf heartbeat)))
                 (run-at-time 0.03 nil (lambda () (cl-incf heartbeat)))))
          (e-graphical-test-wait-until
           (lambda () (>= heartbeat 3))
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
      (dolist (timer heartbeat-timers)
        (when (timerp timer) (cancel-timer timer)))
      (e-runtime-store-recovery-graphical--release-stall
       stall-directory 'open)
      ;; The failed v5 open fences its worker but intentionally leaves the
      ;; transport restartable for a later explicit operation.  This private
      ;; fixture has no later operation: retire it before default cleanup so
      ;; `e-default-runtime-close' cannot submit and synchronously await a
      ;; second close/open cycle during ERT unwind.
      (ignore-errors
        (e-runtime-store-recovery-graphical--finalize-private-transport
         transport))
      (when (e-runtime-store-p e-default--runtime-store)
        (ignore-errors (e-default-runtime-close))))))

(ert-deftest e-runtime-store-recovery-graphical-s92-org-canvas-turn-survives-delayed-persistence ()
  "A new public Org Canvas Daily stays usable while SQLite admission waits."
  ;; Startup transport prewarming has its own pre-init fixture and selector.
  ;; This scenario owns a disposable runtime so its delay/failure controls can
  ;; never touch the process default or a user's canonical database.
  (e-board-e2e-reset-runtime)
  (let* ((directory (make-temp-file "e-runtime-store-org-canvas-" t))
         (canvas-directory (make-temp-file "e-org-canvas-daily-" t))
         (stall-directory (make-temp-file "e-runtime-store-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         sessions reader runtime stream harness target input backing-chat session-id
         failure-target failure-input failure-chat failure-id
         unrelated-target unrelated-input unrelated-chat unrelated-id
         heartbeat-timers (heartbeats [0 0 0])
         first-response second-response
         terminal-hook-metadata
         curation-package-bytes large-result-source-label
         (e-org-canvas-input-auto-close-delay nil)
         ;; The regression needs an ordinary tool result larger than the
         ;; query-row scalar ABI.  The tool's own output policy is independent
         ;; of the session context query's aggregate byte budget.
         (e-emacs-tools-run-elisp-string-max-bytes 12000)
         (large-curation-summary
          (concat (make-string 9000 ?s) "-daily-large-curation-summary"))
         synchronous-operation synchronous-backtrace synchronous-read-backtrace
         original-session-get original-ensure-loaded
         service-events service-subscription)
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
          ;; Match the complete public default harness capability graph.  A
          ;; lone Org Canvas capability misses chat-session and the configured
          ;; layer hooks that execute in the user's real turn lifecycle.
          ;; Only the network adapter is replaced by the controllable stream.
          (e-default-chat-sync-harness-layers
           harness nil canvas-directory)
          ;; The live regression occurred in a configured turn-finished hook:
          ;; its hook-audit append succeeded, then the return path reread the
          ;; complete durable activity aggregate and prevented turn-finished
          ;; publication.  Keep this explicit even when the disposable default
          ;; layer set does not activate the user's Bayesian capability.
          (e-harness-activate-capability
           harness
           (e-capability-create
            :id 'e2e-terminal-audit
            :hooks
            (list
             (e-hook-create
              :id "99-e2e-terminal-audit"
              :point :turn-finished
              :description "Record one terminal audit without a history read."
              :handler
              (lambda (value context)
                (setq terminal-hook-metadata
                      (copy-tree (plist-get context :session-metadata) t))
                (e-harness-record-hook-audit
                 (plist-get context :harness)
                 (plist-get context :session-id)
                 (plist-get context :turn-id)
                 :owner 'e2e-terminal-audit
                 :hook-id "99-e2e-terminal-audit"
                 :outcome 'checked
                 :summary "Terminal audit recorded")
                value)))))
          (setq original-session-get (symbol-function 'e-session-local-state))
          (setq original-ensure-loaded
                (symbol-function 'e-session--ensure-local-state))
          (fset 'e-session--ensure-local-state
                (lambda (&rest arguments)
                  (when (< (length synchronous-read-backtrace) 12)
                    (push (let ((print-level 4) (print-length 50))
                            (format "ensure-loaded: %S\narguments=%S"
                                    (seq-take (backtrace-frames) 24)
                                    arguments))
                          synchronous-read-backtrace))
                  (error "Forbidden interactive e-session--ensure-local-state %S"
                         arguments)))
          (fset 'e-session-local-state
                (lambda (&rest arguments)
                  (when (< (length synchronous-read-backtrace) 12)
                    (push (let ((print-level 4) (print-length 50))
                            (format "%S\narguments=%S"
                                    (seq-take (backtrace-frames) 24)
                                    arguments))
                          synchronous-read-backtrace))
                  (error "Forbidden interactive e-session-local-state %S" arguments)))
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
          (setq heartbeat-timers
                (mapcar
                 (lambda (index)
                   (run-at-time
                    (+ 0.01 (* index 0.005)) 0.01
                    (lambda ()
                      (aset heartbeats index
                            (1+ (aref heartbeats index))))))
                 '(0 1 2)))
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
                                     request)
                                     synchronous-backtrace
                                     (let ((print-level 3)
                                           (print-length 30))
                                       (format "%S"
                                               (seq-take
                                                (seq-drop
                                                 (backtrace-frames) 6)
                                                18))))
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
                 (lambda (event) (push (copy-tree event t) service-events))))
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-runtime-store-recovery-graphical--stall-ready-p
              stall-directory 'board-create))
           2.0 "Org Canvas new-session admission")
          ;; Foreground and headless Daily work are presentation modes on the
          ;; same canonical owner.  Admission must not manufacture a second
          ;; session merely because persistence is delayed.
          (should (= (hash-table-count
                      (e-chat-service--harness-bindings harness))
                     1))
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
          (should (seq-every-p (lambda (count) (> count 0)) heartbeats))
          (when synchronous-operation
            (ert-fail
             (format "Org Canvas interactive path awaited %S\n%s"
                     synchronous-operation synchronous-backtrace)))
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
          ;; Hold the public Board record so its same-Board classification timer
          ;; must admit routing while a SQLite request is live without freezing
          ;; the Board.  Transcript delay is armed after the provider starts,
          ;; matching the real causal order: the input transcript settles before
          ;; Board publication, while the response and next turn may overlap.
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'board-record-put)
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
             (e-runtime-store-recovery-graphical--stall-ready-p
              stall-directory 'board-record-put))
           3.0 "held public Board record")
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--runtime-operation-p
              runtime 'board-routing-put))
           3.0 "same-Board classification admitted during held Board record")
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'board-record-put)
          (let ((deadline (+ (float-time) 1.0)))
            (while (and (< (float-time) deadline)
                        (not (e-graphical-test-stream-active-p stream)))
              (sit-for 0.01))
            (unless (e-graphical-test-stream-active-p stream)
              (let* ((entry (gethash session-id
                                     (e-harness-active-turns harness)))
                     (context-work (plist-get entry :context-work)))
                (ert-fail
                 (format
                  (concat
                   "Org Canvas provider did not start: turn=%S context=%S "
                   "events=%S runtime=%S\nforbidden-session-get=%s")
                  (list :status (plist-get entry :status)
                        :condition (plist-get entry :condition)
                        :error (plist-get entry :error)
                        :has-context (and (plist-get entry :context) t))
                  (and context-work (e-work-status context-work))
                  service-events (e-runtime-store-status runtime)
                  synchronous-read-backtrace)))))
          (let* ((requests (e-graphical-test-stream-requests stream))
                 (wire (prin1-to-string
                        (plist-get (car requests) :messages))))
            (should (= (length requests) 1))
            (when (equal wire "nil")
              (let ((entry (gethash session-id
                                    (e-harness-active-turns harness))))
                (ert-fail
                 (format "Empty provider request: request=%S context=%S query-state=%S"
                         (car requests) (plist-get entry :context)
                         (plist-get entry :session-query-state)))))
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
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'session-command)
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
            ;; Exercise the real provider -> model-facing tool -> provider
            ;; continuation lifecycle.  The production failure reported by a
            ;; Daily user occurred here, after the tool result was appended:
            ;; the lifetime callback attempted a synchronous session aggregate
            ;; read even though the initial context was detached from SQLite.
            (e-graphical-test-stream-emit
             stream '(:type tool-call
                       :id "daily-run-elisp"
                       :name "run_elisp"
                       :arguments (:stated_purpose "Verify a harmless value."
                                   :code "(concat (make-string 9000 ?x) \"-daily-large-tool-result\")"))
             0.02)
            (e-graphical-test-stream-finish stream 0.03 'tool-use)
            (e-graphical-test-wait-until
             (lambda ()
               (or synchronous-operation
                   (e-graphical-test-stream-failure stream)
                   (and (= (length (e-graphical-test-stream-requests stream)) 2)
                        (e-graphical-test-stream-active-p stream))))
             2.0 "Org Canvas provider tool continuation after delayed admission"))
          (when synchronous-operation
            (ert-fail (format "Org Canvas callback awaited %S"
                              synchronous-operation)))
          (should-not (e-graphical-test-stream-failure stream))
          (let ((wire
                 (prin1-to-string
                  (plist-get
                   (cadr (e-graphical-test-stream-requests stream))
                   :messages))))
            (should (string-match-p "daily-run-elisp" wire))
            (let ((result-position
                   (string-match "daily-large-tool-result" wire))
                  (search-position 0))
              (should result-position)
              ;; Tool-observation markers are inserted immediately before
              ;; their matching results.  Select the last marker preceding
              ;; this result instead of assuming it is the frame's first
              ;; source; the complete default harness contributes earlier
              ;; current-state and dynamic-context sources.
              (while (and
                      (string-match
                       "\\[ephemeral context source \\([0-9]+\\),"
                       wire search-position)
                      (< (match-beginning 0) result-position))
                (setq large-result-source-label
                      (string-to-number (match-string 1 wire))
                      search-position (match-end 0)))
              (should (integerp large-result-source-label))))
          ;; Follow the large tool result through the reserved provider
          ;; curation action used by production models.  The previous gate
          ;; stopped one request too early, so it proved that the result could
          ;; reach the provider while missing the content-bearing durable
          ;; curation record that failed real Daily turns.
          (let ((prepare
                 (symbol-function
                  'e-context-lifetime-prepare-curation-disposition)))
            (cl-letf
                (((symbol-function
                   'e-context-lifetime-prepare-curation-disposition)
                  (lambda (&rest arguments)
                    (let ((prepared (apply prepare arguments)))
                      (setq curation-package-bytes
                            (e-context-lifetime--bytes
                             (plist-get prepared :package)))
                      prepared))))
              (e-graphical-test-stream-emit
               stream
               (e-openai-decoder--context-curation-effect
                (list :keep nil
                      :summaries
                      (list (list :sources (list large-result-source-label)
                                  :text large-curation-summary))
                      :erase nil)
                "daily-large-result-curation")
               0.01)
              (e-graphical-test-stream-finish stream 0.02)
              (condition-case _curation-timeout
                  (e-graphical-test-wait-until
                   (lambda ()
                     (or (e-graphical-test-stream-failure stream)
                         (and
                          (= (length
                              (e-graphical-test-stream-requests stream))
                             3)
                          (e-graphical-test-stream-active-p stream))))
                   2.0 "Org Canvas large-result curation continuation")
                (ert-test-failed
                 (let ((entry (gethash session-id
                                       (e-harness-active-turns harness))))
                   (ert-fail
                    (format
                     (concat "Large-result curation did not continue: "
                             "stream=%S requests=%d turn=%S transcript=%S")
                     (e-graphical-test-stream-failure stream)
                     (length (e-graphical-test-stream-requests stream))
                     (and entry
                          (list :status (plist-get entry :status)
                                :condition (plist-get entry :condition)
                                :error (plist-get entry :error)))
                     (with-current-buffer input (buffer-string)))))))))
          (should (> curation-package-bytes 8192))
          (should (<= curation-package-bytes
                      e-context-lifetime-curation-max-record-bytes))
          (should-not (e-graphical-test-stream-failure stream))
          (should
           (string-match-p
            "daily-large-curation-summary"
            (prin1-to-string
             (plist-get
              (nth 2 (e-graphical-test-stream-requests stream))
              :messages))))
          (e-graphical-test-stream-emit
           stream (list :type 'assistant-message :content first-response)
           0.01)
          (e-graphical-test-stream-finish stream 0.02)
          (e-graphical-test-wait-until
           (lambda ()
             (or (e-graphical-test-stream-failure stream)
                 (not (e-graphical-test-stream-active-p stream))))
           2.0 "Org Canvas post-tool provider completion")
          (should-not (e-graphical-test-stream-failure stream))
          (with-current-buffer input
            (when (string-match-p "Turn failed" (buffer-string))
              (ert-fail
               (format "Turn failed after synchronous session read: %s\n%s"
                       (buffer-string) synchronous-read-backtrace))))
          (e-graphical-test-wait-until
           (lambda ()
             (with-current-buffer input
               (string-match-p (regexp-quote first-response) (buffer-string))))
           2.0 "first data-dependent Org Canvas response")
          (e-graphical-test-wait-until
           (lambda ()
             (let ((terminal-events
                    (seq-filter
                     (lambda (event)
                       (let ((message
                              (plist-get (plist-get event :payload) :message)))
                         (and (eq (plist-get event :type) 'message-added)
                              (equal (plist-get event :session-id) session-id)
                              (eq (plist-get message :role) 'assistant)
                              (plist-get message :terminal-output))))
                     service-events)))
               (and (not (e-chat-service-active-turn-p harness session-id))
                    (with-current-buffer backing-chat
                      (equal (e-chat-surface-status) "done"))
                    (= (length terminal-events) 1))))
           2.0 "first Org Canvas turn reaches public terminal state")
          (let ((expected-canvas-uri
                 (concat "file://"
                         (expand-file-name (buffer-file-name target)))))
            (unless
                (equal
                 (plist-get
                  (plist-get terminal-hook-metadata :org-canvas-ref) :uri)
                 expected-canvas-uri)
              (ert-fail
               (format "Terminal hook metadata lacks Canvas URI: metadata=%S expected=%S"
                       terminal-hook-metadata expected-canvas-uri))))
          ;; The next run is queued headlessly on the same owner session.  This
          ;; is the architectural distinction Grimoire needs: no window change
          ;; and no shadow coordinator, while output remains visible here.
          (with-timeout
              (1.0 (error "Headless Daily owner queue blocked"))
            (cl-letf (((symbol-function 'e-runtime-store-await)
                       (lambda (_store request &optional _timeout)
                         (setq synchronous-operation
                               (e-runtime-store-request--operation request))
                         (error "Headless Daily owner queue awaited %S"
                                synchronous-operation))))
              (e-chat-service-queue-session
               harness session-id
               (concat "Using the alert from our previous turn, which project "
                       "needs a human review?")
               :source-input-key '("daily-headless" "same-owner" 0))))
          (should (= (hash-table-count
                      (e-chat-service--harness-bindings harness))
                     1))
          (condition-case _second-request-timeout
              (e-graphical-test-wait-until
               (lambda ()
                 (or (e-graphical-test-stream-failure stream)
                     (e-graphical-test-stream-active-p stream)))
               3.0 "second Org Canvas provider request while SQLite delayed")
            (ert-test-failed
             (let ((entry (gethash session-id
                                   (e-harness-active-turns harness))))
               (ert-fail
                (format
                 (concat "Second Org Canvas request did not start: "
                         "stream=%S turn=%S pending=%d transcript=%S")
                 (e-graphical-test-stream-failure stream)
                 (and entry
                      (list :status (plist-get entry :status)
                            :condition (plist-get entry :condition)
                            :error (plist-get entry :error)
                            :timer (and (timerp (plist-get entry :timer)) t)
                            :context-work
                            (and-let* ((work (plist-get entry :context-work)))
                              (e-work-status work))))
                 (e-session-async-pending-count sessions session-id)
                 (with-current-buffer input (buffer-string)))))))
          (should-not (e-graphical-test-stream-failure stream))
          (let* ((requests (e-graphical-test-stream-requests stream))
                 (wire (prin1-to-string
                        (plist-get (nth 3 requests) :messages))))
            (should (= (length requests) 4))
            (should (string-match-p (regexp-quote first-response) wire))
            (should (string-match-p "daily-large-curation-summary" wire))
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
          (should (seq-every-p (lambda (count) (> count 3)) heartbeats))
          (should (e-session-async-pending-p sessions session-id))
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'session-command)
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
          ;; This is an explicit test observation boundary.  Read only the
          ;; consumer-shaped visible window from v6; never reconstruct the
          ;; retired session aggregate merely to prove durable publication.
          (let ((visible
                 (cl-letf
                     (((symbol-function 'e-runtime-store-await)
                       #'e-runtime-store-recovery-graphical--await-with-pump))
                   (e-runtime-store-call
                    runtime 'read
                    (list :op 'session-visible-message-page
                          :session-id session-id :limit 16)))))
            (should
             (cl-find-if
              (lambda (message)
                (equal (plist-get message :content) second-response))
              (plist-get visible :messages))))
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
                  (e-session-sqlite-store-create directory)))
          (let ((query-state
                 (cl-letf
                     (((symbol-function 'e-runtime-store-await)
                       #'e-runtime-store-recovery-graphical--await-with-pump))
                   (e-runtime-store-call
                    (e-session-storage-runtime-store reader) 'read
                    (list :op 'session-query-state
                          :session-id unrelated-id)))))
            (should (equal (plist-get query-state :session-id) unrelated-id))
            (should
             (plist-get (plist-get query-state :metadata) :org-canvas-ref))))
      (dolist (timer heartbeat-timers)
        (when (timerp timer) (cancel-timer timer)))
      (when (e-graphical-test-stream-p stream)
        (e-graphical-test-stream-cancel stream))
      (when service-subscription
        (e-chat-service-unsubscribe service-subscription))
      (when original-session-get
        (fset 'e-session-local-state original-session-get))
      (when original-ensure-loaded
        (fset 'e-session--ensure-local-state original-ensure-loaded))
      (ignore-errors
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'board-create)
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'board-record-put)
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'board-routing-put)
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'session-command)
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

(ert-deftest e-runtime-store-recovery-graphical-s92-killed-open-retires-late-binding ()
  "A chat killed before SQL readiness leaves no process-local binding."
  (e-board-e2e-reset-runtime)
  (let* ((directory (make-temp-file "e-runtime-store-killed-open-" t))
         (stall-directory (make-temp-file "e-runtime-store-killed-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         sessions runtime harness transcript readiness session-id)
    (unwind-protect
        (progn
          (setq sessions
                (cl-letf (((symbol-function 'e-runtime-store-await)
                           #'e-runtime-store-recovery-graphical--await-with-pump))
                  (e-session-sqlite-store-create directory)))
          (e-session-enable sessions)
          (setq runtime (e-session-storage-runtime-store sessions)
                harness
                (e-harness-create
                 :backend (e-backend-fake-create :items nil)
                 :sessions sessions))
          (e-runtime-store-recovery-graphical--prepare-frame)
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'chat-session-owner-admit)
          (setq transcript
                (with-timeout (1.0 (ert-fail "Delayed chat open blocked"))
                  (e-chat-open :harness harness
                               :session-id "killed-before-readiness"
                               :new-session t)))
          (e-chat-surface-pop-to-buffer transcript)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--surface-windows transcript))
           1.0 "delayed chat surface")
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-runtime-store-recovery-graphical--stall-ready-p
              stall-directory 'chat-session-owner-admit))
           5.0 "delayed owner admission")
          (setq session-id (buffer-local-value 'e-chat-session-id transcript)
                readiness
                (buffer-local-value 'e-chat--session-readiness-work transcript))
          (should (e-work-handle-p readiness))
          (should-not
           (buffer-local-value 'e-chat--event-subscription transcript))
          (kill-buffer transcript)
          (setq transcript nil)
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'chat-session-owner-admit)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (and (eq (plist-get (e-work-status readiness) :state) 'finished)
                  (null (gethash harness e-chat-service--bindings))
                  (= (hash-table-count e-chat-service--board-bindings) 0)))
           5.0 "late binding retirement")
          ;; Presentation abandonment never cancels or rolls back the durable
          ;; owner admission that SQLite has already accepted.
          (let ((state
                 (cl-letf (((symbol-function 'e-runtime-store-await)
                            #'e-runtime-store-recovery-graphical--await-with-pump))
                   (e-runtime-store-call
                    runtime 'read
                    (list :op 'session-query-state :session-id session-id)))))
            (should (equal (plist-get state :session-id) session-id))))
      (ignore-errors
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'chat-session-owner-admit))
      (when (buffer-live-p transcript) (kill-buffer transcript))
      (when runtime
        (ignore-errors (e-runtime-store--finalize-close runtime)))
      (when (file-directory-p directory) (delete-directory directory t))
      (when (file-directory-p stall-directory)
        (delete-directory stall-directory t)))))

(ert-deftest e-runtime-store-recovery-graphical-s92-public-new-chat-survives-delayed-worker ()
  "Public new-chat UI remains usable and ordered while SQLite is delayed."
  (e-board-e2e-reset-runtime)
  (let* ((directory (make-temp-file "e-runtime-store-new-chat-" t))
         (stall-directory (make-temp-file "e-runtime-store-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         sessions reader runtime stream harness transcript failure-transcript
         unrelated-transcript heartbeat-timer (heartbeat 0) (phase 'setup)
         synchronous-operation detached-view-call)
    (unwind-protect
        (ert-info ((format "DP6B phase: %s" phase))
          (let (synchronous-session-get)
            (cl-letf (((symbol-function 'e-session-local-state)
                       (lambda (&rest arguments)
                         (setq synchronous-session-get
                               (let ((print-level 4) (print-length 40))
                                 (list arguments
                                       (seq-take (backtrace-frames) 24))))
                         (error "Public new-chat path called e-session-local-state %S"
                                arguments))))
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
          (setq phase 'open-new-chat)
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'chat-session-owner-admit)
          (let ((started (float-time))
                (original-chat-view
                 (symbol-function 'e-session-async-chat-view)))
            (setq heartbeat-timer
                  (run-at-time 0.01 0.01 (lambda () (cl-incf heartbeat))))
            (setq transcript
                  (cl-letf (((symbol-function 'e-runtime-store-await)
                             (lambda (&rest arguments)
                               (signal 'error
                                       (list "New-chat path called synchronous runtime await"
                                             arguments))))
                            ((symbol-function 'e-chat--select-chat-instance)
                             (lambda (&optional _prompt) nil))
                            ((symbol-function 'e-chat--default-harness)
                             (lambda () harness))
                            ((symbol-function 'e-session-async-chat-view)
                             (lambda (&rest arguments)
                               (setq detached-view-call arguments)
                               (apply original-chat-view arguments))))
                    ;; Exercise the user-facing command, including new-session
                    ;; creation and the composer surface it returns.
                    (e-chat-new)))
            (setq phase 'new-chat-opened)
            (should (< (- (float-time) started) 0.1))
            (should (buffer-live-p transcript))
            (should (string-match-p
                     "pending"
                     (or (e-chat-surface-status transcript) "")))
            (with-current-buffer transcript
              ;; Creation already establishes the exact empty visible window.
              ;; Starting a detached read here would race the independent read
              ;; connection against the queued session-create transaction.
              (should-not e-chat--session-query-work)
              (should-not (string-match-p "Loading recent messages"
                                          (buffer-string)))
              (should-not (string-match-p "Unable to load recent messages"
                                          (buffer-string))))
            (should-not detached-view-call)
            ;; `e-chat-open' deliberately returns an undisplayed buffer.  Put
            ;; that returned public surface on the isolated frame before the
            ;; graphical composer assertion below.
            (e-chat-surface-pop-to-buffer transcript)
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--surface-windows transcript))
             1.0 "new-chat transcript and composer windows")
            (condition-case _wait-error
                (e-graphical-test-wait-until
                 (lambda ()
                   (e-runtime-store-recovery-graphical--pump-runtime runtime)
                   (e-runtime-store-recovery-graphical--stall-ready-p
                    stall-directory 'chat-session-owner-admit))
                 5.0 "new-chat persistence admission")
              (ert-test-failed
               (ert-fail
                (format
                 (concat "Timed out waiting for new-chat persistence admission: "
                         "synchronous=%S runtime=%S status=%S readiness=%S")
                 synchronous-session-get
                 (e-runtime-store-recovery-graphical--runtime-operations runtime)
                 (e-chat-surface-status transcript)
                 (e-work-status
                  (buffer-local-value 'e-chat--session-readiness-work
                                      transcript))))))
            (let* ((session-id
                    (buffer-local-value 'e-chat-session-id transcript))
                   (composer (e-chat-surface-composer-buffer transcript))
                   (draft "draft survives persistence stall")
                   (message "public composer submit survives persistence stall"))
              (should (buffer-live-p composer))
              (let ((surface-windows
                     (e-runtime-store-recovery-graphical--surface-windows
                      transcript)))
                (should surface-windows)
                (should (eq (window-buffer (car surface-windows)) transcript))
                (should (eq (window-buffer (cdr surface-windows)) composer))
                ;; A real command in the transcript runs global
                ;; `post-command-hook'.  This is the exact focus shape that
                ;; previously escaped composer-only coverage and reached the
                ;; removed aggregate reader from e-debug focus tracking.
                (should (memq #'e-debug--record-focused-buffer
                              (default-value 'post-command-hook)))
                (select-window (car surface-windows))
                (execute-kbd-macro (kbd "C-e"))
                (should (eq e-debug--last-focused-buffer transcript))
                (select-window (cdr surface-windows))
                ;; Exercise the actual interactive composer path.  Calling
                ;; `e-chat-submit-session' directly does not run submit-intent
                ;; selection and therefore cannot detect a stale synchronous
                ;; session-aggregate lookup in the public command.
                (e-graphical-test-type-text message)
                (call-interactively #'e-chat-submit)
                (e-graphical-test-type-text draft))
              (should
               (e-runtime-store-recovery-graphical--runtime-operation-p
                runtime 'chat-session-owner-admit))
              (should
               (e-runtime-store-recovery-graphical--runtime-operation-p
                runtime 'chat-session-input-admit))
              (setq phase 'sql-input-enqueued)
              (e-graphical-test-wait-until (lambda () (> heartbeat 3))
                                           1.0 "independent heartbeat")
              (setq phase 'heartbeat-live)
              (should (= (e-runtime-store-recovery-graphical--count-string
                          message transcript)
                         0))
              (cl-letf (((symbol-function 'e-runtime-store-await)
                         (lambda (_store request &optional _timeout)
                           (setq synchronous-operation
                                 (e-runtime-store--request-operation request))
                           (signal 'error
                                   (list "New-chat release called synchronous await"
                                         synchronous-operation)))))
                (e-runtime-store-recovery-graphical--release-stall
                 stall-directory 'chat-session-owner-admit)
                (setq phase 'new-chat-released)
                (e-graphical-test-wait-until
                 (lambda ()
                   (e-runtime-store-recovery-graphical--pump-runtime runtime)
                   (eq (plist-get
                        (e-work-status
                         (buffer-local-value 'e-chat--session-readiness-work
                                             transcript))
                        :state)
                       'finished))
                 2.0 "new-chat session readiness"))
              (when synchronous-operation
                (ert-fail (format "New-chat release awaited %S"
                                  synchronous-operation)))
              (setq phase 'new-chat-ready)
              (with-current-buffer transcript
                (should-not e-chat--session-query-work)
                (should-not (string-match-p "Unable to load recent messages"
                                            (buffer-string)))
                (should-not (string-match-p "Loading recent messages"
                                            (buffer-string))))
              (should-not detached-view-call)
              (condition-case _render-timeout
                  (e-graphical-test-wait-until
                   (lambda ()
                     (e-runtime-store-recovery-graphical--pump-runtime runtime)
                     (= (e-runtime-store-recovery-graphical--count-string
                         message transcript)
                        1))
                   3.0 "classified message render")
                (ert-test-failed
                 (ert-fail
                  (let ((durable
                         (cl-letf
                             (((symbol-function 'e-runtime-store-await)
                               #'e-runtime-store-recovery-graphical--await-with-pump))
                           (list
                            :state
                            (e-runtime-store-call
                             runtime 'read
                             (list :op 'session-query-state
                                   :session-id session-id))
                            :records
                            (e-runtime-store-call
                             runtime 'read
                             (list :op 'session-record-page
                                   :session-id session-id :after 0
                                   :limit 8))))))
                    (format
                     (concat "Timed out waiting for classified message render: "
                             "transcript=%S suspect=%S runtime=%S "
                             "synchronous=%S durable=%S")
                     (with-current-buffer transcript (buffer-string))
                     (e-session-persistence-suspect sessions session-id)
                     (e-runtime-store-recovery-graphical--runtime-operations
                      runtime)
                     synchronous-session-get
                     durable)))))
              (setq phase 'message-rendered)
              (should-not synchronous-session-get)
              (should (= (e-runtime-store-recovery-graphical--count-string
                          message transcript)
                         1))
              (with-current-buffer composer
                (should (string-match-p (regexp-quote draft) (buffer-string)))))
            (when (timerp heartbeat-timer)
              (cancel-timer heartbeat-timer)
              (setq heartbeat-timer nil))

            ;; Exercise the owner-local failure branch on a fresh public chat.
            ;; Hold the atomic SQL owner admission, then kill only the isolated
            ;; worker process.
            (let ((failure-id "new-chat-failure-owner"))
              (setq phase 'failure-open)
              (e-runtime-store-recovery-graphical--arm-stall
               stall-directory 'chat-session-owner-admit)
              (setq failure-transcript
                    (e-chat-open :harness harness :session-id failure-id
                                 :new-session t))
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (e-runtime-store-recovery-graphical--stall-ready-p
                  stall-directory 'chat-session-owner-admit))
               2.0 "new-chat failure session mutation")
              (setq phase 'failure-stalled)
              (let ((failed-process (e-runtime-store--process runtime)))
                (delete-process failed-process)
                (setq phase 'failure-worker-recovery)
                ;; The transport may either partition immediately or replay
                ;; once under the same request identity.  If replay begins,
                ;; lose that isolated replacement too; the product contract is
                ;; the resulting owner-local warning, not a particular retry
                ;; timing window.
                (condition-case _wait-error
                    (e-graphical-test-wait-until
                     (lambda ()
                       (e-runtime-store-recovery-graphical--pump-runtime runtime)
                       (or (string-match-p
                            (regexp-quote failure-id)
                            (or (e-chat-surface-status failure-transcript) ""))
                           (let ((replacement
                                  (e-runtime-store--process runtime)))
                             (and replacement
                                  (not (eq replacement failed-process))
                                  (process-live-p replacement)
                                  (or (e-runtime-store--recovering-request runtime)
                                      (e-runtime-store-recovery-graphical--runtime-operation-p
                                       runtime
                                       'chat-session-owner-admit))))))
                     2.0 "new-chat owner failure or same-ID recovery")
                  (ert-test-failed
                   (let* ((process (e-runtime-store--process runtime))
                          (active (e-runtime-store--active-request runtime))
                          (recovering
                           (e-runtime-store--recovering-request runtime))
                          (readiness
                           (e-work-status
                            (buffer-local-value
                             'e-chat--session-readiness-work
                             failure-transcript))))
                     (ert-fail
                      (format
                       (concat "Owner failure made no progress: same-process=%S "
                               "process-status=%S active=%S recovering=%S "
                               "queue=%S readiness=%S surface=%S")
                       (eq process failed-process)
                       (and process (process-status process))
                       (and active
                            (list (e-runtime-store-request--kind active)
                                  (e-runtime-store-request--operation active)
                                  (e-runtime-store-request--state active)))
                       (and recovering
                            (list (e-runtime-store-request--kind recovering)
                                  (e-runtime-store-request--operation recovering)
                                  (e-runtime-store-request--state recovering)))
                       (mapcar #'e-runtime-store-request--operation
                               (e-runtime-store--client-queue runtime))
                       (plist-get readiness :state)
                       (e-chat-surface-status failure-transcript))))))
                (unless (string-match-p
                         (regexp-quote failure-id)
                         (or (e-chat-surface-status failure-transcript) ""))
                  (delete-process (e-runtime-store--process runtime))))
              (setq phase 'failure-worker-killed)
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (string-match-p
                  (regexp-quote failure-id)
                  (or (e-chat-surface-status failure-transcript) "")))
               2.0 "new-chat owner suspect warning")
              (setq phase 'failure-suspect-visible)
              (let ((warning (e-chat-surface-status failure-transcript)))
                (should (string-match-p "suspect" warning))
                (should (<= (string-bytes warning) 1152))
                (with-temp-buffer
                  (insert warning)
                  (goto-char (point-min))
                  (should (= (how-many (regexp-quote failure-id)) 1)))
                (sit-for 0.05)
                (should (equal (e-chat-surface-status failure-transcript)
                               warning)))
              ;; The failed owner is now terminally partitioned.  Release the
              ;; operation-level test stall before proving unrelated recovery.
              (e-runtime-store-recovery-graphical--release-stall
               stall-directory 'chat-session-owner-admit)
              (setq unrelated-transcript
                    (e-chat-open :harness harness
                                 :session-id "new-chat-unrelated-owner"
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
                      (e-session-sqlite-store-create directory)))
              (let ((query-state
                     (cl-letf
                         (((symbol-function 'e-runtime-store-await)
                           #'e-runtime-store-recovery-graphical--await-with-pump))
                       (e-runtime-store-call
                        (e-session-storage-runtime-store reader) 'read
                        (list :op 'session-query-state
                              :session-id "new-chat-unrelated-owner")))))
                (should
                 (equal (plist-get query-state :session-id)
                        "new-chat-unrelated-owner"))))
            (setq phase 'readback-complete)
            (should-not synchronous-session-get))))))
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
         stall-directory 'chat-session-owner-admit))
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

(ert-deftest e-runtime-store-recovery-graphical-s92-public-v5-upgrade-opens-chat ()
  "A real v5 refusal, offline upgrade, and fresh public chat work end to end."
  (e-board-e2e-reset-runtime)
  (let* ((directory (make-temp-file "e-runtime-store-v5-public-" t))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory))
         old-runtime sessions runtime stream harness transcript)
    (unwind-protect
        (progn
          (e-runtime-store-recovery-graphical--make-upgrade-v5-fixture directory)
          ;; Reproduce the user's first post-restart boundary.  A schema
          ;; refusal is global readiness state, never owner-local damage.
          (setq old-runtime (e-runtime-store-open directory))
          (let ((open-request
                 (e-runtime-store--open-control-request old-runtime)))
            (should open-request)
            (should-error
             (e-runtime-store-recovery-graphical--await-with-pump
              old-runtime open-request)
             :type 'e-runtime-store-schema-too-old))
          (should (e-runtime-store--unavailable old-runtime))
          (should-not (e-runtime-store--suspect-owners old-runtime))
          (e-runtime-store-recovery-graphical--finalize-private-transport
           old-runtime)
          (setq old-runtime nil)

          ;; This is the named blocking operator/test boundary.  It launches
          ;; the same private batch worker as scripts/e-runtime-upgrade.
          (let ((result
                 (e-runtime-store-offline-upgrade directory backup)))
            (should (= (plist-get result :from) 5))
            (should (= (plist-get result :to) 6))
            (should (equal (plist-get result :integrity) "ok")))

          ;; A fresh runtime after the upgrade is the restart boundary the old
          ;; split tests failed to cover.
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
          (let (synchronous-session-get)
            (cl-letf (((symbol-function 'e-session-local-state)
                       (lambda (&rest arguments)
                         (setq synchronous-session-get arguments)
                         (error "Upgraded public chat called e-session-local-state")))
                      ((symbol-function 'e-runtime-store-await)
                       (lambda (&rest arguments)
                         (error "Upgraded public chat awaited SQLite %S"
                                arguments)))
                      ((symbol-function 'e-chat--select-chat-instance)
                       (lambda (&optional _prompt) nil))
                      ((symbol-function 'e-chat--default-harness)
                       (lambda () harness)))
              (let ((started (float-time)))
                (setq transcript (e-chat-new))
                (should (< (- (float-time) started) 0.1)))
              (should (buffer-live-p transcript))
              (e-chat-surface-pop-to-buffer transcript)
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (and
                  (e-runtime-store-recovery-graphical--surface-windows
                   transcript)
                  (null (buffer-local-value 'e-chat--session-query-work
                                            transcript))
                  (let ((work
                         (buffer-local-value 'e-chat--session-readiness-work
                                             transcript)))
                    (and work
                         (eq (plist-get (e-work-status work) :state)
                             'finished)))))
               3.0 "public chat after v5 upgrade")
              (should-not synchronous-session-get)))
          (with-current-buffer transcript
            (should-not (string-match-p "Unable to load recent messages"
                                        (buffer-string)))
            (should-not (string-match-p "upgrade required"
                                        (or (e-chat-surface-status transcript)
                                            ""))))
          (let ((composer (e-chat-surface-composer-buffer transcript)))
            (should (buffer-live-p composer))
            (with-current-buffer composer
              (insert "composer works after upgrade")
              (should (string-match-p "composer works after upgrade"
                                      (buffer-string)))))
          (e-graphical-test-capture-state "public-v5-upgrade-chat-ready")
          (cl-letf (((symbol-function 'e-runtime-store-await)
                     #'e-runtime-store-recovery-graphical--await-with-pump))
            (let* ((existing
                    (e-runtime-store-call
                     runtime 'read
                     '(:op session-query-state
                       :session-id "graphical-v5-existing")))
                   (rootless
                    (e-runtime-store-call
                     runtime 'read
                     '(:op session-query-state
                       :session-id "graphical-v5-rootless"))))
              (should (equal (plist-get existing :name) "After replacement"))
              (should (= (plist-get existing :journal-position) 4))
              (should-not rootless))))
      (when (buffer-live-p transcript) (kill-buffer transcript))
      (when (e-graphical-test-stream-p stream)
        (e-graphical-test-stream-cancel stream))
      (when old-runtime
        (ignore-errors
          (e-runtime-store-recovery-graphical--finalize-private-transport
           old-runtime)))
      (when runtime
        (ignore-errors (e-runtime-store--finalize-close runtime)))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-recovery-graphical-s92-daily-query-window-is-bounded ()
  "Existing Daily opens before held v6 reads and renders one visible window."
  (e-board-e2e-reset-runtime)
  (let* ((directory (make-temp-file "e-runtime-store-daily-query-" t))
         (stall-directory (make-temp-file "e-runtime-store-daily-query-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         sessions runtime stream harness transcript heartbeat-timers
         (heartbeat 0) submitted-bodies unrelated-count)
    (unwind-protect
        (progn
          (setq unrelated-count
                (e-runtime-store-recovery-graphical--make-daily-v6-fixture
                 directory))
          (dolist (operation
                   '(session-metadata session-board-association
                     session-visible-message-page))
            (e-runtime-store-recovery-graphical--arm-stall
             stall-directory operation))
          (setq runtime (e-runtime-store-open directory))
          ;; The bounded status observation settles the transport open before
          ;; the session adapter starts its three held query reads.
          (cl-letf (((symbol-function 'e-runtime-store-await)
                     #'e-runtime-store-recovery-graphical--await-with-pump))
            (e-runtime-store-call runtime 'read '(:op status)))
          (setq sessions
                (e-session-sqlite-store-create
                 directory :runtime-store runtime))
          (e-session-enable sessions)
          (setq stream (e-graphical-test-stream-create)
                harness (e-harness-create
                         :backend (e-graphical-test-stream-backend stream)
                         :sessions sessions))
          (e-runtime-store-recovery-graphical--prepare-frame)
          (setq heartbeat-timers
                (list
                 (run-at-time 0.01 nil (lambda () (cl-incf heartbeat)))
                 (run-at-time 0.02 nil (lambda () (cl-incf heartbeat)))
                 (run-at-time 0.03 nil (lambda () (cl-incf heartbeat)))))
          (cl-letf (((symbol-function 'e-session-storage-submit)
                     (let ((submit (symbol-function 'e-session-storage-submit)))
                       (lambda (store kind body on-settle &optional escrow)
                         (push (list kind body) submitted-bodies)
                         (funcall submit store kind body on-settle escrow)))))
            ;; All three worker responses are held before this call.  The
            ;; returned buffer and composer therefore prove that attach did
            ;; not synchronously load a catalog, header, or aggregate.
            (setq transcript
                  (e-chat-open :harness harness :session-id "daily-known"))
            (should (buffer-live-p transcript))
            (with-current-buffer transcript
              (should (e-work-handle-p e-chat--session-query-work))
              (should (= (hash-table-count (e-session-store-sessions sessions)) 0))
              (should (string-match-p "Loading recent messages"
                                      (buffer-string))))
            (e-chat-surface-pop-to-buffer transcript)
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--surface-windows transcript))
             1.0 "persistent Daily query surface")
            (let ((surface-windows
                   (e-runtime-store-recovery-graphical--surface-windows transcript)))
              (should surface-windows)
              (select-window (cdr surface-windows))
              (e-graphical-test-type-text "draft while Daily reads wait")
              (with-current-buffer (window-buffer (cdr surface-windows))
                (should (string-match-p "draft while Daily reads wait"
                                        (buffer-string)))))
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--pump-runtime runtime)
               (e-runtime-store-recovery-graphical--stall-ready-p
                stall-directory 'session-metadata))
             2.0
             (format "held metadata read (query=%S submitted=%S runtime=%S)"
                     (with-current-buffer transcript
                       (e-work-status e-chat--session-query-work))
                     (reverse submitted-bodies)
                     (e-runtime-store-recovery-graphical--runtime-operations
                      runtime)))
            (should (= unrelated-count 1277))
            (should
             (equal
              (e-runtime-store-recovery-graphical--runtime-operations runtime)
              '(session-metadata session-board-association
                session-visible-message-page)))
            (should
             (equal
              (mapcar (lambda (entry) (plist-get (cadr entry) :op))
                      (reverse submitted-bodies))
              '(session-metadata session-board-association
                session-visible-message-page)))
            (should (= (hash-table-count (e-session-store-sessions sessions)) 0))
            (e-graphical-test-wait-until
             (lambda () (>= heartbeat 3))
             1.0 "independent Daily heartbeats")
            (should
             (equal
              (e-runtime-store-recovery-graphical--runtime-operations runtime)
              '(session-metadata session-board-association
                session-visible-message-page)))
            (e-runtime-store-recovery-graphical--release-stall
             stall-directory 'session-metadata)
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--pump-runtime runtime)
               (e-runtime-store-recovery-graphical--stall-ready-p
                stall-directory 'session-board-association))
             2.0 "held Board association read")
            (e-runtime-store-recovery-graphical--release-stall
             stall-directory 'session-board-association)
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--pump-runtime runtime)
               (e-runtime-store-recovery-graphical--stall-ready-p
                stall-directory 'session-visible-message-page))
             2.0 "held visible message read")
            (e-runtime-store-recovery-graphical--release-stall
             stall-directory 'session-visible-message-page)
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--pump-runtime runtime)
               (and (buffer-live-p transcript)
                    (with-current-buffer transcript
                      (null e-chat--session-query-work))))
             3.0 "Daily query composition")
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--pump-runtime runtime)
               (and (buffer-live-p transcript)
                    (with-current-buffer transcript
                      (let ((state
                             (plist-get
                              (e-work-status e-chat--session-readiness-work)
                              :state)))
                        (memq state '(finished failed cancelled))))))
             3.0 "bounded SQL Board binding")
            (with-current-buffer transcript
              (let ((status (e-work-status e-chat--session-readiness-work)))
                (unless (eq (plist-get status :state) 'finished)
                  (ert-fail (format "Board controller failed: %S" status))))
              (should (equal e-chat-board-id "daily-board"))
              (should (= (e-runtime-store-recovery-graphical--count-string
                          "Daily user message" transcript)
                         1))
              (should (= (e-runtime-store-recovery-graphical--count-string
                          "Daily assistant message" transcript)
                         1))
              (should-not (string-match-p "Unrelated" (buffer-string)))
              (should (< (buffer-size) 4096)))
            (let* ((binding
                    (e-chat-service-binding harness "daily-known"))
                   (controller-samples
                    (seq-filter
                     (lambda (sample)
                       (eq (plist-get sample :operation)
                           'board-controller-state))
                     (plist-get
                      (plist-get
                       (plist-get (e-runtime-store-status runtime)
                                  :read-transport)
                       :latencies)
                      :recent))))
              (should binding)
              ;; SQL binding validates exact detached rows.  It must not
              ;; reconstruct the retired bounded Board controller at all.
              (should-not controller-samples))
            (should (= (hash-table-count (e-session-store-sessions sessions)) 0))
            (should-not
             (e-runtime-store-recovery-graphical--runtime-operations runtime))))
        ;; Keep the runner's one-shot timers and the disposable transport out
        ;; of subsequent graphical tests even when a held-read assertion
        ;; fails halfway through the composition.
        (dolist (timer heartbeat-timers)
          (when (timerp timer) (cancel-timer timer)))
        (dolist (operation
                 '(session-metadata session-board-association
                   session-visible-message-page))
          (ignore-errors
            (e-runtime-store-recovery-graphical--release-stall
             stall-directory operation)))
        (when (buffer-live-p transcript) (kill-buffer transcript))
        (when (e-graphical-test-stream-p stream)
          (e-graphical-test-stream-cancel stream))
        (when sessions
          (ignore-errors (e-session-sqlite-store-close sessions)))
        (when (e-runtime-store-p runtime)
          (ignore-errors (e-runtime-store--finalize-close runtime)))
        (when (file-directory-p directory)
          (delete-directory directory t))
        (when (file-directory-p stall-directory)
          (delete-directory stall-directory t)))))

(ert-deftest e-runtime-store-recovery-graphical-f92a2-dp6-public-pre-readiness-turn ()
  "A public Org Canvas turn crosses held SQLite admission without blocking UI."
  (e-board-e2e-reset-runtime)
  (let* ((directory (make-temp-file "e-f92a2-dp6-store-" t))
         (canvas-directory (make-temp-file "e-f92a2-dp6-canvas-" t))
         (stall-directory (make-temp-file "e-f92a2-dp6-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         sessions runtime stream harness target input backing-chat session-id
         existing-transcript existing-composer existing-admission failure-composer
         failure-admission
         sibling-transcript sibling-composer sibling-id
         heartbeat-timers (heartbeats [0 0 0])
         (prompt "Update this Daily through the public composer")
         (reply "Daily update committed exactly once")
         (e-org-canvas-input-auto-close-delay nil)
         synchronous-operation)
    (unwind-protect
        (progn
          (setq sessions
                (cl-letf (((symbol-function 'e-runtime-store-await)
                           #'e-runtime-store-recovery-graphical--await-with-pump))
                  (e-session-sqlite-store-create directory))
                runtime (e-session-storage-runtime-store sessions)
                stream (e-graphical-test-stream-create)
                harness
                (e-harness-create
                 :backend (e-graphical-test-stream-backend stream)
                 :sessions sessions)
                target
                (find-file-noselect
                 (expand-file-name "2026-09-09.org" canvas-directory)))
          (e-session-enable sessions)
          (e-default-chat-sync-harness-layers harness nil canvas-directory)
          (with-current-buffer target
            (org-mode)
            (insert "#+title: Daily 2026-09-09\n\n* Daily\n")
            (save-buffer))
          (e-runtime-store-recovery-graphical--prepare-frame)
          (set-window-buffer (selected-window) target)
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'chat-session-owner-admit)
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'chat-session-input-admit)
          (setq heartbeat-timers
                (mapcar
                 (lambda (index)
                   (run-at-time
                    (+ 0.01 (* index 0.005)) 0.01
                    (lambda ()
                      (aset heartbeats index
                            (1+ (aref heartbeats index))))))
                 '(0 1 2)))
          (setq input
                (with-timeout
                    (1.0 (ert-fail "Public Org Canvas open blocked"))
                  (cl-letf (((symbol-function 'e-org-canvas--default-harness)
                             (lambda () harness))
                            ((symbol-function 'e-runtime-store-await)
                             (lambda (_store request &optional _timeout)
                               (setq synchronous-operation
                                     (e-runtime-store-request--operation request))
                               (error "Interactive Org Canvas awaited %S"
                                      synchronous-operation))))
                    (with-current-buffer target
                      (e-org-canvas-prompt-document)))))
          (should (buffer-live-p input))
          (setq session-id
                (buffer-local-value 'e-org-canvas-input--session-id input)
                backing-chat
                (cl-find-if
                 (lambda (buffer)
                   (with-current-buffer buffer
                     (and (derived-mode-p 'e-chat-mode)
                          (not (derived-mode-p 'e-org-canvas-input-mode))
                          (equal e-chat-session-id session-id))))
                 (buffer-list)))
          (should (stringp session-id))
          (should (buffer-live-p backing-chat))
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-runtime-store-recovery-graphical--stall-ready-p
              stall-directory 'chat-session-owner-admit))
           3.0 "held new Daily owner admission")
          (with-current-buffer input
            (goto-char (point-max))
            (e-graphical-test-type-text prompt)
            (let ((started (float-time)))
              (cl-letf (((symbol-function 'e-runtime-store-await)
                         (lambda (_store request &optional _timeout)
                           (setq synchronous-operation
                                 (e-runtime-store-request--operation request))
                           (error "Interactive Org Canvas submit awaited %S"
                                  synchronous-operation))))
                (e-org-canvas-input-submit))
              (should (< (- (float-time) started) 0.1))))
          (condition-case _wait-error
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (and
                  (e-runtime-store-recovery-graphical--runtime-operation-p
                   runtime 'chat-session-input-admit)
                  (when-let* ((pending
                               (gethash
                                session-id
                                (e-chat-service--harness-pending-creations
                                 harness)))
                              (work
                               (e-chat-service-create-operation-first-input-work
                                pending)))
                    (memq (plist-get (e-work-status work) :state)
                          '(started running)))))
               3.0 "queued atomic first-input admission")
            (ert-test-failed
             (ert-fail
              (format
               (concat "Atomic first-input admission did not reach SQLite: "
                       "runtime=%S creation=%S binding=%S suspect=%S "
                       "input=%S stderr=%S")
               (e-runtime-store-recovery-graphical--runtime-operations runtime)
               (when-let* ((pending
                            (gethash
                             session-id
                             (e-chat-service--harness-pending-creations
                              harness))))
                 (list :work
                       (e-work-status
                        (e-chat-service-create-operation-work pending))
                       :first-input
                       (and
                        (e-chat-service-create-operation-first-input-work pending)
                        (e-work-status
                         (e-chat-service-create-operation-first-input-work
                          pending)))))
               (when-let* ((binding
                            (e-chat-service-binding harness session-id)))
                 (when-let* ((readiness
                              (e-chat-service-binding-readiness-work binding)))
                   (e-work-status readiness)))
               (e-session-persistence-suspect sessions session-id)
               (and (buffer-live-p input)
                    (with-current-buffer input (buffer-string)))
               (when-let* ((buffer (e-runtime-store--stderr-buffer runtime))
                           ((buffer-live-p buffer)))
                 (with-current-buffer buffer
                   (buffer-substring-no-properties
                    (max (point-min) (- (point-max) 3000))
                    (point-max))))))))
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'chat-session-owner-admit)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-runtime-store-recovery-graphical--stall-ready-p
              stall-directory 'chat-session-input-admit))
           3.0 "held atomic first-input admission")
          (e-graphical-test-wait-until
           (lambda () (seq-every-p (lambda (count) (> count 0)) heartbeats))
           1.0 "three independent heartbeats")
          (should-not synchronous-operation)
          (should-not (e-graphical-test-stream-active-p stream))
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'chat-session-input-admit)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-graphical-test-stream-active-p stream))
           4.0 "provider starts after canonical admission")
          (e-graphical-test-stream-emit
           stream (list :type 'assistant-message :content reply) 0.01)
          (e-graphical-test-stream-finish stream 0.02)
          (condition-case _wait-error
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (and (buffer-live-p input)
                      (= (e-runtime-store-recovery-graphical--count-string
                          reply input)
                         1)))
               4.0 "exactly one visible canonical assistant result")
            (ert-test-failed
             (ert-fail
              (format
               (concat "Canonical assistant result was not visible once: "
                       "input-live=%S input=%S transcript=%S runtime=%S "
                       "suspect=%S stream-failure=%S active-turn=%S")
               (buffer-live-p input)
               (and (buffer-live-p input)
                    (with-current-buffer input (buffer-string)))
               (and (buffer-live-p backing-chat)
                    (with-current-buffer backing-chat (buffer-string)))
               (e-runtime-store-recovery-graphical--runtime-operations runtime)
               (e-session-persistence-suspect sessions session-id)
               (e-graphical-test-stream-failure stream)
               (gethash session-id (e-harness-active-turns harness))))))
          (should (= (e-runtime-store-recovery-graphical--count-string
                      reply input)
                     1))
          (should (= (e-runtime-store-recovery-graphical--count-string
                      prompt backing-chat)
                     1))
          (dolist (surface (list input backing-chat))
            (with-current-buffer surface
              (should-not (string-match-p "Event:" (buffer-string)))
              (should-not
               (string-match-p
                "frame-id\\|consumer-request-id\\|response-entry-id"
                (buffer-string)))))

          ;; Drop only test-local presentation/coordinator state, then reopen
          ;; the same durable Daily while its one consumer-shaped metadata /
          ;; association / visible-window snapshot is held.  The public chat
          ;; composer must still submit directly to SQLite before that read
          ;; settles.
          (when (buffer-live-p input)
            (kill-buffer input)
            (setq input nil))
          (when (buffer-live-p backing-chat)
            (kill-buffer backing-chat)
            (setq backing-chat nil))
          (when-let* ((binding (e-chat-service-binding harness session-id)))
            (e-chat-service--retire-binding binding))
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'chat-session-view)
          (setq existing-transcript
                (with-timeout
                    (1.0 (ert-fail "Existing Daily read-blocked open hung"))
                  (cl-letf (((symbol-function 'e-runtime-store-await)
                             (lambda (_store request &optional _timeout)
                               (setq synchronous-operation
                                     (e-runtime-store-request--operation request))
                               (error "Existing Daily open awaited %S"
                                      synchronous-operation))))
                    (e-chat-open :harness harness :session-id session-id))))
          (should (buffer-live-p existing-transcript))
          (e-chat-surface-pop-to-buffer existing-transcript)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-runtime-store-recovery-graphical--stall-ready-p
              stall-directory 'chat-session-view))
           3.0 "held existing Daily snapshot")
          (setq existing-composer
                (e-chat-surface-composer-buffer existing-transcript))
          (should (buffer-live-p existing-composer))
          (let ((original-post
                 (symbol-function 'e-chat-service--post-sqlite)))
            (cl-letf (((symbol-function 'e-chat-service--post-sqlite)
                       (lambda (&rest arguments)
                         (setq existing-admission
                               (apply original-post arguments)))))
              (with-current-buffer existing-composer
                (e-graphical-test-type-text "Submit while Daily read is held")
                (call-interactively #'e-chat-submit))))
          (should (e-work-handle-p existing-admission))
          (should-not synchronous-operation)
          (condition-case _wait-error
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (eq (plist-get (e-work-status existing-admission) :state)
                     'finished))
               3.0 "input commits while existing Daily view is held")
            (ert-test-failed
             (ert-fail
              (format
               (concat "Held-read submit did not commit: admission=%S "
                       "binding=%S runtime=%S suspect=%S status=%S stderr=%S")
               (e-work-status existing-admission)
               (when-let* ((binding
                            (e-chat-service-binding harness session-id)))
                 (list :state (e-chat-service-binding-lifecycle-state binding)
                       :participant
                       (e-chat-service-binding-participant-id binding)))
               (e-runtime-store-recovery-graphical--runtime-operations runtime)
               (e-session-persistence-suspect sessions session-id)
               (e-chat-surface-status existing-transcript)
               (when-let* ((buffer (e-runtime-store--stderr-buffer runtime))
                           ((buffer-live-p buffer)))
                 (with-current-buffer buffer
                   (buffer-substring-no-properties
                    (max (point-min) (- (point-max) 2000))
                    (point-max))))))))
          (should-not (e-graphical-test-stream-active-p stream))
          (with-current-buffer existing-composer
            (e-graphical-test-type-text "editable while view held"))
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'chat-session-view)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-graphical-test-stream-active-p stream))
           4.0 "provider starts after held context read releases")
          (e-graphical-test-stream-emit
           stream '(:type assistant-message
                     :content "Read-held Daily result committed once")
           0.01)
          (e-graphical-test-stream-finish stream 0.02)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (= (e-runtime-store-recovery-graphical--count-string
                 "Read-held Daily result committed once" existing-transcript)
                1))
           4.0 "existing Daily canonical result after held snapshot")
          (with-current-buffer existing-composer
            (should (string-match-p "editable while view held"
                                    (buffer-string)))
            (e-chat-composer-delete)
            (e-chat-composer-insert))

          ;; Fail one later write at the external-worker cut twice: once in
          ;; the original worker and once in its bounded replacement retry.
          ;; The public surface must show the terminal admission failure and
          ;; only this session/Board owner may become suspect.
          (e-runtime-store-recovery-graphical--arm-stall
           stall-directory 'board-append-route)
          (setq failure-composer
                (e-chat-surface-composer-buffer existing-transcript))
          (should (buffer-live-p failure-composer))
          (let ((original-post
                 (symbol-function 'e-chat-service--post-sqlite)))
            (cl-letf (((symbol-function 'e-chat-service--post-sqlite)
                       (lambda (&rest arguments)
                         (setq failure-admission
                               (apply original-post arguments)))))
              (with-current-buffer failure-composer
                (e-graphical-test-type-text "Fail only this Daily owner")
                (call-interactively #'e-chat-submit))))
          (should (e-work-handle-p failure-admission))
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-runtime-store-recovery-graphical--stall-ready-p
              stall-directory 'board-append-route))
           3.0 "failed owner write reaches worker cut")
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
                       runtime 'board-append-route))))
             3.0 "failed owner bounded worker retry")
            (delete-process (e-runtime-store--process runtime)))
          (condition-case _wait-error
              (e-graphical-test-wait-until
               (lambda ()
                 (e-runtime-store-recovery-graphical--pump-runtime runtime)
                 (and (e-session-persistence-suspect sessions session-id)
                      (string-match-p
                       "suspect\\|failed"
                       (concat
                        (or (e-chat-surface-status existing-transcript) "")
                        "\n"
                        (with-current-buffer existing-transcript
                          (buffer-string))))))
               4.0 "owner-local public persistence failure")
            (ert-test-failed
             (ert-fail
              (format
               (concat "Owner-local failure did not settle: admission=%S "
                       "runtime=%S active=%S recovering=%S attempt=%S "
                       "process=%S suspect=%S binding-error=%S status=%S "
                       "transcript=%S stderr=%S")
               (e-work-status failure-admission)
               (e-runtime-store-recovery-graphical--runtime-operations runtime)
               (when-let* ((request (e-runtime-store--active-request runtime)))
                 (list (e-runtime-store-request--operation request)
                       (e-runtime-store-request--state request)))
               (when-let* ((request
                            (e-runtime-store--recovering-request runtime)))
                 (list (e-runtime-store-request--operation request)
                       (e-runtime-store-request--state request)))
               (e-runtime-store--recovery-attempt runtime)
               (when-let* ((process (e-runtime-store--process runtime)))
                 (list (process-live-p process) (process-status process)))
               (e-session-persistence-suspect sessions session-id)
               (when-let* ((binding
                            (e-chat-service-binding harness session-id)))
                 (e-chat-service-binding-first-persistence-error binding))
               (e-chat-surface-status existing-transcript)
               (with-current-buffer existing-transcript
                 (buffer-substring-no-properties
                  (max (point-min) (- (point-max) 2000))
                  (point-max)))
               (when-let* ((buffer (e-runtime-store--stderr-buffer runtime))
                           ((buffer-live-p buffer)))
                 (with-current-buffer buffer
                   (buffer-substring-no-properties
                    (max (point-min) (- (point-max) 2000))
                    (point-max))))))))
          (should
           (e-chat-service-binding-first-persistence-error
            (e-chat-service-binding harness session-id)))
          (e-runtime-store-recovery-graphical--release-stall
           stall-directory 'board-append-route)

          ;; A fresh sibling uses the same failed transport lazily, commits a
          ;; new atomic admission, renders the canonical result, and remains
          ;; independently queryable.
          (setq sibling-transcript
                (with-timeout
                    (1.0 (ert-fail "Healthy sibling open hung"))
                  (e-chat-open :harness harness :new-session t)))
          (setq sibling-id
                (buffer-local-value 'e-chat-session-id sibling-transcript))
          (e-chat-surface-pop-to-buffer sibling-transcript)
          (setq sibling-composer
                (e-chat-surface-composer-buffer sibling-transcript))
          (should (buffer-live-p sibling-composer))
          (with-current-buffer sibling-composer
            (e-graphical-test-type-text "Healthy sibling prompt")
            (call-interactively #'e-chat-submit))
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (e-graphical-test-stream-active-p stream))
           4.0 "healthy sibling provider start")
          (e-graphical-test-stream-emit
           stream '(:type assistant-message
                     :content "Healthy sibling canonical result")
           0.01)
          (e-graphical-test-stream-finish stream 0.02)
          (e-graphical-test-wait-until
           (lambda ()
             (e-runtime-store-recovery-graphical--pump-runtime runtime)
             (= (e-runtime-store-recovery-graphical--count-string
                 "Healthy sibling canonical result" sibling-transcript)
                1))
           4.0 "healthy sibling canonical render")
          (let* ((view
                  (cl-letf (((symbol-function 'e-runtime-store-await)
                             #'e-runtime-store-recovery-graphical--await-with-pump))
                    (e-runtime-store-call
                     runtime 'read
                     (list :op 'chat-session-view
                           :session-id sibling-id :limit 16))))
                 (contents
                  (mapcar (lambda (message) (plist-get message :content))
                          (plist-get view :messages))))
            (should (= (cl-count "Healthy sibling prompt" contents
                                 :test #'equal)
                       1))
            (should (= (cl-count "Healthy sibling canonical result" contents
                                 :test #'equal)
                       1)))
          (should-not (e-session-persistence-suspect sessions sibling-id))
          (should (e-session-persistence-suspect sessions session-id)))
      (dolist (timer heartbeat-timers)
        (when (timerp timer) (cancel-timer timer)))
      (when (e-graphical-test-stream-p stream)
        (e-graphical-test-stream-cancel stream))
      (ignore-errors
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'chat-session-owner-admit))
      (ignore-errors
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'chat-session-input-admit))
      (ignore-errors
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'chat-session-view))
      (ignore-errors
        (e-runtime-store-recovery-graphical--release-stall
         stall-directory 'board-append-route))
      (dolist (buffer (list input backing-chat existing-transcript
                            existing-composer failure-composer
                            sibling-transcript sibling-composer target))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (when runtime
        (ignore-errors (e-runtime-store--finalize-close runtime)))
      (delete-directory directory t)
      (delete-directory canvas-directory t)
      (delete-directory stall-directory t))))

(provide 'e-runtime-store-recovery-behavior-test)

;;; e-runtime-store-recovery-behavior-test.el ends here
