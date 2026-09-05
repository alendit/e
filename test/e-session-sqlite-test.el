;;; e-session-sqlite-test.el --- SQLite session and tool continuity scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-session)
(require 'e-runtime-store-worker)

(cl-defmacro e-session-sqlite-test--with-store ((store directory) &rest body)
  "Run BODY with an opt-in STORE in disposable DIRECTORY."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-session-sqlite-test-" t))
          (,store (e-session-sqlite-store-create ,directory)))
     (unwind-protect (progn ,@body)
       (ignore-errors (e-session-sqlite-store-close ,store))
       (delete-directory ,directory t))))

(ert-deftest e-session-sqlite-s3-restores-session-branch-compaction-display-clear ()
  "SQLite preserves the session facade's semantic paths across restart."
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "source" :metadata '(:model "fake"))
    (let* ((first (e-session-append-message
                   store "source" '(:role user :content "one")))
           (second (e-session-append-message
                    store "source" '(:role assistant :content "two"))))
      (e-session-set-message-display store "source" (plist-get second :id) 'hidden)
      (e-session-append-compaction
       store "source" "compact" :first-kept-entry-id (plist-get first :id))
      (let ((fork (e-session-fork store "source" :at (plist-get first :id))))
        (should (= (length (e-session-messages store (plist-get fork :id))) 1)))
      (e-session-sqlite-store-close store)
      (setq store (e-session-sqlite-store-create directory))
      (let ((restored (e-session-get store "source")))
        (should (equal (mapcar (lambda (message)
                                (plist-get message :content))
                              (plist-get restored :messages))
                       '("one" "two")))
        (should (eq (plist-get (cadr (plist-get restored :messages)) :display)
                    'hidden))
        (should (= (length (plist-get restored :compactions)) 1)))
      (e-session-clear-messages store "source")
      (e-session-sqlite-store-close store)
      (setq store (e-session-sqlite-store-create directory))
      (should-not (e-session-messages store "source")))))

(ert-deftest e-session-sqlite-constructor-does-not-reconcile-journal-roots ()
  "Ordinary construction does not enumerate or materialize durable sessions."
  (e-session-sqlite-test--with-store (store directory)
    (let ((runtime (e-session-storage-runtime-store store)))
      (e-runtime-store-call
       runtime 'write
       '(:op session-append-batch :session-id "missing-catalog"
         :records [(:type "session" :session-id "missing-catalog"
                    :id "root" :timestamp "2026-09-02T00:00:00Z")
                   (:type "message" :session-id "missing-catalog"
                    :id "message" :parent-id "root"
                    :timestamp "2026-09-02T00:00:01Z"
                    :message (:role user :content "not replayed"))])))
    (e-session-sqlite-store-close store)
    (let ((read-page (symbol-function 'e-session-storage-read-session-page))
          page-calls)
      (cl-letf (((symbol-function 'e-session-storage-read-session-records)
                 (lambda (&rest _args)
                   (ert-fail "root reconciliation materialized a journal")))
                ((symbol-function 'e-session-storage-read-session-page)
                 (lambda (candidate session-id after limit)
                   (push (list session-id after limit) page-calls)
                   (funcall read-page candidate session-id after limit))))
        (setq store (e-session-sqlite-store-create directory)))
      (should-not page-calls)
      (should-not (e-session-session-present-p store "missing-catalog"))
      (should-error
       (e-session-aggregate-peek-session store "missing-catalog")
       :type 'e-session-missing))))

(ert-deftest e-session-sqlite-constructor-defers-session-replay-until-access ()
  "Ordinary construction leaves explicit session replay to first access."
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "catalog-lazy")
    (e-session-append-message
     store "catalog-lazy" '(:role user :content "restored on access"))
    (e-session-sqlite-store-close store)
    (let ((read-page (symbol-function 'e-session-storage-read-session-page))
          (read-records
           (symbol-function 'e-session-storage-read-session-records))
          (page-count 0)
          (record-count 0))
      (cl-letf (((symbol-function 'e-session-storage-read-session-page)
                 (lambda (&rest args)
                   (cl-incf page-count)
                   (apply read-page args)))
                ((symbol-function 'e-session-storage-read-session-records)
                 (lambda (&rest args)
                   (cl-incf record-count)
                   (apply read-records args))))
        (setq store (e-session-sqlite-store-create directory))
        (should-not (e-session-session-present-p store "catalog-lazy"))
        (should-error
         (e-session-aggregate-peek-session store "catalog-lazy")
         :type 'e-session-missing)
        (should (= page-count 0))
        (should (= record-count 0))
        (should (equal
                 (mapcar (lambda (message) (plist-get message :content))
                         (e-session-messages store "catalog-lazy"))
                 '("restored on access")))
        (should
         (plist-get
          (e-session-aggregate-peek-session store "catalog-lazy") :loaded))))))

(ert-deftest e-session-sqlite-load-all-remains-an-explicit-batch-boundary ()
  "Only an explicit LOAD-ALL construction eagerly replays all sessions."
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "checkpointed")
    (e-session-append-message
     store "checkpointed" '(:role user :content "from checkpoint"))
    (let* ((checkpoint
            (e-session-storage-read-resume-checkpoint store "checkpointed"))
           (opaque-checkpoint
            (plist-put (copy-tree checkpoint) :legacy-opaque
                       '(:false :json-false :pair (left . right))))
           (runtime (e-session-storage-runtime-store store)))
      (e-session-storage-persist-resume-checkpoint
       store "checkpointed" opaque-checkpoint)
      (e-runtime-store-call
       runtime 'write
       '(:op session-append-batch :session-id "checkpoint-less"
         :records [(:type "session" :session-id "checkpoint-less"
                    :id "plain-root" :timestamp "2026-09-02T00:00:00Z")
                   (:type "message" :session-id "checkpoint-less"
                    :id "plain-message" :parent-id "plain-root"
                    :timestamp "2026-09-02T00:00:01Z"
                    :message (:role user :content "full replay once"))]))
      (should-not
       (e-session-storage-resume-checkpoint-present-p
        store "checkpoint-less"))
      (should
       (equal (e-session-storage-read-resume-checkpoint store "checkpointed")
              opaque-checkpoint))
      (should-not
       (e-session-storage-resume-checkpoint-present-p
        store "checkpoint-less"))
      (e-session-sqlite-store-close store)
      (let ((read-records
             (symbol-function 'e-session-storage-read-session-records))
            (record-count 0))
        (cl-letf (((symbol-function 'e-session-storage-read-session-records)
                   (lambda (&rest args)
                     (cl-incf record-count)
                     (apply read-records args))))
          (setq store (e-session-sqlite-store-create directory :load-all t))
          (should (= (length (e-session-list store)) 2))
          (should (> record-count 0)))))))

(ert-deftest e-session-sqlite-first-lazy-load-rejects-same-session-reentry ()
  "A timer cannot mutate a session while its first replay awaits storage."
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "lazy-reentry")
    (e-session-append-message
     store "lazy-reentry" '(:role user :content "committed before reopen"))
    (e-session-sqlite-store-close store)
    (setq store (e-session-sqlite-store-create directory))
    (let* ((runtime (e-session-storage-runtime-store store))
           (process (e-runtime-store--process runtime))
           (ordinary-filter (process-filter process))
           (captured "")
           response-seen
           reentrant-result
           reentrant-delete-result)
      (set-process-filter
       process
       (lambda (worker text)
         (setq captured (concat captured text))
         (when (and (not response-seen) (string-match-p "\n" captured))
           (setq response-seen t)
           (run-at-time
            0 nil
            (lambda ()
              (setq reentrant-result
                    (condition-case err
                        (e-session-append-message
                         store "lazy-reentry"
                         '(:role user :content "must not commit"))
                      (e-session-persistence-unavailable
                       (list :unavailable (cadr err))))
                    reentrant-delete-result
                    (condition-case err
                        (e-session-delete store "lazy-reentry")
                      (e-session-persistence-unavailable
                       (list :unavailable (cadr err)))))))
           (run-at-time
            0.02 nil
            (lambda ()
              (set-process-filter worker ordinary-filter)
              (funcall ordinary-filter worker captured))))))
      (should (equal
               (mapcar (lambda (message) (plist-get message :content))
                       (e-session-messages store "lazy-reentry"))
               '("committed before reopen")))
      (should response-seen)
      (should (equal (car reentrant-result) :unavailable))
      (should (equal (car reentrant-delete-result) :unavailable))
      (should (= (plist-get
                  (e-session-storage-session-header store "lazy-reentry")
                  :record-count)
                 2))
      (e-session-append-message
       store "lazy-reentry" '(:role user :content "committed after replay"))
      (should (equal
               (mapcar (lambda (message) (plist-get message :content))
                       (e-session-messages store "lazy-reentry"))
               '("committed before reopen" "committed after replay")))
      (e-session-unload-session store "lazy-reentry")
      (cl-letf (((symbol-function 'e-session-load-session)
                 (lambda (&rest _args)
                   (signal 'e-session-storage-error
                           '("injected lazy-load failure")))))
        (should-error (e-session-get store "lazy-reentry")
                      :type 'e-session-storage-error))
      (should (= (length (e-session-messages store "lazy-reentry")) 2)))))

(ert-deftest e-session-sqlite-s3-tool-cut-points-restore-classification ()
  "Production session/activity paths and explicit later transitions persist."
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "tools")
    (e-session-append-message
     store "tools"
     '(:role tool-call :content (:id "call-1" :name "write" :arguments (:x 1))))
    (e-session-append-activity-event
     store "tools" "turn-1" 'tool-started
     '(:tool-call (:id "call-1" :name "write")))
    (e-session-tool-followup-transition
     store "tools" "call-1" 'started '(:request-id "request-1"))
    (e-session-append-activity-event
     store "tools" "turn-1" 'tool-finished
     '(:tool-call (:id "call-1" :name "write")
       :result (:status ok :content "done")))
    (e-session-tool-followup-transition
     store "tools" "call-1" 'follow-up-ready '(:entry-id "result"))
    (e-session-tool-followup-transition
     store "tools" "call-1" 'promoted '(:generation "g1"))
    (e-session-tool-followup-transition
     store "tools" "call-1" 'settled '(:turn-id "turn-1"))
    (e-session-tool-followup-transition
     store "tools" "call-uncertain" 'claimed '(:tool-name "external"))
    (e-session-sqlite-store-close store)
    (setq store (e-session-sqlite-store-create directory))
    (let ((facts (e-session-tool-followup-classifications store "tools")))
      (should (equal (mapcar (lambda (fact)
                              (cons (plist-get fact :call-id)
                                    (plist-get fact :state)))
                            facts)
                     '(("call-1" . settled)
                       ("call-uncertain" . claimed)))))))

(ert-deftest e-session-sqlite-s3-delete-purges-session-and-tool-state ()
  "Explicit deletion removes one private aggregate without touching another."
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "delete")
    (e-session-create store :id "keep")
    (e-session-tool-followup-transition
     store "delete" "call" 'claimed '(:tool-name "external"))
    (e-session-delete store "delete")
    (should-not (member "delete" (e-session-storage-session-ids store)))
    (should (member "keep" (e-session-storage-session-ids store)))
    (should-not (e-session-tool-followup-classifications store "delete"))))

(ert-deftest e-session-sqlite-s3-finalize-is-an-ordered-status-barrier ()
  "The explicit facade barrier cannot overtake an earlier submitted write."
  (e-session-sqlite-test--with-store (store directory)
    (let* ((runtime (e-session-storage-runtime-store store))
           (request
            (e-runtime-store-submit
             runtime 'write
             '(:op session-append :session-id "barrier"
               :record (:type "session" :session-id "barrier" :id "root"))))
           done error)
      (e-session-finalize
       store (lambda (_result) (setq done t))
       (lambda (err) (setq error err)))
      (should done)
      (should-not error)
      (should (eq (e-runtime-store-request--state request) 'committed))
      (should (= (plist-get
                  (e-session-storage-session-header store "barrier")
                  :record-count)
                 1)))))

(ert-deftest e-session-sqlite-s3-ordered-barrier-dispatches-status-only ()
  "The ordered barrier requests status without an integrity operation."
  (let (observed)
    (cl-letf (((symbol-function 'e-session-storage-sqlite--call)
               (lambda (store kind body)
                 (setq observed (list store kind body))
                 'acknowledged)))
      (should (eq (e-session-storage-sqlite-ordered-barrier 'store)
                  'acknowledged))
      (should (equal observed '(store read (:op status)))))))

(ert-deftest e-session-sqlite-s3-fork-batch-is-atomic-and-restorable ()
  "A fork commits its complete bounded record vector in one transaction."
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "fork-source")
    (e-session-append-message
     store "fork-source" '(:role user :content "one"))
    (e-session-append-message
     store "fork-source" '(:role assistant :content "two"))
    (let (fork)
      (cl-letf (((symbol-function 'e-session-identity-generate-id)
                 (lambda () "atomic-fork")))
        (setq fork (e-session-fork store "fork-source")))
      (should (equal (plist-get fork :id) "atomic-fork"))
      (should (= (plist-get
                  (e-session-storage-session-header store "atomic-fork")
                  :record-count)
                 3))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :content))
                             (e-session-messages store "atomic-fork"))
                     '("one" "two"))))
    (e-session-sqlite-store-close store)
    (setq store (e-session-sqlite-store-create directory))
    (should (= (plist-get
                (e-session-storage-session-header store "atomic-fork")
                :record-count)
               3))
    (should (equal (mapcar (lambda (message)
                             (plist-get message :content))
                           (e-session-messages store "atomic-fork"))
                   '("one" "two")))))

(ert-deftest e-session-sqlite-s3-primary-success-survives-projection-failure ()
  "A derived failure stays visible without misreporting primary append state."
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "projection-pending")
    (let (entry)
      (cl-letf (((symbol-function 'e-session-storage-sqlite-write-catalog)
                 (lambda (&rest _args)
                   (signal 'e-session-storage-error
                           '("injected derived projection failure")))))
        (setq entry
              (e-session-append-message
               store "projection-pending"
               '(:role user :content "committed once"))))
      (should (equal (plist-get entry :content) "committed once"))
      (should (= (length
                  (e-session-messages store "projection-pending"))
                 1))
      (let ((status (e-session-storage-durability-status store)))
        (should (plist-get status :index-write-pending))
        (should (> (plist-get status :checkpoint-dirty-count) 0))
        (should (eq (plist-get
                     (plist-get status :projection-last-error)
                     :symbol)
                    'e-session-storage-error)))
      (let (done error)
        (e-session-finalize
         store (lambda (_result) (setq done t))
         (lambda (err) (setq error err)))
        (should done)
        (should-not error))
      (let ((status (e-session-storage-durability-status store)))
        (should-not (plist-get status :index-write-pending))
        (should (= (plist-get status :checkpoint-dirty-count) 0))
        (should-not (plist-get status :projection-last-error)))
      (e-session-sqlite-store-close store)
      (setq store (e-session-sqlite-store-create directory))
      (should (equal
               (mapcar (lambda (message) (plist-get message :content))
                       (e-session-messages store "projection-pending"))
               '("committed once"))))))

(ert-deftest e-session-sqlite-s3-restores-exact-tagged-values ()
  "SQLite replay preserves exact detached Lisp value distinctions."
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "exact")
    (let ((table (make-hash-table :test 'equal))
          payload)
      (puthash "key" [nil t :json-false symbol :keyword (a . b)] table)
      (setq payload
            (list :nil nil :truth t :false :json-false :symbol 'symbol
                  :keyword :keyword :proper '(one two) :dotted '(a . b)
                  :vector [nil :x] :map table))
      (e-session-append-activity-event
       store "exact" "turn" 'exact-values payload)
      (e-session-sqlite-store-close store)
      (setq store (e-session-sqlite-store-create directory))
      (let* ((event (car (last (e-session-activity-events store "exact"))))
             (restored (plist-get event :payload))
             (restored-table (plist-get restored :map)))
        (should (equal (cl-loop for (key value) on payload by #'cddr
                                unless (eq key :map)
                                append (list key value))
                       (cl-loop for (key value) on restored by #'cddr
                                unless (eq key :map)
                                append (list key value))))
        (should (hash-table-p restored-table))
        (should (eq (hash-table-test restored-table) 'equal))
        (should (equal (gethash "key" restored-table)
                       [nil t :json-false symbol :keyword (a . b)]))))))

(ert-deftest e-session-sqlite-s3-restores-large-public-message ()
  "SQLite remains substitutable for legacy records above the old 64 KiB cap."
  (e-session-sqlite-test--with-store (store directory)
    (let ((content (make-string 70000 ?x)))
      (e-session-create store :id "large-message")
      (e-session-append-message
       store "large-message" (list :role 'user :content content))
      (e-session-sqlite-store-close store)
      (setq store (e-session-sqlite-store-create directory))
      (should (equal (plist-get
                      (car (e-session-messages store "large-message"))
                      :content)
                     content)))))

(ert-deftest e-session-sqlite-s3-commit-first-hides-staged-mutation ()
  "Reentrant facade work cannot observe or extend an unacknowledged append."
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "commit-first")
    (let* ((runtime (e-session-storage-runtime-store store))
           (process (e-runtime-store--process runtime))
           (ordinary-filter (process-filter process))
           (captured "")
           response-seen
           read-result
           list-count
           dependent-result)
      ;; Hold one complete worker response after COMMIT but before the runtime
      ;; can acknowledge it.  The timers run from the cooperative await used
      ;; by the production facade path.
      (set-process-filter
       process
       (lambda (worker text)
         (setq captured (concat captured text))
         (when (and (not response-seen) (string-match-p "\n" captured))
           (setq response-seen t)
           (run-at-time
            0 nil
            (lambda ()
              (setq read-result
                    (condition-case err
                        (e-session-messages store "commit-first")
                      (e-session-persistence-unavailable
                       (list :unavailable (cadr err))))
                    list-count
                    (plist-get
                     (seq-find
                      (lambda (session)
                        (equal (plist-get session :id) "commit-first"))
                      (e-session-list store))
                     :message-count)
                    dependent-result
                    (condition-case err
                        (e-session-append-message
                         store "commit-first"
                         '(:role user :content "dependent"))
                      (e-session-persistence-unavailable
                       (list :unavailable (cadr err)))))))
           (run-at-time
            0.02 nil
            (lambda ()
              (set-process-filter worker ordinary-filter)
              (funcall ordinary-filter worker captured))))))
      (e-session-append-message
       store "commit-first" '(:role user :content "committed"))
      (should response-seen)
      (should (equal (car read-result) :unavailable))
      (should (= list-count 0))
      (should (equal (car dependent-result) :unavailable))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :content))
                             (e-session-messages store "commit-first"))
                     '("committed"))))
    ;; A failed physical commit discards only the isolated stage.  A later
    ;; ordinary mutation starts from the still-committed live aggregate.
    (let ((commit (symbol-function 'e-session-storage-commit-mutation)))
      (cl-letf (((symbol-function 'e-session-storage-commit-mutation)
                 (lambda (&rest _args)
                   (signal 'e-session-storage-error '("forced failure")))))
        (should-error
         (e-session-append-message
          store "commit-first" '(:role user :content "failed"))
         :type 'e-session-storage-error))
      (should (= (length (e-session-messages store "commit-first")) 1))
      (cl-letf (((symbol-function 'e-session-storage-commit-mutation) commit))
        (e-session-append-message
         store "commit-first" '(:role user :content "recovered")))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :content))
                             (e-session-messages store "commit-first"))
                     '("committed" "recovered"))))
    ;; The tool admission fence is intentionally ordered before its session
    ;; record.  If that record fails, classification is conservative but the
    ;; tool-call entry is still absent from live model-facing state.
    (e-session-create store :id "fence-failure")
    (cl-letf (((symbol-function 'e-session-storage-commit-mutation)
               (lambda (&rest _args)
                 (signal 'e-session-storage-error '("record failure")))))
      (should-error
       (e-session-append-message
        store "fence-failure"
        '(:role tool-call
          :content (:id "uncertain-call" :name "external")))
       :type 'e-session-storage-error))
    (should-not (e-session-messages store "fence-failure"))
    (let ((facts
           (e-session-tool-followup-classifications
            store "fence-failure")))
      (should (= (length facts) 1))
      (should (equal (list (plist-get (car facts) :call-id)
                           (plist-get (car facts) :state))
                     '("uncertain-call" admitted))))))

(defconst e-session-sqlite-test--large-record-count 15722)
(defconst e-session-sqlite-test--large-content-bytes 37851478)

(ert-deftest e-session-sqlite-s92-c04-normal-checkpoint-resume-uses-two-reads ()
  "Normal resume issues metadata presence then one guarded value request."
  (let* ((directory (make-temp-file "e-session-checkpoint-reads-" t))
         (session-id "checkpoint-reads")
         store)
    (unwind-protect
        (progn
          (setq store (e-session-sqlite-store-create directory))
          (e-session-create store :id session-id)
          (e-session-append-message
           store session-id '(:role user :content "checkpointed"))
          (e-session-sqlite-store-close store)
          (setq store (e-session-sqlite-store-create directory))
          (let ((call (symbol-function 'e-runtime-store-call))
                checkpoint-operations)
            (cl-letf
                (((symbol-function 'e-runtime-store-call)
                  (lambda (runtime kind body)
                    (when (memq (plist-get body :op)
                                '(checkpoint-get checkpoint-read))
                      (push (plist-get body :op) checkpoint-operations))
                    (funcall call runtime kind body))))
              (let ((request (e-session-load-session-start
                              store session-id :chunk-bytes 1)))
                (while (not (e-request-terminal-p request))
                  (accept-process-output nil 0.01))
                (should (eq (e-request-lifecycle-state request) 'finished))))
            (should (equal (nreverse checkpoint-operations)
                           '(checkpoint-get checkpoint-read)))))
      (when store (e-session-sqlite-store-close store))
      (delete-directory directory t))))

(ert-deftest e-session-sqlite-s92-c04-legacy-oversized-checkpoint-replays-journal ()
  "A legacy oversized checkpoint stays local and public replay starts at zero."
  (let* ((directory (make-temp-file "e-session-legacy-checkpoint-" t))
         (session-id "legacy-oversized")
         store)
    (unwind-protect
        (progn
          (setq store (e-session-sqlite-store-create directory))
          (e-session-create store :id session-id)
          (e-session-append-message
           store session-id '(:role user :content "canonical journal"))
          (e-session-sqlite-store-close store)
          (setq store nil)
          (let* ((legacy-value
                  (list :version 1 :session-id session-id
                        :journal-byte-offset 1
                        :legacy-padding
                        (make-string
                         (1+ e-runtime-store-codec-checkpoint-canonical-byte-limit)
                         ?x)))
                 (canonical (e-runtime-store-codec-encode legacy-value)))
            (should (> (string-bytes canonical)
                       e-runtime-store-codec-checkpoint-canonical-byte-limit))
            ;; This is a retired oversized physical payload fixture.  It is
            ;; inserted only after the test worker closes, then reopened through
            ;; the public adapter; no live runtime or canonical database moves.
            (let ((database (sqlite-open (expand-file-name "store.sqlite3" directory))))
              (unwind-protect
                  (sqlite-execute
                   database
                   "INSERT INTO session_checkpoints(session_id,payload,revision) VALUES(?,?,?) ON CONFLICT(session_id) DO UPDATE SET payload=excluded.payload,revision=excluded.revision"
                   (vector session-id (base64-encode-string canonical t) 1))
                (sqlite-close database))))
          (setq store (e-session-sqlite-store-create directory))
          (let ((status
                 (e-runtime-store-call
                  (e-session-storage-runtime-store store) 'read
                  (list :op 'checkpoint-get :session-id session-id))))
            ;; The public metadata query proves the legacy value stayed inside
            ;; SQLite: it reports its size/usefulness without a payload field.
            (should (plist-get status :present))
            (should-not (plist-member status :value))
            (should-not (plist-get status :usable)))
          (should-not
           (e-session-storage-resume-checkpoint-present-p store session-id))
          (let ((request (e-session-load-session-start
                          store session-id :chunk-bytes 1)))
            (while (not (e-request-terminal-p request))
              (accept-process-output nil 0.01))
            (should (eq (e-request-lifecycle-state request) 'finished)))
          (should (equal
                   (mapcar (lambda (message) (plist-get message :content))
                           (e-session-messages store session-id))
                   '("canonical journal")))
          ;; The replayed aggregate remains writable; its next normal
          ;; projection can replace the legacy oversized checkpoint safely.
          (e-session-append-message
           store session-id '(:role assistant :content "healthy later write"))
          (should (equal
                   (mapcar (lambda (message) (plist-get message :content))
                           (e-session-messages store session-id))
                   '("canonical journal" "healthy later write")))
          (should (e-runtime-store-live-p
                   (e-session-storage-runtime-store store))))
      (when store (e-session-sqlite-store-close store))
      (delete-directory directory t))))

(ert-deftest e-session-sqlite-s3-large-indexed-paged-load-remains-responsive ()
  "The exact deterministic large fixture pages while timers and commits run."
  :tags '(:expensive)
  (e-session-sqlite-test--with-store (store directory)
    (e-session-create store :id "large")
    (let* ((message-count (1- e-session-sqlite-test--large-record-count))
           (base (/ e-session-sqlite-test--large-content-bytes message-count))
           (remainder (% e-session-sqlite-test--large-content-bytes message-count))
           (runtime (e-session-storage-runtime-store store))
           (parent (plist-get (e-session-get store "large") :root-event-id))
           (timestamp "2026-09-01T00:00:00Z")
           (batch nil)
           (batch-size 128)
           (actual-bytes 0))
      (dotimes (index message-count)
        (let* ((id (format "large-%05d" index))
               (size (+ base (if (< index remainder) 1 0)))
               (content (make-string size ?x))
               (record
                (list :type "message" :session-id "large" :timestamp timestamp
                      :id id :parent-id parent
                      :message (list :role 'user :content content
                                     :created-at timestamp :type 'message
                                     :id id :parent-id parent))))
          (cl-incf actual-bytes (string-bytes content))
          (setq parent id)
          (push record batch)
          (when (or (= (length batch) batch-size)
                    (= index (1- message-count)))
            (e-runtime-store-call
             runtime 'write
             (list :op 'session-append-batch :session-id "large"
                   :records (vconcat (nreverse batch))))
            (setq batch nil))))
      (should (= actual-bytes e-session-sqlite-test--large-content-bytes))
      (should (= (plist-get (e-session-storage-session-header store "large")
                            :record-count)
                 e-session-sqlite-test--large-record-count)))
    (e-session-sqlite-store-close store)
    (setq store (e-session-sqlite-store-create directory))
    (let* ((runtime (e-session-storage-runtime-store store))
           (timer-ticks 0)
           (timer (run-at-time 0 0.001 (lambda () (cl-incf timer-ticks))))
           queued-commit
           elapsed
           loaded)
      (unwind-protect
          (let ((started (float-time)))
            (setq loaded
                  (e-session-load-session-start
                   store "large" :chunk-bytes 256
                   :on-progress
                   (lambda (progress)
                     (when (and (null queued-commit)
                                (> (plist-get progress :records-read) 1))
                       (setq queued-commit
                             (e-runtime-store-submit
                              runtime 'write
                              '(:op session-append :session-id "concurrent"
                                :record (:type "session" :session-id
                                               "concurrent" :id "root"))))))))
            (while (not (e-request-terminal-p loaded))
              (accept-process-output nil 0.01))
            (setq elapsed (- (float-time) started)))
        (cancel-timer timer))
      (should (equal (list (e-request-lifecycle-state loaded)
                           (e-request-lifecycle-terminal-payload loaded))
                     (list 'finished
                           (e-request-lifecycle-terminal-payload loaded))))
      (should (> timer-ticks 0))
      (should queued-commit)
      (should (plist-get (e-runtime-store-await runtime queued-commit) :revision))
      (should (= (length (e-session-messages store "large"))
                 (1- e-session-sqlite-test--large-record-count)))
      (message "F87 large fixture records=%d content-bytes=%d elapsed=%.6f timer-ticks=%d"
               e-session-sqlite-test--large-record-count
               e-session-sqlite-test--large-content-bytes elapsed timer-ticks))))

(provide 'e-session-sqlite-test)

;;; e-session-sqlite-test.el ends here
