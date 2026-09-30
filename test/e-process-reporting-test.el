;;; e-process-reporting-test.el --- Tests for process reporting -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-actions)
(require 'e-backend)
(require 'e-harness)
(require 'e-json)
(require 'e-process-reporting)
(require 'e-session)
(require 'e-session-async)
(require 'e-tools)
(require 'e-work)
(require 'e-sqlite-test-store-support
         (expand-file-name
          "e-sqlite-test-store-support.el"
          (file-name-directory (or load-file-name buffer-file-name))))

(cl-defmacro e-process-reporting-test--with-store ((store directory) &body body)
  "Run BODY with an isolated persistent session STORE in DIRECTORY."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-process-reporting-" t))
          (,store (e-session-persistent-store-create ,directory)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,directory t))))

(defun e-process-reporting-test--harness (store)
  "Return a harness with process reporting and session STORE."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil)
                  :sessions store)))
    (e-harness-activate-capability
     harness (e-process-reporting-capability-create))
    (condition-case nil
        (e-session-local-state store "session-1")
      (e-session-missing
       (e-harness-create-session
        harness :id "session-1"
        :metadata '(:project-root "/tmp/project/"))))
    (puthash "session-1" (list :id "turn-1")
             (e-harness-active-turns harness))
    harness))

(defun e-process-reporting-test--context (harness)
  "Return the standard process-reporting action context for HARNESS."
  (list :harness harness :session-id "session-1" :turn-id "turn-1"))

(defun e-process-reporting-test--list (harness &optional arguments)
  "List current-session process markers in HARNESS."
  (let ((result
         (e-process-reporting-list
          (e-process-reporting-test--context harness) arguments)))
    (if (e-work-handle-p result)
        (e-work-with-batch-await
          (e-work-await-batch result :timeout 3))
      result)))

(defun e-process-reporting-test--read (harness marker-id)
  "Read current-session MARKER-ID in HARNESS."
  (let ((result
         (e-process-reporting-read
          (e-process-reporting-test--context harness) marker-id)))
    (if (e-work-handle-p result)
        (e-work-with-batch-await
          (e-work-await-batch result :timeout 3))
      result)))

(defun e-process-reporting-test--call-action
    (harness action arguments &optional context)
  "Call process reporting ACTION in HARNESS with ARGUMENTS."
  (e-actions-call
   'process-reporting action arguments
   (list :harness harness :session-id "session-1" :turn-id "turn-1"
         :context context)))

(defun e-process-reporting-test--await-action
    (harness action arguments &optional context session-id)
  "Dispatch and await process reporting ACTION at this test boundary."
  (let* ((session-id (or session-id "session-1"))
         (request
         (plist-get
          (e-actions-dispatch
           'process-reporting action arguments
           (list :harness harness :session-id session-id :turn-id "turn-1"
                 :context context))
          :request)))
    (e-work-with-batch-await
      (e-work-await-batch request :timeout 3))))

(defun e-process-reporting-test--await-work (work)
  "Return WORK's value at an explicit test-only await boundary."
  (if (e-work-handle-p work)
      (e-work-with-batch-await
        (e-work-await-batch work :timeout 3))
    work))

(defun e-process-reporting-test--inject-report-history
    (store session-id count &optional marker-id)
  "Append COUNT test reports, optionally triaging MARKER-ID, in SQL batches."
  (e-session-flush-write-queue store)
  (let* ((runtime (e-session-storage-runtime-store store))
         (state (e-runtime-store-call
                 runtime 'read
                 (list :op 'session-query-state :session-id session-id)))
         (position
          (plist-get
           (e-runtime-store-call
            runtime 'read
            (list :op 'session-record-page :session-id session-id
                  :order 'newest :limit 1))
           :high-water))
         (remaining count)
         (sequence 0))
    (while (> remaining 0)
      (let* ((batch-size (min remaining e-session-storage-batch-record-limit))
             (records
              (cl-loop
               repeat batch-size
               collect
               (let* ((id (format "historical-report-%d" sequence))
                      (report
                       (if marker-id
                           (list :report-type "triage" :id id
                                 :parent-id marker-id :marker-id marker-id
                                 :created-at "2026-09-11T00:00:00Z"
                                 :outcome "understood" :status "closed"
                                 :decision-note "Historical terminal triage.")
                         (list :report-type "historical-noise" :id id
                               :created-at "2026-09-11T00:00:00Z"))))
                 (setq sequence (1+ sequence))
                 (append
                  (list :type "process-report" :session-id session-id
                        :id id :timestamp "2026-09-11T00:00:00Z"
                        :report report)
                  (when marker-id (list :parent-id marker-id))))))
             (next-state (copy-tree state t)))
        (plist-put next-state :journal-position
                   (+ position batch-size))
        (plist-put next-state :updated-at "2026-09-11T00:00:00Z")
        (e-runtime-store-call
         runtime 'write
         (list :op 'session-append-batch :session-id session-id
               :records (vconcat records) :query-delta next-state))
        (setq state next-state
              position (+ position batch-size)
              remaining (- remaining batch-size))))))

(defun e-process-reporting-test--inject-open-markers
    (store session-id count)
  "Append COUNT open markers to SESSION-ID in bounded SQL batches."
  (e-session-flush-write-queue store)
  (let* ((runtime (e-session-storage-runtime-store store))
         (state (e-runtime-store-call
                 runtime 'read
                 (list :op 'session-query-state :session-id session-id)))
         (position
          (plist-get
           (e-runtime-store-call
            runtime 'read
            (list :op 'session-record-page :session-id session-id
                  :order 'newest :limit 1))
           :high-water))
         (remaining count)
         (sequence 0))
    (while (> remaining 0)
      (let* ((batch-size (min remaining e-session-storage-batch-record-limit))
             (records
              (cl-loop
               repeat batch-size
               collect
               (let ((id (format "open-marker-%d" sequence)))
                 (setq sequence (1+ sequence))
                 (list :type "process-report" :session-id session-id
                       :id id :timestamp "2026-09-11T00:00:00Z"
                       :report
                       (list :report-type "marker" :id id :marker-id id
                             :evidence-id (concat "evidence-" id)
                             :signal "success" :note "Newer open marker."
                             :created-at "2026-09-11T00:00:00Z")))))
             (next-state (copy-tree state t)))
        (plist-put next-state :journal-position (+ position batch-size))
        (plist-put next-state :updated-at "2026-09-11T00:00:00Z")
        (e-runtime-store-call
         runtime 'write
         (list :op 'session-append-batch :session-id session-id
               :records (vconcat records) :query-delta next-state))
        (setq state next-state
              position (+ position batch-size)
              remaining (- remaining batch-size))))))

(cl-defmacro e-process-reporting-test--with-async-harness
    ((store harness directory) &body body)
  "Run BODY with a disposable ordinary asynchronous SQLite HARNESS."
  (declare (indent 1) (debug ((symbolp symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-process-reporting-async-" t))
          (,store (e-session-sqlite-store-create ,directory :asynchronous t))
          (,harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions ,store)))
     (unwind-protect
         (progn
           (e-harness-activate-capability
            ,harness (e-process-reporting-capability-create))
           (e-work-with-batch-await
             (e-work-await-batch
              (e-harness-create-session
               ,harness :id "session-1"
               :metadata '(:project-root "/tmp/project/"))
              :timeout 3))
           (puthash "session-1" (list :id "turn-1")
                    (e-harness-active-turns ,harness))
           ,@body)
       (ignore-errors (e-session-sqlite-store-close ,store))
       (delete-directory ,directory t))))

(ert-deftest e-process-reporting-test-async-mark-uses-detached-query-and-commit-ack ()
  "Ordinary SQLite marking never reaches a local durable aggregate."
  (e-process-reporting-test--with-async-harness (store harness directory)
    (ignore store directory)
    (cl-letf (((symbol-function 'e-session-local-process-reports)
               (lambda (&rest _)
                 (error "process reporting read a local report mirror")))
              ((symbol-function 'e-session-local-activity-events)
               (lambda (&rest _)
                 (error "process reporting reread durable activity")))
              ((symbol-function 'e-harness-messages)
               (lambda (&rest _)
                 (error "process reporting reread the transcript"))))
      (let ((record
             (e-process-reporting-test--await-action
              harness :mark
              '(:signal "success" :note "The bounded path worked.")
              '(:tool-call (:id "marker-call" :name "process_marker")))))
        (should (equal (plist-get record :type) "marker"))
        (should (stringp (plist-get record :marker-id)))))))

(ert-deftest e-process-reporting-test-async-actions-use-one-bounded-detached-view ()
  "All reporting actions compose detached SQLite reads with acknowledged writes."
  (e-process-reporting-test--with-async-harness (store harness directory)
    (ignore directory)
    (e-harness-activity-emit-turn-event
     harness "session-1" "turn-1" 'provider-request-started
     '(:provider-request-id "request-1" :provider-request-ordinal 1))
    (let* ((marker
            (e-process-reporting-test--await-action
             harness :mark
             '(:signal "success" :note "Detached reporting worked.")))
           (marker-id (plist-get marker :marker-id)))
      (e-process-reporting-test--await-action
       harness :triage
       (list :marker-id marker-id :outcome "understood" :status "closed"
             :decision-note "The bounded query path is sufficient."))
      (e-process-reporting-test--await-action
       harness :record-extraction
       (list :marker-ids (vector marker-id)
             :session-evidence ["session://e/sessions/session-1/messages"]
             :provider-request-ids ["request-1"]
             :token-usage '(:input-tokens 5 :output-tokens 2)
             :estimation-method "provider-reported usage"))
      (e-process-reporting-test--await-work
       (e-process-reporting-record-request-shape
        (e-process-reporting-test--context harness)
        "request-1" 1
        '(:serialization "backend-neutral-elisp-v1"
          :actual-shape (:sha256 "a" :bytes 140)
          :without-passive-shape (:sha256 "b" :bytes 100)
          :without-active-shape (:sha256 "c" :bytes 120)
          :paired-shape (:sha256 "d" :bytes 80))))
      (let* ((listed (e-process-reporting-test--await-action
                      harness :list nil))
             (read (e-process-reporting-test--await-action
                    harness :read (list :marker-id marker-id)))
             (cost (e-process-reporting-test--await-action
                    harness :cost-report nil))
             (page (e-process-reporting-test--await-work
                    (e-session-async-record-page
                     store "session-1" :record-type "process-report"
                     :limit e-process-reporting-record-limit))))
        (should (= (length listed) 1))
        (should (equal (plist-get (aref listed 0) :status) "closed"))
        (should (= (length (plist-get read :triage)) 1))
        (should (= (length (plist-get read :extractions)) 1))
        (should (= (plist-get cost :measured-request-count) 1))
        (should (= (plist-get cost :marker-count) 1))
        (should (= (length (plist-get page :records)) 4))))))

(ert-deftest e-process-reporting-test-process-marker-bridges-pending-commit-work ()
  "The model tool stays pending and returns only its committed marker record."
  (e-process-reporting-test--with-async-harness (store harness directory)
    (ignore store directory)
    (let ((submit (symbol-function 'e-session-storage-submit))
          held
          tool-work
          tool-result
          tool-error)
      (cl-letf (((symbol-function 'e-session-storage-submit)
                 (lambda (actual-store kind request on-settle &optional escrow)
                   (if (and (eq kind 'read)
                            (eq (plist-get request :op)
                                'session-process-report-marker)
                            (null held))
                       (progn
                         (setq held (list :request request :settle on-settle))
                         :held-process-report-read)
                     (funcall submit actual-store kind request on-settle escrow)))))
        (e-tools-start
         (e-harness-tools harness "session-1" "turn-1")
         '(:id "marker-call" :name "process_marker"
           :arguments (:signal "success"
                       :note "Commit acknowledgement is observable."))
         :context (list :harness harness :session-id "session-1"
                        :turn-id "turn-1")
         :on-work-prepared (lambda (work) (setq tool-work work))
         :on-done (lambda (result) (setq tool-result result))
         :on-error (lambda (error) (setq tool-error error)))
        (should held)
        (should (e-work-handle-p tool-work))
        (should (eq (plist-get (e-work-status tool-work) :state) 'started))
        (should-not tool-result)
        (should-not tool-error)
        (funcall (plist-get held :settle)
                 '(:marker nil) nil)
        (let ((record (e-process-reporting-test--await-work tool-work)))
          (should (equal (plist-get record :type) "marker")))
        (should-not tool-error)
        (should (equal (plist-get tool-result :status) 'ok))
        (should (equal (plist-get (plist-get tool-result :content) :type)
                       "marker"))
        (should-not (stringp (plist-get tool-result :content)))))))

(ert-deftest e-process-reporting-test-process-marker-surfaces-persistence-failure ()
  "The model tool fails instead of reporting success before persistence."
  (e-process-reporting-test--with-async-harness (store harness directory)
    (ignore store directory)
    (let ((submit (symbol-function 'e-session-storage-submit-owned))
          tool-work
          tool-result
          tool-error)
      (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                 (lambda (actual-store session-id body on-settle
                                       &optional escrow)
                   (if (eq (plist-get (plist-get body :command) :tag)
                           'process-report)
                       (progn
                         (funcall on-settle nil
                                  '(e-runtime-store-error
                                    "process marker persistence failed"))
                         :failed-process-marker-write)
                     (funcall submit actual-store session-id body on-settle
                              escrow)))))
        (e-tools-start
         (e-harness-tools harness "session-1" "turn-1")
         '(:id "marker-call" :name "process_marker"
           :arguments (:signal "failure"
                       :note "The durable marker write must settle."))
         :context (list :harness harness :session-id "session-1"
                        :turn-id "turn-1")
         :on-work-prepared (lambda (work) (setq tool-work work))
         :on-done (lambda (result) (setq tool-result result))
         :on-error (lambda (error) (setq tool-error error)))
        (should (e-work-handle-p tool-work))
        (should-error (e-process-reporting-test--await-work tool-work)
                      :type 'e-runtime-store-error)
        (should-not tool-error)
        (should (eq (plist-get tool-result :status) 'error))
        (should (string-match-p
                 "process marker persistence failed"
                 (format "%s" (plist-get tool-result :content))))))))

(ert-deftest e-process-reporting-test-async-read-failure-is-request-local ()
  "One failed report read does not suspect or block a sibling session."
  (e-process-reporting-test--with-async-harness (store harness directory)
    (ignore directory)
    (e-process-reporting-test--await-work
     (e-harness-create-session harness :id "session-2"))
    (puthash "session-2" (list :id "turn-1")
             (e-harness-active-turns harness))
    (let ((submit (symbol-function 'e-session-storage-submit)))
      (cl-letf (((symbol-function 'e-session-storage-submit)
                 (lambda (actual-store kind request on-settle &optional escrow)
                   (if (and (eq kind 'read)
                            (equal (plist-get request :session-id) "session-1")
                            (eq (plist-get request :op)
                                'session-process-report-marker-page))
                       (progn
                         (funcall on-settle nil
                                  '(e-runtime-store-error
                                    "isolated process-report read failure"))
                         :failed-process-report-read)
                     (funcall submit actual-store kind request on-settle escrow)))))
        (should-error
         (e-process-reporting-test--await-action harness :list nil)
         :type 'e-runtime-store-error)
        (should-not
         (e-process-reporting-test--await-action
          harness :list nil nil "session-2"))))
    (should-not (e-session-async-session-suspect store "session-1"))
    (should-not (e-session-async-session-suspect store "session-2"))))

(ert-deftest e-process-reporting-test-async-write-failure-is-owner-local ()
  "A failed report append suspects only its session and a sibling persists."
  (e-process-reporting-test--with-async-harness (store harness directory)
    (ignore directory)
    (e-process-reporting-test--await-work
     (e-harness-create-session harness :id "session-2"))
    (puthash "session-2" (list :id "turn-1")
             (e-harness-active-turns harness))
    (let ((submit (symbol-function 'e-session-storage-submit-owned))
          (failed nil))
      (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                 (lambda (actual-store session-id body on-settle
                                       &optional escrow)
                   (if (and (not failed)
                            (equal session-id "session-1")
                            (eq (plist-get (plist-get body :command) :tag)
                                'process-report))
                       (progn
                         (setq failed t)
                         (funcall on-settle nil
                                  '(e-runtime-store-error
                                    "isolated process-report write failure"))
                         :failed-process-report-write)
                     (funcall submit actual-store session-id body on-settle
                              escrow)))))
        (should-error
         (e-process-reporting-test--await-action
          harness :mark '(:signal "failure" :note "This write fails."))
         :type 'e-runtime-store-error)
        (let ((sibling
               (e-process-reporting-test--await-action
                harness :mark
                '(:signal "success" :note "The sibling remains writable.")
                nil "session-2")))
          (should (equal (plist-get sibling :type) "marker")))))
    (should (e-session-async-session-suspect store "session-1"))
    (should-not (e-session-async-session-suspect store "session-2"))))

(ert-deftest e-process-reporting-test-history-size-does-not-disable-current-actions ()
  "A marker remains exact after more triages than the physical page cap."
  (e-process-reporting-test--with-async-harness (store harness directory)
    (ignore directory)
    (let* ((arguments
            '(:signal "repetition" :note "This exact evidence is durable."))
           (marker (e-process-reporting-test--await-action
                    harness :mark arguments))
           (marker-id (plist-get marker :marker-id)))
      (e-process-reporting-test--inject-report-history
       store "session-1" (1+ e-process-reporting-record-limit) marker-id)
      (let ((again (e-process-reporting-test--await-action
                    harness :mark arguments))
            (read (e-process-reporting-test--await-action
                   harness :read (list :marker-id marker-id))))
        (should (plist-get again :suppressed))
        (should (equal (plist-get (plist-get read :marker) :marker-id)
                       marker-id))
        (should (= (length (plist-get read :triage))
                   e-process-reporting-record-limit)))
      (let ((triage
             (e-process-reporting-test--await-action
              harness :triage
              (list :marker-id marker-id :outcome "understood"
                    :status "closed"
                    :decision-note "The canonical marker remains addressable."))))
        (should (equal (plist-get triage :parent-id) marker-id))
        (should-not (equal (plist-get triage :id) marker-id)))
      (let* ((new-marker
              (e-process-reporting-test--await-action
               harness :mark
               '(:signal "success" :note "Newest reporting remains live.")))
             (listed (e-process-reporting-test--await-action
                      harness :list nil)))
        (should
         (seq-find
          (lambda (entry)
            (equal (plist-get entry :marker-id)
                   (plist-get new-marker :marker-id)))
          listed))))))

(ert-deftest e-process-reporting-test-status-filter-precedes-marker-page-limit ()
  "A closed marker remains listable behind more than one page of open markers."
  (e-process-reporting-test--with-async-harness (store harness directory)
    (ignore directory)
    (let* ((marker
            (e-process-reporting-test--await-action
             harness :mark
             '(:signal "success" :note "This older marker is closed.")))
           (marker-id (plist-get marker :marker-id)))
      (e-process-reporting-test--await-action
       harness :triage
       (list :marker-id marker-id :outcome "understood" :status "closed"
             :decision-note "The older marker is terminal."))
      (e-process-reporting-test--inject-open-markers
       store "session-1" (1+ e-process-reporting-record-limit))
      (let ((closed
             (e-process-reporting-test--await-action
              harness :list '(:status "closed"))))
        (should (= (length closed) 1))
        (should (equal (plist-get (aref closed 0) :marker-id) marker-id))
        (should (equal (plist-get (aref closed 0) :status) "closed"))))))

(ert-deftest e-process-reporting-test-extraction-validates-raw-marker-bound-before-reads ()
  "Raw extraction cardinality is bounded before dedupe or exact marker reads."
  (e-process-reporting-test--with-async-harness (store harness directory)
    (ignore store directory)
    (let* ((marker
            (e-process-reporting-test--await-action
             harness :mark
             '(:signal "success" :note "Extraction marker.")))
           (marker-id (plist-get marker :marker-id))
           (original (symbol-function
                      'e-session-async-process-report-marker))
           (reads 0))
      (cl-letf (((symbol-function 'e-session-async-process-report-marker)
                 (lambda (&rest arguments)
                   (setq reads (1+ reads))
                   (apply original arguments))))
        (should-error
         (e-process-reporting-record-extraction
          (list :marker-ids (vconcat (make-list 65 marker-id))
                :estimation-method "provider-reported usage")
          (e-process-reporting-test--context harness))
         :type 'user-error)
        (should (= reads 0))
        (let ((result
               (e-process-reporting-record-extraction
                (list :marker-ids (vector marker-id marker-id)
                      :estimation-method "provider-reported usage")
                (e-process-reporting-test--context harness))))
          (e-process-reporting-test--await-work result)
          (should (= reads 1)))))))

(ert-deftest e-process-reporting-test-production-callers-have-no-report-mirror ()
  "Reporting consumers do not call the obsolete session report aggregate."
  (dolist (file '("lisp/layers/harness/e-process-reporting.el"
                  "lisp/layers/harness/e-session-resources.el"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file))
      (goto-char (point-min))
      (should-not (search-forward "e-session-local-process-reports" nil t)))))

(ert-deftest e-process-reporting-test-current-turn-event-window-is-bounded-and-terminal ()
  "Process-report evidence exists only as bounded executing-turn coordination."
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (dotimes (index (+ e-harness-activity-current-turn-event-limit 7))
        (e-harness-activity-emit-turn-event
         harness "session-1" "turn-1" 'provider-request-started
         (list :provider-request-id (format "request-%d" index)
               :provider-request-ordinal index)))
      (let ((events (e-harness-activity-current-turn-events
                     harness "session-1" "turn-1")))
        (should (= (length events)
                   e-harness-activity-current-turn-event-limit))
        (should (equal (plist-get (plist-get (car events) :payload)
                                  :provider-request-id)
                       "request-7")))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'turn-failed
       '(:error (:message "terminal")))
      (should-not
       (e-harness-activity-current-turn-events
        harness "session-1" "turn-1")))))

(ert-deftest e-process-reporting-test-tool-is-tiny-and-returns-committed-record ()
  (e-process-reporting-test--with-store (store directory)
    (let* ((harness (e-process-reporting-test--harness store))
           (registry (e-harness-tools harness "session-1" "turn-1"))
           (definition
            (seq-find (lambda (item)
                        (equal (plist-get item :name) "process_marker"))
                      (e-tools-definitions registry)))
           result)
      (should (equal (plist-get definition :description)
                     "Save one coarse, reusable process observation."))
      (should (equal (plist-get (plist-get definition :parameters) :required)
                     ["signal" "note"]))
      (setq result
            (e-tools--execute-batch-with-context
             registry
             '(:id "marker-call" :name "process_marker"
               :arguments (:signal "success"
                           :note "A short method worked."))
             (list :harness harness :session-id "session-1"
                   :turn-id "turn-1")))
      (should (equal (plist-get (plist-get result :content) :type) "marker"))
      (should (stringp
               (plist-get (plist-get result :content) :marker-id))))))

(ert-deftest e-process-reporting-test-guidance-selects-coarse-reusable-flows ()
  "Marker guidance makes routine and self-referential capture exceptional."
  (let ((guidance (e-capability-instructions
                   (e-process-reporting-capability-create))))
    (should (string-match-p "multi-step or complex flow" guidance))
    (should (string-match-p "reusable process observation" guidance))
    (should (string-match-p "most turns should have none" guidance))
    (should (string-match-p "ordinary corrections" guidance))
    (should (string-match-p "requests to change marker frequency" guidance))
    (should-not (string-match-p "signals always qualify" guidance))
    (should-not (string-match-p "scan the WHOLE turn" guidance))
    (should-not (string-match-p "none.*rare" guidance))))

(ert-deftest e-process-reporting-test-capture-attaches-content-free-evidence-and-links ()
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'tool-started
       '(:id "failed-call" :name "bash"
         :arguments (:authorization "Bearer top-secret" :command "false")))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'tool-finished
       '(:tool-call (:id "failed-call" :name "bash")
         :result (:tool-call-id "failed-call" :name "bash" :status error
                  :content "token=private-value failed" :metadata nil)))
      (let* ((record
              (e-process-reporting-test--call-action
               harness :mark
               '(:signal "failure" :note "The shell operation failed.")
               '(:tool-call (:id "marker-call" :name "process_marker"))))
             (trigger (plist-get record :trigger))
             (serialized (prin1-to-string record)))
        (should (equal (plist-get record :type) "marker"))
        (should (equal (plist-get record :session-uri)
                       "session://e/sessions/session-1/"))
        (should (equal (plist-get record :process-reports-uri)
                       "session://e/sessions/session-1/process-reports"))
        (should (equal (plist-get record :tool-call-id) "marker-call"))
        (should (equal (plist-get trigger :call-id) "failed-call"))
        (should (equal (plist-get trigger :event-type) "tool-finished"))
        (should (equal (plist-get trigger :name) "bash"))
        (should (stringp (plist-get trigger :activity-event-id)))
        (should (= (length (plist-get record :trigger-chain)) 1))
        (should (equal (plist-get trigger :activity-event-id)
                       (plist-get (aref (plist-get record :trigger-chain) 0)
                                  :activity-event-id)))
        (should (eq (plist-get trigger :arguments-preview) e-json-null))
        (should (eq (plist-get trigger :result-preview) e-json-null))
        (should (eq (plist-get trigger :error-preview) e-json-null))
        (should-not (string-match-p "REDACTED" serialized))
        (should-not (string-match-p "top-secret\\|private-value" serialized))))))

(ert-deftest e-process-reporting-test-effective-action-feedback-is-task-relative ()
  "Positive action feedback retains the action identity and agent judgment."
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'action-started
       '(:action-call-id "action-1" :capability-id web :action fetch
         :arguments (:uri "https://example.test") :status started))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'action-finished
       '(:action-call-id "action-1" :capability-id web :action fetch
         :status ok :result (:content "done")))
      (let* ((record
              (e-process-reporting-test--call-action
               harness :mark
               '(:signal "effective"
                 :note "The fetch action supplied the task evidence directly.")))
             (trigger (plist-get record :trigger)))
        (should (equal (plist-get record :signal) "effective"))
        (should (equal (plist-get record :note)
                       "The fetch action supplied the task evidence directly."))
        (should (equal (plist-get trigger :call-id) "action-1"))
        (should (equal (plist-get trigger :name) "web/fetch"))))))

(ert-deftest e-process-reporting-test-durable-list-read-and-append-only-triage ()
  (e-process-reporting-test--with-store (store directory)
    (let* ((harness (e-process-reporting-test--harness store))
           (marker (e-process-reporting-test--call-action
                    harness :mark
                    '(:signal "missing-operation"
                      :note "A direct operation was absent.")))
           (marker-copy (copy-tree marker))
           (marker-id (plist-get marker :id)))
      (e-process-reporting-test--call-action
       harness :triage
       (list :marker-id marker-id :outcome "missing-capability"
             :status "routed" :decision-note "Track as feature work."
             :target-reference "docs/feats/99-example/"))
      (should (equal marker marker-copy))
      (let* ((loaded-store (e-session-persistent-store-create directory))
             (loaded-harness (e-process-reporting-test--harness loaded-store))
             (listed (e-process-reporting-test--list loaded-harness))
             (read (e-process-reporting-test--read loaded-harness marker-id)))
        (should (= (length listed) 1))
        (should (equal (plist-get (car listed) :status) "routed"))
        (should (equal (plist-get (plist-get read :marker) :note)
                       "A direct operation was absent."))
        (should (= (length (plist-get read :triage)) 1))
        (should (equal (plist-get (car (plist-get read :triage)) :type)
                       "triage"))
        (should (= (length (e-session-local-process-reports
                            loaded-store "session-1"))
                   2))))))

(ert-deftest e-process-reporting-test-terminal-evidence-is-suppressed-until-changed ()
  (e-process-reporting-test--with-store (store directory)
    (let* ((harness (e-process-reporting-test--harness store))
           (arguments '(:signal "repetition" :note "The same steps repeated."))
           (marker (e-process-reporting-test--call-action harness :mark arguments)))
      (e-process-reporting-test--call-action
       harness :triage
       (list :marker-id (plist-get marker :id) :outcome "duplicate"
             :status "rejected" :decision-note "Already understood."))
      (let ((again (e-process-reporting-test--call-action
                    harness :mark arguments))
            (changed (e-process-reporting-test--call-action
                      harness :mark
                      '(:signal "repetition" :note "Different evidence repeated."))))
        (should (plist-get again :suppressed))
        (should-not (plist-get changed :suppressed))
        (should (= (length (e-process-reporting-test--list harness)) 2))))))

(ert-deftest e-process-reporting-test-extraction-ledger-stays-separate ()
  (e-process-reporting-test--with-store (store directory)
    (let* ((harness (e-process-reporting-test--harness store))
           (marker (e-process-reporting-test--call-action
                    harness :mark
                    '(:signal "workaround" :note "A workaround succeeded.")))
           (marker-id (plist-get marker :id)))
      (e-process-reporting-test--call-action
       harness :record-extraction
       (list :marker-ids (vector marker-id)
             :session-evidence ["session://e/sessions/session-1/messages"]
             :provider-request-ids ["request-9"]
             :token-usage '(:input-tokens 12 :output-tokens 3)
             :estimation-method "provider-reported batch usage"))
      (let ((read (e-process-reporting-test--read harness marker-id)))
        (should (= (length (plist-get read :extractions)) 1))
        (should (equal (plist-get (car (plist-get read :extractions)) :type)
                       "extraction"))
        (should-not (plist-member (plist-get read :marker) :token-usage))))))

(ert-deftest e-process-reporting-test-request-shape-report-preserves-paired-measures ()
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'provider-request-started
       '(:provider-request-id "request-1"
         :provider-request-ordinal 1
         :caused-by-tool-call-id "marker-call"
         :caused-by-tool-name "process_marker"
         :caused-by-tool-calls [(:id "marker-call" :name "process_marker")]))
      (e-process-reporting-record-request-shape
       (e-process-reporting-test--context harness)
       "request-1" 1
       '(:serialization "backend-neutral-elisp-v1"
         :actual-shape (:sha256 "a" :bytes 140)
         :without-passive-shape (:sha256 "b" :bytes 100)
         :without-active-shape (:sha256 "c" :bytes 120)
         :paired-shape (:sha256 "d" :bytes 80)))
      (let* ((report (e-process-reporting-test--call-action
                      harness :cost-report nil))
             (entry (aref (plist-get report :requests) 0)))
        (should (equal (plist-get report :scope)
                       "request-shape-counterfactual"))
        (should (= (plist-get entry :direct-context-delta-bytes) 60))
        (should (= (plist-get entry :passive-surface-bytes) 40))
        (should (= (plist-get entry :active-marker-bytes) 20))
        (should (= (plist-get report :measured-request-count) 1))
        (should (equal (plist-get report :measurement-status) "recorded"))
        (should (plist-get entry :marker-follow-up))
        (should (equal (plist-get report :estimation-method)
                       "backend-neutral-serialized-utf-8-bytes"))
        (should (eq (plist-get report :provider-tokenizer-used) e-json-false))
        (should (eq (plist-get report :behavioral-estimate) e-json-false))))))

(ert-deftest e-process-reporting-test-request-shape-measurement-is-explicit-and-owned ()
  "Explicit accounting removes only capability-owned marker surfaces."
  (let* ((marker-message '(:role system :content "marker guidance"))
         (unrelated
          '(:role system
            :content "A project policy mentions process_marker but is unrelated."))
         (messages (list marker-message unrelated '(:role user :content "hi")))
         (options '(:tools ((:name "process_marker") (:name "echo"))))
         (segments
          (list (list :id '(process-reporting instructions)
                      :messages (list marker-message))))
         (shape
          (e-process-reporting-measure-request-shape
           messages options segments))
         (expected-options (copy-tree options)))
    (plist-put expected-options :tools '((:name "echo")))
    (should (equal (plist-get shape :revision) "request-shape-v2"))
    (should
     (equal
      (plist-get (plist-get shape :without-passive-shape) :sha256)
      (plist-get
       (e-process-reporting--shape-value
        (e-process-reporting--request-snapshot
         (list unrelated '(:role user :content "hi")) expected-options))
       :sha256)))
    (should-not
     (equal
      (plist-get (plist-get shape :without-passive-shape) :sha256)
      (plist-get
       (e-process-reporting--shape-value
        (e-process-reporting--request-snapshot
         '((:role user :content "hi")) expected-options))
       :sha256)))))

(ert-deftest e-process-reporting-test-cost-report-is-honest-when-unmeasured ()
  "Ordinary request events report that no counterfactual was recorded."
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'provider-request-started
       '(:provider-request-id "request-1"
         :provider-request-ordinal 1))
      (let* ((report (e-process-reporting-test--call-action
                      harness :cost-report nil))
             (entry (aref (plist-get report :requests) 0)))
        (should (equal (plist-get report :measurement-status)
                       "not-recorded"))
        (should (= (plist-get report :measured-request-count) 0))
        (should (eq (plist-get report :direct-context-delta-bytes) e-json-null))
        (should (equal (plist-get entry :measurement-status)
                       "not-recorded"))
        (should (eq (plist-get entry :actual-bytes) e-json-null))))))

(ert-deftest e-process-reporting-test-reports-use-session-store-not-transcript ()
  (e-process-reporting-test--with-store (store directory)
    (let* ((harness (e-process-reporting-test--harness store))
           (marker (e-process-reporting-test--call-action
                    harness :mark '(:signal "success" :note "Stored once.")))
           (reports (e-session-local-process-reports store "session-1"))
           (durable-records
            (prin1-to-string
             (e-session-storage-read-session-records store "session-1"))))
      (should (= (length reports) 1))
      (should (equal (plist-get (car reports) :id) (plist-get marker :id)))
      (should-not (e-session-local-messages store "session-1"))
      (should (string-match-p "process-report" durable-records))
      (should-not (string-match-p "records.jsonl" durable-records)))))

(ert-deftest e-process-reporting-test-terminal-suppression-is-session-scoped ()
  (e-process-reporting-test--with-store (store directory)
    (let* ((harness (e-process-reporting-test--harness store))
           (arguments '(:signal "repetition" :note "Shared evidence."))
           (marker (e-process-reporting-test--call-action
                    harness :mark arguments)))
      (e-process-reporting-test--call-action
       harness :triage
       (list :marker-id (plist-get marker :id) :outcome "duplicate"
             :status "rejected" :decision-note "Terminal."))
      (e-harness-create-session harness :id "session-2")
      (let ((other
             (e-actions-call
              'process-reporting :mark arguments
              (list :harness harness :session-id "session-2"
                    :turn-id "turn-1"))))
        (should-not (plist-get other :suppressed))
        (should (= (length (e-session-local-process-reports store "session-1")) 2))
        (should (= (length (e-session-local-process-reports store "session-2")) 1))))))

(ert-deftest e-process-reporting-test-marker-rejects-extra-fields-before-handler ()
  (e-process-reporting-test--with-store (store directory)
    (let* ((harness (e-process-reporting-test--harness store))
           (registry (e-harness-tools harness "session-1" "turn-1"))
           (result
            (e-tools--execute-batch-with-context
             registry
             '(:id "marker-call" :name "process_marker"
               :arguments (:signal "success" :note "Short." :impact "long"))
             (list :harness harness :session-id "session-1"
                   :turn-id "turn-1"))))
      (should (eq (plist-get result :status) 'error))
      (should-not (e-process-reporting-test--list harness)))))

(ert-deftest e-process-reporting-test-loop-rejection-projects-undeclared-fields ()
  (e-process-reporting-test--with-store (store directory)
    (let* ((harness (e-process-reporting-test--harness store))
           (registry (e-harness-tools harness "session-1" "turn-1"))
           (calls 0)
           (backend
            (e-backend-create
             :name "marker-rejection"
             :stream
             (cl-function
              (lambda (&key messages options on-item)
                (ignore messages options)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall on-item
                               '(:type tool-call :id "marker-call"
                                 :name "process_marker"
                                 :arguments
                                 (:signal "success" :note "Short."
                                  :impact "must-not-retain")))
                      (funcall on-item '(:type done :reason tool-use)))
                  (funcall on-item '(:type assistant-message :content "done"))
                  (funcall on-item '(:type done :reason stop)))))))
           appended)
      (e-loop-run-turn-batch
       :session-id "session-1" :turn-id "turn-1"
       :messages '((:role user :content "hi"))
       :backend backend :tools registry
       :options (list :tools (e-tools-definitions registry))
       :on-event (lambda (type payload)
                   (e-harness-activity-emit-turn-event
                    harness "session-1" "turn-1" type payload))
       :append-message (lambda (message) (push message appended)))
      (let ((serialized
             (prin1-to-string
              (list appended
                    (e-session-local-activity-events
                     (e-harness-sessions harness) "session-1")))))
        (should-not (string-match-p "must-not-retain\\|:impact" serialized))
        (should-not (e-process-reporting-test--list harness))))))

(ert-deftest e-process-reporting-test-loop-rejects-invalid-marker-schema-before-persistence ()
  (dolist (case
           '((missing-note (:signal "success") nil)
             (wrong-signal-type (:signal 7 :note "Short.") nil)
             (invalid-signal (:signal "unknown" :note "Short.") nil)
             (blank-note (:signal "success" :note "   ") nil)
             (multiline-note (:signal "success" :note "first\nsecret-line")
                             "secret-line")
             (overlong-note (:signal "success" :note nil) "long-secret")))
    (e-process-reporting-test--with-store (store directory)
      (let* ((harness (e-process-reporting-test--harness store))
             (registry (e-harness-tools harness "session-1" "turn-1"))
             (arguments (copy-tree (nth 1 case)))
             (secret (nth 2 case))
             (calls 0)
             appended)
        (when (eq (car case) 'overlong-note)
          (setq arguments
                (plist-put arguments :note
                           (concat "long-secret-" (make-string 280 ?x)))))
        (let ((backend
               (e-backend-create
                :name "marker-schema-rejection"
                :stream
                (cl-function
                 (lambda (&key messages options on-item)
                   (ignore messages options)
                   (setq calls (1+ calls))
                   (if (= calls 1)
                       (progn
                         (funcall on-item
                                  (list :type 'tool-call :id "marker-call"
                                        :name "process_marker"
                                        :arguments arguments))
                         (funcall on-item '(:type done :reason tool-use)))
                     (funcall on-item '(:type assistant-message :content "done"))
                     (funcall on-item '(:type done :reason stop))))))))
          (e-loop-run-turn-batch
           :session-id "session-1" :turn-id "turn-1"
           :messages '((:role user :content "hi"))
           :backend backend :tools registry
           :options (list :tools (e-tools-definitions registry))
           :on-event (lambda (type payload)
                       (e-harness-activity-emit-turn-event
                        harness "session-1" "turn-1" type payload))
           :append-message (lambda (message) (push message appended))))
        (let ((serialized
               (prin1-to-string
                (list appended
                      (e-session-local-activity-events
                       (e-harness-sessions harness) "session-1")))))
          (when secret
            (should-not (string-match-p (regexp-quote secret) serialized)))
          (should-not (e-process-reporting-test--list harness)))))))

(ert-deftest e-process-reporting-test-note-and-extraction-free-text-are-redacted ()
  (e-process-reporting-test--with-store (store directory)
    (let* ((harness (e-process-reporting-test--harness store))
           (marker (e-process-reporting-test--call-action
                    harness :mark
                    '(:signal "failure"
                      :note "Bearer hidden-secret failed."))))
      (e-process-reporting-test--call-action
       harness :record-extraction
       (list :marker-ids (vector (plist-get marker :id))
             :session-evidence ["https://user:password@example.test/path"]
             :estimation-method "api_key=hidden-key estimate"))
      (let ((durable-records
             (prin1-to-string
              (e-session-storage-read-session-records store "session-1"))))
        (should (string-match-p "REDACTED" durable-records))
        (should-not (string-match-p
                     "hidden-secret\\|hidden-key\\|password@example"
                     durable-records))))))

(ert-deftest e-process-reporting-test-trigger-chain-preserves-nested-action ()
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'tool-started
       '(:id "run-1" :name "run_elisp_probe" :arguments nil))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'action-finished
       '(:action-call-id "action-1" :capability-id outer :action :run
         :parent-tool-call-id "run-1" :status ok))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'tool-finished
       '(:tool-call (:id "run-1" :name "run_elisp_probe")
         :result (:tool-call-id "run-1" :name "run_elisp_probe"
                  :status ok :content "done")))
      (let* ((marker (e-process-reporting-test--call-action
                      harness :mark
                      '(:signal "success" :note "Nested action mattered.")))
             (marker-id (plist-get marker :id))
             (loaded-store (e-session-persistent-store-create directory))
             (loaded-harness
              (e-process-reporting-test--harness loaded-store))
             (reloaded-marker
              (plist-get (e-process-reporting-test--read
                          loaded-harness marker-id)
                         :marker))
             (chain (plist-get reloaded-marker :trigger-chain)))
        (should (= (length chain) 2))
        (should (equal (plist-get (car chain) :call-id) "action-1"))
        (should (equal (plist-get (car chain) :parent-tool-call-id) "run-1"))
        (should (equal (plist-get (cadr chain) :call-id) "run-1"))
        (should (equal (plist-get (cadr chain) :event-type) "tool-finished"))))))

(ert-deftest e-process-reporting-test-trigger-chain-keeps-running-parent-tool ()
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'tool-started
       '(:id "run-1" :name "run_elisp_probe" :arguments nil))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'action-finished
       '(:action-call-id "action-1" :capability-id outer :action :run
         :parent-tool-call-id "run-1" :status ok))
      (let ((chain
             (plist-get
              (e-process-reporting-test--call-action
               harness :mark
               '(:signal "success" :note "Running parent mattered."))
              :trigger-chain)))
        (should (= (length chain) 2))
        (should (equal (plist-get (aref chain 0) :call-id) "action-1"))
        (should (equal (plist-get (aref chain 1) :call-id) "run-1"))
        (should (equal (plist-get (aref chain 1) :event-type) "tool-started"))))))

(ert-deftest e-process-reporting-test-trigger-chain-keeps-latest-independent-operation ()
  (dolist (history
           '(((action-finished
               (:action-call-id "old-action" :capability-id old :action :run
                :status ok))
              (tool-finished
               (:tool-call (:id "fresh-tool" :name "bash")
                :result (:tool-call-id "fresh-tool" :name "bash"
                         :status ok :content "done")))
              "fresh-tool")
             ((tool-finished
               (:tool-call (:id "old-tool" :name "bash")
                :result (:tool-call-id "old-tool" :name "bash"
                         :status ok :content "done")))
              (action-failed
               (:action-call-id "fresh-action" :capability-id fresh :action :run
                :status error))
              "fresh-action")))
    (e-process-reporting-test--with-store (store directory)
      (let ((harness (e-process-reporting-test--harness store)))
        (dolist (event (butlast history))
          (e-harness-activity-emit-turn-event
           harness "session-1" "turn-1" (car event) (cadr event)))
        (let* ((marker (e-process-reporting-test--call-action
                        harness :mark
                        '(:signal "success" :note "Latest operation mattered.")))
               (chain (plist-get marker :trigger-chain)))
          (should (= (length chain) 1))
          (should (equal (plist-get (aref chain 0) :call-id) (car (last history)))))))))

(ert-deftest e-process-reporting-test-completed-request-is-not-current ()
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'provider-request-started
       '(:provider-request-id "finished-request"))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'provider-request-finished
       '(:provider-request-id "finished-request" :status done))
      (let ((marker (e-process-reporting-test--call-action
                     harness :mark
                     '(:signal "success" :note "Runtime action."))))
        (should (eq (plist-get marker :provider-request-id) e-json-null))))))

(provide 'e-process-reporting-test)

;;; e-process-reporting-test.el ends here
