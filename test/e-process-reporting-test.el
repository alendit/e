;;; e-process-reporting-test.el --- Tests for process reporting -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-actions)
(require 'e-backend)
(require 'e-harness)
(require 'e-process-reporting)
(require 'e-session)
(require 'e-tools)
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
        (e-session-get store "session-1")
      (e-session-missing
       (e-harness-create-session
        harness :id "session-1"
        :metadata '(:project-root "/tmp/project/"))))
    harness))

(defun e-process-reporting-test--context (harness)
  "Return the standard process-reporting action context for HARNESS."
  (list :harness harness :session-id "session-1" :turn-id "turn-1"))

(defun e-process-reporting-test--list (harness &optional arguments)
  "List current-session process markers in HARNESS."
  (e-process-reporting-list
   (e-process-reporting-test--context harness) arguments))

(defun e-process-reporting-test--read (harness marker-id)
  "Read current-session MARKER-ID in HARNESS."
  (e-process-reporting-read
   (e-process-reporting-test--context harness) marker-id))

(defun e-process-reporting-test--call-action
    (harness action arguments &optional context)
  "Call process reporting ACTION in HARNESS with ARGUMENTS."
  (e-actions-call
   'process-reporting action arguments
   (list :harness harness :session-id "session-1" :turn-id "turn-1"
         :context context)))

(ert-deftest e-process-reporting-test-tool-is-tiny-and-returns-minimal-ack ()
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
                     ["signal" "note" "stated_purpose"]))
      (setq result
            (e-tools--execute-batch-with-context
             registry
             '(:id "marker-call" :name "process_marker"
               :arguments (:signal "success"
                           :note "A short method worked."))
             (list :harness harness :session-id "session-1"
                   :turn-id "turn-1")))
      (should (equal (plist-get result :content) "ok"))
      (should-not (string-match-p "short method" (plist-get result :content))))))

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
                       (plist-get (car (plist-get record :trigger-chain))
                                  :activity-event-id)))
        (should-not (plist-get trigger :arguments-preview))
        (should-not (plist-get trigger :result-preview))
        (should-not (plist-get trigger :error-preview))
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
        (should (= (length (e-session-process-reports
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
             (entry (car (plist-get report :requests))))
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
        (should-not (plist-get report :provider-tokenizer-used))
        (should-not (plist-get report :behavioral-estimate))))))

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
             (entry (car (plist-get report :requests))))
        (should (equal (plist-get report :measurement-status)
                       "not-recorded"))
        (should (= (plist-get report :measured-request-count) 0))
        (should-not (plist-get report :direct-context-delta-bytes))
        (should (equal (plist-get entry :measurement-status)
                       "not-recorded"))
        (should-not (plist-get entry :actual-bytes))))))

(ert-deftest e-process-reporting-test-reports-use-session-store-not-transcript ()
  (e-process-reporting-test--with-store (store directory)
    (let* ((harness (e-process-reporting-test--harness store))
           (marker (e-process-reporting-test--call-action
                    harness :mark '(:signal "success" :note "Stored once.")))
           (reports (e-session-process-reports store "session-1"))
           (durable-records
            (prin1-to-string
             (e-session-storage-read-session-records store "session-1"))))
      (should (= (length reports) 1))
      (should (equal (plist-get (car reports) :id) (plist-get marker :id)))
      (should-not (e-session-messages store "session-1"))
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
        (should (= (length (e-session-process-reports store "session-1")) 2))
        (should (= (length (e-session-process-reports store "session-2")) 1))))))

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
                    (e-session-activity-events
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
                      (e-session-activity-events
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
        (should (equal (plist-get (car chain) :call-id) "action-1"))
        (should (equal (plist-get (cadr chain) :call-id) "run-1"))
        (should (equal (plist-get (cadr chain) :event-type) "tool-started"))))))

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
          (should (equal (plist-get (car chain) :call-id) (car (last history)))))))))

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
        (should-not (plist-get marker :provider-request-id))))))

(provide 'e-process-reporting-test)

;;; e-process-reporting-test.el ends here
