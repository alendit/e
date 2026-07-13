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

(cl-defmacro e-process-reporting-test--with-store ((store directory) &body body)
  "Run BODY with isolated STORE in DIRECTORY."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-process-reporting-" t))
          (,store (e-process-reporting-store-create :directory ,directory)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,directory t))))

(defun e-process-reporting-test--harness (store)
  "Return a harness with process reporting backed by STORE."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability
     harness (e-process-reporting-capability-create store))
    (e-harness-create-session
     harness :id "session-1"
     :metadata '(:project-root "/tmp/project/"))
    harness))

(defun e-process-reporting-test--call-action
    (harness action arguments &optional context)
  "Call process reporting ACTION in HARNESS with ARGUMENTS."
  (e-actions-call
   'process-reporting action arguments
   (list :harness harness :session-id "session-1" :turn-id "turn-1"
         :context context)))

(ert-deftest e-process-reporting-test-lockf-timeout-is-an-integer ()
  (let ((e-process-reporting-lock-timeout 5.0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (and (equal program "lockf") "/usr/bin/lockf"))))
      (should
       (equal
        (e-process-reporting--lock-command "/tmp/process-reporting.lock")
        '("lockf" "-s" "-k" "-t" "5"
          "/tmp/process-reporting.lock"
          "sh" "-c" "printf 'acquired\\n'; cat >/dev/null"))))))

(ert-deftest e-process-reporting-test-live-lock-cannot-be-reclaimed ()
  (e-process-reporting-test--with-store (store directory)
    (let ((e-process-reporting-lock-timeout 0.05)
          (first (e-process-reporting--acquire-lock store)))
      (unwind-protect
          (progn
            (should-error (e-process-reporting--acquire-lock store)
                          :type 'file-error)
            (should (process-live-p (car first))))
        (e-process-reporting--release-lock first)))
    (let ((after-release (e-process-reporting--acquire-lock store)))
      (unwind-protect
          (should (process-live-p (car after-release)))
        (e-process-reporting--release-lock after-release)))))

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
                     "Save one process observation."))
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
      (should (equal (plist-get result :content) "ok"))
      (should-not (string-match-p "short method" (plist-get result :content))))))

(ert-deftest e-process-reporting-test-capture-attaches-redacted-evidence-and-links ()
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness--emit-turn-event
       harness "session-1" "turn-1" 'tool-started
       '(:id "failed-call" :name "bash"
         :arguments (:authorization "Bearer top-secret" :command "false")))
      (e-harness--emit-turn-event
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
        (should (equal (plist-get record :session-uri)
                       "session://e/sessions/session-1/"))
        (should (equal (plist-get record :tool-call-id) "marker-call"))
        (should (equal (plist-get trigger :call-id) "failed-call"))
        (should (string-match-p "REDACTED" serialized))
        (should-not (string-match-p "top-secret\\|private-value" serialized))))))

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
      (let* ((loaded (e-process-reporting-store-create :directory directory))
             (listed (e-process-reporting-list loaded))
             (read (e-process-reporting-read loaded marker-id)))
        (should (= (length listed) 1))
        (should (equal (plist-get (car listed) :status) "routed"))
        (should (equal (plist-get (plist-get read :marker) :note)
                       "A direct operation was absent."))
        (should (= (length (plist-get read :triage)) 1))
        (should (= (length (e-process-reporting-store-events loaded)) 2))))))

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
        (should (= (length (e-process-reporting-list store)) 2))))))

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
      (let ((read (e-process-reporting-read store marker-id)))
        (should (= (length (plist-get read :extractions)) 1))
        (should-not (plist-member (plist-get read :marker) :token-usage))))))

(ert-deftest e-process-reporting-test-request-shape-report-preserves-paired-measures ()
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness--emit-turn-event
       harness "session-1" "turn-1" 'provider-request-started
       '(:provider-request-id "request-1"
         :provider-request-ordinal 1
         :caused-by-tool-call-id "marker-call"
         :caused-by-tool-name "process_marker"
         :caused-by-tool-calls [(:id "marker-call" :name "process_marker")]
         :request-shape
         (:serialization "backend-neutral-elisp-v1"
          :actual-shape (:sha256 "a" :bytes 140)
          :without-passive-shape (:sha256 "b" :bytes 100)
          :without-active-shape (:sha256 "c" :bytes 120)
          :paired-shape (:sha256 "d" :bytes 80))))
      (let* ((report (e-process-reporting-test--call-action
                      harness :cost-report nil))
             (entry (car (plist-get report :requests))))
        (should (equal (plist-get report :scope)
                       "request-shape-counterfactual"))
        (should (= (plist-get entry :direct-context-delta-bytes) 60))
        (should (= (plist-get entry :passive-surface-bytes) 40))
        (should (= (plist-get entry :active-marker-bytes) 20))
        (should (plist-get entry :marker-follow-up))
        (should (equal (plist-get report :estimation-method)
                       "backend-neutral-serialized-utf-8-bytes"))
        (should-not (plist-get report :provider-tokenizer-used))
        (should-not (plist-get report :behavioral-estimate))))))

(ert-deftest e-process-reporting-test-two-store-writers-refresh-and-recover-tail ()
  (e-process-reporting-test--with-store (store-a directory)
    (let* ((store-b (e-process-reporting-store-create :directory directory))
           (harness-a (e-process-reporting-test--harness store-a))
           (harness-b (e-process-reporting-test--harness store-b))
           (marker-a
            (e-process-reporting-test--call-action
             harness-a :mark '(:signal "success" :note "First writer.")))
           (marker-b
            (e-process-reporting-test--call-action
             harness-b :mark '(:signal "failure" :note "Second writer."))))
      (should (= (length (e-process-reporting-list store-a)) 2))
      (should (plist-get (e-process-reporting-read
                          store-b (plist-get marker-a :id))
                         :marker))
      (e-actions-call
       'process-reporting :triage
       (list :marker-id (plist-get marker-b :id)
             :outcome "understood" :status "closed"
             :decision-note "Peer marker seen.")
       (list :harness harness-a :session-id "session-1" :turn-id "turn-1"))
      (with-temp-buffer
        (insert "{\"type\":\"marker\"")
        (write-region (point-min) (point-max)
                      (e-process-reporting--file store-a) t 'silent))
      (should (= (length (e-process-reporting-list store-b)) 2))
      (should (string-suffix-p
               "\n"
               (with-temp-buffer
                 (insert-file-contents (e-process-reporting--file store-a))
                 (buffer-string)))))))

(ert-deftest e-process-reporting-test-terminal-suppression-is-atomic-across-stores ()
  (e-process-reporting-test--with-store (store-a directory)
    (let* ((store-b (e-process-reporting-store-create :directory directory))
           (harness-a (e-process-reporting-test--harness store-a))
           (harness-b (e-process-reporting-test--harness store-b))
           (arguments '(:signal "repetition" :note "Shared evidence."))
           (marker (e-process-reporting-test--call-action
                    harness-a :mark arguments)))
      (e-process-reporting-test--call-action
       harness-a :triage
       (list :marker-id (plist-get marker :id) :outcome "duplicate"
             :status "rejected" :decision-note "Terminal."))
      (should (plist-get
               (e-process-reporting-test--call-action
                harness-b :mark arguments)
               :suppressed))
      (should (= (length (e-process-reporting-list store-a)) 1)))))

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
      (should-not (e-process-reporting-list store)))))

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
                   (e-harness--emit-turn-event
                    harness "session-1" "turn-1" type payload))
       :append-message (lambda (message) (push message appended)))
      (let ((serialized
             (prin1-to-string
              (list appended
                    (e-session-activity-events
                     (e-harness-sessions harness) "session-1")))))
        (should-not (string-match-p "must-not-retain\\|:impact" serialized))
        (should-not (e-process-reporting-list store))))))

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
      (let ((disk (with-temp-buffer
                    (insert-file-contents
                     (e-process-reporting--file store))
                    (buffer-string))))
        (should (string-match-p "REDACTED" disk))
        (should-not (string-match-p
                     "hidden-secret\\|hidden-key\\|password@example" disk))))))

(ert-deftest e-process-reporting-test-trigger-chain-preserves-nested-action ()
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness--emit-turn-event
       harness "session-1" "turn-1" 'tool-started
       '(:id "run-1" :name "run_elisp_probe" :arguments nil))
      (e-harness--emit-turn-event
       harness "session-1" "turn-1" 'action-finished
       '(:action-call-id "action-1" :capability-id outer :action :run
         :parent-tool-call-id "run-1" :status ok))
      (e-harness--emit-turn-event
       harness "session-1" "turn-1" 'tool-finished
       '(:tool-call (:id "run-1" :name "run_elisp_probe")
         :result (:tool-call-id "run-1" :name "run_elisp_probe"
                  :status ok :content "done")))
      (let* ((marker (e-process-reporting-test--call-action
                      harness :mark
                      '(:signal "success" :note "Nested action mattered.")))
             (marker-id (plist-get marker :id))
             (loaded (e-process-reporting-store-create :directory directory))
             (reloaded-marker
              (plist-get (e-process-reporting-read loaded marker-id) :marker))
             (chain (plist-get reloaded-marker :trigger-chain)))
        (should (= (length chain) 2))
        (should (equal (plist-get (car chain) :call-id) "action-1"))
        (should (equal (plist-get (car chain) :parent-tool-call-id) "run-1"))
        (should (equal (plist-get (cadr chain) :call-id) "run-1"))
        (should (equal (plist-get (cadr chain) :event-type) "tool-finished"))))))

(ert-deftest e-process-reporting-test-completed-request-is-not-current ()
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness--emit-turn-event
       harness "session-1" "turn-1" 'provider-request-started
       '(:provider-request-id "finished-request"))
      (e-harness--emit-turn-event
       harness "session-1" "turn-1" 'provider-request-finished
       '(:provider-request-id "finished-request" :status done))
      (let ((marker (e-process-reporting-test--call-action
                     harness :mark
                     '(:signal "success" :note "Runtime action."))))
        (should-not (plist-get marker :provider-request-id))))))

(provide 'e-process-reporting-test)

;;; e-process-reporting-test.el ends here
