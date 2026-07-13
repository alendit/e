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

(ert-deftest e-process-reporting-test-request-shape-report-names-limit ()
  (e-process-reporting-test--with-store (store directory)
    (let ((harness (e-process-reporting-test--harness store)))
      (e-harness--emit-turn-event
       harness "session-1" "turn-1" 'provider-request-started
       '(:provider-request-id "request-1"
         :provider-request-ordinal 1
         :request-shape
         (:process-marker-surface (:tool-bytes 100 :guidance-bytes 40
                                  :call-bytes 20 :result-bytes 4))))
      (let ((report (e-process-reporting-test--call-action
                     harness :cost-report nil)))
        (should (equal (plist-get report :scope) "request-shape"))
        (should (= (plist-get report :passive-surface-bytes) 164))
        (should (equal (plist-get report :estimation-method) "utf-8-bytes/4"))
        (should-not (plist-get report :behavioral-estimate))))))

(provide 'e-process-reporting-test)

;;; e-process-reporting-test.el ends here
