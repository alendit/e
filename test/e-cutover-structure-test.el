;;; e-cutover-structure-test.el --- Board-only product boundary tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-board-runtime)
(require 'e-harness)
(require 'subr-x)

(defun e-cutover-structure-test--source (file)
  "Return repository-relative Lisp FILE contents."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defconst e-cutover-structure-test--retirement-manifest
  '((:symbol e-harness-prompt-batch :policy absent)
    (:symbol e-harness-prompt-async :policy absent)
    (:symbol e-harness-follow-up-batch :policy absent)
    (:symbol e-harness-request-follow-up :policy absent)
    (:symbol e-harness-queue-prompt :policy absent)
    (:symbol e-harness-steer-active-turn :policy absent)
    (:symbol e-harness-abort :policy absent)
    (:symbol e-harness-subscribe :policy absent)
    (:symbol e-harness-unsubscribe :policy absent)
    (:symbol e-harness--install-activity-sink :policy private
             :owners ("lisp/core/e-board-runtime.el" "lisp/core/e-harness.el"))
    (:symbol e-harness--remove-activity-sink :policy private
             :owners ("lisp/core/e-board-runtime.el" "lisp/core/e-harness.el"))
    (:symbol e-harness--request-attached-follow-up :policy private
             :owners ("lisp/core/e-harness.el"
                      "lisp/core/e-board-runtime.el"
                      "lisp/layers/harness/e-bayesian-reasoning.el"))
    (:symbol e-harness--queue-attached-prompt :policy private
             :owners ("lisp/core/e-harness.el"))
    (:symbol e-harness--steer-attached-turn :policy private
             :owners ("lisp/core/e-board-runtime.el" "lisp/core/e-harness.el"))
    (:symbol e-harness--prompt-attached-batch :policy private
             :owners ("lisp/core/e-harness.el"))
    (:symbol e-harness--prompt-attached-async :policy private
             :owners ("lisp/core/e-board-runtime.el" "lisp/core/e-harness.el"))
    (:symbol e-harness--follow-up-attached-batch :policy private
             :owners ("lisp/core/e-harness.el"))
    (:symbol e-harness--abort-attached :policy private
             :owners ("lisp/core/e-board-runtime.el" "lisp/core/e-harness.el")))
  "Exact hard-cutover policy for retired roots and private survivor ports.")

(defconst e-cutover-structure-test--forbidden-consumer-symbols
  '("e-harness-create-session"
    "e-harness-messages"
    "e-harness-session-activity-events"
    "e-harness-state"
    "e-harness-active-turns")
  "Private live-session symbols forbidden in public shells and producers.")

(defun e-cutover-structure-test--manifest-entries (policy)
  "Return retirement manifest entries having POLICY."
  (cl-remove-if-not (lambda (entry) (eq policy (plist-get entry :policy)))
                    e-cutover-structure-test--retirement-manifest))

(ert-deftest e-cutover-structure-test-retired-live-roots-are-absent ()
  "The compatibility surface cannot start or control standalone work."
  (dolist (entry (e-cutover-structure-test--manifest-entries 'absent))
    (should-not (fboundp (plist-get entry :symbol)))))

(ert-deftest e-cutover-structure-test-private-survivors-have-exact-owners ()
  "Every private survivor occurs only in its declared implementation owners."
  (let ((files (directory-files-recursively "lisp" "\\.el\\'")))
    (dolist (entry (e-cutover-structure-test--manifest-entries 'private))
      (let* ((symbol (symbol-name (plist-get entry :symbol)))
             (owners (sort (copy-sequence (plist-get entry :owners)) #'string<))
             hits)
        (dolist (file files)
          (when (string-match-p
                 (concat "\\_<" (regexp-quote symbol) "\\_>")
                 (e-cutover-structure-test--source file))
            (push file hits)))
        (should (equal (sort hits #'string<) owners))))))

(ert-deftest e-cutover-structure-test-private-port-requires-board-token ()
  "The only harness start port fails closed without a current attachment."
  (let ((harness (e-harness-create :enabled-layer-ids nil)))
    (e-harness-create-session harness :id "standalone-denied")
    (should-error
     (e-harness--prompt-attached-async
      harness "standalone-denied" "must fail" :attachment-token 'forged)
     :type 'e-harness-board-attachment-required)
    (should-not (plist-get (e-harness-state harness "standalone-denied")
                           :active-turn))))

(defun e-cutover-structure-test--symbol-hits (files)
  "Return forbidden symbol hits as (FILE SYMBOL) pairs across FILES."
  (let (hits)
    (dolist (file files)
      (let ((source (e-cutover-structure-test--source file)))
        (dolist (symbol e-cutover-structure-test--forbidden-consumer-symbols)
          (when (string-match-p
                 (concat "\\_<" (regexp-quote symbol) "\\_>") source)
            (push (list file symbol) hits)))))
    (nreverse hits)))

(ert-deftest e-cutover-structure-test-public-shells-use-chat-service-boundary ()
  "Public shells contain no direct live harness interaction or presentation feed."
  (let ((files (directory-files-recursively "lisp/shells" "\\.el\\'")))
    (should-not (e-cutover-structure-test--symbol-hits files))))

(ert-deftest e-cutover-structure-test-bundled-producers-have-no-live-session-path ()
  "Bundled producer implementations cannot create, prompt, or subscribe directly."
  (let ((files '("lisp/core/e-task-queue.el"
                 "lisp/layers/agents/e-cron-actions.el"
                 "lisp/layers/agents/e-subagent-runner.el"
                 "lisp/layers/annotations/e-annotation-answer.el"
                 "lisp/shells/e-background-session.el")))
    (should-not (e-cutover-structure-test--symbol-hits files))))

(ert-deftest e-cutover-structure-test-chat-capability-ingress-is-board-backed ()
  "Chat semantic actions contain no direct harness prompt/queue/steer/abort call."
  (should-not
   (e-cutover-structure-test--symbol-hits
    '("lisp/layers/chat/e-chat-session.el"))))

(provide 'e-cutover-structure-test)

;;; e-cutover-structure-test.el ends here
