;;; e-session-resources-test.el --- Tests for session:// resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for read-only persisted session resources.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-resources)
(require 'e-session-resources)
(require 'e-tools)

(defmacro e-session-resources-test--with-empty-config (&rest body)
  "Run BODY with no configured non-e session engines."
  (declare (indent 0) (debug t))
  `(let ((e-session-resource-engines nil))
     ,@body))

(defun e-session-resources-test--harness ()
  "Return a harness with only session resources active."
  (e-harness-create
   :backend (e-backend-fake-create :items nil)
   :intrinsic-capabilities (list (e-session-resources-capability-create))))

(defun e-session-resources-test--resources (harness)
  "Return resource registry for HARNESS."
  (e-harness-resources harness "session-1" "turn-1"))

(ert-deftest e-session-resources-test-glob-discovers-engine-sessions-and-projections ()
  "session:// glob follows the engine, sessions, projection workflow."
  (e-session-resources-test--with-empty-config
    (let* ((harness (e-session-resources-test--harness))
           (resources (e-session-resources-test--resources harness)))
      (e-harness-create-session
       harness
       :id "session-1"
       :metadata '(:project-root "/project" :harness-instance-id "harness-1"))
      (e-session-rename (e-harness-sessions harness) "session-1" "Named Session")
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:role user :content "hello"))
      (let* ((engines (e-resources-glob resources "session://" nil 5))
             (engine-items (append (plist-get engines :resources) nil)))
        (should (equal (mapcar (lambda (item) (plist-get item :uri))
                               engine-items)
                       '("session://e/sessions/")))
        (should (equal (plist-get (car engine-items) :kind) 'engine)))
      (let* ((sessions (e-resources-glob resources "session://e/sessions/" nil 5))
             (session-items (append (plist-get sessions :resources) nil))
             (item (car session-items)))
        (should (= (length session-items) 1))
        (should (equal (plist-get item :uri) "session://e/sessions/session-1/"))
        (should (equal (plist-get item :name) "Named Session"))
        (should (equal (plist-get (plist-get item :metadata) :engine-id) "e"))
        (should (equal (plist-get (plist-get item :metadata) :project-root)
                       "/project/")))
      (let* ((projections (e-resources-glob
                           resources
                           "session://e/sessions/session-1/"
                           nil
                           5))
             (uris (mapcar (lambda (item) (plist-get item :uri))
                           (append (plist-get projections :resources) nil))))
        (should (member "session://e/sessions/session-1/summary" uris))
        (should (member "session://e/sessions/session-1/messages" uris))))))

(ert-deftest e-session-resources-test-read-summary-and-messages-with-range ()
  "session:// read returns stable text projections and line ranges."
  (e-session-resources-test--with-empty-config
    (let* ((harness (e-session-resources-test--harness))
           (resources (e-session-resources-test--resources harness)))
      (e-harness-create-session harness :id "session-1")
      (e-session-rename (e-harness-sessions harness) "session-1" "Range Test")
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:role user :content "needle one"))
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:role assistant :content "answer two"))
      (let ((summary (e-resources-read
                      resources
                      "session://e/sessions/session-1/summary"
                      nil)))
        (should (string-match-p "# Session session-1" summary))
        (should (string-match-p "Readable subresources:" summary))
        (should (string-match-p "session://e/sessions/session-1/messages"
                                summary))
        (should-not (string-match-p "needle one" summary)))
      (let ((messages (e-resources-read
                       resources
                       "session://e/sessions/session-1/messages"
                       nil)))
        (should (string-match-p "needle one" messages))
        (should (string-match-p "answer two" messages)))
      (should (equal (e-resources-read
                      resources
                      "session://e/sessions/session-1/messages"
                      '(:unit "line" :start 1 :end 1))
                     "# Session session-1 messages\n")))))

(ert-deftest e-session-resources-test-search-defaults-to-messages-and-can-narrow-projections ()
  "Session-root search uses messages by default and glob can select activity."
  (e-session-resources-test--with-empty-config
    (let* ((harness (e-session-resources-test--harness))
           (resources (e-session-resources-test--resources harness)))
      (e-harness-create-session harness :id "session-1")
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:role user :content "message needle"))
      (e-session-append-activity-event
       (e-harness-sessions harness)
       "session-1"
       "turn-1"
       'tool-finished
       '(:text "activity needle"))
      (let* ((default-search (e-resources-search
                              resources
                              "session://e/sessions/session-1/"
                              "activity needle"
                              '(:limit 5)))
             (default-matches (append (plist-get default-search :matches) nil)))
        (should-not default-matches))
      (let* ((message-search (e-resources-search
                              resources
                              "session://e/sessions/session-1/"
                              "message needle"
                              '(:limit 5)))
             (match (aref (plist-get message-search :matches) 0)))
        (should (equal (plist-get match :uri)
                       "session://e/sessions/session-1/messages")))
      (let* ((activity-search (e-resources-search
                               resources
                               "session://e/sessions/session-1/activity"
                               "activity needle"
                               '(:limit 5)))
             (match (aref (plist-get activity-search :matches) 0)))
        (should (equal (plist-get match :uri)
                       "session://e/sessions/session-1/activity")))
      (let* ((glob-search (e-resources-search
                           resources
                           "session://e/sessions/session-1/"
                           "activity needle"
                           '(:glob "activity" :limit 5)))
             (match (aref (plist-get glob-search :matches) 0)))
        (should (equal (plist-get match :uri)
                       "session://e/sessions/session-1/activity"))))))

(ert-deftest e-session-resources-test-errors-and-read-only-contract ()
  "session:// reports invalid reads and remains read-only."
  (e-session-resources-test--with-empty-config
    (let* ((harness (e-session-resources-test--harness))
           (resources (e-session-resources-test--resources harness)))
      (e-harness-create-session harness :id "session-1")
      (should-error (e-resources-read resources "session://" nil)
                    :type 'e-session-resources-invalid-uri)
      (should-error (e-resources-read
                     resources
                     "session://missing/sessions/session-1/messages"
                     nil)
                    :type 'e-session-resources-unknown-engine)
      (should-error (e-resources-read
                     resources
                     "session://e/sessions/missing/messages"
                     nil)
                    :type 'e-session-resources-unknown-session)
      (should-error (e-resources-read
                     resources
                     "session://e/sessions/session-1/raw"
                     nil)
                    :type 'e-session-resources-unsupported-projection)
      (should-error (e-resources-write
                     resources
                     "session://e/sessions/session-1/messages"
                     "new")
                    :type 'e-resources-unsupported-operation)
      (should-error (e-resources-edit
                     resources
                     "session://e/sessions/session-1/messages"
                     '((:oldText "a" :newText "b")))
                    :type 'e-resources-unsupported-operation))))

(ert-deftest e-session-resources-test-configured-engines-are-opt-in ()
  "Configured engines appear only through e-session-resource-engines."
  (let* ((harness (e-session-resources-test--harness))
         (resources (e-session-resources-test--resources harness))
         (adapter (list :list-sessions
                        (lambda (_engine)
                          '((:id "remote-1"
                             :title "Remote Session"
                             :created-at "2026-01-01T00:00:00Z")))
                        :projections
                        (lambda (_engine _session-id)
                          '("summary" "messages"))
                        :render-summary
                        (lambda (_engine session-id)
                          (format "Summary for %s" session-id))
                        :render-messages
                        (lambda (_engine _session-id)
                          "remote needle"))))
    (let ((e-session-resource-engines nil))
      (should-error (e-resources-glob resources "session://remote/sessions/" nil 5)
                    :type 'e-session-resources-unknown-engine))
    (let ((e-session-resource-engines
           (list (list :id "remote"
                       :display-name "Remote"
                       :adapter adapter))))
      (let* ((engines (append (plist-get (e-resources-glob
                                          resources "session://" nil 5)
                                         :resources)
                              nil))
             (uris (mapcar (lambda (item) (plist-get item :uri)) engines)))
        (should (member "session://e/sessions/" uris))
        (should (member "session://remote/sessions/" uris)))
      (let* ((sessions (append (plist-get (e-resources-glob
                                           resources
                                           "session://remote/sessions/"
                                           nil
                                           5)
                                          :resources)
                               nil))
             (item (car sessions)))
        (should (equal (plist-get item :uri)
                       "session://remote/sessions/remote-1/"))
        (should (equal (plist-get item :name) "Remote Session")))
      (should (equal (e-resources-read
                      resources
                      "session://remote/sessions/remote-1/messages"
                      nil)
                     "remote needle")))))

(ert-deftest e-session-resources-test-tool-description-documents-discovery-workflow ()
  "Generated resource tool descriptions tell agents to glob before reading."
  (e-session-resources-test--with-empty-config
    (let* ((harness (e-session-resources-test--harness))
           (tools (e-harness-tools harness "session-1" "turn-1"))
           (definitions (e-tools-definitions tools))
           (read-tool (seq-find (lambda (tool)
                                  (equal (plist-get tool :name) "read"))
                                definitions))
           (glob-tool (seq-find (lambda (tool)
                                  (equal (plist-get tool :name) "glob"))
                                definitions)))
      (should (string-match-p "glob session://"
                              (plist-get read-tool :description)))
      (should (string-match-p "session://<engine-id>/sessions/"
                              (plist-get glob-tool :description))))))

(provide 'e-session-resources-test)

;;; e-session-resources-test.el ends here
