;;; e-session-aggregate-test.el --- Direct session aggregate contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests exercise aggregate-only semantic invariants.  Durable replay,
;; restart, and physical projection behavior live in the facade composition
;; suite; the dedicated contract file covers fresh owner loading.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-session-aggregate)

(ert-deftest e-session-aggregate-test-context-current-path-rejects-broken-links ()
  "Context ownership paths reject missing heads, parents, and cycles."
  (let* ((store (e-session-store-create))
         (session-id "context-path-integrity")
         (session (e-session-aggregate-create store :id session-id))
         (root-id (plist-get session :root-event-id))
         (message
          (e-session-aggregate-append-message
           store session-id
           '(:id "context-path-message" :role assistant :content "path")))
         (message-id (plist-get message :id))
         (root (e-session-aggregate-entry-by-id store session-id root-id)))
    (should-error
     (e-session-aggregate--context-current-path store session-id "missing-head")
     :type 'e-session-error)
    (plist-put message :parent-id "missing-parent")
    (should-error
     (e-session-aggregate--context-current-path store session-id message-id)
     :type 'e-session-error)
    (plist-put message :parent-id root-id)
    (plist-put root :parent-id message-id)
    (should-error
     (e-session-aggregate--context-current-path store session-id message-id)
     :type 'e-session-error)))
(ert-deftest e-session-aggregate-test-message-appends-maintain-derived-fields-incrementally ()
  "Message appends update metadata without full derived-field refreshes."
  (let ((store (e-session-store-create))
        (refresh-count 0)
        (original-refresh (symbol-function 'e-session-aggregate--refresh-derived-fields)))
    (cl-letf (((symbol-function 'e-session-aggregate--refresh-derived-fields)
               (lambda (refresh-store refresh-session)
                 (setq refresh-count (1+ refresh-count))
                 (funcall original-refresh refresh-store refresh-session))))
      (e-session-aggregate-create store :id "session-1")
      (setq refresh-count 0)
      (e-session-aggregate-append-message
       store "session-1"
       '(:id "msg-1" :role user :content "first"))
      (e-session-aggregate-append-message
       store "session-1"
       '(:id "msg-2" :role assistant :content "second"))
      (let ((session (e-session-aggregate-get store "session-1")))
        (should (= refresh-count 0))
        (should (= (plist-get session :message-count) 2))
        (should (equal (plist-get session :summary) "first"))
        (should (equal (plist-get session :last-message-at)
                       (plist-get (cadr (plist-get session :messages))
                                  :created-at)))))))


(ert-deftest e-session-aggregate-test-append-message-stamps-created-at ()
  "Appended messages carry their creation timestamp."
  (let ((store (e-session-store-create)))
    (cl-letf (((symbol-function 'e-session-aggregate--timestamp)
               (lambda (&optional _time) "2026-05-21T10:00:00Z")))
      (e-session-aggregate-create store :id "session-1")
      (e-session-aggregate-append-message
       store "session-1" '(:role user :content "hello"))
      (should (equal (plist-get (car (e-session-aggregate-messages store "session-1"))
                                :created-at)
                     "2026-05-21T10:00:00Z")))))


(ert-deftest e-session-aggregate-test-current-path-uses-entry-index ()
  "Current-path traversal avoids repeated linear entry searches."
  (let ((store (e-session-store-create)))
    (e-session-aggregate-create store :id "session-1")
    (dotimes (index 25)
      (e-session-aggregate-append-message
       store
       "session-1"
       (list :role 'user :content (format "message-%d" index))))
    (let ((calls 0)
          (index (e-session-aggregate--entry-index store "session-1")))
      (cl-letf (((symbol-function 'e-session-aggregate--entries)
                 (lambda (&rest _args)
                   (setq calls (1+ calls))
                   nil)))
        (should (= (length (e-session-aggregate-current-path store "session-1")) 26))
        (should (= calls 0))
        (clrhash index)
        (should-not (e-session-aggregate-current-path store "session-1"))
        (should (> calls 0))))))


(provide 'e-session-aggregate-test)

;;; e-session-aggregate-test.el ends here
