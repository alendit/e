;;; e-session-aggregate-contract-test.el --- Direct aggregate owner contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This suite intentionally loads the aggregate without the public application
;; service.  It proves that semantic state, identity, and replay application
;; remain usable when the JSONL adapter and catalog are absent.

;;; Code:

(require 'ert)
(require 'e-session-aggregate)

(defun e-session-aggregate-contract--source-requires-no-siblings-p ()
  "Return non-nil when aggregate source has no upward owner dependencies.

Fresh-load behavior is exercised by the batch owner-load gate; this source
assertion remains valid when the composed test files share one Emacs process."
  (let ((source-file (or (locate-library "e-session-aggregate")
                         (expand-file-name "lisp/core/e-session-aggregate.el"
                                           default-directory))))
    (with-temp-buffer
      (insert-file-contents source-file)
      (not (re-search-forward
            "(require ['\"]e-session-\\(storage\\|catalog\\|session\\)"
            nil t)))))

(ert-deftest e-session-aggregate-contract-loads-without-facade-or-storage ()
  "The aggregate owner does not load the application service or adapter."
  (should (e-session-aggregate-contract--source-requires-no-siblings-p)))

(ert-deftest e-session-aggregate-contract-owns-semantic-session-state ()
  "Creation, identity, and append semantics need no persistence owner."
  (let* ((store (e-session-store-create))
         (session (e-session-aggregate-create store :id "aggregate-session"))
         (entry (e-session-aggregate-append-message
                 store "aggregate-session"
                 '(:id "message-1" :role user :content "hello"))))
    (should (equal (plist-get session :id) "aggregate-session"))
    (should (equal (plist-get entry :id) "message-1"))
    (should (= (plist-get (e-session-aggregate-get store "aggregate-session")
                          :message-count)
               1))
    (should (equal (plist-get (car (e-session-aggregate-messages
                                   store "aggregate-session"))
                              :content)
                   "hello"))))

(ert-deftest e-session-aggregate-contract-applies-decoded-semantic-replay ()
  "Replay application consumes semantic data and mutates only the aggregate."
  (let ((store (e-session-store-create)))
    (e-session-aggregate-apply-record
     store
     '(:type "session" :session-id "replay-session"
       :id "replay-root" :created-at "2026-08-30T00:00:00Z"
       :updated-at "2026-08-30T00:00:00Z" :metadata nil))
    (e-session-aggregate-apply-record
     store
     '(:type "message" :session-id "replay-session"
       :id "replay-message" :parent-id "replay-root"
       :timestamp "2026-08-30T00:00:01Z"
       :message (:id "replay-message" :role user :content "replayed")))
    (should (equal (plist-get
                    (car (e-session-aggregate-messages store "replay-session"))
                    :content)
                   "replayed"))
    (should (equal (plist-get
                    (e-session-aggregate-entry-by-id
                     store "replay-session" "replay-message")
                    :parent-id)
                   "replay-root"))))

(ert-deftest e-session-aggregate-contract-current-path-is-indexed ()
  "The aggregate's navigation projection is independently usable."
  (let ((store (e-session-store-create)))
    (e-session-aggregate-create store :id "path-session")
    (dotimes (index 4)
      (e-session-aggregate-append-message
       store "path-session"
       (list :id (format "message-%d" index)
             :role 'user :content (format "value-%d" index))))
    (should (= (length (e-session-aggregate-current-path store "path-session"))
               5))
    (should (equal
             (plist-get (car (last (e-session-aggregate-current-path
                                     store "path-session")))
                        :id)
             "message-3"))))

(ert-deftest e-session-aggregate-contract-replay-reset-is-semantic ()
  "Replay reset and index installation stay inside the aggregate owner."
  (let ((store (e-session-store-create)))
    (e-session-aggregate-apply-record
     store
     '(:type "session" :session-id "indexed-session" :id "root"
       :created-at "2026-08-30T00:00:00Z" :metadata nil))
    (e-session-aggregate-reset store)
    (should-not (e-session-aggregate-session-present-p store "indexed-session"))
    (e-session-aggregate-install-index-session
     store '(:id "indexed-session" :updated-seq 7 :loaded nil))
    (should (e-session-aggregate-session-present-p store "indexed-session"))
    (should (= (plist-get (car (e-session-aggregate-session-values store))
                          :updated-seq)
               7))))

(ert-deftest e-session-aggregate-contract-stage-is-isolated-until-published ()
  "A staged mutation cannot alter the live session before publication."
  (let ((store (e-session-store-create))
        (table (make-hash-table :test 'equal)))
    (puthash "key" ["old"] table)
    (e-session-aggregate-create store :id "staged")
    (e-session-aggregate-append-message
     store "staged" '(:id "old" :role user :content "old"))
    (e-session-aggregate-append-activity-event
     store "staged" "turn" 'exact (list :map table))
    (let ((stage
           (e-session-aggregate-stage-session-mutation store "staged")))
      (puthash
       "key" ["staged"]
       (plist-get
        (plist-get (car (e-session-aggregate-activity-events stage "staged"))
                   :payload)
        :map))
      (should (equal
               (gethash
                "key"
                (plist-get
                 (plist-get
                  (car (e-session-aggregate-activity-events store "staged"))
                  :payload)
                 :map))
               ["old"]))
      (e-session-aggregate-append-message
       stage "staged" '(:id "new" :role assistant :content "new"))
      (should (= (length (e-session-aggregate-messages store "staged")) 1))
      (e-session-aggregate-publish-staged-session store stage "staged")
      (should (= (length (e-session-aggregate-messages store "staged")) 2)))))

(provide 'e-session-aggregate-contract-test)

;;; e-session-aggregate-contract-test.el ends here
