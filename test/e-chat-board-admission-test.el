;;; e-chat-board-admission-test.el --- Board-first owner admission tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-backend)
(require 'e-board-sqlite-service)
(require 'e-chat-service)
(require 'e-harness)
(require 'e-session-storage-sqlite)
(require 'e-work)

(defun e-chat-board-admission-test--await (work)
  "Observe WORK at this explicit test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 5.0)))

(defun e-chat-board-admission-test--owner-admission
    (session-id board-id participant-id)
  "Return one detached owner admission fixture."
  (let* ((policy (list :participant-id participant-id
                       :pickup-selector '(:tags (main))
                       :observer-selector '(:tags (main))
                       :default-tags '(main) :default-to nil))
         (session
          (e-board-sqlite-service-session-admission
           :id session-id :metadata (list :name session-id)
           :principal (format "chat:%s" session-id)
           :board-id board-id :association-role "owner"
           :routing-policy policy))
         (position 0))
    (list :records
          (mapcar
           (lambda (record)
             (let ((copy (copy-tree record t)))
               (plist-put copy :journal-position (cl-incf position))
               copy))
           (plist-get session :admission-records))
          :query-delta (plist-get session :query-delta)
          :policy policy)))

(cl-defmacro e-chat-board-admission-test--with-fixture ((store harness) &rest body)
  "Run BODY with a disposable SQLite STORE and HARNESS."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((directory (make-temp-file "e-chat-board-admission-" t))
          (,store (e-session-sqlite-store-create directory :asynchronous t))
          (,harness
           (e-harness-create
            :sessions ,store
            :backend (e-backend-fake-create :items nil))))
     (unwind-protect
         (progn ,@body)
       (when (gethash ,harness e-chat-service--bindings)
         (maphash (lambda (_session-id binding)
                    (ignore-errors (e-chat-service--retire-binding binding)))
                  (gethash ,harness e-chat-service--bindings)))
       (ignore-errors (e-session-sqlite-store-close ,store))
       (delete-directory directory t))))

(ert-deftest e-chat-board-admission-test-stable-key-immediate-id-and-exact-open ()
  "Stable owner admission shares work and opens one exact Board owner."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((first
            (e-chat-service-owner-admission-start
             :harness harness :creation-key "org:daily:stable"
             :metadata '(:name "Stable Daily")))
           (second
            (e-chat-service-owner-admission-start
             :harness harness :creation-key "org:daily:stable"
             :metadata '(:name "Stable Daily"))))
      (should (string-prefix-p "brd_"
                               (e-chat-service-owner-admission-provisional-board-id
                                first)))
      (should (equal
               (e-chat-service-owner-admission-provisional-board-id first)
               (e-chat-service-owner-admission-provisional-board-id second)))
      (should (eq (e-chat-service-owner-admission-work first)
                  (e-chat-service-owner-admission-work second)))
      (let* ((admitted
              (e-chat-board-admission-test--await
               (e-chat-service-owner-admission-work first)))
             (opened
              (e-chat-board-admission-test--await
               (e-chat-service-open-board-owner-start
                (e-chat-service-owner-admission-provisional-board-id first)
                harness)))
        (should (equal (plist-get admitted :board-id)
                       (e-chat-service-binding-board-id opened)))
        (should (equal (plist-get (plist-get admitted :association)
                                  :session-id)
                       (e-chat-service-binding-session-id opened)))
        (should (equal (plist-get (plist-get admitted :association)
                                  :participant-id)
                       (e-chat-service-binding-participant-id opened))))))))

(ert-deftest e-chat-board-admission-test-legacy-resolver-is-read-only ()
  "Legacy session resolution validates the exact owner without binding it."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((session-id "legacy-owner-session")
           (board-id "legacy-owner-board")
           (participant-id "legacy-owner-participant")
           (admission
            (e-chat-board-admission-test--owner-admission
             session-id board-id participant-id))
           (service (e-board-sqlite-service-create
                     (e-session-storage-runtime-store store))))
      (e-chat-board-admission-test--await
       (e-board-sqlite-service-admit-session-owner-start
        service session-id board-id (format "chat:%s" session-id)
        (plist-get admission :records) (plist-get admission :query-delta)
        (list :id participant-id :author "e-chat"
              :principal (format "chat:%s" session-id)
              :controller (format "chat:%s" session-id)
              :role 'owner :state 'active
              :subscription-id "pickup-main" :publication-pending nil)))
      (let ((resolved
             (e-chat-board-admission-test--await
              (e-chat-service-resolve-legacy-session-start
               harness session-id))))
        (should (equal (plist-get resolved :board-id) board-id))
        (should (equal (plist-get (plist-get resolved :association) :session-id)
                       session-id))
        (should-not (e-chat-service-binding harness session-id))))))

(provide 'e-chat-board-admission-test)

;;; e-chat-board-admission-test.el ends here
