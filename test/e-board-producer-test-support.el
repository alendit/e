;;; e-board-producer-test-support.el --- Disposable SQL Board producer fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'e-board-sqlite-service)
(require 'e-runtime-store)
(require 'e-session)
(require 'e-session-query)
(require 'e-work)

(defun e-board-producer-test-await (work)
  "Await request-scoped WORK at this explicit test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 5.0)))

(defun e-board-producer-test-records (target &optional selector)
  "Return TARGET's bounded canonical records matching SELECTOR."
  (mapcar
   (lambda (row) (copy-tree (plist-get row :record) t))
   (plist-get
    (e-board-producer-test-await
     (e-board-sqlite-publication-target-record-page-start
      target :generation 1 :after 0 :limit 256 :selector selector))
    :records)))

(defun e-board-producer-test-admit-participant
    (service board-id session-id participant-id selector)
  "Admit one disposable SQL PARTICIPANT-ID matching SELECTOR."
  (let* ((principal (format "chat:%s" session-id))
         (policy (list :participant-id participant-id
                       :pickup-selector (copy-tree selector t)
                       :observer-selector (copy-tree selector t)
                       :default-tags nil :default-to nil))
         (session
          (e-board-sqlite-service-session-admission
           :id session-id :metadata nil :principal principal
           :board-id board-id :association-role "participant"
           :routing-policy policy))
         (records (copy-tree (plist-get session :admission-records) t))
         (position 0))
    (dolist (record records)
      (plist-put record :journal-position (cl-incf position)))
    (e-board-producer-test-await
     (e-board-sqlite-service-admit-participant-start
      service session-id board-id records (plist-get session :query-delta)
      (list :id participant-id :author "producer-test"
            :principal principal :controller principal
            :role 'participant :state 'active
            :subscription-id (format "sub-%s" participant-id)
            :publication-pending nil)))))

(cl-defmacro e-board-producer-test-with-target
    ((target &optional service board-id runtime) &rest body)
  "Run BODY with one disposable SQLite publication TARGET."
  (declare (indent 1)
           (debug ((symbolp &optional symbolp symbolp symbolp) body)))
  (let ((service-symbol (or service (make-symbol "service")))
        (board-id-symbol (or board-id (make-symbol "board-id")))
        (runtime-symbol (or runtime (make-symbol "runtime"))))
    `(let* ((directory (make-temp-file "e-board-producer-" t))
            (,board-id-symbol "producer-board")
            (,runtime-symbol (e-runtime-store-open directory))
            (,service-symbol
             (e-board-sqlite-service-create ,runtime-symbol))
            (,target
             (e-board-sqlite-publication-target-create
              ,service-symbol ,board-id-symbol :author "producer-test")))
       (unwind-protect
           (progn
             (e-board-producer-test-await
              (e-board-sqlite-service-board-create-start
               ,service-symbol ,board-id-symbol "producer-test"))
             ,@body)
         (ignore-errors (e-runtime-store-close ,runtime-symbol))
         (delete-directory directory t)))))

(provide 'e-board-producer-test-support)

;;; e-board-producer-test-support.el ends here
