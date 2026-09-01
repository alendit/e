;;; e-cron-storage-sqlite.el --- SQLite cron storage adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'e-cron-storage)
(require 'e-runtime-store)
(require 'e-runtime-store-codec)

(defun e-cron-storage-sqlite--stable-id (prefix value)
  "Return stable command id for PREFIX and VALUE."
  (format "%s:%s" prefix
          (secure-hash 'sha256 (e-runtime-store-codec-encode value))))

(defun e-cron-storage-sqlite--call (runtime operation arguments)
  "Dispatch cron OPERATION ARGUMENTS through RUNTIME."
  (pcase operation
    ('register
     (pcase-let ((`(,id ,definition ,anchor) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'cron-register :schedule-id id
              :definition-hash
              (secure-hash 'sha256
                           (e-runtime-store-codec-encode definition))
              :anchor anchor))))
    ('claim
     (pcase-let
         ((`(,id ,expected ,firing-id ,due-at ,fire-at ,next-fire) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'cron-claim :schedule-id id
              :expected-revision expected :firing-id firing-id
              :due-at due-at :fire-at fire-at :next-fire next-fire)
        (e-cron-storage-sqlite--stable-id "cron-claim" firing-id))))
    ('settle
     (pcase-let
         ((`(,id ,firing-id ,expected-state ,state ,result) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'cron-settle :schedule-id id :firing-id firing-id
              :expected-state expected-state :state state :result result)
        (e-cron-storage-sqlite--stable-id
         "cron-settle" (list firing-id state)))))
    ('cadence
     (e-runtime-store-call
      runtime 'read (list :op 'cron-cadence :schedule-id (car arguments))))
    ('delete-history
     (pcase-let ((`(,id ,expected) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'cron-history-delete :schedule-id id
              :expected-revision expected))))
    (_ (signal 'e-cron-storage-error
               (list "Unknown cron storage operation" operation)))))

(defun e-cron-storage-sqlite-create (runtime)
  "Return a cron storage port backed by shared RUNTIME."
  (unless (e-runtime-store-p runtime)
    (signal 'wrong-type-argument (list 'e-runtime-store-p runtime)))
  (e-cron-storage--create
   :runtime runtime
   :call-operation
   (lambda (operation &rest arguments)
     (e-cron-storage-sqlite--call runtime operation arguments))))

(provide 'e-cron-storage-sqlite)

;;; e-cron-storage-sqlite.el ends here
