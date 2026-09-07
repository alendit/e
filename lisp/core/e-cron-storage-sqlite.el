;;; e-cron-storage-sqlite.el --- SQLite cron storage adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'e-cron-storage)
(require 'e-runtime-store)

(defun e-cron-storage-sqlite--body (operation arguments)
  "Return typed worker body for cron OPERATION and ARGUMENTS."
  (pcase operation
    ('register
     (pcase-let ((`(,id ,definition ,anchor) arguments))
       (list :op 'cron-register :schedule-id id
             :definition-hash
             (secure-hash 'sha256
                          (e-runtime-store-codec-encode definition))
             :anchor anchor)))
    ('claim
     (pcase-let ((`(,id ,firing-id ,due-at ,fire-at ,next-fire) arguments))
       (list :op 'cron-claim :schedule-id id
             :firing-id firing-id :due-at due-at :fire-at fire-at
             :next-fire next-fire)))
    ('settle
     (pcase-let ((`(,id ,firing-id ,expected-state ,state ,result) arguments))
       (list :op 'cron-settle :schedule-id id :firing-id firing-id
             :expected-state expected-state :state state :result result)))
    ('cadence (list :op 'cron-cadence :schedule-id (car arguments)))
    ('delete-history
     (list :op 'cron-history-delete :schedule-id (car arguments)))
    (_ (signal 'e-cron-storage-error
               (list "Unknown cron storage operation" operation)))))

(defun e-cron-storage-sqlite--submit
    (runtime kind operation arguments on-settle)
  "Submit cron OPERATION through RUNTIME and observe settlement."
  (let* ((body (e-cron-storage-sqlite--body operation arguments))
         (request (e-runtime-store-submit runtime kind body)))
    (e-runtime-store--observe
     request
     (lambda (settled)
       (if (eq (e-runtime-store-request--state settled) 'committed)
           (funcall on-settle (e-runtime-store-request--result settled) nil)
         (funcall on-settle nil
                  (or (e-runtime-store-request--error settled)
                      '(e-cron-storage-error
                        "Runtime request did not commit"))))))
    request))

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
         ((`(,id ,firing-id ,due-at ,fire-at ,next-fire) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'cron-claim :schedule-id id
              :firing-id firing-id :due-at due-at :fire-at fire-at
              :next-fire next-fire))))
    ('settle
     (pcase-let
         ((`(,id ,firing-id ,expected-state ,state ,result) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'cron-settle :schedule-id id :firing-id firing-id
              :expected-state expected-state :state state :result result))))
    ('cadence
     (e-runtime-store-call
      runtime 'read (list :op 'cron-cadence :schedule-id (car arguments))))
    ('delete-history
     (pcase-let ((`(,id) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'cron-history-delete :schedule-id id))))
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
     (e-cron-storage-sqlite--call runtime operation arguments))
   :submit-operation
   (lambda (kind operation arguments on-settle)
     (e-cron-storage-sqlite--submit
      runtime kind operation arguments on-settle))))

(provide 'e-cron-storage-sqlite)

;;; e-cron-storage-sqlite.el ends here
