;;; e-goodnite-storage-sqlite.el --- SQLite Goodnite demand adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'e-goodnite-storage)
(require 'e-runtime-store)

(defun e-goodnite-storage-sqlite--call (runtime operation arguments)
  "Dispatch Goodnite OPERATION ARGUMENTS through RUNTIME."
  (pcase operation
    ('append
     (pcase-let ((`(,event-id ,event) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'goodnite-event-append :event-id event-id :event event))))
    ('page
     (pcase-let ((`(,after ,limit ,consumer) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'goodnite-event-page :after after :limit limit
              :consumer consumer))))
    ('ack
     (pcase-let ((`(,consumer ,position) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'goodnite-checkpoint-ack :consumer consumer
              :position position))))
    ('cleanup
     (pcase-let ((`(,consumer ,limit) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'goodnite-event-cleanup :consumer consumer :limit limit))))
    (_ (signal 'e-goodnite-storage-error
               (list "Unknown Goodnite storage operation" operation)))))

(defun e-goodnite-storage-sqlite-create (runtime)
  "Return Goodnite demand storage backed by shared RUNTIME."
  (unless (e-runtime-store-p runtime)
    (signal 'wrong-type-argument (list 'e-runtime-store-p runtime)))
  (e-goodnite-storage--create
   :runtime runtime
   :call-operation
   (lambda (operation &rest arguments)
     (e-goodnite-storage-sqlite--call runtime operation arguments))))

(provide 'e-goodnite-storage-sqlite)

;;; e-goodnite-storage-sqlite.el ends here
