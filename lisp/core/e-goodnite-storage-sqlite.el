;;; e-goodnite-storage-sqlite.el --- SQLite Goodnite demand adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'e-goodnite-storage)
(require 'e-runtime-store)
(require 'e-runtime-store-codec)

(defun e-goodnite-storage-sqlite--stable-id (prefix value)
  "Return stable command identity for PREFIX and VALUE."
  (format "%s:%s" prefix
          (secure-hash 'sha256 (e-runtime-store-codec-encode value))))

(defun e-goodnite-storage-sqlite--call (runtime operation arguments)
  "Dispatch Goodnite OPERATION ARGUMENTS through RUNTIME."
  (pcase operation
    ('append
     (pcase-let ((`(,event-id ,event) arguments))
       (let ((body (list :op 'goodnite-event-append
                         :event-id event-id :event event)))
         (e-runtime-store-call
          runtime 'write body
          ;; Transport reconciliation identifies this exact append attempt.
          ;; Domain deduplication by EVENT-ID remains worker-owned, so a later
          ;; equivalent demand carrying a different observation timestamp can
          ;; still return the original position.
          (e-goodnite-storage-sqlite--stable-id "goodnite-event" body)))))
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
