;;; e-voice-storage-sqlite.el --- SQLite voice tell adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'e-runtime-store)
(require 'e-voice-storage)

(defun e-voice-storage-sqlite--call (runtime operation arguments)
  "Dispatch voice OPERATION ARGUMENTS through RUNTIME."
  (pcase operation
    ('record
     (pcase-let ((`(,key ,label ,description ,last ,cap) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'voice-record :key key :label label
              :description description :last last :cap cap))))
    ('list
     (e-runtime-store-call
      runtime 'read (list :op 'voice-list :limit (car arguments))))
    ('clear
     (e-runtime-store-call runtime 'write '(:op voice-clear)))
    (_ (signal 'e-voice-storage-error
               (list "Unknown voice storage operation" operation)))))

(defun e-voice-storage-sqlite-create (runtime)
  "Return voice storage backed by shared RUNTIME."
  (unless (e-runtime-store-p runtime)
    (signal 'wrong-type-argument (list 'e-runtime-store-p runtime)))
  (e-voice-storage--create
   :runtime runtime
   :call-operation
   (lambda (operation &rest arguments)
     (e-voice-storage-sqlite--call runtime operation arguments))))

(provide 'e-voice-storage-sqlite)

;;; e-voice-storage-sqlite.el ends here
