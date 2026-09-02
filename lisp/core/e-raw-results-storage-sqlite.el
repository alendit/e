;;; e-raw-results-storage-sqlite.el --- SQLite raw-result adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'e-raw-results-storage)
(require 'e-runtime-store)

(defun e-raw-results-storage-sqlite--call (runtime operation arguments)
  "Dispatch raw-result OPERATION ARGUMENTS through RUNTIME."
  (pcase operation
    ('put
     (pcase-let ((`(,uri ,content ,metadata ,created-at ,expires-at) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'raw-result-put :uri uri :content content
              :metadata metadata :created-at created-at
              :expires-at expires-at))))
    ('read
     (pcase-let ((`(,uri ,now) arguments))
       (e-runtime-store-call
        runtime 'read (list :op 'raw-result-read :uri uri :now now))))
    ('delete
     (e-runtime-store-call
      runtime 'write (list :op 'raw-result-delete :uri (car arguments))))
    ('expire
     (pcase-let ((`(,now ,limit) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'raw-result-expire :now now :limit limit))))
    (_ (signal 'e-raw-results-storage-error
               (list "Unknown raw-result storage operation" operation)))))

(defun e-raw-results-storage-sqlite-create (runtime)
  "Return raw-result storage backed by shared RUNTIME."
  (unless (e-runtime-store-p runtime)
    (signal 'wrong-type-argument (list 'e-runtime-store-p runtime)))
  (e-raw-results-storage--create
   :runtime runtime
   :call-operation
   (lambda (operation &rest arguments)
     (e-raw-results-storage-sqlite--call runtime operation arguments))))

(provide 'e-raw-results-storage-sqlite)

;;; e-raw-results-storage-sqlite.el ends here
