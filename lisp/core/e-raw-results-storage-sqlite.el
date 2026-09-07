;;; e-raw-results-storage-sqlite.el --- SQLite raw-result adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'e-raw-results-storage)
(require 'e-runtime-store)

(defun e-raw-results-storage-sqlite--body (operation arguments)
  "Return typed worker body for raw-result OPERATION and ARGUMENTS."
  (pcase operation
    ('put
     (pcase-let ((`(,uri ,content ,metadata ,created-at ,expires-at) arguments))
       (list :op 'raw-result-put :uri uri :content content
             :metadata metadata :created-at created-at
             :expires-at expires-at)))
    ('read
     (pcase-let ((`(,uri ,now) arguments))
       (list :op 'raw-result-read :uri uri :now now)))
    ('delete (list :op 'raw-result-delete :uri (car arguments)))
    ('expire
     (pcase-let ((`(,now ,limit) arguments))
       (list :op 'raw-result-expire :now now :limit limit)))
    (_ (signal 'e-raw-results-storage-error
               (list "Unknown raw-result storage operation" operation)))))

(defun e-raw-results-storage-sqlite--submit
    (runtime kind operation arguments on-settle)
  "Submit raw-result OPERATION through RUNTIME and observe settlement."
  (let* ((body (e-raw-results-storage-sqlite--body operation arguments))
         (request (e-runtime-store-submit runtime kind body)))
    (e-runtime-store--observe
     request
     (lambda (settled)
       (if (eq (e-runtime-store-request--state settled) 'committed)
           (funcall on-settle (e-runtime-store-request--result settled) nil)
         (funcall on-settle nil
                  (or (e-runtime-store-request--error settled)
                      '(e-raw-results-storage-error
                        "Runtime request did not commit"))))))
    request))

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
     (e-raw-results-storage-sqlite--call runtime operation arguments))
   :submit-operation
   (lambda (kind operation arguments on-settle)
     (e-raw-results-storage-sqlite--submit
      runtime kind operation arguments on-settle))))

(provide 'e-raw-results-storage-sqlite)

;;; e-raw-results-storage-sqlite.el ends here
