;;; e-session-sqlite-test.el --- SQLite session and tool continuity scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-session)
(require 'e-runtime-store-worker)

(cl-defmacro e-session-sqlite-test--with-store ((store directory) &rest body)
  "Run BODY with an opt-in STORE in disposable DIRECTORY."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-session-sqlite-test-" t))
          (,store (e-session-sqlite-store-create ,directory)))
     (unwind-protect (progn ,@body)
       (ignore-errors (e-session-sqlite-store-close ,store))
       (delete-directory ,directory t))))









(ert-deftest e-session-sqlite-s3-ordered-barrier-dispatches-status-only ()
  "The ordered barrier requests status without an integrity operation."
  (let (observed)
    (cl-letf (((symbol-function 'e-session-storage-sqlite--call)
               (lambda (store kind body)
                 (setq observed (list store kind body))
                 'acknowledged)))
      (should (eq (e-session-storage-sqlite-ordered-barrier 'store)
                  'acknowledged))
      (should (equal observed '(store read (:op status)))))))






(defconst e-session-sqlite-test--large-record-count 15722)
(defconst e-session-sqlite-test--large-content-bytes 37851478)




(provide 'e-session-sqlite-test)

;;; e-session-sqlite-test.el ends here
