;;; e-background-session-test.el --- Board producer trigger tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-background-session)
(load (expand-file-name "e-board-producer-test-support.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(cl-defmacro e-background-session-test--with-board ((target) &body body)
  "Run BODY with isolated triggers and disposable SQLite TARGET."
  (declare (indent 1) (debug ((symbolp) body)))
  `(let ((e-background-session--triggers (make-hash-table :test 'equal)))
     (e-board-producer-test-with-target (,target)
       ,@body)))

(ert-deftest e-background-session-test-register-requires-sql-target ()
  "A trigger cannot be registered without an explicit SQLite target."
  (should-error
   (e-background-session-register :id 'missing :prompt "changed")
   :type 'wrong-type-argument))

(ert-deftest e-background-session-test-fire-publishes-board-input ()
  "A fire queues dispatchable work and exposes zero-match routing."
  (e-background-session-test--with-board (target)
    (let* ((trigger (e-background-session-register
                     :id 'sources :publication-target target :prompt "changed"
                     :paths '("/tmp/source") :metadata '(:source file)))
           (work (e-background-session-fire trigger)))
      (should (e-work-handle-p work))
      (e-board-producer-test-await work)
      (let ((record (car (e-board-producer-test-records target))))
        (should (eq (plist-get record :kind) 'input))
        (should (equal (plist-get record :content) "changed"))
        (should (equal (plist-get record :tags)
                       '(background trigger sources)))
        (should (equal (plist-get (plist-get record :attributes) :source)
                       'file))))))

(ert-deftest e-background-session-test-target-is-detached-from-chat-lifecycle ()
  "A retained trigger needs only its explicit SQLite address."
  (e-background-session-test--with-board (target)
    (let ((trigger (e-background-session-register
                    :id 'detached :publication-target target :prompt "changed")))
      (should (eq (e-background-trigger-publication-target trigger) target))
      (e-board-producer-test-await (e-background-session-fire trigger))
      (should (= (length (e-board-producer-test-records target)) 1)))))

(ert-deftest e-background-session-test-debounce-coalesces-one-publication ()
  "Repeated requests retain only the newest bounded timer callback."
  (e-background-session-test--with-board (target)
    (let ((scheduled nil) (cancelled nil) (published 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest arguments)
                   (let ((token (list function arguments)))
                     (push token scheduled)
                     token)))
                ((symbol-function 'timerp) #'listp)
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled)))
                ((symbol-function
                  'e-board-sqlite-publication-target-append-route-start)
                 (lambda (&rest _arguments)
                   (cl-incf published)
                   :published)))
        (let ((trigger (e-background-session-register
                        :id 'debounce :publication-target target
                        :prompt "changed")))
          (e-background-session--request-fire trigger)
          (e-background-session--request-fire trigger)
          (should (= (length scheduled) 2))
          (should (= (length cancelled) 1))
          (apply (caar scheduled) (cadar scheduled))
          (should (= published 1)))))))

(provide 'e-background-session-test)

;;; e-background-session-test.el ends here
