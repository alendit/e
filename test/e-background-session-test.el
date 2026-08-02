;;; e-background-session-test.el --- Board producer trigger tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-background-session)

(cl-defmacro e-background-session-test--with-board ((board binding) &body body)
  "Run BODY with isolated registry state and live producer BINDING on BOARD."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let ((e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
         (e-board-registry--boards (make-hash-table :test 'equal))
         (e-board-registry--id-sequence 0)
         (e-board-runtime--producer-bindings (make-hash-table :test 'equal))
         (e-board-runtime--producer-epoch 0)
         (e-board-runtime--producer-head nil)
         (e-board-runtime--producer-tail nil)
         (e-board-runtime--producer-drain-scheduled nil)
         (e-board-runtime--producer-scheduler (lambda (_callback)))
         (e-board-runtime--admission-open-p t)
         (e-board-runtime--unsettled-producer-count 0)
         (e-board-runtime--unsettled-generation 0)
         (e-background-session--triggers (make-hash-table :test 'equal)))
     (let* ((,board (e-board-registry-create :id "background-board"))
            (,binding (e-board-runtime-producer-bind
                       'background-test ,board :tags '(domain))))
       ,@body)))

(ert-deftest e-background-session-test-register-requires-live-binding ()
  "A trigger cannot be enabled with missing process-local board authority."
  (should-error
   (e-background-session-register :id 'missing :prompt "changed")
   :type 'e-board-runtime-producer-disabled))

(ert-deftest e-background-session-test-fire-publishes-board-fact ()
  "A fire queues one descriptive fact and creates no participant or turn."
  (e-background-session-test--with-board (board binding)
    (let* ((trigger (e-background-session-register
                     :id 'sources :producer-binding binding :prompt "changed"
                     :paths '("/tmp/source") :metadata '(:source file)))
           (item (e-background-session-fire trigger)))
      (should (eq (e-board-runtime-producer-publication-state item) 'queued))
      (e-board-runtime-drain-producers)
      (let ((message (e-board-publication-message
                      (e-board-runtime-producer-publication-publication item))))
        (should (eq (e-board-message-kind message) 'fact))
        (should (equal (e-board-message-content message) "changed"))
        (should (equal (e-board-message-tags message)
                       '(domain background trigger sources)))
        (should (equal (plist-get (e-board-message-attributes message) :source)
                       'file)))
      (should (= (hash-table-count (e-board-registry-board-participants board)) 0)))))

(ert-deftest e-background-session-test-stale-binding-cannot-start-or-fire ()
  "Disabling a binding fences retained trigger callbacks after restart."
  (e-background-session-test--with-board (_board binding)
    (let ((trigger (e-background-session-register
                    :id 'stale :producer-binding binding :prompt "changed")))
      (e-board-runtime-producer-disable binding)
      (should-error (e-background-session-start trigger)
                    :type 'e-board-runtime-producer-disabled)
      (should-error (e-background-session-fire trigger)
                    :type 'e-board-runtime-producer-disabled))))

(ert-deftest e-background-session-test-debounce-coalesces-one-publication ()
  "Repeated requests retain only the newest bounded timer callback."
  (e-background-session-test--with-board (_board binding)
    (let ((scheduled nil) (cancelled nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest arguments)
                   (let ((token (list function arguments)))
                     (push token scheduled)
                     token)))
                ((symbol-function 'timerp) #'listp)
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled))))
        (let ((trigger (e-background-session-register
                        :id 'debounce :producer-binding binding
                        :prompt "changed")))
          (e-background-session--request-fire trigger)
          (e-background-session--request-fire trigger)
          (should (= (length scheduled) 2))
          (should (= (length cancelled) 1))
          (apply (caar scheduled) (cadar scheduled))
          (should (= e-board-runtime--unsettled-producer-count 1)))))))

(provide 'e-background-session-test)

;;; e-background-session-test.el ends here
