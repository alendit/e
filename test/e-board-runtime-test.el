;;; e-board-runtime-test.el --- Tests for board harness delivery -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board-runtime)

(defmacro e-board-runtime-test--with-empty-state (&rest body)
  "Run BODY with isolated board, registry, and runtime attachment state."
  (declare (indent 0) (debug t))
  `(let ((e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
         (e-board-registry--boards (make-hash-table :test 'equal))
         (e-board-registry--id-sequence 0)
         (e-board-runtime--attachments (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest e-board-runtime-test-attachment-maps-live-session-and-delivers-exact-and-tags ()
  "Attached sessions receive only their frozen exact or tag-routed pickups."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (first-harness (e-harness-create))
           (second-harness (e-harness-create))
           (deliveries nil)
           (delivery
            (lambda (attachment pickup message)
              (setq deliveries
                    (append deliveries
                            (list (list
                                   (e-board-registry-participant-id
                                    (e-board-runtime-attachment-participant attachment))
                                   (e-board-pickup-delivery-id pickup)
                                   (e-board-message-content message))))))))
      (e-harness-create-session first-harness :id "first-session")
      (e-harness-create-session second-harness :id "second-session")
      (let* ((first (e-board-runtime-attach
                     board first-harness "first-session"
                     :participant-id "first" :delivery-function delivery))
             (_second (e-board-runtime-attach
                       board second-harness "second-session"
                       :participant-id "second" :delivery-function delivery)))
        (e-board-registry-install-subscription board "first" '(:tags (main))
                                               :id "first-main")
        (e-board-registry-install-subscription board "second" '(:tags (main))
                                               :id "second-main")
        (should (eq (e-board-runtime-attachment-board first) board))
        (should (equal (e-board-registry-participant-id
                        (e-board-runtime-attachment-participant first))
                       "first"))
        (should (eq (e-board-runtime-attachment-harness first) first-harness))
        (should (equal (e-board-runtime-attachment-session-id first)
                       "first-session"))
        (e-board-runtime-post-input board :id "exact" :to "first" :tags '(main)
                                    :content "exact message")
        (e-board-runtime-post-input board :id "tagged" :tags '(main)
                                    :content "tagged message")
        (should (equal (mapcar #'car deliveries) '("first" "first" "second")))
        (should (equal (mapcar #'car (mapcar #'cdr deliveries))
                       '(("board" "exact" "first")
                         ("board" "tagged" "first")
                         ("board" "tagged" "second"))))))))

(ert-deftest e-board-runtime-test-default-queue-delivery-enters-idle-follow-up-queue ()
  "Default queue delivery uses the harness queue without starting a turn."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (e-board-runtime-post-input board :to "participant" :mode 'queue
                                  :content "queued input")
      (should (equal (plist-get (car (e-harness-queued-prompts harness "session"))
                                :prompt)
                     "queued input"))
      (should-not (plist-get (e-harness-state harness "session") :active-turn)))))

(provide 'e-board-runtime-test)

;;; e-board-runtime-test.el ends here
