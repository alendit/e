;;; e-board-registry-test.el --- Tests for board lifecycle registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board)
(require 'e-board-registry)

(defmacro e-board-registry-test--with-empty-registries (&rest body)
  "Run BODY with isolated source-board and lifecycle registries."
  (declare (indent 0) (debug t))
  `(let ((e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
         (e-board-registry--boards (make-hash-table :test 'equal))
         (e-board-registry--id-sequence 0))
     ,@body))

(ert-deftest e-board-registry-test-owns-identities-and-registers-source-board ()
  "Registry identity generation is deterministic and source boards are core-registered."
  (e-board-registry-test--with-empty-registries
    (let* ((sequence 0)
           (id-function (lambda (kind)
                          (format "%s-%d" kind (cl-incf sequence)))))
      (let* ((board (e-board-registry-create :id-function id-function
                                             :author "author"
                                             :principal "principal"))
             (client (e-board-registry-attach-client board))
             (participant (e-board-registry-add-participant board))
             (subscription
              (e-board-registry-install-subscription board participant
                                                    '(:tags (updates)))))
        (should (equal (e-board-registry-board-id board) "board-1"))
        (should (eq (e-board-registry-board-source-board board)
                    (e-board-get "board-1")))
        (should (equal (e-board-registry-board-author board) "author"))
        (should (equal (e-board-registry-board-principal board) "principal"))
        (should (equal (e-board-registry-client-id client) "client-2"))
        (should (equal (e-board-registry-participant-id participant)
                       "participant-3"))
        (should (equal (e-board-participant-board-id
                        (e-board-registry-participant-source-participant
                         participant))
                       "board-1"))
        (should (equal (e-board-subscription-id subscription) "subscription-5"))
        (should (e-board-subscription-built-in-p
                 (car (e-board-subscriptions
                       (e-board-registry-board-source-board board)))))))))

(ert-deftest e-board-registry-test-participants-are-board-local ()
  "A participant record from one board cannot modify another board."
  (e-board-registry-test--with-empty-registries
    (let* ((one (e-board-registry-create :id "one"))
           (two (e-board-registry-create :id "two"))
           (participant (e-board-registry-add-participant one :id "member")))
      (should-error (e-board-registry-install-subscription
                     two participant '(:tags (updates)))
                    :type 'e-board-registry-participant-board-local)
      (should-error (e-board-registry-remove-participant two participant)
                    :type 'e-board-registry-participant-board-local)
      (should (e-board-registry-participant one participant)))))

(ert-deftest e-board-registry-test-close-prevents-further-mutation ()
  "Closing a board disables routing and leaves registry state unchanged thereafter."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (client (e-board-registry-attach-client board :id "client"))
           (participant (e-board-registry-add-participant board :id "member"))
           (source-board (e-board-registry-board-source-board board)))
      (e-board-registry-close board)
      (should (eq (e-board-registry-board-state board) 'closed))
      (should-error (e-board-get "board") :type 'e-board-missing)
      (should-error (e-board-registry-attach-client board :id "other")
                    :type 'e-board-registry-closed)
      (should-error (e-board-registry-detach-client board "client")
                    :type 'e-board-registry-closed)
      (should-error (e-board-registry-add-participant board :id "other")
                    :type 'e-board-registry-closed)
      (should-error (e-board-registry-remove-participant board participant)
                    :type 'e-board-registry-closed)
      (should-error (e-board-registry-install-subscription
                     board participant '(:tags (updates)))
                    :type 'e-board-registry-closed)
      (should-error (e-board-registry-close board)
                    :type 'e-board-registry-closed)
      (should (eq client (gethash "client" (e-board-registry-board-clients board))))
      (should (eq participant
                  (gethash "member" (e-board-registry-board-participants board))))
      (should (eq (e-board-participant-state
                   (e-board-registry-participant-source-participant participant))
                  'closed))
      (should (cl-every (lambda (subscription)
                          (eq (e-board-subscription-state subscription) 'inactive))
                         (e-board-subscriptions source-board))))))

(ert-deftest e-board-registry-test-controls-ordinary-subscription-lifecycle ()
  "Registry lifecycle operations do not expose the membership-owned route."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (participant (e-board-registry-add-participant board :id "member"))
           (subscription (e-board-registry-install-subscription
                          board participant '(:tags (main)) :id "main"))
           (source (e-board-registry-board-source-board board)))
      (e-board-registry-mute-subscription board "main")
      (should (eq (e-board-subscription-state subscription) 'muted))
      (e-board-registry-resume-subscription board "main")
      (should (eq (e-board-subscription-state subscription) 'active))
      (e-board-registry-cancel-subscription board "main")
      (should (eq (e-board-subscription-state subscription) 'cancelled))
      (should-error
       (e-board-registry-mute-subscription
        board
        (e-board-participant-create-pickup-subscription-id
         (e-board-registry-participant-source-participant participant)))
       :type 'e-board-error)
      (should (e-board-find-subscription source "main")))))

(provide 'e-board-registry-test)

;;; e-board-registry-test.el ends here
