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

(ert-deftest e-board-registry-test-default-identities-have-board-local-prefixes ()
  "Registry fallback identities retain their distinct board-local namespaces."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create))
           (client (e-board-registry-attach-client board))
           (participant (e-board-registry-add-participant board)))
      (should (string-prefix-p "brd_" (e-board-registry-board-id board)))
      (should (string-prefix-p "cli_" (e-board-registry-client-id client)))
      (should (string-prefix-p "ptc_" (e-board-registry-participant-id participant))))))

(ert-deftest e-board-registry-test-owner-controls-explicit-principal-grants ()
  "Only owners mutate board grants, and a board retains an owner."
  (e-board-registry-test--with-empty-registries
    (let ((board (e-board-registry-create :id "board" :principal "owner")))
      (should (eq (e-board-registry-principal-role board "owner") 'owner))
      (e-board-registry-authorize-principal board "owner" "member" 'member)
      (should (eq (e-board-registry-principal-role board "member") 'member))
      (should-error (e-board-registry-authorize-principal
                     board "member" "other" 'member)
                    :type 'e-board-registry-authorization-denied)
      (e-board-registry-authorize-principal board "owner" "other-owner" 'owner)
      (should (eq (e-board-registry-revoke-principal board "owner" "other-owner")
                  'owner))
      (should-error (e-board-registry-revoke-principal board "owner" "owner")
                    :type 'e-board-registry-authorization-denied))))

(ert-deftest e-board-registry-test-reconnected-client-gets-a-fresh-generation ()
  "Reusing a detached client id never revives its old connection generation."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (first (e-board-registry-attach-client board :id "client")))
      (should (eq (e-board-registry-client-state first) 'active))
      (should (= (e-board-registry-client-generation first) 1))
      (e-board-registry-detach-client board "client")
      (should (eq (e-board-registry-client-state first) 'detached))
      (let ((replacement (e-board-registry-attach-client board :id "client")))
        (should (= (e-board-registry-client-generation replacement) 2))
        (should (eq (e-board-registry-client-state replacement) 'active))))))

(ert-deftest e-board-registry-test-reconnect-fences-old-observer-cursor ()
  "A reconnect cannot operate a cursor installed by its old generation."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (first (e-board-registry-attach-client board :id "client"))
           (observer (e-board-registry-install-observer
                      board "client" '(:tags (main)) :id "old")))
      (should (= (e-board-observer-client-generation observer) 1))
      (e-board-registry-detach-client board "client")
      (let* ((replacement (e-board-registry-attach-client board :id "client"))
             (fresh (e-board-registry-install-observer
                     board "client" '(:tags (main)) :id "fresh")))
        (should (= (e-board-registry-client-generation replacement) 2))
        (should (= (e-board-observer-client-generation fresh) 2))
        (should-error
         (e-board-registry-prepare-observer-page board "client" "old")
         :type 'e-board-registry-error)
        (should (eq (e-board-registry-client-state first) 'detached))))))

(ert-deftest e-board-registry-test-list-page-is-bounded-and-continuable ()
  "Registry pages keep deterministic order without exposing the whole list."
  (e-board-registry-test--with-empty-registries
    (e-board-registry-create :id "charlie")
    (e-board-registry-create :id "alpha")
    (e-board-registry-create :id "bravo")
    (let ((first (e-board-registry-list-page :limit 2)))
      (should (equal (mapcar #'e-board-registry-board-id (plist-get first :boards))
                     '("alpha" "bravo")))
      (should (equal (plist-get first :next-after) "bravo"))
      (let ((second (e-board-registry-list-page
                     :after (plist-get first :next-after) :limit 2)))
        (should (equal (mapcar #'e-board-registry-board-id
                               (plist-get second :boards))
                       '("charlie")))
        (should-not (plist-get second :next-after))))
    (should-error (e-board-registry-list-page :limit 0)
                  :type 'wrong-type-argument)))

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

(ert-deftest e-board-registry-test-participant-stale-and-dormant-states-retain-route ()
  "Recoverable participant states remain routable and record their lifecycle."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (participant (e-board-registry-add-participant board :id "member"))
           (source-board (e-board-registry-board-source-board board)))
      (e-board-registry-set-participant-state board participant 'dormant)
      (should (eq (e-board-participant-state
                   (e-board-registry-participant-source-participant participant))
                  'dormant))
      (let ((publication (e-board-post-input source-board :id "dormant" :to "member")))
        (e-board-drain-input-classifications source-board)
        (should (equal (e-board-publication-pickup-ids publication)
                       '(("board" "dormant" "member")))))
      (e-board-registry-set-participant-state board participant 'stale)
      (should (eq (e-board-participant-state
                   (e-board-registry-participant-source-participant participant))
                  'stale))
      (should (eq (e-board-event-type (car (last (e-board-events source-board))))
                  'participant-stale))
      (e-board-registry-set-participant-state board participant 'active)
      (should (eq (e-board-event-type (car (last (e-board-events source-board))))
                  'participant-rebound))
      (should-error (e-board-registry-set-participant-state board participant 'removed)
                    :type 'wrong-type-argument))))

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
      (let ((expiring (e-board-registry-install-subscription
                       board participant '(:tags (later)) :id "expiring")))
        (e-board-registry-expire-subscription board "expiring")
        (should (eq (e-board-subscription-state expiring) 'expired))
        (should-error (e-board-registry-resume-subscription board "expiring")
                      :type 'e-board-error))
      (should-error
       (e-board-registry-mute-subscription
        board
        (e-board-participant-create-pickup-subscription-id
         (e-board-registry-participant-source-participant participant)))
       :type 'e-board-error)
      (should (e-board-find-subscription source "main")))))

(ert-deftest e-board-registry-test-replaces-ordinary-subscription-with-fresh-route ()
  "A replacement retains membership but installs only its new future selector."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (participant (e-board-registry-add-participant board :id "member"))
           (source-board (e-board-registry-board-source-board board))
           (old (e-board-registry-install-subscription
                 board participant '(:tags (old)) :id "old"))
           (replacement
            (e-board-registry-replace-subscription
             board "old" '(:tags (new)) :id "new")))
      (should (eq (e-board-subscription-state old) 'cancelled))
      (should (equal (e-board-subscription-participant-id replacement) "member"))
      (let ((old-publication (e-board-post-input source-board :tags '(old))))
        (e-board-drain-input-classifications source-board)
        (should-not (e-board-publication-pickup-ids old-publication)))
      (let ((new-publication (e-board-post-input source-board :tags '(new))))
        (e-board-drain-input-classifications source-board)
        (should (= (length (e-board-publication-pickup-ids new-publication)) 1))))))

(ert-deftest e-board-registry-test-attached-client-owns-observer-cursor ()
  "Observation is available only through a board-local attached client."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (client (e-board-registry-attach-client board :id "client"))
           (observer (e-board-registry-install-observer
                      board (e-board-registry-client-id client)
                      '(:tags (main)) :id "observer")))
      (should (equal (e-board-observer-client-id observer) "client"))
      (should-error (e-board-registry-install-observer
                     board "missing" '(:tags (main)))
                    :type 'e-board-registry-client-missing))))

(ert-deftest e-board-registry-test-new-observer-defaults-to-the-live-high-watermark ()
  "An omitted registry START-SEQ preserves the core's future-only default."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (client (e-board-registry-attach-client board :id "client"))
           (source-board (e-board-registry-board-source-board board)))
      (e-board-post-fact source-board :id "before" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (let ((observer (e-board-registry-install-observer
                       board (e-board-registry-client-id client)
                       '(:tags (main)) :id "observer")))
        (e-board-post-fact source-board :id "after" :tags '(main)
                           :source-fact-key '(producer 1 2))
        (should (equal (mapcar #'e-board-message-id
                               (plist-get (e-board-registry-prepare-observer-page
                                           board "client" "observer" :limit 8)
                                          :messages))
                       '("after")))
        (should (= (e-board-observer-next-seq observer) 1))))))

(ert-deftest e-board-registry-test-stale-live-observer-requires-resnapshot ()
  "A retention advance expires only the stale cursor when it next reads."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (client (e-board-registry-attach-client board :id "client"))
           (source-board (e-board-registry-board-source-board board))
           (stale (e-board-registry-install-observer
                   board "client" '(:tags (main)) :id "stale" :start-seq 0)))
      (e-board-post-fact source-board :id "one" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (e-board-post-fact source-board :id "two" :tags '(main)
                         :source-fact-key '(producer 1 2))
      (e-board-advance-retention-floor source-board 2)
      (let ((page (e-board-registry-prepare-observer-page
                   board (e-board-registry-client-id client) "stale")))
        (should (plist-get page :resnapshot-required))
        (should-not (plist-get page :messages))
        (should (eq (e-board-observer-state stale) 'expired)))
      (let* ((fresh (e-board-registry-install-observer
                     board "client" '(:tags (main)) :id "fresh" :start-seq 1))
             (page (e-board-registry-prepare-observer-page
                    board "client" "fresh")))
        (should (equal (mapcar #'e-board-message-id (plist-get page :messages))
                       '("one" "two")))
        (should-not (plist-get page :resnapshot-required))
        (should (eq (e-board-observer-state fresh) 'active))))))

(ert-deftest e-board-registry-test-client-acknowledges-only-its-observer-page ()
  "An attached client explicitly accepts its own prepared observer page."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (client (e-board-registry-attach-client board :id "client"))
           (other (e-board-registry-attach-client board :id "other"))
           (observer (e-board-registry-install-observer
                      board "client" '(:tags (main)) :id "observer" :start-seq 0))
           (source-board (e-board-registry-board-source-board board)))
      (e-board-post-fact source-board :id "fact" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (let ((page (e-board-registry-prepare-observer-page
                   board (e-board-registry-client-id client) "observer" :limit 1)))
        (should (= (e-board-observer-next-seq observer) 0))
        (should-error
         (e-board-registry-accept-observer-page
          board (e-board-registry-client-id other) "observer"
          (plist-get page :through-seq))
         :type 'e-board-registry-error)
        (e-board-registry-accept-observer-page
         board (e-board-registry-client-id client) "observer"
         (plist-get page :through-seq))
        (should (= (e-board-observer-next-seq observer)
                   (plist-get page :through-seq)))))))

(ert-deftest e-board-registry-test-client-acknowledges-only-its-observer-history-page ()
  "An attached client explicitly accepts only its own prepared history page."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (client (e-board-registry-attach-client board :id "client"))
           (other (e-board-registry-attach-client board :id "other"))
           (observer (e-board-registry-install-observer
                      board "client" '(:tags (main)) :id "observer"
                      :history-before-seq 3))
           (source-board (e-board-registry-board-source-board board)))
      (e-board-post-fact source-board :id "one" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (e-board-post-fact source-board :id "two" :tags '(main)
                         :source-fact-key '(producer 1 2))
      (let ((page (e-board-registry-prepare-observer-history-page
                   board (e-board-registry-client-id client) "observer" :limit 1)))
        (should (= (e-board-observer-history-before-seq observer) 3))
        (should-error
         (e-board-registry-accept-observer-history-page
          board (e-board-registry-client-id other) "observer"
          (plist-get page :before-seq))
         :type 'e-board-registry-error)
        (e-board-registry-accept-observer-history-page
         board (e-board-registry-client-id client) "observer"
         (plist-get page :before-seq))
        (should (= (e-board-observer-history-before-seq observer)
                   (plist-get page :before-seq)))))))

(ert-deftest e-board-registry-test-client-retries-its-observer-history-acceptance ()
  "A client can safely retry an acknowledgement after the board committed it."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (client (e-board-registry-attach-client board :id "client"))
           (observer (e-board-registry-install-observer
                      board "client" '(:tags (main)) :id "observer"
                      :history-before-seq 3))
           (source-board (e-board-registry-board-source-board board)))
      (e-board-post-fact source-board :id "fact" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (let* ((page (e-board-registry-prepare-observer-history-page
                    board (e-board-registry-client-id client) "observer" :limit 1))
             (receipt (plist-get page :before-seq)))
        (e-board-registry-accept-observer-history-page
         board (e-board-registry-client-id client) "observer" receipt)
        (e-board-registry-accept-observer-history-page
         board (e-board-registry-client-id client) "observer" receipt)
        (should (= (e-board-observer-history-before-seq observer) receipt))))))

(ert-deftest e-board-registry-test-client-replaces-only-its-observer-with-explicit-backfill ()
  "A client may widen its own selector without mutating another cursor."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (client (e-board-registry-attach-client board :id "client"))
           (other (e-board-registry-attach-client board :id "other"))
           (observer (e-board-registry-install-observer
                      board "client" '(:tags (main)) :id "main" :start-seq 0))
           (source-board (e-board-registry-board-source-board board)))
      (e-board-post-fact source-board :id "subagent" :tags '(subagent)
                         :source-fact-key '(producer 1 1))
      (should-error
       (e-board-registry-replace-observer
        board (e-board-registry-client-id other) "main" '(:tags (subagent)))
       :type 'e-board-registry-error)
      (let* ((replacement
              (e-board-registry-replace-observer
               board (e-board-registry-client-id client) "main"
               '(:tags (subagent)) :id "subagent" :start-seq 0))
             (page (e-board-registry-prepare-observer-page
                    board (e-board-registry-client-id client) "subagent" :limit 8)))
        (should (eq (e-board-observer-state observer) 'cancelled))
        (should (equal (e-board-observer-client-id replacement) "client"))
        (should (equal (e-board-registry-client-observer-ids client)
                       '("subagent")))
        (should (equal (mapcar #'e-board-message-id (plist-get page :messages))
                       '("subagent")))))))

(ert-deftest e-board-registry-test-client-controls-only-its-observer-lifecycle ()
  "Attached-client authorization precedes every observer state transition."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (client (e-board-registry-attach-client board :id "client"))
           (other (e-board-registry-attach-client board :id "other"))
           (observer (e-board-registry-install-observer
                      board "client" '(:tags (main)) :id "observer")))
      (should-error
       (e-board-registry-set-observer-state
        board (e-board-registry-client-id other) "observer" 'muted)
       :type 'e-board-registry-error)
      (e-board-registry-set-observer-state
       board (e-board-registry-client-id client) "observer" 'muted)
      (should (eq (e-board-observer-state observer) 'muted))
      (e-board-registry-set-observer-state
       board (e-board-registry-client-id client) "observer" 'active)
      (e-board-registry-set-observer-state
       board (e-board-registry-client-id client) "observer" 'cancelled)
      (should (eq (e-board-observer-state observer) 'cancelled)))))

(ert-deftest e-board-registry-test-detach-client-cancels-its-observers ()
  "Disconnecting a client releases every nonterminal cursor it owns."
  (e-board-registry-test--with-empty-registries
    (let* ((board (e-board-registry-create :id "board"))
           (client (e-board-registry-attach-client board :id "client"))
           (observer (e-board-registry-install-observer
                      board "client" '(:tags (main)) :id "observer")))
      (e-board-registry-detach-client board (e-board-registry-client-id client))
      (should (eq (e-board-observer-state observer) 'cancelled))
      (should-not (e-board-registry-client-observer-ids client))
      (should-not (gethash "client" (e-board-registry-board-clients board)))
      (should-not (e-board-observer-read-page
                   (e-board-registry-board-source-board board) "observer")))))

(provide 'e-board-registry-test)

;;; e-board-registry-test.el ends here
