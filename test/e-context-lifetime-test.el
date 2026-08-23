;;; e-context-lifetime-test.el --- Tests for generational context lifetimes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Focused mechanism tests for Feature 88's first semantic slice.  These tests
;; exercise the pure lifetime projection and the session resume manifest; they
;; do not opt the existing request loop into the new behavior.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'e)
(require 'e-context-lifetime)
(require 'e-session)
(require 'e-session-persistence)

(defun e-context-lifetime-test--generation ()
  "Return a small generation fixture."
  (e-context-lifetime-generation-create
   :id "generation-1"
   :checkpoint '((:role "system" :content "checkpoint"))
   :durable-tail '((:role "user" :content "durable intent"))))

(defun e-context-lifetime-test--frame (&optional state id observation)
  "Return a small observation FRAME fixture in STATE and ID.

OBSERVATION makes lifecycle snapshots easy to distinguish while preserving the
 same logical frame identity."
  (e-context-lifetime-frame-create
   :id (or id "frame-1")
   :generation-id "generation-1"
   :state state
   :observations (list (list :role "user"
                             :content (or observation "OBSERVATION-ONE")))
   :source-fingerprints '((:source "canvas" :version 1))
   :observation-ids '("observation-1")
   :consumption-attempt-ids '("attempt-1")
   :consuming-response-ids '("response-1")
   :promotion-ids '("promotion-1")))

(defun e-context-lifetime-test--append-lifecycle
    (store session-id &optional include-settlement)
  "Append a repeated-frame lifecycle to STORE and return its resume state.

The repeated snapshots deliberately reuse one logical frame id.  The final
 snapshot is the only one needed to reconstruct the unsettled frame."
  (e-session-append-context-generation
   store session-id
   (e-context-lifetime-generation-record
    (e-context-lifetime-test--generation)))
  (e-session-append-context-frame
   store session-id
   (e-context-lifetime-frame-record
    (e-context-lifetime-test--frame 'open "frame-1" "OBSERVATION-OPEN")))
  (e-session-append-context-frame
   store session-id
   (e-context-lifetime-frame-record
    (e-context-lifetime-test--frame
     'consuming "frame-1" "OBSERVATION-CONSUMING")))
  (e-session-append-context-frame
   store session-id
   (e-context-lifetime-frame-record
    (e-context-lifetime-test--frame
     'consumed "frame-1" "OBSERVATION-CONSUMED")))
  (e-session-append-context-promotion
   store session-id
   (e-context-lifetime-promotion-record
    (e-context-lifetime-promotion-create
     :id "promotion-1"
     :frame-id "frame-1"
     :facts '((:role "assistant" :content "selected fact")))))
  (when include-settlement
    (e-session-append-context-frame-settlement
     store session-id
     '(:record-version 1 :type context-frame-settlement
       :id "settlement-1" :frame-id "frame-1" :status failed)))
  (e-session-context-lifetime-resume-state store session-id))

(defun e-context-lifetime-test--append-bounded-retries (store session-id)
  "Append stale promotions and settlement attempts and return state metadata."
  (let* ((generation-entry
          (e-session-append-context-generation
           store session-id
           (e-context-lifetime-generation-record
            (e-context-lifetime-test--generation))))
         (frame-entry
          (e-session-append-context-frame
           store session-id
           (e-context-lifetime-frame-record
            (e-context-lifetime-test--frame 'consumed))))
         (_unreferenced-promotion
          (e-session-append-context-promotion
           store session-id
           (e-context-lifetime-promotion-record
            (e-context-lifetime-promotion-create
             :id "promotion-stale"
             :frame-id "frame-1"
             :facts '((:role "assistant" :content "stale fact"))))))
         (_old-promotion
          (e-session-append-context-promotion
           store session-id
           (e-context-lifetime-promotion-record
            (e-context-lifetime-promotion-create
             :id "promotion-1"
             :frame-id "frame-1"
             :facts '((:role "assistant" :content "old fact"))))))
         (latest-promotion
          (e-session-append-context-promotion
           store session-id
           (e-context-lifetime-promotion-record
            (e-context-lifetime-promotion-create
             :id "promotion-1"
             :frame-id "frame-1"
             :facts '((:role "assistant" :content "latest fact"))))))
         (_first-settlement
          (e-session-append-context-frame-settlement
           store session-id
           '(:record-version 1 :type context-frame-settlement
             :id "settlement-1" :frame-id "frame-1" :status failed)))
         (latest-settlement
          (e-session-append-context-frame-settlement
           store session-id
           '(:record-version 1 :type context-frame-settlement
             :id "settlement-2" :frame-id "frame-1" :status failed)))
         (state (e-session-context-lifetime-resume-state store session-id)))
    (list :state state
          :expected-entry-ids
          (list (plist-get generation-entry :id)
                (plist-get frame-entry :id)
                (plist-get latest-promotion :id)
                (plist-get latest-settlement :id)))))

(ert-deftest e-context-lifetime-test-shadow-projection-forgets-consumed-frame ()
  "Consumed observation bytes disappear while checkpoint and durable tail stay."
  (let* ((generation (e-context-lifetime-test--generation))
         (frame (e-context-lifetime-test--frame))
         (open (e-context-lifetime-project
                generation frame
                :static-prefix '((:role system :content "static"))
                :stable-context '((:role system :content "stable"))))
         (consumed (e-context-lifetime-project
                    generation
                    (e-context-lifetime-frame-consume frame)
                    :static-prefix '((:role system :content "static"))
                    :stable-context '((:role system :content "stable")))))
    (should (member "OBSERVATION-ONE"
                    (mapcar (lambda (message) (plist-get message :content))
                            (plist-get open :messages))))
    (should-not (member "OBSERVATION-ONE"
                        (mapcar (lambda (message) (plist-get message :content))
                                (plist-get consumed :messages))))
    (should (member "checkpoint"
                    (mapcar (lambda (message) (plist-get message :content))
                            (plist-get consumed :messages))))
    (should (member "durable intent"
                    (mapcar (lambda (message) (plist-get message :content))
                            (plist-get consumed :messages))))
    (let ((diagnostics (e-context-lifetime-projection-diagnostics open)))
      (should (> (plist-get diagnostics :ephemeral-character-count) 0))
      (should-not (plist-member diagnostics :ephemeral)))
    (should (equal (plist-get consumed :generation-id) "generation-1"))
    (should (equal (plist-get consumed :frame-id) "frame-1"))))

(ert-deftest e-context-lifetime-test-project-rejects-cross-generation-frame ()
  "A frame from another generation cannot enter the projection frontier."
  (let ((generation (e-context-lifetime-test--generation))
        (frame
         (e-context-lifetime-frame-create
          :id "frame-other"
          :generation-id "generation-other"
          :observations '((:role user :content "wrong generation")))))
    (should-error
     (e-context-lifetime-project generation frame)
     :type 'e-context-lifetime-invalid-record)))

(ert-deftest e-context-lifetime-test-canonical-projection-survives-reopen ()
  "Canonical nested semantic values retain exact projection and fingerprint."
  (let* ((directory (make-temp-file "e-context-lifetime-canonical-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "canonical-session")
         (generation
          (e-context-lifetime-generation-create
           :id 'generation-canonical
           :checkpoint
           [(:role system :content "checkpoint"
             :metadata (:kind decision :roles [assistant reviewer]
                        :enabled :json-false
                        :nested (:visible :json-false)))]
           :durable-tail
           '((:role assistant :content "durable"
              :metadata (:kind fact :source 'canvas)))))
         (frame
          (e-context-lifetime-frame-create
           :id 'frame-canonical
           :generation-id 'generation-canonical
           :observations
           '((:role user :content "observation"
              :metadata (:kind trace :actors [user tool]
                         :available :json-false)))))
         (static-prefix '((:role system :content "static" :metadata (:kind policy))))
         (stable-context '((:role developer :content "stable" :metadata (:kind rule))))
         (before (e-context-lifetime-project
                  generation frame :static-prefix static-prefix
                  :stable-context stable-context)))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (e-session-append-context-generation
           store session-id (e-context-lifetime-generation-record generation))
          (e-session-append-context-frame
           store session-id (e-context-lifetime-frame-record frame))
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (generation-record
                  (plist-get (car (e-session-context-generations
                                   reopened session-id))
                             :context-record))
                 (frame-record
                  (plist-get (car (e-session-context-frames reopened session-id))
                             :context-record))
                 (after
                  (e-context-lifetime-project
                   (e-context-lifetime-generation-from-record generation-record)
                   (e-context-lifetime-frame-from-record frame-record)
                   :static-prefix static-prefix
                   :stable-context stable-context)))
            (should (equal before after))
            (should (equal (plist-get before :fingerprint)
                           (plist-get after :fingerprint)))
            (should (eq
                     (plist-get
                      (plist-get (car (plist-get before :checkpoint))
                                 :metadata)
                      :enabled)
                     :json-false))
            (should (eq
                     (plist-get
                      (plist-get (car (plist-get before :ephemeral))
                                 :metadata)
                      :available)
                     :json-false))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-context-lifetime-test-context-record-validation-is-strict ()
  "Append rejects malformed lifetime record shape and lifecycle values."
  (let ((store (e-session-store-create))
        (session-id "session-1"))
    (e-session-create store :id session-id)
    (should-error
     (e-session-append-context-generation
      store session-id
      '(:type context-generation :id "generation-1"
        :checkpoint nil :durable-tail nil))
     :type 'e-session-error)
    (e-session-append-context-generation
     store session-id
     (e-context-lifetime-generation-record
      (e-context-lifetime-test--generation)))
    (should-error
     (e-session-append-context-frame
      store session-id
      '(:record-version 1 :type context-frame :id "frame-1"
        :generation-id "generation-1" :state unknown))
     :type 'e-session-error)
    (should-error
     (e-session-append-context-frame-settlement
      store session-id
      '(:record-version 1 :type context-frame-settlement
        :id "settlement-1" :frame-id "missing-frame" :status failed))
     :type 'e-session-error)))

(ert-deftest e-context-lifetime-test-persistent-replay-rejects-corruption ()
  "A malformed persisted lifetime record cannot enter replay state."
  (let* ((directory (make-temp-file "e-context-lifetime-corrupt-" t))
         (store (e-session-persistent-index-store-create directory))
         (session-id "session-1")
         (journal (expand-file-name "sessions/session-1.jsonl" directory)))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (with-temp-buffer
            (insert
             (json-encode
              '(:type "context-generation"
                :session-id "session-1"
                :id "entry-corrupt"
                :timestamp "2026-01-01T00:00:00Z"
                :context-record
                (:record-version 1 :type "context-generation"
                 :checkpoint [] :durable-tail [])))
             "\n")
            (write-region (point-min) (point-max) journal t 'silent))
          (should-error
           (e-session-persistent-store-create directory)
           :type 'e-session-error))
      (delete-directory directory t))))

(ert-deftest e-context-lifetime-test-promotion-never-copies-observation ()
  "Promotion adds selected facts without making the observation durable."
  (let* ((generation (e-context-lifetime-test--generation))
         (frame (e-context-lifetime-test--frame))
         (promotion
          (e-context-lifetime-promotion-create
           :id "promotion-1"
           :frame-id "frame-1"
           :source-observation-ids '("observation-1")
           :facts '((:role "assistant" :content "first-divergence=normalize-price"))))
         (promoted (e-context-lifetime-apply-promotion generation promotion))
         (projection
          (e-context-lifetime-project
           promoted (e-context-lifetime-frame-consume frame))))
    (should (member "first-divergence=normalize-price"
                    (mapcar (lambda (message) (plist-get message :content))
                            (plist-get projection :messages))))
    (should-not (member "OBSERVATION-ONE"
                        (mapcar (lambda (message) (plist-get message :content))
                                (plist-get projection :messages))))
    (should (equal (e-context-lifetime-promotion-source-observation-ids
                    promotion)
                   '("observation-1")))))

(ert-deftest e-context-lifetime-test-frame-transitions-reject-backward-and-terminal-moves ()
  "Lifecycle helpers only allow forward, non-terminal transitions."
  (let* ((frame (e-context-lifetime-test--frame))
         (consuming (e-context-lifetime-frame-start-consuming frame))
         (consumed (e-context-lifetime-frame-consume consuming))
         (settled (e-context-lifetime-frame-settle consumed))
         (aborted (e-context-lifetime-frame-abort frame)))
    (should (eq (e-context-lifetime-frame-state consuming) 'consuming))
    (should (eq (e-context-lifetime-frame-state consumed) 'consumed))
    (should (eq (e-context-lifetime-frame-state settled) 'settled))
    (should (eq (e-context-lifetime-frame-state aborted) 'aborted))
    (should-error (e-context-lifetime-frame-start-consuming consuming))
    (should-error (e-context-lifetime-frame-consume consumed))
    (should-error (e-context-lifetime-frame-consume settled))
    (should-error (e-context-lifetime-frame-settle frame))
    (should-error (e-context-lifetime-frame-abort settled))
    (should-error (e-context-lifetime-frame-abort aborted))))

(ert-deftest e-context-lifetime-test-shadow-boundary-is-opt-in ()
  "Existing request context is returned unchanged until explicitly enabled."
  (let* ((legacy '(:messages ((:role user :content "legacy"))))
         (generation (e-context-lifetime-test--generation))
         (frame (e-context-lifetime-test--frame)))
    (let ((e-context-lifetime-shadow-projection-enabled nil))
      (should (eq (e-context-lifetime-shadow-context
                   legacy generation frame)
                  legacy)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (let ((projection (e-context-lifetime-shadow-context
                         legacy generation frame)))
        (should (equal (plist-get projection :projection)
                       'generational-context))
        (should-not (eq projection legacy))))))

(ert-deftest e-context-lifetime-test-records-round-trip-through-session-checkpoint ()
  "Active generation and unsettled frame state survive checkpoint replay."
  (let* ((directory (make-temp-file "e-context-lifetime-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "lifetime-session")
         (generation (e-context-lifetime-test--generation))
         (frame (e-context-lifetime-test--frame))
         (promotion
          (e-context-lifetime-promotion-create
           :id "promotion-1" :frame-id "frame-1"
           :facts '((:role assistant :content "selected fact")))))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (e-session-append-context-generation
           store session-id (e-context-lifetime-generation-record generation))
          (e-session-append-context-frame
           store session-id (e-context-lifetime-frame-record frame))
          (e-session-append-context-promotion
           store session-id (e-context-lifetime-promotion-record promotion))
          (let* ((manifest (e-session-checkpoint-manifest store session-id))
                 (state (plist-get manifest :context-lifetime))
                 (checkpoint-generation (plist-get state :generation))
                 (checkpoint-frames (append (plist-get state :frames) nil)))
            (should (equal (plist-get checkpoint-generation :id)
                           "generation-1"))
            (should (= (length checkpoint-frames) 1))
            (should (equal (plist-get (car checkpoint-frames) :id)
                           "frame-1"))
            ;; Entry IDs are the session journal IDs, not the logical frame
            ;; ID carried by the nested context record.
            (should (= (length (append (plist-get state :entry-ids) nil)) 3)))
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (frames (e-session-context-frames reopened session-id))
                 (generations (e-session-context-generations reopened session-id))
                 (promotions (e-session-context-promotions reopened session-id)))
            (should (= (length frames) 1))
            (should (= (length generations) 1))
            (should (= (length promotions) 1))
            (should (equal (plist-get
                           (plist-get (car frames) :context-record) :id)
                           "frame-1"))
            (should (equal (plist-get
                            (plist-get (car frames) :context-record)
                            :consumption-attempt-ids)
                           '("attempt-1")))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-context-lifetime-test-latest-frame-snapshot-is-retained-once ()
  "Repeated lifecycle snapshots reduce to one latest unsettled frame."
  (let ((store (e-session-store-create))
        (session-id "session-1"))
    (e-session-create store :id session-id)
    (e-context-lifetime-test--append-lifecycle store session-id t)
    (let* ((state (e-session-context-lifetime-resume-state store session-id))
           (frames (append (plist-get state :frames) nil))
           (frame (car frames))
           (entry-ids (append (plist-get state :entry-ids) nil)))
      (should (= (length frames) 1))
      (should (eq (plist-get frame :state) 'consumed))
      (should (equal (plist-get frame :observations)
                     '((:content "OBSERVATION-CONSUMED" :role "user"))))
      ;; Generation plus the latest frame, promotion, and failed settlement are
      ;; enough to reconstruct this state; superseded frame entries are not
      ;; retained.
      (should (= (length entry-ids) 4))
      (should (equal (plist-get (aref (plist-get state :promotions) 0) :id)
                     "promotion-1")))))

(ert-deftest e-context-lifetime-test-retains-only-referenced-latest-promotions-and-settlement ()
  "Resume state excludes stale promotions and superseded settlement attempts."
  (let ((store (e-session-store-create))
        (session-id "session-1"))
    (e-session-create store :id session-id)
    (let* ((result (e-context-lifetime-test--append-bounded-retries
                    store session-id))
           (state (plist-get result :state))
           (promotions (append (plist-get state :promotions) nil))
           (settlements (append (plist-get state :settlements) nil)))
      (should (equal (append (plist-get state :entry-ids) nil)
                     (plist-get result :expected-entry-ids)))
      (should (= (length promotions) 1))
      (should (equal (plist-get (car promotions) :id) "promotion-1"))
      (should (equal (plist-get (car (plist-get (car promotions) :facts))
                               :content)
                     "latest fact"))
      (should (= (length settlements) 1))
      (should (equal (plist-get (car settlements) :id) "settlement-2"))
      (should (eq (plist-get (car settlements) :status) 'failed))
      ;; The latest failed marker remains authoritative until a later
      ;; acknowledged marker closes the frame.
      (should (= (length (append (plist-get state :frames) nil)) 1))
      (e-session-append-context-frame-settlement
       store session-id
       '(:record-version 1 :type context-frame-settlement
         :id "settlement-ack" :frame-id "frame-1" :status acknowledged))
      (should-not (append (plist-get
                           (e-session-context-lifetime-resume-state
                            store session-id)
                           :frames)
                          nil)))))

(ert-deftest e-context-lifetime-test-bounded-retry-retention-replays-persistently ()
  "Persistent replay preserves bounded promotion and settlement reduction."
  (let* ((directory (make-temp-file "e-context-lifetime-bounded-" t))
         (memory (e-session-store-create))
         (persistent (e-session-persistent-store-create directory))
         (session-id "session-1"))
    (unwind-protect
        (progn
          (e-session-create memory :id session-id)
          (e-session-create persistent :id session-id)
          (let* ((memory-state
                  (plist-get
                   (e-context-lifetime-test--append-bounded-retries
                    memory session-id)
                   :state))
                 (persistent-state
                  (plist-get
                   (e-context-lifetime-test--append-bounded-retries
                    persistent session-id)
                   :state)))
            (should (equal (plist-get memory-state :frames)
                           (plist-get persistent-state :frames)))
            (should (equal (plist-get memory-state :promotions)
                           (plist-get persistent-state :promotions)))
            (should (equal (plist-get memory-state :settlements)
                           (plist-get persistent-state :settlements))))
          (e-session-flush-write-queue persistent)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (state (e-session-context-lifetime-resume-state
                         reopened session-id))
                 (memory-state
                  (e-session-context-lifetime-resume-state memory session-id)))
            (should (equal (plist-get memory-state :frames)
                           (plist-get state :frames)))
            (should (equal (plist-get memory-state :promotions)
                           (plist-get state :promotions)))
            (should (equal (plist-get memory-state :settlements)
                           (plist-get state :settlements)))
            (e-session-append-context-frame-settlement
             reopened session-id
             '(:record-version 1 :type context-frame-settlement
               :id "settlement-ack" :frame-id "frame-1" :status acknowledged))
            (should-not (append (plist-get
                                 (e-session-context-lifetime-resume-state
                                  reopened session-id)
                                 :frames)
                                nil))
            (e-session-flush-write-queue reopened)
            (let ((replayed-after-ack
                   (e-session-persistent-store-create directory)))
              (should-not (append
                           (plist-get
                            (e-session-context-lifetime-resume-state
                             replayed-after-ack session-id)
                            :frames)
                           nil)))))
      (ignore-errors (e-session-flush-write-queue persistent))
      (delete-directory directory t))))

(ert-deftest e-context-lifetime-test-settled-snapshot-without-marker-remains ()
  "A nested settled snapshot cannot close a frame without its marker."
  (let ((store (e-session-store-create))
        (session-id "session-1"))
    (e-session-create store :id session-id)
    (e-session-append-context-generation
     store session-id
     (e-context-lifetime-generation-record
      (e-context-lifetime-test--generation)))
    (e-session-append-context-frame
     store session-id
     (e-context-lifetime-frame-record
      (e-context-lifetime-test--frame 'settled)))
    (let* ((state (e-session-context-lifetime-resume-state store session-id))
           (frames (append (plist-get state :frames) nil)))
      (should (= (length frames) 1))
      (should (eq (plist-get (car frames) :state) 'settled)))))

(ert-deftest e-context-lifetime-test-final-acknowledged-marker-removes-frame ()
  "Only the final acknowledged settlement marker closes a frame."
  (let ((store (e-session-store-create))
        (session-id "session-1"))
    (e-session-create store :id session-id)
    (e-session-append-context-generation
     store session-id
     (e-context-lifetime-generation-record
      (e-context-lifetime-test--generation)))
    (e-session-append-context-frame
     store session-id
     (e-context-lifetime-frame-record
      (e-context-lifetime-test--frame 'settled)))
    (e-session-append-context-frame-settlement
     store session-id
     '(:record-version 1 :type context-frame-settlement
       :id "settlement-failed" :frame-id "frame-1" :status failed))
    (should (= (length (append (plist-get
                                (e-session-context-lifetime-resume-state
                                 store session-id)
                                :frames)
                               nil))
               1))
    (e-session-append-context-frame-settlement
     store session-id
     '(:record-version 1 :type context-frame-settlement
       :id "settlement-ack" :frame-id "frame-1" :status acknowledged))
    (should-not (append (plist-get
                         (e-session-context-lifetime-resume-state
                          store session-id)
                         :frames)
                        nil))))

(ert-deftest e-context-lifetime-test-explicit-abort-closes-frame ()
  "An explicit durable abort closes a logical frame without settlement."
  (let ((store (e-session-store-create))
        (session-id "session-1"))
    (e-session-create store :id session-id)
    (e-session-append-context-generation
     store session-id
     (e-context-lifetime-generation-record
      (e-context-lifetime-test--generation)))
    (e-session-append-context-frame
     store session-id
     (e-context-lifetime-frame-record
      (e-context-lifetime-test--frame 'open)))
    (e-session-append-context-frame
     store session-id
     (e-context-lifetime-frame-record
      (e-context-lifetime-test--frame 'aborted)))
    (should-not (append (plist-get
                         (e-session-context-lifetime-resume-state
                          store session-id)
                         :frames)
                        nil))))

(ert-deftest e-context-lifetime-test-persistent-replay-matches-in-memory-reducer ()
  "Persistent replay reduces the same lifecycle snapshots as memory."
  (let* ((directory (make-temp-file "e-context-lifetime-reducer-" t))
         (memory (e-session-store-create))
         (persistent (e-session-persistent-store-create directory))
         (session-id "session-1"))
    (unwind-protect
        (progn
          (e-session-create memory :id session-id)
          (e-session-create persistent :id session-id)
          (let ((memory-state
                 (e-context-lifetime-test--append-lifecycle
                  memory session-id t))
                (persistent-state
                 (e-context-lifetime-test--append-lifecycle
                  persistent session-id t)))
            (should (equal (plist-get memory-state :generation)
                           (plist-get persistent-state :generation)))
            (should (equal (plist-get memory-state :frames)
                           (plist-get persistent-state :frames)))
            (should (equal (plist-get memory-state :promotions)
                           (plist-get persistent-state :promotions)))
            (should (equal (plist-get memory-state :settlements)
                           (plist-get persistent-state :settlements))))
          (e-session-flush-write-queue persistent)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (state (e-session-context-lifetime-resume-state
                         reopened session-id))
                 (memory-state
                  (e-session-context-lifetime-resume-state memory session-id)))
            (should (equal (plist-get memory-state :generation)
                           (plist-get state :generation)))
            (should (equal (plist-get memory-state :frames)
                           (plist-get state :frames)))
            (should (equal (plist-get memory-state :promotions)
                           (plist-get state :promotions)))
            (should (equal (plist-get memory-state :settlements)
                           (plist-get state :settlements)))))
      (ignore-errors (e-session-flush-write-queue persistent))
      (delete-directory directory t))))

(ert-deftest e-context-lifetime-test-settled-frame-leaves-only-generation-in-resume-state ()
  "An acknowledged frame settlement removes the frame from resume state."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-context-generation
     store "session-1"
     (e-context-lifetime-generation-record
      (e-context-lifetime-test--generation)))
    (e-session-append-context-frame
     store "session-1"
     (e-context-lifetime-frame-record (e-context-lifetime-test--frame)))
    (e-session-append-context-frame-settlement
     store "session-1"
     '(:record-version 1 :type context-frame-settlement
       :id "settlement-1" :frame-id "frame-1" :status acknowledged))
    (let* ((state (e-session-context-lifetime-resume-state store "session-1"))
           (frames (append (plist-get state :frames) nil)))
      (should (equal (plist-get (plist-get state :generation) :id)
                     "generation-1"))
      (should-not frames))))

(ert-deftest e-context-lifetime-test-settlement-status-survives-persistent-replay ()
  "A JSON-decoded acknowledged settlement still closes its frame on resume."
  (let* ((directory (make-temp-file "e-context-lifetime-settlement-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "session-1"))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (e-session-append-context-generation
           store session-id
           (e-context-lifetime-generation-record
            (e-context-lifetime-test--generation)))
          (e-session-append-context-frame
           store session-id
           (e-context-lifetime-frame-record
            (e-context-lifetime-test--frame)))
          (e-session-append-context-frame-settlement
           store session-id
           '(:record-version 1 :type context-frame-settlement
             :id "settlement-1" :frame-id "frame-1" :status acknowledged))
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (state (e-session-context-lifetime-resume-state
                         reopened session-id)))
            (should-not (append (plist-get state :frames) nil))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-context-lifetime-test-settlement-ack-is-disabled-by-default ()
  "Disabled settlement acknowledgement performs no persistence operation."
  (let (done diagnostics)
    (let ((e-context-lifetime-diagnostics-hook
           (list (lambda (event payload)
                   (push (list event payload) diagnostics)))))
      (e-context-lifetime-acknowledge-settlement
       (e-session-store-create) "session-1"
       :frame-id "frame-1"
       :record-count 2
       :record-bytes 64
       :on-done (lambda (result) (setq done result))))
    (should (eq (plist-get done :status) 'disabled))
    (should (eq (caar diagnostics)
                'settlement-acknowledgement-skipped))
    (should (= (plist-get (cadar diagnostics) :record-count) 2))
    (should-not (plist-member (cadar diagnostics) :observation))))

(ert-deftest e-context-lifetime-test-enabled-in-memory-settlement-ack-is-immediate ()
  "An explicitly enabled in-memory boundary acknowledges without I/O."
  (let (done diagnostics)
    (let ((e-context-lifetime-diagnostics-hook
           (list (lambda (event payload)
                   (push (list event payload) diagnostics)))))
      (e-context-lifetime-acknowledge-settlement
       (e-session-store-create) "session-1"
       :frame-id "frame-1" :enabled t
       :on-done (lambda (result) (setq done result))))
    (should (eq (plist-get done :status) 'acknowledged))
    (should (eq (caar diagnostics) 'settlement-prefix-acknowledged))))

(ert-deftest e-context-lifetime-test-enabled-persistent-ack-requires-controller ()
  "An enabled persistent boundary reports missing async persistence visibly."
  (let (done failure diagnostics)
    (let ((e-context-lifetime-diagnostics-hook
           (list (lambda (event payload)
                   (push (list event payload) diagnostics)))))
      (e-context-lifetime-acknowledge-settlement
       (e-session-store-create :persistent t) "session-1"
       :frame-id "frame-1" :enabled t
       :on-done (lambda (result) (setq done result))
       :on-error (lambda (error) (setq failure error))))
    (should-not done)
    (should (equal (car failure)
                   'e-context-lifetime-settlement-unavailable))
    (should (eq (caar diagnostics)
                'settlement-prefix-acknowledgement-failed))))

(ert-deftest e-context-lifetime-test-enabled-persistent-ack-uses-named-prefix ()
  "A controlled persistence controller gates only the named session prefix."
  (let* ((directory (make-temp-file "e-context-lifetime-controller-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller nil)
         (sent nil)
         (done nil)
         (failure nil))
    (unwind-protect
        (progn
          (e-session-create store :id "session-a")
          (e-session-create store :id "session-b")
          (setq controller (e-session-persistence-enable store))
          (cl-letf (((symbol-function 'e-session-persistence--send-submitted-command)
                     (lambda (_controller command)
                       (push command sent)))
                    ((symbol-function 'display-warning)
                     (lambda (&rest _arguments) nil))
                    ;; Local entry appends are enough for this barrier test;
                    ;; suppress their unrelated checkpoint scheduling.
                    ((symbol-function 'e-session-persistence-request-checkpoint)
                     (lambda (&rest _arguments) nil)))
            (let* ((a1 (e-session-append-message
                        store "session-a"
                        '(:role user :content "a-1")))
                   (a2 (e-session-append-message
                        store "session-a"
                        '(:role user :content "a-2")))
                   (b1 (e-session-append-message
                        store "session-b"
                        '(:role user :content "b-1")))
                   (a1-id (plist-get a1 :id))
                   (a2-id (plist-get a2 :id))
                   (b1-id (plist-get b1 :id))
                   (a1-command
                    (gethash (list "session-a" a1-id)
                             (e-session-persistence-record-command-ids
                              controller)))
                   (a2-command
                    (gethash (list "session-a" a2-id)
                             (e-session-persistence-record-command-ids
                              controller)))
                   (b1-command
                    (gethash (list "session-b" b1-id)
                             (e-session-persistence-record-command-ids
                              controller))))
              (e-context-lifetime-acknowledge-settlement
               store "session-a"
               :frame-id "frame-a"
               :prefix-entry-ids (list a1-id)
               :enabled t
               :on-done (lambda (result) (setq done result))
               :on-error (lambda (error) (setq failure error)))
              (should-not done)
              (e-session-persistence--handle-response
               controller
               (list :id b1-command :ok :json-false :retryable :json-false))
              (should-not done)
              (should-not failure)
              (e-session-persistence--handle-response
               controller (list :id a2-command :ok t :result 'written))
              (should-not done)
              (should-not failure)
              (e-session-persistence--handle-response
               controller (list :id a1-command :ok t :result 'written))
              (should (eq (plist-get done :status) 'acknowledged))
              (should (equal (plist-get done :prefix-entry-ids) (list a1-id)))
              (should-not failure)
              (should-not
               (seq-some
                (lambda (command)
                  (member (plist-get
                           (e-session-persistence-command-request command) :op)
                          '("checkpoint" "reindex")))
                sent)))))
      (when-let ((process (and controller
                              (e-session-persistence-process controller))))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(provide 'e-context-lifetime-test)

;;; e-context-lifetime-test.el ends here
