;;; e-harness-base-test.el --- Tests for harness-base layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for harness-owned support capabilities.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-harness-base)
(require 'e-hooks)
(require 'e-operations)
(require 'e-resources)
(require 'e-session)
(require 'e-session-tmp-resources)

(declare-function e-harness-base-layer-create "e-harness-base")

(defun e-harness-base-test--tmp-read-method-p (resources)
  "Return non-nil when RESOURCES includes a tmp:// read method."
  (cl-some (lambda (method)
             (equal (e-resource-method-scheme method) "tmp"))
           (e-resources-methods-for-operation resources e-operation-read)))

(defun e-harness-base-test--raw-result-read-method-p (resources)
  "Return non-nil when RESOURCES includes a raw-result:// read method."
  (cl-some (lambda (method)
             (equal (e-resource-method-scheme method) "raw-result"))
           (e-resources-methods-for-operation resources e-operation-read)))

(defun e-harness-base-test--tmp-operation-ids (resources)
  "Return tmp:// operation ids in RESOURCES."
  (mapcar #'e-operation-id
          (seq-filter
           (lambda (operation)
             (e-resources-methods-for-operation resources operation))
           (e-resources-operations resources))))

(defun e-harness-base-test--emit-receipt
    (harness session-id turn-id call-id tool details-uri &optional purpose)
  "Append one compact receipt-producing TOOL-FINISHED activity event."
  (let ((e-harness--trusted-tool-details-uri details-uri))
    (e-harness--emit-turn-event
     harness session-id turn-id 'tool-finished
     (list :tool-call (list :id call-id
                            :name tool
                            :stated-purpose (or purpose
                                               "Inspect the bounded result.")
                            :arguments '(:secret "must-not-project"))
           :result (list :tool-call-id call-id
                         :name tool
                         :status 'ok
                         :content "must-not-project")))))

(defun e-harness-base-test--receipt-content (projection)
  "Return the sole receipt block content from PROJECTION."
  (plist-get (car (plist-get projection :messages)) :content))

(ert-deftest e-harness-base-test-require-and-create-layer ()
  "The harness-base layer bundles harness-owned support capabilities."
  (should (require 'e-harness-base nil t))
  (let ((layer (e-harness-base-layer-create)))
    (should (eq (e-layer-id layer) 'harness-base))
    (should (equal (e-layer-name layer) "Harness Base"))
    (should (equal (mapcar #'e-capability-id
                           (e-layer-capabilities layer))
                   '(harness-base-context
                     raw-result-resources
                     session-tmp-resources
                     session-resources
                     tool-invocation-details
                     tool-output-truncation)))))

(ert-deftest e-harness-base-test-context-asks-for-novel-reasoning-messages ()
  "The harness-base layer asks for reasoning messages only when they add value."
  (should (require 'e-harness-base nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (e-harness-base-layer-create)))
    (e-harness-set-intrinsic-capabilities
     harness (e-layer-capabilities layer))
    (e-harness-create-session harness :id "session-1")
    (let* ((context (e-harness-context harness "session-1" "turn-1"))
           (messages (plist-get context :messages))
           (system-texts (mapcar (lambda (message)
                                   (plist-get message :content))
                                 messages)))
      (should (cl-some
               (lambda (text)
                 (and (stringp text)
                      (string-match-p
                       "reasoning explicitly and concretely"
                       text)
                      (string-match-p "without unnecessary detail" text)
                      (string-match-p "changes what the user can understand" text)
                      (string-match-p "distinct phase begins" text)
                      (string-match-p "new evidence narrows" text)
                      (string-match-p "Do not send an update for every command" text)
                      (string-match-p "repeat the same reason" text)
                      (not (string-match-p "as you work" text))))
               system-texts)))))

(ert-deftest e-harness-base-test-receipt-provider-is-dynamic-and-bounded ()
  "Receipts form a late dynamic block with measured count and byte bounds."
  (should (require 'e-harness-base nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create))))
         (session-id "receipt-bounds")
         (provider
          (car (e-capability-context-providers
                (e-harness-base-context-capability-create))))
         uris)
    (e-harness-create-session harness :id session-id)
    (dotimes (index 3)
      (let ((uri (format "tmp://details/%d.json" index)))
        (push uri uris)
        (e-harness-base-test--emit-receipt
         harness session-id (format "turn-%d" index)
         (format "call-%d" index) "probe" uri)))
    (setq uris (nreverse uris))
    (should (eq (e-context-provider-cache-placement provider)
                'dynamic-context))
    (let* ((exact-count
            (e-harness-base-receipt-projection
             harness session-id :max-entries 3 :max-bytes 4096))
           (one-over-count
            (e-harness-base-receipt-projection
             harness session-id :max-entries 2 :max-bytes 4096))
           (count-one-over
            (e-harness-base-receipt-projection
             harness session-id :max-entries 4 :max-bytes 4096))
           (exact-bytes (plist-get exact-count :bytes))
           (one-over-bytes
            (e-harness-base-receipt-projection
             harness session-id :max-entries 3
             :max-bytes (1+ exact-bytes)))
           (one-under-bytes
            (e-harness-base-receipt-projection
             harness session-id :max-entries 3
             :max-bytes (1- exact-bytes)))
           (too-small
            (e-harness-base-receipt-projection
             harness session-id :max-entries 3 :max-bytes 1)))
      (should (= (plist-get exact-count :selected-count) 3))
      (should (= (plist-get exact-count :omitted-count) 0))
      (should (= (plist-get one-over-count :selected-count) 2))
      (should (= (plist-get one-over-count :omitted-count) 1))
      (should (= (plist-get count-one-over :selected-count) 3))
      (should (= (plist-get count-one-over :omitted-count) 0))
      (should (= (plist-get one-over-bytes :selected-count) 3))
      (should (= (plist-get one-over-bytes :omitted-count) 0))
      (should (= (plist-get one-over-bytes :bytes) exact-bytes))
      (should (<= (plist-get one-under-bytes :bytes) (1- exact-bytes)))
      (should (> (plist-get one-under-bytes :omitted-count) 0))
      (should (= (plist-get too-small :bytes) 0))
      (should-not (plist-get too-small :messages))
      (should (equal
               (mapcar (lambda (receipt) (plist-get receipt :tool-call-id))
                       (plist-get one-over-count :receipts))
               '("call-1" "call-2")))
      (should (string-match-p "earlier tool receipt"
                              (e-harness-base-test--receipt-content
                               one-over-count))))))

(ert-deftest e-harness-base-test-receipt-provider-filters-erasure-before-aggregates ()
  "Erased identities disappear before ordering, bounds, and omitted counts."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create))))
         (session-id "receipt-erasure"))
    (e-harness-create-session harness :id session-id)
    (dolist (id '("call-a" "call-b" "call-c"))
      (e-harness-base-test--emit-receipt
       harness session-id id id "probe"
       (format "tmp://details/%s.json" id)))
    (let ((projection
           (e-harness-base-receipt-projection
            harness session-id
            :erased-tool-call-ids '("call-b")
            :max-entries 2
            :max-bytes 4096)))
      (should (= (plist-get projection :total-count) 2))
      (should (= (plist-get projection :omitted-count) 0))
      (should (equal
               (mapcar (lambda (receipt) (plist-get receipt :tool-call-id))
                       (plist-get projection :receipts))
               '("call-a" "call-c")))
      (should-not (string-match-p "call-b"
                                  (e-harness-base-test--receipt-content
                                   projection))))))

(ert-deftest e-harness-base-test-receipt-erasure-membership-is-linear ()
  "Receipt filtering performs one equal-set membership operation per receipt."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create))))
         (session-id "receipt-erasure-linear")
         (count 64)
         (lookups 0)
         (original (symbol-function 'e-harness-base--receipt-erased-p)))
    (e-harness-create-session harness :id session-id)
    (dotimes (index count)
      (e-harness-base-test--emit-receipt
       harness session-id (format "turn-%d" index)
       (format "call-%d" index) "probe"
       (format "tmp://details/%d.json" index)))
    (cl-letf (((symbol-function 'e-harness-base--receipt-erased-p)
               (lambda (erased-set id)
                 (setq lookups (1+ lookups))
                 (funcall original erased-set id))))
      (e-harness-base-receipt-projection
       harness session-id
       :erased-tool-call-ids
       (mapcar (lambda (index) (format "call-%d" index))
               (number-sequence 0 (1- count)))
       :max-entries count
       :max-bytes 100000))
    (should (= lookups count))))

(ert-deftest e-harness-base-test-receipt-provider-reads-session-erasure-authority ()
  "The dynamic provider reads erased ids from the active session path."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create))))
         (session-id "receipt-session-erasure")
         (store (e-harness-sessions harness)))
    (e-harness-create-session harness :id session-id)
    (dolist (id '("call-a" "call-b"))
      (e-harness-base-test--emit-receipt
       harness session-id id id "probe"
       (format "tmp://details/%s.json" id)))
    (let* ((session (e-session-get store session-id))
           (root-id (plist-get session :root-event-id))
           (generation-id "generation-receipt-session")
           (generation
            (e-context-lifetime-generation-create
             :id generation-id
             :checkpoint nil
             :covered-session-boundary root-id))
           (erasure
            (list :record-version 1
                  :type 'context-erasure
                  :id "erasure-receipt-session"
                  :frame-id "frame-receipt-session"
                  :generation-id generation-id
                  :consumer-request-id "consumer-receipt-session"
                  :response-entry-id "response-receipt-session"
                  :sources
                  '((:source-observation-id "observation-call-b"
                     :source-ref "context-source:call-b"
                     :source-fingerprint "fingerprint-call-b"
                     :tool-call-id "call-b"))))
           (provider
            (car (e-capability-context-providers
                  (e-harness-base-context-capability-create)))))
      (e-session-append-context-generation store session-id generation)
      (e-session-append-context-curation-package
       store session-id (list :promotion nil :erasure erasure))
      (let* ((messages
              (e-context-provider-build
               provider :harness harness :session-id session-id
               :turn-id "turn-receipt-session" :context-purpose 'turn))
             (content (plist-get (car messages) :content)))
        (should (equal (e-session-erased-tool-call-ids store session-id)
                       '("call-b")))
        (should (string-match-p "call-a" content))
        (should-not (string-match-p "call-b" content))
        (should (= (length messages) 1))))))

(ert-deftest e-harness-base-test-receipt-provider-renders-resource-liveness ()
  "Receipt context distinguishes a readable detail resource from an expired one."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create)
                         (e-session-tmp-capability-create))))
         (session-id "receipt-liveness"))
    (e-harness-create-session harness :id session-id)
    (let ((uri (e-session-tmp-write
                harness session-id "details/live.json" "{}")))
      (e-harness-base-test--emit-receipt
       harness session-id "turn-live" "call-live" "probe" uri)
      (e-harness-base-test--emit-receipt
       harness session-id "turn-missing" "call-missing" "probe"
       "tmp://details/missing.json")
      (let* ((projection (e-harness-base-receipt-projection
                          harness session-id :max-entries 2 :max-bytes 4096))
             (content (e-harness-base-test--receipt-content projection)))
        (should (string-match-p "\"details\":\"available\"" content))
        (should (string-match-p "\"details\":\"unavailable\"" content))
        (should (string-match-p "tmp://details/live.json" content))
        (should (string-match-p "tmp://details/missing.json" content))))))

(ert-deftest e-harness-base-test-receipt-provider-defaults-cover-wide-receipts ()
  "Measured defaults retain a representative eight-item, long-purpose tail."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create))))
         (session-id "receipt-defaults")
         (purpose (make-string 200 ?p)))
    (e-harness-create-session harness :id session-id)
    (dotimes (index e-harness-base-receipt-max-entries)
      (e-harness-base-test--emit-receipt
       harness session-id (format "wide-turn-%d" index)
       (format "wide-call-%d" index) "probe"
       (format "tmp://details/wide-%d.json" index) purpose))
    (let ((projection (e-harness-base-receipt-projection
                       harness session-id)))
      (should (= e-harness-base-receipt-max-entries 8))
      (should (= e-harness-base-receipt-max-bytes 4096))
      (should (= (plist-get projection :total-count) 8))
      (should (= (plist-get projection :selected-count) 8))
      (should (= (plist-get projection :omitted-count) 0))
      (should (<= (plist-get projection :bytes)
                  e-harness-base-receipt-max-bytes)))))

(ert-deftest e-harness-base-test-receipt-provider-uses-selected-path-only ()
  "A sibling session's activity does not enter the selected session path."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create))))
         (session-id "receipt-source"))
    (e-harness-create-session harness :id session-id)
    (e-harness-base-test--emit-receipt
     harness session-id "turn-before-fork" "call-before-fork" "probe"
     "tmp://details/before.json")
    (e-session-append-message
     (e-harness-sessions harness) session-id
     '(:role user :content "fork boundary"))
    (let* ((fork (e-harness-fork-session harness session-id))
           (fork-id (plist-get fork :id)))
      (e-harness-base-test--emit-receipt
       harness session-id "turn-source" "call-source" "probe"
       "tmp://details/source.json")
      (e-harness-base-test--emit-receipt
       harness fork-id "turn-fork" "call-fork" "probe"
       "tmp://details/fork.json")
      (let ((source (e-harness-base-receipt-projection
                     harness session-id :max-entries 8 :max-bytes 4096))
            (fork-projection (e-harness-base-receipt-projection
                              harness fork-id :max-entries 8 :max-bytes 4096)))
        (should (string-match-p "call-source"
                                (e-harness-base-test--receipt-content source)))
        (should-not (string-match-p "call-fork"
                                    (e-harness-base-test--receipt-content source)))
        (should (string-match-p "call-fork"
                                (e-harness-base-test--receipt-content
                                 fork-projection)))
        (should-not (string-match-p "call-source"
                                    (e-harness-base-test--receipt-content
                                     fork-projection)))))))

(ert-deftest e-harness-base-test-receipt-provider-survives-reopen-checkpoint ()
  "A checkpoint and persistent reopen retain the bounded receipt tail."
  (let* ((directory (make-temp-file "e-harness-receipt-checkpoint-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create))))
         (session-id "receipt-checkpoint"))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id session-id)
          (e-harness-base-test--emit-receipt
           harness session-id "turn-checkpoint" "call-checkpoint" "probe"
           "tmp://details/checkpoint.json")
          (e-session--write-session-checkpoint-now store session-id)
          (let* ((reopened-store (e-session-persistent-store-create directory))
                 (reopened (e-harness-create
                            :backend (e-backend-fake-create :items nil)
                            :sessions reopened-store
                            :intrinsic-capabilities
                            (list (e-harness-base-context-capability-create))))
                 (projection (e-harness-base-receipt-projection
                              reopened session-id
                              :max-entries 8 :max-bytes 4096)))
            (should (= (plist-get projection :total-count) 1))
            (should (string-match-p
                     "call-checkpoint"
                     (e-harness-base-test--receipt-content projection)))
            (should-not (string-match-p
                         "must-not-project"
                         (e-harness-base-test--receipt-content projection)))))
      (delete-directory directory t))))

(ert-deftest e-harness-base-test-receipt-marker-pins-past-activity-tail ()
  "A receipt marker survives checkpoint tail eviction and persistent reopen."
  (let* ((directory (make-temp-file "e-harness-receipt-tail-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create))))
         (session-id "receipt-tail"))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id session-id)
          (e-harness-base-test--emit-receipt
           harness session-id "turn-receipt" "call-receipt" "probe"
           "tmp://details/receipt.json")
          (let* ((receipt-event
                  (seq-find
                   (lambda (entry)
                     (and (eq (plist-get entry :event-type) 'tool-finished)
                          (plist-get (plist-get entry :payload) :receipt)))
                   (e-session-activity-events store session-id)))
                 (receipt-id (plist-get receipt-event :id)))
            (should (plist-get receipt-event :checkpoint-retain))
            (e-harness--emit-turn-event
             harness session-id "turn-nested" 'tool-finished
             (list :nested t
                   :tool-call (list :id "nested-call" :name "probe")
                   :result (list :tool-call-id "nested-call"
                                 :name "probe"
                                 :status 'ok)))
            (dotimes (index 70)
              (e-session-append-activity-event
               store session-id (format "turn-%d" index)
               'tool-progress (list :index index)))
            (let* ((records (e-session--checkpoint-records store session-id))
                   (activity-records
                    (seq-filter
                     (lambda (record)
                       (equal (plist-get record :type) "activity-event"))
                     records))
                   (retained-ids
                    (mapcar (lambda (record) (plist-get record :id))
                            activity-records)))
              (should (= (length activity-records) 65))
              (should (member receipt-id retained-ids))
              (should-not
               (seq-find
                (lambda (record)
                  (equal (plist-get (plist-get record :payload)
                                    :tool-call)
                         '(:id "nested-call" :name "probe")))
                activity-records)))
            (e-session--write-session-checkpoint-now store session-id)
            (let* ((reopened-store (e-session-persistent-store-create directory))
                   (reopened (e-harness-create
                              :backend (e-backend-fake-create :items nil)
                              :sessions reopened-store
                              :intrinsic-capabilities
                              (list (e-harness-base-context-capability-create))))
                   (projection (e-harness-base-receipt-projection
                                reopened session-id
                                :max-entries 8 :max-bytes 4096)))
              (should (e-session-entry-by-id
                       reopened-store session-id receipt-id))
              (should (string-match-p
                       "call-receipt"
                       (e-harness-base-test--receipt-content projection))))))
      (delete-directory directory t))))

(ert-deftest e-harness-base-test-receipt-marker-survives-compaction-boundary ()
  "Marked activity before a compaction suffix remains checkpointed."
  (let* ((directory (make-temp-file "e-harness-receipt-compaction-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create))))
         (session-id "receipt-compaction"))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id session-id)
          (e-session-append-message
           store session-id '(:role user :content "old context"))
          (e-harness-base-test--emit-receipt
           harness session-id "turn-receipt" "call-before-compaction" "probe"
           "tmp://details/compaction.json")
          (let ((boundary
                 (e-session-append-message
                  store session-id
                  '(:role user :content "kept boundary"))))
            (e-session-append-compaction
             store session-id "compacted"
             :first-kept-entry-id (plist-get boundary :id)))
          (let* ((records (e-session--checkpoint-records store session-id))
                 (receipt-record
                  (seq-find
                   (lambda (record)
                     (and (equal (plist-get record :type) "activity-event")
                          (plist-get record :checkpoint-retain)))
                   records)))
            (should receipt-record)
            (should (plist-get receipt-record :checkpoint-retain)))
          (e-session--write-session-checkpoint-now store session-id)
          (let* ((reopened-store (e-session-persistent-store-create directory))
                 (reopened (e-harness-create
                            :backend (e-backend-fake-create :items nil)
                            :sessions reopened-store
                            :intrinsic-capabilities
                            (list (e-harness-base-context-capability-create))))
                 (projection (e-harness-base-receipt-projection
                              reopened session-id
                              :max-entries 8 :max-bytes 4096)))
            (should (string-match-p
                     "call-before-compaction"
                     (e-harness-base-test--receipt-content projection)))
            (should-not (string-match-p "old context"
                                        (prin1-to-string
                                         (e-session-messages
                                          reopened-store session-id))))))
      (delete-directory directory t))))

(ert-deftest e-harness-base-test-receipt-provider-skips-snapshots-and-stable-prefix ()
  "Receipt context is absent from snapshots and does not alter stable prefix."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-harness-base-context-capability-create))))
         (session-id "receipt-frontier"))
    (e-harness-create-session harness :id session-id)
    (let* ((before (e-harness-context harness session-id "turn-before"))
           (before-static
            (mapcar (lambda (segment) (plist-get segment :fingerprint))
                    (seq-filter
                     (lambda (segment)
                       (eq (plist-get segment :kind) 'static-prefix))
                     (plist-get before :segments)))))
      (e-harness-base-test--emit-receipt
       harness session-id "turn-receipt" "call-receipt" "probe"
       "tmp://details/missing.json")
      (let* ((turn-context (e-harness-context harness session-id "turn-after"))
             (snapshot (e-harness-context harness session-id nil 'snapshot))
             (segments (plist-get turn-context :segments))
             (receipt-segment
              (seq-find
               (lambda (segment)
                 (and (eq (plist-get segment :kind) 'current-state)
                      (string-match-p
                       "tool_call_id"
                       (or (plist-get (car (plist-get segment :messages))
                                     :content)
                           ""))))
               segments))
             (after-static
              (mapcar (lambda (segment) (plist-get segment :fingerprint))
                      (seq-filter
                       (lambda (segment)
                         (eq (plist-get segment :kind) 'static-prefix))
                       (plist-get turn-context :segments)))))
        (should receipt-segment)
        (should (equal before-static after-static))
        (should-not (string-match-p "tool_call_id"
                                    (prin1-to-string snapshot)))))))

(ert-deftest e-harness-base-test-activation-adds-tmp-resource-and-hook ()
  "Activating harness-base exposes raw result resources and the post-tool hook."
  (should (require 'e-harness-base nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (e-harness-base-layer-create)))
    (e-harness-set-intrinsic-capabilities
     harness (e-layer-capabilities layer))
    (should (e-harness-base-test--raw-result-read-method-p
             (e-harness-resources harness "session-1" "turn-1")))
    (should (e-harness-base-test--tmp-read-method-p
             (e-harness-resources harness "session-1" "turn-1")))
    (should (equal (e-harness-base-test--tmp-operation-ids
                    (e-harness-resources harness "session-1" "turn-1"))
                   '(read write edit glob search)))
    (should (equal (mapcar #'e-hook-id
                           (e-hooks-for-point
                            (e-harness-hooks harness)
                            :invocation-details))
                   '("40-tool-invocation-details")))
    (should (equal (mapcar #'e-hook-id
                           (e-hooks-for-point
                            (e-harness-hooks harness)
                            :tool-result-presentation))
                   '("50-tool-output-truncation")))
    (e-harness-set-intrinsic-capabilities harness nil)
    (should-not (e-harness-base-test--raw-result-read-method-p
                 (e-harness-resources harness "session-1" "turn-1")))
    (should-not (e-harness-base-test--tmp-read-method-p
                 (e-harness-resources harness "session-1" "turn-1")))
    (should-not (e-hooks-for-point
                 (e-harness-hooks harness)
                 :invocation-details))
    (should-not (e-hooks-for-point
                 (e-harness-hooks harness)
                 :tool-result-presentation))))

(provide 'e-harness-base-test)

;;; e-harness-base-test.el ends here
