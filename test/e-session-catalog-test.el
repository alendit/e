;;; e-session-catalog-test.el --- Direct checkpoint/catalog contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The catalog is a value projection owner.  These tests construct semantic
;; session values directly and verify bounded selection, context ownership,
;; and detached wire projections without loading the facade, storage adapter,
;; or aggregate state machinery.  Restart/recovery behavior is covered by the
;; composed e-session suite.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-session-catalog)

(defun e-session-catalog-test--entry
    (type id parent &optional fields timestamp)
  "Return a semantic catalog ENTRY with TYPE, ID, and PARENT."
  (append (list :type type :id id :parent-id parent
                :created-at (or timestamp "2026-08-29T00:00:00Z"))
          fields))

(defun e-session-catalog-test--session ()
  "Return a representative detached semantic session projection."
  (let* ((root (e-session-catalog-test--entry
                'session-event "root" nil
                '(:event-type session-created)
                "2026-08-29T00:00:00Z"))
         (message-1 (e-session-catalog-test--entry
                     'message "message-1" "root"
                     '(:role user :content "prompt")
                     "2026-08-29T00:00:01Z"))
         (message-2 (e-session-catalog-test--entry
                     'message "message-2" "message-1"
                     '(:role assistant :content "answer")
                     "2026-08-29T00:00:02Z"))
         (compaction (e-session-catalog-test--entry
                      'compaction "compaction-1" "message-2"
                      '(:summary "bounded summary"
                        :first-kept-entry-id "message-2")
                      "2026-08-29T00:00:03Z"))
         (generation
          (e-session-catalog-test--entry
           'context-generation "generation-1" "compaction-1"
           '(:context-record
             (:record-version 2 :type context-generation :id "generation-1"
              :checkpoint ((:role system :content "policy"))))
           "2026-08-29T00:00:04Z"))
         (promotion
          (e-session-catalog-test--entry
           'context-promotion "promotion-1" "generation-1"
           '(:context-record
             (:record-version 2 :type context-promotion :id "promotion-1"
              :generation-id "generation-1"
              :items ((:kind summary :text "meaning"))))
           "2026-08-29T00:00:05Z"))
         (curation
          (e-session-catalog-test--entry
           'context-curation-package "curation-1" "promotion-1"
           '(:promotion
             (:record-version 3 :type context-promotion :id "promotion-1"
              :generation-id "generation-1"
              :items ((:kind summary :text "meaning")))
             :erasure
             (:record-version 1 :type context-erasure :id "erasure-1"
              :generation-id "generation-1"
              :response-entry-id "response-1"
              :sources
              ((:source-observation-id "observation-1"
                :source-ref "external:tool:1"
                :source-fingerprint "fingerprint-1"
                :tool-call-id "tool-call-1"))))
           "2026-08-29T00:00:06Z"))
         (response (e-session-catalog-test--entry
                   'activity-event "response-1" "curation-1"
                   '(:turn-id "turn-1"
                     :event-type context-curation-response
                     :payload (:response-entry-id "response-1"))
                   "2026-08-29T00:00:07Z")))
    (list :id "catalog-session"
          :root-event-id "root"
          :current-head-id "response-1"
          :created-at "2026-08-29T00:00:00Z"
          :updated-at "2026-08-29T00:00:07Z"
          :updated-seq 7
          :metadata '(:name "Catalog session")
          :name "Catalog session"
          :summary "prompt"
          :message-count 2
          :last-message-at "2026-08-29T00:00:02Z"
          :session-events (list root)
          :messages (list message-1 message-2)
          :activity-events (list response)
          :compactions (list compaction)
          :context-generations (list generation)
          :context-promotions (list promotion)
          :context-curation-packages (list curation)
          :provider-anchors nil
          :branch-summaries nil
          :process-reports nil
          :board-session-state nil
          :turn-options nil)))

(ert-deftest e-session-catalog-test-checkpoint-path-selects-valid-compaction ()
  "A valid compaction selects the resumable path suffix."
  (let* ((session (e-session-catalog-test--session))
         (path (e-session-catalog--checkpoint-path-suffix session)))
    (should (equal (mapcar (lambda (entry) (plist-get entry :id)) path)
                   '("message-2" "compaction-1" "generation-1"
                     "promotion-1" "curation-1" "response-1")))))

(ert-deftest e-session-catalog-test-checkpoint-manifest-bounds-board-messages ()
  "Checkpoint manifests retain recent board messages and durable facts."
  (let* ((session (e-session-catalog-test--session))
         (messages
          (append
           (mapcar (lambda (index)
                     (list :id (format "fact-%d" index) :kind 'fact))
                   (number-sequence 0 259))
           (mapcar (lambda (index)
                     (list :id (format "message-%d" index) :kind 'activity))
                   (number-sequence 0 299))))
         (manifest (e-session-catalog-checkpoint-manifest session messages))
         (ids (mapcar (lambda (item) (plist-get item :id))
                      (append (plist-get manifest :board-message-identities)
                              nil))))
    (should (= (length ids) 512))
    (should (member "fact-4" ids))
    (should (member "fact-259" ids))
    (should (member "message-299" ids))))

(ert-deftest e-session-catalog-test-context-state-retains-generation-owners ()
  "Context projections retain the active generation and curation owners."
  (let* ((manifest (e-session-catalog-checkpoint-manifest
                    (e-session-catalog-test--session)))
         (context (plist-get manifest :context-lifetime)))
    (should (equal (plist-get (plist-get context :generation) :id)
                   "generation-1"))
    (should (= (length (plist-get context :generations)) 1))
    (should (= (length (plist-get context :promotions)) 2))
    (should (= (length (plist-get context :erasures)) 1))
    (should (equal (aref (plist-get context :entry-ids) 0) "generation-1"))))

(ert-deftest e-session-catalog-test-checkpoint-records-are-detached-wire-values ()
  "Checkpoint records are detached and use the codec's semantic mapping."
  (let* ((session (e-session-catalog-test--session))
         (records (e-session-catalog--checkpoint-records session))
         (root (car records))
         (first-entry (cadr records)))
    (should (= (length records) 7))
    (should (equal (plist-get root :type) "session"))
    (should (equal (plist-get first-entry :type) "message"))
    (should (equal (plist-get first-entry :parent-id) "root"))
    (should (equal (plist-get (plist-get first-entry :message) :id)
                   "message-2"))
    (let ((name (plist-get (plist-get session :metadata) :name)))
      (setcar (cdr (plist-get session :metadata)) "mutated")
      (should (equal name "Catalog session")))))

(ert-deftest e-session-catalog-test-index-sort-uses-message-time-and-sequence ()
  "Index projections sort by latest message and then durable sequence."
  (let ((entries
         (list (list :id "older" :last-message-at "2026-08-29T00:00:01Z"
                     :updated-seq 4)
               (list :id "newer" :last-message-at "2026-08-29T00:00:02Z"
                     :updated-seq 2)
               (list :id "tie-low" :last-message-at "2026-08-29T00:00:02Z"
                     :updated-seq 1))))
    (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                           (e-session-catalog-sort-index-entries entries))
                   '("newer" "tie-low" "older")))))

(ert-deftest e-session-catalog-test-checkpoint-validation-is-value-only ()
  "Recovery-shape policy is testable without storage or aggregate state."
  (let ((checkpoint '(:version 1 :session-id "catalog-session"
                      :journal-byte-offset 0 :records nil)))
    (should (e-session-catalog-checkpoint-valid-p
             checkpoint "catalog-session"))
    (should-not (e-session-catalog-checkpoint-valid-p
                 (plist-put (copy-sequence checkpoint) :version 2)
                 "catalog-session"))
    (should-not (e-session-catalog-checkpoint-valid-p
                 (plist-put (copy-sequence checkpoint) :journal-byte-offset -1)
                 "catalog-session"))))

(provide 'e-session-catalog-test)

;;; e-session-catalog-test.el ends here
