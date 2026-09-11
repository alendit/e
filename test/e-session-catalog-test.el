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
          :turn-options nil)))

(defun e-session-catalog-test--linear-session (count)
  "Return a COUNT-entry session exercising every path membership projection."
  (let ((messages nil)
        (parent "root"))
    (dotimes (index (1- count))
      (let ((id (format "linear-message-%04d" index)))
        (push (e-session-catalog-test--entry
               'message id parent
               (list :role (if (zerop (% index 2)) 'user 'assistant)
                     :content id)
               (format "2026-08-29T00:%02d:%02dZ"
                       (/ index 60) (% index 60)))
              messages)
        (setq parent id)))
    (setq messages (nreverse messages))
    (list :id "linear-session"
          :root-event-id "root"
          :current-head-id parent
          :created-at "2026-08-29T00:00:00Z"
          :updated-at "2026-08-29T01:00:00Z"
          :updated-seq count
          :metadata nil
          :name "Linear session"
          :summary "linear"
          :message-count (1- count)
          :last-message-at "2026-08-29T01:00:00Z"
          :session-events
          (list (e-session-catalog-test--entry
                 'session-event "root" nil
                 '(:event-type session-created)
                 "2026-08-29T00:00:00Z"))
          :messages messages
          :activity-events
          (mapcar (lambda (entry)
                    (list :type 'activity-event
                          :id (plist-get entry :id)
                          :parent-id (plist-get entry :parent-id)
                          :event-type 'reasoning-delta
                          :checkpoint-retain t))
                  messages)
          :branch-summaries nil
          :compactions nil
          :provider-anchors
          (mapcar (lambda (entry)
                    (list :id (plist-get entry :id)
                          :covered-entry-id (plist-get entry :id)))
                  messages)
          :process-reports
          (mapcar (lambda (entry)
                    (list :id (plist-get entry :id)))
                  messages)
          :context-generations nil
          :context-promotions nil
          :context-curation-packages nil
          :latest-token-usage-event nil
          :turn-options nil)))

(ert-deftest e-session-catalog-test-duplicate-ids-preserve-first-match ()
  "Indexed paths retain the historical first equal-ID entry."
  (let* ((baseline (e-session-catalog-test--session))
         (duplicate
          (e-session-catalog-test--entry
           'message "message-2" "root"
           '(:role system :content "unreachable duplicate")
           "2026-08-29T00:00:20Z"))
         (with-duplicate
          (plist-put (copy-tree baseline) :messages
                     (append (plist-get baseline :messages)
                             (list duplicate)))))
    (should (equal (e-session-catalog-checkpoint-manifest baseline)
                   (e-session-catalog-checkpoint-manifest with-duplicate)))
    (should (equal (e-session-catalog--checkpoint-records baseline)
                   (e-session-catalog--checkpoint-records with-duplicate)))
    (should (equal (e-session-catalog-checkpoint-json baseline 17)
                   (e-session-catalog-checkpoint-json with-duplicate 17))))
  (let* ((session (e-session-catalog-test--session))
         (cross-field-first
          (e-session-catalog-test--entry
           'session-event "cross-field" "root"
           '(:event-type session-info :metadata (:name "first"))))
         (cross-field-second
          (e-session-catalog-test--entry
           'message "cross-field" "missing"
           '(:role user :content "second")))
         (session (plist-put (copy-tree session) :session-events
                             (cons cross-field-first
                                   (plist-get session :session-events))))
         (session (plist-put session :messages
                             (cons cross-field-second
                                   (plist-get session :messages))))
         (session (plist-put session :current-head-id "cross-field"))
         (path (e-session-catalog--path session)))
    (should (eq (car (last path)) cross-field-first)))
  (let* ((session (e-session-catalog-test--session))
         (within-first
          (e-session-catalog-test--entry
           'message "within-field" "root"
           '(:role user :content "first")))
         (within-second
          (e-session-catalog-test--entry
           'message "within-field" "missing"
           '(:role user :content "second")))
         (session (plist-put (copy-tree session) :messages
                             (cons within-first
                                   (cons within-second
                                         (plist-get session :messages)))))
         (session (plist-put session :current-head-id "within-field"))
         (path (e-session-catalog--path session)))
    (should (eq (car (last path)) within-first))))

(ert-deftest e-session-catalog-test-missing-head-signals ()
  "A checkpoint projection rejects a head that is not in the catalog."
  (let ((session (plist-put (copy-tree (e-session-catalog-test--session))
                            :current-head-id "missing-head")))
    (should-error (e-session-catalog-checkpoint-manifest session)
                  :type 'e-session-catalog-error)))

(ert-deftest e-session-catalog-test-missing-intermediate-signals ()
  "A checkpoint projection rejects an unresolved intermediate parent."
  (let* ((session (copy-tree (e-session-catalog-test--session)))
         (message (nth 1 (plist-get session :messages))))
    (setf (plist-get message :parent-id) "missing-intermediate")
    (should-error (e-session-catalog-checkpoint-manifest session)
                  :type 'e-session-catalog-error)))

(ert-deftest e-session-catalog-test-two-entry-cycle-signals ()
  "A checkpoint projection rejects a repeated ID in the parent path."
  (let* ((session (copy-tree (e-session-catalog-test--session)))
         (first (e-session-catalog-test--entry
                 'message "cycle-a" "cycle-b"
                 '(:role user :content "cycle-a")))
         (second (e-session-catalog-test--entry
                  'message "cycle-b" "cycle-a"
                  '(:role assistant :content "cycle-b"))))
    (setf (plist-get session :messages) (list first second)
          (plist-get session :current-head-id) "cycle-a")
    (should-error (e-session-catalog-checkpoint-manifest session)
                  :type 'e-session-catalog-error)))

(ert-deftest e-session-catalog-test-projection-uses-one-linear-analysis ()
  "One projection performs a deterministic bounded pass over every category."
  (cl-labels
      ((measure
        (count)
        (let ((entry-collections 0)
              (parent-resolutions 0)
              (membership-operations 0)
              (retained-list-visits 0)
              (final-filter-visits 0)
              (context-components 0)
              (legacy-list-scan-work 0)
              (original-entries
               (symbol-function 'e-session-catalog--session-entries))
              (original-entry
               (symbol-function 'e-session-catalog--analysis-entry))
              (original-member
               (symbol-function 'e-session-catalog--analysis-member-p))
              (original-retained
               (symbol-function 'e-session-catalog--select-retained))
              (original-final
               (symbol-function 'e-session-catalog--select-final-path))
              (original-member-function (symbol-function 'member))
              (original-components
               (symbol-function 'e-session-catalog--context-components))
              (session (e-session-catalog-test--linear-session count)))
          (cl-letf
              (((symbol-function 'e-session-catalog--session-entries)
                (lambda (&rest arguments)
                  (setq entry-collections (1+ entry-collections))
                  (apply original-entries arguments)))
               ((symbol-function 'e-session-catalog--analysis-entry)
                (lambda (&rest arguments)
                  (setq parent-resolutions (1+ parent-resolutions))
                  (apply original-entry arguments)))
               ((symbol-function 'e-session-catalog--analysis-member-p)
                (lambda (&rest arguments)
                  (setq membership-operations (1+ membership-operations))
                  (apply original-member arguments)))
               ((symbol-function 'e-session-catalog--select-retained)
                (lambda (items predicate)
                  (setq retained-list-visits
                        (+ retained-list-visits (length items)))
                  (funcall original-retained items predicate)))
               ((symbol-function 'e-session-catalog--select-final-path)
                (lambda (items predicate)
                  (setq final-filter-visits
                        (+ final-filter-visits (length items)))
                  (funcall original-final items predicate)))
               ;; The old implementation used `member' over a growing path
               ;; inside each retained-entry scan.  Keep this test-only guard
               ;; so that a quadratic list lookup cannot hide behind otherwise-
               ;; linear category counters.
               ((symbol-function 'member)
                (lambda (item items)
                  (setq legacy-list-scan-work
                        (+ legacy-list-scan-work (length items)))
                  (funcall original-member-function item items)))
               ((symbol-function 'e-session-catalog--context-components)
                (lambda (&rest arguments)
                  (setq context-components (1+ context-components))
                  (apply original-components arguments))))
            (let ((manifest (e-session-catalog-checkpoint-manifest session)))
              (list entry-collections parent-resolutions
                    membership-operations retained-list-visits
                    final-filter-visits context-components
                    (length (plist-get manifest :entry-ids))
                    legacy-list-scan-work))))))
    (let* ((small (measure 64))
           (large (measure 128)))
      (should (= (nth 0 small) 1))
      (should (= (nth 0 large) 1))
      ;; The path contains the root plus COUNT-1 messages.  Every path
      ;; resolution, indexed membership operation, retained-category visit,
      ;; final filter visit, and context component call is accounted for.
      (dolist (sample (list small large))
        (let ((count (if (eq sample small) 64 128)))
          (should (= (nth 1 sample) count))
          ;; Membership includes traversal (COUNT), the five retained
          ;; categories (5*(COUNT-1)), three context-owner set checks for
          ;; each complete path entry (3*COUNT), and the final required-ID
          ;; filter (COUNT-1; the root is excluded before membership).
          (should (= (nth 2 sample) (- (* 10 count) 6)))
          (should (= (nth 3 sample) (- (* 5 count) 4)))
          (should (= (nth 4 sample) count))
          (should (= (nth 5 sample) (* 2 count)))
          (should (= (nth 7 sample) 0))))
      ;; The root is excluded from retained entries, so account for it when
      ;; comparing the final selection across the two fixture sizes.
      (should (= (1+ (nth 6 large)) (* 2 (1+ (nth 6 small))))))))

(ert-deftest e-session-catalog-test-checkpoint-path-selects-valid-compaction ()
  "A valid compaction selects the resumable path suffix."
  (let* ((session (e-session-catalog-test--session))
         (path (e-session-catalog--checkpoint-path-suffix session)))
    (should (equal (mapcar (lambda (entry) (plist-get entry :id)) path)
                   '("message-2" "compaction-1" "generation-1"
                     "promotion-1" "curation-1" "response-1")))))

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
