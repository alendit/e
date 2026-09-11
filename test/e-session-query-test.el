;;; e-session-query-test.el --- Pure session query derivation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-session-query)
(require 'e-session-aggregate)

(defconst e-session-query-test--session-id "query-session")

(defun e-session-query-test--record (type id &rest fields)
  "Build a bounded test RECORD of TYPE and ID."
  (append (list :type type :session-id e-session-query-test--session-id
                :id id :timestamp (format "2026-09-06T00:00:%02dZ"
                                           (length id)))
          fields))

(defun e-session-query-test--record-at (type id timestamp &rest fields)
  "Build a bounded RECORD of TYPE, ID, and TIMESTAMP for replay tests."
  (append (list :type type :session-id e-session-query-test--session-id
                :id id :timestamp timestamp)
          fields))

(defun e-session-query-test--assert-row-equivalent
    (aggregate-state derived-state)
  "Compare exact current-row fields, normalizing only journal position."
  (let ((expected (copy-sequence aggregate-state)))
    ;; The adapter supplies this ordering value; it is not an aggregate
    ;; replay counter and is intentionally the sole normalized field here.
    (plist-put expected :journal-position
               (plist-get derived-state :journal-position))
    (dolist (key e-session-query-state-keys)
      (should (equal (plist-get expected key)
                     (plist-get derived-state key))))))

(defun e-session-query-test--root ()
  "Return the canonical test session root record."
  (e-session-query-test--record
   "session" "root"
   :created-at "2026-09-06T00:00:00Z"
   :updated-at "2026-09-06T00:00:00Z"
   :metadata '(:name "query test")))

(defun e-session-query-test--state ()
  "Return a fresh root query state."
  (e-session-query-derive (list (e-session-query-test--root))))

(defun e-session-query-test--without-key (plist key)
  "Return PLIST without KEY, preserving the remaining plist order."
  (let ((tail plist)
        result)
    (while tail
      (let ((current-key (pop tail))
            (value (pop tail)))
        (unless (eq current-key key)
          (setq result (append result (list current-key value))))))
    result))

(ert-deftest e-session-query-test-detaches-nested-lists-and-vectors ()
  "Every admitted container is detached, including vectors below lists."
  (let* ((nested-list (list :value "before"))
         (nested-vector (vector nested-list))
         (metadata (list :name "name" :nested nested-vector))
         (options (vector (list :model "model")))
         (session (list :id "detached" :metadata metadata :name "name"
                        :summary "summary" :created-at "created"
                        :updated-at "updated" :message-count 1
                        :last-message-at "message-time"
                        :latest-assistant-marker "assistant"
                        :current-branch "main" :turn-options options
                        :current-head-id "head" :root-event-id "root"))
         (state (e-session-query-state-from-session session)))
    (plist-put nested-list :value "mutated-list")
    (aset nested-vector 0 (list :value "mutated-vector"))
    (aset options 0 (list :model "mutated-options"))
    (should (equal (plist-get (plist-get state :metadata) :nested)
                   (vector (list :value "before"))))
    (should (equal (plist-get state :turn-options)
                   (vector (list :model "model"))))
    (should-not (plist-get state :routing-policy))))

(ert-deftest e-session-query-test-practical-value-bounds ()
  "Depth, total nodes, width, and total scalar bytes are bounded."
  (let ((deep nil))
    (dotimes (_ (1+ e-session-query-state-value-depth-limit))
      (setq deep (list deep)))
    (should-error (e-session-query--copy-value deep)
                  :type 'e-session-query-delta-error))
  (should-error
   (e-session-query--copy-value
    (make-vector (1+ e-session-query-state-value-width-limit) nil))
   :type 'e-session-query-delta-error)
  (should-error
   (e-session-query--copy-value
    (make-list 1000 (make-vector 8 nil)))
   :type 'e-session-query-delta-error)
  (should-error
   (e-session-query--copy-value
    (make-list 70 (make-string e-session-query-state-string-byte-limit ?x)))
   :type 'e-session-query-delta-error))

(ert-deftest e-session-query-test-large-message-projects-only-bounded-summary ()
  "Large durable content does not become or invalidate a current-row copy."
  (let* ((content (concat (make-string 5000 ?x) "é-tail"))
         (record (e-session-query-test--record
                  "message" "large-message"
                  :message (list :id "large-message" :role 'user
                                 :content content
                                 :provider-payload (make-string 12000 ?p))))
         (state (e-session-query-state-apply-record
                 (e-session-query-test--state) record))
         (summary (plist-get state :summary)))
    (should (= (plist-get state :message-count) 1))
    (should (string-prefix-p summary content))
    (should (= (string-bytes summary)
               e-session-query-state-string-byte-limit))
    (should (equal (plist-get (plist-get record :message) :content) content))))

(ert-deftest e-session-query-test-bignum-magnitude-is-bounded ()
  "Large integers cannot evade the total semantic-value byte bound."
  ;; Keep the fixture below Emacs' own maximum bignum size while shrinking
  ;; this seam's limit locally; the failure must come from magnitude charging.
  (let ((e-session-query-state-value-byte-limit 1024))
    (should-error
     (e-session-query--copy-value (ash 1 20000))
     :type 'e-session-query-delta-error)))

(ert-deftest e-session-query-test-state-requires-derived-message-count ()
  "Row projection never derives message count by scanning transcript history."
  (should-error
   (e-session-query-state-from-session
    '(:id "missing-count" :name "Missing count" :metadata nil
      :summary nil :created-at "created" :updated-at "updated"
      :last-message-at nil :latest-assistant-marker nil
      :messages ((:role user :content "must not be counted"))
      :current-branch nil :turn-options nil :current-head-id "head"
      :root-event-id "root"))
   :type 'e-session-query-delta-error))

(ert-deftest e-session-query-test-full-row-abi-is-exact-and-unique ()
  "Rows reject missing, unknown, and duplicate keys."
  (let ((state (e-session-query-test--state)))
    (should (equal (sort (copy-sequence e-session-query-state-keys)
                        (lambda (left right)
                          (string< (symbol-name left) (symbol-name right))))
                   (let (keys tail)
                     (setq tail state)
                     (while tail
                       (push (pop tail) keys)
                       (pop tail))
                     (sort keys
                           (lambda (left right)
                             (string< (symbol-name left)
                                      (symbol-name right)))))))
    (should-error
     (e-session-query-state-validate
      (e-session-query-test--without-key state :summary))
     :type 'e-session-query-delta-error)
    (should-error
     (e-session-query-state-validate
      (append (copy-sequence state) (list :unknown t)))
     :type 'e-session-query-delta-error)
    (should-error
     (e-session-query-state-validate
      (append (copy-sequence state) (list :name "duplicate")))
     :type 'e-session-query-delta-error)))

(ert-deftest e-session-query-test-control-deltas-are-exact-and-detached ()
  "No-op and deletion controls have only their exact two-key shapes."
  (let* ((state (e-session-query-test--state))
         (noop (e-session-query-delta-noop
                state e-session-query-test--session-id))
         (deleted (e-session-query-delta-delete
                   e-session-query-test--session-id)))
    (should (equal noop
                   (list :session-id e-session-query-test--session-id
                         :noop t)))
    (should (equal deleted
                   (list :session-id e-session-query-test--session-id
                         :deleted t)))
    (should-not (plist-member noop :name))
    (should-not (plist-member deleted :metadata))
    (should-error
     (e-session-query-control-delta-validate
      (append (copy-sequence noop) (list :extra t)))
     :type 'e-session-query-delta-error)
    (should-error
     (e-session-query-control-delta-validate
      (append (copy-sequence noop) (list :noop nil)))
     :type 'e-session-query-delta-error)
    (should-error
     (e-session-query-control-delta-validate
      (list :session-id e-session-query-test--session-id
            :deleted t :noop t))
     :type 'e-session-query-delta-error)
    (should-error (e-session-query-state-validate noop)
                  :type 'e-session-query-delta-error)))

(ert-deftest e-session-query-test-row-omits-retired-counters ()
  "The pure row has no invented activity or revision counters."
  (let ((state (e-session-query-test--state)))
    (dolist (key '(:activity-count :revision :updated-seq))
      (should-not (plist-member state key)))
    (should (plist-member state :journal-position))
    (should (plist-member state :board-output-sequence))
    (should (plist-member state :board-activity-sequence))))

(ert-deftest e-session-query-test-record-shape-rejects-duplicate-keys ()
  "A durable record cannot carry two values for one keyword field."
  (should-error
   (e-session-query-state-apply-record
    nil
    (append (e-session-query-test--root)
            (list :type "session")))
   :type 'e-session-query-record-error))

(ert-deftest e-session-query-test-current-runtime-rejects-retired-board-records ()
  "Current row derivation never interprets retired Board journal records."
  (let ((state (e-session-query-test--state)))
    (dolist (type '("board-message" "board-messages-cleared"
                    "board-session-state"))
      (should-error
       (e-session-query-state-apply-record
        state (e-session-query-test--record type "retired"))
       :type 'e-session-query-record-error))))

(ert-deftest e-session-query-test-replays-every-durable-family-semantically ()
  "Every supported journal family has an explicit bounded-row replay rule."
  (let* ((expected-families
          '("session" "message" "activity-event" "message-display"
            "process-report" "branch-summary" "compaction" "provider-anchor"
            "context-generation" "context-promotion" "context-frame"
            "context-frame-settlement" "context-erasure"
            "context-curation-package" "messages-cleared" "current-branch"
            "session-info" "session-deleted"))
         (root
          (e-session-query-test--record-at
           "session" "root" "2026-09-06T00:00:00Z"
           :name "Root" :metadata '(:name "Root" :model "initial")
           :turn-options '(:model "gpt") :current-branch "main"
           :board-output-sequence 2 :board-activity-sequence 3
           :created-at "2026-09-06T00:00:00Z"
           :updated-at "2026-09-06T00:00:00Z"))
         (state (e-session-query-state-apply-record nil root)))
    (should (equal (sort (copy-sequence e-session-query-supported-record-types)
                         #'string<)
                   (sort (copy-sequence expected-families) #'string<)))
    ;; Root and both message markers feed the derived current-row fields.
    (setq state
          (e-session-query-state-apply-record
           state
           (e-session-query-test--record-at
            "message" "user-entry" "2026-09-06T00:00:01Z"
            :message '(:id "user-message" :role user :content "Prompt"
                       :created-at "2026-09-06T00:00:01Z"))))
    (should (equal (plist-get state :summary) "Prompt"))
    (should (= (plist-get state :message-count) 1))
    (setq state
          (e-session-query-state-apply-record
           state
           (e-session-query-test--record-at
            "message" "assistant-entry" "2026-09-06T00:00:02Z"
            :message '(:id "assistant-message" :role assistant
                       :content "Answer" :created-at "2026-09-06T00:00:02Z"
                       :board-output-sequence 5))))
    (should (= (plist-get state :message-count) 2))
    (should (equal (plist-get state :latest-assistant-marker)
                   "assistant-message"))
    (should (= (plist-get state :board-output-sequence) 5))
    (should (equal (plist-get state :last-message-at)
                   "2026-09-06T00:00:02Z"))
    (let ((before (copy-sequence state)))
      ;; Activity updates the Board sequence and is a session-head entry.
      (setq state
            (e-session-query-state-apply-record
             state
             (e-session-query-test--record-at
              "activity-event" "activity" "2026-09-06T00:00:03Z"
              :board-activity-sequence 8
              :semantic-event '(:event-type tool-finished))))
      (should-not (equal before state))
      (should (= (plist-get state :board-activity-sequence) 8))
      (should (equal (plist-get state :current-head-id) "activity"))
      ;; Display is durable only when a target command emitted a record; it
      ;; touches recency but never becomes the parent-chain head.
      (let ((head (plist-get state :current-head-id)))
        (setq state
              (e-session-query-state-apply-record
               state
               (e-session-query-test--record-at
                "message-display" "user-message" "2026-09-06T00:00:04Z"
                :display "hidden")))
        (should (equal (plist-get state :current-head-id) head))
        (should (equal (plist-get state :updated-at)
                       "2026-09-06T00:00:04Z")))
      ;; Each out-of-band durable family advances the aggregate head.
      (dolist (record
               (list
                (e-session-query-test--record-at
                 "process-report" "report" "2026-09-06T00:00:05Z"
                 :report '(:status done))
                (e-session-query-test--record-at
                 "branch-summary" "branch-summary" "2026-09-06T00:00:06Z"
                 :branch-id "main" :summary "summary")
                (e-session-query-test--record-at
                 "compaction" "compaction" "2026-09-06T00:00:07Z"
                 :summary "compacted" :first-kept-entry-id "root")
                (e-session-query-test--record-at
                 "provider-anchor" "anchor" "2026-09-06T00:00:08Z"
                 :provider-id "openai" :model "gpt")))
        (setq state (e-session-query-state-apply-record state record))
        (should (equal (plist-get state :current-head-id)
                       (plist-get record :id))))
      ;; Version-1 context history and replay-only frames preserve all state;
      ;; they are valid only after a root.
      (let ((ignored (copy-sequence state)))
        (dolist (record
                 (list
                  (e-session-query-test--record-at
                   "context-generation" "legacy-generation"
                   "2026-09-06T00:00:09Z"
                   :context-record
                   '(:record-version 1 :type context-generation
                     :id "legacy-generation"))
                  (e-session-query-test--record-at
                   "context-promotion" "legacy-promotion"
                   "2026-09-06T00:00:10Z"
                   :context-record
                   '(:record-version 1 :type context-promotion
                     :id "legacy-promotion"))
                  (e-session-query-test--record-at
                   "context-frame" "frame" "2026-09-06T00:00:11Z"
                   :context-record '(:record-version 2 :type context-frame))
                  (e-session-query-test--record-at
                   "context-frame-settlement" "settlement"
                   "2026-09-06T00:00:12Z"
                   :context-record
                   '(:record-version 2 :type context-frame-settlement))))
          (setq state (e-session-query-state-apply-record state record))
          (should (equal state ignored))))
      ;; Current context records, unlike the retired/replay-only forms, are
      ;; current session events and therefore update recency/head.
      (dolist (record
               (list
                (e-session-query-test--record-at
                 "context-generation" "generation" "2026-09-06T00:00:13Z"
                 :context-record '(:record-version 2 :type context-generation))
                (e-session-query-test--record-at
                 "context-promotion" "promotion" "2026-09-06T00:00:14Z"
                 :context-record '(:record-version 3 :type context-promotion))))
        (setq state (e-session-query-state-apply-record state record))
        (should (equal (plist-get state :current-head-id)
                       (plist-get record :id))))
      (setq state
            (e-session-query-state-apply-record
             state
             (e-session-query-test--record-at
              "context-curation-package" "curation" "2026-09-06T00:00:15Z"
              :promotion '(:record-version 3 :type context-promotion)
              :erasure nil)))
      (should (equal (plist-get state :current-head-id) "curation"))
      ;; Clear resets message-derived fields but retains Board watermarks.
      (setq state
            (e-session-query-state-apply-record
             state
             (e-session-query-test--record-at
              "messages-cleared" "clear" "2026-09-06T00:00:16Z")))
      (should (= (plist-get state :message-count) 0))
      (should-not (plist-get state :summary))
      (should-not (plist-get state :last-message-at))
      (should-not (plist-get state :latest-assistant-marker))
      (should (= (plist-get state :board-output-sequence) 5))
      (should (= (plist-get state :board-activity-sequence) 8))
      (should (equal (plist-get state :current-head-id) "clear"))
      (setq state
            (e-session-query-state-apply-record
             state
             (e-session-query-test--record-at
              "current-branch" "branch" "2026-09-06T00:00:20Z"
              :branch-id "feature")))
      (should (equal (plist-get state :current-branch) "feature"))
      ;; Exercise every session-info field, including the owner-key mapping
      ;; used by aggregate metadata semantics.
      (dolist (record
               (list
                (e-session-query-test--record-at
                 "session-info" "info-metadata" "2026-09-06T00:00:21Z"
                 :field 'metadata :value '(:name "Meta" :model "meta"))
                (e-session-query-test--record-at
                 "session-info" "info-config" "2026-09-06T00:00:22Z"
                 :field 'config :value '(:model "config"))
                (e-session-query-test--record-at
                 "session-info" "info-reference" "2026-09-06T00:00:23Z"
                 :field 'context-reference :key :source-reference
                 :value "source")
                (e-session-query-test--record-at
                 "session-info" "info-references" "2026-09-06T00:00:24Z"
                 :field 'context-references :owner "owner"
                 :value '(:source "reference"))
                (e-session-query-test--record-at
                 "session-info" "info-capability" "2026-09-06T00:00:25Z"
                 :field 'capability-state :capability-id "capability"
                 :version 1 :value '(:enabled t))
                (e-session-query-test--record-at
                 "session-info" "info-options" "2026-09-06T00:00:26Z"
                 :field 'turn-options :value '(:model "model"))
                (e-session-query-test--record-at
                 "session-info" "info-name" "2026-09-06T00:00:27Z"
                 :field 'name :value "Final")))
        (setq state (e-session-query-state-apply-record state record)))
      (should (equal (plist-get state :name) "Final"))
      (should (equal (plist-get state :turn-options) '(:model "model")))
      (should (equal (plist-get state :metadata)
                     '(:name "Meta" :model "config"
                       :source-reference "source"
                       :context-references (:owner (:source "reference"))
                       :capability-state
                       (:capability (:version 1 :state (:enabled t))))))
      (should (equal (plist-get state :root-p) t))
      ;; A standalone erasure is not a query-row family: it is rejected, while
      ;; an explicit delete is an exact control delta and fences later input.
      (should-error
       (e-session-query-state-apply-record
        state
        (e-session-query-test--record-at
         "context-erasure" "erasure" "2026-09-06T00:00:28Z"))
       :type 'e-session-query-context-erasure-error)
      (let ((deleted
             (e-session-query-state-apply-record
              state
              (e-session-query-test--record-at
               "session-deleted" "deleted" "2026-09-06T00:00:29Z"))))
        (e-session-query-control-delta-validate deleted)
        (should-error
         (e-session-query-state-apply-record
          deleted
          (e-session-query-test--record-at
           "message" "after-delete" "2026-09-06T00:00:30Z"
           :message '(:role user :content "invalid")))
         :type 'e-session-query-record-error)))
      ;; Keep the pre-root rule explicit: a v1 record is ignored only after a
      ;; known root, never used to synthesize a query row.
      (should-error
       (e-session-query-state-apply-record
        nil
        (e-session-query-test--record-at
         "context-generation" "pre-root-v1" "2026-09-06T00:00:31Z"
         :context-record '(:record-version 1 :type context-generation)))
       :type 'e-session-query-record-error)
      (should-error
       (e-session-query-delta-from-record
        nil
        (e-session-query-test--record-at
         "context-generation" "pre-root-delta" "2026-09-06T00:00:32Z"
         :context-record '(:record-version 1 :type context-generation)))
       :type 'e-session-query-record-error)))

(ert-deftest e-session-query-test-row-equivalence-follows-aggregate-replay ()
  "The query row matches exact aggregate replay semantics for all families."
  (let* ((root-time "2026-09-06T01:00:00Z")
         (session-id e-session-query-test--session-id)
         (root
          (e-session-query-test--record-at
           "session" "root" root-time
           :name "Equivalent"
           :metadata '(:name "Equivalent" :model "initial")
           :turn-options '(:model "gpt") :current-branch "main"
           :journal-position 17
           :created-at root-time :updated-at root-time))
         (records
          (list
           root
           (e-session-query-test--record-at
            "message" "user-message" "2026-09-06T01:00:01Z"
            :parent-id "root"
            :message '(:id "user-message" :role user :content "Prompt"
                       :created-at "2026-09-06T01:00:01Z"))
           (e-session-query-test--record-at
            "message" "assistant-message" "2026-09-06T01:00:02Z"
            :parent-id "user-message"
            :message '(:id "assistant-message" :role assistant
                       :content "Answer" :created-at "2026-09-06T01:00:02Z"))
           (e-session-query-test--record-at
            "activity-event" "activity" "2026-09-06T01:00:03Z"
            :parent-id "assistant-message" :turn-id "turn"
            :event-type 'tool-finished :payload '(:tool-call-id "call")
            :semantic-event
            '(:id "activity" :turn-id "turn" :event-type tool-finished
              :payload (:tool-call-id "call")))
           (e-session-query-test--record-at
            "message-display" "user-message" "2026-09-06T01:00:04Z"
            :display "hidden")
           (e-session-query-test--record-at
            "process-report" "report" "2026-09-06T01:00:05Z"
            :parent-id "activity" :report
            '(:id "report" :parent-id "activity" :status done))
           (e-session-query-test--record-at
            "branch-summary" "branch-summary" "2026-09-06T01:00:06Z"
            :parent-id "report" :branch-id "main" :summary "Summary")
           (e-session-query-test--record-at
            "compaction" "compaction" "2026-09-06T01:00:07Z"
            :parent-id "branch-summary" :summary "Compacted"
            :branch-id "main" :range '(0 . 2)
            :first-kept-entry-id "root" :tokens-before 10 :tokens-kept 4)
           (e-session-query-test--record-at
            "provider-anchor" "provider-anchor" "2026-09-06T01:00:08Z"
            :parent-id "compaction" :provider-id "openai" :model "gpt"
            :covered-entry-id "assistant-entry" :fingerprints '("fp"))
           ;; These legacy/replay-only records must be present in the same
           ;; journal but must not alter the current row.
           (e-session-query-test--record-at
            "context-generation" "legacy-generation"
            "2026-09-06T01:00:09Z" :parent-id "provider-anchor"
            :context-record
            '(:record-version 1 :type context-generation
              :id "legacy-generation" :checkpoint nil))
           (e-session-query-test--record-at
            "context-promotion" "legacy-promotion"
            "2026-09-06T01:00:10Z" :parent-id "provider-anchor"
            :context-record
            '(:record-version 1 :type context-promotion
              :id "legacy-promotion" :frame-id "legacy-frame"))
           (e-session-query-test--record-at
            "context-frame" "frame" "2026-09-06T01:00:11Z"
            :parent-id "provider-anchor"
            :context-record '(:record-version 2 :type context-frame))
           (e-session-query-test--record-at
            "context-frame-settlement" "settlement"
            "2026-09-06T01:00:12Z" :parent-id "provider-anchor"
            :context-record
            '(:record-version 2 :type context-frame-settlement))
           ;; Version-2 context records are still valid aggregate history and
           ;; therefore participate in the current head before curation.
           (e-session-query-test--record-at
            "context-generation" "generation" "2026-09-06T01:00:13Z"
            :parent-id "provider-anchor"
            :context-record
            '(:record-version 2 :type context-generation :id "generation"
              :checkpoint nil :covered-session-boundary "root"))
           (e-session-query-test--record-at
            "context-promotion" "promotion-entry"
            "2026-09-06T01:00:14Z" :parent-id "generation"
            :context-record
            '(:record-version 2 :type context-promotion :id "promotion"
              :frame-id "frame" :generation-id "generation"
              :consumer-request-id "consumer" :response-entry-id "response"
              :facts ((:id "fact" :value "value"))
              :source-observation-ids ("observation")
              :source-refs ("source")
              :source-fingerprints ("fingerprint")))))
         (store (e-session-store-create)))
    ;; First replay the static record prefix so curation preparation can use
    ;; the aggregate's real active-generation ownership boundary.
    (dolist (record records)
      (e-session-aggregate-apply-record store record))
    (let* ((promotion
            '(:record-version 3 :type context-promotion
              :id "curated-promotion" :frame-id "frame"
              :generation-id "generation" :consumer-request-id "consumer-3"
              :response-entry-id "response-3"
              :items ((:kind summary :text "Curated summary"
                       :source-observation-ids ("observation-3")
                       :source-refs ("source-3")
                       :source-fingerprints ("fingerprint-3")))))
           (prepared
            (e-session-aggregate--prepare-context-curation-package
             store session-id (list :promotion promotion :erasure nil)))
           (curation (plist-get prepared :record)))
      (setq records (append records (list curation)))
      (e-session-aggregate-apply-record store curation))
    (setq records
          (append
           records
           (list
            (e-session-query-test--record-at
             "messages-cleared" "clear" "2026-09-06T01:00:16Z")
            (e-session-query-test--record-at
             "current-branch" "branch" "2026-09-06T01:00:20Z"
             :branch-id "feature")
            (e-session-query-test--record-at
             "session-info" "info-metadata" "2026-09-06T01:00:21Z"
             :field 'metadata :value '(:name "Meta" :model "meta"))
            (e-session-query-test--record-at
             "session-info" "info-config" "2026-09-06T01:00:22Z"
             :field 'config :value '(:model "config"))
            (e-session-query-test--record-at
             "session-info" "info-reference" "2026-09-06T01:00:23Z"
             :field 'context-reference :key :source-reference :value "source")
            (e-session-query-test--record-at
             "session-info" "info-references" "2026-09-06T01:00:24Z"
             :field 'context-references :owner "owner"
             :value '(:source "reference"))
            (e-session-query-test--record-at
             "session-info" "info-capability" "2026-09-06T01:00:25Z"
             :field 'capability-state :capability-id "capability" :version 1
             :value '(:enabled t))
            (e-session-query-test--record-at
             "session-info" "info-options" "2026-09-06T01:00:26Z"
             :field 'turn-options :value '(:model "model"))
            (e-session-query-test--record-at
             "session-info" "info-name" "2026-09-06T01:00:27Z"
             :field 'name :value "Final"))))
    ;; Apply the post-curation records exactly once.  The prefix length is
    ;; stable here: fifteen static records plus one curation package.
    (let ((prefix-length 16))
      (dolist (record (nthcdr prefix-length records))
        (e-session-aggregate-apply-record store record)))
    (let* ((session (gethash session-id (e-session-store-sessions store)))
           (_final (e-session-aggregate-finalize-replayed-session store session))
           (aggregate-state (e-session-query-state-from-session session))
           (derived-state (e-session-query-derive records)))
      (e-session-query-test--assert-row-equivalent
       aggregate-state derived-state)
      ;; Keep this equivalence check explicit rather than reducing it to a
      ;; timestamp assertion: every closed state key is compared above.
      (should (equal (plist-get derived-state :summary) nil))
      (should (= (plist-get derived-state :message-count) 0))
      (should (equal (plist-get derived-state :current-branch) "feature")))))

(provide 'e-session-query-test)

;;; e-session-query-test.el ends here
