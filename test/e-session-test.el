;;; e-session-test.el --- Tests for e sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for session storage.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-dev-profile)
(require 'e-session)
(require 'e-session-storage)
(require 'e-session-legacy)
(require 'e-context-lifetime)
(require 'e-board)

(defun e-session-test--copy-value (value)
  "Return a detached fixture copy of VALUE, including string leaves."
  (cond
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (vconcat (mapcar #'e-session-test--copy-value (append value nil))))
   ((consp value)
    (cons (e-session-test--copy-value (car value))
          (e-session-test--copy-value (cdr value))))
   (t value)))

(defun e-session-test--replay-legacy-copy (directory session-id)
  "Replay SESSION-ID from copied legacy DIRECTORY through pure boundaries."
  (let* ((decoded (e-session-legacy-decode directory))
         (records (cdr (assoc session-id (plist-get decoded :sessions))))
         (store (e-session-store-create)))
    (unless records
      (signal 'e-session-missing (list session-id)))
    (let ((e-session--load-in-progress t))
      (dolist (record records)
        (e-session--apply-physical-record store record)))
    (e-session--finish-replay store session-id)
    store))

(ert-deftest e-session-test-semantic-storage-port-does-not-require-jsonl-details ()
  "The facade composes a storage double through typed mutation operations.

The double deliberately implements only preparation, commit, and index
publication.  It has no journal paths, queue, controller, or checkpoint
knowledge; those remain adapter details behind the storage owner."
  (let* ((directory (make-temp-file "e-session-storage-port-" t))
         (store (e-session-store-create
                 :directory directory
                 :sessions-directory (expand-file-name "sessions" directory)
                 :index-file (expand-file-name "index.json" directory)
                 :persistent t))
         prepared committed indexed)
    (unwind-protect
        (progn
          (e-session-storage-register
           store :directory directory
           :persistent t :backend 'sqlite :runtime-store 'storage-double
           :owns-runtime-store nil)
          (cl-letf (((symbol-function 'e-session-storage-prepare-mutation)
                   (lambda (_store session-id record)
                     (push (list session-id (plist-get record :type)) prepared)
                     record))
                  ((symbol-function 'e-session-storage-commit-mutation)
                   (lambda (_store session-id record)
                     (push (list session-id (plist-get record :type)) committed)
                     record))
                  ((symbol-function 'e-session-storage-publish-projections)
                   (lambda (&rest _)
                     (setq indexed t))))
          (e-session-create store :id "storage-port")
          (should (equal prepared '(("storage-port" "session"))))
          (should (equal committed '(("storage-port" "session"))))
          (should indexed)
          (should-not (file-exists-p (expand-file-name "index.json" directory)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-unload-session-restores-detached-index-stub ()
  "Offline replay callers can release a loaded session without losing its index."
  (let* ((directory (make-temp-file "e-session-unload-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "unload-session"))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (e-session-append-message
           store session-id '(:role user :content "bounded"))
          (let ((entry (e-session-index-entry store session-id)))
            (should (plist-get entry :id))
            (should-not
             (plist-get
              (e-session-unload-session store session-id entry)
              :loaded)))
          (should-not (plist-get (e-session-index-entry store session-id)
                                 :loaded))
          (should (equal (plist-get (car (e-session-messages store session-id))
                                    :content)
                         "bounded")))
      (delete-directory directory t))))

(defun e-session-test--append-literal-v2-record (store session-id record)
  "Install literal version-2 RECORD as a test-only replay fixture.

This helper writes the journal envelope directly and replays the same literal
record into STORE.  It intentionally does not call a production context
promotion writer; new production records are version 3 only."
  (let* ((session (e-session-get store session-id))
         (entry (list :type "context-promotion"
                      :session-id session-id
                      :id (format "legacy-entry:%s" (plist-get record :id))
                      :parent-id (plist-get session :current-head-id)
                      :timestamp "2026-08-24T00:00:00Z"
                      :context-record
                      (plist-get
                       (e-session-codec-record-for-json
                        (list :context-record record))
                       :context-record))))
    (e-session-storage-commit-mutation store session-id entry)
    ;; The codec is a pure mapping boundary.  Replay application belongs to
    ;; the aggregate owner and is exercised explicitly here.
    (e-session-aggregate-apply-record
     store (e-session-codec-replay-record entry))
    record))

(defun e-session-test--literal-v1-erasure-record (&optional suffix)
  "Return a detached literal version-1 erasure fixture.

This helper is deliberately a test fixture for the session codec; it does not
stand in for the pure curation preparation path."
  (let ((suffix (or suffix "1")))
    (list :record-version 1
          :type 'context-erasure
          :id (format "erasure-%s" suffix)
          :frame-id (format "frame-%s" suffix)
          :generation-id "generation-erasure"
          :consumer-request-id (format "consumer-%s" suffix)
          :response-entry-id (format "response-%s" suffix)
          :sources
          (list (list :source-observation-id (format "observation-%s" suffix)
                      :source-ref (format "external:tool:%s" suffix)
                      :source-fingerprint (format "fingerprint-%s" suffix)
                      :tool-call-id (format "tool-call-%s" suffix))))))

(defun e-session-test--routing-policy (&optional participant-id)
  "Return one valid detached board routing policy fixture."
  (list :participant-id (or participant-id "participant-private")
        :pickup-selector
        '(:kind input :tags (private)
          :attributes (:symbol car :string "car"
                       :nested (car "car" (:inner car))
                       :vector [car "car"]))
        :observer-selector
        '(:subject-participant-id "participant-private"
          :attributes (:symbol car :string "car"
                       :nested (car "car" (:inner car))
                       :vector [car "car"]))
        :default-tags '(private)
        :default-to "participant-private"))

(ert-deftest e-session-test-board-routing-policy-round-trips-and-detaches ()
  "A complete routing policy survives index/checkpoint and caller mutation."
  (let* ((store (e-session-store-create))
         (session-id "routing-policy")
         (policy (e-session-test--routing-policy))
         (expected (e-session-board-routing-policy-copy-value policy))
         (returned nil))
    (e-session-create store :id session-id)
    (setq returned
          (e-session-declare-board-state
           store session-id "chat:routing-policy" "board-routing"
           "participant" policy))
    (setcar (plist-get policy :default-tags) 'caller-mutated)
    (setf (aref (plist-get policy :participant-id) 0) ?X)
    (should (equal (e-session-board-routing-policy
                    (e-session-get store session-id))
                   expected))
    (should (equal (plist-get (plist-get returned :routing-policy)
                              :participant-id)
                   "participant-private"))
    (let* ((indexed (car (e-session-list store)))
           (indexed-state (plist-get indexed :board-state))
           (manifest (e-session-checkpoint-manifest store session-id))
           (manifest-state (plist-get manifest :board-state)))
      (should (equal (plist-get indexed-state :routing-policy) expected))
      (should (equal (plist-get manifest-state :routing-policy) expected))
      (setf (aref (plist-get (plist-get manifest-state :routing-policy)
                             :participant-id)
                  0)
            ?Y))
    (should (equal (plist-get (e-session-board-association
                               (e-session-get store session-id))
                              :routing-policy)
                   expected))))

(ert-deftest e-session-test-board-routing-policy-persistence-round-trips ()
  "A policy is restored from JSONL and its index without compatibility shims."
  (let ((directory (make-temp-file "e-session-routing-policy-" t)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (session-id "persistent-routing")
               (policy (e-session-test--routing-policy "participant-persist")))
          (e-session-create store :id session-id)
          (e-session-declare-board-state
           store session-id "chat:persistent-routing" "board-persist"
           "participant" policy)
          (e-session-flush-write-queue store)
          (let ((restored
                 (e-session-get
                  (e-session-persistent-store-create directory) session-id)))
            (should (equal (e-session-board-routing-policy restored) policy))
            (let* ((pickup (plist-get (e-session-board-routing-policy restored)
                                      :pickup-selector))
                   (attributes (plist-get pickup :attributes)))
              (should (eq (plist-get attributes :symbol) 'car))
              (should (equal (plist-get attributes :string) "car"))
              (should (eq (car (plist-get attributes :nested)) 'car))
              (should (equal (cadr (plist-get attributes :nested)) "car"))
              (should (vectorp (plist-get attributes :vector)))
              (should (eq (aref (plist-get attributes :vector) 0) 'car))
              (should (equal (aref (plist-get attributes :vector) 1) "car")))
            (should (equal (plist-get (e-session-board-association restored)
                                      :association-role)
                           "participant"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-routing-policy-invalid-is-atomic ()
  "Partial, executable, and unknown policy values do not mutate association."
  (let* ((store (e-session-store-create))
         (session-id "routing-invalid"))
    (e-session-create store :id session-id)
    (e-session-declare-board-state
     store session-id "chat:routing-invalid" "board-invalid" "owner")
    (let ((before (copy-tree (e-session-board-association
                              (e-session-get store session-id)))))
      (dolist (policy
               (list
                '(:participant-id "p" :pickup-selector (:tags (private))
                  :observer-selector (:tags (private)) :default-tags (private))
                '(:participant-id "p" :pickup-selector
                  (:tags (private) :predicate (lambda (_message) t))
                  :observer-selector (:tags (private)) :default-tags (private)
                  :default-to nil)
                '(:participant-id "p" :pickup-selector (:tags (private))
                  :observer-selector (:tags (private)) :default-tags (private)
                  :default-to nil :unknown t)))
        (should-error
         (e-session-declare-board-state
          store session-id "chat:routing-invalid" "board-invalid" "owner"
          policy)))
      (should (equal (e-session-board-association
                      (e-session-get store session-id))
                     before)))
      ;; Names that happen to be callable remain data when they are used as
      ;; declarative selector atoms; executable objects/forms do not.
      (let ((data-symbol-policy
             '(:participant-id "p"
               :pickup-selector (:kind car :tags (car mapcar)
                                 :attributes (:marker car))
               :observer-selector (:kind mapcar :tags (length))
               :default-tags (car mapcar)
               :default-to nil)))
        (should (e-session-board-routing-policy-valid-p data-symbol-policy)))
      (let ((function-form-policy
             '(:participant-id "p"
               :pickup-selector (:kind input :tags (private)
                                 :attributes (:marker (lambda () t)))
               :observer-selector (:tags (private))
               :default-tags (private)
               :default-to nil)))
        (should-not
         (e-session-board-routing-policy-valid-p function-form-policy)))))

(ert-deftest e-session-test-board-routing-policy-attributes-reject-before-mutation ()
  "Invalid attribute clauses fail before queue, JSON, or association changes."
  (let* ((directory (make-temp-file "e-session-routing-attributes-" t))
         (store (e-session-persistent-index-store-create directory))
         (session-id "routing-attributes")
         (base-policy (e-session-test--routing-policy "attribute-participant"))
         (invalid-attributes
          (list [car "car"]
                '(car "car")
                '(:kind "ordinary" :odd)
                '((:kind . "ordinary") ("kind" . "bad"))))
         persisted)
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (e-session-declare-board-state
           store session-id "chat:routing-attributes"
           "attribute-board" "participant" base-policy)
          (let ((association-before
                 (copy-tree
                  (e-session-board-association
                   (e-session-get store session-id))))
                (durability-before
                 (e-session-storage-durability-status store)))
            (dolist (attributes invalid-attributes)
              (let ((policy (copy-tree base-policy)))
                (plist-put
                 (plist-get policy :pickup-selector)
                 :attributes attributes)
                (should-not (e-board-selector-attributes-valid-p attributes))
                (let ((json-called nil))
                  (cl-letf (((symbol-function 'json-encode)
                             (lambda (&rest _)
                               (setq json-called t)
                               (error "unexpected JSON encoding")))
                            ((symbol-function 'e-session-storage-commit-mutation)
                             (lambda (&rest _)
                               (setq persisted t)
                               (error "unexpected persistence"))))
                    (should-error
                     (e-session-declare-board-state
                      store session-id "chat:routing-attributes"
                      "attribute-board" "participant" policy)))
                  (should-not json-called))
              (should (equal
                       (e-session-board-association
                        (e-session-get store session-id))
                       association-before))
              (should (equal (e-session-storage-durability-status store)
                             durability-before)))
            (should-not persisted)))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t)))))

(ert-deftest e-session-test-board-routing-policy-owner-legacy-remains-readable ()
  "Role-bearing legacy owner state retains its established readable shape."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "legacy-owner")
    (e-session-declare-board-state
     store "legacy-owner" "chat:legacy-owner" "legacy-board" "owner")
    (let ((association (e-session-board-association
                        (e-session-get store "legacy-owner"))))
      (should (equal association
                     '(:board-id "legacy-board"
                       :principal "chat:legacy-owner"
                       :association-role "owner")))
      (should-not (e-session-board-routing-policy
                   (e-session-get store "legacy-owner"))))))

(ert-deftest e-session-test-standalone-context-erasure-replay-is-rejected ()
  "Version-1 erasure is valid only inside the atomic curation package."
  (let* ((store (e-session-store-create))
         (session-id "standalone-erasure-rejected")
         (session (e-session-create store :id session-id))
         (record
          (list :type "context-erasure"
                :session-id session-id
                :id "outer-erasure"
                :parent-id (plist-get session :root-event-id)
                :timestamp "2026-08-28T00:00:00Z"
                :context-record
                (e-session-test--literal-v1-erasure-record "outer"))))
    (should-error
     (e-session-aggregate-apply-record
      store (e-session-codec-replay-record record))
                  :type 'e-session-error)
    (should-not (e-session-context-erasures store session-id))
    (should-not (e-session-erased-tool-call-ids store session-id))))

(ert-deftest e-session-test-create-and-read ()
  "Sessions can be created and read by id."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1" :metadata '(:model "fake"))
    (should (equal (plist-get (e-session-get store "session-1") :id) "session-1"))
    (should (equal (e-session-messages store "session-1") nil))))

(ert-deftest e-session-test-context-v2-records-round-trip-through-reopen ()
  "Literal version-2 records remain readable after a persistent reopen."
  (let* ((directory (make-temp-file "e-session-context-v2-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "context-v2")
         generation
         (v2-record
          '(:record-version 2
            :type context-promotion
            :id "promotion-v2"
            :frame-id "frame-v2"
            :generation-id "generation-v2"
            :consumer-request-id "consumer-v2"
            :response-entry-id "response-entry"
            :facts ((:id "fact-v2" :value "promoted fact"))
            :source-observation-ids ("observation-v2")
            :source-refs ("external:canvas:v2")
            :source-fingerprints ("canvas-v2"))))
    (unwind-protect
        (progn
          (let* ((session (e-session-create store :id session-id))
                 (generation-id
                  (plist-get session :root-event-id)))
            (setq generation
                  (e-context-lifetime-generation-create
                   :id "generation-v2"
                   :checkpoint
                   '((:role system
                      :content (:text "policy"
                                :nested (:enabled :json-false))))
                   :covered-session-boundary generation-id))
            (e-session-append-context-generation
             store session-id generation)
            (e-session-append-message
             store session-id
             '(:id "response-entry"
               :role assistant
               :content "selected response"))
            (e-session-test--append-literal-v2-record
             store session-id v2-record)))
          (e-session-flush-write-queue store)
          (let* ((before (e-session-persistent-store-create directory))
                 (before-generations
                  (mapcar #'e-session-aggregate-context-record
                          (e-session-context-generations
                           before session-id)))
                 (before-promotions
                  (mapcar #'e-session-aggregate-context-record
                          (e-session-context-promotions
                           before session-id)))
                 (before-manifest
                  (plist-get (e-session-checkpoint-manifest
                              before session-id)
                             :context-lifetime))
                 (reopened (e-session-persistent-store-create directory))
                 (after-manifest
                  (plist-get (e-session-checkpoint-manifest
                              reopened session-id)
                             :context-lifetime)))
            (should (equal before-generations
                           (list (e-context-lifetime-generation-record
                                  generation))))
            (let ((decoded
                   (e-context-lifetime-promotion-from-record
                    (car before-promotions))))
              (should (equal (e-context-lifetime-promotion-id decoded)
                             "promotion-v2"))
              (should (equal (e-context-lifetime-promotion-facts decoded)
                             '((:id "fact-v2" :value "promoted fact")))))
            (should (equal before-manifest after-manifest))
            (should-not
             (plist-member (car before-generations) :durable-tail))
            (should-not
             (plist-member (car before-promotions) :body))))
      (delete-directory directory t)))

(ert-deftest e-session-test-context-v3-append-reopen-projection-and-fork ()
  "Prepared v3 curation records persist beside v2 and fork literally."
  (let* ((directory (make-temp-file "e-session-context-v3-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "context-v3")
         (v2-record
          '(:record-version 2
            :type context-promotion
            :id "promotion-v2"
            :frame-id "frame-v2"
            :generation-id "generation-v3"
            :consumer-request-id "consumer-v2"
            :response-entry-id "response-v2"
            :facts ((:id "fact-v2" :value "legacy value"))
            :source-observation-ids ("observation-v2")
            :source-refs ("source-v2")
            :source-fingerprints ("fingerprint-v2")))
         (web-result
          (e-context-lifetime-canonicalize
           '(:capability "web.fetch"
             :headers ((:name "set-cookie" :value "first=1")
                       (:name "set-cookie" :value "second=2")
                       (:name "server" :value "nginx"))
             :text "Fetched page")))
         (v3-record
          `(:record-version 3
            :type context-promotion
            :id "curation-v3"
            :frame-id "frame-v3"
            :generation-id "generation-v3"
            :consumer-request-id "consumer-v3"
            :response-entry-id "response-v3"
            :items
            ((:kind exact :value ,web-result
              :source-observation-ids ("observation-v3-exact")
              :source-refs ("source-v3-exact")
              :source-fingerprints ("fingerprint-v3-exact"))
             (:kind summary :text "summarized replacement"
              :source-observation-ids ("observation-v3-a" "observation-v3-b")
              :source-refs ("source-v3-a" "source-v3-b")
              :source-fingerprints ("fingerprint-v3-a" "fingerprint-v3-b")))))
         generation-entry)
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (setq generation-entry
                (e-session-append-context-generation
                 store session-id
                 (e-context-lifetime-generation-create
                  :id "generation-v3"
                  :checkpoint '((:role system :content "C0"))
                  :covered-session-boundary
                  (plist-get (e-session-get store session-id)
                             :root-event-id))))
          (e-session-test--append-literal-v2-record
           store session-id v2-record)
          (e-session-append-context-curation-package
           store session-id (list :promotion v3-record :erasure nil))
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (records (mapcar #'e-session-aggregate-context-record
                                  (e-session-context-promotions
                                   reopened session-id)))
                 (projection (e-session-context-lifetime-projection
                              reopened session-id))
                 (promotion-messages
                  (plist-get projection :promotion-messages))
                 (fork (e-session-fork reopened session-id))
                 (fork-id (plist-get fork :id))
                 (fork-generation
                  (plist-get
                   (e-session-context-lifetime-projection reopened fork-id)
                   :generation))
                 (selected-head
                  (plist-get
                   (car (e-session-context-promotions reopened session-id))
                   :id))
                 (selected-fork
                  (e-session-fork reopened session-id :at selected-head))
                 (selected-generation
                  (plist-get
                   (e-session-context-lifetime-projection
                    reopened (plist-get selected-fork :id))
                   :generation)))
            (should (equal records (list v2-record v3-record)))
            (should (= (length (plist-get projection :promotions)) 1))
            (should (= (length (plist-get projection :curations)) 1))
            (should (equal
                     (mapcar (lambda (message)
                               (list (plist-get message :role)
                                     (plist-get message :content)))
                             promotion-messages)
                     (list
                      '(system "Promoted fact fact-v2: legacy value")
                      (list 'system web-result)
                      '(system "summarized replacement"))))
            (should (equal
                     (e-context-lifetime-generation-checkpoint fork-generation)
                     (list
                      '(:role system :content "C0")
                      '(:role system
                        :content "Promoted fact fact-v2: legacy value")
                      (list :role 'system :content web-result)
                      '(:role system :content "summarized replacement"))))
            (should (equal
                     (e-context-lifetime-generation-checkpoint
                      selected-generation)
                     '((:role system :content "C0")
                       (:role system
                        :content "Promoted fact fact-v2: legacy value"))))))
      (delete-directory directory t))))

(ert-deftest e-session-test-context-curation-package-is-one-replayable-unit ()
  "A mixed curation package persists, reopens, and stays clean on a fork."
  (let* ((directory (make-temp-file "e-session-context-package-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "context-package")
         (generation-id "generation-package")
         (response-id "response-package")
         (v3-record
          '(:record-version 3
            :type context-promotion
            :id "curation-package"
            :frame-id "frame-package"
            :generation-id "generation-package"
            :consumer-request-id "consumer-package"
            :response-entry-id "response-package"
            :items ((:kind exact :value "selected package semantic"
                     :source-observation-ids ("observation-package")
                     :source-refs ("source-package")
                     :source-fingerprints ("fingerprint-package")))))
         (erasure-record
          '(:record-version 1
            :type context-erasure
            :id "erasure-package"
            :frame-id "frame-package"
            :generation-id "generation-package"
            :consumer-request-id "consumer-package"
            :response-entry-id "response-package"
            :sources ((:source-observation-id "erasure-observation"
                       :source-ref "erasure-source"
                       :source-fingerprint "erasure-fingerprint"
                       :tool-call-id "tool-call-package")))))
    (unwind-protect
        (progn
          (let* ((session (e-session-create store :id session-id))
                 (root-id (plist-get session :root-event-id)))
            (e-session-append-context-generation
             store session-id
             (e-context-lifetime-generation-create
              :id generation-id
              :checkpoint '((:role system :content "package checkpoint"))
              :covered-session-boundary root-id))
            (e-session-append-message
             store session-id
             '(:id "tool-result-package" :role tool
               :content "raw package output")))
          (let* ((committed
                  (e-session-append-context-curation-package
                   store session-id
                   (list :promotion v3-record :erasure erasure-record)))
                 (package-id (plist-get committed :id))
                 (package-entry (plist-get committed :entry))
                 (control
                  (e-session-append-context-curation-response
                   store session-id "turn-package" response-id))
                 (assistant
                  (e-session-append-message
                   store session-id
                   '(:id "assistant-package" :role assistant
                     :content "package complete"))))
            (should (eq (plist-get package-entry :type)
                        'context-curation-package))
            (should (equal (plist-get (plist-get package-entry :promotion)
                                      :id)
                           "curation-package"))
            (should (equal (plist-get (plist-get package-entry :erasure)
                                      :id)
                           "erasure-package"))
            (should (equal (plist-get (e-session-entry-by-id
                                       store session-id package-id)
                                      :id)
                           package-id))
            (should (= (length (e-session-context-curations store session-id))
                       1))
            (let ((promotion (car (e-session-context-promotions
                                   store session-id))))
              (should (eq (plist-get promotion :type)
                          'context-promotion))
              (should (equal (plist-get promotion :id)
                             "curation-package"))
              (should (equal (plist-get promotion :created-at)
                             (plist-get package-entry :created-at)))
              (should (equal (plist-get promotion :context-record)
                             v3-record)))
            (should (equal (e-session-erased-tool-call-ids store session-id)
                           '("tool-call-package")))
            (should (equal (plist-get control :event-type)
                           'context-curation-response))
            (should (equal (plist-get assistant :role) 'assistant))
            ;; The audit/control and assistant entries advance the selected
            ;; head after the package write.  Retrying the exact package must
            ;; still be idempotent rather than comparing its old parent.
            (let ((retry
                   (e-session-append-context-curation-package
                    store session-id
                    (list :promotion v3-record :erasure erasure-record))))
              (should (plist-get retry :already-present))
              (should (equal (plist-get retry :id) package-id)))
            (should (= (length
                        (seq-filter
                         (lambda (entry)
                           (eq (plist-get entry :type)
                               'context-curation-package))
                         (e-session-current-path store session-id)))
                       1)))
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (package-entry
                  (seq-find
                   (lambda (entry)
                     (eq (plist-get entry :type)
                         'context-curation-package))
                   (e-session-current-path reopened session-id)))
                 (fork (e-session-fork reopened session-id
                                        :at "assistant-package"))
                 (fork-id (plist-get fork :id))
                 (source-projection
                  (prin1-to-string
                   (e-session-context-lifetime-projection
                    reopened session-id)))
                 (fork-projection
                  (prin1-to-string
                   (e-session-context-lifetime-projection
                    reopened fork-id))))
            (should package-entry)
            ;; The same package remains an exact retry after persistent
            ;; reopen, even though the current head is still the assistant
            ;; descendant rather than the package itself.
            (let ((retry
                   (e-session-append-context-curation-package
                    reopened session-id
                    (list :promotion v3-record :erasure erasure-record))))
              (should (plist-get retry :already-present))
              (should (equal (plist-get retry :id)
                             (plist-get package-entry :id))))
            (should (equal (plist-get package-entry :promotion)
                           v3-record))
            (should (equal (plist-get package-entry :erasure)
                           erasure-record))
            (should (= (length (e-session-context-curations
                                reopened session-id))
                       1))
            (should (equal (e-session-erased-tool-call-ids
                            reopened session-id)
                           '("tool-call-package")))
            (should (e-session-entry-by-id
                     reopened session-id response-id))
            (should (equal (plist-get
                            (plist-get (e-session-entry-by-id
                                        reopened session-id response-id)
                                       :payload)
                            :response-entry-id)
                           response-id))
            (should (=
                     (cl-count-if
                      (lambda (record)
                        (equal (plist-get record :type)
                               "context-curation-package"))
                      (e-session-storage-read-session-records
                       reopened session-id))
                     1))
            (should (string-match-p "selected package semantic"
                                    source-projection))
            (should (string-match-p "selected package semantic"
                                    fork-projection))
            ;; Clean forks carry the portable selected meaning, never source
            ;; suppression authority or its audit control.
            (should-not (e-session-erased-tool-call-ids reopened fork-id))
            (should-not (e-session-context-erasures reopened fork-id))
            (should-not (e-session-entry-by-id reopened fork-id response-id))
            (should-not (string-match-p "response-package" fork-projection))))
      (delete-directory directory t))))


(when nil
  ;; Retired direct/queued/Node-controller matrix.  Exact SQLite replay and
  ;; worker-loss propagation are covered by the Feature 87 session suite.
  (ert-deftest e-session-test-context-curation-package-retry-survives-audit-and-backends ()
  "Exact package retries survive audit failure, queueing, and controller writes."
  (let* ((v3-record
          '(:record-version 3
            :type context-promotion
            :id "retry-curation"
            :frame-id "retry-frame"
            :generation-id "retry-generation"
            :consumer-request-id "retry-consumer"
            :response-entry-id "retry-response"
            :items ((:kind exact :value "retry semantic"
                     :source-observation-ids ("retry-observation")
                     :source-refs ("retry-source")
                     :source-fingerprints ("retry-fingerprint")))))
         (package (list :promotion v3-record :erasure nil))
         (install
          (lambda (store session-id)
            (let* ((session (e-session-create store :id session-id))
                   (root-id (plist-get session :root-event-id)))
              (e-session-append-context-generation
               store session-id
               (e-context-lifetime-generation-create
                :id "retry-generation"
                :checkpoint '((:role system :content "retry context"))
                :covered-session-boundary root-id))
              (e-session-append-context-curation-package
               store session-id package))))
         (direct-directory (make-temp-file "e-session-retry-direct-" t))
         (queued-directory (make-temp-file "e-session-retry-queued-" t))
         (controller-directory (make-temp-file "e-session-retry-controller-" t)))
    (unwind-protect
        (progn
          (let* ((store (e-session-persistent-store-create direct-directory))
                 (session-id "retry-direct")
                 (_first (funcall install store session-id)))
            ;; The semantic package can be durable while its separate audit
            ;; append fails.  A retry must find the selected-path package after
            ;; that failure and after another descendant advances the head.
            (cl-letf (((symbol-function
                        'e-session-append-context-curation-response)
                       (lambda (&rest _)
                         (signal 'e-session-error (list "audit failure")))))
              (should-error
               (e-session-append-context-curation-response
                store session-id "retry-turn" "retry-response")
               :type 'e-session-error))
            (e-session-append-message
             store session-id
             '(:role assistant :content "head advanced"))
            (should (plist-get
                     (e-session-append-context-curation-package
                      store session-id package)
                     :already-present))
            (e-session-flush-write-queue store)
            (let ((reopened (e-session-persistent-store-create direct-directory)))
              (should (plist-get
                       (e-session-append-context-curation-package
                        reopened session-id package)
                       :already-present))))
          (let* ((store (e-session-persistent-index-store-create
                         queued-directory))
                 (session-id "retry-queued"))
            (funcall install store session-id)
            (should (plist-get
                     (e-session-append-context-curation-package
                      store session-id package)
                     :already-present))
            (e-session-flush-write-queue store)
            (let ((reopened (e-session-persistent-store-create queued-directory)))
              (should (plist-get
                       (e-session-append-context-curation-package
                        reopened session-id package)
                       :already-present))))
          (let* ((store (e-session-persistent-store-create controller-directory))
                 (session-id "retry-controller")
                 (submitted nil))
            (e-session-storage-enable store)
            (cl-letf (((symbol-function 'e-session-storage-commit-mutation)
                       (lambda (_store _session-id record)
                         (push record submitted))))
              (funcall install store session-id)
              (should (plist-get
                       (e-session-append-context-curation-package
                        store session-id package)
                       :already-present))
              (should (= (length
                          (seq-filter
                           (lambda (record)
                             (equal (plist-get record :type)
                                    "context-curation-package"))
                           submitted))
                         1)))))
      (delete-directory direct-directory t)
      (delete-directory queued-directory t)
      (delete-directory controller-directory t)))))

(ert-deftest e-session-test-context-erasure-query-is-path-scoped-and-not-forked ()
  "Erasure identities follow a selected path but are not copied to a clean fork."
  (let* ((directory (make-temp-file "e-session-context-erasure-path-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "context-erasure-path"))
    (unwind-protect
        (progn
          (let* ((session (e-session-create store :id session-id))
                 (root-id (plist-get session :root-event-id))
                 (generation
                  (e-session-append-context-generation
                   store session-id
                   (e-context-lifetime-generation-create
                    :id "generation-erasure"
                    :checkpoint
                    '((:role system :content "selected semantic context"))
                    :covered-session-boundary root-id)))
                 (before-id (plist-get generation :id))
                 (tool-message
                  (e-session-append-message
                   store session-id
                   '(:id "tool-result-1" :role tool :content "raw tool output")))
                 (before-erasure-id (plist-get tool-message :id))
                 (curation-package
                  (e-session-append-context-curation-package
                   store session-id
                   (list :promotion nil
                         :erasure
                         (e-session-test--literal-v1-erasure-record))))
                 (erasure-entry-id
                  (plist-get (plist-get curation-package :entry) :id))
                 (control
                  (e-session-append-context-curation-response
                   store session-id "turn-erasure" "response-1"))
                 (descendant
                  (e-session-append-message
                   store session-id
                   '(:id "after-erasure" :role assistant :content "continued"))))
            (should (equal (e-session-erased-tool-call-ids
                            store session-id before-id)
                           nil))
            (should (equal (e-session-erased-tool-call-ids
                            store session-id before-erasure-id)
                           nil))
            (should (equal (e-session-erased-tool-call-ids
                            store session-id erasure-entry-id)
                           '("tool-call-1")))
            (should (equal (e-session-erased-tool-call-ids
                            store session-id (plist-get descendant :id))
                           '("tool-call-1")))
            (should (equal (mapcar #'e-context-lifetime-curation-erasure-tool-call-ids
                                   (e-session-context-erasures store session-id))
                           '(("tool-call-1"))))
            (should (eq (plist-get (e-session-entry-by-id
                                    store session-id erasure-entry-id)
                                   :type)
                        'context-curation-package))
            (should (equal (plist-get (e-session-entry-by-id
                                       store session-id "response-1")
                                      :event-type)
                           'context-curation-response))
            (e-session-flush-write-queue store)
            (let* ((reopened (e-session-persistent-store-create directory))
                   (reopened-erasures
                    (e-session-context-erasures reopened session-id))
                   (sibling-message
                    (e-session-append-message
                     reopened session-id
                     '(:parent-id "tool-result-1"
                       :role assistant :content "sibling continuation")))
                   (sibling-head-id (plist-get sibling-message :id))
                   (sibling-path
                    (e-session-current-path reopened session-id sibling-head-id))
                   (sibling-projection
                    (prin1-to-string
                     (e-session-context-lifetime-projection
                      reopened session-id sibling-head-id)))
                   ;; A fork made from the post-erasure head is also clean:
                   ;; it copies portable semantic context, never the source's
                   ;; audit/control records or erasure query state.
                   (post-erasure-fork
                    (e-session-fork reopened session-id :at "after-erasure"))
                   (post-erasure-fork-id (plist-get post-erasure-fork :id))
                   (post-erasure-projection
                    (prin1-to-string
                     (e-session-context-lifetime-projection
                      reopened post-erasure-fork-id))))
              (should (equal (e-session-erased-tool-call-ids
                              reopened session-id "after-erasure")
                             '("tool-call-1")))
              (should-error
               (e-session-erased-tool-call-ids
                reopened session-id "unknown-selected-head")
               :type 'e-session-error)
              ;; Capture the source-path audit view before moving the live
              ;; session head onto the intentionally clean sibling branch.
              (should (= (length reopened-erasures) 1))
              (should (e-session-entry-by-id reopened session-id "response-1"))
              ;; The same-session sibling branches before the erasure and
              ;; remains free of it even after receiving a new descendant.
              (should-not (e-session-erased-tool-call-ids
                           reopened session-id sibling-head-id))
              ;; An exact package identity on an inactive sibling is not a
              ;; retry for the selected path and must not be silently reused.
              (should-error
               (e-session-append-context-curation-package
                reopened session-id
                (plist-get curation-package :package))
               :type 'e-session-error)
              (should-not
               (seq-find (lambda (entry)
                           (memq (plist-get entry :type)
                                 '(context-erasure
                                   context-curation-package)))
                         sibling-path))
              (should-not
               (seq-find (lambda (entry)
                           (eq (plist-get entry :event-type)
                               'context-curation-response))
                         sibling-path))
              (should (string-match-p "sibling continuation"
                                      sibling-projection))
              ;; The post-erasure clean fork likewise does not copy the
              ;; detached erasure/control records or query result.
              (should-not (e-session-erased-tool-call-ids
                           reopened post-erasure-fork-id))
              (should-not (e-session-context-erasures
                           reopened post-erasure-fork-id))
              (should-not (e-session-entry-by-id
                           reopened post-erasure-fork-id "response-1"))
              (should (string-match-p "selected semantic context"
                                      sibling-projection))
              (should (string-match-p "selected semantic context"
                                      post-erasure-projection))
              (should-not (string-match-p "erasure-1" sibling-projection))
              (should-not (string-match-p "response-1" sibling-projection))
              (should-not (string-match-p "erasure-1"
                                          post-erasure-projection))
              (should-not (string-match-p "response-1"
                                          post-erasure-projection)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-context-erasure-append-detaches-input-through-reopen ()
  "Session-owned erasures retain original identities after caller mutation."
  (let* ((directory (make-temp-file "e-session-context-erasure-detach-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "context-erasure-detach"))
    (unwind-protect
        (let* ((session (e-session-create store :id session-id))
               (root-id (plist-get session :root-event-id))
               (record
                (e-session-test--copy-value
                 (e-session-test--literal-v1-erasure-record "detached"))))
          (e-session-append-context-generation
           store session-id
           (e-context-lifetime-generation-create
            :id "generation-erasure"
            :checkpoint '((:role system :content "detach context"))
            :covered-session-boundary root-id))
          (e-session-append-context-curation-package
           store session-id (list :promotion nil :erasure record))
          (let ((source (car (plist-get record :sources))))
            (dolist (value (list (plist-get record :id)
                                 (plist-get record :frame-id)
                                 (plist-get record :generation-id)
                                 (plist-get record :consumer-request-id)
                                 (plist-get record :response-entry-id)
                                 (plist-get source :source-observation-id)
                                 (plist-get source :source-ref)
                                 (plist-get source :source-fingerprint)))
              (setf (aref value 0) ?X))
            (plist-put source :tool-call-id "replaced-tool-call"))
          (let ((stored (car (e-session-context-erasures store session-id))))
            (should (equal (plist-get stored :id) "erasure-detached"))
            (should (equal
                     (plist-get (car (plist-get stored :sources))
                                :tool-call-id)
                     "tool-call-detached"))
            (should (equal (e-session-erased-tool-call-ids
                            store session-id)
                           '("tool-call-detached"))))
          (e-session-flush-write-queue store)
          (let ((reopened (e-session-persistent-store-create directory)))
            (let ((stored (car (e-session-context-erasures
                                reopened session-id))))
              (should (equal (plist-get stored :id) "erasure-detached"))
              (should (equal
                       (plist-get (car (plist-get stored :sources))
                                  :tool-call-id)
                       "tool-call-detached")))
            (should (equal (e-session-erased-tool-call-ids
                            reopened session-id)
                           '("tool-call-detached")))))
      (delete-directory directory t))))



(ert-deftest e-session-test-context-codecs-own-malformed-append-and-replay ()
  "Both context record kinds reject malformed versions at the session boundary."
  (cl-labels
      ((without-key
        (record key)
        (let (result)
          (while record
            (let ((current-key (pop record))
                  (value (pop record)))
              (unless (eq current-key key)
                (setq result (append result (list current-key value))))))
          result))
       (with-key
        (record key value)
        (let ((copy (copy-tree record)))
          (plist-put copy key value)))
       (duplicate-version
        (record)
        (append (list :record-version 2 :record-version 2)
                (cddr record)))
       (variants
        (record)
        (list (without-key record :record-version)
              (duplicate-version record)
              (with-key record :extra "unknown")
              (with-key record :record-version "2")
              (with-key record :record-version nil)
              (with-key record :record-version '(2))))
       (replay-fails
        (type context-record)
        (let* ((store (e-session-store-create))
               (session-id (format "corrupt-%s"
                                   (substring (symbol-name type) 9)))
               (session (e-session-create store :id session-id))
               (parent-id (plist-get session :root-event-id)))
          (when (eq type 'context-promotion)
            (setq parent-id
                  (plist-get
                   (e-session-append-context-generation
                    store session-id
                    (e-context-lifetime-generation-create
                     :id "replay-generation"
                     :checkpoint '((:role system :content "policy"))
                     :covered-session-boundary parent-id))
                   :id)))
          (should-error
           (e-session--apply-physical-record
            store
            (list :type (symbol-name type)
                  :session-id session-id
                  :id (format "corrupt-entry-%s"
                              (substring (symbol-name type) 9))
                  :parent-id parent-id
                  :timestamp "2026-08-24T00:00:01Z"
                  :context-record context-record))
           :type 'e-session-error))))
    (let* ((generation
            (e-context-lifetime-generation-create
             :id "append-generation"
             :checkpoint '((:role system :content "policy"))
             :covered-session-boundary "entry-0"))
           (generation-record
            (e-context-lifetime-generation-record generation))
           (promotion-record
            '(:record-version 2
              :type context-promotion
              :id "append-promotion"
              :frame-id "frame-1"
              :generation-id "append-generation"
              :consumer-request-id "consumer-1"
              :response-entry-id "response-1"
              :facts ((:id "fact-1" :value "selected"))
              :source-observation-ids ("observation-1")
              :source-refs ("external:source")
              :source-fingerprints ("source-fingerprint"))))
      (let ((store (e-session-store-create)))
        (e-session-create store :id "append-context")
        (dolist (bad (variants generation-record))
          (should-error
           (e-session-append-context-generation
            store "append-context" bad)
           :type 'e-session-error))
        (e-session-append-context-generation
         store "append-context" generation-record)
        (dolist (bad (variants promotion-record))
          (should-error
           (e-session-append-context-curation-package
            store "append-context"
            (list :promotion bad :erasure nil))
           :type 'e-session-error))
        (should (= (length (e-session-context-generations
                            store "append-context"))
                   1))
        (should-not (e-session-context-promotions store "append-context")))
      (dolist (bad (variants generation-record))
        (replay-fails 'context-generation bad))
      (dolist (bad (variants promotion-record))
        (replay-fails 'context-promotion bad)))))

(ert-deftest e-session-test-legacy-context-records-do-not-restore-runtime-frames ()
  "Legacy context lifetime journal lines preserve ordinary transcript only."
  (let* ((directory (make-temp-file "e-session-context-legacy-" t))
         (sessions-directory (expand-file-name "sessions" directory))
         (session-id "legacy-context")
         (journal (expand-file-name
                   (concat session-id ".jsonl") sessions-directory)))
    (unwind-protect
        (progn
          (make-directory sessions-directory)
          (with-temp-file journal
            (dolist
                (record
                 (list
                  (list :type "session" :session-id session-id :id "root"
                        :timestamp "2026-08-23T23:59:58Z")
                  (list :type "message" :session-id session-id
                        :id "ordinary-entry" :parent-id "root"
                        :timestamp "2026-08-23T23:59:59Z"
                        :message '(:id "ordinary-entry" :role "user"
                                   :content "retain me"))
                  (list :type "context-generation" :session-id session-id
                        :id "legacy-generation" :parent-id "ordinary-entry"
                        :timestamp "2026-08-24T00:00:00Z"
                        :context-record
                        '(:record-version 1 :type "context-generation"
                          :id "legacy-generation" :checkpoint nil))
                  (list :type "context-frame" :session-id session-id
                        :id "legacy-frame" :parent-id "legacy-generation"
                        :timestamp "2026-08-24T00:00:01Z"
                        :context-record
                        '(:record-version 1 :type "context-frame"
                          :id "legacy-frame" :generation-id "legacy-generation"
                          :state "open"))
                  (list :type "context-promotion" :session-id session-id
                        :id "legacy-promotion" :parent-id "legacy-frame"
                        :timestamp "2026-08-24T00:00:02Z"
                        :context-record
                        '(:record-version 1 :type "context-promotion"
                          :id "legacy-promotion" :frame-id "legacy-frame"))
                  (list :type "context-frame-settlement"
                        :session-id session-id :id "legacy-settlement"
                        :parent-id "legacy-frame"
                        :timestamp "2026-08-24T00:00:03Z"
                        :context-record
                        '(:record-version 1 :type "context-frame-settlement"
                          :frame-id "legacy-frame" :status "acknowledged"))))
              (insert (json-encode record) "\n")))
          (let ((reopened
                 (e-session-test--replay-legacy-copy directory session-id)))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :content))
                                   (e-session-messages reopened session-id))
                           '("retain me")))
            (should-not (e-session-context-generations reopened session-id))
            (should-not (e-session-context-promotions reopened session-id))
            (should-not
             (plist-member
              (plist-get (e-session-checkpoint-manifest reopened session-id)
                         :context-lifetime)
              :frames))))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-log-deduplicates-and-clear-survives-replay ()
  "Board log identity and reset boundaries remain durable across reopen."
  (let ((directory (make-temp-file "e-session-board-log-" t)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (first '(:id "board-1" :kind output :content "old"))
               (second '(:id "board-2" :kind output :content "new")))
          (e-session-create store :id "board-session")
          (e-session-append-board-message store "board-session" first)
          (e-session-append-board-message store "board-session" first)
          (should (= (length (e-session-board-messages
                              store "board-session"))
                     1))
          (e-session-clear-board-messages store "board-session")
          (e-session-append-board-message store "board-session" second)
          (e-session-flush-write-queue store)
          (let ((reopened (e-session-persistent-store-create directory)))
            (let ((messages (e-session-board-messages
                             reopened "board-session")))
              (should (= (length messages) 1))
              (should (equal (plist-get (car messages) :id) "board-2"))
              (should (equal (plist-get (car messages) :content) "new")))
            (e-session-append-board-message reopened "board-session" second)
            (should (= (length (e-session-board-messages
                                reopened "board-session"))
                       1))))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-log-rejects-divergent-typed-envelope-retries ()
  "A typed journal retry must exactly match the envelope it already appended."
  (let ((store (e-session-store-create))
        (first '(:id "chain" :record-type processing-chain
                 :root-message-id "root" :created-at "fixed"))
        (divergent '(:id "chain" :record-type processing-chain
                     :root-message-id "other" :created-at "fixed")))
    (e-session-create store :id "board-session")
    (e-session-append-board-message store "board-session" first)
    (e-session-append-board-message store "board-session" first)
    (should-error
     (e-session-append-board-message store "board-session" divergent)
     :type 'e-session-board-message-conflict)
    (should (equal (e-session-board-messages store "board-session")
                   (list first)))))

(ert-deftest e-session-test-processing-journal-rejects-cross-record-reentrancy-in-order ()
  "The live ledger and replay retain the same durable processing order."
  (let ((directory (make-temp-file "e-session-processing-order-" t))
        reentrant-error)
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (session-id "board-session"))
          (e-session-create store :id session-id)
          (let ((board
                 (e-board-create
                  :id "board"
                  :processing-record-notification-function
                  (lambda (callback-board record _type)
                    (e-session-append-board-message
                     store session-id
                     (e-board-processing-record-envelope record))
                    (condition-case error
                        (e-board-record-processing-chain
                         callback-board :id "nested" :root-message-id "root"
                         :candidate-message-id "nested" :caused-by-message-id "root"
                         :processor-history nil :processing-depth 0)
                      (e-board-id-conflict
                       (setq reentrant-error error)))))))
            (e-board-record-processing-chain
             board :id "outer" :root-message-id "root"
             :candidate-message-id "outer" :caused-by-message-id "root"
             :processor-history nil :processing-depth 0)
            (should reentrant-error)
            (should (equal (mapcar #'e-board-processing-chain-id
                                   (e-board-list-processing-chains board))
                           '("outer")))
            (let* ((reopened (e-session-persistent-store-create directory))
                   (restored (e-board-create :id "restored" :register nil)))
              (dolist (envelope (e-session-board-messages reopened session-id))
                (e-board-import-processing-record restored envelope))
              (should (equal (mapcar #'e-board-processing-chain-id
                                     (e-board-list-processing-chains restored))
                             '("outer"))))))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-journal-is-private-from-generic-session-view ()
  "Generic session mutation cannot alter the private board journal or its index."
  (let ((store (e-session-store-create))
        (message '(:id "board-1" :kind output :content "retained")))
    (e-session-create store :id "board-session")
    (e-session-append-board-message store "board-session" message)
    (let ((session (e-session-get store "board-session")))
      (should-not (plist-member session :board-messages))
      (should-not (plist-member session :board-message-id-index))
      (plist-put session :board-messages
                 (list '(:id "board-1" :kind output :content "mutated")))
      (plist-put session :board-message-id-index (make-hash-table :test 'equal)))
    (e-session-append-board-message store "board-session" message)
    (should (equal (e-session-board-messages store "board-session")
                   (list message)))))

(ert-deftest e-session-test-board-log-rejects-cyclic-envelope-values ()
  "Board journals reject cyclic cons, vector, and hash table envelope values."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "board-session")
    (dolist (value
             (list (let ((cycle (list nil)))
                     (setcar cycle cycle)
                     cycle)
                   (let ((cycle (vector nil)))
                     (aset cycle 0 cycle)
                     cycle)
                   (let ((cycle (make-hash-table :test 'eq)))
                     (puthash :self cycle cycle)
                     cycle)))
      (should-error
       (e-session-append-board-message
        store "board-session" (list :id "cycle" :value value))
       :type 'e-session-board-message-cycle))
    (should-not (e-session-board-messages store "board-session"))))

(ert-deftest e-session-test-board-messages-loads-an-indexed-session ()
  "Board message access loads an unloaded indexed session before reading it."
  (let ((directory (make-temp-file "e-session-board-indexed-access-" t)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (session-id "board-session"))
          (e-session-create store :id session-id)
          (e-session-append-board-message
           store session-id '(:id "message-1" :kind output))
          (e-session-flush-write-queue store)
          (let ((indexed (e-session-persistent-index-store-create directory)))
            (should-not (plist-get (e-session-aggregate-peek-session indexed session-id)
                                   :loaded))
            (should (equal (mapcar (lambda (message) (plist-get message :id))
                                   (e-session-board-messages indexed session-id))
                           '("message-1")))
            (should (plist-get (e-session-aggregate-peek-session indexed session-id)
                               :loaded))))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-log-freezes-input-and-returned-envelopes ()
  "Board journal state remains private across input and return-value mutation."
  (let* ((directory (make-temp-file "e-session-board-freeze-" t))
         (store (e-session-persistent-index-store-create directory))
         (id (copy-sequence "frozen"))
         (value (copy-sequence "top-level"))
         (nested-value (copy-sequence "original"))
         (nested (list nested-value))
         (envelope (list :id id :value value :attributes (list :nested nested))))
    (unwind-protect
        (progn
          (e-session-create store :id "board-session")
          (let ((returned (e-session-append-board-message
                           store "board-session" envelope)))
            (aset id 0 ?x)
            (aset value 0 ?x)
            (aset nested-value 0 ?x)
            (aset (plist-get returned :id) 0 ?x)
            (aset (plist-get returned :value) 0 ?x)
            (aset (car (plist-get (plist-get returned :attributes) :nested))
                  0 ?x))
          (let ((message (car (e-session-board-messages store "board-session"))))
            (should (equal (plist-get message :id) "frozen"))
            (should (equal (plist-get message :value) "top-level"))
            (should (equal (plist-get (plist-get message :attributes) :nested)
                           '("original"))))
          (e-session-flush-write-queue store)
          (let ((message (car (e-session-board-messages
                               (e-session-persistent-store-create directory)
                               "board-session"))))
            (should (equal (plist-get message :id) "frozen"))
            (should (equal (plist-get message :value) "top-level"))
            (should (equal (plist-get (plist-get message :attributes) :nested)
                           '("original")))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-checkpoint-manifest-deep-freezes-all-values ()
  "Manifest mutation cannot alter generic session state or retained entry IDs."
  (let* ((store (e-session-store-create))
         (session-id (copy-sequence "session-1"))
         (name (copy-sequence "session name"))
         (project-root (copy-sequence "project root"))
         (branch-id (copy-sequence "branch-1")))
    (e-session-create store :id session-id
                      :metadata (list :name name :project-root project-root))
    (e-session-append-message store session-id
                              (list :role 'user :content "message"))
    (e-session-set-current-branch store session-id branch-id)
    (let* ((manifest (e-session-checkpoint-manifest store session-id))
           (root (plist-get manifest :root))
           (entry-id (aref (plist-get manifest :entry-ids) 0)))
      (dolist (value (list (plist-get manifest :session-id)
                           (plist-get root :id)
                           (plist-get root :name)
                           (plist-get (plist-get root :metadata) :project-root)
                           (plist-get root :current-branch)
                           entry-id))
        (aset value 0 ?x)))
    (let ((session (e-session-get store session-id)))
      (should (equal session-id "session-1"))
      (should (equal (plist-get session :name) "session name"))
      (should (equal (plist-get (plist-get session :metadata) :project-root)
                     "project root"))
      (should (equal (plist-get session :current-branch) "branch-1"))
      (should (string-prefix-p "01" (plist-get session :root-event-id)))
      (should (string-prefix-p "01"
                               (plist-get (car (e-session-messages store session-id))
                                          :id))))))

(ert-deftest e-session-test-board-log-rejects-invalid-record-types ()
  "Only absent and supported processing record types enter the board journal."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "board-session")
    (e-session-append-board-message store "board-session" '(:id "message"))
    (dolist (record-type '("" 0 :json-false unknown "unknown"))
      (should-error
       (e-session-append-board-message
        store "board-session" (list :id "message" :record-type record-type))
       :type 'e-session-board-message-invalid-record-type))
    (should (equal (mapcar #'e-session-aggregate-board-message-identity
                           (e-session-board-messages store "board-session"))
                   '((board-message . "message"))))))

(ert-deftest e-session-test-checkpoint-manifest-detaches-board-identities ()
  "Checkpoint manifest mutation cannot change the private board journal."
  (let* ((store (e-session-store-create))
         (session-id "board-session")
         (board-id (copy-sequence "board-1"))
         (principal (copy-sequence "principal-1"))
         (message-id (copy-sequence "record-1")))
    (e-session-create store :id session-id)
    (e-session-declare-board-state store session-id principal board-id)
    (e-session-append-board-message
     store session-id
     (list :id message-id :record-type 'processing-chain))
    (let* ((manifest (e-session-checkpoint-manifest store session-id))
           (identity (aref (plist-get manifest :board-message-identities) 0))
           (state (plist-get manifest :board-state)))
      (aset (plist-get identity :id) 0 ?x)
      (aset (plist-get state :board-id) 0 ?x)
      (aset (plist-get state :principal) 0 ?x))
    (should (equal (plist-get (car (e-session-board-messages store session-id)) :id)
                   "record-1"))
    (should (equal (plist-get (plist-get (e-session-get store session-id)
                                          :board-session-state)
                              :board-id)
                   "board-1"))
    (should (equal (plist-get (plist-get (e-session-get store session-id)
                                          :board-session-state)
                              :principal)
                   "principal-1"))))

(ert-deftest e-session-test-board-log-canonicalizes-processing-record-types ()
  "String and symbol processing record types share one durable identity."
  (let ((store (e-session-store-create))
        (symbol-envelope '(:id "record-1" :record-type processing-chain))
        (string-envelope '(:id "record-1" :record-type "processing-chain")))
    (e-session-create store :id "board-session")
    (e-session-append-board-message store "board-session" symbol-envelope)
    (e-session-append-board-message store "board-session" string-envelope)
    (let ((messages (e-session-board-messages store "board-session")))
      (should (= (length messages) 1))
      (should (eq (plist-get (car messages) :record-type)
                  'processing-chain)))))

(ert-deftest e-session-test-board-log-replay-deduplicates-identical-typed-envelope ()
  "Restart ignores repeated typed board envelopes with equal durable values."
  (let ((directory (make-temp-file "e-session-board-replay-duplicate-" t)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (session-id "board-session")
               (message '(:id "chain" :record-type processing-chain
                          :root-message-id "root" :created-at "fixed"))
               (record (list :type "board-message" :session-id session-id
                             :message message)))
          (e-session-create store :id session-id)
          (e-session-append-board-message store session-id message)
          (e-session-storage-commit-mutation store session-id record)
          (let ((messages (e-session-board-messages
                           (e-session-persistent-store-create directory) session-id)))
            (should (= (length messages) 1))
            (should (equal (plist-get (car messages) :id) "chain"))
            (should (eq (plist-get (car messages) :record-type)
                        'processing-chain))))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-log-replay-rejects-divergent-typed-envelope ()
  "Restart rejects repeated typed board envelopes with divergent durable values."
  (let ((directory (make-temp-file "e-session-board-replay-conflict-" t)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (session-id "board-session")
               (message '(:id "chain" :record-type processing-chain
                          :root-message-id "root" :created-at "fixed"))
               (divergent '(:id "chain" :record-type processing-chain
                            :root-message-id "other" :created-at "fixed")))
          (e-session-create store :id session-id)
          (e-session-append-board-message store session-id message)
          (e-session-storage-commit-mutation
           store session-id
           (list :type "board-message" :session-id session-id
                 :message divergent))
          (should-error
           (e-session-board-messages
            (e-session-persistent-store-create directory) session-id)
           :type 'e-session-board-message-conflict))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-log-keeps-colliding-record-kinds-across-restart ()
  "Board messages and processing records share raw ids without journal loss."
  (let ((directory (make-temp-file "e-session-board-collision-" t)))
    (unwind-protect
        (let ((store (e-session-persistent-store-create directory)))
          (e-session-create store :id "board-session")
          (dolist (envelope '((:id "shared" :kind output)
                              (:id "shared" :record-type processing-chain)
                              (:id "shared" :record-type processing-result)))
            (e-session-append-board-message store "board-session" envelope))
          (e-session-flush-write-queue store)
          (let ((reopened (e-session-persistent-store-create directory)))
            (should (equal
                     (mapcar #'e-session-aggregate-board-message-identity
                             (e-session-board-messages reopened "board-session"))
                     '((board-message . "shared")
                       (processing-chain . "shared")
                       (processing-result . "shared"))))))
      (delete-directory directory t))))

(ert-deftest e-session-test-append-message-preserves-order ()
  "Messages are returned in insertion order."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1"
                              '(:id "msg-1" :role user :content "hello"))
    (e-session-append-message store "session-1"
                              '(:id "msg-2" :role assistant :content "hi"))
    (should (equal (mapcar (lambda (message) (plist-get message :id))
                           (e-session-messages store "session-1"))
                   '("msg-1" "msg-2")))))


(ert-deftest e-session-test-append-message-tracks-latest-assistant-marker ()
  "Message appends maintain the latest assistant marker for unread checks."
  (let* ((store (e-session-store-create))
         (session-id "session-assistant-marker"))
    (e-session-create store :id session-id)
    (e-session-append-message
     store session-id
     '(:id "user-1" :role user :content "hello"))
    (should-not
     (plist-get (e-session-get store session-id) :latest-assistant-marker))
    (e-session-append-message
     store session-id
     '(:id "assistant-1" :role assistant :content "one"))
    (should (equal
             (plist-get (e-session-get store session-id)
                        :latest-assistant-marker)
             "assistant-1"))
    (e-session-append-message
     store session-id
     '(:id "assistant-2" :role assistant :content "two"))
    (should (equal
             (plist-get (car (e-session-list store))
                        :latest-assistant-marker)
              "assistant-2"))))

(ert-deftest e-session-test-append-assistant-allocates-board-output-sequence ()
  "New assistant entries receive a stable session-local board output sequence."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "output-sequence")
    (e-session-append-message store "output-sequence" '(:role user :content "one"))
    (let ((first (e-session-append-message
                  store "output-sequence" '(:role assistant :content "two")))
          (second (e-session-append-message
                   store "output-sequence" '(:role assistant :content "three"))))
      (should (= (plist-get first :board-output-sequence) 1))
      (should (= (plist-get second :board-output-sequence) 2)))))

(ert-deftest e-session-test-board-output-sequence-continues-after-replay ()
  "Reopened sessions allocate past stored assistant output identities."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "output-replay")
          (e-session-append-message
           store "output-replay" '(:role assistant :content "first"))
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (message (e-session-append-message
                           reopened "output-replay"
                           '(:role assistant :content "second"))))
            (should (= (plist-get message :board-output-sequence) 2))))
      (delete-directory directory t))))

(ert-deftest e-session-test-append-activity-allocates-board-activity-sequence ()
  "Durable activity entries receive one stable session-local publication sequence."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "activity-sequence")
    (let ((first (e-session-append-activity-event
                  store "activity-sequence" "turn" 'tool-started nil))
          (second (e-session-append-activity-event
                   store "activity-sequence" "turn" 'tool-finished nil)))
      (should (= (plist-get first :board-activity-sequence) 1))
      (should (= (plist-get second :board-activity-sequence) 2)))))

(ert-deftest e-session-test-board-activity-sequence-continues-after-replay ()
  "Reopened sessions retain their durable activity publication high watermark."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "activity-replay")
          (e-session-append-activity-event
           store "activity-replay" "turn" 'tool-started nil)
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (event (e-session-append-activity-event
                         reopened "activity-replay" "turn" 'tool-finished nil)))
            (should (= (plist-get event :board-activity-sequence) 2))))
      (delete-directory directory t))))


(ert-deftest e-session-test-ulid-generation-is-ordered-and-opaque ()
  "Generated durable entry ids are ULID strings ordered by creation."
  (let ((ids nil))
    (cl-letf (((symbol-function 'float-time)
               (let ((times '(1770000000.001 1770000000.001 1770000000.002)))
                 (lambda (&optional _time)
                   (prog1 (car times)
                     (setq times (or (cdr times) times)))))))
      (setq ids (list (e-session-generate-ulid)
                      (e-session-generate-ulid)
                      (e-session-generate-ulid))))
    (dolist (id ids)
      (should (string-match-p "\\`[0-9A-HJKMNP-TV-Z]\\{26\\}\\'" id)))
    (should (equal ids (sort (copy-sequence ids) #'string<)))))

(ert-deftest e-session-test-generate-ulid-does-not-force-garbage-collect ()
  "Generated durable entry ids do not force global garbage collection."
  (cl-letf (((symbol-function 'garbage-collect)
             (lambda (&rest _args)
               (error "garbage-collect should not run"))))
    (dotimes (_ 5)
      (should (stringp (e-session-generate-ulid))))))

(ert-deftest e-session-test-append-message-assigns-entry-ids-and-parent-links ()
  "Appending messages assigns durable ids and links to the previous head."
  (let ((store (e-session-store-create)))
    (let* ((root (car (e-session-session-events
                       store
                       (plist-get (e-session-create store :id "session-1") :id))))
           (first (e-session-append-message
                   store "session-1" '(:role user :content "hello")))
           (second (e-session-append-message
                    store "session-1" '(:role assistant :content "hi")))
           (path (e-session-current-path store "session-1")))
      (should (string-match-p "\\`[0-9A-HJKMNP-TV-Z]\\{26\\}\\'"
                              (plist-get first :id)))
      (should (eq (plist-get root :event-type) 'session-created))
      (should-not (plist-get root :parent-id))
      (should (equal (plist-get first :parent-id)
                     (plist-get root :id)))
      (should (equal (plist-get second :parent-id)
                     (plist-get first :id)))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id)) path)
                     (list (plist-get root :id)
                           (plist-get first :id)
                           (plist-get second :id)))))))



(ert-deftest e-session-test-missing-session-surfaces-error ()
  "Appending to a missing session surfaces a domain error."
  (let ((store (e-session-store-create)))
    (should-error
     (e-session-append-message store "missing" '(:role user :content "x"))
     :type 'e-session-missing)))

(ert-deftest e-session-test-set-message-display-updates-in-memory ()
  "Setting a message's display disposition flips its stored `:display'."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (let ((message (e-session-append-message
                    store "session-1"
                    '(:id "msg-1" :role assistant :content "hi"))))
      (e-session-set-message-display store "session-1"
                                     (plist-get message :id) 'hidden)
      (should (eq (plist-get (car (e-session-messages store "session-1"))
                             :display)
                  'hidden)))))

(ert-deftest e-session-test-set-message-display-survives-reload ()
  "A hidden-display update replays from disk so hiding is durable."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-append-message
           store "session-1" '(:id "msg-1" :role assistant :content "hi"))
          (e-session-set-message-display store "session-1" "msg-1" 'hidden)
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (eq (plist-get (car (e-session-messages loaded "session-1"))
                                   :display)
                        'hidden))))
      (delete-directory directory t))))

(ert-deftest e-session-test-message-origin-survives-reload-as-symbol ()
  "Persistent input provenance replays in the runtime's symbol form."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-append-message
           store "session-1"
           '(:id "msg-1" :role user :origin harness :content "repair"))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (message (car (e-session-messages loaded "session-1"))))
            (should (eq (plist-get message :origin) 'harness))))
      (delete-directory directory t))))

(ert-deftest e-session-test-persistent-session-generates-id-and-reloads ()
  "Persistent sessions get generated ids and replay messages in order."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session (e-session-create store))
         (session-id (plist-get session :id)))
    (unwind-protect
        (progn
          (should (string-match-p
                   "\\`[0-9]\\{8\\}T[0-9]\\{6\\}-[0-9a-f]\\{12\\}\\'"
                   session-id))
          (e-session-append-message
           store session-id '(:id "msg-1" :role user :content "hello"))
          (e-session-append-message
           store session-id '(:id "msg-2" :role assistant :content "hi"))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (mapcar (lambda (message) (plist-get message :id))
                           (e-session-messages loaded session-id))
                           '("msg-1" "msg-2")))))
      (delete-directory directory t))))

(ert-deftest e-session-test-persistent-appends-avoid-noisy-append-api ()
  "Persistent session appends avoid the API that emits write messages."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (append-to-file-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'append-to-file)
                   (lambda (start end filename)
                     (setq append-to-file-called t)
                     (write-region start end filename t 'silent))))
          (let* ((session (e-session-create store :id "session-quiet"))
                 (session-id (plist-get session :id)))
            (should-not append-to-file-called)
            (let ((append-to-file-called nil))
              (e-session-append-message
               store session-id
               '(:id "msg-1" :role user :content "quiet append"))
              (should-not append-to-file-called))
            (let* ((loaded (e-session-persistent-store-create directory))
                   (messages (e-session-messages loaded session-id)))
              (should (equal (plist-get (car messages) :content)
                             "quiet append")))))
      (delete-directory directory t))))









(ert-deftest e-session-test-load-session-start-replays-in-chunks ()
  "Chunked persistent session loading returns before replay completion."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session (e-session-create store :id "chunked-session"))
         (session-id (plist-get session :id)))
    (unwind-protect
        (progn
          (dotimes (index 8)
            (e-session-append-message
             store session-id
             (list :id (format "msg-%d" index)
                   :role 'user
                   :content (format "chunked message %d with enough bytes"
                                    index))))
          ;; Leave a deterministic suffix beyond the latest checkpoint so the
          ;; cooperative current-store reader must issue several bounded pages.
          (dotimes (offset 4)
            (let* ((index (+ 8 offset))
                   (id (format "msg-%d" index))
                   (parent-id (format "msg-%d" (1- index))))
              (e-session-storage-commit-mutation
               store session-id
               (list :type "message" :session-id session-id
                     :id id :parent-id parent-id
                     :timestamp "2026-09-02T00:00:00Z"
                     :message
                     (list :id id :parent-id parent-id :role 'user
                           :content (format
                                     "chunked message %d with enough bytes"
                                     index))))))
          (let ((loaded (e-session-persistent-index-store-create directory))
                result
                failure
                progress
                request)
            (setq request
                  (e-session-load-session-start
                   loaded session-id
                   :chunk-bytes 2
                   :on-progress (lambda (payload)
                                  (push payload progress))
                   :on-done (lambda (session)
                              (setq result session))
                   :on-error (lambda (err)
                               (setq failure err))))
            (should (e-request-lifecycle-p request))
            (should (eq (e-request-lifecycle-state request) 'started))
            (should-not result)
            (let ((deadline (+ (float-time) 5)))
              (while (and (not result) (not failure) (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should-not failure)
            (should (eq (e-request-lifecycle-state request) 'finished))
            (should (plist-get result :loaded))
            (should (< 1 (length progress)))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :content))
                                   (e-session-messages loaded session-id))
                           (mapcar (lambda (index)
                                     (format
                                      "chunked message %d with enough bytes"
                                      index))
                                   (number-sequence 0 11))))))
      (delete-directory directory t))))

(ert-deftest e-session-test-load-requires-explicit-resume-checkpoint ()
  "Normal session load never falls back to a full checkpoint-less replay."
  (let* ((directory (make-temp-file "e-session-no-checkpoint-" t))
         (store (e-session-persistent-index-store-create directory)))
    (unwind-protect
        (progn
          (e-session-storage-commit-mutation
           store "checkpoint-less"
           '(:type "session" :session-id "checkpoint-less" :id "root"
             :timestamp "2026-08-10T00:00:00Z"))
          (should-error (e-session-load-session store "checkpoint-less")
                        :type 'e-session-checkpoint-missing))
      (delete-directory directory t))))




(ert-deftest e-session-test-persistent-replay-preserves-entry-ids ()
  "Persistent replay keeps durable ids and parent links instead of regenerating."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (let* ((first (e-session-append-message
                       store session-id '(:role user :content "hello")))
               (second (e-session-append-message
                        store session-id '(:role assistant :content "hi")))
               (loaded (e-session-persistent-store-create directory))
               (messages (e-session-messages loaded session-id)))
          (should (equal (mapcar (lambda (message) (plist-get message :id))
                                 messages)
                         (list (plist-get first :id)
                               (plist-get second :id))))
          (should (equal (plist-get (cadr messages) :parent-id)
                         (plist-get first :id))))
      (delete-directory directory t))))

(ert-deftest e-session-test-legacy-replay-backfills-entry-ids ()
  "Legacy records without entry ids load with stable in-memory parent links."
  (let* ((directory (make-temp-file "e-session-" t))
         (sessions-dir (expand-file-name "sessions" directory))
         (session-file (expand-file-name "legacy.jsonl" sessions-dir)))
    (unwind-protect
        (progn
          (make-directory sessions-dir t)
          (with-temp-file session-file
            (insert
             "{\"type\":\"session\",\"session-id\":\"legacy\",\"timestamp\":\"2026-05-21T10:00:00Z\"}\n"
             "{\"type\":\"message\",\"session-id\":\"legacy\",\"timestamp\":\"2026-05-21T10:00:01Z\",\"message\":{\"role\":\"user\",\"content\":\"hello\"}}\n"
             "{\"type\":\"message\",\"session-id\":\"legacy\",\"timestamp\":\"2026-05-21T10:00:02Z\",\"message\":{\"role\":\"assistant\",\"content\":\"hi\"}}\n"))
          (let* ((loaded (e-session-test--replay-legacy-copy
                          directory "legacy"))
                 (events (e-session-session-events loaded "legacy"))
                 (root (car events))
                 (messages (e-session-messages loaded "legacy")))
            (should (= (length events) 1))
            (should (eq (plist-get root :event-type) 'session-created))
            (should (plist-get root :id))
            (should (= (length messages) 2))
            (dolist (message messages)
              (should (plist-get message :id)))
            (should (equal (plist-get (car messages) :parent-id)
                           (plist-get root :id)))
            (should (equal (plist-get (cadr messages) :parent-id)
                           (plist-get (car messages) :id)))))
      (delete-directory directory t))))


(ert-deftest e-session-test-index-store-lists-without-loading-transcripts ()
  "Indexed SQLite stores list sessions without replaying record pages."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id '(:id "msg-1" :role user :content "indexed hello"))
          (e-session-append-message
           store session-id '(:id "msg-2" :role tool :content (:payload "large")))
          (cl-letf (((symbol-function 'e-session-load-session)
                     (lambda (&rest _args)
                       (error "index store replayed session records"))))
              (let* ((indexed (e-session-persistent-index-store-create directory))
                     (sessions (e-session-list indexed))
                     (session (car sessions)))
                (should (equal (plist-get session :id) session-id))
                (should (equal (plist-get session :summary) "indexed hello"))
                (should (= (plist-get session :message-count) 2))
                (should-not (plist-get session :loaded))))
          (let ((indexed (e-session-persistent-index-store-create directory)))
            (should (equal (mapcar (lambda (message) (plist-get message :id))
                                   (e-session-messages indexed session-id))
                           '("msg-1" "msg-2")))))
      (delete-directory directory t))))

(ert-deftest e-session-test-index-store-display-title-avoids-transcript-load ()
  "Display titles for indexed sessions use metadata without transcript replay."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get
                      (e-session-create store
                                        :id "session-1"
                                        :metadata '(:name "Indexed title"))
                      :id)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "indexed hello"))
          (let ((loaded nil))
            (cl-letf (((symbol-function 'e-session-load-session)
                       (lambda (&rest _args)
                         (setq loaded t)
                         (error "display title loaded transcript"))))
              (let ((indexed (e-session-persistent-index-store-create directory)))
                (should (equal (e-session-display-title indexed session-id)
                               "Indexed title"))
                (should-not loaded)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-index-store-loads-object-shaped-index ()
  "A legacy-only index receives a targeted offline-migration error."
  (let ((directory (make-temp-file "e-session-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sessions" directory) t)
          (with-temp-file (expand-file-name "index.json" directory)
            (insert
             "{"
             "\"session-1\":{"
             "\"created-at\":\"2026-05-24T17:20:37Z\","
             "\"updated-at\":\"2026-05-24T17:21:00Z\","
             "\"summary\":\"object index prompt\","
             "\"title\":\"object index prompt\","
             "\"message-count\":3,"
             "\"last-message-at\":\"2026-05-24T17:21:00Z\""
             "}"
             "}\n"))
          (should-error
           (e-session-persistent-index-store-create directory)
           :type 'e-runtime-store-schema-too-old))
      (delete-directory directory t))))


(ert-deftest e-session-test-rename-persists-explicit-title ()
  "Renaming a persistent session appends metadata and survives reload."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store) :id)))
    (unwind-protect
        (progn
          (e-session-rename store session-id "Named session")
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (e-session-display-title loaded session-id)
                           "Named session"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-default-title-uses-first-25-prompt-chars ()
  "Default session titles use only the first prompt snippet."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "short")
    (e-session-append-message
     store "short" '(:id "msg-1" :role user :content "abcdefghijklmnopqrstuvwxy"))
    (should (equal (e-session-display-title store "short")
                   "abcdefghijklmnopqrstuvwxy"))
    (e-session-create store :id "long")
    (e-session-append-message
     store "long" '(:id "msg-2" :role user :content "abcdefghijklmnopqrstuvwxyz"))
    (should (equal (e-session-display-title store "long")
                   "abcdefghijklmnopqrstuvwxy..."))))


(ert-deftest e-session-test-metadata-update-persists-through-session-info ()
  "Session metadata updates append session-info records and replay."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create
                                 store
                                 :id "session-1"
                                 :metadata '(:project-root "/tmp/narrow/"))
                                :id)))
    (unwind-protect
        (progn
          (e-session-set-metadata store session-id '(:project-root "/tmp/wide/"))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (plist-get
                            (plist-get (e-session-get loaded session-id) :metadata)
                            :project-root)
	                           "/tmp/wide/"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-replay-repairs-legacy-array-metadata ()
  "Persistent replay repairs legacy metadata arrays without relaxing writes."
  (let* ((directory (make-temp-file "e-session-legacy-array-metadata-" t))
         (sessions-directory (expand-file-name "sessions" directory))
         (session-file (expand-file-name "legacy-array.jsonl"
                                         sessions-directory)))
    (unwind-protect
        (progn
          (make-directory sessions-directory t)
          (with-temp-file session-file
            (insert
             (json-encode
              `(:type "session"
                :session-id "legacy-array"
                :id "root"
                :timestamp "2026-06-29T00:00:00Z"
                :metadata ["/tmp/project-a/"
                           "project-root"
                           "chat-default"
                           "harness-instance-id"]))
             "\n"
             (json-encode
              `(:type "session-info"
                :session-id "legacy-array"
                :id "info-1"
                :parent-id "root"
                :timestamp "2026-06-29T00:00:01Z"
                :metadata ["/tmp/project-b/"
                           "project-root"
                           "chat-updated"
                           "harness-instance-id"]))
             "\n"))
          (let* ((store (e-session-test--replay-legacy-copy
                         directory "legacy-array"))
                 (metadata (plist-get (e-session-get store "legacy-array")
                                      :metadata)))
            (should (e-session-aggregate-keyword-plist-shape-p metadata))
            (should (equal (plist-get metadata :project-root)
                           "/tmp/project-b/"))
            (should (equal (plist-get metadata :harness-instance-id)
                           "chat-updated"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-metadata-schema-rejects-transient-and-unknown-keys ()
  "Generic metadata writes reject unowned or presentation-only state."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (should-error
     (e-session-create
      store
      :id "legacy-array"
      :metadata '("project-root" "/tmp/project/")))
    (should-error
     (e-session-set-metadata store "session-1" '(:unknown t)))
    (should-error
     (e-session-set-metadata
      store "session-1" '(:e-chat-read-markers (:default "marker"))))
    (should-error
     (e-session-set-metadata
      store
      "session-1"
      '(:org-canvas (:uri "buffer://canvas"
                    :last-focus (:point 1)))))))

(ert-deftest e-session-test-typed-state-lanes-persist-through-replay ()
  "Typed metadata helpers persist their owned state lanes."
  (let* ((directory (make-temp-file "e-session-typed-state-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-set-session-config
           store session-id '(:project-root "/tmp/project/"))
          (e-session-set-context-references
           store
           session-id
           'chat-session
           '(:attachments ((:uri "buffer://source" :id "source"))))
          (e-session-set-capability-state
           store
           session-id
           'mcp
           '(:enabled t))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (metadata (plist-get
                            (e-session-get loaded session-id)
                            :metadata)))
            (should (equal (plist-get metadata :project-root)
                           "/tmp/project/"))
            (should (equal
                     (plist-get
                      (car (plist-get
                            (e-session-context-references
                             loaded session-id 'chat-session)
                            :attachments))
                      :uri)
                     "buffer://source"))
            (should (equal (plist-get
                            (e-session-capability-state
                             loaded session-id 'mcp)
                            :enabled)
                           t))))
      (delete-directory directory t))))

(ert-deftest e-session-test-subagent-lineage-metadata-round-trips ()
  "Durable subagent lineage metadata persists through replay."
  (let* ((directory (make-temp-file "e-session-subagent-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "child-1") :id)))
    (unwind-protect
        (progn
          (e-session-set-session-config
           store session-id
           '(:parent-session-id "parent-1"
             :subagent-role "reviewer"
             :subagent-label "review plan.org"
             :tmp-lineage-id "parent-1"))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (metadata (plist-get (e-session-get loaded session-id)
                                      :metadata)))
            (should (equal (plist-get metadata :parent-session-id) "parent-1"))
            (should (equal (plist-get metadata :subagent-role) "reviewer"))
            (should (equal (plist-get metadata :subagent-label)
                           "review plan.org"))
            (should (equal (plist-get metadata :tmp-lineage-id) "parent-1"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-replay-drops-known-transient-metadata ()
  "Legacy replay removes known presentation and high-churn focus metadata."
  (let* ((directory (make-temp-file "e-session-legacy-metadata-" t))
         (sessions-directory (expand-file-name "sessions" directory))
         (session-file (expand-file-name "legacy.jsonl" sessions-directory)))
    (unwind-protect
        (progn
          (make-directory sessions-directory t)
          (with-temp-file session-file
            (insert
             (json-encode
              '(:type "session"
                :session-id "legacy"
                :id "root"
                :timestamp "2026-06-29T00:00:00Z"
                :metadata (:name "Legacy"
                           :e-chat-read-markers (:default "marker")
                           :org-canvas (:uri "buffer://canvas"
                                        :last-scope "document"
                                        :last-focus (:point 42)))))
             "\n"))
          (let* ((store (e-session-test--replay-legacy-copy
                         directory "legacy"))
                 (metadata (plist-get (e-session-get store "legacy")
                                      :metadata))
                 (canvas (plist-get metadata :org-canvas)))
            (should (equal (plist-get metadata :name) "Legacy"))
            (should-not (plist-member metadata :e-chat-read-markers))
            (should (equal (plist-get canvas :uri) "buffer://canvas"))
            (should-not (plist-member canvas :last-scope))
            (should-not (plist-member canvas :last-focus))))
      (delete-directory directory t))))

(ert-deftest e-session-test-turn-options-persist-through-session-info ()
  "Session turn options survive persistent replay."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store) :id)))
    (unwind-protect
        (progn
          (e-session-set-turn-options
           store
           session-id
           '(:model "gpt-test"
             :reasoning-effort "high"
             :prompt-cache-default t
             :prompt-cache-retention "24h"))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (e-session-turn-options loaded session-id)
                           '(:model "gpt-test"
                             :reasoning-effort "high"
                             :prompt-cache-default t
                             :prompt-cache-retention "24h")))))
      (delete-directory directory t))))

(ert-deftest e-session-test-branch-summary-persists-through-replay ()
  "Branch summary records append and replay into session state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-branch-summary
           store session-id "branch-a" "Built the first slice."
           :metadata '(:from "turn-1"))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (summary (car (plist-get
                                (e-session-get loaded session-id)
                                :branch-summaries))))
            (should (equal (plist-get summary :branch-id) "branch-a"))
            (should (equal (plist-get summary :summary)
                           "Built the first slice."))
            (should (equal (plist-get summary :metadata)
                           '(:from "turn-1")))))
      (delete-directory directory t))))

(ert-deftest e-session-test-branch-summaries-preserve-append-order ()
  "Branch summaries stay in insertion order across multiple appends."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-branch-summary store "session-1" "branch-a" "First")
    (e-session-append-branch-summary store "session-1" "branch-b" "Second")
    (should (equal (mapcar (lambda (summary)
                             (plist-get summary :branch-id))
                           (plist-get (e-session-get store "session-1")
                                      :branch-summaries))
                   '("branch-a" "branch-b")))))

(ert-deftest e-session-test-compaction-persists-through-replay ()
  "Compaction records append and replay into session state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-compaction
           store session-id "Compacted early transcript."
           :branch-id "branch-a"
           :range '(:from "msg-1" :to "msg-9")
           :tokens-before 123
           :tokens-kept 45)
          (let* ((loaded (e-session-persistent-store-create directory))
                 (compaction (car (plist-get
                                   (e-session-get loaded session-id)
                                   :compactions))))
            (should (equal (plist-get compaction :summary)
                           "Compacted early transcript."))
            (should (equal (plist-get compaction :branch-id) "branch-a"))
            (should (equal (plist-get compaction :range)
                           '(:from "msg-1" :to "msg-9")))
            (should (= (plist-get compaction :tokens-before) 123))
            (should (= (plist-get compaction :tokens-kept) 45))))
      (delete-directory directory t))))

(ert-deftest e-session-test-compactions-preserve-append-order ()
  "Compactions stay in insertion order across multiple appends."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-compaction store "session-1" "First")
    (e-session-append-compaction store "session-1" "Second")
    (should (equal (mapcar (lambda (compaction)
                             (plist-get compaction :summary))
                           (e-session-compactions store "session-1"))
                   '("First" "Second")))))

(ert-deftest e-session-test-provider-anchor-persists-through-replay ()
  "Provider anchors append and replay as opaque durable session entries."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (let* ((message (e-session-append-message
                         store session-id
                         '(:role assistant :content "anchored")))
               (anchor (e-session-append-provider-anchor
                        store session-id 'openai
                        :model "gpt-test"
                        :covered-entry-id (plist-get message :id)
                        :fingerprints '(:static-prefix "abc"
                                        :current-state "def")
                        :metadata '(:response-id "resp-1")))
               (loaded (e-session-persistent-store-create directory))
               (replayed (car (e-session-provider-anchors
                               loaded session-id))))
          (should (equal (plist-get replayed :id)
                         (plist-get anchor :id)))
          (should (eq (plist-get replayed :provider-id) 'openai))
          (should (equal (plist-get replayed :model) "gpt-test"))
          (should (equal (plist-get replayed :covered-entry-id)
                         (plist-get message :id)))
          (should (equal (plist-get replayed :fingerprints)
                         '(:static-prefix "abc"
                           :current-state "def")))
          (should (equal (plist-get replayed :metadata)
                         '(:response-id "resp-1"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-provider-anchor-persists-nested-fingerprints ()
  "Provider-anchor fingerprint arrays of plists survive JSONL replay."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id))
         (fingerprints
          '(:segments ((:kind "static-prefix"
                        :id "(project-local instructions)"
                        :fingerprint "static-fp")
                       (:kind "current-state"
                        :id "(visible-buffers 0)"
                        :fingerprint "dynamic-fp"))
            :active-layer-ids ("base" "project-local")
            :tools ((:name "read" :fingerprint "read-fp")
                    (:name "write" :fingerprint "write-fp"))
            :reasoning (:reasoning nil
                        :reasoning-effort "high"
                        :effort nil)
            :provider-options (:prompt-cache-key "cache-key"
                               :prompt-cache-retention "24h")
            :compaction-boundary nil)))
    (unwind-protect
        (let* ((message (e-session-append-message
                         store session-id
                         '(:role assistant :content "anchored")))
               (anchor (e-session-append-provider-anchor
                        store session-id 'openai
                        :model "gpt-test"
                        :covered-entry-id (plist-get message :id)
                        :fingerprints fingerprints
                        :metadata '(:response-id "resp-1")))
               (loaded (e-session-persistent-store-create directory))
               (replayed (car (e-session-provider-anchors
                               loaded session-id))))
          (should (equal (plist-get replayed :id)
                         (plist-get anchor :id)))
          (should (equal (plist-get replayed :fingerprints)
                         fingerprints)))
      (delete-directory directory t))))

(ert-deftest e-session-test-provider-anchor-rejects-malformed-replayed-segments ()
  "Malformed pre-fix provider-anchor segments invalidate without crashing."
  (let ((store (e-session-store-create)))
    (let* ((session-id (plist-get (e-session-create store :id "session-1") :id))
           (message (e-session-append-message
                     store session-id
                     '(:role assistant :content "anchored")))
           (malformed
            '(:segments (:kind ("static-prefix"
                                "id"
                                "(project-local instructions)"
                                "fingerprint"
                                "static-fp"))
              :active-layer-ids ("base" "project-local")
              :tools (:name ("read" "fingerprint" "read-fp"))
              :reasoning (:reasoning nil
                          :reasoning-effort "high"
                          :effort nil)
              :provider-options (:prompt-cache-key "cache-key")
              :compaction-boundary nil))
           (current
            '(:segments ((:kind "static-prefix"
                          :id "(project-local instructions)"
                          :fingerprint "static-fp"))
              :active-layer-ids ("base" "project-local")
              :tools ((:name "read" :fingerprint "read-fp"))
              :reasoning (:reasoning nil
                          :reasoning-effort "high"
                          :effort nil)
              :provider-options (:prompt-cache-key "cache-key")
              :compaction-boundary nil)))
      (e-session-append-provider-anchor
       store session-id 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get message :id)
       :fingerprints malformed
       :metadata '(:response-id "resp-1"))
      (should (eq (e-session-provider-anchor-incompatibility-reason
                   store session-id
                   (car (e-session-provider-anchors store session-id))
                   'openai
                   "gpt-test"
                   current)
                  'segment-fingerprint-mismatch)))))

(ert-deftest e-session-test-latest-provider-anchor-requires-current-path ()
  "Provider anchors are compatible only when their covered entry is current."
  (let ((store (e-session-store-create)))
    (let* ((session-id (plist-get (e-session-create store :id "session-1") :id))
           (first (e-session-append-message
                   store session-id '(:role assistant :content "first")))
           (second (e-session-append-message
                    store session-id '(:role assistant :content "second")))
           (first-anchor
            (e-session-append-provider-anchor
             store session-id 'openai
             :model "gpt-test"
             :covered-entry-id (plist-get first :id)
             :fingerprints '(:history "one")
             :metadata '(:response-id "resp-1")))
           (second-anchor
            (e-session-append-provider-anchor
             store session-id 'openai
             :model "gpt-test"
             :covered-entry-id (plist-get second :id)
             :fingerprints '(:history "two")
             :metadata '(:response-id "resp-2"))))
      (should (equal (plist-get
                      (e-session-latest-compatible-provider-anchor
                       store session-id 'openai
                       :model "gpt-test"
                       :fingerprints '(:history "two"))
                      :id)
                     (plist-get second-anchor :id)))
      (should-not
       (e-session-latest-compatible-provider-anchor
        store session-id 'openai
        :model "gpt-test"
        :fingerprints '(:history "changed")))
      (plist-put (e-session-get store session-id)
                 :current-head-id
                 (plist-get first-anchor :id))
      (should (equal (plist-get
                      (e-session-latest-compatible-provider-anchor
                       store session-id 'openai
                       :model "gpt-test"
                       :fingerprints '(:history "one"))
                      :id)
                     (plist-get first-anchor :id)))
      (should-not
       (e-session-latest-compatible-provider-anchor
        store session-id 'openai
        :model "gpt-test"
        :fingerprints '(:history "two"))))))

(ert-deftest e-session-test-entry-query-helpers-cover-paths-turns-and-boundaries ()
  "Entry query helpers return ids, current paths, turn groups, and suffixes."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (let* ((first (e-session-append-message
                   store "session-1"
                   '(:turn-id "turn-a" :role user :content "one")))
           (second (e-session-append-message
                    store "session-1"
                    '(:turn-id "turn-a" :role assistant :content "two")))
           (third (e-session-append-message
                   store "session-1"
                   '(:turn-id "turn-b" :role user :content "three")))
           (compaction (e-session-append-compaction
                        store "session-1" "kept suffix"
                        :first-kept-entry-id (plist-get second :id))))
      (should (equal (plist-get (e-session-entry-by-id
                                 store "session-1" (plist-get second :id))
                                :content)
                     "two"))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                             (e-session-entries-in-turn
                              store "session-1" "turn-a"))
                     (list (plist-get first :id)
                           (plist-get second :id))))
      (should (equal (plist-get (e-session-entry-previous
                                 store "session-1" (plist-get third :id))
                                :id)
                     (plist-get second :id)))
      (should (equal (plist-get (e-session-entry-next
                                 store "session-1" (plist-get second :id))
                                :id)
                     (plist-get third :id)))
      (should (equal (plist-get (e-session-latest-entry-of-type
                                 store "session-1" 'message)
                                :id)
                     (plist-get third :id)))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                             (e-session-entries-from
                              store "session-1" (plist-get second :id)))
                     (list (plist-get second :id)
                           (plist-get third :id)
                           (plist-get compaction :id)))))))

(ert-deftest e-session-test-latest-valid-compaction-requires-current-boundary ()
  "Latest valid compaction ignores records with missing kept-entry boundaries."
  (let ((store (e-session-store-create)))
    (let* ((session-id (plist-get (e-session-create store :id "session-1") :id))
           (root (car (e-session-session-events store session-id)))
           (first (e-session-append-message
                   store "session-1" '(:role user :content "one")))
           (second (e-session-append-message
                    store "session-1" '(:role user :content "two"))))
      (e-session-append-compaction
       store "session-1" "invalid"
       :first-kept-entry-id "missing")
      (let ((valid
             (e-session-append-compaction
              store "session-1" "valid"
              :first-kept-entry-id (plist-get second :id))))
        (should (equal (plist-get (e-session-latest-valid-compaction
                                   store "session-1")
                                  :id)
                       (plist-get valid :id)))
        (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                               (e-session-entries-before
                                store "session-1" (plist-get second :id)))
                       (list (plist-get root :id)
                             (plist-get first :id))))))))

(ert-deftest e-session-test-current-branch-persists-through-replay ()
  "Current branch cursor records append and replay into session state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-set-current-branch store session-id "branch-a")
          (e-session-set-current-branch store session-id "branch-b")
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (plist-get (e-session-get loaded session-id)
                                      :current-branch)
                           "branch-b"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-state-records-are-identifiable-session-events ()
  "Session state records remain identifiable across bounded checkpointing."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-set-metadata store session-id '(:project-root "/tmp/project/"))
          (e-session-set-turn-options store session-id '(:model "gpt-test"))
          (e-session-set-current-branch store session-id "branch-a")
          (let* ((loaded (e-session-persistent-store-create directory))
                 (events (e-session-session-events loaded session-id))
                 (types (mapcar (lambda (event)
                                  (plist-get event :event-type))
                                events))
                 (path-types (mapcar (lambda (entry)
                                       (plist-get entry :event-type))
                                     (e-session-current-path loaded session-id))))
            ;; The current root folds superseded metadata/options; the
            ;; identity-bearing branch event remains on the resumed path.
            (should (equal types '(session-created current-branch)))
            (dolist (event events)
              (should (plist-get event :id)))
            (should-not (plist-get (car events) :parent-id))
            (should (equal (mapcar (lambda (event)
                                     (plist-get event :id))
                                   (butlast events))
                           (mapcar (lambda (event)
                                     (plist-get event :parent-id))
                                   (cdr events))))
            (should (equal path-types types))))
      (delete-directory directory t))))

(ert-deftest e-session-test-current-path-supports-synthetic-branches ()
  "Current-path reconstruction can target explicit branch heads."
  (let ((store (e-session-store-create)))
    (let* ((session-id (plist-get (e-session-create store :id "session-1") :id))
           (root (car (e-session-session-events store session-id)))
           (left (e-session-append-message
                  store session-id
                  '(:role user :content "left branch")))
           (right (e-session-append-message
                   store session-id
                   (list :role 'user
                         :content "right branch"
                         :parent-id (plist-get root :id)))))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                             (e-session-current-path
                              store session-id (plist-get left :id)))
                     (list (plist-get root :id)
                           (plist-get left :id))))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                             (e-session-current-path
                              store session-id (plist-get right :id)))
                     (list (plist-get root :id)
                           (plist-get right :id))))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                             (e-session-current-path store session-id))
                     (list (plist-get root :id)
                           (plist-get right :id)))))))

(ert-deftest e-session-test-clear-messages-is-append-only ()
  "Clearing a session appends a durable reset without deleting old records."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id '(:id "msg-1" :role user :content "hello"))
          (e-session-clear-messages store session-id)
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (e-session-messages loaded session-id) nil))
            (let ((types
                   (mapcar
                    (lambda (record) (plist-get record :type))
                    (e-session-storage-read-session-records loaded session-id))))
              (should (member "message" types))
              (should (member "messages-cleared" types)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-clear-messages-creates-reset-boundary-root ()
  "Clearing messages makes the next message parent to the clear event."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id '(:role user :content "old"))
          (let* ((clear-event (e-session-clear-messages store session-id))
                 (new-message
                  (e-session-append-message
                   store session-id '(:role user :content "new")))
                 (path (e-session-current-path store session-id))
                 (loaded (e-session-persistent-store-create directory))
                 (loaded-path (e-session-current-path loaded session-id)))
            (should (eq (plist-get clear-event :event-type) 'messages-cleared))
            (should (equal (plist-get new-message :parent-id)
                           (plist-get clear-event :id)))
            (should (equal (mapcar (lambda (entry)
                                     (or (plist-get entry :event-type)
                                         (plist-get entry :role)))
                                   path)
                           '(session-created messages-cleared user)))
            ;; The bounded checkpoint folds the reset event into its root, but
            ;; the visible post-reset path and durable journal remain exact.
            (should (equal (mapcar (lambda (entry)
                                     (or (plist-get entry :event-type)
                                         (plist-get entry :role)))
                                   loaded-path)
                           '(session-created user)))
            (should (member
                     "messages-cleared"
                     (mapcar
                      (lambda (record) (plist-get record :type))
                      (e-session-storage-read-session-records
                       loaded session-id))))))
      (delete-directory directory t))))

(ert-deftest e-session-test-activity-events-persist-and-clear-with-messages ()
  "Activity events are durable session records and clear with transcript state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-activity-event
           store
           session-id
           "turn-1"
           'reasoning-delta
           '(:content "Need current buffer state."))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (mapcar (lambda (event)
                                     (plist-get event :event-type))
                                   (e-session-activity-events loaded session-id))
                           '(reasoning-delta)))
            (should (equal (plist-get
                            (car (e-session-activity-events loaded session-id))
                            :payload)
                           '(:content "Need current buffer state.")))))
          (e-session-clear-messages store session-id)
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (e-session-activity-events loaded session-id) nil)))
      (delete-directory directory t))))

(ert-deftest e-session-test-activity-events-preserve-append-order ()
  "Activity events stay in insertion order across multiple appends."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-activity-event
     store "session-1" "turn-1" 'reasoning-delta '(:content "one"))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'tool-started '(:name "read"))
    (should (equal (mapcar (lambda (event)
                             (plist-get event :event-type))
                           (e-session-activity-events store "session-1"))
                   '(reasoning-delta tool-started)))))

(ert-deftest e-session-test-process-reports-persist-outside-messages ()
  "Process reports replay as dedicated entries outside the transcript."
  (let* ((directory (make-temp-file "e-session-process-report-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (let ((report
                 (e-session-append-process-report
                  store "session-1"
                  '(:report-type "marker" :marker-id "marker-1"))))
            (should (eq (plist-get report :type) 'process-report))
            (should (equal (plist-get report :marker-id) "marker-1")))
          (should-not (e-session-messages store "session-1"))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (reports (e-session-process-reports loaded "session-1")))
            (should (= (length reports) 1))
            (should (equal (plist-get (car reports) :marker-id) "marker-1"))
            (should-not (e-session-messages loaded "session-1"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-append-activity-event-can-skip-index-write ()
  "Activity event append callers can skip immediate index persistence."
  (let ((store (e-session-store-create))
        (write-count 0))
    (e-session-create store :id "session-1")
    (cl-letf (((symbol-function 'e-session-storage-publish-projections)
               (lambda (&rest _)
                 (setq write-count (1+ write-count)))))
      (e-session-append-activity-event
       store "session-1" "turn-1" 'reasoning-delta '(:content "one")
       :write-index nil)
      (should (= write-count 0))
      (e-session-append-activity-event
       store "session-1" "turn-1" 'tool-started '(:name "read")
       :write-index t)
      (should (= write-count 1)))
    (should (equal (mapcar (lambda (event)
                             (plist-get event :event-type))
                           (e-session-activity-events store "session-1"))
                   '(reasoning-delta tool-started)))))

(ert-deftest e-session-test-latest-token-usage-event-is-derived-on-append-replay-and-clear ()
  "Latest token usage is available without scanning durable activity events."
  (let* ((directory (make-temp-file "e-session-token-usage-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-append-activity-event
           store "session-1" "turn-1" 'reasoning-delta '(:content "thinking"))
          (e-session-append-activity-event
           store "session-1" "turn-1" 'token-usage '(:input-tokens 10))
          (e-session-append-activity-event
           store "session-1" "turn-2" 'token-usage '(:input-tokens 20))
          (should (equal (plist-get
                          (plist-get
                           (e-session-latest-token-usage-event store "session-1")
                           :payload)
                          :input-tokens)
                         20))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (plist-get
                            (plist-get
                             (e-session-latest-token-usage-event loaded "session-1")
                             :payload)
                            :input-tokens)
                           20))
            (e-session-clear-messages loaded "session-1")
            (should-not
             (e-session-latest-token-usage-event loaded "session-1"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-profile-records-persistent-appends-and-index-writes ()
  "Enabled dev profiling records persistent append and index write spans."
  (let* ((directory (make-temp-file "e-session-profile-" t))
         (profile-directory (make-temp-file "e-session-profile-trace-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-dev-profile-start)
          (e-session-create store :id "session-1")
          (e-session-append-message
           store "session-1" '(:role user :content "hello" :turn-id "turn-1"))
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "session.append-record" aggregates nil nil #'equal))
            (should (alist-get "session.write-index" aggregates nil nil #'equal))))
      (delete-directory directory t)
      (delete-directory profile-directory t))))

(ert-deftest e-session-test-append-after-replay-and-clear-keeps-clean-order ()
  "Replay finalization and clear reset internal append state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id '(:id "msg-1" :role user :content "old"))
          (e-session-append-activity-event
           store session-id "turn-1" 'reasoning-delta '(:content "old"))
          (let ((loaded (e-session-persistent-store-create directory)))
            (e-session-append-message
             loaded session-id '(:id "msg-2" :role assistant :content "replayed"))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :id))
                                   (e-session-messages loaded session-id))
                           '("msg-1" "msg-2")))
            (e-session-clear-messages loaded session-id)
            (e-session-append-message
             loaded session-id '(:id "msg-3" :role user :content "new"))
            (e-session-append-activity-event
             loaded session-id "turn-2" 'tool-started '(:name "after-clear"))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :id))
                                   (e-session-messages loaded session-id))
                           '("msg-3")))
            (should (equal (mapcar (lambda (event)
                                     (plist-get event :event-type))
                                   (e-session-activity-events loaded session-id))
                           '(tool-started)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-list-sessions-sorted-with-display-metadata ()
  "Session list returns recent sessions with current durable references."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "older")
          (e-session-append-message
           store "older" '(:id "old-msg" :role user :content "older prompt"))
          (e-session-create store :id "newer")
          (e-session-append-message
           store "newer" '(:id "new-msg" :role user :content "newer prompt"))
          (e-session-rename store "newer" "Explicit title")
          (let ((sessions (e-session-list store)))
            (should (equal (mapcar (lambda (session) (plist-get session :id))
                                   sessions)
                           '("newer" "older")))
            (should (equal (plist-get (car sessions) :title)
                           "Explicit title"))
            (should (equal (plist-get (cadr sessions) :title)
                           "older prompt"))
            (should (= (plist-get (car sessions) :message-count) 1))
            (should-not (plist-get (car sessions) :file))))
      (delete-directory directory t))))

(ert-deftest e-session-test-list-roots-excludes-worker-sessions ()
  "Root listing omits subagent and task-queue sessions without deleting them."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "root")
    (e-session-create store :id "subagent"
                      :metadata '(:parent-session-id "root"))
    (e-session-create store :id "task"
                      :metadata '(:task-queue-task-id "tsk_000001"))
    (should (equal (mapcar (lambda (session) (plist-get session :id))
                           (e-session-list-roots store))
                   '("root")))
    (should (= (length (e-session-list store)) 3))))

(ert-deftest e-session-test-index-store-list-roots-excludes-worker-sessions ()
  "Root listing classifies unloaded indexed sessions from durable metadata."
  (let* ((directory (make-temp-file "e-session-index-roots-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "root")
          (e-session-create store :id "subagent"
                            :metadata '(:parent-session-id "root"
                                        :subagent-role "tool-user"))
          (e-session-create store :id "task"
                            :metadata '(:task-queue-task-id "tsk_000001"))
          (let ((indexed (e-session-persistent-index-store-create directory)))
            (should (equal (mapcar (lambda (session)
                                     (plist-get session :id))
                                   (e-session-list-roots indexed))
                           '("root")))
            (should (= (length (e-session-list indexed)) 3))))
      (delete-directory directory t))))

(ert-deftest e-session-test-refresh-index-metadata-repairs-unloaded-stubs ()
  "Index refresh repairs stale unloaded metadata without replacing sessions."
  (let* ((directory (make-temp-file "e-session-refresh-index-" t))
         (writer (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create writer :id "root")
          (e-session-create writer :id "worker"
                            :metadata '(:parent-session-id "root"
                                        :subagent-role "tool-user"))
          (let* ((store (e-session-persistent-index-store-create directory))
                 (worker (e-session-aggregate-peek-session store "worker")))
            ;; Reproduce a store retained across reload from code that did not
            ;; hydrate metadata into unloaded index stubs.
            (plist-put worker :metadata nil)
            (should (equal (mapcar (lambda (session)
                                     (plist-get session :id))
                                   (e-session-list-roots store))
                           '("worker" "root")))
            (e-session-refresh-index-metadata store)
            (should (eq (e-session-aggregate-peek-session store "worker") worker))
            (should-not (plist-get worker :loaded))
            (should (equal (plist-get (plist-get worker :metadata)
                                      :parent-session-id)
                           "root"))
            (should (equal (mapcar (lambda (session)
                                     (plist-get session :id))
                                   (e-session-list-roots store))
                           '("root")))))
      (delete-directory directory t))))


(ert-deftest e-session-test-fork-snapshots-messages-and-leaves-source-untouched ()
  "Forking seeds the fork with the source's messages and diverges independently."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "src" :metadata '(:name "Src"))
    (e-session-append-message store "src" '(:role user :content "one"))
    (e-session-append-message store "src" '(:role assistant :content "two"))
    (let* ((fork (e-session-fork store "src"))
           (fork-id (plist-get fork :id)))
      (should (not (equal fork-id "src")))
      (should (equal (mapcar (lambda (m) (plist-get m :content))
                             (e-session-messages store fork-id))
                     '("one" "two")))
      ;; New turns append only to the fork; the source is untouched.
      (e-session-append-message store fork-id '(:role user :content "three"))
      (should (= (length (e-session-messages store "src")) 2))
      (should (= (length (e-session-messages store fork-id)) 3)))))

(ert-deftest e-session-test-fork-mints-fresh-identity-and-linear-chain ()
  "Fork messages get fresh ids and a clean linear parent chain."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "src")
    (e-session-append-message store "src" '(:id "m1" :role user :content "a"))
    (e-session-append-message store "src" '(:id "m2" :role assistant :content "b"))
    (let* ((fork (e-session-fork store "src"))
           (fork-id (plist-get fork :id))
           (messages (e-session-messages store fork-id))
           (ids (mapcar (lambda (m) (plist-get m :id)) messages))
           (parents (mapcar (lambda (m) (plist-get m :parent-id)) messages)))
      ;; Fresh identity: source ids do not leak into the fork.
      (should-not (seq-intersection ids '("m1" "m2")))
      (should (cl-every #'stringp ids))
      ;; Linear chain: the second message's parent is the first's id.
      (should (equal (nth 1 parents) (nth 0 ids))))))

(ert-deftest e-session-test-fork-copies-context-metadata-and-turn-options ()
  "Fork inherits context metadata and turn options, with name override."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "src"
                      :metadata '(:name "Src" :project-root "/tmp/proj/"))
    (e-session-set-turn-options store "src" '(:model "m-1"))
    (let* ((fork (e-session-fork store "src" :name "Forked"))
           (fork-id (plist-get fork :id))
           (metadata (plist-get fork :metadata)))
      (should (equal (plist-get metadata :project-root) "/tmp/proj/"))
      (should (equal (plist-get metadata :name) "Forked"))
      (should (equal (plist-get (e-session-turn-options store fork-id) :model)
                     "m-1")))))

(ert-deftest e-session-test-fork-at-head-truncates-snapshot ()
  "Forking at an explicit head only snapshots messages up to that entry."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "src")
    (let ((first (e-session-append-message
                  store "src" '(:role user :content "keep"))))
      (e-session-append-message store "src" '(:role assistant :content "drop"))
      (let* ((fork (e-session-fork store "src" :at (plist-get first :id)))
             (fork-id (plist-get fork :id)))
        (should (equal (mapcar (lambda (m) (plist-get m :content))
                               (e-session-messages store fork-id))
                       '("keep")))))))

(ert-deftest e-session-test-fork-portable-generation-preserves-selected-at-boundary ()
  "A deliberate portable generation seeds forks without resurrecting its prefix."
  (let* ((store (e-session-store-create))
         (source (e-session-create store :id "portable-source"))
         (old (e-session-append-message
               store "portable-source"
               '(:role user :content "covered prefix")))
         (generation-entry
          (e-session-append-context-generation
           store "portable-source"
           (e-context-lifetime-generation-create
            :id "generation-source-portable"
            :checkpoint '((:role system :content "C1 selected fact"))
            :covered-session-boundary (plist-get old :id))))
         (after (e-session-append-message
                 store "portable-source"
                 '(:role user :content "post-boundary tail")))
         (before-fork
          (e-session-fork store "portable-source" :at (plist-get old :id)))
         (at-fork
          (e-session-fork store "portable-source"
                          :at (plist-get generation-entry :id)))
         (after-fork (e-session-fork store "portable-source"
                                     :at (plist-get after :id)))
         (at-id (plist-get at-fork :id))
         (after-id (plist-get after-fork :id))
         (before-id (plist-get before-fork :id))
         (at-projection
          (e-session-context-lifetime-projection store at-id))
         (after-projection
          (e-session-context-lifetime-projection store after-id)))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           (e-session-messages store before-id))
                   '("covered prefix")))
    (should-not (e-session-context-lifetime-current-generation store before-id))
    (should (string-prefix-p "generation:fork:"
                             (e-context-lifetime-generation-id
                              (plist-get at-projection :generation))))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           (e-session-messages store at-id))
                   '("C1 selected fact")))
    (should (equal (e-context-lifetime-generation-checkpoint
                    (plist-get at-projection :generation))
                   '((:role system :content "C1 selected fact"))))
    (should (string-prefix-p "generation:fork:"
                             (e-context-lifetime-generation-id
                              (plist-get after-projection :generation))))
    (should (equal (e-context-lifetime-generation-checkpoint
                    (plist-get after-projection :generation))
                   '((:role system :content "C1 selected fact")
                     (:role user :content "post-boundary tail"))))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           (e-session-messages store after-id))
                   '("C1 selected fact" "post-boundary tail")))
    (should-not (equal
                 (e-context-lifetime-generation-id
                  (plist-get after-projection :generation))
                 (e-context-lifetime-generation-id
                  (e-session-context-lifetime-current-generation
                   store "portable-source"))))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           (e-session-messages store "portable-source"))
                   '("covered prefix" "post-boundary tail")))
    (should source)))

(ert-deftest e-session-test-portable-fork-seeds-ordinary-messages-through-reopen ()
  "Portable fork seeds remain visible after persistent reopen."
  (let ((directory (make-temp-file "e-session-portable-fork-" t)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (source (e-session-create store :id "portable-source"))
               (old (e-session-append-message
                     store "portable-source"
                     '(:role user :content "covered prefix"))))
          (e-session-append-context-generation
           store "portable-source"
           (e-context-lifetime-generation-create
            :id "generation-source-reopen"
            :checkpoint '((:role system :content "C1 selected fact"))
            :covered-session-boundary (plist-get old :id)))
          (e-session-append-message store "portable-source"
                                    '(:role user :content "post tail"))
          (let* ((fork (e-session-fork store "portable-source"))
                 (fork-id (plist-get fork :id)))
            (e-session-flush-write-queue store)
            (let* ((reopened (e-session-persistent-store-create directory))
                   (messages (e-session-messages reopened fork-id))
                   (projection
                    (e-session-context-lifetime-projection reopened fork-id)))
              (should (equal (mapcar (lambda (message)
                                       (plist-get message :content))
                                     messages)
                             '("C1 selected fact" "post tail")))
              (should (equal
                       (e-context-lifetime-generation-checkpoint
                        (plist-get projection :generation))
                       '((:role system :content "C1 selected fact")
                         (:role user :content "post tail"))))
              (should source))))
      (delete-directory directory t))))

(ert-deftest e-session-test-context-projection-keeps-durable-body-and-forgets-tool-bundle ()
  "The later-request projection keeps intent/facts but not a consumed tool pair."
  (let* ((store (e-session-store-create))
         (session-id "context-projection")
         (session (e-session-create store :id session-id))
         (generation
          (e-context-lifetime-generation-create
           :id "generation-projection"
           :checkpoint '((:role system :content "policy"))
           :covered-session-boundary (plist-get session :root-event-id))))
    (e-session-append-context-generation store session-id generation)
    (e-session-append-message
     store session-id '(:id "prompt" :role user :content "durable intent"))
    (e-session-append-message
     store session-id
     '(:id "call-entry" :role tool-call
       :content (:id "call-1" :name "inspect" :arguments (:path "/tmp"))
       :metadata (:provider-replay-items
                  ((:provider-id openai :item (:type "reasoning"))))))
    (e-session-append-message
     store session-id
     '(:id "result-entry" :role tool
       :content (:tool-call-id "call-1" :name "inspect"
                 :status ok :content "BULKY-TOOL-RESULT")))
    (e-session-append-message
     store session-id
     '(:id "answer" :role assistant :content "ordinary answer"))
    (e-session-test--append-literal-v2-record
     store session-id
     '(:record-version 2
       :type context-promotion
       :id "promotion-projection"
       :frame-id "frame-projection"
       :generation-id "generation-projection"
       :consumer-request-id "consumer-projection"
       :response-entry-id "answer"
       :facts ((:id "fact-selected" :value "first divergence"))
       :source-observation-ids ("observation-tool")
       :source-refs ("result-entry")
       :source-fingerprints ("tool-fingerprint")))
    (let* ((projection (e-session-context-lifetime-projection
                        store session-id))
           (tail (plist-get projection :durable-tail))
           (contents (mapcar (lambda (message)
                               (plist-get message :content))
                             tail)))
      (should (equal contents '("durable intent" "ordinary answer")))
      (should-not (seq-some
                   (lambda (content)
                     (string-match-p "BULKY-TOOL-RESULT"
                                     (format "%S" content)))
                   contents))
      (should (= (length (plist-get projection :promotions)) 1))
      (should (equal
               (e-context-lifetime-promotion-facts
                (car (plist-get projection :promotions)))
               '((:id "fact-selected" :value "first divergence")))))))

(ert-deftest e-session-aggregate-test-persistent-replay-refreshes-derived-fields-once-per-session ()
  "Persistent replay preserves all appended messages and their summary."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (dotimes (index 40)
            (e-session-append-message
             store
             session-id
             (list :id (format "msg-%d" index)
                   :role (if (cl-evenp index) 'user 'tool)
                   :content (list :payload (make-string 1000 ?x)))))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (= (length (e-session-messages loaded session-id)) 40))
            (should (equal (plist-get (e-session-get loaded session-id) :summary)
                           (plist-get (e-session-get store session-id) :summary)))))
      (delete-directory directory t))))

(ert-deftest e-session-aggregate-test-persistent-replay-preserves-message-timestamp ()
  "Persistent replay restores each message's journal timestamp."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create directory))
         (session-id "session-1"))
    (unwind-protect
        (let* ((created (e-session-create store :id session-id))
               (message (e-session-append-message
                         store session-id
                         '(:id "msg-1" :role user :content "hello")))
               (created-at (plist-get message :created-at)))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (plist-get
                            (car (e-session-messages loaded session-id))
                            :created-at)
                           created-at))
            (should (equal (plist-get created :id) session-id))))
      (delete-directory directory t))))

(ert-deftest e-session-aggregate-test-list-sessions-sorted-by-last-message ()
  "Session list order follows last message time, not metadata touches."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "older")
          (e-session-append-message
           store "older" '(:id "old-msg" :role user :content "older prompt"))
          ;; Durable timestamps have second precision.  Keep the two message
          ;; times distinct without reaching into an owner-private clock.
          (sleep-for 1.1)
          (e-session-create store :id "newer")
          (e-session-append-message
           store "newer" '(:id "new-msg" :role user :content "newer prompt"))
          (e-session-rename store "older" "Touched older title")
          (let ((ids (mapcar (lambda (session) (plist-get session :id))
                             (e-session-list store))))
            (should (equal ids '("newer" "older"))))
          (should
           (equal
            (mapcar (lambda (entry) (plist-get entry :id))
                    (e-session-storage-read-catalog-projection store))
            '("newer" "older"))))
      (delete-directory directory t))))

(provide 'e-session-test)

;;; e-session-test.el ends here
