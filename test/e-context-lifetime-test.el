;;; e-context-lifetime-test.el --- Tests for semantic context lifetimes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-context-lifetime)

(defun e-context-lifetime-test--generation ()
  "Return a small portable generation fixture."
  (e-context-lifetime-generation-create
   :id "generation-1"
   :checkpoint '((:role system :content "checkpoint"))
   :covered-session-boundary "entry-0"))

(defun e-context-lifetime-test--frame
    (&optional consumer frame-id observation-id content)
  "Return a runtime frame with core-resolvable source provenance."
  (e-context-lifetime-frame-create
   :id (or frame-id "frame-1")
   :generation-id "generation-1"
   :consumer-request-id (or consumer "consumer-1")
   :observations
   (list (list :observation-id (or observation-id "observation-1")
               :kind "current-state"
               :source-entry-ref "external:canvas:1"
               :source-fingerprint "canvas-fingerprint-1"
               :effective-delivery "request-local-replaceable"
               :body (list :role "user"
                           :content (or content "OBSERVATION-ONE"))))))

(defun e-context-lifetime-test--multi-source-frame (count)
  "Return a live frame with COUNT independently presented sources."
  (e-context-lifetime-frame-create
   :id "multi-source-frame"
   :generation-id "generation-1"
   :consumer-request-id "consumer-1"
   :observations
   (cl-loop for index from 1 to count
            collect (list
                     :observation-id (format "observation-%d" index)
                     :kind "current-state"
                     :source-entry-ref (format "entry-%d" index)
                     :source-fingerprint (format "fingerprint-%d" index)
                     :effective-delivery "inherited"
                     :body (list (list :role "user"
                                       :content (format "source-%d" index)))))))

(ert-deftest e-context-lifetime-test-shadow-projection-forgets-consumed-frame ()
  "Consumed observation bytes disappear from the next semantic projection."
  (let* ((generation (e-context-lifetime-test--generation))
         (frame (e-context-lifetime-test--frame))
         (durable '((:role assistant :content "durable intent")))
         (open (e-context-lifetime-project
                generation frame
                :durable-tail durable
                :static-prefix '((:role system :content "static"))
                :stable-context '((:role system :content "stable"))))
         (consumed-frame
          (e-context-lifetime-frame-complete-for-consumer
           frame "consumer-1" "response-1"))
         (consumed (e-context-lifetime-project
                    generation consumed-frame
                    :durable-tail durable
                    :static-prefix '((:role system :content "static"))
                    :stable-context '((:role system :content "stable")))))
    (should (member "OBSERVATION-ONE"
                    (mapcar (lambda (message)
                              (or (plist-get message :content)
                                  (plist-get (plist-get message :body)
                                             :content)))
                            (plist-get open :messages))))
    (should-not (member "OBSERVATION-ONE"
                        (mapcar (lambda (message)
                                  (or (plist-get message :content)
                                      (plist-get (plist-get message :body)
                                                 :content)))
                                (plist-get consumed :messages))))
    (should (member "durable intent"
                    (mapcar (lambda (message) (plist-get message :content))
                            (plist-get consumed :messages))))
    (should-not (plist-get consumed :ephemeral))
    (should-not (e-context-lifetime-frame-observations consumed-frame))
    (should-not
     (string-match-p
      "OBSERVATION-ONE"
      (prin1-to-string consumed-frame)))
    (should (equal (plist-get consumed :frame-id) "frame-1"))
    (should (equal (plist-get consumed :consumer-request-id) "consumer-1"))
    (should (> (plist-get (e-context-lifetime-projection-diagnostics open)
                          :ephemeral-character-count)
               0))))

(ert-deftest e-context-lifetime-test-project-rejects-cross-generation-frame ()
  "A frame from another generation cannot enter the projection frontier."
  (let ((generation (e-context-lifetime-test--generation))
        (frame
         (e-context-lifetime-frame-create
          :id "frame-other"
          :generation-id "generation-other"
          :consumer-request-id "consumer-other"
          :observations '((:observation-id "observation-other"
                          :kind "current-state"
                          :source-entry-ref "external:other"
                          :source-fingerprint "other"
                          :effective-delivery "inherited"
                          :body (:content "other"))))))
    (should-error
     (e-context-lifetime-project generation frame)
     :type 'e-context-lifetime-invalid-record)))

(ert-deftest e-context-lifetime-test-shadow-boundary-is-opt-in ()
  "Existing request context is unchanged until explicitly enabled."
  (let* ((legacy '(:messages ((:role user :content "legacy"))))
         (generation (e-context-lifetime-test--generation))
         (frame (e-context-lifetime-test--frame)))
    (let ((e-context-lifetime-shadow-projection-enabled nil))
      (should (eq (e-context-lifetime-shadow-context legacy generation frame)
                  legacy)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (should (equal (plist-get
                      (e-context-lifetime-shadow-context
                       legacy generation frame)
                      :projection)
                     'generational-context)))))

(ert-deftest e-context-lifetime-test-generation-record-has-no-copied-tail ()
  "The durable generation record contains only its portable boundary."
  (let* ((generation (e-context-lifetime-test--generation))
         (record (e-context-lifetime-generation-record generation)))
    (should (= (plist-get record :record-version) 2))
    (should (equal (plist-get record :covered-session-boundary) "entry-0"))
    (should-not (plist-member record :durable-tail))
    (should (equal record
                   (e-context-lifetime-generation-record
                    (e-context-lifetime-generation-from-record record))))))

(ert-deftest e-context-lifetime-test-frame-is-consumer-bound ()
  "Each successful invocation owns a distinct frame and consumed frames cannot promote."
  (let* ((first (e-context-lifetime-test--frame "consumer-1" "frame-1"
                                                "observation-1" "same-state"))
         (second (e-context-lifetime-test--frame "consumer-2" "frame-2"
                                                 "observation-2" "same-state"))
         (first-consumed
          (e-context-lifetime-frame-complete-for-consumer
           first "consumer-1" "response-1"))
         (second-consumed
          (e-context-lifetime-frame-complete-for-consumer
           second "consumer-2" "response-2"))
         (effect
          '(:type context-promote :schema-version 1 :frame-id "frame-2"
            :source-observation-ids ("observation-2")
            :facts ((:id "fact-1" :value "selected")))))
    (should-not (e-context-lifetime-frame-consumed-p first))
    (should (e-context-lifetime-frame-consumed-p first-consumed))
    (should (e-context-lifetime-frame-consumed-p second-consumed))
    (should-error
     (e-context-lifetime-promotion-from-effect first-consumed
                                               (plist-put
                                                (copy-tree effect)
                                                :frame-id "frame-1")))
    (should (equal
             (e-context-lifetime-promotion-frame-id
              (e-context-lifetime-promotion-from-effect
               second-consumed effect))
             "frame-2"))))

(ert-deftest e-context-lifetime-test-promotion-preserves-submitted-fact ()
  "Only the explicitly submitted fact and core-derived provenance persist."
  (let* ((frame
         (e-context-lifetime-frame-complete-for-consumer
           (e-context-lifetime-test--frame) "consumer-1" "response-1"))
         (effect
          '(:type context-promote :schema-version 1 :frame-id "frame-1"
            :source-observation-ids ("observation-1")
            :facts ((:id "fact-1"
                     :value "first-divergence=normalize-price"))))
         (promotion (e-context-lifetime-promotion-from-effect frame effect))
         (record (e-context-lifetime-promotion-record promotion))
         (tail (e-context-lifetime-apply-promotion nil promotion)))
    (should (equal (plist-get (car tail) :value)
                   "first-divergence=normalize-price"))
    (should (equal (plist-get record :source-refs)
                   '("external:canvas:1")))
    (should (equal (plist-get record :source-fingerprints)
                   '("canvas-fingerprint-1")))
    (should-not (plist-member record :body))
    (should-not (plist-member record :observations))
    (should (equal (plist-get record :response-entry-id) "response-1"))))

(ert-deftest e-context-lifetime-test-forged-provenance-is-rejected ()
  "Model/adapter provenance controls cannot enter the normalized effect."
  (let ((effect
         '(:type context-promote :schema-version 1 :frame-id "frame-1"
           :source-observation-ids ("observation-1")
           :source-refs ("forged")
           :fingerprint "forged"
           :facts ((:id "fact-1" :value "fact")))))
    (should-error
     (e-context-lifetime-normalize-promotion-effect effect)
     :type 'e-context-lifetime-invalid-record)))

(ert-deftest e-context-lifetime-test-promotion-requires-consumed-frame ()
  "A promotion cannot select information before its frame is consumed."
  (should-error
   (e-context-lifetime-promotion-from-effect
    (e-context-lifetime-test--frame)
    '(:type context-promote :schema-version 1 :frame-id "frame-1"
      :source-observation-ids ("observation-1")
      :facts ((:id "fact-1" :value "fact"))))
   :type 'e-context-lifetime-invalid-record))

(ert-deftest e-context-lifetime-test-promotion-fact-limits ()
  "Core enforces fact count and normalized UTF-8 byte limits."
  (let ((frame
         (e-context-lifetime-frame-complete-for-consumer
          (e-context-lifetime-test--frame) "consumer-1" "response-1")))
    (dolist (facts (list nil
                         (make-list 17 '((:id "fact" :value "too-many")))))
      (should-error
       (e-context-lifetime-promotion-from-effect
        frame
        (list :type 'context-promote :schema-version 1 :frame-id "frame-1"
              :source-observation-ids '("observation-1") :facts facts))
       :type 'e-context-lifetime-invalid-record))
    (should-error
     (e-context-lifetime-promotion-from-effect
      frame
      (list :type 'context-promote :schema-version 1 :frame-id "frame-1"
            :source-observation-ids '("observation-1")
            :facts (list (list :id "fact-1"
                               :value (make-string 9000 ?x)))))
     :type 'e-context-lifetime-invalid-record)))

(ert-deftest e-context-lifetime-test-promotion-exact-limit-is-accepted ()
  "A complete normalized effect exactly at the byte limit is accepted."
  (let* ((frame
         (e-context-lifetime-frame-complete-for-consumer
           (e-context-lifetime-test--frame) "consumer-1" "response-1"))
         (effect
          (cl-loop for length from 1 to 10000
                   for candidate = (list
                                    :type 'context-promote
                                    :schema-version 1
                                    :frame-id "frame-1"
                                    :source-observation-ids '("observation-1")
                                    :facts (list
                                            (list :id "fact-1"
                                                  :value
                                                  (make-string length ?x))))
                   when (= (e-context-lifetime--bytes
                            (e-context-lifetime-normalize-promotion-effect
                             candidate))
                           8192)
                   return candidate)))
    (should effect)
    (should
     (e-context-lifetime-promotion-p
      (e-context-lifetime-promotion-from-effect
       frame effect)))
    (let* ((fact (car (plist-get effect :facts)))
           (too-large (copy-tree effect)))
      (plist-put too-large :facts
                 (list (list :id (plist-get fact :id)
                             :value
                             (concat (plist-get fact :value) "x"))))
      (should-error
       (e-context-lifetime-normalize-promotion-effect too-large)
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-promotion-fact-schema-is-exact ()
  "Facts use the bounded id/value schema and retain declared order."
  (let ((valid
         '(:type context-promote :schema-version 1 :frame-id "frame-1"
           :source-observation-ids ("observation-1")
           :facts ((:id "fact-1" :value "first")
                   (:id "fact-2" :value (:enabled :json-false))))))
    (let* ((normalized
            (e-context-lifetime-normalize-promotion-effect valid))
           (facts (plist-get normalized :facts)))
      (should (equal (mapcar (lambda (fact) (plist-get fact :id)) facts)
                     '("fact-1" "fact-2")))
      (should (eq (plist-get (plist-get (cadr facts) :value) :enabled)
                  :json-false)))
    (dolist (bad
             (list
              (let ((copy (copy-tree valid)))
                (plist-put copy :facts '((:id "fact-1"))))
              (let ((copy (copy-tree valid)))
                (plist-put copy :facts '((:id "fact-1" :value "x"
                                                :extra "no"))))
              (let ((copy (copy-tree valid)))
                (plist-put copy :facts
                           '((:id "fact-1" :value "x")
                             (:id "fact-1" :value "y"))))
              (let ((copy (copy-tree valid)))
                (plist-put copy :facts '((:value "missing-id"))))
              (let ((copy (copy-tree valid)))
                (plist-put copy :facts '((:id "fact-1" :value
                                          (:unsupported-object . t)))))))
      (should-error
       (e-context-lifetime-normalize-promotion-effect bad)
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-frame-completion-validates-consumer ()
  "Only the owning consumer request may complete an open frame once."
  (let ((frame (e-context-lifetime-test--frame)))
    (should-error
     (e-context-lifetime-frame-complete-for-consumer
      frame "different-consumer" "response-1")
     :type 'e-context-lifetime-invalid-record)
    (let ((consumed
           (e-context-lifetime-frame-complete-for-consumer
            frame "consumer-1" "response-1")))
      (should-error
       (e-context-lifetime-frame-complete-for-consumer
        consumed "consumer-1" "response-2")
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-observation-schema-and-derived-arrays-are-strict ()
  "Observation identity, provenance, delivery, and derived arrays cannot drift."
  (let ((base (car (e-context-lifetime-frame-observations
                    (e-context-lifetime-test--frame)))))
    (dolist (bad
             (list
              (let ((copy (copy-tree base)))
                (plist-put copy :observation-id nil))
              (let ((copy (copy-tree base)))
                (plist-put copy :kind "unknown-kind"))
              (let ((copy (copy-tree base)))
                (plist-put copy :source-entry-ref nil))
              (let ((copy (copy-tree base)))
                (plist-put copy :source-fingerprint nil))
              (let ((copy (copy-tree base)))
                (plist-put copy :effective-delivery "unknown-delivery"))
              (let ((copy (copy-tree base)))
                (plist-put copy :extra "unknown-control"))))
      (should-error
       (e-context-lifetime-frame-create
        :id "bad-frame" :generation-id "generation-1"
        :consumer-request-id "consumer-1" :observations (list bad))
       :type 'e-context-lifetime-invalid-record))
    (should-error
     (e-context-lifetime-frame-create
      :id "missing-body" :generation-id "generation-1"
      :consumer-request-id "consumer-1"
      :observations
      '((:observation-id "observation-1"
         :kind "current-state"
         :source-entry-ref "external:canvas:1"
         :source-fingerprint "fp"
         :effective-delivery "inherited")))
     :type 'e-context-lifetime-invalid-record)
    (should-error
     (e-context-lifetime-frame-create
      :id "duplicate-observations" :generation-id "generation-1"
      :consumer-request-id "consumer-1"
      :observations (list base (copy-tree base)))
     :type 'e-context-lifetime-invalid-record)
    (should-error
     (e-context-lifetime-frame-create
      :id "caller-positional-arrays" :generation-id "generation-1"
      :consumer-request-id "consumer-1" :observations (list base)
      :observation-ids '("observation-1")
      :source-entry-refs '("external:canvas:1")
      :source-fingerprints '("fp"))
     :type 'error)
    (should-error
     (e-context-lifetime-frame-create
      :id "public-completion" :generation-id "generation-1"
      :consumer-request-id "consumer-1" :observations (list base)
      :consuming-response-entry-id "response-1")
     :type 'error)
    (should-error
     (e-context-lifetime-frame-create
      :id "public-promotion" :generation-id "generation-1"
      :consumer-request-id "consumer-1" :observations (list base)
      :promotion-ids '("promotion-1"))
     :type 'error)
    (let* ((open (e-context-lifetime-test--frame))
           (open-copy (e-context-lifetime-frame-copy open))
           (consumed
            (e-context-lifetime-frame-complete-for-consumer
             open "consumer-1" "response-1"))
           (copied (e-context-lifetime-frame-copy consumed)))
      (should-not (e-context-lifetime-frame-consumed-p open))
      (should-not
       (e-context-lifetime-frame-consuming-response-entry-id open))
      (should-not (e-context-lifetime-frame-promotion-ids open))
      (should (equal
               (e-context-lifetime-frame-observations open-copy)
               (e-context-lifetime-frame-observations open)))
      (should (equal
               (plist-get (car (e-context-lifetime-frame-observations
                                open-copy))
                          :body)
               '(:role "user" :content "OBSERVATION-ONE")))
      (should-not (e-context-lifetime-frame-observations consumed))
      (should-not (e-context-lifetime-frame-observations copied))
      (should (equal (e-context-lifetime-frame-observation-ids copied)
                     '("observation-1")))
      (should (equal
               (e-context-lifetime-frame-source-entry-refs copied)
               '("external:canvas:1"))))))

(ert-deftest e-context-lifetime-test-narrow-codecs-reject-shape-drift ()
  "Generation and promotion codecs reject missing, extra, and wrong fields."
  (cl-labels
      ((without-key
        (plist key)
        (let (result)
          (while plist
            (let ((current-key (pop plist))
                  (value (pop plist)))
              (unless (eq current-key key)
                (setq result (append result (list current-key value))))))
          result)))
    (let* ((generation (e-context-lifetime-test--generation))
           (generation-record
            (e-context-lifetime-generation-record generation)))
      (should-error
       (e-context-lifetime-generation-create
        :id "missing-boundary" :checkpoint nil)
       :type 'e-context-lifetime-invalid-record)
      (dolist (bad
               (list
                (without-key generation-record :checkpoint)
                (let ((copy (copy-tree generation-record)))
                  (plist-put copy :record-version "2"))
                (let ((copy (copy-tree generation-record)))
                  (plist-put copy :type "context-generation"))
                (let ((copy (copy-tree generation-record)))
                  (plist-put copy :body "runtime-only"))
                (let ((copy (copy-tree generation-record)))
                  (plist-put copy :durable-tail '("copied")))))
        (should-error
         (e-context-lifetime-generation-from-record bad)
         :type 'e-context-lifetime-invalid-record)))
    (let* ((frame
            (e-context-lifetime-frame-complete-for-consumer
             (e-context-lifetime-test--frame) "consumer-1" "response-1"))
           (promotion
            (e-context-lifetime-promotion-from-effect
             frame
             '(:type context-promote :schema-version 1 :frame-id "frame-1"
               :source-observation-ids ("observation-1")
               :facts ((:id "fact-1" :value "selected")))))
           (record (e-context-lifetime-promotion-record promotion)))
      (dolist (bad
               (list
                (without-key record :facts)
                (let ((copy (copy-tree record)))
                  (plist-put copy :record-version nil))
                (let ((copy (copy-tree record)))
                  (plist-put copy :type "context-promotion"))
                (let ((copy (copy-tree record)))
                  (plist-put copy :body '(:content "runtime")))
                (let ((copy (copy-tree record)))
                  (plist-put copy :durable-tail '("copied")))))
        (should-error
         (e-context-lifetime-promotion-from-record bad)
         :type 'e-context-lifetime-invalid-record)))))

(ert-deftest e-context-lifetime-test-json-false-remains-false ()
  "Nested JSON false keeps its semantic boolean representation."
  (let* ((value (e-context-lifetime-canonicalize
                 '(:metadata (:enabled :json-false))))
         (metadata (plist-get value :metadata)))
    (should (eq (plist-get metadata :enabled) :json-false))))

(ert-deftest e-context-lifetime-test-curation-sources-split-and-strip-envelopes ()
  "Curation sources are ordered semantic values with trusted provenance."
  (let* ((mutable (copy-sequence "first source"))
         (backing-object (make-hash-table :test #'equal))
         (frame
          (let ((created
                 (e-context-lifetime-frame-create-from-segments
                  :id "curation-frame"
                  :generation-id "generation-1"
                  :consumer-request-id "consumer-1"
                  :segments
                  (list
                   (list :kind "current-state"
                         :id "current-source"
                         :messages (list (list :role "user" :content mutable)
                                         (list :role "assistant"
                                               :content '(:nested "second source"))))
                   (list :kind "tool-result"
                         :id "tool-source"
                         :messages
                         (list
                          (list :tool-call '(:id "call-1")
                                :tool-result
                                (list :tool-call-id "call-1"
                                      :content "bounded result"
                                      :metadata backing-object)
                                :message-id "message-1")
                          (list :role "tool"
                                :content
                                (list :tool-call-id "call-2"
                                      :name "lookup"
                                      :status 'ok
                                      :content "message result"
                                      :metadata backing-object))))))))
            (aset mutable 0 ?X)
            created))
         (observations (e-context-lifetime-frame-observations frame))
         (observation-ids
          (mapcar (lambda (observation)
                    (plist-get observation :observation-id))
                  observations))
         (fingerprints
          (mapcar (lambda (observation)
                    (plist-get observation :source-fingerprint))
                  observations))
         (sources (e-context-lifetime-frame-curation-sources frame 1.0))
         (sources-again
          (e-context-lifetime-frame-curation-sources frame 1.0))
         (presentation
          (e-context-lifetime-frame-curation-presentation frame 1.0))
         (record
          (e-context-lifetime-prepare-curation
           frame '(:keep (1)) "response-curation" 1.0)))
    (should (= (length observation-ids)
               (length (delete-dups (copy-sequence observation-ids)))))
    (should (= (length fingerprints)
               (length (delete-dups (copy-sequence fingerprints)))))
    (should (equal (mapcar (lambda (source)
                            (plist-get source :source-observation-id))
                          sources)
                   observation-ids))
    (should (equal (mapcar (lambda (source)
                            (plist-get source :source-fingerprint))
                          sources)
                   fingerprints))
    (should (equal (mapcar (lambda (source)
                            (plist-get source :source-observation-id))
                          sources-again)
                   observation-ids))
    (should (equal (mapcar (lambda (source)
                            (plist-get source :source-fingerprint))
                          sources-again)
                   fingerprints))
    (should (equal (mapcar (lambda (source) (plist-get source :label)) sources)
                   '(1 2 3 4)))
    (should (equal (mapcar (lambda (source) (plist-get source :value)) sources)
                   (list "first source" '(:nested "second source")
                         "bounded result" "message result")))
    (should (equal (plist-get (nth 2 sources) :source-observation-id)
                   (nth 2 observation-ids)))
    (should (equal (plist-get (nth 0 sources) :marker)
                   (format "[1, ~%d tokens]"
                           (plist-get (nth 0 sources) :estimated-tokens))))
    (should-not (string-match-p
                 "call-1\|message-1\|metadata"
                 (prin1-to-string (plist-get (nth 2 sources) :value))))
    (should (equal (plist-get (nth 3 sources) :value) "message result"))
    (should (equal (mapcar (lambda (source) (plist-get source :marker))
                           presentation)
                   (mapcar (lambda (source)
                             (format "[%d, ~%d tokens]"
                                     (plist-get source :label)
                                     (plist-get source :estimated-tokens)))
                           sources)))
    (dolist (source presentation)
      (should-not (plist-member source :source-observation-id))
      (should-not (plist-member source :source-entry-ref))
      (should-not (plist-member source :source-fingerprint)))
    (should (equal (plist-get (car sources) :value) "first source"))
    (should (equal (plist-get (car (plist-get record :items)) :value)
                   "first source"))))

(ert-deftest e-context-lifetime-test-curation-rejects-ambiguous-manual-observation ()
  "Curation rejects a hand-built observation containing multiple sources."
  (let ((frame
         (e-context-lifetime-frame-create
          :id "ambiguous-curation-frame"
          :generation-id "generation-1"
          :consumer-request-id "consumer-1"
          :observations
          '((:observation-id "ambiguous-observation"
             :kind "current-state"
             :source-entry-ref "entry-1"
             :source-fingerprint "fingerprint-1"
             :effective-delivery "inherited"
             :body ((:role "user" :content "first")
                    (:role "assistant" :content "second")))))))
    (should-error
     (e-context-lifetime-frame-curation-sources frame)
     :type 'e-context-lifetime-invalid-record)
    (should-error
     (e-context-lifetime-prepare-curation
      frame '(:keep (1)) "response-1")
     :type 'e-context-lifetime-invalid-record)))

(ert-deftest e-context-lifetime-test-segment-fingerprint-uses-semantic-tool-value ()
  "Tool provenance metadata does not affect semantic source fingerprints."
  (cl-labels
      ((make-frame (call-id message-id backing content)
         (e-context-lifetime-frame-create-from-segments
          :id "semantic-fingerprint-frame"
          :generation-id "generation-1"
          :consumer-request-id "consumer-1"
          :segments
          (list
           (list :kind "tool-result"
                 :id "tool-source"
                 :messages
                 (list
                  (list :tool-call (list :id call-id)
                        :tool-result
                        (list :tool-call-id call-id
                              :content content
                              :metadata backing)
                        :message-id message-id)))))))
    (let* ((first-frame
            (make-frame "call-1" "message-1"
                        (let ((table (make-hash-table :test #'equal)))
                          (puthash "trace" "first" table)
                          table)
                        "same semantic value"))
           (second-frame
            (make-frame "call-2" "message-2"
                        (let ((table (make-hash-table :test #'equal)))
                          (puthash "trace" "second" table)
                          table)
                        "same semantic value"))
           (different-frame
            (make-frame "call-3" "message-3"
                        (make-hash-table :test #'equal)
                        "different semantic value"))
           (first-source
            (car (e-context-lifetime-frame-curation-sources
                  first-frame 1.0)))
           (second-source
            (car (e-context-lifetime-frame-curation-sources
                  second-frame 1.0)))
           (different-source
            (car (e-context-lifetime-frame-curation-sources
                  different-frame 1.0))))
      (should (equal (plist-get first-source :value)
                     "same semantic value"))
      (should (equal (plist-get first-source :value)
                     (plist-get second-source :value)))
      (should (equal (plist-get first-source :source-entry-ref)
                     (plist-get second-source :source-entry-ref)))
      (should (equal (plist-get first-source :source-observation-id)
                     (plist-get second-source :source-observation-id)))
      (should (= (plist-get first-source :label)
                 (plist-get second-source :label)
                 1))
      (should (equal (plist-get first-source :source-fingerprint)
                     (plist-get second-source :source-fingerprint)))
      (should-not (equal (plist-get first-source :value)
                         (plist-get different-source :value)))
      (should-not (equal (plist-get first-source :source-fingerprint)
                         (plist-get different-source :source-fingerprint))))))

(ert-deftest e-context-lifetime-test-curation-estimate-uses-upward-fallback ()
  "Source estimates use the existing ratio and its 4.0 fallback."
  (let* ((frame (e-context-lifetime-test--frame
                 nil "estimate-frame" "estimate-observation" "é"))
         (bytes (string-bytes (prin1-to-string "é")))
         (normal (car (e-context-lifetime-frame-curation-sources frame 2.0)))
         (fallback (car (e-context-lifetime-frame-curation-sources frame 0))))
    (should (= (plist-get normal :estimated-tokens)
               (ceiling (/ bytes 2.0))))
    (should (= (plist-get fallback :estimated-tokens)
               (ceiling (/ bytes 4.0))))))

(ert-deftest e-context-lifetime-test-curation-arguments-are-strict ()
  "Curation arguments accept only optional keep and summary dispositions."
  (let ((valid '(:keep (2)
                 :summaries ((:sources (1 3) :text "combined fact")))))
    (should (equal (e-context-lifetime-normalize-curation-arguments valid)
                   valid))
    (should (equal
             (e-context-lifetime-normalize-curation-arguments
              '(:summaries [(:sources [1] :text "one")] :keep nil))
             '(:keep nil :summaries ((:sources (1) :text "one")))))
    (dolist (bad
             (list
              '(:keep ("1"))
              '(:keep 1)
              '(:keep (1 1))
              '(:keep (1) :summaries ((:sources (1) :text "duplicate")))
              '(:summaries ((:sources nil :text "missing-source")))
              '(:summaries ((:sources (1) :text "")))
              '(:summaries ((:sources (1) :text "ok" :extra t)))
              '(:unknown (1))
              '()))
      (should-error
       (e-context-lifetime-normalize-curation-arguments bad)
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-curation-prepares-v3-record-with-provenance ()
  "Preparation copies selected values and derives ordered provenance only."
  (let* ((mutable (copy-sequence "exact-value"))
         (frame
          (e-context-lifetime-frame-create
           :id "prepare-frame"
           :generation-id "generation-1"
           :consumer-request-id "consumer-1"
           :observations
           (cl-loop for index from 1 to 3
                    collect (list
                             :observation-id (format "obs-%d" index)
                             :kind "current-state"
                             :source-entry-ref (format "ref-%d" index)
                             :source-fingerprint (format "fp-%d" index)
                             :effective-delivery "inherited"
                             :body
                             (list (list :role "user"
                                         :content
                                         (if (= index 1)
                                             mutable
                                           (format "exact-%d" index))))))))
         (record
          (e-context-lifetime-prepare-curation
           frame
           '(:keep (1)
             :summaries ((:sources (2 3) :text "durable replacement")))
           "response-1"
           1.0))
         (items (plist-get record :items)))
    (should (= (plist-get record :record-version) 3))
    (should (eq (plist-get record :type) 'context-promotion))
    (should (equal (mapcar (lambda (item) (plist-get item :kind)) items)
                   '(exact summary)))
    (should (equal (plist-get (car items) :value) "exact-value"))
    (should (equal (plist-get (cadr items) :text) "durable replacement"))
    (should (equal (plist-get (cadr items) :source-observation-ids)
                   '("obs-2" "obs-3")))
    (should (equal (plist-get (cadr items) :source-refs)
                   '("ref-2" "ref-3")))
    (should (equal (plist-get (car items) :source-fingerprints)
                   '("fp-1")))
    (should-not (plist-member (car items) :label))
    (should-not (plist-member (car items) :estimated-tokens))
    (should-not (plist-member (car items) :body))
    (should-not (string-match-p "call-\|message-\|backing"
                                (prin1-to-string record)))
    (should-not (e-context-lifetime-frame-consumed-p frame))
    (should (e-context-lifetime-frame-observations frame))
    (aset mutable 0 ?X)
    (should (equal (plist-get (cadr items) :source-observation-ids)
                   '("obs-2" "obs-3")))
    (should (equal (plist-get (car items) :value) "exact-value"))))

(ert-deftest e-context-lifetime-test-curation-requires-live-frame-and-known-labels ()
  "Prepared curation cannot use consumed frames or labels outside the frame."
  (let ((frame (e-context-lifetime-test--frame)))
    (should-error
     (e-context-lifetime-prepare-curation
      frame '(:keep (2)) "response-1")
     :type 'e-context-lifetime-invalid-record)
    (let ((consumed
           (e-context-lifetime-frame-complete-for-consumer
            frame "consumer-1" "response-1")))
      (should-error
       (e-context-lifetime-prepare-curation
        consumed '(:keep (1)) "response-2")
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-curation-source-and-byte-bounds ()
  "Curation accepts exact 16/8192 limits and rejects one-over values."
  (let ((sixteen (e-context-lifetime-test--multi-source-frame 16))
        (seventeen (e-context-lifetime-test--multi-source-frame 17)))
    (should (= (length (plist-get
                        (e-context-lifetime-prepare-curation
                         sixteen
                         (list :keep (number-sequence 1 16))
                         "response-16")
                        :items))
               16))
    (should-error
     (e-context-lifetime-prepare-curation
      seventeen (list :keep (number-sequence 1 17)) "response-17")
     :type 'e-context-lifetime-invalid-record))
  (let* ((frame (e-context-lifetime-test--multi-source-frame 1))
         (sources (e-context-lifetime-frame-curation-sources frame 1.0))
         (length-at-limit
          (cl-loop for length from 1 to 10000
                   for normalized =
                   (list :keep nil
                         :summaries
                         (list (list :sources '(1)
                                     :text (make-string length ?x))))
                   for candidate =
                   (e-context-lifetime--curation-record
                    frame normalized "response-bytes" sources)
                   when (= (e-context-lifetime--bytes candidate) 8192)
                   return length)))
    (should length-at-limit)
    (let ((effect (list :summaries
                        (list (list :sources '(1)
                                    :text (make-string length-at-limit ?x)))))
          (too-large (list :summaries
                           (list (list :sources '(1)
                                       :text
                                       (make-string (1+ length-at-limit)
                                                    ?x))))))
      (should (= (e-context-lifetime--bytes
                  (e-context-lifetime-prepare-curation
                   frame effect "response-bytes" 1.0))
                 8192))
      (should-error
       (e-context-lifetime-prepare-curation
        frame too-large "response-bytes" 1.0)
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-curation-revision-identity-is-stable ()
  "Revision identity exposes schema, presentation, ratio, and bounds inputs."
  (let ((first (e-context-lifetime-curation-revision-identity 2.0))
        (same (e-context-lifetime-curation-revision-identity 2.0))
        (different (e-context-lifetime-curation-revision-identity 3.0)))
    (should (equal first same))
    (should-not (equal first different))
    (should (equal (plist-get first :schema-revision)
                   "context-curate-v1"))
    (should (equal (plist-get first :presentation-revision)
                   "context-curation-presentation-v1"))
    (should (= (plist-get first :estimate-bytes-per-token) 2.0))
    (should (= (plist-get first :max-sources) 16))
    (should (= (plist-get first :max-record-bytes) 8192))))

(ert-deftest e-context-lifetime-test-curation-v3-codec-projects-literal-messages ()
  "The v3 codec preserves ordered exact/summary content and provenance."
  (let* ((record
          '(:record-version 3
            :type context-promotion
            :id "curation-record-1"
            :frame-id "frame-1"
            :generation-id "generation-1"
            :consumer-request-id "consumer-1"
            :response-entry-id "response-1"
            :items
            ((:kind exact
              :value (:enabled :json-false :topic "kept")
              :source-observation-ids ("observation-1")
              :source-refs ("source-1")
              :source-fingerprints ("fingerprint-1"))
             (:kind summary
              :text "submitted summary"
              :source-observation-ids ("observation-2" "observation-3")
              :source-refs ("source-2" "source-3")
              :source-fingerprints ("fingerprint-2" "fingerprint-3")))))
         (decoded (e-context-lifetime-curation-from-record record))
         (messages (e-context-lifetime-curation-messages decoded)))
    (should (equal decoded record))
    (should (equal messages
                   '((:role system :content (:enabled :json-false :topic "kept"))
                     (:role system :content "submitted summary"))))
    (should-not (string-match-p
                 "Promoted fact\|fingerprint\|observation\|frame-1"
                 (prin1-to-string messages)))))

(ert-deftest e-context-lifetime-test-curation-v3-codec-detaches-and-rejects-drift ()
  "The v3 record boundary detaches content and rejects schema drift."
  (let* ((value (copy-sequence "detached exact"))
         (record
          (list :record-version 3
                :type 'context-promotion
                :id "curation-record-detached"
                :frame-id "frame-detached"
                :generation-id "generation-detached"
                :consumer-request-id "consumer-detached"
                :response-entry-id "response-detached"
                :items
                (list (list :kind 'exact :value value
                            :source-observation-ids '("observation-detached")
                            :source-refs '("source-detached")
                            :source-fingerprints '("fingerprint-detached")))))
         (decoded (e-context-lifetime-curation-from-record record)))
    (aset value 0 ?X)
    (should (equal (plist-get (car (plist-get decoded :items)) :value)
                   "detached exact"))
    (dolist (bad
             (list
              (let ((copy (copy-tree record)))
                (plist-put copy :extra t))
              (let ((copy (copy-tree record)))
                (plist-put copy :items
                           (list (list :kind 'exact :value "value"
                                       :source-observation-ids nil
                                       :source-refs nil
                                       :source-fingerprints nil)))
                copy)
              (let ((copy (copy-tree record)))
                (plist-put copy :items
                           (list (list :kind 'summary :text "summary"
                                       :source-observation-ids '("one")
                                       :source-refs nil
                                       :source-fingerprints '("fingerprint"))))
                copy)
              (let ((copy (copy-tree record)))
                (plist-put copy :items
                           (list (list :kind 'summary :text "summary"
                                       :source-observation-ids '("one")
                                       :source-refs '("source")
                                       :source-fingerprints '("one" "two"))))
                copy)))
      (should-error
       (e-context-lifetime-curation-from-record bad)
       :type 'e-context-lifetime-invalid-record))))

(provide 'e-context-lifetime-test)

;;; e-context-lifetime-test.el ends here
