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

(provide 'e-context-lifetime-test)

;;; e-context-lifetime-test.el ends here
