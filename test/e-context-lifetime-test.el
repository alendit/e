;;; e-context-lifetime-test.el --- Tests for semantic context lifetimes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-context-lifetime)

(defun e-context-lifetime-test--prepared-curation-record
    (frame arguments response-entry-id &optional bytes-per-token)
  "Return the promotion component from a prepared disposition.
Tests use the public disposition preparation API directly; this helper keeps
the record-focused assertions concise without restoring the retired wrapper."
  (plist-get
   (e-context-lifetime-prepare-curation-disposition
    frame arguments response-entry-id bytes-per-token)
   :record))

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

(defun e-context-lifetime-test--multi-tool-source-frame (count)
  "Return a live frame with COUNT ordinary tool-result sources."
  (e-context-lifetime-frame-create-from-segments
   :id "multi-tool-source-frame"
   :generation-id "generation-1"
   :consumer-request-id "consumer-1"
   :segments
   (list
    (list :kind "tool-result"
          :id "tool-fanout"
          :messages
          (cl-loop for index from 1 to count
                   collect
                   (list :tool-call
                         (list :id (format "tool-call-%d" index)
                               :name (if (<= index 3) "inspect" "bash"))
                         :tool-result
                         (list :tool-call-id (format "tool-call-%d" index)
                               :name (if (<= index 3) "inspect" "bash")
                               :content (format "tool-source-%d" index))))))))

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

(ert-deftest e-context-lifetime-test-projection-is-enabled-by-default ()
  "The semantic projection is the default request-context behavior."
  (let* ((legacy '(:messages ((:role user :content "legacy"))))
         (generation (e-context-lifetime-test--generation))
         (frame (e-context-lifetime-test--frame)))
    (should e-context-lifetime-shadow-projection-enabled)
    (should (equal (plist-get
                    (e-context-lifetime-shadow-context legacy generation frame)
                    :projection)
                   'generational-context))))

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

(ert-deftest e-context-lifetime-test-literal-v2-record-decodes-strictly ()
  "The compatibility path decodes literal v2 records but has no writer API."
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
    (let ((record
           '(:record-version 2
             :type context-promotion
             :id "promotion-v2"
             :frame-id "frame-v2"
             :generation-id "generation-v2"
             :consumer-request-id "consumer-v2"
             :response-entry-id "response-v2"
             :facts ((:id "fact-v2" :value "selected"))
             :source-observation-ids ("observation-v2")
             :source-refs ("source-v2")
             :source-fingerprints ("fingerprint-v2"))))
      (let ((decoded (e-context-lifetime-promotion-from-record record)))
        (should (e-context-lifetime-promotion-p decoded))
        (should (equal (e-context-lifetime-promotion-id decoded)
                       "promotion-v2"))
        (should (equal (e-context-lifetime-promotion-facts decoded)
                       '((:id "fact-v2" :value "selected")))))
      (dolist (bad
               (list
                (without-key record :facts)
                (let ((copy (copy-tree record)))
                  (plist-put copy :record-version 3))
                (let ((copy (copy-tree record)))
                  (plist-put copy :type "context-promotion"))
                (let ((copy (copy-tree record)))
                  (plist-put copy :body '(:content "runtime-only")))
                (let ((copy (copy-tree record)))
                  (plist-put copy :durable-tail '("copied")))))
        (should-error
         (e-context-lifetime-promotion-from-record bad)
         :type 'e-context-lifetime-invalid-record)))))

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
  "The generation codec rejects missing, extra, and wrong fields."
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
    ))

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
          (e-context-lifetime-test--prepared-curation-record
           frame '(:keep [1])
           "response-curation" 1.0)))
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
    (should (equal (plist-get (nth 2 sources) :tool-call-id)
                   "call-1"))
    (should (equal (plist-get (nth 3 sources) :tool-call-id)
                   "call-2"))
    (should (equal (plist-get (nth 0 sources) :marker)
                   (format "[ephemeral context source 1, ~%d tokens, erase-ineligible]"
                           (plist-get (nth 0 sources) :estimated-tokens))))
    (should (equal (mapcar (lambda (source)
                            (plist-get source :erase-eligible))
                          sources)
                   '(nil nil t t)))
    (should-not (string-match-p
                 "call-1\|message-1\|metadata"
                 (prin1-to-string (plist-get (nth 2 sources) :value))))
    (should (equal (plist-get (nth 3 sources) :value) "message result"))
    (should (equal (mapcar (lambda (source) (plist-get source :marker))
                           presentation)
                   (mapcar (lambda (source)
                             (format "[ephemeral context source %d, ~%d tokens, %s]"
                                     (plist-get source :label)
                                     (plist-get source :estimated-tokens)
                                     (if (plist-get source :erase-eligible)
                                         "erase-eligible"
                                       "erase-ineligible")))
                           sources)))
    (should (equal (mapcar (lambda (source)
                            (plist-get source :erase-eligible))
                          presentation)
                   '(nil nil t t)))
    (dolist (source presentation)
      (should-not (plist-member source :source-observation-id))
      (should-not (plist-member source :source-entry-ref))
      (should-not (plist-member source :source-fingerprint))
      (should-not (plist-member source :tool-call-id)))
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
     (e-context-lifetime-test--prepared-curation-record
      frame '(:keep [1]) "response-1")
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

(ert-deftest e-context-lifetime-test-curation-disposition-arguments-are-strict ()
  "Curation arguments accept optional origin disposition keys strictly."
  (let ((valid '(:keep [2]
                 :summaries [(:sources [1 3] :text "combined fact")])))
    (should (equal (e-context-lifetime-normalize-curation-disposition valid)
                   '(:keep [2]
                     :summaries [(:sources [1 3] :text "combined fact")]
                     :erase [])))
    (should (equal
             (e-context-lifetime-normalize-curation-disposition
              '(:summaries [(:sources [1] :text "one")]
                :keep []))
             '(:keep [] :summaries [(:sources [1] :text "one")]
               :erase [])))
    (should (equal (e-context-lifetime-normalize-curation-disposition nil)
                   '(:keep [] :summaries [] :erase [])))
    (dolist (bad
             (list
              '(:keep ["1"])
              '(:keep 1)
              '(:keep [1 1])
              '(:keep [1] :summaries [(:sources [1] :text "duplicate")]
                :erase [2])
              '(:summaries [(:sources [] :text "missing-source")]
                :keep [])
              '(:summaries [(:sources [1] :text "")]
                :keep [])
              '(:summaries [(:sources [1] :text "ok" :extra t)]
                :keep [])
              '(:keep [1] :summaries [] :erase [1])
              '(:keep [1] :summaries [(:sources [1] :text "overlap")]
                :erase [3])
              '(:keep [1] :summaries [] :erase [1 2])
              '(:unknown [1])
              '(:keep nil)
              '(:keep (1))
              '(:summaries ((:sources [1] :text "list array")))
              '(:summaries [(:sources (1) :text "nested list array")])
              '((keep . [1]))))
      (should-error
       (e-context-lifetime-normalize-curation-disposition bad)
       :type 'e-context-lifetime-invalid-record))
    (should-error
     (e-context-lifetime-normalize-curation-disposition
      (make-hash-table :test #'equal))
     :type 'e-context-lifetime-invalid-record)))

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
          (e-context-lifetime-test--prepared-curation-record
           frame
           '(:keep [1]
             :summaries [(:sources [2 3] :text "durable replacement")])
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
     (e-context-lifetime-test--prepared-curation-record
      frame '(:keep [2]) "response-1")
     :type 'e-context-lifetime-invalid-record)
    (let ((consumed
           (e-context-lifetime-frame-complete-for-consumer
            frame "consumer-1" "response-1")))
      (should-error
       (e-context-lifetime-test--prepared-curation-record
        consumed '(:keep [1]) "response-2")
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-curation-source-and-byte-bounds ()
  "Curation accepts its exact source/byte limits and rejects one-over values."
  (let ((sixteen (e-context-lifetime-test--multi-source-frame 16))
        (seventeen (e-context-lifetime-test--multi-source-frame 17)))
    (should (= (length (plist-get
                        (e-context-lifetime-test--prepared-curation-record
                         sixteen
                         (list :keep (vconcat (number-sequence 1 16))
                               :summaries [])
                         "response-16")
                        :items))
               16))
    (should-error
     (e-context-lifetime-test--prepared-curation-record
      seventeen (list :keep (vconcat (number-sequence 1 17))
                      :summaries [])
      "response-17")
     :type 'e-context-lifetime-invalid-record))
  (let* ((frame (e-context-lifetime-test--multi-source-frame 1))
         (sources (e-context-lifetime-frame-curation-sources frame 1.0))
         (one-byte-summary
          (list :sources [1] :text "x"))
         (one-byte-record
          (e-context-lifetime--curation-record
           frame (list :keep [] :summaries (vector one-byte-summary))
           "response-bytes" sources))
         (one-byte-package
          (list :promotion one-byte-record :erasure nil))
         (length-at-limit
          (+ 1 (- e-context-lifetime-curation-max-record-bytes
                  (e-context-lifetime--bytes one-byte-package)))))
    (let ((effect (list :keep [] :summaries
                        (vector (list :sources [1]
                                      :text (make-string length-at-limit ?x)))))
          (too-large (list :keep [] :summaries
                           (vector (list :sources [1]
                                         :text
                                         (make-string (1+ length-at-limit)
                                                      ?x))))))
      (should (= (e-context-lifetime--bytes
                  (plist-get
                   (e-context-lifetime-prepare-curation-disposition
                    frame effect "response-bytes" 1.0)
                   :package))
                 e-context-lifetime-curation-max-record-bytes))
      (should-error
       (e-context-lifetime-test--prepared-curation-record
        frame too-large "response-bytes" 1.0)
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-curation-disposition-allows-omission ()
  "Optional curation disposes selected sources and omits the rest."
  (let* ((frame (e-context-lifetime-test--multi-source-frame 3))
         (arguments
          '(:keep [1]
            :summaries [(:sources [2] :text "durable summary")]))
         (normalized
          (e-context-lifetime-normalize-curation-disposition arguments 3)))
    (should (equal normalized
                   '(:keep [1]
                     :summaries [(:sources [2] :text "durable summary")]
                     :erase [])))
    (let* ((prepared
            (e-context-lifetime-prepare-curation-disposition
             frame arguments "response-mixed"))
           (record (plist-get prepared :record))
           (items (plist-get record :items)))
      (should (equal (mapcar (lambda (item) (plist-get item :kind)) items)
                     '(exact summary)))
      (should (equal (plist-get (car items) :value) "source-1"))
      (should (equal (plist-get (cadr items) :text) "durable summary"))
      (should (equal (plist-get (car items) :source-observation-ids)
                     '("observation-1")))
      (should (equal (plist-get (cadr items) :source-observation-ids)
                     '("observation-2")))
      (should-not (string-match-p "source-3\\|observation-3\\|entry-3\\|fingerprint-3"
                                  (prin1-to-string record))))
    (let ((all-omitted
           (e-context-lifetime-prepare-curation-disposition
            frame '(:keep [] :summaries [] :erase [])
            "response-all-omitted")))
      (should-not (plist-get all-omitted :record))
      (should-not (plist-get all-omitted :erasure-record))
      (should-not (plist-get all-omitted :package))
      (should (= (plist-get all-omitted :source-count) 3))
      (should (= (plist-get all-omitted :retained-source-count) 0))
      (should (= (plist-get all-omitted :erased-source-count) 0)))
    (dolist (bad
             (list
              '(:keep [1] :summaries [] :erase [1 2 3])
              '(:keep [1] :summaries [(:sources [1] :text "duplicate")])
              '(:keep [] :summaries [(:sources [1] :text "")]
                :erase [])
              '(:keep [1] :summaries [(:sources [1] :text "overlap")]
                :erase [2])))
      (should-error
       (e-context-lifetime-normalize-curation-disposition bad 3)
       :type 'e-context-lifetime-invalid-record))
    (let* ((seventeen (e-context-lifetime-test--multi-source-frame 17))
           (omitted
            (e-context-lifetime-prepare-curation-disposition
             seventeen
             '(:keep [1])
             "response-omitted")))
      (should (plist-get omitted :record))
      (should (= (plist-get omitted :source-count) 17))
      (should (= (plist-get omitted :retained-source-count) 1))
      (should-error
       (e-context-lifetime-prepare-curation-disposition
        seventeen
        (list :keep (vconcat (number-sequence 1 17)))
        "response-retain-17")
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-curation-activity-is-package-scoped ()
  "Activity exposes bounded source stubs for a non-empty semantic package."
  (let* ((frame (e-context-lifetime-test--multi-tool-source-frame 5))
         (prepared
          (e-context-lifetime-prepare-curation-disposition
           frame
           '(:keep [1]
             :summaries [(:sources [2 3] :text "two sources")]
             :erase [4])
           "response-activity"))
         (projection
          (e-context-lifetime-curation-activity-projection prepared)))
    (should (equal projection
                   '(:kept-source-count 1
                     :summary-count 1
                     :summarized-source-count 2
                     :erased-source-count 1
                     :source-stubs
                     ((:disposition kept :source-kind "tool-result"
                       :tool-name "inspect")
                      (:disposition summarized :source-kind "tool-result"
                       :tool-name "inspect")
                      (:disposition summarized :source-kind "tool-result"
                       :tool-name "inspect")
                      (:disposition erased :source-kind "tool-result"
                       :tool-name "bash")))))
    (should-not
     (string-match-p
      "tool-source-\\|observation-\\|entry-\\|fingerprint-\\|response-activity\\|two sources"
      (prin1-to-string projection)))
    (should
     (equal
      (e-context-lifetime-validate-curation-activity-projection
       '(:kept-source-count 1 :summary-count 0
         :summarized-source-count 0 :erased-source-count 0))
      '(:kept-source-count 1 :summary-count 0
        :summarized-source-count 0 :erased-source-count 0)))
    (should-not
     (e-context-lifetime-curation-activity-projection
      (e-context-lifetime-prepare-curation-disposition
       frame '(:keep [] :summaries [] :erase [])
       "response-all-omitted")))
    (dolist (bad
             '((:kept-source-count 0 :summary-count 0
                :summarized-source-count 0 :erased-source-count 0)
               (:kept-source-count 0 :summary-count 1
                :summarized-source-count 0 :erased-source-count 1)
               (:kept-source-count 0 :summary-count 2
                :summarized-source-count 1 :erased-source-count 0)
               (:kept-source-count -1 :summary-count 0
                :summarized-source-count 0 :erased-source-count 1)
               (:kept-source-count 1 :summary-count 0
                :summarized-source-count 0 :erased-source-count 0
                :source-labels (1))
               (:kept-source-count 1 :summary-count 0
                :summarized-source-count 0 :erased-source-count 0
                :source-stubs
                ((:disposition summarized :source-kind "tool-result"
                  :tool-name "inspect")))
               (:kept-source-count 1 :summary-count 0
                :summarized-source-count 0 :erased-source-count 0
                :source-stubs nil)))
      (should-error
       (e-context-lifetime-validate-curation-activity-projection bad)
       :type 'e-context-lifetime-invalid-record))
    (should-error
     (e-context-lifetime-validate-curation-activity-projection
      (list :kept-source-count 17
            :summary-count 0
            :summarized-source-count 0
            :erased-source-count 0
            :source-stubs
            (make-list 17
                       '(:disposition kept :source-kind "current-state"))))
     :type 'e-context-lifetime-invalid-record)))

(ert-deftest e-context-lifetime-test-curation-disposition-bounds-live-frame ()
  "Disposition preparation rejects consumed frames and one-over records."
  (let* ((frame (e-context-lifetime-test--multi-source-frame 1))
         (sources (e-context-lifetime-frame-curation-sources frame 1.0))
         (one-byte-summary
          (list :sources [1] :text "x"))
         (one-byte-record
          (e-context-lifetime--curation-record
           frame (list :keep [] :summaries (vector one-byte-summary))
           "response-disposition-bytes" sources))
         (one-byte-package
          (list :promotion one-byte-record :erasure nil))
         ;; ASCII summary text contributes exactly one canonical byte per
         ;; character, so derive the boundary without a limit-sized search.
         (length-at-limit
          (+ 1 (- e-context-lifetime-curation-max-record-bytes
                  (e-context-lifetime--bytes one-byte-package)))))
    (let* ((at-limit
            (list :keep []
                  :summaries
                  (vector (list :sources [1]
                                :text (make-string length-at-limit ?x)))))
           (one-over
            (list :keep []
                  :summaries
                  (vector (list :sources [1]
                                :text (make-string (1+ length-at-limit) ?x))))))
      (let ((prepared
             (e-context-lifetime-prepare-curation-disposition
              frame at-limit "response-disposition-bytes" 1.0)))
        (should (= (e-context-lifetime--bytes (plist-get prepared :package))
                   e-context-lifetime-curation-max-record-bytes))
        (should (= (plist-get prepared :retained-source-count) 1)))
      (should-error
       (e-context-lifetime-prepare-curation-disposition
        frame one-over "response-disposition-bytes" 1.0)
       :type 'e-context-lifetime-invalid-record))
    (let ((consumed
           (e-context-lifetime-frame-complete-for-consumer
            frame "consumer-1" "response-consumed")))
      (should-error
       (e-context-lifetime-prepare-curation-disposition
        consumed '(:keep [1])
        "response-after-consume")
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-curation-erasure-is-tool-bound-and-content-free ()
  "Erasure records carry ordered tool identities, never source content."
  (let* ((frame (e-context-lifetime-test--multi-tool-source-frame 4))
         (arguments
          '(:keep [1]
            :summaries [(:sources [2] :text "durable summary")]
            :erase [4]))
         (prepared
          (e-context-lifetime-prepare-curation-disposition
           frame arguments "response-erasure"))
         (record (plist-get prepared :record))
         (erasure (plist-get prepared :erasure-record))
         (erased-source (car (plist-get erasure :sources)))
         (encoded (prin1-to-string erasure)))
    (should (equal (plist-get prepared :arguments) arguments))
    (should (eq (plist-get erasure :type) 'context-erasure))
    (should (= (plist-get erasure :record-version) 1))
    (should (equal (plist-get erasure :response-entry-id)
                   "response-erasure"))
    (should (= (length (plist-get erasure :sources)) 1))
    (should (equal (plist-get erased-source :tool-call-id) "tool-call-4"))
    (should (equal (plist-get erased-source :source-observation-id)
                   (plist-get
                    (nth 3 (e-context-lifetime-frame-curation-sources frame))
                    :source-observation-id)))
    (should-not (plist-member erased-source :value))
    (should-not (plist-member erased-source :body))
    (should-not (string-match-p (regexp-quote "tool-source-4") encoded))
    (should-not (string-match-p (regexp-quote "LIVE-TOOL-OUTPUT") encoded))
    (should-not (string-match-p (regexp-quote "tool-source-3")
                                (prin1-to-string record)))
    (should (= (length (plist-get record :items)) 2))
    (should-not
     (plist-member
      (e-context-lifetime-curation-source-presentation
       (car (e-context-lifetime-frame-curation-sources frame)))
      :tool-call-id))
    (should-not
     (plist-member
      (e-context-lifetime-curation-source-presentation
       (car (e-context-lifetime-frame-curation-sources frame)))
      :tool-name))))

(ert-deftest e-context-lifetime-test-curation-omits-invalid-tool-name-metadata ()
  "Unsuitable tool names do not change semantic curation success."
  (dolist (names '((nil "inspect")
                   ("inspect\nspoof" "inspect\nspoof")
                   ("inspect" "bash")))
    (let ((frame
           (e-context-lifetime-frame-create-from-segments
            :id "invalid-tool-name-frame"
            :generation-id "generation-1"
            :consumer-request-id "consumer-1"
            :segments
            (list
             (list :kind "tool-result"
                   :id "tool-source"
                   :messages
                   (list
                    (list :tool-call
                          (list :id "call-1" :name (car names))
                          :tool-result
                          (list :tool-call-id "call-1"
                                :name (cadr names)
                                :content "safe output"))))))))
      (let* ((prepared
              (e-context-lifetime-prepare-curation-disposition
               frame '(:keep [1]) "response-invalid-tool-name"))
             (stub
              (car (plist-get
                    (e-context-lifetime-curation-activity-projection prepared)
                    :source-stubs))))
        (should (equal stub
                       '(:disposition kept :source-kind "tool-result")))))))

(ert-deftest e-context-lifetime-test-curation-erasure-fanout-and-byte-bound ()
  "Erasure shares the disposed-label and prepared-package bounds."
  (let* ((representative e-context-lifetime-curation-max-sources)
         (frame (e-context-lifetime-test--multi-tool-source-frame representative))
         (arguments
          (list :erase (vconcat (number-sequence 1 representative))))
         (prepared
          (e-context-lifetime-prepare-curation-disposition
           frame arguments "response-32"))
         (erasure (plist-get prepared :erasure-record))
         (bytes (e-context-lifetime--bytes erasure)))
    (should (= representative 16))
    (should (<= bytes e-context-lifetime-curation-max-record-bytes))
    (should (= (plist-get prepared :source-count) representative))
    (should (= (plist-get prepared :erased-source-count) representative))
    (should (= (plist-get prepared :retained-source-count) 0))
    (should (plist-get prepared :erase-only-p))
    ;; The shared 16-label ceiling applies to erase as well; there is no
    ;; independent erasure fanout allowance.
    (let* ((larger-count 17)
           (larger-frame
            (e-context-lifetime-test--multi-tool-source-frame larger-count))
           (larger-arguments
            (list
                  :erase (vconcat (number-sequence 1 larger-count)))))
      (should-error
       (e-context-lifetime-prepare-curation-disposition
        larger-frame larger-arguments "response-large")
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-context-lifetime-test-curation-erasure-byte-bound-is-exact ()
  "The shared prepared-package byte bound accepts its exact edge and rejects +1."
  (let* ((count 1)
         (frame (e-context-lifetime-test--multi-tool-source-frame count))
         (sources (e-context-lifetime-frame-curation-sources frame))
         (normalized (list :erase (vconcat (number-sequence 1 count))))
         (base
          (e-context-lifetime--curation-erasure-record
           frame normalized "r" sources))
         ;; The package wrapper is the measured object, so derive a response
         ;; id that places the complete wrapper exactly on the shared bound.
         (package-base (list :promotion nil :erasure base))
         (package-bytes (e-context-lifetime--bytes package-base))
         (package-at-limit-id
          (make-string
           (+ (length "r")
              (- e-context-lifetime-curation-max-record-bytes
                 package-bytes))
           ?r))
         (package-one-over-id (concat package-at-limit-id "r"))
         (prepared
          (e-context-lifetime-prepare-curation-disposition
           frame normalized package-at-limit-id))
      (should (= (e-context-lifetime--bytes
                  (list :promotion nil
                        :erasure (plist-get prepared :erasure-record)))
                 e-context-lifetime-curation-max-record-bytes)))
    (should-error
     (e-context-lifetime-prepare-curation-disposition
      frame normalized package-one-over-id)
     :type 'e-context-lifetime-invalid-record)))

(ert-deftest e-context-lifetime-test-curation-erasure-requires-tool-source ()
  "Erasure rejects non-tool sources and tool envelopes without call IDs."
  (let ((frame (e-context-lifetime-test--frame)))
    (should-error
     (e-context-lifetime-prepare-curation-disposition
      frame '(:erase [1])
      "response-non-tool")
     :type 'e-context-lifetime-invalid-record))
  (let ((frame
         (e-context-lifetime-frame-create-from-segments
          :id "missing-tool-id-frame"
          :generation-id "generation-1"
          :consumer-request-id "consumer-1"
          :segments
          (list (list :kind "tool-result"
                      :id "tool-source"
                      :messages (list (list :role "tool"
                                            :content "no call id")))))))
    (should-error
     (e-context-lifetime-prepare-curation-disposition
      frame '(:erase [1])
      "response-missing-tool-id")
     :type 'e-context-lifetime-invalid-record))
  (let ((frame
         (e-context-lifetime-frame-create-from-segments
          :id "mismatched-tool-id-frame"
          :generation-id "generation-1"
          :consumer-request-id "consumer-1"
          :segments
          (list (list :kind "tool-result"
                      :id "tool-source"
                      :messages
                      (list (list :tool-call '(:id "call-a")
                                  :tool-result
                                  '(:tool-call-id "call-b"
                                    :content "mismatched ids"))))))))
    (should-error
     (e-context-lifetime-prepare-curation-disposition
      frame '(:erase [1])
     "response-mismatched-tool-id")
     :type 'e-context-lifetime-invalid-record)))

(ert-deftest e-context-lifetime-test-curation-erasure-v1-codec-is-detached-and-strict ()
  "The version-1 erasure codec accepts only content-free source identities."
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
    (let ((record
           '(:record-version 1
             :type context-erasure
             :id "erasure-1"
             :frame-id "frame-1"
             :generation-id "generation-1"
             :consumer-request-id "consumer-1"
             :response-entry-id "response-1"
             :sources ((:source-observation-id "observation-1"
                        :source-ref "external:tool:1"
                        :source-fingerprint "fingerprint-1"
                        :tool-call-id "tool-call-1")))))
      (should (equal (e-context-lifetime-curation-erasure-from-record record)
                     record))
      (dolist (bad
               (list
                (without-key record :sources)
                (let ((copy (copy-tree record)))
                  (plist-put copy :record-version 2))
                (let ((copy (copy-tree record)))
                  (plist-put copy :type 'context-promotion))
                (let ((copy (copy-tree record)))
                  (plist-put copy :source-body "must-not-persist"))
                (let ((copy (copy-tree record)))
                  (plist-put
                   copy :sources
                   '((:source-observation-id "observation-1"
                      :source-ref "external:tool:1"
                      :source-fingerprint "fingerprint-1"
                      :tool-call-id "tool-call-1"
                      :value "tool output"))))
                (let ((copy (copy-tree record)))
                  (plist-put
                   copy :sources
                   '((:source-observation-id "observation-1"
                      :source-ref "external:tool:1"
                      :source-fingerprint "fingerprint-1"
                      :tool-call-id "tool-call-1")
                     (:source-observation-id "observation-1"
                      :source-ref "external:tool:2"
                      :source-fingerprint "fingerprint-2"
                      :tool-call-id "tool-call-2"))))))
        (should-error
         (e-context-lifetime-curation-erasure-from-record bad)
         :type 'e-context-lifetime-invalid-record)))))

(ert-deftest e-context-lifetime-test-curation-erasure-v1-codec-detaches-mutated-input ()
  "Decoded erasure identities do not alias mutable caller strings or plists."
  (let* ((id (copy-sequence "erasure-input"))
         (frame-id (copy-sequence "frame-input"))
         (generation-id (copy-sequence "generation-input"))
         (consumer-id (copy-sequence "consumer-input"))
         (response-id (copy-sequence "response-input"))
         (observation-id (copy-sequence "observation-input"))
         (source-ref (copy-sequence "source-ref-input"))
         (fingerprint (copy-sequence "fingerprint-input"))
         (tool-call-id (copy-sequence "tool-call-input"))
         (source (list :source-observation-id observation-id
                       :source-ref source-ref
                       :source-fingerprint fingerprint
                       :tool-call-id tool-call-id))
         (record (list :record-version 1
                       :type 'context-erasure
                       :id id
                       :frame-id frame-id
                       :generation-id generation-id
                       :consumer-request-id consumer-id
                       :response-entry-id response-id
                       :sources (list source)))
         (decoded (e-context-lifetime-curation-erasure-from-record record)))
    (dolist (value (list id frame-id generation-id consumer-id response-id
                         observation-id source-ref fingerprint tool-call-id))
      (setf (aref value 0) ?X))
    (plist-put source :tool-call-id "replaced-tool-call")
    (should (equal (plist-get decoded :id) "erasure-input"))
    (should (equal (plist-get decoded :frame-id) "frame-input"))
    (should (equal (plist-get decoded :generation-id) "generation-input"))
    (should (equal (plist-get decoded :consumer-request-id) "consumer-input"))
    (should (equal (plist-get decoded :response-entry-id) "response-input"))
    (let ((decoded-source (car (plist-get decoded :sources))))
      (should (equal (plist-get decoded-source :source-observation-id)
                     "observation-input"))
      (should (equal (plist-get decoded-source :source-ref)
                     "source-ref-input"))
      (should (equal (plist-get decoded-source :source-fingerprint)
                     "fingerprint-input"))
      (should (equal (plist-get decoded-source :tool-call-id)
                     "tool-call-input")))))

(ert-deftest e-context-lifetime-test-curation-revision-identity-is-stable ()
  "Revision identity exposes schema, presentation, ratio, and bounds inputs."
  (let ((first (e-context-lifetime-curation-revision-identity 2.0))
        (same (e-context-lifetime-curation-revision-identity 2.0))
        (different (e-context-lifetime-curation-revision-identity 3.0)))
    (should (equal first same))
    (should-not (equal first different))
    (should (equal (plist-get first :schema-revision)
                   "context-curate-v8"))
    (should (equal (plist-get first :presentation-revision)
                   "context-curation-presentation-v3"))
    (should (= (plist-get first :estimate-bytes-per-token) 2.0))
    (should (= (plist-get first :max-sources) 16))
    (should (= (plist-get first :max-record-bytes)
               e-context-lifetime-curation-max-record-bytes))
    (should (= e-context-lifetime-curation-max-record-bytes
               (* 1024 1024)))))

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
