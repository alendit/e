;;; e-context-lifetime.el --- Generational context lifetime model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral semantic context values.  A generation and a promotion
;; have a narrow durable representation; an observation frame is deliberately
;; runtime-only and bound to one consumer request.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(define-error 'e-context-lifetime-error "Context lifetime error")
(define-error 'e-context-lifetime-invalid-record
  "Invalid context lifetime record"
  'e-context-lifetime-error)

(defgroup e-context-lifetime nil
  "Generational context lifetime projection."
  :group 'e)

(defcustom e-context-lifetime-shadow-projection-enabled nil
  "When non-nil, callers may opt into the Feature 88 shadow projection.

The default is nil so existing request construction and provider behavior do
not change while semantic lifetime values are compared with the legacy
projection."
  :type 'boolean
  :group 'e-context-lifetime)

(defconst e-context-lifetime-record-version 2
  "Version of the narrowed durable generation and promotion records.")

(defconst e-context-lifetime-promotion-schema-version 1
  "Version of the model-facing context promotion effect.")

(defconst e-context-lifetime-promotion-max-facts 16
  "Maximum facts accepted in one normalized promotion effect.")

(defconst e-context-lifetime-promotion-max-bytes 8192
  "Maximum normalized UTF-8 bytes accepted in one promotion effect.")

(defconst e-context-lifetime-observation-kinds
  '("current-state" "dynamic-context" "tool-result" "trace"
    "retrieved-excerpt")
  "Semantic kinds accepted for runtime observation-frame items.")

(defconst e-context-lifetime-observation-delivery-modes
  '("inherited" "request-local-replaceable")
  "Effective delivery modes accepted for runtime observation-frame items.")

(defconst e-context-lifetime-diagnostic-character-limit 1000000
  "Maximum characters visited while estimating diagnostic payload size.")

(defconst e-context-lifetime-diagnostic-item-limit 100000
  "Maximum items visited while estimating diagnostic payload size.")

(defvar e-context-lifetime-diagnostics-hook nil
  "Hook run with bounded context lifetime diagnostics.

Each function receives an event symbol and a scalar-only plist.  Observation
bodies are never passed to this hook.")

(cl-defstruct (e-context-lifetime-generation
               (:constructor e-context-lifetime-generation--create)
               (:conc-name e-context-lifetime-generation-))
  "Portable checkpoint boundary for one semantic generation.

The durable record intentionally contains no copied durable tail.  The later
projection consumer reconstructs that tail from canonical session entries and
promotion provenance."
  id checkpoint covered-session-boundary)

(cl-defstruct (e-context-lifetime-frame
               (:constructor e-context-lifetime-frame--create)
               (:conc-name e-context-lifetime-frame-))
  "Runtime-only observation frame owned by one consumer request.

A non-nil CONSUMING-RESPONSE-ENTRY-ID means that the frame has been consumed.
The completion operation removes its observation body and records only the
consumer binding needed by the in-memory projection."
  id generation-id consumer-request-id observations observation-ids
  source-entry-refs source-fingerprints consuming-response-entry-id
  promotion-ids)

(cl-defstruct (e-context-lifetime-promotion
               (:constructor e-context-lifetime-promotion--create)
               (:conc-name e-context-lifetime-promotion-))
  "Narrow durable fact selection with core-derived source provenance."
  id frame-id generation-id consumer-request-id response-entry-id facts
  source-observation-ids source-refs source-fingerprints)

(defun e-context-lifetime--keyword-plist-p (value)
  "Return non-nil when VALUE is a proper plist with keyword keys."
  (and (proper-list-p value)
       (let ((tail value)
             (valid t))
         (while (and valid tail)
           (if (and (consp tail)
                    (keywordp (car tail))
                    (consp (cdr tail)))
               (setq tail (cddr tail))
             (setq valid nil)))
         (and valid (null tail)))))

(defun e-context-lifetime-canonicalize (value)
  "Return provider-neutral canonical VALUE.

Keyword plist keys remain keywords for Lisp consumers.  Symbol values become
their stable spelling, vectors become lists, and JSON's :json-false sentinel
remains a boolean false rather than becoming the string \":json-false\"."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((eq value :json-false) :json-false)
   ((or (stringp value) (numberp value)) value)
   ((symbolp value) (symbol-name value))
   ((vectorp value)
    (mapcar #'e-context-lifetime-canonicalize (append value nil)))
   ((e-context-lifetime--keyword-plist-p value)
    (let (pairs)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (push (cons key (e-context-lifetime-canonicalize item)) pairs)))
      (setq pairs
            (sort pairs
                  (lambda (left right)
                    (string< (symbol-name (car left))
                             (symbol-name (car right))))))
      (apply #'append
             (mapcar (lambda (pair) (list (car pair) (cdr pair))) pairs))))
   ((consp value)
    (unless (proper-list-p value)
      (signal 'e-context-lifetime-invalid-record
              (list 'unsupported-semantic-value value)))
    (mapcar #'e-context-lifetime-canonicalize value))
   (t
    (signal 'e-context-lifetime-invalid-record
            (list 'unsupported-semantic-value value)))))

(defun e-context-lifetime--require-id (id kind)
  "Validate and return canonical ID for KIND."
  (unless (and id
               (or (and (stringp id) (not (string-empty-p id)))
                   (and (symbolp id)
                        (not (string-empty-p (symbol-name id))))
                   (numberp id)))
    (signal 'e-context-lifetime-invalid-record
            (list kind :id id)))
  (if (symbolp id) (symbol-name id) id))

(defun e-context-lifetime--id-list (value kind)
  "Return canonical unique ID list VALUE for KIND."
  (let ((items (cond
                ((null value) nil)
                ((vectorp value) (append value nil))
                ((proper-list-p value) value)
                (t (signal 'e-context-lifetime-invalid-record
                           (list kind value)))))
        result)
    (dolist (item items (nreverse result))
      (let ((id (e-context-lifetime--require-id item kind)))
        (when (member id result)
          (signal 'e-context-lifetime-invalid-record
                  (list kind :duplicate id)))
        (push id result)))))

(defun e-context-lifetime--reference-list (value kind)
  "Return canonical scalar references VALUE for KIND.

References may repeat: two observations can legitimately resolve to the same
external handle.  Unlike logical ID lists, this helper therefore validates
each item without applying a uniqueness constraint."
  (let ((items (cond
                ((null value) nil)
                ((vectorp value) (append value nil))
                ((and (proper-list-p value)
                      (not (e-context-lifetime--keyword-plist-p value)))
                 value)
                (t (signal 'e-context-lifetime-invalid-record
                           (list kind value)))))
        result)
    (dolist (item items (nreverse result))
      (push (e-context-lifetime--require-id item kind) result))))

(defun e-context-lifetime--bytes (value)
  "Return normalized UTF-8 byte size for semantic VALUE."
  (string-bytes
   (encode-coding-string
    (prin1-to-string (e-context-lifetime-canonicalize value))
    'utf-8 t)))

(defun e-context-lifetime--validate-exact-plist
    (value allowed kind)
  "Validate that VALUE is exactly the keyword plist shape ALLOWED.
Signal the provider-neutral invalid-record condition for every shape error."
  (unless (e-context-lifetime--keyword-plist-p value)
    (signal 'e-context-lifetime-invalid-record
            (list kind :not-keyword-plist value)))
  (let ((keys nil)
        (tail value))
    (while tail
      (push (pop tail) keys)
      (pop tail))
    (setq keys (nreverse keys))
    (unless (and (= (length keys) (length allowed))
                 (= (length keys) (length (delete-dups (copy-sequence keys))))
                 (cl-every (lambda (key) (memq key allowed)) keys)
                 (cl-every (lambda (key) (plist-member value key)) allowed))
      (signal 'e-context-lifetime-invalid-record
              (list kind :keys keys :allowed allowed)))
    value))

(defun e-context-lifetime--validate-fact (fact)
  "Return one canonical exactly-shaped promotion FACT."
  (e-context-lifetime--validate-exact-plist fact '(:id :value) 'fact)
  (list :id (e-context-lifetime--require-id (plist-get fact :id) 'fact-id)
        :value (e-context-lifetime-canonicalize (plist-get fact :value))))

(defun e-context-lifetime--validate-facts (facts)
  "Return canonical bounded FACTS or signal a semantic-record error.

Each fact is exactly `(:id FACT-ID :value SEMANTIC-VALUE)'.  Fact IDs are
unique within one response and declared order is retained."
  (let ((items (cond
                ((vectorp facts) (append facts nil))
                ((and (listp facts)
                      (not (e-context-lifetime--keyword-plist-p facts)))
                 facts)
                (t (signal 'e-context-lifetime-invalid-record
                           (list 'promotion :facts-shape facts)))))
        result
        ids)
    (unless (and (not (null items))
                 (<= (length items) e-context-lifetime-promotion-max-facts))
      (signal 'e-context-lifetime-invalid-record
              (list 'promotion :facts-count (length items))))
    (dolist (fact items (nreverse result))
      (let* ((normalized (e-context-lifetime--validate-fact fact))
             (id (plist-get normalized :id)))
        (when (member id ids)
          (signal 'e-context-lifetime-invalid-record
                  (list 'fact :duplicate-id id)))
        (push id ids)
        (push normalized result)))))

(defun e-context-lifetime--canonical-observation-kind (kind)
  "Return canonical observation KIND or signal a shape error."
  (let ((kind (cond
               ((symbolp kind) (symbol-name kind))
               ((stringp kind) kind))))
    (unless (member kind e-context-lifetime-observation-kinds)
      (signal 'e-context-lifetime-invalid-record
              (list 'observation :kind kind)))
    kind))

(defun e-context-lifetime--canonical-delivery-mode (mode)
  "Return canonical observation delivery MODE or signal a shape error."
  (let ((mode (cond
               ((symbolp mode) (symbol-name mode))
               ((stringp mode) mode))))
    (unless (member mode e-context-lifetime-observation-delivery-modes)
      (signal 'e-context-lifetime-invalid-record
              (list 'observation :effective-delivery mode)))
    mode))

(defun e-context-lifetime--validate-observation (observation)
  "Return one canonical runtime observation from OBSERVATION."
  (e-context-lifetime--validate-exact-plist
   observation
   '(:observation-id :kind :source-entry-ref :source-fingerprint
     :effective-delivery :body)
   'observation)
  (list :observation-id
        (e-context-lifetime--require-id
         (plist-get observation :observation-id) 'observation-id)
        :kind (e-context-lifetime--canonical-observation-kind
               (plist-get observation :kind))
        :source-entry-ref
        (e-context-lifetime--require-id
         (plist-get observation :source-entry-ref) 'source-entry-ref)
        :source-fingerprint
        (e-context-lifetime--require-id
         (plist-get observation :source-fingerprint) 'source-fingerprint)
        :effective-delivery
        (e-context-lifetime--canonical-delivery-mode
         (plist-get observation :effective-delivery))
        ;; Frame bodies are runtime-only.  Preserve them as detached values;
        ;; the projection boundary canonicalizes them when they become model
        ;; input, and no frame codec persists them.
        :body (e-context-lifetime--copy (plist-get observation :body))))

(defun e-context-lifetime--observation-items (observations)
  "Return OBSERVATIONS as a proper sequence or signal a shape error."
  (cond
   ((null observations) nil)
   ((vectorp observations) (append observations nil))
   ((and (proper-list-p observations)
         (not (e-context-lifetime--keyword-plist-p observations)))
    observations)
   (t (signal 'e-context-lifetime-invalid-record
              (list 'frame :observations observations)))))

(defun e-context-lifetime--copy (value)
  "Return a detached semantic copy of VALUE."
  (copy-tree value))

(cl-defun e-context-lifetime-generation-create
    (&key id checkpoint covered-session-boundary)
  "Create a detached generation boundary."
  (unless covered-session-boundary
    (signal 'e-context-lifetime-invalid-record
            (list 'generation :covered-session-boundary
                  covered-session-boundary)))
  (e-context-lifetime-generation--create
   :id (e-context-lifetime--require-id id 'generation)
   :checkpoint (e-context-lifetime-canonicalize checkpoint)
   :covered-session-boundary
   (e-context-lifetime--require-id
    covered-session-boundary 'covered-session-boundary)))

(cl-defun e-context-lifetime-frame-create
    (&key id generation-id consumer-request-id observations)
  "Create a runtime-only observation frame for one consumer request."
  (let* ((generation-id (e-context-lifetime--require-id
                         generation-id 'frame-generation))
         (consumer-request-id
          (e-context-lifetime--require-id
           consumer-request-id 'consumer-request))
         (raw-observations (e-context-lifetime--observation-items observations))
         (observations (mapcar #'e-context-lifetime--validate-observation
                               raw-observations))
         (derived-ids
          (e-context-lifetime--id-list
           (mapcar (lambda (observation)
                     (plist-get observation :observation-id))
                   observations)
           'observation))
         (derived-refs (mapcar (lambda (observation)
                                 (plist-get observation :source-entry-ref))
                               observations))
         (derived-fingerprints
          (mapcar (lambda (observation)
                    (plist-get observation :source-fingerprint))
                observations)))
    (e-context-lifetime-frame--create
     :id (e-context-lifetime--require-id id 'frame)
     :generation-id generation-id
     :consumer-request-id
     consumer-request-id
     :observations observations
     :observation-ids derived-ids
     :source-entry-refs derived-refs
     :source-fingerprints derived-fingerprints
     :consuming-response-entry-id nil
     :promotion-ids nil)))

(defun e-context-lifetime--delivery-for-kind (delivery kind)
  "Return effective DELIVERY mode for semantic KIND.

The core accepts the provider-neutral mapping shape without importing the
backend capability module.  A scalar is retained for legacy adapters and an
omitted mapping entry is conservatively inherited."
  (let ((kind (e-context-lifetime--canonical-observation-kind kind)))
    (cond
     ((eq delivery 'inherited) "inherited")
     ((eq delivery 'request-local-replaceable)
      (if (member kind '("current-state" "dynamic-context"))
          "request-local-replaceable"
        "inherited"))
     ((and (listp delivery)
           (not (e-context-lifetime--keyword-plist-p delivery)))
      (let ((entry
             (seq-find
              (lambda (item)
                (and (e-context-lifetime--keyword-plist-p item)
                     (equal (format "%s" (plist-get item :kind)) kind)))
              delivery)))
        (or (and entry
                 (e-context-lifetime--canonical-delivery-mode
                  (plist-get entry :mode)))
            "inherited")))
     (t "inherited"))))

(defun e-context-lifetime--segment-source-ref (segment)
  "Return a stable provider-neutral source reference for SEGMENT."
  (let ((id (plist-get segment :id)))
    (if (and (or (stringp id) (symbolp id) (numberp id)) id)
        (format "context-source:%s" id)
      (format "context-source:%s"
              (substring (secure-hash 'sha256 (prin1-to-string id)) 0 32)))))

(defun e-context-lifetime--segment-observations (segments delivery)
  "Return validated runtime observations from semantic context SEGMENTS."
  (let (observations)
    (cl-loop for segment in segments
             for index from 0
             for kind = (plist-get segment :kind)
             when (member (format "%s" kind)
                          e-context-lifetime-observation-kinds)
             do (let* ((kind (e-context-lifetime--canonical-observation-kind
                              kind))
                       (messages (copy-tree (plist-get segment :messages)))
                       (fingerprint
                        (or (plist-get segment :fingerprint)
                            (secure-hash 'sha256
                                         (prin1-to-string messages))))
                       (observation-id
                        (format "observation:%s:%s"
                                (substring
                                 (secure-hash 'sha256
                                              (prin1-to-string
                                               (list kind
                                                     (plist-get segment :id)
                                                     index)))
                                 0 24)
                                index)))
                  (push
                   (list :observation-id observation-id
                         :kind kind
                         :source-entry-ref
                         (e-context-lifetime--segment-source-ref segment)
                         :source-fingerprint
                         (e-context-lifetime--require-id
                          fingerprint 'source-fingerprint)
                         :effective-delivery
                         (e-context-lifetime--delivery-for-kind
                          delivery kind)
                         :body messages)
                   observations)))
    (nreverse observations)))

(cl-defun e-context-lifetime-frame-create-from-segments
    (&key id generation-id consumer-request-id segments observation-delivery)
  "Create a runtime FRAME from validated semantic context SEGMENTS.

Only observation segments become frame items.  Source identities and
fingerprints are derived here from the segment identity/value; callers cannot
provide parallel provenance arrays that could drift from the body."
  (e-context-lifetime-frame-create
   :id id
   :generation-id generation-id
   :consumer-request-id consumer-request-id
   :observations
   (e-context-lifetime--segment-observations segments observation-delivery)))

(defun e-context-lifetime--frame-retain-provenance
    (frame response-entry-id promotion-ids)
  "Copy trusted retained provenance from FRAME after its body is consumed.

This private path is intentionally separate from the public frame constructor:
the parallel source arrays are accepted only after they came from an already
validated FRAME, never as caller-supplied positional provenance."
  (let* ((observation-ids
          (e-context-lifetime--id-list
           (e-context-lifetime-frame-observation-ids frame) 'observation))
         (source-entry-refs
          (e-context-lifetime--reference-list
           (e-context-lifetime-frame-source-entry-refs frame)
           'source-entry-ref))
         (source-fingerprints
          (e-context-lifetime--reference-list
           (e-context-lifetime-frame-source-fingerprints frame)
           'source-fingerprint)))
    (unless (and (= (length observation-ids) (length source-entry-refs))
                 (= (length observation-ids) (length source-fingerprints)))
      (signal 'e-context-lifetime-invalid-record
              (list 'frame :retained-provenance-lengths
                    observation-ids source-entry-refs source-fingerprints)))
    (e-context-lifetime-frame--create
     :id (e-context-lifetime--require-id
          (e-context-lifetime-frame-id frame) 'frame)
     :generation-id (e-context-lifetime--require-id
                     (e-context-lifetime-frame-generation-id frame)
                     'frame-generation)
     :consumer-request-id
     (e-context-lifetime--require-id
      (e-context-lifetime-frame-consumer-request-id frame)
      'consumer-request)
     :observations nil
     :observation-ids observation-ids
     :source-entry-refs source-entry-refs
     :source-fingerprints source-fingerprints
     :consuming-response-entry-id
     (and response-entry-id
          (e-context-lifetime--require-id
           response-entry-id 'response-entry))
     :promotion-ids (e-context-lifetime--id-list
                     promotion-ids 'promotion))))

(cl-defun e-context-lifetime-promotion-create
    (&key id frame-id generation-id consumer-request-id response-entry-id facts
          source-observation-ids source-refs source-fingerprints)
  "Create a durable promotion from core-resolved provenance."
  (let* ((source-observation-ids
          (e-context-lifetime--id-list source-observation-ids
                                       'source-observation))
         (source-refs (e-context-lifetime--reference-list
                       source-refs 'source-ref))
         (source-fingerprints
          (e-context-lifetime--reference-list
           source-fingerprints 'source-fingerprint)))
    (unless source-observation-ids
      (signal 'e-context-lifetime-invalid-record
              (list 'promotion :source-observation-ids
                    source-observation-ids)))
    (unless (= (length source-observation-ids) (length source-refs))
      (signal 'e-context-lifetime-invalid-record
              (list 'promotion :source-refs source-observation-ids source-refs)))
    (unless (= (length source-observation-ids)
               (length source-fingerprints))
      (signal 'e-context-lifetime-invalid-record
              (list 'promotion :source-fingerprints
                    source-observation-ids source-fingerprints)))
    (e-context-lifetime-promotion--create
     :id (e-context-lifetime--require-id id 'promotion)
     :frame-id (e-context-lifetime--require-id frame-id 'promotion)
     :generation-id (e-context-lifetime--require-id
                     generation-id 'promotion-generation)
     :consumer-request-id
     (e-context-lifetime--require-id
      consumer-request-id 'promotion-consumer-request)
     :response-entry-id
     (e-context-lifetime--require-id response-entry-id 'response-entry)
     :facts (e-context-lifetime--validate-facts facts)
     :source-observation-ids source-observation-ids
     :source-refs source-refs
     :source-fingerprints source-fingerprints)))

(defun e-context-lifetime-generation-copy (generation)
  "Return a detached copy of GENERATION."
  (unless (e-context-lifetime-generation-p generation)
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-generation-p generation)))
  (e-context-lifetime-generation-create
   :id (e-context-lifetime-generation-id generation)
   :checkpoint (e-context-lifetime-generation-checkpoint generation)
   :covered-session-boundary
   (e-context-lifetime-generation-covered-session-boundary generation)))

(defun e-context-lifetime-frame-copy (frame)
  "Return a detached copy of runtime-only FRAME."
  (unless (e-context-lifetime-frame-p frame)
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-frame-p frame)))
  (if (e-context-lifetime-frame-consumed-p frame)
      (e-context-lifetime--frame-retain-provenance
       frame (e-context-lifetime-frame-consuming-response-entry-id frame)
       (e-context-lifetime-frame-promotion-ids frame))
    (e-context-lifetime-frame-create
     :id (e-context-lifetime-frame-id frame)
     :generation-id (e-context-lifetime-frame-generation-id frame)
     :consumer-request-id
     (e-context-lifetime-frame-consumer-request-id frame)
     :observations (e-context-lifetime-frame-observations frame))))

(defun e-context-lifetime-promotion-copy (promotion)
  "Return a detached copy of PROMOTION."
  (unless (e-context-lifetime-promotion-p promotion)
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-promotion-p promotion)))
  (e-context-lifetime-promotion-create
   :id (e-context-lifetime-promotion-id promotion)
   :frame-id (e-context-lifetime-promotion-frame-id promotion)
   :generation-id (e-context-lifetime-promotion-generation-id promotion)
   :consumer-request-id
   (e-context-lifetime-promotion-consumer-request-id promotion)
   :response-entry-id
   (e-context-lifetime-promotion-response-entry-id promotion)
   :facts (e-context-lifetime-promotion-facts promotion)
   :source-observation-ids
   (e-context-lifetime-promotion-source-observation-ids promotion)
   :source-refs (e-context-lifetime-promotion-source-refs promotion)
   :source-fingerprints
   (e-context-lifetime-promotion-source-fingerprints promotion)))

(defun e-context-lifetime-generation-record (generation)
  "Return the narrowed JSON-friendly durable GENERATION record."
  (let ((generation (e-context-lifetime-generation-copy generation)))
    (list :record-version e-context-lifetime-record-version
          :type 'context-generation
          :id (e-context-lifetime-generation-id generation)
          :checkpoint (e-context-lifetime-generation-checkpoint generation)
          :covered-session-boundary
          (e-context-lifetime-generation-covered-session-boundary generation))))

(defun e-context-lifetime-generation-from-record (record)
  "Decode a version-2 durable GENERATION record."
  (e-context-lifetime--validate-exact-plist
   record
   '(:record-version :type :id :checkpoint :covered-session-boundary)
   'generation)
  (unless (and (equal (plist-get record :record-version)
                      e-context-lifetime-record-version)
               (eq (plist-get record :type) 'context-generation))
    (signal 'e-context-lifetime-invalid-record (list 'generation record)))
  (e-context-lifetime-generation-create
   :id (plist-get record :id)
   :checkpoint (plist-get record :checkpoint)
   :covered-session-boundary
   (plist-get record :covered-session-boundary)))

(defun e-context-lifetime-promotion-record (promotion)
  "Return the narrowed JSON-friendly durable PROMOTION record."
  (let ((promotion (e-context-lifetime-promotion-copy promotion)))
    (list :record-version e-context-lifetime-record-version
          :type 'context-promotion
          :id (e-context-lifetime-promotion-id promotion)
          :frame-id (e-context-lifetime-promotion-frame-id promotion)
          :generation-id
          (e-context-lifetime-promotion-generation-id promotion)
          :consumer-request-id
          (e-context-lifetime-promotion-consumer-request-id promotion)
          :response-entry-id
          (e-context-lifetime-promotion-response-entry-id promotion)
          :facts (e-context-lifetime-promotion-facts promotion)
          :source-observation-ids
          (e-context-lifetime-promotion-source-observation-ids promotion)
          :source-refs (e-context-lifetime-promotion-source-refs promotion)
          :source-fingerprints
          (e-context-lifetime-promotion-source-fingerprints promotion))))

(defun e-context-lifetime-promotion-from-record (record)
  "Decode a version-2 durable PROMOTION record."
  (e-context-lifetime--validate-exact-plist
   record
   '(:record-version :type :id :frame-id :generation-id
     :consumer-request-id :response-entry-id :facts
     :source-observation-ids :source-refs :source-fingerprints)
   'promotion)
  (unless (and (equal (plist-get record :record-version)
                      e-context-lifetime-record-version)
               (eq (plist-get record :type) 'context-promotion))
    (signal 'e-context-lifetime-invalid-record (list 'promotion record)))
  (e-context-lifetime-promotion-create
   :id (plist-get record :id)
   :frame-id (plist-get record :frame-id)
   :generation-id (plist-get record :generation-id)
   :consumer-request-id (plist-get record :consumer-request-id)
   :response-entry-id (plist-get record :response-entry-id)
   :facts (plist-get record :facts)
   :source-observation-ids (plist-get record :source-observation-ids)
   :source-refs (plist-get record :source-refs)
   :source-fingerprints (plist-get record :source-fingerprints)))

(defun e-context-lifetime-frame-consumed-p (frame)
  "Return non-nil when runtime FRAME has a consuming response binding."
  (and (e-context-lifetime-frame-p frame)
       (e-context-lifetime-frame-consuming-response-entry-id frame)))

(defun e-context-lifetime-frame-complete-for-consumer
    (frame consumer-request-id response-entry-id &optional promotion-ids)
  "Consume FRAME for CONSUMER-REQUEST-ID and RESPONSE-ENTRY-ID.

This is the only frame completion operation retained by Feature 88.  It
requires an unconsumed frame owned by CONSUMER-REQUEST-ID, preserves the
consumer binding, drops the observation body, and records the exact promotion
IDs selected by the response handling path."
  (unless (e-context-lifetime-frame-p frame)
    (signal 'e-context-lifetime-invalid-record
            (list 'frame-complete frame)))
  (when (e-context-lifetime-frame-consumed-p frame)
    (signal 'e-context-lifetime-invalid-record
            (list 'frame-complete :already-consumed
                  (e-context-lifetime-frame-id frame))))
  (let ((consumer-request-id
         (e-context-lifetime--require-id
          consumer-request-id 'consumer-request)))
    (unless (equal consumer-request-id
                   (e-context-lifetime-frame-consumer-request-id frame))
      (signal 'e-context-lifetime-invalid-record
              (list 'frame-complete :consumer-mismatch
                    (e-context-lifetime-frame-consumer-request-id frame)
                    consumer-request-id)))
    (e-context-lifetime--frame-retain-provenance
     frame (e-context-lifetime--require-id response-entry-id
                                           'response-entry)
     promotion-ids)))

(defun e-context-lifetime-normalize-promotion-effect (effect)
  "Validate and canonicalize the model-facing context-promote EFFECT.

Only the effect type, wire schema, frame identity, source observation IDs, and
selected facts are accepted.  Adapter/model supplied source references or
fingerprints are unknown controls and are rejected."
  (e-context-lifetime--validate-exact-plist
   effect '(:type :schema-version :frame-id :source-observation-ids :facts)
   'promotion-effect)
  (unless (eq (plist-get effect :type) 'context-promote)
    (signal 'e-context-lifetime-invalid-record
            (list 'promotion-effect :type (plist-get effect :type))))
  (unless (equal (plist-get effect :schema-version)
                 e-context-lifetime-promotion-schema-version)
    (signal 'e-context-lifetime-invalid-record
            (list 'promotion-effect :schema-version
                  (plist-get effect :schema-version))))
  (let* ((source-observation-ids
          (e-context-lifetime--id-list
           (plist-get effect :source-observation-ids) 'source-observation))
         (normalized (list :type 'context-promote
                           :schema-version
                           e-context-lifetime-promotion-schema-version
                           :frame-id
                           (e-context-lifetime--require-id
                            (plist-get effect :frame-id) 'promotion-frame)
                           :source-observation-ids source-observation-ids
                           :facts (e-context-lifetime--validate-facts
                                   (plist-get effect :facts)))))
    (unless source-observation-ids
      (signal 'e-context-lifetime-invalid-record
              (list 'promotion-effect :source-observation-ids
                    source-observation-ids)))
    (when (> (e-context-lifetime--bytes normalized)
             e-context-lifetime-promotion-max-bytes)
      (signal 'e-context-lifetime-invalid-record
              (list 'promotion-effect :bytes
                    (e-context-lifetime--bytes normalized))))
    normalized))

(defun e-context-lifetime--observation-provenance (frame observation-id)
  "Return core-derived provenance for OBSERVATION-ID in FRAME."
  (let* ((observation
          (seq-find
           (lambda (item)
             (and (e-context-lifetime--keyword-plist-p item)
                  (equal (plist-get item :observation-id) observation-id)))
           (e-context-lifetime-frame-observations frame)))
         (position
          (cl-position observation-id
                       (e-context-lifetime-frame-observation-ids frame)
                       :test #'equal))
         (ref (or (and observation
                        (or (plist-get observation :source-entry-ref)
                            (plist-get observation :source-ref)))
                  (and position
                       (nth position
                            (e-context-lifetime-frame-source-entry-refs
                             frame)))))
         (fingerprint
          (or (and observation
                   (plist-get observation :source-fingerprint))
              (and position
                   (nth position
                        (e-context-lifetime-frame-source-fingerprints
                         frame))))))
    (unless (or observation position)
      (signal 'e-context-lifetime-invalid-record
              (list 'promotion :unknown-observation observation-id)))
    (unless (and ref fingerprint)
      (signal 'e-context-lifetime-invalid-record
              (list 'promotion :missing-source-provenance observation-id)))
    (list :ref (e-context-lifetime-canonicalize ref)
          :fingerprint (e-context-lifetime-canonicalize fingerprint))))

(defun e-context-lifetime-promotion-id-for (frame effect)
  "Return a deterministic durable promotion ID for FRAME and EFFECT."
  (format "promotion:%s"
          (substring
           (secure-hash
            'sha256
            (prin1-to-string
             (list (e-context-lifetime-frame-id frame)
                   (e-context-lifetime-frame-generation-id frame)
                   (e-context-lifetime-frame-consumer-request-id frame)
                   (e-context-lifetime-frame-consuming-response-entry-id frame)
                   (e-context-lifetime-normalize-promotion-effect effect))))
           0 32)))

(defun e-context-lifetime-promotion-from-effect (frame effect)
  "Resolve EFFECT against consumed FRAME using core-derived provenance."
  (let* ((effect (e-context-lifetime-normalize-promotion-effect effect))
         (source-ids (plist-get effect :source-observation-ids)))
    (unless (e-context-lifetime-frame-consumed-p frame)
      (signal 'e-context-lifetime-invalid-record
              (list 'promotion :frame-not-consumed
                    (e-context-lifetime-frame-id frame))))
    (unless (equal (plist-get effect :frame-id)
                   (e-context-lifetime-frame-id frame))
      (signal 'e-context-lifetime-invalid-record
              (list 'promotion :frame-mismatch
                    (e-context-lifetime-frame-id frame)
                    (plist-get effect :frame-id))))
    (let (refs fingerprints)
      (dolist (observation-id source-ids)
        (let ((provenance
               (e-context-lifetime--observation-provenance
                frame observation-id)))
          (push (plist-get provenance :ref) refs)
          (push (plist-get provenance :fingerprint) fingerprints)))
      (e-context-lifetime-promotion-create
       :id (e-context-lifetime-promotion-id-for frame effect)
       :frame-id (e-context-lifetime-frame-id frame)
       :generation-id (e-context-lifetime-frame-generation-id frame)
       :consumer-request-id
       (e-context-lifetime-frame-consumer-request-id frame)
       :response-entry-id
       (e-context-lifetime-frame-consuming-response-entry-id frame)
       :facts (plist-get effect :facts)
       :source-observation-ids source-ids
       :source-refs (nreverse refs)
       :source-fingerprints (nreverse fingerprints)))))

(defun e-context-lifetime-apply-promotion (durable-tail promotion)
  "Return DURABLE-TAIL with selected PROMOTION facts appended.

The helper operates on a caller-owned reconstructed tail; it never stores that
tail on the generation record and never copies source observation bodies."
  (unless (e-context-lifetime-promotion-p promotion)
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-promotion-p promotion)))
  (append (e-context-lifetime-canonicalize durable-tail)
          (e-context-lifetime--copy
           (e-context-lifetime-promotion-facts promotion))))

(defun e-context-lifetime--items (value)
  "Return VALUE as a detached sequence of semantic context items."
  (cond
   ((null value) nil)
   ((vectorp value)
    (mapcar #'e-context-lifetime-canonicalize (append value nil)))
   ((e-context-lifetime--keyword-plist-p value)
    (list (e-context-lifetime-canonicalize value)))
   ((listp value) (mapcar #'e-context-lifetime-canonicalize value))
   (t (list (e-context-lifetime-canonicalize value)))))

(defun e-context-lifetime--segment (kind id messages)
  "Return a detached semantic segment for KIND, ID and MESSAGES."
  (let ((messages (e-context-lifetime--items messages)))
    (list :kind kind
          :id id
          :messages messages
          :fingerprint
          (secure-hash 'sha256 (prin1-to-string messages)))))

(cl-defun e-context-lifetime-project
    (generation &optional frame
               &key static-prefix stable-context durable-tail)
  "Purely project GENERATION and optional runtime FRAME into model context.

DURABLE-TAIL is reconstructed by the owning session/projection consumer and is
not part of the durable generation record.  A completed frame contributes no
observation bytes."
  (unless (e-context-lifetime-generation-p generation)
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-generation-p generation)))
  (when (and frame (not (e-context-lifetime-frame-p frame)))
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-frame-p frame)))
  (when (and frame
             (not (equal (e-context-lifetime-frame-generation-id frame)
                         (e-context-lifetime-generation-id generation))))
    (signal 'e-context-lifetime-invalid-record
            (list 'generation-mismatch
                  (e-context-lifetime-frame-generation-id frame)
                  (e-context-lifetime-generation-id generation))))
  (let* ((checkpoint (e-context-lifetime-generation-checkpoint generation))
         (durable-tail (e-context-lifetime-canonicalize durable-tail))
         (observations
          (and frame
               (not (e-context-lifetime-frame-consumed-p frame))
               (e-context-lifetime-frame-observations frame)))
         (segments
          (delq nil
                (list
                 (when static-prefix
                   (e-context-lifetime--segment
                    'static-prefix 'request-static-prefix static-prefix))
                 (when stable-context
                   (e-context-lifetime--segment
                    'stable-context 'request-stable-context stable-context))
                 (when checkpoint
                   (e-context-lifetime--segment
                    'checkpoint
                    (e-context-lifetime-generation-id generation)
                    checkpoint))
                 (when durable-tail
                   (e-context-lifetime--segment
                    'durable-tail
                    (e-context-lifetime-generation-id generation)
                    durable-tail))
                 (when observations
                   (e-context-lifetime--segment
                    'ephemeral-frame
                    (e-context-lifetime-frame-id frame)
                    observations)))))
         (messages (apply #'append
                          (mapcar (lambda (segment)
                                    (plist-get segment :messages))
                                  segments))))
    (list :projection 'generational-context
          :generation-id (e-context-lifetime-generation-id generation)
          :frame-id (and frame (e-context-lifetime-frame-id frame))
          :consumer-request-id
          (and frame
               (e-context-lifetime-frame-consumer-request-id frame))
          :checkpoint (e-context-lifetime-canonicalize checkpoint)
          :durable-tail durable-tail
          :ephemeral (e-context-lifetime-canonicalize observations)
          :segments segments
          :messages messages
          :fingerprint (secure-hash 'sha256 (prin1-to-string segments)))))

(defalias 'e-context-lifetime-shadow-project #'e-context-lifetime-project)

(defun e-context-lifetime-shadow-enabled-p ()
  "Return non-nil when callers opted into the semantic shadow projection."
  e-context-lifetime-shadow-projection-enabled)

(cl-defun e-context-lifetime-shadow-context
    (legacy-context generation &optional frame
                     &key static-prefix stable-context durable-tail)
  "Return semantic context when enabled, otherwise unchanged LEGACY-CONTEXT."
  (if (e-context-lifetime-shadow-enabled-p)
      (e-context-lifetime-project
       generation frame
       :static-prefix static-prefix
       :stable-context stable-context
       :durable-tail durable-tail)
    legacy-context))

(defun e-context-lifetime--character-estimate (value)
  "Return a bounded character estimate for VALUE without retaining its body."
  (let ((remaining e-context-lifetime-diagnostic-character-limit)
        (seen (make-hash-table :test 'eq)))
    (cl-labels
        ((visit (current)
           (when (> remaining 0)
             (cond
              ((stringp current)
               (setq remaining
                     (- remaining (min remaining (length current)))))
              ((symbolp current)
               (setq remaining
                     (- remaining (min remaining
                                      (length (symbol-name current))))))
              ((numberp current)
               (setq remaining
                     (- remaining (min remaining
                                      (length (number-to-string current))))))
              ((or (consp current) (vectorp current))
               (if (gethash current seen)
                   (setq remaining (1- remaining))
                 (puthash current t seen)
                 (if (vectorp current)
                     (dotimes (index (length current))
                       (visit (aref current index)))
                   (visit (car current))
                   (visit (cdr current)))))))))
      (visit value))
    (- e-context-lifetime-diagnostic-character-limit remaining)))

(defun e-context-lifetime--item-count (value)
  "Return a bounded item count for VALUE without copying its contents."
  (cond
   ((null value) 0)
   ((vectorp value)
    (min e-context-lifetime-diagnostic-item-limit (length value)))
   ((and (listp value) (plist-member value :role)) 1)
   ((consp value)
    (let ((count 0)
          (tail value)
          (seen (make-hash-table :test 'eq)))
      (while (and (consp tail)
                  (< count e-context-lifetime-diagnostic-item-limit)
                  (not (gethash tail seen)))
        (puthash tail t seen)
        (setq count (1+ count)
              tail (cdr tail)))
      count))
   (t 1)))

(defun e-context-lifetime-projection-diagnostics (projection)
  "Return bounded diagnostics for PROJECTION without observation bodies."
  (let* ((segments (plist-get projection :segments))
         (ephemeral (plist-get projection :ephemeral))
         (durable (plist-get projection :durable-tail)))
    (list :generation-id (plist-get projection :generation-id)
          :frame-id (plist-get projection :frame-id)
          :segment-count (length segments)
          :durable-item-count (e-context-lifetime--item-count durable)
          :ephemeral-item-count (e-context-lifetime--item-count ephemeral)
          :ephemeral-character-count
          (e-context-lifetime--character-estimate ephemeral)
          :fingerprint (plist-get projection :fingerprint))))

(provide 'e-context-lifetime)

;;; e-context-lifetime.el ends here
