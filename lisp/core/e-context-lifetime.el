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

(declare-function e-context-budget-value-token-estimate
                  "e-context-budget")
(defvar e-context-budget-estimate-bytes-per-token)

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
  "Version of the narrowed durable generation and legacy promotion records.")

(defconst e-context-lifetime-portable-message-roles
  '(system user assistant)
  "Backend-neutral roles allowed in a portable generation checkpoint.

Tool calls/results and provider-specific roles are observations or wire
artifacts, not portable checkpoint content.  The codec rehydrates JSON role
strings to these established symbolic roles before a checkpoint is exposed to
the rest of E.")

(defun e-context-lifetime--portable-role (role)
  "Return the finite backend-neutral ROLE mapping or signal an error.

Do not intern provider or caller supplied strings here: a checkpoint codec must
have a closed role vocabulary."
  (let ((role (cond
               ((memq role e-context-lifetime-portable-message-roles) role)
               ((equal role "system") 'system)
               ((equal role "user") 'user)
               ((equal role "assistant") 'assistant))))
    (unless (memq role e-context-lifetime-portable-message-roles)
      (signal 'e-context-lifetime-invalid-record
              (list 'portable-message :role role)))
    role))

(defconst e-context-lifetime-legacy-promotion-max-facts 16
  "Maximum facts accepted while decoding a version-2 promotion record.

This bound belongs to the read-only compatibility decoder.  New writes use the
version-3 curation codec and its complete-record bound.")

(defconst e-context-lifetime-curation-record-version 3
  "Version of durable prepared context-curation records.")

(defconst e-context-lifetime-curation-schema-revision "context-curate-v1"
  "Stable revision of the model-facing context-curate shape.")

(defconst e-context-lifetime-curation-presentation-revision
  "context-curation-presentation-v1"
  "Stable revision of frame-local curation labels and size markers.")

(defconst e-context-lifetime-curation-max-sources 16
  "Maximum distinct frame-local sources disposed by one curation.")

(defconst e-context-lifetime-curation-max-record-bytes 8192
  "Maximum canonical UTF-8 bytes in one prepared curation record.")

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
                 (<= (length items) e-context-lifetime-legacy-promotion-max-facts))
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

(defun e-context-lifetime--portable-message (message)
  "Return canonical portable MESSAGE or signal an invalid-record error.

Portable checkpoint messages deliberately have only ROLE and CONTENT.  This
keeps storage/projection metadata, transcript identity, provider controls, and
tool wire fields out of the persisted semantic checkpoint."
  (e-context-lifetime--validate-exact-plist
   message '(:role :content) 'portable-message)
  (let* ((role (e-context-lifetime--portable-role
                (plist-get message :role))))
    (list :role role
          :content (e-context-lifetime-canonicalize
                    (plist-get message :content)))))

(defun e-context-lifetime-portable-message (message)
  "Return the exact portable ROLE/CONTENT projection of real MESSAGE.

Metadata and transcript identity are discarded before the strict portable
checkpoint codec is called.  Existing checkpoint messages should instead use
`e-context-lifetime-portable-checkpoint', which rejects extra fields."
  (unless (and (e-context-lifetime--keyword-plist-p message)
               (plist-member message :role)
               (plist-member message :content))
    (signal 'e-context-lifetime-invalid-record
            (list 'portable-message :missing-fields message)))
  (e-context-lifetime--portable-message
   (list :role (plist-get message :role)
         :content (plist-get message :content))))

(defun e-context-lifetime--portable-checkpoint
    (checkpoint &optional require-nonempty)
  "Return canonical portable CHECKPOINT message sequence.

JSON arrays may arrive as lists or vectors.  A single keyword plist is never
accepted as a message sequence.  When REQUIRE-NONEMPTY is non-nil, reject the
empty checkpoint used only by the initial identity generation."
  (let ((messages
         (cond
          ((null checkpoint) nil)
          ((vectorp checkpoint) (append checkpoint nil))
          ((and (proper-list-p checkpoint)
                (not (e-context-lifetime--keyword-plist-p checkpoint)))
           checkpoint)
          (t
           (signal 'e-context-lifetime-invalid-record
                   (list 'generation :checkpoint checkpoint)))))
        result)
    (when (and require-nonempty (null messages))
      (signal 'e-context-lifetime-invalid-record
              (list 'generation :empty-checkpoint)))
    (dolist (message messages (nreverse result))
      (push (e-context-lifetime--portable-message message) result))))

(defun e-context-lifetime-portable-checkpoint
    (checkpoint &optional require-nonempty)
  "Return canonical portable CHECKPOINT messages.

This is the public codec used by compaction and provider-neutral projection
consumers.  REQUIRE-NONEMPTY is used only for a deliberate new generation;
the initial identity generation may retain a nil checkpoint."
  (e-context-lifetime--portable-checkpoint checkpoint require-nonempty))

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
        ;; Frame bodies are runtime-only.  Preserve them as recursively
        ;; detached values; the projection boundary canonicalizes them when
        ;; they become model input, and no frame codec persists them.
        :body (e-context-lifetime--detached-copy
               (plist-get observation :body))))

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

(defun e-context-lifetime--detached-copy (value)
  "Return a recursively detached copy of semantic VALUE.

`copy-tree' does not copy strings or vector elements.  Curation exact values
cross a runtime presentation boundary, so this narrower helper also detaches
those leaves without changing the established copies used by the v2 path."
  (cond
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (vconcat
     (mapcar #'e-context-lifetime--detached-copy (append value nil))))
   ((consp value)
    (cons (e-context-lifetime--detached-copy (car value))
          (e-context-lifetime--detached-copy (cdr value))))
   (t value)))

(defun e-context-lifetime--detached-canonical (value)
  "Return a detached canonical copy of semantic VALUE."
  (e-context-lifetime--detached-copy
   (e-context-lifetime-canonicalize value)))

(cl-defun e-context-lifetime-generation-create
    (&key id checkpoint covered-session-boundary)
  "Create a detached generation boundary."
  (unless covered-session-boundary
    (signal 'e-context-lifetime-invalid-record
            (list 'generation :covered-session-boundary
                  covered-session-boundary)))
  (e-context-lifetime-generation--create
   :id (e-context-lifetime--require-id id 'generation)
   ;; NIL is reserved for the initial opt-in identity generation.  Deliberate
   ;; portable boundaries validate a non-empty checkpoint at their service
   ;; boundary, while the core codec still preserves this initialization form.
   :checkpoint (e-context-lifetime--portable-checkpoint checkpoint)
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

(defun e-context-lifetime--segment-source-items (value)
  "Return raw independently rendered items from segment MESSAGES VALUE.

Do not canonicalize the complete message envelope here.  Tool-result
transport metadata can contain backing objects that must be discarded by the
source projection before its semantic content is canonicalized."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((and (proper-list-p value)
         (not (e-context-lifetime--keyword-plist-p value)))
    value)
   (t (list value))))

(defun e-context-lifetime--curation-tool-result-content (result item)
  "Return semantic content from tool RESULT in ITEM or signal for an envelope.

RESULT is allowed to be a bounded scalar/sequence value when a caller has
already stripped the result envelope.  A keyword plist without `:content' is
not accepted here, since retaining it would risk copying tool metadata or a
backing object into the semantic source."
  (cond
   ((and (e-context-lifetime--keyword-plist-p result)
         (plist-member result :content))
    (plist-get result :content))
   ((or (null result) (eq result t) (eq result :json-false)
        (stringp result) (numberp result) (vectorp result)
        (and (proper-list-p result)
             (not (e-context-lifetime--keyword-plist-p result))))
    result)
   (t
    (signal 'e-context-lifetime-invalid-record
            (list 'curation-source :tool-result-envelope item)))))

(defun e-context-lifetime--curation-source-value (kind item)
  "Extract the exact semantic source value from KIND and runtime ITEM.

Ordinary message envelopes contribute only `:content'.  Tool-result envelopes
contribute only bounded result `:content'; call/replay IDs, message IDs,
acknowledgements, metadata, and backing objects never become source values."
  (if (equal kind "tool-result")
      (cond
       ((and (e-context-lifetime--keyword-plist-p item)
             (plist-member item :tool-result))
        (e-context-lifetime--curation-tool-result-content
         (plist-get item :tool-result) item))
       ((and (e-context-lifetime--keyword-plist-p item)
             (plist-member item :content))
        (let ((content (plist-get item :content)))
          (if (and (e-context-lifetime--keyword-plist-p content)
                   (plist-member content :tool-call-id)
                   (plist-member content :name)
                   (plist-member content :status)
                   (plist-member content :content))
              (e-context-lifetime--curation-tool-result-content
               content item)
            content)))
       (t
        (signal 'e-context-lifetime-invalid-record
                (list 'curation-source :missing-tool-result-content item))))
    (if (and (e-context-lifetime--keyword-plist-p item)
             (plist-member item :content))
        (plist-get item :content)
      item)))

(defun e-context-lifetime--segment-observations (segments delivery)
  "Return one validated observation per semantic source from SEGMENTS and DELIVERY.

Each message/value in an observation segment is independently curatable, so
its observation identity and fingerprint include the canonical segment and
message positions.  The segment source reference may remain shared because it
identifies the external source that delivered the values."
  (let (observations)
    (cl-loop for segment in segments
             for index from 0
             for kind = (plist-get segment :kind)
             when (member (format "%s" kind)
                          e-context-lifetime-observation-kinds)
             do (let* ((kind (e-context-lifetime--canonical-observation-kind
                              kind))
                       (messages (e-context-lifetime--segment-source-items
                                  (plist-get segment :messages)))
                       (source-entry-ref
                        (e-context-lifetime--segment-source-ref segment))
                       (segment-fingerprint
                        (and (plist-get segment :fingerprint)
                             (e-context-lifetime--require-id
                              (plist-get segment :fingerprint)
                              'segment-fingerprint)))
                       (semantic-values
                        (mapcar
                         (lambda (message)
                           (e-context-lifetime--detached-canonical
                            (e-context-lifetime--curation-source-value
                             kind message)))
                         messages)))
                  (cl-loop for message in messages
                           for semantic-value in semantic-values
                           for message-index from 0
                           for identity-inputs =
                           (list :kind kind
                                 :source-entry-ref source-entry-ref
                                 :segment-index index
                                 :message-index message-index
                                 :source-value semantic-value
                                 :segment-fingerprint segment-fingerprint)
                           for observation-id =
                           (format "observation:%s:%s"
                                   (substring
                                    (secure-hash 'sha256
                                                 (prin1-to-string
                                                  identity-inputs))
                                    0 24)
                                   message-index)
                           for source-fingerprint =
                           (secure-hash 'sha256
                                        (prin1-to-string identity-inputs))
                           do (push
                               (list :observation-id observation-id
                                     :kind kind
                                     :source-entry-ref source-entry-ref
                                     :source-fingerprint source-fingerprint
                                     :effective-delivery
                                     (e-context-lifetime--delivery-for-kind
                                      delivery kind)
                                     :body
                                     (e-context-lifetime--detached-copy
                                      message))
                               observations))))
    (nreverse observations)))

(cl-defun e-context-lifetime-frame-create-from-segments
    (&key id generation-id consumer-request-id segments observation-delivery)
  "Create a runtime FRAME from validated semantic context SEGMENTS.

Only observation segments become frame items, with one frame observation per
independently rendered message/value.  Source identities and fingerprints are
derived here from the segment identity/value and message position; callers
cannot provide parallel provenance arrays that could drift from the body.  ID,
GENERATION-ID, and CONSUMER-REQUEST-ID bind the resulting frame."
  (e-context-lifetime-frame-create
   :id id
   :generation-id generation-id
   :consumer-request-id consumer-request-id
   :observations
   (e-context-lifetime--segment-observations segments observation-delivery)))

(defun e-context-lifetime--curation-estimator-ratio (&optional bytes-per-token)
  "Return the effective curation estimator ratio.

BYTES-PER-TOKEN overrides the configured ratio.

Load the budget owner only when presentation is requested.  This keeps the
existing `e-session' to `e-context-lifetime' load direction acyclic while
sharing the established invalid-ratio fallback."
  (require 'e-context-budget)
  (let ((ratio (or bytes-per-token
                   e-context-budget-estimate-bytes-per-token)))
    (if (and (numberp ratio) (> ratio 0))
        ratio
      4.0)))

(defun e-context-lifetime--curation-estimate (value bytes-per-token)
  "Return the established approximate token estimate for source VALUE.
BYTES-PER-TOKEN supplies the ratio."
  (require 'e-context-budget)
  (e-context-budget-value-token-estimate value bytes-per-token))

(defun e-context-lifetime-curation-revision-identity
    (&optional bytes-per-token)
  "Return stable identity inputs for curation presentation and schema.

BYTES-PER-TOKEN overrides the configured estimator ratio.

Per-frame labels and estimates are intentionally absent.  The effective
estimator ratio and the stable presentation/schema revisions are the inputs
that an outer anchor or cache identity can fence later."
  (list :presentation-revision
        e-context-lifetime-curation-presentation-revision
        :schema-revision e-context-lifetime-curation-schema-revision
        :record-version e-context-lifetime-curation-record-version
        :estimate-bytes-per-token
        (e-context-lifetime--curation-estimator-ratio bytes-per-token)
        :max-sources e-context-lifetime-curation-max-sources
        :max-record-bytes e-context-lifetime-curation-max-record-bytes))

(defun e-context-lifetime--curation-raw-items (value)
  "Return runtime semantic items in VALUE's canonical sequence order.

Do not canonicalize VALUE before this split: a tool-result item can contain
provider replay metadata and backing objects that must be discarded before the
semantic content is canonicalized."
  (e-context-lifetime--segment-source-items value))

(defun e-context-lifetime--curation-source-descriptors
    (frame bytes-per-token)
  "Return trusted, labeled source descriptors for live FRAME.

BYTES-PER-TOKEN supplies the estimate ratio."
  (unless (e-context-lifetime-frame-p frame)
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-frame-p frame)))
  (when (e-context-lifetime-frame-consumed-p frame)
    (signal 'e-context-lifetime-invalid-record
            (list 'curation :frame-not-live
                  (e-context-lifetime-frame-id frame))))
  (let ((ratio (e-context-lifetime--curation-estimator-ratio
                bytes-per-token))
        (label 0)
        result)
    (dolist (observation (e-context-lifetime-frame-observations frame)
                         (nreverse result))
      (let ((kind (plist-get observation :kind))
            (observation-id (plist-get observation :observation-id))
            (source-entry-ref (plist-get observation :source-entry-ref))
            (source-fingerprint (plist-get observation :source-fingerprint))
            (items (e-context-lifetime--curation-raw-items
                    (plist-get observation :body))))
        (unless (= (length items) 1)
          (signal 'e-context-lifetime-invalid-record
                  (list 'curation-source :ambiguous-observation
                        observation-id (length items))))
        (let* ((value
                (e-context-lifetime--detached-canonical
                          (e-context-lifetime--curation-source-value
                           kind (car items))))
               (source-label (setq label (1+ label)))
               (estimated-tokens
                (e-context-lifetime--curation-estimate value ratio)))
          (push
           (list :label source-label
                 :value value
                 :estimated-tokens estimated-tokens
                 :marker (format "[%d, ~%d tokens]"
                                 source-label estimated-tokens)
                 :kind kind
                 :source-observation-id
                 (e-context-lifetime--detached-copy observation-id)
                 :source-entry-ref
                 (e-context-lifetime--detached-copy source-entry-ref)
                 :source-fingerprint
                 (e-context-lifetime--detached-copy source-fingerprint))
           result))))))

(defun e-context-lifetime-frame-curation-sources
    (frame &optional bytes-per-token)
  "Return labeled trusted curation sources from still-live FRAME.

BYTES-PER-TOKEN overrides the configured estimate ratio.

The returned descriptors retain core-derived provenance for preparation.  Use
`e-context-lifetime-frame-curation-presentation' for the model-facing subset
without internal identities."
  (e-context-lifetime--curation-source-descriptors frame bytes-per-token))

(defun e-context-lifetime-curation-source-presentation (source)
  "Return SOURCE's detached model-facing presentation subset.

Only the local label, exact semantic value, and informational size marker are
returned.  Frame and provenance identities remain in the trusted descriptor,
never in this presentation shape."
  (unless (and (e-context-lifetime--keyword-plist-p source)
               (plist-member source :label)
               (plist-member source :value)
               (plist-member source :estimated-tokens)
               (plist-member source :marker))
    (signal 'e-context-lifetime-invalid-record
            (list 'curation-source :presentation source)))
  (list :label (plist-get source :label)
        :value (e-context-lifetime--detached-copy
                (plist-get source :value))
        :estimated-tokens (plist-get source :estimated-tokens)
        :marker (copy-sequence (plist-get source :marker))))

(defun e-context-lifetime-frame-curation-presentation
    (frame &optional bytes-per-token)
  "Return model-facing labeled source presentation for live FRAME.
BYTES-PER-TOKEN overrides the configured estimate ratio."
  (mapcar #'e-context-lifetime-curation-source-presentation
          (e-context-lifetime-frame-curation-sources
           frame bytes-per-token)))

(defun e-context-lifetime--curation-sequence (value kind)
  "Return VALUE as a strict list-shaped curation sequence of KIND."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((and (proper-list-p value)
         (not (e-context-lifetime--keyword-plist-p value)))
    value)
   (t
    (signal 'e-context-lifetime-invalid-record
            (list kind :not-array value)))))

(defun e-context-lifetime--curation-positive-label (value kind)
  "Validate one strict positive integer curation label VALUE for KIND."
  (unless (and (integerp value) (> value 0))
    (signal 'e-context-lifetime-invalid-record
            (list kind :positive-integer-label value)))
  value)

(defun e-context-lifetime--curation-labels
    (value kind &optional require-nonempty)
  "Return strict positive integer labels from VALUE for KIND.
REQUIRE-NONEMPTY rejects an empty sequence when non-nil."
  (let ((items (e-context-lifetime--curation-sequence value kind)))
    (when (and require-nonempty (null items))
      (signal 'e-context-lifetime-invalid-record
              (list kind :empty value)))
    (mapcar (lambda (item)
              (e-context-lifetime--curation-positive-label item kind))
            items)))

(defun e-context-lifetime-normalize-curation-arguments (arguments)
  "Normalize strict model-facing context-curate ARGUMENTS.

The model-facing shape has only optional `:keep' and `:summaries' keys.  The
normal form uses empty lists for omitted keys and carries no frame or provider
identity.  Core binds labels and derives provenance only during preparation."
  (unless (e-context-lifetime--keyword-plist-p arguments)
    (signal 'e-context-lifetime-invalid-record
            (list 'curation-arguments :not-keyword-plist arguments)))
  (let ((keys nil)
        (tail arguments))
    (while tail
      (let ((key (pop tail)))
        (pop tail)
        (when (member key keys)
          (signal 'e-context-lifetime-invalid-record
                  (list 'curation-effect :duplicate-key key)))
        (unless (memq key '(:keep :summaries))
          (signal 'e-context-lifetime-invalid-record
                  (list 'curation-effect :unknown-key key)))
        (push key keys))))
  (let* ((keep (e-context-lifetime--curation-labels
                (if (plist-member arguments :keep)
                    (plist-get arguments :keep)
                  nil)
                'curation-keep))
         (raw-summaries (e-context-lifetime--curation-sequence
                         (if (plist-member arguments :summaries)
                             (plist-get arguments :summaries)
                           nil)
                         'curation-summaries))
         summaries
         seen)
    (dolist (label keep)
      (when (member label seen)
        (signal 'e-context-lifetime-invalid-record
                (list 'curation :duplicate-label label)))
      (push label seen))
    (dolist (summary raw-summaries)
      (e-context-lifetime--validate-exact-plist
       summary '(:sources :text) 'curation-summary)
      (let ((sources
             (e-context-lifetime--curation-labels
              (plist-get summary :sources) 'curation-summary-sources t))
            (text (plist-get summary :text)))
        (unless (and (stringp text) (not (string-empty-p text)))
          (signal 'e-context-lifetime-invalid-record
                  (list 'curation-summary :non-empty-text text)))
        (dolist (label sources)
          (when (member label seen)
            (signal 'e-context-lifetime-invalid-record
                    (list 'curation :duplicate-label label)))
          (push label seen))
        (push (list :sources sources
                    :text (copy-sequence text))
              summaries)))
    (unless seen
      (signal 'e-context-lifetime-invalid-record
              (list 'curation :no-disposition)))
    (when (> (length seen) e-context-lifetime-curation-max-sources)
      (signal 'e-context-lifetime-invalid-record
              (list 'curation :source-count (length seen))))
    (list :keep keep :summaries (nreverse summaries))))

(defun e-context-lifetime--curation-source-for-label (sources label)
  "Return trusted SOURCE from SOURCES matching positive local LABEL."
  (let ((source (nth (1- label) sources)))
    (unless (and source (= (plist-get source :label) label))
      (signal 'e-context-lifetime-invalid-record
              (list 'curation :unknown-label label)))
    source))

(defun e-context-lifetime--curation-item-provenance (sources)
  "Return core-derived provenance fields for SOURCE descriptors in SOURCES."
  (list :source-observation-ids
        (mapcar (lambda (source)
                  (e-context-lifetime--detached-copy
                   (plist-get source :source-observation-id)))
                sources)
        :source-refs
        (mapcar (lambda (source)
                  (e-context-lifetime--detached-copy
                   (plist-get source :source-entry-ref)))
                sources)
        :source-fingerprints
        (mapcar (lambda (source)
                  (e-context-lifetime--detached-copy
                   (plist-get source :source-fingerprint)))
                sources)))

(defun e-context-lifetime--curation-entry-id
    (frame normalized response-entry-id)
  "Return deterministic prepared curation ENTRY ID.
FRAME, NORMALIZED, and RESPONSE-ENTRY-ID supply its identity inputs."
  (format "curation:%s"
          (substring
           (secure-hash
            'sha256
            (prin1-to-string
             (list (e-context-lifetime-frame-id frame)
                   (e-context-lifetime-frame-generation-id frame)
                   (e-context-lifetime-frame-consumer-request-id frame)
                   response-entry-id normalized)))
           0 32)))

(defun e-context-lifetime--curation-record
    (frame normalized response-entry-id sources)
  "Build a version-3 prepared curation record before byte validation.
FRAME and SOURCES are bound using NORMALIZED and RESPONSE-ENTRY-ID."
  (let (items)
    ;; Exact items retain keep argument order.
    (dolist (label (plist-get normalized :keep))
      (let* ((source (e-context-lifetime--curation-source-for-label
                      sources label))
             (provenance
              (e-context-lifetime--curation-item-provenance (list source))))
        (push
         (append (list :kind 'exact
                       :value (e-context-lifetime--detached-copy
                               (plist-get source :value)))
                 provenance)
         items)))
    (setq items (nreverse items))
    ;; Summary items retain summary submission order and source order.
    (dolist (summary (plist-get normalized :summaries))
      (let* ((summary-sources
              (mapcar (lambda (label)
                        (e-context-lifetime--curation-source-for-label
                         sources label))
                      (plist-get summary :sources)))
             (provenance
              (e-context-lifetime--curation-item-provenance summary-sources)))
        (setq items
              (append items
                      (list
                       (append
                        (list :kind 'summary
                              :text (copy-sequence
                                     (plist-get summary :text)))
                        provenance))))))
    (list :record-version e-context-lifetime-curation-record-version
          :type 'context-promotion
          :id (e-context-lifetime--curation-entry-id
               frame normalized response-entry-id)
          :frame-id (e-context-lifetime--detached-copy
                     (e-context-lifetime-frame-id frame))
          :generation-id (e-context-lifetime--detached-copy
                          (e-context-lifetime-frame-generation-id frame))
          :consumer-request-id
          (e-context-lifetime--detached-copy
           (e-context-lifetime-frame-consumer-request-id frame))
          :response-entry-id (e-context-lifetime--detached-copy
                              response-entry-id)
          :items items)))

(defun e-context-lifetime-prepare-curation
    (frame arguments response-entry-id &optional bytes-per-token)
  "Prepare strict curation ARGUMENTS against live FRAME.

RESPONSE-ENTRY-ID is the runtime response binding.  The returned record is a
pure version-3 `context-promotion' shape ready for a later session codec.  No
frame/session mutation occurs here; exact values and provenance are detached
before the complete canonical record is measured against the 8,192-byte bound."
  (unless (e-context-lifetime-frame-p frame)
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-frame-p frame)))
  (when (e-context-lifetime-frame-consumed-p frame)
    (signal 'e-context-lifetime-invalid-record
            (list 'curation :frame-not-live
                  (e-context-lifetime-frame-id frame))))
  (let* ((response-entry-id
          (e-context-lifetime--require-id response-entry-id
                                           'response-entry))
         (normalized
          (e-context-lifetime-normalize-curation-arguments arguments))
         (sources
          (e-context-lifetime-frame-curation-sources
           frame bytes-per-token))
         (record
          (e-context-lifetime--curation-record
           frame normalized response-entry-id sources))
         (bytes (e-context-lifetime--bytes record)))
    (when (> bytes e-context-lifetime-curation-max-record-bytes)
      (signal 'e-context-lifetime-invalid-record
              (list 'curation :bytes bytes)))
    record))

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

(defun e-context-lifetime-promotion-from-record (record)
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
  (let* ((source-observation-ids
          (e-context-lifetime--id-list
           (plist-get record :source-observation-ids)
           'source-observation))
         (source-refs
          (e-context-lifetime--reference-list
           (plist-get record :source-refs) 'source-ref))
         (source-fingerprints
          (e-context-lifetime--reference-list
           (plist-get record :source-fingerprints)
           'source-fingerprint)))
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
    ;; This private struct is a decoded compatibility value, never a writer
    ;; input.  Keep the old shape available to read-only projections.
    (e-context-lifetime-promotion--create
     :id (e-context-lifetime--require-id (plist-get record :id) 'promotion)
     :frame-id (e-context-lifetime--require-id
                (plist-get record :frame-id) 'promotion)
     :generation-id (e-context-lifetime--require-id
                     (plist-get record :generation-id)
                     'promotion-generation)
     :consumer-request-id
     (e-context-lifetime--require-id
      (plist-get record :consumer-request-id)
      'promotion-consumer-request)
     :response-entry-id
     (e-context-lifetime--require-id
      (plist-get record :response-entry-id) 'response-entry)
     :facts (e-context-lifetime--validate-facts
             (plist-get record :facts))
     :source-observation-ids source-observation-ids
     :source-refs source-refs
     :source-fingerprints source-fingerprints)))

(defun e-context-lifetime--curation-record-items (items)
  "Return ITEMS as a non-empty strict curation item sequence."
  (let ((items
         (cond
          ((vectorp items) (append items nil))
          ((and (proper-list-p items)
                (not (e-context-lifetime--keyword-plist-p items)))
           items)
          (t
           (signal 'e-context-lifetime-invalid-record
                   (list 'curation-record :items items)))))
        result)
    (unless items
      (signal 'e-context-lifetime-invalid-record
              (list 'curation-record :empty-items)))
    (dolist (item items (nreverse result))
      (unless (e-context-lifetime--keyword-plist-p item)
        (signal 'e-context-lifetime-invalid-record
                (list 'curation-item :not-keyword-plist item)))
      (push item result))))

(defun e-context-lifetime--curation-item-kind (kind)
  "Return canonical curation item KIND or signal a shape error."
  (let ((kind (cond
               ((eq kind 'exact) 'exact)
               ((equal kind "exact") 'exact)
               ((eq kind 'summary) 'summary)
               ((equal kind "summary") 'summary))))
    (unless (memq kind '(exact summary))
      (signal 'e-context-lifetime-invalid-record
              (list 'curation-item :kind kind)))
    kind))

(defun e-context-lifetime--curation-item-provenance-record
    (item kind)
  "Return normalized provenance for curation ITEM of KIND.

The source arrays are the durable audit boundary.  Exact items require one
source, summaries require one or more, and all arrays must remain parallel.
Observation IDs are checked for uniqueness within the item here; the record
decoder applies the curation-wide uniqueness check."
  (let* ((source-observation-ids
          (e-context-lifetime--id-list
           (plist-get item :source-observation-ids)
           'curation-source-observation))
         (source-refs
          (e-context-lifetime--reference-list
           (plist-get item :source-refs) 'curation-source-ref))
         (source-fingerprints
          (e-context-lifetime--reference-list
           (plist-get item :source-fingerprints)
           'curation-source-fingerprint))
         (count (length source-observation-ids)))
    (unless (if (eq kind 'exact) (= count 1) (> count 0))
      (signal 'e-context-lifetime-invalid-record
              (list 'curation-item :source-count kind count)))
    (unless (and (= count (length source-refs))
                 (= count (length source-fingerprints)))
      (signal 'e-context-lifetime-invalid-record
              (list 'curation-item :parallel-provenance
                    source-observation-ids source-refs source-fingerprints)))
    (list :source-observation-ids
          (mapcar #'e-context-lifetime--detached-copy source-observation-ids)
          :source-refs
          (mapcar #'e-context-lifetime--detached-copy source-refs)
          :source-fingerprints
          (mapcar #'e-context-lifetime--detached-copy source-fingerprints))))

(defun e-context-lifetime--curation-item-from-record (item)
  "Return one canonical strict v3 curation ITEM."
  (let ((kind (e-context-lifetime--curation-item-kind
               (plist-get item :kind))))
    (if (eq kind 'exact)
        (progn
          (e-context-lifetime--validate-exact-plist
           item '(:kind :value :source-observation-ids :source-refs
                  :source-fingerprints)
           'curation-exact-item)
          (append
           (list :kind 'exact
                 :value
                 (e-context-lifetime--detached-canonical
                  (plist-get item :value)))
           (e-context-lifetime--curation-item-provenance-record item kind)))
      (e-context-lifetime--validate-exact-plist
       item '(:kind :text :source-observation-ids :source-refs
              :source-fingerprints)
       'curation-summary-item)
      (let ((text (plist-get item :text)))
        (unless (and (stringp text) (not (string-empty-p text)))
          (signal 'e-context-lifetime-invalid-record
                  (list 'curation-summary-item :non-empty-text text)))
        (append
         (list :kind 'summary :text (copy-sequence text))
         (e-context-lifetime--curation-item-provenance-record item kind))))))

(defun e-context-lifetime--curation-record-from-record (record)
  "Return a detached canonical version-3 curation RECORD."
  (e-context-lifetime--validate-exact-plist
   record
   '(:record-version :type :id :frame-id :generation-id
     :consumer-request-id :response-entry-id :items)
   'curation-record)
  (unless (and (equal (plist-get record :record-version)
                      e-context-lifetime-curation-record-version)
               (eq (plist-get record :type) 'context-promotion))
    (signal 'e-context-lifetime-invalid-record
            (list 'curation-record :version-or-type record)))
  (let (items source-observation-ids)
    (dolist (item (e-context-lifetime--curation-record-items
                   (plist-get record :items)))
      (let* ((normalized (e-context-lifetime--curation-item-from-record item))
             (item-source-ids
              (plist-get normalized :source-observation-ids)))
        (dolist (source-id item-source-ids)
          (when (member source-id source-observation-ids)
            (signal 'e-context-lifetime-invalid-record
                    (list 'curation-record :duplicate-source-observation
                          source-id)))
          (push source-id source-observation-ids))
        (push normalized items)))
    (when (> (length source-observation-ids)
             e-context-lifetime-curation-max-sources)
      (signal 'e-context-lifetime-invalid-record
              (list 'curation-record :source-count
                    (length source-observation-ids))))
    (let ((normalized
           (list :record-version e-context-lifetime-curation-record-version
                 :type 'context-promotion
                 :id (e-context-lifetime--detached-copy
                      (e-context-lifetime--require-id
                       (plist-get record :id) 'curation-id))
                 :frame-id (e-context-lifetime--detached-copy
                            (e-context-lifetime--require-id
                             (plist-get record :frame-id) 'curation-frame))
                 :generation-id (e-context-lifetime--detached-copy
                                 (e-context-lifetime--require-id
                                  (plist-get record :generation-id)
                                  'curation-generation))
                 :consumer-request-id
                 (e-context-lifetime--detached-copy
                  (e-context-lifetime--require-id
                   (plist-get record :consumer-request-id)
                   'curation-consumer))
                 :response-entry-id
                 (e-context-lifetime--detached-copy
                  (e-context-lifetime--require-id
                   (plist-get record :response-entry-id)
                   'curation-response))
                 :items (nreverse items))))
      (let ((bytes (e-context-lifetime--bytes normalized)))
        (when (> bytes e-context-lifetime-curation-max-record-bytes)
          (signal 'e-context-lifetime-invalid-record
                  (list 'curation-record :bytes bytes))))
      normalized)))

(defun e-context-lifetime-curation-from-record (record)
  "Decode and strictly canonicalize a version-3 curation RECORD.

The returned plist is detached and contains only the durable
`context-promotion' fields.  It accepts JSON-decoded vectors and string item
kinds, but does not coerce missing, extra, or malformed fields."
  (e-context-lifetime--curation-record-from-record record))

(defun e-context-lifetime-curation-record (record)
  "Encode prepared version-3 curation RECORD in canonical durable form.

The prepared record is revalidated at this codec boundary so a session cannot
append a v3 value that it would be unable to replay."
  (e-context-lifetime-curation-from-record record))

(defun e-context-lifetime-curation-messages (record)
  "Return one portable system message per v3 curation item in RECORD order.

Exact items retain their canonical source value and summary items retain their
model-authored text.  No label, estimate, provenance, identifier, prefix, or
`prin1' conversion is introduced."
  (mapcar
   (lambda (item)
     (e-context-lifetime--portable-message
      (list :role 'system
            :content (if (eq (plist-get item :kind) 'exact)
                         (plist-get item :value)
                       (plist-get item :text)))))
   (plist-get (e-context-lifetime-curation-from-record record) :items)))

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

(defun e-context-lifetime-promotion-fact-messages (promotions)
  "Return portable model messages for durable PROMOTIONS.

The selected bounded fact is the durable semantic value.  Source observation
bodies and provenance metadata remain outside the model-facing message; the
session record retains provenance separately for audit and validation."
  (cl-loop for promotion in promotions
           append
           (cl-loop for fact in (e-context-lifetime-promotion-facts promotion)
                    collect
                    (list :role 'system
                          :content
                          (format "Promoted fact %s: %s"
                                  (plist-get fact :id)
                                  (let ((value (plist-get fact :value)))
                                    (if (stringp value)
                                        value
                                      (prin1-to-string value))))))))

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
