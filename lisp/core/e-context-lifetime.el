;;; e-context-lifetime.el --- Generational context lifetime model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral records and pure projections for Feature 88.  This module
;; deliberately does not know about provider request fields.  It can therefore
;; be used to compare the proposed lifetime projection with the existing
;; request projection while the feature remains opt-in.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function e-session-context-lifetime-durability-barrier
                  "e-session")

(define-error 'e-context-lifetime-error "Context lifetime error")
(define-error 'e-context-lifetime-invalid-record
  "Invalid context lifetime record"
  'e-context-lifetime-error)
(define-error 'e-context-lifetime-settlement-unavailable
  "Context lifetime settlement acknowledgement is unavailable"
  'e-context-lifetime-error)
(define-error 'e-context-lifetime-invalid-transition
  "Invalid context lifetime transition"
  'e-context-lifetime-error)

(defgroup e-context-lifetime nil
  "Generational context lifetime projection."
  :group 'e)

(defcustom e-context-lifetime-shadow-projection-enabled nil
  "When non-nil, callers may opt into the Feature 88 shadow projection.

The default is nil so existing request construction and provider behavior do
not change while the lifetime records are being introduced and compared."
  :type 'boolean
  :group 'e-context-lifetime)

(defcustom e-context-lifetime-settlement-acknowledgement-enabled nil
  "When non-nil, settlement helpers wait for asynchronous persistence acks.

The normal runtime remains unchanged until the feature is enabled by a caller.
Persistent stores without an asynchronous controller fail visibly instead of
falling back to synchronous filesystem work."
  :type 'boolean
  :group 'e-context-lifetime)

(defconst e-context-lifetime-record-version 1
  "Version of provider-neutral context lifetime records.")

(defconst e-context-lifetime-frame-states
  '(open consuming consumed settled aborted)
  "Supported observation frame states.")

(defconst e-context-lifetime-settlement-statuses
  '(acknowledged settled failed aborted)
  "Supported durable settlement marker statuses.")

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
  "Immutable semantic baseline and durable tail for one generation."
  id checkpoint durable-tail)

(cl-defstruct (e-context-lifetime-frame
               (:constructor e-context-lifetime-frame--create)
               (:conc-name e-context-lifetime-frame-))
  "One bounded observation frontier for one reasoning transition."
  id generation-id observations state source-fingerprints observation-ids
  consumption-attempt-ids consuming-response-ids promotion-ids
  terminal-settlement)

(cl-defstruct (e-context-lifetime-promotion
               (:constructor e-context-lifetime-promotion--create)
               (:conc-name e-context-lifetime-promotion-))
  "Small durable facts selected from one consumed observation frame."
  id frame-id facts source-observation-ids)

(defun e-context-lifetime--copy (value)
  "Return a detached copy of VALUE suitable for a semantic record."
  (copy-tree value))

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

Semantic values use keyword plists and sequences.  Plist keys remain keywords
so consumers can use ordinary `plist-get'; symbol values become their stable
wire spelling; vectors and lists become one canonical list representation; and
plist keys are sorted by spelling.  This is the representation used both for
projection and for persisted lifetime payloads, so a JSON round trip cannot
change its equality or fingerprint merely by turning symbols into strings."
  (cond
   ((null value) nil)
   ;; Preserve JSON's boolean true rather than confusing it with a symbolic
   ;; semantic value.  `nil' was handled above.
   ((eq value t) t)
   ;; `json-parse-string' uses this sentinel for a JSON false.  It is a
   ;; semantic boolean, not a symbol that should become the string
   ;; ":json-false" during canonicalization.
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

(defun e-context-lifetime--canonical-id (id kind)
  "Return canonical ID after validating it for KIND."
  (let ((id (e-context-lifetime--require-id id kind)))
    (if (symbolp id) (symbol-name id) id)))

(defun e-context-lifetime--require-id (id kind)
  "Validate and return ID for record KIND."
  (unless (and id
               (or (and (stringp id) (not (string-empty-p id)))
                   (and (symbolp id)
                        (not (string-empty-p (symbol-name id))))
                   (numberp id)))
    (signal 'e-context-lifetime-invalid-record
            (list kind :id id)))
  id)

(defun e-context-lifetime--normalize-state (state)
  "Return STATE as a supported frame state, defaulting to `open'."
  (setq state (or state 'open))
  (unless (memq state e-context-lifetime-frame-states)
    (signal 'e-context-lifetime-invalid-record
            (list 'frame :state state)))
  state)

(cl-defun e-context-lifetime-generation-create
    (&key id checkpoint durable-tail)
  "Create a detached generation record with ID, CHECKPOINT and DURABLE-TAIL.

CHECKPOINT and DURABLE-TAIL are provider-neutral semantic values.  Their
wire-level rendering is owned by a backend adapter in a later slice."
  (e-context-lifetime-generation--create
   :id (e-context-lifetime--canonical-id id 'generation)
   :checkpoint (e-context-lifetime-canonicalize checkpoint)
   :durable-tail (e-context-lifetime-canonicalize durable-tail)))

(cl-defun e-context-lifetime-frame-create
    (&key id generation-id observations state source-fingerprints observation-ids
          consumption-attempt-ids consuming-response-ids promotion-ids
          terminal-settlement)
  "Create a detached observation FRAME for GENERATION-ID.
OBSERVATIONS are visible only while the frame is open or being consumed."
  (e-context-lifetime-frame--create
   :id (e-context-lifetime--canonical-id id 'frame)
   :generation-id (e-context-lifetime--canonical-id generation-id 'frame)
   :observations (e-context-lifetime-canonicalize observations)
   :state (e-context-lifetime--normalize-state state)
   :source-fingerprints
   (e-context-lifetime-canonicalize source-fingerprints)
   :observation-ids (e-context-lifetime-canonicalize observation-ids)
   :consumption-attempt-ids
   (e-context-lifetime-canonicalize consumption-attempt-ids)
   :consuming-response-ids
   (e-context-lifetime-canonicalize consuming-response-ids)
   :promotion-ids (e-context-lifetime-canonicalize promotion-ids)
   :terminal-settlement
   (e-context-lifetime-canonicalize terminal-settlement)))

(cl-defun e-context-lifetime-promotion-create
    (&key id frame-id facts source-observation-ids)
  "Create a detached durable PROMOTION from FRAME-ID.
FACTS are intentionally caller-selected; this constructor never copies a
frame's observations into the durable tail."
  (e-context-lifetime-promotion--create
   :id (e-context-lifetime--canonical-id id 'promotion)
   :frame-id (e-context-lifetime--canonical-id frame-id 'promotion)
   :facts (e-context-lifetime-canonicalize facts)
   :source-observation-ids
   (e-context-lifetime-canonicalize source-observation-ids)))

(defun e-context-lifetime-generation-copy (generation)
  "Return a detached copy of GENERATION."
  (unless (e-context-lifetime-generation-p generation)
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-generation-p generation)))
  (e-context-lifetime-generation-create
   :id (e-context-lifetime-generation-id generation)
   :checkpoint (e-context-lifetime-generation-checkpoint generation)
   :durable-tail (e-context-lifetime-generation-durable-tail generation)))

(defun e-context-lifetime-frame-copy (frame)
  "Return a detached copy of FRAME."
  (unless (e-context-lifetime-frame-p frame)
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-frame-p frame)))
  (e-context-lifetime-frame-create
   :id (e-context-lifetime-frame-id frame)
   :generation-id (e-context-lifetime-frame-generation-id frame)
   :observations (e-context-lifetime-frame-observations frame)
   :state (e-context-lifetime-frame-state frame)
   :source-fingerprints
   (e-context-lifetime-frame-source-fingerprints frame)
   :observation-ids (e-context-lifetime-frame-observation-ids frame)
   :consumption-attempt-ids
   (e-context-lifetime-frame-consumption-attempt-ids frame)
   :consuming-response-ids
   (e-context-lifetime-frame-consuming-response-ids frame)
   :promotion-ids (e-context-lifetime-frame-promotion-ids frame)
   :terminal-settlement
   (e-context-lifetime-frame-terminal-settlement frame)))

(defun e-context-lifetime-promotion-copy (promotion)
  "Return a detached copy of PROMOTION."
  (unless (e-context-lifetime-promotion-p promotion)
    (signal 'wrong-type-argument
            (list 'e-context-lifetime-promotion-p promotion)))
  (e-context-lifetime-promotion-create
   :id (e-context-lifetime-promotion-id promotion)
   :frame-id (e-context-lifetime-promotion-frame-id promotion)
   :facts (e-context-lifetime-promotion-facts promotion)
   :source-observation-ids
   (e-context-lifetime-promotion-source-observation-ids promotion)))

(defun e-context-lifetime-generation-record (generation)
  "Return JSON-friendly durable RECORD for GENERATION."
  (let ((generation (e-context-lifetime-generation-copy generation)))
    (list :record-version e-context-lifetime-record-version
          :type 'context-generation
          :id (e-context-lifetime-generation-id generation)
          :checkpoint (e-context-lifetime-generation-checkpoint generation)
          :durable-tail
          (e-context-lifetime-generation-durable-tail generation))))

(defun e-context-lifetime-frame-record (frame)
  "Return JSON-friendly durable RECORD for FRAME."
  (let ((frame (e-context-lifetime-frame-copy frame)))
    (list :record-version e-context-lifetime-record-version
          :type 'context-frame
          :id (e-context-lifetime-frame-id frame)
          :generation-id (e-context-lifetime-frame-generation-id frame)
          :observations (e-context-lifetime-frame-observations frame)
          :state (e-context-lifetime-frame-state frame)
          :source-fingerprints
          (e-context-lifetime-frame-source-fingerprints frame)
          :observation-ids
          (e-context-lifetime-frame-observation-ids frame)
          :consumption-attempt-ids
          (e-context-lifetime-frame-consumption-attempt-ids frame)
          :consuming-response-ids
          (e-context-lifetime-frame-consuming-response-ids frame)
          :promotion-ids (e-context-lifetime-frame-promotion-ids frame)
          :terminal-settlement
          (e-context-lifetime-frame-terminal-settlement frame))))

(defun e-context-lifetime-promotion-record (promotion)
  "Return JSON-friendly durable RECORD for PROMOTION."
  (let ((promotion (e-context-lifetime-promotion-copy promotion)))
    (list :record-version e-context-lifetime-record-version
          :type 'context-promotion
          :id (e-context-lifetime-promotion-id promotion)
          :frame-id (e-context-lifetime-promotion-frame-id promotion)
          :facts (e-context-lifetime-promotion-facts promotion)
          :source-observation-ids
          (e-context-lifetime-promotion-source-observation-ids promotion))))

(defun e-context-lifetime-generation-from-record (record)
  "Decode provider-neutral GENERATION RECORD into a detached value."
  (unless (and (listp record)
               (eq (plist-get record :type) 'context-generation))
    (signal 'e-context-lifetime-invalid-record (list 'generation record)))
  (e-context-lifetime-generation-create
   :id (plist-get record :id)
   :checkpoint (plist-get record :checkpoint)
   :durable-tail (plist-get record :durable-tail)))

(defun e-context-lifetime-frame-from-record (record)
  "Decode provider-neutral FRAME RECORD into a detached value."
  (unless (and (listp record)
               (eq (plist-get record :type) 'context-frame))
    (signal 'e-context-lifetime-invalid-record (list 'frame record)))
  (e-context-lifetime-frame-create
   :id (plist-get record :id)
   :generation-id (plist-get record :generation-id)
   :observations (plist-get record :observations)
   :state (plist-get record :state)
   :source-fingerprints (plist-get record :source-fingerprints)
   :observation-ids (plist-get record :observation-ids)
   :consumption-attempt-ids (plist-get record :consumption-attempt-ids)
   :consuming-response-ids (plist-get record :consuming-response-ids)
   :promotion-ids (plist-get record :promotion-ids)
   :terminal-settlement (plist-get record :terminal-settlement)))

(defun e-context-lifetime-promotion-from-record (record)
  "Decode provider-neutral PROMOTION RECORD into a detached value."
  (unless (and (listp record)
               (eq (plist-get record :type) 'context-promotion))
    (signal 'e-context-lifetime-invalid-record (list 'promotion record)))
  (e-context-lifetime-promotion-create
   :id (plist-get record :id)
   :frame-id (plist-get record :frame-id)
   :facts (plist-get record :facts)
   :source-observation-ids (plist-get record :source-observation-ids)))

(defun e-context-lifetime-frame-visible-p (frame)
  "Return non-nil when FRAME observations belong in the next projection."
  (and (e-context-lifetime-frame-p frame)
       (memq (e-context-lifetime-frame-state frame) '(open consuming))))

(defun e-context-lifetime--transition-frame (frame target allowed)
  "Return FRAME transitioned to TARGET when its state is in ALLOWED.

The frame value is immutable from a caller's perspective: this helper copies
the record before changing its state.  Durable replay may construct any valid
snapshot directly, but semantic transitions cannot move a terminal frame or
move it backwards in the lifecycle."
  (unless (e-context-lifetime-frame-p frame)
    (signal 'e-context-lifetime-invalid-record
            (list 'frame-transition frame target)))
  (let ((state (e-context-lifetime-frame-state frame)))
    (unless (memq state allowed)
      (signal 'e-context-lifetime-invalid-transition
              (list (e-context-lifetime-frame-id frame) state target)))
    (let ((copy (e-context-lifetime-frame-copy frame)))
      (setf (e-context-lifetime-frame-state copy) target)
      copy)))

(defun e-context-lifetime-frame-start-consuming (frame)
  "Return FRAME transitioned from open to consuming.

This is the durable-attempt boundary: callers should record the resulting
snapshot before dispatching the provider request."
  (e-context-lifetime--transition-frame frame 'consuming '(open)))

(defun e-context-lifetime-frame-consume (frame)
  "Return FRAME transitioned to consumed from open or consuming.

The open-to-consumed form is an intentional shorthand for a caller that has
already durably represented the attempt elsewhere."
  (e-context-lifetime--transition-frame frame 'consumed '(open consuming)))

(defun e-context-lifetime-frame-settle (frame)
  "Return FRAME transitioned from consumed to settled."
  (e-context-lifetime--transition-frame frame 'settled '(consumed)))

(defun e-context-lifetime-frame-abort (frame)
  "Return FRAME transitioned to aborted from open or consuming.

An abort is an explicit terminal decision for an observation that could not
be consumed or retried safely; a consumed frame instead requires settlement."
  (e-context-lifetime--transition-frame frame 'aborted '(open consuming)))

(defun e-context-lifetime-generation-append-durable
    (generation durable-value)
  "Return GENERATION with DURABLE-VALUE appended to its durable tail."
  (let ((copy (e-context-lifetime-generation-copy generation)))
    (setf (e-context-lifetime-generation-durable-tail copy)
          (append (e-context-lifetime-generation-durable-tail copy)
                  (list (e-context-lifetime--copy durable-value))))
    copy))

(defun e-context-lifetime-apply-promotion (generation promotion)
  "Return GENERATION with selected PROMOTION facts appended durably.
The promotion's source observations are provenance only and are never copied."
  (let ((result (e-context-lifetime-generation-copy generation)))
    (dolist (fact (e-context-lifetime-promotion-facts promotion))
      (setq result
            (e-context-lifetime-generation-append-durable result fact)))
    result))

(defun e-context-lifetime--items (value)
  "Return VALUE as a detached sequence of semantic context items."
  (cond
   ((null value) nil)
   ((vectorp value) (mapcar #'e-context-lifetime-canonicalize (append value nil)))
   ;; A message plist is one item, not a sequence of alternating plist cells.
   ((e-context-lifetime--keyword-plist-p value)
    (list (e-context-lifetime-canonicalize value)))
   ((listp value) (mapcar #'e-context-lifetime-canonicalize value))
   (t (list (e-context-lifetime-canonicalize value)))))

(defun e-context-lifetime--segment (kind id messages)
  "Return a detached semantic segment for KIND, ID and MESSAGES."
  (list :kind kind
        :id id
        :messages (e-context-lifetime--items messages)
        :fingerprint
        (secure-hash 'sha256
                     (prin1-to-string
                      (e-context-lifetime--items messages)))))

(cl-defun e-context-lifetime-project
    (generation &optional frame &key static-prefix stable-context)
  "Purely project GENERATION and optional FRAME into model context.

STATIC-PREFIX and STABLE-CONTEXT are request-time values.  They are included in
the returned request projection but never copied into GENERATION.  FRAME
observations are included only while FRAME is open or consuming; settled and
consumed observations therefore disappear from the next shadow projection."
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
         (durable-tail (e-context-lifetime-generation-durable-tail generation))
         (observations (when (and frame
                                  (e-context-lifetime-frame-visible-p frame))
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
          :checkpoint (e-context-lifetime-canonicalize checkpoint)
          :durable-tail (e-context-lifetime-canonicalize durable-tail)
          :ephemeral (e-context-lifetime-canonicalize observations)
          :segments segments
          :messages messages
          :fingerprint (secure-hash 'sha256 (prin1-to-string segments)))))

(defalias 'e-context-lifetime-shadow-project #'e-context-lifetime-project)

(defun e-context-lifetime-shadow-enabled-p ()
  "Return non-nil when shadow projection is enabled for callers."
  e-context-lifetime-shadow-projection-enabled)

(cl-defun e-context-lifetime-shadow-context
    (legacy-context generation &optional frame &key static-prefix stable-context)
  "Return shadow context when enabled, otherwise unchanged LEGACY-CONTEXT.

This explicit boundary keeps Slice 1 observational: no existing request path is
altered until a caller opts in by binding or setting the feature flag."
  (if (e-context-lifetime-shadow-enabled-p)
      (e-context-lifetime-project
       generation frame :static-prefix static-prefix
       :stable-context stable-context)
    legacy-context))

(defun e-context-lifetime--character-estimate (value)
  "Return a bounded character estimate for VALUE without serializing its body."
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

(defun e-context-lifetime--emit-diagnostic (event payload)
  "Emit bounded diagnostic EVENT with PAYLOAD when hooks are installed."
  (run-hook-with-args 'e-context-lifetime-diagnostics-hook event payload)
  payload)

(cl-defun e-context-lifetime-acknowledge-settlement
    (store session-id &key frame-id record-count record-bytes prefix-entry-ids
           enabled on-done on-error)
  "Asynchronously acknowledge STORE's current settlement boundary.

When ENABLED is nil and
`e-context-lifetime-settlement-acknowledgement-enabled' is nil, the helper
reports a disabled status and performs no persistence work.  For an in-memory
store the boundary is already local and is acknowledged immediately.
For a persistent store, only its asynchronous persistence controller is used;
the helper never introduces synchronous filesystem I/O into the request path.
ON-DONE receives a bounded status plist, and ON-ERROR receives an Emacs
condition list."
  (let* ((enabled (or enabled
                      e-context-lifetime-settlement-acknowledgement-enabled))
         (base (list :frame-id frame-id
                     :prefix-entry-ids (copy-sequence prefix-entry-ids)
                     :record-count (or record-count 0)
                     :record-bytes (or record-bytes 0)
                     :started-at (float-time))))
    (if (not enabled)
        (let ((result (append base (list :status 'disabled))))
          (e-context-lifetime--emit-diagnostic
           'settlement-acknowledgement-skipped
           (e-context-lifetime--copy
            (cl-loop for (key value) on result by #'cddr
                     when (memq key '(:frame-id :prefix-entry-ids :record-count
                                           :record-bytes :status))
                     append (list key value))))
          (when on-done (funcall on-done result))
          result)
      (cl-labels
          ((done (status &optional error)
             (let ((result (append base
                                   (list :status status
                                         :elapsed-seconds
                                         (max 0.0 (- (float-time)
                                                     (plist-get base
                                                                :started-at)))))))
               (when error
                 (setq result (append result (list :error error))))
               (e-context-lifetime--emit-diagnostic
                (if (eq status 'acknowledged)
                    'settlement-prefix-acknowledged
                  'settlement-prefix-acknowledgement-failed)
                (e-context-lifetime--copy
                 (cl-loop for (key value) on result by #'cddr
                          when (memq key '(:frame-id :prefix-entry-ids :record-count
                                           :record-bytes :status :elapsed-seconds))
                          append (list key value))))
               (if (eq status 'acknowledged)
                   (when on-done (funcall on-done result))
                 (when on-error (funcall on-error error)))
               result)))
        (condition-case error
            (e-session-context-lifetime-durability-barrier
             store session-id prefix-entry-ids
             (lambda (_value) (done 'acknowledged))
             (lambda (failure) (done 'failed failure)))
          (e-context-lifetime-settlement-unavailable
           (done 'unavailable error))
          (e-session-persistence-unavailable
           (done 'unavailable
                 (list 'e-context-lifetime-settlement-unavailable
                       session-id))))))))

(provide 'e-context-lifetime)

;;; e-context-lifetime.el ends here
