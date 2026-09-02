;;; e-session-codec.el --- Pure durable session value codec -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module is deliberately stateless.  It maps semantic session values to
;; the existing JSONL spelling and decodes one physical record back into a
;; detached semantic value.  It does not know about stores, files, replay
;; transactions, or the session aggregate.  The application service owns
;; applying the result of `e-session-codec-replay-record'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'e-context-lifetime)

(define-error 'e-session-codec-error "Session durable value error")

(defconst e-session-codec-json-null
  (make-symbol "e-session-codec-json-null")
  "Sentinel used by physical readers to preserve JSON null presence.

An empty JSON object is represented by the Lisp value nil when parsed as a
plist, so index readers use this distinct value for JSON null until the
semantic projection boundary can classify the two shapes.  It is never
written to a durable file or returned by ordinary record decoding.")

(defun e-session-codec-json-null-p (value)
  "Return non-nil when VALUE is the physical JSON-null sentinel."
  (eq value e-session-codec-json-null))

(defun e-session-codec-index-value-from-json (value)
  "Return detached index VALUE with physical JSON nulls mapped to nil.

The index reader keeps JSON null distinct until the caller has had a chance to
classify fields whose empty-object shape is meaningful.  This operation is
the pure recursive value mapping for every other index field; it preserves an
empty plist as nil and never mutates its input."
  (cond
   ((e-session-codec-json-null-p value) nil)
   ((vectorp value)
    (vconcat (mapcar #'e-session-codec-index-value-from-json
                     (append value nil))))
   ((e-session-codec--keyword-plist-p value)
    (let (result)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (setq result
                (append result
                        (list key
                              (e-session-codec-index-value-from-json item))))))
      result))
   ((proper-list-p value)
    (mapcar #'e-session-codec-index-value-from-json value))
   (t value)))

(defconst e-session-codec--routing-attributes-tag
  "e-routing-attributes-v1"
  "Tag used to preserve Lisp selector attribute types across JSON.")

(defun e-session-codec--keyword-plist-p (value)
  "Return non-nil when VALUE is a proper keyword plist."
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

(defun e-session-codec--copy-value (value)
  "Detach JSON-shaped VALUE, preserving its list/vector structure."
  ;; Use an explicit post-order walk so durable values remain safe to detach
  ;; even when a caller supplies a deeply nested (but finite) selector.
  (let ((pending (list (list :value value)))
        (results nil)
        (visiting (make-hash-table :test 'eq)))
    (while pending
      (let ((task (pop pending)))
        (pcase (car task)
          (:leave
           (remhash (cadr task) visiting))
          (:assemble-vector
           (let (items)
             (dotimes (_ (cadr task))
               (push (pop results) items))
             (push (vconcat items) results)))
          (:assemble-cons
           (let ((cdr-value (pop results))
                 (car-value (pop results)))
             (push (cons car-value cdr-value) results)))
          (:value
           (let ((current (cadr task)))
             (cond
              ((stringp current)
               (push (copy-sequence current) results))
              ((or (null current) (eq current t) (numberp current)
                   (symbolp current))
               (push current results))
              ((or (vectorp current) (consp current))
               (when (gethash current visiting)
                 (signal 'e-session-codec-error
                         (list "Cyclic durable value" current)))
               (puthash current t visiting)
               (push (list :leave current) pending)
               (if (vectorp current)
                   (progn
                     (push (list :assemble-vector (length current)) pending)
                     (let ((index (1- (length current))))
                       (while (>= index 0)
                         (push (list :value (aref current index)) pending)
                         (setq index (1- index)))))
                 (push (list :assemble-cons) pending)
                 (push (list :value (cdr current)) pending)
                 (push (list :value (car current)) pending)))
              (t
               (push current results))))))))
    (car results)))

(defun e-session-codec--wire-attribute-value (value)
  "Return reversible JSON-shaped VALUE for selector attributes."
  ;; Keep this traversal iterative.  Routing attributes are user-controlled
  ;; durable data, and a deeply nested but valid selector must not exhaust the
  ;; evaluator merely while crossing the JSON boundary.
  (let ((pending (list (list :value value)))
        (results nil)
        (visiting (make-hash-table :test 'eq)))
    (while pending
      (let ((task (pop pending)))
        (pcase (car task)
          (:leave
           (remhash (cadr task) visiting))
          (:plist-pair
           (push (vector (cadr task) (pop results)) results))
          (:assemble
           (let ((kind (cadr task))
                 (count (caddr task))
                 items)
             (dotimes (_ count)
               (push (pop results) items))
             (setq items (vconcat items))
             (push (pcase kind
                     ('vector (vector "vector" items))
                     ('list (vector "list" items))
                     ('plist (vector "plist" items))
                     ('cons (vector "cons" items)))
                   results)))
          (:value
           (let ((current (cadr task)))
             (cond
              ((or (null current) (eq current t) (numberp current))
               (push current results))
              ((stringp current)
               (push (copy-sequence current) results))
              ((symbolp current)
               (push (vector "symbol" (symbol-name current)) results))
              ((or (vectorp current) (consp current))
               (when (gethash current visiting)
                 (signal 'e-session-codec-error
                         (list "Cyclic selector attribute value" current)))
               (puthash current t visiting)
               (push (list :leave current) pending)
               (cond
                ((vectorp current)
                 (push (list :assemble 'vector (length current)) pending)
                 (let ((index (1- (length current))))
                   (while (>= index 0)
                     (push (list :value (aref current index)) pending)
                     (setq index (1- index)))))
                ((e-session-codec--keyword-plist-p current)
                 (let ((tail current)
                       pairs)
                   (while tail
                     (push (cons (pop tail) (pop tail)) pairs))
                   (push (list :assemble 'plist (length pairs)) pending)
                   (dolist (pair pairs)
                     (push (list :plist-pair (car pair)) pending)
                     (push (list :value (cdr pair)) pending))))
                ((proper-list-p current)
                 (push (list :assemble 'list (length current)) pending)
                 (dolist (item (reverse current))
                   (push (list :value item) pending)))
                (t
                 (push (list :assemble 'cons 2) pending)
                 (push (list :value (cdr current)) pending)
                 (push (list :value (car current)) pending))))
              (t
               (signal 'e-session-codec-error
                       (list "Unsupported selector attribute value" current)))))))))
    (car results)))

(defun e-session-codec--encoded-attribute-p (value)
  "Return non-nil when VALUE has the durable attribute tag."
  (let ((items (cond ((vectorp value) (append value nil))
                     ((proper-list-p value) value))))
    (and (= (length items) 2)
         (equal (car items) e-session-codec--routing-attributes-tag))))

(defun e-session-codec--unwire-attribute-value (value)
  "Decode one reversible selector attribute VALUE."
  (cond
   ((or (null value) (eq value t) (numberp value) (stringp value))
    (e-session-codec--copy-value value))
   ((and (proper-list-p value) (= (length value) 2)
         (stringp (car value)))
    (pcase (car value)
      ("symbol"
       (unless (stringp (cadr value))
         (signal 'e-session-codec-error (list "Invalid selector symbol" value)))
       (intern (cadr value)))
      ("vector"
       (vconcat (mapcar #'e-session-codec--unwire-attribute-value
                        (append (cadr value) nil))))
      ("list"
       (mapcar #'e-session-codec--unwire-attribute-value
               (append (cadr value) nil)))
      ("cons"
       (let ((items (append (cadr value) nil)))
         (unless (= (length items) 2)
           (signal 'e-session-codec-error (list "Invalid selector cons" value)))
         (cons (e-session-codec--unwire-attribute-value (car items))
               (e-session-codec--unwire-attribute-value (cadr items)))))
      ("plist"
       (let (result)
         (dolist (pair (append (cadr value) nil))
           (let ((items (append pair nil)))
             ;; JSON arrays carry plist keys as strings.  The original
             ;; selector codec intentionally restored those keys to keyword
             ;; symbols before exposing the policy to the domain owner.
             (when (and (= (length items) 2) (stringp (car items)))
               (setcar items
                       (intern (concat ":"
                                       (string-remove-prefix ":"
                                                             (car items))))))
             (unless (and (= (length items) 2) (keywordp (car items)))
               (signal 'e-session-codec-error
                       (list "Invalid selector plist pair" pair)))
             (setq result
                   (plist-put result (car items)
                              (e-session-codec--unwire-attribute-value
                               (cadr items))))))
         result))
      (_
       (signal 'e-session-codec-error
               (list "Unknown selector attribute tag" (car value))))))
   ((vectorp value)
    (vconcat (mapcar #'e-session-codec--unwire-attribute-value value)))
   ((consp value)
    (cons (e-session-codec--unwire-attribute-value (car value))
          (e-session-codec--unwire-attribute-value (cdr value))))
   (t
    (signal 'e-session-codec-error
            (list "Unsupported encoded selector value" value)))))

(defun e-session-codec--board-routing-selector-for-json (selector)
  "Return SELECTOR with its attribute values reversibly encoded."
  (let ((result (e-session-codec--copy-value selector)))
    (when (and (e-session-codec--keyword-plist-p result)
               (plist-member result :attributes)
               (not (e-session-codec--encoded-attribute-p
                     (plist-get result :attributes))))
      (plist-put result :attributes
                 (vector e-session-codec--routing-attributes-tag
                         (e-session-codec--wire-attribute-value
                          (plist-get result :attributes)))))
    result))

(defun e-session-codec--board-routing-selector-from-json (selector)
  "Return detached SELECTOR after decoding persisted attributes."
  (if (e-session-codec--keyword-plist-p selector)
      (let ((result (e-session-codec--copy-value selector)))
        (when (and (plist-member result :attributes)
                   (e-session-codec--encoded-attribute-p
                    (plist-get result :attributes)))
          (let ((items (append (plist-get result :attributes) nil)))
            (plist-put result :attributes
                       (e-session-codec--unwire-attribute-value
                        (cadr items)))))
        result)
    selector))

(defun e-session-codec-board-routing-policy-for-json (policy)
  "Return POLICY suitable for JSON persistence."
  (when policy
    (let ((result (e-session-codec--copy-value policy)))
      (dolist (key '(:pickup-selector :observer-selector))
        (when (plist-member result key)
          (plist-put result key
                     (e-session-codec--board-routing-selector-for-json
                      (plist-get result key)))))
      result)))

(defun e-session-codec--board-routing-policy-from-json (policy)
  "Return detached POLICY after decoding persisted attributes."
  (when policy
    (if (e-session-codec--keyword-plist-p policy)
        (let ((result (e-session-codec--copy-value policy)))
          (dolist (key '(:pickup-selector :observer-selector))
            (when (plist-member result key)
              (plist-put result key
                         (e-session-codec--board-routing-selector-from-json
                          (plist-get result key)))))
          result)
      policy)))

(defun e-session-codec-board-association-for-json (association)
  "Return ASSOCIATION with routing attributes encoded for JSON."
  (when association
    (let ((result (e-session-codec--copy-value association)))
      (when (plist-member result :routing-policy)
        (plist-put result :routing-policy
                   (e-session-codec-board-routing-policy-for-json
                    (plist-get result :routing-policy))))
      result)))

(defun e-session-codec-board-association-from-json (association)
  "Return detached ASSOCIATION after decoding persisted attributes."
  (when association
    (let ((association (e-session-codec-index-value-from-json association)))
      (if (e-session-codec--keyword-plist-p association)
          (let ((result (e-session-codec--copy-value association)))
          (when (plist-member result :routing-policy)
            (plist-put result :routing-policy
                       (e-session-codec--board-routing-policy-from-json
                        (plist-get result :routing-policy))))
            result)
        association))))

(defun e-session-codec--context-value-for-json (value)
  "Return context VALUE with semantic sequences encoded as JSON arrays."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'e-session-codec--context-value-for-json
                     (append value nil))))
   ((e-session-codec--keyword-plist-p value)
    (let (result)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (setq result
                (append result
                        (list key
                              (e-session-codec--context-value-for-json item))))))
      result))
   ((consp value)
    (vconcat (mapcar #'e-session-codec--context-value-for-json value)))
   (t value)))

(defun e-session-codec--context-record-for-json (record)
  "Return provider-neutral context RECORD safe for JSON persistence."
  (e-session-codec--context-value-for-json record))

(defun e-session-codec--record-value-for-json (key value)
  "Encode RECORD VALUE at semantic KEY."
  (pcase key
    (:board-state (e-session-codec-board-association-for-json value))
    ((or :context-record :promotion :erasure)
     (and value (e-session-codec--context-record-for-json value)))
    (_ (e-session-codec--copy-value value))))

(defun e-session-codec-record-for-json (record)
  "Return detached semantic RECORD in the existing JSONL representation."
  (if (not (e-session-codec--keyword-plist-p record))
      record
    ;; Copy the complete value before replacing nested fields.  The storage
    ;; queue may retain this record until a later timer/flush, while callers
    ;; are allowed to mutate the detached value returned by the facade.
    (let ((copy (e-session-codec--copy-value record))
          (tail record))
      (while tail
        (let ((key (pop tail)))
          (when (consp tail)
            (let ((value (pop tail)))
              (plist-put copy key (e-session-codec--record-value-for-json
                                   key value))))))
      copy)))

(defun e-session-codec-index-entry-for-json (entry)
  "Return catalog ENTRY with board routing attributes encoded."
  (e-session-codec-record-for-json entry))

(defun e-session-codec-record-for-entry (session-id entry &optional parent-id)
  "Return the durable record for semantic ENTRY in SESSION-ID.

This is a pure entry-to-wire mapping used by the application composition root
when persisting one newly committed aggregate entry.  PARENT-ID is supplied by
checkpoint projection when an entry is reparented; normal live writes use the
entry's own parent."
  (let ((timestamp (plist-get entry :created-at))
        (id (plist-get entry :id))
        (parent-id (or parent-id (plist-get entry :parent-id))))
    (pcase (plist-get entry :type)
      ('message
       (let ((message (copy-tree entry)))
         (cl-remf message :durability-state)
         (plist-put message :parent-id parent-id)
         (list :type "message" :session-id session-id :timestamp timestamp
               :id id :parent-id parent-id :message message)))
      ('activity-event
       (append (list :type "activity-event" :session-id session-id
                     :id id :parent-id parent-id
                     :turn-id (plist-get entry :turn-id)
                     :board-activity-sequence
                     (plist-get entry :board-activity-sequence)
                     :timestamp timestamp
                     :event-type (plist-get entry :event-type)
                     :payload (copy-tree (plist-get entry :payload)))
               (when (plist-get entry :checkpoint-retain)
                 (list :checkpoint-retain t))))
      ('branch-summary
       (list :type "branch-summary" :session-id session-id :id id
             :parent-id parent-id :timestamp timestamp
             :branch-id (plist-get entry :branch-id)
             :summary (plist-get entry :summary)
             :metadata (copy-tree (plist-get entry :metadata))))
      ('compaction
       (list :type "compaction" :session-id session-id :id id
             :parent-id parent-id :timestamp timestamp
             :summary (plist-get entry :summary)
             :branch-id (plist-get entry :branch-id)
             :range (copy-tree (plist-get entry :range))
             :first-kept-entry-id (plist-get entry :first-kept-entry-id)
             :tokens-before (plist-get entry :tokens-before)
             :tokens-kept (plist-get entry :tokens-kept)
             :metadata (copy-tree (plist-get entry :metadata))))
      ('provider-anchor
       (list :type "provider-anchor" :session-id session-id :id id
             :parent-id parent-id :timestamp timestamp
             :provider-id (plist-get entry :provider-id)
             :model (plist-get entry :model)
             :covered-entry-id (plist-get entry :covered-entry-id)
             ;; Current SQLite framing has a canonical tagged value codec and
             ;; therefore preserves list/vector identity directly.  The
             ;; JSONL-only array coercion remains isolated in the legacy JSON
             ;; mapping used by offline migration.
             :fingerprints (copy-tree (plist-get entry :fingerprints))
             :metadata (copy-tree (plist-get entry :metadata))))
      ('process-report
       (let ((report (copy-tree entry)))
         (cl-remf report :durability-state)
         (plist-put report :parent-id parent-id)
         (list :type "process-report" :session-id session-id :id id
               :parent-id parent-id :timestamp timestamp :report report)))
      ((or 'context-generation 'context-promotion)
       (list :type (symbol-name (plist-get entry :type))
             :session-id session-id :id id :parent-id parent-id
             :timestamp timestamp :context-record
             (copy-tree (plist-get entry :context-record))))
      ('context-curation-package
       (list :type "context-curation-package" :session-id session-id :id id
             :parent-id parent-id :timestamp timestamp
             :promotion (copy-tree (plist-get entry :promotion))
             :erasure (copy-tree (plist-get entry :erasure))))
      ('session-event
       (pcase (plist-get entry :event-type)
         ('current-branch
          (list :type "current-branch" :session-id session-id :id id
                :parent-id parent-id :timestamp timestamp
                :branch-id (plist-get entry :branch-id)))
         ('messages-cleared
          (list :type "messages-cleared" :session-id session-id :id id
                :parent-id parent-id :timestamp timestamp))
         (_
          (append (list :type "session-info" :session-id session-id :id id
                        :parent-id parent-id :timestamp timestamp)
                  (when (plist-member entry :name)
                    (list :name (plist-get entry :name)))
                  (when (plist-member entry :metadata)
                    (list :metadata (copy-tree (plist-get entry :metadata))))
                  (when (plist-member entry :turn-options)
                    (list :turn-options
                          (copy-tree (plist-get entry :turn-options))))))))
      (_ (signal 'e-session-codec-error
                 (list "Unsupported semantic entry" (plist-get entry :type)))))))

(defun e-session-codec-json-read-line (line)
  "Parse one JSONL LINE as a plist without touching session state."
  (json-parse-string line
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(defun e-session-codec--known-role (role)
  "Return ROLE normalized for semantic transcript values."
  (if (stringp role) (intern role) role))

(defun e-session-codec--known-display (display)
  "Return DISPLAY normalized for semantic transcript values."
  (if (stringp display) (intern display) display))

(defun e-session-codec--normalize-message (message)
  "Return detached MESSAGE normalized after JSON replay."
  (let ((message (copy-sequence message)))
    (plist-put message :role
               (e-session-codec--known-role (plist-get message :role)))
    (when-let ((origin (plist-get message :origin)))
      (when (stringp origin)
        (plist-put message :origin (intern origin))))
    (when (plist-member message :display)
      (plist-put message :display
                 (e-session-codec--known-display
                  (plist-get message :display))))
    message))

(defun e-session-codec--known-event-type (event-type)
  "Return EVENT-TYPE normalized for semantic activity values."
  (if (stringp event-type) (intern event-type) event-type))

(defun e-session-codec--known-provider-id (provider-id)
  "Return PROVIDER-ID normalized for semantic anchor values."
  (if (stringp provider-id) (intern provider-id) provider-id))

(defun e-session-codec--normalize-activity-event (event)
  "Return detached EVENT normalized after JSON replay."
  (let ((event (copy-sequence event)))
    (plist-put event :event-type
               (e-session-codec--known-event-type
                (plist-get event :event-type)))
    (when (eq (plist-get event :event-type) 'hook-audit)
      (let ((payload (copy-sequence (plist-get event :payload))))
        (dolist (key '(:owner :outcome :truth-status))
          (when-let ((value (plist-get payload key)))
            (when (stringp value)
              (plist-put payload key (intern value)))))
        (plist-put event :payload payload)))
    event))

(defun e-session-codec--normalize-board-message (message)
  "Return durable board MESSAGE normalized after JSON replay."
  (let ((message (copy-sequence message)))
    (dolist (field '(:kind :mode :activity-kind :routing-state
                     :unrouted-reason :record-type :outcome :failure-policy))
      (when-let ((value (plist-get message field)))
        (when (stringp value)
          (plist-put message field (intern value)))))
    (when (plist-member message :tags)
      (plist-put message :tags
                 (mapcar (lambda (tag) (if (stringp tag) (intern tag) tag))
                         (plist-get message :tags))))
    (when-let ((attributes (plist-get message :attributes)))
      (when-let ((status (plist-get attributes :status)))
        (when (stringp status)
          (plist-put attributes :status (intern status)))))
    message))

(defun e-session-codec--message-with-created-at (message timestamp)
  "Return normalized MESSAGE with TIMESTAMP when creation time is missing."
  (let ((normalized (e-session-codec--normalize-message message)))
    (unless (plist-member normalized :created-at)
      (plist-put normalized :created-at timestamp))
    normalized))

(defun e-session-codec--context-record-has-duplicate-key-p (record)
  "Return non-nil when keyword RECORD repeats a field."
  (when (e-session-codec--keyword-plist-p record)
    (let ((tail record)
          seen
          duplicate)
      (while tail
        (let ((key (pop tail)))
          (pop tail)
          (when (memq key seen) (setq duplicate t))
          (push key seen)))
      duplicate)))

(defun e-session-codec--normalize-context-record-for-replay (type record)
  "Restore semantic TYPE on decoded context RECORD."
  (let ((copy (e-session-codec--copy-value record)))
    (when (and (e-session-codec--keyword-plist-p copy)
               (stringp (plist-get copy :type))
               (equal (plist-get copy :type) (symbol-name type)))
      (plist-put copy :type type))
    copy))

(defun e-session-codec--normalize-context-record
    (type record &optional expected-record-version read-legacy-p)
  "Decode and canonicalize context RECORD without applying it anywhere."
  (when (e-session-codec--context-record-has-duplicate-key-p record)
    (signal 'e-session-codec-error
            (list "Context lifetime record has duplicate fields" type)))
  (condition-case error
      (let* ((record-version (and (e-session-codec--keyword-plist-p record)
                                  (plist-get record :record-version)))
             (decoded
              (cond
               ((eq type 'context-generation)
                (e-context-lifetime-generation-from-record record))
               ((eq type 'context-erasure)
                (e-context-lifetime-curation-erasure-from-record record))
               ((equal record-version
                       e-context-lifetime-curation-record-version)
                (e-context-lifetime-curation-from-record record))
               ((and read-legacy-p
                     (equal record-version e-context-lifetime-record-version))
                (e-context-lifetime-promotion-from-record record))
               (t
                (signal 'e-session-codec-error
                        (list "Unsupported context record version"
                              record-version)))))
             (normalized
              (cond
               ((eq type 'context-generation)
                (e-context-lifetime-generation-record decoded))
               ((eq type 'context-erasure)
                (e-context-lifetime-curation-erasure-record decoded))
               ((equal record-version
                       e-context-lifetime-curation-record-version)
                (e-context-lifetime-curation-record decoded))
               (read-legacy-p (e-session-codec--copy-value record))
               (t
                (signal 'e-session-codec-error
                        (list "Unsupported context record version"
                              record-version))))))
        (when (and expected-record-version
                   (not (equal expected-record-version
                               (plist-get normalized :record-version))))
          (signal 'e-session-codec-error
                  (list "Context record version is not accepted"
                        expected-record-version
                        (plist-get normalized :record-version))))
        normalized)
    (e-context-lifetime-invalid-record
     (signal 'e-session-codec-error
             (list "Invalid context lifetime record" type error)))))

(defun e-session-codec--decode-record-value (key value)
  "Decode one physical RECORD VALUE at semantic KEY."
  (pcase key
    (:board-state (e-session-codec-board-association-from-json value))
    ((or :context-record :promotion :erasure)
     (and value
          (e-session-codec--context-value-from-json value)))
    (_ value)))

(defun e-session-codec--context-value-from-json (value)
  "Detach context VALUE after JSON parsing.
JSON arrays are intentionally left as lists: the semantic context lifetime
reader accepts either list or vector sequences."
  (cond
   ((vectorp value)
    (mapcar #'e-session-codec--context-value-from-json (append value nil)))
   ((consp value)
    (if (e-session-codec--keyword-plist-p value)
        (let ((copy (copy-sequence value))
              (tail value))
          (while tail
            (let ((key (pop tail)))
              (when (consp tail)
                (plist-put copy key
                           (e-session-codec--context-value-from-json
                            (pop tail))))))
          copy)
      (mapcar #'e-session-codec--context-value-from-json value)))
   (t value)))

(defun e-session-codec-decode-record (record)
  "Return detached semantic RECORD decoded from its JSONL spelling."
  (unless (e-session-codec--keyword-plist-p record)
    (signal 'e-session-codec-error (list "Record is not a plist" record)))
  (let ((copy (e-session-codec--copy-value record))
        (tail record))
    (while tail
      (let ((key (pop tail)))
        (when (consp tail)
          (let ((value (pop tail)))
            (plist-put copy key
                       (e-session-codec--decode-record-value key value))))))
    (pcase (plist-get copy :type)
      ("message"
       (plist-put copy :message
                  (e-session-codec--message-with-created-at
                   (plist-get copy :message)
                   (plist-get copy :timestamp))))
      ("board-message"
       (plist-put copy :message
                  (e-session-codec--normalize-board-message
                   (plist-get copy :message))))
      ("activity-event"
       (let ((event (e-session-codec--normalize-activity-event
                     (list :id (plist-get copy :id)
                           :parent-id (plist-get copy :parent-id)
                           :turn-id (plist-get copy :turn-id)
                           :event-type (plist-get copy :event-type)
                           :payload (plist-get copy :payload)
                           :created-at (plist-get copy :timestamp)))))
         (dolist (key '(:checkpoint-retain :board-activity-sequence))
           (when (plist-member copy key)
             (plist-put event key (plist-get copy key))))
         (plist-put copy :semantic-event event)))
      ("provider-anchor"
       (plist-put copy :provider-id
                  (e-session-codec--known-provider-id
                   (plist-get copy :provider-id))))
      ((or "context-generation" "context-promotion")
       (plist-put copy :context-record
                  (e-session-codec--normalize-context-record-for-replay
                   (intern (plist-get copy :type))
                   (plist-get copy :context-record))))
      ("context-curation-package"
       (dolist (key '(:promotion :erasure))
         (when (plist-member copy key)
           (plist-put copy key
                      (and (plist-get copy key)
                           (e-session-codec--normalize-context-record-for-replay
                            (if (eq key :promotion)
                                'context-promotion
                              'context-erasure)
                            (plist-get copy key)))))))
      ("session-info"
       (when (plist-member copy :turn-options)
         (plist-put copy :turn-options
                    (copy-tree (plist-get copy :turn-options)))))
      ("board-session-state"
       (when (plist-member copy :board-state)
         (plist-put copy :board-state
                    (e-session-codec-board-association-from-json
                     (plist-get copy :board-state))))))
    copy))

(defun e-session-codec-replay-record (record)
  "Decode RECORD into semantic data for the application replay owner.
Despite the historical name, this function never mutates an aggregate or
looks up a store."
  (e-session-codec-decode-record record))

(provide 'e-session-codec)

;;; e-session-codec.el ends here
