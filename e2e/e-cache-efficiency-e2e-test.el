;;; e-cache-efficiency-e2e-test.el --- Growing cache acceptance -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Exercises the configured ordinary harness with substantial synthetic tools.
;; Captures request hashes and usage, never credentials or prompt content.

;;; Code:

(load (expand-file-name "e-live-e2e-test.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defun e-cache-efficiency-e2e--static-fingerprint (body options)
  "Hash BODY's stable guidance and tools, excluding conversation history."
  (secure-hash
   'sha256
   (prin1-to-string
    (list (plist-get body :instructions)
          (cl-loop for segment in (plist-get options :segments)
                   when (memq (plist-get segment :kind)
                              '(static-prefix stable-context))
                   collect (plist-get segment :messages))
          (plist-get body :tools)))))

(defun e-cache-efficiency-e2e--canonical-json-value (value)
  "Return JSON VALUE with object keys sorted recursively."
  (cond
   ((stringp value) value)
   ((and (listp value)
         (or (null value) (keywordp (car value))))
    (let (members)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (push (cons key item) members)))
      (apply #'append
             (mapcar
              (lambda (member)
                (list (car member)
                      (e-cache-efficiency-e2e--canonical-json-value
                       (cdr member))))
              (sort members
                    (lambda (left right)
                      (string< (symbol-name (car left))
                               (symbol-name (car right)))))))))
   ((and (vectorp value) (not (stringp value)))
    (vconcat
     (mapcar #'e-cache-efficiency-e2e--canonical-json-value
             (append value nil))))
   (t value)))

(defun e-cache-efficiency-e2e--input-item-hashes (input)
  "Return canonical JSON SHA-256 hashes for native Responses INPUT items."
  (mapcar
   (lambda (item)
     (secure-hash
      'sha256
      (encode-coding-string
       (e-json-serialize
        (e-cache-efficiency-e2e--canonical-json-value item))
       'utf-8)))
   (append input nil)))

(defun e-cache-efficiency-e2e--native-input-message-text (item)
  "Return plain text from native developer INPUT ITEM, or nil if not plain."
  (when (and (equal (plist-get item :type) "message")
             (equal (plist-get item :role) "developer"))
    (let ((content (append (plist-get item :content) nil)))
      (when (cl-every
             (lambda (part)
               (and (equal (plist-get part :type) "input_text")
                    (stringp (plist-get part :text))))
             content)
        (mapconcat (lambda (part) (plist-get part :text)) content "")))))

(defun e-cache-efficiency-e2e--stable-instruction-summary (texts)
  "Summarize stable instruction TEXTS without retaining their contents."
  (when (and texts
             (cl-every (lambda (text)
                         (and (stringp text) (not (string-empty-p text))))
                       texts))
    (list :character-count (apply #'+ (mapcar #'length texts))
          :sha256
          (secure-hash
           'sha256
           (encode-coding-string
            (e-json-serialize (vconcat texts))
            'utf-8)))))

(defun e-cache-efficiency-e2e--stable-native-instructions (body options)
  "Summarize stable guidance verified in BODY's native Responses channel.
Return nil unless every expected stable system message is present in order in
developer input, or the complete flattened projection is present in
`instructions'.  The summary contains a digest and size, never the text."
  (let* ((stable-messages
          (cl-loop for segment in (plist-get options :segments)
                   when (memq (plist-get segment :kind)
                              '(static-prefix stable-context))
                   append (seq-filter
                           (lambda (message)
                             (eq (plist-get message :role) 'system))
                           (plist-get segment :messages))))
         (texts (mapcar (lambda (message) (plist-get message :content))
                        stable-messages))
         (native-input (append (plist-get body :input) nil))
         (cursor 0)
         (complete t)
         (any-developer-match nil))
    (dolist (text texts)
      (let ((index
             (and (stringp text)
                  (cl-position-if
                   (lambda (item)
                     (equal (e-cache-efficiency-e2e--native-input-message-text
                             item)
                            text))
                   native-input :start cursor))))
        (if index
            (setq cursor (1+ index))
          (setq complete nil))
        (when (and (stringp text)
                   (cl-some
                    (lambda (item)
                      (equal (e-cache-efficiency-e2e--native-input-message-text
                              item)
                             text))
                    native-input))
          (setq any-developer-match t))))
    (cond
     ((and stable-messages complete)
      (e-cache-efficiency-e2e--stable-instruction-summary texts))
     ((and stable-messages
           (not any-developer-match)
           (cl-every (lambda (text)
                       (and (stringp text) (not (string-empty-p text))))
                     texts)
           (let* ((base (or (plist-get options :instructions)
                            "You are a helpful assistant."))
                  (expected-prefix
                   (concat base "\n\n" (string-join texts "\n\n")))
                  (native-instructions (plist-get body :instructions)))
             (and (stringp native-instructions)
                  (string-prefix-p expected-prefix native-instructions))))
      (e-cache-efficiency-e2e--stable-instruction-summary texts)))))

(defun e-cache-efficiency-e2e--stable-guidance-record-valid-p (record)
  "Return non-nil when RECORD meets the repeated P1 guidance guard."
  (let ((count (plist-get record :stable-instructions-character-count))
        (digest (plist-get record :stable-instructions-sha256)))
    (and (integerp count)
         (> count 30000)
         (stringp digest)
         (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" digest))))

(defun e-cache-efficiency-e2e--inherited-observation-indices (input)
  "Return indices of native INPUT items containing the fixed observation."
  (cl-loop for item in (append input nil)
           for index from 0
           when (string-match-p
                 (regexp-quote e-cache-efficiency-e2e--codex-observation)
                 (e-json-serialize
                  (e-cache-efficiency-e2e--canonical-json-value item)))
           collect index))

(defun e-cache-efficiency-e2e--input-prefix-evidence
    (previous-record current-durable-hashes)
  "Describe the unchanged durable input prefix against PREVIOUS-RECORD."
  (let* ((previous-hashes
          (and previous-record
               (append (plist-get previous-record
                                  :durable-input-item-sha256)
                       nil)))
         (current-hashes current-durable-hashes)
         (previous-count (length previous-hashes))
         (current-count (length current-hashes))
         (common-count 0))
    (while (and (< common-count previous-count)
                (< common-count current-count)
                (equal (nth common-count previous-hashes)
                       (nth common-count current-hashes)))
      (setq common-count (1+ common-count)))
    (let ((type (cond
                 ((null previous-record) 'initial)
                 ((and (= common-count previous-count)
                       (= common-count current-count)) 'stable)
                 ((= common-count previous-count) 'append)
                 ((= common-count current-count) 'truncated)
                 (t 'changed)))
          (index (and previous-record
                      (< common-count (max previous-count current-count))
                      common-count)))
      (list :previous-durable-input-item-count previous-count
            :unchanged-previous-durable-input-prefix-length common-count
            :durable-input-first-divergence-type type
            :durable-input-first-divergence-index index))))

(defun e-cache-efficiency-e2e--joined-usage-row
    (request counters status event-ordinal &optional provenance-events initial-p)
  "Return REQUEST joined with safely attributed COUNTERS and STATUS.
EVENT-ORDINAL is the provider ordinal or chronological position.  When the
join is ambiguous, retain raw PROVENANCE-EVENTS without assigning their usage
to this request."
  (let* ((matched (memq status '(ordinal positional)))
         (joined (copy-tree request))
         (counter-keys '(:input-tokens :context-input-tokens
                         :cached-input-tokens :cache-creation-input-tokens
                         :output-tokens :reasoning-output-tokens :total-tokens))
         (derived-keys (append counter-keys
                               '(:phase :usage-event-ordinal
                                 :usage-attribution-status
                                 :provider-total-input-tokens
                                 :usage-provenance
                                 :unattributed-usage-provenance-events))))
    (when matched
      (setq joined
            (append joined
                    (cl-loop for (key value) on counters by #'cddr
                             unless (or (memq key derived-keys)
                                        (plist-member joined key))
                             append (list key (copy-tree value)))
                    (list :usage-provenance (copy-tree counters)))))
    (when initial-p
      (setq joined (append joined '(:phase initial-context-baseline))))
    (setq joined
          (append joined
                  (list :usage-event-ordinal
                        (if matched event-ordinal 'unavailable)
                        :usage-attribution-status status)))
    (dolist (key counter-keys)
      (setq joined
            (append joined
                    (list key
                          (let ((value (and matched
                                            (plist-get counters key))))
                            (if (and matched (plist-member counters key)
                                     (numberp value))
                                value
                              'unavailable))))))
    (setq joined
          (append joined
                  (list :provider-total-input-tokens
                        (or (e-live-e2e--cache-total-input-tokens joined)
                            'unavailable))))
    (when provenance-events
      (setq joined
            (append joined
                    (list :unattributed-usage-provenance-events
                          (vconcat (mapcar #'copy-tree provenance-events))))))
    joined))

(defun e-cache-efficiency-e2e--unattributed-usage-rows
    (requests usage status)
  "Return REQUESTS with STATUS and raw USAGE retained as unattributed evidence."
  (let ((rows
         (cl-loop for request in requests
                  for index from 1
                  collect
                  (e-cache-efficiency-e2e--joined-usage-row
                   request nil status 'unavailable nil (= index 1)))))
    (when rows
      (setf (car rows)
            (e-cache-efficiency-e2e--joined-usage-row
             (car requests) nil status 'unavailable usage t)))
    rows))

(defun e-cache-efficiency-e2e--join-request-usage-by-id (requests usage)
  "Join REQUESTS to USAGE by unique request ID and preserved turn ordinal."
  (let ((events-by-id (make-hash-table :test #'equal))
        (usage-id-counts (make-hash-table :test #'equal))
        (request-id-counts (make-hash-table :test #'equal)))
    (dolist (event usage)
      (let ((id (plist-get event :provider-request-id)))
        (puthash id (cons event (gethash id events-by-id)) events-by-id)
        (puthash id (1+ (gethash id usage-id-counts 0)) usage-id-counts)))
    (dolist (request requests)
      (let ((id (plist-get request :provider-request-id)))
        (puthash id (1+ (gethash id request-id-counts 0)) request-id-counts)))
    (let* ((rows
            (cl-loop for request in requests
                     for request-id = (plist-get request :provider-request-id)
                     for request-ordinal =
                     (plist-get request :provider-request-ordinal)
                     for matching = (gethash request-id events-by-id)
                     for event = (car matching)
                     for event-ordinal = (and event
                                              (plist-get event
                                                         :provider-request-ordinal))
                     collect
                     (cond
                      ((> (gethash request-id request-id-counts 0) 1)
                       (e-cache-efficiency-e2e--joined-usage-row
                        request nil 'ambiguous-duplicate-provider-id 'unavailable
                        matching))
                      ((null matching)
                       (e-cache-efficiency-e2e--joined-usage-row
                        request nil 'missing-usage 'unavailable))
                      ((> (length matching) 1)
                       (e-cache-efficiency-e2e--joined-usage-row
                        request nil 'ambiguous-duplicate-provider-id 'unavailable
                        matching))
                      ((> (gethash request-id usage-id-counts 0) 1)
                       (e-cache-efficiency-e2e--joined-usage-row
                        request nil 'ambiguous-duplicate-provider-id 'unavailable
                        matching))
                      ((not (and (integerp request-ordinal)
                                 (integerp event-ordinal)))
                       (e-cache-efficiency-e2e--joined-usage-row
                        request nil 'ambiguous-provenance 'unavailable matching))
                      ((/= request-ordinal event-ordinal)
                       (e-cache-efficiency-e2e--joined-usage-row
                        request nil 'ambiguous-ordinal-mismatch 'unavailable
                        matching))
                      (t
                       (e-cache-efficiency-e2e--joined-usage-row
                        request event 'ordinal event-ordinal)))))
           (unmatched
            (cl-remove-if
             (lambda (event)
               (member (plist-get event :provider-request-id)
                       (mapcar (lambda (request)
                                 (plist-get request :provider-request-id))
                               requests)))
             usage)))
      (when (and rows unmatched)
        (let* ((first (car rows))
               (existing
                (append (plist-get first
                                   :unattributed-usage-provenance-events)
                        nil)))
          (setf (car rows)
                (plist-put
                 (copy-sequence first)
                 :unattributed-usage-provenance-events
                 (vconcat (mapcar #'copy-tree (append existing unmatched)))))))
      rows)))

(defun e-cache-efficiency-e2e--join-request-starts (requests starts)
  "Enrich chronological REQUESTS from exact provider-request STARTS.
If cardinality, IDs, or per-turn ordinals cannot prove the mapping, retain the
raw starts and mark every request ambiguous rather than shifting attribution."
  (let* ((ids (mapcar (lambda (start)
                        (plist-get start :provider-request-id))
                      starts))
         (ordinals (mapcar (lambda (start)
                             (plist-get start :provider-request-ordinal))
                           starts))
         (valid (and (= (length requests) (length starts))
                     (cl-every (lambda (id) (and (stringp id)
                                                 (not (string-empty-p id)))) ids)
                     (cl-every (lambda (ordinal)
                                 (and (integerp ordinal) (> ordinal 0)))
                               ordinals)
                     (= (length ids)
                        (length (delete-dups (copy-sequence ids)))))))
    (if valid
        (cl-loop for request in requests
                 for start in starts
                 collect
                 (append (copy-tree request)
                         (list :provider-request-id
                               (plist-get start :provider-request-id)
                               :provider-request-ordinal
                               (plist-get start :provider-request-ordinal)
                               :provider-request-start-attribution-status
                               'matched)
                         (when (plist-member start :turn-id)
                           (list :provider-request-turn-id
                                 (plist-get start :turn-id)))))
      (let ((rows (mapcar (lambda (request)
                            (append (copy-tree request)
                                    '(:provider-request-start-attribution-status
                                      ambiguous)))
                          requests)))
        (when rows
          (setf (car rows)
                (append (copy-sequence (car rows))
                        (list :unattributed-provider-request-start-events
                              (vconcat (mapcar #'copy-tree starts)))))
          rows)))))

(defun e-cache-efficiency-e2e--join-request-usage (requests usage)
  "Join chronological REQUESTS to USAGE without shifting missing events.
The first captured request is sent before any tool result, so its total input
is a conservative ceiling for stable instructions and initial context.  Usage
ordinals take precedence; positional pairing is safe only for complete legacy
evidence."
  (let* ((usage-has-ordinals
          (cl-some (lambda (event)
                     (and (listp event)
                          (plist-member event :provider-request-ordinal)))
                   usage))
         (request-ordinals
          (mapcar (lambda (request)
                    (or (plist-get request :provider-request-ordinal)
                        (plist-get request :request-ordinal)))
                  requests))
         (valid-request-ordinals
          (and request-ordinals
               (cl-every (lambda (ordinal)
                           (and (integerp ordinal) (> ordinal 0)))
                         request-ordinals)
               (= (length request-ordinals)
                  (length (delete-dups (copy-sequence request-ordinals))))))
         (valid-usage-ordinals
          (cl-every (lambda (event)
                      (let ((ordinal (and (listp event)
                                          (plist-get event
                                                     :provider-request-ordinal))))
                        (and (integerp ordinal) (> ordinal 0))))
                    usage))
         (usage-ordinals
          (mapcar (lambda (event)
                    (and (listp event)
                         (plist-get event :provider-request-ordinal)))
                  usage)))
    (cond
     ((cl-some (lambda (request)
                 (plist-member request :provider-request-id))
               requests)
      (if (and (cl-every (lambda (request)
                           (let ((id (plist-get request :provider-request-id)))
                             (and (stringp id) (not (string-empty-p id))
                                  (eq (plist-get request
                                                 :provider-request-start-attribution-status)
                                      'matched))))
                         requests)
               (cl-every (lambda (event)
                           (and (stringp (plist-get event :provider-request-id))
                                (integerp (plist-get event
                                                     :provider-request-ordinal))))
                         usage))
          (e-cache-efficiency-e2e--join-request-usage-by-id requests usage)
        (e-cache-efficiency-e2e--unattributed-usage-rows
         requests usage 'ambiguous-provenance)))
     ((not usage-has-ordinals)
      (if (= (length requests) (length usage))
          (cl-loop for request in requests
                   for counters in usage
                   for index from 1
                   collect
                   (e-cache-efficiency-e2e--joined-usage-row
                    request counters 'positional index nil (= index 1)))
        (let ((rows
               (cl-loop for request in requests
                        for index from 1
                        collect
                        (e-cache-efficiency-e2e--joined-usage-row
                         request nil 'ambiguous-positional 'unavailable nil
                         (= index 1)))))
          (when rows
            (setf (car rows)
                  (e-cache-efficiency-e2e--joined-usage-row
                   (car requests) nil 'ambiguous-positional 'unavailable
                   usage t)))
          rows)))
     ((not (and valid-request-ordinals valid-usage-ordinals))
      (let ((rows
             (cl-loop for request in requests
                      for index from 1
                      collect
                      (e-cache-efficiency-e2e--joined-usage-row
                       request nil 'ambiguous-provenance 'unavailable nil
                       (= index 1)))))
        (when rows
          (setf (car rows)
                (e-cache-efficiency-e2e--joined-usage-row
                 (car requests) nil 'ambiguous-provenance 'unavailable
                 usage t)))
        rows))
     (t
      (let ((by-ordinal (make-hash-table :test #'eql))
            (id-counts (make-hash-table :test #'equal)))
        (dolist (event usage)
          (let* ((ordinal (plist-get event :provider-request-ordinal))
                 (id (plist-get event :provider-request-id)))
            (puthash ordinal (cons event (gethash ordinal by-ordinal)) by-ordinal)
            (when (stringp id)
              (puthash id (1+ (gethash id id-counts 0)) id-counts))))
        (let* ((rows
                (cl-loop for request in requests
                         for ordinal in request-ordinals
                         for index from 1
                         for matching = (gethash ordinal by-ordinal)
                         for duplicate-id =
                         (and (= (length matching) 1)
                              (let ((id (plist-get (car matching)
                                                   :provider-request-id)))
                                (and (stringp id) (> (gethash id id-counts 0) 1))))
                         collect
                         (cond
                          ((null matching)
                           (e-cache-efficiency-e2e--joined-usage-row
                            request nil 'missing-usage 'unavailable nil
                            (= index 1)))
                          ((> (length matching) 1)
                           (e-cache-efficiency-e2e--joined-usage-row
                            request nil 'ambiguous-duplicate-ordinal 'unavailable
                            (nreverse matching) (= index 1)))
                          (duplicate-id
                           (e-cache-efficiency-e2e--joined-usage-row
                            request nil 'ambiguous-duplicate-provider-id
                            'unavailable (list (car matching)) (= index 1)))
                          (t
                           (e-cache-efficiency-e2e--joined-usage-row
                            request (car matching) 'ordinal ordinal nil
                            (= index 1))))))
               (unmatched
                (cl-remove-if
                 (lambda (event)
                   (member (plist-get event :provider-request-ordinal)
                           request-ordinals))
                 usage)))
          (when (and rows unmatched)
            (let* ((first (car rows))
                   (existing
                    (append (plist-get first
                                       :unattributed-usage-provenance-events)
                            nil)))
              (setf (car rows)
                    (plist-put
                     (copy-sequence first)
                     :unattributed-usage-provenance-events
                     (vconcat (mapcar #'copy-tree
                                      (append existing unmatched)))))))
          rows))))))

(defun e-cache-efficiency-e2e--usage-join-complete-p (evidence)
  "Return non-nil when every EVIDENCE request has an unambiguous usage join."
  (and evidence
       (cl-every (lambda (request)
                   (and (memq (plist-get request :usage-attribution-status)
                              '(ordinal positional))
                        (or (not (plist-member
                                  request
                                  :provider-request-start-attribution-status))
                            (eq (plist-get request
                                           :provider-request-start-attribution-status)
                                'matched))
                        (not (plist-member
                              request
                              :unattributed-provider-request-start-events))
                        (or (not (plist-member
                                  request
                                  :provider-request-finish-attribution-status))
                            (eq (plist-get
                                 request
                                 :provider-request-finish-attribution-status)
                                'matched))
                        (not (plist-member
                              request
                              :unattributed-provider-request-finish-events))
                        (not (plist-member request
                                          :unattributed-usage-provenance-events))))
                 evidence)))

(defun e-cache-efficiency-e2e--qualified-growing-cache-result (evidence)
  "Classify growing reuse only when EVIDENCE has complete usage attribution."
  (if (e-cache-efficiency-e2e--usage-join-complete-p evidence)
      (e-live-e2e--classify-growing-cache-evidence :evidence evidence)
    "unconfirmed-incomplete-usage"))

(defun e-cache-efficiency-e2e--with-p3-candidate-capability (identity thunk)
  "Call THUNK with a private P3 carrier for exact route IDENTITY only.
The ordinary production capability owners still resolve endpoint, model, auth,
API, transport, and requester.  This ERT-only wrapper grants no caller-carried
capability and leaves production declarations unchanged."
  (let ((openai-owner
         (symbol-function 'e-openai-profile-context-capabilities))
        (anthropic-owner
         (symbol-function 'e-anthropic-context-capabilities)))
    (cl-letf
        (((symbol-function 'e-openai-profile-context-capabilities)
          (lambda (profile options &rest route)
            (let* ((capabilities
                    (apply openai-owner profile options route))
                   (actual
                    (apply
                     #'e-openai-profile--context-capability-identity
                     profile options
                     (cl-loop for key in '(:provider :base-url :request-function)
                              when (plist-member route key)
                              append (list key (plist-get route key))))))
              (if (equal actual identity)
                  (progn
                    (setq e-cache-efficiency-e2e--p3-candidate-grant-count
                          (1+ e-cache-efficiency-e2e--p3-candidate-grant-count))
                    (plist-put (copy-sequence capabilities)
                               :reserved-effect-carrier 'context-curate-wire))
                capabilities))))
         ((symbol-function 'e-anthropic-context-capabilities)
          (lambda (provider options &rest route)
            (let* ((capabilities
                    (apply anthropic-owner provider options route))
                   (actual
                    (apply
                     #'e-anthropic--context-capability-identity
                     provider options
                     (cl-loop for key in
                              '(:base-url :model :request-function :requester)
                              when (plist-member route key)
                              append (list key (plist-get route key))))))
              (if (e-anthropic--context-capability-same-route-p
                   actual identity)
                  (progn
                    (setq e-cache-efficiency-e2e--p3-candidate-grant-count
                          (1+ e-cache-efficiency-e2e--p3-candidate-grant-count))
                    (plist-put (copy-sequence capabilities)
                               :reserved-effect-carrier 'context-curate-wire))
                capabilities)))))
      (funcall thunk))))

(defvar e-cache-efficiency-e2e--p3-candidate-grant-count 0
  "Number of exact-route curation grants made by the ERT-only wrapper.")

(defun e-cache-efficiency-e2e--p3-route-capabilities (provider options route)
  "Return P3 capability facts for effective PROVIDER, OPTIONS, and ROUTE."
  (if (assq provider e-anthropic-model-providers)
      (apply #'e-anthropic-context-capabilities provider options route)
    (apply #'e-openai-profile-context-capabilities
           (e-openai-provider-profile provider) options
           (append (list :provider provider) route))))

(defun e-cache-efficiency-e2e--p3-route-identity (provider options route)
  "Return the trusted P3 identity for PROVIDER, OPTIONS, and ROUTE."
  (if (assq provider e-anthropic-model-providers)
      (apply #'e-anthropic--context-capability-identity
             provider options route)
    (apply #'e-openai-profile--context-capability-identity
           (e-openai-provider-profile provider) options
           (append (list :provider provider) route))))

(defun e-cache-efficiency-e2e--p3-run-with-authority
    (authority identity normal-source-carrier thunk)
  "Run THUNK under the P3 test AUTHORITY.
Only the existing diagnostic authority uses the private wrapper."
  (pcase authority
    ('ert-exact-route-wrapper
     (e-cache-efficiency-e2e--with-p3-candidate-capability identity thunk))
    ('normal-source
     (unless normal-source-carrier
       (error "Normal-source route has no admitted carrier"))
     (funcall thunk))
    (_ (error "Unknown P3 test authority: %S" authority))))

(defun e-cache-efficiency-e2e--p3-exact-route-negative-p
    (provider options route identity)
  "Return non-nil when P3 wrapper denies a changed trusted route."
  (let ((result 'not-called))
    (e-cache-efficiency-e2e--with-p3-candidate-capability
     identity
     (lambda ()
       (setq result
             (plist-get
              (e-cache-efficiency-e2e--p3-route-capabilities
               provider options route)
              :reserved-effect-carrier))))
    (eq result 'none)))

(defun e-cache-efficiency-e2e--report-evidence (record)
  "Emit bounded cache RECORD using the shared E2E value conversion."
  (message "E98_CACHE_EVIDENCE %s"
           (json-encode (e-live-e2e--json-plist record))))

(defun e-cache-efficiency-e2e--report-canvas-evidence (record)
  "Emit bounded Feature 99 canvas qualification RECORD."
  (message "E99_CANVAS_EVIDENCE %s"
           (json-encode (e-live-e2e--json-plist record))))

(defun e-cache-efficiency-e2e--canvas-source-evidence (options)
  "Return bounded identity/provenance for the current Canvas in OPTIONS."
  (let* ((sources
          (cl-loop for segment in (plist-get options :segments)
                   append (seq-filter
                           (lambda (source)
                             (stringp (plist-get source :canvas-text)))
                           (plist-get segment e-context-evidence-sources-key))))
         (source (seq-find (lambda (item)
                             (stringp (plist-get item :canvas-text)))
                           sources))
         (revision (and source (plist-get source :canvas-revision))))
    (list :canvas-reference-layout-revision
          (or (plist-get options :canvas-reference-layout-revision)
              (and source e-context-canvas-reference-layout-revision))
          :canvas-source-count (length sources)
          :canvas-source-present (and source t)
          :canvas-source-uri-sha256
          (and source
               (secure-hash 'sha256 (plist-get source :uri)))
          :canvas-source-revision revision
          :canvas-source-content-sha256
          (and source (plist-get source :content-sha256))
          :canvas-source-payload-bytes
          (and source (plist-get source :canvas-payload-bytes))
          :canvas-source-revision-matches-content
          (and source
               (equal revision (plist-get source :content-sha256))))))

(defun e-cache-efficiency-e2e--canvas-operation-evidence (messages)
  "Return ordered Canvas operation kinds, hashes, and revisions from MESSAGES."
  (vconcat
   (cl-loop for message in messages
            when (e-context-canvas-reference-message-p message)
            collect
            (let* ((content (plist-get message :content))
                   (revision
                    (and (stringp content)
                         (string-match "revision=\\\"\\([[:xdigit:]]+\\)\\\""
                                       content)
                         (match-string 1 content))))
              (list :representation
                    (if (and (stringp content)
                             (string-prefix-p "<canvas-change " content))
                        'patch
                      'snapshot)
                    :revision revision
                    :content-sha256
                    (and (stringp content) (secure-hash 'sha256 content)))))))

(defun e-cache-efficiency-e2e--native-prefix-evidence (previous current)
  "Return a bounded unchanged native prefix count for PREVIOUS/CURRENT hashes."
  (let* ((previous-items
          (append (plist-get previous :native-history-item-sha256) nil))
         (current-items
          (append (plist-get current :native-history-item-sha256) nil))
         (common 0))
    (while (and (< common (length previous-items))
                (< common (length current-items))
                (equal (nth common previous-items)
                       (nth common current-items)))
      (setq common (1+ common)))
    (list :previous-native-history-item-count (length previous-items)
          :current-native-history-item-count (length current-items)
          :unchanged-previous-native-history-prefix-count common
          :unchanged-previous-native-history-prefix-sha256
          (and (> common 0)
               (secure-hash 'sha256
                            (prin1-to-string (seq-take previous-items common)))))))

(defun e-cache-efficiency-e2e--codex-growing-history-prompt ()
  "Return the ordinary Codex P2 prompt used by provider and synthetic paths."
  (concat
   "Before calling any tool, solve CROSS + ROADS = DANGER as a decimal "
   "cryptarithm with distinct digits and no leading zero. Work through the "
   "column carries and verify the equation. Keep the solution in encrypted "
   "reasoning, not in your final text. Then call cache_probe steps 1, 2, 3, "
   "and 4 in order, waiting for each result before requesting the next. After "
   "step 1 arrives, check the cryptarithm again and retain its solution in "
   "reasoning. Use all four PROBE codes. Return only OBSERVATION=<the fixed "
   "observation code> SUM=<the sum of the four codes>, and omit the "
   "cryptarithm solution. Do not call other tools."))

(defun e-cache-efficiency-e2e--assert-codex-growing-history-answer (answer)
  "Assert the semantic contract for a Codex P2 ANSWER."
  (should (string-match-p
           (regexp-quote e-cache-efficiency-e2e--codex-observation)
           answer))
  (should (string-match-p "\\b2934\\b" answer))
  (should-not (string-match-p "96233\\|62513\\|158746" answer)))

(defun e-cache-efficiency-e2e--report-codex-growing-history-p2
    (started-at requests usage semantic-result)
  "Join REQUESTS and USAGE and emit the P2 evidence record.
Return its cache classification."
  (let* ((evidence
          (e-cache-efficiency-e2e--join-request-usage requests usage))
         (cache-result
          (e-cache-efficiency-e2e--qualified-growing-cache-result evidence)))
    (e-cache-efficiency-e2e--report-evidence
     (list :scenario "codex-growing-tool-history-p2"
           :repository-revision (e-live-e2e--repository-revision)
           :timestamp (format-time-string "%FT%TZ" nil t)
           :elapsed-seconds (- (float-time) started-at)
           :semantic-result semantic-result
           :cache-result cache-result
           :usage-event-count (length usage)
           :usage-join-complete
           (e-cache-efficiency-e2e--usage-join-complete-p evidence)
           :requests (vconcat evidence)))
    cache-result))

(defun e-cache-efficiency-e2e--synthetic-p2-answer (messages)
  "Return a data-dependent P2 answer from ordinary harness MESSAGES."
  (let* ((tool-results
          (cl-remove-if-not
           (lambda (message) (eq (plist-get message :role) 'tool))
           messages))
         (step-codes
          (mapcar
           (lambda (message)
             (let* ((result (plist-get message :content))
                    (content (plist-get result :content)))
               (unless (and (stringp content)
                            (string-match
                             "PROBE-\\([0-9]+\\)-CODE=\\([0-9]+\\)"
                             content))
                 (error "Synthetic P2 backend received an invalid tool result"))
               (cons (string-to-number (match-string 1 content))
                     (string-to-number (match-string 2 content)))))
           tool-results)))
    (unless (equal (mapcar #'car step-codes) '(1 2 3 4))
      (error "Synthetic P2 backend did not receive all ordered tool results"))
    (unless (string-match-p
             (regexp-quote e-cache-efficiency-e2e--codex-observation)
             (prin1-to-string messages))
      (error "Synthetic P2 backend did not receive the inherited observation"))
    (format "OBSERVATION=%s SUM=%d"
            e-cache-efficiency-e2e--codex-observation
            (apply #'+ (mapcar #'cdr step-codes)))))

(defun e-cache-efficiency-e2e--synthetic-p2-backend (request-counter)
  "Return a normal asynchronous backend for P2 and update REQUEST-COUNTER."
  (let ((respond
         (lambda (messages options on-item)
           (ignore options)
           (let ((request-number (1+ (car request-counter))))
             (setcar request-counter request-number)
             (if (<= request-number 4)
                 (progn
                   (funcall on-item
                            (list :type 'tool-call
                                  :id (format "synthetic-p2-%d" request-number)
                                  :name "cache_probe"
                                  :arguments (list :step request-number)))
                   (funcall on-item '(:type done :reason tool-use)))
               (funcall on-item
                        (list :type 'assistant-message
                              :content
                              (e-cache-efficiency-e2e--synthetic-p2-answer
                               messages)))
               (funcall on-item '(:type done :reason stop)))))))
    (e-backend-create
     :name "synthetic ordinary P2 backend"
     :stream
     (cl-function
      (lambda (&key messages options on-item)
        (funcall respond messages options on-item)))
     :start
     (cl-function
      (lambda (&key messages options on-item on-done on-error on-request-start)
        (let ((cancelled nil) timer
              (request (e-backend-request-create
                        :cancel (lambda ()
                                  (setq cancelled t)
                                  (when (timerp timer)
                                    (cancel-timer timer))
                                  t))))
          (when on-request-start
            (funcall on-request-start request))
          (setq timer
                (run-at-time
                 0 nil
                 (lambda ()
                   (unless cancelled
                     (condition-case condition
                         (progn
                           (funcall respond messages options on-item)
                           (when on-done
                             (funcall on-done '(:status done))))
                       (error
                        (when on-error
                          (funcall on-error condition))))))))
          request))))))

(ert-deftest e-cache-efficiency-e2e-test-evidence-emits-provider-values ()
  "The reporting boundary serializes joined provider enums and missing totals."
  (let* ((identity '(:provider anthropic :endpoint "https://anthropic.test/messages"
                    :model "claude-cache-test" :stable-prefix-fingerprint "hash"))
         (usage (plist-get (e-anthropic--usage-item 12000 100 18000 nil) :usage))
         (evidence (e-cache-efficiency-e2e--join-request-usage
                    (list identity) (list usage)))
         emitted)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq emitted (apply #'format format-string args)))))
      (e-cache-efficiency-e2e--report-evidence
       (list :semantic-result "pass" :cache-result "unavailable"
             :requests (vconcat evidence))))
    (let* ((record (e-json-parse-string
                    (substring emitted (length "E98_CACHE_EVIDENCE "))))
           (request (aref (plist-get record :requests) 0)))
      (should (equal (plist-get record :semantic-result) "pass"))
      (should (equal (plist-get request :provider) "anthropic"))
      (should (equal (plist-get request :phase) "initial-context-baseline"))
      (should (= (plist-get request :input-tokens) 12000))
      (should (= (plist-get request :cached-input-tokens) 18000))
      (should (equal (plist-get request :context-input-tokens) "unavailable"))
      (should (equal (plist-get request :provider-total-input-tokens)
                     "unavailable"))
      (should (equal (plist-get request :usage-event-ordinal) 1)))

    (let* ((openai-request (plist-put (copy-sequence identity) :provider 'openai))
           (missing
            (car (e-cache-efficiency-e2e--join-request-usage
                  (list openai-request) (list '(:input-tokens 12000)))))
           (record (e-cache-efficiency-e2e--input-prefix-evidence nil nil)))
      (should (= (plist-get missing :input-tokens) 12000))
      (should (eq (plist-get missing :cached-input-tokens) 'unavailable))
      (should (eq (plist-get missing :provider-total-input-tokens) 12000))
      (should (eq (plist-get record :durable-input-first-divergence-type)
                  'initial)))))

(ert-deftest e-cache-efficiency-e2e-test-configured-anthropic-input-total ()
  "A configured provider name cannot turn fresh tokens into total context."
  (let* ((request '(:adapter anthropic :provider eng-ai-gateway-opus-5-5))
         (usage '(:input-tokens 4 :cached-input-tokens 20497
                  :cache-creation-input-tokens 65 :context-input-tokens 20566))
         (joined (car (e-cache-efficiency-e2e--join-request-usage
                       (list request) (list usage)))))
    (should (= (plist-get joined :provider-total-input-tokens) 20566))
    (should (= (e-live-e2e--cache-total-input-tokens joined) 20566))
    (cl-remf joined :context-input-tokens)
    (should (= (e-live-e2e--cache-total-input-tokens joined) 20566))))

(ert-deftest e-cache-efficiency-e2e-test-f99-canvas-evidence-is-bounded ()
  "F99 evidence retains current source identity and omits Canvas contents."
  (let* ((source (e-context-canvas-capture-create
                  :uri "buffer://f99-evidence-canvas"
                  :label "Canvas" :content "First\n[x] Item"))
         (message
          (list :role 'context
                :metadata (list e-context-canvas-reference-metadata-key t
                                e-context-canvas-chronology-metadata-key t)
                :content (e-context-canvas-snapshot-payload source)))
         (patch-message
          (list :role 'context
                :metadata (list e-context-canvas-reference-metadata-key t
                                e-context-canvas-chronology-metadata-key t)
                :content (e-context-canvas-patch-payload
                          source "previous"
                          (list :start 0 :end 0 :replacement "[x]"))))
         (options
          (list :canvas-reference-layout-revision
                e-context-canvas-reference-layout-revision
                :segments
                (list (list e-context-evidence-sources-key (list source)))))
         (evidence (e-cache-efficiency-e2e--canvas-source-evidence options))
         (operations
          (e-cache-efficiency-e2e--canvas-operation-evidence
           (list message patch-message)))
         (previous '(:native-history-item-sha256 ["a" "b"]))
         (current '(:native-history-item-sha256 ["a" "b" "c"]))
         (prefix (e-cache-efficiency-e2e--native-prefix-evidence
                  previous current)))
    (should (equal (plist-get evidence :canvas-reference-layout-revision)
                   e-context-canvas-reference-layout-revision))
    (should (equal (plist-get evidence :canvas-source-revision)
                   (plist-get source :canvas-revision)))
    (should (eq (plist-get evidence :canvas-source-revision-matches-content) t))
    (should (= (plist-get evidence :canvas-source-count) 1))
    (should (eq (plist-get evidence :canvas-source-present) t))
    (should (equal (plist-get (aref operations 0) :representation) 'snapshot))
    (should (equal (plist-get (aref operations 0) :revision)
                   (plist-get source :canvas-revision)))
    (should (equal (plist-get (aref operations 1) :representation) 'patch))
    (should (equal (plist-get (aref operations 1) :revision)
                   (plist-get source :canvas-revision)))
    (should-not (string-match-p "First" (prin1-to-string operations)))
    (should (= (plist-get prefix :unchanged-previous-native-history-prefix-count)
               2))
    (should-not (plist-member evidence :canvas-text))))

(ert-deftest e-cache-efficiency-e2e-test-f99-producers-bind-marked-layout-identity ()
  "Normal-source evidence producers bind identity to marked Canvas messages."
  (let* ((provider 'f99-synthetic-layout)
         (base-url "https://anthropic.example.test/v1")
         (model "claude-opus-5-5")
         (process-environment
          (cons "E99_SYNTHETIC_TOKEN=synthetic-only" process-environment))
         (e-anthropic-model-providers
          `((,provider :name "synthetic F99 layout route"
             :base-url ,base-url :auth bearer :auth-header authorization
             :env-key "E99_SYNTHETIC_TOKEN" :default-model ,model)))
         (source (e-context-canvas-capture-create
                  :uri "buffer://f99-producer-canvas"
                  :label "Canvas" :content "Synthetic current source"))
         (message
          (list :role 'context
                :metadata (list e-context-canvas-reference-metadata-key t
                                e-context-canvas-chronology-metadata-key t)
                :content (e-context-canvas-snapshot-payload source)))
         (options
          (list :model model :session-id "f99-producer-canvas"
                :segments
                (list (list e-context-evidence-sources-key (list source)))))
         (args (list :provider provider :base-url base-url :model model
                     :requester 'messages-http-start :messages (list message)
                     :options options))
         (f99-requests (make-hash-table :test #'equal))
         (p3-requests (make-hash-table :test #'equal)))
    (e-cache-efficiency-e2e--f99-capture-request
     #'e-anthropic--request-context f99-requests args)
    (e-cache-efficiency-e2e--capture-p3-native-request
     'anthropic #'e-anthropic--request-context p3-requests args)
    (let* ((f99-record (car (gethash "f99-producer-canvas" f99-requests)))
           (p3-record (car (gethash "f99-producer-canvas" p3-requests)))
           (p3-source (plist-get p3-record :canvas-source-evidence)))
      (should (equal (plist-get f99-record :canvas-reference-layout-revision)
                     e-context-canvas-reference-layout-revision))
      (should (equal (plist-get p3-record :canvas-reference-layout-revision)
                     e-context-canvas-reference-layout-revision))
      (should (equal (plist-get p3-source :canvas-source-revision)
                     (plist-get source :canvas-revision)))
      (should (eq (plist-get p3-source
                             :canvas-source-revision-matches-content)
                  t))
      (should-not (plist-member p3-source :canvas-text))
      (should-not (plist-member f99-record :canvas-text)))))

(defun e-cache-efficiency-e2e--tool-register (registry &optional steps)
  "Register a sequential probe with substantial reproducible output.
STEPS defaults to the original three-step public-API scenario."
  (let ((last-step (or steps 3)))
    (e-tools-register
     registry :name "cache_probe"
     :description
     (if (= last-step 3)
         "Read synthetic cache probes sequentially with steps 1, 2, and 3."
       (format "Read synthetic cache probes sequentially with steps 1 through %d."
               last-step))
     :parameters `(:type "object" :required ["step"]
                   :properties (:step (:type "integer"
                                        :enum ,(vconcat
                                                (number-sequence 1 last-step)))))
     :work
     (e-tools-cheap-work
      "e2e.cache-probe"
      (lambda (arguments)
        (let ((step (plist-get arguments :step)))
          (unless (and (integerp step) (<= 1 step last-step))
            (error "Invalid probe step"))
          (concat (format "PROBE-%d-CODE=%d\n" step (+ 731 step))
                  (mapconcat
                   (lambda (index)
                     (format "row-%d-%d synthetic cache evidence" step index))
                   (number-sequence 1 1200) "\n"))))))))

(defconst e-cache-efficiency-e2e--codex-observation
  "E98-CODEX-OBSERVATION-4107"
  "Fixed synthetic inherited observation used by the Codex P2 fixture.")

(defun e-cache-efficiency-e2e--supported-codex-profile ()
  "Return the configured first-party managed-auth Codex profile or skip."
  (e-live-e2e--require-enabled)
  (let* ((provider e-openai-default-provider)
         (profile (e-openai-provider-profile provider))
         (base-url (e-openai-provider-base-url profile))
         (wire-api (e-openai-provider-wire-api profile)))
    (unless (and (e-openai-profile-codex-session-affinity-supported-p
                  profile base-url wire-api)
                 (e-live-e2e--profile-auth-available-p profile))
      (ert-skip
       "Configured default is not an authenticated first-party Codex Responses profile."))
    profile))

(defun e-cache-efficiency-e2e--affinity-request-record (context options)
  "Return a content-bounded affinity and request-shape record for CONTEXT."
  (let* ((body (plist-get context :body-data))
         (headers (plist-get context :headers))
         (session-id (plist-get context :session-id))
         (body-key (plist-get body :prompt_cache_key))
         (header-id (cdr (assoc "session-id" headers)))
         (diagnostics
          (plist-get (plist-get context :metadata) :diagnostics))
         (stable-instructions
          (e-cache-efficiency-e2e--stable-native-instructions body options)))
    (list :provider (plist-get context :provider)
          :endpoint (plist-get context :url)
          :model (plist-get body :model)
          :session-identity-sha256
          (and (stringp session-id) (secure-hash 'sha256 session-id))
          :prompt-cache-key-sha256
          (and (stringp body-key) (secure-hash 'sha256 body-key))
          :session-header-sha256
          (and (stringp header-id) (secure-hash 'sha256 header-id))
          :session-affinity-identities-match
          (and (stringp session-id)
               (equal session-id body-key)
               (equal session-id header-id))
          :legacy-session-header-present (and (assoc "session_id" headers) t)
          :affinity-capability-evidence
          (plist-get diagnostics :prompt-cache-capability-evidence)
          :affinity-selected
          (plist-get diagnostics :codex-session-affinity-selected)
          :affinity-body-key-present
          (plist-get diagnostics :codex-session-affinity-body-key-present)
          :affinity-header-present
          (plist-get diagnostics :codex-session-affinity-header-present)
          :stable-prefix-fingerprint
          (e-cache-efficiency-e2e--static-fingerprint body options)
          :stable-instructions-character-count
          (plist-get stable-instructions :character-count)
          :stable-instructions-sha256
          (plist-get stable-instructions :sha256)
          :input-sha256 (secure-hash 'sha256
                                     (prin1-to-string
                                      (plist-get body :input)))
          :input-item-count (length (plist-get body :input))
          :input-types
          (mapcar (lambda (item) (plist-get item :type))
                  (append (plist-get body :input) nil))
          :tool-names
          (mapcar (lambda (tool) (plist-get tool :name))
                  (append (plist-get body :tools) nil))
          :encrypted-reasoning-requested
          (and (seq-contains-p
                (append (plist-get body :include) nil)
                "reasoning.encrypted_content")
               t)
          :encrypted-reasoning-replay-count
          (cl-count-if
           (lambda (item)
             (and (equal (plist-get item :type) "reasoning")
                  (plist-member item :encrypted_content)))
           (append (plist-get body :input) nil))
          :inherited-observation-present
          (and (string-match-p
                (regexp-quote e-cache-efficiency-e2e--codex-observation)
                (prin1-to-string body))
               t)
          :explicit-cache-controls-present
          (or (plist-member body :prompt_cache_options)
              (plist-member body :prompt_cache_retention)
              (seq-some
               (lambda (item)
                 (seq-some
                  (lambda (content)
                    (and (listp content)
                         (plist-member content :prompt_cache_breakpoint)))
                  (append (plist-get item :content) nil)))
               (append (plist-get body :input) nil))))))

(defun e-cache-efficiency-e2e--capture-request-context
    (native requests-by-session args)
  "Call NATIVE with ARGS and append its bounded request record by session."
  (let* ((context (apply native args))
         (session-id (plist-get context :session-id))
         (options (plist-get args :options))
         (records (gethash session-id requests-by-session))
         (previous-record (car (last records)))
         (body (plist-get context :body-data))
         (input (plist-get body :input))
         (input-hashes (e-cache-efficiency-e2e--input-item-hashes input))
         (observation-indices
          (e-cache-efficiency-e2e--inherited-observation-indices input))
         (durable-hashes
          (cl-loop for hash in input-hashes
                   for index from 0
                   unless (memq index observation-indices)
                   collect hash))
         (record (e-cache-efficiency-e2e--affinity-request-record
                  context options)))
    (setq record
          (append record
                  (list :request-ordinal (1+ (length records))
                        :input-item-sha256 (vconcat input-hashes)
                        :inherited-observation-indices
                        (vconcat observation-indices)
                        :inherited-observation-count
                        (length observation-indices)
                        :durable-input-item-sha256 (vconcat durable-hashes))
                  (e-cache-efficiency-e2e--input-prefix-evidence
                   previous-record durable-hashes)))
    (puthash session-id
             (append records (list record))
             requests-by-session)
    context))

(ert-deftest e-cache-efficiency-e2e-test-capture-tracks-canonical-session-prefixes ()
  "Capture hashes canonical input items and compares each session separately."
  (let* ((requests-by-session (make-hash-table :test 'equal))
         (stable-item '(:type "message" :role "system" :content "static"))
         (reordered-stable-item '(:content "static" :role "system"
                                  :type "message"))
         (observation
          (list :type "message" :role "system"
                :content e-cache-efficiency-e2e--codex-observation))
         (user-a '(:type "message" :role "user" :content "private question A"))
         (user-b '(:type "message" :role "user" :content "private question B"))
         (call '(:type "function_call" :call_id "call-a" :name "lookup"
                 :arguments "{}"))
         (result '(:type "function_call_output" :call_id "call-a"
                   :output "private tool output")))
    (cl-labels
        ((capture (session-id input)
           (e-cache-efficiency-e2e--capture-request-context
            (lambda (&rest args)
              (let* ((options (plist-get args :options))
                     (id (plist-get options :session-id))
                     (items (plist-get options :synthetic-input)))
                (list :session-id id :provider 'codex
                      :url "https://chatgpt.com/backend-api/codex/responses"
                      :headers `(("session-id" . ,id))
                      :metadata nil
                      :body-data (list :model "gpt-6-sol" :input items
                                       :include [] :tools []))))
            requests-by-session
            (list :options (list :session-id session-id
                                 :synthetic-input input)))))
      (capture "session-a" (vector stable-item observation user-a))
      (capture "session-b" (vector stable-item observation user-b))
      (capture "session-a"
               (vector reordered-stable-item user-a observation call result)))
    (let* ((session-a (gethash "session-a" requests-by-session))
           (session-b (gethash "session-b" requests-by-session))
           (first-a (car session-a))
           (second-a (cadr session-a))
           (first-b (car session-b)))
      (should (= (plist-get first-a :request-ordinal) 1))
      (should (= (plist-get second-a :request-ordinal) 2))
      (should (= (plist-get first-b :request-ordinal) 1))
      (should (equal (aref (plist-get first-a :input-item-sha256) 0)
                     (aref (plist-get second-a :input-item-sha256) 0)))
      (should (equal (plist-get first-a :inherited-observation-indices) [1]))
      (should (equal (plist-get second-a :inherited-observation-indices) [2]))
      (should (= (plist-get second-a :inherited-observation-count) 1))
      (should (= (plist-get second-a :previous-durable-input-item-count) 2))
      (should (= (plist-get second-a
                            :unchanged-previous-durable-input-prefix-length)
                 2))
      (should (eq (plist-get second-a
                             :durable-input-first-divergence-type)
                  'append))
      (should (= (plist-get second-a
                            :durable-input-first-divergence-index)
                 2))
      (should (eq (plist-get first-b
                             :durable-input-first-divergence-type)
                  'initial))
      (should (= (plist-get first-b :previous-durable-input-item-count) 0))
      (should-not (string-match-p "private question A\\|private tool output"
                                  (prin1-to-string second-a))))))

(defun e-cache-efficiency-e2e--codex-probe-layer ()
  "Return a normal harness layer with stable policy and one fixed observation."
  (let ((observation-provider
         (e-context-provider-create
          :name 'cache-efficiency-codex-observation
          :cache-placement 'dynamic-context
          :build
          (lambda (&rest _)
            (list (list :role 'system
                        :content
                        (concat "Fixed inherited observation: "
                                e-cache-efficiency-e2e--codex-observation)))))))
    (e-layer-create
     :id 'cache-efficiency-codex-probe
     :name "Codex cache efficiency probe"
     :capabilities
     (list
      (e-capability-create
       :id 'cache-efficiency-codex-probe
       :instructions
       (mapconcat (lambda (n) (format "stable directive %d" n))
                  (number-sequence 1 1500) "\n")
       :context-providers (list observation-provider)
       :tools (list (lambda (registry)
                      (e-cache-efficiency-e2e--tool-register registry 4))))))))

(defun e-cache-efficiency-e2e--codex-affinity-p1-layer ()
  "Return substantial stable Codex guidance without any tools or context."
  (e-layer-create
   :id 'cache-affinity-p1
   :name "Codex affinity P1 stable guidance"
   :capabilities
   (list (e-capability-create
          :id 'cache-affinity-p1
          :instructions
          (mapconcat (lambda (n) (format "stable reference rule %04d" n))
                     (number-sequence 1 1500) "\n")))))

(ert-deftest e-cache-efficiency-e2e-test-codex-p1-record-counts-native-layer-guidance ()
  "The P1 recorder measures layer guidance in native Codex developer input."
  (let* ((layer (e-cache-efficiency-e2e--codex-affinity-p1-layer))
         (capability-context
          (e-capabilities-context (e-layer-capabilities layer)))
         (user-message '(:role user :content "Reply with the fixed marker."))
         (messages
          (append (plist-get capability-context :messages)
                  (list user-message)))
         (segments
          (append (plist-get capability-context :segments)
                  (list (list :kind 'history :messages (list user-message)))))
         (session-id "review9-native-p1-session")
         (requests-by-session (make-hash-table :test 'equal))
         (native (symbol-function 'e-openai--request-context))
         (options (list :model "gpt-6-sol"
                        :session-id session-id
                        :segments segments))
         (args (list :provider 'codex
                     :auth-file "synthetic-auth-not-read"
                     :messages messages
                     :options options))
         context)
    (cl-letf (((symbol-function 'e-openai-codex-read-auth)
               (lambda (&optional _auth-file)
                 '(:tokens (:access_token "synthetic-access-token"))))
              ((symbol-function 'e-openai-codex-auth-account-id)
               (lambda (_auth) "synthetic-account-id")))
      (setq context
            (e-cache-efficiency-e2e--capture-request-context
             native requests-by-session args)))
    (let* ((record (car (gethash session-id requests-by-session)))
           (body (plist-get context :body-data))
           (input (append (plist-get body :input) nil))
           (stable-text
            (e-cache-efficiency-e2e--native-input-message-text (car input)))
           (guidance
            (e-capability-instructions
             (car (e-layer-capabilities layer)))))
      (should record)
      (should (> (plist-get record :stable-instructions-character-count)
                 30000))
      (should (= (plist-get record :stable-instructions-character-count)
                 (length guidance)))
      (should
       (equal (plist-get record :stable-instructions-sha256)
              (plist-get
               (e-cache-efficiency-e2e--stable-instruction-summary
                (list guidance))
               :sha256)))
      (should (e-cache-efficiency-e2e--stable-guidance-record-valid-p
               record))
      (should (equal stable-text guidance))
      (should (equal (plist-get body :instructions)
                     "You are a helpful assistant."))
      (should (equal (mapcar (lambda (item) (plist-get item :role)) input)
                     '("developer" "user")))
      (should (null (plist-get record :tool-names)))
      (should (eq (plist-get record :session-affinity-identities-match) t))
      (should (eq (plist-get record :affinity-body-key-present) t))
      (should (eq (plist-get record :affinity-header-present) t)))))

(ert-deftest e-cache-efficiency-e2e-test-codex-p1-records-flattened-alias-guidance ()
  "The normal managed-auth HTTP alias records stable guidance in instructions."
  (let* ((layer (e-cache-efficiency-e2e--codex-affinity-p1-layer))
         (capability-context
          (e-capabilities-context (e-layer-capabilities layer)))
         (user-message '(:role user :content "Reply with the fixed marker."))
         (messages
          (append (plist-get capability-context :messages)
                  (list user-message)))
         (segments
          (append (plist-get capability-context :segments)
                  (list (list :kind 'history :messages (list user-message)))))
         (session-id "review10-flattened-p1-session")
         (requests-by-session (make-hash-table :test 'equal))
         (profile
          (list :name "Managed Codex HTTP alias"
                :base-url (concat e-openai-codex-default-base-url "/codex")
                :wire-api 'responses
                :responses-transport 'http
                :response-store :json-false
                :requires-openai-auth t
                :include-encrypted-reasoning t
                :observation-delivery 'inherited))
         (e-openai-model-providers
          (append e-openai-model-providers
                  (list (cons 'codex-http-alias profile))))
         (native (symbol-function 'e-openai--request-context))
         (options (list :model "gpt-6-sol"
                        :session-id session-id
                        :segments segments))
         (args (list :provider 'codex-http-alias
                     :auth-file "synthetic-auth-not-read"
                     :messages messages
                     :options options))
         context)
    (cl-letf (((symbol-function 'e-openai-codex-read-auth)
               (lambda (&optional _auth-file)
                 '(:tokens (:access_token "synthetic-access-token"))))
              ((symbol-function 'e-openai-codex-auth-account-id)
               (lambda (_auth) "synthetic-account-id")))
      (setq context
            (e-cache-efficiency-e2e--capture-request-context
             native requests-by-session args)))
    (let* ((record (car (gethash session-id requests-by-session)))
           (body (plist-get context :body-data))
           (input (append (plist-get body :input) nil))
           (guidance
            (e-capability-instructions
             (car (e-layer-capabilities layer))))
           (instructions (plist-get body :instructions)))
      (should record)
      (should (eq (plist-get context :responses-transport) 'http))
      (should-not (plist-member profile :responses-context-layout))
      (should-not (plist-member options :responses-context-layout))
      (should (equal (plist-get context :url)
                     (concat e-openai-codex-default-base-url
                             "/codex/responses")))
      (should (equal (plist-get context :session-id) session-id))
      (should (equal (plist-get body :prompt_cache_key) session-id))
      (should (equal (cdr (assoc "session-id" (plist-get context :headers)))
                     session-id))
      (should (equal (cdr (assoc "Authorization" (plist-get context :headers)))
                     "Bearer synthetic-access-token"))
      (should (string-prefix-p
               (concat "You are a helpful assistant.\n\n" guidance)
               instructions))
      (should (equal (mapcar (lambda (item) (plist-get item :role)) input)
                     '("user")))
      (should (= (plist-get record :stable-instructions-character-count)
                 (length guidance)))
      (should (equal (plist-get record :stable-instructions-sha256)
                     (plist-get
                      (e-cache-efficiency-e2e--stable-instruction-summary
                       (list guidance))
                      :sha256)))
      (should (e-cache-efficiency-e2e--stable-guidance-record-valid-p record))
      (should (null (plist-get record :tool-names)))
      (should (eq (plist-get record :session-affinity-identities-match) t))
      (should (eq (plist-get record :affinity-selected) t))
      (should-not (plist-get record :legacy-session-header-present))
      (should-not (plist-get record :explicit-cache-controls-present))
      (should (eq (plist-get record :affinity-body-key-present) t))
      (should (eq (plist-get record :affinity-header-present) t))
      (let* ((base "You are a helpful assistant.")
             (absent-context
              (plist-put
               (copy-sequence context)
               :body-data
               (plist-put (copy-sequence body) :instructions base)))
             (mismatched-context
              (plist-put
               (copy-sequence context)
               :body-data
               (plist-put (copy-sequence body) :instructions
                          (concat base "\n\nnot the layer guidance"))))
             (absent-record
              (e-cache-efficiency-e2e--affinity-request-record
               absent-context options))
             (mismatched-record
              (e-cache-efficiency-e2e--affinity-request-record
               mismatched-context options)))
        (should-not (plist-get absent-record
                               :stable-instructions-character-count))
        (should-not (plist-get absent-record :stable-instructions-sha256))
        (should-not
         (e-cache-efficiency-e2e--stable-guidance-record-valid-p absent-record))
        (should-not (plist-get mismatched-record
                               :stable-instructions-character-count))
        (should-not (plist-get mismatched-record
                               :stable-instructions-sha256))
        (should-not
         (e-cache-efficiency-e2e--stable-guidance-record-valid-p
          mismatched-record))))))

(ert-deftest e-cache-efficiency-e2e-test-four-step-tool-retains-public-three-step-default ()
  "The public probe stays three-step while the Codex fixture reaches step four."
  (let ((three-step (e-tools-registry-create))
        (four-step (e-tools-registry-create)))
    (e-cache-efficiency-e2e--tool-register three-step)
    (e-cache-efficiency-e2e--tool-register four-step 4)
    (let ((public-result
           (e-tools-execute-batch
            three-step
            '(:id "public-step-3" :name "cache_probe"
              :arguments (:step 3))))
          (codex-result
           (e-tools-execute-batch
            four-step
            '(:id "codex-step-4" :name "cache_probe"
              :arguments (:step 4)))))
      (should (string-match-p "PROBE-3-CODE=734" (plist-get public-result :content)))
      (should (string-match-p "row-3-1200 synthetic cache evidence"
                              (plist-get public-result :content)))
      (should (string-match-p "PROBE-4-CODE=735" (plist-get codex-result :content)))
      (should (string-match-p "row-4-1200 synthetic cache evidence"
                              (plist-get codex-result :content)))
      (should (equal (plist-get (gethash "cache_probe"
                                         (e-tools-registry-tools three-step))
                                :description)
                     "Read synthetic cache probes sequentially with steps 1, 2, and 3."))
      (should (equal (plist-get (gethash "cache_probe"
                                         (e-tools-registry-tools four-step))
                                :description)
                     "Read synthetic cache probes sequentially with steps 1 through 4.")))))

(ert-deftest e-cache-efficiency-e2e-test-joined-initial-control-qualifies-history ()
  "The public request/usage join rejects static warming and needs two advances."
  (let* ((identity '(:provider openai :endpoint "https://openai.test/responses"
                     :model "gpt-cache-test"
                     :stable-prefix-fingerprint "stable"))
         (requests (make-list 4 identity))
         (usage '((:input-tokens 4096 :cached-input-tokens 0)
                  (:input-tokens 10000 :cached-input-tokens 4096)
                  (:input-tokens 14000 :cached-input-tokens 8192)
                  (:input-tokens 18000 :cached-input-tokens 12288)))
         (evidence
          (e-cache-efficiency-e2e--join-request-usage requests usage)))
    (should (= (length evidence) 4))
    (should (eq (plist-get (car evidence) :phase) 'initial-context-baseline))
    (should (equal (mapcar (lambda (event)
                             (plist-get event :input-tokens))
                           evidence)
                   '(4096 10000 14000 18000)))
    (should (equal (e-live-e2e--classify-growing-cache-evidence
                    :evidence evidence)
                   "growing-history-reuse"))
    (let* ((anthropic-requests
            (make-list
             4 '(:provider anthropic
                 :endpoint "https://anthropic.test/messages"
                 :model "claude-cache-test"
                 :stable-prefix-fingerprint "stable")))
           (anthropic-usage
            (mapcar (lambda (values)
                      (plist-get (apply #'e-anthropic--usage-item values) :usage))
                    '((18000 100 0 0) (30000 100 0 0)
                      (42000 100 0 0) (54000 100 0 0))))
           (cold-evidence
            (e-cache-efficiency-e2e--join-request-usage
             anthropic-requests anthropic-usage)))
      (should (equal (mapcar #'e-live-e2e--cache-total-input-tokens
                             cold-evidence)
                     '(18000 30000 42000 54000)))
      (should (equal (mapcar (lambda (event)
                               (plist-get event :cached-input-tokens))
                             cold-evidence)
                     '(0 0 0 0)))
      (should (equal (e-live-e2e--classify-growing-cache-evidence
                      :evidence cold-evidence)
                     "cold-or-inconclusive")))
    (let* ((static-usage '((:input-tokens 4096 :cached-input-tokens 0)
                           (:input-tokens 10000 :cached-input-tokens 1024)
                           (:input-tokens 14000 :cached-input-tokens 2048)))
           (static-evidence
            (e-cache-efficiency-e2e--join-request-usage
             (seq-take requests 3) static-usage)))
      (should (equal (e-live-e2e--classify-growing-cache-evidence
                      :evidence static-evidence)
                     "static-prefix-only")))
    (let* ((one-advance-usage
            '((:input-tokens 4096 :cached-input-tokens 0)
              (:input-tokens 10000 :cached-input-tokens 4096)
              (:input-tokens 14000 :cached-input-tokens 8192)
              (:input-tokens 18000 :cached-input-tokens 8192)))
           (one-advance
            (e-cache-efficiency-e2e--join-request-usage
             requests one-advance-usage)))
      (should (equal (e-live-e2e--classify-growing-cache-evidence
                      :evidence one-advance)
                     "insufficient-history-reuse")))
    (let ((missing-control (copy-tree evidence)))
      (cl-remf (car missing-control) :phase)
      (should (equal (e-live-e2e--classify-growing-cache-evidence
                      :evidence missing-control)
                     "missing-initial-control")))
    (let* ((partial
            (e-cache-efficiency-e2e--join-request-usage
             requests (butlast usage)))
           (last-event (car (last partial))))
      (should (= (length partial) 4))
      (should (cl-every (lambda (event)
                          (and (eq (plist-get event :input-tokens) 'unavailable)
                               (eq (plist-get event :cached-input-tokens)
                                   'unavailable)
                               (eq (plist-get event :usage-attribution-status)
                                   'ambiguous-positional)))
                        partial))
      (should (= (length (plist-get (car partial)
                                    :unattributed-usage-provenance-events))
                 3))
      (should (eq (plist-get last-event :input-tokens) 'unavailable))
      (should (equal (e-live-e2e--classify-growing-cache-evidence
                      :evidence partial)
                     "unavailable")))))

(ert-deftest e-cache-efficiency-e2e-test-ordinal-usage-preserves-provenance ()
  "A missing middle usage event does not shift later counters or causality."
  (let* ((requests
          '((:provider openai :endpoint "https://api.openai.com/v1/responses"
             :model "gpt-6-sol" :stable-prefix-fingerprint "stable-a"
             :request-ordinal 1)
            (:provider openai :endpoint "https://api.openai.com/v1/responses"
             :model "gpt-6-sol" :stable-prefix-fingerprint "stable-a"
             :request-ordinal 2)
            (:provider openai :endpoint "https://api.openai.com/v1/responses"
             :model "gpt-6-sol" :stable-prefix-fingerprint "stable-a"
             :request-ordinal 3)))
         (first-usage
          (append
           (plist-get
            (e-openai-decoder--usage-item
             '(:input_tokens 100
               :input_tokens_details (:cached_tokens 0 :cache_write_tokens 4)
               :output_tokens 5
               :output_tokens_details (:reasoning_tokens 1)
               :total_tokens 105))
            :usage)
           '(:provider-request-id "response-1"
             :provider-request-ordinal 1)))
         (third-usage
          (append
           (plist-get
            (e-openai-decoder--usage-item
             '(:input_tokens 300
               :input_tokens_details (:cached_tokens 200 :cache_write_tokens 0)
               :output_tokens 7
               :output_tokens_details (:reasoning_tokens 2)
               :total_tokens 307))
            :usage)
           '(:provider-request-id "response-3"
             :provider-request-ordinal 3
             :caused-by-tool-call-id "tool-call-2"
             :caused-by-tool-name "cache_probe"
             :caused-by-tool-calls
             [(:id "tool-call-2" :name "cache_probe")])))
         (usage (list first-usage third-usage))
         (evidence (e-cache-efficiency-e2e--join-request-usage requests usage))
         (first (nth 0 evidence))
         (missing (nth 1 evidence))
         (third (nth 2 evidence)))
    (should (equal (mapcar (lambda (request)
                             (plist-get request :input-tokens))
                           evidence)
                   '(100 unavailable 300)))
    (should (equal (plist-get first :provider-request-id) "response-1"))
    (should (= (plist-get first :provider-request-ordinal) 1))
    (should (eq (plist-get first :usage-attribution-status) 'ordinal))
    (should (eq (plist-get first :phase) 'initial-context-baseline))
    (should (eq (plist-get missing :usage-attribution-status) 'missing-usage))
    (should (eq (plist-get missing :usage-event-ordinal) 'unavailable))
    (should (equal (plist-get third :provider-request-id) "response-3"))
    (should (= (plist-get third :provider-request-ordinal) 3))
    (should (equal (plist-get third :caused-by-tool-call-id) "tool-call-2"))
    (should (equal (plist-get third :caused-by-tool-name) "cache_probe"))
    (should (equal (plist-get third :caused-by-tool-calls)
                   [(:id "tool-call-2" :name "cache_probe")]))
    (should (equal (plist-get (plist-get third :usage-provenance)
                              :provider-request-id)
                   "response-3"))
    (should-not (e-cache-efficiency-e2e--usage-join-complete-p evidence))))

(ert-deftest e-cache-efficiency-e2e-test-public-qualification-rejects-unmatched-usage ()
  "An extra ordinal cannot leave public cache qualification positive."
  (let* ((requests
          (cl-loop for ordinal from 1 to 4
                   collect
                   (list :provider 'openai
                         :endpoint "https://api.openai.com/v1/responses"
                         :model "gpt-6-sol"
                         :stable-prefix-fingerprint "stable-prefix"
                         :request-ordinal ordinal)))
         (matched-usage
          (cl-loop for (input cached) in '((4096 0) (10000 4096)
                                           (14000 8192) (18000 12288))
                   for ordinal from 1
                   collect
                   (list :provider-request-id (format "response-%d" ordinal)
                         :provider-request-ordinal ordinal
                         :input-tokens input :cached-input-tokens cached)))
         (complete
          (e-cache-efficiency-e2e--join-request-usage
           requests matched-usage))
         (extra-usage
          (append matched-usage
                  '((:provider-request-id "response-unmatched"
                     :provider-request-ordinal 5
                     :input-tokens 22000 :cached-input-tokens 16000))))
         (incomplete
          (e-cache-efficiency-e2e--join-request-usage requests extra-usage))
         (all-unknown-usage
          (cl-loop for ordinal from 1 to 4
                   collect (list :provider-request-id
                                 (format "unknown-%d" ordinal)
                                 :provider-request-ordinal ordinal)))
         (all-unknown
          (e-cache-efficiency-e2e--join-request-usage
           requests all-unknown-usage))
         emitted)
    (should (equal (e-cache-efficiency-e2e--qualified-growing-cache-result
                    complete)
                   "growing-history-reuse"))
    (should-not (e-cache-efficiency-e2e--usage-join-complete-p incomplete))
    (should (equal (e-cache-efficiency-e2e--qualified-growing-cache-result
                    incomplete)
                   "unconfirmed-incomplete-usage"))
    (should (equal (mapcar (lambda (request)
                             (plist-get request :cached-input-tokens))
                           incomplete)
                   '(0 4096 8192 12288)))
    (should (equal (plist-get (car incomplete)
                              :unattributed-usage-provenance-events)
                   [(:provider-request-id "response-unmatched"
                     :provider-request-ordinal 5
                     :input-tokens 22000 :cached-input-tokens 16000)]))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq emitted (apply #'format format-string args)))))
      (e-cache-efficiency-e2e--report-evidence
       (list :semantic-result "pass"
             :cache-result
             (e-cache-efficiency-e2e--qualified-growing-cache-result incomplete)
             :requests (vconcat incomplete))))
    (let* ((record (e-json-parse-string
                    (substring emitted (length "E98_CACHE_EVIDENCE "))))
           (reported (plist-get record :requests)))
      (should (equal (plist-get record :semantic-result) "pass"))
      (should (equal (plist-get record :cache-result)
                     "unconfirmed-incomplete-usage"))
      (should (= (plist-get (aref reported 1) :cached-input-tokens) 4096))
      (should (= (length (plist-get (aref reported 0)
                                    :unattributed-usage-provenance-events))
                 1)))
    (should (e-cache-efficiency-e2e--usage-join-complete-p all-unknown))
    (should-not (equal (e-cache-efficiency-e2e--qualified-growing-cache-result
                        all-unknown)
                       "growing-history-reuse"))))

(ert-deftest e-cache-efficiency-e2e-test-multi-turn-usage-joins-provider-identity ()
  "Distinct request IDs disambiguate authentic per-turn ordinal resets."
  (let* ((requests
          (cl-loop for ordinal from 1 to 3
                   collect (list :provider 'codex
                                 :request-ordinal ordinal
                                 :endpoint "https://chatgpt.com/backend-api/codex/responses"
                                 :model "gpt-6-sol"
                                 :stable-prefix-fingerprint "stable")))
         (starts
          (cl-loop for ordinal from 1 to 3
                   for id in '("turn-one" "turn-two" "turn-three")
                   collect (list :provider-request-id id
                                 :provider-request-ordinal 1)))
         (enriched (e-cache-efficiency-e2e--join-request-starts requests starts))
         (usage
          (cl-loop for input in '(9000 9100 9200)
                   for cached in '(0 8192 8192)
                   for id in '("turn-one" "turn-two" "turn-three")
                   collect (list :provider-request-id id
                                 :provider-request-ordinal 1
                                 :input-tokens input
                                 :cached-input-tokens cached)))
         (evidence (e-cache-efficiency-e2e--join-request-usage enriched usage)))
    (should (= (length evidence) 3))
    (should (equal (mapcar (lambda (request)
                             (plist-get request :request-ordinal))
                           evidence)
                   '(1 2 3)))
    (should (equal (mapcar (lambda (request)
                             (plist-get request :provider-request-ordinal))
                           evidence)
                   '(1 1 1)))
    (should (equal (mapcar (lambda (request)
                             (plist-get request :provider-request-id))
                           evidence)
                   '("turn-one" "turn-two" "turn-three")))
    (should (equal (mapcar (lambda (request)
                             (plist-get request :cached-input-tokens))
                           evidence)
                   '(0 8192 8192)))
    (should (e-cache-efficiency-e2e--usage-join-complete-p evidence))
    (let* ((missing-start
            (e-cache-efficiency-e2e--join-request-starts requests
                                                          (butlast starts)))
           (missing-evidence
            (e-cache-efficiency-e2e--join-request-usage missing-start usage))
           (duplicate-starts
            (list (car starts) (cadr starts) (car starts)))
           (duplicate
            (e-cache-efficiency-e2e--join-request-starts requests
                                                          duplicate-starts))
           (mixed-starts
            (list (car starts) '(:provider-request-ordinal 1) (caddr starts)))
           (mixed
            (e-cache-efficiency-e2e--join-request-starts requests mixed-starts))
           (mismatch-usage
            (copy-tree usage)))
      (should-not (e-cache-efficiency-e2e--usage-join-complete-p
                   missing-evidence))
      (should (eq (plist-get (car missing-start)
                             :provider-request-start-attribution-status)
                  'ambiguous))
      (should (= (length (plist-get (car missing-start)
                                    :unattributed-provider-request-start-events))
                 2))
      (should-not (e-cache-efficiency-e2e--usage-join-complete-p
                   (e-cache-efficiency-e2e--join-request-usage duplicate usage)))
      (should-not (e-cache-efficiency-e2e--usage-join-complete-p
                   (e-cache-efficiency-e2e--join-request-usage mixed usage)))
      (setf (plist-get (cadr mismatch-usage) :provider-request-ordinal) 2)
      (let ((mismatch
             (e-cache-efficiency-e2e--join-request-usage
              enriched mismatch-usage)))
        (should-not (e-cache-efficiency-e2e--usage-join-complete-p mismatch))
        (should (eq (plist-get (cadr mismatch) :usage-attribution-status)
                    'ambiguous-ordinal-mismatch))
        (should (eq (plist-get (cadr mismatch) :input-tokens)
                    'unavailable))
        (should (= (length (plist-get (cadr mismatch)
                                      :unattributed-usage-provenance-events))
                   1))))))

(ert-deftest e-cache-efficiency-e2e-test-duplicate-and-mixed-usage-provenance-is-unknown ()
  "Duplicate or mixed ordinals are retained as evidence, never positionally joined."
  (let* ((requests '((:provider openai :request-ordinal 1)
                     (:provider openai :request-ordinal 2)))
         (duplicate
          (e-cache-efficiency-e2e--join-request-usage
           requests '((:input-tokens 100 :provider-request-id "same"
                       :provider-request-ordinal 1)
                      (:input-tokens 200 :provider-request-id "same"
                       :provider-request-ordinal 1))))
         (duplicate-id
          (e-cache-efficiency-e2e--join-request-usage
           requests '((:input-tokens 100 :provider-request-id "same"
                       :provider-request-ordinal 1)
                      (:input-tokens 200 :provider-request-id "same"
                       :provider-request-ordinal 2))))
         (mixed
          (e-cache-efficiency-e2e--join-request-usage
           requests '((:input-tokens 100 :provider-request-ordinal 1)
                      (:input-tokens 200 :provider-request-id "legacy")))))
    (should (eq (plist-get (car duplicate) :usage-attribution-status)
                'ambiguous-duplicate-ordinal))
    (should (eq (plist-get (cadr duplicate) :usage-attribution-status)
                'missing-usage))
    (should (eq (plist-get (car duplicate) :input-tokens) 'unavailable))
    (should (= (length (plist-get (car duplicate)
                                  :unattributed-usage-provenance-events))
               2))
    (should (eq (plist-get (car duplicate-id) :usage-attribution-status)
                'ambiguous-duplicate-provider-id))
    (should (eq (plist-get (car duplicate-id) :input-tokens) 'unavailable))
    (should (eq (plist-get (cadr duplicate-id) :usage-attribution-status)
                'ambiguous-duplicate-provider-id))
    (should (eq (plist-get (cadr duplicate-id) :input-tokens) 'unavailable))
    (should (eq (plist-get (car mixed) :input-tokens) 'unavailable))
    (should (eq (plist-get (cadr mixed) :input-tokens) 'unavailable))
    (should (eq (plist-get (car mixed) :usage-attribution-status)
                'ambiguous-provenance))
    (should (equal (plist-get (car mixed)
                             :unattributed-usage-provenance-events)
                   [(:input-tokens 100 :provider-request-ordinal 1)
                    (:input-tokens 200 :provider-request-id "legacy")]))
    (should-not (e-cache-efficiency-e2e--usage-join-complete-p duplicate))
    (should-not (e-cache-efficiency-e2e--usage-join-complete-p duplicate-id))
    (should-not (e-cache-efficiency-e2e--usage-join-complete-p mixed))))

(ert-deftest e-cache-efficiency-e2e-test-p3-missing-route-is-unavailable ()
  "Missing endpoint identity stays conservative until real native dispatch."
  (let* ((profile '(:wire-api responses :responses-transport http
                   :env-key "E98_TEST_TOKEN" :default-model "gpt-6-sol"))
         (options '(:model "gpt-6-sol"))
         (identity (e-openai-profile--context-capability-identity
                    profile options :provider 'p3-incomplete)))
    (should-not (plist-get identity :endpoint))
    (e-cache-efficiency-e2e--with-p3-candidate-capability
     (plist-put (copy-tree identity) :endpoint "https://qualified.test/v1")
     (lambda ()
       (should (eq (plist-get (e-openai-profile-context-capabilities
                              profile options :provider 'p3-incomplete)
                             :reserved-effect-carrier) 'none))))
    (let ((e-openai-model-providers (list (cons 'p3-incomplete profile))))
      (should-error (e-openai--request-context
                     :provider 'p3-incomplete :model "gpt-6-sol"
                     :messages '((:role user :content "synthetic")) :options options)
                    :type 'e-openai-provider-invalid))))

(ert-deftest e-cache-efficiency-e2e-test-p3-candidate-is-route-scoped-at-native-owners ()
  "An ERT-only P3 candidate reaches native serializers for its exact route."
  (let* ((provider 'codex)
         (profile (e-openai-provider-profile provider))
         (base-url (e-openai-provider-base-url profile))
         (options '(:model "gpt-6-sol" :session-id "p3-synthetic"
                    :context-lifetime-enabled t
                    :reserved-effect-carrier context-curate-wire))
         (identity
          (e-openai-profile--context-capability-identity
           profile options :provider provider :base-url base-url)))
    (cl-letf (((symbol-function 'e-openai-codex-read-auth)
               (lambda (&optional _auth-file)
                 '(:tokens (:access_token "synthetic-only"))))
              ((symbol-function 'e-openai-codex-auth-account-id)
               (lambda (_auth) "synthetic-account")))
      (should (eq (plist-get
                   (e-openai-profile-context-capabilities
                    profile options :provider provider :base-url base-url)
                   :reserved-effect-carrier)
                  'none))
      (e-cache-efficiency-e2e--with-p3-candidate-capability
       identity
       (lambda ()
         (let* ((backend (e-openai-codex-backend-create :model "gpt-6-sol"))
                (capabilities (e-backend-context-capabilities backend options))
                (context
                 (e-openai--request-context
                  :provider provider :base-url base-url
                  :messages '((:role user :content "synthetic route"))
                  :options options))
                (tools (append (plist-get (plist-get context :body-data) :tools)
                               nil)))
           (should (eq (plist-get capabilities :reserved-effect-carrier)
                       'context-curate-wire))
           (should (member "context-curate"
                           (mapcar (lambda (tool) (plist-get tool :name)) tools)))
           (let* ((wrong-options (plist-put (copy-sequence options)
                                            :model "gpt-6-luna"))
                  (wrong-profile-capabilities
                   (e-openai-profile-context-capabilities
                    profile wrong-options :provider provider :base-url base-url))
                  (wrong-context
                   (e-openai--request-context
                    :provider provider :base-url base-url
                    :messages '((:role user :content "synthetic route"))
                    :options wrong-options)))
             (should (eq (plist-get wrong-profile-capabilities
                                    :reserved-effect-carrier)
                         'none))
             (should-not
              (member "context-curate"
                      (mapcar (lambda (tool) (plist-get tool :name))
                              (append
                               (plist-get (plist-get wrong-context :body-data)
                                          :tools)
                               nil)))))))))
  (let* ((provider 'p3-synthetic)
         (base-url "https://anthropic.example.test/v1")
         (model "claude-opus-5-5")
         (process-environment
          (cons "E98_P3_SYNTHETIC_TOKEN=synthetic-only" process-environment))
         (e-anthropic-model-providers
          `((p3-synthetic :name "P3 synthetic Anthropic"
             :base-url ,base-url :auth bearer :auth-header x-api-key
             :env-key "E98_P3_SYNTHETIC_TOKEN" :default-model ,model)))
         (options `(:model ,model :context-lifetime-enabled t
                    :reserved-effect-carrier context-curate-wire))
         (identity
          (e-anthropic--context-capability-identity
           provider options :base-url base-url :model model
           :requester 'messages-http-start)))
    (should (eq (plist-get
                 (e-anthropic-context-capabilities
                  provider options :base-url base-url :model model
                  :requester 'messages-http-start)
                 :reserved-effect-carrier)
                'none))
    (e-cache-efficiency-e2e--with-p3-candidate-capability
     identity
     (lambda ()
       (let* ((backend (e-anthropic-backend-create
                        :provider provider :base-url base-url :model model))
              (capabilities (e-backend-context-capabilities backend options))
              (context (e-anthropic--request-context
                        :provider provider :base-url base-url :model model
                        :requester 'messages-http-start
                        :messages '((:role user :content "synthetic route"))
                        :options options))
              (body (e-json-parse-string (plist-get context :body)))
              (tools (append (plist-get body :tools) nil)))
         (should (eq (plist-get capabilities :reserved-effect-carrier)
                     'context-curate-wire))
         (should (member "context-curate"
                         (mapcar (lambda (tool) (plist-get tool :name)) tools)))
         (let ((wrong (e-anthropic--request-context
                       :provider provider :base-url "https://other.test/v1"
                       :model model :requester 'messages-http-start
                       :messages '((:role user :content "synthetic route"))
                       :options options)))
           (should-not
            (member "context-curate"
                    (mapcar (lambda (tool) (plist-get tool :name))
                            (append (plist-get
                                     (e-json-parse-string
                                      (plist-get wrong :body))
                                     :tools)
                                    nil)))))))))))

(ert-deftest e-cache-efficiency-e2e-test-p3-normal-source-bypasses-wrapper ()
  "The normal-source runner uses no wrapper or synthetic grant count."
  (let ((e-cache-efficiency-e2e--p3-candidate-grant-count 0)
        (wrapper-identities nil)
        thunk-called)
    (cl-letf (((symbol-function
                'e-cache-efficiency-e2e--with-p3-candidate-capability)
               (lambda (identity thunk)
                 (push identity wrapper-identities)
                 (funcall thunk))))
      (should
       (eq (e-cache-efficiency-e2e--p3-run-with-authority
            'normal-source 'route-identity t
            (lambda () (setq thunk-called t) 'ran))
           'ran))
      (should thunk-called)
      (should-not wrapper-identities)
      (should (= e-cache-efficiency-e2e--p3-candidate-grant-count 0))
      (setq thunk-called nil)
      (should-error
       (e-cache-efficiency-e2e--p3-run-with-authority
        'normal-source 'route-identity nil
        (lambda () (setq thunk-called t)))
       :type 'error)
      (should-not thunk-called)
      (should-not wrapper-identities)
      (should
       (eq (e-cache-efficiency-e2e--p3-run-with-authority
            'ert-exact-route-wrapper 'diagnostic-route nil
            (lambda () 'diagnostic-ran))
           'diagnostic-ran))
      (should (equal wrapper-identities '(diagnostic-route))))))

(ert-deftest e-cache-efficiency-e2e-test-p3-anthropic-diagnostic-uses-route-identity ()
  "The diagnostic wrapper accepts Anthropic aliases but checks every route field."
  (let* ((provider 'p3-route-owner)
         (alias 'p3-route-alias)
         (base-url
          "https://eng-ai-model-gateway.sfproxy.devx-preprod.aws-esvc1-useast2.aws.sfdc.cl/v1")
         (model "claude-opus-5-5")
         (env-key "ENG_AI_MODEL_GW_KEY")
         (process-environment
          (cons "ENG_AI_MODEL_GW_KEY=synthetic-only" process-environment))
         (profile `(:name "route label is diagnostic"
                    :base-url ,base-url :auth bearer
                    :auth-header authorization :env-key ,env-key
                    :default-model ,model))
         (e-anthropic-default-provider provider)
         (e-anthropic-model-providers
          `((,provider . ,profile) (,alias . ,profile)))
         (options `(:model ,model))
         (identity
          (e-anthropic--context-capability-identity
           provider options :base-url base-url :model model
           :requester 'messages-http-start))
         (e-cache-efficiency-e2e--p3-candidate-grant-count 0)
         observed-provider)
    (cl-letf (((symbol-function 'e-live-e2e--config-file)
               (lambda ()
                 (expand-file-name
                  "e2e/e-e2e-config-anthropic-p3.el" default-directory)))
              ((symbol-function 'e-anthropic-context-capabilities)
               (lambda (&rest _args)
                 '(:reserved-effect-carrier none))))
      (let ((route (e-cache-efficiency-e2e--p3-live-candidate-route)))
        (should (eq (plist-get route :provider) provider))
        (should (eq (plist-get
                     (e-cache-efficiency-e2e--p3-route-capabilities
                      provider options (plist-get route :route))
                     :reserved-effect-carrier)
                    'none))
        (let (normal-thunk-called)
          (should-error
           (e-cache-efficiency-e2e--p3-run-with-authority
            'normal-source identity nil
            (lambda () (setq normal-thunk-called t)))
           :type 'error)
          (should-not normal-thunk-called))
        (should
         (eq (e-cache-efficiency-e2e--p3-run-with-authority
              'ert-exact-route-wrapper identity nil
              (lambda ()
                (let ((actual
                       (e-anthropic--context-capability-identity
                        alias options :base-url base-url :model model
                        :requester 'messages-http-start))
                      (capabilities
                       (e-anthropic-context-capabilities
                        alias options :base-url base-url :model model
                        :requester 'messages-http-start)))
                  (setq observed-provider (plist-get actual :provider))
                  (should (eq (plist-get capabilities
                                         :reserved-effect-carrier)
                              'context-curate-wire))
                  'diagnostic-ran)))
             'diagnostic-ran))
        (should (eq observed-provider alias))
        (should (= e-cache-efficiency-e2e--p3-candidate-grant-count 1))
        (dolist (change
                 '((:endpoint . "https://different.example.test/v1/messages")
                   (:model . "claude-sonnet-4-5")
                   (:auth-mode . (bearer x-api-key environment-token))
                   (:api . responses)
                   (:transport . injected)
                   (:requester . another-requester)))
          (let ((candidate-identity
                 (plist-put (copy-sequence identity)
                            (car change) (cdr change)))
                (e-cache-efficiency-e2e--p3-candidate-grant-count 0)
                carrier)
            (e-cache-efficiency-e2e--with-p3-candidate-capability
             candidate-identity
             (lambda ()
               (setq carrier
                     (plist-get
                      (e-anthropic-context-capabilities
                       alias options :base-url base-url :model model
                       :requester 'messages-http-start)
                      :reserved-effect-carrier))))
            (should (eq carrier 'none))
            (should (= e-cache-efficiency-e2e--p3-candidate-grant-count 0))))))))

(ert-deftest e-cache-efficiency-e2e-test-p3-openai-diagnostic-keeps-provider-identity ()
  "The Anthropic route comparison does not broaden OpenAI diagnostic grants."
  (let* ((source 'p3-openai-source)
         (alias 'p3-openai-alias)
         (profile '(:name "synthetic Responses profile"
                    :base-url "https://responses.example.test/v1"
                    :wire-api responses :responses-transport http
                    :requires-openai-auth nil :env-key "E98_OPENAI_TOKEN"
                    :default-model "gpt-6-sol"))
         (e-openai-model-providers `((,source . ,profile) (,alias . ,profile)))
         (options '(:model "gpt-6-sol"))
         (identity
          (e-openai-profile--context-capability-identity
           profile options :provider source))
         (e-cache-efficiency-e2e--p3-candidate-grant-count 0)
         carrier)
    (e-cache-efficiency-e2e--with-p3-candidate-capability
     identity
     (lambda ()
       (setq carrier
             (plist-get
              (e-cache-efficiency-e2e--p3-route-capabilities
               alias options '(:base-url "https://responses.example.test/v1"))
              :reserved-effect-carrier))))
    (should (eq carrier 'none))
    (should (= e-cache-efficiency-e2e--p3-candidate-grant-count 0))))

(ert-deftest e-cache-efficiency-e2e-test-anthropic-prefixed-model-identity-is-resolved-once ()
  "Backend, native body, and captured identity agree on prefixed model ids."
  (let* ((provider 'eng-ai-gateway-opus-5-5)
         (base-url
          "https://eng-ai-model-gateway.sfproxy.devx-preprod.aws-esvc1-useast2.aws.sfdc.cl/v1")
         (process-environment
          (cons "ENG_AI_MODEL_GW_KEY=synthetic-only" process-environment))
         (e-anthropic-model-providers
          `((,provider :name "synthetic route label"
             :base-url ,base-url :auth bearer :auth-header authorization
             :env-key "ENG_AI_MODEL_GW_KEY" :model-prefix "claude-"
             :default-model "sonnet-5-5"))))
    (cl-labels
        ((check-route (session backend-model options expected-model carrier)
           (let* ((options (append `(:session-id ,session
                                    :context-lifetime-enabled t)
                                  options))
                  (backend (e-anthropic-backend-create
                            :provider provider :base-url base-url
                            :model backend-model))
                  (capabilities (e-backend-context-capabilities backend options))
                  (requests (make-hash-table :test #'equal))
                  (context
                   (e-cache-efficiency-e2e--capture-p3-native-request
                    'anthropic #'e-anthropic--request-context requests
                    (list :provider provider :base-url base-url
                          :model backend-model
                          :requester 'messages-http-start
                          :messages '((:role user :content "synthetic model route"))
                          :options options)))
                  (captured (car (gethash session requests))))
             (should (stringp (plist-get context :body)))
             (should (eq (plist-get capabilities :reserved-effect-carrier)
                         carrier))
             (should (equal (plist-get captured :model) expected-model))
             (should (equal (plist-get captured :identity-model) expected-model))
             (should (eq (plist-get captured :context-curation-tool-present)
                         (eq carrier 'context-curate-wire)))
             captured)))
      (check-route "p3-prefixed-option" "sonnet-5-5"
                   '(:model "opus-5-5") "claude-opus-5-5"
                   'context-curate-wire)
      (check-route "p3-prefixed-backend-default" "opus-5-5" nil
                   "claude-opus-5-5" 'context-curate-wire)
      (check-route "p3-prefixed-profile-default" nil nil
                   "claude-sonnet-5-5" 'none)
      (check-route "p3-prefixed-model-change" "opus-5-5"
                   '(:model "sonnet-5-5") "claude-sonnet-5-5" 'none))))

(ert-deftest e-cache-efficiency-e2e-test-p3-route-resolves-effective-model-once ()
  "The P3 route builder accepts an equivalent provider alias and resolved model."
  (let* ((provider 'review-equivalent-alias)
         (base-url
          "https://eng-ai-model-gateway.sfproxy.devx-preprod.aws-esvc1-useast2.aws.sfdc.cl/v1")
         (process-environment
          (cons "ENG_AI_MODEL_GW_KEY=synthetic-only" process-environment))
         (e-anthropic-default-provider provider)
         (e-anthropic-model-providers
          `((,provider :name "route label may differ"
             :base-url ,base-url :auth bearer :auth-header authorization
             :env-key "ENG_AI_MODEL_GW_KEY" :model-prefix "claude-"
             :default-model "opus-5-5")))
         route)
    (cl-letf (((symbol-function 'e-live-e2e--config-file)
               (lambda ()
                 (expand-file-name
                  "e2e/e-e2e-config-anthropic-p3.el" default-directory))))
      (setq route (e-cache-efficiency-e2e--p3-live-candidate-route))
      (should (equal (plist-get route :model) "claude-opus-5-5"))
      (should (eq (plist-get (plist-get route :identity) :provider) provider))
      (should (equal (plist-get (plist-get route :identity) :model)
                     "claude-opus-5-5"))
      (should (eq (plist-get
                   (e-cache-efficiency-e2e--p3-route-capabilities
                    provider (plist-get route :options) (plist-get route :route))
                   :reserved-effect-carrier)
                  'context-curate-wire)))))

(defun e-cache-efficiency-e2e--p3-register-source-tool (registry)
  "Register a deterministic P3 tool that resolves opaque source keys."
  (e-tools-register
   registry :name "read_p3_source"
   :description "Read one synthetic source by its opaque key."
   :parameters '(:type "object" :properties (:source-key (:type "string"))
                 :required ["source-key"])
   :work (e-tools-cheap-work
          "e98.p3.source"
          (lambda (arguments)
            (let* ((key (plist-get arguments :source-key))
                   (fact (cdr (assoc key
                                     '(("source-one" . "P3-KEEP-FACT-ONE")
                                       ("source-two" . "P3-KEEP-FACT-TWO")
                                       ("source-three" . "P3-SUMMARY-RAW-THREE")
                                       ("source-four" . "P3-ERASE-RAW-FOUR"))))))
              (unless fact
                (error "Unknown synthetic P3 source key"))
              (concat fact "\n"
                      (mapconcat
                       (lambda (index)
                         (format "synthetic-source-row-%04d" index))
                       (number-sequence 1 24) "\n")))))))

(defun e-cache-efficiency-e2e--p3-request-contains-text-p (request text)
  "Return non-nil when captured REQUEST's actual native history contains TEXT."
  (let* ((adapter (plist-get request :adapter))
         (body (plist-get request :native-body))
         (items (and adapter body
                     (e-cache-efficiency-e2e--p3-native-history-items
                      adapter body))))
    (and items
         (string-match-p (regexp-quote text) (prin1-to-string items)))))

(defun e-cache-efficiency-e2e--p3-native-projection-valid-p
    (request required-text forbidden-texts)
  "Return non-nil when native REQUEST has REQUIRED-TEXT and omits FORBIDDEN-TEXTS."
  (and (or (null required-text)
           (e-cache-efficiency-e2e--p3-request-contains-text-p
            request required-text))
       (cl-every
        (lambda (text)
          (not (e-cache-efficiency-e2e--p3-request-contains-text-p
                request text)))
        forbidden-texts)))

(defun e-cache-efficiency-e2e--p3-replace-native-text (value old new)
  "Return a detached VALUE with string OLD replaced by NEW recursively."
  (cond
   ((stringp value) (replace-regexp-in-string (regexp-quote old) new value t t))
   ((and (vectorp value) (not (stringp value)))
    (vconcat (mapcar (lambda (item)
                       (e-cache-efficiency-e2e--p3-replace-native-text
                        item old new))
                     (append value nil))))
   ((consp value)
    (mapcar (lambda (item)
              (e-cache-efficiency-e2e--p3-replace-native-text item old new))
            value))
   (t value)))

(defun e-cache-efficiency-e2e--p3-request-for-user-marker (requests marker)
  "Return the first captured REQUEST containing top-level user MARKER."
  (seq-find
   (lambda (request)
     (seq-some
      (lambda (message)
        (and (eq (plist-get message :role) 'user)
             (stringp (plist-get message :content))
             (string-match-p (regexp-quote marker)
                             (plist-get message :content))))
      (plist-get request :messages)))
   requests))

(defun e-cache-efficiency-e2e--p3-last-user-text (messages)
  "Return the current top-level user text from provider-neutral MESSAGES."
  (plist-get
   (car (last (seq-filter
               (lambda (message) (eq (plist-get message :role) 'user))
               messages)))
   :content))

(defun e-cache-efficiency-e2e--p3-native-body (adapter context)
  "Return decoded native request data for ADAPTER CONTEXT."
  (pcase adapter
    ('anthropic (e-json-parse-string (plist-get context :body)))
    (_ (plist-get context :body-data))))

(defun e-cache-efficiency-e2e--p3-native-item-hashes (adapter body)
  "Return content-bounded hashes for ADAPTER's history items in BODY."
  (vconcat
   (mapcar
    (lambda (item)
      (secure-hash
       'sha256
       (encode-coding-string
        (e-json-serialize
         (e-cache-efficiency-e2e--canonical-json-value item))
        'utf-8)))
    (e-cache-efficiency-e2e--p3-native-history-items adapter body))))

(defun e-cache-efficiency-e2e--p3-native-history-items (adapter body)
  "Return all ordered model-visible native context items from BODY."
  (if (eq adapter 'anthropic)
      (append (append (plist-get body :system) nil)
              (append (plist-get body :messages) nil))
    (append (when (plist-get body :instructions)
              (list (list :instructions (plist-get body :instructions))))
            (append (plist-get body :input) nil))))

(defun e-cache-efficiency-e2e--p3-semantic-wire-value (value)
  "Return VALUE with provider cache-control metadata removed recursively."
  (cond
   ((and (listp value)
         (or (null value) (keywordp (car value))))
    (let (result)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (unless (memq key '(:cache_control :cache-control))
            (setq result
                  (append result
                          (list key
                                (e-cache-efficiency-e2e--p3-semantic-wire-value
                                 item)))))))
      result))
   ((and (vectorp value) (not (stringp value)))
    (vconcat (mapcar #'e-cache-efficiency-e2e--p3-semantic-wire-value
                     (append value nil))))
   (t value)))

(defun e-cache-efficiency-e2e--p3-native-semantic-item-hashes (adapter body)
  "Return ordered semantic history hashes, excluding cache-control metadata."
  (vconcat
   (mapcar
    (lambda (item)
      (secure-hash
       'sha256
       (encode-coding-string
        (e-json-serialize
         (e-cache-efficiency-e2e--canonical-json-value
          (e-cache-efficiency-e2e--p3-semantic-wire-value item)))
        'utf-8)))
    (e-cache-efficiency-e2e--p3-native-history-items adapter body))))

(defun e-cache-efficiency-e2e--capture-p3-native-request
    (adapter native requests-by-session args)
  "Call native context constructor NATIVE and keep a bounded P3 capture."
  (let* ((context (apply native args))
         (options (plist-get args :options))
         (session-id (plist-get options :session-id))
         (body (e-cache-efficiency-e2e--p3-native-body adapter context))
         (messages (plist-get args :messages))
         (layout-revision
          (or (plist-get options :canvas-reference-layout-revision)
              (e-context-canvas-reference-layout-for-messages messages)))
         (effective-options (copy-sequence options))
         (_ (when layout-revision
              (setq effective-options
                    (plist-put effective-options
                               :canvas-reference-layout-revision
                               layout-revision))))
         (user-text (e-cache-efficiency-e2e--p3-last-user-text messages))
         (records (gethash session-id requests-by-session))
         (profile (and (eq adapter 'openai)
                       (e-openai-provider-profile
                        (plist-get args :provider))))
         (identity
          (if (eq adapter 'anthropic)
              (e-anthropic--context-capability-identity
               (plist-get args :provider) effective-options
               :base-url (plist-get args :base-url)
               :model (plist-get body :model)
               :request-function (plist-get args :request-function)
               :requester (plist-get args :requester))
            (e-openai-profile--context-capability-identity
             profile effective-options :provider (plist-get args :provider)
             :base-url (or (plist-get args :base-url)
                           (e-openai-provider-base-url profile))
             :request-function (plist-get args :request-function))))
         (history-hashes
          (e-cache-efficiency-e2e--p3-native-item-hashes adapter body))
         (tools (append (plist-get body :tools) nil))
         (record
          (list :adapter adapter
                :provider (plist-get context :provider)
                :endpoint (plist-get context :url)
                :model (plist-get body :model)
                :identity-model (plist-get identity :model)
                :auth-mode (plist-get identity :auth-mode)
                :api (plist-get identity :api)
                :transport (plist-get identity :transport)
                :requester (plist-get identity :requester)
                :canvas-reference-layout-revision
                (plist-get identity :canvas-reference-layout-revision)
                :canvas-source-evidence
                (e-cache-efficiency-e2e--canvas-source-evidence effective-options)
                :canvas-operations
                (e-cache-efficiency-e2e--canvas-operation-evidence messages)
                :user-scenario
                (e-cache-efficiency-e2e--p3-live-scenario
                 (or user-text ""))
                :user-prompt-sha256
                (and (stringp user-text) (secure-hash 'sha256 user-text))
                :request-ordinal (1+ (length records))
                :stable-prefix-fingerprint
                (secure-hash
                 'sha256
                 (prin1-to-string
                  (list (cl-loop for segment in (plist-get options :segments)
                                 when (memq (plist-get segment :kind)
                                            '(static-prefix stable-context))
                                 collect (plist-get segment :messages))
                        (e-cache-efficiency-e2e--p3-semantic-wire-value
                         (plist-get body :tools)))))
                :native-request-sha256
                (secure-hash 'sha256 (e-json-serialize body))
                :native-item-sha256 history-hashes
                :native-semantic-history-item-sha256
                (e-cache-efficiency-e2e--p3-native-semantic-item-hashes
                 adapter body)
                :native-semantic-history-sha256
                (secure-hash
                 'sha256
                 (prin1-to-string
                  (e-cache-efficiency-e2e--p3-native-semantic-item-hashes
                   adapter body)))
                :tool-names
                (vconcat (mapcar (lambda (tool) (plist-get tool :name)) tools))
                :context-curation-tool-present
                (and (seq-some
                      (lambda (tool)
                        (member (plist-get tool :name)
                                '("context-curate" context-curate)))
                      tools)
                     t)
                ;; Keep contents only in the isolated process for projection
                ;; assertions.  The report serializer removes both fields.
                :messages (copy-tree messages t)
                :native-body (copy-tree body t))))
    (setq record
          (plist-put record :synthetic-source-locations
                     (e-cache-efficiency-e2e--p3-native-source-locations record)))
    (puthash session-id (append records (list record)) requests-by-session)
    context))

(defun e-cache-efficiency-e2e--p3-report-request (request)
  "Return REQUEST without transient prompt or native body content."
  (let ((copy (copy-tree request t)))
    (cl-remf copy :messages)
    (cl-remf copy :native-body)
    copy))

(defun e-cache-efficiency-e2e--p3-live-layer ()
  "Return a context-free layer with stable policy and synthetic-only tools."
  (e-layer-create
   :id 'cache-efficiency-p3-candidate
   :name "P3 isolated cache and curation candidate"
   :capabilities
   (list
    (e-capability-create
     :id 'cache-efficiency-p3-candidate
     :instructions
     (concat "Never quote synthetic source facts in assistant text. "
             "Use source facts only in context-curate arguments. "
             "Every final answer must contain only the requested OK marker.\n"
             (mapconcat (lambda (index) (format "P3 stable policy rule %04d" index))
                        (number-sequence 1 1500) "\n"))
     :tools
     (list #'e-cache-efficiency-e2e--p3-register-source-tool
           (lambda (registry)
             (e-cache-efficiency-e2e--p3-register-cache-probe registry)))))))

(defun e-cache-efficiency-e2e--p3-register-cache-probe (registry)
  "Register two bounded synthetic cache probes in REGISTRY."
  (e-tools-register
   registry :name "cache_probe"
   :description "Read synthetic cache probes sequentially with steps 1 and 2."
   :parameters '(:type "object" :required ["step"]
                 :properties (:step (:type "integer" :enum [1 2])))
   :work
   (e-tools-cheap-work
    "e98.p3.cache-probe"
    (lambda (arguments)
      (let ((step (plist-get arguments :step)))
        (unless (memq step '(1 2))
          (error "Invalid P3 cache probe step"))
        (concat (format "P3-PROBE-%d-CODE=%d\n" step (+ 731 step))
                (mapconcat
                 (lambda (index)
                   (format "p3-row-%d-%04d synthetic cache evidence" step index))
                 (number-sequence 1 320) "\n")))))))

(defun e-cache-efficiency-e2e--p3-live-scenario (prompt)
  "Return a bounded scenario label for P3 top-level PROMPT."
  (cond
   ((string-match-p "P3-STABLE-BASELINE\\|P3-BASELINE" prompt) 'stable-only-baseline)
   ((string-match-p "P3-APPEND-CONTROL" prompt) 'pre-curation-append-only)
   ((string-match-p "P3-CURATE-KEEP-ONE\\|P3-ACTION-KEEP-ONE" prompt) 'curation-keep-one)
   ((string-match-p "P3-CURATE-KEEP-TWO\\|P3-ACTION-KEEP-TWO" prompt) 'curation-keep-two)
   ((string-match-p "P3-CURATE-SUMMARY\\|P3-ACTION-SUMMARY" prompt) 'curation-summary)
   ((string-match-p "P3-CURATE-ERASE\\|P3-ACTION-ERASE" prompt) 'curation-erase)
   ((string-match-p "P3-REUSE-KEEP-ONE\\|P3-FRESH-KEEP-ONE" prompt) 'reuse-keep-one)
   ((string-match-p "P3-REUSE-KEEP-TWO\\|P3-FRESH-KEEP-TWO" prompt) 'reuse-keep-two)
   ((string-match-p "P3-REUSE-SUMMARY\\|P3-FRESH-SUMMARY" prompt) 'reuse-summary)
   ((string-match-p "P3-REUSE-ERASE\\|P3-FRESH-ERASE" prompt) 'reuse-erase)
   (t 'interim-or-extra)))

(defun e-cache-efficiency-e2e--p3-durable-prompt (prompt)
  "Append synthetic durable history to a P3 control or reuse PROMPT."
  (if (or (string-prefix-p "P3-APPEND-CONTROL" prompt)
          (string-prefix-p "P3-REUSE" prompt))
      (concat prompt "\nSynthetic durable append-only history:\n"
              (mapconcat
               (lambda (index)
                 (format "p3-durable-control-row-%04d synthetic append-only history evidence"
                         index))
               (number-sequence 1 (if (string-prefix-p "P3-REUSE" prompt)
                                      300
                                    400)) "\n"))
    prompt))

(defun e-cache-efficiency-e2e--p3-package-projection (package)
  "Return the accepted semantic projection type for P3 PACKAGE."
  (let* ((promotion (plist-get package :promotion))
         (items (append (plist-get promotion :items) nil)))
    (cond
     ((plist-get package :erasure) 'erase)
     ((seq-some (lambda (item) (eq (plist-get item :kind) 'summary)) items)
      'summary)
     ((seq-some (lambda (item) (eq (plist-get item :kind) 'exact)) items)
      'keep)
     (t 'state-only))))

(defun e-cache-efficiency-e2e--p3-package-field (package field)
  "Read shared FIELD from PACKAGE's accepted promotion or erasure component."
  (plist-get (or (plist-get package :promotion) (plist-get package :erasure)) field))

(defun e-cache-efficiency-e2e--p3-package-curation-id (package)
  "Return PACKAGE's accepted curation identity."
  (or (plist-get (plist-get package :promotion) :id)
      (plist-get (plist-get package :erasure) :id)))

(defun e-cache-efficiency-e2e--p3-source-observation-ids (package)
  "Return source observation identities carried by accepted PACKAGE."
  (let ((promotion-items
         (append (plist-get (plist-get package :promotion) :items) nil))
        (erasure-sources
         (append (plist-get (plist-get package :erasure) :sources) nil)))
    (append
     (cl-loop for item in promotion-items
              append (append (plist-get item :source-observation-ids) nil))
     (mapcar (lambda (source) (plist-get source :source-observation-id))
             erasure-sources))))

(defun e-cache-efficiency-e2e--p3-source-fact (source-key)
  "Return the synthetic fact for SOURCE-KEY."
  (cdr (assoc source-key
              '(("source-one" . "P3-KEEP-FACT-ONE")
                ("source-two" . "P3-KEEP-FACT-TWO")
                ("source-three" . "P3-SUMMARY-RAW-THREE")
                ("source-four" . "P3-ERASE-RAW-FOUR")))))

(defun e-cache-efficiency-e2e--p3-native-matching-item-evidence
    (request text)
  "Return native history indexes and semantic hashes whose items contain TEXT."
  (let ((items (e-cache-efficiency-e2e--p3-native-history-items
                (plist-get request :adapter)
                (plist-get request :native-body)))
        (hashes (append (plist-get request
                                   :native-semantic-history-item-sha256)
                        nil))
        indexes digests)
    (cl-loop for item in items
             for index from 0
             when (string-match-p (regexp-quote text) (prin1-to-string item))
             do (push index indexes)
             and do (push (nth index hashes) digests))
    (list :indexes (vconcat (nreverse indexes))
          :semantic-item-sha256 (vconcat (nreverse digests)))))

(defun e-cache-efficiency-e2e--p3-native-source-locations (request)
  "Record synthetic sentinel locations without native content."
  (let ((items (e-cache-efficiency-e2e--p3-native-history-items
                (plist-get request :adapter) (plist-get request :native-body))))
    (vconcat
     (cl-loop for source-key in '("source-one" "source-two" "source-three" "source-four")
              for fact = (e-cache-efficiency-e2e--p3-source-fact source-key)
              collect
              (list :source-key source-key
                    :matching-items
                    (vconcat
                     (cl-loop for item in items
                              for index from 0
                              when (string-match-p (regexp-quote fact)
                                                   (prin1-to-string item))
                              collect (list :index index
                                            :role (plist-get item :role)
                                            :type (plist-get item :type)))))))))

(defun e-cache-efficiency-e2e--p3-protected-prefix (request &optional prefix-items)
  "Bind a previously reused PREFIX-ITEMS boundary to warm REQUEST."
  (let* ((items (append (or prefix-items
                           (plist-get request :native-semantic-history-item-sha256))
                        nil))
         (digest (and items
                      (secure-hash 'sha256 (prin1-to-string (vconcat items))))))
    (when (and items digest
               (equal items (seq-take
                             (append (plist-get request :native-semantic-history-item-sha256) nil)
                             (length items))))
      (list :request-id (plist-get request :provider-request-id)
            :turn-id (plist-get request :provider-request-turn-id)
            :item-count (length items)
            :item-sha256 (vconcat items)
            :sha256 digest))))

(defun e-cache-efficiency-e2e--p3-protected-prefix-preserved-p
    (protected request)
  "Return non-nil when REQUEST preserves PROTECTED's exact ordered positions."
  (let ((expected (append (plist-get protected :item-sha256) nil))
        (actual (append (plist-get request
                                   :native-semantic-history-item-sha256)
                        nil)))
    (and expected
         (= (length expected) (plist-get protected :item-count))
         (<= (length expected) (length actual))
         (equal expected (seq-take actual (length expected))))))

(ert-deftest e-cache-efficiency-e2e-test-p3-protected-native-prefix ()
  "Real native-item hashes distinguish append growth from prefix rewrites."
  (dolist (adapter '(openai anthropic))
    (let* ((body (if (eq adapter 'openai)
                     '(:input [(:role "user" :content "old-one")
                               (:role "assistant" :content "old-two")])
                   '(:messages [(:role "user" :content "old-one")
                                (:role "assistant" :content "old-two")])) )
           (items (e-cache-efficiency-e2e--p3-native-semantic-item-hashes adapter body))
           (before (list :provider-request-id "warm" :provider-request-turn-id "turn"
                         :native-semantic-history-item-sha256 items))
           (protected (e-cache-efficiency-e2e--p3-protected-prefix before))
           (after (list :native-semantic-history-item-sha256 (vconcat items ["appended"]))))
      (should (e-cache-efficiency-e2e--p3-protected-prefix-preserved-p protected after))
      (dolist (changed (list (vconcat (reverse (append items nil)))
                            (vconcat (cdr (append items nil)))
                            (vector "changed" (aref items 1))))
        (should-not (e-cache-efficiency-e2e--p3-protected-prefix-preserved-p
                     protected (list :native-semantic-history-item-sha256 changed)))))))

(defun e-cache-efficiency-e2e--p3-projection-boundary
    (package consumer fresh projection)
  "Return content-free source boundary evidence for PACKAGE's PROJECTION."
  (let* ((source-key
          (let* ((observations (e-cache-efficiency-e2e--p3-source-observation-ids package))
                 (call (seq-find
                        (lambda (message)
                          (let ((call (plist-get message :content)))
                            (and (eq (plist-get message :role) 'tool-call)
                                 (equal (plist-get call :name) "read_p3_source")
                                 (member (format "observation:tool-bundle:%s"
                                                 (plist-get call :id))
                                         observations))))
                        (plist-get consumer :messages))))
            (plist-get (plist-get (plist-get call :content) :arguments)
                       :source-key)))
         (source-fact (and source-key
                           (e-cache-efficiency-e2e--p3-source-fact source-key)))
         (before (and source-fact
                      (e-cache-efficiency-e2e--p3-native-matching-item-evidence
                       consumer source-fact)))
         (after-fact (and (eq projection 'summary) "P3-SUMMARY-THREE"))
         (after (and after-fact
                     (e-cache-efficiency-e2e--p3-native-matching-item-evidence
                      fresh after-fact)))
         (raw-absent
          (and source-fact
               (not (e-cache-efficiency-e2e--p3-request-contains-text-p
                     fresh source-fact))))
         (projected-observed
          (pcase projection
            ('summary (and after (> (length (plist-get after :indexes)) 0)))
            ('erase raw-absent)
            (_ nil))))
    (when (and before (> (length (plist-get before :indexes)) 0)
               projected-observed)
      (list :projection projection
            :source-observation-ids
            (vconcat (e-cache-efficiency-e2e--p3-source-observation-ids
                      package))
            :response-entry-id (e-cache-efficiency-e2e--p3-package-field package :response-entry-id)
            :curation-id (e-cache-efficiency-e2e--p3-package-curation-id package)
            :consumer-request-id (plist-get consumer :provider-request-id)
            :fresh-request-id (plist-get fresh :provider-request-id)
            :source-before-indexes (plist-get before :indexes)
            :source-before-semantic-item-sha256
            (plist-get before :semantic-item-sha256)
            :projection-after-indexes (or (plist-get after :indexes) [])
            :projection-after-semantic-item-sha256
            (or (plist-get after :semantic-item-sha256) [])
            :raw-source-absent-after (and raw-absent t)
            :fresh-history-item-count
            (length (append (plist-get fresh
                                       :native-semantic-history-item-sha256)
                            nil))
            :fresh-history-semantic-sha256
            (plist-get fresh :native-semantic-history-sha256)))))

(defvar e-cache-efficiency-e2e--p3-response-bindings nil)

(defun e-cache-efficiency-e2e--p3-with-response-capture (thunk)
  "Call THUNK while capturing the actual response/frame/provider identity join."
  (let ((owner (symbol-function 'e-harness-context-lifetime-preflight-response)))
    (cl-letf (((symbol-function 'e-harness-context-lifetime-preflight-response)
               (lambda (harness session-id turn-id active payload)
                 (let ((result (funcall owner harness session-id turn-id active payload)))
                   (when (plist-get result :reserved-response-p)
                     (push (list :response-entry-id (plist-get result :response-id)
                                 :provider-request-id (plist-get payload :provider-request-id)
                                 :provider-request-ordinal (plist-get payload :provider-request-ordinal)
                                 :consumer-request-id (plist-get result :consumer-request-id)
                                 :frame-id (e-context-lifetime-frame-id (plist-get result :frame))
                                 :turn-id turn-id)
                           e-cache-efficiency-e2e--p3-response-bindings))
                   result))))
      (funcall thunk))))

(defun e-cache-efficiency-e2e--p3-bound-consumed-events (events)
  "Join captured preflight identities to consumed EVENTS without changing them."
  (mapcar
   (lambda (event)
     (let* ((copy (copy-tree event t))
            (payload (plist-get copy :payload))
            (matches (seq-filter
                      (lambda (binding)
                        (and (equal (plist-get binding :response-entry-id)
                                    (plist-get payload :response-entry-id))
                             (equal (plist-get binding :frame-id) (plist-get payload :frame-id))
                             (equal (plist-get binding :consumer-request-id)
                                    (plist-get payload :consumer-request-id))
                             (equal (plist-get binding :turn-id) (plist-get copy :turn-id))))
                      e-cache-efficiency-e2e--p3-response-bindings)))
       (when (= (length matches) 1)
         (setq payload (plist-put payload :provider-request-id
                                  (plist-get (car matches) :provider-request-id)))
         (plist-put copy :payload payload))
       copy))
   events))

(defun e-cache-efficiency-e2e--p3-fresh-request-after-curation
    (requests package response-events consumed-events)
  "Return unique response, consumer, frame, and first fresh-request joins."
  (let* ((response-id (e-cache-efficiency-e2e--p3-package-field package :response-entry-id))
         (response-matches
          (seq-filter (lambda (event) (equal (plist-get event :id) response-id))
                      response-events))
         (consumed-matches
          (seq-filter
           (lambda (event)
             (equal (plist-get (plist-get event :payload) :response-entry-id)
                    response-id))
           consumed-events))
         (response (and (= (length response-matches) 1)
                        (car response-matches)))
         (consumed (and (= (length consumed-matches) 1)
                        (car consumed-matches)))
         (curation-turn (and consumed (plist-get consumed :turn-id)))
         (payload (plist-get consumed :payload))
         (curation-id (e-cache-efficiency-e2e--p3-package-curation-id package))
         (consumer-id (plist-get payload :provider-request-id))
         (consumer-matches
          (seq-filter (lambda (request)
                        (equal (plist-get request :provider-request-id)
                               consumer-id))
                      requests))
         (consumer (and (= (length consumer-matches) 1)
                        (car consumer-matches)))
         (same-turn
          (seq-filter
           (lambda (request)
             (equal (plist-get request :provider-request-turn-id)
                    curation-turn))
           requests))
         (last-same-turn (car (last same-turn)))
         (last-index (and last-same-turn
                          (seq-position requests last-same-turn)))
         (later (and last-index (nthcdr (1+ last-index) requests)))
         (fresh
          (and later
               (seq-find
                (lambda (request)
                  (not (equal (plist-get request :provider-request-turn-id)
                              curation-turn)))
                later)))
         (expected-scenario
          (pcase (and consumer (plist-get consumer :user-scenario))
            ('curation-keep-one 'reuse-keep-one)
            ('curation-keep-two 'reuse-keep-two)
            ('curation-summary 'reuse-summary)
            ('curation-erase 'reuse-erase)))
         (fresh-group
          (and fresh
               (seq-take-while
                (lambda (request)
                  (eq (plist-get request :user-scenario) expected-scenario))
                (nthcdr (seq-position requests fresh) requests))))
         (same-turn-after-consumer
          (and consumer
               (let ((consumer-index (seq-position requests consumer)))
                 (seq-filter
                  (lambda (request)
                    (and (equal (plist-get request :provider-request-turn-id)
                                curation-turn)
                         (> (seq-position requests request) consumer-index)))
                  requests))))
         (replay (car same-turn-after-consumer)))
    (when (and response consumed consumer fresh replay expected-scenario
               (equal (plist-get response :turn-id) curation-turn)
               (equal (plist-get consumer :provider-request-turn-id)
                      curation-turn)
               (stringp (plist-get payload :frame-id))
               (equal (plist-get payload :frame-id)
                      (e-cache-efficiency-e2e--p3-package-field package :frame-id))
               (equal (plist-get payload :consumer-request-id)
                      (e-cache-efficiency-e2e--p3-package-field package :consumer-request-id))
               (or (null (plist-get package :promotion))
                   (member curation-id (append (plist-get payload :curation-ids) nil)))
               (eq (plist-get fresh :user-scenario) expected-scenario)
               fresh-group)
      (list :join-status 'unique-complete
            :request fresh :turn-id (plist-get fresh :provider-request-turn-id)
            :curation-turn-id curation-turn :consumed-event consumed
            :response-event response :response-entry-id response-id
            :curation-id curation-id :consumer-request consumer
            :consumer-request-id consumer-id
            :frame-consumer-request-id (plist-get payload :consumer-request-id)
            :frame-id (plist-get payload :frame-id)
            :curation-request-count (length same-turn)
            :replay-request replay
            :curation-turn-requests same-turn
            :post-consumer-requests same-turn-after-consumer
            :replay-request-id (plist-get replay :provider-request-id)
            :source-observation-ids
            (e-cache-efficiency-e2e--p3-source-observation-ids package)
            :fresh-group fresh-group))))

(defun e-cache-efficiency-e2e--p3-join-request-finishes (requests finishes)
  "Join FINISHES to REQUESTS by unique provider identity and ordinal."
  (let ((by-id (make-hash-table :test #'equal))
        (counts (make-hash-table :test #'equal))
        (request-counts (make-hash-table :test #'equal)))
    (dolist (finish finishes)
      (let ((id (plist-get finish :provider-request-id)))
        (puthash id (cons finish (gethash id by-id)) by-id)
        (puthash id (1+ (gethash id counts 0)) counts)))
    (dolist (request requests)
      (let ((id (plist-get request :provider-request-id)))
        (puthash id (1+ (gethash id request-counts 0)) request-counts)))
    (let* ((joined
            (mapcar
             (lambda (request)
               (let* ((id (plist-get request :provider-request-id))
                      (matching (gethash id by-id))
                      (finish (car matching))
                      (status
                       (cond
                        ((> (gethash id request-counts 0) 1) 'duplicate-request-id)
                        ((> (gethash id counts 0) 1) 'duplicate-finish-id)
                        ((null finish) 'missing-finish)
                        ((/= (plist-get request :provider-request-ordinal)
                             (plist-get finish :provider-request-ordinal))
                         'ordinal-mismatch)
                        (t 'matched))))
                 (append (copy-tree request)
                         (list :provider-request-finish-attribution-status status)
                         (when (eq status 'matched)
                           (list :provider-request-finish-status
                                 (plist-get finish :status))))))
             requests))
           (unmatched
            (seq-filter
             (lambda (finish)
               (let* ((id (plist-get finish :provider-request-id))
                      (request
                       (seq-find (lambda (candidate)
                                   (equal (plist-get candidate
                                                     :provider-request-id)
                                          id))
                                 requests)))
                 (not (and request
                           (= (gethash id request-counts 0) 1)
                           (= (gethash id counts 0) 1)
                           (equal (plist-get request
                                             :provider-request-ordinal)
                                  (plist-get finish
                                             :provider-request-ordinal))))))
             finishes)))
      (when (and joined unmatched)
        (setf (car joined)
              (append (copy-sequence (car joined))
                      (list :unattributed-provider-request-finish-events
                            (vconcat (mapcar #'copy-tree unmatched))))))
      joined)))

(ert-deftest e-cache-efficiency-e2e-test-p3-finish-joins-retain-extras ()
  "Finish joins preserve missing, duplicate, and unmatched requests."
  (let* ((requests '((:provider-request-id "request-a"
                      :provider-request-ordinal 1)
                     (:provider-request-id "request-b"
                      :provider-request-ordinal 1)))
         (finishes '((:provider-request-id "request-a"
                      :provider-request-ordinal 1 :status done)
                     (:provider-request-id "request-a"
                      :provider-request-ordinal 2 :status failed)
                     (:provider-request-id "request-b"
                      :provider-request-ordinal 2 :status done)
                     (:provider-request-id "request-extra"
                      :provider-request-ordinal 1 :status done)))
         (joined
          (e-cache-efficiency-e2e--p3-join-request-finishes requests finishes)))
    (should (eq (plist-get (car joined)
                           :provider-request-finish-attribution-status)
                'duplicate-finish-id))
    (should (eq (plist-get (cadr joined)
                           :provider-request-finish-attribution-status)
                'ordinal-mismatch))
    (should (equal (mapcar (lambda (request)
                             (plist-get request
                                        :provider-request-finish-attribution-status))
                           joined)
                   '(duplicate-finish-id ordinal-mismatch)))
    (should (= (length (plist-get (car joined)
                                  :unattributed-provider-request-finish-events))
               4))))

(defun e-cache-efficiency-e2e--p3-shared-prefix-items (requests)
  "Return the ordered semantic native prefix actually shared by REQUESTS."
  (let* ((sequences
          (mapcar (lambda (request)
                    (append (plist-get request
                                       :native-semantic-history-item-sha256)
                            nil))
                  requests))
         (first (car sequences))
         (prefix-length
          (if (null first)
              0
            (cl-loop for index from 0 below (length first)
                     while (cl-every
                            (lambda (sequence)
                              (equal (nth index sequence)
                                     (nth index first)))
                            (cdr sequences))
                     count t)))
         (prefix (seq-take first prefix-length)))
    (when prefix (vconcat prefix))))

(defun e-cache-efficiency-e2e--p3-prefix-fingerprint (requests)
  "Hash the longest shared semantic native-history prefix in REQUESTS."
  (when-let* ((items (e-cache-efficiency-e2e--p3-shared-prefix-items requests)))
    (secure-hash 'sha256 (prin1-to-string (append items nil)))))

(defun e-cache-efficiency-e2e--p3-route-evidence (request provider-id)
  "Return REQUEST's own bounded route identity and authenticated ordinals."
  (list :provider-id provider-id
        :provider (plist-get request :provider)
        :endpoint (plist-get request :endpoint)
        :model (plist-get request :model)
        :auth-mode (plist-get request :auth-mode)
        :api (plist-get request :api)
        :transport (plist-get request :transport)
        :requester (plist-get request :requester)
        :canvas-reference-layout-revision
        (plist-get request :canvas-reference-layout-revision)
        :stable-prefix-fingerprint
        (plist-get request :stable-prefix-fingerprint)
        :provider-request-id (plist-get request :provider-request-id)
        :provider-request-ordinal
        (plist-get request :provider-request-ordinal)
        :provider-request-turn-id
        (plist-get request :provider-request-turn-id)
        :request-start-attribution-status
        (plist-get request :provider-request-start-attribution-status)
        :request-finish-attribution-status
        (plist-get request :provider-request-finish-attribution-status)
        :request-finish-status (plist-get request :provider-request-finish-status)
        :usage-attribution-status (plist-get request :usage-attribution-status)
        :native-request-sha256 (plist-get request :native-request-sha256)
        :native-history-wire-item-sha256
        (plist-get request :native-item-sha256)
        :native-history-semantic-item-sha256
        (plist-get request :native-semantic-history-item-sha256)
        :native-history-semantic-sha256
        (plist-get request :native-semantic-history-sha256)
        :input-tokens (plist-get request :input-tokens)
        :context-input-tokens (plist-get request :context-input-tokens)
        :cached-input-tokens (plist-get request :cached-input-tokens)
        :cache-creation-input-tokens
        (plist-get request :cache-creation-input-tokens)
        :output-tokens (plist-get request :output-tokens)
        :reasoning-output-tokens (plist-get request :reasoning-output-tokens)
        :total-tokens (plist-get request :total-tokens)
        :provider-total-input-tokens
        (plist-get request :provider-total-input-tokens)
        :usage-provenance (plist-get request :usage-provenance)
        :complete-request-join-p
        (e-cache-efficiency-e2e--p3-request-provenance-valid-p request)))

(defun e-cache-efficiency-e2e--p3-request-provenance-valid-p (request)
  "Return non-nil when REQUEST has unique successful start, finish, and usage joins."
  (and (stringp (plist-get request :provider-request-id))
       (not (string-empty-p (plist-get request :provider-request-id)))
       (integerp (plist-get request :provider-request-ordinal))
       (> (plist-get request :provider-request-ordinal) 0)
       (stringp (plist-get request :provider-request-turn-id))
       (eq (plist-get request :provider-request-start-attribution-status)
           'matched)
       (eq (plist-get request :provider-request-finish-attribution-status)
           'matched)
       (eq (plist-get request :provider-request-finish-status) 'done)
       (eq (plist-get request :usage-attribution-status) 'ordinal)
       (not (plist-member request :unattributed-provider-request-start-events))
       (not (plist-member request :unattributed-provider-request-finish-events))
       (not (plist-member request :unattributed-usage-provenance-events))
       (numberp (plist-get request :cached-input-tokens))
       (numberp (e-live-e2e--cache-total-input-tokens request))
       (<= (plist-get request :cached-input-tokens)
           (e-live-e2e--cache-total-input-tokens request))))

(defun e-cache-efficiency-e2e--p3-classifier-evidence
    (requests packages response-events consumed-events &optional adapter)
  "Build one chronological, identity-preserving stream from all P3 evidence."
  (let* ((provider-id (if (eq adapter 'anthropic)
                          'anthropic
                        (or (plist-get (car requests) :provider) 'unavailable)))
         (baseline (seq-find
                    (lambda (request)
                      (eq (plist-get request :user-scenario)
                          'stable-only-baseline))
                    requests))
         (control (seq-filter
                   (lambda (request)
                     (eq (plist-get request :user-scenario)
                         'pre-curation-append-only))
                   requests))
         (control-fingerprint
          (e-cache-efficiency-e2e--p3-prefix-fingerprint control))
         (phases (make-hash-table :test #'equal))
         (history-fingerprints (make-hash-table :test #'equal))
         (curation-id-by-request (make-hash-table :test #'equal))
         (curation-by-consumer (make-hash-table :test #'equal))
         (unjoined-curations nil))
    (when baseline
      (puthash (plist-get baseline :provider-request-id)
               'stable-only-baseline phases))
    (dolist (request control)
      (puthash (plist-get request :provider-request-id)
               'pre-curation-append-only phases)
      (puthash (plist-get request :provider-request-id)
               control-fingerprint history-fingerprints))
    (dolist (request requests)
      (when (memq (plist-get request :user-scenario)
                  '(curation-keep-one curation-keep-two
                    curation-summary curation-erase))
        (puthash (plist-get request :provider-request-id)
                 'curation-interim phases)))
    (let ((current-history-fingerprint control-fingerprint)
          (warm-request (car (last control)))
          (warm-group control))
      (dolist (package packages)
        (let* ((boundary
                (e-cache-efficiency-e2e--p3-fresh-request-after-curation
                 requests package response-events consumed-events))
               (fresh-turn (plist-get boundary :turn-id))
               (fresh-group (plist-get boundary :fresh-group))
               (fresh-fingerprint
                (e-cache-efficiency-e2e--p3-prefix-fingerprint fresh-group))
               (curation-id
                (e-cache-efficiency-e2e--p3-package-curation-id package))
               (projection
                (e-cache-efficiency-e2e--p3-package-projection package))
               (consumer (plist-get boundary :consumer-request))
               (replay (plist-get boundary :replay-request))
               (post-consumer-requests
                (plist-get boundary :post-consumer-requests))
               (protected
                (and warm-request
                     (e-cache-efficiency-e2e--p3-protected-prefix
                      warm-request
                      (e-cache-efficiency-e2e--p3-shared-prefix-items warm-group))))
               (projection-boundary
                (and (memq projection '(summary erase))
                     (plist-get boundary :request)
                     (e-cache-efficiency-e2e--p3-projection-boundary
                      package consumer (plist-get boundary :request)
                      projection)))
               (join-complete
                (and (eq (plist-get boundary :join-status) 'unique-complete)
                     protected
                     (e-cache-efficiency-e2e--p3-request-provenance-valid-p
                      consumer)
                     (e-cache-efficiency-e2e--p3-request-provenance-valid-p
                      replay)
                     (cl-every
                      #'e-cache-efficiency-e2e--p3-request-provenance-valid-p
                      fresh-group)
                     (or (not (memq projection '(summary erase)))
                         projection-boundary)))
               (event
                (append
                 (list :type 'curation
                       :accepted (and join-complete t)
                       :join-status (if join-complete
                                        'unique-complete 'incomplete)
                       :curation-id curation-id
                       :projection-change projection
                       :before-history-suffix-fingerprint
                       current-history-fingerprint
                       :protected-history-prefix protected
                       :projection-boundary-evidence projection-boundary
                       :response-entry-id
                       (plist-get boundary :response-entry-id)
                       :consumer-request-id
                       (plist-get boundary :consumer-request-id)
                       :consumed-frame-id (plist-get boundary :frame-id)
                       :turn-id (plist-get boundary :curation-turn-id)
                       :fresh-request-id
                       (plist-get (plist-get boundary :request)
                                  :provider-request-id)
                       :fresh-turn-id fresh-turn
                       :replay-request-id
                       (plist-get boundary :replay-request-id)
                       :source-observation-ids
                       (vconcat (plist-get boundary
                                           :source-observation-ids)))
                 (and consumer
                      (e-cache-efficiency-e2e--p3-route-evidence
                       consumer provider-id)))))
          (if (and consumer (plist-get boundary :consumer-request-id))
              (puthash (plist-get boundary :consumer-request-id)
                       event curation-by-consumer)
            (push event unjoined-curations))
          (when (and replay (plist-get replay :provider-request-id))
            (puthash (plist-get replay :provider-request-id)
                     'curation-replay phases)
            (puthash (plist-get replay :provider-request-id)
                     curation-id curation-id-by-request))
          (dolist (request (cdr post-consumer-requests))
            (puthash (plist-get request :provider-request-id)
                     'curation-followup phases)
            (puthash (plist-get request :provider-request-id)
                     curation-id curation-id-by-request))
          (when fresh-group
            (cl-loop for request in fresh-group
                     for index from 0
                     for request-id = (plist-get request :provider-request-id)
                     do (puthash request-id
                                 (if (= index 0)
                                     'rebuilt-history
                                   'post-curation-reuse)
                                 phases)
                     do (puthash request-id curation-id
                                 curation-id-by-request)
                     do (puthash request-id fresh-fingerprint
                                 history-fingerprints))
            (setq warm-request (car (last fresh-group))
                  warm-group fresh-group))
          (setq current-history-fingerprint fresh-fingerprint)))
    (let (evidence)
      (dolist (request requests)
        (let* ((request-id (plist-get request :provider-request-id))
               (phase (gethash request-id phases 'unexpected))
               (curation-id (gethash request-id curation-id-by-request))
               (history-fingerprint
                (gethash request-id history-fingerprints))
               (event
                (append
                 (list :type 'usage :phase phase
                       :after-curation-id curation-id
                       :unchanged-history-suffix-fingerprint
                       (or (plist-get request
                                      :p3-history-suffix-fingerprint)
                           history-fingerprint))
                 (e-cache-efficiency-e2e--p3-route-evidence
                  request provider-id))))
          (push event evidence))
        (when-let* ((curation
                     (gethash (plist-get request :provider-request-id)
                              curation-by-consumer)))
          (push curation evidence)))
      (nreverse (append evidence (nreverse unjoined-curations)))))))

(defun e-cache-efficiency-e2e--p3-classifier-usage (request)
  "Return REQUEST's joined provider usage fields for the P3 classifier."
  request)

(defun e-cache-efficiency-e2e--p3-live-candidate-route ()
  "Return the exact supported P3 candidate route selected by E_E2E_CONFIG."
  (let ((config (file-truename (e-live-e2e--config-file))))
    (cond
     ((equal config
             (file-truename
              (expand-file-name "e2e/e-e2e-config-openai-codex.el"
                                default-directory)))
      (let* ((provider 'codex)
             (profile (e-openai-provider-profile provider))
             (base-url (e-openai-provider-base-url profile))
             (model "gpt-6-sol")
             (options (list :model model :responses-transport 'websocket
                            :context-lifetime-enabled t))
             (route (list :base-url base-url))
             (identity
              (e-cache-efficiency-e2e--p3-route-identity
               provider options route)))
        (unless (and (e-openai-profile-codex-session-affinity-supported-p
                      profile base-url
                      (e-openai-provider-wire-api profile))
                     (eq (e-openai-profile-responses-transport profile)
                         'websocket)
                     (equal (e-openai-provider-model profile model) model)
                     (plist-get profile :requires-openai-auth))
          (ert-skip "The selected Codex route does not match the P3 candidate."))
        (unless (e-live-e2e--profile-auth-available-p profile)
          (ert-skip "Codex-managed authentication is unavailable."))
        (list :adapter 'openai :provider provider :profile profile
              :base-url base-url :model model :options options :route route
              :identity identity)))
     ((equal config
             (file-truename
              (expand-file-name "e2e/e-e2e-config-anthropic-p3.el"
                                default-directory)))
      (let* ((provider e-anthropic-default-provider)
             (profile (e-anthropic-provider-profile provider))
             (base-url (e-anthropic--provider-base-url profile))
             (model (e-anthropic--provider-model profile nil))
             (options (list :model model :context-lifetime-enabled t))
             (route (list :base-url base-url :model model
                          :requester 'messages-http-start))
             (identity
              (e-cache-efficiency-e2e--p3-route-identity
               provider options route))
             (env-key (plist-get profile :env-key)))
        (unless (and (equal model "claude-opus-5-5")
                     (eq (plist-get profile :auth) 'bearer)
                     (equal env-key "ENG_AI_MODEL_GW_KEY")
                     (e-anthropic--context-capability-supported-p identity))
          (ert-skip "The selected Messages route does not match the P3 candidate."))
        (unless (and (stringp env-key)
                     (not (string-empty-p (or (getenv env-key) ""))))
          (ert-skip "Gateway authentication is unavailable."))
        (list :adapter 'anthropic :provider provider :profile profile
              :base-url base-url :model model :options options :route route
              :identity identity)))
     (t
      (ert-skip
       "Select the dedicated P3 Opus 5.5 or managed Codex candidate config.")))))

(defun e-cache-efficiency-e2e--p3-start-payloads (events)
  "Return provider request start payloads from chronological EVENTS."
  (mapcar
   (lambda (event)
     (let* ((payload (copy-tree (plist-get event :payload) t))
            (turn-id (or (plist-get event :turn-id)
                         (plist-get payload :turn-id))))
       (when turn-id
         (setq payload (plist-put payload :turn-id turn-id)))
       payload))
   events))

(defun e-cache-efficiency-e2e--p3-requests-for-scenario-turns
    (requests scenario)
  "Return the last native request for each top-level turn labeled SCENARIO."
  (let ((turns (delete-dups
                (mapcar (lambda (request)
                          (plist-get request :provider-request-turn-id))
                        (seq-filter (lambda (request)
                                      (eq (plist-get request :user-scenario)
                                          scenario))
                                    requests)))))
    (delq nil
          (mapcar (lambda (turn-id)
                    (car (last
                          (seq-filter
                           (lambda (request)
                             (equal (plist-get request :provider-request-turn-id)
                                    turn-id))
                           (seq-filter
                            (lambda (request)
                              (eq (plist-get request :user-scenario) scenario))
                            requests)))))
                  turns))))

(ert-deftest e-cache-efficiency-e2e-test-p3-public-sql-lifecycle-and-usage-joins ()
  "The public SQLite chat path commits keep, summary, and erase transitions."
  (let* ((request-count 0)
         (request-records nil)
         (anthropic-request-records nil)
         (openai-requests-by-session (make-hash-table :test #'equal))
         (anthropic-requests-by-session (make-hash-table :test #'equal))
         (activity-events nil)
         (stable-fingerprint
          (secure-hash 'sha256 "P3 isolated stable guidance"))
         (action-specs
          '(("P3-ACTION-KEEP-ONE" "source-one" keep-one)
            ("P3-ACTION-KEEP-TWO" "source-two" keep-two)
            ("P3-ACTION-SUMMARY" "source-three" summary)
            ("P3-ACTION-ERASE" "source-four" erase)))
         (openai-provider 'codex)
         (openai-profile (e-openai-provider-profile openai-provider))
         (openai-base-url (e-openai-provider-base-url openai-profile))
         (openai-native-options
          (list :model "gpt-6-sol"
                :responses-transport
                (e-openai-profile-responses-transport openai-profile)
                :session-id "p3-sql-lifecycle"
                :context-lifetime-enabled t))
         (openai-identity
          (e-openai-profile--context-capability-identity
           openai-profile openai-native-options
           :provider openai-provider :base-url openai-base-url))
         (anthropic-provider 'p3-synthetic)
         (anthropic-base-url "https://anthropic.example.test/v1")
         (anthropic-model "claude-opus-5-5")
         (process-environment
          (cons "E98_P3_SYNTHETIC_TOKEN=synthetic-only" process-environment))
         (e-anthropic-model-providers
          `((p3-synthetic :name "P3 synthetic Anthropic"
             :base-url ,anthropic-base-url :auth bearer :auth-header x-api-key
             :env-key "E98_P3_SYNTHETIC_TOKEN"
             :default-model ,anthropic-model)))
         (anthropic-native-options
          (list :model anthropic-model :session-id "p3-sql-lifecycle"
                :context-lifetime-enabled t))
         (anthropic-identity
          (e-anthropic--context-capability-identity
           anthropic-provider anthropic-native-options
           :base-url anthropic-base-url :model anthropic-model
           :requester 'messages-http-start))
         (e-cache-efficiency-e2e--p3-candidate-grant-count 0)
         (e-cache-efficiency-e2e--p3-response-bindings nil)
         (backend
          (e-backend-create
           :name "p3-synthetic-evidence"
           :context-capabilities
           '(:continuation none
             :observation-delivery request-local-replaceable
             :reserved-effect-carrier context-curate-wire)
           :stream
           (cl-function
            (lambda (&key messages options on-item &allow-other-keys)
              (let* ((prompt (e-cache-efficiency-e2e--p3-last-user-text
                              messages))
                     (action
                      (seq-find
                       (lambda (spec)
                         (and (stringp prompt)
                              (string-match-p (regexp-quote (car spec)) prompt)))
                       action-specs))
                     (source-key (nth 1 action))
                     (operation (nth 2 action))
                     (tool-call-id (and source-key
                                        (concat "call_"
                                                (secure-hash 'sha256 source-key))))
                     (curation-call-id
                      (and source-key
                           (format "p3-curation-%s" source-key)))
                     (printed (prin1-to-string messages))
                     (source-called-p
                      (and tool-call-id
                           (string-match-p (regexp-quote tool-call-id) printed)))
                     (curation-returned-p
                      (and curation-call-id
                           (string-match-p (regexp-quote curation-call-id)
                                           printed)))
                     (openai-options (copy-sequence options))
                     (anthropic-options (copy-sequence options)))
              (setq openai-options
                    (plist-put openai-options :model "gpt-6-sol")
                    openai-options
                    (plist-put openai-options :session-id "p3-sql-lifecycle")
                    openai-options
                    (plist-put openai-options :context-lifetime-enabled t)
                    anthropic-options
                    (plist-put anthropic-options :model anthropic-model)
                    anthropic-options
                    (plist-put anthropic-options :session-id "p3-sql-lifecycle")
                    anthropic-options
                    (plist-put anthropic-options :context-lifetime-enabled t))
              (e-cache-efficiency-e2e--capture-p3-native-request
               'openai (symbol-function 'e-openai--request-context)
               openai-requests-by-session
               (list :provider openai-provider :base-url openai-base-url
                     :messages messages :options openai-options))
              (e-cache-efficiency-e2e--capture-p3-native-request
               'anthropic (symbol-function 'e-anthropic--request-context)
               anthropic-requests-by-session
               (list :provider anthropic-provider
                     :base-url anthropic-base-url :model anthropic-model
                     :requester 'messages-http-start
                     :messages messages :options anthropic-options))
              (setq request-count (1+ request-count))
              (setq request-records
                    (append request-records
                            (list (car (last
                                        (gethash "p3-sql-lifecycle"
                                                 openai-requests-by-session)))))
                    anthropic-request-records
                    (append anthropic-request-records
                            (list (car (last
                                        (gethash "p3-sql-lifecycle"
                                                 anthropic-requests-by-session))))))
              ;; These fixture counters exist only to prove provenance joins;
              ;; a synthetic provider never claims cache benefit.
              (funcall on-item
                       (list :type 'token-usage
                             :usage (list :input-tokens
                                          (+ 4096 (* request-count 1024))
                                          :context-input-tokens
                                          (+ 4096 (* request-count 1024))
                                          :cached-input-tokens 0
                                          :cache-creation-input-tokens 0)))
              (cond
               ((and action (not source-called-p))
                (funcall on-item
                         (list :type 'tool-call
                               :id tool-call-id
                               :name "read_p3_source"
                               :arguments (list :source-key source-key)))
                (funcall on-item '(:type done :reason tool-use)))
               ((and action source-called-p (not curation-returned-p))
                (funcall
                 on-item
                 (e-openai-decoder--event-item
                  (list :type "response.output_item.done"
                        :item (list :type "function_call"
                                    :call_id curation-call-id
                                    :name "context-curate"
                                    :arguments (e-json-serialize
                  (pcase operation
                    ((or 'keep-one 'keep-two)
                     '(:keep [2] :summaries [] :erase []))
                    ('summary
                     '(:keep []
                       :summaries [(:sources [2]
                                    :text "P3-SUMMARY-THREE")]
                       :erase []))
                    ('erase '(:keep [] :summaries [] :erase [2]))))))))
                (funcall on-item '(:type done :reason stop)))
               ((string-match-p "P3-BASELINE" (or prompt ""))
                (funcall on-item
                         '(:type assistant-message :content "P3-BASELINE"))
                (funcall on-item '(:type done :reason stop)))
               (t
                (funcall on-item
                         (list :type 'assistant-message
                               :content (format "P3-ANSWER-%s"
                                                (or prompt "UNKNOWN"))))
                (funcall on-item '(:type done :reason stop)))))))))
         (stable-provider
          (e-context-provider-create
           :name 'p3-stable-guidance :cache-placement 'stable-context
           :build (lambda (&rest _)
                    '((:role system :content "P3 isolated stable guidance")))))
         (dynamic-provider
          (e-context-provider-create
           :name 'p3-current-state :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    '((:role system :content "P3 isolated current state")))))
         (capability
          (e-capability-create
           :id 'p3-sql-lifecycle
           :instructions "Use read_p3_source when asked to inspect a source."
           :context-providers (list stable-provider dynamic-provider)
           :tools (list #'e-cache-efficiency-e2e--p3-register-source-tool)))
         (harness
          (e-chat-sql-e2e-make-harness
           :backend backend :intrinsic-capabilities (list capability)))
         (session-id
          (e-chat-sql-e2e-create-session harness :id "p3-sql-lifecycle"))
         (subscription
          (e-harness-activity-subscribe
           harness (lambda (event) (push (copy-tree event t) activity-events))
           :session-id session-id)))
    (unwind-protect
        (let ((e-context-lifetime-shadow-projection-enabled t))
          (cl-letf (((symbol-function 'e-openai-codex-read-auth)
                     (lambda (&optional _auth-file)
                       '(:tokens (:access_token "synthetic-only"))))
                    ((symbol-function 'e-openai-codex-auth-account-id)
                     (lambda (_auth) "synthetic-account")))
            (e-cache-efficiency-e2e--with-p3-candidate-capability
             openai-identity
             (lambda ()
               (e-cache-efficiency-e2e--with-p3-candidate-capability
                anthropic-identity
                (lambda ()
                  (e-cache-efficiency-e2e--p3-with-response-capture
                   (lambda ()
                  (cl-loop for prompt in
                           '("P3-BASELINE"
                             "P3-ACTION-KEEP-ONE read source-one and keep its exact observed fact"
                             "P3-FRESH-KEEP-ONE-A" "P3-FRESH-KEEP-ONE-B"
                             "P3-ACTION-KEEP-TWO read source-two and keep its exact observed fact"
                             "P3-FRESH-KEEP-TWO-A" "P3-FRESH-KEEP-TWO-B"
                             "P3-ACTION-SUMMARY read source-three and remove the RAW- marker from its text before replacing it"
                             "P3-FRESH-SUMMARY-ONE" "P3-FRESH-SUMMARY-TWO"
                             "P3-ACTION-ERASE read source-four and erase the full observed source"
                             "P3-FRESH-ERASE-ONE" "P3-FRESH-ERASE-TWO")
                           do (let ((result
                                     (e-chat-sql-e2e-prompt-batch
                                      harness session-id prompt 20.0)))
                                (should (eq (plist-get result :status) 'done))
                                (if (equal prompt "P3-BASELINE")
                                    (should (equal
                                             (e-live-e2e--assistant-content result)
                                             "P3-BASELINE"))
                                  (should (string-match-p
                                           (regexp-quote prompt)
                                           (e-live-e2e--assistant-content
                                            result))))))))))))
          (e-session-flush-write-queue (e-harness-sessions harness))
          (let* ((events (nreverse activity-events))
                 (starts (e-live-e2e--events-of-type
                          events 'provider-request-started))
                 (finished (e-live-e2e--events-of-type
                            events 'provider-request-finished))
                 (usage-events (e-live-e2e--events-of-type events 'token-usage))
                 (curation-events
                  (e-live-e2e--activity-of-type
                   harness session-id 'context-curation-response))
                 (start-payloads (e-cache-efficiency-e2e--p3-start-payloads starts))
                 (usage-payloads (mapcar (lambda (event)
                                           (plist-get event :payload))
                                         usage-events))
                 (finish-payloads (mapcar (lambda (event)
                                            (plist-get event :payload))
                                          finished))
                 (joined-starts
                  (e-cache-efficiency-e2e--join-request-starts
                   request-records start-payloads))
                 (joined-finishes
                  (e-cache-efficiency-e2e--p3-join-request-finishes
                   joined-starts finish-payloads))
                 (joined
                  (e-cache-efficiency-e2e--join-request-usage
                   joined-finishes usage-payloads))
                 (anthropic-joined-starts
                  (e-cache-efficiency-e2e--join-request-starts
                   anthropic-request-records start-payloads))
                 (anthropic-joined-finishes
                  (e-cache-efficiency-e2e--p3-join-request-finishes
                   anthropic-joined-starts finish-payloads))
                 (anthropic-joined
                  (e-cache-efficiency-e2e--join-request-usage
                   anthropic-joined-finishes usage-payloads))
                 (store (e-harness-sessions harness))
                 (curations
                  (mapcar
                   (lambda (record) (plist-get record :value))
                   (plist-get
                    (e-chat-sql-e2e--await
                     (e-session-async-record-page
                      store session-id :record-type "context-curation-package"
                      :limit 20))
                    :records))))
            (should (= request-count (length request-records)))
            (should (= (length starts) request-count))
            (should (= (length finished) request-count))
            (should (= (length usage-events) request-count))
            (should (= (length anthropic-request-records) request-count))
            (should (= (length curation-events) 4))
            (should (= (length curations) 4))
            (should (= (cl-count-if (lambda (package)
                                      (plist-get package :promotion))
                                    curations)
                       3))
            (should (= (cl-count-if (lambda (package)
                                      (plist-get package :erasure))
                                    curations)
                       1))
            (let ((consumed
                   (e-cache-efficiency-e2e--p3-bound-consumed-events
                    (e-live-e2e--activity-of-type
                     harness session-id 'context-frame-consumed))))
              (should (> (length consumed) 4))
              (dolist (native-requests (list joined anthropic-joined))
                (should
                 (= (length
                     (delete-dups
                      (mapcar
                       (lambda (package)
                         (plist-get
                          (e-cache-efficiency-e2e--p3-fresh-request-after-curation
                           native-requests package curation-events consumed)
                          :consumed-event))
                       curations)))
                    4))
                (dolist (package curations)
                  (let* ((boundary
                          (e-cache-efficiency-e2e--p3-fresh-request-after-curation
                           native-requests package curation-events consumed))
                         (projection (e-cache-efficiency-e2e--p3-package-projection
                                      package)))
                    (should (eq (plist-get boundary :join-status) 'unique-complete))
                    (when (memq projection '(summary erase))
                      (let ((wrong (copy-tree (plist-get boundary :consumer-request) t)))
                        (dolist (message (plist-get wrong :messages))
                          (when (eq (plist-get message :role) 'tool-call)
                            (plist-put (plist-get message :content) :id "call_other")))
                        (should-not
                         (e-cache-efficiency-e2e--p3-projection-boundary
                          package wrong (plist-get boundary :request) projection))))))
                (let* ((events (e-cache-efficiency-e2e--p3-classifier-evidence
                                native-requests curations curation-events consumed
                                (plist-get (car native-requests) :adapter)))
                       (roundtrip (car (read-from-string (prin1-to-string events)))))
                  (should (= (length (seq-filter
                                      (lambda (event) (eq (plist-get event :type) 'usage))
                                      events)) request-count))
                  (should (= (length (seq-filter
                                      (lambda (event) (eq (plist-get event :type) 'curation))
                                      events)) 4))
                  (dolist (event (seq-filter
                                 (lambda (event)
                                   (and (eq (plist-get event :type) 'curation)
                                        (memq (plist-get event :projection-change)
                                              '(summary erase))))
                                 events))
                    (should (plist-get event :accepted))
                    (should (plist-get event :projection-boundary-evidence)))
                  (should (equal events roundtrip))
                  (should-not (string-match-p "P3-KEEP-FACT\\|P3-SUMMARY-RAW"
                                              (prin1-to-string events)))
                  (should-not (equal
                               (e-live-e2e--classify-p3-curation-cache-evidence
                                :carrier-available t
                                :provider-id (plist-get (car events) :provider-id)
                                :evidence roundtrip) "pass")))))
            (dolist (native-requests (list joined anthropic-joined))
              (should (e-cache-efficiency-e2e--usage-join-complete-p
                       native-requests))
              (should
               (cl-every
                (lambda (request)
                  (and (eq (plist-get request
                                      :provider-request-start-attribution-status)
                           'matched)
                       (eq (plist-get request
                                      :provider-request-finish-attribution-status)
                           'matched)
                       (eq (plist-get request :provider-request-finish-status)
                           'done)
                       (numberp (plist-get request :input-tokens))
                       (eq (plist-get request :cached-input-tokens) 0)))
                native-requests))
              (let* ((fresh-keep-one
                    (e-cache-efficiency-e2e--p3-request-for-user-marker
                     native-requests "P3-FRESH-KEEP-ONE-A"))
                   (fresh-keep-two
                    (e-cache-efficiency-e2e--p3-request-for-user-marker
                     native-requests "P3-FRESH-KEEP-TWO-A"))
                   (fresh-summary
                    (e-cache-efficiency-e2e--p3-request-for-user-marker
                     native-requests "P3-FRESH-SUMMARY-ONE"))
                   (fresh-erase
                    (e-cache-efficiency-e2e--p3-request-for-user-marker
                     native-requests "P3-FRESH-ERASE-ONE"))
                   (summary-replay
                    (seq-find
                     (lambda (request)
                       (string-match-p
                        "p3-curation-source-three"
                        (prin1-to-string (plist-get request :messages))))
                     native-requests)))
              (dolist (request (list fresh-keep-one fresh-keep-two
                                     fresh-summary fresh-erase summary-replay))
                (should request)
                (should (eq (plist-get request :usage-attribution-status)
                            'ordinal)))
              (should (e-cache-efficiency-e2e--p3-native-projection-valid-p
                       fresh-keep-one "P3-KEEP-FACT-ONE" nil))
              (should (e-cache-efficiency-e2e--p3-native-projection-valid-p
                       fresh-keep-two "P3-KEEP-FACT-TWO" nil))
              (should (e-cache-efficiency-e2e--p3-request-contains-text-p
                       summary-replay "P3-SUMMARY-RAW-THREE"))
              (should (e-cache-efficiency-e2e--p3-native-projection-valid-p
                       fresh-summary "P3-SUMMARY-THREE"
                       '("P3-SUMMARY-RAW-THREE")))
              (should (e-cache-efficiency-e2e--p3-native-projection-valid-p
                       fresh-erase nil '("P3-ERASE-RAW-FOUR")))
              (should (cl-every
                       (lambda (request)
                         (plist-get request :native-semantic-history-item-sha256))
                       native-requests))
              (let* ((stale-summary (copy-tree fresh-summary t))
                     (missing-keep (copy-tree fresh-keep-one t)))
                (setf (plist-get stale-summary :native-body)
                      (e-cache-efficiency-e2e--p3-replace-native-text
                       (plist-get stale-summary :native-body)
                       "P3-SUMMARY-THREE" "P3-SUMMARY-THREE P3-SUMMARY-RAW-THREE"))
                (setf (plist-get missing-keep :native-body)
                      (e-cache-efficiency-e2e--p3-replace-native-text
                       (plist-get missing-keep :native-body)
                       "P3-KEEP-FACT-ONE" "P3-MISSING-KEEP-FACT"))
                (should (string-match-p
                         "P3-KEEP-FACT-ONE"
                         (prin1-to-string (plist-get missing-keep :messages))))
                (should-not
                 (e-cache-efficiency-e2e--p3-native-projection-valid-p
                  stale-summary "P3-SUMMARY-THREE"
                  '("P3-SUMMARY-RAW-THREE")))
                (should-not
                 (e-cache-efficiency-e2e--p3-native-projection-valid-p
                  missing-keep "P3-KEEP-FACT-ONE" nil)))
              (e-cache-efficiency-e2e--report-evidence
               (list :scenario "p3-synthetic-public-sql-lifecycle"
                     :native-shape (plist-get (car native-requests) :adapter)
                     :semantic-result "pass"
                     :candidate-authority "ert-exact-route-wrapper"
                     :carrier-available t
                     :production-qualification "unavailable"
                     :request-count (length native-requests)
                     :request-start-count (length starts)
                     :request-finish-count (length finished)
                     :usage-event-count (length usage-events)
                     :curation-event-count (length curation-events)
                     :accepted-package-count (length curations)
                     :summary-same-turn-replay-request-id
                     (plist-get summary-replay :provider-request-id)
                     :summary-fresh-request-id
                     (plist-get fresh-summary :provider-request-id)
                     :summary-fresh-request-sha256
                     (plist-get fresh-summary :native-request-sha256)
                     :summary-fresh-semantic-history-sha256
                     (plist-get fresh-summary
                                :native-semantic-history-sha256))))))
      (e-harness-activity-unsubscribe harness subscription)
      (e-chat-sql-e2e-reset))))))

(defun e-cache-efficiency-e2e--p3-measured-result
    (semantic-result condition carrier-available provider evidence)
  "Keep any caught CONDITION authoritative over candidate EVIDENCE."
  (if (and (null condition) (equal semantic-result "pass"))
      (e-live-e2e--classify-p3-curation-cache-evidence
       :carrier-available carrier-available :provider-id provider :evidence evidence)
    "unconfirmed-semantic-evidence"))

(ert-deftest e-cache-efficiency-e2e-test-p3-late-failure-prevents-positive ()
  "Complete warm evidence cannot override an incorrect final answer."
  (let ((evidence (e-live-e2e-test--p3-cache-evidence)))
    (should (equal (e-cache-efficiency-e2e--p3-measured-result
                    "pass" nil t 'openai evidence) "pass"))
    (should (equal (e-cache-efficiency-e2e--p3-measured-result
                    "pass" '(ert-test-failed "wrong OK marker") t 'openai evidence)
                   "unconfirmed-semantic-evidence"))))

(defun e-cache-efficiency-e2e--run-p3-live-native (authority)
  "Run P3 through the ordinary public chat path using AUTHORITY.
AUTHORITY is either the existing ERT-only wrapper or the normal candidate
source.  Both runs share the same prompts and evidence checks."
  (e-live-e2e--require-enabled)
  (when (and (eq authority 'normal-source)
             (not (equal
                   (file-truename (e-live-e2e--config-file))
                   (file-truename
                    (expand-file-name
                     "e2e/e-e2e-config-anthropic-p3.el" default-directory)))))
    (ert-skip "Normal-source P3 is admitted only for gateway Opus Messages."))
  (let* ((route (e-cache-efficiency-e2e--p3-live-candidate-route))
         (adapter (plist-get route :adapter))
         (identity (plist-get route :identity))
         (normal-source-carrier
          (and (eq authority 'normal-source)
               (eq (plist-get
                    (e-cache-efficiency-e2e--p3-route-capabilities
                     (plist-get route :provider)
                     (plist-get route :options)
                     (plist-get route :route))
                    :reserved-effect-carrier)
                   'context-curate-wire)))
         (layer (e-cache-efficiency-e2e--p3-live-layer))
         (started-at (float-time))
         (deadline (+ started-at
                      (e-live-e2e--positive-number-env
                       "E_E2E_P3_TIMEOUT_SECONDS" 900.0)))
         (requests-by-session (make-hash-table :test #'equal))
         (e-cache-efficiency-e2e--p3-candidate-grant-count 0)
         (e-cache-efficiency-e2e--p3-response-bindings nil)
         (semantic-result "unavailable")
         (condition nil))
    (e-live-e2e--with-harness (harness session-id :layers (list layer))
      (let ((e-context-lifetime-shadow-projection-enabled t)
            (native (if (eq adapter 'anthropic)
                        (symbol-function 'e-anthropic--request-context)
                      (symbol-function 'e-openai--request-context)))
            (run
             (lambda ()
               (dolist (prompt
                        '("P3-STABLE-BASELINE. Reply exactly P3-BASELINE-OK."
                          "P3-APPEND-CONTROL-ONE. Call cache_probe once with step 1; do not curate the probe result. Reply P3-CONTROL-ONE-OK."
                          "P3-APPEND-CONTROL-TWO. Call cache_probe once with step 1; do not curate the probe result. Reply P3-CONTROL-TWO-OK."
                          "P3-CURATE-KEEP-ONE. Read source-key source-one with read_p3_source. Keep the exact observed fact for its returned label, with empty summaries and erase. Reply P3-KEEP-ONE-OK."
                          "P3-REUSE-KEEP-ONE-A. Call cache_probe once with step 1; do not curate the probe result. Reply P3-REUSE-KEEP-ONE-A-OK."
                          "P3-REUSE-KEEP-ONE-B. Call cache_probe once with step 2; do not curate the probe result. Reply P3-REUSE-KEEP-ONE-B-OK."
                          "P3-CURATE-KEEP-TWO. Read source-key source-two with read_p3_source. Keep the exact observed fact for its returned label, with empty summaries and erase. Reply P3-KEEP-TWO-OK."
                          "P3-REUSE-KEEP-TWO-A. Call cache_probe once with step 1; do not curate the probe result. Reply P3-REUSE-KEEP-TWO-A-OK."
                          "P3-REUSE-KEEP-TWO-B. Call cache_probe once with step 2; do not curate the probe result. Reply P3-REUSE-KEEP-TWO-B-OK."
                          "P3-CURATE-SUMMARY. Read source-key source-three with read_p3_source. Replace its observed source with one summary for the returned label; remove the RAW- marker from the source text and use the remaining text exactly. Reply P3-SUMMARY-OK."
                          "P3-REUSE-SUMMARY-A. Call cache_probe once with step 1; do not curate the probe result. Reply P3-REUSE-SUMMARY-A-OK."
                          "P3-REUSE-SUMMARY-B. Call cache_probe once with step 2; do not curate the probe result. Reply P3-REUSE-SUMMARY-B-OK."
                          "P3-CURATE-ERASE. Read source-key source-four with read_p3_source. Erase only the returned source label, with empty keep and summaries. Reply P3-ERASE-OK."
                          "P3-REUSE-ERASE-A. Call cache_probe once with step 1; do not curate the probe result. Reply P3-REUSE-ERASE-A-OK."
                          "P3-REUSE-ERASE-B. Call cache_probe once with step 2; do not curate the probe result. Reply P3-REUSE-ERASE-B-OK."))
                 (let* ((prompt (e-cache-efficiency-e2e--p3-durable-prompt
                                 prompt))
                        (result
                         (e-live-e2e--prompt-batch-before-deadline
                          harness session-id prompt deadline))
                        (answer (e-live-e2e--assistant-content result))
                        (marker
                         (and (string-match
                               "\\(P3-[A-Z0-9-]+-OK\\)" prompt)
                              (match-string 1 prompt))))
                   (should (eq (plist-get result :status) 'done))
                   (should marker)
                   (should (string-match-p (regexp-quote marker) answer)))))))
        (condition-case caught
            (let ((capture-and-run
                   (lambda ()
                     (if (eq adapter 'anthropic)
                         (cl-letf (((symbol-function 'e-anthropic--request-context)
                                    (lambda (&rest args)
                                      (e-cache-efficiency-e2e--capture-p3-native-request
                                       'anthropic native requests-by-session args))))
                           (e-cache-efficiency-e2e--p3-with-response-capture run))
                       (cl-letf (((symbol-function 'e-openai--request-context)
                                  (lambda (&rest args)
                                    (e-cache-efficiency-e2e--capture-p3-native-request
                                     'openai native requests-by-session args))))
                         (e-cache-efficiency-e2e--p3-with-response-capture run))))))
              (unless (or (eq authority 'normal-source)
                          (and (eq authority 'ert-exact-route-wrapper)
                               (memq adapter '(anthropic openai))))
                (error "Unsupported P3 source authority: %S" authority))
              (when (eq authority 'normal-source)
                (unless (eq adapter 'anthropic)
                  (error "Normal-source P3 only supports the admitted Messages route")))
              (e-cache-efficiency-e2e--p3-run-with-authority
               authority identity normal-source-carrier capture-and-run))
          (error (setq condition caught)))
        (e-session-flush-write-queue (e-harness-sessions harness))
        (let* ((captured (gethash session-id requests-by-session))
               (starts (e-live-e2e--activity-of-type
                        harness session-id 'provider-request-started))
               (finishes (e-live-e2e--activity-of-type
                          harness session-id 'provider-request-finished))
               (usage-events (e-live-e2e--activity-of-type
                              harness session-id 'token-usage))
               (response-events (e-live-e2e--activity-of-type
                                 harness session-id 'context-curation-response))
               (consumed-events
                (e-cache-efficiency-e2e--p3-bound-consumed-events
                 (e-live-e2e--activity-of-type
                  harness session-id 'context-frame-consumed)))
               (start-payloads
                (e-cache-efficiency-e2e--p3-start-payloads starts))
               (finish-payloads (mapcar (lambda (event)
                                          (plist-get event :payload))
                                        finishes))
               (usage-payloads (mapcar (lambda (event)
                                         (plist-get event :payload))
                                       usage-events))
               (joined-starts
                (e-cache-efficiency-e2e--join-request-starts
                 captured start-payloads))
               (joined-finishes
                (e-cache-efficiency-e2e--p3-join-request-finishes
                 joined-starts finish-payloads))
               (joined
                (e-cache-efficiency-e2e--join-request-usage
                 joined-finishes usage-payloads))
               (store (e-harness-sessions harness))
               (page (e-chat-sql-e2e--await
                      (e-session-async-record-page
                       store session-id :record-type "context-curation-package"
                       :order 'oldest :limit 100)))
               (packages (mapcar (lambda (record) (plist-get record :value))
                                 (plist-get page :records)))
               (fresh-boundaries
                (mapcar (lambda (package)
                          (e-cache-efficiency-e2e--p3-fresh-request-after-curation
                           joined package response-events consumed-events))
                        packages))
               (projections
                (mapcar #'e-cache-efficiency-e2e--p3-package-projection
                        packages))
               (classifier-evidence
                (e-cache-efficiency-e2e--p3-classifier-evidence
                 joined packages response-events consumed-events adapter))
               (carrier-available
                (and (if (eq authority 'normal-source)
                         normal-source-carrier
                       (> e-cache-efficiency-e2e--p3-candidate-grant-count 0))
                     captured
                     (cl-every (lambda (request)
                                 (plist-get request
                                            :context-curation-tool-present))
                               captured)))
               (provider-id (if (eq adapter 'anthropic)
                                'anthropic
                              (plist-get route :provider)))
               (candidate-efficiency-result "unconfirmed")
               (semantic-error condition))
          (condition-case caught
              (progn
                (should (= (length packages) 4))
                (should (equal projections '(keep keep summary erase)))
                (should (= (length response-events) 4))
                (should (= (length fresh-boundaries) 4))
                (should (cl-every #'identity fresh-boundaries))
                (should (= (length
                            (delete-dups
                             (mapcar (lambda (boundary)
                                       (plist-get boundary :consumed-event))
                                     fresh-boundaries)))
                           4))
                (should (cl-some
                         (lambda (item)
                           (and (eq (plist-get item :kind) 'exact)
                                (string-match-p
                                 "P3-KEEP-FACT-ONE"
                                (format "%s" (plist-get item :value)))))
                         (append (plist-get (plist-get (car packages) :promotion)
                                            :items)
                                 nil)))
                (should (cl-some
                         (lambda (item)
                           (and (eq (plist-get item :kind) 'exact)
                                (string-match-p
                                 "P3-KEEP-FACT-TWO"
                                (format "%s" (plist-get item :value)))))
                         (append (plist-get (plist-get (cadr packages) :promotion)
                                            :items)
                                 nil)))
                (should (string-match-p
                         "P3-SUMMARY-THREE"
                         (prin1-to-string
                          (plist-get (plist-get (nth 2 packages) :promotion)
                                     :items))))
                (should (e-cache-efficiency-e2e--p3-native-projection-valid-p
                         (plist-get (nth 0 fresh-boundaries) :request)
                         "P3-KEEP-FACT-ONE" nil))
                (should (e-cache-efficiency-e2e--p3-native-projection-valid-p
                         (plist-get (nth 1 fresh-boundaries) :request)
                         "P3-KEEP-FACT-TWO" nil))
                (should (e-cache-efficiency-e2e--p3-native-projection-valid-p
                         (plist-get (nth 2 fresh-boundaries) :request)
                         "P3-SUMMARY-THREE"
                         '("P3-SUMMARY-RAW-THREE")))
                (should (e-cache-efficiency-e2e--p3-native-projection-valid-p
                         (plist-get (nth 3 fresh-boundaries) :request)
                         nil '("P3-ERASE-RAW-FOUR")))
                (should
                 (e-cache-efficiency-e2e--p3-request-contains-text-p
                  (plist-get (nth 2 fresh-boundaries) :replay-request)
                  "P3-SUMMARY-RAW-THREE"))
                (should (= (length captured) (length starts)))
                (should (= (length captured) (length finishes)))
                (should (= (length captured) (length usage-events)))
                (should (e-cache-efficiency-e2e--usage-join-complete-p joined))
                (setq semantic-result (if semantic-error "failure" "pass")))
            (error (setq semantic-error (or semantic-error caught)
                         semantic-result "failure")))
          (setq candidate-efficiency-result
                (e-cache-efficiency-e2e--p3-measured-result
                 semantic-result semantic-error carrier-available
                 provider-id classifier-evidence))
          (e-cache-efficiency-e2e--report-evidence
           (list :scenario (if (eq authority 'normal-source)
                               "p3-live-native-normal-source"
                             "p3-live-native-candidate")
                 :repository-revision (e-live-e2e--repository-revision)
                 :timestamp (format-time-string "%FT%TZ" nil t)
                 :elapsed-seconds (- (float-time) started-at)
                 :adapter adapter :provider (plist-get route :provider)
                 :endpoint (plist-get identity :endpoint)
                 :model (plist-get route :model)
                 :api (plist-get identity :api)
                 :transport (plist-get identity :transport)
                 :requester (plist-get identity :requester)
                 :candidate-authority
                 (if (eq authority 'normal-source)
                     "normal-source-candidate"
                   "ert-exact-route-wrapper")
                 :carrier-availability-evidence
                 (if (eq authority 'normal-source)
                     "normal-owner-and-native-requests"
                   "ert-wrapper-and-native-requests")
                 :candidate-grant-count
                 e-cache-efficiency-e2e--p3-candidate-grant-count
                 :carrier-available (and carrier-available t)
                 :production-qualification
                 (if (eq authority 'normal-source)
                     "isolated-source-candidate"
                   "unavailable")
                 :semantic-result semantic-result
                 :candidate-efficiency-result candidate-efficiency-result
                 :semantic-failure
                 (and semantic-error (car semantic-error))
                 :request-count (length joined)
                 :request-start-count (length starts)
                 :request-finish-count (length finishes)
                 :usage-event-count (length usage-events)
                 :curation-response-count (length response-events)
                 :consumed-frame-count (length consumed-events)
                 :accepted-package-count (length packages)
                 :curation-projections (vconcat projections)
                 :curation-classifier-evidence (vconcat classifier-evidence)
                 :fresh-boundary-request-ids
                 (vconcat
                  (mapcar (lambda (boundary)
                            (plist-get (plist-get boundary :request)
                                       :provider-request-id))
                          fresh-boundaries))
                 :requests
                 (vconcat (mapcar
                           (lambda (request)
                             (e-cache-efficiency-e2e--p3-report-request
                              request))
                           joined))))
          (when semantic-error
            (signal (car semantic-error) (cdr semantic-error)))
          (when condition
            (signal (car condition) (cdr condition))))))))

(ert-deftest e-cache-efficiency-e2e-test-p3-live-native-candidate ()
  "Run diagnostic P3 through the private exact-route wrapper."
  (e-cache-efficiency-e2e--run-p3-live-native 'ert-exact-route-wrapper))

(ert-deftest e-cache-efficiency-e2e-test-p3-live-native-normal-source ()
  "Run the same P3 scenario through the isolated normal-source candidate."
  (e-cache-efficiency-e2e--run-p3-live-native 'normal-source))

(ert-deftest e-cache-efficiency-e2e-test-p3-live-runner-no-provider-smoke ()
  "Exercise both shared-runner authorities through cleanup without a request."
  (let ((process-environment (copy-sequence process-environment))
        (e-anthropic-default-model nil)
        (e-anthropic-default-provider nil)
        (e-anthropic-model-providers nil)
        (e-default-harness-specs nil))
    (setenv "CI" nil)
    (setenv "E_E2E" "1")
    (setenv "E_E2E_CONFIG" "e2e/e-e2e-config-anthropic-p3.el")
    (setenv "ENG_AI_MODEL_GW_KEY" "synthetic-only")
    (load (expand-file-name (getenv "E_E2E_CONFIG") default-directory)
          nil nil t)
    (let* ((route (e-cache-efficiency-e2e--p3-live-candidate-route))
           (normal-capabilities
            (e-cache-efficiency-e2e--p3-route-capabilities
             (plist-get route :provider)
             (plist-get route :options)
             (plist-get route :route))))
      (should (eq (plist-get normal-capabilities :reserved-effect-carrier)
                  'context-curate-wire))
      (dolist (authority '(normal-source ert-exact-route-wrapper))
        (let* ((run-body-count 0)
               (wrapper-call-count 0)
               (flush-report-order nil)
               (cleanup-events nil)
               (temporary-root nil)
               report-record
               (real-make-temp-file (symbol-function 'make-temp-file))
               (real-flush (symbol-function 'e-session-flush-write-queue))
               (real-unsubscribe
                (symbol-function 'e-harness-activity-unsubscribe))
               (real-store-close
                (symbol-function 'e-session-sqlite-store-close))
               (real-candidate-wrapper
                (symbol-function
                 'e-cache-efficiency-e2e--with-p3-candidate-capability)))
          (cl-letf
              (((symbol-function 'e-live-e2e--require-enabled)
                (lambda () t))
               ((symbol-function 'e-live-e2e--make-harness)
                (lambda (store)
                  (e-e2e-config-anthropic-p3-chat-harness-create
                   :sessions store)))
               ((symbol-function 'e-live-e2e--prompt-batch-before-deadline)
                (lambda (&rest _arguments)
                  (setq run-body-count (1+ run-body-count))
                  (error "P3 no-provider smoke run-body failure")))
               ((symbol-function
                 'e-cache-efficiency-e2e--with-p3-candidate-capability)
                (lambda (route-identity thunk)
                  (setq wrapper-call-count (1+ wrapper-call-count))
                  (funcall real-candidate-wrapper route-identity thunk)))
               ((symbol-function 'e-session-flush-write-queue)
                (lambda (store)
                  (setq flush-report-order
                        (append flush-report-order '(flush)))
                  (funcall real-flush store)))
               ((symbol-function 'e-cache-efficiency-e2e--report-evidence)
                (lambda (record)
                  (setq report-record record
                        flush-report-order
                        (append flush-report-order '(report)))))
               ((symbol-function 'e-harness-activity-unsubscribe)
                (lambda (harness subscription)
                  (setq cleanup-events
                        (append cleanup-events '(unsubscribe)))
                  (funcall real-unsubscribe harness subscription)))
               ((symbol-function 'e-session-sqlite-store-close)
                (lambda (store)
                  (setq cleanup-events
                        (append cleanup-events '(store-close)))
                  (funcall real-store-close store)))
               ((symbol-function 'make-temp-file)
                (lambda (prefix &optional dir-flag suffix)
                  (let ((path
                         (funcall real-make-temp-file prefix dir-flag suffix)))
                    (when (equal prefix "e-live-e2e-")
                      (setq temporary-root path))
                    path))))
            (let ((condition
                   (should-error
                    (e-cache-efficiency-e2e--run-p3-live-native authority)
                    :type 'error)))
              (should (equal (cadr condition)
                             "P3 no-provider smoke run-body failure"))
              (should (= run-body-count 1))
              (should report-record)
              (should (equal flush-report-order '(flush report)))
              (should (equal (plist-get report-record :semantic-result)
                             "failure"))
              (should (equal
                       (plist-get report-record :candidate-efficiency-result)
                       "unconfirmed-semantic-evidence"))
              (should (= (plist-get report-record :candidate-grant-count) 0))
              (should-not (plist-get report-record :carrier-available))
              (should (= (plist-get report-record :request-count) 0))
              (should (= wrapper-call-count
                         (if (eq authority 'normal-source) 0 1)))
              (should (equal cleanup-events '(unsubscribe store-close)))
              (should temporary-root)
              (should-not (file-exists-p temporary-root)))))))))

(ert-deftest e-cache-efficiency-e2e-test-codex-minimal-session-affinity ()
  "The normal Codex harness sends its ordinary identity in both wire fields."
  (e-cache-efficiency-e2e--supported-codex-profile)
  (let* ((started-at (float-time))
         (native-openai (symbol-function 'e-openai--request-context))
         (requests-by-session (make-hash-table :test 'equal))
         (semantic-result "unavailable")
         (condition nil))
    (e-live-e2e--with-harness (harness session-id)
      (unwind-protect
          (condition-case caught
              (cl-letf (((symbol-function 'e-openai--request-context)
                         (lambda (&rest args)
                           (e-cache-efficiency-e2e--capture-request-context
                            native-openai requests-by-session args))))
                (let* ((result
                        (e-live-e2e--prompt-batch-before-deadline
                         harness session-id
                         "Reply with exactly E98-CODEX-AFFINITY-OK. Do not call tools."
                         (+ started-at
                            (e-live-e2e--cache-scenario-timeout))))
                       (answer (e-live-e2e--assistant-content result))
                       (requests (gethash session-id requests-by-session)))
                  (should (string-match-p "E98-CODEX-AFFINITY-OK" answer))
                  (should requests)
                  (should
                   (cl-every
                    (lambda (request)
                      (and (plist-get request :session-affinity-identities-match)
                           (eq (plist-get request :affinity-selected) t)
                           (eq (plist-get request :affinity-body-key-present) t)
                           (eq (plist-get request :affinity-header-present) t)
                           (not (plist-get request :legacy-session-header-present))
                           (not (plist-get request :explicit-cache-controls-present))))
                    requests))
                  (setq semantic-result "pass")))
            (error (setq condition caught)))
        (let* ((requests (gethash session-id requests-by-session))
               (usage
                (mapcar (lambda (event) (plist-get event :payload))
                        (e-live-e2e--activity-of-type
                         harness session-id 'token-usage)))
               (evidence
                (e-cache-efficiency-e2e--join-request-usage requests usage)))
          (e-cache-efficiency-e2e--report-evidence
           (list :scenario "codex-session-affinity-p1-minimal"
                 :repository-revision (e-live-e2e--repository-revision)
                 :timestamp (format-time-string "%FT%TZ" nil t)
                 :elapsed-seconds (- (float-time) started-at)
                 :semantic-result semantic-result
                 :cache-result "not-measured"
                 :usage-event-count (length usage)
                 :usage-join-complete
                 (and (= (length requests) (length usage))
                      (e-cache-efficiency-e2e--usage-join-complete-p evidence))
                 :requests (vconcat evidence))))
        (when condition (signal (car condition) (cdr condition)))))))

(ert-deftest e-cache-efficiency-e2e-test-codex-repeated-session-affinity-p1-no-tool ()
  "Three ordinary no-tool turns preserve Codex affinity and usage attribution."
  (e-cache-efficiency-e2e--supported-codex-profile)
  (let* ((layer (e-cache-efficiency-e2e--codex-affinity-p1-layer))
         (started-at (float-time))
         (deadline (+ started-at (e-live-e2e--cache-scenario-timeout)))
         (native-openai (symbol-function 'e-openai--request-context))
         (requests-by-session (make-hash-table :test 'equal))
         (semantic-results nil)
         (semantic-result "unavailable")
         (affinity-result "unavailable")
         (condition nil))
    (e-live-e2e--with-harness (harness session-id :layers (list layer))
      (unwind-protect
          (condition-case caught
              (cl-letf (((symbol-function 'e-openai--request-context)
                         (lambda (&rest args)
                           (e-cache-efficiency-e2e--capture-request-context
                            native-openai requests-by-session args))))
                (dolist (marker '("E98-CODEX-P1-ONE"
                                  "E98-CODEX-P1-TWO"
                                  "E98-CODEX-P1-THREE"))
                  (let* ((prompt (format "Reply with exactly %s. Do not call tools."
                                          marker))
                         (result
                          (e-live-e2e--prompt-batch-before-deadline
                           harness session-id prompt deadline))
                         (answer (e-live-e2e--assistant-content result)))
                    (should (string-match-p (regexp-quote marker) answer))
                    (push "pass" semantic-results)))
                (setq semantic-result "pass"))
            (error (setq condition caught)))
        (let* ((captured (gethash session-id requests-by-session))
               (starts
                (mapcar (lambda (event) (plist-get event :payload))
                        (e-live-e2e--activity-of-type
                         harness session-id 'provider-request-started)))
               (requests
                (e-cache-efficiency-e2e--join-request-starts captured starts))
               (usage
                (mapcar (lambda (event) (plist-get event :payload))
                        (e-live-e2e--activity-of-type
                         harness session-id 'token-usage)))
               (evidence
                (e-cache-efficiency-e2e--join-request-usage requests usage))
               (usage-complete
                (e-cache-efficiency-e2e--usage-join-complete-p evidence)))
          (when (null condition)
            (condition-case caught
                (progn
                  (should (= (length captured) 3))
                  (should (= (length starts) 3))
                  (should (= (length usage) 3))
                  (should usage-complete)
                  (should (equal (mapcar (lambda (request)
                                           (plist-get request :request-ordinal))
                                         evidence)
                                 '(1 2 3)))
                  (should (equal (mapcar (lambda (request)
                                           (plist-get request
                                                      :provider-request-ordinal))
                                         evidence)
                                 '(1 1 1)))
                  (should (= (length
                              (delete-dups
                               (mapcar (lambda (request)
                                         (plist-get request :provider-request-id))
                                       evidence)))
                             3))
                  (should
                   (cl-every
                    (lambda (request)
                      (and (plist-get request :session-affinity-identities-match)
                           (eq (plist-get request :affinity-selected) t)
                           (eq (plist-get request :affinity-body-key-present) t)
                           (eq (plist-get request :affinity-header-present) t)
                           (not (plist-get request :legacy-session-header-present))
                           (not (plist-get request :explicit-cache-controls-present))
                           (null (plist-get request :tool-names))
                           (e-cache-efficiency-e2e--stable-guidance-record-valid-p
                            request)))
                    evidence))
                  (should
                   (cl-every
                    (lambda (request)
                      (and (numberp (plist-get request :input-tokens))
                           (numberp (plist-get request :cached-input-tokens))))
                    evidence))
                  (should (equal (mapcar (lambda (request)
                                           (plist-get request
                                                      :stable-prefix-fingerprint))
                                         evidence)
                                 (make-list 3
                                            (plist-get (car evidence)
                                                       :stable-prefix-fingerprint))))
                  (should (equal (mapcar (lambda (request)
                                           (plist-get request
                                                      :stable-instructions-sha256))
                                         evidence)
                                 (make-list 3
                                            (plist-get
                                             (car evidence)
                                             :stable-instructions-sha256))))
                  (setq affinity-result "pass"))
              (error (setq condition caught))))
          (e-cache-efficiency-e2e--report-evidence
           (list :scenario "codex-repeated-session-affinity-p1-no-tool"
                 :repository-revision (e-live-e2e--repository-revision)
                 :timestamp (format-time-string "%FT%TZ" nil t)
                 :elapsed-seconds (- (float-time) started-at)
                 :semantic-result semantic-result
                 :semantic-subturn-results (vconcat (nreverse semantic-results))
                 :affinity-field-result affinity-result
                 :cache-result "not-measured"
                 :request-count (length captured)
                 :request-start-count (length starts)
                 :usage-event-count (length usage)
                 :usage-join-complete usage-complete
                 :provider-request-starts (vconcat starts)
                 :usage-events (vconcat usage)
                 :requests (vconcat evidence)))
          (when condition (signal (car condition) (cdr condition))))))))

(ert-deftest e-cache-efficiency-e2e-test-codex-growing-history-affinity ()
  "Codex reuses growing synthetic history with native observation and reasoning."
  (let* ((profile (e-cache-efficiency-e2e--supported-codex-profile))
         (observation-delivery (e-openai-profile-observation-delivery profile)))
    (unless (and (plist-get profile :include-encrypted-reasoning)
                 (eq observation-delivery 'inherited))
      (ert-skip
       "Configured Codex profile does not request encrypted reasoning with inherited observations.")))
  (let* ((layer (e-cache-efficiency-e2e--codex-probe-layer))
         (started-at (float-time))
         (timeout (e-live-e2e--cache-scenario-timeout))
         (deadline (+ started-at timeout))
         (native-openai (symbol-function 'e-openai--request-context))
         (requests-by-session (make-hash-table :test 'equal))
         (semantic-result "unavailable")
         (cache-result "unavailable")
         (condition nil))
    (e-live-e2e--with-harness (harness session-id :layers (list layer))
      (unwind-protect
          (condition-case caught
              (cl-letf (((symbol-function 'e-openai--request-context)
                         (lambda (&rest args)
                           (e-cache-efficiency-e2e--capture-request-context
                            native-openai requests-by-session args))))
                (let* ((result
                        (e-live-e2e--prompt-batch-before-deadline
                        harness session-id
                         (e-cache-efficiency-e2e--codex-growing-history-prompt)
                         deadline))
                       (answer (e-live-e2e--assistant-content result))
                       (requests (gethash session-id requests-by-session)))
                  (e-cache-efficiency-e2e--assert-codex-growing-history-answer
                   answer)
                  (should (>= (length requests) 5))
                  (should (equal (mapcar (lambda (request)
                                           (plist-get request :request-ordinal))
                                         requests)
                                 (number-sequence 1 (length requests))))
                  (should
                   (cl-every
                    (lambda (request)
                      (and (plist-get request :session-affinity-identities-match)
                           (eq (plist-get request :affinity-selected) t)
                           (eq (plist-get request :affinity-body-key-present) t)
                           (eq (plist-get request :affinity-header-present) t)
                           (not (plist-get request :legacy-session-header-present))
                           (not (plist-get request :explicit-cache-controls-present))
                           (plist-get request :inherited-observation-present)
                           (plist-get request :encrypted-reasoning-requested)))
                    requests))
                  (should
                   (> (plist-get (cadr requests)
                                 :encrypted-reasoning-replay-count)
                      0))
                  (should
                   (cl-every
                    (lambda (request)
                      (and (= (plist-get request :inherited-observation-count) 1)
                           (= (plist-get request
                                         :unchanged-previous-durable-input-prefix-length)
                              (plist-get request
                                         :previous-durable-input-item-count))
                           (memq (plist-get request
                                            :durable-input-first-divergence-type)
                                 '(initial append stable))))
                    requests))
                  (setq semantic-result "pass")))
            (error (setq condition caught)))
        (let* ((requests (gethash session-id requests-by-session))
               (usage (mapcar (lambda (event) (plist-get event :payload))
                              (e-live-e2e--activity-of-type
                               harness session-id 'token-usage))))
          (setq cache-result
                (e-cache-efficiency-e2e--report-codex-growing-history-p2
                 started-at requests usage semantic-result)))
        (when condition (signal (car condition) (cdr condition))))
      (cond
       ((equal cache-result "growing-history-reuse") t)
       ((equal cache-result "static-prefix-only")
        (ert-fail "Codex only reread static input as tool history grew."))
      (t (ert-skip
           (format "Codex growing-cache benefit is unconfirmed: %s"
                   cache-result)))))))

(ert-deftest e-cache-efficiency-e2e-test-codex-growing-history-synthetic-lifecycle ()
  "The P2 semantic and report path runs through ordinary SQL chat offline."
  (let* ((request-counter (list 0))
         (layer (e-cache-efficiency-e2e--codex-probe-layer))
         (harness
          (e-chat-sql-e2e-make-harness
           :backend
           (e-cache-efficiency-e2e--synthetic-p2-backend request-counter)))
         (session-id nil)
         (started-at (float-time))
         (semantic-result "unavailable")
         (emitted nil))
    (unwind-protect
        (progn
          (e-harness-set-intrinsic-capabilities
           harness (e-layer-capabilities layer))
          (setq session-id (e-chat-sql-e2e-create-session harness))
          (let* ((result
                  (e-chat-sql-e2e-prompt-batch
                   harness session-id
                   (e-cache-efficiency-e2e--codex-growing-history-prompt)
                   10.0))
                 (answer (e-live-e2e--assistant-content result)))
            (unless (eq (plist-get result :status) 'done)
              (error "Synthetic P2 turn failed: %S" result))
            (e-cache-efficiency-e2e--assert-codex-growing-history-answer answer)
            (setq semantic-result "pass")
            (should (= (car request-counter) 5))
            (should (equal (plist-get result :turn-id)
                           (plist-get result :id)))
            (should (stringp (plist-get result :assistant-message-id))))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest arguments)
                       (setq emitted (apply #'format format-string arguments)))))
            (let ((cache-result
                   (e-cache-efficiency-e2e--report-codex-growing-history-p2
                    started-at nil nil semantic-result)))
              (should (equal cache-result "unconfirmed-incomplete-usage"))))
          (let* ((record
                  (e-json-parse-string
                   (substring emitted (length "E98_CACHE_EVIDENCE ")))))
            (should (equal (plist-get record :scenario)
                           "codex-growing-tool-history-p2"))
            (should (equal (plist-get record :semantic-result) "pass"))
            (should (equal (plist-get record :cache-result)
                           "unconfirmed-incomplete-usage"))
            (should (= (length (plist-get record :requests)) 0))))
      (e-chat-sql-e2e-reset))))

(ert-deftest e-cache-efficiency-e2e-test-sql-output-waits-for-terminal-turn-row ()
  "A previous assistant row cannot satisfy a later SQL turn wait."
  (let* ((reply-count 0)
         (backend
          (e-backend-create
           :name "delayed SQL output fixture"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                         on-request-start)
              (ignore messages options)
              (setq reply-count (1+ reply-count))
              (let ((cancelled nil) timer
                    (request (e-backend-request-create
                              :cancel (lambda ()
                                        (setq cancelled t)
                                        (when (timerp timer)
                                          (cancel-timer timer))
                                        t))))
                (when on-request-start
                  (funcall on-request-start request))
                (setq timer
                      (run-at-time
                       0 nil
                       (lambda ()
                         (unless cancelled
                           (condition-case condition
                               (progn
                                 (funcall on-item
                                          (list :type 'assistant-message
                                                :content
                                                (if (= reply-count 1)
                                                    "first turn answer"
                                                  "second turn answer")))
                                 (funcall on-item '(:type done :reason stop))
                                 (when on-done
                                   (funcall on-done '(:status done))))
                             (error
                              (when on-error
                                (funcall on-error condition))))))))
                request)))))
         (harness (e-chat-sql-e2e-make-harness :backend backend))
         (session-id nil)
         (append-count 0)
         pending-append-work pending-append-arguments actual-append-work
         release-append)
    (unwind-protect
        (cl-letf
            (((symbol-function 'e-board-sqlite-service-record-append-start)
              (let ((original
                     (symbol-function
                      'e-board-sqlite-service-record-append-start)))
                (lambda (service board-id record-kind source-kind source-key
                                &rest fields)
                  (let ((turn-output
                         (and (eq record-kind 'output)
                              (eq source-kind 'output)
                              (equal (car-safe source-key) "harness-output"))))
                    (when turn-output
                      (setq append-count (1+ append-count)))
                    (if (and turn-output (= append-count 2))
                        (progn
                          (setq pending-append-arguments
                                (append (list service board-id record-kind
                                              source-kind source-key)
                                        fields))
                          (setq pending-append-work
                                (e-work-start
                                 (e-work-spec-create
                                  :id "held-output-append"
                                  :execution 'cooperative
                                  :interactive-policy 'async
                                  :runner
                                  (lambda (_handle _arguments _context)
                                    :deferred))
                                 nil))
                          (setq release-append
                                (lambda ()
                                  (when pending-append-work
                                    (let ((held pending-append-work)
                                          (arguments
                                           pending-append-arguments))
                                      (setq pending-append-work nil
                                            pending-append-arguments nil
                                            release-append nil)
                                      (setq actual-append-work
                                            (apply original arguments))
                                      (e-work-on-settle
                                       actual-append-work
                                       (lambda (settled)
                                         (let ((status
                                                (e-work-status settled)))
                                           (if (eq (plist-get status :state)
                                                   'finished)
                                               (e-work-finish
                                                held
                                                (plist-get status :result))
                                             (e-work-fail
                                              held
                                              (plist-get status :error))))))
                                      actual-append-work))))
                          pending-append-work)
                      (apply original
                             (append (list service board-id record-kind
                                           source-kind source-key)
                                     fields))))))))
          (setq session-id (e-chat-sql-e2e-create-session harness))
          (let* ((first
                  (e-chat-sql-e2e-prompt-batch
                   harness session-id "first turn" 5.0))
                 (first-turn-id (plist-get first :turn-id)))
            (unless (eq (plist-get first :status) 'done)
              (error "First SQL turn failed: %S" first))
            (should (equal (plist-get first :assistant-content)
                           "first turn answer"))
            (should (equal (plist-get first :id) first-turn-id))
            (should (stringp (plist-get first :assistant-message-id)))
            (e-chat-sql-e2e-prompt-async
             harness session-id "second turn")
            (let* ((event
                    (e-chat-sql-e2e-wait-until
                     (lambda ()
                       (and release-append
                            (gethash
                             session-id
                             (e-chat-sql-e2e--event-table harness))))
                     5.0))
                   (second-turn-id (plist-get event :turn-id))
                   (view
                    (e-chat-sql-e2e--await
                     (e-session-async-chat-view
                      (e-harness-sessions harness) session-id :limit 64)
                     5.0))
                   (first-row
                    (cl-find-if
                     (lambda (message)
                       (equal (plist-get message :source-turn-id)
                              first-turn-id))
                     (plist-get view :messages))))
              (should (eq (plist-get event :type) 'turn-finished))
              (should-not (equal second-turn-id first-turn-id))
              (should first-row)
              (should (equal (plist-get first-row :content)
                             "first turn answer"))
              (should-error
               (e-chat-sql-e2e-wait-batch harness session-id 0.25)
               :type 'error)
              (should release-append)
              (funcall release-append)
              (let ((second
                     (e-chat-sql-e2e-wait-batch harness session-id 5.0)))
                (should (equal (plist-get second :assistant-content)
                               "second turn answer"))
                (should (equal (plist-get second :id) second-turn-id))
                (should (equal (plist-get second :turn-id) second-turn-id))
                (should (stringp (plist-get second :assistant-message-id)))))))
      (when release-append
        (ignore-errors (funcall release-append)))
      (when actual-append-work
        (ignore-errors (e-chat-sql-e2e--await actual-append-work 5.0)))
      (e-chat-sql-e2e-reset))))

(ert-deftest e-cache-efficiency-e2e-test-durable-usage-collection-pages-sqlite ()
  "Usage collection preserves order across durable bounded SQL pages."
  (let* ((harness (e-chat-sql-e2e-make-harness
                   :backend (e-backend-create :name "unused" :start #'ignore)))
         (store (e-harness-sessions harness)))
    (unwind-protect
        (let ((session-id (e-chat-sql-e2e-create-session harness)))
          (dotimes (index 102)
            (e-chat-sql-e2e--await
             (e-session-append-activity-event
              store session-id nil 'token-usage (list :ordinal index))))
          (e-chat-sql-e2e--await
           (e-session-append-activity-event
            store session-id nil 'provider-request-finished '(:status done)))
          (cl-letf (((symbol-function 'e-session-local-activity-events)
                     (lambda (&rest _) (ert-fail "SQL usage read a local aggregate"))))
            (should
             (equal
              (mapcar (lambda (event)
                        (plist-get (plist-get event :payload) :ordinal))
                      (e-live-e2e--activity-of-type harness session-id 'token-usage))
              (number-sequence 0 101)))))
      (e-chat-sql-e2e-reset))))

(ert-deftest e-cache-efficiency-e2e-test-growing-tool-history ()
  "Subturn cache reads advance beyond the first unchanged-history boundary."
  (e-live-e2e--require-enabled)
  (let* ((layer
          (e-layer-create
           :id 'cache-efficiency-probe :name "Cache efficiency probe"
           :capabilities
           (list (e-capability-create
                  :id 'cache-efficiency-probe
                  :instructions
                  (mapconcat (lambda (n) (format "stable directive %d" n))
                             (number-sequence 1 1500) "\n")
                  :tools (list #'e-cache-efficiency-e2e--tool-register)))))
         (started-at (float-time))
         (timeout (e-live-e2e--cache-scenario-timeout))
         (deadline (+ started-at timeout))
         (requests-by-session (make-hash-table :test #'equal))
         (semantic-result "unavailable")
         (cache-result "unavailable") (condition nil)
         (native-anthropic (symbol-function 'e-anthropic--request-context))
         (native-openai (symbol-function 'e-openai--request-context)))
    (e-live-e2e--with-harness (harness session-id :layers (list layer))
      (unwind-protect
          (condition-case caught
              (cl-letf
                  (((symbol-function 'e-anthropic--request-context)
                    (lambda (&rest args)
                      (let* ((context (apply native-anthropic args))
                             (body (e-json-parse-string (plist-get context :body)))
                             (options (plist-get args :options))
                             (request-session-id
                              (or (plist-get options :session-id) session-id))
                             (session-requests
                              (gethash request-session-id requests-by-session))
                             (request
                              (list :provider 'anthropic
                                    :endpoint (plist-get context :url)
                                    :model (plist-get body :model)
                                    :stable-prefix-fingerprint
                                    (e-cache-efficiency-e2e--static-fingerprint
                                     body options)
                                    :request-ordinal
                                    (1+ (length session-requests))))
                             (updated (append session-requests (list request))))
                        (puthash request-session-id updated requests-by-session)
                        context)))
                   ((symbol-function 'e-openai--request-context)
                    (lambda (&rest args)
                      (let* ((context (apply native-openai args))
                             (body (plist-get context :body))
                             (options (plist-get args :options)))
                        (when (stringp body) (setq body (e-json-parse-string body)))
                        (let* ((request-session-id
                                (or (plist-get options :session-id) session-id))
                               (session-requests
                                (gethash request-session-id requests-by-session))
                               (request
                                (list :provider 'openai
                                      :endpoint (plist-get context :url)
                                      :model (plist-get body :model)
                                      :stable-prefix-fingerprint
                                      (e-cache-efficiency-e2e--static-fingerprint
                                       body options)
                                      :request-ordinal
                                      (1+ (length session-requests))))
                               (updated
                                (append session-requests (list request))))
                          (puthash request-session-id updated requests-by-session))
                        context))))
                (let* ((result
                        (e-live-e2e--prompt-batch-before-deadline
                         harness session-id
                         (concat "Call cache_probe with step 1 and wait for its result. "
                                 "Then call step 2 and wait for its result, then step 3. "
                                 "Finally return only the sum of their PROBE codes. "
                                 "Do not call other tools.")
                         deadline))
                       (answer (e-live-e2e--assistant-content result)))
                  (should (string-match-p "2199" answer))
                  (setq semantic-result "pass")))
            (error (setq condition caught)))
        (let* ((requests (gethash session-id requests-by-session))
               (usage (mapcar (lambda (event) (plist-get event :payload))
                             (e-live-e2e--activity-of-type
                              harness session-id 'token-usage)))
               (evidence
                (e-cache-efficiency-e2e--join-request-usage requests usage)))
          (setq cache-result
                (e-cache-efficiency-e2e--qualified-growing-cache-result evidence))
          (e-cache-efficiency-e2e--report-evidence
                    (list :scenario "growing-tool-history"
                          :repository-revision (e-live-e2e--repository-revision)
                          :timestamp (format-time-string "%FT%TZ" nil t)
                          :elapsed-seconds (- (float-time) started-at)
                          :semantic-result semantic-result :cache-result cache-result
                          :usage-event-count (length usage)
                          :usage-join-complete
                          (e-cache-efficiency-e2e--usage-join-complete-p evidence)
                          :requests (vconcat evidence)))))
      (when condition (signal (car condition) (cdr condition)))
      (cond ((equal cache-result "growing-history-reuse") t)
            ((equal cache-result "static-prefix-only")
             (ert-fail "Only static cache reads were observed as tool history grew."))
            (t (ert-skip (format "Growing cache benefit is unconfirmed: %s" cache-result)))))))

(defun e-cache-efficiency-e2e--f99-anthropic-route ()
  "Return the exact normal-source gateway route or skip without a credential."
  (e-live-e2e--require-enabled)
  (let ((config (e-live-e2e--config-file)))
    (unless (and config
                 (equal (file-truename config)
                        (file-truename
                         (expand-file-name "e2e/e-e2e-config-anthropic-p3.el"
                                           default-directory))))
      (ert-skip "Select the isolated normal-source Anthropic gateway config.")))
  (let* ((provider e-anthropic-default-provider)
         (profile (e-anthropic-provider-profile provider))
         (base-url (e-anthropic--provider-base-url profile))
         (model (e-anthropic--provider-model profile nil))
         (env-key (plist-get profile :env-key))
         (options (list :model model
                        :canvas-reference-layout-revision
                        e-context-canvas-reference-layout-revision))
         (identity
          (e-anthropic--context-capability-identity
           provider options :base-url base-url :model model
           :requester 'messages-http-start)))
    (unless (and (equal model "claude-opus-5-5")
                 (e-anthropic--context-capability-supported-p identity))
      (ert-skip "Configured route is not the reviewed gateway Opus 5.5 route."))
    (unless (and (stringp env-key)
                 (not (string-empty-p (or (getenv env-key) ""))))
      (ert-skip "Gateway authentication is unavailable."))
    identity))

(defun e-cache-efficiency-e2e--f99-stable-instructions ()
  "Return a stable prefix longer than the gateway cache minimum."
  (mapconcat (lambda (index) (format "F99 stable cache instruction %d" index))
             (number-sequence 1 1500) "\n"))

(defun e-cache-efficiency-e2e--f99-canvas-layer (&optional probe-steps)
  "Return a private test layer with stable guidance and optional probe tool."
  (e-layer-create
   :id 'f99-canvas-cache-qualification
   :name "F99 Canvas cache qualification"
   :capabilities
   (list
    (e-capability-create
     :id 'f99-canvas-cache-qualification
     :instructions (e-cache-efficiency-e2e--f99-stable-instructions)
     :tools (when probe-steps
              (list (lambda (registry)
                      (e-cache-efficiency-e2e--tool-register
                       registry probe-steps))))))))

(defun e-cache-efficiency-e2e--f99-ensure-chat-session-provider (harness)
  "Ensure isolated HARNESS has the production current-attachment provider."
  (unless (seq-some (lambda (capability)
                      (eq (e-capability-id capability) 'chat-session))
                    (e-harness-intrinsic-capabilities harness))
    (e-harness-set-intrinsic-capabilities
     harness
     (append (e-harness-intrinsic-capabilities harness)
             (list (e-chat-session-capability-create)))))
  t)

(defun e-cache-efficiency-e2e--f99-attach-canvas
    (harness session-id buffer)
  "Attach live BUFFER as the private session's primary Canvas."
  (e-chat-session-attach-context
   harness session-id
   (list :uri (concat "buffer://" (buffer-name buffer))
         :label "F99 qualification canvas"
         :buffer-name (buffer-name buffer))
   :canvas t :current-attachments nil))

(defun e-cache-efficiency-e2e--f99-canvas-text (prefix)
  "Build a substantial synthetic checklist beginning with PREFIX."
  (mapconcat (lambda (index)
               (format "- [ ] %s item %03d stable canvas text %s"
                       prefix index (make-string 70 ?x)))
             (number-sequence 1 160) "\n"))

(defun e-cache-efficiency-e2e--f99-check-one-item (buffer item-number)
  "Mark ITEM-NUMBER in BUFFER complete as a private simulated user edit."
  (with-current-buffer buffer
    (goto-char (point-min))
    (dotimes (index item-number)
      (unless (search-forward "[ ]" nil t)
        (error "F99 Canvas fixture has no unchecked item %d" item-number))
      (when (= index (1- item-number))
        (replace-match "[x]" t t)))))

(defun e-cache-efficiency-e2e--f99-cache-control-count (value)
  "Count native cache-control fields recursively in VALUE."
  (cond
   ((and (listp value) (or (null value) (keywordp (car value))))
    (+ (if (plist-member value :cache_control) 1 0)
       (cl-loop for (_key item) on value by #'cddr
                sum (e-cache-efficiency-e2e--f99-cache-control-count item))))
   ((and (vectorp value) (not (stringp value)))
    (cl-loop for item across value
             sum (e-cache-efficiency-e2e--f99-cache-control-count item)))
   ((listp value)
    (cl-loop for item in value
             sum (e-cache-efficiency-e2e--f99-cache-control-count item)))
   (t 0)))

(defun e-cache-efficiency-e2e--f99-capture-request
    (native requests-by-session args)
  "Call NATIVE and retain only bounded F99 route/context evidence."
  (let* ((context (apply native args))
         (options (plist-get args :options))
         (session-id (plist-get options :session-id))
         (messages (plist-get args :messages))
         (layout-revision
          (or (plist-get options :canvas-reference-layout-revision)
              (e-context-canvas-reference-layout-for-messages messages)))
         (effective-options (copy-sequence options))
         (_ (when layout-revision
              (setq effective-options
                    (plist-put effective-options
                               :canvas-reference-layout-revision
                               layout-revision))))
         (body (e-json-parse-string (plist-get context :body)))
         (identity
          (e-anthropic--context-capability-identity
           (plist-get args :provider) effective-options
           :base-url (plist-get args :base-url)
           :model (plist-get body :model)
           :request-function (plist-get args :request-function)
           :requester (plist-get args :requester)))
         (source-evidence
          (e-cache-efficiency-e2e--canvas-source-evidence effective-options))
         (records (gethash session-id requests-by-session))
         (wire-hashes
          (e-cache-efficiency-e2e--p3-native-item-hashes 'anthropic body))
         (semantic-hashes
          (e-cache-efficiency-e2e--p3-native-semantic-item-hashes
           'anthropic body))
         (current-hashes (list :native-history-item-sha256 wire-hashes))
         (prefix-evidence
          (and records
               (e-cache-efficiency-e2e--native-prefix-evidence
                (car (last records)) current-hashes)))
         (tools (append (plist-get body :tools) nil))
         (record
          (append
           (list :adapter 'anthropic :provider 'anthropic
                 :route-provider (plist-get identity :provider)
                 :endpoint (plist-get context :url)
                 :model (plist-get body :model)
                 :auth-mode (plist-get identity :auth-mode)
                 :api (plist-get identity :api)
                 :transport (plist-get identity :transport)
                 :requester (plist-get identity :requester)
                 :canvas-reference-layout-revision layout-revision
                 :request-ordinal (1+ (length records))
                 :stable-prefix-fingerprint
                 (e-cache-efficiency-e2e--static-fingerprint body options)
                 :native-request-sha256
                 (secure-hash 'sha256 (e-json-serialize body))
                 :native-history-item-sha256 wire-hashes
                 :native-semantic-history-item-sha256 semantic-hashes
                 :canvas-operations
                 (e-cache-efficiency-e2e--canvas-operation-evidence messages)
                 :cache-control-count
                 (e-cache-efficiency-e2e--f99-cache-control-count body)
                 :context-curation-tool-present
                 (and (seq-some
                       (lambda (tool)
                         (equal (plist-get tool :name) "context-curate"))
                       tools)
                      t))
           source-evidence
           prefix-evidence)))
    (puthash session-id (append records (list record)) requests-by-session)
    context))

(defun e-cache-efficiency-e2e--f99-probe-prompt (step)
  "Return a bounded Canvas cache prompt for PROBE STEP."
  (format "Call cache_probe once with step %d and wait for its result. Return only the PROBE code. Do not edit the attached Canvas."
          step))

(ert-deftest e-cache-efficiency-e2e-test-f99-canvas-layout-e1-minimal ()
  "Probe the changed Canvas layout on the normal gateway route (E1)."
  (let* ((identity (e-cache-efficiency-e2e--f99-anthropic-route))
         (layer (e-cache-efficiency-e2e--f99-canvas-layer))
         (started-at (float-time))
         (deadline (+ started-at (e-live-e2e--cache-scenario-timeout)))
         (requests-by-session (make-hash-table :test #'equal))
         (native (symbol-function 'e-anthropic--request-context))
         (semantic-result "unavailable")
         (semantic-failure nil)
         (provider-error nil))
    (e-live-e2e--with-harness (harness session-id :layers (list layer))
      (e-cache-efficiency-e2e--f99-ensure-chat-session-provider harness)
      (with-temp-buffer
        (rename-buffer "e99-canvas-e1" t)
        (insert (e-cache-efficiency-e2e--f99-canvas-text "E1"))
        (e-cache-efficiency-e2e--f99-attach-canvas
         harness session-id (current-buffer))
        (cl-letf (((symbol-function 'e-anthropic--request-context)
                   (lambda (&rest args)
                     (e-cache-efficiency-e2e--f99-capture-request
                      native requests-by-session args))))
          (condition-case _caught
              (progn
                (dolist (_index '(1 2))
                  (let* ((result
                          (e-live-e2e--prompt-batch-before-deadline
                           harness session-id
                           "Reply exactly E99-CANVAS-E1-OK. Do not call tools or edit the Canvas."
                           deadline))
                         (answer (e-live-e2e--assistant-content result)))
                    (unless (string-match-p "E99-CANVAS-E1-OK" answer)
                      (setq semantic-failure t))))
                (setq semantic-result
                      (if semantic-failure "semantic-failure" "pass")))
            (error (_caught)
              (unless semantic-failure (setq provider-error t)))))
        (let* ((requests (gethash session-id requests-by-session))
               (start-payloads
                (mapcar (lambda (event) (plist-get event :payload))
                        (e-live-e2e--activity-of-type
                         harness session-id 'provider-request-started)))
               (joined-requests
                (e-cache-efficiency-e2e--join-request-starts
                 requests start-payloads))
             (usage
              (mapcar (lambda (event) (plist-get event :payload))
                      (e-live-e2e--activity-of-type
                       harness session-id 'token-usage)))
             (evidence (e-cache-efficiency-e2e--join-request-usage
                          joined-requests usage))
             (usage-complete
              (e-cache-efficiency-e2e--usage-join-complete-p evidence))
             (cache-fields-accepted
              (and (>= (length joined-requests) 2)
                   (cl-every (lambda (request)
                               (and (equal
                                     (e-anthropic--context-capability-route-identity
                                      identity)
                                     (e-anthropic--context-capability-route-identity
                                      request))
                                    (> (plist-get request :cache-control-count) 0)
                                    (equal (plist-get request
                                                      :canvas-reference-layout-revision)
                                           e-context-canvas-reference-layout-revision)
                                    (= (plist-get request :canvas-source-count) 1)
                                    (plist-get request :canvas-source-present)
                                    (plist-get request
                                               :canvas-source-revision-matches-content)
                                    (not (plist-get request
                                                    :context-curation-tool-present))))
                             joined-requests)))
             (counters-observed
              (and evidence
                   (cl-every (lambda (request)
                               (and (numberp (plist-get request :input-tokens))
                                    (or (numberp
                                         (plist-get request :cached-input-tokens))
                                        (numberp
                                         (plist-get request
                                                    :cache-creation-input-tokens)))))
                             evidence)))
             (capability-result
              (cond
               (provider-error "provider/infrastructure-failure")
               ((not cache-fields-accepted)
                "product-failure-layout-or-cache-wire")
               ((and usage-complete counters-observed) "pass")
               (t "unconfirmed"))))
        (e-cache-efficiency-e2e--report-canvas-evidence
         (list :scenario "F99-E1-canvas-layout-minimal"
               :accepted-source-revision (e-live-e2e--repository-revision)
               :completed-at-utc (format-time-string "%FT%TZ" nil t)
               :elapsed-seconds (- (float-time) started-at)
               :effective-route (e-anthropic--context-capability-route-identity
                                 identity)
               :canvas-reference-layout-revision
               e-context-canvas-reference-layout-revision
               :semantic-result semantic-result
               :growing-reuse-result "not-measured"
               :route-capability-result capability-result
               :curation-capability-result "unconfirmed-unavailable-no-P3"
               :provider-request-start-count (length start-payloads)
               :request-count (length joined-requests)
               :usage-event-count (length usage)
               :usage-join-complete usage-complete
               :requests (vconcat evidence)))
        (when provider-error
          (ert-skip "E1 provider request failed; evidence is unconfirmed."))
        (when semantic-failure
          (ert-fail "E1 semantic answer contradicted its requested result."))
        (should (equal semantic-result "pass"))
        (should (>= (length joined-requests) 2))
        (should (equal (e-anthropic--context-capability-route-identity identity)
                       (e-anthropic--context-capability-route-identity
                        (car joined-requests))))
        (cond
         ((equal capability-result "pass") t)
         ((equal capability-result "unconfirmed")
          (ert-skip "E1 provider usage counters are unavailable; qualification is unconfirmed."))
         (t (ert-fail (format "E1 request shape contradicted the reviewed layout: %s"
                              capability-result)))))))))

(ert-deftest e-cache-efficiency-e2e-test-f99-canvas-layout-e2-growing-history ()
  "Compose ordinary Canvas revisions with the normal route E2 cache predicate."
  (let* ((identity (e-cache-efficiency-e2e--f99-anthropic-route))
         (layer (e-cache-efficiency-e2e--f99-canvas-layer 3))
         (started-at (float-time))
         (deadline (+ started-at (e-live-e2e--cache-scenario-timeout)))
         (requests-by-session (make-hash-table :test #'equal))
         (native (symbol-function 'e-anthropic--request-context))
         (semantic-result "unavailable")
         (semantic-failure nil)
         (provider-error nil))
    (e-live-e2e--with-harness (harness session-id :layers (list layer))
      (e-cache-efficiency-e2e--f99-ensure-chat-session-provider harness)
      (with-temp-buffer
        (rename-buffer "e99-canvas-e2" t)
        (insert (e-cache-efficiency-e2e--f99-canvas-text "E2 initial"))
        (let ((canvas-buffer (current-buffer)))
          (e-cache-efficiency-e2e--f99-attach-canvas
           harness session-id canvas-buffer)
          (cl-letf (((symbol-function 'e-anthropic--request-context)
                     (lambda (&rest args)
                       (e-cache-efficiency-e2e--f99-capture-request
                        native requests-by-session args))))
            (condition-case _caught
                (progn
                  (dolist (step '(1 2 3))
                    (let* ((result
                            (e-live-e2e--prompt-batch-before-deadline
                             harness session-id
                             (e-cache-efficiency-e2e--f99-probe-prompt step)
                             deadline))
                           (answer (e-live-e2e--assistant-content result)))
                      (unless (string-match-p
                               (format "PROBE-%d-CODE=%d" step (+ 731 step))
                               answer)
                        (setq semantic-failure t))
                      (when (< step 3)
                        (e-cache-efficiency-e2e--f99-check-one-item
                         canvas-buffer step)))))
              (error (_caught)
                (unless semantic-failure (setq provider-error t))))
            (unless (or provider-error semantic-failure)
              (with-current-buffer canvas-buffer
                (erase-buffer)
                (insert (e-cache-efficiency-e2e--f99-canvas-text
                         "E2 large rewrite")))
              (let* ((result
                      (e-live-e2e--prompt-batch-before-deadline
                       harness session-id
                       "Reply exactly F99-CANVAS-RESET-OK. Do not call tools or edit the Canvas."
                       deadline))
                     (answer (e-live-e2e--assistant-content result)))
                (unless (string-match-p "F99-CANVAS-RESET-OK" answer)
                  (setq semantic-failure t)))
              (unless (or provider-error semantic-failure)
                (setq semantic-result "pass")))
            (when semantic-failure
              (setq semantic-result "semantic-failure"))))
      (let* ((requests (gethash session-id requests-by-session))
             (start-payloads
              (mapcar (lambda (event) (plist-get event :payload))
                      (e-live-e2e--activity-of-type
                       harness session-id 'provider-request-started)))
             (joined-requests
              (e-cache-efficiency-e2e--join-request-starts
               requests start-payloads))
             (usage
              (mapcar (lambda (event) (plist-get event :payload))
                      (e-live-e2e--activity-of-type
                       harness session-id 'token-usage)))
             (evidence (e-cache-efficiency-e2e--join-request-usage
                        joined-requests usage))
             (usage-complete
              (e-cache-efficiency-e2e--usage-join-complete-p evidence))
             (cache-result
              (e-cache-efficiency-e2e--qualified-growing-cache-result evidence))
             (layouts
              (delete-dups
               (mapcar (lambda (request)
                         (plist-get request
                                    :canvas-reference-layout-revision))
                       joined-requests)))
             (source-revisions
              (mapcar (lambda (request)
                        (plist-get request :canvas-source-revision))
                      joined-requests))
             (operations
              (apply #'append
                     (mapcar (lambda (request)
                               (append (plist-get request :canvas-operations) nil))
                             joined-requests)))
             (source-integrity
              (and requests
                   (cl-every
                    (lambda (request)
                      (let ((revision (plist-get request :canvas-source-revision))
                            (request-operations
                             (append (plist-get request :canvas-operations) nil)))
                        (and (= (plist-get request :canvas-source-count) 1)
                             (plist-get request :canvas-source-present)
                             (plist-get request
                                        :canvas-source-revision-matches-content)
                             (seq-some
                              (lambda (operation)
                                (equal revision (plist-get operation :revision)))
                              request-operations))))
                             joined-requests)))
             (unchanged-canvas-reobserved
              (let ((previous nil))
                (seq-some (lambda (revision)
                            (prog1 (and previous (equal previous revision))
                              (setq previous revision)))
                          source-revisions)))
             (small-edit-patch-count
              (length (delete-dups
                       (cl-loop for operation in operations
                                when (eq (plist-get operation :representation) 'patch)
                                collect (plist-get operation :revision)))))
             (route-stable
              (and joined-requests source-integrity
                   (cl-every
                    (lambda (request)
                      (and (equal (e-anthropic--context-capability-route-identity
                                   identity)
                                  (e-anthropic--context-capability-route-identity
                                   request))
                           (equal (plist-get request
                                             :canvas-reference-layout-revision)
                                  e-context-canvas-reference-layout-revision)
                           (> (plist-get request :cache-control-count) 0)
                           (not (plist-get request
                                           :context-curation-tool-present))))
                    joined-requests)))
             (snapshot-revisions
              (mapcar (lambda (operation)
                        (and (eq (plist-get operation :representation) 'snapshot)
                             (plist-get operation :revision)))
                      operations))
             (reset-observed
              (and source-revisions
                   (cl-some (lambda (revision)
                             (and revision (not (member revision
                                                       (butlast source-revisions)))))
                           (last source-revisions))
                   (seq-some (lambda (revision)
                               (and revision (member revision snapshot-revisions)))
                             (last source-revisions))))
             (growing-result
              (cond
               (provider-error "provider/infrastructure-failure")
               ((not route-stable) "semantic-failure-route-identity")
               ((and usage-complete
                     (equal cache-result "growing-history-reuse"))
                cache-result)
               ((and usage-complete
                     (member cache-result
                             '("static-prefix-only" "insufficient-history-reuse"
                               "history-not-growing" "prefix-changed")))
                cache-result)
               (t (concat "unconfirmed:" cache-result)))))
        (e-cache-efficiency-e2e--report-canvas-evidence
         (list :scenario "F99-E2-canvas-growing-history"
               :accepted-source-revision (e-live-e2e--repository-revision)
               :completed-at-utc (format-time-string "%FT%TZ" nil t)
               :elapsed-seconds (- (float-time) started-at)
               :effective-route (e-anthropic--context-capability-route-identity
                                 identity)
               :canvas-reference-layout-revision
               e-context-canvas-reference-layout-revision
               :semantic-result semantic-result
               :growing-reuse-result growing-result
               :curation-capability-result "unconfirmed-unavailable-no-P3"
               :static-only-control
               "initial whole-request input is the conservative ceiling; the credential-free classifier has an explicit static-prefix-only negative control"
               :usage-event-count (length usage)
               :provider-request-start-count (length start-payloads)
               :usage-join-complete usage-complete
               :distinct-canvas-source-revision-count
               (length (delete-dups (copy-sequence source-revisions)))
               :unchanged-canvas-reobserved unchanged-canvas-reobserved
               :small-edit-patch-revision-count small-edit-patch-count
               :large-rewrite-reset-observed reset-observed
               :requests (vconcat evidence)))
        (when provider-error
          (ert-skip "E2 provider request failed; evidence is unconfirmed."))
        (when semantic-failure
          (ert-fail "E2 Canvas/tool result contradicted its requested behavior."))
        (should (equal semantic-result "pass"))
        (should route-stable)
        (should (equal (car layouts)
                       e-context-canvas-reference-layout-revision))
        (should (>= (length (delete-dups (copy-sequence source-revisions))) 4))
        (should unchanged-canvas-reobserved)
        (should (>= small-edit-patch-count 2))
        (should reset-observed)
        (cond
         ((equal growing-result "growing-history-reuse") t)
         ((member growing-result
                  '("static-prefix-only" "insufficient-history-reuse"
                    "history-not-growing" "prefix-changed"))
          (ert-fail (format "Valid E2 route contradicted growing reuse: %s"
                            growing-result)))
         (t (ert-skip (format "E2 growing reuse remains unconfirmed: %s"
                              growing-result)))))))))

(provide 'e-cache-efficiency-e2e-test)
;;; e-cache-efficiency-e2e-test.el ends here
