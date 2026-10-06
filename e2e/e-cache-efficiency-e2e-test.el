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

(defun e-cache-efficiency-e2e--stable-native-instructions (body options)
  "Summarize stable system guidance from BODY's native input projection.
Return nil when an expected stable system message cannot be found in native
developer input.  The summary contains a digest and size, never the text."
  (let* ((stable-messages
          (cl-loop for segment in (plist-get options :segments)
                   when (memq (plist-get segment :kind)
                              '(static-prefix stable-context))
                   append (seq-filter
                           (lambda (message)
                             (eq (plist-get message :role) 'system))
                           (plist-get segment :messages))))
         (native-input (append (plist-get body :input) nil))
         (cursor 0)
         (native-items nil)
         (complete t))
    (dolist (message stable-messages)
      (let* ((text (plist-get message :content))
             (index
              (and (stringp text)
                   (cl-position-if
                    (lambda (item)
                      (equal (e-cache-efficiency-e2e--native-input-message-text
                              item)
                             text))
                    native-input :start cursor))))
        (if index
            (progn
              (push (nth index native-input) native-items)
              (setq cursor (1+ index)))
          (setq complete nil))))
    (when complete
      (setq native-items (nreverse native-items))
      (let ((texts (mapcar
                    #'e-cache-efficiency-e2e--native-input-message-text
                    native-items)))
        (list :character-count
              (apply #'+ (mapcar #'length texts))
              :sha256
              (secure-hash
               'sha256
               (encode-coding-string
                (e-json-serialize
                 (e-cache-efficiency-e2e--canonical-json-value
                  (vconcat native-items)))
                'utf-8)))))))

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
                               'matched)))
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
                        (not (plist-member request
                                          :unattributed-usage-provenance-events))))
                 evidence)))

(defun e-cache-efficiency-e2e--qualified-growing-cache-result (evidence)
  "Classify growing reuse only when EVIDENCE has complete usage attribution."
  (if (e-cache-efficiency-e2e--usage-join-complete-p evidence)
      (e-live-e2e--classify-growing-cache-evidence :evidence evidence)
    "unconfirmed-incomplete-usage"))

(defun e-cache-efficiency-e2e--report-evidence (record)
  "Emit bounded cache RECORD using the shared E2E value conversion."
  (message "E98_CACHE_EVIDENCE %s"
           (json-encode (e-live-e2e--json-plist record))))

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
              (secure-hash
               'sha256
               (encode-coding-string
                (e-json-serialize
                 (e-cache-efficiency-e2e--canonical-json-value
                  (vector (car input))))
                'utf-8))))
      (should (equal stable-text guidance))
      (should (equal (plist-get body :instructions)
                     "You are a helpful assistant."))
      (should (equal (mapcar (lambda (item) (plist-get item :role)) input)
                     '("developer" "user")))
      (should (null (plist-get record :tool-names)))
      (should (eq (plist-get record :session-affinity-identities-match) t))
      (should (eq (plist-get record :affinity-body-key-present) t))
      (should (eq (plist-get record :affinity-header-present) t)))))

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
          (e-live-e2e--report-evidence
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
                           (> (plist-get request
                                         :stable-instructions-character-count)
                              30000)
                           (stringp
                            (plist-get request :stable-instructions-sha256))))
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
                         (concat
                          "Before calling any tool, solve CROSS + ROADS = DANGER "
                          "as a decimal cryptarithm with distinct digits and no "
                          "leading zero. Work through the column carries and verify "
                          "the equation. Keep the solution in encrypted reasoning, "
                          "not in your final text. Then call cache_probe steps 1, "
                          "2, 3, and 4 in order, waiting for each result before "
                          "requesting the next. After step 1 arrives, check the "
                          "cryptarithm again and retain its solution in reasoning. "
                          "Use all four PROBE codes. Return only "
                          "OBSERVATION=<the fixed observation code> "
                          "SUM=<the sum of the four codes>, and omit the "
                          "cryptarithm solution. Do not call other tools.")
                         deadline))
                       (answer (e-live-e2e--assistant-content result))
                       (requests (gethash session-id requests-by-session)))
                  (should (string-match-p
                           (regexp-quote
                            e-cache-efficiency-e2e--codex-observation)
                           answer))
                  (should (string-match-p "\\b2934\\b" answer))
                  (should-not
                   (string-match-p "96233\\|62513\\|158746" answer))
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
                               harness session-id 'token-usage)))
               (evidence
                (e-cache-efficiency-e2e--join-request-usage requests usage)))
          (setq cache-result
                (e-cache-efficiency-e2e--qualified-growing-cache-result evidence))
          (e-live-e2e--report-evidence
           (list :scenario "codex-growing-tool-history-p2"
                 :repository-revision (e-live-e2e--repository-revision)
                 :timestamp (format-time-string "%FT%TZ" nil t)
                 :elapsed-seconds (- (float-time) started-at)
                 :semantic-result semantic-result
                 :cache-result cache-result
                 :usage-event-count (length usage)
                 :usage-join-complete
                 (e-cache-efficiency-e2e--usage-join-complete-p evidence)
                 :requests (vconcat evidence))))
        (when condition (signal (car condition) (cdr condition))))
      (cond
       ((equal cache-result "growing-history-reuse") t)
       ((equal cache-result "static-prefix-only")
        (ert-fail "Codex only reread static input as tool history grew."))
       (t (ert-skip
           (format "Codex growing-cache benefit is unconfirmed: %s"
                   cache-result)))))))

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

(provide 'e-cache-efficiency-e2e-test)
;;; e-cache-efficiency-e2e-test.el ends here
