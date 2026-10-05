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

(defun e-cache-efficiency-e2e--join-request-usage (requests usage)
  "Join chronological REQUESTS and USAGE, marking the initial input control.
The first captured request is sent before any tool result, so its total input
is a conservative ceiling for stable instructions and initial context."
  (when (= (length requests) (length usage))
    (cl-loop for request in requests
             for counters in usage
             for index from 0
             collect (append request
                             (when (= index 0)
                               '(:phase initial-context-baseline))
                             counters))))

(defun e-cache-efficiency-e2e--tool-register (registry)
  "Register a three-step probe with substantial reproducible output."
  (e-tools-register
   registry :name "cache_probe"
   :description "Read synthetic cache probes sequentially with steps 1, 2, and 3."
   :parameters '(:type "object" :required ["step"]
                 :properties (:step (:type "integer" :enum [1 2 3])))
   :work
   (e-tools-cheap-work
    "e2e.cache-probe"
    (lambda (arguments)
      (let ((step (plist-get arguments :step)))
        (unless (memq step '(1 2 3)) (error "Invalid probe step"))
        (concat (format "PROBE-%d-CODE=%d\n" step (+ 731 step))
                (mapconcat
                 (lambda (index) (format "row-%d-%d synthetic cache evidence" step index))
                 (number-sequence 1 1200) "\n")))))))

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
    (should-not (e-cache-efficiency-e2e--join-request-usage
                 requests (butlast usage)))))

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
         (requests nil) (semantic-result "unavailable")
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
                             (body (e-json-parse-string (plist-get context :body))))
                        (push (list :provider 'anthropic
                                    :endpoint (plist-get context :url)
                                    :model (plist-get body :model)
                                    :stable-prefix-fingerprint
                                    (e-cache-efficiency-e2e--static-fingerprint
                                     body (plist-get args :options)))
                              requests)
                        context)))
                   ((symbol-function 'e-openai--request-context)
                    (lambda (&rest args)
                      (let* ((context (apply native-openai args))
                             (body (plist-get context :body)))
                        (when (stringp body) (setq body (e-json-parse-string body)))
                        (push (list :provider 'openai
                                    :endpoint (plist-get context :url)
                                    :model (plist-get body :model)
                                    :stable-prefix-fingerprint
                                    (e-cache-efficiency-e2e--static-fingerprint
                                     body (plist-get args :options)))
                              requests)
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
        (let* ((usage (mapcar (lambda (event) (plist-get event :payload))
                             (e-live-e2e--activity-of-type
                              harness session-id 'token-usage)))
               (ordered (reverse requests))
               (evidence
                (e-cache-efficiency-e2e--join-request-usage ordered usage)))
          (setq cache-result
                (e-live-e2e--classify-growing-cache-evidence :evidence evidence))
          (message "E98_CACHE_EVIDENCE %s"
                   (e-json-serialize
                    (list :scenario "growing-tool-history"
                          :repository-revision (e-live-e2e--repository-revision)
                          :timestamp (format-time-string "%FT%TZ" nil t)
                          :elapsed-seconds (- (float-time) started-at)
                          :semantic-result semantic-result :cache-result cache-result
                          :requests (vconcat evidence))))))
      (when condition (signal (car condition) (cdr condition)))
      (cond ((equal cache-result "growing-history-reuse") t)
            ((equal cache-result "static-prefix-only")
             (ert-fail "Only static cache reads were observed as tool history grew."))
            (t (ert-skip (format "Growing cache benefit is unconfirmed: %s" cache-result)))))))

(provide 'e-cache-efficiency-e2e-test)
;;; e-cache-efficiency-e2e-test.el ends here
