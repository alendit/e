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

(defun e-cache-efficiency-e2e--tool-register (registry)
  "Register a data-dependent probe with substantial reproducible output."
  (e-tools-register
   registry :name "cache_probe"
   :description "Read one synthetic cache probe. Call with step 1, then step 2."
   :parameters '(:type "object" :required ["step"]
                 :properties (:step (:type "integer" :enum [1 2])))
   :work
   (e-tools-cheap-work
    "e2e.cache-probe"
    (lambda (arguments)
      (let ((step (plist-get arguments :step)))
        (unless (memq step '(1 2)) (error "Invalid probe step"))
        (concat (format "PROBE-%d-CODE=%d\n" step (+ 731 step))
                (mapconcat
                 (lambda (index) (format "row-%d-%d synthetic cache evidence" step index))
                 (number-sequence 1 1200) "\n")))))))

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
                         (concat "Call cache_probe with step 1. Wait for its result, "
                                 "then call cache_probe with step 2. Finally return "
                                 "only the sum of their PROBE codes. Do not call other tools.")
                         deadline))
                       (answer (e-live-e2e--assistant-content result)))
                  (should (string-match-p "1465" answer))
                  (setq semantic-result "pass")))
            (error (setq condition caught)))
        (let* ((usage (mapcar (lambda (event) (plist-get event :payload))
                             (e-live-e2e--activity-of-type
                              harness session-id 'token-usage)))
               (ordered (reverse requests))
               (evidence
                (and (= (length ordered) (length usage))
                     (cl-mapcar (lambda (request counters) (append request counters))
                                ordered usage))))
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
