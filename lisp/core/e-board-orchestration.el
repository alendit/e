;;; e-board-orchestration.el --- Durable board run reducer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Durable orchestration is represented only by bounded board facts.  This file
;; validates that wire contract and reduces facts into a replayable projection;
;; it does not dispatch work, schedule timers, or cancel anything.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-board)

(define-error 'e-board-orchestration-error "Board orchestration error")
(define-error 'e-board-orchestration-invalid-fact
  "Invalid board orchestration fact" 'e-board-orchestration-error)

(defconst e-board-orchestration-fact-version 1
  "Current version of the durable orchestration board fact contract.")

(defconst e-board-orchestration-wire-version 1
  "Current JSON-safe encoding for orchestration payload attributes.")

(defconst e-board-orchestration-fact-byte-limit (* 8 1024)
  "Maximum encoded width retained by one orchestration fact payload.")

(defconst e-board-orchestration-summary-limit 240
  "Maximum width of a terminal report summary.")

(defconst e-board-orchestration-error-limit 480
  "Maximum width of a terminal report error.")

(defconst e-board-orchestration-output-limit 32
  "Maximum number of declared outputs in one terminal report.")

(defconst e-board-orchestration--fact-types
  '(manifest task-attempt terminal-report conflict continuation-claim)
  "Fact types understood by the orchestration reducer.")

(defvar e-board-orchestration--restoration-states (make-hash-table :test 'equal)
  "Durable board restoration state keyed by board id.")

(defun e-board-orchestration--invalid (field value)
  "Signal a contract error for FIELD with VALUE."
  (signal 'e-board-orchestration-invalid-fact (list field value)))

(defun e-board-orchestration--string (value field &optional allow-empty)
  "Return VALUE when it is a bounded string for FIELD, otherwise signal."
  (unless (and (stringp value)
               (or allow-empty (not (string-empty-p value)))
               (<= (string-bytes value) e-board-orchestration-fact-byte-limit))
    (e-board-orchestration--invalid field value))
  value)

(defun e-board-orchestration--attempt (value field)
  "Return VALUE when it is a nonnegative attempt number for FIELD."
  (unless (and (integerp value) (>= value 0))
    (e-board-orchestration--invalid field value))
  value)

(defun e-board-orchestration--list (value field)
  "Return VALUE as a list for FIELD, accepting vectors at the boundary."
  (unless (or (listp value) (vectorp value))
    (e-board-orchestration--invalid field value))
  (append value nil))

(defun e-board-orchestration--task (task)
  "Validate and normalize one manifest TASK."
  (unless (listp task) (e-board-orchestration--invalid :task task))
  (let* ((task-key (e-board-orchestration--string
                    (plist-get task :task-key) :task-key))
         (required (plist-get task :required))
         (accepted (e-board-orchestration--attempt
                    (or (plist-get task :accepted-attempt) 0)
                    :accepted-attempt)))
    (unless (memq required '(t nil))
      (e-board-orchestration--invalid :required required))
    (list :task-key task-key :required required :accepted-attempt accepted)))

(defun e-board-orchestration--deadline (deadline)
  "Validate and normalize a manifest DEADLINE policy."
  (unless (listp deadline) (e-board-orchestration--invalid :deadline deadline))
  (let ((kind (plist-get deadline :kind)))
    (pcase kind
      ('none (list :kind 'none))
      ('at (let ((at (plist-get deadline :at)))
             (unless (numberp at) (e-board-orchestration--invalid :deadline-at at))
             (list :kind 'at :at at)))
      (_ (e-board-orchestration--invalid :deadline-kind kind)))))

(defun e-board-orchestration--outputs (outputs)
  "Validate declared report OUTPUTS without interpreting their artifact schema."
  (let ((items (e-board-orchestration--list (or outputs []) :outputs)))
    (when (> (length items) e-board-orchestration-output-limit)
      (e-board-orchestration--invalid :outputs outputs))
    (dolist (output items)
      (unless (listp output) (e-board-orchestration--invalid :output output)))
    (copy-tree items)))

(defun e-board-orchestration--continuation (value run-id)
  "Validate a manifest continuation VALUE for RUN-ID, or return nil.
The publication key is persisted in the manifest so replay never derives a
new input identity for the same reconciliation request."
  (when value
    (unless (listp value) (e-board-orchestration--invalid :continuation value))
    (list :session-id (e-board-orchestration--string
                       (plist-get value :session-id) :continuation-session-id)
          :prompt (e-board-orchestration--string
                   (plist-get value :prompt) :continuation-prompt)
          :publication-key
          (e-board-orchestration--string
           (or (plist-get value :publication-key)
               (format "continuation:%s" run-id))
           :continuation-publication-key))))

(defun e-board-orchestration--descriptor (value)
  "Validate and copy an optional bounded application-owned run descriptor VALUE.
The orchestration core persists this plist for producer recovery but does not
interpret its fields."
  (when value
    (unless (and (proper-list-p value)
                 (zerop (% (length value) 2)))
      (e-board-orchestration--invalid :descriptor value))
    (let ((tail value)
          (seen (make-hash-table :test 'eq)))
      (while tail
        (let ((key (pop tail)))
          (unless (keywordp key)
            (e-board-orchestration--invalid :descriptor-key value))
          (when (gethash key seen)
            (e-board-orchestration--invalid :duplicate-descriptor-key key))
          (puthash key t seen))
        (unless tail
          (e-board-orchestration--invalid :descriptor-key value))
        (pop tail)))
    (let* ((wire-value (e-board-orchestration--wire-encode value))
           (encoded-width (string-bytes (prin1-to-string wire-value))))
      (when (> encoded-width e-board-orchestration-fact-byte-limit)
        (e-board-orchestration--invalid :descriptor value))
      (e-board-orchestration--wire-decode wire-value))))

(defun e-board-orchestration--wire-encode (value)
  "Encode orchestration VALUE without JSON list/object ambiguity."
  (cond
   ((or (null value) (eq value t) (stringp value) (numberp value)) value)
   ((symbolp value) (vector "symbol" (symbol-name value)))
   ((vectorp value)
    (vector "vector"
            (vconcat (mapcar #'e-board-orchestration--wire-encode value))))
   ((consp value)
    (unless (proper-list-p value)
      (e-board-orchestration--invalid :wire-value value))
    (vector "list"
            (vconcat (mapcar #'e-board-orchestration--wire-encode value))))
   (t (e-board-orchestration--invalid :wire-value value))))

(defun e-board-orchestration--wire-sequence (value field)
  "Return tagged wire VALUE as a two-item list, or signal for FIELD."
  (let ((items (cond ((vectorp value) (append value nil))
                     ((listp value) value)
                     (t nil))))
    (unless (= (length items) 2)
      (e-board-orchestration--invalid field value))
    items))

(defun e-board-orchestration--wire-decode (value)
  "Decode one exact orchestration wire VALUE after JSON replay."
  (if (or (null value) (eq value t) (stringp value) (numberp value))
      value
    (pcase-let* ((`(,tag ,contents)
                  (e-board-orchestration--wire-sequence value :wire-value)))
      (pcase tag
        ("symbol"
         (unless (stringp contents)
           (e-board-orchestration--invalid :wire-symbol contents))
         (intern contents))
        ((or "list" "vector")
         (unless (or (listp contents) (vectorp contents))
           (e-board-orchestration--invalid :wire-items contents))
         (let ((decoded
                (mapcar #'e-board-orchestration--wire-decode contents)))
           (if (equal tag "vector") (vconcat decoded) decoded)))
        (_ (e-board-orchestration--invalid :wire-tag tag))))))

(defun e-board-orchestration--legacy-enum (value allowed field)
  "Normalize legacy JSON VALUE against ALLOWED symbols for FIELD."
  (let ((normalized
         (if (stringp value)
             (cl-find value allowed :key #'symbol-name :test #'string=)
           value)))
    (unless (memq normalized allowed)
      (e-board-orchestration--invalid field value))
    normalized))

(defun e-board-orchestration--legacy-object-array (value)
  "Restore legacy JSON VALUE produced from a list of plists.
Emacs' JSON encoder represents such lists as objects with duplicate keys and
places each original plist tail in the corresponding property value."
  (let ((items (if (vectorp value) (append value nil) value)))
    (cond
     ((null items) nil)
     ((cl-every #'listp items) (copy-tree items))
     (t
      (cl-labels
          ((restore-tail
            (tail)
            (let ((tail (if (vectorp tail) (append tail nil) tail))
                  result)
              (while tail
                (let* ((raw-key (pop tail))
                       (key (if (stringp raw-key)
                                (intern (concat ":" raw-key))
                              raw-key))
                       (item (pop tail)))
                  (when (and (eq key :kind) (stringp item))
                    (setq item (intern item)))
                  (when (eq key :outputs)
                    (setq item (restore-array item)))
                  (setq result (append result (list key item)))))
              result))
           (restore-array
            (array)
            (let ((array (if (vectorp array) (append array nil) array))
                  result)
              (while array
                (let ((key (pop array))
                      (tail (pop array)))
                  (unless (keywordp key)
                    (e-board-orchestration--invalid
                     :legacy-object-array value))
                  (setq tail (if (vectorp tail) (append tail nil) tail))
                  (unless tail
                    (e-board-orchestration--invalid
                     :legacy-object-array value))
                  (let ((first (pop tail)))
                    (when (and (eq key :kind) (stringp first))
                      (setq first (intern first)))
                    (push (cons key (cons first (restore-tail tail))) result))))
              (nreverse result))))
        (restore-array items))))))

(defun e-board-orchestration--legacy-payload (type payload)
  "Normalize pre-wire orchestration PAYLOAD of TYPE after JSON replay."
  (let ((payload (copy-tree payload)))
    (pcase type
      ('manifest
       (plist-put payload :tasks
                  (e-board-orchestration--legacy-object-array
                   (plist-get payload :tasks)))
       (when-let ((deadline (plist-get payload :deadline)))
         (plist-put deadline :kind
                    (e-board-orchestration--legacy-enum
                     (plist-get deadline :kind) '(none at) :deadline-kind))))
      ('continuation-claim
       (plist-put payload :status
                  (e-board-orchestration--legacy-enum
                   (plist-get payload :status) '(pending published failed)
                   :continuation-status)))
      ((or 'task-attempt 'terminal-report)
       (plist-put payload :status
                  (e-board-orchestration--legacy-enum
                   (plist-get payload :status)
                   '(queued running done failed cancelled) :status))
       (when (eq type 'terminal-report)
         (plist-put payload :outputs
                    (e-board-orchestration--legacy-object-array
                     (plist-get payload :outputs))))))
    payload))

(defun e-board-orchestration-continuation-claim-key (publication-key status)
  "Return the immutable durable claim key for PUBLICATION-KEY at STATUS."
  (format "continuation-claim:%s:%s" publication-key status))

(defun e-board-orchestration-mark-restoring (board)
  "Mark BOARD as replaying durable facts before run state is exposed."
  (puthash (e-board-id board) 'restoring e-board-orchestration--restoration-states))

(defun e-board-orchestration-mark-restored (board)
  "Mark BOARD's durable fact replay complete."
  (puthash (e-board-id board) 'restored e-board-orchestration--restoration-states))

(defun e-board-orchestration-restoration-state (board)
  "Return BOARD's durable replay state, or `ready' for live boards."
  (or (gethash (e-board-id board) e-board-orchestration--restoration-states)
      'ready))

(defun e-board-orchestration-validate-fact (fact)
  "Validate and normalize versioned orchestration FACT.
FACT is a plist with `:type', `:payload', and an idempotency key.  The result is
safe to store in a board envelope and contains no runtime state."
  (unless (listp fact) (e-board-orchestration--invalid :fact fact))
  (let* ((version (or (plist-get fact :version) e-board-orchestration-fact-version))
         (type (plist-get fact :type))
         (payload (plist-get fact :payload))
         (key (e-board-orchestration--string
               (plist-get fact :idempotency-key) :idempotency-key)))
    (unless (= version e-board-orchestration-fact-version)
      (e-board-orchestration--invalid :version version))
    (unless (memq type e-board-orchestration--fact-types)
      (e-board-orchestration--invalid :type type))
    (unless (listp payload) (e-board-orchestration--invalid :payload payload))
    (let ((run-id (e-board-orchestration--string (plist-get payload :run-id) :run-id)))
      (pcase type
        ('manifest
         (let ((seen (make-hash-table :test 'equal))
               (tasks (mapcar #'e-board-orchestration--task
                              (e-board-orchestration--list (plist-get payload :tasks) :tasks)))
               (descriptor (e-board-orchestration--descriptor
                            (plist-get payload :descriptor))))
           (dolist (task tasks)
             (let ((task-key (plist-get task :task-key)))
               (when (gethash task-key seen)
                 (e-board-orchestration--invalid :duplicate-task-key task-key))
               (puthash task-key t seen)))
           (list :version version :type type :idempotency-key key
                 :payload
                 (append
                  (list :run-id run-id :tasks tasks
                        :deadline (e-board-orchestration--deadline
                                   (or (plist-get payload :deadline) '(:kind none)))
                        :continuation
                        (e-board-orchestration--continuation
                         (plist-get payload :continuation) run-id))
                  (when descriptor (list :descriptor descriptor))))))
        ('continuation-claim
         (let ((publication-key
                (e-board-orchestration--string
                 (plist-get payload :publication-key) :publication-key))
               (status (plist-get payload :status)))
           (unless (memq status '(pending published failed))
             (e-board-orchestration--invalid :continuation-status status))
           (list :version version :type type :idempotency-key key
                 :payload
                 (list :run-id run-id :publication-key publication-key :status status
                       :error (when-let ((error (plist-get payload :error)))
                                (truncate-string-to-width
                                 (format "%s" error)
                                 e-board-orchestration-error-limit nil nil "..."))))))
        ((or 'task-attempt 'terminal-report 'conflict)
         (let* ((task-key (e-board-orchestration--string
                           (plist-get payload :task-key) :task-key))
                (attempt (e-board-orchestration--attempt
                          (plist-get payload :attempt) :attempt))
                (status (plist-get payload :status)))
           (when (and (memq type '(task-attempt terminal-report))
                      (not (memq status '(queued running done failed cancelled))))
             (e-board-orchestration--invalid :status status))
           (when (and (eq type 'terminal-report)
                      (not (memq status '(done failed cancelled))))
             (e-board-orchestration--invalid :terminal-status status))
           (let ((body (append (list :run-id run-id :task-key task-key :attempt attempt)
                               (when status (list :status status)))) )
             (when (eq type 'terminal-report)
               (setq body
                     (append body
                             (list :summary (truncate-string-to-width
                                             (or (plist-get payload :summary) "")
                                             e-board-orchestration-summary-limit nil nil "...")
                                   :outputs (e-board-orchestration--outputs
                                             (plist-get payload :outputs))
                                   :error (when-let ((error (plist-get payload :error)))
                                            (truncate-string-to-width
                                             (format "%s" error)
                                             e-board-orchestration-error-limit nil nil "..."))))))
             (when (eq type 'conflict)
               (setq body
                     (append body
                             (list :reason (truncate-string-to-width
                                            (format "%s" (or (plist-get payload :reason) "conflict"))
                                            e-board-orchestration-error-limit nil nil "...")))))
             (list :version version :type type :idempotency-key key :payload body))))))))

(cl-defun e-board-orchestration-publish-fact (board fact &key author)
  "Validate and idempotently publish durable orchestration FACT to BOARD."
  (let* ((normalized (e-board-orchestration-validate-fact fact))
         (payload (plist-get normalized :payload))
         (source-key (list (format "orchestration:%s:%s"
                                          (plist-get payload :run-id)
                                          (plist-get normalized :type))
                                    (plist-get normalized :idempotency-key) 0)))
    (e-board-post-fact
     board :author author :tags '(orchestration)
     :attributes (list :orchestration-version e-board-orchestration-fact-version
                       :orchestration-wire-version e-board-orchestration-wire-version
                       :orchestration-type (symbol-name (plist-get normalized :type))
                       :orchestration-payload
                       (e-board-orchestration--wire-encode payload)
                       :orchestration-idempotency-key (plist-get normalized :idempotency-key))
     :content (format "Orchestration %s for run %s"
                      (plist-get normalized :type) (plist-get payload :run-id))
     :source-fact-key source-key)))

(defun e-board-orchestration-fact-from-message (message)
  "Return normalized orchestration fact from board MESSAGE, or nil."
  (when (and (eq (e-board-message-kind message) 'fact)
             (memq 'orchestration (e-board-message-tags message)))
    (let* ((attributes (e-board-message-attributes message))
           (wire-version (plist-get attributes :orchestration-wire-version))
           (type (e-board-orchestration--legacy-enum
                  (plist-get attributes :orchestration-type)
                  e-board-orchestration--fact-types :type))
           (payload
            (if wire-version
                (progn
                  (unless (and (integerp wire-version)
                               (= wire-version
                                  e-board-orchestration-wire-version))
                    (e-board-orchestration--invalid :wire-version wire-version))
                  (e-board-orchestration--wire-decode
                   (plist-get attributes :orchestration-payload)))
              (e-board-orchestration--legacy-payload
               type (plist-get attributes :orchestration-payload)))))
      (e-board-orchestration-validate-fact
       (list :version (plist-get attributes :orchestration-version)
             :type type :payload payload
             :idempotency-key
             (plist-get attributes :orchestration-idempotency-key))))))

(defun e-board-orchestration--task-projection (task attempts reports)
  "Reduce TASK with ATTEMPTS and REPORTS into one task projection."
  (let* ((task-key (plist-get task :task-key))
         (accepted-attempt (plist-get task :accepted-attempt))
         (report (cl-find-if (lambda (item)
                               (and (= (plist-get item :attempt) accepted-attempt)
                                    (equal (plist-get item :task-key) task-key)))
                             reports))
         (attempt (cl-find-if (lambda (item)
                                (and (= (plist-get item :attempt) accepted-attempt)
                                     (equal (plist-get item :task-key) task-key)))
                              attempts)))
    (append (copy-tree task)
            (list :state (or (plist-get report :status)
                             (plist-get attempt :status)
                             'pending)
                  :accepted-report (copy-tree report)))))

(defun e-board-orchestration-reduce (facts &optional now)
  "Reduce valid durable FACTS into one idempotent run projection.
FACTS may be normalized facts or board messages.  This pure reducer never
performs a clock-driven cancellation; a passed deadline is only evidence."
  (let ((manifests nil) (attempts nil) (reports nil) (conflicts nil) (claims nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (item facts)
      (let ((fact (if (e-board-message-p item)
                      (e-board-orchestration-fact-from-message item)
                    (e-board-orchestration-validate-fact item))))
        (when fact
          (let ((key (list (plist-get fact :type) (plist-get fact :idempotency-key))))
            (unless (gethash key seen)
              (puthash key t seen)
              (pcase (plist-get fact :type)
                ('manifest (push fact manifests))
                ('task-attempt (push (plist-get fact :payload) attempts))
                ('terminal-report (push (plist-get fact :payload) reports))
                ('conflict (push (plist-get fact :payload) conflicts))
                ('continuation-claim (push (plist-get fact :payload) claims))))))))
    (let* ((manifest-fact (car (nreverse manifests)))
           (manifest (and manifest-fact (plist-get manifest-fact :payload)))
           (run-id (and manifest (plist-get manifest :run-id))))
      (unless manifest (e-board-orchestration--invalid :manifest 'missing))
      (setq attempts (cl-remove-if-not (lambda (item) (equal (plist-get item :run-id) run-id)) attempts)
            reports (cl-remove-if-not (lambda (item) (equal (plist-get item :run-id) run-id)) reports)
            conflicts (cl-remove-if-not (lambda (item) (equal (plist-get item :run-id) run-id)) conflicts)
            claims (cl-remove-if-not (lambda (item) (equal (plist-get item :run-id) run-id)) claims))
      ;; Different reports for the same accepted task attempt are a visible conflict.
      (dolist (task (plist-get manifest :tasks))
        (let* ((key (plist-get task :task-key))
               (attempt (plist-get task :accepted-attempt))
               (matches (cl-remove-if-not (lambda (report)
                                            (and (equal key (plist-get report :task-key))
                                                 (= attempt (plist-get report :attempt)))) reports)))
          (when (> (length (delete-dups (mapcar (lambda (report) (prin1-to-string report)) matches))) 1)
            (push (list :run-id run-id :task-key key :attempt attempt
                        :reason "conflicting terminal reports") conflicts))))
      (let* ((tasks (mapcar (lambda (task)
                              (e-board-orchestration--task-projection task attempts reports))
                            (plist-get manifest :tasks)))
             (required (cl-remove-if-not (lambda (task) (plist-get task :required)) tasks))
             (all-terminal (cl-every (lambda (task)
                                       (memq (plist-get task :state) '(done failed cancelled)))
                                     required))
             (successful (and all-terminal (null conflicts)
                              (cl-every (lambda (task) (eq (plist-get task :state) 'done)) required)))
             (deadline (plist-get manifest :deadline))
             (deadline-state (and (eq (plist-get deadline :kind) 'at)
                                  (<= (plist-get deadline :at) (or now (float-time)))))
             (terminal-status (when all-terminal (if successful 'done 'failed))))
        (list :run-id run-id :manifest (copy-tree manifest) :tasks tasks
              :reports (copy-tree reports) :conflicts (nreverse conflicts)
              :deadline (append (copy-tree deadline) (list :expired deadline-state))
              :continuation
              (when-let ((continuation (plist-get manifest :continuation)))
                (let* ((publication-key (plist-get continuation :publication-key))
                       (matching (cl-remove-if-not
                                  (lambda (claim)
                                    (and (equal (plist-get claim :run-id) run-id)
                                         (equal (plist-get claim :publication-key)
                                                publication-key)))
                                  claims))
                       (published (cl-find 'published matching
                                           :key (lambda (claim) (plist-get claim :status))))
                       (failed (car (last (cl-remove-if-not
                                           (lambda (claim)
                                             (eq (plist-get claim :status) 'failed))
                                           matching)))))
                  (append (copy-tree continuation)
                          (list :state (cond (published 'published)
                                             (failed 'failed)
                                             (terminal-status 'pending)
                                             (t 'waiting))
                                :claims (copy-tree matching)))))
              :continuation-claims (copy-tree claims)
              :terminal-status terminal-status)))))

(defun e-board-orchestration--run-facts (board run-id)
  "Return BOARD facts that belong to RUN-ID.
The board journal may retain several completed runs.  Select before reduction
so an older run cannot be reported missing because a newer manifest exists."
  (cl-remove-if-not
   (lambda (message)
     (when-let ((fact (e-board-orchestration-fact-from-message message)))
       (equal run-id (plist-get (plist-get fact :payload) :run-id))))
   (e-board-messages board)))

(defun e-board-orchestration-run-projection (board run-id &optional now)
  "Return BOARD's bounded projection for RUN-ID without hiding restoration.
A board marked `restoring' has not replayed all persisted facts, so callers
must not treat its absent run as definitively missing."
  (if (eq (e-board-orchestration-restoration-state board) 'restoring)
      (list :run-id run-id :state 'not-restored-yet)
    (let ((facts (e-board-orchestration--run-facts board run-id)))
      (if facts
          (e-board-orchestration-reduce facts now)
        (list :run-id run-id :state 'missing)))))

(defun e-board-orchestration-run-ids (board)
  "Return the durable manifest run ids visible on BOARD after restoration."
  (unless (eq (e-board-orchestration-restoration-state board) 'restoring)
    (delete-dups
     (delq nil
           (mapcar (lambda (message)
                     (when-let* ((fact (e-board-orchestration-fact-from-message message))
                                 ((eq (plist-get fact :type) 'manifest)))
                       (plist-get (plist-get fact :payload) :run-id)))
                   (e-board-messages board))))))

(defun e-board-orchestration-project-board (board &optional now)
  "Reduce BOARD's most recent durable orchestration manifest into a projection."
  (e-board-orchestration-reduce (e-board-messages board) now))

(provide 'e-board-orchestration)

;;; e-board-orchestration.el ends here
