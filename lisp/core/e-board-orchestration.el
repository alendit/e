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
                              (e-board-orchestration--list (plist-get payload :tasks) :tasks))))
           (dolist (task tasks)
             (let ((task-key (plist-get task :task-key)))
               (when (gethash task-key seen)
                 (e-board-orchestration--invalid :duplicate-task-key task-key))
               (puthash task-key t seen)))
           (list :version version :type type :idempotency-key key
                 :payload (list :run-id run-id :tasks tasks
                                :deadline (e-board-orchestration--deadline
                                           (or (plist-get payload :deadline) '(:kind none)))
                                :continuation (copy-tree (plist-get payload :continuation))))))
        ((or 'task-attempt 'terminal-report 'conflict 'continuation-claim)
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
             (list :version version :type type :idempotency-key key :payload body)))))))

)
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
                       :orchestration-type (plist-get normalized :type)
                       :orchestration-payload payload
                       :orchestration-idempotency-key (plist-get normalized :idempotency-key))
     :content (format "Orchestration %s for run %s"
                      (plist-get normalized :type) (plist-get payload :run-id))
     :source-fact-key source-key)))

(defun e-board-orchestration-fact-from-message (message)
  "Return normalized orchestration fact from board MESSAGE, or nil."
  (when (and (eq (e-board-message-kind message) 'fact)
             (memq 'orchestration (e-board-message-tags message)))
    (e-board-orchestration-validate-fact
     (list :version (plist-get (e-board-message-attributes message) :orchestration-version)
           :type (plist-get (e-board-message-attributes message) :orchestration-type)
           :payload (plist-get (e-board-message-attributes message) :orchestration-payload)
           :idempotency-key (plist-get (e-board-message-attributes message)
                                       :orchestration-idempotency-key)))))

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
              :continuation-claims (copy-tree claims)
              :terminal-status terminal-status)))))

(defun e-board-orchestration-project-board (board &optional now)
  "Reduce BOARD's persisted facts into one durable orchestration projection."
  (e-board-orchestration-reduce (e-board-messages board) now))

(provide 'e-board-orchestration)

;;; e-board-orchestration.el ends here
