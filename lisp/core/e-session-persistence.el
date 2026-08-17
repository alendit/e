;;; e-session-persistence.el --- Asynchronous session persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session mutation remains synchronous and in-memory.  This module owns the
;; durable outbox and a small Node writer process.  The writer owns JSONL I/O,
;; atomic resume checkpoints, and the derived session catalog, so none of that
;; work runs in Emacs's UI event loop.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'e-session)

(define-error 'e-session-persistence-error "Session persistence error")
(define-error 'e-session-persistence-command-error
  "Invalid session persistence command"
  'e-session-persistence-error)
(defgroup e-session-persistence nil
  "Asynchronous persistent session storage."
  :group 'e-session)

(defcustom e-session-persistence-node-executable "node"
  "Node executable used by the bundled session writer."
  :type 'string
  :group 'e-session-persistence)

(defcustom e-session-persistence-checkpoint-delay 2.0
  "Seconds of quiet before requesting resume and catalog checkpoints."
  :type 'number
  :group 'e-session-persistence)

(defcustom e-session-persistence-retry-delay 1.0
  "Seconds before restarting a failed writer with unacknowledged commands."
  :type 'number
  :group 'e-session-persistence)

(defcustom e-session-persistence-retry-page-size 32
  "Maximum writer requests resent by one retry callback."
  :type 'integer
  :group 'e-session-persistence)

(defcustom e-session-persistence-command-byte-limit (* 256 1024)
  "Maximum encoded size of one writer command."
  :type 'integer
  :group 'e-session-persistence)

(defcustom e-session-persistence-command-node-limit 4096
  "Maximum Lisp value nodes inspected before encoding one writer command."
  :type 'integer
  :group 'e-session-persistence)

(cl-defstruct (e-session-persistence
               (:constructor e-session-persistence--create)
               (:conc-name e-session-persistence-))
  store process stderr-buffer input-fragment
  instance-id (next-sequence 0) (outbox (make-hash-table :test 'equal))
  outbox-head outbox-tail retry-cursor
  (callbacks (make-hash-table :test 'equal))
  checkpoint-timer retry-timer last-error)

(cl-defstruct (e-session-persistence-command
               (:constructor e-session-persistence-command--create)
               (:conc-name e-session-persistence-command-))
  "One validated writer request and its immutable wire representation."
  request wire)

(defun e-session-persistence--directory ()
  "Return the directory containing this library."
  (file-name-directory
   (file-truename (or load-file-name buffer-file-name
                      (locate-library "e-session-persistence")
                      default-directory))))

(defun e-session-persistence--writer-script ()
  "Return the bundled writer program path."
  (expand-file-name "e-session-writer.mjs" (e-session-persistence--directory)))

(defun e-session-persistence--command ()
  "Return argv for a persistent session writer."
  (let ((node (executable-find e-session-persistence-node-executable)))
    (unless node
      (signal 'e-session-persistence-error
              (list (format "Cannot find Node executable %s"
                            e-session-persistence-node-executable))))
    (list node (e-session-persistence--writer-script))))

(defun e-session-persistence--live-p (controller)
  "Return non-nil when CONTROLLER's writer is live."
  (let ((process (e-session-persistence-process controller)))
    (and process (process-live-p process))))

(defun e-session-persistence--command-within-budget-p (request)
  "Return non-nil when REQUEST fits fixed pre-encoding budgets."
  (let ((pending (list request))
        (nodes 0)
        (string-bytes 0)
        valid)
    (setq valid t)
    (while (and pending valid)
      (let ((value (pop pending)))
        (setq nodes (1+ nodes))
        (when (> nodes e-session-persistence-command-node-limit)
          (setq valid nil))
        (cond
         ((stringp value)
          (setq string-bytes (+ string-bytes (string-bytes value)))
          (when (> string-bytes e-session-persistence-command-byte-limit)
            (setq valid nil)))
         ((consp value)
          (push (car value) pending)
          (push (cdr value) pending))
         ((vectorp value)
          (dotimes (index (length value))
            (push (aref value index) pending))))))
    valid))

(defun e-session-persistence--encode-command (request)
  "Return REQUEST as one bounded newline-terminated writer command."
  (unless (e-session-persistence--command-within-budget-p request)
    (signal 'e-session-persistence-command-error
            (list "Writer command exceeds pre-encoding budget")))
  (let ((encoded
         (condition-case err
             (concat (json-encode request) "\n")
           (json-error
            (signal 'e-session-persistence-command-error
                    (list "Writer command is not JSON-encodable" err))))))
    (when (> (string-bytes encoded)
             e-session-persistence-command-byte-limit)
      (signal 'e-session-persistence-command-error
              (list "Writer command exceeds byte budget")))
    encoded))

(defun e-session-persistence--prepare-command (request)
  "Return a validated immutable writer command for REQUEST."
  (e-session-persistence-command--create
   :request request
   :wire (e-session-persistence--encode-command request)))

(defun e-session-persistence--send (controller command)
  "Send one prepared COMMAND to CONTROLLER's live writer.
Raw request plists remain accepted for controllers created before a live
reload; new submissions always prepare once before entering the outbox."
  (process-send-string (e-session-persistence-process controller)
                       (if (e-session-persistence-command-p command)
                           (e-session-persistence-command-wire command)
                         (e-session-persistence--encode-command command))))

(defun e-session-persistence--append-outbox-id (controller id)
  "Append ID to CONTROLLER's O(1) retry-order queue."
  (let ((cell (list id)))
    (if-let ((tail (e-session-persistence-outbox-tail controller)))
        (setcdr tail cell)
      (setf (e-session-persistence-outbox-head controller) cell))
    (setf (e-session-persistence-outbox-tail controller) cell)))

(defun e-session-persistence--trim-outbox-order (controller)
  "Drop acknowledged ids from the front of CONTROLLER's retry queue."
  (let ((head (e-session-persistence-outbox-head controller))
        (outbox (e-session-persistence-outbox controller)))
    (while (and head (not (gethash (car head) outbox)))
      (setq head (cdr head)))
    (setf (e-session-persistence-outbox-head controller) head)
    (unless head
      (setf (e-session-persistence-outbox-tail controller) nil))))

(defun e-session-persistence--resend-page (controller)
  "Resend one fixed retry page for CONTROLLER and yield between pages."
  (let ((cursor (e-session-persistence-retry-cursor controller))
        (scanned 0))
    (while (and cursor (< scanned e-session-persistence-retry-page-size))
      (when-let ((request (gethash (car cursor)
                                  (e-session-persistence-outbox controller))))
        (e-session-persistence--send controller request))
      (setq cursor (cdr cursor)
            scanned (1+ scanned)))
    (setf (e-session-persistence-retry-cursor controller) cursor)
    (when cursor
      (run-at-time 0 nil
                   (lambda ()
                     (when (e-session-persistence--live-p controller)
                       (e-session-persistence--resend-page controller)))))))

(defun e-session-persistence--restart-later (controller)
  "Retry CONTROLLER's writer when it still has work."
  (when (and (> (hash-table-count (e-session-persistence-outbox controller)) 0)
             (not (timerp (e-session-persistence-retry-timer controller))))
    (setf (e-session-persistence-retry-timer controller)
          (run-at-time
           e-session-persistence-retry-delay nil
           (lambda ()
             (setf (e-session-persistence-retry-timer controller) nil)
             (condition-case err
                 (progn
                   (e-session-persistence--ensure controller)
                   (setf (e-session-persistence-retry-cursor controller)
                         (e-session-persistence-outbox-head controller))
                   (e-session-persistence--resend-page controller))
               (error
                (setf (e-session-persistence-last-error controller) err)
                (e-session-persistence--restart-later controller))))))))

(defun e-session-persistence--handle-response (controller response)
  "Apply one writer RESPONSE to CONTROLLER."
  (let* ((id (plist-get response :id))
         (callbacks (and (stringp id)
                         (gethash id (e-session-persistence-callbacks controller)))))
    (if (eq (plist-get response :ok) :json-false)
        (let ((err (list 'e-session-persistence-error
                         (or (plist-get response :error)
                             "Writer rejected command"))))
          (setf (e-session-persistence-last-error controller) err)
          (if (eq (plist-get response :retryable) :json-false)
              (progn
                (when (and (stringp id)
                           (gethash id (e-session-persistence-outbox controller)))
                  (remhash id (e-session-persistence-outbox controller))
                  (e-session-persistence--trim-outbox-order controller)
                  (remhash id (e-session-persistence-callbacks controller))
                  (e-session--adjust-unsettled-writes
                   (e-session-persistence-store controller) -1))
                (when-let ((on-error (cdr callbacks)))
                  (funcall on-error err))
                (unless (cdr callbacks)
                  (display-warning 'e-session-persistence
                                   (error-message-string err)
                                   :error)))
            ;; Older writers omit `retryable'; preserve their retry behavior.
            (e-session-persistence--restart-later controller)))
      (when (stringp id)
        (when (gethash id (e-session-persistence-outbox controller))
          (remhash id (e-session-persistence-outbox controller))
          (e-session-persistence--trim-outbox-order controller)
          (remhash id (e-session-persistence-callbacks controller))
          (e-session--adjust-unsettled-writes
           (e-session-persistence-store controller) -1))
        (when-let ((on-done (car callbacks)))
          (funcall on-done (plist-get response :result)))
        (setf (e-session-persistence-last-error controller) nil)))))

(defun e-session-persistence-status (controller)
  "Return bounded operational status for persistence CONTROLLER."
  (let ((err (e-session-persistence-last-error controller)))
    (list :writer-live (and (e-session-persistence--live-p controller) t)
          :outbox-count (hash-table-count
                         (e-session-persistence-outbox controller))
          :retry-pending (and (timerp
                               (e-session-persistence-retry-timer controller))
                              t)
          :last-error (and err
                           (condition-case nil
                               (error-message-string err)
                             (error (format "%S" err)))))))

(defun e-session-persistence--consume-output (controller text)
  "Consume newline-delimited writer protocol TEXT."
  (let ((input (concat (or (e-session-persistence-input-fragment controller) "") text)))
    (while (string-match "\n" input)
      (let ((line (substring input 0 (match-beginning 0))))
        (setq input (substring input (match-end 0)))
        (unless (string-empty-p line)
          (condition-case err
              (e-session-persistence--handle-response
               controller
               (json-parse-string line :object-type 'plist :array-type 'list
                                  :null-object nil :false-object :json-false))
            (error
             (setf (e-session-persistence-last-error controller) err))))))
    (setf (e-session-persistence-input-fragment controller) input)))

(defun e-session-persistence--ensure (controller)
  "Start and return CONTROLLER's writer process."
  (unless (e-session-persistence--live-p controller)
    (let ((stderr (generate-new-buffer " *e-session-writer-stderr*")))
      (setf (e-session-persistence-stderr-buffer controller) stderr
            (e-session-persistence-process controller)
            (make-process
             :name "e-session-writer"
             :buffer nil :stderr stderr :command (e-session-persistence--command)
             :connection-type 'pipe :coding 'utf-8-unix :noquery t
             :filter (lambda (_process text)
                       (e-session-persistence--consume-output controller text))
             :sentinel (lambda (_process _event)
                         (unless (e-session-persistence--live-p controller)
                           (e-session-persistence--restart-later controller)))))
      (set-process-query-on-exit-flag (e-session-persistence-process controller) nil)))
  (e-session-persistence-process controller))

(defun e-session-persistence--submit (controller operation &optional on-done on-error)
  "Queue OPERATION for CONTROLLER and return its stable command id."
  (let* ((sequence (cl-incf (e-session-persistence-next-sequence controller)))
         ;; The writer deduplicates this value after an Emacs restart.  A local
         ;; counter would collide with a prior controller's acknowledged work.
         (id (format "%s:%d" (e-session-persistence-instance-id controller)
                     sequence))
         (request (append (list :id id :sequence sequence
                                :directory (e-session-store-directory
                                            (e-session-persistence-store controller)))
                          operation))
         (command
          (condition-case err
              (e-session-persistence--prepare-command request)
            (e-session-persistence-command-error
             (setf (e-session-persistence-last-error controller) err)
             (when on-error
               (funcall on-error err))
             (signal (car err) (cdr err))))))
    ;; Only transport-ready commands become durable outbox obligations.
    (puthash id command (e-session-persistence-outbox controller))
    (e-session-persistence--append-outbox-id controller id)
    (when (or on-done on-error)
      (puthash id (cons on-done on-error)
               (e-session-persistence-callbacks controller)))
    (e-session--adjust-unsettled-writes
     (e-session-persistence-store controller) 1)
    (condition-case err
        (progn
          (e-session-persistence--ensure controller)
          (e-session-persistence--send controller command))
      (error
       (setf (e-session-persistence-last-error controller) err)
       (e-session-persistence--restart-later controller)))
    id))

(defun e-session-persistence-submit-record (controller session-id record)
  "Submit durable RECORD for SESSION-ID through CONTROLLER."
  (e-session-persistence--submit
   controller (list :op "append" :session-id session-id :record record)))

(defun e-session-persistence--checkpoint-operation (controller session-id)
  "Return one bounded writer checkpoint operation for CONTROLLER SESSION-ID."
  (let ((store (e-session-persistence-store controller)))
    (list :op "checkpoint"
          :sessions (vector (e-session-checkpoint-manifest store session-id)))))

(defun e-session-persistence--reindex-operation ()
  "Return the writer operation used as a checkpoint-batch index barrier."
  (list :op "reindex"))

(defun e-session-persistence--submit-checkpoint-batch
    (controller on-done on-error)
  "Submit dirty checkpoints, then one reindex barrier, for CONTROLLER.

Each session manifest travels in its own bounded command.  The writer handles
commands serially, so acknowledging the final reindex means every preceding
checkpoint is durable.  Call ON-DONE after that barrier or ON-ERROR once on the
first terminal failure.  The caller owns one unsettled-write slot spanning the
whole batch; each submitted writer command owns its ordinary outbox slot."
  (let* ((store (e-session-persistence-store controller))
         (session-ids (e-session-checkpoint-dirty-session-ids store))
         (remaining (copy-sequence session-ids))
         (settled nil)
         first-command-id)
    (cl-labels
        ((fail (err)
           (unless settled
             (setq settled t)
             ;; A failed checkpoint or reindex leaves the batch retryable.
             ;; Mutations made after an earlier manifest was submitted already
             ;; re-added their ids; `puthash' safely coalesces both cases.
             (dolist (session-id session-ids)
               (e-session--mark-checkpoint-dirty store session-id))
             (funcall on-error err)))
         (finish (value)
           (unless settled
             (setq settled t)
             (funcall on-done value)))
         (submit-operation (operation success)
           (condition-case err
               (let ((command-id
                      (e-session-persistence--submit
                       controller operation success #'fail)))
                 (unless first-command-id
                   (setq first-command-id command-id))
                 command-id)
             ;; Preflight invokes FAIL before re-signalling.  The settlement
             ;; guard makes this catch idempotent and keeps timer callbacks from
             ;; leaking their batch-level unsettled slot.
             (error
              (fail err)
              nil)))
         (submit-next (&optional _value)
           (unless settled
             (condition-case err
                 (if-let ((session-id (pop remaining)))
                     (when (submit-operation
                            (e-session-persistence--checkpoint-operation
                             controller session-id)
                            #'submit-next)
                       ;; Transfer this snapshot out of the dirty set before the
                       ;; event loop can observe its acknowledgement.  A later
                       ;; mutation marks it dirty again; a failure re-marks the
                       ;; whole captured batch in FAIL.
                       (e-session-checkpoint-mark-clean store (list session-id)))
                   (submit-operation
                    (e-session-persistence--reindex-operation) #'finish))
               (error (fail err))))))
      (submit-next)
      first-command-id)))

(defun e-session-persistence-declare-board-state
    (controller session-id principal board-id)
  "Persist board identity for SESSION-ID.
PRINCIPAL is trusted host policy, never inferred from transcript content.  The
derived session-index checkpoint remains asynchronous."
  (let ((store (e-session-persistence-store controller)))
    (e-session-declare-board-state
     store session-id principal board-id)))

(defun e-session-persistence-request-checkpoint (controller)
  "Debounce a derived session-index checkpoint for CONTROLLER."
  (if-let ((timer (e-session-persistence-checkpoint-timer controller)))
      (cancel-timer timer)
    (e-session--adjust-unsettled-writes
     (e-session-persistence-store controller) 1))
  (setf (e-session-persistence-checkpoint-timer controller)
        (run-at-time
         (max 0 e-session-persistence-checkpoint-delay) nil
         (lambda ()
           ;; Keep the timer's ownership slot across the full command series so
           ;; acknowledgements cannot expose false quiescent edges between
           ;; checkpoints and the final reindex barrier.
           (setf (e-session-persistence-checkpoint-timer controller) nil)
           (e-session-persistence--submit-checkpoint-batch
            controller
            (lambda (_value)
              (e-session--adjust-unsettled-writes
               (e-session-persistence-store controller) -1))
            (lambda (err)
              (e-session--adjust-unsettled-writes
               (e-session-persistence-store controller) -1)
              (display-warning 'e-session-persistence
                               (error-message-string err)
                               :error)))))))

(defun e-session-persistence-finalize (controller on-done on-error)
  "Asynchronously finalize CONTROLLER's current durability boundary.
Call ON-DONE after the writer acknowledges every dirty session checkpoint and
the final reindex barrier, or ON-ERROR if the writer rejects one.  Return the
first stable command id in that batch."
  (let ((timer (e-session-persistence-checkpoint-timer controller)))
    (when timer (cancel-timer timer))
    ;; A scheduled checkpoint already owns one slot.  Otherwise establish the
    ;; batch slot before submitting its first outbox command.
    (unless timer
      (e-session--adjust-unsettled-writes
       (e-session-persistence-store controller) 1))
    (setf (e-session-persistence-checkpoint-timer controller) nil)
    (e-session-persistence--submit-checkpoint-batch
     controller
     (lambda (value)
       (e-session--adjust-unsettled-writes
        (e-session-persistence-store controller) -1)
       (funcall on-done value))
     (lambda (err)
       (e-session--adjust-unsettled-writes
        (e-session-persistence-store controller) -1)
       (funcall on-error err)))))

(defun e-session-persistence-enable (store)
  "Attach and return an asynchronous persistence controller for STORE."
  (unless (e-session-store-persistent store)
    (signal 'e-session-persistence-error (list "Store is not persistent")))
  (or (e-session-store-persistence-controller store)
      (let ((controller (e-session-persistence--create
                         :store store :instance-id (e-session-generate-ulid))))
        (setf (e-session-store-persistence-controller store) controller)
        controller)))

(provide 'e-session-persistence)

;;; e-session-persistence.el ends here
