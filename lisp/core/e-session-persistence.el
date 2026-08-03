;;; e-session-persistence.el --- Asynchronous session persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session mutation remains synchronous and in-memory.  This module owns the
;; durable outbox and a small Node writer process.  The writer owns JSONL I/O
;; and derived catalog checkpoints, so neither runs in Emacs's UI event loop.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'e-session)

(define-error 'e-session-persistence-error "Session persistence error")
(defgroup e-session-persistence nil
  "Asynchronous persistent session storage."
  :group 'e-session)

(defcustom e-session-persistence-node-executable "node"
  "Node executable used by the bundled session writer."
  :type 'string
  :group 'e-session-persistence)

(defcustom e-session-persistence-checkpoint-delay 2.0
  "Seconds of quiet before requesting a derived catalog checkpoint."
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
    (signal 'e-session-persistence-error
            (list "Writer command exceeds pre-encoding budget")))
  (let ((encoded (concat (json-encode request) "\n")))
    (when (> (string-bytes encoded)
             e-session-persistence-command-byte-limit)
      (signal 'e-session-persistence-error
              (list "Writer command exceeds byte budget")))
    encoded))

(defun e-session-persistence--send (controller request)
  "Send one bounded REQUEST to CONTROLLER's live writer."
  (process-send-string (e-session-persistence-process controller)
                       (e-session-persistence--encode-command request)))

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

(defun e-session-persistence--catalog-result (result)
  "Normalize writer catalog RESULT into the core symbolic row contract."
  (cl-labels ((row (value)
                (when (listp value)
                  (let ((copy (copy-tree value)))
                    (when (stringp (plist-get copy :state))
                      (setq copy
                            (plist-put copy :state
                                       (intern (plist-get copy :state)))))
                    copy))))
    (if (and (listp result) (plist-member result :sessions))
        (plist-put (copy-sequence result) :sessions
                   (mapcar #'row (plist-get result :sessions)))
      (row result))))

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
         (request (and (stringp id)
                       (gethash id (e-session-persistence-outbox controller))))
         (callbacks (and (stringp id)
                         (gethash id (e-session-persistence-callbacks controller)))))
    (if (eq (plist-get response :ok) :json-false)
        (if (plist-get request :ephemeral)
            (progn
              (remhash id (e-session-persistence-outbox controller))
              (e-session-persistence--trim-outbox-order controller)
              (remhash id (e-session-persistence-callbacks controller))
              (e-session--adjust-unsettled-writes
               (e-session-persistence-store controller) -1)
              (when-let ((on-error (cdr callbacks)))
                (funcall on-error
                         (list 'e-session-persistence-error
                               (or (plist-get response :error)
                                   "Writer rejected command")))))
          (setf (e-session-persistence-last-error controller)
                (list 'e-session-persistence-error
                      (or (plist-get response :error) "Writer rejected command")))
          (e-session-persistence--restart-later controller))
      (when (stringp id)
        (when (gethash id (e-session-persistence-outbox controller))
          (remhash id (e-session-persistence-outbox controller))
          (e-session-persistence--trim-outbox-order controller)
          (remhash id (e-session-persistence-callbacks controller))
          (e-session--adjust-unsettled-writes
           (e-session-persistence-store controller) -1))
        (when-let ((on-done (car callbacks)))
          (funcall on-done
                   (if (equal (plist-get request :op) "catalog-page")
                       (e-session-persistence--catalog-result
                        (plist-get response :result))
                     (plist-get response :result))))
        (setf (e-session-persistence-last-error controller) nil)))))

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
                          operation)))
    (puthash id request (e-session-persistence-outbox controller))
    (e-session-persistence--append-outbox-id controller id)
    (when (or on-done on-error)
      (puthash id (cons on-done on-error)
               (e-session-persistence-callbacks controller)))
    (e-session--adjust-unsettled-writes
     (e-session-persistence-store controller) 1)
    (condition-case err
        (progn
          (e-session-persistence--ensure controller)
          (e-session-persistence--send controller request))
      (error
       (setf (e-session-persistence-last-error controller) err)
       (e-session-persistence--restart-later controller)))
    id))

(defun e-session-persistence-catalog-request
    (controller arguments on-done on-error)
  "Submit one bounded catalog ARGUMENTS request through CONTROLLER."
  (let ((operation (plist-get arguments :operation)))
    (unless (memq operation '(list-page preflight-page read))
      (signal 'e-session-persistence-error
              (list "Unsupported session catalog operation" operation)))
    (e-session-persistence--submit
     controller
     (list :op "catalog-page" :ephemeral t
           :catalog-operation (symbol-name operation)
           :after (plist-get arguments :after)
           :session-id (plist-get arguments :session-id)
           :limit (plist-get arguments :limit))
     on-done on-error)
    (lambda () nil)))

(defun e-session-persistence-submit-record (controller session-id record)
  "Submit durable RECORD for SESSION-ID through CONTROLLER."
  (e-session-persistence--submit
   controller (list :op "append" :session-id session-id :record record)))

(defun e-session-persistence-declare-board-state
    (controller session-id controller-principal &optional board-id)
  "Persist current dormant board schema for SESSION-ID.
CONTROLLER-PRINCIPAL is trusted host policy, never inferred from transcript
content.  The derived catalog checkpoint remains asynchronous."
  (let ((store (e-session-persistence-store controller)))
    (e-session-declare-board-state
     store session-id controller-principal board-id)
    (e-session-persistence-request-checkpoint controller)))

(defun e-session-persistence-request-checkpoint (controller)
  "Debounce a derived catalog checkpoint for CONTROLLER."
  (if-let ((timer (e-session-persistence-checkpoint-timer controller)))
      (cancel-timer timer)
    (e-session--adjust-unsettled-writes
     (e-session-persistence-store controller) 1))
  (setf (e-session-persistence-checkpoint-timer controller)
        (run-at-time
         (max 0 e-session-persistence-checkpoint-delay) nil
         (lambda ()
           ;; Transfer ownership from the timer to the outbox without exposing
           ;; a false quiescent edge between the two states.
           (e-session-persistence--submit controller (list :op "checkpoint"))
           (setf (e-session-persistence-checkpoint-timer controller) nil)
           (e-session--adjust-unsettled-writes
            (e-session-persistence-store controller) -1)))))

(defun e-session-persistence-finalize (controller on-done on-error)
  "Asynchronously finalize CONTROLLER's current durability boundary.
Call ON-DONE after the writer acknowledges the checkpoint, or ON-ERROR if the
writer rejects it.  Return the stable checkpoint command id."
  (let ((timer (e-session-persistence-checkpoint-timer controller)))
    (when timer (cancel-timer timer))
    (prog1
        (e-session-persistence--submit
         controller (list :op "checkpoint") on-done on-error)
      (when timer
        (setf (e-session-persistence-checkpoint-timer controller) nil)
        (e-session--adjust-unsettled-writes
         (e-session-persistence-store controller) -1)))))

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
