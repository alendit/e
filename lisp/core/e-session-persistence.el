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
(define-error 'e-session-persistence-timeout "Session persistence timed out"
  'e-session-persistence-error)

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

(defcustom e-session-persistence-flush-timeout 5.0
  "Maximum seconds an explicit durability boundary waits for the writer."
  :type 'number
  :group 'e-session-persistence)

(cl-defstruct (e-session-persistence
               (:constructor e-session-persistence--create)
               (:conc-name e-session-persistence-))
  store process stderr-buffer input-fragment
  instance-id (next-sequence 0) (outbox (make-hash-table :test 'equal))
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

(defun e-session-persistence--send (controller request)
  "Send REQUEST to CONTROLLER's live writer."
  (process-send-string (e-session-persistence-process controller)
                       (concat (json-encode request) "\n")))

(defun e-session-persistence--ordered-outbox (controller)
  "Return CONTROLLER requests in submission order."
  (sort (hash-table-values (e-session-persistence-outbox controller))
        (lambda (left right)
          (< (plist-get left :sequence) (plist-get right :sequence)))))

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
                   (dolist (request (e-session-persistence--ordered-outbox controller))
                     (e-session-persistence--send controller request)))
               (error
                (setf (e-session-persistence-last-error controller) err)
                (e-session-persistence--restart-later controller))))))))

(defun e-session-persistence--handle-response (controller response)
  "Apply one writer RESPONSE to CONTROLLER."
  (let ((id (plist-get response :id)))
    (if (eq (plist-get response :ok) :json-false)
        (progn
          (setf (e-session-persistence-last-error controller)
                (list 'e-session-persistence-error
                      (or (plist-get response :error) "Writer rejected command")))
          (e-session-persistence--restart-later controller))
      (when (stringp id)
        (remhash id (e-session-persistence-outbox controller))
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

(defun e-session-persistence--submit (controller operation)
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
    (condition-case err
        (progn
          (e-session-persistence--ensure controller)
          (e-session-persistence--send controller request))
      (error
       (setf (e-session-persistence-last-error controller) err)
       (e-session-persistence--restart-later controller)))
    id))

(defun e-session-persistence-submit-record (controller session-id record)
  "Submit durable RECORD for SESSION-ID through CONTROLLER."
  (e-session-persistence--submit
   controller (list :op "append" :session-id session-id :record record)))

(defun e-session-persistence-request-checkpoint (controller)
  "Debounce a derived catalog checkpoint for CONTROLLER."
  (when-let ((timer (e-session-persistence-checkpoint-timer controller)))
    (cancel-timer timer))
  (setf (e-session-persistence-checkpoint-timer controller)
        (run-at-time
         (max 0 e-session-persistence-checkpoint-delay) nil
         (lambda ()
           (setf (e-session-persistence-checkpoint-timer controller) nil)
           (e-session-persistence--submit controller (list :op "checkpoint"))))))

(defun e-session-persistence-flush (controller &optional timeout)
  "Wait for CONTROLLER's current records and a catalog checkpoint.
This is for controlled durability boundaries, never ordinary interaction."
  (when-let ((timer (e-session-persistence-checkpoint-timer controller)))
    (cancel-timer timer)
    (setf (e-session-persistence-checkpoint-timer controller) nil))
  (e-session-persistence--submit controller (list :op "checkpoint"))
  (let ((deadline (+ (float-time) (or timeout e-session-persistence-flush-timeout))))
    (while (and (> (hash-table-count (e-session-persistence-outbox controller)) 0)
                (< (float-time) deadline))
      (accept-process-output (e-session-persistence--ensure controller) 0.02))
    (when (> (hash-table-count (e-session-persistence-outbox controller)) 0)
      (signal 'e-session-persistence-timeout
              (list "Session writer did not acknowledge durability boundary"
                    (e-session-persistence-last-error controller)))))
  controller)

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
