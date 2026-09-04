;;; e-runtime-store.el --- Typed subordinate runtime-store adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns one batch-Emacs worker, typed request framing, bounded scheduling,
;; failure propagation, liveness, and runtime ownership.  This module knows
;; no session, Board, resource, or tool policy and never opens SQLite itself.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-runtime-store-codec)

(define-error 'e-runtime-store-error "Runtime store error")
(define-error 'e-runtime-store-unavailable "Runtime store worker is unavailable"
  'e-runtime-store-error)
(define-error 'e-runtime-store-timeout "Runtime store request timed out"
  'e-runtime-store-unavailable)
(define-error 'e-runtime-store-protocol-error "Runtime store protocol failed"
  'e-runtime-store-unavailable)
(define-error 'e-runtime-store-cancelled "Runtime store request was cancelled"
  'e-runtime-store-error)
(define-error 'e-runtime-store-worker-error "Runtime store worker error"
  'e-runtime-store-error)
(define-error 'e-runtime-store-owner-active "Runtime store already has a live owner"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-board-conflict "Runtime store Board conflict"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-task-conflict "Runtime store task conflict"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-cron-conflict "Runtime store cron conflict"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-goodnite-conflict "Runtime store Goodnite conflict"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-raw-conflict "Runtime store raw-result conflict"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-resource-too-large "Runtime store resource is too large"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-schema-too-old "Runtime store schema requires explicit upgrade"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-schema-too-new "Runtime store schema is newer than this runtime"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-migration-required
  "Offline runtime migration is required" 'e-runtime-store-error)

(defcustom e-runtime-store-request-timeout 60.0
  "Maximum seconds for each bounded runtime-store request phase.
Queue admission and submitted execution each receive this interval.  A
submitted timeout freezes the store until explicit close/reopen and reload."
  :type 'number :group 'e)

(cl-defstruct (e-runtime-store
               (:constructor e-runtime-store--create)
               (:predicate e-runtime-store-p)
               (:conc-name e-runtime-store--))
  directory database-file runtime-id process opened-process stderr-buffer input-fragment
  (sequence 0) pending write-queue read-queue active-request
  starting-request last-error unavailable-cause startup-status unavailable closed)

(cl-defstruct (e-runtime-store-request
               (:constructor e-runtime-store-request--create)
               (:predicate e-runtime-store-request-p)
               (:conc-name e-runtime-store-request--))
  id kind body state result error admitted-at submitted-at)

(defun e-runtime-store--worker-file ()
  "Return the newest installed worker source or byte-code path.

Source-adjacent byte code can legitimately lag a checkout.  The subordinate
process must execute the same repository revision as its client without
deleting or rewriting those unrelated artifacts."
  (let* ((library (locate-library "e-runtime-store-worker"))
         (source (and library
                      (string-suffix-p ".elc" library)
                      (substring library 0 -1))))
    (or (and source (file-exists-p source)
             (file-newer-than-file-p source library)
             source)
        library
        (signal 'e-runtime-store-error (list "Worker module is missing")))))

(defun e-runtime-store--emacs-program ()
  "Return the current Emacs executable path."
  (expand-file-name invocation-name invocation-directory))

(defun e-runtime-store--command ()
  "Return the private batch worker command."
  (list (e-runtime-store--emacs-program) "--batch" "-Q"
        "-L" (file-name-directory (e-runtime-store--worker-file))
        "--eval" "(setq load-prefer-newer t)"
        "-l" (e-runtime-store--worker-file)
        "--funcall" "e-runtime-store-worker-main"))

(defun e-runtime-store--pack (value)
  "Return VALUE as one ASCII protocol frame."
  (concat (base64-encode-string (e-runtime-store-codec-encode value) t) "\n"))

(defun e-runtime-store--unpack (text)
  "Decode one ASCII protocol frame TEXT."
  (e-runtime-store-codec-decode (base64-decode-string text)))

(defun e-runtime-store--live-p (store)
  "Return non-nil when STORE has a live worker process."
  (and (processp (e-runtime-store--process store))
       (process-live-p (e-runtime-store--process store))))

(defun e-runtime-store-live-p (store)
  "Return non-nil when STORE's worker is live and effects are not frozen."
  (and (e-runtime-store-p store)
       (not (e-runtime-store--closed store))
       (not (e-runtime-store--unavailable store))
       (e-runtime-store--live-p store)))

(defun e-runtime-store--request-operation (request)
  "Return REQUEST's typed operation, or nil when it has no operation body."
  (and request
       (plist-get (e-runtime-store-request--body request) :op)))

(defun e-runtime-store--failure-request (store &optional fallback)
  "Return the request whose identity explains STORE's current failure.

An internal open is only transport setup for the scheduler-selected request,
so its failure reports that selected request while it remains queued."
  (or (e-runtime-store--starting-request store)
      (e-runtime-store--active-request store)
      fallback))

(defun e-runtime-store--request-error (type message request &rest properties)
  "Build TYPE MESSAGE with REQUEST identity and additional PROPERTIES."
  (append (list type message
                :operation (e-runtime-store--request-operation request)
                :kind (and request (e-runtime-store-request--kind request))
                :request-id (and request (e-runtime-store-request--id request)))
          properties))

(defun e-runtime-store--startup-error (request cause)
  "Return the typed startup failure for scheduler-selected REQUEST and CAUSE."
  (e-runtime-store--request-error
   'e-runtime-store-unavailable
   (format "Worker startup failed before submitting %s"
           (or (e-runtime-store--request-operation request) "request"))
   request :cause cause))

(defun e-runtime-store--protocol-error (store protocol-cause &rest properties)
  "Return one typed protocol failure for STORE and PROTOCOL-CAUSE."
  (let ((request (e-runtime-store--failure-request store)))
    (append
     (list 'e-runtime-store-protocol-error
           (format "Runtime-store protocol %s before acknowledgement of %s"
                   protocol-cause
                   (or (e-runtime-store--request-operation request) "request"))
           :operation (e-runtime-store--request-operation request)
           :kind (and request (e-runtime-store-request--kind request))
           :request-id (and request (e-runtime-store-request--id request))
           :protocol-cause protocol-cause)
     properties)))

(defun e-runtime-store--response-valid-p (response)
  "Return non-nil when RESPONSE has the complete worker response shape."
  (condition-case nil
      (and (listp response)
           (stringp (plist-get response :id))
           (plist-member response :ok)
           (if (eq (plist-get response :ok) t)
               (plist-member response :result)
             (and (null (plist-get response :ok))
                  (plist-member response :error-symbol)
                  (plist-member response :error-data)
                  (symbolp (plist-get response :error-symbol))
                  (get (plist-get response :error-symbol) 'error-conditions)
                  (listp (plist-get response :error-data)))))
    (error nil)))

(defun e-runtime-store--signal-unavailable (store)
  "Signal STORE's unavailable state while preserving its first cause."
  (let ((cause (e-runtime-store--unavailable-cause store)))
    (signal
     'e-runtime-store-unavailable
     (if cause
         (list (format "Store is unavailable after %s"
                       (error-message-string cause))
               :cause cause)
       (list "Store is unavailable; close, reopen, and reload canonical state")))))

(defun e-runtime-store--signal-response-error (response)
  "Signal the typed error contained by RESPONSE."
  (let ((symbol (plist-get response :error-symbol))
        (data (plist-get response :error-data)))
    (unless (and (symbolp symbol) (get symbol 'error-conditions))
      (setq symbol 'e-runtime-store-error
            data (list "Unknown worker error" response)))
    (signal symbol data)))

(defun e-runtime-store--settle (store request response)
  "Settle REQUEST on STORE from decoded RESPONSE."
  (remhash (e-runtime-store-request--id request)
           (e-runtime-store--pending store))
  (setf (e-runtime-store--active-request store) nil)
  (if (eq (plist-get response :ok) t)
      (setf (e-runtime-store-request--state request) 'committed
            (e-runtime-store-request--result request)
            (plist-get response :result)
            (e-runtime-store--last-error store) nil)
    (let ((err (condition-case caught
                   (e-runtime-store--signal-response-error response)
                 (error caught))))
      (e-runtime-store--fail-request store request err)))
  ;; An internal open is only a transport prerequisite.  The caller which is
  ;; establishing that transport dispatches its domain request after the open
  ;; acknowledgement, so it cannot overtake the selected request.
  (unless (eq (e-runtime-store-request--kind request) 'open)
    (e-runtime-store--dispatch-next store)))

(defun e-runtime-store--consume-output (store text)
  "Consume worker protocol TEXT for STORE."
  ;; A filter invocation may already be queued when close or a protocol
  ;; failure takes ownership of the process.  No later response may settle.
  (unless (or (e-runtime-store--closed store)
              (e-runtime-store--unavailable store))
    ;; Store each remainder before decoding or settling.  A large flushed frame
    ;; can make process filters reentrant; leaving the prior fragment installed
    ;; until the outer invocation returned duplicated prefixes and corrupted an
    ;; otherwise complete frame.
    (setf (e-runtime-store--input-fragment store)
          (concat (or (e-runtime-store--input-fragment store) "") text))
    (while (and (not (e-runtime-store--unavailable store))
                (string-match "\n" (e-runtime-store--input-fragment store)))
      (let* ((input (e-runtime-store--input-fragment store))
             (line (substring input 0 (match-beginning 0))))
        (setf (e-runtime-store--input-fragment store)
              (substring input (match-end 0)))
        (unless (string-empty-p line)
          (condition-case err
              (let ((response (e-runtime-store--unpack line)))
                (if (not (e-runtime-store--response-valid-p response))
                    (e-runtime-store--freeze-and-stop
                     store
                     (e-runtime-store--protocol-error
                      store 'malformed-response))
                  (let* ((id (plist-get response :id))
                         (request
                          (gethash id (e-runtime-store--pending store))))
                    (cond
                     ((not request)
                      (e-runtime-store--freeze-and-stop
                       store
                       (e-runtime-store--protocol-error
                        store 'unknown-response-id :response-id id)))
                     ((not (eq request (e-runtime-store--active-request store)))
                      (e-runtime-store--freeze-and-stop
                       store
                       (e-runtime-store--protocol-error
                        store 'uncorrelated-response :response-id id)))
                     (t (e-runtime-store--settle store request response))))))
            (error
             (e-runtime-store--freeze-and-stop
              store
              (e-runtime-store--protocol-error
               store 'decode-error :cause err)))))))))

(defun e-runtime-store--fail-all (store error)
  "Fail STORE and every outstanding request with ERROR exactly once."
  (let (pending-requests)
    (maphash (lambda (_id request) (push request pending-requests))
             (e-runtime-store--pending store))
    (let ((requests
           (cl-delete-duplicates
            (delq nil
                  (append (list (e-runtime-store--starting-request store)
                                (e-runtime-store--active-request store))
                          (e-runtime-store--write-queue store)
                          (e-runtime-store--read-queue store)
                          pending-requests))
            :test #'eq)))
      (unless (e-runtime-store--unavailable-cause store)
        (setf (e-runtime-store--unavailable-cause store) error))
      (setf (e-runtime-store--active-request store) nil
            (e-runtime-store--starting-request store) nil
            (e-runtime-store--write-queue store) nil
            (e-runtime-store--read-queue store) nil
            (e-runtime-store--opened-process store) nil
            (e-runtime-store--unavailable store) t
            (e-runtime-store--last-error store) error)
      (dolist (request requests)
        (unless (memq (e-runtime-store-request--state request)
                      '(committed failed cancelled))
          (e-runtime-store--fail-request store request error))))))

(defun e-runtime-store--freeze-and-stop (store error)
  "Freeze STORE with ERROR and stop its worker without processing more output."
  (unwind-protect
      (e-runtime-store--fail-all store error)
    (when (processp (e-runtime-store--process store))
      ;; Once acknowledgement is ambiguous, no buffered response may publish a
      ;; late terminal state.  Reopening reloads the canonical database.
      (set-process-filter (e-runtime-store--process store) #'ignore)
      (set-process-sentinel (e-runtime-store--process store) #'ignore)
      (when (process-live-p (e-runtime-store--process store))
        (delete-process (e-runtime-store--process store))))))

(defun e-runtime-store--worker-exited (store)
  "Fail STORE after worker loss without retrying an ambiguous operation."
  (unless (or (e-runtime-store--closed store)
              (e-runtime-store--unavailable store))
    (let* ((request (e-runtime-store--failure-request store))
           (operation (e-runtime-store--request-operation request)))
      (e-runtime-store--fail-all
       store
       (e-runtime-store--request-error
        'e-runtime-store-unavailable
        (if operation
            (format "Worker exited before acknowledging %s; reload canonical state"
                    operation)
          "Worker exited before acknowledging the request; reload canonical state")
        request :cause 'worker-exited)))))

(defun e-runtime-store--start-process (store)
  "Start STORE's worker process and return it."
  (when (e-runtime-store--closed store)
    (signal 'e-runtime-store-unavailable (list "Store is closed")))
  (when (e-runtime-store--unavailable store)
    (e-runtime-store--signal-unavailable store))
  (unless (e-runtime-store--live-p store)
    (when (buffer-live-p (e-runtime-store--stderr-buffer store))
      (kill-buffer (e-runtime-store--stderr-buffer store)))
    (let ((stderr (generate-new-buffer " *e-runtime-store-stderr*")))
      (setf (e-runtime-store--stderr-buffer store) stderr
            (e-runtime-store--input-fragment store) ""
            (e-runtime-store--process store)
            (make-process
             :name (format "e-runtime-store-%s"
                           (substring (e-runtime-store--runtime-id store) 0
                                      (min 8 (length
                                              (e-runtime-store--runtime-id
                                               store)))))
             :buffer nil :stderr stderr :command (e-runtime-store--command)
             :connection-type 'pipe :coding 'utf-8-unix :noquery t
             :filter (lambda (_process text)
                       (e-runtime-store--consume-output store text))
             :sentinel (lambda (_process _event)
                         (unless (e-runtime-store--live-p store)
                           (e-runtime-store--worker-exited store)))))
      (set-process-query-on-exit-flag (e-runtime-store--process store) nil)))
  (e-runtime-store--process store))

(defun e-runtime-store--ensure-worker-open (store)
  "Open STORE in a newly started worker before dispatching domain work."
  (unless (eq (e-runtime-store--opened-process store)
              (e-runtime-store--process store))
    (let* ((request
            (e-runtime-store-request--create
             :id (e-runtime-store--next-id store "open") :kind 'open
             :body nil :state 'submitted :submitted-at (float-time)))
           (frame (list :id (e-runtime-store-request--id request)
                        :kind 'open :directory (e-runtime-store--directory store)
                        :runtime-id (e-runtime-store--runtime-id store))))
      (setf (e-runtime-store--active-request store) request)
      (puthash (e-runtime-store-request--id request) request
               (e-runtime-store--pending store))
      (process-send-string (e-runtime-store--process store)
                           (e-runtime-store--pack frame))
      (setf (e-runtime-store--startup-status store)
            (e-runtime-store-await store request)
            (e-runtime-store--opened-process store)
            (e-runtime-store--process store)))))

(defun e-runtime-store--next-id (store prefix)
  "Return STORE's next process-local protocol id with PREFIX."
  (format "%s:%s:%d" (e-runtime-store--runtime-id store) prefix
          (cl-incf (e-runtime-store--sequence store))))

(defun e-runtime-store--request-frame (request)
  "Return protocol frame for REQUEST."
  (list :id (e-runtime-store-request--id request)
        :kind (e-runtime-store-request--kind request)
        :body (e-runtime-store-request--body request)))

(defun e-runtime-store--fail-request (store request err)
  "Settle REQUEST on STORE with ERR exactly once."
  (remhash (e-runtime-store-request--id request)
           (e-runtime-store--pending store))
  (setf (e-runtime-store-request--state request) 'failed
        (e-runtime-store-request--error request) err
        (e-runtime-store--last-error store) err))

(defun e-runtime-store--queued-request-p (store request)
  "Return non-nil when REQUEST remains scheduler-owned in STORE's queues."
  (and (eq (e-runtime-store-request--state request) 'queued)
       (or (memq request (e-runtime-store--write-queue store))
           (memq request (e-runtime-store--read-queue store)))))

(defun e-runtime-store--remove-queued-request (store request)
  "Remove REQUEST from STORE's scheduler queues and return REQUEST."
  (setf (e-runtime-store--write-queue store)
        (delq request (e-runtime-store--write-queue store))
        (e-runtime-store--read-queue store)
        (delq request (e-runtime-store--read-queue store)))
  request)

(defun e-runtime-store--take-queued-request (store request)
  "Take still-queued REQUEST from STORE only after transport opening succeeds."
  (unless (e-runtime-store--queued-request-p store request)
    (signal 'e-runtime-store-error
            (list "Scheduler lost its queued request" request)))
  (e-runtime-store--remove-queued-request store request))

(defun e-runtime-store--next-queued-request (store)
  "Return STORE's next request without releasing scheduler ownership."
  (or (car (e-runtime-store--write-queue store))
      (car (e-runtime-store--read-queue store))))

(defun e-runtime-store--dispatch-next (store)
  "Dispatch the next bounded request, with commits preferred over reads."
  (unless (or (e-runtime-store--active-request store)
              (e-runtime-store--closed store)
              (e-runtime-store--unavailable store))
    (when-let* ((request (e-runtime-store--next-queued-request store)))
      ;; Keep REQUEST in its queue while starting and opening the worker.  The
      ;; internal open is setup, not a transfer of domain-request ownership.
      (setf (e-runtime-store--starting-request store) request)
      (condition-case err
          (progn
            (e-runtime-store--start-process store)
            (e-runtime-store--ensure-worker-open store)
            (if (e-runtime-store--queued-request-p store request)
                (progn
                  (e-runtime-store--take-queued-request store request)
                  (setf (e-runtime-store--active-request store) request
                        (e-runtime-store-request--state request) 'submitted
                        (e-runtime-store-request--submitted-at request)
                        (float-time)
                        (e-runtime-store--starting-request store) nil)
                  (puthash (e-runtime-store-request--id request) request
                           (e-runtime-store--pending store))
                  (process-send-string
                   (e-runtime-store--process store)
                   (e-runtime-store--pack
                    (e-runtime-store--request-frame request))))
              (progn
                ;; A timer may cancel the selected request while the internal
                ;; open awaits.  It is terminal already, so continue with the
                ;; next still-owned request rather than sending it late.
                (setf (e-runtime-store--starting-request store) nil)
                (e-runtime-store--dispatch-next store))))
        (error
         (unless (e-runtime-store--unavailable store)
           (let ((failure-request
                  (e-runtime-store--failure-request store request)))
             (if (eq (e-runtime-store-request--state request) 'submitted)
                 ;; A send can have reached the pipe before it signaled.  Its
                 ;; acknowledgement is ambiguous and must therefore freeze.
                 (e-runtime-store--freeze-and-stop
                  store
                  (e-runtime-store--request-error
                   'e-runtime-store-unavailable
                   "Worker transport failed after request submission; reload canonical state"
                   failure-request :cause err))
               (e-runtime-store--fail-all
                store
                (e-runtime-store--startup-error failure-request err))))))))))

(defun e-runtime-store-submit (store kind body)
  "Submit typed KIND BODY to STORE and return its request.
Request identity is process-local transport correlation only."
  (unless (memq kind '(read write))
    (signal 'wrong-type-argument (list '(member read write) kind)))
  (when (e-runtime-store--closed store)
    (signal 'e-runtime-store-unavailable (list "Store is closed")))
  (when (e-runtime-store--unavailable store)
    (e-runtime-store--signal-unavailable store))
  ;; Encoding is the producer-side validation boundary.  Reject unsupported or
  ;; cyclic values before the request can become a submitted durable effect.
  (e-runtime-store-codec-encode body)
  (let ((request
         (e-runtime-store-request--create
          :id (e-runtime-store--next-id store
                                         (if (eq kind 'write) "w" "r"))
          :kind kind :body body :state 'queued :admitted-at (float-time))))
    (if (eq kind 'write)
        (setf (e-runtime-store--write-queue store)
              (nconc (e-runtime-store--write-queue store) (list request)))
      (setf (e-runtime-store--read-queue store)
            (nconc (e-runtime-store--read-queue store) (list request))))
    (e-runtime-store--dispatch-next store)
    request))

(defun e-runtime-store-cancel (store request)
  "Cancel REQUEST before submission.
Return `dropped' for provisional work or `in-flight' once transport began."
  (pcase (e-runtime-store-request--state request)
    ('queued
     (e-runtime-store--remove-queued-request store request)
     (setf
           (e-runtime-store-request--state request) 'cancelled
           (e-runtime-store-request--error request)
           '(e-runtime-store-cancelled "Cancelled before submission"))
     'dropped)
    ('submitted 'in-flight)
    (_ (e-runtime-store-request--state request))))

(defun e-runtime-store--request-deadline (request interval)
  "Return REQUEST's current phase deadline using timeout INTERVAL."
  (let ((started-at
         (pcase (e-runtime-store-request--state request)
           ('queued (e-runtime-store-request--admitted-at request))
           ('submitted (e-runtime-store-request--submitted-at request))
           (_ nil))))
    (unless (numberp started-at)
      (signal 'e-runtime-store-error
              (list "Runtime-store request lacks its phase timestamp" request)))
    (+ started-at interval)))

(defun e-runtime-store-await (store request &optional timeout)
  "Wait cooperatively for REQUEST and return its committed result."
  (let ((interval (or timeout e-runtime-store-request-timeout)))
    (while (and (memq (e-runtime-store-request--state request)
                      '(queued submitted))
                (< (float-time)
                   (e-runtime-store--request-deadline request interval)))
      (if (e-runtime-store--live-p store)
          (accept-process-output (e-runtime-store--process store) 0.01)
        (e-runtime-store--worker-exited store)))
    (pcase (e-runtime-store-request--state request)
      ('committed (e-runtime-store-request--result request))
      ('failed (signal (car (e-runtime-store-request--error request))
                       (cdr (e-runtime-store-request--error request))))
      ('cancelled (signal 'e-runtime-store-cancelled (list request)))
      ('queued
       (let* ((active (e-runtime-store--active-request store))
              (operation (e-runtime-store--request-operation request))
              (err
               (list 'e-runtime-store-timeout
                     (format
                      "Queued %s expired before submission; the request was cancelled"
                      (or operation "request"))
                     :operation operation
                     :kind (e-runtime-store-request--kind request)
                     :request-id (e-runtime-store-request--id request)
                     :request-state 'queued
                     :blocking-operation
                     (e-runtime-store--request-operation active)
                     :blocking-kind
                     (and active (e-runtime-store-request--kind active))
                     :blocking-request-id
                     (and active (e-runtime-store-request--id active)))))
         (e-runtime-store--remove-queued-request store request)
         (e-runtime-store--fail-request store request err)
         (unless (e-runtime-store--active-request store)
           (e-runtime-store--dispatch-next store))
         (signal (car err) (cdr err))))
      (_
       (let* ((blocking (e-runtime-store--failure-request store request))
              (operation (e-runtime-store--request-operation blocking))
              (err
               (list 'e-runtime-store-timeout
                     (format
                      "Worker did not acknowledge %s before the request timeout; close, reopen, and reload canonical state"
                      (or operation "request"))
                     :operation operation
                     :kind (e-runtime-store-request--kind blocking)
                     :request-id (e-runtime-store-request--id blocking)
                     :request-state (e-runtime-store-request--state blocking)
                     :awaited-request-id (e-runtime-store-request--id request))))
         (e-runtime-store--freeze-and-stop store err)
         (signal (car err) (cdr err)))))))

(defun e-runtime-store-call (store kind body)
  "Submit KIND BODY to STORE and return its bounded result."
  (e-runtime-store-await store
                         (e-runtime-store-submit store kind body)))

(defun e-runtime-store-integrity (store &optional full)
  "Run an explicit worker-owned integrity check for STORE."
  (e-runtime-store-call store 'read
                        (list :op 'store-integrity :full (and full t))))

(defun e-runtime-store-metrics (store)
  "Return bounded worker-owned physical metrics for STORE."
  (e-runtime-store-call store 'read '(:op store-metrics)))

(defun e-runtime-store-backup (store destination)
  "Create and verify explicit SQLite backup DESTINATION through STORE's worker."
  (e-runtime-store-call store 'read
                        (list :op 'store-backup
                              :destination (expand-file-name destination))))

(defconst e-runtime-store--legacy-state-markers
  '("sessions" "index.json" "task-queue/records.eld" "cron-state.eld"
    "voice-tells.eld" "goodnite/state/daydream_access.jsonl"
    "raw-results" "session-tmp")
  "Retired state roots that require the explicit offline migrator.")

(defun e-runtime-store--reject-legacy-only-directory (directory database-file)
  "Reject legacy-only DIRECTORY before creating current DATABASE-FILE."
  (when (and (not (file-exists-p database-file))
             (seq-some
              (lambda (relative)
                (file-exists-p (expand-file-name relative directory)))
              e-runtime-store--legacy-state-markers))
    (signal
     'e-runtime-store-migration-required
     (list
      (format
       (concat "Legacy runtime state at %s has no SQLite store. Stop Emacs, "
               "copy that directory to COPIED_LEGACY_SOURCE, run "
               "scripts/e-runtime-migrate cutover COPIED_LEGACY_SOURCE %s "
               "SIBLING_BACKUP, then restart Emacs")
       directory directory)))))

(cl-defun e-runtime-store-open (directory &key runtime-id)
  "Open one subordinate runtime store for DIRECTORY."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (database-file (expand-file-name "store.sqlite3" directory))
         (store (e-runtime-store--create
                 :directory directory
                 :database-file database-file
                 :runtime-id (or runtime-id
                                 (format "%x-%x-%x" (emacs-pid)
                                         (truncate (* 1000000 (float-time)))
                                         (random most-positive-fixnum)))
                 :pending (make-hash-table :test 'equal))))
    (e-runtime-store--reject-legacy-only-directory directory database-file)
    (make-directory directory t)
    (condition-case err
        (progn
          (e-runtime-store--start-process store)
          (e-runtime-store--ensure-worker-open store)
          store)
      (error
       (setf (e-runtime-store--closed store) t)
       (when (processp (e-runtime-store--process store))
         (delete-process (e-runtime-store--process store)))
       (when (buffer-live-p (e-runtime-store--stderr-buffer store))
         (kill-buffer (e-runtime-store--stderr-buffer store)))
       (signal (car err) (cdr err))))))

(defun e-runtime-store-close (store)
  "Close STORE, failing all owned requests before detaching its worker."
  (unless (e-runtime-store--closed store)
    (let ((process (e-runtime-store--process store))
          (error '(e-runtime-store-unavailable "Store is closed")))
      ;; Close owns the lifetime boundary before later observers run.  Detach
      ;; both process handlers first so buffered output cannot overtake failure
      ;; settlement or publish a late success.
      (setf (e-runtime-store--closed store) t)
      (when (processp process)
        (set-process-filter process #'ignore)
        (set-process-sentinel process #'ignore))
      (unwind-protect
          (progn
            (e-runtime-store--fail-all store error)
            (when (and (processp process) (process-live-p process))
              (process-send-eof process)
              (accept-process-output process 0.2)))
        (when (processp process)
          (when (process-live-p process)
            (delete-process process)))
        (when (buffer-live-p (e-runtime-store--stderr-buffer store))
          (kill-buffer (e-runtime-store--stderr-buffer store))))))
  t)

(defun e-runtime-store-status (store)
  "Return bounded liveness and failure status for STORE."
  (let ((active (e-runtime-store--active-request store)))
    (list :database-file (e-runtime-store--database-file store)
          :runtime-id (e-runtime-store--runtime-id store)
          :worker-live (and (e-runtime-store--live-p store) t)
          :worker-pid (and (e-runtime-store--live-p store)
                           (process-id (e-runtime-store--process store)))
          :pending-count (+ (length (e-runtime-store--write-queue store))
                            (length (e-runtime-store--read-queue store))
                            (if active 1 0))
          :oldest-age (and active (e-runtime-store-request--submitted-at active)
                           (- (float-time)
                              (e-runtime-store-request--submitted-at active)))
          :active-request-kind
          (and active (e-runtime-store-request--kind active))
          :active-request-operation
          (e-runtime-store--request-operation active)
          :unavailable (and (e-runtime-store--unavailable store) t)
          :unavailable-cause (e-runtime-store--unavailable-cause store)
          :last-error (e-runtime-store--last-error store)
          :startup (e-runtime-store--startup-status store))))

(provide 'e-runtime-store)

;;; e-runtime-store.el ends here
