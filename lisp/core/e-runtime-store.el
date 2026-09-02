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

(defcustom e-runtime-store-request-timeout 60.0
  "Maximum seconds for one bounded runtime-store request.
A submitted timeout freezes the store until explicit close/reopen and reload."
  :type 'number :group 'e)

(cl-defstruct (e-runtime-store
               (:constructor e-runtime-store--create)
               (:predicate e-runtime-store-p)
               (:conc-name e-runtime-store--))
  directory database-file runtime-id process opened-process stderr-buffer input-fragment
  (sequence 0) pending write-queue read-queue active-request
  last-error startup-status unavailable closed)

(cl-defstruct (e-runtime-store-request
               (:constructor e-runtime-store-request--create)
               (:predicate e-runtime-store-request-p)
               (:conc-name e-runtime-store-request--))
  id kind body state result error on-done on-error submitted-at)

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
  (let (callback-error)
    (if (plist-get response :ok)
        (progn
          (setf (e-runtime-store-request--state request) 'committed
                (e-runtime-store-request--result request)
                (plist-get response :result)
                (e-runtime-store--last-error store) nil)
          (when (e-runtime-store-request--on-done request)
            (condition-case err
                (funcall (e-runtime-store-request--on-done request)
                         (e-runtime-store-request--result request))
              (error (setq callback-error err)))))
      (let ((err (condition-case caught
                     (e-runtime-store--signal-response-error response)
                   (error caught))))
        (setf (e-runtime-store-request--state request) 'failed
              (e-runtime-store-request--error request) err
              (e-runtime-store--last-error store) err)
        (when (e-runtime-store-request--on-error request)
          (condition-case caught
              (funcall (e-runtime-store-request--on-error request) err)
            (error (setq callback-error caught))))))
    (if callback-error
        (e-runtime-store--freeze-and-stop
         store
         (list 'e-runtime-store-error
               "Runtime-store request callback signaled"
               (truncate-string-to-width
                (error-message-string callback-error) 512 nil nil t)))
      ;; An internal open is only a transport prerequisite.  The caller which
      ;; is establishing that transport dispatches its domain request after
      ;; the open acknowledgement, so do not let a nested callback overtake it.
      (unless (eq (e-runtime-store-request--kind request) 'open)
        (e-runtime-store--dispatch-next store)))))

(defun e-runtime-store--consume-output (store text)
  "Consume worker protocol TEXT for STORE."
  ;; A filter invocation may already be queued when close takes ownership of
  ;; the process.  Once closed, no response may settle or publish a callback.
  (unless (e-runtime-store--closed store)
    ;; Store each remainder before decoding or settling.  A large flushed frame
    ;; can make process filters reentrant; leaving the prior fragment installed
    ;; until the outer invocation returned duplicated prefixes and corrupted an
    ;; otherwise complete frame.
    (setf (e-runtime-store--input-fragment store)
          (concat (or (e-runtime-store--input-fragment store) "") text))
    (while (string-match "\n" (e-runtime-store--input-fragment store))
      (let* ((input (e-runtime-store--input-fragment store))
             (line (substring input 0 (match-beginning 0))))
        (setf (e-runtime-store--input-fragment store)
              (substring input (match-end 0)))
        (unless (string-empty-p line)
          (condition-case err
              (let* ((response (e-runtime-store--unpack line))
                     (id (plist-get response :id))
                     (request (gethash id (e-runtime-store--pending store))))
                (if request
                    (e-runtime-store--settle store request response)
                  (setf (e-runtime-store--last-error store)
                        (list 'e-runtime-store-error
                              "Response has no pending request" id))))
            (error
             (setf (e-runtime-store--last-error store) err)
             ;; A malformed response leaves commit acknowledgement ambiguous.
             ;; Fail this runtime; callers must reopen and reload canonical DB
             ;; state rather than resubmit the operation.
             (when (e-runtime-store--live-p store)
               (delete-process (e-runtime-store--process store))))))))))

(defun e-runtime-store--fail-all (store error)
  "Fail STORE and every outstanding request with ERROR exactly once."
  (let (pending-requests callback-errors)
    (maphash (lambda (_id request) (push request pending-requests))
             (e-runtime-store--pending store))
    (let ((requests
           (cl-delete-duplicates
            (delq nil
                  (append (list (e-runtime-store--active-request store))
                          (e-runtime-store--write-queue store)
                          (e-runtime-store--read-queue store)
                          pending-requests))
            :test #'eq)))
      (setf (e-runtime-store--active-request store) nil
            (e-runtime-store--write-queue store) nil
            (e-runtime-store--read-queue store) nil
            (e-runtime-store--opened-process store) nil
            (e-runtime-store--unavailable store) t
            (e-runtime-store--last-error store) error)
      (dolist (request requests)
        (unless (memq (e-runtime-store-request--state request)
                      '(committed failed cancelled))
          (when-let* ((callback-error
                       (e-runtime-store--fail-request store request error)))
            (push callback-error callback-errors)))))
    (when callback-errors
      (setf
       (e-runtime-store--last-error store)
       (list 'e-runtime-store-error
             "Failure callback signaled while the store became unavailable"
             :storage-error
             (truncate-string-to-width
              (error-message-string error) 512 nil nil t)
             :callback-errors
             (mapcar (lambda (callback-error)
                       (truncate-string-to-width
                        (error-message-string callback-error) 512 nil nil t))
                     (nreverse callback-errors)))))))

(defun e-runtime-store--freeze-and-stop (store error)
  "Freeze STORE with ERROR and stop its worker without processing more output."
  (unwind-protect
      (e-runtime-store--fail-all store error)
    (when (processp (e-runtime-store--process store))
      ;; Once acknowledgement is ambiguous, no buffered response may publish a
      ;; late success callback.  Reopening reloads the canonical database.
      (set-process-filter (e-runtime-store--process store) #'ignore)
      (set-process-sentinel (e-runtime-store--process store) #'ignore)
      (when (process-live-p (e-runtime-store--process store))
        (delete-process (e-runtime-store--process store))))))

(defun e-runtime-store--worker-exited (store)
  "Fail STORE after worker loss without retrying an ambiguous operation."
  (unless (e-runtime-store--closed store)
    (e-runtime-store--fail-all
     store '(e-runtime-store-unavailable
             "Worker exited before acknowledging the request; reload canonical state"))))

(defun e-runtime-store--start-process (store)
  "Start STORE's worker process and return it."
  (when (e-runtime-store--closed store)
    (signal 'e-runtime-store-unavailable (list "Store is closed")))
  (when (e-runtime-store--unavailable store)
    (signal 'e-runtime-store-unavailable
            (list "Store is unavailable; close, reopen, and reload canonical state")))
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
             :body nil :state 'submitted))
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
        (e-runtime-store--last-error store) err)
  (when (e-runtime-store-request--on-error request)
    (condition-case callback-error
        (progn
          (funcall (e-runtime-store-request--on-error request) err)
          nil)
      (error callback-error))))

(defun e-runtime-store--dispatch-next (store)
  "Dispatch the next bounded request, with commits preferred over reads."
  (unless (or (e-runtime-store--active-request store)
              (e-runtime-store--closed store))
    (when-let* ((request (or (pop (e-runtime-store--write-queue store))
                             (pop (e-runtime-store--read-queue store)))))
      (condition-case err
          (progn
            (e-runtime-store--start-process store)
            (e-runtime-store--ensure-worker-open store)
            (setf (e-runtime-store--active-request store) request
                  (e-runtime-store-request--state request) 'submitted
                  (e-runtime-store-request--submitted-at request) (float-time))
            (puthash (e-runtime-store-request--id request) request
                     (e-runtime-store--pending store))
            (process-send-string
             (e-runtime-store--process store)
             (e-runtime-store--pack (e-runtime-store--request-frame request))))
        (error
         (e-runtime-store--fail-all store err))))))

(cl-defun e-runtime-store-submit
    (store kind body &key on-done on-error)
  "Submit typed KIND BODY to STORE and return its request.
Request identity is process-local transport correlation only."
  (unless (memq kind '(read write))
    (signal 'wrong-type-argument (list '(member read write) kind)))
  (when (e-runtime-store--closed store)
    (signal 'e-runtime-store-unavailable (list "Store is closed")))
  (when (e-runtime-store--unavailable store)
    (signal 'e-runtime-store-unavailable
            (list "Store is unavailable; close, reopen, and reload canonical state")))
  ;; Encoding is the producer-side validation boundary.  Reject unsupported or
  ;; cyclic values before the request can become a submitted durable effect.
  (let ((request
          (e-runtime-store-request--create
           :id (e-runtime-store--next-id store
                                        (if (eq kind 'write) "w" "r"))
           :kind kind :body body
           :state 'queued :on-done on-done :on-error on-error)))
    (e-runtime-store-codec-encode body)
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
     (setf (e-runtime-store--write-queue store)
           (delq request (e-runtime-store--write-queue store))
           (e-runtime-store--read-queue store)
           (delq request (e-runtime-store--read-queue store))
           (e-runtime-store-request--state request) 'cancelled
           (e-runtime-store-request--error request)
           '(e-runtime-store-cancelled "Cancelled before submission"))
     'dropped)
    ('submitted 'in-flight)
    (_ (e-runtime-store-request--state request))))

(defun e-runtime-store-await (store request &optional timeout)
  "Wait cooperatively for REQUEST and return its committed result."
  (let ((deadline (+ (float-time) (or timeout e-runtime-store-request-timeout))))
    (while (and (memq (e-runtime-store-request--state request)
                      '(queued submitted))
                (< (float-time) deadline))
      (if (e-runtime-store--live-p store)
          (accept-process-output (e-runtime-store--process store) 0.01)
        (e-runtime-store--worker-exited store)))
    (pcase (e-runtime-store-request--state request)
      ('committed (e-runtime-store-request--result request))
      ('failed (signal (car (e-runtime-store-request--error request))
                       (cdr (e-runtime-store-request--error request))))
      ('cancelled (signal 'e-runtime-store-cancelled (list request)))
      (_
       (let ((err
              (list 'e-runtime-store-timeout
                    "Worker did not acknowledge before the request timeout; close, reopen, and reload canonical state"
                    (e-runtime-store-request--kind request))))
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
    (signal 'e-runtime-store-schema-too-old
            (list :actual 'legacy-only
                  :required 4
                  :operation 'e-runtime-migration-run))))

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
      ;; Close owns the lifetime boundary before callbacks run.  Detach both
      ;; process callbacks first so buffered output cannot overtake failure
      ;; fanout or publish a late success.
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
          :unavailable (and (e-runtime-store--unavailable store) t)
          :last-error (e-runtime-store--last-error store)
          :startup (e-runtime-store--startup-status store))))

(provide 'e-runtime-store)

;;; e-runtime-store.el ends here
