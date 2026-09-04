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
(define-error 'e-runtime-store-request-too-large
  "Runtime store request exceeds its transport limit"
  'e-runtime-store-error)
(define-error 'e-runtime-store-response-too-large
  "Runtime store read response exceeds its transport limit"
  'e-runtime-store-error)
(define-error 'e-runtime-store-projection-too-large
  "Runtime store rebuildable projection exceeds its byte limit"
  'e-runtime-store-error)
(require 'e-runtime-store-ownership)
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
submitted timeout fences and replaces the worker once."
  :type 'number :group 'e)

(defvar e-runtime-store--parent-boot-id
  (e-runtime-store-ownership--host-boot-id)
  "Private host-boot identity shared by all scheduler processes on this boot.")

(defun e-runtime-store--parent-identity ()
  "Return the bounded scheduler identity persisted by the SQLite worker."
  (list :boot e-runtime-store--parent-boot-id
        :pid (emacs-pid)
        :process-start (e-runtime-store-ownership--current-process-start)))

(cl-defstruct (e-runtime-store
               (:constructor e-runtime-store--create)
               (:predicate e-runtime-store-p)
               (:conc-name e-runtime-store--))
  directory database-file runtime-id process opened-process stderr-buffer input-fragment
  (sequence 0) pending write-queue read-queue active-request
  starting-request last-error unavailable-cause startup-status unavailable closed
  recovering-request recovery-cause (recovery-attempt 0) (acknowledged-write-prefix 0)
  (write-prefix-sequence 0))

(cl-defstruct (e-runtime-store-request
               (:constructor e-runtime-store-request--create)
               (:predicate e-runtime-store-request-p)
               (:conc-name e-runtime-store-request--))
  id kind body frame state result error admitted-at submitted-at write-prefix)

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

(defun e-runtime-store--encode-frame (value)
  "Return bounded canonical protocol bytes for VALUE.

Canonical and wire domains are checked independently even though the wire
limit is mechanically derived from the canonical limit."
  (let ((canonical
         (e-runtime-store-codec-encode-bounded
          value e-runtime-store-codec-protocol-canonical-byte-limit)))
    (when (> (e-runtime-store-codec-wire-byte-count (string-bytes canonical))
             e-runtime-store-codec-protocol-wire-byte-limit)
      (signal 'e-runtime-store-codec-too-large
              (list "Protocol wire frame exceeds byte limit"
                    :domain 'wire
                    :limit e-runtime-store-codec-protocol-wire-byte-limit
                    :canonical-bytes (string-bytes canonical))))
    canonical))

(defun e-runtime-store--pack-canonical (canonical)
  "Return bounded canonical protocol CANONICAL as one ASCII frame."
  (let ((wire-bytes (e-runtime-store-codec-wire-byte-count
                     (string-bytes canonical))))
    (when (> wire-bytes e-runtime-store-codec-protocol-wire-byte-limit)
      (signal 'e-runtime-store-codec-too-large
              (list "Protocol wire frame exceeds byte limit"
                    :domain 'wire
                    :limit e-runtime-store-codec-protocol-wire-byte-limit
                    :canonical-bytes (string-bytes canonical))))
    (concat (base64-encode-string canonical t) "\n")))

(defun e-runtime-store--pack (value)
  "Return VALUE as one bounded ASCII protocol frame."
  (e-runtime-store--pack-canonical (e-runtime-store--encode-frame value)))

(defun e-runtime-store--unpack (text)
  "Decode one ASCII protocol frame TEXT."
  (when (> (1+ (string-bytes text))
           e-runtime-store-codec-protocol-wire-byte-limit)
    (signal 'e-runtime-store-codec-too-large
            (list "Protocol wire frame exceeds byte limit"
                  :domain 'wire
                  :limit e-runtime-store-codec-protocol-wire-byte-limit
                  :wire-bytes (1+ (string-bytes text)))))
  (let ((canonical (base64-decode-string text)))
    ;; The largest legal base64 character count can otherwise represent one
    ;; extra unpadded raw byte when the canonical ceiling is not divisible by
    ;; three.  Keep both named byte domains independently authoritative.
    (when (> (string-bytes canonical)
             e-runtime-store-codec-protocol-canonical-byte-limit)
      (signal 'e-runtime-store-codec-too-large
              (list "Protocol canonical frame exceeds byte limit"
                    :domain 'canonical
                    :limit e-runtime-store-codec-protocol-canonical-byte-limit
                    :canonical-bytes (string-bytes canonical))))
    (e-runtime-store-codec-decode canonical)))

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
            (e-runtime-store-request--frame request) nil
            (e-runtime-store--last-error store) nil)
    (let ((err (condition-case caught
                   (e-runtime-store--signal-response-error response)
                 (error caught))))
      (e-runtime-store--fail-request store request err)))
  (when (and (eq (e-runtime-store-request--kind request) 'write)
             (eq (e-runtime-store-request--state request) 'committed))
    (setf (e-runtime-store--acknowledged-write-prefix store)
          (max (e-runtime-store--acknowledged-write-prefix store)
               (or (e-runtime-store-request--write-prefix request) 0))))
  (when (and (not (eq (e-runtime-store-request--kind request) 'open))
             (memq (e-runtime-store-request--state request) '(committed failed)))
    (setf (e-runtime-store--recovery-attempt store) 0
          (e-runtime-store--recovery-cause store) nil))
  ;; An internal open is only a transport prerequisite.  The caller which is
  ;; establishing that transport dispatches its domain request after the open
  ;; acknowledgement, so it cannot overtake the selected request.
  (cond
   ((and (eq (e-runtime-store-request--kind request) 'open)
         (e-runtime-store--recovering-request store))
    ;; Recovery opening temporarily owns the active slot.  Restore the exact
    ;; retained request before any queue work can be considered.
    (setf (e-runtime-store--active-request store)
          (e-runtime-store--recovering-request store)))
   ((not (eq (e-runtime-store-request--kind request) 'open))
    (e-runtime-store--dispatch-next store))))

(defun e-runtime-store--freeze-oversized-response (store wire-bytes)
  "Recover submitted work after an oversized response frame of WIRE-BYTES."
  (e-runtime-store--recover-or-fail
   store
   (e-runtime-store--protocol-error
    store 'response-frame-too-large
    :wire-bytes wire-bytes
    :wire-limit e-runtime-store-codec-protocol-wire-byte-limit)))

(defun e-runtime-store--consume-response-line (store line)
  "Decode and settle one complete protocol LINE for STORE."
  ;; The private protocol has no keepalive frame.  A delimiter always closes
  ;; one response, so a bare newline is corruption rather than ignorable idle
  ;; output and must preserve the active request as its first failure cause.
  (if (string-empty-p line)
      (e-runtime-store--recover-or-fail
       store (e-runtime-store--protocol-error store 'empty-response))
    (let ((wire-bytes (1+ (string-bytes line))))
      (if (> wire-bytes e-runtime-store-codec-protocol-wire-byte-limit)
          (e-runtime-store--freeze-oversized-response store wire-bytes)
        (condition-case err
            (let ((response (e-runtime-store--unpack line)))
              (if (not (e-runtime-store--response-valid-p response))
                  (e-runtime-store--recover-or-fail
                   store
                   (e-runtime-store--protocol-error
                    store 'malformed-response))
                (let* ((id (plist-get response :id))
                       (request
                        (gethash id (e-runtime-store--pending store))))
                  (cond
                   ((not request)
                    (e-runtime-store--recover-or-fail
                     store
                     (e-runtime-store--protocol-error
                      store 'unknown-response-id :response-id id)))
                   ((not (eq request (e-runtime-store--active-request store)))
                     (e-runtime-store--recover-or-fail
                     store
                     (e-runtime-store--protocol-error
                      store 'uncorrelated-response :response-id id)))
                   (t (e-runtime-store--settle store request response))))))
          (e-runtime-store-codec-too-large
           (e-runtime-store--freeze-oversized-response store wire-bytes))
          (error
           (e-runtime-store--recover-or-fail
            store
            (e-runtime-store--protocol-error
             store 'decode-error :cause err))))))))

(defun e-runtime-store--consume-output (store text)
  "Consume bounded, newline-framed worker protocol TEXT for STORE."
  ;; A filter invocation may already be queued when close or a protocol
  ;; failure takes ownership of the process.  No later response may settle.
  (unless (or (e-runtime-store--closed store)
              (e-runtime-store--unavailable store))
    ;; Publish the complete unconsumed remainder before decoding each line.
    ;; Settling may synchronously dispatch the next request and reenter this
    ;; filter; that nested invocation must append to the durable remainder,
    ;; never to a stale lexical prefix from the outer invocation.
    (setf (e-runtime-store--input-fragment store)
          (concat (or (e-runtime-store--input-fragment store) "") text))
    (while (and (not (e-runtime-store--unavailable store))
                (let ((input (e-runtime-store--input-fragment store)))
                  (string-match "\n" input)))
      (let* ((input (e-runtime-store--input-fragment store))
             ;; Capture both offsets before decoding: nested printer, codec, or
             ;; scheduler work is free to use regular expressions itself.
             (line-end (match-beginning 0))
             (remainder-start (match-end 0))
             (line (substring input 0 line-end))
             (remainder (substring input remainder-start)))
        (setf (e-runtime-store--input-fragment store) remainder)
        (let ((wire-bytes (1+ (string-bytes line))))
          (if (> wire-bytes e-runtime-store-codec-protocol-wire-byte-limit)
              (e-runtime-store--freeze-oversized-response store wire-bytes)
            (e-runtime-store--consume-response-line store line)))))
    (unless (e-runtime-store--unavailable store)
      (let ((fragment (or (e-runtime-store--input-fragment store) "")))
        ;; A frame with no delimiter cannot reach the complete limit: the
        ;; newline itself still needs one byte.  Fail before retaining an
        ;; impossible-to-complete fragment.
        (when (>= (string-bytes fragment)
                  e-runtime-store-codec-protocol-wire-byte-limit)
          (e-runtime-store--freeze-oversized-response
           store (string-bytes fragment)))))))

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

(defun e-runtime-store--fence-worker (store)
  "Detach STORE from its current worker before a bounded replacement.
Old filter and sentinel callbacks must not settle a replayed request."
  (when-let* ((process (e-runtime-store--process store)))
    (when (processp process)
      (set-process-filter process #'ignore)
      (set-process-sentinel process #'ignore)
      (when (process-live-p process)
        (delete-process process)))))

(defun e-runtime-store--recovery-exhausted (store cause)
  "Fail STORE's owned work once, preserving the original recovery CAUSE."
  (let ((first (or (e-runtime-store--recovery-cause store) cause)))
    (setf (e-runtime-store--recovering-request store) nil)
    (e-runtime-store--freeze-and-stop store first)))

(defun e-runtime-store--recover-active (store cause)
  "Replace STORE's worker once and replay its exact retained active frame.
Writes replay their original request identity, letting the SQLite receipt decide
whether the mutation already committed.  Reads are safe to retry directly."
  (let ((request (e-runtime-store--active-request store)))
    (cond
     ;; The internal open temporarily becomes active while the original domain
     ;; request remains owned by RECOVERING-REQUEST.  A second failure there
     ;; spends the one attempt and settles that original request and its queue.
     ((e-runtime-store--recovering-request store)
      (e-runtime-store--recovery-exhausted store cause))
     ((not request)
      (e-runtime-store--recovery-exhausted store cause))
     (t
      ;; A replacement may have completed but its replay can still corrupt or
      ;; stall.  Retain the first typed transport cause through that second
      ;; incident; exhaustion reports the initiating failure, not whichever
      ;; later symptom happened to win the race.
      (unless (e-runtime-store--recovery-cause store)
        (setf (e-runtime-store--recovery-cause store) cause))
      (setf (e-runtime-store--recovering-request store) request)
      (cl-incf (e-runtime-store--recovery-attempt store))
      (if (> (e-runtime-store--recovery-attempt store) 1)
          (e-runtime-store--recovery-exhausted store cause)
        (condition-case replacement-error
            (progn
              (e-runtime-store--fence-worker store)
              (setf (e-runtime-store--opened-process store) nil
                    (e-runtime-store--input-fragment store) "")
              (e-runtime-store--start-process store)
              (e-runtime-store--ensure-worker-open store)
              (unless (and (eq request (e-runtime-store--active-request store))
                           (stringp (e-runtime-store-request--frame request)))
                (signal 'e-runtime-store-error
                        (list "Recovery lost its immutable request frame")))
              (process-send-string
               (e-runtime-store--process store)
                (e-runtime-store--pack-canonical
                (e-runtime-store-request--frame request)))
              (setf (e-runtime-store-request--submitted-at request) (float-time)
                    (e-runtime-store--recovering-request store) nil))
          (error
           (e-runtime-store--recovery-exhausted
            store (or (e-runtime-store--recovery-cause store)
                      replacement-error)))))))))

(defun e-runtime-store--recover-or-fail (store error)
  "Recover submitted work for ERROR, or fail safely when no request is owned."
  (cond
   ((e-runtime-store--recovering-request store)
    (e-runtime-store--recovery-exhausted store error))
   ((and (e-runtime-store--active-request store)
         (eq (e-runtime-store-request--state
              (e-runtime-store--active-request store)) 'submitted))
    (e-runtime-store--recover-active store error))
   (t (e-runtime-store--freeze-and-stop store error))))

(defun e-runtime-store--worker-exited (store)
  "Recover one submitted request after worker loss when possible."
  (unless (or (e-runtime-store--closed store)
              (e-runtime-store--unavailable store))
    (let* ((request (e-runtime-store--failure-request store))
           (operation (e-runtime-store--request-operation request)))
      (e-runtime-store--recover-or-fail
       store
       (e-runtime-store--request-error
        'e-runtime-store-timeout
        (if operation
            (format "Worker exited before acknowledging %s; reload canonical state"
                    operation)
          "Worker exited before acknowledging the request")
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
                        :runtime-id (e-runtime-store--runtime-id store)
                        :parent-identity (e-runtime-store--parent-identity))))
      (setf (e-runtime-store-request--frame request)
            (e-runtime-store--encode-frame frame))
      (setf (e-runtime-store--active-request store) request)
      (puthash (e-runtime-store-request--id request) request
               (e-runtime-store--pending store))
      (process-send-string (e-runtime-store--process store)
                           (e-runtime-store--pack-canonical
                            (e-runtime-store-request--frame request)))
      ;; The subprocess accepted the complete frame.  The pending request
      ;; needs only its small correlation/terminal facts from this point.
      (setf (e-runtime-store-request--frame request) nil)
      (setf (e-runtime-store--startup-status store)
            (e-runtime-store-await store request)
            (e-runtime-store--opened-process store)
            (e-runtime-store--process store)))))

(defun e-runtime-store--next-id (store prefix)
  "Return STORE's next process-local protocol id with PREFIX."
  (format "%s:%s:%d" (e-runtime-store--runtime-id store) prefix
          (cl-incf (e-runtime-store--sequence store))))

(defun e-runtime-store--request-frame (store-or-request &optional maybe-request)
  "Return protocol frame for REQUEST."
  (let ((store (and maybe-request store-or-request))
        (request (or maybe-request store-or-request)))
    (append
     (list :id (e-runtime-store-request--id request)
           :kind (e-runtime-store-request--kind request)
           :body (e-runtime-store-request--body request))
     (when store
       (list :write-prefix (e-runtime-store-request--write-prefix request)
             :ack-prefix (and (eq (e-runtime-store-request--kind request) 'write)
                              (e-runtime-store--acknowledged-write-prefix store)))))))

(defun e-runtime-store--preflight-request (store-or-request &optional maybe-request)
  "Return REQUEST's exact bounded canonical frame before queue admission."
  (let ((store (and maybe-request store-or-request))
        (request (or maybe-request store-or-request)))
    (or (e-runtime-store-request--frame request)
        (condition-case err
            (setf (e-runtime-store-request--frame request)
                  (e-runtime-store--encode-frame
                   (e-runtime-store--request-frame store request)))
        (e-runtime-store-codec-too-large
         (let ((failure
                (e-runtime-store--request-error
                 'e-runtime-store-request-too-large
                 "Runtime-store request exceeds its transport limit"
                 request :cause err)))
           (signal (car failure) (cdr failure))))))))

(defun e-runtime-store--fail-request (store request err)
  "Settle REQUEST on STORE with ERR exactly once."
  (remhash (e-runtime-store-request--id request)
           (e-runtime-store--pending store))
  (setf (e-runtime-store-request--state request) 'failed
        (e-runtime-store-request--error request) err
        (e-runtime-store-request--frame request) nil
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
                   (e-runtime-store--pack-canonical
                    (e-runtime-store--preflight-request store request))))
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
                 (e-runtime-store--recover-or-fail
                  store
                  (e-runtime-store--request-error
                   'e-runtime-store-timeout
                   "Worker transport failed after request submission"
                   failure-request :cause err))
               (e-runtime-store--fail-all
                store
                (e-runtime-store--startup-error failure-request err))))))))))

(defun e-runtime-store-submit (store kind body)
  "Submit typed KIND BODY to STORE and return its request.
Write identity is also the private durable receipt key used for recovery."
  (unless (memq kind '(read write))
    (signal 'wrong-type-argument (list '(member read write) kind)))
  (when (e-runtime-store--closed store)
    (signal 'e-runtime-store-unavailable (list "Store is closed")))
  (when (e-runtime-store--unavailable store)
    (e-runtime-store--signal-unavailable store))
  (let ((request
         (e-runtime-store-request--create
          :id (e-runtime-store--next-id store
                                         (if (eq kind 'write) "w" "r"))
          :kind kind :body body :state 'queued :admitted-at (float-time)
          :write-prefix (and (eq kind 'write)
                             (cl-incf (e-runtime-store--write-prefix-sequence store))))))
    ;; Capture and bound the complete transport frame, including its generated
    ;; correlation id and request envelope, before this request enters either
    ;; scheduler queue.  Later caller mutation cannot change sent bytes.
    (e-runtime-store--preflight-request store request)
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
           '(e-runtime-store-cancelled "Cancelled before submission")
           (e-runtime-store-request--frame request) nil)
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

(defun e-runtime-store--recover-overdue-active (store interval)
  "Start STORE's single recovery when submitted work expires before a waiter.

Queued waiters remain scheduler-owned while a submitted predecessor is the
only ambiguous operation.  They therefore observe that predecessor's shorter
execution deadline rather than waiting for their own later admission timeout."
  (when-let* ((active (e-runtime-store--active-request store))
              ((eq (e-runtime-store-request--state active) 'submitted))
              ((>= (float-time)
                   (e-runtime-store--request-deadline active interval))))
    (let ((operation (e-runtime-store--request-operation active)))
      (e-runtime-store--recover-or-fail
       store
       (e-runtime-store--request-error
        'e-runtime-store-timeout
        (format "Worker did not acknowledge %s before the request timeout; close, reopen, and reload canonical state"
                (or operation "request"))
        active :awaited-request-id nil)))))

(defun e-runtime-store-await (store request &optional timeout)
  "Wait cooperatively for REQUEST and return its committed result."
  (let ((interval (or timeout e-runtime-store-request-timeout)))
    (while (and (memq (e-runtime-store-request--state request)
                      '(queued submitted))
                (< (float-time)
                   (e-runtime-store--request-deadline request interval)))
      (e-runtime-store--recover-overdue-active store interval)
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
         (e-runtime-store--recover-active store err)
         ;; A replacement resets the submitted interval.  The caller observes
         ;; the definitive receipt result, not a transient transport timeout.
         (e-runtime-store-await store request interval))))))

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
  "Close STORE, retiring an idle runtime before detaching its worker.
When callers still own queued or active work, close retains its established
terminal behavior and settles them locally.  An idle close is a worker-owned,
idempotent retirement so a lost acknowledgement is recoverable once."
  (unless (e-runtime-store--closed store)
    (let ((process (e-runtime-store--process store))
          (error '(e-runtime-store-unavailable "Store is closed"))
          retirement-error)
      (when (and (e-runtime-store--live-p store)
                 (not (e-runtime-store--unavailable store))
                 (not (e-runtime-store--active-request store))
                 (null (e-runtime-store--write-queue store))
                 (null (e-runtime-store--read-queue store)))
        (let* ((request (e-runtime-store-request--create
                         :id (e-runtime-store--next-id store "close")
                         :kind 'close :body nil :state 'submitted
                         :submitted-at (float-time)))
               (frame (e-runtime-store--encode-frame
                       (e-runtime-store--request-frame store request))))
          (setf (e-runtime-store-request--frame request) frame
                (e-runtime-store--active-request store) request)
          (puthash (e-runtime-store-request--id request) request
                   (e-runtime-store--pending store))
          (process-send-string process (e-runtime-store--pack-canonical frame))
          (condition-case caught
              (e-runtime-store-await store request)
            (error (setq retirement-error caught)))))
      (setq process (e-runtime-store--process store))
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
          (kill-buffer (e-runtime-store--stderr-buffer store)))
        ;; The state was made terminal and the process detached above, but a
        ;; failed retirement is observable to the caller rather than hidden.
        (when retirement-error
          (signal (car retirement-error) (cdr retirement-error))))))
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
