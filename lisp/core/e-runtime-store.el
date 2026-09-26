;;; e-runtime-store.el --- Typed subordinate runtime-store adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns a lifecycle writer and its subordinate read-only batch-Emacs transport,
;; typed request framing, bounded scheduling, failure propagation, liveness,
;; and runtime ownership.  This module knows no session, Board, resource, or
;; tool policy and never opens SQLite itself.

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
(define-error 'e-runtime-store-capacity-exhausted
  "Runtime store scheduler capacity is exhausted"
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
(define-error 'e-runtime-store-persistence-suspect
  "Runtime store owner persistence is suspect" 'e-runtime-store-error)

(defconst e-runtime-store-owner-diagnostic-byte-limit 1024
  "Maximum retained UTF-8 bytes in one owner-local failure diagnostic.")

(defconst e-runtime-store-startup-diagnostic-byte-limit 4096
  "Maximum retained UTF-8 bytes from one worker startup diagnostic.")

(defconst e-runtime-store-owner-domains '(session board task)
  "Domains permitted to attach private optimistic-mutation owner keys.")

(defcustom e-runtime-store-request-timeout 60.0
  "Maximum seconds for each bounded runtime-store request phase.
Queue admission and submitted execution each receive this interval.  A
submitted timeout fences and replaces the worker once."
  :type 'number :group 'e)

(defconst e-runtime-store--latency-sample-capacity 64
  "Maximum recent terminal operation latencies retained by one runtime.")

;; These are deliberately scheduler-local rather than application policy.  A
;; later composition owner supplies a shared reservation object when it owns
;; several stores; the default keeps independently opened stores bounded too.
(defconst e-runtime-store-request-capacity 128)
(defconst e-runtime-store-retained-byte-capacity (* 68 1024 1024))
(defconst e-runtime-store-global-byte-capacity (* 85 1024 1024))
(defconst e-runtime-store-notification-capacity 128)
(defconst e-runtime-store-notification-drain-limit 16)
(defconst e-runtime-store-open-control-capacity 1
  "Maximum fixed internal open controls owned by one runtime.

This singleton is scheduler scaffolding, not an admitted client request: it
owns no client notification token and is deliberately outside the 128 client
request/token budget so a full cold queue can still become ready.")
(defconst e-runtime-store-open-control-canonical-byte-limit 16384
  "Maximum canonical bytes for the singleton internal open frame.")
(defconst e-runtime-store-open-control-wire-byte-limit 21849
  "Maximum newline-terminated base64 bytes for an internal open frame.")

(cl-defstruct (e-runtime-store--reservation
               (:constructor e-runtime-store--reservation-create))
  (limit e-runtime-store-global-byte-capacity :read-only t)
  (used 0))

(defvar e-runtime-store--default-reservation
  (e-runtime-store--reservation-create)
  "Private fallback aggregate reservation for independently opened stores.")

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
  directory database-file runtime-id access-mode parent-store read-client
  borrowed-claim
  process opened-process stderr-buffer stderr-process stderr-bytes
  stderr-truncated input-fragment
  (sequence 0) pending client-queue active-request
  starting-request last-error unavailable-cause startup-status unavailable closed
  suspect-owners
  recovering-request recovery-cause (recovery-attempt 0) (acknowledged-write-prefix 0)
  (write-prefix-sequence 0)
  open-control-request
  closing-request close-finalizer-timer
  reservation (reserved-bytes 0) (request-count 0)
  notification-outbox (notification-count 0) notification-timer
  scheduler-timer (scheduler-generation 0)
  latency-samples)

(cl-defstruct (e-runtime-store-request
               (:constructor e-runtime-store-request--create)
               (:predicate e-runtime-store-request-p)
               (:conc-name e-runtime-store-request--))
  id kind body frame state result error admitted-at submitted-at first-submitted-at
  settled-at timeout-interval
  write-prefix owner-key
  operation frame-bytes retained-bytes notification observer observer-detached
  owner-store
  frame-escrow)

(defun e-runtime-store--worker-file ()
  "Return the newest installed worker source or byte-code path.

Source-adjacent byte code can legitimately lag a checkout.  The subordinate
process must execute the same repository revision as its client without
deleting or rewriting those unrelated artifacts.  The explicit test-only
environment override is used by isolated graphical startup fixtures before
the package itself loads; ordinary runtime callers leave it unset."
  (let ((test-override (getenv "E_RUNTIME_STORE_TEST_WORKER_FILE")))
    (if (and test-override (file-readable-p test-override))
        (expand-file-name test-override)
      (let* ((library (locate-library "e-runtime-store-worker"))
             (source (and library
                          (string-suffix-p ".elc" library)
                          (substring library 0 -1))))
        (or (and source (file-exists-p source)
                 (file-newer-than-file-p source library)
                 source)
            library
            (signal 'e-runtime-store-error (list "Worker module is missing")))))))

(defun e-runtime-store--emacs-program ()
  "Return the current Emacs executable path."
  (expand-file-name invocation-name invocation-directory))

(defun e-runtime-store--command ()
  "Return the private batch worker command."
  (let* ((worker-file (e-runtime-store--worker-file))
         ;; An isolated graphical fixture may replace only the worker source
         ;; while retaining the repository's private worker dependencies.
         ;; Keep the override directory first so its schema copy is loaded,
         ;; then expose the ordinary core directory for those dependencies.
         (core-directory
          (file-name-directory
           (file-truename
            (expand-file-name
             (or (locate-library "e-runtime-store-codec") worker-file))))))
    (list (e-runtime-store--emacs-program) "--batch" "-Q"
          "-L" (file-name-directory worker-file)
          "-L" core-directory
          "--eval" "(setq load-prefer-newer t)"
          ;; Loading the worker through a guarded eval keeps command-line load
          ;; failures on stderr and gives the parent a real exit boundary.  A
          ;; raw `-l' failure can emit a leading blank stdout line before the
          ;; error, which is indistinguishable from a corrupt protocol frame.
          "--eval"
          (format
           "(condition-case err (progn (load %S) (unless (fboundp 'e-runtime-store-worker-main) (error \"Runtime-store worker main is unavailable\"))) (error (princ (format \"Runtime-store worker startup failed: %%s\\n\" (error-message-string err)) #'external-debugging-output) (kill-emacs 1)))"
           worker-file)
          "--funcall" "e-runtime-store-worker-main")))

(defun e-runtime-store--encode-frame (value &optional measured-bytes)
  "Return bounded canonical protocol bytes for VALUE.

Canonical and wire domains are checked independently even though the wire
limit is mechanically derived from the canonical limit."
  (let ((canonical (e-runtime-store-codec-encode value)))
    (when (and measured-bytes (/= measured-bytes (string-bytes canonical)))
      (signal 'e-runtime-store-codec-error
              (list "Preflight measurement disagreed with canonical encoding"
                    :measured measured-bytes :encoded (string-bytes canonical))))
    (when (> (string-bytes canonical) e-runtime-store-codec-protocol-canonical-byte-limit)
      (signal 'e-runtime-store-codec-too-large
              (list "Protocol canonical frame exceeds byte limit"
                    :domain 'canonical :canonical-bytes (string-bytes canonical))))
    (when (> (e-runtime-store-codec-wire-byte-count (string-bytes canonical))
             e-runtime-store-codec-protocol-wire-byte-limit)
      (signal 'e-runtime-store-codec-too-large
              (list "Protocol wire frame exceeds byte limit"
                    :domain 'wire
                    :limit e-runtime-store-codec-protocol-wire-byte-limit
                    :canonical-bytes (string-bytes canonical))))
    canonical))

(defun e-runtime-store--open-control-frame (store request)
  "Return the complete bounded open frame value for STORE and REQUEST."
  (append
   (list :id (e-runtime-store-request--id request)
         :kind 'open :directory (e-runtime-store--directory store)
         :runtime-id (e-runtime-store--runtime-id store)
         :access-mode (e-runtime-store--access-mode store)
         :parent-identity (e-runtime-store--parent-identity))
   (when (e-runtime-store--borrowed-claim store)
     (list :borrowed-authorized t))))

(defun e-runtime-store--send-borrow-authorization (store)
  "Authorize STORE's selected worker through its private input pipe.

The authorization is neither retained in STORE nor exposed through argv,
environment, files, or the later open request.  Pipe ordering guarantees the
worker receives this one-use control before the open frame."
  (let* ((process (e-runtime-store--process store))
         (authorization
          (e-runtime-store-ownership--make-borrow-authorization
           (e-runtime-store--borrowed-claim store) (process-id process)))
         (frame
          (e-runtime-store--pack
           (list :kind 'borrow-authorize :authorization authorization))))
    (process-send-string process frame)))

(defun e-runtime-store--prepare-open-control (store)
  "Create and preflight STORE's one fixed-size internal open control.

This intentionally measures before constructing canonical bytes, so an
oversized identity/open envelope fails before a worker is started or sent to.
The returned request is scheduler scaffolding: it owns neither a client slot
nor a notification token."
  (or (e-runtime-store--open-control-request store)
      (let* ((now (float-time))
             (request
              (e-runtime-store-request--create
               :id (e-runtime-store--next-id store "open") :kind 'open
               :body nil :state 'submitted :submitted-at now
               :first-submitted-at now
               :owner-store store
               :timeout-interval e-runtime-store-request-timeout))
             (value (e-runtime-store--open-control-frame store request))
             (bytes (e-runtime-store-codec-measure-bounded
                     value e-runtime-store-open-control-canonical-byte-limit))
             (wire-bytes (e-runtime-store-codec-wire-byte-count bytes)))
        (when (> wire-bytes e-runtime-store-open-control-wire-byte-limit)
          (signal 'e-runtime-store-codec-too-large
                  (list "Internal open wire frame exceeds byte limit"
                        :domain 'wire :limit e-runtime-store-open-control-wire-byte-limit
                        :canonical-bytes bytes)))
        (condition-case err
            (let ((canonical (e-runtime-store-codec-encode value)))
              (unless (= bytes (string-bytes canonical))
                (signal 'e-runtime-store-codec-error
                        (list "Open preflight measurement disagreed with encoding"
                              :measured bytes :encoded (string-bytes canonical))))
              (setf (e-runtime-store-request--frame request) canonical
                    (e-runtime-store-request--frame-bytes request) bytes
                    (e-runtime-store--open-control-request store) request)
              request)
          (error
           (setf (e-runtime-store-request--frame request) nil)
           (signal (car err) (cdr err)))))))

(defun e-runtime-store--release-open-control (store request)
  "Release REQUEST's singleton-open frame and correlation ownership once."
  (when (eq (e-runtime-store--open-control-request store) request)
    (setf (e-runtime-store--open-control-request store) nil))
  (when (eq (e-runtime-store--active-request store) request)
    (setf (e-runtime-store--active-request store) nil))
  (remhash (e-runtime-store-request--id request) (e-runtime-store--pending store))
  (setf (e-runtime-store-request--frame request) nil
        (e-runtime-store-request--frame-bytes request) nil))

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
       (or (e-runtime-store-request--operation request)
           (plist-get (e-runtime-store-request--body request) :op))))

(defun e-runtime-store--latency-ms (started-at finished-at)
  "Return non-negative milliseconds from STARTED-AT to FINISHED-AT."
  (and (numberp started-at)
       (numberp finished-at)
       (* 1000.0 (max 0.0 (- finished-at started-at)))))

(defun e-runtime-store--record-request-latency (store request)
  "Retain REQUEST's bounded terminal latency sample on STORE exactly once."
  (when (and request
             (memq (e-runtime-store-request--state request)
                   '(committed failed cancelled))
             (not (e-runtime-store-request--settled-at request)))
    (let* ((settled-at (float-time))
           (admitted-at (e-runtime-store-request--admitted-at request))
           (first-submitted-at
            (or (e-runtime-store-request--first-submitted-at request)
                (e-runtime-store-request--submitted-at request)))
           (queue-finished-at (or first-submitted-at settled-at))
           (sample
            (list :operation (or (e-runtime-store--request-operation request)
                                 (e-runtime-store-request--kind request))
                  :kind (e-runtime-store-request--kind request)
                  :outcome (e-runtime-store-request--state request)
                  :queue-ms
                  (e-runtime-store--latency-ms admitted-at queue-finished-at)
                  :dispatch-to-settlement-ms
                  (e-runtime-store--latency-ms first-submitted-at settled-at)
                  :total-ms
                  (e-runtime-store--latency-ms
                   (or admitted-at first-submitted-at) settled-at)
                  :settled-at settled-at)))
      (let ((samples (cons sample (e-runtime-store--latency-samples store))))
        (when (> (length samples) e-runtime-store--latency-sample-capacity)
          (setcdr (nthcdr (1- e-runtime-store--latency-sample-capacity) samples)
                  nil))
        (setf (e-runtime-store-request--settled-at request) settled-at
              (e-runtime-store--latency-samples store) samples)))))

(defun e-runtime-store--request-live-latency (request now)
  "Return REQUEST's current bounded latency status at NOW."
  (when request
    (let* ((phase (e-runtime-store-request--state request))
           (phase-start
            (pcase phase
              ('queued (e-runtime-store-request--admitted-at request))
              ('submitted (e-runtime-store-request--submitted-at request))))
           (total-start
            (or (e-runtime-store-request--admitted-at request)
                (e-runtime-store-request--first-submitted-at request)
                (e-runtime-store-request--submitted-at request))))
      (list :operation (or (e-runtime-store--request-operation request)
                           (e-runtime-store-request--kind request))
            :kind (e-runtime-store-request--kind request)
            :phase phase
            :phase-age-ms (e-runtime-store--latency-ms phase-start now)
            :total-age-ms (e-runtime-store--latency-ms total-start now)))))

(defun e-runtime-store--latency-status (store active now)
  "Return STORE's bounded current and recent latency status at NOW."
  (let ((oldest-queued (car (e-runtime-store--client-queue store))))
    (list :unit 'milliseconds
          :active (e-runtime-store--request-live-latency active now)
          :oldest-queued
          (e-runtime-store--request-live-latency oldest-queued now)
          :recent-count (length (e-runtime-store--latency-samples store))
          ;; Samples are newest first and contain only bounded scalar facts.
          :recent (mapcar #'copy-tree
                          (e-runtime-store--latency-samples store)))))

(defun e-runtime-store--startup-request (store)
  "Return STORE's one still-owned domain request while opening.

The internal open frame is transport scaffolding, never a domain failure
  identity.  A selection that cancellation or queue expiry removed is ignored
and the current queue head is selected instead."
  (let ((selected (e-runtime-store--starting-request store)))
    (if (and selected (e-runtime-store--queued-request-p store selected))
        selected
      (or (e-runtime-store--next-queued-request store)
          (let ((closing (e-runtime-store--closing-request store)))
            (and closing
                 (eq (e-runtime-store-request--state closing) 'queued)
                 closing))))))

(defun e-runtime-store--failure-request (store &optional fallback)
  "Return the request whose identity explains STORE's current failure.

An internal open is only transport setup for the scheduler-selected request,
so its failure reports that selected request while it remains queued."
  (or (e-runtime-store--recovering-request store)
      (let ((active (e-runtime-store--active-request store)))
        (unless (or (null active)
                    (eq (e-runtime-store-request--kind active) 'open))
          active))
      (e-runtime-store--startup-request store)
      fallback))

(defun e-runtime-store--request-error (type message request &rest properties)
  "Build TYPE MESSAGE with REQUEST identity and additional PROPERTIES."
  (append (list type message
                :operation (e-runtime-store--request-operation request)
                :kind (and request (e-runtime-store-request--kind request))
                :request-id (and request (e-runtime-store-request--id request)))
          properties))

(defun e-runtime-store--request-live-p (request)
  "Return non-nil while REQUEST is still scheduler-owned."
  (memq (e-runtime-store-request--state request) '(queued submitted)))

(defun e-runtime-store--reserve-request (store request bytes)
  "Atomically reserve REQUEST, BYTES, and its eventual terminal token.

BYTES is the exact canonical frame size measured before the frame is printed.
The token is reserved at admission, so terminal settlement never drops an
already admitted notification because a burst filled the outbox meanwhile."
  (let ((reservation (or (e-runtime-store--reservation store)
                         e-runtime-store--default-reservation)))
    (when (or (>= (e-runtime-store--request-count store)
                  e-runtime-store-request-capacity)
              (> (+ (e-runtime-store--reserved-bytes store) bytes)
                 e-runtime-store-retained-byte-capacity)
              (>= (e-runtime-store--notification-count store)
                  e-runtime-store-notification-capacity)
              (> (+ (e-runtime-store--reservation-used reservation) bytes)
                 (e-runtime-store--reservation-limit reservation)))
      (signal 'e-runtime-store-capacity-exhausted
              (list "Runtime-store admission capacity is exhausted"
                    :request-count (e-runtime-store--request-count store)
                    :retained-bytes (e-runtime-store--reserved-bytes store)
                    :global-bytes (e-runtime-store--reservation-used reservation))))
    (setf (e-runtime-store--reservation store) reservation
          (e-runtime-store--request-count store) (1+ (e-runtime-store--request-count store))
          (e-runtime-store--reserved-bytes store) (+ (e-runtime-store--reserved-bytes store) bytes)
          (e-runtime-store--reservation-used reservation)
          (+ (e-runtime-store--reservation-used reservation) bytes)
          (e-runtime-store--notification-count store)
          (1+ (e-runtime-store--notification-count store))
          (e-runtime-store-request--retained-bytes request) bytes
          (e-runtime-store-request--notification request) 'reserved)))

(defun e-runtime-store--reserve-request-from-escrow (store request bytes escrow)
  "Transfer ESCROW's already-counted frame bytes into REQUEST exactly once.

The composition owner has already charged ESCROW against STORE's shared
reservation.  The runtime adds only its private request/token ownership, keeps
BYTES live for terminal release, and immediately returns the unused portion.
This is private plumbing for a consumer-shaped storage adapter; public submit
continues to own its own admission reservation."
  (let ((reservation (or (e-runtime-store--reservation store)
                         e-runtime-store--default-reservation)))
    (unless (and (integerp escrow) (>= escrow bytes) (>= bytes 0))
      (signal 'e-runtime-store-error
              (list "Invalid composition frame escrow" :escrow escrow :bytes bytes)))
    (when (or (< (e-runtime-store--reservation-used reservation) escrow)
              (> (+ (e-runtime-store--reserved-bytes store) bytes)
                 e-runtime-store-retained-byte-capacity)
              (>= (e-runtime-store--request-count store)
                  e-runtime-store-request-capacity)
              (>= (e-runtime-store--notification-count store)
                  e-runtime-store-notification-capacity))
      (signal 'e-runtime-store-capacity-exhausted
              (list "Runtime-store escrow admission capacity is exhausted"
                    :request-count (e-runtime-store--request-count store)
                    :retained-bytes (e-runtime-store--reserved-bytes store)
                    :global-bytes (e-runtime-store--reservation-used reservation))))
    ;; The owner reserved ESCROW before it could construct the canonical
    ;; frame.  It transfers the surviving actual allocation to this request;
    ;; the difference cannot remain charged after this point.
    (setf (e-runtime-store--request-count store) (1+ (e-runtime-store--request-count store))
          (e-runtime-store--reserved-bytes store) (+ (e-runtime-store--reserved-bytes store) bytes)
          (e-runtime-store--reservation-used reservation)
          (- (e-runtime-store--reservation-used reservation) (- escrow bytes))
          (e-runtime-store--notification-count store)
          (1+ (e-runtime-store--notification-count store))
          (e-runtime-store-request--retained-bytes request) bytes
          (e-runtime-store-request--frame-escrow request) escrow
          (e-runtime-store-request--notification request) 'reserved)))

(defun e-runtime-store--request-slot-available-p (store)
  "Return non-nil when STORE can cheaply admit one request/token pair."
  (and (< (e-runtime-store--request-count store)
          e-runtime-store-request-capacity)
       (< (e-runtime-store--notification-count store)
          e-runtime-store-notification-capacity)))

(defun e-runtime-store--release-request (store request)
  "Release REQUEST's one admission reservation exactly once."
  (when-let* ((bytes (e-runtime-store-request--retained-bytes request)))
    (let ((reservation (or (e-runtime-store--reservation store)
                           e-runtime-store--default-reservation))
          (escrow (e-runtime-store-request--frame-escrow request)))
      (setf (e-runtime-store--reserved-bytes store)
            (max 0 (- (e-runtime-store--reserved-bytes store) bytes))
            (e-runtime-store--reservation-used reservation)
            ;; Before the canonical frame is successfully retained, the
            ;; composition still owns ESCROW.  Restore that exact charge so
            ;; its caller performs the one rollback.  After preflight clears
            ;; FRAME-ESCROW, DP5A owns and releases the actual frame itself.
            (if escrow
                (+ (e-runtime-store--reservation-used reservation)
                   (- escrow bytes))
              (max 0 (- (e-runtime-store--reservation-used reservation) bytes)))
            (e-runtime-store--request-count store)
            (max 0 (1- (e-runtime-store--request-count store)))
            (e-runtime-store-request--retained-bytes request) nil
            (e-runtime-store-request--frame-escrow request) nil))))

(defun e-runtime-store--release-terminal (store request)
  "Release REQUEST's token and frame reservation exactly once."
  (when (e-runtime-store-request--notification request)
    (setf (e-runtime-store-request--notification request) nil
          (e-runtime-store--notification-count store)
          (max 0 (1- (e-runtime-store--notification-count store)))))
  ;; Close retains its immutable protocol frame through retirement/final
  ;; notification; ordinary requests may reach here sooner.  This is the one
  ;; terminal owner that releases both bytes and the retained frame.
  (setf (e-runtime-store-request--frame request) nil
        (e-runtime-store-request--frame-bytes request) nil)
  (e-runtime-store--release-request store request))

(defun e-runtime-store--deliver-terminal-notification (store request)
  "Deliver REQUEST's observer outside transport filters and sentinels."
  (let ((observer (and (not (e-runtime-store-request--observer-detached request))
                       (e-runtime-store-request--observer request))))
    (setf (e-runtime-store-request--observer request) nil)
    ;; A client observer is not a scheduler owner.  Its exception is local;
    ;; `unwind-protect' also releases on quit or any nonlocal transfer.
    (unwind-protect
        (when observer
          (condition-case nil
              (funcall observer request)
            (error nil)
            (quit nil)))
      (e-runtime-store--release-terminal store request))))

(defun e-runtime-store--drain-terminal-notifications (store &optional no-schedule)
  "Deliver at most one bounded page of queued terminal observations."
  (setf (e-runtime-store--notification-timer store) nil)
  ;; `quit' and other nonlocal observer exits are client-local.  Always arm
  ;; the remaining page before propagating one, so no terminal ownership is
  ;; stranded behind the observer that escaped.
  (unwind-protect
      (dotimes (_ e-runtime-store-notification-drain-limit)
        (when-let* ((request (pop (e-runtime-store--notification-outbox store))))
          (e-runtime-store--deliver-terminal-notification store request)))
    (when (and (not no-schedule) (e-runtime-store--notification-outbox store))
      (setf (e-runtime-store--notification-timer store)
            (run-at-time 0 nil #'e-runtime-store--drain-terminal-notifications store)))))

(defun e-runtime-store--enqueue-terminal-notification (store request)
  "Queue REQUEST's pre-reserved terminal observation exactly once."
  (when (eq (e-runtime-store-request--notification request) 'reserved)
    (if (e-runtime-store-request--observer-detached request)
        ;; A detached read has no client notification to schedule, but keeps
        ;; ownership through terminal transport settlement.
        (e-runtime-store--release-terminal store request)
      (setf (e-runtime-store-request--notification request) 'queued
            (e-runtime-store--notification-outbox store)
            (nconc (e-runtime-store--notification-outbox store) (list request)))
      ;; Close drains this bounded outbox synchronously during finalization;
      ;; do not leave a fresh timer holding STORE after it has been retired.
      (unless (or (e-runtime-store--closed store)
                  (timerp (e-runtime-store--notification-timer store)))
        (setf (e-runtime-store--notification-timer store)
              (run-at-time 0 nil #'e-runtime-store--drain-terminal-notifications store))))))

(defun e-runtime-store--startup-error (request cause)
  "Return the typed startup failure for scheduler-selected REQUEST and CAUSE."
  (let* ((data (and (consp cause) (cddr cause)))
         (startup-status (plist-get data :startup-status))
         (status (plist-get data :process-status))
         (exit-status (plist-get data :exit-status))
         (stderr (plist-get data :stderr))
         (truncated (plist-get data :stderr-truncated))
         (suffix
          (when (or status exit-status (and stderr (not (string-empty-p stderr))))
            (format " (process status %s, exit status %s%s%s)"
                    (or status 'unknown)
                    (or exit-status 'unknown)
                    (if (and stderr (not (string-empty-p stderr)))
                        (format ", stderr: %s" (string-trim stderr))
                      "")
                    (if truncated " [stderr truncated]" "")))))
    (e-runtime-store--request-error
     'e-runtime-store-unavailable
     (format "Worker startup failed before submitting %s%s"
             (or (e-runtime-store--request-operation request) "request")
             (or suffix ""))
     request :cause cause
     :startup-status startup-status
     :process-status status :exit-status exit-status :stderr stderr
     :stderr-truncated truncated)))

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

(defun e-runtime-store--startup-transport-error
    (store cause &optional output &rest properties)
  "Return bounded pre-acknowledgement failure CAUSE for STORE.

OUTPUT is child stdout observed while the worker is still in its explicit
`opening' lifecycle state.  Once the open response has committed, callers keep
using `e-runtime-store--protocol-error' so established blank frames and other
corruption retain their existing typed protocol cause."
  (let* ((request (e-runtime-store--failure-request store))
         (bounded-output
          (and (stringp output)
               (e-runtime-store--utf8-prefix
                output e-runtime-store-startup-diagnostic-byte-limit)))
         (diagnostic (e-runtime-store--worker-diagnostic store)))
    (apply #'e-runtime-store--request-error
           'e-runtime-store-timeout
           (if (and bounded-output (not (string-empty-p bounded-output)))
               (format "Worker startup emitted unexpected output before acknowledgement: %s"
                       (string-trim bounded-output))
             (format "Worker startup failed before acknowledgement (%s)" cause))
           request
           (append (list :cause cause
                         :startup-status 'opening
                         :startup-output bounded-output)
                   properties
                   diagnostic))))

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

(defun e-runtime-store--task-enqueue-conflict-p (request error)
  "Return non-nil for a caller conflict on an owned task-enqueue request."
  (let ((owner-key (e-runtime-store-request--owner-key request)))
    (and (eq (e-runtime-store-request--kind request) 'write)
         (eq (car-safe error) 'e-runtime-store-task-conflict)
         (eq (e-runtime-store--request-operation request) 'task-enqueue)
         (consp owner-key)
         (eq (car owner-key) 'task)
         (stringp (cdr owner-key)))))

(defun e-runtime-store--board-generation-conflict-p (request error)
  "Return non-nil for REQUEST's exact stale Board-generation precondition."
  (let ((owner-key (e-runtime-store-request--owner-key request)))
    (and (eq (e-runtime-store-request--kind request) 'write)
         (eq (car-safe error) 'e-runtime-store-board-conflict)
         (proper-list-p error)
         (= (length error) 5)
         (equal (cadr error) "Stale Board generation")
         (consp owner-key)
         (eq (car owner-key) 'board)
         (stringp (cdr owner-key))
         (equal (nth 2 error) (cdr owner-key))
         (integerp (nth 3 error))
         (integerp (nth 4 error))
         (/= (nth 3 error) (nth 4 error)))))

(defun e-runtime-store--settle (store request response)
  "Settle REQUEST on STORE from decoded RESPONSE."
  (remhash (e-runtime-store-request--id request)
           (e-runtime-store--pending store))
  (setf (e-runtime-store--active-request store) nil)
  (when (eq (e-runtime-store-request--kind request) 'open)
    ;; The singleton frame was already discarded after a complete send, but
    ;; its correlation slot must retire on either terminal response.
    (setf (e-runtime-store--open-control-request store) nil
          (e-runtime-store-request--frame-bytes request) nil))
  (if (eq (plist-get response :ok) t)
      (setf (e-runtime-store-request--state request) 'committed
            (e-runtime-store-request--result request)
            (plist-get response :result)
            (e-runtime-store--last-error store) nil)
    (let ((err (condition-case caught
                   (e-runtime-store--signal-response-error response)
                 (error caught))))
      (cond
       ((or (e-runtime-store--task-enqueue-conflict-p request err)
            (e-runtime-store--board-generation-conflict-p request err))
        ;; These typed caller preconditions reject before mutation, so they do
        ;; not make an otherwise usable owner suspect.
        (e-runtime-store--fail-request store request err))
       ((and (eq (e-runtime-store-request--kind request) 'write)
             (e-runtime-store-request--owner-key request))
        ;; A valid, correlated negative response is a domain failure, not a
        ;; transport incident.  Partition this optimistic owner while keeping
        ;; the healthy process and unrelated FIFO entries in place.
        (e-runtime-store--partition-owner-failure store request err t))
       (t (e-runtime-store--fail-request store request err)))))
  (unless (eq (e-runtime-store-request--kind request) 'close)
    (setf (e-runtime-store-request--frame request) nil
          (e-runtime-store-request--frame-bytes request) nil))
  (when (and (eq (e-runtime-store-request--kind request) 'write)
             (eq (e-runtime-store-request--state request) 'committed))
    (setf (e-runtime-store--acknowledged-write-prefix store)
          (max (e-runtime-store--acknowledged-write-prefix store)
               (or (e-runtime-store-request--write-prefix request) 0))))
  (when (and (not (eq (e-runtime-store-request--kind request) 'open))
             (memq (e-runtime-store-request--state request) '(committed failed)))
    (setf (e-runtime-store--recovery-attempt store) 0
          (e-runtime-store--recovery-cause store) nil))
  (e-runtime-store--record-request-latency store request)
  ;; An internal open is only a transport prerequisite.  It is advanced by
  ;; scheduler events, never by the submitter or an awaiter.
  (cond
   ((and (eq (e-runtime-store-request--kind request) 'open)
         (e-runtime-store--recovering-request store))
    (if (eq (e-runtime-store-request--state request) 'committed)
        (let ((replay (e-runtime-store--recovering-request store)))
          ;; A successful replacement open has its own acknowledgement.
          (setf (e-runtime-store--active-request store) replay
                (e-runtime-store--opened-process store)
                (e-runtime-store--process store)
                (e-runtime-store--recovering-request store) nil)
          (condition-case err
              (progn
                (unless (stringp (e-runtime-store-request--frame replay))
                  (signal 'e-runtime-store-error
                          (list "Recovery lost its immutable request frame")))
                (process-send-string
                 (e-runtime-store--process store)
                 (e-runtime-store--pack-canonical
                  (e-runtime-store-request--frame replay)))
                (setf (e-runtime-store-request--submitted-at replay) (float-time)))
            (error
             (e-runtime-store--recovery-exhausted
              store (or (e-runtime-store--recovery-cause store) err)))))
      ;; Fencing can win the local process race before the child drops its
      ;; cross-process ownership lock.  That transient refusal is not a
      ;; second domain recovery attempt: wait for the replacement child to
      ;; die, then retry opening under the same retained request identity.
      (if (eq (plist-get response :error-symbol) 'e-runtime-store-owner-active)
          (progn
            (e-runtime-store--fence-worker store)
            (e-runtime-store--schedule store 0.01))
        (e-runtime-store--recovery-exhausted
         store (or (e-runtime-store--recovery-cause store)
                   (e-runtime-store-request--error request))))))
   ((eq (e-runtime-store-request--kind request) 'open)
    (when (eq (e-runtime-store-request--state request) 'committed)
      (setf (e-runtime-store--opened-process store)
            (e-runtime-store--process store)
            (e-runtime-store--startup-status store) 'ready)
      ;; Prewarm the subordinate read transport after (and only after) the
      ;; writer has created and verified the schema.  This opens no domain
      ;; state, but keeps the first interactive query from paying process
      ;; startup latency or joining the writer's lane.
      (when (and (eq (e-runtime-store--access-mode store) 'read-write)
                 (null (e-runtime-store--parent-store store)))
        (condition-case err
            (e-runtime-store--read-client-for store)
          (error
           ;; Reader setup is request-local capacity.  The writer remains the
           ;; healthy durable authority and a later read retries lazily.
           (setf (e-runtime-store--last-error store) err)))))
    ;; A selected queue entry may have cancelled or expired while open was in
    ;; flight.  Never let that stale identity explain a later startup result.
    (unless (or (null (e-runtime-store--starting-request store))
                (e-runtime-store--queued-request-p
                 store (e-runtime-store--starting-request store)))
      (setf (e-runtime-store--starting-request store) nil))
    (when (not (eq (e-runtime-store-request--state request) 'committed))
      (setf (e-runtime-store--startup-status store) 'failed)
      (let ((selected (e-runtime-store--startup-request store))
            (open-error (e-runtime-store-request--error request)))
        (if (eq (car-safe open-error) 'e-runtime-store-schema-too-old)
            ;; This is a store-wide operator prerequisite, not an optimistic
            ;; mutation failure.  Settle every queued request with the exact
            ;; schema condition and stop retrying; no owner becomes suspect.
            (e-runtime-store--freeze-and-stop store open-error)
          (if selected
              (progn
                (setf (e-runtime-store--starting-request store) selected)
                (e-runtime-store--partition-owner-failure
                 store selected
                 (e-runtime-store--startup-error selected open-error)))
            ;; A cold open has no selected domain request to explain failure.
            ;; Its own typed worker result is still terminal: retaining an
            ;; unowned opening state would leave compatibility observers and
            ;; close teardown waiting until a phase timeout.
            (e-runtime-store--partition-owner-failure
             store nil open-error)))))
    (e-runtime-store--schedule store t))
   ((eq (e-runtime-store-request--kind request) 'close)
    (e-runtime-store--schedule-close-finalization store))
   ((not (eq (e-runtime-store-request--kind request) 'open))
    (when (eq (e-runtime-store-request--state request) 'committed)
      (e-runtime-store--enqueue-terminal-notification store request))
    (e-runtime-store--schedule store t))))

(defun e-runtime-store--freeze-oversized-response (store wire-bytes)
  "Recover submitted work after an oversized response frame of WIRE-BYTES."
  (e-runtime-store--recover-or-fail
   store
   (if (eq (e-runtime-store--startup-status store) 'opening)
       (e-runtime-store--startup-transport-error
        store 'startup-output-too-large nil
        :wire-bytes wire-bytes
        :wire-limit e-runtime-store-codec-protocol-wire-byte-limit)
     (e-runtime-store--protocol-error
      store 'response-frame-too-large
      :wire-bytes wire-bytes
      :wire-limit e-runtime-store-codec-protocol-wire-byte-limit))))

(defun e-runtime-store--consume-response-line (store line)
  "Decode and settle one complete protocol LINE for STORE."
  ;; The private protocol has no keepalive frame.  Once opening has committed,
  ;; a delimiter always closes one response, so a bare newline is corruption
  ;; rather than ignorable idle output and preserves the active request as its
  ;; first failure cause.  The pre-acknowledgement branch below reports startup
  ;; transport evidence separately.
  (if (string-empty-p line)
      (if (eq (e-runtime-store--startup-status store) 'opening)
          ;; A child that prints a blank line while its open control is still
          ;; outstanding has not established the protocol yet.  Keep this
          ;; distinct from a blank frame after the open acknowledgement; the
          ;; latter remains the established-protocol corruption path below.
          (e-runtime-store--recover-or-fail
           store
           (e-runtime-store--startup-transport-error
            store 'startup-empty-response))
        (e-runtime-store--recover-or-fail
         store (e-runtime-store--protocol-error store 'empty-response)))
    (let ((wire-bytes (1+ (string-bytes line))))
      (if (> wire-bytes e-runtime-store-codec-protocol-wire-byte-limit)
          (e-runtime-store--freeze-oversized-response store wire-bytes)
        (condition-case err
            (let ((response (e-runtime-store--unpack line)))
              (if (not (e-runtime-store--response-valid-p response))
                  (e-runtime-store--recover-or-fail
                   store
                   (if (eq (e-runtime-store--startup-status store) 'opening)
                       (e-runtime-store--startup-transport-error
                        store 'startup-malformed-response line)
                     (e-runtime-store--protocol-error
                      store 'malformed-response)))
                (let* ((id (plist-get response :id))
                       (request
                        (gethash id (e-runtime-store--pending store))))
                  (cond
                   ((not request)
                    (e-runtime-store--recover-or-fail
                     store
                     (if (eq (e-runtime-store--startup-status store) 'opening)
                         (e-runtime-store--startup-transport-error
                          store 'startup-unknown-response-id line)
                       (e-runtime-store--protocol-error
                        store 'unknown-response-id :response-id id))))
                   ((not (eq request (e-runtime-store--active-request store)))
                     (e-runtime-store--recover-or-fail
                      store
                      (if (eq (e-runtime-store--startup-status store) 'opening)
                          (e-runtime-store--startup-transport-error
                           store 'startup-uncorrelated-response line)
                        (e-runtime-store--protocol-error
                         store 'uncorrelated-response :response-id id))))
                   (t (e-runtime-store--settle store request response))))))
          (e-runtime-store-codec-too-large
           (e-runtime-store--freeze-oversized-response store wire-bytes))
          (error
           (e-runtime-store--recover-or-fail
            store
            (if (eq (e-runtime-store--startup-status store) 'opening)
                (e-runtime-store--startup-transport-error
                 store 'startup-decode-error line)
              (e-runtime-store--protocol-error
               store 'decode-error :cause err)))))))))

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
                                (e-runtime-store--active-request store)
                                (e-runtime-store--open-control-request store)
                                (e-runtime-store--closing-request store))
                          (e-runtime-store--client-queue store)
                          pending-requests))
            :test #'eq)))
      (unless (e-runtime-store--unavailable-cause store)
        (setf (e-runtime-store--unavailable-cause store) error))
      (setf (e-runtime-store--active-request store) nil
            (e-runtime-store--starting-request store) nil
            (e-runtime-store--open-control-request store) nil
            (e-runtime-store--client-queue store) nil
            (e-runtime-store--opened-process store) nil
            (e-runtime-store--unavailable store) t
            (e-runtime-store--last-error store) error)
      (dolist (request requests)
        (unless (memq (e-runtime-store-request--state request)
                      '(committed failed cancelled))
          (e-runtime-store--fail-request store request error))))))

(defun e-runtime-store--utf8-prefix (string limit)
  "Return the longest prefix of STRING occupying at most LIMIT UTF-8 bytes."
  (if (<= (string-bytes string) limit)
      string
    (let ((low 0) (high (length string)))
      (while (< low high)
        (let ((mid (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring string 0 mid)) limit)
              (setq low mid)
            (setq high (1- mid)))))
      (substring string 0 low))))

(defun e-runtime-store--capture-stderr (store text)
  "Append TEXT to STORE's bounded worker diagnostic buffer."
  (let* ((bytes (or (e-runtime-store--stderr-bytes store) 0))
         (remaining (- e-runtime-store-startup-diagnostic-byte-limit bytes))
         (prefix (if (> remaining 0)
                     (e-runtime-store--utf8-prefix text remaining)
                   "")))
    (when (and (stringp prefix) (> (string-bytes prefix) 0)
               (buffer-live-p (e-runtime-store--stderr-buffer store)))
      (with-current-buffer (e-runtime-store--stderr-buffer store)
        (goto-char (point-max))
        (insert prefix)))
    (setf (e-runtime-store--stderr-bytes store)
          (+ bytes (string-bytes prefix))
          (e-runtime-store--stderr-truncated store)
          (or (e-runtime-store--stderr-truncated store)
              (< (string-bytes prefix) (string-bytes text))))))

(defun e-runtime-store--worker-diagnostic (store)
  "Return bounded process and stderr facts for STORE's current worker."
  (let* ((process (e-runtime-store--process store))
         (status (and (processp process) (process-status process)))
         (exit-status
          (and (processp process)
               (memq status '(exit signal failed closed))
               (condition-case nil
                   (process-exit-status process)
                 (error nil))))
         (stderr
          (if (buffer-live-p (e-runtime-store--stderr-buffer store))
              (with-current-buffer (e-runtime-store--stderr-buffer store)
                (buffer-string))
            "")))
    (list :process-status status :exit-status exit-status
          :stderr stderr
          :stderr-truncated (and (e-runtime-store--stderr-truncated store) t))))

(defun e-runtime-store--stop-stderr-process (store)
  "Detach STORE's bounded stderr pipe and release its process object."
  (when-let* ((stderr-process (e-runtime-store--stderr-process store)))
    (when (processp stderr-process)
      (set-process-filter stderr-process #'ignore)
      (set-process-sentinel stderr-process #'ignore)
      (when (process-live-p stderr-process)
        (delete-process stderr-process))))
  (setf (e-runtime-store--stderr-process store) nil))

(defun e-runtime-store--clear-stderr (store)
  "Release STORE's bounded stderr process and buffer."
  (e-runtime-store--stop-stderr-process store)
  (when (buffer-live-p (e-runtime-store--stderr-buffer store))
    (kill-buffer (e-runtime-store--stderr-buffer store)))
  (setf (e-runtime-store--stderr-buffer store) nil
        (e-runtime-store--stderr-bytes store) 0
        (e-runtime-store--stderr-truncated store) nil))

(defun e-runtime-store--bounded-diagnostic (cause)
  "Return CAUSE's first diagnostic bounded to 1,024 UTF-8 bytes."
  (e-runtime-store--utf8-prefix
   (let ((print-circle t) (print-level 6) (print-length 32))
     (condition-case nil
         (error-message-string cause)
       (error "Runtime-store persistence failed")))
   e-runtime-store-owner-diagnostic-byte-limit))

(defun e-runtime-store--copy-owner-key (owner-key)
  "Return a detached copy of bounded OWNER-KEY, or nil."
  (and owner-key
       (cons (car owner-key) (copy-sequence (cdr owner-key)))))

(defun e-runtime-store--owner-first-error (request cause)
  "Return REQUEST's bounded first terminal error derived from CAUSE."
  ;; Do not retain arbitrary worker/transport error data: it may contain a
  ;; payload-sized string or a caller-owned container.  The condition type and
  ;; request correlation remain exact, while the only free-form value is the
  ;; bounded detached diagnostic.
  (let ((type (if (and (consp cause) (symbolp (car cause))
                       (get (car cause) 'error-conditions))
                  (car cause)
                'e-runtime-store-error)))
    (list type (e-runtime-store--bounded-diagnostic cause)
          :operation (e-runtime-store--request-operation request)
          :kind (and request (e-runtime-store-request--kind request))
          :request-id (and request (e-runtime-store-request--id request)))))

(defun e-runtime-store--owner-successor-error (request cause)
  "Return REQUEST's typed owner-fence error derived from first CAUSE."
  (list 'e-runtime-store-persistence-suspect
        "Persistence is suspect after an earlier owner mutation failed"
        :operation (e-runtime-store--request-operation request)
        :kind (e-runtime-store-request--kind request)
        :request-id (e-runtime-store-request--id request)
        :owner-key
        (e-runtime-store--copy-owner-key
         (e-runtime-store-request--owner-key request))
        :first-diagnostic
        (copy-sequence (e-runtime-store--bounded-diagnostic cause))))

(defun e-runtime-store--partition-owner-failure
    (store request cause &optional preserve-worker-p)
  "Fail REQUEST and same-owner successors, retaining unrelated FIFO entries.

Nil owner keys never group: a nil-key read or non-optimistic operation fails
only itself.  Every terminal client still passes through `--fail-request', so
its pre-reserved notification token remains owned until isolated delivery.
When PRESERVE-WORKER-P is non-nil, CAUSE is a correlated negative response:
the healthy worker and already-consumed input remainder stay authoritative."
  (let* ((owner-key (and request (e-runtime-store-request--owner-key request)))
         ;; Only optimistic session/Board owners need the bounded detached
         ;; diagnostic retained by the owner fence.  Transport control,
         ;; startup, close, and reads have no owner key; preserve their exact
         ;; first cause for the existing recovery contract.
         (first-error (and request
                           (if owner-key
                               (e-runtime-store--owner-first-error request cause)
                             cause)))
         (active (e-runtime-store--active-request store))
         (open (and active
                    (eq (e-runtime-store-request--kind active) 'open)
                    active))
         kept successors)
    (dolist (queued (e-runtime-store--client-queue store))
      (cond
       ((eq queued request) nil)
       ((and owner-key
             (equal owner-key (e-runtime-store-request--owner-key queued)))
        (push queued successors))
       (t (push queued kept))))
    (when owner-key
      (unless (hash-table-p (e-runtime-store--suspect-owners store))
        (setf (e-runtime-store--suspect-owners store)
              (make-hash-table :test #'equal)))
      (unless (gethash owner-key (e-runtime-store--suspect-owners store))
        (puthash (e-runtime-store--copy-owner-key owner-key)
                 (copy-sequence
                  (e-runtime-store--bounded-diagnostic
                   (or first-error cause)))
                 (e-runtime-store--suspect-owners store))))
    (setf (e-runtime-store--client-queue store) (nreverse kept)
          (e-runtime-store--starting-request store) nil
          (e-runtime-store--recovering-request store) nil
          (e-runtime-store--recovery-cause store) nil
          (e-runtime-store--recovery-attempt store) 0
          (e-runtime-store--active-request store) nil
          (e-runtime-store--last-error store) (or first-error cause))
    (unless preserve-worker-p
      (setf (e-runtime-store--opened-process store) nil
            (e-runtime-store--input-fragment store) "")
      (when open
        (setf (e-runtime-store--startup-status store) 'failed)))
    (when open
      (e-runtime-store--release-open-control store open)
      (setf (e-runtime-store-request--state open) 'failed
            (e-runtime-store-request--error open) cause)
      (e-runtime-store--record-request-latency store open))
    (when (and request (e-runtime-store--request-live-p request))
      (e-runtime-store--fail-request
       store request first-error))
    (dolist (successor (nreverse successors))
      (e-runtime-store--fail-request
       store successor (e-runtime-store--owner-successor-error successor cause)))
    ;; Real transport processes must be detached before the retained FIFO can
    ;; restart.  Synthetic scheduler fixtures use non-process sentinels; do
    ;; not count those as another physical fence after recovery exhaustion.
    (when (and (not preserve-worker-p)
               (processp (e-runtime-store--process store)))
      (e-runtime-store--fence-worker store))
    (unless preserve-worker-p
      (setf (e-runtime-store--process store) nil))
    ;; Replacement is demand-driven: a retained unrelated head restarts now;
    ;; an empty queue stays idle until a later submission schedules it.
    (cond
     ((and request (eq (e-runtime-store-request--kind request) 'close))
      (e-runtime-store--schedule-close-finalization store))
     ((e-runtime-store--client-queue store)
      (e-runtime-store--schedule store t)))))

(defun e-runtime-store--freeze-and-stop (store error)
  "Freeze STORE with ERROR and stop its worker without processing more output."
  (when (timerp (e-runtime-store--scheduler-timer store))
    (cancel-timer (e-runtime-store--scheduler-timer store)))
  (setf (e-runtime-store--scheduler-timer store) nil)
  (unwind-protect
      (e-runtime-store--fail-all store error)
    (when (processp (e-runtime-store--process store))
      ;; Once acknowledgement is ambiguous, no buffered response may publish a
      ;; late terminal state.  Reopening reloads the canonical database.
      (set-process-filter (e-runtime-store--process store) #'ignore)
      (set-process-sentinel (e-runtime-store--process store) #'ignore)
      (when (process-live-p (e-runtime-store--process store))
        (delete-process (e-runtime-store--process store))))
    (e-runtime-store--clear-stderr store)))

(defun e-runtime-store--fence-worker (store)
  "Detach STORE from its current worker before a bounded replacement.
Old filter and sentinel callbacks must not settle a replayed request."
  (when-let* ((process (e-runtime-store--process store)))
    (when (processp process)
      (set-process-filter process #'ignore)
      (set-process-sentinel process #'ignore)
      (when (process-live-p process)
        (delete-process process))))
  (e-runtime-store--stop-stderr-process store))

(defun e-runtime-store--recovery-exhausted (store cause)
  "Partition one owner after recovery exhausts, preserving the first CAUSE."
  (let ((first (or (e-runtime-store--recovery-cause store) cause))
        (request (or (e-runtime-store--recovering-request store)
                     (e-runtime-store--failure-request store))))
    (e-runtime-store--partition-owner-failure store request first)))

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
              ;; The old process can retain the ownership lock until its
              ;; asynchronous death completes.  A scheduler turn observes
              ;; that boundary before opening the replacement; spinning or
              ;; waiting here would reintroduce an interactive-stack wait.
              (e-runtime-store--schedule store 0.01))
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
         (eq (e-runtime-store-request--kind
              (e-runtime-store--active-request store)) 'open))
    ;; Initial cold open has not submitted a domain request.  Its selected
    ;; queue owner receives the typed startup cause; replay is only for an
    ;; already submitted domain request or a replacement open above.
    (let ((selected (e-runtime-store--startup-request store)))
      (if selected
          (e-runtime-store--partition-owner-failure
           store selected (e-runtime-store--startup-error selected error))
        ;; No client owns a cold-open attempt.  Retire only the transport
        ;; control and remain restartable for the next submission.
        (e-runtime-store--partition-owner-failure store nil error))))
   ((and (e-runtime-store--active-request store)
         (eq (e-runtime-store-request--state
              (e-runtime-store--active-request store)) 'submitted))
    (e-runtime-store--recover-active store error))
   (t (e-runtime-store--partition-owner-failure
       store (e-runtime-store--failure-request store) error))))

(defun e-runtime-store--worker-exited (store)
  "Recover one submitted request after worker loss when possible."
  (unless (or (e-runtime-store--closed store)
              (e-runtime-store--unavailable store))
    (let* ((request (e-runtime-store--failure-request store))
           (operation (e-runtime-store--request-operation request))
           (diagnostic (e-runtime-store--worker-diagnostic store))
           (startup-p (eq (e-runtime-store--startup-status store) 'opening))
           (status (plist-get diagnostic :process-status))
           (exit-status (plist-get diagnostic :exit-status))
           (stderr (plist-get diagnostic :stderr))
           (truncated (plist-get diagnostic :stderr-truncated))
           (suffix
            (format " (process status %s, exit status %s%s%s)"
                    (or status 'unknown)
                    (or exit-status 'unknown)
                    (if (string-empty-p stderr)
                        ""
                      (format ", stderr: %s" (string-trim stderr)))
                    (if truncated " [stderr truncated]" ""))))
      (e-runtime-store--recover-or-fail
       store
       (apply #'e-runtime-store--request-error
              'e-runtime-store-timeout
              (if startup-p
                  (format "Worker startup failed before acknowledging %s%s"
                          (or operation "request") suffix)
                (if operation
                    (format "Worker exited before acknowledging %s; reload canonical state%s"
                            operation suffix)
                  (format "Worker exited before acknowledging the request%s" suffix)))
              request :cause 'worker-exited
              :startup-status (and startup-p 'opening)
              diagnostic)))))

(defun e-runtime-store--start-process (store)
  "Start STORE's worker process and return it."
  (when (e-runtime-store--closed store)
    (signal 'e-runtime-store-unavailable (list "Store is closed")))
  (when (e-runtime-store--unavailable store)
    (e-runtime-store--signal-unavailable store))
  (unless (e-runtime-store--live-p store)
    (e-runtime-store--clear-stderr store)
    (let* ((stderr (generate-new-buffer " *e-runtime-store-stderr*"))
           ;; `:stderr' accepts a pipe process with its own filter.  Keeping
           ;; the cap in that filter prevents a noisy pre-main child from
           ;; retaining an unbounded diagnostic buffer in the parent.
           (stderr-process
            (make-pipe-process
             :name (format "e-runtime-store-stderr-%s"
                           (substring (e-runtime-store--runtime-id store) 0
                                      (min 8 (length
                                              (e-runtime-store--runtime-id
                                               store)))))
             :buffer nil :coding 'utf-8-unix :noquery t
             :filter (lambda (_process text)
                       (e-runtime-store--capture-stderr store text))
             :sentinel #'ignore)))
      (setf (e-runtime-store--stderr-buffer store) stderr
            (e-runtime-store--stderr-process store) stderr-process
            (e-runtime-store--stderr-bytes store) 0
            (e-runtime-store--stderr-truncated store) nil
            (e-runtime-store--input-fragment store) ""
            (e-runtime-store--process store)
            (make-process
             :name (format "e-runtime-store-%s"
                           (substring (e-runtime-store--runtime-id store) 0
                                      (min 8 (length
                                              (e-runtime-store--runtime-id
                                               store)))))
             :buffer nil :stderr stderr-process :command (e-runtime-store--command)
             :connection-type 'pipe :coding 'utf-8-unix :noquery t
             :filter (lambda (_process text)
                       (e-runtime-store--consume-output store text))
             :sentinel (lambda (_process _event)
                         (unless (e-runtime-store--live-p store)
                           (e-runtime-store--worker-exited store)))))
      (set-process-query-on-exit-flag (e-runtime-store--process store) nil)
      (when (e-runtime-store--borrowed-claim store)
        (e-runtime-store--send-borrow-authorization store))))
  (e-runtime-store--process store))

(defun e-runtime-store--ensure-worker-open (store)
  "Begin opening STORE in its current worker without waiting for an ack."
  (unless (or (eq (e-runtime-store--opened-process store)
                  (e-runtime-store--process store))
              ;; The singleton control is intentionally not client-admitted;
              ;; do not construct a second one if a scheduler turn re-enters
              ;; while the first open acknowledgement is outstanding.
              (when-let* ((active (e-runtime-store--active-request store)))
                (eq (e-runtime-store-request--kind active) 'open)))
    (let ((request (e-runtime-store--prepare-open-control store)))
      (setf (e-runtime-store--active-request store) request)
      (puthash (e-runtime-store-request--id request) request
               (e-runtime-store--pending store))
      (condition-case send-error
          (progn
            (process-send-string (e-runtime-store--process store)
                                 (e-runtime-store--pack-canonical
                                  (e-runtime-store-request--frame request)))
            ;; The subprocess accepted the complete frame.  The pending
            ;; correlation owns only small facts from this point.
            (setf (e-runtime-store-request--frame request) nil
                  (e-runtime-store-request--frame-bytes request) nil
                  (e-runtime-store--startup-status store) 'opening)
            request)
        (error
         ;; A signalled send can follow a partial pipe write.  Treat it as a
         ;; transport incident: no old output may later settle the selected
         ;; client as if this open had never been attempted.
         (let ((selected (e-runtime-store--startup-request store)))
           (e-runtime-store--fence-worker store)
           (e-runtime-store--release-open-control store request)
           (setf (e-runtime-store-request--state request) 'failed
                 (e-runtime-store-request--error request) send-error)
           (e-runtime-store--record-request-latency store request)
           (if (e-runtime-store--recovering-request store)
               (e-runtime-store--recovery-exhausted
                store (or (e-runtime-store--recovery-cause store) send-error))
             (e-runtime-store--partition-owner-failure
              store selected
              (if selected
                  (e-runtime-store--startup-error selected send-error)
                send-error)))))))))

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

(defun e-runtime-store--measure-frame-escrow (store kind body)
  "Return an allocation-free upper bound for STORE KIND BODY's protocol frame.

The session composition uses this before it can allocate a retained immutable
frame.  A maximal same-runtime request id and write/ack prefixes dominate the
later concrete request while BODY remains only referenced by the measured
wrapper."
  (let* ((prefix (if (eq kind 'write) "w" "r"))
         (maximum most-positive-fixnum)
         (value
          ;; `e-runtime-store--request-frame' carries the prefix keys for
          ;; both reads and writes (read values are nil).  Measure the same
          ;; complete wire-visible shape; a maximal numeric read prefix is a
          ;; harmless upper bound and keeps future frame changes explicit.
          (list :id (format "%s:%s:%d" (e-runtime-store--runtime-id store)
                            prefix maximum)
                :kind kind :body body :write-prefix maximum
                :ack-prefix (and (eq kind 'write) maximum)))
         (bytes (e-runtime-store-codec-measure-bounded
                 value e-runtime-store-codec-protocol-canonical-byte-limit)))
    (when (> (e-runtime-store-codec-wire-byte-count bytes)
             e-runtime-store-codec-protocol-wire-byte-limit)
      (signal 'e-runtime-store-codec-too-large
              (list "Protocol frame escrow exceeds wire limit" :bytes bytes)))
    bytes))

(defun e-runtime-store--preflight-request (store-or-request &optional maybe-request admit)
  "Return REQUEST's exact bounded canonical frame before queue admission."
  (let ((store (and maybe-request store-or-request))
        (request (or maybe-request store-or-request))
        reserved success)
    (or (e-runtime-store-request--frame request)
        (condition-case err
            (progn
              ;; Count/token saturation is independent of the value.  Reject
              ;; it before walking a caller's potentially large graph.
              (when (and admit (not (e-runtime-store--request-slot-available-p store)))
                (signal 'e-runtime-store-capacity-exhausted
                        (list "Runtime-store request/token capacity is exhausted")))
            (let* ((value (e-runtime-store--request-frame store request))
                   ;; Measure the precise tagged representation first.  This
                   ;; rejects a large request before allocating the complete
                   ;; canonical frame which will be retained for replay.
                   (bytes
                    (e-runtime-store-codec-measure-bounded
                     value
                     e-runtime-store-codec-protocol-canonical-byte-limit)))
              (when (> (e-runtime-store-codec-wire-byte-count bytes)
                       e-runtime-store-codec-protocol-wire-byte-limit)
                (signal 'e-runtime-store-codec-too-large
                        (list "Protocol wire frame exceeds byte limit"
                              :domain 'wire :canonical-bytes bytes)))
              ;; Reserve the measured canonical frame before allocating its
              ;; large string.  Once encoded, scheduler ownership retains the
              ;; immutable frame and typed operation, never caller BODY.
              (when admit
                (if (integerp admit)
                    (e-runtime-store--reserve-request-from-escrow
                     store request bytes admit)
                  (e-runtime-store--reserve-request store request bytes))
                (setq reserved t))
              (unwind-protect
                  (progn
                    (setf (e-runtime-store-request--frame-bytes request) bytes
                          (e-runtime-store-request--frame request)
                          (e-runtime-store--encode-frame value bytes)
                          (e-runtime-store-request--operation request)
                          (plist-get (e-runtime-store-request--body request) :op)
                          (e-runtime-store-request--body request) nil
                          ;; The retained exact frame is now DP5A-owned.  A
                          ;; later terminal release subtracts its actual bytes;
                          ;; only an earlier setup failure restores ESCROW to
                          ;; the composition owner.
                          (e-runtime-store-request--frame-escrow request) nil
                          success t))
                (unless success
                  (when reserved (e-runtime-store--release-terminal store request))))))
        (e-runtime-store-codec-too-large
         (when reserved (e-runtime-store--release-terminal store request))
         (let ((failure
                (e-runtime-store--request-error
                 'e-runtime-store-request-too-large
                 "Runtime-store request exceeds its transport limit"
                 request :cause err)))
           (signal (car failure) (cdr failure))))
        (error
         (when reserved (e-runtime-store--release-terminal store request))
         (signal (car err) (cdr err)))))))

(defun e-runtime-store--fail-request (store request err)
  "Settle REQUEST on STORE with ERR exactly once."
  (when (e-runtime-store--request-live-p request)
    (remhash (e-runtime-store-request--id request)
             (e-runtime-store--pending store))
    (setf (e-runtime-store-request--state request) 'failed
          (e-runtime-store-request--error request) err
          (e-runtime-store--last-error store) err)
    (e-runtime-store--record-request-latency store request)
    (unless (eq (e-runtime-store-request--kind request) 'close)
      (setf (e-runtime-store-request--frame request) nil
            (e-runtime-store-request--frame-bytes request) nil))
    (when (eq (e-runtime-store-request--kind request) 'open)
      (setf (e-runtime-store--open-control-request store) nil
            (e-runtime-store-request--frame-bytes request) nil))
    (unless (eq (e-runtime-store-request--kind request) 'open)
      (if (eq (e-runtime-store-request--kind request) 'close)
          (e-runtime-store--schedule-close-finalization store)
        (e-runtime-store--enqueue-terminal-notification store request)))))

(defun e-runtime-store--queued-request-p (store request)
  "Return non-nil when REQUEST remains scheduler-owned in STORE's queues."
  (and (eq (e-runtime-store-request--state request) 'queued)
       (memq request (e-runtime-store--client-queue store))))

(defun e-runtime-store--remove-queued-request (store request)
  "Remove REQUEST from STORE's scheduler queues and return REQUEST."
  (setf (e-runtime-store--client-queue store)
        (delq request (e-runtime-store--client-queue store)))
  (when (eq request (e-runtime-store--starting-request store))
    (setf (e-runtime-store--starting-request store) nil))
  request)

(defun e-runtime-store--take-queued-request (store request)
  "Take still-queued REQUEST from STORE only after transport opening succeeds."
  (unless (e-runtime-store--queued-request-p store request)
    (signal 'e-runtime-store-error
            (list "Scheduler lost its queued request" request)))
  (e-runtime-store--remove-queued-request store request))

(defun e-runtime-store--next-queued-request (store)
  "Return STORE's next request without releasing scheduler ownership."
  (car (e-runtime-store--client-queue store)))

(defun e-runtime-store--scheduler-deadline (store)
  "Return STORE's earliest owned phase deadline, or nil when idle."
  (let ((active (e-runtime-store--active-request store)) deadlines)
    (when (and active (eq (e-runtime-store-request--state active) 'submitted))
      (push (e-runtime-store--request-deadline
             active (or (e-runtime-store-request--timeout-interval active)
                        e-runtime-store-request-timeout))
            deadlines))
    (dolist (request (e-runtime-store--client-queue store))
      (push (e-runtime-store--request-deadline
             request (or (e-runtime-store-request--timeout-interval request)
                         e-runtime-store-request-timeout))
            deadlines))
    (when deadlines (apply #'min deadlines))))

(defun e-runtime-store--expire-overdue-queued (store interval)
  "Expire at most one scheduler page of overdue queued requests.

Return non-nil when another immediate timer turn is needed."
  (let ((expired 0) more)
    (catch 'page-full
      (dolist (request (copy-sequence (e-runtime-store--client-queue store)))
    (when (and (e-runtime-store--queued-request-p store request)
               (>= (float-time)
                   (e-runtime-store--request-deadline
                    request
                    (or (e-runtime-store-request--timeout-interval request)
                        interval))))
      (let ((active (e-runtime-store--active-request store)))
        (e-runtime-store--remove-queued-request store request)
        (e-runtime-store--fail-request
         store request
         (e-runtime-store--request-error
          'e-runtime-store-timeout
          "Runtime-store request expired before submission"
          request :request-state 'queued
          :blocking-operation (e-runtime-store--request-operation active)
          :blocking-kind
          (and active (e-runtime-store-request--kind active))
          :blocking-request-id
          (and active (e-runtime-store-request--id active)))))
      (setq expired (1+ expired))
      (when (>= expired e-runtime-store-notification-drain-limit)
        (setq more t)
        (throw 'page-full t)))))
    more))

(defun e-runtime-store--finalize-close (store)
  "Release STORE after its private close request has terminally resolved."
  (when (timerp (e-runtime-store--close-finalizer-timer store))
    (cancel-timer (e-runtime-store--close-finalizer-timer store)))
  (setf (e-runtime-store--close-finalizer-timer store) nil)
  (let ((process (e-runtime-store--process store))
        (close-request (e-runtime-store--closing-request store)))
      (setf (e-runtime-store--closed store) t)
      (when (timerp (e-runtime-store--scheduler-timer store))
        (cancel-timer (e-runtime-store--scheduler-timer store)))
      (when (timerp (e-runtime-store--notification-timer store))
        (cancel-timer (e-runtime-store--notification-timer store)))
      (setf (e-runtime-store--scheduler-timer store) nil
            (e-runtime-store--notification-timer store) nil)
      (when (processp process)
        (set-process-filter process #'ignore)
        (set-process-sentinel process #'ignore))
      (e-runtime-store--fail-all store '(e-runtime-store-unavailable "Store is closed"))
      (when (processp process)
        (when (process-live-p process) (delete-process process)))
      (e-runtime-store--clear-stderr store)
      ;; The close observer is the final client-visible event: resources are
      ;; gone and no scheduler/recovery/close reference can revive transport.
      (setf (e-runtime-store--process store) nil
            (e-runtime-store--opened-process store) nil
            (e-runtime-store--stderr-buffer store) nil
            (e-runtime-store--stderr-process store) nil
            (e-runtime-store--stderr-bytes store) 0
            (e-runtime-store--stderr-truncated store) nil
            (e-runtime-store--input-fragment store) nil
            (e-runtime-store--active-request store) nil
            (e-runtime-store--starting-request store) nil
            (e-runtime-store--recovering-request store) nil
            (e-runtime-store--recovery-cause store) nil)
      ;; A close cannot monopolize a timer turn.  Resources are already
      ;; retired; drain one ordinary bounded page, then yield before the next.
      (e-runtime-store--drain-terminal-notifications store t)
      (if (e-runtime-store--notification-outbox store)
          (setf (e-runtime-store--close-finalizer-timer store)
                (run-at-time 0 nil #'e-runtime-store--finalize-close store))
        (setf (e-runtime-store--closing-request store) nil)
        (when close-request
          (e-runtime-store--deliver-terminal-notification store close-request)))))

(defun e-runtime-store--schedule-close-finalization (store)
  "Schedule STORE's nonblocking terminal close cleanup exactly once."
  (unless (or (e-runtime-store--closed store)
              (timerp (e-runtime-store--close-finalizer-timer store)))
    (setf (e-runtime-store--close-finalizer-timer store)
          (run-at-time 0 nil #'e-runtime-store--finalize-close store))))

(defun e-runtime-store--close-start (store)
  "Return STORE's private asynchronous close request before its acknowledgement.

Close is client-visible completion work: it reserves the same bounded request
and notification ownership as every other client request.  Its private handle
waits in order for active transport work; the legacy blocking wrapper may then
choose immediate local finalization without letting the close escape the cap."
  (or (e-runtime-store--closing-request store)
      (let ((request (e-runtime-store-request--create
                      ;; Do not advance STORE sequence until complete close
                      ;; admission succeeds; a capacity/encode failure leaves
                      ;; the public compatibility surface unchanged/retryable.
                      :id (format "%s:close:%d" (e-runtime-store--runtime-id store)
                                  (1+ (e-runtime-store--sequence store)))
                      :kind 'close :body nil :state 'queued
                      :owner-store store
                      :admitted-at (float-time)
                      :timeout-interval e-runtime-store-request-timeout)))
        ;; This is a client request, not scheduler scaffolding.  Preflight
        ;; measures/reserves the complete immutable close frame and its token
        ;; atomically before publishing closing-request or fencing anything.
        ;; Its own unwind-protect rolls every reservation back on encode/setup
        ;; failure, leaving a later close retry equivalent to the first call.
        (e-runtime-store--preflight-request store request t)
        (cl-incf (e-runtime-store--sequence store))
        (setf (e-runtime-store--closing-request store) request)
        (e-runtime-store--schedule store t)
        request)))

(defun e-runtime-store--dispatch-close (store)
  "Advance STORE's queued private close request without waiting."
  (let ((request (e-runtime-store--closing-request store)))
    (when (and request (eq (e-runtime-store-request--state request) 'queued)
               (not (e-runtime-store--active-request store)))
      (if (eq (e-runtime-store--opened-process store)
              (e-runtime-store--process store))
          (condition-case err
              (let ((frame (e-runtime-store-request--frame request)))
                (unless (stringp frame)
                  (signal 'e-runtime-store-error
                          (list "Close lost its immutable admitted frame" request)))
                (let ((now (float-time)))
                  (setf (e-runtime-store-request--frame request) frame
                        (e-runtime-store-request--state request) 'submitted
                        (e-runtime-store-request--submitted-at request) now
                        (e-runtime-store-request--first-submitted-at request)
                        (or (e-runtime-store-request--first-submitted-at request)
                            now)
                        (e-runtime-store--active-request store) request))
                (puthash (e-runtime-store-request--id request) request
                         (e-runtime-store--pending store))
                (process-send-string (e-runtime-store--process store)
                                     (e-runtime-store--pack-canonical frame)))
            (error
             ;; The close frame may have reached the pipe before this signal.
             ;; Keep its immutable same-ID retirement authority and spend the
             ;; ordinary DP4 recovery attempt; `recover-active' fences the old
             ;; worker before any late response can settle this close.
             (e-runtime-store--recover-or-fail
              store
              (e-runtime-store--request-error
               'e-runtime-store-timeout
               "Worker transport failed after close submission"
               request :cause err))))
        (e-runtime-store--start-process store)
        (e-runtime-store--ensure-worker-open store)))))

(defun e-runtime-store--schedule (store &optional immediate)
  "Arm STORE's single generation-fenced scheduler timer.

IMMEDIATE requests one bounded scheduler turn (or a numeric delay); otherwise
the timer fires at the earliest current phase deadline.  Replacing the timer
increments its generation so stale callbacks are inert."
  (unless (or (e-runtime-store--closed store)
              (e-runtime-store--unavailable store))
    (let ((deadline (and (not immediate)
                         (e-runtime-store--scheduler-deadline store))))
      (when (or immediate deadline)
        (when (timerp (e-runtime-store--scheduler-timer store))
          (cancel-timer (e-runtime-store--scheduler-timer store)))
        (let ((generation (1+ (e-runtime-store--scheduler-generation store))))
          (setf (e-runtime-store--scheduler-generation store) generation
                (e-runtime-store--scheduler-timer store)
                (run-at-time (max 0 (if immediate
                                       (if (numberp immediate) immediate 0)
                                     (- deadline (float-time))))
                             nil #'e-runtime-store--scheduler-fired
                             store generation)))))))

(defun e-runtime-store--scheduler-fired (store generation)
  "Run one bounded scheduler transition for STORE when GENERATION is current."
  (when (and (= generation (e-runtime-store--scheduler-generation store))
             (not (e-runtime-store--closed store))
             (not (e-runtime-store--unavailable store)))
    (setf (e-runtime-store--scheduler-timer store) nil)
    (condition-case err
        (if (e-runtime-store--recovering-request store)
            (cond
             ;; The replacement open is already in flight; leave its response
             ;; or phase deadline to the normal filter/timer boundary.
             ((and (e-runtime-store--active-request store)
                   (eq (e-runtime-store-request--kind
                        (e-runtime-store--active-request store)) 'open)) nil)
             ((e-runtime-store--live-p store)
              ;; Process death releases the cross-process claim.  Polling at
              ;; a bounded cadence is scheduler work, never an awaiter.
              (e-runtime-store--fence-worker store)
              (e-runtime-store--schedule store 0.01))
             (t
              (e-runtime-store--start-process store)
              (e-runtime-store--ensure-worker-open store)))
          (progn
            (e-runtime-store--recover-overdue-active
             store e-runtime-store-request-timeout)
            (let ((more-overdue
                   (e-runtime-store--expire-overdue-queued
                    store e-runtime-store-request-timeout)))
            (cond
             ((e-runtime-store--closing-request store)
              (e-runtime-store--dispatch-close store))
             ((and (not (e-runtime-store--active-request store))
                   (e-runtime-store--next-queued-request store))
              (e-runtime-store--dispatch-next store)))
             (when more-overdue (e-runtime-store--schedule store t)))))
      (error
       ;; Replacement setup is part of the already-owned incident.  Its
       ;; transport or ownership symptom must never displace the cause that
       ;; started the one permitted recovery attempt.
       (if (e-runtime-store--recovering-request store)
           (e-runtime-store--recovery-exhausted store err)
         (e-runtime-store--partition-owner-failure
          store (e-runtime-store--failure-request store) err))))
    (unless (timerp (e-runtime-store--scheduler-timer store))
      (e-runtime-store--schedule store))))

(defun e-runtime-store--dispatch-next (store)
  "Advance one bounded request without waiting for worker open or acknowledgement."
  (unless (or (e-runtime-store--active-request store)
              (e-runtime-store--closed store)
              (e-runtime-store--unavailable store))
    (when-let* ((request (e-runtime-store--next-queued-request store)))
      ;; Keep REQUEST in its queue while starting and opening the worker.  The
      ;; internal open is setup, not a transfer of domain-request ownership.
      (setf (e-runtime-store--starting-request store) request)
      (condition-case err
          (progn
            (unless (eq (e-runtime-store--opened-process store)
                        (e-runtime-store--process store))
              (when (e-runtime-store--borrowed-claim store)
                ;; Borrow authorization travels only over the replacement
                ;; child's private pipe and therefore precedes open preflight.
                (e-runtime-store--start-process store))
              ;; Reject an oversized cold-open envelope before spawning a
              ;; worker for this queued client request.
              (e-runtime-store--prepare-open-control store))
            (e-runtime-store--start-process store)
            (e-runtime-store--ensure-worker-open store)
            (when (eq (e-runtime-store--opened-process store)
                      (e-runtime-store--process store))
              (if (e-runtime-store--queued-request-p store request)
                (progn
                  (e-runtime-store--take-queued-request store request)
                  (let ((now (float-time)))
                    (setf (e-runtime-store--active-request store) request
                          (e-runtime-store-request--state request) 'submitted
                          (e-runtime-store-request--submitted-at request) now
                          (e-runtime-store-request--first-submitted-at request)
                          (or (e-runtime-store-request--first-submitted-at request)
                              now)
                          (e-runtime-store--starting-request store) nil))
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
                (e-runtime-store--schedule store t)))))
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
                (e-runtime-store--partition-owner-failure
                 store failure-request
                 (e-runtime-store--startup-error failure-request err))))))))))

(defun e-runtime-store--submit-owned (store kind body owner-key &optional escrow)
  "Submit KIND BODY with private unencoded OWNER-KEY and optional ESCROW.

OWNER-KEY is nil for ordinary reads and non-optimistic operations, or a
bounded `(DOMAIN . OWNER-ID)' pair supplied by a session/Board adapter.  It is
retained only on the private request and never enters BODY or the worker frame."
  (unless (memq kind '(read write))
    (signal 'wrong-type-argument (list '(member read write) kind)))
  (when (e-runtime-store--closed store)
    (signal 'e-runtime-store-unavailable (list "Store is closed")))
  (when (e-runtime-store--unavailable store)
    (e-runtime-store--signal-unavailable store))
  ;; Once the writer has opened the canonical database, independent reads use
  ;; a subordinate read-only connection.  SQLite WAL, rather than the writer
  ;; transport's private FIFO, then decides concurrency.  Cold reads remain on
  ;; the writer until schema creation or verification has completed.
  (when (and (eq kind 'read)
             (eq (e-runtime-store--access-mode store) 'read-write)
             (eq (e-runtime-store--opened-process store)
                 (e-runtime-store--process store)))
    (setq store (e-runtime-store--read-client-for store)))
  (unless (or (null owner-key)
              (and (eq kind 'write)
                   (consp owner-key)
                   (memq (car owner-key) e-runtime-store-owner-domains)
                   (stringp (cdr owner-key))
                   (<= (string-bytes (cdr owner-key)) 128)))
    (signal 'wrong-type-argument
            (list '(or null (and write (cons (member session board) string)))
                  owner-key)))
  ;; Detach the bounded private identity before frame preflight can reserve any
  ;; request, byte, or notification ownership.  A copy failure is therefore an
  ;; atomic admission rejection rather than a leaked preflight reservation.
  (setq owner-key (e-runtime-store--copy-owner-key owner-key))
  (when (and owner-key
             (hash-table-p (e-runtime-store--suspect-owners store))
             (gethash owner-key (e-runtime-store--suspect-owners store)))
    (signal 'e-runtime-store-persistence-suspect
            (list "Persistence is suspect for this owner"
                  :owner-key (e-runtime-store--copy-owner-key owner-key)
                  :first-diagnostic
                  (copy-sequence
                   (gethash owner-key
                            (e-runtime-store--suspect-owners store))))))
  (let ((request
         (e-runtime-store-request--create
          :id (e-runtime-store--next-id store
                                         (if (eq kind 'write) "w" "r"))
          :kind kind :body body :state 'queued :admitted-at (float-time)
          :owner-store store
          :timeout-interval e-runtime-store-request-timeout
          :write-prefix (and (eq kind 'write)
                             (cl-incf (e-runtime-store--write-prefix-sequence store))))))
    ;; Capture and bound the complete transport frame, including its generated
    ;; correlation id and request envelope, before this request enters either
    ;; scheduler queue.  Later caller mutation cannot change sent bytes.
    (e-runtime-store--preflight-request store request (or escrow t))
    ;; OWNER-KEY is deliberately private and was detached before reservation;
    ;; attach it only after successful protocol preflight.
    (setf (e-runtime-store-request--owner-key request) owner-key)
    (setf (e-runtime-store--client-queue store)
          (nconc (e-runtime-store--client-queue store) (list request)))
    ;; Submission stops at bounded ownership transfer.  The zero-delay event
    ;; below runs only after this caller has its stable private handle.
    (e-runtime-store--schedule store t)
    request))

(defun e-runtime-store-submit (store kind body)
  "Submit typed KIND BODY to STORE and return its request.
Write identity is also the private durable receipt key used for recovery."
  (e-runtime-store--submit-owned store kind body nil))

(defun e-runtime-store--submit-with-frame-escrow (store kind body escrow)
  "Submit KIND BODY by transferring one composition-owned frame ESCROW.

ESCROW is already included in STORE's shared reservation and must dominate the
exact protocol frame.  This private adapter seam is intentionally separate
from the stable public `e-runtime-store-submit' ABI."
  (e-runtime-store--submit-owned store kind body nil escrow))

(defun e-runtime-store-cancel (store request)
  "Cancel REQUEST before submission.
Return `dropped' for queued work, `detached' for submitted reads, and
`in-flight' for submitted writes."
  (setq store (or (e-runtime-store-request--owner-store request) store))
  (pcase (e-runtime-store-request--state request)
    ('queued
     (e-runtime-store--remove-queued-request store request)
     (setf
           (e-runtime-store-request--state request) 'cancelled
           (e-runtime-store-request--error request)
           '(e-runtime-store-cancelled "Cancelled before submission")
           (e-runtime-store-request--frame request) nil)
     (e-runtime-store--record-request-latency store request)
     (e-runtime-store--enqueue-terminal-notification store request)
     (e-runtime-store--schedule store t)
     'dropped)
    ('submitted
     (if (eq (e-runtime-store-request--kind request) 'read)
         (progn
           (setf (e-runtime-store-request--observer-detached request) t
                 (e-runtime-store-request--observer request) nil)
           'detached)
       'in-flight))
    (_ (e-runtime-store-request--state request))))

(defun e-runtime-store--observe (request function)
  "Install FUNCTION as REQUEST's one private terminal observer.

This intentionally lives on the private request handle rather than exposing a
registry.  It is only valid before terminal settlement and is invoked once by
the bounded outbox drain."
  (unless (functionp function)
    (signal 'wrong-type-argument (list 'functionp function)))
  (unless (e-runtime-store--request-live-p request)
    (signal 'e-runtime-store-error (list "Request is already terminal" request)))
  (when (e-runtime-store-request--observer request)
    (signal 'e-runtime-store-error (list "Request already has an observer" request)))
  (setf (e-runtime-store-request--observer request) function)
  request)

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
                   (e-runtime-store--request-deadline
                    active
                    (or (e-runtime-store-request--timeout-interval active)
                        interval)))))
    (let* ((request (if (eq (e-runtime-store-request--kind active) 'open)
                        (e-runtime-store--startup-request store)
                      active))
           (operation (e-runtime-store--request-operation request)))
      (e-runtime-store--recover-or-fail
       store
       (e-runtime-store--request-error
        'e-runtime-store-timeout
        (format "Worker did not acknowledge %s before the request timeout; close, reopen, and reload canonical state"
                (or operation "request"))
        request :awaited-request-id nil)))))

(defun e-runtime-store-await (store request &optional timeout)
  "Observe REQUEST until terminal and return its committed result.

The scheduler is timer/filter driven; this compatibility observer never
opens, dispatches, expires, recovers, cancels, or settles transport work."
  (let ((interval (or timeout e-runtime-store-request-timeout)))
    (while (and (memq (e-runtime-store-request--state request)
                      '(queued submitted))
                (< (float-time)
                   (e-runtime-store--request-deadline request interval)))
      ;; `sit-for' lets process filters and scheduler timers run, but this
      ;; function does not call a scheduler transition itself.
      (sit-for 0.01))
    ;; The observer deadline and scheduler-owned phase deadline may be the
    ;; same instant.  Give an already-due timer one bounded event-loop turn so
    ;; its authoritative terminal state wins that race; this observer still
    ;; never invokes or owns a scheduler transition.
    (when (memq (e-runtime-store-request--state request) '(queued submitted))
      (sit-for 0.001))
    (pcase (e-runtime-store-request--state request)
      ('committed (e-runtime-store-request--result request))
      ('failed (signal (car (e-runtime-store-request--error request))
                       (cdr (e-runtime-store-request--error request))))
      ('cancelled (signal 'e-runtime-store-cancelled (list request)))
      ('queued
       (signal 'e-runtime-store-timeout
               (list "Await observation timed out before scheduler settlement"
                     :request-id (e-runtime-store-request--id request))))
      (_
       (signal 'e-runtime-store-timeout
               (list "Await observation timed out before scheduler settlement"
                     :request-id (e-runtime-store-request--id request)))))))

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

(defun e-runtime-store--read-client-for (store)
  "Return STORE's subordinate read client, replacing a failed client locally."
  (let ((reader (e-runtime-store--read-client store)))
    (when (and reader
               (or (e-runtime-store--closed reader)
                   (e-runtime-store--unavailable reader)))
      (ignore-errors (e-runtime-store-shutdown reader))
      (setq reader nil)
      (setf (e-runtime-store--read-client store) nil))
    (or reader
        (let ((created
               (e-runtime-store-open
                (e-runtime-store--directory store)
                :runtime-id (format "%s-read" (e-runtime-store--runtime-id store))
                :reservation (e-runtime-store--reservation store)
                :access-mode 'read-only
                :parent-store store)))
          (setf (e-runtime-store--read-client store) created)
          created))))

(cl-defun e-runtime-store--open-internal
    (directory &key runtime-id reservation (access-mode 'read-write) parent-store
               borrowed-claim)
  "Create STORE, optionally beneath private BORROWED-CLAIM, and begin opening.

This returns before the worker's open acknowledgement.  Submission never
waits for or advances that phase; an entirely cold test/store can equivalently
be constructed with the private constructor used by scheduler tests."
  (unless (memq access-mode '(read-write read-only))
    (signal 'wrong-type-argument
            (list '(member read-write read-only) access-mode)))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (database-file (expand-file-name "store.sqlite3" directory))
         (store (e-runtime-store--create
                 :directory directory
                 :database-file database-file
                 :access-mode access-mode
                 :parent-store parent-store
                 :borrowed-claim borrowed-claim
                 :runtime-id (or runtime-id
                                 (format "%x-%x-%x" (emacs-pid)
                                         (truncate (* 1000000 (float-time)))
                                         (random most-positive-fixnum)))
                 :pending (make-hash-table :test 'equal)
                 :reservation (or reservation e-runtime-store--default-reservation))))
    (when (eq access-mode 'read-write)
      (e-runtime-store--reject-legacy-only-directory directory database-file)
      (make-directory directory t))
    (when (and (eq access-mode 'read-only)
               (not (file-exists-p database-file)))
      (signal 'e-runtime-store-unavailable
              (list "Read client requires an initialized SQLite store"
                    :database-file database-file)))
    (condition-case err
        (progn
          (if borrowed-claim
              ;; The one-use authorization must enter the selected child's
              ;; private pipe, so this explicit offline boundary starts before
              ;; open-frame preflight.
              (progn
                (e-runtime-store--start-process store)
                (e-runtime-store--prepare-open-control store))
            ;; Ordinary open preflight remains before process creation: a bad
            ;; expanded identity frame has no transport side effect to fence.
            (e-runtime-store--prepare-open-control store)
            (e-runtime-store--start-process store))
          (e-runtime-store--ensure-worker-open store)
          (e-runtime-store--schedule store)
          store)
      (error
       (when-let* ((control (e-runtime-store--open-control-request store)))
         (e-runtime-store--release-open-control store control))
       (setf (e-runtime-store--closed store) t)
       (when (timerp (e-runtime-store--scheduler-timer store))
         (cancel-timer (e-runtime-store--scheduler-timer store)))
       (when (timerp (e-runtime-store--notification-timer store))
         (cancel-timer (e-runtime-store--notification-timer store)))
       (when (processp (e-runtime-store--process store))
         (delete-process (e-runtime-store--process store)))
       (e-runtime-store--clear-stderr store)
       ;; Public-open setup is all-or-nothing.  A caller that catches this
       ;; error must not retain stale process/timer/control ownership which
       ;; could make a later same-directory open look live.
       (setf (e-runtime-store--process store) nil
             (e-runtime-store--opened-process store) nil
             (e-runtime-store--stderr-buffer store) nil
             (e-runtime-store--stderr-process store) nil
             (e-runtime-store--stderr-bytes store) 0
             (e-runtime-store--stderr-truncated store) nil
             (e-runtime-store--input-fragment store) nil
             (e-runtime-store--active-request store) nil
             (e-runtime-store--starting-request store) nil
             (e-runtime-store--recovering-request store) nil
             (e-runtime-store--recovery-cause store) nil
             (e-runtime-store--scheduler-timer store) nil
             (e-runtime-store--notification-timer store) nil)
       (signal (car err) (cdr err))))))

(cl-defun e-runtime-store-open
    (directory &key runtime-id reservation (access-mode 'read-write) parent-store)
  "Create STORE and begin its asynchronous cold-open phase.

This public entry point cannot inject or borrow runtime-store ownership."
  (e-runtime-store--open-internal
   directory :runtime-id runtime-id :reservation reservation
   :access-mode access-mode :parent-store parent-store))

(cl-defun e-runtime-store-open-under-offline-claim
    (directory claim &key runtime-id reservation)
  "Open an ordinary worker beneath exact offline ownership CLAIM.

CLAIM remains owned by the caller and must outlive the returned store.  This
entry point exists only for explicit offline operators which must use ordinary
application services without an ownership gap; interactive runtimes use
`e-runtime-store-open'."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (database-file (expand-file-name "store.sqlite3" directory))
         (claim-file
          (and (e-runtime-store-ownership-claim-p claim)
               (e-runtime-store-ownership-claim--database-file claim))))
    (unless (and claim-file
                 (equal (expand-file-name claim-file) database-file))
      (signal 'e-runtime-store-owner-identity-conflict
              (list "Offline ownership claim targets a different store"
                    :database-file database-file)))
    (e-runtime-store--open-internal
     directory :runtime-id runtime-id :reservation reservation
     :access-mode 'read-write
     :borrowed-claim claim)))

(defun e-runtime-store-close (store)
  "Compatibility observer for STORE's private asynchronous close request."
  (when-let* ((reader (e-runtime-store--read-client store)))
    ;; A read-only connection owns no durable retirement state.  Tear it down
    ;; locally before the writer's explicit retirement boundary.
    (e-runtime-store-shutdown reader)
    (setf (e-runtime-store--read-client store) nil))
  (unless (e-runtime-store--closed store)
    (let ((busy (e-runtime-store--active-request store))
          (request (and (not (e-runtime-store--unavailable store))
                        (e-runtime-store--close-start store)))
          close-error)
      (cond
       ;; Preserve old synchronous busy-close behavior, but only after its
       ;; client-visible close request successfully reserved capacity.
       ((and request busy)
        (e-runtime-store--finalize-close store))
       (request
          (condition-case err
              (e-runtime-store-await store request)
            (error (setq close-error err))))
       (t
        ;; Busy/unavailable compatibility close retains its established local
        ;; settlement meaning; it has no additional client handle to admit.
        (e-runtime-store--finalize-close store)))
      (when (timerp (e-runtime-store--close-finalizer-timer store))
        (e-runtime-store--finalize-close store))
      (when close-error
        (signal 'e-runtime-store-unavailable
                (list "Runtime retirement recovery was exhausted"
                      :cause close-error)))))
  t)

(defun e-runtime-store-shutdown (store)
  "Release STORE locally at process shutdown without worker coordination.

This is the named process-exit boundary.  It submits no close request, starts
no worker or recovery, and never waits for SQLite.  Durable work already
acknowledged by SQLite remains durable; queued or in-flight work is failed
locally because Emacs is exiting and cannot observe later completion.  Use
`e-runtime-store-close' only at explicit graceful operator or test boundaries
that require worker retirement acknowledgement."
  (when-let* ((reader (e-runtime-store--read-client store)))
    (e-runtime-store-shutdown reader)
    (setf (e-runtime-store--read-client store) nil))
  (unless (e-runtime-store--closed store)
    (e-runtime-store--finalize-close store))
  t)

(defun e-runtime-store-status (store)
  "Return bounded liveness, failure, and latency status for STORE.

The `:latencies' entry is process-local and never queries SQLite.  It reports
the active and oldest queued phase ages plus at most 64 newest-first terminal
samples separating queue, dispatch-to-settlement, and total milliseconds."
  (let ((active (e-runtime-store--active-request store))
        (now (float-time)))
    (list :database-file (e-runtime-store--database-file store)
          :runtime-id (e-runtime-store--runtime-id store)
          :access-mode (e-runtime-store--access-mode store)
          :worker-live (and (e-runtime-store--live-p store) t)
          :worker-pid (and (e-runtime-store--live-p store)
                           (process-id (e-runtime-store--process store)))
          :pending-count (+ (length (e-runtime-store--client-queue store))
                            (if active 1 0))
          :oldest-age (and active (e-runtime-store-request--submitted-at active)
                           (- now
                              (e-runtime-store-request--submitted-at active)))
          :active-request-kind
          (and active (e-runtime-store-request--kind active))
          :active-request-operation
          (e-runtime-store--request-operation active)
          :latencies (e-runtime-store--latency-status store active now)
          :unavailable (and (e-runtime-store--unavailable store) t)
          :unavailable-cause (e-runtime-store--unavailable-cause store)
          :suspect-owners
          (and (hash-table-p (e-runtime-store--suspect-owners store))
               (let (owners)
                 (maphash (lambda (key diagnostic)
                            (push (list :owner-key
                                        (e-runtime-store--copy-owner-key key)
                                        :first-diagnostic
                                        (copy-sequence diagnostic))
                                  owners))
                          (e-runtime-store--suspect-owners store))
                 (nreverse owners)))
          :last-error (e-runtime-store--last-error store)
          :startup (e-runtime-store--startup-status store)
          :read-transport
          (when-let* ((reader (e-runtime-store--read-client store)))
            (e-runtime-store-status reader)))))

(provide 'e-runtime-store)

;;; e-runtime-store.el ends here
