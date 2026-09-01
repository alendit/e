;;; e-runtime-store.el --- Typed subordinate runtime-store adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns one batch-Emacs worker, typed request framing, bounded scheduling,
;; restart/reconciliation, liveness, and runtime ownership.  This module knows
;; no session, Board, resource, or tool policy and never opens SQLite itself.

;;; Code:

(require 'cl-lib)
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
(define-error 'e-runtime-store-command-conflict "Runtime store command conflicts"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-revision-conflict "Runtime store revision conflicts"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-board-conflict "Runtime store Board conflict"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-resource-too-large "Runtime store resource is too large"
  'e-runtime-store-worker-error)

(defcustom e-runtime-store-request-timeout 60.0
  "Maximum seconds for one bounded runtime-store request."
  :type 'number :group 'e)

(defcustom e-runtime-store-restart-limit 3
  "Maximum consecutive worker restarts before durable effects freeze."
  :type 'integer :group 'e)

(cl-defstruct (e-runtime-store
               (:constructor e-runtime-store--create)
               (:predicate e-runtime-store-p)
               (:conc-name e-runtime-store--))
  directory database-file runtime-id process opened-process stderr-buffer input-fragment
  (sequence 0) pending write-queue read-queue active-request
  (restart-count 0) recovery-timer last-error startup-status closed)

(cl-defstruct (e-runtime-store-request
               (:constructor e-runtime-store-request--create)
               (:predicate e-runtime-store-request-p)
               (:conc-name e-runtime-store-request--))
  id kind body hash state result error on-done on-error submitted-at)

(defun e-runtime-store--worker-file ()
  "Return the installed worker source or byte-code path."
  (or (locate-library "e-runtime-store-worker")
      (signal 'e-runtime-store-error (list "Worker module is missing"))))

(defun e-runtime-store--emacs-program ()
  "Return the current Emacs executable path."
  (expand-file-name invocation-name invocation-directory))

(defun e-runtime-store--command ()
  "Return the private batch worker command."
  (list (e-runtime-store--emacs-program) "--batch" "-Q"
        "-L" (file-name-directory (e-runtime-store--worker-file))
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
       (<= (e-runtime-store--restart-count store)
           e-runtime-store-restart-limit)
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
  (if (plist-get response :ok)
      (progn
        (setf (e-runtime-store-request--state request) 'committed
              (e-runtime-store-request--result request)
              (plist-get response :result)
              (e-runtime-store--last-error store) nil)
        ;; Opening a replacement worker does not prove that the uncertain
        ;; domain request which killed its predecessor can make progress.
        ;; Reset the consecutive-failure bound only after real work settles.
        (unless (eq (e-runtime-store-request--kind request) 'open)
          (setf (e-runtime-store--restart-count store) 0))
        (when (e-runtime-store-request--on-done request)
          (funcall (e-runtime-store-request--on-done request)
                   (e-runtime-store-request--result request))))
    (let ((err (condition-case caught
                   (e-runtime-store--signal-response-error response)
                 (error caught))))
      (setf (e-runtime-store-request--state request) 'failed
            (e-runtime-store-request--error request) err
            (e-runtime-store--last-error store) err)
      (when (e-runtime-store-request--on-error request)
        (funcall (e-runtime-store-request--on-error request) err))))
  ;; An internal open is only a transport prerequisite.  The caller which is
  ;; establishing that transport dispatches its domain request after the open
  ;; acknowledgement, so do not let a nested callback overtake it.
  (unless (eq (e-runtime-store-request--kind request) 'open)
    (e-runtime-store--dispatch-next store)))

(defun e-runtime-store--consume-output (store text)
  "Consume worker protocol TEXT for STORE."
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
             ;; A malformed response leaves transport position uncertain.  A
             ;; write may already be committed, so restart and reconcile it
             ;; by stable identity instead of failing or waiting indefinitely.
             (when (e-runtime-store--live-p store)
               (delete-process (e-runtime-store--process store)))))))))

(defun e-runtime-store--requeue-active (store)
  "Requeue STORE's uncertain active write for idempotent reconciliation."
  (when-let* ((request (e-runtime-store--active-request store)))
    (setf (e-runtime-store--active-request store) nil
          (e-runtime-store--opened-process store) nil)
    (if (eq (e-runtime-store-request--kind request) 'open)
        ;; Open requests are process-local protocol handshakes.  Never enqueue
        ;; one as domain work: its ordinary request frame lacks the directory
        ;; and runtime identity required by the worker's open operation.
        (progn
          (remhash (e-runtime-store-request--id request)
                   (e-runtime-store--pending store))
          (setf (e-runtime-store-request--state request) 'failed
                (e-runtime-store-request--error request)
                '(e-runtime-store-unavailable "Worker exited during open")))
      (setf (e-runtime-store-request--state request) 'reconciling)
      (if (eq (e-runtime-store-request--kind request) 'write)
          (push request (e-runtime-store--write-queue store))
        (push request (e-runtime-store--read-queue store))))))

(defun e-runtime-store--schedule-recovery (store)
  "Schedule one automatic bounded recovery attempt for STORE."
  (unless (or (e-runtime-store--closed store)
              (timerp (e-runtime-store--recovery-timer store)))
    (setf (e-runtime-store--recovery-timer store)
          (run-at-time
           0 nil
           (lambda ()
             (setf (e-runtime-store--recovery-timer store) nil)
             (unless (e-runtime-store--closed store)
               (e-runtime-store--dispatch-next store)))))))

(defun e-runtime-store--worker-exited (store)
  "Record worker loss for STORE without publishing new durable effects."
  (unless (e-runtime-store--closed store)
    (cl-incf (e-runtime-store--restart-count store))
    (e-runtime-store--requeue-active store)
    (unless (e-runtime-store--last-error store)
      (setf (e-runtime-store--last-error store)
            (list 'e-runtime-store-unavailable
                  (format "Worker exited; reconciliation attempt %d"
                          (e-runtime-store--restart-count store)))))
    (e-runtime-store--schedule-recovery store)))

(defun e-runtime-store--start-process (store)
  "Start STORE's worker process and return it."
  (when (e-runtime-store--closed store)
    (signal 'e-runtime-store-unavailable (list "Store is closed")))
  (when (> (e-runtime-store--restart-count store) e-runtime-store-restart-limit)
    (signal 'e-runtime-store-unavailable
            (list "Worker restart bound exhausted"
                  (e-runtime-store--restart-count store))))
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
  "Return next stable STORE command id with PREFIX."
  (format "%s:%s:%d" (e-runtime-store--runtime-id store) prefix
          (cl-incf (e-runtime-store--sequence store))))

(defun e-runtime-store--request-frame (request)
  "Return protocol frame for REQUEST."
  (append
   (list :id (e-runtime-store-request--id request)
         :kind (e-runtime-store-request--kind request)
         :body (e-runtime-store-request--body request))
   (when (e-runtime-store-request--hash request)
     (list :hash (e-runtime-store-request--hash request)))))

(defun e-runtime-store--fail-request (store request err)
  "Settle REQUEST on STORE with ERR exactly once."
  (remhash (e-runtime-store-request--id request)
           (e-runtime-store--pending store))
  (setf (e-runtime-store-request--state request) 'failed
        (e-runtime-store-request--error request) err
        (e-runtime-store--last-error store) err)
  (when (e-runtime-store-request--on-error request)
    (funcall (e-runtime-store-request--on-error request) err)))

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
         (if (and (not (e-runtime-store--closed store))
                  (<= (e-runtime-store--restart-count store)
                      e-runtime-store-restart-limit))
             (progn
               (setf (e-runtime-store-request--state request) 'reconciling)
               (if (eq (e-runtime-store-request--kind request) 'write)
                   (push request (e-runtime-store--write-queue store))
                 (push request (e-runtime-store--read-queue store)))
               (e-runtime-store--schedule-recovery store))
           (e-runtime-store--fail-request store request err)))))))

(cl-defun e-runtime-store-submit
    (store kind body &key id on-done on-error)
  "Submit typed KIND BODY to STORE and return its request.
Writes use stable command ID and canonical BODY hash."
  (unless (memq kind '(read write))
    (signal 'wrong-type-argument (list '(member read write) kind)))
  ;; Encoding is the producer-side validation boundary.  Reject unsupported or
  ;; cyclic values before the request can become a submitted durable effect.
  (let* ((bytes (e-runtime-store-codec-encode body))
         (request
          (e-runtime-store-request--create
           :id (or id (e-runtime-store--next-id store
                                                (if (eq kind 'write) "w" "r")))
           :kind kind :body body
           :hash (and (eq kind 'write) (secure-hash 'sha256 bytes))
           :state 'queued :on-done on-done :on-error on-error)))
    (if (eq kind 'write)
        (setf (e-runtime-store--write-queue store)
              (nconc (e-runtime-store--write-queue store) (list request)))
      (setf (e-runtime-store--read-queue store)
            (nconc (e-runtime-store--read-queue store) (list request))))
    (e-runtime-store--dispatch-next store)
    request))

(defun e-runtime-store-cancel (store request)
  "Cancel REQUEST before submission, or reconcile it after submission.
Return `dropped' for provisional work and `reconcile' once transport began."
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
    ((or 'submitted 'reconciling) 'reconcile)
    (_ (e-runtime-store-request--state request))))

(defun e-runtime-store-await (store request &optional timeout)
  "Wait cooperatively for REQUEST and return its committed result."
  (let ((deadline (+ (float-time) (or timeout e-runtime-store-request-timeout))))
    (while (and (memq (e-runtime-store-request--state request)
                      '(queued submitted reconciling))
                (< (float-time) deadline))
      (unless (e-runtime-store--live-p store)
        (when (eq (e-runtime-store-request--state request) 'submitted)
          (setf (e-runtime-store-request--state request) 'reconciling))
        (condition-case err
            (progn
              (e-runtime-store--start-process store)
              (e-runtime-store--dispatch-next store))
          (error
           (setf (e-runtime-store--last-error store) err))))
      (accept-process-output (e-runtime-store--process store) 0.01))
    (pcase (e-runtime-store-request--state request)
      ('committed (e-runtime-store-request--result request))
      ('failed (signal (car (e-runtime-store-request--error request))
                       (cdr (e-runtime-store-request--error request))))
      ('cancelled (signal 'e-runtime-store-cancelled (list request)))
      (_ (signal 'e-runtime-store-timeout
                 (list (e-runtime-store-request--id request)
                       (e-runtime-store-status store)))))))

(defun e-runtime-store-call (store kind body &optional id)
  "Submit KIND BODY to STORE and return its bounded result."
  (e-runtime-store-await store
                         (e-runtime-store-submit store kind body :id id)))

(cl-defun e-runtime-store-open (directory &key runtime-id)
  "Open one subordinate runtime store for DIRECTORY."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (store (e-runtime-store--create
                 :directory directory
                 :database-file (expand-file-name "store.sqlite3" directory)
                 :runtime-id (or runtime-id
                                 (format "%x-%x-%x" (emacs-pid)
                                         (truncate (* 1000000 (float-time)))
                                         (random most-positive-fixnum)))
                 :pending (make-hash-table :test 'equal))))
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
  "Close STORE and its worker without affecting another runtime."
  (setf (e-runtime-store--closed store) t)
  (when (timerp (e-runtime-store--recovery-timer store))
    (cancel-timer (e-runtime-store--recovery-timer store))
    (setf (e-runtime-store--recovery-timer store) nil))
  (when (e-runtime-store--live-p store)
    (process-send-eof (e-runtime-store--process store))
    (accept-process-output (e-runtime-store--process store) 0.2))
  (when (processp (e-runtime-store--process store))
    (delete-process (e-runtime-store--process store)))
  (when (buffer-live-p (e-runtime-store--stderr-buffer store))
    (kill-buffer (e-runtime-store--stderr-buffer store)))
  t)

(defun e-runtime-store-status (store)
  "Return bounded liveness and reconciliation status for STORE."
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
          :active-command (and active (e-runtime-store-request--id active))
          :reconciliation (and active
                               (eq (e-runtime-store-request--state active)
                                   'reconciling))
          :restart-count (e-runtime-store--restart-count store)
          :frozen (> (e-runtime-store--restart-count store)
                     e-runtime-store-restart-limit)
          :last-error (e-runtime-store--last-error store)
          :startup (e-runtime-store--startup-status store))))

(provide 'e-runtime-store)

;;; e-runtime-store.el ends here
