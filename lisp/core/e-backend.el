;;; e-backend.el --- Backend contract for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral backend adapter contract for the core runtime.

;;; Code:

(require 'cl-lib)
(require 'e-request)
(require 'seq)

(cl-defstruct (e-backend-request (:constructor e-backend-request-create))
  cancel
  metadata)

(cl-defstruct (e-backend-provider-compaction-result
               (:constructor e-backend-provider-compaction-result-create)
               (:conc-name e-backend-provider-compaction-result-))
  "Provider-owned opaque checkpoint output at a portable boundary.

The core deliberately stores no provider state of this type.  The result is
validated only at the adapter boundary so malformed provider responses fail
before a runtime candidate can be installed."
  output
  usage)

(cl-defstruct (e-backend
               (:constructor e-backend-create)
               (:conc-name e-backend--))
  name
  stream
  start
  provider-compaction
  normalize-error-details
  context-capabilities)

(defconst e-backend-context-capability-values
  '((:continuation none linear branchable)
    ;; A scalar remains accepted for old adapters.  New adapters may return a
    ;; list of `(:kind KIND :mode MODE)' entries; the helper below gives core a
    ;; single kind-scoped query without making provider wire fields part of the
    ;; contract.
    (:observation-delivery inherited request-local-replaceable)
    (:prefix-cache none implicit explicit)
    (:provider-compaction none opaque)
    (:reasoning-state none replayable)
    ;; `context-promote-wire' remains accepted for old adapters while the
    ;; runtime switches to the replacement curation carrier.  New providers
    ;; must advertise only `context-curate-wire'.
    (:reserved-effect-carrier none context-promote-wire context-curate-wire))
  "Provider-neutral values accepted by `e-backend-context-capabilities'.")

(defconst e-backend--default-context-capabilities
  '(:continuation none
    :observation-delivery inherited
    :prefix-cache none
    :provider-compaction none
    :reasoning-state none)
  "Conservative capabilities used when an adapter declares none.")

(defun e-backend-default-context-capabilities ()
  "Return a fresh copy of the conservative context capability defaults."
  (copy-sequence e-backend--default-context-capabilities))

(define-error 'e-backend-invalid-context-capabilities
  "Invalid provider-neutral backend context capabilities")

(define-error 'e-backend-invalid-provider-compaction-result
  "Invalid opaque provider compaction result")

(define-error 'e-backend-provider-compaction-unavailable
  "Opaque provider compaction is unavailable")

(defun e-backend--keyword-plist-p (value)
  "Return non-nil when VALUE is a proper keyword plist."
  (and (listp value)
       (proper-list-p value)
       (let ((rest value)
             (valid t))
         (while (and valid rest)
           (if (and (consp rest)
                    (keywordp (car rest))
                    (consp (cdr rest)))
               (setq rest (cddr rest))
             (setq valid nil)))
         (and valid (null rest)))))

(defconst e-backend-observation-kinds
  '(current-state dynamic-context tool-result trace retrieved-excerpt)
  "Semantic observation kinds understood by the generic backend contract.")

(defun e-backend--observation-delivery-entry (entry)
  "Return normalized delivery ENTRY or signal an invalid capability."
  (unless (and (e-backend--keyword-plist-p entry)
               (= (length entry) 4)
               (plist-member entry :kind)
               (plist-member entry :mode))
    (signal 'e-backend-invalid-context-capabilities
            (list :observation-delivery entry)))
  (let ((kind (plist-get entry :kind))
        (mode (plist-get entry :mode)))
    (unless (and (memq kind e-backend-observation-kinds)
                 (memq mode '(inherited request-local-replaceable)))
      (signal 'e-backend-invalid-context-capabilities
              (list :observation-delivery entry)))
    (list :kind kind :mode mode)))

(defun e-backend--normalize-observation-delivery (value)
  "Return scalar or canonical kind-scoped observation delivery VALUE."
  (cond
   ((memq value '(inherited request-local-replaceable)) value)
   ((and (proper-list-p value)
         (not (e-backend--keyword-plist-p value)))
    (let (entries kinds)
      (dolist (entry value)
        (let ((normalized (e-backend--observation-delivery-entry entry)))
          (when (memq (plist-get normalized :kind) kinds)
            (signal 'e-backend-invalid-context-capabilities
                    (list :observation-delivery :duplicate-kind
                          (plist-get normalized :kind))))
          (push (plist-get normalized :kind) kinds)
          (push normalized entries)))
      (nreverse entries)))
   (t
    (signal 'e-backend-invalid-context-capabilities
            (list :observation-delivery value)))))

(defun e-backend-observation-delivery-for-kind (capabilities kind)
  "Return effective delivery for semantic observation KIND.

CAPABILITIES may use the legacy scalar observation value or the preferred
kind-scoped mapping.  Missing mapping entries are conservative inherited
observations."
  (let ((kind (if (symbolp kind) kind (intern (format "%s" kind))))
        (value (plist-get capabilities :observation-delivery)))
    (cond
     ((eq value 'inherited) 'inherited)
     ((eq value 'request-local-replaceable)
      ;; Legacy scalar claims describe the existing current-state channel only;
      ;; they never silently authorize dropping tool results or traces.
      (if (memq kind '(current-state dynamic-context))
          value
        'inherited))
     ((and (listp value) (not (e-backend--keyword-plist-p value)))
      (or (plist-get (seq-find
                      (lambda (entry)
                        (eq (plist-get entry :kind) kind))
                      value)
                     :mode)
          'inherited))
     (t 'inherited))))

(defun e-backend--validate-context-capabilities (capabilities)
  "Return normalized CAPABILITIES or signal for an unknown semantic value."
  (unless (e-backend--keyword-plist-p capabilities)
    (signal 'e-backend-invalid-context-capabilities
            (list :not-a-plist capabilities)))
  (let ((normalized (copy-sequence e-backend--default-context-capabilities)))
    (dolist (descriptor e-backend-context-capability-values)
      (let* ((key (car descriptor))
             (allowed (cdr descriptor)))
        (when (plist-member capabilities key)
          (let ((value (plist-get capabilities key)))
            (setq value
                  (if (eq key :observation-delivery)
                      (e-backend--normalize-observation-delivery value)
                    value))
            (unless (or (and (eq key :observation-delivery)
                             (listp value))
                        (memq value allowed))
              (signal 'e-backend-invalid-context-capabilities
                      (list key value allowed)))
            (setq normalized (plist-put normalized key value))))))
    ;; Provider-owned metadata is not part of the semantic contract.  Reject a
    ;; misspelled semantic key rather than silently selecting a fallback.
    (let ((rest capabilities))
      (while rest
        (let ((key (pop rest)))
          (pop rest)
          (unless (assq key e-backend-context-capability-values)
            (signal 'e-backend-invalid-context-capabilities
                    (list :unknown-key key)))))
    normalized)))

(defun e-backend-context-capabilities (backend options)
  "Return BACKEND's provider-neutral context capabilities for OPTIONS.

The optional backend slot may be a static plist or a function receiving the
effective backend-neutral OPTIONS.  An adapter that declares no capabilities
gets the conservative stateless defaults.  Core policy sees only semantic
values and never provider wire field names."
  (unless (e-backend-p backend)
    (signal 'wrong-type-argument (list 'e-backend-p backend)))
  (let* ((declaration (and (>= (length backend) 6)
                           (e-backend--context-capabilities backend)))
         (capabilities (cond
                        ((functionp declaration)
                         (funcall declaration options))
                        ((null declaration)
                         (e-backend-default-context-capabilities))
                        (t declaration))))
    (setq capabilities (or capabilities
                           (e-backend-default-context-capabilities)))
    (setq capabilities
          (e-backend--validate-context-capabilities capabilities))
    (when (and (eq (plist-get capabilities :provider-compaction) 'opaque)
               (not (functionp (e-backend--provider-compaction backend))))
      (signal 'e-backend-invalid-context-capabilities
              (list :provider-compaction 'opaque :missing-operation)))
    capabilities))

(defconst e-backend--provider-compaction-usage-keys
  '(:input-tokens :output-tokens :total-tokens)
  "Bounded usage keys accepted in an opaque compaction result.")

(defun e-backend--provider-compaction-usage (usage)
  "Return normalized bounded USAGE or signal for malformed usage.

Adapters may use provider-specific response fields internally, but the generic
result contract exposes only these optional non-negative counters."
  (when usage
    (unless (e-backend--keyword-plist-p usage)
      (signal 'e-backend-invalid-provider-compaction-result
              (list :usage usage)))
    (let ((rest usage)
          (copy (copy-sequence usage)))
      (while rest
        (let ((key (pop rest))
              (value (pop rest)))
          (unless (memq key e-backend--provider-compaction-usage-keys)
            (signal 'e-backend-invalid-provider-compaction-result
                    (list :usage-key key)))
          (unless (and (integerp value) (>= value 0))
            (signal 'e-backend-invalid-provider-compaction-result
                    (list :usage key value)))))
      copy)))

(defun e-backend--copy-provider-compaction-value (value)
  "Deep-copy JSON-shaped opaque provider VALUE, preserving vectors.

`copy-tree' does not detach plist elements nested inside vectors.  Provider
compaction output remains opaque to core policy, but the generic boundary must
still ensure that later adapter mutation cannot change the installed runtime
candidate."
  (cond
   ((vectorp value)
    (let ((copy (copy-sequence value)))
      (cl-loop for index below (length copy) do
        (aset copy index
              (e-backend--copy-provider-compaction-value
               (aref value index)))
        finally return copy)))
   ((consp value)
    (cons (e-backend--copy-provider-compaction-value (car value))
          (e-backend--copy-provider-compaction-value (cdr value))))
   ((stringp value)
    (copy-sequence value))
   (t value)))

(defun e-backend-provider-compaction-result (value)
  "Validate and normalize opaque provider compaction VALUE.

VALUE is either an `e-backend-provider-compaction-result' or the narrow
adapter-facing plist `(:output OUTPUT :usage USAGE)'.  OUTPUT is intentionally
opaque to core policy, but it must be a detached JSON-array-shaped sequence so
the adapter cannot accidentally report a scalar/error object as usable state."
  (let (output usage)
    (cond
     ((e-backend-provider-compaction-result-p value)
      (setq output (e-backend-provider-compaction-result-output value)
            usage (e-backend-provider-compaction-result-usage value)))
     ((and (e-backend--keyword-plist-p value)
           (= (length value) 4)
           (plist-member value :output)
           (plist-member value :usage))
      (setq output (plist-get value :output)
            usage (plist-get value :usage)))
     (t
      (signal 'e-backend-invalid-provider-compaction-result
              (list :result value))))
    (unless (or (listp output) (vectorp output))
      (signal 'e-backend-invalid-provider-compaction-result
              (list :output output)))
    (e-backend-provider-compaction-result-create
     :output (e-backend--copy-provider-compaction-value output)
     :usage (e-backend--provider-compaction-usage usage))))

(cl-defun e-backend-provider-compaction-batch
    (backend &key messages options)
  "Run BACKEND's optional opaque compaction operation synchronously.

The operation receives provider-neutral portable MESSAGES and OPTIONS and
returns the narrow result accepted by
`e-backend-provider-compaction-result'.  This is an acceleration path: callers
must already have committed the canonical portable generation boundary."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-backend-provider-compaction-batch))
  (let ((operation (and (e-backend-p backend)
                        (e-backend--provider-compaction backend))))
    (unless (functionp operation)
      (signal 'e-backend-provider-compaction-unavailable
              (list (and (e-backend-p backend)
                         (e-backend--name backend)))))
    (let ((result (funcall operation :messages messages :options options)))
      (when (e-backend-request-p result)
        (signal 'e-backend-invalid-provider-compaction-result
                (list :async-result-in-batch result)))
      (e-backend-provider-compaction-result result))))

(cl-defun e-backend-provider-compaction-start
    (backend &key messages options on-done on-error)
  "Start BACKEND's optional opaque compaction operation asynchronously.

The adapter calls ON-DONE with its result or ON-ERROR with a condition.  A
small synchronous adapter may return a result directly; this wrapper then
settles ON-DONE, preserving one result-validation boundary for both forms."
  (let ((operation (and (e-backend-p backend)
                        (e-backend--provider-compaction backend))))
    (unless (functionp operation)
      (signal 'e-backend-provider-compaction-unavailable
              (list (and (e-backend-p backend)
                         (e-backend--name backend)))))
    (let ((settled nil)
          request)
      (cl-labels
          ((done (value)
             (unless settled
               (setq settled t)
               (condition-case err
                   (when on-done
                     (funcall on-done
                              (e-backend-provider-compaction-result value)))
                 (error
                  (if on-error
                      (funcall on-error err)
                    (signal (car err) (cdr err)))))))
           (failed (err)
             (unless settled
               (setq settled t)
               (if on-error
                   (funcall on-error err)
                 (signal (car err) (cdr err))))))
        (condition-case err
            (setq request
                  (funcall operation
                           :messages messages
                           :options options
                           :on-done #'done
                           :on-error #'failed))
          (error (failed err)))
        (when (and request
                   (not (e-backend-request-p request))
                   (not settled))
          (done request))
        request))))

(defvar e-backend--request-start-callback nil
  "Dynamically scoped callback for backend request handles.")

(defun e-backend-normalize-error-details (backend message details condition)
  "Return BACKEND-normalized error DETAILS.
MESSAGE is the compact backend error text and CONDITION is the original Emacs
condition.  Adapters use this boundary to add provider-owned retry metadata
such as `:retryable', `:retry-reason', and `:retry-after-seconds'.  Backends
without a normalizer preserve DETAILS unchanged."
  (let ((normalizer (and (e-backend-p backend)
                         (e-backend--normalize-error-details backend))))
    (cond
     ;; Stream items cross the adapter boundary with normalized details
     ;; already.  Trust that explicit decision instead of reclassifying it
     ;; after the loop has wrapped the original provider condition.
     ((and (listp details) (plist-member details :retryable)) details)
     ((functionp normalizer)
      (funcall normalizer message details condition))
     (t details))))

(defun e-backend-note-request-started (request)
  "Publish REQUEST as the active provider request for the current stream."
  (when e-backend--request-start-callback
    (funcall e-backend--request-start-callback request))
  request)

(defun e-backend-cancel-request (request)
  "Cancel REQUEST when it has a provider cancellation function."
  (when-let ((cancel (and (e-backend-request-p request)
                          (e-backend-request-cancel request))))
    (funcall cancel)))

(cl-defun e-backend-stream-batch
    (backend &key messages options on-item on-request-start)
  "Synchronously stream a backend turn through BACKEND from batch/test code.
MESSAGES and OPTIONS are backend-neutral plists/lists.  ON-ITEM receives
backend-neutral stream items.  ON-REQUEST-START receives an optional
`e-backend-request' handle when the adapter can expose request state."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-backend-stream-batch))
  (cond
   ((functionp (e-backend--stream backend))
    (let ((e-backend--request-start-callback on-request-start))
      (funcall (e-backend--stream backend)
               :messages messages
               :options options
               :on-item on-item)))
   ((functionp (e-backend--start backend))
    (let ((done nil)
          (result nil)
          (failure nil))
      (e-backend-start
       backend
       :messages messages
       :options options
       :on-item on-item
       :on-done (lambda (value)
                  (setq result value)
                  (setq done t))
       :on-error (lambda (err)
                   (setq failure err)
                   (setq done t))
       :on-request-start on-request-start)
      (while (not done)
        (accept-process-output nil 0.01))
      (when failure
        (signal (car failure) (cdr failure)))
      result))
   (t
    (signal 'wrong-type-argument
            (list 'functionp (e-backend--stream backend))))))

(cl-defun e-backend-start
    (backend &key messages options on-item on-done on-error on-request-start)
  "Start a backend turn through BACKEND without blocking.
MESSAGES and OPTIONS are backend-neutral inputs.  ON-ITEM receives stream
items.  ON-DONE receives a result plist after the request completes.  ON-ERROR
receives an Emacs condition list.  ON-REQUEST-START receives an optional
`e-backend-request' handle.  Return the request handle when available."
  (cond
   ((functionp (e-backend--start backend))
    (funcall (e-backend--start backend)
             :messages messages
             :options options
             :on-item on-item
             :on-done on-done
             :on-error on-error
             :on-request-start on-request-start))
   ((functionp (e-backend--stream backend))
    (let ((cancelled nil)
          (timer nil)
          request)
      (setq request
            (e-backend-request-create
             :cancel (lambda ()
                       (setq cancelled t)
                       (when (timerp timer)
                         (cancel-timer timer))
                       t)
             :metadata '(:transport timer :cancellable queued-only)))
      (when on-request-start
        (funcall on-request-start request))
      (setq timer
            (run-at-time
             0 nil
             (lambda ()
               (unless cancelled
                 (condition-case err
                     (progn
                       (e-backend-stream-batch
                        backend
                        :messages messages
                        :options options
                        :on-item on-item
                        :on-request-start on-request-start)
                       (when on-done
                         (funcall on-done '(:status done))))
                   (error
                    (when on-error
                      (funcall on-error err))))))))
      request))
   (t
    (signal 'wrong-type-argument
            (list 'functionp (e-backend--start backend))))))

(cl-defun e-backend-fake-create
    (&key name items cancel-function delay context-capabilities
          provider-compaction)
  "Create fake backend NAME that streams ITEMS synchronously.
CANCEL-FUNCTION is attached to the fake request handle when non-nil.
DELAY controls async fake delivery in seconds.  CONTEXT-CAPABILITIES is an
optional semantic declaration used by context/anchor tests."
  (e-backend-create
   :name (or name "fake")
   :provider-compaction provider-compaction
   :context-capabilities context-capabilities
   :stream (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (when cancel-function
                (e-backend-note-request-started
                 (e-backend-request-create :cancel cancel-function)))
              (dolist (item items)
                (funcall on-item item))))
   :start (cl-function
           (lambda (&key messages options on-item on-done on-error
                         on-request-start)
             (ignore messages options)
             (let ((cancelled nil)
                   (timer nil)
                   request)
               (setq request
                     (e-backend-request-create
                      :cancel (lambda ()
                                (setq cancelled t)
                                (when (timerp timer)
                                  (cancel-timer timer))
                                (when cancel-function
                                  (funcall cancel-function))
                                t)
                      :metadata '(:transport timer :cancellable t)))
               (when on-request-start
                 (funcall on-request-start request))
               (setq timer
                     (run-at-time
                      (or delay 0)
                      nil
                      (lambda ()
                        (unless cancelled
                          (condition-case err
                              (progn
                                (dolist (item items)
                                  (funcall on-item item))
                                (when on-done
                                  (funcall on-done '(:status done))))
                            (error
                             (when on-error
                               (funcall on-error err))))))))
               request)))))

(provide 'e-backend)

;;; e-backend.el ends here
