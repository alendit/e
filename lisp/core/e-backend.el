;;; e-backend.el --- Backend contract for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral backend adapter contract for the core runtime.

;;; Code:

(require 'cl-lib)
(require 'e-request)

(cl-defstruct (e-backend-request (:constructor e-backend-request-create))
  cancel
  metadata)

(cl-defstruct (e-backend
               (:constructor e-backend-create)
               (:conc-name e-backend--))
  name
  stream
  start
  normalize-error-details
  context-capabilities)

(defconst e-backend-context-capability-values
  '((:continuation none linear branchable)
    (:observation-delivery inherited request-local-replaceable)
    (:prefix-cache none implicit explicit)
    (:provider-compaction none opaque)
    (:reasoning-state none replayable provider-managed))
  "Provider-neutral values accepted by `e-backend-context-capabilities'.")

(defconst e-backend--default-context-capabilities
  '(:continuation none
    :observation-delivery inherited
    :prefix-cache none
    :provider-compaction none
    :reasoning-state none)
  "Conservative capabilities used when an adapter declares none.")

(define-error 'e-backend-invalid-context-capabilities
  "Invalid provider-neutral backend context capabilities")

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
            (unless (memq value allowed)
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
                         e-backend--default-context-capabilities)
                        (t declaration))))
    (setq capabilities (or capabilities
                           e-backend--default-context-capabilities))
    (e-backend--validate-context-capabilities capabilities)))

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

(cl-defun e-backend-fake-create (&key name items cancel-function delay)
  "Create fake backend NAME that streams ITEMS synchronously.
CANCEL-FUNCTION is attached to the fake request handle when non-nil.
DELAY controls async fake delivery in seconds."
  (e-backend-create
   :name (or name "fake")
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
