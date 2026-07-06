;;; e-ui-work.el --- Main-thread UI work substrate for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Presentation-side scheduling on top of `e-work'.  Async callbacks and timers
;; should enter UI mutation through this layer so buffer/window effects have one
;; lifecycle, cancellation, coalescing, and diagnostic surface.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-request)
(require 'e-work)

(define-error 'e-ui-work-error "e ui work error" 'e-work-error)
(define-error 'e-ui-work-invalid-spec "invalid e ui work spec" 'e-ui-work-error)

(defconst e-ui-work-focus-policies '(preserve tail-if-selected explicit)
  "Supported UI work focus policies.")

(defconst e-ui-work-reentrancy-policies '(defer allow)
  "Supported UI work reentrancy policies.")

(defvar-local e-ui-work--pending-jobs nil
  "Pending UI work jobs owned by the current buffer.")

(defvar-local e-ui-work--running-keys nil
  "UI work owner/key pairs currently running in the current buffer.")

(defvar e-ui-work--batch-drain-allowed nil
  "Non-nil when `e-ui-work-drain-batch' may block.")

(cl-defstruct (e-ui-work-spec
               (:constructor e-ui-work-spec--create)
               (:conc-name e-ui-work-spec-))
  id
  description
  owner
  target-buffer
  key
  generation
  apply
  focus-policy
  reentrancy-policy
  delay
  coalesce
  budget
  deadline
  metadata
  stale-p
  interval)

(cl-defstruct (e-ui-work-job
               (:constructor e-ui-work-job--create)
               (:conc-name e-ui-work-job-))
  id
  spec
  buffer
  handle
  timer)

(defmacro e-ui-work-with-batch-drain (&rest body)
  "Allow batch/test UI work draining around BODY."
  (declare (indent 0) (debug t))
  `(let ((e-ui-work--batch-drain-allowed t))
     ,@body))

(defun e-ui-work--validate-spec (spec)
  "Signal unless SPEC declares a usable UI work contract."
  (unless (e-ui-work-spec-p spec)
    (signal 'wrong-type-argument (list 'e-ui-work-spec-p spec)))
  (unless (e-ui-work-spec-id spec)
    (signal 'e-ui-work-invalid-spec (list "UI work requires :id" spec)))
  (unless (e-ui-work-spec-description spec)
    (signal 'e-ui-work-invalid-spec
            (list "UI work requires :description" spec)))
  (unless (e-ui-work-spec-owner spec)
    (signal 'e-ui-work-invalid-spec (list "UI work requires :owner" spec)))
  (unless (functionp (e-ui-work-spec-apply spec))
    (signal 'e-ui-work-invalid-spec (list "UI work requires :apply" spec)))
  (unless (bufferp (e-ui-work-spec-target-buffer spec))
    (signal 'e-ui-work-invalid-spec
            (list "UI work requires :target-buffer" spec)))
  (unless (memq (e-ui-work-spec-focus-policy spec)
                e-ui-work-focus-policies)
    (signal 'e-ui-work-invalid-spec
            (list "Unsupported UI work focus policy"
                  (e-ui-work-spec-focus-policy spec))))
  (unless (memq (e-ui-work-spec-reentrancy-policy spec)
                e-ui-work-reentrancy-policies)
    (signal 'e-ui-work-invalid-spec
            (list "Unsupported UI work reentrancy policy"
                  (e-ui-work-spec-reentrancy-policy spec))))
  spec)

(cl-defun e-ui-work-spec-create
    (&rest args
           &key id description owner target-buffer key generation apply
           focus-policy reentrancy-policy delay coalesce budget deadline
           metadata stale-p interval
           &allow-other-keys)
  "Create a validated UI work spec."
  (ignore id description owner target-buffer key generation apply
          focus-policy reentrancy-policy delay coalesce budget deadline
          metadata stale-p interval)
  (e-ui-work--validate-spec (apply #'e-ui-work-spec--create args)))

(put 'e-ui-work-spec-create 'compiler-macro nil)

(defun e-ui-work--job-key (job)
  "Return stable owner/key identity for JOB."
  (let ((spec (e-ui-work-job-spec job)))
    (list (e-ui-work-spec-owner spec)
          (e-ui-work-spec-key spec))))

(defun e-ui-work--matches-p (job owner key)
  "Return non-nil when JOB matches OWNER and KEY."
  (let ((spec (e-ui-work-job-spec job)))
    (and (eq (e-ui-work-spec-owner spec) owner)
         (or (eq key :any)
             (equal (e-ui-work-spec-key spec) key)))))

(defun e-ui-work--pending-buffer (buffer)
  "Return live BUFFER or nil."
  (and (bufferp buffer) (buffer-live-p buffer) buffer))

(defun e-ui-work--remember-job (job)
  "Track JOB in its target buffer."
  (when-let ((buffer (e-ui-work--pending-buffer
                      (e-ui-work-job-buffer job))))
    (with-current-buffer buffer
      (push job e-ui-work--pending-jobs))))

(defun e-ui-work--drop-job (job)
  "Drop JOB from its target buffer registry."
  (when-let ((buffer (e-ui-work--pending-buffer
                      (e-ui-work-job-buffer job))))
    (with-current-buffer buffer
      (setq e-ui-work--pending-jobs
            (delq job e-ui-work--pending-jobs)))))

(defun e-ui-work--job-pending-p (job)
  "Return non-nil when JOB is still pending."
  (when-let ((buffer (e-ui-work--pending-buffer
                      (e-ui-work-job-buffer job))))
    (with-current-buffer buffer
      (memq job e-ui-work--pending-jobs))))

(defun e-ui-work--running-p (job)
  "Return non-nil when JOB's owner/key is already running."
  (when-let ((buffer (e-ui-work--pending-buffer
                      (e-ui-work-job-buffer job))))
    (with-current-buffer buffer
      (member (e-ui-work--job-key job) e-ui-work--running-keys))))

(defun e-ui-work--cancel-job-timer (job)
  "Cancel JOB's current timer, if any."
  (when-let ((timer (e-ui-work-job-timer job)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (e-ui-work-job-timer job) nil)))

(defun e-ui-work-cancel (handle-or-job)
  "Cancel HANDLE-OR-JOB and its pending UI timer."
  (let* ((job (if (e-ui-work-job-p handle-or-job)
                  handle-or-job
                (cl-loop for buffer in (buffer-list)
                         thereis
                         (with-current-buffer buffer
                           (and (boundp 'e-ui-work--pending-jobs)
                                (cl-find handle-or-job
                                         e-ui-work--pending-jobs
                                         :key #'e-ui-work-job-handle))))))
         (handle (cond
                  ((e-work-handle-p handle-or-job) handle-or-job)
                  ((e-ui-work-job-p handle-or-job)
                   (e-ui-work-job-handle handle-or-job)))))
    (when job
      (e-ui-work--cancel-job-timer job)
      (e-ui-work--drop-job job))
    (when (e-work-handle-p handle)
      (e-work-cancel handle))))

(cl-defun e-ui-work-cancel-matching (buffer owner &key (key :any))
  "Cancel pending UI work in BUFFER matching OWNER and KEY."
  (when-let ((buffer (e-ui-work--pending-buffer buffer)))
    (with-current-buffer buffer
      (dolist (job (copy-sequence e-ui-work--pending-jobs))
        (when (e-ui-work--matches-p job owner key)
          (e-ui-work-cancel job))))))

(defun e-ui-work--target-window-state (buffer)
  "Capture point and visible window state for BUFFER."
  (when-let ((buffer (e-ui-work--pending-buffer buffer)))
    (with-current-buffer buffer
      (list :point (copy-marker (point) nil)
            :windows
            (mapcar (lambda (window)
                      (list :window window
                            :point (window-point window)
                            :start (window-start window)))
                    (get-buffer-window-list buffer nil t))))))

(defun e-ui-work--restore-target-window-state (state)
  "Restore point and window STATE captured by `e-ui-work--target-window-state'."
  (when state
    (when-let ((point (plist-get state :point)))
      (when (marker-position point)
        (goto-char (min (marker-position point) (point-max)))))
    (dolist (entry (plist-get state :windows))
      (let ((window (plist-get entry :window)))
        (when (window-live-p window)
          (when-let ((start (plist-get entry :start)))
            (set-window-start window (min start (point-max)) t))
          (when-let ((point (plist-get entry :point)))
            (set-window-point window (min point (point-max)))))))))

(defun e-ui-work--position-value (position)
  "Return integer value for POSITION when it names a live buffer position."
  (cond
   ((markerp position) (marker-position position))
   ((integerp position) position)))

(defun e-ui-work--tail-position (spec buffer)
  "Return SPEC's current tail position for BUFFER."
  (with-current-buffer buffer
    (let ((tail (plist-get (e-ui-work-spec-metadata spec) :tail-position)))
      (or (e-ui-work--position-value
           (cond
            ((functionp tail) (funcall tail))
            (tail tail)))
          (point-max)))))

(defun e-ui-work--tail-focus-state (spec buffer)
  "Return tail focus state for SPEC and BUFFER when selected at tail."
  (let ((window (selected-window)))
    (when (and (window-live-p window)
               (eq (window-buffer window) buffer))
      (with-current-buffer buffer
        (let ((tail (e-ui-work--tail-position spec buffer)))
          (when (and tail
                     (= (point) tail)
                     (= (window-point window) tail))
            (list :kind 'tail :window window)))))))

(defun e-ui-work--capture-focus-state (spec buffer)
  "Capture focus state for SPEC in BUFFER."
  (pcase (e-ui-work-spec-focus-policy spec)
    ('preserve
     (list :kind 'preserve
           :state (e-ui-work--target-window-state buffer)))
    ('tail-if-selected
     (or (e-ui-work--tail-focus-state spec buffer)
         (list :kind 'preserve
               :state (e-ui-work--target-window-state buffer))))))

(defun e-ui-work--restore-focus-state (spec buffer state)
  "Restore focus STATE for SPEC in BUFFER."
  (pcase (plist-get state :kind)
    ('preserve
     (e-ui-work--restore-target-window-state (plist-get state :state)))
    ('tail
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (let* ((window (plist-get state :window))
                (tail (min (point-max)
                           (max (point-min)
                                (or (e-ui-work--tail-position spec buffer)
                                    (point-max))))))
           (goto-char tail)
           (when (and (window-live-p window)
                      (eq (window-buffer window) buffer))
             (set-window-point window tail))))))))

(defun e-ui-work--run-job-now (job)
  "Apply JOB now and settle its handle."
  (let* ((spec (e-ui-work-job-spec job))
         (handle (e-ui-work-job-handle job))
         (buffer (e-ui-work-job-buffer job)))
    (cond
     ((not (buffer-live-p buffer))
      (e-ui-work--drop-job job)
      (e-work-cancel handle))
     ((and (functionp (e-ui-work-spec-stale-p spec))
           (funcall (e-ui-work-spec-stale-p spec) job))
      (e-ui-work--drop-job job)
      (e-work-cancel handle))
     ((and (eq (e-ui-work-spec-reentrancy-policy spec) 'defer)
           (e-ui-work--running-p job))
      (e-ui-work--cancel-job-timer job)
      (setf (e-ui-work-job-timer job)
            (run-at-time 0 nil #'e-ui-work--run-job-now job)))
     ((not (e-ui-work--job-pending-p job))
      (e-work-cancel handle))
     (t
      (with-current-buffer buffer
        (let* ((key (e-ui-work--job-key job))
               (focus-policy (e-ui-work-spec-focus-policy spec))
               (state (and (memq focus-policy '(preserve tail-if-selected))
                           (e-ui-work--capture-focus-state spec buffer))))
          (push key e-ui-work--running-keys)
          (e-ui-work--drop-job job)
          (unwind-protect
              (condition-case err
                  (let ((result (funcall (e-ui-work-spec-apply spec)
                                         job handle)))
                    (when state
                      (e-ui-work--restore-focus-state
                       spec buffer state))
                    (e-work-finish handle (or result '(:status ok))))
                (error
                 (e-work-fail handle err)))
            (setq e-ui-work--running-keys
                  (delete key e-ui-work--running-keys)))))))))

(defun e-ui-work--render-runner (job)
  "Run JOB from the underlying render carrier and self-settle."
  (e-ui-work--run-job-now job)
  :deferred)

(cl-defun e-ui-work-schedule
    (spec &key on-done on-error on-progress on-event)
  "Schedule SPEC as main-thread UI work and return its `e-work' handle."
  (setq spec (e-ui-work--validate-spec spec))
  (let* ((buffer (e-ui-work-spec-target-buffer spec))
         (owner (e-ui-work-spec-owner spec))
         (key (e-ui-work-spec-key spec))
         job
         handle)
    (unless (buffer-live-p buffer)
      (signal 'e-ui-work-invalid-spec (list "UI work target buffer is dead")))
    (when (e-ui-work-spec-coalesce spec)
      (e-ui-work-cancel-matching buffer owner :key key))
    (setq job (e-ui-work-job--create
               :id (e-ui-work-spec-id spec)
               :spec spec
               :buffer buffer))
    (setq handle
          (e-work-start
           (e-work-spec-create
            :id (format "ui_%s" (e-ui-work-spec-id spec))
            :description (e-ui-work-spec-description spec)
            :execution 'render
            :interactive-policy 'async
            :owner (e-ui-work-spec-owner spec)
            :metadata (append
                       (list :transport 'ui-work
                             :owner (e-ui-work-spec-owner spec)
                             :key (e-ui-work-spec-key spec)
                             :generation (e-ui-work-spec-generation spec)
                             :target-buffer (buffer-name buffer))
                       (e-ui-work-spec-metadata spec))
            :deadline (e-ui-work-spec-deadline spec)
            :runner (lambda (_arguments _context)
                      (e-ui-work--render-runner job)))
           (list :delay (or (e-ui-work-spec-delay spec) 0))
           :context (list :owner owner
                          :key key
                          :generation (e-ui-work-spec-generation spec))
           :on-done on-done
           :on-error on-error
           :on-progress on-progress
           :on-event on-event))
    (setf (e-ui-work-job-handle job) handle)
    (setf (e-ui-work-job-timer job)
          (plist-get (e-work-handle-metadata handle) :timer))
    (e-work-add-cleanup
     handle
     (lambda (_handle)
       (e-ui-work--cancel-job-timer job)
       (e-ui-work--drop-job job)))
    (e-ui-work--remember-job job)
    handle))

(cl-defun e-ui-work-schedule-chunks
    (buffer items render-item
            &key id description owner key generation delay budget coalesce
            focus-policy reentrancy-policy on-schedule on-finish)
  "Schedule bounded chunk rendering for ITEMS in BUFFER.
RENDER-ITEM is called for each item.  Return the first scheduled handle."
  (let ((remaining (copy-sequence items))
        (budget (max 1 (or budget 1)))
        first-handle)
    (cl-labels
        ((schedule-next ()
           (when remaining
             (let ((handle
                    (e-ui-work-schedule
                     (e-ui-work-spec-create
                      :id id
                      :description description
                      :owner owner
                      :target-buffer buffer
                      :key key
                      :generation generation
                      :delay delay
                      :coalesce coalesce
                      :focus-policy (or focus-policy 'preserve)
                      :reentrancy-policy (or reentrancy-policy 'defer)
                      :apply
                      (lambda (_job _handle)
                        (let ((count 0))
                          (while (and remaining (< count budget))
                            (funcall render-item (pop remaining))
                            (setq count (1+ count))))
                        (if remaining
                            (schedule-next)
                          (when on-finish
                            (funcall on-finish)))
                        (list :remaining (length remaining)))))))
               (setq coalesce nil)
               (unless first-handle
                 (setq first-handle handle))
               (when on-schedule
                 (funcall on-schedule handle))
               handle))))
      (schedule-next)
      first-handle)))

(cl-defun e-ui-work-schedule-interval
    (spec interval &key on-done on-error on-progress on-event)
  "Schedule repeating UI work SPEC every INTERVAL seconds.
SPEC's apply function should return `:continue' to keep the interval alive.
Any other non-error return value finishes the work handle."
  (setq spec (e-ui-work--validate-spec spec))
  (let* ((buffer (e-ui-work-spec-target-buffer spec))
         job
         handle)
    (setq job (e-ui-work-job--create
               :id (e-ui-work-spec-id spec)
               :spec spec
               :buffer buffer))
    (cl-labels
        ((cancel-interval (&optional _handle)
           (e-ui-work--cancel-job-timer job)
           (e-ui-work--drop-job job))
         (tick ()
           (cond
            ((not (buffer-live-p buffer))
             (e-ui-work--drop-job job)
             (e-work-cancel handle))
            ((and (functionp (e-ui-work-spec-stale-p spec))
                  (funcall (e-ui-work-spec-stale-p spec) job))
             (e-ui-work--drop-job job)
             (e-work-cancel handle))
            (t
             (with-current-buffer buffer
               (condition-case err
                   (let ((result (funcall (e-ui-work-spec-apply spec)
                                          job handle)))
                     (if (eq result :continue)
                         (setf (e-ui-work-job-timer job)
                               (run-at-time interval nil #'tick))
                       (e-ui-work--drop-job job)
                       (e-work-finish handle (or result '(:status ok)))))
                 (error
                  (e-ui-work--drop-job job)
                  (e-work-fail handle err))))))))
      (setq handle
            (e-work-start
             (e-work-spec-create
              :id (format "ui_%s_interval" (e-ui-work-spec-id spec))
              :description (e-ui-work-spec-description spec)
              :execution 'render
              :interactive-policy 'async
              :owner (e-ui-work-spec-owner spec)
              :metadata (append
                         (list :transport 'ui-work-interval
                               :owner (e-ui-work-spec-owner spec)
                               :key (e-ui-work-spec-key spec)
                               :generation (e-ui-work-spec-generation spec)
                               :target-buffer (buffer-name buffer))
                         (e-ui-work-spec-metadata spec))
              :deadline (e-ui-work-spec-deadline spec)
              :runner (lambda (_arguments _context)
                        (tick)
                        :deferred))
             (list :delay (or interval 0))
             :on-done on-done
             :on-error on-error
             :on-progress on-progress
             :on-event on-event))
      (setf (e-ui-work-spec-interval spec) interval)
      (setf (e-ui-work-job-handle job) handle)
      (setf (e-ui-work-job-timer job)
            (plist-get (e-work-handle-metadata handle) :timer))
      (e-work-add-cleanup handle #'cancel-interval)
      (e-ui-work--remember-job job)
      handle)))

(cl-defun e-ui-work-pending (buffer &key owner (key :any))
  "Return pending UI work status plists for BUFFER."
  (when-let ((buffer (e-ui-work--pending-buffer buffer)))
    (with-current-buffer buffer
      (mapcar
       (lambda (job)
         (let ((spec (e-ui-work-job-spec job)))
           (list :id (e-ui-work-job-id job)
                 :owner (e-ui-work-spec-owner spec)
                 :key (e-ui-work-spec-key spec)
                 :generation (e-ui-work-spec-generation spec)
                 :interval (e-ui-work-spec-interval spec)
                 :handle (e-ui-work-job-handle job)
                 :timer (e-ui-work-job-timer job)
                 :state (plist-get
                         (e-work-status (e-ui-work-job-handle job))
                         :state))))
       (cl-remove-if-not
        (lambda (job)
          (and (or (not owner)
                   (eq (e-ui-work-spec-owner (e-ui-work-job-spec job))
                       owner))
               (or (eq key :any)
                   (equal (e-ui-work-spec-key (e-ui-work-job-spec job))
                          key))))
        e-ui-work--pending-jobs)))))

(defun e-ui-work-status (handle)
  "Return status for UI work HANDLE."
  (e-work-status handle))

(defun e-ui-work--drainable-jobs (jobs include-intervals owner key)
  "Return drainable JOBS narrowed by INCLUDE-INTERVALS, OWNER, and KEY."
  (cl-remove-if-not
   (lambda (job)
     (let ((spec (e-ui-work-job-spec job)))
       (and (or include-intervals
                (null (e-ui-work-spec-interval spec)))
            (or (not owner)
                (eq (e-ui-work-spec-owner spec) owner))
            (or (eq key :any)
                (equal (e-ui-work-spec-key spec) key)))))
   jobs))

(cl-defun e-ui-work-drain-batch
    (&key buffer timeout include-intervals owner (key :any))
  "Drain pending UI work in batch/test code.
This function is rejected from interactive hot paths."
  (unless e-ui-work--batch-drain-allowed
    (signal 'e-work-await-not-allowed
            (list "Wrap batch/test drains in e-ui-work-with-batch-drain")))
  (when (e-request-hot-path-active-p)
    (signal 'e-work-await-in-hot-path
            (list "UI work batch drain is not allowed in hot paths")))
  (let ((deadline (+ (float-time) (or timeout 1.0))))
    (while (and (< (float-time) deadline)
                (if buffer
                    (with-current-buffer buffer
                      (e-ui-work--drainable-jobs
                       e-ui-work--pending-jobs include-intervals owner key))
                  (cl-some (lambda (buf)
                             (with-current-buffer buf
                               (and (boundp 'e-ui-work--pending-jobs)
                                    (e-ui-work--drainable-jobs
                                     e-ui-work--pending-jobs
                                     include-intervals owner key))))
                           (buffer-list))))
      (accept-process-output nil 0.01)))
  (when (and buffer
             (with-current-buffer buffer
               (e-ui-work--drainable-jobs
                e-ui-work--pending-jobs include-intervals owner key)))
    (signal 'e-work-await-timeout (list buffer timeout)))
  t)

(provide 'e-ui-work)

;;; e-ui-work.el ends here
