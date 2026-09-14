;;; e-board-runs-shell.el --- Durable board run list buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This shell observes bounded durable run projections.  It never opens or
;; polls a child session; board fact notifications are its refresh trigger.

;;; Code:

(require 'seq)
(require 'tabulated-list)
(require 'e-board-orchestration-actions)
(require 'e-keymap-hints)
(require 'e-subagent-live)
(require 'e-workspaces)

(defconst e-board-runs-shell-buffer-name "*e-board-runs*"
  "Name of the durable board run list buffer.")

(defconst e-board-runs-shell-detail-buffer-name "*e-board-run*"
  "Name of the bounded durable board run detail buffer.")

(defconst e-board-runs-shell-raw-buffer-name "*e-board-run-raw*"
  "Name of the explicitly requested bounded raw run buffer.")

(defvar-local e-board-runs-shell--target nil
  "Explicit SQLite publication target whose durable runs this buffer displays.")

(defvar-local e-board-runs-shell--projections nil
  "Detached bounded run projections displayed by this buffer.")

(defvar-local e-board-runs-shell--registry nil
  "Optional live subagent registry used to label admission state.")

(defun e-board-runs-shell--state-label (projection)
  "Return the short lifecycle label for run PROJECTION."
  (or (plist-get projection :terminal-status)
      (plist-get projection :state)
      'running))

(defun e-board-runs-shell--task-label (projection)
  "Return completed and total task counts for PROJECTION."
  (let* ((tasks (plist-get projection :tasks))
         (complete (seq-count (lambda (task)
                                (memq (plist-get task :state)
                                      '(done failed cancelled)))
                              tasks)))
    (format "%d/%d" complete (length tasks))))

(defun e-board-runs-shell--deadline-label (projection)
  "Return a concise deadline evidence label for PROJECTION."
  (let ((deadline (plist-get projection :deadline)))
    (cond
     ((not deadline) "-")
     ((plist-get deadline :expired) "expired")
     ((eq (plist-get deadline :kind) 'none) "none")
     (t "active"))))

(defun e-board-runs-shell--owner-session-id (projection)
  "Return PROJECTION's exact continuation owner session id, or nil."
  (plist-get (or (plist-get (plist-get projection :manifest) :continuation)
                 (plist-get projection :continuation))
             :session-id))

(defun e-board-runs-shell--target-id (target)
  "Return TARGET's durable Board identity without reconstructing a Board."
  (e-board-sqlite-publication-target-board-id target))

(defun e-board-runs-shell--entry (target projection)
  "Return a `tabulated-list' entry for durable run PROJECTION."
  (let ((conflicts (plist-get projection :conflicts)))
    (list (plist-get projection :run-id)
          (vector (plist-get projection :run-id)
                  (e-board-runs-shell--target-id target)
                  (or (e-board-runs-shell--owner-session-id projection) "-")
                  (format "%s" (e-board-runs-shell--state-label projection))
                  (e-board-runs-shell--task-label projection)
                  (e-board-runs-shell--deadline-label projection)
                  (format "%s" (or (plist-get (plist-get projection :continuation) :state)
                                    "-"))
                  (if conflicts
                      (propertize (number-to-string (length conflicts))
                                  'face 'font-lock-warning-face)
                    "0")))))

(defconst e-board-runs-shell--hint-bindings
  '(("RET" . "details")
    ("r" . "raw activity")
    ("g" . "refresh"))
  "Ordered key hints shown in the durable run list footer.")

(defun e-board-runs-shell--render ()
  "Render the buffer's detached bounded run projections."
  (when (derived-mode-p 'e-board-runs-shell-mode)
    (setq tabulated-list-entries
          (mapcar (lambda (projection)
                    (e-board-runs-shell--entry
                     e-board-runs-shell--target projection))
                  e-board-runs-shell--projections))
    (tabulated-list-print t)
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (bolp) (insert "\n"))
        (insert "\n")
        (e-keymap-hints-insert e-board-runs-shell--hint-bindings)))))

(defun e-board-runs-shell--install-query-result (buffer target settled)
  "Install TARGET query SETTLED into live BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq e-board-runs-shell--target target)
        (let ((status (e-work-status settled)))
          (if (eq (plist-get status :state) 'finished)
              (setq e-board-runs-shell--projections
                    (copy-tree (plist-get status :result) t))
            (setq e-board-runs-shell--projections nil)
            (message "Unable to query Board runs: %s"
                     (e-work-error-message (plist-get status :error))))
          (e-board-runs-shell--render))))))

(defun e-board-runs-shell--refresh ()
  "Request and render the target's bounded durable run projections."
  (when (derived-mode-p 'e-board-runs-shell-mode)
    (let* ((buffer (current-buffer))
           (target e-board-runs-shell--target)
           (work (e-board-orchestration-actions-list-runs target)))
      (setq e-board-runs-shell--projections nil)
      (e-board-runs-shell--render)
      (e-work-on-settle
       work
       (lambda (settled)
         (e-board-runs-shell--install-query-result
          buffer target settled))))))

(defun e-board-runs-shell-refresh ()
  "Manually rebuild the durable run list."
  (interactive)
  (e-board-runs-shell--refresh))

(defun e-board-runs-shell--refresh-registry-buffers (registry)
  "Refresh run buffers whose live admission labels use REGISTRY."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'e-board-runs-shell-mode)
                 (eq e-board-runs-shell--registry registry))
        (e-board-runs-shell--render)))))

(defun e-board-runs-shell--run-id-at-point ()
  "Return the durable run id on the current row, or signal."
  (or (tabulated-list-get-id)
      (user-error "No durable run on this line")))

(defun e-board-runs-shell--registered-assignments (registry)
  "Return REGISTRY's one bounded process-local registered child list."
  (and registry (e-subagent-registry-list registry)))

(defun e-board-runs-shell--live-assignment
    (registry registered run-id task-key attempt)
  "Return the exact live assignment from REGISTRY or REGISTERED."
  (and registry
       (or (e-subagent-registry-find-pending-assignment
            registry run-id task-key attempt)
           (seq-find
            (lambda (record)
              (and (equal (plist-get record :run-id) run-id)
                   (equal (plist-get record :task-key) task-key)
                   (equal (plist-get record :attempt) attempt)
                   (memq (plist-get record :status)
                         '(queued running blocked))))
            registered))))

(defun e-board-runs-shell--task-disposition (task live)
  "Return TASK's decision-relevant disposition, considering LIVE state."
  (let ((state (plist-get task :state))
        (attempt (plist-get task :accepted-attempt)))
    (cond
     ((memq state '(done failed cancelled)) 'terminal)
     ((> attempt 0) 'retrying)
     (live (plist-get live :status))
     ((eq state 'running) 'orphaned)
     (t 'pending))))

(defun e-board-runs-shell--task-admission (task live)
  "Return TASK's truthful admission state using exact LIVE assignment."
  (let ((state (plist-get task :state)))
    (cond
     ((memq state '(done failed cancelled)) 'terminal)
     (live (plist-get live :status))
     ((eq state 'running) 'orphaned)
     (t 'pending))))

(defun e-board-runs-shell--task-participant-id (task live)
  "Return TASK's exact live or reported participant session id."
  (or (plist-get live :session-id)
      (plist-get (plist-get task :accepted-report) :participant-session-id)
      "-"))

(defun e-board-runs-shell--format-summary (target projection &optional registry)
  "Return a bounded signal-focused summary for TARGET PROJECTION.
REGISTRY contributes only live pending/running assignment labels; SQLite-backed
PROJECTION remains authoritative for durable run and terminal state."
  (let* ((run-id (plist-get projection :run-id))
         (manifest (plist-get projection :manifest))
         (daily-p (plist-get (plist-get manifest :descriptor) :date))
         (registered (e-board-runs-shell--registered-assignments registry))
         (lines
          (list (format "%s: %s" (if daily-p "Daily run id" "Run id") run-id)
                (format "Board id: %s" (e-board-runs-shell--target-id target))
                (format "Owner session id: %s"
                        (or (e-board-runs-shell--owner-session-id projection) "-"))
                (format "Run state: %s"
                        (e-board-runs-shell--state-label projection)))))
    (dolist (task (plist-get projection :tasks))
      (let* ((task-key (plist-get task :task-key))
             (attempt (plist-get task :accepted-attempt))
             (live (e-board-runs-shell--live-assignment
                    registry registered run-id task-key attempt))
             (report (plist-get task :accepted-report))
             (failure (and (memq (plist-get task :state) '(failed cancelled))
                           (or (plist-get report :error)
                               (plist-get report :summary)))))
        (setq lines
              (append
               lines
               (list ""
                     (format "Task: %s" task-key)
                     (format "Attempt: %d" attempt)
                     (format "Participant session id: %s"
                             (e-board-runs-shell--task-participant-id task live))
                     (format "Admission: %s"
                             (e-board-runs-shell--task-admission task live))
                     (format "Disposition: %s"
                             (e-board-runs-shell--task-disposition task live)))
               (when failure
                 (list (format "First failure: %s" failure)))))))
    (concat (string-join lines "\n") "\n")))

(defun e-board-runs-shell--show-buffer (name content workspace)
  "Show CONTENT in special buffer NAME within WORKSPACE."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (special-mode))
      (e-buffer-set-workspace buffer workspace))
    (e-workspace-pop-to-buffer buffer)
    buffer))

(defun e-board-runs-shell--show-projection-result
    (target run-id registry workspace raw value)
  "Show TARGET RUN-ID VALUE in WORKSPACE, formatted unless RAW."
  (let ((show
         (lambda (projection)
           (e-board-runs-shell--show-buffer
            (if raw e-board-runs-shell-raw-buffer-name
              e-board-runs-shell-detail-buffer-name)
            (if raw
                (pp-to-string projection)
              (e-board-runs-shell--format-summary
               target projection registry))
            workspace))))
    (let ((buffer
           (e-board-runs-shell--show-buffer
            (if raw e-board-runs-shell-raw-buffer-name
              e-board-runs-shell-detail-buffer-name)
            (format "Loading Board run %s...\n" run-id) workspace)))
      (e-work-on-settle
       value
       (lambda (settled)
         (when (buffer-live-p buffer)
           (let ((status (e-work-status settled)))
             (if (eq (plist-get status :state) 'finished)
                 (funcall show (plist-get status :result))
               (e-board-runs-shell--show-buffer
                (buffer-name buffer)
                (format "Unable to query Board run %s: %s\n"
                        run-id
                        (e-work-error-message (plist-get status :error)))
                workspace)))))))))

(defun e-board-runs-shell-show-details ()
  "Show the bounded durable projection for the run at point."
  (interactive)
  (let* ((run-id (e-board-runs-shell--run-id-at-point))
         (projection (e-board-orchestration-actions-run-projection
                      e-board-runs-shell--target run-id))
         (workspace (e-buffer-ensure-workspace (current-buffer))))
    (e-board-runs-shell--show-projection-result
     e-board-runs-shell--target run-id e-board-runs-shell--registry
     workspace nil projection)))

(defun e-board-runs-shell-show-raw-activity ()
  "Explicitly show the selected run's bounded raw durable projection."
  (interactive)
  (let* ((run-id (e-board-runs-shell--run-id-at-point))
         (projection (e-board-orchestration-actions-run-projection
                      e-board-runs-shell--target run-id))
         (workspace (e-buffer-ensure-workspace (current-buffer))))
    (e-board-runs-shell--show-projection-result
     e-board-runs-shell--target run-id e-board-runs-shell--registry
     workspace t projection)))

(defvar e-board-runs-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'e-board-runs-shell-show-details)
    (define-key map (kbd "r") #'e-board-runs-shell-show-raw-activity)
    (define-key map (kbd "g") #'e-board-runs-shell-refresh)
    map)
  "Keymap for `e-board-runs-shell-mode'.")

(define-derived-mode e-board-runs-shell-mode tabulated-list-mode "e-Board-Runs"
  "Major mode listing bounded durable run projections."
  (setq tabulated-list-format
        [("Run id" 26 t)
         ("Board id" 24 t)
         ("Owner session id" 24 t)
         ("Status" 16 t)
         ("Tasks" 10 t)
         ("Deadline" 12 t)
         ("Continuation" 14 t)
         ("Conflicts" 10 t)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

;;;###autoload
(cl-defun e-board-runs-list-buffer (&key target registry)
  "Open TARGET's bounded durable run list and return its buffer immediately.
TARGET is an explicit SQLite publication target."
  (interactive)
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (let ((buffer (get-buffer-create e-board-runs-shell-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'e-board-runs-shell-mode)
        (e-board-runs-shell-mode))
      (setq e-board-runs-shell--target target
            e-board-runs-shell--registry registry
            e-board-runs-shell--projections nil)
      (e-board-runs-shell--refresh))
    (add-hook 'e-subagent-registry-change-functions
              #'e-board-runs-shell--refresh-registry-buffers)
    (when (called-interactively-p 'interactive)
      (e-workspace-pop-to-buffer buffer))
    buffer))

(provide 'e-board-runs-shell)

;;; e-board-runs-shell.el ends here
