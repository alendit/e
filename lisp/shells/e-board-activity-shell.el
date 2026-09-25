;;; e-board-activity-shell.el --- Board participant activity view -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; This file is part of e.

;;; Commentary:

;; One bounded Board-owned activity surface for ordinary participants and
;; orchestration workers.  SQLite supplies the detached page; this shell owns
;; only its buffer-local copy, cursor, request, and rendering.  Live execution
;; state can decorate a row with controls and latest progress, but it cannot
;; create, remove, classify, or reorder a durable participant.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'e-board-observation)
(require 'e-board-sqlite-service)
(require 'e-chat-service)
(require 'e-harness-instances)
(require 'e-keymap-hints)
(require 'e-subagent-actions)
(require 'e-subagent-live)
(require 'e-subagent-runner)
(require 'e-work)
(require 'e-workspaces)

(declare-function e-chat-open-session "e-chat")

(defconst e-board-activity-shell-buffer-name "*e-board-activity*"
  "Name of the bounded Board participant activity buffer.")

(defconst e-board-activity-shell-detail-buffer-name "*e-board-activity-detail*"
  "Name of the selected durable participant detail buffer.")

(defconst e-board-activity-shell-raw-buffer-name "*e-board-activity-raw*"
  "Name of the explicitly requested durable transcript buffer.")

(defvar-local e-board-activity-shell--target nil
  "Explicit SQLite publication target displayed by this buffer.")

(defvar-local e-board-activity-shell--live nil
  "Private live-execution owner used only for exact row decoration.")

(defvar-local e-board-activity-shell--page nil
  "Detached bounded Board activity page displayed by this buffer.")

(defvar-local e-board-activity-shell--after nil
  "Opaque cursor used for the current detached Board page request.")

(defvar-local e-board-activity-shell--next nil
  "Opaque cursor for the next detached Board page, when one exists.")

(defvar-local e-board-activity-shell--cursor ""
  "Cursor at the last participant emitted into the current page.")

(defvar-local e-board-activity-shell--request nil
  "Current request-scoped Board activity work handle.")

(defvar-local e-board-activity-shell--detail-request nil
  "Current request-scoped Board summary detail work handle.")

(defvar-local e-board-activity-shell--error nil
  "Request-local observation error displayed by this buffer.")

(defvar-local e-board-activity-shell--focus-run-id nil
  "Durable run id whose task row should receive initial focus.")

(defvar-local e-board-activity-shell--focus-entry-id nil
  "Last selected durable activity entry id, preserved across page refreshes.")

(defun e-board-activity-shell--target-id ()
  "Return the current durable Board id."
  (e-board-sqlite-publication-target-board-id
   e-board-activity-shell--target))

(defun e-board-activity-shell--participants ()
  "Return the detached page participants, or nil on an empty/error page."
  (plist-get e-board-activity-shell--page :participants))

(defun e-board-activity-shell--tasks ()
  "Return the selected run's detached task rows, or nil without a run."
  (plist-get e-board-activity-shell--page :tasks))

(defun e-board-activity-shell--task-id (task)
  "Return TASK's stable activity row identity."
  (list :run-task
        (plist-get (plist-get e-board-activity-shell--page :run) :run-id)
        (plist-get task :task-key)
        (plist-get task :accepted-attempt)))

(defun e-board-activity-shell--task (entry-id)
  "Return the selected run task matching activity row ENTRY-ID."
  (and (consp entry-id)
       (eq (car entry-id) :run-task)
       (cl-find-if
        (lambda (task)
          (equal entry-id (e-board-activity-shell--task-id task)))
        (e-board-activity-shell--tasks))))

(defun e-board-activity-shell--row (participant-id)
  "Return durable PARTICIPANT-ID's row from the detached page."
  (cl-find participant-id (e-board-activity-shell--participants)
           :test #'equal :key (lambda (row) (plist-get row :participant-id))))

(defun e-board-activity-shell--live-entry (participant-id)
  "Return exact live state for PARTICIPANT-ID on this Board, or nil."
  (and e-board-activity-shell--live
       (e-subagent-live-get e-board-activity-shell--live
                            (e-board-activity-shell--target-id)
                            participant-id)))

(defun e-board-activity-shell--outcome-label (row)
  "Return a concise durable outcome label for ROW."
  (if-let* ((outcome (plist-get row :outcome))
            (source (plist-get outcome :source))
            (status (plist-get outcome :status)))
      (format "%s/%s" source status)
    "-"))

(defun e-board-activity-shell--progress-label (entry)
  "Return the latest bounded live progress label from ENTRY."
  (if-let* ((progress (and entry (plist-get entry :progress))))
      (format "#%s %s"
              (or (plist-get progress :sequence) 0)
              (or (plist-get progress :summary) ""))
    "-"))

(defun e-board-activity-shell--entry (row)
  "Return one tabulated activity entry for durable ROW."
  (let* ((participant-id (plist-get row :participant-id))
         (entry (e-board-activity-shell--live-entry participant-id))
         (outcome (plist-get row :outcome))
         (summary (or (plist-get outcome :summary)
                      (plist-get outcome :error)
                      "-")))
    (list participant-id
          (vector participant-id
                  (or (plist-get row :name) "-")
                  (format "%s" (or (plist-get row :state) '-))
                  (e-board-activity-shell--outcome-label row)
                  (or (plist-get row :run-id) "-")
                  (or (plist-get row :task-key) "-")
                  (if (integerp (plist-get row :attempt))
                      (number-to-string (plist-get row :attempt))
                    "-")
                  (if entry "available" "unavailable")
                  (format "%s" summary)
                  (format "%s"
                          (or (plist-get row :reasoning-summary-preview) "-"))
                  (e-board-activity-shell--progress-label entry)))))

(defun e-board-activity-shell--task-entry (task)
  "Return one tabulated activity entry for durable run TASK."
  (let* ((participant (plist-get task :participant-row))
         (participant-id (plist-get task :participant-id))
         (live (and participant-id
                    (e-board-activity-shell--live-entry participant-id)))
         (outcome (plist-get task :outcome))
         (run (plist-get e-board-activity-shell--page :run))
         (label (or (plist-get task :label)
                    (plist-get task :task-key)))
         (summary (or (plist-get outcome :summary)
                      (plist-get outcome :error)
                      "-")))
    (list (e-board-activity-shell--task-id task)
          (vector (format "Task: %s" label)
                  (or (and participant
                           (or (plist-get participant :name) participant-id))
                      "unassigned")
                  (format "%s" (or (plist-get task :state) '-))
                  (if (plist-get outcome :status)
                      (format "report/%s" (plist-get outcome :status))
                    "-")
                  (or (plist-get run :label)
                      (plist-get run :run-id) "-")
                  (or (plist-get task :task-key) "-")
                  (if (integerp (plist-get task :accepted-attempt))
                      (number-to-string
                       (plist-get task :accepted-attempt))
                    "-")
                  (cond (live "available")
                        (participant-id "unavailable")
                        (t "unassigned"))
                  (format "%s" summary)
                  (format "%s"
                          (or (plist-get participant
                                         :reasoning-summary-preview)
                              "-"))
                  (e-board-activity-shell--progress-label live)))))

(defun e-board-activity-shell--entry-ids (entries)
  "Return activity row ids from tabulated ENTRIES."
  (mapcar #'car entries))

(defun e-board-activity-shell--initial-focus-id (entries)
  "Return the first selected-run task or participant id in ENTRIES."
  (or (and e-board-activity-shell--focus-run-id
           (car (e-board-activity-shell--entry-ids
                 (cl-remove-if-not
                  (lambda (entry)
                    (and (consp (car entry))
                         (eq (caar entry) :run-task)))
                  entries))))
      (and e-board-activity-shell--focus-run-id
           (car (e-board-activity-shell--entry-ids
                 (cl-remove-if-not
                  (lambda (entry)
                    (and (stringp (car entry))
                         (equal e-board-activity-shell--focus-run-id
                                (plist-get
                                 (e-board-activity-shell--row (car entry))
                                 :run-id))))
                  entries))))
      (car (e-board-activity-shell--entry-ids entries))))

(defun e-board-activity-shell--goto-entry-id (entry-id)
  "Move point to activity row ENTRY-ID, or the first row when absent."
  (goto-char (point-min))
  (forward-line 1)
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (equal entry-id (tabulated-list-get-id))
          (setq found t)
        (forward-line 1)))
    (unless found
      (goto-char (point-min))
      (forward-line 1))))

(defconst e-board-activity-shell--hint-bindings
  '(("RET" . "open chat")
    ("d" . "durable details")
    ("v" . "reasoning summary")
    ("r" . "raw transcript")
    ("p" . "live progress")
    ("s" . "steer")
    ("u" . "send")
    ("i" . "interrupt")
    ("k" . "shutdown")
    ("g" . "refresh")
    ("n" . "next page"))
  "Ordered key hints shown in the Board activity footer.")

(defun e-board-activity-shell--render ()
  "Render the detached page and request-local error in this buffer."
  (when (derived-mode-p 'e-board-activity-shell-mode)
    (let* ((current-id (or (tabulated-list-get-id)
                           e-board-activity-shell--focus-entry-id))
           (entries
            (append
             (mapcar #'e-board-activity-shell--task-entry
                     (e-board-activity-shell--tasks))
             (mapcar #'e-board-activity-shell--entry
                     (e-board-activity-shell--participants))))
           (focus-id
            (if (member current-id (e-board-activity-shell--entry-ids entries))
                current-id
              (or (e-board-activity-shell--initial-focus-id entries)
                  (and (null entries) current-id))))
           (inhibit-read-only t))
      (setq tabulated-list-entries entries)
      ;; Rebuild the bounded presentation region so repeated refreshes do not
      ;; retain prior footers or stale rows in the buffer.
      (erase-buffer)
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (e-board-activity-shell--goto-entry-id focus-id)
      (setq e-board-activity-shell--focus-entry-id focus-id)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n")
        (when-let* ((run (plist-get e-board-activity-shell--page :run)))
          (insert (format "Run: %s [%s]  Status: %s\n"
                          (or (plist-get run :label)
                              (plist-get run :run-id) "-")
                          (or (plist-get run :run-id) "-")
                          (or (plist-get run :terminal-status) "active")))
          (insert (format "Tasks: %d\n"
                          (length (e-board-activity-shell--tasks)))))
        (when e-board-activity-shell--error
          (insert (format "Board activity unavailable: %s\n"
                          e-board-activity-shell--error)))
        (insert (format "Board: %s  Generation: %s\n"
                        (e-board-activity-shell--target-id)
                        (or (plist-get e-board-activity-shell--page
                                       :generation)
                            "loading")))
        (insert (format "Cursor: %s  Next: %s\n"
                        (or e-board-activity-shell--cursor "-")
                        (or e-board-activity-shell--next "-")))
        (e-keymap-hints-insert e-board-activity-shell--hint-bindings)))))

(defun e-board-activity-shell--install-result (buffer target request settled)
  "Install SETTLED REQUEST in BUFFER when it still owns TARGET."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq e-board-activity-shell--target target)
                 (eq e-board-activity-shell--request request))
        (setq e-board-activity-shell--request nil)
        (let ((status (e-work-status settled)))
          (if (eq (plist-get status :state) 'finished)
              (setq e-board-activity-shell--page
                    (copy-tree (e-work-handle-result settled) t)
                    e-board-activity-shell--next
                    (plist-get (e-work-handle-result settled) :next)
                    e-board-activity-shell--cursor
                    (or (plist-get (e-work-handle-result settled) :cursor)
                        e-board-activity-shell--after)
                    e-board-activity-shell--error nil)
            (setq e-board-activity-shell--page nil
                  e-board-activity-shell--next nil
                  e-board-activity-shell--cursor e-board-activity-shell--after
                  e-board-activity-shell--error
                  (e-work-error-message
                   (or (plist-get status :error)
                       '(e-work-cancelled "cancelled")))))
          (e-board-activity-shell--render))))))

(defun e-board-activity-shell--cancel-request ()
  "Cancel the current nonterminal request, if any."
  (when (and e-board-activity-shell--request
             (not (memq (plist-get (e-work-status
                                    e-board-activity-shell--request) :state)
                        '(finished failed cancelled))))
    (e-work-cancel e-board-activity-shell--request)))

(defun e-board-activity-shell--cancel-detail-request ()
  "Cancel the current nonterminal summary-detail request, if any."
  (when (and e-board-activity-shell--detail-request
             (not (memq (plist-get (e-work-status
                                    e-board-activity-shell--detail-request)
                                   :state)
                        '(finished failed cancelled))))
    (e-work-cancel e-board-activity-shell--detail-request))
  (setq e-board-activity-shell--detail-request nil))

(defun e-board-activity-shell--refresh (&optional reset)
  "Start one detached Board observation request for the current buffer."
  (when (derived-mode-p 'e-board-activity-shell-mode)
    (when reset
      (setq e-board-activity-shell--after nil
            e-board-activity-shell--next nil
            e-board-activity-shell--cursor ""))
    (e-board-activity-shell--cancel-request)
    (setq e-board-activity-shell--page nil
          e-board-activity-shell--error nil)
    (e-board-activity-shell--render)
    (condition-case error
        (let* ((buffer (current-buffer))
               (target e-board-activity-shell--target)
               (work (e-board-observation-activity-page-start
                      target :after e-board-activity-shell--after
                      :run-id e-board-activity-shell--focus-run-id
                      :limit e-board-observation-default-page-limit)))
          (setq e-board-activity-shell--request work)
          (e-work-on-settle
           work
           (lambda (settled)
             (e-board-activity-shell--install-result
              buffer target work settled))))
      (error
       (setq e-board-activity-shell--error (error-message-string error))
       (e-board-activity-shell--render)))))

(defun e-board-activity-shell-refresh ()
  "Refresh the detached Board activity page."
  (interactive)
  (e-board-activity-shell--refresh t))

(defun e-board-activity-shell-next-page ()
  "Request the next detached Board activity page, when available."
  (interactive)
  (unless e-board-activity-shell--next
    (user-error "The Board activity page has no next cursor"))
  (setq e-board-activity-shell--after e-board-activity-shell--next)
  (e-board-activity-shell--refresh))

(defun e-board-activity-shell--participant-id-at-point ()
  "Return the durable participant id at point, or signal."
  (let* ((entry-id (tabulated-list-get-id))
         (task (e-board-activity-shell--task entry-id))
         (row (and (stringp entry-id)
                   (e-board-activity-shell--row entry-id))))
    (or (and task (plist-get task :participant-id))
        (and row (plist-get row :participant-id))
        (user-error "No participant is associated with this Board activity row"))))

(defun e-board-activity-shell--selected-entry ()
  "Return the durable activity row selected at point, or signal."
  (let* ((entry-id (tabulated-list-get-id))
         (task (e-board-activity-shell--task entry-id)))
    (or task
        (and (stringp entry-id) (e-board-activity-shell--row entry-id))
        (user-error "No Board activity entry on this line"))))

(defun e-board-activity-shell--selected-row ()
  "Return the durable row selected at point, or signal."
  (let* ((entry-id (tabulated-list-get-id))
         (task (e-board-activity-shell--task entry-id)))
    (or (if task
            (plist-get task :participant-row)
          (and (stringp entry-id)
               (e-board-activity-shell--row entry-id)))
        (user-error "Selected activity row has no detached participant"))))

(defun e-board-activity-shell--require-live (participant-id)
  "Return exact live state for PARTICIPANT-ID or signal unavailable."
  (or (e-board-activity-shell--live-entry participant-id)
      (user-error "Participant %s is not executing in this process"
                  participant-id)))

(defun e-board-activity-shell--instance-id (row)
  "Return the configured harness instance id for durable ROW, if known."
  (let ((role (or (plist-get row :subagent-role)
                  (plist-get (plist-get row :participant) :type))))
    (cond
     ((keywordp role) role)
     ((and (stringp role) (not (string-empty-p role)))
      (intern (concat ":" (string-remove-prefix ":" role))))
     (t nil))))

(defun e-board-activity-shell--harness-for-row (row board-id live)
  "Return the configured harness for durable ROW on BOARD-ID using LIVE."
  (let* ((participant-id (plist-get row :participant-id))
         (live-entry (and live board-id participant-id
                          (e-subagent-live-get live board-id participant-id)))
         (live-harness (and live-entry (plist-get live-entry :harness)))
         (instance-id (e-board-activity-shell--instance-id row)))
    (or live-harness
        (if instance-id
            (condition-case nil
                (e-harness-instance-get-or-create instance-id)
              (e-harness-instance-missing
               (user-error "No configured harness for participant role %s"
                           instance-id)))
          (e-chat-service-default-harness)))))

(defun e-board-activity-shell--harness (row)
  "Return the configured harness for durable ROW, or signal when absent."
  (e-board-activity-shell--harness-for-row
   row (e-board-activity-shell--target-id) e-board-activity-shell--live))

(defun e-board-activity-shell-open-participant-chat (row board-id &optional live)
  "Open the chat session for durable participant ROW on BOARD-ID using LIVE."
  (let ((participant-id (plist-get row :participant-id)))
    (unless (and (stringp participant-id) (not (string-empty-p participant-id)))
      (user-error "The selected Board row has no durable participant"))
    (unless (require 'e-chat nil t)
      (user-error "e-chat is not available to open the participant"))
    (e-chat-open-session
     (e-board-activity-shell--harness-for-row row board-id live)
     participant-id t (e-board-activity-shell--instance-id row))))

(defun e-board-activity-shell-open-chat ()
  "Open the selected durable participant's chat session."
  (interactive)
  (e-board-activity-shell-open-participant-chat
   (e-board-activity-shell--selected-row)
   (e-board-activity-shell--target-id)
   e-board-activity-shell--live))

(defun e-board-activity-shell--show-buffer (name content)
  "Display CONTENT in bounded special buffer NAME."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (special-mode)))
    (e-workspace-pop-to-buffer buffer)
    buffer))

(defun e-board-activity-shell-show-details ()
  "Show the selected detached durable activity projection."
  (interactive)
  (e-board-activity-shell--show-buffer
   e-board-activity-shell-detail-buffer-name
   (pp-to-string (e-board-activity-shell--selected-entry))))

(defun e-board-activity-shell-show-progress ()
  "Show the selected participant's latest bounded live progress."
  (interactive)
  (let* ((participant-id (e-board-activity-shell--participant-id-at-point))
         (entry (e-board-activity-shell--require-live participant-id)))
    (e-board-activity-shell--show-buffer
     e-board-activity-shell-detail-buffer-name
     (pp-to-string
      (list :participant-id participant-id
            :progress (copy-tree (plist-get entry :progress) t))))))

(defun e-board-activity-shell-show-summary ()
  "Load the selected Board participant's combined reasoning summary."
  (interactive)
  (let* ((row (e-board-activity-shell--selected-row))
         (record-id (plist-get row :reasoning-summary-record-id)))
    (unless (and (stringp record-id) (not (string-empty-p record-id)))
      (user-error "Selected participant has no reasoning summary"))
    (e-board-activity-shell--cancel-detail-request)
    (let* ((participant-id (plist-get row :participant-id))
           (harness (e-board-activity-shell--harness row))
           (owner (current-buffer))
           (buffer (e-board-activity-shell--show-buffer
                    e-board-activity-shell-detail-buffer-name
                    (format "Loading reasoning summary for %s...\n"
                            participant-id)))
           (work (e-board-observation-activity-detail-start
                  e-board-activity-shell--target record-id
                  (e-chat-service-session-store harness))))
      (setf (buffer-local-value 'e-board-activity-shell--detail-request owner)
            work)
      (e-work-on-settle
       work
       (lambda (settled)
         (when (and (buffer-live-p owner)
                    (eq (buffer-local-value
                         'e-board-activity-shell--detail-request owner)
                        work))
           (setf (buffer-local-value 'e-board-activity-shell--detail-request
                                     owner)
                 nil)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let ((inhibit-read-only t)
                     (status (e-work-status settled)))
                 (erase-buffer)
                 (if (eq (plist-get status :state) 'finished)
                     (insert (plist-get (e-work-handle-result settled)
                                        :summary))
                   (insert (format "Unable to read reasoning summary: %s\n"
                                   (e-work-error-message
                                    (or (plist-get status :error)
                                        '(e-work-cancelled "cancelled"))))))
               (special-mode)))))))
      buffer)))

(defun e-board-activity-shell-show-raw ()
  "Show a bounded durable transcript page for the selected participant."
  (interactive)
  (let* ((row (e-board-activity-shell--selected-row))
         (participant-id (plist-get row :participant-id))
         (harness (e-board-activity-shell--harness row))
         (buffer
          (e-board-activity-shell--show-buffer
           e-board-activity-shell-raw-buffer-name
           (format "Loading durable transcript for %s...\n" participant-id)))
         (work (e-board-observation-session-page-start
                (e-chat-service-session-store harness) participant-id)))
    (e-work-on-settle
     work
     (lambda (settled)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((status (e-work-status settled))
                 (inhibit-read-only t))
             (erase-buffer)
             (if (eq (plist-get status :state) 'finished)
                 (insert (pp-to-string (e-work-handle-result settled)))
               (insert (format "Unable to read durable transcript: %s\n"
                               (e-work-error-message
                                (or (plist-get status :error)
                                    '(e-work-cancelled "cancelled"))))))
             (special-mode))))))
    buffer))

(defun e-board-activity-shell-steer ()
  "Steer the selected live participant with a prompt and audit reason."
  (interactive)
  (let* ((participant-id (e-board-activity-shell--participant-id-at-point))
         (_entry (e-board-activity-shell--require-live participant-id))
         (prompt (read-string "Steer prompt: "))
         (reason (read-string "Reason (optional): ")))
    (when (string-empty-p (string-trim prompt))
      (user-error "Steer prompt cannot be empty"))
    (e-subagent-steer
     e-board-activity-shell--live
     (e-board-activity-shell--target-id)
     e-board-activity-shell--target
     participant-id prompt
     (unless (string-empty-p (string-trim reason)) reason))
    (e-board-activity-shell--refresh)))

(defun e-board-activity-shell-send ()
  "Send a follow-up prompt to the selected live participant."
  (interactive)
  (let* ((participant-id (e-board-activity-shell--participant-id-at-point))
         (_entry (e-board-activity-shell--require-live participant-id))
         (prompt (read-string "Send prompt: ")))
    (when (string-empty-p (string-trim prompt))
      (user-error "Send prompt cannot be empty"))
    (e-subagent-send e-board-activity-shell--live
                     (e-board-activity-shell--target-id)
                     participant-id prompt)
    (e-board-activity-shell--refresh)))

(defun e-board-activity-shell-interrupt ()
  "Interrupt the selected live participant."
  (interactive)
  (let ((participant-id (e-board-activity-shell--participant-id-at-point)))
    (e-board-activity-shell--require-live participant-id)
    (e-subagent-interrupt e-board-activity-shell--live
                          (e-board-activity-shell--target-id)
                          e-board-activity-shell--target participant-id)
    (e-board-activity-shell--refresh)))

(defun e-board-activity-shell-shutdown ()
  "Shut down the selected live participant."
  (interactive)
  (let ((participant-id (e-board-activity-shell--participant-id-at-point)))
    (e-board-activity-shell--require-live participant-id)
    (e-subagent-shutdown e-board-activity-shell--live
                         (e-board-activity-shell--target-id)
                         e-board-activity-shell--target participant-id)
    (e-board-activity-shell--refresh)))

(defvar e-board-activity-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'e-board-activity-shell-open-chat)
    (define-key map (kbd "d") #'e-board-activity-shell-show-details)
    (define-key map (kbd "v") #'e-board-activity-shell-show-summary)
    (define-key map (kbd "r") #'e-board-activity-shell-show-raw)
    (define-key map (kbd "p") #'e-board-activity-shell-show-progress)
    (define-key map (kbd "s") #'e-board-activity-shell-steer)
    (define-key map (kbd "u") #'e-board-activity-shell-send)
    (define-key map (kbd "i") #'e-board-activity-shell-interrupt)
    (define-key map (kbd "k") #'e-board-activity-shell-shutdown)
    (define-key map (kbd "g") #'e-board-activity-shell-refresh)
    (define-key map (kbd "n") #'e-board-activity-shell-next-page)
    map)
  "Keymap for `e-board-activity-shell-mode'.")

(define-derived-mode e-board-activity-shell-mode tabulated-list-mode
  "e-Board-Activity"
  "Major mode displaying one bounded mixed Board participant activity page."
  (setq tabulated-list-format
        [("Participant / task" 32 t)
         ("Name" 24 t)
         ("State" 12 t)
         ("Outcome" 18 t)
         ("Run" 24 t)
         ("Task" 20 t)
         ("Attempt" 8 t)
         ("Live" 12 t)
         ("Summary" 32 nil)
         ("Reasoning" 36 nil)
         ("Progress" 32 nil)])
  (setq tabulated-list-padding 1
        tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'e-board-activity-shell-mode 'emacs))
(with-eval-after-load 'evil
  (evil-set-initial-state 'e-board-activity-shell-mode 'emacs))

;;;###autoload
(cl-defun e-board-activity-list-buffer
    (&key target (live e-subagent-actions-default-live) run-id)
  "Open TARGET's bounded Board activity buffer and return immediately.
TARGET is an explicit SQLite publication address.  LIVE is optional private
execution state used only to add current controls and bounded progress to
matching durable rows.  RUN-ID, when supplied, selects that run's task
dispositions and keeps the Board participant inventory available."
  (interactive)
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (let ((buffer (get-buffer-create e-board-activity-shell-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'e-board-activity-shell-mode)
        (e-board-activity-shell-mode))
      ;; Reopening the singleton buffer must retire the previous detached
      ;; request before replacing its target and request identity.  Otherwise
      ;; a held read can continue after the buffer has moved to another Board.
      (e-board-activity-shell--cancel-request)
      (e-board-activity-shell--cancel-detail-request)
      (setq e-board-activity-shell--target target
            e-board-activity-shell--live live
            e-board-activity-shell--after nil
            e-board-activity-shell--next nil
            e-board-activity-shell--cursor ""
            e-board-activity-shell--focus-run-id run-id
            e-board-activity-shell--focus-entry-id nil
            e-board-activity-shell--page nil
            e-board-activity-shell--request nil
            e-board-activity-shell--error nil)
      (e-board-activity-shell--refresh t))
    (when (called-interactively-p 'interactive)
      (e-workspace-pop-to-buffer buffer))
    buffer))

(provide 'e-board-activity-shell)

;;; e-board-activity-shell.el ends here
