;;; e-board-activity-visual-shell.el --- Visual Board activity shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Optional egui presentation for the shared Board run-set and one coherent
;; selected-run activity page.  Rust owns only visual state; Elisp validates
;; semantic events and requests detached Board-owned values.

;;; Code:

(require 'cl-lib)
(require 'e-board)
(require 'e-board-activity-shell)
(require 'e-board-activity-visual-view-model)
(require 'e-board-observation)
(require 'e-board-orchestration)
(require 'e-board-sqlite-service)
(require 'e-chat-service)
(require 'e-subagent-actions)
(require 'e-subagent-live)
(require 'e-work)
(require 'e-workspaces)

(declare-function emacs-egui-register-app "emacs-egui" (app-name ui-dir))
(declare-function emacs-egui-create-buffer "emacs-egui" (&rest args))
(declare-function emacs-egui-on "emacs-egui" (session action callback))
(declare-function emacs-egui-send-state "emacs-egui" (session state))
(declare-function emacs-egui-get-field "emacs-egui" (payload key))

(defconst e-board-activity-visual--app-name "e-board"
  "emacs-egui app name for Board activity.")

(defconst e-board-activity-visual-buffer-name "*e-board-activity-visual*"
  "Name of the singleton visual Board activity buffer.")

(defconst e-board-activity-visual-page-limit 64
  "Maximum number of active runs on one visual selector page.")

(defvar e-board-activity-visual--epoch-counter 0
  "Monotone action fence across visual buffer rebinding.")

(defconst e-board-activity-visual-update-debounce 0.05
  "Seconds used to coalesce visual snapshot pushes.")

(defvar-local e-board-activity-visual--target nil
  "Exact publication target currently rendered by the visual shell.")

(defvar-local e-board-activity-visual--binding nil
  "Chat binding whose Board-owned run-set feeds this visual shell.")

(defvar-local e-board-activity-visual--live nil
  "Private execution owner used only for exact participant decorations.")

(defvar-local e-board-activity-visual--egui-session nil
  "emacs-egui session associated with the current visual buffer.")

(defvar-local e-board-activity-visual--actions-wired nil
  "Non-nil when the egui session has its semantic action callback.")

(defvar-local e-board-activity-visual--run-set-unsubscribe nil
  "Unsubscribe closure for the Board-owned shared run-set projection.")

(defvar-local e-board-activity-visual--selector-work nil
  "Current request for one indexed selector page.")

(defvar-local e-board-activity-visual--run-set-projection nil
  "Detached shared projection authoritative for Board status and counts.")

(defvar-local e-board-activity-visual--run-set-epoch 0
  "Local action fence advanced on rebind, shared updates, and navigation.")

(defun e-board-activity-visual--advance-epoch ()
  "Assign this selector a process-monotone action epoch."
  (setq e-board-activity-visual--epoch-counter
        (1+ (max e-board-activity-visual--epoch-counter
                 e-board-activity-visual--run-set-epoch))
        e-board-activity-visual--run-set-epoch
        e-board-activity-visual--epoch-counter))

(defvar-local e-board-activity-visual--selector-browsing nil
  "Non-nil while browsing indexed active-run pages.")

(defvar-local e-board-activity-visual--selector-page nil
  "One detached indexed active-run page, never accumulated.")

(defvar-local e-board-activity-visual--selector-cursor nil
  "Cursor for the requested selector page.")

(defvar-local e-board-activity-visual--selector-loading nil
  "Non-nil while an indexed selector page is loading.")

(defvar-local e-board-activity-visual--selector-error nil
  "Request-local selector page error, when present.")

(defvar-local e-board-activity-visual--selected-run-id nil
  "Durable run id selected for the detailed workspace.")

(defvar-local e-board-activity-visual--selected-task nil
  "Durable task identity selected in the detailed workspace.")

(defvar-local e-board-activity-visual--detail-request nil
  "Current detached request for the selected run activity page.")

(defvar-local e-board-activity-visual--detail-state 'loading
  "Selected detail state: loading, ready, error, or empty.")

(defvar-local e-board-activity-visual--detail-page nil
  "One coherent detached Board activity page for the selected run.")

(defvar-local e-board-activity-visual--detail-error nil
  "Request-local selected-run page error, when present.")

(defvar-local e-board-activity-visual--after nil
  "Participant cursor requested for the current selected-run page.")

(defvar-local e-board-activity-visual--next nil
  "Next bounded Board-wide participant cursor, when available.")

(defvar-local e-board-activity-visual--push-timer nil
  "Debounce timer for the current visual buffer.")

(defun e-board-activity-visual--source-directory ()
  "Return the root directory of the e source tree."
  (or (and (fboundp 'e-source-directory) (e-source-directory))
      (when-let* ((root (locate-dominating-file
                        (or load-file-name buffer-file-name default-directory)
                        "e.el")))
        (file-name-as-directory (expand-file-name root)))
      (error "Cannot locate e source directory")))

(defun e-board-activity-visual--ui-directory ()
  "Return the Board visual asset directory."
  (expand-file-name "ui/board/" (e-board-activity-visual--source-directory)))

(defun e-board-activity-visual--ensure-runtime-path ()
  "Add the shared emacs-egui runtime to `load-path' when present."
  (let* ((root (e-board-activity-visual--source-directory))
         (directory (expand-file-name "emacs-egui/lisp/" root)))
    (when (file-readable-p (expand-file-name "emacs-egui.el" directory))
      (add-to-list 'load-path directory))))

(defun e-board-activity-visual-unavailable-reason ()
  "Return why the visual renderer cannot open, or nil when it is ready."
  (e-board-activity-visual--ensure-runtime-path)
  (let* ((root (e-board-activity-visual--source-directory))
         (ui-directory (e-board-activity-visual--ui-directory))
         (runtime (locate-library "emacs-egui")))
    (cond
     ((not (featurep 'xwidget-internal))
      "Emacs has no xwidget-webkit support")
     ((null runtime)
      (format "the emacs-egui submodule is unavailable under %s; initialize it with git submodule update --init emacs-egui"
              root))
     ((not (file-readable-p (expand-file-name "index.html" ui-directory)))
      (format "the Board UI index is missing under %s" ui-directory))
     ((not (and (file-readable-p (expand-file-name "pkg/e_board.js"
                                                   ui-directory))
                (file-readable-p (expand-file-name "pkg/e_board_bg.wasm"
                                                   ui-directory))))
      (format (concat "the Board WASM package is missing; build it with "
                      "wasm-pack build --target web --release --out-dir pkg ui/board")))
     (t nil))))

(defun e-board-activity-visual--ensure-runtime ()
  "Load the shared egui runtime and register the Board UI."
  (when-let* ((reason (e-board-activity-visual-unavailable-reason)))
    (user-error "Visual Board activity is unavailable: %s" reason))
  (require 'emacs-egui)
  (emacs-egui-register-app e-board-activity-visual--app-name
                           (e-board-activity-visual--ui-directory)))

(defun e-board-activity-visual--target-id ()
  "Return the current target's durable Board id."
  (and (e-board-sqlite-publication-target-valid-p
        e-board-activity-visual--target)
       (e-board-sqlite-publication-target-board-id
        e-board-activity-visual--target)))

(defun e-board-activity-visual--cancel-work (work)
  "Cancel nonterminal WORK."
  (when (and (e-work-handle-p work)
             (not (memq (plist-get (e-work-status work) :state)
                        '(finished failed cancelled))))
    (e-work-cancel work)))

(defun e-board-activity-visual--retire-current ()
  "Detach subscriptions and requests owned by the current buffer."
  (when (functionp e-board-activity-visual--run-set-unsubscribe)
    (funcall e-board-activity-visual--run-set-unsubscribe))
  (setq e-board-activity-visual--run-set-unsubscribe nil)
  (e-board-activity-visual--cancel-work
   e-board-activity-visual--selector-work)
  (e-board-activity-visual--cancel-work
   e-board-activity-visual--detail-request)
  (when (timerp e-board-activity-visual--push-timer)
    (cancel-timer e-board-activity-visual--push-timer))
  (setq e-board-activity-visual--selector-work nil
        e-board-activity-visual--detail-request nil
        e-board-activity-visual--push-timer nil))

(defun e-board-activity-visual--snapshot ()
  "Return the current detached egui snapshot."
  (let* ((page e-board-activity-visual--selector-page)
         (projection (copy-tree e-board-activity-visual--run-set-projection t))
         (runs (when page
                 (mapcar
                  (lambda (entry)
                    (e-board-orchestration-run-index-entry-at
                     (plist-get entry :summary)
                     (e-board-activity-visual--target-id)
                     (or (plist-get projection :restore-state) 'ready)
                     (float-time)))
                  (plist-get page :entries)))))
    (when e-board-activity-visual--selector-browsing
      (setq projection (plist-put projection :runs runs)))
    (e-board-activity-visual-view-model-snapshot
     :board-id (e-board-activity-visual--target-id)
     :projection projection
     :selected-run-id e-board-activity-visual--selected-run-id
     :selected-task e-board-activity-visual--selected-task
     :detail-state e-board-activity-visual--detail-state
     :page e-board-activity-visual--detail-page
     :detail-error e-board-activity-visual--detail-error
     :live e-board-activity-visual--live
     :selector-browsing e-board-activity-visual--selector-browsing
     :selector-loading e-board-activity-visual--selector-loading
     :run-set-epoch e-board-activity-visual--run-set-epoch
     :selector-error e-board-activity-visual--selector-error
     :selector-generation (plist-get page :generation)
     :selector-next (and page (plist-get page :next-cursor))
     :selected-run-visible
     (cl-some (lambda (run)
                (equal (plist-get run :run-id)
                       e-board-activity-visual--selected-run-id))
              (plist-get projection :runs)))))

(defun e-board-activity-visual--push-snapshot (&optional buffer)
  "Push a full detached snapshot for BUFFER or the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when e-board-activity-visual--egui-session
          (emacs-egui-send-state
           e-board-activity-visual--egui-session
           (e-board-activity-visual--snapshot)))))))

(defun e-board-activity-visual--schedule-push (&optional buffer)
  "Schedule a coalesced snapshot push for BUFFER or the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (timerp e-board-activity-visual--push-timer)
          (cancel-timer e-board-activity-visual--push-timer))
        (setq e-board-activity-visual--push-timer
              (run-at-time
               e-board-activity-visual-update-debounce nil
               (lambda (target)
                 (when (buffer-live-p target)
                   (with-current-buffer target
                     (setq e-board-activity-visual--push-timer nil))
                   (e-board-activity-visual--push-snapshot target)))
               buffer))))))

(defun e-board-activity-visual--page-task-id (run-id task)
  "Return TASK's stable identity within RUN-ID."
  (list :run-task run-id (plist-get task :task-key)
        (plist-get task :accepted-attempt)))

(defun e-board-activity-visual--page-has-task-p (page identity)
  "Return non-nil when PAGE contains durable task IDENTITY."
  (let* ((run-id (plist-get (plist-get page :run) :run-id))
         (tasks (plist-get page :tasks)))
    (and (consp identity)
         (eq (car identity) :run-task)
         (equal (nth 1 identity) run-id)
         (cl-some (lambda (task)
                    (equal identity
                           (e-board-activity-visual--page-task-id
                            run-id task)))
                  tasks))))

(defun e-board-activity-visual--selected-run-from-projection ()
  "Choose the first run when the presentation has no current selection."
  (unless e-board-activity-visual--selected-run-id
    (setq e-board-activity-visual--selected-run-id
          (plist-get (car (plist-get e-board-activity-visual--run-set-projection
                                     :runs))
                     :run-id))))

(defun e-board-activity-visual--detail-page-settled
    (buffer target run-id after request settled)
  "Install SETTLED activity REQUEST for RUN-ID when its identity is current."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq target e-board-activity-visual--target)
                 (equal (e-board-sqlite-publication-target-board-id target)
                        (e-board-activity-visual--target-id))
                 (equal run-id e-board-activity-visual--selected-run-id)
                 (eq request e-board-activity-visual--detail-request))
        (setq e-board-activity-visual--detail-request nil)
        (let ((status (e-work-status settled)))
          (if (eq (plist-get status :state) 'finished)
              (let ((page (copy-tree (e-work-handle-result settled) t)))
                (if (and (equal (plist-get page :board-id)
                                (e-board-activity-visual--target-id))
                         (equal (plist-get (plist-get page :run) :run-id)
                                run-id))
                    (setq e-board-activity-visual--detail-page page
                          e-board-activity-visual--next
                          (plist-get page :next)
                          e-board-activity-visual--after after
                          e-board-activity-visual--detail-state 'ready
                          e-board-activity-visual--detail-error nil
                          e-board-activity-visual--selected-task
                          (and (e-board-activity-visual--page-has-task-p
                                page e-board-activity-visual--selected-task)
                               e-board-activity-visual--selected-task))
                  (setq e-board-activity-visual--detail-page nil
                        e-board-activity-visual--next nil
                        e-board-activity-visual--detail-state 'error
                        e-board-activity-visual--detail-error
                        "Board activity response identity did not match")))
            (setq e-board-activity-visual--detail-page nil
                  e-board-activity-visual--next nil
                  e-board-activity-visual--detail-state 'error
                  e-board-activity-visual--detail-error
                  (e-work-error-message
                   (or (plist-get status :error)
                       '(e-work-cancelled "cancelled")))))
          (e-board-activity-visual--schedule-push buffer))))))

(defun e-board-activity-visual--refresh-detail-page (&optional after)
  "Request one bounded coherent page for the selected run after AFTER."
  (e-board-activity-visual--cancel-work
   e-board-activity-visual--detail-request)
  (setq e-board-activity-visual--after after
        e-board-activity-visual--next nil
        e-board-activity-visual--detail-page nil
        e-board-activity-visual--detail-error nil)
  (if (not e-board-activity-visual--selected-run-id)
      (setq e-board-activity-visual--detail-request nil
            e-board-activity-visual--detail-state
            (if (and e-board-activity-visual--run-set-projection
                     (plist-get e-board-activity-visual--run-set-projection
                                :ready-p))
                'empty
              'loading))
    (setq e-board-activity-visual--detail-state 'loading)
    (e-board-activity-visual--schedule-push)
    (condition-case error
        (let* ((buffer (current-buffer))
               (target e-board-activity-visual--target)
               (run-id e-board-activity-visual--selected-run-id)
               (request
                (e-board-observation-activity-page-start
                 target :after after :run-id run-id
                 :limit e-board-observation-default-page-limit)))
          (setq e-board-activity-visual--detail-request request)
          (e-work-on-settle
           request
           (lambda (settled)
             (e-board-activity-visual--detail-page-settled
              buffer target run-id after request settled))))
      (error
       (setq e-board-activity-visual--detail-request nil
             e-board-activity-visual--detail-state 'error
             e-board-activity-visual--detail-error
             (error-message-string error))
       (e-board-activity-visual--schedule-push)))))

(defun e-board-activity-visual--selector-page-settled
    (buffer target binding epoch cursor request settled)
  "Install one indexed page after the matching selector EPOCH."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq request e-board-activity-visual--selector-work)
                 (eq target e-board-activity-visual--target)
                 (eq binding e-board-activity-visual--binding)
                 (= epoch e-board-activity-visual--run-set-epoch))
        (setq e-board-activity-visual--selector-work nil
              e-board-activity-visual--selector-loading nil)
        (let ((status (e-work-status settled)))
          (if (eq (plist-get status :state) 'finished)
              (let ((page (copy-tree (e-work-handle-result settled) t)))
                (if (and (equal (plist-get page :board-id)
                                (e-board-activity-visual--target-id))
                         (equal (plist-get page :cursor) cursor)
                         (integerp (plist-get page :generation))
                         (or (null cursor)
                             (= (plist-get page :generation)
                                (plist-get cursor :generation))))
                    (setq e-board-activity-visual--selector-page page
                          e-board-activity-visual--selector-error nil)
                  (setq e-board-activity-visual--selector-page nil
                        e-board-activity-visual--selector-error
                        "Run index response did not match this Board page")))
            (let* ((failure (or (plist-get status :error)
                                '(e-work-cancelled "cancelled")))
                   (error-text (e-work-error-message failure)))
              (if (and cursor
                       (or (eq (car-safe failure)
                               'e-runtime-store-board-conflict)
                           (string-match-p
                            "Board run-index cursor is invalid or stale"
                            error-text)))
                  (e-board-activity-visual--start-selector-page nil)
                (setq e-board-activity-visual--selector-page nil
                      e-board-activity-visual--selector-error error-text))))
          (e-board-activity-visual--schedule-push buffer))))))

(defun e-board-activity-visual--start-selector-page (cursor)
  "Read one indexed active-run page after CURSOR, or the first page."
  (e-board-activity-visual--cancel-work
   e-board-activity-visual--selector-work)
  (e-board-activity-visual--advance-epoch)
  (setq e-board-activity-visual--selector-browsing t
        e-board-activity-visual--selector-cursor cursor
        e-board-activity-visual--selector-page nil
        e-board-activity-visual--selector-loading t
        e-board-activity-visual--selector-error nil)
  (e-board-activity-visual--schedule-push)
  (condition-case error
      (let* ((buffer (current-buffer))
             (target e-board-activity-visual--target)
             (binding e-board-activity-visual--binding)
             (epoch e-board-activity-visual--run-set-epoch)
             (request
              (e-board-sqlite-publication-target-orchestration-run-index-page-start
               target :cursor cursor :active-only t
               :limit e-board-activity-visual-page-limit)))
        (setq e-board-activity-visual--selector-work request)
        (e-work-on-settle
         request
         (lambda (settled)
           (e-board-activity-visual--selector-page-settled
            buffer target binding epoch cursor request settled))))
    (error
     (setq e-board-activity-visual--selector-work nil
           e-board-activity-visual--selector-loading nil
           e-board-activity-visual--selector-error
           (error-message-string error))
     (e-board-activity-visual--schedule-push))))

(defun e-board-activity-visual--run-set-updated
    (buffer target binding status)
  "Apply detached shared run-set STATUS to its current visual BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((projection (plist-get status :projection))
             (target-id (and (e-board-sqlite-publication-target-valid-p target)
                             (e-board-sqlite-publication-target-board-id target))))
        (when (and (eq target e-board-activity-visual--target)
                   (eq binding e-board-activity-visual--binding)
                   (equal target-id (e-board-activity-visual--target-id)))
          (if (equal (plist-get projection :board-id) target-id)
              (progn
                (e-board-activity-visual--advance-epoch)
                (e-board-activity-visual--cancel-work
                 e-board-activity-visual--selector-work)
                (setq e-board-activity-visual--selector-work nil
                      e-board-activity-visual--run-set-projection
                      (copy-tree projection t)
                      e-board-activity-visual--selector-error nil)
                (e-board-activity-visual--selected-run-from-projection)
                (if e-board-activity-visual--selector-browsing
                    (e-board-activity-visual--start-selector-page
                     e-board-activity-visual--selector-cursor)
                  (setq e-board-activity-visual--selector-loading nil))
                (e-board-activity-visual--refresh-detail-page))
            (setq e-board-activity-visual--selector-error
                  "Run-set update did not match this Board"))
          (e-board-activity-visual--schedule-push buffer))))))

(defun e-board-activity-visual--payload-field (payload key)
  "Return KEY from egui PAYLOAD supporting alists and plists."
  (let ((string-key (symbol-name key))
        (keyword-key (intern (concat ":" (symbol-name key)))))
    (cond
     ((and (listp (car-safe payload)) (assoc key payload))
      (cdr (assoc key payload)))
     ((and (listp (car-safe payload)) (assoc string-key payload))
      (cdr (assoc string-key payload)))
     ((and (listp payload) (plist-member payload keyword-key))
      (plist-get payload keyword-key))
     ((and (listp payload) (plist-member payload key))
      (plist-get payload key))
     ((and (fboundp 'emacs-egui-get-field) payload)
      (emacs-egui-get-field payload key)))))

(defun e-board-activity-visual--payload-current-p (payload)
  "Return non-nil when PAYLOAD belongs to this Board and run-set epoch."
  (let ((binding e-board-activity-visual--binding)
        (board-id (e-board-activity-visual--payload-field payload 'boardId))
        (epoch (e-board-activity-visual--payload-field payload 'runSetEpoch)))
    (and (e-chat-service-binding-p binding)
         (not (eq (e-chat-service-binding-lifecycle-state binding) 'retired))
         (equal board-id (e-board-activity-visual--target-id))
         (integerp epoch)
         (= epoch e-board-activity-visual--run-set-epoch))))

(defun e-board-activity-visual--page-current-p (payload run-id)
  "Return non-nil when page coordinates in PAYLOAD still match RUN-ID."
  (let ((page e-board-activity-visual--detail-page)
        (generation
         (e-board-activity-visual--payload-field payload 'generation))
        (revision
         (e-board-activity-visual--payload-field payload 'revision)))
    (and (eq e-board-activity-visual--detail-state 'ready)
         (equal run-id e-board-activity-visual--selected-run-id)
         (equal (plist-get page :board-id)
                (e-board-activity-visual--target-id))
         (equal (plist-get (plist-get page :run) :run-id) run-id)
         (integerp generation)
         (integerp revision)
         (= generation (plist-get page :generation))
         (= revision (plist-get page :revision)))))

(defun e-board-activity-visual--task-action-context (payload)
  "Return the current selected task and exact participant for PAYLOAD."
  (let* ((run-id
          (e-board-activity-visual--payload-field payload 'runId))
         (task-key
          (e-board-activity-visual--payload-field payload 'taskKey))
         (attempt
          (e-board-activity-visual--payload-field payload 'attempt))
         (participant-id
          (e-board-activity-visual--payload-field payload 'participantId))
         (identity (list :run-task run-id task-key attempt))
         (page e-board-activity-visual--detail-page)
         (task
          (and (stringp run-id) (stringp task-key)
               (integerp attempt) (>= attempt 0)
               (cl-find-if
                (lambda (candidate)
                  (equal identity
                         (e-board-activity-visual--page-task-id
                          run-id candidate)))
                (plist-get page :tasks))))
         (row (and task (plist-get task :participant-row)))
         (expected-task e-board-activity-visual--selected-task))
    (when (and (e-board-activity-visual--page-current-p payload run-id)
               (equal identity expected-task)
               task
               (e-board-activity-visual--page-has-task-p page identity)
               (stringp participant-id)
               (equal participant-id (plist-get task :participant-id))
               (equal participant-id (plist-get row :participant-id))
               (equal run-id (plist-get row :run-id))
               (equal task-key (plist-get row :task-key))
               (equal attempt (plist-get row :attempt)))
      (list :task task :participant-row row
            :participant-id participant-id
            :controls
            (e-board-activity-visual-view-model-task-controls
             (e-board-activity-visual--target-id) run-id task
             e-board-activity-visual--live)))))

(defun e-board-activity-visual--with-task-control
    (payload control function)
  "Call FUNCTION with PAYLOAD's task context when CONTROL is still valid."
  (let* ((context (e-board-activity-visual--task-action-context payload))
         (controls (plist-get context :controls)))
    (cond
     ((null context)
      (message "The selected Board task or participant is no longer current."))
     ((not (eq (alist-get control controls) t))
      (message "This Board participant control is no longer available."))
     (t (funcall function context)))))

(defun e-board-activity-visual--run-present-p (run-id)
  "Return non-nil when RUN-ID is in the current bounded selector value."
  (and (stringp run-id)
       (cl-some (lambda (run)
                  (equal (plist-get run :run-id) run-id))
                (if e-board-activity-visual--selector-browsing
                    (mapcar (lambda (entry) (plist-get entry :summary))
                            (plist-get e-board-activity-visual--selector-page
                                       :entries))
                  (plist-get e-board-activity-visual--run-set-projection
                             :runs)))))

(defun e-board-activity-visual--selector-action-current-p (payload)
  "Return non-nil when PAYLOAD names the visible indexed page."
  (or (not e-board-activity-visual--selector-browsing)
      (and e-board-activity-visual--selector-page
           (integerp (e-board-activity-visual--payload-field
                      payload 'pageGeneration))
           (= (e-board-activity-visual--payload-field
               payload 'pageGeneration)
              (plist-get e-board-activity-visual--selector-page :generation)))))

(defun e-board-activity-visual--handle-ui-action (payload)
  "Handle semantic UI action PAYLOAD after checking current Board identity."
  (let* ((action-text
          (e-board-activity-visual--payload-field payload 'action))
         (action (and (stringp action-text) (intern-soft action-text))))
    (when (e-board-activity-visual--payload-current-p payload)
      (pcase action
        ('browse-runs
         (when (and (not e-board-activity-visual--selector-browsing)
                    (not e-board-activity-visual--selector-loading)
                    (or (plist-get
                         e-board-activity-visual--run-set-projection :more-p)
                        (> (or (plist-get
                                e-board-activity-visual--run-set-projection
                                :omitted-count)
                               0)
                           0)))
           (e-board-activity-visual--start-selector-page nil)))
        ('next-runs
         (when (and (e-board-activity-visual--selector-action-current-p
                     payload)
                    (not e-board-activity-visual--selector-loading)
                    (plist-get e-board-activity-visual--selector-page
                               :next-cursor))
           (e-board-activity-visual--start-selector-page
            (plist-get e-board-activity-visual--selector-page
                       :next-cursor))))
        ('current-runs
         (when (and e-board-activity-visual--selector-browsing
                    (or (null e-board-activity-visual--selector-page)
                        (e-board-activity-visual--selector-action-current-p
                         payload)))
           (e-board-activity-visual--cancel-work
            e-board-activity-visual--selector-work)
           (e-board-activity-visual--advance-epoch)
           (setq e-board-activity-visual--selector-work nil
                 e-board-activity-visual--selector-browsing nil
                 e-board-activity-visual--selector-page nil
                 e-board-activity-visual--selector-cursor nil
                 e-board-activity-visual--selector-loading nil
                 e-board-activity-visual--selector-error nil)))
        ('select-run
         (let ((run-id
                (e-board-activity-visual--payload-field payload 'runId)))
           (when (and (stringp run-id)
                      (e-board-activity-visual--selector-action-current-p
                       payload)
                      (e-board-activity-visual--run-present-p run-id))
             (unless (equal run-id e-board-activity-visual--selected-run-id)
               (setq e-board-activity-visual--selected-run-id run-id
                     e-board-activity-visual--selected-task nil)
               (e-board-activity-visual--refresh-detail-page)))))
        ('select-task
         (let* ((run-id
                 (e-board-activity-visual--payload-field payload 'runId))
                (task-key
                 (e-board-activity-visual--payload-field payload 'taskKey))
                (attempt
                 (e-board-activity-visual--payload-field payload 'attempt))
                (identity (list :run-task run-id task-key attempt)))
           (when (and (e-board-activity-visual--page-current-p payload run-id)
                      (e-board-activity-visual--page-has-task-p
                       e-board-activity-visual--detail-page identity))
             (setq e-board-activity-visual--selected-task identity)
             (e-board-activity-visual--schedule-push))))
        ('next-participants
         (let ((run-id
                (e-board-activity-visual--payload-field payload 'runId))
               (cursor
                (e-board-activity-visual--payload-field payload 'cursor)))
           (when (and (stringp cursor)
                      (equal cursor e-board-activity-visual--next)
                      (e-board-activity-visual--page-current-p payload run-id))
             (e-board-activity-visual--refresh-detail-page cursor))))
        ('open-participant
         (let* ((run-id
                 (e-board-activity-visual--payload-field payload 'runId))
                (participant-id
                 (e-board-activity-visual--payload-field payload
                                                         'participantId))
                (row (and (stringp participant-id)
                          (cl-find participant-id
                                   (plist-get
                                    e-board-activity-visual--detail-page
                                    :participants)
                                   :test #'equal
                                   :key (lambda (entry)
                                          (plist-get entry :participant-id))))))
           (when (and row (e-board-activity-visual--page-current-p
                           payload run-id))
             (e-board-activity-shell-open-participant-chat
              row (e-board-activity-visual--target-id)
              e-board-activity-visual--live))))
        ('open-task-participant
         (e-board-activity-visual--with-task-control
          payload 'canOpenChat
          (lambda (context)
            (e-board-activity-shell-open-participant-chat
             (plist-get context :participant-row)
             (e-board-activity-visual--target-id)
             e-board-activity-visual--live))))
        ('steer-participant
         (e-board-activity-visual--with-task-control
          payload 'canSteer
          (lambda (context)
            (let ((prompt
                   (e-board-activity-visual--payload-field payload 'prompt))
                  (reason
                   (e-board-activity-visual--payload-field payload 'reason)))
              (unless (and (stringp prompt)
                           (not (string-empty-p (string-trim prompt))))
                (user-error "Steer prompt cannot be empty"))
              (e-subagent-steer
               e-board-activity-visual--live
               (e-board-activity-visual--target-id)
               e-board-activity-visual--target
               (plist-get context :participant-id)
               prompt
               (and (stringp reason)
                    (not (string-empty-p (string-trim reason)))
                    reason))))))
        ('send-participant
         (e-board-activity-visual--with-task-control
          payload 'canSend
          (lambda (context)
            (let ((prompt
                   (e-board-activity-visual--payload-field payload 'prompt)))
              (unless (and (stringp prompt)
                           (not (string-empty-p (string-trim prompt))))
                (user-error "Send prompt cannot be empty"))
              (e-subagent-send
               e-board-activity-visual--live
               (e-board-activity-visual--target-id)
               (plist-get context :participant-id) prompt)))))
        ('interrupt-participant
         (e-board-activity-visual--with-task-control
          payload 'canInterrupt
          (lambda (context)
            (e-subagent-interrupt
             e-board-activity-visual--live
             (e-board-activity-visual--target-id)
             e-board-activity-visual--target
             (plist-get context :participant-id)))))
        ('shutdown-participant
         (e-board-activity-visual--with-task-control
          payload 'canShutdown
          (lambda (context)
            (e-subagent-shutdown
             e-board-activity-visual--live
             (e-board-activity-visual--target-id)
             e-board-activity-visual--target
             (plist-get context :participant-id)))))
        (_ nil)))
    (e-board-activity-visual--schedule-push)))

(defun e-board-activity-visual--wire-actions (session buffer)
  "Register SESSION's UI action callback for BUFFER."
  (emacs-egui-on session "ui-action"
                 (lambda (payload)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (e-board-activity-visual--handle-ui-action payload))))))

(defun e-board-activity-visual--cleanup ()
  "Retire current subscriptions and detached requests when the buffer closes."
  (e-board-activity-visual--retire-current))

(cl-defun e-board-activity-visual-open-buffer
    (&key target binding (live e-subagent-actions-default-live) run-id)
  "Open the egui Board activity renderer for TARGET and BINDING.
RUN-ID selects the detailed activity page while BINDING supplies the shared
Board run-set observer."
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (unless (and (e-chat-service-binding-p binding)
               (equal (e-board-sqlite-publication-target-board-id target)
                      (e-chat-service-binding-board-id binding)))
    (signal 'wrong-type-argument
            (list 'board-binding-matching-target-p binding target)))
  (e-board-activity-visual--ensure-runtime)
  (let* ((existing (get-buffer e-board-activity-visual-buffer-name))
         (existing-session
          (and existing
               (buffer-local-value
                'e-board-activity-visual--egui-session existing)))
         (session
          (or existing-session
              (emacs-egui-create-buffer
               :app-name e-board-activity-visual--app-name
               :buffer-name
               (if existing
                   (generate-new-buffer-name
                    e-board-activity-visual-buffer-name)
                 e-board-activity-visual-buffer-name))))
         (buffer (or (plist-get session :buffer)
                     (and existing-session existing))))
    (unless (buffer-live-p buffer)
      (error "emacs-egui did not create a live Board activity buffer"))
    (with-current-buffer buffer
      (e-board-activity-visual--retire-current)
      (setq-local e-board-activity-visual--target target
                  e-board-activity-visual--binding binding
                  e-board-activity-visual--live live
                  e-board-activity-visual--egui-session session
                  e-board-activity-visual--run-set-projection nil
                  e-board-activity-visual--selector-browsing nil
                  e-board-activity-visual--selector-page nil
                  e-board-activity-visual--selector-cursor nil
                  e-board-activity-visual--selector-loading nil
                  e-board-activity-visual--selector-error nil
                  e-board-activity-visual--selected-run-id run-id
                  e-board-activity-visual--selected-task nil
                  e-board-activity-visual--detail-state 'loading
                  e-board-activity-visual--detail-page nil
                  e-board-activity-visual--detail-error nil
                  e-board-activity-visual--after nil
                  e-board-activity-visual--next nil)
      (e-board-activity-visual--advance-epoch)
      (add-hook 'kill-buffer-hook
                #'e-board-activity-visual--cleanup nil t)
      (unless e-board-activity-visual--actions-wired
        (e-board-activity-visual--wire-actions session buffer)
        (setq e-board-activity-visual--actions-wired t))
      (e-board-activity-visual--push-snapshot buffer)
      (setq e-board-activity-visual--run-set-unsubscribe
            (e-board-run-set-subscribe
             binding
             (lambda (status)
               (e-board-activity-visual--run-set-updated
                buffer target binding status)))))
    (e-workspace-pop-to-buffer buffer)
    buffer))

(defun e-board-activity-visual--open-text-fallback
    (target run-id live reason)
  "Open the native text view for TARGET and RUN-ID after REASON."
  (message (concat "Visual Board activity unavailable: %s. Open the "
                   "native text view with M-x "
                   "e-chat-open-board-activity-text.")
           reason)
  (e-board-activity-shell-open-buffer :target target :live live
                                      :run-id run-id))

(defun e-board-activity-visual-open-or-text
    (target binding run-id &optional live)
  "Open the visual view when available, falling back to native text on failure."
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (setq live (or live e-subagent-actions-default-live))
  (if-let* ((reason (e-board-activity-visual-unavailable-reason)))
      (e-board-activity-visual--open-text-fallback
       target run-id live reason)
    (condition-case error
        (e-board-activity-visual-open-buffer
         :target target :binding binding :live live :run-id run-id)
      (error
       (e-board-activity-visual--open-text-fallback
        target run-id live
        (format "visual renderer failed to open: %s"
                (error-message-string error)))))))

(provide 'e-board-activity-visual-shell)

;;; e-board-activity-visual-shell.el ends here
