;;; e-chat-overview.el --- Chat overview presentation owner -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns session rows, read markers, workspace unread projection, previews, and
;; overview/sidebar commands.  It consumes session and service APIs while the
;; facade retains chat-session command composition.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-chat-service)
(require 'e-chat-surface)
(require 'e-chat-transcript)
(require 'e-context-status)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-picker)
(require 'e-ui-work)
(require 'e-work)
(require 'e-workspaces)

(defcustom e-chat-overview-buffer-name "*e-chat-overview*"
  "Buffer name for the chat session overview."
  :type 'string
  :group 'e-chat)

(defcustom e-chat-resume-preview-message-limit 2
  "Maximum number of transcript messages rendered in resume previews."
  :type 'integer
  :group 'e-chat)

(defconst e-chat-overview--resume-preview-buffer-name
  "*e-chat-resume-preview*"
  "Reusable buffer name for overview resume candidate previews.")

(declare-function e-chat-surface-selected-chat-surface "e-chat-surface")
(declare-function e-picker-open "e-picker")
(declare-function e-picker-make-line "e-picker")
(declare-function e-context-status-text "e-context-status")

(defvar e-chat-overview--workspace-unread-counts (make-hash-table :test #'equal)
  "Cached unread chat-buffer count by workspace display name.")
(defvar e-chat-overview--workspace-unread-buffer-state (make-hash-table :test #'eq)
  "Cached unread state by chat buffer.")
(defvar e-chat-overview--workspace-unread-cache-valid-p nil
  "Non-nil when workspace unread cache reflects live chat buffers.")
(defvar e-chat-overview--read-markers (make-hash-table :test #'eq)
  "Process-local read markers keyed by harness object.")
(defvar-local e-chat-overview--harness nil
  "Harness whose sessions are rendered in this overview buffer.")
(defvar-local e-chat-overview--subscription nil
  "Harness event subscription for this overview buffer.")
(defvar-local e-chat-overview--subscriptions nil
  "Harness event subscriptions for multi-instance overview buffers.")
(defvar-local e-chat-overview--page-work nil
  "Request-scoped work currently populating this overview buffer.")
(defvar-local e-chat-overview--page-generation 0
  "Presentation generation fencing stale overview page settlements.")
(defvar-local e-chat-overview--displayed-candidates nil
  "Detached candidates represented by the currently displayed bounded page.")
(defvar-local e-chat-overview--page-loaded-p nil
  "Non-nil after this overview has rendered a settled bounded page.")
(defvar e-chat-harness nil)
(defvar e-chat-session-id nil)
(defvar e-chat-harness-instance-id nil)
(defvar e-chat-default-harness-id)

(cl-defstruct (e-chat-overview-page-operation
               (:constructor e-chat-overview--page-operation-create))
  work groups children results pending error settled)

(cl-defstruct (e-chat-overview-page-group
               (:constructor e-chat-overview--page-group-create))
  store harness descriptors page)

(defconst e-chat-overview--page-operation-spec
  (e-work-spec-create
   :id "chat-overview-session-page" :execution 'cooperative
   :interactive-policy 'async :owner 'e-chat-overview
   :runner (lambda (_work _operation _context) :deferred)))

(defun e-chat-overview--make-mode-map (&optional map open-command)
  "Return MAP configured for `e-chat-overview-mode'.

OPEN-COMMAND is the shell command used to compose a selected chat session.
When it is nil, the owner installs its selection-only command.  The facade
supplies its session-opening command after composition, so this owner never
depends on the facade."
  (let ((map (or map (make-sparse-keymap))))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") (or open-command
                                     #'e-chat-overview-select-session))
    (define-key map (kbd "o") (or open-command
                                    #'e-chat-overview-select-session))
    (define-key map (kbd "v") #'e-chat-overview-preview-session)
    (define-key map (kbd "j") #'e-chat-overview-next-session)
    (define-key map (kbd "k") #'e-chat-overview-previous-session)
    (define-key map (kbd "g") #'e-chat-overview-refresh)
    (define-key map (kbd "q") #'e-chat-overview-close)
    map))

(defvar e-chat-overview-mode-map
  (e-chat-overview--make-mode-map)
  "Keymap for `e-chat-overview-mode'.")

(defun e-chat-overview--harness-has-capability-p (harness capability-id)
  "Return non-nil when HARNESS has active CAPABILITY-ID."
  (memq capability-id
        (mapcar #'e-capability-id
                (e-chat-service-active-capabilities harness))))

(defun e-chat-overview--chat-instances ()
  "Return configured chat harness instances for overview rows."
  (e-harness-instance-list :kind 'chat))

(defun e-chat-overview--default-chat-instance ()
  "Return the configured default chat instance, or nil."
  (e-harness-instance-get e-chat-default-harness-id))

(defun e-chat-overview--harness-for-instance (instance)
  "Return the live chat harness represented by INSTANCE."
  (let* ((instance-id (e-harness-instance-id instance))
         (harness
          (condition-case err
              (e-harness-instance-get-or-create instance-id)
            ((e-harness-instance-missing e-harness-registry-missing)
             (user-error "No e harness registered for %S" (cadr err))))))
    (unless (e-chat-overview--harness-has-capability-p harness 'chat-session)
      (user-error "Harness instance %S does not provide chat-session capability"
                  instance-id))
    harness))

(defun e-chat-overview--default-harness ()
  "Return the configured default chat harness for overview operations."
  (let* ((instance (e-chat-overview--default-chat-instance))
         (harness
          (if instance
              (e-chat-overview--harness-for-instance instance)
            (condition-case err
                (e-harness-registry-get-or-create e-chat-default-harness-id)
              (e-harness-registry-missing
               (user-error "No e harness registered for %S" (cadr err)))))))
    (unless (e-chat-overview--harness-has-capability-p harness 'chat-session)
      (user-error "Harness %S does not provide chat-session capability"
                  e-chat-default-harness-id))
    harness))

(defun e-chat-overview--board-session-p (session)
  "Return non-nil when SESSION carries board-native identity."
  (or (plist-get (plist-get session :association) :board-id)
      (plist-get session :board-id)))

(defun e-chat-overview--short-session-id (session-id)
  "Return a compact display id for SESSION-ID."
  (if (> (length session-id) 12)
      (substring session-id 0 12)
    session-id))

(defun e-chat-overview--session-owner-instance-id (_harness session)
  "Return persisted chat harness instance owner for SESSION, or nil."
  (plist-get (plist-get session :metadata) :harness-instance-id))

(defun e-chat-overview--session-belongs-to-instance-p
    (harness session instance-id default-instance-id shared-store-p)
  "Return non-nil when SESSION should be listed under INSTANCE-ID."
  (let ((owner (e-chat-overview--session-owner-instance-id harness session)))
    (if owner
        (equal owner instance-id)
      (or (not shared-store-p)
          (equal instance-id default-instance-id)))))

(defun e-chat-overview--shared-session-store-p (harness store-counts)
  "Return non-nil when HARNESS shares its session store in STORE-COUNTS."
  (> (or (gethash (e-chat-service-session-store harness) store-counts) 0) 1))

(defun e-chat-overview--session-candidate-newer-p (left right)
  "Return non-nil when session LEFT sorts before RIGHT."
  (let ((left-time (or (plist-get left :last-message-at)
                       (plist-get left :created-at) ""))
        (right-time (or (plist-get right :last-message-at)
                        (plist-get right :created-at) ""))
        (left-seq (or (plist-get left :updated-seq) 0))
        (right-seq (or (plist-get right :updated-seq) 0)))
    (or (string> left-time right-time)
        (and (string= left-time right-time)
             (> left-seq right-seq)))))

(defun e-chat-overview--page-descriptors (&optional harness)
  "Return bounded harness descriptors for one overview page request."
  (if harness
      (list (list :harness harness))
    (let ((instances (e-chat-overview--chat-instances)))
      (if instances
          (mapcar
           (lambda (instance)
             (list :instance instance
                   :instance-id (e-harness-instance-id instance)
                   :harness (e-chat-overview--harness-for-instance instance)))
           instances)
        (list (list :harness (e-chat-overview--default-harness)))))))

(defun e-chat-overview--page-groups (&optional harness)
  "Group one page request by authoritative session store."
  (let (groups)
    (dolist (descriptor (e-chat-overview--page-descriptors harness))
      (let* ((owner (plist-get descriptor :harness))
             (store (e-chat-service-session-store owner))
             (group
              (seq-find
               (lambda (candidate)
                 (eq store (e-chat-overview-page-group-store candidate)))
               groups)))
        (if group
            (setf (e-chat-overview-page-group-descriptors group)
                  (append (e-chat-overview-page-group-descriptors group)
                          (list descriptor)))
          (setq group
                (e-chat-overview--page-group-create
                 :store store :harness owner
                 :descriptors (list descriptor)))
          (setq groups (append groups (list group))))))
    groups))

(defun e-chat-overview--page-default-instance-id ()
  "Return the configured default chat instance identity, or nil."
  (when-let* ((instance
               (or (e-chat-overview--default-chat-instance)
                   (e-harness-instance-default :kind 'chat))))
    (e-harness-instance-id instance)))

(defun e-chat-overview--page-row-descriptor (group session default-instance-id)
  "Return GROUP descriptor owning detached SESSION summary."
  (let* ((descriptors (e-chat-overview-page-group-descriptors group))
         (owner (e-chat-overview--session-owner-instance-id
                 (e-chat-overview-page-group-harness group) session)))
    (cond
     (owner
      (seq-find
       (lambda (descriptor)
         (equal owner (plist-get descriptor :instance-id)))
       descriptors))
     ((= (length descriptors) 1) (car descriptors))
     (t
      (seq-find
       (lambda (descriptor)
         (equal default-instance-id (plist-get descriptor :instance-id)))
       descriptors)))))

(defun e-chat-overview--page-operation-candidates (operation)
  "Return sorted detached candidates assembled for OPERATION."
  (let ((default-instance-id (e-chat-overview--page-default-instance-id))
        candidates)
    (dolist (group (e-chat-overview-page-operation-groups operation))
      (dolist (session (plist-get
                        (e-chat-overview-page-group-page group) :rows))
        (when (e-chat-overview--board-session-p session)
          (when-let* ((descriptor
                       (e-chat-overview--page-row-descriptor
                        group session default-instance-id)))
            (push (list :instance (plist-get descriptor :instance)
                        :instance-id (plist-get descriptor :instance-id)
                        :harness (plist-get descriptor :harness)
                        :session session
                        :session-id (plist-get session :id))
                  candidates)))))
    (sort candidates
          (lambda (left right)
            (e-chat-overview--session-candidate-newer-p
             (plist-get left :session)
             (plist-get right :session))))))

(defun e-chat-overview--settle-page-operation (operation)
  "Settle OPERATION after all bounded SQLite page children finish."
  (unless (e-chat-overview-page-operation-settled operation)
    (setf (e-chat-overview-page-operation-settled operation) t)
    (if-let* ((error (e-chat-overview-page-operation-error operation)))
        (e-work-fail (e-chat-overview-page-operation-work operation) error)
      (e-work-finish
       (e-chat-overview-page-operation-work operation)
       (e-chat-overview--page-operation-candidates operation)))))

(defun e-chat-overview--page-child-settled (operation group child)
  "Record GROUP's settled CHILD for request-local OPERATION."
  (unless (e-chat-overview-page-operation-settled operation)
    (let ((status (e-work-status child)))
      (pcase (plist-get status :state)
        ('finished
         (condition-case error
             (setf (e-chat-overview-page-group-page group)
                   (e-chat-service-root-session-page-value child))
           (error
            (setf (e-chat-overview-page-operation-error operation) error))))
        ('failed
         (setf (e-chat-overview-page-operation-error operation)
               (plist-get status :error)))
        ('cancelled
         (setf (e-chat-overview-page-operation-error operation)
               '(e-work-cancelled "Session page request cancelled"))))
      (setf (e-chat-overview-page-operation-pending operation)
            (1- (e-chat-overview-page-operation-pending operation)))
      (when (or (e-chat-overview-page-operation-error operation)
                (zerop (e-chat-overview-page-operation-pending operation)))
        (when (e-chat-overview-page-operation-error operation)
          (dolist (pending (e-chat-overview-page-operation-children operation))
            (unless (memq (plist-get (e-work-status pending) :state)
                          '(finished failed cancelled))
              (e-work-cancel pending))))
        (e-chat-overview--settle-page-operation operation)))))

(defun e-chat-overview-session-candidates-start (&optional harness)
  "Return immediately with work reading one displayed session-summary page.

HARNESS restricts the page to one harness.  With nil HARNESS, configured chat
instances sharing a store share one SQLite page request; the detached rows are
assigned to their presentation owner after settlement."
  (let* ((groups (e-chat-overview--page-groups harness))
         (operation
          (e-chat-overview--page-operation-create
           :groups groups :results (make-hash-table :test 'eq)
           :pending (length groups)))
         (work
          (e-work-prepare
           e-chat-overview--page-operation-spec operation
           :context (list :domain-ref 'chat-overview
                          :work-kind 'session-summary-page))))
    (setf (e-chat-overview-page-operation-work operation) work)
    (e-work-start-prepared work :arguments operation)
    (if (null groups)
        (e-chat-overview--settle-page-operation operation)
      (dolist (group groups)
        (let* ((settled-group group)
               (child
               (e-chat-service-root-session-page-start
                (e-chat-overview-page-group-harness group))))
          (setf (e-chat-overview-page-operation-children operation)
                (append (e-chat-overview-page-operation-children operation)
                        (list child)))
          (e-work-on-settle
           child
           (lambda (settled)
             (e-chat-overview--page-child-settled
              operation settled-group settled))))))
    (setf (e-work-handle-cancel-function work)
          (lambda (_handle)
            (dolist (child (e-chat-overview-page-operation-children operation))
              (unless (memq (plist-get (e-work-status child) :state)
                            '(finished failed cancelled))
                (e-work-cancel child)))))
    work))

(defun e-chat-overview--invalidate-unread-cache ()
  "Mark the workspace unread projection stale."
  (setq e-chat-overview--workspace-unread-cache-valid-p nil))

(defvar-local e-chat-overview--disabling-modal-editing nil
  "Non-nil while overview mode is disabling modal editing.")

(defun e-chat-overview--disable-modal-editing ()
  "Keep Evil from intercepting overview navigation commands."
  (unless e-chat-overview--disabling-modal-editing
    (setq-local e-chat-overview--disabling-modal-editing t)
    (unwind-protect
        (progn
          (when (fboundp 'evil-local-mode)
            (evil-local-mode -1))
          (when (boundp 'evil-local-mode)
            (setq-local evil-local-mode nil))
          (when (boundp 'evil-state)
            (setq-local evil-state nil)))
      (setq-local e-chat-overview--disabling-modal-editing nil))))

(define-derived-mode e-chat-overview-mode special-mode "e-chat-overview"
  "Major mode for the e chat session overview."
  (add-hook 'kill-buffer-hook #'e-chat-overview--unsubscribe nil t)
  (add-hook 'evil-local-mode-hook #'e-chat-overview--disable-modal-editing nil t)
  (e-chat-overview--disable-modal-editing)
  (buffer-disable-undo)
  (setq-local truncate-lines t))

(defun e-chat-overview--read-marker-key (&optional instance-id)
  "Return the process-local read marker key for INSTANCE-ID."
  (cond
   ((null instance-id) "__default__")
   ((stringp instance-id) instance-id)
   ((keywordp instance-id) (substring (symbol-name instance-id) 1))
   ((symbolp instance-id) (symbol-name instance-id))
   (t (prin1-to-string instance-id))))

(defun e-chat-overview--read-marker-table (harness)
  "Return process-local read-marker table for HARNESS."
  (or (gethash harness e-chat-overview--read-markers)
      (let ((table (make-hash-table :test #'equal)))
        (puthash harness table e-chat-overview--read-markers)
        table)))

(defun e-chat-overview--session-read-marker
    (harness session &optional instance-id)
  "Return SESSION's process-local read marker for INSTANCE-ID."
  (gethash
   (cons (plist-get session :id)
         (e-chat-overview--read-marker-key instance-id))
   (e-chat-overview--read-marker-table harness)))

(defun e-chat-overview--set-session-read-marker
    (harness session-id marker &optional instance-id)
  "Store SESSION-ID read MARKER in process-local presentation state."
  (puthash
   (cons session-id (e-chat-overview--read-marker-key instance-id))
   marker
   (e-chat-overview--read-marker-table harness)))

(defun e-chat-overview--read-marker
    (session-id &optional harness instance-id)
  "Return the stored read marker for SESSION-ID in HARNESS."
  (when-let* ((target-harness (or harness
                                  e-chat-overview--harness
                                  (e-chat-overview--default-harness))))
    (gethash
     (cons session-id (e-chat-overview--read-marker-key instance-id))
     (e-chat-overview--read-marker-table target-harness))))

(defun e-chat-overview--set-read-marker
    (session-id marker &optional harness instance-id)
  "Set SESSION-ID read marker to MARKER in HARNESS."
  (when-let* ((target-harness (or harness
                                 e-chat-overview--harness
                                 (e-chat-overview--default-harness))))
    (e-chat-overview--set-session-read-marker
     target-harness
     session-id
     marker
     instance-id)))

(defun e-chat-overview--latest-assistant-marker (harness session)
  "Return SESSION's detached latest-assistant marker."
  (ignore harness)
  (plist-get session :latest-assistant-marker))

(defun e-chat-overview--session-unread-p (harness session &optional instance-id)
  "Return non-nil when SESSION has unread assistant output in HARNESS."
  (when-let* ((marker (e-chat-overview--latest-assistant-marker harness session)))
    (not (equal marker
                (e-chat-overview--session-read-marker
                 harness session instance-id)))))

(defun e-chat-overview--workspace-name (workspace)
  "Return display name for WORKSPACE token or string."
  (cond
   ((e-workspace-token-p workspace)
    (format "%s" (or (e-workspace-token-name workspace)
                     (e-workspace-token-id workspace))))
   ((stringp workspace) workspace)
   ((null workspace)
    (e-chat-overview--workspace-name (e-workspace-current)))
   (t (format "%s" workspace))))

(defun e-chat-overview--workspace-match-p (buffer-workspace workspace)
  "Return non-nil when BUFFER-WORKSPACE matches WORKSPACE."
  (cond
   ((e-workspace-token-p workspace)
    (e-workspace-equal-p buffer-workspace workspace))
   ((e-workspace-token-p buffer-workspace)
    (equal (e-chat-overview--workspace-name buffer-workspace)
           (e-chat-overview--workspace-name workspace)))
   (t nil)))

(defun e-chat-overview--buffer-unread-p (buffer)
  "Return non-nil when chat BUFFER has unread assistant output."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (or (e-chat-surface-transcript-p)
               (e-chat-surface-composer-p))
           (not (e-chat-transcript-preview-p))
           e-chat-harness
           e-chat-session-id
           (when-let* ((marker
                        (plist-get e-chat-session-metadata
                                   :latest-assistant-marker))
                       (session
                        (list :id e-chat-session-id
                              :latest-assistant-marker marker)))
             (e-chat-overview--session-unread-p
              e-chat-harness
              session
              e-chat-harness-instance-id))))))

(defun e-chat-overview--workspace-unread-cache-adjust (workspace delta)
  "Adjust cached unread count for WORKSPACE by DELTA."
  (let* ((current (gethash workspace e-chat-overview--workspace-unread-counts 0))
         (next (+ current delta)))
    (if (> next 0)
        (puthash workspace next e-chat-overview--workspace-unread-counts)
      (remhash workspace e-chat-overview--workspace-unread-counts))))

(defun e-chat-overview--workspace-unread-cache-remove-buffer (&optional buffer)
  "Remove BUFFER's previous unread contribution from the workspace cache."
  (let* ((buffer (or buffer (current-buffer)))
         (previous (gethash buffer e-chat-overview--workspace-unread-buffer-state)))
    (when (and previous (cdr previous))
      (e-chat-overview--workspace-unread-cache-adjust (car previous) -1))
    (remhash buffer e-chat-overview--workspace-unread-buffer-state)))

(defun e-chat-overview--workspace-unread-cache-update-buffer (&optional buffer)
  "Refresh BUFFER's unread contribution when the workspace cache is valid."
  (when e-chat-overview--workspace-unread-cache-valid-p
    (let* ((buffer (or buffer (current-buffer)))
           (workspace (and (buffer-live-p buffer)
                           (e-buffer-workspace buffer)))
           (workspace-name (and workspace
                                (e-chat-overview--workspace-name workspace)))
           (unread (and workspace-name
                        (e-chat-overview--buffer-unread-p buffer))))
      (e-chat-overview--workspace-unread-cache-remove-buffer buffer)
      (when workspace-name
        (puthash buffer
                 (cons workspace-name unread)
                 e-chat-overview--workspace-unread-buffer-state)
        (when unread
          (e-chat-overview--workspace-unread-cache-adjust workspace-name 1))))))

(defun e-chat-overview--workspace-unread-cache-rebuild ()
  "Rebuild cached unread chat-buffer counts by workspace."
  (clrhash e-chat-overview--workspace-unread-counts)
  (clrhash e-chat-overview--workspace-unread-buffer-state)
  (setq e-chat-overview--workspace-unread-cache-valid-p t)
  (dolist (buffer (buffer-list))
    (e-chat-overview--workspace-unread-cache-update-buffer buffer)))

;;;###autoload
(defun e-chat-workspace-unread-p (&optional workspace)
  "Return non-nil when WORKSPACE owns any unread e chat buffer.
WORKSPACE may be an `e-workspace-token', a workspace name string, or nil for
the current workspace."
  (unless e-chat-overview--workspace-unread-cache-valid-p
    (e-chat-overview--workspace-unread-cache-rebuild))
  (> (gethash (e-chat-overview--workspace-name workspace)
              e-chat-overview--workspace-unread-counts
              0)
     0))

;;;###autoload
(defun e-chat-workspace-unread-indicator (&optional workspace)
  "Return a propertized unread marker for WORKSPACE, or nil."
  (when (e-chat-workspace-unread-p workspace)
    (propertize "●" 'font-lock-face 'e-chat-workspace-unread-face)))

(defun e-chat-overview--session-id-at-point ()
  "Return overview session id at point, or nil."
  (or (get-text-property (point) 'e-chat-session-id)
      (get-text-property (line-beginning-position) 'e-chat-session-id)))

(defun e-chat-overview--instance-id-at-point ()
  "Return overview harness instance id at point, or nil."
  (or (get-text-property (point) 'e-chat-harness-instance-id)
      (get-text-property (line-beginning-position)
                         'e-chat-harness-instance-id)))

(defun e-chat-overview--compact-row-text (text max-chars)
  "Return TEXT as a single overview row fragment capped at MAX-CHARS."
  (when (stringp text)
    (let ((lines (split-string (replace-regexp-in-string "\r" "" text) "\n"))
          compact)
      (while (and lines (not compact))
        (let* ((line (pop lines))
               (line (replace-regexp-in-string
                      "</?reference\\b[^>]*>" "" line))
               (line (string-trim line)))
          (unless (or (string-empty-p line)
                      (string-match-p "</?reference\\b" line)
                      (string= line "References:")
                      (string-match-p "\\`\\[[^]]+\\]" line))
            (setq compact (replace-regexp-in-string "[ \t]+" " " line)))))
      (when compact
        (if (> (length compact) max-chars)
            (concat (substring compact 0 max-chars) "...")
          compact)))))

(defun e-chat-overview--compact-timestamp (timestamp)
  "Return TIMESTAMP in compact sidebar form."
  (if (and (stringp timestamp)
           (string-match
            "\\`[0-9]\\{4\\}-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T\\([0-9][0-9]\\):\\([0-9][0-9]\\)"
            timestamp))
      (format "%s-%s %s:%s"
              (match-string 1 timestamp)
              (match-string 2 timestamp)
              (match-string 3 timestamp)
              (match-string 4 timestamp))
    timestamp))

(defun e-chat-overview--insert-faced (text face)
  "Insert TEXT with FONT-LOCK FACE."
  (let ((start (point)))
    (insert text)
    (add-text-properties start (point) `(font-lock-face ,face))))

(defun e-chat-overview--summary-duplicates-title-p (summary title)
  "Return non-nil when SUMMARY is already represented by TITLE."
  (and (stringp summary)
       (stringp title)
       (let ((prefix (if (string-suffix-p "..." title)
                         (string-remove-suffix "..." title)
                       title)))
         (or (string= summary title)
             (and (not (string-empty-p prefix))
                  (string-prefix-p prefix summary))))))

(defun e-chat-overview--insert-session-row
    (harness session &optional instance show-instance)
  "Insert one overview row for SESSION from HARNESS.
INSTANCE is the owning chat harness instance when available.  SHOW-INSTANCE
adds its display name to the row."
  (let* ((session-id (plist-get session :id))
         (instance-id (and instance
                           (e-harness-instance-id instance)))
         (summary (e-chat-overview--compact-row-text
                   (plist-get session :summary)
                   72))
         (title (or (e-chat-overview--compact-row-text
                     (plist-get session :title)
                     48)
                    summary
                    session-id))
         (message-count (or (plist-get session :message-count) 0))
         (last-message-at (or (plist-get session :last-message-at)
                              (plist-get session :created-at)))
         (metadata (string-join
                    (delq nil
                          (list (format "[%s]"
                                        (e-chat-overview--short-session-id session-id))
                                (when (> message-count 0)
                                  (format "%d %s"
                                          message-count
                                          (if (= message-count 1)
                                              "msg"
                                            "msgs")))
                                (e-chat-overview--compact-timestamp
                                 last-message-at)))
                    "  "))
         (unread (e-chat-overview--session-unread-p
                  harness session instance-id))
         (start (point)))
    (e-chat-overview--insert-faced (if unread "! " "  ")
                                   (if unread
                                       'e-chat-overview-unread-face
                                     'e-chat-overview-meta-face))
    (when (and show-instance instance)
      (e-chat-overview--insert-faced
       (format "%s  " (e-harness-instance-name instance))
       'e-chat-overview-meta-face))
    (e-chat-overview--insert-faced title 'e-chat-overview-title-face)
    (insert "\n  ")
    (e-chat-overview--insert-faced metadata 'e-chat-overview-meta-face)
    (when (and summary
               (not (e-chat-overview--summary-duplicates-title-p
                     summary title)))
      (insert "\n  ")
      (e-chat-overview--insert-faced summary 'e-chat-overview-summary-face))
    (insert "\n\n")
    (add-text-properties start (point)
                         `(e-chat-session-id ,session-id
                           e-chat-harness-instance-id ,instance-id
                           help-echo "RET opens this e chat session"))))

(defun e-chat-overview--active-session-title (session)
  "Return display title for active SESSION."
  (or (e-chat-overview--compact-row-text (plist-get session :title) 52)
      (e-chat-overview--compact-row-text (plist-get session :summary) 52)
      (plist-get session :id)))

(defun e-chat-overview--active-session-candidate-key (candidate)
  "Return filter key for active session CANDIDATE."
  (let* ((session (plist-get candidate :session))
         (instance (plist-get candidate :instance)))
    (string-join
     (delq nil
           (list (plist-get session :title)
                 (plist-get session :summary)
                 (plist-get session :id)
                 (and instance
                      (e-harness-instance-name instance))))
     " ")))

(defun e-chat-overview--active-session-user-prompt-p (message)
  "Return non-nil when MESSAGE is a user prompt."
  (and (eq (plist-get message :role) 'user)
       (when-let* ((content (plist-get message :content)))
         (and (stringp content)
              (not (string-empty-p (string-trim content)))))))

(defun e-chat-overview--active-session-preview-message-p (message)
  "Return non-nil when MESSAGE belongs in the active-session preview."
  (and (memq (plist-get message :role) '(user assistant))
       (when-let* ((content (plist-get message :content)))
         (and (stringp content)
              (not (string-empty-p (string-trim content)))))))

(defun e-chat-overview--active-session-has-prompt-p (candidate)
  "Return non-nil when CANDIDATE has at least one user prompt."
  (let* ((session (plist-get candidate :session))
         (message-count (plist-get session :message-count)))
    (or (cl-some #'e-chat-overview--active-session-user-prompt-p
                 (plist-get session :messages))
        (and (integerp message-count)
             (> message-count 0)
             (when-let* ((summary (plist-get session :summary)))
               (and (stringp summary)
                    (not (string-empty-p (string-trim summary)))))))))

(defun e-chat-overview--active-session-active-p (harness session-id)
  "Return non-nil when SESSION-ID has an active turn in HARNESS."
  (and harness
       session-id
       (ignore-errors (e-chat-service-active-turn-p harness session-id))))

(defun e-chat-overview--active-session-state (candidate)
  "Return read state for active-session CANDIDATE."
  (let* ((harness (plist-get candidate :harness))
         (session (plist-get candidate :session))
         (session-id (plist-get candidate :session-id))
         (instance-id (plist-get candidate :instance-id)))
    (cond
     ((e-chat-overview--active-session-active-p harness session-id) 'active)
     ((ignore-errors
        (e-chat-overview--session-unread-p harness session instance-id))
      'unread)
     (t 'read))))

(defun e-chat-overview--active-session-indicator (state)
  "Return the left indicator for active-session STATE."
  (pcase state
    ('active
     (propertize "◆ "
                 'font-lock-face
                 'e-chat-overview-unread-face))
    ('unread
     (propertize "● "
                 'font-lock-face
                 'e-chat-overview-unread-face))
    (_ "  ")))

(defun e-chat-overview--active-session-status-key (candidate)
  "Return semantic context-status cache key for active-session CANDIDATE."
  (let* ((harness (plist-get candidate :harness))
         (session (plist-get candidate :session))
         (session-id (plist-get candidate :session-id))
         (state (ignore-errors
                  (and harness
                       session-id
                       (e-chat-service-state harness session-id))))
         (options (ignore-errors
                    (and harness
                         session-id
                         (e-harness-display-options harness session-id))))
         (usage-event nil))
    (list :session-id session-id
          :message-count (or (plist-get state :message-count)
                             (plist-get session :message-count))
          :active-turn (plist-get state :active-turn)
          :latest-token-usage-id (plist-get usage-event :id)
          :model (plist-get options :model)
          :reasoning-effort (plist-get options :reasoning-effort)
          :layers (ignore-errors
                    (and harness
                         session-id
                         (e-harness-effective-layer-ids harness session-id))))))

(defun e-chat-overview--active-session-status-cache-cell (cache key)
  "Return status snapshot cache cell from CACHE for KEY."
  (when (and cache key)
    (or (gethash key cache)
        (puthash key (cons nil nil) cache))))

(defun e-chat-overview--active-session-line (candidate &optional status-cache)
  "Return picker row text for active session CANDIDATE."
  (let* ((harness (plist-get candidate :harness))
         (session (plist-get candidate :session))
         (session-id (plist-get candidate :session-id))
         (instance (plist-get candidate :instance))
         (state (e-chat-overview--active-session-state candidate))
         (title (concat (e-chat-overview--active-session-indicator state)
                        (e-chat-overview--active-session-title session)))
         (message-count (or (plist-get session :message-count) 0))
         (timestamp (or (plist-get session :last-message-at)
                        (plist-get session :created-at)))
         (status-key (e-chat-overview--active-session-status-key candidate))
         (status-cache-cell
          (e-chat-overview--active-session-status-cache-cell status-cache status-key))
         (status (ignore-errors
                   (e-context-status-text
                    harness
                    session-id
                    :prefix "ctx"
                    :prefer-token-usage t
                    :estimate-context nil
                    :snapshot-cache status-cache-cell
                    :snapshot-cache-key
                    (list :status-key status-key
                          :prefer-token-usage t
                          :estimate-context nil)
                    :allow-stale-snapshot t)))
         (meta (string-join
                (delq nil
                      (list (and instance
                                 (e-harness-instance-name instance))
                            (when (> message-count 0)
                              (format "%d %s"
                                      message-count
                                      (if (= message-count 1)
                                          "msg"
                                        "msgs")))
                            (e-chat-overview--compact-timestamp timestamp)
                            status))
                "  ")))
    (e-picker-make-line title meta 96)))

(defun e-chat-overview--active-session-preview-messages (harness session)
  "Return messages to render for active-session preview of SESSION."
  (ignore harness)
  ;; Persistent candidates are detached query rows and may carry only their
  ;; bounded summary.  Preview must never reach behind that consumer-shaped
  ;; result to inspect or reconstruct a session aggregate.
  (copy-sequence (plist-get session :messages)))

(defun e-chat-overview--tail-messages (messages limit)
  "Return at most LIMIT trailing MESSAGES for an overview preview.

Overview owns preview sizing; it does not require the transcript owner's
replay representation helper merely to choose a bounded catalog preview."
  (if (and (integerp limit) (> limit 0) (> (length messages) limit))
      (nthcdr (- (length messages) limit) messages)
    messages))

(defun e-chat-overview--active-session-sanitize-preview-properties ()
  "Strip chat buffer structural properties from the active-session preview.
The picker owns row layout and selection state, so it must not inherit
`e-chat-mode' field, read-only, sticky, or block-navigation properties.  Keep
face properties so the preview still reflects chat rendering."
  (let ((position (point-min))
        next face font-lock-face)
    (while (< position (point-max))
      (setq next (next-property-change position nil (point-max)))
      (setq face (get-text-property position 'face))
      (setq font-lock-face (get-text-property position 'font-lock-face))
      (set-text-properties position next nil)
      (when face
        (put-text-property position next 'face face))
      (when font-lock-face
        (put-text-property position next 'font-lock-face font-lock-face))
        (setq position next))))

(defun e-chat-overview--initialize-preview-buffer
    (buffer harness session-id &optional read-only)
  "Initialize BUFFER as an isolated transcript preview for SESSION-ID.
The overview owns preview lifetime and layout; it must not invoke the composed
chat major mode merely to borrow transcript state.  TEXT-MODE supplies the
ordinary text presentation, while the surface/transcript ports establish only
the buffer-local identity needed by their renderers.  READ-ONLY is applied
after initialization so transient picker previews remain editable only at the
text-property boundary, matching the historical active-session preview, while
resume previews remain immutable."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (text-mode)
      (e-chat-surface-setup-line-wrapping)
      (e-chat-surface-mark-transcript)
      (setq-local e-chat-harness harness
                  e-chat-session-id session-id
                  cursor-type nil
                  header-line-format nil
                  buffer-read-only nil)
      (e-chat-transcript-cancel-markdown-presentation)
      (e-chat-transcript-reset)
      (e-chat-transcript-set-preview-p t)
      (setq buffer-read-only read-only))))

(defun e-chat-overview--active-session-preview (candidate buffer)
  "Render active session CANDIDATE into preview BUFFER."
  (let* ((harness (plist-get candidate :harness))
         (session (plist-get candidate :session))
         (session-id (plist-get candidate :session-id))
         (messages (cl-remove-if-not
                    #'e-chat-overview--active-session-preview-message-p
                    (e-chat-overview--active-session-preview-messages
                     harness session)))
         (summary-preview (e-chat-transcript-session-summary-preview session)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (e-chat-overview--initialize-preview-buffer
         buffer harness session-id)
        (erase-buffer)
        (cond
         (messages
          (e-chat-transcript-render-session
           (e-chat-overview--tail-messages
            messages
            e-chat-resume-preview-message-limit)
           nil))
         (summary-preview
          (e-chat-transcript-insert-entry "You" summary-preview nil))
         (t
          (e-chat-transcript-insert-protected "No prompts yet")))
        (e-chat-overview--active-session-sanitize-preview-properties)
        (ignore-errors
          (e-chat-overview--mark-session-read
           harness
           session
           (plist-get candidate :instance-id)))
        (goto-char (point-min))))))

(defun e-chat-overview--session-choice-label (session)
  "Return completion label for SESSION metadata."
  (format "%s  [%s]"
          (plist-get session :title)
          (plist-get session :id)))

(defun e-chat-overview--session-candidate-label (candidate &optional show-instance)
  "Return completion label for session CANDIDATE.
When SHOW-INSTANCE is non-nil, prefix the owning target label."
  (let ((session-label
         (e-chat-overview--session-choice-label
          (plist-get candidate :session))))
    (if (and show-instance (plist-get candidate :instance))
        (format "%s  %s"
                (e-harness-instance-name (plist-get candidate :instance))
                session-label)
      session-label)))

(defun e-chat-overview--candidate-for-label (candidates labels label)
  "Return session candidate from CANDIDATES matching LABELS LABEL."
  (when-let* ((index (cl-position label labels :test #'equal)))
    (nth index candidates)))

(defun e-chat-overview--consult-read-available-p ()
  "Return non-nil when Consult's previewing reader is available."
  (and (require 'consult nil t)
       (fboundp 'consult--read)))

(defun e-chat-overview--resume-preview-origin-window ()
  "Return the window that should display resume previews."
  (or (and (minibufferp)
           (window-live-p (minibuffer-selected-window))
           (minibuffer-selected-window))
      (selected-window)))

(defun e-chat-overview--session-preview-metadata-text (session)
  "Return bounded metadata text for a SESSION without loading its transcript."
  (string-join
   (delq nil
         (list (plist-get session :title)
               (e-chat-transcript-session-summary-preview session)
               (when-let* ((message-count (plist-get session :message-count)))
                 (format "%d messages" message-count))
               (plist-get session :last-message-at)))
   "\n\n"))

(defun e-chat-overview--render-resume-preview (harness session)
  "Render detached bounded SESSION metadata into the resume preview buffer."
  (let* ((session-id (plist-get session :id))
         (buffer (get-buffer-create
                  e-chat-overview--resume-preview-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (e-chat-overview--initialize-preview-buffer
         buffer harness session-id t)
        (e-chat-transcript-insert-protected
         (e-chat-overview--session-preview-metadata-text session))
        (setq buffer-read-only t)
        (goto-char (point-min))))
    buffer))

(defun e-chat-overview--resume-preview-state (candidates labels)
  "Return Consult preview state for resume CANDIDATES and LABELS."
  (let (origin-window origin-buffer preview-buffer)
    (cl-labels
        ((ensure-origin ()
           (unless (window-live-p origin-window)
             (setq origin-window
                   (e-chat-overview--resume-preview-origin-window))
             (setq origin-buffer (window-buffer origin-window))))
         (restore-origin (&optional kill-preview)
           (when (and (window-live-p origin-window)
                      (buffer-live-p origin-buffer))
             (with-selected-window origin-window
               (switch-to-buffer origin-buffer 'norecord)))
           (when (and kill-preview
                      (buffer-live-p preview-buffer))
             (kill-buffer preview-buffer)
             (setq preview-buffer nil))))
      (lambda (action candidate-label)
        (pcase action
          ('setup
           (ensure-origin))
          ('preview
           (ensure-origin)
           (if-let* ((candidate
                     (e-chat-overview--candidate-for-label
                      candidates labels candidate-label)))
               (when (window-live-p origin-window)
                 (setq preview-buffer
                       (e-chat-overview--render-resume-preview
                        (plist-get candidate :harness)
                        (plist-get candidate :session)))
                 (with-selected-window origin-window
                   (switch-to-buffer preview-buffer 'norecord)))
             (restore-origin)))
          ((or 'exit 'return)
           (restore-origin t)))))))

(defun e-chat-overview--read-session-candidate (candidates &optional prompt)
  "Read and return one session CANDIDATE from CANDIDATES."
  (let* ((show-instance (> (length (e-chat-overview--chat-instances)) 1))
         (labels (mapcar (lambda (candidate)
                           (e-chat-overview--session-candidate-label
                            candidate show-instance))
                         candidates))
         (selected
          (if (e-chat-overview--consult-read-available-p)
              (funcall (symbol-function 'consult--read)
                       labels
                       :prompt (or prompt "Resume e session: ")
                       :require-match t
                       :sort nil
                       :category 'e-chat-session
                       :state (e-chat-overview--resume-preview-state
                               candidates labels))
            (completing-read
             (or prompt "Resume e session: ")
             labels nil t))))
    (or (e-chat-overview--candidate-for-label candidates labels selected)
        (user-error "No e chat session selected"))))

(defun e-chat-overview--active-session-selection (candidate)
  "Return a semantic selection intent for active-session CANDIDATE.
The overview owner never constructs a chat buffer; the composition root consumes
the identity fields when it decides how to open the selected session."
  (list :candidate candidate
        :harness (plist-get candidate :harness)
        :session (plist-get candidate :session)
        :session-id (plist-get candidate :session-id)
        :instance-id (plist-get candidate :instance-id)))

(defun e-chat-overview-active-session-candidates (candidates)
  "Filter detached bounded CANDIDATES for the active-session picker."
  (cl-remove-if-not
   #'e-chat-overview--active-session-has-prompt-p
   candidates))

(defun e-chat-overview-active-session-candidate-key (candidate)
  "Return the search key for active-session CANDIDATE."
  (e-chat-overview--active-session-candidate-key candidate))

(defun e-chat-overview-active-session-line (candidate &optional status-cache)
  "Return picker line text for active-session CANDIDATE."
  (e-chat-overview--active-session-line candidate status-cache))

(defun e-chat-overview-active-session-preview (candidate buffer)
  "Render active-session CANDIDATE into picker preview BUFFER."
  (e-chat-overview--active-session-preview candidate buffer))

(defun e-chat-overview-session-selection ()
  "Return the overview row selection intent at point.
The composition root consumes this value to construct or attach a chat
surface; the overview owner never opens a chat buffer itself."
  (interactive)
  (e-chat-overview--row-target-at-point))

(defun e-chat-overview--render-candidates (candidates harness)
  "Render detached CANDIDATES for HARNESS into the current buffer."
  (let ((show-instance
         (> (length (delq nil
                          (delete-dups
                           (mapcar (lambda (candidate)
                                     (plist-get candidate :instance-id))
                                   candidates))))
            1))
        (inhibit-read-only t))
    (setq-local e-chat-overview--harness harness
                e-chat-overview--displayed-candidates
                (mapcar
                 (lambda (candidate)
                   ;; Query rows are detached values, while their harness and
                   ;; instance are live presentation authorities.  Copy only
                   ;; the row data; copying the containing structs breaks
                   ;; identity-scoped lookup and routes the row nowhere.
                   (list :instance (plist-get candidate :instance)
                         :instance-id (plist-get candidate :instance-id)
                         :harness (plist-get candidate :harness)
                         :session (copy-tree (plist-get candidate :session) t)
                         :session-id (plist-get candidate :session-id)))
                 candidates)
                e-chat-overview--page-loaded-p t)
    (erase-buffer)
    (if candidates
        (dolist (candidate candidates)
          (e-chat-overview--insert-session-row
           (plist-get candidate :harness)
           (plist-get candidate :session)
           (plist-get candidate :instance)
           show-instance))
      (insert "No e chat sessions\n"))
    (goto-char (point-min))))

(defun e-chat-overview--render (&optional harness)
  "Request and asynchronously render one bounded HARNESS session page."
  (when (and e-chat-overview--page-work
             (not (memq (plist-get (e-work-status e-chat-overview--page-work)
                                   :state)
                        '(finished failed cancelled))))
    (e-work-cancel e-chat-overview--page-work))
  (setq-local e-chat-overview--page-generation
              (1+ e-chat-overview--page-generation))
  (let* ((buffer (current-buffer))
         (generation e-chat-overview--page-generation)
         (target (or harness e-chat-overview--harness))
         (work (e-chat-overview-session-candidates-start target))
         (inhibit-read-only t))
    (setq-local e-chat-overview--harness target
                e-chat-overview--page-work work
                e-chat-overview--displayed-candidates nil
                e-chat-overview--page-loaded-p nil)
    (erase-buffer)
    (insert "Loading e chat sessions…\n")
    (goto-char (point-min))
    (e-work-on-settle
     work
     (lambda (settled)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (= generation e-chat-overview--page-generation)
                      (eq settled e-chat-overview--page-work))
             (setq-local e-chat-overview--page-work nil)
             (let ((status (e-work-status settled)))
               (if (eq (plist-get status :state) 'finished)
                   (e-chat-overview--render-candidates
                    (plist-get status :result) target)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert
                    (format "Unable to query e chat sessions: %s\n\nPress g to retry.\n"
                            (e-work-error-message
                             (or (plist-get status :error)
                                 '(e-work-cancelled "cancelled")))))
                   (goto-char (point-min))))))))))
    work))

(defun e-chat-overview--mark-session-read
    (harness session-or-id &optional instance-id)
  "Record SESSION-OR-ID's latest assistant message as read in HARNESS."
  (let* ((session (if (stringp session-or-id)
                      (e-chat-overview--session-for-id harness session-or-id)
                    session-or-id))
         (session-id (plist-get session :id)))
    (when-let* ((marker (and session
                            (e-chat-overview--latest-assistant-marker
                             harness session))))
      (unless (equal marker
                      (e-chat-overview--session-read-marker
                       harness session instance-id))
        (e-chat-overview--set-session-read-marker
         harness
         session-id
         marker
         instance-id)
        (e-chat-overview--invalidate-unread-cache)))))

(defun e-chat-overview--session-for-id (harness session-id &optional instance-id)
  "Return displayed detached metadata for HARNESS SESSION-ID."
  (plist-get
   (seq-find
    (lambda (candidate)
      (and (eq harness (plist-get candidate :harness))
           (equal session-id (plist-get candidate :session-id))
           (equal instance-id (plist-get candidate :instance-id))))
    e-chat-overview--displayed-candidates)
   :session))

(defun e-chat-overview--harness-for-instance-id (instance-id)
  "Return live harness for INSTANCE-ID, or the overview/default harness."
  (if instance-id
      (e-chat-overview--harness-for-instance
       (or (e-harness-instance-get instance-id)
           (signal 'e-harness-instance-missing (list instance-id))))
    (or e-chat-overview--harness
        (e-chat-overview--default-harness))))

(defun e-chat-overview--row-target-at-point ()
  "Return overview row target at point."
  (let* ((session-id (or (e-chat-overview--session-id-at-point)
                         (user-error "No e chat session at point")))
         (instance-id (e-chat-overview--instance-id-at-point))
         (harness (e-chat-overview--harness-for-instance-id instance-id))
         (session (or (e-chat-overview--session-for-id
                       harness session-id instance-id)
                      (user-error "No e chat session at point"))))
    (list :harness harness
          :session session
          :session-id session-id
          :instance-id instance-id)))

(defun e-chat-overview--session-row-starts ()
  "Return overview session row starts as (POSITION . ROW-KEY) pairs."
  (let ((pos (point-min))
        (limit (point-max))
        last-key
        rows)
    (while (< pos limit)
      (let* ((session-id (get-text-property pos 'e-chat-session-id))
             (instance-id (get-text-property
                           pos 'e-chat-harness-instance-id))
             (key (and session-id (cons instance-id session-id))))
        (when (and key (not (equal key last-key)))
          (push (cons pos key) rows))
        (setq last-key key)
        (setq pos (or (next-single-property-change
                       pos 'e-chat-session-id nil limit)
                      limit))))
    (nreverse rows)))

(defun e-chat-overview--current-session-row-index (rows)
  "Return current session row index in ROWS."
  (let ((key (cons (e-chat-overview--instance-id-at-point)
                   (e-chat-overview--session-id-at-point))))
    (or (cl-position key rows
                     :key #'cdr
                     :test #'equal)
        (user-error "No e chat session at point"))))

(defun e-chat-overview--preview-session-at-point (&optional display)
  "Preview the overview session at point.
When DISPLAY is non-nil, display the preview buffer."
  (let* ((target (e-chat-overview--row-target-at-point))
         (buffer (e-chat-overview--render-resume-preview
                  (plist-get target :harness)
                  (plist-get target :session))))
    (when display
      (display-buffer buffer))
    buffer))

(defun e-chat-overview--goto-session-row (index)
  "Move point to overview session row INDEX and preview it."
  (let* ((rows (e-chat-overview--session-row-starts))
         (row (nth index rows)))
    (unless row
      (user-error "No e chat session at target"))
    (goto-char (car row))
    (e-chat-overview--preview-session-at-point t)))

(defun e-chat-overview-next-session ()
  "Move to the next overview session row and preview it."
  (interactive)
  (let* ((rows (e-chat-overview--session-row-starts))
         (index (e-chat-overview--current-session-row-index rows)))
    (when (>= (1+ index) (length rows))
      (user-error "No next e chat session"))
    (e-chat-overview--goto-session-row (1+ index))))

(defun e-chat-overview-previous-session ()
  "Move to the previous overview session row and preview it."
  (interactive)
  (let* ((rows (e-chat-overview--session-row-starts))
         (index (e-chat-overview--current-session-row-index rows)))
    (when (<= index 0)
      (user-error "No previous e chat session"))
    (e-chat-overview--goto-session-row (1- index))))

(defun e-chat-overview-select-session ()
  "Return the overview session selection intent at point.

The facade's composition command consumes this value to open a chat buffer.
This owner operation only reads the selected row and never constructs a chat
buffer or calls back into the facade."
  (interactive)
  (e-chat-overview-session-selection))

(defun e-chat-overview-preview-session ()
  "Preview the overview session at point."
  (interactive)
  (e-chat-overview--preview-session-at-point
   (called-interactively-p 'interactive)))

(defun e-chat-overview-refresh ()
  "Refresh the current overview buffer."
  (interactive)
  (e-chat-overview--render))

(defun e-chat-overview--unsubscribe ()
  "Clear obsolete overview live-feed state.
The overview is explicitly manual-refresh-only after the board cutover; it does
not open an unbounded process-wide presentation subscription."
  (when (and e-chat-overview--page-work
             (not (memq (plist-get (e-work-status e-chat-overview--page-work)
                                   :state)
                        '(finished failed cancelled))))
    (e-work-cancel e-chat-overview--page-work))
  (setq e-chat-overview--page-work nil)
  (setq e-chat-overview--subscription nil)
  (setq e-chat-overview--subscriptions nil))

(defun e-chat-overview--subscribe (buffer harness)
  "Keep BUFFER manual-refresh-only for HARNESS after the board cutover."
  (ignore harness)
  (with-current-buffer buffer
    (e-chat-overview--unsubscribe)))

(defun e-chat-overview--subscribe-instances (buffer instances)
  "Keep BUFFER manual-refresh-only for INSTANCES after the board cutover."
  (ignore instances)
  (with-current-buffer buffer
    (e-chat-overview--unsubscribe)))

(defun e-chat-overview--display (buffer)
  "Display overview BUFFER as the chat session sidebar."
  (display-buffer-in-side-window
   buffer
   '((side . left)
     (slot . -1)
     (window-width . 36))))

(defun e-chat-overview--visible-window ()
  "Return the visible overview sidebar window, or nil."
  (when-let* ((buffer (get-buffer e-chat-overview-buffer-name))
             (window (get-buffer-window buffer t)))
    (and (window-live-p window) window)))

;;;###autoload
(defun e-chat-overview ()
  "Open the e chat session overview sidebar."
  (interactive)
  (let* ((instances (e-chat-overview--chat-instances))
         (harness (and (not instances)
                       (e-chat-overview--default-harness)))
         (buffer (get-buffer-create e-chat-overview-buffer-name)))
    (with-current-buffer buffer
      (e-chat-overview-mode)
      (setq-local e-chat-overview--harness harness)
      (e-chat-overview--render harness)
      (if instances
          (e-chat-overview--subscribe-instances buffer instances)
        (e-chat-overview--subscribe buffer harness)))
    (when (called-interactively-p 'interactive)
      (e-chat-overview--display buffer))
    buffer))

;;;###autoload
(defun e-chat-overview-close ()
  "Close the e chat session overview sidebar."
  (interactive)
  (let ((buffer (get-buffer e-chat-overview-buffer-name)))
    (when (buffer-live-p buffer)
      (when-let* ((window (get-buffer-window buffer t)))
        (delete-window window))
      (kill-buffer buffer))))

;;;###autoload
(defun e-chat-sidebar-toggle ()
  "Open or close the e chat session overview sidebar."
  (interactive)
  (if (e-chat-overview--visible-window)
      (e-chat-overview-close)
    (let ((window (e-chat-overview--display (e-chat-overview))))
      (when (window-live-p window)
        (select-window window)))))

;;;###autoload


;;; Public overview contract

(defun e-chat-overview-board-session-p (session)
  "Return non-nil when SESSION carries board-native identity."
  (e-chat-overview--board-session-p session))

(defun e-chat-overview-short-session-id (session-id)
  "Return a compact display id for SESSION-ID."
  (e-chat-overview--short-session-id session-id))

(defun e-chat-overview-session-choice-label (session)
  "Return the completion label for SESSION metadata."
  (e-chat-overview--session-choice-label session))

(defun e-chat-overview-session-candidate-label
    (candidate &optional show-instance)
  "Return the completion label for session CANDIDATE."
  (e-chat-overview--session-candidate-label candidate show-instance))

(defun e-chat-overview-read-session-candidate (candidates &optional prompt)
  "Read one session candidate from CANDIDATES."
  (e-chat-overview--read-session-candidate candidates prompt))

(defun e-chat-overview-render-resume-preview (harness session)
  "Render SESSION from HARNESS into the reusable overview preview buffer."
  (e-chat-overview--render-resume-preview harness session))

(defun e-chat-overview-resume-preview-buffer-name ()
  "Return the stable buffer name used by overview resume previews."
  e-chat-overview--resume-preview-buffer-name)

(defun e-chat-overview-render (&optional harness)
  "Render the overview projection for HARNESS in the current buffer.
This is the semantic rendering operation used by the facade and by embedding
shells that own an overview buffer."
  (e-chat-overview--render harness))

(defun e-chat-overview-session-id-at-point (&optional buffer)
  "Return the session identity represented at point in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-overview--session-id-at-point)))

(defun e-chat-overview-refresh-keymap (&optional open-command)
  "Refresh the overview command map after a presentation reload.

OPEN-COMMAND is supplied by the composing shell when its command should be
bound to RET and `o'; nil retains the owner selection contract."
  (setq e-chat-overview-mode-map
        (e-chat-overview--make-mode-map e-chat-overview-mode-map
                                        open-command)))

(defun e-chat-overview-mark-session-read (harness session-or-id &optional instance-id)
  "Mark SESSION-OR-ID read in HARNESS for INSTANCE-ID."
  (e-chat-overview--mark-session-read harness session-or-id instance-id))

(defun e-chat-overview-session-unread-p
    (harness session-or-id &optional instance-id)
  "Return non-nil when SESSION-OR-ID has unread assistant output.
The query exposes the overview's semantic read-marker projection without
returning its process-local marker table or workspace cache representation."
  (let ((session (if (stringp session-or-id)
                     (e-chat-overview--session-for-id harness session-or-id)
                   session-or-id)))
    (and session
         (e-chat-overview--session-unread-p harness session instance-id))))

(defun e-chat-overview-mark-selected-session-read (&optional buffer)
  "Mark the session represented by BUFFER's chat surface as read.
When BUFFER is omitted, use the current chat transcript.  This port keeps
read-marker and workspace-unread mutation in the overview owner while the
facade remains responsible only for deciding when a selected turn settles."
  (let* ((candidate (or buffer (current-buffer)))
         (selected-surface (e-chat-surface-selected-chat-surface))
         (selected-buffer (car-safe selected-surface)))
    (when (and (buffer-live-p candidate)
               (eq candidate selected-buffer))
      (with-current-buffer candidate
        (when (and (boundp 'e-chat-harness)
                   (boundp 'e-chat-session-id)
                   e-chat-harness
                   e-chat-session-id)
          (let ((was-unread (e-chat-overview--buffer-unread-p candidate)))
            (e-chat-overview--mark-session-read
             e-chat-harness
             (list :id e-chat-session-id
                   :latest-assistant-marker
                   (plist-get e-chat-session-metadata
                              :latest-assistant-marker))
             (and (boundp 'e-chat-harness-instance-id)
                  e-chat-harness-instance-id))
            (when was-unread
              (e-chat-overview--workspace-unread-cache-rebuild))))))))

(defun e-chat-overview-prepare-unread-cache ()
  "Initialize an empty valid workspace unread cache for diagnostics/tests."
  (setq e-chat-overview--workspace-unread-cache-valid-p t
        e-chat-overview--workspace-unread-counts (make-hash-table :test 'equal)
        e-chat-overview--workspace-unread-buffer-state (make-hash-table :test 'eq)))

(defun e-chat-overview-update-unread-cache (&optional buffer)
  "Update workspace unread state for BUFFER."
  (e-chat-overview--workspace-unread-cache-update-buffer buffer))

(defun e-chat-overview-remove-unread-buffer (&optional buffer)
  "Remove BUFFER from workspace unread state."
  (e-chat-overview--workspace-unread-cache-remove-buffer buffer))

(defun e-chat-overview-rebuild-unread-cache ()
  "Rebuild workspace unread state from live chat buffers."
  (e-chat-overview--workspace-unread-cache-rebuild))

(provide 'e-chat-overview)

;;; e-chat-overview.el ends here
