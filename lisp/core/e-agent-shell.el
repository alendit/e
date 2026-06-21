;;; e-agent-shell.el --- Agent Shell adapter for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Narrow source-version-sensitive adapter for xenodium/agent-shell.  No other
;; e module should call Agent Shell functions directly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(define-error 'e-agent-shell-unavailable "Agent Shell is unavailable")
(define-error 'e-agent-shell-non-adoptable "Agent Shell buffer is not adoptable")
(define-error 'e-agent-shell-operation-unavailable
  "Agent Shell operation is unavailable")

(defvar agent-shell-project-root)
(defvar agent-shell-config)
(defvar agent-shell--config)
(defvar agent-shell-session-id)
(defvar agent-shell--session-id)
(defvar agent-shell--transcript-file)
(defvar agent-shell-transcript-file-path-function)
(defvar agent-shell-agent-configs)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell--state)

(defconst e-agent-shell-events
  '(input-submitted permission-request permission-response tool-call-update
    file-write turn-complete error clean-up)
  "Agent Shell events tracked by the fleet adapter.")

(defun e-agent-shell-available-p ()
  "Return non-nil when Agent Shell can be loaded."
  (or (featurep 'agent-shell)
      (require 'agent-shell nil t)))

(defun e-agent-shell--ensure-available ()
  "Signal a clear diagnostic unless Agent Shell is available."
  (unless (e-agent-shell-available-p)
    (signal 'e-agent-shell-unavailable
            '("Agent Shell is not installed or not loadable")))
  t)

(defun e-agent-shell--data-get (data key)
  "Return KEY from plist, alist, or hash table DATA."
  (cond
   ((hash-table-p data) (gethash key data))
   ((and (listp data) (plist-member data key)) (plist-get data key))
   ((listp data)
    (let ((cell (assoc key data)))
      (and cell (cdr cell))))
   (t nil)))

(defun e-agent-shell--config-id (config)
  "Return normalized identifier for Agent Shell CONFIG."
  (let ((identifier (e-agent-shell--data-get config :identifier)))
    (cond
     ((symbolp identifier) (symbol-name identifier))
     ((stringp identifier) identifier)
     (identifier (format "%s" identifier))
     (t nil))))

(defun e-agent-shell--config-match-p (config identifier)
  "Return non-nil when CONFIG matches IDENTIFIER."
  (let ((config-id (e-agent-shell--config-id config)))
    (and config-id
         identifier
         (equal config-id (if (symbolp identifier)
                              (symbol-name identifier)
                            (format "%s" identifier))))))

(defun e-agent-shell--resolve-config (identifier)
  "Resolve Agent Shell config IDENTIFIER to a full config alist."
  (cond
   ((and (listp identifier)
         (e-agent-shell--data-get identifier :identifier))
    identifier)
   (identifier
    (or (cl-find-if
         (lambda (config)
           (e-agent-shell--config-match-p config identifier))
         (and (boundp 'agent-shell-agent-configs)
              agent-shell-agent-configs))
        (signal 'e-agent-shell-operation-unavailable
                (list (format "No Agent Shell config found for %s"
                              identifier)))))
   ((and (boundp 'agent-shell-preferred-agent-config)
         agent-shell-preferred-agent-config)
    (e-agent-shell--resolve-config agent-shell-preferred-agent-config))
   ((fboundp 'agent-shell--resolve-preferred-config)
    (agent-shell--resolve-preferred-config))
   ((and (boundp 'agent-shell-agent-configs)
         (= (length agent-shell-agent-configs) 1))
    (car agent-shell-agent-configs))
   (t
    (signal 'e-agent-shell-operation-unavailable
            '("No Agent Shell config found")))))

(cl-defun e-agent-shell-start-worker
    (&key project-root agent-id background no-focus session-strategy)
  "Start a real Agent Shell worker buffer and return it.
PROJECT-ROOT selects `default-directory'.  AGENT-ID resolves to an Agent Shell
config.  BACKGROUND is accepted for the e action contract; NO-FOCUS is the
Agent Shell background-control keyword.  SESSION-STRATEGY is forwarded when
provided."
  (e-agent-shell--ensure-available)
  (ignore background)
  (unless (fboundp 'agent-shell--start)
    (signal 'e-agent-shell-operation-unavailable
            '("agent-shell--start is unavailable")))
  (let* ((config (e-agent-shell--resolve-config agent-id))
         (args (append
                (list :config config
                      :no-focus (if (null no-focus) t no-focus)
                      :new-session t)
                (when session-strategy
                  (list :session-strategy session-strategy))))
         (default-directory (file-name-as-directory
                             (or project-root default-directory)))
         (buffer (apply #'agent-shell--start args)))
    (unless (buffer-live-p buffer)
      (signal 'e-agent-shell-operation-unavailable
              '("agent-shell--start did not return a live buffer")))
    buffer))

(defun e-agent-shell-status (buffer)
  "Return normalized Agent Shell status for BUFFER."
  (e-agent-shell--ensure-available)
  (if (not (buffer-live-p buffer))
      'dead
    (let ((status
           (if (fboundp 'agent-shell-status)
               (with-current-buffer buffer
                 (agent-shell-status))
             (let* ((state (e-agent-shell--buffer-local-value
                            'agent-shell--state buffer))
                    (heartbeat (e-agent-shell--data-get state :heartbeat))
                    (heartbeat-status
                     (e-agent-shell--data-get heartbeat :status)))
               (pcase heartbeat-status
                 ('busy 'busy)
                 ('ended 'ready)
                 (_ (or (e-agent-shell--data-get state :status)
                        'ready)))))))
      (cond
       ((memq status '(ready busy blocked failed finished interrupted dead))
        status)
       ((stringp status) (intern status))
       ((and (listp status) (plist-get status :status))
        (plist-get status :status))
       (t 'ready)))))

(defun e-agent-shell-send-prompt (buffer prompt)
  "Submit PROMPT to Agent Shell BUFFER."
  (e-agent-shell--ensure-available)
  (unless (buffer-live-p buffer)
    (user-error "Agent Shell buffer is dead"))
  (unless (stringp prompt)
    (signal 'wrong-type-argument (list 'stringp prompt)))
  (cond
   ((fboundp 'agent-shell-insert)
    (agent-shell-insert :text prompt :shell-buffer buffer :submit t))
   ((fboundp 'agent-shell--insert-to-shell-buffer)
    (agent-shell--insert-to-shell-buffer
     :shell-buffer buffer
     :text prompt
     :submit t))
   (t
    (signal 'e-agent-shell-operation-unavailable
            '("No Agent Shell prompt insertion function is available"))))
  t)

(defun e-agent-shell-subscribe (buffer callback)
  "Subscribe CALLBACK to Agent Shell events for BUFFER."
  (e-agent-shell--ensure-available)
  (unless (buffer-live-p buffer)
    (user-error "Agent Shell buffer is dead"))
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (unless (fboundp 'agent-shell-subscribe-to)
    (signal 'e-agent-shell-operation-unavailable
            '("agent-shell-subscribe-to is unavailable")))
  (mapcar
   (lambda (event)
     (agent-shell-subscribe-to
      :shell-buffer buffer
      :event event
      :on-event callback))
   e-agent-shell-events))

(cl-defun e-agent-shell-interrupt (buffer &key force)
  "Interrupt Agent Shell BUFFER.
When FORCE is non-nil, request a forceful interrupt when Agent Shell supports
that distinction."
  (e-agent-shell--ensure-available)
  (unless (buffer-live-p buffer)
    (user-error "Agent Shell buffer is dead"))
  (unless (fboundp 'agent-shell-interrupt)
    (signal 'e-agent-shell-operation-unavailable
            '("agent-shell-interrupt is unavailable")))
  (with-current-buffer buffer
    (agent-shell-interrupt force)))

(defun e-agent-shell--buffer-local-value (symbol buffer)
  "Return SYMBOL's local value in BUFFER, or nil when unbound."
  (when (local-variable-p symbol buffer)
    (condition-case nil
        (buffer-local-value symbol buffer)
      (void-variable nil))))

(defun e-agent-shell-transcript-file (buffer)
  "Return Agent Shell transcript file for BUFFER, or nil."
  (cond
   ((not (buffer-live-p buffer)) nil)
   ((e-agent-shell--buffer-local-value 'agent-shell--transcript-file buffer))
   ((e-agent-shell--data-get
     (e-agent-shell--buffer-local-value 'agent-shell--state buffer)
     :transcript-file))
   ((and (boundp 'agent-shell-transcript-file-path-function)
         (functionp agent-shell-transcript-file-path-function))
    (with-current-buffer buffer
      (funcall agent-shell-transcript-file-path-function)))
   (t nil)))

(defun e-agent-shell--adoption-value (buffer symbols)
  "Return the first non-nil buffer-local value among SYMBOLS in BUFFER."
  (cl-loop for symbol in symbols
           for value = (e-agent-shell--buffer-local-value symbol buffer)
           when value return value))

(defun e-agent-shell--agent-shell-buffer-p (buffer)
  "Return non-nil when BUFFER appears to be a real Agent Shell buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (let ((state (e-agent-shell--buffer-local-value
                       'agent-shell--state buffer)))
           (and (derived-mode-p 'agent-shell-mode)
                state
                (eq (e-agent-shell--data-get state :buffer) buffer))))))

(defun e-agent-shell-adopt-buffer (buffer)
  "Return metadata for existing Agent Shell BUFFER or signal a diagnostic."
  (e-agent-shell--ensure-available)
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))))
    (unless (buffer-live-p buffer)
      (signal 'e-agent-shell-non-adoptable
              '("No live Agent Shell buffer found")))
    (unless (e-agent-shell--agent-shell-buffer-p buffer)
      (signal 'e-agent-shell-non-adoptable
              '("Buffer is not an Agent Shell buffer")))
    (let* ((state (e-agent-shell--buffer-local-value 'agent-shell--state
                                                     buffer))
           (config
            (or (e-agent-shell--data-get state :agent-config)
                (e-agent-shell--adoption-value
                 buffer
                 '(agent-shell-config agent-shell--config))))
           (session (e-agent-shell--data-get state :session))
           (project-root
            (or (e-agent-shell--adoption-value
                 buffer
                 '(agent-shell-project-root))
                (buffer-local-value 'default-directory buffer)))
           (agent-id
            (or (e-agent-shell--config-id config)
                (and config (format "%s" config))))
           (agent-session-id
            (or (e-agent-shell--data-get session :id)
                (e-agent-shell--data-get state :resume-session-id)
                (e-agent-shell--adoption-value
                 buffer
                 '(agent-shell-session-id agent-shell--session-id))))
           (transcript-file (e-agent-shell-transcript-file buffer)))
      (unless (and project-root agent-id agent-session-id)
        (signal 'e-agent-shell-non-adoptable
                (list "Agent Shell buffer lacks project, config, or session metadata")))
      (list :shell-buffer buffer
            :project-root project-root
            :agent-id agent-id
            :agent-session-id agent-session-id
            :transcript-file transcript-file))))

(defun e-agent-shell-read-transcript-excerpt (file &optional limit)
  "Read a bounded transcript excerpt from FILE.
LIMIT is a character limit, defaulting to 8000."
  (unless (and file (file-readable-p file))
    (user-error "Agent Shell transcript is unavailable"))
  (let ((limit (or limit 8000)))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((text (buffer-string))
             (length (length text))
             (start (max 0 (- length limit))))
        (list :excerpt (substring text start)
              :truncated (> length limit)
              :limit limit)))))

(provide 'e-agent-shell)

;;; e-agent-shell.el ends here
