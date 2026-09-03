;;; e-modernchat-view-model.el --- View model for egui modern chat -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Convert e harness/session state into a small JSON-friendly DTO consumed by
;; the egui modern chat shell.

;;; Code:

(require 'cl-lib)
(require 'e-chat-service)
(require 'e-chat-output-mode)
(require 'e-harness)
(require 'e-message-details)
(require 'e-session)
(require 'e-structured-blocks)
(require 'subr-x)

(defgroup e-modernchat nil
  "egui-backed modern chat shell for e."
  :group 'e)

(defcustom e-modernchat-view-model-message-limit 200
  "Maximum number of recent messages included in a modern chat snapshot."
  :type 'integer
  :group 'e-modernchat)

(defcustom e-modernchat-view-model-activity-limit 200
  "Maximum number of recent activity events included in a modern chat snapshot."
  :type 'integer
  :group 'e-modernchat)

(defun e-modernchat-view-model--string (value)
  "Return VALUE as a JSON-friendly string, or nil."
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun e-modernchat-view-model--json-bool (value)
  "Return VALUE as JSON boolean t or :json-false."
  (if value t :json-false))

(defun e-modernchat-view-model--take-last (items limit)
  "Return at most LIMIT last ITEMS, preserving order."
  (let* ((items (copy-sequence (or items nil)))
         (length (length items)))
    (if (and limit (> length limit))
        (nthcdr (- length limit) items)
      items)))

(defun e-modernchat-view-model--message-status (message active-turn-id)
  "Return display status for MESSAGE and ACTIVE-TURN-ID."
  (let ((turn-id (plist-get message :turn-id)))
    (cond
     ((and active-turn-id turn-id (equal active-turn-id turn-id)) "streaming")
     ((plist-get message :error) "error")
     (t "final"))))

(defun e-modernchat-view-model--display-content (content registry)
  "Return CONTENT with any registered structured blocks applied for display.
Mirrors the chat shell: ask the core registry generically, never inspect a
specific capability's block syntax.  With no REGISTRY or no registered kinds,
CONTENT is returned unchanged."
  (if (and registry (stringp content))
      (plist-get (e-structured-blocks-render content registry) :text)
    content))

(defun e-modernchat-view-model-message
    (message &optional active-turn-id content-mode registry presentation)
  "Return JSON DTO for session MESSAGE.
REGISTRY, when non-nil, is a structured-block registry consulted to strip
hidden blocks (e.g. a reasoning mark) from the displayed content.
PRESENTATION, when non-nil, is the chat service's already combined content and
capability-owned message details."
  (let ((id (or (plist-get message :id)
                (plist-get message :message-id)
                (plist-get message :turn-id)))
        (turn-id (plist-get message :turn-id))
        (role (or (plist-get message :role) 'unknown))
        (content (or (plist-get presentation :content)
                     (e-modernchat-view-model--display-content
                      (or (plist-get message :content) "") registry)))
        (details (plist-get presentation :details)))
    `((id . ,(e-modernchat-view-model--string id))
      (turnId . ,(e-modernchat-view-model--string turn-id))
      (role . ,(e-modernchat-view-model--string role))
      (status . ,(e-modernchat-view-model--message-status
                  message active-turn-id))
      (createdAt . ,(e-modernchat-view-model--string
                     (plist-get message :created-at)))
      (content . ,(e-modernchat-view-model--string content))
      (detailSummary . ,(e-modernchat-view-model--string
                         (string-join
                          (delq nil (mapcar #'e-message-detail-summary details))
                          ", ")))
      (details . ,(vconcat
                   (mapcar
                    (lambda (detail)
                      `((id . ,(e-modernchat-view-model--string
                                (e-message-detail-id detail)))
                        (summary . ,(e-message-detail-summary detail))
                        (body . ,(e-message-detail-body detail))))
                    details)))
      (contentMode . ,(e-modernchat-view-model--string
                       (or content-mode 'plain))))))

(defun e-modernchat-view-model--activity-title (event)
  "Return compact title for activity EVENT."
  (let* ((type (plist-get event :event-type))
         (payload (plist-get event :payload))
         (tool-name (or (plist-get payload :tool-name)
                        (plist-get payload :name)
                        (plist-get payload :action))))
    (pcase type
      ('hook-audit "Hook audit")
      ('context-curated "Context curated")
      (_
       (string-trim
        (mapconcat #'identity
                   (delq nil (list (e-modernchat-view-model--string type)
                                   (e-modernchat-view-model--string tool-name)))
                   " "))))))

(defun e-modernchat-view-model--activity-status (event)
  "Return display status for activity EVENT."
  (let ((type (plist-get event :event-type))
        (payload (plist-get event :payload)))
    (cond
     ((or (memq type '(tool-failed turn-failed provider-error))
          (plist-get payload :error))
      "error")
     ((memq type '(tool-started turn-started provider-started)) "running")
     ((memq type '(turn-cancelled tool-cancelled)) "cancelled")
     (t "ok"))))

(defun e-modernchat-view-model-activity (event)
  "Return JSON DTO for activity EVENT."
  (let ((type (plist-get event :event-type)))
    `((id . ,(e-modernchat-view-model--string (plist-get event :message-id)))
      (turnId . ,(e-modernchat-view-model--string (plist-get event :turn-id)))
      (kind . ,(e-modernchat-view-model--string type))
      (status . ,(e-modernchat-view-model--activity-status event))
      (createdAt . ,(e-modernchat-view-model--string
                     (plist-get event :created-at)))
      (title . ,(e-modernchat-view-model--activity-title event))
      (summary . ,(e-modernchat-view-model--string
                   (if (eq type 'context-curated)
                       (e-chat-service-format-context-curation
                        (plist-get event :payload))
                     (or (plist-get (plist-get event :payload) :summary)
                         (plist-get (plist-get event :payload) :message)
                         "")))))))

(defun e-modernchat-view-model--attachment-kind (attachment)
  "Return display kind for ATTACHMENT."
  (or (plist-get attachment :kind)
      (plist-get attachment :type)
      (when-let ((uri (plist-get attachment :uri)))
        (cond
         ((string-prefix-p "buffer://" uri) 'buffer)
         ((string-prefix-p "file://" uri) 'file)
         ((string-prefix-p "e://" uri) 'resource)
         (t 'resource)))
      'resource))

(defun e-modernchat-view-model-attachment (attachment index)
  "Return JSON DTO for ATTACHMENT at INDEX."
  (let ((uri (or (plist-get attachment :uri)
                 (plist-get attachment :target))))
    `((id . ,(or (e-modernchat-view-model--string (plist-get attachment :id))
                 (format "attachment-%d" index)))
      (kind . ,(e-modernchat-view-model--string
                (e-modernchat-view-model--attachment-kind attachment)))
      (label . ,(or (e-modernchat-view-model--string
                     (plist-get attachment :label))
                    (e-modernchat-view-model--string uri)
                    (format "Attachment %d" index)))
      (uri . ,(e-modernchat-view-model--string uri)))))

(defun e-modernchat-view-model--attachments (metadata)
  "Return attachment plist list from session METADATA."
  (let ((attachments
         (plist-get
          (plist-get (plist-get metadata :context-references) :chat-session)
          :attachments)))
    (cond
     ((vectorp attachments) (append attachments nil))
     ((listp attachments) attachments)
     (t nil))))

(cl-defun e-modernchat-view-model-snapshot
    (harness session-id &key composer-text message-limit activity-limit)
  "Return JSON-friendly snapshot for HARNESS SESSION-ID."
  (let* ((session (e-chat-service-session harness session-id))
         (metadata (plist-get session :metadata))
         (state (ignore-errors (e-chat-service-state harness session-id)))
         (active-turn-id (or (plist-get (plist-get state :active-turn) :id)
                             (plist-get state :active-turn)))
         (output-mode (e-chat-output-mode-resolve harness session-id))
         (registry (ignore-errors
                     (e-chat-service-structured-blocks harness session-id)))
         (messages (e-modernchat-view-model--take-last
                    (cl-remove-if #'e-harness-message-hidden-p
                                  (e-chat-service-messages harness session-id))
                    (or message-limit e-modernchat-view-model-message-limit)))
         (activities (e-modernchat-view-model--take-last
                      (e-chat-service-activity-events harness session-id)
                      (or activity-limit e-modernchat-view-model-activity-limit)))
         (attachments (e-modernchat-view-model--attachments metadata)))
    `((session . ((id . ,(e-modernchat-view-model--string session-id))
                  (name . ,(or (e-chat-service-session-name harness session-id)
                               (e-chat-service-session-title harness session-id)))
                  (projectRoot . ,(e-modernchat-view-model--string
                                   (plist-get metadata :project-root)))
                  (activeTurnId . ,(e-modernchat-view-model--string
                                    active-turn-id))
                  (model . ,(e-modernchat-view-model--string
                             (plist-get (e-harness-display-options
                                         harness session-id)
                                        :model)))
                  (layers . ,(vconcat
                              (mapcar #'symbol-name
                                      (e-harness-effective-layer-ids
                                       harness session-id))))
                  (outputMode . ,(e-modernchat-view-model--string
                                  output-mode))))
      (messages . ,(vconcat
                    (mapcar (lambda (message)
                              (e-modernchat-view-model-message
                               message active-turn-id output-mode registry
                               (e-chat-service-message-presentation
                                harness session-id message)))
                            messages)))
      (activities . ,(vconcat
                      (mapcar #'e-modernchat-view-model-activity
                              activities)))
      (attachments . ,(vconcat
                       (cl-loop for attachment in attachments
                                for index from 0
                                collect
                                (e-modernchat-view-model-attachment
                                 attachment index))))
      (composer . ((text . ,(or composer-text ""))
                   (enabled . ,(e-modernchat-view-model--json-bool
                                (not active-turn-id)))
                   (placeholder . "Ask e..."))))))

(provide 'e-modernchat-view-model)

;;; e-modernchat-view-model.el ends here
