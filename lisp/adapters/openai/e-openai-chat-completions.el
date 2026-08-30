;;; e-openai-chat-completions.el --- Chat Completions wire mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the Chat Completions request mapping and endpoint URL.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'e-tools)
(require 'e-openai-profile)

(defun e-openai-chat-completions--message-content (content)
  "Return Chat Completions message content for CONTENT."
  (if (stringp content) content (or content "")))

(defun e-openai-chat-completions--tool-definition (tool)
  "Map backend-neutral TOOL to a Chat Completions tool definition."
  (let ((function (list :name (plist-get tool :name)
                        :description (plist-get tool :description)
                        :parameters (plist-get tool :parameters))))
    (when (plist-get tool :strict)
      (setq function (append function (list :strict (plist-get tool :strict)))))
    (list :type "function" :function function)))

(defun e-openai-chat-completions--tool-definitions (tools)
  "Map backend-neutral TOOLS to Chat Completions tool definitions."
  (vconcat (mapcar #'e-openai-chat-completions--tool-definition tools)))

(defun e-openai-chat-completions--message (message)
  "Map backend-neutral MESSAGE to a Chat Completions message."
  (let ((role (plist-get message :role))
        (content (plist-get message :content)))
    (pcase role
      ('tool-call
       (let ((arguments (json-encode
                         (or (plist-get content :arguments)
                             (make-hash-table :test 'equal)))))
         (list :role "assistant"
               :content nil
               :tool_calls
               (vector
                (list :id (plist-get content :id)
                      :type "function"
                      :function (list :name (plist-get content :name)
                                      :arguments arguments))))))
      ('tool
       (let ((result content))
         (list :role "tool"
               :tool_call_id (plist-get result :tool-call-id)
               :content (e-tools-result-content-text
                         (plist-get result :content)))))
      (_
       (list :role (symbol-name role)
             :content (e-openai-chat-completions--message-content content))))))

(defun e-openai-chat-completions--messages (messages options)
  "Return Chat Completions messages from backend-neutral MESSAGES and OPTIONS."
  (let ((instructions (or (plist-get options :instructions)
                          "You are a helpful assistant.")))
    (vconcat
     (append
      (when (and (stringp instructions)
                 (not (string-empty-p instructions)))
        (list (list :role "system" :content instructions)))
      (mapcar #'e-openai-chat-completions--message messages)))))

(cl-defun e-openai-chat-completion-request-body (&key messages options tools)
  "Build a Chat Completions request body from MESSAGES, OPTIONS, and TOOLS."
  (let ((body (list :model (or (plist-get options :model)
                               e-openai-default-model)
                    :stream t
                    :messages (e-openai-chat-completions--messages
                               messages
                               options))))
    (when tools
      (setq body (append body
                         (list :tools
                               (e-openai-chat-completions--tool-definitions tools)
                               :tool_choice "auto"))))
    body))

(defun e-openai-chat-completion-url (base-url)
  "Return the Chat Completions endpoint URL for BASE-URL."
  (let ((normalized (string-remove-suffix "/" base-url)))
    (if (string-suffix-p "/chat/completions" normalized)
        normalized
      (concat normalized "/chat/completions"))))

(provide 'e-openai-chat-completions)

;;; e-openai-chat-completions.el ends here
