;;; e-openai.el --- OpenAI/Codex backend adapter for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; OpenAI Responses/Codex adapter.  Provider auth, endpoint shape, request
;; mapping, and SSE parsing stay here instead of leaking into the harness.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pp)
(require 'subr-x)
(require 'url)
(require 'e-backend)
(require 'e-harness)

(define-error 'e-openai-auth-missing "OpenAI/Codex auth is missing")
(define-error 'e-openai-auth-invalid "OpenAI/Codex auth is invalid")

(defconst e-openai-codex-default-base-url
  "https://chatgpt.com/backend-api"
  "Default base URL for ChatGPT-backed Codex Responses access.")

(defconst e-openai-codex-account-claim
  "https://api.openai.com/auth"
  "JWT claim namespace containing the ChatGPT account id.")

(defgroup e-openai nil
  "OpenAI/Codex backend adapter for e."
  :group 'e
  :prefix "e-openai-")

(defcustom e-openai-codex-default-model "gpt-5.4"
  "Default model for ChatGPT-backed Codex requests."
  :type 'string
  :group 'e-openai)

(defcustom e-openai-codex-debug nil
  "When non-nil, retain the last raw Codex response and event summaries."
  :type 'boolean
  :group 'e-openai)

(defvar e-openai-codex--last-diagnostics nil
  "Last OpenAI/Codex diagnostics captured when debug mode is enabled.")

(defun e-openai-codex-last-diagnostics ()
  "Return the last captured OpenAI/Codex diagnostics.
When called interactively, display diagnostics in a temporary buffer.
Diagnostics are captured only when `e-openai-codex-debug' is non-nil."
  (interactive)
  (if (called-interactively-p 'interactive)
      (with-current-buffer (get-buffer-create "*e-openai-codex-diagnostics*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (pp-to-string e-openai-codex--last-diagnostics))
          (goto-char (point-min))
          (special-mode))
        (display-buffer (current-buffer)))
    e-openai-codex--last-diagnostics))

(defun e-openai-codex-auth-file (&optional codex-home)
  "Return the Codex auth file path for CODEX-HOME.
When CODEX-HOME is nil, use the CODEX_HOME environment variable or
`~/.codex'."
  (expand-file-name
   "auth.json"
   (or codex-home
       (getenv "CODEX_HOME")
       (expand-file-name "~/.codex"))))

(defun e-openai-codex-read-auth (&optional auth-file)
  "Read Codex auth from AUTH-FILE and return a plist."
  (let ((file (or auth-file (e-openai-codex-auth-file))))
    (unless (file-readable-p file)
      (signal 'e-openai-auth-missing (list file)))
    (json-parse-string (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))
                       :object-type 'plist
                       :array-type 'list
                       :null-object nil
                       :false-object :json-false)))

(defun e-openai-codex-auth-access-token (auth)
  "Return AUTH's access token."
  (or (plist-get (plist-get auth :tokens) :access_token)
      (plist-get auth :access_token)
      (signal 'e-openai-auth-invalid '("Missing access_token"))))

(defun e-openai-codex--base64url-decode (value)
  "Decode base64url VALUE."
  (let* ((normalized (replace-regexp-in-string "-" "+" value))
         (normalized (replace-regexp-in-string "_" "/" normalized))
         (padding (mod (- 4 (mod (length normalized) 4)) 4)))
    (base64-decode-string
     (concat normalized (make-string padding ?=)))))

(defun e-openai-codex--json-key (key)
  "Return plist keyword for JSON object KEY."
  (intern (concat ":" key)))

(defun e-openai-codex-auth-account-id (auth)
  "Extract the ChatGPT account id from AUTH's access token."
  (let* ((token (e-openai-codex-auth-access-token auth))
         (parts (split-string token "\\."))
         (payload (nth 1 parts)))
    (unless (= (length parts) 3)
      (signal 'e-openai-auth-invalid '("Access token is not a JWT")))
    (let* ((claims (json-parse-string
                    (e-openai-codex--base64url-decode payload)
                    :object-type 'plist
                    :array-type 'list
                    :null-object nil
                    :false-object :json-false))
           (account-claims (plist-get
                            claims
                            (e-openai-codex--json-key
                             e-openai-codex-account-claim)))
           (account-id (plist-get account-claims :chatgpt_account_id)))
      (or account-id
          (signal 'e-openai-auth-invalid
                  '("Missing ChatGPT account id claim"))))))

(defun e-openai-codex--message-content (role content)
  "Return Responses content item for ROLE and CONTENT."
  (let ((type (if (eq role 'assistant) "output_text" "input_text")))
    (vector (list :type type :text (or content "")))))

(defun e-openai-codex--input-message (message)
  "Map backend-neutral MESSAGE to a Responses input item."
  (let ((role (plist-get message :role))
        (content (plist-get message :content)))
    (pcase role
      ('tool
       (let ((result content))
         (list :type "function_call_output"
               :call_id (plist-get result :tool-call-id)
               :output (format "%s" (plist-get result :content)))))
      (_
       (list :type "message"
             :role (symbol-name role)
             :content (e-openai-codex--message-content role content))))))

(cl-defun e-openai-codex-request-body (&key messages options tools)
  "Build a Codex Responses request body from MESSAGES, OPTIONS, and TOOLS."
  (let ((body (list :model (or (plist-get options :model) "gpt-5.4")
                    :store :json-false
                    :stream t
                    :instructions (or (plist-get options :instructions)
                                      "You are a helpful assistant.")
                    :input (vconcat (mapcar #'e-openai-codex--input-message
                                            messages))
                    :tool_choice "auto"
                    :parallel_tool_calls t)))
    (when tools
      (setq body (append body (list :tools (vconcat tools)))))
    (when-let ((reasoning (plist-get options :reasoning)))
      (setq body (append body (list :reasoning reasoning))))
    body))

(defun e-openai-codex-url (&optional base-url)
  "Return the Codex Responses URL for BASE-URL."
  (let ((normalized (string-remove-suffix
                     "/"
                     (or base-url e-openai-codex-default-base-url))))
    (cond
     ((string-suffix-p "/codex/responses" normalized) normalized)
     ((string-suffix-p "/codex" normalized)
      (concat normalized "/responses"))
     (t (concat normalized "/codex/responses")))))

(defun e-openai-codex--headers (auth &optional session-id)
  "Return Codex request headers for AUTH and SESSION-ID."
  (let* ((token (e-openai-codex-auth-access-token auth))
         (headers `(("Authorization" . ,(concat "Bearer " token))
                    ("chatgpt-account-id" . ,(e-openai-codex-auth-account-id auth))
                    ("originator" . "e")
                    ("OpenAI-Beta" . "responses=experimental")
                    ("Accept" . "text/event-stream")
                    ("Content-Type" . "application/json"))))
    (if session-id
        (append headers
                `(("session_id" . ,session-id)
                  ("x-client-request-id" . ,session-id)))
      headers)))

(cl-defun e-openai-codex--http-request (&key url headers body)
  "POST BODY to URL with HEADERS and return response text."
  (let ((url-request-method "POST")
        (url-request-extra-headers headers)
        (url-request-data (encode-coding-string body 'utf-8)))
    (with-current-buffer (url-retrieve-synchronously url 'silent nil nil)
      (unwind-protect
          (progn
            (goto-char (point-min))
            (re-search-forward "\n\n" nil 'move)
            (buffer-substring-no-properties (point) (point-max)))
        (kill-buffer (current-buffer))))))

(defun e-openai-codex--parse-json (value)
  "Parse VALUE as JSON into plist data."
  (json-parse-string value
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(defun e-openai-codex--function-call-item-p (item)
  "Return non-nil when ITEM is a Responses function-call item."
  (and (listp item)
       (equal (plist-get item :type) "function_call")))

(defun e-openai-codex--parse-function-arguments (arguments)
  "Parse JSON ARGUMENTS from a Responses function call."
  (if (and (stringp arguments) (not (string-empty-p arguments)))
      (e-openai-codex--parse-json arguments)
    nil))

(defun e-openai-codex--sequence-list (value)
  "Return VALUE as a list when it is a JSON array sequence."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun e-openai-codex--content-text (content)
  "Return concatenated output text from Responses CONTENT."
  (string-join
   (delq nil
         (mapcar
          (lambda (part)
            (when (member (plist-get part :type) '("output_text" "text"))
              (plist-get part :text)))
          (e-openai-codex--sequence-list content)))
   ""))

(defun e-openai-codex--message-item-text (item)
  "Return assistant text from a Responses message ITEM."
  (when (equal (plist-get item :type) "message")
    (let ((text (e-openai-codex--content-text (plist-get item :content))))
      (unless (string-empty-p text)
        text))))

(defun e-openai-codex--event-summary (event item)
  "Return a compact diagnostics summary for provider EVENT and parsed ITEM."
  (let ((provider-item (plist-get event :item))
        (provider-part (plist-get event :part)))
    (list :event-type (plist-get event :type)
          :item-type (or (plist-get provider-item :type)
                         (plist-get provider-part :type))
          :parsed-type (plist-get item :type))))

(defun e-openai-codex--event-item (event)
  "Map parsed Responses EVENT to one backend-neutral item, or nil."
  (let ((type (plist-get event :type)))
    (cond
     ((equal type "response.output_text.delta")
      (list :type 'assistant-delta
            :content (plist-get event :delta)))
     ((equal type "response.output_text.done")
      (list :type 'assistant-message
            :content (plist-get event :text)))
     ((and (equal type "response.output_item.done")
           (e-openai-codex--function-call-item-p (plist-get event :item)))
      (let ((item (plist-get event :item)))
        (list :type 'tool-call
              :id (plist-get item :call_id)
              :name (plist-get item :name)
              :arguments (e-openai-codex--parse-function-arguments
                          (plist-get item :arguments)))))
     ((and (equal type "response.output_item.done")
           (e-openai-codex--message-item-text (plist-get event :item)))
      (list :type 'assistant-message-candidate
            :content (e-openai-codex--message-item-text
                      (plist-get event :item))
            :source 'output-item))
     ((and (equal type "response.content_part.done")
           (member (plist-get (plist-get event :part) :type)
                   '("output_text" "text")))
      (list :type 'assistant-message-candidate
            :content (plist-get (plist-get event :part) :text)
            :source 'content-part))
     ((member type '("response.completed" "response.done"))
      (list :type 'done :reason 'stop))
     ((equal type "response.failed")
      (list :type 'backend-error :content event))
     (t nil))))

(defun e-openai-codex-parse-stream (stream-text)
  "Parse Codex Responses STREAM-TEXT into backend-neutral items."
  (let ((items nil)
        (event-summaries nil)
        (assistant-message-seen nil)
        (assistant-message-candidate nil))
    (cl-labels
        ((handle-item
          (item)
          (pcase (plist-get item :type)
            ('assistant-message
             (setq assistant-message-seen t)
             (push item items))
            ('assistant-message-candidate
             (unless assistant-message-candidate
               (setq assistant-message-candidate
                     (list :type 'assistant-message
                           :content (plist-get item :content)))))
            ('done
             (unless assistant-message-seen
               (when assistant-message-candidate
                 (push assistant-message-candidate items)
                 (setq assistant-message-seen t)))
             (push item items))
            (_
             (push item items)))))
      (dolist (chunk (split-string stream-text "\n\n" t))
        (let ((data-lines nil))
          (dolist (line (split-string chunk "\n"))
            (when (string-prefix-p "data:" line)
              (push (string-trim (substring line 5)) data-lines)))
          (when data-lines
            (let ((data (string-join (nreverse data-lines) "\n")))
              (unless (or (string-empty-p data) (equal data "[DONE]"))
                (let* ((event (e-openai-codex--parse-json data))
                       (item (e-openai-codex--event-item event)))
                  (push (e-openai-codex--event-summary event item)
                        event-summaries)
                  (when item
                    (handle-item item)))))))))
    (unless assistant-message-seen
      (when assistant-message-candidate
        (push assistant-message-candidate items)))
    (when e-openai-codex-debug
      (setq e-openai-codex--last-diagnostics
            (list :raw-response stream-text
                  :events (nreverse event-summaries))))
    (nreverse items)))

(cl-defun e-openai-codex-backend-create
    (&key auth-file base-url request-function name)
  "Create an OpenAI/Codex backend named NAME.
AUTH-FILE points at Codex-managed auth.  BASE-URL defaults to ChatGPT's Codex
backend.  REQUEST-FUNCTION is injectable for tests."
  (e-backend-create
   :name (or name "openai-codex")
   :stream
   (cl-function
    (lambda (&key messages options on-item)
      (let* ((auth (e-openai-codex-read-auth auth-file))
             (body (json-encode
                    (e-openai-codex-request-body
                     :messages messages
                     :options options
                     :tools (plist-get options :tools))))
             (session-id (plist-get options :session-id))
             (response (funcall (or request-function
                                    #'e-openai-codex--http-request)
                                :url (e-openai-codex-url base-url)
                                :headers (e-openai-codex--headers auth session-id)
                                :body body)))
        (dolist (item (e-openai-codex-parse-stream response))
          (funcall on-item item)))))))

(cl-defun e-openai-codex-create-harness
    (&key auth-file base-url request-function model)
  "Create a harness configured for ChatGPT-backed Codex.
AUTH-FILE, BASE-URL, and REQUEST-FUNCTION configure the backend adapter.  MODEL
is written into backend-neutral turn options by the default context strategy
path used by `e-harness-prompt'."
  (e-harness-create
   :backend (e-openai-codex-backend-create
             :auth-file auth-file
             :base-url base-url
             :request-function request-function)
   :default-options (list :model (or model e-openai-codex-default-model))))

(provide 'e-openai)

;;; e-openai.el ends here
