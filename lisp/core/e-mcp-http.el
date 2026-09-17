;;; e-mcp-http.el --- MCP streamable HTTP transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns HTTP session headers, initialization, JSON-RPC POST lifecycle, and
;; cancellation.  Catalog policy is kept in e-mcp-client.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'e-request)
(require 'e-json)
(require 'e-tools)
(require 'e-mcp-protocol)
(require 'e-mcp-transport)

(defvar e-mcp-http--sessions nil
  "Alist of (server-id . session-plist) for HTTP MCP sessions.")

(defun e-mcp-http--reject-sync-in-hot-path (operation)
  "Reject synchronous HTTP MCP OPERATION from a marked hot path."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error operation)))

(defun e-mcp-http--parse-json (text)
  "Parse TEXT into the canonical JSON value representation."
  (e-json-parse-string text))

;;; HTTP (Streamable HTTP) transport
;;
;; For servers with :url set, we talk MCP JSON-RPC directly from Emacs over
;; HTTP POST.  No helper process needed.

(defun e-mcp-http--http-session (server)
  "Return or create an HTTP session for SERVER."
  (let* ((id (e-mcp-server-id server))
         (existing (assoc id e-mcp-http--sessions)))
    (if existing
        (cdr existing)
      (let ((session (list :url (e-mcp-server-url server)
                           :headers (e-mcp-server-http-headers server)
                           :session-id nil
                           :initialized nil
                           :next-id 0)))
        (push (cons id session) e-mcp-http--sessions)
        session))))

(defun e-mcp-http--http-session-reset (server-id)
  "Remove cached HTTP session for SERVER-ID."
  (setq e-mcp-http--sessions
        (assoc-delete-all server-id e-mcp-http--sessions)))

(defun e-mcp-http--http-request-headers (session)
  "Return HTTP request headers for SESSION."
  (let ((headers (plist-get session :headers))
        (session-id (plist-get session :session-id))
        result)
    (push '("Content-Type" . "application/json") result)
    (push '("Accept" . "application/json, text/event-stream") result)
    (when session-id
      (push (cons "Mcp-Session-Id" session-id) result))
    (dolist (entry headers)
      (push entry result))
    (nreverse result)))

(defun e-mcp-http--http-next-id (session)
  "Return and increment the next JSON-RPC id for SESSION."
  (let ((id (1+ (plist-get session :next-id))))
    (plist-put session :next-id id)
    id))

(defun e-mcp-http--http-response-body (buffer)
  "Extract HTTP response body from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (buffer-substring-no-properties (point) (point-max)))))

(defun e-mcp-http--http-session-header (buffer)
  "Return the Mcp-Session-Id response header value in BUFFER, or nil.
Only the response header region (before the blank line that ends the
headers) is searched."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((header-end (save-excursion
                        (if (re-search-forward "\r?\n\r?\n" nil t)
                            (point)
                          (point-max))))
          (case-fold-search t))
      (when (re-search-forward
             "^mcp-session\\(?:-id\\)?:[ \t]*\\(.+?\\)[ \t\r]*$"
             header-end t)
        (match-string 1)))))

(defun e-mcp-http--http-result-from-buffer (session buffer url)
  "Return parsed JSON-RPC result for SESSION from response BUFFER at URL."
  (let* ((body (e-mcp-http--http-response-body buffer))
         (_ (unless (and body (not (string-empty-p (string-trim body))))
              (signal 'e-mcp-backend-error
                      (list (format "Empty response from MCP HTTP server %s"
                                    url)))))
         (response (e-mcp-http--parse-json body))
         ;; Capture Mcp-Session-Id header so follow-up requests stay on
         ;; the same MCP session.
         (resp-session-id (e-mcp-http--http-session-header buffer)))
    (when resp-session-id
      (plist-put session :session-id resp-session-id))
    (when (plist-get response :error)
      (let ((err-obj (plist-get response :error)))
        (signal 'e-mcp-backend-error
                (list (or (plist-get err-obj :message)
                          (e-format-safe "%S" err-obj))))))
    (plist-get response :result)))

(defun e-mcp-http--http-post (session method params)
  "Send a JSON-RPC METHOD call with PARAMS to the MCP HTTP server in SESSION.
Return the parsed JSON-RPC result on success, signal on error."
  (e-mcp-http--reject-sync-in-hot-path 'e-mcp-http--http-post)
  (let* ((id (e-mcp-http--http-next-id session))
         (url (plist-get session :url))
         (timeout (or e-mcp-helper-timeout 10))
         (payload (e-json-serialize
                   (list :jsonrpc "2.0"
                         :id id
                         :method method
                         :params params)))
         (url-request-method "POST")
         (url-request-extra-headers (e-mcp-http--http-request-headers session))
         (url-request-data (encode-coding-string payload 'utf-8))
         (buffer (condition-case err
                     (url-retrieve-synchronously url 'silent nil timeout)
                   (error
                    (signal 'e-mcp-backend-error
                            (list (e-format-safe
                                   "HTTP request to %s failed: %S" url err)))))))
    (unwind-protect
        (e-mcp-http--http-result-from-buffer session buffer url)
      (e-kill-buffer-quietly buffer))))

(cl-defun e-mcp-http--http-post-start
    (session method params &key on-done on-error on-event &allow-other-keys)
  "Start a JSON-RPC METHOD call with PARAMS to HTTP SESSION."
  (let* ((id (e-mcp-http--http-next-id session))
         (url (plist-get session :url))
         (timeout (or e-mcp-helper-timeout 10))
         (payload (e-json-serialize
                   (list :jsonrpc "2.0"
                         :id id
                         :method method
                         :params params)))
         (settled nil)
         (reservation (e-mcp-transport-reserve 'http))
         timer
         buffer)
    (cl-labels
        ((cleanup ()
           (when (timerp timer)
             (cancel-timer timer))
           (e-kill-buffer-quietly buffer)
           (when reservation
             (e-mcp-transport-release reservation)
             (setq reservation nil)))
         (fail (condition)
           (unless settled
             (setq settled t)
             (cleanup)
             (when on-error
               (funcall on-error condition))))
         (finish (status)
           (unless settled
             (condition-case condition
                 (progn
                   (when-let ((transport-error (plist-get status :error)))
                     (signal 'e-mcp-backend-error
                             (list (e-format-safe
                                    "HTTP request to %s failed: %S"
                                    url transport-error))))
                   (let ((result (e-mcp-http--http-result-from-buffer
                                  session (current-buffer) url)))
                     (setq settled t)
                     (cleanup)
                     (when on-done
                       (funcall on-done result))))
               (error
                (fail condition)))))
         (timeout! ()
           (fail
            (list 'e-mcp-backend-timeout
                  (format "MCP HTTP request timed out after %s seconds"
                          timeout)))))
      (let ((started nil)
            (url-request-method "POST")
            (url-request-extra-headers (e-mcp-http--http-request-headers session))
            (url-request-data (encode-coding-string payload 'utf-8)))
        (unwind-protect
            (progn
              (setq buffer
                    (condition-case condition
                        (url-retrieve url #'finish nil 'silent)
                      (error
                       (signal 'e-mcp-backend-error
                               (list (format "HTTP request to %s failed: %S"
                                             url condition))))))
              (when on-event
                (funcall on-event 'tool-progress
                         (list :message "MCP HTTP request started")))
              (setq timer (run-at-time timeout nil #'timeout!))
              (setq started t)
              (e-tools-request-create
               :cancel (lambda ()
                         (unless settled
                           (setq settled t)
                           (cleanup))
                         t)
               :metadata (list :transport 'url
                               :url url
                               :method method
                               :cancellable 'ignore-late-result)))
          (unless started
            (cleanup)))))))

(defun e-mcp-http--http-notify (session method params)
  "Send a JSON-RPC notification (no id, no response expected) to SESSION."
  (e-mcp-http--reject-sync-in-hot-path 'e-mcp-http--http-notify)
  (let* ((url (plist-get session :url))
         (payload (e-json-serialize
                   (list :jsonrpc "2.0"
                         :method method
                         :params params)))
         (url-request-method "POST")
         (url-request-extra-headers (e-mcp-http--http-request-headers session))
         (url-request-data (encode-coding-string payload 'utf-8))
         (buffer (ignore-errors
                   (url-retrieve-synchronously url 'silent nil 5))))
    (e-kill-buffer-quietly buffer)))

(defun e-mcp-http--http-notify-start (session method params)
  "Send a JSON-RPC notification to SESSION asynchronously."
  (let* ((url (plist-get session :url))
         (payload (e-json-serialize
                   (list :jsonrpc "2.0"
                         :method method
                         :params params)))
         (url-request-method "POST")
         (url-request-extra-headers (e-mcp-http--http-request-headers session))
         (url-request-data (encode-coding-string payload 'utf-8)))
    (url-retrieve
     url
     (lambda (_status)
       (e-kill-buffer-quietly (current-buffer)))
     nil
     'silent)))

(defun e-mcp-http--http-initialize (session)
  "Send MCP initialize and initialized notification for SESSION."
  (unless (plist-get session :initialized)
    (e-mcp-http--http-post session "initialize"
                      (list :protocolVersion "2024-11-05"
                            :capabilities nil
                            :clientInfo (list :name "e-mcp" :version "0.1.0")))
    (e-mcp-http--http-notify session "notifications/initialized" nil)
    (plist-put session :initialized t)))

(cl-defun e-mcp-http--http-initialize-start
    (session &key on-done on-error on-event &allow-other-keys)
  "Start MCP HTTP initialization for SESSION asynchronously."
  (if (plist-get session :initialized)
      (let ((settled nil)
            timer)
        (setq timer
              (run-at-time
               0 nil
               (lambda ()
                 (unless settled
                   (setq settled t)
                   (when on-done
                     (funcall on-done t))))))
        (e-tools-request-create
         :cancel (lambda ()
                   (unless settled
                     (setq settled t)
                     (when (timerp timer)
                       (cancel-timer timer)))
                   t)
         :metadata '(:transport timer
                     :method initialize
                     :cancellable queued-only)))
    (let (child-request
          settled)
      (cl-labels
          ((fail (condition)
             (unless settled
               (setq settled t)
               (when on-error
                 (funcall on-error condition))))
           (finish (_result)
             (unless settled
               (setq settled t)
               (e-mcp-http--http-notify-start
                session "notifications/initialized" nil)
               (plist-put session :initialized t)
               (when on-done
                 (funcall on-done t)))))
        (setq child-request
              (e-mcp-http--http-post-start
               session "initialize"
               (list :protocolVersion "2024-11-05"
                     :capabilities nil
                     :clientInfo (list :name "e-mcp" :version "0.1.0"))
               :on-done #'finish
               :on-error #'fail
               :on-event on-event))
        (e-tools-request-create
         :cancel (lambda ()
                   (unless settled
                     (setq settled t)
                     (when child-request
                       (e-tools-cancel-request child-request)))
                   t)
         :metadata '(:transport url
                     :method initialize
                     :cancellable ignore-late-result))))))

(defun e-mcp-http--http-tools-from-result (server result)
  "Return tools for SERVER parsed from HTTP tools/list RESULT."
  (mapcar
   (lambda (item)
     (e-mcp-protocol-tool-from-wire (e-mcp-server-id server) item))
   (append (plist-get result :tools) nil)))


(defun e-mcp-http-list-tools (server)
  "Return MCP tools for HTTP SERVER."
  (let ((session (e-mcp-http--http-session server)))
    (e-mcp-http--http-initialize session)
    (let ((result (e-mcp-http--http-post session "tools/list" nil)))
      (e-mcp-http--http-tools-from-result server result))))

(cl-defun e-mcp-http-list-tools-start
    (server &key on-done on-error on-event &allow-other-keys)
  "Start MCP tools/list for HTTP SERVER asynchronously."
  (let ((session (e-mcp-http--http-session server))
        child-request
        settled)
    (cl-labels
        ((fail (condition)
           (unless settled
             (setq settled t)
             (when on-error
               (funcall on-error condition))))
         (finish-list (result)
           (unless settled
             (condition-case condition
                 (let ((tools (e-mcp-http--http-tools-from-result server result)))
                   (setq settled t)
                   (when on-done
                     (funcall on-done tools)))
               (error
                (fail condition)))))
         (start-list (_initialized)
           (unless settled
             (setq child-request
                   (e-mcp-http--http-post-start
                    session "tools/list" nil
                    :on-done #'finish-list
                    :on-error #'fail
                    :on-event on-event)))))
      (setq child-request
            (e-mcp-http--http-initialize-start
             session
             :on-done #'start-list
             :on-error #'fail
             :on-event on-event))
      (e-tools-request-create
       :cancel (lambda ()
                 (unless settled
                   (setq settled t)
                   (when child-request
                     (e-tools-cancel-request child-request)))
                 t)
       :metadata (list :transport 'url
                       :method "tools/list"
                       :server-id (e-mcp-server-id server)
                       :cancellable 'ignore-late-result)))))

(defun e-mcp-http-call-tool (server tool-name arguments)
  "Call TOOL-NAME with ARGUMENTS on HTTP SERVER."
  (let ((session (e-mcp-http--http-session server)))
    (e-mcp-http--http-initialize session)
    (e-mcp-http--http-post session "tools/call"
                      (list :name tool-name
                            :arguments (or arguments nil)))))

(cl-defun e-mcp-http-call-tool-start
    (server tool-name arguments &key on-done on-error on-event &allow-other-keys)
  "Start TOOL-NAME with ARGUMENTS on HTTP SERVER asynchronously."
  (let ((session (e-mcp-http--http-session server))
        child-request
        cancelled)
    (cl-labels
        ((set-child (request)
           (setq child-request request)
           request)
         (cancel-child ()
           (setq cancelled t)
           (when child-request
             (e-tools-cancel-request child-request))
           t)
         (call-tool ()
           (unless cancelled
             (set-child
              (e-mcp-http--http-post-start
               session "tools/call"
               (list :name tool-name
                     :arguments (or arguments nil))
               :on-done on-done
               :on-error on-error
               :on-event on-event)))))
      (if (plist-get session :initialized)
          (call-tool)
        (set-child
         (e-mcp-http--http-post-start
          session "initialize"
          (list :protocolVersion "2024-11-05"
                :capabilities nil
                :clientInfo (list :name "e-mcp" :version "0.1.0"))
          :on-done (lambda (_result)
                     (plist-put session :initialized t)
                     (e-mcp-http--http-notify-start
                      session "notifications/initialized" nil)
                     (call-tool))
          :on-error on-error
          :on-event on-event)))
      (e-tools-request-create
       :cancel #'cancel-child
       :metadata (list :transport 'url
                       :url (plist-get session :url)
                       :method "tools/call"
                       :cancellable 'ignore-late-result)))))

(defun e-mcp-http-refresh (server)
  "Refresh tool catalog for HTTP SERVER."
  (e-mcp-http--http-session-reset (e-mcp-server-id server))
  (e-mcp-http-list-tools server))

(defun e-mcp-http-reset ()
  "Reset all streamable HTTP MCP sessions."
  (setq e-mcp-http--sessions nil)
  t)

(provide 'e-mcp-http)

;;; e-mcp-http.el ends here
