;;; e-mcp-stdio.el --- MCP helper-process transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the helper process, framed JSON-RPC transport, process buffers, and
;; cancellable stdio requests.  It has no catalog or capability state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-request)
(require 'e-json)
(require 'e-tools)
(require 'e-mcp-protocol)
(require 'e-mcp-transport)

(defcustom e-mcp-helper-timeout 10
  "Default number of seconds to wait for MCP helper responses."
  :type 'number
  :group 'e)

(defcustom e-mcp-node-executable "node"
  "Node executable used for the MCP helper."
  :type 'string
  :group 'e)

(defvar e-mcp-stdio--helper-process nil)
(defvar e-mcp-stdio--helper-stdout nil)
(defvar e-mcp-stdio--helper-stderr nil)
(defvar e-mcp-stdio--helper-next-id 0)
(defvar e-mcp-stdio--latest-diagnostics nil)

(defun e-mcp-stdio--directory ()
  "Return the directory containing this file."
  (file-name-directory
   (file-truename
    (or load-file-name
        buffer-file-name
        (locate-library "e-mcp")
        default-directory))))

(defun e-mcp-stdio--helper-script ()
  "Return the MCP helper script path."
  (expand-file-name "e-mcp-helper.mjs" (e-mcp-stdio--directory)))

(defun e-mcp-stdio--helper-command ()
  "Return the command used to start the MCP helper."
  (list e-mcp-node-executable (e-mcp-stdio--helper-script)))

(defun e-mcp-stdio--buffer-string (buffer)
  "Return BUFFER contents, or an empty string when BUFFER is not live."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun e-mcp-stdio--parse-json (text)
  "Parse TEXT into the canonical JSON value representation."
  (e-json-parse-string text))

(defun e-mcp-stdio--plist-without (plist key)
  "Return PLIST without KEY and its value."
  (let (result)
    (while plist
      (let ((current-key (pop plist))
            (value (pop plist)))
        (unless (eq current-key key)
          (setq result (append result (list current-key value))))))
    result))

(defun e-mcp-stdio--helper-live-p ()
  "Return non-nil when the MCP helper process is live."
  (and e-mcp-stdio--helper-process
       (process-live-p e-mcp-stdio--helper-process)))

(defun e-mcp-stdio--helper-ensure ()
  "Start and return the MCP helper process."
  (unless (e-mcp-stdio--helper-live-p)
    (setq e-mcp-stdio--helper-stdout (generate-new-buffer " *e-mcp-stdout*"))
    (setq e-mcp-stdio--helper-stderr (generate-new-buffer " *e-mcp-stderr*"))
    (setq e-mcp-stdio--helper-process
          (make-process
           :name "e-mcp-helper"
           :buffer nil
           :stderr e-mcp-stdio--helper-stderr
           :command (e-mcp-stdio--helper-command)
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :filter
           (lambda (_process text)
             (when (buffer-live-p e-mcp-stdio--helper-stdout)
               (with-current-buffer e-mcp-stdio--helper-stdout
                 (goto-char (point-max))
                 (insert text))))))
    (set-process-query-on-exit-flag e-mcp-stdio--helper-process nil))
  e-mcp-stdio--helper-process)

(defun e-mcp-stdio--stderr-string ()
  "Return captured MCP helper stderr diagnostics."
  (string-trim (e-mcp-stdio--buffer-string e-mcp-stdio--helper-stderr)))

(defun e-mcp-stdio--next-id ()
  "Return next helper protocol request id."
  (setq e-mcp-stdio--helper-next-id (1+ e-mcp-stdio--helper-next-id)))

(defun e-mcp-stdio--response-for-id (id)
  "Return parsed MCP helper response for ID when available."
  (when (buffer-live-p e-mcp-stdio--helper-stdout)
    (with-current-buffer e-mcp-stdio--helper-stdout
      (save-excursion
        (goto-char (point-min))
        (let (response)
          (while (and (not response) (not (eobp)))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (unless (string-empty-p (string-trim line))
                (let ((payload (e-mcp-stdio--parse-json line)))
                  (when (equal (plist-get payload :id) id)
                    (setq response payload)))))
            (forward-line 1))
          response)))))

(defun e-mcp-stdio--truthy-p (value)
  "Return non-nil when VALUE is JSON truthy for helper protocol booleans."
  (and value (not (eq value e-json-false))))

(defun e-mcp-stdio--env-entry (entry)
  "Return helper JSON shape for env ENTRY."
  (list :name (car entry) :value (cdr entry)))

(defun e-mcp-stdio--server-payload (server)
  "Return helper JSON shape for stdio SERVER."
  (append
   (list :id (e-mcp-server-id server)
         :command (vconcat (or (e-mcp-server-command server) nil))
         :env (vconcat (mapcar #'e-mcp-stdio--env-entry
                               (or (e-mcp-server-env server) nil))))
   (when (e-mcp-server-timeout server)
     (list :timeout (e-mcp-server-timeout server)))))

(defun e-mcp-stdio--servers-payload (servers)
  "Return helper JSON shape for SERVERS."
  (vconcat (mapcar #'e-mcp-stdio--server-payload servers)))

(defun e-mcp-stdio--helper-error (response fallback)
  "Signal an infrastructure error from helper RESPONSE and FALLBACK."
  (signal 'e-mcp-backend-error
          (list (or (plist-get response :error) fallback)
                (plist-get response :diagnostics))))

(defun e-mcp-stdio--helper-result (response)
  "Validate helper RESPONSE and return its result."
  (unless (and (listp response) (plist-member response :ok))
    (signal 'e-mcp-protocol-error
            (list "MCP helper returned an invalid response" response)))
  (setq e-mcp-stdio--latest-diagnostics (plist-get response :diagnostics))
  (unless (e-mcp-stdio--truthy-p (plist-get response :ok))
    (e-mcp-stdio--helper-error response "MCP helper returned an error"))
  (plist-get response :result))

(defun e-mcp-stdio--reject-sync-in-hot-path (operation)
  "Reject synchronous MCP OPERATION from marked interactive hot paths."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error operation)))

(defun e-mcp-stdio-request (op servers &rest args)
  "Send OP for SERVERS to the helper with keyword ARGS.
The optional `:transport-function' argument is an explicit transport-owner
test seam; it is removed from the wire request and never stored globally."
(e-mcp-stdio--reject-sync-in-hot-path 'e-mcp-stdio-request)
  (let* ((transport-function (plist-get args :transport-function))
         (wire-args (e-mcp-stdio--plist-without args :transport-function))
         (id (e-mcp-stdio--next-id))
         (request (append (list :id id
                                :op op
                                :servers (e-mcp-stdio--servers-payload servers))
                          wire-args))
         response)
    (setq response
          (if transport-function
              (funcall transport-function request)
            (let* ((process (e-mcp-stdio--helper-ensure))
                   (timeout (or (plist-get args :timeout)
                                e-mcp-helper-timeout))
                   (deadline (+ (float-time) timeout)))
              (process-send-string process (concat (e-json-serialize request) "\n"))
              (while (and (not response)
                          (process-live-p process)
                          (< (float-time) deadline))
                (accept-process-output process 0.01)
                (setq response (e-mcp-stdio--response-for-id id)))
              (unless response
                (if (process-live-p process)
                    (signal 'e-mcp-backend-timeout
                            (list (format "MCP helper timed out after %s seconds"
                                          timeout)))
                  (signal 'e-mcp-backend-error
                          (list (string-trim
                                 (format "MCP helper exited%s"
                                         (if (string-empty-p
                                              (e-mcp-stdio--stderr-string))
                                             ""
                                           (concat ": "
                                                  (e-mcp-stdio--stderr-string)))))))))
              response)))
    (e-mcp-stdio--helper-result response)))

(cl-defun e-mcp-stdio-request-start
    (op servers args &key on-done on-error on-event transport-function
        &allow-other-keys)
  "Start helper OP for SERVERS with keyword ARGS asynchronously.
TRANSPORT-FUNCTION is an explicit test transport seam and is not retained."
  (let* ((wire-args (e-mcp-stdio--plist-without args :transport-function))
         (id (e-mcp-stdio--next-id))
         (request (append (list :id id
                                :op op
                                :servers (e-mcp-stdio--servers-payload servers))
                          wire-args))
         (timeout (or (plist-get args :timeout)
                      e-mcp-helper-timeout))
         (settled nil)
         (reservation (e-mcp-transport-reserve 'stdio))
         poll-timer
         timeout-timer
         transport-timer
         process)
    (cl-labels
        ((cleanup ()
           (when (timerp poll-timer)
             (cancel-timer poll-timer))
           (when (timerp timeout-timer)
             (cancel-timer timeout-timer))
           (when (timerp transport-timer)
             (cancel-timer transport-timer))
           (when reservation
             (e-mcp-transport-release reservation)
             (setq reservation nil)))
         (fail (condition)
           (unless settled
             (setq settled t)
             (cleanup)
             (when on-error
               (funcall on-error condition))))
         (finish (response)
           (unless settled
             (condition-case condition
                 (let ((result (e-mcp-stdio--helper-result response)))
                   (setq settled t)
                   (cleanup)
                   (when on-done
                     (funcall on-done result)))
               (error
                (fail condition)))))
         (poll ()
           (unless settled
             (let ((response (e-mcp-stdio--response-for-id id)))
               (cond
                (response
                 (finish response))
                ((not (process-live-p process))
                 (fail
                  (list 'e-mcp-backend-error
                        (string-trim
                         (format "MCP helper exited%s"
                                 (if (string-empty-p (e-mcp-stdio--stderr-string))
                                     ""
                                   (concat ": " (e-mcp-stdio--stderr-string))))))))
                (t
                 (setq poll-timer (run-at-time 0.01 nil #'poll)))))))
         (timeout! ()
           (fail
            (list 'e-mcp-backend-timeout
                  (format "MCP helper timed out after %s seconds"
                          timeout)))))
      (if transport-function
          (progn
            (setq transport-timer
                  (run-at-time
                   0 nil
                   (lambda ()
                     (condition-case condition
                         (finish
                          (funcall transport-function request))
                       (error
                        (fail condition))))))
            (e-tools-request-create
             :cancel (lambda ()
                       (unless settled
                         (setq settled t)
                         (cleanup))
                       t)
             :metadata '(:transport timer :cancellable queued-only)))
        (let ((started nil))
          (unwind-protect
              (progn
                (setq process (e-mcp-stdio--helper-ensure))
                (process-send-string process (concat (e-json-serialize request) "\n"))
                (when on-event
                  (funcall on-event 'tool-progress
                           (list :message "MCP helper request started")))
                (setq timeout-timer (run-at-time timeout nil #'timeout!))
                (setq poll-timer (run-at-time 0 nil #'poll))
                (setq started t)
                (e-tools-request-create
                 :cancel (lambda ()
                           (unless settled
                             (setq settled t)
                             (cleanup))
                           t)
                 :metadata (list :transport 'process
                                 :process process
                                 :helper 'mcp
                                 :cancellable 'ignore-late-result)))
            (unless started
              (cleanup))))))))



(defun e-mcp-stdio-reset ()
  "Stop the helper process and clear stdio transport state."
  (when (and e-mcp-stdio--helper-process
             (process-live-p e-mcp-stdio--helper-process))
    (kill-process e-mcp-stdio--helper-process))
  (e-kill-buffer-quietly e-mcp-stdio--helper-stdout)
  (e-kill-buffer-quietly e-mcp-stdio--helper-stderr)
  (setq e-mcp-stdio--helper-process nil
        e-mcp-stdio--helper-stdout nil
        e-mcp-stdio--helper-stderr nil
        e-mcp-stdio--helper-next-id 0
        e-mcp-stdio--latest-diagnostics nil)
  t)

(defun e-mcp-stdio-diagnostics ()
  "Return diagnostics from the most recent helper response."
  e-mcp-stdio--latest-diagnostics)

(defun e-mcp-stdio--tools-from-result (servers result)
  "Return tools parsed from helper RESULT for SERVERS."
  (let ((fallback-server-id (when (= (length servers) 1)
                              (e-mcp-server-id (car servers))))
        tools)
    (dolist (item (append (plist-get result :tools) nil))
      (let ((server-id (or (plist-get item :serverId)
                           (plist-get item :server-id)
                           fallback-server-id)))
        (unless server-id
          (signal 'e-mcp-protocol-error
                  (list "MCP helper omitted server id from multi-server catalog"
                        item)))
        (push (e-mcp-protocol-tool-from-wire server-id item) tools)))
    (nreverse tools)))

(defun e-mcp-stdio-list-tools (servers &optional transport-function)
  "List tools for stdio SERVERS as typed MCP tool values.
TRANSPORT-FUNCTION is an explicit test-only transport seam."
  (e-mcp-stdio--tools-from-result
   servers
   (e-mcp-stdio-request "list-tools" servers
                        :transport-function transport-function)))

(cl-defun e-mcp-stdio-list-tools-start
    (servers &key on-done on-error on-event transport-function)
  "Asynchronously list tools for stdio SERVERS.
TRANSPORT-FUNCTION is an explicit test-only transport seam."
  (e-mcp-stdio-request-start
   "list-tools" servers nil
   :transport-function transport-function
   :on-done (lambda (result)
              (when on-done
                (funcall on-done
                         (e-mcp-stdio--tools-from-result servers result))))
   :on-error on-error :on-event on-event))

(defun e-mcp-stdio-call-tool
    (servers server-id tool-name arguments &optional transport-function)
  "Call TOOL-NAME on SERVER-ID through stdio SERVERS.
TRANSPORT-FUNCTION is an explicit test-only transport seam."
  (e-mcp-stdio-request
   "call-tool" servers
   :server server-id :tool tool-name :arguments arguments
   :transport-function transport-function))

(cl-defun e-mcp-stdio-call-tool-start
    (servers server-id tool-name arguments
             &key on-done on-error on-event transport-function)
  "Asynchronously call TOOL-NAME on SERVER-ID through stdio SERVERS.
TRANSPORT-FUNCTION is an explicit test-only transport seam."
  (e-mcp-stdio-request-start
   "call-tool" servers
   (list :server server-id :tool tool-name :arguments arguments)
   :transport-function transport-function
   :on-done on-done :on-error on-error :on-event on-event))

(defun e-mcp-stdio-refresh (servers &optional transport-function)
  "Refresh stdio SERVERS and return the helper result.
TRANSPORT-FUNCTION is an explicit test-only transport seam."
  (e-mcp-stdio-request "refresh" servers
                       :transport-function transport-function))

(cl-defun e-mcp-stdio-refresh-start
    (servers &key on-done on-error on-event transport-function)
  "Asynchronously refresh stdio SERVERS.
TRANSPORT-FUNCTION is an explicit test-only transport seam."
  (e-mcp-stdio-request-start
   "refresh" servers nil :transport-function transport-function
   :on-done on-done :on-error on-error
   :on-event on-event))

(provide 'e-mcp-stdio)

;;; e-mcp-stdio.el ends here
