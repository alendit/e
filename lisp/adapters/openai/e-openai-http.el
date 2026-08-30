;;; e-openai-http.el --- OpenAI HTTP transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns url.el request lifecycle, cancellation, idle deadlines, and structured
;; HTTP error responses.  Wire mapping and provider policy stay elsewhere.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'e-backend)
(require 'e-request)
(require 'e-openai-profile)
(require 'e-openai-diagnostics)

(defun e-openai-http--header-bytes (value)
  "Return VALUE as an ASCII byte string suitable for `url-request-extra-headers'."
  (encode-coding-string (format "%s" value) 'us-ascii))

(defun e-openai-http--header-list (headers)
  "Return HEADERS with names and values normalized to byte strings."
  (mapcar (lambda (header)
            (cons (e-openai-http--header-bytes (car header))
                  (e-openai-http--header-bytes (cdr header))))
          headers))

(cl-defstruct
    (e-openai-http--response
     (:constructor e-openai-http--response-create))
  body
  status
  retry-after)

(cl-defun e-openai-http-request (&key url headers body)
  "POST BODY to URL with HEADERS and return its complete response.
Successful responses are returned as body text.  HTTP errors carry their body,
status, and retry metadata in an `e-openai-http--response'."
  (e-openai-provider-reject-sync-in-hot-path 'e-openai-http-request)
  (let ((response nil)
        (failure nil)
        (done nil))
    (e-openai-http-request-start
     :url url
     :headers headers
     :body body
     :on-complete (lambda (value)
                    (setq response value)
                    (setq done t))
     :on-error (lambda (err)
                 (setq failure err)
                 (setq done t)))
    (while (not done)
      (accept-process-output nil 0.01))
    (when failure
      (signal (car failure) (cdr failure)))
    response))

(defun e-openai-http--make-response-text (buffer)
  "Return response body text from url.el BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n" nil 'move)
    (buffer-substring-no-properties (point) (point-max))))

(defun e-openai-http--make-response-status (callback-status)
  "Return numeric HTTP status from the current buffer or CALLBACK-STATUS."
  (or (and (boundp 'url-http-response-status)
           (numberp url-http-response-status)
           url-http-response-status)
      (let* ((url-error (plist-get callback-status :error))
             (http-tail (and (listp url-error) (memq 'http url-error))))
        (and (numberp (cadr http-tail)) (cadr http-tail)))))

(defun e-openai-http--make-response-header (name)
  "Return response header NAME from the current url.el buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (limit (or (and (re-search-forward "\r?\n\r?\n" nil t)
                          (match-beginning 0))
                     (point-max))))
      (goto-char (point-min))
      (when (re-search-forward
             (format "^%s:[ \t]*\\([^\r\n]*\\)" (regexp-quote name))
             limit t)
        (string-trim (match-string-no-properties 1))))))

(defun e-openai-http--retry-after ()
  "Return a numeric Retry-After response delay from the current buffer."
  (when-let ((value (e-openai-http--make-response-header "Retry-After")))
    (when (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value)
      (string-to-number value))))

(defun e-openai-http--make-response (body callback-status)
  "Return BODY with HTTP metadata from CALLBACK-STATUS when material."
  (let ((status (e-openai-http--make-response-status callback-status))
        (retry-after (e-openai-http--retry-after)))
    (if (and (numberp status) (>= status 400))
        (e-openai-http--response-create
         :body body :status status :retry-after retry-after)
      body)))

(defun e-openai-http--kill-request-buffer (buffer)
  "Cancel any live request process attached to BUFFER and kill BUFFER.
Real/pipe helper processes are force-killed; network processes are deleted.
The exit query is disabled and `kill-buffer-query-functions' is bound off so a
still-live process can never raise the blocking \"has a running process; kill
it?\" prompt that stalls a headless agent."
  (when (buffer-live-p buffer)
    (when-let ((process (get-buffer-process buffer)))
      (when (process-live-p process)
        (set-process-query-on-exit-flag process nil)
        (if (memq (process-type process) '(real pipe))
            (kill-process process)
          (delete-process process))))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(cl-defun e-openai-http-request-start
    (&key url headers body on-complete on-error)
  "POST BODY to URL with HEADERS asynchronously.
ON-COMPLETE receives body text or a structured HTTP error response.  ON-ERROR
receives an Emacs condition list.  Return a cancellable `e-backend-request'
handle."
  (let ((url-request-method "POST")
        (url-request-extra-headers (e-openai-http--header-list headers))
        (url-request-data (encode-coding-string body 'utf-8))
        (timeout e-openai-request-timeout-seconds)
        request-buffer
        timeout-timer
        settled)
    (cl-labels
        ((cancel-timeout ()
           (when (timerp timeout-timer)
             (cancel-timer timeout-timer))
           (setq timeout-timer nil))
         (cleanup (buffer)
           (cancel-timeout)
           (e-openai-http--kill-request-buffer buffer))
         (settle-timeout ()
           (unless settled
             (setq settled t)
             (cleanup request-buffer)
             (when on-error
               (funcall
                on-error
                (list 'e-openai-request-timeout
                      (format "OpenAI request timed out after %s seconds"
                              timeout))))))
         (rearm-timeout ()
           (when (and timeout (not settled))
             (cancel-timeout)
             (setq timeout-timer
                   (run-at-time timeout nil #'settle-timeout))))
         (track-response-progress (&rest _)
           ;; `url-retrieve' calls its completion callback only after the full
           ;; body arrives.  Its private response buffer changes on each
           ;; network chunk, which is the HTTP transport's idle-progress edge.
           (rearm-timeout))
         (handle-callback (status)
           (unless settled
             (setq settled t)
             (let ((buffer (current-buffer)))
               (unwind-protect
                   (condition-case err
                       (let* ((url-error (plist-get status :error))
                              (response-text
                               (e-openai-http--make-response-text buffer))
                              (response
                               (e-openai-http--make-response
                                response-text status)))
                         (if url-error
                             (if (or (e-openai-http--response-p response)
                                     (not (string-empty-p
                                           (string-trim response-text))))
                                   (when on-complete
                                     (funcall on-complete response))
                               (when on-error
                                 (funcall
                                  on-error
                                  (list 'error
                                        (e-format-safe
                                         "OpenAI request failed: %S"
                                         url-error)))))
                           (when on-complete
                             (funcall on-complete response))))
                     (error
                      (when on-error
                        (funcall on-error err))))
                 (cleanup buffer))))))
      (setq request-buffer
            (url-retrieve
             url
             (lambda (status)
               (handle-callback status))
             nil
             'silent
             nil))
      (when (buffer-live-p request-buffer)
        (with-current-buffer request-buffer
          (add-hook 'after-change-functions #'track-response-progress nil t)))
      (rearm-timeout))
    (e-backend-request-create
     :cancel (lambda ()
               (unless settled
                 (setq settled t))
               (when (timerp timeout-timer)
                 (cancel-timer timeout-timer))
               (setq timeout-timer nil)
               (e-openai-http--kill-request-buffer request-buffer)
               t)
     :metadata (append
                (list :transport 'url-retrieve
                      :url url
                      :timeout-seconds timeout
                      :cancellable t)
                (e-openai-diagnostics-url-metadata url)))))



(defun e-openai-http-response-p (value)
  "Return non-nil when VALUE is a structured HTTP error response."
  (e-openai-http--response-p value))

(defun e-openai-http-response-body (response)
  "Return body text from structured HTTP RESPONSE."
  (e-openai-http--response-body response))

(defun e-openai-http-response-status (response)
  "Return status from structured HTTP RESPONSE."
  (e-openai-http--response-status response))

(defun e-openai-http-response-retry-after (response)
  "Return Retry-After seconds from structured HTTP RESPONSE."
  (e-openai-http--response-retry-after response))

(defun e-openai-http-response-body-text (response)
  "Return body text from RESPONSE, whether plain or structured."
  (if (e-openai-http-response-p response)
      (e-openai-http-response-body response)
    (or response "")))

(defun e-openai-http-error-p (response)
  "Return non-nil when RESPONSE is a structured HTTP error."
  (and (e-openai-http-response-p response)
       (let ((status (e-openai-http-response-status response)))
         (and (numberp status) (>= status 400)))))

(provide 'e-openai-http)

;;; e-openai-http.el ends here
