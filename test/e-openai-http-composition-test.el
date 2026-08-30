;;; e-openai-http-composition-test.el --- Tests for e OpenAI/Codex backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for OpenAI HTTP request lifecycle and timeout composition.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-backend)
(require 'e-dev-profile)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-loop)
(require 'e-openai)
(require 'url-http)

(load (expand-file-name "e-openai-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-openai-test-default-http-request-start-accepts-keyword-arguments ()
  "The default async HTTP requester accepts the backend keyword call shape."
  (let (captured-url captured-method captured-headers captured-body)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _args)
                 (setq captured-url url)
                 (setq captured-method url-request-method)
                 (setq captured-headers url-request-extra-headers)
                 (setq captured-body url-request-data)
                 (let ((buffer (generate-new-buffer " *e-openai-test-http*")))
                   (with-current-buffer buffer
                     (insert "HTTP/1.1 200 OK\n\n"
                             "data: {\"type\":\"response.completed\"}\n\n"))
                   (with-current-buffer buffer
                     (funcall callback nil))
                   buffer))))
      (let (response error)
        (e-openai-http-request-start
         :url "https://example.test/codex/responses"
         :headers '(("Authorization" . "Bearer test"))
         :body "{}"
         :on-complete (lambda (value) (setq response value))
         :on-error (lambda (err) (setq error err)))
        (should-not error)
        (should (equal response
                       "data: {\"type\":\"response.completed\"}\n\n")))
      (should (equal captured-url "https://example.test/codex/responses"))
      (should (equal captured-method "POST"))
      (should (equal captured-headers '(("Authorization" . "Bearer test"))))
      (should (equal (decode-coding-string captured-body 'utf-8) "{}")))))

(ert-deftest e-openai-test-sync-http-request-rejects-hot-path-before-start ()
  "The synchronous Codex HTTP wrapper fails before starting transport in hot paths."
  (let (started)
    (cl-letf (((symbol-function 'e-openai-http-request-start)
               (lambda (&rest _args)
                 (setq started t)
                 (error "transport should not start"))))
      (let ((err (should-error
                  (e-request-with-hot-path 'openai-sync-http
                    (e-openai-http-request
                     :url "https://example.test/codex/responses"
                     :headers nil
                     :body "{}"))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err)
                       '(e-openai-http-request openai-sync-http))))
      (should-not started))))

(ert-deftest e-openai-test-default-http-request-start-normalizes-header-bytes ()
  "Multibyte ASCII headers must not make a Unicode request body invalid."
  (let (captured-request)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _args)
                 (ignore url)
                 (let ((buffer (generate-new-buffer " *e-openai-test-http*")))
                   (with-current-buffer buffer
                     (mm-disable-multibyte)
                     (setq url-current-object (url-generic-parse-url url)
                           url-http-target-url url-current-object
                           url-http-method url-request-method
                           url-http-version "1.1"
                           url-http-extra-headers url-request-extra-headers
                           url-http-data url-request-data
                           url-http-proxy nil
                           url-http-referer nil
                           url-http-attempt-keepalives t
                           url-extensions-header nil
                           url-mime-encoding-string nil
                           url-mime-charset-string nil
                           url-mime-language-string nil
                           url-mime-accept-string nil
                           url-privacy-level nil
                           url-user-agent nil
                           url-http-real-basic-auth-storage nil)
                     (setq captured-request (url-http-create-request))
                     (insert "HTTP/1.1 200 OK\n\n"
                             "data: {\"type\":\"response.completed\"}\n\n"))
                   (with-current-buffer buffer
                     (funcall callback nil))
                   buffer))))
      (let (response error)
        (e-openai-http-request-start
         :url "https://example.test/codex/responses"
         :headers `(("Authorization" . ,(string-to-multibyte "Bearer test"))
                    ("Content-Type" . "application/json"))
         :body (json-encode '(:text "▌ unicode body"))
         :on-complete (lambda (value) (setq response value))
         :on-error (lambda (err) (setq error err)))
        (should-not error)
        (should (equal response
                       "data: {\"type\":\"response.completed\"}\n\n")))
      (should (= (string-bytes captured-request)
                 (length captured-request))))))

(ert-deftest e-openai-test-default-http-request-times-out ()
  "A default url-retrieve request that never calls back times out visibly."
  (let ((e-openai-request-timeout-seconds 0.01)
        (buffer nil)
        (error-count 0)
        (complete-count 0)
        error)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url _callback &rest _args)
                 (setq buffer
                       (generate-new-buffer " *e-openai-test-http*"))
                 buffer)))
      (e-openai-http-request-start
       :url "https://example.test/codex/responses"
       :headers '(("Authorization" . "Bearer test"))
       :body "{}"
       :on-complete (lambda (_value)
                      (setq complete-count (1+ complete-count)))
       :on-error (lambda (err)
                   (setq error-count (1+ error-count))
                   (setq error err)))
      (should (e-openai-test--wait-until (lambda () error) 0.2))
      (should (eq (car error) 'e-openai-request-timeout))
      (should (= error-count 1))
      (should (= complete-count 0))
      (should-not (buffer-live-p buffer)))))

(ert-deftest e-openai-test-default-http-timeout-rearms-on-response-progress ()
  "Incoming HTTP response bytes extend the adapter's idle deadline."
  (let ((e-openai-request-timeout-seconds 0.08)
        buffer
        error)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url _callback &rest _args)
                 (setq buffer
                       (generate-new-buffer " *e-openai-test-http*"))
                 buffer)))
      (e-openai-http-request-start
       :url "https://example.test/codex/responses"
       :headers '(("Authorization" . "Bearer test"))
       :body "{}"
       :on-complete #'ignore
       :on-error (lambda (err) (setq error err)))
      (e-openai-test--wait-until (lambda () nil) 0.05)
      (with-current-buffer buffer
        (insert "data: {\"type\":\"response.created\"}\n\n"))
      ;; This crosses the original absolute deadline but remains inside the
      ;; idle interval measured from the response-buffer insertion above.
      (e-openai-test--wait-until (lambda () nil) 0.05)
      (should-not error)
      (should (buffer-live-p buffer))
      ;; A genuinely idle stream still fails once the re-armed deadline passes.
      (should (e-openai-test--wait-until (lambda () error) 0.15))
      (should (eq (car error) 'e-openai-request-timeout))
      (should-not (buffer-live-p buffer)))))

(ert-deftest e-openai-test-http-idle-timeout-rearms-on-real-url-chunks ()
  "Actual `url.el' process-filter chunks extend an explicit idle timeout."
  (let* ((e-openai-request-timeout-seconds 0.12)
         (url-proxy-services nil)
         (server nil)
         (clients nil)
         (timers nil)
         response
         error)
    (cl-labels
        ((send-chunk (client data)
           (when (process-live-p client)
             (process-send-string
              client
              (format "%x\r\n%s\r\n" (string-bytes data) data))))
         (serve-request (client data)
           (when (string-match-p "\r\n\r\n" data)
             (set-process-filter client #'ignore)
             (process-send-string
              client
              (concat "HTTP/1.1 200 OK\r\n"
                      "Content-Type: text/event-stream\r\n"
                      "Transfer-Encoding: chunked\r\n"
                      "Connection: close\r\n\r\n"))
             (push (run-at-time
                    0.08 nil #'send-chunk client
                    "data: {\"type\":\"response.created\"}\n\n")
                   timers)
             (push (run-at-time
                    0.16 nil #'send-chunk client
                    "data: {\"type\":\"response.completed\"}\n\n")
                   timers)
             (push (run-at-time
                    0.24 nil
                    (lambda ()
                      (when (process-live-p client)
                        (process-send-string client "0\r\n\r\n")
                        (process-send-eof client))))
                   timers))))
      (unwind-protect
          (progn
            (setq server
                  (make-network-process
                   :name "e-openai-stream-server"
                   :server t
                   :host 'local
                   :service t
                   :noquery t
                   :log (lambda (_server client _message)
                          (push client clients)
                          (set-process-query-on-exit-flag client nil)
                          (set-process-filter client #'serve-request))))
            (let ((port (process-contact server :service)))
              (e-openai-http-request-start
               :url (format "http://127.0.0.1:%s/responses" port)
               :headers '(("Content-Type" . "application/json"))
               :body "{}"
               :on-complete (lambda (value) (setq response value))
               :on-error (lambda (err) (setq error err))))
            (should (e-openai-test--wait-until
                     (lambda () (or response error)) 1.0))
            (should-not error)
            (should (string-match-p "response.completed" response)))
        (dolist (timer timers)
          (when (timerp timer)
            (cancel-timer timer)))
        (dolist (client clients)
          (when (process-live-p client)
            (delete-process client)))
        (when (process-live-p server)
          (delete-process server))))))

(ert-deftest e-openai-test-timeout-settles-once ()
  "A late url callback after timeout does not settle the request again."
  (let ((e-openai-request-timeout-seconds 0.01)
        (callback nil)
        (error-count 0)
        (complete-count 0)
        error)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url cb &rest _args)
                 (setq callback cb)
                 (generate-new-buffer " *e-openai-test-http*"))))
      (e-openai-http-request-start
       :url "https://example.test/codex/responses"
       :headers '(("Authorization" . "Bearer test"))
       :body "{}"
       :on-complete (lambda (_value)
                      (setq complete-count (1+ complete-count)))
       :on-error (lambda (err)
                   (setq error-count (1+ error-count))
                   (setq error err)))
      (should (e-openai-test--wait-until (lambda () error) 0.2))
      (with-temp-buffer
        (insert "HTTP/1.1 200 OK\n\n"
                "data: {\"type\":\"response.completed\"}\n\n")
        (funcall callback nil))
      (should (eq (car error) 'e-openai-request-timeout))
      (should (= error-count 1))
      (should (= complete-count 0)))))

(provide 'e-openai-http-composition-test)

;;; e-openai-http-composition-test.el ends here
