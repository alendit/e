;;; e-web-tools-test.el --- Tests for web tool handlers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for web access tool handlers.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-request)
(require 'e-tools)
(require 'e-web-tools)
(require 'url-http)

(defun e-web-tools-test--fake-executable (directory name body)
  "Create executable NAME in DIRECTORY with shell BODY."
  (let ((file (expand-file-name name directory)))
    (write-region (concat "#!/bin/sh\n" body) nil file nil 'silent)
    (set-file-modes file #o755)
    file))

(defun e-web-tools-test--registry ()
  "Return a registry with web tools registered."
  (let ((registry (e-tools-registry-create)))
    (e-web-tools-register-search registry)
    (e-web-tools-register-fetch registry)
    (e-web-tools-register-browser registry)
    registry))

(defun e-web-tools-test--http-buffer (status headers body)
  "Return a url-style response buffer with STATUS, HEADERS, and BODY."
  (let ((buffer (generate-new-buffer " *e-web-http-test*")))
    (with-current-buffer buffer
      (insert (format "HTTP/1.1 %s Test\r\n" status))
      (dolist (header headers)
        (insert (format "%s: %s\r\n" (car header) (cdr header))))
      (insert "\r\n")
      (setq-local url-http-end-of-headers (point))
      (insert body))
    buffer))

(ert-deftest e-web-tools-test-search-normalizes-fake-bx-results ()
  "web_search invokes bx directly and returns normalized web results."
  (let* ((directory (make-temp-file "e-web-bx-" t))
         (args-file (expand-file-name "args.txt" directory))
         (bx (e-web-tools-test--fake-executable
              directory
              "bx"
              "printf '%s\\n' \"$@\" > \"$E_WEB_TEST_ARGS\"
cat <<'JSON'
{\"web\":{\"results\":[{\"title\":\"One\",\"url\":\"https://example.com/one\",\"description\":\"First snippet\",\"profile\":{\"name\":\"Example\"},\"age\":\"May 23, 2026\"},{\"title\":\"Two\",\"url\":\"https://example.org/two\",\"snippet\":\"Second snippet\",\"source\":\"Example Org\",\"date\":\"2026-05-22\"}]}}
JSON
")))
    (unwind-protect
        (let* ((e-web-bx-program bx)
               (process-environment
                (cons (concat "E_WEB_TEST_ARGS=" args-file)
                      process-environment))
               (result (e-tools-execute
                        (e-web-tools-test--registry)
                        '(:id "call-1"
                          :name "web_search"
                          :arguments (:query "emacs agents"
                                      :count 2
                                      :freshness "pw"
                                      :include_site ["gnu.org"]
                                      :exclude_site ["spam.example"]
                                      :include_raw t))))
               (content (plist-get result :content)))
          (should (equal (plist-get result :status) 'ok))
          (should (equal (with-temp-buffer
                           (insert-file-contents args-file)
                           (split-string (string-trim (buffer-string)) "\n"))
                         '("web"
                           "emacs agents"
                           "--count"
                           "2"
                           "--freshness"
                           "pw"
                           "--include-site"
                           "gnu.org"
                           "--exclude-site"
                           "spam.example")))
          (should (equal (plist-get content :capability) "web.search"))
          (should (equal (plist-get content :backend) "bx"))
          (should (equal (plist-get content :query) "emacs agents"))
          (should (equal (plist-get content :results)
                         '((:rank 1
                            :title "One"
                            :url "https://example.com/one"
                            :snippet "First snippet"
                            :source "Example"
                            :date "May 23, 2026")
                           (:rank 2
                            :title "Two"
                            :url "https://example.org/two"
                            :snippet "Second snippet"
                            :source "Example Org"
                            :date "2026-05-22"))))
          (should (plist-get content :raw)))
      (delete-directory directory t))))

(ert-deftest e-web-tools-test-search-start-returns-before-process-exits ()
  "web_search starts a cancellable process without waiting for completion."
  (let* ((directory (make-temp-file "e-web-bx-delay-" t))
         (bx (e-web-tools-test--fake-executable
              directory
              "bx-delay"
              "sleep 5
cat <<'JSON'
{\"web\":{\"results\":[]}}
JSON
"))
         result
         failure
         request)
    (unwind-protect
        (let ((e-web-bx-program bx)
              (e-web-bx-timeout 10))
          (e-request-with-blocking-primitive-guard
            (e-request-with-hot-path 'web-search
              (setq request
                    (e-tools-start
                     (e-web-tools-test--registry)
                     '(:id "call-1"
                       :name "web_search"
                       :arguments (:query "emacs agents"))
                     :context '(:interactive t)
                     :on-done (lambda (value)
                                (setq result value))
                     :on-error (lambda (err)
                                 (setq failure err))))))
          (should (e-tools-request-p request))
          (should (process-live-p
                   (plist-get (e-tools-request-metadata request) :process)))
          (should-not result)
          (should-not failure)
          (should (e-tools-cancel-request request)))
      (delete-directory directory t))))

(ert-deftest e-web-tools-test-sync-process-helper-rejects-hot-path ()
  "The synchronous process helper fails before starting a process in hot paths."
  (let (started)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _args)
                 (setq started t)
                 (error "process should not start"))))
      (let ((err (should-error
                  (e-request-with-hot-path 'web-sync-process
                    (e-web-tools--run-process "bx" nil 1 "bx"))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err)
                       '(e-web-tools--run-process web-sync-process))))
      (should-not started))))

(ert-deftest e-web-tools-test-fetch-extracts-html-text-links-and-markdown ()
  "web_fetch reads passive HTML responses without browser rendering."
  (let ((captured nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _args)
                 (setq captured (list :url url))
                 (let ((buffer
                        (e-web-tools-test--http-buffer
                         200
                         '(("Content-Type" . "text/html; charset=utf-8"))
                         "<html><head><title>Example Title</title><script>ignored()</script></head><body><h1>Hello</h1><p>Readable <b>text</b>.</p><a href=\"/next\">Next page</a></body></html>")))
                   (with-current-buffer buffer
                     (funcall callback nil))
                   buffer))))
      (let* ((result (e-tools-execute
                      (e-web-tools-test--registry)
                      '(:id "call-1"
                        :name "web_fetch"
                        :arguments (:url "https://example.com/start"
                                    :format "both"
                                    :include_links t
                                    :include_html t
                                    :max_chars 500
                                    :timeout 12))))
             (content (plist-get result :content)))
        (should (equal (plist-get result :status) 'ok))
        (should (equal captured
                       '(:url "https://example.com/start")))
        (should (equal (plist-get content :capability) "web.fetch"))
        (should (equal (plist-get content :backend) "http"))
        (should (equal (plist-get content :url) "https://example.com/start"))
        (should (equal (plist-get content :final_url)
                       "https://example.com/start"))
        (should (equal (plist-get content :status) 200))
        (should (equal (plist-get content :content_type)
                       "text/html; charset=utf-8"))
        (should (string-match-p "Example Title" (plist-get content :text)))
        (should (string-match-p "Hello" (plist-get content :text)))
        (should-not (string-match-p "ignored" (plist-get content :text)))
        (should (string-match-p "# Example Title"
                                (plist-get content :markdown)))
        (should (equal (plist-get content :links)
                       '((:text "Next page"
                          :url "https://example.com/next"))))
        (should (string-match-p "<html>" (plist-get content :html)))
        (should-not (plist-get (plist-get content :diagnostics)
                               :truncated))))))

(ert-deftest e-web-tools-test-fetch-start-returns-before-callback ()
  "web_fetch starts asynchronously and does not use guarded sync primitives."
  (let ((callback nil)
        (response-buffer (e-web-tools-test--http-buffer
                          200
                          '(("Content-Type" . "text/plain"))
                          "delayed response"))
        result
        failure
        request)
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (_url cb &rest _args)
                     (setq callback cb)
                     response-buffer)))
          (e-request-with-blocking-primitive-guard
            (e-request-with-hot-path 'web-fetch
              (setq request
                    (e-tools-start
                     (e-web-tools-test--registry)
                     '(:id "call-1"
                       :name "web_fetch"
                       :arguments (:url "https://example.com/delayed"))
                     :context '(:interactive t)
                     :on-done (lambda (value)
                                (setq result value))
                     :on-error (lambda (err)
                                 (setq failure err))))))
          (should (e-tools-request-p request))
          (should callback)
          (should-not result)
          (should-not failure)
          (with-current-buffer response-buffer
            (funcall callback nil))
          (should (equal (plist-get result :status) 'ok))
          (should (equal (plist-get (plist-get result :content) :text)
                         "delayed response")))
      (when (buffer-live-p response-buffer)
        (kill-buffer response-buffer)))))

(ert-deftest e-web-tools-test-sync-fetch-helper-rejects-hot-path ()
  "The synchronous fetch helper fails before url.el in hot paths."
  (let (started)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _args)
                 (setq started t)
                 (error "url-retrieve-synchronously should not run"))))
      (let ((err (should-error
                  (e-request-with-hot-path 'web-sync-fetch
                    (e-web-tools--fetch
                     '(:url "https://example.com/delayed")))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err)
                       '(e-web-tools--fetch web-sync-fetch))))
      (should-not started))))

(ert-deftest e-web-tools-test-fetch-truncates-text-and-rejects-unsupported-inputs ()
  "web_fetch truncates large text and rejects unsupported schemes/content."
  (cl-letf (((symbol-function 'url-retrieve)
             (lambda (_url callback &rest _args)
               (let ((buffer
                      (e-web-tools-test--http-buffer
                       200
                       '(("Content-Type" . "text/plain"))
                       "abcdefghij")))
                 (with-current-buffer buffer
                   (funcall callback nil))
                 buffer))))
    (let* ((result (e-tools-execute
                    (e-web-tools-test--registry)
                    '(:id "call-1"
                      :name "web_fetch"
                      :arguments (:url "https://example.com/text"
                                  :max_chars 4))))
           (content (plist-get result :content)))
      (should (equal (plist-get result :status) 'ok))
      (should (equal (plist-get content :text) "abcd"))
      (should (plist-get (plist-get content :diagnostics) :truncated))))
  (let ((bad-scheme
         (e-tools-execute
          (e-web-tools-test--registry)
          '(:id "call-1"
            :name "web_fetch"
            :arguments (:url "file:///tmp/nope")))))
    (should (equal (plist-get bad-scheme :status) 'error))
    (should (equal (plist-get (plist-get bad-scheme :metadata) :error)
                   'e-web-unsupported-url)))
  (cl-letf (((symbol-function 'url-retrieve)
             (lambda (_url callback &rest _args)
               (let ((buffer
                      (e-web-tools-test--http-buffer
                       200
                       '(("Content-Type" . "image/png"))
                       "not really png")))
                 (with-current-buffer buffer
                   (funcall callback nil))
                 buffer))))
    (let ((binary
           (e-tools-execute
            (e-web-tools-test--registry)
            '(:id "call-1"
              :name "web_fetch"
              :arguments (:url "https://example.com/image.png")))))
      (should (equal (plist-get binary :status) 'error))
      (should (equal (plist-get (plist-get binary :metadata) :error)
                     'e-web-unsupported-content)))))

(ert-deftest e-web-tools-test-browser-tools-use-fake-ndjson-helper ()
  "Browser tools send NDJSON requests to the configured helper process."
  (let* ((directory (make-temp-file "e-web-browser-" t))
         (lines-file (expand-file-name "lines.ndjson" directory))
         (helper (e-web-tools-test--fake-executable
                  directory
                  "browser-helper"
                  "while IFS= read -r line; do
  printf '%s\\n' \"$line\" >> \"$E_WEB_BROWSER_LINES\"
  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')
  op=$(printf '%s' \"$line\" | sed -n 's/.*\"op\":\"\\([^\"]*\\)\".*/\\1/p')
  case \"$op\" in
    screenshot)
      result='{\"session\":\"session-1\",\"path\":\"/tmp/e-web-shot.png\"}'
      ;;
    close)
      result='{\"session\":\"session-1\",\"closed\":true}'
      ;;
    *)
      result='{\"session\":\"session-1\",\"url\":\"https://example.com\",\"title\":\"Example\",\"text\":\"Rendered text\"}'
      ;;
  esac
  printf '{\"id\":%s,\"ok\":true,\"result\":%s}\\n' \"$id\" \"$result\"
done
")))
    (unwind-protect
        (let ((e-web-browser-helper-program helper)
              (e-web-browser-helper-args nil)
              (process-environment
               (cons (concat "E_WEB_BROWSER_LINES=" lines-file)
                     process-environment)))
          (e-web-tools-browser-reset)
          (let* ((registry (e-web-tools-test--registry))
                 (calls '((:id "open"
                           :name "web_browser"
                           :arguments (:operation "open"
                                       :url "https://example.com"))
                          (:id "observe"
                           :name "web_browser"
                           :arguments (:operation "observe"
                                       :session "session-1"))
                          (:id "click"
                           :name "web_browser"
                           :arguments (:operation "click"
                                       :session "session-1"
                                       :selector "button"))
                          (:id "type"
                           :name "web_browser"
                           :arguments (:operation "type"
                                       :session "session-1"
                                       :selector "input"
                                       :text "hello"))
                          (:id "press"
                           :name "web_browser"
                           :arguments (:operation "press"
                                       :session "session-1"
                                       :key "Enter"))
                          (:id "screenshot"
                           :name "web_browser"
                           :arguments (:operation "screenshot"
                                       :session "session-1"))
                          (:id "close"
                           :name "web_browser"
                           :arguments (:operation "close"
                                       :session "session-1"))))
                 results)
            (dolist (call calls)
              (push (e-tools-execute registry call) results))
            (setq results (nreverse results))
            (dolist (result results)
              (should (equal (plist-get result :status) 'ok))
              (should (equal (plist-get (plist-get result :content) :backend)
                             "playwright")))
            (let ((open-content (plist-get (car results) :content))
                  (shot-content (plist-get (nth 5 results) :content))
                  (close-content (plist-get (nth 6 results) :content)))
              (should (equal (plist-get open-content :capability)
                             "web.browser.open"))
              (should (equal (plist-get open-content :requested)
                             '(:url "https://example.com")))
              (should (equal (plist-get
                              (plist-get open-content :result)
                              :title)
                             "Example"))
              (should (equal (plist-get
                              (plist-get shot-content :result)
                              :path)
                             "/tmp/e-web-shot.png"))
              (should (equal (plist-get
                              (plist-get close-content :result)
                              :closed)
                             t)))
            (let ((lines (with-temp-buffer
                           (insert-file-contents lines-file)
                           (buffer-string))))
              (dolist (op '("open" "observe" "click" "type" "press"
                            "screenshot" "close"))
                (should (string-match-p (format "\"op\":\"%s\"" op)
                                        lines))))))
      (e-web-tools-browser-reset)
      (delete-directory directory t))))

(ert-deftest e-web-tools-test-browser-start-returns-before-helper-replies ()
  "web_browser starts a cancellable helper request without waiting for a reply."
  (let* ((directory (make-temp-file "e-web-browser-delay-" t))
         (helper (e-web-tools-test--fake-executable
                  directory
                  "browser-helper-delay"
                  "while IFS= read -r line; do
  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')
  sleep 5
  printf '{\"id\":%s,\"ok\":true,\"result\":{\"session\":\"session-1\"}}\\n' \"$id\"
done
"))
         result
         failure
         request)
    (unwind-protect
        (let ((e-web-browser-helper-program helper)
              (e-web-browser-helper-args nil)
              (e-web-browser-helper-timeout 10))
          (e-web-tools-browser-reset)
          (e-request-with-blocking-primitive-guard
            (e-request-with-hot-path 'web-browser
              (setq request
                    (e-tools-start
                     (e-web-tools-test--registry)
                     '(:id "call-1"
                       :name "web_browser"
                       :arguments (:operation "open"
                                   :url "https://example.com"))
                     :context '(:interactive t)
                     :on-done (lambda (value)
                                (setq result value))
                     :on-error (lambda (err)
                                 (setq failure err))))))
          (should (e-tools-request-p request))
          (should (process-live-p
                   (plist-get (e-tools-request-metadata request) :process)))
          (should-not result)
          (should-not failure)
          (should (e-tools-cancel-request request)))
      (e-web-tools-browser-reset)
      (delete-directory directory t))))

(ert-deftest e-web-tools-test-sync-browser-helper-rejects-hot-path ()
  "The synchronous browser helper fails before starting or using the helper."
  (let (started)
    (cl-letf (((symbol-function 'e-web-tools--browser-helper-ensure)
               (lambda ()
                 (setq started t)
                 (error "browser helper should not start"))))
      (let ((err (should-error
                  (e-request-with-hot-path 'web-sync-browser
                    (e-web-tools--browser-request "open" nil))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err)
                       '(e-web-tools--browser-request web-sync-browser))))
      (should-not started))))

(ert-deftest e-web-tools-test-browser-reports-unavailable-helper ()
  "Browser tools return a clear error when the helper cannot be started."
  (let ((e-web-browser-helper-program "/no/such/e-web-browser-helper")
        (e-web-browser-helper-args nil))
    (e-web-tools-browser-reset)
    (let ((result
           (e-tools-execute
            (e-web-tools-test--registry)
            '(:id "call-1"
              :name "web_browser"
              :arguments (:operation "open"
                          :url "https://example.com")))))
      (should (equal (plist-get result :status) 'error))
      (should (equal (plist-get (plist-get result :metadata) :error)
                     'e-web-backend-unavailable))
      (should (string-match-p "browser helper not found"
                              (plist-get result :content))))))

(ert-deftest e-web-tools-test-search-reports-backend-failures ()
  "web_search returns clear errors for missing, failing, and invalid bx backends."
  (let* ((directory (make-temp-file "e-web-bx-errors-" t))
         (failing-bx (e-web-tools-test--fake-executable
                      directory
                      "bx-fail"
                      "echo backend failed >&2
exit 7
"))
         (invalid-bx (e-web-tools-test--fake-executable
                      directory
                      "bx-invalid"
                      "echo not-json
")))
    (unwind-protect
        (cl-labels
            ((search-with
              (program)
              (let ((e-web-bx-program program))
                (e-tools-execute
                 (e-web-tools-test--registry)
                 '(:id "call-1"
                   :name "web_search"
                   :arguments (:query "emacs"))))))
          (let ((missing (search-with (expand-file-name "missing-bx" directory))))
            (should (equal (plist-get missing :status) 'error))
            (should (equal (plist-get (plist-get missing :metadata) :error)
                           'e-web-backend-unavailable))
            (should (string-match-p "bx backend not found"
                                    (plist-get missing :content))))
          (let ((failed (search-with failing-bx)))
            (should (equal (plist-get failed :status) 'error))
            (should (equal (plist-get (plist-get failed :metadata) :error)
                           'e-web-backend-error))
            (should (string-match-p "exit code 7"
                                    (plist-get failed :content))))
          (let ((invalid (search-with invalid-bx)))
            (should (equal (plist-get invalid :status) 'error))
            (should (equal (plist-get (plist-get invalid :metadata) :error)
                           'e-web-invalid-response))
            (should (string-match-p "Invalid bx JSON"
                                    (plist-get invalid :content)))))
      (delete-directory directory t))))

(provide 'e-web-tools-test)

;;; e-web-tools-test.el ends here
