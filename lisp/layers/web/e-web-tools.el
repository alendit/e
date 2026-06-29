;;; e-web-tools.el --- Web tool handlers for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tool registration and handlers for the basic web layer.

;;; Code:

(require 'cl-lib)
(require 'e-tools)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-expand)
(require 'url-parse)

(define-error 'e-web-unimplemented "Web tool is not implemented")
(define-error 'e-web-backend-unavailable "Web backend is unavailable")
(define-error 'e-web-backend-error "Web backend failed")
(define-error 'e-web-backend-timeout "Web backend timed out")
(define-error 'e-web-invalid-response "Web backend returned an invalid response")
(define-error 'e-web-unsupported-url "Web URL is unsupported")
(define-error 'e-web-unsupported-content "Web response content is unsupported")

(defcustom e-web-search-backend 'bx
  "Active web search backend used by `web_search'.
`bx' shells out to the Brave-backed bx CLI; `ddgr' shells out to the
DuckDuckGo ddgr CLI.  Override this to switch the single active backend;
there is no automatic fallback."
  :type '(choice (const :tag "bx (Brave CLI)" bx)
                 (const :tag "ddgr (DuckDuckGo CLI)" ddgr))
  :group 'e)

(defcustom e-web-bx-program "bx"
  "Program used for the bx web search backend."
  :type 'string
  :group 'e)

(defcustom e-web-bx-timeout 30
  "Default timeout in seconds for bx web search calls."
  :type 'number
  :group 'e)

(defcustom e-web-ddgr-program "ddgr"
  "Program used for the ddgr web search backend."
  :type 'string
  :group 'e)

(defcustom e-web-ddgr-timeout 30
  "Default timeout in seconds for ddgr web search calls."
  :type 'number
  :group 'e)

(defcustom e-web-browser-helper-program "node"
  "Program used to run the Playwright browser helper."
  :type 'string
  :group 'e)

(defcustom e-web-browser-helper-args nil
  "Arguments passed to `e-web-browser-helper-program'.
When nil and the program is node, e uses the bundled helper script."
  :type '(repeat string)
  :group 'e)

(defcustom e-web-browser-helper-timeout 20
  "Default timeout in seconds for browser helper requests."
  :type 'number
  :group 'e)

(defconst e-web-tools--directory
  (file-name-directory
   (file-truename
    (expand-file-name (or load-file-name buffer-file-name default-directory))))
  "Directory containing e web tool support files.")

(defvar e-web-tools--browser-process nil)
(defvar e-web-tools--browser-stdout nil)
(defvar e-web-tools--browser-stderr nil)
(defvar e-web-tools--browser-next-id 0)

(defun e-web-tools--argument-string (arguments key)
  "Return string argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp value)))
    value))

(defun e-web-tools--optional-string (arguments key)
  "Return optional string argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (when value
      (unless (stringp value)
        (signal 'wrong-type-argument (list 'stringp value)))
      value)))

(defun e-web-tools--optional-number (arguments key)
  "Return optional positive number argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (when value
      (unless (and (numberp value) (> value 0))
        (signal 'wrong-type-argument (list 'positive-number-p value)))
      value)))

(defun e-web-tools--truthy-p (value)
  "Return non-nil when VALUE is true for model-facing JSON arguments."
  (and value (not (eq value :json-false))))

(defun e-web-tools--string-list (arguments key)
  "Return optional string list argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (cond
     ((null value) nil)
     ((vectorp value) (mapcar #'identity value))
     ((listp value) value)
     (t (signal 'wrong-type-argument (list 'sequencep value))))))

(defun e-web-tools--executable-path (program &optional label)
  "Return executable path for PROGRAM or signal a backend error.
LABEL describes the executable in error messages."
  (or (and (stringp program)
           (file-name-absolute-p program)
           (file-executable-p program)
           program)
      (and (stringp program)
           (not (string-empty-p program))
           (executable-find program))
      (signal 'e-web-backend-unavailable
              (list (format "%s not found: %s"
                            (or label "backend")
                            program)))))

(defun e-web-tools--buffer-string (buffer)
  "Return BUFFER contents when BUFFER is live."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-string))
    ""))

(defun e-web-tools--run-process (program args timeout &optional label)
  "Run PROGRAM with ARGS and TIMEOUT, returning stdout text.
LABEL names the backend in error messages and defaults to \"backend\"."
  (let ((stdout (generate-new-buffer " *e-web-stdout*"))
        (stderr (generate-new-buffer " *e-web-stderr*"))
        (label (or label "backend"))
        (done nil)
        (exit-code nil)
        process)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name "e-web-backend"
                 :buffer stdout
                 :stderr stderr
                 :command (cons program args)
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :noquery t
                 :sentinel
                 (lambda (proc _event)
                   (when (memq (process-status proc) '(exit signal))
                     (setq exit-code (process-exit-status proc))
                     (setq done t)))))
          (set-process-query-on-exit-flag process nil)
          (let ((deadline (and timeout (+ (float-time) timeout))))
            (while (and (not done)
                        (or (not deadline)
                            (< (float-time) deadline)))
              (accept-process-output process 0.01)))
          (unless done
            (when (process-live-p process)
              (kill-process process))
            (signal 'e-web-backend-timeout
                    (list (format "%s timed out after %s seconds"
                                  label
                                  timeout))))
          (let ((out (e-web-tools--buffer-string stdout))
                (err (string-trim (e-web-tools--buffer-string stderr))))
            (unless (zerop exit-code)
              (signal 'e-web-backend-error
                      (list (string-trim
                             (format "%s exited with exit code %s%s"
                                     label
                                     exit-code
                                     (if (string-empty-p err)
                                         ""
                                       (concat ": " err)))))))
            out))
      (when (buffer-live-p stdout)
        (kill-buffer stdout))
      (when (buffer-live-p stderr)
        (kill-buffer stderr)))))

(defun e-web-tools--parse-json (text)
  "Parse JSON TEXT into plists."
  (condition-case err
      (json-parse-string text
                         :object-type 'plist
                         :array-type 'list
                         :null-object nil
                         :false-object :json-false)
    (error
     (signal 'e-web-invalid-response
             (list (format "Invalid bx JSON: %s"
                           (error-message-string err)))))))

(defun e-web-tools--put-when (plist key value)
  "Append KEY VALUE to PLIST when VALUE is non-nil."
  (if value
      (append plist (list key value))
    plist))

(defun e-web-tools--search-result-source (result)
  "Return normalized source for bx RESULT."
  (or (plist-get result :source)
      (when-let ((profile (plist-get result :profile)))
        (plist-get profile :name))))

(defun e-web-tools--normalize-search-result (result rank)
  "Normalize bx RESULT at one-based RANK."
  (let* ((title (plist-get result :title))
         (url (plist-get result :url))
         (snippet (or (plist-get result :snippet)
                      (plist-get result :description)))
         (source (e-web-tools--search-result-source result))
         (date (or (plist-get result :date)
                   (plist-get result :age)))
         (normalized (list :rank rank
                           :title title
                           :url url)))
    (setq normalized (e-web-tools--put-when normalized :snippet snippet))
    (setq normalized (e-web-tools--put-when normalized :source source))
    (setq normalized (e-web-tools--put-when normalized :date date))
    normalized))

(defun e-web-tools--normalize-search-results (payload)
  "Return normalized web search results from bx PAYLOAD."
  (let ((rank 0)
        results)
    (dolist (result (plist-get (plist-get payload :web) :results))
      (setq rank (1+ rank))
      (push (e-web-tools--normalize-search-result result rank) results))
    (nreverse results)))

(defun e-web-tools--normalize-ddgr-result (result rank)
  "Normalize ddgr RESULT at one-based RANK."
  (let ((normalized (list :rank rank
                          :title (plist-get result :title)
                          :url (plist-get result :url))))
    (e-web-tools--put-when normalized :snippet (plist-get result :abstract))))

(defun e-web-tools--normalize-ddgr-results (payload)
  "Return normalized web search results from ddgr PAYLOAD.
PAYLOAD is the flat result list ddgr emits with --json."
  (let ((rank 0)
        results)
    (dolist (result payload)
      (setq rank (1+ rank))
      (push (e-web-tools--normalize-ddgr-result result rank) results))
    (nreverse results)))

(defun e-web-tools--ddgr-freshness (freshness)
  "Map a bx-style FRESHNESS token to ddgr's -t span (d/w/m/y), else nil."
  (pcase freshness
    ((or "pd" "d" "day") "d")
    ((or "pw" "w" "week") "w")
    ((or "pm" "m" "month") "m")
    ((or "py" "y" "year") "y")
    (_ nil)))

(defun e-web-tools--ddgr-argv (arguments)
  "Return ddgr argv for model-facing ARGUMENTS.
Site include/exclude is expressed as DuckDuckGo `site:'/`-site:' query
operators because ddgr's --site accepts only a single domain."
  (let* ((query (e-web-tools--argument-string arguments :query))
         (terms (list query)))
    (dolist (site (e-web-tools--string-list arguments :include_site))
      (unless (stringp site)
        (signal 'wrong-type-argument (list 'stringp site)))
      (setq terms (append terms (list (concat "site:" site)))))
    (dolist (site (e-web-tools--string-list arguments :exclude_site))
      (unless (stringp site)
        (signal 'wrong-type-argument (list 'stringp site)))
      (setq terms (append terms (list (concat "-site:" site)))))
    (let ((argv (list "--json" "--noprompt")))
      (when-let ((count (e-web-tools--optional-number arguments :count)))
        (setq argv (append argv (list "--num" (number-to-string count)))))
      (when-let* ((freshness (e-web-tools--optional-string arguments :freshness))
                  (span (e-web-tools--ddgr-freshness freshness)))
        (setq argv (append argv (list "--time" span))))
      (append argv terms))))

(defun e-web-tools--search-argv (arguments)
  "Return bx web argv for model-facing ARGUMENTS."
  (let ((argv (list "web" (e-web-tools--argument-string arguments :query))))
    (when-let ((count (e-web-tools--optional-number arguments :count)))
      (setq argv (append argv (list "--count" (number-to-string count)))))
    (when-let ((freshness (e-web-tools--optional-string arguments :freshness)))
      (setq argv (append argv (list "--freshness" freshness))))
    (dolist (site (e-web-tools--string-list arguments :include_site))
      (unless (stringp site)
        (signal 'wrong-type-argument (list 'stringp site)))
      (setq argv (append argv (list "--include-site" site))))
    (dolist (site (e-web-tools--string-list arguments :exclude_site))
      (unless (stringp site)
        (signal 'wrong-type-argument (list 'stringp site)))
      (setq argv (append argv (list "--exclude-site" site))))
    argv))

(defun e-web-tools--search-bx (arguments)
  "Run a bx web search for ARGUMENTS, returning (PAYLOAD . RESULTS)."
  (let* ((program (e-web-tools--executable-path e-web-bx-program "bx backend"))
         (payload (e-web-tools--parse-json
                   (e-web-tools--run-process
                    program
                    (e-web-tools--search-argv arguments)
                    e-web-bx-timeout
                    "bx backend"))))
    (cons payload (e-web-tools--normalize-search-results payload))))

(defun e-web-tools--search-ddgr (arguments)
  "Run a ddgr web search for ARGUMENTS, returning (PAYLOAD . RESULTS)."
  (let* ((program (e-web-tools--executable-path e-web-ddgr-program "ddgr backend"))
         (payload (e-web-tools--parse-json
                   (e-web-tools--run-process
                    program
                    (e-web-tools--ddgr-argv arguments)
                    e-web-ddgr-timeout
                    "ddgr backend"))))
    (cons payload (e-web-tools--normalize-ddgr-results payload))))

(defun e-web-tools--search (arguments)
  "Run web search for ARGUMENTS using `e-web-search-backend'."
  (let* ((query (e-web-tools--argument-string arguments :query))
         (backend e-web-search-backend)
         (run (pcase backend
                ('bx #'e-web-tools--search-bx)
                ('ddgr #'e-web-tools--search-ddgr)
                (_ (signal 'e-web-backend-unavailable
                           (list (format "Unknown web search backend: %s"
                                         backend))))))
         (payload-results (funcall run arguments))
         (content (list :capability "web.search"
                        :backend (symbol-name backend)
                        :query query
                        :results (cdr payload-results)
                        :diagnostics nil)))
    (when (e-web-tools--truthy-p (plist-get arguments :include_raw))
      (setq content (append content (list :raw (car payload-results)))))
    content))

(defun e-web-tools--url-scheme (url)
  "Return lowercase scheme for URL."
  (let ((scheme (url-type (url-generic-parse-url url))))
    (and scheme (downcase scheme))))

(defun e-web-tools--validate-http-url (url)
  "Signal unless URL is an HTTP or HTTPS URL."
  (unless (member (e-web-tools--url-scheme url) '("http" "https"))
    (signal 'e-web-unsupported-url
            (list (format "web_fetch supports only http and https URLs: %s"
                          url)))))

(defun e-web-tools--response-header-end ()
  "Return current buffer response body start."
  (or (and (boundp 'url-http-end-of-headers)
           (integerp url-http-end-of-headers)
           url-http-end-of-headers)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\r?\n\r?\n" nil t)
          (point)))
      (point-min)))

(defun e-web-tools--parse-headers (header-text)
  "Parse HTTP HEADER-TEXT into an alist."
  (let (headers)
    (dolist (line (cdr (split-string header-text "\r?\n" t)))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
        (push (cons (downcase (match-string 1 line))
                    (match-string 2 line))
              headers)))
    (nreverse headers)))

(defun e-web-tools--response-status (header-text)
  "Return numeric HTTP status from HEADER-TEXT."
  (if (string-match "\\`HTTP/[0-9.]+[ \t]+\\([0-9]+\\)" header-text)
      (string-to-number (match-string 1 header-text))
    0))

(defun e-web-tools--parse-http-buffer (buffer)
  "Parse url response BUFFER into a plist and kill BUFFER."
  (unwind-protect
      (with-current-buffer buffer
        (let* ((body-start (e-web-tools--response-header-end))
               (header-text (buffer-substring-no-properties
                             (point-min)
                             body-start))
               (headers (e-web-tools--parse-headers header-text))
               (body (buffer-substring-no-properties body-start (point-max))))
          (list :status (e-web-tools--response-status header-text)
                :headers headers
                :content-type (or (cdr (assoc "content-type" headers)) "")
                :body body)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun e-web-tools--textual-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE is safe to treat as text."
  (let ((type (downcase (or content-type ""))))
    (or (string-empty-p type)
        (string-prefix-p "text/" type)
        (string-match-p
         "\\`application/\\(json\\|xml\\|xhtml\\+xml\\|javascript\\)"
         type))))

(defun e-web-tools--html-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE is HTML."
  (string-match-p "html" (downcase (or content-type ""))))

(defun e-web-tools--decode-html-entities (text)
  "Decode a small useful subset of HTML entities in TEXT."
  (let ((decoded text))
    (dolist (pair '(("&nbsp;" . " ")
                    ("&amp;" . "&")
                    ("&lt;" . "<")
                    ("&gt;" . ">")
                    ("&quot;" . "\"")
                    ("&#39;" . "'")))
      (setq decoded
            (replace-regexp-in-string
             (regexp-quote (car pair))
             (cdr pair)
             decoded
             t
             t)))
    decoded))

(defun e-web-tools--remove-html-blocks (html tag)
  "Remove TAG blocks from HTML without broad backtracking regexps."
  (let ((case-fold-search t)
        (text html)
        (start 0)
        open-start content-start)
    (while (string-match (format "<%s\\b[^>]*>" tag) text start)
      (setq open-start (match-beginning 0))
      (setq content-start (match-end 0))
      (if (string-match (format "</%s>" tag) text content-start)
          (progn
            (setq text (concat (substring text 0 open-start)
                               " "
                               (substring text (match-end 0))))
            (setq start open-start))
        (setq start content-start)))
    text))

(defun e-web-tools--strip-html (html)
  "Return readable plain text from simple HTML."
  (let ((text html))
    (setq text (e-web-tools--remove-html-blocks text "script"))
    (setq text (e-web-tools--remove-html-blocks text "style"))
    (setq text (replace-regexp-in-string "<[^>]+>" " " text))
    (setq text (e-web-tools--decode-html-entities text))
    (setq text (replace-regexp-in-string "[ \t\r\n]+" " " text))
    (string-trim text)))

(defun e-web-tools--html-title (html)
  "Return HTML title from HTML when present."
  (let ((case-fold-search t))
    (when (string-match "<title[^>]*>\\([^<]*\\)</title>" html)
      (string-trim
       (e-web-tools--decode-html-entities (match-string 1 html))))))

(defun e-web-tools--html-links (html base-url)
  "Return normalized links from HTML using BASE-URL."
  (let ((case-fold-search t)
        (start 0)
        links
        open-start tag-end)
    (while (setq open-start (string-match "<a" html start))
      (setq tag-end (string-match ">" html open-start))
      (if (not tag-end)
          (setq start (length html))
        (let ((tag (substring html open-start (1+ tag-end))))
          (if (not (string-match "href=[\"']\\([^\"']+\\)[\"']" tag))
              (setq start (1+ tag-end))
            (let ((href (match-string 1 tag))
                  (label-start (1+ tag-end)))
              (if (string-match "</a>" html label-start)
                  (let* ((close-start (match-beginning 0))
                         (close-end (match-end 0))
                         (label (e-web-tools--strip-html
                                 (substring html
                                            label-start
                                            close-start))))
                    (push (list :text label
                                :url (url-expand-file-name href base-url))
                          links)
                    (setq start close-end))
                (setq start label-start)))))))
    (nreverse links)))

(defun e-web-tools--markdown (title text links)
  "Return simple Markdown from TITLE, TEXT, and LINKS."
  (let ((parts nil))
    (when title
      (push (format "# %s" title) parts))
    (when (and text (not (string-empty-p text)))
      (push text parts))
    (when links
      (push (concat
             "Links:\n"
             (mapconcat
              (lambda (link)
                (format "- [%s](%s)"
                        (plist-get link :text)
                        (plist-get link :url)))
              links
              "\n"))
            parts))
    (mapconcat #'identity (nreverse parts) "\n\n")))

(defun e-web-tools--truncate-value (value max-chars)
  "Return (VALUE TRUNCATED) after applying MAX-CHARS to string VALUE."
  (if (and (stringp value)
           max-chars
           (> (length value) max-chars))
      (list (substring value 0 max-chars) t)
    (list value nil)))

(defun e-web-tools--fetch-format (arguments)
  "Return normalized fetch output format from ARGUMENTS."
  (let ((format (or (e-web-tools--optional-string arguments :format)
                    "text")))
    (unless (member format '("text" "markdown" "both" "html"))
      (signal 'wrong-type-argument (list 'e-web-fetch-format format)))
    format))

(defun e-web-tools--include-links-p (arguments)
  "Return non-nil when ARGUMENTS say fetch should include links."
  (not (eq (plist-get arguments :include_links) :json-false)))

(defun e-web-tools--fetch (arguments)
  "Fetch a passive HTTP resource for ARGUMENTS."
  (let* ((url (e-web-tools--argument-string arguments :url))
         (format (e-web-tools--fetch-format arguments))
         (timeout (or (e-web-tools--optional-number arguments :timeout) 20))
         (max-chars (or (e-web-tools--optional-number arguments :max_chars)
                        50000)))
    (e-web-tools--validate-http-url url)
    (let* ((response-buffer
            (url-retrieve-synchronously url t nil timeout))
           (response
            (if response-buffer
                (e-web-tools--parse-http-buffer response-buffer)
              (signal 'e-web-backend-error
                      (list (format "No response for URL: %s" url)))))
           (content-type (plist-get response :content-type))
           (htmlp (e-web-tools--html-content-type-p content-type))
           (body (plist-get response :body))
           (title (and htmlp (e-web-tools--html-title body)))
           (text (if htmlp
                     (e-web-tools--strip-html body)
                   body))
           (links (and htmlp
                       (e-web-tools--include-links-p arguments)
                       (e-web-tools--html-links body url)))
           (markdown (and (member format '("markdown" "both"))
                          (e-web-tools--markdown title text links)))
           (include-html (or (equal format "html")
                             (e-web-tools--truthy-p
                              (plist-get arguments :include_html))))
           (diagnostics nil)
           truncated)
      (unless (e-web-tools--textual-content-type-p content-type)
        (signal 'e-web-unsupported-content
                (list (format "Unsupported content type: %s" content-type))))
      (pcase-let ((`(,new-text ,text-truncated)
                   (e-web-tools--truncate-value text max-chars))
                  (`(,new-markdown ,markdown-truncated)
                   (e-web-tools--truncate-value markdown max-chars))
                  (`(,new-html ,html-truncated)
                   (e-web-tools--truncate-value body max-chars)))
        (setq text new-text)
        (setq markdown new-markdown)
        (setq body new-html)
        (setq truncated (or text-truncated
                            markdown-truncated
                            html-truncated)))
      (when truncated
        (setq diagnostics (plist-put diagnostics :truncated t)))
      (let ((content (list :capability "web.fetch"
                           :backend "http"
                           :url url
                           :final_url url
                           :status (plist-get response :status)
                           :content_type content-type
                           :headers (plist-get response :headers)
                           :diagnostics diagnostics)))
        (when (member format '("text" "both"))
          (setq content (append content (list :text text))))
        (when (member format '("markdown" "both"))
          (setq content (append content (list :markdown markdown))))
        (when (equal format "html")
          (setq content (append content (list :html body))))
        (when include-html
          (setq content (append content (list :html body))))
        (when (and links (e-web-tools--include-links-p arguments))
          (setq content (append content (list :links links))))
        content))))

(defun e-web-tools--fetch-content (arguments url response)
  "Return web fetch content for ARGUMENTS, URL, and parsed RESPONSE."
  (let* ((format (e-web-tools--fetch-format arguments))
         (max-chars (or (e-web-tools--optional-number arguments :max_chars)
                        50000))
         (content-type (plist-get response :content-type))
         (htmlp (e-web-tools--html-content-type-p content-type))
         (body (plist-get response :body))
         (title (and htmlp (e-web-tools--html-title body)))
         (text (if htmlp
                   (e-web-tools--strip-html body)
                 body))
         (links (and htmlp
                     (e-web-tools--include-links-p arguments)
                     (e-web-tools--html-links body url)))
         (markdown (and (member format '("markdown" "both"))
                        (e-web-tools--markdown title text links)))
         (include-html (or (equal format "html")
                           (e-web-tools--truthy-p
                            (plist-get arguments :include_html))))
         (diagnostics nil)
         truncated)
    (unless (e-web-tools--textual-content-type-p content-type)
      (signal 'e-web-unsupported-content
              (list (format "Unsupported content type: %s" content-type))))
    (pcase-let ((`(,new-text ,text-truncated)
                 (e-web-tools--truncate-value text max-chars))
                (`(,new-markdown ,markdown-truncated)
                 (e-web-tools--truncate-value markdown max-chars))
                (`(,new-html ,html-truncated)
                 (e-web-tools--truncate-value body max-chars)))
      (setq text new-text)
      (setq markdown new-markdown)
      (setq body new-html)
      (setq truncated (or text-truncated
                          markdown-truncated
                          html-truncated)))
    (when truncated
      (setq diagnostics (plist-put diagnostics :truncated t)))
    (let ((content (list :capability "web.fetch"
                         :backend "http"
                         :url url
                         :final_url url
                         :status (plist-get response :status)
                         :content_type content-type
                         :headers (plist-get response :headers)
                         :diagnostics diagnostics)))
      (when (member format '("text" "both"))
        (setq content (append content (list :text text))))
      (when (member format '("markdown" "both"))
        (setq content (append content (list :markdown markdown))))
      (when (equal format "html")
        (setq content (append content (list :html body))))
      (when include-html
        (setq content (append content (list :html body))))
      (when (and links (e-web-tools--include-links-p arguments))
        (setq content (append content (list :links links))))
      content)))

(cl-defun e-web-tools--fetch-start
    (&key arguments on-done on-error on-event &allow-other-keys)
  "Start passive HTTP fetch for ARGUMENTS asynchronously."
  (let* ((url (e-web-tools--argument-string arguments :url))
         (timeout (or (e-web-tools--optional-number arguments :timeout) 20))
         (settled nil)
         timer
         response-buffer)
    (e-web-tools--validate-http-url url)
    (cl-labels
        ((cleanup ()
           (when (timerp timer)
             (cancel-timer timer))
           (when (buffer-live-p response-buffer)
             (kill-buffer response-buffer)))
         (fail (err)
           (unless settled
             (setq settled t)
             (cleanup)
             (when on-error
               (funcall on-error err))))
         (finish (buffer)
           (unless settled
             (setq response-buffer buffer)
             (condition-case err
                 (let* ((response (if (buffer-live-p buffer)
                                      (with-current-buffer buffer
                                        (e-web-tools--parse-http-buffer buffer))
                                    (signal 'e-web-backend-error
                                            (list (format "No response for URL: %s"
                                                          url)))))
                        (content (e-web-tools--fetch-content
                                  arguments url response)))
                   (setq settled t)
                   (cleanup)
                   (when on-done
                     (funcall on-done content)))
               (error
                (fail err)))))
         (timeout! ()
           (fail
            (list 'e-web-backend-timeout
                  (format "HTTP fetch timed out after %s seconds" timeout)))))
      (condition-case err
          (progn
            (when on-event
              (funcall on-event 'tool-progress
                       (list :message (format "Fetching %s" url))))
            (setq timer (run-at-time timeout nil #'timeout!))
            (setq response-buffer
                  (url-retrieve
                   url
                   (lambda (_status)
                     (finish (current-buffer)))
                   nil
                   t
                   nil))
            (e-tools-request-create
             :cancel (lambda ()
                       (unless settled
                         (setq settled t)
                         (cleanup))
                       t)
             :metadata (list :transport 'url
                             :url url)))
        (error
         (fail err)
         nil)))))

(defun e-web-tools--browser-helper-script ()
  "Return bundled browser helper script path."
  (expand-file-name "e-web-browser-helper.mjs" e-web-tools--directory))

(defun e-web-tools--browser-helper-command ()
  "Return command list for the browser helper."
  (let* ((program (e-web-tools--executable-path
                   e-web-browser-helper-program
                   "browser helper"))
         (args (or e-web-browser-helper-args
                   (when (string-equal
                          (file-name-nondirectory
                           e-web-browser-helper-program)
                          "node")
                     (list (e-web-tools--browser-helper-script))))))
    (cons program args)))

(defun e-web-tools-browser-reset ()
  "Stop the current browser helper process and clear protocol state."
  (when (and e-web-tools--browser-process
             (process-live-p e-web-tools--browser-process))
    (kill-process e-web-tools--browser-process))
  (when (buffer-live-p e-web-tools--browser-stdout)
    (kill-buffer e-web-tools--browser-stdout))
  (when (buffer-live-p e-web-tools--browser-stderr)
    (kill-buffer e-web-tools--browser-stderr))
  (setq e-web-tools--browser-process nil)
  (setq e-web-tools--browser-stdout nil)
  (setq e-web-tools--browser-stderr nil)
  (setq e-web-tools--browser-next-id 0)
  t)

(defun e-web-tools--browser-helper-live-p ()
  "Return non-nil when the browser helper process is live."
  (and e-web-tools--browser-process
       (process-live-p e-web-tools--browser-process)))

(defun e-web-tools--browser-helper-ensure ()
  "Start and return the browser helper process."
  (unless (e-web-tools--browser-helper-live-p)
    (setq e-web-tools--browser-stdout
          (generate-new-buffer " *e-web-browser-stdout*"))
    (setq e-web-tools--browser-stderr
          (generate-new-buffer " *e-web-browser-stderr*"))
    (setq e-web-tools--browser-process
          (make-process
           :name "e-web-browser-helper"
           :buffer nil
           :stderr e-web-tools--browser-stderr
           :command (e-web-tools--browser-helper-command)
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :filter
           (lambda (_process text)
             (when (buffer-live-p e-web-tools--browser-stdout)
               (with-current-buffer e-web-tools--browser-stdout
                 (goto-char (point-max))
                 (insert text))))))
    (set-process-query-on-exit-flag e-web-tools--browser-process nil))
  e-web-tools--browser-process)

(defun e-web-tools--browser-stderr-string ()
  "Return captured browser helper stderr."
  (string-trim (e-web-tools--buffer-string e-web-tools--browser-stderr)))

(defun e-web-tools--browser-next-id ()
  "Return next browser protocol request id."
  (setq e-web-tools--browser-next-id
        (1+ e-web-tools--browser-next-id)))

(defun e-web-tools--browser-response-for-id (id)
  "Return parsed browser helper response for ID when available."
  (when (buffer-live-p e-web-tools--browser-stdout)
    (with-current-buffer e-web-tools--browser-stdout
      (save-excursion
        (goto-char (point-min))
        (let (response)
          (while (and (not response) (not (eobp)))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (unless (string-empty-p (string-trim line))
                (let ((payload (e-web-tools--parse-json line)))
                  (when (equal (plist-get payload :id) id)
                    (setq response payload)))))
            (forward-line 1))
          response)))))

(defun e-web-tools--browser-request (operation arguments &optional timeout)
  "Send browser OPERATION with ARGUMENTS and wait up to TIMEOUT seconds."
  (let* ((process (e-web-tools--browser-helper-ensure))
         (id (e-web-tools--browser-next-id))
         (request (append (list :id id :op operation) arguments))
         (deadline (+ (float-time)
                      (or timeout e-web-browser-helper-timeout)))
         response)
    (process-send-string process (concat (json-encode request) "\n"))
    (while (and (not response)
                (process-live-p process)
                (< (float-time) deadline))
      (accept-process-output process 0.01)
      (setq response (e-web-tools--browser-response-for-id id)))
    (unless response
      (if (process-live-p process)
          (signal 'e-web-backend-timeout
                  (list (format "browser helper timed out after %s seconds"
                                (or timeout e-web-browser-helper-timeout))))
        (signal 'e-web-backend-error
                (list (string-trim
                       (format "browser helper exited%s"
                               (if (string-empty-p
                                    (e-web-tools--browser-stderr-string))
                                   ""
                                 (concat ": "
                                         (e-web-tools--browser-stderr-string)))))))))
    (unless (e-web-tools--truthy-p (plist-get response :ok))
      (signal 'e-web-backend-error
              (list (or (plist-get response :error)
                        "browser helper returned an error"))))
    (plist-get response :result)))

(defun e-web-tools--browser-arguments (operation arguments)
  "Return protocol arguments for browser OPERATION from tool ARGUMENTS."
  (pcase operation
    ("open"
     (let ((request (list :url (e-web-tools--argument-string arguments :url))))
       (when-let ((session (e-web-tools--optional-string arguments :session)))
         (setq request (append request (list :session session))))
       request))
    ("observe"
     (when-let ((session (e-web-tools--optional-string arguments :session)))
       (list :session session)))
    ("click"
     (append
      (when-let ((session (e-web-tools--optional-string arguments :session)))
        (list :session session))
      (list :selector (e-web-tools--argument-string arguments :selector))))
    ("type"
     (append
      (when-let ((session (e-web-tools--optional-string arguments :session)))
        (list :session session))
      (list :selector (e-web-tools--argument-string arguments :selector)
            :text (e-web-tools--argument-string arguments :text))))
    ("press"
     (append
      (when-let ((session (e-web-tools--optional-string arguments :session)))
        (list :session session))
      (list :key (e-web-tools--argument-string arguments :key))))
    ("screenshot"
     (append
      (when-let ((session (e-web-tools--optional-string arguments :session)))
        (list :session session))
      (when-let ((path (e-web-tools--optional-string arguments :path)))
        (list :path path))))
    ("close"
     (when-let ((session (e-web-tools--optional-string arguments :session)))
       (list :session session)))
    (_ (signal 'e-web-unimplemented
               (list (format "Unsupported browser operation: %s"
                             operation))))))

(defun e-web-tools--browser (operation arguments)
  "Run browser OPERATION with model-facing ARGUMENTS."
  (let* ((request-arguments
          (or (e-web-tools--browser-arguments operation arguments) nil))
         (timeout (e-web-tools--optional-number arguments :timeout))
         (result (e-web-tools--browser-request
                  operation
                  request-arguments
                  timeout)))
    (list :capability (format "web.browser.%s" operation)
          :backend "playwright"
          :operation operation
          :requested request-arguments
          :result result
          :diagnostics (list :stderr (e-web-tools--browser-stderr-string)))))

(defun e-web-tools--unimplemented (operation _arguments)
  "Signal that OPERATION is not implemented yet."
  (signal 'e-web-unimplemented
          (list (format "%s is not implemented yet" operation))))

(defun e-web-tools-register-search (registry)
  "Register the web search tool in REGISTRY."
  (e-tools-register
   registry
   :name "web_search"
   :description "Search the web using the configured backend (e-web-search-backend: bx or ddgr)."
   :parameters '(:type "object"
                 :properties (:query (:type "string")
                              :count (:type "number")
                              :freshness (:type "string")
                              :include_site (:type "array"
                                             :items (:type "string"))
                              :exclude_site (:type "array"
                                             :items (:type "string"))
                              :include_raw (:type "boolean"))
                 :required ["query"])
   :handler (lambda (arguments)
              (e-web-tools--search arguments))))

(defun e-web-tools-register-fetch (registry)
  "Register the passive web fetch tool in REGISTRY."
  (e-tools-register
   registry
   :name "web_fetch"
   :description "Fetch HTTP or HTTPS content without browser rendering."
   :parameters '(:type "object"
                 :properties (:url (:type "string")
                              :format (:type "string")
                              :include_links (:type "boolean")
                              :include_html (:type "boolean")
                              :max_chars (:type "number")
                              :timeout (:type "number"))
                 :required ["url"])
   :handler (lambda (arguments)
              (e-web-tools--fetch arguments))
   :start #'e-web-tools--fetch-start
   :blocking-class 'network))

(defun e-web-tools-register-browser (registry)
  "Register the rendered browser entrypoint in REGISTRY."
  (e-tools-register
   registry
   :name "web_browser"
   :description "Run a rendered browser operation. Read e://web/refs/browser.md for supported operations and arguments before calling."
   :parameters '(:type "object"
                 :properties (:operation (:type "string")
                              :session (:type "string")
                              :url (:type "string")
                              :selector (:type "string")
                              :text (:type "string")
                              :key (:type "string")
                              :path (:type "string")
                              :timeout (:type "number"))
                 :required ["operation"])
   :handler (lambda (arguments)
              (e-web-tools--browser
               (e-web-tools--argument-string arguments :operation)
               arguments))))

(provide 'e-web-tools)

;;; e-web-tools.el ends here
