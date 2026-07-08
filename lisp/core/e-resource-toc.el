;;; e-resource-toc.el --- wot-backed resource table-of-content helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared helpers for resource methods that expose compact `wot' outlines.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-request)
(require 'e-work)

(define-error 'e-resource-toc-missing-command
  "Resource table-of-content command is missing")
(define-error 'e-resource-toc-invalid-option
  "Resource table-of-content option is invalid")
(define-error 'e-resource-toc-language-required
  "Resource table-of-content language is required")
(define-error 'e-resource-toc-process-failed
  "Resource table-of-content process failed")

(defun e-resource-toc-wot-executable ()
  "Return the wot executable path, or nil."
  (executable-find "wot"))

(defun e-resource-toc-available-p ()
  "Return non-nil when table-of-content resources can be registered."
  (and (e-resource-toc-wot-executable) t))

(defun e-resource-toc--require-wot ()
  "Return the wot executable path or signal a clear error."
  (or (e-resource-toc-wot-executable)
      (signal 'e-resource-toc-missing-command
              '("Missing executable: wot"))))

(defun e-resource-toc--positive-integer (options key)
  "Return positive integer OPTIONS value for KEY, or nil."
  (let ((value (plist-get options key)))
    (cond
     ((null value) nil)
     ((and (integerp value) (> value 0)) value)
     ((and (numberp value) (> value 0)) (truncate value))
     (t (signal 'e-resource-toc-invalid-option
                (list (format "%s must be a positive integer" key)))))))

(defun e-resource-toc--non-negative-integer (options key)
  "Return non-negative integer OPTIONS value for KEY, or nil."
  (let ((value (plist-get options key)))
    (cond
     ((null value) nil)
     ((and (integerp value) (>= value 0)) value)
     ((and (numberp value) (>= value 0)) (truncate value))
     (t (signal 'e-resource-toc-invalid-option
                (list (format "%s must be a non-negative integer" key)))))))

(defun e-resource-toc--format (options)
  "Return normalized wot output format from OPTIONS."
  (let ((value (or (plist-get options :format) "markdown")))
    (unless (member value '("markdown" "json"))
      (signal 'e-resource-toc-invalid-option
              (list "format must be markdown or json")))
    value))

(defun e-resource-toc--language-option (options)
  "Return explicit language option from OPTIONS, or nil."
  (let ((value (plist-get options :language)))
    (cond
     ((null value) nil)
     ((and (stringp value) (not (string-empty-p value))) value)
     (t (signal 'e-resource-toc-invalid-option
                '("language must be a non-empty string"))))))

(defun e-resource-toc-normalize-options (options)
  "Return normalized table-of-content OPTIONS."
  (list :max-depth (e-resource-toc--positive-integer options :max-depth)
        :max-items (e-resource-toc--positive-integer options :max-items)
        :min-lines (e-resource-toc--non-negative-integer options :min-lines)
        :format (e-resource-toc--format options)
        :language (e-resource-toc--language-option options)
        :lenient (and (plist-get options :lenient) t)))

(defun e-resource-toc--extension (name)
  "Return lowercase extension for NAME, or nil."
  (when (stringp name)
    (downcase (or (file-name-extension name t) ""))))

(defun e-resource-toc-infer-language (name &optional fallback)
  "Infer a wot language from NAME, or return FALLBACK."
  (let* ((base (and name (file-name-nondirectory name)))
         (ext (e-resource-toc--extension name)))
    (or
     (cond
      ((null name) nil)
      ((member base '("Dockerfile" "Containerfile")) "dockerfile")
      ((or (equal base ".env")
           (and base (string-prefix-p ".env." base)))
       "dotenv")
      ((member ext '(".rs")) "rust")
      ((member ext '(".ts" ".tsx" ".mts" ".cts")) "typescript")
      ((member ext '(".js" ".jsx" ".mjs" ".cjs")) "javascript")
      ((member ext '(".go")) "go")
      ((member ext '(".c" ".h")) "c")
      ((member ext '(".cc" ".cpp" ".cxx" ".hpp" ".hh" ".hxx")) "cpp")
      ((member ext '(".java")) "java")
      ((member ext '(".kt" ".kts")) "kotlin")
      ((member ext '(".cs")) "csharp")
      ((member ext '(".sh" ".bash" ".zsh")) "shell")
      ((member ext '(".clj" ".cljs" ".cljc" ".bb")) "clojure")
      ((member ext '(".el")) "elisp")
      ((member ext '(".md" ".markdown")) "markdown")
      ((member ext '(".org")) "org")
      ((member ext '(".py")) "python")
      ((member ext '(".json")) "json")
      ((member ext '(".yaml" ".yml")) "yaml")
      ((member ext '(".toml")) "toml")
      ((member ext '(".ini")) "ini")
      ((member ext '(".xml" ".svg" ".plist")) "xml")
      ((member ext '(".hcl" ".tf" ".tfvars")) "hcl")
      ((member ext '(".dockerfile")) "dockerfile")
      ((member ext '(".ipynb")) "notebook"))
     fallback)))

(defun e-resource-toc-require-language (options name &optional fallback)
  "Return explicit or inferred language for OPTIONS and NAME."
  (or (plist-get options :language)
      (e-resource-toc-infer-language name fallback)
      (signal 'e-resource-toc-language-required
              (list (format "table-of-content for stdin-backed resource %s requires a language argument"
                            (or name "<unknown>"))))))

(defun e-resource-toc--argv (options &optional file language)
  "Return wot argv for OPTIONS and optional FILE or stdin LANGUAGE."
  (let ((args nil))
    (when-let ((value (plist-get options :max-depth)))
      (setq args (append args (list "--max-depth" (number-to-string value)))))
    (when-let ((value (plist-get options :max-items)))
      (setq args (append args (list "--max-items" (number-to-string value)))))
    (when-let ((value (plist-get options :min-lines)))
      (setq args (append args (list "--min-lines" (number-to-string value)))))
    (setq args (append args (list "--format" (plist-get options :format))))
    (when (plist-get options :lenient)
      (setq args (append args (list "--lenient"))))
    (if file
        (progn
          (when-let ((explicit (plist-get options :language)))
            (setq args (append args (list "--language" explicit))))
          (append args (list file)))
      (append args (list "--stdin" "--language" language)))))

(defun e-resource-toc--metadata (uri options language)
  "Return common table-of-content metadata for URI OPTIONS LANGUAGE."
  (list :uri uri
        :operation 'table-of-content
        :format (plist-get options :format)
        :wot-executable (e-resource-toc--require-wot)
        :language language))

(defun e-resource-toc--content-result (uri options language output)
  "Return table-of-content result for URI OPTIONS LANGUAGE and OUTPUT."
  (list :content output
        :metadata (e-resource-toc--metadata uri options language)))

(defun e-resource-toc--check-status (program status stderr)
  "Signal if PROGRAM exited with non-zero STATUS, using STDERR."
  (unless (zerop status)
    (signal 'e-resource-toc-process-failed
            (list (format "%s failed with exit status %s: %s"
                          program status (string-trim stderr))))))

(defun e-resource-toc--read-stderr-file (file)
  "Return stderr text from FILE when it exists."
  (if (and (stringp file) (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))
    ""))

(defun e-resource-toc-run-file (uri file options)
  "Run wot for URI using backing FILE and OPTIONS."
  (let* ((options (e-resource-toc-normalize-options options))
         (program (e-resource-toc--require-wot))
         (args (e-resource-toc--argv options file nil))
         (stdout (generate-new-buffer " *e-resource-toc-stdout*"))
         (stderr-file (make-temp-file "e-resource-toc-stderr-")))
    (unwind-protect
        (let ((status (apply #'process-file program nil (list stdout stderr-file) nil args)))
          (let ((stderr-text (e-resource-toc--read-stderr-file stderr-file))
                (stdout-text (with-current-buffer stdout (buffer-string))))
            (e-resource-toc--check-status program status stderr-text)
            (e-resource-toc--content-result
             uri options (plist-get options :language) stdout-text)))
      (when (buffer-live-p stdout) (kill-buffer stdout))
      (when (file-exists-p stderr-file) (delete-file stderr-file)))))

(defun e-resource-toc-run-content (uri name content options &optional fallback-language)
  "Run wot for URI using stdin CONTENT named NAME and OPTIONS."
  (let* ((options (e-resource-toc-normalize-options options))
         (language (e-resource-toc-require-language options name fallback-language))
         (program (e-resource-toc--require-wot))
         (args (e-resource-toc--argv options nil language))
         (stdout (generate-new-buffer " *e-resource-toc-stdout*"))
         (stderr-file (make-temp-file "e-resource-toc-stderr-")))
    (unwind-protect
        (let ((status (with-temp-buffer
                        (insert content)
                        (apply #'call-process-region
                               (point-min) (point-max)
                               program nil (list stdout stderr-file) nil args))))
          (let ((stderr-text (e-resource-toc--read-stderr-file stderr-file))
                (stdout-text (with-current-buffer stdout (buffer-string))))
            (e-resource-toc--check-status program status stderr-text)
            (e-resource-toc--content-result
             uri options language stdout-text)))
      (when (buffer-live-p stdout) (kill-buffer stdout))
      (when (file-exists-p stderr-file) (delete-file stderr-file)))))

(defun e-resource-toc--raw-process-result (raw)
  "Signal on failed process RAW and return stdout."
  (unless (eq (plist-get raw :status) 'ok)
    (signal 'e-resource-toc-process-failed
            (list (or (plist-get raw :suffix)
                      (string-trim (plist-get raw :stderr))))))
  (plist-get raw :stdout))

(defun e-resource-toc-file-work (file-resolver)
  "Return Work spec for file-backed table-of-content using FILE-RESOLVER.
FILE-RESOLVER accepts WORK-ARGUMENTS and CONTEXT and returns a plist with :uri,
:file, :options, and optional :language."
  (e-work-spec-create
   :id "resource_table_of_content_file"
   :description "Run wot for a file-backed resource."
   :execution 'process
   :interactive-policy 'async
   :owner 'resources
   :command (lambda (work-arguments context)
              (let* ((request (funcall file-resolver work-arguments context))
                     (options (e-resource-toc-normalize-options
                               (plist-get request :options)))
                     (program (e-resource-toc--require-wot)))
                (list :program program
                      :args (e-resource-toc--argv options
                                                  (plist-get request :file)
                                                  nil)
                      :metadata (list :operation 'table-of-content
                                      :scheme (plist-get (plist-get work-arguments :uri)
                                                         :scheme)
                                      :resource-uri (plist-get request :uri))
                      :state (list :uri (plist-get request :uri)
                                   :options options
                                   :language (plist-get options :language)))))
   :result-shaper (lambda (raw _work-arguments _context)
                    (let* ((state (plist-get raw :state))
                           (options (plist-get state :options)))
                      (e-resource-toc--content-result
                       (plist-get state :uri)
                       options
                       (plist-get state :language)
                       (e-resource-toc--raw-process-result raw))))))

(defun e-resource-toc-content-work (content-resolver)
  "Return Work spec for stdin-backed table-of-content using CONTENT-RESOLVER.
CONTENT-RESOLVER accepts WORK-ARGUMENTS and CONTEXT and returns a plist with
:uri, :name, :content, :options, and optional :fallback-language."
  (e-work-spec-create
   :id "resource_table_of_content_content"
   :description "Run wot for an in-memory resource through stdin."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'resources
   :runner
   (lambda (handle work-arguments context)
     (let* ((request (funcall content-resolver work-arguments context))
            (options (e-resource-toc-normalize-options
                      (plist-get request :options)))
            (language (e-resource-toc-require-language
                       options
                       (or (plist-get request :name)
                           (plist-get request :uri))
                       (plist-get request :fallback-language)))
            (program (e-resource-toc--require-wot))
            (args (e-resource-toc--argv options nil language))
            (stdout (generate-new-buffer " *e-resource-toc-stdout*"))
            (stderr (generate-new-buffer " *e-resource-toc-stderr*"))
            process)
       (cl-labels
           ((cleanup (_handle)
              (when (buffer-live-p stdout) (kill-buffer stdout))
              (when (buffer-live-p stderr) (kill-buffer stderr)))
            (buffer-text (buffer)
              (if (buffer-live-p buffer)
                  (with-current-buffer buffer (buffer-string))
                ""))
            (finish ()
              (unless (e-request-terminal-p (e-work-handle-lifecycle handle))
                (let ((status (process-exit-status process)))
                  (if (zerop status)
                      (e-work-finish
                       handle
                       (e-resource-toc--content-result
                        (plist-get request :uri)
                        options
                        language
                        (buffer-text stdout)))
                    (e-work-fail
                     handle
                     (list 'e-resource-toc-process-failed
                           (format "%s failed with exit status %s: %s"
                                   program status (string-trim (buffer-text stderr))))))))))
         (e-work-add-cleanup handle #'cleanup)
         (setf (e-work-handle-cancel-function handle)
               (lambda (_handle)
                 (when (and process (process-live-p process))
                   (kill-process process))))
         (setq process
               (make-process
                :name "e-resource-toc"
                :buffer stdout
                :stderr stderr
                :command (cons program args)
                :connection-type 'pipe
                :coding 'utf-8-unix
                :noquery t
                :sentinel (lambda (proc _event)
                            (when (and (eq proc process)
                                       (memq (process-status proc) '(exit signal)))
                              (finish)))))
         (set-process-query-on-exit-flag process nil)
         (setf (e-work-handle-metadata handle)
               (append (e-work-handle-metadata handle)
                       (list :process process
                             :transport 'process
                             :resource-uri (plist-get request :uri)
                             :resource-operation 'table-of-content)))
         (process-send-string process (plist-get request :content))
         (process-send-eof process)
         :deferred)))))

(provide 'e-resource-toc)

;;; e-resource-toc.el ends here
