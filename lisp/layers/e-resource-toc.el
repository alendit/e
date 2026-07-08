;;; e-resource-toc.el --- wot-backed resource table-of-content helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared helpers for resource methods that expose compact `wot' outlines.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-action-resources)
(require 'e-base-tools)
(require 'e-capabilities)
(require 'e-emacs-tools)
(require 'e-harness)
(require 'e-layers)
(require 'e-raw-results)
(require 'e-request)
(require 'e-resources)
(require 'e-session-tmp-resources)
(require 'e-store)
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


(defun e-resource-toc--context-harness (context)
  "Return harness from registration CONTEXT, or nil."
  (plist-get context :harness))

(defun e-resource-toc--context-session-id (context)
  "Return session id from registration CONTEXT, or nil."
  (plist-get context :session-id))

(defun e-resource-toc--context-turn-id (context)
  "Return turn id from registration CONTEXT, or nil."
  (plist-get context :turn-id))

(defun e-resource-toc--context-store (context)
  "Return e:// store from registration CONTEXT, or nil."
  (plist-get context :store))

(defun e-resource-toc--registry-has-read-scheme-p (registry scheme)
  "Return non-nil when REGISTRY already has read support for SCHEME."
  (cl-some (lambda (method)
             (equal (e-resource-method-scheme method) scheme))
           (e-resources-methods-for-operation registry e-operation-read)))

(defun e-resource-toc--workspace-roots (context)
  "Return workspace roots for resource TOC CONTEXT."
  (let ((harness (e-resource-toc--context-harness context))
        (session-id (e-resource-toc--context-session-id context))
        (turn-id (e-resource-toc--context-turn-id context)))
    (or (and (e-harness-p harness)
             session-id
             (e-harness-workspace-roots harness session-id turn-id))
        (list (file-name-as-directory (expand-file-name default-directory))))))

(defun e-resource-toc--file-request (uri options context)
  "Return a stdin-backed TOC request for file URI using CONTEXT roots."
  (let* ((roots (e-resource-toc--workspace-roots context))
         (path (e-base-tools--resource-path uri roots))
         (group (e-base-tools-file-buffer-coherence-group path (plist-get uri :uri)))
         (buffer (e-base-tools--preferred-buffer-for-group group)))
    (list :uri (plist-get uri :uri)
          :name path
          :content (if buffer
                       (with-current-buffer buffer
                         (buffer-substring-no-properties (point-min) (point-max)))
                     (e-base-tools--file-disk-text path))
          :options options)))

(defun e-resource-toc--file-method (context)
  "Return a file:// table-of-content method for CONTEXT."
  (e-resource-method-create
   :scheme "file"
   :operation e-operation-table-of-content
   :description "Workspace text files outlined with wot. Uses live buffer text when it is the coherent view; session:// is not supported."
   :uri-patterns '("file://<path>")
   :handler (lambda (uri options)
              (let ((request (e-resource-toc--file-request uri options context)))
                (e-resource-toc-run-content
                 (plist-get request :uri)
                 (plist-get request :name)
                 (plist-get request :content)
                 (plist-get request :options))))
   :work (e-resource-toc-content-work
          (lambda (work-arguments _work-context)
            (e-resource-toc--file-request
             (plist-get work-arguments :uri)
             (car (plist-get work-arguments :operation-arguments))
             context)))))

(defun e-resource-toc--buffer-request (uri options)
  "Return a stdin-backed TOC request for buffer URI."
  (let* ((name (e-emacs-tools--buffer-resource-name uri))
         (buffer (e-emacs-tools--buffer name)))
    (with-current-buffer buffer
      (list :uri (plist-get uri :uri)
            :name (or buffer-file-name name)
            :content (buffer-substring-no-properties (point-min) (point-max))
            :options options))))

(defun e-resource-toc--buffer-method ()
  "Return a buffer:// table-of-content method."
  (e-resource-method-create
   :scheme "buffer"
   :operation e-operation-table-of-content
   :description "Live Emacs buffers outlined by piping buffer text to wot --stdin. Pass language when inference is ambiguous."
   :uri-patterns '("buffer://<buffer-name>")
   :handler (lambda (uri options)
              (let ((request (e-resource-toc--buffer-request uri options)))
                (e-resource-toc-run-content
                 (plist-get request :uri)
                 (plist-get request :name)
                 (plist-get request :content)
                 (plist-get request :options))))
   :work (e-resource-toc-content-work
          (lambda (work-arguments _context)
            (e-resource-toc--buffer-request
             (plist-get work-arguments :uri)
             (car (plist-get work-arguments :operation-arguments)))))))

(defun e-resource-toc--store-method (store)
  "Return an e:// table-of-content method backed by STORE."
  (e-resource-method-create
   :scheme "e"
   :operation e-operation-table-of-content
   :description "Capability-contributed in-memory resources outlined by piping text to wot --stdin. Pass language when inference is ambiguous."
   :uri-patterns '("e://<capability>/skills/<skill>"
                   "e://<capability>/refs/<name>.md"
                   "e://<capability>/<path>")
   :handler (lambda (uri options)
              (e-resource-toc-run-content
               (plist-get uri :uri)
               (plist-get uri :address)
               (e-store-read store (plist-get uri :uri) nil)
               options))
   :work (e-resource-toc-content-work
          (lambda (work-arguments _context)
            (let ((uri (plist-get work-arguments :uri)))
              (list :uri (plist-get uri :uri)
                    :name (plist-get uri :address)
                    :content (e-store-read store (plist-get uri :uri) nil)
                    :options (car (plist-get work-arguments
                                             :operation-arguments))))))))

(defun e-resource-toc--action-method (context)
  "Return an e-action:// table-of-content method for CONTEXT."
  (let ((harness (e-resource-toc--context-harness context))
        (session-id (e-resource-toc--context-session-id context))
        (turn-id (e-resource-toc--context-turn-id context)))
    (e-resource-method-create
     :scheme "e-action"
     :operation e-operation-table-of-content
     :description "Generated action descriptions outlined by piping Markdown text to wot --stdin."
     :uri-patterns '("e-action://active"
                     "e-action://<capability>"
                     "e-action://<capability>/<action>")
     :handler (lambda (uri options)
                (e-resource-toc-run-content
                 (plist-get uri :uri)
                 (plist-get uri :address)
                 (e-action-resources--read harness session-id turn-id uri nil)
                 options
                 "markdown"))
     :work (e-resource-toc-content-work
            (lambda (work-arguments _work-context)
              (let ((uri (plist-get work-arguments :uri)))
                (list :uri (plist-get uri :uri)
                      :name (plist-get uri :address)
                      :content (e-action-resources--read
                                harness session-id turn-id uri nil)
                      :options (car (plist-get work-arguments
                                               :operation-arguments))
                      :fallback-language "markdown")))))))

(defun e-resource-toc--tmp-file-request (uri options context)
  "Return a file-backed TOC request for tmp URI."
  (let* ((harness (e-resource-toc--context-harness context))
         (session-id (e-resource-toc--context-session-id context))
         (relative-name (plist-get uri :address))
         (path (e-session-tmp--path harness session-id relative-name)))
    (list :uri (plist-get uri :uri)
          :file path
          :options options)))

(defun e-resource-toc--tmp-method (context)
  "Return a tmp:// table-of-content method for CONTEXT."
  (e-resource-method-create
   :scheme "tmp"
   :operation e-operation-table-of-content
   :description "Ephemeral session-scoped temporary text resources outlined with wot on the backing file."
   :uri-patterns '("tmp://<relative-path>")
   :handler (lambda (uri options)
              (let ((request (e-resource-toc--tmp-file-request uri options context)))
                (e-resource-toc-run-file
                 (plist-get request :uri)
                 (plist-get request :file)
                 (plist-get request :options))))
   :work (e-resource-toc-file-work
          (lambda (work-arguments _work-context)
            (e-resource-toc--tmp-file-request
             (plist-get work-arguments :uri)
             (car (plist-get work-arguments :operation-arguments))
             context)))))

(defun e-resource-toc--raw-result-method ()
  "Return a raw-result:// table-of-content method."
  (e-resource-method-create
   :scheme "raw-result"
   :operation e-operation-table-of-content
   :description "Generic ephemeral raw tool result resources outlined by piping text to wot --stdin. Pass language when inference is ambiguous."
   :uri-patterns '("raw-result://<name>")
   :handler (lambda (parsed-uri options)
              (e-resource-toc-run-content
               (plist-get parsed-uri :uri)
               (plist-get parsed-uri :address)
               (e-raw-results-read (plist-get parsed-uri :uri))
               options))
   :work (e-resource-toc-content-work
          (lambda (work-arguments _context)
            (let ((uri (plist-get work-arguments :uri)))
              (list :uri (plist-get uri :uri)
                    :name (plist-get uri :address)
                    :content (e-raw-results-read (plist-get uri :uri))
                    :options (car (plist-get work-arguments
                                             :operation-arguments))))))))

(defun e-resource-toc-register-resource-methods (registry &rest context)
  "Register all table-of-content resource methods in REGISTRY.
CONTEXT is the harness resource registration context.  Registration is skipped
when wot is not installed in `exec-path'.  The method for each scheme is added
only when the corresponding read resource scheme is already active, except e://
which follows the active store."
  (when (e-resource-toc-available-p)
    (dolist (method (delq nil
                          (list (when (e-resource-toc--registry-has-read-scheme-p registry "file")
                                  (e-resource-toc--file-method context))
                                (when (e-resource-toc--registry-has-read-scheme-p registry "buffer")
                                  (e-resource-toc--buffer-method))
                                (when-let ((store (e-resource-toc--context-store context)))
                                  (when (e-store-list store)
                                    (e-resource-toc--store-method store)))
                                (when (e-resource-toc--registry-has-read-scheme-p registry "e-action")
                                  (e-resource-toc--action-method context))
                                (when (e-resource-toc--registry-has-read-scheme-p registry "tmp")
                                  (e-resource-toc--tmp-method context))
                                (when (e-resource-toc--registry-has-read-scheme-p registry "raw-result")
                                  (e-resource-toc--raw-result-method)))))
      (e-resources-register registry method))))

(defun e-resource-toc-capability-create ()
  "Create the resource table-of-content capability."
  (e-capability-create
   :id 'resource-toc
   :name "Resource Table Of Content"
   :resource-methods
   (list (e-capability-resource-method-provider-create
          :handler #'e-resource-toc-register-resource-methods))))

(defun e-resource-toc-layer-create ()
  "Create the resource table-of-content layer."
  (e-layer-create
   :id 'resource-toc
   :name "Resource Table Of Content"
   :requires '(harness-base e os-base emacs-base)
   :capabilities (list (e-resource-toc-capability-create))))

(provide 'e-resource-toc)

;;; e-resource-toc.el ends here
