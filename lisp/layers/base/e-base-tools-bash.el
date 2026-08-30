;;; e-base-tools-bash.el --- Base bash process and output owner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns shell process lifecycle, streaming collection, cancellation, and
;; bounded output.  Filesystem/resource/coherence behavior has a separate
;; owner and is not a dependency of this module.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-request)
(require 'e-tools)
(require 'e-work)

(defcustom e-base-tools-shell-file-name nil
  "Shell executable used by the base bash tool.
When nil, `shell-file-name' is used."
  :type '(choice (const :tag "Use shell-file-name" nil) file)
  :group 'e-base-tools)

(defcustom e-base-tools-shell-command-switch nil
  "Shell command switch used by the base bash tool.
When nil, `shell-command-switch' is used."
  :type '(choice (const :tag "Use shell-command-switch" nil) string)
  :group 'e-base-tools)

(defcustom e-base-tools-default-shell-timeout 30
  "Default hard timeout (seconds) for the base bash tool."
  :type '(choice (const :tag "No default timeout (unbounded)" nil) number)
  :group 'e-base-tools)

(defcustom e-base-tools-bash-default-wait-for 30
  "Default seconds the bash tool holds the turn before detaching."
  :type 'number
  :group 'e-base-tools)

(define-error 'e-base-tools-bash-invalid "Base bash tool input is invalid")

(defconst e-base-tools-bash--default-max-lines 1000)
(defconst e-base-tools-bash--default-max-bytes (* 8 1024))

(defun e-base-tools-bash--argument-string (arguments key)
  "Return required string argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp key)))
    value))

(defun e-base-tools-bash--optional-positive-number (arguments key)
  "Return optional positive numeric KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (when value
      (unless (and (numberp value) (> value 0))
        (signal 'wrong-type-argument (list 'positive-number-p key)))
      value)))

(defmacro e-base-tools-bash--with-utf8-write (&rest body)
  "Run BODY writing bash output as UTF-8 without a coding prompt."
  (declare (indent 0) (debug t))
  `(let ((coding-system-for-write 'utf-8-unix)
         (select-safe-coding-system-function nil))
     ,@body))

(defun e-base-tools-bash--reject-sync-in-hot-path (operation)
  "Reject synchronous bash OPERATION from marked interactive hot paths."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error operation)))

(defun e-base-tools-bash--lines (content)
  "Return CONTENT split into lines, dropping a terminal empty line."
  (let ((lines (split-string content "\n")))
    (if (and (string-suffix-p "\n" content)
             lines
             (equal (car (last lines)) ""))
        (butlast lines)
      lines)))

(defun e-base-tools-bash--truncate-tail-lines (content)
  "Return bash CONTENT truncated from the tail, with metadata."
  (let* ((lines (e-base-tools-bash--lines content))
         (total-lines (length lines))
         (max-lines (max 0 (e-base-tools-bash--bash-max-lines)))
         (max-bytes (max 0 (e-base-tools-bash--bash-max-bytes)))
         (line-truncated (> total-lines max-lines))
         (tail-lines (if line-truncated (last lines max-lines) lines))
         (tail-content (concat (mapconcat #'identity tail-lines "\n")
                               (if (string-suffix-p "\n" content) "\n" "")))
         (byte-truncated nil))
    (when (> (string-bytes tail-content) max-bytes)
      (setq byte-truncated t)
      (setq tail-content
            (decode-coding-string
             (apply #'unibyte-string
                    (last (append (encode-coding-string tail-content 'utf-8) nil)
                          max-bytes))
             'utf-8 t))
      (setq tail-lines (e-base-tools-bash--lines tail-content)))
    (let* ((output-lines (length tail-lines))
           (start-line (max 1 (- total-lines output-lines -1))))
      (list :content tail-content
            :truncated (or line-truncated byte-truncated)
            :truncated-by (cond (line-truncated 'lines)
                                (byte-truncated 'bytes))
            :output-lines output-lines
            :start-line start-line
            :end-line total-lines
            :total-lines total-lines))))

(defun e-base-tools-bash--shell-command ()
  "Return command list prefix for the base bash tool."
  (list (or e-base-tools-shell-file-name shell-file-name "/bin/sh")
        (or e-base-tools-shell-command-switch shell-command-switch "-c")))

(cl-defstruct (e-base-tools-bash--bash-collector
               (:constructor e-base-tools-bash--bash-collector-create))
  output-file
  output-uri
  preview
  (preview-bytes 0)
  (preview-newlines 0)
  (preview-accepting t)
  truncated
  (total-bytes 0)
  (total-newlines 0)
  last-newline)

(defvar e-base-tools-bash--bash-progress-interval 0.1
  "Minimum seconds between streaming bash progress events.")

(defun e-base-tools-bash--bash-max-bytes ()
  "Return the active bash output byte preview limit."
  (if (boundp 'e-tool-output-truncation-max-bytes)
      e-tool-output-truncation-max-bytes
    e-base-tools-bash--default-max-bytes))

(defun e-base-tools-bash--bash-max-lines ()
  "Return the active bash output line preview limit."
  (if (boundp 'e-tool-output-truncation-max-lines)
      e-tool-output-truncation-max-lines
    e-base-tools-bash--default-max-lines))

(defun e-base-tools-bash--logical-line-count (text)
  "Return the number of logical lines in TEXT."
  (cond
   ((string-empty-p text)
    0)
   ((string-suffix-p "\n" text)
    (cl-count ?\n text))
   (t
    (1+ (cl-count ?\n text)))))

(defun e-base-tools-bash--safe-fragment (value fallback)
  "Return VALUE as a safe path fragment, or FALLBACK."
  (let* ((text (if (and (stringp value)
                        (not (string-empty-p value)))
                   value
                 fallback))
         (safe (replace-regexp-in-string "[^A-Za-z0-9._-]" "-" text)))
    (if (string-empty-p safe) fallback safe)))

(defun e-base-tools-bash--bash-relative-name (context)
  "Return a collision-resistant session tmp output name for CONTEXT."
  (let* ((turn-value (format "%s" (or (plist-get context :turn-id) "turn")))
         (call-value
          (format "%s"
                  (or (plist-get (plist-get context :tool-call) :id) "call")))
         (turn-id
          (format "%s-%s"
                  (substring (e-base-tools-bash--safe-fragment turn-value "turn")
                             0 (min 48 (length turn-value)))
                  (substring (secure-hash 'sha256 turn-value) 0 16)))
         (call-id
          (format "%s-%s"
                  (substring (e-base-tools-bash--safe-fragment call-value "call")
                             0 (min 48 (length call-value)))
                  (substring (secure-hash 'sha256 call-value) 0 16))))
    (format "tool-results/%s/bash-%s.txt" turn-id call-id)))

(defun e-base-tools-bash--context-capability-active-p (context capability-id)
  "Return non-nil when CONTEXT includes CAPABILITY-ID."
  (cl-some (lambda (capability)
             (eq (e-capability-id capability) capability-id))
           (plist-get context :capabilities)))

(defun e-base-tools-bash--bash-output-target (context)
  "Return plist describing where bash output should be streamed for CONTEXT."
  (let ((relative-name (e-base-tools-bash--bash-relative-name context)))
    (if (and (plist-get context :harness)
           (plist-get context :session-id)
           (e-base-tools-bash--context-capability-active-p
            context
            'session-tmp-resources)
           (require 'e-session-tmp-resources nil t)
           (fboundp 'e-session-tmp-file-path))
      (let ((path (e-session-tmp-file-path
                   (plist-get context :harness)
                   (plist-get context :session-id)
                   relative-name)))
        (list :output-file path
              :output-uri (format "tmp://%s" relative-name)))
      (list :output-file (make-temp-file "e-base-bash-" nil ".log")))))

(defun e-base-tools-bash--bash-collector-start (context)
  "Return a streaming bash output collector for CONTEXT."
  (let* ((target (e-base-tools-bash--bash-output-target context))
         (output-file (plist-get target :output-file)))
    (e-base-tools-bash--with-utf8-write
      (write-region "" nil output-file nil 'silent))
    (e-base-tools-bash--bash-collector-create
     :output-file output-file
     :output-uri (plist-get target :output-uri)
     :preview "")))

(defun e-base-tools-bash--bash-collector-count-chunk (collector chunk)
  "Update COLLECTOR total counters for CHUNK."
  (setf (e-base-tools-bash--bash-collector-total-bytes collector)
        (+ (e-base-tools-bash--bash-collector-total-bytes collector)
           (string-bytes chunk)))
  (setf (e-base-tools-bash--bash-collector-total-newlines collector)
        (+ (e-base-tools-bash--bash-collector-total-newlines collector)
           (cl-count ?\n chunk)))
  (when (> (length chunk) 0)
    (setf (e-base-tools-bash--bash-collector-last-newline collector)
          (eq (aref chunk (1- (length chunk))) ?\n))))

(defun e-base-tools-bash--bash-collector-add-preview (collector chunk)
  "Add as much of CHUNK as allowed to COLLECTOR preview."
  (let ((max-bytes (max 0 (e-base-tools-bash--bash-max-bytes)))
        (max-lines (max 0 (e-base-tools-bash--bash-max-lines)))
        (index 0)
        (length (length chunk)))
    (when (or (zerop max-bytes) (zerop max-lines))
      (when (> length 0)
        (setf (e-base-tools-bash--bash-collector-truncated collector) t)
        (setf (e-base-tools-bash--bash-collector-preview-accepting collector) nil))
      (setq index length))
    (while (< index length)
      (if (not (e-base-tools-bash--bash-collector-preview-accepting collector))
          (progn
            (setf (e-base-tools-bash--bash-collector-truncated collector) t)
            (setq index length))
        (let* ((char (substring chunk index (1+ index)))
               (char-bytes (string-bytes char)))
          (if (> (+ (e-base-tools-bash--bash-collector-preview-bytes collector)
                    char-bytes)
                 max-bytes)
              (progn
                (setf (e-base-tools-bash--bash-collector-truncated collector) t)
                (setf (e-base-tools-bash--bash-collector-preview-accepting collector)
                      nil)
                (setq index length))
            (setf (e-base-tools-bash--bash-collector-preview collector)
                  (concat (e-base-tools-bash--bash-collector-preview collector)
                          char))
            (setf (e-base-tools-bash--bash-collector-preview-bytes collector)
                  (+ (e-base-tools-bash--bash-collector-preview-bytes collector)
                     char-bytes))
            (when (equal char "\n")
              (setf (e-base-tools-bash--bash-collector-preview-newlines collector)
                    (1+ (e-base-tools-bash--bash-collector-preview-newlines collector)))
              (when (>= (e-base-tools-bash--bash-collector-preview-newlines collector)
                        max-lines)
                (setf (e-base-tools-bash--bash-collector-preview-accepting collector)
                      nil)))
            (setq index (1+ index))
            (when (and (< index length)
                       (not (e-base-tools-bash--bash-collector-preview-accepting
                             collector)))
              (setf (e-base-tools-bash--bash-collector-truncated collector) t)
              (setq index length))))))))

(defun e-base-tools-bash--bash-collector-append (collector chunk)
  "Append CHUNK to COLLECTOR's output file and bounded preview."
  (when (> (length chunk) 0)
    (e-base-tools-bash--with-utf8-write
      (write-region chunk nil
                    (e-base-tools-bash--bash-collector-output-file collector)
                    'append
                    'silent))
    (e-base-tools-bash--bash-collector-count-chunk collector chunk)
    (e-base-tools-bash--bash-collector-add-preview collector chunk)))

(defun e-base-tools-bash--bash-collector-original-lines (collector)
  "Return total logical line count for COLLECTOR."
  (if (zerop (e-base-tools-bash--bash-collector-total-bytes collector))
      0
    (+ (e-base-tools-bash--bash-collector-total-newlines collector)
       (if (e-base-tools-bash--bash-collector-last-newline collector) 0 1))))

(defun e-base-tools-bash--bash-collector-location (collector)
  "Return model-facing location for COLLECTOR full output."
  (or (e-base-tools-bash--bash-collector-output-uri collector)
      (e-base-tools-bash--bash-collector-output-file collector)))

(defun e-base-tools-bash--bash-truncation-notice
    (shown-bytes shown-lines original-bytes original-lines location)
  "Return a model-facing truncation notice for bash output."
  (format "[Tool output truncated: showing first %d bytes / %d lines of %d bytes / %d lines. Full output: %s]"
          shown-bytes
          shown-lines
          original-bytes
          original-lines
          location))

(defun e-base-tools-bash--bash-collector-metadata (collector)
  "Return truncation metadata for COLLECTOR."
  (let ((metadata (list :truncated t
                        :original-bytes
                        (e-base-tools-bash--bash-collector-total-bytes collector)
                        :original-lines
                        (e-base-tools-bash--bash-collector-original-lines collector)
                        :shown-bytes
                        (e-base-tools-bash--bash-collector-preview-bytes collector)
                        :shown-lines
                        (e-base-tools-bash--logical-line-count
                         (e-base-tools-bash--bash-collector-preview collector)))))
    (if (e-base-tools-bash--bash-collector-output-uri collector)
        (plist-put metadata
                   :tmp-uri
                   (e-base-tools-bash--bash-collector-output-uri collector))
      (plist-put metadata
                 :full-output-path
                 (e-base-tools-bash--bash-collector-output-file collector)))))

(defun e-base-tools-bash--bash-collector-content (collector &optional suffix)
  "Return bounded model-facing content from COLLECTOR with optional SUFFIX."
  (let ((preview (e-base-tools-bash--bash-collector-preview collector)))
    (if (not (e-base-tools-bash--bash-collector-truncated collector))
        (if suffix
            (string-trim-right (format "%s\n\n%s" preview suffix))
          preview)
      (let* ((metadata (e-base-tools-bash--bash-collector-metadata collector))
             (notice (e-base-tools-bash--bash-truncation-notice
                      (plist-get metadata :shown-bytes)
                      (plist-get metadata :shown-lines)
                      (plist-get metadata :original-bytes)
                      (plist-get metadata :original-lines)
                      (e-base-tools-bash--bash-collector-location collector)))
             (content (if (string-empty-p preview)
                          notice
                        (concat preview "\n\n" notice))))
        (if suffix
            (string-trim-right (format "%s\n\n%s" content suffix))
          content)))))

(defun e-base-tools-bash--bash-progress-payload (collector call)
  "Return compact streaming progress for COLLECTOR and tool CALL."
  (let ((metadata (e-base-tools-bash--bash-collector-metadata collector)))
    (append
     (list :tool-call-id (plist-get call :id)
           :name (plist-get call :name)
           :bytes (e-base-tools-bash--bash-collector-total-bytes collector)
           :lines (e-base-tools-bash--bash-collector-original-lines collector)
           :preview (e-base-tools-bash--bash-collector-preview collector))
     metadata)))

(defun e-base-tools-bash--bash-finish-value
    (collector call status &optional suffix)
  "Return final bash result value from COLLECTOR for CALL and STATUS."
  (when suffix
    ;; Make the backing file the complete semantic string.  The bounded
    ;; preview is updated independently while output remains within its limit.
    (e-base-tools-bash--bash-collector-append collector (format "\n\n%s" suffix)))
  (let* ((file-backed-p
          (e-base-tools-bash--bash-collector-output-uri collector))
         (content
          (if file-backed-p
              (e-tools-file-content-create
               :path (e-base-tools-bash--bash-collector-output-file collector)
               :uri (e-base-tools-bash--bash-collector-output-uri collector)
               :preview (e-base-tools-bash--bash-collector-preview collector)
               :original-bytes
               (e-base-tools-bash--bash-collector-total-bytes collector)
               :original-lines
               (e-base-tools-bash--bash-collector-original-lines collector)
               :preview-bytes
               (e-base-tools-bash--bash-collector-preview-bytes collector)
               :preview-lines
               (e-base-tools-bash--logical-line-count
                (e-base-tools-bash--bash-collector-preview collector))
               :owned t)
            (e-base-tools-bash--bash-collector-content collector)))
         (metadata
          (unless file-backed-p
            (when (e-base-tools-bash--bash-collector-truncated collector)
              (e-base-tools-bash--bash-collector-metadata collector)))))
    (when (and (not file-backed-p)
               (not (e-base-tools-bash--bash-collector-truncated collector))
               (file-exists-p
                (e-base-tools-bash--bash-collector-output-file collector)))
      (delete-file (e-base-tools-bash--bash-collector-output-file collector)))
    (if call
        (e-tools-result-create call status content metadata)
      content)))

(defun e-base-tools-bash--bash-work-command (directory arguments context)
  "Return the process carrier command for bash ARGUMENTS in DIRECTORY.
CONTEXT is the current tool context, used for session-tmp output ownership and
tool-call progress metadata."
  (let* ((command (e-base-tools-bash--argument-string arguments :command))
         (timeout (if (plist-member arguments :timeout)
                      (e-base-tools-bash--optional-positive-number arguments :timeout)
                    e-base-tools-default-shell-timeout))
         (command-prefix (e-base-tools-bash--shell-command))
         (shell-command (format "{\n%s\n} 2>&1" command))
         (collector (e-base-tools-bash--bash-collector-start context))
         (call (plist-get context :tool-call)))
    (list :name "e-base-bash"
          :program (car command-prefix)
          :args (append (cdr command-prefix) (list shell-command))
          :directory directory
          :capture-output nil
          :finish-on-nonzero t
          :finish-on-timeout t
          :timeout timeout
          :timeout-message
          (and timeout
               (format "Command timed out after %s seconds" timeout))
          :state collector
          :metadata (list :output-file
                          (e-base-tools-bash--bash-collector-output-file collector)
                          :output-uri
                          (e-base-tools-bash--bash-collector-output-uri collector)
                          :cancellable t)
          :on-cancel
          (lambda (_handle _process active-collector)
            (when (file-exists-p
                    (e-base-tools-bash--bash-collector-output-file active-collector))
              (delete-file
               (e-base-tools-bash--bash-collector-output-file active-collector))))
          :on-output
          (lambda (_handle _process chunk active-collector)
            (e-base-tools-bash--bash-collector-append active-collector chunk))
          :progress
          (lambda (_handle _process active-collector)
            (when (> (e-base-tools-bash--bash-collector-total-bytes
                      active-collector)
                     0)
              (e-base-tools-bash--bash-progress-payload active-collector call)))
          :progress-interval e-base-tools-bash--bash-progress-interval)))

(defun e-base-tools-bash--bash-work-result (raw _arguments context)
  "Shape bash process carrier RAW output into the existing tool result."
  (let* ((collector (plist-get raw :state))
         (call (plist-get context :tool-call))
         (status (plist-get raw :status))
         (reason (plist-get raw :reason))
         (exit-code (plist-get raw :exit-code))
         (suffix (pcase reason
                   ('exit
                    (unless (eq status 'ok)
                      (format "Command exited with code %s" exit-code)))
                   (_ (plist-get raw :suffix)))))
    (if (or call (eq status 'ok))
        (e-base-tools-bash--bash-finish-value collector call status suffix)
      (signal 'e-base-tools-bash-invalid
              (list (e-base-tools-bash--bash-collector-content collector suffix))))))

(defun e-base-tools-bash--bash-child-work (directory)
  "Return the process work spec that runs one bash command in DIRECTORY.
This is the raw carrier: it starts the shell process, streams output, and
shapes the captured result.  The detachable tool spec races it against
`wait_for'; the synchronous helper runs it directly to completion."
  (e-work-spec-create
   :id "bash"
   :description "Run a shell command through the base bash tool."
   :execution 'process
   :interactive-policy 'async
   :owner 'base-tools
   :command (lambda (arguments context)
              (e-base-tools-bash--bash-work-command directory arguments context))
   :result-shaper #'e-base-tools-bash--bash-work-result))

(defun e-base-tools-bash--bash-work (directory)
  "Return the detachable work spec backing the bash tool in DIRECTORY.
The command runs inline for up to `wait_for' seconds; if it is still running
when that window expires, it detaches into the generic detached-work registry
and the call returns a `work:<id>' reference plus the streaming `output_uri'."
  (e-work-detachable-spec
   (e-base-tools-bash--bash-child-work directory)
   :id "bash"
   :description "Run a shell command through the base bash tool."
   :owner 'base-tools
   :default-wait-for e-base-tools-bash-default-wait-for
   :ack-extra
   (lambda (arguments)
     (list :command (e-base-tools-bash--argument-string arguments :command)))))

(cl-defun e-base-tools-bash--run-shell-command-start
    (command directory timeout &key on-done on-error on-request-start on-event)
  "Start shell COMMAND in DIRECTORY with optional TIMEOUT seconds.
ON-DONE receives captured output.  ON-ERROR receives an Emacs condition list.
ON-REQUEST-START receives the cancellable process request.  ON-EVENT receives
streaming progress events."
  (let* ((context (e-tools-current-context))
         (arguments (list :command command :timeout timeout))
         ;; The synchronous helper runs the raw process carrier to completion;
         ;; racing/detachment is only for the model-facing tool path.
         (handle (e-work-start
                  (e-base-tools-bash--bash-child-work directory)
                  arguments
                  :context context
                  :on-done (lambda (value)
                             (when on-done
                               (funcall on-done value)))
                  :on-error (lambda (err)
                              (when on-error
                                (funcall on-error err)))
                  :on-progress
                  (lambda (payload)
                    (when on-event
                      (funcall on-event 'tool-progress payload)))))
         (request (e-tools-request-create
                   :cancel (lambda ()
                             (e-work-cancel handle)
                             t)
                   :metadata (append
                              (list :transport 'work
                                    :work-id (e-work-handle-id handle)
                                    :work-handle handle)
                              (e-work-handle-metadata handle)))))
    (when on-request-start
      (funcall on-request-start request))
    request))

(defun e-base-tools-bash--run-shell-command (command directory timeout)
  "Run shell COMMAND in DIRECTORY with optional TIMEOUT seconds."
  (e-base-tools-bash--reject-sync-in-hot-path 'e-base-tools-bash--run-shell-command)
  (let ((done nil)
        (result nil)
        (failure nil))
    (e-base-tools-bash--run-shell-command-start
     command
     directory
     timeout
     :on-done (lambda (output)
                (setq result output)
                (setq done t))
     :on-error (lambda (err)
                 (setq failure err)
                 (setq done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(defun e-base-tools-bash--truncate-bash-output (output)
  "Return bash OUTPUT, truncating and persisting full output when needed."
  (let ((truncation (e-base-tools-bash--truncate-tail-lines output)))
    (if (not (plist-get truncation :truncated))
        output
      (let ((full-output-path (make-temp-file "e-base-bash-" nil ".log")))
        (e-base-tools-bash--with-utf8-write
          (write-region output nil full-output-path nil 'silent))
        (format "%s\n\n[Showing lines %d-%d of %d. Full output: %s]"
                (plist-get truncation :content)
                (plist-get truncation :start-line)
                (plist-get truncation :end-line)
                (plist-get truncation :total-lines)
                full-output-path)))))

(defun e-base-tools-register-bash (registry directory)
  "Register the base bash tool in REGISTRY rooted at DIRECTORY."
  (e-tools-register
   registry
   :name "bash"
   :description "Execute a shell command in the current working directory and return captured stdout and stderr. Never start a recursive search or traversal whose effective root is `/`, `~`, `$HOME`, or any other large ancestor. A large ancestor is not only the home directory: any directory that holds many projects or repositories is a banned root too, not just the single project you care about. This is about where the walk actually reaches, not the literal argument: a recursive `find`, `grep -r`, or `ls -R` rooted at home, at a parent that contains many projects, or at any other huge tree is banned, and so is a bare `find .` or `grep -rn PATTERN .` when the working directory itself is such a tree -- they are equally slow and flood output. Before a recursive search, resolve where the root actually points and consider what lives under it: if it is the home directory, a directory of many projects, or otherwise large, do not search it. Descend to the single project or repository that matters and scope the search there rather than at a broad parent or a bare `.` sitting at one. A bare `.` is only safe when the working directory is itself one bounded project directory. When a command's runtime OR extent is unknown or potentially unbounded -- a network fetch, a build, a watcher, a server, anything that may hang, and equally any filesystem search or traversal (`find`, `grep -r`, `ls -R`, `du`) whose reached scope you are not certain is small -- wrap it in `timeout(1)` (e.g. `timeout 30 CMD`) so it self-terminates, in addition to the tool's own `timeout` parameter. If you are unsure how big a directory tree is or how long a command will take, assume it is large and add `timeout(1)` rather than running it bare."
   :parameters (e-work-detachable-merge-parameters
                '(:type "object"
                  :properties (:command (:type "string")
                               :timeout (:type "number"
                                         :description "Hard timeout in seconds. When reached, e kills the process and returns a tool error. Keep this SMALL and modest: default to about 10s for routine commands and 30s at most for anything you expect to be quick. Setting no timeout, or a large one, is a mistake for ordinary commands -- a bounded command that hangs should fail fast, not stall the turn. Only exceed 30s when the command is genuinely expected to run long (a real build, a large test suite, a slow network fetch), and prefer an explicit control pattern (backgrounding, polling) over a big blocking timeout.")
                               :resource_usage
                               (:type "object"
                                :description "Optional high-value resource usage for future context. Use only when the command reads, writes, or edits resources that matter for future work."
                                :properties (:resources
                                             (:type "array"
                                              :items
                                              (:type "object"
                                               :properties
                                               (:uri (:type "string")
                                                :operation
                                                (:type "string"
                                                 :enum ["read" "write" "edit"]))
                                               :required ["uri" "operation"]))
                                             :summary
                                             (:type "string"
                                              :description "Compact summary of why these resources matter."))))
                  :required ["command"]))
   :work (e-base-tools-bash--bash-work directory)))


(provide 'e-base-tools-bash)

;;; e-base-tools-bash.el ends here
