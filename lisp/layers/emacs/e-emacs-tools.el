;;; e-emacs-tools.el --- Harmless Emacs tools for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Low-risk concrete tools.  M2 intentionally avoids file writes, process
;; execution, elisp evaluation, buffer edits, and harness mutation tools.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-operations)
(require 'e-resource-patterns)
(require 'e-resource-query)
(require 'e-resource-toc)
(require 'e-resources)
(require 'e-tools)

(define-error 'e-emacs-tools-buffer-missing "Emacs buffer is missing")
(define-error 'e-emacs-tools-edit-invalid "Emacs buffer edit is invalid")
(define-error 'e-emacs-tools-save-invalid "Emacs buffer cannot be saved")
(define-error 'e-emacs-tools-run-elisp-timeout
  "run_elisp evaluation exceeded its time budget")
(define-error 'e-emacs-tools-run-elisp-blocking
  "run_elisp code calls a blocking wait primitive")

(defcustom e-emacs-tools-run-elisp-print-length 200
  "Maximum sequence entries printed from a `run_elisp' result."
  :type 'integer
  :group 'e)

(defcustom e-emacs-tools-run-elisp-print-level 8
  "Maximum nesting depth printed from a `run_elisp' result."
  :type 'integer
  :group 'e)

(defcustom e-emacs-tools-run-elisp-string-max-bytes 4096
  "Maximum bytes retained from any string inside a `run_elisp' result."
  :type 'integer
  :group 'e)

(defcustom e-emacs-tools-run-elisp-result-max-bytes (* 16 1024)
  "Maximum bytes returned in the printed `run_elisp' result."
  :type 'integer
  :group 'e)

(defcustom e-emacs-tools-run-elisp-default-timeout 10
  "Default seconds before interactive `run_elisp' evaluation is aborted.
A `run_elisp' call may override this with its own `:timeout' argument.  When
nil, evaluation runs uncapped unless the call supplies a timeout.

The timeout is a cooperative backstop, not a hard kill.  It fires from a
timer on the single Emacs thread, and that timer only runs when execution
reaches a wait point (`sleep-for', `sit-for', `accept-process-output',
I/O).  So it aborts an eval that blocks or waits, but it cannot preempt a
tight compute loop such as (while t) -- that spins until the process is
killed.  Long or guaranteed-killable work belongs in `elisp-job', which
runs a separate process."
  :type '(choice (const :tag "Uncapped" nil)
                 number)
  :group 'e)

(defun e-emacs-tools--buffer (name)
  "Return live buffer NAME or signal an explicit tool error."
  (or (get-buffer name)
      (signal 'e-emacs-tools-buffer-missing
              (list (format "No buffer named %s" name)))))

(defun e-emacs-tools--buffer-visible-p (buffer)
  "Return non-nil when BUFFER is visible in a live window."
  (and (get-buffer-window buffer t) t))

(defun e-emacs-tools--buffer-metadata (buffer)
  "Return metadata for BUFFER."
  (with-current-buffer buffer
    (list :name (buffer-name buffer)
          :mode (symbol-name major-mode)
          :file buffer-file-name
          :file-backed (and buffer-file-name t)
          :modified (buffer-modified-p buffer)
          :visible (e-emacs-tools--buffer-visible-p buffer))))

(defun e-emacs-tools--buffer-updated-at (buffer)
  "Return reliable BUFFER updated time, or nil."
  (with-current-buffer buffer
    (when (and buffer-file-name (file-exists-p buffer-file-name))
      (file-attribute-modification-time (file-attributes buffer-file-name)))))

(defun e-emacs-tools--buffer-resource (buffer)
  "Return public resource result for BUFFER."
  (let ((name (buffer-name buffer)))
    (list :uri (concat "buffer://" name)
          :name name
          :kind 'buffer
          :metadata (append (e-emacs-tools--buffer-metadata buffer)
                            (when-let ((updated-at
                                        (e-emacs-tools--buffer-updated-at buffer)))
                              (list :updated-at updated-at))))))

(defun e-emacs-tools--buffer-query-field-functions ()
  "Return buffer:// resource query field functions."
  `(("name" . ,(lambda (resource) (plist-get resource :name)))
    ("uri" . ,(lambda (resource) (plist-get resource :uri)))
    ("updated-at" . ,(lambda (resource)
                         (plist-get (plist-get resource :metadata) :updated-at)))))

(defun e-emacs-tools--buffer-apply-query
    (resources sort-by sort-order created-after created-before
               updated-after updated-before)
  "Apply buffer:// query controls to RESOURCES."
  (e-resource-query-apply
   resources
   "buffer"
   '("default" "name" "uri" "updated-at")
   '("updated-at")
   :sort-by sort-by
   :sort-order sort-order
   :created-after created-after
   :created-before created-before
   :updated-after updated-after
   :updated-before updated-before
   :field-functions (e-emacs-tools--buffer-query-field-functions)))

(defun e-emacs-tools-buffer-metadata-list (&optional visible-only)
  "Return metadata for live buffers.
When VISIBLE-ONLY is non-nil, include only buffers visible in windows."
  (let ((buffers nil))
    (dolist (buffer (buffer-list))
      (when (or (not visible-only)
                (e-emacs-tools--buffer-visible-p buffer))
        (push (e-emacs-tools--buffer-metadata buffer) buffers)))
    (nreverse buffers)))

(defun e-emacs-tools--argument-string (arguments key)
  "Return required string argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp value)))
    value))

(defun e-emacs-tools--range-start (arguments)
  "Return requested buffer range start from ARGUMENTS."
  (or (plist-get arguments :start) 1))

(defun e-emacs-tools--range-end (arguments)
  "Return requested buffer range end from ARGUMENTS or nil."
  (plist-get arguments :end))

(defun e-emacs-tools--read-range (arguments)
  "Return validated buffer range from ARGUMENTS."
  (let ((start (e-emacs-tools--range-start arguments))
        (requested-end (e-emacs-tools--range-end arguments)))
    (let ((exclusive-end (if requested-end
                             (1+ requested-end)
                           (point-max))))
      (unless (and (integerp start)
                   (integerp exclusive-end)
                   (<= (point-min) start exclusive-end (point-max)))
        (signal 'args-out-of-range (list start requested-end)))
      (list :start start
            :exclusive-end exclusive-end
            :reported-end (or requested-end (1- exclusive-end))))))

(defun e-emacs-tools--range-positive-number (range key)
  "Return optional positive integer KEY from RANGE."
  (let ((value (plist-get range key)))
    (when value
      (unless (and (integerp value) (> value 0))
        (signal 'wrong-type-argument (list 'positive-integer-p key)))
      value)))

(defun e-emacs-tools--line-range (range)
  "Return character bounds for line-based RANGE in the current buffer."
  (let ((start-line (e-emacs-tools--range-positive-number range :start))
        (end-line (e-emacs-tools--range-positive-number range :end)))
    (unless start-line
      (signal 'args-out-of-range (list :start nil)))
    (when (and end-line (< end-line start-line))
      (signal 'args-out-of-range (list start-line end-line)))
    (save-excursion
      (goto-char (point-min))
      (when (> (forward-line (1- start-line)) 0)
        (signal 'args-out-of-range (list start-line end-line)))
      (let ((start (point)))
        (if end-line
            (progn
              (goto-char (point-min))
              (when (> (forward-line (1- end-line)) 0)
                (signal 'args-out-of-range (list start-line end-line)))
              (list :start start
                    :exclusive-end (min (point-max) (1+ (line-end-position)))
                    :reported-end end-line))
          (list :start start
                :exclusive-end (point-max)
                :reported-end (1- (point-max))))))))

(defun e-emacs-tools--resource-read-range (range)
  "Return validated character bounds for structured resource RANGE."
  (if (null range)
      (e-emacs-tools--read-range nil)
    (pcase (plist-get range :unit)
      ((or "offset" "char")
       (let* ((start (or (e-emacs-tools--range-positive-number range :start) 1))
              (limit (e-emacs-tools--range-positive-number range :limit))
              (end (or (plist-get range :end)
                       (and limit (1- (+ start limit))))))
         (e-emacs-tools--read-range (list :start start :end end))))
      ("line"
       (e-emacs-tools--line-range range))
      (_
       (signal 'args-out-of-range
               (list (format "Unsupported buffer range unit: %s"
                             (plist-get range :unit))))))))

(defun e-emacs-tools--replacement-count (old-text)
  "Return match positions for OLD-TEXT in current buffer."
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward old-text nil t)
        (push (cons (match-beginning 0) (match-end 0)) matches)))
    (nreverse matches)))

(defun e-emacs-tools--edit-field (edit key)
  "Return string KEY from EDIT."
  (let ((value (plist-get edit key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp key)))
    value))

(defun e-emacs-tools--normalize-edits (edits)
  "Return normalized resource EDITS for exact buffer replacement."
  (unless (and (listp edits) edits)
    (signal 'e-emacs-tools-edit-invalid
            '("edits must contain at least one replacement")))
  (cl-loop for edit in edits
           collect (let ((old-text (e-emacs-tools--edit-field edit :oldText))
                         (new-text (e-emacs-tools--edit-field edit :newText)))
                     (when (string-empty-p old-text)
                       (signal 'e-emacs-tools-edit-invalid
                               '("oldText must not be empty")))
                     (when (equal old-text new-text)
                       (signal 'e-emacs-tools-edit-invalid
                               '("oldText and newText are identical")))
                     (list :old-text old-text :new-text new-text))))

(defun e-emacs-tools--apply-edits-to-current-buffer (edits)
  "Apply exact resource EDITS to the current buffer."
  (let ((matches nil)
        (index 0))
    (dolist (edit edits)
      (let* ((old-text (plist-get edit :old-text))
             (positions (e-emacs-tools--replacement-count old-text)))
        (pcase (length positions)
          (0 (signal 'e-emacs-tools-edit-invalid
                     (list (format "edits[%d].oldText was not found" index))))
          (1 (let ((match (car positions)))
               (push (list :edit-index index
                           :start (car match)
                           :end (cdr match)
                           :new-text (plist-get edit :new-text))
                     matches)))
          (_ (signal 'e-emacs-tools-edit-invalid
                     (list (format "edits[%d].oldText matched more than once"
                                   index))))))
      (setq index (1+ index)))
    (setq matches (sort matches
                        (lambda (left right)
                          (< (plist-get left :start)
                             (plist-get right :start)))))
    (cl-loop for previous in matches
             for current in (cdr matches)
             when (> (plist-get previous :end) (plist-get current :start))
             do (signal 'e-emacs-tools-edit-invalid
                        (list (format "edits[%d] and edits[%d] overlap"
                                      (plist-get previous :edit-index)
                                      (plist-get current :edit-index)))))
    (dolist (match (reverse matches))
      (goto-char (plist-get match :start))
      (delete-region (plist-get match :start) (plist-get match :end))
      (insert (plist-get match :new-text)))
    (length matches)))

(defun e-emacs-tools--buffer-resource-name (uri)
  "Return buffer name addressed by parsed buffer URI."
  (plist-get uri :address))

(defun e-emacs-tools--discovery-limit (limit)
  "Return normalized discovery LIMIT."
  (cond
   ((null limit) 100)
   ((and (numberp limit) (> limit 0)) (truncate limit))
   (t (signal 'wrong-type-argument (list 'positive-number-p limit)))))

(defun e-emacs-tools--internal-buffer-p (name)
  "Return non-nil when NAME is an internal buffer name."
  (string-prefix-p " " name))

(defun e-emacs-tools--buffer-resource-candidates
    (uri &optional pattern case-sensitive)
  "Return live buffers matching parsed URI and optional glob PATTERN."
  (let* ((prefix (e-emacs-tools--buffer-resource-name uri))
         (actual-pattern (or pattern "*"))
         (actual-case-sensitive (if (null case-sensitive) t case-sensitive))
         buffers)
    (e-resource-pattern-compile-glob actual-pattern)
    (dolist (buffer (buffer-list) (nreverse buffers))
      (let ((name (buffer-name buffer)))
        (when (and (string-prefix-p prefix name)
                   (let ((relative-name (if (string-empty-p prefix)
                                            name
                                          (substring name (length prefix)))))
                     (e-resource-pattern-glob-match-p
                      actual-pattern
                      relative-name
                      actual-case-sensitive))
                   (or (not (e-emacs-tools--internal-buffer-p name))
                       (string= prefix name)
                       pattern))
          (push buffer buffers))))))

(defun e-emacs-tools--buffer-glob-resource
    (uri pattern limit case-sensitive &optional sort-by sort-order
         created-after created-before updated-after updated-before)
  "List live buffer resources under parsed URI with PATTERN and LIMIT."
  (let* ((actual-limit (e-emacs-tools--discovery-limit limit))
         (resources (mapcar #'e-emacs-tools--buffer-resource
                            (e-emacs-tools--buffer-resource-candidates
                             uri
                             pattern
                             case-sensitive)))
         (queried (e-emacs-tools--buffer-apply-query
                   resources sort-by sort-order created-after created-before
                   updated-after updated-before))
         (truncated (> (length queried) actual-limit)))
    (list :resources (vconcat (seq-take queried actual-limit))
          :truncated truncated)))

(defun e-emacs-tools--current-line-text ()
  "Return current line text without properties."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun e-emacs-tools--buffer-search-one (buffer query options)
  "Return search matches for BUFFER and QUERY with OPTIONS."
  (let ((case-fold-search (not (plist-get options :case-sensitive)))
        (regexp (e-resource-pattern-search-emacs-regexp query options))
        matches)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (catch 'done
          (while (re-search-forward regexp nil t)
            (let ((start (match-beginning 0))
                  (end (match-end 0)))
              (push (list :uri (concat "buffer://" (buffer-name buffer))
                          :line (line-number-at-pos start)
                          :column (1+ (- start (line-beginning-position)))
                          :text (e-emacs-tools--current-line-text))
                    matches)
              (when (= start end)
                (if (eobp)
                    (throw 'done nil)
                  (forward-char 1))))))))
    (nreverse matches)))

(defun e-emacs-tools--buffer-search-resource (uri query options)
  "Search live buffer resources under parsed URI for QUERY with OPTIONS."
  (let* ((actual-limit (e-emacs-tools--discovery-limit
                        (plist-get options :limit)))
         (buffers (e-emacs-tools--buffer-resource-candidates
                   uri
                   (plist-get options :glob)
                   t))
         (buffer-by-name (make-hash-table :test 'equal))
         matches)
    (dolist (buffer buffers)
      (puthash (buffer-name buffer) buffer buffer-by-name))
    (setq buffers
          (delq nil
                (mapcar (lambda (resource)
                          (gethash (plist-get resource :name) buffer-by-name))
                        (e-resource-query-apply-search
                         (mapcar #'e-emacs-tools--buffer-resource buffers)
                         "buffer"
                         '("default" "name" "uri" "updated-at")
                         '("updated-at")
                         options
                         (e-emacs-tools--buffer-query-field-functions)))))
    (dolist (buffer buffers)
      (setq matches
            (append matches
                    (e-emacs-tools--buffer-search-one
                     buffer query options))))
    (list :matches (vconcat (seq-take matches actual-limit))
          :truncated (> (length matches) actual-limit))))

(defun e-emacs-tools--read-buffer-resource (uri range)
  "Read parsed buffer URI with structured RANGE."
  (let ((name (e-emacs-tools--buffer-resource-name uri)))
    (with-current-buffer (e-emacs-tools--buffer name)
      (let* ((bounds (e-emacs-tools--resource-read-range range))
             (start (plist-get bounds :start))
             (exclusive-end (plist-get bounds :exclusive-end))
             (reported-end (plist-get bounds :reported-end)))
        (list :name name
              :content (buffer-substring-no-properties start exclusive-end)
              :start start
              :end reported-end)))))

(defun e-emacs-tools--write-buffer-resource (uri content)
  "Replace parsed buffer URI contents with CONTENT."
  (let ((name (e-emacs-tools--buffer-resource-name uri)))
    (with-current-buffer (get-buffer-create name)
      (erase-buffer)
      (insert content)
      (append
       (list :chars (length content)
             :saved nil)
       (e-emacs-tools--buffer-metadata (current-buffer))))))

(defun e-emacs-tools--edit-buffer-resource (uri edits)
  "Apply exact resource EDITS to parsed buffer URI."
  (let* ((name (e-emacs-tools--buffer-resource-name uri))
         (normalized-edits (e-emacs-tools--normalize-edits edits)))
    (with-current-buffer (e-emacs-tools--buffer name)
      (let ((replacements (e-emacs-tools--apply-edits-to-current-buffer
                           normalized-edits)))
        (append
         (list :replacements replacements
               :saved nil)
         (e-emacs-tools--buffer-metadata (current-buffer)))))))

(defun e-emacs-tools--buffer-read-method ()
  "Return a buffer read resource method."
  (e-resource-method-create
   :scheme "buffer"
   :operation e-operation-read
   :description "Live Emacs buffers. Buffer names are matched exactly."
   :uri-patterns '("buffer://<buffer-name>")
   :range-modes '("offset" "char" "line")
   :handler #'e-emacs-tools--read-buffer-resource))

(defun e-emacs-tools--buffer-write-method ()
  "Return a buffer write resource method."
  (e-resource-method-create
   :scheme "buffer"
   :operation e-operation-write
   :description "Live Emacs buffers. Writes mutate buffers but do not save file-backed buffers."
   :uri-patterns '("buffer://<buffer-name>")
   :handler #'e-emacs-tools--write-buffer-resource))

(defun e-emacs-tools--buffer-edit-method ()
  "Return a buffer edit resource method."
  (e-resource-method-create
   :scheme "buffer"
   :operation e-operation-edit
   :description "Live Emacs buffers. Edits mutate buffers but do not save file-backed buffers."
   :uri-patterns '("buffer://<buffer-name>")
   :handler #'e-emacs-tools--edit-buffer-resource))

(defun e-emacs-tools--buffer-glob-method ()
  "Return a buffer glob resource method."
  (e-resource-method-create
   :scheme "buffer"
   :operation e-operation-glob
   :description "Live Emacs buffers by name prefix and glob pattern."
   :uri-patterns '("buffer://<optional-buffer-name-prefix>")
   :handler (lambda (uri pattern limit case-sensitive sort-by sort-order
                     created-after created-before updated-after updated-before)
              (e-emacs-tools--buffer-glob-resource
               uri pattern limit case-sensitive sort-by sort-order
               created-after created-before updated-after updated-before))))

(defun e-emacs-tools--buffer-search-method ()
  "Return a buffer search resource method."
  (e-resource-method-create
   :scheme "buffer"
   :operation e-operation-search
   :description "Live Emacs buffer content searched natively."
   :uri-patterns '("buffer://<optional-buffer-name-prefix>")
   :handler #'e-emacs-tools--buffer-search-resource))


(defun e-emacs-tools--buffer-table-of-content-request (uri options)
  "Return a stdin-backed table-of-content request for buffer URI."
  (let* ((name (e-emacs-tools--buffer-resource-name uri))
         (buffer (e-emacs-tools--buffer name)))
    (with-current-buffer buffer
      (list :uri (plist-get uri :uri)
            :name (or buffer-file-name name)
            :content (buffer-substring-no-properties (point-min) (point-max))
            :options options))))

(defun e-emacs-tools--buffer-table-of-content-method ()
  "Return a buffer table-of-content resource method, if available."
  (when (e-resource-toc-available-p)
    (e-resource-method-create
     :scheme "buffer"
     :operation e-operation-table-of-content
     :description "Live Emacs buffers outlined by piping buffer text to wot --stdin. Pass language when inference is ambiguous."
     :uri-patterns '("buffer://<buffer-name>")
     :handler (lambda (uri options)
                (let ((request (e-emacs-tools--buffer-table-of-content-request uri options)))
                  (e-resource-toc-run-content
                   (plist-get request :uri)
                   (plist-get request :name)
                   (plist-get request :content)
                   (plist-get request :options))))
     :work (e-resource-toc-content-work
            (lambda (work-arguments _context)
              (e-emacs-tools--buffer-table-of-content-request
               (plist-get work-arguments :uri)
               (car (plist-get work-arguments :operation-arguments))))))))

(defun e-emacs-tools-register-buffer-read-resource (registry)
  "Register read-only buffer resource methods in REGISTRY."
  (dolist (method (delq nil
                         (list (e-emacs-tools--buffer-read-method)
                               (e-emacs-tools--buffer-glob-method)
                               (e-emacs-tools--buffer-search-method)
                               (e-emacs-tools--buffer-table-of-content-method))))
    (e-resources-register registry method)))

(defun e-emacs-tools-register-buffer-resource (registry)
  "Register buffer resource methods in REGISTRY."
  (dolist (method (delq nil
                         (list (e-emacs-tools--buffer-read-method)
                               (e-emacs-tools--buffer-write-method)
                               (e-emacs-tools--buffer-edit-method)
                               (e-emacs-tools--buffer-glob-method)
                               (e-emacs-tools--buffer-search-method)
                               (e-emacs-tools--buffer-table-of-content-method))))
    (e-resources-register registry method)))

(defun e-emacs-tools--read-forms (code)
  "Read all elisp forms from CODE."
  (let ((position 0)
        (forms nil)
        read-result)
    (while (< position (length code))
      (setq read-result (read-from-string code position))
      (push (car read-result) forms)
      (setq position (cdr read-result))
      (while (and (< position (length code))
                  (memq (aref code position) '(?\s ?\t ?\n ?\r)))
        (setq position (1+ position))))
    (nreverse forms)))

(defun e-emacs-tools--run-elisp-timeout (arguments)
  "Return the effective `run_elisp' timeout in seconds from ARGUMENTS.
A positive numeric `:timeout' argument overrides
`e-emacs-tools-run-elisp-default-timeout'.  A non-positive `:timeout' disables
the cap for that call.  Returns nil when evaluation should run uncapped."
  (let ((value (plist-get arguments :timeout)))
    (cond
     ((null value) e-emacs-tools-run-elisp-default-timeout)
     ((numberp value) (and (> value 0) value))
     (t (signal 'wrong-type-argument (list 'numberp :timeout))))))

(defun e-emacs-tools--reject-run-elisp-timeout (timeout)
  "Signal that `run_elisp' evaluation exceeded TIMEOUT seconds."
  (signal
   'e-emacs-tools-run-elisp-timeout
   (list
    (format
     "run_elisp evaluation aborted after %s seconds; move long-running work to (e-actions-call 'elisp-job :run-batch ...), which runs a separate process you can poll and cancel, or pass a larger :timeout for a one-off"
     timeout))))

(defconst e-emacs-tools-run-elisp-blocking-functions
  '(sleep-for sit-for accept-process-output
    read-event read-char read-char-exclusive read-from-minibuffer
    call-process call-process-region shell-command shell-command-to-string
    process-file url-retrieve-synchronously
    e-harness-wait-batch e-work-await-batch)
  "Function symbols that block the single UI thread when called from run_elisp.
Waiting on these freezes Emacs; agents must poll across turns instead.  This is
a cheap, syntactic guard against the common mistakes, not a full sandbox: it
catches a literal top-level or nested call in the submitted forms, and does not
see blocking done indirectly through `apply', `funcall', or a helper.")

(defun e-emacs-tools--scan-blocking-call (forms)
  "Return the first blocking function symbol called literally in FORMS, or nil.
Walks the read forms looking for a call whose operator is in
`e-emacs-tools-run-elisp-blocking-functions'.  Quoted data is skipped so a
blocking symbol only used as a datum does not trip the guard."
  (let ((found nil))
    (cl-labels
        ((walk (form)
           (when (and (consp form) (not found))
             (let ((head (car form)))
               (cond
                ;; Do not descend into quoted data.
                ((memq head '(quote function)) nil)
                (t
                 (when (and (symbolp head)
                            (memq head e-emacs-tools-run-elisp-blocking-functions))
                   (setq found head))
                 (dolist (sub form)
                   (walk sub))))))))
      (dolist (form forms)
        (walk form)))
    found))

(defun e-emacs-tools--reject-run-elisp-blocking (symbol)
  "Signal that run_elisp code calls blocking SYMBOL."
  (signal
   'e-emacs-tools-run-elisp-blocking
   (list
    (format
     "run_elisp code calls `%s', which blocks the single UI Emacs thread and freezes the interface. Do not wait inside run_elisp: to observe async work (a subagent, a task-queue task, an elisp-job) poll its status across separate run_elisp calls and let the turn end between checks, or move blocking/long work to (e-actions-call 'elisp-job :run-batch ...). If a blocking call is genuinely required, run it as a top-level tool, not inside run_elisp."
     symbol))))

(defun e-emacs-tools--byte-prefix (text max-bytes)
  "Return a UTF-8 safe prefix of TEXT no larger than MAX-BYTES."
  (let ((bytes 0)
        (index 0)
        (limit (max 0 max-bytes)))
    (while (and (< index (length text))
                (let ((next-bytes
                       (string-bytes (substring text index (1+ index)))))
                  (when (<= (+ bytes next-bytes) limit)
                    (setq bytes (+ bytes next-bytes))
                    t)))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-emacs-tools--truncate-string-value (text)
  "Return TEXT bounded for safe `run_elisp' result printing."
  (let* ((max-bytes (max 0 e-emacs-tools-run-elisp-string-max-bytes))
         (original-bytes (string-bytes text)))
    (if (<= original-bytes max-bytes)
        text
      (let* ((preview (e-emacs-tools--byte-prefix text max-bytes))
             (shown-bytes (string-bytes preview)))
        (format
         "%s\n[run_elisp string truncated: showing first %d of %d bytes]"
         preview shown-bytes original-bytes)))))

(defun e-emacs-tools--result-preview-value (value depth seen)
  "Return a bounded preview copy of VALUE for `run_elisp' printing.
DEPTH limits recursive descent.  SEEN tracks container identity to avoid cycles."
  (cond
   ((stringp value)
    (e-emacs-tools--truncate-string-value value))
   ((or (not value) (symbolp value) (numberp value) (characterp value))
    value)
   ((<= depth 0)
    '...)
   ((or (consp value) (vectorp value) (hash-table-p value))
    (if (gethash value seen)
        "#<cycle>"
      (puthash value t seen)
      (cond
       ((consp value)
        (let ((tail value)
              (items nil)
              (count 0)
              (limit (max 0 e-emacs-tools-run-elisp-print-length)))
          (while (and (consp tail) (< count limit))
            (push (e-emacs-tools--result-preview-value
                   (car tail) (1- depth) seen)
                  items)
            (setq tail (cdr tail))
            (setq count (1+ count)))
          (cond
           ((consp tail)
            (append (nreverse items) '(...)))
           ((null tail)
            (nreverse items))
           (t
            (nconc (nreverse items)
                   (e-emacs-tools--result-preview-value
                    tail (1- depth) seen))))))
       ((vectorp value)
        (let* ((limit (max 0 e-emacs-tools-run-elisp-print-length))
               (count (min (length value) limit))
               (items nil))
          (dotimes (index count)
            (push (e-emacs-tools--result-preview-value
                   (aref value index) (1- depth) seen)
                  items))
          (apply #'vector
                 (nreverse
                  (if (< count (length value))
                      (cons '... items)
                    items)))))
       ((hash-table-p value)
        (let ((pairs nil)
              (count 0)
              (limit (max 0 e-emacs-tools-run-elisp-print-length))
              (truncated nil))
          (catch 'done
            (maphash
             (lambda (key entry)
               (if (>= count limit)
                   (progn
                     (setq truncated t)
                     (throw 'done nil))
                 (push
                  (cons
                   (e-emacs-tools--result-preview-value key (1- depth) seen)
                   (e-emacs-tools--result-preview-value entry (1- depth) seen))
                  pairs)
                 (setq count (1+ count))))
             value))
          (list :hash-table-preview (nreverse pairs)
                :truncated truncated
                :test (hash-table-test value)))))))
   (t value)))

(defun e-emacs-tools--truncate-printed-result (text)
  "Return TEXT capped to `e-emacs-tools-run-elisp-result-max-bytes'."
  (let* ((max-bytes (max 0 e-emacs-tools-run-elisp-result-max-bytes))
         (original-bytes (string-bytes text)))
    (if (<= original-bytes max-bytes)
        text
      (let* ((preview (e-emacs-tools--byte-prefix text max-bytes))
             (shown-bytes (string-bytes preview)))
        (format
         "%s\n\n[run_elisp result truncated: showing first %d of %d bytes. Return a smaller scalar or use elisp-job for large inspection.]"
         preview shown-bytes original-bytes)))))

(defun e-emacs-tools--bounded-result-string (value)
  "Return a bounded printed representation of VALUE for `run_elisp'."
  (let* ((preview
          (e-emacs-tools--result-preview-value
           value
           (max 0 e-emacs-tools-run-elisp-print-level)
           (make-hash-table :test 'eq)))
         (print-length (max 0 e-emacs-tools-run-elisp-print-length))
         (print-level (max 0 e-emacs-tools-run-elisp-print-level)))
    (e-emacs-tools--truncate-printed-result
     (prin1-to-string preview))))

(defun e-emacs-tools-register-list-buffers (registry)
  "Register a tool that lists live Emacs buffers in REGISTRY."
  (e-tools-register
   registry
   :name "list_buffers"
   :description "Return live Emacs buffer names and metadata."
   :parameters '(:type "object"
                 :properties (:visible_only (:type "boolean")))
   :handler (lambda (arguments)
              (list :buffers
                    (e-emacs-tools-buffer-metadata-list
                     (plist-get arguments :visible_only))))))

(defun e-emacs-tools-register-save-buffer (registry)
  "Register a tool to save file-backed Emacs buffers in REGISTRY."
  (e-tools-register
   registry
   :name "save_buffer"
   :description "Save a named file-backed Emacs buffer using its existing file path."
   :parameters '(:type "object"
                 :properties (:name (:type "string"))
                 :required ["name"])
   :handler
   (lambda (arguments)
     (let ((name (e-emacs-tools--argument-string arguments :name)))
       (with-current-buffer (e-emacs-tools--buffer name)
         (unless buffer-file-name
           (signal 'e-emacs-tools-save-invalid
                   (list (format "Buffer %s does not visit a file" name))))
         ;; Save non-interactively: never prompt the user to choose a coding
         ;; system.  With the selector disabled `save-buffer' uses the buffer's
         ;; own coding directly instead of asking.
         (let ((select-safe-coding-system-function nil))
           (save-buffer))
         (list :name name
               :file buffer-file-name
               :saved t))))))

(defun e-emacs-tools-register-run-elisp (registry)
  "Register a tool to evaluate explicit Emacs Lisp in REGISTRY."
  (e-tools-register
   registry
   :name "run_elisp"
   :description
   (concat
    "Evaluate explicit Emacs Lisp in Emacs and return the printed result. "
    "When this tool runs in an active e tool context, code may call currently "
    "active tools with e-tools-call/e-tools-call! and active capability actions "
    "with e-actions-call. Nested tool calls are for cheap composition; long or "
    "async-backed tools must run as top-level tools so their Work lifecycle can "
    "stream progress and settle without blocking this eval. "
    "This runs in the live UI Emacs, so anything that "
    "blocks it -- loading or byte-compiling large Elisp, recursive directory "
    "scans, long computations -- freezes the interface while it runs. "
    "Do not wait inside run_elisp: a literal call to a blocking primitive "
    "(sleep-for, sit-for, accept-process-output, read-event, a synchronous "
    "process/URL call, e-harness-wait-batch, e-work-await-batch) is rejected. "
    "To observe async work -- a subagent, a task-queue task, an elisp-job -- "
    "poll its status across separate run_elisp calls and let the turn end "
    "between checks, rather than sleeping for it. "
    "Evaluation is time-capped; pass :timeout seconds to raise or lower the cap "
    "for one call. Move expensive validation or byte-compilation to "
    "(e-actions-call 'elisp-job :run-batch ...), which runs in a separate "
    "process that cannot freeze the UI.")
   :parameters '(:type "object"
                 :properties
                 (:code (:type "string")
                  :timeout
                  (:type "number"
                   :description "Seconds before evaluation is aborted. Overrides the default cap; a value <= 0 disables it for this call. Aborts evals that block or wait; it cannot stop a tight compute loop, so use elisp-job for that."))
                 :required ["code"])
   :handler
   (lambda (arguments)
     (let* ((code (e-emacs-tools--argument-string arguments :code))
            (forms (e-emacs-tools--read-forms code))
            (blocking (e-emacs-tools--scan-blocking-call forms))
            (timeout (e-emacs-tools--run-elisp-timeout arguments))
            ;; Never let agent code pop the interactive debugger.  This runs
            ;; with no human to dismiss it, so an error (or agent code that
            ;; sets `debug-on-error') would otherwise enter `debug', whose own
            ;; buffer setup can re-signal and recurse, pinning Emacs at 100%
            ;; CPU.  `inhibit-debugger' is the hard backstop honored by
            ;; `debug' regardless of the debug-on-* flags.
            (inhibit-debugger t)
            (debug-on-error nil)
            (debug-on-signal nil)
            (debug-on-quit nil)
            (eval-expression-debug-on-error nil)
            result)
       ;; Cheap syntactic guard: reject a literal call to a common blocking
       ;; wait primitive before evaluating.  Waiting inside run_elisp freezes
       ;; the single UI thread; agents must poll async work across turns.  This
       ;; catches the frequent mistakes only, not blocking hidden behind apply
       ;; or a helper -- the timeout below remains the backstop for those.
       (when blocking
         (e-emacs-tools--reject-run-elisp-blocking blocking))
       ;; `with-timeout' arms a timer that throws only when execution reaches a
       ;; wait point.  It aborts an eval that blocks or waits (sleep-for, I/O,
       ;; process waits) but cannot preempt a tight compute loop such as
       ;; (while t); the guaranteed kill is elisp-job's separate process, which
       ;; the timeout message points agents toward.  The timeout is also the
       ;; backstop for a slow `load'/`require': there is no separate load guard.
       (if timeout
           (with-timeout (timeout
                          (e-emacs-tools--reject-run-elisp-timeout timeout))
             (dolist (form forms)
               (setq result (eval form t))))
         (dolist (form forms)
           (setq result (eval form t))))
       (list :result (e-emacs-tools--bounded-result-string result))))))

(defun e-emacs-tools-register-elisp-eval (registry)
  "Register explicit elisp evaluation tools in REGISTRY."
  (e-emacs-tools-register-run-elisp registry)
  registry)

(provide 'e-emacs-tools)

;;; e-emacs-tools.el ends here
