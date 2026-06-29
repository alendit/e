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
(require 'e-resources)
(require 'e-tools)

(define-error 'e-emacs-tools-buffer-missing "Emacs buffer is missing")
(define-error 'e-emacs-tools-edit-invalid "Emacs buffer edit is invalid")
(define-error 'e-emacs-tools-save-invalid "Emacs buffer cannot be saved")
(define-error 'e-emacs-tools-blocking-elisp-load
  "Blocking Elisp loading is not allowed in interactive run_elisp")

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
    (uri pattern limit case-sensitive)
  "List live buffer resources under parsed URI with PATTERN and LIMIT."
  (let* ((actual-limit (e-emacs-tools--discovery-limit limit))
         (buffers (e-emacs-tools--buffer-resource-candidates
                   uri
                   pattern
                   case-sensitive))
         (truncated (> (length buffers) actual-limit)))
    (list :resources
          (vconcat
           (mapcar
            (lambda (buffer)
              (let ((name (buffer-name buffer)))
                (list :uri (concat "buffer://" name)
                      :name name
                      :kind 'buffer
                      :metadata (e-emacs-tools--buffer-metadata buffer))))
            (seq-take buffers actual-limit)))
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
         matches)
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
   :handler #'e-emacs-tools--buffer-glob-resource))

(defun e-emacs-tools--buffer-search-method ()
  "Return a buffer search resource method."
  (e-resource-method-create
   :scheme "buffer"
   :operation e-operation-search
   :description "Live Emacs buffer content searched natively."
   :uri-patterns '("buffer://<optional-buffer-name-prefix>")
   :handler #'e-emacs-tools--buffer-search-resource))

(defun e-emacs-tools-register-buffer-read-resource (registry)
  "Register read-only buffer resource methods in REGISTRY."
  (dolist (method (list (e-emacs-tools--buffer-read-method)
                        (e-emacs-tools--buffer-glob-method)
                        (e-emacs-tools--buffer-search-method)))
    (e-resources-register registry method)))

(defun e-emacs-tools-register-buffer-resource (registry)
  "Register buffer resource methods in REGISTRY."
  (dolist (method (list (e-emacs-tools--buffer-read-method)
                        (e-emacs-tools--buffer-write-method)
                        (e-emacs-tools--buffer-edit-method)
                        (e-emacs-tools--buffer-glob-method)
                        (e-emacs-tools--buffer-search-method)))
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

(defun e-emacs-tools--interactive-run-elisp-context-p ()
  "Return non-nil when `run_elisp' is evaluating in an interactive context."
  (let ((context (e-tools-current-context)))
    (or (e-tools--interactive-context-p context)
        (and (plist-get context :session-id)
             (plist-get context :turn-id)))))

(defun e-emacs-tools--reject-blocking-elisp-load (primitive)
  "Signal that PRIMITIVE cannot run from interactive `run_elisp'."
  (signal
   'e-emacs-tools-blocking-elisp-load
   (list
    (format
     "%s is blocking in interactive run_elisp; inspect external Elisp with resource/file tools or use project-local actions for trusted activation"
     primitive))))

(defmacro e-emacs-tools--with-run-elisp-load-guard (&rest body)
  "Run BODY while rejecting blocking Elisp loading in interactive contexts."
  (declare (indent 0) (debug t))
  `(if (not (e-emacs-tools--interactive-run-elisp-context-p))
       (progn ,@body)
     (let ((original-require (symbol-function 'require)))
       (cl-letf (((symbol-function 'load)
                  (lambda (&rest _args)
                    (e-emacs-tools--reject-blocking-elisp-load 'load)))
                 ((symbol-function 'load-file)
                  (lambda (&rest _args)
                    (e-emacs-tools--reject-blocking-elisp-load 'load-file)))
                 ((symbol-function 'require)
                  (lambda (feature &optional filename noerror)
                    (if (featurep feature)
                        (funcall original-require feature filename noerror)
                      (e-emacs-tools--reject-blocking-elisp-load 'require))))
                 ((symbol-function 'byte-compile-file)
                  (lambda (&rest _args)
                    (e-emacs-tools--reject-blocking-elisp-load
                     'byte-compile-file)))
                 ((symbol-function 'directory-files-recursively)
                  (lambda (&rest _args)
                    (e-emacs-tools--reject-blocking-elisp-load
                     'directory-files-recursively))))
         ,@body))))

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
    "with e-actions-call. Inspect external Elisp with resource/file tools; do "
    "not load, require, byte-compile, or recursively scan external Elisp here "
    "during an interactive turn. Use elisp_job operation=run-batch for "
    "expensive validation or byte-compilation that must not freeze the live "
    "UI Emacs.")
   :parameters '(:type "object"
                 :properties (:code (:type "string"))
                 :required ["code"])
   :handler
   (lambda (arguments)
     (let* ((code (e-emacs-tools--argument-string arguments :code))
            (forms (e-emacs-tools--read-forms code))
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
       (e-emacs-tools--with-run-elisp-load-guard
         (dolist (form forms)
           (setq result (eval form t))))
       (list :result (prin1-to-string result))))))

(defun e-emacs-tools-register-elisp-eval (registry)
  "Register explicit elisp evaluation tools in REGISTRY."
  (e-emacs-tools-register-run-elisp registry)
  registry)

(provide 'e-emacs-tools)

;;; e-emacs-tools.el ends here
