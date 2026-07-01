;;; e-emacs-tools-test.el --- Tests for harmless Emacs tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for low-risk concrete tools.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'e)
(require 'e-actions)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-elisp-job)
(require 'e-emacs-capabilities)
(require 'e-emacs-tools)
(require 'e-harness)
(require 'e-resources)
(require 'e-tools)

(defun e-emacs-tools-test--resource-tools (&optional read-only)
  "Return resource-backed buffer tools.
When READ-ONLY is non-nil, buffer resources only support reads."
  (let ((resources (e-resources-registry-create))
        (tools (e-tools-registry-create)))
    (if read-only
        (e-emacs-tools-register-buffer-read-resource resources)
      (e-emacs-tools-register-buffer-resource resources))
    (e-harness--register-resource-tools tools resources)
    tools))

(defun e-emacs-tools-test--run-elisp-result (registry code &optional context)
  "Run CODE through REGISTRY's `run_elisp' tool and return the result."
  (let (result)
    (e-tools-start
     registry
     (list :id "call-1"
           :name "run_elisp"
           :arguments (list :code code))
     :context context
     :on-done (lambda (value) (setq result value)))
    (while (not result)
      (accept-process-output nil 0.01))
    result))

(defun e-emacs-tools-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(ert-deftest e-emacs-tools-test-list-buffers-reports-buffer-metadata ()
  "The list-buffers tool returns live buffer metadata."
  (let ((registry (e-tools-registry-create))
        (buffer (generate-new-buffer " *e-test-list*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq buffer-file-name "/tmp/e-test-list.txt")
            (set-buffer-modified-p t))
          (e-emacs-tools-register-list-buffers registry)
          (let* ((result (e-tools-execute
                          registry
                          '(:id "call-1" :name "list_buffers" :arguments nil)))
                 (buffers (plist-get (plist-get result :content) :buffers))
                 (metadata (seq-find
                            (lambda (item)
                              (equal (plist-get item :name)
                                     (buffer-name buffer)))
                            buffers)))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (plist-get metadata :file-backed) t))
            (should (equal (plist-get metadata :modified) t))))
      (kill-buffer buffer))))

(ert-deftest e-emacs-tools-test-read-buffer-returns-content ()
  "The read tool reads live buffer text through buffer:// URIs."
  (let ((registry (e-emacs-tools-test--resource-tools t))
        (buffer (generate-new-buffer " *e-test-read*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "abcdef"))
          (should
           (equal (plist-get
                   (e-tools-execute
                    registry
                    `(:id "call-1"
                      :name "read"
                      :arguments (:uri ,(concat "buffer://" (buffer-name buffer))
                                  :range (:unit "offset" :start 2 :limit 3))))
                   :content)
                  '(:name " *e-test-read*" :content "bcd" :start 2 :end 4))))
      (kill-buffer buffer))))

(ert-deftest e-emacs-tools-test-glob-buffer-resources ()
  "The glob tool lists live buffer:// resources by prefix and pattern."
  (let ((registry (e-emacs-tools-test--resource-tools t))
        (alpha (generate-new-buffer "e-test-glob-alpha"))
        (beta (generate-new-buffer "e-test-glob-beta")))
    (unwind-protect
        (let* ((content (plist-get
                         (e-tools-execute
                          registry
                          `(:id "call-1"
                            :name "glob"
                            :arguments (:uri "buffer://e-test-glob-"
                                        :pattern "*alpha*"
                                        :limit 5)))
                         :content))
               (resources (append (plist-get content :resources) nil)))
          (should (equal (plist-get content :truncated) nil))
          (should (equal (mapcar (lambda (entry)
                                   (plist-get entry :uri))
                                 resources)
                         (list (concat "buffer://" (buffer-name alpha)))))
          (should (equal (plist-get (car resources) :name)
                         (buffer-name alpha))))
      (kill-buffer alpha)
      (kill-buffer beta))))

(ert-deftest e-emacs-tools-test-search-buffer-resources ()
  "The search tool searches live buffer:// resources natively."
  (let ((registry (e-emacs-tools-test--resource-tools t))
        (alpha (generate-new-buffer "e-test-search-alpha"))
        (beta (generate-new-buffer "e-test-search-beta")))
    (unwind-protect
        (progn
          (with-current-buffer alpha
            (insert "Alpha needle\nbeta\n"))
          (with-current-buffer beta
            (insert "alpha NEEDLE\nneedle again\n"))
          (should
           (equal (plist-get
                   (e-tools-execute
                    registry
                    `(:id "call-1"
                      :name "search"
                      :arguments (:uri "buffer://e-test-search-"
                                  :query "needle"
                                  :glob "*alpha*"
                                  :limit 5)))
                   :content)
                  `(:matches [(:uri ,(concat "buffer://" (buffer-name alpha))
                                :line 1
                                :column 7
                                :text "Alpha needle")]
                    :truncated nil)))
          (should
           (equal (plist-get
                   (e-tools-execute
                    registry
                    `(:id "call-2"
                      :name "search"
                      :arguments (:uri "buffer://e-test-search-"
                                  :query "needle again"
                                  :glob "*beta*"
                                  :case-sensitive t
                                  :limit 5)))
                   :content)
                  `(:matches [(:uri ,(concat "buffer://" (buffer-name beta))
                                :line 2
                                :column 1
                                :text "needle again")]
                    :truncated nil))))
      (kill-buffer alpha)
      (kill-buffer beta))))

(ert-deftest e-emacs-tools-test-write-buffer-mutates-without-saving ()
  "The write tool replaces live buffer text without saving."
  (let ((registry (e-emacs-tools-test--resource-tools))
        (buffer (generate-new-buffer " *e-test-write*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq buffer-file-name "/tmp/e-test-write.txt")
            (insert "old"))
          (let ((result (e-tools-execute
                         registry
                         `(:id "call-1"
                           :name "write"
                           :arguments (:uri ,(concat "buffer://" (buffer-name buffer))
                                             :content "new text")))))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (with-current-buffer buffer (buffer-string))
                           "new text"))
            (should (buffer-modified-p buffer))
            (let ((content (plist-get result :content)))
              (should-not (plist-get content :saved))
              (should (plist-get content :modified))
              (should (plist-get content :file-backed))
              (should (equal (plist-get content :file)
                             "/tmp/e-test-write.txt")))))
      (kill-buffer buffer))))

(ert-deftest e-emacs-tools-test-write-buffer-creates-missing-live-buffer ()
  "The write tool creates missing live buffers and inserts complete content."
  (let* ((registry (e-emacs-tools-test--resource-tools))
         (name (generate-new-buffer-name " *e-test-write-created*"))
         (buffer nil))
    (unwind-protect
        (progn
          (should-not (get-buffer name))
          (let ((result (e-tools-execute
                         registry
                         `(:id "call-1"
                           :name "write"
                           :arguments (:uri ,(concat "buffer://" name)
                                             :content "created text")))))
            (setq buffer (get-buffer name))
            (should (equal (plist-get result :status) 'ok))
            (should (buffer-live-p buffer))
            (should (equal (with-current-buffer buffer (buffer-string))
                           "created text"))
            (should-not (with-current-buffer buffer buffer-file-name))
            (let ((content (plist-get result :content)))
              (should-not (plist-get content :saved))
              (should (plist-get content :modified))
              (should-not (plist-get content :file-backed))
              (should-not (plist-get content :file)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-emacs-tools-test-edit-buffer-replaces-unique-match ()
  "The edit tool replaces exactly one old-text match in a live buffer."
  (let ((registry (e-emacs-tools-test--resource-tools))
        (buffer (generate-new-buffer " *e-test-edit*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "alpha beta gamma"))
          (let ((result (e-tools-execute
                         registry
                         `(:id "call-1"
                           :name "edit"
                           :arguments (:uri ,(concat "buffer://" (buffer-name buffer))
                                      :edits ((:oldText "beta"
                                               :newText "delta")))))))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (with-current-buffer buffer (buffer-string))
                           "alpha delta gamma"))
            (let ((content (plist-get result :content)))
              (should-not (plist-get content :saved))
              (should (plist-get content :modified))
              (should-not (plist-get content :file-backed))
              (should-not (plist-get content :file)))))
      (kill-buffer buffer))))

(ert-deftest e-emacs-tools-test-edit-buffer-rejects-invalid-replacements ()
  "The edit tool rejects missing, duplicate, and no-op buffer replacements."
  (let ((registry (e-emacs-tools-test--resource-tools))
        (buffer (generate-new-buffer " *e-test-edit-errors*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "alpha beta beta"))
          (should (equal (plist-get
                          (e-tools-execute
                           registry
                           `(:id "call-1"
                             :name "edit"
                             :arguments (:uri ,(concat "buffer://" (buffer-name buffer))
                                        :edits ((:oldText "missing"
                                                 :newText "x")))))
                          :status)
                         'error))
          (should (equal (plist-get
                          (e-tools-execute
                           registry
                           `(:id "call-2"
                             :name "edit"
                             :arguments (:uri ,(concat "buffer://" (buffer-name buffer))
                                        :edits ((:oldText "beta"
                                                 :newText "x")))))
                          :status)
                         'error))
          (should (equal (plist-get
                          (e-tools-execute
                           registry
                           `(:id "call-3"
                             :name "edit"
                             :arguments (:uri ,(concat "buffer://" (buffer-name buffer))
                                        :edits ((:oldText "alpha"
                                                 :newText "alpha")))))
                          :status)
                         'error)))
      (kill-buffer buffer))))

(ert-deftest e-emacs-tools-test-edit-buffer-does-not-create-missing-buffer ()
  "The edit tool stays strict and does not create missing buffers."
  (let* ((registry (e-emacs-tools-test--resource-tools))
         (name (generate-new-buffer-name " *e-test-edit-missing*")))
    (let ((result (e-tools-execute
                   registry
                   `(:id "call-1"
                     :name "edit"
                     :arguments (:uri ,(concat "buffer://" name)
                                :edits ((:oldText "old"
                                         :newText "new")))))))
      (should (equal (plist-get result :status) 'error))
      (should (string-match-p "No buffer named"
                              (plist-get result :content)))
      (should-not (get-buffer name)))))

(ert-deftest e-emacs-tools-test-save-buffer-persists-file-backed-buffer ()
  "The save-buffer tool saves a file-backed buffer."
  (let ((registry (e-tools-registry-create))
        (file (make-temp-file "e-save-buffer-"))
        buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (erase-buffer)
            (insert "saved content"))
          (e-emacs-tools-register-save-buffer registry)
          (let ((result (e-tools-execute
                         registry
                         `(:id "call-1"
                           :name "save_buffer"
                           :arguments (:name ,(buffer-name buffer))))))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))
                           "saved content"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest e-emacs-tools-test-save-buffer-errors-for-non-file-buffer ()
  "The save-buffer tool fails clearly for non-file buffers."
  (let ((registry (e-tools-registry-create))
        (buffer (generate-new-buffer " *e-test-save-error*")))
    (unwind-protect
        (progn
          (e-emacs-tools-register-save-buffer registry)
          (let ((result (e-tools-execute
                         registry
                         `(:id "call-1"
                           :name "save_buffer"
                           :arguments (:name ,(buffer-name buffer))))))
            (should (equal (plist-get result :status) 'error))
            (should (string-match-p "does not visit a file"
                                    (plist-get result :content)))))
      (kill-buffer buffer))))

(ert-deftest e-emacs-tools-test-run-elisp-returns-value-and-errors ()
  "The run-elisp tool evaluates forms and surfaces errors."
  (let ((registry (e-tools-registry-create)))
    (e-emacs-tools-register-run-elisp registry)
    (should (equal (plist-get
                    (e-tools-execute
                     registry
                     '(:id "call-1"
                       :name "run_elisp"
                       :arguments (:code "(+ 1 2)")))
                    :content)
                   '(:result "3")))
    (should (equal (plist-get
                    (e-tools-execute
                     registry
                     '(:id "call-2"
                       :name "run_elisp"
                       :arguments (:code "(error \"boom\")")))
                    :status)
                   'error))))

(ert-deftest e-emacs-tools-test-run-elisp-bounds-sequence-results ()
  "run_elisp bounds sequence printing before returning tool output."
  (let ((registry (e-tools-registry-create))
        (e-emacs-tools-run-elisp-print-length 3)
        (e-emacs-tools-run-elisp-print-level 4)
        (e-emacs-tools-run-elisp-result-max-bytes 1000))
    (e-emacs-tools-register-run-elisp registry)
    (should
     (equal (plist-get
             (e-tools-execute
              registry
              '(:id "call-1"
                :name "run_elisp"
                :arguments (:code "(number-sequence 1 10)")))
             :content)
            '(:result "(1 2 3 ...)")))))

(ert-deftest e-emacs-tools-test-run-elisp-bounds-string-results ()
  "run_elisp bounds large string results before printing them."
  (let ((registry (e-tools-registry-create))
        (e-emacs-tools-run-elisp-string-max-bytes 5)
        (e-emacs-tools-run-elisp-result-max-bytes 1000))
    (e-emacs-tools-register-run-elisp registry)
    (let* ((result
            (e-tools-execute
             registry
             '(:id "call-1"
               :name "run_elisp"
               :arguments (:code "(make-string 20 ?a)"))))
           (printed (plist-get (plist-get result :content) :result)))
      (should (eq (plist-get result :status) 'ok))
      (should (string-match-p "aaaaa" printed))
      (should-not (string-match-p "aaaaaaaaaaaaaaaaaaaa" printed))
      (should (string-match-p "run_elisp string truncated" printed)))))

(ert-deftest e-emacs-tools-test-run-elisp-bounds-nested-string-results ()
  "run_elisp bounds large strings inside shallow containers."
  (let ((registry (e-tools-registry-create))
        (e-emacs-tools-run-elisp-string-max-bytes 5)
        (e-emacs-tools-run-elisp-print-length 10)
        (e-emacs-tools-run-elisp-print-level 4)
        (e-emacs-tools-run-elisp-result-max-bytes 1000))
    (e-emacs-tools-register-run-elisp registry)
    (let* ((result
            (e-tools-execute
             registry
             '(:id "call-1"
               :name "run_elisp"
               :arguments (:code "(list (make-string 20 ?a))"))))
           (printed (plist-get (plist-get result :content) :result)))
      (should (eq (plist-get result :status) 'ok))
      (should (string-match-p "aaaaa" printed))
      (should-not (string-match-p "aaaaaaaaaaaaaaaaaaaa" printed))
      (should (string-match-p "run_elisp string truncated" printed)))))

(ert-deftest e-emacs-tools-test-run-elisp-caps-final-printed-result ()
  "run_elisp caps the final printed text after structural previewing."
  (let ((registry (e-tools-registry-create))
        (e-emacs-tools-run-elisp-print-length 100)
        (e-emacs-tools-run-elisp-print-level 4)
        (e-emacs-tools-run-elisp-string-max-bytes 100)
        (e-emacs-tools-run-elisp-result-max-bytes 40))
    (e-emacs-tools-register-run-elisp registry)
    (let* ((result
            (e-tools-execute
             registry
             '(:id "call-1"
               :name "run_elisp"
               :arguments (:code "(make-list 50 'tool)"))))
           (printed (plist-get (plist-get result :content) :result)))
      (should (eq (plist-get result :status) 'ok))
      (should (<= (string-bytes printed) 200))
      (should (string-match-p "run_elisp result truncated" printed)))))

(ert-deftest e-emacs-tools-test-run-elisp-rejects-blocking-loads-interactively ()
  "Interactive run_elisp rejects blocking Elisp loading primitives."
  (let ((registry (e-tools-registry-create)))
    (e-emacs-tools-register-run-elisp registry)
    (dolist (case '(("load" "(load \"e-test-missing-feature\")")
                    ("load-file" "(load-file \"/tmp/e-test-missing.el\")")
                    ("require" "(require 'e-test-missing-feature)")
                    ("byte-compile-file"
                     "(byte-compile-file \"/tmp/e-test-missing.el\")")
                    ("directory-files-recursively"
                     "(directory-files-recursively default-directory \"\\\\.el\\\\'\")")))
      (pcase-let ((`(,primitive ,code) case))
        (let ((result (e-emacs-tools-test--run-elisp-result
                       registry
                       code
                       '(:interactive t))))
          (should (eq (plist-get result :status) 'error))
          (should (string-match-p primitive
                                  (format "%s" (plist-get result :content))))
          (should (string-match-p
                   "resource/file tools"
                   (format "%s" (plist-get result :content))))
          (should (string-match-p
                   "e-actions-call"
                   (format "%s" (plist-get result :content))))
          (should (string-match-p
                   "elisp-job"
                   (format "%s" (plist-get result :content)))))))))

(defun e-emacs-tools-test--run-elisp-arguments (registry arguments &optional context)
  "Run ARGUMENTS through REGISTRY's `run_elisp' tool and return the result."
  (let (result)
    (e-tools-start
     registry
     (list :id "call-1"
           :name "run_elisp"
           :arguments arguments)
     :context context
     :on-done (lambda (value) (setq result value)))
    (while (not result)
      (accept-process-output nil 0.01))
    result))

(ert-deftest e-emacs-tools-test-run-elisp-timeout-resolves-effective-value ()
  "The run_elisp timeout resolver honors overrides and the default cap."
  (let ((e-emacs-tools-run-elisp-default-timeout 10))
    ;; No override falls back to the default cap.
    (should (equal (e-emacs-tools--run-elisp-timeout '()) 10))
    ;; A positive override wins over the default.
    (should (equal (e-emacs-tools--run-elisp-timeout '(:timeout 2)) 2))
    ;; A non-positive override disables the cap for that call.
    (should (null (e-emacs-tools--run-elisp-timeout '(:timeout 0))))
    (should (null (e-emacs-tools--run-elisp-timeout '(:timeout -1))))
    ;; A nil default with no override runs uncapped.
    (let ((e-emacs-tools-run-elisp-default-timeout nil))
      (should (null (e-emacs-tools--run-elisp-timeout '()))))
    ;; A non-numeric timeout is rejected.
    (should-error (e-emacs-tools--run-elisp-timeout '(:timeout "soon"))
                  :type 'wrong-type-argument)))

(ert-deftest e-emacs-tools-test-run-elisp-default-timeout-aborts-runaway ()
  "The default cap aborts a long-running eval and nudges toward elisp-job.
The eval blocks in `sleep-for'; `with-timeout' only fires at such a wait
point, so the test loop must yield rather than spin the CPU."
  (let ((registry (e-tools-registry-create))
        (e-emacs-tools-run-elisp-default-timeout 0.1))
    (e-emacs-tools-register-run-elisp registry)
    (let ((result (e-emacs-tools-test--run-elisp-arguments
                   registry
                   '(:code "(sleep-for 30)"))))
      (should (eq (plist-get result :status) 'error))
      (let ((content (format "%s" (plist-get result :content))))
        (should (string-match-p "aborted after" content))
        (should (string-match-p "elisp-job" content))
        (should (string-match-p ":timeout" content))))))

(ert-deftest e-emacs-tools-test-run-elisp-timeout-argument-overrides-default ()
  "A per-call :timeout lowers the cap below a generous default."
  (let ((registry (e-tools-registry-create))
        (e-emacs-tools-run-elisp-default-timeout 60))
    (e-emacs-tools-register-run-elisp registry)
    (let ((result (e-emacs-tools-test--run-elisp-arguments
                   registry
                   '(:code "(sleep-for 30)" :timeout 0.1))))
      (should (eq (plist-get result :status) 'error))
      (should (string-match-p
               "aborted after 0.1 seconds"
               (format "%s" (plist-get result :content)))))))

(ert-deftest e-emacs-tools-test-run-elisp-timeout-allows-quick-eval ()
  "A quick evaluation returns its value even under a tight cap."
  (let ((registry (e-tools-registry-create))
        (e-emacs-tools-run-elisp-default-timeout 5))
    (e-emacs-tools-register-run-elisp registry)
    (let ((result (e-emacs-tools-test--run-elisp-arguments
                   registry
                   '(:code "(+ 1 2)" :timeout 0.5))))
      (should (eq (plist-get result :status) 'ok))
      (should (equal (plist-get result :content) '(:result "3"))))))

(ert-deftest e-emacs-tools-test-run-elisp-bypass-permits-trusted-load ()
  "Interactive run_elisp permits a load wrapped in the trusted-load bypass.
Trusted runtime code (e.g. project-local layer resolution reached through
`e-actions-call') binds `e-emacs-tools-bypass-run-elisp-load-guard' around its
own loads, so the guard must not reject those even though it rejects bare
agent-authored loads in the same interactive context."
  (let* ((registry (e-tools-registry-create))
         (file (make-temp-file "e-run-elisp-bypass-load-" nil ".el")))
    (unwind-protect
        (progn
          (write-region "(setq e-emacs-tools-test--bypass-loaded t)"
                        nil file nil 'silent)
          (e-emacs-tools-register-run-elisp registry)
          (let ((result (e-emacs-tools-test--run-elisp-result
                         registry
                         (format
                          (concat "(let ((e-emacs-tools-bypass-run-elisp-load-guard t))"
                                  " (load-file %S)) e-emacs-tools-test--bypass-loaded")
                          file)
                         '(:interactive t))))
            (should (eq (plist-get result :status) 'ok))
            (should (equal (plist-get result :content)
                           '(:result "t")))))
      (when (boundp 'e-emacs-tools-test--bypass-loaded)
        (makunbound 'e-emacs-tools-test--bypass-loaded))
      (delete-file file))))

(ert-deftest e-emacs-tools-test-run-elisp-allows-batch-loading ()
  "Batch run_elisp keeps existing direct Elisp loading semantics."
  (let* ((registry (e-tools-registry-create))
         (file (make-temp-file "e-run-elisp-batch-load-" nil ".el")))
    (unwind-protect
        (progn
          (write-region "(setq e-emacs-tools-test--batch-loaded t)"
                        nil file nil 'silent)
          (e-emacs-tools-register-run-elisp registry)
          (let ((result (e-emacs-tools-test--run-elisp-result
                         registry
                         (format "(load-file %S) e-emacs-tools-test--batch-loaded"
                                 file))))
            (should (eq (plist-get result :status) 'ok))
            (should (equal (plist-get result :content)
                           '(:result "t")))))
      (when (boundp 'e-emacs-tools-test--batch-loaded)
        (makunbound 'e-emacs-tools-test--batch-loaded))
      (delete-file file))))

(ert-deftest e-emacs-tools-test-run-elisp-allows-loaded-require-interactively ()
  "Interactive run_elisp allows `require' for features already loaded."
  (let ((registry (e-tools-registry-create)))
    (e-emacs-tools-register-run-elisp registry)
    (let ((result (e-emacs-tools-test--run-elisp-result
                   registry
                   "(require 'cl-lib)"
                   '(:interactive t))))
      (should (eq (plist-get result :status) 'ok))
      (should (equal (plist-get result :content)
                     '(:result "cl-lib"))))))

(ert-deftest e-emacs-tools-test-run-elisp-description-names-tool-chaining-api ()
  "run_elisp tells models about context-bound tool/action calls."
  (let ((registry (e-tools-registry-create)))
    (e-emacs-tools-register-run-elisp registry)
    (let* ((definition (car (e-tools-definitions registry)))
           (description (plist-get definition :description)))
      (should (string-match-p "e-tools-call" description))
      (should (string-match-p "e-tools-call!" description))
      (should (string-match-p "e-actions-call" description))
      (should (string-match-p "active tools" description))
      (should (string-match-p "active capability actions" description))
      (should (string-match-p "Inspect external Elisp with resource/file tools"
                              description))
      (should (string-match-p "do not load, require, byte-compile"
                              description))
      (should (string-match-p "e-actions-call" description))
      (should (string-match-p "elisp-job" description))
      (should (string-match-p "run-batch" description)))))

(defun e-emacs-tools-test--elisp-job-harness ()
  "Return a harness/session pair with Elisp job actions active."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-elisp-job-capability-create))
    (e-harness-create-session harness :id "session-1")
    (list :harness harness :session-id "session-1")))

(ert-deftest e-emacs-tools-test-elisp-job-action-starts-asynchronously ()
  "elisp-job run-batch action returns before worker completion."
  (pcase-let* ((context (e-emacs-tools-test--elisp-job-harness))
               (`(:harness ,harness :session-id ,session-id) context)
               (start (e-actions-call
                       'elisp-job
                       :run-batch
                       '(:code "(progn (sleep-for 0.1) (princ \"done\"))")
                       (list :harness harness :session-id session-id)))
               (job-id (plist-get start :job_id)))
    (should (stringp job-id))
    (should (eq (plist-get start :status) 'running))
    (should (e-emacs-tools-test--wait-until
             (lambda ()
               (eq (plist-get (e-elisp-job-result job-id) :status) 'ok))
             2.0))
    (let ((result (e-elisp-job-result job-id)))
      (should (eq (plist-get result :status) 'ok))
      (should (string-match-p "done" (plist-get result :content))))))

(ert-deftest e-emacs-tools-test-elisp-job-action-exposes-progress-by-status ()
  "elisp-job status action exposes bounded worker output while running."
  (pcase-let* ((context (e-emacs-tools-test--elisp-job-harness))
               (`(:harness ,harness :session-id ,session-id) context)
               (start (e-actions-call
                       'elisp-job
                       :run-batch
                       '(:code
                         "(progn (princ \"one\\n\") (sleep-for 0.2) (princ \"two\\n\"))")
                       (list :harness harness :session-id session-id)))
               (job-id (plist-get start :job_id)))
    (should (e-emacs-tools-test--wait-until
             (lambda ()
               (string-match-p
                "one"
                (plist-get
                 (e-actions-call
                  'elisp-job
                  :status
                  (list :job_id job-id)
                  (list :harness harness :session-id session-id))
                 :content)))
             2.0))
    (should (e-emacs-tools-test--wait-until
             (lambda ()
               (memq (plist-get (e-elisp-job-result job-id) :status)
                     '(ok error)))
             2.0))))

(ert-deftest e-emacs-tools-test-elisp-job-action-timeout-kills-worker ()
  "elisp-job action marks timed-out worker processes as errors."
  (pcase-let* ((context (e-emacs-tools-test--elisp-job-harness))
               (`(:harness ,harness :session-id ,session-id) context)
               (start (e-actions-call
                       'elisp-job
                       :run-batch
                       '(:code "(sleep-for 2)" :timeout 0.1)
                       (list :harness harness :session-id session-id)))
               (job-id (plist-get start :job_id)))
    (should (e-emacs-tools-test--wait-until
             (lambda ()
               (eq (plist-get (e-elisp-job-result job-id) :status) 'error))
             2.0))
    (let ((result (e-elisp-job-result job-id)))
      (should (string-match-p "timed out after 0.1 seconds"
                              (plist-get result :content))))))

(ert-deftest e-emacs-tools-test-elisp-job-is-action-not-tool ()
  "The operator Elisp surface keeps Elisp jobs out of model-facing tools."
  (let ((registry (e-tools-registry-create))
        (capability (e-elisp-job-capability-create)))
    (e-capabilities-register-tools
     (e-elisp-eval-capability-create)
     registry)
    (let ((names (mapcar (lambda (tool) (plist-get tool :name))
                         (e-tools-definitions registry))))
      (should (member "run_elisp" names))
      (should-not (member "elisp_job" names)))
    (should (e-capabilities-action-spec capability :run-batch))
    (should (e-capabilities-action-spec capability :status))
    (should (e-capabilities-action-spec capability :result))
    (should (e-capabilities-action-spec capability :cancel))))

(ert-deftest e-emacs-tools-test-run-elisp-can-start-elisp-job-action ()
  "run_elisp can start Elisp jobs through e-actions-call."
  (pcase-let* ((context (e-emacs-tools-test--elisp-job-harness))
               (`(:harness ,harness :session-id ,session-id) context)
               (registry (e-tools-registry-create)))
    (e-emacs-tools-register-run-elisp registry)
    (let ((result
           (e-emacs-tools-test--run-elisp-result
            registry
            "(stringp (plist-get (e-actions-call 'elisp-job :run-batch '(:code \"(princ \\\"ok\\\")\")) :job_id))"
            (list :interactive t
                  :harness harness
                  :session-id session-id))))
      (should (eq (plist-get result :status) 'ok))
      (should (equal (plist-get result :content)
                     '(:result "t"))))))

(ert-deftest e-emacs-tools-test-run-elisp-elisp-job-loads-tmp-resources-under-guard ()
  "run_elisp's elisp-job dispatch resolves tmp resources through the load guard.
Regression: `e-elisp-job--output-target' requires `e-session-tmp-resources'
while the interactive `run_elisp' load guard is active.  The guard lets a
require through only for an already-loaded feature or a bypassed load, so the
trusted runtime require must bind the bypass.  Shadowing `features' here
removes the feature so the guard's featurep shortcut cannot mask the bug: the
job errors unless the bypass is bound."
  (pcase-let* ((context (e-emacs-tools-test--elisp-job-harness))
               (`(:harness ,harness :session-id ,session-id) context)
               (loaded (featurep 'e-session-tmp-resources)))
    ;; `run_elisp' evaluates on a timer, outside this body's dynamic extent,
    ;; and dispatching through `e-actions-call' reloads the feature via trusted
    ;; layer discovery, so neither the full path nor a `let'-shadow of
    ;; `features' can reproduce the bug.  Exercise the exact guarded require
    ;; directly: run `e-elisp-job--output-target' under the same load guard in
    ;; an interactive context, with `e-session-tmp-resources' genuinely absent
    ;; from `features'.
    ;;
    ;; `featurep' reads the C `Vfeatures' cell and ignores a `let'-binding of
    ;; `features', so the feature must be removed from the global list with
    ;; `setq' (restored in `unwind-protect').  With the feature absent, the
    ;; guard's require shim rejects the load unless `output-target' binds the
    ;; bypass -- which is exactly the regression this guards.
    (unwind-protect
        (let ((e-tools--current-context (list :interactive t
                                              :harness harness
                                              :session-id session-id)))
          (setq features (remq 'e-session-tmp-resources features))
          (should-not (featurep 'e-session-tmp-resources))
          (let ((target (e-emacs-tools--with-run-elisp-load-guard
                          (e-elisp-job--output-target
                           (list :harness harness :session-id session-id)
                           "elisp-job-test-1"))))
            (should (string-prefix-p "tmp://" (plist-get target :uri)))))
      (when (and loaded (not (featurep 'e-session-tmp-resources)))
        (add-to-list 'features 'e-session-tmp-resources)))))

(ert-deftest e-emacs-tools-test-run-elisp-never-enters-debugger ()
  "run_elisp surfaces errors as tool errors without popping the debugger.
Regression: agent code that bound `debug-on-error' and signalled an error
entered the interactive debugger, whose buffer setup re-signalled and recursed,
pinning Emacs at 100% CPU.  The tool must inhibit the debugger so such code
returns a normal tool error instead."
  (let ((registry (e-tools-registry-create))
        (debug-on-error nil)
        ;; Fail loudly if anything tries to enter the debugger.
        (debugger (lambda (&rest _) (error "debugger must not be entered"))))
    (e-emacs-tools-register-run-elisp registry)
    ;; Agent code that turns the debugger on and then errors.
    (let ((result (e-tools-execute
                   registry
                   '(:id "call-1"
                     :name "run_elisp"
                     :arguments (:code "(let ((debug-on-error t)) (error \"boom\"))")))))
      (should (eq (plist-get result :status) 'error))
      (should (string-match-p "boom" (format "%s" (plist-get result :content)))))
    ;; The flags are restored after the call (not leaked globally).
    (should-not debug-on-error)))

(provide 'e-emacs-tools-test)

;;; e-emacs-tools-test.el ends here
