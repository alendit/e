;;; e-emacs-tools-test.el --- Tests for harmless Emacs tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for low-risk concrete tools.

;;; Code:

(require 'ert)
(require 'seq)
(require 'e)
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

(ert-deftest e-emacs-tools-test-run-elisp-never-enters-debugger ()
  "run_elisp surfaces errors as tool errors without popping the debugger.
Regression: agent code that bound `debug-on-error' and signalled an error
entered the interactive debugger, whose buffer setup re-signalled and recursed,
pinning Emacs at 100% CPU.  The tool must inhibit the debugger so such code
returns a normal tool error instead."
  (let ((registry (e-tools-registry-create))
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
