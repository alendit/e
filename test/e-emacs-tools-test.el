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
(require 'e-tools)

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
  "The read-buffer tool reads live buffer text."
  (let ((registry (e-tools-registry-create))
        (buffer (generate-new-buffer " *e-test-read*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "abcdef"))
          (e-emacs-tools-register-read-buffer registry)
          (should
           (equal (plist-get
                   (e-tools-execute
                    registry
                    `(:id "call-1"
                      :name "read_buffer"
                      :arguments (:name ,(buffer-name buffer) :start 2 :end 4)))
                   :content)
                  '(:name " *e-test-read*" :content "bcd" :start 2 :end 4))))
      (kill-buffer buffer))))

(ert-deftest e-emacs-tools-test-write-buffer-mutates-without-saving ()
  "The write-buffer tool replaces live buffer text without saving."
  (let ((registry (e-tools-registry-create))
        (buffer (generate-new-buffer " *e-test-write*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq buffer-file-name "/tmp/e-test-write.txt")
            (insert "old"))
          (e-emacs-tools-register-write-buffer registry)
          (let ((result (e-tools-execute
                         registry
                         `(:id "call-1"
                           :name "write_buffer"
                           :arguments (:name ,(buffer-name buffer)
                                             :content "new text")))))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (with-current-buffer buffer (buffer-string))
                           "new text"))
            (should (buffer-modified-p buffer))
            (should-not (plist-get (plist-get result :content) :saved))))
      (kill-buffer buffer))))

(ert-deftest e-emacs-tools-test-edit-buffer-replaces-unique-match ()
  "The edit-buffer tool replaces exactly one old-text match."
  (let ((registry (e-tools-registry-create))
        (buffer (generate-new-buffer " *e-test-edit*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "alpha beta gamma"))
          (e-emacs-tools-register-edit-buffer registry)
          (let ((result (e-tools-execute
                         registry
                         `(:id "call-1"
                           :name "edit_buffer"
                           :arguments (:name ,(buffer-name buffer)
                                             :old_text "beta"
                                             :new_text "delta")))))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (with-current-buffer buffer (buffer-string))
                           "alpha delta gamma"))))
      (kill-buffer buffer))))

(ert-deftest e-emacs-tools-test-edit-buffer-rejects-invalid-replacements ()
  "The edit-buffer tool rejects missing, duplicate, and no-op replacements."
  (let ((registry (e-tools-registry-create))
        (buffer (generate-new-buffer " *e-test-edit-errors*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "alpha beta beta"))
          (e-emacs-tools-register-edit-buffer registry)
          (should (equal (plist-get
                          (e-tools-execute
                           registry
                           `(:id "call-1"
                             :name "edit_buffer"
                             :arguments (:name ,(buffer-name buffer)
                                               :old_text "missing"
                                               :new_text "x")))
                          :status)
                         'error))
          (should (equal (plist-get
                          (e-tools-execute
                           registry
                           `(:id "call-2"
                             :name "edit_buffer"
                             :arguments (:name ,(buffer-name buffer)
                                               :old_text "beta"
                                               :new_text "x")))
                          :status)
                         'error))
          (should (equal (plist-get
                          (e-tools-execute
                           registry
                           `(:id "call-3"
                             :name "edit_buffer"
                             :arguments (:name ,(buffer-name buffer)
                                               :old_text "alpha"
                                               :new_text "alpha")))
                          :status)
                         'error)))
      (kill-buffer buffer))))

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

(provide 'e-emacs-tools-test)

;;; e-emacs-tools-test.el ends here
