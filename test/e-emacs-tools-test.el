;;; e-emacs-tools-test.el --- Tests for harmless Emacs tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for low-risk concrete tools.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-emacs-tools)
(require 'e-tools)

(ert-deftest e-emacs-tools-test-current-time-tool ()
  "The current-time tool returns a readable time string."
  (let ((registry (e-tools-registry-create)))
    (e-emacs-tools-register-current-time registry)
    (let ((result (e-tools-execute registry
                                   '(:id "call-1"
                                     :name "current_time"
                                     :arguments nil))))
      (should (equal (plist-get result :status) 'ok))
      (should (stringp (plist-get result :content)))
      (should (< 0 (length (plist-get result :content)))))))

(provide 'e-emacs-tools-test)

;;; e-emacs-tools-test.el ends here
