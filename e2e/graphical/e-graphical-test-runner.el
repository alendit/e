;;; e-graphical-test-runner.el --- Run graphical ERT tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Loaded last by `e2e/run-graphical-tests.sh'.  Unlike ERT's batch exit
;; helper, this runner executes in an interactive graphical Emacs and exits
;; explicitly after reporting the result to the invoking terminal.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defvar e-graphical-test-runner--output nil
  "Dynamically bound buffer collecting one graphical ERT report.")

(defvar e-graphical-test-runner--echo nil
  "When non-nil, also relay the current report to the parent process.")

(defun e-graphical-test-runner--print (format-string &rest arguments)
  "Record FORMAT-STRING with ARGUMENTS in the current test report."
  (let ((text (apply #'format format-string arguments)))
    (when (buffer-live-p e-graphical-test-runner--output)
      (with-current-buffer e-graphical-test-runner--output
        (goto-char (point-max))
        (insert text)))
    (when e-graphical-test-runner--echo
      (princ text 'external-debugging-output))))

(defun e-graphical-test-runner-run (&optional selector)
  "Run graphical ERT SELECTOR and return (:exit CODE :output REPORT)."
  (let ((selector (or selector
                      (getenv "E_GRAPHICAL_E2E_SELECTOR")
                      "^e-\\(?:chat\\|window-surface\\|workspace\\)-behavior-test-"))
        (e-graphical-test-runner--output
         (generate-new-buffer " *e graphical ERT report*")))
    (unwind-protect
        (if (not (and (display-graphic-p) (not noninteractive)))
            (progn
              (e-graphical-test-runner--print
               "Graphical E2E requires an interactive graphical Emacs frame.\n")
              (list :exit 2
                    :output
                    (with-current-buffer e-graphical-test-runner--output
                      (buffer-string))))
          (e-graphical-test-runner--print
           "Graphical E2E: Emacs %s, window system %S, config %s, selector %S\n"
           emacs-version window-system
           (or (getenv "E_E2E_EMACS_CONFIG") "isolated") selector)
          (when-let ((directory
                      (getenv "E_GRAPHICAL_E2E_SCREENSHOT_DIR")))
            (when (fboundp 'e-graphical-test-reset-screenshots)
              (e-graphical-test-reset-screenshots))
            (e-graphical-test-runner--print
             "Graphical E2E screenshots: %s\n"
             (expand-file-name directory)))
          (condition-case err
              (let ((original-message (symbol-function 'message)))
                ;; Graphical ERT reports through the echo area.  Retain those
                ;; messages in the process report so failures include their
                ;; condition and backtrace rather than only an aggregate.
                (cl-letf (((symbol-function 'message)
                           (lambda (format-string &rest arguments)
                             (when format-string
                               (e-graphical-test-runner--print
                                "%s\n" (apply #'format-message
                                              format-string arguments)))
                             (apply original-message
                                    format-string arguments))))
                  (let* ((standard-output e-graphical-test-runner--output)
                         (stats (ert-run-tests-batch selector)))
                    ;; Snapshot rendering is intentionally outside test timing.
                    ;; Captures retain the real frame model immediately, then
                    ;; pay SVG serialization cost only after behavior settles.
                    (when (fboundp 'e-graphical-test-render-pending-screenshots)
                      (e-graphical-test-render-pending-screenshots))
                    (let* ((unexpected
                            (ert-stats-completed-unexpected stats))
                           (exit (if (zerop unexpected) 0 1)))
                      (e-graphical-test-runner--print
                       "Graphical E2E complete: %d tests, %d unexpected.\n"
                       (ert-stats-total stats) unexpected)
                      (list :exit exit
                            :output
                            (with-current-buffer
                                e-graphical-test-runner--output
                              (buffer-string)))))))
            (error
             (e-graphical-test-runner--print
              "Graphical E2E runner failed: %S\n" err)
             (list :exit 2
                   :output
                   (with-current-buffer e-graphical-test-runner--output
                     (buffer-string))))))
      (kill-buffer e-graphical-test-runner--output))))

(defun e-graphical-test-runner-run-to-file (path &optional selector)
  "Run graphical ERT SELECTOR, write its report to PATH, and return exit code."
  (let ((result (e-graphical-test-runner-run selector)))
    (with-temp-file path
      (insert (plist-get result :output)))
    (plist-get result :exit)))

(when (getenv "E_GRAPHICAL_E2E_AUTORUN")
  (let* ((e-graphical-test-runner--echo t)
         (result (e-graphical-test-runner-run)))
    (kill-emacs (plist-get result :exit))))

;;; e-graphical-test-runner.el ends here
