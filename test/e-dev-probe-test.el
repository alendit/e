;;; e-dev-probe-test.el --- Tests for bounded e live probes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the fixed external live-diagnostic boundary.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-dev-probe)
(require 'e-chat-surface)
(require 'e-chat-activity)

(ert-deftest e-dev-probe-test-ping-is-fixed-and-scalar ()
  "The ping probe returns one fixed bounded result."
  (should
   (equal (e-dev-live-probe 'ping)
          '(:ok t :operation ping :result (:responsive t)))))

(ert-deftest e-dev-probe-test-selected-projects-window-scalars ()
  "The selected probe returns presentation scalars, not live objects."
  (let ((buffer (generate-new-buffer " *e-dev-probe-selected*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert "probe")
          (let* ((result (e-dev-live-probe 'selected))
                 (projection (plist-get result :result)))
            (should (eq (plist-get result :ok) t))
            (should (equal (plist-get projection :buffer)
                           (buffer-name buffer)))
            (should (eq (plist-get projection :major-mode) 'fundamental-mode))
            (should (= (plist-get projection :buffer-size) 5))
            (should (cl-every #'numberp
                              (plist-get projection :window-edges)))))
      (kill-buffer buffer))))

(ert-deftest e-dev-probe-test-chat-reads-only-presentation-state ()
  "The chat probe projects bounded buffer-local presentation fields."
  (let ((buffer (generate-new-buffer " *e-dev-probe-chat*"))
        (harness (make-vector 1 nil)))
    (aset harness 0 harness)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer buffer
            (setq-local e-chat-session-id "session-1")
            (e-chat-surface-set-status "waiting")
            (e-chat-activity-start-progress "turn-1")
            (setq-local e-chat-harness harness))
          (switch-to-buffer buffer)
          (let* ((result (e-dev-live-probe 'chat))
                 (projection (plist-get result :result)))
            (should (eq (plist-get result :ok) t))
            (should (equal (plist-get projection :session-id) "session-1"))
            (should (equal (plist-get projection :status) "waiting"))
            (should (equal (plist-get projection :progress-turn-id) "turn-1"))
            (should (eq (plist-get projection :has-harness) t))
            (should-not (memq harness projection))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (e-chat-activity-stop-progress)))
      (kill-buffer buffer))))

(ert-deftest e-dev-probe-test-windows-obeys-hard-limit ()
  "The windows probe cannot return more than its configured limit."
  (let ((e-dev-live-probe--window-limit 1))
    (save-window-excursion
      (delete-other-windows)
      (split-window-right)
      (let* ((result (e-dev-live-probe 'windows))
             (projection (plist-get result :result))
             (windows (plist-get projection :windows)))
        (should (eq (plist-get result :ok) t))
        (should (= (length windows) 1))
        (should (eq (plist-get projection :truncated) t))))))

(ert-deftest e-dev-probe-test-symbol-requires-and-projects-a-symbol ()
  "The symbol probe exposes definition scalars and safely rejects other data."
  (let* ((result (e-dev-live-probe 'symbol 'e-dev-live-probe))
         (projection (plist-get result :result)))
    (should (eq (plist-get result :ok) t))
    (should (eq (plist-get projection :symbol) 'e-dev-live-probe))
    (should (eq (plist-get projection :function-bound) t))
    (should (stringp (plist-get projection :function-file))))
  (let ((failure (e-dev-live-probe 'symbol [not-a-symbol])))
    (should-not (plist-get failure :ok))
    (should (eq (plist-get failure :condition) 'wrong-type-argument))))

(ert-deftest e-dev-probe-test-unsupported-operation-is-bounded ()
  "Unsupported operations become bounded data rather than server errors."
  (let ((result (e-dev-live-probe 'anything)))
    (should-not (plist-get result :ok))
    (should (eq (plist-get result :condition)
                'e-dev-live-probe-unsupported))))

(ert-deftest e-dev-probe-test-cyclic-success-result-is-bounded ()
  "A buggy probe cannot return a cyclic object to the Emacs server."
  (let ((cycle (vector nil)))
    (aset cycle 0 cycle)
    (cl-letf (((symbol-function 'e-dev-live-probe--dispatch)
               (lambda (_operation _argument) cycle)))
      (let* ((result (e-dev-live-probe 'ping))
             (printed (prin1-to-string result)))
        (should (eq (plist-get result :ok) t))
        (should (string-match-p "#<cycle>" printed))
        (should (< (string-bytes printed) 1024))))))

(ert-deftest e-dev-probe-test-cyclic-error-data-is-bounded ()
  "A probe error cannot pass a cyclic condition object to the Emacs server."
  (let ((cycle (vector nil)))
    (aset cycle 0 cycle)
    (cl-letf (((symbol-function 'e-dev-live-probe--dispatch)
               (lambda (_operation _argument)
                 (signal 'wrong-type-argument (list 'e-harness cycle)))))
      (let* ((result (e-dev-live-probe 'ping))
             (printed (prin1-to-string result)))
        (should-not (plist-get result :ok))
        (should (eq (plist-get result :condition) 'wrong-type-argument))
        (should (string-match-p "#<cycle>" printed))
        (should (< (string-bytes printed) 2048))))))

(ert-deftest e-dev-probe-test-large-strings-are-truncated-before-return ()
  "A probe cannot return a large string to the Emacs server."
  (cl-letf (((symbol-function 'e-dev-live-probe--dispatch)
             (lambda (_operation _argument) (make-string 10000 ?x))))
    (let* ((result (e-dev-live-probe 'ping))
           (printed (prin1-to-string result)))
      (should (string-match-p "e live probe string truncated" printed))
      (should (< (string-bytes printed) 1024)))))

(provide 'e-dev-probe-test)

;;; e-dev-probe-test.el ends here
