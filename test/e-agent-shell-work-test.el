;;; e-agent-shell-work-test.el --- Tests for Agent Shell work registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for normalized Agent Shell work records.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-agent-shell-work)

(ert-deftest e-agent-shell-work-test-create-list-and-dead-buffer ()
  "Registry returns normalized work records and marks dead shell buffers."
  (let ((registry (e-agent-shell-work-registry-create))
        (buffer (get-buffer-create " *agent-shell-work*")))
    (let ((record (e-agent-shell-work-create
                   registry
                   :shell-buffer buffer
                   :agent-id "codex"
                   :project-root "/tmp/project"
                   :agent-session-id "session-1"
                   :transcript-file "/tmp/transcript.jsonl"
                   :origin 'e-created
                   :prompt "Implement feature 34")))
      (should (string-prefix-p "asw_" (plist-get record :work-id)))
      (should (equal (plist-get record :shell-buffer)
                     " *agent-shell-work*"))
      (should (eq (plist-get record :status) 'ready))
      (should (equal (length (e-agent-shell-work-list registry)) 1))
      (kill-buffer buffer)
      (should (eq (plist-get
                   (e-agent-shell-work-get registry
                                           (plist-get record :work-id))
                   :status)
                  'dead)))))

(ert-deftest e-agent-shell-work-test-event-transitions ()
  "Agent Shell events update normalized work status."
  (let* ((registry (e-agent-shell-work-registry-create))
         (buffer (get-buffer-create " *agent-shell-events*"))
         (record (e-agent-shell-work-create
                  registry
                  :shell-buffer buffer
                  :agent-id "codex"
                  :project-root "/tmp/project"))
         (work-id (plist-get record :work-id)))
    (unwind-protect
        (progn
          (should (eq (plist-get
                       (e-agent-shell-work-update-from-event
                        registry work-id '(:type input-submitted))
                       :status)
                      'busy))
          (should (eq (plist-get
                       (e-agent-shell-work-update-from-event
                        registry work-id '(:type permission-request
                                           :detail "edit file?"))
                       :status)
                      'blocked))
          (should (equal (plist-get
                          (e-agent-shell-work-update-from-event
                           registry work-id '(:type file-write
                                              :file "lisp/demo.el"))
                          :changed-files)
                         '("lisp/demo.el")))
          (should (eq (plist-get
                       (e-agent-shell-work-update-from-event
                        registry work-id '(:type turn-complete
                                           :response "done"
                                           :usage (:output-tokens 10)))
                       :status)
                      'finished))
          (should (equal (plist-get
                          (e-agent-shell-work-get registry work-id)
                          :latest-response-preview)
                         "done"))
          (should (eq (plist-get
                       (e-agent-shell-work-update-from-event
                        registry work-id '(:type clean-up))
                       :status)
                      'finished))
          (should (eq (plist-get
                       (e-agent-shell-work-update-from-event
                        registry work-id '(:type error :error "boom"))
                       :status)
                      'failed)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-agent-shell-work-test-status-can-be-refreshed ()
  "Registry records can be refreshed from adapter status probes."
  (let* ((registry (e-agent-shell-work-registry-create))
         (buffer (get-buffer-create " *agent-shell-refresh*"))
         (record (e-agent-shell-work-create
                  registry
                  :shell-buffer buffer
                  :agent-id "codex"
                  :project-root "/tmp/project"))
         (work-id (plist-get record :work-id)))
    (unwind-protect
        (progn
          (should (eq (plist-get record :status) 'ready))
          (should (eq (plist-get
                       (e-agent-shell-work-set-status registry work-id 'blocked)
                       :status)
                      'blocked))
          (should (eq (plist-get
                       (e-agent-shell-work-get registry work-id)
                       :status)
                      'blocked)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-agent-shell-work-test-agent-shell-alist-events ()
  "Agent Shell alist events with nested data update work records."
  (let* ((registry (e-agent-shell-work-registry-create))
         (buffer (get-buffer-create " *agent-shell-alist-events*"))
         (record (e-agent-shell-work-create
                  registry
                  :shell-buffer buffer
                  :agent-id "codex"
                  :project-root "/tmp/project"))
         (work-id (plist-get record :work-id)))
    (unwind-protect
        (progn
          (should (eq (plist-get
                       (e-agent-shell-work-update-from-event
                        registry work-id
                        '((:event . input-submitted)
                          (:data . nil)))
                       :status)
                      'busy))
          (should (equal (plist-get
                          (e-agent-shell-work-update-from-event
                           registry work-id
                           '((:event . file-write)
                             (:data . ((:file . "lisp/demo.el")))))
                          :changed-files)
                         '("lisp/demo.el")))
          (let* ((event
                  (list (cons :event 'turn-complete)
                        (cons :data
                              (list (cons :response "done")
                                    (cons :usage
                                          '((:output-tokens . 10)))))))
                 (done
                  (e-agent-shell-work-update-from-event
                   registry work-id event)))
            (should (eq (plist-get done :status) 'finished))
            (should (equal (plist-get done :latest-response-preview)
                           "done"))
            (should (equal (plist-get done :usage)
                           '((:output-tokens . 10)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

(provide 'e-agent-shell-work-test)

;;; e-agent-shell-work-test.el ends here
