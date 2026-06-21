;;; e-agent-shell-fleet-test.el --- Tests for Agent Shell Fleet capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the Agent Shell Fleet capability actions.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-agent-shell-fleet)
(require 'e-capabilities)
(require 'e-store)

(ert-deftest e-agent-shell-fleet-test-capability-actions-and-resource ()
  "Fleet capability exposes shell actions and a readable reference resource."
  (let* ((capability (e-capability-with-agent-shell-create))
         (store (e-store-create)))
    (should (eq (e-capability-id capability) 'agent-shell-fleet))
    (dolist (action '(:handoff-work :adopt-work :list-work :work-status
                      :read-work :send-followup :interrupt-work))
      (should (functionp (e-capabilities-action capability action))))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://agent-shell-fleet/skills/agent-shell-work")))
    (should (string-match-p
             "handoff-work"
             (e-store-read store
                           "e://agent-shell-fleet/skills/agent-shell-work"
                           nil)))))

(ert-deftest e-agent-shell-fleet-test-handoff-list-status-read-followup-interrupt ()
  "Fleet actions return normalized records over real Agent Shell buffers."
  (let* ((registry (e-agent-shell-work-registry-create))
         (buffer (get-buffer-create " *agent-shell-fleet*"))
         (transcript (make-temp-file "e-agent-shell-fleet" nil ".txt"))
         (capability (e-capability-with-agent-shell-create
                      :registry registry))
         (sent nil)
         (interrupted nil))
    (unwind-protect
        (progn
          (with-temp-file transcript
            (insert "first line\nfinal response"))
          (cl-letf (((symbol-function 'e-agent-shell-start-worker)
                     (lambda (&rest _args) buffer))
                    ((symbol-function 'e-agent-shell-send-prompt)
                     (lambda (candidate prompt)
                       (push (list candidate prompt) sent)))
                    ((symbol-function 'e-agent-shell-subscribe)
                     (lambda (_candidate callback)
                       (funcall callback '(:type input-submitted))
                       :subscription))
                    ((symbol-function 'e-agent-shell-transcript-file)
                     (lambda (_candidate) transcript))
                    ((symbol-function 'e-agent-shell-status)
                     (lambda (_candidate) 'busy))
                    ((symbol-function 'e-agent-shell-interrupt)
                     (lambda (candidate &key force)
                       (setq interrupted (list candidate force))
                       t)))
            (let* ((handoff
                    (funcall (e-capabilities-action capability :handoff-work)
                             (list :prompt "Do feature 34"
                                   :project-root "/tmp/project"
                                   :agent-id "codex")))
                   (work-id (plist-get handoff :work-id)))
              (should (string-prefix-p "asw_" work-id))
              (should (eq (plist-get handoff :status) 'busy))
              (should (equal (plist-get handoff :shell-buffer)
                             " *agent-shell-fleet*"))
              (should (equal sent (list (list buffer "Do feature 34"))))
              (should (= (length (funcall
                                  (e-capabilities-action capability :list-work)
                                  nil))
                         1))
              (should (equal (plist-get
                              (funcall
                               (e-capabilities-action capability :work-status)
                               (list :work-id work-id))
                              :work-id)
                             work-id))
              (should (string-match-p
                       "final response"
                       (plist-get
                        (funcall (e-capabilities-action capability :read-work)
                                 (list :work-id work-id :limit 100))
                        :excerpt)))
              (funcall (e-capabilities-action capability :send-followup)
                       (list :work-id work-id :prompt "continue"))
              (should (equal (car sent) (list buffer "continue")))
              (should (eq (plist-get
                           (funcall
                            (e-capabilities-action capability :interrupt-work)
                            (list :work-id work-id :force t))
                           :status)
                          'interrupted))
              (should (equal interrupted (list buffer t))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-exists-p transcript)
        (delete-file transcript)))))

(ert-deftest e-agent-shell-fleet-test-adopt-work ()
  "Fleet can adopt an existing manual Agent Shell buffer."
  (let* ((registry (e-agent-shell-work-registry-create))
         (buffer (get-buffer-create " *agent-shell-manual*"))
         (capability (e-capability-with-agent-shell-create
                      :registry registry)))
    (unwind-protect
        (cl-letf (((symbol-function 'e-agent-shell-adopt-buffer)
                   (lambda (candidate)
                     (should (eq candidate buffer))
                     (list :shell-buffer buffer
                           :project-root "/tmp/manual"
                           :agent-id "codex"
                           :agent-session-id "manual-session"
                           :transcript-file "/tmp/manual.jsonl")))
                  ((symbol-function 'e-agent-shell-subscribe)
                   (lambda (_candidate _callback) :subscription))
                  ((symbol-function 'e-agent-shell-status)
                   (lambda (_candidate) 'blocked)))
          (let ((record (funcall (e-capabilities-action capability :adopt-work)
                                 (list :buffer " *agent-shell-manual*"))))
            (should (eq (plist-get record :origin) 'adopted))
            (should (eq (plist-get record :status) 'blocked))
            (should (equal (plist-get record :agent-session-id)
                           "manual-session"))
            (should (= (length (e-agent-shell-work-list registry)) 1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-agent-shell-fleet-test-list-and-status-refresh-adapter-state ()
  "Fleet list/status actions refresh records from Agent Shell status."
  (let* ((registry (e-agent-shell-work-registry-create))
         (buffer (get-buffer-create " *agent-shell-refresh-fleet*"))
         (record (e-agent-shell-work-create
                  registry
                  :shell-buffer buffer
                  :agent-id "codex"
                  :project-root "/tmp/project"))
         (work-id (plist-get record :work-id))
         (capability (e-capability-with-agent-shell-create
                      :registry registry))
         (status 'blocked))
    (unwind-protect
        (cl-letf (((symbol-function 'e-agent-shell-status)
                   (lambda (candidate)
                     (should (eq candidate buffer))
                     status)))
          (should (eq (plist-get
                       (car (funcall
                             (e-capabilities-action capability :list-work)
                             nil))
                       :status)
                      'blocked))
          (setq status 'finished)
          (should (eq (plist-get
                       (funcall
                        (e-capabilities-action capability :work-status)
                        (list :work-id work-id))
                       :status)
                      'finished)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-agent-shell-fleet-test-status-refresh-preserves-terminal-events ()
  "Fleet status refresh does not erase terminal event-derived states."
  (let* ((registry (e-agent-shell-work-registry-create))
         (buffer (get-buffer-create " *agent-shell-terminal-fleet*"))
         (record (e-agent-shell-work-create
                  registry
                  :shell-buffer buffer
                  :agent-id "codex"
                  :project-root "/tmp/project"))
         (work-id (plist-get record :work-id))
         (capability (e-capability-with-agent-shell-create
                      :registry registry)))
    (unwind-protect
        (progn
          (e-agent-shell-work-update-from-event
           registry work-id '(:type turn-complete :response "done"))
          (cl-letf (((symbol-function 'e-agent-shell-status)
                     (lambda (candidate)
                       (should (eq candidate buffer))
                       'ready)))
            (should (eq (plist-get
                         (funcall
                          (e-capabilities-action capability :work-status)
                          (list :work-id work-id))
                         :status)
                        'finished))
            (should (eq (plist-get
                         (car (funcall
                               (e-capabilities-action capability :list-work)
                               nil))
                         :status)
                        'finished))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-agent-shell-fleet-test)

;;; e-agent-shell-fleet-test.el ends here
