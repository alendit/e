;;; e-goal-test.el --- Tests for Goal capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the harness-advanced Goal capability.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-goal)
(require 'e-harness-advanced)
(require 'e-capabilities)
(require 'e-store)
(require 'e-work)

(defun e-goal-test--call (capability action arguments)
  "Start CAPABILITY ACTION work with ARGUMENTS and return its result."
  (let* ((spec (e-capabilities-action-spec capability action))
         (handle (e-work-start (e-action-work spec) arguments)))
    (e-work-handle-result handle)))

(ert-deftest e-goal-test-capability-actions-and-reference ()
  "Goal capability exposes deterministic actions and a process reference."
  (let* ((registry (e-goal-registry-create))
         (capability (e-goal-capability-create :registry registry))
         (store (e-store-create)))
    (should (eq (e-capability-id capability) 'goal))
    (dolist (action '(:define-goal :list-goals :goal-status :next-action
                      :record-step :record-criterion :record-blocker
                      :clear-blocker :assess-goal :stop-goal))
      (should (e-action-p (e-capabilities-action-spec capability action))))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://goal/refs/process.md")))
    (should (string-match-p
             "Achievement predicate"
             (e-store-read store "e://goal/refs/process.md" nil)))))

(ert-deftest e-goal-test-controller-loop-achieves-goal ()
  "Goal actions track next action, step result, criteria, and achievement."
  (let* ((registry (e-goal-registry-create))
         (capability (e-goal-capability-create :registry registry)))
    (let ((goal (e-goal-test--call
                 capability :define-goal
                 (list :goal-id "feature"
                       :title "Finish feature"
                       :objective "Complete the plan safely"
                       :reference-uri "file://plan.org"
                       :steps '("Implement slice" "Review slice")
                       :success-criteria '("Tests passed")))))
      (should-not (plist-get goal :achieved))
      (let ((action (e-goal-test--call
                     capability :next-action (list :goal-id "feature"))))
        (should (eq (plist-get action :kind) 'step))
        (should (equal (plist-get (plist-get action :step) :id)
                       "step-1"))
        (should (string-match-p "Do only this step: step-1"
                                (plist-get action :prompt)))
        (should (string-match-p "file://plan.org"
                                (plist-get action :prompt))))
      (e-goal-test--call
       capability :record-step
       (list :goal-id "feature"
             :step-id "step-1"
             :status 'done
             :evidence "commit abc"))
      (should (equal (plist-get
                      (plist-get (e-goal-test--call
                                  capability :next-action
                                  (list :goal-id "feature"))
                                 :step)
                      :id)
                     "step-2"))
      (e-goal-test--call
       capability :record-step
       (list :goal-id "feature"
             :step-id "step-2"
             :status 'done
             :evidence "review clean"))
      (let ((action (e-goal-test--call
                     capability :next-action (list :goal-id "feature"))))
        (should (eq (plist-get action :kind) 'criterion))
        (should (string-match-p "Verify success criterion criterion-1"
                                (plist-get action :prompt))))
      (e-goal-test--call
       capability :record-criterion
       (list :goal-id "feature"
             :criterion-id "criterion-1"
             :verified t
             :evidence "rtk eldev test"))
      (let ((status (e-goal-test--call
                     capability :assess-goal (list :goal-id "feature"))))
        (should (plist-get status :achieved))
        (should (eq (plist-get status :computed-status) 'achieved)))
      (let ((action (e-goal-test--call
                     capability :next-action (list :goal-id "feature"))))
        (should (eq (plist-get action :status) 'achieved))
        (should-not (plist-get action :prompt))))))

(ert-deftest e-goal-test-blocker-prevents-achievement-until-cleared ()
  "Unresolved blockers prevent achievement even when work is complete."
  (let* ((registry (e-goal-registry-create))
         (capability (e-goal-capability-create :registry registry)))
    (e-goal-test--call
     capability :define-goal
     (list :goal-id "blocked"
           :title "Blocked goal"
           :steps '("Implement")
           :success-criteria '("Reviewed")))
    (e-goal-test--call
     capability :record-step
     (list :goal-id "blocked"
           :step-id "step-1"
           :status 'done))
    (e-goal-test--call
     capability :record-criterion
     (list :goal-id "blocked"
           :criterion-id "criterion-1"
           :verified t
           :evidence "review"))
    (e-goal-test--call
     capability :record-blocker
     (list :goal-id "blocked"
           :blocker-id "b1"
           :description "Need user decision"))
    (let ((status (e-goal-test--call
                   capability :assess-goal (list :goal-id "blocked"))))
      (should-not (plist-get status :achieved))
      (should (eq (plist-get status :computed-status) 'blocked)))
    (let ((action (e-goal-test--call
                   capability :next-action (list :goal-id "blocked"))))
      (should (eq (plist-get action :status) 'blocked))
      (should (string-match-p "Resolve or clear"
                              (plist-get action :prompt))))
    (let ((status (e-goal-test--call
                   capability :clear-blocker
                   (list :goal-id "blocked"
                         :blocker-id "b1"
                         :evidence "user approved"))))
      (should (plist-get status :achieved))
      (should (eq (plist-get status :computed-status) 'achieved)))))

(ert-deftest e-goal-test-harness-advanced-includes-goal-capability ()
  "Harness advanced layer includes the Goal capability."
  (let* ((layer (e-harness-advanced-layer-create))
         (ids (mapcar #'e-capability-id (e-layer-capabilities layer))))
    (should (memq 'goal ids))
    (should (equal (e-layer-requires layer) '(harness-base)))))

(provide 'e-goal-test)

;;; e-goal-test.el ends here
