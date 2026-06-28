;;; e-goal-test.el --- Tests for Goal capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the harness-base Goal capability.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-goal)
(require 'e-harness-base)
(require 'e-capabilities)
(require 'e-store)

(ert-deftest e-goal-test-capability-actions-and-reference ()
  "Goal capability exposes deterministic actions and a process reference."
  (let* ((registry (e-goal-registry-create))
         (capability (e-goal-capability-create :registry registry))
         (store (e-store-create)))
    (should (eq (e-capability-id capability) 'goal))
    (dolist (action '(:define-goal :list-goals :goal-status :next-action
                      :record-step :record-criterion :record-blocker
                      :clear-blocker :assess-goal :stop-goal))
      (should (functionp (e-capabilities-action capability action))))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://goal/refs/process.md")))
    (should (string-match-p
             "Achievement predicate"
             (e-store-read store "e://goal/refs/process.md" nil)))))

(ert-deftest e-goal-test-controller-loop-achieves-goal ()
  "Goal actions track next action, step result, criteria, and achievement."
  (let* ((registry (e-goal-registry-create))
         (capability (e-goal-capability-create :registry registry))
         (define (e-capabilities-action capability :define-goal))
         (next (e-capabilities-action capability :next-action))
         (record-step (e-capabilities-action capability :record-step))
         (record-criterion (e-capabilities-action capability :record-criterion))
         (assess (e-capabilities-action capability :assess-goal)))
    (let ((goal (funcall define
                         (list :goal-id "feature"
                               :title "Finish feature"
                               :objective "Complete the plan safely"
                               :reference-uri "file://plan.org"
                               :steps '("Implement slice" "Review slice")
                               :success-criteria '("Tests passed")))))
      (should-not (plist-get goal :achieved))
      (let ((action (funcall next (list :goal-id "feature"))))
        (should (eq (plist-get action :kind) 'step))
        (should (equal (plist-get (plist-get action :step) :id)
                       "step-1"))
        (should (string-match-p "Do only this step: step-1"
                                (plist-get action :prompt)))
        (should (string-match-p "file://plan.org"
                                (plist-get action :prompt))))
      (funcall record-step
               (list :goal-id "feature"
                     :step-id "step-1"
                     :status 'done
                     :evidence "commit abc"))
      (should (equal (plist-get
                      (plist-get (funcall next (list :goal-id "feature"))
                                 :step)
                      :id)
                     "step-2"))
      (funcall record-step
               (list :goal-id "feature"
                     :step-id "step-2"
                     :status 'done
                     :evidence "review clean"))
      (let ((action (funcall next (list :goal-id "feature"))))
        (should (eq (plist-get action :kind) 'criterion))
        (should (string-match-p "Verify success criterion criterion-1"
                                (plist-get action :prompt))))
      (funcall record-criterion
               (list :goal-id "feature"
                     :criterion-id "criterion-1"
                     :verified t
                     :evidence "rtk eldev test"))
      (let ((status (funcall assess (list :goal-id "feature"))))
        (should (plist-get status :achieved))
        (should (eq (plist-get status :computed-status) 'achieved)))
      (let ((action (funcall next (list :goal-id "feature"))))
        (should (eq (plist-get action :status) 'achieved))
        (should-not (plist-get action :prompt))))))

(ert-deftest e-goal-test-blocker-prevents-achievement-until-cleared ()
  "Unresolved blockers prevent achievement even when work is complete."
  (let* ((registry (e-goal-registry-create))
         (capability (e-goal-capability-create :registry registry)))
    (funcall (e-capabilities-action capability :define-goal)
             (list :goal-id "blocked"
                   :title "Blocked goal"
                   :steps '("Implement")
                   :success-criteria '("Reviewed")))
    (funcall (e-capabilities-action capability :record-step)
             (list :goal-id "blocked"
                   :step-id "step-1"
                   :status 'done))
    (funcall (e-capabilities-action capability :record-criterion)
             (list :goal-id "blocked"
                   :criterion-id "criterion-1"
                   :verified t
                   :evidence "review"))
    (funcall (e-capabilities-action capability :record-blocker)
             (list :goal-id "blocked"
                   :blocker-id "b1"
                   :description "Need user decision"))
    (let ((status (funcall (e-capabilities-action capability :assess-goal)
                           (list :goal-id "blocked"))))
      (should-not (plist-get status :achieved))
      (should (eq (plist-get status :computed-status) 'blocked)))
    (let ((action (funcall (e-capabilities-action capability :next-action)
                           (list :goal-id "blocked"))))
      (should (eq (plist-get action :status) 'blocked))
      (should (string-match-p "Resolve or clear"
                              (plist-get action :prompt))))
    (let ((status (funcall (e-capabilities-action capability :clear-blocker)
                           (list :goal-id "blocked"
                                 :blocker-id "b1"
                                 :evidence "user approved"))))
      (should (plist-get status :achieved))
      (should (eq (plist-get status :computed-status) 'achieved)))))

(ert-deftest e-goal-test-harness-base-includes-goal-capability ()
  "Harness base layer includes the Goal capability."
  (let* ((layer (e-harness-base-layer-create))
         (ids (mapcar #'e-capability-id (e-layer-capabilities layer))))
    (should (memq 'goal ids))))

(provide 'e-goal-test)

;;; e-goal-test.el ends here
