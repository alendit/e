;;; e-voice-adjustment-test.el --- Voice adjustment capability tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the gradual-discovery voice-adjustment surface.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-capabilities)
(require 'e-store)
(require 'e-tools)
(require 'e-voice-adjustment)
(require 'e-work)

(defun e-voice-adjustment-test--call (capability action arguments)
  "Call CAPABILITY ACTION with ARGUMENTS and return its cheap work result."
  (let* ((spec (e-capabilities-action-spec capability action))
         (handle (e-work-start (e-action-work spec) arguments)))
    (e-work-handle-result handle)))

(ert-deftest e-voice-adjustment-test-is-action-not-tool ()
  "Voice maintenance uses actions and contributes no dedicated model tools."
  (let* ((capability (e-voice-adjustment-capability-create))
         (registry (e-tools-registry-create)))
    (e-capabilities-register-tools capability registry)
    (should-not (e-tools-definitions registry))
    (dolist (action '(:record :list :clear))
      (should (e-action-p (e-capabilities-action-spec capability action))))))

(ert-deftest e-voice-adjustment-test-default-context-is-gradual ()
  "Default guidance advertises the guide without prescribing a check."
  (let* ((capability (e-voice-adjustment-capability-create))
         (instructions (e-capability-instructions capability)))
    (should (string-match-p "Voice adjustment is available on request"
                            instructions))
    (should (string-match-p
             "e://voice-adjustment/skills/voice-adjustment"
             instructions))
    (should-not (string-match-p "before drafting" instructions))
    (should-not (string-match-p "run an extra voice-check" instructions))))

(ert-deftest e-voice-adjustment-test-resources-separate-guide-and-tells ()
  "The detailed workflow and cached descriptions remain readable on demand."
  (let* ((e-voice-adjustment-store-file nil)
         (e-voice-adjustment--loaded t)
         (e-voice-adjustment--tells
          '((:key "catalog prose" :label "Catalog prose"
             :description "A system tour instead of the requested fact."
             :count 1 :last "2026-08-12T00:00:00Z")))
         (capability (e-voice-adjustment-capability-create))
         (store (e-store-create)))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://voice-adjustment/tells"
                     "e://voice-adjustment/skills/voice-adjustment")))
    (let ((guide (e-store-read
                  store "e://voice-adjustment/skills/voice-adjustment" nil))
          (tells (e-store-read store "e://voice-adjustment/tells" nil)))
      (should (string-match-p "Do not run an extra voice-check pass" guide))
      (should (string-match-p "e-actions-call 'voice-adjustment :record" guide))
      (should (string-match-p "A system tour" tells)))))

(ert-deftest e-voice-adjustment-test-passive-context-is-labels-only ()
  "Cached tells occupy one short avoidance line without detailed workflow."
  (let* ((e-voice-adjustment-store-file nil)
         (e-voice-adjustment--loaded t)
         (e-voice-adjustment--tells
          '((:key "catalog prose" :label "Catalog prose"
             :description "Long detail belongs behind the resource.")
            (:key "bolded not" :label "Bolded-not stress"
             :description "Another long detail.")))
         (messages (e-voice-adjustment--context-provider))
         (content (plist-get (car messages) :content)))
    (should (= (length messages) 1))
    (should (equal content
                   (concat "Reader-facing prose: avoid cached writing tells: "
                           "Catalog prose; Bolded-not stress.")))
    (should-not (string-match-p "Long detail" content))
    (should-not (string-match-p "check" content))))

(ert-deftest e-voice-adjustment-test-tells-are-dynamic-context ()
  "Mutable cached tells do not claim a stable-prefix cache position."
  (let* ((capability (e-voice-adjustment-capability-create))
         (provider (car (e-capability-context-providers capability))))
    (should (eq (e-context-provider-cache-placement provider)
                'dynamic-context))))

(ert-deftest e-voice-adjustment-test-actions-round-trip ()
  "Record, list, and clear remain available through capability actions."
  (let* ((e-voice-adjustment-store-file nil)
         (e-voice-adjustment--loaded t)
         (e-voice-adjustment--tells nil)
         (capability (e-voice-adjustment-capability-create)))
    (e-voice-adjustment-test--call
     capability :record
     '(:label "Catalog prose" :description "System-tour prose."))
    (let ((listed (e-voice-adjustment-test--call capability :list nil)))
      (should (= (plist-get listed :count) 1))
      (should (equal (plist-get (aref (plist-get listed :tells) 0) :label)
                     "Catalog prose")))
    (should (= (plist-get
                (e-voice-adjustment-test--call capability :clear nil)
                :count)
               0))))

(provide 'e-voice-adjustment-test)

;;; e-voice-adjustment-test.el ends here
