;;; e-harness-base-test.el --- Tests for harness-base layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for harness-owned support capabilities.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-hooks)
(require 'e-operations)
(require 'e-resources)

(declare-function e-harness-base-layer-create "e-harness-base")

(defun e-harness-base-test--tmp-read-method-p (resources)
  "Return non-nil when RESOURCES includes a tmp:// read method."
  (cl-some (lambda (method)
             (equal (e-resource-method-scheme method) "tmp"))
           (e-resources-methods-for-operation resources e-operation-read)))

(defun e-harness-base-test--raw-result-read-method-p (resources)
  "Return non-nil when RESOURCES includes a raw-result:// read method."
  (cl-some (lambda (method)
             (equal (e-resource-method-scheme method) "raw-result"))
           (e-resources-methods-for-operation resources e-operation-read)))

(defun e-harness-base-test--tmp-operation-ids (resources)
  "Return tmp:// operation ids in RESOURCES."
  (mapcar #'e-operation-id
          (seq-filter
           (lambda (operation)
             (e-resources-methods-for-operation resources operation))
           (e-resources-operations resources))))

(ert-deftest e-harness-base-test-require-and-create-layer ()
  "The harness-base layer bundles harness-owned support capabilities."
  (should (require 'e-harness-base nil t))
  (let ((layer (e-harness-base-layer-create)))
    (should (eq (e-layer-id layer) 'harness-base))
    (should (equal (e-layer-name layer) "Harness Base"))
    (should (equal (mapcar #'e-capability-id
                           (e-layer-capabilities layer))
                   '(harness-base-context
                     await
                     raw-result-resources
                     session-tmp-resources
                     session-resources
                     tool-output-truncation)))))

(ert-deftest e-harness-base-test-context-asks-for-novel-reasoning-messages ()
  "The harness-base layer asks for reasoning messages only when they add value."
  (should (require 'e-harness-base nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (e-harness-base-layer-create)))
    (e-harness-set-intrinsic-capabilities
     harness (e-layer-capabilities layer))
    (e-harness-create-session harness :id "session-1")
    (let* ((context (e-harness-context harness "session-1" "turn-1"))
           (messages (plist-get context :messages))
           (system-texts (mapcar (lambda (message)
                                   (plist-get message :content))
                                 messages)))
      (should (cl-some
               (lambda (text)
                 (and (stringp text)
                      (string-match-p
                       "reasoning explicitly and concretely"
                       text)
                      (string-match-p "without unnecessary detail" text)
                      (string-match-p "changes what the user can understand" text)
                      (string-match-p "distinct phase begins" text)
                      (string-match-p "new evidence narrows" text)
                      (string-match-p "Do not send an update for every command" text)
                      (string-match-p "repeat the same reason" text)
                      (not (string-match-p "as you work" text))))
               system-texts)))))

(ert-deftest e-harness-base-test-activation-adds-tmp-resource-and-hook ()
  "Activating harness-base exposes raw result resources and the post-tool hook."
  (should (require 'e-harness-base nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (e-harness-base-layer-create)))
    (e-harness-set-intrinsic-capabilities
     harness (e-layer-capabilities layer))
    (should (e-harness-base-test--raw-result-read-method-p
             (e-harness-resources harness "session-1" "turn-1")))
    (should (e-harness-base-test--tmp-read-method-p
             (e-harness-resources harness "session-1" "turn-1")))
    (should (equal (e-harness-base-test--tmp-operation-ids
                    (e-harness-resources harness "session-1" "turn-1"))
                   (if (executable-find "wot")
                       '(read table-of-content write edit glob search)
                     '(read write edit glob search))))
    (should (equal (mapcar #'e-hook-id
                           (e-hooks-for-point
                            (e-harness-hooks harness)
                            :post-tool-call))
                   '("50-tool-output-truncation")))
    (e-harness-set-intrinsic-capabilities harness nil)
    (should-not (e-harness-base-test--raw-result-read-method-p
                 (e-harness-resources harness "session-1" "turn-1")))
    (should-not (e-harness-base-test--tmp-read-method-p
                 (e-harness-resources harness "session-1" "turn-1")))
    (should-not (e-hooks-for-point
                 (e-harness-hooks harness)
                 :post-tool-call))))

(provide 'e-harness-base-test)

;;; e-harness-base-test.el ends here
