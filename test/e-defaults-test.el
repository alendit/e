;;; e-defaults-test.el --- Tests for default harness startup -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for public default harness startup registration and construction.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-agents-std-context)
(require 'e-layer-selection)
(require 'e-layers)
(require 'e-openai)
(require 'e-session)

(defmacro e-defaults-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest e-defaults-test-startup-specs-only-include-chat-default ()
  "The startup default harness specs currently include only chat-default."
  (should (equal e-default-harness-specs
                 '((:id :chat-default
                    :factory e-default-chat-harness-create)))))

(ert-deftest e-defaults-test-registers-chat-default-factory ()
  "Startup default registration adds a lazy chat-default factory."
  (e-defaults-test--with-empty-harness-registry
    (e-default-harnesses-register)
    (should (member :chat-default (e-harness-registry-list)))
    (should-not (e-harness-registry-get :chat-default))))

(ert-deftest e-defaults-test-layer-specs-include-harness-base-and-os-base ()
  "Built-in layer specs use the current harness and OS base layer ids."
  (should (memq 'harness-base
                (mapcar (lambda (spec) (plist-get spec :id))
                        e-default-layer-specs)))
  (should (memq 'os-base
                (mapcar (lambda (spec) (plist-get spec :id))
                        e-default-layer-specs)))
  (should-not (memq 'base
                    (mapcar (lambda (spec) (plist-get spec :id))
                            e-default-layer-specs))))

(ert-deftest e-defaults-test-chat-default-factory-is-lazy ()
  "Looking up chat-default delegates to `e-default-chat-harness-create'."
  (e-defaults-test--with-empty-harness-registry
    (let ((created nil)
          (fake (e-harness-create
                 :backend (e-backend-fake-create :items nil))))
      (cl-letf (((symbol-function 'e-default-chat-harness-create)
                 (lambda ()
                   (setq created t)
                   fake)))
        (e-default-harnesses-register
         '((:id :chat-default :factory e-default-chat-harness-create)))
        (should (eq (e-harness-registry-get-or-create :chat-default) fake))
        (should created)))))

(ert-deftest e-defaults-test-session-store-is-persistent-and-cached ()
  "The default session store is persistent and reused for the same directory."
  (let ((directory (make-temp-file "e-defaults-" t))
        (e-default--chat-sessions nil))
    (unwind-protect
        (let ((e-session-directory directory))
          (let ((first (e-default-session-store))
                (second (e-default-session-store)))
            (should (eq first second))
            (should (e-session-store-p first))
            (should (e-session-store-persistent first))
            (should (equal (file-name-as-directory
                            (expand-file-name directory))
                           (e-session-store-directory first)))))
      (delete-directory directory t))))

(ert-deftest e-defaults-test-chat-harness-uses-provider-and-session-store ()
  "Default chat harness creation delegates provider setup outside presentation."
  (let ((e-openai-default-provider 'openai-compatible-gateway)
        (store (e-session-store-create))
        seen-provider
        seen-sessions)
    (cl-letf (((symbol-function 'e-openai-create-harness)
               (lambda (&rest args)
                 (setq seen-provider (plist-get args :provider))
                 (setq seen-sessions (plist-get args :sessions))
                 (e-harness-create
                  :backend (e-backend-fake-create :items nil)
                  :sessions seen-sessions))))
      (let ((harness (e-default-chat-harness-create :sessions store)))
        (should (e-harness-p harness))
        (should (eq seen-provider 'openai-compatible-gateway))
        (should (eq seen-sessions store))))))

(ert-deftest e-defaults-test-chat-harness-activates-chat-session-base-and-emacs ()
  "Default chat harness activation includes chat-session and configured layers."
  (cl-letf (((symbol-function 'e-openai-create-harness)
             (lambda (&rest _args)
               (e-harness-create
                :backend (e-backend-fake-create :items nil)))))
    (let ((e-default-chat-layer-ids '(agents-std-context harness-base e os-base emacs-base)))
      (let ((harness (e-default-chat-harness-create)))
        (should (equal (mapcar #'e-layer-id
                               (e-harness-active-layers harness))
                       '(chat-session agents-std-context harness-base e os-base emacs-base)))
        (should (memq 'agents-std-context
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (should (memq 'session-tmp-resources
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (should (memq 'tool-output-truncation
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (should (memq 'chat-session
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (should (memq 'layer-selection
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))))))

(ert-deftest e-defaults-test-chat-harness-uses-layer-ids-as-source-of-truth ()
  "Default chat harness creation uses configured layer ids."
  (cl-letf (((symbol-function 'e-openai-create-harness)
             (lambda (&rest _args)
               (e-harness-create
                :backend (e-backend-fake-create :items nil)))))
    (let ((e-default-chat-layer-ids '(e os-base web)))
      (let ((harness (e-default-chat-harness-create)))
        (should (equal (mapcar #'e-layer-id
                               (e-harness-active-layers harness))
                       '(chat-session e os-base web)))))))

(ert-deftest e-defaults-test-default-chat-layer_changes-update-config ()
  "Layer changes on the default chat harness update configured layer ids."
  (cl-letf (((symbol-function 'e-openai-create-harness)
             (lambda (&rest _args)
               (e-harness-create
                :backend (e-backend-fake-create :items nil)))))
    (let ((e-default-chat-layer-ids '(e os-base)))
      (let ((harness (e-default-chat-harness-create)))
        (e-layer-selection-enable harness 'web)
        (should (equal e-default-chat-layer-ids '(e os-base web)))
        (e-layer-selection-disable harness 'os-base)
        (should (equal e-default-chat-layer-ids '(e web)))))))

(ert-deftest e-defaults-test-startup-syncs-existing-chat-default-instance ()
  "Startup reconciles existing default chat harness instances from config."
  (e-defaults-test--with-empty-harness-registry
    (cl-letf (((symbol-function 'e-openai-create-harness)
               (lambda (&rest _args)
                 (e-harness-create
                  :backend (e-backend-fake-create :items nil)))))
      (let ((e-default-chat-layer-ids '(e os-base)))
        (e-default-harnesses-register)
        (let ((harness (e-harness-registry-get-or-create :chat-default)))
          (should (equal (mapcar #'e-layer-id
                                 (e-harness-active-layers harness))
                         '(chat-session e os-base)))
          (setq e-default-chat-layer-ids '(e web))
          (e-default-harnesses-startup)
          (should (eq (e-harness-registry-get :chat-default) harness))
          (should (equal (mapcar #'e-layer-id
                                 (e-harness-active-layers harness))
                         '(chat-session e web))))))))

(provide 'e-defaults-test)

;;; e-defaults-test.el ends here
