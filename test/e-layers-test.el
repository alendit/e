;;; e-layers-test.el --- Tests for e layer activation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for harness-owned layers and context providers.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-context)
(require 'e-harness)
(require 'e-layers)
(require 'e-tools)

(ert-deftest e-layers-test-activation-registers-layer-tools ()
  "Activating a layer registers its tools against the harness registry."
  (let* ((layer (e-layer-create
                 :id 'test-layer
                 :name "Test Layer"
                 :tools (list (lambda (registry)
                                (e-tools-register
                                 registry
                                 :name "layer_tool"
                                 :description "Return the layer marker."
                                 :handler (lambda (_arguments) "layer"))))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-layer harness layer)
    (should (equal (mapcar (lambda (definition)
                             (plist-get definition :name))
                           (e-tools-definitions (e-harness-tools harness)))
                   '("layer_tool")))))

(ert-deftest e-layers-test-active-layer-context-prepends-transcript ()
  "Active layer instructions and providers are prepended to transcript context."
  (let* ((captured-messages nil)
         (backend (e-backend-create
                   :name "capture"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore options)
                              (setq captured-messages messages)
                              (funcall on-item
                                       '(:type assistant-message :content "ok"))
                              (funcall on-item '(:type done :reason stop))))))
         (provider (e-context-provider-create
                    :name 'test-context
                    :build (cl-function
                            (lambda (&key harness session-id turn-id)
                              (ignore harness session-id turn-id)
                              '((:role system :content "provider context"))))))
         (layer (e-layer-create
                 :id 'test-layer
                 :name "Test Layer"
                 :instructions "layer instructions"
                 :context-providers (list provider)))
         (harness (e-harness-create :backend backend)))
    (e-harness-activate-layer harness layer)
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "hello")
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           captured-messages)
                   '("layer instructions" "provider context" "hello")))))

(provide 'e-layers-test)

;;; e-layers-test.el ends here
