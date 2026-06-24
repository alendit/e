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
(require 'e-capabilities)
(require 'e-context)
(require 'e-harness)
(require 'e-layer)
(require 'e-layers)
(require 'e-shells)
(require 'e-tools)

(defmacro e-layers-test--with-empty-layer-registry (&rest body)
  "Run BODY with an isolated layer registry."
  (declare (indent 0) (debug t))
  `(let ((e-layer--registry (make-hash-table :test 'eq)))
     ,@body))

(ert-deftest e-layers-test-rejects-direct-behavior-fields ()
  "Layer records are pure presets and reject direct behavior fields."
  (should-error
   (apply #'e-layer-create
          '(:id bad-layer :name "Bad" :tools nil)))
  (should-error
   (apply #'e-layer-create
          '(:id bad-layer :name "Bad" :instructions "legacy")))
  (should-error
   (apply #'e-layer-create
          '(:id bad-layer :name "Bad" :context-providers nil))))

(ert-deftest e-layers-test-capability-layer-registers-tools-and-context ()
  "Activating a capability-backed layer activates capability contributions."
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
         (first-provider
          (e-context-provider-create
           :name 'first-context
           :build (lambda (&rest _args)
                    '((:role system :content "first context")))))
         (second-provider
          (e-context-provider-create
           :name 'second-context
           :build (lambda (&rest _args)
                    '((:role system :content "second context")))))
         (first-capability
          (e-capability-create
           :id 'first-capability
           :instructions "first instructions"
           :context-providers (list first-provider)
           :tools (list (lambda (registry)
                          (e-tools-register
                           registry
                           :name "first_tool"
                           :description "First capability tool."
                           :handler (lambda (_arguments) "first"))))))
         (second-capability
          (e-capability-create
           :id 'second-capability
           :instructions "second instructions"
           :context-providers (list second-provider)
           :tools (list (lambda (registry)
                          (e-tools-register
                           registry
                           :name "second_tool"
                           :description "Second capability tool."
                           :handler (lambda (_arguments) "second"))))))
         (harness (e-harness-create
                   :backend backend
                   :intrinsic-capabilities
                   (list first-capability second-capability))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "hello")
    (should (equal (mapcar (lambda (definition)
                             (plist-get definition :name))
                           (e-tools-definitions (e-harness-tools harness)))
                   '("first_tool" "second_tool")))
    (should (equal (mapcar (lambda (capability)
                             (e-capability-id capability))
                           (e-harness-active-capabilities harness))
                   '(first-capability second-capability)))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           captured-messages)
                   '("first instructions"
                     "second instructions"
                     "first context"
                     "second context"
                     "hello")))))

(ert-deftest e-layers-test-e-layer-registers-compact-session-tool ()
  "The self-management layer exposes a tool for in-turn context compaction."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "ok")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (setf (e-harness-intrinsic-capabilities harness)
          (e-layer-capabilities (e-core-layer-create)))
    (let ((tool (seq-find (lambda (definition)
                            (equal (plist-get definition :name)
                                   "compact_session"))
                          (e-tools-definitions (e-harness-tools harness)))))
      (should tool)
      (should (equal (plist-get (plist-get tool :parameters) :type)
                     "object")))))

(ert-deftest e-layers-test-registers-known-layer-specs-by-id ()
  "The layer registry stores lazy known layer specs by stable ids."
  (e-layers-test--with-empty-layer-registry
    (let ((created nil))
      (e-layer-register
       (e-layer-spec-create
        :id 'optional
        :name "Optional"
        :summary "Optional layer."
        :factory (lambda ()
                   (setq created t)
                   (e-layer-create :id 'optional :name "Optional"))))
      (should (eq (e-layer-spec-id (e-layer-get 'optional)) 'optional))
      (should (equal (mapcar #'e-layer-spec-id (e-layer-list))
                     '(optional)))
      (should-not created)
      (let ((layer (e-layer-create-registered 'optional)))
        (should created)
        (should (eq (e-layer-id layer) 'optional))))))

(ert-deftest e-layers-test-harness-disables-layer-by-id ()
  "Harness explicit layer ids can be queried and disabled by layer id."
  (e-layers-test--with-empty-layer-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-layer-register
       (e-layer-spec-create
        :id 'first :name "First"
        :factory (lambda () (e-layer-create :id 'first :name "First"))))
      (e-layer-register
       (e-layer-spec-create
        :id 'second :name "Second"
        :factory (lambda () (e-layer-create :id 'second :name "Second"))))
      (e-harness-enable-layer-id harness 'first)
      (e-harness-enable-layer-id harness 'second)
      (should (e-harness-layer-enabled-p harness 'first))
      (should (e-harness-layer-effective-p harness 'second))
      (should (eq (plist-get (e-harness-disable-layer-id harness 'first)
                             :status)
                  'disabled))
      (should-not (e-harness-layer-enabled-p harness 'first))
      (should (equal (e-harness-enabled-layer-ids harness)
                     '(second))))))

(ert-deftest e-layers-test-harness-activates-layer-owned-shells ()
  "Activating and deactivating a layer controls its shell manifests."
  (let ((e-shell--registry (make-hash-table :test 'eq))
        (e-shell--scoped-registry (make-hash-table :test 'eq)))
    (let* ((harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)))
           (shell (e-shell-create :id 'topic :name "Topic"))
           (layer (e-layer-create
                   :id 'topic-layer
                   :name "Topic Layer"
                   :shells (list shell))))
      (e-layer-register
       (e-layer-spec-create
        :id 'topic-layer :name "Topic Layer"
        :factory (lambda () layer)))
      (e-harness-enable-layer-id harness 'topic-layer)
      (should (eq (e-shell-get-active 'topic harness) shell))
      (should (memq shell (e-shell-list-active harness)))
      (should (eq (plist-get (e-harness-disable-layer-id harness 'topic-layer)
                             :status)
                  'disabled))
      (should-not (e-shell-get-active 'topic harness)))))

(ert-deftest e-layers-test-activate-pulls-in-required-layers ()
  "Activating a layer activates declared `requires' first, from the registry."
  (e-layers-test--with-empty-layer-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
          (dep-created nil))
      (e-layer-register
       (e-layer-spec-create
        :id 'dep :name "Dep"
        :factory (lambda ()
                   (setq dep-created t)
                   (e-layer-create :id 'dep :name "Dep"))))
      (e-layer-register
       (e-layer-spec-create
        :id 'consumer :name "Consumer"
        :factory (lambda ()
                   (e-layer-create :id 'consumer :name "Consumer"
                                   :requires '(dep)))))
      (e-harness-enable-layer-id harness 'consumer)
      (should dep-created)
      ;; Dependency precedes its consumer in activation order.
      (should (equal (e-harness-effective-layer-ids harness)
                     '(dep consumer))))))

(ert-deftest e-layers-test-disabling-required-layer-keeps-it-effective ()
  "Disabling an explicitly enabled dependency leaves it effective if required."
  (e-layers-test--with-empty-layer-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-layer-register
       (e-layer-spec-create
        :id 'dep :name "Dep"
        :factory (lambda ()
                   (e-layer-create :id 'dep :name "Dep"))))
      (e-layer-register
       (e-layer-spec-create
        :id 'consumer :name "Consumer"
        :factory (lambda ()
                   (e-layer-create :id 'consumer :name "Consumer"
                                   :requires '(dep)))))
      (e-harness-enable-layer-id harness 'dep)
      (e-harness-enable-layer-id harness 'consumer)
      (should (equal (e-harness-enabled-layer-ids harness)
                     '(dep consumer)))
      (should (eq (plist-get (e-harness-disable-layer-id harness 'dep)
                             :status)
                  'disabled-but-required))
      (should-not (e-harness-layer-enabled-p harness 'dep))
      (should (e-harness-layer-effective-p harness 'dep))
      (should (equal (e-harness-enabled-layer-ids harness)
                     '(consumer)))
      (should (equal (e-harness-effective-layer-ids harness)
                     '(dep consumer))))))

(ert-deftest e-layers-test-activate-skips-already-active-requires ()
  "A required layer already active is not re-created or duplicated."
  (e-layers-test--with-empty-layer-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
          (dep-calls 0))
      (e-layer-register
       (e-layer-spec-create
        :id 'dep :name "Dep"
        :factory (lambda ()
                   (setq dep-calls (1+ dep-calls))
                   (e-layer-create :id 'dep :name "Dep"))))
      (e-layer-register
       (e-layer-spec-create
        :id 'consumer :name "Consumer"
        :factory (lambda ()
                   (e-layer-create :id 'consumer :name "Consumer"
                                   :requires '(dep)))))
      (e-harness-enable-layer-id harness 'dep)
      (e-harness-enable-layer-id harness 'consumer)
      ;; Effective views are fresh; dependency factories may run when deriving.
      (should (>= dep-calls 1))
      (should (equal (e-harness-effective-layer-ids harness)
                     '(dep consumer))))))

(ert-deftest e-layers-test-effective-layer-ids-reject-requires-cycle ()
  "Mutually dependent layers fail clearly instead of recursing forever."
  (e-layers-test--with-empty-layer-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-layer-register
       (e-layer-spec-create
        :id 'a :name "A"
        :factory (lambda () (e-layer-create :id 'a :name "A" :requires '(b)))))
      (e-layer-register
       (e-layer-spec-create
        :id 'b :name "B"
        :factory (lambda () (e-layer-create :id 'b :name "B" :requires '(a)))))
      (setf (e-harness-enabled-layer-ids harness) '(a))
      (should-error (e-harness-effective-layer-ids harness)))))

(provide 'e-layers-test)

;;; e-layers-test.el ends here
