;;; e-harness-capability-composition-test.el --- Public harness capability composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Effective capability, layer, option, and session projection scenarios.

;;; Code:

(require 'ert)
(load (expand-file-name "e-harness-composition-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-harness-test-effective-capability-config-uses-session-root ()
  "Effective runtime config uses session project root and harness-local config."
  (let ((directory (make-temp-file "e-harness-config-" t)))
    (unwind-protect
        (progn
          (write-region
           "((nil . ((e-capability-config . ((dummy-config :value \"project\" :items \"project-item\"))))))"
           nil
           (expand-file-name ".dir-locals.el" directory)
           nil
           'silent)
          (let* ((e-capability-config
                  '((dummy-config :value "global" :items ("global-item"))))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
            (e-harness-create-session
             harness
             :id "session-1"
             :metadata (list :project-root directory))
            (e-harness-set-capability-config
             harness
             'dummy-config
             '(:value "runtime"))
            (should
             (equal
              (e-harness-effective-capability-config
               harness
               'dummy-config
               e-harness-test--capability-config-options
               :session-id "session-1")
              '(:value "runtime" :items ("project-item"))))
            (should
             (equal
              (e-harness-effective-capability-config
               harness
               'dummy-config
               e-harness-test--capability-config-options
               :session-id "session-1"
               :overrides '(:value "explicit"))
              '(:value "explicit" :items ("project-item"))))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-effective-capability-config-caches-resolved-values ()
  "Repeated equivalent config reads avoid resolving directory-local state."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
        (calls 0))
    (cl-letf (((symbol-function 'e-capability-config-resolve)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (list :value "resolved"))))
      (let ((first (e-harness-effective-capability-config
                    harness 'dummy-config
                    e-harness-test--capability-config-options))
            (second (e-harness-effective-capability-config
                     harness 'dummy-config
                     e-harness-test--capability-config-options)))
        (should (= calls 1))
        (should (equal first second))
        (setf (plist-get first :value) "mutated")
        (should (equal (plist-get second :value) "resolved"))))))

(ert-deftest e-harness-test-effective-capability-config-invalidates-runtime-cache ()
  "Changing runtime config discards derived effective config for the harness."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
        (calls 0))
    (cl-letf (((symbol-function 'e-capability-config-resolve)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (list :value (number-to-string calls)))))
      (e-harness-effective-capability-config
       harness 'dummy-config e-harness-test--capability-config-options)
      (e-harness-set-capability-config harness 'dummy-config '(:value "runtime"))
      (should
       (equal
        (e-harness-effective-capability-config
         harness 'dummy-config e-harness-test--capability-config-options)
        '(:value "2")))
      (should (= calls 2)))))

(ert-deftest e-harness-test-effective-capability-config-observes-global-revision ()
  "Changing global config makes a cached effective config stale."
  (let ((e-capability-config nil)
        (harness (e-harness-create :backend (e-backend-fake-create :items nil)))
        (calls 0))
    (cl-letf (((symbol-function 'e-capability-config-resolve)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (list :value (number-to-string calls)))))
      (e-harness-effective-capability-config
       harness 'dummy-config e-harness-test--capability-config-options)
      (setq e-capability-config '((dummy-config :value "global")))
      (should
       (equal
        (e-harness-effective-capability-config
         harness 'dummy-config e-harness-test--capability-config-options)
        '(:value "2")))
      (should (= calls 2)))))

(ert-deftest e-harness-test-effective-capability-config-does-not-cache-overrides ()
  "Explicit override values remain per-call and are never retained by the cache."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
        (calls 0))
    (cl-letf (((symbol-function 'e-capability-config-resolve)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (list :value (number-to-string calls)))))
      (e-harness-effective-capability-config
       harness 'dummy-config e-harness-test--capability-config-options
       :overrides '(:value "one"))
      (e-harness-effective-capability-config
       harness 'dummy-config e-harness-test--capability-config-options
       :overrides '(:value "two"))
      (should (= calls 2)))))

(ert-deftest e-harness-test-capability-config-describe-uses-buffer-harness ()
  "Describing config in a chat-like buffer uses the active session root."
  (let ((project (make-temp-file "e-harness-describe-project-" t))
        (other (make-temp-file "e-harness-describe-other-" t)))
    (unwind-protect
        (progn
          (write-region
           "((nil . ((e-capability-config . ((dummy-config :value \"project\"))))))"
           nil
           (expand-file-name ".dir-locals.el" project)
           nil
           'silent)
          (let* ((harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
                 (e-capability-config '((dummy-config :value "global"))))
            (e-harness-create-session
             harness
             :id "session-1"
             :metadata (list :project-root project))
            (with-temp-buffer
              (let ((default-directory other))
                (setq-local e-current-harness harness)
                (setq-local e-chat-session-id "session-1")
                (should
                 (string-match-p
                  ":value \"project\""
                  (e-capability-config-describe
                   'dummy-config
                   nil
                   e-harness-test--capability-config-options)))))))
      (delete-directory project t)
      (delete-directory other t))))

(ert-deftest e-harness-test-activate-capability-registers-tools-and-context ()
  "Direct capability activation registers capability contributions."
  (let* ((capability
          (e-capability-create
           :id 'direct-capability
           :instructions "direct instructions"
           :tools (list (lambda (registry)
                          (e-tools-test-register
                           registry
                           :name "direct_tool"
                           :description "Direct capability tool."
                           :handler (lambda (_arguments) "direct"))))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "hello"))
    (let ((context (e-harness-context harness "session-1")))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
                             (plist-get context :messages))
                     '("direct instructions" "hello"))))
    (should (equal (mapcar (lambda (definition)
                             (plist-get definition :name))
                           (e-tools-definitions (e-harness-tools harness)))
                   '("direct_tool")))))

(ert-deftest e-harness-test-active-capabilities-are-derived-from-layers ()
  "Active capabilities are a view over active layers, not duplicated state."
  (let* ((first-capability (e-capability-create :id 'first-capability))
         (second-capability (e-capability-create :id 'second-capability))
         (layer (e-layer-create
                 :id 'derived-layer
                 :name "Derived Layer"
                 :capabilities (list first-capability second-capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (should (equal (mapcar #'e-capability-id
                           (e-harness-active-capabilities harness))
                   '(first-capability second-capability)))))

(ert-deftest e-harness-test-tools-are-derived-from-effective-capabilities ()
  "The harness tool surface is rebuilt from effective capabilities on demand."
  (let* ((capability
          (e-capability-create
           :id 'tool-capability
           :tools (list (lambda (registry)
                          (e-tools-test-register
                           registry
                           :name "derived_tool"
                           :description "Derived tool."
                           :handler (lambda (_arguments) "derived"))))))
         (layer (e-layer-create
                 :id 'tool-layer
                 :name "Tool Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (stale-tools (e-harness-tools harness)))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (should-not (e-tools-definitions stale-tools))
    (should (equal (mapcar (lambda (definition)
                             (plist-get definition :name))
                           (e-tools-definitions (e-harness-tools harness)))
                   '("derived_tool")))
    (should (equal (plist-get
                    (e-tools-execute-batch
                     (e-harness-tools harness)
                     '(:id "call-1"
                       :name "derived_tool"
                       :arguments nil))
                    :content)
                   "derived"))))

(ert-deftest e-harness-test-prompts-are-derived-from-effective-capabilities ()
  "Prompts are aggregated from effective capability prompts in order."
  (let* ((first (e-prompt-spec-create
                 :name "explain"
                 :description "Explain."
                 :template "Explain this."))
         (second (e-prompt-spec-create
                  :name "review"
                  :description "Review."
                  :template "Review this."))
         (duplicate (e-prompt-spec-create
                     :name "explain"
                     :description "Explain differently."
                     :template "Explain this differently."))
         (harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-set-intrinsic-capabilities
     harness
     (list (e-capability-with-prompts-create
            :id 'prompt-one
            :name "Prompt One"
            :prompts (list first second))
           (e-capability-with-prompts-create
            :id 'prompt-two
            :name "Prompt Two"
            :prompts (list duplicate))))
    (should (equal (e-harness-prompts harness)
                   (list first second duplicate)))
    (should (eq (e-harness-prompt-by-name harness "explain") first))
    (should (equal (mapcar (lambda (collision)
                             (list (plist-get collision :name)
                                   (length (plist-get collision :prompts))))
                           (e-harness-prompt-name-collisions harness))
                   '(("explain" 2))))))

(ert-deftest e-harness-test-hooks-are-derived-from-effective-capabilities ()
  "Harness hook registries are derived from effective capabilities."
  (should (require 'e-hooks nil t))
  (let* ((capability
          (e-capability-create
           :id 'hook-capability
           :hooks
           (list (e-hook-create
                  :id "50-hook"
                  :point :post-tool-call
                  :handler (lambda (value _context)
                             (concat value "-hooked"))))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (list capability))))
    (should (equal (e-hooks-run-reduce
                    (e-harness-hooks harness)
                    :post-tool-call
                    "value"
                    nil)
                   "value-hooked"))))

(ert-deftest e-harness-test-direct-capability-activation-uses-layer-source ()
  "Direct capability activation wraps the capability as a layer."
  (let* ((capability (e-capability-create :id 'direct-capability))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness capability)
    (should-not (e-harness-effective-layer-ids harness))
    (should (equal (mapcar #'e-capability-id
                           (e-harness-active-capabilities harness))
                   '(direct-capability)))))

(ert-deftest e-harness-test-store-is-derived-from-effective-capabilities ()
  "Harness e:// stores are derived from active capability resource contributions."
  (let* ((capability
          (e-capability-with-skills-create
           :id 'skill-capability
           :skills
           (list
            (e-skill-spec-create
             :name "focused-work"
             :description "Use for focused work."
             :content "Stay focused."))))
         (layer (e-layer-create
                 :id 'skill-layer
                 :name "Skill Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (should (equal (mapcar #'e-store-entry-uri
                           (e-store-list (e-harness-store harness)))
                   '("e://skill-capability/skills/focused-work")))))

(ert-deftest e-harness-test-derived-views-do-not-keep-struct-compiler-macros ()
  "Derived harness view functions must not expand into stale struct slots."
  (should-not (get 'e-harness-active-capabilities 'compiler-macro))
  (should-not (get 'e-harness-store 'compiler-macro))
  (should-not (get 'e-harness-resources 'compiler-macro))
  (should-not (get 'e-harness-tools 'compiler-macro)))

(ert-deftest e-harness-test-prompt-passes-tool-definitions-as-options ()
  "Prompting includes registered tool definitions in backend options."
  (let* ((captured-options nil)
         (backend (e-backend-create
                   :name "capture"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages)
                              (setq captured-options options)
                              (funcall on-item
                                       '(:type assistant-message :content "ok"))
                              (funcall on-item '(:type done :reason stop))))))
         (capability
          (e-capability-create
           :id 'noop-capability
           :tools (list (lambda (registry)
                          (e-tools-test-register
                           registry
                           :name "noop"
                           :description "Accept no arguments."
                           :parameters '(:type "object" :properties nil)
                           :handler (lambda (_arguments) "now"))))))
         (layer (e-layer-create
                 :id 'noop-layer
                 :name "Noop Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create :backend backend)))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "raw prompt")
    (let* ((tool (seq-find (lambda (definition)
                             (equal (plist-get definition :name) "noop"))
                           (plist-get captured-options :tools)))
           (parameters (plist-get tool :parameters)))
      (should tool)
      (should (equal (plist-get parameters :type) "object"))
      (should (hash-table-p (plist-get parameters :properties)))
      (should (equal tool
                     `(:type "function"
                       :name "noop"
                       :description "Accept no arguments."
                       :parameters ,parameters
                       :strict :json-false))))))

(ert-deftest e-harness-test-session-options-override-default-options ()
  "Session-specific turn options override harness defaults and keep tools."
  (let* ((captured-options nil)
         (backend (e-backend-create
                   :name "capture"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages)
                              (setq captured-options options)
                              (funcall on-item
                                       '(:type assistant-message :content "ok"))
                              (funcall on-item '(:type done :reason stop))))))
         (capability
          (e-capability-create
           :id 'noop-capability
           :tools (list (lambda (registry)
                          (e-tools-test-register
                           registry
                           :name "noop"
                           :description "Accept no arguments."
                           :parameters '(:type "object" :properties nil)
                           :handler (lambda (_arguments) "now"))))))
         (layer (e-layer-create
                 :id 'noop-layer
                 :name "Noop Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend backend
                   :default-options '(:model "default-model"
                                      :reasoning-effort "medium"))))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (e-harness-create-session harness :id "session-1")
    (e-harness-set-session-model harness "session-1" "session-model")
    (e-harness-set-session-reasoning-effort harness "session-1" "high")
    (e-harness-test-prompt-batch harness "session-1" "raw prompt")
    (should (equal (plist-get captured-options :model) "session-model"))
    (should (equal (plist-get captured-options :reasoning-effort) "high"))
    (should (plist-get captured-options :tools))))

(ert-deftest e-harness-test-session-option-changes-emit-events ()
  "Changing session options emits a core event."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-set-session-model harness "session-1" "gpt-test")
    (let ((event (car events)))
      (should (eq (plist-get event :type) 'session-options-changed))
      (should (equal (plist-get event :session-id) "session-1"))
      (should (equal (plist-get (plist-get event :payload) :turn-options)
                     '(:model "gpt-test"))))))

(ert-deftest e-harness-test-session-projection-accessors ()
  "Harness exposes public read-only projections for presentation shells."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options '(:model "default-model"))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     store "session-1" '(:id "msg-1" :role user :content "hello title"))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'tool-started '(:name "tool"))
    (e-harness-set-session-model harness "session-1" "gpt-test")
    (should (equal (e-harness-session-title harness "session-1")
                   "hello title"))
    (should (equal (mapcar (lambda (session) (plist-get session :id))
                           (e-harness-session-list harness))
                   '("session-1")))
    (should (equal (mapcar (lambda (event) (plist-get event :event-type))
                           (e-harness-session-activity-events
                            harness "session-1"))
                   '(tool-started)))
    (let ((options (e-harness-turn-options harness "session-1")))
      (should (equal (plist-get options :model) "gpt-test"))
      (should-not (plist-get options :tools)))))

(ert-deftest e-harness-test-display-options-skip-tool-definitions ()
  "Display options merge model settings without materializing tools."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil)
                  :default-options '(:model "default-model"
                                     :reasoning-effort "medium"))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-set-session-model harness "session-1" "session-model")
    (cl-letf (((symbol-function 'e-harness-tools)
               (lambda (&rest _args)
                 (error "tools should not be materialized"))))
      (let ((options (e-harness-display-options harness "session-1")))
        (should (equal (plist-get options :model) "session-model"))
        (should (equal (plist-get options :reasoning-effort) "medium"))
        (should-not (plist-get options :tools))))))

(provide 'e-harness-capability-composition-test)

;;; e-harness-capability-composition-test.el ends here
