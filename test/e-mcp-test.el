;;; e-mcp-test.el --- Tests for MCP capability wrappers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for MCP server discovery, generated e tools, and result mapping.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-layer)
(require 'e-mcp)
(require 'e-tools)

(defconst e-mcp-test--schema
  '(:type "object" :properties (:text (:type "string")))
  "Reusable object JSON schema for MCP tests.")

(defun e-mcp-test--server (&optional timeout)
  "Return a valid test MCP server with optional TIMEOUT."
  (e-mcp-server-create
   :id "fixture"
   :command '("node" "fixture.mjs")
   :env '(("TOKEN" . "redacted"))
   :timeout timeout))

(defun e-mcp-test--tool (&rest args)
  "Return a valid test MCP tool from keyword ARGS."
  (apply #'e-mcp-tool-create
         :server-id "fixture"
         :name "echo"
         :description "Echo text."
         :input-schema e-mcp-test--schema
         args))

(defun e-mcp-test--fake-tool (name schema &optional description)
  "Return a helper catalog tool plist named NAME with SCHEMA."
  (list :name name
        :description (or description (format "Run %s." name))
        :inputSchema schema))

(defmacro e-mcp-test--with-transport (transport &rest body)
  "Run BODY with fake MCP helper TRANSPORT and reset helper state."
  (declare (indent 1))
  `(let ((e-mcp-helper-transport-function ,transport))
     (unwind-protect
         (progn
           (e-mcp-reset)
           ,@body)
       (e-mcp-reset))))

(ert-deftest e-mcp-test-server-validates-required-fields ()
  "MCP server specs reject empty ids and commands."
  (let ((server (e-mcp-test--server 7)))
    (should (e-mcp-server-p server))
    (should (equal (e-mcp-server-id server) "fixture"))
    (should (equal (e-mcp-server-command server) '("node" "fixture.mjs")))
    (should (= (e-mcp-server-timeout server) 7)))
  (should-error (e-mcp-server-create :id "" :command '("node"))
                :type 'wrong-type-argument)
  (should-error (e-mcp-server-create :id "fixture" :command nil)
                :type 'wrong-type-argument))

(ert-deftest e-mcp-test-tool-validates-required-fields ()
  "MCP catalog tools reject empty names and non-object input schemas."
  (let ((tool (e-mcp-test--tool :metadata '(:origin fixture))))
    (should (e-mcp-tool-p tool))
    (should (equal (e-mcp-tool-server-id tool) "fixture"))
    (should (equal (e-mcp-tool-name tool) "echo"))
    (should (equal (e-mcp-tool-metadata tool) '(:origin fixture))))
  (should-error (e-mcp-tool-create
                 :server-id "fixture"
                 :name ""
                 :input-schema e-mcp-test--schema)
                :type 'wrong-type-argument)
  (should-error (e-mcp-tool-create
                 :server-id "fixture"
                 :name "echo"
                 :input-schema "not an object")
                :type 'wrong-type-argument))

(ert-deftest e-mcp-test-helper-request-uses-fake-transport ()
  "The helper client serializes list, call, and refresh operations."
  (let (requests)
    (e-mcp-test--with-transport
        (lambda (request)
          (push request requests)
          (pcase (plist-get request :op)
            ("list-tools"
             '(:ok t :result (:tools [(:name "echo"
                                      :description "Echo text."
                                      :inputSchema (:type "object"))])))
            ("call-tool"
             '(:ok t :result (:content [(:type "text" :text "ok")]
                            :isError :json-false)))
            ("refresh"
             '(:ok t :result (:refreshed t)))))
      (should (= (length (e-mcp-list-tools (list (e-mcp-test--server)))) 1))
      (should (equal (e-mcp-call-tool (list (e-mcp-test--server))
                                      "fixture"
                                      "echo"
                                      '(:text "hi"))
                     '(:content [(:type "text" :text "ok")]
                       :isError :json-false)))
      (should (equal (e-mcp-refresh (list (e-mcp-test--server)))
                     '(:refreshed t)))
      (should (equal (mapcar (lambda (request) (plist-get request :op))
                             (nreverse requests))
                     '("list-tools" "call-tool" "refresh"))))))

(ert-deftest e-mcp-test-helper-request-rejects-malformed-response ()
  "Malformed helper responses signal a protocol error."
  (e-mcp-test--with-transport
      (lambda (_request)
        '(:result (:tools [])))
    (should-error (e-mcp-list-tools (list (e-mcp-test--server)))
                  :type 'e-mcp-protocol-error)))

(ert-deftest e-mcp-test-helper-request-times-out ()
  "A live helper process that never answers signals a backend timeout."
  (skip-unless (executable-find "tail"))
  (let ((server (e-mcp-test--server)))
    (unwind-protect
        (cl-letf (((symbol-function 'e-mcp--helper-command)
                   (lambda () '("tail" "-f" "/dev/null"))))
          (e-mcp-reset)
          (should-error
           (e-mcp--helper-request "list-tools" (list server) :timeout 0.02)
           :type 'e-mcp-backend-timeout)
          (should (get 'e-mcp-backend-timeout 'e-tools-infrastructure-error)))
      (e-mcp-reset))))

(ert-deftest e-mcp-test-capability-registers-discovered-tools ()
  "MCP capability construction registers discovered tools as normal e tools."
  (e-mcp-test--with-transport
      (lambda (request)
        (should (equal (plist-get request :op) "list-tools"))
        '(:ok t :result
          (:tools [(:name "echo"
                   :description "Echo text."
                   :inputSchema (:type "object"
                                :properties (:text (:type "string"))))])))
    (let* ((capability
            (e-capability-with-mcp-create
             :id 'fixture-mcp
             :name "Fixture MCP"
             :mcp-servers (list (e-mcp-test--server))))
           (registry (e-tools-registry-create)))
      (e-capabilities-register-tools capability registry)
      (let* ((definitions (e-tools-definitions registry))
             (definition (car definitions))
             (stored (gethash "mcp__fixture__echo"
                              (e-tools-registry-tools registry))))
        (should (equal (mapcar (lambda (item) (plist-get item :name))
                               definitions)
                       '("mcp__fixture__echo")))
        (should (equal (plist-get definition :description)
                       "[MCP fixture] Echo text."))
        (should (equal (plist-get definition :parameters)
                       '(:type "object"
                         :properties (:text (:type "string")))))
        (should (equal (plist-get stored :metadata)
                       '(:kind mcp-tool
                         :server-id "fixture"
                         :tool-name "echo")))))))

(ert-deftest e-mcp-test-list-tools-keeps-multiple-server-catalogs-distinct ()
  "Flattened helper catalogs retain the originating MCP server id."
  (e-mcp-test--with-transport
      (lambda (_request)
        '(:ok t :result
          (:tools [(:serverId "one"
                   :name "echo"
                   :description "Echo one."
                   :inputSchema (:type "object"))
                  (:serverId "two"
                   :name "echo"
                   :description "Echo two."
                   :inputSchema (:type "object"))])))
    (let* ((servers
            (list (e-mcp-server-create :id "one" :command '("node" "one.mjs"))
                  (e-mcp-server-create :id "two" :command '("node" "two.mjs"))))
           (tools (e-mcp-list-tools servers)))
      (should (equal (mapcar #'e-mcp-tool-server-id tools)
                     '("one" "two")))
      (should (equal (mapcar #'e-mcp--generated-tool-name tools)
                     '("mcp__one__echo" "mcp__two__echo"))))))

(ert-deftest e-mcp-test-generated-tool-maps-text-and-structured-results ()
  "Generated MCP tools preserve text and structured JSON result content."
  (let ((structured-values
         (list "text" 42 t :json-false nil [1 2] '(:nested (:ok t)))))
    (dolist (value structured-values)
      (e-mcp-test--with-transport
          (lambda (request)
            (pcase (plist-get request :op)
              ("list-tools"
               (list :ok t
                     :result
                     (list :tools
                           (vector (e-mcp-test--fake-tool
                                    "value"
                                    '(:type "object"))))))
              ("call-tool"
               (list :ok t
                     :result
                     (list :content [(:type "text" :text "summary")]
                           :structuredContent value
                           :isError :json-false)))))
        (let* ((capability
                (e-capability-with-mcp-create
                 :id 'fixture-mcp
                 :mcp-servers (list (e-mcp-test--server))))
               (registry (e-tools-registry-create)))
          (e-capabilities-register-tools capability registry)
          (should
           (equal (e-tools-execute
                   registry
                   '(:id "call-1"
                     :name "mcp__fixture__value"
                     :arguments (:unused t)))
                  (list :tool-call-id "call-1"
                        :name "mcp__fixture__value"
                        :status 'ok
                        :content (list :content "summary"
                                       :structuredContent value)
                        :metadata '(:kind mcp-tool
                                    :server-id "fixture"
                                    :tool-name "value")))))))))

(ert-deftest e-mcp-test-generated-tool-maps-mcp-error-result ()
  "MCP execution errors are model-visible tool errors."
  (e-mcp-test--with-transport
      (lambda (request)
        (pcase (plist-get request :op)
          ("list-tools"
           (list :ok t
                 :result (list :tools
                               (vector (e-mcp-test--fake-tool
                                        "fail"
                                        '(:type "object"))))))
          ("call-tool"
           '(:ok t :result (:content [(:type "text" :text "denied")]
                          :isError t)))))
    (let* ((capability
            (e-capability-with-mcp-create
             :id 'fixture-mcp
             :mcp-servers (list (e-mcp-test--server))))
           (registry (e-tools-registry-create)))
      (e-capabilities-register-tools capability registry)
      (should (equal (e-tools-execute
                      registry
                      '(:id "call-1"
                        :name "mcp__fixture__fail"
                        :arguments nil))
                     '(:tool-call-id "call-1"
                       :name "mcp__fixture__fail"
                       :status error
                       :content "denied"
                       :metadata (:kind mcp-tool
                                  :server-id "fixture"
                                  :tool-name "fail"
                                  :error mcp-execution-error)))))))

(ert-deftest e-mcp-test-generated-tool-summarizes-unsupported-content ()
  "Unsupported MCP content blocks are truthful model-visible summaries."
  (e-mcp-test--with-transport
      (lambda (request)
        (pcase (plist-get request :op)
          ("list-tools"
           (list :ok t
                 :result (list :tools
                               (vector (e-mcp-test--fake-tool
                                        "image"
                                        '(:type "object"))))))
          ("call-tool"
           '(:ok t :result (:content [(:type "image" :data "payload")]
                          :isError :json-false)))))
    (let* ((capability
            (e-capability-with-mcp-create
             :id 'fixture-mcp
             :mcp-servers (list (e-mcp-test--server))))
           (registry (e-tools-registry-create)))
      (e-capabilities-register-tools capability registry)
      (should (equal (plist-get
                      (e-tools-execute
                       registry
                       '(:id "call-1"
                         :name "mcp__fixture__image"
                         :arguments nil))
                      :content)
                     "[Unsupported MCP content block: image]")))))

(ert-deftest e-mcp-test-helper-errors-surface-as-infrastructure-errors ()
  "Helper protocol errors are hard tool infrastructure failures."
  (e-mcp-test--with-transport
      (lambda (request)
        (pcase (plist-get request :op)
          ("list-tools"
           (list :ok t
                 :result (list :tools
                               (vector (e-mcp-test--fake-tool
                                        "boom"
                                        '(:type "object"))))))
          ("call-tool"
           '(:ok :json-false
             :error "helper failed"
             :diagnostics (:stderr "stack trace")))))
    (let* ((capability
            (e-capability-with-mcp-create
             :id 'fixture-mcp
             :mcp-servers (list (e-mcp-test--server))))
           (registry (e-tools-registry-create)))
      (e-capabilities-register-tools capability registry)
      (should-error
       (e-tools-execute registry
                        '(:id "call-1"
                          :name "mcp__fixture__boom"
                          :arguments nil))
       :type 'e-mcp-backend-error))))

(ert-deftest e-mcp-test-refresh-affects-next-registration ()
  "Explicit refresh asks the helper for fresh catalogs used later."
  (let ((catalog-version 0))
    (e-mcp-test--with-transport
        (lambda (request)
          (pcase (plist-get request :op)
            ("refresh"
             (setq catalog-version 1)
             '(:ok t :result (:refreshed t)))
            ("list-tools"
             (list :ok t
                   :result
                   (list :tools
                         (vector
                          (e-mcp-test--fake-tool
                           (if (= catalog-version 0) "before" "after")
                           '(:type "object"))))))))
      (let ((servers (list (e-mcp-test--server))))
        (should (equal (mapcar #'e-mcp-tool-name
                               (e-mcp-list-tools servers))
                       '("before")))
        (should (equal (e-mcp-refresh servers) '(:refreshed t)))
        (should (equal (mapcar #'e-mcp-tool-name
                               (e-mcp-list-tools servers))
                       '("after")))))))

(ert-deftest e-mcp-test-harness-smoke-registers-and-executes-generated-tool ()
  "An opt-in MCP capability contributes generated tools through the harness."
  (e-mcp-test--with-transport
      (lambda (request)
        (pcase (plist-get request :op)
          ("list-tools"
           (list :ok t
                 :result (list :tools
                               (vector (e-mcp-test--fake-tool
                                        "echo"
                                        e-mcp-test--schema
                                        "Echo text.")))))
          ("call-tool"
           (list :ok t
                 :result
                 (list :content
                       (vector (list :type "text"
                                     :text (plist-get
                                            (plist-get request :arguments)
                                            :text)))
                       :isError :json-false)))))
    (let* ((capability
            (e-capability-with-mcp-create
             :id 'fixture-mcp
             :mcp-servers (list (e-mcp-test--server))))
           (layer (e-layer-create
                   :id 'fixture-mcp-layer
                   :name "Fixture MCP"
                   :capabilities (list capability)))
           (harness (e-harness-create
                     :backend (e-backend-fake-create :items nil))))
      (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
      (let ((registry (e-harness-tools harness)))
        (should (equal (mapcar (lambda (definition)
                                 (plist-get definition :name))
                               (e-tools-definitions registry))
                       '("mcp__fixture__echo")))
        (should (equal (plist-get
                        (e-tools-execute
                         registry
                         '(:id "call-1"
                           :name "mcp__fixture__echo"
                           :arguments (:text "hello")))
                        :content)
                       "hello"))))))

(ert-deftest e-mcp-test-real-helper-discovers-and-calls-fixture-server ()
  "The Node helper can discover and call a stdio MCP fixture server."
  (skip-unless (executable-find "node"))
  (let* ((fixture (expand-file-name
                   "test/fixtures/e-mcp-fixture-server.mjs"
                   default-directory))
         (server (e-mcp-server-create
                  :id "fixture"
                  :command (list "node" fixture)
                  :timeout 2)))
    (unwind-protect
        (progn
          (e-mcp-reset)
          (should (member "echo"
                          (mapcar #'e-mcp-tool-name
                                  (e-mcp-list-tools (list server)))))
          (should (equal (e-mcp-call-tool
                          (list server)
                          "fixture"
                          "echo"
                          '(:text "from fixture"))
                         '(:content [(:type "text" :text "from fixture")]
                           :isError :json-false))))
      (e-mcp-reset))))

(ert-deftest e-mcp-test-real-helper-refresh-fetches-fresh-catalog ()
  "The Node helper refresh operation performs a fresh tools/list request."
  (skip-unless (executable-find "node"))
  (let* ((fixture (expand-file-name
                   "test/fixtures/e-mcp-fixture-server.mjs"
                   default-directory))
         (server (e-mcp-server-create
                  :id "fixture"
                  :command (list "node" fixture)
                  :timeout 2)))
    (unwind-protect
        (progn
          (e-mcp-reset)
          (should (equal
                   (mapcar (lambda (item) (plist-get item :name))
                           (append (plist-get (e-mcp-refresh (list server))
                                              :tools)
                                   nil))
                   '("echo" "structured" "fail"))))
      (e-mcp-reset))))

(ert-deftest e-mcp-test-real-helper-reports-stale-after-list-changed-notification ()
  "A tools/list_changed notification marks diagnostics stale until refresh."
  (skip-unless (executable-find "node"))
  (let* ((fixture (expand-file-name
                   "test/fixtures/e-mcp-stale-after-list-server.mjs"
                   default-directory))
         (server (e-mcp-server-create
                  :id "stale"
                  :command (list "node" fixture)
                  :timeout 2)))
    (unwind-protect
        (progn
          (e-mcp-reset)
          (should (equal (mapcar #'e-mcp-tool-name
                                 (e-mcp-list-tools (list server)))
                         '("echo")))
          (sleep-for 0.05)
          (should (equal
                   (e-mcp-call-tool (list server) "stale" "echo" nil)
                   '(:content [(:type "text" :text "ok")]
                     :isError :json-false)))
          (should (plist-get (e-mcp-diagnostics) :stale))
          (should (equal
                   (mapcar (lambda (item) (plist-get item :name))
                           (append (plist-get (e-mcp-refresh (list server))
                                              :tools)
                                   nil))
                   '("echo")))
          (should-not (e-mcp--truthy-p
                       (plist-get (plist-get (e-mcp-diagnostics) :stale)
                                  :stale))))
      (e-mcp-reset))))

(ert-deftest e-mcp-test-real-helper-respawns-exited-server ()
  "The Node helper starts a fresh MCP server after a clean server exit."
  (skip-unless (executable-find "node"))
  (let* ((fixture (expand-file-name
                   "test/fixtures/e-mcp-exit-after-list-server.mjs"
                   default-directory))
         (server (e-mcp-server-create
                  :id "exiting"
                  :command (list "node" fixture)
                  :timeout 1)))
    (unwind-protect
        (progn
          (e-mcp-reset)
          (should (equal (mapcar #'e-mcp-tool-name
                                 (e-mcp-list-tools (list server)))
                         '("once")))
          (sleep-for 0.05)
          (should (equal (mapcar #'e-mcp-tool-name
                                 (e-mcp-list-tools (list server)))
                         '("once"))))
      (e-mcp-reset))))

(defun e-mcp-test--start-http-fixture ()
  "Start the stateful HTTP MCP fixture and return (PROCESS . URL).
Blocks until the fixture prints its listening port."
  (let* ((fixture (expand-file-name
                   "test/fixtures/e-mcp-http-server.mjs"
                   default-directory))
         (buffer (generate-new-buffer " *e-mcp-http-fixture*"))
         (process (make-process
                   :name "e-mcp-http-fixture"
                   :buffer buffer
                   :command (list "node" fixture)
                   :connection-type 'pipe
                   :noquery t))
         (deadline (+ (float-time) 5))
         port)
    (while (and (not port)
                (process-live-p process)
                (< (float-time) deadline))
      (accept-process-output process 0.05)
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "^\\([0-9]+\\)$" nil t)
          (setq port (string-to-number (match-string 1))))))
    (unless port
      (kill-process process)
      (kill-buffer buffer)
      (error "HTTP MCP fixture did not report a port"))
    (cons process (format "http://127.0.0.1:%d/mcp" port))))

(ert-deftest e-mcp-test-http-transport-discovers-and-calls-stateful-server ()
  "HTTP transport captures the session id and reuses it across requests.
The fixture rejects any request that omits the `Mcp-Session-Id' header it
issued during initialize, so success proves the session id is captured and
echoed back on `tools/list' and `tools/call'."
  (skip-unless (executable-find "node"))
  (let* ((fixture (e-mcp-test--start-http-fixture))
         (process (car fixture))
         (url (cdr fixture))
         (server (e-mcp-server-create :id "http-fixture" :url url)))
    (unwind-protect
        (progn
          (e-mcp-reset)
          (should (equal (mapcar #'e-mcp-tool-name
                                 (e-mcp-list-tools (list server)))
                         '("echo")))
          (should (equal (e-mcp-call-tool
                          (list server)
                          "http-fixture"
                          "echo"
                          '(:text "over http"))
                         '(:content [(:type "text" :text "over http")]
                           :isError :json-false))))
      (e-mcp-reset)
      (when (process-live-p process)
        (kill-process process))
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process))))))

(defun e-mcp-test--progressive-transport ()
  "Return a fake helper transport exposing two tools for progressive tests."
  (lambda (request)
    (pcase (plist-get request :op)
      ("list-tools"
       (list :ok t
             :result (list :tools
                           (vector (e-mcp-test--fake-tool
                                    "echo" e-mcp-test--schema "Echo text.")
                                   (e-mcp-test--fake-tool
                                    "ping" '(:type "object") "Ping host.")))))
      ("call-tool"
       (list :ok t
             :result (list :content
                           (vector (list :type "text" :text "called"))
                           :isError :json-false))))))

(defmacro e-mcp-test--with-progressive-harness (harness-var &rest body)
  "Bind HARNESS-VAR to a harness with a progressive MCP layer and run BODY."
  (declare (indent 1))
  `(e-mcp-test--with-transport (e-mcp-test--progressive-transport)
     (let ((e-layer--registry (make-hash-table :test 'eq)))
       (e-layer-register
        (e-layer-spec-create
         :id 'fixture-mcp
         :name "Fixture MCP"
         :factory
         (lambda ()
           (e-layer-create
            :id 'fixture-mcp
            :name "Fixture MCP"
            :capabilities
            (list (e-capability-with-mcp-create
                   :id 'fixture-mcp
                   :name "Fixture MCP"
                   :mcp-servers (list (e-mcp-test--server))))))))
       (let ((,harness-var
              (e-harness-create
               :backend (e-backend-fake-create :items nil)
               :sessions (e-session-store-create))))
         (e-harness-enable-layer-id ,harness-var 'fixture-mcp)
         ,@body))))

(ert-deftest e-mcp-test-eager-mode-registers-every-tool ()
  "With progressive disabled (the default), all tool schemas are registered."
  (e-mcp-test--with-progressive-harness harness
    (e-harness-create-session harness :id "s1")
    (should (equal (sort (mapcar (lambda (d) (plist-get d :name))
                                 (e-tools-definitions
                                  (e-harness-tools harness "s1")))
                         #'string<)
                   '("mcp__fixture__echo" "mcp__fixture__ping")))))

(ert-deftest e-mcp-test-progressive-mode-registers-only-meta-tool ()
  "Progressive mode hides tool schemas behind mcp_activate until activated."
  (e-mcp-test--with-progressive-harness harness
    (e-harness-set-capability-config harness 'fixture-mcp '(:progressive t))
    (e-harness-create-session harness :id "s1")
    (let ((names (mapcar (lambda (d) (plist-get d :name))
                         (e-tools-definitions
                          (e-harness-tools harness "s1")))))
      (should (member "mcp_activate" names))
      (should-not (member "mcp__fixture__echo" names))
      (should-not (member "mcp__fixture__ping" names)))))

(ert-deftest e-mcp-test-progressive-mode-emits-capability-card ()
  "Progressive mode injects a single Tier-0 card naming the server tools."
  (e-mcp-test--with-progressive-harness harness
    (e-harness-set-capability-config harness 'fixture-mcp '(:progressive t))
    (e-harness-create-session harness :id "s1")
    (let* ((messages (plist-get (e-harness-context harness "s1") :messages))
           (card (cl-find-if
                  (lambda (m)
                    (and (stringp (plist-get m :content))
                         (string-match-p "MCP tool families"
                                         (plist-get m :content))))
                  messages)))
      (should card)
      (should (string-match-p "echo" (plist-get card :content)))
      (should (string-match-p "ping" (plist-get card :content))))))

(ert-deftest e-mcp-test-progressive-mode-registers-schema-resources ()
  "Progressive mode exposes per-tool schemas as on-demand e:// resources."
  (e-mcp-test--with-progressive-harness harness
    (e-harness-set-capability-config harness 'fixture-mcp '(:progressive t))
    (e-harness-create-session harness :id "s1")
    (let ((uris (mapcar #'e-store-entry-uri
                        (e-store-list (e-harness-store harness "s1")))))
      (should (member "e://fixture-mcp/mcp/fixture/tools" uris))
      (should (member "e://fixture-mcp/mcp/fixture/tools/echo" uris))
      (should (member "e://fixture-mcp/mcp/fixture/tools/ping" uris)))))

(ert-deftest e-mcp-test-eager-mode-emits-no-card-or-resources ()
  "Eager mode leaves context and resources free of progressive scaffolding."
  (e-mcp-test--with-progressive-harness harness
    (e-harness-create-session harness :id "s1")
    (should-not
     (cl-find-if
      (lambda (m)
        (and (stringp (plist-get m :content))
             (string-match-p "MCP tool families" (plist-get m :content))))
      (plist-get (e-harness-context harness "s1") :messages)))
    (should-not (e-store-list (e-harness-store harness "s1")))))

(ert-deftest e-mcp-test-activate-promotes-tool-into-active-set ()
  "mcp_activate records the named tool and makes it callable next turn."
  (e-mcp-test--with-progressive-harness harness
    (e-harness-set-capability-config harness 'fixture-mcp '(:progressive t))
    (e-harness-create-session harness :id "s1")
    (let* ((registry (e-harness-tools harness "s1"))
           (done nil)
           (result nil))
      (e-tools-start
       registry
       '(:id "c1" :name "mcp_activate"
         :arguments (:server "fixture" :tools ["echo"]))
       :context (list :harness harness :session-id "s1")
       :on-done (lambda (value) (setq result value done t))
       :on-error (lambda (err) (setq result err done t)))
      (while (not done) (accept-process-output nil 0.01))
      (should (eq (plist-get result :status) 'ok))
      (should (string-match-p "# mcp__fixture__echo"
                              (e-tools-result-content-text
                               (plist-get result :content)))))
    (should (equal (e-mcp--active-set harness "s1") '(("fixture" "echo"))))
    (let ((names (mapcar (lambda (d) (plist-get d :name))
                         (e-tools-definitions
                          (e-harness-tools harness "s1")))))
      (should (member "mcp__fixture__echo" names))
      (should-not (member "mcp__fixture__ping" names)))))

(ert-deftest e-mcp-test-activate-without-tools-activates-whole-server ()
  "mcp_activate with no tool list activates every tool on the server."
  (e-mcp-test--with-progressive-harness harness
    (e-harness-set-capability-config harness 'fixture-mcp '(:progressive t))
    (e-harness-create-session harness :id "s1")
    (e-mcp--activate harness "s1" "fixture" nil)
    (should (equal (e-mcp--active-set harness "s1") '(("fixture" . t))))
    (should (equal (sort (mapcar (lambda (d) (plist-get d :name))
                                 (cl-remove-if-not
                                  (lambda (d)
                                    (string-prefix-p "mcp__"
                                                     (plist-get d :name)))
                                  (e-tools-definitions
                                   (e-harness-tools harness "s1"))))
                         #'string<)
                   '("mcp__fixture__echo" "mcp__fixture__ping")))))

(ert-deftest e-mcp-test-list-tools-memoizes-catalog ()
  "`e-mcp-list-tools' caches catalogs and `e-mcp-refresh' invalidates them."
  (let ((calls 0))
    (e-mcp-test--with-transport
        (lambda (request)
          (when (equal (plist-get request :op) "list-tools")
            (setq calls (1+ calls)))
          (list :ok t
                :result (list :tools
                              (vector (e-mcp-test--fake-tool
                                       "echo" '(:type "object"))))))
      (let ((servers (list (e-mcp-test--server))))
        (e-mcp-list-tools servers)
        (e-mcp-list-tools servers)
        (should (= calls 1))
        (e-mcp-refresh servers)
        ;; Refresh invalidates the cache; the next list re-fetches once more.
        (e-mcp-list-tools servers)
        (should (= calls 2))))))

(defun e-mcp-test--broken-server ()
  "Return a second stdio MCP server whose discovery fails."
  (e-mcp-server-create :id "broken" :command '("node" "broken.mjs")))

(ert-deftest e-mcp-test-catalogs-safe-skips-failing-servers ()
  "A server that fails discovery is logged and omitted, others survive."
  (e-mcp-test--with-transport
      (lambda (request)
        (let ((server-id (plist-get (aref (plist-get request :servers) 0) :id)))
          (if (equal server-id "broken")
              (list :ok :json-false :error "Bad url: 'https://example/mcp'")
            (list :ok t
                  :result (list :tools
                                (vector (e-mcp-test--fake-tool
                                         "echo" '(:type "object"))))))))
    (let* ((servers (list (e-mcp-test--server) (e-mcp-test--broken-server)))
           (warnings nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest args) (push args warnings))))
        ;; Strict discovery still propagates the broken server's error.
        (should-error (e-mcp-list-tools (list (e-mcp-test--broken-server)))
                      :type 'e-mcp-backend-error)
        (let ((catalogs (e-mcp--catalogs-safe servers)))
          (should (= (length catalogs) 1))
          (should (equal (e-mcp-server-id (caar catalogs)) "fixture")))
        (should (= (length (e-mcp--tools-safe servers)) 1))
        (should warnings)))))

(provide 'e-mcp-test)

;;; e-mcp-test.el ends here
