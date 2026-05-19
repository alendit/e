# Core Functionality Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the first real, testable core harness for `e`: lifecycle requests, durable in-memory sessions, event emission, backend dispatch, tool-call handling, and provider-independent contracts.

**Architecture:** Keep core policy in pure harness modules under `lisp/`, with no presentation buffers, keymaps, window state, provider auth, or concrete Emacs side effects. The first implementation uses fake and in-memory adapters so lifecycle, event ordering, queue handling, session writes, tool dispatch, and backend independence are proven before OpenAI or UI work lands.

**Tech Stack:** Emacs Lisp with lexical binding, `cl-lib`, plists for public data, ERT tests, Eldev for test/lint/compile/package checks.

---

## File Structure

- Create `lisp/e-events.el`: event constructors and event validation helpers. Owns the stable event shape consumed by harness clients and presentation shells.
- Create `lisp/e-session.el`: in-memory session repository and message append/read APIs. Owns transcript state, metadata, and current branch cursor fields reserved for future durable stores.
- Create `lisp/e-backend.el`: backend-neutral adapter contract plus a fake backend for tests. Owns request normalization and streaming callback conventions, not provider auth.
- Create `lisp/e-tools.el`: pure tool registry, tool-call validation, and structured tool result creation. Owns tool lookup and error reporting around tool contracts, not concrete side effects.
- Create `lisp/e-loop.el`: turn execution loop. Owns backend streaming, assistant deltas, tool-call handling, stop conditions, and lifecycle events.
- Create `lisp/e-harness.el`: public core harness service. Owns lifecycle operations, queue state, session coordination, loop invocation, event subscription, and settled state.
- Modify `lisp/e-core.el`: require the core modules and keep `e-core-status` as a compatibility scaffold over real harness readiness.
- Modify `e.el`: require only `e-core` as the package entry point and expose public commands later through presentation modules, not the core harness.
- Modify `lisp/e-dev.el`: reload new core files in dependency order.
- Create tests under `test/`: `e-events-test.el`, `e-session-test.el`, `e-backend-test.el`, `e-tools-test.el`, `e-loop-test.el`, and `e-harness-test.el`.
- Modify `test/e-test.el`: keep package smoke tests and add only public package surface assertions.

## Stable Data Shapes

Use plists for public core data because they are simple to inspect in Emacs, easy to persist later, and do not force a speculative object hierarchy.

Message shape:

```elisp
(:id "msg-1"
 :role user
 :content "Inspect this buffer"
 :created-at 0
 :metadata nil)
```

Event shape:

```elisp
(:id "evt-1"
 :type message-added
 :session-id "session-1"
 :turn-id "turn-1"
 :payload (:message-id "msg-1")
 :created-at 0)
```

Backend stream item shape:

```elisp
(:type assistant-delta :content "text")
(:type assistant-message :content "final text")
(:type tool-call :id "call-1" :name "buffer-read" :arguments (:buffer "*scratch*"))
(:type done :reason stop)
```

Tool result shape:

```elisp
(:tool-call-id "call-1"
 :name "buffer-read"
 :status ok
 :content "buffer contents"
 :metadata nil)
```

## Task 1: Events

**Files:**

- Create: `lisp/e-events.el`
- Create: `test/e-events-test.el`
- Modify: `lisp/e-dev.el`

- [ ] **Step 1: Write failing event tests**

```elisp
;;; e-events-test.el --- Tests for e events -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-events)

(ert-deftest e-events-test-make-event ()
  (let ((event (e-events-make
                :id "evt-1"
                :type 'turn-started
                :session-id "session-1"
                :turn-id "turn-1"
                :payload '(:prompt "hello")
                :created-at 10)))
    (should (equal (plist-get event :id) "evt-1"))
    (should (equal (plist-get event :type) 'turn-started))
    (should (equal (plist-get event :session-id) "session-1"))
    (should (equal (plist-get event :turn-id) "turn-1"))
    (should (equal (plist-get event :payload) '(:prompt "hello")))
    (should (equal (plist-get event :created-at) 10))))

(ert-deftest e-events-test-rejects-missing-type ()
  (should-error
   (e-events-make :id "evt-1" :session-id "session-1")
   :type 'wrong-type-argument))

(provide 'e-events-test)
```

- [ ] **Step 2: Run the focused failing test**

Run:

```sh
rtk eldev test test/e-events-test.el
```

Expected: FAIL because `e-events` does not exist.

- [ ] **Step 3: Implement event construction**

```elisp
;;; e-events.el --- Core event helpers for e -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar e-events--counter 0
  "Monotonic event id counter for in-process events.")

(defun e-events-next-id ()
  "Return a new in-process event id."
  (setq e-events--counter (1+ e-events--counter))
  (format "evt-%d" e-events--counter))

(cl-defun e-events-make (&key id type session-id turn-id payload created-at)
  "Create a core event plist.
TYPE and SESSION-ID are required.  ID and CREATED-AT default to in-process
values so tests can inject deterministic values without changing the public
shape."
  (unless type
    (signal 'wrong-type-argument '(e-event-type nil)))
  (unless session-id
    (signal 'wrong-type-argument '(e-event-session-id nil)))
  (list :id (or id (e-events-next-id))
        :type type
        :session-id session-id
        :turn-id turn-id
        :payload payload
        :created-at (or created-at (float-time))))

(defun e-events-type (event)
  "Return EVENT's type."
  (plist-get event :type))

(provide 'e-events)
```

- [ ] **Step 4: Add reload ordering**

Update `lisp/e-dev.el` so `files` starts with:

```elisp
'("lisp/e-events.el" "lisp/e-core.el" "e.el" "lisp/e-dev.el")
```

- [ ] **Step 5: Verify events**

Run:

```sh
rtk eldev test test/e-events-test.el
```

Expected: PASS.

- [ ] **Step 6: Commit**

```sh
git add lisp/e-events.el lisp/e-dev.el test/e-events-test.el
git commit -m "feat: add core event helpers" -m "Co-authored-by: Codex <noreply@openai.com>"
```

## Task 2: In-Memory Sessions

**Files:**

- Create: `lisp/e-session.el`
- Create: `test/e-session-test.el`
- Modify: `lisp/e-dev.el`

- [ ] **Step 1: Write failing session tests**

```elisp
;;; e-session-test.el --- Tests for e sessions -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-session)

(ert-deftest e-session-test-create-and-read ()
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1" :metadata '(:model "fake"))
    (should (equal (plist-get (e-session-get store "session-1") :id) "session-1"))
    (should (equal (e-session-messages store "session-1") nil))))

(ert-deftest e-session-test-append-message-preserves-order ()
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1"
                              '(:id "msg-1" :role user :content "hello"))
    (e-session-append-message store "session-1"
                              '(:id "msg-2" :role assistant :content "hi"))
    (should (equal (mapcar (lambda (message) (plist-get message :id))
                           (e-session-messages store "session-1"))
                   '("msg-1" "msg-2")))))

(ert-deftest e-session-test-missing-session-surfaces-error ()
  (let ((store (e-session-store-create)))
    (should-error
     (e-session-append-message store "missing" '(:role user :content "x"))
     :type 'e-session-missing)))

(provide 'e-session-test)
```

- [ ] **Step 2: Run the focused failing test**

Run:

```sh
rtk eldev test test/e-session-test.el
```

Expected: FAIL because `e-session` does not exist.

- [ ] **Step 3: Implement in-memory session store**

```elisp
;;; e-session.el --- Session store for e core -*- lexical-binding: t; -*-

(require 'cl-lib)

(define-error 'e-session-missing "Session does not exist")
(define-error 'e-session-duplicate "Session already exists")

(cl-defstruct (e-session-store (:constructor e-session-store-create))
  (sessions (make-hash-table :test 'equal)))

(cl-defun e-session-create (store &key id metadata)
  "Create a session in STORE with ID and METADATA."
  (when (gethash id (e-session-store-sessions store))
    (signal 'e-session-duplicate (list id)))
  (let ((session (list :id id
                       :metadata metadata
                       :messages nil
                       :current-branch nil
                       :compactions nil)))
    (puthash id session (e-session-store-sessions store))
    session))

(defun e-session-get (store session-id)
  "Return SESSION-ID from STORE."
  (or (gethash session-id (e-session-store-sessions store))
      (signal 'e-session-missing (list session-id))))

(defun e-session-messages (store session-id)
  "Return messages for SESSION-ID in insertion order."
  (copy-sequence (plist-get (e-session-get store session-id) :messages)))

(defun e-session-append-message (store session-id message)
  "Append MESSAGE to SESSION-ID in STORE."
  (let* ((session (e-session-get store session-id))
         (messages (plist-get session :messages)))
    (plist-put session :messages (append messages (list message)))
    message))

(provide 'e-session)
```

- [ ] **Step 4: Add reload ordering**

Update `lisp/e-dev.el` so `files` starts with:

```elisp
'("lisp/e-events.el" "lisp/e-session.el" "lisp/e-core.el" "e.el" "lisp/e-dev.el")
```

- [ ] **Step 5: Verify sessions**

Run:

```sh
rtk eldev test test/e-session-test.el
```

Expected: PASS.

- [ ] **Step 6: Commit**

```sh
git add lisp/e-session.el lisp/e-dev.el test/e-session-test.el
git commit -m "feat: add in-memory session store" -m "Co-authored-by: Codex <noreply@openai.com>"
```

## Task 3: Backend Contract And Fake Backend

**Files:**

- Create: `lisp/e-backend.el`
- Create: `test/e-backend-test.el`
- Modify: `lisp/e-dev.el`

- [ ] **Step 1: Write failing backend tests**

```elisp
;;; e-backend-test.el --- Tests for e backends -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-backend)

(ert-deftest e-backend-test-fake-streams-items ()
  (let ((backend (e-backend-fake-create
                  :name "fake"
                  :items '((:type assistant-delta :content "hi")
                           (:type done :reason stop))))
        (seen nil))
    (e-backend-stream backend
                      :messages '((:role user :content "hello"))
                      :options '(:model "fake")
                      :on-item (lambda (item) (push item seen)))
    (should (equal (nreverse seen)
                   '((:type assistant-delta :content "hi")
                     (:type done :reason stop))))))

(ert-deftest e-backend-test-rejects-missing-streamer ()
  (let ((backend (e-backend-create :name "bad" :stream nil)))
    (should-error
     (e-backend-stream backend
                       :messages nil
                       :options nil
                       :on-item #'ignore)
     :type 'wrong-type-argument)))

(provide 'e-backend-test)
```

- [ ] **Step 2: Run the focused failing test**

Run:

```sh
rtk eldev test test/e-backend-test.el
```

Expected: FAIL because `e-backend` does not exist.

- [ ] **Step 3: Implement backend contract**

```elisp
;;; e-backend.el --- Backend contract for e core -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (e-backend (:constructor e-backend-create))
  name
  stream)

(cl-defun e-backend-stream (backend &key messages options on-item)
  "Stream a backend turn through BACKEND.
MESSAGES and OPTIONS are backend-neutral plists/lists.  ON-ITEM receives
backend-neutral stream items."
  (unless (functionp (e-backend-stream backend))
    (signal 'wrong-type-argument (list 'functionp (e-backend-stream backend))))
  (funcall (e-backend-stream backend)
           :messages messages
           :options options
           :on-item on-item))

(cl-defun e-backend-fake-create (&key name items)
  "Create a fake backend that streams ITEMS synchronously."
  (e-backend-create
   :name (or name "fake")
   :stream (lambda (&key messages options on-item)
             (ignore messages options)
             (dolist (item items)
               (funcall on-item item)))))

(provide 'e-backend)
```

- [ ] **Step 4: Add reload ordering**

Update `lisp/e-dev.el` so `files` includes:

```elisp
"lisp/e-backend.el"
```

before `lisp/e-core.el`.

- [ ] **Step 5: Verify backend contract**

Run:

```sh
rtk eldev test test/e-backend-test.el
```

Expected: PASS.

- [ ] **Step 6: Commit**

```sh
git add lisp/e-backend.el lisp/e-dev.el test/e-backend-test.el
git commit -m "feat: add backend adapter contract" -m "Co-authored-by: Codex <noreply@openai.com>"
```

## Task 4: Tool Registry

**Files:**

- Create: `lisp/e-tools.el`
- Create: `test/e-tools-test.el`
- Modify: `lisp/e-dev.el`

- [ ] **Step 1: Write failing tool tests**

```elisp
;;; e-tools-test.el --- Tests for e tool registry -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-tools)

(ert-deftest e-tools-test-register-and-execute ()
  (let ((registry (e-tools-registry-create)))
    (e-tools-register registry
                      :name "echo"
                      :description "Return the input text."
                      :handler (lambda (arguments)
                                 (plist-get arguments :text)))
    (should (equal (e-tools-execute registry
                                    '(:id "call-1" :name "echo" :arguments (:text "hi")))
                   '(:tool-call-id "call-1"
                     :name "echo"
                     :status ok
                     :content "hi"
                     :metadata nil)))))

(ert-deftest e-tools-test-missing-tool-returns-structured-error ()
  (let ((registry (e-tools-registry-create)))
    (should (equal (e-tools-execute registry
                                    '(:id "call-1" :name "missing" :arguments nil))
                   '(:tool-call-id "call-1"
                     :name "missing"
                     :status error
                     :content "Unknown tool: missing"
                     :metadata (:error e-tool-missing))))))

(provide 'e-tools-test)
```

- [ ] **Step 2: Run the focused failing test**

Run:

```sh
rtk eldev test test/e-tools-test.el
```

Expected: FAIL because `e-tools` does not exist.

- [ ] **Step 3: Implement pure tool registry**

```elisp
;;; e-tools.el --- Tool registry for e core -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (e-tools-registry (:constructor e-tools-registry-create))
  (tools (make-hash-table :test 'equal)))

(cl-defun e-tools-register (registry &key name description handler)
  "Register a tool in REGISTRY."
  (unless (functionp handler)
    (signal 'wrong-type-argument (list 'functionp handler)))
  (puthash name
           (list :name name :description description :handler handler)
           (e-tools-registry-tools registry)))

(defun e-tools--result (call status content &optional metadata)
  "Return a structured tool result for CALL."
  (list :tool-call-id (plist-get call :id)
        :name (plist-get call :name)
        :status status
        :content content
        :metadata metadata))

(defun e-tools-execute (registry call)
  "Execute CALL against REGISTRY and return a structured tool result."
  (let* ((name (plist-get call :name))
         (tool (gethash name (e-tools-registry-tools registry))))
    (if (not tool)
        (e-tools--result call
                         'error
                         (format "Unknown tool: %s" name)
                         '(:error e-tool-missing))
      (condition-case err
          (e-tools--result call
                           'ok
                           (funcall (plist-get tool :handler)
                                    (plist-get call :arguments))
                           nil)
        (error
         (e-tools--result call
                          'error
                          (error-message-string err)
                          (list :error (car err))))))))

(provide 'e-tools)
```

- [ ] **Step 4: Add reload ordering**

Update `lisp/e-dev.el` so `files` includes:

```elisp
"lisp/e-tools.el"
```

before `lisp/e-core.el`.

- [ ] **Step 5: Verify tools**

Run:

```sh
rtk eldev test test/e-tools-test.el
```

Expected: PASS.

- [ ] **Step 6: Commit**

```sh
git add lisp/e-tools.el lisp/e-dev.el test/e-tools-test.el
git commit -m "feat: add core tool registry" -m "Co-authored-by: Codex <noreply@openai.com>"
```

## Task 5: Agent Loop

**Files:**

- Create: `lisp/e-loop.el`
- Create: `test/e-loop-test.el`
- Modify: `lisp/e-dev.el`

- [ ] **Step 1: Write failing loop tests**

```elisp
;;; e-loop-test.el --- Tests for e agent loop -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-backend)
(require 'e-loop)
(require 'e-tools)

(ert-deftest e-loop-test-persists-assistant-message ()
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-delta :content "hello")
                            (:type assistant-message :content "hello")
                            (:type done :reason stop))))
         (events nil)
         (messages nil)
         (result (e-loop-run-turn
                  :session-id "session-1"
                  :turn-id "turn-1"
                  :messages '((:role user :content "hi"))
                  :backend backend
                  :tools (e-tools-registry-create)
                  :options '(:model "fake")
                  :on-event (lambda (event) (push event events))
                  :append-message (lambda (message) (push message messages)))))
    (should (equal (plist-get result :status) 'done))
    (should (equal (plist-get (car messages) :role) 'assistant))
    (should (equal (plist-get (car messages) :content) "hello"))
    (should (member 'turn-started (mapcar (lambda (event) (plist-get event :type)) events)))
    (should (member 'turn-finished (mapcar (lambda (event) (plist-get event :type)) events)))))

(ert-deftest e-loop-test-executes-tool-call-and-appends-result ()
  (let* ((backend (e-backend-fake-create
                   :items '((:type tool-call :id "call-1" :name "echo" :arguments (:text "hi"))
                            (:type done :reason stop))))
         (tools (e-tools-registry-create))
         (messages nil))
    (e-tools-register tools
                      :name "echo"
                      :description "Echo text."
                      :handler (lambda (arguments) (plist-get arguments :text)))
    (e-loop-run-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools tools
     :options nil
     :on-event #'ignore
     :append-message (lambda (message) (push message messages)))
    (should (equal (plist-get (car messages) :role) 'tool))
    (should (equal (plist-get (plist-get (car messages) :content) :status) 'ok))))

(provide 'e-loop-test)
```

- [ ] **Step 2: Run the focused failing test**

Run:

```sh
rtk eldev test test/e-loop-test.el
```

Expected: FAIL because `e-loop` does not exist.

- [ ] **Step 3: Implement synchronous turn loop**

```elisp
;;; e-loop.el --- Agent turn loop for e core -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'e-backend)
(require 'e-events)
(require 'e-tools)

(defvar e-loop--message-counter 0
  "Monotonic message id counter for loop-created messages.")

(defun e-loop--next-message-id ()
  "Return a new in-process message id."
  (setq e-loop--message-counter (1+ e-loop--message-counter))
  (format "msg-%d" e-loop--message-counter))

(cl-defun e-loop--emit (&key on-event session-id turn-id type payload)
  "Emit a loop event through ON-EVENT."
  (funcall on-event
           (e-events-make :type type
                          :session-id session-id
                          :turn-id turn-id
                          :payload payload)))

(cl-defun e-loop-run-turn
    (&key session-id turn-id messages backend tools options on-event append-message)
  "Run one agent turn.
The loop is synchronous for the first core implementation.  Async process
management stays outside this task until the core event and state semantics are
stable."
  (let ((assistant-content nil)
        (done-reason nil))
    (e-loop--emit :on-event on-event
                  :session-id session-id
                  :turn-id turn-id
                  :type 'turn-started
                  :payload nil)
    (e-backend-stream
     backend
     :messages messages
     :options options
     :on-item
     (lambda (item)
       (pcase (plist-get item :type)
         ('assistant-delta
          (setq assistant-content
                (concat assistant-content (plist-get item :content)))
          (e-loop--emit :on-event on-event
                        :session-id session-id
                        :turn-id turn-id
                        :type 'assistant-delta
                        :payload item))
         ('assistant-message
          (let ((message (list :id (e-loop--next-message-id)
                               :role 'assistant
                               :content (plist-get item :content)
                               :metadata nil)))
            (funcall append-message message)
            (e-loop--emit :on-event on-event
                          :session-id session-id
                          :turn-id turn-id
                          :type 'message-added
                          :payload (list :message message))))
         ('tool-call
          (let* ((result (e-tools-execute tools item))
                 (message (list :id (e-loop--next-message-id)
                                :role 'tool
                                :content result
                                :metadata nil)))
            (funcall append-message message)
            (e-loop--emit :on-event on-event
                          :session-id session-id
                          :turn-id turn-id
                          :type 'tool-finished
                          :payload (list :result result))))
         ('done
          (setq done-reason (plist-get item :reason)))
         (_
          (e-loop--emit :on-event on-event
                        :session-id session-id
                        :turn-id turn-id
                        :type 'backend-item-ignored
                        :payload item)))))
    (e-loop--emit :on-event on-event
                  :session-id session-id
                  :turn-id turn-id
                  :type 'turn-finished
                  :payload (list :reason done-reason))
    (list :status 'done
          :reason done-reason
          :assistant-content assistant-content)))

(provide 'e-loop)
```

- [ ] **Step 4: Add reload ordering**

Update `lisp/e-dev.el` so `files` includes:

```elisp
"lisp/e-loop.el"
```

after `lisp/e-tools.el` and before `lisp/e-core.el`.

- [ ] **Step 5: Verify loop behavior**

Run:

```sh
rtk eldev test test/e-loop-test.el
```

Expected: PASS.

- [ ] **Step 6: Commit**

```sh
git add lisp/e-loop.el lisp/e-dev.el test/e-loop-test.el
git commit -m "feat: add synchronous core agent loop" -m "Co-authored-by: Codex <noreply@openai.com>"
```

## Task 6: Harness Service

**Files:**

- Create: `lisp/e-harness.el`
- Create: `test/e-harness-test.el`
- Modify: `lisp/e-dev.el`

- [ ] **Step 1: Write failing harness tests**

```elisp
;;; e-harness-test.el --- Tests for e harness service -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-backend)
(require 'e-harness)

(ert-deftest e-harness-test-prompt-writes-user-and-assistant-messages ()
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (let ((messages (e-harness-messages harness "session-1")))
      (should (equal (mapcar (lambda (message) (plist-get message :role)) messages)
                     '(user assistant)))
      (should (equal (plist-get (cadr messages) :content) "answer")))
    (should (member 'turn-started (mapcar (lambda (event) (plist-get event :type)) events)))))

(ert-deftest e-harness-test-abort-idle-session-is-explicit-error ()
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-harness-abort harness "session-1")
     :type 'e-harness-no-active-turn)))

(provide 'e-harness-test)
```

- [ ] **Step 2: Run the focused failing test**

Run:

```sh
rtk eldev test test/e-harness-test.el
```

Expected: FAIL because `e-harness` does not exist.

- [ ] **Step 3: Implement harness service**

```elisp
;;; e-harness.el --- Core harness service for e -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'e-events)
(require 'e-loop)
(require 'e-session)
(require 'e-tools)

(define-error 'e-harness-no-active-turn "No active turn")

(cl-defstruct (e-harness (:constructor e-harness--make))
  backend
  (sessions (e-session-store-create))
  (tools (e-tools-registry-create))
  (subscribers nil)
  active-turns)

(defvar e-harness--turn-counter 0
  "Monotonic turn id counter.")

(defvar e-harness--message-counter 0
  "Monotonic harness message id counter.")

(defun e-harness--next-turn-id ()
  "Return a new in-process turn id."
  (setq e-harness--turn-counter (1+ e-harness--turn-counter))
  (format "turn-%d" e-harness--turn-counter))

(defun e-harness--next-message-id ()
  "Return a new in-process harness message id."
  (setq e-harness--message-counter (1+ e-harness--message-counter))
  (format "msg-user-%d" e-harness--message-counter))

(cl-defun e-harness-create (&key backend sessions tools)
  "Create a core harness."
  (e-harness--make :backend backend
                   :sessions (or sessions (e-session-store-create))
                   :tools (or tools (e-tools-registry-create))
                   :active-turns (make-hash-table :test 'equal)))

(cl-defun e-harness-create-session (harness &key id metadata)
  "Create a session in HARNESS."
  (e-session-create (e-harness-sessions harness)
                    :id id
                    :metadata metadata))

(defun e-harness-subscribe (harness subscriber)
  "Register SUBSCRIBER for core events."
  (push subscriber (e-harness-subscribers harness))
  subscriber)

(defun e-harness--emit (harness event)
  "Emit EVENT to HARNESS subscribers."
  (dolist (subscriber (reverse (e-harness-subscribers harness)))
    (funcall subscriber event)))

(defun e-harness-messages (harness session-id)
  "Return messages for SESSION-ID."
  (e-session-messages (e-harness-sessions harness) session-id))

(defun e-harness-prompt (harness session-id prompt)
  "Append PROMPT and run one backend turn for SESSION-ID."
  (let* ((turn-id (e-harness--next-turn-id))
         (user-message (list :id (e-harness--next-message-id)
                             :role 'user
                             :content prompt
                             :metadata nil)))
    (puthash session-id turn-id (e-harness-active-turns harness))
    (unwind-protect
        (progn
          (e-session-append-message (e-harness-sessions harness)
                                    session-id
                                    user-message)
          (e-harness--emit
           harness
           (e-events-make :type 'message-added
                          :session-id session-id
                          :turn-id turn-id
                          :payload (list :message user-message)))
          (e-loop-run-turn
           :session-id session-id
           :turn-id turn-id
           :messages (e-session-messages (e-harness-sessions harness) session-id)
           :backend (e-harness-backend harness)
           :tools (e-harness-tools harness)
           :options nil
           :on-event (lambda (event) (e-harness--emit harness event))
           :append-message
           (lambda (message)
             (e-session-append-message (e-harness-sessions harness)
                                       session-id
                                       message))))
      (remhash session-id (e-harness-active-turns harness)))))

(defun e-harness-abort (harness session-id)
  "Abort the active turn for SESSION-ID.
The synchronous first implementation can only surface that no turn is active
after `e-harness-prompt' settles.  Async cancellation belongs to the later
process/queue package."
  (unless (gethash session-id (e-harness-active-turns harness))
    (signal 'e-harness-no-active-turn (list session-id))))

(provide 'e-harness)
```

- [ ] **Step 4: Add reload ordering**

Update `lisp/e-dev.el` so `files` includes:

```elisp
"lisp/e-harness.el"
```

after `lisp/e-loop.el` and before `lisp/e-core.el`.

- [ ] **Step 5: Verify harness behavior**

Run:

```sh
rtk eldev test test/e-harness-test.el
```

Expected: PASS.

- [ ] **Step 6: Commit**

```sh
git add lisp/e-harness.el lisp/e-dev.el test/e-harness-test.el
git commit -m "feat: add core harness service" -m "Co-authored-by: Codex <noreply@openai.com>"
```

## Task 7: Core Entry Point Integration

**Files:**

- Modify: `lisp/e-core.el`
- Modify: `test/e-test.el`
- Modify: `lisp/e-dev.el`

- [ ] **Step 1: Write failing integration tests**

Extend `test/e-test.el`:

```elisp
(ert-deftest e-test-exposes-core-harness-api ()
  "The package exposes the core harness API after requiring e."
  (require 'e)
  (should (fboundp 'e-harness-create))
  (should (fboundp 'e-harness-prompt))
  (should (fboundp 'e-harness-messages)))
```

- [ ] **Step 2: Run the focused failing integration test**

Run:

```sh
rtk eldev test test/e-test.el
```

Expected: FAIL until `e-core.el` requires the new core modules.

- [ ] **Step 3: Wire core modules through `e-core.el`**

Replace `lisp/e-core.el` with:

```elisp
;;; e-core.el --- Core runtime for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure core runtime for e.  This module must stay independent from
;; presentation buffers, keymaps, provider adapters, and concrete side effects.

;;; Code:

(require 'e-backend)
(require 'e-events)
(require 'e-harness)
(require 'e-loop)
(require 'e-session)
(require 'e-tools)

(defconst e-core-scaffold-state 'ready
  "Minimal state marker for the core runtime scaffold.")

(defun e-core-status ()
  "Return a plist describing the current core state."
  (list :state e-core-scaffold-state
        :events t
        :sessions t
        :backends t
        :tools t
        :loop t
        :harness t))

(provide 'e-core)

;;; e-core.el ends here
```

- [ ] **Step 4: Verify package smoke tests**

Run:

```sh
rtk eldev test test/e-test.el
```

Expected: PASS.

- [ ] **Step 5: Verify all tests**

Run:

```sh
rtk eldev test
```

Expected: PASS.

- [ ] **Step 6: Commit**

```sh
git add lisp/e-core.el lisp/e-dev.el test/e-test.el
git commit -m "feat: expose core harness surface" -m "Co-authored-by: Codex <noreply@openai.com>"
```

## Task 8: Queue And Follow-Up Semantics

**Files:**

- Modify: `lisp/e-harness.el`
- Modify: `test/e-harness-test.el`

- [ ] **Step 1: Write failing queue tests**

Add to `test/e-harness-test.el`:

```elisp
(ert-deftest e-harness-test-follow-up-appends-user-message ()
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "first")
    (e-harness-follow-up harness "session-1" "second")
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant user assistant)))))
```

- [ ] **Step 2: Run the focused failing test**

Run:

```sh
rtk eldev test test/e-harness-test.el
```

Expected: FAIL because `e-harness-follow-up` does not exist.

- [ ] **Step 3: Implement follow-up as lifecycle alias**

Add to `lisp/e-harness.el`:

```elisp
(defun e-harness-follow-up (harness session-id prompt)
  "Submit PROMPT as the next turn for SESSION-ID."
  (e-harness-prompt harness session-id prompt))
```

This keeps initial queue behavior synchronous and explicit. A later async queue package can replace the internal scheduling without changing the public lifecycle name.

- [ ] **Step 4: Verify harness queue semantics**

Run:

```sh
rtk eldev test test/e-harness-test.el
```

Expected: PASS.

- [ ] **Step 5: Commit**

```sh
git add lisp/e-harness.el test/e-harness-test.el
git commit -m "feat: add harness follow-up lifecycle" -m "Co-authored-by: Codex <noreply@openai.com>"
```

## Task 9: Continue, Reset, And State Access

**Files:**

- Modify: `lisp/e-harness.el`
- Modify: `test/e-harness-test.el`

- [ ] **Step 1: Write failing lifecycle tests**

Add to `test/e-harness-test.el`:

```elisp
(ert-deftest e-harness-test-reset-clears-session-messages ()
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (e-harness-reset harness "session-1")
    (should (equal (e-harness-messages harness "session-1") nil))))

(ert-deftest e-harness-test-state-reports-session-and-active-turn ()
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (should (equal (e-harness-state harness "session-1")
                   '(:session-id "session-1" :active-turn nil :message-count 0)))))
```

- [ ] **Step 2: Run the focused failing test**

Run:

```sh
rtk eldev test test/e-harness-test.el
```

Expected: FAIL because `e-harness-reset` and `e-harness-state` do not exist.

- [ ] **Step 3: Add session reset to `e-session.el`**

```elisp
(defun e-session-clear-messages (store session-id)
  "Clear all messages for SESSION-ID."
  (let ((session (e-session-get store session-id)))
    (plist-put session :messages nil)
    session))
```

- [ ] **Step 4: Add reset and state to `e-harness.el`**

```elisp
(defun e-harness-reset (harness session-id)
  "Clear SESSION-ID transcript state."
  (e-session-clear-messages (e-harness-sessions harness) session-id)
  (e-harness--emit
   harness
   (e-events-make :type 'session-reset
                  :session-id session-id
                  :turn-id nil
                  :payload nil)))

(defun e-harness-state (harness session-id)
  "Return settled state for SESSION-ID."
  (list :session-id session-id
        :active-turn (gethash session-id (e-harness-active-turns harness))
        :message-count (length (e-harness-messages harness session-id))))
```

- [ ] **Step 5: Verify lifecycle behavior**

Run:

```sh
rtk eldev test test/e-harness-test.el test/e-session-test.el
```

Expected: PASS.

- [ ] **Step 6: Commit**

```sh
git add lisp/e-harness.el lisp/e-session.el test/e-harness-test.el test/e-session-test.el
git commit -m "feat: add core reset and state access" -m "Co-authored-by: Codex <noreply@openai.com>"
```

## Task 10: Validation And Package Checks

**Files:**

- Modify only files needed to fix failures from the commands below.

- [ ] **Step 1: Run all ERT tests**

Run:

```sh
rtk eldev test
```

Expected: PASS.

- [ ] **Step 2: Run lint**

Run:

```sh
rtk eldev lint
```

Expected: PASS. Fix only actionable lint findings in touched files.

- [ ] **Step 3: Run byte compilation**

Run:

```sh
rtk eldev compile
```

Expected: PASS.

- [ ] **Step 4: Verify package load path**

Run:

```sh
rtk emacs -Q --batch -L . -L lisp -l e.el --eval "(progn (require 'e) (princ (e-status)))"
```

Expected output includes:

```text
e 0.1.0 loaded
```

- [ ] **Step 5: Commit validation fixes**

If any files changed during validation:

```sh
git add lisp/*.el test/*.el
git commit -m "test: validate core harness package" -m "Co-authored-by: Codex <noreply@openai.com>"
```

If no files changed, do not create an empty commit.

## Deferred Work

The following work is intentionally outside this core package because it would mix volatile side effects into the first harness implementation:

- OpenAI or ChatGPT subscription auth adapter.
- Async process management and cancellation.
- Presentation buffers, keymaps, rendering, and user interaction commands.
- Concrete Emacs buffer, file, process, and elisp-evaluation tools.
- Durable on-disk session persistence.
- Harness self-modification tools and permission UI.

## Required Design Self-Check

1. If this work package stopped here, the project would be better off because `e` would have a tested harness core instead of only a scaffold.
2. This moves toward the final direction in `docs/architecture.md`: stable harness core first, replaceable shells and adapters later.
3. Stable decisions are lifecycle names, event/message/tool-result shapes, and dependency direction. Uncertain decisions are persistence format, async execution, provider stream details, and UI rendering; those remain local to adapters or later modules.
4. The harness owns lifecycle and session coordination; the loop owns turn execution; sessions own transcript state; tools own tool dispatch; backends own provider interaction.
5. Coupling is reduced because presentation, provider auth, and concrete side effects stay outside core modules.
6. Dependencies point inward to stable contracts: future UI and provider adapters depend on the harness/backend contracts, while the core does not depend on UI or OpenAI details.
7. Side effects are not introduced in this package except in-process state mutation for tests and core coordination.
8. The abstractions are semantically real: harness lifecycle, backend streaming, tool execution, session storage, and event emission are all named behaviors from the architecture.
9. Compatibility expectation is only the existing scaffold package load/status surface; obsolete scaffold-only internals can be replaced once tests prove the new harness surface.
10. After this package lands, `e-core-scaffold-state` can be removed when `e-status` reports real harness capability state.
11. Expected domain errors such as missing sessions, missing tools, and idle aborts are handled where their owner has context; unexpected backend/tool errors surface as structured tool errors or normal Emacs errors.
12. ERT tests prove core behavior through fake backends, fake tools, and in-memory sessions without launching a presentation shell or provider adapter.
