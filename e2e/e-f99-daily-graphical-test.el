;;; e-f99-daily-graphical-test.el --- Daily Canvas acceptance -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)

(let ((fixture (getenv "GRIMOIRE_DAILY_GRAPHICAL_TEST_FILE")))
  (unless (and fixture (file-readable-p fixture))
    (error "GRIMOIRE_DAILY_GRAPHICAL_TEST_FILE must name the actual Grimoire fixture"))
  (load-file fixture))

(require 'e-anthropic)
(require 'e-context-canvas)
(require 'e-graphical-test-support)
(require 'e-json)
(require 'e-org-canvas)
(require 'e-session-sqlite)

(defun e-f99-daily-graphical-test--sse (events)
  "Encode provider-native EVENTS as a bounded Messages SSE response."
  (mapconcat
   (lambda (event)
     (format "event: %s\ndata: %s\n\n"
             (plist-get event :type)
             (e-json-serialize event)))
   events ""))

(defun e-f99-daily-graphical-test--response (state action)
  "Return the native response for ACTION, or a final text response."
  (let ((id (format "f99-call-%d" (cl-incf (aref state 3)))))
    (if action
        (let* ((name (plist-get action :name))
               (arguments (plist-get action :arguments))
               (argument-json (e-json-serialize arguments)))
          (e-f99-daily-graphical-test--sse
           (list
            '(:type "message_start"
              :message (:role "assistant" :content []
                        :usage (:input_tokens 100 :output_tokens 0)))
            (list :type "content_block_start" :index 0
                  :content_block
                  (list :type "tool_use" :id id :name name
                        :input nil))
            (list :type "content_block_delta" :index 0
                  :delta (list :type "input_json_delta"
                               :partial_json argument-json))
            '(:type "content_block_stop" :index 0)
            '(:type "message_delta"
              :delta (:stop_reason "tool_use")
              :usage (:output_tokens 12))
            '(:type "message_stop"))))
      (e-f99-daily-graphical-test--sse
       '((:type "message_start"
          :message (:role "assistant" :content []
                    :usage (:input_tokens 100 :output_tokens 0)))
         (:type "content_block_start" :index 0
          :content_block (:type "text" :text ""))
         (:type "content_block_delta" :index 0
          :delta (:type "text_delta" :text "Daily checked."))
         (:type "content_block_stop" :index 0)
         (:type "message_delta"
          :delta (:stop_reason "end_turn")
          :usage (:output_tokens 6))
         (:type "message_stop"))))))

(defun e-f99-daily-graphical-test--request-function (state)
  "Return a native Anthropic requester backed by fixture STATE."
  (cl-function
   (lambda (&key url headers body)
     (let* ((payload (e-json-parse-string body))
            (action (aref state 0))
            (wire (copy-tree payload t)))
       (aset state 0 nil)
       (aset state 1
             (append (aref state 1)
                     (list (list :url url :model (plist-get payload :model)
                                 :headers (mapcar #'car headers)
                                 :payload wire
                                 :sha256 (secure-hash 'sha256 body)))))
       (e-f99-daily-graphical-test--response state action)))))

(defun e-f99-daily-graphical-test--texts (value)
  "Return all strings nested in canonical JSON VALUE."
  (cond
   ((stringp value) (list value))
   ((and (vectorp value) (not (stringp value)))
    (cl-mapcan #'e-f99-daily-graphical-test--texts (append value nil)))
   ((consp value)
    (cl-mapcan #'e-f99-daily-graphical-test--texts value))
   (t nil)))

(defun e-f99-daily-graphical-test--canvas-texts (request)
  "Return native text blocks carrying request-scoped Canvas references."
  (seq-filter
   (lambda (text)
     (string-match-p "<canvas\\(?:-change\\)? " text))
   (e-f99-daily-graphical-test--texts (plist-get request :payload))))

(defun e-f99-daily-graphical-test--attribute (payload name)
  "Return attribute NAME from Canvas PAYLOAD."
  (when (string-match
         (concat "\\b" (regexp-quote name) "=\\\"\\([^\\\"]+\\)\\\"")
         payload)
    (match-string 1 payload)))

(defun e-f99-daily-graphical-test--patch (payload)
  "Parse one exact Canvas-change block from PAYLOAD."
  (when (string-match
         "<canvas-change [^>]*revision=\\\"\\([^\\\"]+\\)\\\" baseRevision=\\\"\\([^\\\"]+\\)\\\" start=\\\"\\([0-9]+\\)\\\" end=\\\"\\([0-9]+\\)\\\">"
         payload)
    (let* ((revision (match-string 1 payload))
           (base-revision (match-string 2 payload))
           (start (string-to-number (match-string 3 payload)))
           (end (string-to-number (match-string 4 payload)))
           (body-start (match-end 0))
           (body-end (string-match "\n</canvas-change>" payload body-start)))
      (unless body-end (error "Malformed native Canvas patch"))
      (list :revision revision :base-revision base-revision
            :start start :end end
            :replacement (substring payload (1+ body-start) body-end)))))

(defun e-f99-daily-graphical-test--canvas-summary (payload)
  "Summarize Canvas references in PAYLOAD without retaining document text."
  (mapcar
   (lambda (text)
     (list :kind (if (string-match-p "<canvas-change " text) 'patch 'snapshot)
           :revision (e-f99-daily-graphical-test--attribute text "revision")
           :base-revision
           (e-f99-daily-graphical-test--attribute text "baseRevision")
           :utf8-bytes (string-bytes (encode-coding-string text 'utf-8-unix t))
           :payload-sha256 (secure-hash 'sha256 text)))
   (e-f99-daily-graphical-test--canvas-texts payload)))

(defun e-f99-daily-graphical-test--messages (request)
  "Return REQUEST's native message sequence as a list."
  (append (plist-get (plist-get request :payload) :messages) nil))

(defun e-f99-daily-graphical-test--tool-result-texts (request)
  "Return text only from REQUEST's native tool-result blocks."
  (cl-loop for message in (e-f99-daily-graphical-test--messages request)
           append
           (cl-loop for block in (append (plist-get message :content) nil)
                    when (equal (plist-get block :type) "tool_result")
                    append (e-f99-daily-graphical-test--texts
                            (plist-get block :content)))))

(defun e-f99-daily-graphical-test--tool-pairs-valid-p (request)
  "Return non-nil when every native tool-use id has one ordered result."
  (let (uses results)
    (dolist (message (e-f99-daily-graphical-test--messages request))
      (dolist (block (append (plist-get message :content) nil))
        (pcase (plist-get block :type)
          ("tool_use" (push (plist-get block :id) uses))
          ("tool_result" (push (plist-get block :tool_use_id) results)))))
    (setq uses (nreverse uses)
          results (nreverse results))
    (and uses (equal uses results))))

(defun e-f99-daily-graphical-test--capture (name)
  "Capture settled visible Daily and chat state under NAME."
  (e-graphical-test-capture-state name))

(defun e-f99-daily-graphical-test--request-count (state)
  "Return the number of captured native requests in STATE."
  (length (aref state 1)))

(defun e-f99-daily-graphical-test--submit
    (runtime harness session-id composer state action prompt)
  "Submit PROMPT through COMPOSER and await one provider tool round."
  (let ((before (e-f99-daily-graphical-test--request-count state)))
    (aset state 0 action)
    (select-window (get-buffer-window composer t))
    (e-graphical-test-type-text prompt)
    (e-graphical-test-send-keys "C-c C-c")
    (e-graphical-test-wait-until
     (lambda ()
       (grimoire-graphical-daily-test--pump-runtime runtime)
       (and (>= (- (e-f99-daily-graphical-test--request-count state) before) 2)
            (not (e-chat-service-active-turn-p harness session-id))))
     20.0 "settled Daily provider tool round")
    (should (= (- (e-f99-daily-graphical-test--request-count state) before) 2))
    (should-not (aref state 2))
    (cons before (1- (e-f99-daily-graphical-test--request-count state)))))

(defun e-f99-daily-graphical-test--read-disk (file)
  "Return FILE's current saved content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun e-f99-daily-graphical-test--write-evidence (phase evidence)
  "Write bounded, content-free PHASE evidence and return its path."
  (let* ((directory (or (getenv "E_F99_DAILY_EVIDENCE_DIR")
                        (expand-file-name "evidence" temporary-file-directory)))
         (path (expand-file-name (format "f99-daily-%s-%s.el" phase (emacs-pid))
                                 directory)))
    (make-directory directory t)
    (with-temp-file path
      (let ((print-length nil) (print-level nil))
        (prin1 evidence (current-buffer))
        (insert "\n")))
    path))

(ert-deftest e-f99-daily-graphical-test-direct-write-phase-one ()
  "Actual Daily Canvas requests preserve snapshots, patches, and write guards."
  (e-chat-sql-e2e-reset)
  (let* ((repo (file-truename (make-temp-file "e-f99-daily-" t)))
         (runtime-directory (make-temp-file "e-f99-daily-runtime-" t))
         (date (format-time-string "%Y-%m-%d"))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-registry--generations (make-hash-table :test 'equal))
         (e-harness-registry--invalidation-events (make-hash-table :test 'equal))
         (e-subagent--configured-harnesses
          (make-hash-table :test 'eq :weakness 'key))
         sessions runtime harness state result doc-buffer composer transcript
         owner-session-id file uri initial-revision initial-snapshot-bytes
         (handoff-written nil))
    (unwind-protect
        (progn
          (let* ((fixture-result
                  (grimoire-topic-daily repo (list :date date)))
                 (path (expand-file-name (plist-get fixture-result :path) repo)))
            (setq file path
                  uri (concat "file://" (expand-file-name path)))
            (with-temp-buffer
              (insert-file-contents path)
              (goto-char (point-min))
              (unless (re-search-forward "^\\* Today's TODO$" nil t)
                (error "Generated Daily scaffold omitted Today's TODO"))
              (end-of-line)
              (insert
               "\n- [ ] Verify the current canvas revision\n"
               "- Agent target: before.\n"
               "- Fresh target: before.\n"
               "\n* Stable fixture notes\n")
              (dotimes (index 48)
                (insert
                 (format
                  "- Stable context baseline %02d: this fixture line remains unchanged during editor and agent edits.\n"
                  index)))
              (write-region (point-min) (point-max) path nil 'silent)))
          (setq state (vector nil nil nil 0)
                grimoire-graphical-daily-test--buffers-before (buffer-list))
          (let* ((profile
                  '((f99-daily-fixture
                     :name "F99 native serialization fixture"
                     :base-url "https://fixture.example.test/v1"
                     :auth bearer :env-key "F99_DAILY_FIXTURE_TOKEN"
                     :default-model "claude-opus-5-5")))
                 (e-anthropic-model-providers profile)
                 (process-environment (copy-sequence process-environment)))
            (setenv "F99_DAILY_FIXTURE_TOKEN" "synthetic-only")
            (setq sessions
                  (cl-letf (((symbol-function 'e-runtime-store-await)
                             #'grimoire-graphical-daily-test--await-with-pump))
                    (e-session-sqlite-store-create
                     runtime-directory :asynchronous t))
                  runtime (e-session-storage-runtime-store sessions)
                  harness
                  (e-anthropic-create-harness
                   :provider 'f99-daily-fixture
                   :request-function
                   (e-f99-daily-graphical-test--request-function state)
                   :model "claude-opus-5-5"
                   :sessions sessions))
            ;; The adapter backend is the native serializer/parser under test;
            ;; its injected requester returns fixture-owned SSE and opens no socket.
            (e-harness-instance-register
             :id :chat-default :kind 'chat :harness-id :chat-default
             :factory (lambda () harness) :default t)
            (e-session-enable sessions)
            (e-default-chat-sync-harness-layers harness nil repo)
            (cl-letf (((symbol-function 'grimoire-topic--require-repo-root)
                       (lambda () repo))
                      ((symbol-function 'e-chat-service-default-harness)
                       (lambda () harness)))
              (grimoire-daily-open date)
              (setq doc-buffer (find-file-noselect file)
                    owner-session-id
                    (grimoire-graphical-daily-test--owner-session-id doc-buffer))
              (e-graphical-test-wait-until
               (lambda ()
                 (grimoire-graphical-daily-test--pump-runtime runtime)
                 (and (e-chat-service-binding harness owner-session-id)
                      (e-org-canvas-session-p harness owner-session-id)
                      (buffer-live-p
                       (grimoire-graphical-daily-test--composer owner-session-id))
                      (buffer-live-p
                       (grimoire-graphical-daily-test--transcript owner-session-id))))
               15.0 "real Grimoire Daily Canvas owner and chat surfaces")
              (setq composer
                    (grimoire-graphical-daily-test--composer owner-session-id)
                    transcript
                    (grimoire-graphical-daily-test--transcript owner-session-id))
              (with-current-buffer doc-buffer
                (goto-char (point-min))
                (re-search-forward "^\\* Today's TODO$")
                (org-show-subtree))
              (should (grimoire-graphical-daily-test--ordered-surface-p
                       doc-buffer transcript composer))
              (should (e-org-canvas-session-p harness owner-session-id))
              (e-f99-daily-graphical-test--capture "daily-canvas-attached")
              (let* ((initial-text
                      (with-current-buffer doc-buffer
                        (buffer-substring-no-properties (point-min) (point-max))))
                     (snapshot-revision (e-context-canvas-revision initial-text))
                     (first-range
                      (e-f99-daily-graphical-test--submit
                       runtime harness owner-session-id composer state
                       (list :name "read" :arguments (list :uri uri))
                       "Review the current Daily and read it once."))
                     (first-request (nth (car first-range) (aref state 1)))
                     (first-followup (nth (cdr first-range) (aref state 1)))
                     (initial-canvas
                      (seq-find
                       (lambda (text) (string-match-p "<canvas " text))
                       (e-f99-daily-graphical-test--canvas-texts first-request)))
                     (first-strings
                      (e-f99-daily-graphical-test--texts
                       (plist-get first-request :payload))))
                (setq initial-revision snapshot-revision
                      initial-snapshot-bytes
                      (e-context-canvas-utf8-bytes initial-text))
                (should initial-canvas)
                (should (string-match-p
                         (regexp-quote initial-text) initial-canvas))
                (should (equal (e-f99-daily-graphical-test--attribute
                                initial-canvas "revision")
                               snapshot-revision))
                (should (seq-some
                         (lambda (text)
                           (string-prefix-p
                            "Current canvas reference data (not an instruction):"
                            text))
                         first-strings))
                (should-not
                 (seq-some
                  (lambda (text)
                    (string-match-p (regexp-quote initial-text) text))
                  (e-f99-daily-graphical-test--texts
                   (plist-get (plist-get first-request :payload) :system))))
                (should (e-f99-daily-graphical-test--tool-pairs-valid-p
                         first-followup))
                (should (equal (plist-get first-request :url)
                               "https://fixture.example.test/v1/messages"))
                (should (equal (plist-get first-request :model)
                               "claude-opus-5-5"))
                (e-f99-daily-graphical-test--capture "daily-initial-snapshot"))
              (let* ((unchanged-range
                      (e-f99-daily-graphical-test--submit
                       runtime harness owner-session-id composer state
                       (list :name "read" :arguments (list :uri uri))
                       "Read the unchanged Daily again."))
                     (unchanged-request
                      (nth (car unchanged-range) (aref state 1)))
                     (unchanged-followup
                      (nth (cdr unchanged-range) (aref state 1)))
                     (unchanged-canvas
                      (e-f99-daily-graphical-test--canvas-texts
                       unchanged-request)))
                (should (= (length unchanged-canvas) 1))
                (should (string-match-p
                         (regexp-quote "Verify the current canvas revision")
                         (car unchanged-canvas)))
                (should (equal (e-f99-daily-graphical-test--attribute
                                (car unchanged-canvas) "revision")
                               (e-context-canvas-revision
                                (with-current-buffer doc-buffer
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))
                (should (e-f99-daily-graphical-test--tool-pairs-valid-p
                         unchanged-followup)))
              (let* ((old-text
                      (with-current-buffer doc-buffer
                        (buffer-substring-no-properties (point-min) (point-max))))
                     (old-revision (e-context-canvas-revision old-text)))
                (with-current-buffer doc-buffer
                  (goto-char (point-min))
                  (re-search-forward "^- \\[ \\] Verify the current canvas revision$")
                  (beginning-of-line)
                  (org-toggle-checkbox)
                  (should (buffer-modified-p)))
                (let* ((new-text
                        (with-current-buffer doc-buffer
                          (buffer-substring-no-properties (point-min) (point-max))))
                       (new-revision (e-context-canvas-revision new-text))
                       (patch-range
                        (e-f99-daily-graphical-test--submit
                         runtime harness owner-session-id composer state
                         (list :name "read" :arguments (list :uri uri))
                         "Read my checkbox change."))
                       (patch-request
                        (nth (car patch-range) (aref state 1)))
                       (patch-followup
                        (nth (cdr patch-range) (aref state 1)))
                       (patch-text
                        (seq-find (lambda (text)
                                    (string-match-p "<canvas-change " text))
                                  (e-f99-daily-graphical-test--canvas-texts
                                   patch-request)))
                       (patch
                        (and patch-text
                             (e-f99-daily-graphical-test--patch patch-text))))
                  (e-f99-daily-graphical-test--write-evidence
                   "patch-probe"
                   (list :request-count (e-f99-daily-graphical-test--request-count state)
                         :canvas-references
                         (e-f99-daily-graphical-test--canvas-summary patch-request)
                         :checkbox-changed
                         (string-match-p "- \\[X\\] Verify the current canvas revision"
                                         new-text)))
                  (should (buffer-modified-p doc-buffer))
                  (should patch)
                  (should (equal (plist-get patch :base-revision) old-revision))
                  (should (equal (plist-get patch :revision) new-revision))
                  (should (equal
                           (e-context-canvas-apply-patch
                            old-text
                            (list :start (plist-get patch :start)
                                  :end (plist-get patch :end)
                                  :replacement (plist-get patch :replacement)))
                           new-text))
                  (should (equal (substring old-text
                                            (plist-get patch :start)
                                            (plist-get patch :end))
                                 " "))
                  (should (equal (plist-get patch :replacement) "X"))
                  (should (e-f99-daily-graphical-test--tool-pairs-valid-p
                           patch-followup))
                  (e-f99-daily-graphical-test--capture "daily-checkbox-patch")
                  (select-window (get-buffer-window doc-buffer t))
                  (e-graphical-test-send-keys "C-x C-s")
                  (should-not (buffer-modified-p doc-buffer))
                  (let* ((observed-revision new-revision)
                         (agent-range
                          (e-f99-daily-graphical-test--submit
                           runtime harness owner-session-id composer state
                           (list :name "edit"
                                 :arguments
                                 (list :uri uri
                                       :edits [(:oldText "Agent target: before."
                                                :newText "Agent target: after.")]
                                       :expectedRevision observed-revision))
                           "Update the Agent target line."))
                         (agent-followup
                          (nth (cdr agent-range) (aref state 1)))
                         (agent-result-text
                          (string-join
                           (e-f99-daily-graphical-test--tool-result-texts
                            agent-followup) "\n"))
                         (saved-after-agent
                          (e-f99-daily-graphical-test--read-disk file))
                         (agent-revision
                          (e-context-canvas-revision saved-after-agent)))
                    (e-f99-daily-graphical-test--write-evidence
                     "agent-edit-probe"
                     (list :status-ok
                           (string-match-p "\"status\":\"ok\""
                                           agent-result-text)
                           :status-conflict
                           (string-match-p "conflict" agent-result-text)
                           :result-length (length agent-result-text)
                           :result-sha256 (secure-hash 'sha256 agent-result-text)
                           :disk-revision agent-revision
                           :buffer-modified (buffer-modified-p doc-buffer)))
                    (should (string-match-p "\"status\":\"ok\""
                                            agent-result-text))
                    (should (string-match-p "Agent target: after\\." saved-after-agent))
                    (should-not (buffer-modified-p doc-buffer))
                    (let* ((user-range
                            (progn
                              (select-window (get-buffer-window doc-buffer t))
                              (goto-char (point-max))
                              (e-graphical-test-type-text
                               "\nUnsaved user addition after agent revision.\n")
                              (should (buffer-modified-p doc-buffer))
                              (e-f99-daily-graphical-test--capture
                               "daily-unsaved-user-edit")
                              (e-f99-daily-graphical-test--submit
                               runtime harness owner-session-id composer state
                               (list :name "write"
                                     :arguments
                                     (list :uri uri
                                           :content "Stale overwrite must not land."
                                           :expectedRevision agent-revision))
                               "Apply the previous edit request.")))
                           (stale-followup
                            (nth (cdr user-range) (aref state 1)))
                           (unsaved-text
                            (with-current-buffer doc-buffer
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
                           (current-revision
                            (e-context-canvas-revision unsaved-text))
                           (stale-result-text
                            (string-join
                             (e-f99-daily-graphical-test--texts
                              (plist-get stale-followup :payload)) "\n")))
                      (should (equal (e-f99-daily-graphical-test--read-disk file)
                                     saved-after-agent))
                      (should (equal (with-current-buffer doc-buffer
                                       (buffer-substring-no-properties
                                        (point-min) (point-max)))
                                     unsaved-text))
                      (should (buffer-modified-p doc-buffer))
                      (should (string-match-p "conflict" stale-result-text))
                      (should (string-match-p (regexp-quote current-revision)
                                              stale-result-text))
                      (should (e-f99-daily-graphical-test--tool-pairs-valid-p
                               stale-followup))
                      (let* ((read-range
                              (e-f99-daily-graphical-test--submit
                               runtime harness owner-session-id composer state
                               (list :name "read" :arguments (list :uri uri))
                               "Read the current Daily after the conflict."))
                             (read-followup
                              (nth (cdr read-range) (aref state 1)))
                             (read-result-text
                              (string-join
                               (e-f99-daily-graphical-test--texts
                                (plist-get read-followup :payload)) "\n")))
                        (should (string-match-p (regexp-quote current-revision)
                                                read-result-text))
                        (should (string-match-p
                                 (regexp-quote "Unsaved user addition after agent revision.")
                                 read-result-text))
                        (should (e-f99-daily-graphical-test--tool-pairs-valid-p
                                 read-followup)))
                      (let* ((fresh-range
                              (e-f99-daily-graphical-test--submit
                               runtime harness owner-session-id composer state
                               (list :name "edit"
                                     :arguments
                                     (list :uri
                                           (concat "buffer://"
                                                   (buffer-name doc-buffer))
                                           :edits [(:oldText "Fresh target: before."
                                                    :newText "Fresh target: revised.")]
                                           :expectedRevision current-revision))
                               "Use the current revision to update Fresh target."))
                             (fresh-followup
                              (nth (cdr fresh-range) (aref state 1)))
                             (fresh-result-text
                              (string-join
                               (e-f99-daily-graphical-test--tool-result-texts
                                fresh-followup) "\n"))
                             (buffer-after-fresh
                              (with-current-buffer doc-buffer
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
                             (disk-before-fresh
                              (e-f99-daily-graphical-test--read-disk file))
                             (artifact
                              (e-f99-daily-graphical-test--write-evidence
                               "phase-one"
                               (list :surface "actual Grimoire Daily + Org Canvas"
                                     :source-directory (e-source-directory)
                                     :provider "Anthropic Messages adapter; injected fixture transport"
                                     :endpoint "https://fixture.example.test/v1/messages"
                                     :model "claude-opus-5-5"
                                     :auth "synthetic bearer token; no transport"
                                     :snapshot-revision initial-revision
                                     :snapshot-utf8-bytes initial-snapshot-bytes
                                     :snapshot-sha256 initial-revision
                                     :unchanged-payload-count 1
                                     :checkbox-base-revision old-revision
                                     :checkbox-revision new-revision
                                     :checkbox-patch-reconstructs t
                                     :agent-edit-expected-revision observed-revision
                                     :agent-edit-result "ok"
                                     :stale-write-expected-revision agent-revision
                                     :stale-write-current-revision current-revision
                                     :stale-write-result "conflict; buffer and disk preserved"
                                     :fresh-read-revision current-revision
                                     :fresh-edit-uri-scheme "buffer"
                                     :fresh-edit-result "ok; unsaved buffer only"
                                     :native-request-count
                                     (e-f99-daily-graphical-test--request-count state)))))
                        (should (string-match-p "\"status\":\"ok\""
                                                fresh-result-text))
                        (should (string-match-p "Fresh target: revised\\."
                                                buffer-after-fresh))
                        (should (string-match-p
                                 "Unsaved user addition after agent revision\."
                                 buffer-after-fresh))
                        (should (string-suffix-p
                                 "Unsaved user addition after agent revision.\n"
                                 buffer-after-fresh))
                        (should (equal disk-before-fresh saved-after-agent))
                        (should (buffer-modified-p doc-buffer))
                        (should (e-f99-daily-graphical-test--tool-pairs-valid-p
                                 fresh-followup))
                        (select-window (get-buffer-window doc-buffer t))
                        (e-graphical-test-send-keys "C-x C-s")
                        (should-not (buffer-modified-p doc-buffer))
                        (let ((final-text
                               (e-f99-daily-graphical-test--read-disk file))
                              (final-revision
                               (e-context-canvas-revision
                                (e-f99-daily-graphical-test--read-disk file))))
                          (should (string-match-p
                                   "Fresh target: revised\\." final-text))
                          (should (string-match-p
                                   "Unsaved user addition after agent revision\."
                                   final-text))
                        (e-f99-daily-graphical-test--capture "daily-fresh-edit-saved")
                        (grimoire-graphical-daily-test--write-handoff
                         (list :repo repo :runtime-directory runtime-directory
                               :date date :final-revision final-revision
                               :evidence artifact))
                          (setq handoff-written t))))))))))
      (when sessions
        (ignore-errors (e-session-async-teardown sessions))
        (ignore-errors (e-runtime-store-shutdown runtime)))
      (e-chat-sql-e2e-reset)
      (grimoire-graphical-daily-test--kill-phase-buffers)
      (unless handoff-written
        (dolist (directory (list repo runtime-directory))
          (when (file-directory-p directory)
            (delete-directory directory t)))))))

(ert-deftest e-f99-daily-graphical-test-direct-write-phase-two ()
  "A fresh graphical Emacs reconstructs a complete Daily baseline."
  (e-chat-sql-e2e-reset)
  (let* ((handoff (grimoire-graphical-daily-test--read-handoff))
         (repo (plist-get handoff :repo))
         (runtime-directory (plist-get handoff :runtime-directory))
         (date (plist-get handoff :date))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-registry--generations (make-hash-table :test 'equal))
         (e-harness-registry--invalidation-events (make-hash-table :test 'equal))
         (e-subagent--configured-harnesses
          (make-hash-table :test 'eq :weakness 'key))
         (state (vector nil nil nil 0))
         sessions runtime harness doc-buffer composer transcript owner-session-id file uri)
    (unwind-protect
        (progn
          (setq file (expand-file-name (format "daily/%s.org" date) repo)
                uri (concat "file://" file)
                grimoire-graphical-daily-test--buffers-before (buffer-list))
          (let* ((profile
                  '((f99-daily-fixture
                     :name "F99 native serialization fixture"
                     :base-url "https://fixture.example.test/v1"
                     :auth bearer :env-key "F99_DAILY_FIXTURE_TOKEN"
                     :default-model "claude-opus-5-5")))
                 (e-anthropic-model-providers profile)
                 (process-environment (copy-sequence process-environment)))
            (setenv "F99_DAILY_FIXTURE_TOKEN" "synthetic-only")
            (setq sessions
                  (cl-letf (((symbol-function 'e-runtime-store-await)
                             #'grimoire-graphical-daily-test--await-with-pump))
                    (e-session-sqlite-store-create
                     runtime-directory :asynchronous t))
                  runtime (e-session-storage-runtime-store sessions)
                  harness
                  (e-anthropic-create-harness
                   :provider 'f99-daily-fixture
                   :request-function
                   (e-f99-daily-graphical-test--request-function state)
                   :model "claude-opus-5-5"
                   :sessions sessions))
            (e-harness-instance-register
             :id :chat-default :kind 'chat :harness-id :chat-default
             :factory (lambda () harness) :default t)
            (e-session-enable sessions)
            (e-default-chat-sync-harness-layers harness nil repo)
            (cl-letf (((symbol-function 'grimoire-topic--require-repo-root)
                       (lambda () repo))
                      ((symbol-function 'e-chat-service-default-harness)
                       (lambda () harness)))
              (grimoire-daily-open date)
              (setq doc-buffer (find-file-noselect file)
                    owner-session-id
                    (grimoire-graphical-daily-test--owner-session-id doc-buffer))
              (e-graphical-test-wait-until
               (lambda ()
                 (grimoire-graphical-daily-test--pump-runtime runtime)
                 (and (e-chat-service-binding harness owner-session-id)
                      (e-org-canvas-session-p harness owner-session-id)
                      (buffer-live-p
                       (grimoire-graphical-daily-test--composer owner-session-id))
                      (buffer-live-p
                       (grimoire-graphical-daily-test--transcript owner-session-id))))
               15.0 "reopened Grimoire Daily Canvas owner")
              (setq composer
                    (grimoire-graphical-daily-test--composer owner-session-id)
                    transcript
                    (grimoire-graphical-daily-test--transcript owner-session-id))
              (should (grimoire-graphical-daily-test--ordered-surface-p
                       doc-buffer transcript composer))
              (let* ((full-text
                      (with-current-buffer doc-buffer
                        (buffer-substring-no-properties (point-min) (point-max))))
                     (revision (e-context-canvas-revision full-text))
                     (range
                      (e-f99-daily-graphical-test--submit
                       runtime harness owner-session-id composer state
                       (list :name "read" :arguments (list :uri uri))
                       "Read the reopened Daily from its fresh baseline."))
                     (request (nth (car range) (aref state 1)))
                     (followup (nth (cdr range) (aref state 1)))
                     (canvases
                      (e-f99-daily-graphical-test--canvas-texts request))
                     (snapshot
                      (seq-find (lambda (text)
                                  (string-match-p "<canvas " text))
                                canvases))
                     (artifact
                      (e-f99-daily-graphical-test--write-evidence
                       "phase-two"
                       (list :fresh-process t
                             :surface "actual Grimoire Daily + Org Canvas"
                             :source-directory (e-source-directory)
                             :previous-process-revision
                             (plist-get handoff :final-revision)
                             :snapshot-revision revision
                             :snapshot-utf8-bytes
                             (e-context-canvas-utf8-bytes full-text)
                             :snapshot-sha256 revision
                             :complete-current-text t
                             :canvas-patch-count
                             (cl-count-if
                              (lambda (text)
                                (string-match-p "<canvas-change " text))
                              canvases)
                             :native-request-count
                             (e-f99-daily-graphical-test--request-count state)))))
                (should (= (length canvases) 1))
                (should snapshot)
                (should (string-match-p (regexp-quote full-text) snapshot))
                (should (equal (e-f99-daily-graphical-test--attribute
                                snapshot "revision") revision))
                (should (e-f99-daily-graphical-test--tool-pairs-valid-p followup))
                (e-f99-daily-graphical-test--capture "daily-fresh-process-snapshot")
                (message "F99 Daily graphical evidence: %s / %s"
                         (plist-get handoff :evidence) artifact))))))
      (when sessions
        (ignore-errors (e-session-async-teardown sessions))
        (ignore-errors (e-runtime-store-shutdown runtime)))
      (e-chat-sql-e2e-reset)
      (grimoire-graphical-daily-test--kill-phase-buffers)
      (dolist (directory (list repo runtime-directory))
        (when (file-directory-p directory)
          (delete-directory directory t)))
      (let ((path (grimoire-graphical-daily-test--handoff-path)))
        (when (file-exists-p path)
          (delete-file path)))))

(provide 'e-f99-daily-graphical-test)
;;; e-f99-daily-graphical-test.el ends here
