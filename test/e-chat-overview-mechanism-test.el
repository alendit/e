;;; e-chat-overview-mechanism-test.el --- Overview owner mechanism tests -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise overview-owned private mechanisms through the composed
;; shell fixture.  Public composed behavior remains in
;; `e-chat-presentation-integration-test.el'; standalone catalog and command
;; contracts live in `e-chat-overview-test.el'.

;;; Code:

(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-chat-test-resume-preview-renders-only-tail-messages ()
  "Resume previews render a small transcript tail for responsive selection."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (origin (get-buffer-create "chat-resume-preview-tail-origin"))
         (e-chat-resume-preview-message-limit 2))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "preview-tail"
                            :metadata '(:name "Tail preview"))
          (dotimes (index 6)
            (e-session-append-message
             store
             "preview-tail"
             (list :id (format "msg-%d" index)
                   :role (if (cl-evenp index) 'user 'assistant)
                   :content (format "preview message %d" index))))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "preview-tail")
          (let* ((sessions (e-harness-session-list harness))
                 (candidates
                  (mapcar (lambda (session)
                            (list :harness harness
                                  :session session
                                  :session-id (plist-get session :id)))
                          sessions))
                 (labels
                  (mapcar #'e-chat-overview-session-candidate-label
                          candidates))
                 (state (e-chat-overview--resume-preview-state
                         candidates labels)))
            (switch-to-buffer origin)
            (funcall state 'preview (car labels))
            (let ((preview (get-buffer
                            (e-chat-overview-resume-preview-buffer-name))))
              (should preview)
              (with-current-buffer preview
                (let ((text (buffer-string)))
                  (should-not (string-match-p "preview message 0" text))
                  (should-not (string-match-p "preview message 3" text))
                  (should (string-match-p "preview message 4" text))
                  (should (string-match-p "preview message 5" text)))))
            (funcall state 'exit nil)))
      (e-chat-test--kill-chat-buffers)
      (when (buffer-live-p origin)
        (kill-buffer origin)))))





(ert-deftest e-chat-test-resume-preview-state-renders-selected-session ()
  "Resume completion preview renders the highlighted session in a preview buffer."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (origin (get-buffer-create "chat-resume-preview-origin")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "preview-me")
          (e-session-append-message
           store "preview-me" '(:id "msg-1" :role user :content "preview hello"))
          (let* ((sessions (e-harness-session-list harness))
                 (candidates
                  (mapcar (lambda (session)
                            (list :harness harness
                                  :session session
                                  :session-id (plist-get session :id)))
                          sessions))
                 (labels
                  (mapcar #'e-chat-overview-session-candidate-label
                          candidates))
                 (state (e-chat-overview--resume-preview-state
                         candidates labels)))
            (switch-to-buffer origin)
            (funcall state 'preview (car labels))
            (let ((preview (get-buffer
                            (e-chat-overview-resume-preview-buffer-name))))
              (should preview)
              (should (eq (window-buffer (selected-window)) preview))
              (with-current-buffer preview
                (should (equal e-chat-session-id "preview-me"))
                (should (string-match-p "preview hello" (buffer-string)))))
            (funcall state 'exit nil)
            (should (eq (window-buffer (selected-window)) origin))
            (should-not (get-buffer
                         (e-chat-overview-resume-preview-buffer-name)))))
      (e-chat-test--kill-chat-buffers)
      (when (buffer-live-p origin)
        (kill-buffer origin)))))





(ert-deftest e-chat-test-overview-open-session-marks-session-read ()
  "Opening from overview records the selected session read marker."
  (let* ((directory (make-temp-file "e-chat-overview-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (e-chat-overview--read-markers (make-hash-table :test #'eq)))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "read-me"
                            :metadata '(:name "Read Me"))
          (e-session-append-message
           store "read-me"
           '(:id "assistant-read" :role assistant :content "answer"))
          (let ((buffer (get-buffer-create "*e-chat-overview-test*")))
            (unwind-protect
                (with-current-buffer buffer
                  (e-chat-overview-mode)
                  (e-chat-overview-render harness)
                  (goto-char (point-min))
                  (let ((chat-buffer (e-chat-overview-open-session)))
                    (with-current-buffer chat-buffer
                      (should (equal e-chat-session-id "read-me")))
                         (should (equal
                                  (e-chat-overview--read-marker
                                   "read-me" harness)
                                  "assistant-read"))
                    (should-not
                     (plist-member
                      (plist-get (e-session-get store "read-me") :metadata)
                      :e-chat-read-markers))
                    (e-chat-overview-render harness)
                    (should-not (string-match-p
                                 "! Read Me"
                                 (buffer-string)))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))





(ert-deftest e-chat-test-active-session-line-can-use-stale-status-snapshot ()
  "Active-session picker rows can serve stale cached status without rebuilding."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (candidate (list :harness harness
                          :session '(:id "picker-stale-status-cache"
                                      :title "Picker Stale Status"
                                      :message-count 1
                                      :loaded t)
                          :session-id "picker-stale-status-cache"))
         (status-cache (make-hash-table :test #'equal))
         (calls 0))
    (e-chat-test--create-session store :id "picker-stale-status-cache")
    (let* ((status-key (e-chat-overview--active-session-status-key candidate))
           (status-cache-cell (puthash status-key
                                       (cons
                                        (list :text "ctx cached"
                                              :time 0.0
                                              :snapshot-cache-keyed t
                                              :snapshot-cache-key
                                              (list :status-key status-key
                                                    :prefer-token-usage t
                                                    :estimate-context nil))
                                        nil)
                                       status-cache)))
      (cl-letf (((symbol-function 'e-context-budget-status)
                 (lambda (&rest _args)
                   (setq calls (1+ calls))
                   (error "stale picker status should not rebuild")))
                ((symbol-function 'e-chat-overview-session-unread-p)
                 (lambda (&rest _args) nil)))
        (let ((e-context-status-estimate-cache-seconds 1))
          (should (string-match-p
                   "stale ctx cached"
                   (e-chat-overview-active-session-line candidate status-cache)))))
      (should (eq status-cache-cell
                  (gethash status-key status-cache))))
    (should (= calls 0))))





(ert-deftest e-chat-overview-owner-active-session-selection-is-an-intent ()
  "Active-session selection returns intent without opening a chat buffer."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (session '(:id "workspace-active" :title "Workspace Active"
                    :summary "Prompt text" :message-count 1 :loaded t))
         (candidate (list :harness harness :session session
                          :session-id "workspace-active"))
         (selection (e-chat-overview--active-session-selection candidate)))
    (should (equal (plist-get selection :session-id) "workspace-active"))
    (should (eq (plist-get selection :candidate) candidate))
    (should-not (plist-get selection :buffer))))

(ert-deftest e-chat-overview-owner-active-candidates-require-a-prompt ()
  "Active-session candidates exclude empty and assistant-only sessions."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions (e-session-store-create)))
         (empty (list :harness harness
                      :session '(:id "empty" :message-count 0 :messages nil)
                      :session-id "empty"))
         (assistant-only
          (list :harness harness
                :session '(:id "assistant-only" :message-count 1
                           :messages ((:role assistant :content "answer")))
                :session-id "assistant-only"))
         (prompted
          (list :harness harness
                :session '(:id "prompted" :message-count 1
                           :messages ((:role user :content "prompt")))
                :session-id "prompted")))
    (cl-letf (((symbol-function 'e-chat-overview--session-candidates)
               (lambda () (list empty assistant-only prompted))))
      (should (equal (mapcar (lambda (candidate)
                               (plist-get candidate :session-id))
                             (e-chat-overview-active-session-candidates))
                     '("prompted"))))))





(ert-deftest e-chat-test-only-unread-relevant-events-refresh-unread-cache ()
  "Tool activity skips unread work while assistant output refreshes it once."
  (with-temp-buffer
    (e-chat-mode)
    (setq-local e-chat-session-id "unread-events")
    (setq-local e-chat-harness
                (e-harness-create :enabled-layer-ids nil))
    (let ((updates 0)
          (inhibit-read-only t))
      (cl-letf (((symbol-function 'e-chat-overview--workspace-unread-cache-update-buffer)
                 (lambda (&rest _args) (setq updates (1+ updates)))))
        (e-chat-render-event
         '(:type tool-started :session-id "unread-events" :turn-id "turn-1"
           :created-at 1.0 :payload (:id "tool-1" :name "fake")))
        (should (= updates 0))
        (e-chat-render-event
         '(:type message-added :session-id "unread-events" :turn-id "turn-1"
           :created-at 2.0
           :payload (:message (:id "answer" :role assistant
                               :content "done" :turn-id "turn-1"))))
        (should (= updates 1))))))





(ert-deftest e-chat-test-workspace-unread-indicator-follows-chat-affinity ()
  "Workspace unread markers follow chat buffer workspace affinity."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (workspace (make-e-workspace-token
                     :backend 'single
                     :id 'target
                     :name "target"
                     :frame (selected-frame)))
         (other-workspace (make-e-workspace-token
                           :backend 'single
                           :id 'other
                           :name "other"
                           :frame (selected-frame)))
         (buffer (generate-new-buffer " *e-chat-workspace-unread-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "workspace-unread"
                            :metadata '(:name "Workspace unread"))
          (e-session-append-message
           store "workspace-unread"
           '(:id "msg-1" :role user :content "prompt"))
          (e-session-append-message
           store "workspace-unread"
           '(:id "msg-2" :role assistant :content "response"))
          (with-current-buffer buffer
            (e-chat-mode)
            (setq-local e-chat-harness harness)
            (setq-local e-chat-session-id "workspace-unread")
            (e-buffer-set-workspace buffer workspace))
          ;; Build after the surface has joined its workspace.  The first
          ;; public query performs the same lazy rebuild in normal use, while
          ;; this explicit rebuild keeps the cache assertion independent of
          ;; test ordering.
          (e-chat-overview-rebuild-unread-cache)
          (should (e-chat-workspace-unread-p workspace))
          (cl-letf (((symbol-function 'e-chat-overview--buffer-unread-p)
                     (lambda (&rest _args)
                       (error "cached unread lookup should not scan buffers"))))
            (should (e-chat-workspace-unread-p "target"))
            (should-not (e-chat-workspace-unread-p other-workspace))
            (should (equal (substring-no-properties
                            (e-chat-workspace-unread-indicator workspace))
                           "●"))
            (should (eq (get-text-property
                         0
                         'font-lock-face
                         (e-chat-workspace-unread-indicator workspace))
                        'e-chat-workspace-unread-face)))
          (e-chat-overview-mark-session-read
           harness
           (e-chat-overview--session-for-id harness "workspace-unread"))
          (should-not (e-chat-workspace-unread-p workspace))
          (should-not (e-chat-workspace-unread-indicator workspace)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-overview-rebuild-unread-cache))))





(ert-deftest e-chat-test-session-id-lookup-does-not-list-session-catalog ()
  "Unread lookup resolves one session without listing and sorting its catalog."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create :sessions store :enabled-layer-ids nil)))
    (dotimes (index 512)
      (e-session-create store :id (format "catalog-%03d" index)))
    (cl-letf (((symbol-function 'e-harness-session-list)
               (lambda (&rest _args) (error "catalog scan"))))
      (should (equal (plist-get
                      (e-chat-overview--session-for-id harness "catalog-511")
                      :id)
                     "catalog-511")))))
(provide 'e-chat-overview-test)

;;; e-chat-overview-test.el ends here
